;;; timelog.el --- Extension to timeclock.  -*- lexical-binding: t; -*-
;;
;; Original Author: Markus Flambard in 2009
;; Original copy from: https://gist.github.com/flambard/419770#file-timelog-el
;; Modified by Pierre Rouleau : use lexical-binding, fixed compiler warnings.
;; Time-stamp: <2023-01-09 22:58:12 EST, updated by Pierre Rouleau>

;;; --------------------------------------------------------------------------
;;; Commentary:
;;
;; Markus Flambard created this file to extend Emacs built-in timeclock.
;; The extensions provide commands to summarize the time accumulated in
;; various entries.
;;
;; I (Pierre Rouleau) cloned his file and modernized it to ensure it byte
;; compiles cleanly with Emacs 26 and later. More work would be required to
;; provide docstrings to the commands, something I might do later.
;;
;; There was no mention of a license in the original copy, so I'm not putting
;; any for the moment.
;;
;; Added:
;; - timelog customization group:
;;   - select report format with `timelog-summary-format' which can be the
;;   original (called visual for lack of a better word, and is the default),
;;   and a CSV format.
;; - The CSV format has 3 filled columns with an extra columns to compute
;;   duration in the final spreadsheet.  The CSV has a title row which has a
;;   title in the fourth column describing the period.
;; - The `timelog-open-file' command that opens the `timeclock-file' into the
;;   current buffer.
;; - Add command-specific prompt history for all timelog-summarize commands.
;;
;; Fix:
;; - The original code did not handle periods crossing midnight, reporting
;;   negative time duration for those.  The new code is able to handle time
;;   periods that cross over midnight, but only once.  The longest time period
;;   supported is therefore 48 hours less 1 second.
;; - The same issue affected counting time for the last project of the day if
;;   that project crossed midnight when trying to create a report for a single
;;   day.  The new code detects the situation and compute tine for an opened
;;   ended project at the end of a day as if it was ending at midnight.  This
;;   way a single day report includes that period properly.
;; - To ensure that all periods are properly counted the file content is
;;   corrected by `timelog--fix-midnight-crossings' before any report is
;;   created.  This inserts project termination and start over midnight to
;;   prevent all possibilities of duration computation errors.  This
;;   correction is done once per day, which should suffice.
;;
;;
;; Code modifications:
;; - Renamed internal functions to timelog--SOMETHING
;; - Added docstrings to several functions but not all (yet).

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
(require 'timeclock)
(require 'cl-lib)

;;; --------------------------------------------------------------------------
;;; Code:

(defgroup timelog nil
  "Timelog - print summary of time spent on projects."
  :group 'timeclock)

(defcustom timelog-summary-format nil
  "Default format of summary reports.

The choices are:
- visual,  (nil) the default: a line separated ledger report of the
           time spent per project.
- csv,    A comma-separated value. Use this to export into spreadsheets."
  :group 'timelog
  :type '(choice
          (const :tag "visual" nil)
          (const :tag "Comma Separated Value" csv)))

;; --

(defun timelog--timelist-cmp (t1 t2)
  "Compare T1 and T2 timelist entries.

Sort predicate to list in increased order of project name."
  (string< (car t1) (car t2)))

(defun timelog--read-date ()
  "Read and return the date string on timelog buffer current line."
  (save-excursion
    (beginning-of-line)
    (buffer-substring-no-properties (+ 2 (point)) (+ 12 (point)))))

(defun timelog--read-time (&optional use-current-time-if-eobp)
  "Read and returns the time stamp in seconds of timelog buffer current line."
  (if (and (eobp)
           use-current-time-if-eobp)
      (timelog--current-time-string)
    (progn
      (forward-char 15)
      (let ((hour (current-word nil t)))
        (forward-word 1)
        (let ((minute (current-word nil t)))
          (forward-word 1)
          (let ((second (current-word nil t)))
            (+
             (* (string-to-number hour) 3600)
             (* (string-to-number minute) 60)
             (string-to-number second))))))))

(defun timelog--read-time-and-project ()
  "Read and return date and time stamp in seconds of timelog buffer current line."
  (beginning-of-line)
  (let ((time (timelog--read-time)))
    (forward-char 1)
    (list time (current-word))))

(defun timelog--seconds-to-time (total-seconds)
  "Return a [H]H:MM:SS time string for TOTAL-SECONDS."
  (let ((hours (/ total-seconds 3600))
        (%hours (% total-seconds 3600)))
    (format "%d:%02d:%02d" hours (/ %hours 60) (% %hours 60))))

(defun timelog--current-time-string ()
  "Return current time as a number of seconds from the beginning of the day."
  (cl-destructuring-bind (seconds minutes hours &rest ignored) (decode-time)
    (+ seconds (* minutes 60) (* hours 3600))))

(defun timelog--current-date ()
  "Return a list of day, month, year for today."
  (cl-destructuring-bind (_s _m _h day month year &rest ignored) (decode-time)
    (list day month year)))

(defun timelog--date-differ-p (date1 date2)
  "Return t if DATE1 and DATE2 are the same date, nil otherwise.

Both DATE1 and DATE2 are (day month year) lists."
  (or  (not (eq (nth 0 date1) (nth 0 date2)))
       (not (eq (nth 1 date1) (nth 1 date2)))
       (not (eq (nth 2 date1) (nth 2 date2)))))

(defun timelog--current-date-string (&optional format-string)
  "Return today's date in \"YYYY/MM/DD\" format."
  (cl-destructuring-bind (_s _m _h day month year &rest ignored) (decode-time)
    (format (or format-string
                "%4d/%02d/%02d")
            year month day)))

(defun timelog--current-month-string ()
  "Return YYYYMM string representing current month."
  (cl-destructuring-bind (_s _m _h _day month year &rest ignored) (decode-time)
    (format "%4d%02d" year month)))

(defun timelog--add-to-time-list (project start-time stop-time time-list)
  "Return an alist of (project . time-spent) for provided arguments.

PROJECT:    string: project
START-TIME: integer: time of day in seconds of the start point.
STOP-TIME:  integer: time of day in seconds of the stop point.
TIME-LIST:  existing alist that must be updated and returned.

The function handles period that crosses over midnight where STOP-TIME
is a smaller number than START-TIME.

*LIMITATION*: Period longer than 47h59m59s are *not* supported."
  (let ((previous (assoc project time-list))
        (time-spent (- stop-time start-time)))
    (when (< time-spent 0)
      ;; adjust time period that crosses midnight.
      (cl-incf time-spent 86400 ))
    (if previous
        (cons (cons project (+ time-spent (cdr previous)))
              (remove previous time-list))
      (cons (cons project time-spent) time-list))))

(defun timelog--narrow-to-date-range (first-date last-date) ;; YYYY/MM/DD
  (widen)
  (goto-char (point-min))
  (when (re-search-forward (format "^[io] %s" first-date) nil t)
    (beginning-of-line)
    (let ((start (point)))
      (goto-char (point-max))
      (re-search-backward (format "^[io] %s" last-date))
      (end-of-line)
      (narrow-to-region start (point))
      (goto-char (point-min))
      t)))

(defun timelog--narrow-to-date (date-string) ;; YYYY/MM/DD
  (timelog--narrow-to-date-range date-string date-string))

(defun timelog--narrow-to-month (month-string) ;; YYYY/MM
  (timelog--narrow-to-date month-string))

(defun timelog--get-dates-in-range (first-date last-date)
  (let ((dates (list)))
    (with-current-buffer (find-file-noselect timeclock-file t)
      (save-excursion
        (when (timelog--narrow-to-date-range first-date last-date)
          (let ((current-date ""))
            (while (not (eobp))
              (let ((this-date (timelog--read-date)))
                (unless (string= current-date this-date)
                  (push this-date dates)
                  (setq current-date this-date)))
              (beginning-of-line 2))))))
    (reverse dates)))

(defun timelog--move-to-first-input-line ()
  "Move point to the first line that is a time entry line.
Leave point at the beginning of a time entry line.
Return position of date entry if found, nil otherwise."
  (goto-char (point-min))
  (prog1
      (re-search-forward "^i " nil t)
    (beginning-of-line)))

(defun timelog--to-beginning-of-next-line ()
  "Move point to the beginning of next line. Return t on success.

Return nil otherwise."
  (beginning-of-line 2)
  (eq (point)
      (save-excursion
        (move-beginning-of-line 1))))

(defun timelog--end-of-day (date-string)
  "Return end of day in seconds.

If DATE-STRING corresponds to today, return current time in seconds,
otherwise return 86400, which corresponds to the total number of seconds in
24 hours.rn time of end of day in seconds.
If DATE-STRING represents today, return the current time in seconds,
otherwise return 86400, the equivalent of 24 hours."
  (if (string= date-string (timelog--current-date-string))
      (timelog--current-time-string)
    86400))

(defun timelog--do-summarize-day (date-string)
  "Return activity information for date specified by DATE-STRING.

DATE-STRING: format must be \"YYYY/MM/DD\".
The returned value is a list that includes:
- The time of day, in seconds, of the first entry for the day.
- The time of day, in seconds, of the last entry in the day.
- a list of (project . duration-in-second) cons cells for the activity
  during the specified day."
  (let ((time-list (list))
        (first-start-time)
        (last-stop-time)
        (next-line-found)
        (extant-timelog-buffer (find-buffer-visiting timeclock-file)))
    (with-current-buffer (find-file-noselect timeclock-file t)
      (save-excursion
        (save-restriction
          (when (timelog--narrow-to-date date-string)
            ;; some days may start with a time output line because the
            ;; activity started the previous day. Since timeclock does not
            ;; guarantee that the end-period report identifies the project,
            ;; there's no obvious way to determine the project of that
            ;; activity unless we go to the previous line but currently its
            ;; narrowed, so we can't.
            ;; For now just jump to the next time period entry line with
            ;; `timelog--move-to-first-input-line' as we minimize the
            ;; possibility of this to occur because we call
            ;; `timelog--fix-midnight-crossings' before starting any report.
            ;;
            (timelog--move-to-first-input-line)
            (setq first-start-time (timelog--read-time))
            (while (not (eobp))
              (cl-destructuring-bind (start-time project)
                  (timelog--read-time-and-project)
                (setq next-line-found (timelog--to-beginning-of-next-line))
                (let ((stop-time (if next-line-found
                                     (timelog--read-time t)
                                   ;; no other lines, stop counting at
                                   ;; midnight unless it's today: in that case
                                   ;; count up to the current time
                                   (timelog--end-of-day date-string))))
                  (setq time-list
                        (timelog--add-to-time-list
                         project start-time stop-time time-list))
                  (setq last-stop-time stop-time)))
              (beginning-of-line 2)))))
      (unless extant-timelog-buffer (kill-buffer (current-buffer))))
    (setq time-list (sort time-list #'(lambda (a b) (> (cdr a) (cdr b)))))
    (list first-start-time last-stop-time time-list)))

(defun timelog--do-summarize-month (month-string) ;; YYYY/MM
  (let ((time-list (list))
        (first-day "")
        (last-day "")
        (total-days 0)
        (extant-timelog-buffer (find-buffer-visiting timeclock-file)))
    (with-current-buffer (find-file-noselect timeclock-file t)
      (save-excursion
        (save-restriction
          (when (timelog--narrow-to-month month-string)
            (timelog--move-to-first-input-line)
            (setq first-day (timelog--read-date))
            (while (not (eobp))
              (let ((this-day (timelog--read-date)))
                (unless (string= last-day this-day)
                  (cl-incf total-days)
                  (setq last-day this-day)))
              (cl-destructuring-bind (start-time project)
                  (timelog--read-time-and-project)
                (beginning-of-line 2)
                (let ((stop-time (timelog--read-time t)))
                  (setq time-list
                        (timelog--add-to-time-list
                         project start-time stop-time time-list))))
              (beginning-of-line 2)))))
      (unless extant-timelog-buffer (kill-buffer (current-buffer))))
    (setq time-list (sort time-list #'(lambda (a b) (> (cdr a) (cdr b)))))
    (list first-day last-day total-days time-list)))

(defun timelog--do-summarize-range (first-day last-day) ;; date-strings
  (let ((time-list (list))
        (current-day "")
        (total-days 0)
        (extant-timelog-buffer (find-buffer-visiting timeclock-file)))
    (with-current-buffer (find-file-noselect timeclock-file t)
      (save-excursion
        (save-restriction
          (when (timelog--narrow-to-date-range first-day last-day)
            (timelog--move-to-first-input-line)
            (while (not (eobp))
              (let ((this-day (timelog--read-date)))
                (unless (string= current-day this-day)
                  (cl-incf total-days)
                  (setq current-day this-day)))
              (cl-destructuring-bind (start-time project)
                  (timelog--read-time-and-project)
                (beginning-of-line 2)
                (let ((stop-time (timelog--read-time t)))
                  (setq time-list
                        (timelog--add-to-time-list
                         project start-time stop-time time-list))))
              (beginning-of-line 2)))))
      (unless extant-timelog-buffer (kill-buffer (current-buffer))))
    (setq time-list (sort time-list #'(lambda (a b) (> (cdr a) (cdr b)))))
    (list total-days time-list)))

(defun timelog--time-list-sum (time-list)
  (cl-reduce #'+ time-list :initial-value 0 :key #'cdr))

(defun timelog--generate-time-table (time-list)
  (concat
   (format " Time spent  Project\n")
   (format " ----------  -------\n")
   (cl-reduce #'(lambda (table project-and-time)
                  (cl-destructuring-bind (project . time) project-and-time
                    (concat
                     (format "%11s  %s\n" (timelog--seconds-to-time time) project)
                     table)))
              (reverse (sort time-list (function timelog--timelist-cmp)))
              :initial-value "\n")
   (format "      Total\n")
   (format " ----------\n")
   (format "%11s\n" (timelog--seconds-to-time (timelog--time-list-sum time-list)))))

(defun timelog--generate-time-table-csv (time-list title)
  "Return a CSV formatted table of activity."
  (let ((item (length time-list)))
    (concat
     (format "item, time (seconds), activity, %s\n" title)
     (cl-reduce #'(lambda (table project-and-time)
                    (prog1
                        (cl-destructuring-bind (project . time) project-and-time
                          (concat
                           (format "%4d, %13s , %s\n" item time project)
                           table))
                      (setq item (1- item))))
                (reverse time-list)
                :initial-value "\n"))))

(defun timelog--generate-day-report (date-string
                                     start-time stop-time
                                     time-list)
  "Print a day activity report."
  (let ((title (format "Report for time spent %s; between %s and %s."
                       date-string
                       (timelog--seconds-to-time start-time)
                       (timelog--seconds-to-time stop-time))))
    (cond ((eq timelog-summary-format 'csv)
           (timelog--generate-time-table-csv time-list title))
          (t
           (concat
            (format "________________________________\n%s\n\n" title)
            (timelog--generate-time-table time-list)
            (format "________________________________\n"))))))

(defun timelog--generate-month-report (first-day last-day total-days time-list)
  "Print a monthly activity report."
  (let ((title (format "Report for time spent between %s and %s; total of %d days."
                       first-day last-day total-days)))
    (cond ((eq timelog-summary-format 'csv)
           (timelog--generate-time-table-csv time-list title))
          (t
           (concat
            (format "________________________________\n%s\n\n" title)
            (timelog--generate-time-table time-list)
            (format "________________________________\n"))))))

(defun timelog--add-slashes-to-date (date-string)
  (if (= 6 (length date-string))
      (concat (substring date-string 0 4) "/" (substring date-string 4))
    (concat (substring date-string 0 4) "/"
            (substring date-string 4 6) "/"
            (substring date-string 6))))
;; --

(defconst timelog--in-out-regexp
  "^i \\([0-9]+/[0-9][0-9]/[0-9][0-9]\\) [0-9][0-9]:[0-9][0-9]:[0-9][0-9] \\(.+\\)
o \\([0-9]+/[0-9][0-9]/[0-9][0-9]\\) [0-9][0-9]:[0-9][0-9]:[0-9][0-9]"
  "Regexp to extract the following:
- group 1: date of the in record
- group 2: name of the project (taken from the in record)
- group 3: date of the out record following the in record just before.")

(defvar timelog--fix-midnight-crossings-last-run-date nil
  "Date (day month year) of last `timelog--fix-midnight-crossings' call.")


(defun timelog--fix-midnight-crossings (&optional verbose forced)
  "Fix all midnight crossings in the timelog file.

Insert project end in day 1 and start in day 2.
Save file and reload it if anything changed.
Return nil if nothing was fixed, otherwise return the number of
midnight periods fixed.

This function executes fully only once a day unless FORCED is set."
  (when (or forced
            (null timelog--fix-midnight-crossings-last-run-date)
            (timelog--date-differ-p
             timelog--fix-midnight-crossings-last-run-date
             (timelog--current-date)))
    (setq timelog--fix-midnight-crossings-last-run-date
          (timelog--current-date))
    (let ((fix-count 0)
          (extant-timelog-buffer (find-buffer-visiting timeclock-file)))
      (with-current-buffer (find-file-noselect timeclock-file t)
        (save-excursion
          (save-restriction
            (goto-char (point-min))
            (while (not (eobp))
              (when (and (re-search-forward timelog--in-out-regexp nil :noerror)
                         (not (string= (match-string 1)
                                       (match-string 3))))
                ;; point is toward end of project input that ends next day
                ;; Insert 2 new lines the end that project
                (cl-incf fix-count)
                (move-beginning-of-line 1)
                (insert (format "o %s 23:59:59\ni %s 00:00:00 %s\n"
                                (match-string 1)
                                (match-string 3)
                                (match-string 2)))))
            (if (> fix-count 0)
                (progn
                  (save-buffer)
                  (timeclock-reread-log)
                  (when verbose
                    (message "Fixed %d midnight crossing issue%s in %s"
                             fix-count
                             (if (> fix-count 1)
                                 "s"
                               "")
                             timeclock-file)))
              (setq fix-count nil))))
        (unless extant-timelog-buffer (kill-buffer (current-buffer))))
      fix-count)))

;; --

(defun timelog-summarize-day (&optional date-string)
  "Print a time log summary for the specified date.

The DATE-STRING must be entered using the \"YYYMMDD\" format.
Use M-n to select today's date.
Use M-p and then M-n to navigate through prompt history."
  (interactive)
  (setq date-string
        (timelog--add-slashes-to-date
         (or date-string
             (read-string "Date [YYYYMMDD]: " nil
                          'timelog-summarize-day
                          (timelog--current-date-string "%4d%02d%02d")))))
  (timelog--fix-midnight-crossings :verbose)
  (cl-destructuring-bind (start-time stop-time projects)
      (timelog--do-summarize-day date-string)
    (if (null projects)
        (message "No entries for date %s in %s"
                 date-string
                 timeclock-file)
      (insert (timelog--generate-day-report
               date-string start-time stop-time projects)))))

(defun timelog-summarize-today ()
  "Print a time log summary for today's activities."
  (interactive)
  (cl-destructuring-bind (_s _m _h day month year . ignored)
      (decode-time)
    (timelog-summarize-day (format "%d%02d%02d" year month day))))

(defun timelog-summarize-month (&optional month-string)
  "Print a time log summary for activities done in the specified month.

MONTH-STRING format must be \"YYYYMM\". Prompt if not specified.
Use M-n to select this current month.
Use M-p and then M-n to navigate through prompt history."
  (interactive)
  (setq month-string (timelog--add-slashes-to-date
                      (or month-string
                          (read-string "Month [YYYYMM]: " nil
                                       'timelog-summarize-month
                                       (timelog--current-month-string)))))
  (timelog--fix-midnight-crossings :verbose)
  (cl-destructuring-bind (start-date stop-date total-days projects)
      (timelog--do-summarize-month month-string)
    (if (null projects)
        (message "No entries for month %s in %s"
                 month-string
                 timeclock-file)
      (insert (timelog--generate-month-report
               start-date stop-date total-days projects)))))

(defun timelog-summarize-range (&optional first-day last-day)
  "Print a time log for the activity in the days in the specified range.

If not specified, prompts for FIRST-DAY and LAST-DAY, which must
be entered using the \"YYYYMMDD\" format.
Use M-n to select today's date for the last date.
Use M-p and then M-n to navigate through prompt history."
  (interactive)
  (setq first-day (timelog--add-slashes-to-date
                   (or first-day
                       (read-string "First date [YYYYMMDD]: " nil
                                    'timelog-summarize-date-first)))
        last-day (timelog--add-slashes-to-date
                  (or last-day
                      (read-string "Last date [YYYYMMDD]: " nil
                                   'timelog-summarize-date-last
                                   (timelog--current-date-string
                                    "%4d%02d%02d")))))
  (timelog--fix-midnight-crossings :verbose)
  (cl-destructuring-bind (total-days projects)
      (timelog--do-summarize-range first-day last-day)
    (if (null projects)
        (message "No entries between dates %s and %s"
                 first-day
                 last-day)
      (insert (timelog--generate-month-report first-day
                                              last-day
                                              total-days
                                              projects)))))

(defun timelog-summarize-each-day-in-range (&optional first-day last-day)
  "Print a time log for the activity of each days in the specified range.

Prompts for the first date and the last date, which must be entered using
the \"YYYYMMDD\" format.
Use M-n to select today's date for the last date.
Use M-p and then M-n to navigate through prompt history."
  (interactive)
  (setq first-day (timelog--add-slashes-to-date
                   (or first-day
                       (read-string "First date [YYYYMMDD]: " nil
                                    'timelog-summarize-each-day-first)))
        last-day (timelog--add-slashes-to-date
                  (or last-day
                      (read-string "Last date [YYYYMMDD]: " nil
                                   'timelog-summarize-each-day-last
                                   (timelog--current-date-string
                                    "%4d%02d%02d")))))
  (timelog--fix-midnight-crossings :verbose)
  (mapcar
   #'(lambda (summary)
       (cl-destructuring-bind (date-string start stop time-list) summary
         (insert
          (timelog--generate-day-report date-string start stop time-list))))
   (mapcar #'(lambda (date-string)
               (cons date-string (timelog--do-summarize-day date-string)))
           (timelog--get-dates-in-range first-day last-day))))

(defun timelog-current-project ()
  "Display the last entry in timelog.  It corresponds to the current project."
  (interactive)
  (let ((extant-timelog-buffer (find-buffer-visiting timeclock-file)))
    (with-current-buffer (find-file-noselect timeclock-file t)
      (save-excursion
        (save-restriction
          (widen)
          (goto-char (point-max))
          (let ((last-line (buffer-substring-no-properties
                            (line-beginning-position 0) (1- (point)))))
            (message last-line))))
      (unless extant-timelog-buffer (kill-buffer (current-buffer))))))

(defun timelog-workday-elapsed ()
  (interactive)
  (timelog--fix-midnight-crossings :verbose)
  (cl-destructuring-bind (_s _m _h day month year . ignored) (decode-time)
    (let ((date-string (format "%d/%02d/%02d" year month day)))
      (cl-destructuring-bind (_start-time _stop-time projects)
          (timelog--do-summarize-day date-string)
        (if (null projects)
            (message "No entries for date %s in %s" date-string timeclock-file)
          (message "Total time worked today: %s" (timelog--seconds-to-time
                                                  (timelog--time-list-sum projects))))))))

(defun timelog-open-file ()
  "Open the `timeclock-file' in current buffer."
  (interactive)
  (find-file timeclock-file))

;;; --------------------------------------------------------------------------
(provide 'timelog)

;;; timelog.el ends here
