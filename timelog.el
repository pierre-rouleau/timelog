;;; timelog.el --- Extension to timeclock.  -*- lexical-binding: t; -*-
;;
;; Original Author: Markus Flambard in 2009
;; Original copy from: https://gist.github.com/flambard/419770#file-timelog-el
;; Modified by Pierre Rouleau : use lexical-binding, fixed compiler warnings.
;; Time-stamp: <2021-10-21 23:32:39, updated by Pierre Rouleau>

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


(defun timelog-read-date ()
  (save-excursion
    (beginning-of-line)
    (buffer-substring-no-properties (+ 2 (point)) (+ 12 (point)))))

(defun timelog-read-time (&optional use-current-time-if-eobp)
  (if (and (eobp)
           use-current-time-if-eobp)
      (timelog-current-time)
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

(defun timelog-read-time-and-project ()
  (beginning-of-line)
  (let ((time (timelog-read-time)))
    (forward-char 1)
    (list time (current-word))))

(defun timelog-seconds-to-time (total-seconds)
  (let ((hours (/ total-seconds 3600))
        (%hours (% total-seconds 3600)))
    (format "%d:%02d:%02d" hours (/ %hours 60) (% %hours 60))))

(defun timelog-current-time ()
  (cl-destructuring-bind (seconds minutes hours &rest ignored) (decode-time)
    (+ seconds (* minutes 60) (* hours 3600))))

(defun add-to-time-list (project time-spent time-list)
  (let ((previous (assoc project time-list)))
    (if previous
        (cons (cons project (+ time-spent (cdr previous)))
              (remove previous time-list))
      (cons (cons project time-spent) time-list))))

(defun timelog-narrow-to-date-range (first-date last-date) ;; YYYY/MM/DD
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

(defun timelog-narrow-to-date (date-string) ;; YYYY/MM/DD
  (timelog-narrow-to-date-range date-string date-string))

(defun timelog-narrow-to-month (month-string) ;; YYYY/MM
  (timelog-narrow-to-date month-string))

(defun timelog-get-dates-in-range (first-date last-date)
  (let ((dates (list)))
    (with-current-buffer (find-file-noselect timeclock-file t)
      (save-excursion
        (when (timelog-narrow-to-date-range first-date last-date)
          (let ((current-date ""))
            (while (not (eobp))
              (let ((this-date (timelog-read-date)))
                (unless (string= current-date this-date)
                  (push this-date dates)
                  (setq current-date this-date)))
              (beginning-of-line 2))))))
    (reverse dates)))


(defun timelog-do-summarize-day (date-string) ;; YYYY/MM/DD
  (let ((time-list (list))
        (first-start-time)
        (last-stop-time)
        (extant-timelog-buffer (find-buffer-visiting timeclock-file)))
    (with-current-buffer (find-file-noselect timeclock-file t)
      (save-excursion
        (save-restriction
          (when (timelog-narrow-to-date date-string)
            (setq first-start-time (timelog-read-time))
            (while (not (eobp))
              (cl-destructuring-bind (start-time project)
                  (timelog-read-time-and-project)
                (beginning-of-line 2)
                (let ((stop-time (timelog-read-time t)))
                  (setq time-list
                        (add-to-time-list
                         project (- stop-time start-time) time-list))
                  (setq last-stop-time stop-time)))
              (beginning-of-line 2)))))
      (unless extant-timelog-buffer (kill-buffer (current-buffer))))
    (setq time-list (sort time-list #'(lambda (a b) (> (cdr a) (cdr b)))))
    (list first-start-time last-stop-time time-list)))

(defun timelog-do-summarize-month (month-string) ;; YYYY/MM
  (let ((time-list (list))
        (first-day "")
        (last-day "")
        (total-days 0)
        (extant-timelog-buffer (find-buffer-visiting timeclock-file)))
    (with-current-buffer (find-file-noselect timeclock-file t)
      (save-excursion
        (save-restriction
          (when (timelog-narrow-to-month month-string)
            (setq first-day (timelog-read-date))
            (while (not (eobp))
              (let ((this-day (timelog-read-date)))
                (unless (string= last-day this-day)
                  (cl-incf total-days)
                  (setq last-day this-day)))
              (cl-destructuring-bind (start-time project)
                  (timelog-read-time-and-project)
                (beginning-of-line 2)
                (let ((stop-time (timelog-read-time t)))
                  (setq time-list
                        (add-to-time-list
                         project (- stop-time start-time) time-list))))
              (beginning-of-line 2)))))
      (unless extant-timelog-buffer (kill-buffer (current-buffer))))
    (setq time-list (sort time-list #'(lambda (a b) (> (cdr a) (cdr b)))))
    (list first-day last-day total-days time-list)))

(defun timelog-do-summarize-range (first-day last-day) ;; date-strings
  (let ((time-list (list))
        (current-day "")
        (total-days 0)
        (extant-timelog-buffer (find-buffer-visiting timeclock-file)))
    (with-current-buffer (find-file-noselect timeclock-file t)
      (save-excursion
        (save-restriction
          (when (timelog-narrow-to-date-range first-day last-day)
            (while (not (eobp))
              (let ((this-day (timelog-read-date)))
                (unless (string= current-day this-day)
                  (cl-incf total-days)
                  (setq current-day this-day)))
              (cl-destructuring-bind (start-time project)
                  (timelog-read-time-and-project)
                (beginning-of-line 2)
                (let ((stop-time (timelog-read-time t)))
                  (setq time-list
                        (add-to-time-list
                         project (- stop-time start-time) time-list))))
              (beginning-of-line 2)))))
      (unless extant-timelog-buffer (kill-buffer (current-buffer))))
    (setq time-list (sort time-list #'(lambda (a b) (> (cdr a) (cdr b)))))
    (list total-days time-list)))

(defun time-list-sum (time-list)
  (cl-reduce #'+ time-list :initial-value 0 :key #'cdr))

(defun generate-time-table (time-list)
  (concat
   (format " Time spent  Project\n")
   (format " ----------  -------\n")
   (cl-reduce #'(lambda (table project-and-time)
                  (cl-destructuring-bind (project . time) project-and-time
                    (concat
                     (format "%11s  %s\n" (timelog-seconds-to-time time) project)
                     table)))
              (reverse time-list)
              :initial-value "\n")
   (format "      Total\n")
   (format " ----------\n")
   (format "%11s\n" (timelog-seconds-to-time (time-list-sum time-list)))))

(defun generate-time-table-csv (time-list title)
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

(defun timelog-generate-day-report (date-string
                                    start-time stop-time
                                    time-list)
  "Print a day activity report."
  (let ((title  (format "Report for time spent %s; between %s and %s."
                        date-string
                        (timelog-seconds-to-time start-time)
                        (timelog-seconds-to-time stop-time))))
    (cond ((eq timelog-summary-format 'csv)
           (generate-time-table-csv time-list title))
          (t
           (concat
            (format "________________________________\n%s\n\n" title)
            (generate-time-table time-list)
            (format "________________________________\n"))))))

(defun timelog-generate-month-report (first-day last-day total-days time-list)
  "Print a monthly activity report."
  (let ((title (format "Report for time spent between %s and %s; total of %d days."
                       first-day last-day total-days)))
    (cond ((eq timelog-summary-format 'csv)
           (generate-time-table-csv time-list title))
          (t
           (concat
            (format "________________________________\n%s\n\n" title)
            (generate-time-table time-list)
            (format "________________________________\n"))))))

(defun timelog-add-slashes-to-date (date-string)
  (if (= 6 (length date-string))
      (concat (substring date-string 0 4) "/" (substring date-string 4))
    (concat (substring date-string 0 4) "/"
            (substring date-string 4 6) "/"
            (substring date-string 6))))

(defun timelog-summarize-day (date-string) ;; YYYYMMDD
  (interactive "sDate [YYYYMMDD]: ")
  (setq date-string (timelog-add-slashes-to-date date-string))
  (cl-destructuring-bind (start-time stop-time projects)
      (timelog-do-summarize-day date-string)
    (if (null projects)
        (message "No entries for date %s in %s" date-string timeclock-file)
      (insert (timelog-generate-day-report
               date-string start-time stop-time projects)))))

(defun timelog-summarize-today ()
  (interactive)
  (cl-destructuring-bind (_s _m _h day month year . ignored)
      (decode-time)
    (timelog-summarize-day (format "%d%02d%02d" year month day))))

(defun timelog-summarize-month (month-string) ;; YYYYMM
  (interactive "sMonth [YYYYMM]: ")
  (setq month-string (timelog-add-slashes-to-date month-string))
  (cl-destructuring-bind (start-date stop-date total-days projects)
      (timelog-do-summarize-month month-string)
    (if (null projects)
        (message "No entries for month %s in %s" month-string timeclock-file)
      (insert (timelog-generate-month-report
               start-date stop-date total-days projects)))))

(defun timelog-summarize-range (first-day last-day) ;; date-strings
  (interactive "sFirst date [YYYYMMDD]: \nsLast date [YYYYMMDD]: ")
  (setq first-day (timelog-add-slashes-to-date first-day))
  (setq last-day  (timelog-add-slashes-to-date last-day))
  (cl-destructuring-bind (total-days projects)
      (timelog-do-summarize-range first-day last-day)
    (if (null projects)
	(message "No entries between dates %s and %s" first-day last-day)
      (insert (timelog-generate-month-report first-day last-day total-days projects)))))

(defun timelog-summarize-each-day-in-range (first-day last-day) ;; date-strings
  (interactive "sFirst date [YYYYMMDD]: \nsLast date [YYYYMMDD]: ")
  (setq first-day (timelog-add-slashes-to-date first-day))
  (setq last-day  (timelog-add-slashes-to-date last-day))
  (mapcar
   #'(lambda (summary)
       (cl-destructuring-bind (date-string start stop time-list) summary
	 (insert
	  (timelog-generate-day-report date-string start stop time-list))))
   (mapcar #'(lambda (date-string)
	       (cons date-string (timelog-do-summarize-day date-string)))
	   (timelog-get-dates-in-range first-day last-day))))

(defun timelog-current-project ()
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
  (cl-destructuring-bind (_s _m _h day month year . ignored) (decode-time)
    (let ((date-string (format "%d/%02d/%02d" year month day)))
      (cl-destructuring-bind (_start-time _stop-time projects)
          (timelog-do-summarize-day date-string)
        (if (null projects)
            (message "No entries for date %s in %s" date-string timeclock-file)
          (message "Total time worked today: %s" (timelog-seconds-to-time
                                                  (time-list-sum projects))))))))

;;; --------------------------------------------------------------------------
(provide 'timelog)

;;; timelog.el ends here
