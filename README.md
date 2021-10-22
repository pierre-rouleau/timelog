# timelog - summarize time periods created by timeclock

This is a modernized clone of [Markus Flambard timelog gist](https://gist.github.com/flambard/419770#file-timelog-el).

timelog provides commands that display and print summaries of timed activities
created by the built-in timeclock library.

This evolves timelog:
- Modernizes the code to byte-compile cleanly under Emacs 26 and later.
- Add some features: the `timelog` custom group with ability to select a report format: the normal or CSV output format.


timelog is integrated inside [PEL](https://github.com/pierre-rouleau/pel#readme)
[time tracking support](https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/time-tracking.pdf).

# Installation

There is currently no support for Elpa-compliant packages like MELPA.

To install copy the file timelog.el into a directory that is in your Emacs load-path
and add the following code inside your initialization:

``` lisp

(require 'timelog)

(autoload 'timelog-summarize-day               "timelog" nil :interactive)
(autoload 'timelog-summarize-today             "timelog" nil :interactive)
(autoload 'timelog-summarize-month             "timelog" nil :interactive)
(autoload 'timelog-summarize-range             "timelog" nil :interactive)
(autoload 'timelog-summarize-each-day-in-range "timelog" nil :interactive)
(autoload 'timelog-summarize-project           "timelog" nil :interactive)
(autoload 'timelog-summarize-elapsed           "timelog" nil :interactive)

(define-key ctl-x-map "tld" 'timelog-summarize-day)
(define-key ctl-x-map "tlt" 'timelog-summarize-today)
(define-key ctl-x-map "tlm" 'timelog-summarize-month)
(define-key ctl-x-map "tlr" 'timelog-summarize-range)
(define-key ctl-x-map "tlD" 'timelog-summarize-each-day-in-range)
(define-key ctl-x-map "tlp" 'timelog-current-project)
(define-key ctl-x-map "tle" 'timelog-workday-elapsed)

```

Or something similar.

For the moment, the easiest way is to use PEL, which installs timelog
when the `pel-use-timelog` user-option is turned on.

# Modifications over original timelog

This code include the following modification over the original code:

- Additions:
  - timelog customization group:
    - select report format with `timelog-summary-format' which can be the
      original (called visual for lack of a better word, and is the default),
      and a CSV format.
  - The CSV format has 3 filled columns with an extra columns to compute
    duration in the final spreadsheet.  The CSV has a title row which has a
    title in the fourth column describing the period.
- Code Fix:
  - The original code did not handle periods crossing midnight, reporting
    negative time duration for those.  The new code is able to handle time
    periods that cross over midnight, but only once.  The longest time period
    supported is therefore 48 hours less 1 second.
  - The same issue affected counting time for the last project of the day if
    that project crossed midnight when trying to create a report for a single
    day.  The new code detects the situation and compute tine for an opened
    ended project at the end of a day as if it was ending at midnight.  This
    way a single day report includes that period properly.
  - To ensure that all periods are properly counted the file content is
    corrected by `timelog--fix-midnight-crossings` before any report is
    created.  This inserts project termination and start over midnight to
    prevent all possibilities of duration computation errors.

- Code modifications:
  - Renamed internal functions to timelog--*SOMETHING*
  - Added docstrings to several functions but not all (yet).

# Credit

This is mostly the work of [Markus Flambard](https://gist.github.com/flambard/419770#file-timelog-el)
with some changes to modernize the code, add CSV format support and support burning the midnight oil.
