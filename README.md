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


# Credit

This is mostly the work of [Markus Flambard](https://gist.github.com/flambard/419770#file-timelog-el)
with some changes to modernize the code and add CSV format support.
