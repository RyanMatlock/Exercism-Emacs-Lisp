;;; gigasecond.el --- Gigasecond exercise (exercism)  -*- lexical-binding: t; -*-

;;; Commentary:
;; Calculate the date one gigasecond (10^9 seconds) from the
;; given date.
;;
;; NB: Pay attention to  Emacs' handling of time zones and dst
;; in the encode-time and decode-time functions.

(defun from (second minute hour day month year)
  "Add 10^9 seconds to the timestamp specified by SECOND, MINUTE, HOUR, DAY,
MONTH, and YEAR."
  (let* ((args (list second minute hour day month year))
         (t-init (make-decoded-time
                  :second second
                  :minute minute
                  :hour hour
                  :day day
                  :month month
                  :year year))
         (giga (expt 10 9))
         (gigasecond (make-decoded-time :second giga)))
    (take (length args) (decoded-time-add t-init gigasecond))))

(provide 'gigasecond)
;;; gigasecond.el ends here
