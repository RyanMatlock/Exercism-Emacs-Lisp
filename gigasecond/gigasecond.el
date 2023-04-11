;;; gigasecond.el --- Gigasecond exercise (exercism)  -*- lexical-binding: t; -*-

;;; Commentary:
;; Calculate the date one gigasecond (10^9 seconds) from the
;; given date.
;;
;; NB: Pay attention to  Emacs' handling of time zones and dst
;; in the encode-time and decode-time functions.

;; aryat555's solution (lightly edited):
(defun from(second minute hour day month year)
  "Add 10^9 seconds to the timestamp specified by SECOND, MINUTE, HOUR, DAY,
MONTH, and YEAR."
  (let* ((tz "UTC")
         (num-significant-values
          (length (list second minute hour day month year)))
         (init-time (encode-time second minute hour day month year tz))
         (giga (expt 10 9))
         (new-time (time-add init-time giga))
         (new-decoded-time (decode-time new-time tz))
         (num-values-to-skip (- (length new-decoded-time)
                                num-significant-values)))
    (butlast new-decoded-time num-values-to-skip)))

(provide 'gigasecond)
;;; gigasecond.el ends here
