;;; leap.el --- Leap exercise (exercism)  -*- lexical-binding: t; -*-

;;; Commentary:

(defun leap-year-p (year)
  "Determine if a year is a leap year."
  (defun divisible-by-p (number divisor)
    "Determine if number is divisible by divisor."
    (eq (mod number divisor) 0))
  (cond ((divisible-by-p year 400) t)
        ((divisible-by-p year 100) nil)
        ((divisible-by-p year 4) t)
        (t nil)))

;; -- IELM testing --
;; ELISP> (divisible-by-p 21 7)
;; t
;; ELISP> (divisible-by-p 22 7)
;; nil

(provide 'leap-year-p)
;;; leap.el ends here
