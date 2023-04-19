;;; leap.el --- Leap exercise (exercism)  -*- lexical-binding: t; -*-

;;; Commentary:

(defun divisorp (number divisor)
  "Return T if NUMBER is evenly divisible by DIVISOR; else return nil; error if
number and divisor aren't integers."
  (cond ((not (integerp number)) (error "NUMBER must be an integer."))
        ((not (integerp divisor)) (error "DIVISOR must be an integer."))
        (t (zerop (mod number divisor)))))

(defun leap-year-p (year)
  "Determine if a YEAR is a leap year."
  (cond ((integerp year)
         (or (divisorp year 400)
             (and (not (divisorp year 100))
                  (divisorp year 4))))
        (t (error "YEAR must be an integer."))))

(provide 'leap-year-p)
;;; leap.el ends here
