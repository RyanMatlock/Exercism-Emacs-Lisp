;;; perfect-numbers.el --- perfect-numbers Exercise (exercism)  -*- lexical-binding: t; -*-

;;; Commentary:

(defun factors (n)
  "Return the factors of nonzero whole number N excluding N; error if N doesn't
satisfy the numerical requirements."
  (defun factorp (n factor)
    (zerop (mod n factor)))
  (cond ((and (wholenump n) (not (zerop n)))
         (let* ((stop (max 1 (/ n 2)))
                (possible-factors (number-sequence 1 stop)))
           (seq-filter #'(lambda (x) (and (factorp n x)
                                          (not (= n x))))
                       possible-factors)))
        ;; error handling not actually necessary
        (t (error "N must be an integer > 0."))))

(defun aliquot-sum (n)
  "Return the sum of the factors of N excluding N itself."
  (apply #'+ (factors n)))

(defun classify (number)
  "Classify NUMBER as 'PERFECT if it equal to its aliquot sum; 'ABUNDANT if it
is less than its aliquot sum, and 'DEFICIENT if it is greater than its aliquot
sum."
  (cond ((and (wholenump number)
              (not (zerop number)))
         (let ((aliquot (aliquot-sum number)))
           (cond ((= number aliquot) 'perfect)
                 ((< number aliquot) 'abundant)
                 ((> number aliquot) 'deficient)
                 (t (error "Not sure what happened here.")))))
        (t (error "Classification is only possible for natural numbers"))))

(provide 'perfect-numbers)
;;; perfect-numbers.el ends here
