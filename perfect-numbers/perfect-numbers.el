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
           (seq-filter #'(lambda (x) (factorp n x)) possible-factors)))
        (t (error "N must be an integer > 0."))))

(defun classify (number)
;;; Code:
  )

(provide 'perfect-numbers)
;;; perfect-numbers.el ends here
