;;; perfect-numbers.el --- perfect-numbers Exercise (exercism)  -*- lexical-binding: t; -*-

;;; Commentary:

;; referenced
;; https://exercism.org/tracks/emacs-lisp/exercises/perfect-numbers/solutions/bkaestner
;; in order to come up with this solution
(defun factors (n)
  "Return the factors of nonzero whole number N excluding N."

  (defun factorp (maybe-factor)
    (zerop (mod n maybe-factor)))

  (cond ((> n 1)
         (let* ((stop (floor (sqrt n)))
                (maybe-factors (number-sequence 2 stop))
                (factors (list 1)))
           (mapc #'(lambda (maybe-factor)
                     ;; only doing this let so I don't have to do the factorp
                     ;; calculation twice
                     (let ((factorp-result (factorp maybe-factor)))
                       (cond ((and factorp-result
                                   (= n (* maybe-factor maybe-factor)))
                              ;; push maybe-factor once if it's the square root
                              ;; of n
                              (push maybe-factor factors))
                             (factorp-result (progn
                                               (push maybe-factor factors)
                                               (push (/ n maybe-factor)
                                                     factors))))))
                 maybe-factors)
           ;; not really necessary to sort them, but it's nicer for visually
           ;; inspecting the output
           (sort factors #'<)))
        (t '())))

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
