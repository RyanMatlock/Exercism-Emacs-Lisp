;;; perfect-numbers.el --- perfect-numbers Exercise (exercism)  -*- lexical-binding: t; -*-

;;; Commentary:

;; stolen from
;; https://exercism.org/tracks/emacs-lisp/exercises/perfect-numbers/solutions/bkaestner
(defun factors (n)
  "Return the factors of nonzero whole number N excluding N."

  (defun factorp (maybe-factor)
    (zerop (mod n maybe-factor)))

  (cond ((> n 1)
         (let ((stop (floor (sqrt n)))
               (maybe-factor 2)
               (factors (list 1)))
           (while (<= maybe-factor stop)
             ;; this is all very side effect-y
             (cond ((and (factorp maybe-factor)
                         ;; check if (= maybe-factor (sqrt n))
                         (= n (* maybe-factor maybe-factor)))
                    (push maybe-factor factors))
                   ((factorp maybe-factor)
                    (push maybe-factor factors)
                    (push (/ n maybe-factor) factors)))
             ;; as is this
             (setq maybe-factor (1+ maybe-factor)))
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
