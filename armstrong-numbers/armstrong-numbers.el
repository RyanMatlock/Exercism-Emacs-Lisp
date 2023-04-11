;;; armstrong-numbers.el --- armstrong-numbers Exercise (exercism)  -*- lexical-binding: t; -*-

;;; Commentary:

(defun char-to-number (c)
  "Convert char C to number between 0 and 9; error if C is not in that range."
  (cond ((characterp c)
         (let ((maybe-digit
                ;; saw this in a community solution to another exercise
                (- c ?0)))
           (if (and (>= maybe-digit 0) (<= maybe-digit 9))
               maybe-digit
             (error "Char C out of range."))))
        (t (error "Input must be of type char."))))

(defun wholenum-to-digits (num)
  "Turn NUM into a list of digits; error if NUM is not a whole number."
  (cond
   ((wholenump num)
    (seq-mapn #'char-to-number (format "%d" num)))
   (t (error "NUM must be a whole number."))))

(defun armstrong-p (n)
  "Return T if number N is an Armstrong number, which is defined as a number
whose digits raised to the power of the number of digits and summed is equal to
the original number; otherwise, return NIL."
  (let* ((digits (wholenum-to-digits n))
         (power (length digits)))
    (= n (apply #'+ (mapcar #'(lambda (x) (expt x power)) digits)))))

(provide 'armstrong-numbers)
;;; armstrong-numbers.el ends here
