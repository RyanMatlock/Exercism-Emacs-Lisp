;;; armstrong-numbers.el --- armstrong-numbers Exercise (exercism)  -*- lexical-binding: t; -*-

;;; Commentary:

(defun wholenum-to-digits (num)
  "Turn NUM into a list of digits; error if NUM is not a whole number."
  (cond
   ((wholenump num)
    (seq-mapn #'(lambda (c) (string-to-number (string c))) (format "%d" num)))
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
