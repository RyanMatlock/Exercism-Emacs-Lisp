;;; luhn.el --- Luhn exercise (exercism)  -*- lexical-binding: t; -*-

;;; Commentary:

(defun string-to-number-or-error (s)
  "Converts string S to a number or raises an error."
  (let ((s2n (string-to-number s)))
    (if (and (zerop s2n) (not (string= "0" s)))
        (error (format "'%s' is not a number." s))
      s2n)))

(defun luhn-p (str)
  "Apply Luhn algorithm to STR: double every other number; if the result is
greater than 9, subtract 9; sum the resulting list, and if the sum is evenly
divisible by 10, return T; otherwise, return NIL."
  (let ((min-length 1)
        (luhn-max 9)
        (luhn-mult 2)
        (digits
         (reverse (mapcar #'string-to-number-or-error
                          (seq-filter
                           #'(lambda (s) (not (string= " " s)))
                           (mapcar #'string str)))))
        (luhn-divisor 10))
    (defun luhn-helper (digits double-p acc)
      (let ((digit (car digits)))
        (cond ((and digit double-p)
               (let* ((luhn-intermediate-result (* digit luhn-mult))
                      (luhn-result (if (> luhn-intermediate-result luhn-max)
                                       (- luhn-intermediate-result luhn-max)
                                     luhn-intermediate-result)))
                 (luhn-helper (cdr digits) nil (cons luhn-result acc))))
              (digit (luhn-helper (cdr digits) t (cons digit acc)))
              (t acc))))
    ;; (print (format "digits: %s" digits))
    (and digits
         (> (length digits) min-length)
         (zerop (mod (apply #'+ (luhn-helper digits nil '())) luhn-divisor)))))

(provide 'luhn)
;;; luhn.el ends here
