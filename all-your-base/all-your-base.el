;;; all-your-base.el --- All Your Base (exercism)  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defun rebase (list-of-digits in-base out-base)
  (defun base10-to-base-n (base10-digits n accumulator)
    "Convert digits in base-10 to base-n"
    (if base10-digits
        (let ((power (1- (length base10-digits)))
              (x (car base10-digits)))
          ;; recursive step
          (base10-to-base-n
           (cdr base10-digits)
           n
           (cons (* x (expt n power)) accumulator)))
      (apply #'+ accumulator)))
  (error "Delete this S-Expression and write your own implementation"))

(provide 'all-your-base)
;;; all-your-base.el ends here
