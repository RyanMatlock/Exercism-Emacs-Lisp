;;; all-your-base.el --- All Your Base (exercism)  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defun base-n-to-base-10 (digits base-n)
  "Convert DIGITS from BASE-N to base 10 (output is a number)."
  (defun n-to-10-helper (digits base-n acc)
    (if digits
        (let* ((digit (car digits))
               (power (1- (length digits)))
               (base-10 (* digit (expt base-n power))))
          ;; (print (format (concat "digit: %d\t"
          ;;                        "power: %d\t"
          ;;                        "base-10: %d\t"
          ;;                        "acc: %d")
          ;;                digit power base-10 acc))
          (n-to-10-helper (cdr digits) base-n (+ base-10 acc)))
      acc))
  (n-to-10-helper digits base-n 0))

(defun rebase (list-of-digits in-base out-base)
  "Convert LIST-OF-DIGITS from base IN-BASE to base OUT-BASE."
  (error "Delete this S-Expression and write your own implementation"))

(provide 'all-your-base)
;;; all-your-base.el ends here
