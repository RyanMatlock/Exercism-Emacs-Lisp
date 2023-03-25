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
          (n-to-10-helper (cdr digits) base-n (+ base-10 acc)))
      acc))
  (n-to-10-helper digits base-n 0))

(defun base-10-to-base-n (base-10-num base-n)
  "Convert BASE-10-NUM to a BASE-N number in the form of a list of digits.

Note that
  (base-10-to-base-n (base-n-to-base-10 digits n) n) => digits."

  (defun largest-power-of-n-in-base-10 (base-10-num base-n)
    "Return the largest number P such that
(> (/ BASE-10-NUM (expt BASE-N P)) 0)."
    (defun largest-power-helper (base10 basen pow)
      (if (> (/ base10 (expt basen pow)) 0)
          (largest-power-helper base10 basen (1+ pow))
        ;; careful here: don't return pow because it's too large
        (1- pow)))
    (largest-power-helper base-10-num base-n 0))

  (defun 10-to-n-helper (num base-n power acc)
    (if (>= power 0)
        (let* ((position (expt base-n power)) ;; think of it like 10s place
               (digit (/ num position))
               (remainder (- num (* digit position))))
          ;; note the power decrements -- it has to --  so start with the
          ;; largest power
          (10-to-n-helper remainder base-n (1- power) (cons digit acc)))
      (reverse acc)))

  (10-to-n-helper
   base-10-num
   base-n
   (largest-power-of-n-in-base-10 base-10-num base-n)
   '()))

(defun rebase (list-of-digits in-base out-base)
  "Convert LIST-OF-DIGITS from base IN-BASE to base OUT-BASE."
  (error "Delete this S-Expression and write your own implementation"))

(provide 'all-your-base)
;;; all-your-base.el ends here
