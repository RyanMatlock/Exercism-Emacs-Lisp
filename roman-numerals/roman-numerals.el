;;; roman-numerals.el --- roman-numerals Exercise (exercism)  -*- lexical-binding: t; -*-

;;; Commentary:

(defun to-roman (value)
  (defun simple-roman-lookup (simple-number)
    "Look up non-compound Roman numerals."
    (let ((roman-numeral-alist '((0 . "")
                                 (1 . "I")
                                 (5 . "V")
                                 (10 . "X")
                                 (50 . "L")
                                 (100 . "C")
                                 (500 . "D")
                                 (1000 . "M"))))
      (cdr (assoc simple-number roman-numeral-alist))))
)

(provide 'roman-numerals)
;;; roman-numerals.el ends here
