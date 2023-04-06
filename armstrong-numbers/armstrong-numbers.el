;;; armstrong-numbers.el --- armstrong-numbers Exercise (exercism)  -*- lexical-binding: t; -*-

;;; Commentary:

(defun wholenum-to-digits (num)
  "Turn NUM into a list of digits; error if NUM is not a whole number."
  (cond
   ((wholenump num)
    (seq-mapn #'(lambda (c) (string-to-number (string c))) (format "%d" num)))
   (t (error "NUM must be a whole number."))))

(defun armstrong-p (n)
;;; Code:
)

(provide 'armstrong-numbers)
;;; armstrong-numbers.el ends here
