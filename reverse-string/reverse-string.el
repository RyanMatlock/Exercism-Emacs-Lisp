;;; reverse-string.el --- Reverse String (exercism)  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:


(defun reverse-string (value)
  "Reverse string VALUE without using the built-in `reverse' function."

  (defun reverse-helper (char-list acc)
    (if char-list
        (reverse-helper (cdr char-list) (cons (car char-list) acc))
      (mapconcat #'string acc "")))

  (reverse-helper (seq-map #'identity value) '()))


(provide 'reverse-string)
;;; reverse-string.el ends here
