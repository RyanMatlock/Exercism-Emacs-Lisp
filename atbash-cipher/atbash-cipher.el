;;; atbash-cipher.el --- Atbash-Cipher (exercism)  -*- lexical-binding: t; -*-

;;; Commentary:

(defun chunk-text (chunk-size text &optional sep)
  "Break string TEXT into pieces of length CHUNK-SIZE separated by SEP."
  (let ((sep (string-to-char (or sep " ")))
        (text-list (mapcar #'identity text)))
    (defun chunk-helper (size lst count acc)
      (cond ((and lst (= chunk-size count))
             (chunk-helper size lst 0 (cons sep acc)))
            (lst (chunk-helper size (cdr lst) (1+ count) (cons (car lst) acc)))
            (t (reverse acc))))
    (mapconcat #'string (chunk-helper chunk-size text-list 0 '()) "")))

(defun encode (plaintext)
  "Encode PLAINTEXT to atbash-cipher encoding."
  ;;; Code:
)

(provide 'atbash-cipher)
;;; atbash-cipher.el ends here
