;;; simple-cipher.el --- Simple Cipher (exercism)  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:


(defconst sc--min-random-key-length 100)

(defun encode (plaintext key)
  (error
   "Delete this S-Expression and write your own implementation"))

(defun decode (ciphertext key)
  (error
   "Delete this S-Expression and write your own implementation"))

(defun generate-key ()
  "Generate a random lowercase key of length 'sc--min-random-key-length'."
  (let ((key (make-string sc--min-random-key-length ?a)))
    ;; random returns an integer in the range [0, limit), so ?a + 0 = ?a and
    ;; ?a + (1- 26) = ?z, so all letters are possible
    (mapconcat #'string
               (seq-map #'(lambda (c) (+ c (random sc--num-letters))) key)
               "")))


(provide 'simple-cipher)
;;; simple-cipher.el ends here
