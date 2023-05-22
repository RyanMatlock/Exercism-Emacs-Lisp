;;; rotational-cipher.el --- Rotational Cipher (exercism)  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:


(defun rotate (text shift-key)
  "Take modulo 26 of the sum of number SHIFT-KEY (between 0 and 26) and each
letter char in string TEXT and return the result as a string."

  (defun shift (char)
    "If CHAR is a letter, rotate it by the appropriate amount; otherwise,
return it unchanged."
    (let* ((uppercase (number-sequence ?A ?Z))
           (lowercase (number-sequence ?a ?z))
           (first-letter (cond ((member char uppercase) (car uppercase))
                               ((member char lowercase) (car lowercase))
                               (t nil))))
      (if first-letter
          (+ (mod (+ (- char first-letter) shift-key) 26)
             first-letter)
        char)))

  (mapconcat #'string (seq-map #'shift text) ""))


(provide 'rotational-cipher)
;;; rotational-cipher.el ends here
