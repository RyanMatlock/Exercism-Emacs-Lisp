;;; crypto-square.el --- Crypto Square (exercism)  -*- lexical-binding: t; -*-

;;; Commentary:

(defun normalize (text)
  "Downcase and remove spaces and punctuation from string TEXT."
  (cond ((stringp text)
         (mapconcat #'string
                    (seq-filter
                     #'(lambda (c) (string-match-p "[a-z0-9]" (string c)))
                     (downcase text))
                    ""))
        (t (error "TEXT must be a string."))))

(defun rectangle-values (seq)
  "For sequence SEQ, find the smallest integer C such that:
    R * C >= (LENGTH SEQ),
    C >= R, AND
    C - R <= 1
and return them as (CONS C R).

Note that C is for columns and R is for rows."
  (cond ((sequencep seq)
         (let* ((len (length seq))
                (c (ceiling (sqrt len))))
           (if (= (* c c) len)
               (cons c c)
             (cons c (1- c)))))
        (t (error "SEQ must be a sequence."))))

(defun encipher (plaintext)
;;; Code:
  )

(provide 'crypto-square)
;;; crypto-square.el ends here
