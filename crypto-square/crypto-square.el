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
           (if (>= (* c (1- c)) len)
               (cons c (1- c))
             (cons c c))))
        (t (error "SEQ must be a sequence."))))

(defun normalized-text-to-block (text)
  "Convert string TEXT to a list of strings of length R, given by the
(CAR (RECTANGLE-VALUES TEXT))."
  ;; padding shouldn't change the values of r and c, so we can recalculate r
  ;; here
  (let* ((rect-vals (rectangle-values text))
         (r (car rect-vals)))
    (defun nt2b-helper (text acc)
      (if (not (zerop (length text)))
          (nt2b-helper (substring text r) (cons (substring text 0 r) acc))
        (reverse acc)))
    (nt2b-helper text '())))

(defun block-to-ciphertext (block &optional sep)
  "Convert BLOCK (a list of strings of identical size) to cipher text separated
by SEP, which defaults to the space character."
  (defun b2ct-helper (bs block index acc)
    (if (< index bs)
        (let ((chunk
               (mapconcat
                #'string
                (mapcar #'(lambda (str) (elt str index)) block) "")))
          (b2ct-helper bs block (1+ index) (cons chunk acc)))
      (reverse acc)))
  (let ((block-size (length (car block)))
        (sep (or sep " ")))
    (mapconcat #'identity (b2ct-helper block-size block 0 '()) sep)))

(defun encipher (plaintext)
  "Convert string PLAINTEXT to a cipher using the crypto square algorithm."
  (let* ((text (normalize plaintext))
         (rect-vals (rectangle-values text))
         (c (car rect-vals))
         (r (cdr rect-vals))
         (padded-text (string-pad text (* r c))))
    (block-to-ciphertext (normalized-text-to-block padded-text))))

(provide 'crypto-square)
;;; crypto-square.el ends here
