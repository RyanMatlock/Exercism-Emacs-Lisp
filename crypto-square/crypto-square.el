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
         (block-size (car rect-vals))
         (text-len (length text))
         (starts (number-sequence 0 (- text-len block-size) block-size))
         (ends (number-sequence block-size text-len block-size))
         (indices-alist (seq-mapn #'(lambda (x y) (cons x y))
                                  starts
                                  ends)))
    (seq-mapn #'(lambda (indices) (let ((start (car indices))
                                        (end (cdr indices)))
                                    (substring text start end)))
              indices-alist)))

(defun block-to-ciphertext (block &optional sep)
  "Convert BLOCK (a list of strings of identical size) to cipher text separated
by SEP, which defaults to the space character."
  (let* ((block-size (length (car block)))
         (sep (or sep " "))
         (indices (number-sequence 0 (1- block-size))))
    (mapconcat #'(lambda (i)
                   (mapcar #'(lambda (str) (elt str i)) block))
               indices
               sep)))

;; Exercism complained about my use of string-pad in my original solution
(defun cb--string-pad (str n &optional padding)
  "Pad string STR with string PADDING (default: space character) so it reaches
length N; if (< N (LENGTH STR)), return STR unchanged; error if STR isn't a
string or N isn't a whole number."
  (cond ((and (stringp str) (wholenump n))
         (let ((padding (or padding " "))
               (diff (- n (length str))))
           (if (> diff 0)
               (mapconcat #'identity
                          (append (seq-mapn #'string str)
                                  (make-list diff padding))
                          "")
             str)))
        ((not (stringp str)) (error "STR must be a string."))
        ((not (wholenump n)) (error "N must be a whole number."))
        (t (error "Something went wrong."))))

(defun encipher (plaintext)
  "Convert string PLAINTEXT to a cipher using the crypto square algorithm."
  (let* ((text (normalize plaintext))
         (rect-vals (rectangle-values text))
         (c (car rect-vals))
         (r (cdr rect-vals))
         (padded-text (cb--string-pad text (* r c))))
    (block-to-ciphertext (normalized-text-to-block padded-text))))

(provide 'crypto-square)
;;; crypto-square.el ends here
