;;; simple-cipher.el --- Simple Cipher (exercism)  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:


(defconst sc--num-letters 26)
(defconst sc--min-random-key-length 100)

(defun sc--rotate (xs rotations period)
  (unless (= (length xs) (length rotations))
    (error "XS and ROTATIONS must be the same length."))
  (seq-mapn #'(lambda (x rot) (mod (+ x rot) period)) xs rotations))

(defun sc--repeat-or-trim-key (text key)
  "Return a string the length of string TEXT consisting of as many repeats of
string KEY as necessary."
  (let* ((text-len (length text)))
    (seq-subseq (progn
                  (while (< (length key) text-len)
                    (setq key (concat key key)))
                  key)
                0 text-len)))

(defun sc--string-to-alpha-indices (str)
  (seq-map #'(lambda (char) (- char ?a)) str))

(defun sc--alpha-indices-to-string (xs)
  (mapconcat #'string (mapcar #'(lambda (x) (+ x ?a)) xs) ""))

(defun encode (plaintext key)
  "Apply a rotational cipher to lowercase string PLAINTEXT using the characters
of lowercase string KEY."
  (let* ((key (sc--repeat-or-trim-key plaintext key))
         (plaintext-xs (sc--string-to-alpha-indices plaintext))
         (encode-rots (sc--string-to-alpha-indices key)))
    (sc--alpha-indices-to-string
     (sc--rotate plaintext-xs encode-rots sc--num-letters))))

(defun decode (ciphertext key)
  (let* ((encode-key (sc--repeat-or-trim-key ciphertext key))
         (decode-rots (mapcar #'(lambda (x) (- sc--num-letters x))
                              (sc--string-to-alpha-indices encode-key)))
         (cipher-xs (sc--string-to-alpha-indices ciphertext)))
    (sc--alpha-indices-to-string
     (sc--rotate cipher-xs decode-rots sc--num-letters))))

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
