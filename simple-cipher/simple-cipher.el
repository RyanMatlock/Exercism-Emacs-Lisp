;;; simple-cipher.el --- Simple Cipher (exercism)  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:


(defconst sc--num-letters 26)
(defconst sc--min-random-key-length 100)

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
  "Convert string STR to a list of alphabetical indices.

e.g. \"abcd\" => '(0 1 2 3)"
  (seq-map #'(lambda (char) (- char ?a)) str))

(defun sc--alpha-indices-to-string (xs)
  "Convert alphabetical indices XS to a string."
  (mapconcat #'string (mapcar #'(lambda (x) (+ x ?a)) xs) ""))

(defun sc--rotate (text rotations)
  "Apply rotational cipher to string TEXT using the values in list of numbers
ROTATIONS."
  (unless (= (length text) (length rotations))
    (error "TEXT and ROTATIONS must be the same length."))
  (let ((xs (sc--string-to-alpha-indices text))
        (period sc--num-letters))
    (sc--alpha-indices-to-string
     (seq-mapn #'(lambda (x rot) (mod (+ x rot) period)) xs rotations))))

(defun encode (plaintext key)
  "Apply a rotational cipher to lowercase string PLAINTEXT using the characters
of lowercase string KEY."
  (let* ((key (sc--repeat-or-trim-key plaintext key))
         (encode-rots (sc--string-to-alpha-indices key)))
    (sc--rotate plaintext encode-rots)))

(defun decode (ciphertext key)
  "Apply a rotational cipher to lowercase string CIPHERTEXT using the
characters of lowercase string KEY using KEY's alphabetical complement
(i.e. for each K in KEY, (= (+ K Q) 26) => t, where Q is the alphabetical
complement)."
  (let* ((encode-key (sc--repeat-or-trim-key ciphertext key))
         (decode-rots (mapcar #'(lambda (x) (- sc--num-letters x))
                              (sc--string-to-alpha-indices encode-key))))
    (sc--rotate ciphertext decode-rots)))

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
