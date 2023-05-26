;;; affine-cipher.el --- Affine Cipher (exercism)  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:


(defun ac--gcd (p q)
  "Apply the Euclidean GCD algorithm on whole numbers P and Q."
  (cond ((or (zerop p) (zerop q)) (max p q))
        (t (let ((diff (abs (- p q)))
                 (smaller (min p q)))
             (ac--gcd diff smaller)))))

(defun ac--coprimep (n m)
  "Return t if whole numbers N and M have no common factors besides 1."
  (unless (and (wholenump n) (wholenump m))
    (error "N and M must be whole numbers."))
  (eq 1 (ac--gcd n m)))

(defun encode (phrase key)
  (let ((a (alist-get "a" key nil nil #'string=))
        (b (alist-get "b" key nil nil #'string=))
        (m 26)
        (group-size 5)
        (validated-phrase
         (replace-regexp-in-string "[^a-z0-9]+" "" (downcase phrase))))

    (unless (ac--coprimep a m)
      (error "a and m must be coprime."))

    (defun encode-number (i)
      (mod (+ (* a i) b) m))

    (defun encode-char (c)
      (let* ((lowercase (number-sequence ?a ?z)))
        (cond ((member c lowercase)
               (let ((c-offset (car lowercase)))
                 (+ (encode-number (- c c-offset)) c-offset)))
              (t c))))

    (mapconcat #'identity
               (seq-map #'(lambda (chunk) (mapconcat #'string chunk ""))
                        (seq-partition (seq-map #'encode-char validated-phrase)
                                       group-size))
               " ")))

(defun decode (phrase key)
  ;; (error
  ;;  "Delete this S-Expression and write your own implementation")
  )


(provide 'affine-cipher)
;;; affine-cipher.el ends here
