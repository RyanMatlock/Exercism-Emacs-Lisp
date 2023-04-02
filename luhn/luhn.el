;;; luhn.el --- Luhn exercise (exercism)  -*- lexical-binding: t; -*-

;;; Commentary:

(defun valid-luhn-char-p (c)
  "For char C, return T if it is a digit or space; otherwise, return NIL."
  (let ((valid-chars
         (append '(" ") (mapcar #'string (number-sequence ?0 ?9)))))
    (seq-some #'(lambda (char) (string= char (string c))) valid-chars)))

(defun remove-all-spaces (str)
  "Remove all spaces from string STR."
  (let ((space-char (string-to-char " ")))
    (mapconcat #'string
               (seq-filter #'(lambda (char) (not (equal space-char char))) str)
               "")))

(defun zip-seqs-alist (xs ys)
  "Combine each element of sequence XS with each element of sequence YS in an
alist; if YS is shorter than XS, YS will be repeated enough times to exceed the
length of XS before combining into the alist."
  (defun zipper (ps qs)
    (seq-mapn #'(lambda (p q) (cons p q)) ps qs))
  (cond ((not (sequencep xs)) (error "XS must be a sequence."))
        ((not (sequencep ys)) (error "YS must be a sequence."))
        ((> (length xs) (length ys))
         (let
             ;; note that (>= (length new-ys) (length xs)), and that's ok
             ((new-ys (apply #'append (make-list (length xs) ys))))
           (zipper xs new-ys)))
        (t (zipper xs ys))))

(defun luhn-p (str)
  "Apply Luhn algorithm to STR: starting from the *right*, double every other
number; if the result is greater than 9, subtract 9; sum the resulting list,
and if the sum is evenly divisible by 10, return T; otherwise, return NIL."
  (let* ((min-length 1)
         (luhn-max 9)
         (luhn-mult 2)
         (luhn-divisor 10)
         (str-no-spaces (remove-all-spaces str))
         (digits
          (reverse (mapcar #'(lambda (c) (string-to-number (string c)))
                           (seq-filter #'valid-luhn-char-p str-no-spaces)))))

    (defun luhn-helper (digits double-p acc)
      (let ((digit (car digits)))
        (cond ((and digit double-p)
               (let* ((luhn-intermediate-result (* digit luhn-mult))
                      (luhn-result (if (> luhn-intermediate-result luhn-max)
                                       (- luhn-intermediate-result luhn-max)
                                     luhn-intermediate-result)))
                 (luhn-helper (cdr digits) nil (cons luhn-result acc))))
              (digit (luhn-helper (cdr digits) t (cons digit acc)))
              (t acc))))

    (cond ((length= str-no-spaces (length digits))
           (and digits
                (> (length digits) min-length)
                (zerop (mod (apply #'+ (luhn-helper digits nil '()))
                            luhn-divisor))))
          (t (error (format "Invalid character(s) in '%s'" str))))))

(provide 'luhn)
;;; luhn.el ends here
