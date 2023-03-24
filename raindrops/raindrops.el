;;; raindrops.el --- Raindrops (exercism)  -*- lexical-binding: t; -*-

;;; Commentary: I'm (probably) overcomplicating this by using an alist, but I
;;; want to solve this in such a way that if you wanted to add 20 more factors,
;;; you'd only have to update the alist. Solving this with a cond and figuring
;;; out all the permutation of factors is messier.

;;; Actually, I take it back; this is a really nice way to do it that's both
;;; relatively elegant and concise.

(defun factorp (num divisor)
  "Return T if DIVISOR is a factor of NUM; otherwise return NIL; error if
DIVISOR is 0."
  (cond ((equal 0 divisor) (error "DIVISOR cannot be 0."))
        (t (equal 0 (mod num divisor)))))

(defun convert (n)
  "Convert integer N to its raindrops string."
  (let* ((factors-alist
         ;; note that order matters here -- check for smaller factors first
         '((3 . "Pling")
           (5 . "Plang")
           (7 . "Plong")))
         (raindrops  (mapconcat #'(lambda (factor-alist)
                                    (let ((possible-factor (car factor-alist))
                                          (value (cdr factor-alist)))
                                      (if (factorp n possible-factor)
                                          value
                                        "")))
                                factors-alist
                                "")))
    (if (string= "" raindrops)
        n
      raindrops)))

(provide 'raindrops)
;;; raindrops.el ends here
