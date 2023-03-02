;;; roman-numerals.el --- roman-numerals Exercise (exercism)  -*- lexical-binding: t; -*-

;;; Commentary:

(defun to-roman (value)
  (let* ((roman-numeral-alist '((0 . "")
                                (1 . "I")
                                (5 . "V")
                                (10 . "X")
                                (50 . "L")
                                (100 . "C")
                                (500 . "D")
                                (1000 . "M")))
         ;; stolen from https://emacs.stackexchange.com/a/33626 when looking
         ;; for a standard function for this operation
         ;; I can skip the first element
         (roman-base-values (mapcar #'car (cdr roman-numeral-alist))))
    (defun simple-roman-lookup (simple-number)
      "Look up non-compound Roman numerals."
      (alist-get simple-number roman-numeral-alist))
    (defun roman-oom-and-multple (n)
      "Calculate the Roman numeral 'order of magnitude,' which goes in 5s
instead of 10s, as well as the multple of that OOM and return it as an alist in
the form (OOM . multiple)"
      ;; I need the base values in descending order because I need to pick off
      ;; the biggest chunks first
      (let ((base-values (reverse roman-base-values)))
        (defun oom-mult-helper (n base-values)
          (let* ((oom (car base-values))
                 (quotient (/ n oom)))
            (if (> quotient 0)
                (cons oom quotient)
              (oom-mult-helper n (cdr base-values)))))
        (oom-mult-helper n base-values))))
  )

;; -- IELM testing --
;; ELISP> (simple-roman-lookup 500)
;; "D"
;; ELISP> (roman-oom-and-multple 49)
;; (10 . 4)
;; ELISP> (roman-oom-and-multple 9)
;; (5 . 1)
;; ELISP> (roman-oom-and-multple 4)
;; (1 . 4)
;; this might not be the right strategy given the way 9 gets broken up

(provide 'roman-numerals)
;;; roman-numerals.el ends here
