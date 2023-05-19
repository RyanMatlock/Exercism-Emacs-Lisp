;;; roman-numerals.el --- roman-numerals Exercise (exercism)  -*- lexical-binding: t; -*-

;;; Commentary:

(require 'cl)

(defun num-to-alist (num)
  "Convert NUM to an alist of the form ((digit . order-of-magnitude) ...)."
  (let* ((num-string (format "%d" num))
         (size (length num-string))
         (powers (number-sequence (1- size) 0 -1)))
    (seq-mapn #'(lambda (digit power)
                  (cons (- digit ?0)
                        (expt 10 power)))
              num-string
              powers)))

(defun digit-oom-to-roman (digit-oom-ccell)
  (let ((digit (car digit-oom-ccell))
        (oom (cdr digit-oom-ccell))
        (roman-numeral-alist '((0 . 0)
                               (1 . ?I)
                               (5 . ?V)
                               (10 . ?X)
                               (50 . ?L)
                               (100 . ?C)
                               (500 . ?D)
                               (1000 . ?M))))
    (cond ((member digit '(0 1 2 3))
           (make-string digit (alist-get oom roman-numeral-alist)))
          ((member digit '(5 6 7 8)) ;;
           )
          ((= digit 4) ;;
           )
          ((= digit 9) ;;
           )
          (t (error "You shouldn't be here")))
    ))

(defun to-roman (value)
  (mapconcat #'digit-oom-to-roman (num-to-alist value) ""))

(provide 'roman-numerals)
;;; roman-numerals.el ends here
