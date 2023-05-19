;;; roman-numerals.el --- roman-numerals Exercise (exercism)  -*- lexical-binding: t; -*-

;;; Commentary:

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
        (roman-numeral-alist '(;; (0 . 0)
                               (1 . ?I)
                               (5 . ?V)
                               (10 . ?X)
                               (50 . ?L)
                               (100 . ?C)
                               (500 . ?D)
                               (1000 . ?M))))
    (cond ((member digit (append (number-sequence 0 3)
                                 (number-sequence 5 8)))
           (let ((fivep (not (zerop (/ digit 5)))) ;; t for 5 6 7 8
                 (rest (mod digit 5)))
             (concat
              (if fivep
                  (string (alist-get (* 5 oom) roman-numeral-alist))
                "")
              ;; this processing is the same for all numbers, which is why I
              ;; have  the conditional inside of here: to keep things DRY
              (make-string rest (alist-get oom roman-numeral-alist)))))
          (t (let (;; this is super cryptic, but basically, if the digit is 4,
                   ;; multiply the oom by 5 (i.e. promote it by one Roman order
                   ;; of magnitude step); otherwise, the digit is 9, so promote
                   ;; the Roman oom by two steps
                   (oom-promotion (* oom (if (= digit 4) 5 10))))
               (format "%c%c"
                       (alist-get oom roman-numeral-alist)
                       (alist-get oom-promotion roman-numeral-alist)))))))

(defun to-roman (value)
  (mapconcat #'digit-oom-to-roman (num-to-alist value) ""))

(provide 'roman-numerals)
;;; roman-numerals.el ends here
