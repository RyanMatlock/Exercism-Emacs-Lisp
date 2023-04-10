;;; trinary.el --- Trinary (exercism)  -*- lexical-binding: t; -*-

;;; Commentary:

(defun trinary-to-decimal (string)
  "Convert STRING representing a trinary number to its decimal equivalent; if
STRING contains invalid characters (i.e. other than 0, 1, or 2), return 0."
  (cond ((= (length string)
            (length (seq-filter #'(lambda (c) (or (= c ?0)
                                                  (= c ?1)
                                                  (= c ?2)))
                                string)))
         (let ((base 3)
               (power (1- (length string))))
           (apply #'+ (seq-mapn #'(lambda (n index)
                                    (* (string-to-number (string n))
                                       (expt 3 (- power index))))
                                string
                                (number-sequence 0 power)))))
        (t 0)))

(provide 'trinary)
;;; trinary.el ends here
