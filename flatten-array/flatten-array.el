;;; flatten-array.el --- Flatten Array (exercism)  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:


(defun list-flatten (list)
  "Unpack nesting from LIST and filter out all nil elements."

  (defun lf-helper (list acc)
    (let ((x (car-safe list))
          (xs (cdr-safe list)))
      (cond ((and (null x) (null xs)) (reverse acc))          ;; base case
            ((atom x) (lf-helper xs (if x (cons x acc) acc))) ;; cons non-nil x
            ((listp x) (append (lf-helper x acc) (lf-helper xs '()))))))

  (lf-helper list '()))


(provide 'flatten-array)
;;; flatten-array.el ends here
