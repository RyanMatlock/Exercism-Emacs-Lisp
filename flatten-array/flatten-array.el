;;; flatten-array.el --- Flatten Array (exercism)  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:


(defun list-flatten (list)

  (defun lf-helper (list acc)
    (let ((x (car-safe list))
          (xs (cdr-safe list)))
      (print (format (concat "list: %s\n"
                             "acc: %s")
                     list acc))
      (cond ((and (null x) (null xs)) (reverse acc))
            ((and x (atom x)) (lf-helper xs (cons x acc)))
            ((null x) (lf-helper xs acc))
            ((listp x) (lf-helper xs (append (lf-helper x '()) acc))))))

  (lf-helper list '()))


(provide 'flatten-array)
;;; flatten-array.el ends here
