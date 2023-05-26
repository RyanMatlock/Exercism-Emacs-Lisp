;;; flatten-array.el --- Flatten Array (exercism)  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:


(defun list-flatten (list)

  ;; (defun lf-helper (list acc)
  ;;   (let ((x (car-safe list))
  ;;         (xs (cdr-safe list)))
  ;;     (print (format (concat "list: %s\n"
  ;;                            "acc: %s")
  ;;                    list acc))
  ;;     (cond ((and (null x) (null xs)) (reverse acc))
  ;;           ((and x (atom x)) (lf-helper xs (cons x acc)))
  ;;           ((null x) (lf-helper xs acc))
  ;;           ((listp x) (lf-helper xs (append (lf-helper x '()) acc))))))

  (defun lf-helper (list acc)
    (let ((x (car-safe list))
          (xs (cdr-safe list)))

      (defun debug-state (state-name)
        (print (format (concat "\t\t--- %s ---\n"
                               "x: %s\t\t\t\t"
                               "xs: %s\n"
                               "list: %s\t\t\t"
                               "acc: %s")
                       state-name x xs list acc)))

      (cond ((and (null x) (null xs))
             (debug-state "base case")
             (reverse acc))
            ((atom x)
             (debug-state "(atom x)")
             (lf-helper xs (cons x acc)))
            ((listp x)
             (debug-state "(listp x)")
             (append (lf-helper x acc) (lf-helper xs '()))))))

  (seq-filter #'identity (lf-helper list '())))


(provide 'flatten-array)
;;; flatten-array.el ends here
