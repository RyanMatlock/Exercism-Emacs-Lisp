;;; sublist.el --- Sublist (exercism)  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defun list-classify (list1 list2)
  "Determine if list1 is equal to, a sublist of, a superlist of, or unequal to
list2."
  (defun compare-equal-length-lists (xs ys)
    "Determine if lists of equal length have identical elements in the same
order, in which case they're :equal; otherwise, return :unequal."
    (cond ((eq (length xs) (length ys))
           (defun compare-equal-length-lists-helper (xs ys)
             (let ((x (car xs))
                   (y (car ys)))
               (cond ((and x y (eq x y))
                      (compare-equal-length-lists-helper
                       (cdr xs)
                       (cdr ys)))
                     ((not (eq x y)) :unequal)
                     (t :equal)))))
          (t (error "Lists are of unequal length.")))
    (compare-equal-length-lists-helper xs ys))
  (compare-equal-length-lists list1 list2))

;; -- IELM testing --
;; ELISP> (compare-equal-length-lists '("foo" "bar") '("bar" "foo"))
;; :unequal
;; ELISP> (compare-equal-length-lists '(1 2 3) '(1 2 3))
;; :equal


(provide 'sublist)
;;; sublist.el ends here
