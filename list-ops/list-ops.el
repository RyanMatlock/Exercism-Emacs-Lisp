;;; list-ops.el --- List Ops (exercism)  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:


(defun list-foldl (fun list accu)
  (error "Delete this S-Expression and write your own implementation"))

(defun list-foldr (fun list accu)
  (error "Delete this S-Expression and write your own implementation"))

(defun list-empty-p (list)
  "Return T if LIST is empty; otherwise return NIL."
  (if list
      nil
    t))

(defun list-sum (list)
  "Sum up the elements in LIST."
  ;; I'm assuming they're not looking for me to use (apply #'+ list) here even
  ;; though that's the idiomatic way to do it
  (defun sum-helper (list total)
    (if list
        (sum-helper (cdr list) (+ (car list) total))
      total))
  (sum-helper list 0))

(defun list-length (list)
  "Return the number of top-level elements in LIST."
  (defun length-helper (list len)
    (if list
        (length-helper (cdr list) (1+ len))
      len))
  (length-helper list 0))

(defun list-append (list1 list2)
  (error "Delete this S-Expression and write your own implementation"))

(defun list-reverse (list)
  "Reverse the elements of LIST."
  (defun reverse-helper (list acc)
    (if list
        (reverse-helper (cdr list) (cons (car list) acc))
      acc))
  (reverse-helper list '()))

(defun list-concatenate (list1 list2 &rest LISTS)
  (error "Delete this S-Expression and write your own implementation"))

(defun list-filter (list predicate)
  (error "Delete this S-Expression and write your own implementation"))

(defun list-map (list fun)
  "Return list where FUN has been applied to all elements of LIST."
  (defun map-helper (list fun acc)
    (if list
        (map-helper (cdr list) fun (cons (funcall fun (car list)) acc))
      (list-reverse acc)))
  (map-helper list fun '()))

(provide 'list-ops)
;;; list-ops.el ends here
