;;; list-ops.el --- List Ops (exercism)  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:


(defun list-foldl (fun list accu)
  "Apply FUN, a function of two arguments, to each element of LIST and
ACCU. The function is applied as
  (FUN ACCU item).
Note that the direction matters for non-associative functions."
  (let ((x (car list)))
    (if list
        (list-foldl fun (cdr list) (funcall fun accu x))
      accu)))

(defun list-foldr (fun list accu)
  "Apply FUN, a function of two arguments, to each element of LIST and ACCU,
starting from the right of LIST. The function is applied as
  (FUN item ACCU).
Note that the direction matters for non-associative functions."
  (defun foldr-helper (fun list accu index)
    (if (>= index 0)
        (let* ((elem (nth index list))
               (new-accu (funcall fun elem accu)))
          ;; (print (format (concat "index: %d\n"
          ;;                        "\telem: %d\n"
          ;;                        "\taccu: %d\n"
          ;;                        "\tnew-accu: %d")
          ;;                index elem accu new-accu))
          (foldr-helper
           fun list new-accu (1- index)))
      accu))
  (foldr-helper fun list accu (1- (length list))))

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
  "Add elements of LIST2 to the end of LIST1."
  (defun append-helper (xs acc)
    (if xs
        (append-helper (cdr xs) (cons (car xs) acc))
      (list-reverse acc)))
  (append-helper list2 (list-reverse list1)))

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
  "Return a list containing only elements of LIST where PREDICATE is T."
  (defun filter-helper (list predicate acc)
    (let ((elem (car list)))
      (cond ((not elem) (reverse acc))
            ((funcall predicate elem)
             (filter-helper (cdr list) predicate (cons elem acc)))
            (t (filter-helper (cdr list) predicate acc)))))
  (filter-helper list predicate '()))

(defun list-map (list fun)
  "Return list where FUN has been applied to all elements of LIST."
  (defun map-helper (list fun acc)
    (if list
        (map-helper (cdr list) fun (cons (funcall fun (car list)) acc))
      (list-reverse acc)))
  (map-helper list fun '()))

(provide 'list-ops)
;;; list-ops.el ends here
