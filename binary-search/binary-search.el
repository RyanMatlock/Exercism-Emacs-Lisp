;;; binary-search.el --- Binary Search (exercism)  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:


(require 'cl)

(defun bs--safe-1+ (value)
  "If VALUE is a number, add 1 to VALUE; otherwise, return nil."
  (when (numberp value)
    (1+ value)))

(defun bs--middle-index (seq)
  (let ((len (length seq)))
    (cond ((= len 0) nil)
          ((evenp len) (1- (/ len 2))) ;; 1- is the result of 0-indexing
          ((oddp len) (/ len 2))
          (t (error "I've made a terrible mistake.")))))

(defun bs--array-bisect (arr)
  (unless (arrayp arr)
    (error "ARR must be an array."))
  (let ((length-arr (length arr)))
    (cond ((= length-arr 0) nil)
          ((= length-arr 1) (cons arr nil))
          (t (let ((middle-index (bs--middle-index arr)))
               ;; 1+ is due to the END argument of seq-subseq being exclusive
               (cons (seq-subseq arr 0 (1+ middle-index))
                     ;; 1+ is there to not doubly include the middle
                     (seq-subseq arr (1+ middle-index))))))))

(defun find-binary (array value)

  (defun fb-helper (arr val)
    (let ((larr (length arr)))
      (cond
       ((> larr 1)
        (let* ((middle-elt (elt arr (/ larr 2)))
               (bisection (bs--array-bisect arr))
               (left (car-safe bisection))
               (right (cdr-safe bisection)))
          (print (format (concat "arr: %s, val: %d\n"
                                 "middle-elt: %d\n"
                                 "smaller: %s\n"
                                 "larger: %s")
                         arr val middle-elt smaller larger))
          (if (>= val middle-elt)
              (bs--safe-1+ (fb-helper larger val))
            (bs--safe-1+ (fb-helper smaller val)))))
       ((= larr 1)
        (when (equal (elt arr 0) val) 0))
       (t nil))))

  (let ((array (sort array '<)))
    (fb-helper array value)))


(provide 'binary-search)
;;; binary-search.el ends here
