;;; binary-search.el --- Binary Search (exercism)  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:


(require 'cl)

(defun bs--safe-1+ (value)
  "If VALUE is a number, add 1 to VALUE; otherwise, return nil."
  (when (numberp value)
    (1+ value)))

(defun bs--array-bisect (arr)
  (unless (arrayp arr)
    (error "ARR must be an array."))
  (let ((len (length arr)))
    (cond ((= len 0) nil)
          ((= len 1) (cons arr nil))
          (t (let ((middle-index (if (evenp len)
                                     (/ len 2)
                                   (1+ (/ len 2)))))
               (cons (seq-subseq arr 0 middle-index)
                     (seq-subseq arr middle-index)))))))

(defun find-binary (array value)
  (error
   "Delete this S-Expression and write your own implementation"))


(provide 'binary-search)
;;; binary-search.el ends here
