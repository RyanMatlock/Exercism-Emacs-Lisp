;;; binary-search.el --- Binary Search (exercism)  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:


(require 'cl)

(defun bs--middle-index (seq)
  "Find the index of the middle element of sequence SEQ; on sequences of even
length, return the value closer to zero."
  (let ((len (length seq)))
    (cond ((= len 0) nil)
          ((evenp len) (1- (/ len 2))) ;; 1- is the result of 0-indexing
          ((oddp len) (/ len 2))
          ;; too many Arrested Development clips this morning
          (t (error "I've made a terrible mistake.")))))

(defun bs--split-array (arr)
  "Split array ARR at the middle element and return a cons cell of the array
from index 0 to one less than the middle element and the array from one greater
than the middle element to the end of the array."
  (unless (arrayp arr)
    (error "ARR must be an array."))
  (let ((length-arr (length arr)))
    (cond ((= length-arr 0) nil)
          ((= length-arr 1) '(nil . nil)) ;; ≡ '(nil) but this better captures
                                          ;; its purpose/meaning
          (t (let ((middle-index (bs--middle-index arr)))
               ;; exclude middle value from result
               (cons (seq-subseq arr 0 middle-index)
                     (seq-subseq arr (1+ middle-index))))))))

(defun find-binary (array value)
  "Check if VALUE is in ARRAY using a binary search and return its index if it
is; otherwise return nil."

  (defun fb-helper (arr val abs-index)
    (let ((larr (length array))
          (middle-index (bs--middle-index arr)))
      (cond ((= larr 0) nil)
            ((= larr 1) (when (= val (elt arr 0)) abs-index))
            (t
             ;; recall that `when' returns nil when COND is nil
             (let* ((middle-elt (when middle-index (elt arr middle-index)))
                    (split (when arr (bs--split-array arr)))
                    (left (car-safe split))
                    (right (cdr-safe split)))
               (cond ((null middle-elt) nil)
                     ((= val middle-elt) (+ abs-index middle-index))
                     ((< val middle-elt)
                      (fb-helper left val abs-index))
                     (t (fb-helper right val (1+ (+ abs-index
                                                    middle-index))))))))))

  (fb-helper array value 0))


(provide 'binary-search)
;;; binary-search.el ends here
