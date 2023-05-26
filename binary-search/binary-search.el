;;; binary-search.el --- Binary Search (exercism)  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:


(require 'cl)

;; (defun bs--safe-1+ (value)
;;   "If VALUE is a number, add 1 to VALUE; otherwise, return nil."
;;   (when (numberp value)
;;     (1+ value)))

(defun bs--middle-index (seq)
  (let ((len (length seq)))
    (cond ((= len 0) nil)
          ((evenp len) (1- (/ len 2))) ;; 1- is the result of 0-indexing
          ((oddp len) (/ len 2))
          ;; too many Arrested Development clips this morning
          (t (error "I've made a terrible mistake.")))))

;; (defun bs--array-bisect (arr)
;;   (unless (arrayp arr)
;;     (error "ARR must be an array."))
;;   (let ((length-arr (length arr)))
;;     (cond ((= length-arr 0) nil)
;;           ((= length-arr 1) (cons arr nil))
;;           (t (let ((middle-index (bs--middle-index arr)))
;;                (cons
;;                 ;; 1+ is due to exclusivity of seq-subseq's END argument
;;                 (seq-subseq arr 0 (1+ middle-index))
;;                 ;; 1+ is there to not include middle element twice
;;                 (seq-subseq arr (1+ middle-index))))))))

(defun bs--split-array (arr)
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

  ;; (defun fb-helper (arr val)
  ;;   (let ((larr (length arr)))
  ;;     (cond
  ;;      ((> larr 1)
  ;;       (let* ((middle-elt (elt arr (bs--middle-index arr)))
  ;;              (bisection (bs--array-bisect arr))
  ;;              (left (car-safe bisection))
  ;;              (right (cdr-safe bisection)))
  ;;         (print (format (concat "arr: %s, val: %d\n"
  ;;                                "middle-elt: %d\n"
  ;;                                "left: %s\n"
  ;;                                "right: %s")
  ;;                        arr val middle-elt left right))
  ;;         (cond ((= val middle-elt) 0)
  ;;               ((< val middle-elt) (bs--safe-1+ (fb-helper left val)))
  ;;               (t (bs--safe-1+ (fb-helper right val))))))
  ;;      ((= larr 1)
  ;;       (when (equal (elt arr 0) val) 0))
  ;;      (t nil))))

  (defun fb-helper (arr val abs-index)
    (let ((larr (length array))
          (middle-index (bs--middle-index arr)))
      (cond ((= larr 0) nil)
            ((= larr 1) (when (= val (elt arr 0)) abs-index))
            (t (let* ((middle-elt (elt arr middle-index))
                      (split (bs--split-array arr))
                      (left (car-safe split))
                      (right (cdr-safe split)))
                 (cond ((= val middle-elt) (+ abs-index middle-index))
                       ((< val middle-elt)
                        (fb-helper left val (1+ (- abs-index middle-index))))
                       (t (fb-helper right val (1+ (+ abs-index
                                                      middle-index))))))))))

  (fb-helper array value 0))


(provide 'binary-search)
;;; binary-search.el ends here
