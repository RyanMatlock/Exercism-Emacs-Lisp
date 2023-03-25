;;; etl.el --- etl Exercise (exercism)  -*- lexical-binding: t; -*-

;;; Commentary: based on etl-test.el, it looks like I should be using a hash
;;; table instead of an alist, and the data will be presented in the form of a
;;; hash table. Note that the letters in the input are mixed case.

(defun etl (data)
  "Translate hash table data, where keys are points and the values are lists of
mixed case letters to a hash table where keys are lowercase letters and values
are points."
  (let ((newdata (make-hash-table :test #'equal)))
    (defun swap-kv (k vs)
      "Apply to data with maphash"
      (let ((v (car vs)))
        ;; it's weird writing impure code like this
        (cond ((and v (stringp v))
               (progn (puthash (downcase v) k newdata)
                      (swap-kv k (cdr vs))))
              (v (error "New keys must be chars/strings."))
              (t nil)))
      (if (< k 0)
          (error "Keys should be non-negative.")
        (swap-kv-helper k vs)))
    (maphash #'swap-kv data)
    newdata))

(provide 'etl)
;;; etl.el ends here
