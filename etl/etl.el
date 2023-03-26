;;; etl.el --- etl Exercise (exercism)  -*- lexical-binding: t; -*-

;;; Commentary: based on etl-test.el, it looks like I should be using a hash
;;; table instead of an alist, and the data will be presented in the form of a
;;; hash table. Note that the letters in the input are mixed case.

(defun etl (data)
  "Translate hash table DATA, where keys are points and the values are lists of
mixed case letters to a hash table where keys are lowercase letters and values
are points."
  (let ((newdata (make-hash-table :test #'equal)))
    (defun swap-key-value (key values)
      "Apply to data with maphash"
      (let ((value (car values)))
        ;; it's weird writing impure code like this
        (cond ((and value (char-or-string-p value))
               (progn (puthash (downcase value) key newdata)
                      (swap-key-value key (cdr values))))
              (value (error "New keys must be chars/strings."))
              (t nil)))
      (if (< key 0)
          (error "Keys should be non-negative.")
        (swap-key-value key values)))
    (maphash #'swap-key-value data)
    newdata))

(provide 'etl)
;;; etl.el ends here
