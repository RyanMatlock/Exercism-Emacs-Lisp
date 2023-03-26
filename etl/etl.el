;;; etl.el --- etl Exercise (exercism)  -*- lexical-binding: t; -*-

;;; Commentary: based on etl-test.el, it looks like I should be using a hash
;;; table instead of an alist, and the data will be presented in the form of a
;;; hash table. Note that the letters in the input are mixed case.

(defun etl (data)
  "Translate hash table DATA, where keys are points and the values are lists of
mixed case letters to a hash table where keys are lowercase letters and values
are points."
  (let ((newdata (make-hash-table :test #'equal)))
    (defun swap-key-value (key raw-values)
      "Swap KEY with each VALUE in VALUES and add to NEWDATA using
PUTHASH. Ensure each VALUE is a lowercase string or char."
      (cond ((< key 0) (error "KEY must be non-negative."))
            ((seq-every-p #'stringp raw-values)
             (let ((values (mapcar #'downcase raw-values)))
               ;; (print (format "key: %d\tvalues: %s" key values))
               (mapcar #'(lambda (val) (puthash val key newdata)) values)))
            (t (error "Each VALUE must be a string."))))
    (maphash #'swap-key-value data)
    newdata))

(provide 'etl)
;;; etl.el ends here
