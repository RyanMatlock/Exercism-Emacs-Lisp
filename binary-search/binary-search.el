;;; binary-search.el --- Binary Search (exercism)  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:


(defun bs--safe-1+ (value)
  "If VALUE is a number, add 1 to VALUE; otherwise, return nil."
  (when (numberp value)
      (1+ value)))

(defun find-binary (array value)
  (error
   "Delete this S-Expression and write your own implementation"))


(provide 'binary-search)
;;; binary-search.el ends here
