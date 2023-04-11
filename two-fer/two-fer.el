;;; two-fer.el --- Two-fer Exercise (exercism)  -*- lexical-binding: t; -*-

;;; Commentary:

(defun two-fer (&optional name)
  "Return string \"One for <NAME>, one for me.\" where default NAME is
\"you\"."
  (let ((name (or name "you")))
    (format "One for %s, one for me." name)))

(provide 'two-fer)
;;; two-fer.el ends here
