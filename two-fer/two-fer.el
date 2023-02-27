;;; two-fer.el --- Two-fer Exercise (exercism)  -*- lexical-binding: t; -*-

;;; Commentary:

(defun two-fer (&optional name)
  "return string 'One for <name>, one for me.' where <name> is 'you' if not
   passed."
  (let ((two-fer-string (lambda (n)
                         (format "One for %s, one for me." n))))
    (if name
        (two-fer-string name)
      (two-fer-string "you"))))

(provide 'two-fer)
;;; two-fer.el ends here
