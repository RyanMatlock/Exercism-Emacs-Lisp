;;; pangram.el --- Pangram (exercism)  -*- lexical-binding: t; -*-

;;; Commentary:

(defun pangramp (phrase)
  "Return T if phrase contains every letter of the alphabet; otherwise, return
NIL."
  (let* ((alphabet (number-sequence ?a ?z))
         (alpha-alist (mapcar #'(lambda (c) (cons c nil)) alphabet)))
    (seq-mapn #'(lambda (c)
                  (let ((entry (assq c alpha-alist)))
                    (if entry
                        (setcdr entry t)
                      nil)))
              (downcase phrase))
    (seq-every-p #'(lambda (aa-elem) (cdr aa-elem)) alpha-alist)))

(provide 'pangram)
;;; pangram.el ends here
