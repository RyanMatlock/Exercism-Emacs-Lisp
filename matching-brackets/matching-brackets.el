;;; matching-brackets.el --- Matching Brackets (exercism)  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:


(defun is-paired (value)
  "Return T if string VALUE contains paired brackets (i.e. (), [], and {},
which may be nested); otherwise return NIL."
  (let* ((only-brackets
          (replace-regexp-in-string "[^][(){}]+" "" value))
         (regexp-or "\\|")
         (matched-pairs-removed
          (replace-regexp-in-string (concat "\\(\\{\\}\\)" ;; {}
                                            regexp-or
                                            "\\(\\[\\]\\)" ;; []
                                            regexp-or
                                            "\\(()\\)+") ;; ()
                                    "" only-brackets)))
    (cond ((string= only-brackets matched-pairs-removed) nil)
          ((string= "" matched-pairs-removed) t)
          (t (is-paired matched-pairs-removed)))))


(provide 'matching-brackets)
;;; matching-brackets.el ends here
