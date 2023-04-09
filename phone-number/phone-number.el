;;; phone-number.el --- phone-number Exercise (exercism)  -*- lexical-binding: t; -*-

;;; Commentary:

(defun numbers (num &optional strict)
  "Transform string NUM representing a NANP phone number. When STRICT is
non-NIL, area code and exchange code must be in range 2-9."
  (let* ((nanp-length 10)
         (begin "^")
         (end "$")
         (separator "[ .-]?")
         (country-code "\\(\\+?1\\)?")
         (area-code-strict "[(]?\\([2-9][0-9]\\{2\\}\\)[)]?")
         (area-code-lax "[(]?\\([0-9]\\{3\\}\\)[)]?")
         (area-code (if strict area-code-strict area-code-lax))
         (exchange-code-strict "\\([2-9][0-9]\\{2\\}\\)")
         (exchange-code-lax "\\([0-9]\\{3\\}\\)")
         (exchange-code (if strict exchange-code-strict exchange-code-lax))
         (subscriber-number "\\([0-9]\\{4\\}\\)")
         (nanp-regexp (concat begin
                              country-code
                              separator
                              area-code
                              separator
                              exchange-code
                              separator
                              subscriber-number
                              end))
         (match (string-match nanp-regexp num)))
    (cond (match (replace-match "\\2\\3\\4" nil nil num))
          (t (make-string nanp-length ?0)))))

(defun area-code (num &optional strict)
    "Capture area code from string NUM representing a NANP phone number. When
STRICT is non-NIL, area code and exchange code must be in range 2-9."
  (let ((clean (numbers num strict)))
    (string-match "^\\([[:digit:]]\\{3\\}\\).+" clean)
    (replace-match "\\1" nil nil clean)))

(defun pprint (num &optional strict)
    "Pretty print string NUM representing a NANP phone number as
(XXX) XXX-XXXX. When STRICT is non-NIL, area code and exchange code must be in
range 2-9."
  (let ((clean (numbers num strict)))
    (string-match (concat "\\([[:digit:]]\\{3\\}\\)"
                          "\\([[:digit:]]\\{3\\}\\)"
                          "\\([[:digit:]]\\{4\\}\\)")
                  clean)
    (replace-match "\(\\1\) \\2-\\3" nil nil clean)))

(provide 'phone-number)
;;; phone-number.el ends here
