;;; run-length-encoding.el --- run-length-encoding Exercise (exercism)  -*- lexical-binding: t; -*-

;;; Commentary:

(defun run-length-encode (s)
  "For string S consisting only of letters A-Z, a-z, and whitespace, replace
repeated consecutive characters with the number of repetitions followed by the
character."
  (defun encode-helper (letters prev-letter count acc)
    (let ((letter (car letters)))
      (cond ((string= letter prev-letter)
             (encode-helper (cdr letters) letter (1+ count) acc))
            ((> count 0)
             (encode-helper
              (cdr letters) letter 0 (cons (format "%d%s" letter count) acc)))
            (letter (encode-helper (cdr letters) letter 0 (cons letter acc)))
            (t (mapconcat #'string (reverse acc) "")))))
  (let ((slist (mapcar #'string s)))
    (encode-helper slist "" 0 '())))

(defun run-length-decode (s)
;;; Code:
)

(provide 'run-length-encoding)
;;; run-length-encoding.el ends here
