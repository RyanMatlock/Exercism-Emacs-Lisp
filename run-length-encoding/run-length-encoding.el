;;; run-length-encoding.el --- run-length-encoding Exercise (exercism)  -*- lexical-binding: t; -*-

;;; Commentary:

(defun run-length-encode (s)
  "For string S consisting only of letters A-Z, a-z, and whitespace, replace
repeated consecutive characters with the number of repetitions followed by the
character."
  (defun encode-helper (letters prev-letter count acc)
    (let* ((min-count 1)
           (letter (car letters)))
      (cond ((and letter (string= letter prev-letter))
             (encode-helper (cdr letters) letter (1+ count) acc))
            ((> count min-count)
             (encode-helper
              (cdr letters)
              letter
              min-count
              (cons (format "%d%s" count prev-letter) acc)))
            ((and letter (not (string= "" prev-letter)))
             (encode-helper
              (cdr letters)
              letter
              min-count
              (cons prev-letter acc)))
            (letter
             (encode-helper (cdr letters) letter min-count acc))
            (t
             ;; stick the last letter on at the end, hence the cons
             (mapconcat #'identity (reverse (cons prev-letter acc)) "")))))
  (let ((slist (mapcar #'string s)))
    (encode-helper slist "" 1 '())))

(defun str-digit-p (str)
  "Return T if STR is a digit (i.e. a character in the range 0-9); otherwise,
return NIL."
  (defun string=-digit (digit)
    (string= str digit))
  (let ((str-digits '("0" "1" "2" "3" "4" "5" "6" "7" "8" "9")))
    (seq-some #'string=-digit str-digits)))

(defun repeat-str (n str &optional separator)
  "Repeat string STR N times separated by SEPARATOR, which defaults to the empty
string."
  (let ((min-length 0)
        (sep (or separator "")))
    (if (< n min-length)
        (error (format "N must be greater than or equal to %d." min-length))
      (mapconcat #'identity (make-list n str) sep))))

(defun run-length-decode (s)
;;; Code:
)

(provide 'run-length-encoding)
;;; run-length-encoding.el ends here
