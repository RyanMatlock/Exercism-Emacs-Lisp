;;; run-length-encoding.el --- run-length-encoding Exercise (exercism)  -*- lexical-binding: t; -*-

;;; Commentary:

(defun run-length-encode (s)
  "For string S consisting only of letters A-Z, a-z, and whitespace, replace
repeated consecutive characters with the number of repetitions followed by the
character."
  (defun encode-helper (letters prev-letter count acc)
    (let* ((min-count 1)
           (letter (car letters))
           (print-debug (lambda (label)
                          (print
                           (format (concat "%s\n\t"
                                           "letter: %s\t"
                                           "prev-letter: %s\t"
                                           "count: %d\t"
                                           "acc: %s")
                                   label letter prev-letter count acc)))))
      (cond ((and letter (string= letter prev-letter))
             (funcall print-debug "string=:")
             (encode-helper (cdr letters) letter (1+ count) acc))
            ((> count min-count)
             (funcall print-debug (format "count > %d:" min-count))
             (encode-helper
              (cdr letters)
              letter
              min-count
              (cons (format "%d%s" count prev-letter) acc)))
            ((and letter (not (string= "" prev-letter)))
             (funcall print-debug "letter not nil and prev-letter not '':")
             (encode-helper
              (cdr letters)
              letter
              min-count
              (cons prev-letter acc)))
            (letter
             (funcall print-debug "letter not nil")
             (encode-helper (cdr letters) letter min-count acc))
            (t
             (funcall print-debug "out of letters")
             ;; you need to stick the last letter on at the end
             (mapconcat #'identity (reverse (cons prev-letter acc)) "")))))
  (let ((slist (mapcar #'string s)))
    (encode-helper slist "" 1 '())))

(defun run-length-decode (s)
;;; Code:
)

(provide 'run-length-encoding)
;;; run-length-encoding.el ends here
