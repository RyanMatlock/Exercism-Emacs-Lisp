;;; word-count.el --- word-count Exercise (exercism)  -*- lexical-binding: t; -*-

;;; Commentary:

(defun word-count (sentence)
  "Count the number of words in a sentence. Words can be numbers, letters, or
contractions (letters with an apostrophe), and the count is case insensitive."
  (defun wc-alist-formatter (wc-alist acc)
    (let ((entry (car wc-alist)))
      (if entry
          (let ((formatted-entry (format "%s: %d" (car entry) (cdr entry))))
            (wc-alist-formatter (cdr wc-alist) (cons formatted-entry acc)))
        ;; need to get the order right => reverse the accumulator
        (mapconcat #'identity (reverse acc) "\n"))))
  (defun wc-helper (s wc-alist)
    (let ((word (car s)))
      (if word)))
)

;; -- IELM testing --
;; ELISP> (wc-alist-formatter '(("foo" . 2) ("bar". 3)))
;; nil
;; ELISP> (wc-alist-formatter '(("foo" . 2) ("bar". 3)) '())
;; ("bar: 3" "foo: 2")
;; (wc-alist-formatter '(("foo" . 2) ("bar". 3)) '())
;; "bar: 3
;; foo: 2"
;; ELISP> (wc-alist-formatter '(("foo" . 2) ("bar". 3)) '())
;; "foo: 2
;; bar: 3"

(provide 'word-count)
;;; word-count.el ends here
