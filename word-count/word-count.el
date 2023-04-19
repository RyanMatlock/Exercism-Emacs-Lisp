;;; word-count.el --- word-count Exercise (exercism)  -*- lexical-binding: t; -*-

;;; Commentary:

(defun sentence-to-words (sentence)
  (let ((raw-words (split-string sentence "[[:blank:]\n]+"))
        (invalid-char-regexp "[^'[:alnum:]]\\|^'\\|'$"))
    (mapcar #'downcase
            (mapcar #'(lambda (rw)
                        ;; doing this all in one go doesn't handle edge cases
                        (replace-regexp-in-string invalid-char-regexp "" rw))
                    raw-words))))

(defun count-element (elem list)
  "Count number of times ELEM appears in LIST."
  (let ((first-match (member elem list)))
    (cond ((and (listp list) first-match)
           (1+ (count-element elem (cdr first-match))))
          (t 0))))

(defun count-elements (list)
  "Count elements in LIST and return as an alist, e.g.
'((ELEM-1 . P) (ELEM-2 . Q) ... (ELEM-N . R))."
  (defun ce-helper (xs acc)
    (let ((x (car-safe xs))
          (rest (cdr-safe xs))
          (keys (mapcar #'car acc)))
      (cond ((and x (not (member x keys)))
             (ce-helper rest (cons (cons x (count-element x xs)) acc)))
            (x (ce-helper rest acc))
            (t (reverse acc)))))
  (ce-helper list '()))

(defun word-count (sentence)
  "Count the number of words in string SENTENCE and return results an an
alist. Words can be numbers, letters, or contractions (letters with an
apostrophe), and the count is case insensitive."
  (let* ((words (sentence-to-words sentence))
         (elements-alist (count-elements words)))
    ;; formatted string instead of alist
    ;; (mapconcat #'(lambda (ccell)
    ;;                (let ((word (car ccell))
    ;;                      (count (cdr ccell)))
    ;;                  (format "%s: %d" word count)))
    ;;            elements-alist
    ;;            "\n")
    elements-alist))

(provide 'word-count)
;;; word-count.el ends here
