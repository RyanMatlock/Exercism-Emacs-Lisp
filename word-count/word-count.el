;;; word-count.el --- word-count Exercise (exercism)  -*- lexical-binding: t; -*-

;;; Commentary:

(defun word-count (sentence)
  "Count the number of words in a sentence. Words can be numbers, letters, or
contractions (letters with an apostrophe), and the count is case insensitive."
  (defun alist-increment-value (key alist)
  "Increment value of key in alist; if key is not in alist, add to alist with
initial value of 1."
  (let ((entry (assoc key alist)))
    (if entry
        (progn (setcdr entry (1+ (cdr entry)))
               ;; need to return the alist because setcdr returns the cons cell
               ;; while updating the alist
               alist)
      ;; note that adding a key doesn't update the original alist the way
      ;; setcdr does; that shouldn't matter given that in either branch of the
      ;; if statement, you're returning the updated alist
      (cons (cons key 1) alist))))
  ;; looking at word-coun-test.el, it appears that the alist is good enough,
  ;; and I didn't have to format it into a string
  (defun wc-alist-formatter (wc-alist acc)
    (let ((entry (car wc-alist)))
      (if entry
          (let ((formatted-entry (format "%s: %d" (car entry) (cdr entry))))
            (wc-alist-formatter (cdr wc-alist) (cons formatted-entry acc)))
        ;; need to get the order right => reverse the accumulator
        (mapconcat #'identity (reverse acc) "\n"))))
  (defun wc-helper (words wc-alist)
    (let ((word (car words)))
      ;; need to check that word isn't the empty string in case the sentence
      ;; passed to word-count is empty
      ;; (if (and word (not (string= word "")))
      ;;     (wc-helper (cdr words) (alist-increment-value word wc-alist))
      ;;   ;; to get the original sentence order
      ;;   (reverse wc-alist))
      (cond
       ;; skip empty strings
       ((and word (string= word "")) (wc-helper (cdr words) wc-alist))
       ;; if word isn't the empty string, add it to the alist
       (word (wc-helper (cdr words) (alist-increment-value word wc-alist)))
       ;; if you've read all the words, return a reversed list to get the order
       ;; of the words equivalent to how they appear in the sentence
       (t (reverse wc-alist)))))
  (defun strip-punctuation-from-word (word)
    "Strip punctuation off beginning and end of words."
    ;; so glad I found out about the [:punct:] character class
    (replace-regexp-in-string
     "[[:punct:]]+$" ""
     (replace-regexp-in-string
      "^[[:punct:]]+" "" word)))
  (let* ((words (mapcar #'downcase (split-string sentence "[[:blank:]\n]+")))
         (processed-words (mapcar #'strip-punctuation-from-word words)))    
    ;; (wc-alist-formatter (wc-helper processed-words '()) '())
    (wc-helper processed-words '())))

(provide 'word-count)
;;; word-count.el ends here
