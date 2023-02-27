;;; anagram.el --- Anagram (exercism)  -*- lexical-binding: t; -*-

;;; Commentary:

;; how to form a cons cell
;; (message (format "%s" (cons "a" 2)))  ;; (a . 2)

(defun anagrams-for (subject candidates)
  "Determine which, if any, of the words in candidates are anagrams of
subject."
  (defun count-letters (word)
    "Counts instances of each letter in a given word, e.g. apple:
'((a . 1)
  (e . 1)
  (l . 2
  (p . 2))
[note: sorting the list alphabetically should make comparision easier]"
    )
  (defun anagrams-for-helper (subject candidates anagrams)
    (cond ((not candidates) anagrams)
          ((string= (downcase subject) (cons (downcase candidates)))
           (anagrams-for-helper subject (car candidates) anagrams))))
  (anagrams-for-helper subjects candidates '()))

(provide 'anagram)
;;; anagram.el ends here
