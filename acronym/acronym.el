;;; acronym.el --- Acronym (exercism)  -*- lexical-binding: t; -*-

;;; Commentary: This is probably an overly verbose way of doing it, but it's
;;; also easier (for me) to understand this way.

(defun acronym (phrase)
  "Convert phrase to acronym. Hyphens count as whitespace, and all other
punctuation is considered part of the word to which it's atttached."
  (defun hyphens-to-whitespace (phrase)
    "Replace hyphens with whitespace."
    (replace-regexp-in-string "-" " " phrase))
  (defun phrase-to-words (phrase)
    "Split phrase on whitespace characters."
    (split-string phrase "[[:space:]]+"))
  (defun word-to-letters (word)
    "Turn a word into a list of letters."
    (mapcar #'string word))
  (defun extract-initials-from-words (words)
    "Extract first letter of each word from list of words"
    (mapcar #'car (mapcar #'word-to-letters words)))
  (upcase (mapconcat
           #'identity
           (extract-initials-from-words
            (phrase-to-words (hyphens-to-whitespace phrase))) "")))

(provide 'acronym)
;;; acronym.el ends here
