;;; acronym.el --- Acronym (exercism)  -*- lexical-binding: t; -*-

;;; Commentary:

(defun acronym (phrase)
  "Convert string PHRASE to an acronym (i.e. concatenating the uppercase first
letter of each word). Hyphens and whitespace are word separators, and all other
punctuation is considered part of the word to which it's atttached."
  (let* ((words (split-string phrase "[-[:space:]]+"))
         (initial-chars (mapcar #'(lambda (word) (elt word 0)) words)))
    (upcase (mapconcat #'string initial-chars ""))))

(provide 'acronym)
;;; acronym.el ends here
