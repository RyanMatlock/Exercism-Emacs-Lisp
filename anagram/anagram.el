;;; anagram.el --- Anagram (exercism)  -*- lexical-binding: t; -*-

;;; Commentary:

(defun word-signature (word)
  "Convert string WORD to an alphabetically ascending list of letters,
including repeats.

The motivation for this is that words that are anagrams should possess the same
ordered letter list (signature), e.g. 'stop' and 'pots' should both produce
('o' 'p' 's' 't')."
  (sort (mapcar #'string word) #'string<))

(defun anagrams-p (subj cand)
  "Determine if cand is an anagram of subj."
  (let ((subj (downcase subj))
        (cand (downcase cand)))
    (and (not (string= subj cand))
         (equal (word-signature subj)
                (word-signature cand)))))

(defun anagrams-for (subject candidates)
  "Determine which, if any, of the words in candidates are anagrams of
subject."
  (seq-filter
   #'(lambda (candidate) (anagrams-p subject candidate)) candidates))

(provide 'anagram)
;;; anagram.el ends here
