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

  (defun anagrams-for-helper (subject candidates anagrams)
    "Build a list of anagrams of subject from candidates."
    ;; only using nested if statements instead of cond due to let statement
    ;; if candidates is nil, you can't take the cdr of it
    (if candidates
        (let ((candidate (car candidates))
              (rest (cdr candidates)))
          (if (anagrams-p subject candidate)
              (anagrams-for-helper subject rest (cons candidate anagrams))
            (anagrams-for-helper subject rest anagrams)))
      ;; return anagrams when out of candidates
      ;; reverse anagrams to present them in order found in candidates
      (reverse anagrams)))
  (anagrams-for-helper subject candidates '()))

(provide 'anagram)
;;; anagram.el ends here
