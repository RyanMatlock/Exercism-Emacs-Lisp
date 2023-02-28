;;; anagram.el --- Anagram (exercism)  -*- lexical-binding: t; -*-

;;; Commentary:

;; (word-to-letters-signature "stop")
;; ("o" "p" "s" "t")
;; (word-to-letters-signature "pots")
;; ("o" "p" "s" "t")
;; (defun w2s (x) (sort (mapcar #'identity (downcase x))
;;                             '<))
;; w2s
;; (w2s "stop")
;; (111 112 115 116)

;; ELISP> (not '())
;; t
;; ELISP> (not '("a" "b"))
;; nil
;; ELISP> (and (not '()) (not '()))
;; t
;; ELISP> (and (not '()) (not '("a" "b")))
;; nil

(defun anagrams-for (subject candidates)
  "Determine which, if any, of the words in candidates are anagrams of
subject."
  (defun word-to-letters-signature (str)
    "Convert a word to an alphabetically ascending list of letters, including
repeats.

Words that are anagrams should possess the same ordered letter list,
e.g. 'stop' and 'pots' should both produce ('o' 'p' 's' 't')."
    ;; technically, the outer mapcar isn't necessary, but the output is chars
    ;; AKA ints, so converting them to strings makes debugging easier
    (sort (mapcar #'string (mapcar #'identity (downcase str)))
          'string<))
  (defun identical-letter-list-p (xs ys)
    "Determine if two ordered lists of lowercase letters are identical."
    ;; there's probably a cleverer way of doing this with and and/or or
    ;; functions and omitting the cond entirely
    (cond
     ;; if xs and ys are empty, then they're the same
     ((and (not xs) (not ys)) t
      ;; redundant
      ;; ;; if xs and ys are different lengths, they're not the same
      ;; ((not (eq (length xs) (length ys))) nil)
      ;; if the first element is different, they're not the same
      ((not (string= (car xs) (car ys))) nil)
      ;; recursive step: check next eleemnt
      (t identical-letter-list-p (cdr xs) (cdr ys)))))
  (defun anagrams-for-helper (subject candidates anagrams)
    "Build a list of anagrams of subject from candidates."
    ;; (let ((dsubject (downcase subject))
    ;;       (candidate (car candidates))))
    (cond
     ;; when out of candidates, return anagrams
     ((not candidates) anagrams)
     ;; if subject and candidate are different lengths, they can't be anagrams
     ((/= (length subject) (length (car candidates)))
      (anagrams-for-helper subject (cdr candidates) anagrams))
     ;; if subject is identical to candidate, they are not anagrams
     ((string= (downcase subject) (cons (downcase candidates)))
      (anagrams-for-helper subject (car candidates) anagrams))))
  (anagrams-for-helper subjects candidates '()))

(provide 'anagram)
;;; anagram.el ends here
