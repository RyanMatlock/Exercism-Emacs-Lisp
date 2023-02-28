;; not wrong per se, but overly complicated -- no need to use alists

;; how to form a cons cell
;; (message (format "%s" (cons "a" 2)))  ;; (a . 2)

;; (message (format "car of nil: '%s'" (car '())))  ;; nil

;; (message (format "car of 'apple': '%s'" (car "apple")))
;; format: Wrong type argument: listp, "apple"
;; you need to start using 「M-x ielm」 to test out Emacs Lisp

;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Sequences-Arrays-Vectors.html
;; strings are a subset of arrays, and lists and arrays are a subset of
;; sequences

;; (mapcar #'identity "apple")
;; (97 112 112 108 101)
;; (mapcar #'string '(97 112 112 108 101))
;; ("a" "p" "p" "l" "e")

;; (sort '(("a" . 1) ("p" . 2) ("l" . 1) ("e" . 1)) '(lambda (x y)
;;                                                            (< (car x) (car y))))
;; *** Eval error ***  Wrong type argument: number-or-marker-p, "p"

;; (print (sort '(("a" . 1) ("p" . 2) ("l" . 1) ("e" . 1))
;;                     '(lambda (x y)
;;                        (string< (car x) (car y)))))

;; (("a" . 1) ("e" . 1) ("l" . 1) ("p" . 2))

;; (print (sort-alist '(("a" . 1) ("p" . 2) ("l" . 1) ("e" . 1))
;;                           'string<))

;; (("a" . 1) ("e" . 1) ("l" . 1) ("p" . 2))

(defun anagrams-for (subject candidates)
  "Determine which, if any, of the words in candidates are anagrams of
subject."
  ;; seems like something like this should be built in, but I couldn't find it
  (defun string-to-letters (str)
    "Example: convert 'apple' to ('a' 'p' 'p' 'l' 'e')."
    (mapcar #'string (mapcar #'identity str)))
  (defun sort-alist (alist comparison)
    "Sort association list using function comparison (e.g. for alists with keys
of type string, you'd pass string< for an ascending list)."
    (sort alist #'(lambda (x y)
                    (funcall comparison (car x) (car y)))))
  (defun count-letters (word)
    "Counts instances of each letter in a given word and returns an association
list, e.g. apple:
'((a . 1)
  (e . 1)
  (l . 2
  (p . 2))
[note: sorting the list alphabetically should make comparision easier]"
    (let* ((lcase-word (downcase word))
           (letters ))
      ))
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
