  ;; (defun strip-punctuation (sentence)
  ;;   "Strip all punctuation except internal apostrophes from sentence."
  ;;   (replace-regexp-in-string
  ;;    "[[,.?\"!@#$%^&*()-_=+;:]][[^A-Za-z0-9]]"" " sentence))
;; didn't work as I'd hoped it would


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
;; ELISP> (split-string "foo   bar\nbaz" "[[:space:]]+")
;; ("foo" "bar
;; baz")
;; ELISP> (split-string "foo   bar\nbaz" "[[:blank:]]+")
;; ("foo" "bar
;; baz")
;; ELISP> (split-string "foo   bar\nbaz" "[[:blank:]\n]+")
;; ("foo" "bar" "baz")
;; ELISP> (strip-punctuation "\"That's the password: 'PASSWORD 123'!\", cried the Special Agent.\nSo I fled.")
;; "\"That's the password: 'PASSWORD 123'!\", cried the Special Agent.
;; So I fled."

;; 💡 maybe instead of stripping punctuation from a sentence, you can just
;; strip it from the words once they've been split because it will all be at
;; the beginning or the end of the word

;; ELISP> (strip-punctuation-from-word "\"There's")
;; "\"There's"
;; ELISP> (strip-punctuation-from-word "Whoa!")
;; "Whoa!"
;; regexps can be such a struggle

;; ELISP> (replace-regexp-in-string "\([[:alnum:]]+\)[[:punct:]]*" "\1" "foo")
;; "foo"
;; ELISP> (replace-regexp-in-string "\([[:alnum:]]+\)[[:punct:]]*" "\1" "bar!")
;; "bar!"
;; ELISP> (replace-regexp-in-string "foo" "bar" "foobar")
;; "barbar"
;; ELISP> (replace-regexp-in-string "[[:punct:]]" "bar" "foo!")
;; "foobar"
;; ELISP> (replace-regexp-in-string "^[[:punct:]]" "bar" "foo!")
;; "foo!"
;; ELISP> (replace-regexp-in-string "[[:punct:]]+$" "bar" "foo!")
;; "foobar"
;; ELISP> (replace-regexp-in-string "^[[:punct:]*[^[:punct:]]+[[:punct:]]*$" "bar" "foo!")
;; "foo!"
;; oh, duh, I get it; the reference groups don't make sense in this context

;; ELISP> (strip-punctuation-from-word "foo!")
;; "foo"
;; ELISP> (strip-punctuation-from-word "\"There's")
;; "There's"

;; ELISP> (word-count "That's 'a' foo-bar!")
;; ("That's" "a" "foo-bar")

;; ELISP> (setq test-cons '("a" . 2))
;; ("a" . 2)
;; ELISP> (setcdr test-cons 4)
;; 4 (#o4, #x4, ?\C-d)
;; ELISP> test-cons
;; ("a" . 4)
;; I don't *love* this because it's impure, but it doesn't look like there's a
;; better way of updating a key's value

;; ELISP> (setq test-alist '(("foo" . 1) ("bar" . 3)))
;; (("foo" . 1)
;;  ("bar" . 3))
;; ELISP> (alist-increment-value "quux" test-alist)
;; (("foo" . 1)
;;  ("bar" . 3))
;; ELISP> (alist-increment-value "quux" test-alist)
;; (("quux" . 1)
;;  ("foo" . 1)
;;  ("bar" . 3))
;; ELISP> (alist-increment-value "bar" test-alist)
;; (("foo" . 1)
;;  ("bar" . 4))
;; ELISP> test-alist
;; (("foo" . 1)
;;  ("bar" . 4))
;; ok, this is actually an acceptable way of doing it even if it's probably not
;; the most performant as the number of alist operations increases

;; ELISP> (word-count "'Egads! My roast is ruined!' exclaimed Skinner. Is it
;; though?")
;; *** Eval error ***  Wrong number of arguments: (((sentence . "'Egads! My roast is ruined!' exclaimed Skinner. Is it
;; though?") t) (wc-alist acc) (let ((entry (car wc-alist))) (if entry (let ((formatted-entry (format "%s: %d" (car entry) (cdr entry)))) (wc-alist-formatter (cdr wc-alist) (cons formatted-entry acc))) (mapconcat #'identity (reverse acc) "
;; ")))), 1
;; have fun debugging that

;; ELISP> (wc-helper '("foo" "foo" "bar") '())
;; (("foo" . 2)
;;  ("bar" . 1))

;; ELISP> (wc-alist-formatter (wc-helper '("foo" "foo" "bar") '()))
;; *** Eval error ***  Wrong number of arguments: ((t) (wc-alist acc) (let ((entry (car wc-alist))) (if entry (let ((formatted-entry (format "%s: %d" (car entry) (cdr entry)))) (wc-alist-formatter (cdr wc-alist) (cons formatted-entry acc))) (mapconcat #'identity (reverse acc) "
;; ")))), 1

;; ELISP> (word-count "'Egads! My roast is ruined!' exclaimed Skinner. Is it
;; though?")
;; (("egads" . 1)
;;  ("my" . 1)
;;  ("roast" . 1)
;;  ("is" . 2)
;;  ("ruined" . 1)
;;  ("exclaimed" . 1)
;;  ("skinner" . 1)
;;  ("it" . 1)
;;  ("though" . 1))

;; (wc-alist-formatter (wc-helper '("foo" "foo" "bar") '()) '())
;; "foo: 2
;; bar: 1"
;; I forgot to call it with the initial accumulator value of '() 🤦

;; ELISP> (word-count "'Egads! My roast is ruined!' exclaimed Skinner. Is it
;; though?")
;; "egads: 1
;; my: 1
;; roast: 1
;; is: 2
;; ruined: 1
;; exclaimed: 1
;; skinner: 1
;; it: 1
;; though: 1"

;; ELISP> (word-count "\"That's the password: 'PASSWORD 123'!\", cried the Special Agent.\nSo I fled.")
;; "that's: 1
;; the: 2
;; password: 2
;; 123: 1
;; cried: 1
;; special: 1
;; agent: 1
;; so: 1
;; i: 1
;; fled: 1"

;; looks like it's working, and it's even getting the words in the right order

;; ok, failing all the tests, so let's start addressing these
;; ELISP> (word-count "")
;; ": 1"
;; yeah, that's a problem

;; ELISP> (word-count "")
;; ""

;;;;;;;;;;;;;;;;;;;;;;;;; ERT results ;;;;;;;;;;;;;;;;;;;;;;;;;
;; FFFFFFFFF

;; F count-one-of-each-word-test
;;     (wrong-type-argument list-or-vector-p "one: 1\nof: 1\neach: 1")

;; F count-one-word-test
;;     (wrong-type-argument list-or-vector-p "word: 1")

;; F ignore-punctuation-test
;;     (wrong-type-argument list-or-vector-p "car: 1")

;; F include-numbers-test
;;     (wrong-type-argument list-or-vector-p "testing: 2\n1: 1\n2: 1")

;; F multiple-occurrences-of-a-word-test
;;     (wrong-type-argument list-or-vector-p "one: 1\nfish: 4\ntwo: 1\nred: 1\nblue: 1")

;; F no-words-test
;;     (wrong-type-argument list-or-vector-p "")

;; F normalize-case-test
;;     (wrong-type-argument list-or-vector-p "go: 3\nstop: 2")

;; F quotation-for-word-with-apostrophe-test
;;     (wrong-type-argument list-or-vector-p "can: 1\ncan't: 2")

;; F with-apostrophes-test
;;     (wrong-type-argument list-or-vector-p "first: 1\ndon't: 2\nlaugh: 1\nthen: 1\ncry: 1\nyou're: 1\ngetting: 1\nit: 1")

;;;; look at what the tests are telling you: you're returning the wrong type;
;;;; don't go by what the Exercism website's toy example; read the tests first!

;; turns out an alist was fine

;; ELISP> (word-count "car : carpet as java : javascript!!&@$%^&")
;; (("car" . 1))
;; ah, so ":" was getting turned into a word, but then it was turned into the
;; empty string, and wc-helper finished prematurely

;; ELISP> (word-count "car : carpet as java : javascript!!&@$%^&")
;; (("car" . 1)
;;  ("carpet" . 1)
;;  ("as" . 1)
;;  ("java" . 1)
;;  ("javascript" . 1))

;; fixed!
