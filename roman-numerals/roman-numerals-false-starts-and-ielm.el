;;;; Want to save this for possible future reference so I don't have to dig
;;;; through git commits

;; only used these SETQs for IELM testing; LET is better to not pollute the
;; namespace
(setq roman-numeral-alist '((0 . "")
                            (1 . "I")
                            (5 . "V")
                            (10 . "X")
                            (50 . "L")
                            (100 . "C")
                            (500 . "D")
                            (1000 . "M")))
(setq roman-base-values (mapcar #'car (cdr roman-numeral-alist)))

(defun even-p (n)
  "Determine if n is an even number."
  (eq 0 (mod n 2)))
(defun odd-p (n)
  "Determine if n is an odd number."
  (not (even-p n)))

;; -- IELM testing --
;; ELISP> (simple-roman-lookup 500)
;; "D"
;; ELISP> (roman-oom-and-multple 49)
;; (10 . 4)
;; ELISP> (roman-oom-and-multple 9)
;; (5 . 1)
;; ELISP> (roman-oom-and-multple 4)
;; (1 . 4)
;; this might not be the right strategy given the way 9 gets broken up
;; on second thought, what if I had a special case for 9?

;; ELISP> (position-in-list 3 '(1 2 3 4))
;; 2 (#o2, #x2, ?\C-b)
;; ELISP> (position-in-list "bar" '("foo" "bar" "baz") #'string=)
;; 1 (#o1, #x1, ?\C-a)
;; ELISP> (position-in-list "quux" '("foo" "bar" "baz") #'string=)
;; nil
;; ELISP> (let ((lst '("foo" "bar" "baz")))
;; (nth (position-in-list "bar" lst #'string=) lst))
;; "bar"

;; nice

;; ELISP> (position-in-list 10 roman-base-values)
;; 2 (#o2, #x2, ?\C-b)
;; ELISP> (nth (1- (position-in-list 10 roman-base-values)) roman-base-values)
;; 5 (#o5, #x5, ?\C-e)
;; ELISP> (prev-roman-oom 5)
;; 1 (#o1, #x1, ?\C-a)
;; ELISP> (prev-roman-oom 1000)
;; 500 (#o764, #x1f4)

;; ELISP> (10s-oom-p 1000)
;; t
;; ELISP> (10s-oom-p 500)
;; nil
;; ELISP> (10s-oom-p 50)
;; nil
;; ELISP> (10s-oom-p 1)
;; t

;; good

;; ELISP> (repeat-string "M" 3)
;; "MMM"
;; that will be useful for applying the multiple to the roman-oom

;; ELISP> (next-roman-oom 50)
;; 100 (#o144, #x64, ?d)

;; ELISP> (repeat-string "foo" 0)
;; ""
;; ELISP> (repeat-string "" 0)
;; ""

;; ELISP> (mult-to-operation '(10 . 2))
;; ""
;; ELISP> (alist-get 10 (mapcar #'car roman-numeral-alist))
;; nil
;; ELISP> (eq 10 10)
;; t
;; weird

;; ELISP> (alist-get 10 '((1 . "foo") (10 . "bar")))
;; "bar"

;; ELISP> (mult-to-operation '(10 . 2))
;; "XX"
;; got it!

;; ELISP> (mult-to-operation '(10 . 4))
;; "XL"

;; ELISP> (mult-to-operation '(10 . 9))
;; "LC"
;; that's wrong!

;; ELISP> (mult-to-operation '(10 . 9))
;; "XC"
;; nice!

;; ELISP> (rn-formatter '((10 . 9) (1 . 9)))
;; *** Eval error ***  Wrong type argument: characterp, "IX"
;; ELISP> (rn-formatter '((10 . 9)))
;; *** Eval error ***  Wrong type argument: characterp, "XC"

;; ELISP> (rn-formatter '((10 . 9) (1 . 9)))
;; ("IX" "XC")


;; ELISP> (rn-formatter '((10 . 9) (1 . 9)))
;; ("XC" "IX")

;; now we're getting somewhere

;; ELISP> (mapconcat #'identity (rn-formatter '((10 . 9) (1 . 9))) "")
;; "XCIX"

;; ELISP> (rn-formatter '((10 . 9) (1 . 9)))
;; "XCIX"
;; ELISP> (rn-formatter '((50 . 1) (10 . 1) (1 . 9)))
;; "LXIX"
;; nice!

;; ELISP> (roman-oom-and-multple 99)
;; (50 . 1)

;; (defun get-largest-oom-mult (n base-values)
;;   (let* ((oom (car base-values))
;;          (quotient (/ n oom)))
;;     (cond
;;      ;; maybe I don't need a special case for 9 because I just promote
;;      ;; it to the next OOM and subtrac the next OOM for it
;;      ;; wait, no
;;      ;; maybe figure out the processing first, and then write this to
;;      ;; fit with that implementation
;;      ;; ;; special case for 9
;;      ;; ;; needed because in Roman numerals
;;      ;; ;; 9 = 10 - 1 != 5 + (5 - 1), and 4 = 5 - 1
;;      ;; ;; wheras 6 = 5 + 1, 7 = 5 + 2, and 8 = 5 + 3
;;      ;; ((and (not (eq (mod oom 10)))
;;      ;;       (eq quotient 4))
;;      ;;  (cons (oom 9)))
;;      ((> quotient 0) (cons oom quotient))
;;      (t (get-largest-oom-mult n (cdr base-values))))))

;;   ELISP> (get-largest-oom-mult 13 (reverse roman-base-values))
;; (10 . 1)

;; ELISP> (to-roman 10)
;; "X"
;; ELISP> (to-roman 13)
;; *** Eval error ***  Symbol’s function definition is void: roman-oom-and-multiple-alist

;; ELISP> (roman-numeral-alist-formatter '((10 . 1) (1 . 3)))
;; "XIII"

;; ELISP> (to-roman 13)
;; *** Eval error ***  Symbol’s function definition is void: roman-oom-and-multiple-alist
;; ELISP> (roman-numeral-alist-formatter '((50 . 1) (10 . 1) (1 . 9)))
;; "LXIX"
;; ELISP> (roman-numeral-alist-formatter '((10 . 1) (1 . 4)))
;; "XIV"

;; ELISP> (roman-oom-mult-helper 13 '())
;; *** Eval error ***  Wrong type argument: number-or-marker-p, nil
;; ELISP> (if 0 "foo" "bar")
;; "foo"
;; ELISP> (roman-oom-mult-helper 13 '())
;; ((10 . 1)
;;  (1 . 3))

;; ELISP> (to-roman 13)
;; "XIII"
;; ELISP> (to-roman 14)
;; "XIV"
;; ELISP> (to-roman 19)
;; "XVIV"
;; good, good, bad
