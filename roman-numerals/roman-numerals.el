;;; roman-numerals.el --- roman-numerals Exercise (exercism)  -*- lexical-binding: t; -*-

;;; Commentary:

(setq roman-numeral-alist '((0 . "")
                            (1 . "I")
                            (5 . "V")
                            (10 . "X")
                            (50 . "L")
                            (100 . "C")
                            (500 . "D")
                            (1000 . "M")))
(setq roman-base-values (mapcar #'car (cdr roman-numeral-alist)))

(defun to-roman (value)
  ;; (let* ((roman-numeral-alist '((0 . "")
  ;;                               (1 . "I")
  ;;                               (5 . "V")
  ;;                               (10 . "X")
  ;;                               (50 . "L")
  ;;                               (100 . "C")
  ;;                               (500 . "D")
  ;;                               (1000 . "M")))
  ;;        ;; stolen from https://emacs.stackexchange.com/a/33626 when looking
  ;;        ;; for a standard function for this operation
  ;;        ;; I can skip the first element
  ;;        (roman-base-values (mapcar #'car (cdr roman-numeral-alist))))
    (defun simple-roman-lookup (simple-number)
      "Look up non-compound Roman numerals."
      (alist-get simple-number roman-numeral-alist))
    ;; (defun even-p (n)
    ;;   "Determine if n is an even number."
    ;;   (eq 0 (mod n 2)))
    ;; (defun odd-p (n)
    ;;   "Determine if n is an odd number."
    ;;   (not (even-p n)))
    ;; (defun 5s-oom-p (oom)
    ;;   "Determine if ")
    (defun position-in-list (element list &optional test)
      "Return position in list of first occurrence of element starting at 0 (as
god intended; seriously, Lua, MATLAB, etc., what is wrong with you?); if
element is not in list, return nil. test is the function used to compare
element and the elements of list (default: eq).

  (nth (position-in-list element list) list)

should return element if element is in list."
      ;; see https://emacs.stackexchange.com/a/14203
      (let ((test (or test #'eq)))
        (defun pos-in-list-helper (e lst pos)
          (let ((first (car lst)))
            (if first
                ;; I always forget funcall
                (if (funcall test e first)
                    pos
                  (pos-in-list-helper e (cdr lst) (1+ pos)))
              nil)))
        (pos-in-list-helper element list 0)))
    (defun prev-roman-oom (roman-oom)
      "Return the next-lowest roman-oom (returns nil if roman-oom is 1)."
      (let ((pos (position-in-list roman-oom roman-base-values)))
        (if (and pos (> pos 0))
            (nth (1- pos) roman-base-values)
          nil)))
    (defun next-roman-oom (roman-oom)
            (let ((pos (position-in-list roman-oom roman-base-values)))
        (if (and pos (< pos (length roman-base-values)))
            (nth (1+ pos) roman-base-values)
          nil)))
    (defun 10s-oom-p (roman-oom)
      "Determine if roman-oom (order of magnitude) is in the 10s series (for
lack of a better term), i.e. 1, 10, 100, 1000, rather than the 5s series, i.e.
5, 50, 500.

This may be helpful in handling the case of 9s."
      (or (eq roman-oom 1)
          (eq (/ roman-oom (prev-roman-oom roman-oom)) 2)))
    (defun 5s-oom-p (roman-oom)
      "Determine if roman-oom (order of magnitude) is in the 5s series (for
lack of a better term), i.e. 5, 50, 500, rather than the 10s series, i.e. 1,
10, 100, 1000."
      (not (10s-oom-p roman-oom)))
    (defun repeat-string (str n &optional join)
      "Repeat str n times separated by join (default: '')."
      (let ((join (or join "")))
        (defun repeat-string-helper (str n str-list)
          (if (> n 0)
              (repeat-string-helper str (1- n) (append str str-list))
            (mapconcat #'string str-list join))))
      (repeat-string-helper str n '()))
    (defun roman-oom-and-multple-alist (n)
      "Calculate the Roman numeral 'order of magnitude,' which goes in 5s
instead of 10s, as well as the multple of that OOM and return it as an alist in
the form (OOM . multiple)"
      (let
          ;; I need the base values in descending order because I need to pick
          ;; off the biggest chunks first
          ((base-values (reverse roman-base-values)))
        (defun get-largest-oom-mult (n base-values)
          (let* ((oom (car base-values))
                 (quotient (/ n oom)))
            (cond
             ((> quotient 0) (cons oom quotient))
             (t (get-largest-oom-mult n (cdr base-values))))))
        (defun roman-oom-mult-helper (n oom-mult-alist)
          (if n
              (let* ((oom-mult-entry (get-largest-oom-mult n base-values))
                     (oom (car oom-mult-entry))
                     (mult (cdr oom-mult-entry))
                     (n-reduced (- n (* oom mult))))
                (roman-oom-mult-helper
                 n-reduced
                 (cons oom-mult-entry oom-mult-alist)))
            (reverse oom-mult-alist)))
        (roman-oom-mult-helper n '())))
  (defun roman-numeral-alist-formatter (oom-mult-alist)
    "Converts a number like alist ((roman-oom . multiple) ...) into a Roman
numeral. e.g.
((10 . 4)
 (5 . 3)) -> 'XLVIII'."
    (defun oom-mult-to-numeral-string (oom-mult-alist-elem)
      (let ((oom (car oom-mult-alist-elem))
            (mult (cdr oom-mult-alist-elem)))
        (cond ((eq 4 mult) (concat (alist-get oom roman-numeral-alist)
                                   (alist-get (next-roman-oom oom)
                                              roman-numeral-alist)))
              ;; the difference between 4 and 9 is that with 9 I'm going up by
              ;; 2 Roman orders of magnitude
              ((eq 9 mult) (concat (alist-get oom roman-numeral-alist)
                                   (alist-get (next-roman-oom
                                               (next-roman-oom oom))
                                              roman-numeral-alist)))
              ;; works for mult = 0, 1, 2, 3 (although 0 shouldn't come up)
              ;; (this is the clever step -- assuming there is such a thing in
              ;; this monstrosity)
              (t (repeat-string (alist-get oom roman-numeral-alist) mult)))))
    (defun rn-formatter-helper (oom-mult-alist accumulator)
      (let ((oom-mult-elem (car oom-mult-alist)))
        (if oom-mult-elem
            (rn-formatter-helper
             (cdr oom-mult-alist)
             (cons (oom-mult-to-numeral-string oom-mult-elem) accumulator))
          ;; (mapconcat #'string accumulator "")
          ;; (reverse accumulator)
          (mapconcat #'identity (reverse accumulator) ""))))
    (rn-formatter-helper oom-mult-alist '()))
  (let ((simple (simple-roman-lookup value)))
    ;; start using this pattern instead of
    ;; (if foo
    ;;     foo
    ;;   else-action)
    (or simple
        (roman-numeral-alist-formatter (roman-oom-and-multiple-alist value)))))

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

(provide 'roman-numerals)
;;; roman-numerals.el ends here
