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
  (let* ((roman-numeral-alist '((0 . "")
                                (1 . "I")
                                (5 . "V")
                                (10 . "X")
                                (50 . "L")
                                (100 . "C")
                                (500 . "D")
                                (1000 . "M")))
         ;; stolen from https://emacs.stackexchange.com/a/33626 when looking
         ;; for a standard function for this operation
         ;; I can skip the first element
         (roman-base-values (mapcar #'car (cdr roman-numeral-alist))))
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
    (defun 10s-oom-p (roman-oom)
      "Determine if roman-oom (order of magnitude) is in the 10s series (for
lack of a better term), i.e. 1, 10, 100, 1000, rather than the 5s series, i.e.
5, 50, 500.

This may be helpful in handling the case of 9s."
      (or (eq roman-oom 1)
          (eq (/ roman-oom (prev-roman-oom roman-oom)) 2)))
    (defun repeat-string (str n &optional join)
      "Repeat str n times separated by join (default: '')."
      (defun repeat-string-helper (str n str-list)
        (if (> n 0)
            (repeat-string-helper str (1- n) (append str str-list))
          (let ((join (or join "")))
            (mapconcat #'string str-list join))))
      (repeat-string-helper str n '()))
    (defun roman-oom-and-multple (n)
      "Calculate the Roman numeral 'order of magnitude,' which goes in 5s
instead of 10s, as well as the multple of that OOM and return it as an alist in
the form (OOM . multiple)"
      ;; I need the base values in descending order because I need to pick off
      ;; the biggest chunks first
      (let ((base-values (reverse roman-base-values)))
        (defun oom-mult-helper (n base-values)
          (let* ((oom (car base-values))
                 (quotient (/ n oom)))
            (cond
             ;; maybe I don't need a special case for 9 because I just promote
             ;; it to the next OOM and subtrac the next OOM for it
             ;; wait, no
             ;; maybe figure out the processing first, and then write this to
             ;; fit with that implementation
             ;; ;; special case for 9
             ;; ;; needed because in Roman numerals
             ;; ;; 9 = 10 - 1 != 5 + (5 - 1), and 4 = 5 - 1
             ;; ;; wheras 6 = 5 + 1, 7 = 5 + 2, and 8 = 5 + 3
             ;; ((and (not (eq (mod oom 10)))
             ;;       (eq quotient 4))
             ;;  (cons (oom 9)))
             ((> quotient 0) (cons oom quotient))
             (t (oom-mult-helper n (cdr base-values))))))
        (oom-mult-helper n base-values))))
  (simple-roman-lookup value))

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

(provide 'roman-numerals)
;;; roman-numerals.el ends here
