;;; roman-numerals.el --- roman-numerals Exercise (exercism)  -*- lexical-binding: t; -*-

;;; Commentary:

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
    (defun roman-oom-and-multiple-alist (n)
      "Calculate the Roman numeral 'order of magnitude,' which goes in 5s
instead of 10s, as well as the multiple of that OOM and return it as an alist in
the form (OOM . multiple)"
      (let
          ;; I need the base values in descending order because I need to pick
          ;; off the biggest chunks first
          ((base-values (reverse roman-base-values)))
        (defun get-largest-oom-mult (n base-values)
          (let* ((oom (car base-values))
                 (next-smallest-oom (prev-roman-oom oom))
                 (quotient (/ n oom)))
            ;; your issue with 9s is happening somewhere in here
            (cond
             ;; ((and
             ;;   ;; convince yourself that these conditions may mean a 9
             ;;   (eq quotient 1)
             ;;   (5s-oom-p oom))
             ;;  )
             ((and (5s-oom-p oom)
                   (eq 9 (/ n next-smallest-oom)))
              (cons next-smallest-oom 9))
             ((> quotient 0) (cons oom quotient))
             (t (get-largest-oom-mult n (cdr base-values))))))
        (defun roman-oom-mult-helper (n oom-mult-alist)
          ;; 🤦 0 evaluates to t, so you need to check that n != 0
          (if (not (eq n 0))
              ;; 💡 maybe instead of handling 9s in get-largest-oom-mult, I can
              ;; do a sort of look-ahead here and handle it that way
              ;; 💡💡 better still: you could do a look behind, and if it's a
              ;; 9, take the cdr of the alist and cons (oom . 9) to it
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
          (mapconcat #'identity (reverse accumulator) ""))))
    (rn-formatter-helper oom-mult-alist '()))
  ;; (let ((simple (simple-roman-lookup value)))
  ;;   ;; start using this pattern instead of
  ;;   ;; (if foo
  ;;   ;;     foo
  ;;   ;;   else-action)
  ;;   (or simple
  ;;       (roman-numeral-alist-formatter (roman-oom-and-multiple-alist value))))
  (roman-oom-and-multiple-alist value)
  ))

;; -- IELM testing --
;; ELISP> (get-largest-oom-mult 9 base-values)
;; (1 . 9)
;; ELISP> (to-roman 19)
;; ((10 . 1)
;;  (5 . 1)
;;  (1 . 4))
;; that's confusing

;; ELISP> (get-largest-oom-mult 90 base-values)
;; (50 . 1)
;; ELISP> (5s-oom-p 50)
;; t
;; very confusing

;; ELISP> (to-roman 9)
;; ((5 . 1)
;;  (1 . 4))
;; still not working 🤔

;; ELISP> (to-roman 9)
;; ((5 . 1)
;;  (1 . 4))

;; ELISP> (to-roman 90)
;; ((50 . 1)
;;  (10 . 4)

;; so 9s always take the form of ((5s-oom . 1) ((prev-oom 5s-oom) . 4)), so if
;; I just look for that pattern, I should be able to turn it into
;; ((prev-oom 5s-oom) . 9), which is what the formatter is expecting

(provide 'roman-numerals)
;;; roman-numerals.el ends here
