;;; sublist.el --- Sublist (exercism)  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defun list-classify (list1 list2)
  "Determine if list1 is equal to, a sublist of, a superlist of, or unequal to
list2."

  (defun slice (xs start size)
    "Create a sublist of xs beginning at index start of length size. Throw an
error if trying to take a sublist that's too long.

(slice xs 0 (length xs)) should return xs."
    (if (< (- (length xs) start) size)
        (error "Slice is too large.")
      (defun slice-helper (xs start size result)
        (let ((x (nth start xs)))
          (if (> size 0)
              (slice-helper xs (1+ start) (1- size) (cons x result))
            (reverse result))))
      (slice-helper xs start size '())))

  (defun compare-equal-length-lists (xs ys)
    "Determine if lists of equal length have identical elements in the same
order, in which case they're :equal; otherwise, return :unequal."
    (cond ((eq (length xs) (length ys))
           ;; only reason I'm writing the helper function is so the length
           ;; check only happens once (not that performance really matters
           ;; here)
           (defun compare-equal-length-lists-helper (xs ys)
             (let ((x (car xs))
                   (y (car ys)))
               (cond ((and x y (eq x y))
                      (compare-equal-length-lists-helper
                       (cdr xs)
                       (cdr ys)))
                     ((not (eq x y)) :unequal)
                     (t :equal)))))
          (t (error "Lists are of unequal length.")))
    (compare-equal-length-lists-helper xs ys))

  (cond ((and (not list1) list2) :sublist)
        ((and list1 (not list2)) :superlist)
        ((eq (length list1) (length list2))
         (compare-equal-length-lists list1 list2))
        (t (error "Not implemented."))))

;; -- IELM testing --
;; ELISP> (compare-equal-length-lists '("foo" "bar") '("bar" "foo"))
;; :unequal
;; ELISP> (compare-equal-length-lists '(1 2 3) '(1 2 3))
;; :equal

;; ELISP> (slice '(1 2 3 4 5) 1 3)
;; (2 3 4)

;; ELISP> (slice '(1 2 3 4 5) 2 3)
;; (3 4 5)

;; ELISP> (slice '(1 2 3 4 5) 1 5)
;; (2 3 4 5 nil)
;; oops, going off the end

;; ELISP> (slice '(1 2 3 4 5) 1 6)
;; (2 3 4 5 nil nil)
;; really going off the end!

;; ELISP> (slice '(1 2 3 4 5) 1 7)
;; *** Eval error ***  Slice is too large.

;; ELISP> (slice '(1 2 3 4 5) 1 5)
;; *** Eval error ***  Slice is too large.
;; ELISP> (slice '(1 2 3 4 5) 1 4)
;; (2 3 4 5)

;; ELISP> (slice '(1 2 3 4 5) 0 5)
;; (1 2 3 4 5)
;; fixed

(provide 'sublist)
;;; sublist.el ends here
