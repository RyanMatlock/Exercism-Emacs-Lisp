;;; sublist.el --- Sublist (exercism)  -*- lexical-binding: t; -*-

;;; Commentary: I've pulled out helper functions from the scope of
;;; list-classify in order to run my own ERT tests on them. Testing is a good
;;; habit to get into, but in general I wouldn't want to pollute my namespace
;;; like this.

;;; Code:

(defun slice (xs start size)
  "Create a sublist of XS beginning at index START of length SIZE. Throw an
error if trying to take a sublist that's too long.

(slice XS 0 (length XS)) should return XS."
  (cond ((< (- (length xs) start) size)
         (error "Slice is too large."))
        ((< start 0)
         (error "0 <= start < (length XS)."))
        ((< size 0)
         (error "0 <= size < (- (length XS) start)."))
        (t
         ;; use seq-subseq instead; see
         ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Sequence-Functions.html
         ;; (defun slice-helper (xs start)
         ;;   (if (> start 0)
         ;;       (slice-helper (cdr xs) (1- start))
         ;;     xs))
         ;; (seq-take (slice-helper xs start) size)
         (seq-subseq xs start (+ start size)))))

(defun longer-list (xs ys)
  "Determine the longer list.
(length XS) > (length YS) => :first
(length XS) < (length YS) => :second
(length XS) == (length YS) => :equal"
  (let ((lx (length xs))
        (ly (length ys)))
    (cond ((> lx ly) :first)
          ((< lx ly) :second)
          (t :equal))))

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

(defun successive-sublists (xs size)
  "Break XS into as many sublists of length SIZE as possible and return as a
list of lists; e.g.

(successive-sublists '(:foo :bar :baz :qux) 2) ->
('(:foo :bar) '(:bar :baz) '(:baz :qux))"
  )

(defun list-classify (list1 list2)
  "Determine if LIST1 is equal to, a sublist of, a superlist of, or unequal to
LIST2."

  (cond ((and (not list1) list2) :sublist)
        ((and list1 (not list2)) :superlist)
        ((eq (length list1) (length list2))
         (compare-equal-length-lists list1 list2))
        (t (error "Not implemented."))))

(provide 'sublist)
;;; sublist.el ends here
