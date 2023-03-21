;;; sublist.el --- Sublist (exercism)  -*- lexical-binding: t; -*-

;;; Commentary: I've pulled out helper functions from the scope of
;;; list-classify in order to run my own ERT tests on them. Testing is a good
;;; habit to get into, but in general I wouldn't want to pollute my namespace
;;; like this.

;;; Code:

(defun slice (xs start size)
  "Create a sublist of xs beginning at index start of length size. Throw an
error if trying to take a sublist that's too long.

(slice xs 0 (length xs)) should return xs."
  (cond ((< (- (length xs) start) size)
         (error "Slice is too large."))
        ((< start 0)
         (error "0 <= start < (length xs)."))
        ((< size 0)
         (error "0 <= size < (- (length xs) start)."))
        (t
         ;; use seq-subseq instead; see
         ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Sequence-Functions.html
         (defun slice-helper (xs start)
           (if (> start 0)
               (slice-helper (cdr xs) (1- start))
             xs))
         (seq-take (slice-helper xs start) size))))

(defun longer-list (xs ys)
  "Determine the longer list.
(length xs) > (length ys) => :first
(length xs) < (length ys) => :second
(length xs) == (length ys) => :equal"
  (let ((lx (length xs))
        (ly (length ys)))
    (cond ((> lx ly) :first)
          ((< lx ly) :second)
          (t :equal))))

(defun list-classify (list1 list2)
  "Determine if list1 is equal to, a sublist of, a superlist of, or unequal to
list2."

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

;; ELISP> (longer-list '(1 2 3) '(1 2 3 4))
;; :second
;; ELISP> (longer-list '(1 2 3) '())
;; :first
;; ELISP> (longer-list '(1 2 3) '(3 2 1))
;; :equal

;; ELISP> (equal :first :first)
;; t
;; ELISP> (eq :first :first)
;; t
;; ELISP> (eq :first :second)
;; nil
;; ELISP> (equal :first :second)
;; nil
;; ok, that works as expected

;; ELISP> (mapcar #'(lambda (x y) (eq x y)) '(1 2 3) '(3 2 1))
;; *** Eval error ***  Wrong number of arguments: mapcar, 3
;; ELISP> (seq-mapn #'(lambda (x y) (eq x y)) '(1 2 3) '(3 2 1))
;; (nil t nil)

;; ELISP> (seq-reduce #'and (seq-mapn #'(lambda (x y) (eq x y)) '(1 2 3) '(3 2 1))
;; t)
;; *** Eval error ***  Invalid function: #<subr and>
;; ELISP> (seq-reduce 'and '(t t nil) t)
;; *** Eval error ***  Invalid function: #<subr and>
;; ELISP> (seq-reduce #'(lambda (x y) (and x y)) '(t t nil) t)
;; nil
;; ELISP> (seq-reduce #'(lambda (x y) (and x y)) '(t t t) t)
;; t
;; ELISP> (functionp 'and)
;; nil
;; ELISP> (functionp 'length)
;; t
;; see
;; https://emacs.stackexchange.com/questions/16332/and-is-an-invalid-function
;; well, that's weird about #'and, but it's good to know, and it's really good
;; to know about seq-mapn for essentially being able to zip lists

;; ELISP> (seq-mapn #'(lambda (x y) (cons x y)) '(:foo :bar :baz) '(1 2 3 4))
;; ((:foo . 1)
;;  (:bar . 2)
;;  (:baz . 3))
;; looks like I'll probably be able to make my code a lot shorter

;; ELISP> (defun any (bools) (seq-reduce #'(lambda (x y) (or x y)) bools nil))
;; any
;; ELISP> (any '(nil nil t))
;; t

(provide 'sublist)
;;; sublist.el ends here
