;;; sublist-additional-testing.el --- tests for helper functions for sublist (Exercism)  -*- lexical-binding: t; -*-

;;; Commentary: This testing is probably more exhaustive than what I'd do in
;;; real production code, especially because these are just helper functions,
;;; and if list-classify passes its (hopefully exhaustive) test suite, it
;;; doesn't matter how it's implemented, so the quality of its helper functions
;;; are less critial. The purpose of this is to build some "muscle memory" for
;;; working with ERT. One thing I've gotten from it so far: factoring out
;;; reused arguments into lets seems like a good pattern.

;;; Code:

(load-file "sublist.el")

;; --- slice testing ---
(declare-function slice "sublist.el" (xs start size))

(ert-deftest slice-returns-original-list ()
  "Slice should return original list for start = 0 and size = (length xs)"
  (let ((test-list '(1 2 3)))
    (should (equal test-list (slice test-list 0 (length test-list))))))

(ert-deftest slice-error-on-long-list ()
  (let ((test-list2 '("foo" "bar")))
    (should-error (slice test-list2 0 (1+ (length test-list))))))

(ert-deftest slice-error-falling-off-end-of-list ()
  (let ((test-list3 '(:foo :bar :baz)))
    (should-error (slice test-list3 2 2))))

(ert-deftest slice-error-negative-start ()
  (let ((test-list4 '(:foo :bar)))
    (should-error (slice test-list4 -1 1))))

(ert-deftest slice-error-negative-size ()
  (let ((test-list5 '(1 2 3)))
    (should-error (slice test-list5 1 -1))))

(ert-deftest slice-returns-nil-for-size-0 ()
  (let ((test-list6 '("foo" "bar" "baz")))
    (should (equal '() (slice test-list6 1 0)))))

;; --- longer-list testing ---
(declare-function longer-list "sublist.el" (xs ys))

(ert-deftest ll-empty-lists-are-equal ()
  (should (equal :equal (longer-list '() '()))))

(ert-deftest non-empty-list-first-is-longer-than-empty-list ()
  (should (equal :first (longer-list '(:foo) '()))))

(ert-deftest non-empty-list-second-is-longer-than-empty-list ()
  (should (equal :second (longer-list '() '(:foo)))))

(let ((longer '(:foo :bar :baz))
      (shorter '(:foo :bar)))
  (ert-deftest non-empty-lists-first-longer ()
    (should (equal :first (longer-list longer shorter))))

  (ert-deftest non-empty-lists-first-longer ()
    (should (equal :second (longer-list shorter longer)))))

(ert-deftest lists-of-same-length-equal ()
  (should (equal :equal (longer-list '(:foo :bar :baz) '(1 2 3)))))

;; --- compare-equal-length-lists testing ---
(declare-function compare-equal-length-lists "sublist.el" (xs ys))

(ert-deftest compeqlenl-empty-lists-are-equal ()
  (should (equal :equal (compare-equal-length-lists '() '()))))

(ert-deftest transposition-of-elements-unequal-lists ()
  (should (equal :unequal (compare-equal-length-lists '(:foo :bar :baz)
                                                      '(:bar :foo :baz)))))

(let ((longer '(:foo :bar :baz))
      (shorter '(:foo :bar)))
  (ert-deftest unequal-lengths-should-error-smaller-list-first ()
    (should-error (compare-equal-length-lists shorter longer)))

  (ert-deftest unequal-lengths-should-error-larger-list-first ()
    (should-error (compare-equal-length-lists longer shorter))))

(ert-deftest non-empty-equal-lists-are-equal ()
  (let ((lst '(:foo :bar :baz)))
    (should (equal :equal (compare-equal-length-lists lst lst)))))

;; --- successive-sublists testing ---
(declare-function successive-sublists "sublist.el" (xs size))

(ert-deftest size-greater-than-list-length-error ()
  (should-error (successive-sublists '(:foo :bar) 3)))

(ert-deftest non-empty-list-size-zero-error ()
  (should-error (successive-sublists '(:foo :bar) 0)))

(ert-deftest empty-list-size-zero-no-error ()
  (should (equal '() (successive-sublists '() 0))))

(ert-deftest successive-sublists-expected-behavior ()
  (should (equal '('(:foo :bar)
                   '(:bar :baz)
                   '(:baz :qux))
                 (successive-sublists '(:foo :bar :baz :qux) 2))))

(provide 'sublist-additional-testing)
;;; sublist-additional-testing.el ends here
