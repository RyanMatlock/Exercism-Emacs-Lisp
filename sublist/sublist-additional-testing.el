;;; sublist-additional-testing.el --- tests for helper functions for sublist (Exercism)  -*- lexical-binding: t; -*-

;;; Commentary:

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

(provide 'sublist-additional-testing)
;;; sublist-additional-testing.el ends here
