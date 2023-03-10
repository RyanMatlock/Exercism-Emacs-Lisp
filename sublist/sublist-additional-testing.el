;;; sublist-additional-testing.el --- tests for helper functions for sublist (Exercism)  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(load-file "sublist.el")

(declare-function slice "sublist.el" (xs start size))

(ert-deftest slice-returns-original-list ()
  (let ((test-list '(1 2 3)))
    (should (equal test-list (slice test-list 0 (length test-list))))))

(ert-deftest slice-error-on-long-list ()
  (let ((test-list2 '("foo" "bar")))
    (should-error (slice test-list2 0 (1+ (length test-list))))))

(ert-deftest slice-error-falling-off-end-of-list ()
  (let ((test-list3 '(:foo :bar :baz)))
    (should-error (slice test-list3 2 2))))

(ert-deftest slice-error-negative-start ()
  (let (test-list4 '(:foo :bar))
    (should-error (slice test-list4 -1 2))))

(ert-deftest slice-error-negative-size ()
  (let (test-list5 '(:foo :bar :baz))
    (should-error (slice test-list5 1 -1))))

(provide 'sublist-additional-testing)
;;; sublist-additional-testing.el ends here
