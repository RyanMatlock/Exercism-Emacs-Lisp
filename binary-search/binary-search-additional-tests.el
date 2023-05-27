;;; binary-search-additional-tests.el --- Binary Search (exercism)  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:


(load-file "binary-search.el")
(declare-function bs--middle-index "binary-search.el" (seq))
(declare-function bs--split-array "binary-search.el" (arr))
(declare-function find-binary "binary-search.el" (array value))

(ert-deftest middle-index-empty-seq ()
  (should-not (bs--middle-index [])))

(ert-deftest middle-index-singleton-seq ()
  (should (= 0 (bs--middle-index [:foo]))))

(ert-deftest middle-index-even-sequence ()
  (should (= 2 (bs--middle-index "012345"))))

(ert-deftest middle-index-odd-sequence ()
  (should (= 2 (bs--middle-index '(0 1 2 3 4)))))

(ert-deftest split-array-requires-array ()
  "This method of checking an error string is copied from
affine-cipher-test.el"
  (let ((error-data (should-error (bs--split-array '(:foo :bar)))))
    (should (string= "ARR must be an array."
                     (error-message-string error-data)))))

(ert-deftest split-array-string-odd-length ()
  (should (equal '("tac" . "cat") (bs--split-array "tacocat"))))

(ert-deftest split-array-string-even-length ()
  (should (equal '("fo" . "bar") (bs--split-array "foobar"))))

(ert-deftest split-array-two-elements ()
  "The empty string (as opposed to nil) makes the implementation way easier,
and the empty string should always be the discarded side of the list, so its
value shouldn't really matter."
  (should (equal '("" . "b") (bs--split-array "ab"))))

(ert-deftest split-array-singleton ()
  (should (equal '(nil . nil) ;; equivalent to '(nil) but being explicit to
                              ;; emphasize how the result will be used
                 (bs--split-array [:foo]))))

(ert-deftest split-array-empty-array ()
  "I don't think bs--split-array will be called on an empty array, but just in
case, let's define that behavior."
  (should-not (bs--split-array [])))

(ert-deftest find-binary-find-every-element-of-list ()
  (let* ((ns (number-sequence 0 12))
         (2-to-ns (apply #'vector (mapcar #'(lambda (n) (expt 2 n)) ns))))
    (seq-do
     #'(lambda (n)
         (should (equal n (find-binary 2-to-ns (expt 2 n)))))
     ns)))


(provide 'binary-search-additional-tests)
;;; binary-search-additional-tests.el ends here
