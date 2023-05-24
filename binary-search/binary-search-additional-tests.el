;;; binary-search-additional-tests.el --- Binary Search (exercism)  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:


(load-file "binary-search.el")
(declare-function bs--safe-1+ "binary-search.el" (value))
(declare-function bs--middle-index "binary-search.el" (seq))
(declare-function bs--array-bisect "binary-search.el" (arr))


(ert-deftest safe-1+-integer ()
  (should (= 2 (bs--safe-1+ 1))))

(ert-deftest safe-1+-nil ()
  (should-not (bs--safe-1+ nil)))

(ert-deftest middle-index-empty-seq ()
  (should-not (bs--middle-index [])))

(ert-deftest middle-index-singleton-seq ()
  (should (= 0 (bs--middle-index [:foo]))))

(ert-deftest middle-index-even-sequence ()
  (should (= 2 (bs--middle-index "012345"))))

(ert-deftest middle-index-odd-sequence ()
  (should (= 2 (bs--middle-index '(0 1 2 3 4)))))

(ert-deftest array-bisect-requires-array ()
  (should-error (bs--array-bisect '(:foo :bar))))

;; use car-safe/cdr-safe to safely unpack a bisected array
(ert-deftest array-bisect-empty-array ()
  (should-not (bs--array-bisect [])))

(ert-deftest array-bisect-singleton ()
  (should (equal '([:foo]) (bs--array-bisect [:foo]))))

(ert-deftest array-bisect-two-elements ()
  (should (equal '([:foo] . [:bar]) (bs--array-bisect [:foo :bar]))))

;; always favor the left/car side for array of odd length
(ert-deftest array-bisect-three-elements ()
  (should (equal '([:foo :bar] . [:baz])
                 (bs--array-bisect [:foo :bar :baz]))))

(ert-deftest array-bisect-string-even-length ()
  (should (equal '("foo" . "bar") (bs--array-bisect "foobar"))))

(ert-deftest array-bisect-string-odd-length ()
  (should (equal '("taco" . "cat") (bs--array-bisect "tacocat"))))

(provide 'binary-search-additional-tests)
;;; binary-search-additional-tests.el ends here
