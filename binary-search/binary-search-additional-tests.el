;;; binary-search-additional-tests.el --- Binary Search (exercism)  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:


(load-file "binary-search.el")
(declare-function bs--safe-1+ "binary-search.el" (value))
(declare-function bs--array-bisect "binary-search.el" (seq))

(ert-deftest safe-1+-integer ()
  (should (= 2 (bs--safe-1+ 1))))

(ert-deftest safe-1+-nil ()
  (should-not (bs--safe-1+ nil)))

;; use car-safe/cdr-safe to safely unpack a bisected array
(ert-deftest array-bisect-empty-array ()
  (should-not (bs--array-bisect [])))

(ert-deftest array-bisect-singleton ()
  (should (equal '([:foo]) (bs--array-bisect [:foo]))))

(ert-deftest array-bisect-two-elements ()
  (should (equal '([:foo] . [:bar]) (bs--array-bisect [:foo :bar]))))

(ert-deftest array-bisect-three-elements ()
  (should (equal '([:foo :bar] . [:baz])
                 (bs--array-bisect [:foo :bar :baz]))))

(provide 'binary-search-additional-tests)
;;; binary-search-additional-tests.el ends here
