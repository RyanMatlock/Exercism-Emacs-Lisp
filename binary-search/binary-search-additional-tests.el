;;; binary-search-additional-tests.el --- Binary Search (exercism)  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:


(load-file "binary-search.el")
(declare-function bs--safe-1+ "binary-search.el" (value))

(ert-deftest safe-1+-integer ()
  (should (= 2 (bs--safe-1+ 1))))

(ert-deftest safe-1+-nil ()
  (should-not (bs--safe-1+ nil)))

(provide 'binary-search-additional-tests)
;;; binary-search-additional-tests.el ends here
