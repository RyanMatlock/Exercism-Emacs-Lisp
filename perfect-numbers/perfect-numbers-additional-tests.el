(load-file "perfect-numbers.el")

(declare-function factors "perfect-numbers.el" (n))
(declare-function aliquot-sum "perfect-numbers.el" (n))

(ert-deftest factors-of-12 ()
  (should (equal '(1 2 3 4 6) (factors 12))))

;; classify will take care of error handling
;; (ert-deftest factors-error-on-zero ()
;;   (should-error (factors 0)))

(ert-deftest factors-of-1 ()
  (should (equal '() (factors 1))))

(ert-deftest factors-of-2 ()
  (should (equal '(1) (factors 2))))

(ert-deftest aliquot-sum-28 ()
  (should (= 28 (aliquot-sum 28))))

(ert-deftest aliquot-sum-24 ()
  (should (= 36 (aliquot-sum 24))))

(ert-deftest aliquot-sum-prime ()
  (should (= 1 (aliquot-sum 47))))
