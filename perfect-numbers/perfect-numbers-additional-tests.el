(load-file "perfect-numbers.el")

(declare-function factors "perfect-numbers.el" (n))

(ert-deftest factors-of-12 ()
  (should (equal '(1 2 3 4 6) (factors 12))))

(ert-deftest factors-error-on-zero ()
  (should-error (factors 0)))

(ert-deftest factors-of-1 ()
  (should (equal '(1) (factors 1))))

(ert-deftest factors-of-2 ()
  (should (equal '(1) (factors 2))))
