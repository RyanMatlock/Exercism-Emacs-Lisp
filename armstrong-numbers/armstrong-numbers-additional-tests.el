(load-file "armstrong-numbers.el")
(declare-function wholenum-to-digits "armstrong-numbers.el" (num))

(ert-deftest wholenum-to-digits-zero ()
  (should (equal '(0) (wholenum-to-digits 0))))

(ert-deftest wholenum-to-digits-multi-digit-number ()
  (should (equal '(1 2 3) (wholenum-to-digits 123))))

(ert-deftest wholenum-to-digits-float-error ()
  (should-error (wholenum-to-digits 1.23)))
