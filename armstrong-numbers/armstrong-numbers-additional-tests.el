(load-file "armstrong-numbers.el")
(declare-function char-to-number "armstrong-numbers.el" (c))
(declare-function wholenum-to-digits "armstrong-numbers.el" (num))

(ert-deftest char-to-number-zero ()
  (should (= 0 (char-to-number ?0))))

(ert-deftest char-to-number-nine ()
  (should (= 9 (char-to-number ?9))))

(ert-deftest char-to-number-error-on-letter ()
  (should-error (char-to-number ?A)))

(ert-deftest char-to-number-should-error-on-string-digit ()
  (should-error (char-to-number "4")))

(ert-deftest wholenum-to-digits-zero ()
  (should (equal '(0) (wholenum-to-digits 0))))

(ert-deftest wholenum-to-digits-multi-digit-number ()
  (should (equal '(1 2 3) (wholenum-to-digits 123))))

(ert-deftest wholenum-to-digits-float-error ()
  (should-error (wholenum-to-digits 1.23)))
