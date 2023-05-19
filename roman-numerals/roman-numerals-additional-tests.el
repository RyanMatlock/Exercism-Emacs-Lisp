(load-file "roman-numerals.el")

(declare-function num-to-alist "roman-numerals.el" (num))

(ert-deftest num-to-alist-multi-digit ()
  (should (equal '((9 . 100)
                   (1 . 10)
                   (3 . 1))
                 (num-to-alist 913))))

(declare-function digit-oom-to-roman "roman-numerals.el" (digit-oom-ccell))

(ert-deftest digit-oom-to-roman-one ()
  (should (string= "I" (digit-oom-to-roman '(1 . 1)))))

(ert-deftest digit-oom-to-roman-nine ()
  (should (string= "IX" (digit-oom-to-roman '(9 . 1)))))

(ert-deftest digit-oom-to-roman-ten ()
  (should (string= "X" (digit-oom-to-roman '(1 . 10)))))

(ert-deftest digit-oom-to-roman-eighty ()
  (should (string= "LXXX" (digit-oom-to-roman '(8 . 10)))))

(ert-deftest digit-oom-to-roman-ninety ()
  (should (string= "XC" (digit-oom-to-roman '(9 . 10)))))

(ert-deftest digit-oom-to-roman-four ()
  (should (string= "IV" (digit-oom-to-roman '(4 . 1)))))

(ert-deftest digit-oom-to-roman-forty ()
  (should (string= "XL" (digit-oom-to-roman '(4 . 10)))))
