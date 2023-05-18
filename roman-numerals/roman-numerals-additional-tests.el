(load-file "roman-numerals.el")

(declare-function num-to-alist "roman-numerals.el" (num))

(ert-deftest num-to-alist-multi-digit ()
  (should (equal '((9 . 100)
                   (1 . 10)
                   (3 . 1))
                 (num-to-alist 913))))
