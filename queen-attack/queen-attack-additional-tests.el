;;; queen-attack-test.el --- Queen Attack (exercism)  -*- lexical-binding: t; -*-

(load-file "queen-attack.el")

(declare-function can-attack-p "queen-attack.el"
                  (white-queen black-queen))

(ert-deftest cannot-attack-if-same-position ()
  (should-not (can-attack-p '(2 . 3) '(2 . 3))))
