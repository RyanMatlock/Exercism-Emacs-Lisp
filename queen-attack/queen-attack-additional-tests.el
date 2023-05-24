;;; queen-attack-test.el --- Queen Attack (exercism)  -*- lexical-binding: t; -*-

(load-file "queen-attack.el")
(load-file "queen-attack-extra.el")

(declare-function grid-to-position "queen-attack-extra.el" (grid-position))

(ert-deftest grid-to-position-error-on-invalid-string ()
  (should-error (grid-to-position "E35")))

(ert-deftest grid-to-position-error-out-of-range-column ()
  (should-error (grid-to-position "I5")))

(ert-deftest grid-to-position-error-out-of-range-row ()
  (should-error (grid-to-position "B9")))

(ert-deftest grid-to-position-exercism-example-white-queen-c5 ()
  (should (equal '(2 . 3) (grid-to-position "c5"))))

(ert-deftest grid-to-position-exercism-example-black-queen-f2 ()
  (should (equal '(5 . 6) (grid-to-position "f2"))))

(declare-function can-attack-p "queen-attack.el"
                  (white-queen black-queen))

(ert-deftest cannot-attack-if-same-position ()
  (should-not (can-attack-p '(2 . 3) '(2 . 3))))
