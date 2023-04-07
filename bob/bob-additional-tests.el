(load-file "bob.el")

(declare-function capitalp "bob.el" (c))
(declare-function questionp "bob.el" (sentence))

(ert-deftest capitalp-all-capital-letters ()
  (should (seq-every-p #'capitalp (number-sequence ?A ?Z))))

(ert-deftest capitalp-fail-on-lowercase-char ()
  (should-not (capitalp ?a)))

(ert-deftest capitalp-fail-on-punctuation-char ()
  (should-not (capitalp (string-to-char "?"))))

(ert-deftest capitalp-error-on-string ()
  (should-error (capitalp "A")))

(ert-deftest questionp-not-a-question ()
  (should-not (questionp "This is not a question.")))

(ert-deftest questionp-question ()
  (should (questionp "Is this a question?")))

(ert-deftest questionp-not-a-string ()
  (should-error (questionp '("f" "o" "o" "?"))))
