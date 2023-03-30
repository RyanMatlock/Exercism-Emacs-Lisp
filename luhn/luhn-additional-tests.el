(load-file "luhn.el")
(declare-function valid-luhn-char-p "luhn.el" (c))

(ert-deftest valid-luhn-char-p-on-str ()
  (should-error (valid-luhn-char-p " ")))

(ert-deftest valid-luhn-char-p-on-digit ()
  (should (valid-luhn-char-p ?7)))

(ert-deftest valid-luhn-char-p-on-space ()
  (should (valid-luhn-char-p (string-to-char " "))))

(ert-deftest valid-luhn-char-p-on-letter ()
  (should-not (valid-luhn-char-p ?g)))
