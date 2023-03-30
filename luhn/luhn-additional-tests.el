(load-file "luhn.el")
(declare-function valid-luhn-char-p "luhn.el" (c))
(declare-function remove-all-spaces "luhn.el" (str))

(ert-deftest valid-luhn-char-p-on-str ()
  (should-error (valid-luhn-char-p " ")))

(ert-deftest valid-luhn-char-p-on-digit ()
  (should (valid-luhn-char-p ?7)))

(ert-deftest valid-luhn-char-p-on-space ()
  (should (valid-luhn-char-p (string-to-char " "))))

(ert-deftest valid-luhn-char-p-on-letter ()
  (should-not (valid-luhn-char-p ?g)))

(ert-deftest remove-all-spaces-empty-string ()
  (should (string= "" (remove-all-spaces ""))))

(ert-deftest remove-all-spaces-only-spaces ()
  (should (string= "" (remove-all-spaces "    "))))

(ert-deftest remove-all-spaces-around-every-character ()
  (should (string= "foo" (remove-all-spaces " f o o "))))

(ert-deftest remove-all-spaces-scattered-spaces ()
  (should (string= "foobar" (remove-all-spaces "fo oba   r   "))))
