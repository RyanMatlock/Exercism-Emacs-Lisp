(load-file "luhn.el")
(declare-function valid-luhn-char-p "luhn.el" (c))
(declare-function remove-all-spaces "luhn.el" (str))
(declare-function zip-lists-alist "luhn.el" (xs ys))

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

(ert-deftest zip-lists-to-alist-list-with-nil-t ()
  (should (equal '((:foo . nil) (:bar . t) (:baz . nil) (:qux .t))
                 (zip-lists-alist '(:foo :bar :baz :qux) '(nil t)))))

(ert-deftest zip-lists-to-alist-with-nil ()
  (should (equal '((:foo) (:bar) (:baz))
                 (zip-lists-alist '(:foo :bar :baz) '(nil)))))

(ert-deftest zip-lists-alist-longer-ys ()
  (should (equal '((:foo . 1) (:bar . 2) (:baz . 3))
                 (zip-lists-alist '(:foo :bar :baz) '(1 2 3 4 5)))))

(ert-deftest zip-lists-alist-xs-not-sequence-error ()
  (should-error (zip-lists-alist :foo '(1 2))))

(ert-deftest zip-lists-alist-ys-not-sequence-error ()
  (should-error (zip-lists-alist '(:foo :bar) :baz)))

(ert-deftest zip-lists-alist-non-list-sequences ()
  (should (equal '((?f . ?b) (?o . ?a) (?o . ?r))
                 (zip-lists-alist "foo" "bar"))))
