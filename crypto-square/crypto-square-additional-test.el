(load-file "crypto-square.el")
(declare-function normalize "crypto-square.el" (text))
(declare-function rectangle-values "crypto-square.el" (seq))

(ert-deftest normalize-already-normal ()
  (let ((normal-text "foobarbaz"))
    (should (string= normal-text (normalize normal-text)))))

(ert-deftest normalize-non-string-input-error ()
  (should-error (normalize (mapcar #'string "foobar"))))

(ert-deftest normalize-punctuation-and-capitalization ()
  (should (string= "foobarbaz" (normalize "Foo, bar -- BAZ?!"))))

(ert-deftest rectangle-values-54 ()
  (should (equal '(8 . 7) (rectangle-values (make-list 54 t)))))

(ert-deftest rectangle-values-square-length ()
  (should (equal '(4 . 4) (rectangle-values (make-list 16 t)))))

(ert-deftest rectangle-values-prime-length ()
  (should (equal '(5 . 4) (rectangle-values (make-list 17 t)))))

(ert-deftest rectangle-values-length13-string ()
  (should (equal '(4 . 4) (rectangle-values "foobarbazquxt"))))
