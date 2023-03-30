(load-file "run-length-encoding.el")
(declare-function str-digit-p "run-length-encoding.el" (str))
(declare-function repeat-str "run-length-encoding.el" (n str &optional join))

(ert-deftest str-digit-p-letters ()
  (let* ((uppercase (mapcar #'string (number-sequence ?A ?Z)))
         (lowercase (mapcar #'string (number-sequence ?a ?z)))
         (letters (append uppercase lowercase)))
    (should (equal nil (seq-every-p #'str-digit-p letters)))))

(ert-deftest str-digit-p-digits ()
  (let ((digits (mapcar #'string (number-sequence ?0 ?9))))
    (should (equal t (seq-every-p #'str-digit-p digits)))))

(ert-deftest str-digit-p-punctuation ()
  (let ((punctuation-list '("!" "@" "#" "&" ")" "<")))
    (should (equal nil (seq-every-p #'str-digit-p punctuation-list)))))

(ert-deftest repeat-str-less-than-zero-error ()
  (should-error (repeat-str -1 "foo")))

(ert-deftest repeat-str-zero-empty-string ()
  (should (equal "" (repeat-str 0 "foo"))))

(ert-deftest repeat-str-expected-behavior ()
  (should (equal "foofoofoo" (repeat-str 3 "foo"))))

(ert-deftest repeat-str-period-separator ()
  (should (equal "foo.foo.foo" (repeat-str 3 "foo" "."))))
