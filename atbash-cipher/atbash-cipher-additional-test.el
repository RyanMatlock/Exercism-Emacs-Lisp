(load-file "atbash-cipher.el")
(declare-function
 chunk-text "atbash-cipher.el" (chunk-size text &optional sep))

(ert-deftest chunk-text-short-string ()
  (let ((text "foo"))
    (should (string= text (chunk-text (1+ (length text)) text)))))

(ert-deftest chunk-text-two-chunks ()
  (should (string= "fooba r" (chunk-text 5 "foobar"))))

(ert-deftest chunk-text-multiple-chunks ()
  (should (string= "fooba rbazq ux" (chunk-text 5 "foobarbazqux"))))

(ert-deftest chunk-text-with-specified-separator ()
  (should (string= "foo.bar.baz" (chunk-text 3 "foobarbaz" "."))))
