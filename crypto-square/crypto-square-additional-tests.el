(load-file "crypto-square.el")
(declare-function normalize "crypto-square.el" (text))
(declare-function rectangle-values "crypto-square.el" (seq))
(declare-function normalized-text-to-block "crypto-square.el" (text))
(declare-function
 block-to-ciphertext "crypto-square.el" ((block &optional sep)))
(declare-function cb--string-pad "crypto-square.el" (str n &optional padding))

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

(ert-deftest normalized-text-to-block-square-text ()
  (should (equal '("foo" "bar" "baz")
                 (normalized-text-to-block "foobarbaz"))))

(ert-deftest block-to-ciphertext-simple-block ()
  (should
   (string= "fbb oaa orz"
            (block-to-ciphertext '("foo" "bar" "baz")))))

(ert-deftest block-to-ciphertext-with-padding ()
  (should
   (string= "fazt orq  obu  bax "
            (block-to-ciphertext '("foob" "arba" "zqux" "t    ")))))

(ert-deftest cb--string-pad-str-longer-than-n ()
  (let* ((longstring "foobarbaz")
         (n (- (length longstring) 3)))
    (should (string= longstring (cb--string-pad longstring n)))))

(ert-deftest cb--string-pad-equivalent-to-string-pad ()
  (let ((str "foobar")
        (n 11))
    (should (string= (string-pad str n) (cb--string-pad str n)))))
