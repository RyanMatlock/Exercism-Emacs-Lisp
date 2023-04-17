(load-file "anagram.el")

(declare-function word-signature "anagram.el" (word))

(ert-deftest word-signature-no-repeated-letters ()
  (should (equal '("o" "p" "s" "t") (word-signature "stop"))))

(ert-deftest word-signature-repeated-letter ()
  (should (equal '("a" "e" "e" "p" "r" "t") (word-signature "repeat"))))

(ert-deftest word-signature-multiple-repeats ()
  (should (equal '("a" "a" "b" "b" "r" "z") (word-signature "barbaz"))))
