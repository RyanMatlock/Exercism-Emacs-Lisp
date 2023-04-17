(load-file "anagram.el")

(declare-function word-signature "anagram.el" (word))
(declare-function anagrams-p "anagram.el" (subj cand))

(ert-deftest word-signature-no-repeated-letters ()
  (should (equal '("o" "p" "s" "t") (word-signature "stop"))))

(ert-deftest word-signature-repeated-letter ()
  (should (equal '("a" "e" "e" "p" "r" "t") (word-signature "repeat"))))

(ert-deftest word-signature-multiple-repeats ()
  (should (equal '("a" "a" "b" "b" "r" "z") (word-signature "barbaz"))))

(ert-deftest anagrams-p-stop-pots ()
  (should (anagrams-p "stop" "pots")))

(ert-deftest anagrams-p-foo-bar ()
  (should-not (anagrams-p "foo" "bar")))

(ert-deftest anagrams-p-subset ()
  (should-not (anagrams-p "dog" "good")))

(ert-deftest anagrams-p-same-word ()
  (let ((word "word"))
    (should-not (anagrams-p word word))))

