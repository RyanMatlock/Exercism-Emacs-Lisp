(load-file "word-count.el")
(declare-function sentence-to-words "word-count.el" (sentence))
(declare-function count-element "word-count.el" (elem list))
(declare-function count-elements "word-count.el" (lst))

(ert-deftest sentence-to-words-simple-sentence ()
  (should (equal '("hello" "world") (sentence-to-words "Hello, World!"))))

(ert-deftest sentence-to-words-split-on-newline ()
  (should (equal '("hello" "down" "there")
                 (sentence-to-words "Hello\ndown there!!"))))

;; (ert-deftest sentence-to-words-pathological-case ()
;;   (let ((pathological-sentence
;;          (concat "\"That's the password: 'PASSWORD 123'!\", cried the Special "
;;                  "Agent.\nSo I fled.")))
;;     (should (equal '("that's" "the" "password" "password" "123" "cried" "the"
;;                      "special" "agent" "so" "i" "fled")
;;                    (sentence-to-words pathological-sentence)))))

(ert-deftest count-element-empty-list ()
  (should (zerop (count-element :foo '()))))

(ert-deftest count-element-not-in-list ()
  (should (zerop (count-element :qux '(:foo :bar :baz)))))

(ert-deftest count-element-single-element-in-list ()
  (should (= 1 (count-element :baz '(:foo :bar :baz)))))

(ert-deftest count-element-multiple-elements ()
  (should (= 3 (count-element :foo '(:foo :bar :foo :baz :foo)))))

(ert-deftest count-elments-empty-list ()
  (should-not (count-elements '())))

(ert-deftest count-elements-no-repeats ()
  (should (equal '((:foo . 1) (:bar . 1) (:baz . 1))
                 (count-elements '(:foo :bar :baz)))))

(ert-deftest count-elements-some-repeats ()
  (should (equal '((:foo . 3) (:bar . 2) (:baz . 1))
                 (count-elements '(:foo :foo :bar :baz :foo :bar)))))
