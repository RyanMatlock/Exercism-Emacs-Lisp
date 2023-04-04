(load-file "robot-name.el")
(declare-function random-element "robot-name.el" (xs))

(ert-deftest random-element-non-list-error ()
  (should-error (random-element "foobar")))

(ert-deftest random-element-empty-list ()
  (should-not (random-element '())))

(ert-deftest random-element-singleton-list ()
  "This should at least verify that I'm not picking an element from past the
end of the list."
  (should (equal :foo (random-element '(:foo)))))

(ert-deftest random-element-never-nil ()
  (let ((list-of-lists (make-list 100 (number-sequence 0 1))))
    (should (seq-every-p #'random-element list-of-lists))))

(provide 'robot-name-additional-tests)
