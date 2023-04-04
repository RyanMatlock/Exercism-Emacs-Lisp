(load-file "robot-name.el")
(declare-function random-element "robot-name.el" (xs))

(ert-deftest random-element-non-list-error ()
  (should-error (random-element "foobar")))

(ert-deftest random-element-empty-list ()
  (should-not (random-element '())))

(ert-deftest random-element-singleton-list ()
  (let ((elem :foo)
        (num-lists 100))
    (should (seq-every-p #'(lambda (lst) (equal elem (random-element lst)))
                         (make-list num-lists (list elem))))))

(ert-deftest random-element-never-nil ()
  "Call RANDOM-ELEMENT a bunch of times on small lists (explicitly not
containing NIL) and ensure that it never 'goes off the end' or returns NIL."
  (let ((list-of-lists (make-list 1000 (number-sequence 0 1))))
    (should (seq-every-p #'random-element list-of-lists))))


(ert-deftest random-element-every-element-selected ()
  "Call RANDOM-ELEMENT on the same list a bunch of times and ensure that every
element is hit at least once."

  ;; probably bad form to define a function just to perform a test because it
  ;; assumes this function works, but ¯\_(ツ)_/¯
  (defun elem-in-list-p (elem lst &optional num-tests)
    "Check if ELEM is in list LST by calling RANDOM-ELEMENT on LST and checking
for equality with ELEM NUM-TESTS times (default for NUM-TESTS: 1000). Note that
NUM-TESTS should be >> (LENGTH LST)."
    (let* ((trials (if (wholenump num-tests) num-tests (* 1000 (length lst))))
           (list-of-lists (make-list trials lst)))
      (seq-some #'(lambda (xs) (equal elem (random-element xs)))
                list-of-lists)))

  (let* ((ys '(:foo :bar :baz)))
    (should (seq-every-p #'(lambda (y) (elem-in-list-p y ys)) ys))))

(provide 'robot-name-additional-tests)
