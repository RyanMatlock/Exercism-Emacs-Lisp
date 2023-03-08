;;; etl.el --- etl Exercise (exercism)  -*- lexical-binding: t; -*-

;;; Commentary: based on etl-test.el, it looks like I should be using a hash
;;; table instead of an alist, and the data will be presented in the form of a
;;; hash table. Note that the letters in the input are mixed case.

;; can't figure out why this is passing tests locally but not on Exercism
;; see https://exercism.org/docs/using/solving-exercises/tests-pass-locally
;; and https://exercism.org/docs/tracks/emacs-lisp/tests
;; although I can't figure out what's wrong yet

(defun etl (data)
  "Translate hash table data, where keys are points and the values are lists of
mixed case letters to a hash table where keys are lowercase letters and values
are points."
  (let ((newdata (make-hash-table :test #'equal)))
    (defun swap-kv (k vs)
      "Apply to data with maphash"
      (let ((v (car vs)))
        ;; it's weird writing impure code like this
        (cond ((and v (stringp v))
               (progn (puthash (downcase v) k newdata)
                      (swap-kv k (cdr vs))))
              (v (error "New keys must be chars/strings."))
              (t nil)))
      (if (< k 0)
          (error "Keys should be non-negative.")
        (swap-kv-helper k vs)))
    (maphash #'swap-kv data)
    newdata))

;;; -- Exercism server test failure info --

;; We received the following error when we ran your code:
;; Loading /mnt/exercism-iteration/etl.el (source)...
;; Running 4 tests (2023-03-08 07:43:07+0000, selector `t')
;;    passed  1/4  empty-hash-test (0.000105 sec)

;;    passed  2/4  list-of-non-string-values-test (0.002601 sec)

;; Test mixed-case-test backtrace:
;;   signal(void-function (swap-kv-helper))
;;   apply(signal (void-function (swap-kv-helper)))
;;   (setq value-7 (apply fn-5 args-6))
;;   (unwind-protect (setq value-7 (apply fn-5 args-6)) (setq form-descri
;;   (if (unwind-protect (setq value-7 (apply fn-5 args-6)) (setq form-de
;;   (let (form-description-9) (if (unwind-protect (setq value-7 (apply f
;;   (let ((value-7 'ert-form-evaluation-aborted-8)) (let (form-descripti
;;   (let* ((fn-5 #'hash-equal) (args-6 (condition-case err (let ((signal
;;   (let ((lexical-binding t)) (let* ((fn-5 #'hash-equal) (args-6 (condi
;;   (closure (t) nil (let ((lexical-binding t)) (let* ((fn-5 #'hash-equa
;;   ert--run-test-internal(#s(ert--test-execution-info :test #s(ert-test
;;   ert-run-test(#s(ert-test :name mixed-case-test :documentation nil :b
;;   ert-run-or-rerun-test(#s(ert--stats :selector t :tests [#s(ert-test 
;;   ert-run-tests(t #f(compiled-function (event-type &rest event-args) #
;;   ert-run-tests-batch(nil)
;;   ert-run-tests-batch-and-exit()
;;   command-line-1(("-l" "ert" "-l" "/mnt/exercism-iteration/etl-test.el
;;   command-line()
;;   normal-top-level()
;; Test mixed-case-test condition:
;;     (void-function swap-kv-helper)
;;    FAILED  3/4  mixed-case-test (0.000124 sec)

;;    passed  4/4  negative-key-test (0.000083 sec)


;; Ran 4 tests, 3 results as expected, 1 unexpected (2023-03-08 07:43:07+0000, 0.124185 sec)

;; 1 unexpected results:
;;    FAILED  mixed-case-test

(provide 'etl)
;;; etl.el ends here
