(load-file "all-your-base.el")

(declare-function base-n-to-base-10 "all-your-base.el" (digits base-n))

(ert-deftest base-2-to-base-10-42 ()
  (should (equal 42 (base-n-to-base-10 '(1 0 1 0 1 0) 2))))

(declare-function base-10-to-base-n "all-your-base.el" (base-10-num base-n))

(ert-deftest base-10-to-base-2-42 ()
  (should (equal '(1 0 1 0 1 0) (base-10-to-base-n (42 2)))))

(provide 'all-your-base-more-tests)

