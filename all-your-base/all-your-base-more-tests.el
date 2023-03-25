(load-file "all-your-base.el")

(declare-function base-n-to-base-10 "all-your-base.el" (digits base-n))

(ert-deftest base-2-to-base-10-42 ()
  (should (equal 42 (base-n-to-base-10 '(1 0 1 0 1 0) 2))))

(declare-function base-10-to-base-n "all-your-base.el" (base-10-num base-n))

(ert-deftest base-10-to-base-2-42 ()
  (should (equal '(1 0 1 0 1 0) (base-10-to-base-n 42 2))))

(ert-deftest base-10-to-n-to-10-identity ()
  ;; for some reason, having let outside of ert-deftest didn't work ¯\_(ツ)_/¯
  (let ((number 2432)
        (base 5))
    (should
     (equal number (base-n-to-base-10 (base-10-to-base-n number base) base)))))

(ert-deftest base-n-to-10-to-n-identity ()
  (let ((digits '(7 8 4 2 0 6 3))
        (base 9))
    (should
     (equal digits
            (base-10-to-base-n (base-n-to-base-10 digits base) base)))))

(provide 'all-your-base-more-tests)

