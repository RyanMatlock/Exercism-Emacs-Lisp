(load-file "all-your-base.el")

(declare-function base-n-to-base-10 "all-your-base.el" (digits base-n))

(ert-deftest base-2-to-base-10-42 ()
  (should (equal 42 (base-n-to-base-10 '(1 0 1 0 1 0) 2))))

(declare-function base-10-to-base-n "all-your-base.el" (base-10-num base-n))

(ert-deftest base-10-to-base-2-42 ()
  (should (equal '(1 0 1 0 1 0) (base-10-to-base-n 42 2))))

;; (let ((digits '(3 0 4 8 0 3))
;;       (base-n 7))
;;   (ert-deftest base-n-to-base-10-to-base-n-identity ()
;;     (should
;;      (equal digits
;;             (base-10-to-base-n (base-n-to-base-10 digits base-n) base-n)))))

;; (ert-deftest base-n-to-10-to-n-identity ()
;;   (should (equal '(9 1 3)
;;                  (base-10-to-base-n (base-n-to-base-10 '(9 1 3) 7) 7))))

(let ((num 923)
      (base-n 7))
  (ert-deftest base-10-to-n-to-10-identity ()
    (should
     (equal num (base-n-to-base-10 (base-10-to-base-n num base-n) base-n)))))

(provide 'all-your-base-more-tests)

