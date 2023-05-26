;;; affine-cipher-additional-tests.el --- Affine Cipher (exercism)  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:


(load-file "affine-cipher.el")
(declare-function ac--gcd "affine-cipher.el" (p q))
(declare-function ac--coprimep "affine-cipher.el" (n m))

(ert-deftest gcd-simple-example ()
  (should (eq 5 (ac--gcd 10 15))))

(ert-deftest gcd-wikipedia-first-example ()
  (should (eq 21 (ac--gcd 252 105))))

(ert-deftest coprimep-should-error-on-negative-int ()
  (should-error (ac--coprimep 8 -1)))

(ert-deftest coprimep-primes-are-always-coprime ()
  (should (ac--coprimep 7 47)))

(ert-deftest coprimep-nonprime-andprime-always-coprime ()
  (should (ac--coprimep 60 109)))

(ert-deftest coprimep-easy-nonprimes-without-shared-factors ()
  (should (ac--coprimep (* 2 3 5)
                        (* 7 17))))

(ert-deftest coprimep-harder-nonprimes-without-shared-factors ()
  (should (ac--coprimep (* 2 3 5 11 13)
                        (* 7 17 29 37))))

(ert-deftest coprimep-easy-shared-factor ()
  (should-not (ac--coprimep (* 2 13)
                            (* 2 17))))

(ert-deftest coprimep-medium-shared-factor ()
  (should-not (ac--coprimep (* 2 13 29)
                            (* 3 13 37))))

(provide 'affine-cipher-additional-tests)
;;; affine-cipher-additional-tests.el ends here
