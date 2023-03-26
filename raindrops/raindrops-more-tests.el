(load-file "raindrops.el")
(declare-function factorp "raindrops.el" (num divisor))

(ert-deftest factorp-error-on-zero-divisor ()
  (should-error (factorp 1 0)))

(ert-deftest factorp-not-a-factor ()
  (should (equal nil (factorp 13 2))))

(ert-deftest factorp-is-a-factor ()
  (should (equal t (factorp 9 3))))

(provide 'raindrops-more-test)

