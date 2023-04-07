(load-file "bob.el")

;; (declare-function capitalp "bob.el" (c))
;; (declare-function lowercasep "bob.el" (c))
;; (declare-function whitespacep "bob.el" (c))
(declare-function re-string-match-p "bob.el" (regexp str &optional start))
(declare-function questionp "bob.el" (sentence))
(declare-function all-letters-capital-p "bob.el" (sentence))

;; (ert-deftest capitalp-all-capital-letters ()
;;   (should (seq-every-p #'capitalp (number-sequence ?A ?Z))))

;; (ert-deftest capitalp-fail-on-lowercase-char ()
;;   (should-not (capitalp ?a)))

;; (ert-deftest capitalp-fail-on-punctuation-char ()
;;   (should-not (capitalp (string-to-char "?"))))

;; (ert-deftest capitalp-error-on-string ()
;;   (should-error (capitalp "A")))

;; (ert-deftest lowercasep-all-lowercase-letters ()
;;   (should (seq-every-p #'lowercasep (number-sequence ?a ?z))))

;; (ert-deftest lowercasep-fail-on-uppercase-char ()
;;   (should-not (lowercasep ?A)))

;; (ert-deftest lowercasep-fail-on-punctuation-char ()
;;   (should-not (lowercasep (string-to-char "!"))))

;; (ert-deftest lowercasep-error-on-string ()
;;   (should-error (lowercasep "a")))

;; (ert-deftest whitespacep-all-whitespace ()
;;   (should (seq-every-p #'whitespacep "  \n\n\t \n \t\r")))

;; (ert-deftest whitespacep-some-whitespace ()
;;   (should-not (seq-every-p #'whitespacep " foo bar baz ")))

(ert-deftest re-string-match-p-single-lowercase-letter-among-caps ()
  (should (re-string-match-p "[a-z]+" "FOObARBAZ")))

(ert-deftest re-string-match-p-no-digits-among-letters-ws-and-punct ()
  (should-not (re-string-match-p "[0-9]+" "fooBAR bAZ!@%&")))

(ert-deftest re-string-match-p-ends-with-question-mark-and-maybe-whitespace ()
  (should (re-string-match-p "[?][[:space:]]*$" "A question?   \n\r\t  ")))

(ert-deftest re-string-match-p-wrong-case ()
  (should-not (re-string-match-p "^foo$" "FOO")))

(ert-deftest questionp-not-a-question ()
  (should-not (questionp "This is not a question.")))

(ert-deftest questionp-question ()
  (should (questionp "Is this a question?")))

(ert-deftest question-followed-by-whitespace ()
  (should (questionp "Is this also a question?    ")))

(ert-deftest questionp-not-a-string ()
  (should-error (questionp '("f" "o" "o" "?"))))

(ert-deftest all-letters-capital-p-you-maniacs ()
  (should (all-letters-capital-p "YOU MANIACS! YOU BLEW IT UP!")))

(ert-deftest all-letters-capital-p-calm ()
  (should-not (all-letters-capital-p "Oh hai")))

(ert-deftest all-letters-capital-p-list ()
  (should-error (all-letters-capital-p '(?F ?O ?O ?B ?A ?R))))
