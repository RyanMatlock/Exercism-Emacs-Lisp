(load-file "run-length-encoding.el")
(declare-function str-digit-p "run-length-encoding.el" (str))

;; (let ((uppercase (mapcar #'string (number-sequence ?A ?Z)))
;;       (lowercase (mapcar #'string (number-sequence ?a ?z)))
;;       (digits (mapcar #'string (number-sequence ?0 ?9)))
;;       ))

(ert-deftest str-digit-p-letters ()
  (let* ((uppercase (mapcar #'string (number-sequence ?A ?Z)))
         (lowercase (mapcar #'string (number-sequence ?a ?z)))
         (letters (append uppercase lowercase)))
    (should (equal nil (seq-every-p #'str-digit-p letters)))))

(ert-deftest str-digit-p-digits ()
  (let ((digits (mapcar #'string (number-sequence ?0 ?9))))
    (should (equal t (seq-every-p #'str-digit-p digits)))))

(ert-deftest str-digit-p-punctuation ()
  (let ((punctuation-list '("!" "@" "#" "&" ")" "<")))
    (should (equal nil (seq-every-p #'str-digit-p punctuation-list)))))


