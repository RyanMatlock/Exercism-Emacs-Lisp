;;; bob.el --- Bob exercise (exercism)  -*- lexical-binding: t; -*-

;;; Commentary:

(defun capitalp (c)
  "Return T if char C is a capital letter (in range A-Z); otherwise, return
NIL; error if C is not a char."
  (cond ((and (numberp c) (char-or-string-p c))
         (and (>= c ?A) (<= c ?Z)))
        (t (error "C must be a char."))))

(defun lowercasep (c)
  "Return T if char C is a lowercase letter (in range a-z); otherwise, return
NIL; error if C is not a char."
  (cond ((and (numberp c) (char-or-string-p c))
         (and (>= c ?a) (<= c ?z)))
        (t (error "C must be a char."))))

(defun questionp (sentence)
  "Return T if string SENTENCE ends in '?'; otherwise return NIL; error if
SENTENCE is not a string."
  (cond ((stringp sentence)
         (let ((last (string (elt sentence (1- (length sentence))))))
           (string= last "?")))
        (t (error "SENTENCE must be a string."))))

(defun all-letters-capital-p (sentence)
  "Return T if string SENTENCE contains only capital letters and punctuation;
otherwise, return NIL; error if SENTENCE is not a string."
  (cond ((stringp sentence)
         (and (seq-some #'capitalp sentence)
              (not (seq-some #'lowercasep sentence))))
        (t (error "SENTENCE must be a string."))))

(defun response-for (phrase)
;;; Code:
  )

(provide 'bob)
;;; bob.el ends here
