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
  "For string PHRASE:
If PHRASE is a question, respond with, 'Sure.'
If PHRASE is all caps and punctuation, respond with, 'Whoa, chill out!'
If PHRASE is an all caps question, respond with, 'Calm down, I know what I'm
doing!'
If PHRASE is the empty string, respond with, 'Fine. Be that way!'
If PHRASE is anything else, respond with, 'Whatever.'"
  (cond ((seq-empty-p phrase) "Fine. Be that way!")
        ((and (all-letters-capital-p phrase) (questionp phrase))
         "Calm down, I know what I'm doing!")
        ((all-letters-capital-p phrase) "Whoa, chill out!")
        ((questionp phrase) "Sure.")
        (t "Whatever.")))

(provide 'bob)
;;; bob.el ends here
