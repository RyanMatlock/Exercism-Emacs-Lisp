;;; bob.el --- Bob exercise (exercism)  -*- lexical-binding: t; -*-

;;; Commentary:

;; (defun capitalp (c)
;;   "Return T if char C is a capital letter (in range A-Z); otherwise, return
;; NIL; error if C is not a char."
;;   (cond ((and (numberp c) (char-or-string-p c))
;;          (and (>= c ?A) (<= c ?Z)))
;;         (t (error "C must be a char."))))

;; (defun lowercasep (c)
;;   "Return T if char C is a lowercase letter (in range a-z); otherwise, return
;; NIL; error if C is not a char."
;;   (cond ((and (numberp c) (char-or-string-p c))
;;          (and (>= c ?a) (<= c ?z)))
;;         (t (error "C must be a char."))))

;; (defun whitespacep (c)
;;   (let ((whitespace-chars (mapcar #'string-to-char '(" " "\t" "\n" "\r"))))
;;     (cond ((and (numberp c) (char-or-string-p c))
;;            (seq-some #'(lambda (whitespace-char) (= whitespace-char c))
;;                      whitespace-chars)))))

(defun re-string-match-p (regexp str &optional start)
  "Works like STRING-MATCH-P except instead of returning an index when REGEXP
matches STR, it returns T; otherwise, it returns NIL."
  (let ((case-fold-search nil))
    (not (null (string-match-p regexp str start)))))

(defun questionp (sentence)
  "Return T if string SENTENCE ends in '?'; otherwise return NIL; error if
SENTENCE is not a string."
  (cond ((stringp sentence) (re-string-match-p "[?][[:space:]]*$" sentence))
        (t (error "SENTENCE must be a string."))))

(defun all-letters-capital-p (sentence)
  "Return T if string SENTENCE contains only capital letters and punctuation;
otherwise, return NIL; error if SENTENCE is not a string."
  (cond ((stringp sentence) (and
                             ;; *some* capital letters
                             (re-string-match-p "[A-Z]+" sentence)
                             ;; AND *no* lowercase letters
                             (not (re-string-match-p "[a-z]+" sentence))))
        (t (error "SENTENCE must be a string."))))

(defun response-for (phrase)
  "For string PHRASE:
If PHRASE is a question, respond with, 'Sure.'
If PHRASE is all caps and punctuation, respond with, 'Whoa, chill out!'
If PHRASE is an all caps question, respond with, 'Calm down, I know what I'm
doing!'
If PHRASE is the empty string, respond with, 'Fine. Be that way!'
If PHRASE is anything else, respond with, 'Whatever.'"
  (let ((phrase-sans-newlines (string-replace "\n" " " phrase)))
    (cond ((re-string-match-p "^[[:space:]]*$" phrase-sans-newlines)
           "Fine. Be that way!")
          ((and (all-letters-capital-p phrase-sans-newlines)
                (questionp phrase-sans-newlines))
           "Calm down, I know what I'm doing!")
          ((all-letters-capital-p phrase-sans-newlines) "Whoa, chill out!")
          ((questionp phrase-sans-newlines) "Sure.")
          (t "Whatever."))))

(provide 'bob)
;;; bob.el ends here
