;;; difference-of-squares.el --- Difference of Squares (exercism)  -*- lexical-binding: t; -*-

;;; Commentary: got it on the first try. Based on the prompt, I thought there'd
;;; be an issue where the naive solution was impractically inefficient
;;; ¯\_(ツ)_/¯

(defun sum-of-squares (n)
  "Return the sum of the square of numbers 1 through N."
  (let* ((1-to-n (number-sequence 1 n))
         (squares (mapcar #'(lambda (x) (* x x)) 1-to-n)))
    (apply #'+ squares)))

(defun square-of-sum (n)
  "Return the square of the sum of the numbers 1 through N."
  (let* ((1-to-n (number-sequence 1 n))
         (sum (apply #'+ 1-to-n)))
    (* sum sum)))

(defun difference (n)
  "Return the difference of square of the sum of numbers 1 through N and the
sum of each number 1 through N squared."
  (- (square-of-sum n) (sum-of-squares n)))

(provide 'difference-of-squares)
;;; difference-of-squares.el ends here
