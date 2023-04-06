;;; grains.el --- Grains exercise (exercism)  -*- lexical-binding: t; -*-

;;; Commentary:

(defun square (n)
  "Return the number of grains on square N of a chessboard, where square 1 has
1 grain, and each successive square is double the previous grain."
  (expt 2 (1- n)))

(defun total ()
  "Return the total number of grains on the chessboard."
  (let ((board (number-sequence 1 64)))
    (apply #'+ (mapcar #'square board)))
)

(provide 'grains)
;;; grains.el ends here
