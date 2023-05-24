;;; queen-attack.el --- Queen Attack (exercism)  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:


(defun valid-position-p (queen)
  "For cons cell QUEEN, given as zero-indexed (COLUMN . ROW) return t if COLUMN
and ROW are both between 0 and 7 inclusive; otherwise, return nil."

  (defun betweenp (n lower upper)
    (and (<= lower n)
         (<= n upper)))

  (let ((col (car queen))
        (row (cdr queen))
        (col-row-min 0)
        (col-row-max 7))
    (and (betweenp col col-row-min col-row-max)
         (betweenp row col-row-min col-row-max))))

(defun qa--diagonalp (pos1 pos2)
    "Return t if the absolute value of the difference in the `car' and `cdr' of
POS1 and POS2 is equal (convince yourself that this occurs if and only if POS1
and POS2 are on a diagonal)."
    (let ((col1 (car pos1))
          (row1 (cdr pos1))
          (col2 (car pos2))
          (row2 (cdr pos2)))
      (= (abs (- col1 col2))
         (abs (- row1 row2)))))

(defun can-attack-p (white-queen black-queen)
  (error
   "Delete this S-Expression and write your own implementation"))


(provide 'queen-attack)
;;; queen-attack.el ends here
