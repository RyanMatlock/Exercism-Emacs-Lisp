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

(defun can-attack-p (white-queen black-queen)
  (error
   "Delete this S-Expression and write your own implementation"))


(provide 'queen-attack)
;;; queen-attack.el ends here
