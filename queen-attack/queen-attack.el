;;; queen-attack.el --- Queen Attack (exercism)  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:


(defun valid-position-p (queen)
  "For cons cell QUEEN, given as zero-indexed (COLUMN . ROW) return t if COLUMN
and ROW are both between 0 and 7 inclusive; otherwise, return nil."
  (let ((col (car queen))
        (row (cdr queen))
        (min-val 0)
        (max-val 7))
    (and (<= min-val col max-val)
         (<= min-val row max-val))))

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
  (unless (and (valid-position-p white-queen)
               (valid-position-p black-queen))
    (error "Queens must have valid positions."))

  (let ((wcol (car white-queen))
        (wrow (cdr white-queen))
        (bcol (car black-queen))
        (brow (cdr black-queen)))
    (and (not (and (= wcol bcol)
                   (= wrow brow)))           ;; cannot attack own position
         (or (= wcol bcol)                   ;; can attack same column
             (= wrow brow)                   ;; can attack same row
             (qa--diagonalp white-queen      ;; can attack same diagonal
                            black-queen)))))


(provide 'queen-attack)
;;; queen-attack.el ends here
