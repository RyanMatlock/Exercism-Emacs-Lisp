;;; darts.el --- Darts (exercism)  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defun score (x y)
  "Calculate the dart score of a point given by cartesian coordinates X Y. The
scoring is as follows:
radius : score
r <= 1 : 10
1 < r <= 5 : 5
5 < r <= 10 : 1
10 < r : 0,
and the target is located at (0, 0)."

  (defun radius (x y)
    "Calculate the distance of the cartesian coordinates (X, Y) from (0, 0)."
    (sqrt (+ (* x x) (* y y))))

  (let ((r (radius x y))
        (radius-score-alist '((1 . 10)
                              (5 . 5)
                              (10 . 1))))
    ;; more confusing than the obvious cond expression, but it feels Lispier
    (or (cdr-safe
         (car-safe
          (seq-filter #'(lambda (rs) (<= r (car rs))) radius-score-alist)))
        0)))

(provide 'darts)
;;; darts.el ends here
