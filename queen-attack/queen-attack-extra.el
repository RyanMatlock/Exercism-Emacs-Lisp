;;; queen-attack-extra.el --- Queen Attack (exercism)  -*- lexical-binding: t; -*-

;; this makes testing so much easier
(defun grid-to-position (grid-coordinate)
  "Convert a standard chess GRID-COORDINATE e.g. \"E5\" to a position cons cell
e.g. (4 . 3)."
  (unless (and (stringp grid-coordinate)
               (length= grid-coordinate 2))
    (error (concat "GRID-COORDINATE must be a string representing a chess "
                   "board position, e.g. \"E5\".")))
  (let* ((valid-rows (number-sequence ?1 ?8))
         (valid-cols (number-sequence ?a ?h))
         (grid-coordiante (downcase grid-coordinate))
         (row (elt grid-coordinate 1))
         (col (elt grid-coordiante 0)))
    (unless (and (member col valid-cols)
                 (member row valid-rows))
      (error "GRID-COORDINATE value out of bounds."))
    (cons (- col (car valid-cols))
          (- 7 (- row (car valid-rows))))))

(provide 'queen-attack-extra)
;;; queen-attack-extra.el ends here
