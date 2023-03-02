;;; hamming.el --- Hamming (exercism)  -*- lexical-binding: t; -*-

;;; Commentary: idea: turn the dna strings into lists, zip them together as
;;; cons cells (element by element), filter for cons cells where car != cdr,
;;; and take the length of the resultant list

(require 'seq)

(defun hamming-distance (dna1 dna2)
  "Determine the number of differences between two equal segments of DNA given
as strings."
  (defun string-to-list (str)
    "Convert string to list of letters"
    (mapcar #'string str))
  ;; (defun cars-equal-p (xs ys)
  ;;   "Determine if the first element of two lists are equal"
  ;;   (string= (car xs) (car ys)))
  (defun zip-lists-to-alist (xs ys)
    "Zip two lists into an alist: (x1 x2 ...) (y1 y2 ...) -> ((x1 . y1)
(x2 . y2) ...)"
    (if (eq (length xs) (length ys))
        (progn (defun zip-lists-to-alists-helper (ps qs alist)
                 (if ps
                     (let* ((p (car ps))
                            (q (car qs))
                            (pq (cons p q)))
                       (zip-lists-to-alists-helper
                        (cdr ps) (cdr qs) (cons pq alist)))
                   (reverse alist)))
               (zip-lists-to-alists-helper xs ys '()))
      (error "Lists are not the same length.")))
  (defun cons-cell-eq-p (cons-cell)
    "Check string equality for car and cdr of cons cell."
    (string= (car cons-cell) (cdr cons-cell)))
  (if (eq (length dna1) (length dna2))
      (let ((dna1-list (string-to-list dna1))
            (dna2-list (string-to-list dna2)))
        (seq-filter 'cons-cell-eq-p (zip-lists-to-alist dna1-list dna2-list)))
    (error "DNA segment lengths don't match.")))

;; -- IELM testing --
;; ELISP> (zip-lists-to-alist '("a" "b") '(1 2))
;; (("a" . 1)
;;  ("b" . 2))
;; ELISP> (cons-cell-eq-p '("a" . "b"))
;; nil
;; ELISP> (cons-cell-eq-p '("cat" . "cat"))
;; t

(provide 'hamming)
;;; hamming.el ends here
