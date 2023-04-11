;;; hamming.el --- Hamming (exercism)  -*- lexical-binding: t; -*-

;;; Commentary: idea: turn the dna strings into lists, zip them together as
;;; cons cells (element by element), filter for cons cells where car != cdr,
;;; and take the length of the resultant list

(defun hamming-distance (dna1 dna2)
  "Determine the number of differences between two equal segments of DNA given
as strings, DNA1 and DNA2."
  (cond ((eq (length dna1) (length dna2))
         (let ((dna-alist (seq-mapn #'(lambda (x y) (cons x y)) dna1 dna2)))
           (length (seq-filter
                    #'(lambda (cons-cell) (not (equal (car cons-cell)
                                                      (cdr cons-cell))))
                    dna-alist))))
        (t (error "DNA segment lengths don't match."))))

(provide 'hamming)
;;; hamming.el ends here
