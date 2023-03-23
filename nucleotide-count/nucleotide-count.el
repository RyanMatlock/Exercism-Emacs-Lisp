;;; nucleotide-count.el --- nucleotide-count Exercise (exercism)  -*- lexical-binding: t; -*-

;;; Commentary:

(defun nucleotide-count (sequence)
  "Count number of instances of A, C, G, and T within SEQUENCE and report
results as an alist."
  (defun instances-of-char-in-string (c str)
    "Count the number of times char C appears in string STR."
    (length (seq-filter (lambda (letter) (eq c letter)) str)))
  (let* ((nucleotides '(?A ?C ?G ?T))
         (ncount-alist (mapcar
                        #'(lambda (nucleotide)
                            (cons
                             nucleotide
                             (instances-of-char-in-string
                              nucleotide
                              sequence)))
                        nucleotides))
         (num-nucleotides (apply #'+ (mapcar #'cdr ncount-alist))))
    (if (length= sequence num-nucleotides)
        ncount-alist
      (error (format "Invalid nucleotide in '%s'." sequence)))))

(provide 'nucleotide-count)
;;; nucleotide-count.el ends here
