;;; rna-transcription.el -- RNA Transcription (exercism)  -*- lexical-binding: t; -*-

;;; Commentary:

(defun to-rna (strand)
  "Convert sequence of DNA (as a string) to a sequence of RNA (also a string)."
  (defun dna-nucleotide-to-rna (dna-nucleotide)
    "Convert single DNA dna-nucleotide to RNA dna-nucleotide as follows:
`G` -> `C`
`C` -> `G`
`T` -> `A`
`A` -> `U`
otherwise -> error

Note: this will throw an error for lowercase inputs."
    (cond ((string= dna-nucleotide "G") "C")
          ((string= dna-nucleotide "C") "G")
          ((string= dna-nucleotide "T") "A")
          ((string= dna-nucleotide "A") "U")
          (t (error "Encountered unknown DNA nucleotide."))))
  (let* ((dna-sequence (mapcar #'string strand))
         (rna-sequence (mapcar #'dna-nucleotide-to-rna dna-sequence)))
    (mapconcat #'identity rna-sequence "")))

;; -- IELM testing --
;; ELISP> (mapcar #'string "ACTG")
;; ("A" "C" "T" "G")
;; ELISP> (mapcar #'dna-nucleotide-to-rna '("A" "C" "T" "G"))
;; ("U" "G" "A" "C")
;; ELISP> (mapconcat #'identity '("U" "G" "A" "C") "")
;; "UGAC"
;; ELISP> (to-rna "ACTG")
;; "UGAC"

(provide 'rna-transcription)
;;; rna-transcription.el ends here
