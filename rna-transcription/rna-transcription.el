;;; rna-transcription.el -- RNA Transcription (exercism)  -*- lexical-binding: t; -*-

;;; Commentary:

(defun to-rna (strand)
  "Convert sequence of DNA (as a string) to a sequence of RNA (also a string)."
  (defun dna-nucleotide-to-rna (dna-nucleotide)
    "Convert single DNA dna-nucleotide to RNA dna-nucleotide as follows:
`G` -> `C`
`C` -> `G`
`T` -> `A`
`A` -> `U`"
    (cond ((string= dna-nucleotide "G") "C")
          ((string= dna-nucleotide "C") "G")
          ((string= dna-nucleotide "T") "A")
          ((string= dna-nucleotide "A") "U")
          (t (error "Encountered unknown DNA nucleotide."))))
)

(provide 'rna-transcription)
;;; rna-transcription.el ends here
