;;; allergies.el --- Allergies Exercise (exercism)  -*- lexical-binding: t; -*-

;;; Commentary: The allergy number is written as
;;; A = \sum_{i=1}^n c_i \cdot 2^{i-1}, where c_i = 0 if one is not allergic to
;;; the i-th allergen and 1 if one is allergic to it. An important observation
;;; is that \sum_{i=0}^k 2^i < 2^{k+1}, so you can figure out the coefficients
;;; by successively subtracting the largest c_i ≤ A.

(defvar allergen-lookup-alist
  ;; using number as key to make it easier to look up allergens later
  '((1 . "eggs")
    (2 . "peanuts")
    (4 . "shellfish")
    (8 . "strawberries")
    (16 . "tomatoes")
    (32 . "chocolate")
    (64 . "pollen")
    (128 . "cats"))
  "Allergen alist: key is the number, and value is the allergen as a string.")

(defun allergen-list (score)
  "List all allergens with a given SCORE."
  (defun highest-power-of-2 (n)
    "Return largest p = 2^i ≤ n."
    (if (< n 1)
        (error "Argument must be greater than or equal to 1.")
      (defun highest-power-of-2-helper (n prev)
        (let ((next (* prev 2)))
          (if
              ;; if (n - next) < 0
              (< (- n next) 0)
              prev
            (highest-power-of-2-helper n next)))))
    (highest-power-of-2-helper n 1))
  )

(defun allergic-to-p (score allergen)
"Check if Allergic to allergen based on SCORE and ALLERGEN."
;;; Code:
)

;; -- IELM testing --
;; ELISP> (alist-get 128 allergen-lookup-alist)
;; "cats"

;; ELISP> (highest-power-of-2 32)
;; 32 (#o40, #x20, ? )
;; ELISP> (highest-power-of-2 1204)
;; 1024 (#o2000, #x400)
;; ELISP> (highest-power-of-2 1)
;; 1 (#o1, #x1, ?\C-a)
;; ELISP> (highest-power-of-2 0)
;; *** Eval error ***  Argument must be greater than or equal to 1.
;; ELISP> (highest-power-of-2 127)
;; 64 (#o100, #x40, ?@)

(provide 'allergies)
;;; allergies.el ends here
