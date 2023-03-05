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
;;; Code:
  )

(defun allergic-to-p (score allergen)
"Check if Allergic to allergen based on SCORE and ALLERGEN."
;;; Code:
)

(provide 'allergies)
;;; allergies.el ends here
