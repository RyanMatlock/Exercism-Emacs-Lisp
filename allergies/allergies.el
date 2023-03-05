;;; allergies.el --- Allergies Exercise (exercism)  -*- lexical-binding: t; -*-

;;; Commentary: The allergy number is written as
;;; A = \sum_{i=1}^n c_i \cdot 2^{i-1}, where c_i = 0 if one is not allergic to
;;; the i-th allergen and 1 if one is allergic to it. An important observation
;;; is that \sum_{i=0}^k 2^i < 2^{k+1}, so you can figure out the coefficients
;;; by successively subtracting the largest c_i ≤ A.

;; normally, I wouldn't want to pollute my namespace, but I think in this case
;; it makes sense
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
  (defun allergen-list-helper (remaining-score score-decomposition-list)
    (if (> remaining-score 0)
        (let ((allergen-num (highest-power-of-2 remaining-score)))
          (allergen-list-helper (- remaining-score allergen-num)
                                (cons allergen-num score-decomposition-list)))
      ;; actually, I don't need to reverse the list because I want to start
      ;; with eggs and end with cats
      ;; (reverse score-decomposition-list)
      score-decomposition-list))
  (defun scores-to-allergens (scores)
    "Transform a list of allergen numbers into a list of allergen strings."
    (defun scores-to-allergens-helper (scores allergen-list)
      (if scores
          (let* ((score (car scores))
                 (allergen (alist-get score allergen-lookup-alist)))
            ;; ensure allergen is defined
            (if allergen
                (scores-to-allergens-helper (cdr scores)
                                        (cons allergen allergen-list))
              ;; if allergen is undefined, keep on movin'
              (scores-to-allergens-helper (cdr scores) allergen-list)))
        (reverse allergen-list)))
    (scores-to-allergens-helper scores '()))
  (scores-to-allergens (allergen-list-helper score '())))

(defun allergic-to-p (score allergen)
  "Check if Allergic to allergen based on SCORE and ALLERGEN."
  (defun member-p (obj list)
    "Works like the member function but returns t if obj is in list instead of
obj."
    (if (member obj list)
        t
      nil))
  (member-p allergen (allergen-list score)))

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

;; ELISP> (allergen-list 34)
;; (32 2)
;; ELISP> (allergen-list 34)
;; (2 32)
;; ELISP> (allergen-list 203)
;; (1 2 8 64 128)

;; ELISP> (allergen-list 203)
;; ("eggs" "peanuts" "strawberries" "pollen" "cats")

;; ELISP> (allergen-list 257)
;; ("eggs" nil)

;; ELISP> (allergen-list 257)
;; ("eggs")
;; ELISP> (allergen-list 2257)
;; ("eggs" "tomatoes" "pollen" "cats")
;; ELISP> (allergen-list 0)
;; nil

;; ELISP> (member "cats" (allergen-list 2257))
;; ("cats")
;; ELISP> (member "dogs" (allergen-list 2257))
;; nil

;; ELISP> (allergic-to-p 2257 "cats")
;; t
;; ELISP> (allergic-to-p 2257 "dust")
;; nil

(provide 'allergies)
;;; allergies.el ends here
