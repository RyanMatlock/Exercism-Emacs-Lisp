;;; perfect-numbers.el --- perfect-numbers Exercise (exercism)  -*- lexical-binding: t; -*-

;;; Commentary:

(defun factors-v1 (n)
  "Return the factors of nonzero whole number N excluding N; error if N doesn't
satisfy the numerical requirements."
  (defun factorp (n maybe-factor)
    (let ((result (mod n maybe-factor)))
      ;; (print (format (concat "n: %d\t"
      ;;                        "maybe-factor: %d\t"
      ;;                        "(mod n maybe-factor): %d")
      ;;                n maybe-factor result))
      (zerop result)))
  (let* ((stop (max 1 (/ n 2)))
         (possible-factors (number-sequence 1 stop)))
    (seq-filter #'(lambda (x) (and (factorp n x)
                                   (not (= n x))))
                possible-factors)))

;; (defun factors (n)
;;   "Return the factors of nonzero whole number N excluding N; error if N doesn't
;; satisfy the numerical requirements."
;;   (defun factorp (num factor)
;;     (zerop (mod num factor)))
;;   (defun factors-helper (possible-factors acc)
;;     (let ((pf (car possible-factors)))
;;       (cond ((and pf (factorp n pf))
;;              (let ((other-factor (/ n pf)))
;;                (factors-helper (remove other-factor (cdr possible-factors))
;;                                (cons pf (cons other-factor (acc))))))
;;             (pf (factors-heper (cdr possible-factors) acc))
;;             (t acc))))
;;   ;; (defun factors-helper (possible-factors index)
;;   ;;   (if (< index (length possible-factors))
;;   ;;       (let ((possible-factor (elt possible-factors index)))
;;   ;;         (if (factorp n possible-factor)
;;   ;;             (factors-helper possible-factors (1+ index))
;;   ;;           ;; (print (format (concat "possible-factors %s\n"
;;   ;;           ;;                        "possible-factor: %d\n"
;;   ;;           ;;                        "index: %d")
;;   ;;           ;;                possible-factors
;;   ;;           ;;                possible-factor
;;   ;;           ;;                index))
;;   ;;           (factors-helper (seq-filter
;;   ;;                            #'(lambda (x)
;;   ;;                                (not (factorp x possible-factor)))
;;   ;;                            possible-factors)
;;   ;;                           index)))
;;   ;;     possible-factors))
;;   (cond ((<= n 1) '())
;;         (t (let* ((stop (max 1 (/ n 2)))
;;                   (possible-factors (number-sequence 1 stop)))
;;              (factors-helper possible-factors '())))))

(defun factors-v2 (n)
  "Return the factors of nonzero whole number N excluding N."
  (defun factorp (factor)
    (zerop (mod n factor)))
  ;; (let ((stop (/ n 2))))
  (defun factors-helper (maybe-factor factors)
    (let ((stop (car (last factors))))
      (cond ((and (< maybe-factor stop)
                  (factorp maybe-factor))
             (let* ((other-factor (/ n maybe-factor))
                    (new-factors (list maybe-factor other-factor)))
               (factors-helper (1+ maybe-factor) (append factors
                                                         new-factors))))
            ((< maybe-factor stop)
             (factors-helper (1+ maybe-factor) factors))
            (t (butlast (sort factors #'<))))))
  (cond ((= n 1) '())
        (t (factors-helper 2 (list 1 n)))))

(defun factors-v3 (n)
  "Return the factors of nonzero whole number N excluding N; error if N doesn't
satisfy the numerical requirements."

  (defun factorp (n maybe-factor)
    (zerop (mod n maybe-factor)))

  (defun factors-helper (possible-factors acc)
    (let ((pf (car possible-factors)))
      (cond ((and pf (factorp n pf))
             (factors-helper (cdr possible-factors) (cons pf acc)))
            (pf
             (factors-helper (seq-filter #'(lambda (x) (not (factorp x pf)))
                                         possible-factors)
                             acc))
            (t (reverse acc)))))
  (let* ((stop (max 1 (/ n 2)))
         (possible-factors (number-sequence 1 stop)))
    (factors-helper possible-factors '())))

(defun factors (n)
  "Return the factors of nonzero whole number N excluding N."

  (defun factorp (maybe-factor)
    (zerop (mod n maybe-factor)))

  (cond ((> n 1)
         (let ((stop (floor (sqrt n)))
               (maybe-factor 2)
               (factors (list 1)))
           (while (<= maybe-factor stop)
             ;; this is all very side effect-y
             (cond ((and (factorp maybe-factor)
                         ;; check if (= maybe-factor (sqrt n))
                         (= n (* maybe-factor maybe-factor)))
                    (push maybe-factor factors))
                   ((factorp maybe-factor)
                    (push maybe-factor factors)
                    (push (/ n maybe-factor) factors)))
             ;; as is this
             (setq maybe-factor (1+ maybe-factor)))
           (sort factors #'<)))
        (t '())))

(defun aliquot-sum (n)
  "Return the sum of the factors of N excluding N itself."
  (apply #'+ (factors n)))

(defun classify (number)
  "Classify NUMBER as 'PERFECT if it equal to its aliquot sum; 'ABUNDANT if it
is less than its aliquot sum, and 'DEFICIENT if it is greater than its aliquot
sum."
  (cond ((and (wholenump number)
              (not (zerop number)))
         (let ((aliquot (aliquot-sum number)))
           (cond ((= number aliquot) 'perfect)
                 ((< number aliquot) 'abundant)
                 ((> number aliquot) 'deficient)
                 (t (error "Not sure what happened here.")))))
        (t (error "Classification is only possible for natural numbers"))))

(provide 'perfect-numbers)
;;; perfect-numbers.el ends here
