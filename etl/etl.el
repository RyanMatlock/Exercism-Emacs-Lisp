;;; etl.el --- etl Exercise (exercism)  -*- lexical-binding: t; -*-

;;; Commentary: based on etl-test.el, it looks like I should be using a hash
;;; table instead of an alist, and the data will be presented in the form of a
;;; hash table. Note that the letters in the input are mixed case.

(defun etl (data)
  "Translate hash table data, where keys are points and the values are lists of
mixed case letters to a hash table where keys are lowercase letters and values
are points."
  (let ((newdata (make-hash-table :test #'equal)))
      (defun swap-kv (k vs)
        "Apply to data with maphash"
        ;; (print (format "k: %s\nvs: %s" k vs))
        ;; not sure if helper function is actually necessary
        (defun swap-kv-helper (k vs)
          (let ((v (car vs)))
            ;; it's weird writing impure code like this
            ;; (if v
            ;;     (progn
            ;;       ;; (print (format "%s: %d" v k))
            ;;       (puthash (downcase v) k newdata)
            ;;       (swap-kv-helper k (cdr vs)))
            ;;   nil)
            (cond ((and v (stringp v))
                   (progn (puthash (downcase v) k newdata)
                          (swap-kv-helper k (cdr vs))))
                  (v (error "New keys must be chars/strings."))
                  (t nil))
            ))
        (if (< k 0)
            (error "Keys should be non-negative.")
          (swap-kv-helper k vs)))
      (maphash #'swap-kv data)
      newdata))

;; -- IELM testing --
;; note: applied eval-region to (defconstant mixed-case-input ...) in
;; etl-test.el

;; ELISP> (etl mixed-case-input)

;; "k: 1
;; vs: (a E I o U L N r s T)"

;; "k: 2
;; vs: (D G)"

;; "k: 3
;; vs: (B c M P)"

;; "k: 4
;; vs: (f h V W y)"

;; "k: 5
;; vs: (K)"

;; "k: 8
;; vs: (J x)"

;; "k: 10
;; vs: (q z)"

;; #<hash-table equal 0/65 0x1fe46e761755>

;; ELISP> (etl mixed-case-input)

;; "k: 1
;; vs: (a E I o U L N r s T)"

;; "a: 1"

;; *** Eval error ***  Wrong type argument: characterp, "a"
;; ELISP> (downcase (string "A"))
;; *** Eval error ***  Wrong type argument: characterp, "A"

;; ELISP> (etl mixed-case-input)

;; "k: 1
;; vs: (a E I o U L N r s T)"

;; "a: 1"

;; "E: 1"

;; "I: 1"

;; "o: 1"

;; "U: 1"

;; "L: 1"

;; "N: 1"

;; "r: 1"

;; "s: 1"

;; "T: 1"

;; "k: 2
;; vs: (D G)"

;; "D: 2"

;; "G: 2"

;; "k: 3
;; vs: (B c M P)"

;; "B: 3"

;; "c: 3"

;; "M: 3"

;; "P: 3"

;; "k: 4
;; vs: (f h V W y)"

;; "f: 4"

;; "h: 4"

;; "V: 4"

;; "W: 4"

;; "y: 4"

;; "k: 5
;; vs: (K)"

;; "K: 5"

;; "k: 8
;; vs: (J x)"

;; "J: 8"

;; "x: 8"

;; "k: 10
;; vs: (q z)"

;; "q: 10"

;; "z: 10"

;; #<hash-table equal 26/65 0x1fe452d48841>
;; this looks promising

;; not working; I guess negative keys in data should throw an error

;; ELISP> (char-or-string-p ?N)
;; t

;; ELISP> ?N
;; 78 (#o116, #x4e, ?N)
;; ELISP> (int "N")
;; *** Eval error ***  Symbol’s function definition is void: int
;; ELISP> (string-to-number "N")
;; 0 (#o0, #x0, ?\C-@)
;; ELISP> (number-to-string ?N)
;; "78"
;; ELISP> (string ?N)
;; "N"
;; ELISP> (stringp ?N)
;; nil

(provide 'etl)
;;; etl.el ends here
