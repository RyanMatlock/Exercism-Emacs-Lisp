;;; gigasecond.el --- Gigasecond exercise (exercism)  -*- lexical-binding: t; -*-

;;; Commentary:
;; Calculate the date one gigasecond (10^9 seconds) from the
;; given date.
;;
;; NB: Pay attention to  Emacs' handling of time zones and dst
;; in the encode-time and decode-time functions.

(defun from (second minute hour day month year)
  "Add 10^9 seconds to the timestamp specified by SECOND, MINUTE, HOUR, DAY,
MONTH, and YEAR."
  ;; (defun --take (n seq)
  ;;   "Exercism says take is a void function for some reason"
  ;;   (let* ((ts (make-list n t))
  ;;          (nils (make-list (- (length seq) n) nil))
  ;;          (ts-nils (append ts nils))
  ;;          (seq-alist (seq-mapn #'(lambda (x bool) (cons x bool))
  ;;                               seq
  ;;                               ts-nils)))
  ;;     (mapcar #'car (seq-filter
  ;;                    #'(lambda (alist-elem) (cdr alist-elem)) seq-alist))))
  ;; (let* ((args (list second minute hour day month year))
  ;;        (t-init (list second minute hour day month year))
  ;;        (giga (expt 10 9)))
  ;;   (--take (length args) (decoded-time-add t-init (list giga))))
  ()
  )

(provide 'gigasecond)
;;; gigasecond.el ends here
