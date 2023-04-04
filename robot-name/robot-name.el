;;; robot-name.el --- Robot Name (exercism)  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Build a robot with a name like AA000, that can be reset
;; to a new name. Instructions are in README.md
;;

(defun random-element (xs)
  "Return a random element from list XS; error if XS is not a list."
  (cond ((listp xs) (nth (random (length xs)) xs))
        (t (error "XS must be a list."))))

(defun build-robot ()
  "Build a new robot with a random name."
  (let* ((letters (number-sequence ?A ?Z))
         (letter-limit (1+ (length letters)))
         (digits (number-sequence ?0 ?9))
         (digit-limit (1+ (length digits))))
    (random t)
    (concat
     ;; note: not using the values of the list mapcar is applied to
     (mapcar #'(lambda (-) (nth (random letter-limit) letters))
             '(t t))
     (mapcar #'(lambda (-) (nth (random digit-limit) digits))
             '(t t t)))))

(defun robot-name (robot)
  "Get the ROBOT's name."
  (print robot))

(defun reset-robot (robot)
  "Reset the name of ROBOT.  Factory reset!"
  (setq robot (build-robot)))

(provide 'robot-name)
;;; robot-name.el ends here
