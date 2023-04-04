;;; robot-name.el --- Robot Name (exercism)  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Build a robot with a name like AA000, that can be reset
;; to a new name. Instructions are in README.md
;;

(defun random-element (xs)
  "Return a random element from list XS; error if XS is not a list."
  (cond ((sequencep xs)
         (progn
           (random t)
           (nth (random (length xs)) (mapcar #'identity xs))))
        (t (error "XS must be a sequence."))))

(defun generate-robot-name ()
  "Generate a random robot name of the form [A-Z]\\{2\\}[0-9]\\{3\\}."
  (let* ((letters (number-sequence ?A ?Z))
         (digits (number-sequence ?0 ?9))
         (letter-prefix
          ;; note: not using the values of the list mapcar is applied to, so
          ;; using the dash like the underscore as a variable name in Python
          (mapcar #'(lambda (-) (random-element letters)) '(1 2)))
         (numbers
          (mapcar #'(lambda (-) (random-element digits) ) '(1 2 3))))
    (concat letter-prefix numbers)))

(defun build-robot ()
  "Build a new robot with a random name."
  (list (cons :name (generate-robot-name))))

(defun robot-name (robot)
  "Get the ROBOT's name."
  (cdr (assq :name robot)))

(defun reset-robot (robot)
  "Reset the name of ROBOT.  Factory reset!"
  (setcdr (assq :name robot) (generate-robot-name)))

(provide 'robot-name)
;;; robot-name.el ends here
