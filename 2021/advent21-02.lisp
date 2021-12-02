(defpackage :advent21-02
  (:use :cl))

(ql:quickload :cl-ppcre)
(ql:quickload :lisp-unit)
(ql:quickload :uiop)
(ql:quickload :alexandria)

(in-package :advent21-02)

(defun parse-line (str)
  (let ((tmpstr (cl-ppcre:split "\ " str) ))
    (list (car tmpstr) (parse-integer (car (cdr tmpstr))))))

(defun get-input-list ()
  (mapcar #'parse-line (uiop:read-file-lines "inputs/input21-02.txt")))

(defparameter +steps+ (get-input-list))
(defparameter +steps-test+ '(("forward" 5) ("down" 5) ("forward" 8) ("up" 3) ("down" 8) ("forward" 2)))

(defparameter *hpos* 0)
(defparameter *dpos* 0)
(defparameter *aim* 0)

(defun move-val (move-list) (car (cdr move-list)))

(defun calc-move-part-1 (a)
  (cond 
    ((string= (car a) '"forward") (setq *hpos* (+ *hpos* (move-val a))))
    ((string= (car a) '"down")    (setq *dpos* (+ *dpos* (move-val a))))
    ((string= (car a) '"up")      (setq *dpos* (- *dpos* (move-val a))))
    )
  )

(defun calc-move-part-2 (a)
  (cond 
    ((string= (car a) '"forward") (progn
                                    (setq *hpos* (+ *hpos* (move-val a)))
                                    (setq *dpos* (+ *dpos* (* *aim* (move-val a))))))
     ((string= (car a) '"down")     (setq *aim*  (+ *aim*  (move-val a))))
     ((string= (car a) '"up")       (setq *aim*  (- *aim* (move-val a))))
    )
  )

(defun run-advent21-02 (move-func data-list)
  (progn
    (setq *dpos* 0)
    (setq *hpos* 0)
    (setq *aim* 0)
    (mapcar move-func data-list)
    (* *hpos* *dpos*)
    ))

;;test for the first part
(run-advent21-02 #'calc-move-part-1 +steps-test+)

;;this is the first part of the challenge
(run-advent21-02 #'calc-move-part-1 +steps+)

;;this is the test for the second part
(run-advent21-02 #'calc-move-part-2 +steps-test+)

;;this is the second part of the challenge
(run-advent21-02 #'calc-move-part-2 +steps+)


