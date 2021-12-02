(defpackage :advent10
  (:use :cl))

(ql:quickload :cl-ppcre)
(ql:quickload :lisp-unit)
(ql:quickload :uiop)
(ql:quickload :alexandria)

(in-package :advent10)

(defun get-plug-list ()
  (let ((pluglist (append '(0) (sort (mapcar #'parse-integer (uiop:read-file-lines "input10.txt") ) #'< ))))
    (append pluglist (list (+ 3 (apply #'max pluglist))))))

(defparameter +plugs+ (get-plug-list))

;;+plugs+

(defun list-difference (list)
  (loop for i from 0 to (- (length list) 2)
    collect (- (nth (1+ i) list) (nth i list))))

(defun docalc ()
  (let ((listd (list-difference +plugs+)))
    (list (length (remove '1 listd)) (length (remove '3 listd)))))

;;this is the answer for part 1
(apply #'* (docalc))
 
;;part 2
;;we need to find all paths from the first to the last, in steps of 1 or 3
+plugs+
(

(defun step-perms (start-list rem-list end)
  ;; given the start list and remainder list, find all paths to end

