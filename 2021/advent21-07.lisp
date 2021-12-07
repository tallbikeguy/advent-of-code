(ql:quickload 'april)
(ql:quickload 'iterate)
(ql:quickload 'series)
(ql:quickload 'cl-ppcre)
(require 'iterate)
(defpackage :advent21-07 (:use :cl :uiop :april :iterate))
(in-package :advent21-07)

(defparameter  *data* (mapcar #'parse-integer 
                            (cl-ppcre:split "," (with-open-file (s "input") 
                                                  (read-line s)))))
(defun minimum (list) (loop for element in list minimizing element))
(defun maximum (list) (loop for element in list maximizing element))

(defparameter *dist-data* 
  (loop for i from (minimum *data*) to (maximum *data*)
        collect (cons i (loop for element in *data* sum (abs (- element i))) )))

;; this is part 1
(iterate:iterate (for el in *dist-data*)
           (finding el minimizing (cdr el)))

(defun calc-dist (e i)
  (loop for i from 1 to (abs (- e i)) sum i))

(defparameter *dist-data-2* 
  (loop for i from (minimum *data*) to (maximum *data*)
        collect (cons i (loop for element in *data* sum (calc-dist element i)))))

;;this is part 2
(iterate:iterate (for el in *dist-data-2*)
           (finding el minimizing (cdr el)))


