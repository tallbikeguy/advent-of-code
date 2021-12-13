(ql:quickload 'cl-ppcre)
(ql:quickload :uiop)

(defpackage :advent21-13 (:use :cl :cl-ppcre))
(in-package :advent21-13)

(defparameter +test-points+ '(
"6,10"
"0,14"
"9,10"
"0,3"
"10,4"
"4,11"
"6,0"
"6,12"
"4,1"
"0,13"
"10,12"
"3,4"
"3,0"
"8,4"
"1,10"
"2,14"
"8,10"
"9,0"))

(defparameter +test-folds+ '(
"fold along y=7"
"fold along x=5"
))
                              

(defparameter *input* 'nil)

(defun parse-line (str)
  (let ((tmpstr (cl-ppcre:split "\," str) ))
    (list (parse-integer (car tmpstr)) (parse-integer (car (cdr tmpstr))))))

(defparameter +points+   (mapcar #'parse-line +test-points+))

(defun parse-fold (str)
  (let* ((tmpstr (cl-ppcre:split "\=" str) )
         (foldstr (cl-ppcre:split "\ " (car tmpstr))))
    (list (nth 2 foldstr) (parse-integer (car (cdr tmpstr))))))

(defparameter +folds+ (mapcar #'parse-fold +test-folds+)) 

(defun foldpoint (p dir amt)
  (if (string= dir "x")
      (if (> (car p) amt )
          (list (- amt (- (car p) amt)) (cadr p))
          p)
      (if (> (cadr p) amt )
          (list (car p) (- amt (- (cadr p) amt)))
          p)))

(defun do-fold (points fold)
  (remove-duplicates (mapcar (lambda (p) (foldpoint p (car fold) (cadr fold))) points)  :test #'point=))

(defun point= (a b) (if (and (= (car a) (car b)) (= (cadr a) (cadr b))) 'T 'nil))

(defun do-folds ()
  (let ((cur-points +points+))
    (loop for i in +folds+
          :do (setf cur-points (do-fold cur-points i)))
    cur-points))
          
(defparameter +fold-results+ (do-folds))

(loop for i in +fold-results+
      :maximizing (car i) into xmax)

(defun result-dims ()
  (let ((xmax 0)
        (ymax 0))
    (loop for i in +fold-results+
          :maximizing (car i) into xmax
          :maximizing (cadr i) into ymax
          :finally (return (list xmax ymax)))))

(defun print-results ()
  (let* ((dims (result-dims))
         (arr (make-array (list (1+ (car dims)) (1+ (cadr dims))) :element-type 'character :initial-element #\Space  )))
    (loop for i in +fold-results+
          :do (setf (aref arr (car i) (cadr i)) #\# ))
    (loop for y from 0 :to (cadr dims)
          :do (loop for x :from 0 :to (car dims)
                    :do (format t "~c" (aref arr x y)))
          :do (terpri)
              )))
                        

(print-results)
    

