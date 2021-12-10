(ql:quickload 'iterate)
(ql:quickload 'queues)
(ql:quickload 'series)
(ql:quickload 'cl-ppcre)
(require 'iterate)
(require :queues.simple-queue)
    (defpackage :advent21-09 (:use :cl :uiop :iterate :queues))
(in-package :advent21-09)


(defparameter +test-map+
  #2A(
(2 1 9 9 9 4 3 2 1 0)
(3 9 8 7 8 9 4 9 2 1)
(9 8 5 6 7 8 9 8 9 2)
(8 7 6 7 8 9 6 7 8 9)
(9 8 9 9 9 6 5 6 7 8)
))

(defun getpoint (ary x y)
  (if (ARRAY-IN-BOUNDS-P ary x y)
      (aref ary x y)
  'nil))

(defun setpoint (ary x y val)
  (if (ARRAY-IN-BOUNDS-P ary x y)
      (setf (aref ary x y) val)
      'nil))

(defun fand (&optional a b)
  (and a b))

(defun lowpoint-p (ary x y)
  (if (ARRAY-IN-BOUNDS-P ary x y)
      (let ((xyval 'nil))
        (setf xyval (getpoint ary x y))
        (reduce #'fand (mapcar (lambda (p) (< xyval p)) (remove 'nil (list (getpoint ary x (1- y)) (getpoint ary (1- x)  y) (getpoint ary (1+ x) y) (getpoint ary x (1+ y)))))))
      (progn
        'X)))

(defun flatten (lst)
  (labels ((rflatten (lst1 acc)
             (dolist (el lst1)
               (if (listp el)
                   (setf acc (rflatten el acc))
                   (push el acc)))
             acc))
    (reverse (rflatten lst nil))))

;;part 1
(write (reduce #'+ (mapcar #'1+ (flatten (remove 'nil (loop :for i :from 0 :below (array-dimension +test-map+ 0)
      collect (remove 'nil (loop :for j from 0 :below (array-dimension +test-map+ 1)
                    collect (if (lowpoint-p +test-map+ i j) (aref +test-map+ i j))))))))))

(defparameter *basin* 'nil)
(defparameter *areas* 'nil)
(defparameter *procq* 'nil)

(defun mkpt (x y) (list x y))

(defun addmove (a x y)
  (progn
  (if (and (ARRAY-IN-BOUNDS-P a x y)
           (< (getpoint a x y) 9)
           (>= (getpoint a x y) 0))
      (progn
        (format t "addmove ~a:~a" x y)
        (terpri)
        (queues:qpush *procq* (mkpt x y))))))

(defun addmoves (i ary) 
  (let* ((bx (car i)) (by (cadr i)))
    (addmove ary (1- bx) by)
    (addmove ary (1+ bx) by)
    (addmove ary bx (1- by))
    (addmove ary bx (1+ by))
    (if (<= 0 (getpoint ary bx by))
        (progn
          (setpoint ary bx by -1)
          (setq *basin* (1+ *basin*))
          ))
    *basin*)
  )

(defun mark-basin (ary x y)
  (let ((i 'nil))
    (setq *procq* (make-queue :simple-queue ))
    (queues:qpush *procq* (mkpt x y))
    (setq *basin* 0)
    (loop :while (> (queues:qsize *procq*) 0 )
          :do (progn
                (setq i (qpop *procq*))
                (format t "mark-basin ~a" i)
                (addmoves i ary) ))
    (nconc *areas* (list  *basin*))))

(defparameter *basin* 0)
(defparameter *areas* '(0))
(defparameter *procq* 'nil)

;;part 1
 (remove 'nil (loop :for i :from 0 :below (array-dimension +test-map+ 0)
      collect (remove 'nil (loop :for j from 0 :below (array-dimension +test-map+ 1)
                    collect (if (lowpoint-p +test-map+ i j) (list i j))))))
;; part2
(loop :for i :from 0 :below (array-dimension +test-map+ 0)
      :do (loop :for j from 0 :below (array-dimension +test-map+ 1)
                    :do (if (lowpoint-p +test-map+ i j) (mark-basin +test-map+ i j))))
(defparameter *sareas* (sort *areas* #'>))
(* (car *sareas*)(cadr *sareas*)(caddr *sareas*))

