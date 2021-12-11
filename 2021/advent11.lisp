(ql:quickload 'alexandria)
(defpackage :advent21-11 (:use :cl :alexandria))
(in-package :advent21-11)

(defparameter +test-map+
  #2A(
(5 4 8 3 1 4 3 2 2 3 )
(2 7 4 5 8 5 4 7 1 1 )
(5 2 6 4 5 5 6 1 7 3 )
(6 1 4 1 3 3 6 1 4 6 )
(6 3 5 7 3 8 5 4 7 8 )
(4 1 6 7 5 2 4 6 4 5 )
(2 1 7 6 8 4 1 7 2 1 )
(6 8 8 2 8 8 1 1 3 4 )
(4 8 4 6 8 4 8 5 5 4 )
(5 2 8 3 7 5 1 5 2 6 )
))

(defparameter +map+ (alexandria:copy-array +test-map+))

(defun reset-test-map ()
  (setf +map+ (alexandria:copy-array +test-map+)))
  
(defun getpoint (a x y)
  (if (array-in-bounds-p a x y)
      (aref a x y)
    'nil))

(defun getpointc (a c)
  (if (array-in-bounds-p a (car c) (cdr c))
      (aref a (car c) (cdr c))
    'nil))

(defun setpoint (a x y v)
  (if (array-in-bounds-p a x y)
      (setf (aref a x y) v)
    'nil))

(defun setpointc (a c v)
  (if (array-in-bounds-p a (car c) (cdr c))
      (setf (aref a (car c) (cdr c)) v)
    'nil))

(defun  incpt (a i j)
  (if (array-in-bounds-p a i j)
      (setpoint a i j (1+ (getpoint a i j)))))

(defun apply-to-array (fn map)
  (loop :for i :from 0 :below (array-dimension map 0)
        collect (loop :for j from 0 :below (array-dimension map 1)
                      collect (funcall fn map i j))))

(defparameter *modlist* '((-1 . 0) (1 . 0) (0 . -1) (0 . 1) (-1 . -1) (1 . -1) (-1 . 1) (1 . 1)))

(defparameter *total-flashes* 0)
(defparameter *round-count* 0)

(defun  do-flash (i j)
  (progn
    (setf *total-flashes* (1+ *total-flashes*))
    (mapcar (lambda (m) (incpt +map+ (+ i (car m)) (+ j (cdr m)))) *modlist*)
    (cons i j))
  )

(defun flash-round ()
  (let ((flash-list 'nil ))
    (setf *round-count* (1+ *round-count*))
    (apply-to-array #'incpt +map+)
    (loop :while (> (length (flatten (apply-to-array (lambda (m i j) 
                                                       (if (and (> (getpoint m i j) 9)
                                                                (not (find (cons i j) flash-list :test #'equal))
                                                                )
                                                           (if (push (do-flash i j) flash-list)
                                                               1)
                                                           'nil))
                                                     +map+))) 0))
    (mapcar (lambda (pt) (setpointc +map+ pt 0)) flash-list)
    (length flash-list)))

(defun advent-11-1 ()
(progn
  (setf *round-count* 0)
  (setf *total-flashes* 0)
  (reset-test-map)
  (dotimes (i 100) (flash-round))
  *total-flashes*
))

(defun advent-11-2 ()
(progn
  (setf *round-count* 0)
  (setf *total-flashes* 0)
  (reset-test-map)
  (loop while (/= (reduce #'* (array-dimensions +map+)) (flash-round)))
  *round-count*
  ))

(cons (advent-11-1) (advent-11-2))

