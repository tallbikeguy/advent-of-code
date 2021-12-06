(defpackage :advent21-06 (:use :cl ))

(in-package :advent21-06)

(defun fish-list-counts (c l) 
  (loop for f in l do (incf (svref c f))))
  
(defun rotate-vec-newval (vec newval)
  ;;rotate vector left, but shift newval onto right most
  (progn
    (loop for i from 1 to (1- (length vec))
          do (setf (svref vec (1- i)) (svref vec i)))
    (setf (svref vec (1- (length vec))) newval)
    vec))

(defun fish-day (f)
  (let ((newfish (svref f 0)))
    (rotate-vec-newval f newfish)
    (setf (svref f 6) (+ (svref f 6) newfish))
    f))

(defun total-fish (v)
  (loop for i from 0 to (1- (length v))
        sum (svref v i)))

(defun do-days (v num-days)
  (progn
    (loop for i from 1 to num-days
          do (fish-day v)))
    (total-fish v))

(defparameter *fish-test* '(3 4 3 1 2))
(defparameter  *fish-test-vec* (vector 0 0 0 0 0 0 0 0 0 ))

;;this is the test
(fish-list-counts *fish-test-vec* *fish-test*)

(do-days *fish-test-vec* 256)


(defparameter *fish* '(4 1 1 4 1 2 1 4 1 3 4 4 1 5 5 1 3 1 1 1 4 4 3 1 5 3 1 2 5 1 1 5 1 1 4 1 1 1 1 2 1 5 3 4 4 1 1 1 1 1 1 1 1 1 2 1 1 1 1 1 5 1 1 1 4 1 2 3 5 1 2 2 4 1 4 4 4 1 2 5 1 2 1 1 1 1 1 1 4 1 1 4 3 4 2 1 3 1 1 1 3 5 5 4 3 4 1 5 1 1 1 2 2 1 3 1 2 4 1 1 3 3 1 3 3 1 1 3 1 5 1 1 3 1 1 1 5 4 1 1 1 1 4 1 1 3 5 4 3 1 1 5 4 1 1 2 5 4 2 1 4 1 1 1 1 3 1 1 1 1 4 1 1 1 1 2 4 1 1 1 1 3 1 1 5 1 1 1 1 1 1 4 2 1 3 1 1 1 2 4 2 3 1 4 1 2 1 4 2 1 4 4 1 5 1 1 4 4 1 2 2 1 1 1 1 1 1 1 1 1 1 1 4 5 4 1 3 1 3 1 1 1 5 3 5 5 2 2 1 4 1 4 2 1 4 1 2 1 1 2 1 1 5 4 2 1 1 1 2 4 1 1 1 1 2 1 1 5 1 1 2 2 5 1 1 1 1 1 2 4 2 3 1 2 1 5 4 5 1 4))
(defparameter  *fish-vec* (vector 0 0 0 0 0 0 0 0 0 ))
(fish-list-counts *fish-vec* *fish*)

(do-days *fish-vec* 256)
;;;;;all done!
