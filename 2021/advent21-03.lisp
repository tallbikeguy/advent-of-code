(defpackage :advent21-03
  (:use :cl))

(ql:quickload :cl-ppcre)
(ql:quickload :lisp-unit)
(ql:quickload :uiop)
(ql:quickload :alexandria)

(in-package :advent21-03)

(defparameter +report+ 
  (mapcar (lambda (x) (coerce x 'simple-vector))   (uiop:read-file-lines "inputs/input21-03.txt")))

(defun vec-bit-count (v)
  (length (car v)))

(defun getbit (n vec)
    (if (char= (svref vec n) #\1) 1 0))

(defun getbitcount (n vecs)
  (let* ((nbits (mapcar (lambda (x) (svref x n)) vecs))
         (nz (count #\0 nbits))
         (no (count #\1 nbits)))
    (list nz no)))

(defun bit-at (n vec)
  (if (svref vec n) 1 0))

(defun getmostcommonbit (n vecs)
  (if (<= (car (getbitcount  n vecs)) (car (cdr (getbitcount n vecs)))) 1 0))

(defun getleastcommonbit (n vecs)
  (if (<= (car (getbitcount  n vecs)) (car (cdr (getbitcount n vecs)))) 0 1))

(defun mostcommonbits (v)
  (coerce (loop for i from 0 to (1- (vec-bit-count v))
                collect (getmostcommonbit i v)) 'simple-vector))

(defun leastcommonbits (v)
  (coerce (loop for i from 0 to (1- (vec-bit-count v))
                collect (getleastcommonbit i v)) 'simple-vector))

(defun getgamma (v)
  (loop for i from 0 to (1- (vec-bit-count v))
        with result = 0
        with mcb = (mostcommonbits v)
        do (setf result (+ (* result 2) (svref mcb i)))
        finally
           (return result)))

(defun getepsilon (v)
  (loop for i from 0 to (1- (vec-bit-count v))
        with result = 0
        with lcb = (leastcommonbits v)
        do (setf result (+ (* result 2) (svref lcb i)))
        finally
           (return result)))

(defun get-power-consumption (v)
  (* (getgamma v) (getepsilon v)))

;;this is the answer to part 1
(get-power-consumption +report+)

(defun bin-to-int (vec)
  (loop for curpos from 0 to (1- (length vec))
        with mysum = 0
        do (setf mysum (+ (* mysum 2) (if (char= #\0 (svref vec curpos)) 0 1)))
        finally (return mysum)))

(defun ogr (invec)
  (let ((myvec invec))
  (loop for curbit from 0 to (1- (vec-bit-count myvec))
        with gmc = 0
        do (progn
             (setf gmc (getmostcommonbit curbit myvec) )
             (setf myvec (remove-if (lambda (p) (/= (getbit curbit p) gmc)) myvec))
             )
        until (= 1 (length myvec))
      finally (return (bin-to-int (car myvec)) ))))

(defun csr (invec)
  (let ((myvec invec))
  (loop for curbit from 0 to (1- (vec-bit-count myvec))
        with gmc = 0
        do (progn
             (setf gmc (getleastcommonbit curbit myvec) )
             (setf myvec (remove-if (lambda (p) (/= (getbit curbit p) gmc)) myvec))
             )
        until (= 1 (length myvec))
      finally (return (bin-to-int (car myvec))) )))

(defun lsr (invec)
  (* (ogr invec) (csr invec)))

;;answer to part 2
(lsr +report+)
