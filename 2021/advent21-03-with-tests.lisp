(defpackage :advent21-03
  (:use :cl))

(ql:quickload :cl-ppcre)
(ql:quickload :lisp-unit)
(ql:quickload :uiop)
(ql:quickload :alexandria)

(in-package :advent21-03)

(defparameter +small-test-report+ 
  (mapcar (lambda (x) (coerce x 'simple-vector)) '("10110" "10111")))

(defparameter +test-report+ 
  (mapcar (lambda (x) (coerce x 'simple-vector)) '("00100" "11110" "10110" "10111" "10101" "01111" "00111" "11100" "10000" "11001" "00010" "01010")))

(defparameter +report+ 
  (mapcar (lambda (x) (coerce x 'simple-vector))   (uiop:read-file-lines "inputs/input21-03.txt")))

(defun vec-bit-count (v)
  (length (car v)))

(lisp-unit:define-test test-list-size
  (lisp-unit:assert-equal 12  (length +test-report+))
  (lisp-unit:assert-equal 5  (vec-bit-count +test-report+)))

(lisp-unit:run-tests '(test-list-size))

(defun getbit (n vec)
    (if (char= (svref vec n) #\1) 1 0))

(defun getbitcount (n vecs)
  (let* ((nbits (mapcar (lambda (x) (svref x n)) vecs))
         (nz (count #\0 nbits))
         (no (count #\1 nbits)))
    (list nz no)))

(lisp-unit:define-test test-bit-count
  (lisp-unit:assert-equal 0 (getbit 0 (coerce '"00100" 'simple-vector)))
  (lisp-unit:assert-equal 0 (getbit 1 (coerce '"00100" 'simple-vector)))
  (lisp-unit:assert-equal 1 (getbit 2 (coerce '"00100" 'simple-vector)))
  (lisp-unit:assert-equal 0 (getbit 3 (coerce '"00100" 'simple-vector)))
  (lisp-unit:assert-equal 0 (getbit 4 (coerce '"00100" 'simple-vector)))
  (lisp-unit:assert-equal 5 (car (getbitcount 0 +test-report+)))
  (lisp-unit:assert-equal 7 (car (getbitcount 1 +test-report+))))

(lisp-unit:run-tests '(test-bit-count))

(defun bit-at (n vec)
  (if (svref vec n) 1 0))

(defun getmostcommonbit (n vecs)
  (if (<= (car (getbitcount  n vecs)) (car (cdr (getbitcount n vecs)))) 1 0))

(defun getleastcommonbit (n vecs)
  (if (<= (car (getbitcount  n vecs)) (car (cdr (getbitcount n vecs)))) 0 1))

(lisp-unit:define-test test-common-bit
  (lisp-unit:assert-equal 1 (getmostcommonbit 4 +small-test-report+))
  (lisp-unit:assert-equal 1 (getmostcommonbit 0 +test-report+))
  (lisp-unit:assert-equal 0 (getmostcommonbit 1 +test-report+))
  (lisp-unit:assert-equal 1 (getmostcommonbit 2 +test-report+))
  (lisp-unit:assert-equal 1 (getmostcommonbit 3 +test-report+))
  (lisp-unit:assert-equal 0 (getmostcommonbit 4 +test-report+))
  (lisp-unit:assert-equal 0 (getleastcommonbit 4 +small-test-report+))
  (lisp-unit:assert-equal 0 (getleastcommonbit 0 +test-report+))
  (lisp-unit:assert-equal 1 (getleastcommonbit 1 +test-report+))
  (lisp-unit:assert-equal 0 (getleastcommonbit 2 +test-report+))
  (lisp-unit:assert-equal 0 (getleastcommonbit 3 +test-report+))
  (lisp-unit:assert-equal 1 (getleastcommonbit 4 +test-report+))
  )

(lisp-unit:run-tests '(test-common-bit))

(defun mostcommonbits (v)
  (coerce (loop for i from 0 to (1- (vec-bit-count v))
                collect (getmostcommonbit i v)) 'simple-vector))

(defun leastcommonbits (v)
  (coerce (loop for i from 0 to (1- (vec-bit-count v))
                collect (getleastcommonbit i v)) 'simple-vector))

(defun equal-vector (v1 v2)
  (if (/= (length v1) (length v2))
      'nil
      (loop named loop1 for i from 0 to (1- (length v1))
            when (/= (svref v1 i) (svref v2 i)) do (return-from loop1 'nil)
              finally (return-from loop1 'T))))

(lisp-unit:define-test test-equal-vector
  (lisp-unit:assert-true (equal-vector #() #()))
  (lisp-unit:assert-false (equal-vector #(1 0 1 1 0) #(1 0 1 0)))
  (lisp-unit:assert-true (equal-vector #(1 0 1 1 0) #(1 0 1 1 0)))
  (lisp-unit:assert-false (equal-vector #(1 0 1 1 0) #(1 0 1 0 0)))
  )

(lisp-unit:run-tests '(test-equal-vector))

(lisp-unit:define-test test-mcb
  (lisp-unit:assert-equality #'equal-vector #(1 0 1 1 0) (mostcommonbits +test-report+))
  (lisp-unit:assert-equality #'equal-vector #(0 1 0 0 1) (leastcommonbits +test-report+))
  )

(lisp-unit:run-tests '(test-mcb))

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

(lisp-unit:define-test test-gamma
  (lisp-unit:assert-equal 22  (getgamma +test-report+))
  (lisp-unit:assert-equal 9  (getepsilon +test-report+))
  (lisp-unit:assert-equal 198  (get-power-consumption +test-report+))
  )

(lisp-unit:run-tests '(test-gamma))

;;this is the answer to part 1
(get-power-consumption +report+)

(defun bin-to-int (vec)
  (loop for curpos from 0 to (1- (length vec))
        with mysum = 0
        do (setf mysum (+ (* mysum 2) (if (char= #\0 (svref vec curpos)) 0 1)))
        finally (return mysum)))

(lisp-unit:define-test test-bin-to-int
  (lisp-unit:assert-equal 22  (bin-to-int #(#\1 #\0 #\1 #\1 #\0)))
  (lisp-unit:assert-equal 18  (bin-to-int #(#\1 #\0 #\0 #\1 #\0)))
  (lisp-unit:assert-equal 2  (bin-to-int #(#\0 #\0 #\0 #\1 #\0)))
  )

(lisp-unit:run-tests '(test-bin-to-int))

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

(lisp-unit:define-test test-ogr-csr
  (lisp-unit:assert-equal 23  (ogr +test-report+))
  (lisp-unit:assert-equal 10  (csr +test-report+))
  (lisp-unit:assert-equal 230  (lsr +test-report+))  
  )

(lisp-unit:run-tests '(test-ogr-csr))

(lisp-unit:run-tests :all)

;;answer to part 2
(lsr +report+)
