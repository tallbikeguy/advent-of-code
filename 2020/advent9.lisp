(defpackage :advent9-package
  (:use :cl))

(ql:quickload "uiop")
(ql:quickload :cl-ppcre)
(ql:quickload :lisp-unit)
(ql:quickload :uiop)
(ql:quickload :alexandria)

(in-package :advent9-package)

(defun read-file-as-lines (filename)
  (with-open-file (in filename)
    (loop for line = (read-line in nil nil)
          while line
          collect (parse-integer line))))

(defparameter *allrecords* (read-file-as-lines "~/Downloads/lisp/input9.txt"))

(defparameter *preamble-size* 25)

(defun checksum (index value)
  (let (
        (retval 'nil)
        )
    (loop for j from (- index *preamble-size*) to (1- index) do
      (loop for k from (1+ j) to (1- index) do
        (if (= value (+ (nth j *allrecords*)(nth k *allrecords*)))
            (setf retval 'T))))
    retval))
                              
(defun sumrange (b e)
  (let ((loopsum 0))
    (loop for i from b to e do
      (setf loopsum (+ loopsum (nth i *allrecords*))))
    loopsum))

(defun maxrange (b e)
  (let ((retval (nth b *allrecords*)))
    (loop for i from b to e do
      (if (< retval (nth i *allrecords*))
          (setf retval (nth i *allrecords*))))
    retval))

(defun minrange (b e)
  (let ((retval (nth b *allrecords*)))
    (loop for i from b to e do
      (if (> retval (nth i *allrecords*))
          (setf retval (nth i *allrecords*))))
    retval))

(defun attack-step1 ()
  (loop named outer for i from *preamble-size* to (1- (length *allrecords*)) do
    (if (not(checksum i (nth i *allrecords*)))
        (return-from outer (nth i *allrecords*))
        )))

(defun attack-step2 ()
  (let (
        (targetval (attack-step1))
        )
    (write (format 'nil "looking for value of ~A" targetval))
    (loop named outer for i from 0 to (1- (length *allrecords*)) do
      (loop for j from 1 to (1- (length *allrecords*)) do
        (if (= (sumrange i j) targetval)
            (return-from outer ((+ (maxrange i j) (minrange i j)))))))))

;;advent9 part 1
(attack-step1)
;;advent9 part 2
(attack-step2)

