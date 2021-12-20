(ql:quickload 'cl-ppcre)
(defpackage :advent21-20 (:use :cl :uiop :cl-ppcre))
(in-package :advent21-20)

(defparameter *img-alg* 'nil)

(defun c2p (v)
  (if ( integerp v) v
      (if (char= v #\#) 1 0)))

(defclass image ()
  ((points :initarg :points :accessor points)
   (inf-value :initarg :inf-value :accessor inf-value)
   ))

(defun make-image ()
  (make-instance 'image :points (make-hash-table) :inf-value 0))

(defun image-from-input (points infval)
  (let ((img (make-image)))
    (setf (slot-value img 'points) points)
    (setf (slot-value img 'inf-value) infval)
    img))

(defun read-input (filename)
  (let* (
         (allinput (ppcre:split "\\n\\n" (uiop:read-file-string filename)))
         (points (make-hash-table))
         (splitimg (ppcre:split "\\n" (cadr  allinput)))
         )
    (defparameter *img-alg* (car allinput))
    (loop for x from 0 below (length (car splitimg))
          :do (loop for y from 0 below (length splitimg)
                    :do (setf (gethash (complex x y) points) (c2p (char (nth y splitimg) x)))))
    (image-from-input points 0))
  )

(defun getpixelc (img c)
  (multiple-value-bind (value there) (gethash c (points img))
    (if there
        (c2p value)
        (inf-value img))))

(defun getpixel (img x y) (getpixelc img (complex x y)))

(defun setpixelc (img c val)
  (setf (gethash c (points img )) val))

(defun setpixel (img x y val)
  (setpixelc img (complex x y) val)) 

(defun img-dimensions (imgobj)
  (loop for h being the hash-keys in (points imgobj) 
        minimizing (realpart h) into xmin
        minimizing (imagpart h) into ymin
        maximizing (realpart h) into xmax
        maximizing (imagpart h) into ymax
        finally (return (list xmin ymin xmax ymax))))

(defun use-imgalg (ptval)
  (c2p (char *img-alg* ptval)))

(defun calc-newpixel (pts x y)
  (loop
    for offset in '(#C(-1 -1) #C(0 -1) #C(1 -1) #C(-1 0) #C(0 0) #C(1 0) #C(-1 1) #C(0 1) #C(1 1))
    for current = (+ (complex x y) offset)
    with code = 0
    do (setf code (+ (ash code 1) (getpixelc pts current)))
    finally (return (use-imgalg code))))

(defun calc-infpixel (infval)
  (if (= infval 1) (use-imgalg 511) (use-imgalg 0)))

(defun apply-imgalg (imgobj)
  (let* ((newimg (make-image))
        (newpoints (points newimg))
        (imgdims (img-dimensions imgobj)))
    (loop for x from (- (nth 0 imgdims) 2) to (+ (nth 2 imgdims) 2)
          :do (loop for y from (- (nth 1 imgdims) 2) to (+ (nth 3 imgdims) 2)
                    :do ( setpixel newimg x y (calc-newpixel imgobj x y))))
    (setf (inf-value newimg) (calc-infpixel (inf-value imgobj)))
    newimg)
  )

(defun pixelprt (v)
  (if (integerp v)
      (if (= v 1) #\# #\.)
      v))

(defun print-image (imgobj &optional xmax ymax)
  (let* ((dims 'nil)
         (xm 0)
         (ym 0))
    (setf dims (img-dimensions imgobj))
    (if xmax (setf xm (min xmax (nth 2 dims))) (setf xm (nth 2 dims)))
    (if ymax (setf ym (min ymax (nth 3 dims))) (setf ym (nth 3 dims)))
    (format t "image infinity val=~a~%" (inf-value imgobj))
    (loop for y from (nth 1 dims) to ym
          :do (loop for x :from (nth 0 dims) to xm
                    :do (format t "~c" (pixelprt (getpixel imgobj x y)))
                      :finally (terpri))
          :finally (terpri)
              )))

(defun lit-pixels (imgobj)
  (let* ((dims (img-dimensions imgobj)))
    (loop for y from (nth 1 dims) to (nth 3 dims)
          with litpix = 0
          :do (loop for x :from (nth 0 dims) to (nth 2 dims)
                    :do ( incf litpix (getpixel imgobj x y)))
          :finally (return litpix))
              ))


(defparameter *iimg* (read-input  "input"))

(let ((myimg *iimg*))
  (dotimes (i 2)
    (setf myimg (apply-imgalg myimg)))
  (lit-pixels myimg))

(let ((myimg *iimg*))
  (dotimes (i 50)
    (setf myimg (apply-imgalg myimg)))
  (lit-pixels myimg))
