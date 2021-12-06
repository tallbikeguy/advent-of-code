(ql:quickload :cl-ppcre)
(ql:quickload :lisp-unit)
(ql:quickload :uiop)

(defpackage :advent21-05 (:use :cl :cl-ppcre :uiop :lisp-unit))

(in-package :advent21-05)

(defun group<n (l n) 'nil)

(defun group<n-sub ( l m n )
  (if (and (cdr l) (< 0 n))
      (group<n-sub (cons (cons (cadr l) (car l)) (cddr l)) m (1- n))
      (cons (reverse (car l)) (group<n (cdr l) m))
      )
  )

(defun group<n ( l n )
  (if l (group<n-sub (cons nil l) n n))
  )

(defun flatten (lst)
  (labels ((rflatten (lst1 acc)
             (dolist (el lst1)
               (if (listp el)
                   (setf acc (rflatten el acc))
                   (push el acc)))
             acc))
    (reverse (rflatten lst nil))))

(defparameter *matchstring* "([0-9]+),([[0-9]+) -> ([0-9]+),([0-9]+)")

(defun parse-line (line)
  (let* (
         (astr nil)
         (bstr nil)
         )
    (setf (values astr bstr)
          (cl-ppcre:scan-to-strings *matchstring* line))
    (group<n (mapcar #'parse-integer (coerce bstr 'list)) 2)))

(defun pt-x (pt) (car pt))
(defun pt-y (pt) (car (cdr pt)))

(defun points-x (ptlist) (mapcar #'pt-x ptlist))
(defun points-y (ptlist) (mapcar #'pt-y ptlist))
(defun line-p1 (line) (car line))
(defun line-p2 (line) (car (cdr line)))
(defun line-x1 (line) (pt-x (line-p1 line)))
(defun line-x2 (line) (pt-x (line-p2 line)))
(defun line-y1 (line) (pt-y (line-p1 line)))
(defun line-y2 (line) (pt-y (line-p2 line)))
(defun line-xlist (line) (list (line-x1 line) (line-x2 line)))
(defun line-ylist (line) (list (line-y1 line) (line-y2 line)))
(defun makepoint (x y) (list x y))
(defun lines-p1 (linelist) (mapcar #'line-p1 linelist))
(defun lines-p2 (linelist) (mapcar #'line-p2 linelist))
(defun lines-points (linelist) (append (lines-p1 linelist) (lines-p2 linelist)))
(defun maximum (list)  (loop for element in list maximizing element))
(defun minimum (list) (loop for element in list minimizing element))
(defun max-x (linelist) (maximum (points-x (lines-points linelist))))
(defun max-y (linelist) (maximum (points-y (lines-points linelist))))

(defun horiz-p (line)
  (= (pt-y (line-p1 line)) (pt-y (line-p2 line))))

(defun vert-p (line)
  (= (pt-x (line-p1 line)) (pt-x (line-p2 line))))

(defun angular-p (line)
  (not (or (horiz-p line) (vert-p line))))

(defun /angular-p (line)
  (or (horiz-p line) (vert-p line)))

(defun gen-series (start end)
  ;; generate a list with integers from start to end
  ( if (< start end)
       (loop for x from start to end
	     collect x)
       (loop for x from start downto end
	     collect x)))

(defun line-points (line)
  ;; create a list of all points in a line
  ;; assumes horizontal, vertical or 45 degree angle
  (if (horiz-p line)
      (loop for i from (minimum (line-xlist line)) to (maximum (line-xlist line))
	    collect (makepoint i (line-y1 line) ))
      (if (vert-p line)
	  (loop for i from (minimum (line-ylist line)) to (maximum (line-ylist line))
		collect (makepoint (line-x1 line) i))
	  (loop for x in (gen-series (line-x1 line) (line-x2 line))
	        for y in (gen-series (line-y1 line) (line-y2 line))
		collect (makepoint x y)))))

(defun score-map (the-map)
  (loop for b being each hash-value of the-map
        counting (>= b 2)))

;;process all the lines
(defun mark-map (map lines)
    (mapcar (lambda (l)
	      (mapcar (lambda (p)
			(incf (gethash p map 0)))
		      (line-points l)))
	    lines)
    map)

(defun mark-map-one (map line)
  (mapcar (lambda (p) (incf (gethash p map 0)))
		      (line-points line)))

(defun get-file (filename)
  (score-map (let ((mymap (make-hash-table :test 'equal)))  
               (with-open-file (stream filename)
                 (loop for line = (read-line stream nil)
                       while line
                       do (mark-map-one mymap (parse-line line))))
               mymap
               )
             ))

;;(get-file "inputs/input21-05.txt")
;(write (get-file "inputs/5-20000-6400.in"))
(format T "your answer is ~A " (get-file "inputs/5-50000-10000.in"))

;;this is the answer to part 1
(defun answer-part1 (infile)
  (let ((mymap (make-hash-table :test 'equal)))  
    (score-map (mark-map mymap (remove-if #'angular-p
				      (mapcar #'parse-line
					      (uiop:read-file-lines infile)))))))

;;(answer-part1 "inputs/input21-05.txt")

;;this is the answer to part 2, just deleting the test for angular
(defun answer-part2 (infile)
  (let ((m (make-hash-table :test 'equal)))  
    (score-map (mark-map m (mapcar #'parse-line
			         (uiop:read-file-lines infile))))))

;;(answer-part2 "inputs/input21-05.txt")
