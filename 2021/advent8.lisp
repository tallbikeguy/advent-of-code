    (ql:quickload 'iterate)
    (ql:quickload 'series)
    (ql:quickload 'cl-ppcre)
    (require 'iterate)
    (defpackage :advent21-08 (:use :cl :uiop :iterate))
(in-package :advent21-08)

(defun flatten (lst)
  (labels ((rflatten (lst1 acc)
             (dolist (el lst1)
               (if (listp el)
                   (setf acc (rflatten el acc))
                   (push el acc)))
             acc))
    (reverse (rflatten lst nil))))

(defun list-to-string (lst)
  (format nil "~{~A~}" lst))

(defun sortstr (str)
  (list-to-string(sort (cl-ppcre:split "" str) #'string<)))

(defun parse-line (line)
  (let* (
         (astr nil)
         (bstr nil)
         (cstr nil)
         )
    (setf astr (cl-ppcre:split '#\| line))
    (setf bstr (mapcar #'sortstr (cl-ppcre:split "\ " (car astr))))
    (setf cstr (mapcar #'sortstr (cdr (cl-ppcre:split "\ " (car (cdr astr))))))
    (list bstr cstr)))

(defun findstrings (i c)
  (remove 'nil (loop for thisstr in i
        collect (if (= (length thisstr) c) thisstr 'nil))))

(defun determine-maps (instr)
  (let* (
         (instrings (parse-line instr))
         (7string (findstrings (car instrings) 3))
         (1string (findstrings (car instrings) 2))
         (4string (findstrings (car instrings) 4))
         (8string (findstrings (car instrings) 7))
         (6-9-0-string (findstrings (car instrings) 6))
         (2-3-5-string (findstrings (car instrings) 5))
         (s7 (sort (cl-ppcre:split "" (car 7string)) #'string<))
         (s1 (sort (cl-ppcre:split "" (car 1string)) #'string<))
         (s4 (sort (cl-ppcre:split "" (car 4string)) #'string<))
         (s8 (sort (cl-ppcre:split "" (car 8string)) #'string<))
         (s-6-9-0 (mapcar (lambda (s) (cl-ppcre:split "" s)) 6-9-0-string ))
         (s-2-3-5 (mapcar (lambda (s) (cl-ppcre:split "" s)) 2-3-5-string ))
         (v2-v4 s1)
         (h1-v2-v4 s7)
         (h2-v1-v2-v4 s4)
         (h1-h2-h3-v1-v2-v3-v4 s8)
         (h1 (set-difference h1-v2-v4 v2-v4 :test #'string=))
         (h2-v1 (set-difference h2-v1-v2-v4 v2-v4 :test #'string=))
         (h2-h3-v1-v3 (set-difference h1-h2-h3-v1-v2-v3-v4 h1-v2-v4 :test #'string=))
         (h3-v3 (set-difference h2-h3-v1-v3 h2-v1 :test #'string=))
         (h1-h3-v1-v4 (intersection (intersection (car s-6-9-0)(cadr s-6-9-0) :test #'string=) (caddr s-6-9-0) :test #'string=))
         (h1-v4 (set-difference h1-h3-v1-v4 h2-h3-v1-v3 :test #'string=))
         (v4 (set-difference h1-v4 h1 :test #'string=))
         (v2 (set-difference v2-v4 v4 :test #'string=))
         (h3-v1 (set-difference (set-difference h1-h3-v1-v4 h1 :test #'string=) v4 :test #'string=))
         (h3 (set-difference h3-v1 h2-v1 :test #'string=))
         (v1 (set-difference h3-v1 h3 :test #'string=))
         (h2 (set-difference h2-v1 v1 :test #'string=))
         (v3 (set-difference h3-v3 h3 :test #'string=)))
    (list
     (list-to-string (sort (flatten (list v1 v2 v3 v4 h1 h3)) 'string<)) ;;0
     (list-to-string (sort (flatten (list v2 v4)) 'string<)) ;;1
     (list-to-string (sort (flatten (list h1 h2 h3 v2 v3)) 'string<)) ;;2
     (list-to-string (sort (flatten (list h1 h2 h3 v2 v4)) 'string<)) ;;3
     (list-to-string (sort (flatten (list v1 v2 v4 h2)) 'string<)) ;;4
     (list-to-string (sort (flatten (list h1 h2 h3 v1 v4)) 'string<)) ;;5
     (list-to-string (sort (flatten (list h1 h2 h3 v1 v3 v4)) 'string<)) ;;6
     (list-to-string (sort (flatten (list h1 v2 v4)) 'string<)) ;;7
     (list-to-string (sort (flatten (list h1 h2 h3 v1 v2 v3 v4)) 'string<)) ;;8
     (list-to-string (sort (flatten (list h1 h2 h3 v1 v2 v4)) 'string<)) ;;9
     )))

(defun convert-out (str maps)
  (loop for i from 0 to 9
        when (string= str (nth i maps))
          return i
        ))
        
(defun map-output-data (maps instr)
    (mapcar (lambda (x) (convert-out x maps)) 
     (mapcar #'sortstr (cadr (parse-line instr)))))

(defun proc-input-line (inline)
  (parse-integer (list-to-string (map-output-data (determine-maps inline) inline))))

(defun proc-input (fname)
  (reduce #'+ (mapcar #'proc-input-line (uiop:read-file-lines fname))))

(proc-input "input")


