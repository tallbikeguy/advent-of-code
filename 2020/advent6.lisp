(defpackage :advent6-package
  (:use :cl))

(ql:quickload "uiop")
(ql:quickload :cl-ppcre)
(ql:quickload :lisp-unit)
(ql:quickload :uiop)

;;(in-package :cl-ppcre)
;;(in-package :uiop)

(in-package :advent6-package)

(defun flatten (lst)
  (labels ((rflatten (lst1 acc)
             (dolist (el lst1)
               (if (listp el)
                   (setf acc (rflatten el acc))
                   (push el acc)))
             acc))
    (reverse (rflatten lst nil))))


(defun read-file-as-lines (filename)
  (with-open-file (in filename)
    (loop for line = (read-line in nil nil)
          while line
          collect line)))

(defparameter *allrecords*
  (read-file-as-lines "~/Downloads/input6.txt"))

(defparameter *allrecordsn* (reverse (cons '"" (reverse *allrecords*))))

(defparameter *allrecordsinlists*
  (loop with sublist
        for el in *allrecordsn*
        if (equal el '"" )
          collect (nreverse sublist) and do (setf sublist nil)
        else
          do (push el sublist)))

(defun concatString (list)
  "A non-recursive function that concatenates a list of strings."
  (if (listp list)
      (let ((result ""))
        (dolist (item list)
          (if (stringp item)
              (setq result (concatenate 'string result item))))
        result)))

(defun sum-list-integers (list)
  (reduce '+ list))
;;this is the answer to the first part
(sum-list-integers (mapcar #'length (mapcar #'remove-duplicates (mapcar #'concatString *allrecordsinlists*))))


(defun string-to-list (str)
  (loop for c in (coerce str 'list)
        collecting (intern (string-upcase (string c)))))

(defun intersect (lst-a lst-b &aux result)
  (dolist (a lst-a (nreverse result))
    (dolist (b lst-b)
      (when (equal a b)
        (push a result)))))

(defparameter *recsinlist*
  (mapcar (lambda (s) (mapcar #'string-to-list s)) *allrecordsinlists*))

(defun intersect-all-lists (alllist)
  (if (< (length alllist) 2)
      alllist
      (intersect-all-lists (cons (intersection (nth 0 alllist) (nth 1 alllist)) (cdr (cdr alllist))))
      ))

(defun list-to-str ( lst )
    (if (cdr lst)
        (concatenate (car lst) (list-to-str (cdr lst) ))
        (car lst)
    )
  )

(defun list-to-string (lst)
  (if (not lst)
      'nil
      (format nil "~{~A~}" lst)))

(defun list-to-string-len (s) (length (list-to-string s)))

;;this is the answer to advent6.2
(sum-list-integers (flatten (mapcar (lambda (s) (mapcar #'list-to-string-len s)) (mapcar #'intersect-all-lists  (mapcar (lambda (s) (mapcar #'string-to-list s)) *allrecordsinlists*)))))
