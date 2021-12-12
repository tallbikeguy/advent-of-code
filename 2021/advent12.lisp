(ql:quickload 'alexandria)
(ql:quickload 'cl-ppcre)
(defpackage :advent21-12 (:use :cl :cl-ppcre :alexandria))
(in-package :advent21-12)

(defparameter *input* 'nil)

(defun is-in-current (current move)
  (find (cadr move) current :test #'string= ))

(defun count-is-in-current (current move)
  (length (remove 'nil (mapcar (lambda (x) (string= move x)) current))))

(defun is-lower-case (s)
  (string= (string-downcase s ) s))

(defun existing-duplicate-lowercase (c)
  ;;return true if there are any duplicate lowercase caves in list
  (let ((result (first (sort (mapcar (lambda (x) (if (is-lower-case x)
                                       (count-is-in-current c x)
                                       '0)) c) #'>))))
    (if (not result) 'nil (if (> result 1) 'T 'nil))))

;;for part 1, function to remove in-current and lower case
(defun valid-move-p (current x)
  (not (and (is-in-current current x) (is-lower-case (cadr x)))))
  
;;for part 2, we need to make sure that we don't visit
;;two lower case caves
(defun valid-move-p (current x)
  (not (or (and (is-in-current current x)
            (is-lower-case (cadr x))
            (existing-duplicate-lowercase current))
           (string= '"start" (cadr x))
       )
  ))

(defun getmoves ( current startloc)
  ;;get a list of all the places you can go to, from the current location
  (remove-if (lambda (x) (not (valid-move-p current x)))
             (remove 'nil (mapcar (lambda (e)
                                    (if (string= startloc (car e)) e 'nil)) *input*))))

(defun move-count (current-moves startloc)
  (let ((local-moves current-moves))    
  (if (string= startloc "end")
      '1
      (reduce #'+ (mapcar (lambda (m)
                            (let ((lm local-moves))
                              (push (cadr m) lm)
                              (move-count lm (cadr m))))
                          (getmoves current-moves startloc))))))

(defun parse-line (str)
  (cl-ppcre:split "-" str))

(defun get-input-list ()
  (mapcar #'parse-line (uiop:read-file-lines "input")))

(defparameter *input*  (append  (mapcar #'reverse (get-input-list)) (get-input-list)))
(time (move-count '("start") "start"))
