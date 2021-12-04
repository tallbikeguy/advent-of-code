(ql:quickload :cl-ppcre)
(ql:quickload :lisp-unit)
(ql:quickload :uiop)

(defpackage :advent21-04
  (:use :cl :cl-ppcre :uiop :lisp-unit))

(in-package :advent21-04)

(defparameter *draws* 'nil)
(defparameter *boards* 'nil)

(defun group<n (l n)
  'nil)

(defun group<n-sub ( l m n )
    (if (and (cdr l) (< 0 n))
        (group<n-sub (cons (cons (cadr l) (car l)) (cddr l)) m (1- n))
        (cons (reverse (car l)) (group<n (cdr l) m))
    )
  )

(defun group<n ( l n )
    (if l (group<n-sub (cons nil l) n n))
)

(defun proc-board-line (line)
  (mapcar #'parse-integer (split '" " (regex-replace-all '"^ " (regex-replace-all '" +" line '" ") '"" ))))

(defun get-file (filename)
  (with-open-file (stream filename)
    (progn
      (setf *draws* (mapcar #'parse-integer (cl-ppcre:split '"\," (read-line stream nil))))
      (setf *boards* (group<n (remove 'nil (loop for line = (read-line stream nil)
                           while line
                           collect  (proc-board-line line))) 5 )))))

(defun register-draw-on-spot (d spot)
  (if (= d spot) (- (+ 100 d)) spot))

(defun register-draw-on-line (d l)
  (mapcar (lambda (spot) (register-draw-on-spot d spot) ) l))

(defun register-draw-on-board (d b)
  (mapcar (lambda (l) (register-draw-on-line d l) ) b))

(defun register-draw (d)
  (progn
    (setf *boards* (mapcar (lambda (b) (register-draw-on-board d b) ) *boards*)))
  )

(defun check-board-rows-for-win (b)
  (if (> (length (remove-if (lambda (l) (/= 5 (length l)))  (remove 'nil (loop for row from 0 to 4
        collect (remove 'nil (loop for col from 0 to 4
                      collect (if (> 0 (nth col (nth row b))) 'T 'nil))))))) 0) 'T 'nil))

(defun check-board-cols-for-win (b)
  (if (> (length (remove-if (lambda (l) (/= 5 (length l)))  (remove 'nil (loop for row from 0 to 4
        collect (remove 'nil (loop for col from 0 to 4
                      collect (if (> 0 (nth row (nth col b))) 'T 'nil))))))) 0) 'T 'nil))


(defun sum-list-integers (list)
  (reduce '+ list))

(defun calc-score (b x)
  (* x (sum-list-integers (loop for row from 0 to 4
        collect (sum-list-integers (loop for col from 0 to 4
                      collect (if (< 0 (nth col (nth row b))) (nth col (nth row b)) 0) ))))))

(defun check-board-for-win (b)
  (or (check-board-rows-for-win b)
          (check-board-cols-for-win b)))
  

(defun check-boards-for-win ()
  (loop for boardnum from 0 to (1- (length *boards*))
        when (check-board-for-win (nth boardnum *boards*))
          return boardnum
        ))

(defun check-most-boards-for-win ()
  (if (= (length (remove 'nil (loop for boardnum from 0 to (1- (length *boards*))
        collect (check-board-for-win (nth boardnum *boards*))
        ))) (1- (length *boards*))) 'T 'nil))

(defun check-all-boards-for-win ()
  (if (= (length (remove 'nil (loop for boardnum from 0 to (1- (length *boards*))
        collect (check-board-for-win (nth boardnum *boards*))
        )))  (length *boards*)) 'T 'nil))

(defun play ()
  (loop for draw in *draws*
        do (register-draw draw)
        when (check-boards-for-win)
          return (calc-score (nth (check-boards-for-win) *boards*) draw)))

(defun find-unwinning-board ()
  (loop for boardnum from 0 to (1- (length *boards*))
        when (not (check-board-for-win (nth boardnum *boards*)))
          return boardnum
        ))

(defun neg-play ()
  (loop for draw in *draws*
        with boardnum = 0
        do (progn
             (register-draw draw)
             (if (check-most-boards-for-win) (setq boardnum (find-unwinning-board))))
        when (check-all-boards-for-win)
          return (calc-score (nth boardnum *boards*) draw)))

;;answer to the first part
(get-file "inputs/input21-04.txt")
(play)

;;answer to the second part
(get-file "inputs/input21-04.txt")
(neg-play)

