(ql:quickload 'iterate)
(ql:quickload 'cl-ppcre)
(ql:quickload 'uiop)

(require 'iterate)
(require :queues.simple-queue)
    (defpackage :advent21-10 (:use :cl :uiop :iterate ))
(in-package :advent21-10)

(defparameter *stack* 
  (list 0))

(defun stack-push (&rest args)
  (dolist (arg args)
    (push arg *stack*)))

(defun stack-pop ()
  (let ((c (car *stack*)))
    (setf *stack* (cdr *stack*))
    c))

(defun stack-clear ()
  (defparameter *stack* (list 0)))

(defun stack-list ()
  *stack*)

(defun stack-print () (print *stack*))

(defparameter *openchars*  (coerce "[({<" 'list))
(defparameter *charpairs*  '((#\[ . #\]) (#\( . #\)) (#\{ . #\})(#\< . #\>)))

(defun char-check (c)
  (if (find c *openchars* :test #'char=)
      (progn (stack-push c) 'nil)
      (let ((mc (stack-pop )))
        (if (char= c (cdr (assoc mc *charpairs*))) 'nil
            (progn (stack-push c)
                   c)))))

(defparameter *charvals*  '((#\) . 3) (#\] . 57) (#\} . 1197)(#\> . 25137)))

(defun check-input-line (l)
  (loop :for j in l
        :when (char-check j)
          :return j))       

(defparameter *input* 'nil)

(defun check-input ()
  (loop for i in *input*
        :collect (let ((v 0))
                   (stack-clear)
                   (setf v (check-input-line (coerce i 'list)))
                   (if v (cdr (assoc v *charvals*))
                       'nil))))
             
;;this is part 1 answer
(defun advent-part1 ()
  (progn
    (defparameter *input* (uiop:read-file-lines "input"))
    (reduce #'+ (remove 'nil (check-input)))
    ))

;; part2
(defparameter *incompvals*  '((#\( . 1) (#\[ . 2) (#\{ . 3)(#\< . 4)))

(defun stack-score ()
  (let ((retval 0))
    (loop for i in (stack-list)
        :do (if (characterp i)  (setq retval (+ (* 5 retval) (cdr (assoc i *incompvals*)))))
          )
    retval))

(defun check-incomplete-line (l)
  (loop :for j in l
        :when (char-check j)
        :return (stack-score)))       

(defun check-incomplete-input ()
  (loop for i in *input*
        :collect (progn
                   (stack-clear)
                   (if (not (check-input-line (coerce i 'list)))
                       (stack-score)))))

(defparameter *score-vals* 'nil )

;;final answer to part 2
(defun advent-part2 ()
  (progn
    (defparameter *input* (uiop:read-file-lines "input"))
    (defparameter *score-vals* (sort  (remove 'nil (check-incomplete-input)) #'>))
    (nth  (floor (/ (length *score-vals*) 2 ) ) *score-vals*)
    ))

(format t "advent day10 part1 answer: ~a" (advent-part1))
(format t "advent day10 part2 answer: ~a" (advent-part2))
