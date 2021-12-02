(defpackage :advent21-01
  (:use :cl))

(ql:quickload :cl-ppcre)
(ql:quickload :lisp-unit)
(ql:quickload :uiop)
(ql:quickload :alexandria)

(in-package :advent21-01)

(defun parse-line (str)
  (let ((tmpstr (cl-ppcre:split "\ " str) ))
    (list (car tmpstr) (parse-integer (car (cdr tmpstr))))))

(parse-line '"forward 5")
    

(defun get-ping-list ()
  (mapcar #'parse-line (uiop:read-file-lines "inputs/input21-02.txt")))


(defparameter +pings+ (get-ping-list))
(defparameter +pings-test+ '(("forward" 5) ("down" 5) ("forward" 8) ("up" 3) ("down" 8) ("forward" 2)))

+pings+
+pings-test+
(defparameter *hpos* 0)
(defparameter *dpos* 0)
(defparameter *aim* 0)

(defun calc-move (a)
  (cond 
	 ((string= (car a) '"forward") (setq *hpos* (+ *hpos* (car (cdr a)))))
	 ((string= (car a) '"down")    (progn
					 (setq *dpos* (+ *dpos* (car (cdr a))))
					 (setq *aim* (+ *aim* (car (cdr a))))))
	 ((string= (car a) '"up")      ((setq *aim* (- *aim* (car (cdr a))))))
	 )
  )



(setq *dpos* 0)
(setq *hpos* 0)
(setq *aim* 0)

((string= (car '("forward" 5)) '"forward") (setq *hpos* 5))
						 (+ *hpos* (car (cdr '("forward" 5))))))

(car (cdr '("forward" 5)))
(string= (car '("forward" 5)) '"forward" )

(calc-move '(('"forward" 5)))

(mapcar #'calc-move +pings+)
(* *hpos* *dpos*)



(lisp-unit:define-test test-list-difference
  (lisp-unit:assert-equal 'nil (list-difference '(0) ))
  (lisp-unit:assert-equal '(1) (list-difference '(0 1)))
  (lisp-unit:assert-equal '(1 2 2) (list-difference '(0 1 3 5) )))

(lisp-unit:run-tests '(test-list-difference))

(defun sums-difference (list)
  (length (remove-if (lambda (x) (if (<= x 0) x 'nil)) (list-difference list))))

(lisp-unit:define-test test-sums-difference
  (lisp-unit:assert-equal '0 (sums-difference '(0) ))
  (lisp-unit:assert-equal '1 (sums-difference '(0 1)))
  (lisp-unit:assert-equal '3 (sums-difference '(0 1 3 5) ))
  (lisp-unit:assert-equal '7 (sums-difference +pings-test+ )))

(lisp-unit:run-tests '(test-sums-difference))

;;answer to part 1
(sums-difference +pings+)

(defun window (i list)
  (let ((listdiff (- (length list) i)))
    (cond ((> listdiff 2) (list (nth i list) (nth (1+ i) list) (nth (+ i 2) list)))
          ('T 'nil))))

(lisp-unit:define-test test-window
  (lisp-unit:assert-equal 'nil (window 1 '(0) ))
  (lisp-unit:assert-equal 'nil (window 0 '(0 1)))
  (lisp-unit:assert-equal '(0 1 3) (window 0 '(0 1 3 5) ))
  (lisp-unit:assert-equal '(1 3 5) (window 1 '(0 1 3 5) ))
  (lisp-unit:assert-equal 'nil (window 2 '(0 1 3 5) ))
  (lisp-unit:assert-equal '(208 210 200) (window 2 +pings-test+ )))

(lisp-unit:run-tests '(test-window))

(defun list-window (list)
  (loop for i from 0 to (- (length list) 1)
        collect (reduce '+ (remove 'nil (window i list)))))

(defun sonar-sweep (list)
  (sums-difference (remove '0 (list-window list))))

(lisp-unit:define-test test-list-window
  (lisp-unit:assert-equal '(0) (list-window '(0)))
  (lisp-unit:assert-equal '(0 0) (list-window '(0 1)))
  (lisp-unit:assert-equal '(4 9 0 0) (list-window '(0 1 3 5) ))
  (lisp-unit:assert-equal '(607 618 618 617 647 716 769 792 0 0) (list-window +pings-test+ ))
  (lisp-unit:assert-equal '5 (sonar-sweep +pings-test+ )))

(lisp-unit:run-tests '(test-list-window))

(lisp-unit:run-tests :all)
;;this is the answer to part 2
(sonar-sweep +pings+)



