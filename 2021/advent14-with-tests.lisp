(ql:quickload 'cl-ppcre)
(ql:quickload :uiop)
(ql:quickload :lisp-unit)
(defpackage :advent21-14 (:use :cl :cl-ppcre :lisp-unit))
(in-package :advent21-14)

(defparameter *allinput* (ppcre:split "\\n\\n" (uiop:read-file-string "input-test")))
(defparameter *start* (ppcre:split "" (car *allinput*)))
(defparameter *rules* (mapcar (lambda (x) (ppcre:split #\  x)) (ppcre:split #\newline (cadr *allinput*))))

(defun list-to-string (l)
  (coerce (mapcar (lambda (x) (coerce x 'character)) l ) 'string))

(defun find-applicable-rule (rules a b )
  (if (and a b) (let ((replacement (assoc (list-to-string (list a b)) rules :test #'string=)))
                  (if replacement (caddr replacement) 'nil))))

(defun ind (x)
  (- (char-code (char x 0)) (char-code (char '"A" 0))))


(defun make-lc (&optional a b c)
  (let ((ret (make-array 26 :element-type 'integer :initial-element 0)))
    (if a (setf (aref ret (ind a)) (1+ (aref ret (ind a)))))
    (if b (setf (aref ret (ind b)) (1+ (aref ret (ind b)))))
    (if c (setf (aref ret (ind c)) (1+ (aref ret (ind c)))))
    ret))

(defun print-lc (lc)
  (progn
    (loop for i from 0 to 25
          do (format t "~a:~a " (code-char (+ i (char-code #\A))) (aref lc i)))
    (terpri)))

(defun lc+ (a b)
  (let ((result (make-array 26 :element-type 'integer :initial-element 0)))
    (loop for i from 0 to 25
          do (setf (aref result i) (+ (aref a i) (aref b i))))
    result))

(defun lc= (a b)
  (loop for i from 0 to 25
        when (/= (aref a i) (aref b i))
          do (return  'nil)
        finally (return 'T)))

(lisp-unit:define-test test-lcops
  (lisp-unit:assert-true (lc= '#(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) (make-lc)))
  (lisp-unit:assert-true (lc= '#(1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) (make-lc "A")))
  (lisp-unit:assert-true (lc= '#(0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) (make-lc "B")))
  (lisp-unit:assert-true (lc= '#(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1) (make-lc "Z")))
  (lisp-unit:assert-true (lc= '#(1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1) (make-lc "Z" "B" "A")))
  (lisp-unit:assert-true (lc= '#(1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) '#(1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))
  )

(lisp-unit:run-tests '(test-lcops))

(defun level-counts-impl (a b l d)
(if (= l d)
    (make-lc b)
    (let ((new-elt (find-applicable-rule *rules* a b)))
      (lc+ (level-counts a new-elt (1+ l) d) (level-counts new-elt b (1+ l) d)))))

(defun memoize (fn)
  (let ((cache (make-hash-table :test #'equal)))
    #'(lambda (&rest args)
        (multiple-value-bind
              (result exists)
            (gethash args cache)
          (if exists
              result
              (setf (gethash args cache)
                    (apply fn args)))))))

(setf (fdefinition 'level-counts) (memoize #'level-counts-impl))

(defun doloops (start iters)
(let ((counts (make-lc)))
(loop for (a b) on start
      do (if b (setf counts (lc+ counts (level-counts a b 1 (1+ iters))))))
  (lc+ (make-lc (car start)) counts)))

(defun lcsumstr (str)
(let ((lcsum (make-lc)))
  (loop for i in (split "" str)
        do (setf lcsum (lc+ lcsum (make-lc i))))
  lcsum))

(lisp-unit:define-test test-lcsumstr
  (lisp-unit:assert-true (lc= '#(1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) (lcsumstr "A")))
  (lisp-unit:assert-true (lc= '#(2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) (lcsumstr "AA")))
  (lisp-unit:assert-true (lc= '#(2 3 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) (lcsumstr "AABBB")))
  (lisp-unit:assert-true (lc= '#(4 3 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1) (lcsumstr "AAAABBBZ")))
  )

(lisp-unit:run-tests '(test-lcsumstr))

(lisp-unit:define-test test-lccompute
  (lisp-unit:assert-true (lc= (doloops (split "" "NNCB" ) 1) (lcsumstr "NCNBCHB")))
  (lisp-unit:assert-true (lc= (doloops (split "" "NNCB" ) 2) (lcsumstr "NBCCNBBBCBHCB")))
  (lisp-unit:assert-true (lc= (doloops (split "" "NNCB" ) 3) (lcsumstr "NBBBCNCCNBBNBNBBCHBHHBCHB")))
  (lisp-unit:assert-true (lc= (doloops (split "" "NNCB" ) 4) (lcsumstr "NBBNBNBBCCNBCNCCNBBNBBNBBBNBBNBBCBHCBHHNHCBBCBHCB")))
  )
 
(lisp-unit:run-tests '(test-lccompute))
  
(defparameter *allinput* (ppcre:split "\\n\\n" (uiop:read-file-string "input")))
(defparameter *start* (ppcre:split "" (car *allinput*)))
(defparameter *rules* (mapcar (lambda (x) (ppcre:split #\  x)) (ppcre:split #\newline (cadr *allinput*))))

(defun do-part-2 ()
  (let* (
        (raw-out (doloops *start* 40))
        (out (coerce (sort (remove-if (lambda (x) (= x 0)) raw-out) #'>) 'list )))
        (write (- (car out) (car (reverse out))))))

(time ( do-part-2))
