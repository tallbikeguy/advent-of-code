(defpackage :advent20-10
  (:use :cl))

(ql:quickload :cl-ppcre)
(ql:quickload :lisp-unit)
(ql:quickload :uiop)
(ql:quickload :alexandria)
(require :sb-sprof)

(in-package :advent20-10)

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

(defparameter *input-file* 'nil)
(defun get-plug-list ()
  (let ((pluglist (append '(0) (sort (mapcar #'parse-integer (uiop:read-file-lines *input-file* ) ) #'< ))))
    (append pluglist (list (+ 3 (apply #'max pluglist))))))

(defparameter +plugs+ 'nil)

;;+plugs+
(defparameter +vec-plugs+ 'nil)

(defun list-difference (list)
  (loop for i from 0 to (- (length list) 2)
    collect (- (nth (1+ i) list) (nth i list))))

(defun list-difference (list)
  (loop for (a b) on list while b collect (- b a)))

(lisp-unit:define-test test-list-difference
  (lisp-unit:assert-equal 'nil (list-difference '(0) ))
  (lisp-unit:assert-equal '(1) (list-difference '(0 1)))
  (lisp-unit:assert-equal '(1 2 2) (list-difference '(0 1 3 5) ))
  (lisp-unit:assert-equal '(1 1 2 1 2) (list-difference '(0 1 2 4 5 7) )))

(lisp-unit:run-tests '(test-list-difference))
(lisp-unit:run-tests :all)

  
(defun docalc ()
  (let ((listd (list-difference +plugs+)))
    (list (length (remove '1 listd)) (length (remove '3 listd)))))



;;this is the answer for part 1
(apply #'* (docalc))


(defun maxl (l)
  (let ((retval (nth 0 l)))
    (loop for i from 0 to (1- (length l)) do
      (if (< retval (nth i l))
          (setf retval (nth i l))))
    retval))

;;part 2
;;we need to find all paths from the first to the last, in steps of 1 or 3
+plugs+

(defun getplugs (s pluglist)
    (remove 'nil (mapcar (lambda (n) (if (or (<= (- n s) 0) (> (- n s ) 3)) 'nil n) ) pluglist )))

(defun getrem (s pluglist)
    (remove 'nil (mapcar (lambda (n) (if (>  n s) n 'nil ))  pluglist )))


(defparameter  *steplist* 0)

(defparameter +vec-plugs-len+ 0)
(defparameter +vec-plugs-len-m1+ 0)

(defparameter *ranges* 'nil)
(defparameter *vec-ranges* 'nil)
  
(defun makerange (s)
  (loop for i from (1+ s ) to +vec-plugs-len-m1+
        with startval = (svref +vec-plugs+ s)
        for svrefdiff = (- (svref +vec-plugs+ i) startval)
        while (<= svrefdiff 3)
        collect i
        ))


(defun makeranges ()
  (loop for i from 0 to (1- +vec-plugs-len-m1+)
        collect (makerange i)))

(defun  getrange (s)
  (nth s *ranges*))

(defun step-perms-old (startindex end)
  (if (= startindex +vec-plugs-len-m1+)
      (setf *steplist* (1+ *steplist*))
      (let ((remlist (nth startindex *ranges*)))            
        (loop for i in remlist do
          (step-perms i end)
              ))))

(defun step-perms-old-new (startindex end)
  (if (= startindex +vec-plugs-len-m1+)
      1
      (loop for i in (nth startindex *ranges*) 
            sum (step-perms-old-new i end)
            )))

(setf (fdefinition 'step-perms-old-new) (memoize #'step-perms-old-new))

(defun step-perms (startindex end)
  (loop for i in (svref *vec-ranges* startindex) do
    (if (= i +vec-plugs-len-m1+)
        (setf *steplist* (1+ *steplist*))
        (step-perms i end)
        )))

(defun step-perms-new (startindex end)
  (loop named loop1 for i in (svref *vec-ranges* startindex) do
    (if (= i +vec-plugs-len-m1+)
        return-from loop1 values 1
        (step-perms-new i end)
        )))


(defun do-part-2 (filename)
  (progn
    (setf *input-file* filename )
    (setf +plugs+ (get-plug-list))
    (setf +vec-plugs+ (coerce +plugs+ 'simple-vector))
    (setf +vec-plugs-len+ (length +vec-plugs+))
    (setf +vec-plugs-len-m1+ (1- (length +vec-plugs+)))
    (setf *steplist* 0)
    (setf *ranges* (makeranges))
    (setf *vec-ranges* (coerce *ranges* 'simple-vector))
    ;(sb-sprof:with-profiling (:max-samples 1000000 :report :flat :loop nil)
    (time (setf *steplist* (step-perms-old-new 0 (svref +vec-plugs+ (1- (length +vec-plugs+))))))
    ;)
    (format t "Number of paths: ~a" *steplist*)
    *steplist*))

(do-part-2 "inputs/input10-test1.txt")
(do-part-2 "inputs/input10.txt")

;;(trace step-perms )
;;(untrace step-perms)

;;+plugs+

(defun do-step-perms (l s r e)
  (progn
    (setf *steplist* 'nil)
    (step-perms l s r e)
    *steplist*))

;;(length *steplist*)
;;(do-step-perms '(1 ) 2 '(0 ) 2)
;;(step-perms '( ) 1 '(1 2 ) 2)
;;(step-perms '() 0 '(0 1 2 ) 2)
cl:;;(step-perms '() 0 '(0 1 3 ) 3)
;;(step-perms '() 0 '(0 1 2 3 ) 3)
;;(step-perms '() 0 '(0 2 3) 2)
;;(step-perms '() 0 '(0 1 2) 2)

;;(step-perms '() 0 '(0 1 2 ) 2)

;;(step-perms '() 0 '(0 1 2 3 6 7 8 9) 9)


;;(maxl '(16 10 15 5 1 11 7 19 6 12 4))
;;(sort '(16 10 15 5 1 11 7 19 6 12 4) #'< )


(defun test-advent10 (mylist)
  (let ((test1 mylist))
    (step-perms '() 0 (sort test1 #'< ) (maxl test1))))

;;(length (test-advent10))
;;(setf *steplist* 'nil)
;;(length (test-advent10 '(16 10 15 5 1 11 7 19 6 12 4)))
;;(length *steplist*)

;;(setf *steplist* 0)
;; (test-advent10 '(28 33 18 42 31 14 46 20 48 47 24 23 49 45 19 38 39 11 1 32 25 35 8 17 7 9 4 2 34 10 3))
;; *steplist*

(setf *steplist* 0)
(test-advent10 +plugs+)
*steplist*

;;28 33 18 42 31 14 46 20 48 47 24 23 49 45 19 38 39 11 1 32 25 35 8 17 7 9 4 2 34 10 3
