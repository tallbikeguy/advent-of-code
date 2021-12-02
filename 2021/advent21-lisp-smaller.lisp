    (defpackage :advent2101
      (:use :cl))
    
    (ql:quickload :uiop)
    
    (in-package :advent2101)
    
    (defparameter +pings+  (mapcar #'parse-integer (uiop:read-file-lines "input21-01.txt")))
    
    (defun list-difference (list)
      (loop for i from 0 to (- (length list) 2)
        collect (- (nth (1+ i) list) (nth i list))))
    
    (defun sums-difference (list)
      (length (remove-if (lambda (x) (if (<= x 0) x 'nil)) (list-difference list))))
    
    ;;answer to part 1
    (sums-difference +pings+)
    
    (defun window (i list)
      (let ((listdiff (- (length list) i)))
        (cond ((> listdiff 2) (list (nth i list) (nth (1+ i) list) (nth (+ i 2) list)))
              ('T 'nil))))
    
    (defun list-window (list)
      (loop for i from 0 to (- (length list) 1)
            collect (reduce '+ (remove 'nil (window i list)))))
    
    (defun sonar-sweep (list)
      (sums-difference (remove '0 (list-window list))))
    
    ;;this is the answer to part 2
    (sonar-sweep +pings+)

