(defpackage :advent21-02-small
  (:use :cl))

(ql:quickload :cl-ppcre)
(ql:quickload :lisp-unit)
(ql:quickload :uiop)
(ql:quickload :alexandria)

(in-package :advent21-02-small)

(defparameter *hpos* 0)
(defparameter *dpos* 0)
(defparameter *aim* 0)

(cl-ppcre:split "\ " '"forward 5")
http://10.235.0.27:9664/metrics

(defun proc-line (str)
  (let* ((tmpstr (cl-ppcre:split "\ " str) )
         (tmpcmd (car tmpstr))
         (tmpparm (parse-integer (car (cdr tmpstr)))))
    (cond 
      ((string= tmpcmd '"forward") (setq *hpos* (+ *hpos* tmpparm)))
      ((string= tmpcmd '"down")    (setq *dpos* (+ *dpos* tmpparm)))
      ((string= tmpcmd '"up")      (setq *dpos* (- *dpos* tmpparm)))
      )))
    

(defun proc-input ()
  (progn
    (setq *dpos* 0)
    (setq *hpos* 0)
    (setq *aim* 0)    
    (mapcar #'proc-line (uiop:read-file-lines "inputs/input21-02.txt"))
    (* *hpos* *dpos*)))

;;part 1
(write (proc-input))

(defun proc-line (str)
  (let* ((tmpstr (cl-ppcre:split "\ " str) )
         (tmpcmd (car tmpstr))
         (tmpparm (parse-integer (car (cdr tmpstr)))))
    (cond 
      ((string= tmpcmd '"forward") (progn (setq *hpos* (+ *hpos* tmpparm))
                                          (setq *dpos* (+ *dpos* (* *aim* tmpparm)))))
      ((string= tmpcmd '"down")    (setq *aim* (+ *aim* tmpparm)))
      ((string= tmpcmd '"up")      (setq *aim* (- *aim* tmpparm)))
      )))

;;part 2
(write (proc-input))


