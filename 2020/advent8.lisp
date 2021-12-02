(defpackage :advent8-package
  (:use :cl))

(ql:quickload "uiop")
(ql:quickload :cl-ppcre)
(ql:quickload :lisp-unit)
(ql:quickload :uiop)
(ql:quickload :alexandria)

(in-package :advent8-package)

(defstruct instruction
  instr-loc
  raw-instr
  instr
  sign
  unsigned-value
  value
  exec-count
  )
  
(defun read-file-as-lines (filename)
  (with-open-file (in filename)
    (loop for line = (read-line in nil nil)
          while line
          collect line)))

(defparameter *allrecords* 'nil)

;;(nth 0 *allrecords*)
;;(nth 0 *all-instructions*)
;;(nth 1 *all-instructions*)
;;(nth 2 *all-instructions*)

(defun parse-string (str)
  (let* (
         (astr nil)
         (bstr nil)
         )
    (setf (values astr bstr)
          (cl-ppcre:scan-to-strings "^([a-z][a-z][a-z]) (.)([0-9]+)$" str))
    (make-instruction :instr-loc 0
                      :raw-instr astr
                      :instr (svref bstr 0)
                      :sign (svref bstr 1)
                      :unsigned-value (parse-integer(svref bstr 2))
                      :value (if (string= (svref bstr 1 ) '"+") (parse-integer(svref bstr 2)) (- (parse-integer(svref bstr 2))))
                      :exec-count 0
                      )
    ))

(defparameter *all-instructions* 'nil)

(defun parse-all-instructions (filename)
  (progn
    (setq *pcloc* 0)
    (setq *allrecords* (read-file-as-lines filename))
    (setq *all-instructions*  (mapcar #'parse-string *allrecords*))
    (loop for i from 0 to (1- (length *all-instructions*)) do
      (setf (instruction-instr-loc (nth i *all-instructions*)) i))
    ))


(defparameter *accumulator* '0 )
(defparameter *program-counter* '0)
(defparameter *current-instruction* 'nil)

(defun get-full-instruction (inst-pc)
  (nth inst-pc *all-instructions*))


;;*allrecords*
;;(nth 0 *all-instructions*)
;;(nth 616 *all-instructions*)
;;(length *all-instructions*)
;;(nth 1 *all-instructions*)
;;(get-full-instruction 0)
;;(get-full-instruction 1)
;;(instruction-value (get-full-instruction 616))

(defun get-instruction-parm (inst-pc)
  (instruction-value (get-full-instruction inst-pc)))

;;(get-instruction-parm 0)
  
(defun get-instruction-sign (inst-pc)
  (instruction-sign (get-full-instruction inst-pc)))

;;(get-instruction-sign 0)

(defun get-instruction-parm-val (inst-pc)
  (instruction-value (get-full-instruction inst-pc)))

;;(get-full-instruction 0)
;;(get-instruction-parm-val 0)

(defun get-instruction (inst-pc)
  (instruction-instr (get-full-instruction inst-pc)))

;;(get-instruction 0)

(defun set-instruction-count (inst-pc newcount)
  (setf (instruction-exec-count (get-full-instruction inst-pc)) newcount))

;;(set-instruction-count 0 5)

(defun get-instruction-count (inst-pc)
  (instruction-exec-count (get-full-instruction inst-pc)))

;;(get-instruction-count 0)


(defun incr-instruction-count (inst-pc)
  (setf (instruction-exec-count (get-full-instruction inst-pc)) (1+ (get-instruction-count inst-pc))))

;;(incr-instruction-count 0)
;;(get-instruction-count 0)

(defun instruction-count-check (the-pc)
  (if (>= (get-instruction-count the-pc) 1)
      (progn
        ;;(write (format 'nil "found loop ~A" *accumulator*))
        'T
        )
      'nil))

(defun instr-nop ( )
  (if (instruction-count-check *program-counter*)
      'nil
      (progn
        ;;(write '"-NOP-")
        (incr-instruction-count *program-counter*)
        (setf *program-counter* (1+ *program-counter*))
        )))

(defun is-positive-arg (pc)
  (string= (get-instruction-sign pc ) '"+"))

(defun set-accumulator (newval)
  (setf *accumulator* newval))

(defun instr-acc ()
  (if (instruction-count-check *program-counter*)
      'nil
      (progn
        ;;(write '"-ACC-")
        (incr-instruction-count *program-counter*)
        (setf *accumulator* (+ *accumulator* (get-instruction-parm *program-counter*)))
        (setf *program-counter* (1+ *program-counter*))
        )))

(defun instr-jmp ()
  (if (instruction-count-check *program-counter*)
      'nil
      (progn
        ;;(write '"-JMP-")
        (incr-instruction-count *program-counter*)
        (setf *program-counter* (+ *program-counter* (get-instruction-parm *program-counter*)))
        )))

(defun is-jmp ()  (string= (get-instruction *program-counter*) '"jmp") )
(defun is-acc ()  (string= (get-instruction *program-counter*) '"acc") )
(defun is-nop ()  (string= (get-instruction *program-counter*) '"nop") )

(defun fetch-instruction (pc)
  (setq *current-instruction* (get-instruction pc)))

(defun substitute-instruction (instr)
  (if instr 
      (if (string= '"jmp" (instruction-instr (get-full-instruction instr)))
          (setf (instruction-instr (get-full-instruction instr)) '"nop")
          (if (string= '"nop" (instruction-instr (get-full-instruction instr)))
              (setf (instruction-instr (get-full-instruction instr)) '"jmp"))
      )))

;;(get-full-instruction 1)
;;(substitute-instruction 1)
;;(get-full-instruction 1)

(defun run-program (substitute-instr)
  (let (
        (instr-ret 'T)
        )
    (parse-all-instructions "~/Downloads/lisp/input8.txt")
    (setf *accumulator* 0)
    (setf *program-counter* 0)
    (substitute-instruction substitute-instr)
    (loop while (and instr-ret (< *program-counter* (length *all-instructions*))) do
      (progn
        (fetch-instruction *program-counter*)
        ;;(write (format 'nil "PC:~A ACC: ~A ~A"  *program-counter* *accumulator* (nth *program-counter* *all-instructions*)))
        (terpri)
        (cond ((is-jmp) (setq instr-ret (instr-jmp) ))
              ((is-acc) (setq instr-ret (instr-acc) ))
              ((is-nop) (setq instr-ret (instr-nop) ))))
          )
    (substitute-instruction substitute-instr)
    (if substitute-instr
        (if instr-ret
            *accumulator*
            'nil
            )
        *accumulator*
        )
    ))


(defun test-substitute ()
  (let (
        (executed-instructions 'nil)
        (executed-jmp 'nil)
        (executed-nop 'nil)
        (changed-acc 'nil)
        )
    (run-program 'nil)
    (setf executed-instructions (remove 'nil (mapcar (lambda (p) (if (> (instruction-exec-count p) 0) p 'nil)) *all-instructions*)))
    (remove 'nil (mapcar (lambda (x) (run-program (instruction-instr-loc x))) executed-instructions))
    ))

;;answer to part 1
(run-program 'nil)

;;answer to part 2
(test-substitute)

