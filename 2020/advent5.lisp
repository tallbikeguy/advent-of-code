(defpackage :advent5-package
  (:use :cl))

(ql:quickload "uiop")
(ql:quickload :cl-ppcre)
(ql:quickload :lisp-unit)
(ql:quickload :uiop)

;;(in-package :cl-ppcre)
;;(in-package :uiop)

(in-package :advent5-package)

(defun cut-range (low high dir)
  (if (or (string= dir "F") (string= dir "L"))
      (list low (+ low (floor (/ (- high low) 2) )))
      (list (+ 1 (- high (ceiling (/ (- high low) 2)))) high)
  ))

(defun cut-range-l (locrange locstr)
  (cut-range (nth 0 locrange) (nth 1 locrange) locstr))

(lisp-unit:define-test test-cut-range
  (lisp-unit:assert-equal '(0 63) (cut-range 0 127 '"F" ))
  (lisp-unit:assert-equal '(32 63) (cut-range 0 63 '"B" ))
  (lisp-unit:assert-equal '(32 47) (cut-range 32 63 '"F" ))
  (lisp-unit:assert-equal '(40 47) (cut-range 32 47 '"B" ))
  (lisp-unit:assert-equal '(44 47) (cut-range 40 47 '"B" ))
  (lisp-unit:assert-equal '(44 45) (cut-range 44 47 '"F" ))    
  (lisp-unit:assert-equal '(44 44) (cut-range 44 45 '"F" ))    
  (lisp-unit:assert-equal '(64 127)(cut-range 0 127 '"B" ))
  (lisp-unit:assert-equal '(0 63)  (cut-range-l '(0 127) '"F" ))
  (lisp-unit:assert-equal '(32 63) (cut-range-l '(0 63)'"B" ))
  (lisp-unit:assert-equal '(32 47) (cut-range-l '(32 63) '"F" ))
  (lisp-unit:assert-equal '(40 47) (cut-range-l '(32 47) '"B" ))
  (lisp-unit:assert-equal '(44 47) (cut-range-l '(40 47) '"B" ))
  (lisp-unit:assert-equal '(44 45) (cut-range-l '(44 47) '"F" ))    
  (lisp-unit:assert-equal '(44 44) (cut-range-l '(44 45) '"F" ))    
  (lisp-unit:assert-equal '(64 127)(cut-range-l '(0 127) '"B" ))
  (lisp-unit:assert-equal '(4 7) (cut-range 0 7 '"R" ))
  (lisp-unit:assert-equal '(0 3) (cut-range 0 7 '"L" ))
  (lisp-unit:assert-equal '(4 5) (cut-range 4 7 '"L" ))
  (lisp-unit:assert-equal '(5 5) (cut-range 4 5 '"R" ))
  )

  (lisp-unit:run-tests '(test-cut-range))

(defun findrow (locrange locstr)
  (let (
        (loc-lo (nth 0 locrange))
        (loc-hi (nth 1 locrange))
        )
    (if (= loc-lo loc-hi)
        loc-lo
        (findrow (cut-range loc-lo loc-hi (subseq locstr 0 1)) (subseq locstr 1))
        )))

(lisp-unit:define-test test-findrow
  (lisp-unit:assert-equal '44 (findrow '(0 127) '"FBFBBFF"))
  (lisp-unit:assert-equal '5 (findrow '(0 7) '"RLR"))
  )

  (lisp-unit:run-tests '(test-findrow))

(subseq '"BFFFBBFRRR" 0 7)
(subseq '"BFFFBBFRRR" 7 )

(defun findseat (locstr)
  (list (findrow '(0 127) (subseq locstr 0 7)) (findrow '(0 7) (subseq locstr 7))))

(lisp-unit:define-test test-findseat
  (lisp-unit:assert-equal '(70 7) (findseat '"BFFFBBFRRR"))
  (lisp-unit:assert-equal '(14 7) (findseat '"FFFBBBFRRR"))
  (lisp-unit:assert-equal '(102 4) (findseat '"BBFFBBFRLL"))
  )

  (lisp-unit:run-tests '(test-findseat))

(defun seatid (locstr)
  (let ( (seatloc (findseat locstr)))
    (+ (* 8 (nth 0 seatloc)) (nth 1 seatloc))))

(lisp-unit:define-test test-seatid
  (lisp-unit:assert-equal '567 (seatid '"BFFFBBFRRR"))
  (lisp-unit:assert-equal '119 (seatid '"FFFBBBFRRR"))
  (lisp-unit:assert-equal '820 (seatid '"BBFFBBFRLL"))
  )

  (lisp-unit:run-tests '(test-findseat))

(defun get-file (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          while line
          collect (seatid line))))

(get-file '"~/Downloads/input5.txt")

(defun maximum (list)
  (loop for element in list maximizing element))

(defun minimum (list)
  (loop for element in list minimizing element))

(maximum (get-file '"~/Downloads/input5.txt"))
(minimum (get-file '"~/Downloads/input5.txt"))

(defparameter *sortedrecords* (sort (get-file '"~/Downloads/input5.txt") #'<))

(defun minrecord () (nth 0 *sortedrecords*))

(defun maxrecord () (nth (- (length *sortedrecords*) 1) *sortedrecords* ))

(let* (
       (all     (loop for i from (minrecord) to (maxrecord) collect i))
       )
  (set-difference all *sortedrecords* :test '=))


