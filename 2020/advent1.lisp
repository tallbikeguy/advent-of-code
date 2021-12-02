;;---------------------advent1-----------------------
(defun flatten (lst)
  (labels ((rflatten (lst1 acc)
             (dolist (el lst1)
               (if (listp el)
                   (setf acc (rflatten el acc))
                   (push el acc)))
             acc))
    (reverse (rflatten lst nil))))

(defun get-file (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          while line
          collect (parse-integer line))))

(defun filter-eq (mynum mylist)
  (remove-if (lambda (num) (/= mynum num)) mylist))

(defun findsum (mysum mylist)
  (mapcar (lambda (num) (filter-eq (- mysum num) mylist)) mylist))

(defun advent-contest-1 (filename)
  (let ((mylist (remove-if #'NULL (findsum 2020 (get-file filename)))))
    (* (car(car mylist)) (car (car (cdr mylist))))))

(advent-contest-1 "/Users/ross/Downloads/input.txt")

(defun advent-contest-2 (myvalue inputlist)
  (let* ( (mylist     (remove-if #'NULL (findsum myvalue inputlist))))
    (remove nil (list (car (car mylist)) (car (car (cdr mylist)))))
    )
  )


(advent-contest-2 5 (get-file "/Users/ross/Downloads/input.txt"))

(remove nil (advent-contest-2 (- 2020 1895) '(1895 1732 1660)))

(defun advent-contest-3 (inputlist)
  (remove-if #'NULL (mapcar (lambda (num) (advent-contest-2 (- 2020 num) inputlist) ) inputlist)))

(remove-duplicates (flatten (advent-contest-3 (get-file "/Users/ross/Downloads/input.txt"))))

;;---------------------advent2-----------------------
(defun get-advent2-file (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          while line
          collect  line)))
  
(get-advent2-file "/Users/ross/Downloads/input2.txt")

(ql:quickload :uiop)

(ql:quickload :cl-ppcre)
(asdf:oos 'asdf:test-op :cl-ppcre)

(in-package :cl-ppcre-test)
(setq *allow-named-registers* t)
(scan-to-strings "[^b]*b" "aaabd")
(scan-to-strings "^(?<min>[0-9]+)-(?<max>[0-9]+) (?<char>.)[:] (?<passwd>.+)" "1-4 j: jjjqzmgbjwpj")

(register-groups-bind (min max char passwd)
    ("^(?<min>[0-9]+)-(?<max>[0-9]+) (?<char>.)[:] (?<passwd>.+)" "1-4 j: jjjqzmgbjwpj")
  (list min max char passwd))

(defun parse-password (line)
   (scan-to-strings "^(?<min>[0-9]+)-(?<max>[0-9]+) (?<char>.)[:] (?<passwd>.+)" line))

(defun parse-password-r (line)
  (register-groups-bind (min max char passwd)
      ("^(?<min>[0-9]+)-(?<max>[0-9]+) (?<char>.)[:] (?<passwd>.+)" line)
  (list (parse-integer min) (parse-integer max) char passwd)))

(defparameter *passwordlines*
  (mapcar #'parse-password-r
	  (uiop:read-file-lines "/Users/ross/downloads/input2.txt")))

(parse-password-r "1-4 j: jjjqzmgbjwpj")

(defun validpassp (pmin pmax pchar pass)
  (let ((ccount (/ (length (all-matches pchar pass)) 2)))
    (and (>= ccount pmin) (<= ccount pmax))))

(defun tchar (string pos)
  (char string (- pos 1)))

(defun xor (a b)
  (and (or a b)
       (not(and a b))))

(defun validpass2p (pmin pmax pchar pass)
  (xor (string= (string(tchar pass pmin)) pchar)
      (string= (string(tchar pass pmax)) pchar)))

(defun validpasslinep (line)
  (destructuring-bind (a b c d) line
    (validpassp a b c d)))

(defun validpassline2p (line)
  (destructuring-bind (a b c d) line
    (validpass2p a b c d)))


(length (remove-if #'NULL (loop for passwordline in *passwordlines* 
	collect (validpasslinep passwordline))))

(length (remove-if #'NULL (loop for passwordline in *passwordlines* 
	collect (validpassline2p passwordline))))

(print (count-if (lambda (val) val) (loop for passwordline in *passwordlines*
                       collect (validpassline2p passwordline))))

;;------------------------advent 3------------------------
(defparameter *maplines*
  (uiop:read-file-lines "/Users/ross/downloads/input3.txt"))

(defun mylinesval (x y)
  (char (nth y *maplines*) x))

(defun lineslen () (length (car *maplines*)))

(defun adventlineval (x y)
  (let ((myx (mod x (lineslen))))
    (mylinesval myx y)))

(defun locx (myloc)
  (nth 0 myloc))

(defun locy (myloc)
  (nth 1 myloc))

(defun istreep (myloc)
  (char= #\# (adventlineval (locx myloc) (locy myloc)))) 

(defun modstep (xylist slope)
  (list (+ (locx xylist) (locx slope)) (+ (locy xylist) (locy slope))))

(defun boundsp (theloc)
  (< (nth 1 theloc) (length *maplines*)))

(defun advent3 (myloc slope)
  (if (boundsp myloc)
      (+ (advent3 (modstep myloc slope) slope) (if (istreep myloc) 1 0) )
      0))

(advent3 '(0 0) '(3 1))

(* (advent3 '(0 0) '(1 1))
   (advent3 '(0 0) '(3 1))
   (advent3 '(0 0) '(5 1))
   (advent3 '(0 0) '(7 1))
   (advent3 '(0 0) '(1 2)))


;;----------------
(defparameter *map* (uiop:read-file-lines "/Users/ross/downloads/input3.txt"))
(defparameter *map-width* (length (car *map*)))
(defparameter *map-length* (length *map*))

;; A straight-forward way to complete first task
(print (count t
	      (loop for line in *map* collect
				      (eql #\# (elt line
						    (rem (* 3 (position line *map*))
							 *map-width*))))))


;; A more mathematical solution for the rest...
(defun travel (curr-x curr-y x-dir y-dir)
  (if (< curr-y *map-length*)
      (+ (travel (+ curr-x x-dir) (+ curr-y y-dir) x-dir y-dir)
	 (if (eql #\# (elt (nth curr-y *map*) (rem curr-x *map-width*)))
	     1
	     0))
      0))

(print (reduce #'* (loop for slope in '((1 . 1) (3 . 1) (5 . 1) (7 . 1) (1 . 2))
			 collect (travel 0 0 (car slope) (cdr slope)))))


(car '(1 . 1))
(cdr '(1 . 1))
(car '(1 1))
(cdr '(1 1))
