(defpackage :advent4-package
  (:use :cl))

(ql:quickload "uiop")
(ql:quickload :cl-ppcre)
(ql:quickload :lisp-unit)
(ql:quickload :uiop)

;;(in-package :cl-ppcre)
;;(in-package :uiop)

(in-package :advent4-package)

(defun read-file-as-lines (filename)
  (with-open-file (in filename)
    (loop for line = (read-line in nil nil)
      while line
      collect line)))

(defun flatten (lst)
  (labels ((rflatten (lst1 acc)
             (dolist (el lst1)
               (if (listp el)
                   (setf acc (rflatten el acc))
                   (push el acc)))
             acc))
    (reverse (rflatten lst nil))))

(defun parse-line-r (line)
  (cl-ppcre:all-matches-as-strings "...:[a-zA-Z0-9#]*" line))

(defun line-as-list (line)
  "Read all objects from the line as a list."
  (read-from-string (concatenate 'string "(" line ")")))

(defun r-do-scans (pat line)
  (cl-ppcre:scan-to-strings pat line))

(defun parse-string-r (str)
  (cl-ppcre:split "\:" str) )

;;(parse-string-r "ecl:gry")

;;(split-sequence:SPLIT-SEQUENCE #\: "ecl:gry")

(defun parse-list-r (contents)
  (mapcar #'parse-string-r contents))

(defparameter *allrecords* 
  (mapcar #'parse-line-r (read-file-as-lines "~/Downloads/input4.txt")))

(defparameter *allrecordsn* (reverse (cons 'NIL (reverse *allrecords*))))

(defparameter *allrecordsinlists*
  (loop with sublist
           for el in *allrecordsn*
           if (equal el 'nil )
             collect (nreverse sublist) and do (setf sublist nil)
           else
             do (push el sublist)))

(defun flatparse (lst)
  (mapcar #'parse-string-r (flatten lst)))

(defparameter *allparsedrecords* 
(mapcar #'flatparse *allrecordsinlists*))

(defparameter *firstrecord*
  (car *allparsedrecords*))

;;*firstrecord*

(defun record-keys (therecord)
(mapcar #'car  therecord))

;;(record-keys *firstrecord*)

(defun notnullp (p) (not (null p)))

(defun isitin (eee rec)
  (some #'notnullp
	(mapcar (lambda (str) (string= eee str)) (record-keys rec))))

;;*allparsedrecords*

;;(isitin '"ecl" *firstrecord*)

(defun rec-good-p (rec)
  (every #'notnullp 
	 (mapcar (lambda (key) (isitin key rec))
		 '("ecl" "pid" "eyr" "hcl" "byr" "iyr" "hgt"))))

(defun yearsp (yrstr yrstart yrend)
  (let* (
	 (yr (cl-ppcre:scan-to-strings "^[0-9]{4}$" yrstr) )
	 (yrlen (length yr))
	 (theyear (if (= yrlen 0) 0 (parse-integer yr)))
	 (result (if (= theyear 0) 0 (and (>= theyear yrstart)(<= theyear yrend))))
	 )
    result ))


(defun byrp (yrstr)
  ;;byr (Birth Year) - four digits; at least 1920 and at most 2002.
  (yearsp yrstr '1920 '2002))

(defun iyrp (yrstr)
  ;;iyr (Issue Year) - four digits; at least 2010 and at most 2020.
  (yearsp yrstr '2010 '2020))

(defun eyrp (yrstr)
  ;;eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
  (yearsp yrstr '2020 '2030))


(lisp-unit:define-test test-all-yrp
  (lisp-unit:assert-false (byrp '"1919") )
  (lisp-unit:assert-true (byrp '"1920") )
  (lisp-unit:assert-true (byrp '"2002") )
  (lisp-unit:assert-false (byrp '"2003") )

  (lisp-unit:assert-false (iyrp '"2009") )
  (lisp-unit:assert-true (iyrp '"2010") )
  (lisp-unit:assert-true (iyrp '"2020") )
  (lisp-unit:assert-false (iyrp '"2021") )

  (lisp-unit:assert-false (eyrp '"2019") )
  (lisp-unit:assert-true (eyrp '"2020") )
  (lisp-unit:assert-true (eyrp '"2030") )
  (lisp-unit:assert-false (eyrp '"2031") )
  )

(lisp-unit:run-tests '(test-all-yrp))

(defun hgtp (hgtstr)
  (let* (
	     (astr nil)
	     (bstr nil)
	     (valint nil)
	     )	  
    (setf (values astr bstr)
	  (cl-ppcre:scan-to-strings "^([0-9]+)(cm)|([0-9]+)(in)$" hgtstr))
    (if astr
	(progn (if (svref bstr 0)
	    (setf valint (parse-integer (svref bstr 0)))
	    (setf valint (parse-integer (svref bstr 2)))
	    )
	(if (svref bstr 0)
	    (if (and (>= valint 150) (<= valint 193)) valint 'nil)
	    (if (and (>= valint 59) (<= valint 76)) valint 'nil)
	    )
	)
	'nil)))

 (lisp-unit:define-test test-hgtp
   (lisp-unit:assert-equal 'nil (hgtp '"20in") )
   (lisp-unit:assert-equal '59 (hgtp '"59in") )
   (lisp-unit:assert-equal '76 (hgtp '"76in") )
   (lisp-unit:assert-equal 'nil (hgtp '"77in") )
   (lisp-unit:assert-equal 'nil (hgtp '"149cm") )
   (lisp-unit:assert-equal '150 (hgtp '"150cm") )
   (lisp-unit:assert-equal '193 (hgtp '"193cm") )
   (lisp-unit:assert-equal 'nil (hgtp '"194cm") )
   (lisp-unit:assert-equal 'nil (hgtp '"x149cm") )   
   )

(lisp-unit:run-tests '(test-hgtp))

(defun eclp (eystr)
  ;;ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
  (= 3 (length (cl-ppcre:scan-to-strings "^(amb|blu|brn|gry|grn|hzl|oth)$" eystr))))

(lisp-unit:define-test test-eclp
  (lisp-unit:assert-true (eclp '"amb"))
  (lisp-unit:assert-true  (eclp '"blu"))
  (lisp-unit:assert-true  (eclp '"brn"))
  (lisp-unit:assert-true  (eclp '"gry"))
  (lisp-unit:assert-true  (eclp '"grn"))
  (lisp-unit:assert-true  (eclp '"hzl"))
  (lisp-unit:assert-true  (eclp '"oth"))
  (lisp-unit:assert-false (eclp '"amx"))
  (lisp-unit:assert-false  (eclp '"xxbluxx"))
  )

(lisp-unit:run-tests '(test-eclp))

(defun hclp (hclstr)
  ;;hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
  (= 7 (length (cl-ppcre:scan-to-strings "^#([0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f])$" hclstr))))

(lisp-unit:define-test test-hclp
  (lisp-unit:assert-true (hclp '"#123abc"))
  (lisp-unit:assert-false (hclp '"#123abz"))
  (lisp-unit:assert-false (hclp '"123abc"))  
  )

(lisp-unit:run-tests '(test-hclp))


(defun pidp (pidstr)
  ;;pid (Passport ID) - a nine-digit number, including leading zeroes.
  (= 9 (length (cl-ppcre:scan-to-strings "^([0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9])$" pidstr))))

(lisp-unit:define-test test-pidp
  (lisp-unit:assert-true (pidp '"123456789"))
  (lisp-unit:assert-true (pidp '"000000001"))
  (lisp-unit:assert-false (pidp '"00000001"))
  (lisp-unit:assert-false (pidp '"00000000x"))
  (lisp-unit:assert-false (pidp '"0000000011"))
  )

(lisp-unit:run-tests '(test-pidp))

(lisp-unit:run-tests :all)

(defun cidp (cidstr)
  'T )

(defun findvalid (rec val)
  (string-equal (car rec) (car val)))

(defun valid-field-r (rec)
  (let* (
         (keyval (nth 0 rec))
         (dataval (nth 1 rec))
         )
    (cond ((string-equal "byr" keyval) (byrp dataval))
          ((string-equal "iyr" keyval) (iyrp dataval))
          ((string-equal "eyr" keyval) (eyrp dataval))
          ((string-equal "hgt" keyval) (hgtp dataval))
          ((string-equal "hcl" keyval) (hclp dataval))
          ((string-equal "ecl" keyval) (eclp dataval))
          ((string-equal "pid" keyval) (pidp dataval))
          ((string-equal "cid" keyval) (cidp dataval))
          )))
         
(VALID-FIELD-r '("hgt" "189cm"))

(defun rec-good-p2 (rec)
  (if (rec-good-p rec)
      (every #'notnullp  (mapcar #'valid-field-r rec))
      'nil
  ))


(defun times (list element)
  (cond
    ((null list) 0)
    ((equal (car list) element) (1+ (times (cdr list) element)))
    (t (times (cdr list) element))))

(times (mapcar #'rec-good-p *allparsedrecords*) 'T)

(times (mapcar #'rec-good-p2 *allparsedrecords*) 'T)


