(defpackage :advent7-package
  (:use :cl))

(ql:quickload "uiop")
(ql:quickload :cl-ppcre)
(ql:quickload :lisp-unit)
(ql:quickload :uiop)

;;(in-package :cl-ppcre)
;;(in-package :uiop)

(in-package :advent7-package)




(defun flatten (lst)
  (labels ((rflatten (lst1 acc)
             (dolist (el lst1)
               (if (listp el)
                   (setf acc (rflatten el acc))
                   (push el acc)))
             acc))
    (reverse (rflatten lst nil))))


(defun read-file-as-lines (filename)
  (with-open-file (in filename)
    (loop for line = (read-line in nil nil)
          while line
          collect line)))

(defparameter *allrecords*
  (read-file-as-lines "~/Downloads/input7.txt"))

(defun proc-bag-string (bagstring)
  (let* (
         (thestring bagstring)
         (containing nil)
         )
    (write "initial string: ")
    (write thestring)
    (terpri)
    (if (cl-ppcre:scan "contain no other bags." thestring)
        (setq thestring 'nil)
        (progn 
         (setq thestring (cl-ppcre:regex-replace-all ", " thestring "\")(def-contained "))
         (setq thestring (cl-ppcre:regex-replace-all "^" thestring "(def-containing \'\""))
         (setq thestring (cl-ppcre:regex-replace-all " contain " thestring "\" (list (def-contained "))
         (setq thestring (cl-ppcre:regex-replace-all ".$" thestring "\")))"))
         (setq thestring (cl-ppcre:regex-replace-all "bags|bag" thestring ""))
         (setq thestring (cl-ppcre:regex-replace-all " \"" thestring "\""))
         (setq thestring (cl-ppcre:regex-replace-all " [0-9] " thestring " \\& \""))
    ;;(setq thestring (cl-ppcre:regex-replace-all "def-contained no other\"" thestring ""))
    ;;(setq thestring (cl-ppcre:regex-replace-all "(list ())" thestring ""))
    ;;(setq thestring (cl-ppcre:regex-replace-all "\050\050\051\051\051" thestring 051"))
         (write "final string: ")
         (write thestring)
         (terpri)
         ))
    thestring))

(cl-ppcre:scan "contain no other bags." (nth 236 *allrecords*) )

(cl-ppcre:regex-replace-all " contain no other bags" (nth 236 *allrecords*) "")
(proc-bag-string (nth 236 *allrecords*) )
(proc-bag-string (nth 0 *allrecords*) )
;;    (setq thestring (cl-ppcre:regex-replace-all "(list (def-contained no other\"))" thestring " \'nil"))
;;(def-containing '"dark tomato" 'nil)

;;    (setq thestring (cl-ppcre:regex-replace-all " contain no other bags." thestring ""))



(defparameter *recs-this-time* 'nil)

(defun def-containing (bagstr containedbags)
  (progn
    ;(write (format nil "containing ~A : ~A" bagstr containedbags))
    (mapcar (lambda (p) (push (list bagstr p) *recs-this-time*)) containedbags)
    ))

(defun def-contained (nn bagstr)
  (progn
    (write (format nil "contained ~A" bagstr))
    (terpri)
    (list bagstr nn) ))

(defun eval-bag-string (bagstring)
  (let (
        (mystring nil)
        )
    (setq mystring (proc-bag-string bagstring))
    (if mystring
      (eval (read-from-string mystring))
      'nil)))



(setq *recs-this-time* 'nil)
(mapcar  #'eval-bag-string *allrecords*)

(length *recs-this-time*)

(defun matchp (var list)
  (string= var (car (nth 1 list))))

(string= "dotted plum" (car (nth 1 (nth 0 *recs-this-time*))))
(nth 1 *recs-this-time*)

(matchp "shiny gold" (nth 0 *recs-this-time*))
(matchp "dotted plum" (nth 0 *recs-this-time*))
(matchp "bright white" (nth 4 *recs-this-time*))

(defun getcontaining (bagstr thelist)
  (if (matchp bagstr thelist)
      (car thelist)
      'nil))

(defun containingbags (bagstr)
  (remove 'nil (mapcar (lambda (p) (getcontaining bagstr p)) *recs-this-time*)))

(containingbags "dotted plum")

(defun concat-lists (seq1 seq2)
  (if (null seq1)
      seq2
      (cons (car seq1) (concat-lists (cdr seq1) seq2))))

(defun mapcontaining (a)
  (if  a
       (let ((foolist (flatten (mapcar #'containingbags a))))
         (concat-lists foolist (mapcontaining foolist))
         )
       'nil
       ))
    
(length (remove-duplicates (flatten (mapcontaining '("shiny gold")))))

(defun matchconp (var list)
  (if (string= var (car list))
      (car (cdr list))
      'nil)
  )

(matchconp "clear gray" (nth 0 *recs-this-time*))
(matchconp "clear gray" (nth 1 *recs-this-time*))

(defun getcontained (bagstr)
  (let (
        (mybagstr (if (listp bagstr) (car bagstr) bagstr))
        )
  (remove 'nil (mapcar (lambda (p) (matchconp mybagstr p)) *recs-this-time*))))

(defun do-inden (inden)
  (let ((mystr '""))
  (dotimes (i inden)
    (setq mystr (concatenate 'string mystr '" ")))
  (write mystr)))

(do-inden 3)

(defun wrap-contained-count (bagstr inden)
  (let ((retval nil))
    (do-inden inden) (write (format nil "contained-count: ~A" bagstr))
    (terpri)
    (setq retval (contained-count bagstr inden))
    (do-inden inden)(write (format nil "retval: ~A" retval))
    (terpri)
    retval
    )
  )


(defun contained-count (bagstr inden)
  ;;for a given type of bag
  ;;if the passed value is a single bag, find all contained (and sub-contained) bag counts
  ;;add them up
  (let ((templist nil)(retval 0))
    (if (not bagstr)
        0
        (if (listp bagstr)
            (if (and (= 2 (length bagstr))(stringp (nth 0 bagstr)) (integerp (nth 1 bagstr)) )
                (progn
                  (setq templist (getcontained bagstr))
                  (if (= (length templist) 0)
                      (nth 1 bagstr)
                      (* (nth 1 bagstr)(+ 1 (reduce '+ (mapcar (lambda (cc) (wrap-contained-count cc (+ inden 2))) templist))))))
                  )
                0
                )
            )))

(stringp '"foo")
(numberp '9)
(integerp '9)
(integerp 'nil)

(contained-count 'nil)
(contained-count "shiny gold")
(contained-count '("shiny gold" 1) 0)
(contained-count '("vibrant orange" 3) 0)
(contained-count '(("foo" 3)("bar" 4)))

(contained-count '("bright tomato" 1) 0)
(contained-count '("posh green" 1) 0)
(contained-count '("muted gray" 1) 0)

(wrap-contained-count '("dark violet" 1) 0)
(wrap-contained-count '("dark blue" 1) 0)
(wrap-contained-count '("dark green" 1) 0)
(wrap-contained-count '("dark yellow" 1) 0)

(defun getcontained (bagstr)
  (let (
        (mybagstr (if (listp bagstr) (car bagstr) bagstr))
        )
  (remove 'nil (mapcar (lambda (p) (matchconp mybagstr p)) *recs-this-time*))))

(getcontained '("shiny gold" 3))

(defun containedlevel (baglist)
  (if (listp baglist)
      ;;(append baglist (mapcar #'getcontained baglist))
      (append baglist (mapcar #'containedlevel baglist))
      (list baglist (getcontained baglist))
      ))
(trace containedlevel)
(containedlevel '"shiny gold")
(containedlevel '("shiny gold"))
(containedlevel '("bright tomato"))
(containedlevel '"shiny gold")
(containedlevel '"bright tomato")
(containedlevel '"muted gray")
(containedlevel '"posh green")
(containedlevel '"vibrant red")
(containedlevel '"dark tomato")
(containedlevel '"wavy gray")
(containedlevel '"vibrant orange")

(containedlevel '"shiny gold")
(containedlevel '"dark red")
(containedlevel '"dark orange")
(containedlevel '"dark yellow")
(containedlevel '"dark green")
(containedlevel '"dark blue")
(containedlevel '"dark violet")

(defun getcontained (bagstr thelist)
  (if (matchp bagstr thelist)
      (car thelist)
      'nil))

(defun containedbags (bagstr)
  (remove 'nil (mapcar (lambda (p) (getcontained bagstr p)) *recs-this-time*)))

(
