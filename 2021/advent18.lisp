(ql:quickload 'iterate)
(ql:quickload 'series)
(ql:quickload 'cl-ppcre)
(ql:quickload 'lisp-unit)
(defpackage :advent21-18 (:use :cl :uiop :iterate :cl-ppcre :lisp-unit))
(in-package :advent21-18)

(defun parse-line (line)
  (let ((l line))
    (setf l (cl-ppcre:regex-replace-all "\\]" l ")"))
    (setf l (cl-ppcre:regex-replace-all "\\["  l "("))
    (setf l (cl-ppcre:regex-replace-all "\\,"  l " "))
    (setf l (cl-ppcre:regex-replace-all "^"  l "'"))
    (eval (read-from-string l))))

(defun snailstr-to-list (str)
  (eval (read-from-string (format 'nil "'~a" (parse-line str)))))

(defun list-to-snailstr (list)
  (let ((l  (format 'nil "~a" list)))
    (setf l (cl-ppcre:regex-replace-all "\\)" l "]"))
    (setf l (cl-ppcre:regex-replace-all "\\("  l "["))
    (setf l (cl-ppcre:regex-replace-all "\\ "  l ","))
  l))

(lisp-unit:define-test inputs
  (let ((str "[[[9,[3,8]],[[0,9],6]],[[[3,7],[4,9]],3]]"))
  (lisp-unit:assert-true (string=  (list-to-snailstr (snailstr-to-list str)) str)))
  )

(lisp-unit:run-tests '(inputs))

(defun flatten (lst)
  (labels ((rflatten (lst1 acc)
             (dolist (el lst1)
               (if (listp el)
                   (setf acc (rflatten el acc))
                   (push el acc)))
             acc))
    (reverse (rflatten lst nil))))


(defun split-num (n &optional v)
  (if *is-split* (values n 'nil)
      (if (>= n 10)
          (progn
            (setf *is-split* 'T)
            (values (list (floor n 2) (ceiling n 2)) 'T))
          (values n 'nil))))

(defparameter *is-split* 'nil)
(defun snail-split2 (l)
  (if (numberp l)
      (split-num l *is-split*) 
      (mapcar #'snail-split2  l)))

(defun snail-split (l)
  (progn
    (setf *is-split* 'nil)
    (values (snail-split2 l) *is-split*)))


;(snail-split '(10))
;(snail-split (parse-line "[[[[0,7],4],[15,[0,13]]],[1,1]]"))


(define-test splits
  (assert-equal '(1) (multiple-value-bind (x y)(snail-split '(1)) x))
  (assert-equal 'nil (multiple-value-bind (x y)(snail-split '(1)) y))
  (assert-equal '((5 5)) (multiple-value-bind (x y)(snail-split '(10)) x))
  (assert-equal 't (multiple-value-bind (x y)(snail-split '(10)) y))  
  (assert-equal '((5 6)) (multiple-value-bind (x y)(snail-split '(11)) x))
  (assert-equal 't (multiple-value-bind (x y)(snail-split '(11)) y))  
  )

(run-tests '(splits))

(defparameter *is-explode* 'nil)

(defun simple-pair-p (l)
  (and (listp l) (= (length l) 2) (numberp (car l)) (numberp (cadr l))))

(defun simple-list-p (l)
  (and (listp l) (= (length l) 1) (numberp (car l))))

(define-test simple-pairs
  (assert-equal 'nil (simple-pair-p '5))
  (assert-equal 'nil (simple-pair-p '(5)))
  (assert-equal 't (simple-pair-p '(5 6)))
  (assert-equal '((5 6)) (multiple-value-bind (x y)(snail-split '(11)) x))
  (assert-equal 'nil (simple-pair-p '(5 (6 7))))
  )

(run-tests '(simple-pairs))

(defun maximum (list)  (loop for element in list maximizing element))

(defun max-depth (list)
  (if (not list)
      0
      (if (atom list)
          0
          (if (listp list) (1+ (maximum (mapcar #'max-depth list)))))))

;(max-depth 0)
;(max-depth '())
;(max-depth '(1))
;(max-depth '((1)))
;(max-depth '(((1))))
;(max-depth '((((1) 2)3 )4 ) )
;(max-depth '(((((1) 2)3 )4 ) 5) )
                  

(defparameter *is-explode* 'nil)
(defun snail-ex (in depth)
  (if *is-explode*
      in
      (if (not in)
          'nil
          (if (numberp in)
              in
              (if (simple-list-p in)
                  in
                  (if (simple-pair-p in)
                      (if (and (>= depth 4) (not *is-explode*))
                          (progn
                            (setf *is-explode* 'T)
                            (list (format 'nil "replaceleft~axx" (car in)) (format 'nil "replaceright~axx" (cadr in)))
                            )
                          (remove 'nil (list (snail-ex (car in) (1+ depth)) (snail-ex (cadr in) (1+ depth)))))
                      (remove 'nil (list (snail-ex (car in) (1+ depth)) (snail-ex (cadr in) (1+ depth))))))))))


;(trace snail-explode2)
;(trace snail-ex)

;(snail-ex '0  1)
;(snail-ex '10 1)
;(snail-ex '( 0 ) 1)
;(snail-ex '(1 2 ) 1)
;(snail-ex '(1 (2 3)) 1)
;(snail-ex '(2 (3 4)) 1)          
;(snail-ex '(1 (2 (3 4))) 1)
;(snail-ex '((1 (2 (3 (4 5)))))  1)
;*extest*

(defun snail-explode-0 (list)
  (progn
    (setf *is-explode* 'nil)
    (values (snail-ex list 0) *is-explode*)))
  
;(scan "([0-9]+)" "( 3( 2( 1(")

(define-test explode1
  (assert-equal '(0) (multiple-value-bind (x y)(snail-explode-0 '( 0) ) x))
  (assert-equal 'nil (multiple-value-bind (x y)(snail-explode-0 '( 0) ) y))
  (assert-equal '(10) (multiple-value-bind (x y)(snail-explode-0 '( 10 ) ) x))
  (assert-equal 'nil (multiple-value-bind (x y)(snail-explode-0 '( 10) ) y))
  (assert-equal '(1 2) (multiple-value-bind (x y)(snail-explode-0 '(1 2) ) x))
  (assert-equal 'nil (multiple-value-bind (x y)(snail-explode-0 '( 1 2) ) y))
  (assert-equal '(1 (2 3)) (multiple-value-bind (x y)(snail-explode-0 '( 1 (2 3)) ) x))
  (assert-equal 'nil (multiple-value-bind (x y)(snail-explode-0 '( 1 (2 3)) ) y))
  (assert-equal '(1 (2 (3 4))) (multiple-value-bind (x y)(snail-explode-0 '( 1 (2 (3 4))) ) x))
  (assert-equal 'nil (multiple-value-bind (x y)(snail-explode-0 '( 1 (2 (3 4))) ) y))
  (assert-equal '(1 (2 (3 (4 5)))) (multiple-value-bind (x y)(snail-explode-0 '( 1 (2 (3 (4 5))))) x))
  (assert-equal 'nil (multiple-value-bind (x y)(snail-explode-0 '( 1 (2 (3 (4 5)))) ) y))
  (assert-equal '(1 (2 (3 (4 ("replaceleft5xx" "replaceright6xx"))))) (multiple-value-bind (x y)(snail-explode-0 '( 1 (2 (3 (4 (5 6)))))) x))
  (assert-equal 't (multiple-value-bind (x y)(snail-explode-0 '( 1 (2 (3 (4 (5 6))))) ) y))
  (assert-equal '((((("replaceleft6xx" "replaceright5xx") 4) 3) 2) 1) (multiple-value-bind (x y)(snail-explode-0 '(  ( ( ( (6 5) 4) 3) 2) 1)) x))
  (assert-equal 't (multiple-value-bind (x y)(snail-explode-0 '(  ( ( ( (6 5) 4) 3) 2) 1)) y))  
  )
(run-tests '(explode1))

;*exteststr*

(defun exleft (the-str)
  (let ((leftvar 'nil)
        (leftval 'nil)
        (tempstr 'nil)
        (valstr 'nil)
        (result 'nil))
    (multiple-value-bind (rs re vs ve) (cl-ppcre:scan "replaceleft([0-9]+)xx" the-str)
      (setf leftvar (parse-integer (subseq the-str (svref vs 0) (svref ve 0))))
      (setf tempstr (reverse (subseq the-str 0 (1- rs))))
      ;;(format t "t1: tempstr: ~a~%" tempstr)
      (multiple-value-bind (as ae avs ave) (scan "([0-9]+)" tempstr)
        (if as (let* (( valstr (reverse (subseq tempstr (svref avs 0) (svref ave 0))))
                      (val (parse-integer valstr)))
                 (setf leftval (+ val leftvar))    
                 (setf result (reverse (format 'nil "~a~a~a" (subseq tempstr 0 as) (reverse (format 'nil "~a" leftval)) (subseq tempstr ae))) )
                 )
            (setf result tempstr))
      result
      ))))

(exleft '"((((12 12) (6 14)) ((15 0) (17 (replaceleft8xx replaceright1xx)))) (2 9))" )



(defun exright (the-str)
  (let ((rightvar 'nil)
        (rightval 'nil)
        (tempstr 'nil)
        (valstr 'nil)
        (result 'nil))
    (multiple-value-bind (rs re vs ve) (cl-ppcre:scan "replaceright([0-9]+)xx" the-str)
      (setf rightvar (parse-integer (subseq the-str (svref vs 0) (svref ve 0))))
      (setf tempstr (subseq (subseq the-str re) 1))
      ;;(format t "t2: ~a~%" tempstr)  
      (multiple-value-bind (as ae avs ave) (scan "([0-9]+)" tempstr)
        (if as (progn
                 (setf rightval (parse-integer (subseq tempstr (svref avs 0) (svref ave 0))))    
                 (setf result  (format 'nil "~a~a~a" (subseq tempstr 0 as) (+ rightvar rightval) (subseq tempstr ae)))
                 )
            (progn
              (setf result tempstr)
              )
            )
        result
        ))))

(defun explod (list)
  (multiple-value-bind (extest did-explode) (snail-explode-0 list)
    (if did-explode
        (values (let ((extextstr (format 'nil "~a" extest))
                      (result 'nil))
                  ;(format t "calling (exleft ' ~a )~%" extextstr)
                  ;(format t "return val ~a~%" (exleft extextstr))
                  ;(format t "calling (exright ' ~a )~%" extextstr)
                  ;(format t "return val ~a~%" (exright extextstr))
                  (setf result (eval (read-from-string (format 'nil "'~a~a~a" (exleft extextstr) 0  (exright extextstr)))))
                  ;(format t "result val ~a~%" result)
                  result) 'T)
        (values list 'nil))))


(defparameter *is-explode* 'nil)
;(snail-ex '((1 (2 (3 (4 5)))) 6)  1)

;(write (read-from-string (explod '((1 (2 (3 (4 5)))) 6)  )))

;(eval (read-from-string "'((1 (2 (7 (0)))) 11)"))
;(eval (read-from-string "'((1 (2 (7 0))) 11)"))
;(eval (read-from-string "'(1 11)"))
;(explod '((1 (2 3)) 6)  )
;(explod '((1 (2 (3 (4 5)))) 6)  )
;  (explod '((1 (2 (3 (4 5)))))  )
;  (explo-test '((1 (2 (3 (4 5)))))  )


;;from the instructions


(define-test explode
  (assert-true (string= (list-to-snailstr (explod (snailstr-to-list "[[[[[9,8],1],2],3],4])"))) "[[[[0,9],2],3],4]"))
  (assert-true (string= (list-to-snailstr (explod (snailstr-to-list "[7,[6,[5,[4,[3,2]]]]]"))) "[7,[6,[5,[7,0]]]]"))
  (assert-true (string= (list-to-snailstr (explod (snailstr-to-list "[[6,[5,[4,[3,2]]]],1]"))) "[[6,[5,[7,0]]],3]"))
  (assert-true (string= (list-to-snailstr (explod (snailstr-to-list "[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]"))) "[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]"))
  (assert-true (string= (list-to-snailstr (explod (snailstr-to-list "[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]"))) "[[3,[2,[8,0]]],[9,[5,[7,0]]]]"))
  (assert-true (string= (list-to-snailstr
                         (explod '((((7 8) (6 7)) ((6 8) (0 8))) ((9 ((5 6) 0)) ((6 8) (15 0))))))
                        (list-to-snailstr
                                 '((((7 8) (6 7)) ((6 8) (0 8))) ((14 (0 6)) ((6 8) (15 0)))))))

  
  )
(run-tests '(explode))


(define-test explode-and-split-reddit
  (assert-true (string= (list-to-snailstr (explod (snailstr-to-list "[[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]],[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]]"))) "[[[[4,0],[5,0]],[[[4,5],[2,6]],[9,5]]],[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]]"))

  (assert-true (string= (list-to-snailstr (explod (snailstr-to-list "[[[[12,12],[6,14]],[[15,0],[17,[8,1]]]],[2,9]]"))) "[[[[12,12],[6,14]],[[15,0],[25,0]]],[3,9]]"))
  )

(run-tests '(explode-and-split-reddit))
  



(defun snail-reduce (list)
  (let ((keep-reducing 'T)
        (accum list))
    ;(format t "reducing ~a~%" list)
    (loop while keep-reducing
          :do (progn
                (setf keep-reducing 'nil)
                (multiple-value-bind (v e) (explod accum)
                  (if e
                      (progn
                        ;(format t "after explode ~a~%" v)
                        (setf keep-reducing 'T)
                        (setf accum v))
                      (multiple-value-bind (v e) (snail-split accum)
                        (if e (progn
                                ;(format t "after split   ~a~%" v)
                                (setf keep-reducing 'T)
                                (setf accum v))))))))
    accum))

(defun snail+ (a b)
  (snail-reduce (list a b)))


(string= (list-to-snailstr (snail-reduce (snailstr-to-list "[[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]]")))   "[[[[0,7],4],[[7,8],[6,0]]],[8,1]]")

(string= (list-to-snailstr (snail+ (snailstr-to-list "[[[[4,3],4],4],[7,[[8,4],9]]]" ) (snailstr-to-list "[1,1]")))   "[[[[0,7],4],[[7,8],[6,0]]],[8,1]]")

(define-test reduce1
  (defparameter *s1* (snailstr-to-list "[[[[4,3],4],4],[7,[[8,4],9]]]"))
  (defparameter *s1a* (list *s1* (snailstr-to-list "[1,1]")))
  (assert-true (string= (list-to-snailstr *s1a*) "[[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]]"))
  (defparameter *s2* (explod *s1a*))
  (assert-true (string= (list-to-snailstr *s2*) "[[[[0,7],4],[7,[[8,4],9]]],[1,1]]"))
  (defparameter *s3* (explod *s2*))          
  (assert-true (string= (list-to-snailstr *s3*) "[[[[0,7],4],[15,[0,13]]],[1,1]]"))
  (defparameter *s4* (snail-split *s3*))
  (assert-true (string= (list-to-snailstr *s4*) "[[[[0,7],4],[[7,8],[0,13]]],[1,1]]"))
  (defparameter *s5* (snail-split *s4*))
  (assert-true (string= (list-to-snailstr *s5*) "[[[[0,7],4],[[7,8],[0,[6,7]]]],[1,1]]"))
  (defparameter *s6* (explod *s5*))          
  (assert-true (string= (list-to-snailstr *s6*) "[[[[0,7],4],[[7,8],[6,0]]],[8,1]]"))

  (assert-true (string= (list-to-snailstr (snail-reduce (snailstr-to-list "[[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]],[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]]")))
                        "[[[[4,0],[5,4]],[[7,7],[6,0]]],[[8,[7,7]],[[7,9],[5,0]]]]"))
  (assert-true (string= (list-to-snailstr (snail-reduce (snailstr-to-list "[[[[[4,0],[5,4]],[[7,7],[6,0]]],[[8,[7,7]],[[7,9],[5,0]]]],[[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]]]")))
                        "[[[[6,7],[6,7]],[[7,7],[0,7]]],[[[8,7],[7,7]],[[8,8],[8,0]]]]"))

  (assert-true (string= (list-to-snailstr (snail-reduce (snailstr-to-list "[[[[[6,7],[6,7]],[[7,7],[0,7]]],[[[8,7],[7,7]],[[8,8],[8,0]]]],[[[[2,4],7],[6,[0,5]]],[[[6,8],[2,8]],[[2,1],[4,5]]]]]")))
                        "[[[[7,0],[7,7]],[[7,7],[7,8]]],[[[7,7],[8,8]],[[7,7],[8,7]]]]"))
  (assert-true (string= (list-to-snailstr (snail-reduce (snailstr-to-list "[[[[[7,0],[7,7]],[[7,7],[7,8]]],[[[7,7],[8,8]],[[7,7],[8,7]]]],[7,[5,[[3,8],[1,4]]]]]")))
                        "[[[[7,7],[7,8]],[[9,5],[8,7]]],[[[6,8],[0,8]],[[9,9],[9,0]]]]"))
  (assert-true (string= (list-to-snailstr (snail-reduce (snailstr-to-list "[[[[[7,7],[7,8]],[[9,5],[8,7]]],[[[6,8],[0,8]],[[9,9],[9,0]]]],[[2,[2,2]],[8,[8,1]]]]")))
                        "[[[[6,6],[6,6]],[[6,0],[6,7]]],[[[7,7],[8,9]],[8,[8,1]]]]"))
  (assert-true (string= (list-to-snailstr (snail-reduce (snailstr-to-list "[[[[[6,6],[6,6]],[[6,0],[6,7]]],[[[7,7],[8,9]],[8,[8,1]]]],[2,9]]")))
                        "[[[[6,6],[7,7]],[[0,7],[7,7]]],[[[5,5],[5,6]],9]]"))
  (assert-true (string= (list-to-snailstr (snail-reduce (snailstr-to-list "[[[[7,7],[12,0]],[[5,10],[13,17]]],[3,9]]")))
                        "[[[[6,6],[7,7]],[[0,7],[7,7]]],[[[5,5],[5,6]],9]]"))
  (assert-true (string= (list-to-snailstr (snail-reduce (snailstr-to-list "[[[[0,8],[6,[9,10]]],[[0,13],[25,0]]],[3,9]]")))
                        "[[[[6,6],[7,7]],[[0,7],[7,7]]],[[[5,5],[5,6]],9]]"))
 
  (assert-true (string= (list-to-snailstr (snail-reduce (snailstr-to-list "[[[[9,0],[15,14]],[[15,0],[25,0]]],[3,9]]")))
                        "[[[[6,6],[7,7]],[[0,7],[7,7]]],[[[5,5],[5,6]],9]]"))
  (assert-true (string= (list-to-snailstr (snail-reduce (snailstr-to-list "[[[[12,12],[6,0]],[[[14,7],[8,9]],[8,[8,1]]]],[2,9]]")))
                        "[[[[6,6],[7,7]],[[0,7],[7,7]]],[[[5,5],[5,6]],9]]"))
  (assert-true (string= (list-to-snailstr (snail-reduce (snailstr-to-list "[[[[12,12],[6,14]],[[15,0],[25,0]]],[3,9]]")))
                        "[[[[6,6],[7,7]],[[0,7],[7,7]]],[[[5,5],[5,6]],9]]"))
  (assert-true (string= (list-to-snailstr (snail-reduce (snailstr-to-list "[[[[12,12],[6,14]],[[15,0],[17,[8,1]]]],[2,9]]")))
                                                "[[[[6,6],[7,7]],[[0,7],[7,7]]],[[[5,5],[5,6]],9]]"))
  )
(run-tests '(reduce1))

(defun snail-sum-list (list)
  (let ((accum (car list)))
    (loop for i from 1 below (length list)
         :do (setf accum (snail+ accum (nth i list))))
    accum))

(defun parse-and-snail-sum-list (list)
  (snail-sum-list (mapcar #'parse-line list)))

(define-test list-tests
  (assert-true (string=
                (list-to-snailstr (parse-and-snail-sum-list '("[1,1]" "[2,2]" "[3,3]" "[4,4]")))
                "[[[[1,1],[2,2]],[3,3]],[4,4]]"))
  (assert-true (string=
                (list-to-snailstr (parse-and-snail-sum-list '("[1,1]" "[2,2]" "[3,3]" "[4,4]" "[5,5]")))
                "[[[[3,0],[5,3]],[4,4]],[5,5]]"))
  (assert-true (string=
                (list-to-snailstr (parse-and-snail-sum-list '("[1,1]" "[2,2]" "[3,3]" "[4,4]" "[5,5]" "[6,6]")))
                "[[[[5,0],[7,4]],[5,5]],[6,6]]"))

  (assert-true (string=
                (list-to-snailstr (parse-and-snail-sum-list '(
                                                              "[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]"
                                                              "[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]"
                                                              "[[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]]"
                                                              "[[[[2,4],7],[6,[0,5]]],[[[6,8],[2,8]],[[2,1],[4,5]]]]"
                                                              "[7,[5,[[3,8],[1,4]]]]"
                                                              "[[2,[2,2]],[8,[8,1]]]"
                                                              "[2,9]"
                                                              "[1,[[[9,3],9],[[9,0],[0,7]]]]"
                                                              "[[[5,[7,4]],7],1]"
                                                              "[[[[4,2],2],6],[8,7]]"))) "[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]"))
  )

(run-tests '(list-tests))

(defun magnitude (list)
  (if (atom list)
      list
      (+ (* 3 (magnitude (car list))) (* 2 (magnitude (cadr list))))))


(untrace magnitude)

(define-test magnitude
  (assert-equal 9 (magnitude 9))
  (assert-equal 1 (magnitude 1))
  (assert-equal 29 (magnitude '(9 1)))

  (assert-equal (magnitude (snailstr-to-list "[9,1]")) 29)
  (assert-equal (magnitude (snailstr-to-list "[1,9]")) 21)
  (assert-equal (magnitude (snailstr-to-list "[[9,1],[1,9]]")) 129)
  (assert-equal (magnitude (snailstr-to-list "[[1,2],[[3,4],5]]")) 143)
  (assert-equal (magnitude (snailstr-to-list "[[[[0,7],4],[[7,8],[6,0]]],[8,1]]")) 1384)
  (assert-equal (magnitude (snailstr-to-list "[[[[1,1],[2,2]],[3,3]],[4,4]]")) 445)
  (assert-equal (magnitude (snailstr-to-list "[[[[3,0],[5,3]],[4,4]],[5,5]]")) 791)
  (assert-equal (magnitude (snailstr-to-list "[[[[5,0],[7,4]],[5,5]],[6,6]]")) 1137)
  (assert-equal (magnitude (snailstr-to-list "[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]")) 3488)

  (assert-true (string=
                (list-to-snailstr (parse-and-snail-sum-list '(
                                                              "[[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]"
                                                              "[[[5,[2,8]],4],[5,[[9,9],0]]]"
                                                              "[6,[[[6,2],[5,6]],[[7,6],[4,7]]]]"
                                                              "[[[6,[0,7]],[0,9]],[4,[9,[9,0]]]]"
                                                              "[[[7,[6,4]],[3,[1,3]]],[[[5,5],1],9]]"
                                                              "[[6,[[7,3],[3,2]]],[[[3,8],[5,7]],4]]"
                                                              "[[[[5,4],[7,7]],8],[[8,3],8]]"
                                                              "[[9,3],[[9,9],[6,[4,9]]]]"
                                                              "[[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]"
                                                              "[[[[5,2],5],[8,[3,7]]],[[5,[7,5]],[4,4]]]"
                                                              ))) "[[[[6,6],[7,6]],[[7,7],[7,0]]],[[[7,7],[7,7]],[[7,8],[9,9]]]]"))
  (assert-equal
                (magnitude (parse-and-snail-sum-list '(
                                                              "[[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]"
                                                              "[[[5,[2,8]],4],[5,[[9,9],0]]]"
                                                              "[6,[[[6,2],[5,6]],[[7,6],[4,7]]]]"
                                                              "[[[6,[0,7]],[0,9]],[4,[9,[9,0]]]]"
                                                              "[[[7,[6,4]],[3,[1,3]]],[[[5,5],1],9]]"
                                                              "[[6,[[7,3],[3,2]]],[[[3,8],[5,7]],4]]"
                                                              "[[[[5,4],[7,7]],8],[[8,3],8]]"
                                                              "[[9,3],[[9,9],[6,[4,9]]]]"
                                                              "[[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]"
                                                              "[[[[5,2],5],[8,[3,7]]],[[5,[7,5]],[4,4]]]"
                                                              ))) 4140 )

)  

(defparameter *test-list* '(
(  "[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]" "[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]" "[[[[4,0],[5,4]],[[7,7],[6,0]]],[[8,[7,7]],[[7,9],[5,0]]]]" )
(  "[[[[4,0],[5,4]],[[7,7],[6,0]]],[[8,[7,7]],[[7,9],[5,0]]]]" "[[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]]" "[[[[6,7],[6,7]],[[7,7],[0,7]]],[[[8,7],[7,7]],[[8,8],[8,0]]]]" )
(  "[[[[6,7],[6,7]],[[7,7],[0,7]]],[[[8,7],[7,7]],[[8,8],[8,0]]]]" "[[[[2,4],7],[6,[0,5]]],[[[6,8],[2,8]],[[2,1],[4,5]]]]" "[[[[7,0],[7,7]],[[7,7],[7,8]]],[[[7,7],[8,8]],[[7,7],[8,7]]]]" )
(  "[[[[7,0],[7,7]],[[7,7],[7,8]]],[[[7,7],[8,8]],[[7,7],[8,7]]]]" "[7,[5,[[3,8],[1,4]]]]" "[[[[7,7],[7,8]],[[9,5],[8,7]]],[[[6,8],[0,8]],[[9,9],[9,0]]]]" )
(  "[[[[7,7],[7,8]],[[9,5],[8,7]]],[[[6,8],[0,8]],[[9,9],[9,0]]]]" "[[2,[2,2]],[8,[8,1]]]" "[[[[6,6],[6,6]],[[6,0],[6,7]]],[[[7,7],[8,9]],[8,[8,1]]]]" )
(  "[[[[6,6],[6,6]],[[6,0],[6,7]]],[[[7,7],[8,9]],[8,[8,1]]]]" "[2,9]" "[[[[6,6],[7,7]],[[0,7],[7,7]]],[[[5,5],[5,6]],9]]" )
(  "[[[[6,6],[7,7]],[[0,7],[7,7]]],[[[5,5],[5,6]],9]]" "[1,[[[9,3],9],[[9,0],[0,7]]]]" "[[[[7,8],[6,7]],[[6,8],[0,8]]],[[[7,7],[5,0]],[[5,5],[5,6]]]]" )
(  "[[[[7,8],[6,7]],[[6,8],[0,8]]],[[[7,7],[5,0]],[[5,5],[5,6]]]]" "[[[5,[7,4]],7],1]" "[[[[7,7],[7,7]],[[8,7],[8,7]]],[[[7,0],[7,7]],9]]" )
(  "[[[[7,7],[7,7]],[[8,7],[8,7]]],[[[7,0],[7,7]],9]]" "[[[[4,2],2],6],[8,7]]" "[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]" )))

(defun do-tests ()
  (loop for test in *test-list*
        :do (let ((sumval 'nil))
              (setf sumval (snail+ (snailstr-to-list (nth 0 test)) (snailstr-to-list (nth 1 test))))
              (assert-true (string= (list-to-snailstr sumval ) (nth 2 test)) )
              (if (not (string= (list-to-snailstr sumval ) (nth 2 test)) )
                  (progn
                    (format t "test failed ~a~%" test)
                    (format t "i got ~a~%" (list-to-snailstr sumval))
                    (format t "comparing values~%")
                    (format t "mine  :~a~%" (list-to-snailstr sumval))
                    (format t "theirs:~a~%" (nth 2 test))
                    )))))

(define-test sum-list-test
  (do-tests)
  )

(run-tests '(sum-list-test))

(defun homework (filename)
  (magnitude (snail-sum-list (mapcar #'parse-line (uiop:read-file-lines filename)))))

(format t "part 2 final answer ~a~%" (homework  "inputs/input18.txt"))

(defun max-magnitude (list)
  (maximum (flatten (loop for l1 in list
        :collect (loop for l2 in list
                       :collect (magnitude (snail+ l1 l2)))))))
                     
(defun parse-wholelist (list)
  (mapcar #'parse-line list))

(max-magnitude (parse-wholelist '(
                   "[[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]"
                   "[[[5,[2,8]],4],[5,[[9,9],0]]]"
                   "[6,[[[6,2],[5,6]],[[7,6],[4,7]]]]"
                   "[[[6,[0,7]],[0,9]],[4,[9,[9,0]]]]"
                   "[[[7,[6,4]],[3,[1,3]]],[[[5,5],1],9]]"
                   "[[6,[[7,3],[3,2]]],[[[3,8],[5,7]],4]]"
                   "[[[[5,4],[7,7]],8],[[8,3],8]]"
                   "[[9,3],[[9,9],[6,[4,9]]]]"
                   "[[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]"
                   "[[[[5,2],5],[8,[3,7]]],[[5,[7,5]],[4,4]]]"
                   )))


(defun homework2 (filename)
  (max-magnitude (mapcar #'parse-line (uiop:read-file-lines filename))))

(format t "part 2 final answer ~a~%" (homework2  "inputs/input18.txt"))



