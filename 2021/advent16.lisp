(ql:quickload 'cl-ppcre)
(ql:quickload :uiop)
(ql:quickload 'cl-ppcre)
(ql:quickload 'queues)
(ql:quickload 'alexandria)
(ql:quickload 'priority-queue)
(ql:quickload :lisp-unit)
(defpackage :advent21-16 (:use :cl :cl-ppcre :queues :priority-queue))
(in-package :advent21-16)
(require :queues.simple-queue)

(defparameter *hexmap* '(
(#\0 . "0000")
(#\1 . "0001")
(#\2 . "0010")
(#\3 . "0011")
(#\4 . "0100")
(#\5 . "0101")
(#\6 . "0110")
(#\7 . "0111")
(#\8 . "1000")
(#\9 . "1001")
(#\A . "1010")
(#\B . "1011")
(#\C . "1100")
(#\D . "1101")
(#\E . "1110")
(#\F . "1111")))


(defparameter *instrnib* 0)
(defparameter *bitptr* 0)
(defparameter *nibbuf* 'nil)
(defparameter *inq* 'nil)
(defparameter *interp* "")

(defun indent (depth)
  (dotimes (i (* depth 2)) (format t " ")))

(defun bitq-from-hexstring (instring)
  (progn
    (defparameter *inq* (make-queue :simple-queue ))
    (loop for i from 0 below (length instring)
          do (let* ((c (char-upcase (char instring i)))
                    (a (assoc c *hexmap*)))
                    (if a
                        (loop for j from 0 below 4
                         do (qpush *inq* (char (cdr a) j))))))))

(defun bitq-from-bitstring (instring)
  (progn
    (defparameter *inq* (make-queue :simple-queue ))
    (loop for i from 0 below (length instring)
          do (if (not (char= (char instring i) #\Space)) (qpush *inq* (char instring i))))))

(defun make-bitq (filename)
  (bitq-from-hexstring (uiop:read-file-string filename)))

(defun bit-eof ()
  (let ((e (qsize *inq*) ))
    (if (and e (> e 0)) 'nil 'T)))

(defun read-bits (nbits)
  (let ((r 0)
        (b 0)
        (e 'nil)
        (pos  (qsize *inq*)))
    (dotimes ( i nbits )
      (let ((inbit (qpop *inq*)))
        (if inbit ( progn
                    (incf b 1)
                    (setf r (logior (ash r 1) (- (char-code inbit) (char-code #\0 ) )))))))
    ;;(format t "read-bits at pos ~a returning ~a ~%" pos r) 
    (values r b (bit-eof))))


(lisp-unit:define-test bitq
  (lisp-unit:assert-equal 'nil (bitq-from-bitstring "0101"))
  (lisp-unit:assert-equal 5 (read-bits 4))
  (lisp-unit:assert-equal 'nil (bitq-from-bitstring "01010101"))
  (lisp-unit:assert-equal 5 (read-bits 4))
  (lisp-unit:assert-equal 5 (read-bits 4))
  (lisp-unit:assert-true  (bit-eof))
  (lisp-unit:assert-equal 'nil (bitq-from-hexstring "F"))
  (lisp-unit:assert-equal 15 (read-bits 4))
  (lisp-unit:assert-true  (bit-eof))
  (lisp-unit:assert-equal 0 (read-bits 4))
  (lisp-unit:assert-equal 'nil (bitq-from-hexstring "65"))
  (lisp-unit:assert-equal 6 (read-bits 4))
  (lisp-unit:assert-equal 5 (read-bits 4))
  (lisp-unit:assert-true  (bit-eof))
  (lisp-unit:assert-equal 'nil (bitq-from-hexstring "22010ca048804326232185990cff39a4e3b11948a0cb1ca805321af18c8698610c9f2c63256c558"))
  (lisp-unit:assert-equal 4456985 (read-bits 25))
  )
(lisp-unit:run-tests '(bitq))

(defun read-literal-value (depth)
  (let* ((my-read-bits 0)
         (result 0)
         (keeplooping 1)
         (valuestring 'nil))
    (loop until (= keeplooping 0)
          :do (multiple-value-bind (l r) (read-bits 1)
                (setf keeplooping l)
                (incf my-read-bits r)
                (multiple-value-bind (r b e) (read-bits 4)
                  (setf result (logior (ash result 4) r))
                  (incf my-read-bits b)
                  )
                )
          )
    (setf valuestring (write-to-string result))
    (setf *interp* (format 'nil "~a ~a" *interp* (write-to-string result)))
    ;;(indent depth)
    ;;(format t "literal read returns ~a ~a ~%" result my-read-bits)
    ;;(indent depth)(format t "literal valuestring:~a~%" valuestring)
    (values result my-read-bits valuestring)))

(lisp-unit:define-test readbits
  (lisp-unit:assert-equal 'nil (bitq-from-hexstring "F"))
  (lisp-unit:assert-equal 15 (read-bits 4))
  (lisp-unit:assert-true  (bit-eof))
  (lisp-unit:assert-equal 0 (read-bits 4))
  (lisp-unit:assert-equal 'nil (bitq-from-hexstring "65"))
  (lisp-unit:assert-equal 6 (read-bits 4))
  (lisp-unit:assert-equal 5 (read-bits 4))
  (lisp-unit:assert-true  (bit-eof))
  (lisp-unit:assert-equal 'nil (bitq-from-hexstring "22010ca048804326232185990cff39a4e3b11948a0cb1ca805321af18c8698610c9f2c63256c558"))
  (lisp-unit:assert-equal 4456985 (read-bits 25))
  (lisp-unit:assert-equal 8462849 (read-bits 25))
  (lisp-unit:assert-equal 1650969 (read-bits 25))
  (lisp-unit:assert-equal 1595792  (read-bits 25))
  (lisp-unit:assert-equal 27256628  (read-bits 25))
  (lisp-unit:assert-equal 20507718  (read-bits 25))
  (lisp-unit:assert-equal 10768485  (read-bits 25))
  (lisp-unit:assert-equal 18655237  (read-bits 25))
  (lisp-unit:assert-equal 6567395  (read-bits 25))
  (lisp-unit:assert-equal 3283553  (read-bits 25))
  (lisp-unit:assert-equal 17327353 (read-bits 25))
  (lisp-unit:assert-equal 12989014  (read-bits 25))
  (lisp-unit:assert-equal   50520  (read-bits 25))
  (lisp-unit:assert-equal  0    (read-bits 25))

  (lisp-unit:assert-equal 'nil (bitq-from-bitstring "100001 000010 00010000 00000"))
  (lisp-unit:assert-equal  0    (read-literal-value 0))
  (lisp-unit:assert-true  (bit-eof))
  (lisp-unit:assert-equal 'nil (bitq-from-bitstring "1000010000100001000000001"))
  (lisp-unit:assert-equal  1    (read-literal-value 0))
  (lisp-unit:assert-true  (bit-eof))
  (lisp-unit:assert-equal 'nil (bitq-from-bitstring "1000010000100001010000001"))
  (lisp-unit:assert-equal  65    (read-literal-value 0))
  (lisp-unit:assert-true  (bit-eof))
  (lisp-unit:assert-equal 'nil (bitq-from-bitstring "1010000000"))
  (lisp-unit:assert-equal  64    (read-literal-value 0))
    (lisp-unit:assert-true  (bit-eof))
  )
(lisp-unit:run-tests '(readbits))

(defparameter *int-op* '((0 . "#'add") (1 . "#'mult") (2 . "#'min") (3 . "#'max") (5 . "#'gtop") (6 . "#'ltop") (7 . "#'eqop")))


(defun op-str (op) (cdr (assoc op *int-op*)))

(defun do-operator (ptype values depth)
  (let ((retval 'nil))
    ;;(indent depth)  (format t "***executing operator ~a (~a) on ~a ~%" (op-str ptype) ptype values)
    (if values
        (progn
          ;;(indent depth)  (format t " values good ~%")
          (if (and (or (= ptype 5) (= ptype 6) (= ptype 7)) (/= 2 (length values)))
              (progn
                (indent depth)  (format t "**********************************************~%")
                (indent depth)  (format t "***        ERROR                           ***~%")
                (indent depth)  (format t "*** operator ~a should have 2 values but has ~a ~%" ptype (length values))
                (indent depth)  (format t "*** values: ~a ***~%" values)
                (indent depth)  (format t "**********************************************~%")
                (setf retval 'nil)
                )
          (cond ((= ptype 0) (setf retval (reduce #'+ values)))
                ((= ptype 1) (setf retval (reduce #'* values)))
                ((= ptype 2) (setf retval (loop for e in values :minimize e)))
                ((= ptype 3) (setf retval (loop for e in values :maximize e)))
                ((= ptype 4) (setf retval (values)))
                ((= ptype 5) (setf retval (if (reduce #'< values) 1 0)))
                ((= ptype 6) (setf retval (if (reduce #'> values) 1 0)))
                ((= ptype 7) (setf retval (if (reduce #'= values) 1 0))))
          ))
        )
    (indent depth)
    ;;(format t "***executing operator ~a on ~a result ~a ~%" (op-str ptype) values retval)
    retval))

(setf *interp* 'nil)
*interp*
(lisp-unit:define-test operator-tests
  (lisp-unit:assert-equal 'nil (do-operator 0 '( ) 0))
  (lisp-unit:assert-equal 2 (do-operator 0 '( 2) 0))
  (lisp-unit:assert-equal 3 (do-operator 0 '( 2 1) 0))
  (lisp-unit:assert-equal 5 (do-operator 0 '( 2 3) 0))
  (lisp-unit:assert-equal 9 (do-operator 0 '( 2 3 4) 0))
  (lisp-unit:assert-equal 14 (do-operator 0 '( 2 3 4 5) 0))
  (lisp-unit:assert-equal 6 (do-operator 1 '( 2 3) 0))
  (lisp-unit:assert-equal 24 (do-operator 1 '( 2 3 4) 0))
  (lisp-unit:assert-equal 120 (do-operator 1 '( 2 3 4 5) 0))
  
  (lisp-unit:assert-equal 'nil (do-operator 2 '( ) 0))
  (lisp-unit:assert-equal 2 (do-operator 2 '( 2) 0))
  (lisp-unit:assert-equal 1 (do-operator 2 '( 2 1) 0))
  (lisp-unit:assert-equal 2 (do-operator 2 '( 2 3) 0))
  (lisp-unit:assert-equal 2 (do-operator 2 '( 2 3 4) 0))
  (lisp-unit:assert-equal 2 (do-operator 2 '( 2 3 4 5) 0))

  (lisp-unit:assert-equal 'nil (do-operator 3 '( ) 0))
  (lisp-unit:assert-equal 2 (do-operator 3 '( 2) 0))
  (lisp-unit:assert-equal 2 (do-operator 3 '( 2 1) 0))
  (lisp-unit:assert-equal 3 (do-operator 3 '( 2 3) 0))
  (lisp-unit:assert-equal 4 (do-operator 3 '( 2 3 4) 0))
  (lisp-unit:assert-equal 5 (do-operator 3 '( 2 3 4 5) 0))

  (lisp-unit:assert-equal 'nil (do-operator 5 '( ) 0))
  (lisp-unit:assert-equal 'nil (do-operator 5 '( 2) 0))
  (lisp-unit:assert-equal 0 (do-operator 5 '( 2 1) 0))
  (lisp-unit:assert-equal 1 (do-operator 5 '( 2 3) 0))
  (lisp-unit:assert-equal 'nil (do-operator 5 '( 2 3 4) 0))

  (lisp-unit:assert-equal 'nil (do-operator 6 '( ) 0))
  (lisp-unit:assert-equal 'nil (do-operator 6 '( 2) 0))
  (lisp-unit:assert-equal 1 (do-operator 6 '( 2 1) 0))
  (lisp-unit:assert-equal 0 (do-operator 6 '( 2 3) 0))
  (lisp-unit:assert-equal 'nil (do-operator 6 '( 2 3 4) 0))

  (lisp-unit:assert-equal 'nil (do-operator 7 '( ) 0))
  (lisp-unit:assert-equal 'nil (do-operator 7 '( 2) 0))
  (lisp-unit:assert-equal 1 (do-operator 7 '( 1 1) 0))
  (lisp-unit:assert-equal 0 (do-operator 7 '( 2 3) 0))
  (lisp-unit:assert-equal 'nil (do-operator 7 '( 2 3 4) 0))
  )
(lisp-unit:run-tests '(operator-tests))

(defun add (&rest rest)  (apply #'\+ rest))
(defun mult (&rest rest)  (apply #'\* rest))
(defun ltop (a b)  (if (< a b) 1 0))
(defun gtop (a b)  (if (> a b) 1 0))
(defun eqop (a b)  (if (= a b) 1 0))

;;(reduce #'> '(0 124 1180 1180) )
;;(DO-OPERATOR 6 '(0 124 1180 1180) 1)
                                   
(defun read-operator (ptype depth)
  (let* ((my-read-bits 0)
         (eofval 'nil)
         (packetval 'nil)
         (values 'nil)
         (valuestring 'nil))
    ;;(indent depth)(format t "1 in read operator ~a ~%" (qsize *inq*))
    (setf valuestring (format 'nil "(oper ~a " (op-str ptype)))
    (setf *interp* (format 'nil "~a (oper ~a" *interp* (op-str ptype)))
    (multiple-value-bind (op bits) (read-bits 1)
      (incf my-read-bits bits)
      ;;(indent depth)(format t "2 in read operator operator is ~a, so we will count ~a ~a ~%" op (if (= op 1) "packets" "bits") (qsize *inq*))
      (if (= 0 op )
          (multiple-value-bind (totallen b) (read-bits 15)
            ;;(indent depth)(format t "2a read-operator, bit count: ~a read ~a bits left~%" totallen b)
            (incf my-read-bits b)
            (if (= b 15)
                (progn
                  ;;(indent depth)(format t "3 read-operator opltype 0 ~A size ~a ~%" totallen  (qsize *inq*))
                  (let* ((loop-bits totallen))
                    (loop while (and (not eofval) (>= loop-bits 0))
                          :do (progn
                                ;;(indent depth)(format t "4 read-operator loop opltype 0 ~A loop-bits ~a size ~a ~%" totallen loop-bits (qsize *inq*))
                                (multiple-value-bind (pv pt b eof vs) (read-packet (1+ depth))
                                  (decf loop-bits b)
                                  (setf eofval eof)
                                  (if pv
                                      (progn
                                        (setf valuestring (format 'nil "~a ~a " valuestring vs))
                                        (push pv values)
                                        ;;(indent depth)(format t "5 push-val ~a now ~a~%" pv values)
                                        ))
                                  ))
                          ) 
                    ))))
          (multiple-value-bind (subpackets b) (read-bits 11)
            ;;(indent depth)(format t "6a read-operator, subpacket count ~a bits ~a~%" subpackets b)
            (incf my-read-bits b)
            ;;(indent depth)(format t "6 read-operator subpackets ~A size ~a ~%" subpackets  (qsize *inq*))
            (loop for i from 0 below subpackets when (not eofval )
                  :do (progn
                        ;;(indent depth)(format t "7 subpacket ~a size ~a~%" i (qsize *inq*))
                        (multiple-value-bind (pv pt b eof vs) (read-packet (1+ depth))
                          (if pv
                              (progn
                                (setf valuestring (format 'nil "~a ~a " valuestring vs))
                                (setf eofval eof)
                                (incf my-read-bits b)
                                ;;(indent depth)(format t "7.1 subpacket ~a size ~a~%" i (qsize *inq*))
                                (push pv values)
                                ;;(indent depth)(format t "8 push val ~a now ~a~%" pv values)
                                ))))))))
    ;;(indent depth)(format t "9 read operator values ~A ~%" values)
    (setf packetval (do-operator ptype values (1+ depth)))
    ;;(indent depth)(format t "a read operator ~A returning value~%" packetval)
    (setf valuestring (format 'nil "~a )" valuestring))
    ;;(indent depth)(format t "operator valuestring: ~a  ~%" valuestring)
    (values packetval my-read-bits eofval valuestring)))

(op-str 0)
(defun packet (x) x)
(defun oper (op &rest vals) (apply op vals))
(eval (read-from-string "(packet 5)"))
(eval (read-from-string "(oper #'add (packet 5) (packet 5))"))
(eval (read-from-string "(oper #'mult (packet 5) )"))
(oper +  5 6)


(lisp-unit:define-test read-operator
  (lisp-unit:assert-equal 'nil (bitq-from-bitstring "0 000000000001011 0011 0000 010"))
  (lisp-unit:assert-equal 5 (multiple-value-bind (pv b e vs)(read-operator 0 0) pv))
  (lisp-unit:assert-equal 'nil (bitq-from-bitstring "0 000000000000000 0011 0000 101"))
  (lisp-unit:assert-equal 5 (eval (read-from-string (multiple-value-bind (pv b e vs)(read-operator 1 0) vs))))
  (lisp-unit:assert-true (bit-eof))
  (lisp-unit:assert-equal 'nil (bitq-from-bitstring "0 000000000001011 0011 0000 101"))
  (lisp-unit:assert-equal 5 (multiple-value-bind (pv b e vs)(read-operator 0 0) pv))
  (lisp-unit:assert-true (bit-eof))
  (lisp-unit:assert-equal 'nil (bitq-from-bitstring "0 000000000001011 0011 0000 110"))
  (lisp-unit:assert-equal 6 (eval (read-from-string (multiple-value-bind (pv b e vs)(read-operator 1 0) vs))))
  (lisp-unit:assert-true (bit-eof))
  (lisp-unit:assert-equal 'nil (bitq-from-bitstring "0 000000000010110 0011 0000 110  0011 0000 101"))
  (lisp-unit:assert-equal 30 (eval (read-from-string (multiple-value-bind (pv b e vs)(read-operator 1 0) vs))))
  (lisp-unit:assert-true (bit-eof))

  (lisp-unit:assert-equal 'nil (bitq-from-bitstring "1 00000000010 0011 0000 110  0011 0000 101"))
  (lisp-unit:assert-equal 30 (eval (read-from-string (multiple-value-bind (pv b e vs)(read-operator 1 0) vs))))
  (lisp-unit:assert-true (bit-eof))
  (lisp-unit:assert-equal 'nil (bitq-from-bitstring "1 00000000011 0011 0000 110  0011 0000 101 0011 0001 101"))
  (lisp-unit:assert-equal 390 (eval (read-from-string (multiple-value-bind (pv b e vs)(read-operator 1 0) vs))))
  (lisp-unit:assert-true (bit-eof))
  )
(lisp-unit:run-tests '(read-operator))


(untrace read-operator)

(defparameter *total-versions* 0)


;(defparameter *mylist* 'nil)

;(reduce #'= *mylist*)
;      (push 5 *mylist*)
;      (push 6 *mylist*)
;    (loop for element in *mylist* :maximize element)
  
(defun read-packet (depth)
  (let* ((my-read-bits 0)
         (pvers 0)
         (ptype 0)
         (eof 'nil)
         (result 'nil )
         (valuestring 'nil))
    ;;(indent depth)(format t "read packet start bits left ~a ~%" (qsize *inq*))
    (setf valuestring (format 'nil " (packet "))
    (if (not (bit-eof))
        (multiple-value-bind (p b) (read-bits 3)
          (incf my-read-bits b)
          (setq pvers p)
          (if (and pvers (not (bit-eof)))
              (progn
                ;;(indent depth)(format t "read packet after check for eof~%")
                (multiple-value-bind (p b) (read-bits 3)
                  (incf my-read-bits b)
                  (setf ptype p))
                (incf *total-versions* pvers)
                ;;(indent depth)(format t "read packet version ~a type ~a bits left ~a~%" pvers ptype (qsize *inq*))
                (if (not (bit-eof))
                    (if (= ptype 4)
                        (multiple-value-bind (litval readbits vstring) (read-literal-value (1+ depth))
                          (incf my-read-bits readbits)
                          (setf result litval)
                          ;;(indent depth)(format t "litval ~A ~%" litval)
                          (setf valuestring (format 'nil "~a ~a" valuestring vstring))
                          )
                        (multiple-value-bind (operval readbits eof vstring) (read-operator ptype (1+ depth))
                          (incf my-read-bits readbits)
                          (setf result operval)
                          ;;(indent depth)(format t "operator ~A ~a ~a~%" operval readbits vstring)
                          (setf valuestring (format 'nil "~a ~a" valuestring vstring))
                          ))
                    )
                ))))
    (setf eof (bit-eof))
    ;;(indent depth)(format t "read packet zero version -  bits left ~a~%" (qsize *inq*))
    (setf valuestring (format 'nil " ~a )" valuestring))
    (indent depth)(format t "valuestring: ~a  ~%" valuestring)
    (values result ptype my-read-bits eof valuestring)))

(lisp-unit:define-test read-packet
  (lisp-unit:assert-equal 'nil (bitq-from-hexstring "30a"))
  (lisp-unit:assert-equal 5 (multiple-value-bind (pv b e v)(read-packet 1) pv))
  (lisp-unit:assert-equal 'nil (bitq-from-bitstring "001 100 00 1011"))
  (lisp-unit:assert-equal 5 (eval (read-from-string (multiple-value-bind (pv b e x v)(read-packet 1) v))))

  (lisp-unit:assert-equal 'nil (bitq-from-bitstring "0011 001 xxxx"))
  (lisp-unit:assert-equal 5 (multiple-value-bind (pv b e x v)(read-packet 1) pv))
  

  )
(lisp-unit:run-tests '(read-packet))


(unttrace read-packet)
(defun version-sum (str &optional interp)
  (let ((retval 'nil)
        (final-str 'nil))
    (format t "-------------------------------------------------------------~%")
    (format t "--                   start                                 --~%")
    (format t "--                   ~A~%" interp)
    (format t "-------------------------------------------------------------~%")
    (defparameter *total-versions* 0)
    (defparameter *interp* 'nil)
    (bitq-from-hexstring str)
    (multiple-value-bind (pv pt b e vs)(read-packet 0)
      ;;(format t "pv ~a pt ~a b ~a e ~a ~%" pv pt b e)
      (setf final-str vs)
      (setf retval pv)
      )
   
    (format t "-------------------------------------------------------------~%")
    (format t "--                   end                                 --~%")
    (format t "--                   ~A~%" final-str)
    (format t "-------------------------------------------------------------~%")
    ;;(format t "TOTAL VERSIONS: ~a~%" *total-versions* )
    (values retval *total-versions* final-str)))

(defun file-sum (str)
  (let ((retval 'nil))
    (defparameter *total-versions* 0)
    (make-bitq str)
    (multiple-value-bind (pv pt b e)(read-packet 0)
      (format t "pv ~a pt ~a b ~a e ~a ~%" pv pt b e)
      (setf retval pv)
      )
    ;;(format t "TOTAL VERSIONS: ~a~%" *total-versions* )
    retval))

        ;(file-sum "input")
;;(bitq-from-hexstring "D2FE28")
;;(read-bits 3)
;;(read-bits 3)


(lisp-unit:define-test final-part1-tests
  (lisp-unit:assert-equal 2021 (multiple-value-bind (rv tv) (version-sum "D2FE28") rv))
  (lisp-unit:assert-equal 6 (multiple-value-bind (rv tv) (version-sum "D2FE28") tv))
  (lisp-unit:assert-equal 1 (multiple-value-bind (rv tv) (version-sum "38006F45291200") rv))
  (lisp-unit:assert-equal 9 (multiple-value-bind (rv tv) (version-sum "38006F45291200") tv))
  (lisp-unit:assert-equal 16 (multiple-value-bind (rv tv)(version-sum "8A004A801A8002F478") tv))
  (lisp-unit:assert-equal 12 (multiple-value-bind (rv tv)(version-sum "620080001611562C8802118E34") tv))
  (lisp-unit:assert-equal 23 (multiple-value-bind (rv tv)(version-sum "C0015000016115A2E0802F182340") tv))  
  (lisp-unit:assert-equal 31 (multiple-value-bind (rv tv)(version-sum "A0016C880162017C3686B18A3D4780") tv))
  )
(lisp-unit:run-tests '(final-part1-tests))

;; part 2 tests
(lisp-unit:define-test final-part2-tests
  (lisp-unit:assert-equal 3 (version-sum "C200B40A82"))
  (lisp-unit:assert-equal 54 (version-sum "04005AC33890") )
  (lisp-unit:assert-equal 7  (version-sum "880086C3E88112"))
  (lisp-unit:assert-equal 9  (version-sum "CE00C43D881120"))
  (lisp-unit:assert-equal 1  (version-sum "D8005AC2A8F0"))
  (lisp-unit:assert-equal 0  (version-sum "F600BC2D8F"))
  (lisp-unit:assert-equal 0  (version-sum "9C005AC2F8F0"))
  (lisp-unit:assert-equal 1  (version-sum "9C0141080250320F1802104A08"))
  )
(lisp-unit:run-tests '(final-part2-tests))

(defun rv (str)
  (multiple-value-bind (rv tv vs)(version-sum str) rv))
(defun vs (str)
  (eval (read-from-string (multiple-value-bind (rv tv vs)(version-sum str) vs))))


(defparameter *exp-tests* '(
                            ("302" . 1)
                            ("22008c09820" . 3)
                            ( "2200cc1982308" .  9 )
                            ( "22008c19820" . 5)
                            ( "26008c19820" .  6 )
                            ( "26008c19820" .  6 )
                            ( "2600cc1982308" .  24 )
                            ( "2a00cc1982308" .  2 )
                            ( "22008c09500660cc11840" .  3 )
                            ( "26008c11700660cc11840" .  8 )
                            ( "36008c09820" .  0 )
                            ( "22010ca048804326232185990cff39a4e3b11948a0cb1ca805321af18c8698610c9f2c63256c558" .  42841945907 )
                            ( "2a004b8012a004b801321ec9c" .  3735 )
                            ( "2e004a8012a004a8012a004b8012e004b8012a004a8012a004b8012a004cb5f13bff000" .  1736671104 )
                            ( "26008f8023293ef9a64186431cae7d1cdda" .  0 )
                            ( "22008cbf8a540113004644860cc10" .  2023 )
                            ))

(lisp-unit:define-test part2-simple-tests
  (loop for expt in *exp-tests*
        :do (progn
              (format t "testing ~a against ~a" (car expt) (cdr expt))
              (lisp-unit:assert-equal  (cdr expt) (rv (car expt)))
              (lisp-unit:assert-equal  (cdr expt) (vs (car expt)))
              )))

(lisp-unit:run-tests '(part2-simple-tests))
  

(lisp-unit:define-test part2-complex-tests
;;the next two have the same semantics, but different encoding
  ;;(min(max(min(min(min(min(max(max(max(min(min(min(max(min(1736671104)))))))))))))) + 4 + max(522179) + (19219 * (38942 < 247596226)) + ((7 * 8 * 14) + (8 * 2 * 12) + (4 * 10 * 3)) + 131 + ((17052 < 2979) * 46058) + 664609 + ((2159 > 2159) * 580431) + (13 * 200 * 84 * 116) + 15 + 80 + (95 * (2 < 16651)) + (8599772 * ((10 + 14 + 2) < (8 + 9 + 11))) + 14698 + min(6, 176724, 17, 218) + ((2951 > 61) * 30979330271) + (((5 + 2 + 6) > (9 + 12 + 2)) * 35056) + max(21, 8, 10464633, 21766) + (95 * 161) + ((28 > 28) * 1176227742) + (102 * ((2 + 4 + 11) > (10 + 15 + 10))) + min(246, 1554) + ((1470 == 6827177) * 208) + min(728, 763810927, 55700720745) + ((5916415 > 2489) * 289600) + (230 * 92 * 120 * 220 * 159) + (((11 + 5 + 11) == (15 + 14 + 10)) * 49769) + min(1466502782) + max(1589, 214, 13) + ((170605153 < 170605153) * 198398) + 18956 + (196905722 * (112 == 112)) + (4217206544 + 48993801750 + 25736 + 3879782983) + ((2 + 15 + 4) * (4 + 15 + 11) * (7 + 15 + 9)) + 211 + ((16 > 63733) * 44586) + (14 + 1) + max(3511, 10) + max(12718799, 171629, 9, 264022477, 98) + (535767221581 * ((12 + 10 + 11) < (11 + 2 + 9))) + min(391364, 3067700, 611, 2388317100, 141) + ((237 < 237) * 3462) + (139 * 53 * 151) + 7 + (12998225 + 689 + 2007 + 5 + 826) + 547 + (193 * (1458 < 138)) + ((277997 == 12) * 37675211133) + 11 + ((955 > 2610) * 67374456587) + (886515 + 86 + 254) + 3)

  (lisp-unit:assert-equal 180616437720 (rv "220d4a8012e004a8012a004a8012a004b8012e004b8012a004a8012a004b8012a004cb5f13bff003085c00997fdf8326008ca6e233a008c867117190f7250a70444019300661cc820c8389803320830464184c01984320a3066430326008e8023292cb0c86f43321b9f9464352a4824c011b004643167990c59e6431ddd1e4c021906990e20ca90cb90c83cca80980232af3a008c11948c164c01190c4e78eb0e8022200cc828c838c11100664106412641664f36515008618c96e5521910990ea8980236008c86f07326d32f3b6271875e4c011b004440198530461888033209320c30464318f80b804322532083219febb74995ac0c4c011957990d04980236008c8b0c8b0ca5a3be7f2e26008cb18d8022200cc1184320b2200cc828c83cc828a802321f319688898023e008caedc65b12e692643a02a00cc975064bb8b76967990e7f10ae3964930046c01195d52df7990ced2652dba009805321e319561974190eb0c865e4c011f00444019905985320b2200cc83cc838c828c87256495002656f6cc3d771700665a65321d31906930047401190d4b73d58264352dcf56099385bee329a8309802321bdf31afa8f80232e032e022010c87f75edef10190ddb14946d13196a610643d7a439aa1c98032200cc119079842200cc2190799059100661cc83cc824c87464c011b0046440643f8f94c86bd2511004641c604b802321dd9cc828b805321c9465c7992cfacd3209321fdf359e34cb08980232fcdfa778568d3a0088803320c320a320b2200cc82cc119049500a657f8e10c97b9f990c95866431eaebb3d30c861a4c011d004643cd321e6990ee0c4c01990c2cc994c864e61c8805321cb56d50992d84cbf4e614c9cd464a4326008c87027401195d88c86144c011f0046527df34c830c86395cfa39bb4c82c980236008c9ed6643531190febfda67f0591006643b8b7c6654c643ee306"))
  (lisp-unit:assert-equal 180616437720 (vs "220d4a8012e004a8012a004a8012a004b8012e004b8012a004a8012a004b8012a004cb5f13bff003085c00997fdf8326008ca6e233a008c867117190f7250a70444019300661cc820c8389803320830464184c01984320a3066430326008e8023292cb0c86f43321b9f9464352a4824c011b004643167990c59e6431ddd1e4c021906990e20ca90cb90c83cca80980232af3a008c11948c164c01190c4e78eb0e8022200cc828c838c11100664106412641664f36515008618c96e5521910990ea8980236008c86f07326d32f3b6271875e4c011b004440198530461888033209320c30464318f80b804322532083219febb74995ac0c4c011957990d04980236008c8b0c8b0ca5a3be7f2e26008cb18d8022200cc1184320b2200cc828c83cc828a802321f319688898023e008caedc65b12e692643a02a00cc975064bb8b76967990e7f10ae3964930046c01195d52df7990ced2652dba009805321e319561974190eb0c865e4c011f00444019905985320b2200cc83cc838c828c87256495002656f6cc3d771700665a65321d31906930047401190d4b73d58264352dcf56099385bee329a8309802321bdf31afa8f80232e032e022010c87f75edef10190ddb14946d13196a610643d7a439aa1c98032200cc119079842200cc2190799059100661cc83cc824c87464c011b0046440643f8f94c86bd2511004641c604b802321dd9cc828b805321c9465c7992cfacd3209321fdf359e34cb08980232fcdfa778568d3a0088803320c320a320b2200cc82cc119049500a657f8e10c97b9f990c95866431eaebb3d30c861a4c011d004643cd321e6990ee0c4c01990c2cc994c864e61c8805321cb56d50992d84cbf4e614c9cd464a4326008c87027401195d88c86144c011f0046527df34c830c86395cfa39bb4c82c980236008c9ed6643531190febfda67f0591006643b8b7c6654c643ee306"))

  ;;(min(max(min(min(min(min((max(max(max(min(((min(min((max(((min(1736671104)))))))))))))))))))) + 4 + max(522179) + (19219 * (38942 < 247596226)) + ((7 * 8 * 14) + (8 * 2 * 12) + (4 * 10 * 3)) + 131 + ((17052 < 2979) * 46058) + (664609) + ((2159 > 2159) * 580431) + (13 * 200 * 84 * 116) + 15 + (80) + (95 * (2 < 16651)) + (8599772 * ((10 + 14 + 2) < (8 + 9 + 11))) + 14698 + min(6, 176724, 17, 218) + ((2951 > 61) * 30979330271) + (((5 + 2 + 6) > (9 + 12 + 2)) * 35056) + max(21, 8, 10464633, 21766) + (95 * 161) + ((28 > 28) * 1176227742) + (102 * ((2 + 4 + 11) > (10 + 15 + 10))) + min(246, 1554) + ((1470 == 6827177) * 208) + min(728, 763810927, 55700720745) + ((5916415 > 2489) * 289600) + (230 * 92 * 120 * 220 * 159) + (((11 + 5 + 11) == (15 + 14 + 10)) * 49769) + min(1466502782) + max(1589, 214, 13) + ((170605153 < 170605153) * 198398) + 18956 + (196905722 * (112 == 112)) + (4217206544 + 48993801750 + 25736 + 3879782983) + ((2 + 15 + 4) * (4 + 15 + 11) * (7 + 15 + 9)) + 211 + ((16 > 63733) * 44586) + (14 + 1) + max(3511, 10) + max(12718799, 171629, 9, 264022477, 98) + (535767221581 * ((12 + 10 + 11) < (11 + 2 + 9))) + min(391364, 3067700, 611, 2388317100, 141) + ((237 < 237) * 3462) + (139 * 53 * 151) + 7 + (12998225 + 689 + 2007 + 5 + 826) + 547 + (193 * (1458 < 138)) + ((277997 == 12) * 37675211133) + 11 + ((955 > 2610) * 67374456587) + (886515 + 86 + 254) + 3)

  (lisp-unit:assert-equal 180616437720 (rv "820D4A801EE00720190CA005201682A00498014C04BBB01186C040A200EC66006900C44802BA280104021B30070A4016980044C800B84B5F13BFF007081800FE97FDF830401BF4A6E239A009CCE22E53DC9429C170013A8C01E87D102399803F1120B4632004261045183F303E4017DE002F3292CB04DE86E6E7E54100366A5490698023400ABCC59E262CFD31DDD1E8C0228D938872A472E471FC80082950220096E55EF0012882529182D180293139E3AC9A00A080391563B4121007223C4A8B3279B2AA80450DE4B72A9248864EAB1802940095CDE0FA4DAA5E76C4E30EBE18021401B88002170BA0A43000043E27462829318F83B00593225F10267FAEDD2E56B0323005E55EE6830C013B00464592458E52D1DF3F97720110258DAC0161007A084228B0200DC568FB14D40129F33968891005FBC00E7CAEDD25B12E692A7409003B392EA3497716ED2CFF39FC42B8E593CC015B00525754B7DFA67699296DD018802839E35956397449D66997F2013C3803760004262C4288B40008747E8E114672564E5002256F6CC3D7726006125A6593A671A48043DC00A4A6A5B9EAC1F352DCF560A9385BEED29A8311802B37BE635F54F004A5C1A5C1C40279FDD7B7BC4126ED8A4A368994B530833D7A439AA1E9009D4200C4178FF0880010E8431F62C880370F63E44B9D1E200ADAC01091029FC7CB26BD25710052384097004677679159C02D9C9465C7B92CFACD91227F7CD678D12C2A402C24BF37E9DE15A36E8026200F4668AF170401A8BD05A242009692BFC708A4BDCFCC8A4AC3931EAEBB3D314C35900477A0094F36CF354EE0CCC01B985A932D993D87E2017CE5AB6A84C96C265FA750BA4E6A52521C300467033401595D8BCC2818029C00AA4A4FBE6F8CB31CAE7D1CDDAE2E9006FD600AC9ED666A6293FAFF699FC168001FE9DC5BE3B2A6B3EED060"))
  (lisp-unit:assert-equal 180616437720 (vs "820D4A801EE00720190CA005201682A00498014C04BBB01186C040A200EC66006900C44802BA280104021B30070A4016980044C800B84B5F13BFF007081800FE97FDF830401BF4A6E239A009CCE22E53DC9429C170013A8C01E87D102399803F1120B4632004261045183F303E4017DE002F3292CB04DE86E6E7E54100366A5490698023400ABCC59E262CFD31DDD1E8C0228D938872A472E471FC80082950220096E55EF0012882529182D180293139E3AC9A00A080391563B4121007223C4A8B3279B2AA80450DE4B72A9248864EAB1802940095CDE0FA4DAA5E76C4E30EBE18021401B88002170BA0A43000043E27462829318F83B00593225F10267FAEDD2E56B0323005E55EE6830C013B00464592458E52D1DF3F97720110258DAC0161007A084228B0200DC568FB14D40129F33968891005FBC00E7CAEDD25B12E692A7409003B392EA3497716ED2CFF39FC42B8E593CC015B00525754B7DFA67699296DD018802839E35956397449D66997F2013C3803760004262C4288B40008747E8E114672564E5002256F6CC3D7726006125A6593A671A48043DC00A4A6A5B9EAC1F352DCF560A9385BEED29A8311802B37BE635F54F004A5C1A5C1C40279FDD7B7BC4126ED8A4A368994B530833D7A439AA1E9009D4200C4178FF0880010E8431F62C880370F63E44B9D1E200ADAC01091029FC7CB26BD25710052384097004677679159C02D9C9465C7B92CFACD91227F7CD678D12C2A402C24BF37E9DE15A36E8026200F4668AF170401A8BD05A242009692BFC708A4BDCFCC8A4AC3931EAEBB3D314C35900477A0094F36CF354EE0CCC01B985A932D993D87E2017CE5AB6A84C96C265FA750BA4E6A52521C300467033401595D8BCC2818029C00AA4A4FBE6F8CB31CAE7D1CDDAE2E9006FD600AC9ED666A6293FAFF699FC168001FE9DC5BE3B2A6B3EED060"))
  
  (lisp-unit:assert-equal 912901337844 (version-sum "0054FEC8C54DC02295D5AE9B243D2F4FEA154493A43E0E60084E61CE802419A95E38958DE4F100B9708300466AB2AB7D80291DA471EB9110010328F820084D5742D2C8E600AC8DF3DBD486C010999B44CCDBD401C9BBCE3FD3DCA624652C400007FC97B113B8C4600A6002A33907E9C83ECB4F709FD51400B3002C4009202E9D00AF260290D400D70038400E7003C400A201B01400B401609C008201115003915002D002525003A6EB49C751ED114C013865800BFCA234E677512952E20040649A26DFA1C90087D600A8803F0CA1AC1F00042A3E41F8D31EE7C8D800FD97E43CCE401A9E802D377B5B751A95BCD3E574124017CF00341353E672A32E2D2356B9EE79088032AF005E7E8F33F47F95EC29AD3018038000864658471280010C8FD1D63C080390E61D44600092645366202933C9FA2F460095006E40008742A8E70F80010F8DF0AA264B331004C52B647D004E6EEF534C8600BCC93E802D38B5311AC7E7B02D804629DD034DFBB1E2D4E2ACBDE9F9FF8ED2F10099DE828803C7C0068E7B9A7D9EE69F263B7D427541200806582E49725CFA64240050A20043E25C148CC600F45C8E717C8010E84506E1F18023600A4D934DC379B9EC96B242402504A027006E200085C6B8D51200010F89913629A805925FBD3322191A1C45A9EACB4733FBC5631A210805315A7E3BC324BCE8573ACF3222600BCD6B3997E7430F004E37CED091401293BEAC2D138402496508873967A840E00E41E99DE6B9D3CCB5E3F9A69802B2368E7558056802E200D4458AF1180010A82B1520DB80212588014C009803B2A3134DD32706009498C600664200F4558630F840188E11EE3B200C292B59124AFF9AE6775ED8BE73D4FEEFFAD4CE7E72FFBB7BB49005FB3BEBFA84140096CD5FEDF048C011B004A5B327F96CC9E653C9060174EA0CF15CA0E4D044F9E4B6258A5065400D9B68"))

  )
(lisp-unit:run-tests '(part2-complex-tests))

(defparameter *interp-good* 'nil)
(defparameter *interp-bad* 'nil)

;;;;;;;final answers
(multiple-value-bind (v bits expr) (version-sum "220d4ca048804326232185990cff39a4e3b11948a0cb1ca805321af18c8698610c9f2c63256c5584c011833a008c862b987f8932b58e24d26008e80232996194cb0cbb098023e008c9de26321bd6b927990f4ea626008cb4ca7401195cb8cae5c4c01190d24ca00cad564c011b0046433e2990fc0065632bd7f1b566f2200cc830c87e09321e3130086512659c64364321c61300474011957195d30ca4c84c01190c4e91b6c66c01190ed31e0990ed31e097002646f286eaa32ea26008c877ff21d004647b198cc87a092a010c86b16fc6121990cca962987325696e3ea10b8023294418226008cbe7bd01b00464166430626008d802323e8d7f393cc8fa35fce4f3218c8c9805321d3990da0c299319930910064c0199041873064c0198430a60c980330a60cc2130047c01195ec08641464392bdc04401190fe5e643fcdfa93e4566438a26008d8022200cc29904990791006608c828c828c29300464377ec695c6b7211f0046433f1990cfc664357a568d26008c865474011100660cc83cc39100660cc21832600cc86d0645e643652e014c866bdc7656a0cc9db7caef726990e7638f62fc5997979a64e3a19300464106c01190fa4c879065b73117008618c97a73c4d064377b24c87bf796bd0904c87f5284f1c19300465b3819b004656f8d34c83cc87adb61500464384321ba989802321f96d074011929da893290ff6f5e289802321ebcb033e0088803320e30660c880330e641a61498023a008880330e608c8308803304641c618c873e53025c019906993c1cc86b58d5bed2a00ccbacc872958dfd20195e5f187130046c0111006614c828c8308803320b30660cc8b4a80132f44193dee76b7f4422014cb80cac7d0924ca7686320e320932d90930064401983320b30e4401982308641c4401985320e30a540097002540097002643d938")
  (setf *interp-good* expr) v)
(length *interp-good*)
(write *interp-good*)
(multiple-value-bind (v bits expr)(version-sum "00569F4A0488043262D30B333FCE6938EC5E5228F2C78A017CD78C269921249F2C69256C559CC01083BA00A4C5730FF12A56B1C49A480283C0055A532CF2996197653005FC01093BC4CE6F5AE49E27A7532200AB25A653800A8CAE5DE572EC40080CD26CA01CAD578803CBB004E67C573F000958CAF5FC6D59BC8803D1967E0953C68401034A24CB3ACD934E311004C5A00A4AB9CAE99E52648401F5CC4E91B6C76801F59DA63C1F3B4C78298014F91BCA1BAA9CBA99006093BFF916802923D8CC7A7A09CA010CD62DF8C2439332A58BA1E495A5B8FA846C00814A511A0B9004C52F9EF41EC0128BF306E4021FD005CD23E8D7F393F48FA35FCE4F53191920096674F66D1215C98C49850803A600D4468790748010F8430A60E1002150B20C4273005F8012D95EC09E2A4E4AF7041004A7F2FB3FCDFA93E4578C0099C52201166C01600042E1444F8FA00087C178AF15E179802F377EC695C6B7213F005267E3D33F189ABD2B46B30042655F0035300042A0F47B87A200EC1E84306C801819B45917F9B29700AA66BDC7656A0C49DB7CAEF726C9CEC71EC5F8BB2F2F37C9C743A600A442B004A7D2279125B73127009218C97A73C4D1E6EF64A9EFDE5AF4241F3FA94278E0D9005A32D9C0DD002AB2B7C69B23CCF5B6C280094CE12CDD4D0803CF9F96D1F4012929DA895290FF6F5E2A9009F33D796063803551006E3941A8340008743B8D90ACC015C00DDC0010B873052320002130563A4359CF968000B10258024C8DF2783F9AD6356FB6280312EBB394AC6FE9014AF2F8C381008CB600880021B0AA28463100762FC1983122D2A005CBD11A4F7B9DADFD110805B2E012B1F4249129DA184768912D90B2013A4001098391661E8803D05612C731007216C768566007280126005101656E0062013D64049F10111E6006100E90E004100C1620048009900020E0006DA0015C000418000AF80015B3D938")
  (setf *interp-bad* expr))

(write *interp-bad*)
(length *interp-bad*)


;;(format t "interp-good: ~A~%~%interp-bad: ~A~%" *interp-good* *interp-bad*)
