

(defparameter *matchstring* '"^([a-z]+) ([a-z]+) bags contain ([0-9]) ([a-z]+) ([a-z]+) (bag|bags).$")
(defparameter *matchstring* '"^([a-z]+) ([a-z]+) bags contain ([0-9]) ([a-z]+) ([a-z]+) (bag|bags)((.)|(, ([0-9]) ([a-z]+) ([a-z]+) (bags|bag).))$")
(defparameter *matchstring* '"^([a-z]+) ([a-z]+) bags contain ((no other bags.)|(([0-9]) ([a-z]+) ([a-z]+) (bag|bags)((.)|(, ([0-9]) ([a-z]+) ([a-z]+) (bags|bag).))))$")

(defparameter *matchstring* '"^([a-z]+) ([a-z]+) bags contain ([0-9]) ([a-z]+) ([a-z]+) (bag|bags)(, ([0-9]) ([a-z]+) ([a-z]+) (bags|bag))*.$")

(defparameter *matchstring* '"^([a-z]+) ([a-z]+) bags contain CONTAINSTR.$")
(defparameter CONTAINSTR '"((no other bags)|(LISTOFBAGS))"
(defparameter LISTOFBAGS '"((BAGCLAUSE)|(BAGCLAUSE), (LISTOFBAGS))"
(defparameter BAGCLAUSE '"(([0-9]) ([a-z]+) ([a-z]+) (bag|bags))"

  (defparameter *BAGSTRING*)
  
(defun read-bagstring (bstr)
  (let* (
         ()
         )
    (setq bagtype (read-bagtype-from-bagstring))
    (setq 
  
(length *allrecords*)
(cl-ppcre:scan-to-strings *matchstring* (nth 0 *allrecords*))
(cl-ppcre:scan-to-strings *matchstring* (nth 1 *allrecords*))
(cl-ppcre:scan-to-strings *matchstring* (nth 2 *allrecords*))
(cl-ppcre:scan-to-strings *matchstring* (nth 3 *allrecords*))
(cl-ppcre:scan-to-strings *matchstring* (nth 4 *allrecords*))
(cl-ppcre:scan-to-strings *matchstring* (nth 5 *allrecords*))
(cl-ppcre:scan-to-strings *matchstring* (nth 6 *allrecords*))
(cl-ppcre:scan-to-strings *matchstring* (nth 7 *allrecords*))
(cl-ppcre:scan-to-strings *matchstring* (nth 8 *allrecords*))

(cl-ppcre:scan-to-strings *matchstring* (nth 0 *allrecords*))
(defun proc-bstr (bstr)
  (progn
    (write bstr)
    (list
     (format nil "~A ~A" (svref bstr 0) (svref bstr 1))
     (if (svref bstr 5) (list (parse-integer (svref bstr 5)) (format nil "~A ~A" (svref bstr 6)(svref bstr 7))) 'nil)
     (if (svref bstr 12) (list (parse-integer (svref bstr 12)) (format nil "~A ~A" (svref bstr 13)(svref bstr 14))) 'nil)
     (if (svref bstr 19) (list (parse-integer (svref bstr 19)) (format nil "~A ~A" (svref bstr 20)(svref bstr 21))) 'nil)
     (if (svref bstr 26) (list (parse-integer (svref bstr 26)) (format nil "~A ~A" (svref bstr 27)(svref bstr 28))) 'nil)
     )))

(defun parse-line-r (line)
  (let* (
         (astr nil)
         (bstr nil)
         )
    (setf (values astr bstr)
          (cl-ppcre:scan-to-strings *matchstring* line))
    (proc-bstr bstr)))


(parse-line-r (nth 0 *allrecords*))
(parse-line-r (nth 1 *allrecords*))
(parse-line-r (nth 2 *allrecords*))
(parse-line-r (nth 3 *allrecords*))
(parse-line-r (nth 4 *allrecords*))
(parse-line-r (nth 5 *allrecords*))
(parse-line-r (nth 6 *allrecords*))
(parse-line-r (nth 7 *allrecords*))
(parse-line-r (nth 8 *allrecords*))

(defparameter *allrecordsparsed*
  (mapcar #'parse-line-r *allrecords*))


*allrecordsparsed*

(defun matchp (var list)
  (mapcar (lambda (p) (string= var (nth 1 p))) (cdr list)))


(matchp "shiny gold" (nth 0 *allrecordsparsed*))
(matchp "bright white" (nth 0 *allrecordsparsed*))

(defun getcontaining (bagstr thelist)
  (if (some (lambda (p) (not (not p))) (matchp bagstr thelist))
      (car thelist)
      'nil))
  
(some (lambda (p) (not (not p))) (matchp "shiny gold" (nth 2 *allrecordsparsed*)))

(getcontaining "bright white" (nth 1 *allrecordsparsed*))
(getcontaining "shiny gold" (nth 2 *allrecordsparsed*))

(nth 0 *allrecordsparsed*)
(nth 1 *allrecordsparsed*)
(nth 2 *allrecordsparsed*)

(defun allcontaining (q)
  (remove 'nil (mapcar (lambda (p) (getcontaining q p)) *allrecordsparsed*)))

(defun concat-lists (seq1 seq2)
  (if (null seq1)
      seq2
      (cons (car seq1) (concat-lists (cdr seq1) seq2))))

(defun mapcontaining (a)
  (let* ( (level1 (allcontaining a))
          )
    (concat-lists level1 (mapcar #'allcontaining (allcontaining "shiny gold")))))
    
(length (remove-duplicates (flatten (mapcontaining "shiny gold"))))

(allcontaining "bright white")
(allcontaining "muted yellow")
(allcontaining "light red")
(allcontaining "dark orange")
