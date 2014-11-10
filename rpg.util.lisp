;;;; rpg.util.lisp -*- Mode: Lisp;-*- 

(cl:in-package :rpg.util.internal) 


;; (in-readtable :rpg.util)


;;; "rpg.util" goes here. Hacks and glory await!


;;; Reads from disk until an end of file is encountered. ?step
;;; is bound to the input object, and form is performed after each read.
#|(match-macro (read-until-eof) (with ?step do *form)
 ((lambda (*form1 *form2)
	(%match '(*form1 return *form2) *form)
	(code ((lambda (eof)
	       (do ((?step (read eof)(read eof)))
		   ((eq ?step eof) *form2)
                *form1))
               (list nil))))
  *form nil))|#

#|(with-input-from-string (*standard-input* "foo bar baz")
  (read-until-eof in))|#

;;; While loop
;; (declare (special ?cond *form *form1 *form2))
(match-macro (while) (?cond do *form)
 ((lambda(*form1 *form2)
   (%match '(*form1 return *form2) *form)
   (cond ((or (%match '(not ?x) ?cond)
	      (%match '(null ?x) ?cond))
	  (code (do () (?x *form2) *form1)))
	 (t
	  (code (do () ((not ?cond) *form2) *form1))))) 
  *form nil)) 


;;; Until loop
(match-macro (until) (?cond do *form)
 ((lambda(*form1 *form2)
	(%match '(*form1 return *form2) *form)
	(cond ((null *form2) (setq *form2 '(nil))))
	(code (prog nil
		loop *form1
		     (cond (?cond (return (progn *form2)))
			   (t (go loop)))))) 
   *form nil)) 


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun q (vars form)
    (cond ((null form) nil)
          ((member form vars) form)
          ((consp (car form))
           (cons (q vars (car form))
                 (q vars (cdr form))))
          ((member (car form) vars)
           (cons (car form)
                 (q vars (cdr form))))
          (T (list* 'list
                    (list 'quote (car form))
                    (q vars (cdr form)))))))


(defmacro macrodef (name (&rest args) &body body)
  `(defmacro ,name (,@args)
     (progn
       ,@(cl:let ((args (set-difference args lambda-list-keywords)))
           (q args body)))))


;;; (princ x)
(macrodef tell (mess)(princ mess)) 


(match-macro (if) (*form1 then *form2)
  (cond ((%match '(*form2 else *form3) *form2)
         (code (cond (*form1 *form2)
                     (t *form3))))
        (t (code (cond (*form1 *form2)))))) 


(match-macro (spc) (?n)
 ((lambda (?n)
	(cond (?n `(repeat ,?n do (tyo 32.)))
	      (t `(tyo 32.))))
 (and (not (atom ?n)) (car ?n)))) 


#|(match-macro (until) (?cond do *form)
 ((lambda(*form1 *form2)
    (%match '(*form1 return *form2) *form)
    (cond ((null *form2) (setq *form2 '(nil))))
    `(prog nil
        loop ,@*form1
           (cond (,?cond (return (progn ,@*form2)))
                 (t (go loop))))) 
  *form nil))|# 


#|(match-macro (repeat) (?n do *form)
       ((lambda (*form1 *form2)
		(%match '(*form1 return *form2) *form)
		(cond ((atom ?n)
		       (code (or (< ?n 0)
				 (do ((%%%step%%% 0 (1+ %%%step%%%)))
				      ((= %%%step%%% ?n) *form2)
				      *form1))))
		       (t (code ((lambda (%%%stop%%%)
					  (or (< %%%stop%%% 0)
					      (do ((%%%step%%% 0 (1+ %%%step%%%)))
						   ((= %%%step%%% %%%stop%%%) *form2)
						    *form1))) ?n)))))
			  *form nil))|# 


(match-macro (repeat) (?n do *form)
  ((lambda (*form1 *form2)
     (%match '(*form1 return *form2) *form)
     (cond ((atom ?n)
            (code (or (< ?n 0)
                      (do ((%%%step%%% 0 (1+ %%%step%%%)))
                          ((= %%%step%%% ?n) *form2)
                        *form1))))
           (t (code ((lambda (%%%stop%%%)
                       (or (< %%%stop%%% 0)
                           (do ((%%%step%%% 0 (1+ %%%step%%%)))
                               ((= %%%step%%% %%%stop%%%) *form2)
                             *form1))) ?n)))))
   *form nil)) 


#|(*:declare-indentation match-macro 2 &body)|#

;;; does n terpri's
(match-macro (newline) ?n
  ((lambda (?n)
     (cond (?n (code (repeat ?n do (terpri))))
           (t (code (terpri)))))
   (and (not (atom ?n)) (car ?n)))) 


#|(newline 10)|# 

(match-macro (let) (?var ← ?val do *body)
  (code (cl:let ((?var ?val)) *body)))



;;; Similar to Interlisp SELECTQ (uses equal)
(match-macro (select) (?item *list ?else)
 (let item ← (cond ((atom ?item) ?item) (t '%%temp%%)) do
 (let *clauses ← (for i ∈ *list collect
		      (cons (list 'equal item
				  (list 'quote (car i)))
			    (cdr i)))
      do
  (cond ((atom ?item)
	 (code (cond *clauses (t ?else))))
        (t (code
	   ((lambda (%%temp%%)
	    (cond *clauses (t ?else)))
            ?item)))))) )


;;; Similar to Interlisp SELECTQ (uses = instead of eq)
(match-macro (select=) (?item *list ?else)
 (let item ← (cond ((atom ?item) ?item) (t '%%temp%%)) do
 (let *clauses ← (for i ∈ *list collect
  		  (cond ((numberp (car i)) (cons (list '= item
					   	     (car i))
					      (cdr i)))
			(t (cons (list 'member item (list 'quote (car i)))
				 (cdr i))))) do
  (cond ((atom ?item)
	 (code (cond *clauses (t ?else))))
        (t (code
	    ((lambda (%%temp%%)
	    (cond *clauses (t ?else)))
            ?item)))))) )

;;; Similar to Interlisp SELECTQ (uses %match instead of eq)
(match-macro (select-match) (?item *list ?else)
 (let item ← (cond ((atom ?item) ?item) 
		   (t '%%temp%%)) 
      do
      (let *clauses ← (for i ∈ *list collect
			   (cons
			    (list '%match 
				  (list 'quote (car i)) item)
			    (cdr i))) 
	   do
	   (cond ((atom ?item)
		  (code (cond *clauses (t ?else))))
		 (t (code
		     ((lambda (%%temp%%)
			      (cond *clauses (t ?else)))
		      ?item)))))) )


;;; (FOR X {IN,ε} L {DO,APPEND,COLLECT,CONC,SELECT,SCAN} form {RETURN} form)
;;; (FOR X FROM LOWER TO UPPER {by STEPPER} DO form {return} form)

#|(declare (special *x ?prep ?step ?lower ?upper ?stepper *l ?verb *x *form1 *form2))|#


;; (for x in '(1 2 3 4 nil 5) append x) 


;=>  (1 2 3 4 5)


;(for x ∈ '(1 2 3 4) select (oddp x))(for x from 1 to 5 by 2 do x)

 
(defun for-in-from-memq (q)
  (member q '(∈ in from))) 


(defun for-do-collect-append-etc-memq (q)
  (member q '(do collect append conc select to scan))) 


(match-macro (multi-for) 
             (*x 
	      ($r ?prep for-in-from-memq)  
	      *l 
	      ($r ?verb for-do-collect-append-etc-memq)
	      *form)
  ;; (print (list ?verb *l))
  (cons (cond ((member ?verb '(do scan)) 'progn)
              (t 'append))
        (mapcan (lambda (x l)
                  (mapcar (lambda (i)
                            `(for ,x ∈ ,i ,?verb . ,*form))
                          l))
                *x *l))) 


(defun ncons (x)
  (list x)) 


(match-macro (for) 
             (*x 
              ($r ?prep for-in-from-memq)
              *l 
              ($r ?verb for-do-collect-append-etc-memq)
              *form)
  ;; (print (list :x *x :prep ?prep :verb ?verb :form *form))
  ((lambda (*form1 *form2 ?step ?lower ?upper ?stepper)
     (%match '(*form1 return *form2) *form)
     (case ?verb 
       (append
        (cond (*form2 
               (code (progn (mapcan (lambda (*x)
                                      ((lambda (q) (cond (q (ncons q))))
                                       (progn *form1)))
                                    *l)
                            *form2)))
              (t (code (mapcan (lambda(*x)
                                 ((lambda(q)
                                    (cond (q (ncons q))))
                                  (progn *form1))) *l)))))  
       (conc
        (cond (*form2
               (code (progn (mapcan (lambda (*x)
                                      (progn *form1)) *l) *form2)))
              (t (code (mapcan (lambda (*x)
                                 (progn *form1)) *l)))))
       (select
           (cond (*form2
                  (code (progn (mapcan (lambda (*x)
                                         (and (progn *form1) (list *x))) *l) 
                               *form2)))
                 (t (code (mapcan (lambda (*x)
                                    (and (progn *form1) (list *x))) *l)))))

       (scan
        ((lambda (?steppers aux *setqs ?ender)
           (setq ?ender
                 ((lambda (q)
                    (cond ((= (length q) 1)
                           (car q))
                          (t (cons 'or q))))
                  (mapcar (lambda (x)
                            (list 'null (car x)))
                          ?steppers)))   
           (setq *setqs 
                 (mapcan (lambda (x y)
                           (cond (x
                                  `((setq
                                     ,x (car ,(car y))))))) 
                         *x ?steppers))
           (setq ?steppers (nconc ?steppers aux))
           (code (do ?steppers
                     (?ender *form2)
                   *setqs
                   *form1)))
         (mapcar (lambda (x)
                   ((lambda (q)
                      (list q
                            x (list 'cdr q)))
                    (gensym)))
                 *l)
         (mapcar #'ncons *x)
         ()
         ()))
       (t (cond ((and (eq ?prep 'from)
                      (eq ?verb 'to))
                 (cond (*form2
                        (or (%match '(?upper by ?stepper do *form1 return *) *form)
                            (%match '(?upper do *form1 return *) *form) ))
                       (t (or (%match '(?upper by ?stepper do *form1) *form)
                              (%match '(?upper do *form1) *form) )) )
                 (cond (?stepper
                        (code (do ((?step ?lower (+ ?step ?stepper)))
                                  ((< ?upper ?step) *form2) *form1)))
                       (t (code (do ((?step ?lower (1+ ?step)))
                                    ((< ?upper ?step) *form2)
                                  *form1))))) 
                (t ((lambda (?verb)
                      (cond (*form2 
                             (code (progn (?verb (lambda (*x) *form1) *l) *form2)))
                            (t
                             (code (?verb (lambda (*x) *form1) *l) )))) 
                    (cond ((eq ?verb 'do) 'mapc)
                          (t 'mapcar))))))))  
   *form nil (car *X) (car *l) nil nil)) 


#|||

;;; The following is a file of useful Maclisp macro definitions.
(setq %%%ofasload%%% fasload) 


(setq fasload ()) 


(declare (cond ((eq (cadr (status uname)) 'rpg)
		(setq no-disk-hacks t)))) 


(declare (mapex t)) 


(declare (special ↑R ↑W ↑T ↑Q)
	 (*expr macrobind)) 


;;; Selects the disk for input
(declare (special x)) 


(macrodef select-disk-input x
	((lambda(↑Q) . x) t)) 


(macrodef unselect-disk-input x
	((lambda(↑Q) . x) nil)) 


;;; Selects the disk for output
;(macrodef select-disk-output x
;	(lambda(↑R ↑W . x) t t))
(macrodef select-disk-output x
	((lambda(↑R) . x) t)) 


(macrodef unselect-disk-output x
	((lambda(↑R) . x) nil)) 


;;; Unselects the tty for output
(macrodef unselect-tty x
	((lambda(↑W) . x) t)) 


;;; Selects the tty for output
(macrodef select-tty x
	((lambda(↑W) . x) nil)) 


;;; Reads from disk until an end of file is encountered. ?step
;;; is bound to the input object, and form is performed after each read.
(match-macro (read-until-eof) (with ?step do *form)
 ((lambda (*form1 *form2)
	(%match '(*form1 return *form2) *form)
	(code ((lambda (eof)
	       (do ((?step (read eof)(read eof)))
		   ((eq ?step eof) *form2)
		   *form1)) (list nil))))
  *form nil)) 


;;; TYI's from disk until an end of file is encountered. ?step
;;; is bound to the input object, and form is performed after each read.
(match-macro (tyi-until-eof) (with ?step do *form)
 ((lambda (*form1 *form2)
	(%match '(*form1 return *form2) *form)
	(code 
	 (progn (eoffn infile 
		       (function 
			(lambda 
			 (()
			  b)(close infile) b)))
	       (do ((?step (tyi infile -1)(tyi infile -1)))
		   ((= ?step -1) *form2)
		   *form1))))
  *form nil)) 


;;; While loop
(declare (special ?cond *form *form1 *form2)) 


(match-macro (while) (?cond do *form)
 ((lambda(*form1 *form2)
   (%match '(*form1 return *form2) *form)
   (cond ((or (%match '(not ?x) ?cond)
	      (%match '(null ?x) ?cond))
	  (code (do () (?x *form2) *form1)))
	 (t
	  (code (do () ((not ?cond) *form2) *form1))))) 
  *form nil)) 


;;; Until loop
(match-macro (until) (?cond do *form)
 ((lambda(*form1 *form2)
	(%match '(*form1 return *form2) *form)
	(cond ((null *form2) (setq *form2 '(nil))))
	(code (prog nil
		loop *form1
		     (cond (?cond (return *form2))
			   (t (go loop)))))) 
   *form nil)) 


;;; These functions do some file defaulting and are
;;; similar to DSKIN & DSKOUT in Ilisp. (DSKIN <file>) (DSKOUT <form><file>)

(macrodef %push% (x) (setq pdl (cons x pdl))) 


(macrodef %pop% () (setq file (cdr file))) 


(macrodef %check% () (cond ((null file)(throw (nreverse pdl) out)))) 


(macrodef %default% (filespecs)
(let file ← filespecs do
(cond (file
 (catch (prog (pdl)
	 (%push% (car file))
	 (%pop%)
	 (%push% (cond ((or (null file)
			  (memq (car file) '(dsk sys))
		   	  (not (atom (car file)))) (cond ((status features newio)
							  '|←←←|)
							 (t '| |)))
		     (t (prog2 nil (car file) (%pop%)))))
	 (%check%)
	 (%push% (cond ((atom (car file)) (prog2 nil (car file)(%pop%)))
		     (t 'dsk)))
	 (%check%)
	 (%push% (cond ((= (length (car file)) 2)(car file))
		     (t (list (caar file)(cadr (status udir))))))
	 (throw (nreverse pdl) out))
  out))))) 


(declare (read)) 


(cond ((and (boundp 'no-disk-hacks) no-disk-hacks)
       (read)(read)(read)(read))) 


(declare (cond ((and (boundp 'no-disk-hacks)
		     no-disk-hacks)(read)(read)(read)))) 


(defun dskin fexpr (file)
 ((lambda (ocrunit file)
	(apply 'eread file)
        (select-disk-input
	 (read-until-eof with form do (print (eval form))))
	(terpri)
	(apply 'crunit (list 'dsk ocrunit)))
	(status udir) (%default% file))
	(uclose)
        'Done) 


(defun slurpin fexpr (file)
 (unselect-tty
 (let file ← (%default% file) do
 ((lambda (ocrunit fns atom)
	(apply 'eread file)
        (select-disk-input
	 (read-until-eof with form do (cond ((memq (car form) 
					     '(defun macrodef macro))
					     (setq fns (cons (eval form) fns)))
					    (t (eval form)))))
	(terpri)
	(apply 'crunit (list 'dsk ocrunit)) 
  	(set atom fns)
      	(uclose)
	atom)
	(status udir) nil
	 (intern (implode (append (explode (car file))'(!) (explode (cadr file))
	     '(- f n s)))) )))) 


(defun dskout fexpr (form)
 ((lambda(file ocrunit)
	(apply 'uwrite (cddr file))
	(select-disk-output (eval (car form)))
	(terpri)
	(prog2 nil 
	(apply 'ufile file)
	(apply 'crunit (list 'dsk ocrunit))))
  (%default% (cdr form)) (status udir))) 


;;; (FOR X {IN,ε} L {DO,APPEND,COLLECT,CONC,SELECT,SCAN} form {RETURN} form)
;;; (FOR X FROM LOWER TO UPPER {by STEPPER} DO form {return} form)

(declare (special *x ?prep ?step ?lower ?upper ?stepper *l ?verb *x *form1 *form2)) 


(defun for-in-from-memq
       (q) 
       (memq q '(ε in from))) 


(defun for-do-collect-append-etc-memq (q)
       (memq q '(do collect append conc select to scan))) 


(match-macro (multi-for) 
             (*x 
	      ($r ?prep 
		  for-in-from-memq)  
	      *l 
	      ($r ?verb 
		  for-do-collect-append-etc-memq)
	      *form)
(cons (cond ((memq ?verb '(do scan)) 'progn)
	    (t 'append))
      (mapcan (function (lambda (x l)
				(mapcar
				 (function
				  (lambda (i)
					  `(for ,x ε ,i ,?verb . ,*form)))
				 l)))
	      *x *l))) 


)))))))))

(match-macro (for) (*x 
	    ($r ?prep 
		for-in-from-memq)
	    *l 
	    ($r ?verb 
		for-do-collect-append-etc-memq)
	    *form)
 ((lambda (*form1 *form2 ?step ?lower ?upper ?stepper)
   (%match '(*form1 return *form2) *form)
	(caseq ?verb 
	       (append
	       (cond (*form2 
	        (code (progn (mapcan (function (lambda (*x)
					((lambda (q) (cond (q (ncons q))))
		 			 (progn *form1)))) *l) *form2)))
		(t (code (mapcan (function (lambda(*x)((lambda(q)(cond (q (ncons q))))
					(progn *form1)))) *l)))))  
	      (conc
	       (cond (*form2
	 	(code (progn (mapcan (function (lambda (*x)
						(progn *form1))) *l) *form2)))
		     (t (code (mapcan (function (lambda (*x)
					         (progn *form1))) *l)))))
	      (select
	       (cond (*form2
	 	(code (progn (mapcan (function (lambda (*x)
						(and (progn *form1) (list *x)))) *l) 
					        *form2)))
		     (t (code (mapcan (function (lambda (*x)
					         (and (progn *form1) (list *x)))) *l)))))

	      (scan
	       ((lambda (?steppers aux *setqs ?ender)
			(setq ?ender
			      ((lambda (q)
				       (cond ((= (length q) 1)
					      (car q))
					     (t (cons 'or q))))
			       (mapcar
				(function
				 (lambda (x)
					 (list 'null (car x))))
				?steppers)))   
			(setq *setqs 
			      (mapcan
			       (function 
				(lambda (x y)
					(cond (x
					       `((setq
						  ,x (car ,(car y)))))))) 
			       *x ?steppers))
			(setq ?steppers (nconc ?steppers aux))
			(code (do ?steppers
				  (?ender *form2)
				  *setqs
				  *form1)))
		(mapcar
		 (function
		  (lambda (x)
			  ((lambda (q)
				   (list q
					 x (list 'cdr q)))
			   (gensym))))
		 *l) 
		(mapcar
		 'ncons
		 *x)
		() ()))
		
	      (t (cond
		  ((and (eq ?prep 'from)
			(eq ?verb 'to))
		   (cond (*form2
			  (or (%match '(?upper by ?stepper do *form1 return *) *form)
			      (%match '(?upper do *form1 return *) *form) ))
			 (t (or (%match '(?upper by ?stepper do *form1) *form)
				(%match '(?upper do *form1) *form) )) ) 
		   (cond (?stepper
			  (code (do ((?step ?lower (+ ?step ?stepper)))
				    ((< ?upper ?step) *form2) *form1)))
			 (t (code (do ((?step ?lower (1+ ?step)))
				      ((< ?upper ?step) *form2)
				      *form1))))) 
		  (t ((lambda (?verb)
			      (cond (*form2 
				     (code (progn (?verb (function (lambda (*x) *form1)) *l) *form2)))
				    (t
				     (code (?verb (function (lambda (*x) *form1)) *l) )))) 
		      (cond ((eq ?verb 'do) 'mapc)
			    (t 'mapcar))))))))  
  *form nil (car *X) (car *l) nil nil)) 


;;; (princ x)
(declare (special ?set mess)) 


(macrodef tell (mess)(princ mess)) 


;;; (princ mess)(terpri)
(macrodef speak (mess)
	(progn (princ mess)(terpri))) 


;;; Reads from selected device until input is in a specified set.
(match-macro (read-until) (?step ($r ? (lambda(q) (memq q '(isin ε)))) ?set do *form)
 ((lambda (*form1 *form2)
   (%match '(*form1 return *form2) *form)
   (code (do ((?step (read)(read)))
	     ((member ?step ?set) *form2)
	     *form1)))
  *form nil)) 


;;; Tyis from selected device until input is in a specified set.
(match-macro (inch-until) (?step ($r ? (lambda(q) (memq q '(isin ε)))) ?set do *form)
 ((lambda (*form1 *form2)
   (%match '(*form1 return *form2) *form)
   (code (do ((?step (tyi)(tyi)))
	     ((member ?step ?set) *form2)
	     *form1)))
  *form nil)) 


;;; Loops n times
(declare (special ?n)) 


(match-macro (repeat) (?n do *form)
       ((lambda (*form1 *form2)
		(%match '(*form1 return *form2) *form)
		(cond ((atom ?n)
		       (code (or (< ?n 0)
				 (do ((%%%step%%% 0 (1+ %%%step%%%)))
				      ((= %%%step%%% ?n) *form2)
				      *form1))))
		       (t (code ((lambda (%%%stop%%%)
					  (or (< %%%stop%%% 0)
					      (do ((%%%step%%% 0 (1+ %%%step%%%)))
						   ((= %%%step%%% %%%stop%%%) *form2)
						    *form1))) ?n)))))
			  *form nil)) 


;;; does n terpri's
(match-macro (newline) ?n
 ((lambda (?n)
	(cond (?n (code (repeat ?n do (terpri))))
	      (t (code (terpri)))))
 (and (not (atom ?n)) (car ?n)))) 


;;; does n space's
(match-macro (space) ?n
 ((lambda (?n)
	(cond (?n (code (repeat ?n do (tyo 32.))))
	      (t (code (tyo 32.)))))
 (and (not (atom ?n)) (car ?n)))) 


;;; Prog1
;(macrodef prog1 x (prog2 nil . x))

;;; Push & pop
(declare (special x y)) 


(macrodef push (x y) (setq y (cons x y))) 


(macrodef pop (y) (prog1 (car y) (setq y (cdr y)))) 


(macrodef top (y) (car y)) 


(macrodef ask (mess)
	(princ mess)
	(read)) 


;;; Not similar to Interlisp SELECTQ (uses eq instead of equal)
(declare (special ?item *list ?else)) 


;(match-macro (selectq) (?item *list ?else)
; (let item ← (cond ((atom ?item) ?item) (t '%%temp%%)) do
; (let *clauses ← (for i ε *list collect
;  		  (cond ((atom (car i)) (cons (list 'eq item
;					   	    (list 'quote (car i)))
;					      (cdr i)))
;			(t (cons (list 'memq item (list 'quote (car i)))
;				 (cdr i))))) do
;  (cond ((atom ?item)
;	 (code (cond *clauses (t ?else))))
;        (t (code
;	   ((lambda (%%temp%%)
;	    (cond *clauses (t ?else)))
;            ?item)))))))

;(declare (eval (read)))
;(cond ((status features newio)
;       (cond ((status features complr)
;	      (read))
;	     (t (read (or uread eread-file tyi)))))) 

;(match-macro (caseq) (?item *list ?else)
; (let item ← (cond ((atom ?item) ?item) (t '%%temp%%)) do
; (let *clauses ← (for i ε *list collect
;  		  (cond ((atom (car i)) (cons (list 'eq item
;					   	    (list 'quote (car i)))
;					      (cdr i)))
;			(t (cons (list 'memq item (list 'quote (car i)))
;				 (cdr i))))) do
;  (cond ((atom ?item)
;	 (code (cond *clauses ?else)))
;        (t (code
;	   ((lambda (%%temp%%)
;	    (cond *clauses ?else))
;            ?item)))))))

;;; Similar to Interlisp SELECTQ (uses equal)
(match-macro (select) (?item *list ?else)
 (let item ← (cond ((atom ?item) ?item) (t '%%temp%%)) do
 (let *clauses ← (for i ε *list collect
		      (cons (list 'equal item
				  (list 'quote (car i)))
			    (cdr i)))
      do
  (cond ((atom ?item)
	 (code (cond *clauses (t ?else))))
        (t (code
	   ((lambda (%%temp%%)
	    (cond *clauses (t ?else)))
            ?item)))))) ) 


;;; Similar to Interlisp SELECTQ (uses = instead of eq)
(match-macro (select=) (?item *list ?else)
 (let item ← (cond ((atom ?item) ?item) (t '%%temp%%)) do
 (let *clauses ← (for i ε *list collect
  		  (cond ((numberp (car i)) (cons (list '= item
					   	     (car i))
					      (cdr i)))
			(t (cons (list 'member item (list 'quote (car i)))
				 (cdr i))))) do
  (cond ((atom ?item)
	 (code (cond *clauses (t ?else))))
        (t (code
	    ((lambda (%%temp%%)
	    (cond *clauses (t ?else)))
            ?item)))))) ) 


;;; Similar to Interlisp SELECTQ (uses %match instead of eq)
(match-macro (select-match) (?item *list ?else)
 (let item ← (cond ((atom ?item) ?item) 
		   (t '%%temp%%)) 
      do
      (let *clauses ← (for i ε *list collect
			   (cons
			    (list '%match 
				  (list 'quote (car i)) item)
			    (cdr i))) 
	   do
	   (cond ((atom ?item)
		  (code (cond *clauses (t ?else))))
		 (t (code
		     ((lambda (%%temp%%)
			      (cond *clauses (t ?else)))
		      ?item)))))) ) 


;;; Forever loop. Get out via throw.

(macrodef do-forever x (do nil (nil) . x)) 


;;; If-then-else
(declare (special *form3)) 


;(match-macro (if) (*form1 then *form2 )
; (cond ((%match '(*form2 else *form3) *form2)
;	(code (cond (*form1 *form2)
;		    (t *form3))))
;       (t (code (cond (*form1 *form2))))))

;;; TAIL-RECURSIVE-DEFUN
;;; a macro to provide tail recursive function definitions
;;; EXPRS and FEXPRS only

(EVAL-WHEN (COMPILE EVAL LOAD)

(DEFUN (CCODE MACRO)(X)
  (DO-CODE (CADR X)))

(DEFUN DO-CODE(X)
  (COND ((NULL X)NIL)
	((ATOM X)
	 ((LAMBDA(CHAR1)
	   (COND ((MEMQ CHAR1 '(? *))X)
		 (T (LIST 'QUOTE X))))
	  (GETCHAR X 1)))
	((AND (ATOM (CAR X))(EQ '* (GETCHAR (CAR X) 1)))
	 (LIST 'APPEND (DO-CODE (CAR X)) (DO-CODE (CDR X))))
	(T(LIST 'CONS (DO-CODE (CAR X)) (DO-CODE (CDR X))))))

) 


(DEFUN (TAIL-RECURSIVE-DEFUN MACRO)(X)
  ((LAMBDA(?F-NAME *TYPE)
    ((LAMBDA(*ARGS *DEFINITION)
      ((LAMBDA(?GO-LABEL)
	(α-GRAB-TAILS *ARGS *DEFINITION ?GO-LABEL)
	(CCODE (DEFUN ?F-NAME *TYPE (*ARGS) (PROG NIL
						  ?GO-LABEL
						  (RETURN (PROGN *DEFINITION))))))
       (GENSYM)))
     (COND (*TYPE (CADDDR X))(T (CADDR X)))
     (COND (*TYPE (CDDDDR X))(T (CDDDR X)))))
   (CADR X)
   (COND ((MEMQ (CADDR X) '(EXPR FEXPR))
	  (LIST (CADDR X)))
	 (T NIL)))) 


(DEFUN α-GRAB-TAILS (ARGS DEF ?GO-LABEL)
 (COND ((ATOM DEF)NIL)
       ((AND (ATOM(CAR DEF)) (EQ 'TAIL-RECUR (CAR DEF)))
	(COND ((EQUAL ARGS (CDR DEF))		;calling with same args!
	       (RPLACA DEF 'GO)
	       (RPLACD DEF (LIST ?GO-LABEL)))
	      (T(DO ((ARGS ARGS (CDR ARGS))
		     (NEWARGS (CDR DEF) (CDR NEWARGS))
		     (SETS NIL (NCONC SETS
				      (COND ((EQ (CAR ARGS) (CAR NEWARGS))
					     NIL)
					    (T (NCONS
						((LAMBDA(SYM)
						  (CONS (CONS (CAR ARGS)SYM)
							(LIST 'SETQ
							      (CAR ARGS)
							      (SUBLIS (MAPCAR 'CAR
									      SETS)
								      (CAR NEWARGS)))))
						 (GENSYM))))))))
		    ((NULL ARGS)
		     ((LAMBDA(L-EXP)
		       (RPLACA DEF (CAR L-EXP))
		       (RPLACD DEF (CDR L-EXP)))
		      (α-OPTIMIZE-λ (MAPCAR 'CDAR SETS)
				    (NCONC (MAPCAR 'CDR SETS)
					   (NCONS(LIST 'GO ?GO-LABEL)))
				    (MAPCAR 'CAAR SETS))))))))
       (T(MAPC (FUNCTION(LAMBDA(DEF)
			 (α-GRAB-TAILS ARGS DEF ?GO-LABEL)))
	       DEF)))) 


(DEFUN α-OPTIMIZE-λ (VARS BODY BINDINGS)
  (DO ((VARS VARS (CDR VARS))
       (BINDINGS BINDINGS (CDR BINDINGS))
       (NVARS NIL (NCONC NVARS
			 (COND ((ANY-MEMQ (CAR VARS) BODY)(NCONS (CAR VARS)))
			       (T NIL))))
       (NBINS NIL (NCONC NBINS
			 (COND ((ANY-MEMQ (CAR VARS) BODY)(NCONS (CAR BINDINGS)))
			       (T NIL)))))
      ((NULL VARS)(CONS (CONS 'LAMBDA (CONS NVARS BODY))
			NBINS)))) 


(DEFUN ANY-MEMQ(X Y)
  (COND ((NULL Y)NIL)
	((ATOM Y)(EQ X Y))
	(T(OR (ANY-MEMQ X (CAR Y))
	      (ANY-MEMQ X (CDR Y)))))) 


)

|||#


;;; *EOF* 


