;;;; test.lisp -*- Mode: Lisp;-*- 

(cl:in-package rpg.util.internal)
;; (in-readtable :rpg.util)

(def-suite rpg.util)
(in-suite rpg.util)

(while (zerop (random 3)) do
  (print 'foo))

;>>  
;>>  FOO 
;>>  FOO 
;=>  NIL


(until (zerop (random 3)) do
  (print 'foo))
;>>  
;>>  FOO 
;>>  FOO 
;=>  NIL


(if t then t else nil)
;=>  T

(if t then t)
;=>  T


(until (zerop (random 3)) do
  (print "foo")
  return (print "1")
         (print "2")
         10)
;>>  
;>>  "foo" 
;>>  "foo" 
;>>  "foo" 
;>>  "foo" 
;>>  "foo" 
;>>  "1" 
;>>  "2" 
;=>  10

(select "foo"
  ("bar" "bar")
  ("foo" "foo")
  "baz")
;=>  "foo"

(cl:let (?x ?y ?z)
  (select-match '(1 2 3)
    ((?x ?y ?z) (list ?x ?y ?z))
    :else))
;=>  (1 2 3)


(let list ← '(1 2 3 4) do
  (for x ∈ list collect (list x))) 
;=>  ((1) (2) (3) (4))


(for x from 1 to 5 by 2 do (print x))
;==> (DO ((X 1 (+ X 2))) ((< 5 X)) (PRINT X))
;>>  
;>>  1 
;>>  3 
;>>  5 
;=>  NIL

(for x ∈ '(1 2 3 4) select (oddp x))
;==> (MAPCAN (LAMBDA (X) (AND (PROGN (ODDP X)) (LIST X))) '(1 2 3 4))
;=>  (1 3)

(for x ∈ '(1 2 3 4) scan (print x))
;>>  
;>>  1 
;>>  2 
;>>  3 
;>>  4 
;=>  NIL


(for x ∈ '(1 2 3 4) do (print x))
;>>  
;>>  1 
;>>  2 
;>>  3 
;>>  4 
;=>  (1 2 3 4)




DO,APPEND,COLLECT,CONC,SELECT,SCAN

