;;;; package.lisp -*- Mode: Lisp;-*- 

(cl:in-package :cl-user)


(defpackage :rpg.util
  (:use)
  (:export :if :let))


(defpackage :rpg.util.internal
  (:use :rpg.util :cl :rpg.match-macro :fiveam)
  (:shadowing-import-from :rpg.util :if :let))

