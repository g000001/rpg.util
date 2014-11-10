;;;; rpg.util.asd -*- Mode: Lisp;-*- 

(cl:in-package :asdf)


(defsystem :rpg.util
  :serial t
  :depends-on (:rpg.match-macro)
  :components ((:file "package")
               (:file "rpg.util")
               (:file "test")))


(defmethod perform ((o test-op) (c (eql (find-system :rpg.util))))
  (load-system :rpg.util)
  (or (flet (($ (pkg sym)
               (intern (symbol-name sym) (find-package pkg))))
        (let ((result (funcall ($ :fiveam :run) ($ :rpg.util.internal :rpg.util))))
          (funcall ($ :fiveam :explain!) result)
          (funcall ($ :fiveam :results-status) result)))
      (error "test-op failed") ))


;;; *EOF*
