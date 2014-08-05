#|
  This file is a part of pddl.macro-action project.
  Copyright (c) 2014 guicho
|#

(in-package :cl-user)
(defpackage :pddl.macro-action.test
  (:use :cl
        :iterate
        :pddl
        :pddl.macro-action
        :fiveam)
  (:shadow :maximize :minimize))
(in-package :pddl.macro-action.test)



(def-suite :pddl.macro-action)
(in-suite :pddl.macro-action)

;; run test with (run! test-name) 
;;   test as you like ...


