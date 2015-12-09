#|
  This file is a part of pddl.macro-action project.
  Copyright (c) 2014 guicho
|#


(in-package :cl-user)
(defpackage pddl.macro-action.test-asd
  (:use :cl :asdf))
(in-package :pddl.macro-action.test-asd)


(defsystem pddl.macro-action.test
  :author "guicho"
  :license ""
  :depends-on (:pddl.macro-action
               :fiveam)
  :components ((:module "t"
                :components
                ((:file "package")
                 (:file "merge"))))
  :perform (test-op :after (op c) (eval (read-from-string "(fiveam:run! :pddl.macro-action)"))))
