#|
  This file is a part of pddl.macro-action project.
  Copyright (c) 2014 guicho
|#

(in-package :cl-user)
(defpackage pddl.macro-action
  (:use :cl :pddl :iterate :alexandria :optima
        :guicho-utilities)
  (:shadowing-import-from :iterate :maximize :minimize)
  (:export
   :merge-ground-actions
   :macro-action
   :decode-plan))
(in-package :pddl.macro-action)

;; blah blah blah.

