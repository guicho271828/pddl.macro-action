#|
  This file is a part of pddl.macro-action project.
  Copyright (c) 2014 guicho
|#

(in-package :cl-user)
(defpackage pddl.macro-action
  (:use :cl :pddl :iterate :alexandria :trivia
        :guicho-utilities)
  (:shadowing-import-from :iterate :maximize :minimize)
  (:export
   :merge-ground-actions
   :macro-action
   :decode-plan
   :originals
   :ground-macro-action
   :zero-length-plan
   :constants-in-macro
   :objects-in-macro
   :lift-action
   :parameter-not-found
   :lift
   :ground
   :nullary-macro-action
   :conflict))
(in-package :pddl.macro-action)

;; blah blah blah.

