(in-package :pddl.macro-action)

(defmethod pddl::%add-costs ((a macro-action))
  (if (member :action-costs (requirements (domain a)))
      a
  (shallow-copy
   a
   :domain *domain*
   :actions (map 'vector #'remove-costs (actions a))
   :effect `(and ,@(add-list a)
                 ,@(mapcar (lambda (x) `(not ,x)) (delete-list a))
                 ,(parse-numeric-effect
                   `(increase (total-cost) ,(length (actions a)))))
   'add-list +unbound+
   'delete-list +unbound+
       'assign-ops +unbound+)))

(defmethod pddl::%remove-costs ((a macro-action))
  (shallow-copy
   a
   :domain *domain*
   :actions (map 'vector #'remove-costs (actions a))
   :effect `(and ,@(add-list a)
                 ,@(mapcar (lambda (x) `(not ,x)) (delete-list a)))
   'assign-ops +unbound+
   'add-list +unbound+
   'delete-list +unbound+))
