
(in-package :pddl.macro-action)


(defun decode-action (macro ground-action)
  (ematch ground-action
    ((pddl-ground-action :name name
                         :parameters objects)
     (ematch macro
       ((macro-action :name (eq name)
                      :parameters (guard vars (= (length vars)
                                                 (length objects)))
                      :actions actions)
        (flet ((obj (var) (nth (position var vars) objects)))
          (iter (for a in actions)
                (collecting
                 (ground-action a (mapcar #'obj (parameters a)))))))
       (_ (list ground-action))))))

(defun decode-plan (macro plan)
  (match plan
    ((pddl-plan actions)
     (pddl-plan ; newly create a pddl-plan, not shallow-copy, so that it
      :actions  ; uses the special binding of *domain* and *problem*
      (apply #'concatenate 'vector
             (map 'list (curry #'decode-action macro) actions))))))
