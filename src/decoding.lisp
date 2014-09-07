
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
        (format t "~&Decoding action ~a" name)
        (flet ((obj (var)
                 ;; alist type is
                 ;; (object . variable), (constant . variable),
                 ;; (object . constant) or (constant . constant)
                 ;; so the query is the cdr part: a variable or a constant.
                 (ematch var
                   ;; 9/7 added pddl-object in the first clause
                   ;; this is for adopting "always grounded" strategy of
                   ;; macro composition.
                   ((or (pddl-object) (pddl-constant))
                    ;; when the pair is (* . c), then the variable is
                    ;; grounded and does not appear in `vars' and
                    ;; its instantiation does not appear in `objects' either.
                    ;; therefore, use itself.
                    var)
                   ((pddl-variable)
                    ;; or it is a variable.  it will be in the same
                    ;; position in `objects' as var is in vars.
                    (nth (position var vars) objects)))))
          (iter (for pa in actions)
                ;; these actions are partially grounded, so the parameters
                ;; may contain objects.
                (collecting
                 (ground-action pa (mapcar #'obj (parameters pa)))))))
       ((macro-action) (list ground-action))))))

(defun decode-plan (macro plan)
  (match plan
    ((pddl-plan actions)
     (pddl-plan ; newly create a pddl-plan, not shallow-copy, so that it
      :actions  ; uses the current special binding of *domain* and
                ; *problem*
      (apply #'concatenate 'vector
             (map 'list (curry #'decode-action macro) actions))))))
