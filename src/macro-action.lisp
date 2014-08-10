
(in-package :pddl.macro-action)

(defclass macro-action (pddl-action)
  ((actions :type list :initarg :actions :initform nil)))

(defun macro-action (actions)
  "Merge the given ground-action, dereference them, then re-instantiate as
a macro-action. The secondary value `alist' is an association list
of (object . variable)."
  (multiple-value-bind (result alist)
      (dereference-action (reduce #'merge-ground-actions actions))
    (values
     (change-class result 'macro-action
                   :actions (map 'list (lambda (a)
                                         (dereference-action a alist))
                                 actions))
     alist)))


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

