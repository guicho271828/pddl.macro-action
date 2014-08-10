

(in-package :pddl.macro-action)

(defun merge-ground-actions (ga1 ga2)
  "Reference implimatation as in Macro-FF paper, Botea et. al., JAIR 2005, Figure 8.
Creates a new ground-action that is a result of merging consequtive two
actions ga1 and ga2, where ga1 is followed by ga2. "
  (flet ((s/union (set1 set2) (union set1 set2 :test #'eqstate))
         (s/diff (set1 set2) (set-difference set1 set2 :test #'eqstate))
         (w/not (list) (mapcar (lambda (x) `(not ,x)) list)))
    (ematch ga1
      ((pddl-ground-action
        :problem *problem*
        :name n1 :parameters objs1
        :positive-preconditions pre1
        :assign-ops ops1
        :add-list a1
        :delete-list d1)
       (ematch ga2
         ((pddl-ground-action
           :name n2 :parameters objs2
           :positive-preconditions pre2
           :assign-ops ops2
           :add-list a2
           :delete-list d2)
          (handler-bind ((simple-error
                          (lambda (c)
                            (declare (ignore c))
                            (invoke-restart (find-restart 'ignore)))))
            (pddl-ground-action
             :name (symbolicate n1 '- n2)
             :parameters (union objs1 objs2)
             :precondition `(and ,@(s/union pre1 (s/diff pre2 a1)))
             ;; do not assume action-costs currently
             :effect
             (let ((add-maybe-duplicated (s/union (s/diff a1 d2) (s/diff a2 d1)))
                   (del-maybe-duplicated (s/union (s/diff d1 a2) (s/diff d2 a1))))
               `(and ,@add-maybe-duplicated
                     ,@(w/not del-maybe-duplicated)
                     ,@ops1 ;; multiple assign-ops are allowed cf. pddl3.1
                     ,@ops2))))))))))

