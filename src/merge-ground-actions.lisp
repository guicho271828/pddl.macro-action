

(in-package :pddl.macro-action)

(define-pddl-class macro-action (pddl-ground-action)
  ((actions :type list :initform nil)))

(defun merge-ground-actions (ga1 ga2)
  "Reference implimatation as in Macro-FF paper, Botea et. al., JAIR 2005, Figure 8.
Creates a new ground-action that is a result of merging consequtive two
actions ga1 and ga2, where ga1 is followed by ga2. "
  (flet ((s/union (set1 set2) (union set1 set2 :test #'eqstate))
         (s/diff (set1 set2) (set-difference set1 set2 :test #'eqstate))
         (w/not (list) (mapcar (lambda (x) `(not ,x)) list)))
    (ematch ga1
      ((pddl-ground-action :name n1 :parameters objs1)
       (let ((a1 (add-list ga1))
             (d1 (delete-list ga1))
             (pre1 (positive-preconditions ga1)))
         (ematch ga2
           ((pddl-ground-action :name n2 :parameters objs2)
            (let ((a2 (add-list ga2))
                  (d2 (delete-list ga2))
                  (pre2 (positive-preconditions ga2)))
              (handler-bind ((simple-error (lambda (c)
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
                         ,@(w/not del-maybe-duplicated)))))))))))))

