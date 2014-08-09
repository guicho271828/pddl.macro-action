

(in-package :pddl.macro-action)

(defclass macro-action (pddl-action)
  ((actions :type list :initarg :actions :initform nil)))

(defun macro-action (actions)
  (change-class 
   (dereference-action (reduce #'merge-ground-actions actions))
   'macro-action
   :actions (map 'list #'dereference-action actions)))
    
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

(defun dereference-parameters (params) ; -> alist
  (mapcar #'dereference-parameter params))

(defun dereference-parameter (p)
  (ematch p
    ((pddl-object domain name type)
     (cons p
           (pddl-variable :domain domain
                          :name (gensym (symbol-name name))
                          :type type)))))

(defun dereference-predicates (alist fs)
  (mapcar (curry #'dereference-predicate alist) fs))
(defun dereference-predicate (alist f)
  (flet ((var (o)
           (or (when-let ((pair (assoc o alist))) (cdr pair))
               (when (typep o 'pddl-constant) o)
               (error "Parameter ~a not found" o))))
    (match f
      ((pddl-atomic-state name parameters)
       (pddl-predicate
        :name name
        :parameters (mapcar #'var parameters)))
      ((pddl-function-state name parameters type)
       (pddl-function
        :name name :type type
        :parameters (mapcar #'var parameters))))))

(defun dereference-action (ga)
  (flet ((w/not (list) (mapcar (lambda (x) `(not ,x)) list)))
    (ematch ga
      ((pddl-ground-action domain
                           name parameters
                           assign-ops
                           positive-preconditions
                           add-list
                           delete-list)
       (let ((alist (dereference-parameters parameters)))
         (pddl-action
          :domain domain
          :name name
          :parameters (mapcar #'cdr alist)
          :precondition `(and
                          ,@(dereference-predicates
                             alist positive-preconditions))
          ;; do not assume action-costs currently
          :effect
          `(and ,@(dereference-predicates alist add-list)
                ,@(w/not (dereference-predicates alist delete-list))
                ,@(dereference-assign-ops alist assign-ops))))))))

(defun dereference-assign-ops (alist ground-assign-ops)
  (mapcar (curry #'derefernence-assign-op alist) ground-assign-ops))

(defun dereference-assign-op (alist ground-assign-op)
  (ematch ground-assign-op
    ((pddl-ground-assign-op value-form place)
     (pddl-assign-op :value-form (dereference-f-exp alist value-form)
                     :place (dereference-predicate alist place)))))

(defun dereference-f-exp (alist f-exp)
  "Dereferences each f-head in a f-exp tree."
  (labels ((rec (e)
             (ematch e
               ((list* (and op (or '+ '- '* '/)) fexps)
                (list* op (mapcar #'rec fexps)))
               ((type number) e)
               (_ (dereference-predicate alist e)))))
    (rec f-exp)))
