
(in-package :pddl.macro-action)

(defun dereference-parameters (params) ; -> alist
  (mapcar #'dereference-parameter params))

(defun dereference-parameter (p)
  (ematch p
    ((pddl-variable domain type) ;; the p might be a pddl-object or a pddl-constant
     (cons p
           (pddl-variable :domain domain
                          :name (gensym
                                 (concatenate
                                  'string "?"
                                  (symbol-name (name type))))
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

(defun dereference-action (ga &optional default-alist)
  (flet ((w/not (list) (mapcar (lambda (x) `(not ,x)) list)))
    (ematch ga
      ((pddl-ground-action domain
                           name parameters
                           assign-ops
                           positive-preconditions
                           add-list
                           delete-list)
       (let ((alist
              (if default-alist
                  (mapcar (lambda (o) (assoc o default-alist)) parameters)
                  (dereference-parameters parameters))))
         (values
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
                 ,@(dereference-assign-ops alist assign-ops)))
          (or default-alist alist)))))))

(defun dereference-assign-ops (alist ground-assign-ops)
  (mapcar (curry #'dereference-assign-op alist) ground-assign-ops))

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
