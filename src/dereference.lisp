
(in-package :pddl.macro-action)

;; alist type is
;; (object . variable), (constant . variable), (not in ignore list)
;; (object . constant) or (constant . constant), (in ignore list)

;; the car is always the original grouded parameter that was found in the
;; template plan. Therefore, they can be both an object or an constant.
;; In either case, if they are not ignored, they become variables.

;; if they are ignored, they are converted into a constant in the new
;; domain.
;; when (constant . constant), then they are NOT eq.

(defun dereference-parameters (objs ignored) ; -> alist
  (append
   (mapcar #'dereference-parameter-as-constant ignored)
   (mapcar #'dereference-parameter (set-difference objs ignored))))

(defun dereference-parameter (o)
  (ematch o
    ((pddl-variable domain type)
     ;; the o might be a pddl-object or a pddl-constant
     (cons o
           (pddl-variable :domain domain
                          :name (gensym
                                 (concatenate
                                  'string "?"
                                  (symbol-name (name type))))
                          :type type)))))

(defun dereference-parameter-as-constant (o)
  (cons o (change-class (shallow-copy o) 'pddl-constant)))

(defun dereference-predicates (alist fs)
  (mapcar (curry #'dereference-predicate alist) fs))
(defun dereference-predicate (alist f)
  (flet ((var (o)
           ;; it is called var, but actually it may return the ignored
           ;; objects
           (or (when-let ((pair (assoc o alist))) (cdr pair))
               (when (typep o 'pddl-constant)
                 (warn "~a: This is included neither in the obj-var alist ~
                        or in the ignored-objects list, however, appeared =
                        in the component plan. This is because
                        it has a type ~a (a member of abstract type) ~
                        but is a constant defined in the domain. ~
                        Currently we don't treat it as an error."
                       o (type o))
                 o)
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

(defun dereference-action (ga &optional
                                default-alist
                                ignored-objects)
  (flet ((w/not (list) (mapcar (lambda (x) `(not ,x)) list)))
    (ematch ga
      ((pddl-ground-action domain
                           name parameters
                           assign-ops
                           positive-preconditions
                           add-list
                           delete-list)
       (let ((alist
              (or default-alist
                  (dereference-parameters
                   parameters ignored-objects))))
         (values
          (pddl-action
           :domain domain
           :name name
           :parameters
           (if default-alist
               ;; if the default-alist is non-nil, it means that this
               ;; action is not a macro-action, but a partially grounded
               ;; action instantiated for decoding the macro-action later.
               ;; The number of parameters should be = to that
               ;; of the original action.
               (mapcar (lambda (p) (cdr (assoc p alist))) parameters)
               ;; If the default-alist is nil, this action is a
               ;; macro-action. It should remove the ignored-objects from
               ;; the alist. Otherwise, the parameter list in the domain
               ;; description contains a constant, resulting in an
               ;; infeasible PDDL description. Also, it causes the
               ;; explosion during the translation.
               (iter (for (orig . var) in alist)
                     (unless (find orig ignored-objects)
                       (collect var))))
           :precondition `(and
                           ;; the precondition may be partially grounded
                           ,@(dereference-predicates
                              alist positive-preconditions))
           :effect
           ;; the effects may be partially grounded
           `(and ,@(dereference-predicates alist add-list)
                 ,@(w/not (dereference-predicates alist delete-list))
                 ,@(dereference-assign-ops alist assign-ops)))
          (or default-alist alist)))))))

(defun dereference-assign-ops (alist ground-assign-ops)
  (mapcar (curry #'dereference-assign-op alist) ground-assign-ops))

(defun dereference-assign-op (alist ground-assign-op)
  (ematch ground-assign-op
    ((pddl-ground-assign-op value-form place increase)
     (pddl-assign-op :value-form (dereference-f-exp alist value-form)
                     :place (dereference-predicate alist place)
                     :increase (dereference-f-exp alist place)))))

(defun dereference-f-exp (alist f-exp)
  "Dereferences each f-head in a f-exp tree."
  (labels ((rec (e)
             (ematch e
               ((list* (and op (or '+ '- '* '/)) fexps)
                (list* op (mapcar #'rec fexps)))
               ((type number) e)
               (_ (dereference-predicate alist e)))))
    (rec f-exp)))
