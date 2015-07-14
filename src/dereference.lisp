
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

(defun lift-parameters (objs &optional objs-to-be-constant) ; -> alist
  (append
   (mapcar #'lift-parameter-as-constant objs-to-be-constant)
   (mapcar #'lift-parameter-as-variable (set-difference objs objs-to-be-constant))))

(defun lift-parameter-as-variable (o)
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

(defun lift-parameter-as-constant (o)
  (cons o (change-class (shallow-copy o) 'pddl-constant)))

(define-condition parameter-not-found (error)
  ((parameter :initarg :parameter :reader parameter))
  (:report (lambda (c s)
             (format s "Parameter ~a not found" (parameter c)))))

(defun lift-predicates (alist fs)
  (mapcar (curry #'lift-predicate alist) fs))
(defun lift-predicate (alist f)
  (labels ((var (o)
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
               (error 'parameter-not-found :parameter o))))
    (match f
      ((pddl-atomic-state domain name parameters)
       (pddl-predicate
        :domain domain
        :name name
        :parameters (mapcar #'var parameters)))
      ((pddl-function-state domain name parameters type)
       (pddl-function
        :domain domain
        :name name :type type
        :parameters (mapcar #'var parameters))))))

(defun lift-action (ga &optional default-alist)
  "Lift a ground action GA.
It completely lifts all parameters of the ground action.
Unless default-alist is given, it creates a new object-variable bindings.
If it is given, then it overrides the default, which is useful when
lifting the underlying actions of a macro action."
  (flet ((w/not (list) (mapcar (lambda (x) `(not ,x)) list)))
    (prog* ((parameters (parameters ga))
            (alist (or default-alist (lift-parameters parameters))))
      start
      (return
        (restart-case
            (macrolet ((result (class &rest args)
                         `(,class :domain domain
                                  :name name
                                  :precondition `(and ,@(lift-predicates alist positive-preconditions)
                                                      ,@(w/not (lift-predicates alist negative-preconditions))
                                                      ,@(w/not (unequals newparams domain)))
                                  :parameters newparams
                                  :effect `(and ,@(lift-predicates alist add-list)
                                                ,@(w/not (lift-predicates alist delete-list))
                                                ,@(lift-assign-ops alist assign-ops))
                                  ,@args)))
              (ematch ga
                ((ground-macro-action domain name actions
                                      positive-preconditions negative-preconditions
                                      add-list delete-list assign-ops)
                 (let ((newparams (mapcar #'cdr alist)
                         #+nil (mapcar (lambda (p) (cdr (assoc p alist))) parameters)))
                   (result macro-action
                           :actions (map 'vector (lambda (ga) (lift-action ga alist)) actions))))
                ((pddl-ground-action domain name
                                     positive-preconditions negative-preconditions
                                     add-list delete-list assign-ops)
                 (let ((newparams (mapcar (lambda (p) (cdr (assoc p alist))) parameters)))
                   (result pddl-action)))))
          (ground (c)
            (push (lift-parameter-as-constant (parameter c)) alist)
            (go start))
          (lift (c)
            (push (lift-parameter-as-variable (parameter c)) alist)
            (go start)))))))

(defun unequals (params domain)
  (iter outer
        (for p1 in params)
        (iter (for p2 in params)
              (unless (eq p1 p2)
                (in outer
                    (collect
                        (pddl-predicate
                         :domain domain
                         :name 'equal
                         :parameters (list p1 p2))))))))

(defun lift-assign-ops (alist ground-assign-ops)
  (mapcar (curry #'lift-assign-op alist) ground-assign-ops))

(defun lift-assign-op (alist ground-assign-op)
  (ematch ground-assign-op
    ((pddl-ground-assign-op domain value-form place increase)
     (pddl-assign-op :domain domain
                     :value-form (lift-f-exp alist value-form)
                     :place (lift-predicate alist place)
                     :increase (lift-f-exp alist increase)))))

(defun lift-f-exp (alist f-exp)
  "Lifts each f-head in a f-exp tree."
  (labels ((rec (e)
             (ematch e
               ((list* (and op (or '+ '- '* '/)) fexps)
                (list* op (mapcar #'rec fexps)))
               ((type number) e)
               (_ (lift-predicate alist e)))))
    (rec f-exp)))
