
(in-package :pddl.macro-action)

(define-pddl-class macro-action (pddl-action)
  ((actions :type list :initarg :actions :initform nil)
   (alist :type list :initarg :alist :initform nil)))
(define-pddl-class ground-macro-action (macro-action pddl-ground-action)
  ())

(define-condition zero-length-plan (error) ())

(declaim (ftype (function ((vector pddl-ground-action) list) macro-action)
                macro-action))
(defun macro-action (actions arguments)
  (let ((merged (ematch actions
                  ((vector)
                   (error 'zero-length-plan))
                  ;; 1 element
                  ((vector (and a (pddl-ground-action name)))
                   (shallow-copy a :name (gensym (symbol-name name))))
                  ;; more elements
                  ((type vector)
                   (reduce #'merge-ground-actions actions))))
        ;; (params (mapcar #'dereference-parameter arguments))
        )
    (change-class
     merged
     'macro-action
     :parameters arguments ;; (mapcar #'cdr params)
     :actions actions
     :alist (mapcar (lambda (x) (cons x x)) arguments) ;; params
     )))

(declaim (ftype (function ((vector pddl-ground-action) list) ground-macro-action)
                ground-macro-action))
(defun ground-macro-action (actions arguments)
  (let ((merged (ematch actions
                  ((vector)
                   (error 'zero-length-plan))
                  ;; 1 element
                  ((vector (and a (pddl-ground-action name)))
                   (shallow-copy a :name (gensym (symbol-name name))))
                  ;; more elements
                  ((type vector)
                   (reduce #'merge-ground-actions actions))))
        ;; (params (mapcar #'dereference-parameter arguments))
        )
    (change-class
     merged
     'ground-macro-action
     :problem (problem (elt actions 0))
     :parameters arguments ;; (mapcar #'cdr params)
     :actions actions
     :alist (mapcar (lambda (x) (cons x x)) arguments) ;; params
     )))



#+nil
(defun macro-action (actions &optional ign/objs)
  "Merge the given ground-action, dereference them, then re-instantiate as
a macro-action. The secondary value `alist' is an association list
of (object . variable) or (constant . variable)
 if objects are not in the ignore list (== member of abstract component),
 and (object . constant) or (constant . constant)
 if they are in the ignore list (== environment object).
If the argument `actions' is #(), returns nil."
  (unless (zerop (length actions))
    (multiple-value-bind (result alist)
        (handler-bind ((warning #'muffle-warning))
          (dereference-action
           (ematch actions
             ((vector (and a (pddl-ground-action name)))
              (shallow-copy a :name (gensym (symbol-name name))))
             ((type vector)
              (reduce #'merge-ground-actions actions)))
           nil ign/objs))
      (values
       (change-class
        result 'macro-action
        :alist alist
        :actions (map 'list
                      (lambda (a)
                        (dereference-action a alist ign/objs))
                      actions))
       alist))))

(defun constants-in-macro (m)
  "Return a list of constants that should be introduced in the enhanced
domain due to the object grounding."
  (mapcar #'cdr
          (remove-if-not (lambda (x) (typep x 'pddl-constant))
                         (alist m) :key #'cdr)))

(defun objects-in-macro (m)
  "Return a list of objects (and constants) that should be removed from the
problem to the enhanced domain due to the object grounding."
  (mapcar #'car
          (remove-if-not (lambda (x) (typep x 'pddl-constant))
                         (alist m) :key #'cdr)))
