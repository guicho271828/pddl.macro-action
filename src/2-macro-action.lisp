
(in-package :pddl.macro-action)

(define-pddl-class macro-action (pddl-action)
  ((actions :type vector :initarg :actions :initform nil)
   (alist :type list :initarg :alist :initform nil)))
(define-pddl-class ground-macro-action (macro-action pddl-ground-action)
  ())

(define-condition zero-length-plan (error) ())

(defun ground-macro-action (actions)
  (let ((merged (ematch actions
                  ((vector)
                   (error 'zero-length-plan))
                  ;; 1 element
                  ((vector (and a (pddl-ground-action name)))
                   (shallow-copy a :name (gensym (symbol-name name))))
                  ;; more elements
                  ((type vector)
                   (reduce #'merge-ground-actions actions)))))
    (change-class
     merged
     'ground-macro-action
     :actions actions
     :alist (mapcar (lambda (x) (cons x x)) (parameters merged)))))


(defun nullary-macro-action (actions)
  (change-class (shallow-copy (ground-macro-action actions))
                'macro-action
                :parameters nil))


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
