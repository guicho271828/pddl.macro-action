
(in-package :pddl.macro-action)

(define-pddl-class macro-action (pddl-action)
  ((actions :type list :initarg :actions :initform nil)
   (alist :type list :initarg :alist :initform nil)))

(defun macro-action (actions &optional ign/objs)
  "Merge the given ground-action, dereference them, then re-instantiate as
a macro-action. The secondary value `alist' is an association list
of (object . variable), (constant . variable) -- if objects are not in the ignore list,
 (object . constant) or (constant . constant) -- if they are in the ignore list.
If the argument `actions' is #(), returns nil."
  (unless (zerop (length actions))
    (multiple-value-bind (result alist)
        (handler-bind ((warning #'muffle-warning))
          (dereference-action
           (reduce #'merge-ground-actions actions)
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


(defmethod constants ((m macro-action))
  (remove-if-not (lambda (x) (typep x 'pddl-constant))
                 (alist m)))
