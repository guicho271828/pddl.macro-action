
(in-package :pddl.macro-action)

(defclass macro-action (pddl-action)
  ((actions :type list :initarg :actions :initform nil)))

(defun macro-action (actions)
  "Merge the given ground-action, dereference them, then re-instantiate as
a macro-action"
  (multiple-value-bind (result alist)
      (dereference-action (reduce #'merge-ground-actions actions))
    (change-class result 'macro-action
                  :actions (map 'list (lambda (a)
                                        (dereference-action a alist))
                                actions))))
