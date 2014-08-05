
(in-package :pddl.macro-action.test)
(in-suite :pddl.macro-action)

(define (domain logistics)
  (:requirements :strips)
  (:predicates (truck ?t) (at ?t ?a) (connected ?x ?y))
  (:action move
           :parameters (?t ?x ?y)
           :precondition (and (truck ?t) (at ?t ?x) (connected ?x ?y))
           :effect (and (not (at ?t ?x)) (at ?t ?y))))

(define (problem logistics-prob)
  (:domain logistics)
  (:objects t1 a b c)
  (:init (truck t1) (at t1 a) (connected a b) (connected b c))
  (:goal (at t1 c)))

(test merged-action
  (let* ((*domain* logistics)
         (*problem* logistics-prob)
         (ga1 (ground-action (action *domain* :move)
                             (list (object *problem* :t1)
                                   (object *problem* :a)
                                   (object *problem* :b))))
         (ga2 (ground-action (action *domain* :move)
                             (list (object *problem* :t1)
                                   (object *problem* :b)
                                   (object *problem* :c))))
         (m (merge-ground-actions ga1 ga2)))
    ;(inspect m)
    (is (= 4 (length (parameters m))))
    (is (= 4 (length (positive-preconditions m))))
    (is (= 1 (length (add-list m))))
    (is (= 1 (length (delete-list m))))
    (is (null (set-exclusive-or
               (reduce (lambda (state ga) (apply-ground-action ga state))
                       (list ga1 ga2)
                       :initial-value (init *problem*))
               (apply-ground-action m (init *problem*)))))))

