(in-package :fluturel.program-synth)

(defparameter *mutation-chance* 0.2)
(defparameter *population* nil)

(defun mutate(individual)
  (permutation individual)
  (replace-random-subtree individual (generate-random-tree 3)))

(defun crossover(p1 p2)
  (replace-random-subtree p1 (random-subtree p2)))

(defun init-population(population-size)
  (setf *population* (make-array `(,population-size 2) :adjustable t))
  (loop :for i :from 0 :below population-size :do
       (setf (aref *population* i 0) (generate-random-tree 5))))

(defun compute-fitness()
  (loop :for i :from 0 :below (array-dimension *population* 0) :do
       (setf (aref *population* i 1) (exec-func (aref *population* i 0) 2 2 2 2))))
