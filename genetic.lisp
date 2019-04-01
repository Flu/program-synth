(in-package :fluturel.program-synth)

(defparameter *mutation-chance* 0.2)

(defun mutate(tree)
  (replace-random-subtree tree (generate-random-tree (3))))

(defun crossover(parent1 parent2)
  (replace-random-subtree parent1 (random-subtree parent2)))
