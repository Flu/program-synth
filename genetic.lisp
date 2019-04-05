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
       (compile-func (aref *population* i 0))
       (setf (aref *population* i 1)
	     (/ (reduce #'+ (mapcar (lambda (args)
				      (if (eql
					   (exec-func (subseq args 0 4)) (nth 4 args))
					  1 0))
				    *constraints*))
		(length *constraints*)))))

(defun check-for-completion()
  (let ((result nil))
   (loop :for i :from 0 :below (array-dimension *population* 0) :do
	(if (= 1 (aref *population* i 1))
	    (setf result (cons (aref *population* i 0) result))))
   result))

(defun choose-parents(number)
  (let ((parents nil))
    (labels ((tournament (n)
	       (let ((max-fitness 0)
		     (result nil))
		 (dotimes (a 9)
		   (let ((rand (random (array-dimension *population* 0))))
		     (when (<= max-fitness (aref *population* rand 1))
		       (setf result (aref *population* rand 0)))))
		 result)))
      (dotimes (a number)
	(setf parents (cons (tournament 10) parents))))
    parents))

(defun evolve(generations population-size)
  (init-population population-size)
  (format t ">> Started evolving. Constraints:~%")
  (format t "~{~a~%~}" *constraints*)
  (loop :for i :from 0 :below generations :do
       (compute-fitness)
       (let ((children nil)
	     (parents nil))
	 (setf parent-vector (choose-parents
       (check-for-completion)))
  
