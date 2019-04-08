(in-package :fluturel.program-synth)

(defparameter *mutation-chance* 0.2)

(defparameter *population* nil)

(defclass func-object()
  ((func-tree :initform nil :initarg :func-tree :accessor func-tree)
   (fitness-score :initform 0 :accessor fitness)))

(defmethod update-fitness((p func-object))
  (with-accessors ((fitness fitness) (func-tree func-tree)) p
    (compile-func func-tree)
    (setf fitness
	  (/ (reduce #'+ (mapcar (lambda (args)
				   (if (eql
					(exec-func (subseq args 0 4)) (nth 4 args))
				       1 0))
				 *constraints*))
	     (length *constraints*)))))

(defun mutate(individual)
  (replace-random-subtree individual (generate-random-tree 3)))

(defun crossover(p1 p2)
  (replace-random-subtree p1 (random-subtree p2)))

(defun init-population(population-size)
  (setf *population* (make-array population-size :adjustable t))
  (loop :for i :from 0 :below population-size :do
       (setf
	(aref *population* i) (make-instance 'func-object :func-tree (generate-random-tree 5)))))

(defun compute-fitness()
  (loop :for i :from 0 :below (length *population*) :do
       (update-fitness (aref *population* i))))

(defun check-for-completion()
  (let ((result nil))
   (loop :for i :from 0 :below (length *population*) :do
	(if (= 1 (aref *population* i))
	    (setf result (cons (aref *population* i) result))))
   result))

(defun choose-parents(number)
  (let ((parents nil))
    (labels ((tournament (n)
	       (let ((max-fitness 0)
		     (result nil))
		 (dotimes (a (- n 1))
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
	 (setf parents (choose-parents (floor (/ population-size 10))))
	 (setf children (mapcar #'crossover parents (reverse parents))))
       (check-for-completion)))
  
