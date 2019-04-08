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

(defmethod <fitness((p func-object) (q func-object))
  (with-accessors ((fitness-p fitness)) p
    (with-accessors ((fitness-q fitness)) q
      (< fitness-p fitness-q))))

(defmethod mutate((individual func-object))
  (with-accessors ((func-tree func-tree)) individual
    (replace-random-subtree func-tree (generate-random-tree 3)))
  (update-fitness individual))

(defmethod crossover((p func-object) (q func-object))
  (with-accessors ((p-tree func-tree)) p
    (with-accessors ((q-tree func-tree)) q
      (replace-random-subtree p (random-subtree q))))
  (update-fitness p)
  (update-fitness q))

(defun init-population(population-size)
  (setf *population* (make-array population-size :adjustable t))
  (loop :for i :from 0 :below population-size :do
       (setf
	(aref *population* i) (make-instance 'func-object :func-tree (generate-random-tree 5)))))

(defun compute-fitness-population()
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
		   (let* ((rand (random (length *population*)))
			  (p (aref *population* rand)))
		     (with-accessors ((fitness fitness)) p
		       (when (<= max-fitness fitness)
			 (setf result p)))))
		 result)))
      (dotimes (a number)
	(setf parents (cons (tournament 10) parents))))
    parents))

(defun evolve(generations population-size)
  (init-population population-size)
  (format t ">> Started evolving. Constraints:~%")
  (format t "~{~a~%~}----------~%" *constraints*)
  (loop :for i :from 0 :below generations :do
       (compute-fitness-population)
       (let ((children nil)
	     (parents nil))
	 (setf parents (choose-parents (floor (/ population-size 10))))
	 (setf children (mapcar #'crossover parents (reverse parents))))
       (check-for-completion)))
  
