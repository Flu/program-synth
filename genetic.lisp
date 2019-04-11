(in-package :fluturel.program-synth)

(defparameter *population* nil)
(defvar *global-lock* (bt:make-lock))

(defclass func-object()
  ((func-tree :initform nil :initarg :func-tree :accessor func-tree)
   (fitness-score :initform -1 :accessor fitness)))

(defmethod update-fitness((p func-object))
  (with-accessors ((fitness fitness) (func-tree func-tree)) p
    (setf fitness
	  (/ (reduce #'+
		     (mapcar (lambda (args)
			       (let ((result (exec-func func-tree (subseq args 0 4))))
				 (abs (pd (- (nth 4 args) result) (nth 4 args)))))
			     *constraints*))
	     (length *constraints*)))))

(defmethod <fitness((p func-object) (q func-object))
  (with-accessors ((fitness-p fitness)) p
    (with-accessors ((fitness-q fitness)) q
      (< fitness-q fitness-p))))

(defmethod mutate((individual func-object))
  (with-accessors ((func-tree func-tree)) individual
    (if (< (random 100) 25)
	(setf func-tree (replace-random-subtree func-tree (generate-random-tree 2))))
    individual))

(defmethod crossover((p func-object) (q func-object))
  (with-accessors ((p-tree func-tree)) p
    (with-accessors ((q-tree func-tree)) q
      (make-instance 'func-object
		     :func-tree (replace-random-subtree p-tree (random-subtree q-tree))))))

(defun init-population(population-size)
  (setf *population* (make-array population-size :adjustable t :fill-pointer 0))
  (loop :for i :from 0 :below population-size :do
       (vector-push-extend
	(make-instance 'func-object :func-tree (generate-random-tree 5))
	*population*)))

(defun compute-fitness-population()
  (dotimes (a 8)
    (let* ((thr a)
	   (start (floor (* (length *population*) 1/8 thr)))
	   (end (floor (* (length *population*) 1/8 (+ thr 1)))))
      (bt:make-thread
       (lambda ()
	 (locally
	     (declare (sb-ext:muffle-conditions sb-kernel:redefinition-warning))
	   (handler-bind ((sb-kernel:redefinition-warning #'muffle-warning))
	     (loop :for i :from start :below end :do
		  (update-fitness (aref *population* i))))))))))

(defun check-for-completion()
  (let ((results nil))
   (loop :for i :from 0 :below (length *population*) :do
	(if (= 0 (fitness (aref *population* i)))
	    (setf results (cons (aref *population* i) results))))
   (dolist (res results)
     (format t "~a~%" (slot-value res 'func-tree)))))

(defun choose-parents(number)
  (let ((parents nil))
    (labels ((tournament (n)
	       (let ((max-fitness -2)
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
  (time
   (locally
       (declare (sb-ext:muffle-conditions sb-kernel:redefinition-warning))
     (handler-bind ((sb-kernel:redefinition-warning #'muffle-warning))
       (init-population population-size)
       (format t ">> Started evolving. Constraints:~%")
       (format t "~{~a~%~}----------~%" *constraints*)
       (loop :for i :from 0 :below generations :do
	    (compute-fitness-population)
	    (let ((children nil)
		  (parents nil))
	      (setf parents (choose-parents (floor (/ population-size 10))))
	      (setf children (mapcar #'crossover parents (reverse parents)))
	      (loop :for j :from 0 :below (length children) :do
		   (vector-push-extend (elt children j) *population*)))
	    (map 'list #'mutate *population*)
	    (compute-fitness-population)
	    (sort *population* #'<fitness)
	    (setf (fill-pointer *population*) population-size)
	    (format t "Generation ~a is done, maximum fitness was ~a~%"
		    i (fitness (aref *population* 0)))
	    (check-for-completion))))))
  
