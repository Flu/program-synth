(in-package :fluturel.program-synth)

(defparameter *constraints*
  '((1 2 0 0 2) (-5 8 0 0 -40) (4 5 0 0 20) (1 -1 0 0 -1) (7 -23 0 0 -161)))

(defun compile-func(func-tree)
  (eval `(defun func(_1 _2 _3 _4) ,func-tree)))

(defun exec-func(args)
  (apply #'func args))

(defun range(start end &optional (inc 1))
  (cond
    ((< end start) nil)
    (t
     (let ((return-list nil))
       (do ((elem start (+ elem inc)))
	   ((<= end elem))
	 (setf return-list (append return-list (list elem))))
       return-list))))
