(in-package :fluturel.program-synth)

(defparameter *constraints*
  '((2 2 3 6 36) (2 1 5 6 33) (1 15 3 6 144) (1 1 2 2 8)
    (5 6 7 8 165) (4 -1 2 3 15) (1 2 3 5 24) (-2 -14 5 8 -208)
    (0 1 8 9 17) (-23 41 2 2 72) (56 -200 2 -47 6480)))

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
