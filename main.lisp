(in-package :fluturel.program-synth)

(defun compile-func(func-tree)
  (eval `(defun func(_1 _2 _3 _4) ,func-tree)))

(defun exec-func(func-tree &rest args)
  (compile-func func-tree)
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
