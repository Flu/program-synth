(in-package :fluturel.program-synth)

(defmacro eval-defun(args &body body)
  `(progn
     (eval ,@body)
     (apply (cadr ,@body) ,args)))

(defun range(start end &optional (inc 1))
  (cond
    ((< end start) nil)
    (t (let ((return-list nil))
	 (do ((elem start (+ elem inc)))
	     ((<= end elem))
	   (setf return-list (append return-list (list elem))))
	 return-list))))
