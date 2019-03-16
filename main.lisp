(in-package :fluturel.program-synth)

(defmacro eval-defun(args &body body)
  `(progn
     (eval ,@body)
     (funcall (cadr ,@body) ,@args)))
