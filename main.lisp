(in-package :fluturel.program-synth)

(load "~/Lisp/program-synth/functree.lisp")

(defmacro eval-defun(args &body body)
  `(progn
     (eval ,@body)
     (funcall (cadr ,@body) ,@args)))
