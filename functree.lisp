(in-package :fluturel.program-synth)

(defparameter *valid-forms*
  (make-array 10 :adjustable t :fill-pointer 0))

(defun populate-forms()
  (push *valid-forms* '(+))
  (push *valid-forms* '(-))
  (push *valid-forms* '(*))
  (push *valid-forms* '(/))
  (push *valid-forms* '(mod)))

(defun generate-random-tree(name))

(defun change-name(func-tree new-name)
  (setf (cadr func-tree) new-name))

(defun add-instr(tree instr)
  (setf tree (append tree (list instr))))

(defun remove-last-instr(tree)
  (if (null (cddr tree))
      (setf (cdr tree) nil)
      (remove-last-instr (cdr tree))))

(defun flatten(tree)
  (cond
    ((null tree) nil)
    ((atom tree) (list tree))
    (t (loop :for a :in tree :appending (flatten a)))))
