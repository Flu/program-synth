(in-package :fluturel.program-synth)

(defparameter *valid-forms*
  '(+ - * pd sin cos expt))

(defmacro pd(number denominator)
  `(float (cond
    ((eql ,denominator 0) 0)
    (t (/ ,number ,denominator)))))
  
(defparameter *numeric-constants*
  (range -10 10 0.1))

(defun generate-random-tree(name))

(defun change-name(func-tree new-name)
  (setf (cadr func-tree) new-name))

(defun add-instr(tree instr)
  (setf tree (append tree (list instr))))

(defun remove-last-instr(tree)
  (if (null (cddr tree))
      (setf (cdr tree) nil)
      (remove-last-instr (cdr tree))))

(defun tree-size(tree)
  (cond
    ((null tree) nil)
    (t (length (flatten tree)))))

(defun flatten(tree)
  (cond
    ((null tree) nil)
    ((atom tree) (list tree))
    (t (loop :for a :in tree :appending (flatten a)))))


