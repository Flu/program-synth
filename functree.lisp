(in-package :fluturel.program-synth)

(defclass func-tree()
  ((func-name
    :initarg :func-name :initform (error "Must have a name") :accessor func-name)
   (data
    :initarg :data :initform (cons 'defun nil) :accessor data)
   (num-args
    :initarg :num-args :initform 0 :accessor num-args)))

(defgeneric remove-last-instr(func &optional depth))

(defmethod initialize-instance :after ((func func-tree) &key)
  (with-accessors ((func-name func-name) (data data)) func
    (setf data (append data (list func-name '(&optional _1 _2 _3 _4))))))

(defmethod change-name((func func-tree) new-name)
  (with-accessors ((func-name func-name)) func
    (setf func-name new-name)))

(defmethod add-instr((func func-tree) new-instr)
  (with-accessors ((data data)) func
    (setf data (append data (list new-instr)))))

(defun remove-last-elem(list &optional (depth 1))
  (if (null (cddr list))
      (progn
	(setf (cdr list) nil)
	depth)
      (remove-last-elem (cdr list) (+ depth 1))))

(defmethod remove-last-instr((func func-tree) &optional (depth 0))
  (with-accessors ((data data)) func
    (remove-last-elem data depth)))
