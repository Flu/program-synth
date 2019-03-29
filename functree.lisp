(in-package :fluturel.program-synth)

(defparameter *function-table*
  '((+ . 2) (- . 2) (* . 2) (pd . 2) (sin . 1) (cos . 1) (exponent . 2)))

(defparameter *numeric-constants*
  (range -10 10 0.5))

(defparameter *args*
  '(_1 _2 _3 _4))

(defmacro get-random-value(l)
  `(nth (random (length ,l)) ,l))

(defmacro get-value-alist(key alist)
  `(cdr (assoc ,key ,alist)))

(defun pd(number denominator)
  (float
   (cond
     ((= denominator 0) 0)
     (t (/ number denominator)))))

(defun exponent(base power)
  (expt (abs base) power))
  
(defun random-function()
  (car (get-random-value *function-table*)))

(defun random-terminal()
  (let ((term (random 2)))
    (if (eql term 1)
	(get-random-value *args*)
	(get-random-value *numeric-constants*))))

(defun generate-random-tree(depth)
  (cond
    ((eql depth 0) (random-terminal))
    (t (let* ((func (random-function))
	      (num-args (get-value-alist func *function-table*)))
	 (if (eql (random 5) 0)
	     (random-terminal)
	     `(,func ,@(loop :for i :from 1 :to num-args :collect
			  (generate-random-tree (- depth 1)))))))))

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
    ((atom tree) 1)
    (t (length (flatten tree)))))

(defun flatten(tree)
  (cond
    ((null tree) nil)
    ((atom tree) (list tree))
    (t (loop :for a :in tree :appending (flatten a)))))

(defun repeat(data times &optional (acc nil))
  (if (zerop times)
      acc
      (repeat data (- times 1)
	      (append acc (list data)))))

(defun random-subtree(tree)
  (if (zerop (random (tree-size tree)))
      tree
      (random-subtree
       (get-random-value
	(apply 'append (map 'list #'(lambda (x)
			       (repeat (tree-size x) x))
			   (rest tree)))))))
  ;; TODO: Select a random subtree and copy it

