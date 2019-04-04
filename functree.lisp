;;; Functions and macros for generating and working with function trees. Basically helper functions for me to use in genetic algorithm

(in-package :fluturel.program-synth)

; Hashtable for functions and the number of arguments it takes
(defparameter *function-table*
  '((+ . 2) (- . 2) (* . 2) (pd . 2) (sin . 1) (cos . 1)))

; TODO: Numeric constants slow down the algorithm too much
; Numeric constants to use for computation
(defparameter *numeric-constants*
  (range -10 10 0.5))

; Args to use, provided as the function arguments (they will express the constraints later)
(defparameter *args*
  '(_1 _2 _3 _4))

(defmacro get-random-value(l)
  "Selects a random element from a list"
  `(nth (random (length ,l)) ,l))

(defmacro get-value-alist(key alist)
  "Provided with a key, returns the value from the association list"
  `(cdr (assoc ,key ,alist)))

(defun pd(number denominator)
  "Protected division: if the denominator is 0, return 0. Circumvents arithmetic errors"
  (float
   (cond
     ((= denominator 0) 0)
     (t (/ number denominator)))))

(defun exponent(base power)
  "Just like expt, except the base is always positive. Never returns complex numbers"
  (expt (abs base) power))
  
(defun random-function()
  "Returns a random function from the function table"
  (car (get-random-value *function-table*)))

(defun random-terminal()
  "Returns either a function argument or a numeric constant"
  (let ((term (random 2)))
    (if (eql term 1)
	(get-random-value *args*)
	(get-random-value *numeric-constants*))))

(defun generate-random-tree(depth)
  "Generates a random code tree with an arbitrary maximum depth"
  (cond
    ((eql depth 0) (random-terminal))
    (t (let* ((func (random-function))
	      (num-args (get-value-alist func *function-table*)))
	 (if (eql (random 5) 0)
	     (random-terminal)
	     `(,func ,@(loop :for i :from 1 :to num-args :collect
			  (generate-random-tree (- depth 1)))))))))

(defun change-name(func-tree new-name)
  "Changes name of function tree -- now deprecated"
  (setf (cadr func-tree) new-name))

(defun add-instr(tree instr)
  "Adds a new instruction tree at the end of the code tree"
  (setf tree (append tree (list instr))))

(defun remove-last-instr(tree)
  "Removes the last instructin tree at the end of the code tree"
  (if (null (cddr tree))
      (setf (cdr tree) nil)
      (remove-last-instr (cdr tree))))

(defun tree-size(tree)
  "Measures code tree size after flattening - total number of symbols"
  (cond
    ((null tree) nil)
    ((atom tree) 1)
    (t (length (flatten tree)))))

(defun flatten(tree)
  "Flattens a tree into a list"
  (cond
    ((null tree) nil)
    ((atom tree) (list tree))
    (t (loop :for a :in tree :appending (flatten a)))))

(defun repeat(times data &optional (acc nil))
  "Returns a list with DATA repeated TIMES times"
  (if (zerop times)
      acc
      (repeat (- times 1) data
	      (append acc (list data)))))

(defun random-subtree(tree)
  "Selects a random subtree from the code tree, with a probability proportional with the total code size of that subtree (the return value of TREE-SIZE)"
  (if (zerop (random (tree-size tree)))
      tree
      (random-subtree
       (get-random-value
	(apply 'append (map 'list #'(lambda (x)
				      (repeat (tree-size x) x))
			    (rest tree)))))))

(defun replace-random-subtree(tree replacement)
  "Replaces a random subtree from TREE with REPLACEMENT"
  (let ((tree-to-replace (random-subtree tree)))
    (values
     (setf tree (subst replacement tree-to-replace tree))
     tree-to-replace)))
