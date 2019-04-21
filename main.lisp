(in-package :fluturel.program-synth)

(defparameter *constraints*
  '((2 2 3 6 36) (2 1 5 6 33) (1 15 3 6 144) (1 1 2 2 8)
    (5 6 7 8 165) (4 -1 2 3 15) (1 2 3 5 24) (-2 -14 5 8 -208)
    (0 1 8 9 17) (-23 41 2 2 72) (56 -200 2 -47 6480)))

(defun fill-constraints(lambda-func &optional (tests 10))
  (if (not (eql tests 0))
      (let ((rand '(- (random 40) 20))
	    (ret-list nil))
	(dotimes (a 4)
	  (setf ret-list (cons (eval rand) ret-list)))
	(push (apply lambda-func ret-list) (cdr (last ret-list)))
	(push ret-list *constraints*)
	(fill-constraints lambda-func (- tests 1)))))

(defun exec-func(func-tree args)
  (let ((temp-tree (copy-tree func-tree)))
    (nsubst (elt args 0) '_1 temp-tree)
    (nsubst (elt args 1) '_2 temp-tree)
    (nsubst (elt args 2) '_3 temp-tree)
    (nsubst (elt args 3) '_4 temp-tree)
    (cond
      ((eql temp-tree '_1) (elt args 0))
      ((eql temp-tree '_2) (elt args 1))
      ((eql temp-tree '_3) (elt args 2))
      ((eql temp-tree '_4) (elt args 3))
      (t (eval temp-tree)))))

(defun exec-func%(func-tree args)
  (eval
   `(let ((_1 ,(elt args 0))
	  (_2 ,(elt args 1))
	  (_3 ,(elt args 2))
	  (_4 ,(elt args 3)))
      ,func-tree)))

(defun range(start end &optional (inc 1))
  (cond
    ((< end start) nil)
    (t
     (let ((return-list nil))
       (do ((elem start (+ elem inc)))
	   ((<= end elem))
	 (setf return-list (append return-list (list elem))))
       return-list))))
