;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: USER; Base: 10 -*-
;;; Lyle Borg-Graham, Equipe Cogniscience, Institut Alfred Fessard, CNRS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;              The Surf-Hippo Neuron Simulator                    ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This code was written as part of the Surf-Hippo project at the  ;;;
;;; Center for Biological Information Processing, Department of     ;;;
;;; Brain and Cognitive Sciences, Massachusetts Institute of        ;;;
;;; Technology, and currently at the Equipe Cogniscience, Institut  ;;;
;;; Alfred Fessard, CNRS and has been placed in the public domain.  ;;;
;;; If you are using this code or any part of Surf-Hippo, please    ;;;
;;; contact surf-hippo@ai.mit.edu to be put on the mailing list.    ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;; A simple optimization of the "with hash-value..." construct.

(in-package :common-lisp)


(defmacro with-hash-table-iterator ((function hash-table) &body body)
  "WITH-HASH-TABLE-ITERATOR ((function hash-table) &body body)
   provides a method of manually looping over the elements of a hash-table.
   function is bound to a generator-macro that, withing the scope of the
   invocation, returns three values.  First, whether there are any more objects
   in the hash-table, second, the key, and third, the value."
  (let ((n-function (gensym "WITH-HASH-TABLE-ITERRATOR-")))
    `(let ((,n-function
	    (let* ((table ,hash-table)
		   (weak-p (hash-table-weak-p ,hash-table))
		   (vector (hash-table-table table))
		   (length (length vector))
		   (index 0)
		   (bucket (svref vector 0)))
	      (declare (fixnum index))
	      (labels
		  ((,function ()
		     (cond
		      (bucket
		       (let ((orig bucket))
			 (setf bucket (hash-table-bucket-next orig))
			 (if (and weak-p
				  #-gengc (null (hash-table-bucket-hash orig))
				  #+gengc (hash-table-eq-bucket-p orig))
			     (multiple-value-bind
				 (key valid)
				 (weak-pointer-value
				  (hash-table-bucket-key orig))
			       (if valid
				   (values t
					   key
					   (hash-table-bucket-value orig))
				   (,function)))
			     (values t
				     (hash-table-bucket-key orig)
				     (hash-table-bucket-value orig)))))
		      ((= (incf index) length)
		       (values nil))
		      (t
		       (setf bucket (svref vector index))
		       (,function)))))
		#',function))))
       (macrolet ((,function () '(funcall ,n-function)))
	 ,@body))))


(in-package :ansi-loop)

(defun loop-hash-table-iteration-path (variable data-type prep-phrases &key which)
  (check-type which (member hash-key hash-value))
  (cond ((or (cdr prep-phrases) (not (member (caar prep-phrases) '(:in :of))))
	 (loop-error "Too many prepositions!"))
	((null prep-phrases) (loop-error "Missing OF or IN in ~S iteration path.")))
  (let ((ht-var (loop-gentemp 'loop-hashtab-))
	(next-fn (loop-gentemp 'loop-hashtab-next-))
	(dummy-predicate-var nil)
	(post-steps nil))
    (multiple-value-bind (other-var other-p)
	(named-variable (if (eq which 'hash-key) 'hash-value 'hash-key))
      ;;@@@@ named-variable returns a second value of T if the name was actually
      ;; specified, so clever code can throw away the gensym'ed up variable if
      ;; it isn't really needed.
      ;;The following is for those implementations in which we cannot put dummy NILs
      ;; into multiple-value-setq variable lists.
      #-Genera (setq other-p t
		     dummy-predicate-var (loop-when-it-variable))
      (let ((key-var nil)
	    (val-var nil)
	    (bindings `((,variable nil ,data-type)
			(,ht-var ,(cadar prep-phrases))
			,@(and other-p other-var `((,other-var nil))))))
	(if (eq which 'hash-key)
	    (setq key-var variable val-var (and other-p other-var))
	    (setq key-var (and other-p other-var) val-var variable))
	(push `(with-hash-table-iterator (,next-fn ,ht-var)) *loop-wrappers*)
	(when (consp key-var)
	  (setq post-steps `(,key-var ,(setq key-var (loop-gentemp 'loop-hash-key-temp-))
			     ,@post-steps))
	  (push `(,key-var nil) bindings))
	(when (consp val-var)
	  (setq post-steps `(,val-var ,(setq val-var (loop-gentemp 'loop-hash-val-temp-))
			     ,@post-steps))
	  (push `(,val-var nil) bindings))
	`(,bindings				;bindings
	  ()					;prologue
	  ()					;pre-test
	  ()					;parallel steps
	  (not (multiple-value-setq (,dummy-predicate-var ,key-var ,val-var) (,next-fn)))	;post-test
	  ,post-steps)))))