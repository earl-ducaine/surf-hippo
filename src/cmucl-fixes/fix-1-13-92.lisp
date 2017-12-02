;;; -*- Log: code.log; Package: LISP -*-

;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of CMU Common Lisp, please contact
;;; Scott Fahlman or slisp-group@cs.cmu.edu.
;;;
;;; Fix problem with inferring the type of non-constant keyword argument
;;; defaults.
;;;
(in-package "C")

#|
diff  -c -r1.25 ctype.lisp
*** /tmp/,RCSt1027929	Thu Jan 14 16:07:37 1993
--- ctype.lisp	Tue Jan 12 16:32:32 1993
***************
*** 659,665 ****
  			(kinfo (find key keys :key #'key-info-name)))
  		   (cond
  		    (kinfo
! 		     (res (type-union (key-info-type kinfo) def-type)))
  		    (t
  		     (note-lossage
  		      "Defining a ~S keyword not present in ~A."
--- 659,666 ----
  			(kinfo (find key keys :key #'key-info-name)))
  		   (cond
  		    (kinfo
! 		     (res (type-union (key-info-type kinfo)
! 				      (or def-type (specifier-type 'null)))))
  		    (t
  		     (note-lossage
  		      "Defining a ~S keyword not present in ~A."
***************
*** 667,674 ****
  		     (res *universal-type*)))))
  		(:required (res (pop req)))
  		(:optional
! 		 (res (type-union (pop opt)
! 				  (or def-type *universal-type*))))
  		(:rest
  		 (when (function-type-rest type)
  		   (res (specifier-type 'list)))))
--- 668,674 ----
  		     (res *universal-type*)))))
  		(:required (res (pop req)))
  		(:optional
! 		 (res (type-union (pop opt) (or def-type *universal-type*))))
  		(:rest
  		 (when (function-type-rest type)
  		   (res (specifier-type 'list)))))
|#

(defun find-optional-dispatch-types (od type where)
  (declare (type optional-dispatch od) (type function-type type)
	   (string where))
  (let* ((min (optional-dispatch-min-args od))
	 (req (function-type-required type))
	 (opt (function-type-optional type)))
    (flet ((frob (x y what)
	     (unless (= x y)
	       (note-lossage
		"Definition has ~R ~A arg~P, but ~A has ~R."
		x what x where y))))
      (frob min (length req) "fixed")
      (frob (- (optional-dispatch-max-args od) min) (length opt) "optional"))
    (flet ((frob (x y what)
	     (unless (eq x y)
	       (note-lossage
		"Definition ~:[doesn't have~;has~] ~A, but ~
		~A ~:[doesn't~;does~]."
		x what where y))))
      (frob (optional-dispatch-keyp od) (function-type-keyp type)
	    "keyword args")
      (unless (optional-dispatch-keyp od)
	(frob (not (null (optional-dispatch-more-entry od)))
	      (not (null (function-type-rest type)))
	      "rest args"))
      (frob (optional-dispatch-allowp od) (function-type-allowp type)
	    "&allow-other-keys"))

    (when *lossage-detected*
      (return-from find-optional-dispatch-types (values nil nil)))

    (collect ((res)
	      (vars))
      (let ((keys (function-type-keywords type))
	    (arglist (optional-dispatch-arglist od)))
	(dolist (arg arglist)
	  (cond
	   ((lambda-var-arg-info arg)
	    (let* ((info (lambda-var-arg-info arg))
		   (default (arg-info-default info))
		   (def-type (when (constantp default)
			       (ctype-of (eval default)))))
	      (ecase (arg-info-kind info)
		(:keyword
		 (let* ((key (arg-info-keyword info))
			(kinfo (find key keys :key #'key-info-name)))
		   (cond
		    (kinfo
		     (res (type-union (key-info-type kinfo)
				      (or def-type (specifier-type 'null)))))
		    (t
		     (note-lossage
		      "Defining a ~S keyword not present in ~A."
		      key where)
		     (res *universal-type*)))))
		(:required (res (pop req)))
		(:optional
		 (res (type-union (pop opt) (or def-type *universal-type*))))
		(:rest
		 (when (function-type-rest type)
		   (res (specifier-type 'list)))))
	      (vars arg)
	      (when (arg-info-supplied-p info)
		(res *universal-type*)
		(vars (arg-info-supplied-p info)))))
	   (t
	    (res (pop req))
	    (vars arg))))

	(dolist (key keys)
	  (unless (find (key-info-name key) arglist
			:key #'(lambda (x)
				 (let ((info (lambda-var-arg-info x)))
				   (when info
				     (arg-info-keyword info)))))
	    (note-lossage
	     "Definition lacks the ~S keyword present in ~A."
	     (key-info-name key) where))))

      (try-type-intersections (vars) (res) where))))

