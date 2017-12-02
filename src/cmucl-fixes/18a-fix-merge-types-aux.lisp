;;; -*- mode: lisp; Syntax: Common-lisp; package: surf ; base: 10; -*-
;;; Cmucl 18a fixes.
;;; Should complement any distributed lisp image.

;;; included 9/10/97, should be part of the lisp core eventually.
(in-package "C")

(defun merge-types-aux (tlist)
  ;; Merge the first interval in the list with the rest of
  ;; intervals in the list.  The list of intervals MUST be
  ;; sorted in ascending order of lower limits.
  
  (let* ((cur (first tlist))
	 (cur-intvrl (if (numeric-type-real-p cur)
			 (numeric-type->interval cur)
			 nil))
	 (res (list cur)))
    (dolist (this-interval (rest tlist) res)
      (let ((this (if (numeric-type-real-p this-interval)
		      (numeric-type->interval this-interval)
		      nil)))
	;; If the current interval is complex (cur-intvrl is nil) or
	;; the next interval is complex (this is nil), we just simply
	;; add that to the resulting list.  That is we don't try to
	;; merge complex types at all.
	;;
	;;If interval intersects cur or if they are adjacent, we can
	;;merge them together, but only if they are the same type of
	;;number.  If they are different, we can't merge them.
	(cond ((and cur-intvrl
		    this
		    (eq (numeric-type-class cur) 
			(numeric-type-class this-interval))
		    (eq (numeric-type-format cur)
			(numeric-type-format this-interval))
		    (or (interval-intersect-p cur-intvrl this)
			(interval-adjacent-p cur-intvrl this)))
	       (let ((result (interval-merge-pair cur-intvrl this)))
		 (when result
		   (setf (numeric-type-high cur)
			 (interval-high result)))))
	      (t
	       (setf res (cons this-interval res))))))))
