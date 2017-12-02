;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: OPAL; Base: 10 -*-

#|

====================================================================================================
			       The Surf-Hippo Neuron Simulator System
====================================================================================================

This code was written as part of the Surf-Hippo Project, originally at the Center for Biological
Information Processing, Department of Brain and Cognitive Sciences, Massachusetts Institute of
Technology, and currently at the Neurophysiology of Visual Computation Laboratory, CNRS.
                                                                                 
Permission to use, copy, modify, and distribute this software and its documentation for any purpose
and without fee is hereby granted, provided that this software is cited in derived published work,
and the copyright notice appears in all copies and in supporting documentation. The Surf-Hippo
Project makes no representations about the suitability of this software for any purpose. It is
provided "as is" without express or implied warranty.
                                                                                 
If you are using this code or any part of Surf-Hippo, please contact surf-hippo@ai.mit.edu to be put
on the mailing list.
                                                                                 
Copyright (c) 1989 - 2003, Lyle J. Graham                                                                                              

|#

;;

(in-package "OPAL")

;; Supplied by:
#|
Return-Path: <gilham@csl.sri.com>
To: lyle@cogni.iaf.cnrs-gif.fr (Lyle Borg-Graham)
Subject: Re: More Garnet 3.0 glitches 
In-Reply-To: Your message of "Thu, 27 Aug 1998 02:55:20 EDT."
             <9808270655.AA01096@cogni.iaf.cnrs-gif.fr> 
Date: Thu, 03 Sep 1998 11:19:29 -0700
From: Fred Gilham <gilham@csl.sri.com>
|#

;; Patch to opal/update.lisp:

(define-method :update opal:graphical-object (gob update-info
						  bbox-1 bbox-2
						  &optional (total-p NIL))
  (declare (optimize (speed 3) (safety 0)))
  (let ((old-bbox (update-info-old-bbox update-info))
	(a-window (g-value gob :window)))
    (declare (optimize (speed 3) (safety 0)))
    ;; The following fixes a problem that developed in the change from
    ;; garnet-2.2 to garnet-3.0.  When the item-prototype of a virtual
    ;; aggregate consists of an aggregadget, the :window slot doesn't
    ;; get set in the parts of the aggregate for some reason.  This
    ;; results in a run-time error when the part gets drawn.
    ;; I don't know if this is the right way to fix this, but it seems
    ;; to work and doesn't seem to break anything else so far.
    ;; Also, see the :initialize method in virtual-aggregates.lisp.
    (unless a-window
      (setf a-window (g-value gob :parent :window)))
    (unless (update-info-on-fastdraw-list-p update-info)
      (cond (total-p
	     (update-slots-values-changed gob 0 update-info)
	     (update-bbox gob old-bbox)
	     (draw gob a-window)
	     (setf (update-info-dirty-p update-info) NIL))

	    ((update-info-dirty-p update-info)
	     (when (update-info-force-computation-p update-info)
	       (update-slots-values-changed gob 0 update-info)
	       (update-bbox gob old-bbox))
	     (draw gob a-window)
	     (setf (update-info-dirty-p update-info) NIL))

	    (bbox-2			; 2 valid clip-masks?
	     (when (or (bbox-intersect-p old-bbox bbox-1)
		       (bbox-intersect-p old-bbox bbox-2))
	       (draw gob a-window)))
	    ((bbox-intersect-p old-bbox bbox-1)
	     (draw gob a-window)))
      ;; New line added because of new KR 2.0.10 -- ECP 6/23/92
      ;; Without this line, the Save window in garnetdraw does not update.
      (setf (update-info-invalid-p update-info) nil))))


;; Patch to opal/virtual-aggregates.lisp:


(define-method :initialize opal:virtual-aggregate (gob)
  (declare (optimize (speed 3) (safety 0)))
  (let ((dummy (create-instance nil (g-value gob :item-prototype)))
	array-length)
    ;; If dummy does not have :draw method, create one.
    (when (and (not (g-value dummy :draw))
	       (g-value dummy :update))

      ;; Start of fix.
      ;; The following change is necessary because the call signature
      ;; of the update function for aggregates no longer matches the
      ;; call signature of the update function for graphical objects.
      ;; Also, the window needs to be propagated.
      (if (is-a-p dummy opal:aggregate)
	(s-value dummy :draw
		 #'(lambda (dummy a-window)
		     ;; (declare (ignore a-window))
		     (s-value dummy :window a-window)
		     (update dummy (g-value dummy :update-info)
			     nil nil
			     NIL NIL T)))
	(s-value dummy :draw
		 #'(lambda (dummy a-window)
		     (declare (ignore a-window))
		     (update dummy (g-value dummy :update-info)
			     NIL NIL T)))))
    ;; End of fix.

    (s-value gob :invalid-object
	     (create-instance nil opal::virtual-invalid-object
			      (:parent gob)
			      (:make-update-think-i-have-changed 0)))
    (s-value dummy :parent gob)
    (s-value dummy :update-slots-values
	     (make-array (length (the cons (g-value dummy :update-slots)))
			 :initial-element nil))
    (s-value gob :dummy-item dummy)
    (unless (g-value gob :item-array)
      (s-value gob :item-array
	       (make-array 0 :adjustable t :initial-element nil)))
    (setq array-length (array-dimensions (g-value gob :item-array)))
    (if (cdr array-length)		; TWO-DIMENSIONAL
      (progn
	(s-value gob :add-item NIL)
	(s-value gob :remove-item NIL))
      (setq array-length (car array-length)))
    (s-value gob :array-length array-length)
    (s-value gob :bbox-array (make-array (the fixnum array-length) :element-type 'opal::bbox :adjustable t))
    (when (numberp array-length);; one dimensional
      (s-value gob :next-available-rank array-length))
    (call-prototype-method gob)
    (recalculate-virtual-aggregate-bboxes gob) ; expensive
    (update-slots-values-changed gob 0 (g-local-value gob :update-info))
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 29.8.99 Noticed that the instances of a virtual agg's prototype are not destroyed when the parent
;; is. This seems to be one hack to do it.
(define-method :destroy-me opal:virtual-aggregate (gob &optional (top-level-p t))
  (DESTROY-ME-METHOD-VIEW-OBJECT (g-value gob :dummy-item)) ; Explicitly kill the :dummy-item.
  (call-prototype-method gob top-level-p))


#|
;; debugging
(defun do-partial-update (invalid-objects invalid-xors invalid-copys a-window
			  window-agg buffer exposed-clip-mask
			  line-style-gc filling-style-gc obj-update-info
                          obj-update-slots-values win-info win-new-bbox
                          win-old-bbox fastdraw-objects)
  (let (obj-old-bbox f-obj-update-info f-obj-old-bbox)
	(setf (bbox-valid-p win-new-bbox) NIL)
	;;; First deal with FASTDRAWs.
	(dothings (fastdraws invalid-xors invalid-copys)
	  (let (first-changed)
	    (dolist (object fastdraws)
	      (when (and (schema-p object) ; check if it was already destroyed
			 (or (not (setq obj-update-slots-values
					(g-local-value object
						       :update-slots-values)))
			     (aref obj-update-slots-values 1)
			     (not (aref obj-update-slots-values 0))))
		(setf (update-info-invalid-p
		       (setq obj-update-info
			     (the UPDATE-INFO
				  (g-local-value object :update-info))))
		      NIL)
		;; Check if it really has changed!
		(when (setq first-changed
			    (simple-update-slots-values-changed object))
		  ;; if it was visible, erase it...
		  (when (and obj-update-slots-values
			     (aref obj-update-slots-values 0))
		    ;; Change for values of :rectangle and :redraw in
		    ;; :fast-redraw-p slot  --Andrew Mickish
		    (case (g-value object :fast-redraw-p)
		      (:rectangle
		       ;; Draw a rectangle over the bbox of the object.
		       ;; This rectangle will have the background filling
		       ;; style, so the object will disappear.
		       (set-frr-bbox object)
		       (setf (aref frr-update-vals *rect-fstyle*)
			     (g-value object :fast-redraw-filling-style))
		       (fast-erase fast-redraw-rectangle a-window
				   line-style-gc filling-style-gc))
		      (:redraw
		       ;; Set the filling and line styles of the object to be
		       ;; the background styles, redraw the object, restore
		       ;; its real styles (the changes occur in the update-
		       ;; values-slots array, not the object's style slots)
		       (set-styles object
				   (g-value object :fast-redraw-line-style)
				   (g-value object :fast-redraw-filling-style))
		       (fast-erase object a-window
				   line-style-gc filling-style-gc)
		       (set-styles object
				   (g-value object :line-style)
				   (g-value object :filling-style)))
		      (t
		       ;; The object is drawn with an :xor draw-function,
		       ;; so just draw the object again to erase it.
		       (fast-erase object a-window
				    line-style-gc filling-style-gc))))
		  (if (g-value object :visible)
		    (progn
		      ;; Add "first-changed" & object to
		      ;; the fastdraw list to draw later
		      (setq fastdraw-objects
			    (get-cons object
				      (get-cons first-changed
						fastdraw-objects)))
		      (setf (update-info-on-fastdraw-list-p obj-update-info)
			    T))
		    ;; ELSE it's NOT VISIBLE....
		    (progn
		      (if obj-update-slots-values
			(setf (aref obj-update-slots-values 0) NIL))
		      (merge-bbox newly-invisible-fastdraws-bbox
				  (update-info-old-bbox obj-update-info))
		      (setf (bbox-valid-p
			     (update-info-old-bbox obj-update-info))
			    NIL))))))
	    (free-list fastdraws)))

	;; Now process non-FASTDRAWs
	(when invalid-objects
	  (dolist (object invalid-objects)
	    ;; The next line represents a temporary hack to deal with a
	    ;; problem discovered in demo-arith, in which occasionally
	    ;; objects marked as *DESTROYED* were still contained in
	    ;; the invalid objects list.
	    (when (schema-p object)
	      (setq obj-old-bbox
		    (update-info-old-bbox
		     (the UPDATE-INFO
			  (setq obj-update-info
				(g-local-value object :update-info)))))
	      (setf (update-info-invalid-p obj-update-info) NIL)
	      (setq obj-update-slots-values
		    (g-local-value object :update-slots-values))
	      (if (g-value object :visible)
		;; Object is a VISIBLE NORMAL OBJ
		(if (bbox-valid-p obj-old-bbox)	
		  ;;object IS and WAS visible
		  (when (update-slots-values-changed object 0 obj-update-info)
		    (merge-bbox win-old-bbox obj-old-bbox)
		    (update-bbox object obj-old-bbox)
		    (merge-bbox win-new-bbox obj-old-bbox)
		    (propagate-dirty-bit object obj-update-info)
		    )
		  (progn;;object IS and WAS NOT visible
		    (update-bbox object obj-old-bbox)
		    (update-slots-values-changed object 0 obj-update-info)
		    (merge-bbox win-new-bbox obj-old-bbox)
		    (propagate-dirty-bit object obj-update-info)
		    ))
		(when (bbox-valid-p obj-old-bbox);object IS NOT and WAS visible
		  (merge-bbox win-old-bbox obj-old-bbox)
		  (setf (bbox-valid-p obj-old-bbox)
			(setf (aref obj-update-slots-values 0)
			      NIL)))
		;;if object IS NOT and WAS NOT
		;;visible, then do nothing!!
		)))
	  (free-list invalid-objects))

	;; Now only perform the update if one
	;; of the two window's bboxes is valid.
	(let ((old-bbox-valid (bbox-valid-p win-old-bbox))
	      (new-bbox-valid (bbox-valid-p win-new-bbox))
	      (clip-mask-1 (win-update-info-clip-mask-1 win-info))
	      (clip-mask-2 (win-update-info-clip-mask-2 win-info))
	      two-bboxes-p)
	  (when (or new-bbox-valid old-bbox-valid)

	    (if (setq two-bboxes-p (and new-bbox-valid old-bbox-valid))
	      (if (bbox-intersect-p win-old-bbox win-new-bbox) ;they intrsect?
		(progn
		  (merge-bbox win-new-bbox win-old-bbox) ;merge into new
		  (setq two-bboxes-p NIL);; really only 1!
		  ;; (setf (bbox-valid-p win-old-bbox) NIL) ;; save until end
		  (erase-bbox win-new-bbox a-window buffer)
		  (bbox-to-clip-mask win-new-bbox clip-mask-1))
		(progn
		  ;; (setf (bbox-valid-p win-old-bbox) NIL) ;; save until end
		  (erase-bbox win-old-bbox a-window buffer)
		  (erase-bbox win-new-bbox a-window buffer)
		  (bbox-to-clip-mask win-old-bbox clip-mask-1)
		  (bbox-to-clip-mask win-new-bbox clip-mask-2)))

	      (progn                           ;; Only one valid bbox
		(when old-bbox-valid
		  (swap win-old-bbox win-new-bbox)
		  ;; (setf (bbox-valid-p win-old-bbox) NIL)
		  ;; save 'til end
		  )
		(erase-bbox win-new-bbox a-window buffer)
		(bbox-to-clip-mask win-new-bbox clip-mask-1)))

	    (if two-bboxes-p
	       (progn
		 (gem:set-clip-mask a-window clip-mask-2
				    line-style-gc filling-style-gc)
	         (update-method-aggregate
		  window-agg (g-local-value window-agg :update-info)
		  line-style-gc filling-style-gc
		  win-old-bbox win-new-bbox NIL))
	       (progn
	         (gem:set-clip-mask a-window clip-mask-1
				    line-style-gc filling-style-gc)
		 (update-method-aggregate
		  window-agg (g-local-value window-agg :update-info)
		  line-style-gc filling-style-gc win-new-bbox NIL NIL)))))
	;; If there are fastdraw objects, draw
	;; them, then clear the list....
	(when fastdraw-objects
	  (do* ((flist         fastdraw-objects (cddr flist))
		(fastdraw-obj  (first flist) (first flist))
		(first-changed (second flist) (second flist)))
	       ((null flist))
	    (setq f-obj-old-bbox
		  (update-info-old-bbox
		   (the UPDATE-INFO
			(setq f-obj-update-info
			      (g-local-value fastdraw-obj :update-info)))))
	    (update-slots-values-changed fastdraw-obj first-changed
					 f-obj-update-info)
	    (when buffer
	      (merge-bbox win-old-bbox f-obj-old-bbox))

	    ;; Next 2 lines are for parent propagation (** below)
	    (swap fastdraw-old-bbox f-obj-old-bbox)
	    (setf (update-info-old-bbox f-obj-update-info) f-obj-old-bbox)

	    (update-bbox fastdraw-obj f-obj-old-bbox)
	    (when buffer
	      (merge-bbox win-old-bbox f-obj-old-bbox))
	    (unless buffer
	      (gem:set-clip-mask a-window :none line-style-gc filling-style-gc)
	      (draw fastdraw-obj a-window))

	    ;; (**) Now must propagate bbox changes to parent(s), but
	    ;; ONLY IF NECESSARY, and then as CHEAPLY AS POSSIBLE!!!(koz)
	    (let ((old-bbox fastdraw-old-bbox)
		  (new-bbox f-obj-old-bbox)
		  (object   fastdraw-obj)
		  parent parent-ui parent-bbox parent-changed?)
	      (loop

	       ;; If there is no parent, return!
	       (if (null (setq parent (g-local-value object :parent)))
		 (return))

	       ;; else, (re)set parent-ui, parent-bbox and parent-changed?
	       (setq parent-bbox
		     (update-info-old-bbox
		      (the UPDATE-INFO
			   (setq parent-ui (g-local-value parent :update-info)))))
	       (setq parent-changed? NIL)

	       ;; If the parent-bbox has never been updated, then its
	       ;; valid-p will be NIL, so copy current fastdraw bbox
	       ;; into it, and set up to check parent's parent.
	       (if (null (bbox-valid-p parent-bbox))
		 (progn
		   (setq parent-changed? T)
		   (copy-bbox-fn parent-bbox new-bbox)
		   (setf (bbox-valid-p parent-old-bbox) NIL))

		 (progn;; else for (if (null (bbox-valid-p parent-bbox)) ...)
		   (when (or (< (bbox-x1 new-bbox) (bbox-x1 parent-bbox))
			     (> (bbox-x2 new-bbox) (bbox-x2 parent-bbox))
			     (< (bbox-y1 new-bbox) (bbox-y1 parent-bbox))
			     (> (bbox-y2 new-bbox) (bbox-y2 parent-bbox)))
		     (setq parent-changed? T)
		     ;; Must copy explicitly, instead of using SWAP, since
		     ;; the old values are also needed by merge-bbox below
		     (copy-bbox-fn parent-old-bbox parent-bbox)
		     (merge-bbox parent-bbox new-bbox))
		  

		   ;; Now, if for any dimension, both:
		   ;;  * old-bbox defines boundary of parent-bbox
		   ;;    (ie, old-bbox equals parent-bbox), and
		   ;;  * old-bbox does not equal new-bbox
		   ;; Then deleting the old-bbox contracts the parent-bbox.
		   (when
		       (and (bbox-valid-p old-bbox)
			    (or (and (eql  (bbox-x1 old-bbox) (bbox-x1 parent-bbox))
				     (not (eql (bbox-x1 old-bbox) (bbox-x1 new-bbox))))
				(and (eql  (bbox-x2 old-bbox) (bbox-x2 parent-bbox))
				     (not (eql (bbox-x2 old-bbox) (bbox-x2 new-bbox))))
				(and (eql  (bbox-y1 old-bbox) (bbox-y1 parent-bbox))
				     (not (eql (bbox-y1 old-bbox) (bbox-y1 new-bbox))))
				(and (eql  (bbox-y2 old-bbox) (bbox-y2 parent-bbox))
				     (not (eql (bbox-y2 old-bbox) (bbox-y2 new-bbox))))))

		     ;; so, if parent-changed? is NIL, set it to T and store
		     ;; parent-old-bbox.  Then we finally cannot avoid the
		     ;; expensive operation of update-bbox (ack!)
		     (unless parent-changed?
		       (setq parent-changed? T)
		       (swap parent-old-bbox parent-bbox)
		       (setf (update-info-old-bbox parent-ui) parent-bbox))
		     (update-bbox parent parent-bbox))
		   )
		 )			; close (if (null (bbox-valid-p parent-bbox)) ...)

	       ;; Finally, if parent-changed? is T, then set up
	       ;; variables for next iteration, else return!
	       (if parent-changed?
		 (progn
		   (swap parent-old-bbox fastdraw-old-bbox)
		   (setq old-bbox fastdraw-old-bbox)
		   (setq new-bbox parent-bbox)
		   (setq object   parent))
		 (return))))
	    ;; (**) Done propagating bbox changes to parent(s)

	    (when buffer
	      (gem:set-clip-mask a-window :none line-style-gc filling-style-gc)
	      (draw fastdraw-obj a-window))
	    (setf (update-info-on-fastdraw-list-p f-obj-update-info)
		  NIL))
	   (free-list fastdraw-objects))))

(define-method :destroy-me opal:virtual-aggregate (a-aggregate &optional (top-level-p t))
  (if a-aggregate
      
      (let* ((the-window (g-local-value a-aggregate :window))
             (erase-p (and top-level-p the-window))
             (parent  (g-local-value a-aggregate :parent))
             total-update-p)
	(format t "New Destroy me v-agg ~A~%" a-aggregate)
        (if erase-p;; If at top-level, then erase...
            (if (null parent)
                (if (eq a-aggregate (g-value the-window :aggregate))
                    (s-value the-window :aggregate NIL)
                    (progn
                      (format t "~%Warning in Destroy: aggregate '~A' has no parent,~%" a-aggregate)
                      (format t   "        is in window '~A', but is not that window's:aggregate.~%"
                              the-window)
                      (setq erase-p NIL)))
                (setq total-update-p (not (carefully-erase a-aggregate the-window)))))
        (when (and top-level-p parent)
          (s-value parent :components
		   (delete a-aggregate (g-local-value parent :components)))
          (mark-as-changed parent :components))
        (let ((components (g-local-value a-aggregate :components)))
          (with-constants-disabled (destroy-slot a-aggregate :components))
	  (format t "~A components ~A~%" a-aggregate components)
          (dolist (component components)
            (when (schema-p component)
              (destroy component NIL))))
        (destroy-schema a-aggregate)
        (if erase-p
            (update the-window total-update-p)))))

(define-method :destroy-me opal:aggregate (a-aggregate &optional (top-level-p T))
  (if a-aggregate
      (let* ((the-window (g-local-value a-aggregate :window))
             (erase-p (and top-level-p the-window))
             (parent  (g-local-value a-aggregate :parent))
             total-update-p)
        (if erase-p;; If at top-level, then erase...
            (if (null parent)
                (if (eq a-aggregate (g-value the-window :aggregate))
                    (s-value the-window :aggregate NIL)
                    (progn
                      (format t "~%Warning in Destroy: aggregate '~A' has no parent,~%" a-aggregate)
                      (format t   "        is in window '~A', but is not that window's:aggregate.~%"
                              the-window)
                      (setq erase-p NIL)))
                (setq total-update-p (not (carefully-erase a-aggregate the-window)))))
        (when (and top-level-p parent)
          (s-value parent :components
		   (delete a-aggregate (g-local-value parent :components)))
          (mark-as-changed parent :components))
        (let ((components (g-local-value a-aggregate :components)))
          (with-constants-disabled (destroy-slot a-aggregate :components))
;	  (format t "~A components ~A~%" a-aggregate components)
          (dolist (component components)
            (when (schema-p component)
              (destroy component NIL))))
        (destroy-schema a-aggregate)
        (if erase-p
            (update the-window total-update-p)))))
|#
