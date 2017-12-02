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

;; This file is modified from Garnet source code.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;         The Garnet User Interface Development Environment.      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This code was written as part of the Garnet project at          ;;;
;;; Carnegie Mellon University, and has been placed in the public   ;;;
;;; domain.  If you are using this code or any part of Garnet,      ;;;
;;; please contact garnet@cs.cmu.edu to be put on the mailing list. ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; improvements to some macros and functions

(in-package "KR")

(defmacro call-prototype-method (&rest args)
  (let ((entry (gensym)))
    `(let ((first-c-p-m (and (null *kr-send-parent*)
			     (let ((,entry (slot-accessor *kr-send-self*
							  *kr-send-slot*)))
			       (or (null ,entry)
				   (is-inherited (sl-bits ,entry)))))))
      (multiple-value-bind (method new-parent)
	  (find-parent *kr-send-self* *kr-send-slot*)
	(when method
	  (if first-c-p-m
	    (multiple-value-setq (method *kr-send-parent*)
	      (find-parent new-parent *kr-send-slot*))
	    (setf *kr-send-parent* new-parent))
	  (if method
	    (let ((*kr-send-self* *kr-send-parent*))
	      (funcall method ,@args))))))))


(in-package "OPAL")

;;; "thing" is a virtual aggregate.
(proclaim '(inline set-things-size))
(defun set-things-size (thing thing-bbox)
  (declare (optimize (safety 0) (speed 3) (space 1)))
  (s-value thing :left (bbox-x1 thing-bbox))
  (s-value thing :top (bbox-y1 thing-bbox))
  (s-value thing :width (the fixnum (- (the fixnum (bbox-x2 thing-bbox)) (the fixnum (bbox-x1 thing-bbox)))))
  (s-value thing :height (the fixnum (- (the fixnum (bbox-y2 thing-bbox)) (the fixnum (bbox-y1 thing-bbox))))))

(proclaim '(inline initialize-item-bbox))
(defun initialize-item-bbox (thing thing-bbox bbox-array dummy item-array rank &optional rank2)
  (declare (optimize (safety 0) (speed 3) (space 1)))
  (let (src-bbox)
    (if rank2
	(progn
	  (s-value dummy :rank1 rank)
	  (s-value dummy :rank2 rank2))
        (s-value dummy :rank rank))
    (s-value dummy :item-values 
	     (if rank2
		 (aref (the (simple-array cons (* *)) item-array) rank rank2)
		 (aref (the (simple-array cons (*)) item-array) rank)))
    (if rank2
	(setf (aref (the (array opal::bbox (*)) bbox-array) rank rank2) (make-bbox))
	(setf (aref (the (array opal::bbox (* *)) bbox-array) rank) (make-bbox)))
    (setq src-bbox
	  (if rank2
	      (aref (the (array opal::bbox (*)) bbox-array) rank rank2)
	      (aref (the (array opal::bbox (* *)) bbox-array) rank)))
    (update-bbox dummy src-bbox)
    (merge-bbox thing-bbox src-bbox)
    (set-things-size thing thing-bbox)))

(defun recalculate-virtual-aggregate-bboxes (thing)
  (declare (optimize (safety 0) (speed 3) (space 1)))
  (let ((dummy (g-value thing :dummy-item))
	(item-array (g-value thing :item-array))
	(bbox-array (g-value thing :bbox-array))
	(array-length (g-value thing :array-length))
        (thing-bbox (update-info-old-bbox (the UPDATE-INFO (g-value thing :update-info)))))
    (if (numberp array-length);; one-dimensional
	(do ((n 0 (the fixnum (1+ n))))
	    ((= (the fixnum n) (the fixnum array-length)))
	  (when (aref (the (simple-array cons (*)) item-array) n)
	    (initialize-item-bbox thing thing-bbox bbox-array dummy item-array n)))
	(do ((m 0 (the fixnum (1+ m))))
	    ((= (the fixnum m) (the fixnum (first array-length))))
	  (do ((n 0 (the fixnum (1+ n))))
	      ((= (the fixnum n) (the fixnum (second array-length))))
	    (when (aref (the (simple-array cons (* *)) item-array) m n)
	      (initialize-item-bbox thing thing-bbox bbox-array dummy item-array m n)))))
    (if (and thing-bbox (bbox-valid-p thing-bbox))
	(set-things-size thing thing-bbox)
	(progn
	  (s-value thing :left 0)
	  (s-value thing :top 0)
	  (s-value thing :width 0)
	  (s-value thing :height 0)))))


(define-method :initialize opal:virtual-aggregate (gob)
  (let ((dummy (create-instance nil (g-value gob :item-prototype)))
	array-length)
    ;; If dummy does not have :draw method, create one.
    (when (and (not (g-value dummy :draw))
	       (g-value dummy :update))
      (s-value dummy :draw
	 #'(lambda (dummy line-style-gc filling-style-gc drawable root-window)
	     (update dummy (g-value dummy :update-info) line-style-gc filling-style-gc
		     drawable root-window NIL NIL T))))	     
    (s-value gob :invalid-object
      (create-instance nil opal::virtual-invalid-object
	(:parent gob)
	(:make-update-think-i-have-changed 0)))
    (s-value dummy :parent gob)
    (s-value dummy :update-slots-values
       (make-array (length (g-value dummy :update-slots)) :initial-element nil))
    (s-value gob :dummy-item dummy)
    (unless (g-value gob :item-array)
      (s-value gob :item-array (make-array 0 :adjustable t :initial-element nil)))
    (setq array-length (array-dimensions (g-value gob :item-array)))
    (if (cdr array-length) ; TWO-DIMENSIONAL
	(progn
	  (s-value gob :add-item NIL)
	  (s-value gob :remove-item NIL))
	(setq array-length (car array-length)))
    (s-value gob :array-length array-length)
    (s-value gob :bbox-array (make-array array-length :element-type 'opal::bbox ; :adjustable t
					 ))
    (when (numberp array-length) ;; one dimensional
      (s-value gob :next-available-rank array-length))
    (call-prototype-method gob)
    (recalculate-virtual-aggregate-bboxes gob)
    (update-slots-values-changed gob 0 (g-local-value gob :update-info))))



  
(define-method :draw opal:line (gob line-style-gc filling-style-gc
				drawable root-window)
  (declare (optimize (safety 0) (speed 3) (space 0))
	   (ignore filling-style-gc))
  (let* ((xlib-gc-line (opal::opal-gc-gcontext line-style-gc))
	 (update-vals  (g-local-value gob :update-slots-values))
	 (lstyle       (aref (the (simple-array * (*)) update-vals) OPal::*line-lstyle*))
	 (x-draw-fn    (get (aref (the (simple-array * (*)) update-vals) opal::*line-draw-function*)
			    :x-draw-function)))
  (when lstyle
    (opal::set-line-style lstyle line-style-gc xlib-gc-line root-window x-draw-fn)
    (xlib:draw-line drawable
		    xlib-gc-line
		    (aref (the (simple-array * (*)) update-vals) opal::*line-x1*)
		    (aref (the (simple-array * (*)) update-vals) opal::*line-y1*)
		    (aref (the (simple-array * (*)) update-vals) opal::*line-x2*)
		    (aref (the (simple-array * (*)) update-vals) opal::*line-y2*)))))


(defmacro do-in-clip-rect ((m n agg rect) &body body)
  `(let* ((agg* ,agg)
	  (p-to-r (g-value agg* :point-to-rank))
	  (r* ,rect)
	  (array-size* (g-value agg* :array-length))
	  (max-x2* (1- (first array-size*)))
	  (max-y2* (1- (second array-size*)))
	  (first* (first r*))
	  (second* (second r*)))
    (declare (fixnum first* second* max-x2* max-y2*)) 
     (multiple-value-bind (x1* y1*)
       		          (funcall p-to-r agg* first* second*)
       (multiple-value-bind (x2* y2*)
			    (funcall p-to-r agg* (the fixnum (+ first* (the fixnum (third r*)) -1))
				     (the fixnum (+ second* (the fixnum (fourth r*)) -1)))
	 (declare (fixnum x1* x2* y1* y2*)) 
	 (setq x1* (the fixnum (if x1* (max 0 x1*) 0)))
	 (setq y1* (the fixnum (if y1* (max 0 y1*) 0)))
	 (setq x2* (the fixnum (if x2* (min x2* max-x2*) max-x2*)))
	 (setq y2* (the fixnum (if y2* (min y2* max-y2*) max-y2*)))
	 (when (and (<= x1* x2*) (<= y1* y2*))
	   (do ((,m x1* (1+ (the fixnum ,m))))
	       ((> (the fixnum ,m) (the fixnum x2*)))
	     (do ((,n y1* (1+ (the fixnum ,n))))
	         ((> (the fixnum ,n) (the fixnum y2*)))
	       ,@body)))))))




(defmacro copy-from-buffer-to-drawable (bbox buffer buffer-gc drawable)
  `(let ((x1 (the fixnum (bbox-x1 ,bbox)))
	 (x2 (the fixnum (bbox-x2 ,bbox)))
	 (y1 (the fixnum (bbox-y1 ,bbox)))
	 (y2 (the fixnum (bbox-y2 ,bbox))))
     (xlib:copy-area ,buffer ,buffer-gc x1 y1 (- x2 x1) (- y2 y1)
		     ,drawable x1 y1)))

#-GARNET-V3.0
(define-method :update opal:aggregate (agg update-info
				       line-style-gc filling-style-gc
				       drawable root-window
				       bbox-1 bbox-2
				       &optional (total-p NIL))
  (declare (optimize (speed 3) (safety 0)))
  (let ((dirty-p (update-info-dirty-p update-info))
	(agg-bbox (update-info-old-bbox update-info)))
      (when
	(or  dirty-p
	     total-p
	     (and (bbox-valid-p agg-bbox)
	          (bbox-intersects-either-p agg-bbox bbox-1 bbox-2)))
	(let (child-update-info child-bbox)
	  (setf (bbox-valid-p agg-bbox) NIL)		;; clear the old one!
	  (dovalues (child agg :components :local t)
	    (if (g-value child :visible)
		(progn
		  (setq child-bbox
			(update-info-old-bbox
			 (setq child-update-info
			       (g-local-value child :update-info))))
		  ; (format t "Updating ~A...~%" child)
		  (update child child-update-info
                                line-style-gc filling-style-gc
                                drawable root-window
                                bbox-1 bbox-2
                                total-p)
		  ; (format t "Done Updating...~%")
		  (merge-bbox agg-bbox child-bbox))	;; and set the new one!
		; else
		;; if the child's dirty bit is set, recursively visit the child
		;; and all its children and turn off their dirty bits
		(let ((child-update-info (g-local-value child :update-info)))
		  (when (update-info-dirty-p child-update-info)
		     (clear-dirty-bits child child-update-info)))))
	  (if dirty-p (setf (update-info-dirty-p update-info) NIL))
	))))

#-GARNET-V3.0
(define-method :update opal::window (a-window &optional (total-p NIL))
  (declare (optimize (speed 3) (safety 0)))
  (unwind-protect
    (progn
      (update-start-fn a-window)
; (breaK)
 (let* ((win-info (g-local-value a-window :win-update-info))
	(drawable (g-local-value a-window :drawable))
	(window-agg (g-local-value a-window :aggregate))
	(invalid-slots (win-update-info-invalid-slots win-info))
	invalid-vobs)

   (unless drawable
     (setq drawable (install-drawable window-agg a-window win-info))
     (setq total-p T))

   (when invalid-slots
     (setq invalid-slots (fix-invalid-slots invalid-slots win-info a-window))
     (if (process-invalid-slots invalid-slots win-info a-window drawable)
	 (setq total-p T)))
   
   ;; If this is a total update, call the :fix-update-slots method on every
   ;; object in the window that has one.
   (when total-p
     (do-fix-update-slots win-info a-window))

 ;; The "invalid-view-objects" code iterates through all view-objects
 ;; which were invalidated and (if they are still in the window) calls
 ;; their :fix-update-slots method.  Since these can invalidate *other*
 ;; view-objects with their side-effects, this must loop until no more
 ;; view-objects are being invalidated.
   (loop
      (setq invalid-vobs (win-update-info-invalid-view-objects win-info))
      (unless invalid-vobs (return))
      (setf (win-update-info-invalid-view-objects win-info) NIL)
      (dolist (vob invalid-vobs)
        (if (and (schema-p vob) (eq a-window (g-local-value vob :window)))
           (fix-update-slots vob)))
      (free-list invalid-vobs))
	       
 (let* ((invalid-objects (win-update-info-invalid-objects win-info))
	(invalid-xors    (win-update-info-invalid-xor-fastdraws win-info))
	(invalid-copys   (win-update-info-invalid-copy-fastdraws win-info))
	(visible         (eq (g-value a-window :visible) T))
	(win-old-bbox    (update-info-old-bbox
			  (the UPDATE-INFO
			       (g-local-value a-window :update-info))))
	(partial-p       (and window-agg
                              (g-value window-agg :visible)
                              (or invalid-objects
                                  invalid-xors
                                  invalid-copys
                                  (bbox-valid-p win-old-bbox))))
	)


   (when visible
    (setf (win-update-info-invalid-objects        win-info) nil
	  (win-update-info-invalid-xor-fastdraws  win-info) nil
	  (win-update-info-invalid-copy-fastdraws win-info) nil))
  
  ;;; At this point, we try to abort if possible -- only do the main part
  ;;; of update if something really has changed...
  (when (or total-p partial-p)
    (let* (
	(win-new-bbox	  (win-update-info-new-bbox win-info))
	(buffer           (g-value a-window :buffer))
        (display-info     (g-value a-window :display-info))
	(line-style-gc    (display-info-line-style-gc    display-info))
	(filling-style-gc (display-info-filling-style-gc display-info))
	(root-window      (display-info-root-window      display-info))
	buffer-gc
	fastdraw-objects
	obj-update-slots-values
	obj-update-info
      )

  (when buffer
    (setq buffer-gc (g-value a-window :buffer-gcontext))
    (setf (bbox-valid-p newly-invisible-fastdraws-bbox) nil))

  (when (and window-agg visible)
    (if total-p
	(do-total-update invalid-objects invalid-xors invalid-copys a-window
			 window-agg drawable buffer buffer-gc exposed-clip-mask
			 line-style-gc filling-style-gc root-window)
      
   ;else this is a PARTIAL update
      (let (obj-old-bbox f-obj-update-info f-obj-old-bbox)
	(setf (bbox-valid-p win-new-bbox) NIL)

	;;; First Deal with FASTDRAWs
 (dothings (fastdraws invalid-xors invalid-copys)
 (let (first-changed)
  (dolist (object fastdraws)
    (when (and (schema-p object) ; check if it has already been destroyed
	       (or (not (setq obj-update-slots-values
			      (g-local-value object :update-slots-values)))
		   (aref obj-update-slots-values 1)
		   (not (aref obj-update-slots-values 0))))
      (setf (update-info-invalid-p
               (setq obj-update-info
	         (the UPDATE-INFO (g-local-value object :update-info))))
	    NIL)
		;; Check if it really has changed!
      (when (setq first-changed (simple-update-slots-values-changed object))
					;; if it was visible, erase it...
	(when (and obj-update-slots-values
	      (aref obj-update-slots-values 0))
		     ;; Change for values of :rectangle and :redraw in
		     ;; :fast-redraw-p slot  --Andrew Mickish
		     (case (g-value object :fast-redraw-p)
		       (:rectangle
			; Draw a rectangle over the bbox of the object.
			; This rectangle will have the background filling
			; style, so the object will disappear.
			(set-frr-bbox object)
			(setf (aref frr-update-vals *rect-fstyle*)
			  (g-value object :fast-redraw-filling-style))
			(fast-erase fast-redraw-rectangle line-style-gc
				    filling-style-gc drawable root-window
				    buffer))
		       (:redraw
			; Set the filling and line styles of the object to be
			; the background styles, redraw the object, restore
			; its real styles (the changes occur in the update-
			; values-slots array, not the object's style slots)
			(set-styles object
			  (g-value object :fast-redraw-line-style)
			  (g-value object :fast-redraw-filling-style))
			(fast-erase object line-style-gc filling-style-gc
				    drawable root-window buffer)
			(set-styles object
			  (g-value object :line-style)
			  (g-value object :filling-style)))
		       (t
			; The object is drawn with an :xor draw-function,
			; so just draw the object again to erase it.
			(fast-erase object line-style-gc filling-style-gc
				    drawable root-window buffer))))

		  (if (g-value object :visible)
		   (progn
					;; Add "first-changed" & object to
					;; the fastdraw list to draw later
		     (setq fastdraw-objects
		       (get-cons object
		                 (get-cons first-changed fastdraw-objects)))
		     (setf (update-info-on-fastdraw-list-p obj-update-info) T))

					;;; ELSE it's NOT VISIBLE....
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
	       (progn				;;object IS and WAS NOT visible
		(update-bbox object obj-old-bbox)
		(update-slots-values-changed object 0 obj-update-info)
		(merge-bbox win-new-bbox obj-old-bbox)
		(propagate-dirty-bit object obj-update-info)
		))
	    (when (bbox-valid-p obj-old-bbox)	;;object IS NOT and WAS visible
		(merge-bbox win-old-bbox obj-old-bbox)
		(setf (bbox-valid-p obj-old-bbox)
		   (setf (aref obj-update-slots-values 0)
			NIL))
            )
						;;if object IS NOT and WAS NOT
						;;visible, then do nothing!!
	  )))
	(free-list invalid-objects))

					;; Now only perform the update if one
					;; of the two window's bboxes is valid
	(let ((old-bbox-valid (bbox-valid-p win-old-bbox))
	      (new-bbox-valid (bbox-valid-p win-new-bbox))
	      (clip-mask-1 (win-update-info-clip-mask-1 win-info))
	      (clip-mask-2 (win-update-info-clip-mask-2 win-info))
	      two-bboxes-p)
	  (when (or new-bbox-valid old-bbox-valid)

	    (if (setq two-bboxes-p (and new-bbox-valid old-bbox-valid))
               (if (bbox-intersect-p win-old-bbox win-new-bbox) ;they intrsect?
		   (progn
                        (merge-bbox win-new-bbox win-old-bbox)  ;merge into new
                        (setq two-bboxes-p NIL)                 ;; really only 1!
		     ;; (setf (bbox-valid-p win-old-bbox) NIL) ;; save until end
                        (erase-bbox win-new-bbox drawable buffer buffer-gc)
                        (bbox-to-clip-mask win-new-bbox clip-mask-1))
                   (progn
		     ;; (setf (bbox-valid-p win-old-bbox) NIL) ;; save until end
			(erase-bbox win-old-bbox drawable buffer buffer-gc)
			(erase-bbox win-new-bbox drawable buffer buffer-gc)
			(bbox-to-clip-mask win-old-bbox clip-mask-1)
			(bbox-to-clip-mask win-new-bbox clip-mask-2)))

               (progn                           ;; Only one valid bbox
			(when old-bbox-valid
                                (swap win-old-bbox win-new-bbox)
			     ;; (setf (bbox-valid-p win-old-bbox) NIL)
                             ;; save 'til end
			        )
			(erase-bbox win-new-bbox drawable buffer buffer-gc)
			(bbox-to-clip-mask win-new-bbox clip-mask-1)))

	    (if two-bboxes-p
	       (progn
	         (set-gc-clip-masks line-style-gc filling-style-gc clip-mask-2)
	         (update-method-aggregate window-agg
		   (g-local-value window-agg :update-info)
		   line-style-gc filling-style-gc 
		   (or buffer drawable) root-window
		   win-old-bbox win-new-bbox
		   NIL))
	       (progn
	         (set-gc-clip-masks line-style-gc filling-style-gc clip-mask-1)
		 (update-method-aggregate window-agg
		   (g-local-value window-agg :update-info)
		   line-style-gc filling-style-gc
		   (or buffer drawable) root-window
		   win-new-bbox NIL
		   NIL))
	     )
	   ))
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
		  (set-gc-clip-masks line-style-gc filling-style-gc :none)
	  	  (draw fastdraw-obj line-style-gc filling-style-gc
		        drawable root-window)
		)

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

		(progn  ; else for (if (null (bbox-valid-p parent-bbox)) ...)
                  (when (or (< (the fixnum (bbox-x1 new-bbox)) (the fixnum (bbox-x1 parent-bbox)))
                            (> (the fixnum (bbox-x2 new-bbox)) (the fixnum (bbox-x2 parent-bbox)))
                            (< (the fixnum (bbox-y1 new-bbox)) (the fixnum (bbox-y1 parent-bbox)))
                            (> (the fixnum (bbox-y2 new-bbox)) (the fixnum (bbox-y2 parent-bbox))))
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
		      (or (and (eql  (the fixnum (bbox-x1 old-bbox)) (the fixnum (bbox-x1 parent-bbox)))
			   (not (eql (the fixnum (bbox-x1 old-bbox)) (the fixnum (bbox-x1 new-bbox)))))
			  (and (eql  (the fixnum (bbox-x2 old-bbox)) (the fixnum (bbox-x2 parent-bbox)))
			   (not (eql (the fixnum (bbox-x2 old-bbox)) (the fixnum (bbox-x2 new-bbox)))))
			  (and (eql  (the fixnum (bbox-y1 old-bbox)) (the fixnum (bbox-y1 parent-bbox)))
			   (not (eql (the fixnum (bbox-y1 old-bbox)) (the fixnum (bbox-y1 new-bbox)))))
			  (and (eql (the fixnum (bbox-y2 old-bbox)) (the fixnum (bbox-y2 parent-bbox)))
			   (not (eql (the fixnum (bbox-y2 old-bbox)) (the fixnum (bbox-y2 new-bbox)))))))

                    ;; so, if parent-changed? is NIL, set it to T and store
                    ;; parent-old-bbox.  Then we finally cannot avoid the
                    ;; expensive operation of update-bbox (ack!)
                    (unless parent-changed?
                      (setq parent-changed? T)
                      (swap parent-old-bbox parent-bbox)
                      (setf (update-info-old-bbox parent-ui) parent-bbox))
                    (update-bbox parent parent-bbox))
		  )
		) ; close (if (null (bbox-valid-p parent-bbox)) ...)

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
		  (set-gc-clip-masks line-style-gc filling-style-gc :none)
		  (draw fastdraw-obj line-style-gc filling-style-gc
		      buffer root-window))
		(setf (update-info-on-fastdraw-list-p f-obj-update-info)
		      NIL))
	   (free-list fastdraw-objects)
	))
     ))  ;; matches (when (and window-agg visible)....)

   ; When using double-buffering, copy buffer into window.
   (when (and visible buffer)
     (if (or total-p (null win-new-bbox))
	 (xlib:copy-area buffer buffer-gc 0 0
			 (g-value a-window :width)
			 (g-value a-window :height)
			 drawable 0 0)
	 (progn
           (if win-new-bbox
             (merge-bbox newly-invisible-fastdraws-bbox win-new-bbox))
           (if win-old-bbox
              (merge-bbox newly-invisible-fastdraws-bbox win-old-bbox))
	   (when (bbox-valid-p newly-invisible-fastdraws-bbox)
             (copy-from-buffer-to-drawable newly-invisible-fastdraws-bbox
                                           buffer buffer-gc drawable)))))

   (setf (bbox-valid-p win-old-bbox) NIL)
   (setf (bbox-valid-p win-new-bbox) NIL)

   ))  ;; end of (when (or total-p partial-p) ...)

   (when (or total-p partial-p invalid-slots)
     (xlib:display-force-output (display-info-display (g-value a-window :display-info))))

   ; Recursively update children
   (let ((base-children (g-value a-window :child)))
    (if (and base-children
             (not (g-value a-window :exposed-bbox)))
     (do* ((children base-children    (rest children))
           (child    (first children) (first children)))
          ((null children))
       (unless (eq a-window (g-value child :parent))
	;; this code is for when a sub-window is re-parented, but then the
	;; old parent (ie, "a-window") is updated before the sub-window is.

	;; The pushnew of :parent makes sure the parent slot will be checked.
	;; The copy-list is needed because the ensuing update will
	;; destructively remove the sub-window from "a-window"'s :child list.
	(pushnew :parent (win-update-info-invalid-slots
	                      (g-value child :win-update-info)))
	(setq children (copy-list children))
	(unless (eq a-window (g-value child :old-parent))
	   (s-value a-window :child (delete child (g-value a-window :child)))))

     (update child total-p))))
    )))

    ;; Protect clause: release the process lock.
    (update-stop-fn a-window))

  ;; Mark that we are finished updating this window (it was set by update-all)
  (s-value a-window :in-progress NIL)
  )


;;; Gives rank of item at point <x,y>.
(define-method :point-to-rank opal:virtual-aggregate (gob x y)
  (let ((item-array (g-value gob :item-array))
        (point-in-item (g-value gob :point-in-item))
        item)
    (do ((rank (1- (g-value gob :next-available-rank)) (1- rank)))
        ((< rank 0) (return nil))
      (setq item (aref item-array rank))
      (when (and item (funcall point-in-item gob item x y))
        (return rank)))))



(define-method :point-to-component opal:virtual-aggregate (a-thing x y &key (type t))
  (when (or (eq type t)
            (opal::my-is-a-p (g-value a-thing :item-prototype) type))
    (let ((dummy (g-value a-thing :dummy-item))
          (rank (point-to-rank a-thing x y)))
      (when rank
        (s-value dummy :rank rank)
        (s-value dummy :item-values (aref (g-value a-thing :item-array) rank))
        dummy))))



(defmacro do-in-clip-rect ((m n agg rect) &body body)
  `(let* ((agg* ,agg)
	  (p-to-r (g-value agg* :point-to-rank))
	  (r* ,rect)
	  (array-size* (g-value agg* :array-length))
	  (max-x2* (1- (first array-size*)))
	  (max-y2* (1- (second array-size*)))
	  (first* (first r*))
	  (second* (second r*)))
     (multiple-value-bind (x1* y1*)
       		          (funcall p-to-r agg* first* second*)
       (multiple-value-bind (x2* y2*)
			    (funcall p-to-r agg* (+ first* (third r*) -1)
						 (+ second* (fourth r*) -1))
	 (setq x1* (if x1* (max 0 x1*) 0))
	 (setq y1* (if y1* (max 0 y1*) 0))
	 (setq x2* (if x2* (min x2* max-x2*) max-x2*))
	 (setq y2* (if y2* (min y2* max-y2*) max-y2*))
	 (when (and (<= x1* x2*) (<= y1* y2*))
	   (do ((,m x1* (1+ ,m)))
	       ((> ,m x2*))
	     (do ((,n y1* (1+ ,n)))
	         ((> ,n y2*))
	       ,@body)))))))



(defvar *virtual-line* nil)






(define-method :destroy-me opal:virtual-aggregate (gob &optional (top-level-p t))
  (call-prototype-method gob top-level-p))

