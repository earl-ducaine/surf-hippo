;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: OPAL; Base: 10 -*-

(in-package "OPAL")

;; Dec 14 2003 - Try to avoid too large bboxes (for zooming)
;; Not verified!

(defparameter smallest-SIGNED-BYTE-16 (- (expt 2 15)))
(defparameter largest-SIGNED-BYTE-16 (1- (expt 2 15)))

(defun LIMIT-NUM-TO-SIGNED-BYTE-16 (num)
  (when num
    (min (max smallest-SIGNED-BYTE-16 num) largest-SIGNED-BYTE-16)))

(define-method :update opal::window (a-window &optional (total-p NIL))
  (declare ; (optimize (speed 3) (safety 0))
   )
  (unwind-protect
    (#-apple progn
     #+apple ccl:without-interrupts
      (update-start-fn a-window)

 (let* ((win-info (g-local-value a-window :win-update-info))
	(drawable (g-local-value a-window :drawable))
	(window-agg (g-local-value a-window :aggregate))
	(invalid-slots (win-update-info-invalid-slots win-info))
	invalid-vobs
        ;; For validate? see ccl:validate-view below
        #+apple validate?)

   (unless drawable
     (setq drawable (install-drawable window-agg a-window win-info))
     (setq total-p T))

   (when invalid-slots
     (setq invalid-slots (fix-invalid-slots invalid-slots win-info a-window))
     #+apple
     (setq validate? (subsetp invalid-slots '(:left :top)))
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
                                  (bbox-valid-p win-old-bbox)))))

   (when visible
    (setf (win-update-info-invalid-objects        win-info) nil
	  (win-update-info-invalid-xor-fastdraws  win-info) nil
	  (win-update-info-invalid-copy-fastdraws win-info) nil))
  
  ;;; At this point, we try to abort if possible -- only do the main part
  ;;; of update if something really has changed...
  (when (or total-p partial-p)
    (let* ((win-new-bbox (win-update-info-new-bbox win-info))
	   (buffer (g-value a-window :buffer))
	   (display-info (g-value a-window :display-info))
	   (line-style-gc (display-info-line-style-gc display-info))
	   (filling-style-gc (display-info-filling-style-gc display-info))
	   fastdraw-objects
	   obj-update-slots-values
	   obj-update-info)
      ;; (sh::printvars win-new-bbox)
      (when win-new-bbox
	(setf (bbox-x1 win-new-bbox) (limit-num-to-signed-byte-16 (bbox-x1 win-new-bbox))
	    (bbox-x2 win-new-bbox) (limit-num-to-signed-byte-16 (bbox-x2 win-new-bbox))
	    (bbox-y1 win-new-bbox) (limit-num-to-signed-byte-16 (bbox-y1 win-new-bbox))
	    (bbox-y2 win-new-bbox) (limit-num-to-signed-byte-16 (bbox-y2 win-new-bbox))))
      ;; (sh::printvars win-new-bbox)
	    

  (if buffer
    (setf (bbox-valid-p newly-invisible-fastdraws-bbox) nil))

  (when (and window-agg visible)
    ;; gem::MAC-with-focused-view-or-gworld is defined in gem/define-methods.lisp
    (gem::MAC-with-focused-view-or-gworld (drawable buffer)
    (if total-p
      ;; This is a TOTAL window update.
      (progn
	(do-total-update invalid-objects invalid-xors invalid-copys a-window
			 window-agg buffer exposed-clip-mask
			 line-style-gc filling-style-gc))
      
      ;;else this is a PARTIAL window update.
      (do-partial-update invalid-objects invalid-xors invalid-copys a-window
			 window-agg buffer exposed-clip-mask
			 line-style-gc filling-style-gc obj-update-info
                          obj-update-slots-values win-info win-new-bbox
                          win-old-bbox fastdraw-objects)
     )))  ;; matches (when (and window-agg visible)....)

   ; When using double-buffering, copy buffer into window.

   (when (and visible buffer)
     (if (or total-p (null win-new-bbox))
       (gem:bit-blit a-window buffer
		     0 0 (g-value a-window :width) (g-value a-window :height)
		     drawable 0 0)
       (progn
	 (if win-new-bbox
	   (merge-bbox newly-invisible-fastdraws-bbox win-new-bbox))
	 (if win-old-bbox
	   (merge-bbox newly-invisible-fastdraws-bbox win-old-bbox))
	 (if (bbox-valid-p newly-invisible-fastdraws-bbox)
	   (copy-from-buffer-to-drawable a-window
					 newly-invisible-fastdraws-bbox
					 buffer drawable)))))

   (setf (bbox-valid-p win-old-bbox) NIL)
   (setf (bbox-valid-p win-new-bbox) NIL)

   ))  ;; end of (when (or total-p partial-p) ...)

   (when (or total-p partial-p invalid-slots)
     (gem:flush-output a-window))

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

       (update child total-p)

     )))

   ;; It would be nice to validate the view after we have drawn it, since this
   ;; would prevent exposure events from redundantly redrawing it (like when
   ;; you scroll a scrolling-window.  However, validating the view would also
   ;; prevent *necessary* exposure events from refreshing the window, like
   ;; when popup windows go away.  For example, if the view were unconditionally
   ;; validated, then the main window in the motif-menubar gadget demo would
   ;; not be redrawn properly.  Since we can identify a specific occasion on
   ;; which we want to prevent redundant redrawing (when scrolling-windows are
   ;; scrolled), let's just check whether the :left and/or :top of the window
   ;; was the only thing that changed and validate then.
   #+apple
   (if validate? (ccl:validate-view drawable))

    )))

   ;; Protect clause: release the process lock.
   (update-stop-fn a-window))

  ;; Mark that we are finished updating this window (it was set by update-all)
  (s-value a-window :in-progress NIL))


(defmacro validate-window-reference-slot (schema slot)
  `(if (not (opal::is-a-p (gv ,schema ,slot) opal::WINDOW))
       (setf  (gv ,schema ,slot) nil)
       (gv ,schema ,slot)))

(export '(validate-window-reference-slot))
