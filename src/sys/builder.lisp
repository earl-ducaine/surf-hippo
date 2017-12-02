;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: SURF; Base: 10 -*-

#|

====================================================================================================
			       The Surf-Hippo Neuron Simulator System
====================================================================================================

This code was written as part of the Surf-Hippo Project, originally at the Center for Biological
Information Processing, Department of Brain and Cognitive Sciences, Massachusetts Institute of
Technology, and currently at the Neurophysiology of Visual Compuation Laboratory, CNRS.
                                                                                 
Permission to use, copy, modify, and distribute this software and its documentation for any purpose
and without fee is hereby granted, provided that this software is cited in derived published work,
and the copyright notice appears in all copies and in supporting documentation. The Surf-Hippo
Project makes no representations about the suitability of this software for any purpose. It is
provided "as is" without express or implied warranty.
                                                                                 
If you are using this code or any part of Surf-Hippo, please contact surf-hippo@ai.mit.edu to be put
on the mailing list.
                                                                                 
Copyright (c) 1989 - 2003, Lyle J. Graham                                                                                              

|#


;;; SYS Source file: builder.lisp

;;; The builder file for tracing cells. Requires Garnet. Doesn't work now.

(in-package "SURF")


;;;; "The Dynamic Neuron Builder"
(defvar *segment-list* nil)
(defvar *neuron-builder-scale 1)
(defvar *delete nil)
(defvar *synapse nil)
(defvar *diameter nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *builder-window*)

(defun create-builder-window (&key width height)
  (let ((win (create-instance  nil histology-window
			       (:aggregate (create-instance nil opal:aggregate)
					   (:current-xfrm 
					    (let ((array (make-array (list 3 3) :initial-element 0.0)))
					      (loop for i from 0 to 2 do
						    (setf (aref array i i) 1.0))
					      array))
					   (:dummy-xfrm1 (make-array (list 3 3) :initial-element 0.0))
					   (:dummy-xfrm2 (make-array (list 3 3) :initial-element 0.0))
					   (:width (COERCE-TO-EVEN-INT width))
					   (:height (COERCE-TO-EVEN-INT height))))))
    (TRANSFER-CURRENT-XFRM-TO-XFRM-SLOTS win)
    (add-builder-interactors win)
    (s-value win :title "Dynamic Neuron Builder")
    (s-value win :current-proximal-node-xy '(0.0 0.0)) ; start xy, end xy
    (s-value win :current-proximal-segment nil)			
    (s-value win :visible t)
    (get-plot-agg win `histology)
    (opal:update win)
    (setq *builder-window* win)
    (push win *output-windows*)))



(defun builder-coords-pointer-result (interactor points-list)
  (let ((x-y (x-y-histology-win-inv (nth 2 points-list) (nth 3 points-list)
				    (g-value interactor :window)))
	(x-label "um")
	(y-label "um")
	*automatic-run*)
    (add-temp-comment
     (g-value interactor :window)
     (concatenate-strings
      (format nil "X: ~a ~a~%Y: ~a ~a~%"
	      (tidy-number-format (car x-y) :default-decimals 4) x-label
	      (tidy-number-format (cadr x-y) :default-decimals 4) y-label)
      (if (g-value interactor :last-xy)
	(format nil "      Length: ~a um"
		(tidy-number-format (sqrt
				     (+ (square (- (cadr x-y) (nth 1 (g-value interactor :last-xy))))
					(square (- (car x-y) (nth 0 (g-value interactor :last-xy))))))))
	)))
    (s-value interactor :last-xy (list (car x-y) (cadr x-y)))
    ))



(defun new-segment-pointer-result (interactor points-list)
  (defvars dummy0 dummy1 dummy2 dummy3 dummy4)
  (let* ((end-x-y (x-y-histology-win-inv (nth 2 points-list)
					 (nth 3 points-list)
					 (g-value interactor :window)))
	 (start-x-y
	  (g-value interactor :window :current-proximal-node-xy)
					;	  (x-y-histology-win-inv (nth 0 points-list)
					;					  (nth 1 points-list)
					;					  (g-value interactor :window))
	  )
	 (segment (add-line (car start-x-y)(cadr start-x-y)(car end-x-y)(cadr end-x-y)
			    (get-plot-agg  (g-value interactor :window) `histology)))
	 dummy0 dummy1 dummy2 dummy3 dummy4)
    (opal:update (g-value interactor :window))
    (add-comment (g-value interactor :window)
		 (format nil "Current Segment Start/End : [~A,~A] [~a,~A], length ~a um~%"
			 (car start-x-y)(cadr start-x-y)
			 (car end-x-y)(cadr end-x-y)
			 (tidy-number-format (cartesian-distance (car start-x-y)(cadr start-x-y)
								 (car end-x-y)(cadr end-x-y))
					     :default-decimals 4)))
    ;;    (if (not (g-value interactor :window :segment-list)))
    (setq dummy0 t
	  dummy1
	  (if (g-value interactor :window :current-segment)
	    (car (last (g-value interactor :window :current-segment)))
	    1.0)
	  dummy2 "End of latest segment" dummy3 "temp")
    (choose-variable-values
     '((dummy0  "Do you want to keep this segment" :boolean)
       (dummy1  "Diameter" :number)
       (dummy2 "Start next segment from:"
	       :choose ("Soma" "End of latest segment"))
       (dummy3 "Name" :string))
     ':label "Setting segment parameters")
    (if dummy0
      (progn
	(s-value (g-value interactor :window) :current-segment
		 (concatenate `list start-x-y end-x-y (list dummy1)))
	(s-value segment :line-thickness (round (/ dummy1 (g-value interactor :window :scale))))
	(s-value (g-value segment :line-style) :line-thickness (round (/ dummy1 (g-value interactor :window :scale))))
	(opal:update (g-value interactor :window) t)
	(s-value (g-value interactor :window) :segment-list
		 (cons (g-value interactor :window :current-segment)
		       (g-value interactor :window :segment-list))))

      (opal:destroy segment))
    (if (and dummy0 (string= dummy2  "End of latest segment"))
      (s-value (g-value interactor :window) :CURRENT-PROXIMAL-NODE-XY
	       end-x-y)
      (if  (string= dummy2  "Soma")
	(s-value (g-value interactor :window) :CURRENT-PROXIMAL-NODE-XY
		 (list 0.0 0.0))))

    ))



(defun new-segment-pointer-running-result (interactor points-list)
  (let ((end-x-y (x-y-histology-win-inv (nth 2 points-list)
					(nth 3 points-list)
					(g-value interactor :window)))

	(start-x-y
	 (g-value interactor :window :current-proximal-node-xy)
					;	  (x-y-histology-win-inv (nth 0 points-list)
					;					  (nth 1 points-list)
					;					  (g-value interactor :window))
	 ))
    (add-comment (g-value interactor :window)
		 (format nil "Current Segment Start/End : [~A,~A] [~a,~A], length ~a um~%"
			 (car start-x-y)(cadr start-x-y)
			 (car end-x-y)(cadr end-x-y)
			 (tidy-number-format (cartesian-distance (car start-x-y)(cadr start-x-y)
								 (car end-x-y)(cadr end-x-y))
					     :default-decimals 4)))))

(defun add-new-segment-pointer (win)
  ;;; select segment node
  ;;;
  ;;;
  ;;;
  (let ((new-segment-shadow (get-new-segment-shadow win)))
    (opal:add-component (g-value win :aggregate) new-segment-shadow)
    (create-instance nil  inter:two-point-Interactor
		     (:Window win)
		     (:start-where t)
		     (:line-p t)
		     ;; (:running-action #'new-segment-pointer-running-result)
		     (:last-xy nil)
		     (:start-event :control-leftdown)
		     (:feedback-obj new-segment-shadow)
		     (:final-function #'new-segment-pointer-result))))


(defun builder-menu-inter-function (interactor final-obj-over)
  (declare (ignore final-obj-over))
  (defvars dummy1) 
  (setq dummy1
	(if (equal (g-value interactor :window :CURRENT-PROXIMAL-NODE-XY)
		   (list 0.0 0.0))
	    "Soma"  "End of latest segment"))
  (let ((win (g-value interactor :window)))
    (choose-variable-values
     '((dummy1 "Start next segment from:"
	:choose ("Soma" "End of latest segment")))
     ':label "Setting segment parameters")
    (if (string= dummy1  "Soma")
	(s-value win :CURRENT-PROXIMAL-NODE-XY  (list 0.0 0.0)))))
		  
(create-instance 'builder-menu-Interactor inter:button-Interactor
		 (:continuous nil)
		 (:start-where t)
		 ;; LG fix 01.09.2016 CMUCL 20 reads :CONTROL-LATIN_CAPITAL_G instead of :CONTROL-G
		 ;; (:start-event :control-\m)
		 (:start-event '(:control-\m :CONTROL-LATIN_SMALL_LETTER_M))
		 (:final-function #'builder-menu-inter-function))

(defun add-builder-interactors (win)
  (add-new-segment-pointer win)
  (add-window-coords-pointer win #'builder-coords-pointer-result)
  (create-instance nil builder-menu-Interactor (:Window win)))


(defun get-new-segment-shadow (win)
  (create-instance
   NIL opal:line
   (:window win)
   (:fast-redraw-p T) (:draw-function :xor)
   (:x1 (o-formula (multiple-value-bind (x)
		       (x-y-histology-win (first (gvl :window :current-proximal-node-xy))
					  (second (gvl :window :current-proximal-node-xy))
					  (gvl :window))
		     x)))
   (:y1 (o-formula (multiple-value-bind (x)
		       (x-y-histology-win (first (gvl :window :current-proximal-node-xy))
					  (second (gvl :window :current-proximal-node-xy))
					  (gvl :window))
		     x)))
   (:x2 (o-formula (third (gvl :points))))
   (:y2 (o-formula (fourth (gvl :points))))
   (:points '(0 0 0 0))
   (:visible nil)
   (:line-style opal:dashed-line)))

;;;;;;;;;;;;;;;;;;;;;;;

(defun find-and-query-nearest-node (x y segment-list)
  (let ((min-distance)(distance)(nearest-segment))
    (loop for segment in segment-list do
	  (setq distance
		(sqrt (+ (square (- x (nth 2 segment)))(square (- y (nth 3 segment))))))
	  (if (not min-distance)
	      (setq min-distance distance nearest-segment segment)
	      (if (< distance min-distance)
		  (setq min-distance distance nearest-segment segment))))
    (if (y-or-n-p (format t "Do you want node ~a? " (nth 1 nearest-segment)))
	nearest-segment)))

;SCALE FACTOR SHOULD BE IN MICRONS PER PIXEL
#+symbolics
(defun segment-exists (segment-candidate-nodes segment-list)
  (let ((node1 (nth 0 segment-candidate-nodes))
	(node2 (nth 10 segment-candidate-nodes)))
    (dolist (segment segment-list)
      (if (or (and (eql (nth 0 segment) node1)(eql (nth 1 segment) node2))
	      (and (eql (nth 1 segment) node1)(eql (nth 0 segment) node2)))
	  (return segment)))))

(defun look-for-element-in-extras-list (extras-list element)
  (dolist (element-list extras-list)
    (if (equal (car element-list) element)
	(return t))))

#+symbolics
(defun edit-segment (edited-node segment-list cell-name)
  (setq *synapse (look-for-element-in-extras-list (nth 6 edited-node) 'synapse))
  (setq *diameter (nth 5 edited-node))
  (choose-variable-values
    '((*delete "Delete this node" :boolean)
      (*synapse "Add synapse to this node" :boolean)
      (*diameter "Diameter of this segment" :number))
    ':label (format nil "Edit Node ~a" (nth 1 edited-node)))
  (draw-cell-from-segment-list segment-list edited-node)
  (setq segment-list (remove edited-node segment-list :test #'equal))
  (if (not (and *delete
		(y-or-n-p (format nil "Are you sure you want to delete node ~a? " (nth 1 edited-node)))))
      (progn
	(if (= 7 (length edited-node))
	    (setq edited-node (butlast edited-node)))
	(setq edited-node
	      (nconc (butlast edited-node) (if *synapse (list *diameter (list (list 'synapse cell-name)))
					       (list *diameter))))
	(setq segment-list (nconc segment-list (list edited-node)))
	(draw-cell-from-segment-list segment-list edited-node)))
  segment-list)

#+symbolics
(defun edit-synapse-annulus (segment-list cell-name)
  (let ((inner-radius
	  (prompt-and-accept (list :type 'number :default 0) "Enter inner radius"))
	(outer-radius 
	  (prompt-and-accept (list :type 'number :default 0) "Enter outer radius"))
	(soma-location-x)(soma-location-y))
    (dolist (segment segment-list)
      (if (equal (nth 0 segment)(nth 1 segment))
	  (setq soma-location-x (nth 2 segment)
		soma-location-y (nth 3 segment))))
    (dolist (edited-node segment-list)
      (let ((distance
	      (/ (cartesian-distance soma-location-x soma-location-y
				     (nth 2 edited-node) (nth 3 edited-node))
		 *pixels-per-micron)))
	(draw-cell-from-segment-list segment-list edited-node)
	(setq segment-list (remove edited-node segment-list :test #'equal))
	(if (and (> distance inner-radius) (< distance outer-radius))
	    (if (= 6 (length edited-node))
		(setq edited-node (nconc edited-node (list (list (list 'synapse cell-name))))))
	    (if (= 7 (length edited-node))
		(setq edited-node (butlast edited-node))))
	(setq segment-list (nconc (list edited-node)  segment-list))
	(draw-cell-from-segment-list segment-list edited-node))))
  segment-list)

#+symbolics
(defun edit-distal-synapses (segment-list cell-name)
  (let ((distal-synapses (y-or-n-p "Do you want distal tip synapses? ")))
    (dolist (edited-node segment-list)
      (let ((is-distal t))
	(dolist (test-node segment-list)
	  (if (equal (nth 1 edited-node) (nth 0 test-node))
	      (return (setq is-distal nil))))
	(draw-cell-from-segment-list segment-list edited-node)
	(setq segment-list (remove edited-node segment-list :test #'equal))
	(cond ((and distal-synapses is-distal)
	       (if (= 6 (length edited-node))
		   (setq edited-node (nconc edited-node (list (list (list 'synapse cell-name)))))))
	      (t
	       (if (= 7 (length edited-node))
		   (setq edited-node (butlast edited-node)))))
	(setq segment-list (nconc (list edited-node)  segment-list))
	(draw-cell-from-segment-list segment-list edited-node))))
  segment-list)



#+symbolics
(defun mouse-find-node (segment-list)
  (let ((scale-left)(scale-right) 
	(segment-diameter-in-microns (if segment-list (nth 5 (car (last segment-list))) 1)))
    (multiple-value-bind (start-x start-y edited-node extras-flag)
	(dw:tracking-mouse ()
	  (:who-line-documentation-string
	    ()
	    (cond
 	      ((key-state :super) "s-Mouse-L to refresh display; s-Mouse-R for syn annulus.")
	      ((key-state :meta) "m-Mouse-L for scale 1;m-Mouse-R for scale 2;m-Mouse-M for scale distance.")
	      ((key-state :shift)
	       "sh-Mouse-L diameter, sh-Mouse-M to edit segment.")
	      (t (if segment-list
		     "Mouse-L: Find nearest node; Mouse-R to QUIT."
		     "Mouse-L: Put soma here; Mouse-R to QUIT."))))
	  (:mouse-click (click x y)
	   (graphics:untransform-window-points *standard-output* x y)
	   (cond ((eql click #\mouse-l)		;Put down starting point
		  (if segment-list 
		      (let ((segment (find-and-query-nearest-node x y segment-list)))
			(if segment (return (values (nth 2 segment)(nth 3 segment)(nth 1 segment)))))
		      (return (values x y nil))))
		 ((eql click #\m-mouse-l) (setq scale-left (cons x y)))
		 ((eql click #\m-mouse-r) (setq scale-right (cons x y)))
		 ((eql click #\m-mouse-m)
		  (if (and scale-left scale-right)
		      (progn
			(setq *pixels-per-micron
			      (/ (cartesian-distance
				   (car scale-left)(cdr scale-left)(car scale-right)(cdr scale-right))
				 (prompt-and-accept (list :type 'number)
						    "Enter scale bar distance in microns")))
			(return (values nil nil nil 'rescale)))))
		 ((eql click #\s-mouse-l)	;Rescale and redraw
		  (setq  *neuron-builder-scale
			 (prompt-and-accept
			   (list :type 'number :default *neuron-builder-scale) "Enter scale - "))
		  (return (values nil nil nil 'rescale)))
		 ((eql click #\sh-mouse-m)	;Edit segment
		  (let ((segment (find-and-query-nearest-node x y segment-list)))
		    (if segment (return (values nil nil segment)))))
		 ((eql click #\s-mouse-r)	;Add synapse annulus
		  (cond ((y-or-n-p "Edit synapse annulus? ")
			 (return (values nil nil nil 'edit-synapse-annulus)))
			((y-or-n-p "Edit distal synapses? ")
			 (return (values nil nil nil 'edit-distal-synapses)))))
		 ((eql click #\sh-mouse-l)
		  (setq segment-diameter-in-microns
			(prompt-and-accept
			  (list :type 'number :default segment-diameter-in-microns
				:stream *histology-note-pane) "Enter segment diameter in microns")))
		 ((eql click #\mouse-r)  (return)))))
      (let ((old-x nil)(old-y nil))
	(dw:with-output-recording-disabled ()
	  (if start-x
	      (dw:tracking-mouse ()
		(:who-line-documentation-string () "Put other end of line here.")
		(:mouse-motion (x y)
		 (graphics:untransform-window-points *standard-output* x y)
		 (when (and old-x old-y)
		   (graphics:draw-line start-x start-y old-x old-y :alu :flip
				       :thickness (minimum-plotted-segment-diameter segment-diameter-in-microns)))
		 (graphics:draw-line start-x start-y x y :alu :flip
				     :thickness (minimum-plotted-segment-diameter segment-diameter-in-microns))
		 (setq old-x x old-y y))
		(:mouse-click (click x y)
		 (graphics:untransform-window-points *standard-output* x y)
		 (ignore click)
;		 (unless (eql click #\mouse-l)
;		   (signal 'sys:abort))
		 (when (and old-x old-y)
		   (graphics:draw-line start-x start-y old-x old-y :alu :flip
				       :thickness (minimum-plotted-segment-diameter segment-diameter-in-microns)))
		 (return (values start-x start-y edited-node x y segment-diameter-in-microns nil))))
	      (if edited-node
		  (values nil nil edited-node nil nil nil NIL)
		  (values nil nil nil nil nil nil extras-flag))))))))

#+symbolics
(defun clear-and-select-window (&optional (window *standard-output*))
  (send window :clear-window)
  (send window :select))



(defun draw-cell-from-segment-list (segment-list &optional only-this-segment)
  (let ((proximal-segment)(proximal-node)
	(builder-agg (get-plot-agg *builder-window* `histology t)))
    (s-value  *builder-window* :segment-list segment-list)
    (dolist (segment segment-list)
      (setq proximal-node (nth 0 segment))
      (if (or (not only-this-segment) (eql only-this-segment segment))
	  (if (equal proximal-node (nth 1 segment)) ;we have soma
	      (add-circle
	       (nth 2 segment) (nth 3 segment)
	       (* *pixels-per-micron (* 0.5 (if (nth 5 segment)(nth 5 segment) 10)))
	       builder-agg)

	      (if (setq proximal-segment
			(dolist (segment segment-list)
			  (if (eql proximal-node (nth 1 segment)) (return segment))))
		  (progn
		    (add-line
		     (nth 2 proximal-segment) (nth 3 proximal-segment) (nth 2 segment) (nth 3 segment)
		     builder-agg :thickness (minimum-plotted-segment-diameter (if (nth 5 segment)(nth 5 segment) 1))
		     )
		    (if (look-for-element-in-extras-list (nth 6 segment) 'synapse)
			(add-circle  (nth 2 segment) (nth 3 segment) 6 builder-agg))
		    (add-string
		     (format nil "~a"  (nth 1 segment)) (nth 2 segment) (nth 3 segment) builder-agg
		     :character-style '(:fix :roman :very-small))))))))
  (opal:update *builder-window*)
  )

;;;  (with-graphics-translation (t 700 500)
;    (with-graphics-scale (t *pixels-per-micron)
;      (add-line 0.0 0.0  100.0 0.0 builder-agg :thickness 1) ;
;      (graphics:draw-line 0 -5 0 5 :thickness 1)
;      (graphics:draw-line 100 -5 100 5 :thickness 1)
;      (graphics:draw-string "100 microns"  30 -10  :character-style '(:fix :italic :small)))))

(defun name-already-used (name segment-list)
  (dolist (segment segment-list)
    (if (eql name (nth 1 segment)) (return t))))


(defun draw-some-lines (&optional (segment-list '()) cell-name &key (origin-x 0.0) (origin-y 0.0))
  (setq *neuron-builder-scale 1)
  (if segment-list
      (setq *pixels-per-micron
	    (prompt-and-read 'number "Enter pixels-per-micron - ")))
  (loop
   while 
   (with-graphics-translation (t origin-x origin-y)
     (if segment-list (draw-cell-from-segment-list segment-list))
     (let ((builder-agg (get-plot-agg *builder-window* `histology))
	   (new-name (top-node-number segment-list))
	   (last-name (if (not segment-list) 'soma)))
       (loop
	(multiple-value-bind (x1 y1 edited-node x2 y2 diameter extras-flag)
	    (mouse-find-node segment-list)
	  (cond  ((and extras-flag (not (equal extras-flag 'rescale)))
		  (setq segment-list (funcall extras-flag segment-list cell-name)))
		 ((and segment-list (not edited-node))
		  (return (equal extras-flag 'rescale)))
		 ((not x1)
		  (setq segment-list
			(edit-segment edited-node segment-list cell-name)))
		 (t
		  (add-line x1 y1 x2 y2 builder-agg :thickness (minimum-plotted-segment-diameter diameter))
		  (find-and-draw-soma segment-list)
		  (if edited-node (setq last-name edited-node))
		  (loop do (setq new-name (get-new-name new-name last-name))
			while (if (not new-name) t (name-already-used new-name segment-list)))
		  (add-string (format nil "~a" new-name) x2 y2 builder-agg
			      :character-style
			      '(:fix :roman :very-small) )
		  (setq segment-list
			(if segment-list
			    (nconc segment-list (list  (list last-name new-name x2 y2 0 diameter)))
			    (list (list last-name last-name x1 y1 0 10)
				  (list last-name new-name x2 y2 0 diameter))))))
	  (setq *segment-list* segment-list)))))))


(defun top-node-number (segment-list)
  (if segment-list
      (let ((top-node 0))
  	(dolist (segment segment-list)
	  (if (numberp (nth 1 segment))
	      (if (>  (nth 1 segment) top-node)
		  (setq  top-node (nth 1 segment)))))
	(if (> top-node 0)
	    top-node))))

#+symbolics
(defun find-and-draw-soma (segment-list)
  (if (= 2 (length segment-list))
      (graphics:draw-circle
	(nth 2 (car segment-list)) (nth 3 (car segment-list))
	(* *neuron-builder-scale *pixels-per-micron (* 0.5 (nth 5  (car segment-list)))))))
#+symbolics
(defun get-new-name (new-name last-name)
  (if (not
	(if (numberp new-name)
	    (if (y-or-n-p
		  (format *histology-note-pane
			  "Connect to node ~a - increment node ~a for new node? " last-name new-name))
		(setq new-name (1+ new-name)))))
      (setq new-name
	    (prompt-and-accept '(or number symbol) "Connect to node ~a - new node:" last-name)))  
  new-name)



;
;
;
;(create-instance 'moving-rectangle opal:rectangle
;		 (:box '(80 20 100 150))
;		 (:left (o-formula (first (gvl :box))))
;		 (:top (o-formula (second (gvl :box))))
;		 (:width (o-formula (third (gvl :box))))
;		 (:height (o-formula (fourth (gvl :box)))))
;
;(create-instance 'mywindow inter:interactor-window
;		 (:leftk 100) (:top 10)
;		 (:width 400) (:height 500)
;		 (:title "My Window")
;		 )
;
;(create-instance 'mymover inter:move-grow-interactor
;		 (:start-where (list :in moving-rectangle))
;		 (:window mywindow)
;		 (:running-where t))
;
;(s-value mywindow :aggregate (create-instance 'myagg opal:aggregate))
;
;(opal:add-component myagg moving-rectangle)
;(opal:update mywindow)
;
;(s-value mymover (:final-function 
;		  #'(lambda (grower obj final-points)
;		      (print (ps grower))
;		      (print (ps obj))
;		      (print final-points))))
