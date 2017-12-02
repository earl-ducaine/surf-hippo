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


;;; SYS Source file: cell-graphics-user-functions.lisp

;;; Some of the user graphics functions.

(IN-PACKAGE "SURF-HIPPO")

(defun just-draw (&key (win (opal-obj-exists *standard-graphics-output*))
		  mark-elements
		  mark-all-nodes
		  mark-plotted-nodes label-plotted-nodes
		  draw-light-stimulus
		  (motion-snapshots *motion-snapshots*)
		  (colorize (when win (gv win :colorize)))
		  (background-color (extract-color (when win (gv win :background-color)) *default-histology-window-background-color*))
		  (draw-all-synapse-rfs (when win (gv win :draw-all-synapse-rfs)))	; "Draw all synapse RFs"
		  (draw-synapse-rfs (when win (gv win :draw-synapse-rfs)))	; "Enable synapse RFs and connections"
		       
		  (scale (if win (gv win :scale) 3.0)) ; (microns/pixel)
		  (phi-deg (if (and win (gv win :phi-deg)) (gv win :phi-deg) 0.0))
		  (theta-deg (if (and win (gv win :theta-deg)) (gv win :theta-deg) 0.0))
		  (soma-outline-p t)
		  (soma-histology-fixed-diameter-p nil)
		  (soma-histology-fixed-diameter-pixels 10)
		  (DRAW-AXONS T)
		  (DRAW-SYNAPSE-CXNS T))
  "Draw the current circuit without invoking the Histology Menu. Arguments are the same as those for SET-MISC-HISTO-SLOTS."
  (process-circuit-structure)	   ; Just in case this is not done yet
  (when *circuit-loaded*
    (let ((*automatic-run* t)
	  (win (if (or (not win) *create-new-histology-window*)
		   (get-generic-histology-window)
		   win)))
      (SET-MISC-HISTO-SLOTS :win win :scale scale :phi-deg phi-deg :theta-deg theta-deg
			    :colorize colorize
			    :mark-plotted-nodes mark-plotted-nodes :label-plotted-nodes label-plotted-nodes
			    :background-color (get-opal-color background-color)
			    :draw-axons draw-axons :draw-synapse-cxns draw-synapse-cxns
			    :draw-all-synapse-rfs draw-all-synapse-rfs ; "Draw all synapse RFs"
			    :draw-synapse-rfs draw-synapse-rfs ; "Enable synapse RFs and connections"
			    :soma-outline-p soma-outline-p
			    :soma-histology-fixed-diameter-p soma-histology-fixed-diameter-p :soma-histology-fixed-diameter-pixels soma-histology-fixed-diameter-pixels)
      (s-value win :draw-light-stimulus draw-light-stimulus)
      (let ((*motion-snapshots* motion-snapshots))
	(when mark-all-nodes (s-value win :all-node-graphics '(:mark)))
	(drawing-menu win t *create-new-histology-window* t))
      (when mark-elements (mark-elements mark-elements :win win))
      win)))

(defun histo ()
  "Invoke the Histology Menu for drawing the circuit."
  (drawing-menu))

(defun mark-elements (&optional marked-elements &key (win *standard-graphics-output*) (key-diameter 15))
  "MARKED-ELEMENTS can be an atom or list, including 'CHANNEL, 'SYNAPSE, :ALL or specific cell elements, membrane elements or element
types. If a cell element is referenced, then all channel or synapses on that cell element will be marked. KEY-DIAMETER is in
pixels (default 15). Previously marked elements in the histology WIN will be erased."
  ;;  (declare (optimize (safety 0) (speed 3) (space 1)))
  (declare (ignore key-diameter))
  (when (opal-obj-exists win)
    (let* ((marked-element-types (s-value win :marked-element-types (loop for thing in (flatten-list marked-elements)
									  when (cell-element-p thing) collect (cell-element-elements thing) into out
									  else collect (cond ((element thing) (element thing))
											     ((member thing '(channel channels)) (channel-types))
											     ((member thing '(synapse synapses)) (synapse-types))
											     ((eq thing :all) (list (synapse-types)(channel-types))))
									  into out
									  finally (return (clean-up-list (flatten-list out))))))
	   (element-marker-agg (clear-and-add-plot-agg win `element-markers :add marked-element-types :clear t :where :front))
	   (*grape-size* (grape-size win)))
      (when (gv win :include-element-key-window) (element-key-window win ; :Key-diameter Key-diameter
								     ))
      (when marked-element-types
	(loop for cell-element in (cell-elements (gv win :cells)) do
	     (let ((node (cell-element-physical-node-fast cell-element)) node-marker-colors)
		(multiple-value-bind (x y)
		    (X-Y-HISTOLOGY-WIN-FROM-VIEW-VALUES (node-absolute-location node) win)
		  (when (x-y-in-win-values x y win)
		    (loop for elt in (node-elements node)
			  when
			  (and (not (and (synapse-p elt) (eq (synapse-pre-synaptic-node elt) node)))
			       (or (member elt marked-element-types)
				   (member (element-type elt) marked-element-types)))
			  do (push (get-marker-color elt) node-marker-colors))
		    ;; (format t "Ready to make grapes...~%")
		    (when node-marker-colors (opal:add-component element-marker-agg (make-grapes node-marker-colors win x y)))))))
	(histology-window-finishing win))
      (opal::update win t)
      nil)))

(defun color-cell-element (element &optional color)
  ;; Use element-parameter instead
  ;;  "Assigns a COLOR (including 'RED, 'GREEN, 'BLUE, 'YELLOW, 'ORANGE, 'CYAN, 'PURPLE, 'BLACK, 'WHITE) to the cell element or
  ;; elements associated with ELEMENT (can be an atom or list). To clear a color, COLOR should be NIL, or not supplied."
  (loop for cell-elt in (coerce-to-list (element element))
	when (cell-p cell-elt) do (color-cell-element (cell-elements cell-elt) color)
	else do (element-parameter (element-cell-element cell-elt) 'color (get-opal-color color)))
  nil)

(defun shade-cell-element (element &optional (shading 100))
  ;; Use element-parameter instead
  ;;  "Assigns a SHADING (percentage will be bounded between 0 and 100 percent) to the cell element or elements associated with
  ;; ELEMENT (can be an atom or list)."
  (loop for cell-elt in (coerce-to-list (element element))
	when (cell-p cell-elt) do (shade-cell-element (cell-elements cell-elt) shading)
	else do (element-parameter (element-cell-element cell-elt) 'shading (s-flt (bound-val shading 100 0))))
  nil)

(defun set-type-color (type color)
  ;; Use element-parameter instead
  ;;  "Sets the graphics color for elements of TYPE (name or object). Possible colors include: 'RED, 'GREEN, 'BLUE, 'YELLOW, 'ORANGE,
  ;;'CYAN, 'PURPLE, 'BLACK, 'WHITE"
  (element-parameter type 'color (get-opal-color color)))

(defun set-type-graphics (element value &optional (parameter 'color) (win *standard-graphics-output*))
  ;; Use element-parameter instead
  ;;  "Sets graphic qualities for type associated with ELEMENT (e.g. types of channels and synapses), and
  ;; specifically enables the graphics of the type, e.g.:
  ;;
  ;;     (set-type-graphics 'DR-HH 'cyan 'color)
  ;;
  ;;For colors, possible colors include: 'RED, 'GREEN, 'BLUE, 'YELLOW, 'ORANGE, 'CYAN, 'PURPLE, 'BLACK, 'WHITE.
  ;;Valid PARAMETER arguments include 'COLOR, and 'HEIGHT (for light synapse RF plotting)."
  (when (opal-obj-exists win)
    (loop for type in (coerce-to-list (element-type element)) do
	  (case parameter
	    (color (set-type-color type value))
	    (t
	     (setq *create-new-histology-window* nil *circuit-drawn* t) ; For using this during a circuit load.
	     (let* ((old-type-parameters (loop for type-info in (gv win :element-type-graphics-parameters)
					       when (eq type (cdr-assoc 'type type-info))
					       do (return type-info)))
		    (new-type-parameters
		     (cons (acons parameter value (remove (assoc parameter old-type-parameters) old-type-parameters))
			   (remove old-type-parameters (gv win :element-type-graphics-parameters)))))
	       (s-value win :element-type-graphics-parameters new-type-parameters)))))))
	    
(defun set-misc-histo-slots (&key (win *standard-graphics-output*)
			     (scale (if win (gv win :scale) 3.0))
			     (colorize (when win (gv win :colorize)))
			     mark-plotted-nodes label-plotted-nodes
			     (background-color (if win (gv win :background-color) opal::white))
			     (mark-all-synapses (when win (gv win :mark-all-synapses)))
			     (enable-marked-synapses (when win (gv win :enable-marked-synapses)))
			     (draw-all-synapse-rfs (when win (gv win :draw-all-synapse-rfs)))
			     (draw-synapse-rfs (when win (gv win :draw-synapse-rfs)))
		       
			     (phi-deg (if (and win (gv win :phi-deg)) (gv win :phi-deg) 0.0))
			     (theta-deg (if (and win (gv win :theta-deg)) (gv win :theta-deg) 0.0))
			     (soma-histology-fixed-diameter-p nil)
			     (soma-histology-fixed-diameter-pixels 10)
			     (soma-outline-p t) 
			     (DRAW-AXONS T)
			     (DRAW-SYNAPSE-CXNS T))	      
  "Set some basic graphics parameters without using the menus. e.g.

 (SET-MISC-HISTO-SLOTS :SCALE 3 :PHI-DEG 90 :DRAW-AXONS NIL)

Angle args PHI-DEG and THETA-DEG are in degrees, and SCALE arg is in microns/pixel."
  (unless (opal-obj-exists win) (setq win (get-generic-histology-window)))
  (setq *create-new-histology-window* nil *circuit-drawn* t) ; For using this during a circuit load.
  (s-value win :plotted-node-graphics (list (when mark-plotted-nodes :mark) (when  label-plotted-nodes :label)))
  (s-value win :colorize colorize)
  (s-value win :background-color (get-opal-color background-color))
  (s-value win :scale (float scale))
  (s-value win :phi (deg-to-rad phi-deg)) (s-value win :theta (deg-to-rad theta-deg))
  (s-value win :soma-outline-p soma-outline-p)
  (s-value win :soma-histology-fixed-diameter-p soma-histology-fixed-diameter-p)
  (s-value win :soma-histology-fixed-diameter-pixels (round soma-histology-fixed-diameter-pixels))
  (s-value win :draw-axons draw-axons)

  (let ((update-syn-graphics (or t
				 (xor (gv win :draw-synapse-cxns) draw-synapse-cxns)
				 (xor (gv win :mark-all-synapses) mark-all-synapses) ; "Mark all synapses"
				 (xor (gv win :enable-marked-synapses) enable-marked-synapses) ; "Enable synapse markers"
				 (xor (gv win :draw-all-synapse-rfs) draw-all-synapse-rfs)	; "Draw all synapse RFs"
				 (xor (gv win :draw-synapse-rfs) draw-synapse-rfs)	; "Enable synapse RFs and connections"
				 )))
    (s-value win :draw-synapse-cxns draw-synapse-cxns)
    (s-value win :mark-all-synapses mark-all-synapses) ; "Enable synapse markers"
    (s-value win :enable-marked-synapses enable-marked-synapses) ; "Mark all synapses"
    (s-value win :draw-all-synapse-rfs draw-all-synapse-rfs) ; "Draw all synapse RFs"
    (s-value win :draw-synapse-rfs draw-synapse-rfs) ; "Enable synapse RFs and connections"
    (s-value win :update-synapse-rfs update-syn-graphics)
    (s-value win :update-marked-synapses update-syn-graphics)))

(defun label-element (element &optional (win *standard-graphics-output*) (update t) label-agg)
  "Labels the cell element associated with ELEMENT in the histology WIN [default *STANDARD-GRAPHICS-OUTPUT*]."
  (when (opal-obj-exists win)
    (let* ((original-labeled-elements (gv win :labeled-elements))
	   (cell-elements (loop for elt in (coerce-to-list (element-cell-element element))
				unless (member elt original-labeled-elements)
				do (push elt (gv win :labeled-elements)) and collect elt))
	   (label-agg (when cell-elements (or label-agg (add-plot-agg win `node-labels :front)))))
      (when label-agg
	(mark-segments-and-somas
	 cell-elements
	 :win win
					;       :mark-agg label-agg
	 :label-agg label-agg
	 :type 'node-labels)
	(when update (histology-window-finishing win))
	label-agg))))

(defun mark-segments-and-somas (elements &key window-menu (win *standard-graphics-output*) (type 'marked-nodes) label-agg 
					 mark-agg (MARKER-DIAMETER (or (and win (gv win :marker-diameter)) 10))
					 (mark-somas t) (mark-fill opal:black-fill) (update t))
  "Add marker to the cell elements associated with ELEMENTS - in all cases, a marker and/or label will be drawn referring to the
underlying segment or soma. Markers are added to the histology in WIN, if supplied, otherwise if WINDOW-MENU is non-nil, then a
window selection menu is invoked, otherwise, the current value of *STANDARD-GRAPHICS-OUTPUT* is used."
  (declare (optimize (safety 0) (speed 3) (space 1)))
  (when (opal-obj-exists win)
    (when (and (or mark-agg label-agg) elements)
      (loop for win in (coerce-to-list (or win (if window-menu (win-menu) (opal-obj-exists *standard-graphics-output*)))) do
	    (when update (opal:update win))
	    (let ((out '())
		  (mark-agg (and mark-agg (typecase mark-agg
					    (schema mark-agg)
					    (t (clear-and-add-plot-agg win type :add t :where :front)))))
		  (label-agg (and label-agg (typecase label-agg
					      (schema label-agg)
					      (t (clear-and-add-plot-agg win type :add t :where :front))))))
	      (loop for element-comp in (coerce-to-list elements)
		    do (let* ((element (if (listp element-comp) (car element-comp) element-comp))
			      (end-location (if (and (listp element-comp) (nth 2 element-comp))
					      (nth 2 element-comp)
					      (element-absolute-location element)))
			      (x-y-2 (if (electrode-p element)
				       (electrode-sourcepoint win element)
				       (x-y-histology-win-from-view end-location win))))
			 (when (x-y-in-win x-y-2 win)
			   (when label-agg
			     (label-cell-node element label-agg :x-offset (or (gv win :label-x-offset) 0)
					      :explicit-node-xy-pixel-location (when (electrode-p element) (electrode-sourcepoint win element))))
			   (when (and mark-agg (or (not (soma-p element)) mark-somas))
			     (push (virtual-cell-element-marker-specs win element-comp element marker-diameter mark-fill x-y-2) out)))))
	      (when mark-agg (install-virtual-cell-element-markers win mark-agg out type)))
	    (when update (opal:update win))))))

(defun virtual-cell-element-marker-specs (win element-comp element marker-diameter mark-fill x-y-2)
  (let* ((diameter (or MARKER-DIAMETER (element-marker-diameter element win)))
	 (xcenter (the fn (car x-y-2)))
	 (ycenter (the fn (cadr x-y-2)))
	 (radius (the fn (round (* 0.5 diameter))))
	 (filling-style (case mark-fill (use-element-colors
					 (if (listp element-comp)
					   (colors-to-fill (nth 1 element-comp))
					   opal:black-fill))
			      (t mark-fill)))
	 (left (- xcenter radius))
	 (top (- ycenter radius))
	 (width diameter)
	 (height diameter))
    (make-virtual-circle-item-values-array left top width height filling-style nil xcenter ycenter)))

(defun install-virtual-cell-element-markers (win mark-agg item-list type) 
  (virtual-agg-finishing
   (make-v-agg (if (gv win :suppress-element-marker-borders) virtual-cell-element-marker-no-border virtual-cell-element-marker)
	       (list-to-array item-list)
	       type)
   mark-agg
   (gv win :where-element-markers-go)))

(defun spin-histology (&key (win *standard-graphics-output*) 
			 (delta-theta 5) (delta-phi 5) 
			 (rotations 2) (rotations-per-second 100))
  (when (opal-obj-exists win)
    (let ((*automatic-run* t)
	 )
      (s-value win :adjust-histology-window :fix)
      (loop for theta from 0.0 by delta-theta 
	   for phi from 0.0 by delta-phi
	   until (and rotations (>= (/ (max phi theta) 360) rotations))
	 do
	   (sleep (/ rotations-per-second))
	   ;; (SET-MISC-HISTO-SLOTS :win win :phi-deg (mod (/ theta 10) 360) :theta-deg (mod theta 360))
	   (SET-MISC-HISTO-SLOTS :win win :phi-deg (mod phi 360) :theta-deg (mod theta 360))
	    (drawing-menu win t nil t)))))



















