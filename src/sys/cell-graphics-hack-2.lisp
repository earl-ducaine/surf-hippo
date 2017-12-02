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


;;; SYS Source file: cell-graphics-hack-2.lisp

(IN-PACKAGE "SURF-HIPPO")

(defun drawing-menu (&optional (win (opal-obj-exists *standard-graphics-output*)) (parented *circuit-drawn*) (create-new-win *create-new-histology-window*) draw-cells-now)
  (clean-up-*output-windows*)
  (when (and *circuit-loaded*
	     (or (not win)
		 (not (gv win :locked))
		 (go-ahead-menu (format nil "Unlock window ~A" (gv win :title)) "Authorization" nil)))
    (process-circuit-structure)		; Just in case this is not done yet
    (when win (s-value win :locked nil))
    ;; These two forms shouldn't be necessary, but this is a cheap fix. 
    (unless (and (opal-obj-exists win) (not (gv win :auxiliary-type)))
      (setq win nil))
    (unless (and (opal-obj-exists *standard-graphics-output*) (not (gv *standard-graphics-output* :auxiliary-type)))
      (setq *standard-graphics-output* nil))
    (let* ((new-win (not win))
	   dummy28
	   (prototype (if (and *circuit-drawn* win)
			  win
			  (setq dummy28 t
				win (get-histology-window 'histology (find-histology-window-title)
							  :exclude-auxiliary-type :keys))))
	   (select-cell-menu-string (cond ((gv win :restrict-cells-to-cell-types)
					   (CONCATENATE-STRINGs (format nil "Select cell or cell types to draw~%")
								"(now showing types: "
								(CONCATENATE-STRING-LIST (gv win :cell-types) :string-count-to-add-linefeed 5 :string-spacer " ")
								")"))
					  ((and (gv win :cells) (not (= (hash-table-count (CELL-HASH-TABLE)) (length (gv win :cells)))))
					   (CONCATENATE-STRINGs (format nil "Select cell or cell types to draw~%")
								"(now showing cells: "
								(CONCATENATE-STRING-LIST (gv win :cells) :string-count-to-add-linefeed 5 :string-spacer " ")
								")"))
					  (t (format nil "Select cell or cell types to draw"))))
	   (dummy1 (s-flt (rad-to-deg (gv win :phi))))
	   (dummy2 (s-flt (rad-to-deg (gv win :theta))))
	   (dummy3 (case (gv win :adjust-histology-window)
		     (:automatic :automatic)
		     ((:fix :match :menu) :fix)))
	   (dummy4 *label-stimulus-times*)
	   (dummy5 *motion-snapshots*)
	   (dummy8 (gv win :scale))
	   (dummy9 (or (not (= (gv win :width) (gv win :last-drawn-width)))
		       (not (= (gv win :height) (gv win :last-drawn-height)))
		       draw-cells-now (not *circuit-drawn*) dummy28))
	   (dummy10 (when create-new-win :Create_new))
	   dummy11 dummy12 dummy15  dummy17
	   (dummy18 (and (or *enable-light* (are-there-light-synapses))
		     (gv win :draw-light-stimulus)))
	   dummy22 dummy24 (dummy25 100.0) (dummy26 t) dummy27 dummy28 dummy29
	   (menu-list `(,(if (gv win :valid-histology-p)
			     `(dummy10 "Action on histology window:" :choose (:Refresh :Raise :Create_new))
			     `(:comment "Window holds an invalid circuit, which cannot be redrawn"))
			 ,(when (and (gv win :valid-histology-p) (> (hash-table-count (CELL-HASH-TABLE)) 1)) `(dummy22 ,select-cell-menu-string :boolean))
			 (dummy8 "Drawing scale (microns/pixel)" :number)
			 (dummy2 ,(format nil "Viewing angle theta [degrees]~% Retina: 0/90 => flat/radial mount]") :float)
			 (dummy1 ,(format nil "Viewing angle phi [degrees]~%[Retina: 0 => flat & radial mount]") :float)
			 ,(when (gv win :valid-histology-p) `(dummy3 "Method to size histology window:" :choose (:fix :match :menu	:automatic) :rank-margin 4))
			 ,(when (or *enable-light* (are-there-light-synapses)) `(dummy18 ,(format nil "Draw light stimulus~% (=> Orient the view to stimulus plane)") :boolean))
			 (dummy17 "Edit histology rendering details" :boolean)
			 ,(when (loaded-sparse-data-p) `(dummy29 "Colorize and sparse data menu" :boolean)))))
      (when (gv win :valid-histology-p)
	(unless (gv win :cell-types) (s-value win :cell-types (cell-type-names)))
	(unless (gv win :cells) (s-value win :cells (cell-names))))
      (loop do (setq dummy29 nil
		     dummy17 nil)
	   (unless *automatic-run*
	     (choose-variable-values menu-list :label (if new-win "Setting Up Cell Drawing" (format nil "Drawing Menu for ~A" (gv win :title))) 
				     :title (unless new-win (format nil "Drawing Menu for ~A" (gv win :title)))))
	 ;; Avoid getting a new window if one was generated at the beginning of this function.
	   (when (gv win :valid-histology-p)
	     (when (and *circuit-drawn* (eq :Create_new dummy10))
	       (setq win (get-histology-child-window win parented :exclude-auxiliary-type :keys)))
	     (s-value win :update-synapse-stimulus (or (not (= dummy5 *motion-snapshots*))
						       (xor dummy4 *label-stimulus-times*)
						       (xor dummy18 (gv win :draw-light-stimulus))))
	     (s-value win :draw-light-stimulus dummy18)
	     (s-value win :raise (eq dummy10 :Raise))
	     (load-window-with-cells win dummy22)
	     (when (gv win :draw-light-stimulus) (case *light-stimulus-plane*
						     (:xy (setq dummy1 0.0 dummy2 0.0)) 
						     (:xz (setq dummy1 0.0 dummy2 90.0))))
	     (setq dummy9 (or dummy9
			      (gv win :update-synapse-stimulus)
			      (eq dummy10 :Refresh)
			      (gv win :complete-update)
			      (gv win :update-anatomy)
			      (not (= (gv win :width) (gv win :last-drawn-width)))
			      (not (= (gv win :height) (gv win :last-drawn-height)))
			      (not (= dummy1 (rad-to-deg (gv win :phi))))
			      (not (= dummy2 (rad-to-deg (gv win :theta))))
			      (not (equal dummy3 (gv win :adjust-histology-window)))
			      (equal dummy3 :menu) (equal dummy3 :match)
			      (not (= dummy8 (gv win :scale)))
			      dummy10))
	     (setq *create-new-histology-window* nil)
	     (s-value win :adjust-histology-window dummy3)
	     (s-value win :update-anatomy dummy9)
	     (s-value win :draw-anatomy t)
	     (s-value win :complete-update (or (gv win :complete-update) dummy9))
	     (when (equal dummy3 :match) (let ((dummy1 :none))
					   (choose-variable-values
					    `((dummy1 "Match window dimensions to another plot:" :choose (:Width_&_Height :Width :Height :none) :rank-margin 4))
					    :title "Editing Histology Graphics" :text (gv prototype :title))
					   (match-win-dimensions-menu prototype dummy1)))
	     (let ((x-shift (* -1 (gv prototype :scale) (aref (gv prototype :current-xfrm) 2 0))) ; x-shift
		   (y-shift (* -1 (gv prototype :scale) (aref (gv prototype :current-xfrm) 2 1))) ;  y-shift
		   (width (* (gv prototype :scale) (gv prototype :width)))
		   (height (* (gv prototype :scale) (gv prototype :height))))
	       (cond-every
		(dummy9 (set-histology-window-angle-scale-parameters win (deg-to-rad dummy2) (deg-to-rad dummy1) dummy8))
		(dummy9 (resize-histology-window win x-shift y-shift width height)))))
	   (when dummy17 (histology-rendering-menu win))
	   (when (gv win :valid-histology-p) (if (gv win :cells)
						 (progn (setq *circuit-drawn* (or *circuit-drawn* dummy9))
							(when (or dummy9 dummy17) (draw-cells win)))
						 (clear-window win))
		 (when dummy29 (colorize-and-sparse-data-menu win)))
	 while (or dummy17 dummy29)))
    win))

(defun draw-cells (&optional (win (opal-obj-exists *standard-graphics-output*)) complete-update)
  (when (gv win :valid-histology-p)
    (process-circuit-structure)	   ; Just in case this is not done yet
    ;;  (format t "complete-update ~A, (gv win :complete-update) ~A~%" complete-update (gv win :complete-update))
    (unless win
      (let ((title (concatenate-strings *simulation-name* ": Histology")))
	(setq win (get-histology-window 'histology title :exclude-auxiliary-type :keys))
	(ADD-TIME-COMMENT win)
	(s-value win :complete-update t)
	(load-window-with-cells win t)))
    (add-time-comment win)
    (when complete-update (s-value win :complete-update t))
    (let ((something-done (or (gv win :complete-update)
			      (gv win :update-axons) (gv win :update-synapse-cxns)
			      (gv win :update-marked-nodes) (gv win :update-marked-elements)
			      (gv win :update-synapse-rfs) (gv win :update-synapse-stimulus))))
      ;;    (format t "something-done ~A~%" something-done)
      (when (gv win :complete-update)
	(s-value win :update-anatomy t)
	(s-value win :update-axons t)
	(s-value win :update-synapse-cxns t)
	(s-value win :update-marked-nodes t)
	(s-value win :update-marked-elements t)
	(s-value win :update-element-key-window t)
	(s-value win :update-synapse-rfs t))
      (cond-every
       (something-done (add-temp-comment win "Redrawing..." :update nil))
     
       ((or (gv win :update-anatomy)
	    (gv win :complete-update)
	    (gv win :update-axons)
	    (gv win :update-synapse-cxns)
	    (gv win :update-marked-nodes)
	    (gv win :update-marked-elements)
	    (gv win :update-element-key-window)
	    (gv win :update-synapse-rfs)))
       (t 
	(colorizing-scale :remove (not (and (gv win :colorize) (gv win :include-colorizing-scale))))
	)
       ((or (gv win :update-element-key-window) (gv win :update-marked-elements) (gv win :complete-update))
	;; This is killing the segs/somas if called after draw-segments or draw-somas!
	(mark-elements (gv win :marked-element-types) :win win)
	(s-value win :update-element-key-window nil)
	(s-value win :update-marked-elements nil))
     
       ((or (gv win :update-anatomy) (gv win :complete-update))
	(s-value win :session-name *simulation-name*)
	(draw-extracellular-electrodes win (gv win :draw-extracellular-electrodes))
	;;      (frob-win-for-virtual-agg win)	; So that the virtual aggregate created for the virtual elements appears properly.
	(draw-somas win (and (gv win :draw-somas) (gv win :draw-anatomy)))
	;;      (frob-win-for-virtual-agg win)	; So that the virtual aggregate created for the virtual elements appears properly.
	(draw-segments win (gv win :draw-anatomy))
	)
       (nil				; *colorizing-simulation*
	(draw-chosen-one (or (gv win :chosen-ones) (gv win :chosen-one)) win))
       ((or (gv win :update-axons) (gv win :complete-update))
	(draw-axons win (gv win :draw-axons))
	(s-value win :update-axons nil))
       ((or (gv win :update-synapse-cxns) (gv win :complete-update))
	(draw-synapse-connections win (gv win :draw-synapse-cxns))
	(s-value win :update-synapse-cxns nil))
       ((or (gv win :update-electrodes) (gv win :update-anatomy) (gv win :complete-update))
	(draw-electrodes win (gv win :draw-electrodes))
	(s-value win :update-electrodes nil))
       ((or (gv win :update-sources) (gv win :update-anatomy) (gv win :complete-update))
	(draw-sources win (member :draw (gv win :source-graphics)))
	(s-value win :update-sources nil))
       ((or (gv win :update-marked-nodes) (gv win :update-anatomy) (gv win :complete-update))
	(mark-nodes win (member :mark (gv win :all-node-graphics)))
	;; fix this later
	(mark-segment-chains win t nil nil)
	(s-value win :update-anatomy nil))
       ((or (gv win :update-synapse-rfs) (gv win :complete-update))
	(draw-synapse-rfs win (gv win :draw-synapse-rfs))
	(s-value win :update-synapse-rfs nil))
       ((and (gv win :draw-light-stimulus) (or (gv win :update-synapse-stimulus) (gv win :complete-update)))
	(draw-light-stimulus win)
	(s-value win :update-synapse-stimulus nil))     
       ((or (gv win :update-plotted-nodes) (gv win :complete-update) (gv win :update-marked-nodes))
	(mark-plotted-nodes win (or (member :mark (gv win :plotted-node-graphics))
				    (member :label (gv win :plotted-node-graphics))
				    (member :label (gv win :all-node-graphics))
				    (member :mark (gv win :all-node-graphics))))
	(s-value win :update-marked-nodes nil)
	(s-value win :update-plotted-nodes nil)))
      (s-value win :complete-update nil)
      (unless nil			; *colorizing-simulation*
	(adjust-all-labels win)
	)
      (cond (something-done
	     (s-value win :last-drawn-width (gv win :width))
	     (s-value win :last-drawn-height (gv win :height))
	     (add-temp-comment win "" :update nil :resurrect *colorizing-simulation*)
	     )
	    ((gv win :raise)
	     (add-temp-comment win "" :update nil :resurrect t)
	     (s-value win :raise nil)))
      (REFRESH-MARKERs-POSITION win)
      )
    (histology-window-finishing win)
    (when (and (gv win :lock-color) (gv win :colorize) (gv win :displayed-time))
      (show-sparse-data :target-time (gv win :displayed-time) :window win))
    win))

(defun histology-window-finishing (win)
  (update-labeled-elements)
  (move-top-agg-components win 'marked-segments)
  (move-top-agg-components win 'node-labels)
  (move-top-agg-components win 'histology-scale-bar)
  (opal:update win))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Time comment for windows.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(create-instance 'time-comment WINDOW-COMMENT
		 (:visible nil)
		 (:parts
		  `((:background ,opal:rectangle
		     (:filling-style ,(o-formula (or (gvl :parent :window :default-graphics-background) opal::white-fill)))
		     (:left ,(o-formula (gvl :parent :left)))
		     (:Top ,(o-formula (gvl :parent :top)))
		     (:width ,(o-formula (+ (the fn (gvl :parent :label :width)) (* 2 *background-border-width*))))
		     (:height ,(o-formula (+ (the fn (gvl :parent :label :height)) (* 2 *background-border-width*))))
		     (:box '(0 0 0 0))
		     (:line-style nil))
		    (:frame ,opal:rectangle
		     (:left ,(o-formula (gvl :parent :background :left)))
		     (:top ,(o-formula (gvl :parent :background :top)))
		     (:width ,(o-formula (gvl :parent :background :width)))
		     (:height ,(o-formula (gvl :parent :background :height)))
		     (:visible nil))
		    (:label ,opal:cursor-multi-text ; opal:multifont-text
		     (:line-style ,(o-formula (gvl :parent :window :default-line-style)))
		     (:string ,(o-formula (if (gvl :parent :window :show-time)
					      (typecase (gvl :parent :window :displayed-time)
						(number (format nil "Time: ~,2fms" (or (gvl :parent :window :displayed-time) *real-time*)))
						(string (gvl :parent :window :displayed-time))
						(t ""))
					      "")))
		     (:left ,(o-formula (+ (the fn (gvl :parent :left)) *background-border-width*)))
		     (:top ,(o-formula (+ (the fn (gvl :parent :top)) *background-border-width*)))
		     (:font ,(o-formula (gvl :parent :window :font)))))))

(defun update-all-time-comments ()
  (loop for time-comment in (gv time-comment :is-a-inv) do (opal::recompute-formula (gv time-comment :label) :string)))

(defun remove-time-comment (&optional (win *standard-graphics-output*))
  (when (opal-obj-exists win)
    (let ((top-time-comment (GET-TOP-LEVEL-THING win time-comment)))
      (when top-time-comment
	(opal:remove-component (gv win :aggregate) top-time-comment)
	(opal:destroy top-time-comment)))))

(defun add-time-comment (&optional (win *standard-graphics-output*))
  (when (opal-obj-exists win)
    (add-temp-comment win "")
    (or (GET-TOP-LEVEL-THING win time-comment)
	(let* ((agg (get-agg win))
	       (new-time-comment (create-instance nil time-comment (:font (gv win :temp-comment-font)) (:position :upper-left))))
	  (opal:add-component agg new-time-comment)
	  (s-value new-time-comment :frame :visible nil)
	  (s-value new-time-comment :visible t)
	  (gv agg :left)
	  (s-value agg :left (loop for comp in (gv agg :components) minimizing (gv comp :left)))
	  (opal:update win t)
	  new-time-comment))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun load-window-with-cells (&optional (win *standard-graphics-output*) use-menu)
  (when (opal-obj-exists win)
    (let* ((old-cells (gv win :cells))
	   (dummy1 (gv win :restrict-cells-to-cell-types))
	   (dummy2 (if dummy1 :cell-types :cells))
	   dummy3)
      (when use-menu
	(choose-variable-values
	 '((dummy2 "Choose by:" :choose (:cell-types :cells))
	   (dummy3 "Clear or add all cells from/to windows" :choose (:clear :fill)))
	 :label "Criterium for displaying cells")
	(case dummy3
	  (:clear (s-value win :cells nil)
		  (s-value win :cell-types nil))
	  (:fill (s-value win :cells (cell-names))
		 (s-value win :cell-types (cell-type-names))))
	(s-value win :restrict-cells-to-cell-types (case dummy2 (:cell-types t) (t nil))))
      (s-value win :cells
	       (if (gv win :restrict-cells-to-cell-types)
		 (flatten-list
		  (loop for cell-type in
			(if use-menu
			  (s-value win :cell-types (select-hash-values-menu
						    (CELL-TYPE-HASH-TABLE) "Select cell types to draw" :selected-list (gv win :cell-types)))
			  (gv win :cell-types))
			collect (element-name (cell-type-cells cell-type))))
		 (if use-menu
		   (select-hash-values-menu (CELL-HASH-TABLE) "Select cells to draw" :continue-prompt nil :selected-list (gv win :cells))
		   (or old-cells (CELL-NAMES)))))
      (s-value win :update-anatomy (or (gv win :update-anatomy) (not (subsetp old-cells (gv win :cells))))))))

(defun min-max-xy-cell-element-in-win (win)
  (declare (optimize (safety 1) (speed 3) (space 1)))
  (let ((cells (elements (gv win :cells) 'cell)))
    (let (location
	  (vpx 0.0) (vpy 0.0))
      (declare (single-float vpx vpy))
      (loop for element in (concatenate 'list (extracellular-electrodes) (cell-elements cells))
	    do (setq location (if (extracellular-electrode-p element) (where element) (cell-element-absolute-location-fast element))
		     vpx (the sf (get-win-view-plane-x location win))
		     vpy (the sf (get-win-view-plane-y location win)))
	    minimize vpx into min-x single-float minimize vpy into min-y single-float maximize vpx into max-x single-float maximize vpy into max-y single-float
	    finally (return (list min-x min-y max-x max-y))))))

(defun min-max-xy-coordinates-in-win (win model-type)
  (declare (optimize (safety 0) (speed 3) (space 1)))
  (let ((vpx 0.0) (vpy 0.0)
	(locations (collect-locations-of-drawable-things (collect-drawable-things-w-connections win model-type))))
    (declare (single-float vpx vpy))
    (when locations
      (loop for location in locations
	    do (setq vpx (the sf (get-win-view-plane-x location win))
		     vpy (the sf (get-win-view-plane-y location win)))
	    minimize vpx into min-x single-float maximize vpx into max-x single-float minimize vpy into min-y single-float maximize vpy into max-y single-float 
	    finally (return (list min-x min-y max-x max-y))))))

(defun resize-histology-window (win &optional x-shift y-shift width height adjust-mode)
  (process-circuit-structure)		; Just in case.
  (let ((center-x (or x-shift (gv win :center-x)))
	(center-y (or y-shift (gv win :center-y)))
	(width (or width (gv win :width)))
	(height (or height (gv win :height)))
	(scale (gv win :scale)))
    (declare (special center-x center-y width height))
    (case (or adjust-mode (gv win :adjust-histology-window))
      (:automatic
       (loop for min-max-xy in (list (when (and (are-there-light-synapses) (is-light-moving) (gv win :draw-light-stimulus)) (draw-moving-stimulus nil nil t))
				     (min-max-xy-coordinates-in-win win 'axon)
				     (min-max-xy-coordinates-in-win win 'synapse)
				     (min-max-xy-cell-element-in-win win))
	  when min-max-xy minimizing (nth 0 min-max-xy) into min-x-all and minimizing (nth 1 min-max-xy) into min-y-all
	  and maximizing (nth 2 min-max-xy) into max-x-all and maximizing (nth 3 min-max-xy) into max-y-all
	  finally (when max-x-all
		    (setf center-x (* 0.5 (+ max-x-all min-x-all))
			  center-y (* 0.5 (+ max-y-all min-y-all))
			  ;; The factor of 1.2 to make the window a bit bigger than the cell(s).
			  width (* 1.3 (max *minimum-cell-histo-x-span* (- max-x-all min-x-all)))
			  height (+ (* *HISTOLOGY-WINDOW-MIN-HEIGHT-EXTRA* scale) ; a little extra height
				    (* 1.3 (max *minimum-cell-histo-y-span* (- max-y-all min-y-all))))))))	       
      (:menu
       (choose-variable-values
	'((center-x "Center of window along X direction [um]:" :number)
	  (center-y "Center of window along Y direction [um]:" :number)
	  (width "Histology window width [um]" :float)
	  (height "Histology window height [um]" :float))
	:label "Histology XY (center moves when cell is redrawn)")))
    (reset-histology-xfrm win center-x center-y width height scale)
    nil))

#|
(defun histology-rendering-menu (&optional (win *standard-graphics-output*))
  (when (opal-obj-exists win)
    (let (dummy1
	  dummy2 (dummy3 (princ-to-string (gv win :segment-default-color)))
	  dummy4 dummy5
	  (dummy6 (string-capitalize (OPAL-COLOR-TO-STRING (gv win :background-color))))
	  (dummy10 (gv win :source-graphics))
	  dummy11
	  (dummy13 (gv win :restrict-to-PROXIMAL-SEGMENT))
	  (dummy14 (gv win :draw-somas))
	  (dummy15 (gv win :where-somas-go))
	  (dummy16 (gv win :draw-anatomical-soma))
	  (dummy17 (gv win :where-segments-go))
	  (dummy18 (gv win :all-node-graphics))
	  (dummy19 (gv win :plotted-node-graphics))
	  (dummy20 (gv win :draw-electrodes))
	  (dummy21 (gv win :draw-extracellular-electrodes))
	  (dummy22 (gv win :soma-outline-p))
	  dummy23
	  (dummy24 (princ-to-string (gv win :segment-color-shading)))
	  (dummy25 (gv win :show-time))
	  menu-list)
      (unless dummy3 (setq dummy3 :black))
      (setq menu-list `((dummy18 "For all cell somas and segments:" :x-choose (:mark :label) :label-left)
			(dummy19 "For all plotted somas and segments:" :x-choose (:mark :label) :label-left)
			(dummy23 "Mark specific segments or disable/enable marking"  :boolean)
			(dummy13 "Draw only proximal segments" :boolean)
			(dummy14 "Draw somas" :boolean)
			(dummy15 "Position of somas relative to cell:" :choose (:front :back) :label-left)
			(dummy22 "Include outline for spherical somas" :boolean)
			,(when (electrodes) `(dummy20 "Draw electrodes" :boolean))
			,(when (extracellular-electrodes) `(dummy21 "Draw extracellular electrodes" :boolean))
			(*override-screen-max* "Override screen size limits on window" :boolean)
			;; (dummy17 "Position of segment relative to cell:" :choose (:front :back))
			(dummy6 "Background color:" :choose ("Black" "White") :label-left)
			(dummy3 "Default segment color:" :choose ,(mapcar 'princ-to-string *line-styles-colors*) :rank-margin 4)
			(dummy24 "Segment shading:" :choose ,(mapcar 'princ-to-string *line-styles-shadings*) :label-left :rank-margin 6)
			(dummy25 "Display simulation time" :boolean)
			(:comment )
			(dummy11 "Modify other element graphics" :boolean)))
      (cond-every
       ((loop for cell in (element (gv win :cells) 'cell)
	      thereis (or (element-parameter (cell-soma cell) 'soma-outline) (element-parameter (cell-soma cell) 'soma-points)))
	(push '(dummy16 "Draw anatomical soma(s) rendering [else circuit sphere]" :boolean) menu-list))
       ((are-there-axons) (push '(dummy4 "Axon graphics menu" :boolean) menu-list))
       ((are-there-voltage-synapses) (push '(dummy5 "Synapse connection graphics menu" :boolean) menu-list))
       ((are-there-sources) (push '(dummy10 "Current or voltage sources:" :x-choose (:Draw :label) :label-left) menu-list))
       ((are-there-channels) (push '(dummy2 "Channel graphics menu" :boolean) menu-list))
       ((are-there-synapses) (push '(dummy1 "Synapse graphics menu" ; "Synapse location, RF graphics menu"
					    :boolean) menu-list)))
      (choose-variable-values menu-list :label "Details of Cell Drawing")

      (let ((background-color (string-to-opal-color dummy6)))
	;; (format t "background-color ~A~%" background-color)
	(s-value win :show-time dummy25)
	(cond-every
	 (dummy4 (axon-graphics-menu win))
	 (dummy5 (synapse-cxn-graphics-menu win))
	 (dummy1 (synapse-graphics-menu win))
	 (dummy2 (channel-marking-graphics-menu win))
	 (dummy11 (element-graphics-menu win))
	 (dummy23 (mark-segment-chains-menu win)))
	(s-value win :update-marked-nodes (or (gv win :update-marked-nodes) dummy23 (not (equal (gv win :all-node-graphics) dummy18))))
	(s-value win :update-plotted-nodes (or (gv win :update-plotted-nodes) (gv win :update-marked-nodes)
					       (not (equal dummy19 (gv win :plotted-node-graphics)))))
	(s-value win :update-electrodes (or (gv win :update-electrodes) (not (equal dummy20 (gv win :draw-electrodes)))))
	(s-value win :update-sources (or (gv win :update-sources) (not (equal dummy10 (gv win :source-graphics)))))
	(s-value win :update-anatomy (or (gv win :update-anatomy)
					 (not (= (read-from-string dummy24) (gv win :segment-color-shading)))
					 (not (equal (gv win :background-color) background-color))
					 ;; (not (equal dummy24 (gv win :include-colorizing-scale)))
					 (not (equal dummy21 (gv win :draw-extracellular-electrodes)))
					 (not (equal dummy16 (gv win :draw-anatomical-soma)))
					 (not (equal dummy22 (gv win :soma-outline-p)))
					 (not (equal (read-from-string dummy3) (gv win :segment-default-color)))
					 (not (equal dummy13 (gv win :restrict-to-PROXIMAL-SEGMENT)))
					 (xor dummy14 (gv win :draw-somas))
					 (not (equal (gv win :where-segments-go) dummy17))
					 (not (equal dummy15 (gv win :where-somas-go)))))
	(s-value win :segment-color-shading (s-flt (read-from-string dummy24)))
	(s-value win :background-color background-color)
	;; (s-value win :default-graphics-color (default-graphics-color win))
	(when (s-value win :restrict-to-PROXIMAL-SEGMENT dummy13)
	  (setq dummy13 (if (numberp (gv win :PROXIMAL-SEGMENT-level)) (gv win :PROXIMAL-SEGMENT-level)	1))
	  (choose-variable-values '((dummy13 "How many levels of proximal segments:" :number)) :label "Draw only proximal segments")
	  (s-value win :proximal-segment-level dummy13))
	(s-value win :segment-default-color (read-from-string dummy3))
	(s-value win :source-graphics dummy10)
	(s-value win :where-segments-go dummy17)
	(s-value win :draw-anatomical-soma dummy16)
	(s-value win :where-somas-go dummy15)
	(s-value win :soma-outline-p dummy22)
	(s-value win :draw-somas dummy14)
	(s-value win :draw-electrodes dummy20)
	(s-value win :draw-extracellular-electrodes dummy21)
	(s-value win :all-node-graphics dummy18) 
	(s-value win :plotted-node-graphics dummy19)
	))))
|#

(defun histology-rendering-menu (&optional (win *standard-graphics-output*))
  (when (opal-obj-exists win)
    (let ((segment-default-color (princ-to-string (gv win :segment-default-color)))
	  (background-color (string-capitalize (OPAL-COLOR-TO-STRING (gv win :background-color))))
	  (source-graphics (gv win :source-graphics))
	  (restrict-to-proximal-segment (gv win :restrict-to-PROXIMAL-SEGMENT))
	  (draw-somas (gv win :draw-somas))
	  (where-somas-go (gv win :where-somas-go))
	  (draw-anatomical-somas (gv win :draw-anatomical-soma))
	  (where-segments-go (gv win :where-segments-go))
	  (all-node-graphics (gv win :all-node-graphics))
	  (plotted-node-graphics (gv win :plotted-node-graphics))
	  (draw-electrodes (gv win :draw-electrodes))
	  (draw-extracellular-electrodes (gv win :draw-extracellular-electrodes))
	  (soma-outline-p (gv win :soma-outline-p))
	  (segment-color-shading (princ-to-string (gv win :segment-color-shading)))
	  (show-time (gv win :show-time))
	  mark-specific-segs-or-disable/enable-marking modify-other-graphics synapse-graphics-menu-p channel-marking-graphics-menu-p axon-graphics-menu-p synapse-cxn-graphics-menu-p
	  menu-list)
      (declare (special SYNAPSE-GRAPHICS-MENU-P CHANNEL-MARKING-GRAPHICS-MENU-P SEGMENT-DEFAULT-COLOR AXON-GRAPHICS-MENU-P SYNAPSE-CXN-GRAPHICS-MENU-P BACKGROUND-COLOR
			SOURCE-GRAPHICS MODIFY-OTHER-GRAPHICS RESTRICT-TO-PROXIMAL-SEGMENT DRAW-SOMAS WHERE-SOMAS-GO DRAW-ANATOMICAL-SOMAS WHERE-SEGMENTS-GO ALL-NODE-GRAPHICS
			PLOTTED-NODE-GRAPHICS DRAW-ELECTRODES DRAW-EXTRACELLULAR-ELECTRODES SOMA-OUTLINE-P MARK-SPECIFIC-SEGS-OR-DISABLE/ENABLE-MARKING
			SEGMENT-COLOR-SHADING SHOW-TIME))
      (unless segment-default-color (setq segment-default-color :black))
      (setq menu-list `((all-node-graphics "For all cell somas and segments:" :x-choose (:mark :label) :label-left)
			(plotted-node-graphics "For all plotted somas and segments:" :x-choose (:mark :label) :label-left)
			(mark-specific-segs-or-disable/enable-marking "Mark specific segments or disable/enable marking"  :boolean)
			(restrict-to-proximal-segment "Draw only proximal segments" :boolean)
			(draw-somas "Draw somas" :boolean)
			(where-somas-go "Position of somas relative to cell:" :choose (:front :back) :label-left)
			(soma-outline-p "Include outline for spherical somas" :boolean)
			,(when (electrodes) `(draw-electrodes "Draw electrodes" :boolean))
			,(when (extracellular-electrodes) `(draw-extracellular-electrodes "Draw extracellular electrodes" :boolean))
			(*override-screen-max* "Override screen size limits on window" :boolean)
			;; (where-segments-go "Position of segment relative to cell:" :choose (:front :back))
			(background-color "Background color:" :choose ("Black" "White") :label-left)
			(segment-default-color "Default segment color:" :choose ,(mapcar 'princ-to-string *line-styles-colors*) :rank-margin 4)
			(segment-color-shading "Segment shading:" :choose ,(mapcar 'princ-to-string *line-styles-shadings*) :label-left :rank-margin 6)
			(show-time "Display simulation time" :boolean)
			(:comment )
			(modify-other-graphics "Modify other element graphics" :boolean)))
      (cond-every
       ((loop for cell in (element (gv win :cells) 'cell) thereis (or (element-parameter (cell-soma cell) 'soma-outline) (element-parameter (cell-soma cell) 'soma-points)))
	(push '(draw-anatomical-somas "Draw anatomical soma(s) rendering [else circuit sphere]" :boolean) menu-list))
       ((are-there-axons) (push '(axon-graphics-menu-p "Axon graphics menu" :boolean) menu-list))
       ((are-there-voltage-synapses) (push '(synapse-cxn-graphics-menu-p "Synapse connection graphics menu" :boolean) menu-list))
       ((are-there-sources) (push '(source-graphics "Current or voltage sources:" :x-choose (:Draw :label) :label-left) menu-list))
       ((are-there-channels) (push '(channel-marking-graphics-menu-p "Channel graphics menu" :boolean) menu-list))
       ((are-there-synapses) (push '(synapse-graphics-menu-p "Synapse graphics menu" ; "Synapse location, RF graphics menu"
					    :boolean) menu-list)))
      (choose-variable-values menu-list :label "Details of Cell Drawing")
      (let ((background-color (string-to-opal-color background-color)))
	(s-value win :show-time show-time)
	(cond-every
	 (axon-graphics-menu-p (axon-graphics-menu win))
	 (synapse-cxn-graphics-menu-p (synapse-cxn-graphics-menu win))
	 (synapse-graphics-menu-p (synapse-graphics-menu win))
	 (channel-marking-graphics-menu-p (channel-marking-graphics-menu win))
	 (modify-other-graphics (element-graphics-menu win))
	 (mark-specific-segs-or-disable/enable-marking (mark-segment-chains-menu win)))
	(s-value win :update-marked-nodes (or (gv win :update-marked-nodes) mark-specific-segs-or-disable/enable-marking (not (equal (gv win :all-node-graphics) all-node-graphics))))
	(s-value win :update-plotted-nodes (or (gv win :update-plotted-nodes) (gv win :update-marked-nodes)
					       (not (equal plotted-node-graphics (gv win :plotted-node-graphics)))))
	(s-value win :update-electrodes (or (gv win :update-electrodes) (not (equal draw-electrodes (gv win :draw-electrodes)))))
	(s-value win :update-sources (or (gv win :update-sources) (not (equal source-graphics (gv win :source-graphics)))))
	(s-value win :update-anatomy (or (gv win :update-anatomy)
					 (not (= (read-from-string segment-color-shading) (gv win :segment-color-shading)))
					 (not (equal (gv win :background-color) background-color))
					 ;; (not (equal segment-color-shading (gv win :include-colorizing-scale)))
					 (not (equal draw-extracellular-electrodes (gv win :draw-extracellular-electrodes)))
					 (not (equal draw-anatomical-somas (gv win :draw-anatomical-soma)))
					 (not (equal soma-outline-p (gv win :soma-outline-p)))
					 (not (equal (read-from-string segment-default-color) (gv win :segment-default-color)))
					 (not (equal restrict-to-proximal-segment (gv win :restrict-to-PROXIMAL-SEGMENT)))
					 (xor draw-somas (gv win :draw-somas))
					 (not (equal (gv win :where-segments-go) where-segments-go))
					 (not (equal where-somas-go (gv win :where-somas-go)))))
	(s-value win :segment-color-shading (s-flt (read-from-string segment-color-shading)))
	(s-value win :background-color background-color)
	;; (s-value win :default-graphics-color (default-graphics-color win))
	(when (s-value win :restrict-to-PROXIMAL-SEGMENT restrict-to-proximal-segment)
	  (setq restrict-to-proximal-segment (if (numberp (gv win :PROXIMAL-SEGMENT-level)) (gv win :PROXIMAL-SEGMENT-level)	1))
	  (choose-variable-values '((restrict-to-proximal-segment "How many levels of proximal segments:" :number)) :label "Draw only proximal segments")
	  (s-value win :proximal-segment-level restrict-to-proximal-segment))
	(s-value win :segment-default-color (read-from-string segment-default-color))
	(s-value win :source-graphics source-graphics)
	(s-value win :where-segments-go where-segments-go)
	(s-value win :draw-anatomical-soma draw-anatomical-somas)
	(s-value win :where-somas-go where-somas-go)
	(s-value win :soma-outline-p soma-outline-p)
	(s-value win :draw-somas draw-somas)
	(s-value win :draw-electrodes draw-electrodes)
	(s-value win :draw-extracellular-electrodes draw-extracellular-electrodes)
	(s-value win :all-node-graphics all-node-graphics) 
	(s-value win :plotted-node-graphics plotted-node-graphics)
	))))

(defun axon-graphics-menu (&optional (win *standard-graphics-output*))
  (when (opal-obj-exists win)
    (let* (dummy1
	   (dummy2 (gv win :axon-color-from-synapse))
	   (dummy3 *axon-graphics-diameter*)
	   (dummy4 (gv win :use-connection-midpoints))
	   (dummy16 (gv win :where-axons-go))
	   (dummy20 (princ-to-string (gv win :axon-color)))
	   (dummy21 (princ-to-string (gv win :axon-color-shading)))
	   (dummy24 (gv win :restrict-axons-to-cells))
	   (dummy25 (> (length (gv win :restrict-axons-to-synapse-types)) 0))
	   (dummy28 (gv win :draw-axons)))
      (choose-variable-values
       `((dummy28 "Draw axons" :boolean)
	 (dummy3 "Axon graphics diameter [um]" :float)
	 (dummy16 "Position of axons relative to cell -" :choose (:front :back))
	 (dummy4 "Draw with midpoints" :boolean)
	 (dummy2 "Get axon color from target synapse" :boolean)
	 (dummy20 "Axon default color" :choose ,(mapcar 'princ-to-string *line-styles-colors*) :rank-margin 4)
	 (dummy21 "Axon shading" :choose ,(mapcar 'princ-to-string *line-styles-shadings*) :rank-margin 6)
	 (dummy24 "Restrict axons to drawn cells" :boolean)
	 (dummy25 "Restrict axons to synapse types" :boolean)
	 (dummy1 "Edit viewed target synapse types" :boolean))
       :label "Setting Up Axon Graphics")
      (when dummy1
	(let ((synapse-types (elements (select-hash-values-menu (SYNAPSE-TYPE-HASH-TABLE)
								"Select types of target synapses for plotted axons"
								:inclusion-key #'(lambda (type)
										   (case (synapse-type-control type)
										     ((:voltage :channel) t)))
								:selected-list (element-names (gv win :restrict-axons-to-synapse-types))))))
	  (s-value win :update-axons (or (gv win :update-axons) (not (equal synapse-types (gv win :restrict-axons-to-synapse-types)))))
	  (s-value win :restrict-axons-to-synapse-types synapse-types)))
      (s-value win :update-axons
	       (or (gv win :update-axons)
		   (xor dummy4 (gv win :use-connection-midpoints))
		   (not (= (read-from-string dummy21)  (gv win :axon-color-shading)))
		   (not (= dummy3 *axon-graphics-diameter*))
		   (xor (gv win :axon-color-from-synapse) dummy2)
		   (xor dummy28 (gv win :draw-axons))
		   (not (equal (gv win :where-axons-go) dummy16))
		   (not (equal (read-from-string dummy20) (gv win :axon-color)))
		   (xor dummy24 (gv win :restrict-axons-to-cells))))
      (s-value win :axon-color-shading (read-from-string dummy21))
      (setq *axon-graphics-diameter* dummy3)
      (s-value win :use-connection-midpoints dummy4)
      (s-value win :axon-color-from-synapse dummy2)
      (s-value win :draw-axons dummy28)
      (s-value win :restrict-axons-to-cells dummy24)
      (s-value win :where-axons-go dummy16)
      (s-value win :axon-color (read-from-string dummy20)))))

(defun synapse-cxn-graphics-menu (&optional (win *standard-graphics-output*))
  (when (opal-obj-exists win)
    (let* (dummy1
	   (dummy2 (gv win :synapse-cxn-color-from-synapse))
	   (dummy3 *synapse-cxn-graphics-diameter*)
	   (dummy4 (gv win :use-connection-midpoints))
	   (dummy16 (gv win :where-synapse-cxns-go))
	   (dummy20 (princ-to-string (gv win :synapse-cxn-color)))
	   (dummy21 (princ-to-string (gv win :synapse-cxn-color-shading)))
	   (dummy24 (gv win :restrict-synapse-cxns-to-cells))
	   (dummy25 (> (length (gv win :restrict-synapse-cxns-to-synapse-types)) 0))
	   (dummy28 (gv win :draw-synapse-cxns)))
      (choose-variable-values
       `((dummy28 "Draw synapse connections" :boolean)
	 (dummy3 "Synapse connection graphics diameter [um]" :float)
	 (dummy4 "Draw with midpoints" :boolean)
	 (dummy16 "Position of synapse connections relative to cell -" :choose (:front :back))
	 (dummy2 "Get synapse-cxn color from target synapse" :boolean)
	 (dummy20 "Synapse-Cxn default color" :choose ,(mapcar 'princ-to-string *line-styles-colors*) :rank-margin 4)
	 (dummy21 "Synapse-Cxn shading" :choose ,(mapcar 'princ-to-string *line-styles-shadings*) :rank-margin 6)
	 (dummy24 "Restrict synapse connections to drawn cells" :boolean)
	 (dummy25 "Restrict synapse connections to synapse types" :boolean)
	 (dummy1 "Edit viewed target synapse types" :boolean))
       :label "Setting Up Synapse-Cxn Graphics")
      (when dummy1
	(let ((synapse-types
	       (loop for name in 
		     (select-hash-values-menu (SYNAPSE-TYPE-HASH-TABLE)
					      "Select types of target synapses for plotted synapse-cxns"
					      :inclusion-key #'(lambda (type)
								 (case (synapse-type-control type)
								   ((:voltage :channel) t)))
					      :selected-list (element-names (gv win :restrict-synapse-cxns-to-synapse-types)))
		     collect (element name))))
	  (s-value win :update-synapse-cxns (or (gv win :update-synapse-cxns) (not (equal synapse-types (gv win :restrict-synapse-cxns-to-synapse-types)))))
	  (s-value win :restrict-synapse-cxns-to-synapse-types synapse-types)))
      (s-value win :update-synapse-cxns (or (gv win :update-synapse-cxns)
					    (xor dummy4 (gv win :use-connection-midpoints))
					    (and (numberp (gv win :synapse-cxn-color-shading))
						 (numberp (read-from-string dummy21))
						 (not (= (read-from-string dummy21) (gv win :synapse-cxn-color-shading))))
					    (not (= dummy3 *synapse-cxn-graphics-diameter*))
					    (xor (gv win :synapse-cxn-color-from-synapse) dummy2)
					    (xor dummy28 (gv win :draw-synapse-cxns))
					    (not (equal (gv win :where-synapse-cxns-go) dummy16))
					    (not (equal (read-from-string dummy20) (gv win :synapse-cxn-color)))
					    (xor dummy24 (gv win :restrict-synapse-cxns-to-cells))))
      (s-value win :synapse-cxn-color-shading (read-from-string dummy21))
      (setq *synapse-cxn-graphics-diameter* dummy3)
      (s-value win :use-connection-midpoints dummy4)
      (s-value win :synapse-cxn-color-from-synapse dummy2)
      (s-value win :draw-synapse-cxns dummy28)
      (s-value win :restrict-synapse-cxns-to-cells dummy24)
      (s-value win :where-synapse-cxns-go dummy16)
      (s-value win :synapse-cxn-color (read-from-string dummy20))))) 

#|
(defun synapse-graphics-menu (&optional (win *standard-graphics-output*))
  (let (reset-draw-all-synapse-rfs-connections
	reset-label-all-synapses reset-draw-all-synapse-rfs update-flag original-label-flag
	(dummy1 (gv win :mark-all-synapses)) (dummy2 (gv win :enable-marked-synapses))
	dummy3
	(dummy4 *syn-rf-connection-thickness*) (dummy5 *syn-rf-connection-dash*)
	dummy6 dummy7
	(dummy8 (or (gv win :where-synapse-stimulus-goes) *WHERE-SYNAPSE-STIMULUS-GOES*))
	dummy10 dummy11
	(dummy12 (gv win :draw-all-synapse-rfs))
	(dummy13 (or (gv win :synapse-rf-height) 100.0))
	(dummy14 (gv win :draw-all-synapse-rfs-connections)) (dummy16 (gv win :use-same-synapse-rfs-height))
	dummy17 dummy18 (dummy19 *motion-snapshots*) (dummy20 (gv win :draw-synapse-rfs)) (dummy21 *label-stimulus-times*))
    (choose-variable-values
     `((dummy2 "Enable synapse markers" :boolean)
       ,(if dummy1 `(dummy3 "Clear all synapse markers" :boolean) `(dummy1 "Mark all synapses" :boolean))
       (dummy11 "Edit synapse marker colors" :boolean)
       (dummy10 "Set individual synapse type parameters" :boolean)
       (dummy20 "Enable synapse RFs and connections" :boolean)
       ,(if dummy12 `(dummy17 "Clear all synapse RFs" :boolean) `(dummy12 "Draw all synapse RFs" :boolean))
       (dummy16 "Use same synapse RF height above cells" :boolean)
       ,(if dummy14 `(dummy18 "Clear all RF connections" :boolean) `(dummy14 "Include all RF connections" :boolean))
       ,(when (are-there-light-synapses) `(:comment "Light stimulus graphics"))
       ,(when (is-light-moving)
	  `(dummy19 "Number of moving stimulus snapshots" :integer)
	  `(dummy21 "If moving stimulus drawn, label times of stimulus snapshots" :boolean))
       ,(when (are-there-light-synapses)
	  `(dummy7 "View in plane of synapse stimulus (automatic if stimulus shown)" :boolean)
	  `(dummy8 "Position of stimulus drawing relative to cell:" :choose (:front :back) :label-left))
       (dummy6 "Some more details" :boolean))
     :label "Setting Up Synapse Graphics")
    (when dummy6
      (setq dummy5 (CHOOSE-DASH-PATTERNS dummy5 "Dash Pattern for Synapse RF Connections"))
      (choose-variable-values
       '((*SYN-RF-SHAPE-SHADING* "Synapse RF shape shading (0 => transparent)" :float)
	 (dummy13 "Common synapse RF height above cells" :float)
	 (*syn-rf-connection-shading* "Shading of RF connections (percent)" :float)
	 (dummy4 "Thickness of RF connections" :float))
       :label "Synapse RF Graphics Details"))
    (setq update-flag (or dummy11
			  (not (eq dummy8 (gv win :where-synapse-stimulus-goes)))
			  (xor dummy21 *label-stimulus-times*)
			  (xor dummy19 *motion-snapshots*)
			  (and (or (gv win :draw-light-stimulus) dummy7)
			       (case *light-stimulus-plane*
				 (:xy (not (and (zerop (rad-to-deg (gv win :phi))) (zerop (rad-to-deg (gv win :theta))))))
				 (:xz (not (and (zerop (rad-to-deg (gv win :phi))) (= 90.0 (rad-to-deg (gv win :theta))))))))
			  (not (eq dummy4 *syn-rf-connection-thickness*))
			  (not (eq dummy5 *syn-rf-connection-dash*))
			  (xor dummy20 (gv win :draw-synapse-rfs))
			  (xor dummy1 (gv win :mark-all-synapses)) dummy3
			  (xor dummy2 (gv win :enable-marked-synapses))
			  (xor dummy12 (gv win :draw-all-synapse-rfs)) dummy17
			  (not (= dummy13 (if (gv win :synapse-rf-height) (gv win :synapse-rf-height) 100.0)))
			  (xor dummy14 (gv win :draw-all-synapse-rfs-connections)) dummy18
			  (xor dummy16 (gv win :use-same-synapse-rfs-height))))
    (s-value win :where-synapse-stimulus-goes dummy8) 
    (setq *motion-snapshots* dummy19
	  *label-stimulus-times* dummy21
	  *syn-rf-connection-thickness* dummy4
	  *syn-rf-connection-dash* dummy5)
    (when dummy7
      (case *light-stimulus-plane*
	(:xy (set-histology-window-angle-scale-parameters win (deg-to-rad 0.0) (deg-to-rad 0.0) (gv win :scale)))
	(:xz (set-histology-window-angle-scale-parameters win (deg-to-rad 90.0) (deg-to-rad 0.0) (gv win :scale)))))
    (s-value win :synapse-rf-height dummy13)
    (s-value win :use-same-synapse-rfs-height dummy16)
    (let ((win-element-type-graphics-parameters-w/o-synapses
	   (loop for type-info in (gv win :element-type-graphics-parameters) unless (synapse-type-p (cdr-assoc 'type type-info)) collect type-info))
	  (synapse-type-graphics-parameters 
	   (loop for synapse-type in (synapse-types) when (element-in-circuit synapse-type) collect
		 (multiple-value-bind (dummy5 dummy7 dummy8 dummy9)
		     (loop for type-info in (gv win :element-type-graphics-parameters)
			   when (eq synapse-type (cdr-assoc 'type type-info))
			   do (return (values (cdr-assoc 'label-type type-info) (cdr-assoc 'height type-info)
					      (cdr-assoc 'draw-rf-shape type-info) (cdr-assoc 'draw-rf-connections type-info)))
			   finally (return (values nil (gv win :synapse-rf-height) nil nil)))
		   (setq original-label-flag dummy5)
		   (cond (dummy1 (setq dummy5 t))
			 (dummy3 (setq dummy5 nil)))
		   (cond (dummy12 (setq dummy8 t))
			 (dummy17 (setq dummy8 nil)))
		   (cond (dummy14 (setq dummy9 t))
			 (dummy18 (setq dummy9 nil)))
		   (when dummy16 (setq dummy7 dummy13))
		   (let ((orig-height dummy7)
			 (orig-rf-enable dummy8)
			 (orig-rf-conn-enable dummy9)
			 (menu-list `((dummy5 ,(format nil "Label synapse type ~a" (synapse-type-name synapse-type)) :boolean))))
		     (case (synapse-type-control synapse-type)
		       (:light
			(push '(dummy7 "RF height above cells" :float) menu-list)
			(push '(dummy8 "Draw synapse type RF" :boolean) menu-list)
			(push '(dummy9 "Draw RF connections to cells" :boolean) menu-list))
		       (setq dummy8 nil dummy9 nil))
		     (when dummy10 
		       (choose-variable-values menu-list :label (format nil "Graphics Options for Synapse Type ~a" (synapse-type-name synapse-type))))
		     (setq update-flag (or update-flag (xor original-label-flag dummy5) (not (= orig-height dummy7))
					   (xor orig-rf-enable dummy8) (xor orig-rf-conn-enable dummy9)))
		     (setq reset-label-all-synapses (not dummy5)
			   reset-draw-all-synapse-rfs (not dummy8)
			   reset-draw-all-synapse-rfs-connections (not dummy9))
		     `((type . ,synapse-type) (label-type . ,dummy5) (height . ,dummy7) (draw-rf-shape . ,dummy8) (draw-rf-connections . ,dummy9)))))))
      (s-value win :element-type-graphics-parameters (concatenate 'list win-element-type-graphics-parameters-w/o-synapses synapse-type-graphics-parameters)))
    (when dummy11 (update-synapse-type-colors win))
    (s-value win :mark-all-synapses (unless (or reset-label-all-synapses dummy3) dummy1))
    (s-value win :draw-all-synapse-rfs (unless (or reset-draw-all-synapse-rfs dummy17) dummy12))
    (s-value win :draw-all-synapse-rfs-connections (unless (or reset-draw-all-synapse-rfs-connections dummy18) dummy14))
    (s-value win :draw-synapse-rfs dummy20)
    (s-value win :enable-marked-synapses dummy2)
    (s-value win :update-synapse-stimulus update-flag)
    (s-value win :update-synapse-rfs (or (gv win :update-synapse-rfs) update-flag))
    (s-value win :update-marked-synapses (or (gv win :update-marked-synapses) update-flag))))
|#

(defun synapse-graphics-menu (&optional (win *standard-graphics-output*)) (synapse-marking-graphics-menu win))

(defun synapse-marking-graphics-menu (&optional (win *standard-graphics-output*))
  (when (opal-obj-exists win)
    (let (dummy1 dummy5)
      (choose-variable-values
       `((dummy1 "Select synapse types to label" :boolean)
	 (dummy5 "Edit synapse colors" :boolean))
       :label "Setting Up Synapse Graphics")
      (loop for type in (gv win :marked-element-types)
	    unless (synapse-type-p type) collect type
	    into marked-types-w/o-synapses
	    else collect type into selected-types
	    finally
	    (let ((marked-synapse-types (if dummy1
					  (choose-list-values-from-keys
					   (mapcar #'(lambda (type) (list (element-name type) type)) (synapse-types)) selected-types
					   :punt-if-only-one-entry nil
					   :label (format nil "Choose Marked Synapse Types for ~A" (gv win :title)))
					  selected-types)))
	      (s-value win :marked-element-types (concatenate 'list marked-types-w/o-synapses marked-synapse-types))
	      (when dummy5 (update-type-colors marked-synapse-types))
	      (s-value win :update-marked-elements t))))))

(defun channel-marking-graphics-menu (&optional (win *standard-graphics-output*))
  (when (opal-obj-exists win)
    (let (dummy1 dummy5)
      (choose-variable-values
       `((dummy1 "Select channel types to label" :boolean)
	 (dummy5 "Edit channel colors" :boolean))
       :label "Setting Up Channel Graphics")
      (loop for type in (gv win :marked-element-types)
	    unless (channel-type-p type) collect type
	    into marked-types-w/o-channels
	    else collect type into selected-types
	    finally
	    (let ((marked-channel-types (if dummy1 (choose-list-values-from-keys
						    (mapcar #'(lambda (type) (list (element-name type) type)) (channel-types)) selected-types
						    :punt-if-only-one-entry nil
						    :label (format nil "Choose Marked Channel Types for ~A" (gv win :title)))
					    selected-types)))
	      (s-value win :marked-element-types (concatenate 'list marked-types-w/o-channels marked-channel-types))
	      (when dummy5 (update-type-colors marked-channel-types))
	      (s-value win :update-marked-elements t))))))

(defun update-type-colors (element-type-sym)
  ;; ELEMENT-TYPE-SYM should be 'synapse-type or 'channel-type ....
  (let ((element-types (if (consp element-type-sym)
			 (coerce-to-list (element-type element-type-sym))
			 (case element-type-sym
			   (synapse-type (synapse-types))
			   (channel-type (channel-types))))))
    (loop for name-color in
	  (color-CHOOSE-BUTTON-menu
	   (mapcar #'(lambda (type) `(,(element-name type) ,(element-parameter type 'color))) element-types)
	  "Assigning Colors to Element Types" ""
	  :all-values '(red green blue purple cyan orange)
	  :xclusive-choice nil)
	  ; when (cadr name-color) do
	  do (element-parameter (element (car name-color)) 'color (cadr name-color)))))

(defun element-graphics-menu (&optional (win *standard-graphics-output*))
  (when (opal-obj-exists win)
    (let (dummy1
	  (dummy2 (gv win :node-label-background-p))
	  dummy3
	  (dummy4 (gv win :where-element-markers-go))
	  (dummy5 (gv win :suppress-element-marker-borders)) ; applies to soma/segment markers
	  (dummy12 (gv win :suppress-grape-borders)) ; applies to soma/segment markers
	  (dummy6 (gv win :grape-size-reference))
	  (dummy7 (or (gv win :grape-size-microns) 10.0))
	  (dummy8 (or (gv win :grape-size-pixels) 10))
	  (dummy9 (gv win :include-element-key-window))
	  dummy10 dummy11
	  (dummy15 (or (gv win :label-x-offset) 0))
	  (dummy16 (gv win :view-angle-comment-p))
	  (dummy17 (gv win :soma-histology-fixed-diameter-p))
	  (dummy19 (or (gv win :soma-histology-fixed-diameter-pixels) 0))
	  dummy18
	  (dummy20 (gv win :marker-diameter))
	  (menu-list
	   ;; (*node-graphics-coefficient* "Coefficient for element marker size" :float)
	   `((dummy1 "Edit window font" :boolean)
	     ,(when (gv win :markers) '(dummy3 "Edit marked points" :boolean))
	     ,(when (gv win :markers) '(dummy10 "Edit marked points font" :boolean))
	     (dummy11 "Edit scale bar" :boolean)
	     (dummy18 "Edit default line style (e.g. for scale bar)" :boolean)
	     (dummy16 "Include view angle" :boolean)
	     (dummy17 "Draw anatomical somas with fixed diameter (below)" :boolean)
	     (dummy19 "Fixed anatomical soma diameter [pixels]" :integer)
	     (:comment "Soma/segment markers highlight node positions")
	     (dummy4 "Position of soma/segment markers relative to cell:" :choose (:front :back) :label-left)
	     (dummy20 "Soma/segment marker diameter [pixels]" :integer)
	     (dummy5 "Suppress drawing of soma/segment marker borders" :boolean)
	     (dummy2 "Add background to soma/segment labels" :boolean)
	     (dummy15 "Label X offset [pixels]" :integer)
	     (:comment "Element markers are for channels and synapses")
	     (dummy9 "Include element marker key window" :boolean)
	     (dummy12 "Suppress drawing of element marker borders" :boolean)
	     (dummy6 "Reference for element marker size:" :choose (:microns :pixel) :label-left)
	     (dummy7 "Relative size for element markers [microns]" :float)
	     (dummy8 "Pixel size for element markers" :integer))))	     
      (choose-variable-values menu-list :label "Histology Details....")
      (when dummy18
	(s-value win :default-line-style-base (line-style-menu :default-style (gv win :default-line-style)
							       :label (format nil "~A Default Line Style" (gv win :title)))))
      (cond-every
       (dummy11 (edit-histology-scale-bar win))
       (dummy3 (mark-coords-pointer-menu win))
       (dummy10 (ph::Edit-marked-points-font win))
       (dummy1 (s-value win :comment-font (s-value win :font (font-menu (gv win :font) (format nil "Comment font for ~A" (gv win :title)))))))
      (let ((update-element-graphics
	     (or dummy11
		 (xor dummy17 (gv win :soma-histology-fixed-diameter-p))
		 (not (= dummy19 (gv win :soma-histology-fixed-diameter-pixels)))
		 (not (= dummy20 (gv win :marker-diameter)))
		 (not (= dummy15 (gv win :label-x-offset)))
		 (xor dummy2 (gv win :node-label-background-p))
		 (xor dummy11 (gv win :include-scale-bar))
		 (xor (gv win :suppress-element-marker-borders) dummy5)
		 (xor (gv win :suppress-grape-borders) dummy12)
		 (not (equal (gv win :where-element-markers-go) dummy4))
		 (not (eq (gv win :grape-size-reference) dummy6))
		 (not (= (gv win :grape-size-microns) dummy7))
		 (not (= (gv win :grape-size-pixels) dummy8)))))
	(s-value win :soma-histology-fixed-diameter-p dummy17)
	(s-value win :soma-histology-fixed-diameter-pixels dummy19)
	(s-value win :marker-diameter dummy20)
	(s-value win :label-x-offset dummy15)
	(s-value win :view-angle-comment-p dummy16)
	(s-value win :update-marked-nodes (or (gv win :update-marked-nodes) update-element-graphics))
	(s-value win :update-marked-elements (or (gv win :update-marked-elements) update-element-graphics))
	(s-value win :update-plotted-nodes (or (gv win :update-plotted-nodes) update-element-graphics))
	(s-value win :update-element-key-window (or (xor dummy9 (gv win :element-key-window)) (xor dummy9 (gv win :include-element-key-window))))
	(s-value win :include-element-key-window dummy9)
	(s-value win :node-label-background-p dummy2)
	(s-value win :suppress-element-marker-borders dummy5)
	(s-value win :suppress-grape-borders dummy12)
	(s-value win :where-element-markers-go dummy4)
	(s-value win :grape-size-reference dummy6)
	(s-value win :grape-size-microns dummy7)
	(s-value win :grape-size-pixels dummy8)))
    nil))

(defun edit-histology-scale-bar (win)
  (let ((dummy11 (gv win :include-scale-bar))
	(dummy12 (not (gv win :auto-histology-scale-bar-um-length)))
	(dummy13 (gv win :histology-scale-bar-um-length))
	(dummy17 (gv win :fix-histology-scale-bar-length))
	(dummy19 (gv win :visible-histology-scale-bar-sides))
	(dummy21 (gv win :include-histology-scale-bar-label)))
    (choose-variable-values
     '((dummy11 "Include scale bar" :boolean)
       (dummy12 "Disable automatic scale bar length" :boolean)
       (dummy13 "Length of scale bar [um]" :number)
       (dummy17 "Fix histology scale bar length" :boolean)
       (dummy19 "Include histology scale bar sides" :boolean)
       (dummy21 "Include histology scale bar label" :boolean))
     :label "Histology Scale Bar Details....")
    (s-value win :include-scale-bar dummy11)
    (s-value win :auto-histology-scale-bar-um-length (not dummy12))
    (s-value win :histology-scale-bar-um-length dummy13)
    (s-value win :fix-histology-scale-bar-length dummy17)
    (s-value win :visible-histology-scale-bar-sides dummy19)
    (s-value win :include-histology-scale-bar-label dummy21))
  nil)
      
(defun mark-nodes-menu (&optional (win *standard-graphics-output*))
  (when (opal-obj-exists win)
    (let (dummy1 (dummy2 "") branches)
      (choose-variable-values
       '((dummy1 "Use menu (T) or choose branch entered below (NIL)" :boolean)
	 (dummy2 "Branch name" :string))
       :label "Choosing Cell Branch")
      (setq branches
	    (if (not dummy1)
	      (list dummy2)
	      (choose-list-values (loop for branch in *branch-list* collect (element-name (car (last branch))))
				  (loop for segco in (gv win :marked-segments-and-colors) collect (element-name (caar segco)))
				  :label "Select branch(s) to display")))
      (let (branch-color)
	(s-value win :marked-segments-and-colors
		 (loop for name-color in (color-CHOOSE-BUTTON-menu
					  (loop for branch in (elements branches 'segment)
						when (setq branch-color (get-branch-color-from-win-marked-segments-and-colors branch win))
						collect (list branch branch-color) else collect (list branch nil))
					  "Assigning Colors to Branches" "Branches (proximal node)" :xclusive-choice nil)
		       when (cadr name-color) collect (list (get-branch-elements (car name-color) 'segment) (cadr name-color))))))))

(defun get-branch-color-from-win-marked-segments-and-colors (branch win)
  (let ((branch-and-color (find branch (gv win :marked-segments-and-colors) :key `car :test 'equal)))
    (when branch-and-color (cadr branch-and-color))))

(defun mark-segment-chains-menu (&optional (win *standard-graphics-output*) chosen-one)
  (when (opal-obj-exists win)
    (when chosen-one (s-value win :chosen-one chosen-one))
    (let (dummy1
	  (dummy2 (or (when (segment-p (element (gv win :chosen-one))) (element-name (gv win :chosen-one))) ""))
	  (dummy3 (gv win :disable-segment-chain-marking))
	  dummy4 dummy5 (dummy6 t) dummy7 dummy8 
	  (dummy14 (princ-to-string (gv win :segment-color-shading))))
      (choose-variable-values
       `((dummy2 "Reference for segment" :string)
	 (dummy8 "Label reference" :boolean)
	 (dummy4 "Mark distal segments starting from reference" :boolean)
	 (dummy5 "Mark proximal segments starting from reference" :boolean)
	 (dummy3 "Disable segment chain marking" :boolean)
	 (dummy7 "Clear any previously marked segments" :boolean)
	 (dummy14 "Segment shading:" :choose ,(mapcar 'princ-to-string *line-styles-shadings*) :rank-margin 6))
       :title (format nil "~A: Choose Cell Segments for Marking" (gv win :title)))
      (when dummy8 (label-element dummy2 win nil))
      (s-value win :disable-segment-chain-marking dummy3)
      (when dummy7
	(s-value win :marked-segments-and-colors nil)
	(s-value win :labeled-elements nil))
      (s-value win :segment-color-shading (s-flt (read-from-string dummy14)))
      (unless dummy3
	(let* ((reference-segment (element-cell-element dummy2 'segment))
	       (segments-out (when dummy4 (remove reference-segment (SEGMENTS-OUT reference-segment))))
	       (segments-in (when dummy5 (remove reference-segment (SEGMENTS-in reference-segment))))
	       (keys (no-nils (list (element-name reference-segment)
				    (when dummy4 (format nil "~A: Distal segments" (element-name reference-segment)))
				    (when dummy5 (format nil "~A: Proximal segments" (element-name reference-segment))))))
	       (seg-groups (no-nils `((,reference-segment) ,segments-out ,segments-in)))
	       (keys-colors
		(when (segment-p reference-segment)
		  (color-CHOOSE-BUTTON-menu
		   (loop for seg-group in seg-groups
			 for key in keys
			 collect (list key (get-branch-color-from-win-marked-segments-and-colors seg-group win)))
		   (format nil "Color for Marked Segments Referenced by ~A" (element-name reference-segment)) "" :xclusive-choice nil))))
	  (when reference-segment
	    (loop for chain in seg-groups
		  for key-color in keys-colors do	(update-win-marked-segments-and-colors win (list chain (or (cadr key-color) (get-opal-color 'red)))))))))))

(defun init-histology-window-draw-slots (win &optional parent-win)
  (s-value win :complete-update t)
  (s-value win :include-element-key-window (not parent-win))
  (transfer-schema-slots
   parent-win win
   '(:font :comment-font
     :soma-outline-p
     :soma-histology-fixed-diameter-p
     :soma-histology-fixed-diameter-pixels
     :include-colorizing-scale
     :displayed-time
     :colorize-start-time :colorize-stop-time :colorize-step :colorize-instant-time :colorize :colorize-integrate-start-time :colorize-integrate-stop-time
     :lock-color
     :show-time

     :labeled-elements :where-element-markers-go :restrict-axons-to-cells :restrict-axons-to-synapse-types :restrict-synapse-cxns-to-cells
     :restrict-synapse-cxns-to-synapse-types :where-somas-go :where-segments-go :where-axons-go :where-synapse-cxns-go :segment-color
     :restrict-cells-to-cell-types :cells :cell-types :element-type-graphics-parameters
     :grape-size-microns :grape-size-pixels :grape-size-reference :disable-segment-chain-marking
     :enable-marked-channels :draw-light-stimulus :draw-synapse-rfs :plotted-node-graphics :all-node-graphics :enable-marked-synapses
     (:auto-histology-scale-bar-um-length t) (:segment-color-shading 100.0)
     (:histology-scale-bar-um-length 100.0)  (:include-scale-bar t)
     (:adjust-histology-window :automatic)   (:draw-somas t)
     (:draw-axons t) (:axon-color-from-synapse)
     (:draw-synapse-cxns t) (:synapse-cxn-color-from-synapse)
     (:draw-anatomy t)      (:label-x-offset 0)
     (:source-graphics nil))))

(defun unchoose-chosen-ones (&optional (wins *standard-graphics-output*))
  (loop for win in (no-nils (coerce-to-list wins))
	when (opal-obj-exists win)
	do
	(add-temp-comment win "")
	(draw-chosen-one nil win t)
	(s-value win :chosen-one nil)
	(opal::update win t))
  nil)

(defun rechoose-chosen-ones (&optional (wins *standard-graphics-output*))
  (loop for win in (no-nils (coerce-to-list wins)) when (opal-obj-exists win) do (draw-chosen-one nil win) (opal::update win t))
  nil)

(defun draw-chosen-one (&optional chosen-ones (win *standard-graphics-output*) erase)
  (when (opal-obj-exists win)
    (unless (gv win :lock-color)
      (when chosen-ones
	(typecase chosen-ones
	  (cons (s-value win :chosen-ones (loop for chosen-one in chosen-ones collect (element-cell-element chosen-one))))
	  (t (s-value win :chosen-one (element-cell-element chosen-ones)))))
      (let* ((chosen-ones (or (gv win :chosen-ones) (list (gv win :chosen-one))))
	     (agg (clear-and-add-plot-agg  win `colored-node-agg :add (and (not erase) chosen-ones) :where :front)))
	(when agg
	  (loop for chosen-one in chosen-ones do
		(typecase chosen-one
		  (segment (draw-segment win chosen-one agg :color *chosen-one-color* :shading *chosen-one-shading*))
		  (soma (draw-somas win t :soma-agg agg :color *chosen-one-color* :target-soma chosen-one :shading *chosen-one-shading*)))))))))

(defun mark-nodes (win draw &optional update)
  (let ((mark-agg (clear-and-add-plot-agg win `marked-nodes :add draw :where :front)))
    (when mark-agg
      (loop for segs-and-color in (gv win :marked-segments-and-colors) do
	    (loop for seg in (car segs-and-color) do (draw-segment win seg mark-agg :color (cadr segs-and-color) :shading 50)))))
  (when update (histology-window-finishing win)))

(defun mark-segment-chains (&optional (win *standard-graphics-output*) (draw t) (use-menu t) (update t))
  (when (opal-obj-exists win)
    (when (and draw use-menu) (mark-segment-chains-menu win))
    (draw-segments win (and draw (not (gv win :disable-segment-chain-marking)) (gv win :marked-segments-and-colors)) 'marked-segments)
    (when update (histology-window-finishing win))))
  
(defun update-win-marked-segments-and-colors (win new-chain-and-color)
  (let* (new-chain-and-color-is-a-replacement 
	 (cleaned-up-marked-segments-and-colors
	  (loop for chain-and-color in (gv win :marked-segments-and-colors)
		collect	(if (= (length (intersection (coerce-to-list (car chain-and-color))
						     (coerce-to-list (car new-chain-and-color))))
			       (length (car chain-and-color)))
			  (setq new-chain-and-color-is-a-replacement new-chain-and-color)
			  chain-and-color))))
    (s-value win :marked-segments-and-colors (if new-chain-and-color-is-a-replacement
					       cleaned-up-marked-segments-and-colors
					       (cons new-chain-and-color cleaned-up-marked-segments-and-colors)))))

(defun update-labeled-elements (&optional (win *standard-graphics-output*))
  (when (opal-obj-exists win)
    (let ((top-agg (get-agg win)))
      (loop for comp in (gv top-agg :components)
	    when (case (gv comp :type)
		   (node-labels t))
	    do (opal::remove-component top-agg comp))
      (let ((original-labeled-elements (gv win :labeled-elements)))
	(s-value win :labeled-elements nil)
	(label-element original-labeled-elements win nil)))))

(defun mark-plotted-nodes (win draw)
  ;; For both plotted and all nodes
  (let ((label-agg (clear-and-add-plot-agg win `plotted-node-labels :add draw :where :front))
	(mark-agg (clear-and-add-plot-agg win `plotted-nodes :add draw :where :front)))
    (when draw
      (cond-every
       ((gv win :all-node-graphics)
	(mark-segments-and-somas
	 (somas-and-segments (element (gv win :cells) 'cell) nil nil)
	 :win win :type 'all-nodes :marker-diameter (gv win :marker-diameter) :mark-fill *marked-node-fill*
	 :mark-agg (when (member :mark (gv win :all-node-graphics)) mark-agg)
	 :label-agg (when (member :label (gv win :all-node-graphics)) label-agg)
	 :update nil))
       ((and (not (member :label (gv win :all-node-graphics))) (gv win :plotted-node-graphics))
	(mark-segments-and-somas
	 (plotted-somas-and-segments (element (gv win :cells) 'cell)) ; Mark plotted nodes
	 :win win :type 'plotted-nodes
	 :mark-agg (when (member :mark (gv win :plotted-node-graphics)) mark-agg)
	 :marker-diameter (gv win :marker-diameter) :mark-fill *plotted-node-fill* 
	 :label-agg (when (member :label (gv win :plotted-node-graphics)) label-agg)
	 :update nil))))))

(defun plotted-somas-and-segments (&optional (cells (cells))) (somas-and-segments cells t nil))

(defun get-marker-color (element)
  (let ((elt-color (or (element-parameter element 'color) (element-parameter (element-type element) 'color))))
    (when elt-color (get-opal-color elt-color))))

(defun element-key-window (key-ref-win &key (key-diameter 10) (border 10))
  (let* ((element-key-win (opal::validate-window-reference-slot key-ref-win :element-key-window))
	 ;; (key-window-left (gv (or element-key-win key-ref-win) :left))
	 ;; (key-window-top (if element-key-win (gv element-key-win :top) (+ (gv key-ref-win :top) (gv key-ref-win :height))))
	 )
    (clear-window (gv key-ref-win :element-key-window) t)
    (setf (gv key-ref-win :element-key-window) nil)
    (when (gv key-ref-win :marked-element-types)
      (setq element-key-win (s-value key-ref-win :element-key-window (create-aux-histology-window :auxiliary-type :keys :title (format nil "~A Keys" (gv key-ref-win :title)))))
      (let* ((*grape-size* key-diameter)
	     (names-and-colors
	      (loop for type in (gv key-ref-win :marked-element-types)
		 collect (list (format nil "~A ~A" (string-capitalize (replace-char-w-space-in-string (string (type-of type)) #\-)) (element-name type))
			       (get-marker-color type))))
	     (element-key-win key-ref-win) ; Now put element key into histo window
	     (element-label-key-agg (clear-and-add-plot-agg element-key-win `element-label-key :add t :where :front))
	     (key-font (opal:get-standard-font :serif :bold-italic :medium))
	     (key-font-height (gv key-font :font-height))
	     (key-step (+ 7 (max key-diameter key-font-height)))
	     (y 10))
	;; (s-value element-key-win :top key-window-top)
	;; (s-value element-key-win :left key-window-left)
	;; (s-value element-key-win :key-reference-window key-ref-win) 
	;; (s-value element-key-win :background-color (o-formula (gvl :key-reference-window :background-color)))
	(loop for name-and-color in names-and-colors
	   maximize (gv (opal:add-component
			 element-label-key-agg
			 (create-instance nil window-hack-text (:visible t) (:top y) (:left (round (+ key-diameter border border)))
					  (:string (car name-and-color)) (:font key-font)))
			:width)
	   into max-text-width
	   do
	     (opal:add-component
	      element-label-key-agg (make-grapes `(,(cadr name-and-color)) element-key-win (round (+ border (/ key-diameter 2))) (round (+ y (/ key-font-height 2)))))
	     (setq y (+ y key-step))
	   ;; finally
	     ;; (s-value element-key-win :width (+ max-text-width key-diameter border border border))
	     ;; (s-value element-key-win :height y)
	     )
	(s-value element-key-win :visible (and (when element-label-key-agg (gv element-label-key-agg :components)) names-and-colors))
	(resurrect-opal-win element-key-win :update t :raise t)))))

(defun grape-size (win)
  (* 2 (round (* 0.5 (case (gv win :grape-size-reference)
		       (:microns  (/ (or (gv win :grape-size-microns) *grape-size-microns*) (gv win :scale)))
		       (:pixel (or (gv win :grape-size-pixels) *grape-size*))
		       (t *grape-size*))))))

(defun make-grapes (colors win x y)
  (create-instance nil GRAPES
		   ;; Assigning the window here kills on OPAL::ADD-COMPONENT-METHOD-AGGREGATE...
		   ;; (:window win)
		   (:grape-colors colors)
		   (:center-x x)
		   (:center-y y)))

(defun synapse-color (syn) (element-parameter (synapse-type syn) 'color))

(defun element-marker-diameter (element win)
  (declare (optimize (safety 0) (speed 3) (space 1)))
  (let ((element (element-cell-element element)))
    (round (the sf (* *node-graphics-coefficient*
		      (/ (the sf (typecase element
				   (soma (soma-diameter element))
				   (segment (sqrt (the sf (* (segment-diameter element) (segment-length element)))))))
			 (the sf (gv win :scale))))))))

;; need these garnet functions.
(defun clear-histology-window ())

(defun draw-light-stimulus-ok (win)
  (and *enable-light*
       ; (ARE-THERE-LIGHT-SYNAPSES)
       (gv win :draw-light-stimulus)
       (case *light-stimulus-plane*
	 (:xy (and (close-to (gv win :cos-theta) 1.0) (close-to (gv win :cos-phi) 1.0)))
	 (:xz (and (close-to (gv win :cos-theta) 0.0) (close-to (gv win :cos-phi) 1.0))))))

(defun draw-light-stimulus (win &key (color *stimulus-graphic-color*))
  (let ((agg (clear-and-add-plot-agg win `light
				     :add (draw-light-stimulus-ok win)
				     :where (or (gv win :where-synapse-stimulus-goes)
						(if (member *WHERE-SYNAPSE-STIMULUS-GOES* '(:back :front))
						  *WHERE-SYNAPSE-STIMULUS-GOES*
						  :back)))))
    (when agg
      (case *light-stimulus*
	((:moving-bar :on-moving-bar :off-moving-bar :reversing-bar :moving-spot :on-moving-spot :off-moving-spot) (draw-moving-stimulus agg color))
	((:on-bar :off-bar :bar) (draw-bar-stimulus agg color))
	(:moving-bar-grating (draw-grating-stimulus agg color))	
	(:apparent-motion (draw-apparent-motion-stimulus agg color))
	((:on-spot :off-spot :spot) (draw-spot-stimulus agg color))
	(:annulus (draw-annulus-stimulus agg color)))
      (when *use-aperture* (draw-stimulus-aperture agg color)))))

(defun draw-annulus-stimulus (agg color)
  (add-circle *light-start-position-x* *light-start-position-y*
	      (/ *spot-outside-diameter* 2.0) agg ; :halftone-percent *stimulus-graphic-shading-percent* 
	      :color color :drawing-function :and)
  (add-circle *light-start-position-x* *light-start-position-y*
	      (/ *spot-inside-diameter* 2.0) agg ; :halftone-percent *stimulus-graphic-shading-percent* 
	      :color color :drawing-function :and)
  ;; (draw-stimulus-time "Annulus" agg)
  (draw-stimulus-time agg))

(defun draw-stimulus-aperture (agg color)
  (add-circle *aperture-center-x* *aperture-center-y* *aperture-radius* agg :line-style thin-dashed-2-blue-line :color color :drawing-function :and))

(defun draw-spot-stimulus (agg color)
  (if *fast-full-field-spot*
      (draw-stimulus-time agg "Full Field Spot")
      (progn
	(add-circle *light-start-position-x* *light-start-position-y*
		    (/ *spot-outside-diameter* 2.0) agg
		    :halftone-percent *stimulus-graphic-shading-percent* :color color :drawing-function :and)
	;; (draw-stimulus-time "Spot" agg)
	(draw-stimulus-time agg)
)))

(defun x-anatomy (x-stimulus y-stimulus x-anatomy-shift theta) 
  ;; Translate coordinates in the stimulus frame to the anatomical frame.
  (+ (+ (* (cos theta) x-stimulus)
	(* (sin (- theta)) y-stimulus))
     x-anatomy-shift))

(defun y-anatomy (x-stimulus y-stimulus y-anatomy-shift theta) 
  ;; Translate coordinates in the stimulus frame to the anatomical frame.
  (+ (+ (* (sin theta) x-stimulus)
	(* (cos theta) y-stimulus))
     y-anatomy-shift))

(defun draw-apparent-motion-stimulus (agg color)
  (add-line
   (x-anatomy (+ *bar-a-position-x* (/ *bar-a-length* 2)) *bar-a-position-y* *light-start-position-x* *light-theta*)
   (y-anatomy (+ *bar-a-position-x* (/ *bar-a-length* 2)) *bar-a-position-y* *light-start-position-y* *light-theta*)
   (x-anatomy (- *bar-a-position-x* (/ *bar-a-length* 2)) *bar-a-position-y* *light-start-position-x* *light-theta*)
   (y-anatomy (- *bar-a-position-x* (/ *bar-a-length* 2)) *bar-a-position-y* *light-start-position-y* *light-theta*)
   agg :stipple-percent *stimulus-graphic-shading-percent* :thickness *bar-a-width* :color color)
  (add-line
   (x-anatomy (+ *bar-b-position-x* (/ *bar-b-length* 2)) *bar-b-position-y* *light-start-position-x* *light-theta*)
   (y-anatomy (+ *bar-b-position-x* (/ *bar-b-length* 2)) *bar-b-position-y* *light-start-position-y* *light-theta*)
   (x-anatomy (- *bar-b-position-x* (/ *bar-b-length* 2)) *bar-b-position-y* *light-start-position-x* *light-theta*)
   (y-anatomy (- *bar-b-position-x* (/ *bar-b-length* 2)) *bar-b-position-y* *light-start-position-y* *light-theta*)
   agg :stipple-percent *stimulus-graphic-shading-percent* :thickness *bar-b-width* :color color)
  ;; (draw-stimulus-time "Apparent Motion" agg)
  (draw-stimulus-time agg)
  (add-string (format nil "A")
	      (x-anatomy *bar-a-position-x* *bar-a-position-y* *light-start-position-x* *light-theta*)
	      (y-anatomy *bar-a-position-x* *bar-a-position-y* *light-start-position-y* *light-theta*)
	      agg)
  (add-string (format nil "B")
	      (x-anatomy *bar-b-position-x* *bar-b-position-y* *light-start-position-x* *light-theta*)
	      (y-anatomy *bar-b-position-x* *bar-b-position-y* *light-start-position-y* *light-theta*)
	      agg))

(defun draw-bar-stimulus (agg color)
  (add-line
   (x-anatomy (/ *bar-length* 2) 0.0 *light-start-position-x* *light-theta*) (y-anatomy (/ *bar-length* 2) 0.0 *light-start-position-y* *light-theta*)
   (x-anatomy (/ *bar-length* -2) 0.0 *light-start-position-x* *light-theta*) (y-anatomy (/ *bar-length* -2) 0.0 *light-start-position-y* *light-theta*)
   agg :stipple-percent *stimulus-graphic-shading-percent* :thickness *bar-width* :color color)
  ;; (draw-stimulus-time "Bar" agg)
  (draw-stimulus-time agg)
)

(defun collect-snapshot-times ()
  (if *restrict-snapshot-to-real-time*
      (list *real-time*)
      (delete-duplicates
       (loop for i from 0 to (1- *motion-snapshots*)
	  collect (if (= *motion-snapshots* 1)
		      *light-stimulus-start-time*
		      (+ *light-stimulus-start-time*
			 (truncate (* i (/ (- (min *light-stimulus-stop-time* *user-stop-time*)
					      *light-stimulus-start-time*)
					   (1- *motion-snapshots*))))))))))

(defun draw-moving-stimulus (agg color &optional only-stimulus-sweep)
  (let (min-x min-y max-x max-y) 
    (dolist (snapshot-time (collect-snapshot-times))
      (let* ((y-translation-stim-frame
	      (cond ((>= snapshot-time *light-stimulus-stop-time*)
		     (* (if *light-direction* *light-speed* (- *light-speed*))
			(- *light-stimulus-stop-time* *light-stimulus-start-time*)))
		    ((< snapshot-time *light-stimulus-stop-time*)
		     (* (if *light-direction* *light-speed* (- *light-speed*))
			(- snapshot-time *light-stimulus-start-time*)))
		    (t 0.0)))
	     (x-translation (+ (* (sin (- *light-theta*)) y-translation-stim-frame) *light-start-position-x*))
	     (y-translation (+ (* (cos *light-theta*) y-translation-stim-frame) *light-start-position-y*))
	     time-label-y time-label-x)
	(case *light-stimulus* 
	  ((:moving-bar :on-moving-bar :off-moving-bar :reversing-bar)
	   (let ((x1 (s-flt (x-anatomy (/ *bar-length* 2) y-translation-stim-frame *light-start-position-x* *light-theta*)))
		 (y1 (s-flt (y-anatomy (/ *bar-length* 2) y-translation-stim-frame *light-start-position-y* *light-theta*)))
		 (x2 (s-flt (x-anatomy (/ *bar-length* -2) y-translation-stim-frame *light-start-position-x* *light-theta*)))
		 (y2 (s-flt (y-anatomy (/ *bar-length* -2) y-translation-stim-frame *light-start-position-y* *light-theta*))))
	     (if only-stimulus-sweep
	       (setq min-x (if min-x (min min-x x1 x2) (min x1 x2))
		     min-y (if min-y (min min-y y1 y2) (min y1 y2))
		     max-x (if max-x (max max-x x1 x2) (max x1 x2))
		     max-y (if max-y (max max-y y1 y2) (max y1 y2)))
	       (add-line x1 y1 x2 y2 agg :stipple-percent *stimulus-graphic-shading-percent* :thickness *bar-width* :color color))
	     (setq time-label-y (min y1 y2)
		   time-label-x (min x1 x2))))
	  ((:moving-spot :on-moving-spot :off-moving-spot)
	   (if only-stimulus-sweep
	     (setq min-x (if min-x
			   (min min-x (- x-translation (/ *spot-outside-diameter* 2.0)))
			   (- x-translation (/ *spot-outside-diameter* 2.0)))
		   min-y (if min-y
			   (min min-y (- y-translation (/ *spot-outside-diameter* 2.0)))
			   (- y-translation (/ *spot-outside-diameter* 2.0)))
		   max-x (if max-x
			   (max max-x (+ x-translation (/ *spot-outside-diameter* 2.0)))
			   (+ x-translation (/ *spot-outside-diameter* 2.0)))
		   max-y (if max-y
			   (max max-y (+ y-translation (/ *spot-outside-diameter* 2.0)))
			   (+ y-translation (/ *spot-outside-diameter* 2.0))))
	     (add-circle x-translation y-translation
			 (/ *spot-outside-diameter* 2.0) agg
			 :halftone-percent *stimulus-graphic-shading-percent* :color color :drawing-function :and))
	   (setq time-label-y y-translation
		 time-label-x x-translation)))
	(when (and (not only-stimulus-sweep) *label-stimulus-times*)
	  (add-string (format nil "~a ms" (round snapshot-time))
		      time-label-x time-label-y ; x-translation y-translation
		      agg :size :medium :background (gv agg :window :node-label-background-p) :y-pixel-offset -5))))
    (unless only-stimulus-sweep (draw-motion-arrow agg))
    (list min-x min-y max-x max-y)))

(defun draw-motion-arrow (agg) ;; (draw-stimulus-time "Motion" agg t)
  (draw-stimulus-time agg nil t))
(defun draw-grating-stimulus (agg color) (declare (ignore agg color)))

(defun label-cell-node (node agg &key (x-offset 0) (y-offset 0) (extra-label nil) explicit-node-xy-pixel-location font)
  (let* ((node (element-node node))
	 (node-location (where node))
	 (win (gv agg :window))
	 (view-plane-x (get-win-view-plane-x node-location win))
	 (view-plane-y (get-win-view-plane-y node-location win)))
    (add-string (if extra-label (format nil "~a~a"  (node-name node) extra-label) (format nil "~a" (node-name node)))
		view-plane-x view-plane-y agg
		:x-pixel (when explicit-node-xy-pixel-location (car explicit-node-xy-pixel-location))
		:y-pixel (when explicit-node-xy-pixel-location (cadr explicit-node-xy-pixel-location))
		:x-pixel-offset (round (if (zerop x-offset) (+ (/ (gv win :marker-diameter) 2) 4) x-offset)) :y-pixel-offset (round y-offset)
		:background (gv win :node-label-background-p) :color (gv win :node-label-color) :font font)))

(defun electrode-endpoint (win segment) (x-y-histology-win-from-view (node-absolute-location (segment-node-1 segment)) win))

(defun electrode-sourcepoint (win segment) `(,(+ 45 (car (electrode-endpoint win segment))) ,(+ -45 (cadr (electrode-endpoint win segment)))))

(defun draw-electrode (win segment agg &key (color opal:purple) (shading 100))
  (let* ((foreground-color (get-opal-color color shading))
	 (line-style (create-instance nil opal:line-style (:constant t) (:line-thickness 2) (:foreground-color foreground-color))))
    (opal:add-components
     agg 
     (create-instance nil opal:line	; gg::arrow-line
		      (:constant nil) (:open-p t) (:line-style line-style);; (:filling-style opal:red-fill)
		      (:x1 (car (electrode-sourcepoint win segment))) (:x2 (car (electrode-endpoint win segment)))
		      (:y2 (cadr (electrode-endpoint win segment)))   (:y1 (cadr (electrode-sourcepoint win segment))))
     (create-instance nil opal:circle
		      (:constant nil) (:open-p t) (:line-style line-style)
		      (:left (- (car (electrode-endpoint win segment)) 8)) (:top (- (cadr (electrode-endpoint win segment)) 8))
		      (:height 16) (:width 16)))))

(defun draw-electrodes (win &optional (draw-electrodes t) (type 'electrodes))
  (declare (optimize (safety 0) (speed 3) (space 1)))
  (unless (gv win :where-electrodes-go) (s-value win :where-electrodes-go :front))
  (let* ((electrodes
	  (when draw-electrodes
	    (loop for electrode in (electrodes) when (member (element-cell electrode) (element (gv win :cells) 'cell)) collect electrode)))
	 (electrode-agg (clear-and-add-plot-agg win type :add electrodes)))
    (when electrode-agg
      (loop for electrode in electrodes do (draw-electrode win electrode electrode-agg)))))

(defun draw-extracellular-electrodes (win &optional (draw-electrodes t) (type 'extracellular-electrodes))
  (declare (optimize (safety 0) (speed 3) (space 1)))
  (let* ((extracellular-electrodes (extracellular-electrodes))
	 (electrode-agg (clear-and-add-plot-agg win type :add (and extracellular-electrodes draw-electrodes))))
    (when electrode-agg
      (loop for electrode in extracellular-electrodes do (draw-extracellular-electrode win electrode electrode-agg)))))

(defun draw-extracellular-electrode (win electrode agg &key (color opal:green) (shading 150))
  (let* ((xy (x-y-histology-win-from-view (where electrode) win))
	 (foreground-color (get-opal-color color shading))
	 (line-style (create-instance nil opal:line-style (:constant t) (:line-thickness 2) (:foreground-color foreground-color))))
    (opal:add-component
     agg
     (create-instance nil opal:circle (:constant nil) (:open-p t) (:line-style line-style) (:left (car xy)) (:top (cadr xy)) (:height 12) (:width 12))
     :where :front)))

(defun draw-sources (win draw-them)
  (let* ((window-cells (element (gv win :cells) 'cell))
	 (sources (when draw-them (loop for source in (nconc (hash-table-list (ISOURCE-HASH-TABLE)) (hash-table-list (VSOURCE-HASH-TABLE)))
					when (member (element-cell source) window-cells) collect source)))
	 (agg (clear-and-add-plot-agg win `sources :add (and sources draw-them) :where :front)))
    (when agg
      (loop for source in sources
	    do (let* ((node-location (where source))
		      (x-y (if (electrode-p (element-cell-element source))
			       (electrode-sourcepoint win (element-cell-element source))
			       (x-y-histology-win-from-view node-location (gv agg :window))))
		      ;; Offset a bit for soma source	      
		      (y-offset (if (equalp (element-relative-location source) (list 0.0 0.0 0.0))
				    (* -0.5 (soma-diameter (cell-soma (element-cell source)))) 0.0)))
		 (when (member :label (gv win :source-graphics))
		   (label-cell-node (element-physical-node source) agg :x-offset 17
				    :y-offset (+ 30 0 ; y-offset
						 )
				    :extra-label (if (eq (named-structure-symbol source) 'isource) "-Isrc" "-Vsrc")))
		 (opal:add-component
		  agg
		  (create-instance nil gg::arrow-line
				   (:constant nil) (:open-p t)
				   ;; (:filling-style opal:red-fill)
				   (:line-style (create-instance nil opal:line-style (:constant t) (:line-thickness 2) (:foreground-color opal:red)))
				   (:x1 (+ 15 (car x-y))) (:x2 (car x-y)) (:y2 (+ (cadr x-y) 5)) (:y1 (+ (cadr x-y) 30)))
		  :where :front)))
      (opal:move-component (gv win :aggregate) agg :where :front)))) 
		     
(defun draw-stimulus-time (agg &optional stimulus-text motion-arrow)
  (destroy-stimulus-line agg)
  (when (or (eq *light-stimulus* :apparent-motion) (> *light-stimulus-start-time* 0) (< *light-stimulus-stop-time* *user-stop-time*))
    (let ((font (opal:get-standard-font :sans-serif :bold :medium)))
    (opal:add-component
     agg
     (create-instance nil opal:text (:visible t) (:type 'stimulus-label)
		      (:top *histology-window-stimulus-time-distance-from-top*)
		      (:left (o-formula (round (- *histology-window-stimulus-time-distance-from-left* (* 0.5 (gvl :width))))))
		      (:string (format nil "~Dms" (round *user-start-time*))) (:font font))
     :where :front)
    (opal:add-component
     agg
     (create-instance nil opal:text
		      (:visible t) (:type 'stimulus-label)
		      (:top *histology-window-stimulus-time-distance-from-top*)
		      (:string (format nil "~Dms" (round *user-stop-time*)))
		      (:left (o-formula (round (- (+ *histology-window-stimulus-time-distance-from-left* *histology-window-stimulus-time-length*)
						  (* 0.5 (gvl :width))))))
		      (:font font))
     :where :front)
    (loop for start in (if (eq *light-stimulus* :apparent-motion) (list *bar-a-start-time* *bar-b-start-time*) (list *light-stimulus-start-time*))
	  for stop in (if (eq *light-stimulus* :apparent-motion) (list *bar-a-stop-time* *bar-b-stop-time*) (list *light-stimulus-stop-time*))
	  for label in '("A" "B")
	  do (let ((x1 (round (+ *histology-window-stimulus-time-distance-from-left*
				 (* (max 0.0 (/ start *user-stop-time*)) *histology-window-stimulus-time-length*))))
		   (x2 (round (+ *histology-window-stimulus-time-distance-from-left*
				 (* (min 1.0 (/ stop *user-stop-time*)) *histology-window-stimulus-time-length*)))))
	       (when (= x1 x2) (setq x2 (1+ x1)))
	       (when (eq *light-stimulus* :apparent-motion)
		 (opal:add-component
		  agg
		  (create-instance nil opal:text (:visible t) (:type 'stimulus-label)
				   (:top *histology-window-stimulus-time-distance-from-top*)
				   (:left (o-formula (round (- (+ x1 (* 0.5 (- x2 x1))) (* 0.5 (gvl :width))))))
				   (:string label) (:font font))
		  :where :front))	    
	       (opal:add-component
		agg
		(create-instance nil opal:line
				 (:visible t) (:type 'stimulus-label)
				 (:x1 x1) (:x2 x2)
				 (:y1 (- *histology-window-stimulus-time-distance-from-top* 3)) (:y2 (- *histology-window-stimulus-time-distance-from-top* 3))
				 (:line-style
				  (create-instance nil opal:line-style
						   (:line-thickness 6) (:filling-style (create-instance nil opal:filling-style (:foreground-color opal:black)))))
				 (:draw-function :copy) (:line-thickness 6)) :where :front))))
  ;; Time line
  (unless (and (>= *light-stimulus-stop-time* *user-stop-time*) (= *light-stimulus-start-time* 0)
	       (not (eq *light-stimulus* :apparent-motion)))
    (opal:add-component agg (create-instance nil opal:line
					     (:visible t) (:type 'stimulus-label)
					     (:x1 *histology-window-stimulus-time-distance-from-left*)
					     (:x2 (o-formula (+ (gvl :x1) *histology-window-stimulus-time-length*)))
					     (:y1 *histology-window-stimulus-time-distance-from-top*)
					     (:y2 *histology-window-stimulus-time-distance-from-top*)
					     (:draw-function :copy)) :where :front))
  (unless (eq *light-stimulus* :apparent-motion)
    (let ((stimulus-label
	   (opal:add-component agg (create-instance nil opal:text (:visible t) (:type 'stimulus-label)
						    (:top *histology-window-stimulus-time-distance-from-top*)
						    (:string (if (and (>= *light-stimulus-stop-time* *user-stop-time*)
								      (= *light-stimulus-start-time* 0))
							       (concatenate-strings stimulus-text (if (> (length stimulus-text) 0) " " "") "On for entire run")
							       (or stimulus-text "")))
						    (:left (o-formula (round (- (+ *histology-window-stimulus-time-distance-from-left*
										   (* 0.5 *histology-window-stimulus-time-length*))
										(* 0.5 (gvl :width))))))
						    (:font (opal:get-standard-font :sans-serif :bold :large)))
			       :where :front)))
      (when motion-arrow
	(let* ((arrow-center-x (truncate (+ (gv stimulus-label :left) (* 0.5 (gv stimulus-label :width)))))
	       (arrow-center-y (truncate (+ *histology-window-stimulus-time-distance-from-top* 17 (gv stimulus-label :height)))))
	  (opal:add-component agg (create-instance nil gg::arrow-line
						   (:type 'stimulus-label) (:open-p t) (:line-style opal:line-2)
						   (:x1 (round (+ arrow-center-x (* -15 (sin (- (- *light-theta*) (if *light-direction* 0 pi)))))))
						   (:x2 (round (+ arrow-center-x (* 15 (sin (- (- *light-theta*) (if *light-direction* 0 pi)))))))
						   (:y1 (round (+ arrow-center-y (* -1 -15 (cos (- *light-theta* (if *light-direction* 0 pi)))))))
						   (:y2 (round (+ arrow-center-y (* -1 15 (cos (- *light-theta* (if *light-direction* 0 pi))))))))
			      :where :front)))))))

(defun destroy-stimulus-line (agg)
  (loop for comp in (gv agg :components)
	when (case (gv comp :type)
	       (stimulus-label t))
	do (opal:remove-component agg comp) (opal:destroy comp)))
	
(defun draw-segment (win segment agg &key color (shading 100))
  (declare (optimize (safety 0) (speed 3) (space 1)))
  (if (electrode-p segment)
      (draw-electrode win segment agg :color color :shading shading)
      (let* ((cell (segment-cell segment))
	     (start-location
	      (if (segment-dummy-proximal-node-location segment)
		  (list (+ (the sf (first (cell-origin cell))) (the sf (first (segment-dummy-proximal-node-location segment))))
			(+ (the sf (second (cell-origin cell))) (the sf (second (segment-dummy-proximal-node-location segment))))
			(+ (the sf (third (cell-origin cell))) (the sf (third (segment-dummy-proximal-node-location segment)))))
		  (node-absolute-location (segment-node-1 segment))))
	     (end-location (where segment)))
	(add-line (get-win-view-plane-x start-location win) (get-win-view-plane-y start-location win)
		  (get-win-view-plane-x end-location win)   (get-win-view-plane-y end-location win)
		  agg :thickness (segment-diameter segment) :color color :stipple-percent shading :where :front)))) 

(defun circle-to-polyline-list (center-x center-y center-z diameter &optional (number-vertices 16))
  ;; This assumes that the circle is perpendicular to the x axis.
  (let ((radius (* 0.5 diameter)))
    (loop for angle from 0 by (/ (* 2 pi-single) number-vertices)
	  for i from 0 to number-vertices
	  collect (list (float center-x) (+ (* radius (sin angle)) center-y) (+ (* radius (cos angle)) center-z)))))

(defun histology-window-segments (win) (loop for cell in (element (gv win :cells) 'cell) nconc (copy-list (cell-segments-to-plot win cell))))

(defun histology-window-somas (win) (loop for cell in (element (gv win :cells) 'cell) collect (cell-soma cell)))
  
(defun draw-somas (win draw-somas &key soma-agg color target-soma (shading 100))
  (declare (optimize (safety 0) (speed 3) (space 1)))
  (unless soma-agg (setq soma-agg (clear-and-add-plot-agg win `somas :add draw-somas :where (gv win :where-somas-go))))
  (let ((virtual-somas))
    (when soma-agg
      (draw-virtual-somas
       win color
       (do ((cells (element (gv win :cells) 'cell) (cdr cells)))
	   ((null cells) virtual-somas)
	 (let* ((cell (car cells))
		(virtual-soma
		 (let ((soma (cell-soma cell)))
		   (when (or (eq target-soma soma) (not target-soma))
		     (cond
		      ((and target-soma (soma-segments cell))
		       (loop for seg in (soma-segments cell) do (draw-segment win seg soma-agg :color *chosen-one-color* :shading *chosen-one-shading*))
		       nil)
		      ((and (gv win :draw-anatomical-soma) (or (element-parameter soma 'soma-outline) (element-parameter soma 'soma-points)))
		       (let* ((soma-center-correction (mapcar '- (soma-center-correction soma) (cell-origin cell)))
			      (soma-center-correction-points (concatenate 'list soma-center-correction '(0.0)))
			      (soma-outline (loop for point in (element-parameter soma 'soma-outline) ; Make copies so sort doesn't screw up original
						  collect (mapcar '- point soma-center-correction)))
			      (soma-points (loop for soma-circle in (element-parameter soma 'soma-points)
						 ;; Add a 0 at the end since the format of the SOMA-POINTS entries is (X Y Z Diameter). 
						 collect (mapcar '- soma-circle soma-center-correction-points))))
			 ;; To make the pancakes cover the behind ones.
			 (loop for soma-circle in (sort soma-points (if (>= (gv win :sin-phi) 0) '< '>) :key 'car) 
			       do (add-polyline 
				   (loop for polyline-3d-point
					 in (circle-to-polyline-list (nth 0 soma-circle) (nth 1 soma-circle) (nth 2 soma-circle) (nth 3 soma-circle))
					 collect (get-win-view-plane-x polyline-3d-point win) collect (get-win-view-plane-y polyline-3d-point win))
				   soma-agg :filled t :halftone-percent 20 :element soma

				   ; :color ; color
				   :filling-style (o-formula (color-to-fill (gvl :parent :window :default-graphics-color) 20))
				   :line-style (o-formula (gvl :parent :window :default-line-style))
				   ; (pick-thickness (round (the sf (/ 0.1 (the sf (gv win :scale))))))
				   :drawing-function :or 
				   ))
			 (when soma-outline
			   (add-polyline 
			    (loop for soma-point in soma-outline collect (get-win-view-plane-x soma-point win) collect (get-win-view-plane-y soma-point win))
			    soma-agg :filled t :halftone-percent 20 :where :back :element soma
			    :line-style (o-formula (gvl :parent :window :default-line-style)) ; opal:line-2
			    :drawing-function :or :color color)))
		       nil)
		      ((element-parameter soma 'cylinder-soma-segments) nil)
		      (t soma))))))
	   (when virtual-soma (push virtual-soma virtual-somas))))
       soma-agg))))
		
(defun draw-virtual-somas (win color somas &optional agg)
  (declare (optimize (safety 0) (speed 3) (space 1)))
  (when somas
    (let ((soma-agg (or agg (clear-and-add-plot-agg win 'somas :add t)))
	  (fill (color-to-fill color))
	  itemlist)
      (when soma-agg
	(loop for soma in somas
	      do (let* ((x-y (x-y-histology-win-from-view (node-absolute-location (soma-node soma)) win))
			(xcenter (car x-y))
			(ycenter (cadr x-y)))
		   (declare (fixnum xcenter ycenter))
		   (when (x-y-in-win x-y win)
		     (let* ((diameter (the fn (round (/ (soma-diameter soma) (the sf (gv win :scale))))))
			    (radius (the fn (round (/ diameter 2))))
			    (left (the fn (- xcenter radius)))
			    (top (the fn (- ycenter radius)))
			    (color (or (get-a-value 'color (soma-parameters soma)) (get-a-value 'color (node-parameters (soma-node soma)))))
			    (shading (or (get-a-value 'shading (soma-parameters soma)) (get-a-value 'shading (cell-parameters (soma-cell soma)))))
			    (filling-style (if (or color shading) (color-to-fill (or color 'black) (or shading (gv win :segment-color-shading))) fill)))
		       ;; #(left top width height filling-style line-style soma color-index) 
		       (push (make-virtual-soma-item-values-array left top diameter diameter filling-style nil xcenter ycenter soma (color-index color)) itemlist)))))
	(when itemlist
	  (let ((v-agg (make-v-agg virtual-soma itemlist 'somas)))
	    (s-value v-agg :colorizeable t)
	    (virtual-agg-finishing v-agg soma-agg (or (gv win :where-somas-go) :front))))))))

(defun synapse-rf-points (syn height &key include-center include-shape)
  (declare (optimize (safety 0) (speed 3) (space 1)))
  (syn-rf-geometry-values
   (syn)
   (values
    (when include-shape
      (loop for base-x in (first (cdr-assoc 'rf-shape type-parameters))
	    for base-y in (second (cdr-assoc 'rf-shape type-parameters))
	    collect (let ((nominal-x (+ syn-rf-center-x (the sf base-x)))
			  (nominal-y (+ syn-rf-center-y (the sf base-y)))
			  (nomimal-z height))
		      (case *light-stimulus-plane*
			(:xy (list nominal-x nominal-y nomimal-z))
			(:xz (list nominal-x height nominal-y)))))
      (case *light-stimulus-plane*
	(:xy (list syn-rf-center-x syn-rf-center-y height))
	(:xz (list syn-rf-center-x height syn-rf-center-y)))))))

(defun draw-synapse-rfs (win draw-them &key (halftone-percent *syn-rf-shape-shading*)
			     (thickness *syn-rf-connection-thickness*) (connection-shading *syn-rf-connection-shading*) (dash *syn-rf-connection-dash*))
  (declare (optimize (safety 0) (speed 3) (space 1)))
  (let ((synapse-rf-agg (clear-and-add-plot-agg win `synapse-rf-agg :add draw-them :where :front))
	(halftone-percent (or halftone-percent 0))
	(shape-array-index -1)
	(connections-array-index -1)
	(shape-item-values-array-list '())
	(connections-item-values-array-list '())
	(rf-shape-line-style faint-line)
	shape-virtual-agg connections-virtual-agg)
    (declare (fixnum shape-array-index connections-array-index))
    (when synapse-rf-agg
      (loop for type-info in (gv win :element-type-graphics-parameters)
	    when (and (synapse-type-p (cdr-assoc 'type type-info)) (eq :light (synapse-type-control (cdr-assoc 'type type-info))))
	    do (let* ((synapse-type (cdr-assoc 'type type-info))
		      (color (element-parameter synapse-type 'color))
		      (fill (when (> halftone-percent 0) (color-to-fill color halftone-percent)))
		      (height (s-flt (cdr-assoc 'height type-info)))
		      (line-style (ACCESS-*LINE-STYLES-ARRAY* thickness color connection-shading dash))
		      ;; (rf-shape-line-style (colored-line-style color 0))
		      ;; (rf-shape (cdr-assoc 'rf-shape type-info))
		      (draw-rf-shape (cdr-assoc 'draw-rf-shape type-info))
		      (draw-rf-connections (cdr-assoc 'draw-rf-connections type-info)))
		 (when (or draw-rf-shape draw-rf-connections)
		   (loop for syn being the hash-value of (SYNAPSE-HASH-TABLE) when (eq synapse-type (synapse-type syn))
			 when (member (element-cell syn) (element (gv win :cells) 'cell)) do 
			 (multiple-value-bind (shape-points connection-points)
			     (synapse-rf-points syn height :include-center draw-rf-connections :include-shape draw-rf-shape)
			   (when draw-rf-shape
			     (push
			      (make-virtual-polyline-item-values-array
			       (loop for point in shape-points
				     nconc (x-y-cons-histology-win (list (get-win-view-plane-x point win) (get-win-view-plane-y point win)) win))
			       rf-shape-line-style fill)
			      shape-item-values-array-list))
			   (when draw-rf-connections
			     (push
			      (apply 'make-virtual-line-item-values-array
				     (flatten-list 
				      (x-y-cons-histology-win (list (get-win-view-plane-x (where syn) win) (get-win-view-plane-y (where syn) win)) win)
				      (x-y-cons-histology-win (list (get-win-view-plane-x connection-points win) (get-win-view-plane-y connection-points win)) win)
				      line-style))
			      connections-item-values-array-list)))))
		 (when shape-item-values-array-list
		   (setq shape-virtual-agg (make-v-agg virtual-polyline shape-item-values-array-list 'rf-outlines))
		   (virtual-agg-finishing shape-virtual-agg synapse-rf-agg))
		 (when connections-item-values-array-list
		   (setq connections-virtual-agg (make-v-agg virtual-line connections-item-values-array-list 'synapse-rf-cxns))
		   (virtual-agg-finishing connections-virtual-agg synapse-rf-agg :back))
		 (s-value win :visible t)
		 (resurrect-opal-win win))))))

(defun colorize-voltages (target-time &optional (win *standard-graphics-output*))
  (when (opal-obj-exists win)
    (loop for comp in (gv (get-plot-agg win `histology) :components)
	  when (car (gv comp :cell-elements))
	  do (let ((voltage (get-element-value (car (gv comp :cell-elements)) target-time)))
	       (when voltage
		 (let ((color (get-opal-variable-color voltage)))
		   (case (car (gv comp :is-a))
		     (opal:circle (s-value comp :filling-style (create-instance nil opal:filling-style (:FOREGROUND-COLOR color))))
		     (opal:line (s-value (gv comp :line-style :filling-style) :foreground-color color)))))))
    (opal:update win)))

(defun color-cell (cell &optional (color 'blue) (win *standard-graphics-output*))
  (when (opal-obj-exists win)
    (let ((element-cell (coerce-to-list (element-cell cell))))
      (loop for comp in (gv (get-plot-agg win 'histology) :components)
	    when (and (loop for elt in (gv comp :cell-elements)
			    thereis (or (eq cell 'all) (member (node-cell (element-physical-node elt)) element-cell :test 'equal)))
		      (eq opal:line (car (gv comp :is-a))))
	    do (s-value (gv comp :line-style) :foreground-color (case color
								       (green opal:green)
								       (orange opal:orange)
								       (cyan opal:cyan)
								       (red opal:red)
								       (black opal:black)
								       (blue opal:blue))))
      (opal:update win t))))

(defun get-segments-to-plot (win)
  (loop for cell in (coerce-to-list (element (gv win :cells) 'cell))
	nconcing (if (gv win :restrict-to-PROXIMAL-SEGMENT)
		     (loop for seg in (PROXIMAL-DENDRITE-LIST nil (gv win :PROXIMAL-SEGMENT-LEVEL)) when (eq (segment-cell seg) cell) collect seg)
		     (copy-list (cell-segments cell)))))

(defun cell-segments-to-plot (win cell)
  (if (gv win :restrict-to-PROXIMAL-SEGMENT)
      (loop for seg in (PROXIMAL-DENDRITE-LIST nil (gv win :PROXIMAL-SEGMENT-LEVEL)) when (eq (segment-cell seg) cell) collect seg)
      (cell-segments cell)))

(defun segment-dummy-proximal-node-absolute-location (cell segment)
  (declare (optimize (safety 0) (speed 3) (space 1)))
  (let ((cell (or cell (segment-cell segment))))
    (list (+ (the sf (first (cell-origin cell))) (the sf (first (segment-dummy-proximal-node-location segment))))
	  (+ (the sf (second (cell-origin cell))) (the sf (second (segment-dummy-proximal-node-location segment))))
	  (+ (the sf (third (cell-origin cell))) (the sf (third (segment-dummy-proximal-node-location segment)))))))

(defun move-top-agg-components (win type)
  (let ((top-agg (get-agg win)))
    (when top-agg
      (loop for comp in (copy-list (gv top-agg :components)) when (eq (gv comp :type) type) do (opal::move-component top-agg comp :where :front)))))

(defun parse-marked-segments-and-colors (win)
  (loop for marked-segments-and-color in (gv win :marked-segments-and-colors)
	nconc (copy-list (car marked-segments-and-color)) into colored-segments
	nconc (copy-list (loop for seg in (car marked-segments-and-color) collect (cadr marked-segments-and-color)))
	into colors1 finally (return (list colored-segments colors1))))

(defun virtual-segment-core (segments colors win segment-agg)
  (declare (optimize (safety 0) (speed 3) (space 1)))
  (let (itemlist
	(segment-color-shading (the sf (or (gv win :segment-color-shading) 100.0)))
	(win-scale (the sf (gv win :scale)))
	(half-win-width (schema-half-width win))
	(half-win-height (schema-half-height win)))
    (declare (single-float win-scale half-win-width half-win-height))
    (do ((segments segments (cdr segments)))
	((null segments) itemlist)
      (let ((segment (car segments)))
	(if (get-a-value 'electrode (segment-parameters segment)) ; (electrode-p segment)
	  (draw-electrode win segment segment-agg)
	  (multiple-value-bind (x1 y1)
	      (x-y-histology-win-from-view-values-with-dims
	       (the cons (if (segment-dummy-proximal-node-location segment)
			   (segment-dummy-proximal-node-absolute-location (segment-cell segment) segment)
			   (node-absolute-location (segment-node-1 segment))))
	       half-win-width half-win-height win)
	    (declare (fixnum x1 y1))
	    (multiple-value-bind (x2 y2)
		(x-y-histology-win-from-view-values-with-dims (node-absolute-location (segment-node-2 segment)) half-win-width half-win-height win)
	      (declare (fixnum x2 y2))
	      (when (or (x-y-in-win-values x1 y1 win) (x-y-in-win-values x2 y2 win))
		(let ((thickness-pixels (the fn (round (the sf (/ (the sf (segment-diameter segment)) win-scale)))))
		      (segment-color-shading (or (get-a-value 'shading (segment-parameters segment)) (get-a-value 'shading (cell-parameters (segment-cell segment)))
						 segment-color-shading))
		      (color (or (car colors)
				 (get-a-value 'color (segment-parameters segment))
				 (get-a-value 'color (node-parameters (segment-node-2 segment)))
				 (gv win :segment-default-color)
				 (gv win :default-graphics-color))))
		  (push
		   (make-virtual-segment-item-values-array
		    ;; x1 y1 x2 y2 line-style thickness segment color-index
		    x1 y1 x2 y2
		    (access-*line-styles-array*-for-segments-fast thickness-pixels color segment-color-shading nil) thickness-pixels segment (color-index color))
		   itemlist))))		       
	    (when colors (setq colors (cdr colors)))))))))

(defun draw-segments (win &optional (draw-segments t) (type 'segments))
  (let ((segment-agg (clear-and-add-plot-agg win type :add draw-segments)))
    (when segment-agg
      (s-value segment-agg :type type)
      (let* (colors
	     (segments-to-draw (case type
				 (segments (histology-window-segments win))
				 (marked-segments (let ((marked-segments-and-colors (parse-marked-segments-and-colors win)))
						    (setq colors (cadr marked-segments-and-colors))
						    (car marked-segments-and-colors))))))
	(when segments-to-draw
 	  (let ((v-agg (make-v-agg virtual-segment (virtual-segment-core segments-to-draw colors win segment-agg) type)))
	    (s-value v-agg :colorizeable t)
	    (virtual-agg-finishing v-agg segment-agg)))))))

(defun collect-things-colors (win elements default-color)
  (loop for element in elements collect (typecase element
					  (axon (if (and (gv win :axon-color-from-synapse) (synapse-color (axon-target-synapse element)))
						  (synapse-color (axon-target-synapse element))
						  default-color))
					  (synapse (if (and (gv win :synapse-cxn-color-from-synapse) (synapse-color element))
						     (synapse-color element)
						     default-color)))))

(defun get-things-w-connections-virtual-seg-item-values-array-list (elements line-styles win)
  (let (item-list)
    (loop for element in elements
	  for line-style in line-styles
	  do (let* ((start (where (typecase element
				    (synapse (synapse-pre-synaptic-element element))
				    (axon element))))
		    (end (where (typecase element
				  (synapse element)
				  (axon (axon-target-synapse element)))))
		    (mid-points (when (gv win :use-connection-midpoints) (element-parameter element 'mid-points))))
	       (when (and start end)
		 (if mid-points
		   (do ((point-list (concatenate 'list (list start) mid-points (list end)) (cdr point-list)))
		       ((null (cdr point-list)))
		     (let* ((start (car point-list))
			    (end (cadr point-list))
			    (x-y-1 (x-y-histology-win-from-view start win))
			    (x-y-2 (x-y-histology-win-from-view end win)))
		       (when (or (x-y-in-win x-y-1 win) (x-y-in-win x-y-2 win))
			 (push (make-virtual-segment-item-values-array (car x-y-1) (cadr x-y-1) (car x-y-2) (cadr x-y-2) line-style 1 nil) item-list))))
		   (let ((x-y-1 (x-y-histology-win-from-view start win))
			 (x-y-2 (x-y-histology-win-from-view end win)))
		     (when (or (x-y-in-win x-y-1 win) (x-y-in-win x-y-2 win))
		       (push (make-virtual-segment-item-values-array (car x-y-1) (cadr x-y-1) (car x-y-2) (cadr x-y-2) line-style 1 nil) item-list)))))))
    item-list))

(defun collect-drawable-things-w-connections (win model-type)
  (loop for element in (list-of-all-things model-type)
	when (typecase element
	       (axon (and (axon-target-synapse element)
			  (or (not (gv win :restrict-axons-to-cells))
			      (member (element-cell element) (element (gv win :cells) 'cell)))
			  (or (not (gv win :restrict-axons-to-synapse-types))
			      (member (synapse-type (axon-target-synapse element)) (gv win :restrict-axons-to-synapse-types)))))
	       (synapse (and (eq :voltage (synapse-type-control (synapse-type element)))
			     (not (axon-p (synapse-pre-synaptic-element element)))
			     (or (not (gv win :restrict-synapse-cxns-to-cells))
				 (member (element-cell element) (element (gv win :cells) 'cell)))
			     (or (not (gv win :restrict-synapse-cxns-to-synapse-types))
				 (member (synapse-type (axon-target-synapse element)) (gv win :restrict-synapse-cxns-to-synapse-types))))))
	collect element))

(defun collect-locations-of-drawable-things (elements)
  (loop for element in elements
	nconcing (list (where element)) into locations
	when (and (axon-p element) (axon-target-synapse element)) nconcing (list (where (axon-target-synapse element))) into locations
	when (element-parameter element 'mid-points) nconcing (element-parameter element 'mid-points) into locations
	finally (return locations)))

(defun draw-axons (win &optional (draw-axons t) (type 'axons))
  (declare (optimize (safety 0) (speed 3) (space 1)))
  (unless (gv win :where-axons-go) (s-value win :where-axons-go :back))
  (let* ((axons (case type
		  (axons (collect-drawable-things-w-connections win 'axon))
		  (marked-axons (get-win-branch-elements-of-type win 'axon))))
	 (axon-agg (clear-and-add-plot-agg win type :add (and axons draw-axons)))
	 (axon-color (or (gv win :axon-color) (s-value win :axon-color 'red)))
	 (thickness (round (/ *axon-graphics-diameter* (gv win :scale)))))
    (when axon-agg
      (let ((line-styles (loop for color in (case type
					      (marked-axons (get-win-branch-element-colors-of-type win 'axon))
					      (t (collect-things-colors win axons axon-color)))			 
			       collect (access-*line-styles-array* thickness color (gv win :axon-color-shading))))
	    item-list)
	(when axons
	  (virtual-agg-finishing
	   (make-v-agg virtual-segment (get-things-w-connections-virtual-seg-item-values-array-list axons line-styles win) 'v-axons)
	   axon-agg (gv win :where-axons-go)))))))

(defun draw-synapse-connections (win &optional (draw-synapse-cxns t) (type 'synapse-cxns))
  (declare (optimize (safety 0) (speed 3) (space 1)))
  (unless (gv win :where-synapse-cxns-go) (s-value win :where-synapse-cxns-go :back))
  (let* ((synapse-cxns (case type
			 (synapse-cxns (collect-drawable-things-w-connections win 'synapse))
			 (marked-synapse-cxns (get-win-branch-elements-of-type win 'synapse-cxn))))
	 (synapse-cxn-agg (clear-and-add-plot-agg win type :add (and synapse-cxns draw-synapse-cxns)))
	 (synapse-cxn-color (or (gv win :synapse-cxn-color) (s-value win :synapse-cxn-color 'blue)))
	 (thickness (round (/ *synapse-cxn-graphics-diameter* (gv win :scale)))))
    (when synapse-cxn-agg
      (let ((line-styles (loop for color in (case type
					      (marked-synapse-cxns (get-win-branch-element-colors-of-type win 'synapse-cxn))
					      (t (collect-things-colors win synapse-cxns synapse-cxn-color)))
			       collect (access-*line-styles-array* thickness color (gv win :synapse-cxn-color-shading)))))
	(when synapse-cxns
	  (virtual-agg-finishing
	   (make-v-agg virtual-segment (get-things-w-connections-virtual-seg-item-values-array-list synapse-cxns line-styles win) 'v-synapse-cxns)
	   synapse-cxn-agg (gv win :where-synapse-cxns-go)))))))

(defun axon-coordinates (win)
  (loop for axon in (collect-drawable-things-w-connections win 'axon)
	nconc (loop for location in
		    (concatenate 'list
				 (list (where axon) (where (axon-target-synapse axon)))
				 (element-parameter axon 'mid-points))
		    collect (list (get-win-view-plane-x location win) (get-win-view-plane-y location win)))))

