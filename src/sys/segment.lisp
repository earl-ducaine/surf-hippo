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


;;; SYS Source file: segment.lisp

; the segment model, for approximating distributed RC lines

(in-package "SURF-HIPPO")

(defun color-segment (element color)
  ;; Assign a COLOR to the cell elements associated with ELEMENT, that will be used in plotting the circuit histology.
  (element-parameter element 'color color)
  nil)

(defun segment-rm (segment)
  "The membrane resistivity of SEGMENT, in ohms-cm2."
  (let* ((seg (if (segment-p segment) segment (element segment 'segment)))
	 (rm (unless (segment-inherit-parameters-from-type seg)
	       (get-a-value `rm (segment-parameters seg)))))
    (the sf (or rm (cell-type-rm-dendrite (cell-type (segment-cell seg)))))))

(defun segment-membrane-resistivity (segment) (segment-rm segment))

(defun segment-cm (segment)
  "The membrane specific capacitance of SEGMENT, in uF/cm2."
  (let* ((seg (if (segment-p segment) segment (element segment 'segment)))
	 (cm (unless (segment-inherit-parameters-from-type seg)
	       (get-a-value `cm (segment-parameters seg)))))
    (the sf (or cm (cell-type-cm-dendrite (cell-type (segment-cell seg)))))))

(defun segment-ri (segment)
  "The cytoplasmic resistivity of SEGMENT, in ohms-cm."
  (let* ((seg (if (segment-p segment) segment (element segment 'segment)))
	 (ri (unless (segment-inherit-parameters-from-type seg)
	       (get-a-value 'ri (segment-parameters seg)))))
    (the sf (or ri (cell-type-ri (cell-type (segment-cell seg)))))))
	      
(defun segment-v-leak (segment)
  "The reversal potential of the leak resistance of SEGMENT, in mV."
  (let* ((seg (if (segment-p segment) segment (element segment 'segment)))
	 (v-leak (unless (segment-inherit-parameters-from-type seg)
		   (get-a-value `v-leak (segment-parameters seg)))))
    (the sf (or v-leak (cell-type-v-leak-dendrite (cell-type (segment-cell seg)))))))
	      
(defun segment-ri-coefficient (segment)
  "The dimensionless coefficient for the cytoplasmic resistivity of SEGMENT."
  (let ((seg (if (segment-p segment) segment (element segment 'segment))))
    (s-flt (or (get-a-value `ri-coefficient (segment-parameters seg)) 1.0))))

(defun segment-relative-location (segment) (node-relative-location (segment-node-2 segment)))

(defun print-segment (&optional (seg *segment*))
  (let ((seg (element seg 'segment)))
    (format t "~a (~a, ~a):~A~%"
	    (MASSAGE-ELEMENT-PLOT-LABEL seg nil t)
	    (node-name (segment-node-1 seg))
	    (node-name (segment-node-2 seg))
	    (if *simulation-initialized* (format nil " Membrane voltage ~,2e mV @ ~,2e ms" (recorded-element-voltage seg) *real-time*) ""))
    (format t "    V-leak ~d mV; G-axial ~,2e, G-leak ~,2e uS; Cap ~,2e nF ~A~%    Length ~,2e, Diameter ~,2e um; Lateral area ~,2e um2, Volume ~,2e um3"
	    (s-flt (segment-v-leak seg)) (s-flt (segment-g-axial seg)) (s-flt (segment-g-leak seg)) 
	    (s-flt (segment-capacitance seg)) 
	    (if (segment-inherit-parameters-from-type seg) "(From Type)" "(Non-inherited)")
	    (s-flt (segment-length seg)) (s-flt (segment-diameter seg))
	    (s-flt (element-area seg)) (s-flt (element-volume seg)))
    (let ((ri-coefficient (element-parameter seg 'ri-coefficient)))
      (unless (or (not ri-coefficient) (= ri-coefficient 1.0)) (format t ", ri-coefficient ~,2e" (s-flt ri-coefficient))))
    (format t "~%    Cable electrotonic length ~,2e" (s-flt (segment-electrotonic-length seg)))
    (when (element-parameter seg 'membrane-area-coefficient)
      (format t "~%    Membrane area coefficient: ~,2e" (s-flt (element-parameter seg 'membrane-area-coefficient))))
    (when (element-parameter (cell-element-physical-node-fast seg) 'constant-current)
      (format t "~%    ~,2enA constant current injected into this segment." (s-flt (element-parameter (cell-element-physical-node-fast seg) 'constant-current))))
    (when (element-parameter seg 'electrode) (format t "  This segment is used as an electrode."))
    (when (element-parameter seg 'soma-segment) (format t "  This segment is used as part of a virtual soma."))
    (format t "~%")
    (let ((elt-string (PRINT-CELL-ELEMENT-ELEMENTS seg)))
      (when (> (length elt-string) 0) (format t "    This segment has ~A.~%" elt-string)))))

(defun print-segment-location (seg)
  (let ((seg (element seg 'segment)))
    (format t "Segment ~a (~a, ~a):~%" (segment-name seg) (node-name (segment-node-1 seg)) (node-name (segment-node-2 seg)))
    (format t " Proximal node @ [~,2f, ~,2f, ~,2f], distal node @ [~,2f, ~,2f, ~,2f]~%"
	    (nth 0 (node-absolute-location (segment-node-1 seg)))
	    (nth 1 (node-absolute-location (segment-node-1 seg)))
	    (nth 2 (node-absolute-location (segment-node-1 seg)))
	    (nth 0 (node-absolute-location (segment-node-2 seg)))
	    (nth 1 (node-absolute-location (segment-node-2 seg)))
	    (nth 2 (node-absolute-location (segment-node-2 seg))))))

(defun print-segment-relative-location (seg)
  (let ((seg (element seg 'segment)))
    (format t "Segment ~a (~a, ~a):~%" (segment-name seg) (node-name (segment-node-1 seg)) (node-name (segment-node-2 seg)))
    (format t " Proximal node @ [~,2f, ~,2f, ~,2f], distal node @ [~,2f, ~,2f, ~,2f]~%"
	    (nth 0 (node-relative-location (segment-node-1 seg)))
	    (nth 1 (node-relative-location (segment-node-1 seg)))
	    (nth 2 (node-relative-location (segment-node-1 seg)))
	    (nth 0 (node-relative-location (segment-node-2 seg)))
	    (nth 1 (node-relative-location (segment-node-2 seg)))
	    (nth 2 (node-relative-location (segment-node-2 seg))))))

(defun get-cell-element-simple-name ()
  (loop for candidate from (max 1 *cell-element-simple-name-counter*)
	until (not (or (soma-hash-table candidate) (segment-hash-table candidate) (node-hash-table candidate)))
	finally (return (setf *cell-element-simple-name-counter* candidate))))

(defun find-segment-name (segment-name cell)
  (or ; (and (segment-hash-table segment-name) (segment-name (segment-hash-table segment-name)))
      (cdr (assoc segment-name (element-parameter cell 'name-map) :test 'equal))))

(defvar *suppress-cell-element-simple-names* nil)

(defun make-segment-name (segment-name cell &key dont-check)
  (let ((cell (element cell 'cell)))
    (or (find-segment-name segment-name cell)
	(let* ((segment-name (or segment-name (gensym)))
	       synthesized-name
	       (name (cond ((and (not *suppress-cell-element-simple-names*) *use-simple-names*)
			    (setq synthesized-name t)
			    (get-cell-element-simple-name))
			   (*add-cell-name-to-segs*
			    (setq synthesized-name t)
			    (ADD-CELL-NAME-TO-NAME segment-name (cell-name cell)))
			   ((soma-hash-table segment-name)
			    (setq synthesized-name t)
			    (format nil "~A-seg" segment-name))
			   (t (if (stringp segment-name)
				  (if (string-has-non-number segment-name)
				      segment-name
				      (progn (setq synthesized-name t) (read-from-string segment-name)))
				  segment-name)))))
	  (when synthesized-name (element-parameter cell 'name-map (acons segment-name name (element-parameter cell 'name-map))))
	  (unless dont-check
	    (when (segment-hash-table name)
	      (sim-error (concatenate 'string
				      (format nil "~A already used by a segment in cell ~A!" name (segment-cell (segment-hash-table name)))
				      (unless *add-cell-name-to-segs* (format nil "~%  Try setting *ADD-CELL-NAME-TO-SEGS* to T."))))))
	  name))))

(defun create-segment (name proximal-element &optional cell
			    &key (diameter 0.0) (length 0.0) (theta 0.0) (phi (* -0.5 pi-single))
			    (relative-location '(0.0 0.0 0.0)) relative-location-is-float absolute-location absolute-location-is-float 
			    dummy-proximal-element-location dummy-proximal-element-location-is-float
			    (ri-coefficient 1.0) parameter-a-list)
  "Returns a segment with NAME that is attached to the soma, segment or node PROXIMAL-ELEMENT. DIAMETER and LENGTH are in
microns. The location of the distal node relative to the soma of CELL [if not given, derived from PROXIMAL-ELEMENT] is given by
the XYZ values [microns] in the list RELATIVE-LOCATION. Alternatively, if PROXIMAL-ELEMENT is a segment, the location can be
defined relative to the orientation of the proximal segment by THETA and PHI, each in radians."
  (declare (optimize (safety 0) (speed 3) (space 1))) 
  (let* ((cell (or (element cell 'cell) (element-cell proximal-element)
		   (sim-error (format nil "create-segment: segment ~a needs a cell reference!" name))))
	 *print-pretty*
	 (segment-name (make-segment-name name cell)))
    (or
     (SEGMENT-HASH-TABLE segment-name)
     (let* ((n1 (typecase proximal-element
		  (node proximal-element)
		  ((or segment soma) (element-physical-node proximal-element))
		  (t (cond ((soma-hash-table proximal-element) (soma-node (soma-hash-table proximal-element)))
			   (t (let* ((proximal-element-name (make-segment-name proximal-element cell :dont-check t))
				     (previous-node (node-hash-table proximal-element-name)))
				(or previous-node (create-node proximal-element-name :cell cell :is-physical-cell-node t))))))))
	    (n2 (or (node-hash-table segment-name) (create-node segment-name :cell cell :is-physical-cell-node t)))
	    (ri-coefficient (s-flt ri-coefficient))
	    (cell-type (cell-type cell)))
       (cond-every
	((and relative-location (not relative-location-is-float)) (setq relative-location (s-flt-list relative-location)))
	((and absolute-location (not absolute-location-is-float)) (setq absolute-location (s-flt-list absolute-location)))
	((and dummy-proximal-element-location (not dummy-proximal-element-location-is-float))
	 (setq dummy-proximal-element-location (s-flt-list dummy-proximal-element-location))))
       (if (eq n1 n2)
	 (sim-error (format nil "create-segment: duplicate nodes for segment ~a" segment-name))
	 (let ((seg (make-segment :name segment-name :theta (s-flt theta) :phi (s-flt phi)
				  :length (s-flt length) :diameter (s-flt diameter) :node-1 n1 :node-2 n2
				  :parameters parameter-a-list :inherit-parameters-from-type (cell-type-inherit-parameters-from-type cell-type)
				  :dummy-proximal-node-location dummy-proximal-element-location)))
	   (setq *circuit-processed* nil)
	   (setf (node-relative-location n2) relative-location
		 (node-elements n1) (cons seg (node-elements n1))
		 (node-elements n2) (cons seg (node-elements n2))
		 (SEGMENT-HASH-TABLE segment-name) seg)
	   (unless (= 1.0 (the sf ri-coefficient)) (element-parameter seg 'ri-coefficient ri-coefficient)) 
	   (when absolute-location (setf (node-absolute-location n2) absolute-location))
	   (setq *make-segment-lists* t)
	   (push seg (cell-segments cell))
	   (setq *segment* seg)
	   (initialize-segment-voltage seg)
	   seg))))))

(proclaim '(inline create-segment-fast))
(defun create-segment-fast (name proximal-element &optional cell
				 &key (diameter 0.0) (length 0.0) (theta 0.0) (phi (* -0.5 pi-single))
				 (relative-location '(0.0 0.0 0.0)) absolute-location dummy-proximal-element-location 
				 parameter-a-list (ri-coefficient 1.0))
  "An optimized version of CREATE-SEGMENT. All numeric arguments are assumed to be single floats, except if NAME is a number, in
which case it must be an integer."
  (declare (optimize (safety 0) (speed 3) (space 1))
	   (single-float diameter length theta phi ri-coefficient))
  (let* (*print-pretty*
	 (cell (if (cell-p cell) cell (element-cell proximal-element)))
	 (cell-name (cell-name cell))
	 (segment-name (make-segment-name name cell-name)))
    (or (SEGMENT-HASH-TABLE segment-name)
	(let* ((n1 (typecase proximal-element
		     (node proximal-element)
		     ((or segment soma) (element-physical-node proximal-element))
		     (t (cond ((gethash proximal-element (soma-hash-table)) (soma-node (gethash proximal-element (soma-hash-table))))
			      (t (let* ((proximal-element-name (make-segment-name proximal-element cell))
					(previous-node (gethash proximal-element-name (node-hash-table))))
				   (or previous-node (create-node proximal-element-name :cell cell :is-physical-cell-node t))))))))
	       (n2 (or (gethash segment-name (node-hash-table)) (create-node segment-name :cell cell :is-physical-cell-node t)))
	       (cell-type (cell-type cell)))
	  (if (eq n1 n2)
	    (sim-error (format nil "create-segment-fast: duplicate nodes for segment ~a" segment-name))
	    (let ((seg (make-segment :name segment-name :node-1 n1 :node-2 n2 :parameters parameter-a-list
				     :inherit-parameters-from-type (cell-type-inherit-parameters-from-type cell-type)
				     :dummy-proximal-node-location dummy-proximal-element-location)))
	      (setf (segment-length seg) length
		    (segment-diameter seg) diameter
		    (segment-theta seg) theta
		    (segment-phi seg) phi)
	      (setq *circuit-processed* nil)
	      (setf (node-relative-location n2) relative-location
		    (node-elements n1) (cons seg (node-elements n1))
		    (node-elements n2) (cons seg (node-elements n2))
		    (SEGMENT-HASH-TABLE segment-name) seg)
	      (unless (= 1.0 (the sf ri-coefficient)) (element-parameter seg 'ri-coefficient ri-coefficient))
	      (when absolute-location (setf (node-absolute-location n2) absolute-location))
	      (setq *make-segment-lists* t)
	      (push seg (cell-segments cell))
	      (setq *segment* seg)
	      (initialize-segment-voltage seg)
	      seg))))))

(defun set-segment-voltage (segment voltage) (set-node-voltage (segment-node-2 segment) voltage))
(defun set-segment-voltage-df (segment voltage) (set-node-voltage-double (segment-node-2 segment) voltage))

(defun initialize-segment-voltage (seg &optional v-leak)
  (let ((v-leak (or v-leak (cell-type-v-leak-dendrite (cell-type (segment-cell seg))))))
    (set-segment-voltage seg (or (element-holding-potential seg)
				 (if (= (segment-v-leak seg) v-leak) v-leak (d-flt (segment-v-leak seg)))))))

(defun set-segments-membrane-parameters (&optional ignore-membrane-elements cell-type)
  (loop for seg being the hash-value of (SEGMENT-HASH-TABLE)
	when (or (not cell-type) (eq cell-type (cell-type (segment-cell seg))))
	do (set-segment-membrane-parameters seg ignore-membrane-elements)))

(defun set-segment-membrane-parameters (seg &optional ignore-membrane-elements)
  ;; Set SEG properties that depend on segment dimensions, and others.
  (declare (optimize (safety 0) (speed 3) (space 1)))
  (let* ((proximal-cell-element (PROXIMAL-CELL-ELEMENT seg))
	 (membrane-factor (the sf (or (get-a-value 'membrane-area-coefficient (segment-parameters seg)) 1.0)))
	 (vl (the sf (segment-v-leak seg)))
	 (cm (the sf (segment-cm seg)))
	 (rm (the sf (segment-rm seg)))
	 (ra (the sf (segment-ri seg)))
	 (this-g-axial (the sf (g-axial (segment-length seg) (segment-diameter seg) (the sf (* ra (segment-ri-coefficient seg))))))
	 (g-axial-computation (or (get-a-value 'g-axial-computation (segment-parameters seg))
				  (get-a-value 'g-axial-computation (cell-type-parameters (cell-type (segment-cell seg))))
				  :single-leg))
	 (need-prox-info (case g-axial-computation
			   (:single-leg nil)
			   (t t)))
	 (proximal-seg (when (and need-prox-info (segment-p proximal-cell-element)) proximal-cell-element))
	 (prox-g-axial
	  (when need-prox-info
	    (cond (proximal-seg
		   (g-axial (segment-length proximal-seg) (segment-diameter proximal-seg)
			    (* (segment-ri proximal-seg) (segment-ri-coefficient proximal-seg))))
		  (t (the sf (or (and (get-a-value 'soma-cylinder (soma-parameters proximal-cell-element))
				      (get-a-value 'soma-cylinder-g-axial (soma-parameters proximal-cell-element)))
				 0.0)))))))
    (setf (segment-g-axial seg)
	  (case g-axial-computation
	    (:average-g (/ (+ (the sf prox-g-axial) this-g-axial) 2.0d0))
	    (:average-r (if (= 0.0 (the sf prox-g-axial)) (coerce this-g-axial 'double-float) (/ 2.0d0 (+ (/ 1.0 prox-g-axial) (/ 1.0 this-g-axial)))))
	    (t (coerce this-g-axial 'double-float))))
    (setf (segment-g-leak seg) (d-flt (g-leak-mem (segment-length seg) (segment-diameter seg) (/ rm membrane-factor))))
    (setf (segment-capacitance seg) (d-flt (cap-mem (segment-length seg) (segment-diameter seg) (* cm membrane-factor))))
    (setf (segment-guts-g-leak*v-leak (segment-guts seg)) (* (segment-g-leak seg) vl))
    (unless ignore-membrane-elements
      (loop for elt in (node-elements (segment-node-2 seg)) unless (segment-p elt) do (set-element-membrane-parameters elt t))))
  nil)

(defun set-segments-inherit-parameters-from-type (&optional cell)
  "Makes all segments in CELL (if supplied) or in the circuit (else) inherit their properties from the associated CELL-TYPE."
  (loop for seg in (if cell (segments cell) (segments)) do (setf (segment-inherit-parameters-from-type seg) t)))

(defun set-segment-absolute-parameters (seg capacitance g-axial g-leak)
  "Set cable properties of SEG to the absolute values of CAPACITANCE [nF], G-AXIAL and G-LEAK [uS]. Sets
:INHERIT-PARAMETERS-FROM-TYPE of SEG to NIL. If any of the segment parameter arguments are NIL, then the original value is
retained."
  (loop for seg in (coerce-to-list (element seg 'segment)) do
	(cond-every (capacitance (setf (segment-capacitance seg) (d-flt capacitance)))
		    (g-axial (setf (segment-g-axial seg) (d-flt g-axial)))
		    (g-leak (setf (segment-g-leak seg) (d-flt g-leak))))
	(setf (segment-inherit-parameters-from-type seg) nil)
	(element-parameter seg 'g-axial-computation nil) ; default is :single-leg
	(when (element-parameter seg 'ri-coefficient) (element-parameter seg 'ri-coefficient 1.0))
	;; Adjust local membrane parameters to reflect absolute value assignment.
	(element-parameter seg 'rm (s-flt (* 1e6 (/ (element-area-cm2 seg) (segment-g-leak seg)))))
	;; (* 1.0e6 pi-single 1.0e-4 0.25) = 78.53982
	(element-parameter seg 'ri (s-flt (/ (* 78.53982 (segment-diameter seg) (segment-diameter seg))
					     (* (segment-length seg) (segment-g-axial seg)))))
	(element-parameter seg 'cm (s-flt (* 1e-3 (/ (segment-capacitance seg)
						     (element-area-cm2 seg)))))))

(defun init-segments ()
  (declare (optimize (safety 0) (speed 3) (space 1)))
  (loop for seg being the hash-value of (SEGMENT-HASH-TABLE) do
	(setf (segment-guts-g-leak*v-leak (segment-guts seg)) (* (segment-g-leak seg) (segment-v-leak seg)))))

(defun set-segment-parameter (seg parameter value)
  "Set a PARAMETER distinct from the associated cell type for segments associated with SEG, for example including 'RM, 'CM,
'V-LEAK, 'RI, or 'RI-COEFFICIENT. Sets :INHERIT-PARAMETERS-FROM-TYPE for SEG to NIL."
  (let ((value (s-flt value)))
    (loop for seg in (coerce-to-list (element seg 'segment)) do
	  (setf (segment-inherit-parameters-from-type seg) nil)
	  (element-parameter seg parameter value)
	  (set-segment-membrane-parameters seg))))

(defun rename-segments-simple (&optional segments)
  "Rename SEGMENTS [default all segments in circuit] with simple integer names."
  (loop for seg in (coerce-to-list (or segments (segments))) do
	(let ((name (get-cell-element-simple-name)))
	  (set-element-name seg name 'segment)
	  (set-element-name (segment-node-2 seg) name 'node))))

(defun edit-segment (seg)
  (let ((seg (element seg 'segment)))
    (if (electrode-p seg)
      (edit-electrode seg)
      (let* ((dummy1 (segment-rm seg))
	     (dummy2 (segment-cm seg))
	     (dummy4 (segment-ri seg))
	     (dummy6 (segment-v-leak seg))	  
	     (dummy3 (segment-inherit-parameters-from-type seg))
	     (dummy7 (segment-ri-coefficient seg))
	     (dummy8 (or (element-parameter seg 'membrane-area-coefficient) 1.0))
	     dummy9 dummy5
	     (cell-type-g-axial-computation (or (element-parameter (cell-type (segment-cell seg)) 'g-axial-computation) :single-leg))
	     (dummy10 (or (element-parameter seg 'g-axial-computation) cell-type-g-axial-computation)))
	(choose-variable-values
	 `((dummy3 "Ignore values listed below, and inherit from cell type" :boolean)
	   (dummy1 "Dendrite segment membrane resistivity [ohm-cm-sq]" :float) ;
	   (dummy2 "Dendrite segment specific capacitance [uF/cm-sq]" :float)
	   (dummy4 "Dendrite segment cytoplasmic resistivity [ohm-cm] (>0)" :float)
	   (dummy6 "Dendrite segment leak battery [mV]" :float) ;
	   (dummy7 "Cytoplasmic resistivity coefficient [1.0 => no effect]" :float)
	   (dummy8 "Membrane area coefficient" :float)
	   (,(format nil "Single leg method considers only this ~A's properties for calculating g-axial." (if (electrode-p seg) "electrode" "segment"))
	    :comment)
	   (dummy10 ,(format nil "Method for computing compartment coupling (g-axial)~%(Cell type ~A method is ~A):"
			     (cell-type-name (cell-type (segment-cell seg))) cell-type-g-axial-computation)
		    :choose (:single-leg :average-r :average-g))
	   (dummy9 "Edit segment absolute values (when not inheriting params from cell type)" :boolean) 
	   (dummy5 "Edit cell type" :boolean))
	 :label (format nil "Edit ~A ~a" (if (electrode-p seg) "Electrode" "Segment") (segment-name seg)))
	(clear-z-cable-in-to-soma seg)
	(when (cond ((not (element-parameter seg 'membrane-area-coefficient))
		     (not (= dummy8 1.0)))
		    ((not (= (element-parameter seg 'membrane-area-coefficient) dummy8))))
	  (element-parameter seg 'membrane-area-coefficient dummy8))
	(when (cond ((not (element-parameter seg 'ri-coefficient))
		     (not (= dummy7 1.0)))
		    ((not (= (element-parameter seg 'ri-coefficient) dummy7))))
	  (element-parameter seg 'ri-coefficient dummy7))
	(element-parameter seg 'g-axial-computation dummy10)    
	(unless dummy3
	  (element-parameter seg 'ri dummy4)
	  (element-parameter seg 'rm dummy1)
	  (element-parameter seg 'cm dummy2)
	  (element-parameter seg 'v-leak dummy6))
	(setf (segment-inherit-parameters-from-type seg) dummy3)
	(when dummy9 (edit-segment-absolute seg))
	(when dummy5 (edit-cell-type (cell-type (segment-cell seg))))
	(unless dummy9
	  (set-segment-membrane-parameters seg)
	  (update-linear-z-in (segment-cell seg)))))))

(defun edit-segment-absolute (seg &optional (text "") (use-g-axial-p t))
  (let* ((seg (element seg 'segment))
	 (dummy1 (s-flt (segment-capacitance seg)))
	 (dummy2 (s-flt (if use-g-axial-p (segment-g-axial seg) (/ 1.0 (segment-g-axial seg)))))
	 (dummy3 (s-flt (segment-g-leak seg)))
	 (dummy4 (segment-inherit-parameters-from-type seg))
	 (cell-type-g-axial-computation (or (element-parameter (cell-type (segment-cell seg)) 'g-axial-computation) :single-leg))
	 (dummy10 (or (element-parameter seg 'g-axial-computation) cell-type-g-axial-computation)))
    (choose-variable-values
     `((dummy4 "Ignore values listed below, and inherit from cell type" :boolean)
       (dummy1 "Absolute capacitance [nF]" :float)
       (dummy2 ,(if use-g-axial-p "Absolute axial conductance [uS]" "Absolute axial resistance [Mohms, must be >0]") :float)
       (dummy3 "Absolute membrane conductance [uS]" :float)
       (,(format nil "Single leg method considers only this ~A's properties for calculating g-axial." (if (electrode-p seg) "electrode" "segment"))
	:comment)
       (dummy10	,(format nil "Method for computing compartment coupling (g-axial)~%(Cell type ~A method is ~A):"
			 (cell-type-name (cell-type (segment-cell seg))) cell-type-g-axial-computation)
		:choose (:single-leg :average-r :average-g)))
     :text text :label (format nil "~A ~a: Edit Absolute Linear Parameters" (if (electrode-p seg) "Electrode" "Segment") (segment-name seg)))
    (setf (segment-inherit-parameters-from-type seg) dummy4)
    (unless dummy4
      (clear-z-cable-in-to-soma seg)
      (element-parameter seg 'g-axial-computation nil)
      (unless (eq dummy10 :single-leg) (element-parameter seg 'g-axial-computation dummy10))
      (set-segment-absolute-parameters seg dummy1 (if use-g-axial-p dummy2 (/ 1.0 dummy2)) dummy3)
      (set-segment-membrane-parameters seg)
      (update-linear-z-in (segment-cell seg))))
  nil)

(defun edit-segments-of-type (&optional (type nil type-supplied-p))
  (let ((type (element type 'cell-type)))
    (when (or (not type-supplied-p) type)
      (loop for seg in 
	    (choose-list-values
	     (loop for seg in (segments) when (or (not type-supplied-p) (eq type (cell-type (segment-cell seg)))) collect (segment-name seg))
	     nil :max-per-menu 20 :label (format nil "Choose Segment of Type ~A To Modify" (cell-type-name type)))
	    do (edit-segment (element seg 'segment))))))

(defun menu-for-dendrites ()
  (process-circuit-structure)
  (let (dummy1 dummy2)
    (choose-variable-values
     '((dummy2 "Edit individual segments" :boolean)
       (dummy1  "Examine/modify distribution of electrotonic lengths" :boolean))
     :label "Edit dendrites")
    (cond-every
     (dummy2 (loop for type in
		   (select-hash-values-menu (CELL-TYPE-HASH-TABLE) "Select Cell Types To Modify Segments" :punt-if-only-one-entry t :inclusion-key 'cell-type-cells)
		   do (EDIT-SEGMENTS-OF-TYPE type)))
     (dummy1 (consolidate-cells-tree)))
    nil))

(defun print-all-segment-nodes () (maphash 'print-segment-nodes (SEGMENT-HASH-TABLE)))

(defun print-segment-nodes (name seg)
  (format t "Segt ~a: " name)
  (format t "node1 ~a @~A" (node-name (segment-node-1 seg)) (node-relative-location (segment-node-1 seg)))
  (format t " node2 ~a @~A~%" (node-name (segment-node-2 seg)) (node-relative-location (segment-node-2 seg))))

(defun print-node-segments (name nd)
  (format t "Node ~a: " name)
  (loop for element in (node-elements nd) when (eq (named-structure-symbol element) 'segment) do (format t " ~a " (segment-name element))))

(defun print-virtually-connected-segments ()
  (loop for seg in (segments) when (segment-dummy-proximal-node-location seg) do (print-element seg))) 

(defun loop-check (&optional exclude-segments)
 ;; Find any loops in the circuit trees by sucessive calls to SEGMENTS-OUT. If a loop is found, then SEGMENTS-OUT signals an error.
  (loop for seg being the hash-value of (SEGMENT-HASH-TABLE) 
	unless (or (loop for tested-seg-list in tested-seg-lists thereis (loop for tested-seg in tested-seg-list thereis (eq seg tested-seg)))
		   (loop for exclude in exclude-segments thereis (eq seg (element exclude))))
	collect (segments-out seg 0) into tested-seg-lists))

(defun connect-loose-segments (&optional cell)
  ;; Finds all segments which are not connected to anything by their proximal node. Attaches them electrically to the segment or
  ;; soma which is closest to the proximal node location. If the original proximal node does not have anything else attached to
  ;; it, then it is destroyed.
  (loop for cell in (or (and cell (list cell)) (cells)) do
	(let ((candidates (cell-segments cell)))
	  (move-relative-to-absolute cell)
	  (loop for seg in (loose-segments cell) do
		;; (format t "connecting loose ~A~%" seg)
		(setf (segment-dummy-proximal-node-location seg) (node-relative-location (segment-node-1 seg)))
		(move-proximal-node seg (closest-element seg :exclude-these-elements (segments-out seg) :proximal-measure t :candidates candidates))))))

(defun move-relative-to-absolute (&optional cell)
  (loop for cell in (or (and cell (list cell)) (cells)) do
	(setf (node-absolute-location (soma-node (cell-soma cell)))
	      (node-relative-location (soma-node (cell-soma cell))))
	(loop for seg in (cell-segments cell) do
	      (setf (node-absolute-location (segment-node-1 seg)) (node-relative-location (segment-node-1 seg))
		    (node-absolute-location (segment-node-2 seg)) (node-relative-location (segment-node-2 seg))))))

(defun loose-segments (&optional cell)
  ;; Returns all segments which are not connected to anything by their proximal node.
  (flatten-no-nils-list
   (loop for cell in (or (and cell (list cell)) (cells))
	 collect (loop for seg in (cell-segments cell)
		       unless (or (eq (segment-node-1 seg) (soma-node (cell-soma cell))) (proximal-segment seg))
		       collect seg))))

(defun clean-up-cell-segments (&optional cells)
  (loop for cell in (or cells (cells)) do
	(setf (cell-segments cell) (loop for seg in (cell-segments cell) when (SEGMENT-HASH-TABLE (segment-name seg)) collect seg))))

(defun move-proximal-node (segment new-proximal-cell-element)
  ;; Move the proximal end of SEGMENT and attach it to NEW-PROXIMAL-CELL-ELEMENT. If the original proximal node does not have
  ;; anything else attached to it, then it is destroyed.
  (let* ((segment (element segment))
	 (new-proximal-cell-element (element new-proximal-cell-element))
	 (old-proximal-node (segment-node-1 segment)))
    (push segment (node-elements (element-physical-node new-proximal-cell-element)))
    (setf (segment-node-1 segment) (element-physical-node new-proximal-cell-element)
	  (node-elements old-proximal-node) (remove segment (node-elements old-proximal-node)))
    (unless (node-elements old-proximal-node) (erase-element old-proximal-node))))

(defun erase-branch (proximal-segment &optional (top-level t))
  (let* ((proximal-segment (element proximal-segment 'segment))
	 (distal-segments (distal-segments proximal-segment)))
    (erase-element proximal-segment 'segment nil)
    (loop for seg in distal-segments do (erase-branch seg nil))
    (when top-level (CLEAN-UP-CELL-SEGMENTS) (process-circuit-structure t))
    nil))

(defun erase-branch (proximal-segment)
  (erase-element proximal-segment 'segment nil)
  (CLEAN-UP-CELL-SEGMENTS))

(defun locate-all-nodes ()
  ;; Complete the geometrical description of the cells' nodes, after the circuit has been loaded.
  (setf *branch-list* '())
  (loop for cell being the hash-value of (CELL-HASH-TABLE) do (locate-cell-nodes cell t))
  (maphash 'set-cells-coordinate-extrema (NODE-HASH-TABLE))
  (loop for cell being the hash-value of (CELL-HASH-TABLE) do (UPDATE-CELL-AREA cell)))

(defun locate-unconnected-nodes (&optional cell)
  (loop for cell in (or (and cell (list cell)) (cells))	do
	(loop for seg in (cell-segments cell)
	      unless (or (eq (segment-node-1 seg) (soma-node (cell-soma cell))) (proximal-segment seg))
	      do
	      (locate-distal-node seg)
	      (build-branch-list seg)
	      (get-and-locate-distal-segments seg))))

(proclaim '(notinline compute-segment-length))
(defun compute-segment-length (one-end other-end)
  (declare (optimize (safety 0) (speed 3) (space 1))
	   (type cons one-end other-end))
  (do* ((num-1 one-end (cdr num-1))
	(num-2 other-end (cdr num-2))
	(diff (- (the sf (car num-2)) (the sf (car num-1)))
	      (- (the sf (car num-2)) (the sf (car num-1))))
	(sum (* diff diff)
	     (+ (* diff diff)
		(the sf sum))))
      ((null num-1) (the sf (if (plusp sum) (sqrt (the sf sum)) 0.0)))))

(defun locate-cell-nodes (cell &optional make-branches look-for-loops)
  ;; Cell nodes are located seqentially from the soma outward.
  (setf (node-absolute-location (soma-node (cell-soma cell))) ; Make sure soma is located first.
        (mapcar '+ (cell-origin cell) (node-relative-location (soma-node (cell-soma cell)))))
  ;; Now start at the cell soma, and work on each of the branches that originate there in turn.
  (loop for seg in (node-segments (cell-soma cell)) do
	(locate-distal-node seg)
	(when make-branches (build-branch-list seg))
	(get-and-locate-distal-segments seg nil make-branches look-for-loops)))

(defun distal-tip-p (element)
  "Predicate for whether cell element associated with ELEMENT is located on a distal tip of the dendritic tree."
  (= (length (distal-segments element)) 0))

#|
(defun distal-segments (element &optional include-electrodes)
  "Returns a list of all the segments directly attached to the distal node of segment associated with ELEMENT, or the trunk
segments if ELEMENT is associated with the soma. Electrode segments are not included, unless INCLUDE-ELECTRODES is T [default
NIL]."
  (let ((cell-element (element-cell-element element)))
    (typecase cell-element
      (soma (trunk-segments cell-element))
      (segment
       (loop for element in (node-elements (segment-node-2 cell-element))
	     ;; Collect all segments attached to this one's distal node.
	     when (and (segment-p element) ; Just want segments
		       (or include-electrodes (not (electrode-p element t)))
		       (not (eq cell-element element))) ; Avoid parent segment.
	     collect element)))))
|#
(defun distal-segments (element &optional include-electrodes)
  "Returns a list of all the segments directly attached to the distal node of segment associated with ELEMENT. Electrode segments are not included, unless
INCLUDE-ELECTRODES is T [default NIL]."
  (let* ((cell-element (element-cell-element element))
	 (cell-element-node (element-node cell-element)))
    (loop for element in (node-elements cell-element-node)
	  ;; Collect all segments attached to this one's distal node.
	  when (and (segment-p element) ; Just want segments
		    (or include-electrodes (not (electrode-p element t)))
		    (not (eq cell-element element))) ; Avoid parent segment.
	  collect element)))

(defun SEGS-UNTIL-BIFURCATION (seg)
  "Given a segment, returns all distal segments, including this one, before the next branch point in the tree."
  (let ((distal-segs (distal-segments seg)))
    (if (= 1 (length distal-segs))
	(cons seg (segs-until-bifurcation (car distal-segs)))
	(list seg))))

(defun get-and-locate-distal-segments (segment &optional originals make-branches look-for-loops)
  (declare (optimize (safety 1) (speed 3) (space 1)))
  ;; (printvars segment originals make-branches look-for-loops)
  (loop for distal-segment in (distal-segments segment t)
	when (member distal-segment originals :test 'equal) do (format t "Found loop at ~A!!!~%" distal-segment) (break)
	do
	(locate-distal-node distal-segment segment)
	(when make-branches (build-branch-list distal-segment))
	(get-and-locate-distal-segments
	 distal-segment (when look-for-loops (cons distal-segment originals)) make-branches look-for-loops)))

(defun locate-distal-node (seg &optional proximal-segment)
  ;; This function is called under the assumption that the location of the proximal node (the segment :NODE-1) has already been
  ;; determined. This means that segments' node locations must be determined from the soma OUT. Segment locations are specified in
  ;; one of two ways: 1] in terms of segment length and the phi and theta parameters relative to the proximal segment, and 2] in
  ;; term of the coordinates of their nodes, relative to the soma. In the first case a segment's distal node :RELATIVE-LOCATION
  ;; will be equal to '(0.0 0.0 0.0); in the second case this list will have a nonzero coordinate.  When LOCATE-DISTAL-NODE is
  ;; finished, the segment distal nodes relative and absolute locations and its length will be set.
  (declare (optimize (safety 1) (speed 3) (space 1)))
  (let* ((cell (segment-cell seg))
	 (distal-node (segment-node-2 seg))
	 (relative-location (the cons (node-relative-location distal-node)))
	 (proximal-node (segment-node-1 seg))
	 proximal-segment-end
	 (phi-distal 0.0) (theta-distal 0.0)
	 (length (the sf (segment-length seg))))
    (declare (single-float phi-distal theta-distal length))
    ;; (format t "Proxi of seg ~a is ~a ~%" (segment-name seg) (if proximal-segment (segment-name proximal-segment)))
    (cond (proximal-segment		; If no proximal segment, then this segment comes from soma.
	   (setq proximal-segment-end (node-relative-location proximal-node))
	   (if (equal relative-location '(0.0 0.0 0.0))
	     (let* ((proximal-segment-proximal-node (segment-node-1 proximal-segment))
		    (proximal-segment-start (node-relative-location proximal-segment-proximal-node))
		    (proximal-segment-length (segment-length proximal-segment)))
	       (declare (single-float proximal-segment-length))
	       (setq phi-distal (acos (the sf
					;; The bounding between 1 and -1 is for any roundoff errors busting the ACOS.
					(bound-val (/ (- (the sf (third proximal-segment-end)) (the sf (third proximal-segment-start)))
						      proximal-segment-length) 1.0 -1.0))))
	       (setq theta-distal (if (= (the sf (first proximal-segment-end)) (the sf (first proximal-segment-start)))
				    (let ((dif (- (the sf (second proximal-segment-end))
						  (the sf (second proximal-segment-start)))))
				      (cond ((= 0.0 dif) 0.0)
					    ((> dif 0.0) pi-over-2)
					    (t (- pi-over-2))))
				    (the sf (atan (- (the sf (second proximal-segment-end)) (the sf (second proximal-segment-start)))
						  (- (the sf (first proximal-segment-end))  (the sf (first proximal-segment-start)))))))
	       )
	     (setf (segment-length seg) (compute-segment-length proximal-segment-end relative-location))))
	  ;; Branches originating from soma are assumed to be coming out from "branches" that lie in the xy plane.
	  (t (setq proximal-segment-end '(0.0 0.0 0.0))
	     (if (equal relative-location '(0.0 0.0 0.0))
	       (setq phi-distal pi-over-2 theta-distal 0.0)
	       (setf (segment-length seg) (compute-segment-length (node-relative-location distal-node) (or (segment-dummy-proximal-node-location seg)
													   (node-relative-location proximal-node)))))))
    ;; See if relative location of distal node has been calculated - if not, do so.

    (when (equal relative-location '(0.0 0.0 0.0))
      (let ((theta (segment-theta seg))
	    (phi (segment-phi seg)))
	;; (format t "~A~%" (+ (* length (sin (+ phi phi-distal)) (cos (+ theta theta-distal))) (the sf (first proximal-segment-end))))
	(setf (node-relative-location distal-node)
	      (list (+ (* length (sin (+ phi phi-distal)) (cos (+ theta theta-distal)))
		       (the sf (first proximal-segment-end)))
		    (+ (* length (sin (+ phi phi-distal)) (sin (+ theta theta-distal)))
		       (the sf (second proximal-segment-end)))
		    (+ (* length (cos (+ phi phi-distal)))
		       (the sf (third proximal-segment-end)))))))
    (when *calculate-absolute-locations*
      (setf (node-absolute-location distal-node)
	    (if (equal (cell-origin cell) '(0.0 0.0 0.0))
	      (node-relative-location distal-node)
	      (list (+ (the sf (first (cell-origin cell)))
		       (the sf (first (node-relative-location distal-node))))
		    (+ (the sf (second (cell-origin cell)))
		       (the sf (second (node-relative-location distal-node))))
		    (+ (the sf (third (cell-origin cell)))
		       (the sf (third (node-relative-location distal-node))))))))
    nil))

(defun proximal-cell-element (elt)
  "Returns the proximal cell element (segment or soma) associated with the cell element of ELT. If ELT is on the soma, then the
soma is returned."
  (let ((cell-elt (element-cell-element elt)))
    (typecase cell-elt
      (soma cell-elt)
      (segment (or (proximal-segment cell-elt)
		   (loop for elt in (node-elements (segment-node-1 cell-elt))
			 when (soma-p elt) do (return elt)))))))

(defun proximal-segment (elt)
  "Returns the proximal segment associated with the cell element of ELT, if there is one."
  (declare (optimize (safety 0) (speed 3) (space 1)))
  (let ((cell-elt (element-cell-element elt)))
    (typecase cell-elt
      (soma nil)
      (segment (loop for element in (node-elements (segment-node-1 cell-elt))
		     when (and (segment-p element)
			       (not (electrode-p element))
			       (eq (segment-node-1 cell-elt) (segment-node-2 element)))
		     do (return element))))))

(defun attached-to-soma-p (element)
  "True if ELEMENT is either a soma, a membrane element of a soma, or a trunk segment."
  (let ((element element))
    (typecase element
      (soma t)
      (segment (not (proximal-segment element)))
      (t (soma-p (proximal-cell-element element))))))

(defun find-proximal-segment (seg) (proximal-segment seg))

(defun set-cells-coordinate-extrema (name node)
  ;; Update the cells' extreme coordinate values.
  (declare (optimize (safety 0) (speed 3) (space 1))
	   (ignore name))
  (unless (equal node *ground-node*)
    (setf (cell-max-x (node-cell node)) (max (the sf (cell-max-x (node-cell node)))
					     (the sf (first (node-absolute-location node))))
	  (cell-max-y (node-cell node)) (max (the sf (cell-max-y (node-cell node)))
					     (the sf (second (node-absolute-location node))))
	  (cell-max-z (node-cell node)) (max (the sf (cell-max-z (node-cell node)))
					     (the sf (third (node-absolute-location node))))
	  (cell-min-x (node-cell node)) (min (the sf (cell-min-x (node-cell node)))
					     (the sf (first (node-absolute-location node))))
	  (cell-min-y (node-cell node)) (min (the sf (cell-min-y (node-cell node)))
					     (the sf (second (node-absolute-location node))))
	  (cell-min-z (node-cell node)) (min (the sf (cell-min-z (node-cell node)))
					     (the sf (third (node-absolute-location node)))))))

(defun add-off-diag-segment (seg diag off-diag off-diag-entry)
  ;; Adds off diagonal entries for this segment.
  (declare (ignore diag))
  (let ((node1 (segment-node-1 seg))
	(node2 (segment-node-2 seg)))
    (cond ((eq off-diag node1) (setf (segment-mat-21-point seg) off-diag-entry))
	  ((eq off-diag node2) (setf (segment-mat-12-point seg) off-diag-entry)))))

(defun clear-matrix-pointers ()
  (loop for seg being the hash-value of (SEGMENT-HASH-TABLE) do
	(setf (segment-mat-21-point seg) nil
	      (segment-mat-12-point seg) nil)))

(proclaim '(inline get-segment-voltage-1-double))
(defun get-segment-voltage-1-double (seg) (the df (node-voltage-n (segment-node-1 seg))))

(proclaim '(inline get-segment-voltage-2))
(defun get-segment-voltage-2 (seg) (the df (node-voltage-n (segment-node-2 seg))))

(defun segment-voltage (seg)
  (let ((seg (element seg 'segment)))
    (when seg (s-flt (node-voltage-n (segment-node-2 seg))))))

(proclaim '(inline eval-segment))
(defun eval-segment (segment-guts segment-node-2-floats)
  (declare (optimize (safety 0) (speed 3) (space 0) (debug 0))
	   (type (SIMPLE-ARRAY DOUBLE-FLOAT (*)) segment-guts segment-node-2-floats)
	   ;(type segment-df-array segment-guts)
	   ;(type node-df-array segment-node-2-floats)
	   )
  (let* ((alpha-cap-double (* (the df (*2/delta-t[n]*)) (segment-guts-capacitance segment-guts)))
	 (node-double-floats segment-node-2-floats))
    (declare (double-float alpha-cap-double))
    ;; (when *debug-segments* (format t " (~f + ~f) => " (* (node-aref-voltage-n node-double-floats) alpha-cap-double) (node-aref-alpha-charge node-double-floats)))
    (setf
     (node-aref-alpha-charge node-double-floats) (+ (node-aref-alpha-charge node-double-floats) (* alpha-cap-double (node-aref-voltage-n node-double-floats)))
     (node-aref-jacobian node-double-floats) (+ (node-aref-jacobian node-double-floats) alpha-cap-double))
    ;; (when *debug-segments* (format t " Result: ~f~%" (node-aref-alpha-charge node-double-floats)))
    )
  nil)

(defun eval-all-segments ()
  (declare (optimize (safety 0) (speed 3) (space 0) (compilation-speed 0)))
  (loop for segment-guts in *segment-guts-list*
	for segment-node-2-floats in *segment-node-2-floats-list*
	do (eval-segment segment-guts segment-node-2-floats))
  nil)

(defun initialize-seg-node-jacobians-and-currents ()
  ;; Calculate the time invariant component of the node jacobians, which consist of segment g-axial terms, and soma and segment g-leak terms.
  (declare (optimize (safety 0) (speed 3) (space 1)))
  (segment-iterator
   (progn
     (unless (or (node-has-ideal-voltage-source (segment-node-1 seg)) (node-has-ideal-voltage-source (segment-node-2 seg)))
       (accumulate-setf
	(node-const-jacobian (segment-node-1 seg))
	(segment-g-axial seg)))
     (unless (node-has-ideal-voltage-source (segment-node-2 seg))
       (accumulate-setf
	(node-const-jacobian (segment-node-2 seg))
	(+ (if (node-has-ideal-voltage-source (segment-node-1 seg)) 0.0d0 (segment-g-axial seg)) (segment-g-leak seg)))
       (deccumulate-setf
	(node-const-current (segment-node-2 seg))
	(segment-guts-g-leak*v-leak (segment-guts seg)))))))

(defun fix-up-off-diags ()
  ;; take care of the off diag arrays
  (add-segs-to-off-diags))

(defun add-segs-to-off-diags ()
  (declare (optimize (safety 0) (speed 3) (space 1)))
  (dotimes (i *num-unknowns*)
    (setf (aref (the (vector double-float) *lower-diag-double*) i) 0.0d0
	  (aref (the (vector double-float) *upper-diag-double*) i) 0.0d0))
  (loop for seg being the hash-value of (SEGMENT-HASH-TABLE) unless (node-has-ideal-voltage-source (segment-node-2 seg)) do (add-seg-params-to-off-diag seg)))

(defun add-seg-params-to-off-diag (seg)
  (declare (optimize (safety 0) (speed 3) (space 1)))
  (when (segment-mat-21-point seg)
    (let ((off-diag-entry (segment-mat-21-point seg))
	  (node2 (segment-node-2 seg)))
      (if (core-off-diag-lower off-diag-entry)
	  (deccumulate-setf (aref *lower-diag-double* (node-index node2)) (segment-g-axial seg))
	  (deccumulate-setf (aref *upper-diag-double* (node-index node2)) (segment-g-axial seg)))))
  (when (segment-mat-12-point seg)
    (let ((off-diag-entry (segment-mat-12-point seg))
	  (node1 (segment-node-1 seg)))
      (if (core-off-diag-lower off-diag-entry)
	  (deccumulate-setf (aref *lower-diag-double* (node-index node1)) (segment-g-axial seg))
	  (deccumulate-setf (aref *upper-diag-double* (node-index node1)) (segment-g-axial seg)))))
  nil)

(defun get-segment-name (name seg) (declare (ignore seg)) (print name))
(defun get-segment-cell-name (name seg) (declare (ignore name)) (print (cell-name (segment-cell seg))))

(defun make-segment-lists ()
  (when *make-segment-lists*
    (loop for segment being the hash-value of (SEGMENT-HASH-TABLE)
	  unless (node-has-ideal-voltage-source (segment-node-2 segment))
	  collect (node-double-floats (segment-node-2 segment)) into segments-node-2-floats
	  and collect (segment-guts segment) into segments-guts
	  finally (setq *segment-guts-list* segments-guts
			*segment-node-2-floats-list* segments-node-2-floats))
    (setq *make-segment-lists* nil)))




