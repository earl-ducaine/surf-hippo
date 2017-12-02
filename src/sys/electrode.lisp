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


;;; SYS Source file: electrode.lisp

(in-package "SURF-HIPPO")

(defun add-ielectrode (element &key (capacitance 1.0e-3) (resistance 10e6) (SOURCE-RESISTANCE 0.0) (leak-resistance 1.0d16) name)
  "Adds an electrode model with current source to the cell element associated with ELEMENT -

                      RESISTANCE (ohms)       SOURCE-RESISTANCE (ohms)     
                                             -----------------------------
   ELEMENT node <----------/\\/\\/\\-----+-------| -/\\/\\/\\- <-- Current source |--Gnd
                                    |        -----------------------------
                CAPACITANCE (nF)  -----                                      
                                  -----                                     
                                    |
                                    |
                                   Gnd

Default values are CAPACITANCE 1.0e-3 (nF), RESISTANCE 10e6 (ohms), SOURCE-RESISTANCE 0 (ohms). A LEAK-RESISTANCE (ohms, default
10e16) is also included in parallel to the electrode capacitance. RESISTANCE must be greater than 0."
  (create-electrode
   element :capacitance capacitance :resistance resistance :SOURCE-RESISTANCE SOURCE-RESISTANCE :leak-resistance leak-resistance :name name :type 'current))

(defun add-velectrode (element &key (capacitance 1.0e-3) (resistance 10e6) (SOURCE-RESISTANCE 0.0) (leak-resistance 1.0d16) name)
  "Adds an electrode model with voltage source to the cell element associated with ELEMENT -

                      RESISTANCE (ohms)       SOURCE-RESISTANCE (ohms)     
                                             -----------------------------
   ELEMENT node <----------/\\/\\/\\-----+-------| -/\\/\\/\\- <-- Voltage source |--Gnd 
                                    |        -----------------------------
                CAPACITANCE (nF)  -----                                           
                                  -----                                           
                                    |
                                    |
                                   Gnd

Default values are CAPACITANCE 1.0e-3 (nF), RESISTANCE 10e6 (ohms), SOURCE-RESISTANCE 0 (ohms). A LEAK-RESISTANCE (ohms, default
10e16) is also included in parallel to the electrode capacitance. RESISTANCE must be greater than 0."
  (create-electrode
   element :capacitance capacitance :resistance resistance :SOURCE-RESISTANCE SOURCE-RESISTANCE :leak-resistance leak-resistance :name name :type 'voltage))

(defun electrodes (&OPTIONAL (ELEMENT NIL ELEMENT-SUPPLIED-P))
  "Returns a list of all electrodes associated with the cell elements referenced by ELEMENT, if supplied; otherwise, all
electrodes in circuit."
  (when *electrode* (loop for seg in (if ELEMENT-SUPPLIED-P (segments element) (segments)) when (electrode-p seg) collect seg)))

(defun create-electrode (element &key (capacitance 10e-3) ; nF
				 (resistance 10e6) (SOURCE-RESISTANCE 10.0) (leak-resistance 1.0d16) ; ohms
				 (type 'current) name)
  (let* ((cell-element (element-cell-element element))
	 (name (or name (format nil "~a-electrode" (element-name cell-element)))))
    (when (element name 'segment) (sim-error (format nil "~A already refers to a segment or electrode" name)))
    (let* ((circuit-processed *circuit-processed*)
	   ;; If circuit already processed, make sure to reprocess with the new segment at the end of this function. Note that
	   ;; CREATE-SEGMENT sets *CIRCUIT-PROCESSED* to NIL.
	   (electrode (create-segment name cell-element nil :length 100.0 :diameter 10.0 :phi 0.0)))
      (setf (segment-capacitance electrode) (d-flt capacitance))
      (setf (segment-g-axial electrode) (* 1d6 (/ 1.0 resistance)))
      (setf (segment-g-leak electrode) (/ 1.0e6 leak-resistance))

      (element-parameter electrode 'electrode t)
      (setf (segment-inherit-parameters-from-type electrode) nil)
      (element-parameter electrode 'rm (s-flt (* 1e6 (/ (element-area-cm2 electrode) (segment-g-leak electrode)))))
      ;; (* 1.0e6 pi-single 1.0e-4 0.25) = 78.53982
      (element-parameter electrode 'ri (s-flt (/ (* 78.53982 10.0 10.0) (* 100 (segment-g-axial electrode)))))

      (element-parameter electrode 'cm (s-flt (* 1e-3 (/ (segment-capacitance electrode) (element-area-cm2 electrode)))))
    
      (case type
	(current (setf (isource-resistance (add-isource electrode)) (/ SOURCE-RESISTANCE 1.0e6)))
	(voltage (setf (vsource-resistance (add-vsource electrode)) (/ SOURCE-RESISTANCE 1.0e6))))

      (when circuit-processed (process-circuit-structure t electrode))
      (setq *electrode* electrode))))

;; backward compatibility
(defun add-electrode (element &key (capacitance 10e-3) (resistance 10e6) (SOURCE-RESISTANCE 10.0) (leak-resistance 1.0d16) (type 'current) name)
  (create-electrode
   element :capacitance capacitance :resistance resistance :SOURCE-RESISTANCE SOURCE-RESISTANCE :leak-resistance leak-resistance :type type :name name))

(defun electrode-source (electrode)
  "Returns any source(s) associated with ELECTRODE."
  (let ((elect (element electrode 'electrode)))
    (when elect
      (loop for elt in (node-elements (element-physical-node elect))
	    when (or (isource-p elt)
		     (vsource-p elt))
	    collect elt into out
	    finally (return (atomize-list out))))))

(defun electrode-structure-p (structure)
  (and (segment-p structure)
       (get-a-value 'electrode (segment-parameters structure))))

(defun electrode-p (elt &optional elt-is-structure)
  (if elt-is-structure
    (electrode-structure-p elt)
    (element-parameter elt 'electrode)))

(defun electrode-resistance (&optional (electrode *electrode*))
  "Return the axial resistance of ELECTRODE in Mohms."
  (let ((seg (element electrode 'electrode)))
    (when seg (/ 1.0 (segment-g-axial seg)))))

(defun electrode-leak-resistance (&optional (electrode *electrode*))
  "Return the axial leak-resistance of ELECTRODE in Mohms."
  (let ((seg (element electrode 'electrode)))
    (when seg (/ 1.0 (segment-g-leak seg)))))

(defun electrode-capacitance (&optional (electrode *electrode*))
  "Return the capacitance of ELECTRODE in pF."
  (let ((seg (element electrode 'electrode)))
    (when seg (* (segment-guts-capacitance (segment-guts seg)) 1.0e3))))
  
(defun print-electrode (&optional (electrode *electrode*))
  (let* ((seg (element electrode 'segment))
	 (source (electrode-source electrode)))
    (loop for src in (coerce-to-list source)
	  when (isource-p src) sum 1 into num-isrcs
	  when (vsource-p src) sum 1 into num-vsrcs
	  finally 
	  (format
	   t
	   "Electrode ~a (~a, ~a):~% Series Resistance ~,1e Mohms, Leak Resistance ~,1e Mohms, Capacitance ~,1e pF~%"
	   (segment-name seg)
	   (node-name (segment-node-1 seg))
	   (node-name (segment-node-2 seg))
	   (/ 1.0 (segment-g-axial seg))
	   (/ 1.0 (segment-g-leak seg))
	   (* (segment-guts-capacitance (segment-guts seg)) 1.0e3))
	  (when source
	    (format t "  This electrode has")
	    (when (> num-isrcs 0) (format t " ~D isource~:p" num-isrcs))
	    (when (> num-vsrcs 0) (format t "~D vsource~:p" num-vsrcs))
	    (format t "~%")))))
    
(defun remove-electrode (&optional (electrode *electrode*))
  (let ((electrode (element electrode 'segment)))
    (erase-element
     (loop for elt in (node-elements (element-physical-node electrode))
	   when (or (isource-p elt) (vsource-p elt)) do (return elt)))
    (erase-element electrode)))

(defun menu-for-adding-electrodes (target-elt)
  (let* ((elcts-already-there
	  (loop for elct in (electrodes)
		when (eq (element-physical-node target-elt) (segment-node-1 elct))
		collect elct))
	 (types (loop for elct in elcts-already-there collect (source-type-on-node (segment-node-2 elct))))
	 (dummy1 (member 'isource types))
	 (dummy2 (member 'vsource types))
	 (dummy3 dummy1)
	 (dummy4 dummy2))
    (choose-variable-values
     '((dummy1 "Include current source model electrode" :boolean)
       (dummy2 "Include voltage source model electrode" :boolean))
     :label (format nil "Add Electrode Model to ~a" (element-name target-elt)))
    (cond-every
     ((not (eq dummy1 dummy3))
      (add-ielectrode target-elt))
     ((and dummy3 (not dummy1))
      (remove-electrode (loop for elct in elcts-already-there
			      when (eq 'isource (source-type-on-node (segment-node-2 elct)))
			      do (return elct))))
     ((not (eq dummy2 dummy4))
      (add-velectrode target-elt))
     ((and dummy4 (not dummy2))
      (remove-electrode (loop for elct in elcts-already-there
			      when (eq 'vsource (source-type-on-node (segment-node-2 elct)))
			      do (return elct)))))))
    
(defun set-electrode-capacitance (c-electrode &optional (electrode *electrode*))
  "Sets the capacitance of ELECTRODE to C-ELECTRODE (nF)."
  (let ((seg (element electrode 'electrode)))
    (when seg
      (setf (segment-capacitance seg) c-electrode)
      (element-parameter seg 'cm (* 1e-3 (/ (segment-capacitance seg)
					    (element-area-cm2 seg)))))))

(defun set-electrode-resistance (r-electrode &optional (electrode *electrode*))
  "Sets the resistance of ELECTRODE to R-ELECTRODE (Mohms - must be greater than 0)."
  (let ((seg (element electrode 'electrode)))
    (when seg
      (setf (segment-g-axial seg) (/ 1.0 r-electrode))
      ;; (* 1.0e6 pi-single 1.0e-4 0.25) = 78.53982
      (element-parameter seg 'ri (s-flt
				  (/ (* 78.53982 (segment-diameter seg) (segment-diameter seg))
				     (* (segment-length seg) (segment-g-axial seg))))))))    

(defun edit-electrode (&optional (electrode *electrode*)) (edit-electrodes electrode))

(defun edit-electrodes (&optional electrode)
  (loop for electrode in (coerce-to-list (element (or electrode (electrodes)) 'electrode))
	do (edit-segment-absolute electrode (format nil "~A source electrode" (case (source-type-on-node (segment-node-2 electrode))
										(isource "Current")
										(vsource "Voltage")))
				  nil)))

(defun ze-zc-i ()
  ;; Simple soma - (current source) electrode simulation.
  (add-ielectrode (create-soma
		   :cell (create-cell "Cell" :cell-type (create-cell-type-w-params "ze-zc" :cm 1.0 :v-leak 0.0))
		   :diameter 100.0)))

(defun ze-zc-v ()
  ;; Simple soma - (voltage source) electrode simulation.
  (add-velectrode (create-soma
		   :cell (create-cell "Cell" :cell-type (create-cell-type-w-params "ze-zc" :cm 1.0 :v-leak 0.0))
		   :diameter 100.0)))
	  


