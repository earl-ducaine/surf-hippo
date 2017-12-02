;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: SURF; Base: 10 -*-

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

;; Some sample hippo pc neurons (soma/short-cable model).


(in-package "SURF-HIPPO")

(defun dead-HIPPO (&optional (name "Dead Hippo")) (working-hpc :name name :active nil))
	      
(push 'dead-hippo *CIRCUIT-CATALOG-FUNCTIONS*)  

(defun Two-HIPPOs ()
  ;; Make two HIPPO cells, next to each other.
  (let ((cell1 (working-hpc :name "Hippo-1" :origin '(-300 0 100)))
	(cell2 (working-hpc :name "Hippo-2" :origin '(0 -100 0))))

    ;; A connection from cell Hippo-1 to cell Hippo-2.
    (create-synapse "Hippo-2-4"		; Destination is a segment of cell2.
		    'fast-ex
		    ;; The axon creation is embedded in the subsequent synapse creation.
		    (create-axon (cell-soma cell1) ; Presynaptic signal originates is the soma of cell1 and conveyed by a simple axon.
				 'simple))

    ;; And another synapse onto the tree of Hippo-2, this time driven directly from the soma voltage of Hippo 1.
    (create-synapse "Hippo-2-3"		; Destination is a segment of cell2.
		    'fast-ex
		    (cell-soma cell1))	; Presynaptic signal
  
    ;; And a connection from the soma of cell Hippo-2 to the dendrite of cell Hippo-1.
    (create-synapse "Hippo-1-5"
		    'fast-ex
		    (create-axon
		     (cell-soma cell2)	; Presynaptic signal
		     'simple)) 

    ;; Some external inputs to Hippo-1.

    ;; One gets a simple pulse    
    (pulse-list (add-ISOURCE (cell-soma cell1)) '((10 20 2)))
    ;; The other a sine burst
    (add-waveform (add-isource "Hippo-1-3") :waveform-spec '(sinewave 4 100 0.1 :STEP 0.1 :start 60 :OFFSET 4))

    (setq *user-stop-time* 100)

    ;; Set up some plotting 

    (plot-segments-to-soma "Hippo-1-5")
    (plot-segments-to-soma "Hippo-2-5")

    (enable-element-plot (list (somas) (cell-element 'synapse)))
    (enable-element-plot (list (channels) (synapses)) 'conductance)
    (enable-element-plot (isources))
    (enable-element-plot (axons))

    ;; To mark the synapses on the histology
    (set-type-graphics 'fast-ex 'yellow)
    nil))

(defun three-HIPPOs ()
  (let* ((cell1 (working-hpc :name "Hippo-1" :origin '(-300 0 100)))
	 (cell2 (working-hpc :name "Hippo-2"  :origin '(0 200 -300)))
	 (cell3 (working-hpc :name "Hippo-3"  :origin '(400 -200 0)))
	 (axon-1-3 (create-axon (cell-soma cell1) 'simple :mid-points '((-210 -100 200)	(0 -250 100) (280 35 -200) (200 820 -100)))))
    (setf (axon-type-propagation-velocity (element-type axon-1-3)) 20.0)
    (create-synapse "Hippo-3-4" 'fast-ex-alpha-voltage axon-1-3)
    (create-synapse "Hippo-2-4" 'fast-ex-alpha-voltage (cell-soma cell1))
    (events (create-synapse "Hippo-1-5" 'fast-ex-alpha) (poisson-events 0.2 40 100))

    (element-type-parameter 'fast-ex-alpha-voltage :gbar-source :absolute)
    (element-type-parameter 'fast-ex-alpha-voltage :gbar-ref 0.05)
	
    (pulse-list (add-isource (cell-soma cell1)) '(10 20 2))

    (setq *user-stop-time* 250)

    (plot-segments-to-soma "Hippo-1-5")
    (plot-segments-to-soma "Hippo-2-5")
    (plot-segments-to-soma "Hippo-3-5")

    (enable-element-plot `(soma axon ,(cell-element 'synapse)))
    (enable-element-plot `(axon synapse) 'event)

    (setq *group-plot-data-by-cell* nil)

    (set-type-graphics 'fast-ex-alpha-voltage 'yellow)
    (set-type-graphics 'fast-ex-alpha 'cyan)
  
    nil))

(push 'three-hippos *CIRCUIT-CATALOG-FUNCTIONS*)

(defun axon-HIPPO ()
  (working-hpc :name "Hippo-1" :origin '(0 100 0))
  (create-axon *soma* 'simple :length 100 :delay 15)
  (pulse-list (add-isource *soma*) '((10 20 2)))
  (enable-element-plot (list *soma* "Hippo-1-5" *axon*))
  (setq *user-stop-time* 100)
  *cell*)

;Deeds which populate the dimensions of space and which reach their end
;when someone dies may cause us wonderment, but one thing, or an infinite
;number of things, dies in every final agony, unless there is a universal
;memory as the theosophists have conjectured. In time there was a day that
;extinguished the last eyes to see Christ; the battle of Junin and the
;love of Helen died with the death of a man. What will die with me when I
;die, what pathetic or fragile form will the world lose? The voice of
;Macedonia Fernandez, the image of a red horse in the vacant lot at Serrano
;and Charcas, a bar of sulphur in the drawer of a mahogany desk?
;
;				- Jorge Luis Borges, "The Witness"

(defun ca3_32da-hippo ()
  (hippo :name "ca3_32da-hippo"
	 :apical-dendrite-diameter 13.4
	 :apical-dendrite-length (* 5 200)
	 :soma-diameter 30.07
	 :r-cyto 311
	 :r-mem 1.13e5
	 :r-mem-soma 2.02e5
	 :cap-mem 9.54e-1
	 :cap-mem-soma 9.54e-1))
