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


;;; SYS Source file: sample-cells.lisp
(in-package "SURF-HIPPO")


;; Some basic cell types and cells....

(cell-type-def
 '(CORTICAL
   (v-leak . -65)			; mV
   (rm . 40000)				; ohms-cm2
   (ri  . 200)				; ohms-cm
   (cm . 0.7))				; uF/cm2
 )

(defun ball-and-sticks (&key name
			     (origin '(0 0 0)) ; microns
			     (cell-type 'CORTICAL)
			     (soma-diameter 35) ; microns
			     (include-apical t) (apical-dendrite-diameter 12) (apical-dendrite-length 1200) ; microns
			     include-basal (basal-dendrite-diameter 12) (basal-dendrite-length 200) ; microns
			     ri		; ohms-cm
			     rm rm-soma ; ohms-cm2
			     cm cm-soma ; uF/cm2
			     (apical-total-segs 5) (basal-total-segs 5))
  "Create and returns a soma/short-cable cell model of CELL-TYPE (default 'CORTICAL) at ORIGIN (default '(0 0 0)), with an
\"apical\" dendrite (when INCLUDE-APICAL is T, the default) with APICAL-DENDRITE-DIAMETER (default 12) and APICAL-DENDRITE-LENGTH
in microns (default 1200) and comprised of APICAL-TOTAL-SEGS (default 5), and a \"basal\" dendrite (when INCLUDE-BASAL is T,
default NIL), with similar arguments. Soma is specified with SOMA-DIAMETER in microns (default 35). The cell type parameters RI
 (ohm-cm), RM, RM-SOMA (ohm-cm2), CM, CM-SOMA (uF/cm2) may be explicitly specified if non-NIL (default NIL)."
  (let* ((cell-type (create-cell-type cell-type))
	 (name (or name (cell-type-name cell-type)))
	 (soma (create-soma :diameter soma-diameter :cell (create-cell name :origin origin :cell-type cell-type))))
    (cond-every
     (rm (cell-type-parameter cell-type :rm rm))
     (ri (cell-type-parameter cell-type :ri ri))
     (rm-soma (cell-type-parameter cell-type :rm-soma rm-soma))
     (cm (cell-type-parameter cell-type :cm cm))
     (cm-soma (cell-type-parameter cell-type :cm-soma cm-soma)))
    (cond-every
     (include-apical
      (segment-chain
       soma (when include-basal "a") apical-total-segs (/ apical-dendrite-length apical-total-segs) apical-dendrite-diameter :proximal-theta (* 0.5 pi)))
     (include-basal
      (segment-chain
       soma (when include-apical "b") basal-total-segs (/ basal-dendrite-length basal-total-segs)  basal-dendrite-diameter :proximal-theta (* -0.5 pi))))
    (soma-cell soma)))

;; Rm ~40K cm2, Rin ~50M => soma diameter ~160um
(defun basic-soma-cell () (create-cell "basic-soma" :soma-diameter 160))

(defun basic-is-cell ()
  (create-cell "basic-tree"
	       :soma-diameter 100
	       :segment-diameter 1
	       :tree-list '((soma 1 5  0 0)
			    (1    2 10 0 0)
			    (2    3 15 0 0)
			    (3    4 20 0 0)
			    (4    5 25 0 0))))

(defun basic-tree-cell ()
  (create-cell "basic-tree"
	       :soma-diameter 25
	       :segment-diameter 5
	       :tree-list '((soma 1 20  -10 0)
			    (1    2 40 10 0)
			    (2    3 60 -10 0)
			    (3    4 80 10 0)
			    (4    5 100 -10 0))))
	       
(defun basic-tree-cell-2 ()
  (create-cell "basic-tree"
	       :soma-diameter 25
	       :segment-diameter 5
	       :tree-list '((soma 1 20  -10 0)
			    (1    2a 40 10 0)
			    (2a    3a 60 -10 0)
			    (3a    4a 80 10 0)
			    (4a    5a 100 -10 0)
			    (1    2b 40 -40 0)
			    (2b    3b 60 -80 0)
			    (3b    4b 80 -90 0)
			    (4b    5b 100 -100 0))))

(defun basic-single-seg-cell ()
  (create-cell "basic-tree"
	       :soma-diameter 25
	       :segment-diameter 5
	       :tree-list '((soma 1 20 -10 0))))

(defun basic-single-seg-cell-vs ()
  (basic-single-seg-cell)
  (add-vsource *soma*)
  (enable-element-plot *soma*)
  (setq *user-stop-time* 10))

(defun basic-synapse-cell ()
  (create-cell "Basic-auto-synapse" :soma-diameter 25)
  (add-isource *soma* :pulse-list '(5 10 -0.1))
  (events (CREATE-SYNAPSE *soma* 'fast-ex-alpha) 1)
  (element-type-parameter *synapse* 'iv-density 5)
  (events (CREATE-SYNAPSE *soma* 'fast-ex-abs) 2)
  (events (create-element *soma* 'FAST-EX) 3)
  (element-type-parameter *synapse* 'iv-density 5)
  (events (create-element *soma* 'FAST-EX-ABS) 4)
  (setq *user-stop-time* 50)
  (enable-element-plot (list 'synapse *soma* *isource*) :all)
  (setq *save-data-step* 1)
  *cell*)

(defun basic-light-synapse-cell ()
  (create-cell "Basic-light-synapse" :soma-diameter 25)
  (CREATE-SYNAPSE *soma* 'l-ex-1)
  (enable-element-plot '(synapse channel soma))
  (add-isource *soma* :pulse-list '(5 10 -0.1))
  (setq *enable-light* t
	*user-stop-time* 30
	*light-stimulus* :spot
	*spot-outside-diameter* 300
	*light-stimulus-start-time* 10 *light-stimulus-stop-time* 20
	*light-start-position-x* 0 *light-start-position-y* 0)
  *cell*)

