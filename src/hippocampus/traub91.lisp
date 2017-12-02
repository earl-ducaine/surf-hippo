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


(in-package "SURF-HIPPO")

;; Cell structures of Traub et al 1991.
;; See also channel definitions in parameters/traub91-chs.lisp
;; From the article :
;; A Model of a CA3 Hippocampal Pyramidal Neuron Incorporating
;;    Voltage-Clamp Data on Intrinsic Conductances
;; RD. Traub, RKS. Wong, R. Miles, H. Michelson
;; J. Neurophysiol. 1991, vol. 66, no. 2, 635-650.

;; The toplevel functions that define the various cell models relevent to this paper are
;; TRAUB91-CA1, TRAUB91-CA3, and TRAUB91-CA3-STUB. The "stub" version applies to fig. 4 of this
;; paper.


(defun traub-91-structure-tree (cell-name &optional (soma-type :spherical))
  (let* ((soma (cell-soma (element cell-name)))
	 (soma-name (soma-name soma))
	 (basal-soma-connection (case soma-type
				  (:SYMMETRIC-CYLINDER (element-parameter soma 'cylinder-soma-segment-1))
				  (:spherical soma)))
	 (basal-ref-angle  (case soma-type
			     (:SYMMETRIC-CYLINDER 0.0)
			     (:spherical -90.0)))
	 (apical-ref-angle  (case soma-type
			      (:SYMMETRIC-CYLINDER 0.0)
			      (:spherical 90.0)))
	 (apical-soma-connection (case soma-type
				   (:SYMMETRIC-CYLINDER (element-parameter soma 'cylinder-soma-segment-2))
				   (:spherical soma))))
    (create-tree-polar cell-name
		       `(("apical-10" ,apical-soma-connection 5.78 120.0 ,apical-ref-angle 0.0)
			 ("apical-11" "apical-10" 5.78 120.0 0.0 0.0)
			 ("apical-12" "apical-11" 5.78 120.0 0.0 0.0)
			 ("apical-13" "apical-12" 5.78 120.0 0.0 0.0)
			 ("apical-14" "apical-13" 5.78 120.0 0.0 0.0)
			 ("apical-15" "apical-14" 5.78 120.0 0.0 0.0)
			 ("apical-16" "apical-15" 5.78 120.0 0.0 0.0)
			 ("apical-17" "apical-16" 5.78 120.0 0.0 0.0)
			 ("apical-18" "apical-17" 5.78 120.0 0.0 0.0)
			 ("apical-19" "apical-18" 5.78 120.0 0.0 0.0)
		       
			 ("basal-8" ,basal-soma-connection 4.84 120.0  -90.0 0.0)
			 ("basal-7" "basal-8" 4.84 120.0 0.0 0.0)
			 ("basal-6" "basal-7" 4.84 120.0 0.0 0.0)
			 ("basal-5" "basal-6" 4.84 120.0 0.0 0.0)
			 ("basal-4" "basal-5" 4.84 120.0 0.0 0.0)
			 ("basal-3" "basal-4" 4.84 120.0 0.0 0.0)
			 ("basal-2" "basal-3" 4.84 120.0 0.0 0.0)
			 ("basal-1" "basal-2" 4.84 120.0 0.0 0.0))
		       t)))

(defun traub-91-structure-tree-stub (cell-name &optional soma-type)
  (let* ((soma (cell-soma (element cell-name)))
	 (soma-name (soma-name soma))
	 (basal-soma-connection (case soma-type
				  (:SYMMETRIC-CYLINDER (element-parameter soma 'cylinder-soma-segment-1))
				  (:spherical soma)))
	 (basal-ref-angle  (case soma-type
			     (:SYMMETRIC-CYLINDER 0.0)
			     (:spherical -90.0)))
	 (apical-ref-angle  (case soma-type
			      (:SYMMETRIC-CYLINDER 0.0)
			      (:spherical 90.0)))
	 (apical-soma-connection (case soma-type
				   (:SYMMETRIC-CYLINDER (element-parameter soma 'cylinder-soma-segment-2))
				   (:spherical soma))))
    (create-tree-polar cell-name
		       `(("apical-10" ,apical-soma-connection 5.78 120.0 ,apical-ref-angle 0.0)
			 ("basal-8" ,basal-soma-connection 4.84 120.0 ,basal-ref-angle 0.0))
		       t)))
		       
(defun traub-91-ca1-channels (cell &optional (soma-type :spherical))
  (let ((name-prefix (when *add-cell-name-to-segs* (format nil "~A-" (cell-name cell))))
	(soma-ch-density-factor (case soma-type
				  (:symmetric-cylinder 3)
				  (:spherical 1))))
    (mapcar (lambda (cell-elt-refs-type-densities)
	      (add-channel-in-element-with-density (car cell-elt-refs-type-densities) (cadr cell-elt-refs-type-densities) name-prefix))
	    `((*soma* ((na-trb91 ,(* soma-ch-density-factor 300.0))
		       (Kdr-trb91 ,(* soma-ch-density-factor 250.0))
		       (ca-trb91 ,(* soma-ch-density-factor 40.0))
		       (kc-trb91 ,(* soma-ch-density-factor 100.0))
		       (ahp-trb91 ,(* soma-ch-density-factor 8.0))
		       (ka-trb91 ,(* soma-ch-density-factor 50.0))))
	      (("apical-18" "apical-17" "apical-16") ((ca-trb91 50.0) (kc-trb91 50.0) (ahp-trb91 8.0)))
	      (("apical-15" "apical-14" "apical-13") ((ca-trb91 70.0) (kc-trb91 50.0) (ahp-trb91  8.0)))
	      (("apical-12") ((na-trb91 200.0) (Kdr-trb91 200.0) (ca-trb91 170.0) (kc-trb91 150.0) (ahp-trb91 8.0)))
	      (("apical-11") ((Kdr-trb91 50.0) (ca-trb91 50.0) (kc-trb91 50.0) (ahp-trb91 8.0)))
	      (("apical-10") ((na-trb91 150.0) (Kdr-trb91 100.0) (ca-trb91 80.0) (kc-trb91 200.0) (ahp-trb91 8.0)))
	      (("basal-8") ((na-trb91 150.0) (Kdr-trb91 100.0) (ca-trb91 80.0) (kc-trb91 200.0) (ahp-trb91 8.0)))
	      (("basal-7") ((Kdr-trb91 50.0) (ca-trb91 50.0) (kc-trb91 50.0) (ahp-trb91 8.0)))
	      (("basal-6") ((na-trb91 200.0) (Kdr-trb91 200.0) (ca-trb91 120.0) (kc-trb91 100.0) (ahp-trb91 8.0)))
	      (("basal-4" "basal-5") ((ca-trb91 70.0) (kc-trb91 50.0) (ahp-trb91 8.0)))
	      (("basal-2" "basal-3") ((ca-trb91 50.0) (kc-trb91 50.0) (ahp-trb91 8.0)))))))

(defun traub-91-ca3-channels (cell &optional (soma-type :spherical))
  (let ((name-prefix (when *add-cell-name-to-segs* (format nil "~A-" (cell-name cell))))
	(soma-ch-density-factor (case soma-type
				  (:symmetric-cylinder 3)
				  (:spherical 1))))
    (mapcar (lambda (cell-elt-refs-type-densities)
	      (add-channel-in-element-with-density (car cell-elt-refs-type-densities) (cadr cell-elt-refs-type-densities) name-prefix))
	    `((*soma* ((na-trb91  ,(* soma-ch-density-factor 300.0))
		       (Kdr-trb91  ,(* soma-ch-density-factor 150.0))
		       (ca-trb91  ,(* soma-ch-density-factor 40.0))
		       (kc-trb91  ,(* soma-ch-density-factor 100.0))
		       (ahp-trb91  ,(* soma-ch-density-factor  8.0))
		       (ka-trb91  ,(* soma-ch-density-factor 50.0))))
	      (("apical-18" "apical-17") ((ca-trb91 50.0) (kc-trb91 50.0) (ahp-trb91 8.0)))
	      (("apical-16" "apical-15") ((ca-trb91 100.0) (kc-trb91 150.0) (ahp-trb91 8.0)))
	      (("apical-14" "apical-13") ((ca-trb91 170.0) (kc-trb91 150.0) (ahp-trb91 8.0)))
	      (("apical-12") ((na-trb91 200.0) (Kdr-trb91 200.0) (ca-trb91 170.0) (kc-trb91 150.0) (ahp-trb91 8.0)))
	      (("apical-11") ((Kdr-trb91 0.0) (ca-trb91 50.0) (kc-trb91  50.0) (ahp-trb91 8.0)))
	      (("apical-10") ((na-trb91 150.0) (Kdr-trb91 50.0)	(ca-trb91 80.0)	(kc-trb91  200.0) (ahp-trb91 8.0)))
	      (("basal-8")   ((na-trb91 150.0) (Kdr-trb91 50.0)	(ca-trb91  80.0) (kc-trb91  200.0) (ahp-trb91 8.0)))
	      (("basal-7")   ((Kdr-trb91 0.0) (ca-trb91 50.0) (kc-trb91 50.0) (ahp-trb91 8.0)))
	      (("basal-6")   ((na-trb91 200.0) (Kdr-trb91 200.0) (ca-trb91 120.0) (kc-trb91 100.0) (ahp-trb91 8.0)))
	      (("basal-4" "basal-5") ((ca-trb91 120.0) (kc-trb91 100.0) (ahp-trb91 8.0)))
	      (("basal-2" "basal-3") ((ca-trb91 50.0) (kc-trb91 50.0) (ahp-trb91 8.0)))))))
			
(defun traub-91-conc-int-adjustments (cell &optional (soma-type :spherical))
  (let ((soma-ch-density-factor (case soma-type
				  (:symmetric-cylinder 3)
				  (:spherical 1))))
    (loop for conc-int in (conc-ints cell)
	  do
	  (element-parameter conc-int
			     'phi
			     (let ((candidate-name (element-name (conc-int-cell-element conc-int))))
			       (flet ((find-root-name (root-names) (mapcar (lambda (root-name) (search root-name candidate-name)) root-names)))
				 (cond
				   ((find-root-name '("basal-1" "basal-2" "basal-3" "basal-4" "basal-5" "basal-6" "basal-7"))
				    7.769) ; Misprint in paper 
				   ((find-root-name '("basal-8"))
				    34.53) ; Misprint in paper 
				   ((find-root-name '("soma"))
				    (* soma-ch-density-factor  17.402)) ; Misprint in paper 
				   ((find-root-name '("apical-10"))
				    26.404) ; Misprint in paper 
				   ((find-root-name '("apical-11" "apical-12" "apical-13" "apical-14" "apical-15" "apical-16" "apical-17" "apical-18"  "apical-19"))
				    5.941)) ; Misprint in paper 
				 )))
	  (when (element-parameter conc-int 'phi)
	    (element-parameter conc-int 'current-shell-base-coefficient-factor
			       (* (juxtamembrane-shell-volume conc-int)
				  (/ (* 2 Faraday) ; +2 for calcium valence
				     1.0d-6) ; 1d-6 to scale properly
				  (element-parameter conc-int 'phi)))))))

(defvar *trb-91-soma-type* :spherical)

(cell-type-def
 '(TRAUB-91-HPC
   (rm . 10000)
   (ri . 100)
   (cm . 3)
   (v-leak . -60)))
 
(defun traub91 (cell-name &key stub ca1p (soma-type *trb-91-soma-type*) (origin '(0.0 0.0 0.0)))
  (setq *enable-channel-membrane-parameter-update* nil
	*ignore-q10* t)
  (let* ((cell (create-cell cell-name :cell-type 'TRAUB-91-HPC :origin origin))
	 (cell-name (cell-name cell))
	 (*add-cell-name-to-segs* t)
	 (soma (case soma-type
		 (:symmetric-cylinder (create-symmetric-cylinder-soma cell-name 8.46 ; 32.519226
								      :length 125.0 
								      :location-1 `(0.0 ,(/ 125.0 -2.0) 0.0)
								      :location-2 `(0.0 ,(/ 125.0 2.0) 0.0)))
		 (:spherical (create-soma :cell cell-name :diameter (sqrt (*  8.46 125.0))
					  :location '(0.0 0.0 0.0)
					  :length 125.0 :soma-cylinder-diameter 8.46)))))

    (if stub (traub-91-structure-tree-stub cell-name soma-type)	(traub-91-structure-tree cell-name soma-type))
    (if ca1p (traub-91-ca1-channels cell soma-type) (traub-91-ca3-channels cell soma-type))
    (traub-91-conc-int-adjustments cell soma-type)))

(defun traub91-CA1 ()
  (traub91 "Traub91-CA1" :ca1p t)
  (std-setup)
  (pulse-list *isource* '(10.0 250.0 0.70))
  (enable-element-plot '("Traub91-CA3-apical-12"
			 "Traub91-CA3-apical-15"))
  (setq	*user-stop-time* 300.0))

(defun traub91-CA3 ()
  (traub91 "Traub91-CA3" :ca1p nil)
  (std-setup)
  (pulse-list *isource* '(10.0 250.0 0.70))
  (enable-element-plot '("Traub91-CA3-apical-12"
			 "Traub91-CA3-apical-15"))
  (setq	*user-stop-time* 300.0))


(defun traub91-CA3-stub ()
  (traub91 "Traub91-CA3" :stub t :ca1p nil)
  (loop for type in '("KC-TRB91" "AHP-TRB91") do
	(loop for ch in (channels-of-type (element type))
	      do (element-parameter ch 'gbar-density (/ (element-parameter ch 'gbar-density) 20.0))))
  (std-setup)
  (pulse-list *isource* '(10.0 250.0 0.70))
  (enable-element-plot '("Traub91-CA3-apical-15"
			 "Traub91-CA3-apical-12"))
  (setq *user-stop-time* 300.0))



(push 'traub91-CA1 *CIRCUIT-CATALOG-FUNCTIONS*)

(push 'traub91-CA3 *CIRCUIT-CATALOG-FUNCTIONS*)

(push 'traub91-CA3-stub *CIRCUIT-CATALOG-FUNCTIONS*)


#|

(let ((types (get-current-channel-types-menu "Edit Channel Types In Circuit")))
  (plot-scatter
   (loop for type in types collect
	 (no-nils (loop for ch in (channels-of-type type)
			collect (list (second (element-absolute-location (element-cell-element ch)))
				      (coerce (get-element-gbar-reference ch) 'single-float)))))
   (loop for type in types  collect (element-name type))
   :x-label "um"
   :y-label "uS"))

|#
