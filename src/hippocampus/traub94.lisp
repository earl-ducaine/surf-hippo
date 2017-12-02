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

#|
;; Cell structure of traub and al 1994.
;; See also channel definitions in parameters/traub94-chs.lisp
;; From the article :
@ARTICLE{Tra-Jef-Mil-Whi-Tot-94,
	AUTHOR = {Traub, R. D. and Jefferys, J. G. R. and Miles, R. and Whittington, M. A. and T\'oth, K.},
	TITLE = {A branching dendritic model of a rodent {CA3} pyramidal neurone},
	JOURNAL = {Journal of Physiology},
	YEAR = {1994},
	VOLUME = {481},
	NUMBER = {1}

The main cell function relevent to the article is TRAUB-CA3-B.

|#

(defun traub-94-structure-tree (cell &optional (soma-type :spherical))
  (let* ((soma (cell-soma (element cell 'cell)))
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
    (create-tree-polar
     cell
     `(("IS" ,soma 4.0 75.0 180.0 0.0)
       ("Axon-1" "IS" 1.0 75.0 -5.0 0.0)
       ("Axon-2" "Axon-1" 1.0 75.0 90.0 0.0)
       ("Axon-3" "Axon-2" 1.0 75.0 30.0 0.0)
       ("Axon-4" "Axon-3" 1.0 75.0 30.0 0.0)

       ("c-1" ,basal-soma-connection 5.0 15.0 ,(+ basal-ref-angle 60.0) 0.0 2.0 ((level . 3))) ;level 3
       ("c-2" ,basal-soma-connection 5.0 15.0 ,(+ basal-ref-angle 20.0) 0.0 2.0 ((level . 3))) ;level 3
       ("c-3" ,basal-soma-connection 5.0 15.0 ,(+ basal-ref-angle -20.0) 0.0 2.0 ((level . 3))) ;level 3
       ("c-4" ,basal-soma-connection 5.0 15.0 ,(+ basal-ref-angle -60.0) 0.0 2.0 ((level . 3))) ;level 3

       ("b-11" "c-1" 3.14 40.0 -10.0  0.0 2.0 ((level . 2))) ;level2
       ("b-12" "c-1" 3.14 40.0 10.0 0.0 2.0 ((level . 2)))
       ("b-21" "c-2" 3.14 40.0 -10.0  0.0 2.0 ((level . 2)))
       ("b-22" "c-2" 3.14 40.0 10.0 0.0 2.0 ((level . 2)))
       ("b-31" "c-3" 3.14 40.0 -10.0  0.0 2.0 ((level . 2)))
       ("b-32" "c-3" 3.14 40.0 10.0 0.0 2.0 ((level . 2)))
       ("b-41" "c-4" 3.14 40.0 -10.0  0.0 2.0 ((level . 2)))
       ("b-42" "c-4" 3.14 40.0 10.0 0.0 2.0 ((level . 2)))

       ("a-111" "b-11" 2.0 70.0 -10.0  0.0 2.0 ((level . 1))) ;level1
       ("a-112" "b-11" 2.0 70.0 10.0 0.0 2.0 ((level . 1)))
       ("a-121" "b-12" 2.0 70.0 -10.0  0.0 2.0 ((level . 1)))
       ("a-122" "b-12" 2.0 70.0 10.0 0.0 2.0 ((level . 1))) 
       ("a-211" "b-21" 2.0 70.0 -10.0  0.0 2.0 ((level . 1)))
       ("a-212" "b-21" 2.0 70.0 10.0 0.0 2.0 ((level . 1)))
       ("a-221" "b-22" 2.0 70.0 -10.0  0.0 2.0 ((level . 1)))
       ("a-222" "b-22" 2.0 70.0 10.0 0.0 2.0 ((level . 1)))
       ("a-311" "b-31" 2.0 70.0 -10.0  0.0 2.0 ((level . 1)))
       ("a-312" "b-31" 2.0 70.0 10.0 0.0 2.0 ((level . 1)))
       ("a-321" "b-32" 2.0 70.0 -10.0  0.0 2.0 ((level . 1)))
       ("a-322" "b-32" 2.0 70.0 10.0 0.0 2.0 ((level . 1)))
       ("a-411" "b-41" 2.0 70.0 -10.0  0.0 2.0 ((level . 1)))
       ("a-412" "b-41" 2.0 70.0 10.0 0.0 2.0 ((level . 1)))
       ("a-421" "b-42" 2.0 70.0 -10.0  0.0 2.0 ((level . 1)))
       ("a-422" "b-42" 2.0 70.0 10.0 0.0 2.0 ((level . 1)))
	  
       ("shaft" ,apical-soma-connection	10.0 50.0 ,apical-ref-angle 0.0 2.0 ((level . 5)))

       ("f-1" "shaft" 6.30 50.0 -45  0.0 2.0 ((level . 6))) ;level 6
       ("f-2" "shaft" 6.30 50.0 45.0 0.0 2.0 ((level . 6))) ;level 6
		       
       ("goblique-1" "f-1" 2.78 70.0 -45.0 0.0 2.0 ((level . 7o))) ;level 7
       ("goblique-2" "f-2" 2.78 70.0 45.0 0.0 2.0 ((level . 7o))) ;level 7
		       
       ("g-11" "f-1" 5.0 50.0 0.0 0.0 2.0 ((level . 7))) ;level 7
       ("g-22" "f-2" 5.0 50.0 0.0 0.0 2.0 ((level . 7))) ;level 7

       ("h-111" "g-11" 3.14 60.0 0.0 0.0 2.0 ((level . 8))) ;level 8
       ("h-112" "g-11" 3.14 60.0 80.0 0.0 2.0 ((level . 8))) ;level 8
       ("h-221" "g-22" 3.14 60.0 -80.0 0.0 2.0 ((level . 8))) ;level 8
       ("h-222" "g-22" 3.14 60.0 0.0 0.0 2.0 ((level . 8))) ;level 8
    
       ("ioblique-111" "h-111" 1.4 50.0 10.0 0.0 2.0 ((level . 9o))) ;level 9 obliques
       ("ioblique-112" "h-112" 1.4 50.0 -10.0 0.0 2.0 ((level . 9o))) ;level 9
       ("ioblique-221" "h-221" 1.4 50.0 10.0 0.0 2.0 ((level . 9o))) ;level 9
       ("ioblique-222" "h-222" 1.4 50.0 -10.0 0.0 2.0 ((level . 9o))) ;level 9

       ("i-1111" "h-111" 2.5 60.0 30.0 0.0 2.0 ((level . 9))) ;level 9
       ("i-1121" "h-112" 2.5 60.0 -40.0 0.0 2.0 ((level . 9))) ;level 9
       ("i-2211" "h-221" 2.5 60.0 40.0 0.0 2.0 ((level . 9))) ;level 9
       ("i-2221" "h-222" 2.5 60.0 -30.0 0.0 2.0 ((level . 9))) ;level 9


       ("j-11111" "i-1111" 1.6 60.0 20.0 0.0 2.0 ((level . 10))) ;level 10
       ("j-11112" "i-1111" 1.6 60.0 -20.0 0.0 2.0 ((level . 10))) ;level 10
       ("j-11211" "i-1121" 1.6 60.0 20.0 0.0 2.0 ((level . 10))) ;level 10
       ("j-11212" "i-1121" 1.6 60.0 -20.0 0.0 2.0 ((level . 10))) ;level 10
       ("j-22111" "i-2211" 1.6 60.0 20.0 0.0 2.0 ((level . 10))) ;level 10
       ("j-22112" "i-2211" 1.6 60.0 -20.0 0.0 2.0 ((level . 10))) ;level 10
       ("j-22211" "i-2221" 1.6 60.0 20.0 0.0 2.0 ((level . 10))) ;level 10
       ("j-22212" "i-2221" 1.6 60.0 -20.0 0.0 2.0 ((level . 10))) ;level 10

       ("k-11111" "j-11111" 1.6 60.0 -10.0 0.0 2.0 ((level . 11))) ;level 11
       ("k-11112" "j-11112" 1.6 60.0 10.0 0.0 2.0 ((level . 11))) ;level 11
       ("k-11211" "j-11211" 1.6 60.0 -10.0 0.0 2.0 ((level . 11))) ;level 11
       ("k-11212" "j-11212" 1.6 60.0 10.0 0.0 2.0 ((level . 11))) ;level 11
       ("k-22111" "j-22111" 1.6 60.0 -10.0 0.0 2.0 ((level . 11))) ;level 11
       ("k-22112" "j-22112" 1.6 60.0 10.0 0.0 2.0 ((level . 11))) ;level 11
       ("k-22211" "j-22211" 1.6 60.0 -10.0 0.0 2.0 ((level . 11))) ;level 11
       ("k-22212" "j-22212" 1.6 60.0 10.0 0.0 2.0 ((level . 11))))) ;level 11
    ))
		       		       
;; The voltage in the original equations is referenced to the "resting potential", which we will
;; take here to be -60.0mV (as suggested by the 91 paper).
(cell-type-def
 '(TRAUB-94-HPC
   (rm . 50000)
   (ri . 200)
   (cm . 0.75)
   (v-leak . -60)))
 
(defvar *trb-94-soma-type* :spherical)

(defun traub-CA3-branched (cell-name &key (origin (list 0.0 0.0 0.0)) (soma-type *trb-94-soma-type*))
  (setq					; *use-variable-e-rev nil
   *ignore-q10* t
   *e-na* 55.0
   ;; Traub puts the axonal e-k lower (-85.0) than soma, although he mentions that having the same e-k also works
   *e-k* -75.0			 
   *e-ca* 80.0)
  
  (let ((cell (create-cell cell-name :cell-type 'TRAUB-94-HPC :origin origin)))
    (element-parameter *cell-type* 'g-axial-computation :average-g) ; This is what traub uses?
    (case soma-type
      (:symmetric-cylinder
       (create-symmetric-cylinder-soma cell-name 30.0 :location-1 '(0.0 -12.75 0.0) :location-2 '(0.0 12.75 0.0)))
      (:spherical (create-soma :cell cell-name :diameter (sqrt (* 25.5 30.0))
			       :location '(0.0 0.0 0.0)
			       :length 25.5 :soma-cylinder-diameter 30.0)))
    (element-parameter *soma* 'level 4)
    (loop for seg in (soma-segments *soma*) do (element-parameter seg 'level 4))
    (traub-94-structure-tree *cell* soma-type)
    (setq *enable-segment-membrane-parameter-update* T)
    (let ((axons '("Axon-1" "Axon-2" "Axon-3" "Axon-4" "IS")))
      (set-segment-parameter axons 'MEMBRANE-RESISTIVITY 1000.0)
      (set-segment-parameter Axons 'CYTOPLASMIC-RESISTIVITY 100.0)
      (add-channel-in-element-with-density axons
					   '((na-ax-trb94 5000.0)
					     (Kdr-ax-trb94 2500.0))))
    (let ((soma-ch-density-factor (case soma-type
				    (:symmetric-cylinder 3)
				    (:spherical 1))))
      (add-channel-in-element-with-density  *soma*
					 `((na-trb94 ,(* soma-ch-density-factor 1000.0))
					   (Kdr-trb94 ,(* soma-ch-density-factor 1350.0))
					   (ca-soma-trb94 ,(* soma-ch-density-factor 10.0))
					   (ahp-soma-trb94 ,(* soma-ch-density-factor 8.0))
					   (kc-soma-trb94 ,(* soma-ch-density-factor 200.0))
					   (ka-trb94 ,(* soma-ch-density-factor 5.0)))))

    (add-channel-in-element-with-density '("shaft") '((na-trb94 30.0)
						      (Kdr-trb94 200.0)
						      (ca-dendrite-trb94  10.0)
						      (ahp-dendrite-trb94  8.0)
						      (kc-dendrite-trb94  80.0)
						      (ka-trb94 5.0)))

    (add-channel-in-element-with-density-motif "a" 0 '((ca-dendrite-trb94 10.0)
						       (ahp-dendrite-trb94 8.0)
						       (kc-dendrite-trb94 40.0))
					       cell)

    (add-channel-in-element-with-density-motif "b" 0 '((ca-dendrite-trb94 10.0)
						       (ahp-dendrite-trb94 8.0)
						       (kc-dendrite-trb94  40.0)
						       (ka-trb94 5.0))
					       cell)

    (add-channel-in-element-with-density-motif "c" 0 '((na-trb94 10.0)
						       (Kdr-trb94 150.0)
						       (ca-dendrite-trb94 10.0)
						       (ahp-dendrite-trb94  8.0)
						       (kc-dendrite-trb94  80.0)
						       (ka-trb94 5.0))
					       cell)

    (add-channel-in-element-with-density-motif "f" 0 '((na-trb94 30.0)
						       (Kdr-trb94 200.0)
						       (ca-dendrite-trb94 10.0)
						       (ahp-dendrite-trb94 8.0)
						       (kc-dendrite-trb94 80.0)
						       (ka-trb94 5.0))
					       cell)
  
    (add-channel-in-element-with-density-motif "g" 0 '((ca-dendrite-trb94 20.0)
						       (ahp-dendrite-trb94 8.0)
						       (kc-dendrite-trb94 40.0))
					       cell)

    (add-channel-in-element-with-density-motif "h" 0 '((ca-dendrite-trb94 30.0)
						       (ahp-dendrite-trb94 8.0)
						       (kc-dendrite-trb94 120.0))
					       cell)

    (add-channel-in-element-with-density-motif "i" 0 '((ca-dendrite-trb94 30.0)
						       (ahp-dendrite-trb94 8.0)
						       (kc-dendrite-trb94 120.0))
					       cell)
					     
    (add-channel-in-element-with-density-motif "j" 0 '((ca-dendrite-trb94 10.0)
						       (ahp-dendrite-trb94 8.0)
						       (kc-dendrite-trb94 40.0))
					       cell)
  
    (add-channel-in-element-with-density-motif "k" 0 '((ca-dendrite-trb94 10.0)
						       (ahp-dendrite-trb94  8.0)
						       (kc-dendrite-trb94 40.0))
					       cell)
    (loop for conc-int in (conc-ints) do
	  (element-parameter conc-int 'phi
			     (case (element-parameter (conc-int-cell-element conc-int) 'level)
			       (1 148.0)
			       (2 164.0)
			       (3 123.0)
			       (4 24.0)
			       (5 18.0)
			       (6 29.0)
			       (7 37.0)
			       (7o 47.0)
			       (8 13.0)
			       (9 16.0)
			       (9o 34.0)
			       (10 25.0)
			       (11 25.0)))
	  (when (element-parameter conc-int 'phi)
	    (element-parameter conc-int 'current-shell-base-coefficient-factor
			       (* (juxtamembrane-shell-volume conc-int)
				  (/ (* 2 Faraday) ; +2 for calcium valence
				     1.0d-6) ; 1d-6 to scale properly
				  (element-parameter conc-int 'phi)))))))

(defun traub-CA3-B ()
  (traub-CA3-branched "traub")
  (std-setup)  
  (enable-element-plot '("shaft" "c-1" "i-2221" "IS" "Axon-4")))


(push 'traub-CA3-B *CIRCUIT-CATALOG-FUNCTIONS*)





#|
;; ***************;; ***************;; ***************;; ***************;; ***************;; ***************
;; ***************;; ***************;; ***************;; ***************;; ***************;; ***************
;; The following are various printouts/scripts for validating the elements in the tree.
;; ***************;; ***************;; ***************;; ***************;; ***************;; ***************
;; ***************;; ***************;; ***************;; ***************;; ***************;; ***************

checking area quote on p81
(+ (* 880  16)
 (* 792 8)
 (* 471 4)
; (* 2403 1) 
 (* 3142 1)
 (* 1979 2)
 (* 1571 2)
 (* 1223 2)
 (* 1188 4)
 (* 942 4)
 (* 440 4)
 (* 603 8)
 (* 603 8)
; (* 942 1)
; (* 236 4)
)
54916



 (let ((level 0.0))
  (loop for seg-level in (sort (loop for seg in (segments)
      collect (list seg 
		    (case (element-parameter seg 'level)
		      (9o 9.5)
		      (7o 7.5)
		      (t (or (element-parameter seg 'level) 0.5)))))
      '< :key 'cadr)
      do
      (when (> (cadr seg-level) level)
	(setq level (cadr seg-level)) (format t "~%**** Level: ~A ****~%" level))
      (format t " Seg ~A radius ~A, length ~A, area ~A, phi ~A~%"
	      (segment-name (car seg-level))
	      (* 0.5 (segment-diameter (car seg-level)))
	      (segment-length (car seg-level))
	      (element-area (car seg-level))
	      (element-parameter (car (node-elements-of-type (car seg-level)
								 'conc-int))
				     'phi))))
**** Level: 0.5 ****
 Seg IS radius 2.0, length 75.0, area 942.47784, phi NIL
 Seg Axon-3 radius 0.5, length 75.0, area 235.61946, phi NIL
 Seg Axon-2 radius 0.5, length 75.0, area 235.61946, phi NIL
 Seg Axon-1 radius 0.5, length 75.0, area 235.61946, phi NIL
 Seg Axon-4 radius 0.5, length 75.0, area 235.61946, phi NIL

**** Level: 1 ****
 Seg a-111 radius 1.0, length 70.0, area 879.646, phi 148.0
 Seg a-121 radius 1.0, length 70.0, area 879.646, phi 148.0
 Seg a-112 radius 1.0, length 70.0, area 879.646, phi 148.0
 Seg a-122 radius 1.0, length 70.0, area 879.646, phi 148.0
 Seg a-411 radius 1.0, length 70.0, area 879.646, phi 148.0
 Seg a-421 radius 1.0, length 70.0, area 879.646, phi 148.0
 Seg a-412 radius 1.0, length 70.0, area 879.646, phi 148.0
 Seg a-422 radius 1.0, length 70.0, area 879.646, phi 148.0
 Seg a-211 radius 1.0, length 70.0, area 879.646, phi 148.0
 Seg a-221 radius 1.0, length 70.0, area 879.646, phi 148.0
 Seg a-212 radius 1.0, length 70.0, area 879.646, phi 148.0
 Seg a-222 radius 1.0, length 70.0, area 879.646, phi 148.0
 Seg a-311 radius 1.0, length 70.0, area 879.646, phi 148.0
 Seg a-321 radius 1.0, length 70.0, area 879.646, phi 148.0
 Seg a-312 radius 1.0, length 70.0, area 879.646, phi 148.0
 Seg a-322 radius 1.0, length 70.0, area 879.646, phi 148.0

**** Level: 2 ****
 Seg b-11 radius 1.57, length 40.0, area 789.16815, phi 164.0
 Seg b-12 radius 1.57, length 40.0, area 789.16815, phi 164.0
 Seg b-21 radius 1.57, length 40.0, area 789.16815, phi 164.0
 Seg b-22 radius 1.57, length 40.0, area 789.16815, phi 164.0
 Seg b-31 radius 1.57, length 40.0, area 789.16815, phi 164.0
 Seg b-32 radius 1.57, length 40.0, area 789.16815, phi 164.0
 Seg b-41 radius 1.57, length 40.0, area 789.16815, phi 164.0
 Seg b-42 radius 1.57, length 40.0, area 789.16815, phi 164.0

**** Level: 3 ****
 Seg c-1 radius 2.5, length 15.0, area 471.23892, phi 123.0
 Seg c-2 radius 2.5, length 15.0, area 471.23892, phi 123.0
 Seg c-3 radius 2.5, length 15.0, area 471.23892, phi 123.0
 Seg c-4 radius 2.5, length 15.0, area 471.23892, phi 123.0

**** Level: 4 ****
 Seg virtual-soma radius 15.0, length 25.5, area 2403.3184, phi 24.0

**** Level: 5 ****
 Seg shaft radius 5.0, length 50.0, area 3141.5928, phi 18.0

**** Level: 6 ****
 Seg f-2 radius 3.15, length 50.0, area 1979.2034, phi 29.0
 Seg f-1 radius 3.15, length 50.0, area 1979.2034, phi 29.0

**** Level: 7 ****
 Seg g-11 radius 2.5, length 50.0, area 1570.7964, phi 37.0
 Seg g-22 radius 2.5, length 50.0, area 1570.7964, phi 37.0

**** Level: 7.5 ****
 Seg goblique-2 radius 1.39, length 70.0, area 1222.7079, phi 47.0
 Seg goblique-1 radius 1.39, length 70.0, area 1222.7079, phi 47.0

**** Level: 8 ****
 Seg h-111 radius 1.57, length 60.0, area 1183.7522, phi 13.0
 Seg h-112 radius 1.57, length 60.0, area 1183.7522, phi 13.0
 Seg h-221 radius 1.57, length 60.0, area 1183.7522, phi 13.0
 Seg h-222 radius 1.57, length 60.0, area 1183.7522, phi 13.0

**** Level: 9 ****
 Seg i-2221 radius 1.25, length 60.0, area 942.47784, phi 16.0
 Seg i-2211 radius 1.25, length 60.0, area 942.47784, phi 16.0
 Seg i-1121 radius 1.25, length 60.0, area 942.47784, phi 16.0
 Seg i-1111 radius 1.25, length 60.0, area 942.47784, phi 16.0

**** Level: 9.5 ****
 Seg ioblique-221 radius 0.7, length 50.0, area 439.823, phi 34.0
 Seg ioblique-222 radius 0.7, length 50.0, area 439.823, phi 34.0
 Seg ioblique-111 radius 0.7, length 50.0, area 439.823, phi 34.0
 Seg ioblique-112 radius 0.7, length 50.0, area 439.823, phi 34.0

**** Level: 10 ****
 Seg j-22111 radius 0.8, length 60.0, area 603.1858, phi 25.0
 Seg j-22112 radius 0.8, length 60.0, area 603.1858, phi 25.0
 Seg j-22211 radius 0.8, length 60.0, area 603.1858, phi 25.0
 Seg j-22212 radius 0.8, length 60.0, area 603.1858, phi 25.0
 Seg j-11111 radius 0.8, length 60.0, area 603.1858, phi 25.0
 Seg j-11112 radius 0.8, length 60.0, area 603.1858, phi 25.0
 Seg j-11211 radius 0.8, length 60.0, area 603.1858, phi 25.0
 Seg j-11212 radius 0.8, length 60.0, area 603.1858, phi 25.0

**** Level: 11 ****
 Seg k-22111 radius 0.8, length 60.0, area 603.1858, phi 25.0
 Seg k-22112 radius 0.8, length 60.0, area 603.1858, phi 25.0
 Seg k-22211 radius 0.8, length 60.0, area 603.1858, phi 25.0
 Seg k-22212 radius 0.8, length 60.0, area 603.1858, phi 25.0
 Seg k-11111 radius 0.8, length 60.0, area 603.1858, phi 25.0
 Seg k-11112 radius 0.8, length 60.0, area 603.1858, phi 25.0
 Seg k-11211 radius 0.8, length 60.0, area 603.1858, phi 25.0
 Seg k-11212 radius 0.8, length 60.0, area 603.1858, phi 25.0
NIL
*






(let (level
      (ch-levels (sort (loop for ch in (channels)
			     collect (list ch (case (element-parameter (element-cell-element ch) 'level)
						(9o 9.5)
						(7o 7.5)
						(t (or (element-parameter (element-cell-element ch) 'level) 0.5)))))
		       '< :key 'cadr))
      out temp)
  (loop for ch-level in ch-levels
	collect (list (element (car ch-level)) (element-parameter (car ch-level) 'gbar-density)) into temp
	unless level do (setq level (cadr ch-level))
	when (> (cadr ch-level) level) do (push (list level temp) out) (setq level (cadr ch-level))
	finally (push (list level temp) out))
  (loop for level-list in out
	do (format t "~%~%  ****** Level ~A ****** ~%" (car level-list))
	(loop for type in (channel-types) do (element-parameter type `temp '()))
	(loop for ch-dens in (cadr level-list) do
	      (element-parameter (channel-type (element (car ch-dens))) `temp
				     (cons ch-dens (element-parameter (channel-type (element (car ch-dens))) 'temp))))
	(loop for type in (channel-types) when (element-parameter type `temp) do
	      (let ((first-time t))
		(loop for ch-dens in (element-parameter type `temp)
		      when (eq (car level-list)
			       (case (element-parameter (element-cell-element (car ch-dens)) 'level)
				 (9o 9.5)
				 (7o 7.5)
				 (t (or (element-parameter (element-cell-element (car ch-dens)) 'level) 0.5))))
		      do
		      (when first-time
			(format t "  Type ~A density:~%   " (channel-type (caar (element-parameter type `temp))))
			(setq first-time nil))
		      and do (format t "  ~A" (cadr ch-dens)))
		(unless first-time (format t " ~%" ))))))



  ****** Level 11 ****** 
  Type <Channel a-121-CA-DENDRITE-TRB94: type CA-DENDRITE-TRB94> density:
     10.0  10.0  10.0  10.0  10.0  10.0  10.0  10.0 
  Type <Channel a-211-KC-DENDRITE-TRB94: type KC-DENDRITE-TRB94> density:
     40.0  40.0  40.0  40.0  40.0  40.0  40.0  40.0 
  Type <Channel a-121-AHP-DENDRITE-TRB94: type AHP-DENDRITE-TRB94> density:
     8.0  8.0  8.0  8.0  8.0  8.0  8.0  8.0 


  ****** Level 10 ****** 
  Type <Channel Type CA-DENDRITE-TRB94> density:
     10.0  10.0  10.0  10.0  10.0  10.0  10.0  10.0 
  Type <Channel Type KC-DENDRITE-TRB94> density:
     40.0  40.0  40.0  40.0  40.0  40.0  40.0  40.0 
  Type <Channel Type AHP-DENDRITE-TRB94> density:
     8.0  8.0  8.0  8.0  8.0  8.0  8.0  8.0 


  ****** Level 9.5 ****** 
  Type <Channel Type CA-DENDRITE-TRB94> density:
     30.0  30.0  30.0  30.0 
  Type <Channel Type KC-DENDRITE-TRB94> density:
     120.0  120.0  120.0  120.0 
  Type <Channel Type AHP-DENDRITE-TRB94> density:
     8.0  8.0  8.0  8.0 


  ****** Level 9 ****** 
  Type <Channel Type CA-DENDRITE-TRB94> density:
     30.0  30.0  30.0  30.0 
  Type <Channel Type KC-DENDRITE-TRB94> density:
     120.0  120.0  120.0  120.0 
  Type <Channel Type AHP-DENDRITE-TRB94> density:
     8.0  8.0  8.0  8.0 


  ****** Level 8 ****** 
  Type <Channel Type CA-DENDRITE-TRB94> density:
     30.0  30.0  30.0  30.0 
  Type <Channel Type KC-DENDRITE-TRB94> density:
     120.0  120.0  120.0  120.0 
  Type <Channel Type AHP-DENDRITE-TRB94> density:
     8.0  8.0  8.0  8.0 


  ****** Level 7.5 ****** 
  Type <Channel Type CA-DENDRITE-TRB94> density:
     20.0  20.0 
  Type <Channel Type KC-DENDRITE-TRB94> density:
     40.0  40.0 
  Type <Channel Type AHP-DENDRITE-TRB94> density:
     8.0  8.0 


  ****** Level 7 ****** 
  Type <Channel Type CA-DENDRITE-TRB94> density:
     20.0  20.0 
  Type <Channel Type KC-DENDRITE-TRB94> density:
     40.0  40.0 
  Type <Channel Type AHP-DENDRITE-TRB94> density:
     8.0  8.0 


  ****** Level 6 ****** 
  Type <Channel Type CA-DENDRITE-TRB94> density:
     10.0  10.0 
  Type <Channel Type KC-DENDRITE-TRB94> density:
     80.0  80.0 
  Type <Channel Type KDR-TRB94> density:
     200.0  200.0 
  Type <Channel Type KA-TRB94> density:
     5.0  5.0 
  Type <Channel Type AHP-DENDRITE-TRB94> density:
     8.0  8.0 
  Type <Channel Type NA-TRB94> density:
     30.0  30.0 


  ****** Level 5 ****** 
  Type <Channel Type CA-DENDRITE-TRB94> density:
     10.0 
  Type <Channel Type KC-DENDRITE-TRB94> density:
     80.0 
  Type <Channel Type KDR-TRB94> density:
     200.0 
  Type <Channel Type KA-TRB94> density:
     5.0 
  Type <Channel Type AHP-DENDRITE-TRB94> density:
     8.0 
  Type <Channel Type NA-TRB94> density:
     30.0 


  ****** Level 4 ****** 
  Type <Channel Type CA-SOMA-TRB94> density:
     10.0 
  Type <Channel Type AHP-SOMA-TRB94> density:
     8.0 
  Type <Channel Type KDR-TRB94> density:
     1350.0 
  Type <Channel Type KA-TRB94> density:
     5.0 
  Type <Channel Type NA-TRB94> density:
     1000.0 
  Type <Channel Type KC-SOMA-TRB94> density:
     200.0 


  ****** Level 3 ****** 
  Type <Channel Type CA-DENDRITE-TRB94> density:
     10.0  10.0  10.0  10.0 
  Type <Channel Type KC-DENDRITE-TRB94> density:
     80.0  80.0  80.0  80.0 
  Type <Channel Type KDR-TRB94> density:
     150.0  150.0  150.0  150.0 
  Type <Channel Type KA-TRB94> density:
     5.0  5.0  5.0  5.0 
  Type <Channel Type AHP-DENDRITE-TRB94> density:
     8.0  8.0  8.0  8.0 
  Type <Channel Type NA-TRB94> density:
     10.0  10.0  10.0  10.0 


  ****** Level 2 ****** 
  Type <Channel Type CA-DENDRITE-TRB94> density:
     10.0  10.0  10.0  10.0  10.0  10.0  10.0  10.0 
  Type <Channel Type KC-DENDRITE-TRB94> density:
     40.0  40.0  40.0  40.0  40.0  40.0  40.0  40.0 
  Type <Channel Type KA-TRB94> density:
     5.0  5.0  5.0  5.0  5.0  5.0  5.0  5.0 
  Type <Channel Type AHP-DENDRITE-TRB94> density:
     8.0  8.0  8.0  8.0  8.0  8.0  8.0  8.0 


  ****** Level 1 ****** 
  Type <Channel Type CA-DENDRITE-TRB94> density:
     10.0  10.0  10.0  10.0  10.0  10.0  10.0  10.0  10.0  10.0  10.0  10.0  10.0  10.0  10.0  10.0 
  Type <Channel Type KC-DENDRITE-TRB94> density:
     40.0  40.0  40.0  40.0  40.0  40.0  40.0  40.0  40.0  40.0  40.0  40.0  40.0  40.0  40.0  40.0 
  Type <Channel Type AHP-DENDRITE-TRB94> density:
     8.0  8.0  8.0  8.0  8.0  8.0  8.0  8.0  8.0  8.0  8.0  8.0  8.0  8.0  8.0  8.0 


  ****** Level 0.5 ****** 
  Type <Channel Type KDR-AX-TRB94> density:
     2500.0  2500.0  2500.0  2500.0  2500.0 
  Type <Channel Type NA-AX-TRB94> density:
     5000.0  5000.0  5000.0  5000.0  5000.0 
NIL
* 

(let ((seg (element "i-2221")))
  (print-element seg)
  (loop for ch in (node-elements-of-type seg 'channel)
	do (format t "ch ~A, density ~A~%" 
		   ch (/ 
		       (* 1.0e-3 (get-element-gbar-reference ch))
			 (* 1.0e-8 (element-area seg))
			 ))))



|#



  

#|
;; ***************;; ***************;; ***************;; ***************;; ***************;; ***************
;; ***************;; ***************;; ***************;; ***************;; ***************;; ***************
;; The following are some scripts relevent to specific figures in the paper.
;; ***************;; ***************;; ***************;; ***************;; ***************;; ***************
;; ***************;; ***************;; ***************;; ***************;; ***************;; ***************

;; Apropos fig 5
(synapse-type-def
 '(traub-94-tonic-ampa
   (gbar-density . 50.0)
   (e-rev . 0.0)
   (control . tonic)))

(synapse-type-def
 '(traub-94-tonic-gaba-a
   (gbar-density . 50.0)
   (e-rev . -75.0)
   (control . tonic)))

(let ((ampa-segs (loop for seg in (segments) when (and (numberp (element-parameter seg 'level))
						       (= 6 (element-parameter seg 'level)))
		       collect seg))
      (gaba-segs (loop for seg in (segments) when (or (and (numberp (element-parameter seg 'level))
							   (= 9 (element-parameter seg 'level)))
						      (eq '9o (element-parameter seg 'level)))
		       collect seg)))
  (loop for seg in ampa-segs do
	(set-element-gbar-ref (create-synapse seg 'traub-94-tonic-ampa) (/ 8.0e-3 (length ampa-segs))))
  (loop for seg in gaba-segs do
	(set-element-gbar-ref (create-synapse seg 'traub-94-tonic-gaba-a) (/ 20.0e-3 (length gaba-segs)))))


;; Apropos Fig 6
(synapse-type-def
 `(auto-traub-ampa
   (gbar-density . 50.0)
   (e-rev . 0.0)
   (control . auto)
   (waveform . ,(loop for time from 0.0 to 100.0 by 0.2 collecting
		 (loop for delay from 0.0 to 20 by 5
		       summing (if (< time delay) 0.0
				   (* (- time delay) (exp (/ (- time delay) -2.0)))))))
   (waveform-time-interval . 0.2)))

(set-element-gbar-ref (create-synapse (element "h-222")  'auto-traub-ampa) 1.25e-3
		      ; this looks to be twice to big according to ".92nS peak conductance"
					; 2.50e-3 
		      )


;; Apropos Fig 7
(synapse-type-def
 `(auto-traub-gaba-a
   (gbar-density . 50.0)
   (e-rev . 0.0)
   (control . auto)
   (waveform . ,(loop for time from 0.0 to 100.0 by 0.2 collecting
		 (if (< time 1) 1.0
		     (exp (/ (- time 1.0) -50.0)))))
   (waveform-time-interval . 0.2)))




;; ***************;; ***************;; ***************;; ***************;; ***************;; ***************
;; ***************;; ***************;; ***************;; ***************;; ***************;; ***************
;; The following are a variety of scripts looking at the effects of g-axial calculation method, soma
;; geometry, holding currents, etc (?).
;; ***************;; ***************;; ***************;; ***************;; ***************;; ***************
;; ***************;; ***************;; ***************;; ***************;; ***************;; ***************


(loop for soma-cyl in '(t nil) do
      (element-parameter *soma* 'SOMA-CYLINDER soma-cyl)
      (loop for g-axial-method in '(:single-leg :average-r :average-g) do
	    (element-parameter (cell-type *cell*) 'g-axial-computation g-axial-method)
	    (set-circuit-elements-parameters)
	    (let ((*simulation-plot-window-comment*
		  (format
		   nil
		   "fig6~%g-axial: ~A, ~A~%Rin ~,2eMohms~%~A"
			  (element-parameter (cell-type *cell*) 'g-axial-computation)
			  (if soma-cyl "soma cylinder" "soma sphere")
			  (cell-z-discrete-in-cell *cell*)
			  *simulation-name*))	
		  (user-stop-time 100) 
		  (*CREATE-NEW-SIMULATION-PLOTS* t)
		  (*plot-window-width* 500)
		  (*plot-window-height* 600)
		  (*plot-nodes* '("b-41" "h-111" "h-222" "shaft" "IS" "Axon-4")))
	      (print-element *cell*)
	      (gotimed)
	      (ARRANGE-PLOT-WINDOWS)
	      (print-windows (windows-of-mode :STANDARD-PLOT) t)
	      (clear-windows-of-mode :standard-plot))))

;; Looking at effect of g-axial calculation method and soma geometry.
(loop for soma-cyl in '(t nil) do
      (element-parameter *soma* 'SOMA-CYLINDER soma-cyl)
      (loop for g-axial-method in '(:single-leg :average-r :average-g) do
	    (element-parameter (cell-type *cell*) 'g-axial-computation g-axial-method)
	    (set-circuit-elements-parameters)
	    (let ((*simulation-plot-window-comment*
		  (format
		   nil
		   "fig7B~%g-axial: ~A, ~A~%Rin ~,2eMohms~%~A"
			  (element-parameter (cell-type *cell*) 'g-axial-computation)
			  (if soma-cyl "soma cylinder" "soma sphere")
			  (cell-z-discrete-in-cell *cell*)
			  *simulation-name*))	
		  (user-stop-time 100) 
		  (*CREATE-NEW-SIMULATION-PLOTS* t)
		  (*plot-window-width* 500)
		  (*plot-window-height* 600)
		  (*plot-nodes* '("j-22211" "h-111" "h-222" "IS"))
		  )
	      (print-element *cell*)
	      (gotimed)
	      (ARRANGE-PLOT-WINDOWS)
	      (print-windows (windows-of-mode :STANDARD-PLOT) t)
	      (clear-windows-of-mode :standard-plot))))

;; Looking at effect of g-axial calculation method and soma geometry.
(loop for soma-cyl in '(t nil) do
      (element-parameter *soma* 'SOMA-CYLINDER soma-cyl)
      (loop for g-axial-method in '(:single-leg :average-r :average-g) do
	    (element-parameter (cell-type *cell*) 'g-axial-computation g-axial-method)
	    (set-circuit-elements-parameters)
	    (setq *simulation-plot-window-comment*
		  (format nil "g-axial: ~A, ~A~%Rin ~,2eMohms~%~A"
			  (element-parameter (cell-type *cell*) 'g-axial-computation)
			  (if soma-cyl "soma cylinder" "soma sphere")
			  (cell-z-discrete-in-cell *cell*)
			  *simulation-name*))	
	    (let ((*CREATE-NEW-SIMULATION-PLOTS* t)
		  (*plot-window-width* 400)
		  (*plot-window-height* 600)
		  (*plot-nodes* '("i-2221" "IS" "Axon-4")))
	      (gotimed))))

;; Looking at effect of g-axial calculation method and soma geometry.
(progn (loop for soma-cyl in '(t nil) do
	     (element-parameter *soma* 'SOMA-CYLINDER soma-cyl)
	     (loop for g-axial-method in '(:single-leg :average-r :average-g) do
		   (element-parameter (cell-type *cell*) 'g-axial-computation g-axial-method)
		   (set-circuit-elements-parameters)
		   (let ((*simulation-plot-window-comment*
			  (format
			   nil "fig9B~%g-axial: ~A, ~A~%Rin ~,2eMohms~%~A"
			   (element-parameter (cell-type *cell*) 'g-axial-computation)
			   (if soma-cyl "soma cylinder" "soma sphere")
			   (cell-z-discrete-in-cell *cell*)
			   *simulation-name*))	
			 (user-stop-time 100) 
			 (*CREATE-NEW-SIMULATION-PLOTS* t)
			 (*plot-window-width* 500) (*plot-window-height* 600)
			 (*plot-nodes* '("j-22211" "IS")))
		     (print-element *cell*)
		     (gotimed))))
       (ARRANGE-PLOT-WINDOWS))


;; Looking at effect of g-axial calculation method and soma geometry.
(progn (loop for soma-cyl in '(t nil) do
	     (element-parameter *soma* 'SOMA-CYLINDER soma-cyl)
	     (loop for g-axial-method in '(:single-leg :average-r :average-g) do
		   (element-parameter (cell-type *cell*) 'g-axial-computation g-axial-method)
		   (set-circuit-elements-parameters)
		   (let ((*simulation-plot-window-comment*
			  (format
			   nil "fig9B, -0.07nA@soma start @0s ~%g-axial: ~A, ~A~%Rin ~,2eMohms~%~A"
			   (element-parameter (cell-type *cell*) 'g-axial-computation)
			   (if soma-cyl "soma cylinder" "soma sphere")
			   (cell-z-discrete-in-cell *cell*)
			   *simulation-name*))	
			 (user-stop-time 100) 
			 (*CREATE-NEW-SIMULATION-PLOTS* t)
			 (*plot-window-width* 500) (*plot-window-height* 600)
			 (*plot-nodes* '("j-22211" "IS")))
		     (print-element *cell*)
		     (gotimed))))
       (ARRANGE-PLOT-WINDOWS 3)
       (print-windows (windows-of-mode :STANDARD-PLOT) t)
       (clear-windows-of-mode :standard-plot))

;; Looking at effect of g-axial calculation method and soma geometry.
(loop for is-g-a from 200.0 to 300.0 by 100.0 do
      (element-parameter "IS" 'cytoplasmic-resistivity is-g-a)
      (loop for soma-current downfrom 0.0 to -0.1 by .02 do
	    (add-constant-current-to-element *soma* soma-current)
	    (loop for soma-cyl in '(t nil) do
		  (element-parameter *soma* 'SOMA-CYLINDER soma-cyl)
		  (loop for g-axial-method in '(:single-leg :average-r :average-g) do
			(element-parameter (cell-type *cell*) 'g-axial-computation g-axial-method)
			(set-circuit-elements-parameters)
			(let ((*simulation-plot-window-comment*
			       (format
				nil "fig2, ISga ~a, ~,2enA@soma @0s, axonstim@700ms~%g-axial: ~A, ~A~%Rin ~,2eMohms~%~A"
				is-g-a
				soma-current
				(element-parameter (cell-type *cell*) 'g-axial-computation)
				(if soma-cyl "soma cylinder" "soma sphere")
				(cell-z-discrete-in-cell *cell*)
				*simulation-name*))	
			      (user-stop-time 1000) 
			      (*CREATE-NEW-SIMULATION-PLOTS* t)
			      (*plot-window-width* 500) (*plot-window-height* 600))
			  (print-element *cell*)
			  (gotimed))))
	    (ARRANGE-PLOT-WINDOWS 3)
	    (print-windows (windows-of-mode :STANDARD-PLOT) t)
	    (clear-windows-of-mode :standard-plot)))


;; Looking at effect of g-axial calculation method and soma geometry.
(let (*auto-update-sim-name*)
  (loop for is-g-a from 100.0 to 100.0 by 100.0 do
	(element-parameter "IS" 'cytoplasmic-resistivity is-g-a)
	(loop for soma-current downfrom 0.0 to -0.1 by .02 do
	      (add-constant-current-to-element *soma* soma-current)
	      (loop for soma-cyl in '(t nil) do
		    (element-parameter *soma* 'SOMA-CYLINDER soma-cyl)
		    (loop for g-axial-method in '(:single-leg :average-r :average-g) do
			  (element-parameter (cell-type *cell*) 'g-axial-computation g-axial-method)
			  (set-circuit-elements-parameters)
			  (update-simulation-time-stamp-and-name)
			  (let (*resurrect-plots*
				(*simulation-plot-window-comment*
				 (format
				  nil "fig2, ISga ~a, ~,2enA@soma @0s, axonstim@22ms~%g-axial: ~A, ~A~%Rin ~,2eMohms~%~A"
				  is-g-a
				  soma-current
				  (element-parameter (cell-type *cell*) 'g-axial-computation)
				  (if soma-cyl "soma cylinder" "soma sphere")
				  (cell-z-discrete-in-cell *cell*)
				  *simulation-name*))	
				(user-stop-time 80) 
				(*include-printer-flags* nil)
				(*shell-lpr-command* "ps2epson2")
				(*print-together* t)
				(*printer* "")
				(*ps-landscape-p* t)
				(*CREATE-NEW-SIMULATION-PLOTS* t)
				(*plot-window-width* 500) (*plot-window-height* 600))
			    (print-element *cell*)
			    (gotimed))))
	      (ARRANGE-PLOT-WINDOWS 3)
	      (print-windows (windows-of-mode :STANDARD-PLOT) t)
	      (clear-windows-of-mode :standard-plot))))

;; Looking at effect of g-axial calculation method and soma geometry and constant holding current.
(let (*auto-update-sim-name*)
  (loop for soma-current downfrom 0.0 to -0.12 by .02 do
	(add-constant-current-to-element *soma* soma-current)
	(loop for soma-cyl in '(t nil) do
	      (element-parameter *soma* 'SOMA-CYLINDER soma-cyl)
	      (loop for g-axial-method in '(:single-leg :average-r :average-g) do
		    (element-parameter (cell-type *cell*) 'g-axial-computation g-axial-method)
		    (set-circuit-elements-parameters)
		    (update-simulation-time-stamp-and-name)
		    (let ((*simulation-plot-window-comment*
			   (format
			    nil "fig9b ~,2enA@soma @0s,~%g-axial: ~A, ~A~%Rin ~,2eMohms~%~A"
			    soma-current (element-parameter (cell-type *cell*) 'g-axial-computation)
			    (if soma-cyl "soma cylinder" "soma sphere") (cell-z-discrete-in-cell *cell*)
			    *simulation-name*))	
			  (user-stop-time 80) 
			  (*include-printer-flags* nil)
			  (*shell-lpr-command* "ps2epson2")
			  (*print-together* t)
			  (*printer* "")
			  (*ps-landscape-p* t)
			  (*CREATE-NEW-SIMULATION-PLOTS* t)
			  (*plot-window-width* 500) (*plot-window-height* 600))
		      (print-element *cell*)
		      (gotimed))))
	(ARRANGE-PLOT-WINDOWS 3)
	(print-windows (windows-of-mode :STANDARD-PLOT) t)
	(clear-windows-of-mode :standard-plot)))

|#
