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

;; From -
;;   Computer simulations of a morphologically reconstructed
;;   CA3 hippocampal neuron
;;   M.Migliore, EP. Cook, DB. Jaffe, DA. Turner, D. Johnston
;;   J. Neurophysiol, March 1995, vol. 73, no. 3, 1157-1168
;;

;; See also parameters/migliore95-chs.lisp

(cell-type-def
 '(MIGLIORE95-HPC
   (rm . 60000)				; ohms-cm2
   (ri  . 200)				; ohm-cm
   (cm . 1)				; uF/cm2
   (v-leak . -65)			; mV
   (e-k . -91)
   (e-k-dependence . :fixed)
   (e-na . 50)
   (e-na-dependence . :fixed)))

(defvar mig95-k-ca-channels '(ca-t-mig95 ca-n-mig95 ca-l-mig95 ka-mig95 km-mig95 kahp-mig95 kdr-mig95 kc-Mig95))

(defun mig95-set-e-revs-to-cell-type () (loop for type in mig95-k-ca-channels do (setf (channel-type-use-defined-e-rev (element type)) nil)))

(defun mig95-hippo ()
  (let ((cell (ball-and-sticks :name "mig95-hippo" :cell-type 'MIGLIORE95-HPC)))
    (process-circuit-structure)
    (loop for seg in (segments-out (car (trunk-segments))) ; "Apical" segments
	  when (< (distance-to-soma seg) 500) ; apical-extent-of-na-k
	  do (create-element *soma* seg 'na-Mig95))
    (create-element (cell-elements) mig95-k-ca-channels)
    (add-isource *soma*)
    ;; (mig95-plot-settings-hippo)
    (mig95-set-e-revs-to-cell-type)))

;; Load c12861-ca1-max-red-tree.lisp first.
(defun mig95-nak-soma (&optional (apical-extent-of-na-k 150))
  (load-surf-home-file "src/hippocampus/c12861-ca1-max-red-tree.lisp") 
  (ca1-max-red :name "mig95-c12861-ca1" :cell-type 'MIGLIORE95-HPC)
  (process-circuit-structure)
  (loop for seg in (segments-out (element 46)) ; "Apical" segments. By eye.
	when (< 100 (distance-to-soma seg) 250)
	do (create-element seg 'mig95-ampa-abs)
	when (< (distance-to-soma seg) apical-extent-of-na-k)
	do (create-element seg 'na-Mig95))
  (create-element *soma* 'na-Mig95)
  (add-isource *soma*)
  (create-element (cell-elements) mig95-k-ca-channels)
  (element-type-parameter 'mig95-ampa-abs 'gbar-ref
			  ;; 5nS total, spread out over all the syns.
			  (/ 5.0e-3 (length (synapses)))
			  t)
  (add-events (SYNAPSEs) (loop for time from 10.0 to 520 by 2 collect time))
  (mig95-set-e-revs-to-cell-type)
  (mig95-plot-settings))

(defun mig95-n120 (&optional (apical-extent-of-na-k 150))
  (setq *plot-channels-by-major-ion* t)
  (n120-max-red "mig95-n120" 'MIGLIORE95-HPC)
  (process-circuit-structure)
  (loop for seg in (segments-out (element "17")) ; "Apical" segments. By eye.
	when (< (distance-to-soma seg) apical-extent-of-na-k)
	do (create-element seg 'na-Mig95))
  (add-isource *soma*)
  (create-element (or (soma-segments) *soma*) 'na-Mig95)
  (create-element (cell-elements) mig95-k-ca-channels)
  (mig95-set-e-revs-to-cell-type))

(defun mig95-plot-settings ()
  (setq *plot-channels-by-major-ion* t)
  (setq *PLOT-NODE-VOLTAGES-p* T)
  (setq *PLOT-SOMA-VOLTAGE-p* T)
  (setq *PLOT-CHANNEL-CURRENTS-p* T)
  (setq *PLOT-SHELL-1-CONCENTRATIONS-p* T))

(defun mig95-plot-settings-hippo ()
  (setq *plot-channels-by-major-ion* t)
  (enable-element-plot`("Hippo-5" "Hippo-soma"))
  (enable-element-plot `("Hippo-soma-KC-MIG95"  "Hippo-soma-kdr-mig95"  "Hippo-soma-CA-L-JAFFE"
			 "Hippo-soma-CA-N-JAFFE"  "Hippo-soma-CA-T-JAFFE"  "Hippo-soma-na-Mig95") :all)
  (enable-element-plot `( "Hippo-soma-CA-IN-jaffe-94"  ))
  (enable-element-plot 'isource)

  (setq *PLOT-NODE-VOLTAGES-P* T)
  (setq *PLOT-SOMA-VOLTAGE-p* T)
  (setq *PLOT-CHANNEL-CURRENTS-p* T)
  (setq *PLOT-CHANNEL-CONDUCTANCES-p* t)
  (setq *PLOT-CHANNEL-REVERSAL-POTENTIALS-p* NIL)
  (setq *PLOT-SHELL-1-CONCENTRATIONS-p* T))

  
#|
(defun test-mig (&optional (include-control t))
  (unblock-all-channel-types)
  (let ((user-stop-time 160) *BEEP-AFTER-SURF* *BEEP-AFTER-gc*
	*automatic-voltage-plot-scaling
	(*soma-voltage-plot-min -80)(*soma-voltage-plot-max 20)
	(*voltage-plot-min -80)(*voltage-plot-max 20)
	)
    (reset-non-unity-channel-type-gbar-modulation)
    (when include-control
      (cc-steps 0.2 0.4 .2 :individual-plots t
		:timeit t :current-start-time 10 :current-stop-time 100
		:extra-comment
		(loop for type in '(ka-mig95 km-mig95 kahp-mig95 kdr-mig95 kc-Mig95)
		      collect (format nil "~A ~,2f" type (or (element-parameter type 'gbar-modulation) 1))
		      into out
		      finally (return (concatenate-string-list (cons "" out) :string-count-to-add-linefeed 1)))))
    (loop for type in '(ka-mig95 km-mig95 kahp-mig95 kdr-mig95 kc-Mig95) do
	  (reset-non-unity-channel-type-gbar-modulation)
	  (loop for gbar-mod in '(2.0 5.0) do
		(element-parameter type 'gbar-modulation gbar-mod t)
		(cc-steps 0.2 0.4 .2 :individual-plots t
			  :timeit t :current-start-time 10 :current-stop-time 100
			  :extra-comment
			  (loop for type in '(ka-mig95 km-mig95 kahp-mig95 kdr-mig95 kc-Mig95)
				collect (format nil "~A ~,2f" type (or (element-parameter type 'gbar-modulation) 1))
				into out
				finally (return
					  (concatenate-string-list (cons "" out) :string-count-to-add-linefeed 1))))))))


(defun test-mig (&optional (include-control t))
  (unblock-all-channel-types)
  (let ((user-stop-time 50) *BEEP-AFTER-SURF* *BEEP-AFTER-gc*
	(*CREATE-NEW-SIMULATION-PLOTS* t)
	*automatic-voltage-plot-scaling
	(*soma-voltage-plot-min -80)(*soma-voltage-plot-max 20) (*voltage-plot-min -80)(*voltage-plot-max 20))
    (element-parameter '(ka-mig95 km-mig95 kahp-mig95 kdr-mig95 kc-Mig95) 'gbar-modulation 1.0 t)
    (when include-control
      (let ((*simulation-plot-window-comment*
	     (loop for type in '(ka-mig95 km-mig95 kahp-mig95 kdr-mig95 kc-Mig95)
		   collect (format nil "~A ~,2f" type (or (element-parameter type 'gbar-modulation) 1))
		   into out
		   finally (return (concatenate-string-list out :string-count-to-add-linefeed 1)))))
	(goferit)(setq *CREATE-NEW-SIMULATION-PLOTS* t)))
    (loop for type in '(ka-mig95 km-mig95 kahp-mig95 kdr-mig95 kc-Mig95) do
	  (element-parameter '(ka-mig95 km-mig95 kahp-mig95 kdr-mig95 kc-Mig95) 'gbar-modulation 1.0 t))
	  (loop for gbar-mod in '(2.0 5.0) do
		(element-parameter type 'gbar-modulation gbar-mod t)
		(let ((*simulation-plot-window-comment*
		       (loop for type in '(ka-mig95 km-mig95 kahp-mig95 kdr-mig95 kc-Mig95)
			     collect (format nil "~A ~,2f" type (or (element-parameter type 'gbar-modulation) 1))
			     into out
			     finally (return (concatenate-string-list out :string-count-to-add-linefeed 1)))))
		  (setq *CREATE-NEW-SIMULATION-PLOTS* t)
		  (goferit))))))

(defun test-mig-syns ()
  (unblock-all-channel-types)
  (let ((user-stop-time 900) *BEEP-AFTER-SURF* *BEEP-AFTER-gc*
	(*CREATE-NEW-SIMULATION-PLOTS* t)
	*automatic-voltage-plot-scaling
	(*soma-voltage-plot-min -80)(*soma-voltage-plot-max 20) (*voltage-plot-min -80)(*voltage-plot-max 20))

    (reset-non-unity-channel-type-gbar-modulation)

    (loop for gbar-mod in '(1.0 2.0 5.0) do
	  (element-parameter 'km-mig95 'gbar-modulation gbar-mod t)
	  (loop for gbar-mod in '(1.0 2.0 5.0) do
		(element-parameter 'ka-mig95 'gbar-modulation gbar-mod t)
		(loop for gbar-mod in '(1.0 2.0 5.0) do
		      (element-parameter 'kdr-mig95 'gbar-modulation gbar-mod t)

		      (core-mig-syn))))))
		      


(defun core-mig-syn ()
  (let ((*simulation-plot-window-comment*
	 (loop for type in '(ka-mig95 km-mig95 kahp-mig95 kdr-mig95 kc-Mig95)
	       collect (format nil "~A ~,2f" type (or (element-parameter type 'gbar-modulation) 1))
	       into out
	       finally (return (concatenate-string-list out :string-count-to-add-linefeed 1)))))
    (setq *CREATE-NEW-SIMULATION-PLOTS* t)
    (goferit)))

;; useful functions (ARRANGE-pLOT-WINDOWS t) (default-window-font-menu)




|#
