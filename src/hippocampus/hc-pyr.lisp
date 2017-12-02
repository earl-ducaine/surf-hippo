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

;(LOAD-SURF-USER-FILE "~/surf-hippo/data/HC-PYR/8_20_1996/6326856-reference.elts")      
;(LOAD-SURF-USER-FILE "~/surf-hippo/data/HC-PYR/8_21_1996/6333862.elts")
;
;(load-surf-user-file "~/surf-hippo/data/HC-PYR/8_21_1996/6336136.elts")
;(load-surf-user-file "~/surf-hippo/data/HC-PYR/9_16_1996/6559771.elts")
;(load-surf-user-file "~/surf-hippo/data/HC-PYR/9_21_1996/6601510.elts")
;(load-surf-user-file "~/surf-hippo/data/HC-PYR/9_22_1996/6612864.elts")
;(load-surf-user-file "~/surf-hippo/data/HC-PYR/9_23_1996/6614942.elts")
;(load-surf-user-file "~/surf-hippo/data/HC-PYR/9_23_1996/6619315.elts")
;(load-surf-user-file "~/surf-hippo/data/HC-PYR/9_23_1996/6621480.elts")
;(load-surf-user-file "~/surf-hippo/data/HC-PYR/9_24_1996/6629465.elts")
;(load-surf-user-file "~/surf-hippo/data/HC-PYR/9_24_1996/6630053.elts")
;(load-surf-user-file "~/surf-hippo/data/HC-PYR/9_25_1996/6638363.elts")
;(load-surf-user-file "~/surf-hippo/data/HC-PYR/9_25_1996/6638363-soft.elts")
;(load-surf-user-file "~/surf-hippo/data/HC-PYR/9_30_1996/6681658.elts")

;; 4state, conc-int with buffer shell 3, 500 
;(load-surf-user-file "~/surf-hippo/data/HC-PYR/10_5_1996/6723980.elts")

;(load-surf-user-file "~/surf-hippo/data/HC-PYR/10_12_1996/6785512.elts")

;(load-surf-user-file "~/surf-hippo/data/HC-PYR/10_14_1996/6800567.elts")


;(load-surf-user-file "~/surf-hippo/data/HC-PYR/10_15_1996/6810528.elts")

;(load-surf-user-file "~/surf-hippo/data/HC-PYR/10_16_1996/6820388.elts")

; constant tau-y for ia 
;(load-surf-user-file "~/surf-hippo/data/HC-PYR/10_25_1996/6895509.elts")

; killing voltage-dep of ic activation
;(load-surf-user-file "~/surf-hippo/data/HC-PYR/10_25_1996/6896205.elts")

;(load-surf-user-file "~/surf-hippo/data/HC-PYR/10_25_1996/6898130.elts")



;; also modified i-d (gbar 0.06uS, slower inactivation)
;; beta 10.0 for shell 2 and 3, alpha-s 0.01, juxtamembrane-shell-thickness 0.2
(load-surf-user-file "~/surf-hippo/data/HC-PYR/10_16_1996/6821250.elts")


#|
Concentration Integrator Type CA-IN-GEN (class MULTI-SHELL): intracellular, species CA
  3 shells (interdigitated juxta-membrane pair and inner) + core
  Inner shell(s) thickness: 0.50um, shell 3 thickness: 0.00um
  Proportion of juxta-membrane shell assigned to shell 1: 0.00
  Shells 1&2 interdigitation (partitioning) coefficient: 1.00e+0 [1/um]
  Resting free concentration: 5.00e-5mM
  D-12 8.00e-6   D-13 0.00e+0   D-23 8.00e-6   D-3core 0.00e+0 [cm^2/sec]
  Shell 1 instantaneous buffer ratio (bound[X]/[X]): 100.0
  Shell 2 instantaneous buffer ratio (bound[X]/[X]): 100.0
  Shell 3 instantaneous buffer ratio (bound[X]/[X]): 100.0
  Instantaneous buffer enabled

Membrane Ion Pump Type CA-JAFFE-94 (class MM): species CA, Q10 1.0 T_ref 27.0
  V-max 6.00e-11 [mM ms^-1 cm^-2], Kd 10.00e-3 mM
|#

;;(load-surf-user-file "~/surf-hippo/data/HC-PYR/12_8_1996/7275846.elts")
;; (load-surf-user-file "~/surf-hippo/data/HC-PYR/3_14_1997/8107039.elts")

;;(load-surf-user-file "~/surf-hippo/data/HC-PYR/3_16_1997/8121762.elts")

;;(load-surf-user-file "~/surf-hippo/data/HPC/3_16_1997/8123576.elts")

(load-surf-user-file "~/surf-hippo/data/HPC/3_17_1997/8132647.elts")



(defun hpc ()
  (setq *fix-e-k* nil *fix-e-na* nil)
  (hippo)) 

(topload 'hpc)

(pulse-list (add-isource *soma*) '((0.0 1.5 1.1)))

(create-element *soma* '(
			  
					; NA-BURST-MARKOV 
			 NA-4STATE-exp-GEN ; NA-6STATE-exp-GEN
			 ;; NA-4STATE-PWL-GEN
			 ;; na-4state-gen
			 ;;NA-RF1
			 ;;na-rf2
			 ;;NA-FSBG-GEN

			 KDR-sah	;KDR-GEN
			 KM-GEN
			 kd-gen
			  
			 kc-markov	;kc-gen
			 kahp-gen
			 KA-gen

			 h-mac-etal93

			 ca-l-gen
			 ca-n-gen
			 ca-t-gen
			 ))



(loop for ch in (channels) when (element-of-ion-type-p ch 'ca)
      do (element-parameter ch 'conc-int-delta 0.34)
      (set-conc-integrators-parameters))

(revamp-type-parameters)


(std-setup)


(load-surf-user-file "~/surf-hippo/data/HC-PYR/12_8_1996/7275846-revised.plot-settings.lisp")
