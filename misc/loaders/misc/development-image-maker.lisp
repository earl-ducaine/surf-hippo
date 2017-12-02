;;; -*- Mode: lisp; package: user; base: 10;  Syntax: Common-lisp; -*-
;;; Lyle Borg-Graham, Equipe Cognisciences, Institut Alfred Fessard, CNRS
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;              The Surf-Hippo Neuron Simulator                    ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This code was written as part of the Surf-Hippo project at the  ;;;
;;; Center for Biological Information Processing, Department of     ;;;
;;; Brain and Cognitive Sciences, Massachusetts Institute of        ;;;
;;; Technology, and currently at the Equipe Cognisciences, Institut ;;;
;;; Alfred Fessard, CNRS and has been placed in the public domain.  ;;;
;;; If you are using this code or any part of Surf-Hippo, please    ;;;
;;; contact surf-hippo@ai.mit.edu to be put on the mailing list.    ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;; This compiles and loads the Garnet and Surf-Hippo systems, according to the Unix environment
;; variables GARNETHOME and SURFHOME. The image is stored in the file given by
;;
;;                (format nil "~Aimage" *surf-home*) 

;; This file is loaded into a fresh CMUCL Ilisp environment, or one with GARNET added.

(defvar surf-hippo-compile t)

(load (concatenate 'string (cdr (assoc :SURFHOME lisp::*environment-list*))
		     "loaders/main-loader")
	:if-source-newer :compile)




(opal:make-image (format nil "~Aimage" *surf-home*))
