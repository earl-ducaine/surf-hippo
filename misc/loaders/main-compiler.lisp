;; -*- Mode: LISP; Syntax: Common-Lisp; Package: USER; Base: 10 -*-
;; Lyle Graham, Equipe Cognisciences, Institut Alfred Fessard, CNRS
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                                  ;;; 
;;;                   The Surf-Hippo Neuron Simulator                                ;;; 
;;;                                                                                  ;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                                  ;;; 
;;; This code was written as part of the Surf-Hippo Project at Center for Biological ;;; 
;;; Information Processing, Department of Brain and Cognitive Sciences,              ;;; 
;;; Massachusetts Institute of Technology, and currently at the Unite de             ;;; 
;;; Neurosciences Integratives et Computationnelles, Institut Federatif de           ;;; 
;;; Neurobiologie Alfred Fessard, CNRS.                                              ;;; 
;;;                                                                                  ;;; 
;;; Permission to use, copy, modify, and distribute this software and its            ;;; 
;;; documentation for any purpose and without fee is hereby granted, provided that   ;;; 
;;; this software is cited in derived published work, and the copyright notice       ;;; 
;;; appears in all copies and in supporting documentation. The Surf-Hippo Project    ;;; 
;;; makes no representations about the suitability of this software for any          ;;; 
;;; purpose. It is provided "as is" without express or implied warranty.             ;;; 
;;;                                                                                  ;;; 
;;; If you are using this code or any part of Surf-Hippo, please contact             ;;; 
;;; surf-hippo@ai.mit.edu to be put on the mailing list.                             ;;; 
;;;                                                                                  ;;; 
;;; Copyright (c) 1989 - 2002, Lyle J. Graham                                        ;;;                                                       
;;;                                                                                  ;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;;
;;; main-compiler.lisp
;;;
;; This loads the Garnet and Surf-Hippo systems, according to the Unix
;; environment variables GARNETHOME and SURFHOME.
;; LBG 9/30/92


;; This is loaded into a fresh CMUCL Ilisp environment, or one with GARNET added.
;; LBG 10/0/94

(defvar surf-hippo-compile t)

(defvar *SURF-HOME*)
(setq *SURF-HOME*
      (let ((surfhome (cdr (assoc :SURFHOME lisp::*environment-list*)))
	    (home (cdr (assoc :HOME lisp::*environment-list*))))
	(if surfhome
	    (concatenate 'string (string-right-trim '(#\/) surfhome) "/")
	    "/usr/local/surf-hippo/"
	    ; (concatenate 'string (string-right-trim '(#\/) home) "/surf-hippo/")
	    )))

(load (concatenate 'string *surf-home* "misc/loaders/main-loader") :if-source-newer :compile)






