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


;;; This file setups Surf-Hippo pathnames, etc. and loads some CMUCL fixes.

;; For possible user/site variables to set search for "$$$$ CUSTOMIZE $$$$"

(defvar *force-all-compile* nil)

(push (read-from-string (format nil ":surf-hippo-V~a" Surf-Hippo-Version-Number)) *features*)
(push :SURF-HIPPO *features*)

#|
(rplaca
 (cdr (member :bugs *herald-items*))
 '("Send bug reports and questions to your local CMU CL maintainer, " terpri
   "or to " 
   "pvaneynd@debian.org" terpri
   "or to " 
   "cmucl-help@cons.org. (prefered)" terpri terpri
   "Type (help) for help, (quit) to exit, and (demo) to see the demos." terpri
   terpri
   "Loaded subsystems:" terpri))
|#

(setf (getf ext:*herald-items* :bugs)
 '("Send bug reports and questions to surfhippo@gmail.com."))

(setf (getf ext:*herald-items* :garnet)
      `("    Garnet Version " ,Garnet-Version-Number))
      
(setf (getf ext:*herald-items* :surf-hippo)
      `("    Surf-Hippo Version " ,Surf-Hippo-Version-Number ", " ,Surf-Hippo-Version-Date TERPRI
	"    Surf-Hippo Home: surf-hipppo.neurophysics.eu" terpri
	terpri ; terpri
	"To start the Surf-Hippo menus enter (SURF) at the Lisp prompt." terpri
	"To start the Surf-Hippo demo circuit menu enter (DEMO) at the Lisp prompt." terpri
	"For help, read the User Manual in the surf-hippo/doc directory." terpri
	"To quit Lisp, enter (QUIT) at the Lisp prompt."
	terpri terpri
	))

(if (> (length Surf-Hippo-Version-Comment) 0)
    (setf
     (getf ext:*herald-items* :surf-hippo-comment)
     `("               " ,Surf-Hippo-Version-Comment)))


(defvar load-cmucl-fixes-p t)		; LG 20.08.2016 - cmucl-fixes unecessary for CMUCL-20D?
(defvar compile-cmucl-fixes-p t)
(defvar Surf-Hippo-cmucl-fixes-PathName)
(defvar Surf-Hippo-cmucl-fixes-Src)
(setq Surf-Hippo-cmucl-fixes-PathName (fixup-pathname (concatenate 'string Surf-Hippo-Pathname "/bin/cmucl-fixes/"))
      Surf-Hippo-cmucl-fixes-Src (fixup-pathname (concatenate 'string Surf-Hippo-Pathname "/src/cmucl-fixes/")))

#+cmu
(progn
  (setf (ext:search-list "cmucl-fixes:")
	(list Surf-Hippo-cmucl-fixes-PathName))
  (setf (ext:search-list "cmucl-fixes-src:")
	(list Surf-Hippo-cmucl-fixes-Src)))

(defparameter Surf-Hippo-cmucl-fixes-Loader
  (merge-pathnames "cmucl-fixes-loader"
		   #+cmu "cmucl-fixes-src:" ;"cmucl-fixes:"
		   #+(not cmu) Surf-Hippo-cmucl-fixes-PathName))

;;; *dont-load-modules-twice* tells whether to re-load modules
;;; if a user loads Surf-Hippo-loader.lisp a second time.
(defparameter *dont-load-modules-twice* nil)

;;(break)
;; Load some cmucl fixes before loading Garnet.
(if load-cmucl-fixes-p
    (if (and *dont-load-modules-twice* (get :Surf-Hippo-modules :cmucl-fixes))
	(format T "~%****** CMUCL-FIXES already loaded *******~%")
	(progn
	  (format T "~% %%%%%%%% Loading CMUCL-FIXES %%%%%%%%~%")
	  (load Surf-Hippo-CMUCL-FIXES-Loader)))
    (format T "~%****** NOT Loading CMUCL-FIXES *******~%"))








