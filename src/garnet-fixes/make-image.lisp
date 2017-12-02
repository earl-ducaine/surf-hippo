;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: OPAL; Base: 10 -*-

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

(in-package "OPAL")

;; Chmod of image file to 755.


(defun make-image (filename &rest args)
  #-(or cmu allegro lucid lispworks clisp apple)
    (error "Don't know how to automatically save an image for this lisp.
Please consult your lisp's user manual for instructions.~%")

  #+clisp (declare (compile))
  
  (multiple-value-bind (quit gc verbose libfile flush-source-info? extra-args)
      (Extract-Image-Args args)
  #+apple (declare (ignore quit))
  #-presto (declare (ignore libfile))
  #-allegro (declare (ignore flush-source-info?))

  ;; When the image is restarted, we want *readtable* to be restored to its
  ;; current value, instead of being reinitialized to the default.  This will
  ;; keep the #k<> and #f() reader macros active in the saved image.
  #+allegro
  (progn  
    (if verbose (format t "~%Copying readtable..."))
    (copy-readtable *readtable* user::Garnet-Readtable)
    (setf (cdr (assoc '*readtable* excl:*cl-default-special-bindings*))
          'user::Garnet-Readtable)
    (if verbose (format t "copied.~%")))

  (progn
    (if verbose (format t "Disconnecting Garnet..."))
    (opal:disconnect-garnet)
    (if verbose (format t "disconnected.~%")))

  (setf garnet-image-date (time-to-string))

  ;; RGA --- kills extra source file info, which is not useful when
  ;; making a portable image.
  #+allegro
  (when flush-source-info?
    (format t "Flushing source file info . . .")
    (excl:discard-all-source-file-info)
    (format t "Fssssh-gurgle-hisss!~%"))

  ;; LispWorks and CLISP GC are done below, during the save
  #+(or allegro lucid cmu apple)
  (when gc
    (if verbose (format t "Garbage collecting..."))
    #+allegro (excl:gc T)
    #+cmu     (ext:gc)			; (ext:gc T) CMU18a+ has :key args only. LBG 8/22/98
    ; There is no equivalent of "total" garbage collection in Lucid
    #+lucid   (lcl:gc)
    #+apple   (ccl:gc)
    (if verbose (format t "collected.~%")))
  
  ;; RGA added code for prestoized lisp images.
  #+presto
  (when libfile
    (if verbose (format t "Saving stub functions . . ."))
    (sys:presto-build-lib libfile))

  (if verbose (format t "Saving image..."))
  #+allegro
  (apply #'excl:dumplisp :name filename
	                 :restart-function #'garnet-restart-function
			 :checkpoint NIL
			 :read-init-file T
			 extra-args)
  #+lucid
  (apply #'lcl:disksave filename
	                :restart-function #'garnet-restart-function
			extra-args)
  #+cmu
  (apply #'ext:save-lisp filename
	                 :init-function #'(lambda ()
					    (garnet-restart-function)
					    #-cmu17
					    (return-from make-image T)
					    #+cmu17
					    (cl::%top-level))
			 extra-args)
  #+lispworks
  (apply #'system:save-image filename
	                     :gc gc
			     :restart-function #'garnet-restart-function
			     extra-args)
  #+CLISP
  (let* ((old-driver system::*driver*)
	 (system::*driver* #'(lambda ()
			       (setq system::*driver* old-driver)
			       (garnet-restart-function)
			       (funcall system::*driver*))))
    (apply #'system::saveinitmem extra-args)
    (rename-file "lispinit.mem" filename))

  #+apple
  (progn
    (pushnew #'garnet-restart-function ccl:*lisp-startup-functions*)
    (apply #'ccl:save-application filename extra-args))

  ;; 22/10/98 LBG chmod on image
  (ext::run-program "chmod" (list 755 filename))
  
  (if verbose (format t "saved.~%"))

  ;; Mac Lisp always quits
  #-apple
  (cond
    (quit
     (if verbose (format t "Quitting lisp...~%"))
     #+allegro (excl:exit)
     #+lucid (lcl:quit)
     #+cmu (ext:quit)
     #+lispworks (system:bye)
     #+clisp (system::exit)
     )
    (t
     (if verbose (format t "Reconnecting Garnet..."))
     (opal:reconnect-garnet)
     (if verbose (format t "reconnected.~%"))
     ))
  ))
