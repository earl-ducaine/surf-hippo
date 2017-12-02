;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: WINDOWS-HACK; Base: 10 -*-

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

;; GUI Source file: annotation-file.lisp


(IN-PACKAGE "WINDOWS-HACK")


(defvar *annotation-filename* nil)


(defun dated-filename (directory-name extension &optional (time-stamp *actual-time-stamp*))
  (let ((directory-name (if (stringp directory-name)
			    directory-name
			    (namestring directory-name))))
    (replace-repeated-character-w-single
     (multiple-value-bind (second minute hour date month year day daylight zone)
	 (decode-universal-time (+ (truncate (* 10 time-stamp)) *universal-time-conversion-factor*))
       (concatenate 'string "/" directory-name "/" (format nil "~D_~D_~D.~A" month date year extension)))
     "/")))


#|
(defun update-annotation-file (info-function path extension &optional info-function-args)
  (let ((filename (DATED-FILEname (create-path path) extension)))
    (dribble filename)
    (apply info-function info-function-args)
    (dribble)))
|#

(defun update-annotation-file (info-function path extension &optional (info-function-args '()))
  (let ((filename (DATED-FILEname (create-path path) extension)))
    (with-open-stream (stream (open filename :if-exists :append :if-does-not-exist :create :direction :output))
      (let ((*standard-output* stream))
	(format t "~%")
	(if info-function-args
	    (apply info-function info-function-args)
	    (funcall info-function))))
    (format t ";; ~A updated..~%" filename)))

(export '(update-annotation-file dated-filenam))



  
