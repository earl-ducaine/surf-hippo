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

;; A small change to garnet-restart-function.

(in-package "OPAL")

(defun garnet-restart-function ()
  (when nil ; (string= *username* "lyle")
    (format t "*** Restarting Garnet ~A image created with opal:make-image ***~%"
	    user::Garnet-Version-Number)
    (if (boundp 'garnet-image-date)
      (format t "*** Image creation date: ~A ***~%" garnet-image-date)))
  (opal:reconnect-garnet)
  )


;; LBG Added 7/2/00 since the original appears to choke on DOS names.

;; This is an industrial-strength version of opal:directory-p.  The difference
;; is that extra work is done to ensure that single and double quotes are
;; passed to the shell correctly.  Since it does more work, only use this
;; version if you find you really need it.  This code was contributed by
;; Bruno Haible.

(defun directory-p (pathname)
  ;; Must quote the pathname since Unix shells interpret characters like
  ;; #\Space, #\', #\<, #\>, #\$ etc. in a special way. This kind of quoting
  ;; should work unless the pathname contains #\Newline and we call csh.
  (when pathname
    (flet ((shell-quote (string) ; surround a string by single quotes
	   (let ((qchar nil) ; last quote character: nil or #\' or #\"
		 (qstring (make-array 10 :element-type 'character
				      :adjustable t :fill-pointer 0)))
	     (map nil #'(lambda (c)
			  (let ((q (if (eql c #\') #\" #\')))
			    (unless (eql qchar q)
			      (when qchar (vector-push-extend qchar qstring))
			      (vector-push-extend (setq qchar q) qstring))
			    (vector-push-extend c qstring)))
		  string)
	     (when qchar (vector-push-extend qchar qstring))
	     qstring)))
    ;; command-string is the string that's going to be executed.
    (let ((command-string (concatenate 'string "test -d " (shell-quote pathname) " && echo 1")))
      (unless (equal "" (shell-exec command-string)) T)))))

;; This was the original opal:directory-p.
(defun lite-directory-p (pathname)
  #+clisp
  ;; 1. Needn't call a shell if we can do the test ourselves.
  ;; 2. In case pathname contains Latin-1 characters. clisp is 8 bit clean,
  ;;    while most Unix shells aren't.
  (gu:probe-directory pathname)

  #+apple
  (ccl:directory-pathname-p pathname)

  #-(or clisp apple)
  ;; command-string is the string that's going to be executed.
  (let ((command-string
	 (concatenate 'string "test -d " pathname " && echo 1")))
    (unless (equal "" (shell-exec command-string))
	    T)))
