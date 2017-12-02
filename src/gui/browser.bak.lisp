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


;; GUI Source file: browser.lisp


(IN-PACKAGE "WINDOWS-HACK")

;;; Stolen from Garnet demo-browser.lisp



;;   Schemata defined in the DO-GO procedure which are referenced by other
;; functions
;;

(defun PATHNAME-TO-STRING-FN (pathname)
  (if pathname
      (let ((file (file-namestring pathname)))
	(if (opal:directory-p (namestring pathname))
	  ; (string= file "")   ; then pathname is a directory.
	                        ; So strip off the "/", get the directory name,
	                        ; and restore the "/".
	    (let ((directory (string-right-trim "/" (namestring pathname))))
	      (concatenate 'string (file-namestring directory) "/"))
	    file))  ; else we already got the file name.
      ""))

(defvar *browser-extensions* '(""))

;; NAMESTRING may be either a path or a pathname
(defun DIRECTORY-FN (namestring &optional (*browser-extensions* *browser-extensions*))
  (let* ((namestring (if (opal:directory-p (namestring namestring))
		       (format nil "~a/" (namestring namestring)) namestring))
	 (dir (loop for path in (directory namestring :follow-links nil :sort t :all nil)
		    when (or (opal::directory-p (namestring path))
			     (test-for-*browser-extensions* path))
		    collect path)))
;    (format t "Entering DIRECTORY-FN with ~A....~%" namestring)
    (unless (or (null dir) (equal (car dir) namestring))
;      (format t "DIRECTORY-FN returns ~A....~%" dir)
      dir)))

(defun test-for-*browser-extensions* (path)
  (when path
    (or (string= "" (file-namestring path))
	(loop for extension in *browser-extensions*
	      when (string= extension (pathname-type (file-namestring path)))
	      do (return t)
	      finally (return (or (= (length *browser-extensions*) 0)
				  (loop for string in *browser-extensions* when (> (length string) 1)
					do (return nil) finally (return t))))))))

(defun quit-with-file (sm-item)
  (let ((file (file-namestring sm-item)))
    (cond ((string= file "")		; then pathname is a directory.
	   nil)
	  ((test-for-*browser-extensions* sm-item)
	   (setq *browser-file* sm-item)
	   (inter:interaction-complete)
	   sm-item))))


(defvar *BROWSER-FILE* nil)

(create-instance 'file-browser-win inter:interactor-window (:visible nil))

(create-instance 'FILE-BROWSER garnet-gadgets:browser-gadget (:menu-items-generating-function #'DIRECTORY-FN))

(create-instance 'FILE-BROWSER-TOP-AGG opal:aggregate)


(defun file-browser-menu-function (browser sm-item)
  (when (and (g-value sm-item :highlighted)
	     (OPAL:DIRECTORY-P (namestring (g-value sm-item :item))))
    (let ((feed (g-value sm-item :parent :parent :feedback-obj)))
;      (format t "processing ~a~%" feed)
      (s-value feed :obj-over sm-item)
      (s-value sm-item :highlighted NIL)
      (s-value (g-value browser :parent :parent :STATUS) :visible T)
      (opal:update FILE-BROWSER-WIN)
      (s-value sm-item :highlighted T)
      (s-value feed :obj-over NIL)
      (garnet-gadgets:push-first-item FILE-BROWSER (g-value sm-item :item))
      (s-value (g-value browser :parent :parent :STATUS) :visible NIL))))

(defun file-browser-additional-selection-function (browser sm-item)
  (declare (ignore browser))
  (when sm-item (quit-with-file sm-item)))

(defun file-browser-previous-selection-function (gadget value)
  (declare (ignore gadget value))
  (let* ((items (g-value FILE-BROWSER :items)))
    (when items
      (let* ((new-top-level-namestring (directory-namestring (string-right-trim "/" (namestring (car items))))))
	(unless (string= "" new-top-level-namestring)
	  ;; Add the new item to the browser
	  (s-value (g-value file-browser :STATUS) :visible T)
	  (opal:update FILE-BROWSER-WIN)
;	  (print FILE-BROWSER-WIN)
;	  (break)
	  (garnet-gadgets:push-first-item FILE-BROWSER (pathname new-top-level-namestring))
	  (s-value (g-value file-browser :STATUS) :visible nil))))))

(defun file-browser (title start-directory &optional extensions)
  (unless *automatic-run*
    (let* ((*BROWSER-FILE* nil)
	   (*browser-extensions* extensions)
	   (file-browser-win
	    (create-instance nil file-browser-win (:left 400) (:top 10) (:width 300) (:height 270) (:title title) (:icon-title title) (:visible t)))
	   (FILE-BROWSER-TOP-AGG (create-instance nil FILE-BROWSER-TOP-AGG))
	   (FILE-BROWSER (create-instance nil FILE-BROWSER
					  (:constant T :except :num-menus)
					  (:menu-items-generating-function #'DIRECTORY-FN)
					  (:left 10) (:top 85)
					  (:num-menus 1)
					  (:additional-selection-p t) (:additional-selection-function #'file-browser-additional-selection-function)
					  (:item-to-string-function #'PATHNAME-TO-STRING-FN)
					  (:menu-function #'file-browser-menu-function))))
      (s-value FILE-BROWSER-WIN :aggregate FILE-BROWSER-TOP-AGG)
      (add-comment FILE-BROWSER-WIN "Left for Directory, Middle for File")
      (garnet-gadgets:push-first-item FILE-BROWSER start-directory)
      (opal:add-component FILE-BROWSER-TOP-AGG FILE-BROWSER)
      (opal:update FILE-BROWSER-WIN)
      (create-instance 'CONTROL-PANEL opal:aggregadget
		       (:constant :left :top)    (:left 30) (:top 10)
		       (:parts
			`((:QUIT-BUTTON ,garnet-gadgets:text-button-panel
					(:left ,(o-formula (+ 150 (gvl :parent :left))))
					(:top ,(o-formula (gvl :parent :top)))
					(:text-offset 3) (:shadow-offset 5) (:gray-width 3) (:final-feedback-p NIL)
					(:items ("Cancel"))
					(:selection-function ,#'(lambda (gadget value)
								  (declare (ignore gadget value))
								  (inter:interaction-complete))))
			  (:prev ,garnet-gadgets:text-button-panel
				 (:left ,(o-formula (gvl :parent :left)))
				 (:top ,(o-formula (gvl :parent :top)))
				 (:shadow-offset 5) (:gray-width 3) (:text-offset 3) (:final-feedback-p NIL)
				 (:items ("Previous Directory"))
				 (:selection-function ,#'file-browser-previous-selection-function)))))
      (s-value FILE-BROWSER :status
	       (create-instance nil opal:text
				(:constant T :except :visible)
				(:left 30)
				(:top (o-formula (+ 10 (opal:gv-bottom CONTROL-PANEL))))
				(:string "Fetching directory information...")
				(:font (create-instance NIL opal:font (:face :italic)))
				(:visible NIL)))
      (opal:add-components FILE-BROWSER-TOP-AGG CONTROL-PANEL)
      (opal:update FILE-BROWSER-WIN)
      (inter:wait-interaction-complete FILE-BROWSER-WIN)
      (opal:destroy FILE-BROWSER-WIN)
      *BROWSER-FILE*)))

(export '(PATHNAME-TO-STRING-FN
	  *browser-extensions*
	  DIRECTORY-FN
	  *BROWSER-FILE*
	  file-browser))






