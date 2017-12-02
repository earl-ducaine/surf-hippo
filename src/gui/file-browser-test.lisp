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

;; GUI Source file: file-browser.lisp


(IN-PACKAGE "WINDOWS-HACK")

;;; Stolen from Garnet demo-browser.lisp

;; Schemata defined in the DO-GO procedure which are referenced by other functions


(defun PATHNAME-TO-STRING-FN (pathname)
  (if pathname
    (namestring pathname)
;      (let ((file (file-namestring pathname)))
;	(if (opal::lite-directory-p (namestring pathname))
;	  ; (string= file "")   ; then pathname is a directory.
;	                        ; So strip off the "/", get the directory name,
;	                        ; and restore the "/".
;	    (let ((directory (string-right-trim "/" (namestring pathname))))
;	      (concatenate 'string (file-namestring directory) "/"))
;	    file))  ; else we already got the file name.
      ""))

(defun test-for-file-extensions (path &optional allowed-extensions no-extension-ok)
  ;; If PATH is NIL, return NIL, else if EXTENSIONS is either NIL or PATH is a directory, return T. Otherwise, when EXTENSIONS is
  ;; a list of strings, if any matches the extension of PATH, then return T, otherwise return nil.
  (declare (optimize (safety 1) (speed 3) (space 1)))
  (when path
    (or (and (t-or-nil-p allowed-extensions) allowed-extensions)
	(= (length (the cons allowed-extensions)) 0)
	(let ((file-namestring (file-namestring path)))
	  (or (not file-namestring)	; It's a directory
	      (let ((pathname-type (pathname-type file-namestring))
		    good-extension-found)
		(if (not pathname-type)
		    no-extension-ok
		    (if (consp allowed-extensions)
			(loop for extension in allowed-extensions
			      when (string= (the simple-base-string extension) (the simple-base-string pathname-type))
			      do (return t)
			      when (> (length (the simple-base-string extension)) 1) do (setq good-extension-found t)
			      finally (return (not good-extension-found)))
			allowed-extensions))))))))

(defun DIRECTORY-FN (namestring &optional allowed-extensions no-extension-ok only-directories)
  ;; NAMESTRING may be either a path or a pathname. If a directory, then return a list of all subdirectories and files that pass
  (declare (optimize (safety 0) (speed 3) (space 1)))
  (let* ((temp-namestring (namestring namestring))
	 (namestring (if (and (opal:directory-p temp-namestring) (not (string= "/" (the simple-base-string (string-tail temp-namestring 1)))))
		       (format nil "~a/" temp-namestring) ; Add trailing "/" to directories.
		       temp-namestring))
	 (dir-list (loop for path in (directory namestring :follow-links nil :sort t :all nil :check-for-subdirs t)
			 when (if only-directories
				  (opal:directory-p (pathname-to-string-fn path))
				  (test-for-file-extensions path allowed-extensions no-extension-ok))
			 collect path)))
    (unless (or (null dir-list) (equal (car dir-list) namestring))
      dir-list)))

(defvar *browser-allowed-extensions* '(""))

(defun browser-directory-fn (namestring) (DIRECTORY-FN namestring *browser-allowed-extensions*))
(defun browser-only-directory-fn (namestring) (DIRECTORY-FN namestring *browser-allowed-extensions* nil t))

(defun quit-with-file (sm-item &optional allowed-extensions)
  ;; Only quit if SM-ITEM points to a file, with the ALLOWED-EXTENSIONS according to TEST-FOR-FILE-EXTENSIONS.
  (let ((file (file-namestring sm-item)))
    (cond ((or (not file) (string= file "")) ; then pathname is a directory.
	   nil)
	  ((test-for-file-extensions sm-item allowed-extensions)
	   (setq *browser-file* sm-item)
	   (inter:interaction-complete)
	   sm-item))))

(defun quit-with-directory (sm-item)
  ;; Only quit if SM-ITEM points to a directory.
  (when (opal:directory-p (PATHNAME-TO-STRING-FN sm-item))
    (setq *browser-file* sm-item)
    (inter:interaction-complete)
    sm-item))

(defvar *BROWSER-FILE* nil)

(defun file-browser-menu-function (browser sm-item)
  (when (and (gv sm-item :highlighted) (OPAL::lite-DIRECTORY-P (namestring (gv sm-item :item))))
    (let ((feed (gv sm-item :parent :parent :feedback-obj)))
      (s-value feed :obj-over sm-item)
      (s-value sm-item :highlighted NIL)
      (s-value (gv browser :parent :parent :STATUS) :visible T)
      (opal:update (gv browser :window))
      (s-value sm-item :highlighted T)
      (s-value feed :obj-over NIL)
      (garnet-gadgets:push-first-item (gv browser :window :file-browser) (gv sm-item :item))
      (s-value (gv browser :parent :parent :STATUS) :visible NIL))))

(defun file-browser-additional-selection-function (browser sm-item)
  (declare (ignore browser))
  (when sm-item (quit-with-file sm-item *browser-allowed-extensions*)))

(defun directory-browser-additional-selection-function (browser sm-item)
  (declare (ignore browser))
  (when sm-item (quit-with-directory sm-item)))

(defun get-new-top-level-namestring-from-items (items) (directory-namestring (string-right-trim "/" (namestring (car items)))))

(defun file-browser-previous-selection-function (gadget value)
  (declare (ignore value))
  (let* ((file-browser-win (gv gadget :window))
	 (file-browser (gv file-browser-win :file-browser))
	 (items (gv FILE-BROWSER :items)))
    (when items
      (let ((new-top-level-namestring (get-new-top-level-namestring-from-items items)))
	(unless (string= "" new-top-level-namestring)
	  ;; Add the new item to the browser
	  (s-value (gv file-browser :STATUS) :visible T)
	  (opal:update FILE-BROWSER-WIN)
	  (garnet-gadgets:push-first-item FILE-BROWSER (pathname new-top-level-namestring))
	  (s-value (gv file-browser :STATUS) :visible nil))))))

(defun selection-function-interaction-complete (gadget value)
  (declare (ignore gadget value))
  (inter:interaction-complete))

(defun get-browser-CONTROL-PANEL ()
  (create-instance nil opal:aggregadget
		   (:constant :left :top)
		   (:left 30)
		   (:top 10)
		   (:parts
		    `((:QUIT-BUTTON ,garnet-gadgets:text-button-panel
		       (:left ,(o-formula (+ 150 (the fn (gvl :parent :left)))))
		       (:top ,(o-formula (gvl :parent :top)))
		       (:text-offset 3) (:shadow-offset 5) (:gray-width 3) (:final-feedback-p NIL)
		       (:items ("Cancel"))
		       (:selection-function ,#'selection-function-interaction-complete))
		      (:prev ,garnet-gadgets:text-button-panel
		       (:visible ,(o-formula (not (string= "" (get-new-top-level-namestring-from-items (gvl :parent :window :file-browser :items))))) )
		       (:left ,(o-formula (gvl :parent :left)))
		       (:top ,(o-formula (gvl :parent :top)))
		       (:shadow-offset 5) (:gray-width 3) (:text-offset 3) (:final-feedback-p NIL)
		       (:items ("Previous Directory"))
		       (:selection-function ,#'file-browser-previous-selection-function))))))

(defun get-FILE-BROWSER (&optional directory-search)
  (create-instance nil gg:browser-gadget 
		   ; (:constant T :except :num-menus)
		   (:menu-items-generating-function (o-formula (if (gvl :directory-search)
								   #'browser-only-DIRECTORY-FN
								   #'browser-DIRECTORY-FN)))
		   (:left 10) (:top 50)
		   ;; Need at least 4 rows to accomodate the sliders, I guess.
		   (:num-rows (o-formula (max 4 (min (length (car (gvl :all-items))) 10))))
		   (:num-menus 1)
		   (:additional-selection-p t)
		   (:directory-search directory-search)
		   (:additional-selection-function (o-formula (if (gvl :directory-search)
								  #'directory-browser-additional-selection-function
								  #'file-browser-additional-selection-function)))
		   (:item-to-string-function #'PATHNAME-TO-STRING-FN)
		   (:menu-function #'file-browser-menu-function)))

(defun get-file-browser-status (control-panel)
  (create-instance nil opal:text
		   (:constant T :except :visible)
		   (:left 30)
		   (:top (o-formula (+ 10 (the fn (opal:gv-bottom CONTROL-PANEL)))))
		   (:string "Fetching directory information...")
		   (:font (create-instance NIL opal:font (:face :italic)))
		   (:visible NIL)))

(defvar *file-browser-left* 500)
(defvar *file-browser-top* 10)

(defvar *min-file-browser-menu-width* 250) ; this seems to cover the PREVIOUS and CANCEL buttons.

(defun directory-browser (&optional (title "Browse and Select Directory") start-directory) (file-browser title start-directory nil t))
  
(defun file-browser (&optional (title "Browse and Select File") start-directory (extensions t) directory-search)
  ;; Returns a pathname, or NIL if CANCEL.
  (unless *automatic-run*
    (let* ((*BROWSER-FILE* nil)
	   (start-directory (if (and start-directory (probe-file start-directory))
				(lisp::unix-namestring (probe-file start-directory))
				(multiple-value-bind (flag dir)
				    (unix::unix-current-directory)
				  dir)))
	   (*browser-allowed-extensions* extensions)
	   (file-browser-win (create-instance nil basic-graphics-window
					      (:visible nil)
					      (:left (o-formula (or *menu-left* (round (/ (- *screen-width* (gvl :width)) 2)))))
					      (:top (o-formula (or *menu-top* (round (/ (- *screen-height* (gvl :height)) 2)))))
					      (:background-color (when wh::*motif-gui-p* OPAL:MOTIF-GRAY))
					      ;; (:left *file-browser-left*) (:top *file-browser-top*)
					      (:width (o-formula (if (gvl :file-browser)
								     (max *min-file-browser-menu-width* (+ 40 (gvl :file-browser :width)))
								     100)))
					      (:min-width (o-formula (gvl :width))) ; (:max-width (o-formula (gvl :width)))

					      (:min-height (o-formula (gvl :height))) ; (:max-height (o-formula (gvl :height)))
					      (:title title) (:icon-title title)
					      (:visible t)
					      (:file-browser (get-FILE-BROWSER directory-search))))
	   (control-panel (get-browser-CONTROL-PANEL))
	   (FILE-BROWSER-TOP-AGG (create-instance nil opal:aggregate)))
      (S-VALUE file-browser-win :instructions-comment (add-comment FILE-BROWSER-WIN
								   (if extensions
								       (format nil "LEFT to go to directory~%MIDDLE to return file~%~A"
									       (if (consp extensions) (format nil "File extensions:~{ ~a~}" extensions) "All files allowed"))
								       (format nil "LEFT to go to directory~%MIDDLE to return ~A" (if directory-search "directory" "file")))
								   :position :LOWER-left))
      (s-value  file-browser-win :height (o-formula (if (gvl :file-browser)
							(+ (gvl :instructions-comment :height)
					; 110 ; Account for ADD-COMMENT below.
							   (gvl :file-browser :height))
							100)))
      (add-comment FILE-BROWSER-WIN
								   (if extensions
								       (format nil "LEFT to go to directory~%MIDDLE to return file~%~A"
									       (if (consp extensions) (format nil "File extensions:~{ ~a~}" extensions) "All files allowed"))
								       (format nil "LEFT to go to directory~%MIDDLE to return ~A" (if directory-search "directory" "file")))
								   :position :LOWER-left)
      (s-value FILE-BROWSER-WIN :aggregate FILE-BROWSER-TOP-AGG)
      (garnet-gadgets:push-first-item (gv FILE-BROWSER-win :file-browser) start-directory)
      (opal:add-component FILE-BROWSER-TOP-AGG (gv FILE-BROWSER-win :file-browser))
      (opal:update FILE-BROWSER-WIN)
      (opal:add-components FILE-BROWSER-TOP-AGG control-panel)
      (s-value (gv FILE-BROWSER-win :file-browser) :status (get-file-browser-status control-panel))
      (s-value FILE-BROWSER-WIN :visible t)
      (opal:update FILE-BROWSER-WIN)
      (inter:wait-interaction-complete FILE-BROWSER-WIN)
      (setq *menu-left* (gv FILE-BROWSER-WIN :left)
	    *menu-top* (gv FILE-BROWSER-WIN :top))
      (opal:destroy FILE-BROWSER-WIN)
      *BROWSER-FILE*)))


(export '(PATHNAME-TO-STRING-FN
;	  *browser-allowed-extensions*
;	  browser-DIRECTORY-FN
	  *BROWSER-FILE*
	  directory-browser
	  file-browser))






