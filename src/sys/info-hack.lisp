;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: SURF; Base: 10 -*-

#|

====================================================================================================
			       The Surf-Hippo Neuron Simulator System
====================================================================================================

This code was written as part of the Surf-Hippo Project, originally at the Center for Biological
Information Processing, Department of Brain and Cognitive Sciences, Massachusetts Institute of
Technology, and currently at the Neurophysiology of Visual Compuation Laboratory, CNRS.
                                                                                 
Permission to use, copy, modify, and distribute this software and its documentation for any purpose
and without fee is hereby granted, provided that this software is cited in derived published work,
and the copyright notice appears in all copies and in supporting documentation. The Surf-Hippo
Project makes no representations about the suitability of this software for any purpose. It is
provided "as is" without express or implied warranty.
                                                                                 
If you are using this code or any part of Surf-Hippo, please contact surf-hippo@ai.mit.edu to be put
on the mailing list.
                                                                                 
Copyright (c) 1989 - 2003, Lyle J. Graham                                                                                              

|#


;;; SYS Source file: info-hack.lisp
(IN-PACKAGE "SURF")

;; These will let us expose info-windows wherever the last info-window was placed.
(defvar *info-window-top* nil)
(defvar *info-window-left* nil)

(defun info-window-wrap-up (win)
  (setq *info-window-top* (max 10 (fn-gv win :top))
	*info-window-left* (max 10 (fn-gv win :left))))
  
(defun create-info-window (&key (width 750) (height 300) type)
  (setq *create-new-info-window* nil)
  (let ((info-win (create-scrolling-display-window
		   :mode :info :width width :height height :type type
		   :display-object (create-instance nil opal:multifont-text (:text-width (o-formula (gvl :scrolling-window :clip-window :width))))
		   :title (GET-win-TITLE-STRING (concatenate-strings *simulation-name* ": Information")))))
    (s-value info-win :left (o-formula (or *info-window-left* (round (/ (- *screen-width* (gvl :width)) 2)))))
    (s-value info-win :top (o-formula (or *info-window-top* (round (/ (- *screen-height* (gvl :height)) 2)))))
    (s-value info-win :wrap-up-function #'info-window-wrap-up)
    info-win))

(defun get-info-window (info-pane-type &optional name &key (width 925) (height 300) create-new-win session-name)
  (let ((win (if (not create-new-win) (find-info-window info-pane-type name))))
    (unless win (setq win (create-info-window :width width :height height :type info-pane-type)))
    (s-value win :title (GET-win-TITLE-STRING (string (or name info-pane-type))))
    (s-value win :locked *lock-all-windows*)
    (create-instance nil toggle-window-lock-Interactor (:window win))
    (if width (s-value win :width (COERCE-TO-EVEN-INT width)))
    (if height (s-value win :height (COERCE-TO-EVEN-INT height)))
    (s-value win :session-name (if session-name session-name (g-value win :title)))
    win))

(defun find-info-window (info-win-type &optional name allow-help-windows)
  (loop for window in (clean-up-*output-windows*) ; Is there already the right kind of window?
	when (and (not (g-value window :locked))
		  (or allow-help-windows (not (gv window :help-window)))
		  (eq :info (g-value window :mode))
		  (string-equal info-win-type (g-value window :type))
		  (if (and *create-new-info-window* name)
		      (string-equal name (g-value window :title)) t))
	do (return window)))

(defun add-title-to-info-window (win) (s-value win :title (GET-win-TITLE-STRING (concatenate-strings *simulation-name* ": Information"))))

(defun print-circuit-to-info-window (&optional description)
  (let (*use-gc-announce-window* *gc-announce-text*)
    (OUTPUT-TEXT-TO-INFO-WIN (list 'print-circuit description)))
  (let (*DUMP-ANALYSIS-TO-FILE*)
    (OUTPUT-TEXT-TO-INFO-WIN (list 'print-simulation-stats))))

(defun output-text-to-info-win (string-or-printing-function)
  (let (*use-gc-announce-window* *gc-announce-text*)
    (when (or (not (opal-obj-exists *standard-info-output*)) *create-new-info-window*)
      (setq *standard-info-output* (create-info-window)))
    (s-value *standard-info-output* :session-name *simulation-name*)
    (add-title-to-info-window *standard-info-output*)
    (opal:insert-string (g-value *standard-info-output* :display-object)
			(with-output-to-string (*standard-output*)
			  (typecase string-or-printing-function
			    (string (format t string-or-printing-function))
			    (cons (apply (car string-or-printing-function) (cdr string-or-printing-function)))
			    (t (funcall string-or-printing-function)))))
    (opal:insert-string (g-value *standard-info-output* :display-object) (string #\newline))
    (s-value *standard-info-output* :visible t)
    (opal:update *standard-info-output*)))

(defun string-to-info-win (string &optional refresh-info-window (linespacer 1) (update t))
  (let (*use-gc-announce-window* *gc-announce-text*)
    (when (or (not (opal-obj-exists *standard-info-output*))
	      *create-new-info-window*)
      (setq *standard-info-output* (create-info-window)))
    (let ((text-gadget (g-value *standard-info-output* :display-object)))
      (cond (refresh-info-window
	     #+garnet-v2.1 (opal:set-strings text-gadget string)
	     #+garnet-v2.2 (opal:set-text text-gadget string)
	     (opal:go-to-end-of-text text-gadget))
	    (t (opal:insert-text text-gadget string)
	       (opal:go-to-end-of-text text-gadget)))
      (dotimes (i linespacer) (opal:go-to-next-line text-gadget))
      (s-value *standard-info-output* :visible t)
      (when update (opal:update *standard-info-output*)))))

(defun clear-info-windows () (clear-windows-of-mode :info))

(defun help-window-inter-function (interactor final-obj-over)
  (declare (ignore final-obj-over))
  (let (*standard-info-output*
	(window (if interactor (g-value interactor :window))) *print-pretty* mode *automatic-run*)
    (when (consp window) (loop for win in window when (g-value win :mode) do (setq window win)))
    (setq mode (if window (g-value window :mode) :top-level))
    (when (and (not (find-INFO-WINDOW mode nil t))
	       (member mode '(:standard-plot :histology :histogram; :top-level
			      )))
      (let ((info-win (get-INFO-WINDOW mode nil :width 350 :height (round (/ *screen-height* 3))))
	    string)
	(s-value info-win :help-window t)
	(s-value info-win :title (GET-win-TITLE-STRING (format nil "~a Help Window" mode)))
	(with-open-file (stream
			 (case mode
			   ;; (:top-level (concatenate-strings *surf-home* "doc/window-help/running.doc"))
			   ((:standard-plot plot) (concatenate-strings *surf-home* "doc/window-help/plotting-control.doc.txt"))
			   (:histogram (concatenate-strings *surf-home* "doc/window-help/graphics-control.doc.txt"))
			   (:histology (concatenate-strings *surf-home* "doc/window-help/histology-control.doc.txt"))))
	  (loop while (setq string (read-line stream nil nil nil))
		do (opal:insert-string (g-value info-win :display-object) (format nil "~A~%" (replace-tabs-in-string string)))))
	(s-value info-win :width (min (+ (g-value info-win :display-object :width) 25) 750))))
    (resurrect-opal-win (find-INFO-WINDOW mode nil) :raise t :deiconify t :update t :visible t)))

(s-value help-window-Interactor :final-function #'help-window-inter-function)

