;;; -*- Mode: LISP; Syntax: Common-lisp; Package: XLIB; Base: 10; Lowercase: Yes -*-

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

;Return-Path: <Garnet-Users-Request@a.gp.cs.cmu.edu>
;Date: Mon, 21 Dec 92 14:10:22 EST
;From: Andrew.Mickish@a.gp.cs.cmu.edu
;To: yc0@bobcat.cis.ufl.edu
;Subject: Raise window
;Cc: garnet-users@cs.cmu.edu
;
;Youjun,
;
;A while ago you complained that opal:raise-window and opal:lower-window
;did not work as you expected.  Indeed, I found that a total update of
;the window was required after making the raise- or lower-window call.
;
;I believe I have found the problem that kept the windows from being
;raised or lowered immediately.  In an effort to speed up updates, we
;changed the update method so that it did not do a xlib:display-force-output
;on a window unless something in the window actually changed.  In other words,
;if no objects changed in a window, no work was done in the window at all.
;
;Unfortunately, the raise-window and lower-window functions depended on the
;xlib:display-force-output call that appeared at the end of the old update
;procedure.  Raise- and lower-window do not change any objects -- they change
;the "window priority" of the drawable of the window -- so update was not
;doing anything at all when it was called after raise- and lower-window.
;A total update was causing the change to appear because that redraws everything
;inside and about the window.
;
;So, the fix appears to be the addition of calls to xlib:display-force-output
;at the end of the raise- and lower- window functions.  I have retained the
;calls to update just in case someone is relying on these functions to actually
;perform updates.
;
;You can install these changes by replacing the old function definitions
;in opal/windows.lisp, and calling (garnet-compile "opal:windows") to recompile
;the file.  Then you can remove the total updates in your code that you
;installed to get around this bug.
;
;Please let me know if you have any questions about this fix.
;
;--Andrew Mickish
;
;P.S.  These new definitions should also fix problems with error-gadgets
;failing to become visible after gg:display-error is called.


;;;
;;; New opal:raise-window and opal:lower-window
;;; Install in opal/windows.lisp
;;;
;;; Andrew Mickish - 12/21/92
;;;


(in-package "OPAL" :use '("LISP" "KR"))

(defun raise-window (a-window)
  (when (is-a-p a-window window)
    (let ((drawable (g-value a-window :drawable)))
      (when drawable
	(setf (xlib:window-priority drawable) :above))
      ;; if drawable was NIL, window will appear on top anyway.
      (update a-window)
      ;; if there were no invalid objects in the window, update wouldn't
      ;; have called display-force-output
      (xlib:display-force-output
       (display-info-display (g-value a-window :display-info))))))

(defun lower-window (a-window)
  (when (is-a-p a-window window)
    (let ((drawable (g-value a-window :drawable)))
      (unless drawable
	(setq drawable (create-x-drawable a-window)))
      (setf (xlib:window-priority drawable) :below)
      (update a-window)
      ;; if there were no invalid objects in the window, update wouldn't
      ;; have called display-force-output
      (xlib:display-force-output
       (display-info-display (g-value a-window :display-info))))))

