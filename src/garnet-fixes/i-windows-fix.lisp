;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: INTERACTORS; Base: 10 -*-

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

;; Fixes for Garnet 3.0 where destroyed windows could mess up button handling.

;; Original functions in inter/i-windows.lisp



(in-package "INTERACTORS")


(defun Button-Press (window x y state code event-key time)
  (let (c)
    ;; Change 8/24/98 LBG
    ;; Check for null window before getting gem:Check-Double-Press code.
    (when (null window);; if window was just destroyed, exit.
      (return-from button-press t))
    (setf code (gem:Check-Double-Press window state code time))
    (when (or (null window);; if window was just destroyed, exit.
              (null code));; if bad double press (Mac) then exit
      (return-from button-press t))
    (setf c (gem:translate-mouse-character window code state event-key))
    (if-debug
     :event 
     (format
      t "~%<><><><> Button down ~s event=~s code=~s state=~s window=~s"
      c event-key code state window)
     (format t " time=~s x=~s  y=~S~%" time x y))
    (setf (event-char *Current-Event*) c		
	  (event-mousep *Current-Event*) t
	  (event-x *Current-Event*) x
	  (event-y *Current-Event*) y
 	  (event-code *Current-Event*) code
	  (event-downp *Current-Event*) t 
	  (event-window *Current-Event*) window
	  (event-timestamp *Current-Event*) time
	  (event-state *Current-Event*) state
	  )	
    (trans-out-and-process-current-event))
  t)

(defun Button-Release (window x y state code event-key time)
  ;; Change 8/24/98 LBG
  ;; Check for null window before getting gem:translate-mouse-character code.
  (if (null window)			; if window was just destroyed, exit.
    (return-from button-release t))
  (let ((c (gem:translate-mouse-character window code state event-key)))
    (when (null window)
      (return-from button-release t));; if window was just destroyed, exit.
    (if-debug :event 
	      (format t "~%<><><><> Button Up ~s event=~s code=~s state=~s window=~s"
		      c event-key code state window)
	      (format t " time=~s x=~s  y=~s~%" time x y))
    (setf (event-char *Current-Event*) c		
	  (event-mousep *Current-Event*) t
	  (event-x *Current-Event*) x
	  (event-y *Current-Event*) y
 	  (event-code *Current-Event*) code
 	  (event-downp *Current-Event*) nil
	  (event-window *Current-Event*) window
	  (event-timestamp *Current-Event*) time
	  (event-state *Current-Event*) state
	  )
    (trans-out-and-process-current-event))
  t)


#|
;; Debug
(define-method :update interactor-window (window &optional (total nil))
  (declare (ignore window total)))

|#
