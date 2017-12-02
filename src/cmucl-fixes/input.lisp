;;; -*- Mode: LISP; Syntax: Common-lisp; Package: XLIB; Base: 10; Lowercase: Yes -*-

;;; This file contains definitions for the DISPLAY object for Common-Lisp X windows version 11

;;;
;;;			 TEXAS INSTRUMENTS INCORPORATED
;;;				  P.O. BOX 2909
;;;			       AUSTIN, TEXAS 78769
;;;
;;; Copyright (C) 1987 Texas Instruments Incorporated.
;;;
;;; Permission is granted to any individual or institution to use, copy, modify,
;;; and distribute this software, provided that this complete copyright and
;;; permission notice is maintained, intact, in all copies and supporting
;;; documentation.
;;;
;;; Texas Instruments Incorporated provides this software "as is" without
;;; express or implied warranty.
;;;

;;;
;;; Change history:
;;;
;;;  Date	Author	Description
;;; -------------------------------------------------------------------------------------
;;; 12/10/87	LGO	Created
#+cmu
(ext:file-comment
  "$Header: /home/CVS-cmucl/src/clx/input.lisp,v 1.7 1999/03/16 23:37:43 pw Exp $")

(in-package :xlib)

(defvar *read-reply-read-input-timeout* 10) ; seconds

(defun read-reply (display pending-command)
  (declare (type display display)
	   (type pending-command pending-command))
  (loop
   (when (read-input display *read-reply-read-input-timeout*
		     nil
		     #'(lambda (pending-command)
			 (declare (type pending-command pending-command))
			 (not (null (pending-command-reply-buffer pending-command))))
		     pending-command)
     (format t "Calling (x-error 'closed-display :display display)~%")
     (x-error 'closed-display :display display))
   (let ((reply-buffer
	  (with-event-queue-internal (display)
				     (threaded-pop (pending-command-reply-buffer pending-command)
						   reply-next reply-buffer))))
     (declare (type reply-buffer reply-buffer))
     ;; Check for error.
     (with-buffer-input (reply-buffer)
			(ecase (read-card8 0)
			  (0 (apply #'report-error display
				    (prog1 (make-error display reply-buffer nil)
				      (deallocate-reply-buffer reply-buffer))))
			  (1 (return reply-buffer)))))))

