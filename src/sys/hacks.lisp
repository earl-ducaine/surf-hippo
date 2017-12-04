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


;;; SYS Source file: hacks.lisp

(IN-PACKAGE "SURF-HIPPO")

;; Miscellaneous hacks.

(defun type-on-the-path-p (type target)
  "From the point of view of the cell element associated with TARGET, is an element of TYPE on the path to the soma."
  (let ((type (element-type type))
	(cell-element (element-cell-element target)))
    (when (and type cell-element)
      (loop for seg in (cons cell-element (segments-in cell-element))
	    thereis (loop for elt in (node-elements (element-physical-node seg))
			  thereis (eq (element-type elt) type))))))

;; ************* ************* ************* *************
;;
;;               Branch Related Functions
;;
;; ************* ************* ************* *************

(defun branch (element &optional type)
  "Returns branch associated with ELEMENT."
  (let ((segment (element-cell-element element type)))
    (loop for branch in *branch-list*
	  when (member segment branch) do (return branch))))

(defun identify-branch (element) (branch element))

(defun look-at-branchs ()
  "Print out branch structure of circuit."
  (dolist (branch *branch-list*)
    (print "A new branch:")
    (dolist (segment branch)
      (if (eq (named-structure-symbol segment) 'segment)
	  (format t "Branch segment ~a ~%" (segment-name segment))
	  (format t "soma - ~a ~%" (soma-name segment))))))

(defun branch-ends (element)
  "Returns the proximal and distal end segments of the branch associated with ELEMENT."
  (let ((branch (branch (element-cell-element element))))
    (list (car (last branch)) (first branch))))

(defun branch-elements (branch-element element-type &optional total-segments ends)
  "Returns a list of elements of type ELEMENT-TYPE on branch associated with BRANCH-ELEMENT. If TOTAL-SEGMENTS is a
number, then segments of the branch are chosen mod TOTAL-SEGMENTS. If ENDS is non-NIL, then only the ends of the branch
are considered. Otherwise, all segments of a branch are examined. A branch is defined as a set of singly connected
segments whose proximal and distal ends are nodes with more than 2 segments, or a termination (soma or distal tip)
point."
  (let ((branch (branch branch-element)))
    (if (not branch)
	(format t "Branch ~a not found!~%" branch-element)
	(let* ((segments-in-branch (length branch))
	       (elts
		(loop for segment in (reverse branch)
		      for count from 0
		      when
		      (if ends
			  (or (= 0 count) (= (1- (length branch)) count))
			  (if total-segments
			      (= 0 (mod count (max 1 (ceiling (/ segments-in-branch total-segments)))))
			      t))
		      nconcing
		      (if (eq element-type 'segment)
			  (list segment)
			  (delete-duplicates
			   (loop for elt in (node-elements (segment-node-2 segment))
				 when (eq element-type (type-of elt))
				 collect elt))))))
	  (delete-duplicates elts)))))

(defun get-branch-elements (branch-element element-type &optional total-segments ends)
  (branch-elements branch-element element-type total-segments ends))

(defun get-branch-element-names (branch-element element-type &optional total-segments ends)
  (element-name (BRANCH-ELEMENTS branch-element element-type total-segments ends)))

(defun get-branch-synapses-of-type (branch-element type &optional total-segments ends)
  (branch-synapses-of-type branch-element type total-segments ends))

(defun branch-synapses-of-type (branch-element type &optional total-segments ends)
  "Returns a list of synapses of TYPE that are associated with BRANCH-ELEMENT (an element on a branch), using BRANCH-ELEMENTS."
  (let ((type (element type)))
    (loop for syn in (BRANCH-ELEMENTS branch-element 'synapse total-segments ends)
	  when (eq type (synapse-type syn)) collect syn)))

(defun get-branch-synapse-names-of-type (branch-element type  &optional total-segments ends)
  (branch-synapse-names-of-type branch-element type total-segments ends))

(defun branch-synapse-names-of-type (branch-element type  &optional total-segments ends)
  (element-name (branch-synapses-of-type branch-element type total-segments ends)))

(defun get-branch-channels-of-type (branch-element type  &optional total-segments ends)
  (branch-channels-of-type branch-element type total-segments ends))

(defun branch-channels-of-type (branch-element type &optional total-segments ends)
  "Returns a list of channels of TYPE that are associated with BRANCH-ELEMENT (an element on a branch), using BRANCH-ELEMENTS."
  (let ((type (element type)))
    (loop for ch in (BRANCH-ELEMENTS branch-element 'channel total-segments ends)
	  when (eq type (channel-type ch))
	  collect ch)))

(defun get-branch-channel-names-of-type (branch-element type &optional total-segments ends)
  (branch-channel-names-of-type branch-element type total-segments ends))

(defun branch-channel-names-of-type (branch-element type  &optional total-segments ends)
  (element-name (branch-channels-of-type branch-element type total-segments ends)))

;; ************* ************* ************* *************
;;
;;    GC Notification Related Functions
;;
;; ************* ************* ************* *************

(create-instance 'break-into-simulation-window-interactor inter:button-Interactor
		 (:continuous nil) (:start-where t)
		 ;; LG fix 01.09.2016 CMUCL 20 reads :CONTROL-LATIN_CAPITAL_G instead of :CONTROL-G
		 ;; (:start-event :control-\c)
		 (:start-event '(:control-\c :CONTROL-LATIN_SMALL_LETTER_C))
		 (:final-function
		  #'(lambda (interactor final-obj-over)
		      (declare (ignore final-obj-over))
		      (s-value (g-value  interactor :window) :break t))))

#|
(create-instance 'hide-window-interactor inter:button-Interactor
		 (:continuous nil) (:start-where t) (:start-event '(#\h #\H))
		 (:final-function #'hide-window-interactor-function))
|#

(defvar *simulation-timer-window* nil)

(defun get-SIMULATION-TIMER-WINDOW ()
  (let ((win (create-instance nil inter:interactor-window
			      (:visible nil)
			      (:default-graphics-color opal::black)
			      (:background-color CHARTREUSE3) (:title "") (:width 230) (:height 140)
			      (:aggregate (create-instance nil opal:aggregate)))))
    ;; (s-value win :break-interactor (create-instance nil break-into-simulation-window-interactor (:Window win)))
    ;;    (create-instance nil hide-window-interactor (:Window win))

    (s-value win :background-color CHARTREUSE3)
    (s-value win :title "Surf-Hippo Time")
    (s-value win :title-font (opal:get-standard-font :serif :bold-italic :medium))
    (s-value win :gauge (create-instance nil (if *motif-gui-p* garnet-gadgets:motif-gauge garnet-gadgets:gauge)
					 (:polygon-needle-p nil) (:val-1 100) (:val-2 0)
					 (:width 180)))
    (opal:add-component (g-value win :aggregate) (g-value win :gauge))
    (s-value win :gauge :num-marks 11)
    (s-value win :gauge :value-feedback-p nil)
    (opal:add-component
     (g-value win :aggregate)
     (create-instance nil opal:text (:visible t) (:top 0) (:left 10) (:font (g-value win :title-font))))
    win))

(defun init-timer-window ()
  (unless (opal-obj-exists *SIMULATION-TIMER-WINDOW*)
    (setq *SIMULATION-TIMER-WINDOW* (get-SIMULATION-TIMER-WINDOW)))
  (s-value *SIMULATION-TIMER-WINDOW*
	   :width (+ 20 (max
			 (g-value *SIMULATION-TIMER-WINDOW* :gauge :width)
			 (opal:string-width (g-value *SIMULATION-TIMER-WINDOW* :gauge :title-font) (get-timer-window-title)))))
  (fix-window-size *SIMULATION-TIMER-WINDOW*)
  (s-value *SIMULATION-TIMER-WINDOW* :gauge :left (round (/ (- (g-value *SIMULATION-TIMER-WINDOW* :width)
							       (g-value *SIMULATION-TIMER-WINDOW* :gauge :width)) 2)))
;  (resurrect-opal-win *SIMULATION-TIMER-WINDOW* :raise t :visible nil :update t)
;  (resurrect-opal-win *SIMULATION-TIMER-WINDOW* :raise t :visible t :update t)
  )

(defun get-timer-window-title ()
  (format nil "~A" (if (or *always-add-host-name-to-windows* (and (not (HOST-IS-DISPLAY-SERVER)) *add-host-name-to-windows*))
		     (format nil "(~A) SH % Simulation Time" *displayed-host-name*)
		     (format nil "% Simulation Time"))))

(defun update-timer-window (&optional (time *real-time*) (expose t) force)
  (declare (optimize (safety 0) (speed 3) (space 0)))
  (when (or force (not *kill-all-output*))
    (unless (opal-obj-exists *SIMULATION-TIMER-WINDOW*)
      (setq *SIMULATION-TIMER-WINDOW* (get-SIMULATION-TIMER-WINDOW)))
    (s-value *simulation-timer-window* :visible expose)
    (s-value *simulation-timer-window* :gauge :value (* 100.0 (/ (the sf time) *user-stop-time*)))
    (s-value *simulation-timer-window* :gauge :title (get-timer-window-title))
    (add-comment *SIMULATION-TIMER-WINDOW*
		 (format nil "Time: ~,2fms (Stop time ~ams)"
			 time
			 (if (= *user-stop-time* (round *user-stop-time*)) (format nil "~D" (round *user-stop-time*)) (format nil "~,2f" *user-stop-time*)))
		 :position :lower-middle)
;;    (when (gv *SIMULATION-TIMER-WINDOW* :break) (format t "Caught break!!!~%"))
    (opal::update *simulation-timer-window*)))

#|
(create-instance 'gc-notify-window
		 inter:interactor-window
		 (:visible nil) (:background-color opal:red) (:title "GC NOTIFICATION") (:width 376) (:height 16) (:aggregate (create-instance nil opal:aggregate)))

(opal:add-component
 (g-value gc-notify-window :aggregate)
 (create-instance nil opal:text (:visible t) (:top 0) (:left 10) (:font (opal:get-standard-font :serif :bold-italic :medium))))
|#

(defun simulation-trial-message (trial &optional num-trials message)
  "Useful for printing out iteration numbers. Insert this in the
   iteration loops in user defined simulation functions."
  (format *error-output* "Trial: ~A"
	  ;; (di:debug-function-name
	  ;;  (di:frame-debug-function
	  ;;   (di:frame-down (di::top-frame))))
	  trial)
  (when num-trials (format *error-output* " (out of ~A)" num-trials))
  (when message (format t " ~A~%" message))
  (format *error-output* "~%"))

(defvar *log-gc-to-file* nil "When true [default NIL], GC messages will be written to a text file.")
(defvar *use-gc-announce-window* nil)	; problems with save-image version.
(defvar *GC-ANNOUNCE-text* nil)

;; Don't use until a safe beep is found...
(defvar *beep-after-gc* t ; "When true [default], GC will beep when done. Useful signal for long simulations to verify machine is breathing."
  )

(defun announce-gc (bytes-in-use)
  ;; noop
  )

(defun gc-announce-to-window (bytes-in-use)
  (let ((announce-string (format nil "GC threshold exceeded with ~:D bytes in use. Commencing GC." bytes-in-use)))
    (if nil				; *use-gc-announce-window*
      (progn  (s-value GC-NOTiFY-WINDOW :visible t)
	      (s-value (loop for comp in (g-value gc-notify-window :aggregate :components)
			     when (eq opal:text (car (g-value comp :is-a)))
			     do (return comp))
		       :string announce-string)
	      (opal:update GC-NOTiFY-WINDOW t))

      (format t "~a~%"  announce-string))))

(defun announce-gc-done (bytes-retained bytes-freed new-threshold)
  ;; noop
  )
  ;; (cond-every
  ;;  ((> new-threshold *gc-bytes-retained-warning-threshold*) (gc-bytes-retained-warning-threshold new-threshold))
  ;;  (*log-gc-to-file* (log-gc-done-to-file bytes-retained bytes-freed new-threshold))
  ;;  ;; Source of (COMMON-LISP::SUB-SERVE-EVENT NIL 0) bug?
  ;;  ;; (*beep-after-gc* (inter:beep))
  ;;  ;; And this doesn't work
  ;;  ;;  (*beep-after-gc* (progn (system:beep) (system:beep)))
  ;;  )
  ;; (finish-output))

(defun gc-done-announce-to-window (bytes-retained bytes-freed new-threshold)
  (let ((announce-string
	 (format nil "GC finished - bytes retained ~:D, bytes freed ~:D, new-threshold ~:D ~%" bytes-retained bytes-freed new-threshold)))
    (if nil				; *use-gc-announce-window*
      (progn
	(s-value (loop for comp in (g-value gc-notify-window :aggregate :components)
		       when (eq opal:text (car (g-value comp :is-a)))
		       do (return comp))
		 :string announce-string)
	(opal:update GC-NOTiFY-WINDOW)
	(s-value GC-NOTiFY-WINDOW :visible nil)   (opal:update GC-NOTiFY-WINDOW)
	(opal:update GC-NOTiFY-WINDOW) (opal:update GC-NOTiFY-WINDOW)  (opal:update GC-NOTiFY-WINDOW))
      (format t "~a~%"  announce-string))))

(defun gc-bytes-retained-warning-threshold (new-threshold)
  (inter:beep)	(inter:beep) (inter:beep) (inter:beep)	(inter:beep) (inter:beep)
  (inter:beep)	(inter:beep) (inter:beep) (inter:beep)	(inter:beep) (inter:beep)
  (format t "************************************************************~%")
  (format t "************************************************************~%")
  (format t "***************  Garbage Collection Warning  ***************~%")
  (format t "***************   - Heap Getting Too Big -   ***************~%")
  (format t "************************************************************~%")
  (format t "************************************************************~%~%")
  (format t "The new threshold of ~:D for garbage collection puts you into~%" new-threshold)
  (format t "dangerous territory. Possibilities include the following:~%")
  (format t "  1. Simulation is too large~%")
  (format t "  2. Simulation is too long~%")
  (format t "  3. There are too many plots hanging around,~%")
  (format t "     either in separate windows, or in overlaid plots.~%")
  (format t "  4. There is too much simulation data.~%")
  (when (> (LENGTH-ELEMENT-OUTPUT-DATA) 0)
    (format t "     The length of all the simulation data lists is ~D.~%" (LENGTH-ELEMENT-OUTPUT-DATA))
    (format t "     Use CLEAR-ELEMENT-OUTPUT-DATA to clear these.~%"))

  (format t "~%")
  (format t "  You may want to consider saving vital stuff~%")
  (format t "  and punting LISP (quit-sh) to be safe.~%~%")
  (format t "  Risky: Change value of *GC-BYTES-RETAINED-WARNING-THRESHOLD* (currently ~e).~%" *gc-bytes-retained-warning-threshold*)
  (format t "~%")
  (format t "~%")
  (BREAK  "Continue the simulation and risk losing everything, including crashing X" 'ERROR))

(defun log-gc-done-to-file (bytes-retained bytes-freed new-threshold)
  (let* ((pathname-directory (get-surf-data-directory))
	 (log-filename (format nil "~Agc.log" pathname-directory)))
    (mkdir (namestring pathname-directory nil) #o777)
    (when (probe-file (namestring pathname-directory nil))
      (with-open-stream (stream (open log-filename :direction :output :IF-DOES-NOT-EXIST :CREATE :if-exists :append))
	(format stream "GC finished - bytes retained ~:D, bytes freed ~:D, new-threshold ~:D ~%"
		bytes-retained bytes-freed new-threshold)))))

(defun log-gc-to-file (bytes-in-use)
  (let* ((pathname-directory (get-surf-data-directory))
	 (log-filename (format nil "~Agc.log" pathname-directory)))
    (unix-mkdir (namestring pathname-directory nil) #o777)
    (when (probe-file (namestring pathname-directory nil))
      (with-open-stream (stream (open log-filename :direction :output :IF-DOES-NOT-EXIST :CREATE :if-exists :append))
	(format stream "GC threshold exceeded with ~:D bytes in use. Commencing GC.~%" bytes-in-use)))))

;; 09.11.00 The root of the SUB-SERVE-EVENT bug?
;(setq ext:*gc-notify-before* #'ANNOUNCE-GC)
;(setq ext:*gc-notify-after* #'ANNOUNCE-GC-done)

;; 09.28.00 The problem appears to be the call to inter:beep, which is different than the system:beep that may be called by the default
;; COMMON-LISP::DEFAULT-GC-NOTIFY-before and COMMON-LISP::DEFAULT-GC-NOTIFY-after functions.

;;(defun COMMON-LISP::DEFAULT-GC-NOTIFY-before (bytes-in-use)
;;  (format t "new gc with ~a~%" bytes-in-use))

;; 09.11.00 These bindings seem to avoid the SUB-SERVE-EVENT bug.
;(defun COMMON-LISP::DEFAULT-GC-NOTIFY-before (bytes-in-use) nil)
;(defun COMMON-LISP::DEFAULT-GC-NOTIFY-AFTER (bytes-retained bytes-freed new-threshold) nil)
;;
;;(setq ext:*gc-notify-before* #'ext::DEFAULT-GC-NOTIFY-before)
;;(setq ext:*gc-notify-before* #'COMMON-LISP::DEFAULT-GC-NOTIFY-before)
;;;;(setq ext:*gc-notify-after* #'ext::DEFAULT-GC-NOTIFY-AFTER)
;;(setq ext:*gc-notify-after* #'COMMON-LISP::DEFAULT-GC-NOTIFY-AFTER)
