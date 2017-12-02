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


;;; SYS Source file: event-generators.lisp

(in-package "SURF-HIPPO")


;;; *************** *************** *************** ***************
;;;
;;;                EVENT GENERATOR RELATED FUNCTIONS
;;;
;;; *************** *************** *************** ***************
;;;
;;; These apply for axons and voltage controlled and light/light-event controlled synapses.
;;;

(defun type-event-generators (type)
  ;; Return all event-generators associated with TYPE, which should be either an axon-type or synapse-type. 
  (let ((type (element type)) out)
    (typecase type
      (axon-type
       (loop for axon in (axon-type-axons type) when (eq axon (axon-event-generator axon)) collect axon))
      (synapse-type
       (do ((syn (synapse-type-first-element type) (synapse-next-element syn)))
	   ((null syn) out)
	 (when (eq syn (synapse-event-generator syn))
	   (push syn out)))))))

(defun event-element-control-designator (event-element)
  (typecase event-element
    (synapse (if (light-controlled-p event-element)
		 (light-syn-rf-label event-element)
		 (synapse-pre-synaptic-node event-element)))
    (axon (axon-proximal-node event-element))))

(defun event-generator (event-element &optional only-slot-value)
  "Returns the value of the :EVENT-GENERATOR slot of EVENT-ELEMENT. If NIL, and ONLY-SLOT-VALUE is NIL, then returns
EVENT-ELEMENT."
  (let ((elt (or (element event-element 'synapse) (element event-element 'axon))))
    (if (or *ENABLE-EVENT-GENERATORS* only-slot-value)
	(or (typecase elt
	      (synapse (synapse-event-generator elt))
	      (axon (axon-event-generator elt)))
	    (and (not only-slot-value) elt))
	elt)))

(defun event-generator-p (elt)
  (let ((elt (element elt)))
    (eq elt (event-generator elt))))

(defun event-driven-element-p (elt)
  (let ((type (element-type elt)))
    (typecase type
      (axon-type t)
      (synapse-type
       (case (synapse-type-control type)
	 ((:event :light-event :voltage) t))))))

(defun event-followers (synapse &optional (value nil value-supplied-p))
  (if value-supplied-p
      (element-parameter synapse 'event-followers value)
      (element-parameter synapse 'event-followers)))

(defun setup-event-generators-and-followers-of-type (event-elements)
  ;; All EVENT-ELEMENTS should be of the same type. The 'event-followers parameters list includes the event generator.
  (when (and *SETUP-EVENT-GENERATORS-AND-FOLLOWERS*
	     (not *USER-SPECIFIED-EVENT-ELEMENT-SETS*)
	     *ENABLE-EVENT-GENERATORS*)
    (let (event-element-control-designator sublist)
	  
      (loop for event-element in event-elements

	    ;; EVENT-ELEMENT-CONTROL-DESIGNATOR => 
	    ;; For a light dependent synapse type, this is (light-syn-rf-label event-element).
	    ;; For a voltage synapse type, this is the pre-synaptic node.
	    ;; For an axon, this is the proximal node.
	    
	    do (setq event-element-control-designator (event-element-control-designator event-element))

	    unless (setq sublist (find event-element-control-designator event-element-control-designators-and-elements :key 'car))
	     
	    collect (list event-element-control-designator (list event-element))

	    ;; Group all the EVENT-ELEMENTS according to their common control-designators into
	    ;; EVENT-ELEMENT-CONTROL-DESIGNATORS-AND-ELEMENTS: 
	    ;;
	    ;;     ((control-designator-1 (element element ... element))
	    ;;      (control-designator-2 (element element ... element))
	    ;;                     .
	    ;;                     .
	    ;;                     .
	    ;;      (control-designator-n (element element ... element)))
	    ;;
	    ;; Thus for each unique control-designator, there will be an event-generator, taken as the first element of the
	    ;; associated list, with the remaining elements in the list assigned as event-followers.
	    
	    into event-element-control-designators-and-elements
	    else do (push event-element (cadr sublist))

	    ;;	    (format t "~A ~%" event-element-control-designators-and-elements)
	    finally
	    (loop for event-generator-sublist in event-element-control-designators-and-elements do

		  ;; EVENT-GENERATOR-SUBLIST = (control-designator-x (element element ... element))

		  (let* ((event-elements-w-same-generator (cadr event-generator-sublist))

			 ;; Choose the last pushed element as the event-generator.
			 (event-generator (car event-elements-w-same-generator)))
		    
		    (loop for elt in event-elements-w-same-generator do
			  ;; Why this???  (element-parameter elt 'event-generator event-generator)
			  
			  ;; Set the proper :EVENT-GENERATOR slots.
			  (set-element-event-generator-slot-value elt event-generator))
		    
		    (set-element-event-generator-slot-value event-generator event-generator)
		    ;; The 'EVENT-FOLLOWERS list for an event-generator includes the event-generator.
					; (element-parameter event-generator 'event-followers event-elements-w-same-generator)
		    (event-followers event-generator event-elements-w-same-generator)))))))

(defun set-element-event-generator-slot-value (element value)
  (typecase element
    (synapse (setf (synapse-event-generator element) value))
    (axon (setf (axon-event-generator element) value))))

(defun setup-all-event-generators-and-followers ()
  (let ((*SETUP-EVENT-GENERATORS-AND-FOLLOWERS* t)
	(*USER-SPECIFIED-EVENT-ELEMENT-SETS* nil))
    (loop for type in (flatten-list (axon-types) (synapse-types))
	  when (event-driven-element-p type) do
	  (setup-event-generators-and-followers-of-type (elements-of-type type)))))

(defun user-setup-event-generators-and-followers (event-generator event-followers)
  "Sets the :EVENT-GENERATOR slot of EVENT-GENERATOR and the EVENT-FOLLOWERS to EVENT-GENERATOR, and assigns the list of
EVENT-FOLLOWERS to EVENT-GENERATOR. Ensures that EVENT-GENERATOR is also a member of the actual event-followers."
  (let ((event-generator (element event-generator))
	(event-followers (delete-duplicates (cons event-generator (coerce-to-list (element event-followers))))))
    (loop for elt in event-followers do (set-element-event-generator-slot-value elt event-generator))
    (event-followers event-generator event-followers)
    nil))

#|
(defun get-events (event-element &optional original-event-time-p)
  "Returns a list of event times associated with EVENT-ELEMENT. If ORIGINAL-EVENT-TIME-P is T, then
the event times correspond to the original generation of those events, as opposed to the times when
the events trigger the EVENT-ELEMENT response. EVENT-ELEMENT is typically an axon or a
voltage-dependent or event synapse."
  (let* ((event-element (element event-element))
	 (event-generator (event-generator event-element)))
    (typecase event-element
      (axon
       (if original-event-time-p
	   (axon-spike-times event-generator)
	   (loop for time in (axon-spike-times event-generator)
		 collect (+ (axon-propagation-delay event-element) (axon-delay event-element) time))))
      (synapse
       (if original-event-time-p
	   (synapse-event-times event-generator)
	   (loop for time in (synapse-event-times event-generator) collect (+ (synapse-delay event-element) time)))))))
|#

(defun GET-EVENTS (event-element &optional original-event-time-p)
  (declare (optimize (speed 3)))
  ;; This version incorporates NG's elaboration vis-a-vis synapse EVENT-TIMES-HOLDER.
  "Returns a list of event times associated with EVENT-ELEMENT. If ORIGINAL-EVENT-TIME-P is T, then the event times correspond to
the original generation of those events, as opposed to the times when the events trigger the EVENT-ELEMENT response. EVENT-ELEMENT
is typically an axon or a voltage-dependent or event synapse."
  (let ((event-element (element event-element)))
    (typecase event-element
      (axon
       (let ((event-generator (event-generator event-element)))
	 (if original-event-time-p
	   (axon-spike-times event-generator)
	   (loop for time single-float in (axon-spike-times event-generator)
		 collect (+ (axon-propagation-delay event-element)
			    (axon-delay event-element)
			    time)))))
      (synapse
       ;; For synapses, the holder is another synapse whose :EVENT-TIMES slot holds the spike-times that the current synapse
       ;; uses. The holder is different than the generator in the case where the conversion of the frequency-waveform into events
       ;; is done synapse by synapse, ie. when *CONVERT-LIGHT-RESPONSE-TO-EVENTS-FOR-EACH-SYNAPSE* is T. The holder is the same as
       ;; the generator when the generator events are used for constructing the open activation times of all its followers.
       (let ((event-times-holder
	      (if *CONVERT-LIGHT-RESPONSE-TO-EVENTS-FOR-EACH-SYNAPSE*
		event-element
		(event-generator event-element))))
	 (if original-event-time-p
	   (synapse-event-times event-times-holder) 
	   (loop for time single-float in (synapse-event-times event-times-holder)
		 collect (+ (the sf (synapse-delay event-element)) (the sf time)) single-float)))))))









