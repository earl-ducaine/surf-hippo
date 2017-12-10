;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: WINDOWS-HACK; Base: 10 -*-

;; The Surf-Hippo Neuron Simulator System
;;
;; This code was written as part of the Surf-Hippo Project, originally
;; at the Center for Biological Information Processing, Department of
;; Brain and Cognitive Sciences, Massachusetts Institute of
;; Technology, and currently at the Neurophysiology of Visual
;; Computation Laboratory, CNRS.
;;
;; Permission to use, copy, modify, and distribute this software and
;; its documentation for any purpose and without fee is hereby
;; granted, provided that this software is cited in derived published
;; work, and the copyright notice appears in all copies and in
;; supporting documentation. The Surf-Hippo Project makes no
;; representations about the suitability of this software for any
;; purpose. It is provided "as is" without express or implied
;; warranty.
;;
;; If you are using this code or any part of Surf-Hippo, please
;; contact surf-hippo@ai.mit.edu to be put on the mailing list.
;;
;; Copyright (c) 1989 - 2003, Lyle J. Graham

(in-package :windows-hack)

;;; This file contains some basic menu routines, inspired by the
;;; CHOOSE-VARIABLE-VALUES style of menus in the Symbolics window
;;; system, and built upon the Garnet GUI toolset from CMU. This
;;; system requires that WINDOW-HACK be loaded.

(defvar *motif-gui-p* t)		; Set to T for Motif look-and-feel.
(defvar *use-motif-gui-background* nil)	; Trying to get motif window backgrounds to work.

(s-value garnet-gadgets:motif-check-button :button-width 18)
(s-value garnet-gadgets:motif-check-button :toggle-p t)
(s-value garnet-gadgets:motif-check-button-panel :button-width 18)
(s-value garnet-gadgets:motif-radio-button-panel :button-width 18)
(s-value garnet-gadgets:motif-check-button-panel :toggle-p t)

(defvar *window-manager-char-width* 8)	; This is empirical for TWM.
(defvar *window-manager-mouse-box-width* 18)	; This is empirical for TWM.

(proclaim '(fixnum *window-manager-char-width* *window-manager-mouse-box-width*))

(defvar *menu-entry-v-spacing 10)

#|
(defun generate-and-set-special-vars (values)
  (let (sym)
    (loop for value in values for i from 1
	  do (setq sym (gentemp))
	  (eval (list 'defvar sym value))
	  collect sym)))
(defun generate-and-set-special-vars (values)
  (loop for value in values
	for sym in (list-of-dummys (length values))
	do `(setf ,sym ,value)
	collect sym))
|#

(defun generate-and-set-special-vars (values)
  (loop for value in values
	for sym in (list-of-dummys (length values))
	do (eval (list 'setf sym value))
	collect sym))


#|
;; Backwards??
(s-value gg::X-BUTTON :button-left (o-formula (if (gvl :text-on-left-p)
						  (+ (gvl :left) (gvl :text-width) (gvl :text-offset))
						  (gvl :left))))

(s-value garnet-gadgets:x-button-panel :h-align (o-formula (if (gvl :text-on-left-p) :right  :left )))
|#

(s-value gg::radio-BUTTON :Text-on-left-p t)
(s-value gg::radio-BUTTON :toggle-p t)
(s-value gg::radio-BUTTON-panel :toggle-p t)
(s-value gg::radio-BUTTON-panel :Text-on-left-p (o-formula (not (eq (gvl :direction) :vertical))))
(s-value gg::X-BUTTON :Text-on-left-p t)
(s-value gg::X-BUTTON :toggle-p t)
(s-value garnet-gadgets:x-button-panel :Text-on-left-p t)
(s-value garnet-gadgets:x-button-panel :toggle-p t)
(s-value gg::MOTIF-CHECK-BUTTON :Text-on-left-p t)
(s-value gg::MOTIF-CHECK-BUTTON-PANEL :Text-on-left-p (o-formula (not (eq (gvl :direction) :vertical))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun gadget-button-string-function (thing)
  ;; So that dashes and underscores in atoms are not shown in menu labels.
  (if (or (stringp thing) (schema-p thing))
      thing
      (if (or (numberp thing) (keywordp thing))
	  (nice-string-from-atom thing)
	  (string thing))))

(s-value gg::MOTIF-CHECK-BUTTON-PANEL :strings (o-formula (mapcar #'(lambda (item-obj) (gadget-button-string-function item-obj)) (gvl :item-objs))))

(s-value gg::MOTIF-RADIO-BUTTON-TEXT-LABEL-PROTOTYPE :string (o-formula (let ((s (gv (kr-path 0 :parent) :string))) (GADGET-BUTTON-STRING-FUNCTION s))))

(s-value gg::MOTIF-CHECK-BUTTON-TEXT-LABEL-PROTOTYPE :string (o-formula (let ((s (gv (kr-path 0 :parent) :string))) (GADGET-BUTTON-STRING-FUNCTION s))))

(s-value gg::BESIDE-BUTTON-TEXT :string (o-formula (let ((s (gv (kr-path 0 :parent) :string))) (GADGET-BUTTON-STRING-FUNCTION s))))

(s-value gg::MOTIF-RADIO-BUTTON-PANEL :strings (o-formula (mapcar #'(lambda (item-obj) (gadget-button-string-function item-obj)) (gvl :item-objs))))

;; For setting the final width of a menu - added to the width
(defvar *menu-width-fudge* 30)

;; taken up by the various menu interactors plus the width of the OK button.
(defvar *menu-height-fudge* 30)

;; :sans-serif scrolling gadget needs fixed font
(defvar *menu-comment-font*
  (opal:get-standard-font :fixed
			  :bold-italic :medium))

;; :sans-serif scrolling gadget needs fixed font
(defvar *menu-text-font* (opal:get-standard-font :fixed
						 :bold :medium))

(defparameter *menu-font*
  (create-instance nil opal:font (:face :bold)))

;; :sans-serif scrolling gadget needs fixed font
(defparameter *menu-button-font*
  (opal:get-standard-font :fixed
			  :bold :medium))

;; :sans-serif scrolling gadget needs fixed font
(defparameter *color-menu-button-font*
  (opal:get-standard-font
   :fixed
   :roman :small))

;; These will let us expose menus wherever the last menu was placed.
(defvar *menu-top* nil)
(defvar *menu-left* nil)

(create-instance '*menu-win* basic-graphics-window
  (:visible nil)
  (:title "SH Menu")
  (:mode :menu)
  ;; Add these :left and :top formulae so that the first menu will be
  ;; centered on the screen, and all others will track the (updated)
  ;; values of *MENU-LEFT* and *MENU-TOP*.
  (:left (o-formula (or *menu-left* (round (/ (- gem:*screen-width* (gvl :width)) 2)))))
  (:top (o-formula (or *menu-top* (round (/ (- gem:*screen-height* (gvl :height)) 2))))))

(create-instance '*ss-win-w-bars* gg:scrolling-window-with-bars (:visible nil)
		 (:title "SH Menu") (:icon-title (o-formula (gvl :title)))
		 ;; (:left (o-formula (or *menu-left* (round (/ (- *screen-width* (gvl :width)) 2)))))
		 ;; (:top (o-formula (or *menu-top* (round (/ (- *screen-height* (gvl :height)) 2)))))
		 (:h-scroll-bar-p nil) (:scr-trill-p nil) (:mode :menu))

;; Access the :value slot to initialize the formula in the slot and
;; establish dependencies (Garnet Manual p.300).
(gv *ss-win-w-bars* :value)

(create-instance '*motif-ss-win-w-bars* gg:motif-scrolling-window-with-bars
  (:visible nil)
  (:title "SH Menu")
  (:icon-title (o-formula (gvl :title)))
  (:background-color (when *motif-gui-p* OPAL:MOTIF-green))
  ;; (:left (o-formula (or *menu-left* (round (/ (- *screen-width* (gvl :width)) 2)))))
  ;; (:top (o-formula (or *menu-top* (round (/ (- *screen-height* (gvl :height)) 2)))))
  (:h-scroll-bar-p nil)
  (:scr-trill-p nil)
  (:mode :menu))

;; Access the :value slot to initialize the formula in the slot and
;; establish dependencies (Garnet Manual p.300).
(gv *motif-ss-win-w-bars* :value)

(defun get-menu-window (height label)
  (let ((win (create-instance nil *menu-win* (:height height)
			      (:title (GET-win-TITLE-STRING label))
			      (:aggregate (create-instance nil opal:aggregate)))))
    (s-value win :aggregate :window win)
    (when (and *motif-gui-p* *use-motif-gui-background*)
      (opal:add-component (gv win :aggregate) (create-instance nil gg:motif-background)))
    (create-instance NIL destroy-window-Interactor (:Window win))
    win))

(defun get-regular-menu-window (height width label &optional title)
  (let ((win (create-instance nil *menu-win*
	       ;; (:background-color (when *motif-gui-p* OPAL:MOTIF-GRAY))
	       (:background-color (when *motif-gui-p* OPAL:MOTIF-green))
			      (:height height) (:width width)
			      (:title (or title (GET-win-TITLE-STRING label)))
			      (:aggregate (create-instance nil opal:aggregate)))))
    (s-value win :aggregate :window win)
    (when (and *motif-gui-p* *use-motif-gui-background*) (opal:add-component (gv win :aggregate) (create-instance nil gg:motif-background)))
    (when nil
      (opal:add-component (gv win :aggregate) (create-instance nil gg:motif-background
                                                                    (:filling-style (create-instance nil opal:filling-style
                                                                                                     (:foreground-color *original-motif-gray*))))))
    (create-instance NIL destroy-window-Interactor (:Window win))
    (create-instance nil resurrect-window-Interactor (:Window win))
    win))

(defun retrieve-menu-thing (array prototype)
  (declare (optimize (safety 1) (speed 3) (space 0)))
  (loop for index from 0 to 49
	when (and (opal-obj-exists (aref (the (simple-array opal::schema (*)) array) index))
		  (not (gv (aref (the (simple-array opal::schema (*)) array) index) :in-use)))
	return (progn (s-value (aref (the (simple-array opal::schema (*)) array) index) :in-use t)
		      (aref (the (simple-array opal::schema (*)) array) index))
	finally (return (create-instance nil prototype))))

(defun ok-function (object value)
  (declare (ignore value object))
  (inter:interaction-complete))

(create-instance '*ok-button* garnet-gadgets:text-button
		 (:string "OK")
		 (:font (opal:get-standard-font :serif :bold :large))
		 (:top (o-formula
			(if (and (gvl :window :SCROLL-WIN-GADGET)
				 (gvl :window :SCROLL-WIN-GADGET :Y-OFFSET))
			    (- 5 (the fn (gvl :window :SCROLL-WIN-GADGET :Y-OFFSET)))
			    5)))
		 (:selection-function #'ok-function))

(create-instance '*motif-ok-button* gg:motif-text-button
		 (:string "OK")
		 (:font  (opal:get-standard-font :serif :bold :large)
		  ; (opal:get-standard-font :sans-serif :roman :large)
		  )
		 (:top (o-formula
			(if (and (gvl :window :SCROLL-WIN-GADGET)
				 (gvl :window :SCROLL-WIN-GADGET :Y-OFFSET))
			    (- 5 (the fn (gvl :window :SCROLL-WIN-GADGET :Y-OFFSET)))
			    5)))
		 (:selection-function #'ok-function))

;;Access the :value slot to initialize the formula in the slot and establish dependencies (Garnet Manual p.300).
(gv *ok-button* :value)
(gv *motif-ok-button* :value)

(defvar *ok-buttons* (sequence-to-gen-array (loop for i from 1 to 50 collect (create-instance nil *ok-button*))))
(defvar *motif-ok-buttons* (sequence-to-gen-array (loop for i from 1 to 50 collect (create-instance nil *motif-ok-button*))))

(defun add-ok-button (win top-agg &key (label "OK"))
  ;; Adds a properly positioned OK button interactor to TOP-AGG. The width of the object is 43.
  (declare (optimize (safety 1) (speed 3) (space 0)))
  (s-value win :menu-p t)		; This should get all menus.
  (let ((ok-button
	 (if *motif-gui-p*
	   (retrieve-menu-thing *motif-ok-buttons* *motif-ok-button*)
	   (retrieve-menu-thing *ok-buttons* *ok-button*))))
    (s-value ok-button :left (o-formula (- (the fn (if (gvl :window :SCROLL-WIN-GADGET)
						     (gvl :window :SCROLL-WIN-GADGET :width)
						     (gv win :width)))
					   (the fn (+ (the fn (if (gvl :window :SCROLL-WIN-GADGET) 20 5))
						      (the fn (gvl :width)))))))
    (if *motif-gui-p*
      (s-value ok-button :foreground-color chartreuse3)
      (s-value ok-button :white-field :filling-style opal:green-fill))
    (s-value ok-button :string label)
    (gv ok-button :value)
    (s-value ok-button :value nil)
    (opal:add-component top-agg ok-button)))

(defun update-*ok-button* (win)
  (declare (optimize (safety 1) (speed 3) (space 0)))
  (s-value *ok-button* :left (- (gv win :width) 70))
  (s-value *ok-button* :parent nil)
  *ok-button*)

(defun ok-button () (if *motif-gui-p* *motif-ok-button* *ok-button*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun abort-function (object value)
  (declare (ignore value object))
  (inter:interaction-complete)
  (break))

(create-instance '*abort-button* garnet-gadgets:text-button
		 (:string "ABORT")
		 (:font (opal:get-standard-font :serif :bold :large))
		 (:top (o-formula
			(+ (gvl :height)
			   (if (and (gvl :window :SCROLL-WIN-GADGET)
				    (gvl :window :SCROLL-WIN-GADGET :Y-OFFSET))
			       (- 5 (the fn (gvl :window :SCROLL-WIN-GADGET :Y-OFFSET)))
			       5))))
		 (:selection-function #'abort-function))

;;Access the :value slot to initialize the formula in the slot and establish dependencies (Garnet Manual p.300).
(gv *abort-button* :value)

(defvar *abort-buttons* (sequence-to-gen-array (loop for i from 1 to 50 collect (create-instance nil *abort-button*))))

(defun add-abort-button (win top-agg &key (label "ABORT"))
  ;; Adds a properly positioned ABORT button interactor to TOP-AGG. The width of the object is 43.
  (declare (optimize (safety 1) (speed 3) (space 0)))
  (let ((abort-button (retrieve-menu-thing *abort-buttons* *abort-button*)))
    (s-value abort-button :left (o-formula (- (the fn (if (gvl :window :SCROLL-WIN-GADGET)
							(gvl :window :SCROLL-WIN-GADGET :width)
							(gv win :width)))
					      (the fn (+ (the fn (if (gvl :window :SCROLL-WIN-GADGET) 20 10))
							 (the fn (gvl :width)))))))
    (s-value abort-button :white-field :filling-style opal:blue-fill)
    (s-value abort-button :string label)
    (gv abort-button :value)
    (s-value abort-button :value nil)
    (opal:add-component top-agg abort-button)))

(defun update-*abort-button* (win)
  (declare (optimize (safety 1) (speed 3) (space 0)))
  (s-value *abort-button* :left (- (gv win :width) 70))
  (s-value *abort-button* :parent nil)
  *abort-button*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(create-instance 'clear-or-select-all-button garnet-gadgets:radio-button
		 (:toggle-p t)
		 (:font (opal:get-standard-font :serif :bold-italic nil))
		 (:left (o-formula (- (the fn (if (gvl :window :SCROLL-WIN-GADGET)
						(gvl :window :SCROLL-WIN-GADGET :width)
						(gvl :window :width)))
				      (+ (if (gvl :window :SCROLL-WIN-GADGET) 20 0)
					 (the fn (gv :self :width))))))
		 (:top (o-formula (if (and (gvl :window :SCROLL-WIN-GADGET)
					   (gvl :window :SCROLL-WIN-GADGET :Y-OFFSET))
				    (- (- (the fn (gvl :window :height)) (gvl :top-offset))
				       (the fn (gvl :window :SCROLL-WIN-GADGET :Y-OFFSET)))
				    (- (the fn (gvl :window :height)) (gvl :top-offset))))))

(create-instance 'clear-all-button clear-or-select-all-button
		 (:string "Clear All")
		 (:top-offset 35)
		 (:selection-function #'(lambda (object value)
					  (declare (ignore value))
					  (s-value (gv object :panel) :value nil)
					  (s-value object :value nil)
					  (opal:update (gv object :window) t))))

(create-instance 'select-all-button clear-or-select-all-button
		 (:string "Select All")
		 (:top-offset 60)
		 (:selection-function #'(lambda (object value)
					  (declare (ignore value))
					  (s-value (gv object :panel) :value (remove "CANCEL" (gv object :panel :items)))
					  (s-value object :value nil)
					  (opal:update (gv object :window) t))))

(defun add-clear-all-button (win top-agg panel)
  ;; Adds a properly positioned clear-all button interactor to TOP-AGG.
  (let ((button (create-instance nil clear-all-button (:panel panel))))
    (s-value button :constant (remove ':white-field (gv garnet-gadgets:radio-button :constant)))
    (s-value button :white-field :filling-style opal:cyan-fill)
    ;; Access the :value slot to initialize the formula in the slot and establish dependencies (Garnet Manual p.300).
    (gv button :value)
    (opal:add-component top-agg button)))

(defun add-select-all-button (win top-agg panel)
  ;; Adds a properly positioned select-all button interactor to TOP-AGG.
  (let ((button (create-instance nil select-all-button (:panel panel))))
    (s-value button :constant (remove ':white-field (gv garnet-gadgets:radio-button :constant)))
    (s-value button :white-field :filling-style opal:cyan-fill)
    ;; Access the :value slot to initialize the formula in the slot and establish dependencies (Garnet Manual p.300).
    (gv button :value)
    (opal:add-component top-agg button)))

(defun add-ok-select-and-clear-all-buttons (win top-agg panel)
  (let ((ok-button (add-ok-button win top-agg)))
    ;; Adjust width for all the extra buttons on the right, considering that ADD-OK-BUTTON also adjusted the width.
    (s-value win :width (+ (gv win :width)
			   10		; A space between the left column of buttons and the right buttons.
			   (max (gv clear-all-button :width)
				(gv select-all-button :width)
				(gv ok-button :width))
			   (- (gv ok-button :width))))
    (s-value win :height (max (gv win :height)
			      (+ (gv ok-button :height)
				 20
				 (gv clear-all-button :height)
				 (gv select-all-button :height))))
    (add-select-all-button win top-agg panel)
    (add-clear-all-button win top-agg panel)))

(create-instance '*x-button-panel* garnet-gadgets:x-button-panel (:left 10) (:font (o-formula *menu-font*)) (:toggle-p t))
;; Access the :value slot to initialize the formula in the slot and establish dependencies (Garnet Manual p.300).
(gv *x-button-panel* :value)

(defmacro global-var-from-menu-list (global-var-menu-list) `(car ,global-var-menu-list))

(defun labeled-box-int-set-button (global-var-menu-list &optional (top 20))
  (let ((gadget (create-instance
		 nil (if *motif-gui-p* garnet-gadgets:motif-scrolling-labeled-box garnet-gadgets:labeled-box)
		 (:original-value  (format nil "~A" (round (symbol-value (global-var-from-menu-list global-var-menu-list)))))
		 (:variable (global-var-from-menu-list global-var-menu-list))
		 (:top top) (:left 10)	; (:constant t)
		 (:font (o-formula *menu-font*))
		 (:label-offset 10)
		 (:label-string (return-comment-string-or-var global-var-menu-list))
		 (:selection-function #'(lambda (labeled-box-object value)
					  (let ((input (read-from-string value)))
					    (cond ((numberp input)
						   (set (gv labeled-box-object :variable) (round input))
						   (s-value labeled-box-object :original-value (round (read-from-string (gv labeled-box-object :value)))))
						  (t (s-value labeled-box-object :value (gv labeled-box-object :original-value))))))))))
    (gv gadget :value) ; Access the :value slot to initialize the formula in the slot and establish dependencies (Garnet Manual p.300).
    (s-value gadget :value "")		; Reused gadgets will have parameters reflecting their last use, such as cursor positions. This seems to reset things.
    (s-value gadget :value (format nil "~A" (round (symbol-value (global-var-from-menu-list global-var-menu-list)))))
    (when *motif-gui-p* (s-value gadget :width (+ (gv gadget :label-text :width) 150)))
    gadget))

;; LG change 14.01.2017, menus now crashing if comment string too long...
#|
(defun return-comment-string-or-var (global-var-menu-list)
  ;; Given a parameter list for a menu entry, return the first string found in that list. If no string found, then return a stringified version of the
  ;;first element in the list, which should be a symbol corresponding to the global variable referenced by the list.
  (let ((global-var (global-var-from-menu-list global-var-menu-list))
	(comment-string (loop for component in global-var-menu-list when (stringp component) do (return component))))
    (if (documentation global-var 'variable)
	(format nil "~A~%~A" (string global-var) (or comment-string (documentation global-var 'variable)))
	(or comment-string (if (keywordp global-var) "" (string global-var))))))
|#
(defvar *max-menu-comment-length* 10)
(setq *max-menu-comment-length* 200)

(defun return-comment-string-or-var (global-var-menu-list &optional enforce-max-menu-comment-length ignore-documentation-string)
  ;; Given a parameter list for a menu entry, return the first string found in that list. If no string found, then return a stringified version of the
  ;;first element in the list, which should be a symbol corresponding to the global variable referenced by the list.
  (declare (ignore ignore-documentation-string))
  (let* ((global-var (global-var-from-menu-list global-var-menu-list))
	 (comment-string (loop for component in global-var-menu-list when (stringp component) do (return component)))
	 (final-complete-comment-string (if (documentation global-var 'variable)
					    (format nil "~A~%~A" (string global-var) (or comment-string (string-first-line (documentation global-var 'variable))))
					    (or comment-string
						(if (keywordp global-var)
						    ""
						    (string global-var))))))
    (if (and enforce-max-menu-comment-length (> (length final-complete-comment-string) *max-menu-comment-length*))
	(subseq final-complete-comment-string 0 *max-menu-comment-length*)
	final-complete-comment-string)))




(defvar *max-text-box-width* 100)
(setq *max-text-box-width* 200)

(defun labeled-menu-box-width (gadget original-value font)
  (+ (max 30 (opal::string-width (gv gadget :field-font) original-value))
     30
     (opal:string-width font (gv gadget :label-string))))

(defun labeled-box-num-or-symbol-or-string-list-set-button (var-list &optional (top 20))
  (labeled-box-symbol-set-button var-list top))

(defun labeled-box-text-set-button (global-var-menu-list &optional (top 20))
  (declare (optimize (safety 1) (speed 3) (space 0)))
  (let* ((variable-symbol (global-var-from-menu-list global-var-menu-list))
	 (original-value (if (symbol-value variable-symbol) (format nil "~a" (symbol-value variable-symbol)) ""))
	 (gadget (create-instance
		     nil (if *motif-gui-p* garnet-gadgets:motif-scrolling-labeled-box garnet-gadgets:scrolling-labeled-box)
		   (:original-value original-value)
		   (:variable variable-symbol)
		   (:top top)
					;		  (:left (cond ((member :label-left global-var-menu-list) (+ 20 (gv label :width)))
					;			       (t (gv prototype :left))))
		   (:left 10)
		   (:label-offset 10)
		   (:font (o-formula *menu-font*))
		   (:label-string (return-comment-string-or-var global-var-menu-list t t))
		   (:selection-function #'(lambda (scrolling-labeled-box-object value)
					    (set (gv scrolling-labeled-box-object :variable) value)
					    (s-value scrolling-labeled-box-object :original-value (gv scrolling-labeled-box-object :value)))))))
    (set variable-symbol original-value) ; Make sure that the var is set to a string.
    ;;Access the :value slot to initialize the formula in the slot and establish dependencies (Garnet Manual p.300).
    (gv gadget :value)
    (s-value gadget :value "")		; Reused gadgets will have parameters reflecting their last use, such as cursor positions. This seems to reset things.
    (s-value gadget :value original-value)
    (s-value gadget :width (labeled-menu-box-width gadget original-value *menu-font*))
    gadget))

(defun labeled-box-symbol-set-button (global-var-menu-list &optional (top 20))
  (declare (optimize (safety 1) (speed 3) (space 0)))
  (let* ((variable-symbol (global-var-from-menu-list global-var-menu-list))
	 (original-value (format nil "~s" (symbol-value variable-symbol)))
	 (gadget (create-instance
		  nil garnet-gadgets:scrolling-labeled-box
		  (:original-value original-value)
		  (:variable (global-var-from-menu-list global-var-menu-list))
		  (:top top) (:left 10) (:label-offset 10)
		  (:font (o-formula *menu-font*))
		  (:label-string (return-comment-string-or-var global-var-menu-list))
		  (:selection-function
		   #'(lambda (scrolling-labeled-box-object value)
		       (set (gv scrolling-labeled-box-object :variable) (read-from-string value))
		       (s-value scrolling-labeled-box-object :original-value
				(gv scrolling-labeled-box-object :value)))))))
    (set variable-symbol (read-from-string original-value)) ; Make sure that the var is set to a symbol.
    ;;Access the :value slot to initialize the formula in the slot and establish dependencies (Garnet Manual p.300).
    (gv gadget :value)
    (s-value gadget :value "")		; Reused gadgets will have parameters reflecting their last use, such as cursor positions. This seems to reset things.
    (s-value gadget :value original-value)
    (s-value gadget :width (labeled-menu-box-width gadget original-value *menu-font*))
    gadget))

(create-instance 'labeled-box-text-gadget
		 garnet-gadgets:scrolling-labeled-box
		 (:left 10) (:label-offset 10)
		 (:font (o-formula *menu-font*))
		 (:selection-function #'(lambda (scrolling-labeled-box-object value)
					  (set (gv scrolling-labeled-box-object :variable) value)
					  (s-value scrolling-labeled-box-object :original-value (gv scrolling-labeled-box-object :value)))))

(defvar *labeled-box-text-set-buttons* (sequence-to-gen-array (loop for i from 1 to 50 collect (create-instance nil labeled-box-text-gadget))))
(defvar *enable-precision-float-menus* nil)

(defun labeled-box-gadget-selection-function (labeled-box-object value)
  (let ((input (read-from-string value)))
    (cond ((numberp input)
	   (set (gv labeled-box-object :variable) (float input))
	   (s-value labeled-box-object :original-value (gv labeled-box-object :value)))
	  (t (s-value labeled-box-object :value (gv labeled-box-object :original-value))))))

(create-instance 'labeled-box-gadget gg:labeled-box (:left 10) (:label-offset 10) (:selection-function #'labeled-box-gadget-selection-function))

(create-instance 'motif-labeled-box-gadget gg::Motif-Scrolling-Labeled-Box
  (:font (o-formula *menu-font*))
  (:left 10) (:label-offset 10) (:selection-function #'labeled-box-gadget-selection-function))

(defvar *labeled-box-float-set-buttons* (sequence-to-gen-array (loop for i from 1 to 50 collect (create-instance nil labeled-box-gadget))))

(defvar *motif-labeled-box-float-set-buttons* (sequence-to-gen-array (loop for i from 1 to 50 collect (create-instance nil motif-labeled-box-gadget))))

(defvar *motif-labeled-box-float-field-width* 100) ; pixels

(defun labeled-box-float-set-button (global-var-menu-list &optional (top 20))
  (declare (optimize (safety 1) (speed 3) (space 0)))
  (let ((gadget (create-instance nil motif-labeled-box-gadget))
	(value (format nil "~f" (let ((car-value (symbol-value (global-var-from-menu-list global-var-menu-list))))
				  (s-flt (typecase car-value
					   (float car-value)
					   (t (float car-value))))))))
    (s-value gadget :top top)
    (s-value gadget :label-string (return-comment-string-or-var global-var-menu-list))
    (s-value gadget :original-value value)
    (s-value gadget :variable (global-var-from-menu-list global-var-menu-list))
    (when *motif-gui-p*
      (s-value gadget :width 0)(gv gadget :value)
      (s-value gadget :width (+ (gv gadget :label-text :width) *MOTIF-LABELED-BOX-FLOAT-FIELD-WIDTH*)))
    (gv gadget :value) ; Access the :value slot to initialize the formula in the slot and establish dependencies (Garnet Manual p.300).
    (s-value gadget :value value)
    gadget))

(defvar *menu-general-comment-line-right-margin* 20)
(defun menu-general-comment-line (global-var-menu-list &optional (top 20))
  (let ((text-gadget (create-instance nil opal:multifont-text (:top top)
				      (:left 10)
				      (:text-width (o-formula (if (gvl :parent :width)
								  (- (gvl :parent :width) *menu-general-comment-line-right-margin*)
								  250)
							      250))
				      (:word-wrap-p t)
				      (:constant nil) (:current-font *menu-comment-font*))))
    (opal:insert-text text-gadget (return-comment-string-or-var global-var-menu-list))
    text-gadget))

(defun menu-comment-line (global-var-menu-list &optional (top 20))
  (let ((pad-char #\:))
    (if (find #\newline (return-comment-string-or-var global-var-menu-list))
	(menu-general-comment-line global-var-menu-list top)
	(let ((text-gadget (create-instance nil opal:text (:top top)
					    (:left (o-formula
						    (round (/ (- (gvl :parent :window :width) (gvl :width)) 2))
						    0))
					    (:message (return-comment-string-or-var global-var-menu-list))
					    (:constant nil) (:font (o-formula *menu-comment-font*)))))
	  (s-value text-gadget :string (o-formula
					(if (gvl :parent :width)
					    (let ((num-dashes (1- (floor (/
									  (/ (- (gvl :parent :width) (opal:string-width (gvl :font) (gvl :message))) 2)
									  (opal:string-width (gvl :font) (string pad-char)))))))
					      (concatenate 'string
							   (REPEATED-CHARACTER-STRING  pad-char num-dashes)
							   (if (zerop (length (gvl :message))) "" (format nil " ~a " (gvl :message)))
							   (REPEATED-CHARACTER-STRING pad-char num-dashes)))
					    (gvl :message))))
	  text-gadget))))

(defun get-menu-button-label (label-string top left)
  (when (> (length label-string) 0)
    (create-instance nil opal:text (:visible t) (:top top) (:left left) (:string label-string)
		     (:font (o-formula *menu-button-font*)))))

(defun x-button-selection-function  (x-button-object value)
  (declare (ignore value))
  (set (gv x-button-object :variable) (gv x-button-object :selected))
  nil)

(create-instance 'x-boolean-set-button-gadget garnet-gadgets:x-button (:selection-function #'x-button-selection-function) (:left 10)
		 (:font (o-formula *menu-font*)))

(create-instance 'motif-check-boolean-set-button-gadget garnet-gadgets:motif-check-button
		 (:selection-function #'x-button-selection-function) (:left 10) (:button-width 18)
		 (:font (o-formula *menu-font*)))

(defvar *x-boolean-set-buttons* (sequence-to-gen-array (loop for i from 1 to 50 collect (create-instance nil x-boolean-set-button-gadget))))

(defvar *motif-check-boolean-set-buttons* (sequence-to-gen-array (loop for i from 1 to 50 collect (create-instance nil motif-check-boolean-set-button-gadget))))

(defun x-boolean-set-button (global-var-menu-list &optional (top 20))
  (declare (optimize (safety 1) (speed 3) (space 0)))
  (let ((gadget (if *motif-gui-p*
		    (create-instance nil motif-check-boolean-set-button-gadget) ;; (retrieve-menu-thing *motif-check-boolean-set-buttons* motif-check-boolean-set-button-gadget)
		    (create-instance nil x-boolean-set-button-gadget) ;; (retrieve-menu-thing *x-boolean-set-buttons* x-boolean-set-button-gadget)
		    )))
    (s-value gadget :variable (global-var-from-menu-list global-var-menu-list))
    (s-value gadget :top top)
    (s-value gadget :string (return-comment-string-or-var global-var-menu-list))
    ;; Access the :value slot to initialize the formula in the slot and establish dependencies (Garnet Manual p.300).
    (gv gadget :value)
    (s-value gadget :selected (symbol-value (global-var-from-menu-list global-var-menu-list)))
    gadget))

(defun FREE-arrayed-BUTTON (button)
  (s-value button :in-use nil)
  NIL)

;; `(function-name "Execute Function-name" :action)
(defun x-action-button (global-var-menu-list  &optional (top 20))
  (let ((gadget (create-instance
		 nil (if *motif-gui-p* garnet-gadgets:motif-check-button garnet-gadgets:x-button)
		 (:top top) (:left 10) (:font (o-formula *menu-font*))
		 ;; (when *motif-gui-p* (:button-width 18))
		 (:string (return-comment-string-or-var global-var-menu-list))
		 (:selection-function #'(lambda (x-button-object value)
					  (declare (ignore value))
					  (funcall (global-var-from-menu-list global-var-menu-list))
					  (set (gv x-button-object :selected) nil))))))
    ;; Access the :value slot to initialize the formula in the slot and establish dependencies (Garnet Manual p.300).
    (gv gadget :value)
    gadget))

;;;;;;;;;;;;;;;;;;;;;;;;;

(create-instance 'x-panel-choose-button garnet-gadgets:x-button-panel
		 (:font (o-formula *menu-font*)) (:left 10) (:direction :horizontal) (:rank-margin 3)
		 (:selection-function #'(lambda (x-button-object value) (set (gv x-button-object :variable) value))))

(create-instance 'motif-check-panel-choose-button garnet-gadgets:motif-check-button-panel
		 (:font (o-formula *menu-font*)) (:left 10) (:direction :horizontal) (:rank-margin 3)
		 (:selection-function #'(lambda (x-button-object value) (set (gv x-button-object :variable) value))))

(defun make-motif-check-panel-choose-button-agg ()
  (let ((agg (create-instance nil opal:aggregate))
	(label (create-instance nil opal:text (:visible t) (:string "") (:font (o-formula *menu-button-font*))))
	(gadget (create-instance nil motif-check-panel-choose-button (:button-width 18))))
    (s-value agg :gadget gadget)
    (s-value agg :label label)
    (opal:add-components agg gadget label)
    agg))

(defun make-x-panel-choose-button-agg ()
  (let ((agg (create-instance nil opal:aggregate))
	(label (create-instance nil opal:text (:visible t) (:string "")	(:font (o-formula *menu-button-font*))))
	(gadget (create-instance nil x-panel-choose-button)))
    (s-value agg :gadget gadget)
    (s-value agg :label label)
    (opal:add-components agg gadget label)
    agg))

(defun get-rank-margin (global-var-menu-list)
  (cond ((position :rank-margin global-var-menu-list)
	 (nth (1+ (position :rank-margin global-var-menu-list)) global-var-menu-list))
	((member :vertical global-var-menu-list) 3)
	(t (let* ((choose-position (or (position :choose global-var-menu-list) (position :x-choose global-var-menu-list)))
		  (num-items (length (nth (1+ choose-position) global-var-menu-list))))
	     (if (<= num-items 3) 3 (round (sqrt (* 2 (round (/ num-items 2))))))))))

(create-instance 'x-panel-choose-button-agg opal:aggregadget
		 (:top 20)
		 (:parts
		  `((:label ,opal:text
			    (:visible t)
			    (:top ,(o-formula (the fn (gvl :parent :top))))
			    (:string "")
			    (:font ,(o-formula *menu-button-font*)))
		    (:panel ,x-panel-choose-button
			    (:constant nil)
			    (:top ,(o-formula (+ (the fn (gvl :parent :label :height))
						 (the fn (gvl :parent :top))
						 5)))))))

(create-instance 'motif-check-panel-choose-button-agg opal:aggregadget
		 (:top 20)
		 (:parts
		  `((:label ,opal:text
		     (:visible t)
		     (:top ,(o-formula (the fn (gvl :parent :top))))
		     (:string "")
		     (:font ,(o-formula *menu-button-font*)))
		    (:panel ,motif-check-panel-choose-button
		     (:constant nil)
		     (:top ,(o-formula (+ (the fn (gvl :parent :label :height))
					  (the fn (gvl :parent :top))
					  5)))))))

(defvar *x-panel-choose-button-aggs* (sequence-to-gen-array (loop for i from 1 to 50 collect (create-instance nil x-panel-choose-button-agg))))
(defvar *motif-check-panel-choose-button-aggs* (sequence-to-gen-array (loop for i from 1 to 50 collect (create-instance nil motif-check-panel-choose-button-agg))))

(proclaim '(inline retrieve-x-panel-choose-button-agg))
(defun retrieve-x-panel-choose-button-agg ()
  (declare (optimize (safety 1) (speed 3) (space 0)))
  (loop for index from 0 to 49
	when (and (opal-obj-exists (aref (the (simple-array opal::schema (*)) *x-panel-choose-button-aggs*) index))
		  (not (gv (aref (the (simple-array opal::schema (*)) *x-panel-choose-button-aggs*) index) :in-use)))
	return (progn (s-value (aref (the (simple-array opal::schema (*)) *x-panel-choose-button-aggs*) index) :in-use t)
		      (aref (the (simple-array opal::schema (*)) *x-panel-choose-button-aggs*) index))
	finally (return (create-instance nil x-panel-choose-button-agg))))

(proclaim '(inline retrieve-motif-check-panel-choose-button-agg))
(defun retrieve-motif-check-panel-choose-button-agg ()
  (declare (optimize (safety 1) (speed 3) (space 0)))
  (loop for index from 0 to 49
	when
	(and (opal-obj-exists (aref (the (simple-array opal::schema (*)) *motif-check-panel-choose-button-aggs*) index))
	     (not (gv (aref (the (simple-array opal::schema (*)) *motif-check-panel-choose-button-aggs*) index) :in-use)))
	return
	(progn (s-value (aref (the (simple-array opal::schema (*)) *motif-check-panel-choose-button-aggs*) index) :in-use t)
	       (aref (the (simple-array opal::schema (*)) *motif-check-panel-choose-button-aggs*) index))
	finally (return (create-instance nil motif-check-panel-choose-button-agg))))

(defun x-panel-choose-button (global-var-menu-list &optional (top 20))
  ;; Operates on a list e.g. -
  ;;  `(*things-included "Include the following:" :x-choose ("First Thing" "Second Thing" "Nothing"))
  ;; old version
  (let* ((agg (create-instance nil opal:aggregate))
	 (label (get-menu-button-label (return-comment-string-or-var global-var-menu-list) top 10))
	 (prototype (if *motif-gui-p* garnet-gadgets:motif-check-button-panel garnet-gadgets:x-button-panel))
	 (gadget (create-instance nil prototype
				  (:fixed-width-p nil) (:h-spacing 20)
				  (:variable (global-var-from-menu-list global-var-menu-list))
				  (:items (nth (1+ (position :x-choose global-var-menu-list)) global-var-menu-list))
				  (:font (o-formula *menu-font*))
				  (:left (cond ((member :label-left global-var-menu-list) (+ 20 (gv label :width)))
					       (t (+ 10 (gv prototype :left)))))
				  (:direction :horizontal)
				  (:rank-margin (get-rank-margin global-var-menu-list))
				  (:selection-function #'(lambda (x-button-object value) (set (gv x-button-object :variable) value))))))
    (arrange-choose-button-label-and-gadget-top global-var-menu-list gadget label top)
    (opal:add-components agg label gadget)
    ;;Access the :value slot to initialize the formula and establish dependencies (Garnet Manual p.300).
    (gv gadget :value)
    (s-value gadget :value (no-nils (symbol-value (global-var-from-menu-list global-var-menu-list))))
    agg))

(defun motif-check-panel-choose-button (global-var-menu-list  &optional (top 20))
  (let* ((agg (create-instance nil opal:aggregate))
	 (label (get-menu-button-label (return-comment-string-or-var global-var-menu-list) top 10))
	 (prototype garnet-gadgets:motif-check-button-panel)
	 (gadget (create-instance nil prototype
				  (:button-width 18)
				  (:variable (global-var-from-menu-list global-var-menu-list))
				  (:items (nth (1+ (position :x-choose global-var-menu-list)) ; 3
					       global-var-menu-list))
				  (:font (o-formula *menu-font*))
				  (:left (cond ((member :label-left global-var-menu-list)
						(+ 20 (gv label :width)))
					       (t (gv prototype :left))))
				  ; (:left 10)
				  (:rank-margin (get-rank-margin global-var-menu-list))
				  (:top (+ (gv label :height) top 5)) (:direction :horizontal) (:rank-margin 3)
				  (:selection-function #'(lambda (x-button-object value) (set (gv x-button-object :variable) value))))))
    (opal:add-components agg label gadget)
    ;;Access the :value slot to initialize the formula and establish dependencies (Garnet Manual p.300).
    (gv gadget :value)
    (s-value gadget :value (no-nils (symbol-value (global-var-from-menu-list global-var-menu-list))))
    agg))

#|
(defun x-panel-choose-button (global-var-menu-list &optional (top 20))
  (declare (optimize (safety 0) (speed 3) (space 0)))
  (let ((agg (retrieve-x-panel-choose-button-agg)))
    (gv agg :panel :value)
    (gv agg :label :string)
    (s-value agg :label :string (nth 1 global-var-menu-list))
    (s-value agg :top top)
    (s-value agg :label :left 10)
    (s-value agg :panel :variable nil)
    (s-value agg :panel :items nil)
    (s-value agg :panel :button-width 18)
    (s-value agg :panel :button-height 18)
    ;;Access the :value slot to initialize the formula and establish dependencies (Garnet Manual p.300).
    (gv agg :panel :value)
    (s-value agg :panel :value (no-nils (symbol-value (global-var-from-menu-list global-var-menu-list))))
    (s-value agg :panel :variable (global-var-from-menu-list global-var-menu-list))
    (format t "items ~s~%" (nth 3 global-var-menu-list) )
    (loop for item in (gv agg :panel :items)
	  do (opal::remove-item (gv agg :panel) item))
    (loop for item in (nth 3 global-var-menu-list)
	  do (opal::add-item (gv agg :panel) item))
    (opal::notice-items-changed (gv agg :panel))
    (gv agg :panel :height)
    (gv agg :label :height)
    agg))
|#

(defun radio-choose-button-item-to-string-function (item)
  (REPLACE-UNDERSCORE-WITH-SPACE (string-capitalize (string-trim ":" (when (stringp item) item (string item))))))

(defun radio-button-selection-function (radio-button-object value) (set (gv radio-button-object :variable) value))

(create-instance 'radio-button-panel-gadget
		 garnet-gadgets:radio-button-panel
		 (:font (o-formula *menu-font*))
		 (:item-to-string-function #'radio-choose-button-item-to-string-function)
		 (:selection-function #'radio-button-selection-function)
		 (:left 10))

(create-instance 'motif-radio-button-panel-gadget
		 garnet-gadgets:motif-radio-button-panel
		 (:font (o-formula *menu-font*))
		 (:button-width 18)
		 (:item-to-string-function #'radio-choose-button-item-to-string-function)
		 (:selection-function #'radio-button-selection-function)
		 (:left 10))

(defun radio-choose-button (global-var-menu-list &optional (top 20))
  ;;
  ;; Operates on GLOBAL-VAR-MENU-LIST, e.g. -
  ;;
  ;;   `(*clamp-type* "Current or voltage clamp" :choose ("Current clamp" "Voltage clamp"))
  ;;
  ;; The order of these list elements is fixed. Subsequent keywords may be in any order. The value of the global variable and the
  ;; entries of the choices list *must* be strings or NIL.
  ;;
  ;; A keyword may be included to specify layout, ie :horizontal (default) or :vertical.
  ;;   `(*clamp-type* "Current or voltage clamp" :choose ("Current clamp" "Voltage clamp") :vertical)
  ;;
  ;; The default label position is on top of the radio buttons. If :LABEL-LEFT is included in the
  ;; list, then the label will be to the left of the radio buttons.
  ;;
  ;; The keyword :TOGGLE-P may be included to allow deselection of a selected button.
  ;;
  ;; Also, the rank margin may be specified in this list with the keyword :RANK-MARGIN followed by an integer.
  ;;
  ;; For example:
  ;;
  ;;   `(dummy1 "Lot o' choices" :choose ("A" "B" "C" "D" "E" "F") :rank-margin 2 :vertical :label-left)
  ;;
  (let* ((agg (create-instance nil opal:aggregate))
	 (label (get-menu-button-label (return-comment-string-or-var global-var-menu-list) top 10))
	 (prototype (if *motif-gui-p* motif-radio-button-panel-gadget radio-button-panel-gadget))
	 (gadget (create-instance
		  nil prototype
		  (:toggle-p (member :toggle-p global-var-menu-list))
		  (:fixed-width-p nil) (:h-spacing 20)
		  (:items (nth (1+ (position :choose global-var-menu-list)) global-var-menu-list))
		  (:variable (global-var-from-menu-list global-var-menu-list))
		  (:left (cond ((member :label-left global-var-menu-list) (+ 20 (gv label :width)))
			       (t (gv prototype :left))))
		  (:direction (if (member :vertical global-var-menu-list) :vertical :horizontal))
		  (:rank-margin (get-rank-margin global-var-menu-list)))))
    (arrange-choose-button-label-and-gadget-top global-var-menu-list gadget label top)
    (when label (opal:add-components agg label))
    (opal:add-components agg gadget)
    ;;Access the :value slot to initialize the formula and establish dependencies (Garnet Manual p.300).
    (gv gadget :value)
    (s-value gadget :value (symbol-value (global-var-from-menu-list global-var-menu-list)))
    agg))

(defun arrange-choose-button-label-and-gadget-top (global-var-menu-list gadget label top)
  (if (member :label-left global-var-menu-list)
      (let (thicker-one thinner-one)
	(if (> (gv gadget :height) (gv label :height))
	    (setq thicker-one gadget
		  thinner-one label)
	    (setq thicker-one label
		  thinner-one gadget))
	(s-value thicker-one :top top)
	(s-value thinner-one :top (round (+ top (/ (gv thicker-one :height) 2) (- (/ (gv thinner-one :height) 2))))))
      (s-value gadget :top (+ 5 (if label (gv label :height) 0) top))))

(defun color-option-button (global-var-menu-list &optional (top 20) (colors *basic-opal-colors*))
  (let* ((agg (create-instance nil opal:aggregate))
	 (label (get-menu-button-label (return-comment-string-or-var global-var-menu-list)
					; (nth 1 global-var-menu-list)
					top 10))
	 (gadget (create-instance
		  nil (if *motif-gui-p* garnet-gadgets:motif-option-button garnet-gadgets:option-button)
		  (:items colors)
		  (:variable (global-var-from-menu-list global-var-menu-list))
		  (:font (o-formula *menu-font*)) (:left 10) (:top (+ (if label (gv label :height) 0) top 5)))))
    (when label (opal:add-components agg label))
    (opal:add-components agg gadget)
    ;;Access the :value slot to initialize the formula and establish dependencies (Garnet Manual p.300).
    (gv gadget :value)
    (s-value gadget :value (symbol-value (global-var-from-menu-list global-var-menu-list)))
    agg))

(defun color-choose-button-menu (keys-and-their-colors label key-label
				 &key return-only-single-color (xclusive-choice t) (can-quit-w/o-all-keys-assigned t)
				 (all-values *basic-opal-colors-w-white*))
  (when (and (> (length keys-and-their-colors) 0) all-values)
    (let ((all-values (mapcar #'(lambda (color) (get-opal-color color)) all-values)))
      (loop while t do
	    (setq keys-and-their-colors
		  (loop for key-value in keys-and-their-colors collecting
			(typecase key-value
			  (cons (list (car key-value) (when (cadr key-value) (get-opal-color (cadr key-value)))))
			  (t (list key-value nil)))))
	    (let* ((button-labels (loop for value in all-values collect (kr::name-for-schema value)))
		   (key-label-font (opal:get-standard-font :serif :bold-italic :medium))
		   (height (+ 25 (gv key-label-font :font-height)))
		   (menu-entries
		    (loop for key-value in keys-and-their-colors collecting
			  (let ((gadget (create-instance
					 nil (if nil ; *motif-gui-p*
						 garnet-gadgets:motif-text-button-panel
						 garnet-gadgets:text-button-panel)
					 (:constant nil)
					 (:items button-labels) (:item-colors all-values)
					 (:key (format nil "~A" (car key-value))) (:font (o-formula *color-menu-button-font*))
					 (:top height) (:direction :horizontal)	; (:rank-margin 4)
					 (:toggle-p t)
					 (:selection-function (when xclusive-choice
								#'(lambda (object value)
								    (loop for comp in (gv object :parent :components)
									  when (and (not (eq comp object))
										    (equal value (gv comp :value)))
									  do (s-value comp :value nil))))))))
			    (setq height (+ height (+ *menu-entry-v-spacing (gv gadget :height))))
			    gadget)))
		   (win (get-regular-menu-window height 10 (GET-win-TITLE-STRING label)))
		   (top-agg (gv win :aggregate)))
	      (opal:add-component top-agg (create-instance nil opal:text (:visible t) (:top 10) (:string key-label) (:left 10) (:font key-label-font)))
	      (let ((text-width
		     (loop for menu-entry in menu-entries
			   do (opal:add-component top-agg menu-entry)
			   maximizing
			   (gv (opal:add-component top-agg (create-instance nil opal:text
									    (:visible t) (:top (gv menu-entry :top))
									    (:string (string (gv menu-entry :key)))
									    (:left 10) (:font key-label-font)))
			       :width))))
		(process-color-choose-menu-entries-and-color-keys menu-entries keys-and-their-colors text-width))
	      (s-value win :width (+ (gv (car menu-entries) :left) (gv (car menu-entries) :width) 80))
	      (add-ok-button win top-agg)
	      (s-value win :height (+ (gv win :aggregate :height) *menu-height-fudge*))
	      (resurrect-opal-win win :visible t)
					; (opal:update win t)
	      (inter:wait-interaction-complete win)
	      (setq keys-and-their-colors
		    (loop for menu-entry in menu-entries
			  for key-value in keys-and-their-colors
			  collect (list (car key-value) (string-to-opal-color (gv menu-entry :value)))))
	      (when (or can-quit-w/o-all-keys-assigned
			(loop for key-value in keys-and-their-colors thereis (cadr key-value)))
		(setq *menu-top* (max 10 (gv win :top)))
		(setq *menu-left* (max 10 (gv win :left)))
		(remove-and-free-buttons top-agg)
		(opal:destroy win)
		(return t))))
      (if return-only-single-color (cadar keys-and-their-colors) keys-and-their-colors))))

(defun process-color-choose-menu-entries-and-color-keys (menu-entries keys-and-their-colors text-width)
  (loop for menu-entry in menu-entries
	for key-value in keys-and-their-colors do
	(s-value menu-entry :left (+ 20 text-width))
	(let ((button-list (gv MENU-entry :text-button-list))
	      (button-color-list (gv MENU-entry :item-colors)))
	  (dotimes (n (length (gv button-list :items)))
	    (let ((button (nth n (gv button-list :components)))
		  (button-color (nth n button-color-list)))
	      (s-value (gv button :text) :line-style (create-instance nil opal:line-style (:foreground-color button-color)))
	      (s-value (gv button :shadow) :filling-style (create-instance nil opal:black-fill (:foreground-color button-color)))
	      (s-value (gv button :gray-outline) :filling-style (create-instance nil opal:gray-fill (:foreground-color button-color))))))
	(s-value (gv MENU-entry :final-feedback) :filling-style opal::gray-fill)
	(gv menu-entry :value)
	(s-value menu-entry :value (if (cadr key-value) (kr::name-for-schema (cadr key-value))))
	(gv menu-entry :selected)
	(s-value menu-entry :selected (if (cadr key-value) (kr::name-for-schema (cadr key-value))))))

(defun new-color-choose-button-menu (keys-and-their-colors label key-label
				     &key (xclusive-choice t) (can-quit-w/o-all-keys-assigned t) (all-values *basic-opal-colors*))
  (when (and (> (length keys-and-their-colors) 0) all-values)
    (loop while t do
	  (let* ((button-labels (loop for value in all-values collect (kr::name-for-schema value)))
		 (key-label-font (opal:get-standard-font :serif :bold-italic :medium))
		 (top (+ 25 (gv key-label-font :font-height)))
		 (menu-entries
		  (loop for key-value in keys-and-their-colors collecting
			(let ((gadget (create-instance
				       nil (if *motif-gui-p* garnet-gadgets:motif-text-button-panel garnet-gadgets:text-button-panel)
				       (:constant nil)
				       (:item-colors all-values)
				       (:key (string (car key-value))) (:font (o-formula *color-menu-button-font*))
				       (:top top) (:direction :horizontal) ; (:rank-margin 4)
                                       (:selection-function (when xclusive-choice #'(lambda (object value)
										      (loop for comp in (gv object :parent :components)
											    when (and (not (eq comp object)) (equal value (gv comp :value)))
											    do (s-value comp :value nil))))))))
			  (setq top (+ top (+ *menu-entry-v-spacing (gv gadget :height))))
			  gadget)))
		 (win (get-menu-window top label))
		 (top-agg (gv win :aggregate)))
	    (opal:add-component top-agg	(create-instance nil opal:text (:font key-label-font) (:visible t) (:top 10) (:left 10) (:string key-label)))
	    (let ((text-width (loop for menu-entry in menu-entries
				    do (opal:add-component top-agg menu-entry)
				    maximizing
				    (gv (opal:add-component top-agg (create-instance nil opal:text
										     (:visible t) (:top (gv menu-entry :top))
										     (:string (string (gv menu-entry :key)))
										     (:left 10) (:font key-label-font)))
					:width))))
	      (process-color-choose-menu-entries-and-color-keys menu-entries keys-and-their-colors text-width))
	    (s-value win :width (+ (gv (car menu-entries) :left) (gv (car menu-entries) :width) 80))
	    (add-ok-button win top-agg)
	    (opal:update win t)
	    (inter:wait-interaction-complete win)
	    (setq keys-and-their-colors
		  (loop for menu-entry in menu-entries collect (list (gv menu-entry :key) (string-to-opal-color (gv menu-entry :value)))))
	    (when (or can-quit-w/o-all-keys-assigned (loop for key-value in keys-and-their-colors thereis (cadr key-value)))
	      (setq *menu-top* (max 10 (gv win :top)))
	      (setq *menu-left* (max 10 (gv win :left)))
	      (remove-and-free-buttons top-agg)
	      (opal:destroy win)
	      (return t))))
    keys-and-their-colors))

(defun xclusive-choose-button (keys-and-their-values all-values &optional can-quit-w/o-all-keys-assigned &key (label "Choose Variables") text (rank-margin 3))
  (let ((extra-text ""))
    (loop while t do
	  (let* ((top 10)
		 (text (concatenate 'string text (when text (format nil "~%")) extra-text))
		 (menu-entries
		  (loop for key-value in keys-and-their-values
			collecting
			(let ((gadget (create-instance
				       nil
				       (if nil ; *motif-gui-p*
					   garnet-gadgets:motif-radio-button-panel
					   garnet-gadgets:radio-button-panel)
				       (:items all-values)
				       (:key (string (car key-value)))
				       ;; (:value (cadr key-value))
				       (:constant t) ;(:font  (o-formula *menu-font*))
				       (:left (o-formula (gvl :parent :max-text-width)))
				       (:top top) (:direction :horizontal) (:rank-margin rank-margin)
				       (:selection-function
					#'(lambda (object value)
					    (loop for comp in (gv object :parent :components)
						  when (and (not (eq comp object))
							    (equal value (gv comp :value)))
						  do (s-value comp :value nil)))))))
			  (setq top (+ top (+ *menu-entry-v-spacing (gv gadget :height))))
			  (gv gadget :value)
			  (s-value gadget :value (cadr key-value))
			  gadget)))
		 (win (create-instance nil *menu-win*
				       (:visible t)
				       (:title (GET-win-TITLE-STRING label))
				       (:icon-title "SH Menu")
				       (:aggregate (create-instance nil opal:aggregate))))
		 (top-agg (gv win :aggregate)))
	    (when (and *motif-gui-p* *use-motif-gui-background*) (opal:add-component (gv win :aggregate) (create-instance nil gg:motif-background)))
	    (s-value win :aggregate :window win)
	    (create-instance NIL destroy-window-Interactor (:Window win))
	    (loop for menu-entry in menu-entries
		  do (opal:add-component top-agg menu-entry)
		  maximize
		  (gv
		   (opal:add-component top-agg
				       (create-instance nil opal:text
							(:visible t) (:top (gv menu-entry :top))
							(:string (string
								  (if nil ; *motif-gui-p*
								      (gv menu-entry :value)
								      (gv menu-entry :key))))
							(:left 10)
							(:font (opal:get-standard-font :serif :bold-italic :large))))
		   :width)
		  into max-text-width
		  do (s-value win :height (+ (gv menu-entry :top) (gv menu-entry :height)))
		  finally (s-value top-agg :max-text-width (+ 15 max-text-width)))
	    (s-value win :width (+ (gv top-agg :max-text-width) (gv (car menu-entries) :width) 80))
	    (s-value win :height (+ (gv win :height) 10))
	    (when text (add-menu-text text top-agg win))
	    (add-ok-button win top-agg)
	    (opal:update win t)
	    (inter:wait-interaction-complete win)

	    ;; Kludge since the menus tend to walk over to the left
	    (setq *menu-top* (max 10 (gv win :top))
		  *menu-left* (max 10 (gv win :left)))
	    (setq keys-and-their-values
		  (loop for menu-entry in menu-entries collect
			(list (gv menu-entry :key)
			      (gv menu-entry :value))))
	    (remove-and-free-buttons top-agg)
	    (opal:destroy win))
	  when (or can-quit-w/o-all-keys-assigned
		   (if (loop for key-value in keys-and-their-values thereis (not (cadr key-value)))
		       (progn (setq extra-text "All entries must be assigned") nil)
		       t))
	  do (return t)))
  keys-and-their-values)

(defun reorder-value-list-from-keys-menu (key-value-list &key (label "Choose Ordering of List Variables") unassigned-values text (ok-to-leave-out-values t))
  (loop for key in
	(reorder-list-menu-core (loop for key-value in key-value-list collect (car key-value))
				:unassigned-values unassigned-values :label label :text text :ok-to-leave-out-values ok-to-leave-out-values)
	collect (cadr (find key key-value-list :key 'car :test 'equal))))

(defun reorder-list-menu (list &key (label "Choose Ordering of List Variables") unassigned-values text (ok-to-leave-out-values t) use-list-values-as-keys)
  (if nil ; use-list-values-as-keys
      (let ((key-value-list (loop for val in list collect (list val val))))
	(loop for key in
	      (reorder-list-menu-core (loop for key-value in key-value-list collect (car key-value))
				      :unassigned-values unassigned-values :label label :text text :ok-to-leave-out-values ok-to-leave-out-values)
	      collect (cadr (find key key-value-list :key 'car :test 'equal))))
      (reorder-list-menu-core list :label label :unassigned-values unassigned-values :text text :ok-to-leave-out-values ok-to-leave-out-values)))

(defun reorder-list-menu-core (list &key (label "Choose Ordering of List Variables") unassigned-values text (ok-to-leave-out-values t))
  (unless text
    (when ok-to-leave-out-values (setq text "Unselected items will be left out of new list")))
  (let ((order 0))
    (loop for val in list
	  for count from 1
	  collect (list val (if (member val unassigned-values :test 'equal) "" (format nil "~D" (setq order (1+ order)))))
	  into key-values
	  collect (format nil "~D" count) into all-values
	  finally (return
		   (loop for sorted-key-val in
			 (sort (loop for key-val in (xclusive-choose-button key-values all-values ok-to-leave-out-values :label label :text text)
				     when (and (cadr key-val) (> (length (cadr key-val)) 0))
				     collect (list (car key-val) (read-from-string (cadr key-val))))
			       '< :key 'cadr)
			 collect (car sorted-key-val))))))


(defun labeled-box-thing-set-button (var-list &optional (top 20))
  (let ((value (symbol-value (car var-list))))
    (cond ((T-OR-NIL-P value) (x-boolean-set-button var-list top))
	  ((NUM-OR-SYMBOL-OR-STRING-LIST-P value) (labeled-box-num-or-symbol-or-string-list-set-button var-list top))
	  (t (typecase value
	       (symbol (labeled-box-symbol-set-button var-list top))
	       ((or float number) (labeled-box-float-set-button var-list top))
					;	(float (labeled-box-float-set-button var-list top))
	       (string (labeled-box-text-set-button var-list top))
	       (fixnum (labeled-box-int-set-button var-list top)))))))

(defvar *menu-text-gadgets* (sequence-to-gen-array (loop for i from 1 to 50 collect (create-instance nil opal:multifont-text))))

(defun add-menu-text (text top-agg win)
  ;; Add multifont-text TEXT at bottom of WIN, adding TEXT to the WIN aggregate TOP-AGG. The height of WIN is adjusted
  ;; to account for the height of the new text object, and the width of WIN is set to the larger of the original width
  ;; and the width of the text object. Returns the height of the text object.
  (declare (optimize (safety 1) (speed 3) (space 0)))
  (let ((text-gadget (retrieve-menu-thing *menu-text-gadgets* opal:multifont-text)))
    (s-value text-gadget :current-font *menu-text-font*)
    (s-value text-gadget :left 5)
    (s-value text-gadget :word-wrap-p nil)
    ;; If text starts with a newline, we have to add a space at the beginning - otherwise the result of opal::set-text chokes with
    ;;
    ;; OPAL::FIX-UPDATE-SLOTS-METHOD-MULTIFONT-TEXT
    ;;
    ;;      Type-error in KERNEL::OBJECT-NOT-TYPE-ERROR-HANDLER:
    ;;          NIL is not of type OPAL::FRAG
    ;;
    (when (equal (schar text 0) #\newline) (setq text (concatenate-strings " " text)))
    (opal::set-text text-gadget (list (list (cons text (gv text-gadget :current-font)))))
    (s-value text-gadget :text-width (if (and (> (gv text-gadget :width) (gv win :width))
					      (< (gv win :width) 150))
					 150
					 (gv win :width)))
    (s-value text-gadget :top (gv win :height))
    (opal:add-component top-agg text-gadget)
    (s-value win :height (the fn (+ (gv win :height) (gv text-gadget :height))))
    (s-value win :width (max (gv win :width) (+ 15 (gv text-gadget :width))))
    (gv text-gadget :height)))

(defun get-menu-entry (var-list top)
  (cond
    ;; ((my-member-equal :color var-list) (color-option-button var-list top))
   ((my-member-equal :num-or-symbol-or-string-list var-list) (labeled-box-num-or-symbol-or-string-list-set-button var-list top))
   ((my-member-equal :thing var-list) (labeled-box-thing-set-button var-list top))
   ((my-member-equal :string var-list) (labeled-box-text-set-button var-list top))
   ((my-member-equal :action var-list) (x-action-button var-list top))
   ((my-member-equal :boolean var-list) (x-boolean-set-button var-list top))
   ((my-member-equal :symbol var-list) (labeled-box-symbol-set-button var-list top))
   ((my-member-equal :choose var-list) (radio-choose-button var-list top))
   ((my-member-equal :x-choose var-list) (x-panel-choose-button var-list top))
   ((or (my-member-equal :float var-list) (my-member-equal :number var-list)) (labeled-box-float-set-button var-list top))
   ((or (my-member-equal :fixnum var-list) (my-member-equal :integer var-list)) (labeled-box-int-set-button var-list top))
   ((my-member-equal :general-comment var-list) (menu-general-comment-line var-list top))
   ((my-member-equal :comment var-list) (menu-comment-line var-list top))))

(defun vertical-space-for-menu-entry (menu-entry)
  (the fn (+ (the fn *menu-entry-v-spacing)
	     (the fn (max (gv menu-entry :height)
			  (the fn (if (gv menu-entry :label-text)
				      (gv menu-entry :label-text :height)
				      0)))))))

#|


(eval (let* ((g (gensym)))
  `(progn ,(list `defvar g 10)
    (choose-variable-values (quote ((,g "This is nuthin" :number)))) ,g)))

(defmacro choose-variable-menu (var-lists &key (label "Choose Variables") title text (max-height (* 0.9 *screen-height*)) image (image-top :bottom) (image-left :right))
  (loop for var-list in (if (eq 'quote (car var-lists)) (cadr var-lists) var-lists) ; So it will accept quoted (as before) or non-quoted lists (as is correct)
	collect (gensym) into gensym-locals
	collect (car var-list) into locals
	collect (cdr var-list) into var-list-cdrs
	finally (return
		  (loop for gensym-local in gensym-locals
			for local in locals
			unless (boundp local) collect `(defvar ,gensym-local ,local) into defvars and collect `(setq ,local ,gensym-local) into setqs
			finally
			(return
			  `(progn
			    ,@defvars
			    (choose-variable-values ; -internal
			     (quote ,(mapcar #'(lambda (gensym-local local var-list-cdr) (cons (if (boundp local) local gensym-local) var-list-cdr))
					     gensym-locals locals var-list-cdrs))
			     :label ,label :title ,title :text ,text :max-height ,max-height :image ,image :image-top ,image-top :image-left ,image-left
			     )
			    ,@setqs
			    nil))))))


(export '(VARIABLE-MENU))

(defun get-scrolling-menu-window (top width label title image image-left image-top max-height)
  (let ((swin (create-instance nil (if *motif-gui-p* *motif-ss-win-w-bars* *ss-win-w-bars*)
			       (:visible t) (:width width) (:height max-height) (:title (or title label)) (:icon-title "SH Menu")
			       (:total-height top) (:total-width (o-formula (gv :self :width))))))
    (when (and *motif-gui-p* *use-motif-gui-background*) (opal:add-component (gv swin :inner-aggregate) (create-instance nil gg:motif-background)))
    (when image (show-image image :win swin :image-left image-left :image-top image-top :update nil))
    (fix-window-size swin)
    (opal:update swin t)
    (create-instance NIL destroy-window-Interactor (:Window (gv swin :outer-window)))))

(defun choose-variable-values-internal (var-lists &key (label "Choose Variables") title text (max-height (* 0.9 *screen-height*)) image (image-top :bottom) (image-left :right))
  (when (and (not *automatic-run*) var-lists)
    (let* ((max-height (round max-height))
	   (var-lists (no-nils var-lists))
	   (top 10) (width 0)
	   (label-length (length label)))
      (multiple-value-bind (menu-entries top width) ;; Create menu entries here because srolling windows are handled a bit differently than interactor-windows
	  (choose-variable-values-menu-entries var-lists top width)
	(setq width (max (the fn (+ width *menu-width-fudge* (gv (ok-button) :width))) ; Allow some room for the OK button.
			 (the fn (+ (* *window-manager-char-width* (the (UNSIGNED-BYTE 32) label-length))
				    ;; 2 mouseable boxes in window title bar
				    (+ *window-manager-mouse-box-width* *window-manager-mouse-box-width*)))))
	(multiple-value-bind (win top-agg)
	    (if (> top max-height)
		(let ((win (get-scrolling-menu-window top width label title image image-left image-top max-height)))
		  (values win (gv win :inner-aggregate)))
		(let ((win (get-regular-menu-window top width label title)))
		  (values win (gv win :aggregate))))
	  (mapcar #'(lambda (menu-entry) (opal:add-component top-agg menu-entry)) menu-entries)
	  (let ((menu-text-height (when text (add-menu-text text top-agg win))))
	    (when image (show-image image :win win :image-left image-left :image-top image-top :update nil :VERTICAL-SIDE-BORDER (+ (or menu-text-height 0)))))
	  (mapcar #'(lambda (comp)
		      (kr::recompute-formula comp :left)
		      ;; Make sure to center all :comment lines. Don't know why this is not handled automatically by the :left o-formulae!
		      (kr::recompute-formula comp :text-width))
		  (gv top-agg :components))
	  (add-ok-button win top-agg)
	  (fix-window-size win)
	  (resurrect-opal-win win :raise t :visible t :show t :update t :deiconify t)
	  (menu-wrap-up win top-agg))))))

|#

;;; This partly mimics the Symbolics function of the same name. When
;;; the OK button is pressed, this function returns NIL. VAR-LISTS is
;;; a single list or a list of lists, each being a variable setting
;;; parameter lists (VAR-LIST) with the following minimum format:
;;;
;;;      (GLOBAL-VARIABLE TYPE-KEYWORD)
;;;
;;; NIL entries in VAR-LISTS will be ignored. If there is a string in
;;; a VAR-LIST,
;;;
;;;      (GLOBAL-VARIABLE "Comment string" TYPE-KEYWORD)
;;;
;;; then this string will appear as a comment in the menu
;;; line. Otherwise, the GLOBAL-VARIABLE symbol will be printed.
;;;
;;; The TYPE-KEYWORD can be :NUMBER :FLOAT :INTEGER :FIXNUM :COMMENT
;;; :BOOLEAN :CHOOSE :X-CHOOSE :STRING :SYMBOL
;;;
;;; If TYPE-KEYWORD = :STRING,
;;;     then the GLOBAL-VARIABLE will be reinitialized to its print-name string.
;;;
;;; If TYPE-KEYWORD = :SYMBOL,
;;;     then the GLOBAL-VARIABLE will be reinitialized to a symbol based on its print-name.
;;;
;;; :COMMENT just takes a single comment string.
;;;
;;; If TYPE-KEYWORD = :CHOOSE or :X-CHOOSE, then the variable setting
;;; parameter list format is:
;;;
;;;     (GLOBAL-VARIABLE "Comment string" :CHOOSE (first-choice second-choice ...))
;;;
;;; where FIRST-CHOICE, SECOND-CHOICE, etc. may be strings or keyword
;;; symbols. :X-CHOOSE allows multiple selections from a list, while
;;; :CHOOSE allows only one chosen value. The keyword :TOGGLE-P
;;; allows selected button(s) to be deselected by repressing that
;;; button.
;;;
;;; The GLOBAL-VARIABLE symbol *must* be a variable that has been
;;; declared special, that is a global variable. If necessary, there
;;; are a set of pre-defined DUMMYx variables [x from 0 to 100] that
;;; may be used.  For example, a temporary value in a function may be
;;; passed to a menu by localling binding a DUMMYx variable
;;; [e.g. with LET], and then using the symbol in a variable setting
;;; parameter list.
;;;
;;; The GLOBAL-VARIABLE symbol may also be declared locally special,
;;; thus (declare (special foo)).
;;;
;;; For example, VAR-LISTS could be:
;;;
;;; '((*user-stop-time* "Length of simulation [ms]" :number)
;;;   (*modify-plot-parameters "Modify plot parameters" :boolean)
;;;   (*traces-per-plot* :integer)
;;;   (dummy12 "Plot upside down" :boolean)
;;;   (dummy4 "Current or voltage clamp" :choose ("Current clamp" "Voltage clamp"))
;;;
;;; Note that literal lists (constants) may be used as the VAR-LISTS
;;; argument, since we don't try to modify it.
(defun choose-variable-values
    (var-lists
     &key
       (label "Choose Variables")
       title
       text
       (max-height (* 0.9 gem::*screen-height*))
       image
       (image-top :bottom)
       (image-left :right))
  (when (and (not *automatic-run*) var-lists)
    (let* ((max-height (round max-height))
	   (var-lists (no-nils var-lists))
	   (top 10) (width 0) win top-agg
	   (label-length (the (UNSIGNED-BYTE 32) (length label))))
      ;; Create menu entries here because srolling windows are handled
      ;; a bit differently than interactor-windows
      (multiple-value-bind (menu-entries top width)
	  (choose-variable-values-menu-entries var-lists top width)
	;; (declare (fixnum top width max-height)) Allow some room for
	;; the OK button.
	(setq width (max (+ width
			    *menu-width-fudge*
			    (gv (ok-button) :width))
			 (+ (* *window-manager-char-width*
			       label-length)
			    ;; 2 mouseable boxes in window title bar
			    (+ *window-manager-mouse-box-width*
			       *window-manager-mouse-box-width*))))
	(setq win
	      (cond
		((> top max-height)
		 (let ((swin
			(create-instance
			    nil
			    (if *motif-gui-p* *motif-ss-win-w-bars*
				*ss-win-w-bars*)
			  (:visible t)
			  (:width width)
			  (:height max-height)
			  (:title (or title label))
			  (:icon-title "SH Menu")
			  (:background-color (when *motif-gui-p*
					       OPAL:MOTIF-green))
			  (:total-height top)
			  (:total-width (o-formula (gv :self :width))))))
		   (opal::update swin)
		   (when (and *motif-gui-p*
			      *use-motif-gui-background*)
		     (opal:add-component
		      (gv swin :inner-aggregate)
		      (create-instance nil gg:motif-background)))
		   (when image
		     (show-image image
				 :win swin
				 :image-left image-left
				 :image-top image-top
				 :update nil))
		   (fix-window-size swin)
		   (opal:update swin t)
		   (setq top-agg (gv swin :inner-aggregate))
		   (create-instance NIL destroy-window-Interactor
		     (:Window (gv swin :outer-window)))
		   swin))
		(t
		 (let ((win (get-regular-menu-window top width label title)))
		   (setq top-agg (gv win :aggregate))
		   win))))
	(mapcar #'(lambda (menu-entry)
		    (opal:add-component top-agg menu-entry))
		menu-entries)
	(let ((menu-text-height (when text (add-menu-text text top-agg win))))
	  (when image
	    (opal:update win t)
	    (show-image image
			:win win
			:image-left image-left
			:image-top image-top
			:update nil
			:VERTICAL-SIDE-BORDER (+ (or menu-text-height 0)
						 -15))))
	(loop for comp in (gv top-agg :components) do
	     (kr::recompute-formula comp :left)
	   ;; Make sure to center all :comment lines. Don't know why
	   ;; this is not handled automatically by the :left
	   ;; o-formulae!
	     (kr::recompute-formula comp :text-width))
	(add-ok-button win top-agg)
	(fix-window-size win)
	(resurrect-opal-win win :raise t :visible t :show t :update t :deiconify t)
	(menu-wrap-up win top-agg)))))

(defun choose-variable-values-menu-entries (var-lists top width)
  (let ((menu-entries
	 (loop for var-list in (no-nils (if (consp (car var-lists)) var-lists (list var-lists)))
	    collect (let ((menu-entry (get-menu-entry var-list top)))
		      (setq top (the fn (+ top (vertical-space-for-menu-entry menu-entry)))
			    width (max width (gv menu-entry :width)))
		      menu-entry))))
    (values menu-entries top width)))

(defun remove-and-free-buttons (top-agg)
  (declare (optimize (safety 1) (speed 3) (space 0)))
  (loop for comp in (gv top-agg :components) when (gv comp :in-use)
	do (FREE-arrayed-BUTTON comp) (opal:remove-component top-agg comp))
  nil)

(defun menu-wrap-up (win top-agg)
  (when (opal-obj-exists win)
    (s-value win :menu-p t)		; overkill
    (inter:wait-interaction-complete (or (gv win :window) win))
    ;; kludge since the menus tend to walk over to the left
    (setq *menu-top* (max 10 (gv win :top))
	  *menu-left* (max 10 (gv win :left))))
  (when (opal-obj-exists top-agg) (remove-and-free-buttons top-agg))
  (when (opal-obj-exists win) (opal:destroy win))
  nil)

(create-instance
    'edit-text opal:aggregadget
  (:string "")
  (:parts
   `((:label ,opal:cursor-multi-text ; opal:multifont-text
	     (:string ,(o-formula (gvl :parent :string)))
	     (:left ,(o-formula (the fn (gvl :parent :left))))
	     (:top ,(o-formula (the fn (gvl :parent :top))))
	     (:font ,(o-formula (or (gvl :parent :font)
				    (gvl :window :plot-axis-font)
				    (gvl :window :font)
				    (opal:get-standard-font :serif :bold-italic :large)))))))
  (:interactors
   `((:text-inter ,inter:text-interactor
		  (:window ,(o-formula (gv-local :self :operates-on :window)))
		  (:feedback-obj nil)
		  (:start-where ,(o-formula (list :in (gvl :operates-on :label))))
		  (:abort-event :CONTROL-\g)
		  (:stop-event (:leftdown))))))

(defun simple-text-menu (label &optional title (string "") (height 400) (width 400) (comment "Edit stuff above"))
  (let* ((win (get-regular-menu-window height width label title))
	 (top-agg (gv win :aggregate))
	 (axis-text (create-instance nil edit-text (:visible t) (:string string) (:top 5) (:left 10)))
	 output)
    (opal::add-component top-agg axis-text)
    (add-ok-button win top-agg)
    (add-menu-text comment top-agg win)
    (resurrect-opal-win win :visible t)
    (inter:wait-interaction-complete win)
    (setq output (gv axis-text :label :string))
    (remove-and-free-buttons top-agg)
    (opal:destroy win)
    output))

(defun reorder-list-menu (list &optional label-list (title "Reorder List"))
  (let ((labels (loop for count from 1 to (length list)
		      collect (or (nth (1- count) label-list) (format nil "List element ~A" count)))))
    (loop for count from 1
	  for label in labels
	  collect (list label (format nil "~D" count)) into menu-list
	  collect (format nil "~D" count) into all-values
	  finally
	  (return (let ((new-order (mapcar (lambda (string-val) (when (stringp (cadr string-val)) (read-from-string (cadr string-val))))
					   (XCLUSIVE-CHOOSE-BUTTON menu-list all-values t :rank-margin 6 :label title))))
		    (loop for position from 1 to (length list) when (position position new-order) collect (nth (position position new-order) list)))))))

(defun edit-string-list (list &key (label "Edit Strings") (entry-string "Entry") text)
  ;; Given a LIST of strings, returns a list with the same number of strings, possibly edited.
  (let ((syms (LIST-OF-DUMMYs list)))
    (choose-variable-values
     (loop for sym in syms
	   for count from 1
	   collect (list sym (string-capitalize (format nil "~:R ~a" count entry-string)) :string))
     :text text :label label)
    (mapcar 'symbol-value syms)))

(defun edit-list (list &key (label "List Menu") entry-strings text)
  (let ((entry-strings (loop for thing in list
			     for count from 0
			     collect (if (consp entry-strings)
				       (nth count entry-strings)
				       (string-capitalize (format nil "~:R ~a" (1+ count) (or entry-strings "Entry"))))))
	(menu-vars (generate-and-set-special-vars list)))
    (choose-variable-values
     (loop for thing in menu-vars
	   for string in entry-strings
	   for i from 1
	   collect (list thing string (get-menu-entry-type-keyword thing)))
     :text text :label label)
    (mapcar 'symbol-value menu-vars)))

(defun function-arg-menu-text (extra-text doc-string only-keywords)
  (concatenate 'string
	       extra-text
	       (when (and extra-text doc-string) (format nil "~%"))
	       doc-string
	       (when doc-string (format nil "~%~%"))
	       (when only-keywords (format nil "~%Only function keyword values appear here.~%~%"))
	       (limit-string-span
		"Note that entries which appear to be boolean (and set to NIL) may in fact be
either args w/o a default value or args that do not appear in the current values."
		70)
	       ;; (format nil "~% ** This edit is not 100% tested ** Verify the results independently **")
	       ))

(defun extract-function-from-atom (thing)
  (cond ((functionp thing) thing)
	((and (symbolp thing) (fboundp thing)) (fdefinition thing))
	((stringp thing)
	 (let ((thing (read-from-string thing)))
	   (when (and (symbolp thing) (fboundp thing)) (fdefinition thing))))
	(t nil)))

(defun extract-function (funspec) (extract-function-from-atom (if (consp funspec) (car funspec) funspec)))

(defvar *suppress-lambda-form-compiler-messages* t "When compiling unamed functions (lambda forms) that appear in TYPE-DEFs, suppress all compiler messages.")

(defun compile-or-extract-function-or-number (function-candidate &optional car-is-not-function)
  ;; This is able to parse a simple list whose car is 'LAMBDA and
  ;; compile the function, thus avoiding the #'(lambda (arg)...)
  ;; syntax, although this format is parsed too. If FUNCTION-CANDIDATE
  ;; is a number just return that value.
  (let* ((*compile-print* (not *suppress-lambda-form-compiler-messages*))
	 (*compile-verbose* (not *suppress-lambda-form-compiler-messages*))
	 (result
	  (typecase function-candidate
	    (function (compile nil function-candidate))
	    (number function-candidate)
	    (cons
	     (if (and (not car-is-not-function) (eq 'function (car function-candidate))) ; This handles the #'(LAMBDA ...) syntax
		 (compile-or-extract-function-or-number-car-is-not-function (cadr function-candidate))
		 (if (eq 'lambda (car function-candidate)) ; This handles the (LAMBDA ..) syntax
		     (if (not (consp (nth 1 function-candidate)))
			 (sim-error (format nil "~A is not a correct LAMBDA form" function-candidate))
			 (compile nil function-candidate))
		     function-candidate)))
	    (t (extract-function-from-atom function-candidate)))))
    result))

;; Just do this so that we can trace COMPILE-OR-EXTRACT-FUNCTION-OR-NUMBER.
(defun compile-or-extract-function-or-number-car-is-not-function (function-candidate) (compile-or-extract-function-or-number function-candidate t))

(defun get-menu-entry-type-keyword (thing)
  (let ((value (if (and (symbolp thing)
			(boundp thing))
		   (symbol-value thing)
		   thing)))
    (cond ((t-or-nil-p value) :boolean)
	  ((NUM-OR-SYMBOL-OR-STRING-LIST-P value) :thing)
	  (t (typecase value
	       (symbol :symbol)
	       (string :string)
	       (fixnum :integer)
	       (number :float)
	       (float :float)
	       (t :boolean))))))

(defun function-type (function-name)
  (type-of function-name))

(export 'function-type)

(defun extract-all-args (arglist &optional current-argvs)
  ;; Parse the current args according to the function description.
  (let ((nothing-found t) key-found optional-found)
    (loop for arg-descriptor in arglist
	  ;; E.G.  "(tau-rise tau-fall &key (amplitude 1.0) normalize (step 1.0) length (offset 0.0))"
	  unless (case arg-descriptor	; SIGNAL LAMBDA LIST KEYWORDS
		   (&optional (setq optional-found t nothing-found nil) t)
		   (&key (setq key-found t optional-found nil nothing-found nil) t)
		   (LAMBDA-LIST-KEYWORDS (setq key-found nil optional-found nil nothing-found nil) t))

	  when nothing-found
	  collect (list arg-descriptor (car current-argvs)) into reg-args and do (setq current-argvs (cdr current-argvs))

	  else when optional-found
	  collect (list (if (consp arg-descriptor) (car arg-descriptor) arg-descriptor) ; Get the symbol-var
			(or (car current-argvs)
			    (when (consp arg-descriptor) (cadr arg-descriptor)))) ; Default value
	  into optional-args and do (setq current-argvs (cdr current-argvs))

	  else when key-found collect
	  (list (if (consp arg-descriptor) (car arg-descriptor) arg-descriptor) ; Get the symbol-var
		(when (consp arg-descriptor) (cadr arg-descriptor)))
	  into default-key-args

	  finally (return
		    (values reg-args
			    optional-args
			    (delete-duplicates
			     (concatenate 'list default-key-args
					  (let ((out '()))
					    (do ((current-argvs current-argvs (cddr current-argvs)))
						((null current-argvs) (reverse out))
					      (push (list (read-from-string (format nil "~a" (car current-argvs)))
							  (cadr current-argvs))
						    out))))
			     :key 'car))))))

(defun extract-funspec-args (funspec &optional current-argvs return-as-single-list)
  ;; not implemented
  (error "this function hasn't been implemented yet"))



;; old CMUCL only
;;
;; (defun extract-funspec-args (funspec &optional current-argvs return-as-single-list)
;; Returns values (reg-args optional-args key-args)
;; (let ((current-argvs (or current-argvs (when (consp funspec) (cdr funspec))))
;; 	(function (extract-function funspec)))
;;   (unless (or (not function) (eval:interpreted-function-p function))
;;     (multiple-value-bind (reg-args optional-args key-args)
;; 	  (extract-all-args (read-from-string (lisp::%function-arglist function)) current-argvs)
;; 	(if return-as-single-list
;; 	  (no-nils (concatenate 'list reg-args (concatenate 'list optional-args key-args)))
;; 	  (values reg-args optional-args key-args))))))

#|
;; Doesn't work
(defun arglist (name)
  ;; Taken from LISP faq-doc-14.txt
  (let* ((function (symbol-function name))
	 (stype (system:%primitive get-vector-subtype function)))
    (when (eql stype system:%function-entry-subtype)
      (cadr (system:%primitive header-ref function system:%function-entry-type-slot)))))
|#

;; not implemented
(defun function-required-args (funspec)
  (error "this function hasn't been implemented yet"))


;; old CMUCL only
;;
;; (defun function-required-args (funspec)
;;   (let ((function (extract-function funspec)))
;;     (when function
;;       (loop for symbol in (read-from-string (lisp::%function-arglist function))
;; 	    until (member symbol LAMBDA-LIST-KEYWORDS) collect symbol))))

;; not implemented
(defun macro-required-args (funspec)
  (error "this function hasn't been implemented yet"))

;; old CMUCL only
;;
;; (defun macro-required-args (funspec)
;;   (let ((function (macro-function funspec)))
;;     (when function
;;       (loop for symbol in (read-from-string (lisp::%function-arglist function))
;; 	    until (member symbol LAMBDA-LIST-KEYWORDS) collect symbol))))

(defun setfable-p (symbol)
  ;; if it's macro we have to assume setf isn't bound.
  (if (macro-function symbol)
      nil
    ;;(setfable-p (car (macroexpand (cons symbol (MACRO-REQUIRED-ARGS symbol)))))
      (fboundp `(setf ,symbol))))

(defvar *edit-function-menu-doc-string-width* 90)


;; not implemented
(defun edit-function-args (funspec &optional current-argvs &key label extra-text only-keywords)
  (error "this function hasn't been implemented yet"))

;; old CMUCL only
;;
;; (defun edit-function-args (funspec &optional current-argvs &key label extra-text only-keywords)
;;   ;; Returns a list of arguments ARG-LIST with keywords in format that can be evaluated by -
;;   ;;
;;   ;;   (eval (cons function arg-list))
;;   ;;
;;   (let ((function (extract-function funspec)))
;;     (cond ((eval:interpreted-function-p function)
;; 	   (format t "~A is an interpreted function, so I'm not going to edit it...." function))
;; 	  (function
;; 	   (let* ((function-name (lisp::%function-name function))
;; 		  (doc-string (limit-string-span (documentation function-name 'function)
;; 						 *edit-function-menu-doc-string-width*))
;; 		  (label (or label (format nil "Edit ~A Function Args" function-name))))
;; 	     (multiple-value-bind (reg-args optional-args key-args)
;; 		 (extract-funspec-args funspec current-argvs)
;; 	       (let* ((all-argvs
;; 		       (if only-keywords key-args
;; 			   (concatenate 'list reg-args (concatenate 'list optional-args key-args))))
;; 		      (menu-vars (generate-and-set-special-vars (loop for argv in all-argvs collect (when (consp argv) (cadr argv))))))
;; 		 (loop for thing in all-argvs for i from 1 for menu-var in menu-vars
;; 		       when (and (not only-keywords) optional-args (= i (1+ (length reg-args))))
;; 		       collect '(:comment "&OPTIONAL arguments:") into menu-list
;; 		       when (and key-args (not only-keywords) (= i (+ 1 (length optional-args) (length reg-args))))
;; 		       collect '(:comment "&KEY arguments:") into menu-list
;; 		       collect (list menu-var (format nil "~s" (if (consp thing) (car thing) thing))
;; 				     (get-menu-entry-type-keyword (and (consp thing) (cadr thing))))
;; 		       into menu-list
;; 		       finally (choose-variable-values menu-list :label label :text (function-arg-menu-text extra-text doc-string only-keywords)))
;; 		 (loop for thing in all-argvs
;; 		       for thingv in (loop for menu-var in menu-vars collect (symbol-value menu-var))
;; 		       when (find-key-word optional-args (if (consp thing) (car thing) thing))
;; 		       collect thingv into out
;; 		       else when (find-key-word key-args (if (consp thing) (car thing) thing))
;; 		       collect (read-from-string (format nil ":~A" (if (consp thing) (car thing) thing)))
;; 		       into out and collect thingv into out
;; 		       else collect thingv into out
;; 		       finally (return out)))))))))

(defun setup-edited-function (funspec &key label extra-text)
  (error "this function hasn't been implemented yet"))

;; old CMUCL only
;;
;; (defun setup-edited-function (funspec &key label extra-text)
;;   (concatenate 'list
;; 	       (list (lisp::%function-name (extract-function funspec)))
;; 	       (function-required-args funspec)
;; 	       (edit-function-args funspec nil :label label :extra-text extra-text :only-keywords t)))

(defun document-function-args (funspec)
  (error "this function hasn't been implemented yet"))

;; old CMUCL only
;;
;; (defun document-function-args (funspec)
;;   (let ((current-argvs (when (consp funspec) (cdr funspec)))
;; 	(function (extract-function funspec)))
;;     (cond ((eval:interpreted-function-p function)
;; 	   (format t "~A is an interpreted function." function))
;; 	  (function
;; 	   (format t "~A args: " (lisp::%function-name function))
;; 	   (multiple-value-bind (reg-args optional-args key-args)
;; 	       (extract-funspec-args funspec current-argvs)
;; 	     (let ((symbol-values (concatenate 'list reg-args (concatenate 'list optional-args key-args))))
;; 	       (loop for symbol-value in symbol-values
;; 		     for arg-count from 1
;; 		     do (format t "~s ~s" (car symbol-value) (cadr symbol-value))
;; 		     when (< arg-count (length symbol-values))
;; 		     do (format t ", "))))))))

(defun find-key-word (key-word-value-list word)
  (loop for key-word-value in key-word-value-list
	when (string= (format nil "~A" (if (consp key-word-value) (car key-word-value) key-word-value))
		      (format nil "~A" word))
	do (return (if (consp key-word-value) key-word-value (list key-word-value nil)))))

(defun key-values-from-symbol-list (symbol-list)
  (loop for thing in symbol-list collect (list (format nil "~s" thing) thing)))

(defun choose-list-values-from-keys (list-of-key-values selected-keys ; selected-values
				     &key (label "Choose Some Items") text
				     (num-items-for-select-and-clear-all-buttons  4)
				     only-one-choice (punt-if-only-one-entry t) do-all-at-once (max-per-menu 10)
				     rank-margin (direction :vertical) (max-height 440))
  ;; '((key value)...(key value)) -> The KEYs appear in the menu, and a list of the values corresponding to the chosen keys is returned.
  (let* ((list-of-key-values
	  (loop for key-value in list-of-key-values
		when key-value collect (list (typecase (car key-value)
					       (string (car key-value))
					       (t ; princ-to-string
						(format nil "~S" (car key-value))))
					     (cadr key-value))))
	 (chosen-keys
	  (choose-list-values (loop for key-value in list-of-key-values collect (car key-value))
			      (when selected-keys
				(loop for key-value in list-of-key-values
				      when (or (member (car key-value) selected-keys :test 'equal)
					       (member (cadr key-value) selected-keys :test 'equal))
				      collect (car key-value)))
			      :label label :text text :max-height max-height
			      :num-items-for-select-and-clear-all-buttons num-items-for-select-and-clear-all-buttons
			      :do-all-at-once do-all-at-once :only-one-choice only-one-choice :punt-if-only-one-entry punt-if-only-one-entry
			      :max-per-menu max-per-menu :rank-margin rank-margin :direction direction)))
    (loop for key-value in list-of-key-values
	  when (and key-value (member (car key-value) (if only-one-choice (list chosen-keys) chosen-keys)))
	  when only-one-choice do (return (cadr key-value))
	  else collect (cadr key-value))))

(defvar *absolute-max-items-per-menu* 200)

(defun choose-list-values (items &optional selected-items
			   &key (num-items-for-select-and-clear-all-buttons 5)
			   only-one-choice do-all-at-once (punt-if-only-one-entry t) (max-per-menu 10)
			   rank-margin (direction :vertical) (label "Choose Some Items") (max-height 440) text)
  ;; Returns a list of selected items. The SELECTED-ITEMS argument does not have to include only items which are in the ITEMS list.
  (if (and (not *automatic-run*) items)
      (let* ((first-time t) (items (coerce-to-list items)) sub-items)
	(loop for item in items
	      for count from 1
	      for total-count from 1
	      do (push item sub-items)
	      when (or (= total-count (length items))
		       (and (not do-all-at-once) (= count max-per-menu))
		       (= count *absolute-max-items-per-menu*))
	      do (unless (or first-time (go-ahead-menu (format nil "~a - continue (~d left)?" label (- (length items) (- total-count count)))))
		   (return result))
	      (setq first-time nil)
	      and nconc
	      (let ((core-result (choose-list-values-core
				  (reverse sub-items)
				  selected-items
				  :num-items-for-select-and-clear-all-buttons num-items-for-select-and-clear-all-buttons
				  :label label :max-height max-height :punt-if-only-one-entry (unless (= 1 max-per-menu) punt-if-only-one-entry)
				  :only-one-choice only-one-choice :rank-margin rank-margin :direction direction :text text)))
		(if only-one-choice (list core-result) core-result))
	      into result
	      and do (setq punt-if-only-one-entry nil
			   count 0
			   sub-items '())
	      when (and only-one-choice (no-nils result)) do (return (car (no-nils result)))
	      finally (return (no-nils result))))
      selected-items))

(defun get-choose-list-values-panel (items rank-margin direction only-one-choice)
  (create-instance nil (if only-one-choice
			 (if *motif-gui-p* garnet-gadgets:motif-radio-button-panel garnet-gadgets:radio-button-panel)
			 (if *motif-gui-p* garnet-gadgets:motif-check-button-panel garnet-gadgets:x-button-panel))
		   (:toggle-p t)
		   (:left 10) (:font (o-formula *menu-font*)) (:top 10) (:rank-margin rank-margin) (:direction direction)
		   (:items items)))

(defun choose-list-values-core (items selected-items &key (label "Choose Some Items") title text num-items-for-select-and-clear-all-buttons
						       (max-height 440) only-one-choice (punt-if-only-one-entry t) rank-margin (direction :vertical))
  (when (and (not *automatic-run*) items)
    (if (and punt-if-only-one-entry (= 1 (length items)))
	(setq selected-items (if only-one-choice (car items) items))
	(let* ((initial-values (rem-not-in-keys selected-items items))
	       (panel (get-choose-list-values-panel items rank-margin direction only-one-choice))
	       (height (+ (gv panel :height) 25)) ; A little extra room
	       win top-agg panel-agg value) ; Create these here because srolling windows are handled a bit differently than interactor-windows.
	  (gv panel :value) ; Access :value slot to initialize slot formula and establish dependencies (Garnet Manual p.300).
	  (setq win (if (> (gv panel :height) max-height)
			(let* ((scroll-win-width (if (and (numberp rank-margin) (> rank-margin 1))
						     (min 300 (gv panel :width))
						     (+ 40 (gv panel :width))))
			       (win (get-regular-menu-window
				     (+ 20 max-height)
				     (+ scroll-win-width 50) ; this seems ok for the select and clear all buttons.
				     label title))
			       (scroll-win (create-instance nil	; gg:motif-scrolling-window-with-bars ;
					       (if *motif-gui-p* *motif-ss-win-w-bars* *ss-win-w-bars*)
					     (:background-color (when *motif-gui-p* OPAL:MOTIF-green))
					     (:left 0) (:top 0)
					     (:h-scroll-bar-p (and (numberp rank-margin) (> rank-margin 1)))
					     (:total-height (+ 50 height)) ; of the full area that graphics will be in
					     (:total-width (+ 10 (gv panel :width)))
					     (:height max-height) ; note: INNER (width and) height of outermost window
					     (:width scroll-win-width)
					     (:visible nil) ; (:title label) (:icon-title "SH Menu")
					     (:parent-window win))))
			  (opal:update win T)
			  (s-value win :scrolling-window scroll-win)
			  (opal:update scroll-win)
			  (s-value scroll-win :visible t)
			  (s-value win :visible t)
			  (create-instance nil scroll-window-Interactor (:Window `(,win ,(gv scroll-win :clip-window) ,(gv scroll-win :inner-window))))
			  (setq top-agg (gv scroll-win :PARENT-WINdow :aggregate)
				panel-agg (gv scroll-win :inner-aggregate))
			  (gv scroll-win :PARENT-WINdow))
			(let ((win (get-regular-menu-window height (+ (gv panel :width) *menu-width-fudge* (gv (ok-button) :width)) label title)))
			  (setq top-agg (gv win :aggregate)
				panel-agg (gv win :aggregate))
			  win)))
	  (when text (add-menu-text text top-agg win))
	  (if (and (not only-one-choice)
		   (or (not num-items-for-select-and-clear-all-buttons)
		       (>= (length items) num-items-for-select-and-clear-all-buttons)))
	      (add-ok-select-and-clear-all-buttons win top-agg panel)
	      (add-ok-button win top-agg))
	  (s-value panel :value (if only-one-choice (car initial-values) initial-values))
	  (opal:add-component panel-agg panel)
	  (s-value win :width (max (gv win :width)
				   (+ (* *window-manager-char-width* (length (gv win :title)))
				      ;; 2 mouseable boxes in window title bar
				      *window-manager-mouse-box-width* *window-manager-mouse-box-width*)))
	  (s-value win :visible t) (opal:update win t)
	  ;; (format t "Panel left ~A~%" ( gv panel :left)) (break)
	  (inter:wait-interaction-complete)
	  ;; kludge since the menus tend to walk over to the left
	  (setq *menu-top* (max 10 (gv win :top))
		*menu-left* (max 10 (gv win :left)))
	  (remove-and-free-buttons top-agg)
	  (setq value (gv panel :value))
	  (opal:destroy win)
	  (setq selected-items value)))
    selected-items))

(defun choose-hash-table-entries (table &optional selected-items &key (label (format nil "Choose hash table entries")) (punt-if-only-one-entry t))
  ;; Returns a list of selected hash table keys. This is based on the more sophisticated SELECT-HASH-VALUES-MENU -
  ;; CHOOSE-HASH-TABLE-ENTRIES is used, for example, at some intermediate stage in a selection sequence, and it is possible that
  (let ((list-of-table-names (list-all-hash-names table)))
    (if (and punt-if-only-one-entry
	     (= (length list-of-table-names) 1))
	list-of-table-names
	(select-hash-values-menu table label :selected-list selected-items :punt-if-only-one-entry punt-if-only-one-entry))))

(defun list-all-hash-names (hash-table-or-tables &optional selection-key)
  ;; Returns a list of all the table keys in the HASH-TABLE-OR-TABLES argument. If SELECTION-KEY is non-NIL, then only those table
  ;; entries with a non-NIL value for the :SELECTION-KEY slot will be added to the list.
  (loop for hash-table in (if (typep hash-table-or-tables 'cons) hash-table-or-tables (list hash-table-or-tables))
	nconcing
	(loop for key being the hash-key in hash-table
	      when (or (not selection-key)
		       (typecase selection-key
			 (function (funcall selection-key (gethash key hash-table)))
			 (t (eval (list selection-key (gethash key hash-table))))))
	      collecting key)))

(defun select-hash-values-menu (hash-tables menu-label &key selection-key ; #'null
				selected-list (rank-margin 5)
				(inclusion-key #'atom) punt-if-only-one-entry continue-prompt do-all-at-once (direction :horizontal) (max-per-menu 10))
  ;; Generates a selection menu and returns a list of names (hash table keys) that are selected from the HASH-TABLES. The
  ;; selection menu includes the names of all the elements in the HASH-TABLES, with pre-selected names taken from both any names
  ;; in SELECTED-LIST and the names of those HASH-TABLE elements for which the applied function SELECTION-KEY is non-nil. If the
  ;; tables are big enough, then the user is prompted at various intervals whether or not to continue checking off the items in
  ;; the tables. If the menu(s) are aborted at any time (via the user not authorizing additional menus), then the selected keys of
  ;; the queried entries, as well as the untested keys of the original SELECTED-LIST argument is returned. The INCLUSION-KEY is
  ;; applied to the table entries to determine whether or not they are included in the menu at all.
  (let* ((hash-tables (coerce-to-list hash-tables))
	 (table-key-list (loop for table in hash-tables nconcing (list-all-hash-names table inclusion-key)))
	 (key-list '())
	 (menu-key-value-list '())
	 (complete-selected-list
	  (delete-duplicates
	   (concatenate 'list (when selection-key (list-all-hash-names hash-tables selection-key) )
			;; Make sure that the items in selected-list are available in the hash-tables.
			(rem-not-in-keys selected-list table-key-list)))))
    (when table-key-list
      (if (or (not continue-prompt)
	      (ask-for-element-modifications menu-label (length table-key-list) "element"))
	  (if (and punt-if-only-one-entry (= (length table-key-list) 1))
	      table-key-list
	      (loop for key in table-key-list
		    for count upfrom 1
		    for total-count upfrom 1
		    do (push key key-list) (push (list (SIMPLE-NICE-STRING-FROM-ATOM key) key) menu-key-value-list)
		    when (or (and (not do-all-at-once) (= count max-per-menu))
			     (= count *absolute-max-items-per-menu*))
		    do (setq complete-selected-list
			     (delete-duplicates
			      (concatenate 'list
					   (rem-in-keys complete-selected-list key-list)
					   (choose-list-values-from-keys menu-key-value-list complete-selected-list
									 :label menu-label :do-all-at-once t :rank-margin rank-margin :direction direction
									 :punt-if-only-one-entry punt-if-only-one-entry)))
			     count 0
			     key-list '()
			     menu-key-value-list '())
		    and when (or (= total-count (length table-key-list)) (not (go-ahead-menu)))
		    do (return complete-selected-list)
		    finally (return
			      (delete-duplicates
			       (concatenate 'list
					    (rem-in-keys complete-selected-list key-list)
					    (choose-list-values-from-keys menu-key-value-list complete-selected-list
									  :label menu-label :punt-if-only-one-entry punt-if-only-one-entry
									  :do-all-at-once t :rank-margin rank-margin :direction direction))))))
	  complete-selected-list))))

(defun ask-for-element-modifications (label element-count element-name &optional default (max-count 10))
  ;; Returns T or NIL - Useful when there are a lot of elements.
  (or (< element-count max-count)
      (go-ahead-menu label (format nil "Go ahead to modify ~a property (~A ~as)" element-name element-count element-name) default)))

(defun go-ahead-menu (&optional (question "Go ahead with what we are doing?") (label "Authorization") (default t) enable-abort-option)
  ;; Returns T or NIL.
  (let ((dummy1 default) dummy2)
    (choose-variable-values
     `((dummy1 ,question :boolean)
       ,(when enable-abort-option '(dummy2 "Abort this thread to top level" :boolean)))
     :label (if (stringp label) label ""))
    (when dummy2
      (sim-error
       (format nil "Willfully aborting from the go-ahead menu")))
    dummy1))

(defun notification (&optional (message "Hey, pay attention!") (label "Notification"))
  (choose-variable-values `((,message :general-comment)) :label (if (stringp label) label "")))

(defun font-menu (&optional default-font label)
  (let ((dummy1 (when default-font (gv default-font :family)))
	(dummy2 (when default-font (gv default-font :face)))
	(dummy3 (when default-font (gv default-font :size))))
    (choose-variable-values
     '((dummy1 "Font family" :choose (:fixed :serif :sans-serif))
       (dummy2 "Font face" :choose (:roman :italic :bold :bold-italic) :rank-margin 4)
       (dummy3 "Font size" :choose (:small :medium :large :very-large) :rank-margin 4))
     :label (or label "Choose font characteristics"))
    (opal:Get-Standard-Font dummy1 dummy2 dummy3)))

(defun get-number (&optional (default 0.0) maximum minimum (menu-value-label "") menu-title menu-text (type :number))
  (let ((dummy1 default)
	(menu-value-label (when menu-value-label (if (stringp menu-value-label) menu-value-label (string menu-value-label)))))
    (when (and maximum minimum (< maximum minimum))
      (sim-error (format nil "Setting the maximum and minimum limits to ~A and ~A, respectively, just doesn't make sense!" maximum minimum)))
    (loop do (choose-variable-values
	      `((dummy1 ,menu-value-label ,type))
	      :title menu-title
	      :text (or menu-text
			(let ((type-string (string-downcase (format nil "~A" type))))
			  (cond ((and maximum minimum) (format nil "Enter ~A between ~d and ~d" type-string minimum maximum))
				(maximum (format nil "Enter ~a below or equal to ~d" type-string maximum))
				(minimum (format nil "Enter ~a above or equal to ~d" type-string minimum))
				(t (format nil "Enter ~A" type-string))))))
	  until (cond ((and maximum minimum) (<= minimum dummy1 maximum))
		      (maximum (<= dummy1 maximum))
		      (minimum (<= minimum dummy1))
		      (t t)))
    dummy1))

(defun get-integer (&optional (default 0) maximum minimum (menu-value-label "") menu-title menu-text)
  (get-number default maximum minimum menu-value-label menu-title menu-text :integer))

(defun get-float (&optional (default 0.0) maximum minimum (menu-value-label "") menu-title menu-text)
  (get-number default maximum minimum menu-value-label menu-title menu-text :float))

(defun choose-dash-patterns (&optional (default 0) (title "Line style Dash pattern"))
  (let ((dummy1 (princ-to-string default))
	(patterns (mapcar 'princ-to-string *line-styles-dash-patterns*)))
    (choose-variable-values `((dummy1 "Choose a dash pattern (0 is solid):" :choose ,patterns)) :label title)
    (read-from-string dummy1)))

(defun choose-list-contents (list-length candidates &optional (label (format nil "Construct A List of ~d Element~:p" list-length)))
  (let* ((dummies (list-head menu-dummys list-length))
	 (candidates (mapcar #'(lambda (elt)
				 (typecase elt
				   (schema (opal::name-for-schema elt))
				   (t (princ-to-string elt))))
			     candidates))
	 (menu-list (loop for dummy in dummies
			  for count from 1
			  unless (or (not (symbol-value dummy))
				     (member (symbol-value dummy) candidates))
			  do  (setf (symbol-value dummy) "NIL")
			  collect (list dummy (format nil "~D choice" count) :choose ; '("foo" "bar") ;
					(cons "NIL" (mapcar 'princ-to-string candidates))
					:rank-margin 6))))
    (choose-variable-values menu-list :label label)
    (mapcar 'read-from-string (mapcar 'symbol-value dummies))))

(defun menu-font-menu ()
  (setq *menu-font* (font-menu *menu-font* "Setting general menu font")
	*menu-text-font* (font-menu *menu-text-font* "Setting menu text font")
	*menu-button-font* (font-menu *menu-button-font* "Setting menu button font")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(export '(

	  MENU-FONT-MENU EDIT-STRING-LIST EDIT-LIST EDIT-FUNCTION-ARGS choose-dash-patterns CHOOSE-LIST-VALUES-FROM-KEYS choose-variable-values CHOOSE-LIST-VALUES
	  CHOOSE-HASH-TABLE-ENTRIES CHOOSE-LIST-CONTENTS
	  SELECT-HASH-VALUES-MENU FONT-MENU go-ahead-menu COLOR-CHOOSE-BUTTON-MENU new-color-choose-button-menu
	  REORDER-LIST-MENU
	  SIMPLE-TEXT-MENU
	  ASK-FOR-ELEMENT-MODIFICATIONS

	  NOTIFICATION GET-MENU-ENTRY-TYPE-KEYWORD
	  *menu-top* *menu-left* *menu-win* *ss-win-w-bars* *MOTIF-GUI-P*

	  FIND-KEY-WORD key-values-from-symbol-list add-ok-button LIST-ALL-HASH-NAMES

	  GET-NUMBER GET-FLOAT get-integer REORDER-VALUE-LIST-FROM-KEYS-MENU xclusive-choose-button CHOOSE-LIST-VALUES-CORE

	  *enable-precision-float-menus*
	  setup-edited-function EXTRACT-FUNCTION EXTRACT-FUNCTION-FROM-ATOM COMPILE-OR-EXTRACT-FUNCTION-OR-NUMBER *SUPPRESS-LAMBDA-FORM-COMPILER-MESSAGES*
	  EXTRACT-ALL-ARGS DOCUMENT-FUNCTION-ARGS GENERATE-AND-SET-SPECIAL-VARS extract-funspec-args function-required-args MACRO-REQUIRED-ARGS
	  setfable-p
	  ))
