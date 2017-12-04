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

;; GUI Source file: windows-hack.lisp


(in-package :windows-hack)


;;; Contains various window and miscellaneous utilities that are required by the MENU-HACK and
;;; PLOT-HACK systems, all of which depend on the Garnet GUI toolset from CMU.

;; Output windows :MODE slot refers to the general sort of output that goes to the window. These
;; include -
;;
;; :standard-plot
;; :histogram
;; :2dplot
;; :3dplot
;;
;; :info
;; :menu
;; :histology


;; The :PLOT-TYPE slot may be used to classify various windows whose :MODE is :STANDARD-PLOT
;; :polar
;; :waterfall
;; :XY

;; The :DATA-TYPE slot may be used to classify various windows of the same :MODE, and refers to the data
;; rather than the output format.


;; Note from garnet mailing list - for future reference:

;To make everything wait for a button to be hit in a window, the
;usual technique is to:
;1) make the window :modal-p T so that only that window can be
;operated on.
;2) call inter:wait-interaction-complete AFTER the window is
;updated.
;3) put a INTER:INTERACTION-COMPLETE in the final-function or
;selection-function.
;
;You might be able to use the query-gadget or error-gadget instead
;of writing your own.  If not, you can still look at the code of
;it to see how to do this, in particular: display-query-and-wait.
;
;
;
;
;;; *****************************************
;
;
;;; I've tried to use QUERY-GADGET as follows:
;
;(create-instance My-Query gg:query-gadget
;                          (:parent-window Top-Win))
;
;(gg:display-query-and-wait My-Query "some message ..." '("OK"))
;
;;; ---> a dialog box appears with a single "OK" button
;;; This is fine.
;
;;; then tried to display another dialog box (hopefully with the
;;; default button names, "OK" & "Cancel").
;
;(gg:display-query-and-wait My-Query "another message ...")
;
;;; ---> a dialog box appears with twin buttons, "OK" and "Cancel",
;;; but the button width is small enough for "OK" button.
;;; The width does not fit the width of "Cancel" button.
;
;;; I guess this is due to the :CONSTANT declaration for
;;; the TEXT-BUTTON that is to be appeard in a dialog box
;;; (see below).
;
;(create-instance 'ERROR-GADGET opal:aggregadget
;    ...
;   (:parts
;    `((:text ,opal:multi-text ... )
;      (:button ,TEXT-BUTTON
;                ...
;               (:Constant (T :Except :left :top)) ;;<----- here !!!
;                ...))))
;
;;; So one way to avoid this situation will be to initialize
;;; :FIXED-WIDTH-SIZE slot in the TEXT-BUTTON, just after
;;; an instance of QUERY-GADGET is created. Then the button
;;; width will fit that of "Cancel" button. But this does not
;;; care the case of a button whose width is longer than "Cancel".
;;; I think the :FIXED-WIDTH-SIZE slot should be declared as an
;;; exception for the constant.


;; **********************************
;;
;; Variables
;;
;; **********************************

(defvar *bleach-motif-gray* nil)	; opal:motif-gray is set to white, since on the laptop (at least) non-white/black window
				; backgrounds (e.g. motif) go to white. Setting motif-gray to white values explicitely avoids true
				; motif-gray gadgets with true to look wierd on the window white background.

(defvar *original-motif-gray* (create-instance nil opal:color))
(transfer-opal-color-values opal::motif-gray *original-motif-gray*)
#|
(gv gg:motif-background :filling-style)
(s-value gg:motif-background :filling-style (o-formula
					     (progn ; (format t "evaluating fillingstyle~%")
					       (if (gv opal:color :color-p)
						 (let ((fg (gvl :foreground-color)))
					; (format t "fg is ~A~%" fg)
						   (cond
						    ((eq fg opal:MOTIF-GRAY) opal:MOTIF-GRAY-FILL)
						    ((eq fg opal:MOTIF-BLUE) opal:MOTIF-BLUE-FILL)
						    ((eq fg opal:MOTIF-ORANGE) opal:MOTIF-ORANGE-FILL)
						    ((eq fg opal:MOTIF-GREEN) opal:MOTIF-GREEN-FILL)
						    (t (create-instance NIL opal:filling-style
									(:foreground-color fg)))))
						 NIL))))
; (s-value gg:motif-background :filling-style (create-instance nil opal:filling-style (:foreground-color *original-motif-gray*)))
|#
(export '(*BLEACH-MOTIF-GRAY* *original-motif-gray*))

(defun bleach-motif-gray () (transfer-opal-color-values opal::white opal::motif-gray))

(when *bleach-motif-gray* (bleach-motif-gray))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *automatic-run* nil "When T suppress GUI, e.g. menus are disabled.")

(defvar *default-graphics-window-background-color* 'white)
(defvar *default-plot-window-background-color* 'white
  "Default background color for plot windows (e.g. 'BLACK 'RED 'GREEN 'BLUE 'ORANGE 'CYAN 'PURPLE 'YELLOW 'WHITE).")

(export '*DEFAULT-PLOT-WINDOW-BACKGROUND-COLOR*)

(defvar *default-add-values-to-marker-label* nil)
(defvar *default-marker-label-position* :right)
(defvar *default-marker-point-type* :cross)
(defvar *default-marker-point-width* 10)
(defvar *default-marker-point-height* 10)
(defvar *default-marker-options* '(:include_cross_hair :include_label :include_point))

(defvar *comment-positions*
  `(:upper-left :upper-middle :upper-right
    :middle-left :middle :middle-right
    :lower-left :lower-middle :lower-right))

(defvar *default-running-comment-position* :upper-right)
(defvar *default-comment-position* :upper-right "Default comment/title position for graphics windows.")

(defvar *displayed-host-name* "" "For annotating windows - defaults to actual host.")
(defvar *always-add-host-name-to-windows* nil)
(defvar *add-host-name-to-windows* t "Add host name to windows if different than display server name.")

(defvar *plotting-window-top* 20)

(defvar *maker-label-vertical-fudge* 4)	; pixels
(defvar *maker-label-horizontal-fudge* 0)	; pixels

(defvar *window-tile-fudge-factor* 2) ; pixels

;; In pixels. Added to a window component's dimensions to give a little space for positioning. Used in COMPONENT-LEFT-AND-TOP.
(defvar *window-comp-fudge-factor-top* 0)
(defvar *window-comp-fudge-factor-left* 0)

(proclaim '(type fixnum *window-comp-fudge-factor-top* *window-comp-fudge-factor-left*))

(defvar *output-windows* '())		; A list of all the Garnet output windows (not menus).
(defvar *colorized-windows* '())
(defvar *twin* nil)			; The most recent window (car) of *output-windows*

(defvar *lock-all-windows* nil "When T, any new graphics windows are locked.")


;;; Empirical screen dimensions in pixels - opal globals are off (because of OLVM?).
(defvar *screen-width* 1 "Slightly adjusted value from OPAL:*SCREEN-WIDTH*.") ; opal:*screen-width* gives 1152
(defvar *screen-height* 1 "Slightly adjusted value from OPAL:*SCREEN-height*.") ; opal:*screen-height* gives 900
(defvar *window-manager-title-border-height* 18)	; Approximate height of border added by window manager


(defvar *standard-graphics-width* 1 "Default width in pixels for windows initialized with
INITIALIZE-GRAPHICS-WINDOW. Initialized by INITIALIZE-WINDOW-SYSTEM-VARIABLES.")

(defvar *standard-graphics-height* 1 "Default height in pixels for windows initialized with
INITIALIZE-GRAPHICS-WINDOW. Initialized by INITIALIZE-WINDOW-SYSTEM-VARIABLES.")

(defvar *raise-output-windows* t "New content to output windows will raise them.")
(defvar *show-output-windows* t)
(defvar *hide-output-windows* nil "Hide all output windows. Overrides *SHOW-OUTPUT-WINDOWS*, *RAISE-OUTPUT-WINDOWS*.")
(defvar *deiconify-output-windows* nil)
(defvar *reorder-top-agg* t)
(defvar *update-output-windows* t)
(defvar *print-and-destroy-output-windows* nil)

(defvar *plot-comment* "")
(defvar *omit-title-bar* nil)

(defvar *STANDARD-GRAPHICS-OUTPUT* nil)
(defvar *STANDARD-info-OUTPUT* nil)

(defvar *global-window-title-suffix* NIL "When a non-zero length string, this is added to the end of all window titles, preceded by ': '.")

(defvar *use-*plot-directory* nil)

(defvar *plot-directory* "//tmp/")
(defvar *data-directory* "//tmp/")
(defvar *plot-code-directory* "//tmp/")

(defvar *background-border-width* 2)
(defvar *running-comment-background-border-width* 10)
(proclaim '(type fixnum *background-border-width* *running-comment-background-border-width*))

(defvar *default-align-to-window-specification* nil)

(defvar *window-tile-x-gap* 0 "Sets the horizontal spacing in pixels used by ARRANGE-WINDOWS.")
(defvar *window-tile-y-gap* 0 "Sets the vertical spacing in pixels used by ARRANGE-WINDOWS.")
(defvar *window-tile-start-x* 0 "Sets the horizontal start reference in pixels used by ARRANGE-WINDOWS.")
(defvar *window-tile-start-y* 0 "Sets the vertical start reference in pixels used by ARRANGE-WINDOWS.")

(defvar *arrange-windows-per-row* 3)

(defvar *num-graphics-windows-rows* 3 "Number of graphics windows rows")
(defvar *num-graphics-windows-columns* 3 "Number of graphics windows columns")


(defvar *comment-font* *window-default-font*
  ;; opal:default-font
  ;; Default font for window comments. Set this to font returned by OPAL:GET-STANDARD-FONT. See also SET-*COMMENT-FONT*-MENU.
  )

(defun set-*comment-font*-menu (&optional default-font) (setq *comment-font* (font-menu (or default-font *comment-font*) "Set *COMMENT-FONT* (default commment font)")))


;;; Time and Date variables.


;; This factor is used to reduce the length of the integer returned by
;; GET-UNIVERSAL-TIME, since we don't need dates previous to 1994.
(defparameter *universal-time-conversion-factor*  2986300000)

(defvar *actual-time-stamp* 0)		; This is an integer
(defvar *time-stamp* "")		; This is a string
(defvar *time-stamp-suffix* 1)

;; These macros are used in several places for window calculations.
(defmacro schema-half-height (schema)
  `(/ (fn-gv ,schema :height) 2.0))

(defmacro schema-half-height-fn (schema)
  `(the (signed-byte 32) (round (schema-half-height ,schema))))

(defmacro schema-half-width (schema)
  `(/ (fn-gv ,schema :width) 2.0))

(defmacro schema-half-width-fn (schema)
  `(the (signed-byte 32) (round (schema-half-width ,schema))))

;; Running on mars (ultra) dosen't give a (assoc :HOST
;; lisp::*environment-list*) (??).
;; (defun initialize-window-system-variables ()
;;   (setq *displayed-host-name*
;; 	(cond ((and (stringp (cdr (assoc :HOST lisp::*environment-list*)))
;; 		    (> (length (cdr (assoc :HOST lisp::*environment-list*))) 0))
;; 	       (cdr (assoc :HOST lisp::*environment-list*)))
;; 	      ((> (length (machine-instance)) 0)
;; 	       (machine-instance))
;; 	      (t "")))
;;   (setq *screen-width* (- opal:*screen-width* 12)
;; 	*screen-height* (- opal:*screen-height* 10))
;;   ;; Set *STANDARD-GRAPHICS-WIDTH* and  *STANDARD-GRAPHICS-HEIGHT*.
;;   (setup-plot-tiling))

(defun windowp (object)
  (is-a-p object opal::window))

(defun transfer-schema-slots (source destination slots)
  ;; Each slot in SLOTS may be either a slot keyword or a list of a
  ;; keyword and a default value. In the latter case, if there SOURCE
  ;; is NIL then the default value is used for the slot value of
  ;; DESTINATION.
  (loop for slot in slots do
	(let ((slot-ref (if (consp slot) (first slot) slot))
	      (default-p (consp slot)))
	  (when (or source default-p)
	    ;; (format t "slot ~A value ~A~%" slot-ref (cond (source (gv source slot-ref)) (default-p (second slot))))
	    (s-value destination slot-ref (cond (source (gv source slot-ref))
						(default-p (second slot))))))))

(defun safe-destroy (obj)
  ;; Here is a kludge to avoid destroying already destroyed objects.
  (when (opal-obj-exists obj)
    (when (gv obj :aggregate)
      (destroy-top-agg (gv obj :aggregate) obj)
      ;; (opal:remove-component (gv obj :aggregate) obj)
      )
    (opal:destroy obj)))

(defun opal-obj-exists (obj)
  ;; When the Opal OBJ exists, e.g. not destroyed, return OBJ.
  (and obj
       (kr::is-schema obj)
       (kr::schema-bins obj)
					;       (eq (type-of obj) 'schema)
					;      (not (search "*DESTROYED*" (prin1-to-string obj)))
       obj))


;; This is used all over the place for insurance.
(defun clean-up-*output-windows* ()
  (setq *output-windows* (delete-if-not 'opal-obj-exists *output-windows*))
  (unless (opal-obj-exists *twin*) (setq *twin* (car *output-windows*)))
;  (unless (opal-obj-exists *standard-graphics-output*)
;    (setq *standard-graphics-output* nil)
;    (update-*standard-graphics-output*))
  *output-windows*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(proclaim '(inline x-y-in-win))
(defun x-y-in-win (x-y win)
  (declare (optimize (safety 0) (speed 3) (space 1)))
  (and (< 0 (the fn (car x-y)) (fn-gv win :width))
       (< 0 (the fn (cadr x-y)) (fn-gv win :height))))

(proclaim '(inline x-y-in-win-values))
(defun x-y-in-win-values (x y win)
  (declare (optimize (safety 0) (speed 3) (space 1)))
  (and (< 0 (the fn x) (fn-gv win :width))
       (< 0 (the fn y) (fn-gv win :height))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun comp-left (comp-width win-width position)
  (declare (optimize (safety 0) (speed 3) (space 1))
	   (fixnum comp-width win-width))
  (case position
    ((:upper-right :middle-right :lower-right) (- win-width (+ *window-comp-fudge-factor-left* comp-width)))
    ((:upper-left :middle-left :lower-left) *window-comp-fudge-factor-left*)
    ((:middle :upper-middle :lower-middle) (round (* 0.5 (- win-width comp-width))))))

(defun component-left (component window position)
  ;; Returns the left coordinate of COMPONENT when placed at the relative POSITION in WINDOW.
  (comp-left (gv component :width) (gv window :width) position))

(defun comp-top (comp-height win-height position)
  (declare (optimize (safety 0) (speed 3) (space 1))
	   (fixnum comp-height win-height))
  (case position
    ((:upper-right :upper-left :upper-middle) *window-comp-fudge-factor-top*)
    ((:middle-right :middle-left :middle) (round (* 0.5 (- win-height comp-height))))
    ((:lower-right :lower-left :lower-middle) (- win-height (+ *window-comp-fudge-factor-top* comp-height)))))

(defun component-top (component window position)
  ;; Returns the top coordinate of COMPONENT when placed at the relative POSITION in WINDOW.
  (comp-top (gv component :height) (gv window :height) position))

(defun component-left-and-top (component window position)
  ;; Returns as values the left and top coordinates of COMPONENT when placed at the relative POSITION in WINDOW.
  (values (component-left component window position) (component-top component window position)))

(defun move-comp (comp window position)
  (s-value comp :left (component-left comp window position))
  (s-value comp :top (component-top comp window position))
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun default-graphics-color (&optional (win *standard-graphics-output*))
  (if (and (opal-obj-exists win) (eql (gv win :background-color) opal::black))
    opal::red opal::blue
      ; opal::white opal::black
      ))

(defun graphics-text (string win &key
			     color background-color
			     background
			     size	; :large
			     face	; :bold-italic
			     family	; :serif
			     font)
  (let* ((font (or font (and family face size (opal:get-standard-font family face size))))
	 (line-style (graphics-text-linestyle color win 0 background-color)))
    (create-instance nil opal:text
		     (:window win)
		     (:fill-background-p background)
		     (:string string)
		     (:font (or font (o-formula (graphics-text-font (gvl :window)))))
		     (:line-style line-style))))

(defun graphics-text-linestyle (color win &optional thickness background-color)
  (let ((linestyle (create-instance nil opal:default-line-style (:window win) (:constant nil) (:thickness thickness)))
	(background-color (or background-color (o-formula (or (gvl :window :background-color) opal::white))))
	(foreground-color (or color (o-formula (or (gvl :window :default-graphics-color) opal::black)))))
    (cond-every (background-color (s-value linestyle :background-color background-color))
		(foreground-color (s-value linestyle :foreground-color foreground-color)))
    linestyle))

(defun graphics-text-font (&optional win)
  (or (and win (gv win :font))
      *window-default-font*
      ;;      (opal:get-standard-font :serif :roman ; ;bold-italic
      ;;			      :medium)
      ))




(create-instance 'white-feedback-object-line-style dashed-line (:foreground-color opal::white))
(create-instance 'black-feedback-object-line-style dashed-line (:foreground-color opal::black))

;; (create-instance 'feedback-object-line-style dashed-line (:foreground-color (o-formula (gvl :window :default-graphics-color))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(create-instance 'basic-graphics-window
		 inter:interactor-window
		 (:icon-title (o-formula (gvl :title)))
		 (:visible nil)
		 (:double-buffered-p t)
		 (:min-width 100) (:min-height 100)

		 (:comment-font (o-formula *comment-font*))
		 (:font *window-default-font*
			;; (opal:get-standard-font :serif :roman :medium)
			)

		 (:theta 0.0) (:phi 0.0) ; radians

		 ;; Convenient variables for graphing routines
		 (:cos-phi (o-formula (cos (gvl :phi))))
		 (:sin-phi (o-formula (sin (gvl :phi))))
		 (:cos-theta (o-formula (cos (gvl :theta))))
		 (:sin-theta (o-formula (sin (gvl :theta))))
		 (:sin-phi*sin-theta (o-formula (* (gvl :sin-phi) (gvl :sin-theta))))
		 (:cos-phi*sin-theta (o-formula (* (gvl :cos-phi) (gvl :sin-theta))))

		 ;; These functions transform x and y values in window data units to window pixels.
		 (:x-graphics-win-function nil) (:y-graphics-win-function nil)

		 (:background-color (symbol-to-opal-color *default-graphics-window-background-color*))

		 (:feedback-object-line-style ;(o-formula
					; (if (eql opal::black (gvl :background-color))
					; white-feedback-object-line-style
					; black-feedback-object-line-style))
					; (create-instance nil feedback-object-line-style)
					; (o-formula (create-instance nil dashed-line (:foreground-color  (gvl :window :default-graphics-color))))
		  dashed-line
		  )
		 (:default-graphics-color (o-formula (if (eql opal::black (gvl :background-color)) opal::white opal::black)))
		 (:default-line-style (o-formula (create-instance nil opal:default-line-style (:constant nil)
								  (:foreground-color (or (gvl :default-graphics-color) opal::black)))))
		 (:default-graphics-background (o-formula (if (eql opal::black (gvl :background-color)) opal::black-fill opal::white-fill)))
		 (:default-graphics-filling-style (o-formula (if (eql opal::black (gvl :background-color)) opal::white-fill opal::black-fill))))

(defun create-scrolling-display-window (&key (width 750) (height 300) type display-object title (mode :scrolling-output))
  (let* ((win (create-instance nil basic-graphics-window ; inter:interactor-window
			       (:visible nil)
			       (:mode mode)
			       (:width width)
			       (:title title)
			       (:height height)
			       (:type type)
			       (:omit-title-bar-p *omit-title-bar*)))
	 (scroll-win (create-instance nil gg:motif-scrolling-window-with-bars
				      (:mode 'top-scroll)
				      (:visible nil)
				      (:title (o-formula (gvl :parent-window :title)))
				      (:left 0)
				      (:top 0)
				      (:width (o-formula (- (gvl :parent-window :width) (* 2 (gvl :border-width)))))
				      (:height (o-formula (floor (- (gvl :parent-window :height) (gvl :border-width)))))
				      (:parent-window win)
				      (:total-width (o-formula (+ (gvl :parent-window :display-object :width)
								  (gvl :parent-window :display-object :left)) 200))
				      (:total-height (o-formula (+ (gvl :parent-window :display-object :top)
								   (gvl :parent-window :display-object :height)) 200))
				      (:h-scroll-bar-p t)
				      (:v-scroll-bar-p t))))
    (s-value win :display-object display-object)
    (opal:update win)
    (s-value display-object :scrolling-window scroll-win)
    (s-value win :scrolling-window scroll-win)
    (opal:update scroll-win)
    (s-value scroll-win :visible t)
    (s-value win :visible t)
    (opal:add-component (gv scroll-win :inner-aggregate) display-object)
    (create-instance nil scroll-window-Interactor (:Window `(,win ,(gv scroll-win :clip-window) ,(gv scroll-win :inner-window))))
    (create-instance NIL print-window-Interactor (:Window `(,win ,(gv scroll-win :clip-window) ,(gv scroll-win :inner-window))))
    (create-instance NIL destroy-window-Interactor (:Window `(,win ,(gv scroll-win :clip-window) ,(gv scroll-win :inner-window))))
    (create-instance nil raise-all-menus-interactor (:Window `(,win ,(gv scroll-win :clip-window) ,(gv scroll-win :inner-window))))
    (s-value scroll-win :foreground-color OPAL:white)
    (push win *output-windows*)
    win))

(defun x-graphics-win (x win)
  ;; Transform X value in window data units to window pixels, according the window's :X-GRAPHICS-WIN-FUNCTION slot. Return original X value if
  ;; :X-GRAPHICS-WIN-FUNCTION is NIL.
  (if (gv win :x-graphics-win-function)
    (funcall (gv win :x-graphics-win-function) (float x) win)
    (round x)))

(defun y-graphics-win (y win)
  ;; Transform Y value in window data units to window pixels, according the window's :Y-GRAPHICS-WIN-FUNCTION slot. Return original Y value if
  ;; :Y-GRAPHICS-WIN-FUNCTION is NIL.
  (if (gv win :y-graphics-win-function)
    (funcall (gv win :y-graphics-win-function) (float y) win)
    (round y)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun fix-window-size (win)
  (s-value win :min-width (gv win :width))
  (s-value win :max-width (gv win :width))
  (s-value win :min-height (gv win :height))
  (s-value win :max-height (gv win :height)))

(defun create-instances (classes) (loop for class in classes collect (create-instance nil class)))

(defun get-agg (win &optional force)
  ;; Return the :AGGREGATE of WIN, or the :AGGREGATE of the :CHILD window if WIN :MODE is :INFO. If non INFO window, and FORCE is T then create and
  ;; add new aggregate.
  ;; Now makes sure that :window slot of returned agg is set to WIN.
  (let ((agg (cond ((eq (gv win :mode) :info) (gv (car (gv win :child)) :aggregate))
		   ((gv win :aggregate) (gv win :aggregate))
		   (force (s-value win :aggregate (create-instance nil opal:aggregate))))))
    (when agg (s-value agg :window win))
    agg))

(defun get-agg-components (win)
  (let ((agg (get-agg win)))
    (when agg (gv agg :components))))

(defun get-top-level-thing (win thing)
  (loop for comp in (get-agg-components win) when (eq thing (car (gv comp :is-a))) do (return comp)))

(defun retrieve-window-agg-comp (win comp-type)
  ;; Return first component in WIN :AGGREGATE that :IS-A COMP-TYPE.
  (loop for comp in (get-agg-components win)
	when (eq (car (gv comp :is-a)) comp-type)
	do (return comp)))

(defun retrieve-window-agg-comps (win comp-type)
  ;; Return all components in WIN :AGGREGATE that :IS-A COMP-TYPE.
  (loop for comp in (get-agg-components win)
	when (eq (car (gv comp :is-a)) comp-type)
	collect comp))

(defun schema-window (schema)
  (when schema
    (or (gv schema :window)
	(schema-window (gv schema :parent)))))

(export 'SCHEMA-WINDOW)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; These are various types of window labels. Each is added to an instance of LABEL-AGG-PROTO, and then the label-agg is added to
;; the window's top aggregate.

(create-instance 'label-agg-proto opal:aggregate)

(create-instance 'window-hack-text opal:cursor-multi-text
		 (:text "")
		 (:orientation :horizontal)
		 (:line-style (o-formula (or (gvl :parent :window :default-line-style)
					     (graphics-text-linestyle (or (gvl :parent :window :default-graphics-color) opal::black)
								      (gvl :parent :window)))))
		 (:string (o-formula (or (case (gvl :orientation)
					   (:vertical (convert-string-to-vertical (gvl :text)))
					   (t (gvl :text)))
					 "")))
		 (:font (o-formula (graphics-text-font (gvl :parent :window)))))

#|
:top
(case (gvl :position)
  ((:upper-middle :upper-left :upper-right) 0)
  ((:middle :middle-left :middle-right)
   (round (* 0.5 (- (the fn (gvl :window :height)) (the fn (gvl :background :height))))))
  ((:lower-middle :lower-right :lower-left) (- (the fn (gvl :window :height)) (the fn (gvl :background :height)))))
:left
(case (gvl :position)
  ((:lower-left :middle-left :upper-left) 0)
  ((:upper-right :middle-right :lower-right)
   (- (the fn (gvl :window :width)) (the fn (gvl :background :width))))
  ((:lower-middle :middle :upper-middle)
   (round (* 0.5 (- (the fn (gvl :window :width)) (the fn (gvl :background :width)))))))
|#

;;; A comment is sometimes useful. Default goes in the lower right hand corner.
(create-instance 'window-comment opal:aggregadget
		 (:position :lower-right)
		 (:orientation :horizontal) ; or :VERTICAL
                 (:top (o-formula (comp-top (gvl :background :height) (gvl :window :height) (gvl :position))))
                 (:left (o-formula (comp-left (gvl :background :width) (gvl :window :width) (gvl :position))))
		 (:background-border-width *background-border-width*)
		 (:parts
		  `((:background ,opal:rectangle
				 ;; (:filling-style ,(o-formula (or (gvl :parent :window :default-graphics-background) opal::white-fill)))
				 (:filling-style ,(o-formula (if (black-p (or (and (gvl :window) (gvl :window :background-color))
									      (and (gvl :parent)
										   (gvl :parent :window) (gvl :parent :window :background-color))))
							       opal::black-fill opal::white-fill)))
				 (:left ,(o-formula (gvl :parent :left)))
				 (:top ,(o-formula (gvl :parent :top)))
				 (:width ,(o-formula (+ (the fn (gvl :parent :label :width))
							(if (zerop (length (gvl :parent :label :string))) 0 (* 2 (the fn (gvl :parent :background-border-width)))))))
				 (:height ,(o-formula (+ (the fn (gvl :parent :label :height))
							 (if (zerop (length (gvl :parent :label :string)))
							   0
							   (* 2 (the fn (gvl :parent :background-border-width)))))))
				 (:box '(0 0 0 0))
				 ;; LBG 10.03.00 - uncomment apr 18, 00 -commented out may 8, 01
				 (:line-style nil)
				 (:visible ,(o-formula (gvl :parent :visible))))
		    (:frame ,opal:rectangle
			    (:line-style ,(o-formula (or (gvl :parent :window :default-line-style)
							 (graphics-text-linestyle (or (gvl :parent :window :default-graphics-color) opal::black)
										  (gvl :parent :window)))))
			    (:left ,(o-formula (gvl :parent :background :left)))
			    (:top ,(o-formula (gvl :parent :background :top)))
			    (:width ,(o-formula (gvl :parent :background :width)))
			    (:height ,(o-formula (gvl :parent :background :height)))
			    (:visible ,(o-formula (and (gvl :parent :visible) (gvl :parent :borderp)))))
		    (:label ,window-hack-text
			    (:font ,(o-formula (or (gvl :parent :window :comment-font) (graphics-text-font (gvl :parent :window)))))
			    ;; (:text ,(o-formula (or (gvl :parent :window :title) "")))
			    (:orientation ,(o-formula (gvl :parent :orientation)))
			    ;; (:visible ,(o-formula (gvl :parent :visible)))
			    (:left ,(o-formula (+ (the fn (gvl :parent :left)) (the fn (gvl :parent :background-border-width)))))
			    (:top ,(o-formula (+ (the fn (gvl :parent :top)) (the fn (gvl :parent :background-border-width))))))))
		 (:interactors
		  `((:text-inter ,inter:text-interactor
				 (:window ,(o-formula (gv-local :self :operates-on :window)))
				 (:feedback-obj nil)
				 (:start-where ,(o-formula (list :in (gvl :operates-on :label))))
				 (:abort-event :CONTROL-\g)
				 (:stop-event (:leftdown))))))

;; Why needed???
#|
(s-value window-comment :label :font
	 (o-formula (cond
		     ((gvl :parent :comment-font) (gvl :parent :comment-font))
		     ((gvl :parent :font) (gvl :parent :font))
		     (t (opal:get-standard-font :serif :bold-italic :medium)))))
|#

(s-value window-comment :label :font
	 (o-formula (or (gvl :parent :window :comment-font)
			(gvl :parent :window :font)
			*window-default-font*
			;; (opal:get-standard-font :serif :roman :medium)
			)))

(gv window-comment :frame :visible)
(s-value window-comment :visible nil) ;(s-value window-comment :frame :visible nil)
(s-value (create-instance 'window-temp-comment window-comment) :visible nil)
(s-value (create-instance 'window-title window-comment (:window-title t)) :visible nil)
(s-value window-title :position :lower-left)

; not used ... sure? 30.7.00
(create-instance 'label-background
                 opal:rectangle
                 (:visible t)
		 (:filling-style opal::white-fill)
		 (:label nil)
		 (:left (o-formula (first (gvl :box))))
		 (:top (o-formula (second (gvl :box))))
		 (:width (o-formula (third (gvl :box))))
		 (:height (o-formula (fourth (gvl :box))))
		 (:box (o-formula (multiple-value-bind (left top width height)
				      (opal:bounding-box (gvl :parent))
				    (list (- (the fn left) 5)
					  (- (the fn top) 5)
					  (+ (the fn width) 10)
					  (+ (the fn height) 10)))))
		 ;; (:box '(0 0 0 0))
		 (:line-style nil))

(defun redo-label-background (label-agg)
  (loop for comp in (gv label-agg :components) when (eq label-background (car (gv comp :is-a)))
	do (opal:remove-component label-agg comp) (opal:destroy comp))
  (let ((label-background (create-instance nil label-background
					   (:where :back)

;                                           (:box (multiple-value-bind (left top width height)
;                                                     (opal:bounding-box label-agg)
;                                                   (list (- left 5)
;                                                         (- top 5)
;                                                         (+ width 10)
;                                                         (+ height 10))))

					   )))
    (opal:add-component label-agg label-background :where :back)))

(defun host-is-display-server ()
  "Return T if host machine is display server."
  (error "Not supported.  Garnet API still in flux with respect to this"))

;; (let* ((display (cdr (assoc :DISPLAY lisp::*environment-list*)))
;; 	 (display-machine (string-head display (search ":" display))))
;;   (true-p (or (string= display ":0.0")
;; 		(string= (machine-instance) display-machine)
;; 		(search display-machine (machine-instance))))))

(defun GET-win-TITLE-STRING (base-title)
  ;; Starting with a base title string, tacks on the current value of *GLOBAL-WINDOW-TITLE-SUFFIX* and, if appropriate, the *DISPLAYED-HOST-NAME*.
  (let ((base-title (if (and (sequencep *global-window-title-suffix*) (> (length *global-window-title-suffix*) 0))
		      (format nil "~A: ~A" base-title *global-window-title-suffix*)
		      base-title)))
    (if (or *always-add-host-name-to-windows*
	    (and *add-host-name-to-windows* (not (HOST-IS-DISPLAY-SERVER))))
      (format nil "(~A) ~A" *displayed-host-name* base-title)
      base-title)))

(defun strip-displayed-host-name-from-title (title)
  (let* ((host-name-string (format nil "(~A) " *displayed-host-name*))
	 (search-position (search host-name-string title)))
    (if search-position
	(string-tail title (- (length title) (+ (length host-name-string) search-position)))
	title)))

(defun find-output-windows-from-title (title) (MAPCAR-RETURN-NO-NILS *output-windows* (when (string= (gv value :title) title) value)))

(defun find-output-windows (windows &optional type)
  (loop for win in (MAPCAR-RETURN-NO-NILS windows (cond
						    ((windowp value) value)
						    ((stringp value) (find-output-windows-from-title value))))
	when (or (not type) (equal (gv win :type) type))
	collect win))

;; Window Alignment/Dimensions

(defun 3x2-plot-tiling-square ()
  ;; "Set *STANDARD-GRAPHICS-WIDTH* and *STANDARD-GRAPHICS-HEIGHT* for 3x2 tiling, with squares."
  (let ((side (- (truncate (/ *screen-height* 3)) *window-manager-title-border-height*)))
    (setq *standard-graphics-width* side
	  *standard-graphics-height* side)))

(defun 3x2-plot-tiling ()
;;  "Set *STANDARD-GRAPHICS-WIDTH* and *STANDARD-GRAPHICS-HEIGHT* for 3x2 tiling, with windows filling screen."
  (setq *standard-graphics-width* (truncate (- (/ *screen-width* 2) 10))
	*standard-graphics-height* (- (truncate (/ *screen-height* 3)) *window-manager-title-border-height*)))

(defun setup-plot-tiling ()
  "Set *STANDARD-GRAPHICS-WIDTH* and *STANDARD-GRAPHICS-HEIGHT* for tiling, according to the global variables
*NUM-GRAPHICS-WINDOWS-ROWS* and *NUM-GRAPHICS-WINDOWS-COLUMNS*, with windows filling screen."
  (setq *standard-graphics-width* (truncate (- (/ *screen-width* *NUM-GRAPHICS-WINDOWS-COLUMNS*) 10))
	*standard-graphics-height* (- (truncate (/ *screen-height* *NUM-GRAPHICS-WINDOWS-ROWS*)) *window-manager-title-border-height*)))

(defun align-to-window (&optional reference-window &key (alignment *default-align-to-window-specification*))
  (let* ((reference-window (or reference-window (win-menu "Align Windows Reference" nil nil t)))
	 (windows (win-menu (format nil "Choose Windows to Align with ~A" (gv reference-window :title))))
	 (dummy1 alignment))
    (choose-variable-values
     '((dummy1 "Align to:" :x-choose (:left :top)))
     :label (format nil "Aligning Windows to ~A" (gv reference-window :title)))
    (loop for win in windows
	  do (move-window win
	      (if (member :top dummy1) (gv reference-window :top) (gv win :top))
	      (if (member :left dummy1) (gv reference-window :left) (gv win :left))))))

(defun match-win-dimensions-menu (win matched-dimensions &optional additional-windows-to-set)
  (let ((match-win (choose-list-values-from-keys
		    (loop for candidate-win in (windows-of-mode (gv win :mode))
			  unless (member candidate-win (cons win additional-windows-to-set))
			  collect (list (gv candidate-win :title) candidate-win))
		    nil
		    :punt-if-only-one-entry nil :only-one-choice t
		    :text (format nil "Choose window for setting ~a dimension~%of window(s) ~a"
				  (NICE-STRING-FROM-KEYWORD matched-dimensions)
				  (atomize-list (loop for win in (cons win additional-windows-to-set) collect (gv win :title))))
		    :label "Matching Window Dimensions")))
    (when match-win (match-win-dimensions win match-win matched-dimensions))))

(defun match-win-dimensions (&optional target match (matched-dimensions :Width_&_Height))
  "Resize windows referenced by TARGET to the dimensions of the MATCH window as specified by MATCHED-DIMENSIONS [default
:WIDTH_&_HEIGHT, otherwise :WIDTH or :HEIGHT]."
  (let* ((match (or match (id-win "Click or type on window for resize reference")))
	 (target (or target (id-win (format nil "Click or type on window to resize to ~A" (gv match :title))))))
    (loop for target-win in (case target
			      (:all *output-windows*)
			      (t (coerce-to-list target)))
	  do (case matched-dimensions
	       (:Width_&_Height (s-value target-win :width (gv match :width)) (s-value target-win :height (gv match :height)))
	       (:Width (s-value target-win :width (gv match :width)))
	       (:Height (s-value target-win :height (gv match :height))))
	  (opal::update target-win t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Window Comments
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-window-comment (win &key position (type window-comment) ignore-reserved)
  ;; Return first component of WIN :AGGREGATE of TYPE that is not :RESERVED (unless IGNORE-RESERVED) and, if POSITION is non-NIL, has :POSITION eq to
  ;; POSITION.
  (loop for comment in (retrieve-window-agg-comps win type)
	when (and (or ignore-reserved (not (gv comment :reserved)))
		  (or (not position) (eq (gv comment :position) position)))
	do (return comment)))

(defun get-window-comment-text (win &key position (type window-comment))
  (let ((comment (get-window-comment win :position position :type window-comment)))
    (if comment
	(gv comment :label :string)
	"")))

(defun parse-comment-position (position &optional (default *default-comment-position*)) (if (member position *comment-positions*) position default))

(defun add-comment (window comment &key font (update t) append-to-old-comment (position *default-comment-position*) (borderp t))
  ;; Add a comment to WINDOW, which may be either a single window or a list of windows, and may be either the pointers to the windows or window
  ;; titles. Return last created comment.
  (let (return-value)
    (loop for win in (find-output-windows window) do
	  (setq return-value
		(if (or (not comment) (string= comment ""))
		    (when position (let ((*automatic-run* t)) (add-comment-menu win :position-to-remove position)))
		    (add-label win window-comment comment :append-to-existing-text append-to-old-comment :font font :borderp borderp :position position)))
	  (when update (opal:update win)))
    return-value))

(defun add-temp-comment (win &optional comment &key font (update t) position resurrect deiconify ignore-reserved)
  (let ((temp-comment (get-window-comment win :type window-temp-comment :ignore-reserved ignore-reserved)))
    (when font (s-value win :temp-comment-font font))
    (cond (position (s-value win :temp-comment-position position))
	  ((not (gv win :temp-comment-position))
	   (s-value win :temp-comment-position :upper-right)))
    (unless (and (not temp-comment) (stringp comment) (string= comment ""))
      (if (and temp-comment (stringp comment) (string= comment ""))
	(progn
	  (s-value temp-comment :label :text comment)
	  (s-value temp-comment :visible nil))
	(progn
	  (if temp-comment (s-value temp-comment :label :text comment) (setq temp-comment (add-label win window-temp-comment comment)))
	  (s-value temp-comment :visible t)))
      (when temp-comment (s-value temp-comment :position (gv win :temp-comment-position)))
      (cond (resurrect (resurrect-opal-win win :raise t :visible t :deiconify deiconify :update nil))
	    (update (opal:update win))))))

(defun retrieve-window-comment (win position)
  (loop for comp in (gv win :aggregate :components) when (and (eq (car (gv comp :is-a)) window-comment) (eq (gv comp :position) position)) do (return comp)))

(defun remove-comment (win position)
  (let ((old-comment (retrieve-window-comment win position)))
    (when old-comment
      (opal:remove-component (get-agg win t) old-comment)
      (opal:destroy old-comment))))

(defun add-comment-menu (win &key position-to-remove)
  (let* ((positions *comment-positions*)
	 (last-comment (gv win :last-comment))
	 (dummy1 (or position-to-remove (when last-comment (gv last-comment :position)) *default-comment-position*))
	 (dummy2 "") dummy3 (dummy4 position-to-remove) dummy5 dummy6 (dummy7 t)
	 (dummy8 (when (opal-obj-exists last-comment) (gv last-comment :frame :visible)))
	 dummy9 (dummy10 t) dummy11 return-value)
    (choose-variable-values
     `((dummy1 "Position:" :choose ,positions)
       (dummy2 "String, can be edited from the window:" :string)
       (dummy7 "Add comment at indicated position" :boolean)
       (dummy10 "Append new text to any existing comment at same position" :boolean)
       (dummy4 "Remove comment at indicated position" :boolean)
       (dummy8 "Include border in comment" :boolean)
       (dummy5 "Edit comment/title font" :boolean)
       (dummy6 "Change pointer comment position" :boolean)
       (dummy9 "Add/edit window title" :boolean)
       (dummy11 "Edit running comment" :boolean)
       (dummy3 "CANCEL" :boolean))
     :label (format nil "Add/remove comment to ~s" (gv win :title)))
    (when dummy9 (edit-title win))
    (when dummy11 (let ((dummy1 (or (gv win :running-comment-position) *default-running-comment-position*)))
		    (choose-variable-values
		     `((dummy1 "Position:" :choose ,positions))
		     :label "Running Comment Menu" :text (gv win :title))
		    (s-value win :running-comment-position dummy1)))
    (unless dummy3
      (when dummy6
	(let ((dummy1 (or (gv win :temp-comment-position) :upper-right))
	      (temp-comment (retrieve-window-agg-comp win window-temp-comment)))
	  (choose-variable-values `((dummy1 "Position:" :choose ,positions)) :label (format nil "Pointer comment position in ~s" (gv win :title)))
	  (s-value win :temp-comment-position dummy1)
	  (when temp-comment (s-value temp-comment :position dummy1))))
      (when dummy5 (s-value win :comment-font (font-menu (or (gv win :comment-font) (gv win :font) *window-default-font*
							     ;; (opal:get-standard-font :serif :bold-italic :medium)
							     )
							 (format nil "Comment font for ~A" (gv win :title)))))
      (if dummy4
	  (remove-comment win dummy1)
	  (let* ((old-comment (retrieve-window-comment win dummy1))
		 (text (if (and old-comment dummy10) (concatenate 'string (gv old-comment :label :string) dummy2) dummy2)))
	    (when (or dummy10 dummy7)
	      (if old-comment
		  (s-value old-comment :label :string text)
		  (when dummy7 (s-value (opal:add-component (get-agg win t) (setq old-comment (create-instance nil window-comment (:position dummy1)))) :label :string text)))
	      (when old-comment (s-value win :last-comment old-comment)))
	    (unless dummy4 (when old-comment (s-value old-comment :frame :visible dummy8)))
	    (setq return-value old-comment)))
      (opal:update win)
      return-value)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Window Labels
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun adjust-label-background (label-agg) (redo-label-background label-agg))

(defun adjust-all-labels (win)
  (let ((agg (get-agg win)))
    (when agg
      (loop for comp in (get-agg-components win)
	    when (eq label-agg-proto (car (gv comp :is-a))) do (redo-label-background comp) and collect comp into labels
	    when (gv comp :label) collect comp into labels
	    finally (loop for label in labels do (opal:move-component agg label :where :front))))))

(defun get-label (win type position get-new)
  (or (unless get-new (get-window-comment win :position position :type type))
      (opal:add-component (get-agg win t) (create-instance nil (or type window-comment)))))

(defun add-label (win type text &key append-to-existing-text font (borderp t) position get-new)
  (when win
    (let ((position (parse-comment-position position)))
      (case (gv win :mode)
	(:info (let ((text-obj (gv win :text-object)) temp)
		 (s-value text-obj :last-font (gv text-obj :current-font))
		 (opal:set-cursor-to-x-y-position text-obj 0 (- (gv text-obj :scrolling-window :height) (gv text-obj :scrolling-window :y-offset)))
		 (setq temp (opal:kill-rest-of-line text-obj))
		 (push (if (consp temp) (caaar temp) "") (gv text-obj :deletions))
		 (opal:go-to-prev-line text-obj)
		 (setq temp (opal:kill-rest-of-line text-obj))
		 (push (if (consp temp) (caaar temp) "") (gv text-obj :deletions))
		 (opal:insert-string text-obj text font)
		 (opal:go-to-beginning-of-line text-obj)))
	(t (let ((label (get-label win type position get-new)))
	     (when position (s-value label :position position))
	     (s-value label :label :text (if text
					     (if append-to-existing-text
						 (concatenate `string (gv label :label :text) (when text (format nil "~%")) text) text)
					     ""))
	     ;; (s-value label :background :visible borderp)
	     (s-value label :borderp borderp)
	     (s-value label :window win)
	     (when font (s-value label :font font))
	     label))))))

(defun remove-label (win type &key position)
  (if (eq (gv win :mode) :info)
      (let* ((text-obj (gv win :text-object))
	     (font (gv text-obj :last-font)))
	(opal:kill-rest-of-line text-obj)
	(loop for string in (gv text-obj :deletions) do
	      (opal:insert-string text-obj string font)
	      (opal:go-to-next-line text-obj))
	(opal:go-to-end-of-text text-obj)
	(s-value text-obj :deletions nil))
      (let ((label (loop for comp in (get-agg-components win)
			 when (and (eq (car (gv comp :is-a)) (or type window-comment))
				   (or (not position) (eq (gv comp :position) position)))
			 do (return comp))))
	(when label
	  (opal:remove-component win label)
	  (opal:destroy label)))))

(defun find-label-type (label-agg label-type) (loop for comp in (gv label-agg :components) when (eq label-type (car (gv comp :is-a))) do (return comp)))

(defun get-label-agg (win label-type &optional clear-it (where :front))
  ;; Looks for an instance of label-AGG-PROTO in WIN top aggregate, which has a label-type component of type LABEL-TYPE, and
  ;; returns it - if not found, then create one and return it.
  (let ((label-agg (loop for comp in (get-agg-components win)
			 when (and (eq label-agg-proto (car (gv comp :is-a))) (find-label-type comp label-type))
			 do (return comp)))
	label-type-instance)
    (if (and label-agg (not clear-it))
	label-agg
	(progn
	  (when label-agg
	    (opal:remove-component (get-agg win) label-agg)
	    (opal:destroy label-agg nil)) ; Don't want Opal to erase the object
	  (setq label-agg (opal:add-component (get-agg win t) (create-instance nil label-agg-proto (:where where))))
	  (setq label-type-instance (opal:add-component label-agg (create-instance nil label-type)))
	  (opal:add-components label-agg (create-instances (gv label-type-instance :parts)))
	  label-agg))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Window Titles
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun set-window-title (win title &optional USE-TITLE-AS-IS)
  ;; Unless USE-TITLE-AS-IS, TITLE is used as a base for the actual title of WIN, via the function GET-WIN-TITLE-STRING
  (s-value win :title (If USE-TITLE-AS-IS (string title) (GET-win-TITLE-STRING (string title)))))

(defun update-title (win)
  (let ((title (retrieve-window-agg-comp win window-title)))
    (when title (s-value title :label :text (gv win :title)))))

(defun add-window-title-prefix (prefix &optional wins)
  "For each window referenced in WINS [atom or list, default NIL], add PREFIX and a space to the beginning of the title. If WINS not supplied, the
windows are selected from a menu of all output windows."
  (loop for win in (if wins (coerce-to-list wins) (win-menu))
	do (s-value win :title (format nil "~A ~A" (string prefix) (gv win :title)))
	(update-title win)))

(defun add-window-title-suffix (suffix &optional wins)
  "For each window referenced in WINS [atom or list, default NIL], add a space and SUFFIX to the beginning of the title. If WINS not supplied, the
windows are selected from a menu of all output windows."
  (loop for win in (if wins (coerce-to-list wins) (win-menu))
	do (s-value win :title (format nil "~A ~A" (gv win :title)  (string suffix)))
	(update-title win)))

(defun add-title (win &key font (update t) position border)
  (loop for win in (case win
		     (:all (garnet-debug:windows))
		     (t (coerce-to-list win)))
	do
	(unless (eq (gv win :mode) :info)
	  (let ((title (retrieve-window-agg-comp win window-title))
		(title-text (gv win :title)))
	    (when font (s-value win :comment-font font))
	    (cond (position (s-value win :title-position (parse-comment-position position :lower-left)))
		  ((not (gv win :title-position))
		   (s-value win :title-position :lower-left)))
	    (if (and title (= 0 (length title-text)))
	      (s-value title :visible nil)
	      (progn
		(if title
		  (s-value title :label :text title-text)
		  (setq title (add-label win window-title title-text :font (gv win :title-font))))
		(s-value title :visible t)))
	    (when title
	      (s-value title :position (gv win :title-position))
	      (s-value title :borderp border))
	    (when update (opal:update win))))))

(defun remove-title (win) (remove-label win window-title))

(defun edit-title (win)
  (let* ((positions *comment-positions*)
	 (dummy1 (or (gv win :title-position) :lower-left))
	 (dummy2 (gv win :title))
	 dummy3 dummy4 dummy5 dummy6
	 (title (retrieve-window-agg-comp win window-title))
	 (dummy8 (when title (gv title :frame :visible)))
	 dummy9)
    (choose-variable-values
     `((dummy6 ,(if title "Remove visible title from window" "Add visible title to window") :boolean)
       (dummy2 ,(format nil "Window title (displayed version can~%be edited from the window)") :string)
       (dummy1 "Title Position:" :choose ,positions)
       (dummy8 "Include border in title" :boolean)
       ,(when title '(dummy4 "Update actual title with displayed title text" :boolean))
       (dummy3 "CANCEL" :boolean))
     :label (format nil "Edit displayed title of ~s" (gv win :title)))
    (unless dummy3
      (s-value win :title-position dummy1)
      (when dummy6
	(if title
	  (progn (remove-title win) (setq title nil))
	  (progn (add-title win) (setq title (retrieve-window-agg-comp win window-title)))))
      (when title
	(s-value title :frame :visible dummy8)
	(s-value title :position dummy1))
      (s-value win :title (if (and dummy4 title) (gv title :label :string) dummy2)))
    title))

(defun s-value-window-title-visible (win s-value)
  ;; The S-VALUE should be either NIL or T - this is for the aggregate of plotting window WIN.
  (when (get-agg win)
    (loop for comp in (gv win :aggregate :components)
	  when (eq window-title (car (gv comp :is-a))) do (s-value comp :visible s-value))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun initialize-graphics-window (win &key (width *standard-graphics-width*) (height *standard-graphics-height*) erase-temp-comment-Interactor-final-function left top)
  (s-value win :locked *lock-all-windows*)
  ;; (s-value win :left (if width 0 (truncate (/ *screen-width* 2))))
  ;; (s-value win :top (if height 0 *plotting-window-top*))
  (s-value win :left (or left (truncate (/ *screen-width* 2))))
  (s-value win :top (or top *plotting-window-top*))
  (s-value win :width (COERCE-TO-EVEN-INT (or width *standard-graphics-width*)))
  (s-value win :height (COERCE-TO-EVEN-INT (or height *standard-graphics-height*)))
  (s-value (get-agg win t) :window win)
  (add-window-embellishments win erase-temp-comment-Interactor-final-function)
  win)

(defun reorder-top-agg-components (win &optional (where :front))
  (loop for comp in (gv  win :stuff-that-goes-first) do (opal::move-component (get-agg win t) comp :where where)))

(defun resurrect-windows (&optional (windows *output-windows*)) (loop for win in windows do (resurrect-opal-win win)))

(defvar *debug-RESURRECT-OPAL-WIN* nil)

(defun resurrect-opal-win (win &key
			       (reorder-top-agg *reorder-top-agg*)
			       (raise *raise-output-windows*)
			       (visible *show-output-windows*)
			       (invisible *hide-output-windows*) ; Overrides *SHOW-OUTPUT-WINDOWS*, *RAISE-OUTPUT-WINDOWS*.
			       (show *show-output-windows*)
			       (deiconify *deiconify-output-windows*)
			       (update *update-output-windows*))
  (loop for win in (coerce-to-list win) ; In case WIN is not a real window.
	do
	(let ((win (or (gv win :window) win)))
	  (cond-every
	   (reorder-top-agg (reorder-top-agg-components win))
	   (t (adjust-all-labels win))
	   ((or raise deiconify update show visible) (s-value win :visible t))
	   (invisible (s-value win :visible nil))
	   (raise (unless invisible (opal:raise-window win))) ; does update
	   ;; This just sets :visible
	   ;;     (deiconify (opal:deiconify-window win))
	   ;;     (*debug-RESURRECT-OPAL-WIN* (format t "RESURRECT-OPAL-WIN: Ready to update ~A.~%" win))
	   ((and (not raise) update) (opal:update win t))
	   ;;     (*debug-RESURRECT-OPAL-WIN* (format t "RESURRECT-OPAL-WIN: Done with update ~A.~%" win))
	   (*print-and-destroy-output-windows*
	    (print-windows :windows win :what-to-do :print-now)
	    ;; PRINT-WINDOWS can destroy the window.
	    (when (opal-obj-exists win) (clear-window win t))))
	  (when (opal-obj-exists win) win))))

(defun first-interactor-window (interactor)
  (if (consp (gv interactor :window))
    (car (gv interactor :window))
    (gv interactor :window)))

(defun align-to-window-inter-function (interactor final-obj-over)
  (declare (ignore final-obj-over))
  (let ((window (first-interactor-window interactor))
	*automatic-run*)
    (unless (INTERACTORS-RUNNING window)
      (s-value window :print-interactor-running t)
      (when (consp window) (loop for win in window when (gv win :mode) do (setq window win)))
      (align-to-window window)
      ;; PRINT-WINDOWS can destroy the window.
      (when (opal-obj-exists window) (s-value window :print-interactor-running nil)))))

(create-instance 'align-window-Interactor inter:button-Interactor
		 (:continuous nil) (:start-where t)
		 ;; LG fix 01.09.2016 CMUCL 20 reads :CONTROL-LATIN_CAPITAL-G instead of :CONTROL-G
		 ;; (:start-event :control-\a)
		 (:start-event '(:control-\a :CONTROL-LATIN_SMALL_LETTER_A))
		 (:final-function #'ALIGN-TO-WINDOW-INTER-FUNCTION))

(create-instance 'help-window-Interactor inter:button-Interactor
		 (:continuous nil) (:start-where t)
		 ;; LG fix 01.09.2016 CMUCL 20 reads :CONTROL-LATIN_CAPITAL-G instead of :CONTROL-G
		 ;; (:start-event '(#\h #\H :control-\h :control-H))
		 (:start-event '(#\h #\H :control-\h :CONTROL-LATIN_SMALL_LETTER_H :control-\H :CONTROL-LATIN_CAPITAL_LETTER_H))
		 (:final-function nil))

(defun print-window-inter-function (interactor final-obj-over)
  (declare (ignore final-obj-over))
  (let ((window (first-interactor-window interactor))
	*automatic-run*)
    (unless (INTERACTORS-RUNNING window)
      (s-value window :print-interactor-running t)
      (when (consp window) (loop for win in window when (gv win :mode) do (setq window win)))
      (print-windows :windows window :use-menu t)
      ;; PRINT-WINDOWS can destroy the window.
      (when (opal-obj-exists window) (s-value window :print-interactor-running nil)))))

(create-instance 'print-window-Interactor inter:button-Interactor
		 (:continuous nil) (:start-where t)
		 ;; LG fix 01.09.2016 CMUCL 20 reads :CONTROL-LATIN_CAPITAL-G instead of :CONTROL-G
		 ;; (:start-event :control-\p)
		 (:start-event '(:control-\p :CONTROL-LATIN_SMALL_LETTER_P))
		 (:final-function #'print-window-inter-function))

(defun verify-interactor-window (interactor)
  (loop for win in (coerce-to-list (gv interactor :window))
	when (not (opal-obj-exists win)) do (return nil)
	finally (return t)))

(defmacro window-interactor-wrapper (interactor body)
  `(let ((window (first-interactor-window ,interactor))
	 *automatic-run*)
    (unless (INTERACTORS-RUNNING window)
      (s-value window :window-menu-interactor-running t)
      (when (consp window) (loop for win in window when (gv win :mode) do (setq window win)))
      ,body
      (when (opal-obj-exists window) (s-value window :window-menu-interactor-running nil)))))

(export '(window))			; Symbol used in the wrapper macro

(defun toggle-window-lock-inter-function (interactor final-obj-over)
  (declare (ignore final-obj-over))
  (window-interactor-wrapper
   interactor
   (s-value window :locked
	    (not (and (gv window :locked) (go-ahead-menu (format nil "Unlock window ~A?" (gv window :title)) "Authorization" nil))))))

(create-instance 'toggle-window-lock-Interactor inter:button-Interactor
		 (:continuous nil)
		 (:start-where t)
		 ;; LG fix 01.09.2016 CMUCL 20 reads :CONTROL-LATIN_CAPITAL-G instead of :CONTROL-G
		 ;; (:start-event :control-\L)
		 (:start-event '(:control-\L :CONTROL-LATIN_CAPITAL_LETTER_L))
		 (:final-function #'toggle-window-lock-inter-function))

(defun interactors-running (win)
  (or (gv win :destroy-interactor-running)
      (gv win :window-menu-interactor-running)
      (loop for win in (clean-up-*output-windows*) when (gv win :print-interactor-running) do (return t))
      (gv win :comment-interactor-running)))

(create-instance 'resurrect-window-Interactor inter:button-Interactor
		 (:continuous nil) (:start-where t)
		 ;; LG fix 01.09.2016 CMUCL 20 reads :CONTROL-LATIN_CAPITAL-G instead of :CONTROL-G
		 ;; (:start-event :control-\r)
		 (:start-event '(:control-\r :CONTROL-LATIN_SMALL_LETTER_R))
		 (:final-function #'(lambda (interactor final-obj-over)
				      (declare (ignore final-obj-over))
				      (resurrect-opal-win (gv interactor :window) :raise t :visible t :show t :update t :deiconify t))))

(create-instance 'refresh-window-Interactor inter:button-Interactor (:continuous nil) (:start-where t) (:start-event #\R))

;; The :FINAL-FUNCTION for the menu interactor is specific for the various window types.
(create-instance 'window-menu-Interactor inter:button-Interactor (:continuous nil) (:start-where t)
		 ;; LG fix 01.09.2016 CMUCL 20 reads :CONTROL-LATIN_CAPITAL-G instead of :CONTROL-G
		 ;; (:start-event :control-\m)
		 (:start-event '(:control-\m :CONTROL-LATIN_SMALL_LETTER_M)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun destroy-windows-with-menu (&optional reference-window windows-to-destroy)
  (let ((windows-to-destroy (case windows-to-destroy
			      (:all (garnet-debug:windows))
			      (t (clean-up-*output-windows*)))))
    (loop for new-win in (win-menu "Select Windows to Destroy" windows-to-destroy reference-window)
	  when (and (opal-obj-exists new-win)
		    (or (not (gv new-win :locked))
			(let ((dummy10 nil))
			  (choose-variable-values
			   `((dummy10 "Unlock and Destroy" :boolean))
			   :label (format nil "Unlock ~A to Destroy" (gv new-win :title)))
			  dummy1)))
	  do (clear-window new-win)
	  finally (when (opal-obj-exists reference-window) (s-value reference-window :destroy-interactor-running nil)))))

(defun destroy-window-with-menu (win mode)
  (when (opal-obj-exists win)
    (let* ((locked-wins (loop for dummy-win in *output-windows* when (gv dummy-win :locked) sum 1))
	   (unlocked-wins (- (length *output-windows*) locked-wins))
	   (locked-wins-of-mode (loop for dummy-win in (windows-of-mode mode) when (gv dummy-win :locked) sum 1))
	   (unlocked-wins-of-mode (- (length (windows-of-mode mode)) locked-wins-of-mode))
	   (dummy1 (not (menu-p win)))
	   dummy2 dummy4 dummy5)
      (choose-variable-values
       `((dummy1 ,(if (gv win :locked) "Selected window is locked: Unlock and destroy window" "Go ahead and destroy selected window") :boolean)
	 ,(when (> locked-wins 0)
	    `(dummy4 ,(format nil "Unlock all (~D) locked windows (before destroy action below)" locked-wins) :boolean))
	 ,(when (> (length (windows-of-mode mode)) 1)
	    `(dummy2 ,(format nil "Destroy all (currently ~D) non-locked ~a windows" unlocked-wins-of-mode mode) :boolean))
	 ,(when (> (length *output-windows*) (length (windows-of-mode mode)))
	    `(dummy5 ,(format nil "Destroy all (currently ~D) non-locked windows" unlocked-wins) :boolean)))
       :text (format nil "Destroying \"~A\" Window~A" (gv win :title) (if (menu-p win) (format nil"~%** NOTE: This is a menu! Destroy only if necessary! **") ""))
       :label "Destroying Windows")
      (when dummy4 (unlock-all-windows))
      (when (and dummy1 (gv win :locked)) (s-value win :locked nil))
      (s-value win :destroy-interactor-running nil)
      (cond (dummy5 (clear-windows *output-windows*))
	    (dummy2 (clear-windows-of-mode mode))
	    (dummy1 (clear-window win))))))

(defun destroy-window-inter-function (interactor final-obj-over &optional use-all-win-menu)
  (declare (ignore final-obj-over))
  (when (verify-interactor-window interactor)
    (let* ((win (gv interactor :window))
	   (windows-to-destroy (coerce-to-list win))
	   ;; If C-D then dispense with menus and just kill the window.
	   (*automatic-run* (equal (gv INTERACTOR :START-CHAR) :CONTROL-\D))
	   ;; For some reason make sure that only one window associated with interactor is used.
	   (mode (if (consp win)
		   (loop for window in win when (and (opal-obj-exists window) (gv window :mode))
			 do (setq win window) (return (gv window :mode)))
		   (gv win :mode)))
	   (dummy1 t) dummy2 (dummy3 (gv win :locked)) dummy4 dummy5)
      (unless (interactors-running win)
	(s-value win :destroy-interactor-running t)
	(if use-all-win-menu
	  (destroy-windows-with-menu win windows-to-destroy)
	  (destroy-window-with-menu win mode))))))

(create-instance 'destroy-window-Interactor inter:button-Interactor
		 (:continuous nil) (:start-where t)
		 ;; LG fix 01.09.2016 CMUCL 20 reads :CONTROL-LATIN_CAPITAL-G instead of :CONTROL-G
		 ;; 		 (:start-event '(:CONTROL-\d :CONTROL-\D))
		 (:start-event '(:control-\d :CONTROL-\D :CONTROL-LATIN_SMALL_LETTER_D :CONTROL-LATIN_CAPITAL_LETTER_D))
		 (:final-function #'destroy-window-inter-function))

(defun destroy-all-window-inter-function (interactor final-obj-over) (destroy-window-inter-function interactor final-obj-over t))

(create-instance 'destroy-all-window-Interactor inter:button-Interactor
		 (:continuous nil) (:start-where t)
		 ;; LG fix 01.09.2016 CMUCL 20 reads :CONTROL-LATIN_CAPITAL-G instead of :CONTROL-G
		 ;; (:start-event :control-\D)
		 (:start-event '(:CONTROL-\D :CONTROL-LATIN_CAPITAL_LETTER_D))
		 (:final-function #'destroy-all-window-inter-function))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *scroll-window-inter-function-scroll-fraction* 0.80)

(defun scroll-window-inter-function (interactor final-obj-over)
  ;;
  ;; Scroll window by setting :Y-OFFSET or :X-OFFSET slot.
  ;;      Action          Keypress
  ;;----------------------------------
  ;;   Scroll DOWN  -  SPACE or DOWNARROW
  ;;   Scroll UP    -  BACKSPACE or UPARROW
  ;;   Scroll RIGHT -  RIGHTARROW
  ;;   Scroll LEFT  -  LEFTARROW
  ;;
  ;; Scroll amount relative to width or height is given by *SCROLL-WINDOW-INTER-FUNCTION-SCROLL-FRACTION*.
  ;;
  (declare (ignore final-obj-over))
  (when (verify-interactor-window interactor)
    (let ((win (loop for win in (gv interactor :window) when (gv win :scrolling-window) do (return win))))
      (when win
	(let* ((scroll-win (gv win :scrolling-window))
	       (scroll-fraction-width (round (* *SCROLL-WINDOW-INTER-FUNCTION-SCROLL-FRACTION* (gv win :width))))
	       (scroll-fraction-height (round (* *SCROLL-WINDOW-INTER-FUNCTION-SCROLL-FRACTION* (gv win :height)))))
	  (case (gv interactor :START-CHAR)
	    (:rightarrow (when (>= (gv scroll-win :x-offset) (- (gv win :width) (gv scroll-win :total-width)))
			   (s-value scroll-win :x-offset (- (gv scroll-win :x-offset) scroll-fraction-width))))
	    (:leftarrow (when (<= (gv scroll-win :x-offset) 0)
			  (s-value scroll-win :x-offset (min 0 (+ (gv scroll-win :x-offset) scroll-fraction-width)))))
	    ((:downarrow #\Space) (when (>= (gv scroll-win :y-offset) (- (gv win :height) (gv scroll-win :total-height)))
				    (s-value scroll-win :y-offset (- (gv scroll-win :y-offset) scroll-fraction-height))))
	    ((:uparrow #\backspace) (when (<= (gv scroll-win :y-offset) 0)
				      (s-value scroll-win :y-offset (min 0 (+ (gv scroll-win :y-offset) scroll-fraction-height))))))
	  (gg::Set-Scroll-Bar-Values (gv scroll-win )))))))

(create-instance 'scroll-window-Interactor inter:button-Interactor
		 (:continuous nil) (:start-where t)
		 (:start-event '(#\SPACE #\BACKSPACE :LEFTARROW :RIGHTARROW :UPARROW :DOWNARROW))
		 (:final-function #'SCROLL-WINDOW-INTER-FUNCTION))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun window-comment-inter-function (interactor final-obj-over)
  (declare (ignore final-obj-over))
  (let ((win (gv interactor :window)) *automatic-run*)
    (unless (or (consp win) (INTERACTORS-RUNNING win))
      (s-value win :comment-interactor-running t)
      (ADD-COMMENT-MENU win)
      (s-value win :comment-interactor-running nil))))

(create-instance 'window-comment-Interactor inter:button-Interactor
		 (:continuous nil) (:start-where t)
		 ;; LG fix 01.09.2016 CMUCL 20 reads :CONTROL-LATIN_CAPITAL_G instead of :CONTROL-G
		 ;; (:start-event :control-\t)
		 (:start-event '(:control-\t :CONTROL-LATIN_SMALL_LETTER_T))
		 (:final-function #'window-comment-inter-function))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun erase-temp-comment-inter-function (interactor final-obj-over)
  (declare (ignore final-obj-over))
  (let ((win (gv interactor :window))
	*automatic-run*)
    (unless (consp win)
      (add-temp-comment win "" :ignore-reserved t))))

(create-instance 'erase-temp-comment-Interactor inter:button-Interactor
		 (:continuous nil) (:start-where t)
		 ;; LG fix 01.09.2016 CMUCL 20 reads :CONTROL-LATIN_CAPITAL_G instead of :CONTROL-G
		 ;; (:start-event :control-\e)
		 (:start-event '(:control-\e :CONTROL-LATIN_SMALL_LETTER_E))
		 (:final-function #'erase-temp-comment-inter-function))

#|
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Not using the hide-window-interactor....
(defun hide-window-interactor-function (interactor final-obj-over)
  (declare (ignore final-obj-over))
  (resurrect-opal-win (gv interactor :window) :raise t :invisible t :show t :update t :deiconify t))

(create-instance 'hide-window-interactor inter:button-Interactor
		 (:continuous nil) (:start-where t) (:start-event '(#\h #\H))
		 (:final-function #'hide-window-interactor-function))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
|#

;; The following four objects are used for coordinate pointers in graphics windows.

(create-instance 'cross-hair-point opal:circle ; (:fast-redraw-p T)
		 (:draw-function :or) (:radius 3)
		 ; (:filling-style (o-formula (gvl :parent :window :default-graphics-filling-style)))
		 (:line-style ; (o-formula (gvl :parent :line-style))
;		  (o-formula (gvl :parent :window :default-line-style ; :feedback-object-line-style
;				  ))
		  ; opal:line-style
		  )
		 (:left (o-formula (- (the fn (third (gvl :parent :points))) (the fn (gvl :radius)))))
		 (:top (o-formula (- (the fn (fourth (gvl :parent :points))) (the fn (gvl :radius)))))
		 (:width (o-formula (+ (the fn (gvl :radius)) (the fn (gvl :radius)))))
		 (:height (o-formula (+ (the fn (gvl :radius)) (the fn (gvl :radius)))))
		 (:visible (o-formula (gvl :parent :visible))))

(create-instance 'x-cross-hair opal:line ; (:fast-redraw-p T)
		 (:draw-function :xor)
		 (:x1 0) (:y1 (o-formula (fourth (gvl :parent :points))))
		 (:x2 (o-formula (gvl :parent :window :width))) (:y2 (o-formula (fourth (gvl :parent :points))))
		 (:visible (o-formula (gvl :parent :visible)))
		 (:line-style (o-formula (gvl :parent :line-style))))

(create-instance 'y-cross-hair opal:line ; (:fast-redraw-p T)
		 (:draw-function :xor)
		 (:x1 (o-formula (third (gvl :parent :points)))) (:y1 0)
		 (:x2 (o-formula (third (gvl :parent :points)))) (:y2 (o-formula (gvl :parent :window :height)))
		 (:visible (o-formula (gvl :parent :visible)))
		 (:line-style (o-formula (gvl :parent :line-style))))

(create-instance 'cross-hair opal:aggregadget (:visible nil) (:points '(0 0 0 0))
		 (:line-style dashed-line)
		 (:parts `((:cross-hair-point ,cross-hair-point) (:x-cross-hair ,x-cross-hair) (:y-cross-hair ,y-cross-hair))))

(create-instance 'cross-vertical-hair opal:aggregadget (:visible nil) (:points '(0 0 0 0))
		 (:parts `((:y-cross-hair ,y-cross-hair) (:cross-hair-point ,cross-hair-point))))

(create-instance 'cross-horizontal-hair opal:aggregadget (:visible nil) (:points '(0 0 0 0))
		 (:parts `((:x-cross-hair ,x-cross-hair) (:cross-hair-point ,cross-hair-point))))

(create-instance 'cross-hair-w/o-hairs opal:aggregadget (:visible nil) (:points '(0 0 0 0))
		 (:parts `((:cross-hair-point ,cross-hair-point))))

(create-instance 'cross-hair-w/o-point opal:aggregadget (:visible nil) (:points '(0 0 0 0)) (:line-style dashed-line)
		 (:parts `((:x-cross-hair ,x-cross-hair) (:y-cross-hair ,y-cross-hair))))

(create-instance 'window-coords-pointer inter:two-point-Interactor (:start-where t) (:line-p t) (:last-xy nil) (:start-event :middledown))
(create-instance 'window-mark-point inter:button-Interactor (:continuous nil) (:start-where t) (:start-event :control-middledown))

(create-instance 'marker-label opal:aggregadget
		 (:visible nil)
		 (:points '(0 0 0 0))
		 (:parts `((:label ,window-hack-text
				   (:font ,(o-formula (or (gvl :parent :window :marked-points-font)
							  (gvl :parent :window :comment-font)
							  (gvl :parent :window :font)
							  (gvl :parent :window :plot-axis-font)
							  *window-default-font*
							  ;; (opal:get-standard-font :serif :bold-italic :medium)
							  )))
				   (:top ,(o-formula (case (gvl :parent :label-position)
						       ((:left-up :center-up :right-up)
							(- (the fn (fourth (gvl :parent :points)))
							   (+ (round *maker-label-vertical-fudge*) (the fn (gvl :height)))))
						       ((:left-down :center-down :right-down)
							(+ (the fn (fourth (gvl :parent :points)))
							   (round (* 0.5 (+ *maker-label-vertical-fudge* (the fn (gvl :height)))))))
						       (t (- (the fn (fourth (gvl :parent :points)))
							     (round (* 0.5 (the fn (gvl :height)))))))))
				   (:left ,(o-formula
					    (case (gvl :parent :label-position)
					      ((:left-up :left-down :left)
					       (- (the fn (third (gvl :parent :points))) (+ 10 (the fn (gvl :width)))))
					      ((:right-up :right-down :right)
					       (+ 10 (the fn (third (gvl :parent :points)))))
					      ;; Center
					      (t (round (+ *maker-label-horizontal-fudge*
							   (- (the fn (third (gvl :parent :points)))
							      (* 0.5 (the fn (gvl :width)))))))))))))
		 (:interactors `((:text-inter ,inter:text-interactor
					      (:window ,(o-formula (gv-local :self :operates-on :window)))
					      (:feedback-obj nil)
					      (:start-where ,(o-formula (list :in (gvl :operates-on :label))))
					      (:abort-event :CONTROL-\g)
					      (:stop-event (:leftdown))))))

(create-instance 'mark-point-cross opal:aggregadget
		 (:label t)
		 (:v-bar-width 10) (:h-bar-width 10)
		 (:points '(0 0 0 0))
		 (:line-style (o-formula (gvl :parent :window :default-line-style))
			      ;; dashed-line ; (o-formula (gvl :parent :window :default-graphics-line-style))
			      )
		 (:parts `((:cross-h-bar ,opal:line
					 (:visible t) (:line-style ,(o-formula (gvl :parent :line-style)))
					 (:x1 ,(o-formula (- (the fn (third (gvl :parent :points)))
							     (round (* 0.5 (the fn (gvl :parent :h-bar-width)))))))
					 (:x2 ,(o-formula (+ (the fn (third (gvl :parent :points)))
							     (round (* 0.5 (the fn (gvl :parent :h-bar-width)))))))
					 (:y1 ,(o-formula (fourth (gvl :parent :points))))
					 (:y2 ,(o-formula (fourth (gvl :parent :points)))))
			   (:cross-v-bar ,opal:line
					 (:visible t) (:line-style ,(o-formula (gvl :parent :line-style)))
					 (:x1 ,(o-formula (third (gvl :parent :points))))
					 (:x2 ,(o-formula (third (gvl :parent :points))))
					 (:y1 ,(o-formula (- (the fn (fourth (gvl :parent :points)))
							     (round (* 0.5 (the fn (gvl :parent :v-bar-width)))))))
					 (:y2 ,(o-formula (+ (the fn (fourth (gvl :parent :points)))
							     (round (* 0.5 (the fn (gvl :parent :v-bar-width)))))))))))

(create-instance 'mark-point-circle opal:circle (:label t) (:diameter 10) (:points '(0 0 0 0))
		 (:line-style (o-formula (gvl :parent :window :default-line-style))
			      ;; thick-black-line
			      )
		 (:width (o-formula (the fn (gvl :diameter))))
		 (:height (o-formula (the fn (gvl :diameter))))
		 (:left (o-formula (+ (the fn (third (gvl :points))) (round (* -0.5 (the fn (gvl :diameter)))))))
		 (:top (o-formula (+ (the fn (fourth (gvl :points))) (round (* -0.5 (the fn (gvl :diameter))))))))

(create-instance 'mark-point-dot mark-point-circle
		 (:filling-style (o-formula (gvl :parent :window :default-graphics-filling-style)))
		 (:line-style nil))

(create-instance 'mark-point-box opal:rectangle (:label t) (:width 10) (:points '(0 0 0 0))
		 (:line-style (o-formula (gvl :parent :window :default-line-style))
			      ;; thick-black-line
			      )
		 (:height 10)
		 (:left (o-formula (+ (the fn (third (gvl :points))) (round (* -0.5 (the fn (gvl :width)))))))
		 (:top (o-formula (+ (the fn (fourth (gvl :points))) (round (* -0.5 (the fn (gvl :height))))))))

(create-instance 'mark-point-filled-box mark-point-box (:filling-style (o-formula (gvl :parent :window :default-graphics-filling-style))))

(defun update-running-comment (window text)
  (let ((running-comment (gv window :running-comment)))
    ;; (s-value running-comment :label :text text)
    (s-value running-comment :label :string text)
    (s-value running-comment :visible t)))

(defun add-running-comment (window &optional text borderp)
  (let ((running-comment (add-label window window-temp-comment "" :borderp borderp)))
    (s-value running-comment :position (o-formula (or (gvl :parent :window :running-comment-position) *default-running-comment-position*)))
    (when text				; (s-value running-comment :text text)
      (s-value running-comment :string text))
    (s-value running-comment :background-border-width *running-comment-background-border-width*)
    (s-value running-comment :reserved t)
    (s-value running-comment :frame :visible nil)
    (s-value running-comment :visible t)
    (s-value window :running-comment running-comment)))

(defun add-window-coords-pointer (win final-function &optional mark-point-final-function running-function)
  ;; Adds pointer to graphics windows that on middle mouse puts a cross hair on the window which moves with the mouse and
  ;; disappears when the button is released. Calling ADD-WINDOW-COORDS-POINTER includes a function symbol FINAL-FUNCTION that
  ;; specifies what should be done when the coords pointer finishes (e.g. display pointer coordinates).
  (let ((cross-hair (create-instance nil cross-hair-w/o-point)))
    (opal:add-component (get-agg win t) cross-hair)
    (create-instance nil window-coords-pointer (:window win) (:feedback-obj cross-hair) (:running-action running-function) (:final-function final-function))
    (unless (and (gv win :running-comment) (eq win (gv win :running-comment :window))) (add-running-comment win))
    (when mark-point-final-function (create-instance nil window-mark-point (:window win) (:final-function mark-point-final-function))))
  nil)

(defun add-marker-point (win points &key (symbol :cross) (width 10) (height 10))
  (let ((point (case symbol
		 (:box (create-instance nil mark-point-box (:width width) (:height height) (:points points) (:visible t)))
		 (:filled-box (create-instance nil mark-point-filled-box (:width width) (:height height) (:points points) (:visible t)))
		 (:dot (create-instance nil mark-point-dot (:diameter width) (:points points) (:visible t)))
		 (:circle (create-instance nil mark-point-circle (:diameter width) (:points points) (:visible t)))
		 (:cross (create-instance nil mark-point-cross (:points points) (:visible t) (:v-bar-width height) (:h-bar-width width))))))
    (opal:add-component (gv win :aggregate) point)
    point))

(defun add-marker-label (win points &key (label "") (label-position :right))
  (let ((mlabel (create-instance nil marker-label (:points (copy-list points)) (:label-position label-position) (:visible t))))
    (s-value mlabel :label :text label) (s-value mlabel :label :string label)
    (opal:add-component (gv win :aggregate) mlabel)
    mlabel))

(defun add-cross-hair (win points &optional type)
  (let ((cross-hair (create-instance nil (case type
					   (:w/o-hairs cross-hair-w/o-hairs)
					   (:w-vertical-hair cross-vertical-hair)
					   (:w-horizontal-hair cross-horizontal-hair)
					   (t cross-hair))
				     (:visible t)
				     (:points points)))
	(agg (get-agg win t)))
    (s-value cross-hair :cross-hair-point :filling-style opal:black-fill)
    (opal:add-component agg cross-hair)
    ;; (opal:move-component agg (gv cross-hair :cross-hair-point) :where :front)
    cross-hair))

(defun add-marker (win points &key
		   data-x data-y
		   clear-previous-markers
		   (add-point nil add-point-supplied-p)
		   (point-type *default-marker-point-type*) (point-width *default-marker-point-width*)
		   (point-height *default-marker-point-height*)
		   (add-cross-hair nil add-cross-hair-supplied-p) (add-label nil add-label-supplied-p)
		   (add-values-to-label *default-add-values-to-marker-label*)
		   (label-position *default-marker-label-position*) (label "")
		   data-to-points-function)
  (when win
    (when clear-previous-markers (remove-all-markers win))
    (let ((data-to-points-function (or data-to-points-function (gv win :data-to-points-function)))
	  (add-point (if add-point-supplied-p add-point (member :include_point *default-marker-options*)))
	  (add-cross-hair (if add-cross-hair-supplied-p add-cross-hair (member :include_cross_hair *default-marker-options*)))
	  (label (when (if add-label-supplied-p add-label (member :include_label *default-marker-options*)) label))
	  (x-label (if (gv win :event-plotter) "ms" (or (gv win :x-units) (gv win :x-label) "")))
	  (y-label (or (gv win :y-units) (gv win :y-label) "")))
      (when (or points data-to-points-function)
	(let* ((points (or points
			   (flatten-list
			    (list 0 0 (funcall data-to-points-function (or data-x 0.0) (or data-y 0.0) win)))))
	       (a-list (list (cons 'window win) (cons 'data-x data-x) (cons 'data-y data-y))))
	  (setq a-list (acons 'points (copy-list points) a-list))
	  (when label
	    (setq a-list
		  (acons
		   'marker-label
		   (add-marker-label
		    win points
		    :label (get-marker-label-string data-x data-y win x-label y-label label add-values-to-label)
		    :label-position label-position) a-list)))
	  (when add-cross-hair
	    (setq a-list (acons 'cross (add-cross-hair win points add-cross-hair) a-list)))
	  (when add-point
	    (setq a-list (acons 'point-type point-type a-list))
	    (setq a-list (acons 'point-width point-width a-list))
	    (setq a-list (acons 'point-height point-height a-list))
	    (setq a-list (acons 'point (add-marker-point win points :symbol point-type :width point-width :height point-height) a-list)))
	  (when data-to-points-function (setq a-list (acons 'data-to-points-function data-to-points-function a-list)))
	  (push a-list (gv win :markers))
	  nil)))))

(defun add-markers-with-data (win
			      data-x data-y
			      &key
			      clear-previous-markers
			      (add-point nil add-point-supplied-p)
			      (point-type *default-marker-point-type*) (point-width *default-marker-point-width*)
			      (point-height *default-marker-point-height*)
			      (add-cross-hair nil add-cross-hair-supplied-p) (add-label nil add-label-supplied-p)
			      (add-values-to-label *default-add-values-to-marker-label*)
			      (label-position *default-marker-label-position*) (label "")
			      data-to-points-function)
  (when clear-previous-markers (remove-all-markers win))
  (loop for x in data-x
	for y in data-y
	do (add-marker win nil
		       :data-x x :data-y y
		       :add-point add-point
		       :point-type point-type :point-width point-width :point-height point-height
		       :add-cross-hair add-cross-hair :add-label add-label :add-values-to-label add-values-to-label
		       :label-position label-position :label label
		       :data-to-points-function data-to-points-function)))

(defun marker-window (marker) (cdr (assoc 'window marker)))
(defun marker-data-y (marker) (cdr (assoc 'data-y marker)))
(defun marker-data-x (marker) (cdr (assoc 'data-x marker)))
(defun marker-x (marker) (third (marker-points marker)))
(defun marker-y (marker) (fourth (marker-points marker)))
(defun marker-points (marker) (cdr (assoc 'points marker)))
(defun marker-point (marker) (cdr (assoc 'point marker)))
(defun marker-cross (marker) (cdr (assoc 'cross marker)))
(defun marker-label (marker) (cdr (assoc 'marker-label marker)))
;; (defun marker-label (marker)  (cdr (assoc 'label marker)))

(defun marker-point-type (marker &optional value)
  (if value
    (acons 'point-type value marker)
    (or (cdr (assoc 'point-type marker)) (gv (marker-window marker) :point-type) *default-marker-point-type* :cross)))

(defun marker-point-width (marker &optional value)
  (if value
    (acons 'point-width value marker)
    (or (cdr (assoc 'point-width marker)) (gv (marker-window marker) :point-width) *default-marker-point-width* 10)))

(defun marker-point-height (marker &optional value)
  (if value
    (acons 'point-height value marker)
    (or (cdr (assoc 'point-height marker)) (gv (marker-window marker) :point-height) *default-marker-point-height* 10)))

(defun window-marker-description-string (marker)
  (let* ((win (marker-window marker))
	 (x-label (if (gv win :event-plotter) "ms" (or (gv win :x-units) (gv win :x-label) "")))
	 (y-label (or (gv win :y-units) (gv win :y-label) ""))
	 (data-x (marker-data-x marker))
	 (data-y (marker-data-y marker))
	 (name (if (and (marker-label marker) (> (length (gv (marker-label marker) :label :string)) 0))
		 (gv (marker-label marker) :label :string)
		 "Marker")))
    (if (and data-x data-y)
      (format nil "~A @ ~,2f~d, ~,2f~d" name data-x x-label data-y y-label)
      (format nil "~A @ ~,2f~d" name data-x x-label))))

(defun choose-window-markers (win)
  (choose-list-values-from-keys
   (loop for marker in (gv win :markers)
	 collect (list (window-marker-description-string marker) marker))
   (list (window-marker-description-string (car (gv win :markers))))
   :label (format nil "Choose Markers of ~A" (gv win :title))))

(defun edit-marked-points-font (win)
  (s-value win :marked-points-font (font-menu (or (gv win :marked-points-font) (gv win :font)) (format nil "Marked points font for ~A" (gv win :title)))))

(defun get-marker-label-string (data-x data-y win x-label y-label label-text add-values-to-marker-label)
  (let* ((raster (or (eq (gv win :plot-type) 'raster) (eq (gv win :plot-type) :raster)))
	 (data-x-label
	  (format nil "~A~a" (tidy-number-FORMAT data-x :range (when (gv win :x-inc) (* 0.01 (gv win :x-inc)))) x-label))
	 (data-y-label
	  (unless raster
	    (format nil "~A~a" (tidy-number-FORMAT data-y :range (when (gv win :y-inc) (* 0.01 (gv win :y-inc)))) y-label))))
    (format nil "~a~A~a" label-text
	    (if (and (> (length label-text) 0) add-values-to-marker-label) (format nil "~%") "")
	    (if add-values-to-marker-label (if raster (format nil "~a" data-x-label) (format nil "~a, ~a" data-x-label data-y-label)) ""))))

(defun marker-window-x-label (win) (if (gv win :event-plotter) "ms" (or (gv win :x-units) (gv win :x-label) "")))

(defun marker-data-x-label (marker win) (format nil "~A~a" (tidy-number-FORMAT (marker-data-x marker) :range (gv win :x-inc)) (marker-window-x-label win)))

(defun marker-window-y-label (win) (or (gv win :y-units) (gv win :y-label) ""))

(defun marker-data-y-label (marker win) (format nil "~A~a" (tidy-number-FORMAT (marker-data-y marker) :range (gv win :y-inc)) (marker-window-y-label win)))

(defun mark-coords-pointer-menu (win &optional markers)
  (unless (gv win :mark-coords-pointer-menu-active)
    (when (gv win :markers)
      (s-value win :mark-coords-pointer-menu-active t)
      (let ((raster (or (eq (gv win :plot-type) 'raster) (eq (gv win :plot-type) :raster)))
	    (markers (case markers
		       (:all (gv win :markers))
		       ((consp markers) markers)
		       (t (choose-window-markers win))))
	    dummy2 dummy11)
	(loop for marker in markers unless (or dummy2 (string-member "All markers" dummy11)) do
	      (let* ((window-markers-w/o-this-one (remove marker (gv win :markers)))
		     (x-label (marker-window-x-label win))
		     (y-label (marker-window-y-label win))
		     (points (marker-points marker))
		     (point (marker-point marker))
		     (cross (marker-cross marker))
		     (label (marker-label marker))
		     (data-x-label (marker-data-x-label marker win))
		     (data-y-label (unless raster (marker-data-y-label marker win)))
		     (point-data-string (if raster (format nil "~a" data-x-label)	(format nil "~a, ~a" data-x-label data-y-label)))
		     (dummy1 (or (and label ; (gv label :label :text)
				      (gv label :label :string)
				      )
				 ""))	; dummy2 used above
		     dummy3
		     (dummy4 *default-add-values-to-marker-label*)
		     (dummy5 (list (when (or (and cross (not (member :clear-cross *default-marker-options*)))
					     (member :include_cross_hair *default-marker-options*))
				     :include_cross_hair)
				   (when (or (and label (not (member :clear-label *default-marker-options*)))
					     (member :include_label *default-marker-options*))
				     :include_label)
				   (when (or (and point (not (member :clear-point *default-marker-options*)))
					     (member :include_point *default-marker-options*))
				     :include_point)))
		     dummy6
		     (dummy7 (if label (gv label :label-position) (or *default-marker-label-position* :right)))
		     dummy8
		     (dummy13 (marker-data-x marker))
		     (dummy14 (marker-data-y marker))
		     (dummy15 (or (marker-point-type marker) (gv win :point-type) *default-marker-point-type* :cross))
		     (dummy16 (or (marker-point-width marker) (gv win :point-width) *default-marker-point-width* 10))
		     (dummy17 (or (marker-point-height marker) (gv win :point-height) *default-marker-point-height*)))
		(choose-variable-values
		 `((dummy11 "Remove:" :x-choose ("This marker" "All markers") :label-left)
		   (dummy13 ,(format nil "X position~a" (if (> (length x-label) 0) (format nil "[~a]" x-label) "")) :float)
		   ,(unless raster `(dummy14 ,(format nil "Y position~a" (if (> (length y-label) 0) (format nil "[~a]" y-label) "")) :float))
		   (dummy5 "Marker display options:" :x-choose (:include_point :include_cross_hair :include_label) ; :label-left
			   )
		   (dummy1 "Label text" :string)
		   (dummy4 "Add point value to label" :boolean)
		   (dummy15 "Point type:" :choose (:box :filled-box :dot :circle :cross) :rank-margin 5 ; :label-left
			    )
		   (dummy16 "Point width [pixels] (diameter for :DOT and :CIRCLE)" :integer)
		   (dummy17 "Point height [pixels]" :integer)
		   (dummy7 "Label position:" :choose (:left-up :left-down :left :right-up :right-down :right :center-up :center-down :center))
		   (dummy3 "Make above options default" :boolean)
		   (dummy8 "Remove from all markers:" :x-choose ("Labels" "Cross hairs" "Points") :label-left :horizontal)
		   (dummy6 "Edit marked points font" :boolean)
		   (dummy2 "Cancel marker editing" :boolean))
		 :text (format nil "Editing point @ ~A" point-data-string) :label (format nil "Marker Menu ~A" (gv win :title)))
		(unless dummy2
		  (s-value win :point-type dummy15)
		  (s-value win :point-width dummy16)
		  (s-value win :point-height dummy17)
		  (when dummy6 (Edit-marked-points-font win))
		  (when dummy3 (setq *default-add-values-to-marker-label* dummy4
				     *default-marker-label-position* dummy7
				     *default-marker-point-type* dummy15
				     *default-marker-point-width* dummy16
				     *default-marker-point-height* dummy17
				     *default-marker-options* dummy5))
		  (cond ((string-member "All markers" dummy11) (remove-all-markers win))
			((string-member "This marker" dummy11) (remove-marker marker win) (setq marker nil)) ; Remove this marker
			(t (setq marker (marker-point-type marker dummy15))
			   (setq marker (marker-point-width marker dummy16))
			   (setq marker (marker-point-height marker dummy17))
			   (push (cons 'data-x dummy13) marker)
			   (unless raster (push (cons 'data-y dummy14) marker))
			   (when (string-member "Points" dummy8) (remove-all-points win) (setq point nil))
			   (when (string-member "Labels" dummy8) (remove-all-marker-labels win) (setq label nil))
			   (when (string-member "Cross hairs" dummy8) (remove-all-cross-hairs win) (setq cross nil))
			   (if (member :include_cross_hair dummy5)
			     (unless cross (setq marker (acons 'cross (add-cross-hair win points) marker)))
			     (setq marker (remove-cross-from-marker marker win)))
			   (if (member :include_point dummy5)
			     (progn
			       (remove-point-from-marker marker win)
			       (setq marker (acons 'point (add-marker-point win points :symbol dummy15 :width dummy16 :height dummy17) marker)))
			     (setq marker (remove-point-from-marker marker win)))
			   (setq marker (remove-label-from-marker marker win))
			   (when (member :include_label dummy5)
			     (setq marker (acons 'marker-label
						 (add-marker-label
						  win points
						  :label-position dummy7
						  :label (get-marker-label-string dummy13 dummy14 win x-label y-label dummy1 dummy4))
						 marker))))))
		(unless dummy11
		  (s-value win :markers (if marker
					  (progn (refresh-marker-position marker win)
						 (push marker window-markers-w/o-this-one))
					  window-markers-w/o-this-one))))))
      (opal:update win t)
      (s-value win :mark-coords-pointer-menu-active nil))))

(defun refresh-markers-position (win) (loop for marker in (gv win :markers) do (refresh-marker-position marker win)))

(defun refresh-marker-position (marker win)
  (when (assoc 'data-to-points-function marker)
    (let* ((old-points (cdr (assoc 'points marker)))
	   (xy-points (funcall (cdr (assoc 'data-to-points-function marker))
			       (or (marker-data-x marker) 0.0) (or (marker-data-y marker) 0.0) win))
	   (new-points (list 0 0
			     (if (marker-data-x marker) (car xy-points) (third old-points))
			     (if (marker-data-y marker) (cadr xy-points) (fourth old-points)))))
      (push (cons 'points new-points) marker)
      (when (assoc 'cross marker) (s-value (cdr (assoc 'cross marker)) :points new-points))
      (when (assoc 'marker-label marker) (s-value (cdr (assoc 'marker-label marker)) :points new-points))
      (when (assoc 'point marker) (s-value (cdr (assoc 'point marker)) :points new-points)))))

(defun remove-marker (marker win)
  (s-value win :markers (remove marker (gv win :markers)))
  (remove-label-from-marker marker win)
  (remove-point-from-marker marker win)
  (remove-cross-from-marker marker win))

(defun refresh-all-markers (&optional win)
  (let ((*automatic-run* t))
    (loop for win in (or (coerce-to-list win) (clean-up-*output-windows*)) do (mark-coords-pointer-menu win :all))))

(defun remove-all-markers (win &optional update)
  (loop for marker in (gv win :markers) do
	(remove-label-from-marker marker win)
	(remove-point-from-marker marker win)
	(remove-cross-from-marker marker win))
  (s-values-nil win last-pointer-xy markers)
  (when update (opal:update win)))

(defun remove-all-cross-hairs (win &optional update)
  (s-value win :markers (loop for marker in (gv win :markers) collect (remove-cross-from-marker marker win)))
  (when update (opal:update win)))

(defun remove-cross-from-marker (marker win)
  (when marker
    (let ((cross (marker-cross marker)))
      (when cross
	(setq marker (delete (assoc 'cross marker) marker))
	(opal:remove-component (get-agg win t) cross)
	(opal:destroy cross)))
    marker))

(defun remove-all-cross-hairs-inter (interactor final-obj-over)
  (declare (ignore final-obj-over))
  (remove-all-markers (gv interactor :window))
  (REMOVE-ALL-ZOOMERS (gv interactor :window)))

(defun remove-last-zoomer (win)
  (let ((last-zoomer (car (last (gv win :cross-hair-markers)))))
    (when last-zoomer
      (s-value win :cross-hair-markers (butlast (gv win :cross-hair-markers)))
      (opal::destroy last-zoomer))))

(defun remove-last-cross-hair (interactor final-obj-over)
  (declare (ignore final-obj-over))
  (let* ((win (gv interactor :window))
	 (last-marker (car (last (gv win :markers)))))
    (remove-last-zoomer win)
    (s-value win :markers
	     (loop for marker in (gv win :markers) collect
		   (if (eq marker last-marker)
		       (remove-cross-from-marker last-marker win)
		       marker)))))

(defun remove-all-zoomers (win) (loop for zoomer in (gv win :cross-hair-markers) do (remove-first-zoomer win)))

(defun remove-first-zoomer (win)
  (let ((first-zoomer (first (gv win :cross-hair-markers))))
    (when first-zoomer
      (s-value win :cross-hair-markers (rest (gv win :cross-hair-markers)))
      (opal::destroy first-zoomer))))

(defun remove-first-cross-hair (interactor final-obj-over)
  (declare (ignore final-obj-over))
  (let* ((win (gv interactor :window))
	 (first-marker (car (gv win :markers))))
    (remove-first-zoomer win)
    (s-value win :markers
	     (loop for marker in (gv win :markers) collect
		   (if (eq marker first-marker)
		       (remove-cross-from-marker first-marker win)
		       marker)))))

(defun remove-all-marker-labels (win &optional update)
  (s-value win :markers (loop for marker in (gv win :markers) collect (remove-label-from-marker marker win)))
  (when update (opal:update win)))

(defun remove-label-from-marker (marker win)
  (let ((marker-label (marker-label marker)))
    (when marker-label
      (setq marker (delete (assoc 'marker-label marker) marker))
      (opal:remove-component (get-agg win t) marker-label)
      (opal:destroy marker-label)))
  marker)

(defun remove-all-marker-labels-inter (interactor final-obj-over)
  (declare (ignore final-obj-over))
  (remove-all-marker-labels (gv interactor :window)))

(defun remove-all-points (win &optional update)
  (s-value win :markers (loop for marker in (gv win :markers) collect (remove-point-from-marker marker win)))
  (when update (opal:update win)))

(defun remove-point-from-marker (marker win)
  (let ((point (marker-point marker)))
    (when point
      (setq marker (delete (assoc 'point marker) marker))
      (opal:remove-component (get-agg win t) point)
      (opal:destroy point)))
  marker)

(defun remove-all-points-inter (interactor final-obj-over)
  (declare (ignore final-obj-over))
  (remove-all-points (gv interactor :window)))

(defun remove-last-point (interactor final-obj-over)
  (declare (ignore final-obj-over))
  (let* ((win (gv interactor :window))
	 (last-marker (car (last (gv win :markers)))))
    (when last-marker
      (s-value win :markers
	       (loop for marker in (gv win :markers)
		     collect
		     (if (eq marker last-marker)
			 (remove-point-from-marker last-marker win)
			 marker))))))

(defun remove-first-point (interactor final-obj-over)
  (declare (ignore final-obj-over))
  (let* ((win (gv interactor :window))
	 (first-marker (car (gv win :markers))))
    (when first-marker
      (s-value win :markers
	       (loop for marker in (gv win :markers)
		     collect
		     (if (eq marker first-marker)
			 (remove-point-from-marker first-marker win)
			 marker))))))

(create-instance 'remove-first-window-coords-pointer inter:button-Interactor
		 (:continuous nil) (:start-where t)
		 ;; LG fix 01.09.2016 CMUCL 20 reads :CONTROL-LATIN_CAPITAL_G instead of :CONTROL-G
		 ;; (:start-event :control-\f)
		 (:start-event '(:control-\f :CONTROL-LATIN_SMALL_LETTER_F))
		 (:final-function #'remove-first-cross-hair))

(create-instance 'remove-last-window-coords-pointer inter:button-Interactor
		 (:continuous nil) (:start-where t)
		 ;; LG fix 01.09.2016 CMUCL 20 reads :CONTROL-LATIN_CAPITAL_G instead of :CONTROL-G
		 ;; (:start-event :control-\l)
		 (:start-event '(:control-\l :CONTROL-LATIN_SMALL_LETTER_L))
		 (:final-function #'remove-last-cross-hair))

(create-instance 'remove-all-window-coords-pointer inter:button-Interactor
		 (:continuous nil) (:start-where t)
		 ;; LG fix 01.09.2016 CMUCL 20 reads :CONTROL-LATIN_CAPITAL_G instead of :CONTROL-G
		 ;; (:start-event :control-\A)
		 (:start-event '(:control-\A :CONTROL-LATIN_CAPITAL_LETTER_A))
		 (:final-function #'remove-all-cross-hairs-inter))

(create-instance 'zoom-Interactor inter:Two-Point-Interactor (:start-where t) (:line-p NIL) (:Min-width 20) (:Min-height 20))

(create-instance 'zoom-feedback-object opal:rectangle
		 ; (:fast-redraw-p T)
		 (:draw-function :xor)
		 (:left (o-formula (first (gvl :box))))
		 (:top (o-formula (second (gvl :box))))
		 (:width (o-formula (third (gvl :box))))
		 (:height (o-formula (fourth (gvl :box))))
		 (:visible NIL) (:box '(0 0 0 0)) (:line-style opal:dashed-line))

(defun add-zoom-marker (pointer)
  (let* ((box (gv pointer :feedback-obj :box))
	 (zoom-rectangle (create-instance nil opal:rectangle
					  ; (:fast-redraw-p T)
					  (:draw-function :xor)
					  (:left (first box))
					  (:top (second box))
					  (:width (third box))
					  (:height (fourth box))
					  (:visible t) (:line-style opal:dashed-line))))
    (opal:add-component (gv pointer :window :aggregate) zoom-rectangle)
    (push zoom-rectangle (gv pointer :window :cross-hair-markers))))

(defun add-window-zoom (win zoom-function start-event &optional (include-feedback-obj t))
  (create-instance NIL zoom-Interactor (:Window win)
		   (:feedback-obj (when include-feedback-obj
				    (or (loop for comp in (gv win :aggregate :components)
					      when (eq (type-of comp) 'zoom-feedback-object) do (return comp))
					(let ((zoom-feed (create-instance NIL zoom-feedback-object)))
					  (opal:add-component (get-agg win t) zoom-feed)
					  zoom-feed))))
		   (:start-event start-event)
		   (:final-function zoom-function)))

(defun menu-p (win) (gv win :menu-p))

(create-instance 'raise-all-menus-interactor inter:button-Interactor
		 (:continuous nil) (:start-where t)
		 ;; LG fix 01.09.2016 CMUCL 20 reads :CONTROL-LATIN_CAPITAL_G instead of :CONTROL-G
		 ;; (:start-event :control-\q)
		 (:start-event '(:control-\q :CONTROL-LATIN_SMALL_LETTER_Q))
		 (:final-function #'(lambda (interactor final-obj-over)
				      (declare (ignore final-obj-over interactor))
				      (loop for win in (garnet-debug:windows)
					    when (menu-p win)
					    do (resurrect-opal-win win :raise t :visible t :deiconify t :update t)))))

(defun add-window-embellishments (win &optional erase-temp-comment-Interactor-final-function)
  ;; (create-instance nil hide-window-interactor (:Window win))
  ;; All graphics windows get these things.
  (mapcar #'(lambda (interactor) (create-instance NIL interactor (:Window win)))
	  (list help-window-Interactor
		align-window-Interactor
		print-window-Interactor
		destroy-window-Interactor
		destroy-all-window-Interactor
		toggle-window-lock-Interactor
		window-comment-Interactor
		remove-first-window-coords-pointer
		remove-last-window-coords-pointer
		remove-all-window-coords-pointer
		resurrect-window-Interactor
		raise-all-menus-interactor))
  (let ((erase-temp-comment-Interactor (create-instance nil erase-temp-comment-Interactor (:Window win))))
    (when erase-temp-comment-Interactor-final-function (s-value erase-temp-comment-Interactor :final-function erase-temp-comment-Interactor-final-function))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; The CLEAR-WINDOW function family.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun update-*standard-graphics-output* (&optional win)
  ;; Find a replacement for WIN to set *STANDARD-GRAPHICS-OUTPUT*.
  (setq *standard-graphics-output*
	(when win
	  (loop for output-window in *output-windows*
		when (and (not (eq win output-window))
			  (eq (gv win :auxiliary-type) (gv output-window :auxiliary-type))
			  (eq  (gv win :data-type) (gv output-window :data-type))
			  (eq  (gv win :mode) (gv output-window :mode)))
		do (return output-window))))
  ;; (when (member win *colorized-windows*) (push *standard-graphics-output* *colorized-windows*))
  )

(defun update-*standard-info-output* (&optional win)
  ;; Find a replacement for WIN to set *STANDARD-INFO-OUTPUT*.
  (setq *standard-info-output*
	(loop for output-window in *output-windows*
	      when (and (not (eq win output-window))
			(eq (gv output-window :mode) :info)
			(not (gv output-window :help-window)))
	      do (return output-window))))

(defun clear-window (window &optional always)
  "Destroy WINDOW if unlocked, or if ALWAYS is T. Updates
   *OUTPUT-WINDOWS*. WINDOW may be a list of windows."
  (clean-up-*output-windows*)
  (loop for window in (coerce-to-list window) do
       (when (and (opal-obj-exists window)
		  (or always (and (not (gv window :locked)))))
	 (setq *output-windows* (remove window *output-windows*)
	       *colorized-windows* (remove window *colorized-windows*))
	 (cond ((eq window *standard-graphics-output*)
		(update-*standard-graphics-output* window))
	       ((eq window *standard-info-output*)
		(update-*standard-info-output* window)))
	 (when (gv window :wrap-up-function)
	   (funcall (gv window :wrap-up-function) window)))
       (destroy-opal-obj window))
  (update-*twin*)
  nil)

(defun destroy-opal-obj (obj)
  ;; (safe-destroy obj)
   (opal:destroy obj))

(defun clear-windows (wins &optional always) (loop for win in (flatten-list wins) do (clear-window win always)))

(defun clean-windows () (destroy-window-menu))

(defun unstick-windows ()
  "If windows don't respond, maybe they're stuck."
  (clear-interactors-running-flags))

(defun destroy-window-menu (&optional all)
  (clear-interactors-running-flags)
  (mapcar #'(lambda (win) (clear-window win t)) (WINDOW-SELECTION-MENU "Choose windows to Destroy (locked or not)" (if all (garnet-debug:windows) *output-windows*))))

(defun clear-interactors-running-flags ()
  (mapcar #'(lambda (win)
	      (s-values-nil win mark-coords-pointer-menu-active print-interactor-running destroy-interactor-running WINDOW-MENU-INTERACTOR-RUNNING comment-interactor-running))
	  (clean-up-*output-windows*)))

(defun clear-windows-of-mode (mode &optional always)
  (loop for win in (clean-up-*output-windows*) when (eq mode (gv win :mode)) do (clear-window win always))
  (update-*Twin*))

(defun windows-of-mode (modes)
  (let ((modes (coerce-to-list modes)))
    (loop for win in (clean-up-*output-windows*) when (member (gv win :mode) modes) collect win)))

(defun clear-all-output-windows ()
  (loop for win in (clean-up-*output-windows*) do (clear-window win t))
  (update-*Twin*))

(defun caows ()
  "Clear all output windows. Does not kill menus (use MDW)."
  (clear-all-output-windows))

(defun caulws ()
  "Clear all unlocked output windows. Does not kill menus (use MDW)."
  (loop for win in (clean-up-*output-windows*) unless (gv win :locked) do (clear-window win t))
  (update-*Twin*))

(defun update-*Twin* () (setq *twin* (car (clean-up-*output-windows*))))

(defun lock-window (&optional (win *twin*))
  "Make sure that WIN (can be a list) is not overwritten. If :ALL then all windows locked."
  (mapcar #'(lambda (win) (s-value win :locked t))
	  (case win (:all (clean-up-*output-windows*))
		(t (flatten-list win)))))

(defun lock-windows (&optional (wins :all))
  "Make sure that none of the output windows are overwritten."
  (lock-window wins))

(defun lock-all-windows () (mapcar #'(lambda (win) (s-value win :locked t)) (clean-up-*output-windows*)))

(defun lock-new-windows (old-windows) (mapcar #'(lambda (win) (unless (member win old-windows) (lock-window win))) *output-windows*))

(defun unlock-window (&optional (win *twin*))
  "Allow overwriting WIN (can be a list). If :ALL then all windows unlocked."
  (unstick-windows)
  (loop for win in (case win
		     (:all *output-windows*)
		     (t (flatten-list win)))
	do (s-value win :locked nil)))

(defun unlock-windows (&optional (wins :all)) (unlock-window wins))

(defun unlock-all-windows ()
  "If windows don't respond, maybe they're stuck. Also unlock them all."
  (unstick-windows)
  (mapcar #'(lambda (win) (s-value win :locked nil)) (clean-up-*output-windows*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-top-agg-comp (type &optional (win *standard-graphics-output*))
  (when (opal-obj-exists win) (loop for comp in (gv win :aggregate :components) when (eq type (gv comp :type)) do (return comp))))

(defun destroy-top-agg (agg parent)
  ;; Performance hint from Garnet v2.1 Manual p507
  (when agg
    (let ((temp-agg agg))
      (s-value parent :aggregate nil)
      (opal:update parent)
      (if (and (not (and (listp (gv temp-agg :constant))
			 (member :components (gv temp-agg :constant))))
	       (gv temp-agg :components))
	  (progn
	    (destroy-agg-object temp-agg)
	    (s-value parent :aggregate temp-agg))
	  (opal:destroy temp-agg)))))

(defun destroy-agg-object (object)	; some things don't like to have their components killed first, so get the parent.
  (if (and (not (and (listp (gv object :constant))
		     (member :components (gv object :constant))))
	   (gv object :components))
      (progn
	(loop for comp in (gv object :components)
	      when (gv comp :components)
	      do (destroy-agg-object comp))
	(dolist (object (gv object :components))
	  (destroy-agg-object object)))
      (opal:destroy object)))

(create-instance 'PLOT-AGG-PROTO opal:aggregate
		 ;; Performance hint [GRM v2.1 p.166]
		 (:top (o-formula (gvl :window :top)))
		 (:left (o-formula (gvl :window :left)))
		 (:width (o-formula (gvl :window :width)))
		 (:height (o-formula (gvl :window :height))))

(defun add-plot-agg (win type &optional (where :back))
  ;; Adds an instance of PLOT-AGG-PROTO of type TYPE to WIN's top aggregate and returns it.
  (opal:add-component (get-agg win t) (create-instance nil PLOT-AGG-PROTO (:type type)) :where where))

(defun find-plot-agg (win type)
  ;; Find and return from WIN all instances of a PLOT-AGG-PROTO whose :type is TYPE. This should really be named find-plot-aggs.
  (find-plot-aggs win type))

(defun find-plot-aggs (win type)
  (loop for comp in (gv win :aggregate :components) when (and (eq PLOT-AGG-PROTO (car (gv comp :is-a))) (eq type (gv comp :type))) collect comp))

(defun clear-plot-aggs (win type)
  ;; Remove and destroy all instances of PLOT-AGG-PROTO of type TYPE in WIN.
  (unless (eq (gv win :mode) :info)
    (let ((plot-aggs (find-plot-agg win type)))
      (when plot-aggs
	(let ((temp-agg (gv win :aggregate)))
	  (s-value win :aggregate nil)
	  (opal:update win)
	  (loop for plot-agg in plot-aggs do
		(opal:destroy plot-agg)
		(s-value win :aggregate temp-agg)))))))

(defun clear-window-aggregate-components (win comps)
  (loop for comp in (coerce-to-list comps) do (opal:remove-component (gv win :aggregate) comp) (opal:destroy comp)))

(defun clear-plot-aggs (win type)
  (loop for plot-agg in (find-plot-agg win type) do (opal:remove-component (gv win :aggregate) plot-agg) (opal:destroy plot-agg))
  nil)

(defun efficient-clear-agg-components (agg)
  (loop for comp in (gv agg :components)
	do (if nil ; (gv comp :components)
	       (efficient-clear-agg-components comp)
	       (opal:destroy comp)))
  (opal:destroy agg))

(defun clear-and-add-plot-agg (win type &key (where :back) add (clear t))
  ;; Clears any plot-agg's of type TYPE from WIN's top aggregate, then adds and returns a new plot-agg of type TYPE.
  (when clear (clear-plot-aggs win type))
  (when add (add-plot-agg win type where)))

(defun get-plot-agg (win type &optional clear-it (where :back))
  ;; Looks for an instance of PLOT-AGG-PROTO in WIN top aggregate, type TYPE, and returns it - if not found, then
  ;; create one and return it.
  (when clear-it (clear-plot-aggs win type))
  (if (find-plot-agg win type) (car (find-plot-agg win type)) (add-plot-agg win type where)))

#|
(defun renew-plot-agg (win type &optional (where :back))
  (when (gv win :aggregate)
    (loop for comp in (gv win :aggregate :components)
	  when (eql type comp))))
|#

(defun get-true-alternate-title-for-wins (base-title alternate-count current-titles-and-wins)
  (let ((alternate-title (if (zerop alternate-count) base-title (format nil "~A (alternate ~D)" base-title alternate-count))))
    (if (member alternate-title current-titles-and-wins :test 'string= :key 'car)
      (get-true-alternate-title-for-wins base-title (incf alternate-count) current-titles-and-wins)
      alternate-title)))

(defun list-of-unique-titles-and-windows (&optional (windows *output-windows*))
  (let (current-titles-and-wins)
      (loop for win in windows
	    do (push (list (get-true-alternate-title-for-wins (gv win :title) 0 current-titles-and-wins) win)
		     current-titles-and-wins)
	    finally (return current-titles-and-wins))))

(defun window-selection-menu (&optional (title "Select Windows") windows pre-selected-window-list only-one)
  (choose-list-values-from-keys
   (reverse (list-of-unique-titles-and-windows (flatten-list (or windows (clean-up-*output-windows*)))))
   (loop for win in (no-nils (flatten-list (case pre-selected-window-list
				    (:all *output-windows*)
				    (t pre-selected-window-list))))
	 collect (gv win :title))
   :label title :max-per-menu 30 :punt-if-only-one-entry nil :only-one-choice only-one))

(defun win-menu (&optional (title "Select Windows") (windows (clean-up-*output-windows*)) pre-selected-window-list only-one)
  "Returns a list of output windows selected with a menu."
  (window-selection-menu title windows pre-selected-window-list only-one))

(defun mouse-destroy-window ()
  "Destroy any window selected by the mouse."
  (clear-window (id-win "Click or Type (perhaps twice!) on any object or window to destroy...")))

(setf (symbol-function 'mdw) (symbol-function 'mouse-destroy-window)) ; Backward compatibility

(defun Id-win (&optional (prompt-string "Click or Type on any object or window..."))
  "Identifies a window selected by the mouse."
  (format t prompt-string) (format t "~%")
  (nth 1 (garnet-debug::ident nil)))

(setf (symbol-function 'identify-window) (symbol-function 'Id-win))
(setf (symbol-function 'click-window) (symbol-function 'Id-win))

(defun window-titles () (mapcar #'(lambda (win) (format t "~A~%" (gv win :title))) *output-windows*) nil)

;; ******************************************
;; Time and Date functions.
;; ******************************************

(defun print-universal-date (&optional (stream t))
  (multiple-value-bind (ss mm hh d m y dow dst zone) (decode-universal-time (get-universal-time))
 (format stream "~4,'0D-~2,'0D-~2,'0D" y m d)))

(defun print-universal-time (&optional (stream t))
  (multiple-value-bind (ss mm hh d m y dow dst zone) (decode-universal-time (get-universal-time))
    (format stream "~2,'0D:~2,'0D:~2,'0D" hh mm ss)))

(defun encode-time-stamp ()
  ;; The time stamp changes every 10 seconds. The function DECODE-UNIVERSAL-TIME will decode the
  ;; *time-stamp* component of a simulation name if the *time-stamp* component is multiplied by 10.
  (let ((new-actual-time-stamp (round (/ (- (get-universal-time) *universal-time-conversion-factor*)
					 1d1)))) ; double precision since get-universal-time returns a 10-digit integer.
    (setq *time-stamp-suffix* (if (= *actual-time-stamp* new-actual-time-stamp)
				(1+ *time-stamp-suffix*)
				1))
    (setq  *actual-time-stamp* new-actual-time-stamp
	   *time-stamp* (if (= 1 *time-stamp-suffix*)
			  (format nil "~d" new-actual-time-stamp)
			  (format nil "~d+~d" new-actual-time-stamp  *time-stamp-suffix*)))))

(defun decode-time-stamp (&optional (time-stamp *actual-time-stamp*))
  (format-universal-time
   t
   (+ (truncate (* 10 time-stamp)) *universal-time-conversion-factor*))
  (unless (= 1 *time-stamp-suffix*)
    (format t " (~:r in this 10 second period)" *time-stamp-suffix*)))

(defun print-date (&optional (stream t) include-tod)
  "Print out the date, and if INCLUDE-TOD is T [default NIL] precede with the time of day. If STREAM is NIL, then return the
time/date string. If STREAM is T [default], then print out to standard output."
  (let (second minute hour date month year day daylight zone)
    (multiple-value-setq (second minute hour date month year day daylight zone)
      (get-decoded-time))
    (format stream
	    "~:[~2*~;~2,'0D:~2,'0D ~]~a ~2,'0D ~D"
	    include-tod hour minute
	    (nth (1- month) '("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))
	    date year)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Arranging Windows
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun reassociate-windows (windows sublist-length)
  (if (zerop sublist-length)
    windows
    (let (out)
      (loop for win-num from 0 to (1- sublist-length) do
	    (loop for simulation in (split-up windows sublist-length)
		  unless (> win-num (1- (length simulation)))
		  do (push (elt simulation win-num) out)
		  finally (return out)))
      (reverse out))))

(defvar *reassociate-windows* nil "Retiling of windows changes the window list order using REASSOCIATE-WINDOWS.")
(defvar *reassociate-windows-sublist-length* 0 "Association length for REASSOCIATE-WINDOWS.")

(defun arrange-windows (&key (windows-per-row *arrange-windows-per-row*) windows use-menu
			  (reassociate-windows *reassociate-windows*)
			  (reassociate-windows-sublist-length *reassociate-windows-sublist-length*)
			  (start-x *window-tile-start-x*) (start-y *window-tile-start-y*)	; pixels from top left corner
			  ignore-window-width ignore-window-height
			  (window-tile-x-gap *window-tile-x-gap*) (window-tile-y-gap *window-tile-y-gap*))
  "Retiles the WINDOWS according to WINDOWS-PER-ROW [default *ARRANGE-WINDOWS-PER-ROW*].
If WINS is NIL, or if USE-MENU is T, then a window menu will choose the retiled windows. If WINS is :ALL, then all *OUTPUT-WINDOWS* will be arranged.
Tiling references the largest width and height over all windows. The starting position [upper left corner of window] of the tiling is given by START-X
and START-Y, pixels [default 0 for both, thus the top left corner of the screen]. The spacing between windows is given by WINDOW-TILE-X-GAP and
WINDOW-TILE-X-GAP. If IGNORE-WINDOW-WIDTH or IGNORE-WINDOW-HEIGHT is T, then these spacing parameter apply to the left and top edges of the successive
windows, respectively."
  (let* ((wins (if (eq :all windows)
		   (clean-up-*output-windows*)
		   (flatten-list (if (or use-menu (not windows)) (win-menu "Select Windows to Arrange" windows) windows))))
	 (num-wins (length wins))
	 (maximum-window-width (loop for win in wins maximizing (gv win :width)))
	 (maximum-window-height (loop for win in wins maximizing (gv win :height))))
    (setq *window-tile-x-gap* window-tile-x-gap
	  *window-tile-y-gap* window-tile-y-gap
	  *window-tile-start-x* start-x
	  *window-tile-start-y* start-y
	  *arrange-windows-per-row* (min windows-per-row num-wins)
	  *reassociate-windows* reassociate-windows
	  *reassociate-windows-sublist-length* reassociate-windows-sublist-length)
    (when (and wins use-menu)
      (let ((dummy1 ignore-window-width)
	    (dummy2 ignore-window-height))
	(choose-variable-values
	 `((:comment ,(format nil "There are ~A selected windows" num-wins))
	   (*window-tile-x-gap* "Window tiling horizontal gap (pixels)" :integer)
	   (*window-tile-y-gap* "Window tiling vertical gap (pixels)" :integer)
	   (*window-tile-start-x* "Window tiling horizontal start point (pixels)" :integer)
	   (*window-tile-start-y* "Window tiling vertical start point (pixels)" :integer)
	   (*arrange-windows-per-row* "Number of windows per row (max)" :integer)
	   (dummy1 "Ignore window widths" :boolean)
	   (dummy2 "Ignore window-heights" :boolean)
	   (*reassociate-windows* "Reassociate windows" :boolean)
	   (*reassociate-windows-sublist-length* "Reassociation sublist length" :integer))
	 :label "Format for Arranging Windows")
	(setq ignore-window-width dummy1
	      ignore-window-height dummy2)
	(printvars dummy1 dummy2 ignore-window-width ignore-window-height))
      (printvars ignore-window-width ignore-window-height)
      (when *reassociate-windows* (setq wins (reassociate-windows wins *reassociate-windows-sublist-length*)))
      (let ((x *window-tile-start-x*)
	    (y *window-tile-start-y*)
	    (row-count 0)
	    (column-count 0)
	    last-win-height)
	(loop for win in wins
	   do (setq row-count (1+ row-count))
	     (s-values win (left x) (top y))
	     (printvars win x y IGNORE-WINDOW-WIDTH)
	     (setq last-win-height (gv win :height))
	     (resurrect-opal-win win)	; (opal:update win t)
	     (if (= row-count *arrange-windows-per-row*)
		 (setq row-count 0
		       x start-x
		       y (+ y
			    (if IGNORE-WINDOW-height 0 (if (= *arrange-windows-per-row* 1) last-win-height maximum-window-height))
			    *window-tile-y-gap* *window-tile-fudge-factor*)
		       column-count (1+ column-count))
		 (setq x (+ x (if IGNORE-WINDOW-WIDTH 0 maximum-window-width) *window-tile-x-gap* *window-tile-fudge-factor*)))))
      wins)))

(defun move-window (win left top)
  (s-values win (left (round left)) (top (round top)))
  (opal:update win))

(defun resize-windows (width height &optional windows font add-titles)
  "Resize with WIDTH and HEIGHT [pixels] all WINDOWS, if supplied, otherwise as selected from menu."
  (let ((windows (cond ((eq windows :all) *output-windows*)
		       (windows windows)
		       (t (win-menu (format nil "Select Windows to resize width to ~A pixels and height to ~A pixels" width height) )))))
    (loop for win in (coerce-to-list windows) do (s-value win :width width) (s-value win :height height)
	  when (gv win :resize-window-refresh-function)
	  do (progn
	       (when font (s-value win :comment-font font) (s-value win :plot-axis-font font))
	       (when add-titles (add-titles-to-those-without win))
	       (funcall (gv win :resize-window-refresh-function) win)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun visible-windows ()
  (loop for win in (garnet-debug:windows)
	when (and (gv win :visible)
		  (not (eq (gv win :visible) :iconified)))
	collect win))

(defun add-titles-to-those-without (windows)
  (loop for win in (coerce-to-list windows)
	unless (or (retrieve-window-agg-comp win window-title)
		   (eq :keys (gv win :auxiliary-type)))
	do (add-title win) and collect win))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(export '(WINDOWP
	  CREATE-SCROLLING-DISPLAY-WINDOW
	  FIX-WINDOW-SIZE
	  raise-all-menus-interactor
	  basic-graphics-window
	  component-top component-left comp-top comp-left
	  *background-border-width* *running-comment-background-border-width*
	  UPDATE-*STANDARD-INFO-OUTPUT* UPDATE-*STANDARD-GRAPHICS-OUTPUT*
	  match-win-dimensions-menu MATCH-WIN-DIMENSIONS
	  add-running-comment
	  update-running-comment
	  graphics-text-font
	  graphics-text
	  graphics-text-linestyle
	  default-graphics-color
	  window-hack-text
	  schema-half-height
	  schema-half-height-fn
	  schema-half-width
	  schema-half-width-fn
	  safe-destroy
	  *GLOBAL-WINDOW-TITLE-SUFFIX*
	  clean-up-*output-windows*
	  opal-obj-exists
	  *lock-all-windows*
	  *default-comment-position*
	  *comment-positions*
	  *DEFAULT-RUNNING-COMMENT-POSITION*
	  *maker-label-vertical-fudge* *maker-label-horizontal-fudge*
	  *print-and-destroy-output-windows*
	  *TWIN*
	  *universal-time-conversion-factor* *time-stamp* *actual-time-stamp*
	  PRINT-UNIVERSAL-DATE PRINT-UNIVERSAL-time
	  TRANSFER-SCHEMA-SLOTS
	  window-interactor-wrapper
	  toggle-window-lock-Interactor
	  initialize-window-system-variables
	  *DISPLAYED-HOST-NAME*
	  *add-host-name-to-windows*
	  *always-add-host-name-to-windows*
	  GET-win-TITLE-STRING
	  HOST-IS-DISPLAY-SERVER
	  encode-time-stamp decode-time-stamp
	  create-path

	  PRINT-DATE
	  interactors-running clear-interactors-running-flags
	  update-*Twin* CLEAR-WINDOWS help-window-Interactor  info top-scroll
	  find-comment clear-windows-of-mode ADD-WINDOW-COORDS-POINTER
	  clear-window add-comment add-temp-comment
	  REMOVE-COMMENT
	  resurrect-opal-win initialize-graphics-window
	  x-y-in-win
	  x-y-in-win-values
	  add-window-zoom clear-all-output-windows  caows CAULWS adjust-all-labels add-label
	  window-menu-interactor add-title remove-title
	  ADD-WINDOW-TITLE-SUFFIX ADD-WINDOW-TITLE-PREFIX
	  remove-all-cross-hairs
	  CLEAR-WINDOW-AGGREGATE-COMPONENTs
	  FIND-PLOT-AGGS find-plot-agg
	  clear-and-add-plot-agg destroy-agg clear-plot-aggs add-plot-agg get-plot-agg

	  GET-TOP-AGG-COMP get-agg
	  *screen-width* *screen-height*
	  *standard-graphics-width* *standard-graphics-height*
	  *raise-output-windows* *deiconify-output-windows*
	  *show-output-windows* *plotting-window-top*
	  *update-output-windows*
	  *hide-output-windows*
	  *STANDARD-GRAPHICS-OUTPUT* *STANDARD-INFO-OUTPUT*
	  *colorized-windows*
	  *COMMENT-FONT*	  SET-*COMMENT-FONT*-MENU
	  *automatic-run*
	  *output-windows* *omit-title-bar* *window-manager-title-border-height*
	  *use-*plot-directory*
	  *plot-directory*
	  *DATA-DIRECTORY*
	  *plot-code-directory*
	  *PLOT-COMMENT*
	  RESURRECT-WINDOWS
	  ADD-COMMENT-MENU
	  strip-displayed-host-name-from-title
	  WINDOW-SELECTION-MENU
	  WIN-MENU
	  LIST-OF-UNIQUE-TITLES-AND-WINDOWS
	  destroy-window-menu
	  destroy-windows-with-menu
	  UNLOCK-ALL-WINDOWS
	  unlock-windows
	  LOCK-ALL-WINDOWS
	  lock-windows
	  lock-new-windows
	  GET-WINDOW-COMMENT get-window-comment-text
	  WINDOW-COMMENT
	  window-temp-comment
	  GET-TOP-LEVEL-THING WINDOWS-OF-MODE

	  mark-point-cross
	  mark-point-dot
	  mark-point-circle
	  MOVE-WINDOW
	  resize-windows
	  Y-GRAPHICS-WIN
	  x-GRAPHICS-WIN
	  ADD-MARKER
	  ADD-MARKERS-WITH-DATA
	  remove-all-markers
	  mark-coords-pointer-menu
	  edit-marked-points-font
	  cross points data-x data-y point marker-label

	  marker-data-y marker-data-x marker-points marker-point marker-cross marker-label
	  marker-x  marker-y
	  MARKER-POINT-HEIGHT MARKER-POINT-TYPE MARKER-POINT-WIDTH
	  REMOVE-MARKER

	  data-to-points-function
	  REFRESH-MARKER-POSITION REFRESH-MARKERs-POSITION
	  Id-win IDENTIFY-WINDOW click-window mouse-destroy-window mdw unlock-window lock-window clean-windows UNSTICK-WINDOWS ARRANGE-WINDOWS
	  window-titles set-window-title
	  *reassociate-windows*
	  *reassociate-windows-sublist-length*
	  hide-window-interactor-function refresh-window-Interactor
	  visible-windows
	  *window-tile-fudge-factor*
	  *WINDOW-TILE-x-GAP*
	  *WINDOW-TILE-Y-GAP*
	  *window-tile-start-x* *window-tile-start-y*
	  *DEFAULT-ALIGN-TO-WINDOW-SPECIFICATION*
	  ALIGN-TO-WINDOW
	  *arrange-windows-per-row*
	  *default-marker-label-position*
	  *default-marker-point-type*
	  *default-marker-point-width*
	   *default-marker-point-height*
	   *default-marker-options*
	  *DEFAULT-ADD-VALUES-TO-MARKER-LABEL*
	  *default-graphics-window-background-color*
	  *num-graphics-windows-rows* *num-graphics-windows-columns* setup-plot-tiling
	  ))


;;; Note all the below code is taking straight from CMUCL.  Surely
;;; there's convenient maintained library to do all this?

(defparameter abbrev-weekday-table
  '#("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun"))

(defparameter long-weekday-table
  '#("Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday"
     "Sunday"))

(defparameter abbrev-month-table
  '#("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov"
     "Dec"))

(defparameter long-month-table
  '#("January" "February" "March" "April" "May" "June" "July" "August"
     "September" "October" "November" "December"))

;;; The timezone-table is incomplete but workable.

(defparameter timezone-table
  '#("GMT" "" "" "" "" "EST" "CST" "MST" "PST"))

(defparameter daylight-table
  '#(nil nil nil nil nil "EDT" "CDT" "MDT" "PDT"))

(defun format-universal-time (destination universal-time
					  &key (timezone nil)
					  (style :short)
					  (date-first t)
					  (print-seconds t)
					  (print-meridian t)
					  (print-timezone t)
					  (print-weekday t))
  "Format-Universal-Time formats a string containing the time and date
   given by universal-time in a common manner.  The destination is any
   destination which can be accepted by the Format function.  The
   timezone keyword is an integer specifying hours west of Greenwich.
   The style keyword can be :short (numeric date), :long (months and
   weekdays expressed as words), :abbreviated (like :long but words
   are abbreviated), :rfc1123 (conforming to RFC 1123), :government
   (of the form \"XX Mon XX XX:XX:XX\"), or :iso8601 (conforming to
   ISO 8601), which is the recommended way of printing date and time.
   The keyword date-first, if nil, will print the time first instead of
   the date (the default).  The print- keywords, if nil, inhibit the
   printing of the obvious part of the time/date."
  (unless (valid-destination-p destination)
    (error "~A: Not a valid format destination." destination))
  (unless (integerp universal-time)
    (error  "~A: Universal-Time should be an integer." universal-time))
  (when timezone
    (unless (and (rationalp timezone) (<= -24 timezone 24))
      (error "~A: Timezone should be a rational between -24 and 24." timezone))
    (unless (zerop (rem timezone 1/3600))
      (error "~A: Timezone is not a second (1/3600) multiple." timezone)))
  (multiple-value-bind (secs mins hours day month year dow dst tz)
		       (if timezone
			   (decode-universal-time universal-time timezone)
			   (decode-universal-time universal-time))
    (declare (fixnum secs mins hours day month year dow))
    (let ((time-string "~2,'0D:~2,'0D")
	  (date-string
	   (case style
	     (:short "~D/~D/~2,'0D")			;;  MM/DD/YY
	     ((:abbreviated :long) "~A ~D, ~D")		;;  Month DD, YYYY
	     (:rfc1123 "~2,'0D ~A ~4,'0D")		;;  DD Mon YYYY
	     (:government "~2,'0D ~:@(~A~) ~2,'0D")	;;  DD MON YY
	     (:iso8601 "~4,'0D-~2,'0D-~2,'0D")          ;;  YYYY-MM-DD
	     (t
	      (error "~A: Unrecognized :style keyword value." style))))
	  (time-args
	   (case style
	     ((:rfc1123 :iso8601) (list mins hours))
	     (t (list mins (max (mod hours 12) (1+ (mod (1- hours) 12)))))))
	  (date-args (case style
		       (:short
			(list month day (mod year 100)))
		       (:abbreviated
			(list (svref abbrev-month-table (1- month)) day year))
		       (:long
			(list (svref long-month-table (1- month)) day year))
		       (:rfc1123
			(list day (svref abbrev-month-table (1- month)) year))
		       (:government
			(list day (svref abbrev-month-table (1- month))
			      (mod year 100)))
		       (:iso8601
			(list year month day))))
	  (timezone-name (case style
			   (:rfc1123 (timezone-rfc1123-name dst tz))
			   (:iso8601 (timezone-iso8601-name dst tz))
			   (t (timezone-name dst tz)))))
      (declare (simple-string time-string date-string timezone-name))
      (when print-weekday
	(push (case style
		((:short :long) (aref long-weekday-table dow))
		((:abbreviated :rfc1123 :government :iso8601)
		 (svref abbrev-weekday-table dow)))
	      date-args)
	(setq date-string
	      (concatenate 'simple-string "~A, " date-string)))
      (when (or print-seconds (eq style :government))
	(push secs time-args)
	(setq time-string
	      (concatenate 'simple-string time-string ":~2,'0D")))
      (when (and print-meridian (not (member style '(:rfc1123 :iso8601))))
	(push (signum (floor hours 12)) time-args)
	(setq time-string
	      (concatenate 'simple-string time-string " ~[am~;pm~]")))
      (apply #'format destination
	     (if (or date-first (eq style :iso8601))
		 (concatenate 'simple-string date-string " " time-string
			      (when print-timezone
				(if (eq style :iso8601)
				    "~A"
				    " ~A")))
		 (concatenate 'simple-string time-string " " date-string
			      (if print-timezone " ~A")))
	     (if (or date-first (eq style :iso8601))
		 (nconc date-args (nreverse time-args)
			(if print-timezone
			    (list timezone-name)))
		 (nconc (nreverse time-args) date-args
			(if print-timezone
			    (list timezone-name))))))))

;;; ISO 8601 style timezone: Z, +1000, -1000.  Timezone is the
;;; negative of the CL timezone.
(defun timezone-iso8601-name (dst tz)
  (let ((tz (- tz)))
    (if (and (not dst) (= tz 0))
	"Z"
	(multiple-value-bind (hours minutes)
	    (truncate (if dst (1+ tz) tz))
	  (format nil "~C~2,'0D:~2,'0D"
		  (if (minusp tz) #\- #\+)
		  (abs hours)
		  (abs (truncate (* minutes 60))))))))

;;; RFC 1123 style timezone: GMT, +1000, -1000.
;;; Timezone is the negative of the CL timezone.
(defun timezone-rfc1123-name (dst tz)
  (let ((tz (- tz)))
    (if (and (integerp tz)
	     (or (and (not dst) (= tz 0))
		 (<= 5 tz 8)))
	(svref (if dst daylight-table timezone-table) tz)
	(multiple-value-bind
	      (hours minutes)
	    (truncate (if dst (1+ tz) tz))
	  (format nil "~C~2,'0D~2,'0D"
		  (if (minusp tz) #\- #\+)
		  (abs hours)
		  (abs (truncate (* minutes 60))))))))

(defun timezone-name (dst tz)
  (if (and (integerp tz)
	   (or (and (not dst) (= tz 0))
	       (<= 5 tz 8)))
      (svref (if dst daylight-table timezone-table) tz)
      (multiple-value-bind
	  (rest seconds)
	  (truncate (* (if dst (1- tz) tz) 60 60) 60)
	(multiple-value-bind
	    (hours minutes)
	    (truncate rest 60)
	  (format nil "[~C~D~@[~*:~2,'0D~@[~*:~2,'0D~]~]]"
		  (if (minusp tz) #\- #\+)
		  (abs hours)
		  (not (and (zerop minutes) (zerop seconds)))
		  (abs minutes)
		  (not (zerop seconds))
		  (abs seconds))))))

;;; Valid-Destination-P ensures the destination stream is okay
;;; for the Format function.
(defun valid-destination-p (destination)
  (or (not destination)
      (eq destination 't)
      (streamp destination)
      (and (stringp destination)
	   (array-has-fill-pointer-p destination))))
