;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: WINDOWS-HACK; Base: 10 -*-
;;; Lyle Borg-Graham, Equipe Cognisciences, Institut Alfred Fessard, CNRS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;              The Surf-Hippo Neuron Simulator                    ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This code was written as part of the Surf-Hippo project at the  ;;;
;;; Center for Biological Information Processing, Department of     ;;;
;;; Brain and Cognitive Sciences, Massachusetts Institute of        ;;;
;;; Technology, and currently at the Equipe Cognisciences, Institut ;;;
;;; Alfred Fessard, CNRS and has been placed in the public domain.  ;;;
;;; If you are using this code or any part of Surf-Hippo, please    ;;;
;;; contact surf-hippo@ai.mit.edu to be put on the mailing list.    ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; GUI Source file: windows-hack.lisp


(IN-PACKAGE "WINDOWS-HACK")


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
;


;;; ******** ******** ******** ******** ******** ******** ********
;;; ******** ******** ******** ******** ******** ******** ********
;;; ******** ******** ******** ******** ******** ******** ********



;; **********************************
;;
;; Some Variables
;;
;; **********************************

(defvar *automatic-run* nil "This run is completely under computer control, thus suppress GUI.")

(defvar *default-add-values-to-marker-label* nil)
(defvar *default-marker-label-position* :right)
(defvar *default-marker-point-type* :cross)
(defvar *default-marker-point-width* 10)
(defvar *default-marker-point-height* 10)
(defvar *default-marker-options* '(:include-cross :include-label :include-point))

(defvar *comment-positions*
  `(:upper-left :upper-middle :upper-right
    :middle-left :middle :middle-right 
    :lower-left :lower-middle :lower-right))

(defvar *default-running-comment-position* :upper-right)
(defvar *default-comment-position* :lower-right "Default comment/title position for graphics windows.")




(defvar *displayed-host-name* "" "For annotating windows - defaults to actual host.")
(defvar *always-add-host-name-to-windows* nil "Always add host name to windows.")
(defvar *add-host-name-to-windows* t "Add host name to windows if different than the display server name.")

(defvar *plotting-window-top* 20)

(defvar *maker-label-vertical-fudge* 4)	; pixels
(defvar *maker-label-horizontal-fudge* 0)	; pixels

(defvar *window-tile-fudge-factor* 0) ; pixels
(defvar *window-comp-fudge-factor* 2) ; In pixels. Added to a window component's dimensions to give a little space for positioning. Used in
				      ; COMPONENT-LEFT-AND-TOP. 

(proclaim '(type fixnum *window-comp-fudge-factor*))


(defvar *output-windows* '())		; A list of all the Garnet output windows (not menus).
(defvar *colorized-windows* '())
(defvar *twin* nil)			; The most recent window (car) of *output-windows*

(defvar *lock-all-windows* nil "When T, any new graphics windows are locked.") 


;;; Empirical screen dimensions in pixels - opal globals are off (because of OLVM?).
(defvar *screen-width* 0 "Slightly adjusted value from OPAL:*SCREEN-WIDTH*.") ; opal:*screen-width* gives 1152
(defvar *screen-height* 0 "Slightly adjusted value from OPAL:*SCREEN-height*.") ; opal:*screen-height* gives 900
(defvar *window-manager-title-border-height* 18)	; Approximate height of border added by window manager



(defvar *standard-graphics-width* 0 "Default width in pixels for windows initialized with
INITIALIZE-GRAPHICS-WINDOW. Initialized by INITIALIZE-WINDOW-SYSTEM-VARIABLES.")

(defvar *standard-graphics-height* 0 "Default height in pixels for windows initialized with
INITIALIZE-GRAPHICS-WINDOW. Initialized by INITIALIZE-WINDOW-SYSTEM-VARIABLES.")

(defvar *raise-output-windows* t "New content to output windows will raise them.")
(defvar *show-output-windows* t)
(defvar *hide-output-windows* nil "Hide all output windows. Overrides *SHOW-OUTPUT-WINDOWS*.")
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
(proclaim '(type fixnum *background-border-width*))

(defvar *default-align-to-window-specification* nil)

(defvar *window-tile-x-gap* 0 "Sets the horizontal spacing in pixels used by ARRANGE-WINDOWS.")
(defvar *window-tile-y-gap* 0 "Sets the vertical spacing in pixels used by ARRANGE-WINDOWS.")

(defvar *arrange-windows-per-row* 2)

(defvar *comment-font* opal:default-font)

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Time and Date variables.
;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This factor is used to reduce the length of the integer returned by GET-UNIVERSAL-TIME, since we don't need dates previous to 1994.
(defparameter *universal-time-conversion-factor*  2986300000)

(defvar *actual-time-stamp* 0)		; This is an integer
(defvar *time-stamp* "")		; This is a string
(defvar *time-stamp-suffix* 1)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; These macros are used in several places for window calculations.

(defmacro schema-half-height (schema)
  `(/ (the fn (g-value ,schema :height)) 2.0))

(defmacro schema-half-height-fn (schema)
  `(the (signed-byte 32) (kernel:%unary-round (schema-half-height ,schema))))

(defmacro schema-half-width (schema)
  `(/ (the fn (g-value ,schema :width)) 2.0))

(defmacro schema-half-width-fn (schema)
  `(the (signed-byte 32) (kernel:%unary-round (schema-half-width ,schema))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; running on mars (ultra) dosen't give a (assoc :HOST lisp::*environment-list*) (??).
(defun initialize-window-system-variables ()
  (setq *displayed-host-name* (cond ((and (stringp (cdr (assoc :HOST lisp::*environment-list*)))
					  (> (length (cdr (assoc :HOST lisp::*environment-list*))) 0))
				     (cdr (assoc :HOST lisp::*environment-list*)))
				    ((> (length (machine-instance)) 0)
				     (machine-instance))
				    (t "")))
  (setq *screen-width* (- opal:*screen-width* 12)
	*screen-height* (- opal:*screen-height* 10))
  ;; Set *STANDARD-GRAPHICS-WIDTH* and  *STANDARD-GRAPHICS-HEIGHT*.
  (3x2-plot-tiling))


(defun transfer-schema-slots (source destination slots)
  (loop for slot in slots do
	(s-value destination slot (g-value source slot))))

;;; SAFE-DESTROY-obj Here is a kludge to avoid destroying already destroyed objects.
(defun safe-destroy (obj)
  (when (opal-obj-exists obj)
    (when (g-value obj :aggregate)
      (destroy-top-agg (g-value obj :aggregate) obj)
;      (opal:remove-component (g-value obj :aggregate) obj)
      )
    (opal:destroy obj)))

(defun opal-obj-exists (obj)
  (and obj
       (kr::is-schema obj)
       (kr::schema-bins obj)
					;       (eq (type-of obj) 'schema)
					;      (not (search "*DESTROYED*" (prin1-to-string obj)))
       ))


;; This is used all over the place for insurance.
(defun clean-up-*output-windows* ()
  (setq *output-windows* (delete-if-not 'opal-obj-exists *output-windows*))
;  (unless (opal-obj-exists *standard-graphics-output*)
;    (setq *standard-graphics-output* nil)
;    (update-*standard-graphics-output*))
  *output-windows*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(proclaim '(inline x-y-in-win))
(defun x-y-in-win (x-y win)
  (declare (optimize (safety 0) (speed 3) (space 1)))
  (and (< 0 (the fn (car x-y)) (the fn (g-value win :width)))
       (< 0 (the fn (cadr x-y)) (the fn (g-value win :height)))))

(proclaim '(inline x-y-in-win-values))
(defun x-y-in-win-values (x y win)
  (declare (optimize (safety 0) (speed 3) (space 1)))
  (and (< 0 (the fn x) (the fn (g-value win :width)))
       (< 0 (the fn y) (the fn (g-value win :height)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun component-left (component window position)
  ;; Returns the left coordinate of COMPONENT when placed at the relative POSITION in WINDOW.
  (comp-left (g-value component :width) (g-value window :width) position))
      
(defun comp-left (comp-width win-width position)
  ;; Returns the left coordinate of COMPONENT when placed at the relative POSITION in WINDOW.
  (declare (optimize (safety 0) (speed 3) (space 1))
	   (fixnum comp-width win-width))
  (case position
    ((:upper-right :middle-right :lower-right) (- win-width (+ *window-comp-fudge-factor* comp-width)))
    ((:upper-left :middle-left :lower-left) *window-comp-fudge-factor*)
    ((:middle :upper-middle :lower-middle) (round (* 0.5 (- win-width comp-width))))))

(defun component-top (component window position)
  ;; Returns the top coordinate of COMPONENT when placed at the relative POSITION in WINDOW.
  (comp-top (g-value component :height) (g-value window :height) position))

(defun comp-top (comp-height win-height position)
  (declare (optimize (safety 0) (speed 3) (space 1))
	   (fixnum comp-height win-height))
  (case position
    ((:upper-right :upper-left :upper-middle) *window-comp-fudge-factor*)
    ((:middle-right :middle-left :middle) (round (* 0.5 (- win-height comp-height))))
    ((:lower-right :lower-left :lower-middle) (- win-height (+ *window-comp-fudge-factor* comp-height)))))

(defun component-left-and-top (component window position)
  ;; Returns as values the left and top coordinates of COMPONENT when placed at the relative POSITION in WINDOW.
  (values (component-left component window position) (component-top component window position)))

(defun move-comp (comp window position)
  (s-value comp :left (component-left comp window position))
  (s-value comp :top (component-top comp window position))
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun default-graphics-color (&optional (win *standard-graphics-output*))
  (if (eql (g-value win :background-color) opal::black)
      opal::white
      opal::black))

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
  (let ((linestyle (create-instance nil opal:default-line-style
				    (:window win)
				    (:constant nil)
				    (:thickness thickness)))
	(background-color (or background-color (o-formula (gvl :window :background-color))))
	(foreground-color (or color (o-formula (gvl :window :default-graphics-color)))))
    (cond-every (background-color (s-value linestyle :background-color background-color))
		(foreground-color (s-value linestyle :foreground-color foreground-color)))
    linestyle))
	       
(defun graphics-text-font (&optional win)
  (or (and win (g-value win :font))
      (opal:get-standard-font :serif :bold-italic :medium)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(create-instance 'basic-graphics-window
		 inter:interactor-window
		 (:icon-title (o-formula (gvl :title)))
		 (:visible nil)
		 (:double-buffered-p t)

		 (:comment-font (o-formula *comment-font*))
		 (:font (opal:get-standard-font :serif :bold-italic :medium))

		 ;; These functions transform x and y values in window data units to window pixels.
		 (:x-graphics-win-function nil)		 (:y-graphics-win-function nil)

		 (:background-color opal::white)
		 (:default-graphics-color (o-formula (if (eql opal::black (gvl :background-color))
						       opal::white
						       opal::black)))
		 (:default-line-style (o-formula (let ((color (gvl :default-graphics-color)))
						   (create-instance nil opal:default-line-style
								    (:constant nil)
								    (:foreground-color color)))))
		 (:default-graphics-background (o-formula (if (eql opal::black (gvl :background-color))
							    opal::black-fill opal::white-fill)))
		 (:default-graphics-filling-style (o-formula (if (eql opal::black (gvl :background-color))
							       opal::white-fill opal::black-fill))))
		 

;; Transform X value in window data units to window pixels, according the window's :X-GRAPHICS-WIN-FUNCTION slot. Return original X value if
;; :X-GRAPHICS-WIN-FUNCTION is NIL.
(defun x-graphics-win (x win)
  (if (g-value win :x-graphics-win-function)
      (funcall (g-value win :x-graphics-win-function) (float x) win)
      (round x)))

;; Transform Y value in window data units to window pixels, according the window's :Y-GRAPHICS-WIN-FUNCTION slot. Return original Y value if
;; :Y-GRAPHICS-WIN-FUNCTION is NIL.
(defun y-graphics-win (y win)
  (if (g-value win :y-graphics-win-function)
      (funcall (g-value win :y-graphics-win-function) (float y) win)
      (round y)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-agg (win &optional force)
  ;; Return the :AGGREGATE of WIN, or the :AGGREGATE of the :CHILD window if WIN :MODE is 'INFO. If non INFO window, and FORCE is T then create and
  ;; add new aggregate.
  (cond ((eq (g-value win :mode) 'info) (g-value (car (g-value win :child)) :aggregate))
	((g-value win :aggregate) (g-value win :aggregate))
	(force (s-value win :aggregate (create-instance nil opal:aggregate)))))

(defun get-agg-components (win)
  (let ((agg (get-agg win)))
    (when agg (g-value agg :components))))

(defun get-top-level-thing (win thing)
  (loop for comp in (get-agg-components win) when (eq thing (car (g-value comp :is-a))) do (return comp)))

(defun retrieve-window-agg-comp (win comp-type)
  ;; Return first component in WIN :AGGREGATE that :IS-A COMP-TYPE.
  (loop for comp in (get-agg-components win)
	when (eq (car (g-value comp :is-a)) comp-type)
	do (return comp)))

(defun retrieve-window-agg-comps (win comp-type)
  ;; Return all components in WIN :AGGREGATE that :IS-A COMP-TYPE.
  (loop for comp in (get-agg-components win)
	when (eq (car (g-value comp :is-a)) comp-type)
	collect comp))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; These are various types of window labels. Each is added to an instance of LABEL-AGG-PROTO, and then the label-agg is added to the window's top
;; aggregate.

(create-instance 'label-agg-proto opal:aggregate)

(create-instance 'window-hack-text opal:cursor-multi-text
		 (:text "")
		 (:orientation :horizontal)
		 ;;; LBG 10.03.00
;		 (:line-style (o-formula (or (gvl :parent :window :default-line-style)
;					     (graphics-text-linestyle (gvl :parent :window :default-graphics-color) (gvl :parent :window)))))
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
		 (:parts
		  `((:background ,opal:rectangle
				 (:filling-style ,(o-formula (or (gvl :parent :window :default-graphics-background) opal::white-fill)))
				 (:left ,(o-formula (gvl :parent :left)))
				 (:top ,(o-formula (gvl :parent :top)))
				 (:width ,(o-formula (+ (the fn (gvl :parent :label :width)) (* 2 *background-border-width*))))
				 (:height ,(o-formula (+ (the fn (gvl :parent :label :height)) (* 2 *background-border-width*))))
				 (:box '(0 0 0 0))
				 ;; LBG 10.03.00 - uncomment apr 18, 00
				 (:line-style nil)
				 )
		    (:frame ,opal:rectangle
			    (:left ,(o-formula (gvl :parent :background :left)))
			    (:top ,(o-formula (gvl :parent :background :top)))
			    (:width ,(o-formula (gvl :parent :background :width)))
			    (:height ,(o-formula (gvl :parent :background :height)))
			    (:visible nil))
		    (:label ,window-hack-text
			    (:font ,(o-formula (or (gvl :parent :window :comment-font) (graphics-text-font (gvl :parent :window)))))
			    (:text ,(o-formula (or (gvl :parent :window :title) "")))
			    (:orientation ,(o-formula (gvl :parent :orientation)))
			    (:left ,(o-formula (+ (the fn (gvl :parent :left)) *background-border-width*)))
			    (:top ,(o-formula (+ (the fn (gvl :parent :top)) *background-border-width*))))))
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
			(opal:get-standard-font :serif :bold-italic :medium))))

(g-value window-comment :frame :visible)
(s-value window-comment :frame :visible nil)
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
		 (:box '(0 0 0 0))
		 (:line-style nil))

(defun redo-label-background (label-agg)
  (loop for comp in (g-value label-agg :components) when (eq label-background (car (g-value comp :is-a)))
	do (opal:remove-component label-agg comp) (opal:destroy comp))
  (let ((label-background (create-instance nil label-background
					   (:where :back)
					   (:box (multiple-value-bind (left top width height)
						     (opal:bounding-box label-agg)
						   (list (- left 5)
							 (- top 5)
							 (+ width 10)
							 (+ height 10)))))))
    (opal:add-component label-agg label-background :where :back)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun host-is-display-server ()
  "Return T if host machine is display server."
  (let* ((display (cdr (assoc :DISPLAY lisp::*environment-list*)))
	 (display-machine (string-head display (search ":" display))))
    (true-p (or (string= display ":0.0")
		(string= (machine-instance) display-machine)
		(search display-machine (machine-instance))))))

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
	(string-tail title (- (length title)
			      (+ (length host-name-string) search-position)))
	title)))

  
(defun 3x2-plot-tiling-square ()
  "Set *STANDARD-GRAPHICS-WIDTH* and *STANDARD-GRAPHICS-HEIGHT* for 3x2 tiling, with squares."
  (let ((side (- (truncate (/ *screen-height* 3)) *window-manager-title-border-height*)))
    (setq *standard-graphics-width* side
	  *standard-graphics-height* side)))

(defun 3x2-plot-tiling ()
  "Set *STANDARD-GRAPHICS-WIDTH* and *STANDARD-GRAPHICS-HEIGHT* for 3x2 tiling, with windows filling screen."
  (setq *standard-graphics-width* (truncate (- (/ *screen-width* 2) 10)) 
	*standard-graphics-height* (- (truncate (/ *screen-height* 3)) *window-manager-title-border-height*)))

(defun align-to-window (&optional reference-window &key (alignment *default-align-to-window-specification*))
  (let* ((reference-window (or reference-window (win-menu "Align Windows Reference" nil nil t)))
	 (windows (win-menu (format nil "Choose Windows to Align with ~A" (g-value reference-window :title))))
	 (dummy1 alignment))
    (choose-variable-values
     '((dummy1 "Align to:" :x-choose (:left :top)))
     :label (format nil "Aligning Windows to ~A" (g-value reference-window :title)))
    (loop for win in windows
	  do (cond-every
	      ((member :top dummy1) (s-value win :top (g-value reference-window :top)))
	      ((member :left dummy1) (s-value win :left (g-value reference-window :left))))
	  (opal::update win))))
    
   
(defun match-win-dimensions-menu (win matched-dimensions &optional additional-windows-to-set)
  (let ((match-win (choose-list-values-from-keys
		    (loop for candidate-win in (windows-of-mode (g-value win :mode))
			  unless (member candidate-win (cons win additional-windows-to-set))
			  collect (list (g-value candidate-win :title) candidate-win))
		    nil
		    :punt-if-only-one-entry nil :only-one-choice t
		    :text (format nil "Choose window for setting ~a dimension~%of window(s) ~a"
				  (NICE-STRING-FROM-KEYWORD matched-dimensions)
				  (atomize-list (loop for win in (cons win additional-windows-to-set) collect (g-value win :title))))
		    :label "Matching Window Dimensions")))
    (when match-win
      (case matched-dimensions
	(:Width_&_Height (s-value win :width (g-value match-win :width)) (s-value win :height (g-value match-win :height)))
	(:Width (s-value win :width (g-value match-win :width)))
	(:Height (s-value win :height (g-value match-win :height))))
;      (break)
      (opal::update win t))))

(defun adjust-label-background (label-agg)
  (redo-label-background label-agg))

(defun adjust-all-labels (win)
  (when (get-agg win)
    (loop for comp in (get-agg-components win)
	  when (eq label-agg-proto (car (g-value comp :is-a))) do (redo-label-background comp) and collect comp into labels
	  when (g-value comp :label) collect comp into labels
	  finally (loop for label in labels do (opal:move-component (get-agg win) label :where :front)))))

;; Return first component of WIN :AGGREGATE of TYPE that is not :RESERVED (unless IGNORE-RESERVED) and, if POSITION is non-NIL, has :POSITION eq to
;; POSITION. 
(defun get-window-comment (win &key position (type window-comment) ignore-reserved)
  (loop for comment in (retrieve-window-agg-comps win type)
	when (and (or ignore-reserved (not (g-value comment :reserved)))
		  (or (not position) (eq (g-value comment :position) position)))
	do (return comment)))

(defun parse-comment-position (position &optional (default *default-comment-position*))
  (if (member position *comment-positions*) position default))
	      
(defun add-label (win type text &key append-to-existing-text font (borderp t) position get-new)
  (when win
    (let ((position (parse-comment-position position)))
      (case (g-value win :mode)
	(:info (let ((text-obj (g-value win :text-object)) temp)
		 (s-value text-obj :last-font (g-value text-obj :current-font))
		 (opal:set-cursor-to-x-y-position text-obj 0 (- (g-value text-obj :scrolling-window :height)
								(g-value text-obj :scrolling-window :y-offset)))
		 (setq temp (opal:kill-rest-of-line text-obj))
		 (push (if (consp temp) (caaar temp) "") (g-value text-obj :deletions))
		 (opal:go-to-prev-line text-obj)
		 (setq temp (opal:kill-rest-of-line text-obj))
		 (push (if (consp temp) (caaar temp) "") (g-value text-obj :deletions))
		 (opal:insert-string text-obj text font)
		 (opal:go-to-beginning-of-line text-obj)))
	(t
	 (let ((label (or (unless get-new (get-window-comment win :position position :type type))
			  (opal:add-component (get-agg win t) (create-instance nil (or type window-comment))))))
	   (when position (s-value label :position position))
	   (s-value label :label :text
		    (if text
		      (if append-to-existing-text
			(concatenate `string (g-value label :label :text) (when text (format nil "~%")) text) text)
		      ""))
	   (s-value label :background :visible borderp)
	   (s-value label :window win)
	   (when font (s-value label :font font))
	   label))))))


(defun remove-label (win type &key position)
  (if (eq (g-value win :mode) :info)
      (let* ((text-obj (g-value win :text-object))
	     (font (g-value text-obj :last-font)))
	(opal:kill-rest-of-line text-obj)
	(loop for string in (g-value text-obj :deletions) do
	      (opal:insert-string text-obj string font)
	      (opal:go-to-next-line text-obj))
	(opal:go-to-end-of-text text-obj) 
	(s-value text-obj :deletions nil))
      (let ((label (loop for comp in (get-agg-components win)
			 when (and (eq (car (g-value comp :is-a)) (or type window-comment))
				   (or (not position) (eq (g-value comp :position) position)))
			 do (return comp))))
	(when label
	  (opal:remove-component win label)
	  (opal:destroy label)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun add-comment (win comment &key font (update t) append-to-old-comment (position *default-comment-position*) (borderp t))
  (when win
    (if (or (not comment) (string= comment ""))
      (when position (let ((*automatic-run* t)) (add-comment-menu win :position-to-remove position)))
      (add-label win window-comment comment :append-to-existing-text append-to-old-comment :font font :borderp borderp :position position))
    (when update (opal:update win))))

(defun add-temp-comment (win &optional comment &key font (update t) position resurrect deiconify ignore-reserved)
  (let ((temp-comment (get-window-comment win :type window-temp-comment :ignore-reserved ignore-reserved)))
    (when font (s-value win :temp-comment-font font))
    (cond (position (s-value win :temp-comment-position position))
	  ((not (g-value win :temp-comment-position))
	   (s-value win :temp-comment-position :upper-right)))
    (if (and temp-comment (stringp comment) (string= comment ""))
	(progn
	  (s-value temp-comment :visible nil)
	  (s-value temp-comment :frame :visible nil))
	(progn
	  (if temp-comment
	      (s-value temp-comment :label :text comment)
	      (setq temp-comment (add-label win window-temp-comment comment
					    ; :font (g-value win :temp-comment-font)
					    )))
	  (s-value temp-comment :frame :visible nil)
	  (s-value temp-comment :visible t)))
    (when temp-comment (s-value temp-comment :position (g-value win :temp-comment-position)))
    (cond (resurrect (resurrect-opal-win win :raise t :visible t :deiconify deiconify :update nil))
	  (update (opal:update win)))))


(defun update-title (win)
  (let ((title (retrieve-window-agg-comp win window-title)))
    (when title (s-value title :label :text (g-value win :title)))))

(defun add-window-title-prefix (prefix &optional wins)
  "For each window referenced in WINS [atom or list, default NIL], add PREFIX and a space to the beginning of the title. If WINS not supplied, the
windows are selected from a menu of all output windows."
  (loop for win in (if wins (coerce-to-list wins) (win-menu))
	do (s-value win :title (format nil "~A ~A" (string prefix) (g-value win :title)))
	(update-title win)))

(defun add-window-title-suffix (suffix &optional wins)
  "For each window referenced in WINS [atom or list, default NIL], add a space and SUFFIX to the beginning of the title. If WINS not supplied, the
windows are selected from a menu of all output windows."
  (loop for win in (if wins (coerce-to-list wins) (win-menu))
	do (s-value win :title (format nil "~A ~A" (g-value win :title)  (string suffix)))
	(update-title win)))
  

(defun add-title (win &key font (update t) position border)
  (loop for win in (coerce-to-list win) do
	(unless (eq (g-value win :mode) :info)
	  (let ((title (retrieve-window-agg-comp win window-title))
		(title-text (g-value win :title)))
	    (when font			; (s-value win :title-font font)
	      (s-value win :comment-font font))
	    (cond (position (s-value win :title-position (parse-comment-position position :lower-left)))
		  ((not (g-value win :title-position))
		   (s-value win :title-position :lower-left)))
	    (if (and title (= 0 (length title-text)))
		(s-value title :visible nil)
		(progn
		  (if title
		      (s-value title :label :text title-text)
		      (setq title (add-label win window-title title-text
					     ;; :borderp border
					     :font (g-value win :title-font))))
		  (s-value title :visible t)))
	    (when title
	      (s-value title :position (g-value win :title-position))
	      (s-value title :frame :visible border))
	    (if update (opal:update win))))))

(defun remove-title (win)
  (remove-label win window-title))

(defun find-label-type (label-agg label-type)
  (loop for comp in (g-value label-agg :components)
	when (eq label-type (car (g-value comp :is-a)))
	do (return comp)))

;;; GET-label-AGG Looks for an instance of label-AGG-PROTO in WIN top aggregate, which has a label-type component of type LABEL-TYPE, and returns it -
;;; if not found, then create one and return it.
(defun get-label-agg (win label-type &optional clear-it (where :front))
  (let ((label-agg (loop for comp in (get-agg-components win)
			 when (and (eq label-agg-proto (car (g-value comp :is-a))) (find-label-type comp label-type))
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
	  (loop for part in (g-value label-type-instance :parts)
		do (opal:add-component label-agg (create-instance nil part)))
	  label-agg))))


(defun edit-title (win)
  (let* ((positions *comment-positions*)
	 (dummy1 (or (g-value win :title-position) :lower-left))
	 (dummy2 (g-value win :title))
	 dummy3 dummy4 dummy5 dummy6
	 (title (retrieve-window-agg-comp win window-title))
	 (dummy8 (when title (g-value title :frame :visible)))
	 dummy9)	
    (choose-variable-values
     `((dummy6 ,(if title "Remove visible title from window" "Add visible title to window") :boolean)
       (dummy2 ,(format nil "Window title (displayed version can~%be edited from the window)") :string)
       (dummy1 "Title Position:" :choose ,positions)
       (dummy8 "Include border in title" :boolean)
       (dummy3 "CANCEL" :boolean))
     :label (format nil "Edit displayed title of ~s" (g-value win :title)))
    (unless dummy3
      (s-value win :title-position dummy1)
      (s-value win :title dummy2)
      (when dummy6
	(if title
	    (progn (remove-title win) (setq title nil))
	    (progn (add-title win) (setq title (retrieve-window-agg-comp win window-title)))))
      (when title
	(s-value title :frame :visible dummy8)
	(s-value title :position dummy1)))
    title))

(defun retrieve-window-comment (win position)
  (loop for comp in (g-value win :aggregate :components)
	when (and (eq (car (g-value comp :is-a)) window-comment) (eq (g-value comp :position) position))
	do (return comp)))

(defun remove-comment (win position)
  (let ((old-comment (retrieve-window-comment win position)))
    (when old-comment
      (opal:remove-component (get-agg win t) old-comment)
      (opal:destroy old-comment))))

(defun add-comment-menu (win &key position-to-remove)
  (let ((positions *comment-positions*)
	(dummy1 (or position-to-remove *default-comment-position*))
	(dummy2 "") dummy3 (dummy4 position-to-remove) dummy5 dummy6 (dummy7 nil) (dummy8 nil) dummy9 (dummy10 t) dummy11)
    (choose-variable-values
     `((dummy1 "Position:" :choose ,positions)
       (dummy2 "String (this can be edited from the window):" :string)
       (dummy10 "Append new text to existing comment" :boolean)
       (dummy7 "Add comment at indicated position" :boolean)
       (dummy4 "Remove comment at indicated position" :boolean)
       (dummy8 "Include border in comment" :boolean)
       (dummy5 "Edit comment/title font" :boolean)
       (dummy6 "Change pointer comment position" :boolean)
       (dummy9 "Add/edit window title" :boolean)
       (dummy11 "Edit running comment" :boolean)
       (dummy3 "CANCEL" :boolean))
     :label (format nil "Add/remove comment to ~s" (g-value win :title)))
    (when dummy9 (edit-title win))
    (when dummy11 (let ((dummy1 (or (g-value win :running-comment-position) *default-running-comment-position*)))
		    (choose-variable-values
		     `((dummy1 "Position:" :choose ,positions))
		     :label "Running Comment Menu" :text (g-value win :title))
		    (s-value win :running-comment-position dummy1)))
    (unless dummy3
      (let ((old-comment (retrieve-window-comment win dummy1)))
	(when dummy6
	  (let ((dummy1 (or (g-value win :temp-comment-position) :upper-right))
		(temp-comment (retrieve-window-agg-comp win window-temp-comment)))
	    (choose-variable-values
	     `((dummy1 "Position:" :choose ,positions))
	     :label (format nil "Pointer comment position in ~s" (g-value win :title)))
	    (s-value win :temp-comment-position dummy1)
	    (when temp-comment (s-value temp-comment :position dummy1))))
	(when dummy5
	  (s-value win :comment-font (font-menu (or (g-value win :comment-font)
						    (g-value win :font)
						    (opal:get-standard-font :serif :bold-italic :medium)))))
	(if dummy4
	  (remove-comment win dummy1)
	  (when dummy7
	    (if old-comment
	      (s-value old-comment :label :text
		       (if dummy10 (concatenate 'string (g-value old-comment :label :text) dummy2) dummy2))
	      (s-value
	       (opal:add-component (get-agg win t)
				   (setq old-comment (create-instance nil window-comment (:position dummy1))))
	       :label :text dummy2))))
	(unless dummy4 (when old-comment (s-value old-comment :frame :visible dummy8)))
	(opal:update win)))))

#|
;; NO LONGER USED
(defun find-comment (win)
  (loop for comp in (g-value win :aggregate :components)
	when (eq window-comment (car (g-value comp :is-a))) do (return (g-value comp :string))))

;; NO LONGER USED
(defun change-comment-font (win font &key (update t))
  (loop for comp in (g-value win :aggregate :components)
	when (g-value comp :components)
	do  (loop for comp2 in (g-value comp :components)
		  when (eq window-comment (car (g-value comp2 :is-a))) do (s-value comp2 :font font))
	when (eq window-comment (car (g-value comp :is-a))) do (s-value comp :font font))
  (when update (opal:update win)))
|#

;;; S-VALUE-WINDOW-TITLE-VISIBLE The S-VALUE should be either NIL or T - this is for the aggregate of plotting window WIN.
(defun s-value-window-title-visible (win s-value)
  (when (get-agg win)
    (loop for comp in (g-value win :aggregate :components)
	  when (eq window-title (car (g-value comp :is-a))) do (s-value comp :visible s-value))))

(defun initialize-graphics-window (win &key (width *standard-graphics-width*) (height *standard-graphics-height*) extra-temp-stuff left top)
  (s-value win :locked *lock-all-windows*)
;;  (s-value win :left (if width 0 (truncate (/ *screen-width* 2))))
;;  (s-value win :top (if height 0 *plotting-window-top*))
  (s-value win :left (or left (truncate (/ *screen-width* 2))))
  (s-value win :top (or top *plotting-window-top*))
  (s-value win :width (COERCE-TO-EVEN-INT (or width *standard-graphics-width*)))
  (s-value win :height (COERCE-TO-EVEN-INT (or height *standard-graphics-height*)))
  (s-value (get-agg win t) :window win)
  (add-window-embellishments win extra-temp-stuff)
  win)


(defun reorder-top-agg-components (win &optional (where :front))
  (loop for comp in (g-value  win :stuff-that-goes-first) do (opal::move-component (get-agg win t) comp :where where)))

(defun resurrect-windows (windows)
  (loop for win in windows do (resurrect-opal-win win)))

(defun resurrect-opal-win (win &key
			       (reorder-top-agg *reorder-top-agg*)
			       (raise *raise-output-windows*)
			       (visible *show-output-windows*)
			       (invisible *hide-output-windows*)
			       (show *show-output-windows*)
			       (deiconify *deiconify-output-windows*)
			       (update *update-output-windows*))
  (let ((win (or (g-value win :window) win))) ; In case WIN is not a real window.
    (cond-every
     (reorder-top-agg (reorder-top-agg-components win))
     (t (adjust-all-labels win))
     ((or raise deiconify update show visible) (s-value win :visible t))
     (raise (unless invisible (opal:raise-window win)))
     (deiconify (opal:deiconify-window win))
     (invisible (s-value win :visible nil))
     (update (opal:update win t))
     (*print-and-destroy-output-windows*
      (print-windows win :what-to-do :print-now)
      ;; PRINT-WINDOWS can destroy the window.
      (when (opal-obj-exists win) (clear-window win t))))
    (when (opal-obj-exists win) win)))


(defun first-interactor-window (interactor)
  (if (consp (g-value interactor :window))
    (car (g-value interactor :window))
    (g-value interactor :window)))
  
(defun align-to-window-inter-function (interactor final-obj-over)
  (declare (ignore final-obj-over))
  (let ((window (first-interactor-window interactor))
	*automatic-run*)
    (unless (INTERACTORS-RUNNING window)
      (s-value window :print-interactor-running t)
      (when (consp window) (loop for win in window when (g-value win :mode) do (setq window win)))
      (align-to-window window)
      ;; PRINT-WINDOWS can destroy the window.
      (when (opal-obj-exists window) (s-value window :print-interactor-running nil)))))

(create-instance 'align-window-Interactor inter:button-Interactor
		 (:continuous nil) (:start-where t) (:start-event '(:control-\a))
		 (:final-function #'ALIGN-TO-WINDOW-INTER-FUNCTION))

(create-instance 'help-window-Interactor inter:button-Interactor
		 (:continuous nil) (:start-where t) (:start-event '(#\h #\H :control-\h :control-H))
		 (:final-function nil))

(defun print-window-inter-function (interactor final-obj-over)
  (declare (ignore final-obj-over))
  (let ((window (first-interactor-window interactor))
	*automatic-run*)
    (unless (INTERACTORS-RUNNING window)
      (s-value window :print-interactor-running t)
      (when (consp window) (loop for win in window when (g-value win :mode) do (setq window win)))
      (print-windows window :use-menu t)
      ;; PRINT-WINDOWS can destroy the window.
      (when (opal-obj-exists window) (s-value window :print-interactor-running nil)))))

(create-instance 'print-window-Interactor inter:button-Interactor
		 (:continuous nil) (:start-where t) (:start-event :control-\p)
		 (:final-function #'print-window-inter-function))

(defun verify-interactor-window (interactor)
  (loop for win in (coerce-to-list (g-value interactor :window))
	when (not (opal-obj-exists win))
	do (return nil)
	finally (return t)))

(defmacro window-interactor-wrapper (interactor body)
  `(let ((window (first-interactor-window ,interactor))
	 *automatic-run*)
    (unless (INTERACTORS-RUNNING window)
      (s-value window :window-menu-interactor-running t)
      (when (consp window) (loop for win in window when (g-value win :mode) do (setq window win)))
      ,body
      (when (opal-obj-exists window) (s-value window :window-menu-interactor-running nil)))))


(export '(window))			; Symbol used in the wrapper macro

(defun toggle-window-lock-inter-function (interactor final-obj-over)
  (declare (ignore final-obj-over))
  (window-interactor-wrapper
   interactor
   (s-value window :locked
	    (not (and (g-value window :locked)
		      (go-ahead-menu (format nil "Unlock window ~A?" (g-value window :title))))))))


(create-instance 'toggle-window-lock-Interactor inter:button-Interactor
		 (:continuous nil)
		 (:start-where t)
		 (:start-event :control-\L)
		 (:final-function #'toggle-window-lock-inter-function))


(defun interactors-running (win)
  (or (g-value win :destroy-interactor-running)
      (g-value win :window-menu-interactor-running)
      (loop for win in (clean-up-*output-windows*) when (g-value win :print-interactor-running) do (return t))
      (g-value win :comment-interactor-running)))

(create-instance 'resurrect-window-Interactor inter:button-Interactor
		 (:continuous nil) (:start-where t) (:start-event #\r)
		 (:final-function #'(lambda (interactor final-obj-over)
				      (declare (ignore final-obj-over))
				      (resurrect-opal-win (g-value interactor :window) :raise t :visible t :show t :update t :deiconify t))))


(create-instance 'refresh-window-Interactor inter:button-Interactor
		 (:continuous nil) (:start-where t) (:start-event #\R))



;; The :FINAL-FUNCTION for the menu interactor is specific for the various window types.
(create-instance 'window-menu-Interactor inter:button-Interactor
		 (:continuous nil) (:start-where t) (:start-event :control-\m))


;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun destroy-windows-with-menu (reference-window windows-to-destroy)
  (loop for new-win in (win-menu "Select Windows to Destroy" (clean-up-*output-windows*) windows-to-destroy)
	when (and (opal-obj-exists new-win)
		  (or (not (g-value new-win :locked))
		      (let ((dummy10 nil))
			(choose-variable-values
			 `((dummy10 "Unlock and Destroy" :boolean))
			 :label (format nil "Unlock ~A to Destroy" (g-value new-win :title)))
			dummy1)))
	do (clear-window new-win)
	finally (when (opal-obj-exists reference-window) (s-value reference-window :destroy-interactor-running nil))))

(defun destroy-window-with-menu (win mode)
  (when (opal-obj-exists win)
    (let ((locked-wins (loop for win in *output-windows* when (g-value win :locked) sum 1)))
      (choose-variable-values
       `((dummy1 ,(if dummy3 "Selected window is locked: Unlock and destroy window" "Go ahead and destroy selected window") :boolean)
	 ,(when (> locked-wins 0)
	    `(dummy4 ,(format nil "Unlock all (~D) locked windows" locked-wins) :boolean))
	 ,(when (> (length (windows-of-mode mode)) 1)
	    `(dummy2 ,(format nil "Destroy all (currently ~D) non-locked ~a windows" (length (windows-of-mode mode)) mode) :boolean))
	 ,(when (> (length *output-windows*) (length (windows-of-mode mode)))
	    `(dummy5 ,(format nil "Destroy all (currently ~D) non-locked windows" (length *output-windows*)) :boolean)))
       :text (format nil "Destroying ~A" (g-value win :title)) :label "Destroying Windows"))
    (when dummy4 (unlock-all-windows))
    (when (and dummy1 (g-value win :locked)) (s-value win :locked nil))
    (s-value win :destroy-interactor-running nil)
    (cond (dummy5 (clear-windows *output-windows*))
	  (dummy2 (clear-windows-of-mode mode))
	  (dummy1 (clear-window win)))))

(defun destroy-window-inter-function (interactor final-obj-over &optional use-all-win-menu)
  (declare (ignore final-obj-over))
  (when (verify-interactor-window interactor)
    (let* ((win (g-value interactor :window))
	   (windows-to-destroy (coerce-to-list win))
	   ;; If C-D then dispense with menus and just kill the window.
	   (*automatic-run* (equal (g-value INTERACTOR :START-CHAR) :CONTROL-\D))
	   ;; For some reason make sure that only one window associated with interactor is used.
	   (mode (if (consp win)
		   (loop for window in win when (and (opal-obj-exists window) (g-value window :mode))
			 do (setq win window) (return (g-value window :mode)))
		   (g-value win :mode)))
	   (dummy1 t) dummy2 (dummy3 (g-value win :locked)) dummy4 dummy5)
      (unless (interactors-running win)
	(s-value win :destroy-interactor-running t)
	(if use-all-win-menu
	  (destroy-windows-with-menu win windows-to-destroy)
	  (destroy-window-with-menu win mode))))))

(create-instance 'destroy-window-Interactor inter:button-Interactor
		 (:continuous nil) (:start-where t) (:start-event '(:CONTROL-\d :CONTROL-\D))
		 (:final-function #'destroy-window-inter-function))

(defun destroy-all-window-inter-function (interactor final-obj-over)
  (destroy-window-inter-function interactor final-obj-over t))

(create-instance 'destroy-all-window-Interactor inter:button-Interactor
		 (:continuous nil) (:start-where t) (:start-event :control-\D)
		 (:final-function #'destroy-all-window-inter-function))

;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun window-comment-inter-function (interactor final-obj-over)
  (declare (ignore final-obj-over))
  (let ((win (g-value interactor :window)) *automatic-run*)
    (unless (or (consp win) (INTERACTORS-RUNNING win))
      (s-value win :comment-interactor-running t)
      (ADD-COMMENT-MENU win)
      (s-value win :comment-interactor-running nil))))


(create-instance 'window-comment-Interactor inter:button-Interactor
		 (:continuous nil) (:start-where t) (:start-event :control-\t)
		 (:final-function #'window-comment-inter-function))

;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun erase-temp-comment-inter-function (interactor final-obj-over)  
  (declare (ignore final-obj-over))
  (let ((win (g-value interactor :window))
	*automatic-run*)
    (unless (consp win)
      (add-temp-comment win "" :ignore-reserved t))))

(create-instance 'erase-temp-comment-Interactor inter:button-Interactor
		 (:continuous nil) (:start-where t) (:start-event :control-\e)
		 (:final-function #'erase-temp-comment-inter-function))

;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Not using the hide-window-interactor....
(defun hide-window-interactor-function (interactor final-obj-over)
  (declare (ignore final-obj-over))
  (resurrect-opal-win (g-value interactor :window) :raise t :invisible t :show t :update t :deiconify t))

(create-instance 'hide-window-interactor inter:button-Interactor
		 (:continuous nil) (:start-where t) (:start-event '(#\h #\H))
		 (:final-function #'hide-window-interactor-function))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; The following four objects are used for coordinate pointers in graphics windows.

(create-instance 'cross-hair-point
		 opal:circle
		 (:fast-redraw-p T)  (:draw-function :xor)
		 (:radius 2)
		 (:line-style nil)
		 (:left (o-formula (- (the fn (third (gvl :parent :points))) (the fn (gvl :radius)))))
		 (:top (o-formula (- (the fn (fourth (gvl :parent :points))) (the fn (gvl :radius)))))
		 (:width (o-formula (+ (the fn (gvl :radius)) (the fn (gvl :radius)))))
		 (:height (o-formula (+ (the fn (gvl :radius)) (the fn (gvl :radius)))))
		 (:visible (o-formula (gvl :parent :visible))))

(create-instance 'x-cross-hair
		 opal:line
		 (:fast-redraw-p T) (:draw-function :xor)
		 (:x1 0) (:y1 (o-formula (fourth (gvl :parent :points))))
		 (:x2 (o-formula (gvl :window :width))) (:y2 (o-formula (fourth (gvl :parent :points))))
		 (:visible (o-formula (gvl :parent :visible)))
		 (:line-style opal:dashed-line))

(create-instance 'y-cross-hair
		 opal:line
		 (:fast-redraw-p T) (:draw-function :xor)
		 (:x1 (o-formula (third (gvl :parent :points)))) (:y1 0)
		 (:x2 (o-formula (third (gvl :parent :points)))) (:y2 (o-formula (gvl :window :height)))
		 (:visible (o-formula (gvl :parent :visible)))
		 (:line-style opal:dashed-line))

(create-instance 'cross-hair opal:aggregadget
		 (:visible nil)
		 (:points '(0 0 0 0))
		 (:parts `((:x-cross-hair ,(create-instance nil x-cross-hair))
			   (:y-cross-hair ,(create-instance nil y-cross-hair))
			   (:cross-hair-point ,(create-instance nil cross-hair-point)))))

(create-instance 'cross-vertical-hair opal:aggregadget
		 (:visible nil)
		 (:points '(0 0 0 0))
		 (:parts `((:y-cross-hair ,(create-instance nil y-cross-hair))
			   (:cross-hair-point ,(create-instance nil cross-hair-point)))))

(create-instance 'cross-horizontal-hair opal:aggregadget
		 (:visible nil)
		 (:points '(0 0 0 0))
		 (:parts `((:x-cross-hair ,(create-instance nil x-cross-hair))
			   (:cross-hair-point ,(create-instance nil cross-hair-point)))))

(create-instance 'cross-hair-w/o-hairs opal:aggregadget
		 (:visible nil)
		 (:points '(0 0 0 0))
		 (:parts `((:cross-hair-point ,(create-instance nil cross-hair-point)))))

(create-instance 'window-coords-pointer inter:two-point-Interactor
		 (:start-where t)
		 (:line-p t)
		 (:last-xy nil)
		 (:start-event :middledown))

(create-instance 'window-mark-point inter:button-Interactor
		 (:continuous nil) (:start-where t)
		 (:start-event :control-middledown))

(create-instance 'marker-label opal:aggregadget
		 (:visible nil)
		 (:points '(0 0 0 0))
		 (:parts `((:label ,window-hack-text
				   (:font ,(o-formula (or (gvl :parent :window :marked-points-font)
							  (gvl :parent :window :comment-font)
							  (gvl :parent :window :font)
							  (gvl :parent :window :plot-axis-font)
							  (opal:get-standard-font :serif :bold-italic :medium))))
				   (:top ,(o-formula (case (gvl :parent :label-position)
						       ((:left-up :center-up :right-up)
							(- (the fn (fourth (gvl :parent :points)))
							   (+ (round *maker-label-vertical-fudge*)
							      (the fn (gvl :height)))))
						       ((:left-down :center-down :right-down)
							(+ (the fn (fourth (gvl :parent :points)))
							   (round (* 0.5 (+ *maker-label-vertical-fudge*
									    (the fn (gvl :height)))))))
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
		 (:v-bar-width 10)
		 (:h-bar-width 10)
		 (:points '(0 0 0 0))
		 (:parts `((:cross-h-bar ,opal:line
			    (:visible t) (:line-style ,thick-black-line)
			    (:x1 ,(o-formula (- (the fn (third (gvl :parent :points)))
						(round (* 0.5 (the fn (gvl :parent :h-bar-width)))))))
			    (:x2 ,(o-formula (+ (the fn (third (gvl :parent :points)))
						(round (* 0.5 (the fn (gvl :parent :h-bar-width)))))))
			    (:y1 ,(o-formula (fourth (gvl :parent :points))))
			    (:y2 ,(o-formula (fourth (gvl :parent :points)))))
			   (:cross-v-bar ,opal:line
			    (:visible t) (:line-style ,thick-black-line)
			    (:x1 ,(o-formula (third (gvl :parent :points))))
			    (:x2 ,(o-formula (third (gvl :parent :points))))
			    (:y1 ,(o-formula (- (the fn (fourth (gvl :parent :points)))
						(round (* 0.5 (the fn (gvl :parent :v-bar-width)))))))
			    (:y2 ,(o-formula (+ (the fn (fourth (gvl :parent :points)))
						(round (* 0.5 (the fn (gvl :parent :v-bar-width)))))))))))

(create-instance 'mark-point-circle opal:circle
		 (:label t)
		 (:diameter 10)
		 (:points '(0 0 0 0))
		 (:line-style thick-black-line)
		 (:width (o-formula (the fn (gvl :diameter))))
		 (:height (o-formula (the fn (gvl :diameter))))
		 (:left (o-formula (+ (the fn (third (gvl :points))) (round (* -0.5 (the fn (gvl :diameter)))))))
		 (:top (o-formula (+ (the fn (fourth (gvl :points))) (round (* -0.5 (the fn (gvl :diameter))))))))

(create-instance 'mark-point-dot mark-point-circle
		 (:filling-style opal::black-fill)
		 (:line-style nil))

(create-instance 'mark-point-box opal:rectangle
		 (:label t)
		 (:width 10)
		 (:points '(0 0 0 0))
		 (:line-style thick-black-line)
		 (:height 10)
		 (:left (o-formula (+ (the fn (third (gvl :points))) (round (* -0.5 (the fn (gvl :width)))))))
		 (:top (o-formula (+ (the fn (fourth (gvl :points))) (round (* -0.5 (the fn (gvl :height))))))))

(create-instance 'mark-point-filled-box mark-point-box
		 (:line-style nil)
		 (:filling-style opal::black-fill))

(defun update-running-comment (window text)
  (let ((running-comment (g-value window :running-comment)))
    (s-value running-comment :label :text text)
    (s-value running-comment :visible t)))

(defun add-running-comment (window &optional text borderp)
  (let ((label (add-label window window-temp-comment "" :borderp borderp)))
    (s-value label :position (o-formula (or (gvl :parent :window :running-comment-position) *default-running-comment-position*)))
    (when text (s-value label :text text))
    (s-value label :reserved t)
    (s-value label :frame :visible nil)
    (s-value label :visible t)
    (s-value window :running-comment label)))

;; ADD-WINDOW-COORDS-POINTER

;; Adds pointer to graphics windows that on middle mouse puts a cross hair on the window which moves with the mouse and
;; disappears when the button is released. Calling ADD-WINDOW-COORDS-POINTER includes a function symbol that specifies what should be done when the
;; coords pointer finishes (e.g. display pointer coordinates).

(defun add-window-coords-pointer (win final-function &optional mark-point-final-function running-function)
  (let ((cross-hair (create-instance nil cross-hair)))
    (opal:add-component (get-agg win t) cross-hair)
    (create-instance nil window-coords-pointer (:window win) (:feedback-obj cross-hair)
		     (:running-action running-function)
		     (:final-function final-function))
    (unless (g-value win :running-comment) (add-running-comment win))
    (when mark-point-final-function
      (create-instance nil window-mark-point (:window win) (:final-function mark-point-final-function)))))


(defun add-point (win points &key (symbol :cross) (width 10) (height 10))
  (let ((point (case symbol
		 (:box (create-instance nil mark-point-box (:width width) (:height height) (:points points) (:visible t)))
		 (:filled-box (create-instance nil mark-point-filled-box (:width width) (:height height) (:points points) (:visible t)))
		 (:dot (create-instance nil mark-point-dot (:diameter width) (:points points) (:visible t)))
		 (:circle (create-instance nil mark-point-circle (:diameter width) (:points points) (:visible t)))
		 (:cross (create-instance nil mark-point-cross (:points points) (:visible t) (:v-bar-width height) (:h-bar-width width))))))
    (opal:add-component (g-value win :aggregate) point)
    point))


(defun add-marker-label (win points &key (label "") (label-position :right))
  (let ((mlabel (create-instance nil marker-label (:points (copy-list points)) (:label-position label-position) (:visible t))))
    (s-value mlabel :label :text label)
    (opal:add-component (g-value win :aggregate) mlabel)
    mlabel))


(defun add-cross (win points &optional type)
  (let ((cross (create-instance nil (case type
				      (:w/o-hairs cross-hair-w/o-hairs)
				      (:w-vertical-hair cross-vertical-hair)
				      (:w-horizontal-hair cross-horizontal-hair)
				      (t cross-hair))
				(:points points)
				(:visible t))))
    (s-value cross :cross-hair-point :filling-style opal:blue-fill)
    (opal:add-component (g-value win :aggregate) cross)
    cross))

(defun add-marker (win points &key 
		       data-x data-y
		       (add-point nil add-point-supplied-p)
		       (point-type *default-marker-point-type*) (point-width *default-marker-point-width*)
		       (point-height *default-marker-point-height*)
		       (add-cross nil add-cross-supplied-p) 
		       (add-label nil add-label-supplied-p)
		       (add-values-to-label *default-add-values-to-marker-label*)
		       (label-position *default-marker-label-position*) (label "")
		       data-to-points-function)
  (when win
    (let ((data-to-points-function (or data-to-points-function (g-value win :data-to-points-function)))
	  (add-point (if add-point-supplied-p
		       add-point
		       (member :include-point *default-marker-options*)))
	  (add-cross (if add-cross-supplied-p
		       add-cross
		       (member :include-cross *default-marker-options*)))
	  (label (when (if add-label-supplied-p
			 add-label
			 (member :include-label *default-marker-options*))
		   label))
	  (x-label (if (g-value win :event-plotter)
		     "ms"
		     (or (g-value win :x-units) (g-value win :x-label) "")))
	  (y-label (or (g-value win :y-units) (g-value win :y-label) "")))	
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
	  (when add-cross (setq a-list (acons 'cross (add-cross win points add-cross) a-list)))
	  (when add-point
	    (setq a-list (acons 'point-type point-type a-list))
	    (setq a-list (acons 'point-width point-width a-list))
	    (setq a-list (acons 'point-height point-height a-list))
	    (setq a-list (acons 'point (add-point win points :symbol point-type :width point-width :height point-height) a-list)))
	  (when data-to-points-function (setq a-list (acons 'data-to-points-function data-to-points-function a-list)))
	  (push a-list (g-value win :markers))
	  nil)))))

(defun add-markers-with-data (win 
			      data-x data-y
			      &key 
			      (add-point nil add-point-supplied-p)
			      (point-type *default-marker-point-type*) (point-width *default-marker-point-width*)
			      (point-height *default-marker-point-height*)
			      (add-cross nil add-cross-supplied-p) 
			      (add-label nil add-label-supplied-p)
			      (add-values-to-label *default-add-values-to-marker-label*)
			      (label-position *default-marker-label-position*) (label "")
			      data-to-points-function)
  (loop for x in data-x
	for y in data-y
	do (add-marker win nil
		       :data-x x :data-y y
		       :add-point add-point
		       :point-type point-type
		       :point-width point-width
		       :point-height point-height
		       :add-cross add-cross
		       :add-label add-label
		       :add-values-to-label add-values-to-label
		       :label-position label-position
		       :label label
		       :data-to-points-function data-to-points-function)))


(defun marker-window (marker)
  (cdr (assoc 'window marker)))

(defun marker-data-y (marker)
  (cdr (assoc 'data-y marker)))

(defun marker-data-x (marker)
  (cdr (assoc 'data-x marker)))

(defun marker-x (marker)
  (third (marker-points marker)))

(defun marker-y (marker)
  (fourth (marker-points marker)))

(defun marker-points (marker)
  (cdr (assoc 'points marker)))

(defun marker-point (marker)
  (cdr (assoc 'point marker)))

(defun marker-cross (marker)
  (cdr (assoc 'cross marker)))

(defun marker-label (marker)
  (cdr (assoc 'marker-label marker)))

;; (defun marker-label (marker)  (cdr (assoc 'label marker)))

(defun marker-point-type (marker &optional value)
  (if value
    (acons 'point-type value marker)
    (or (cdr (assoc 'point-type marker)) (g-value (marker-window marker) :point-type) *default-marker-point-type* :cross)))

(defun marker-point-width (marker &optional value)
  (if value
    (acons 'point-width value marker)
    (or (cdr (assoc 'point-width marker)) (g-value (marker-window marker) :point-width) *default-marker-point-width* 10)))

(defun marker-point-height (marker &optional value)
  (if value
    (acons 'point-height value marker)
    (or (cdr (assoc 'point-height marker)) (g-value (marker-window marker) :point-height) *default-marker-point-height* 10)))

(defun choose-window-markers (win)
  (choose-list-values-from-keys
   (loop for marker in (g-value win :markers)
	 collect (list (let ((x-label (if (g-value win :event-plotter) "ms" (or (g-value win :x-units) (g-value win :x-label) "")))
			     (y-label (or (g-value win :y-units) (g-value win :y-label) ""))
			     (data-x (marker-data-x marker))
			     (data-y (marker-data-y marker))
			     (name (if (and (marker-label marker) (> (length (g-value (marker-label marker) :label :text)) 0))
				     (g-value (marker-label marker) :label :text)
				     "Marker")))
			 (if (and data-x data-y)
			   (format nil "~A @ ~,2f~d, ~,2f~d" name data-x x-label data-y y-label)
			   (format nil "~A @ ~,2f~d" name data-x x-label)))
		       marker))
   (list (car (g-value win :markers))) :label "Choose Markers to Edit"))

(defun edit-marked-points-font (win)
  (s-value win :marked-points-font (font-menu (or (g-value win :marked-points-font) (g-value win :font)) (g-value win :title))))

(defun get-marker-label-string (data-x data-y win x-label y-label label-text add-values-to-marker-label)
  (let* ((raster (or (eq (g-value win :plot-type) 'raster) (eq (g-value win :plot-type) :raster)))
	 (data-x-label (format nil "~A~a" (ANOTHER-NICE-FLOAT-FORMAT data-x :range (when (g-value win :x-inc) (* 0.01 (g-value win :x-inc)))) x-label))
	 (data-y-label (unless raster
			 (format nil "~A~a" (ANOTHER-NICE-FLOAT-FORMAT data-y :range (when (g-value win :y-inc) (* 0.01 (g-value win :y-inc)))) y-label))))
    (format nil "~a~A~a" label-text
	    (if (and (> (length label-text) 0) add-values-to-marker-label) (format nil "~%") "")
	    (if add-values-to-marker-label (if raster (format nil "~a" data-x-label)(format nil "~a, ~a" data-x-label data-y-label)) ""))))

(defun marker-window-x-label (win)
  (if (g-value win :event-plotter) "ms" (or (g-value win :x-units) (g-value win :x-label) "")))


(defun marker-data-x-label (marker win)
  (format nil "~A~a" (ANOTHER-NICE-FLOAT-FORMAT (marker-data-x marker) :range (g-value win :x-inc)) (marker-window-x-label win)))

(defun marker-window-y-label (win)
  (or (g-value win :y-units) (g-value win :y-label) ""))

(defun marker-data-y-label (marker win)
  (format nil "~A~a" (ANOTHER-NICE-FLOAT-FORMAT (marker-data-y marker) :range (g-value win :y-inc)) (marker-window-y-label win)))


(defun mark-coords-pointer-menu (win &optional markers)
  (unless (g-value win :mark-coords-pointer-menu-active)
    (s-value win :mark-coords-pointer-menu-active t)
    (let ((raster (or (eq (g-value win :plot-type) 'raster) (eq (g-value win :plot-type) :raster)))
	  (markers (case markers
		     (:all (g-value win :markers))
		     ((consp markers) markers)
		     (t (choose-window-markers win))))
	  dummy2 dummy11)
      (loop for marker in markers unless (or dummy2 (string-member "All markers" dummy11)) do
	    (let* ((window-markers-w/o-this-one (remove marker (g-value win :markers))) 
		   (x-label (marker-window-x-label win))
		   (y-label (marker-window-y-label win))
		   (points (marker-points marker))
		   (point (marker-point marker))
		   (cross (marker-cross marker))
		   (label (marker-label marker))
		   (data-x-label (marker-data-x-label marker win))
		   (data-y-label (unless raster (marker-data-y-label marker win)))
		   (point-data-string (if raster (format nil "~a" data-x-label)	(format nil "~a, ~a" data-x-label data-y-label)))
		   (dummy1 (or (and label (g-value label :label :text)) "")) ; dummy2 used above
		   dummy3
		   (dummy4 *default-add-values-to-marker-label*)
		   (dummy5 (list (when (or (and cross (not (member :clear-cross *default-marker-options*)))
					   (member :include-cross *default-marker-options*))
				   :include-cross)
				 (when (or (and label (not (member :clear-label *default-marker-options*)))
					   (member :include-label *default-marker-options*))
				   :include-label)
				 (when (or (and point (not (member :clear-point *default-marker-options*)))
					   (member :include-point *default-marker-options*))
				   :include-point)))
		   dummy6
		   (dummy7 (if label (g-value label :label-position) (or *default-marker-label-position* :right)))
		   dummy8
		   (dummy13 (marker-data-x marker))
		   (dummy14 (marker-data-y marker))
		   (dummy15 (or (marker-point-type marker) (g-value win :point-type) *default-marker-point-type* :cross))
		   (dummy16 (or (marker-point-width marker) (g-value win :point-width) *default-marker-point-width* 10))
		   (dummy17 (or (marker-point-height marker) (g-value win :point-height) *default-marker-point-height*)))
	      (choose-variable-values
	       `((dummy11 "Remove:" :x-choose ("This marker" "All markers") :label-left)
		 (dummy13 ,(format nil "X position~a" (if (> (length x-label) 0) (format nil "[~a]" x-label) "")) :float)
		 ,(unless raster `(dummy14 ,(format nil "Y position~a" (if (> (length y-label) 0) (format nil "[~a]" y-label) "")) :float))
		 (dummy5 "Marker display options:" :x-choose (:include-point :include-cross :include-label) :label-left)
		 (dummy1 "Label text" :string)
		 (dummy4 "Add point value to label" :boolean)
		 (dummy15 "Point type:" :choose (:box :filled-box :dot :circle :cross) :rank-margin 5 :label-left)
		 (dummy16 "Point width [pixels] (diameter for :DOT and :CIRCLE)" :integer)
		 (dummy17 "Point height [pixels]" :integer)
		 (dummy7 "Label position:" :choose (:left-up :left-down :left :right-up :right-down :right :center-up :center-down :center))
		 (dummy3 "Make above options default" :boolean)
		 (dummy8 "Remove from all markers:" :x-choose ("Labels" "Cross hairs" "Points") :label-left :horizontal)
		 (dummy6 "Edit marked points font" :boolean)
		 (dummy2 "Cancel marker editing" :boolean))
	       :text (format nil "Editing point @ ~A" point-data-string) :label (format nil "Marker Menu ~A" (g-value win :title)))
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
			 (if (member :include-cross dummy5)
			   (unless cross (setq marker (acons 'cross (add-cross win points) marker)))
			   (setq marker (remove-cross-from-marker marker win)))
			 (if (member :include-point dummy5)
			   (progn
			     (remove-point-from-marker marker win)
			     (setq marker (acons 'point (add-point win points :symbol dummy15 :width dummy16 :height dummy17) marker)))
			   (setq marker (remove-point-from-marker marker win)))
			 (setq marker (remove-label-from-marker marker win))
			 (when (member :include-label dummy5)
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
    (s-value win :mark-coords-pointer-menu-active nil)))


(defun refresh-markers-position (win)
  (loop for marker in (g-value win :markers) do
	; (break)
	(refresh-marker-position marker win)))

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
  (s-value win :markers (remove marker (g-value win :markers)))
  (remove-label-from-marker marker win)
  (remove-point-from-marker marker win)
  (remove-cross-from-marker marker win))

  
(defun refresh-all-markers (&optional win)
  (let ((*automatic-run* t))
    (loop for win in (or (coerce-to-list win) (clean-up-*output-windows*))
	  do (mark-coords-pointer-menu win :all))))
	
(defun remove-all-markers (win &optional update)
  (loop for marker in (g-value win :markers) do
	(remove-label-from-marker marker win)
	(remove-point-from-marker marker win)
	(remove-cross-from-marker marker win))
  (s-value win :last-pointer-xy nil)
  (s-value win :markers nil)
  (when update (opal:update win)))

(defun remove-all-cross-hairs (win &optional update)
  (s-value win :markers (loop for marker in (g-value win :markers) collect (remove-cross-from-marker marker win)))
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
  (remove-all-markers (g-value interactor :window))
  (REMOVE-ALL-ZOOMERS (g-value interactor :window)))

(defun remove-last-zoomer (win)
  (let ((last-zoomer (car (last (g-value win :cross-hair-markers)))))
    (when last-zoomer
      (s-value win :cross-hair-markers (butlast (g-value win :cross-hair-markers)))
      (opal::destroy last-zoomer))))

(defun remove-last-cross-hair (interactor final-obj-over)  
  (declare (ignore final-obj-over))
  (let* ((win (g-value interactor :window))
	 (last-marker (car (last (g-value win :markers)))))
    (remove-last-zoomer win)
    (s-value win :markers
	     (loop for marker in (g-value win :markers) collect
		   (if (eq marker last-marker)
		       (remove-cross-from-marker last-marker win)
		       marker)))))

(defun remove-all-zoomers (win)
  (loop for zoomer in (g-value win :cross-hair-markers)
	do (remove-first-zoomer win)))

(defun remove-first-zoomer (win)
  (let ((first-zoomer (first (g-value win :cross-hair-markers))))
    (when first-zoomer
      (s-value win :cross-hair-markers (rest (g-value win :cross-hair-markers)))
      (opal::destroy first-zoomer))))

(defun remove-first-cross-hair (interactor final-obj-over)  
  (declare (ignore final-obj-over))
  (let* ((win (g-value interactor :window))
	 (first-marker (car (g-value win :markers))))
    (remove-first-zoomer win)
    (s-value win :markers
	     (loop for marker in (g-value win :markers) collect
		   (if (eq marker first-marker)
		       (remove-cross-from-marker first-marker win)
		       marker)))))

(defun remove-all-marker-labels (win &optional update)
  (s-value win :markers (loop for marker in (g-value win :markers) collect (remove-label-from-marker marker win)))
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
  (remove-all-marker-labels (g-value interactor :window)))

(defun remove-all-points (win &optional update)
  (s-value win :markers (loop for marker in (g-value win :markers) collect (remove-point-from-marker marker win)))
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
  (remove-all-points (g-value interactor :window)))

(defun remove-last-point (interactor final-obj-over)  
  (declare (ignore final-obj-over))
  (let* ((win (g-value interactor :window))
	 (last-marker (car (last (g-value win :markers)))))
    (when last-marker
      (s-value win :markers
	       (loop for marker in (g-value win :markers)
		     collect
		     (if (eq marker last-marker)
			 (remove-point-from-marker last-marker win)
			 marker))))))

(defun remove-first-point (interactor final-obj-over)  
  (declare (ignore final-obj-over))
  (let* ((win (g-value interactor :window))
	 (first-marker (car (g-value win :markers))))
    (when first-marker
      (s-value win :markers
	       (loop for marker in (g-value win :markers)
		     collect
		     (if (eq marker first-marker)
			 (remove-point-from-marker first-marker win)
			 marker))))))

(create-instance 'remove-first-window-coords-pointer inter:button-Interactor
		 (:continuous nil) (:start-where t) (:start-event :control-\f)
		 (:final-function #'remove-first-cross-hair))

(create-instance 'remove-last-window-coords-pointer inter:button-Interactor
		 (:continuous nil) (:start-where t) (:start-event :control-\l)
		 (:final-function #'remove-last-cross-hair))

(create-instance 'remove-all-window-coords-pointer inter:button-Interactor
		 (:continuous nil) (:start-where t) (:start-event :control-\A)
		 (:final-function #'remove-all-cross-hairs-inter))

(create-instance 'zoom-Interactor inter:Two-Point-Interactor
		 (:start-where t) (:line-p NIL) (:Min-width 20) (:Min-height 20))

(create-instance 'zoom-feedback-object opal:rectangle
		 (:fast-redraw-p T) (:draw-function :xor)
		 (:left (o-formula (first (gvl :box))))
		 (:top (o-formula (second (gvl :box))))
		 (:width (o-formula (third (gvl :box))))
		 (:height (o-formula (fourth (gvl :box))))
		 (:visible NIL) (:box '(0 0 0 0)) (:line-style opal:dashed-line))

(defun add-zoom-marker (pointer)
  (let* ((box (g-value pointer :feedback-obj :box))
	 (zoom-rectangle (create-instance nil opal:rectangle
					  (:fast-redraw-p T)
					  (:draw-function :xor)
					  (:left (first box))
					  (:top (second box))
					  (:width (third box))
					  (:height (fourth box))
					  (:visible t) (:line-style opal:dashed-line))))
    (opal:add-component (g-value pointer :window :aggregate) zoom-rectangle)
    (push zoom-rectangle (g-value pointer :window :cross-hair-markers))))

(defun add-window-zoom (win zoom-function start-event &optional (include-feedback-obj t))
  (create-instance NIL zoom-Interactor (:Window win)
		   (:feedback-obj (when include-feedback-obj
				    (or (loop for comp in (g-value win :aggregate :components)
					      when (eq (type-of comp) 'zoom-feedback-object) do (return comp))
					(let ((zoom-feed (create-instance NIL zoom-feedback-object)))
					  (opal:add-component (get-agg win t) zoom-feed)
					  zoom-feed))))
		   (:start-event start-event)
		   (:final-function zoom-function)))

;; All graphics windows get these things.
(defun add-window-embellishments (win &optional extra-temp-stuff)
;  (create-instance nil hide-window-interactor (:Window win))
  (create-instance NIL help-window-Interactor (:Window win))
  (create-instance nil align-window-Interactor (:Window win))
  (create-instance NIL print-window-Interactor (:Window win))
  (create-instance NIL destroy-window-Interactor (:Window win))
  (create-instance NIL destroy-all-window-Interactor (:Window win))
  (create-instance nil toggle-window-lock-Interactor (:window win))
  (create-instance nil window-comment-Interactor (:Window win))
  (create-instance nil remove-first-window-coords-pointer (:Window win))
  (create-instance nil remove-last-window-coords-pointer (:Window win))
  (create-instance nil remove-all-window-coords-pointer (:Window win))
  (create-instance nil resurrect-window-Interactor (:Window win))
  (let ((etci (create-instance nil erase-temp-comment-Interactor (:Window win))))
    (when extra-temp-stuff (s-value etci :final-function extra-temp-stuff))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; The CLEAR-WINDOW function family.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun update-*standard-graphics-output* (&optional win)
  (setq *standard-graphics-output*
	(when win
	  (loop for output-window in *output-windows*
		when (and (not (eq win output-window))
			  (eq (g-value win :auxiliary-type) (g-value output-window :auxiliary-type))
			  (eq  (g-value win :data-type) (g-value output-window :data-type))
			  (eq  (g-value win :mode) (g-value output-window :mode)))
		do (return output-window))))
  (when (member win *colorized-windows*) (push *standard-graphics-output* *colorized-windows*)))

(defun update-*standard-info-output* (&optional win)
  (setq *standard-info-output* 
	(loop for output-window in *output-windows*
	      when (and (not (eq win output-window))
			(eq (g-value output-window :mode) :info))
	      do (return output-window))))

;; CLEAR-WINDOW also updates the list of windows in *OUTPUT-WINDOWS*.
(defun clear-window (win &optional always)
  "Destroy WIN if unlocked, or if ALWAYS is T. Updates *OUTPUT-WINDOWS*."
  (clean-up-*output-windows*)		; Just for insurance.
  (when (and (opal-obj-exists win) (or always (and (not (g-value win :locked)) ; (member win *output-windows*)
						   )))
    (cond ((eq win *standard-graphics-output*) (update-*standard-graphics-output* win))
	  ((eq win *standard-info-output*) (update-*standard-info-output* win)))
    (setq *output-windows* (remove win *output-windows*)
	  *colorized-windows* (remove win *colorized-windows*))
    (update-*Twin*)
#+GARNET-V3.0      (destroy-opal-obj win)
#-GARNET-V3.0      (safe-destroy win)
      nil))

(defun destroy-opal-obj (obj)
;  (safe-destroy obj)
   (opal:destroy obj)
  )

(defun clear-windows (wins &optional always)
  (loop for win in (flatten-list wins) do (clear-window win always)))

(defun clean-windows ()
  (destroy-window-menu))

(defun unstick-windows ()
  "If windows don't respond, maybe they're stuck."
  (clear-interactors-running-flags))

(defun destroy-window-menu ()
  (clear-interactors-running-flags)
  (loop for win in (WINDOW-SELECTION-MENU "Choose windows to Destroy (locked or not)"  (garnet-debug:windows))
	do (clear-window win t)))

(defun clear-interactors-running-flags ()
  (loop for win in (clean-up-*output-windows*) do
	(s-value win :mark-coords-pointer-menu-active nil)
	(s-value win :print-interactor-running nil)
	(s-value win :destroy-interactor-running nil)
	(s-value win :WINDOW-MENU-INTERACTOR-RUNNING nil)
	(s-value win :comment-interactor-running nil)))

(defun clear-windows-of-mode (mode &optional always)
  (loop for win in (clean-up-*output-windows*) when (eq mode (g-value win :mode)) do (clear-window win always)) 
  (update-*Twin*))

(defun windows-of-mode (modes)
  (let ((modes (coerce-to-list modes)))
    (loop for win in (clean-up-*output-windows*) when (member (g-value win :mode) modes) collect win)))

(defun clear-all-output-windows ()
  (loop for win in (clean-up-*output-windows*) do (clear-window win t))
  (update-*Twin*))

(defun caows ()
  "Clear all output windows. Does not kill menus (use MDW)."
  (clear-all-output-windows))

(defun update-*Twin* ()
  (setq *twin* (car (clean-up-*output-windows*))))

(defun lock-window (&optional (win *twin*))
  "Make sure that WIN (can be a list) is not overwritten. If :ALL then all windows locked."
  (loop for win in
	(case win
	  (:all (clean-up-*output-windows*))
	  (t (flatten-list win)))
	do (s-value win :locked t)))

(defun lock-windows (&optional (wins :all))
  "Make sure that none of the output windows are overwritten."
  (lock-window wins))

(defun lock-all-windows ()
  (loop for win in (clean-up-*output-windows*) do (s-value win :locked t)))

(defun lock-new-windows (old-windows)
  (loop for win in *output-windows*
	unless (member win old-windows)
	do (lock-window win)))

(defun unlock-window (&optional (win *twin*))
  "Allow overwriting WIN (can be a list). If :ALL then all windows unlocked."
  (unstick-windows)
  (loop for win in
	(case win
	  (:all *output-windows*)
	  (t (flatten-list win)))
	do (s-value win :locked nil)))

(defun unlock-windows (&optional (wins :all))
  (unlock-window wins))
        

(defun unlock-all-windows ()
  "If windows don't respond, maybe they're stuck. Also unlock them all."
  (unstick-windows)
  (loop for win in (clean-up-*output-windows*) do (s-value win :locked nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-top-agg-comp (type &optional (win *standard-graphics-output*))
  (loop for comp in (g-value win :aggregate :components) when (eq type (g-value comp :type)) do (return comp)))


(defun destroy-top-agg (agg parent)
  ;; Performance hint from Garnet v2.1 Manual p507
  (when agg
    (let ((temp-agg agg))
      (s-value parent :aggregate nil)
      (opal:update parent)
      (if (and (not (and (listp (g-value temp-agg :constant))
			 (member :components (g-value temp-agg :constant))))
	       (g-value temp-agg :components))
	  (progn
	    (destroy-agg-object temp-agg)
	    (s-value parent :aggregate temp-agg))
	  (opal:destroy temp-agg)))))
	  

(defun destroy-agg-object (object)	; some things don't like to have their components killed first, so get the parent.
  (if (and (not (and (listp (g-value object :constant))
		     (member :components (g-value object :constant))))
	   (g-value object :components))
      (progn
	(loop for comp in (g-value object :components)
	      when (g-value comp :components)
	      do (destroy-agg-object comp))
	(dolist (object (g-value object :components))
	  (destroy-agg-object object)))
      (opal:destroy object)))


(create-instance 'PLOT-AGG-PROTO opal:aggregate
;;; Performance hint [GRM v2.1 p.166]
		 (:top (o-formula (gvl :window :top)))
		 (:left (o-formula (gvl :window :left)))
		 (:width (o-formula (gvl :window :width)))
		 (:height (o-formula (gvl :window :height))))


;;; ADD-PLOT-AGG Adds an instance of PLOT-AGG-PROTO of type TYPE to WIN's top aggregate and returns
;;; it.
(defun add-plot-agg (win type &optional (where :back))
  (opal:add-component (get-agg win t) (create-instance nil PLOT-AGG-PROTO (:type type)) :where where))


;;; FIND-PLOT-AGG Find and return from WIN all instances of a PLOT-AGG-PROTO whose :type is
;;; TYPE.
(defun find-plot-agg (win type)
  (loop for comp in (g-value win :aggregate :components)
	when (and (eq PLOT-AGG-PROTO (car (g-value comp :is-a))) (eq type (g-value comp :type)))
	collect comp))


;;; CLEAR-PLOT-AGGS Remove and destroy all instances of PLOT-AGG-PROTO of type TYPE in WIN.
(defun clear-plot-aggs (win type)
  (unless (eq (g-value win :mode) 'info)
    (let ((plot-aggs (find-plot-agg win type)))
      (when plot-aggs
	(let ((temp-agg (g-value win :aggregate)))
	  (s-value win :aggregate nil)
	  (opal:update win)
	  (loop for plot-agg in plot-aggs do
		(opal:destroy plot-agg)
		(s-value win :aggregate temp-agg)))))))


(defun clear-plot-aggs (win type)
  (loop for plot-agg in (find-plot-agg win type) do
	(opal:remove-component (g-value win :aggregate) plot-agg)
	(opal:destroy plot-agg))
  nil)


(defun efficient-clear-agg-components (agg)
  (loop for comp in (g-value agg :components)
	do (if nil ; (g-value comp :components)
	       (efficient-clear-agg-components comp)
	       (opal:destroy comp)))
  (opal:destroy agg))
  

;;; CLEAR-AND-ADD-PLOT-AGG Clears any plot-agg's of type TYPE from WIN's top aggregate, then adds
;;; and returns a new plot-agg of type TYPE.
(defun clear-and-add-plot-agg (win type &key (where :back) add (clear t))
  (when clear (clear-plot-aggs win type))
  (when add (add-plot-agg win type where)))


;;; GET-PLOT-AGG Looks for an instance of PLOT-AGG-PROTO in WIN top aggregate, type TYPE, and
;;; returns it - if not found, then create one and return it.
(defun get-plot-agg (win type &optional clear-it (where :back))
  (when clear-it (clear-plot-aggs win type))
  (if (find-plot-agg win type) (car (find-plot-agg win type)) (add-plot-agg win type where)))


#|
(defun renew-plot-agg (win type &optional (where :back))
  (when (g-value win :aggregate)
    (loop for comp in (g-value win :aggregate :components)
	  when (eql type comp))))
|#

(defun list-of-unique-titles-and-windows (&optional (windows *output-windows*))
  (let ((unique-tail 1)
	(titles-so-far)
	(out))
    (loop for win in windows
	  unless (member (g-value win :title) titles-so-far :test 'string=)
	  do (push (list (g-value win :title) win) out)
	  (push (g-value win :title) titles-so-far)
	  else do
	  (let ((new-title (format nil "~a (alternate ~D)" (g-value win :title) unique-tail)))
	    (push (list new-title win) out)
	    (push new-title titles-so-far))
	  (incf unique-tail)
	  finally (return out))))


(defun window-selection-menu (&optional (title "Select Windows") windows pre-selected-window-list only-one)
  (let* ((windows (flatten-list (or windows (clean-up-*output-windows*))))
	 (chosen-wins (choose-list-values-from-keys
		       (reverse (list-of-unique-titles-and-windows windows))
		       (flatten-list
			(case pre-selected-window-list
			  (:all *output-windows*)
			  (t pre-selected-window-list)))
		        :label title
			:max-per-menu 30
		        :punt-if-only-one-entry nil
		        :only-one-choice only-one
		       ; :do-all-at-once t
		       )))
    chosen-wins))


(defun win-menu (&optional (title "Select Windows") (windows (clean-up-*output-windows*))
			   pre-selected-window-list only-one)
  "Returns a list of output windows selected with a menu."
  (window-selection-menu title windows pre-selected-window-list only-one))

   
;; Destroy a window identified by the mouse.
(defun mdw ()
  "Destroy any window selected by the mouse."
  (let ((win (nth 1 (garnet-debug::ident))))
     (clear-window win t)))


(defun Identify-window ()
  "Identifies any window selected by the mouse."
 (nth 1 (garnet-debug::ident)))


(defun click-window ()
  (Identify-window))
 




;; ******************************************

;; Time and Date functions.
;; ******************************************

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
  (system::format-universal-time t (+ (truncate (* 10 time-stamp)) *universal-time-conversion-factor*))
  (unless (= 1 *time-stamp-suffix*)
    (format t " (~:r in this 10 second period)" *time-stamp-suffix*)))


;; ******************************************
;; Directory creation functions.
;; ******************************************

;; CREATE-PATH If pathname exists, return the path associated with it. Otherwise create the entire
;; path and return the path.
(defun create-path (pathname)
  (or (probe-file pathname)
      (when (> (length pathname) 1)
	(let ((sub-pathname (string-head pathname (1+ (find-tail-char pathname #\/ 1)))))
	  (create-path sub-pathname)
	  (unix:unix-mkdir (ext:unix-namestring pathname nil) #o777)
	  (when (probe-file (ext:unix-namestring pathname nil))
	    (pathname pathname))))))


(defun print-date ()
  (let (second minute hour date month year day daylight zone)
    (multiple-value-setq (second minute hour date month year day daylight zone)
	(get-decoded-time))
    (format t "~D ~D ~D"
	    (nth (1- month) '("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))
	    date year)))


(defun get-dated-directory (sub-directory-name &optional (time-stamp *actual-time-stamp*) make-dated-subdir)
  (let ((pathname-directory (concatenate 'string "/" sub-directory-name "/")))
    (when make-dated-subdir
      (let (second minute hour date month year day daylight zone)
	(multiple-value-setq (second minute hour date month year day daylight zone)
	  (decode-universal-time (+ (truncate (* 10 time-stamp)) *universal-time-conversion-factor*)))
	(setq pathname-directory (concatenate 'string pathname-directory (format nil "~D_~D_~D/" month date year)))))
    (setq pathname-directory (REPLACE-REPEATED-CHARACTER-W-SINGLE pathname-directory "/"))
    (create-path pathname-directory)
    pathname-directory))


;; These functions are redefined in sys/misc.lisp to function within the Surf-Hippo environment.
(defun get-plot-directory ()
  (format nil "~A/" *plot-directory*))

(defun get-plot-code-directory ()
  (format nil "~A/" *plot-code-directory*))

(defun get-data-directory ()
  (format nil "~A/" *data-directory*))
		  
(defun remove-components-in-list (list win)
  (loop for part in list do
	(if (consp part)
	  (remove-components-in-list part win)
	  (opal:remove-component (g-value win :aggregate) part))))

(defun arrange-windows (&key (windows-per-row *arrange-windows-per-row*) wins use-menu
			     (window-tile-x-gap *window-tile-x-gap*) (window-tile-y-gap *window-tile-y-gap*))
  "Retiles the WINS, according to WINDOWS-PER-ROW. If WINS is nil, or if USE-MENU is T, then a window menu will choose the retiled windows. If
WINDOWS-PER-ROW is nil, then there will be a menu for this as well. If WINS is :ALL, then all *OUTPUT-WINDOWS* will be arranged. Tiling references the
largest width and height over all windows."
  (let* ((wins (if (eq :all wins)
		 (clean-up-*output-windows*)
		 (flatten-list (if (or use-menu (not wins)) (win-menu "Select Windows to Arrange" wins) wins))))
	 (num-wins (length wins))
	 (maximum-window-width (loop for win in wins maximizing (g-value win :width)))
	 (maximum-window-height (loop for win in wins maximizing (g-value win :height)))
	 (x 0)(y 0)(row-count 0)(column-count 0))
    (multiple-value-bind (window-tile-x-gap window-tile-y-gap max-row)
	(let ((dummy1 window-tile-x-gap)
	      (dummy2 window-tile-y-gap)
	      (dummy3 (min (or windows-per-row *arrange-windows-per-row*) num-wins)))
	  (unless windows-per-row
	    (choose-variable-values
	     `((:comment ,(format nil "There are ~A selected windows" num-wins))
	       (dummy1 "Window tiling horizontal gap (pixels)" :integer)
	       (dummy2 "Window tiling vertical gap (pixels)" :integer)
	       (dummy3 "Number of windows per row (max)" :integer))
	     :label "Format for Arranging Windows"))
	  (values dummy1 dummy2 dummy3))
      (setq *arrange-windows-per-row* max-row)
      (let ((last-win-height))
	(loop for win in wins
	      do (setq row-count (1+ row-count))
	      (s-value win :left x)
	      (s-value win :top y)
	      (format nil "set left to ~A (value is ~A), top to ~A (value is ~A)~%"
		      x (g-value win :left)
		      y (g-value win :top))
	      (setq last-win-height (g-value win :height))
	      (opal:update win t)
	      (if (= row-count max-row)
		(setq row-count 0 
		      x 0 
		      y (+ y (if (= max-row 1) last-win-height maximum-window-height) *window-tile-fudge-factor*
			   window-tile-y-gap
			   )
		      column-count (1+ column-count))
		(setq x (+ x maximum-window-width window-tile-x-gap
			   *window-tile-fudge-factor*))))
	wins))))



(defun visible-windows ()
  (loop for win in (garnet-debug:windows)
	when (and (g-value win :visible)
		  (not (eq (g-value win :visible) :iconified)))
	collect win))


(defun add-titles-to-those-without (windows)
  (loop for win in windows
	unless (or (retrieve-window-agg-comp win window-title)
		   (eq :keys (g-value win :auxiliary-type)))
	do (add-title win) and collect win))


(defun move-window (win left top)
  (s-value win :left (round left))
  (s-value win :top (round top))
  (opal:update win))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(export '(basic-graphics-window
	  component-top component-left comp-top comp-left
	  *background-border-width*
	  UPDATE-*STANDARD-INFO-OUTPUT* UPDATE-*STANDARD-GRAPHICS-OUTPUT*
	  match-win-dimensions-menu
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
	  TRANSFER-SCHEMA-SLOTS
	  window-interactor-wrapper
	  toggle-window-lock-Interactor
	  initialize-window-system-variables
	  *DISPLAYED-HOST-NAME*	  
	  *add-host-name-to-windows*
	  *always-add-host-name-to-windows*
	  GET-win-TITLE-STRING
	  HOST-IS-DISPLAY-SERVER
	  encode-time-stamp decode-time-stamp create-path get-dated-directory
	  PRINT-DATE
	  interactors-running clear-interactors-running-flags
	  update-*Twin* CLEAR-WINDOWS help-window-Interactor  info top-scroll
	  find-comment clear-windows-of-mode ADD-WINDOW-COORDS-POINTER
	  clear-window add-comment add-temp-comment
	  REMOVE-COMMENT
	  resurrect-opal-win initialize-graphics-window
	  x-y-in-win
	  x-y-in-win-values
	  add-window-zoom clear-all-output-windows  caows adjust-all-labels add-label
	  window-menu-interactor add-title remove-title
	  ADD-WINDOW-TITLE-SUFFIX ADD-WINDOW-TITLE-PREFIX
	  remove-all-cross-hairs
	  clear-and-add-plot-agg destroy-agg clear-plot-aggs add-plot-agg get-plot-agg
	   get-plot-directory get-data-directory
	  GET-TOP-AGG-COMP get-agg
	  *screen-width* *screen-height*
	  *standard-graphics-width* *standard-graphics-height*
	  *raise-output-windows* *deiconify-output-windows*
	  *show-output-windows* *plotting-window-top*  
	  *update-output-windows* 
	  *hide-output-windows*
	  *STANDARD-GRAPHICS-OUTPUT* *STANDARD-INFO-OUTPUT*
	  *colorized-windows*
	  *COMMENT-FONT*	  
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
	  UNLOCK-ALL-WINDOWS
	  unlock-windows 
	  LOCK-ALL-WINDOWS
	  lock-windows
	  lock-new-windows
	  GET-WINDOW-COMMENT
	  WINDOW-COMMENT
	  window-temp-comment
	  GET-TOP-LEVEL-THING WINDOWS-OF-MODE

	  mark-point-cross
	  mark-point-dot
	  mark-point-circle
	  MOVE-WINDOW
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

	  REMOVE-MARKER
	  CLEAN-NAME

	  data-to-points-function
	  REFRESH-MARKER-POSITION REFRESH-MARKERs-POSITION
	  IDENTIFY-WINDOW click-window mdw unlock-window lock-window clean-windows UNSTICK-WINDOWS ARRANGE-WINDOWS
	  hide-window-interactor-function refresh-window-Interactor
	  visible-windows
	  *window-tile-fudge-factor*
	  *WINDOW-TILE-x-GAP*
	  *WINDOW-TILE-Y-GAP*
	  *DEFAULT-ALIGN-TO-WINDOW-SPECIFICATION*
	  ALIGN-TO-WINDOW
	  *arrange-windows-per-row*

	  *default-marker-label-position*
	  *default-marker-point-type*
	  *default-marker-point-width*
	   *default-marker-point-height*
	   *default-marker-options*
	  *DEFAULT-ADD-VALUES-TO-MARKER-LABEL*

	  ))

;; From latest windows-hack.lisp
(defun fix-window-size (win)
  (s-value win :min-width (gv win :width))
  (s-value win :max-width (gv win :width))
  (s-value win :min-height (gv win :height))
  (s-value win :max-height (gv win :height)))

(export '(fix-window-size))


