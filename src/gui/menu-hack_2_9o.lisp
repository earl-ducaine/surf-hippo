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


;; GUI Source file: menu-hack.lisp


(IN-PACKAGE "WINDOWS-HACK")



;;; This file contains some basic menu routines, inspired by the CHOOSE-VARIABLE-VALUES style of
;;; menus in the Symbolics window system, and built upon the Garnet GUI toolset from CMU. This
;;; system requires that WINDOW-HACK be loaded.


;; Set this to T for Motif look-and-feel.
(defvar *motif-gui-p* t)

(s-value garnet-gadgets:motif-check-button :button-width 18)
(s-value garnet-gadgets:motif-check-button-panel :button-width 18)
(s-value garnet-gadgets:motif-radio-button-panel :button-width 18)

(defvar *window-manager-char-width 8)	; This is empirical for TWM.
(defvar *window-manager-mouse-box-width 18)	; This is empirical for TWM.

(defvar *menu-entry-v-spacing 10)




(defun t-or-nil-p (thing)
  "Predicate for the explicit values T or NIL."
  (or (equal thing t)
      (equal thing nil)))

(defun generate-and-set-special-vars (values)
  (let (sym)
    (loop for value in values for i from 1
	  do (setq sym (gentemp))
	  (eval (list 'defvar sym value))
	  collect sym)))


;; change 2/3/99
(s-value gg::MOTIF-CHECK-BUTTON-PANEL
	 :strings (o-formula (mapcar #'(lambda (item-obj)
					 (if (stringp item-obj)
					     item-obj
					     (if (schema-p item-obj)
						 item-obj
						 ;; Must be an atom
						 (nice-string-from-atom item-obj)
						 ;; This was killing colons
						 ; (string-capitalize (string-trim ":" item-obj))
						 )))
				     (gvl :item-objs))))

;;; So that dashes and underscores in atoms are not shown in menu labels.
(s-value gg::MOTIF-RADIO-BUTTON-TEXT-LABEL-PROTOTYPE
	 :string (o-formula (let ((s (gv (kr-path 0 :parent) :string)))	
			      (if nil	; (stringp s)
				  s
				  (nice-string-from-atom s)))))

(s-value gg::MOTIF-CHECK-BUTTON-TEXT-LABEL-PROTOTYPE
	 :string (o-formula (let ((s (gv (kr-path 0 :parent) :string)))
			      ; (format t ":string - ~s, which is a ~A~%" s (type-of s))
			      (if nil	; (stringp s)
				  s
				  (string-trim ":" s ; (replace-chars-w-space-in-string s '( #\_))
					       )
				  ; (nice-string-from-atom s)
				  ))))
				  
(s-value gg::BESIDE-BUTTON-TEXT
	 :string (o-formula (let ((s (gv (kr-path 0 :parent) :string)))
			      (if (stringp s)
				  s
				   (replace-chars-w-space-in-string (string s) '( #\_))
					; (nice-string-from-atom s)
				  ))))

       
;; This is a way for now to force menu output by pausing for a moment
;; between menu window creation and the wait-interactor.  
(defvar *menu-sleep-time* 0.3)		; Seconds


;; These will let us expose menus wherever the last menu was placed.
(defvar *menu-top* 0)
(defvar *menu-left* 0)

(defvar *menu-width-fudge* 30)		; For setting the final width of a menu - added to the width
					; taken up by the various menu interactors plus the width of the OK button.

(defvar *menu-height-fudge* 30)

(defvar *menu-comment-font* (opal:get-standard-font :sans-serif :bold-italic :medium))
(defvar *menu-text-font* (opal:get-standard-font :sans-serif :bold :medium))
(defparameter *menu-font* (create-instance nil opal:font (:face :bold)))
(defparameter *menu-button-font* (opal:get-standard-font :sans-serif :bold :medium))
(defparameter *color-menu-button-font* (opal:get-standard-font :sans-serif :roman :small))

(create-instance '*menu-win* inter:interactor-window
		 (:visible nil) (:icon-title "SH Menu") (:mode :menu) (:top 20) (:left 10)) 

(create-instance '*ss-win-w-bars* gg:scrolling-window-with-bars (:visible nil)
		 (:icon-title "SH Menu") (:h-scroll-bar-p nil) (:scr-trill-p nil)
		 (:mode :menu))

;; Access the :value slot to initialize the formula in the slot and establish dependencies (Garnet
;; Manual p.300).
(g-value *ss-win-w-bars* :value)

(create-instance '*motif-ss-win-w-bars* gg:motif-scrolling-window-with-bars (:visible nil)
		 (:icon-title "SH Menu") (:h-scroll-bar-p nil) (:scr-trill-p nil) (:mode :menu))
		 
;; Access the :value slot to initialize the formula in the slot and establish dependencies (Garnet
;; Manual p.300).
(g-value *motif-ss-win-w-bars* :value)

#|
(create-instance '*scrolling-menu* gg:scrolling-menu (:icon-title "SH Menu")  (:mode :menu)
		 (:visible nil)
		 (:h-scroll-bar-p nil) (:scr-trill-p nil))

;; Access the :value slot to initialize the formula in the slot and establish dependencies (Garnet
;; Manual p.300).
(g-value *scrolling-menu* :value)		
|#

(defun get-menu-window (height label)
  (let ((win (create-instance nil *menu-win* (:top *menu-top*) (:left *menu-left*) (:height height)
			      (:title (GET-win-TITLE-STRING label))
			      (:aggregate (create-instance nil opal:aggregate)))))
    (create-instance NIL destroy-window-Interactor (:Window win))
    win))


(defun get-regular-menu-window (height width label &optional title)
  (let ((win (create-instance nil *menu-win*
			      (:background-color (when *motif-gui-p* OPAL:MOTIF-GRAY))
			      (:height height)
			      (:width width) (:left *menu-left*)
			      ; (:top top)
			      (:top *menu-top*)
			      (:title (or title (GET-win-TITLE-STRING label)))
			      (:aggregate (create-instance nil opal:aggregate)))))
    (create-instance NIL destroy-window-Interactor (:Window win))
    (create-instance nil resurrect-window-Interactor (:Window win))
    win))






(proclaim '(notinline retrieve-menu-thing))
(defun retrieve-menu-thing (array prototype)
  (declare (optimize (safety 0) (speed 3) (space 0)))
  (loop for index from 0 to 49
	when (and (opal-obj-exists (aref (the (simple-array opal::schema (*)) array) index))
		  (not (g-value (aref (the (simple-array opal::schema (*)) array) index) :in-use)))
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

(create-instance '*motif-ok-button* garnet-gadgets:motif-text-button
		 (:string "OK")
		 (:font (opal:get-standard-font :serif :bold :large))
		 (:top (o-formula
			(if (and (gvl :window :SCROLL-WIN-GADGET)
				 (gvl :window :SCROLL-WIN-GADGET :Y-OFFSET))
			    (- 5 (the fn (gvl :window :SCROLL-WIN-GADGET :Y-OFFSET)))
			    5)))
		 (:selection-function #'ok-function))


;;Access the :value slot to initialize the formula in the slot and establish dependencies (Garnet
;;Manual p.300).
(g-value *ok-button* :value)
(g-value *motif-ok-button* :value)		

;; ADD-OK-BUTTON Adds a properly positioned OK button interactor to TOP-AGG. The width of the object is 43.
(defvar *ok-buttons* (sequence-to-gen-array (loop for i from 1 to 50 collect (create-instance nil *ok-button*))))
(defvar *motif-ok-buttons* (sequence-to-gen-array (loop for i from 1 to 50 collect (create-instance nil *motif-ok-button*))))

(defun add-ok-button (win top-agg &key (label "OK"))
  (declare (optimize (safety 0) (speed 3) (space 0)))
  (let ((ok-button
	 (if *motif-gui-p*
	     (retrieve-menu-thing *motif-ok-buttons* *motif-ok-button*)
	     (retrieve-menu-thing *ok-buttons* *ok-button*))))
    (s-value ok-button :left (o-formula (- (the fn (if (gvl :window :SCROLL-WIN-GADGET)
						       (gvl :window :SCROLL-WIN-GADGET :width)
						       (gv win :width)))
					   (the fn (+ (the fn (if (gvl :window :SCROLL-WIN-GADGET) 20 10))
						      (the fn (gvl :width)))))))
    (if *motif-gui-p*
	(s-value ok-button :foreground-color opal:green)
	(s-value ok-button :white-field :filling-style opal:green-fill))
    (s-value ok-button :string label)
    (g-value ok-button :value)
    (s-value ok-button :value nil)
    (opal:add-component top-agg ok-button)))


(defun update-*ok-button* (win)
  (declare (optimize (safety 0) (speed 3) (space 0)))
  (s-value *ok-button* :left (- (the fn (g-value win :width)) 70))
  (s-value *ok-button* :parent nil)
  *ok-button*)





;;;;;;;;;;;;;;;;;;;;;;;

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


;;Access the :value slot to initialize the formula in the slot and establish dependencies (Garnet
;;Manual p.300).
(g-value *abort-button* :value)		

;; ADD-ABORT-BUTTON Adds a properly positioned ABORT button interactor to TOP-AGG. The width of the object is 43.
(defvar *abort-buttons* (sequence-to-gen-array (loop for i from 1 to 50 collect (create-instance nil *abort-button*))))

(defun add-abort-button (win top-agg &key (label "ABORT"))
  (declare (optimize (safety 0) (speed 3) (space 0)))
  (let ((abort-button (retrieve-menu-thing *abort-buttons* *abort-button*)))
    (s-value abort-button :left (o-formula (- (the fn (if (gvl :window :SCROLL-WIN-GADGET)
						       (gvl :window :SCROLL-WIN-GADGET :width)
						       (gv win :width)))
					   (the fn (+ (the fn (if (gvl :window :SCROLL-WIN-GADGET) 20 10))
						      (the fn (gvl :width)))))))
    (s-value abort-button :white-field :filling-style opal:blue-fill)
    (s-value abort-button :string label)
    (g-value abort-button :value)
    (s-value abort-button :value nil)
    (opal:add-component top-agg abort-button)))


(defun update-*abort-button* (win)
  (declare (optimize (safety 0) (speed 3) (space 0)))
  (s-value *abort-button* :left (- (the fn (g-value win :width)) 70))
  (s-value *abort-button* :parent nil)
  *abort-button*)



;;;;;;;;;;;;;;;;;;;;;

(defvar *radio-button*)
(defun test-radio ()
  (setq *radio-button* (create-instance nil garnet-gadgets:radio-button (:constant '(T)))))

			 
(defvar *modify* t)

(defun test-ch ()
  (choose-variable-values '(( *modify* "Modify clamp stimulus" :boolean))
			  ':label "More questions...."))


(create-instance 'clear-all-button garnet-gadgets:radio-button
		 (:string "Clear All")
		 (:font (opal:get-standard-font :serif :bold-italic nil))
		 (:selection-function
		  #'(lambda (object value) 
		      (declare (ignore value))
		      (s-value (g-value object :panel) :value nil)
		      (s-value object :value nil)
		      (opal:update (g-value object :window) t))))


;; ADD-clear-all-BUTTON Adds a properly positioned clear-all button interactor to TOP-AGG.
(defun add-clear-all-button (win top-agg panel)
  (let ((button (create-instance nil clear-all-button
				 (:panel panel)
				 (:top (o-formula
					(if (and (gvl :window :SCROLL-WIN-GADGET)
						 (gvl :window :SCROLL-WIN-GADGET :Y-OFFSET))
					    (- (- (the fn (gv win :height)) 35)
					       (the fn (gvl :window :SCROLL-WIN-GADGET :Y-OFFSET)))
					    (- (the fn (gv win :height)) 35))))

				 (:left (o-formula (- (the fn (if (gvl :window :SCROLL-WIN-GADGET)
								  (gvl :window :SCROLL-WIN-GADGET :width)
								  (gv win :width)))
						      (+ (if (gvl :window :SCROLL-WIN-GADGET)
							     20 0)
							     (the fn (gv :self :width)))))))))
    (s-value button :constant
	     (remove ':white-field (g-value garnet-gadgets:radio-button :constant)))
    (s-value button :white-field :filling-style opal:cyan-fill)
    ;;Access the :value slot to initialize the formula in the slot and establish dependencies (Garnet
    ;;Manual p.300).
    (g-value button :value)
    (opal:add-component top-agg button)))

(create-instance 'select-all-button garnet-gadgets:radio-button
		 (:string "Select All")
		 (:font (opal:get-standard-font :serif :bold-italic nil))
		 (:selection-function
		  #'(lambda (object value) 
		      (declare (ignore value))
		      (s-value (g-value object :panel) :value
			       (remove "CANCEL"(g-value object :panel :items) :test 'string=))
		      (s-value object :value nil)
		      (opal:update (g-value object :window) t))))

;; ADD-select-all-BUTTON Adds a properly positioned select-all button interactor to TOP-AGG.
(defun add-select-all-button (win top-agg panel)
  (let ((button (create-instance nil select-all-button
				 (:panel panel)
				 (:top (o-formula
					(if (and (gvl :window :SCROLL-WIN-GADGET)
						 (gvl :window :SCROLL-WIN-GADGET :Y-OFFSET))
					    (- (- (the fn (gvl :window :SCROLL-WIN-GADGET :height)) 65)
					       (the fn (gvl :window :SCROLL-WIN-GADGET :Y-OFFSET)))
					    (- (the fn (gvl :window :height)) 65))))
				 
				 (:left (o-formula (- (the fn (if (gvl :window :SCROLL-WIN-GADGET)
								  (gvl :window :SCROLL-WIN-GADGET :width)
								  (gv win :width)))
						      (+ (if (gvl :window :SCROLL-WIN-GADGET)
							     20 0)
							 (the fn (gv :self :width)))))))))

    (s-value button :constant
	     (remove ':white-field (g-value garnet-gadgets:radio-button :constant)))
    (s-value button :white-field :filling-style opal:cyan-fill)
    ;;Access the :value slot to initialize the formula in the slot and establish dependencies (Garnet
    ;;Manual p.300).
    (g-value button :value)
    (opal:add-component top-agg button)))

(defun add-ok-select-and-clear-all-buttons (win top-agg panel)
  (let ((ok-button (add-ok-button win top-agg)))
    ;; Adjust width for all the extra buttons on the right, considering that ADD-OK-BUTTON also
    ;; adjusted the width.
    (s-value win :width
	     (+ (g-value win :width)
		10			; A space between the left column of buttons and the right buttons.
		(max (g-value clear-all-button :width)
		     (g-value select-all-button :width)
		     (g-value ok-button :width))
		(- (g-value ok-button :width))))
    (s-value win :height
	     (max (g-value win :height)
		  (+ (g-value ok-button :height)
		     20
		     (g-value clear-all-button :height)
		     (g-value select-all-button :height))))
    (add-select-all-button win top-agg panel)
    (add-clear-all-button win top-agg panel)))


(create-instance '*x-button-panel* garnet-gadgets:x-button-panel
 (:left 10) (:font *menu-font*))
;;Access the :value slot to initialize the formula in the slot and establish dependencies (Garnet
;;Manual p.300).
(g-value *x-button-panel* :value)		


(defun labeled-box-int-set-button (global-var-menu-list &optional (top 20))
  (let ((gadget
	 (create-instance
	  nil (if *motif-gui-p* garnet-gadgets:motif-scrolling-labeled-box garnet-gadgets:labeled-box)
	  (:original-value  (format nil "~A" (round (eval (car global-var-menu-list)))))
	  (:variable (car global-var-menu-list))
	  (:top top) (:left 10) ; (:constant t)
	  (:label-offset 10)
	  (:label-string (return-comment-string-or-var global-var-menu-list))
	  (:selection-function 
	   #'(lambda (labeled-box-object value)
	       (let ((input (read-from-string value)))
		 (cond ((numberp input)
			(set (g-value labeled-box-object :variable) (round input))
			(s-value labeled-box-object :original-value
				 (round (read-from-string (g-value labeled-box-object :value)))))
		       (t
			(s-value labeled-box-object :value
				 (g-value labeled-box-object :original-value))))))))))
    ;;Access the :value slot to initialize the formula in the slot and establish dependencies (Garnet
    ;;Manual p.300).
    (g-value gadget :value)		
    (s-value gadget :value (format nil "~A" (round (eval (car global-var-menu-list)))))
    (when *motif-gui-p* (s-value gadget :width (+ (g-value gadget :label-text :width) 100)))
    gadget))

#|
(defun return-comment-string-or-var (global-var-menu-list)
  (declare (optimize (safety 0) (speed 3) (space 0)))
  (if (stringp (cadr global-var-menu-list))
      (cadr global-var-menu-list)
      (string (car global-var-menu-list))))
|#

;; Given a parameter list for a menu entry, return the first string found in that list. If no string found, then return a stringified version of the
;;first element in the list, which should be a symbol corresponding to the global variable referenced by the list. 
(defun return-comment-string-or-var (global-var-menu-list)
  (loop for element in global-var-menu-list
	when (stringp element) do (return element)
	finally (return (string (car global-var-menu-list)))))

(defvar *max-text-box-width* 100)

#|
(defun labeled-box-text-set-button (global-var-menu-list &optional (top 20))
  (declare (optimize (safety 0) (speed 3) (space 0)))
  (let* ((original-value (format nil "~A" (symbol-value (car global-var-menu-list))))
	 (gadget (create-instance
		 nil garnet-gadgets:scrolling-labeled-box
		 (:original-value original-value)
		 (:variable (car global-var-menu-list))
		 (:top top) (:left 10) (:label-offset 10)
		 (:font *menu-font*)
		 (:label-string (return-comment-string-or-var global-var-menu-list))
		 (:selection-function 
		  #'(lambda (scrolling-labeled-box-object value)
		      (set (g-value scrolling-labeled-box-object :variable) value)
		      (s-value scrolling-labeled-box-object :original-value
			       (g-value scrolling-labeled-box-object :value)))))))

    ;;Access the :value slot to initialize the formula in the slot and establish dependencies (Garnet
    ;;Manual p.300).
    (g-value gadget :value)
    (s-value gadget :value original-value)
;    (s-value gadget :width
;             (min *max-text-box-width*
;                  (+ (opal::string-width *menu-font* original-value)
;                     (opal:string-width *menu-font* (g-value gadget :label-string)))))
    gadget))
|#

(defun labeled-box-text-set-button (global-var-menu-list &optional (top 20))
  (declare (optimize (safety 0) (speed 3) (space 0)))
;;  (break)
  (let* ((variable-symbol (car global-var-menu-list))
	 (original-value (format nil "~a" (symbol-value variable-symbol)))
	 (gadget (create-instance
		  nil (if *motif-gui-p*
			  garnet-gadgets:motif-scrolling-labeled-box
			  garnet-gadgets:scrolling-labeled-box)
		  (:original-value original-value)
		  (:variable variable-symbol)
		  (:top top) (:left 10) (:label-offset 10)
		  (:font *menu-font*)
		  (:label-string (return-comment-string-or-var global-var-menu-list))
		  (:selection-function 
		   #'(lambda (scrolling-labeled-box-object value)
		       (set (g-value scrolling-labeled-box-object :variable) value)
		       (s-value scrolling-labeled-box-object :original-value
				(g-value scrolling-labeled-box-object :value)))))))
    (set variable-symbol original-value) ; Make sure that the var is set to a string.
    ;;Access the :value slot to initialize the formula in the slot and establish dependencies (Garnet
    ;;Manual p.300).
    (printvars gadget)

    (g-value gadget :value)
    (s-value gadget :value original-value)
    (s-value gadget :width
	     (+ (max 30 (opal::string-width (g-value gadget :field-font) original-value))
		30
		(opal:string-width *menu-font* (g-value gadget :label-string))))
;    (s-value gadget :width
;	     (+ (max 30 (opal::string-width (g-value gadget :field-font) original-value))
;		30
;		(opal:string-width *menu-font* (g-value gadget :label-string))))
    gadget))

(defun labeled-box-symbol-set-button (global-var-menu-list &optional (top 20))
  (declare (optimize (safety 0) (speed 3) (space 0)))
  (let* ((variable-symbol (car global-var-menu-list))
	 (original-value (format nil "~s" (symbol-value variable-symbol)))
	 (gadget (create-instance
		  nil garnet-gadgets:scrolling-labeled-box
		  (:original-value original-value)
		  (:variable (car global-var-menu-list))
		  (:top top) (:left 10) (:label-offset 10)
		  (:font *menu-font*)
		  (:label-string (return-comment-string-or-var global-var-menu-list))
		  (:selection-function 
		   #'(lambda (scrolling-labeled-box-object value)
		       (set (g-value scrolling-labeled-box-object :variable) (read-from-string value))
		       (s-value scrolling-labeled-box-object :original-value
				(g-value scrolling-labeled-box-object :value)))))))
    (set variable-symbol (read-from-string original-value)) ; Make sure that the var is set to a symbol.
    ;;Access the :value slot to initialize the formula in the slot and establish dependencies (Garnet
    ;;Manual p.300).
    (g-value gadget :value)
    (s-value gadget :value original-value)
    (s-value gadget :width
	     (+ (max 30 (opal::string-width (g-value gadget :field-font) original-value))
		30
		(opal:string-width *menu-font* (g-value gadget :label-string))))
    gadget))





(create-instance 'labeled-box-text-gadget
		 garnet-gadgets:scrolling-labeled-box
		 (:left 10) (:label-offset 10)
		 (:font *menu-font*)
		 (:selection-function 
		  #'(lambda (scrolling-labeled-box-object value)
		      (set (g-value scrolling-labeled-box-object :variable) value)
		      (s-value scrolling-labeled-box-object :original-value
			       (g-value scrolling-labeled-box-object :value)))))

(defvar *labeled-box-text-set-buttons* 
  (sequence-to-gen-array (loop for i from 1 to 50 collect (create-instance nil labeled-box-text-gadget))))

#|
(defun labeled-box-text-set-button (global-var-menu-list &optional (top 20))
  (declare (optimize (safety 0) (speed 3) (space 0)))
  (let ((gadget (retrieve-menu-thing *labeled-box-text-set-buttons* labeled-box-gadget)))
    (s-value gadget :top top)
    (s-value gadget :original-value (string (symbol-value (car global-var-menu-list))))
    (s-value gadget :variable (car global-var-menu-list))
    (s-value gadget :label-string (return-comment-string-or-var global-var-menu-list))
    ;;Access the :value slot to initialize the formula in the slot and establish dependencies (Garnet
    ;;Manual p.300).
    (g-value gadget :value)
    (s-value gadget :value (string (symbol-value (car global-var-menu-list))))
    (s-value gadget :width (+ 100 (the fixnum (opal:string-width *menu-font* (g-value gadget :label-string)))))
    gadget))
|#




(defvar *enable-precision-float-menus* nil)


(create-instance 'labeled-box-gadget
		 garnet-gadgets:labeled-box
		 (:left 10)(:constant '(:except :label-string :top)) (:label-offset 10)
		 (:selection-function 
		  #'(lambda (labeled-box-object value)
		      (let ((input (read-from-string value)))
			(cond ((numberp input)
			       (set (g-value labeled-box-object :variable) (float input))
			       (s-value labeled-box-object :original-value
					(g-value labeled-box-object :value)))
			      (t
			       (s-value labeled-box-object :value
					(g-value labeled-box-object :original-value))))))))

(create-instance 'motif-labeled-box-gadget
		 gg::Motif-Scrolling-Labeled-Box
		 (:left 10) ; (:constant '(:except :label-string :top))
		 (:label-offset 10)
		 (:selection-function 
		  #'(lambda (labeled-box-object value)
		      (let ((input (read-from-string value)))
			(cond ((numberp input)
			       (set (g-value labeled-box-object :variable) (float input))
			       (s-value labeled-box-object :original-value
					(g-value labeled-box-object :value)))
			      (t
			       (s-value labeled-box-object :value
					(g-value labeled-box-object :original-value))))))))



(defvar *labeled-box-float-set-buttons*
  (sequence-to-gen-array (loop for i from 1 to 50 collect (create-instance nil labeled-box-gadget))))

(defvar *motif-labeled-box-float-set-buttons*
  (sequence-to-gen-array (loop for i from 1 to 50 collect (create-instance nil motif-labeled-box-gadget))))



#|
(defun labeled-box-float-set-button (global-var-menu-list &optional (top 20))
  (let ((gadget  (create-instance
		  nil garnet-gadgets:labeled-box
		  (:original-value	; (format nil "~2,4,0,3,,g" (float (eval (car global-var-menu-list))))
		   (if (or *enable-precision-float-menus* (member :precision global-var-menu-list))
		       (format nil "~e" (float (eval (car global-var-menu-list))))
		       (format nil "~e" (float (eval (car global-var-menu-list))))))		       
		  (:variable (car global-var-menu-list))
		  (:top top) (:left 10)(:constant t) (:label-offset 10)
		  (:label-string (return-comment-string-or-var global-var-menu-list))
		  (:selection-function 
		   #'(lambda (labeled-box-object value)
		       (let ((input (read-from-string value)))
			 (cond ((numberp input)
				(set (g-value labeled-box-object :variable) (float input))
				(s-value labeled-box-object :original-value
					 (g-value labeled-box-object :value)))
			       (t
				(s-value labeled-box-object :value
					 (g-value labeled-box-object :original-value))))))))))

    ;;Access the :value slot to initialize the formula in the slot and establish dependencies (Garnet
    ;;Manual p.300).
    (g-value gadget :value)
    (s-value gadget :value		
	     (if (or *enable-precision-float-menus* (member :precision global-var-menu-list))
		       (format nil "~e" (float (eval (car global-var-menu-list))))
		       (format nil "~e" (float (eval (car global-var-menu-list))))))		       
    gadget))
|#

(defun labeled-box-float-set-button (global-var-menu-list &optional (top 20))
  (declare (optimize (safety 0) (speed 3) (space 0)))
  (let ((gadget
	 (if *motif-gui-p*
	     (retrieve-menu-thing *motif-labeled-box-float-set-buttons* motif-labeled-box-gadget)
	     (retrieve-menu-thing *labeled-box-float-set-buttons* labeled-box-gadget)))
	(value (format nil "~f"
		       (let ((car-value (eval (car global-var-menu-list))))
			 (typecase car-value
			   (float car-value)
			   (t (float car-value)))))))
    (s-value gadget :top top)
    (s-value gadget :label-string (return-comment-string-or-var global-var-menu-list))
    (s-value gadget :original-value value)				  
    (s-value gadget :variable (car global-var-menu-list))
    (when *motif-gui-p* (s-value gadget :width (+ (g-value gadget :label-text :width) 100)))
    ;;Access the :value slot to initialize the formula in the slot and establish dependencies
    ;;(Garnet Manual p.300).
    (g-value gadget :value)
    (s-value gadget :value value)		 
    gadget))



(defun menu-comment-line (global-var-menu-list &optional (top 20))
  (let ((text-gadget (create-instance nil opal:multifont-text
				      (:top top) (:left 10)(:constant t) 
				      (:current-font *menu-comment-font*))))
    (opal:insert-text text-gadget (return-comment-string-or-var global-var-menu-list))
    text-gadget))


(defun get-menu-button-label (label-string top left)
  (when (> (length label-string) 0)
    (create-instance nil opal:text (:visible t) (:top top) (:left left) (:string label-string) (:font *menu-button-font*))))


(defun x-button-selection-function  (x-button-object value)
  (declare (ignore value))
  (set (g-value x-button-object :variable)
       (g-value x-button-object :selected))
  nil)


(create-instance 'x-boolean-set-button-gadget
		 garnet-gadgets:x-button
		 (:selection-function #'x-button-selection-function)
		 (:left 10)
		 (:font *menu-font*))

(create-instance 'motif-check-boolean-set-button-gadget
		 garnet-gadgets:motif-check-button
		 (:selection-function #'x-button-selection-function)
		 (:left 10)
		 (:button-width 18)
		 (:font *menu-font*))

(defvar *x-boolean-set-buttons*
  (sequence-to-gen-array (loop for i from 1 to 50 collect (create-instance nil x-boolean-set-button-gadget))))

(defvar *motif-check-boolean-set-buttons*
  (sequence-to-gen-array (loop for i from 1 to 50 collect (create-instance nil motif-check-boolean-set-button-gadget))))

(defun x-boolean-set-button (global-var-menu-list &optional (top 20))
  (declare (optimize (safety 0) (speed 3) (space 0)))
  (let ((gadget (if *motif-gui-p*
		    (retrieve-menu-thing *motif-check-boolean-set-buttons* motif-check-boolean-set-button-gadget)
		    (retrieve-menu-thing *x-boolean-set-buttons* x-boolean-set-button-gadget))))
    (s-value gadget :variable (car global-var-menu-list))
    (s-value gadget :top top)
    (s-value gadget :string (return-comment-string-or-var global-var-menu-list))
    ;;Access the :value slot to initialize the formula in the slot and establish dependencies (Garnet
    ;;Manual p.300).
    (g-value gadget :value)
    (s-value gadget :selected (eval (car global-var-menu-list)))
    gadget))



(defun FREE-arrayed-BUTTON (button)
  (s-value button :in-use nil)
  NIL)


;;; `(function-name "Execute Function-name" :action)
(defun x-action-button (global-var-menu-list  &optional (top 20))
  (let ((gadget (create-instance
		 nil (if *motif-gui-p* garnet-gadgets:motif-check-button garnet-gadgets:x-button)
		 (:top top) (:left 10) (:font *menu-font*)
		 ; (when *motif-gui-p* (:button-width 18))
		 (:string (return-comment-string-or-var global-var-menu-list))
		 (:selection-function 
		  #'(lambda (x-button-object value)
		      (declare (ignore value))
		      (funcall (car global-var-menu-list))
		      (set (g-value x-button-object :selected) nil))))))
    ;;Access the :value slot to initialize the formula in the slot and establish dependencies (Garnet
    ;;Manual p.300).
    (g-value gadget :value)
    gadget))




;;;;;;;;;;;;;;;;;;;;;;;;;

(create-instance 'x-panel-choose-button garnet-gadgets:x-button-panel
		 (:font *menu-font*) (:left 10) (:direction :horizontal) (:rank-margin 3)
		 (:selection-function #'(lambda (x-button-object value) (set (g-value x-button-object :variable) value))))

(create-instance 'motif-check-panel-choose-button garnet-gadgets:motif-check-button-panel
		 (:font *menu-font*) (:left 10) (:direction :horizontal) (:rank-margin 3)
		 (:selection-function #'(lambda (x-button-object value) (set (g-value x-button-object :variable) value))))

(defun make-motif-check-panel-choose-button-agg ()
  (let ((agg (create-instance nil opal:aggregate))
	(label (create-instance nil opal:text (:visible t)
				(:string "")
				(:font *menu-button-font*)))
	(gadget (create-instance nil motif-check-panel-choose-button
				 (:button-width 18))))
    (s-value agg :gadget gadget)
    (s-value agg :label label)
    (opal:add-components agg gadget label)
    agg))

(defun make-x-panel-choose-button-agg ()
  (let ((agg (create-instance nil opal:aggregate))
	(label (create-instance nil opal:text (:visible t)
				(:string "")
				(:font *menu-button-font*)))
	(gadget (create-instance nil x-panel-choose-button)))
    (s-value agg :gadget gadget)
    (s-value agg :label label)
    (opal:add-components agg gadget label)
    agg))

(defun get-rank-margin (global-var-menu-list)
  (cond ((position :rank-margin global-var-menu-list)
	 (nth (1+ (position :rank-margin global-var-menu-list)) global-var-menu-list))
	((member :vertical global-var-menu-list) 3)
	(t (let ((num-items (length (nth 3 global-var-menu-list))))
	     (if (<= num-items 3) 3 (round (sqrt (* 2 (round (/ num-items 2))))))))))


(create-instance 'x-panel-choose-button-agg opal:aggregadget
		 (:top 20)
		 (:parts
		  `((:label ,opal:text
		     (:visible t)
		     (:top ,(o-formula (the fn (gvl :parent :top))))
		     (:string "")
		     (:font ,*menu-button-font*))
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
		     (:font ,*menu-button-font*))
		    (:panel ,motif-check-panel-choose-button
		     (:constant nil)
		     (:top ,(o-formula (+ (the fn (gvl :parent :label :height))
					  (the fn (gvl :parent :top))
					  5)))))))

(defvar *x-panel-choose-button-aggs*
  (sequence-to-gen-array (loop for i from 1 to 50 collect (create-instance nil x-panel-choose-button-agg))))

(defvar *motif-check-panel-choose-button-aggs*
  (sequence-to-gen-array (loop for i from 1 to 50 collect (create-instance nil motif-check-panel-choose-button-agg))))

(proclaim '(inline retrieve-x-panel-choose-button-agg))
(defun retrieve-x-panel-choose-button-agg ()
  (declare (optimize (safety 0) (speed 3) (space 0)))
  (loop for index from 0 to 49
	when (and (opal-obj-exists (aref (the (simple-array opal::schema (*)) *x-panel-choose-button-aggs*) index))
		  (not (g-value (aref (the (simple-array opal::schema (*)) *x-panel-choose-button-aggs*) index) :in-use)))
	return (progn (s-value (aref (the (simple-array opal::schema (*)) *x-panel-choose-button-aggs*) index) :in-use t)
		      (aref (the (simple-array opal::schema (*)) *x-panel-choose-button-aggs*) index))
	finally (return (create-instance nil x-panel-choose-button-agg))))

(proclaim '(inline retrieve-motif-check-panel-choose-button-agg))
(defun retrieve-motif-check-panel-choose-button-agg ()
  (declare (optimize (safety 0) (speed 3) (space 0)))
  (loop for index from 0 to 49
	when
	(and (opal-obj-exists (aref (the (simple-array opal::schema (*)) *motif-check-panel-choose-button-aggs*) index))
	     (not (g-value
		   (aref (the (simple-array opal::schema (*)) *motif-check-panel-choose-button-aggs*) index)
		   :in-use)))
	return
	(progn (s-value (aref (the (simple-array opal::schema (*)) *motif-check-panel-choose-button-aggs*) index) :in-use t)
	       (aref (the (simple-array opal::schema (*)) *motif-check-panel-choose-button-aggs*) index))
	finally (return (create-instance nil motif-check-panel-choose-button-agg))))


;;; x-panel-CHOOSE-BUTTON Operates on a list e.g. - 
;;;  `(*things-included "Include the following:" :x-choose ("First Thing" "Second Thing" "Nothing"))
;; old version
(defun x-panel-choose-button (global-var-menu-list  &optional (top 20))
  (let* ((agg (create-instance nil opal:aggregate))
	 (label (get-menu-button-label (return-comment-string-or-var global-var-menu-list) top 10))
	 (prototype (if *motif-gui-p* garnet-gadgets:motif-check-button-panel garnet-gadgets:x-button-panel))
	 (gadget (create-instance nil prototype
				  (:variable (car global-var-menu-list))
				  (:items (nth (1+ (position :x-choose global-var-menu-list))
					       global-var-menu-list))
				  (:font *menu-font*)
				  (:left (cond ((member :label-left global-var-menu-list) (+ 20 (g-value label :width)))
					       (t (+ 10 (g-value prototype :left)))))
				  (:direction :horizontal)
				  (:rank-margin (get-rank-margin global-var-menu-list))
				  (:selection-function #'(lambda (x-button-object value)
							   (set (g-value x-button-object :variable) value))))))
    (arrange-choose-buttom-label-and-gadget-top global-var-menu-list gadget label top) 
    (opal:add-components agg label gadget)
    ;;Access the :value slot to initialize the formula and establish dependencies (Garnet Manual p.300).
    (g-value gadget :value)
    (s-value gadget :value (no-nils (eval (car global-var-menu-list))))
    agg))

(defun motif-check-panel-choose-button (global-var-menu-list  &optional (top 20))
  (let* ((agg (create-instance nil opal:aggregate))
	 (label (get-menu-button-label (return-comment-string-or-var global-var-menu-list) top 10))
	 (prototype garnet-gadgets:motif-check-button-panel)
	 (gadget (create-instance nil prototype
				  (:button-width 18)
				  (:variable (car global-var-menu-list))
				  (:items (nth (1+ (position :x-choose global-var-menu-list)) ;3
					       global-var-menu-list))
				  (:font *menu-font*)
				  (:left (cond ((member :label-left global-var-menu-list)
						(+ 20 (g-value label :width)))
					       (t (g-value prototype :left))))
				  ; (:left 10)
				  (:rank-margin (get-rank-margin global-var-menu-list))
				  (:top (+ (g-value label :height) top 5)) (:direction :horizontal) (:rank-margin 3)
				  (:selection-function 
				   #'(lambda (x-button-object value) (set (g-value x-button-object :variable) value))))))
    (opal:add-components agg label gadget)
    ;;Access the :value slot to initialize the formula and establish dependencies (Garnet Manual p.300).
    (g-value gadget :value)
    (s-value gadget :value (no-nils (eval (car global-var-menu-list))))
    agg))

#|
(defun x-panel-choose-button (global-var-menu-list &optional (top 20))
  (declare (optimize (safety 0) (speed 3) (space 0)))
  (let ((agg (retrieve-x-panel-choose-button-agg)))

    (g-value agg :panel :value)
    (g-value agg :label :string)
    
    (s-value agg :label :string (nth 1 global-var-menu-list))
    (s-value agg :top top)
    (s-value agg :label :left 10)

        
    (s-value agg :panel :variable nil)
    (s-value agg :panel :items nil)
    
    (s-value agg :panel :button-width 18)
    (s-value agg :panel :button-height 18)
    
    ;;Access the :value slot to initialize the formula and establish dependencies (Garnet Manual p.300).
    (g-value agg :panel :value)
    (s-value agg :panel :value (no-nils (eval (car global-var-menu-list))))

    (s-value agg :panel :variable (car global-var-menu-list))
    (format t "items ~s~%" (nth 3 global-var-menu-list) )
    (loop for item in (g-value agg :panel :items)
	  do (opal::remove-item (g-value agg :panel) item))
    (loop for item in (nth 3 global-var-menu-list) 
	  do (opal::add-item (g-value agg :panel) item))
    (opal::notice-items-changed (g-value agg :panel))
    (g-value agg :panel :height)
    (g-value agg :label :height)
    agg))

|#


(defun radio-choose-button-item-to-string-function (item)
  (if item (when (stringp item) item) (REPLACE-UNDERSCORE-WITH-SPACE (string-capitalize (string-trim ":" item)))))

(defun radio-button-selection-function (radio-button-object value)
  (set (g-value radio-button-object :variable) value))

(create-instance 'radio-button-panel-gadget
		 garnet-gadgets:radio-button-panel
		 (:font *menu-font*)
		 (:item-to-string-function #'radio-choose-button-item-to-string-function)
		 (:selection-function #'radio-button-selection-function)
		 (:left 10))

(create-instance 'motif-radio-button-panel-gadget
		 garnet-gadgets:motif-radio-button-panel
		 (:font *menu-font*)
		 (:button-width 18)
		 (:item-to-string-function #'radio-choose-button-item-to-string-function)
		 (:selection-function #'radio-button-selection-function)
		 (:left 10))

;;; RADIO-CHOOSE-BUTTON Operates on GLOBAL-VAR-MENU-LIST, e.g. -
;;;
;;;   `(*clamp-type "Current or voltage clamp" :choose ("Current clamp" "Voltage clamp"))
;;;
;;; The order of these list elements is fixed. Subsequent keywords may be in any order.
;;;
;;; A keyword may be included to specify layout, ie :horizontal (default) or :vertical.
;;;   `(*clamp-type "Current or voltage clamp" :choose ("Current clamp" "Voltage clamp") :vertical)
;;;
;;; The default label position is on top of the radio buttons. If :LABEL-LEFT is included in the
;;; list, then the label will be to the left of the radio buttons.
;;;
;;; Also, the rank margin may be specified in this list with the keyword :RANK-MARGIN followed by an
;;; integer.
;;;
;;; For example:
;;;
;;;   `(dummy1 "Lot o' choices" :choose ("A" "B" "C" "D" "E" "F") :rank-margin 2 :vertical :label-left)
;;;
(defun radio-choose-button (global-var-menu-list &optional (top 20))
  (let* ((agg (create-instance nil opal:aggregate))
	 (label (get-menu-button-label
		 (return-comment-string-or-var global-var-menu-list)
		 ; (nth 1 global-var-menu-list)
				       top 10))
	 (prototype (if *motif-gui-p* motif-radio-button-panel-gadget radio-button-panel-gadget))
	 (gadget (create-instance
		  nil prototype
		  (:items
		   (nth (1+ (position :choose global-var-menu-list)) ;3
			global-var-menu-list))
		  (:variable (car global-var-menu-list))
		  (:left (cond ((member :label-left global-var-menu-list)
				(+ 20 (g-value label :width)))
			       (t (g-value prototype :left))))
		  (:direction (if (member :vertical global-var-menu-list) :vertical :horizontal))
		  (:rank-margin (get-rank-margin global-var-menu-list)))))
				
    (arrange-choose-buttom-label-and-gadget-top global-var-menu-list gadget label top) 
    (when label (opal:add-components agg label))
    (opal:add-components agg gadget)
    ;;Access the :value slot to initialize the formula and establish dependencies (Garnet Manual p.300).
    (g-value gadget :value)
    (s-value gadget :value (eval (car global-var-menu-list)))
    ;;    (format t "~A left: ~A, ~A left: ~A~%" agg (g-value agg :left) gadget (g-value gadget :left))
    agg))



(defun arrange-choose-buttom-label-and-gadget-top (global-var-menu-list gadget label top)  
  (if (member :label-left global-var-menu-list)
      (let (thicker-one thinner-one)
	(if (> (g-value gadget :height) (g-value label :height))
	    (setq thicker-one gadget
		  thinner-one label)
	    (setq thicker-one label
		  thinner-one gadget))
	(s-value thicker-one :top top)
	(s-value thinner-one :top (round (+ top
					    (/ (g-value thicker-one :height) 2)
					    (- (/ (g-value thinner-one :height) 2))))))
      (s-value gadget :top (+ 5 (if label (g-value label :height) 0) top))))



(defun color-option-button (global-var-menu-list &optional (top 20) (colors *basic-opal-colors*))
  (let* ((agg (create-instance nil opal:aggregate))
	 (label (get-menu-button-label (return-comment-string-or-var global-var-menu-list)
					; (nth 1 global-var-menu-list)
					top 10))
	 (gadget (create-instance
		  nil (if *motif-gui-p* garnet-gadgets:motif-option-button garnet-gadgets:option-button)
		  (:items colors)
		  (:variable (car global-var-menu-list))
		  (:font *menu-font*) (:left 10) (:top (+ (if label (g-value label :height) 0) top 5)))))		  
    (when label (opal:add-components agg label))
    (opal:add-components agg gadget)
    ;;Access the :value slot to initialize the formula and establish dependencies (Garnet Manual p.300).
    (g-value gadget :value)
    (s-value gadget :value (eval (car global-var-menu-list)))
    agg))


(defun color-choose-button-menu (keys-and-their-colors
				 label key-label
				 &key
				 return-only-single-color (xclusive-choice t) (can-quit-w/o-all-keys-assigned t)
				 (all-values *basic-opal-colors-w-white* ; *basic-opal-colors*
					     ))
  (when (and (> (length keys-and-their-colors) 0) all-values)
    (loop while t do
	  (setq keys-and-their-colors
		(loop for key-value in keys-and-their-colors collecting
		      (typecase key-value
			(cons (list (car key-value) (get-opal-color (cadr key-value))))
			(t (list key-value nil)))))
	  (let* ((button-labels (loop for value in all-values collect (kr::name-for-schema value)))
		 (key-label-font (opal:get-standard-font :serif :bold-italic :medium))
		 (height (+ 25 (g-value key-label-font :font-height)))
		 (menu-entries
		  (loop for key-value in keys-and-their-colors collecting
			(let ((gadget (create-instance
				       nil (if nil ; *motif-gui-p*
					       garnet-gadgets:motif-text-button-panel
					       garnet-gadgets:text-button-panel)
				       (:constant nil)
				       (:items button-labels) (:item-colors all-values)
				       (:key (format nil "~A" (car key-value))) (:font *color-menu-button-font*)
				       (:top height) (:direction :horizontal) ; (:rank-margin 4)
                                       (:selection-function
                                        (if xclusive-choice
                                            #'(lambda (object value)
                                                (loop for comp in (g-value object :parent :components)
                                                      when (and (not (eq comp object))
                                                                (equal value (g-value comp :value)))
                                                      do (s-value comp :value nil))))))))
			  (setq height (+ height (+ *menu-entry-v-spacing (g-value gadget :height))))
			  gadget))) 
		 (win (get-regular-menu-window height 10 (GET-win-TITLE-STRING label)))
		 (top-agg (g-value win :aggregate)))
	    (opal:add-component
	     top-agg (create-instance nil opal:text (:visible t) (:top 10) (:string key-label) (:left 10)
				      (:font key-label-font)))
	    (let ((text-width
		   (loop for menu-entry in menu-entries
			 do (opal:add-component top-agg menu-entry)
			 maximizing
			 (g-value (opal:add-component top-agg 
						      (create-instance nil opal:text
								       (:visible t) (:top (g-value menu-entry :top))
								       (:string (string (g-value menu-entry :key)))
								       (:left 10)
								       (:font key-label-font)))
				  :width))))
	      (process-color-choose-menu-entries-and-color-keys menu-entries keys-and-their-colors text-width))	    
	    (s-value win :width (+ (g-value (car menu-entries) :left) (g-value (car menu-entries) :width) 80))
	    (add-ok-button win top-agg)
	    (s-value win :height (+ (g-value win :aggregate :height) *menu-height-fudge*))
	    (resurrect-opal-win win :visible t)
	    ; (opal:update win t)
	    (inter:wait-interaction-complete win)
	    (setq keys-and-their-colors
		  (loop for menu-entry in menu-entries 
			for key-value in keys-and-their-colors 
			collect (list (car key-value) (string-to-opal-color (g-value menu-entry :value)))))
	    (when (or can-quit-w/o-all-keys-assigned
		      (loop for key-value in keys-and-their-colors when (cadr key-value) do (return t)))
	      (setq *menu-top* (max 10 (g-value win :top)))
	      (setq *menu-left* (max 10 (g-value win :left)))
	      (remove-and-free-buttons top-agg)
	      (opal:destroy win)
	      (return t))))
    (if return-only-single-color (cadar keys-and-their-colors) keys-and-their-colors)))
	




(defun process-color-choose-menu-entries-and-color-keys (menu-entries keys-and-their-colors text-width)
  (loop for menu-entry in menu-entries  
	for key-value in keys-and-their-colors do
	(s-value menu-entry :left (+ 20 text-width))
	(let ((button-list (g-value MENU-entry :text-button-list))
	      (button-color-list (g-value MENU-entry :item-colors)))
	  (dotimes (n (length (g-value button-list :items)))
	    (let ((button (nth n (g-value button-list :components)))
		  (button-color (nth n button-color-list)))
	      (s-value (g-value button :text) :line-style
		       (create-instance nil opal:line-style (:foreground-color button-color)))
	      (s-value (g-value button :shadow) :filling-style
		       (create-instance nil opal:black-fill (:foreground-color button-color)))
	      (s-value (g-value button :gray-outline) :filling-style
		       (create-instance nil opal:gray-fill (:foreground-color button-color))))))
	(s-value (g-value MENU-entry :final-feedback) :filling-style opal::gray-fill)
	(g-value menu-entry :value)
	(s-value menu-entry :value (if (cadr key-value) (kr::name-for-schema (cadr key-value))))
	(g-value menu-entry :selected)
	(s-value menu-entry :selected (if (cadr key-value) (kr::name-for-schema (cadr key-value))))))


(defun new-color-choose-button-menu (keys-and-their-colors
				     label key-label
				     &key (xclusive-choice t)(can-quit-w/o-all-keys-assigned t)
				     (all-values *basic-opal-colors*))
  (when (and (> (length keys-and-their-colors) 0) all-values)
    (loop while t do
	  (let* ((button-labels (loop for value in all-values collect (kr::name-for-schema value)))
		 (key-label-font (opal:get-standard-font :serif :bold-italic :medium))
		 (top (+ 25 (g-value key-label-font :font-height)))
		 (menu-entries
		  (loop for key-value in keys-and-their-colors collecting
			(let ((gadget (create-instance
				       nil (if *motif-gui-p*
					       garnet-gadgets:motif-text-button-panel garnet-gadgets:text-button-panel)
				       (:constant nil)
				       (:item-colors all-values)
				       (:key (string (car key-value))) (:font *color-menu-button-font*)
				       (:top top) (:direction :horizontal) ; (:rank-margin 4)
                                       (:selection-function
                                        (when xclusive-choice
					  #'(lambda (object value)
					      (loop for comp in (g-value object :parent :components)
						    when (and (not (eq comp object)) (equal value (g-value comp :value)))
						    do (s-value comp :value nil))))))))
			  (setq top (+ top (+ *menu-entry-v-spacing (g-value gadget :height))))
			  gadget))) 
		 (win (get-menu-window top label))
		 (top-agg (g-value win :aggregate)))
	    (opal:add-component top-agg	(create-instance nil opal:text (:font key-label-font)
							 (:visible t) (:top 10) (:left 10) (:string key-label)))
	    (let ((text-width (loop for menu-entry in menu-entries
				    do (opal:add-component top-agg menu-entry)
				    maximizing
				    (g-value 
				     (opal:add-component top-agg 
							 (create-instance nil opal:text
									  (:visible t) (:top (g-value menu-entry :top))
									  (:string (string (g-value menu-entry :key)))
									  (:left 10) (:font key-label-font)))
				     :width))))
	      (process-color-choose-menu-entries-and-color-keys menu-entries keys-and-their-colors text-width))
	    (s-value win :width (+ (g-value (car menu-entries) :left) (g-value (car menu-entries) :width) 80))
	    (add-ok-button win top-agg)
	    (opal:update win t)
	    (inter:wait-interaction-complete win)
	    (setq keys-and-their-colors
		  (loop for menu-entry in menu-entries collect (list (g-value menu-entry :key)
								     (string-to-opal-color (g-value menu-entry :value)))))
	    (when (or can-quit-w/o-all-keys-assigned
		      (loop for key-value in keys-and-their-colors when (cadr key-value) do (return t)))
	      (setq *menu-top* (max 10 (g-value win :top)))
	      (setq *menu-left* (max 10 (g-value win :left)))
	      (remove-and-free-buttons top-agg)
	      (opal:destroy win)
	      (return t))))
    keys-and-their-colors))


(defun xclusive-choose-button (keys-and-their-values all-values &optional can-quit-w/o-all-keys-assigned
						     &key (label "Choose Variables") text (rank-margin 3))
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
				       ;;			       (:value (cadr key-value))
				       (:constant t) ;(:font  *menu-font*)
				       (:left (o-formula (gvl :parent :max-text-width)))
				       (:top top) (:direction :horizontal) (:rank-margin rank-margin)
				       (:selection-function 
					#'(lambda (object value)
					    (loop for comp in (g-value object :parent :components) do
						  do
						  when (and
							(not (eq comp object))
							(equal value (g-value comp :value)))
						  do (s-value comp :value nil)))))))
			  (setq top (+ top (+ *menu-entry-v-spacing (g-value gadget :height))))
			  (g-value gadget :value)
			  (s-value gadget :value (cadr key-value))
			  gadget))) 

		 (win (create-instance nil *menu-win* 
				       (:visible t) (:top *menu-top*) (:left *menu-left*)
				       (:title (GET-win-TITLE-STRING label))
				       (:icon-title "SH Menu")
				       (:aggregate (create-instance nil opal:aggregate))))
		 (top-agg (g-value win :aggregate)))

	    (create-instance NIL destroy-window-Interactor (:Window win))
	    (loop for menu-entry in menu-entries
		  do (opal:add-component top-agg menu-entry)
		  maximize
		  (g-value
		   (opal:add-component top-agg 
				       (create-instance nil opal:text
							(:visible t) (:top (g-value menu-entry :top))
							(:string (string
								  (if nil ; *motif-gui-p*
								      (g-value menu-entry :value)
								      (g-value menu-entry :key))

								      ))
							(:left 10)
							(:font (opal:get-standard-font :serif :bold-italic :large))))
		   :width)
		  into max-text-width
		  do
		  (s-value win :height (+ (g-value menu-entry :top)
					  (g-value menu-entry :height)))
		  finally
		  (s-value top-agg :max-text-width (+ 15 max-text-width)))

	    (s-value win :width (+ (g-value top-agg :max-text-width)
				   (g-value (car menu-entries) :width)
				   80))
	    (s-value win :height (+ (g-value win :height) 10))
	    (when text (add-menu-text text top-agg win))
	    (add-ok-button win top-agg)
	    (opal:update win t )
	    (inter:wait-interaction-complete win)
    
	    ;; kludge since the menus tend to walk over to the left
	    (setq *menu-top* (max 10 (g-value win :top)))
	    (setq *menu-left* (max 10 (g-value win :left)))
	    (setq keys-and-their-values
		  (loop for menu-entry in menu-entries collect
			(list (g-value menu-entry :key)
			      (g-value menu-entry :value))))
	    (remove-and-free-buttons top-agg)
	    (opal:destroy win))
	  when (or can-quit-w/o-all-keys-assigned
		   (loop for key-value in keys-and-their-values
			 when (not (cadr key-value))
			 do (setq extra-text "All entries must be assigned") (return nil)
			 finally (return t)))
	  do (return t)))
  keys-and-their-values)


(defun reorder-value-list-from-keys-menu (key-value-list &key (label "Choose Ordering of List Variables")
							 unassigned-values
						   text (ok-to-leave-out-values t))
  (loop for key in 
	(reorder-list-menu-core (loop for key-value in key-value-list collect (car key-value))
			   :unassigned-values unassigned-values
			   :label label :text text :ok-to-leave-out-values ok-to-leave-out-values)
	collect (cadr (find key key-value-list :key 'car :test 'equal))))


(defun reorder-list-menu (list &key (label "Choose Ordering of List Variables")
			       unassigned-values text (ok-to-leave-out-values t) use-list-values-as-keys)
  (if nil ;use-list-values-as-keys
      (let ((key-value-list (loop for val in list collect (list val val))))
	(loop for key in
	      (reorder-list-menu-core (loop for key-value in key-value-list collect (car key-value))
				      :unassigned-values unassigned-values
				      :label label :text text :ok-to-leave-out-values ok-to-leave-out-values)
	      collect (cadr (find key key-value-list :key 'car :test 'equal))))
      (reorder-list-menu-core list :label label :unassigned-values unassigned-values
			      :text text :ok-to-leave-out-values ok-to-leave-out-values)))
			      

 
(defun reorder-list-menu-core (list &key (label "Choose Ordering of List Variables")
			       unassigned-values
			       text (ok-to-leave-out-values t))
  (unless text
    (when ok-to-leave-out-values
      (setq text "Unselected items will be left out of new list")))
  (let ((order 0))
    (loop for val in list
	  for count from 1
	  collect (list val (if (member val unassigned-values :test 'equal) "" (format nil "~D" (setq order (1+ order)))))
	  into key-values
	  collect (format nil "~D" count) into all-values
	  finally
	  (return
	    (loop for sorted-key-val in
		  (sort (loop for key-val in (xclusive-choose-button key-values all-values
								     ok-to-leave-out-values
								     :label label :text text)
			      when (and (cadr key-val) (> (length (cadr key-val)) 0))
			      collect (list (car key-val) (read-from-string (cadr key-val))))
			'< :key 'cadr)
		  collect (car sorted-key-val))))))


(defun labeled-box-thing-set-button (var-list  &optional (top 20))
  (let ((value (eval (car var-list))))
    (if (T-OR-NIL-P value)
	(x-boolean-set-button var-list top)
      (typecase value
		(symbol (labeled-box-symbol-set-button var-list top))
		(number (labeled-box-float-set-button var-list top))
		(float (labeled-box-float-set-button var-list top))
		(string (labeled-box-text-set-button var-list top))
		(fixnum (labeled-box-int-set-button var-list top))))))


(defvar *menu-text-gadgets*
  (sequence-to-gen-array (loop for i from 1 to 50 collect (create-instance nil opal:multifont-text))))


(defun add-menu-text (text top-agg win)
  (declare (optimize (safety 0) (speed 3) (space 0)))
  (let ((text-gadget (retrieve-menu-thing *menu-text-gadgets* opal:multifont-text)))
    (s-value text-gadget :current-font *menu-text-font*)
    (s-value text-gadget :left 5)
    (s-value text-gadget :word-wrap-p nil )

    ;; If text starts with a newline, we have to add a space at the beginning - otherwise
    ;; the result of opal::set-text chokes with
    ;;
    ;; OPAL::FIX-UPDATE-SLOTS-METHOD-MULTIFONT-TEXT
    ;;
    ;;      Type-error in KERNEL::OBJECT-NOT-TYPE-ERROR-HANDLER:
    ;;          NIL is not of type OPAL::FRAG
    ;;
    (when (equal (schar text 0) #\newline)
      (setq text (concatenate-strings " " text)))
    (opal::set-text text-gadget (list (list (cons text (g-value text-gadget :current-font)))))

    
    (s-value text-gadget :text-width
	     (the fn
		  (if (and (> (the fn (g-value text-gadget :width)) (the fn (g-value win :width)))
			   (< (the fn (g-value win :width)) 150))
		      150
		      (g-value win :width))))
    (s-value text-gadget :top (g-value win :height))
    (opal:add-component top-agg text-gadget)
    (s-value win :height (the fn (+ (the fn (g-value win :height))
				    (the fn (g-value text-gadget :height)))))

    (s-value win :width (max (the fn (g-value win :width))
			     (+ 15 (the fn (g-value text-gadget :width)))))
    nil))



;; CHOOSE-VARIABLE-VALUES This partly mimics the Symbolics function of the same name. When the OK
;; button is pressed, this function returns NIL. VAR-LISTS is a list of variable setting parameter
;; lists (VAR-LIST), each with the following minimum format:
;;
;;      (global-variable :type-keyword)
;;
;; NIL entries in VAR-LISTS will be ignored. If there is a string in a VAR-LIST, 
;;
;;      (global-variable "Comment string" :type-keyword)
;;
;; then this string will appear as a comment in the menu line. Otherwise, the GLOBAL-VARIABLE symbol will be printed.
;;
;; The :type-keyword can be -
;;
;;         :NUMBER
;;         :FLOAT
;;         :INTEGER
;;         :COMMENT
;;         :BOOLEAN
;;         :CHOOSE
;;         :X-CHOOSE
;;         :STRING
;;         :SYMBOL
;;
;; If :type-keyword = :STRING, then the GLOBAL-VARIABLE will be reinitialized to its print-name string.  
;; If :type-keyword = :SYMBOL, then the GLOBAL-VARIABLE will be reinitialized to a symbol based on its print-name.
;; :comment just takes a single comment string.
;;
;; If :type-keyword = :CHOOSE or :X-CHOOSE, then the variable setting parameter list format is:
;;
;;     (global-variable "Comment string" :choose (first-choice second-choice ...))
;;
;; where FIRST-CHOICE, SECOND-CHOICE, etc. may be strings or symbols. :X-CHOOSE allows multiple
;; selections from a list, while :CHOOSE allows only one chosen value. 
;;
;; The GLOBAL-VARIABLE symbol *must* be a variable that has been declared special, that is a global variable.
;; If necessary, there are a set of pre-defined DUMMYx variables [x from 0 to 30] that may be used.
;; For example, a temporary value in a function may be passed to a menu by localling binding [e.g.
;; with LET] a DUMMYx variable, and then using the symbol in a variable setting parameter list.
;;
;; The "Comment string" may be omitted - in this case the menu label will be the print-name of the
;; GLOBAL-VARIABLE. 
;;
;; For example, VAR-LISTS could be:
;;
;; '((*user-stop-time* "Length of simulation [ms]" :number)
;;   (*modify-plot-parameters "Modify plot parameters" :boolean)
;;   (*traces-per-plot* :integer)
;;   (dummy12 "Plot upside down" :boolean)
;;   (dummy4 "Current or voltage clamp" :choose ("Current clamp" "Voltage clamp"))
;;

(defun choose-variable-values (var-lists &key (label "Choose Variables") title (max-height 650) text image (image-top :bottom) (image-left :right))
  (declare (optimize (safety 0) (speed 3) (space 0))
	   (fixnum max-height)
	   (string label))
  (when (and (not *automatic-run*) var-lists)
    (let* ((var-lists (no-nils var-lists))
	   (top 10) (width 0) win top-agg
	   (label-length (the (UNSIGNED-BYTE 32) (length label)))
	   ;; Create these here because srolling windows are handled a bit differently than interactor-windows
	   (menu-entries ; VAR-LISTS can be a single list or a list of list.
	    (loop for var-list in (if (consp (car var-lists)) var-lists (list var-lists))
		  when var-list collecting
		  (let ((menu-entry
			 (cond
			   ; ((my-member-equal :color var-list) (color-option-button var-list top))
			   ((my-member-equal :thing var-list) (labeled-box-thing-set-button var-list top))
			   ((my-member-equal :string var-list) (labeled-box-text-set-button var-list top))
			   ((my-member-equal :action var-list) (x-action-button var-list top))
			   ((my-member-equal :boolean var-list) (x-boolean-set-button var-list top))
			   ((my-member-equal :symbol var-list) (labeled-box-symbol-set-button var-list top))
			   ((my-member-equal :choose var-list) (radio-choose-button var-list top))
			   ((my-member-equal :x-choose var-list) (x-panel-choose-button var-list top))
			   ((my-member-equal :float var-list) (labeled-box-float-set-button var-list top))
			   ((my-member-equal :number var-list) (labeled-box-float-set-button var-list top))
			   ((my-member-equal :integer var-list) (labeled-box-int-set-button var-list top))
			   ((my-member-equal :comment var-list) (menu-comment-line var-list top)))))	
		    (setq top (the fn (+ top
					 (the fn (+ (the fn *menu-entry-v-spacing)
						    (the fn (max (the fn (g-value menu-entry :height))
								 (the fn (if (g-value menu-entry :label-text)
									     (g-value menu-entry :label-text :height)
									     0))))))))
			  width (max width (the fn (g-value menu-entry :width))))
		    menu-entry))))
      (declare (fixnum top width))
      (setq width (max (the fn (+ width *menu-width-fudge* (g-value (if *motif-gui-p* *motif-ok-button* *ok-button*)
								    :width))) ; Allow some room for the OK button.
		       (the fn (+ (the fn (* (the fn *window-manager-char-width)
					     (the (UNSIGNED-BYTE 32) label-length)))
				  ;; 2 mouseable boxes in window title bar
				  (the fn (+ (the fn *window-manager-mouse-box-width)
					     (the fn *window-manager-mouse-box-width)))))))
      (setq top (the fn (+ top 25)))	; A little extra room
      (setq win (cond ((> top max-height)
		       (let ((swin (create-instance nil (if *motif-gui-p* *motif-ss-win-w-bars* *ss-win-w-bars*)
						    (:visible t)
						    (:left *menu-left*) (:top *menu-top*)
						    (:width (+ 0 width)) (:height max-height)
						    (:title (or title label)) (:icon-title "SH Menu")
						    (:total-height top) (:total-width (o-formula (gv :self :width))))))
			 (opal:update swin t)
			 (setq top-agg (g-value swin :inner-aggregate))
			 (create-instance NIL destroy-window-Interactor (:Window (g-value swin :outer-window)))
			 swin))
		      (t (let ((win (get-regular-menu-window top width label title)))
			   (setq top-agg (g-value win :aggregate))
			   win))))
      ; (s-value win :height top) 
      (add-menu-entries menu-entries top-agg)
      (when text (add-menu-text text top-agg win))
      (add-ok-button win top-agg)
      (when image (show-image image :win win :image-left image-left :image-top image-top))
      (resurrect-opal-win win :raise t :visible t :show t :update t :deiconify t)
      (menu-wrap-up win top-agg))))


(defun remove-and-free-buttons (top-agg)
  (declare (optimize (safety 0) (speed 3) (space 0)))
  (loop for comp in (g-value top-agg :components) when (g-value comp :in-use)
	do (FREE-arrayed-BUTTON comp) (opal:remove-component top-agg comp))
  nil)

(defun add-menu-entries (menu-entries top-agg)
  (loop for menu-entry in menu-entries do (opal:add-component top-agg menu-entry)))
      

(defun menu-wrap-up (win top-agg)
  (inter:wait-interaction-complete (or (g-value win :window) win))
  ;; kludge since the menus tend to walk over to the left
  (setq *menu-top* (max 10 (the fn (g-value win :top)))
	*menu-left* (max 10 (the fn (g-value win :left))))
  (remove-and-free-buttons top-agg)
  (opal:destroy win)
  nil)


(create-instance 'edit-text opal:aggregadget
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


(defun simple-text-menu (label &optional title (string "") (height 400) (width 400)
			       (comment "Edit stuff above"))
  (let* ((win (get-regular-menu-window height width label title))
	 (top-agg (g-value win :aggregate))
	 (axis-text (create-instance nil edit-text
				     (:visible t)
				     (:string string)
				     (:top 5)
				     (:left 10)))
	 output)
    (opal::add-component top-agg axis-text)
    (add-ok-button win top-agg)
    (add-menu-text comment top-agg win)
    (resurrect-opal-win win :visible t)
    (inter:wait-interaction-complete win)
    (setq output (g-value axis-text :label :string))
    (remove-and-free-buttons top-agg)
    (opal:destroy win)
    output))


(defun reorder-list-menu (list &optional label-list (title "Reorder List"))
  (let* ((labels (loop for val in list
		       for count from 0
		       collect (or (nth count label-list) (format nil "List element ~A" (1+ count))))))
    (loop for count from 0
	  for label in labels 
	  collect (list label (format nil "~D" (1+ count))) into menu-list
	  collect (format nil "~D" (1+ count)) into all-values
	  finally
	  (return
	    (let ((new-order
		   (loop for string-val in (XCLUSIVE-CHOOSE-BUTTON menu-list all-values t :rank-margin 6 :label title) 
			 collect (typecase (cadr string-val)
				   (string (read-from-string (cadr string-val)))
				   (t nil)))))
	      (loop for position from 1
		    for val in list
		    when (position position new-order) collect (nth (position position new-order) list)))))))


;; Given a LIST of strings, returns a list with the same number of strings, possibly edited.
(defun edit-string-list (list &key (label "Edit Strings") (entry-string "Entry") text)
  (eval (cons 'defvars-w-value
	      (loop for string in list for i from 1
		    collect (list (read-from-string (format nil "dummy-string-~d" i)) nil))))
  (loop for string in list for i from 1
	do (eval (list 'setq (read-from-string (format nil "dummy-string-~d" i)) string)))
  (choose-variable-values
   (loop for string in list for i from 1
	 collect (list (read-from-string (format nil "dummy-string-~d" i))
		       (string-capitalize (format nil "~:R ~a" i entry-string)) :string))
   :text text :label label)
  (loop for string in list for i from 1
	collect (eval (read-from-string (format nil "dummy-string-~d" i)))))

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
	   collect (list thing string	
			 (if (t-or-nil-p thing)
			     :boolean
			   (typecase thing
				     (symbol :symbol)
				     (string :string)
				     (fixnum :integer)
				     (float :float)
				     (t :boolean)))))
     :text text :label label)
    (loop for thing in menu-vars
	  for i from 1
	  collect (symbol-value thing))))
	  


;; EDIT-FUNCTION-ARGS Returns a list of arguments ARG-LIST with keywords in format that can be evaluated by -
;;
;;   (eval (cons function arg-list))
;;
#|
(defun function-arg-menu-text (extra-text doc-string only-keywords)
  (concatenate 'string
	       extra-text
	       (when (and extra-text doc-string) (format nil "~%"))
	       doc-string
	       (when doc-string (format nil "~%~%"))
	       (when only-keywords (format nil "~%Only function keyword values appear here.~%~%"))

	       (limit-string-span
		(concatenate
		 'string
		 "Note that entries which appear to be boolean (and set to NIL) "
		 "may in fact be either args w/o a default value or args that do "
		 "not appear in the current values.")
		70)
	       ;; (format nil "~% ** This edit is not 100% tested ** Verify the results independently **")
	       ))
|#
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
	((stringp thing)
	 (let ((thing (read-from-string thing)))
	   (when (and (symbolp thing) (fboundp thing)) (fdefinition thing))))
	((and (symbolp thing) (fboundp thing)) (fdefinition thing))
	(t nil)))

(defun extract-function (funspec)
  (let ((thing (if (consp funspec) (car funspec) funspec)))
    (extract-function-from-atom thing)))


(defun get-menu-entry-type-keyword (thing)
  (if nil ; (not (consp thing))
      :boolean
    (let ((value (if (and (symbolp (cadr thing))
			  (boundp (cadr thing)))
		     (symbol-value (cadr thing))
		   (cadr thing))))
      (if (t-or-nil-p value)
	  :boolean
	(typecase value
		  (number :float)
		  (symbol :symbol)
		  (string :string)
		  (fixnum :integer)
		  (float :float)
		  (t :boolean))))))

(defun get-menu-entry-type-keyword (thing)
  (if nil				; (not (consp thing))
      :boolean
    (let ((value (if (and (symbolp thing)
			  (boundp thing))
		     (symbol-value thing)
		   thing)))
      (if (t-or-nil-p value)
	  :boolean
	(typecase value
		  (number :float)
		  (symbol :symbol)
		  (string :string)
		  (fixnum :integer)
		  (float :float)
		  (t :boolean))))))

(defun extract-all-args (arglist &optional current-arg-values)
  ;; Parse the current args according to the function description.
  (let ((nothing-found t) key-found optional-found)
    (loop for arg-descriptor in arglist
	  ;; E.G.  "(tau-rise tau-fall &key (amplitude 1.0) normalize (step 1.0) length (offset 0.0))"
	  unless (case arg-descriptor	; SIGNAL LAMBDA LIST KEYWORDS
		   (&optional (setq optional-found t nothing-found nil) t)
		   (&key (setq key-found t optional-found nil nothing-found nil) t)
		   (LAMBDA-LIST-KEYWORDS (setq key-found nil optional-found nil nothing-found nil) t))
	  when nothing-found
	  collect (list arg-descriptor (car current-arg-values)) 
	  into reg-args and do (setq current-arg-values (cdr current-arg-values))
	  else when optional-found
	  collect (list (if (consp arg-descriptor) (car arg-descriptor) arg-descriptor) ; Get the symbol-var
			(or (car current-arg-values)
			    (when (consp arg-descriptor) (cadr arg-descriptor)))) ; Default value
	  into optional-args and do (setq current-arg-values (cdr current-arg-values))
	  
	  else when key-found collect
	  (list (if (consp arg-descriptor) (car arg-descriptor) arg-descriptor) ; Get the symbol-var
		(when (consp arg-descriptor) (cadr arg-descriptor)))
	  into default-key-args
	  finally
	  (return
	   (values reg-args
		   optional-args
		   (delete-duplicates
		    (concatenate 'list default-key-args
				 (let ((out '()))
				   (do ((current-arg-values current-arg-values (cddr current-arg-values)))
				       ((null current-arg-values) (reverse out))
				     (push (list (read-from-string (format nil "~a" (car current-arg-values)))
						 (cadr current-arg-values))
					   out))))
		    :key 'car))))))


;; Returns values (reg-args optional-args key-args)
(defun extract-funspec-args (funspec &optional current-arg-values return-as-single-list)
  (let ((current-arg-values (or current-arg-values (when (consp funspec) (cdr funspec))))
	(function (extract-function funspec)))
    (unless (or (not function) (eval:interpreted-function-p function))
      (multiple-value-bind (reg-args optional-args key-args)
	  (extract-all-args (read-from-string (lisp::%function-arglist function)) current-arg-values)
	(if return-as-single-list
	    (no-nils
	     (concatenate 'list reg-args
			  (concatenate 'list optional-args key-args)))
	  (values reg-args optional-args key-args))))))


(defun function-required-args (funspec)
  (let ((function (extract-function funspec)))
    (when function
      (loop for symbol in (read-from-string (lisp::%function-arglist function))
	    until (member symbol LAMBDA-LIST-KEYWORDS) collect symbol))))

(defvar *edit-function-menu-doc-string-width* 90)

(defun edit-function-args (funspec &optional current-arg-values &key label extra-text only-keywords)
  (let ((function (extract-function funspec)))
    (cond ((eval:interpreted-function-p function)
	   (format t "~A is an interpreted function, so I'm not going to edit it...." function))
	  (function
	   (let* ((function-name (lisp::%function-name function))
		  (doc-string (limit-string-span (documentation function-name 'function)
						 *edit-function-menu-doc-string-width*))
		  (label (or label (format nil "Edit ~A Function Args" function-name))))
	     (multiple-value-bind (reg-args optional-args key-args)
		 (extract-funspec-args funspec current-arg-values)
	       (let* ((all-arg-values
		       (if only-keywords key-args
			 (concatenate 'list reg-args (concatenate 'list optional-args key-args))))
		      (menu-vars
		       (generate-and-set-special-vars (loop for arg-value in all-arg-values
							    collect (when (consp arg-value) (cadr arg-value))))))
		 (loop for thing in all-arg-values for i from 1 for menu-var in menu-vars
		       when (and (not only-keywords) optional-args (= i (1+ (length reg-args))))
		       collect '(:comment "&OPTIONAL arguments:") into menu-list
		       when (and key-args (not only-keywords) (= i (+ 1 (length optional-args) (length reg-args))))
		       collect '(:comment "&KEY arguments:") into menu-list
		       collect (list menu-var (format nil "~s" (if (consp thing) (car thing) thing))
				     (get-menu-entry-type-keyword (and (consp thing) (cadr thing))))
		       into menu-list
		       finally (choose-variable-values
				menu-list :label label
				:text (function-arg-menu-text extra-text doc-string only-keywords)))
		 (loop for thing in all-arg-values
		       for thing-value in (loop for menu-var in menu-vars collect (symbol-value menu-var))
		       when (find-key-word optional-args (if (consp thing) (car thing) thing))
		       collect thing-value into out 
		       else when (find-key-word key-args (if (consp thing) (car thing) thing))
		       collect (read-from-string (format nil ":~A" (if (consp thing) (car thing) thing)))
		       into out and collect thing-value into out
		       else collect thing-value into out
		       finally (return out)))))))))


(defun setup-edited-function (funspec &key label extra-text)
  (concatenate 'list
	       (list (lisp::%function-name (extract-function funspec)))
	       (function-required-args funspec)
	       (edit-function-args funspec nil :label label :extra-text extra-text :only-keywords t)))

(defun document-function-args (funspec)
  (let ((current-arg-values (when (consp funspec) (cdr funspec)))
	(function (extract-function funspec)))
    (cond ((eval:interpreted-function-p function)
	   (format t "~A is an interpreted function." function))
	  (function
	   (format t "~A args: " (lisp::%function-name function))
	   (multiple-value-bind (reg-args optional-args key-args)
	       (extract-funspec-args funspec current-arg-values)
	     (let ((symbol-values (concatenate 'list reg-args (concatenate 'list optional-args key-args))))
	       (loop for symbol-value in symbol-values
		     for arg-count from 1
		     do (format t "~s ~s" (car symbol-value) (cadr symbol-value))
		     when (< arg-count (length symbol-values))
		     do (format t ", "))))))))



	

(defun find-key-word (key-word-value-list word)
  (loop for key-word-value in key-word-value-list
	when (string= (format nil "~A" (if (consp key-word-value) (car key-word-value) key-word-value))
		      (format nil "~A" word))
	do (return (if (consp key-word-value) key-word-value (list key-word-value nil)))))


(defun key-values-from-symbol-list (symbol-list)
  (loop for thing in symbol-list collect (list (format nil "~s" thing) thing)))


(defun choose-list-values-from-keys (list-of-key-values
				     selected-values
				     &key (label "Choose Some Items")
				     (num-items-for-select-and-clear-all-buttons  4)
				     selected-keys
				     text
				     only-one-choice
				     do-all-at-once
				     (max-per-menu 10)
				     (punt-if-only-one-entry t)
				     rank-margin (direction :vertical)
				     (max-height 440)) 
  ;; '((key value)...(key value)) -> The KEYs appear in the menu, and a list of the values
  ;; corresponding to the chosen keys is returned.
  (setq list-of-key-values
	(loop for key-value in list-of-key-values
	      when key-value collect
	      (list (typecase (car key-value)
		      (string (car key-value))
		      (t		; princ-to-string
		       (format nil "~S" (car key-value))))
		    (cadr key-value))))
  ;;  (print list-of-key-values)
  (let ((chosen-keys
	 (choose-list-values (loop for key-value in list-of-key-values
				   when key-value collect (car key-value))
			     (if selected-keys
				 (loop for key-value in list-of-key-values
				       when (and key-value (member (car key-value) selected-keys :test 'string=))
				       collect (car key-value))
				 (loop for key-value in list-of-key-values
				       when (and key-value (member (cadr key-value) selected-values))
				       collect (car key-value)))
			     :label label :max-height max-height
			     :num-items-for-select-and-clear-all-buttons num-items-for-select-and-clear-all-buttons
			     :do-all-at-once do-all-at-once
			     :only-one-choice only-one-choice
			     :punt-if-only-one-entry punt-if-only-one-entry 
			     :max-per-menu max-per-menu
			     :rank-margin rank-margin :direction direction
			     :text text)))
    (loop for key-value in list-of-key-values
	  when (and key-value (member (car key-value) (if only-one-choice (list chosen-keys) chosen-keys)))
	  when only-one-choice do (return (cadr key-value))
	  else collect (cadr key-value))))


(defun choose-list-values (items &optional selected-items
				 &key
				 (num-items-for-select-and-clear-all-buttons 5)
				 only-one-choice
				 do-all-at-once
				 (punt-if-only-one-entry t)
				 (max-per-menu 10)
				 rank-margin (direction :vertical)
				 (label "Choose Some Items")(max-height 440) text)
  (if (and (not *automatic-run*) items)
      (let* ((first-time t) sub-items)
	(loop for item in items
	      for count from 1
	      for total-count from 1
	      do (push item sub-items)
	      ;; (format t "total ~a count ~A~%" total-count count)
	      when (or (= total-count (length items))
		       (and (not do-all-at-once) (= count max-per-menu)))
	      do
	      (unless (or first-time (go-ahead-menu (format nil "~a - continue?" label))) (return result))
	      (setq first-time nil)
	      and
	      nconc
	      (let ((core-result (choose-list-values-core
				  (reverse sub-items)
				  selected-items
				  :num-items-for-select-and-clear-all-buttons num-items-for-select-and-clear-all-buttons
				  :label label :max-height max-height
				  :punt-if-only-one-entry (unless (= 1 max-per-menu) punt-if-only-one-entry)
				  :only-one-choice only-one-choice
				  :rank-margin rank-margin :direction direction
				  :text text)))
		(if only-one-choice 
		    (list core-result) core-result))
	      into result
	      and do
	      (setq punt-if-only-one-entry nil)
	      (setq count 0 sub-items '())
	      when (and only-one-choice (no-nils result)) do (return (car (no-nils result)))
	  
	      finally (return (no-nils result))))
    selected-items))




;; CHOOSE-LIST-VALUES Returns a list of selected items. The SELECTED-ITEMS argument does not have to
;; include only items which are in the ITEMS list.
(defun choose-list-values-core (items selected-items &key (label "Choose Some Items")
				      title
				      num-items-for-select-and-clear-all-buttons
				      (max-height  440
						  )
				      text
				      (punt-if-only-one-entry  t)
				      only-one-choice
				      rank-margin (direction :vertical))
  (when (and (not *automatic-run*) items)
    (if (and punt-if-only-one-entry (= 1 (length items)))
	(setq selected-items (if only-one-choice (car items) items))
	(let* ((initial-values (rem-not-in-keys selected-items items))
	       (panel (create-instance nil (if only-one-choice
					       (if *motif-gui-p*
						   garnet-gadgets:motif-radio-button-panel
						   garnet-gadgets:radio-button-panel)
					       (if *motif-gui-p*
						   garnet-gadgets:motif-check-button-panel
						   garnet-gadgets:x-button-panel))
					; (when *motif-gui-p* (:button-width 18))
				       (:left 10) (:font *menu-font*) (:top 10)
				       (:rank-margin rank-margin) (:direction direction) (:items items)))
	       (height (+ (g-value panel :height) 25)) ; A little extra room
	       win top-agg value)	;Create these here because srolling windows are handled a
					;bit differently than interactor-windows.

	  (g-value panel :value)	;Access :value slot to initialize slot formula and
					;establish dependencies (Garnet Manual p.300).
	  
	  (setq win (if (> (g-value panel :height) max-height)
			(let ((swin (create-instance nil (if *motif-gui-p* *motif-ss-win-w-bars* *ss-win-w-bars*)
						     (:height (+ 30 max-height)) (:title label) (:total-height height)
						     (:icon-title "SH Menu")
						     (:total-width (o-formula (gv :self :width))))))
			  (opal:update swin t)
			  (setq top-agg (g-value swin :inner-aggregate))
			  (create-instance NIL destroy-window-Interactor (:Window (g-value swin :outer-window)))
			  ; (s-value swin :visible t)
			  ; (opal:update swin t)
			  ; (break)
			  swin)
			(let ((win (get-regular-menu-window height 100 label title)))
			  (setq top-agg (g-value win :aggregate))
			  win)))
	  (s-value win :width (+ (g-value panel :width)
				 *menu-width-fudge*
				 (g-value (if *motif-gui-p* *motif-ok-button* *ok-button*) :width)))
	  (when text (add-menu-text text top-agg win))
	  (if (and (not only-one-choice)
		   (or (not num-items-for-select-and-clear-all-buttons)
		       (>= (length items) num-items-for-select-and-clear-all-buttons)))
	      (add-ok-select-and-clear-all-buttons win top-agg panel)
	      (add-ok-button win top-agg))
	  
	  (s-value panel :value (if only-one-choice (car initial-values) initial-values))
	  (opal:add-component top-agg panel)
	  (s-value win :width (max (g-value win :width)
				   (+ (* *window-manager-char-width (length (g-value win :title)))
				      ;; 2 mouseable boxes in window title bar
				      *window-manager-mouse-box-width *window-manager-mouse-box-width)))
	  (s-value win :top *menu-top*)
	  (s-value win :left *menu-left*)
	  (s-value win :visible t)   (opal:update win t)
; 	  (resurrect-opal-win win :raise t :visible t :show t :update t :deiconify t)
	  (inter:wait-interaction-complete)
	  ;; kludge since the menus tend to walk over to the left
	  (setq *menu-top* (max 10 (g-value win :top))
		*menu-left* (max 10 (g-value win :left)))
	  (remove-and-free-buttons top-agg)
	  (setq value (g-value panel :value))
	  (opal:destroy win)
	  (setq selected-items value)))
    selected-items))



;;; CHOOSE-HASH-TABLE-ENTRIES Returns a list of selected hash table keys. This is based on the more
;;; sophisticated SELECT-HASH-VALUES-MENU - CHOOSE-HASH-TABLE-ENTRIES is used, for example, at some
;;; intermediate stage in a selection sequence, and it is possible that 
(defun choose-hash-table-entries (table &optional selected-items
					&key (label (format nil "Choose hash table entries"))
					(punt-if-only-one-entry t))
  (let ((list-of-table-names (list-all-hash-names table)))
    (if (and punt-if-only-one-entry
	     (= (length list-of-table-names) 1))
	list-of-table-names
	(select-hash-values-menu table label :selected-list selected-items
				 :punt-if-only-one-entry punt-if-only-one-entry))))

;;; LIST-ALL-HASH-NAMES Returns a list of all the table keys in the HASH-TABLE-OR-TABLES argument.
;;; If SELECTION-KEY is non-NIL, then only those table entries with a non-NIL value for the
;;; :SELECTION-KEY slot will be added to the list.
(defun list-all-hash-names (hash-table-or-tables &optional selection-key)
  (loop for hash-table in (if (typep hash-table-or-tables 'cons) hash-table-or-tables (list hash-table-or-tables))
	nconcing
	(loop for key being the hash-key in hash-table
	      when (or (not selection-key)
		       (typecase selection-key
			 (function (funcall selection-key (gethash key hash-table)))
			 (t (eval (list selection-key (gethash key hash-table))))))
	      collecting key)))

;;; SELECT-HASH-VALUES-MENU Generates a selection menu and returns a list of names (hash table keys)
;;; that are selected from the HASH-TABLES. The selection menu includes the names of all the elements in
;;; the HASH-TABLES, with pre-selected names taken from both any names in SELECTED-LIST and the names of
;;; those HASH-TABLE elements for which the applied function SELECTION-KEY is non-nil. If the tables
;;; are big enough, then the user is prompted at various intervals whether or not to continue
;;; checking off the items in the tables. If the menu(s) are aborted at any time (via the user not
;;; authorizing additional menus), then the selected keys of the queried entries, as well as the
;;; untested keys of the original SELECTED-LIST argument is returned. The INCLUSION-KEY is applied
;;; to the table entries to determine whether or not they are included in the menu at all.
(defun select-hash-values-menu (hash-tables menu-label &key selection-key ; #'null
					    selected-list
					    (rank-margin 5)
					    (inclusion-key #'atom) punt-if-only-one-entry
					    continue-prompt
					    do-all-at-once
					    (direction :horizontal)
					    (max-per-menu 10))
  (let* ((hash-tables (coerce-to-list hash-tables))
	 (table-key-list (loop for table in hash-tables
			       nconcing (list-all-hash-names table inclusion-key)))
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
		    do (push key key-list)
		    (push (list (SIMPLE-NICE-STRING-FROM-ATOM key) key) menu-key-value-list)

		    when (and (not do-all-at-once) (= count max-per-menu))
		    do
		    (setq complete-selected-list
			  (delete-duplicates
			   (concatenate 'list
					(rem-in-keys complete-selected-list key-list)
					(choose-list-values-from-keys menu-key-value-list complete-selected-list
								      :label menu-label
								      :do-all-at-once t
								      :rank-margin rank-margin :direction direction
								      :punt-if-only-one-entry punt-if-only-one-entry
								      ))))
		    (setq count 0 key-list '() menu-key-value-list '())
		    and when (or (= total-count (length table-key-list)) (not (go-ahead-menu)))
		    do (return complete-selected-list)
		    finally (return
			      (delete-duplicates
			       (concatenate 'list
					    (rem-in-keys complete-selected-list key-list)
					    (choose-list-values-from-keys menu-key-value-list complete-selected-list
									  :label menu-label
									  :punt-if-only-one-entry punt-if-only-one-entry
									  :do-all-at-once t
									  :rank-margin rank-margin :direction direction))))))
	  complete-selected-list))))

		  

;; ASK-FOR-ELEMENT-MODIFICATIONS Returns T or NIL - Useful when there are a lot of elements.
(defun ask-for-element-modifications (label element-count element-name &optional default (max-count 10))
  (if (>= element-count max-count)
      (go-ahead-menu label (format nil "Go ahead to modify ~a property (~A ~as)"
				   element-name element-count element-name)
		     default)
      t))

;;; Returns true or nil.
(defun go-ahead-menu (&optional (question "Go ahead with what we are doing?")
				(label "Authorization") (default t) enable-abort-option)
  (let ((dummy1 default) dummy2)
    (choose-variable-values
     `((dummy1 ,question :boolean)
       ,(when enable-abort-option '(dummy2 "Abort this thread to top level" :boolean)))
     :label (if (stringp label) label "")
     )
    (when dummy2 (sim-error (format nil "Aborting from the go-ahead menu invoked by ~A" (kernel:find-caller-name))))
    dummy1))



;;; REM-NOT-IN-KEYS Returns a version of LIST that includes only items in LIST that appear in KEYS.
(defun rem-not-in-keys (list keys)
  (loop for item in list when (member item keys :test #'equal) collect item))

;;; REM-IN-KEYS Returns a version of LIST that includes only items in LIST that do not appear in KEYS.
(defun rem-in-keys (list keys)
  (loop for item in list when (not (member item keys :test #'equal)) collect item))





(defun replace-spaces-w-underscores (string)
  (let ((out ""))
    (loop for char in 
	  (loop for i from 0 to (1- (length string))
		when (eq (schar string i) #\space)
		collect "_"
		else
		collect (string (schar string i)))
	  do
	  (setq out  (concatenate 'string out char)))
    out))






(defun font-menu (&optional default-font label)
  (let ((dummy1 (when default-font (g-value default-font :family)))
	(dummy2 (when default-font (g-value default-font :face)))
	(dummy3 (when default-font (g-value default-font :size))))
    (choose-variable-values
     '((dummy1 "Font family" :choose (:fixed :serif :sans-serif))
       (dummy2 "Font face" :choose (:roman :italic :bold :bold-italic) :rank-margin 4)
       (dummy3 "Font size" :choose (:small :medium :large :very-large) :rank-margin 4))
     :label (if label (concatenate 'string "Font for " label) "Choose font characteristics"))
    (opal::Get-Standard-Font dummy1 dummy2 dummy3)))

(defun get-integer (default maximum minimum integer-label menu-label)
  (let ((dummy1 default)
	(first-time t))
    (loop until (and (not first-time)
		     (<= minimum dummy1 maximum))
	  do
	  (choose-variable-values
	   `((dummy1 ,integer-label :integer))
	   :label menu-label
	   :text (format nil "Enter integer between ~d and ~d" minimum maximum))
	  (setq first-time nil))
    dummy1))

(defun choose-dash-patterns (&optional (default 0) (title "Line style Dash pattern"))
  (let ((dummy1 (princ-to-string default))
	(patterns (mapcar 'princ-to-string *line-styles-dash-patterns*)))
    (choose-variable-values
     `((dummy1 "Choose a dash pattern (0 is solid):" :choose ,patterns))
     :label title)
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
(export '(*menu-win*
	  *ss-win-w-bars*
	  choose-variable-values CHOOSE-LIST-VALUES CHOOSE-HASH-TABLE-ENTRIES
	  T-OR-NIL-P
	  CHOOSE-LIST-CONTENTS
	  *MOTIF-GUI-P*
	  EDIT-STRING-LIST
	  EDIT-LIST
	  EDIT-FUNCTION-ARGS
	  FIND-KEY-WORD
	  choose-dash-patterns
	  key-values-from-symbol-list
	  CHOOSE-LIST-VALUES-FROM-KEYS
	  add-ok-button  LIST-ALL-HASH-NAMES	  REM-NOT-IN-KEYS  REM-IN-KEYS
	  SELECT-HASH-VALUES-MENU FONT-MENU
	  go-ahead-menu  *menu-sleep-time*
	  COLOR-CHOOSE-BUTTON-MENU

	  new-color-choose-button-menu
	  get-integer
	  REORDER-VALUE-LIST-FROM-KEYS-MENU
	  xclusive-choose-button
	  REORDER-LIST-MENU
	  CHOOSE-LIST-VALUES-CORE
	  ASK-FOR-ELEMENT-MODIFICATIONS
	  SIMPLE-TEXT-MENU
	  *enable-precision-float-menus*
	  setup-edited-function
	  EXTRACT-FUNCTION
	  EXTRACT-FUNCTION-FROM-ATOM
	  EXTRACT-ALL-ARGS
	  DOCUMENT-FUNCTION-ARGS
	  GENERATE-AND-SET-SPECIAL-VARS
	  extract-funspec-args
	  function-required-args
	  ))

;; From the latest menu-hack.lisp

(defun setfable-p (symbol)
  (if (macro-function symbol)
    (setfable-p (car (macroexpand (cons symbol (MACRO-REQUIRED-ARGS symbol)))))
    (fboundp `(setf ,symbol))))

(export '(setfable-p))
