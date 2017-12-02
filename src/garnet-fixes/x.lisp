(in-package "GEM")

;;; Input event handling
;;;

;; For T23 (Thinkpad), running RedHat 7.3, double button presses seem to be broken, so disable them.

(setq inter:*double-click-time* nil)

;;; Fix bugging out when mouse wheel is moved over Opal window
;;; 29-OCT-2001 Edi Weitz   - Ignore mouse buttons > 3 in X-EVENT-HANDLER


;; LG Change 20.02.2017
(defvar *announce-x-event-handler-illegal-event* nil)
(defvar *debug-x-event-handler* nil)
(export '(*debug-x-event-handler* *announce-x-event-handler-illegal-event*))

(defun x-event-handler (root-window ignore-keys)
  (let ((display (the-display root-window)))
    (xlib:event-case
	(display :discard-p t :timeout (if ignore-keys 0 NIL))
      ;; this first one is for when a window is deleted by the wm
      (:CLIENT-MESSAGE
       (event-window type data format)
       (event-handler-debug :CLIENT-MESSAGE event-window type data format)
       (do-client-message event-window type data format display))
      (:MAP-NOTIFY
       (event-window)
       (event-handler-debug :MAP-NOTIFY)
       (interactors::do-map-notify (x-window-from-drawable root-window
							   event-window)))
      (:UNMAP-NOTIFY
       (event-window)
       (event-handler-debug :UNMAP-NOTIFY)
       (interactors::do-unmap-notify (x-window-from-drawable root-window
							     event-window)))
      (:REPARENT-NOTIFY
       (event-window)
       (event-handler-debug :REPARENT-NOTIFY)
       (if (connected-window-p event-window)
	   (let ((window (x-window-from-drawable root-window event-window)))
	     (s-value window :already-initialized-border-widths nil)
	     (s-value window :lineage (lineage-of-drawable event-window)))))
      (:CIRCULATE-NOTIFY
       ()
       (event-handler-debug :CIRCULATE-NOTIFY)
       (interactors::do-circulate-notify))
      (:GRAVITY-NOTIFY
       ()
       (event-handler-debug :GRAVITY-NOTIFY)
       (interactors::do-gravity-notify))
      (:DESTROY-NOTIFY
       (event-window)
       (event-handler-debug :DESTROY-NOTIFY)
       (destroy-notify-window event-window))
      (:CONFIGURE-NOTIFY
       (event-window x y width height above-sibling)
       (event-handler-debug :CONFIGURE-NOTIFY)
       (if (connected-window-p event-window)
	   (interactors::do-configure-notify (x-window-from-drawable root-window
								     event-window)
	     x y width height above-sibling)))
      (:EXPOSURE
       (event-window x y width height count)
       (event-handler-debug :EXPOSURE x y width height count)
       (if (connected-window-p event-window)
	   (interactors::do-exposure (x-window-from-drawable root-window
							     event-window)
	     x y width height count display)))
      (:KEY-PRESS
       (event-window x y state code time)
       (event-handler-debug :KEY-PRESS event-window x y state code time)
       (if ignore-keys
	   ;; We don't want keys, but check if this is the abort key
	   (let ((c (x-translate-character *root-window* 0 0 state code 0)))
	     (when (eq c interactors::*garnet-break-key*)
	       (format T "~%**Aborting transcript due to user command**~%")
	       (return-from x-event-handler :abort)))
	   ;; Normal case: we do want keys
	   (interactors::do-key-press
	       (x-window-from-drawable root-window
				       event-window) x y state code time)))
      (:BUTTON-PRESS
       (event-window x y state code time event-key)
       ;;      (event-handler-debug :BUTTON-PRESS event-window x y state code time
       ;;			   event-key)
       ;;      (unless ignore-keys
       ;;	(interactors::do-button-press (x-window-from-drawable root-window
       ;;							      event-window)
       ;;	  x y state code time event-key
       ;;           (format t "code ~D !!!!!!!!!!!!!!~%" code)
       (when (< code 4)
	 (event-handler-debug :BUTTON-PRESS event-window x y state code time
			      event-key)
	 (unless ignore-keys
	   (interactors::do-button-press (x-window-from-drawable root-window
								 event-window)
	     x y state code time event-key))))
      
      (:BUTTON-RELEASE
       (event-window x y state code time event-key)
       ;;      (event-handler-debug :BUTTON-RELEASE event-window x y state code time
       ;;			   event-key)
       ;;      (unless ignore-keys
       ;;	(interactors::do-button-release (x-window-from-drawable root-window
       ;;								event-window)
       ;;	  x y state code time event-key
       (when (< code 4)
	 (event-handler-debug :BUTTON-RELEASE event-window x y state code time
			      event-key)
	 (unless ignore-keys
	   (interactors::do-button-release (x-window-from-drawable root-window
								   event-window)
	     x y state code time event-key))))

      (:MOTION-NOTIFY
       (event-window x y)
       (event-handler-debug :MOTION-NOTIFY event-window x y)
       (unless ignore-keys
	 (interactors::do-motion-notify (x-window-from-drawable root-window
								event-window)
	   x y display)))
      (:ENTER-NOTIFY
       (event-window x y time)
       (event-handler-debug :ENTER-NOTIFY event-window x y time)
       (unless ignore-keys
	 (interactors::do-enter-notify (x-window-from-drawable root-window
							       event-window)
	   x y time)))
      (:LEAVE-NOTIFY
       (event-window x y time)
       (event-handler-debug :LEAVE-NOTIFY event-window x y time)
       (unless ignore-keys
	 (interactors::do-leave-notify (x-window-from-drawable root-window
							       event-window)
	   x y time)))
      (:NO-EXPOSURE
       ()
       (event-handler-debug :NO-EXPOSURE)
       (unless ignore-keys
	 t))
      (OTHERWISE
       ()
       (when *debug-x-event-handler* (break)) ; LG Changes 20.02.2017
       (when *announce-x-event-handler-illegal-event* (format t "illegal event") t)))))



