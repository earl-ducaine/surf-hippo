

;; Finally found out why the Thinkpad colors were screwed up?


(in-package "OPAL")


(defun initialize-x11-values (full-display-name root-window)
  (setq *default-x-display-name*
	(if full-display-name
	  (get-display-name full-display-name)
	  #-(or allegro clisp) (machine-instance)
	  #+clisp ""
	  #+allegro (short-site-name)))
  (setq *default-x-screen-number* (get-screen-number full-display-name))

  ;; Set up all the Opal variables used to identify display, screen, etc.
  ;; Unfortunately, these are needed by discard-all-pending-events (in
  ;; process.lisp), which is called by launch-main-event-loop-process.
  (gem:set-device-variables root-window)

  ;; This is really dumb, but it's the only way I can think of
  ;; to find out if the screen is color or not.
  (let* ((*print-pretty* NIL)
	 (colormap-string (string-upcase
			   (princ-to-string opal::*default-x-colormap*))))
    (if (or (search "PSEUDO-COLOR" colormap-string)
            (search "DIRECT-COLOR" colormap-string)
	    (search "GRAY-SCALE" colormap-string)

	    ;; LG Change March 8, 2002
	    ;; This is what the Thinkpad returns.

	    ;; However, making *is-this-a-color-screen?* T give ALLOC-ERROR!
	    ;; (search "TRUE-COLOR" colormap-string)
	    
	    )
      (setq *is-this-a-color-screen?* t)
      (setq *is-this-a-color-screen?* nil)))
  (with-constants-disabled

   ;; LG Change March 8, 2002
   ;; To make background colors work.
    (s-value opal::COLOR :color-p t
	     ; *is-this-a-color-screen?*
	     ))
  (setq *HP-display-type?* (and *is-this-a-color-screen?* (zerop *black*)))
  )
