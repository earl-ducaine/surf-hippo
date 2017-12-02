

;; Test color assignment for the :foreground-color of a garnet-gadgets:motif-text-button, and the
;; :background-color for a inter:interactor-window. Both seemed to be screwed up 
;; when running Garnet 3.0 on a Thinkpad 600 (CMUCL 18a+ release x86-linux 2.4.5 29 June 1998)
;; LBG Oct. 25 1999

(create-instance 'test-window inter:interactor-window
		 (:aggregate (create-instance nil opal:aggregate)))

(opal:add-component (g-value test-window :aggregate)
		    (create-instance 'motif-text-button garnet-gadgets:motif-text-button))

(loop for button-color in (list opal::black OPAL:MOTIF-GRAY opal::blue opal::red opal:white) do
      (s-value motif-text-button :foreground-color button-color)
      (s-value test-window :visible t)(opal:update test-window t)
      (y-or-n-p (format nil "Test button should be ~A... (y to continue) " button-color)))

(loop for window-color in (list opal::black OPAL:MOTIF-GRAY opal::blue opal::red opal:white) do
      (s-value test-window :background-color window-color)
      (s-value test-window :visible t)(opal:update test-window t)
      (y-or-n-p (format nil "Window should be ~A... (y to continue) " window-color)))
