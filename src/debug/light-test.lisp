(topload "/usr/local/surf-hippo/circuits/demos/light-hippo.lisp")

(progn
  (caows)
  (topload "/usr/local/surf-hippo/circuits/demos/light-hippo.lisp")
  (setq *user-stop-time* 400		; Total simulation time (ms).
	*enable-colorize-time* t        ; Enable time display in colorized histology windows.
	*enable-colorize-scale* t       ; Enable color scale display in colorized histology windows.
	*colorize-simulation* t		; Enable colorization of simulation in some or all histology windows.
	*enable-sparse-data* t		; Enable data storage from all the circuit elements.
	*sparse-data-step* 0.1		; Time step (ms) target for sparse data storage. Simulation times for sparse data are collected in *SPARSE-DATA-TIMES*.
	)
  ;; (create-element 'vsource *soma*) (add-working-hpc-channels *soma* t)  
					;  (test-chr2-opsin nil)
 ;; *light-stimulus-types*
  (setq *light-stimulus-start-time* 100	*light-stimulus-stop-time* 200
	*light-stimulus-strength* 1.0 *light-background* 0.0
	*light-stimulus* :spot ;; From *LIGHT-STIMULUS-TYPES*
	*light-stimulus-plane* :XZ   ;; :XY for retina, :XZ for radial mount cortical cells.
	*spot-outside-diameter* 100
	*enable-light* t
	*light-speed* 4.0 ;; Microns per millisecond
	*light-theta* 0 ;; Radians
	*light-direction* T ;; T / nil => movement is in the direction of / opposite to *light-theta*
	*light-start-position-x* 00.0  *light-start-position-y* 00.0 ;; Point of center of stimulus at *motion-start-time* in microns
	)
  (setq *stimulus-graphic-color* (get-color-from-library .25 .25 .25) *stimulus-graphic-shading-percent* 100)
  (just-draw :scale .5 :mark-elements 'chr2-opsin :draw-light-stimulus t)
  (setq *I2-magnitude* 264.6)
  ;; After running the simulation (GOFERIT), show the middle of the first spike in the colorized histology with (SHOW-SPARSE-DATA :TARGET-TIME 6.5).			
  )


;; Moving spot example
(setq *light-stimulus-start-time* 100	*light-stimulus-stop-time* 200
      *light-stimulus-strength* 1.0 *light-background* 0.0
      *light-stimulus* :moving-spot ;; From *LIGHT-STIMULUS-TYPES*
      *light-stimulus-plane* :XZ   ;; :XY for retina, :XZ for radial mount cortical cells.
      *spot-outside-diameter* 100
      *enable-light* t
      *light-speed* 4.0 ;; Microns per millisecond
      *light-theta* (/ pi 3) ;; Radians
      *light-direction* T ;; T / nil => movement is in the direction of / opposite to *light-theta*
      *light-start-position-x* 00.0  *light-start-position-y* -200.0 ;; Point of center of stimulus at *motion-start-time* in microns
      )

(goferit)
(caows)
(just-draw :scale 1 :draw-light-stimulus nil :background-color 'white)
(replay-colorized-simulation :start-time 110 :stop-time 150 :time-step .10)
(SHOW-SPARSE-DATA :TARGET-TIME 240)
;; draw-light-stimulus
(opal:update *twin* t)
