;; These parameters are taken from MIT-AI TR 1161, with some modifications, but in general this can be a backbone for a standard
;; soma/short-cable geometry cell model. 

;; Note that the HIPPO thesis value for R-M-soma is 1/3 too small. 2550 ohms-cm2 is the correct value for matching R-in of 39Mohms.

(cell-type-def
 '(tr-1161-CA1
   (rm . 40000)				; ohms-cm2
   (ri  . 200)				; ohms-cm
   (rm-soma . 2550)			; ohms-cm2
   (v-leak . -70)			; mV
   (cm . 1))				; uF/cm2
 )

(defun TR-1161-hippo ()
  (hippo :name "TR-1161" :cell-type 'TR-1161-CA1
	 :soma-diameter 35 :apical-dendrite-diameter 12 :apical-dendrite-length 1200 ; microns
	 :apical-total-segs 5)
  (create-element *soma* '(na1-TR1161 na2-TR1161 na3-TR1161 ca-TR1161 a-TR1161 dr-TR1161 c-TR1161 ahp-TR1161 m-TR1161)) 
  *cell*)
