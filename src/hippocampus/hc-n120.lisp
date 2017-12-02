


(defun hc-n120 (&optional (apical-extent-of-na-k 150))
  (load-surf-user-file "~/surf-hippo/data/HC-PYR/9_26_1996/6646344-densities.elts")
  (revamp-type-parameters)
  (setq *plot-channels-by-major-ion* t)
  (n120-max-red "HC-120")
  (process-circuit-structure)
  (add-isource *soma*)
  (let* ((apical-segments (segments-out (element "17"))) ; By eye.
	 (na-segs (loop for seg in apical-segments
			when (< (distance-to-soma seg) apical-extent-of-na-k)
			collect seg))
	 (na-channels '(NA-4STATE-exp-GEN))
	 (k-channels '(KDR-GEN
		       KM-GEN
		       kd-gen
		       KA-gen))
	 (ca-dep-k-channels '(kc-markov
			      kahp-gen))
	 (ca-channels '(ca-l-gen ca-n-gen ca-t-gen)))
	   
    (create-element (soma-segments) na-channels)
    (create-element na-segs na-channels)

    (create-element (soma-segments) k-channels)
    (create-element na-segs k-channels)
    (create-element (soma-segments) ca-dep-k-channels)
    (create-element na-segs ca-dep-k-channels)
    (create-element (soma-segments) ca-channels)
    (create-element na-segs ca-channels)
    (LOAD-SURF-USER-FILE "~/surf-hippo/data/HC-N120/9_27_1996/6654702.plot-settings.lisp")
    nil))

#|
(let* (*monitor-circuit-modifications*
       (apical-segments (segments-out (element "17"))) ; By eye.
	 (na-channels '(NA-4STATE-exp-GEN))
	 (k-channels '(KDR-GEN
		       KM-GEN
		       kd-gen
		       KA-gen))
	 (ca-dep-k-channels '(kc-markov
			      kahp-gen))
	 (ca-channels '(ca-l-gen ca-n-gen ca-t-gen)))
  (create-element (segments) na-channels)
  ;  (create-element (segments) k-channels)
   ; (create-element (segments) ca-dep-k-channels)
    ;(create-element (segments) ca-channels)
    nil)


(create-element (ELEMENT-CLOUD "165" 100) 'mig95-syn)

(element-type-param 'mig95-syn 'gbar-ref
			  ;; 5nS total, spread out over all the syns.
			  (/ 5.0e-3 (length (synapses))) t)

;; regular synapse firing, each w/2ms period from 10 to 520 ms
(let ((events (loop for time from 10.0 to 520 by 2 collect time)))
  (add-events (SYNAPSE) EVENTS))

(erase-elements (synapses))

(create-element (ELEMENT-CLOUD "10" 100) 'mig95-syn)


;; poisson synapse firing, from 10 to 520 ms, total similar to that of reg firing above.
(let ((num-events (loop for time from 10.0 to 520 by 2 sum 1)))
  (add-events (synapses)
	      (poisson-events (/ 510.0 num-events) 10.0 520.0)))


|#