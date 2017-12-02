;; TITLE HH sodium channel
;; Hodgkin - Huxley squid sodium channel

(channel-type-def
 '(NA-HH
   (gbar-density . 1200)	
   (e-rev . 50)			
   (v-particles . ((M-HH 3) (H-HH 1)))))

(particle-type-def
 `(M-HH
   (class . :HH)
   (reference-temp . 6.3)
   (qten . 3)
   (alpha . (lambda (voltage)
	      (/ (* -0.1 (- voltage -40))
		 (1- (exp (/ (- voltage -40) -10))))))
   (beta . (lambda (voltage) (* 4 (exp (/ (- voltage -65) -18)))))))

(particle-type-def
 `(H-HH
   (class . :HH)
   (reference-temp . 6.3)
   (qten . 3)
   (alpha . (lambda (voltage) (* 0.07 (exp (/ (- voltage -65) -20)))))
   (beta . (lambda (voltage) (/ 1 (1+ (exp (/ (- voltage -35) -10))))))))