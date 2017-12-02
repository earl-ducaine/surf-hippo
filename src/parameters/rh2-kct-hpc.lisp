;;; -*- Package: SURF; Mode: LISP -*-

(channel-type-def
 '(RH2-KCT-HPC
   (gbar . 0.4)
   (e-rev . -80.0)
   (use-defined-e-rev . T)
   (ion-permeabilities . ((K 0.9) (NA 0.1)))
   (q10 . 1.0)
   (reference-temp . 27.0)
   (v-particles . ((RH2-KCTX-HPC 1)))))


(defvar *RH2-KCTX-HPC-ca-activation-forward-exponential-term*)

(setq *RH2-KCTX-HPC-ca-activation-forward-exponential-term*
      (v-function-array '(squeezed-exponential-rate
			  voltage
			  ; :tau-min 0.001
			  :k 7.0
			  :v-half -35.0
			  ; :tau-max 1.0
			  ))
      g t)

(defun RH2-KCTX-HPC-ca-activation-forward (prt)
  (declare (optimize (safety 0) (speed 3) (space 0) (compilation-speed 0)))
  (return-markov-rate
   (* (aref (the vec-df *RH2-KCTX-HPC-ca-activation-forward-exponential-term*) (particle-v-index prt))
      (nthorder-conc-particle-forward-rate (particle-concentration-particle prt)))))

(particle-type-def
 `(RH2-KCTX-HPC
   (class . :MARKOV)
   (STATES . (C O I))
   (OPEN-STATES . (O))
   (STATE-TRANSITIONS . 
		      ((C O RH2-KCTX-HPC-CA-ACTIVATION-FORWARD T)
		       (I C (SQUEEZED-EXPONENTIAL-RATE VOLTAGE :V-HALF -120.0 :K -10.0 :TAU-MIN 10.0))
		       (O C (SQUEEZED-EXPONENTIAL-RATE VOLTAGE :TAU-MAX -1.0 :V-HALF -60.0 :K -5.0 :TAU-MIN 0.01))
		       (O I (SQUEEZED-EXPONENTIAL-RATE VOLTAGE :TAU-MAX -1.0 :V-HALF -20.0 :K 10.0 :TAU-MIN 0.1))
		       ))
   (reference-temp . 27.0)
   (q10 . 1.0)
   (concentration-particle-type . RH2-KCTX-HPC-CA)))

(conc-particle-type-def
 '(RH2-KCTX-HPC-CA
        (class . :NTH-ORDER)
        (alpha . 1.0e+7)
        (beta . 0.05)
        (tau-0 . 100.0d0)
        (power . 3)
        (q10 . 1.0)
        (reference-temp . 27.0)
        (shell . 1)
        (conc-int-type . CA-IN-HPC)
                   ))


