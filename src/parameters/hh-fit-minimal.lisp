;; TITLE HH sodium channel
;; Hodgkin - Huxley squid sodium channel

(channel-type-def
 '(na-hh-fit
   (gbar-density . 1200)
   (e-rev . 50)
   (v-particles . ((m-hh-fit 3) (H-hh-fit 1)))))

(particle-type-def
 '(m-hh-fit
   (class . :hh-ext)
   (VALENCE . 2.7)
   (GAMMA . 0.4)
   (BASE-RATE . 1.2)
   (V-HALF . -40)
   (TAU-0 . 0.07)
   (QTEN . 3)
   (reference-temp . 6.3)))

(particle-type-def
 '(h-hh-fit
   (class . :hh-ext)
   (VALENCE . -3.7)
   (GAMMA . 0.4)
   (BASE-RATE . 0.07)
   (V-HALF . -62)
   (TAU-0 . 0.9)
   (QTEN . 3)
   (reference-temp . 6.3)))

