;; This slot is set NIL by OPAL::INITIALIZE-X11-VALUES, by a NIL value of OPAL::*IS-THIS-A-COLOR-SCREEN?*. Setting this variable
;; to T seems to generate ALLOC-ERROR, but so far just setting :color-p to t is ok.

(with-constants-disabled (s-value opal::COLOR :color-p t))

