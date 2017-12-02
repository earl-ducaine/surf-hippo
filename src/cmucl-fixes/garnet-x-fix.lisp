;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: GEM; Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;         The Garnet User Interface Development Environment.      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This code was written as part of the Garnet project at          ;;;
;;; Carnegie Mellon University, and has been placed in the public   ;;;
;;; domain.  If you are using this code or any part of Garnet,      ;;;
;;; please contact garnet@cs.cmu.edu to be put on the mailing list. ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; CHANGE LOG:
;;; 01/30/95 Andrew Mickish - New destroy-notify-window for CMUCL
;;; 12/02/94 Andrew Mickish - Removed stippled-p parameter from draw-image
;;; 05/29/94 Andrew Mickish - Added ability to specify a list for font :face
;;; 05/25/94 Andrew Mickish - Added optional drawable parameter to X-Draw-Line
;;; 04/15/94 Andrew Mickish - Fixed tiny width and height for X-Draw-Arc
;;; 04/13/94 Andrew Mickish - Fixed :QUERY-COLORS branch of X-Colormap-Property
;;; 03/25/94 Andrew Mickish - Fixed title clause in x-set-window-property
;;; 03/21/94 Andrew Mickish - Fixed corner fill of roundtangles
;;; 01/21/94 Andrew Mickish - New Gem method color-to-index
;;; 01/12/94 Andrew Mickish - New window-from-drawable
;;; 01/07/94 Andrew Mickish - Fixed functions that destroy window from X menu
;;; 01/03/94 Andrew Mickish - Moved PI variables to utils/general.lisp
;;; 12/17/93 Andrew Mickish - New X-Device-Image
;;; 12/15/93 Andrew Mickish - Moved macros to macros.lisp
;;; 11/23/93 Andrew Mickish - Fixed line and fill placement in x-draw-arc,
;;;            line placement in x-draw-rectangle, and fill placement in
;;;            x-draw-roundtangle
;;; 11/18/93 Andrew Mickish - Removed "list" from call to xlib:free-colors
;;; 11/11/93 Andrew Mickish - Put into CLTL2 form

(in-package "GEM")

#|
LG 21.08.2016

CMUCL 20D Patch to be verified to avoid window bug:

    Type-error in KERNEL::OBJECT-NOT-TYPE-ERROR-HANDLER:
    NIL is not of type XLIB:WINDOW
    [Condition of type TYPE-ERROR]

    Restarts:
    0: [ABORT] Return to sldb level 1.
    1: [RETRY] Retry SLIME REPL evaluation request.
    2: [*ABORT] Return to SLIME's top level.
    3: [ABORT] Return to Top-Level.

    Backtrace:
    0: (XLIB:QUERY-TREE 1 NIL)[:EXTERNAL]
    1: (GEM::LINEAGE-OF-DRAWABLE NIL)
    2: (GEM::LINEAGE-OF-DRAWABLE #<XLIB:WINDOW :0 16D>)
    3: (GEM::LINEAGE-OF-DRAWABLE #<XLIB:WINDOW :0 126DD1A>)
    4: (GEM::LINEAGE-OF-DRAWABLE #<XLIB:WINDOW :0 126DD1B>)
    5: (GEM::LINEAGE-OF-DRAWABLE #<XLIB:WINDOW :0 3C00077>)
    6: (GEM::LINEAGE-OF-DRAWABLE 1 #<XLIB:WINDOW :0 3C00077>)[:EXTERNAL]
    7: (GEM::X-EVENT-HANDLER #k<GEM::*ROOT-WINDOW*> NIL)
    8: (LISP::SUB-SERVE-EVENT NIL 0)
    9: ((FLET SWANK/BACKEND:WAIT-FOR-INPUT) (#<Stream for descriptor 6>) NIL)

|#

;;; Returns list of drawable, parent, grandparent, ... , root.
;;;
(defun lineage-of-drawable (drawable)
  ;; Certain versions of Allegro CL/CLX give an error when you
  ;; call xlib:query-tree.  These versions seem to be
  ;; characterised by the feature :clx-cl-error.
#|
  #-clx-cl-error
  (multiple-value-bind (children parent root)
      (xlib:query-tree drawable)
    (declare (ignore children))
    (if (eq parent root)
      (list drawable root)
      (cons drawable (lineage-of-drawable parent))))
|#
 ; #+clx-cl-error
  (list drawable opal::*default-x-root*))
