;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: SURF; Base: 10 -*-

#|

====================================================================================================
			       The Surf-Hippo Neuron Simulator System
====================================================================================================

This code was written as part of the Surf-Hippo Project, originally at the Center for Biological
Information Processing, Department of Brain and Cognitive Sciences, Massachusetts Institute of
Technology, and currently at the Neurophysiology of Visual Computation Laboratory, CNRS.
                                                                                 
Permission to use, copy, modify, and distribute this software and its documentation for any purpose
and without fee is hereby granted, provided that this software is cited in derived published work,
and the copyright notice appears in all copies and in supporting documentation. The Surf-Hippo
Project makes no representations about the suitability of this software for any purpose. It is
provided "as is" without express or implied warranty.
                                                                                 
If you are using this code or any part of Surf-Hippo, please contact surf-hippo@ai.mit.edu to be put
on the mailing list.
                                                                                 
Copyright (c) 1989 - 2003, Lyle J. Graham                                                                                              

|#


(in-package "SURF-HIPPO")

;;; Cholinergic rabbit retina starburst AMACRINE cell.



;;; This file contains the coordinates of the nodes for Cell 7 (about 1.25 mm from streak) of Tauchi and Masland (1984).  The
;;; metric unit for the table's coordinate entries corresponds to about 6.06 microns. To a good approximation, the cell body is
;;; an ellipsoid with vertical major axis (of about 18 micrometers) and horizontal minor axis (of about 12 micrometers).

;;; Famiglietti (1983) notes that there is little overlap in dendritic fields.  Tapering of primary branches until approx 2nd
;;; order branch on average 50 uM from soma.  Proximal region gradually tapering dendrites of no more than 1.5 uM at origin.  At
;;; the 3rd or 4th order of branching dendrites become thin rather abruptly (transition between proximal and intermediate zones).
;;; 3 annular zones (proximal,intermediate and distal). Intemediate and distal zones are approx .2 to .4 uM diameter.  Distal zone
;;; marked by varicosities and boutons.

;;; Miller and Bloomfield (1983) claim that distal regions (with varicosities) are thicker than the intermediate zone, and suggest
;;; that this difference is due to different staining technique than Famiglietti (who uses Golgi) (Miller and Bloomfield use HRP)
;;; (Masland uses DAPI and LY). Soma on the order of 11uM diameter. Thin dendrites (intermediate zone ??) are estimated at much
;;; less than 0.5 uM, perhaps 0.1 uM.  !!!!!! They claim that 1st order branches are typically about 0.1 uM in diameter. !!!!!! If
;;; true, this of course changes the communication between major branches drastically.

;;;; The morphological coordinates are from Tauchi and Masland (1984).  The dendrite diameters are inferred from Famiglietti
;;;; (1983). Diameter and XYZ entries are in microns. Z entries are fictional. 
 
(cell-type-def
 '(starburst
   (membrane-resistivity . 40000)
   (cytoplasmic-resistivity  . 200)
   (specific-capacitance . 0.7)
   (v-leak . -70)))


(defun star-amacrine (&optional (cell-name "starburst") &key (cell-origin '(0 0 0)) (segment-diameter 0.20))
  (setq *light-stimulus-plane* :xy)
  (let ((cell (create-cell cell-name :cell-type 'starburst :cell-origin cell-origin)))
    (create-tree (create-soma :cell cell :diameter 16) *star-amacrine-tree* :default-diameter segment-diameter :xy-factor 6.06)
    (setf (cell-type-notes (cell-type cell)) "Rabbit ACh starburst cell (cell 7) (~1.25 mm from streak), Tauchi and Masland (1984).")
    cell))

;;; The segment-list format is as follows:
;;;
;;;    (mother-segment-name segment-name x y z diameter extras-list) [diameter and extras-list are optional]
;;;

(defvar *star-amacrine-tree*
  `((soma	1a	7	-1	5	1.2)
    (1a	        1b	12	-3	7	0.6)
    (1b	        11	18	-5	10)
    (11	        111	20	-3	10)
    (111	1111	30	-3	10)
    (111	1112a	30	-5	10)
    (1112a	1112b	33	-5	10)
    (11	        112	30	-6	10)
    (11	        113a	17	-7	10)
    (113a	113b	24	-9	10)
    (113b	1131	28	-9	10)
    (113b	1132	24	-11	10)
    (1b	        12a	11	-6	10)
    (12a	12b	24	-14	10)
    (12b	121	28	-14	10)
    (12b	122	34	-17	10)
    (soma	2	-2	0	5	1.2)
    (2	        21	-3	-2	7	1.0)
    (21	        211	5	-6	10	0.6)
    (211	2111	16	-14	10)
    (2111	21111a	17	-13	10)
    (21111a	21111b	21	-15	10)
    (21111b	211111a	23	-16	10)
    (211111a	211111b	27	-16	10)
    (21111b	211112a	22	-17	10)
    (211112a	211112b	25	-20	10)
    (2111	21112	19	-18	10)
    (2111	21113	21	-23	10)
    (211	2112	9	-13	10)
    (2112	21121a	13	-14	10)
    (21121a	21121b	13	-17	10)
    (21121b	211211	18	-22	10)
    (21121b	211212	17	-24	10)
    (2112	21122	11	-16	10)
    (2112	21123a	8	-16	10)
    (21123a	21123b	12	-23	10)
    (21123b	211231	14	-24	10)
    (21123b	211232	12	-26	10)
    (21	        212a	-5	-3	10	0.8)
    (212a	212b	-5	-5	10	0.6)
    (212b	2121	-4	-7	10)
    (2121	21211	3	-11	10)
    (21211	212111	10	-22	10)
    (21211	212112	3	-18	10)
    (212112	2121121	9	-25	10)
    (212112	2121122a	1	-19	10)
    (2121122a	2121122b	5	-23	10)
    (21211	212113a	-1	-11	10)
    (212113a	212113b	0	-16	10)
    (2121	21212a	-7	-10	10)
    (21212a	21212b	-7	-13	10)
    (21212b	212121a	5	-24	10)
    (212121a	212121b	3	-25	10)
    (21212b	212122	-7	-17	10)
    (212122	2121221a	-2	-19	10)
    (2121221a	2121221b	0	-27	10)
    (212122	2121222	-7	-20	10)
    (2121222	21212221	-5	-26	10)
    (2121222	21212222	-5	-29	10)
    (212b	2122a	-8	-6	10)
    (2122a	2122b	-12	-13	10)
    (2122b	21221a	-10	-14	10)
    (21221a	21221b	-14	-26	10)
    (2122b	21222	-13	-15	10)
    (21222	212221	-14	-20	10)
    (212221	2122211a	-14	-23	10)
    (2122211a	2122211b	-16	-23	10)
    (212221	2122212	-17	-23	10)
    (2122212	21222121	-19	-26	10)
    (2122212	21222122	-19	-20	10)
    (21222	212222	-16	-16	10)
    (2	        22	-4	0	10	1.0)
    (22	        221	-8	-2	10	0.8)
    (221	2211	-10	-6	10	0.6)
    (2211	22111a	-10	-8	10)
    (22111a	22111b	-19	-16	10)
    (22111b	221111a	-25	-23	10)
    (221111a	221111b	-27	-22	10)
    (22111b	221112a	-20	-15	10)
    (221112a	221112b	-26	-18	10)
    (2211	22112	-13	-8	10)
    (22112	221121	-18	-13	10)
    (221121	2211211	-20	-14	10)
    (221121	2211212	-24	-14	10)
    (2211212	22112121	-24	-16	10)
    (2211212	22112122	-28	-16	10)
    (22112	221122	-19	-10	10)
    (221122	2211221a	-21	-12	10)
    (2211221a	2211221b	-22	-13	10)
    (221122	2211222	-30	-16	10)
    (221	2212	-14	-2	10	0.6)
    (2212	22121	-29	-8	10)
    (22121	221211a	-29	-12	10)
    (221211a	221211b	-32	-12	10)
    (22121	221212	-33	-9	10)
    (2212	22122	-17	-1	10)
    (22122	221221	-19	-2	10)
    (221221	2212211	-21	-3	10)
    (2212211	22122111	-21	-4	10)
    (2212211	22122112	-23	-4	10)
    (22122112	221221121	-29	-5	10)
    (22122112	221221122	-27	-4	10)
    (221221	2212212	-20	-2	10)
    (22122	221222a	-19	0	10)
    (221222a	221222b	-20	-1	10)
    (221222b	2212221	-30	-3	10)
    (221222b	2212222a	-28	-1	10)
    (2212222a	2212222b	-33	-3	10)
    (22	222	-15	4	10	0.6)
    (222	2221	-18	4	10)
    (2221	22211	-27	3	10)
    (22211	222111a	-32	3	10)
    (222111a	222111b	-39	0	10)
    (22211	222112	-35	5	10)
    (2221	22212	-28	8	10)
    (22212	222121a	-30	6	10)
    (222121a	222121b	-35	6	10)
    (22212	222122	-34	11	10)
    (222	2222	-25	10	10)
    (22	        223	-4	4	10	0.8)
    (223	2231	-11	8	10	0.6)
    (2231	22311	-15	9	10)
    (22311	223111	-21	10	10)
    (223111	2231111	-29	12	10)
    (2231111	22311111	-30	11	10)
    (2231111	22311112a	-32	13	10)
    (22311112a	22311112b	-34	12	10)
    (223111	2231112	-25	14	10)
    (2231112	22311121a	-29	18	10)
    (22311121a	22311121b	-31	17	10)
    (2231112	22311122a	-25	16	10)
    (22311122a	22311122b	-26	16	10)
    (22311	223112	-18	12	10)
    (223112	2231121	-22	14	10)
    (223112	2231122	-28	20	10)
    (2231	22312	-19	18	10)
    (22312	223121	-22	21	10)
    (223121	2231211a	-24	21	10)
    (2231211a	2231211b	-25	23	10)
    (223121	2231212a	-21	22	10)
    (2231212a	2231212b	-26	26	10)
    (22312	223122	-18	20	10)
    (223	2232	-8	9	10	0.6)
    (2232	22321	-18	21	10)
    (22321	223211a	-20	21	10)
    (223211a	223211b	-21	24	10)
    (22321	223212a	-19	24	10)
    (223212a	223212b	-23	25	10)
    (2232	22322	-9	16	10)
    (22322	223221	-11	18	10)
    (223221	2232211	-17	21	10)
    (223221	2232212	-18	29	10)
    (22322	223222a	-8	17	10)
    (223222a	223222b	-14	29	10)
    (223	2233a	-1	7	10	0.6)
    (2233a	2233b	-4	10	10)
    (2233b	22331	-12	28	10)
    (2233b	22332a	-3	18	10)
    (22332a	22332b	-12	31	10)
    (soma	3	0	2	5	1.2)
    (3	        31	4	9	7	0.6)
    (31	        311	2	11	10)
    (311	3111a	-1	11	10)
    (3111a	3111b	-2	18	10)
    (311	3112	3	17	10)
    (3112	31121	-4	30	10)
    (3112	31122	5	25	10)
    (31	        312	8	12	10)
    (312	3121	7	19	10)
    (312	3122	10	13	10)
    (3122	31221a	10	14	10)
    (31221a	31221b	15	20	10)
    (3122	31222a	12	14	10)
    (31222a	31222b	22	20	10)
    (31221b	312211	14	22	10)
    (31221b	312212	18	22	10)
    (31	        313a	8	8	10)
    (313a	313b	18	16	10)
    (3	        32	6	3	10	0.9)
    (32	        321	12	5	10	0.6)
    (321	3211	18	9	10)
    (3211	32111a	17	13	10)
    (32111a	32111b	22	19	10)
    (3211	32112	32	17	10)
    (321	3212a	14	4	10)
    (3212a	3212b	19	6	10)
    (3212b	32121	33	13	10)
    (3212b	32122	30	9	10)
    (32	        322	18	2	10	0.6)
    (322	3221	23	3	10)
    (3221	32211	35	5	10)
    (3221	32212	33	0	10)
    (322	3222	26	-1	10)
    ))



















