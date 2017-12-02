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

(defun star-amacrine (&key (cell-name "starburst") (origin '(0 0 0)))
  (let ((cell (create-cell cell-name :cell-type 'starburst :origin origin)))
    (create-tree (create-soma :cell cell :diameter 16) *star-amacrine-tree*)
    (setf (cell-type-notes (cell-type cell)) "Rabbit ACh starburst cell (cell 7) (~1.25 mm from streak), Tauchi and Masland (1984).")
    cell))

;;; The segment-list format is as follows:
;;;
;;;    (mother-segment-name segment-name x y z diameter extras-list) [diameter and extras-list are optional]
;;;

(defvar *star-amacrine-tree*
  `((7 8 181.8 -18.18 10.0 0.2)
    (9 10 199.98 -30.3 10.0 0.2)
    (7 9 181.8 -30.3 10.0 0.2)
    (6 7 121.200005 -18.18 10.0 0.2)
    (6 11 181.8 -36.36 10.0 0.2)
    (13 14 169.68 -54.54 10.0 0.2)
    (13 15 145.43999 -66.659996 10.0 0.2)
    (12 13 145.43999 -54.54 10.0 0.2)
    (6 12 103.020004 -42.420002 10.0 0.2)
    (5 6 109.08 -30.3 10.0 0.2)
    (17 18 169.68 -84.840004 10.0 0.2)
    (17 19 206.04001 -103.020004 10.0 0.2)
    (16 17 145.43999 -84.840004 10.0 0.2)
    (5 16 66.659996 -36.36 10.0 0.2)
    (4 5 72.72 -18.18 7.0 0.6)
    (SOMA 4 42.420002 -6.06 5.0 1.2)
    (28 29 163.62 -96.96 10.0 0.2)
    (27 28 139.38 -96.96 10.0 0.2)
    (30 33 151.5 -121.200005 10.0 0.2)
    (27 30 133.31999 -103.020004 10.0 0.2)
    (26 27 127.26 -90.9 10.0 0.2)
    (25 26 103.020004 -78.78 10.0 0.2)
    (25 34 115.14 -109.08 10.0 0.2)
    (25 35 127.26 -139.38 10.0 0.2)
    (24 25 96.96 -84.840004 10.0 0.2)
    (38 39 109.08 -133.31999 10.0 0.2)
    (38 40 103.020004 -145.43999 10.0 0.2)
    (37 38 78.78 -103.020004 10.0 0.2)
    (36 37 78.78 -84.840004 10.0 0.2)
    (36 41 66.659996 -96.96 10.0 0.2)
    (43 44 84.840004 -145.43999 10.0 0.2)
    (43 45 72.72 -157.56 10.0 0.2)
    (42 43 72.72 -139.38 10.0 0.2)
    (36 42 48.48 -96.96 10.0 0.2)
    (24 36 54.54 -78.78 10.0 0.2)
    (23 24 30.3 -36.36 10.0 0.6)
    (49 50 60.6 -133.31999 10.0 0.2)
    (51 52 54.54 -151.5 10.0 0.2)
    (53 54 30.3 -139.38 10.0 0.2)
    (51 53 6.06 -115.14 10.0 0.2)
    (49 51 18.18 -109.08 10.0 0.2)
    (55 56 0.0 -96.96 10.0 0.2)
    (49 55 -6.06 -66.659996 10.0 0.2)
    (48 49 18.18 -66.659996 10.0 0.2)
    (59 60 18.18 -151.5 10.0 0.2)
    (58 59 30.3 -145.43999 10.0 0.2)
    (62 63 0.0 -163.62 10.0 0.2)
    (61 62 -12.12 -115.14 10.0 0.2)
    (64 65 -30.3 -157.56 10.0 0.2)
    (64 66 -30.3 -175.74 10.0 0.2)
    (61 64 -42.420002 -121.200005 10.0 0.2)
    (58 61 -42.420002 -103.020004 10.0 0.2)
    (57 58 -42.420002 -78.78 10.0 0.2)
    (48 57 -42.420002 -60.6 10.0 0.2)
    (47 48 -24.24 -42.420002 10.0 0.2)
    (69 70 -84.840004 -157.56 10.0 0.2)
    (68 69 -60.6 -84.840004 10.0 0.2)
    (73 74 -96.96 -139.38 10.0 0.2)
    (72 73 -84.840004 -139.38 10.0 0.2)
    (75 76 -115.14 -157.56 10.0 0.2)
    (75 77 -115.14 -121.200005 10.0 0.2)
    (72 75 -103.020004 -139.38 10.0 0.2)
    (71 72 -84.840004 -121.200005 10.0 0.2)
    (71 78 -96.96 -96.96 10.0 0.2)
    (68 71 -78.78 -90.9 10.0 0.2)
    (67 68 -72.72 -78.78 10.0 0.2)
    (47 67 -48.48 -36.36 10.0 0.2)
    (46 47 -30.3 -30.3 10.0 0.6)
    (23 46 -30.3 -18.18 10.0 0.8)
    (20 23 -18.18 -12.12 7.0 1.0)
    (84 85 -163.62 -133.31999 10.0 0.2)
    (83 84 -151.5 -139.38 10.0 0.2)
    (86 87 -157.56 -109.08 10.0 0.2)
    (83 86 -121.200005 -90.9 10.0 0.2)
    (82 83 -115.14 -96.96 10.0 0.2)
    (81 82 -60.6 -48.48 10.0 0.2)
    (89 90 -121.200005 -84.840004 10.0 0.2)
    (91 92 -145.43999 -96.96 10.0 0.2)
    (91 93 -169.68 -96.96 10.0 0.2)
    (89 91 -145.43999 -84.840004 10.0 0.2)
    (88 89 -109.08 -78.78 10.0 0.2)
    (95 96 -133.31999 -78.78 10.0 0.2)
    (94 95 -127.26 -72.72 10.0 0.2)
    (94 97 -181.8 -96.96 10.0 0.2)
    (88 94 -115.14 -60.6 10.0 0.2)
    (81 88 -78.78 -48.48 10.0 0.2)
    (80 81 -60.6 -36.36 10.0 0.6)
    (100 101 -193.92 -72.72 10.0 0.2)
    (99 100 -175.74 -72.72 10.0 0.2)
    (99 102 -199.98 -54.54 10.0 0.2)
    (98 99 -175.74 -48.48 10.0 0.2)
    (105 106 -127.26 -24.24 10.0 0.2)
    (107 108 -175.74 -30.3 10.0 0.2)
    (107 109 -163.62 -24.24 10.0 0.2)
    (105 107 -139.38 -24.24 10.0 0.2)
    (104 105 -127.26 -18.18 10.0 0.2)
    (104 110 -121.200005 -12.12 10.0 0.2)
    (103 104 -115.14 -12.12 10.0 0.2)
    (112 113 -181.8 -18.18 10.0 0.2)
    (114 115 -199.98 -18.18 10.0 0.2)
    (112 114 -169.68 -6.06 10.0 0.2)
    (111 112 -121.200005 -6.06 10.0 0.2)
    (103 111 -115.14 0.0 10.0 0.2)
    (98 103 -103.020004 -6.06 10.0 0.2)
    (80 98 -84.840004 -12.12 10.0 0.6)
    (79 80 -48.48 -12.12 10.0 0.8)
    (119 120 -236.34 0.0 10.0 0.2)
    (118 119 -193.92 18.18 10.0 0.2)
    (118 121 -212.1 30.3 10.0 0.2)
    (117 118 -163.62 18.18 10.0 0.2)
    (123 124 -212.1 36.36 10.0 0.2)
    (122 123 -181.8 36.36 10.0 0.2)
    (122 125 -206.04001 66.659996 10.0 0.2)
    (117 122 -169.68 48.48 10.0 0.2)
    (116 117 -109.08 24.24 10.0 0.2)
    (116 126 -151.5 60.6 10.0 0.2)
    (79 116 -90.9 24.24 10.0 0.6)
    (131 132 -181.8 66.659996 10.0 0.2)
    (133 134 -206.04001 72.72 10.0 0.2)
    (131 133 -193.92 78.78 10.0 0.2)
    (130 131 -175.74 72.72 10.0 0.2)
    (136 137 -187.86 103.020004 10.0 0.2)
    (135 136 -175.74 109.08 10.0 0.2)
    (138 139 -157.56 96.96 10.0 0.2)
    (135 138 -151.5 96.96 10.0 0.2)
    (130 135 -151.5 84.840004 10.0 0.2)
    (129 130 -127.26 60.6 10.0 0.2)
    (140 141 -133.31999 84.840004 10.0 0.2)
    (140 142 -169.68 121.200005 10.0 0.2)
    (129 140 -109.08 72.72 10.0 0.2)
    (128 129 -90.9 54.54 10.0 0.2)
    (145 146 -151.5 139.38 10.0 0.2)
    (144 145 -145.43999 127.26 10.0 0.2)
    (147 148 -157.56 157.56 10.0 0.2)
    (144 147 -127.26 133.31999 10.0 0.2)
    (143 144 -133.31999 127.26 10.0 0.2)
    (143 149 -109.08 121.200005 10.0 0.2)
    (128 143 -115.14 109.08 10.0 0.2)
    (127 128 -66.659996 48.48 10.0 0.6)
    (152 153 -127.26 145.43999 10.0 0.2)
    (151 152 -121.200005 127.26 10.0 0.2)
    (154 155 -139.38 151.5 10.0 0.2)
    (151 154 -115.14 145.43999 10.0 0.2)
    (150 151 -109.08 127.26 10.0 0.2)
    (157 158 -103.020004 127.26 10.0 0.2)
    (157 159 -109.08 175.74 10.0 0.2)
    (156 157 -66.659996 109.08 10.0 0.2)
    (160 161 -84.840004 175.74 10.0 0.2)
    (156 160 -48.48 103.020004 10.0 0.2)
    (150 156 -54.54 96.96 10.0 0.2)
    (127 150 -48.48 54.54 10.0 0.6)
    (163 164 -72.72 169.68 10.0 0.2)
    (165 166 -72.72 187.86 10.0 0.2)
    (163 165 -18.18 109.08 10.0 0.2)
    (162 163 -24.24 60.6 10.0 0.2)
    (127 162 -6.06 42.420002 10.0 0.6)
    (79 127 -24.24 24.24 10.0 0.8)
    (20 79 -24.24 0.0 10.0 1.0)
    (SOMA 20 -12.12 0.0 5.0 1.2)
    (170 171 -12.12 109.08 10.0 0.2)
    (169 170 -6.06 66.659996 10.0 0.2)
    (172 173 -24.24 181.8 10.0 0.2)
    (172 174 30.3 151.5 10.0 0.2)
    (169 172 18.18 103.020004 10.0 0.2)
    (168 169 12.12 66.659996 10.0 0.2)
    (175 176 42.420002 115.14 10.0 0.2)
    (179 182 84.840004 133.31999 10.0 0.2)
    (179 183 109.08 133.31999 10.0 0.2)
    (178 179 90.9 121.200005 10.0 0.2)
    (177 178 60.6 84.840004 10.0 0.2)
    (180 181 133.31999 121.200005 10.0 0.2)
    (177 180 72.72 84.840004 10.0 0.2)
    (175 177 60.6 78.78 10.0 0.2)
    (168 175 48.48 72.72 10.0 0.2)
    (184 185 109.08 96.96 10.0 0.2)
    (168 184 48.48 48.48 10.0 0.2)
    (167 168 24.24 54.54 7.0 0.6)
    (189 190 133.31999 115.14 10.0 0.2)
    (188 189 103.020004 78.78 10.0 0.2)
    (188 191 193.92 103.020004 10.0 0.2)
    (187 188 109.08 54.54 10.0 0.2)
    (193 194 199.98 78.78 10.0 0.2)
    (193 195 181.8 54.54 10.0 0.2)
    (192 193 115.14 36.36 10.0 0.2)
    (187 192 84.840004 24.24 10.0 0.2)
    (186 187 72.72 30.3 10.0 0.6)
    (197 198 212.1 30.3 10.0 0.2)
    (197 199 199.98 0.0 10.0 0.2)
    (196 197 139.38 18.18 10.0 0.2)
    (196 200 157.56 -6.06 10.0 0.2)
    (186 196 109.08 12.12 10.0 0.6)
    (167 186 36.36 18.18 10.0 0.90000004)
    (SOMA 167 0.0 12.12 5.0 1.2)))




















