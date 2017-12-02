;;; -*- Package: SURF; Mode: LISP -*-
(in-package "SURF-HIPPO")
#|

 *******  Dump Tree File for Cell ca1-max-red  ******* 


    Cell originally created from c12861.ca1.sparcf


************************************
 Friday, 3/10/95 05:32:00 am EST
************************************
Simulation 'c12861.ca1-1752032' [Composed from: c12861.ca1.sparcf]
[File: /home/lyle/surf-hippo/anatomy/c12861.ca1.sparcf]

Trees consolidated with a max electrotonic length of 10.25
1 cell type, 1 cell, with 182 nodes. Temperature 27.0 degrees(C).
There is 1 soma, and 181 segments.

  Cell-type ca1-max-red:
  Rm 40000.0, Rm-sm 40000.0 (ohm-cm2)
  Soma shunt 1.0e+30 ohms, Ra 200.0 ohm-cm, Cm-sm 0.7, Cm-den 0.7 (uF/cm2) 
  E-soma-leak -70.0 mV, E-dendrite-leak -70.0 mV 
  Cells of type ca1-max-red are:
    ca1-max-red (soma @ [0.0 0.0 0.0])
    Created from c12861.ca1.sparcf
      Max G-in/Min R-in = 5.14e-2 uS/1.95e+1 Mohms 
      Soma passive R-in = 3.83e+3 Mohms 
      Dendritic tree passive R-in (actual model) = 1.39e+2 Mohms 
                                  (cable model) = 1.35e+2 Mohms 
      Passive total R-in from soma (actual model) = 1.34e+2 Mohms 
                                   (cable model) = 1.31e+2 Mohms 



|#

(defun ca1-max-red (&key (cell-type 'HPC) (name "ca1-max-red"))
  (defvar CA1-MAX-RED-CELL-TREE)
  (setq CA1-MAX-RED-CELL-TREE 
	`( ("9" "162" 75.6 -230.6 -71.9 1.0 ((RA-COEFF 1.1))) 
	  ("9" "8" 105.1 -48.6 -49.9 1.1 ((RA-COEFF 1.2))) 
	  ("7" "9" 19.1 -42.6 -11.9 1.0) 
	  ("2" "18" -55.4 -218.1 -17.4 1.0 ((RA-COEFF 1.1))) 
	  ("159" "145" -28.4 -251.6 -16.9 1.0 ((RA-COEFF 1.0))) 
	  ("159" "10" 4.6 -197.1 -57.4 1.0 ((RA-COEFF 1.2))) 
	  ("29" "159" -1.4 -113.6 -20.4 1.0 ((RA-COEFF 1.0))) 
	  ("171" "82" 2.6 -131.1 -64.4 1.1 ((RA-COEFF 1.2))) 
	  ("171" "178" 63.6 -221.1 -83.4 1.1 ((RA-COEFF 1.3))) 
	  ("29" "171" 10.6 -89.1 -33.9 1.0 ((RA-COEFF 1.1))) 
	  ("2" "29" 4.1 -67.1 -14.9 1.0 ((RA-COEFF 1.0))) 
	  ("7" "2" 7.6 -34.1 -10.4 1.0) 
	  ("169" "7" 10.1 -29.6 -7.9 1.0) 
	  ("14" "161" 132.6 -200.1 -76.9 1.1 ((RA-COEFF 1.2))) 
	  ("40" "60" 124.1 -186.1 -44.4 1.0 ((RA-COEFF 1.0))) 
	  ("40" "3" 121.1 -215.1 -35.9 1.0 ((RA-COEFF 1.0))) 
	  ("14" "40" 101.1 -174.6 -32.9 1.0 ((RA-COEFF 1.0))) 
	  ("169" "14" 31.1 -42.1 -9.9 1.0 ((RA-COEFF 1.0))) 
	  (SOMA "169" 8.6 -19.1 -5.9 1.0) 
	  ("165" "180" -90.9 -192.6 37.6 1.0 ((RA-COEFF 1.1))) 
	  ("19" "152" -54.9 -263.1 13.6 1.0 ((RA-COEFF 1.1))) 
	  ("19" "166" -6.9 -236.1 1.6 1.0 ((RA-COEFF 1.0))) 
	  ("165" "19" -24.9 -105.1 18.1 1.0 ((RA-COEFF 1.0))) 
	  ("129" "165" -15.9 -59.6 15.6 1.1 ((RA-COEFF 1.2))) 
	  ("126" "181" -34.4 -256.6 24.1 1.0 ((RA-COEFF 1.1))) 
	  ("126" "22" -29.9 -134.1 77.1 1.0 ((RA-COEFF 1.1))) 
	  ("47" "126" -6.9 -56.6 24.1 1.0 ((RA-COEFF 1.1))) 
	  ("163" "125" -12.9 -231.6 14.6 1.0 ((RA-COEFF 1.1))) 
	  ("163" "1" 89.6 -227.6 16.6 1.0 ((RA-COEFF 1.1))) 
	  ("47" "163" 14.1 -90.6 21.1 1.0 ((RA-COEFF 1.0))) 
	  ("129" "47" 0.1 -43.1 19.1 1.1 ((RA-COEFF 1.4))) 
	  (SOMA "129" -3.9 -25.1 1.6 1.0) 
	  ("170" "15" -94.4 -240.1 39.1 1.0 ((RA-COEFF 1.1))) 
	  ("170" "31" -40.9 -228.6 37.6 1.0 ((RA-COEFF 1.0))) 
	  ("156" "170" -40.4 -119.1 27.1 1.0 ((RA-COEFF 1.1))) 
	  ("177" "78" -88.9 -202.6 19.1 1.1 ((RA-COEFF 1.2))) 
	  ("130" "4" -80.9 -244.6 -29.4 1.1 ((RA-COEFF 1.3))) 
	  ("130" "57" -18.4 -228.6 -21.9 1.0 ((RA-COEFF 1.0))) 
	  ("177" "130" -35.9 -103.1 3.1 1.0 ((RA-COEFF 1.0))) 
	  ("116" "177" -11.9 -55.1 5.1 1.0) 
	  ("158" "141" -4.4 -225.1 4.6 1.1 ((RA-COEFF 1.5))) 
	  ("158" "98" 9.1 -206.6 -14.9 1.0 ((RA-COEFF 1.0))) 
	  ("36" "158" -3.4 -172.1 -7.4 1.0 ((RA-COEFF 1.1))) 
	  ("36" "167" -1.9 -216.6 -33.4 1.2 ((RA-COEFF 1.5))) 
	  ("116" "36" -3.4 -141.1 -4.9 1.0 ((RA-COEFF 1.1))) 
	  ("156" "116" -5.9 -41.1 6.6 1.0 ((RA-COEFF 1.0))) 
	  ("154" "156" -1.9 -29.1 2.6 1.0) 
	  ("13" "102" -91.4 -201.1 30.1 1.0 ((RA-COEFF 1.2))) 
	  ("13" "120" -42.4 -205.1 43.1 1.0 ((RA-COEFF 1.0))) 
	  ("154" "13" -23.9 -90.1 28.6 1.1 ((RA-COEFF 1.2))) 
	  (SOMA "154" -1.4 -24.6 4.1 1.0) 
	  ("5" "42" 136.6 -224.6 1.6 1.0 ((RA-COEFF 1.1))) 
	  ("5" "179" 128.1 -88.1 -3.4 1.1 ((RA-COEFF 1.3))) 
	  ("121" "5" 19.6 -31.6 1.6 1.0) 
	  ("174" "143" 83.6 -253.1 -27.9 1.0 ((RA-COEFF 1.1))) 
	  ("174" "164" 104.6 -261.6 -9.4 1.0 ((RA-COEFF 1.1))) 
	  ("85" "174" 27.6 -91.1 -4.4 1.0) 
	  ("85" "150" 49.1 -210.6 -44.9 1.0 ((RA-COEFF 1.1))) 
	  ("121" "85" 26.6 -88.6 -4.9 1.0 ((RA-COEFF 1.0))) 
	  (SOMA "121" 10.6 -25.6 0.1 1.1 ((RA-COEFF 1.4))) 
	  ("144" "77" -15.9 356.9 -1.9 1.0 ((RA-COEFF 1.1))) 
	  ("127" "124" 143.6 492.9 79.6 1.0 ((RA-COEFF 1.1))) 
	  ("127" "173" 76.6 472.9 114.6 1.1 ((RA-COEFF 1.4))) 
	  ("58" "127" 65.6 451.9 49.1 1.1 ((RA-COEFF 1.3))) 
	  ("58" "131" -70.4 528.9 146.6 1.2 ((RA-COEFF 1.7))) 
	  ("75" "58" 41.1 435.4 30.6 1.0 ((RA-COEFF 1.1))) 
	  ("175" "59" 152.1 485.9 27.6 1.0 ((RA-COEFF 1.1))) 
	  ("147" "79" 128.1 539.4 87.6 1.2 ((RA-COEFF 1.5))) 
	  ("147" "21" -47.4 546.9 39.1 1.2 ((RA-COEFF 1.8))) 
	  ("175" "147" 74.6 494.9 55.1 1.2 ((RA-COEFF 1.7))) 
	  ("75" "175" 92.1 459.4 40.1 1.1 ((RA-COEFF 1.3))) 
	  ("172" "75" 48.1 425.4 27.1 1.0 ((RA-COEFF 1.1))) 
	  ("132" "55" 14.6 575.4 21.1 1.1 ((RA-COEFF 1.3))) 
	  ("49" "32" -150.9 552.9 -69.4 1.1 ((RA-COEFF 1.3))) 
	  ("49" "6" -254.9 618.4 -45.9 1.0 ((RA-COEFF 1.1))) 
	  ("132" "49" -128.4 481.4 -10.4 1.0 ((RA-COEFF 1.0))) 
	  ("172" "132" -44.9 431.4 8.6 1.0 ((RA-COEFF 1.1))) 
	  ("153" "172" 18.1 368.4 27.1 1.0) 
	  ("86" "157" 85.6 643.4 -100.4 1.1 ((RA-COEFF 1.4))) 
	  ("86" "89" 239.1 570.4 -60.4 1.0 ((RA-COEFF 1.0))) 
	  ("63" "86" 44.1 435.4 -6.9 1.0 ((RA-COEFF 1.1))) 
	  ("76" "140" -122.4 565.4 -95.4 1.1 ((RA-COEFF 1.3))) 
	  ("76" "56" -126.9 524.9 -18.9 1.1 ((RA-COEFF 1.2))) 
	  ("11" "76" -74.9 463.4 -30.4 1.1 ((RA-COEFF 1.3))) 
	  ("16" "69" -207.9 543.9 -127.4 1.0 ((RA-COEFF 1.1))) 
	  ("16" "146" -17.9 487.9 -134.4 1.1 ((RA-COEFF 1.2))) 
	  ("11" "16" -48.4 412.4 -26.9 1.1 ((RA-COEFF 1.2))) 
	  ("63" "11" -19.4 398.9 -3.9 1.0 ((RA-COEFF 1.0))) 
	  ("153" "63" 11.6 384.4 13.6 1.0 ((RA-COEFF 1.0))) 
	  ("149" "153" 20.1 354.4 23.6 1.0) 
	  ("149" "103" 38.6 396.4 -21.9 1.1 ((RA-COEFF 1.5))) 
	  ("137" "149" 20.1 348.4 24.1 1.0) 
	  ("137" "113" 121.1 404.4 43.6 1.1 ((RA-COEFF 1.2))) 
	  ("136" "137" 22.6 321.4 24.6 1.0) 
	  ("136" "37" 30.6 340.9 63.6 1.5 ((RA-COEFF 3.5))) 
	  ("135" "136" 23.6 295.4 22.6 1.0) 
	  ("23" "62" 100.1 369.9 6.1 1.1 ((RA-COEFF 1.3))) 
	  ("23" "30" 97.1 424.4 20.6 1.0 ((RA-COEFF 1.1))) 
	  ("135" "23" 32.6 291.4 19.1 1.0) 
	  ("61" "135" 23.6 276.9 21.6 1.0) 
	  ("61" "133" 23.1 363.9 83.1 1.1 ((RA-COEFF 1.3))) 
	  ("144" "61" 23.6 266.4 19.6 1.0 ((RA-COEFF 1.0))) 
	  ("142" "144" 20.1 234.9 18.1 1.0) 
	  ("80" "87" -54.4 362.9 9.1 1.1 ((RA-COEFF 1.2))) 
	  ("80" "160" 3.6 415.9 -6.9 1.0 ((RA-COEFF 1.1))) 
	  ("142" "80" 5.6 277.4 14.1 1.0 ((RA-COEFF 1.1))) 
	  ("139" "142" 20.1 231.9 18.6 1.0) 
	  ("139" "155" 67.1 352.4 62.1 1.1 ((RA-COEFF 1.2))) 
	  ("123" "139" 19.6 229.9 18.6 1.0) 
	  ("104" "114" -62.4 371.4 7.1 1.0 ((RA-COEFF 1.1))) 
	  ("104" "168" -66.4 337.9 47.1 1.1 ((RA-COEFF 1.2))) 
	  ("123" "104" -0.9 261.4 21.1 1.0 ((RA-COEFF 1.1))) 
	  ("122" "123" 19.1 222.4 17.6 1.1) 
	  ("122" "38" 93.6 342.9 30.6 1.0 ((RA-COEFF 1.0))) 
	  ("101" "122" 18.1 197.4 17.1 1.1) 
	  ("101" "94" 86.6 327.4 -7.4 1.1 ((RA-COEFF 1.2))) 
	  ("100" "101" 18.1 179.9 14.6 1.1) 
	  ("20" "70" 68.1 242.4 43.6 1.1 ((RA-COEFF 1.2))) 
	  ("20" "148" -13.9 278.9 79.6 1.0 ((RA-COEFF 1.2))) 
	  ("64" "20" 15.1 169.4 21.6 1.0) 
	  ("64" "134" 38.6 104.9 63.6 1.1 ((RA-COEFF 1.5))) 
	  ("100" "64" 13.6 166.4 23.1 1.0 ((RA-COEFF 1.2))) 
	  ("99" "100" 17.1 162.9 14.6 1.0) 
	  ("81" "88" 129.1 188.9 19.1 1.1 ((RA-COEFF 1.2))) 
	  ("81" "48" 115.6 289.4 -5.9 1.0 ((RA-COEFF 1.1))) 
	  ("99" "81" 39.1 158.4 10.1 1.0 ((RA-COEFF 1.0))) 
	  ("95" "99" 17.6 154.4 15.6 1.2) 
	  ("95" "151" -20.9 298.4 29.6 1.0 ((RA-COEFF 1.1))) 
	  ("112" "95" 16.1 138.9 14.6 1.2) 
	  ("105" "39" 51.6 106.9 47.1 1.3 ((RA-COEFF 2.0))) 
	  ("43" "115" 114.6 106.9 44.1 1.1 ((RA-COEFF 1.2))) 
	  ("43" "128" 53.6 189.4 58.6 1.1 ((RA-COEFF 1.2))) 
	  ("105" "43" 45.1 132.9 31.6 1.1 ((RA-COEFF 1.2))) 
	  ("112" "105" 38.1 133.9 21.6 1.2 ((RA-COEFF 1.8))) 
	  ("111" "112" 16.6 131.9 13.6 1.2) 
	  ("68" "73" -52.4 135.4 31.6 1.1 ((RA-COEFF 1.2))) 
	  ("68" "92" -66.9 159.4 24.6 1.0 ((RA-COEFF 1.1))) 
	  ("17" "68" -25.9 134.9 20.1 1.0 ((RA-COEFF 1.0))) 
	  ("17" "74" -36.9 204.4 61.6 1.1 ((RA-COEFF 1.4))) 
	  ("111" "17" 11.6 124.9 12.6 1.0) 
	  ("110" "111" 14.1 127.4 12.6 1.5) 
	  ("110" "138" -23.9 331.4 -1.4 1.0 ((RA-COEFF 1.1))) 
	  ("106" "110" 14.1 122.4 12.1 1.2) 
	  ("84" "91" 98.1 187.4 35.1 1.1 ((RA-COEFF 1.3))) 
	  ("84" "119" 61.1 188.4 -3.9 1.1 ((RA-COEFF 1.2))) 
	  ("106" "84" 37.6 129.4 11.1 1.1 ((RA-COEFF 1.2))) 
	  ("35" "106" 13.1 111.4 10.6 1.6) 
	  ("97" "109" 85.6 56.4 -1.4 1.1 ((RA-COEFF 1.4))) 
	  ("97" "67" 99.1 34.4 17.1 1.0 ((RA-COEFF 1.2))) 
	  ("35" "97" 67.6 62.4 12.6 1.0 ((RA-COEFF 1.1))) 
	  ("34" "35" 9.1 89.9 8.1 1.6) 
	  ("34" "118" 49.6 14.4 8.1 1.2 ((RA-COEFF 1.6))) 
	  ("28" "34" 8.6 87.9 8.1 1.5) 
	  ("41" "45" -39.4 250.9 1.6 1.0 ((RA-COEFF 1.1))) 
	  ("41" "26" 12.1 281.9 -24.9 1.0 ((RA-COEFF 1.1))) 
	  ("28" "41" -8.4 152.9 -0.4 1.0 ((RA-COEFF 1.1))) 
	  ("27" "28" 6.6 84.9 8.1 1.5) 
	  ("66" "72" 4.6 68.4 96.1 1.1 ((RA-COEFF 1.5))) 
	  ("66" "12" -87.9 81.4 58.1 1.1 ((RA-COEFF 1.5))) 
	  ("27" "66" -5.4 67.9 20.6 1.1 ((RA-COEFF 1.2))) 
	  ("25" "27" 2.1 68.4 5.6 1.6) 
	  ("54" "83" -69.4 149.9 -15.4 1.1 ((RA-COEFF 1.3))) 
	  ("54" "108" -117.4 160.4 13.6 1.0 ((RA-COEFF 1.0))) 
	  ("53" "54" -26.4 78.9 5.1 1.0) 
	  ("53" "90" -70.9 159.4 24.1 1.0 ((RA-COEFF 1.1))) 
	  ("51" "53" -23.9 75.9 4.6 1.0) 
	  ("51" "96" -108.9 111.4 44.1 1.1 ((RA-COEFF 1.4))) 
	  ("25" "51" -10.4 59.9 5.6 1.0) 
	  ("24" "25" -1.4 58.4 4.6 1.7) 
	  ("107" "117" 79.6 213.9 -38.9 1.0 ((RA-COEFF 1.0))) 
	  ("107" "93" 95.6 137.9 1.6 1.1 ((RA-COEFF 1.2))) 
	  ("24" "107" 42.6 96.9 -6.9 1.0 ((RA-COEFF 1.1))) 
	  ("33" "24" -0.9 52.4 3.1 1.7) 
	  ("52" "44" -22.9 100.4 -17.4 1.0 ((RA-COEFF 1.1))) 
	  ("52" "176" 17.1 144.4 -33.9 1.0 ((RA-COEFF 1.0))) 
	  ("50" "52" -8.9 54.9 -14.4 1.0) 
	  ("50" "65" -54.9 59.4 -40.4 1.3 ((RA-COEFF 2.0))) 
	  ("33" "50" -7.4 49.4 -10.9 1.0) 
	  ("46" "33" -0.4 36.4 -0.9 1.7) 
	  ("46" "71" 69.1 171.4 4.6 1.1 ((RA-COEFF 1.2))) 
	  (SOMA "46" 1.6 27.9 -0.4 2.8 ((RA-COEFF 1.2))) ))
  (create-tree
   (create-soma :cell (create-cell name :cell-type cell-type)
		:diameter 18.238783)		; Soma diameter in um
   ca1-max-red-cell-tree))


(push 'ca1-max-red *CIRCUIT-CATALOG-FUNCTIONS*)



