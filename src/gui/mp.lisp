;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: WINDOWS-HACK; Base: 10 -*-

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

;; GUI Source file: mp.lisp


(IN-PACKAGE "WINDOWS-HACK")


(in-package :user)

(format t "THIS A A HUDGE SECURITY RISC. CHANGE THE PASSWORD!!!~%~%")
(setf mp::*idle-process* mp::*initial-process*)
(mp::start-lisp-connection-listener :port 6789 :password "Clara")

;;; now you can telnet in and do:
#|
$telnet localhost 6789
Trying 127.0.0.1...
Connected to localhost.
Escape character is '^]'.
Enter password: "Clara"

CMU Common Lisp Experimental 18a+ release x86-linux 2.2.0 cvs, running on slartibartfast
Send bug reports and questions to your local CMU CL maintainer,
or to pw@snoopy.mv.com,
or to s950045@uia.ac.be or Peter.VanEynde@uia.ua.ac.be
or to cmucl-help@cons.org. (prefered)

type (help) for help, (quit) to exit, and (demo) to see the demos

Loaded subsystems:
    Python 1.0, target Intel x86
    CLOS based on PCL version:  September 16 92 PCL (f)
* (+ 1 1)

2
* (quit)
Connection closed by foreign host.
|#

