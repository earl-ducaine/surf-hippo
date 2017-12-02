;;; -*- Mode: Lisp; Package: Kernel -*-
;;;

(in-package "KERNEL")

#|
Return-Path: <owner-cmucl-imp@seagull.cdrom.com>
X-Authentication-Warning: seagull.cdrom.com: majordom set sender to owner-cmucl-imp@seagull.cdrom.com using -f
Organization:  Centro di Calcolo - Dipartimento di Informatica di Pisa - Italy
Date: Fri, 27 Aug 1999 18:11:49 +0200 (MET DST)
From: Pierpaolo Bernardi <bernardp@CLI.DI.Unipi.IT>
To: cmucl-imp@cons.org
Subject: RANDOM broken
Sender: owner-cmucl-imp@seagull.cons.org
Precedence: bulk


Hello,

While looking at the rand-mt19937 code, with the intent of making it
portable CL, I found a serious bug in RANDOM:

(defun random (arg &optional (state *random-state*))
...
  (cond
    ((and (fixnump arg) (<= arg random-fixnum-max))
     (rem (random-chunk state) arg))
     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

This is not correct.  Analogously for the INTEGER case.

For an explanation, see for example Knuth, The Stanford GraphBase,
page 221, or TAOCP.

the following is the function that I use for obtaining small integers
from (unsigned 32)s.


Hope this helps.

Pierpaolo

|#

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant lim-chunk (expt 2 random-chunk-length)))

(defun random-small-integer (arg &optional (state *random-state*))
  (declare (type (integer 1 (#.lim-chunk)) arg)
           (type random-state state))
  (loop with good = (- #.lim-chunk (mod #.lim-chunk arg))
      for chunk = (random-chunk state)
      while (> chunk good)
      finally (return (mod chunk arg))))




