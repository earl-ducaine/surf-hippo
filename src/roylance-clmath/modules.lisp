;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: USER -*-

;;;; Module Name to Pathname Mappings

;;;  (c) Copyright Gerald Roylance 1989
;;;      All Rights Reserved.
;;;  No part of this material may be used, photocopied,
;;;  reproduced or committed to magnetic, electronic or any other
;;;  form of media without prior written consent.

;;; Bugs and Fixes
;;;   compile only those files that need it
;;;   make distribution tapes (maybe binary only)
;;;     include/exclude some systems
;;;   separate compilation without loading
;;;   bug with declares
;;;   release/version/edit
;;;   patches
;;;   might have an argument that imports the packages
;;;     external symbols.
;;;   have a module-spec-date that defaults to the src-date
;;;     if binary is younger, then recompile
;;;     eg, system-a uses system-b
;;;     we start loading system-a using its date
;;;       subload of system-b notices that system-a was
;;;       compiled before system-b's spec-date
;;;       -> system-a must be recompiled.

(in-package "USER")

(provide "MODULES")

(export '(
	  module-pathname-get
	  module-pathname-put
	  module-compile
	  module-compile-file
	  module-require
	  module-provide))

(eval-when (compile)
  (error "Do Not Compile the File!"))


;;;; Module Name - Pathname Database

;;; Keep the correspondence between names and pathnames in a hash table.
;;;
(defvar module-database (make-hash-table :test #'equal :size 200))

(defun module-pathname-get (name)
  (let ((pathnames (gethash name module-database)))
    (when (null pathnames)
      (warn "Module ~A has no pathname translations" name))
    pathnames))

;;; Associate Name with Pathname
;;;
(defun module-pathname-put (name pathname)
  (if (null pathname)
      (remhash name module-database)
      (setf (gethash name module-database) pathname)))

(defmacro module-namestring (module-name host namestring)
  `(module-pathname-put ,module-name (parse-namestring ,namestring ,host)))


;;;; Recursive Compilation

;;; Compile a File that make cause recursive compiles

;;; The strategy is to start compiling the file.  If we we need to
;;; compile a subfile, then blow out out of the first compile and do the
;;; subcompile.  This strategy will work if the subcompiles are
;;; triggered at the start of a load.

;;; A different strategy would postpone the subcompiles until after the
;;; first compilation finishes; the sources are just loaded until then.
;;; When the first compilation finishes, the subs are compiled and
;;; loaded.

;;; The depth-first list of files that need to be compiled.
;;;
(defvar *module-compile-subs* NIL)

(defun module-compile-file (path)
  (cond ((null *module-compile-subs*)		; First File to Compile?
	 ;; Bind a new list of the files so an abort will leave an empty list.
	 (let ((*module-compile-subs* (list path)))

	   (do ()				; until we run out of compiles
	       ((null *module-compile-subs*))
	     (catch 'module-compile-file
	       (format *error-output* "~&;;; Module Compile File(~d):  ~s~%"
		       (length *module-compile-subs*)
		       (car    *module-compile-subs*))
	       (compile-file (car *module-compile-subs*))

	       ;; if the compile-file returned, then we are done with this path
	       (pop *module-compile-subs*)))))

	(T					; No.  Recursive Compilation
	 (push path *module-compile-subs*)	;   remember a new file to compile
	 (format *error-output* "~&;;; Module Compile File:  abort for subcompile~%")
	 (throw 'module-compile-file NIL))))

(defun module-compile (module-name)
  (let ((path (module-pathname-get module-name)))
    (if (pathnamep path)
	(module-compile-file path)
	(error "No pathname for module ~A" module-name))))


;;;; Load, Require, and Provide

#+LUCID
(if (not (member "lisp" *load-source-pathname-types* :test #'equal))
    (setq *load-source-pathname-types*
	  (append *load-source-pathname-types* '("lisp"))))

;;; This routine doesn't recompile the binary if it finds out that one
;;; of the included files needs recompilation.  For example, the
;;; included file might change some macros.
;;;
;;; There could be a date that is passed in; if any loaded file
;;; is more recent than that date, then abort the load, recompile
;;; the binary, and reload it.

;;; if-source-only    :load-source, :query (if compile-file exists), :compile
;;;    *load-if-source-only*
;;; if-source-newer   :query, :load-source, :load-binary, :compile
;;;    *load-if-source-newer*
;;;
(defvar *module-load-if-source-only*  :compile)
(defvar *module-load-if-source-newer* :compile)

(defun module-load (name filename)
  (declare (ignore name))
  (let ((path (pathname filename)))
    (if (or (null (pathname-type path))		; blank file type?
	    (eql  (pathname-type path) :unspecific))
	(let* ((src  (make-pathname :defaults path
				    :type #+LISPM "LISP" #-LISPM "lisp"))
	       (bin  (make-pathname :defaults path
				    :type
				    #+LISPM "BIN"
				    #+LUCID (car *load-binary-pathname-types*)
				    #-(or LISPM LUCID) (error "module-load: no bin type")))
	       (psrc (probe-file src))
	       (pbin (probe-file bin)))

	  (cond ((null psrc)			; no source file, so pray there is something
		 (load path))

		((null pbin)			; no binary
		 (ecase *module-load-if-source-only*
		   (:query
		     (if (and (fboundp 'compile-file)
			      (y-or-n-p "module-load:  compile file ~a?" src))
			 (progn (module-compile-file src)
				(load                path))
			 (load src)))
		   (:compile
		     (module-compile-file src )
		     (load                path))
		   (:load-source
		     (load                src ))))

		((> (file-write-date src)
		    (file-write-date bin))
		 (ecase *module-load-if-source-newer*
		   (:query
		     ;; option to compile-and-load, load-binary, load-source
		     (when (and (fboundp 'compile-file)
				(y-or-n-p "module-load:  compile file ~a?" src))
		       (module-compile-file src))
		     (load bin))
		   (:compile
		     (module-compile-file src )
		     (load                path))
		   (:load-source
		     (load                src ))
		   (:load-binary
		     (load                bin ))))

		(t				; hmm.  bin must be good
		 (load bin))))
	(load path))))

(defun module-require (name &optional (paths))
  (unless (member name *modules* :test #'string-equal)
    ;; the module is not present
    (let ((paths (or paths (module-pathname-get name))))
      (unless (consp paths) (setq paths (list paths)))
      (dolist (path paths)
	(module-load name path)))))

(defun module-provide (name)
  ;; we could remember a name-pathname association here
  (provide name))


;;;; The Database

;;; should acquire this data from a few canonical places

(module-namestring "ACCT-BOM"              "R2" "/homes/glr/acct/bom")
(module-namestring "ACCT-INVENTORY"        "R2" "/homes/glr/acct/inv")
(module-namestring "ACCT-MRP"              "R2" "/homes/glr/acct/mrp")
(module-namestring "ACCT-FILE"             "R2" "/homes/glr/acct/file")
(module-namestring "ACCT-GL"               "R2" "/homes/glr/acct/gl/gl000")
(module-namestring "ACCT-GL010"            "R2" "/homes/glr/acct/gl/gl010")
(module-namestring "ACCT-GL030"            "R2" "/homes/glr/acct/gl/gl030")
(module-namestring "ACCT-GL040"            "R2" "/homes/glr/acct/gl/gl040")
(module-namestring "ACCT-GL050"            "R2" "/homes/glr/acct/gl/gl050")
(module-namestring "ACCT-GL070"            "R2" "/homes/glr/acct/gl/gl070")
(module-namestring "ACCT-GL080"            "R2" "/homes/glr/acct/gl/gl080")
(module-namestring "ACCT-SUBS1"            "R2" "/homes/glr/acct/subs1")

(module-namestring "ANAGRAM"               "R2" "/homes/glr/language/anagram")

(module-namestring "BFILE"                 "R2" "/homes/glr/sort/btree/bfile")
(module-namestring "BTEST"                 "R2" "/homes/glr/sort/btree/btest")
(module-namestring "BTREE"                 "R2" "/homes/glr/sort/btree/btree")

(module-namestring "CARDS"                 "R2" "/homes/glr/funct/sym/cards")
(module-namestring "CCITT-COMPRESSION"     "R2" "/homes/glr/net/fax/faxcomp")
(module-namestring "CLX"                   "R2" "/homes/glr/pstrm/clx/clx")
(module-namestring "CLXAPP"                "R2" "/homes/glr/pstrm/clx/clxapp")
(module-namestring "CLXLIB"                "R2" "/homes/glr/pstrm/clx/clxlib")
(module-namestring "CLX-CODE"              "R2" "/homes/glr/pstrm/clx/clxcode")
(module-namestring "COAXIAL-CABLE"         "R2" "/homes/glr/design/rf/coax")
(module-namestring "CODES-RSA"             "R2" "/homes/glr/codes/rsa")
(module-namestring "COMBINATORICS"         "R2" "/homes/glr/funct/number/combin")
(module-namestring "COMPRESSION-CCITT"     "R2" "/homes/glr/net/fax/faxcomp")
(module-namestring "COMPRESSION-LZW"       "R2" "/homes/glr/codes/lzw")
(module-namestring "COMP-VARACTOR"         "R2" "/homes/glr/design/rf/varactor")
(module-namestring "CRC"                   "R2" "/homes/glr/codes/crc")

(module-namestring "DB2"                   "R2" "/homes/glr/sort/db2/db2")
(module-namestring "DB2-SAVE"              "R2" "/homes/glr/sort/db2/db2sav")
(module-namestring "DB2-OPERATIONS"        "R2" "/homes/glr/sort/db2/db2ops")
(module-namestring "DB2-LOG"               "R2" "/homes/glr/sort/db2/db2log")
(module-namestring "DB2-DISPLAY"           "R2" "/homes/glr/sort/db2/db2dis")
(module-namestring "DB2-DEFS"              "R2" "/homes/glr/sort/db2/db2def")
(module-namestring "DB2-CHECK"             "R2" "/homes/glr/sort/db2/db2chk")
(module-namestring "DB2-BUFFER"            "R2" "/homes/glr/sort/db2/db2buf")
(module-namestring "DB2-RECOVER"           "R2" "/homes/glr/sort/db2/recover")

(module-namestring "DES"                   "R2" "/homes/glr/codes/des")
(module-namestring "DES-FILE"              "R2" "/homes/glr/codes/desfile")
(module-namestring "DICTIONARY-WB7"        "R2" "/homes/glr/language/wb7/dict")
(module-namestring "DIMENSIONAL-ANALYSIS"  "R2" "/homes/glr/funct/sym/dimen")
(module-namestring "DRAW-3D"               "R2" "/homes/glr/pstrm/3d/draw3d")
(module-namestring "DRAW-STIPPLES"         "R2" "/homes/glr/pstrm/raster/stip")
(module-namestring "DUMPER"                "R2" "/homes/glr/sys/tops20/dumper/dumber")

(module-namestring "FAX"                   "R2" "/homes/glr/net/fax/fax")
(module-namestring "FAX-COMPRESSION"       "R2" "/homes/glr/net/fax/faxcomp")
(module-namestring "FAX-TEST"              "R2" "/homes/glr/net/fax/faxtest")
(module-namestring "FILE-FORMAT-IDENTIFY"  "R2" "/homes/glr/lisp/io/formatid")
(module-namestring "FILTER"                "R2" "/homes/glr/design/filter/filter")
(module-namestring "FILTER-AF100"          "R2" "/homes/glr/design/filter/af100")
(module-namestring "FILTER-BIQUAD"         "R2" "/homes/glr/design/filter/biquad")
(module-namestring "FILTER-BIQUAD1"        "R2" "/homes/glr/design/filter/biquad1")
(module-namestring "FILTER-CATALOG"        "R2" "/homes/glr/design/filter/catalog")
(module-namestring "FILTER-CIRCUITS"       "R2" "/homes/glr/design/filter/filckt")
(module-namestring "FILTER-COMPONENTS"     "R2" "/homes/glr/design/filter/filcmp")
(module-namestring "FILTER-DEFS"           "R2" "/homes/glr/design/filter/fdefs")
(module-namestring "FILTER-DIGITAL"        "R2" "/homes/glr/design/filter/digital")
(module-namestring "FILTER-PLOT"           "R2" "/homes/glr/design/filter/fplot")
(module-namestring "FILTER-SALLEN"         "R2" "/homes/glr/design/filter/sallen")
(module-namestring "FILTER-TRANSFORM"      "R2" "/homes/glr/design/filter/fxform")
(module-namestring "FILTER-TRANSIENT"      "R2" "/homes/glr/design/filter/transient")
(module-namestring "FILTER-WIZARD"         "R2" "/homes/glr/papers/phd/wizard/wizfil")
(module-namestring "FILTER-ZEROFINDER"     "R2" "/homes/glr/design/filter/zfind")
(module-namestring "FIX-TRUENAME"          "R2" "/homes/glr/lisp/unix/truname")

(module-namestring "GPPP"                  "R2" "/homes/glr/design/gppp/gppp")
(module-namestring "GPPP-CUTTING"          "R2" "/homes/glr/design/gppp/cutting")
(module-namestring "GPPP-DRILLING"         "R2" "/homes/glr/design/gppp/drill")
(module-namestring "GPPP-MATERIALS"        "R2" "/homes/glr/design/gppp/material")
(module-namestring "GPPP-MACHINES"         "R2" "/homes/glr/design/gppp/machines")
(module-namestring "GPPP-OPERATIONS"       "R2" "/homes/glr/design/gppp/operation")

(module-namestring "HIDSUR"                "R2" "/homes/glr/pstrm/3d/hidsur")
(module-namestring "HIDSUR-BONES"          "R2" "/homes/glr/pstrm/3d/bones")

(module-namestring "IGES"                  "R2" "/homes/glr/design/iges/iges")
(module-namestring "IGES-CONVERT"          "R2" "/homes/glr/design/iges/convert")
(module-namestring "IGES-PARAMETERS"       "R2" "/homes/glr/design/iges/param")
(module-namestring "IMAGE-UTILITIES"       "R2" "/homes/glr/pstrm/imageutil")
(module-namestring "INFIX"                 "R2" "/homes/glr/lisp/infix")
(module-namestring "INFIX-W"               "R2" "/homes/glr/lisp/infixw")
(module-namestring "INDUCTANCE"            "R2" "/homes/glr/design/rf/induct")
(module-namestring "INDUCTANCE-FERRITE"    "R2" "/homes/glr/design/filter/ferrit")

(module-namestring "LIFER"                 "R2" "/homes/glr/language/lifer")

(module-namestring "MATRIX"                "R2" "/homes/glr/funct/matrix")
(module-namestring "MATRIX-LINEQ"          "R2" "/homes/glr/funct/lineq")
(module-namestring "MODULAR-ARITHMETIC"    "R2" "/homes/glr/funct/number/mod")

(module-namestring "NC-GERBER-TO-PS"       "R2" "/homes/glr/design/pc/gerberps")
(module-namestring "NC-UTILITIES"          "R2" "/homes/glr/design/mech/ncutil")

(module-namestring "NICAD"                 "R2" "/homes/glr/design/nicad/src/nicad")
(module-namestring "NICAD-CMDDEF"          "R2" "/homes/glr/design/nicad/src/cmddef")
(module-namestring "NICAD-COMMANDS"        "R2" "/homes/glr/design/nicad/src/commands")
(module-namestring "NICAD-COMPILE"         "R2" "/homes/glr/design/nicad/src/compile")
(module-namestring "NICAD-DISPLAY"         "R2" "/homes/glr/design/nicad/src/display")
(module-namestring "NICAD-DXFIN"           "R2" "/homes/glr/design/nicad/src/dxfin")
(module-namestring "NICAD-DXFOUT"          "R2" "/homes/glr/design/nicad/src/dxfout")
(module-namestring "NICAD-ENTITIES"        "R2" "/homes/glr/design/nicad/src/entities")
(module-namestring "NICAD-GEOMETRY"        "R2" "/homes/glr/design/nicad/src/geometry")
(module-namestring "NICAD-GETFCNS"         "R2" "/homes/glr/design/nicad/src/getfcns")
(module-namestring "NICAD-IGES"            "R2" "/homes/glr/design/nicad/src/igesio")
(module-namestring "NICAD-OBJSNAP"         "R2" "/homes/glr/design/nicad/src/objsnap")
(module-namestring "NICAD-SELECT"          "R2" "/homes/glr/design/nicad/src/select")
(module-namestring "NICAD-SHAPES"          "R2" "/homes/glr/design/nicad/src/shapes")
(module-namestring "NICAD-SUBROUTINES"     "R2" "/homes/glr/design/nicad/src/subroutines")
(module-namestring "NICAD-TABLES"          "R2" "/homes/glr/design/nicad/src/tables")
(module-namestring "NICAD-VARIABLES"       "R2" "/homes/glr/design/nicad/src/variables")

(module-namestring "OPTIMIZE-GOLDEN"       "R2" "/homes/glr/funct/optim/golden")
(module-namestring "OPTIMIZE-LINEAR-FIT"   "R2" "/homes/glr/funct/optim/fit")
(module-namestring "OPTIMIZE-MARQUARDT"    "R2" "/homes/glr/funct/optim/marq")
(module-namestring "OPTIMIZE-REGRESSION"   "R2" "/homes/glr/funct/optim/regres")
(module-namestring "OPTIMIZE-REMES"        "R2" "/homes/glr/funct/optim/remes")

(module-namestring "PACKBITS"              "R2" "/homes/glr/codes/packbits")
(module-namestring "PERMUTATIONS"          "R2" "/homes/glr/funct/number/permute")
(module-namestring "PICT"                  "R2" "/homes/glr/pstrm/pict/pict")
(module-namestring "PLOT"                  "R2" "/homes/glr/pstrm/plot/plot")
(module-namestring "PLOT-CONTOUR"          "R2" "/homes/glr/pstrm/plot/cplot")
(module-namestring "PLOT-FLOATING-HORIZON" "R2" "/homes/glr/pstrm/plot/floating")
(module-namestring "POSTSCRIPT-AFM"        "R2" "/homes/glr/pstrm/postscript/afm")
(module-namestring "POSTSCRIPT-BBSIZE"     "R2" "/homes/glr/pstrm/postscript/bbsize")
(module-namestring "POSTSCRIPT-UTILITIES"  "R2" "/homes/glr/pstrm/postscript/psutil")
(module-namestring "PSTRM"                 "R2" "/homes/glr/pstrm/pstrm")
(module-namestring "PSTRM-DEFS"            "R2" "/homes/glr/pstrm/psdefs")
(module-namestring "PSTRM-POSTSCRIPT"      "R2" "/homes/glr/pstrm/postscript/pspost")
(module-namestring "PSTRM-PTTY"            "R2" "/homes/glr/pstrm/ptty")
(module-namestring "PSTRM-SUBS"            "R2" "/homes/glr/pstrm/pssubs")
(module-namestring "PSTRM-SUPDUP"          "R2" "/homes/glr/pstrm/supgrf")
(module-namestring "PSTRM-TEKTRONIX"       "R2" "/homes/glr/pstrm/tk4014")
(module-namestring "PSTRM-TV"              "R2" "/homes/glr/pstrm/tv")

(module-namestring "QUADTREE"              "R2" "/homes/glr/sort/quadtree")

(module-namestring "RANDOM-SELECTION"      "R2" "/homes/glr/funct/statis/ransel")
(module-namestring "RECORD"                "R2" "/homes/glr/lisp/record")
(module-namestring "REGULAR-EXPRESSIONS"   "R2" "/homes/glr/funct/sym/regexp")
(module-namestring "RF"                    "R2" "/homes/glr/design/rf/rf")
(module-namestring "RF-LAN"                "R2" "/homes/glr/design/rf/lan")

(module-namestring "SCANNER-SUBS"          "R2" "/homes/glr/pstrm/scanner/scansubs")
(module-namestring "SPECTRUM"              "R2" "/homes/glr/design/signal/spectrum")
(module-namestring "SPRINGS"               "R2" "/homes/glr/design/mech/springs")
(module-namestring "STATISTICS"            "R2" "/homes/glr/funct/statis/statis")
(module-namestring "STATISTICS-BINOMIAL"   "R2" "/homes/glr/funct/statis/binomial")
(module-namestring "SUN-RASTER"            "R2" "/homes/glr/pstrm/scanner/sunrast")
(module-namestring "SYMTAB"                "R2" "/homes/glr/lisp/comp/symtab")

(module-namestring "TAPE"                  "R2" "/homes/glr/lisp/tapeio/tape")
(module-namestring "TAPE-ANSI"             "R2" "/homes/glr/lisp/tapeio/ansi")
(module-namestring "TAPE-ANSI-TEST"        "R2" "/homes/glr/lisp/tapeio/ansitest")
(module-namestring "TAPE-LISPM"            "R2" "/homes/glr/lisp/tapeio/tapel")
(module-namestring "TAPE-TEST"             "R2" "/homes/glr/lisp/tapeio/tapetest")
(module-namestring "TAPE-UNIX"             "R2" "/homes/glr/lisp/tapeio/tapeu")
(module-namestring "TERMCAP"               "R2" "/homes/glr/lisp/io/termcap")

(module-namestring "TEX-DVI-DEFS"          "R2" "/homes/glr/tex/tools/dvidefs")
(module-namestring "TEX-DVI-TYPE"          "R2" "/homes/glr/tex/tools/dvitype")
(module-namestring "TEX-DVI2PS"            "R2" "/homes/glr/tex/tools/dvi2ps")
(module-namestring "TEX-GF-READ"           "R2" "/homes/glr/tex/tools/gfread")
(module-namestring "TEX-TFM-READ"          "R2" "/homes/glr/tex/tools/tfmread")

(module-namestring "TIFF"                  "R2" "/homes/glr/pstrm/tiff/tiff")

(module-namestring "UNIX-IO"               "R2" "/homes/glr/lisp/io/unixio")
(module-namestring "UNIX-GETPWENT"         "R2" "/homes/glr/lisp/unix/getpwent")

(module-namestring "VMEM"                  "R2" "/homes/glr/sort/btree/vmem")

(module-namestring "WIRE-TABLE"            "R2" "/homes/glr/design/rf/wire")

(module-namestring "XTIFF"                 "R2" "/homes/glr/pstrm/tiff/xtiff")

(module-namestring "ZEROS-BAIRSTOW"        "R2" "/homes/glr/funct/zeros/bairst")
(module-namestring "ZEROS-BISECTION"       "R2" "/homes/glr/funct/zeros/bisection")
(module-namestring "ZEROS-BRENT"           "R2" "/homes/glr/funct/zeros/brent")
(module-namestring "ZEROS-FORMULA"         "R2" "/homes/glr/funct/zeros/proots")
(module-namestring "ZEROS-NEWTON"          "R2" "/homes/glr/funct/zeros/newton")

(module-namestring "INTERVAL-ARITHMETIC"   "R2" "/homes/glr/funct/interval")
(module-namestring "POLYNOMIALS"           "R2" "/homes/glr/funct/polyn")
(module-namestring "HORNER"                "R2" "/homes/glr/funct/horner")
(module-namestring "ERROR-FUNCTION"        "R2" "/homes/glr/funct/functions/erf")
(module-namestring "GAMMA-FUNCTION"        "R2" "/homes/glr/funct/functions/gamma")
(module-namestring "BETA-FUNCTION"         "R2" "/homes/glr/funct/functions/beta")
(module-namestring "BESSEL-FUNCTIONS"      "R2" "/homes/glr/funct/functions/bessel")
(module-namestring "ELLIPTIC-FUNCTIONS"    "R2" "/homes/glr/funct/functions/ellip")

;;; not yet fixed...

(module-namestring "FIFO"                  "R2" "/homes/glr/sort/fifo")
