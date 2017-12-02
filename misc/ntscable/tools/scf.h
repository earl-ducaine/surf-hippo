	/*-------------------------------------------------------------
	|
	|	MODULE:   %M%
	|
	|	MACHINE:  any UNIX machine
	|
	|	STARTED:              BY:  R. Thomas et al.
	|
	|	REVISED:  %G%         BY:  JCW
	|
	|	STATUS:   -> incomplete or untested
	|                    compiles; partly tested
	|                    runs; revisions in progress
	|                    runs; stable version
	|
	|       CONTAINS: routine standard_command_format()
	|
	|	COMPILE:  (use makefile)
	|
	-------------------------------------------------------------*/


#define	DASH	'-'
#define OPTEND	"--"

void free_command_line_options();
