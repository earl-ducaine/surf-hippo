
	/*-------------------------------------------------------------
	|
	|	MODULE:	  %M%
	|
	|	MACHINE:  Sun 3/60
	|
	|	STARTED:  11-FEB-91	   BY:	J.C. Wathey
	|
	|	REVISED:  %G%	   BY:	JCW
	|
	|	STATUS:	     incomplete or untested		
	|		     compiles; partly tested
	|		  -> runs; revisions in progress
	|		     runs; stable version
	|
	|	CONTAINS: global definitions and declarations for 
	|		  statistics routines
	|
	-------------------------------------------------------------*/


/*--------------------------------- GLOBAL DEFINITIONS --------------*/


/*-------------------------------- GLOBAL DECLARATIONS --------------*/

void clear_statistics();
void add_variate_value();
int  delete_variate_value();
int  get_statistics();
int  get_statistics_from_list();
