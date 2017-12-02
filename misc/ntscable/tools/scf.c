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

#include <stdio.h>
#include <ctype.h>

#include "tf.h"
#include "jwtools.h"
#include "scf.h"

#define	CNULL	(char **) NULL

static char  ** Nargv = CNULL,
		routine_name[] = "standard_command_format";

/*-------------------------------------------------------------------*/
int standard_command_format( valid_options, 
			     valid_arg_options,
			     argc_ptr,
			     argv_ptr)

	/* Translates command line arguments in the Standard Command
	Format into a form that is easily manipulated by C programs.
	See chaper 5 of: R Thomas, L Rogers, J Yates (1986) Advanced 
	programmer's guide to Unix system V. Osborne McGraw-Hill: 
	Berkeley.  Unlike the version in that book, this routine 
	returns TRUE if error, FALSE otherwise.  Another difference
	is that this routine does not require a space between an
	option and its argument. */

    char      * valid_options,
	      * valid_arg_options,
	     ** argv_ptr[];
    int	      * argc_ptr;



{
				       /*----- functions called -----*/

				       /*----- extern variables -----*/
    extern char * strchr(),
		* malloc(),
		* realloc();
				       /*----- local  variables -----*/

    char	c,
	     ** nargv = CNULL,
	     ** files = CNULL;

    unsigned int num_args = 0,
		 num_files = 0;

    int		error = FALSE,
		arg_i,
		char_i,
		i;
				       /*----- start initialize -----*/

    if (allocate_bytes( (char **) &nargv, 
			sizeof(char *), 
			"1st nargv in scf"))
	return TRUE;

    if (check_args( valid_options, valid_arg_options, (*argv_ptr)[0])) {
	free_memory((char **) &nargv);
	return TRUE;
    }

    nargv[num_args++] = (*argv_ptr)[0];

    for( arg_i=1; 

	 arg_i < *argc_ptr 
	 && 
	 strcmp((*argv_ptr)[arg_i], OPTEND);

	 arg_i++ ) {

	if ( (*argv_ptr)[arg_i][0] == DASH 
	     && 
	     (*argv_ptr)[arg_i][1]) {            /* process 1 option
	     					 or group of options*/

	    for ( char_i=1; 
		  char_i < strlen((*argv_ptr)[arg_i]); 
		  char_i++ ) {

		c = (*argv_ptr)[arg_i][char_i];

		if (!(int) strchr(valid_options,c)) {
		    fprintf(stderr, "illegal option %c\n", c);
	            free_memory((char **) &nargv);
	            free_memory((char **) &files);
	            return TRUE;
		}

    		if ( reallocate( (char **)&nargv,
		         	 (num_args + 1)*sizeof(char *),
		         	 "nargv in scf" )
		     ||
		     allocate_bytes( nargv + num_args,
				     3*sizeof(char),
				     "option in scf" )
		     ) {

		    free_memory((char **) &nargv);
		    free_memory((char **) &files);
		    return TRUE;
    		}

		nargv[num_args][0] = DASH;
		nargv[num_args][1] = c;
		nargv[num_args++][2] = NULL;

		if (strchr(valid_arg_options,c)) {

    		    if ( reallocate( (char **)&nargv,
		         	     (num_args + 1)*sizeof(char *),
		         	     "nargv in scf" )) {

		        free_memory((char **) &nargv);
		        free_memory((char **) &files);
		        return TRUE;
    		    }

		    if ((*argv_ptr)[arg_i][++char_i]) {
		        nargv[num_args++] = (*argv_ptr)[arg_i] 
					    + char_i;
		    }
		    else if (arg_i+1 >= *argc_ptr) {
		        fprintf(stderr, 
		        "no argument for option %c\n", c);
	                free_memory((char **) &nargv);
	                free_memory((char **) &files);
	                return TRUE;
		    }
		    else {
		        nargv[num_args++] = (*argv_ptr)[++arg_i];
		    }
		    char_i = strlen((*argv_ptr)[arg_i]);

		}
	    }
	}
	else {      		               /* process 1 filename */

    	    if (reallocate( (char **)&files,
			    (num_files+1)*sizeof(char *),
			    "files in scf" )) {
	        free_memory((char **) &nargv);
	        free_memory((char **) &files);
	        return TRUE;
	    }

	    files[num_files++] = (*argv_ptr)[arg_i];
	}
    }

    if (arg_i < *argc_ptr)
	arg_i++;
    
    if (reallocate( 
	(char **)&nargv,
	(num_args + num_files - arg_i + *argc_ptr + 2)*sizeof(char *),
	"final nargv in scf" )) {

	free_memory((char **) &nargv);
	free_memory((char **) &files);
	return TRUE;

    }


    nargv[num_args++] = OPTEND; 

    for (i=0; i<num_files; i++) 
        nargv[num_args++] = files[i]; 

    for (; arg_i<*argc_ptr; arg_i++) 
        nargv[num_args++] = (*argv_ptr)[arg_i]; 

    nargv[num_args] = (char *) NULL;



    *argv_ptr = Nargv = nargv;

    *argc_ptr = num_args;

    free_memory((char **) &files);

    return FALSE;
}




/*-------------------------------------------------------------------*/
void free_command_line_options()

{
    free_memory((char **) &Nargv);
}

/*-------------------------------------------------------------------*/
static int check_args( valid_options, valid_arg_options, program_name)

	/* Checks lists of supposedly valid options for proper syntax 
	and internal consistency.  Returns TRUE if error; FALSE 
	otherwise. */

    char      * valid_options,
	      * valid_arg_options,
	      * program_name;

{
    register	char * ptr;
				       /*----- start function -------*/

    for (ptr = valid_options; *ptr; ptr++) {
	if (!isalpha(*ptr)) {
	    fprintf(stderr,
	    "%s: program error: option char '%c' not a letter\n", 
	    program_name, *ptr);
	    return TRUE;
	}
    }

    for (ptr = valid_arg_options; *ptr; ptr++) {
	if (!strchr(valid_options, *ptr)) {
	    fprintf(stderr,
	    "%s: program error: option arg '%c' not a legal option\n",
	    program_name, *ptr);
	    return TRUE;
	}
    }

    return FALSE;
}
