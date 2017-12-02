	/*-------------------------------------------------------------
	|							      |
	|	MODULE:	  %W%
	|							      |
	|	MACHINE:  Sun 3/60         		      |
	|							      |
	|	STARTED:  10-OCT-90	   BY:	J.C. Wathey	      |
	|							      |
	|	REVISED:  %G%	   BY:	JCW		      |
	|							      |
	|	STATUS:	     incomplete or untested		      |
	|		     compiles; partly tested		      |
	|		  -> runs; revisions in progress	      |
	|		     runs; stable version		      |
	|							      |
	|	CONTAINS: routines for file i/o
	|							      |
	-------------------------------------------------------------*/


/*--------------------------------- GLOBAL DEFINITIONS --------------*/
#include <stdio.h>
#include "tf.h"
#include "filtools.h"
#include "sccstools.h"


static char version_chars[]=".0123456789";

/*-------------------------------------------------------------------*/
char * sccs_what(pathname)

	/* Returns the sccs version (as a string) of the file 
	specified by pathname, or an empty string if the version
	number cannot be obtained.  The file must exist in the
	current directory.  The version number is obtained via 
	an equivalent of "sccs what", so the original sccs-protected 
	file must contain the sccs 'W' id keyword. */

   char * pathname;

{
				       /*----- functions called -----*/
    char * fgets(),
	 * strrchr(),
	 * strchr();

				       /*----- extern variables -----*/
				       /*----- local  variables -----*/
    static char version[50];

    char   command[BUFSIZ],
	   target[21],
	 * filename,
	 * slash_ptr,
	 * newline_ptr,
	 * version_ptr;
    FILE * version_pipe;
    int    target_length;
				       /*----- start function -------*/
    if (!file_exists(pathname))
	return("");

    if (slash_ptr = strrchr(pathname, '/'))
	filename = slash_ptr+1;
    else
	filename = pathname;

    sprintf(target, "@(#)%s", filename);
    target_length = strlen(target);
    sprintf( command, "grep '%s' %s", target, pathname);

    if( !(version_pipe = popen(command, "r"))) {
	fprintf(stderr,
	"sccs_what: can't open version_pipe\n");
	return("");
    }

    if (fgets(version, BUFSIZ, version_pipe) == (char *) NULL) {
        pclose(version_pipe);
        return("");
    }

    pclose(version_pipe);

    if (newline_ptr = strchr(version, '\n'))
        *newline_ptr = '\0';

    for (version_ptr = version;
	 *version_ptr
	 &&
	 strncmp(version_ptr, target, target_length);
	 version_ptr++ );

    if (!*version_ptr)
	return("");

    version_ptr += target_length + 1;
    /* version_ptr now points to the 1st char of version string */

    *(version_ptr + strspn(version_ptr, version_chars)) = '\0';

    return(version_ptr);

}

/*-------------------------------------------------------------------*/
static char * latest_sccs_version(filename)

	/* Returns the latest sccs version (as a string) of the file 
	specified by filename, or an empty string if the version
	number cannot be obtained. */

   char * filename;

{
				       /*----- functions called -----*/
    char * fgets(),
	 * strchr();

				       /*----- extern variables -----*/
    extern char version_chars[];

				       /*----- local  variables -----*/
    static char version[50];
    char   history_line[BUFSIZ],
	   history_filename[BUFSIZ],
	 * version_ptr;
    FILE * history_fp;
				       /*----- start function -------*/

					    /* open the history file */

    sprintf(history_filename, "SCCS/s.%s", filename);

    if( !(history_fp = fopen(history_filename, "r"))) {
	fprintf(stderr,
	"latest_sccs_version: can't open %s\n", history_filename);
	return("");
    }
    		     /* find the line with the latest version number */

    while ( fgets( history_line, BUFSIZ, history_fp)
	    &&
	    strncmp(history_line, "\001d D ", 5) );

    if (feof(history_fp) || ferror(history_fp)) {
	fprintf(stderr,
	"latest_sccs_version: error reading %s\n", history_filename);
	fclose(history_fp);
	return("");
    }

    if (fclose(history_fp)) {
	fprintf(stderr,
	"latest_sccs_version: error closing %s\n", history_filename);
    }
    				   /* find the latest version number */

    version_ptr = history_line + 5;
    *(version_ptr + strspn(version_ptr, version_chars)) = '\0';
    strcpy(version, version_ptr);

    return(version);

}


#ifdef WIPEOUT
/* old version; slower but less dependent on internal
details of sccs history file */
/*-------------------------------------------------------------------*/
static char * latest_sccs_version(filename)

	/* Returns the latest sccs version (as a string) of the file 
	specified by filename, or an empty string if the version
	number cannot be obtained. */

   char * filename;

{
				       /*----- functions called -----*/
    char * fgets(),
	 * strchr();

				       /*----- extern variables -----*/

				       /*----- local  variables -----*/
    static char version[50];
    char   command[BUFSIZ],
	 * newline_ptr;
    FILE * version_pipe;
				       /*----- start function -------*/
    sprintf( command,
    "sccs prs %s | grep '[0-9]\.[0-9]' | head -1 | cut -f2 -d\" \"",
    filename);

    if( !(version_pipe = popen(command, "r"))) {
	fprintf(stderr,
	"last_sccs_version: can't open version_pipe\n");
	return("");
    }

    if (fgets(version, BUFSIZ, version_pipe) == (char *) NULL) {
        perror("version_pipe");
        pclose(version_pipe);
        return("");
    }

    pclose(version_pipe);

    if (newline_ptr = strchr(version, '\n'))
        *newline_ptr = '\0';

    return(version);

}
#endif

