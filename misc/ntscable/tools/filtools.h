	/*-------------------------------------------------------------
	|			
	|	MODULE:   %W%
	|				
	|	MACHINE:  UNIX                 
	|					
	|	STARTED:  25-SEP-89  BY:  J.C. Wathey 
	|			
	|	REVISED:  %G%   BY:  JCW
	|				
	|	STATUS:      incomplete or untested		
	|                    compiles; partly tested	
	|                 -> runs; revisions in progress
	|                    runs; stable version
	|					
	|       CONTAINS: macros, definitions and declarations for
	|		  file i/o tools
	|                                     
	-------------------------------------------------------------*/

#include <sys/file.h>

#define file_exists(path)         ((path) && *(path) && !access(path, F_OK))
#define file_is_read_write(path)  ((path) && *(path) && !access(path, R_OK | W_OK))
#define file_is_readable(path)    ((path) && *(path) && !access(path, R_OK))
#define file_is_writable(path)    ((path) && *(path) && !access(path, W_OK))
#define file_is_executable(path)  ((path) && *(path) && !access(path, X_OK))

		/* Flag values for the addressing mode argument to
		fseek(3). */

#define	ABSOLUTE	0
#define RELATIVE	1
#define FROM_END	2

int	view_file(),
    	is_directory(),
	d_get_new_filename(),
	get_pathname(),
    	cd_or_ls();
char  * change_extension(),
      * extension_number(),
      * current_working_dir();
void 	purge_leading_space();
time_t 	mod_time();
