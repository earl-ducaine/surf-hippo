	/*-------------------------------------------------------------
	|							      |
	|	MODULE:	  %W%
	|							      |
	|	MACHINE:  Sun 3/60         		      |
	|							      |
	|	STARTED:  21-SEP-90	   BY:	J.C. Wathey	      |
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
#include <sys/types.h>
#include <sys/stat.h>
#include "tf.h"
#include "filtools.h"



/*-------------------------------------------------------------------*/
int view_file(filename)

	/* Displays contents of a file using the 'more' command.
	Returns TRUE if error; FALSE otherwise. */

    char * filename;

{
				       /*----- local  variables -----*/
    int 	error;
    char	command[BUFSIZ];

				       /*----- start function -------*/
    sprintf(command, "more %s", filename);
    error = system(command);
    return(error);
}

/*-------------------------------------------------------------------*/
char * change_extension(filename, new_extension)

	/* On entry: filename points to a file name which may be
	preceded by a path.  On exit: the original extension is 
	replaced by new_extension, or, if there was no extension in
	filename, new_extension is appended to filename.  If 
	new_extension is NULL or empty on entry, any existing 
	extension in filename is simply deleted.  Does nothing if 
	filename is NULL or empty on entry.  Returns a pointer to 
	filename.  */

    char      * filename;
    char      * new_extension;

{
				       /*----- functions called -----*/
    char      * strrchr();

				       /*----- extern variables -----*/

				       /*----- local  variables -----*/
    char      * fn_ptr,
	      * ext_ptr;
				       /*----- start function -------*/

    if (filename && *filename) {

        if (fn_ptr = strrchr(filename,'/'))
	    fn_ptr++;
        else
	    fn_ptr = filename;

        if ( (ext_ptr = strrchr(fn_ptr,'.'))
	     &&
	     (ext_ptr != fn_ptr) )
	    *ext_ptr = '\0';

	if (new_extension && *new_extension) {
            strcat(filename, ".");
            strcat(filename, new_extension);
	}
    }

    return(filename);
}

/*-------------------------------------------------------------------*/
char * extension_number(n)

	/* Returns a 3 digit string representation of n, for use as a
	file extension, or 0 if n is out of range. */

    int		n;

{
				       /*----- functions called -----*/
				       /*----- extern variables -----*/
				       /*----- local  variables -----*/
    static char extension[4];
				       /*----- start function -------*/

    if (n < 0 || n > 999) 
	n = 0;
    sprintf(extension,"%03d",n);
    return(extension);
}


/*-------------------------------------------------------------------*/
char * current_working_dir()

        /* Returns a pointer to the name of the current working 
        directory. */

{
				       /*----- functions called -----*/
    char      * getwd();
				       /*----- local  variables -----*/
    char      * current_working_ptr;
    static char	path[BUFSIZ]; 
				       /*----- start function -------*/

    if ( !(current_working_ptr = getwd(path))) {
	fprintf(stderr, "%s\n", path);
    }

    return(path);
}				       /*----- end function ---------*/

/*-------------------------------------------------------------------*/
int is_directory(path)

	/* Returns TRUE if path specifies an existing directory; else 
	returns FALSE. */

    char      * path;

{
				       /*----- local  variables -----*/
    struct stat	file_status;
				       /*----- start function -------*/

    return( path
	    &&
	    *path
	    &&
	    !stat(path, &file_status) 
	    &&
            (file_status.st_mode & S_IFDIR) );
}



/*-------------------------------------------------------------------*/
void purge_leading_space( str )

	/* Purges leading spaces and tabs from the argument.  The 
        quotes below contain 0x20 0x09 (one space and one tab). */

    char      * str;

{
    int num_leading_spaces;

    if (str && *str && (num_leading_spaces=strspn(str," 	")))
	strcpy(str, str + num_leading_spaces);
}


/*-------------------------------------------------------------------*/
int cd_or_ls(possible_command)

	/* Returns TRUE and executes possible_command as a shell 
	command (via system(3)) if possible_command is one of a few 
	recognized commands; otherwise does nothing and returns 
	FALSE. */

    char * possible_command;

{
				       /*----- functions called -----*/
    void	purge_leading_space();
    char      * current_working_dir();
    char      * getenv();

				       /*----- extern variables -----*/

				       /*----- local  variables -----*/
    int is_command;
    char command[BUFSIZ];

				       /*----- start function -------*/
		       /* check for and execute "ls" or "cd" command */

    if (is_command =( !strcmp(possible_command,"ls") 
		      || 
		      !strncmp(possible_command,"ls ",3))) {
	fflush(stdout);
	system(possible_command);
    }
    else if (is_command =( !strcmp(possible_command,"cd"))) {
	chdir(getenv("HOME"));
	printf("%s\n", current_working_dir());
    }
    else if (is_command =( !strncmp(possible_command,"cd ",3))) {
    	purge_leading_space(possible_command + 3),
	chdir(possible_command + 3);
	printf("%s\n", current_working_dir());
    }

    return(is_command);

}

/*-------------------------------------------------------------------*/
time_t mod_time( filename )

	/* Returns the time (as long int) at which the file specified
	by filename was last modified, if it can be obtained; otherwise
	returns 0. */

    char * filename;

{
				       /*----- functions called -----*/

				       /*----- extern variables -----*/

				       /*----- local  variables -----*/
    struct stat stat_info;

				       /*----- start function -------*/
    if (stat(filename, &stat_info)) {
	fprintf( stderr,
	"mod_time: can't stat %s\n", filename);
	return(0);
    }
    return(stat_info.st_mtime);
}

/*-------------------------------------------------------------------*/
int d_get_new_filename(filename_ptr)

	/* Prompts user for a filename and puts it into dynamic memory 
	at the address returned in *filename_ptr.  The user may also 
	execute cd or ls commands at the prompt.  If no filename was 
	entered and *filename_ptr is nonempty on entry, the routine 
	does nothing.  If the entered filename is an existing file, 
	the routine issues a warning message to that effect and gives
	the user another chance to change the name.  If the user enters
	the name of an existing directory, the routine rejects this and
	prompts again.  Returns TRUE if error, else FALSE.  NOTE THAT
	THE ARGUMENT IS A DOUBLY INDIRECT POINTER! */

    char     ** filename_ptr;

{
				       /*----- functions called -----*/

    char * fgets(),
	 * strchr();
    void	purge_leading_space();
				       /*----- extern variables -----*/

				       /*----- local  variables -----*/
    char 	text[BUFSIZ];
    char      * newline_ptr;
    int 	user_entered_bad_name,
		error = FALSE;
				       /*----- start function -------*/

    do {

        printf("\nEnter filename");

        if (*filename_ptr && **filename_ptr)
	    printf(" (%s)", *filename_ptr);

        printf(": ");

	fflush(stdout);

        if (error = ((fgets(text, BUFSIZ, stdin) == (char *) NULL)
	   	     ||
	  	     fseek(stdin, 0L, 2)) ){
	    perror("stdin");
	    return(error);
        }

        purge_leading_space( text );

        if (newline_ptr = strchr(text, '\n'))
	    *newline_ptr = '\0';

	if (user_entered_bad_name = is_directory(text)) {
	    fprintf(stderr,
	    "error: %s is an existing directory\n", text);
	}
	else if (file_exists(text)) {
	    printf("%s exists.  Overwrite", text);
	    user_entered_bad_name = !user_says_yes(TRUE);
	}

    } while (text[0] 
	     &&
	     ( user_entered_bad_name
	       ||
	       cd_or_ls(text) ));

    if (text[0]) {
	if (*filename_ptr)
	    free(*filename_ptr);
        error = strsave(filename_ptr, text);
    }

    return(error);
}



/*-------------------------------------------------------------------*/
int get_pathname(path, default_path, prompt_string)

	/* Gets a pathname from the user and copies it to path.  The 
        user is prompted with prompt_string followed by " filename:".  
        If default_path is not empty on entry, then its contents appear 
        in parentheses in the prompt, before the colon; pressing only 
        RETURN causes default_path to be assigned to path.  If no 
        default is present, pressing only RETURN causes routine to 
        return TRUE, indicating that the user has given up trying to 
        enter a pathname.  Prompts repeatedly if the file does not 
	exist.  The user may enter 'ls' or 'cd' commands at the prompt,
	in which case he or she will be prompted again after the command
	has been executed.  Returns FALSE if a pathname has been 
        assigned to path.  */

    char      * path,
	      * default_path,
	      * prompt_string;

{
				       /*----- functions called -----*/
    void	purge_leading_space();
    char * fgets(),
	 * strchr();
				       /*----- extern variables -----*/

				       /*----- local  variables -----*/
    int 	error;
    char	temp_path[BUFSIZ],
	      * newline_ptr;
				       /*----- start function -------*/

    do {

        printf("%s filename", prompt_string);

        if (*default_path)
	    printf(" (%s)", default_path);

        printf(": ");

        if (error = ((fgets(temp_path, BUFSIZ, stdin) == (char *) NULL)
	   	     ||
	  	     fseek(stdin, 0L, 2)) ){
	    perror("stdin");
	    return(error);
        }

        purge_leading_space( temp_path );

        if (newline_ptr = strchr(temp_path, '\n'))
	    *newline_ptr = '\0';

    } while (*temp_path &&
	     (cd_or_ls(temp_path) || !file_exists(temp_path) ));

    if (error = (!*temp_path && !*default_path))
	return(error);

    if (*temp_path)
	strcpy(path, temp_path);
    else
	strcpy(path, default_path);
	
    return(error);
}


