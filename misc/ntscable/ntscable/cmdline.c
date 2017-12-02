	/* %W%  %G% */

	/*-------------------------------------------------------------
	|							      |
	|	PROGRAM:  ntscable                                    |
	|							      |
	|	MODULE:   %M%                                         |
	|							      |
	|	MACHINE:  Sun 3/60                                    |
	|							      |
	|	STARTED:  20-APR-89        BY:  J.C. Wathey           |
	|							      |
	|	REVISED:  %G%         BY:  JCW                   |
	|							      |
	|	STATUS:      incomplete or untested		      |
	|                    compiles; partly tested		      |
	|                 -> runs; revisions in progress	      |
	|                    runs; stable version		      |
	|							      |
	|       CONTAINS: routines for processing command line        |
	|                 arguments                                   |
	|                                                             |
	|       COMPILE:                                              |
	|                                                             |
	|       (use makefile)                                        |
	|                                                             |
	-------------------------------------------------------------*/

/*--------------------------------------- HEADER FILES --------------*/
#include "tf.h"
#include <stdio.h>
#include <ctype.h>
#include "jwmath.h"
#include "sccstools.h"
#include "scf.h"
#include "diagrams.h"
#include "ntscable.h"		       /* global defs & declarations */

#define arg_error(X)	show_usage(argv[0]);\
			fprintf(stderr,\
			"%s: invalid argument to -%c option\n",\
			argv[0],'X');\
			return(TRUE)

/*-------------------------------------------------------------------*/
int process_command_line( argc,
			  argv,
			  verifying_ptr,
			  nts_fp_ptr,
			  hoc_fp_ptr,
			  draw_input_diagram_ptr,
			  draw_output_diagram_ptr,
			  draw_in_fig_syntax_ptr,
			  draw_in_graph_syntax_ptr,
			  diagram_parameters_ptr,
			  neuron_ptr )

	/* Parses command line information (in argc and argv) and 
	initializes variables pointed to by the remaining arguments
	accordingly.  Returns TRUE if error occurs; FALSE otherwise. */

    int	       argc,
 	     * verifying_ptr,
	     * draw_input_diagram_ptr,
	     * draw_output_diagram_ptr,
	     * draw_in_fig_syntax_ptr,
	     * draw_in_graph_syntax_ptr;

    char     * argv[];

    FILE    ** nts_fp_ptr,
	    ** hoc_fp_ptr;

    DIAGRAM  * diagram_parameters_ptr;
    NEURON   * neuron_ptr;

{
				       /*----- functions called -----*/
    void 	show_usage();
				       /*----- extern variables -----*/

				       /*----- local  variables -----*/

    static char	valid_options[]     = "BDGLMOSTWZadlnxfgiovz",
                valid_arg_options[] = "BDGLMOSTWZadlnx";

    int		arg_int,
		num_files,
		i,
		draw_input_diagram   = FALSE,
		draw_output_diagram  = FALSE,
		draw_in_fig_syntax   = FALSE,
		draw_in_graph_syntax = FALSE;

    double	segment_separation = 1.0,
    		branch_diameters = 1.0,
    		scalebar_length = 1.0,
    		label_height = 1.0,
    		description_height = 1.0,
    		max_tag_size = 1.0,
    		tag_separation = 1.0,
    		connection_width = 1.0;


				       /*------- start function -----*/


			     /* convert arguments to standard format */

    if (standard_command_format( valid_options,
			         valid_arg_options,
			         &argc,
			         &argv )) {

	show_usage(argv[0]);
	return(TRUE);
    }

    			     /* process options and option arguments */

    for( i=1; strcmp(argv[i], OPTEND); i++) {

	switch( argv[i][1] ) {

	    case 'f':
		draw_in_fig_syntax = TRUE;
		break;

	    case 'g':
		draw_in_graph_syntax = TRUE;
    		diagram_parameters_ptr->destinations |= GRAPH;
		break;

	    case 'i':
		draw_input_diagram = TRUE;
		break;

	    case 'o':
		draw_output_diagram = TRUE;
		break;

	    case 'v':
		*verifying_ptr = TRUE;
		break;

	    case 'z':
		neuron_ptr->average_outline_z_coords = TRUE;
		break;

	    case 'x':
		i++;
		switch (argv[i][0]) {

		    case 'c':
		    case 'C':
			neuron_ptr->destination= O_CABLE;
			break;
		    case 'm':
		    case 'M':
			neuron_ptr->destination= O_MULTI;
			break;
		    case 'n':
		    case 'N':
			neuron_ptr->destination= O_NEURON;
			break;
		    default:
		        arg_error(O);
		}
		break;

	    case 'a':
		i++;
		if ( !isdigit(argv[i][0])
		     ||
		     ((arg_int = atoi(argv[i])) < WHOLE_BRANCH) 
		     ||
		     (arg_int > BETWEEN_POINTS) ) {

		    show_usage(argv[0]);
		    fprintf(stderr,
		    "%s: accuracy level must be 0, 1 or 2\n",
		    argv[0]);
		    return(TRUE);
		}
		else {
		    neuron_ptr->accuracy_level = arg_int;
		}
		break;

	    case 'd':
		i++;
		if (!sscanf( argv[i], "%lf", 
			     &(neuron_ptr->max_dx_over_mean_diam))
		    ||
		    (neuron_ptr->max_dx_over_mean_diam) <= 0.0) {

		    arg_error(d);
		}
		break;

	    case 'l':
		i++;
		if (!sscanf(argv[i], "%lf", &(neuron_ptr->max_dx))
		    ||
		    (neuron_ptr->max_dx < 0.0)) {

		    arg_error(l);
		}
		break;

	    case 'n':
		i++;
		if ( (arg_int = atoi(argv[i])) <= 0) {

		    show_usage(argv[0]);
		    fprintf(stderr,
		    "%s: num_segments must be positive\n",
		    argv[0]);
		    return(TRUE);
		}
		else {
		    neuron_ptr->num_segments_requested = arg_int;
		}
		break;

	    case 'B':
		i++;
		if (!sscanf(argv[i], "%lf", &branch_diameters)
		    ||
		    branch_diameters <= 0.0) {

		    arg_error(B);
		}
		draw_in_fig_syntax = TRUE;
		break;

	    case 'D':
		i++;
		if (!sscanf(argv[i], "%lf", &description_height)
		    ||
		    description_height <= 0.0) {

		    arg_error(D);
		}
		draw_in_fig_syntax = TRUE;
		break;

	    case 'G':
		i++;
		if (!sscanf(argv[i], "%lf", &segment_separation)
		    ||
		    segment_separation < 0.0) {

		    arg_error(G);
		}
		draw_in_fig_syntax = TRUE;
		break;

	    case 'L':
		i++;
		if (!sscanf(argv[i], "%lf", &label_height)
		    ||
		    label_height <= 0.0) {

		    arg_error(L);
		}
		draw_in_fig_syntax = TRUE;
		break;

	    case 'M':
		i++;
		if (!sscanf(argv[i], "%d",
		    &(diagram_parameters_ptr->magnification))
		    ||
		    diagram_parameters_ptr->magnification < 0) {

		    show_usage(argv[0]);
		    fprintf(stderr,
		    "%s: magnification must be integer >= 0\n",
		    argv[0]);
		    return(TRUE);
		}
		draw_in_fig_syntax = TRUE;
		break;

	    case 'O':
		i++;
		switch (argv[i][0]) {

		    case 'l':
		    case 'L':
			diagram_parameters_ptr->show_labels= FALSE;
			break;
		    case 'd':
		    case 'D':
			diagram_parameters_ptr->show_diameters= FALSE;
			break;
		    default:
		        arg_error(O);
		}
		draw_in_fig_syntax = TRUE;
		break;

	    case 'S':
		i++;
		if (!sscanf(argv[i], "%lf", &scalebar_length)
		    ||
		    scalebar_length <= 0.0) {

		    arg_error(S);
		}
		draw_in_fig_syntax = TRUE;
		break;

	    case 'T':
		i++;
		if (!sscanf(argv[i], "%lf", &max_tag_size)
		    ||
		    max_tag_size <= 0.0) {

		    arg_error(T);
		}
		draw_in_fig_syntax = TRUE;
		break;

	    case 'W':
		i++;
		if (!sscanf(argv[i], "%lf", &connection_width)
		    ||
		    connection_width <= 0.0) {

		    arg_error(W);
		}
		draw_in_fig_syntax = TRUE;
		break;

	    case 'Z':
		i++;
		if (!sscanf(argv[i], "%lf", &tag_separation)
		    ||
		    tag_separation <= 0.0) {

		    arg_error(Z);
		}
		draw_in_fig_syntax = TRUE;
		break;

	}

    }

    ++i;					/* skip over OPTEND */

    					/* store diagram parameters */

    diagram_parameters_ptr->segment_separation  = 
        irint(diagram_parameters_ptr->segment_separation 
				    * segment_separation);

    diagram_parameters_ptr->branch_separation  /= branch_diameters;
    diagram_parameters_ptr->scalebar_length    *= scalebar_length;
    diagram_parameters_ptr->label_height       *= label_height;
    diagram_parameters_ptr->description_height *= description_height;
    diagram_parameters_ptr->max_tag_size       *= max_tag_size;
    diagram_parameters_ptr->tag_separation     *= tag_separation;
    diagram_parameters_ptr->connection_width   *= connection_width
    					          /branch_diameters;

    		   /* make diagram flags consistent with each other */

    if ( (draw_in_fig_syntax || draw_in_graph_syntax)
	 &&
	 !draw_output_diagram
	 &&
	 !draw_input_diagram )
	draw_input_diagram = draw_output_diagram = TRUE;
	 
    if ( (draw_input_diagram || draw_output_diagram)
	 &&
	 !draw_in_fig_syntax
	 &&
	 !draw_in_graph_syntax)
	draw_in_fig_syntax = draw_in_graph_syntax = TRUE;
	 

    if (draw_in_fig_syntax)
        diagram_parameters_ptr->destinations |= FIG;

    if (draw_in_graph_syntax)
        diagram_parameters_ptr->destinations |= GRAPH;

    *draw_input_diagram_ptr   = draw_input_diagram;
    *draw_output_diagram_ptr  = draw_output_diagram;
    *draw_in_fig_syntax_ptr   = draw_in_fig_syntax;
    *draw_in_graph_syntax_ptr = draw_in_graph_syntax;

    if (neuron_ptr->destination == O_NEURON)
	neuron_ptr->accuracy_level = WHOLE_BRANCH;

				   	       /* process file names */
    num_files = argc-i;


    *nts_fp_ptr = stdin;
    *hoc_fp_ptr = stdout;

    switch (num_files) {

	case 2:
	    neuron_ptr->output_filename = argv[i+1];
	    *hoc_fp_ptr = (FILE *) NULL;

	case 1:
	    if ( strcmp("-", argv[i])) {
		neuron_ptr->input_filename = argv[i];
		*nts_fp_ptr = (FILE *) NULL;
	    }
	    break;
	
	default:
	    show_usage(argv[0]);
	    return(TRUE);
    }

    if (strsave( &(neuron_ptr->input_version), 
        	 sccs_what(neuron_ptr->input_filename)))
	exit(TRUE);


    return(FALSE);
}


/*-------------------------------------------------------------------*/
void show_usage(pgm_name)

    char	*pgm_name;
				/* Shows proper command line format. */

{
				       /*----- extern variables -----*/
    extern char version[],
		pgm_date[];
				       /*----- start function -------*/

    fprintf(stderr,
    "\n%s  Version %s, %s\n", pgm_name, version, pgm_date );
    fprintf(stderr,"usage:\n");
    fprintf(stderr,"%s\n\n",
    "	ntscable [options] [option_args] input_file [output_file]");
    fprintf(stderr,"options:\n");
    fprintf(stderr, "%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n\n",
    "  -f                         create diagrams in fig syntax",
    "  -g                         create diagrams in graph syntax",
    "  -i                         create diagram of input file",
    "  -o                         create diagram of output file",
    "  -v                         create verify.txt",
    "  -z                         average soma outline z coordinates",
    "  -x <output_syntax>         neuron, cable or multi",
    "  -a <translation_accuracy>  0, 1 or 2; 0=least, 2=most accurate",
    "  -n <num_segments>          minimum number of segments",
    "  -l <segment_length>        maximum segment length in microns",
    "  -d <length_factor>         max seg length as multiple of diam"
    );

    fprintf(stderr, "%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n\n",
    "  -M <magnification>         magnify diagram by integer factor",
    "  -O <feature_to_omit>       omit labels or diams from diagram",
    "  -B <branch_diameters>     \\",
    "  -D <description_size>      |--Each of these options changes",
    "  -G <gap_between_segments>  |  the size of one feature of the",
    "  -L <label_size>            |  schematic diagram.  The argu-",
    "  -S <scalebar_length>       |  ment is a floating point factor",
    "  -T <max_tag_size>          |  by which the default size is",
    "  -W <connection_width>      |  multiplied.",
    "  -Z <tag_separation>       /"
    );
}



