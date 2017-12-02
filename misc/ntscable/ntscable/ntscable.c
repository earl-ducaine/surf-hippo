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
	|       CONTAINS: file inclusions and  main program           |
	|                                                             |
	|       COMPILE:                                              |
	|                                                             |
	|       (use makefile)                                        |
	|                                                             |
	-------------------------------------------------------------*/

/*--------------------------------------- HEADER FILES --------------*/
#include "tf.h"
#include <stdio.h>
#include "diagrams.h"
#include "ntscable.h"		       /* global defs & declarations */


/*----------------------------------------- MAIN PROGRAM ------------*/
main(argc,argv)

	/* Translates an ascii file from the Neuron Tracing System to 
	a geometry() routine in 'hoc' syntax for use by CABLE. */

int	argc;
char	*argv[];

{
				       /*----- FUNCTIONS CALLED -----*/
    void 	show_usage();
    void 	translate();
    void	init_diagram();
    int		process_command_line();

				       /*----- EXTERN VARIABLES -----*/

				       /*----- LOCAL  VARIABLES -----*/


    int		error,
		verifying = FALSE,
		draw_input_diagram = FALSE,
		draw_output_diagram = FALSE,
		draw_in_fig_syntax = FALSE,
		draw_in_graph_syntax = FALSE;


    NEURON		neuron;
    CABLE		cable;
    NEURITE	      *	neurite_list = (NEURITE *) NULL;

    DIAGRAM	      * output_diagram_list = (DIAGRAM *) NULL,
           	      * input_diagram_list  = (DIAGRAM *) NULL,
			diagram_parameters;

    FILE      * nts_fp,
	      * hoc_fp;


				       /*-------- START MAIN --------*/


				    /* initialize diagram parameters */

    init_diagram( &diagram_parameters );

    						 /* initialize cable */

    bzero( (char *) &cable, sizeof(CABLE));

    						/* initialize neuron */

    bzero( (char *) &neuron, sizeof(NEURON));
    neuron.cytoplasmic_resistivity = DEFAULT_RA;
    neuron.accuracy_level          = DEF_ACCURACY_LEVEL;
    neuron.num_segments_requested  = DEF_NUM_SEGMENTS;
    neuron.input_filename          = "stdin";
    neuron.output_filename         = "stdout";

				   /* process command line arguments */

    if (process_command_line( argc,
			      argv,
			      &verifying,
			      &nts_fp,
			      &hoc_fp,
			      &draw_input_diagram,
			      &draw_output_diagram,
			      &draw_in_fig_syntax,
			      &draw_in_graph_syntax,
			      &diagram_parameters,
			      &neuron))
	exit(TRUE);

					      /* open input file if
					      not reading from stdin */

    if (! (int)nts_fp && !(nts_fp=fopen(neuron.input_filename, "r"))) {
	perror(neuron.input_filename);
	show_usage(argv[0]);
	exit(TRUE);
    }

						  /* read input file */
    if (read_neuron(nts_fp, &neuron))
	exit(TRUE);

							/* translate */
    translate( &neuron, &cable, &neurite_list );


					       /* open output file if
					       not writing to stdout */

    if ( ! (int)hoc_fp 
	 && 
	 !(hoc_fp=fopen(neuron.output_filename, "w"))) {

	perror(neuron.output_filename);
	show_usage(argv[0]);
	exit(TRUE);
    }
						/* write output file */

    error = write_geometry( hoc_fp, &neuron, &cable, neurite_list );

    if (nts_fp != stdin)
    	fclose(nts_fp);
	
    if (hoc_fp != stdout)
    	fclose(hoc_fp);
			            /* delete old schematic diagrams */

    if (draw_input_diagram || draw_output_diagram)
    	system("rm -f in*.gph out*.gph in*.fig* out*.fig*");

			      /* draw schematic diagrams of neurites */
    if ( !error 
	 &&
	 draw_input_diagram
	 &&
	 draw_in_graph_syntax
	 &&
	 (error = draw_nts_neuron( &neuron )) )

	printf("error in draw_nts_neuron()\n");

    if (neuron.accuracy_level == WHOLE_BRANCH) {

        if ( (!error) 
	     &&
	     draw_output_diagram
	     &&
	     draw_in_graph_syntax
	     &&
	     (error = draw_cable_neuron( &neuron, 
					 &cable )) )

            printf("error in draw_cable_neuron()\n");

    }

    if (draw_in_fig_syntax) {

	if ( draw_input_diagram
	     &&
	     start_input_diagram_list( &neuron,
    				       &diagram_parameters,
				       &input_diagram_list ))
	    exit( TRUE );

	if ( draw_output_diagram
	     &&
	     start_output_diagram_list( &neuron,
					&cable,
					neurite_list,
    					&diagram_parameters,
					&output_diagram_list ) )
	    exit( TRUE );


	if ( draw_input_diagram )
	    error = draw_diagram_list( input_diagram_list );

	if ( draw_output_diagram )
	    error = error
		    ||
		    draw_diagram_list( output_diagram_list );
    }

							   /* verify */
    if (verifying)
        verify( &neuron, 
		  &cable,
		    neurite_list,
		      input_diagram_list,
		        output_diagram_list);


    				   /* give nseg warning if necessary */

    if (neuron.destination == O_CABLE
	&&
	neuron.num_segments_used > MAX_NSEG)
        fprintf(stderr, 
	"\nWARNING: recompile cable with NSEG >= %d before using %s\n\n",
        neuron.num_segments_used, neuron.output_filename);

    exit(error);

}				       /*--------- END MAIN ---------*/






