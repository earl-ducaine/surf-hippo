	/* @(#)write.c	1.17  9/15/91 */

	/*-------------------------------------------------------------
	|							      |
	|	PROGRAM:  ntscable                                    |
	|							      |
	|	MODULE:   write.c                                         |
	|							      |
	|	MACHINE:  Sun 3/60                                    |
	|							      |
	|	STARTED:  20-APR-89        BY:  J.C. Wathey           |
	|							      |
	|	REVISED:  9/15/91         BY:  JCW                   |
	|							      |
	|	STATUS:      incomplete or untested		      |
 	|                    compiles; partly tested		      |
	|                    runs; revisions in progress	      |
	|                 -> runs; stable version		      |
	|							      |
	|       CONTAINS: routines write_geometry() and               |
	|                 and write_neuron_description()              |
	|                                                             |
	|       COMPILE:                                              |
	|                                                             |
	|       (use makefile)                                        |
	|                                                             |
	-------------------------------------------------------------*/

/*--------------------------------------- HEADER FILES --------------*/
#include "tf.h"
#include <stdio.h>
#include <math.h>
#include <sys/types.h>
#include <ctype.h>
#include <errno.h>
#include "ntscable.h"		       /* global defs & declarations */

static char sepline[] = 
"----------------------------------------------------------------";



/*-------------------------------------------------------------------*/
int write_geometry( fp, neuron_ptr, cable_ptr, neurite_list )


	/* Writes to fp a "geometry" subroutine in hoc syntax, using
	the info in the *neuron_ptr and either *cable_ptr or 
	*neurite_list, depending on the syntax requested for the
	output file.  Returns TRUE if error; FALSE otherwise. */


    FILE      * fp; 
    NEURON    * neuron_ptr; 
    CABLE     * cable_ptr;
    NEURITE   * neurite_list;

{
				       /*----- functions called -----*/

				       /*----- extern variables -----*/

				       /*----- local  variables -----*/

				       /*----- start function -------*/

					 /* check translation syntax */
    switch (neuron_ptr->destination) {

	case O_MULTI:
	case O_CABLE:
            return(write_cable_geometry( fp, 
					 neuron_ptr, 
					 cable_ptr));

	default:
	case O_NEURON:
            return(write_neuron_geometry( fp, 
					  neuron_ptr, 
					  neurite_list));
    }

}



/*-------------------------------------------------------------------*/
static int write_neuron_geometry( fp, 
			          neuron_ptr, 
			          neurite_list )


	/* Writes to fp a "geometry" subroutine in hoc syntax, using
	the info in the *neuron_ptr and *neurite_list.  Returns TRUE 
	if error; FALSE otherwise. */


    FILE      * fp; 
    NEURON    * neuron_ptr; 
    NEURITE   * neurite_list;


{
				       /*----- functions called -----*/
				       /*----- extern variables -----*/
				       /*----- local  variables -----*/

    static char for_loop_i[] = "    for i = %d,%d {\n",
                for_loop_j[] = "for j = 1, fscan() {\n",
                read_3D_points[] =
		"pt3dadd(fscan(),fscan(),fscan(),fscan())";

    int			neurite_i,
			section_i, 
			xyzd_i;

    NEURITE	      * neurite_ptr,
		      * neurite_list_end;
    SECTION	      * section_ptr,
		      * section_list_end;
    XYZD	      * xyzd_ptr,
		      * xyzd_list_end;

				       /*----- start function -------*/


    write_neuron_comment(fp, neuron_ptr);
	
    neurite_list_end = neurite_list + neuron_ptr->num_primary_neurites;

    fprintf(fp,"\n\nRa = %g\n", neuron_ptr->cytoplasmic_resistivity);

    						  /* create sections */

    fprintf(fp,"\t\t\t\t\t/* create sections */\n");
    fprintf(fp,"create    soma");

    for ( neurite_ptr = neurite_list;
          neurite_ptr < neurite_list_end;
          neurite_ptr++ ) {

	fprintf(fp, ",\\\n          %s[%d]",
		    neurite_ptr->name,
		    neurite_ptr->num_sections);
    }
    fprintf(fp,"\n\n");

					  /* main geometry procedure */

    fprintf(fp, "\n\n/*%s*/\n", sepline);
    fprintf(fp,"proc geometry() { local i, j\n\n");

						    /* soma geometry */

    fprintf(fp,"\t\t\t\t\t\t/* soma geometry */\n");

    fprintf(fp, "    soma {\n");
    fprintf(fp, "        nseg = 1\n");
    fprintf(fp, "        pt3dclear()\n");
    fprintf(fp, "        %s", for_loop_j );
    fprintf(fp, "            %s\n", read_3D_points);
    fprintf(fp, "        }\n");
    fprintf(fp,"    }\n\n");

    					 /* connect primary neurites */

    fprintf(fp,"\t\t\t\t\t/* connect primary neurites */\n");
    for ( neurite_ptr = neurite_list;
          neurite_ptr < neurite_list_end;
          neurite_ptr++ ) {

	fprintf(fp, "    soma connect %s[0] (0), 0.5\n",
		    neurite_ptr->name);
    }
    fprintf(fp,"\n\n");
						 /* neurite geometry */

    fprintf(fp,"\t\t\t\t\t/* neurite geometry*/\n");
    for ( neurite_ptr = neurite_list;
          neurite_ptr < neurite_list_end;
          neurite_ptr++ ) {


	fprintf(fp, for_loop_i, 0, neurite_ptr->num_sections-1);
	fprintf(fp, "        %s[i] {\n", neurite_ptr->name);
	fprintf(fp, "            nseg = fscan()\n");
	fprintf(fp, "            pt3dclear()\n");
	fprintf(fp, "            %s", for_loop_j );
	fprintf(fp, "                %s\n", read_3D_points);
	fprintf(fp, "            }\n");
	fprintf(fp, "        }\n");
	fprintf(fp, "    }\n\n");
    }
    fprintf(fp,"\n\n");

    					       /* branching topology */

    fprintf(fp,"\t\t\t\t\t/* branching topology*/\n");
    for ( neurite_ptr = neurite_list;
          neurite_ptr < neurite_list_end;
          neurite_ptr++ ) {

	fprintf(fp, for_loop_i, 1, neurite_ptr->num_sections-1);
	fprintf(fp, "        %s[fscan()] connect %s[i] (0), fscan()\n",
			     neurite_ptr->name, neurite_ptr->name);
	fprintf(fp, "    }\n\n");
    }

    fprintf(fp,"}\n\n");

    fprintf(fp,"geometry()\n");


    fprintf(fp,"\n\n");

    fprintf(fp, "SOMA COORDINATES AND DIAMETERS:\n");

    fprintf(fp, "\n    %d\n",
		neuron_ptr->num_soma_3D_points);

    xyzd_list_end = neuron_ptr->soma_3D_list +
		    neuron_ptr->num_soma_3D_points;

    for( xyzd_ptr = neuron_ptr->soma_3D_list;
	 xyzd_ptr < xyzd_list_end;
	 xyzd_ptr++ ) {

	fprintf(fp, 
	"%        8.5g %8.5g %8.5g %8.5g\n",
	xyzd_ptr->x,
	xyzd_ptr->y,
	xyzd_ptr->z,
	xyzd_ptr->diam);
    }
    fprintf(fp,"\n\n");

    fprintf(fp, "NEURITE COORDINATES AND DIAMETERS:\n");

    for ( neurite_ptr = neurite_list;
          neurite_ptr < neurite_list_end;
          neurite_ptr++ ) {

        section_list_end = neurite_ptr->section_list +
			   neurite_ptr->num_sections;

	for ( section_ptr = neurite_ptr->section_list;
	      section_ptr < section_list_end;
	      section_ptr++ ) {

	    fprintf(fp, "\n    %d %d\n",
			section_ptr->num_segments,
			section_ptr->num_xyzd_points);

	    xyzd_list_end = section_ptr->xyzd_list +
			    section_ptr->num_xyzd_points;

	    for( xyzd_ptr = section_ptr->xyzd_list;
		 xyzd_ptr < xyzd_list_end;
		 xyzd_ptr++ ) {

		fprintf(fp, 
		"%        8.5g %8.5g %8.5g %8.5g\n",
		xyzd_ptr->x,
		xyzd_ptr->y,
		xyzd_ptr->z,
		xyzd_ptr->diam);
	    }
	}
    }

    fprintf(fp,"\n\n");

    fprintf(fp, "CONNECTIONS:\n");

    for ( neurite_ptr = neurite_list;
          neurite_ptr < neurite_list_end;
          neurite_ptr++ ) {

        section_list_end = neurite_ptr->section_list +
			   neurite_ptr->num_sections;

	for ( section_ptr = neurite_ptr->section_list+1;
	      section_ptr < section_list_end;
	      section_ptr++ ) {

	    fprintf(fp,"    %5d    %g\n",
		       section_ptr->parent_index,
		       section_ptr->origin_in_parent);

	}

        fprintf(fp,"\n");
    }

					  /* NULL geometry procedure */

    fprintf(fp, "\n/*%s*/\n", sepline);
    fprintf(fp,"proc geometry() { \n\n");
    fprintf(fp,"\t/* NULL geometry procedure: keeps the user from\n");
    fprintf(fp,"\tcalling the geometry procedure in isolation. */\n\n");
    fprintf(fp,
    "    printf(\"\\nYou must re-read the entire geometry\\n\")\n");
    fprintf(fp,
    "    printf(\"file to execute geometry().\\n\\n\")\n");
    fprintf(fp,"}\n\n");

    return(ferror(fp));

}

/*-------------------------------------------------------------------*/
static int write_cable_geometry( fp, 
			         neuron_ptr, 
			         cable_ptr )


	/* Writes to fp a "geometry" subroutine in hoc syntax, using
	the info in the *neuron_ptr and *cable_ptr.  Returns TRUE if 
	error; FALSE otherwise. */


    FILE      * fp; 
    NEURON    * neuron_ptr; 
    CABLE     * cable_ptr;


{
				       /*----- functions called -----*/
				       /*----- extern variables -----*/
				       /*----- local  variables -----*/

    static char for_loop[] = "    for (i=%d; i<=%d; i=i+1)\t{";
    char      * type_index;
    int 	first,
		last,
		i,
		num_lines_in_procedure,
		num_fcbs_procedures,
		num_dx_procedures,
		num_diam_procedures;
    double	first_diam;
    double	last_diam;
    double	slope;

    FCBS_ARGS         * fcbs_args_ptr,
		      * fcbs_list_end;
    DX_INTERVAL       * dx_interval_ptr,
   		      * dx_list_end;
    DIAM_INTERVAL     * diam_interval_ptr,
   		      * diam_list_end;

				       /*----- start function -------*/

					 /* check translation syntax */
    switch (neuron_ptr->destination) {
	case O_MULTI:
	    type_index = "[type]";
	    break;
	case O_CABLE:
	default:
	    type_index = "";
    }


    write_neuron_comment(fp, neuron_ptr);

    					       /* branching topology */
    num_fcbs_procedures = 0;
    fcbs_args_ptr = cable_ptr->fcbs_args_list;
    fcbs_list_end = fcbs_args_ptr + cable_ptr->num_fcbs_args;

    do {
        fprintf(fp, "\n\n/*%s*/\n", sepline);
        fprintf(fp,"proc init_topology_%d() {\n\n",
			num_fcbs_procedures);

	if (!num_fcbs_procedures)
            fprintf(fp,"    fcbs(0)\n");

        for (num_lines_in_procedure = 0;
		
             fcbs_args_ptr < fcbs_list_end
	     &&
	     num_lines_in_procedure < MAX_LINES_PER_PROC - 3;

	     num_lines_in_procedure++,
             fcbs_args_ptr++)

    	    fprintf(fp, "    fcbs(%d, %d, %d)\n", 
    	    fcbs_args_ptr->first_index,
    	    fcbs_args_ptr->last_index,
    	    fcbs_args_ptr->root_index);

        fprintf(fp,"}\n");
	num_fcbs_procedures++;
    } while (fcbs_args_ptr < fcbs_list_end);
	
    						  /* segment lengths */
    num_dx_procedures = 0;
    dx_interval_ptr = cable_ptr->dx_interval_list;
    dx_list_end = dx_interval_ptr + cable_ptr->num_dx_intervals;

    do {
        fprintf(fp, "\n\n/*%s*/\n", sepline);
        fprintf(fp,"proc init_dx_%d() { local i\n\n",
			num_dx_procedures);

        for (num_lines_in_procedure = 0;

             dx_interval_ptr < dx_list_end
	     &&
	     num_lines_in_procedure < MAX_LINES_PER_PROC - 2;

	     num_lines_in_procedure++,
             dx_interval_ptr++) {

	    if ( (first = dx_interval_ptr->first_index)
		    ==
    	         (last = dx_interval_ptr->last_index) ) {
    	        fprintf(fp,"    dx%s[%d] = %g\n", type_index,
		first, dx_interval_ptr->dx);
	    }
	    else if ( first + 1 == last  ) {
    	        fprintf(fp,"    dx%s[%d] = %g\n", type_index,
		first, dx_interval_ptr->dx);
    	        fprintf(fp,"    dx%s[%d] = %g\n", type_index,
		last, dx_interval_ptr->dx);
	        num_lines_in_procedure++;
	    }
	    else {
	        fprintf(fp, for_loop, first, last);
    	        fprintf(fp," dx%s[i] = %g }\n", type_index,
    	        dx_interval_ptr->dx);
	    }

        }

        fprintf(fp,"}\n");
	num_dx_procedures++;
    } while (dx_interval_ptr < dx_list_end);

    						/* segment diameters */

    num_diam_procedures = 0;
    diam_interval_ptr = cable_ptr->diam_interval_list;
    diam_list_end = diam_interval_ptr + cable_ptr->num_diam_intervals;

    do {
        fprintf(fp, "\n\n/*%s*/\n", sepline);
        fprintf(fp,"proc init_diam_%d() { local i\n\n",
			num_diam_procedures);

        for (num_lines_in_procedure = 0;

             diam_interval_ptr < diam_list_end
	     &&
	     num_lines_in_procedure < MAX_LINES_PER_PROC - 2;

	     num_lines_in_procedure++,
             diam_interval_ptr++) {

	    first      = diam_interval_ptr->first_index;
	    last       = diam_interval_ptr->last_index;
	    first_diam = diam_interval_ptr->first_diam;
	    last_diam  = diam_interval_ptr->last_diam;

	    if ( first == last ) {
    	        fprintf(fp,"    diam%s[%d] = %g\n", type_index,
		first, (first_diam + last_diam)/2 );
	    }
	    else if ( first + 1 == last  ) {
    	        fprintf(fp,"    diam%s[%d] = %g\n", type_index,
		first, first_diam);
    	        fprintf(fp,"    diam%s[%d] = %g\n", type_index,
		last, last_diam);
	        num_lines_in_procedure++;
	    }
	    else if (first_diam == last_diam) {
	        fprintf(fp, for_loop, first, last);
    	        fprintf(fp," diam%s[i] = %g }\n", type_index,
		first_diam);
	    }
	    else {
	        slope = (last_diam - first_diam)/(last-first);
	        fprintf(fp, for_loop, first, last);
    	        fprintf(fp," diam%s[i] = %g %c %g*(i-%d) }\n",
		type_index,
	        first_diam, slope<0 ? '-' : '+', fabs(slope), first);
	    }

        }

        fprintf(fp,"}\n");
	num_diam_procedures++;
    } while (diam_interval_ptr < diam_list_end);

					  /* main geometry procedure */
    fprintf(fp, "\n\n/*%s*/\n", sepline);
    fprintf(fp,"proc geometry() {\n\n");

    for (i = 0; i<num_fcbs_procedures; i++)
    	fprintf(fp, "    init_topology_%d()\n\n", i);

    for (i = 0; i<num_dx_procedures; i++)
    	fprintf(fp, "    init_dx_%d()\n\n", i);

    for (i = 0; i<num_diam_procedures; i++)
    	fprintf(fp, "    init_diam_%d()\n\n", i);

						    /* soma geometry */

    fprintf(fp,"\t\t\t\t\t/* cylindrical soma geometry */\n");
    fprintf(fp,"    diam%s[0] = %g\n", type_index,
    		neuron_ptr->soma_diam);
    fprintf(fp,"    dx%s[0]   = %g\n", type_index, 
		neuron_ptr->soma_length);
    fprintf(fp,"    ra%s      = %g\n", type_index,
		neuron_ptr->cytoplasmic_resistivity);
    fprintf(fp,"    farea(3)\n");
    fprintf(fp,"    area%s[0] = PI*diam%s[0]*dx%s[0]\n", 
		type_index, type_index, type_index );
    fprintf(fp,"    fcbs()\n\n");
    fprintf(fp,"}\n\n");

    return(ferror(fp));

}


/*-------------------------------------------------------------------*/
int write_neuron_description(fp, neuron_ptr)

	/* Writes to fp a text description of the neuron and its
	translation. Returns TRUE if error, FALSE otherwise. */

    FILE      * fp;
    NEURON    * neuron_ptr;

{
				       /*----- functions called -----*/

				       /*----- extern variables -----*/
    extern char version[];

				       /*----- local  variables -----*/

				       /*----- start function -------*/

    fprintf(fp,
    "%s %s translated %24.24s by ntscable %s\n",
    neuron_ptr->input_filename,
    neuron_ptr->input_version,
    neuron_ptr->time_of_translation,
    version);
    fprintf(fp, "source file syntax: %s\n", 
	source_syntax[(neuron_ptr->source)]);
    fprintf(fp, "output file syntax: %s\n", 
	output_syntax[(neuron_ptr->destination)]);

    fprintf(fp,
    "soma: diameter = %g um  length = %g um  area = %g um2\n",
    neuron_ptr->soma_diam, neuron_ptr->soma_length,
    neuron_ptr->soma_area);

    fprintf(fp, "      %d three-D points", 
    	neuron_ptr->num_soma_3D_points);
    if (neuron_ptr->num_soma_outline_points) {
        fprintf(fp, "; %d outline points",
    	neuron_ptr->num_soma_outline_points);
    }
    if (neuron_ptr->first_soma_point
	||
        neuron_ptr->last_soma_point) {
	fprintf(fp,
	" numbered %d-%d",
	neuron_ptr->first_soma_point,
	neuron_ptr->last_soma_point);
    }
    fprintf(fp, "\n");

    if (neuron_ptr->num_soma_outline_points) {
        fprintf(fp, "      outline diameter = %g um%s\n",
    	neuron_ptr->soma_outline_diam,
        (neuron_ptr->average_outline_z_coords) ?
	"; z coordinates averaged" : "" );
    }

    fprintf(fp,
    "%d primary neurites\n",
    neuron_ptr->num_primary_neurites);

    fprintf(fp,
    "%d branches totaling %g um in length, %g um2 in area\n",
    neuron_ptr->num_branches,
    neuron_ptr->total_length_of_neurites,
    neuron_ptr->total_area_of_neurites);

    fprintf(fp,
    "%d tree points translated to %d segments (%d requested)\n",
    neuron_ptr->num_tree_points,
    neuron_ptr->num_segments_used,
    neuron_ptr->num_segments_requested);

    fprintf(fp,
    "Neurites divided into segments of equal dx ");

    switch (neuron_ptr->accuracy_level) {
	case BETWEEN_POINTS:
            fprintf(fp,
	    "between adjacent digitized\npoints.\n");
	    break;
	case BETWEEN_BRANCHPOINTS:
            fprintf(fp,
	    "between adjacent digitized\nbranch points.\n");
	    break;
	case WHOLE_BRANCH:
            fprintf(fp,
	    "over each entire branch.\n");
    }

    fprintf(fp,
    "Segment length constrained to be < %g um",
    neuron_ptr->max_dx);
    if (neuron_ptr->max_dx_over_mean_diam > 0.0)  {
        fprintf(fp,
	", or < %g times\n",
        neuron_ptr->max_dx_over_mean_diam );
        fprintf(fp,
	"mean diameter of translation interval, whichever is less.\n");
    }
    else {
        fprintf(fp,".\n");
    }

    fprintf(fp, "%s", neuron_ptr->comment);

    return(ferror(fp));

}




/*-------------------------------------------------------------------*/
static int write_neuron_comment(fp, neuron_ptr)

	/* Writes to fp a HOC-style comment describing the neuron and 
	its translation. Returns TRUE if error, FALSE otherwise. */

    FILE      * fp;
    NEURON    * neuron_ptr;


{
				       /*----- functions called -----*/
    char * strchr();

				       /*----- extern variables -----*/

				       /*----- local  variables -----*/
    char  * ptr;

				       /*----- start function -------*/

    			     /* delete any C-style comment terminators 
    			     from the comment */

    ptr = neuron_ptr->comment;

    while( *ptr && (ptr = strchr(ptr, '*')) )
	if (*(ptr+1) == '/') 
	    strcpy(ptr, ptr+2);
	else
	    ptr++;

    		       /* Write comment describing the source file and
    		       the results of translation.  Include leading 
    		       comment from the source file. */

    fprintf(fp, "/*%s\n", sepline);
    fprintf(fp, "%%%c%%  %%%c%%\n", 'W', 'G');

    write_neuron_description(fp, neuron_ptr);

    fprintf(fp, "%s*/\n", sepline);
    return(ferror(fp));

}
