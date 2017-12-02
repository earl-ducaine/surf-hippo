	/* @(#)write.c	1.17  9/15/91 */


        /*
	  
	  Modified version for generating SURF-HIPPO files.
	  Lyle Borg-Graham, 8/1/94
	   
        */  



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

/* 10-1-93 Modified by LBG to write out a lisp format list that can be
digested by the SURF-HIPPO function (NTS-CELL).
*/

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
                        xyzd_i,
                        i;


    NEURITE	      * neurite_ptr,
                      * neurite_list_end;
    SECTION	      * section_ptr,
		      * section_list_end;
    XYZD	      * xyzd_ptr,
                      * xyzd_list_end;
    SOMA_POINT        * soma_ptr;
				       /*----- start function -------*/


    fprintf(fp, ";;; -*- Mode: Lisp; Syntax: Common-lisp; Package: SURF; Base: 10; -*- ");
    
    fprintf(fp, "\n\n");
    fprintf(fp, "(in-package \"SURF-HIPPO\")");
    fprintf(fp, "\n\n");
    fprintf(fp, "#|\n");

    write_neuron_comment(fp, neuron_ptr);

    fprintf(fp,"\n\n");

/*
    fprintf(fp,"create    soma");

    for ( neurite_ptr = neurite_list;
          neurite_ptr < neurite_list_end;
          neurite_ptr++ ) {

	fprintf(fp, ",\\\n          %s[%d]",
		    neurite_ptr->name,
		    neurite_ptr->num_sections);
    }
    fprintf(fp,"\n\n");
*/

						    /* soma geometry */

    fprintf(fp,"\t\t\t\t\t\t/* soma geometry */\n");

    fprintf(fp, "    soma {\n");
    fprintf(fp, "        nseg = 1\n");
    fprintf(fp, "        pt3dclear()\n");
    fprintf(fp, "        %s", for_loop_j );
    fprintf(fp, "            %s\n", read_3D_points);
    fprintf(fp, "        }\n");
    fprintf(fp,"    }\n\n");


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


    fprintf(fp, "|#\n\n\n\n");




/* lbg change 4/7/94 */

    if (!(neuron_ptr->soma_outline)) {
        fprintf(fp,"\n;; No soma outline. \n");
    }
    else {
        fprintf(fp,"\n(setq *soma-outline*\n\n '(\n");

	soma_ptr = neuron_ptr->soma_outline;
	     
	do {
	    fprintf(fp, "  (%g %g %g)\n",
	            soma_ptr->x,
	            soma_ptr->y,
	            soma_ptr->z );

	    soma_ptr = soma_ptr->next;
	} while (soma_ptr != neuron_ptr->soma_outline);
        fprintf(fp,"\n ))\n");
    }

    if (!(neuron_ptr->num_soma_3D_points)) {
        fprintf(fp,"\n;; No soma 3D points.\n");
    }
    else {
        fprintf(fp,"\n(setq *soma-points*\n\n '(\n");
	for (i=0; i< neuron_ptr->num_soma_3D_points; i++) {

	    fprintf(fp, "  (%g %g %g %g)\n",
	            neuron_ptr->soma_3D_list[i].x,
	            neuron_ptr->soma_3D_list[i].y,
	            neuron_ptr->soma_3D_list[i].z,
	            neuron_ptr->soma_3D_list[i].diam);
	}
        fprintf(fp,"\n ))\n");
    }

/* lbg change 4/7/94 */





/* lbg change 4/6/94 */

    fprintf(fp, 
    "(setq  *nts-radius* (sqrt (/ %g (* 3.14159 4))))   \n \n",
	    neuron_ptr->soma_area);

/* lbg change 4/6/94 */


    fprintf(fp, "(setq *ntscable-list* '(\n");

    neurite_list_end = neurite_list + neuron_ptr->num_primary_neurites;

    						  /* create sections */

    



						    /* soma geometry */


/*    fprintf(fp, "NEURITE COORDINATES AND DIAMETERS:\n"); */

    neurite_i = 1;

    for ( neurite_ptr = neurite_list;
          neurite_ptr < neurite_list_end;
          neurite_ptr++ ) {

        section_list_end = neurite_ptr->section_list +
			   neurite_ptr->num_sections;

	section_i = 1;
	
	for ( section_ptr = neurite_ptr->section_list;
	      section_ptr < section_list_end;
	      section_ptr++ ) {

	    fprintf(fp, "\n ;;  Number of segs - %d, Number of xyzd points - %d\n",
			section_ptr->num_segments, section_ptr->num_xyzd_points);

	    xyzd_list_end = section_ptr->xyzd_list +
			    section_ptr->num_xyzd_points;
	    fprintf(fp, "% ;; Section %d, parent index %d\n", section_i, section_ptr->parent_index);
	    xyzd_i = 0;
	    for( xyzd_ptr = section_ptr->xyzd_list;
		 xyzd_ptr < xyzd_list_end;
		 xyzd_ptr++ ) {
	      if (xyzd_i == 0) 
		{if (section_ptr->parent_index == -1)
		   {
		    fprintf(fp, " \n"); 
		    fprintf(fp, "  ((%d %d %d)  SOMA ",
			    neurite_i, section_i, xyzd_i);}
		else
		  {
		   fprintf(fp, " \n");
/* lbg change - 4/6/94 */
/*		   fprintf(fp, "  ((%d %d %d) (BRANCH-PT %d %d %g) ", */
		   fprintf(fp, "  ((%d %d %d) (BRANCH-PT %d %d %d) ",
			   neurite_i, section_i, xyzd_i,
			   neurite_i, section_ptr->parent_index + 1, 
/* lbg change - 4/6/94 */
/*			   section_ptr->origin_in_parent, */
/*			   section_ptr->origin_in_name */
			   section_ptr->origin_in_index + 1
			   );}
	       }
	      else
		{
		 fprintf(fp, " \n");
		 fprintf(fp, "  ((%d %d %d) (%d %d %d) ",
			 neurite_i, section_i, xyzd_i,
			 neurite_i, section_i, xyzd_i - 1);}

	      fprintf(fp, " (%6.6g %6.6g %6.6g) %3.4g)\n",
		      (float) xyzd_ptr->x,
		      (float) xyzd_ptr->y,
		      (float) xyzd_ptr->z,
		      (float) xyzd_ptr->diam);
	      xyzd_i++;
	    }
	    fprintf(fp, ";;; End of section ********* \n");
	    section_i++;
	  }
	fprintf(fp, ";;; End of neurite ********* \n");
	neurite_i++;
    }
    fprintf(fp, "\n))\n");
    fprintf(fp,"\n\n");
    fprintf(fp, "(setq *process-ntscable-list* t)\n");


    fprintf(fp, "(setq *nts-cell-type* \"ntscable\")\n");

    fprintf(fp, "(setq *nts-cell-name* \"ntscable\")\n");
    fprintf(fp, "(setq *nts-r-mem* 40000.0)\n");
    fprintf(fp, "(setq *nts-soma-r-mem* 40000.0)\n");
    fprintf(fp, "(setq *nts-r-a* 200.0)\n");



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
