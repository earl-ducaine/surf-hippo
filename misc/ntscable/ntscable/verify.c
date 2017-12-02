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
	|       CONTAINS: routines to create verification file        |
	|                                                             |
	|       COMPILE:                                              |
	|                                                             |
	|       (use makefile)                                        |
	|                                                             |
	-------------------------------------------------------------*/

/*--------------------------------------- HEADER FILES --------------*/
#include "tf.h"
#include <stdio.h>
#include "ntscable.h"
#include "diagrams.h"

static int	indent_step = 4;

/*-------------------------------------------------------------------*/
int verify( neuron_ptr, 
	    cable_ptr,
	    neurite_list,
	    input_diagram_list,
	    output_diagram_list)

	/* Creates the file verify.txt for program verification or
	debugging.  verify.txt will contain a text listing of the
	complete internal representation of the original digitized 
	neuron at *neuron_ptr, as well as a listing of the 
	translation in *cable_ptr or *neurite_list used to generate 
	the geometry file.  It will also contain complete listings
	of the internal data structures used to generate schematic
	diagrams.  Returns TRUE if error occurs; FALSE otherwise. */

    NEURON	      * neuron_ptr;
    CABLE	      * cable_ptr;
    NEURITE           * neurite_list;
    DIAGRAM	      * input_diagram_list,
    		      * output_diagram_list;


{
				       /*----- functions called -----*/

				       /*----- extern variables -----*/

				       /*----- local  variables -----*/
    FILE	      * fp;
    static char		filename[] = "verify.txt";

				       /*----- start function -------*/

    if (!(fp=fopen(filename,"w"))) {
        perror(filename);
        return(TRUE);
    }

    if ( dump_neuron(fp, neuron_ptr)
	 ||
    	 dump_translation( fp,
			   cable_ptr,
			   neurite_list,
			   neuron_ptr)
	 ||
    	 dump_diagrams(fp, input_diagram_list)
	 ||
    	 dump_diagrams(fp, output_diagram_list) )
	return(TRUE);

    if (fclose(fp)) {
        fprintf(stderr, "%s: error closing file\n", filename);
        return(TRUE);
    }

    return(FALSE);
}


/*-------------------------------------------------------------------*/
static int dump_translation( fp,
			     cable_ptr,
			     neurite_list,
			     neuron_ptr)

	/* Writes to fp a text listing of the translated neuronal
	morphology in *cable_ptr or *neurite_list.  Returns TRUE if 
	error; FALSE otherwise. */

    FILE	      * fp;
    CABLE	      * cable_ptr;
    NEURITE	      * neurite_list;
    NEURON	      * neuron_ptr;


{
				       /*----- functions called -----*/

				       /*----- extern variables -----*/

				       /*----- local  variables -----*/
    int 		error,
			i,
       			num_primary_neurites,
			destination;

    FCBS_ARGS         * fcbs_args_ptr,
		      * fcbs_list_end;
    DX_INTERVAL       * dx_interval_ptr,
		      * dx_list_end;
    DIAM_INTERVAL     * diam_interval_ptr,
		      * diam_list_end;

    NEURITE	      * neurite_ptr,
		      * neurite_list_end;
    SECTION	      * section_ptr,
		      * section_list_end;
    XYZD	      * xyzd_ptr,
		      * xyzd_list_end;
				       /*----- start function -------*/

    num_primary_neurites = neuron_ptr->num_primary_neurites;
    destination = neuron_ptr->destination;


    if (destination == O_NEURON) {
				      /* dump the NEURON translation */

	fprintf(fp, "\n\n\nTranslation to NEURON syntax:\n\n");
	fprintf(fp, "    section  parent  parent_x  length");
	fprintf(fp, "       x       y       z    diam\n");

	fprintf(fp, "\nsoma:\n\n");
	fprintf(fp, 
	"%37.5g\n",
	neuron_ptr->soma_length );

	xyzd_list_end = neuron_ptr->soma_3D_list +
			neuron_ptr->num_soma_3D_points;

	for( xyzd_ptr = neuron_ptr->soma_3D_list;
	     xyzd_ptr < xyzd_list_end;
	     xyzd_ptr++ ) {

	    fprintf(fp, 
	    "%45.4g %7.4g %7.4g %7.4g\n",
	    xyzd_ptr->x,
	    xyzd_ptr->y,
	    xyzd_ptr->z,
	    xyzd_ptr->diam);
	}

	neurite_list_end = neurite_list +
			    num_primary_neurites;

	for ( neurite_ptr = neurite_list;
	      neurite_ptr < neurite_list_end;
	      neurite_ptr++ ) {

	    fprintf(fp, "\n%s:\n\n", neurite_ptr->name);

	    section_list_end = neurite_ptr->section_list +
			       neurite_ptr->num_sections;

	    for ( section_ptr = neurite_ptr->section_list, i=0;
		  section_ptr < section_list_end;
		  section_ptr++, i++ ) {

		fprintf(fp, 
		"    %7d %7d %9.6g %7.5g\n",
		i,
		section_ptr->parent_index,
		section_ptr->origin_in_parent,
		section_ptr->length );


		xyzd_list_end = section_ptr->xyzd_list +
				section_ptr->num_xyzd_points;

		for( xyzd_ptr = section_ptr->xyzd_list;
		     xyzd_ptr < xyzd_list_end;
		     xyzd_ptr++ ) {

		    fprintf(fp, 
		    "%45.4g %7.4g %7.4g %7.4g\n",
		    xyzd_ptr->x,
		    xyzd_ptr->y,
		    xyzd_ptr->z,
		    xyzd_ptr->diam);
		}
	    }
	}
	fprintf(fp,"\n\n");
    }


				       /* dump the CABLE translation */

    else if (destination==O_CABLE
	     ||
	     destination==O_MULTI) {

	fprintf(fp, "\n\n\nTranslation to CABLE syntax:\n\n");

	fcbs_list_end = cable_ptr->fcbs_args_list + 
			cable_ptr->num_fcbs_args;

	fprintf(fp, "\n\nfcbs args:\n\n");
	fprintf(fp, "   first    last    root\n");
	fprintf(fp, "   index   index   index\n");
	fprintf(fp, "   -----   -----   -----\n");

	for (fcbs_args_ptr = cable_ptr->fcbs_args_list;
	     fcbs_args_ptr < fcbs_list_end;
	     fcbs_args_ptr++) {
	    fprintf(fp, "%8d %7d %7d\n", 
	    fcbs_args_ptr->first_index,
	    fcbs_args_ptr->last_index,
	    fcbs_args_ptr->root_index);
	}

	dx_list_end = cable_ptr->dx_interval_list + 
			cable_ptr->num_dx_intervals;

	fprintf(fp, "\ndx info:\n\n");
	fprintf(fp, "   first    last\n");
	fprintf(fp, "   index   index      dx  \n");
	fprintf(fp, "   -----   -----   -----\n");

	for (dx_interval_ptr = cable_ptr->dx_interval_list;
	     dx_interval_ptr < dx_list_end;
	     dx_interval_ptr++) {
	    fprintf(fp, "%8d %7d %7g\n", 
	    dx_interval_ptr->first_index,
	    dx_interval_ptr->last_index,
	    dx_interval_ptr->dx);
	}

	diam_list_end = cable_ptr->diam_interval_list + 
			cable_ptr->num_diam_intervals;

	fprintf(fp, "\ndiam info:\n\n");
	fprintf(fp, "   first    last   first    last\n");
	fprintf(fp, "   index   index    diam    diam\n");
	fprintf(fp, "   -----   -----   -----   -----\n");

	for (diam_interval_ptr = cable_ptr->diam_interval_list;
	     diam_interval_ptr < diam_list_end;
	     diam_interval_ptr++) {
	    fprintf(fp, "%8d %7d %7g %7g\n", 
	    diam_interval_ptr->first_index,
	    diam_interval_ptr->last_index,
	    diam_interval_ptr->first_diam,
	    diam_interval_ptr->last_diam);
	}
    }

    if (error=ferror(fp))
	fprintf(stderr, 
	"error writing translation to verification file\n");

    return(error);
}

/*-------------------------------------------------------------------*/
static int dump_diagrams(fp, diagram_list)

	/* Writes to fp a text listing of the schematic diagrams in
	diagram_list.  Returns TRUE if error; FALSE otherwise. */

    FILE      * fp;
    DIAGRAM   * diagram_list;

{
				       /*----- functions called -----*/

				       /*----- extern variables -----*/

				       /*----- local  variables -----*/
    int       	i,
		error;
    SEGMENT   * segment_ptr;
    BRANCH    * branch_ptr;
    DIAGRAM   * diagram_ptr;

				       /*----- start function -------*/
    for ( diagram_ptr = diagram_list;
	  diagram_ptr;
	  diagram_ptr = diagram_ptr->next ) {

        fprintf(fp,
	"\n\nschematic diagram from %s\n", 
	(diagram_ptr->from_input) ? 
	"original digitized data" : "translated data" );

        fprintf(fp,"%s\n", diagram_ptr->description);
        fprintf(fp,
	"x: %g-%g um  y: %d-%d  diameters: %g-%g um\n", 
	diagram_ptr->x_min,
	diagram_ptr->x_max,
	diagram_ptr->y_min,
	diagram_ptr->y_max,
	diagram_ptr->diam_min,
	diagram_ptr->diam_max);

        fprintf(fp,
	"most distal branch: %s\n",
	diagram_ptr->branch_list[diagram_ptr->x_max_branch].label);

        fprintf(fp,
	"labels %s\n", 
	(diagram_ptr->show_labels) ? "displayed" : "suppressed" );

        fprintf(fp,
	"diameters %s\n", 
	(diagram_ptr->show_diameters) ? "displayed" : "suppressed" );

        fprintf(fp,
	"segment separation = %d laser printer pixels\n", 
	(diagram_ptr->segment_separation < 0) ?
	DEF_SEGMENT_SEPARATION : diagram_ptr->segment_separation );

        fprintf(fp,
	"branch separation = %g * diam_max (diam_max = %g%s)\n", 
	(diagram_ptr->branch_separation <= 0.0) ?
	DEF_BRANCH_SEPARATION : diagram_ptr->branch_separation,
	diagram_ptr->diam_max,
	(diagram_ptr->show_diameters) ? " microns" : "" );

        fprintf(fp,
	"scalebar length = %g microns\n", 
	(diagram_ptr->scalebar_length <= 0.0) ?
	DEF_SCALEBAR_LENGTH : diagram_ptr->scalebar_length);

        fprintf(fp,
	"label height = %g * branch separation\n", 
	(diagram_ptr->label_height <= 0.0) ?
	DEF_LABEL_HEIGHT : diagram_ptr->label_height);

        fprintf(fp,
	"description height = %g * vertical extent of viewport\n", 
	(diagram_ptr->description_height <= 0.0) ?
	DEF_DESCRIPTION_HEIGHT : diagram_ptr->description_height);

        fprintf(fp,
	"max tag size = %g * branch separation\n", 
	(diagram_ptr->max_tag_size <= 0.0) ?
	DEF_MAX_TAG_SIZE : diagram_ptr->max_tag_size);

        fprintf(fp,
	"separation of overlapping tags = %g * tag_size\n", 
	diagram_ptr->tag_separation);

        fprintf(fp,
	"connection width = %g * diam_min\n", 
	(diagram_ptr->connection_width <= 0.0) ?
	DEF_CONNECTION_WIDTH : diagram_ptr->connection_width);

        fprintf(fp, "plotting destinations:");

	if (!(diagram_ptr->destinations))
            fprintf(fp, " (none)");

	if (diagram_ptr->destinations & FIG)
            fprintf(fp, " fig");

	if (diagram_ptr->destinations & GRAPH)
            fprintf(fp, " graph");

        fprintf(fp, "\n\n");

        fprintf(fp,"%d segments:\n\n", diagram_ptr->num_segments);
        fprintf(fp,
        "   index       dx     diam  x_start  y_start filled     size  shape   open     size  shape\n");

        for ( segment_ptr = diagram_ptr->segment_list,
	      i=0;

	      i < diagram_ptr->num_segments;

	      segment_ptr++,
	      i++) {

    	    fprintf(fp, "%8d %8g %8g %8g %8d", 
		        i,
		        segment_ptr->dx, 
		        segment_ptr->diam,
		        segment_ptr->x_start,
		        segment_ptr->y_start);

	    if (segment_ptr->num_filled_tags) {
    	        fprintf(fp, " %6d %8g %6d", 
		             segment_ptr->num_filled_tags, 
		             segment_ptr->filled_tag_size, 
		             segment_ptr->filled_tag_shape );
	    }
	    if (segment_ptr->num_open_tags) {
	        if (!(segment_ptr->num_filled_tags))
    	            fprintf(fp, "%23s", "" );
    	        fprintf(fp, " %6d %8g %6d", 
		             segment_ptr->num_open_tags, 
		             segment_ptr->open_tag_size, 
		             segment_ptr->open_tag_shape );
	    }
    	    fprintf(fp, "\n");
        }

        fprintf(fp,"\n%d branches:\n\n", diagram_ptr->num_branches);

        fprintf(fp,
        "   first     last     root    con_L    con_x        y    lbl_x label\n");
        for ( branch_ptr = diagram_ptr->branch_list, 
	      i=0;

	      i < diagram_ptr->num_branches;

	      branch_ptr++,
	      i++) {

    	    fprintf(fp, "%8d %8d %8d %8d %8g %8d %8g%s\n", 
		        branch_ptr->first_index, 
		        branch_ptr->last_index,
		        branch_ptr->root_index,
		        branch_ptr->connection_length,
		        branch_ptr->connection_x,
		        branch_ptr->y,
		        branch_ptr->label_x,
		        branch_ptr->label);
        }

    }

    if (error=ferror(fp))
	fprintf(stderr, 
	"error writing diagrams to verification file\n");

    return(error);
}




/*-------------------------------------------------------------------*/
static int dump_neuron( fp, neuron_ptr ) 

	/* Writes to fp a text listing of the neuron in *neuron_ptr. 
        Neurites are numbered sequentially, starting with 1, in the 
	order in which they were read from the input file.  As used 
	here, a single "neurite" is a primary neurite, rooted at the 
	soma, and all higher-order branches thereof.  Returns TRUE if 
	error; FALSE otherwise. */


    FILE       * fp;
    NEURON     * neuron_ptr;

{
				       /*----- functions called -----*/

				       /*----- extern variables -----*/
    extern char	version[];
				       /*----- local  variables -----*/
    int		error,
		i,
    		neurite_number;

    TREE_POINT        * tree_ptr;
    SOMA_POINT        * soma_ptr;
				       /*----- start function -------*/

    write_neuron_description(fp, neuron_ptr);

    if (!(neuron_ptr->soma_outline)) {
        fprintf(fp,"\n(no soma outline)\n");
    }
    else {
        fprintf(fp,"\nsoma outline:\n\n");
        fprintf(fp," pt num        x            y            z\n");
        fprintf(fp,"--------  -----------  -----------  -----------\n");

	soma_ptr = neuron_ptr->soma_outline;
	     
	do {
	    fprintf(fp, "%8d %12.6g %12.6g %12.6g\n",
	            soma_ptr->point_number,
	            soma_ptr->x,
	            soma_ptr->y,
	            soma_ptr->z );

	    soma_ptr = soma_ptr->next;
	} while (soma_ptr != neuron_ptr->soma_outline);
    }

    if (!(neuron_ptr->num_soma_3D_points)) {
        fprintf(fp,"\n(no soma 3D points)\n");
    }
    else {
        fprintf(fp,"\nsoma points:\n\n");
        fprintf(fp,
	"     x            y            z          diam\n");
        fprintf(fp,
	"-----------  -----------  -----------  -----------\n");

	for (i=0; i< neuron_ptr->num_soma_3D_points; i++) {

	    fprintf(fp, "%11.6g %12.6g %12.6g %12.6g\n",
	            neuron_ptr->soma_3D_list[i].x,
	            neuron_ptr->soma_3D_list[i].y,
	            neuron_ptr->soma_3D_list[i].z,
	            neuron_ptr->soma_3D_list[i].diam);
	}
    }

    fprintf(fp,
    "\nlisting format:\n\n    %s\n\n%s\n",
    "point_number cable_index diam length [-> branch point_number]",
    "(indentation indicates branching hierarchy)" );

    for( tree_ptr = neuron_ptr->neurites,
	 neurite_number = 1;
	 tree_ptr && tree_ptr->next;
	 neurite_number++,
	 tree_ptr = tree_ptr->branch ) {

	fprintf(fp, "\nneurite %d:\n", neurite_number);

        if (error = dump_tree_point( fp, tree_ptr, indent_step ))
	    return(error);

	if (error = dump_neurite( fp, tree_ptr->next, indent_step))
	    return(error);

    }

    return(error);
}				       /*----- end function ---------*/


/*-------------------------------------------------------------------*/
static int dump_neurite( fp, root_ptr, indent )

	/* Dumps to fp a text listing of the branch specified by 
	*root_ptr.  Recursively does the same for all higher 
	order branches connected to this one.  indent specifies the
	number of spaces by which the text line will be indented.  
	Returns TRUE if error; FALSE otherwise. */

    FILE	   * fp;
    TREE_POINT     * root_ptr;
    int		     indent;

{
				       /*----- functions called -----*/

				       /*----- extern variables -----*/
    extern int indent_step;
				       /*----- local  variables -----*/
    int		error;
    TREE_POINT *tree_ptr,
	       *most_distal_point;
				       /*----- start function -------*/

    for( tree_ptr = root_ptr;
	 tree_ptr;
	 tree_ptr = tree_ptr->next ) {

        if (error = dump_tree_point( fp, tree_ptr, indent ))
	    return(error);

	most_distal_point = tree_ptr;
    }

    fprintf(fp, "\n");

			       /* do connecting branches recursively */
    for ( tree_ptr = most_distal_point;
	  (tree_ptr->next) != root_ptr
	  &&
	  (tree_ptr->branch) != root_ptr;
	  tree_ptr = tree_ptr->previous ) {

	if (tree_ptr->branch) {
            if (error = dump_neurite( fp, 
				      tree_ptr->branch, 
				      indent + indent_step ))
	        return(error);
    	}
    
    }

    return(error);

}				       /*----- end function ---------*/




/*-------------------------------------------------------------------*/
static int dump_tree_point( fp, tree_ptr, indent )

	/* Dumps to fp a text listing of the tree_point specified by 
	*tree_ptr.  indent specifies the number of spaces by which the 
	text line will be indented.  Writes error message and returns 
	TRUE if error; FALSE otherwise. */

    FILE	   * fp;
    TREE_POINT     * tree_ptr;
    int		     indent;

{
				       /*----- functions called -----*/

				       /*----- extern variables -----*/

				       /*----- local  variables -----*/
    int		i, error;
				       /*----- start function -------*/

    for( i = 0; i < indent; i++ )
	fprintf(fp, " ");

    fprintf(fp, "%8d %7d %7g %7g",
	 tree_ptr->point_number,
	 tree_ptr->cable_index,
	 tree_ptr->diam,
	 tree_ptr->length );

    if (tree_ptr->branch)
    	fprintf(fp, " -> %d", tree_ptr->branch->point_number);

    if (tree_ptr->previous)
      {fprintf(fp, " <- %d", tree_ptr->previous->point_number);
       fprintf(fp, " - %g", tree_ptr->previous->section_x);
       fprintf(fp, " - %s", tree_ptr->previous->section_name);
     }

    fprintf(fp, "\n");

    if (error=ferror(fp))
	fprintf(stderr, 
	"error writing digitized data to verification file\n");

    return(error);

}				       /*----- end function ---------*/
