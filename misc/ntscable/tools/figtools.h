
	/*-------------------------------------------------------------
	|							      |
	|	PROGRAM:  figtools				      |
	|							      |
	|	MODULE:	  %M%   				      |
	|							      |
	|	MACHINE:  Sun 3/60         		      |
	|							      |
	|	STARTED:  10-SEP-90	   BY:	J.C. Wathey	      |
	|							      |
	|	REVISED:  %G%	   BY:	JCW		      |
	|							      |
	|	STATUS:	     incomplete or untested		      |
	|		     compiles; partly tested		      |
	|		  -> runs; revisions in progress	      |
	|		     runs; stable version		      |
	|							      |
	|	CONTAINS: global definitions and declarations for     |
	|		  figtools routines			      |
	|							      |
	-------------------------------------------------------------*/


/*--------------------------------- GLOBAL DEFINITIONS --------------*/

						    /* point shapes */
#define	CIRCLE		1
						    /* point styles */
#define	OPEN		0
#define	FILLED		1

						     /* line styles */
#define SOLID_LINE	0
#define DASHED_LINE	1
#define DOTTED_LINE	2
#define LAST_STYLE	DOTTED_LINE

				/* tic and label positions for axes */
#define	CLOSER_SIDE	0
#define	LEFT	       -1
#define	RIGHT		1
#define	BELOW	       -1
#define	ABOVE		1
					      /* text justification */
#define LEFT_JUSTIFY	0
#define CENTER		1
#define RIGHT_JUSTIFY	2

                    /* The fonts currently recognized by f2ps are:
			0  default (Helvetica)
			1  Times-Roman
			2  Times-Bold
			3  Times-Italic
			4  Helvetica
			5  Courier	*/

#define TIMES_ROMAN	1
#define TIMES_BOLD	2
#define TIMES_ITALIC	3
#define HELVETICA	4
#define COURIER		5
#define	MAXFONT		5
							/* in pitch */

/*-------------------------------- GLOBAL DECLARATIONS --------------*/

void 	window(),
     	viewport(),
	init_figtools(),
     	axes(),
     	x_axis(),
     	y_axis(),
     	write_comment(),
     	start_compound(),
     	end_compound(),
	set_font(),
     	set_linewidth_in_x(),
     	set_linewidth_in_y(),
     	set_line_style(),
     	line_style_key(),
     	draw_line_style_key(),
	legend(),
     	set_dash_in_x(),
     	set_dash_in_y(),
     	move(),
     	draw(),
     	rmove(),
     	rdraw(),
	mark_point_in_x(),
	mark_point_in_y(),
     	write_text();

int  	open_fig_file(),
     	close_fig_file(),
     	set_charsize_in_pitch(),
     	set_charsize_in_x(),
     	set_charsize_in_y();
     	set_charsize_in_xrange(),
     	set_charsize_in_yrange();

double	char_width_in_x(),
    	char_width_in_y(),
	y_to_x(),
	x_to_y(),
    	laser_pixel_in_x(),
    	laser_pixel_in_y(),
        nice_interval();
