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
	|	CONTAINS: routines for producing vector graphics and  |
	|		  text in fig format                          |
	|							      |
	-------------------------------------------------------------*/


/*--------------------------------- GLOBAL DEFINITIONS --------------*/
#include <stdio.h>
#include <math.h>
#include "tf.h"
#include "jwmath.h"
#include "filtools.h"
#include "figtools.h"

#define DEF_NUM_DIVISIONS	4
#define DEF_AXIS_LABEL_FONTSIZE 0.05
#define TIC_LABEL_GAP		0.2
#define LABEL_DESCRIPTION_GAP	0.5
#define LEGEND_GAP		0.8
#define LINE_STYLE_KEY_GAP	0.5
#define DEF_LINE_STYLE_KEY_LENGTH	0.1
#define	DEF_STYLE_KEY_FONTSIZE	(DEF_AXIS_LABEL_FONTSIZE * 0.8)
#define DEF_DESCRIPTION_PLACEMENT	CENTER

#define Y_INVERTED	TRUE

#define SPACES_FOR_NUM_OBJECTS	20
#define SPACES_FOR_BOUNDING_BOX	30

				    /* fraction of x or y data range */
#define DEF_POINT_SIZE		0.005

#define POINTS_PER_ELLIPSE	12
					   /* fraction of point size */
#define POINT_LINE_WIDTH	0.1
#define DEF_POINT_SEPARATION	0.5

		       /* maximum depth for nesting compound objects */
#define STACK_SIZE	50

					    /* fig coordinate ranges */
#define FIG_X_MIN	0.0
#define FIG_X_MAX	800.0
#define FIG_Y_MIN	0.0
#define FIG_Y_MAX	800.0

#define PIX_PER_INCH	80
#define PITCH_PER_INCH	72
#define PIX_PER_PITCH	((double) PIX_PER_INCH/(double) PITCH_PER_INCH)

#define LASER_PIX_PER_INCH	300
#define FIG_PIX_PER_LASER_PIX ((double)PIX_PER_INCH/LASER_PIX_PER_INCH)

				      /* num spaces per indentation */
#define INDENT 4
						   /* in fig pixels */
#define DEF_LINE_WIDTH	1.0

#define DEF_FONT		HELVETICA
#define DEF_FONT_SIZE		12
#define SMALLEST_READABLE_FONT	3
#define CHAR_WIDTH_OVER_HEIGHT	0.64
#define CHAR_HALF		0.4
#define CHAR_HEIGHT_ON_SCREEN	16
#define CHAR_WIDTH_ON_SCREEN	8

#define MOVE	0
#define DRAW	1


#define give_warning(message) \
	if (verbose) \
	    fprintf(stderr,"%s%s\n", figtools_warning, message)


#define x_inch_to_fig(x) \
	(high_res ? (rint(10.0*((x) * PIX_PER_INCH + FIG_X_MIN))/10.0) : \
		    rint((x) * PIX_PER_INCH + FIG_X_MIN))

#if Y_INVERTED
#define y_inch_to_fig(y) (FIG_Y_MAX - \
	high_res ? (rint(10.0*((y) * PIX_PER_INCH + FIG_Y_MIN))/10.0) : \
	 	    rint((y) * PIX_PER_INCH + FIG_Y_MIN))
#else
#define y_inch_to_fig(y) \
	(high_res ? (rint(10.0*((y) * PIX_PER_INCH + FIG_Y_MIN))/10.0) : \
	 	    rint((y) * PIX_PER_INCH + FIG_Y_MIN))
#endif
		/* Returns TRUE if the passed (x,y) lies within or 
		on the window borders; otherwise returns FALSE. */

#define within_window(x, y) \
    (!((x)<windXmin || (x)>windXmax || (y)<windYmin || (y)>windYmax ))

#define check_err() if (error) return;

#define check_wr_err() \
    if (error) return;\
    if (!fp) {\
        set_error( file_closed_msg );\
	return;\
    }

#define update_bounding_box() \
    if (xmin > figCurrentX) xmin = figCurrentX; \
    if (xmax < figCurrentX) xmax = figCurrentX; \
    if (ymin > figCurrentY) ymin = figCurrentY; \
    if (ymax < figCurrentY) ymax = figCurrentY;

typedef struct compound {
    int		num_objects;
    long	num_obj_offset;
    double	xmin, 
		xmax,
  		ymin, 
		ymax;
    long	box_offset;
} COMPOUND;

typedef struct line_style_record {
    char      * description;
    int		line_style;
    double	dash_or_dot_interval,
		line_width;
} LINE_STYLE_RECORD;

/*-------------------------------- GLOBAL DECLARATIONS --------------*/

#if Y_INVERTED
static char	fig_preamble[] = "#FIG 1.4-TFX\n80 2\n";
#else
static char	fig_preamble[] = "#FIG 1.4-TFX\n80 1\n";
#endif

static char	file_closed_msg[] = "tried to write with file closed",
		figtools_warning[] = "figtools warning: ",
		default_filename[] = "figtools.out",
		line_postamble[] = "9999 9999\n",
		text_postamble[] = "\1\n",
		compound_postamble[] = "-6\n";

static int	error        = FALSE,
            	drawing_line = FALSE,
		verbose	     = TRUE,
    		high_res     = FALSE;	/* set to false DKS */

		     /* viewport boundaries in fig coordinate space */
static double	viewXmin = FIG_X_MIN, 
		viewXmax = FIG_X_MAX,
  		viewYmin = FIG_Y_MIN, 
		viewYmax = FIG_Y_MAX;

				 /* window boundaries in data space */
static double	windXmin = FIG_X_MIN, 
		windXmax = FIG_X_MAX, 
		windYmin = FIG_Y_MIN, 
		windYmax = FIG_Y_MAX; 

				 /* scale factors relating fig      */
                                 /*  coordinate space to data space */
static double	Xscale = 1.0,
		Yscale = 1.0; 

			        /* current pen position in data space */
static double	currentX = FIG_X_MIN, 
		currentY = FIG_Y_MIN;
				 /* current pen position in fig space */
static double	figCurrentX = 0.0,
		figCurrentY = 0.0;

			      /* default location of line style key */
static double	x_line_style_key = FIG_X_MIN,
             	y_line_style_key = FIG_Y_MIN;

static int	clipped = FALSE;

static int	line_style = SOLID_LINE;
static double	dash_or_dot_interval = 0.0,
		line_width = DEF_LINE_WIDTH;

	   			   /* in pitch (1/72 inch per unit) */
static int	font_size = DEF_FONT_SIZE,
		font      = DEF_FONT,
		justify   = LEFT_JUSTIFY;

static double	text_angle_in_radians = 0.0;
static int	legend_font = DEF_FONT;
static double	legend_fontsize_as_fraction_of_yrange 
		= DEF_STYLE_KEY_FONTSIZE;
static char   * legend_text = (char *) NULL;
static char   * filename = (char *) NULL;

static int	num_objects = 0;
static long	num_obj_offset;

	     /* compound object bounding box in fig coordinate space */

static double	xmin = FIG_X_MAX, 
		xmax = FIG_X_MIN,
  		ymin = FIG_Y_MAX, 
		ymax = FIG_Y_MIN;

static long	box_offset;

static FILE   * fp = (FILE *) NULL;

static int	  	   num_line_style_records = 0;
static LINE_STYLE_RECORD * line_style_record_list = 
      (LINE_STYLE_RECORD *) NULL;

static COMPOUND   compound_stack[ STACK_SIZE ];
static COMPOUND * compound_stack_ptr = compound_stack;
static int	  depth = 0;



	/*=============================================================
				PRIVATE ROUTINES
	=============================================================*/


/*-------------------------------------------------------------------*/
static void push_compound( depth_ptr, compound_ptr )

	/* Pushes *compound_ptr onto the stack.  Increments *depth_ptr
	and the extern compound_stack_ptr.  Writes error msg and 
	sets extern error flag TRUE if stack overflow occurs. */

    int	      * depth_ptr;
    COMPOUND  * compound_ptr;

{
				       /*----- functions called -----*/

				       /*----- extern variables -----*/
    extern COMPOUND   compound_stack[],
		    * compound_stack_ptr;

				       /*----- local  variables -----*/
				       /*----- start function -------*/

    if (error = (compound_stack_ptr == compound_stack + STACK_SIZE))
	fprintf(stderr,
	"figtools: compound objects nested too deeply\n");
    else {
	memcpy( (char *) compound_stack_ptr,
	        (char *) compound_ptr,
		sizeof( COMPOUND ) );
	(*depth_ptr)++;
	compound_stack_ptr++;
    }
}

/*-------------------------------------------------------------------*/
static void pop_compound( depth_ptr, compound_ptr )

	/* Pops COMPOUND on top of stack to *compound_ptr.  Decrements
	*depth_ptr and the extern compound_stack_ptr.  Writes error 
	msg and sets extern error flag TRUE if stack underflow occurs.
	*/

    int	      * depth_ptr;
    COMPOUND  * compound_ptr;

{
				       /*----- functions called -----*/

				       /*----- extern variables -----*/
    extern COMPOUND   compound_stack[],
		    * compound_stack_ptr;

				       /*----- local  variables -----*/
				       /*----- start function -------*/

    if (error = (compound_stack_ptr == compound_stack))
	fprintf(stderr,
	"figtools: end_compound() without matching start_compound()\n");
    else {
	(*depth_ptr)--;
	compound_stack_ptr--;
	memcpy( (char *) compound_ptr,
	        (char *) compound_stack_ptr,
		sizeof( COMPOUND ) );
    }
}


/*-------------------------------------------------------------------*/
static void finish_compounds()

			  /* Completes any pending compound objects. */

{
    if (depth) {
	give_warning("completing unfinished compound objects");
	while (depth && !error)
	    end_compound();
    }
}


/*-------------------------------------------------------------------*/
static void indent()
					 /* indents line in fig file */
{

				       /*----- extern variables -----*/
    extern FILE * fp;
    extern int    depth,
		  error;
				       /*----- local  variables -----*/
    int		i;

				       /*----- start function -------*/
    for (i=0; i<INDENT*depth; i++) {
	if (error = (putc(' ', fp) == EOF))
	    break;
    }

}

/*-------------------------------------------------------------------*/
static void line_indent()

	/* indents line containing polyline coordinates in fig file */
{

				       /*----- extern variables -----*/
    extern FILE * fp;
    extern int    depth,
		  error;
				       /*----- local  variables -----*/
    int		i;

				       /*----- start function -------*/
    for (i=0; i<INDENT*(depth+1); i++) {
	if (error = (putc(' ', fp) == EOF))
	    break;
    }

}


/*-------------------------------------------------------------------*/
static void finish_line()

	/* If a polyline object is being drawn, this routine completes
	that object by writing line_postamble to the fig file.  Clears
	global flag drawing_line and increments the object count. */
{
    if (drawing_line) {
	line_indent();
	fprintf(fp, line_postamble);
	drawing_line = FALSE;
	num_objects++;
    }
}


/*-----------------------------------------------------------------*/
static void  set_error(errormsg)

	 /* Writes error message and sets extern int "error" TRUE. */

    char      * errormsg;

{
				 /*------- functions called -------*/
				 /*------- extern variables -------*/
    extern int error;
				 /*------- local  variables -------*/
				 /*------- start function   -------*/
    fprintf( stderr, "figtools: %s\n", errormsg );
    error = TRUE;
}

/*-------------------------------------------------------------------*/
static void map_to_fig( x,  y, x_ptr, y_ptr )

	/* Maps x, y in data space to x_ptr, y_ptr in fig screen 
	pixels space. */

     double 	x,
		y;
     double   * x_ptr,
     	      * y_ptr;

{
				       /*----- functions called -----*/
				       /*----- extern variables -----*/
    extern int	high_res;
				       /*----- local  variables -----*/
				       /*----- start function -------*/
    *x_ptr  = Xscale * (x - windXmin) + viewXmin;
    *y_ptr  = Yscale * (y - windYmin) + viewYmin;
#if Y_INVERTED
    *y_ptr  = FIG_Y_MAX - *y_ptr;
#endif

    if (high_res) {
	*x_ptr  = rint( (*x_ptr) * 10 )/10.0;
	*y_ptr  = rint( (*y_ptr) * 10 )/10.0;
    }
    else {
	*x_ptr  = rint( *x_ptr );
	*y_ptr  = rint( *y_ptr );
   }
}


/*-----------------------------------------------------------------*/
static void  setScales()

        /* Calculates scaling factors relating data space to fig 
        space.  Relationship is: 
        (X plotter units) = (X data units) * Xscale
        (similarly for Y). */

				 /*------- functions called -------*/
				 /*------- extern variables -------*/
				 /*------- local  variables -------*/
				 /*------- start function   -------*/

{
    Xscale  = (viewXmax-viewXmin)/(windXmax-windXmin);
    Yscale  = (viewYmax-viewYmin)/(windYmax-windYmin);
    move(windXmin,windYmin);
    x_line_style_key = windXmin;
    y_line_style_key = windYmin; 
}


/*-------------------------------------------------------------------*/
static char * fig_xy_string( x,  y )

	/* Formats x, y in fig screen space as an ascii string in fig 
        file syntax. */

     double 	x,
		y;
{
				       /*----- functions called -----*/
				       /*----- extern variables -----*/
    extern int	high_res;
				       /*----- local  variables -----*/
    static char	xy_string[50];

				       /*----- start function -------*/

    printf("putting out string %s from %d %d %f %f hr %d\n",xy_string,
	   (int)x,(int)y,x,y,high_res);
    if (high_res)
	sprintf( xy_string, "%.1f %.1f", x, y );
    else
	sprintf( xy_string, "%d %d", (int) x, (int) y );

    return(xy_string);
}


/*-------------------------------------------------------------------*/
static void findCrossPoints( newX, 
			     newY, 
			     newIn,
			     currentIn, 
			     Xcross, 
			     Ycross, 
			     crossesFound_ptr )

        /* This routine finds zero, one or two points at which the line 
        segment from (currentX, currentY) to (newX, newY) crosses the 
        window borders.  Returns the number of crosses found in 
        *crossesFound_ptr.  If only one is found, it is returned in 
        Xcross[0], Ycross[0].  If there are two cross points, the one 
        closest to (currentX, currentY) is returned in Xcross[0], 
        Ycross[0], the other in Xcross[1], Ycross[1].  The algorithm 
        assumes that the current and new points are different, and that 
        one or both of them lie outside the window borders. */

    double	newX, 
	   	newY,
		Xcross[], 
		Ycross[]; 
    int		newIn,
		currentIn,
	      * crossesFound_ptr;

{

				       /*----- functions called -----*/
				       /*----- extern variables -----*/
    extern double	currentX,
			currentY,
			windXmax,
			windYmin,
			windXmin,
			windYmax;
				       /*----- local  variables -----*/

    int		crossesFound;

                /* slope, intercept for line segment current -> new */
    double	m, 
		b;

                                /* storage for possible cross point */
    double	PcrossX, 
		PcrossY;

                        /* endpoints of line segment current -> new */
    double	new[2],
		current[2];

                          /* x vs y indices: if 0, xy->x and yx->y; */
                          /*                 if 1, xy->y and yx->x  */
    int		xy, 
		yx;

                                                  /* window borders */
    double	border[4];

                         /* border index: 0->windXmax, 1->windYmin, */
                         /*               2->windXmin, 3->windYmax  */
    int		bdr;

                       /* directions from line segment              */
                       /* current -> new to endpts of tested border */
    double	dir1, 
		dir2;

				       /*----- start function -------*/


    current[0]  = currentX;            /* initialize arrays for loop */
    current[1]  = currentY;
    new[0]      = newX;
    new[1]      = newY;
    border[0]   = windXmax;
    border[1]   = windYmin;
    border[2]   = windXmin;
    border[3]   = windYmax;

    crossesFound  = 0;             /* initialize counter and indices */
    bdr           = 0;
    xy            = 0;
    yx            = 1;

    for ( crossesFound = 0,
	  bdr = 0,
	  xy = 0,
	  yx = 1;

	  crossesFound < 2
	  &&
	  bdr < 4;

	  bdr++,
	  xy = (xy+1) % 2,
	  yx = (yx+1) % 2 ) {

			  /* Can't cross if parallel to border or if 
			  both endpoints are on same side of border. */

	if ( (current[xy] == new[xy])
    	     ||
	     (((current[xy]-border[bdr]) * (new[xy]-border[bdr])) > 0) )
	    continue;

					   /* calc slope & intercept */
	b  = new[xy]-current[xy];
	m  = (new[yx]-current[yx])/b;
	b  = (new[xy]*current[yx]-new[yx]*current[xy])/b;

		/* Each dir is the numerator of the  expression for the
		shortest distance from the border endpoint to the line
		containing new and current.  If dir1 and dir2 have the
		same sign, then both border endpoints lie on the same 
		side of this line, and the two segments cannot cross.
		The equivalent test was done above (at the ||) for the
		new and current endpoints and the line through the
		border. */

	dir2  = m * border[bdr] + b;                /* partial calc  */
	dir1  = dir2 - border[(bdr+3) % 4];
  	dir2  = dir2 - border[(bdr+1) % 4];

	if ( dir1 * dir2 > 0 )
	    continue;

	PcrossX  = border[bdr];              /* possible cross point */
	PcrossY  = m * PcrossX + b;

	if ( odd(bdr) ) {             /* unreverse x, y if necessary */
	    b        = PcrossY;
	    PcrossY  = PcrossX;
	    PcrossX  = b;
    	}
				/* If cross equals  current 
				or if this cross has already been found
				then discard the cross */

	if ( (PcrossX==currentX && PcrossY==currentY)
	     ||
	     ( crossesFound == 1
	       &&
	       PcrossX == Xcross[0]
	       &&
	       PcrossY == Ycross[0] ) )

	    continue; 

				/* A new cross point has been found. */

	crossesFound++;
	Xcross[ crossesFound-1 ]  = PcrossX;
	Ycross[ crossesFound-1 ]  = PcrossY;

    }  /* end search loop */

				/* If segment is outside the window and
				just touches one corner of the window
				then reject the cross.  */

    if ((! newIn) &&  (! currentIn) && (crossesFound == 1))  
    	crossesFound = 0; 
			      /* If there are two cross points, then
			      find which is closer to current point. */
    if (crossesFound == 2) {

				/* now dir1, dir2 will be the Manhattan
				norms from current to crosses  */

	dir1  = fabs(Xcross[0]-currentX) + fabs(Ycross[0]-currentY);
    	dir2  = fabs(Xcross[1]-currentX) + fabs(Ycross[1]-currentY);

			       /* exchange cross points if necessary */

	if (dir1 > dir2) {
	    PcrossX    = Xcross[0];
	    PcrossY    = Ycross[0];
	    Xcross[0]  = Xcross[1];
	    Ycross[0]  = Ycross[1];
	    Xcross[1]  = PcrossX;
	    Ycross[1]  = PcrossY;
	}  
				/* If second cross point == new point
				then reject the second cross point.  */

	if ((Xcross[1] == newX) &&  (Ycross[1] == newY))  
	    crossesFound = 1;

    }  /* if 2 found */

    *crossesFound_ptr = crossesFound;

}  /* findCrossPoints */


/*-----------------------------------------------------------------*/
static void  fig_move( x, y )

	/* First converts the arguments (in data space) to fig 
        coordinates and executes a "virtual" move by resetting the 
        values of figCurrentX, figCurrentY to the values of the 
        converted arguments.  The pen is not physically moved.  
	fig_draw will make a physical move to this point just prior 
	to the next draw. */

    double	x,
		y;
{
				       /*----- functions called -----*/
				       /*----- extern variables -----*/
    extern double	figCurrentX, 
			figCurrentY;
				       /*----- local  variables -----*/
				       /*----- start function -------*/
    map_to_fig(x, y, &figCurrentX, &figCurrentY);

}


/*-----------------------------------------------------------------*/
static void fig_draw_with_style(x,
				y,
			    	line_style_arg,
				dash_or_dot_interval_arg,
			    	line_width_arg)

	/* First converts the real arguments (data space) to fig 
        coordinates.  If the latter are identical to figCurrentX, 
        figCurrentY then the routine returns without doing anything;
	otherwise it draws to the fig coordinates converted from 
        the arguments.  Ends by setting figCurrentX, figCurrentY to the 
        point just drawn to.  Line style is determined by the last
	3 arguments rather than by the extern line style variables. */

    double	x,
		y,
		dash_or_dot_interval_arg,
		line_width_arg;
    int		line_style_arg;
		

{
				       /*----- functions called -----*/
				       /*----- extern variables -----*/
    extern double	figCurrentX, 
			figCurrentY;
				       /*----- local  variables -----*/
    double		figNewX, 
			figNewY;
				       /*----- start function -------*/

    map_to_fig(x, y, &figNewX, &figNewY);

    if (figNewX != figCurrentX || figNewY != figCurrentY) {

        if (!drawing_line) {
	    indent();
	    fprintf( fp, "%c 1 %d %.3f 0 0 0 0 %.3f 0 0\n",
		    /* (high_res ? '7' : '2'),   no 7 code -- DKS */
		     '2',
		     line_style_arg, 
		     line_width_arg,
		     dash_or_dot_interval_arg );
            line_indent();
	    fprintf( fp, "%s\n",
		     fig_xy_string( figCurrentX, figCurrentY ) );
	    printf("first point: %s %f %f\n",
		   fig_xy_string( figCurrentX, figCurrentY ),
		   figCurrentX, figCurrentY );
            update_bounding_box();
	    drawing_line = TRUE;
        }

        line_indent();
        fprintf( fp, "%s\n", fig_xy_string( figNewX, figNewY ) );
	printf("second point: %s %f %f\n",
		   fig_xy_string( figNewX, figNewY ),
		   figCurrentX, figCurrentY );

        figCurrentX  = figNewX;
        figCurrentY  = figNewY;

        update_bounding_box();

        if (ferror(fp))
	    set_error("i/o error while drawing");
    }
}

/*-----------------------------------------------------------------*/
static void fig_draw(x, y)

	/* First converts the real arguments (data space) to fig 
        coordinates and executes a physical move to figCurrentX, 
        figCurrentY; then draws to the fig coordinates converted from 
        the arguments.  Ends by setting figCurrentX, figCurrentY to the 
        point just drawn to.  */

    double	x,
		y;


{
				       /*----- functions called -----*/
				       /*----- extern variables -----*/
    extern double	figCurrentX, 
			figCurrentY,
			dash_or_dot_interval,
			line_width;
    extern int		line_style;
				       /*----- local  variables -----*/
				       /*----- start function -------*/

    fig_draw_with_style(x,
			y,
			line_style,
			dash_or_dot_interval,
			line_width);

}



/*-----------------------------------------------------------------*/
static void plot_segment( md, Xstart, Ystart, Xstop, Ystop)

	/* If md == MOVE, does a "virtual" move by calling fig_move, 
        which resets figCurrentX, figCurrentY to Xstop, Ystop.  Pen is 
        not physically moved.  If md == DRAW, does a "virtual" move by 
        setting figCurrentX, figCurrentY to Xstart, Ystart; then calls 
        fig_draw which does a physical move to figCurrentX, figCurrentY 
        and then draws to Xstop, Ystop. */

    int		md;
    double	Xstart, 
		Ystart, 
		Xstop, 
		Ystop;


{
    if (md == MOVE)  
  	fig_move(Xstop, Ystop);

    if (md == DRAW)  {
  	fig_move(Xstart, Ystart);
  	fig_draw(Xstop, Ystop);
    }  
}


/*-----------------------------------------------------------------*/
static void move_or_draw( md, newX, newY)

	/* If md== DRAW on entry:

	Draws a line from current pen position to the position 
	specified by the arguments.  Arguments are in data     
	space units.  If any part of the line to be drawn      
	falls outside the current window, that part of the     
	line is not drawn; pen will move to window boundary    
	(clipping boundary) and lift.  If the specified draw   
	command brings the pen back into the window from       
	outside, the pen will first move to the point of       
	re-entry (from last point of exit, where it has been   
	waiting) and will then draw that portion of the        
	specified line that lies within the window.

	If md== MOVE on entry:                                  
	
	Works exactly as above except that pen never touches   
	the paper.  */

    int		md;
    double	newX,
		newY;
{
				       /*----- functions called -----*/
				       /*----- extern variables -----*/
				       /*----- local  variables -----*/

                         	  /* point(s) where clipped line enters
                         	  and/or exits window, in data space */
    double	Xcross[2],
		Ycross[2];

                                     /* number of cross points found */
    int		crossesFound;

                                   /* true if point is within window */
    int		newIn, currentIn;

				       /*----- start function -------*/


    newIn      = within_window(newX, newY);
    currentIn  = within_window(currentX, currentY);
    clipped    = !newIn;

                                /* If both points inside window...  */

                                         /*       ________          */
                                         /*      |        |         */
                                         /*      |  *---> |         */
                                         /*      |________|         */
    if (newIn &&  currentIn)
	plot_segment(md, currentX, currentY, newX, newY);

    else {            		/* If one or both points outside... */

    	findCrossPoints( newX, 
			 newY, 
			 newIn,
			 currentIn, 
			 Xcross, 
			 Ycross, 
			 &crossesFound );

                                         /*       ________          */
                                         /*      |        |         */
                                         /*      |        *--->     */
                                         /*      |________|         */
    	if ( (!crossesFound) &&  currentIn )
	    finish_line();


                                         /*       ________          */
                                         /*      |        |         */
                                         /*      |      *-+->       */
                                         /*      |________|         */
    	if ((crossesFound == 1) &&  currentIn ) {
      	    plot_segment(md, currentX, currentY, Xcross[0], Ycross[0]);
      	    finish_line();
      	}


    	if ((crossesFound == 1) &&  newIn)  {

                                         /*       ________          */
                                         /*      |        |         */
                                         /*      |      <-+-*       */
                                         /*      |________|         */
      	    if ((Xcross[0] != newX) || (Ycross[0] != newY))
                plot_segment(md, Xcross[0], Ycross[0], newX, newY);


                                         /*       ________          */
                                         /*      |        |         */
                                         /*      |        <---*     */
                                         /*      |________|         */
      	    else
                fig_move(newX, newY);
      	}  /* 1 cross & newIn */



                                         /*       ________          */
                                         /*      |        |         */
                                         /*   *--+--------+-->      */
                                         /*      |________|         */
       
    	if (crossesFound== 2 ) {                  
      	    plot_segment(md, Xcross[0], Ycross[0], Xcross[1], Ycross[1]);
      	    finish_line();
      	}   /* if 2 crosses */

    }  /* if one or both outside */

					  /* update current location */
    currentX  = newX;
    currentY  = newY;

}  /* move_or_draw */



/*-------------------------------------------------------------------*/
static void private_draw_line_style_key(x_upper_left,
			  		y_upper_left_ptr,
			  		length_as_fraction_of_xrange,
			  		font_arg,
			  		fontsize_as_fraction_of_yrange)

	/* Draws line style key at the location specified by the first
	two arguments (in x,y data space units).  The key will be drawn
	at this location even if it lies outside the window.  The other
	arguments specify the length of lines in the key and the font 
	and fontsize used for writing the descriptions.  Any of these 
	may be -1, in which case a default value is used.  If length 
	or fontsize is 0, the routine does nothing.  Returns the y 
	coordinate at the bottom of the finished key in 
	*y_upper_left_ptr.  */

    double	x_upper_left,
    	      *	y_upper_left_ptr,
    		length_as_fraction_of_xrange,
    		fontsize_as_fraction_of_yrange;
    int		font_arg;


{
				       /*----- functions called -----*/
    void private_write_text(),
    	 free_line_style_key();
				       /*----- extern variables -----*/
				       /*----- local  variables -----*/
    int		i,
		dashes_per_line;
    double	length,
		max_dash_length = 0.0,
		x,
		y,
		x_range,
		y_range,
		y_step,
		fontsize_in_y;
				       /*----- start function -------*/
    if ( ! length_as_fraction_of_xrange
	 ||
    	 ! fontsize_as_fraction_of_yrange
	 ||
	 ! num_line_style_records)
	return;

    x_range = windXmax - windXmin;
    y_range = windYmax - windYmin;

    if (length_as_fraction_of_xrange < 0.0
	||
        length_as_fraction_of_xrange > 0.5)
        length_as_fraction_of_xrange = DEF_LINE_STYLE_KEY_LENGTH;
    if (font_arg < 0 || font_arg > MAXFONT)
	font_arg  = DEF_FONT;
    if (fontsize_as_fraction_of_yrange < 0.0
	||
        fontsize_as_fraction_of_yrange > 0.2)
        fontsize_as_fraction_of_yrange = DEF_STYLE_KEY_FONTSIZE;

    fontsize_in_y = fontsize_as_fraction_of_yrange * y_range;
    y_step = fontsize_in_y * (1 + LINE_STYLE_KEY_GAP);

    for (i=0; i<num_line_style_records; i++) {
	if ((line_style_record_list[i].line_style == DASHED_LINE
	     ||
	     line_style_record_list[i].line_style == DOTTED_LINE)
	     &&
	     max_dash_length <
	     line_style_record_list[i].dash_or_dot_interval)

	     max_dash_length =
	     line_style_record_list[i].dash_or_dot_interval;
    }

    length = length_as_fraction_of_xrange * x_range;

    max_dash_length /= Xscale;

    if (max_dash_length) {
	dashes_per_line = irint(length/max_dash_length);
	if (!(dashes_per_line % 2))
	    dashes_per_line++;
	if (dashes_per_line < 3)
	    dashes_per_line = 3;
	length = dashes_per_line * max_dash_length;
    }

    x = x_upper_left;
    y = *y_upper_left_ptr;


    for (i=0; i<num_line_style_records; i++) {
	y -= y_step;
	fig_move( x, y );
	fig_draw_with_style( x + length,
			    y,
			    line_style_record_list[i].line_style,
			    line_style_record_list[i].
				dash_or_dot_interval,
			    line_style_record_list[i].line_width);
	fig_move( x + length + y_to_x(fontsize_in_y/2.0),
		 y - CHAR_HALF * fontsize_in_y );

	private_write_text( line_style_record_list[i].description,
			    convert_charsize( fontsize_in_y,Yscale),
			    font_arg,
			    LEFT_JUSTIFY,
			    0.0,
			    FALSE);

    }

    *y_upper_left_ptr = y - CHAR_HALF * fontsize_in_y;

    free_line_style_key();

}


/*-------------------------------------------------------------------*/
static void free_line_style_key()

	/* Frees any dynamic memory allocated to the line style key
	information in line_style_record_list. */

{
				       /*----- functions called -----*/
				       /*----- extern variables -----*/

    extern int	  	       num_line_style_records;
    extern LINE_STYLE_RECORD * line_style_record_list; 

				       /*----- local  variables -----*/
    LINE_STYLE_RECORD * ptr; 
    int	       i;
				       /*----- start function -------*/
    if (line_style_record_list) {

        for (i = 0; i<num_line_style_records; i++)
	    if (line_style_record_list[i].description)
	        free(line_style_record_list[i].description);

        free(line_style_record_list);
    }

    num_line_style_records = 0;
    line_style_record_list = (LINE_STYLE_RECORD *) NULL;
}

/*-------------------------------------------------------------------*/
static void locate_tics( requested_num_intervals,
		  	 axis_start,
		  	 axis_end,
		  	 num_tics_ptr,
		  	 interval_ptr,
		  	 first_tic_index_ptr,
		  	 last_tic_index_ptr)

	/* Divides axis extending from axis_start to axis_end into 
	approximately requested_num_intervals divisions; the exact 
	number is adjusted so that the spacing between tics is 1, 2,
	or 5 times an integral power of ten.  The number of tics to 
	be drawn (i.e. 1 + the actual number of divisions resulting) 
	is returned in *num_tics_ptr.  The location of the first and 
	last tic marks on the axis are returned in *first_tic_index_ptr 
	and *last_tic_index_ptr, respectively.  The interval between 
	tics is returned in *interval_ptr.  If requested_num_intervals
	is negative or zero on entry, the routine behaves as if 2 
	intervals were requested. */


    int		requested_num_intervals,
              * first_tic_index_ptr,
              * last_tic_index_ptr,
              * num_tics_ptr;
    double	axis_start,
          	axis_end,
              * interval_ptr;
    

{
				       /*----- functions called -----*/
    double	nice_interval();
				       /*----- extern variables -----*/

				       /*----- local  variables -----*/
    double	interval, full_scale;
				       /*----- start function -------*/
    if (requested_num_intervals <= 0)
        requested_num_intervals = 2;

    full_scale = axis_end - axis_start; 
    * interval_ptr = interval = nice_interval( full_scale,
				 requested_num_intervals );
    * first_tic_index_ptr = (int) ceil(axis_start/interval);
    * last_tic_index_ptr =  (int) floor(axis_end/interval);
    * num_tics_ptr = 1 + *last_tic_index_ptr - *first_tic_index_ptr;

}

/*-------------------------------------------------------------------*/
static int convert_charsize( charsize, scale, magnification_ptr )

	/* Converts charsize from data units (using scale) to pitch 
	units and returns the result.  If the result is too small to 
	read, SMALLEST_READABLE_FONT is returned instead, and
	*magnification_ptr is set to the smallest integer magnification
	which, when multiplied by charsize, would yield a font size >=
	SMALLEST_READABLE_FONT.  If the result is not too small to read
	*magnification_ptr is 1 on exit.  */

    int  * magnification_ptr;
    double charsize,
	   scale;

{
				       /*----- functions called -----*/

				       /*----- extern variables -----*/
				       /*----- local  variables -----*/
    int    font_size;
    double d_font_size;

    				       /*----- start function -------*/
    font_size = irint( d_font_size = charsize * scale / PIX_PER_PITCH );
    if (font_size < SMALLEST_READABLE_FONT) {
        font_size = SMALLEST_READABLE_FONT;
	*magnification_ptr = (int) ceil(SMALLEST_READABLE_FONT/d_font_size);
    }
    else
	*magnification_ptr = 1;

    return(font_size);
}


/*-------------------------------------------------------------------*/
static void private_write_text( text_str,
				font_size,
				font,
				justify,
				text_angle_in_radians,
				clipped_arg)

        /* Creates and writes to the fig file a fig text object 
	containing text_str; font, angle and justification are 
	determined by the other arguments. The text must contain no 
	newlines.  Sets global flag "error" and writes error message 
	if i/o error occurs.  Preserves leading spaces (by adjusting 
	the fig x, y coordinates) if text_angle_in_radians is 0.  
	Future versions will preserve leading spaces for any text 
	angle and will handle newlines in text_str. */

    char      * text_str;
    int		font_size,
		font,
		justify,
		clipped_arg;
    double	text_angle_in_radians;

{
				       /*----- functions called -----*/
    char * strchr();
				       /*----- extern variables -----*/
    extern char		text_postamble[];
				       /*----- local  variables -----*/
    char      * newline_ptr;
    double	x_leading_space = 0.0,
		y_leading_space = 0.0;
    int		fig_x,
		fig_y,
		num_leading_spaces,
		height_in_pixels,
		width_in_pixels;
				       /*----- start function -------*/
    check_wr_err();
    finish_line();

    if (text_str && (newline_ptr = strchr(text_str, '\n'))) {
	give_warning("text string truncated at newline");
	*newline_ptr = '\0';
    }

    if (!text_str || !(*text_str)) {
	give_warning("empty text string ignored");
	return;
    }

    num_leading_spaces = strspn(text_str, " ");

    if (justify == LEFT_JUSTIFY && !text_angle_in_radians)
    	x_leading_space = font_size * num_leading_spaces
    		          * PIX_PER_PITCH * CHAR_WIDTH_OVER_HEIGHT;

    height_in_pixels = CHAR_HEIGHT_ON_SCREEN;
    width_in_pixels  = CHAR_WIDTH_ON_SCREEN
			* strlen(text_str) - num_leading_spaces;

    fig_x = irint(figCurrentX + x_leading_space);
    fig_y = irint(figCurrentY + y_leading_space);

    if (!clipped_arg) {

        indent();
        fprintf(fp, "4 %d %d %d 0 0 0 %g 1 %d %d %d %d %s%s",
		    justify,
		    font,
		    font_size,
		    text_angle_in_radians,
		    height_in_pixels,
		    width_in_pixels,
		    fig_x,
		    fig_y,
		    text_str,
		    text_postamble );

    					      /* update_bounding_box */
        if (xmin > fig_x) 
	    xmin = fig_x;
        if (xmax < fig_x) 
	    xmax = fig_x;
        if (ymin > fig_y) 
	    ymin = fig_y;
        if (ymax < fig_y) 
	    ymax = fig_y;

        num_objects++;

        if (ferror(fp))
	    set_error( "error writing text object" );
    }
}

/*-------------------------------------------------------------------*/
static void draw_ellipse( x_size,
			  x_radius, 
			  y_radius, 
			  num_points, 
			  separation)

	/* Draws an ellipse centered at (currentX,currentY) in data
	space with radii as specified by x_radius, y_radius. The 
	argument "num_points" is the number of overlapping symbols 
	to be drawn at this point, and "separation" specifies the 
	degree of separation between overlapping points (1 => just 
	touching with no overlap; 0 => exactly superimposed).  See 
	"move_or_draw" for description of clipping.  */

    int		num_points;
    double	x_radius,
		y_radius,
		x_size,
		separation;

{
				       /*----- functions called -----*/

				       /*----- extern variables -----*/

				       /*----- local  variables -----*/
    int		i,
		j;
    double	angle,
		angle_at_intersection,
		x,
		y;

				       /*----- start function -------*/

    x = currentX;
    y = currentY;
    move(x + x_radius, y);
    					     /* draw complete symbol */

    for(i = 1; i <= POINTS_PER_ELLIPSE + 1; i++) {
	angle = 2.0 * M_PI * i / POINTS_PER_ELLIPSE;
	draw( x + x_radius*cos(angle), y + y_radius*sin(angle) );
    }
				  /* draw overlapped symbols, if any */

    angle_at_intersection = acos(separation);

    for (j=1; j<num_points; j++) {
	x -= x_size * separation;
    	move(x + x_radius*separation, 
	     y + y_radius*sqrt(1-separation*separation));
        for(i = 1; i <= POINTS_PER_ELLIPSE; i++) {
	    angle = angle_at_intersection + 
	    2.0 * (M_PI-angle_at_intersection) * i / POINTS_PER_ELLIPSE;
	    draw( x + x_radius*cos(angle), y + y_radius*sin(angle) );
        }
    }
}



/*-----------------------------------------------------------------*/
static void mark_point( size, 
			size_is_in_x_units,
		        shape, 
		        filled, 
		        num_points,
		        separation)

	/* Marks the current point in data space.  The current position 
	is not changed.  The appearance of the mark is specified by the
	arguments.  The argument "size" is specified in x data units
	if size_is_in_x_units is TRUE, in y data units otherwise.
	A default size is used if "size" is not positive on entry.
	The argument "num_points" is the number of overlapping symbols
	to be drawn at this point, and "separation" specifies 
	the degree of separation between overlapping points (1 => 
	just touching with no overlap; 0 => exactly superimposed).
	A default separation is used if separation is invalid on
	entry.  A warning is given and num_points is set to 1 if 
	num_points is invalid on entry.  The routine does nothing if
	num_points is 0 on entry.  See "move_or_draw" for description
	of clipping.  */

    int		shape,
		filled,
		size_is_in_x_units,
		num_points;
    double	size,
		separation;
{
				       /*----- functions called -----*/
    void	draw_ellipse();
				       /*----- extern variables -----*/
    extern int		line_style;
    extern double	dash_or_dot_interval,
			line_width,
			currentX,
			currentY,
			windXmax,
			windXmin,
			windYmax,
			windYmin;
				       /*----- local  variables -----*/
    int		line_style_on_entry;
    double	dash_or_dot_interval_on_entry,
		line_width_on_entry,
		currentX_on_entry,
		currentY_on_entry,
		outer_radius,
		drawn_radius;
				       /*----- start function -------*/
    if (!num_points)
	return;

    check_wr_err();

    if (num_points < 0) {
	give_warning("invalid num_points ignored in mark_point");
	num_points = 1;
    }

    if (separation < 0.0 || separation > 1.0)
	separation = DEF_POINT_SEPARATION;

    if (size <= 0.0)
	size = DEF_POINT_SIZE * (size_is_in_x_units ?
	windXmax-windXmin : windYmax-windYmin);

    line_style_on_entry = line_style;
    dash_or_dot_interval_on_entry = dash_or_dot_interval;
    line_width_on_entry = line_width;
    currentX_on_entry = currentX;
    currentY_on_entry = currentY;

    line_style = SOLID_LINE;
    dash_or_dot_interval = 0.0;

    switch (shape) {

	case CIRCLE:
	default:
	    outer_radius = size/2.0;
	    if (size_is_in_x_units)
	        set_linewidth_in_x(filled ?
	            outer_radius : size*POINT_LINE_WIDTH);
	    else
	        set_linewidth_in_y(filled ?
	            outer_radius : size*POINT_LINE_WIDTH);
	    drawn_radius = filled ?
	    outer_radius/2.0 : outer_radius*(1.0-POINT_LINE_WIDTH);
	    draw_ellipse( size_is_in_x_units ?
			  size : y_to_x(size),
			  size_is_in_x_units ?
			  drawn_radius : y_to_x(drawn_radius), 
			  size_is_in_x_units ?
			  x_to_y(drawn_radius) : drawn_radius, 
			  num_points,
			  separation);

    }  /* end: switch shape */


    line_style = line_style_on_entry;
    dash_or_dot_interval = dash_or_dot_interval_on_entry;
    line_width = line_width_on_entry;
    move(currentX_on_entry, currentY_on_entry);

}


/*-------------------------------------------------------------------*/
static int private_open_fig_file( filename_arg, comment_arg, reset )

	/* Opens a new fig file using the argument "filename_arg" for 
        the file name.  If filename_arg is NULL or empty on entry, 
        default_filename is used instead.  The file name is written 
        into the file as a comment immediately following fig_preamble.  
        If it is nonempty, the string argument "comment_arg" is 
        appended to this comment line (this string must NOT contain
	any newlines).  If on entry the fig file is already open 
	(i.e., fp is nonzero), then the routine just gives a warning 
	message and returns FALSE.  Reinitializes most global 
	variables if either the global flag "error" or the argument
	"reset" is TRUE on entry.  Writes error message, sets error 
	TRUE and returns TRUE if open fails or if i/o error occurs. */
     
    int    reset;
    char * filename_arg,
	 * comment_arg;

{
				       /*----- functions called -----*/
    void init_figtools();
				       /*----- extern variables -----*/
    extern int 	error;
				       /*----- local  variables -----*/
    int		i;
				       /*----- start function -------*/
    if (fp) {
	give_warning("open_fig_file(): file already open");
	return(error);
    }
    				 /* If error, reinitialize variables */
    if (error || reset) {
	init_figtools();
    }

    if ( filename_arg && *filename_arg ) {
	if (error = strsave( &filename, filename_arg ))
	    return(TRUE);
    }
    else {
	if (error = strsave( &filename, default_filename ))
	    return(TRUE);
    }

    if ( error = !(fp = fopen(filename, "w")) ) {
	perror( filename );
	return( TRUE );
    }

    fprintf( fp, fig_preamble );
    fprintf( fp, "# %s", filename );
    if ( comment_arg && *comment_arg )
    	fprintf( fp, ": %s", comment_arg);
    fprintf( fp, "\n# ");
    num_obj_offset = ftell(fp);
    depth = 0;
    num_objects = 0;

    for (i=0; i<SPACES_FOR_NUM_OBJECTS; i++)
	putc(' ', fp);
    fprintf( fp, "\n");

    if (error=ferror(fp)) {
	fprintf( stderr,
	"figtools: error writing fig preamble\n" );
	return(TRUE);
    }

    return( FALSE );
}




	/*=============================================================
				PUBLIC  ROUTINES
	=============================================================*/


/*-------------------------------------------------------------------*/
void window( xmin_arg, xmax_arg, ymin_arg, ymax_arg )

        /* Determines what rectangle of data space will	be mapped 
        onto the viewport rectangle for graphing.  Generates error 
        message and sets extern flag "error" if either the horizontal 
        or vertical interval has 0 length; otherwise it stores the 
        window limits and calculates the scale factors relating data 
        space to fig space. */

    double xmin_arg, xmax_arg, ymin_arg, ymax_arg;


{

				       /*----- functions called -----*/

				       /*----- extern variables -----*/

				       /*----- local  variables -----*/

				       /*----- start function -------*/
    check_err();
    finish_compounds();
    finish_line();
				        /* check for valid arguments */
    if (xmax_arg-xmin_arg <= 0.0 || ymax_arg-ymin_arg <= 0.0 )  
  	set_error("invalid window arguments");
    else  {                                        /* set new window */
	windXmin  = xmin_arg;
	windXmax  = xmax_arg;
	windYmin  = ymin_arg;
	windYmax  = ymax_arg;
	setScales();
    }
}



/*-------------------------------------------------------------------*/
void viewport( xmin_arg, xmax_arg, ymin_arg, ymax_arg )

	/* Determines what rectangular region of the fig screen or plot
	page (in inches) will contain the graph drawn by subsequent 
	draw commands.  Generates error message and sets error flag
	if either the horizontal or vertical interval has 0  length.
	Program stores the viewport limits and calculates the scale 
	factors relating data space to fig space.  */

    double xmin_arg, xmax_arg, ymin_arg, ymax_arg;

{
				       /*----- functions called -----*/
				       /*----- extern variables -----*/
				       /*----- local  variables -----*/
				       /*----- start function -------*/
    check_err();
    finish_compounds();
    finish_line();
    			 /* convert from inches to fig screen pixels */
    xmin_arg = x_inch_to_fig( xmin_arg );
    xmax_arg = x_inch_to_fig( xmax_arg );
    ymin_arg = y_inch_to_fig( ymin_arg );
    ymax_arg = y_inch_to_fig( ymax_arg );

    		      /* shrink viewport to fig on page if necessary */
    if (xmin_arg < FIG_X_MIN) 
        xmin_arg = FIG_X_MIN;
    if (xmax_arg > FIG_X_MAX)
        xmax_arg = FIG_X_MAX;
    if (ymin_arg < FIG_Y_MIN) 
        ymin_arg = FIG_Y_MIN; 
    if (ymax_arg > FIG_Y_MAX)
        ymax_arg = FIG_Y_MAX;
           				/* check for valid arguments */
    if (xmax_arg-xmin_arg <= 0.0 || ymax_arg-ymin_arg <= 0.0 )  
  	set_error("invalid viewport arguments");
    else { 					 /* set new viewport */
        viewXmin  = xmin_arg;
        viewXmax  = xmax_arg;
        viewYmin  = ymin_arg;
        viewYmax  = ymax_arg;
        setScales();
    }
}



/*-------------------------------------------------------------------*/
void move(x, y)

	/* Absolute move to position specified by arguments in data 
        space.  See "move_or_draw" for description of clipping.  */

    double	x,
		y;
{
				       /*----- functions called -----*/
				       /*----- extern variables -----*/
				       /*----- local  variables -----*/
				       /*----- start function -------*/
    check_err();
    finish_line();

    move_or_draw(MOVE, x, y);

}



/*-------------------------------------------------------------------*/
void draw( x, y )

	/* Absolute draw to position specified by arguments in data 
        space.  See "move_or_draw" for description of clipping.  */

    double	x,
		y;
{
				       /*----- functions called -----*/
				       /*----- extern variables -----*/
				       /*----- local  variables -----*/
				       /*----- start function -------*/
    check_wr_err();

    move_or_draw(DRAW, x, y);
}


/*-------------------------------------------------------------------*/
void rmove(deltaX, deltaY)

	/* Relative move to position specified by arguments in data 
        space.  See "move_or_draw" for description of clipping.  */

    double	deltaX,
		deltaY;
{
				       /*----- functions called -----*/
				       /*----- extern variables -----*/
				       /*----- local  variables -----*/
				       /*----- start function -------*/
    check_wr_err();
    finish_line();

    move_or_draw(MOVE, currentX + deltaX, currentY + deltaY);

}


/*-----------------------------------------------------------------*/
void rdraw(deltaX, deltaY)

	/* Relative draw to position specified by arguments in data 
        space.  See "move_or_draw" for description of clipping.  */

    double	deltaX,
		deltaY;
{
				       /*----- functions called -----*/
				       /*----- extern variables -----*/
				       /*----- local  variables -----*/
				       /*----- start function -------*/
    check_wr_err();

    move_or_draw(DRAW, currentX + deltaX, currentY + deltaY);

}


/*-----------------------------------------------------------------*/
void mark_point_in_x( size, 
		      shape, 
		      filled, 
		      num_points,
		      separation)

	/* Marks the current point in data space.
	The appearance of the mark is specified by the 
	arguments; see comments under mark_point() for descriptions
	of these.  Argument "size" is in x data units. */

    int		shape,
		filled,
		num_points;
    double	size,
		separation;
{
				       /*----- functions called -----*/
    void	mark_point();
				       /*----- extern variables -----*/
				       /*----- local  variables -----*/
				       /*----- start function -------*/
    mark_point( size, 
		TRUE,
		shape, 
		filled, 
		num_points,
		separation);
}


/*-----------------------------------------------------------------*/
void mark_point_in_y( size, 
		      shape, 
		      filled, 
		      num_points,
		      separation)

	/* Marks the current point in data space.
	The appearance of the mark is specified by the
	arguments; see comments under mark_point() for descriptions
	of these.  Argument "size" is in y data units. */

    int		shape,
		filled,
		num_points;
    double	size,
		separation;
{
				       /*----- functions called -----*/
    void	mark_point();
				       /*----- extern variables -----*/
				       /*----- local  variables -----*/
				       /*----- start function -------*/
    mark_point( size, 
		FALSE,
		shape, 
		filled, 
		num_points,
		separation);
}



/*-------------------------------------------------------------------*/
void write_comment(comment_str)

	/* Writes comment_str to the fig file if comment_str is 
	nonempty.  Sets extern "error" and writes msg if i/o error
	occurs.  Does nothing if extern "error" is set on entry. */

    char *comment_str;

{
				       /*----- functions called -----*/
    char * strchr();
				       /*----- extern variables -----*/

				       /*----- local  variables -----*/
    char      * newline_ptr;

				       /*----- start function -------*/
    check_wr_err();
    if ( comment_str && (newline_ptr = strchr(comment_str, '\n'))) {
	give_warning("comment truncated at newline");
	*newline_ptr = '\0';
    }

    if (comment_str && *comment_str) {
	/* indent(); */
	fprintf(fp, "# %s\n", comment_str);
    }
    if (ferror(fp))
	set_error( "error writing comment" );
}


/*-------------------------------------------------------------------*/
double char_width_in_x()

	/* Returns the approximate width of a character in X data 
	units. */
{
				       /*----- functions called -----*/
				       /*----- extern variables -----*/
				       /*----- local  variables -----*/
				       /*----- start function -------*/
    return(CHAR_WIDTH_OVER_HEIGHT*font_size * PIX_PER_PITCH / Xscale);
}

/*-------------------------------------------------------------------*/
double char_width_in_y()

	/* Returns the approximate width of a character in Y data 
	units. */
{
				       /*----- functions called -----*/
				       /*----- extern variables -----*/
				       /*----- local  variables -----*/
				       /*----- start function -------*/
    return(CHAR_WIDTH_OVER_HEIGHT*font_size * PIX_PER_PITCH / Yscale);
}

/*-------------------------------------------------------------------*/
double laser_pixel_in_x()

	/* Returns the approximate size of a laser printer pixel in
	X data units. */
{
				       /*----- functions called -----*/
				       /*----- extern variables -----*/
				       /*----- local  variables -----*/
				       /*----- start function -------*/
    return(ceil(10.0*FIG_PIX_PER_LASER_PIX)/(10*Xscale));
}

/*-------------------------------------------------------------------*/
double laser_pixel_in_y()

	/* Returns the approximate size of a laser printer pixel in
	Y data units. */
{
				       /*----- functions called -----*/
				       /*----- extern variables -----*/
				       /*----- local  variables -----*/
				       /*----- start function -------*/
    return(ceil(10.0*FIG_PIX_PER_LASER_PIX)/(10*Yscale));
}

/*-------------------------------------------------------------------*/
void set_font( new_font )

	/* Changes extern variable font to new_font if the latter is
	a valid font number on entry; otherwise sets font to DEF_FONT.
	Does nothing if extern "error" is set on entry. */

    int new_font;

{
				       /*----- functions called -----*/

				       /*----- extern variables -----*/
    extern int    font;
    extern int    error;
				       /*----- local  variables -----*/

    				       /*----- start function -------*/
    check_err();
    if (new_font >= 0 && new_font <= MAXFONT)
	font = new_font;
    else
	font = DEF_FONT;
}


/*-------------------------------------------------------------------*/
int set_charsize_in_y( charsize )

	/* Converts charsize from Y-axis data units to pitch units
	and stores the result in the extern font_size.  If the result
	is too small to read, SMALLEST_READABLE_FONT is used instead,
	and the magnification necessary to make the font readable is
	returned.  Does nothing if extern "error" is set on entry. */

    double charsize;

{
				       /*----- functions called -----*/

				       /*----- extern variables -----*/
    extern int    font_size;
    extern double Yscale;
				       /*----- local  variables -----*/
    int	magnification;

    				       /*----- start function -------*/
    if (error)
	return(1);
    font_size = convert_charsize( charsize, Yscale, &magnification );
    return( magnification );
}


/*-------------------------------------------------------------------*/
int set_charsize_in_x( charsize )

	/* Converts charsize from X-axis data units to pitch units
	and stores the result in the extern font_size.  If the result
	is too small to read, SMALLEST_READABLE_FONT is used instead,
	and the magnification necessary to make the font readable is
	returned.  Does nothing if extern "error" is set on entry. */

    double charsize;

{
				       /*----- functions called -----*/

				       /*----- extern variables -----*/
    extern int    font_size;
    extern double Xscale;
				       /*----- local  variables -----*/
    int	magnification;

    				       /*----- start function -------*/
    if (error)
	return(1);
    font_size = convert_charsize( charsize, Xscale, &magnification );
    return( magnification );
}

/*-------------------------------------------------------------------*/
int set_charsize_in_yrange( charsize )

	/* Converts charsize (given as a fraction of total vertical
	extent of the viewport) to pitch units and stores the result 
	in the extern font_size.  If the result is too small to read, 
	SMALLEST_READABLE_FONT is used instead, and the magnification 
	necessary to make the font readable is returned.  Does 
	nothing if extern "error" is set on entry. */

    double charsize;

{
				       /*----- functions called -----*/

				       /*----- extern variables -----*/
    extern int    font_size;
    extern double Yscale,
    		  windYmax,
		  windYmin;
				       /*----- local  variables -----*/
    int	magnification;
    double y_range;

    				       /*----- start function -------*/
    y_range = windYmax - windYmin;
    if (error)
	return(1);
    font_size = convert_charsize( charsize*y_range,
				  Yscale, &magnification );
    return( magnification );
}


/*-------------------------------------------------------------------*/
int set_charsize_in_xrange( charsize )

	/* Converts charsize (given as a fraction of total horizontal
	extent of the viewport) to pitch units and stores the result 
	in the extern font_size.  If the result is too small to read, 
	SMALLEST_READABLE_FONT is used instead, and the magnification 
	necessary to make the font readable is returned.  Does 
	nothing if extern "error" is set on entry. */

    double charsize;

{
				       /*----- functions called -----*/

				       /*----- extern variables -----*/
    extern int    font_size;
    extern double Xscale,
    		  windXmax,
		  windXmin;
				       /*----- local  variables -----*/
    int	magnification;
    double x_range;

    				       /*----- start function -------*/
    x_range = windXmax - windXmin;
    if (error)
	return(1);
    font_size = convert_charsize( charsize*x_range,
				  Xscale, &magnification );
    return( magnification );
}


/*-------------------------------------------------------------------*/
int set_charsize_in_pitch( charsize )

	/* Stores charsize in pitch units in the extern font_size.  If
	charsize is too small to read, SMALLEST_READABLE_FONT is used
	instead, and the magnification necessary to make the font 
	readable is returned.  Does nothing if extern "error" is set on entry. */

    double charsize;

{
				       /*----- functions called -----*/

				       /*----- extern variables -----*/
    extern int    font_size;
				       /*----- local  variables -----*/
    int	magnification;

    				       /*----- start function -------*/
    if (error)
	return(1);
    font_size = irint( charsize );
    if (font_size < SMALLEST_READABLE_FONT) {
        font_size = SMALLEST_READABLE_FONT;
	magnification = (int) ceil(SMALLEST_READABLE_FONT/charsize);
    }
    else magnification = 1;

    return( magnification );
}


/*-------------------------------------------------------------------*/
void set_linewidth_in_y( linewidth_arg )

	/* Converts linewidth_arg from Y-axis data units to fig pixels
	and stores the result in the extern line_width.  Does
	nothing if extern "error" is set on entry. */

    double linewidth_arg;

{
				       /*----- functions called -----*/

				       /*----- extern variables -----*/
    extern double line_width,
		  Yscale;
				       /*----- local  variables -----*/

				       /*----- start function -------*/
    check_err();
    finish_line();
    line_width = linewidth_arg * Yscale;
}


/*-------------------------------------------------------------------*/
void set_linewidth_in_x( linewidth_arg )

	/* Converts linewidth_arg from X-axis data units to fig pixels
	and stores the result in the extern line_width.  Does
	nothing if extern "error" is set on entry. */

    double linewidth_arg;

{
				       /*----- functions called -----*/

				       /*----- extern variables -----*/
    extern double line_width,
		  Xscale;
				       /*----- local  variables -----*/

				       /*----- start function -------*/
    check_err();
    finish_line();
    line_width = linewidth_arg * Xscale;
}


/*-------------------------------------------------------------------*/
void set_line_style( style )

	/* Checks style for validity and stores it in extern 
	line_style.  Does nothing if extern "error" is set on entry. */

    int style;

{
				       /*----- functions called -----*/

				       /*----- extern variables -----*/
    extern int	line_style;
				       /*----- local  variables -----*/

				       /*----- start function -------*/
    check_err();
    if (style < SOLID_LINE || style > LAST_STYLE) {
	give_warning("invalid line style ignored");
	return;
    }
    line_style = style;
}

/*-------------------------------------------------------------------*/
void set_dash_in_y( dash_length )

	/* Converts dash_length from Y-axis data units to fig pixels
	and stores the result in the extern dash_or_dot_interval.  Does
	nothing if extern "error" is set on entry. */

    double dash_length;

{
				       /*----- functions called -----*/
				       /*----- extern variables -----*/
    extern double dash_or_dot_interval,
		  Yscale;
				       /*----- local  variables -----*/
				       /*----- start function -------*/
    check_err();
    finish_line();

    dash_or_dot_interval = dash_length * Yscale;
}


/*-------------------------------------------------------------------*/
void set_dash_in_x( dash_length )

	/* Converts dash_length from X-axis data units to fig pixels
	and stores the result in the extern dash_or_dot_interval.  Does
	nothing if extern "error" is set on entry. */

    double dash_length;

{
				       /*----- functions called -----*/
				       /*----- extern variables -----*/
    extern double dash_or_dot_interval,
		  Xscale;
				       /*----- local  variables -----*/
				       /*----- start function -------*/
    check_err();
    finish_line();

    dash_or_dot_interval = dash_length * Xscale;
}



/*-------------------------------------------------------------------*/
void write_text( text_str )

        /* Creates a fig text object from text_str, and from the 
        current	values of the text-related extern variables, and writes 
        it to the fig file.  The text must contain no newlines.  Sets 
        global flag "error" and writes error message if i/o error 
        occurs.  Preserves leading spaces (by adjusting the fig x, y 
        coordinates) if text_angle_in_radians is 0.  Future versions 
        will preserve leading spaces for any text angle and will handle 
        newlines in text_str. */

    char *text_str;

{
				       /*----- functions called -----*/
    void private_write_text();
				       /*----- extern variables -----*/
    extern int		font_size,
			font,
			justify;
    extern double	text_angle_in_radians;
				       /*----- local  variables -----*/
				       /*----- start function -------*/
    private_write_text( text_str,
			font_size,
			font,
			justify,
			text_angle_in_radians,
			clipped);

}

/*-------------------------------------------------------------------*/
void init_figtools()

	/* Reinitializes most static global variables. */

{
				       /*----- functions called -----*/
				       /*----- extern variables -----*/
				       /*----- local  variables -----*/
				       /*----- start function -------*/
	error        = FALSE;
        drawing_line = FALSE;
	verbose	     = TRUE,
	high_res     = FALSE;	/* set to FALSE -- DKS */

	viewXmin = FIG_X_MIN; 
	viewXmax = FIG_X_MAX;
  	viewYmin = FIG_Y_MIN; 
	viewYmax = FIG_Y_MAX;

	windXmin = FIG_X_MIN; 
	windXmax = FIG_X_MAX; 
	windYmin = FIG_Y_MIN; 
	windYmax = FIG_Y_MAX; 

	Xscale = 1.0;
	Yscale = 1.0; 

	currentX = 0.0; 
	currentY = 0.0;

	figCurrentX = 0.0;
	figCurrentY = 0.0;

	clipped = FALSE;

	line_style = SOLID_LINE;
	dash_or_dot_interval = 0.0;
	line_width = DEF_LINE_WIDTH;
	free_line_style_key();

	font_size = DEF_FONT_SIZE;
	font      = DEF_FONT;
	justify   = LEFT_JUSTIFY;

	text_angle_in_radians = 0.0;

	xmin = FIG_X_MAX; 
	xmax = FIG_X_MIN; 
	ymin = FIG_Y_MAX; 
	ymax = FIG_Y_MIN; 

	compound_stack_ptr = compound_stack;

	if (filename)
	    free(filename);
	filename = (char *) NULL;

	if (legend_text)
	    free(legend_text);
	legend_text = (char *) NULL;
	legend_font = DEF_FONT;
	legend_fontsize_as_fraction_of_yrange = DEF_STYLE_KEY_FONTSIZE;
}

/*-------------------------------------------------------------------*/
int  open_fig_file( filename_arg, comment_arg )

	/* Opens a new fig file using the argument "filename_arg" for 
        the file name.  If filename_arg is NULL or empty on entry, 
        default_filename is used instead.  The file name is written 
        into the file as a comment immediately following fig_preamble.  
        If it is nonempty, the string argument "comment_arg" is 
        appended to this comment line (this string must NOT contain
	any newlines).  If on entry the fig file is already open 
	(i.e., fp is nonzero), then the routine just gives a warning 
	message and returns FALSE.  Reinitializes most global 
	variables if the global flag "error" is TRUE on entry.  Writes 
	error message, sets error TRUE and returns TRUE if open fails 
	or if i/o error occurs. */
     
    char * filename_arg,
	 * comment_arg;
{
return( private_open_fig_file( filename_arg, comment_arg, TRUE ) );
}


/*-------------------------------------------------------------------*/
int  close_fig_file()

	/* Closes the fig file specified by the extern variables fp
	and filename.  Gives warning message and completes any 
	incomplete compound objects.  Writes line style key and legend
	(if nonempty) below the data window.  Gives warning and returns 
	FALSE if file is not open on entry.  Gives warning and deletes 
	the file if no objects have been written to it; otherwise 
	writes num_objects into the file as a comment (each compound 
	object counts as a single object even if it has multiple 
	compound objects nested within it).  The routine then closes 
	the file, frees the dynamic memory used by filename, and clears 
	the extern variables fp and filename.  Returns TRUE if error is 
	TRUE on entry or if closing is not successful; otherwise 
	returns FALSE. */
{
				       /*----- functions called -----*/

				       /*----- extern variables -----*/
    extern int 	error;
				       /*----- local  variables -----*/
    char	message[BUFSIZ];
    int		close_error,
		dummy;
    double	fontsize_in_y;

				       /*----- start function -------*/

    if (!fp) {
	give_warning("close_fig_file(): file already closed" );
	return(error);
    }

    finish_compounds();
    finish_line();

    private_draw_line_style_key(x_line_style_key,
				&y_line_style_key,
				-1.0,
				-1,
				-1.0);

    if (legend_text) {
        fontsize_in_y = legend_fontsize_as_fraction_of_yrange * 
        (windYmax - windYmin);

        fig_move( x_line_style_key, 
		  y_line_style_key - fontsize_in_y*(1+LEGEND_GAP));

	private_write_text( legend_text,
			    convert_charsize( 
				fontsize_in_y,Yscale, &dummy),
			    legend_font,
			    LEFT_JUSTIFY,
			    0.0,
			    FALSE);
	num_objects--;
    }

    if (num_objects) {
	fseek( fp, num_obj_offset, ABSOLUTE );
	fprintf(fp, "%d object%s", num_objects,
		(num_objects==1)? "" : "s");
	fseek( fp, 0L, FROM_END );
    }
    else {
	sprintf(message, "deleting empty file %s", filename );
	give_warning(message);
	unlink( filename );
    }

    if (close_error = fclose(fp))
	fprintf( stderr, "figtools: error closing %s\n", filename);
    fp = (FILE *) NULL;

    free(filename);
    filename = (char *) NULL;

    return(error || close_error);

}



/*-------------------------------------------------------------------*/
void start_compound(comment_arg)

	/* Starts a new compound object.  The extern variables 
	num_objects, num_obj_offset, xmin, xmax, ymin, ymax,
	and box_offset are all saved on the compound_stack, and depth 
	is incremented.  The compound preamble is written with blank
	spaces where the bounding box coordinates will ultimately
	go.  The location in the file of this blank space is saved in
	box_offset, so that end_compound will know where to write the
	bounding box coordinates.  The same is done for num_objects
	using num_obj_offset.  If nonempty on entry, comment_arg
	is written into the file as a comment after the preamble and
	before num_objects.  This routine does nothing if the extern
	flag error is TRUE on entry. */

    char      * comment_arg;

{
				       /*----- functions called -----*/
    void push_compound();
				       /*----- extern variables -----*/
				       /*----- local  variables -----*/
    int		i;
    COMPOUND	local_compound;
				       /*----- start function -------*/
    check_wr_err();
    finish_line();

    local_compound.num_objects    = num_objects;
    local_compound.num_obj_offset = num_obj_offset;
    local_compound.xmin           = xmin;
    local_compound.xmax           = xmax;
    local_compound.ymin           = ymin;
    local_compound.ymax           = ymax;
    local_compound.box_offset     = box_offset;

    indent();
    fprintf(fp, "6");
    box_offset = ftell(fp);
    for (i=0; i<SPACES_FOR_BOUNDING_BOX; i++)
	putc(' ', fp);
    fprintf( fp, "\n");

    if ( comment_arg && *comment_arg ) {
    	/* indent(); */
    	fprintf( fp, "# %s\n", comment_arg);
    }

    /*indent();*/
    fprintf( fp, "# ");
    num_obj_offset = ftell(fp);
    for (i=0; i<SPACES_FOR_NUM_OBJECTS; i++)
	putc(' ', fp);
    fprintf( fp, "\n");

    push_compound( &depth, &local_compound );

    num_objects = 0;
    xmin = FIG_X_MAX; 
    xmax = FIG_X_MIN; 
    ymin = FIG_Y_MAX; 
    ymax = FIG_Y_MIN; 

    if (ferror(fp)) {
	fprintf( stderr, 
	"figtools: error creating compound object\n");
	error = TRUE;
    }

}


/*-------------------------------------------------------------------*/
void end_compound()

	/* Ends the current compound object.  The number of objects
	included in this compound object (num_objects) is written into
	the file at num_obj_offset.  The bounding box coordinates are
	adjusted if necessary and are then written to the file at
	box_offset.  The extern variables num_objects, num_obj_offset,
	xmin, xmax, ymin, ymax, and box_offset are then restored from
	the compound_stack, and depth is decremented.  This routine 
	does nothing if the extern flag error is TRUE on entry. */


{
				       /*----- functions called -----*/
    void pop_compound();
				       /*----- extern variables -----*/
				       /*----- local  variables -----*/
    COMPOUND	local_compound;
				       /*----- start function -------*/
    check_wr_err();
    finish_line();

    xmin = floor(xmin-1.0);
    if (xmin < FIG_X_MIN)
        xmin = FIG_X_MIN;
	
    xmax = ceil(xmax+1.0);
    if (xmax > FIG_X_MAX)
        xmax = FIG_X_MAX;
	
    ymin = floor(ymin-1.0);
    if (ymin < FIG_Y_MIN)
        ymin = FIG_Y_MIN;
	
    ymax = ceil(ymax+1.0);
    if (ymax > FIG_Y_MAX)
        ymax = FIG_Y_MAX;
	
    fseek( fp, box_offset, ABSOLUTE );
    fprintf(fp, " %d %d %d %d",
	    (int)xmin, (int)ymin, (int)xmax, (int)ymax);

    fseek( fp, num_obj_offset, ABSOLUTE );
    fprintf(fp, "%d object%s", num_objects, (num_objects==1)? "" : "s");
    fseek( fp, 0L, FROM_END );

    if (!num_objects)
	give_warning("empty compound object");

    pop_compound( &depth, &local_compound );

    if (num_objects)
        num_objects = local_compound.num_objects + 1;
    else
        num_objects = local_compound.num_objects;
    num_obj_offset  = local_compound.num_obj_offset;
    xmin	    = min(xmin, local_compound.xmin);
    xmax	    = max(xmax, local_compound.xmax);
    ymin	    = min(ymin, local_compound.ymin);
    ymax	    = max(ymax, local_compound.ymax);
    box_offset	    = local_compound.box_offset;

    indent();
    fprintf(fp, "%s", compound_postamble);

    if (ferror(fp)) {
	fprintf( stderr, 
	"figtools: error ending compound object\n");
	error = TRUE;
    }

}



/*-------------------------------------------------------------------*/
double nice_interval( FullScale, NumDivisions )

	/* This function calculates an interval in data space to be 
	used as spacing between tics on an axis.  The returned
	interval is of the form 1, 2, or 5 times an integral power 
	of ten, selected so as to divide the plot's full scale 
	length (argument FullScale) into approximately the specified 
	number of divisions (argument NumDivisions).  Returns -1.0
	if NumDivisions is negative or 0 on entry. */

    double	FullScale;
    int		NumDivisions;

{
				       /*----- functions called -----*/

				       /*----- extern variables -----*/

				       /*----- local  variables -----*/
    int		i, mantissa, exponent;
    double	real_mantissa, LogRoughInterval, Tens;

				       /*----- start function -------*/
    check_err();

    if (NumDivisions <=0)
	return(-1.0);

    LogRoughInterval  = log10( fabs(FullScale/NumDivisions) );
    real_mantissa    = exp10( frac(LogRoughInterval) );
    exponent          = (int) floor(LogRoughInterval);

    if (real_mantissa < 1 )  {
        real_mantissa *= 10;
        exponent--;
    }

    if (1.0 <= real_mantissa && real_mantissa < 1.5)
        mantissa  = 1;
    else  if (1.5 <= real_mantissa && real_mantissa < 3.5)
        mantissa  = 2;
    else  if (3.5 <= real_mantissa && real_mantissa < 7.5)
        mantissa  = 5;
    else  if (7.5 <= real_mantissa && real_mantissa < 10.0) {
	mantissa = 1;
	exponent++;
    }

    Tens  = 1;

    for ( i = 0; i < abs(exponent); i++)
	Tens *= 10;

    return( (exponent > 0) ? mantissa * Tens : mantissa / Tens);

}


/*-------------------------------------------------------------------*/
void axes( x_num_divisions, x_description, x_units,
           y_num_divisions, y_description, y_units,
	   tic_label_fontsize, description_fontsize )

	/* Draws x and y axes for the current data window.  The x-axis
	is drawn at y=0 if that lies within the window; otherwise it 
	is drawn on the bottom edge of the widow.  The y-axis is drawn 
	at x=0 if that lies within the window; otherwise it is drawn 
	on the left edge of the widow.  The number of divisions 
	(separated by tic marks) is specified for the x and y axes by 
	x_num_divisions and y_num_divisions, respectively.  If either 
	of these is zero, then tic marks for that axis are suppressed; 
	if negative, then a default number of divisions is used.  The 
	number of divisions actually drawn on each axis is adjusted so 
	that the spacing between tics is 1, 2, or 5 times an integral 
	power of ten.  The tics are labeled with numerals having the 
	size specified by tic_label_fontsize.  If tic_label_fontsize
	is 0, labels are suppressed; if negative, a default fontsize 
	is used.  The [xy]_descriptions and [xy]_units arguments are 
	strings to be written along the axes; description_fontsize 
	specifies their size.  If description_fontsize is 0, printing 
	of description and units is suppressed; if negative, a default 
	fontsize is used.  Both fontsize arguments specify font size
	as a fraction of the entire y-axis range.  Note that the tic 
	marks, tic labels and description strings will normally be 
	outside of the current viewport.  */

    int   	x_num_divisions,
		y_num_divisions;
    double      tic_label_fontsize,
		description_fontsize;
    char      * x_description, 
	      * x_units,
	      * y_description,
	      * y_units;
    

{
				       /*----- functions called -----*/

				       /*----- extern variables -----*/

				       /*----- local  variables -----*/
    double	x_org = 0.0,
		y_org = 0.0;
				       /*----- start function -------*/

    x_axis(  y_org, 
	     CLOSER_SIDE, 
	     x_num_divisions, 
	     -1.0,
	     x_description, 
	     x_units,
	     tic_label_fontsize, 
	     description_fontsize,
	     DEF_DESCRIPTION_PLACEMENT,
	     1,
	     &x_org );

    y_axis(  x_org, 
	     CLOSER_SIDE, 
	     y_num_divisions, 
	     -1.0,
	     y_description, 
	     y_units,
	     tic_label_fontsize, 
	     description_fontsize,
	     DEF_DESCRIPTION_PLACEMENT,
	     1,
	     &y_org );


}

/*-------------------------------------------------------------------*/
void x_axis( y_coordinate, 
	     text_side, 
	     num_divisions, 
	     tic_length,
	     description, 
	     units,
	     tic_label_fontsize, 
	     description_fontsize,
	     description_placement,
	     num_axis_crossings,
	     axis_cross_points)

	/* Draws an x-axis for the current data window.  The axis
	is drawn at y=y_coordinate if that lies within the window;
	otherwise it is drawn along the bottom edge of the widow.  The 
	number of divisions (separated by tic marks) is specified by 
	num_divisions.  If this is zero, tic marks are suppressed; 
	if negative, then a default number of divisions is used.  The 
	number of divisions actually drawn on the axis is adjusted so 
	that the spacing between tics is 1, 2, or 5 times an integral 
	power of ten.  The length of the tic marks (in y data units)
	is specified by tic_length.  If 0, tics are suppressed; if 
	negative, a default tic length is used.  The tics are labeled 
	with numerals having the size specified by tic_label_fontsize.
	If tic_label_fontsize is 0, labels are suppressed; if negative,
	a default fontsize is used.  The descriptions and units  
	arguments are strings to be written along the axis; 
	description_fontsize specifies their size.  If 
	description_fontsize is 0, printing of description 
	and units is suppressed; if negative, a default fontsize is 
	used.  The argument text_side specifies the location of tics, 
	labels and description text with respect to the axis; its 
	value may be BELOW, ABOVE or CLOSER_SIDE.  If CLOSER_SIDE,
	the routine places tics and text below the axis if the axis 
	is closer to the bottom of the window; otherwise it places 
	tics and text above the axis.  Note that tic marks, tic 
	labels and description strings may extend beyond the current 
	viewport.  The arguments num_axis_crossings and 
	axis_cross_points specify the number of other axes which
	cross this one and the locations of the intersections.  At
	present these are not used; future versions may use them to
	suppress tic labels which would otherwise overlap another 
	axis.  description_placement specifies whether the description
	will be left-justified, centered, or right-justified. */

    int   	text_side,
    		num_divisions,
		description_placement,
		num_axis_crossings;
    double	axis_cross_points[],
                y_coordinate,
		tic_length,
		tic_label_fontsize,
		description_fontsize;
    char      * description, 
	      * units;
    

{
				       /*----- functions called -----*/

				       /*----- extern variables -----*/
    extern double y_line_style_key;

				       /*----- local  variables -----*/
    char	tic_label[30],
		description_and_units[BUFSIZ];
    int		i,
		dummy,
		first_tic_index,
		last_tic_index,
		num_tics;
    double	x,
		y,
		y_range,
		tic_fontsize_in_y,
		desc_fontsize_in_y,
		first_tic_location,
		last_tic_location,
		tic_interval,
		space_used_by_tics;

				       /*----- start function -------*/
    check_wr_err();
    finish_line();

    y_range = windYmax - windYmin;

    if (!description)
	description = "";

    if (!units)
	units = "";

    if (y_coordinate < windYmin || y_coordinate > windYmax) {
	y_coordinate = windYmin;
	text_side = BELOW;
    }

    if (text_side != BELOW && text_side != ABOVE) 
	text_side = (y_coordinate - windYmin
		     <=
		     windYmax - y_coordinate) ? BELOW : ABOVE;

    if (tic_length == 0.0)
        num_divisions = 0;

    if (num_divisions < 0)
        num_divisions = DEF_NUM_DIVISIONS;

    if (description_fontsize < 0.0)
        description_fontsize = DEF_AXIS_LABEL_FONTSIZE;

    desc_fontsize_in_y = description_fontsize * y_range;

    start_compound("x axis");

    if (num_divisions) {

        if (tic_length < 0.0)
            tic_length = DEF_AXIS_LABEL_FONTSIZE * y_range/2.0;

        if (tic_label_fontsize < 0.0)
            tic_label_fontsize = DEF_AXIS_LABEL_FONTSIZE;

        tic_fontsize_in_y = tic_label_fontsize * y_range;

        locate_tics(  num_divisions,
		      windXmin,
		      windXmax,
		      &num_tics,
		      &tic_interval,
		      &first_tic_index,
		      &last_tic_index );
	
	first_tic_location = first_tic_index * tic_interval;
	last_tic_location  =  last_tic_index * tic_interval;

	if (first_tic_location - windXmin > 0.0)
	    plot_segment( DRAW, windXmin, y_coordinate, 
			 first_tic_location, y_coordinate);

	plot_segment( DRAW, first_tic_location, y_coordinate,
		     last_tic_location, y_coordinate);
	
	if (windXmax - last_tic_location > 0.0)
	    plot_segment( DRAW, last_tic_location, y_coordinate,
			 windXmax, y_coordinate);

	finish_line();

	for ( i = first_tic_index; i <= last_tic_index; i++ ) {

	    x = i * tic_interval;
	    plot_segment( DRAW, x, y_coordinate, 
	                       x, y_coordinate +
			       text_side * tic_length  );
	    finish_line();
	}

	space_used_by_tics = tic_length;

	if (tic_fontsize_in_y > 0.0) {

	    for ( i = first_tic_index; i <= last_tic_index; i++ ) {

	        x = i * tic_interval;

	        y = y_coordinate + 
		    text_side * ( tic_length 
				  +
				  TIC_LABEL_GAP*tic_fontsize_in_y )
	        - ((text_side == BELOW) ? tic_fontsize_in_y : 0.0);

		fig_move( x, y );

		sprintf(tic_label, "%g", x);

		private_write_text( tic_label,
				    convert_charsize(
				      tic_fontsize_in_y,Yscale, &dummy),
				    font,
				    CENTER,
				    0.0,
				    FALSE);

	    }

	    space_used_by_tics += (1 + TIC_LABEL_GAP) 
				  * tic_fontsize_in_y;

	}
    }
    else {			       /* num_divisions = 0; no tics */
	plot_segment( DRAW, windXmin, y_coordinate,
	                   windXmax, y_coordinate);
	finish_line();
	space_used_by_tics = 0.0;
    }

    if ( text_side == BELOW
	 &&
	 (y_coordinate - space_used_by_tics < windYmin)) {
	y_line_style_key -= 
	windYmin - (y_coordinate - space_used_by_tics);
    }

    if (desc_fontsize_in_y && (*description || *units)) {

	strcpy(description_and_units, description); 

	if (*units) {
	    strcat(description_and_units, " (");
	    strcat(description_and_units, units);
	    strcat(description_and_units, ")");
	}

	if (text_side == BELOW) {
	    y = windYmin - desc_fontsize_in_y - 
		LABEL_DESCRIPTION_GAP * desc_fontsize_in_y;
	    if (y_coordinate - space_used_by_tics < windYmin)
		y -= windYmin - (y_coordinate - space_used_by_tics);
	    y_line_style_key = y;
	}
	else {
	    y = windYmax + 
		LABEL_DESCRIPTION_GAP * desc_fontsize_in_y;
	    if (y_coordinate + space_used_by_tics > windYmax)
		y += y_coordinate + space_used_by_tics - windYmax;
	}

	if (description_placement < LEFT_JUSTIFY
	    ||
	    description_placement > RIGHT_JUSTIFY)
	    description_placement = DEF_DESCRIPTION_PLACEMENT;

	switch (description_placement) {
	    case LEFT_JUSTIFY:
		x = windXmin;
		break;
	    case CENTER:
		x = (windXmin + windXmax)/2.0;
	        break;
	    case RIGHT_JUSTIFY:
		x = windXmax;
	        break;
	}

	fig_move(x, y);
	private_write_text( description_and_units,
			    convert_charsize(
			      desc_fontsize_in_y,Yscale, &dummy),
			    font,
			    description_placement,
			    0.0,
			    FALSE);
    }

    end_compound();
    map_to_fig( currentX, currentY, &figCurrentX, &figCurrentY );
}
 

/*-------------------------------------------------------------------*/
void y_axis( x_coordinate, 
	     text_side, 
	     num_divisions, 
	     tic_length,
	     description, 
	     units,
	     tic_label_fontsize, 
	     description_fontsize,
	     description_placement,
	     num_axis_crossings,
	     axis_cross_points)

	/* Draws a y-axis for the current data window.  The axis is 
	drawn at x=x_coordinate if that lies within the window;
	otherwise it is drawn along the left edge of the widow.  The 
	number of divisions (separated by tic marks) is specified by 
	num_divisions.  If this is zero, tic marks are suppressed; 
	if negative, then a default number of divisions is used.  The 
	number of divisions actually drawn on the axis is adjusted so 
	that the spacing between tics is 1, 2, or 5 times an integral 
	power of ten.  The length of the tic marks (in x data units)
	is specified by tic_length.  If 0, tics are suppressed; if 
	negative, a default tic length is used.  The tics are labeled 
	with numerals having the size specified by tic_label_fontsize.
	If tic_label_fontsize is 0, labels are suppressed; if negative,
	a default fontsize is used.  The descriptions and units  
	arguments are strings to be written along the axis; 
	description_fontsize specifies their size.  If 
	description_fontsize is 0, printing of description 
	and units is suppressed; if negative, a default fontsize is 
	used.  The argument text_side specifies the location of tics, 
	labels and description text with respect to the axis; its 
	value may be LEFT, RIGHT or CLOSER_SIDE.  If CLOSER_SIDE, 
	the routine places tics and text at the left of the axis if 
	the axis is closer to the left edge of the window; otherwise 
	it places tics and text at the right of the axis.  Note that 
	tic marks, tic labels and description strings may extend 
	beyond the current viewport.  The arguments num_axis_crossings
	and axis_cross_points specify the number of other axes which
	cross this one and the locations of the intersections.  At
	present these are not used; future versions may use them to
	suppress tic labels which would otherwise overlap another 
	axis.  description_placement specifies whether the description
	will be left-justified, centered, or right-justified. */

    int   	text_side,
    		num_divisions,
		description_placement,
		num_axis_crossings;
    double      axis_cross_points[],
		x_coordinate,
		tic_length,
		tic_label_fontsize,
		description_fontsize;
    char      * description, 
	      * units;
    

{
				       /*----- functions called -----*/

				       /*----- extern variables -----*/

				       /*----- local  variables -----*/
    char	tic_label[30],
		description_and_units[BUFSIZ];
    int		i,
		dummy,
		first_tic_index,
		last_tic_index,
		label_length,
		max_label_length,
		num_tics;
    double	x,
		y,
		y_range,
		tic_fontsize_in_y,
		desc_fontsize_in_y,
		tic_interval,
		first_tic_location,
		last_tic_location,
		space_used_by_tics;

				       /*----- start function -------*/
    check_wr_err();
    finish_line();

    y_range = windYmax-windYmin;

    if (!description)
	description = "";

    if (!units)
	units = "";

    if (x_coordinate < windXmin || x_coordinate > windXmax) {
	x_coordinate = windXmin;
	text_side = LEFT;
    }

    if (text_side != LEFT && text_side != RIGHT) 
	text_side = (x_coordinate - windXmin
		     <=
		     windXmax - x_coordinate) ? LEFT : RIGHT;

    if (tic_length == 0.0)
        num_divisions = 0;

    if (num_divisions < 0)
        num_divisions = DEF_NUM_DIVISIONS;

    if (description_fontsize < 0.0)
        description_fontsize = DEF_AXIS_LABEL_FONTSIZE;

    desc_fontsize_in_y = description_fontsize * y_range;

    start_compound("y axis");

    if (num_divisions) {

        if (tic_length < 0.0)
            tic_length = DEF_AXIS_LABEL_FONTSIZE * y_to_x(y_range/2.0);

        if (tic_label_fontsize < 0.0)
            tic_label_fontsize = DEF_AXIS_LABEL_FONTSIZE;

	tic_fontsize_in_y = tic_label_fontsize * y_range;

        locate_tics(  num_divisions,
		      windYmin,
		      windYmax,
		      &num_tics,
		      &tic_interval,
		      &first_tic_index,
		      &last_tic_index );

	first_tic_location = first_tic_index * tic_interval;
	last_tic_location  =  last_tic_index * tic_interval;

	if (first_tic_location - windYmin > 0.0)
	    plot_segment( DRAW, x_coordinate, windYmin,
			 x_coordinate, first_tic_location );

	plot_segment( DRAW, x_coordinate, first_tic_location, 
		     x_coordinate, last_tic_location );

	if (windYmax - last_tic_location > 0.0)
	    plot_segment( DRAW, x_coordinate, last_tic_location,
			 x_coordinate, windYmax );

	finish_line();

	for ( i = first_tic_index; i <= last_tic_index; i++ ) {

	    y = i * tic_interval;

	    plot_segment( DRAW, x_coordinate, y,
	                       x_coordinate +
			       text_side * tic_length, y );
	    finish_line();
	}

	space_used_by_tics = tic_length;

	if (tic_fontsize_in_y > 0.0) {

	    max_label_length = 0;

	    for ( i = first_tic_index; i <= last_tic_index; i++ ) {

	        y = i * tic_interval;

	        x = x_coordinate + 
		    text_side * (tic_length + 
				 y_to_x(TIC_LABEL_GAP*tic_fontsize_in_y));

		fig_move( x, y - CHAR_HALF*tic_fontsize_in_y );

		sprintf(tic_label, "%g", y);
		private_write_text( tic_label,
				    convert_charsize(
				      tic_fontsize_in_y,Yscale, &dummy),
				    font,
				    (text_side==LEFT)? 
				      RIGHT_JUSTIFY : LEFT_JUSTIFY,
				    0.0,
				    FALSE);

		if (max_label_length <(label_length=strlen(tic_label)))
		    max_label_length = label_length;

	    }

	    space_used_by_tics +=
	    y_to_x( tic_fontsize_in_y *
	    (TIC_LABEL_GAP + max_label_length*CHAR_WIDTH_OVER_HEIGHT) );

	}
    }
    else {			       /* num_divisions = 0; no tics */
	plot_segment( DRAW, x_coordinate, windYmin,
	                   x_coordinate, windYmax );
	finish_line();
	space_used_by_tics = 0.0;
    }


    if (desc_fontsize_in_y && (*description || *units)) {

	strcpy(description_and_units, description); 

	if (*units) {
	    strcat(description_and_units, " (");
	    strcat(description_and_units, units);
	    strcat(description_and_units, ")");
	}

	if (text_side == LEFT) {
	    x = windXmin - y_to_x( 
		LABEL_DESCRIPTION_GAP * desc_fontsize_in_y);
	    if (x_coordinate - space_used_by_tics < windXmin)
		x -= windXmin - (x_coordinate - space_used_by_tics);
	}
	else {
	    x = windXmax + y_to_x( 
		(1 + LABEL_DESCRIPTION_GAP) * desc_fontsize_in_y);
	    if (x_coordinate + space_used_by_tics > windXmax)
		x += x_coordinate + space_used_by_tics - windXmax;
	}

	if (description_placement < LEFT_JUSTIFY
	    ||
	    description_placement > RIGHT_JUSTIFY)
	    description_placement = DEF_DESCRIPTION_PLACEMENT;

	switch (description_placement) {
	    case LEFT_JUSTIFY:
		y = windYmin;
		break;
	    case CENTER:
		y = (windYmin + windYmax)/2.0;
	        break;
	    case RIGHT_JUSTIFY:
		y = windYmax;
	        break;
	}

	fig_move(x, y);
	private_write_text( description_and_units,
			    convert_charsize(
			      desc_fontsize_in_y,Yscale,&dummy),
			    font,
			    description_placement,
			    M_PI/2.0,
			    FALSE);
    }

    end_compound();
    map_to_fig( currentX, currentY, &figCurrentX, &figCurrentY );

}

/*-------------------------------------------------------------------*/
void legend( new_legend, font_arg, fontsize_as_fraction_of_yrange)

	/* If new_legend points to text on entry, this routine stores 
	that text in dynamic memory, sets legend_text to point to
	the stored text and copies last 2 args to legend_font and
	legend_fontsize_as_fraction_of_yrange.  If new_legend is NULL
	or empty on entry, any previously stored legend is deleted and
	legend font and legend_fontsize_.. are set to defaults.  Does 
	nothing if extern variable 'error' is TRUE on entry. */

    char      * new_legend;
    int 	font_arg;
    double	fontsize_as_fraction_of_yrange;

{
				       /*----- functions called -----*/
				       /*----- extern variables -----*/
    extern char * legend_text; 
				       /*----- local  variables -----*/
				       /*----- start function -------*/

    check_err();

    if (legend_text)
	free(legend_text);
    legend_text = (char *) NULL;
    legend_font = DEF_FONT;
    legend_fontsize_as_fraction_of_yrange = DEF_STYLE_KEY_FONTSIZE;

    if ( new_legend && *new_legend && fontsize_as_fraction_of_yrange) {

        if (font_arg < 0 || font_arg > MAXFONT)
	    font_arg  = DEF_FONT;
        if (fontsize_as_fraction_of_yrange < 0.0
	    ||
            fontsize_as_fraction_of_yrange > 0.2)
            fontsize_as_fraction_of_yrange = DEF_STYLE_KEY_FONTSIZE;

    	legend_font = font_arg;
    	legend_fontsize_as_fraction_of_yrange = 
    	       fontsize_as_fraction_of_yrange; 

	if (strsave(&legend_text, new_legend) ) {
	    fprintf(stderr,
	    "legend(): not enough memory for new legend\n");
	    exit(TRUE);
	}
    }
}


/*-------------------------------------------------------------------*/
void line_style_key( description )

	/* Appends a record to line_style_record_list using the 
	current line style variables and description.  Does nothing
	if description is empty or NULL on entry. */

    char      * description;

{
				       /*----- functions called -----*/

				       /*----- extern variables -----*/

    extern int	  	       num_line_style_records;
    extern LINE_STYLE_RECORD * line_style_record_list; 

				       /*----- local  variables -----*/
    LINE_STYLE_RECORD * ptr; 
   
				       /*----- start function -------*/

    check_err();

    if (!description || !(*description))
	return;

    num_line_style_records++;
    if (error = reallocate( &line_style_record_list,
			    num_line_style_records 
			    * sizeof(LINE_STYLE_RECORD),
			    "line style key" ))
	exit(TRUE);

    ptr = line_style_record_list + num_line_style_records - 1;

    if (error = strsave(&(ptr->description), description))
	exit(TRUE);
    ptr->line_style = line_style;
    ptr->dash_or_dot_interval = dash_or_dot_interval;
    ptr->line_width = line_width;
}

/*-------------------------------------------------------------------*/
void draw_line_style_key(x_upper_left,
			 y_upper_left,
			 length_as_fraction_of_xrange,
			 font_arg,
			 fontsize_as_fraction_of_yrange)

	/* Draws line style key at the location specified by the first
	two arguments (in x,y data space units).  If this point lies 
	outside the window, then the key will be drawn at the default
	location below and at the left edge of the window.  The other
	arguments specify the length of lines in the key and the font 
	and fontsize used for writing the descriptions.  Any of these 
	may be -1, in which case a default value is used.  If length 
	or fontsize is 0, the routine does nothing. */

    double	x_upper_left,
    		y_upper_left,
    		length_as_fraction_of_xrange,
    		fontsize_as_fraction_of_yrange;
    int		font_arg;


{
				       /*----- functions called -----*/
				       /*----- extern variables -----*/
				       /*----- local  variables -----*/
				       /*----- start function -------*/

    check_wr_err();
    finish_line();

    if (within_window(x_upper_left, y_upper_left))
    	private_draw_line_style_key(x_upper_left,
				    &y_upper_left,
				    length_as_fraction_of_xrange,
				    font_arg,
				    fontsize_as_fraction_of_yrange);
    else
    	private_draw_line_style_key(x_line_style_key,
				    &y_line_style_key,
				    length_as_fraction_of_xrange,
				    font_arg,
				    fontsize_as_fraction_of_yrange);
}

/*-------------------------------------------------------------------*/
double y_to_x(y)

	/* Returns a distance in x data units which covers the same 
	physical distance on the graph as does the argument 'y' in y
	data units. */

    double	y;

{
				       /*----- extern variables -----*/
    extern double Yscale, Xscale;
				       /*----- start function -------*/
    return(y*Yscale/Xscale);
}

/*-------------------------------------------------------------------*/
double x_to_y(x)

	/* Returns a distance in y data units which covers the same 
	physical distance on the graph as does the argument 'x' in x
	data units. */

    double	x;

{
				       /*----- extern variables -----*/
    extern double Yscale, Xscale;
				       /*----- start function -------*/
    return(x*Xscale/Yscale);
}


