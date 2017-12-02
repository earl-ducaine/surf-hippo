#######################################
#    %W%
#
#    Makefile for Jack Wathey's 
#	C programming tools
#                                    
#      revised:  %G%
#                                   
#######################################

# subdir containing "toolbox" source code and obj files
T	= ../tools

# compiler
CC	= cc

#CFLAGS	= -g -I$T
CFLAGS	= -O2 -I$T


OBJECTS	=	$T/directio.o $T/figtools.o $T/filtools.o fstnoise.o\
		$T/getnum.o $T/jwstat.o $T/jwtools.o $T/sccstools.o\
		$T/scf.o $T/screen.o $T/sets.o

M	=	makefile

$T/libjw.a:	$(OBJECTS) $M
		ar ru $T/libjw.a $(OBJECTS)
		ranlib $T/libjw.a

$T/directio.o:	$T/directio.h $T/tf.h $M
		$(CC) -c $(CFLAGS) $T/directio.c -o $T/directio.o

$T/figtools.o:	$T/figtools.h $T/filtools.h $T/tf.h $T/jwmath.h $M
		$(CC) -c $(CFLAGS) $T/figtools.c -o $T/figtools.o

$T/filtools.o:	$T/filtools.h $T/tf.h $M
		$(CC) -c $(CFLAGS) $T/filtools.c -o $T/filtools.o

$T/fstnoise.o:	$T/fstnoise.h $M
		$(CC) -c $(CFLAGS) $T/fstnoise.c -o $T/fstnoise.o

$T/getnum.o:	$T/getnum.h $T/tf.h $M
		$(CC) -c $(CFLAGS) $T/getnum.c -o $T/getnum.o

$T/jwstat.o:	$T/jwstat.h $T/jwmath.h $T/tf.h $M
		$(CC) -c $(CFLAGS) $T/jwstat.c -o $T/jwstat.o

$T/jwtools.o: 	$T/jwtools.h $T/tf.h $M
		$(CC) -c $(CFLAGS) $T/jwtools.c -o $T/jwtools.o

$T/sccstools.o:	$T/sccstools.h $T/tf.h $M
		$(CC) -c $(CFLAGS) $T/sccstools.c -o $T/sccstools.o

$T/scf.o:	$T/scf.h $T/tf.h $T/jwtools.h $M
		$(CC) -c $(CFLAGS) $T/scf.c -o $T/scf.o

$T/screen.o:	$T/tf.h $M
		$(CC) -c $(CFLAGS) $T/screen.c -o $T/screen.o

$T/sets.o:	$T/sets.h $M
		$(CC) -c $(CFLAGS) $T/sets.c -o $T/sets.o


clean: 		$M
	rm -f $(OBJECTS) 

