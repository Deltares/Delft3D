/*****************************************************************************/
/*                                                                           */
/*  (tricall.c)                                                              */
/*                                                                           */
/*  Example program that demonstrates how to call Triangle.                  */
/*                                                                           */
/*  Accompanies Triangle Versions 1.3 and 1.4                                */
/*  July 19, 1996                                                            */
/*                                                                           */
/*  This file is placed in the public domain (but the file that it calls     */
/*  is still copyrighted!) by                                                */
/*  Jonathan Richard Shewchuk                                                */
/*  2360 Woolsey #H                                                          */
/*  Berkeley, California  94705-1927                                         */
/*  jrs@cs.berkeley.edu                                                      */
/*                                                                           */
/*****************************************************************************/

/* If SINGLE is defined when triangle.o is compiled, it should also be       */
/*   defined here.  If not, it should not be defined here.                   */

#define NOTSINGLE

#ifdef SINGLE
#define REAL float
#else /* not SINGLE */
#define REAL double
#endif /* not SINGLE */

#include <stdio.h>
#include <stdlib.h>
/* #include <windows.h>*/
/* #include "triangle.h" */

#define _STDLIB_H_
#ifndef _STDLIB_H_
extern void *malloc();
extern void free();
#endif /* _STDLIB_H_ */


/*****************************************************************************/
/*                                                                           */
/*  report()   Print the input or output.                                    */
/*                                                                           */
/*****************************************************************************/


/*****************************************************************************/
/*                                                                           */
/*  main()   Create and refine a mesh.                                       */
/*                                                                           */
/*****************************************************************************/

/* int main() */

#include "f2c.h"
#include "triangle.h"


/**
 * Calls the triangulate() C-routine and stores its output in the pointer
 * variables passed by the (Fortran) caller.
 *
 * Note: all output arrays should be preallocated by the caller!
 *
 * \param[in]  jatri triangulate mode: When 2,  create points (samples),
 *             when 1, produce final Delaunay triangulation.
 *             when 3, produce final Delaunay triangulation AND
 *             edgeidx/triedge arrays (see below).
 * \param[in]  xs Coordinates of input points.
 *             When jatri==2: points of bounding polygon (segment points).
 *             When jatri==1 or 3: all points in grid (output from a previous
 *             tricall).
 * \param[in]  ns Number of input points (in xs/ys).
 * \param[out] indx Array that will be filled with node numbers for each
 *             triangle. Nr. of filled elements equals numtri*3.
 * \param[inout] numtri Number of produced triangles (out) and max nr of triangles possible in indx array (in).
 * \param[out] edgeidx (only when jatri==3)
 *             Array that will be filled with node numbers for each
 *             edge. Nr. of filled elements equals numedge*3.
 * \param[out] numedge Number of produced edges.
 * \param[out] triedge (only when jatri==3)
 *             Array that will be filled with edge numbers for
 *             triangle. Nr. of filled elements equals numtr*3.
 * \param[out] xs3 Only when jatri==2. Coordinates of all points/nodes
 *             in the triangular grid.
 * \param[inout] ns3 out: number of generated points in grid, in: if >0 used to check array size of xs3 and ys3 (not used if jatri=1)
 * \param[in]  trisize Only used when generating points (jatri==2).
 *             Maximum area for generated triangles.
 */
void STDCALL TRICALL(int *jatri, REAL *xs, REAL *ys, int *ns, int *indx, int *numtri, int *edgeidx, int *numedge, int *triedge, REAL *xs3, REAL *ys3, int *ns3, REAL *trisize)

{
  struct triangulateio in, mid, out, vorout;

  int number, i, j, itri, maxnumtri;
  int *trinumedge;
  char opties[256];

  maxnumtri = *numtri; // input numtri indicates (max) array size.

  /* Define input points. */
  in.numberofpoints = *ns;
  number = in.numberofpoints * 2 * sizeof(REAL);
  in.pointlist = (REAL *) malloc(number);
  for (i=0; i< *ns; i++)     {
    in.pointlist[2*i]   = xs[i];
    in.pointlist[2*i+1] = ys[i];
  }

  in.numberofpointattributes = 0;
  number = in.numberofpoints * in.numberofpointattributes * sizeof(REAL);
  in.pointattributelist = (REAL *) NULL; /* malloc( number) ; */

/*  in.pointattributelist[0] = 0.0;
  in.pointattributelist[1] = 1.0;
  in.pointattributelist[2] = 11.0;
  in.pointattributelist[3] = 10.0; */

  in.pointmarkerlist = (int *) NULL ; /* malloc(in.numberofpoints * sizeof(int));
  in.pointmarkerlist[0] = 0;
  in.pointmarkerlist[1] = 2;
  in.pointmarkerlist[2] = 0;
  in.pointmarkerlist[3] = 0; */


  if (*jatri == 1 || *jatri == 3) {
   in.numberofsegments = 0; }

  else {
    in.numberofsegments = *ns;
    in.segmentlist = (int *) malloc(in.numberofsegments* 2 * sizeof(int));
    in.segmentmarkerlist = (int *) NULL;
    for (i=0; i< *ns; i++)     {
      in.segmentlist[2*i]   = i;
      in.segmentlist[2*i+1] = i+1;
 }
    in.segmentlist[2*(*ns-1)+1] = 0;
  }



  in.numberofholes = 0;
  in.numberofregions = 0;
  in.regionlist = (REAL *) NULL ; /* malloc(in.numberofregions * 4 * sizeof(REAL)); */
  in.holelist = (REAL *) NULL ;   /* malloc(in.numberofregions * 4 * sizeof(REAL)); */


/*   in.regionlist[0] = 0.5;  */
/*   in.regionlist[1] = 5.0;  */
/*   in.regionlist[2] = 0.1;               Regional attribute (for whole mesh). */
/*   in.regionlist[3] = 0.1;             Area constraint that will not be used. */



/*  printf("Input point set:\n\n");
  report(&in, 1, 0, 0, 0, 0, 0);  */

  /* Make necessary initializations so that Triangle can return a */
  /*   triangulation in `mid' and a voronoi diagram in `vorout'.  */

  mid.pointlist = (REAL *) NULL;            /* Not needed if -N switch used. */
  /* Not needed if -N switch used or number of point attributes is zero: */
  mid.pointattributelist = (REAL *) NULL;
  mid.pointmarkerlist = (int *) NULL; /* Not needed if -N or -B switch used. */
  mid.trianglelist = (int *) NULL;          /* Not needed if -E switch used. */
  /* Not needed if -E switch used or number of triangle attributes is zero: */
  mid.triangleattributelist = (REAL *) NULL;
  mid.neighborlist = (int *) NULL;         /* Needed only if -n switch used. */
  /* Needed only if segments are output (-p or -c) and -P not used: */
  mid.segmentlist = (int *) NULL;
  /* Needed only if segments are output (-p or -c) and -P and -B not used: */
  mid.segmentmarkerlist = (int *) NULL;
  mid.edgelist = (int *) NULL;             /* Needed only if -e switch used. */
  mid.edgemarkerlist = (int *) NULL;   /* Needed if -e used and -B not used. */

  vorout.pointlist = (REAL *) NULL;        /* Needed only if -v switch used. */
  /* Needed only if -v switch used and number of attributes is not zero: */
  vorout.pointattributelist = (REAL *) NULL;
  vorout.edgelist = (int *) NULL;          /* Needed only if -v switch used. */
  vorout.normlist = (REAL *) NULL;         /* Needed only if -v switch used. */

  /* Triangulate the points.  Switches are chosen to read and write a  */
  /*   PSLG (p), preserve the convex hull (c), number everything from  */
  /*   zero (z), assign a regional attribute to each element (A), and  */
  /*   produce an edge list (e), a Voronoi diagram (v), and a triangle */
  /*   neighbor list (n).                                              */

  if (*jatri == 1) {
   /* triangulate("pcAevnQP", &in, &mid, &vorout); */

    triangulate("-Qpc", &in, &mid, &vorout);

  } else if (*jatri == 3) {
    /* Also produce edge-to-node mapping and tri-to-edge mapping
       (uses quite a bit more memory!) */
    triangulate("-Qpc-e-v", &in, &mid, &vorout);

    *numedge = mid.numberofedges;
    for (i=0; i < *numedge *2; i++)      {
        edgeidx[i] = mid.edgelist[i];
    }

	/* trinumedge array maintains current nr of edges already
	   registered for each triangle. */
	trinumedge = (int*)malloc(mid.numberoftriangles*sizeof(int));
	for (i = 0; i < mid.numberoftriangles; i++) trinumedge[i] = 0;

	/* For all edges, use Voronoi nodes (==triangle nrs) to register
	   the 3 edge nrs for each triangle. (mapping trinr -> edgenrs) */
    for (i = 0; i < *numedge; i++)      {
	  for (j = 0; j < 2; j++) {
	    itri = vorout.edgelist[2*i+j]; // left or right triangle.
		if (itri <= 0) continue;
		triedge[3*(itri-1)+trinumedge[itri-1]] = i+1;
		trinumedge[itri-1] = trinumedge[itri-1]+1;
      }
    }
    free(trinumedge);
  } else {
  /*    i=sprintf(opties,"-QDq20.0a%f",*trisize);       */

        i=sprintf(opties,"-Q-Y-q30.0-D-a%f",*trisize);  

  /*    i=sprintf(opties,"-Q-Y-q30.0-D-a",*trisize);    */   /* geeft grote cellen in het midden */ 



  /*      i=sprintf(opties,"-QYDa");             */

     

      triangulate(opties, &in, &mid, &vorout);

//    check size of xs3, ys3 if *ns3>0
	  if ( *ns3 > 0 & mid.numberofpoints > *ns3 )	{
		 printf("tricall: unsufficient mem for nodes in xs3, ys3 (%d > %d)\n", mid.numberofpoints, *ns3);
	     *numtri = -*numtri; // serves as error indicator
		 *ns3 = -mid.numberofpoints; // serves as error indicator
	  }
	  else	{
		 for (i=0; i< mid.numberofpoints; i++)     {
	  	 if (i <= mid.numberofpoints) {
	  		xs3[i] = mid.pointlist[2*i] ;
	  		ys3[i] = mid.pointlist[2*i+1];
	    	}
		 }
		 *ns3 = mid.numberofpoints;
	  }

  }



  *numtri = mid.numberoftriangles;
  if (*numtri > maxnumtri) {
	  printf("tricall: unsufficient mem for triangle nodes in indx (%d > %d)\n", *numtri, maxnumtri);
	  *numtri = -*numtri; // serves as error indicator
  } else {
	   for (i=0; i< *numtri *3; i++)     {
		  indx[i] = mid.trianglelist[i];
	   }
  }

    /* Free all allocated arrays, including those allocated by Triangle. */

  free(in.pointlist);
  free(in.pointattributelist);
  free(in.pointmarkerlist);
  free(in.regionlist);

  free(mid.pointlist);
  free(mid.pointattributelist);
  free(mid.pointmarkerlist);
  free(mid.trianglelist);

  free(mid.triangleattributelist);

  /* free(mid.trianglearealist);
  free(mid.neighborlist); */
  free(mid.segmentlist);
  free(mid.segmentmarkerlist);
  free(mid.edgelist);
  free(mid.edgemarkerlist);

  free(vorout.pointlist);
  free(vorout.pointattributelist);
  free(vorout.edgelist);
  free(vorout.normlist);

  return;

  printf("Initial triangulation:\n\n");
/*  report(&mid, 1, 1, 1, 1, 1, 0);   */
  printf("Initial Voronoi diagram:\n\n");
/*  report(&vorout, 0, 0, 0, 0, 1, 1); */

  /* Attach area constraints to the triangles in preparation for */
  /*   refining the triangulation.                               */

  /* Needed only if -r and -a switches used: */
  mid.trianglearealist = (REAL *) malloc(mid.numberoftriangles * sizeof(REAL));
  mid.trianglearealist[0] = 3.0;
  mid.trianglearealist[1] = 1.0;

  /* Make necessary initializations so that Triangle can return a */
  /*   triangulation in `out'.                                    */

  out.pointlist = (REAL *) NULL;            /* Not needed if -N switch used. */
  /* Not needed if -N switch used or number of attributes is zero: */
  out.pointattributelist = (REAL *) NULL;
  out.trianglelist = (int *) NULL;          /* Not needed if -E switch used. */
  /* Not needed if -E switch used or number of triangle attributes is zero: */
  out.triangleattributelist = (REAL *) NULL;

  /* Refine the triangulation according to the attached */
  /*   triangle area constraints.                       */

/*  triangulate("prazBP", &mid, &out, (struct triangulateio *) NULL);     */

  printf("Refined triangulation:\n\n");
/*  report(&out, 0, 1, 0, 0, 0, 0);                                   */

  /* Free all allocated arrays, including those allocated by Triangle. */

  free(in.pointlist);
  free(in.pointattributelist);
  free(in.pointmarkerlist);
  free(in.regionlist);
  free(mid.pointlist);
  free(mid.pointattributelist);
  free(mid.pointmarkerlist);
  free(mid.trianglelist);
  free(mid.triangleattributelist);
  free(mid.trianglearealist);
  free(mid.neighborlist);
  free(mid.segmentlist);
  free(mid.segmentmarkerlist);
  free(mid.edgelist);
  free(mid.edgemarkerlist);
  free(vorout.pointlist);
  free(vorout.pointattributelist);
  free(vorout.edgelist);
  free(vorout.normlist);
  free(out.pointlist);
  free(out.pointattributelist);
  free(out.trianglelist);
  free(out.triangleattributelist);

  return ;
}
