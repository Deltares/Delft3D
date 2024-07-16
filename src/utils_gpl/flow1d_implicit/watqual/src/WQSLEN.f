      subroutine wqslen ( nsegmt ,segmnt ,nsegtb ,segtab ,
     +                    ngrid  ,nposeg ,x      ,slen   )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Water Quality Interface Module
c
c Programmer:         S.L. van der Woude
c
c Module:             WQSLEN (Water Quality Segment LENgths)
c
c Module description: This routine calculates a length for each segment.
c
c                     The routine uses the segment definition and the
c                     water quantity network definition. Each segment is
c                     defined by a number of enclosed gridpoints, length
c                     factors and section number. For each segment the
c                     lengths of the enclosed grid-cells is added and
c                     totalised for the segment.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  5 ngrid             I  Number of grid points in network.
c  6 nposeg            I  Number of positive segment numbers.
c  1 nsegmt            I  Number of segments defined.
c  3 nsegtb            I  Number of entries in segtab table.
c  2 segmnt(3,nsegmt)  I  Definition of table with segment pointers:
c                         (1,j) = Segment number
c                         (2,j) = Pointer to segtab for segment j.
c                         (3,j) = Number of enclosed gridpoints.
c  4 segtab(5,nsegtb)  I  This table contains for each segment the en-
c                         closed grid-cells together with the length
c                         factors and section indication.
c                         (1,j) = Gridpoint 1
c                         (2,j) = Gridpoint 2
c                         (3,j) = Length factor Lb
c                         (4,j) = Length factor Le
c                         (5,j) = Section
c                                 cnopar (0) : No parallel sections
c                                 cmainc (1) : Main channel
c                                 csub1  (2) : Sub section 1
c                                 csub2  (3) : Sub section 2
c  8 slen(nposeg)      IO Lengths for each segment.
c  7 x(ngrid)          I  x(i) = X-coordinate of grid point i.
c=======================================================================
c
c
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: wqslen.pf,v $
c Revision 1.3  1999/03/12  12:42:26  kuipe_j
c parallel segments added
c
c Revision 1.2  1995/05/30  07:08:43  hoeks_a
c file changed from dos to ux
c
c Revision 1.1  1995/04/13  07:11:04  hoeks_a
c Initial check-in
c
c Revision 1.2  1993/11/26  15:35:54  kuipe_j
c Update after finishing Sobeksel.
c
c Revision 1.1.1.1  1993/07/21  14:44:29  kuipe_j
c Initial version
c
c
c***********************************************************************
c
c       Parameters
c
      integer    ngrid,
     +           nsegmt,
     +           nsegtb,
     +           nposeg

      integer    segmnt (3,nsegmt)

      real       segtab (5,nsegtb),
     +           slen   (nposeg),
     +           x      (ngrid)
c
c       Variables
c
      integer    igp, igp1, igp2, iseg, itab, ngrd, segno
      real       lb, le
c
c     Loop over segment numbers
c
      do 200 iseg = 1, nsegmt
c
        segno = segmnt (1,iseg)
c
        if (segno .gt. 0) then
c
           slen (segno) = 0
c
           itab  = segmnt (2,iseg)
           ngrd  = segmnt (3,iseg)
           do 100 igp = 1, ngrd

              igp1 = int ( segtab(1,itab))
              igp2 = int ( segtab(2,itab))
              lb   =       segtab(3,itab)
              le   =       segtab(4,itab)
c
c             Add length of this grid-cell to segment length
c
              slen(segno) = slen(segno) + (le-lb) * (x(igp2) - x(igp1))
c
c             Increment itab pointer
c
              itab = itab + 1

 100       continue

        endif
 200  continue

      return
      end
