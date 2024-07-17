      subroutine wqsvol ( nsegmt ,segmnt ,nsegtb ,segtab ,
     +                    ngrid  ,nposeg ,psi    ,at     ,
     +                    afs    ,x      ,svol   )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Water Quality Interface Module
c
c Programmer:         S.L. van der Woude
c
c Module:             WQSVOL (Water Quality Segment VOLumes)
c
c Module description: This routine calculates the segment volumes for
c                     one time step.
c
c                     The routine uses the segment definition and the
c                     water quantity network definition. Each segment is
c                     defined by a number of enclosed gridpoints, length
c                     factors and section number.
c
c                     For each working unit the space averaged total
c                     area is calculated which is multiplied with the
c                     working unit length. The segment volume is calcu-
c                     lated by adding all working unit fractions.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  8 at                P  -
c  9 afs               P  -
c  5 ngrid             I  Number of grid points in network.
c  6 nposeg            I  Number of positive segment numbers.
c  1 nsegmt            I  Number of segments defined.
c  3 nsegtb            I  Number of entries in segtab table.
c  7 psi               I  Space weight factor in Preissmann scheme.
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
c 11 svol(nposeg)      IO Segment volumes
c 10 x(ngrid)          I  x(i) = X-coordinate of grid point i.
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c wqgpar  Water Quality GridPoint ARea
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
c $Log: wqsvol.pf,v $
c Revision 1.3  1999/03/12  12:42:27  kuipe_j
c parallel segments added
c
c Revision 1.2  1995/05/30  07:08:44  hoeks_a
c file changed from dos to ux
c
c Revision 1.1  1995/04/13  07:11:05  hoeks_a
c Initial check-in
c
c Revision 1.2  1993/11/26  15:35:56  kuipe_j
c Update after finishing Sobeksel.
c
c Revision 1.1.1.1  1993/07/21  14:44:29  kuipe_j
c Initial version
c
c
c***********************************************************************
c
c     Parameters
c
      integer ngrid,
     +        nposeg,
     +        nsegmt,
     +        nsegtb
c
      integer segmnt (3,nsegmt)
c
      real    psi
c
      real    at     (ngrid),
     +        afs    (ngrid,2),
     +        segtab (5,nsegtb),
     +        svol   (nposeg),
     +        x      (ngrid)
c
c     Variables
c
      integer igp, igp1, igp2, isecwq, iseg, itab, ngrd, segno
      real    a1, a2, awu, lb, le, lemlb, xemxb
c
c     Loop over segment numbers
c
      do 300 iseg = 1, nsegmt
c
        segno = segmnt (1,iseg)
c
        if (segno .gt. 0) then
c
           svol (segno) = 0
c
           itab = segmnt (2,iseg)
           ngrd = segmnt (3,iseg)
c
c           Loop over enclosed grid-cells
c
           do 200 igp = 1, ngrd
c
              igp1   = int (segtab (1,itab))
              igp2   = int (segtab (2,itab))
              lb     =      segtab (3,itab)
              le     =      segtab (4,itab)
              isecwq = int (segtab (5,itab))
c
c              Calculate Le - Lb
c
              lemlb = le - lb
c
c              Calculate Xe - Xb
c
              xemxb = x (igp2) - x (igp1 )
c
c              Calculate area of working unit. Note that if sec equates 0
c              the whole gridcell is included in the working unit.
c
              call wqgpar ( ngrid, at, afs, igp1, isecwq, a1 )
              call wqgpar ( ngrid, at, afs, igp2, isecwq, a2 )
c
              awu = (( 1.-psi ) * a1 ) + ( psi * a2 )
c
c              Add volume of this working unit to segment volume
c
              svol (segno) = svol (segno) + ( awu * lemlb * xemxb )
c
c              Increment itab pointer
c
              itab = itab + 1
c
 200        continue
            svol (segno) = max (svol (segno),0.01)
        endif
 300  continue
c
      return
      end
