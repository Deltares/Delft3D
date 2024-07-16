      subroutine wqslat ( nsegmt ,segmnt ,nsegtb ,segtab ,
     +                    ngrid  ,nposeg ,qlatgr ,qunkno )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Water Quality Interface Module
c
c Programmer:         J.Kuipers
c
c Module:             WQSLAT (Water Quality Segment LATeral flows)
c
c Module description: This routine calculates the lateral discharge
c                     per segment in one time step.
c
c                     The routine uses the segment definition and the
c                     water quantity network definition. Each segment is
c                     defined by a number of enclosed gridpoints, length
c                     factors and section number.
c
c                     Lateral discharges are imposed on main or
c                     unseparated segments.
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
c 11 qunkno(nposeg)      IO Segment correction discharge
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
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
c $Log: wqslat.pf,v $
c Revision 1.1  1999/03/12  12:45:38  kuipe_j
c parallel segments added
c
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
      real    qlatgr (ngrid),
     +        segtab (5,nsegtb),
     +        qunkno (nposeg)
c
c     Variables
c
      integer igp, igp1, isecwq, iseg, itab, ngrd, segno
      real    lb, le, lemlb
c
c     Loop over segment numbers
c
      do 300 iseg = 1, nsegmt
c
        segno = segmnt (1,iseg)
c
        if (segno .gt. 0) then
c
           itab = segmnt (2,iseg)
           ngrd = segmnt (3,iseg)
c
c          Loop over enclosed grid-cells
c
           do 200 igp = 1, ngrd
c
              isecwq = int (segtab (5,itab))

              if (isecwq .le. 1) then
                 igp1   = int (segtab (1,itab))
                 lb     =      segtab (3,itab)
                 le     =      segtab (4,itab)
c
c                Calculate Le - Lb
c
                 lemlb = le - lb
c
c                Add lateral flow of this working unit to segment
c                lateral flow
c
                 qunkno(segno) = qunkno(segno) -
     +                           qlatgr(igp1) * lemlb
c                Increment itab pointer
c
                 itab = itab + 1
              endif
c
 200       continue
        endif
 300  continue
c
      return
      end
