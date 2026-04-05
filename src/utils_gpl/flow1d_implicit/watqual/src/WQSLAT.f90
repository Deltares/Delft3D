subroutine wqslat ( nsegmt ,segmnt ,nsegtb ,segtab ,&
&ngrid  ,nposeg ,qlatgr ,qunkno )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Water Quality Interface Module
!
! Programmer:         J.Kuipers
!
! Module:             WQSLAT (Water Quality Segment LATeral flows)
!
! Module description: This routine calculates the lateral discharge
!                     per segment in one time step.
!
!                     The routine uses the segment definition and the
!                     water quantity network definition. Each segment is
!                     defined by a number of enclosed gridpoints, length
!                     factors and section number.
!
!                     Lateral discharges are imposed on main or
!                     unseparated segments.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  5 ngrid             I  Number of grid points in network.
!  6 nposeg            I  Number of positive segment numbers.
!  1 nsegmt            I  Number of segments defined.
!  3 nsegtb            I  Number of entries in segtab table.
!  2 segmnt(3,nsegmt)  I  Definition of table with segment pointers:
!                         (1,j) = Segment number
!                         (2,j) = Pointer to segtab for segment j.
!                         (3,j) = Number of enclosed gridpoints.
!  4 segtab(5,nsegtb)  I  This table contains for each segment the en-
!                         closed grid-cells together with the length
!                         factors and section indication.
!                         (1,j) = Gridpoint 1
!                         (2,j) = Gridpoint 2
!                         (3,j) = Length factor Lb
!                         (4,j) = Length factor Le
!                         (5,j) = Section
!                                 cnopar (0) : No parallel sections
!                                 cmainc (1) : Main channel
!                                 csub1  (2) : Sub section 1
!                                 csub2  (3) : Sub section 2
! 11 qunkno(nposeg)      IO Segment correction discharge
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
!=======================================================================
!
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: wqslat.pf,v $
! Revision 1.1  1999/03/12  12:45:38  kuipe_j
! parallel segments added
!
!
!
!***********************************************************************
!
!     Parameters
!
   integer ngrid,&
   &nposeg,&
   &nsegmt,&
   &nsegtb
!
   integer segmnt (3,nsegmt)
!
   real    qlatgr (ngrid),&
   &segtab (5,nsegtb),&
   &qunkno (nposeg)
!
!     Variables
!
   integer igp, igp1, isecwq, iseg, itab, ngrd, segno
   real    lb, le, lemlb
!
!     Loop over segment numbers
!
   do 300 iseg = 1, nsegmt
!
      segno = segmnt (1,iseg)
!
      if (segno .gt. 0) then
!
         itab = segmnt (2,iseg)
         ngrd = segmnt (3,iseg)
!
!          Loop over enclosed grid-cells
!
         do 200 igp = 1, ngrd
!
            isecwq = int (segtab (5,itab))

            if (isecwq .le. 1) then
               igp1   = int (segtab (1,itab))
               lb     =      segtab (3,itab)
               le     =      segtab (4,itab)
!
!                Calculate Le - Lb
!
               lemlb = le - lb
!
!                Add lateral flow of this working unit to segment
!                lateral flow
!
               qunkno(segno) = qunkno(segno) -&
               &qlatgr(igp1) * lemlb
!                Increment itab pointer
!
               itab = itab + 1
            endif
!
200      continue
      endif
300 continue
!
   return
end
