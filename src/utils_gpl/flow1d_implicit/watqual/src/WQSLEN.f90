subroutine wqslen ( nsegmt ,segmnt ,nsegtb ,segtab ,&
&ngrid  ,nposeg ,x      ,slen   )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Water Quality Interface Module
!
! Programmer:         S.L. van der Woude
!
! Module:             WQSLEN (Water Quality Segment LENgths)
!
! Module description: This routine calculates a length for each segment.
!
!                     The routine uses the segment definition and the
!                     water quantity network definition. Each segment is
!                     defined by a number of enclosed gridpoints, length
!                     factors and section number. For each segment the
!                     lengths of the enclosed grid-cells is added and
!                     totalised for the segment.
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
!  8 slen(nposeg)      IO Lengths for each segment.
!  7 x(ngrid)          I  x(i) = X-coordinate of grid point i.
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
! $Log: wqslen.pf,v $
! Revision 1.3  1999/03/12  12:42:26  kuipe_j
! parallel segments added
!
! Revision 1.2  1995/05/30  07:08:43  hoeks_a
! file changed from dos to ux
!
! Revision 1.1  1995/04/13  07:11:04  hoeks_a
! Initial check-in
!
! Revision 1.2  1993/11/26  15:35:54  kuipe_j
! Update after finishing Sobeksel.
!
! Revision 1.1.1.1  1993/07/21  14:44:29  kuipe_j
! Initial version
!
!
!***********************************************************************
!
!       Parameters
!
   integer    ngrid,&
   &nsegmt,&
   &nsegtb,&
   &nposeg

   integer    segmnt (3,nsegmt)

   real       segtab (5,nsegtb),&
   &slen   (nposeg),&
   &x      (ngrid)
!
!       Variables
!
   integer    igp, igp1, igp2, iseg, itab, ngrd, segno
   real       lb, le
!
!     Loop over segment numbers
!
   do 200 iseg = 1, nsegmt
!
      segno = segmnt (1,iseg)
!
      if (segno .gt. 0) then
!
         slen (segno) = 0
!
         itab  = segmnt (2,iseg)
         ngrd  = segmnt (3,iseg)
         do 100 igp = 1, ngrd

            igp1 = int ( segtab(1,itab))
            igp2 = int ( segtab(2,itab))
            lb   =       segtab(3,itab)
            le   =       segtab(4,itab)
!
!             Add length of this grid-cell to segment length
!
            slen(segno) = slen(segno) + (le-lb) * (x(igp2) - x(igp1))
!
!             Increment itab pointer
!
            itab = itab + 1

100      continue

      endif
200 continue

   return
end
