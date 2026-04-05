subroutine wqsvol ( nsegmt ,segmnt ,nsegtb ,segtab ,&
&ngrid  ,nposeg ,psi    ,at     ,&
&afs    ,x      ,svol   )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Water Quality Interface Module
!
! Programmer:         S.L. van der Woude
!
! Module:             WQSVOL (Water Quality Segment VOLumes)
!
! Module description: This routine calculates the segment volumes for
!                     one time step.
!
!                     The routine uses the segment definition and the
!                     water quantity network definition. Each segment is
!                     defined by a number of enclosed gridpoints, length
!                     factors and section number.
!
!                     For each working unit the space averaged total
!                     area is calculated which is multiplied with the
!                     working unit length. The segment volume is calcu-
!                     lated by adding all working unit fractions.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  8 at                P  -
!  9 afs               P  -
!  5 ngrid             I  Number of grid points in network.
!  6 nposeg            I  Number of positive segment numbers.
!  1 nsegmt            I  Number of segments defined.
!  3 nsegtb            I  Number of entries in segtab table.
!  7 psi               I  Space weight factor in Preissmann scheme.
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
! 11 svol(nposeg)      IO Segment volumes
! 10 x(ngrid)          I  x(i) = X-coordinate of grid point i.
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! wqgpar  Water Quality GridPoint ARea
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
! $Log: wqsvol.pf,v $
! Revision 1.3  1999/03/12  12:42:27  kuipe_j
! parallel segments added
!
! Revision 1.2  1995/05/30  07:08:44  hoeks_a
! file changed from dos to ux
!
! Revision 1.1  1995/04/13  07:11:05  hoeks_a
! Initial check-in
!
! Revision 1.2  1993/11/26  15:35:56  kuipe_j
! Update after finishing Sobeksel.
!
! Revision 1.1.1.1  1993/07/21  14:44:29  kuipe_j
! Initial version
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
   real    psi
!
   real    at     (ngrid),&
   &afs    (ngrid,2),&
   &segtab (5,nsegtb),&
   &svol   (nposeg),&
   &x      (ngrid)
!
!     Variables
!
   integer igp, igp1, igp2, isecwq, iseg, itab, ngrd, segno
   real    a1, a2, awu, lb, le, lemlb, xemxb
!
!     Loop over segment numbers
!
   do 300 iseg = 1, nsegmt
!
      segno = segmnt (1,iseg)
!
      if (segno .gt. 0) then
!
         svol (segno) = 0
!
         itab = segmnt (2,iseg)
         ngrd = segmnt (3,iseg)
!
!           Loop over enclosed grid-cells
!
         do 200 igp = 1, ngrd
!
            igp1   = int (segtab (1,itab))
            igp2   = int (segtab (2,itab))
            lb     =      segtab (3,itab)
            le     =      segtab (4,itab)
            isecwq = int (segtab (5,itab))
!
!              Calculate Le - Lb
!
            lemlb = le - lb
!
!              Calculate Xe - Xb
!
            xemxb = x (igp2) - x (igp1 )
!
!              Calculate area of working unit. Note that if sec equates 0
!              the whole gridcell is included in the working unit.
!
            call wqgpar ( ngrid, at, afs, igp1, isecwq, a1 )
            call wqgpar ( ngrid, at, afs, igp2, isecwq, a2 )
!
            awu = (( 1.-psi ) * a1 ) + ( psi * a2 )
!
!              Add volume of this working unit to segment volume
!
            svol (segno) = svol (segno) + ( awu * lemlb * xemxb )
!
!              Increment itab pointer
!
            itab = itab + 1
!
200      continue
         svol (segno) = max (svol (segno),0.01)
      endif
300 continue
!
   return
end
