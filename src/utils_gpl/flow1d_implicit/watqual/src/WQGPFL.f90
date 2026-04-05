subroutine wqgpfl ( ngrid ,qaggr ,igp   ,isecwq  ,qcalc )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Water Quality Interface Module
!
! Programmer:         S.L. van der Woude
!
! Module:             WQGPFL (Water Quality GridPoint FLow)
!
! Module description: This routine calculates the flow in a particular
!                     gridpoint/section.
!
!                     The total flow for the requested gridpoint/section
!                     is extracted. When the whole channel is indicated,
!                     including parallel sections, all flows are summed
!                     to one flow.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  3 igp               I  Gridpoint.
!  4 isecwq            I  Section:
!                         0 = Channel including parallel sections
!                         1 = Main channel
!                         2 = Sub section 1
!                         3 = Sub section 2
!  1 ngrid             I  Number of grid points in network.
!  2 qaggr(ngrid,3)    I  Aggregated flow through main and subsections 1
!                         and 2, using time step of the water quality
!                         process run.
!  5 qcalc             O  Calculated flow value.
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
! $Log: wqgpfl.pf,v $
! Revision 1.3  1999/03/15  15:54:01  kuipe_j
! tabs removed
!
! Revision 1.2  1995/05/30  07:08:36  hoeks_a
! file changed from dos to ux
!
! Revision 1.1  1995/04/13  07:10:57  hoeks_a
! Initial check-in
!
! Revision 1.1.1.1  1993/07/21  14:44:28  kuipe_j
! Initial version
!
!
!***********************************************************************
!
!       Parameters
!
   integer   igp,&
   &isecwq,&
   &ngrid

   real      qcalc

   real      qaggr (ngrid,3)

!
!     Determine flow through section
!
   if      (isecwq .eq. 0) then
      qcalc = qaggr (igp,1)
   else if (isecwq .eq. 1) then
      qcalc = qaggr (igp,2)
   else if (isecwq .eq. 2) then
      qcalc = qaggr (igp,3)
   else
      qcalc = qaggr (igp,1) - qaggr (igp,2) - qaggr (igp,3)
   endif

   return
end
