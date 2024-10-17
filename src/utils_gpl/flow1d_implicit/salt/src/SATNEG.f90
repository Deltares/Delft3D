subroutine satneg (ngrid ,ceps ,csa2 ,filter)

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Salt Transport Module
!
! Programmer:         J.Kuipers
!
! Module:             SATNEG (SAlt Test NEGative concentrations)
!
! Module description: Test for negative concentrations in network
!
!                     Test if all concentrations do have a value greater
!                     then cs,eps. If this is not the case negative
!                     concentrations occur.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  2 ceps              I  Minimum allowed concentration.
!  3 csa2(ngrid)       I  Salt concentration in every grid point at time
!                         t(n+1).
!  4 filter            O  = True if there are salt concentrations with a
!                         value lower than the minimum value (ceps).
!  1 ngrid             I  Number of grid points in network.
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
! $Log: satneg.pf,v $
! Revision 1.2  1995/05/30  07:06:22  hoeks_a
! file changed from dos to ux
!
! Revision 1.1  1995/04/13  07:10:03  hoeks_a
! Initial check-in
!
! Revision 1.1.1.1  1993/07/21  14:44:16  kuipe_j
! Initial version
!
!
!***********************************************************************
!
!     Declaration of parameters
!
   integer    ngrid
   real       ceps
   real       csa2(ngrid)
   logical    filter
!
!     Declaration of local variables
!
   integer    igr
!
   filter  = .false.
   do 10 igr = 1,ngrid
      if (csa2(igr) .lt. ceps) then
         filter  = .true.
      endif
10 continue
!
end
