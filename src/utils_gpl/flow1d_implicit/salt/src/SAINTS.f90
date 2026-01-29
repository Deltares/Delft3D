subroutine saints (ngrid  ,csa1  ,csa2   ,csd1  ,csd2)

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Salt Transport Module
!
! Programmer:         J.Kuipers
!
! Module:             SAINTS (SAlt INItialise)
!
! Module description: SAlt Initialise Next Time Step
!
!                     The resulting concentrations (Cs) and diffusion
!                     (C's) of time level n+1 will be copied to time
!                     level n.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  2 csa1(ngrid)       O  Salt concentration in every grid point at time
!                         t(n).
!  3 csa2(ngrid)       I  Salt concentration in every grid point at time
!                         t(n+1).
!  4 csd1(ngrid)       O  Diffusion (c s) in every grid point at time
!                         t(n).
!  5 csd2(ngrid)       I  Diffusion (c s) in every grid point at time
!                         t(n+1).
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
! $Log: saints.pf,v $
! Revision 1.3  1995/10/18  09:00:25  kuipe_j
! Changes concerning aux. ouput and IVR adjustments
!
! Revision 1.2  1995/05/30  07:06:07  hoeks_a
! file changed from dos to ux
!
! Revision 1.1  1995/04/13  07:09:49  hoeks_a
! Initial check-in
!
! Revision 1.1.1.1  1993/07/21  14:44:14  kuipe_j
! Initial version
!
!
!***********************************************************************
!
!     Declaration of parameters
!
   integer    ngrid
   real       csa1   (ngrid) ,csa2  (ngrid) ,csd1  (ngrid)  ,&
   &csd2   (ngrid)
!
!     Declaration of local variables
!
   integer    igr
!
   do 10 igr = 1,ngrid
      csa1(igr) =csa2(igr)
      csd1(igr) =csd2(igr)
10 continue
!
end
