function FLHRPI (x,d)
!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Flow module
!
! Programmer:         P.R. Evans
!
! Module:             FLHRPI (FLow Hydraulic Radius in circular PIpe)
!
! Module description: Calculate the hydraulic radius in a circular cross
!                     section. This function is used in the discharge
!                     calculation for a culvert. Code taken from WENDY.
!                     WENDY history is:
!                       Projekt: Construction-Module
!                       Programmeur: H. van Zanten
!                       Funktie: Calculation of the hydraulic radius in
!                                a circular section
!                       No updates.
!
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
!=======================================================================
!
!***********************************************************************
! CVS log information:
!
!
! $Id$
!
! History:
! $Log: flhrpi.pf,v $
! Revision 1.2  1998/06/08  12:35:34  kuipe_j
! log added
!
!
!
!***********************************************************************
!
!  Parameters  (Input / Output) :
!  ------------------------------
!
!   Number    Name         I/O    Description
!   ------    ----         ---    ------------
!      1      x             I     parameter
!      2      d             I     diameter circular section
!
!***********************************************************************

!
! declare arguments
   real x, d

!
! declare variables
   real x2, x3, x4, x5, corr

!
! declare function
   real FLHRPI

   if (x .lt. 0.) x = 0.
   if (x .gt. 1.) x = 1.
   x2     = x * x
   x3     = x * x2
   x4     = x * x3
   x5     = x * x4
   corr   = .152*x5 - 5.449*x4 + 11.739*x3 - 8.771*x2 + 2.5486*x+.745
   FLHRPI = (-0.3629*x3 + 0.05742*x2 + 0.55739*x) * d * corr

   return
end
