function FLWAPI (x,d)
!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Flow module
!
! Programmer:         P.R. Evans
!
! Module:             FLWAPI (FLow Wet Area in circular PIpe)
!
! Module description: Calculate the wat area in a circular cross
!                     section. This function is used in the discharge
!                     calculation for a culvert. Code taken from WENDY.
!                     WENDY history is:
!                       Projekt: Construction-Module
!                       Programmeur: H. van Zanten
!                       Funktie: Calculation of the wet area in a
!                                circular section
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
! $Log: flwapi.pf,v $
! Revision 1.2  1998/06/08  12:35:54  kuipe_j
! log added
!
!
!
!***********************************************************************
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
! declare function
   real FLWAPI

   if (x .lt. 0.) x = 0.
   if (x .gt. 1.) x = 1.
   FLWAPI = (-0.8895*x*x*x + 1.33425*x*x + 0.33939*x) * d * d

   return
end
