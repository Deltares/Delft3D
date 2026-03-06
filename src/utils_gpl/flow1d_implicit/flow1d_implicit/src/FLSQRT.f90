function FLSQRT (root)
!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Flow module
!
! Programmer:         P.R. Evans
!
! Module:             Flow calculate SQuare RooT
!
! Module description: Used in structure routines to evaluate the root
!                     of a value which may be negative.
!                     If x < 0 then FLSQRT = -sqrt(-x)
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
! $Log: flsqrt.pf,v $
! Revision 1.2  1998/06/08  12:35:53  kuipe_j
! log added
!
!
!
!***********************************************************************
!
! declare arguments
   real root

!
! declare function
   real FLSQRT

   if (root .lt. 0) then
      FLSQRT = -SQRT(-root)
   else
      FLSQRT = SQRT(root)
   endif

end
