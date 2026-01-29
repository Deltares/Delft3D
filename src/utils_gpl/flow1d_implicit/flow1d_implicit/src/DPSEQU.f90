function DPSEQU (dvar1, dvar2, eps)

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          General routines Module
!
! Programmer:         J.Brouwer
!
! Module:             DPSEQU (EQUal test with Double precision interval EPSilon)
!
! Module description: Logical function to check if the difference be-
!                     tween two double precision values is lower than a
!                     defined interval epsilon.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  0 dpsequ            O  -
!  1 dvar1             I  Double precision variable.
!  2 dvar2             I  Double precision variable.
!  3 eps               I  Interval epsilon.
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
! $Log: dpsequ.pf,v $
! Revision 1.1  1995/09/22  10:02:56  kuipe_j
! variable dimensions, new headers
!
!
!***********************************************************************
!
!     Declaration of function:
!
   logical DPSEQU
!
!     Declaration of parameters:
!
   double precision  dvar1, dvar2, eps
!
   DPSEQU = abs(dvar1 - dvar2) .lt. eps
end
