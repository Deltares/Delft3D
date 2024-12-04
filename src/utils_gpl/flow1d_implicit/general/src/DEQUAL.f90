function DEQUAL (dvar1, dvar2)

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          General routines Module
!
! Programmer:         J.Brouwer
!
! Module:             DEQUAL (EQUAL test of two Double precision variables)
!
! Module description: Logical function to check if two double precision
!                     variables are identical.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  0 dequal            O  Function value, TRUE/FALSE.
!  1 dvar1             I  Double precision variable.
!  2 dvar2             I  Double precision variable.
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
! $Log: dequal.pf,v $
! Revision 1.2  1995/05/30  07:02:19  hoeks_a
! file converted from dos to ux
!
! Revision 1.1  1995/04/13  07:08:23  hoeks_a
! Initial check-in
!
! Revision 1.1.1.1  1993/07/21  14:43:58  kuipe_j
! Initial version
!
!
!***********************************************************************
!
!     Declaration of function:
!
   logical DEQUAL
!
!     Declaration of parameters:
!
   double precision dvar1, dvar2
!
   DEQUAL = dvar1 .eq. dvar2
end
