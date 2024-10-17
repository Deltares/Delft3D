function EPSEQU (rvar1, rvar2, eps)

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          General routines Module
!
! Programmer:         J.Brouwer
!
! Module:             EPSEQU (EQUal test with interval EPSilon)
!
! Module description: Logical function to check if the difference be-
!                     tween two real values is lower than a defined
!                     interval epsilon.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  3 eps               I  Interval epsilon.
!  0 epsequ            O  Function value, TRUE/FALSE.
!  1 rvar1             I  Real variable.
!  2 rvar2             I  Real variable.
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
! $Log: epsequ.pf,v $
! Revision 1.2  1995/05/30  07:02:22  hoeks_a
! file converted from dos to ux
!
! Revision 1.1  1995/04/13  07:08:25  hoeks_a
! Initial check-in
!
! Revision 1.1.1.1  1993/07/21  14:43:59  kuipe_j
! Initial version
!
!
!***********************************************************************
!
!     Declaration of function:
!
   logical EPSEQU
!
!     Declaration of parameters:
!
   real    rvar1, rvar2, eps
!
   EPSEQU = abs(rvar1 - rvar2) .lt. eps
end
