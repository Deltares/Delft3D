function EQUAL (rvar1, rvar2)

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          General routines Module
!
! Programmer:         J.Brouwer
!
! Module:             EQUAL (EQUAL test of two real variables)
!
! Module description: Logical function to check if two real values are
!                     identical.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  0 equal             O  Function value, TRUE/FALSE.
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
! $Log: equal.pf,v $
! Revision 1.2  1995/05/30  07:02:22  hoeks_a
! file converted from dos to ux
!
! Revision 1.1  1995/04/13  07:08:26  hoeks_a
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
   logical EQUAL
!
!     Declaration of parameters:
!
   real    rvar1, rvar2
!
   EQUAL = rvar1 .eq. rvar2
end
