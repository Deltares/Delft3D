subroutine KAINS3(np     ,p1     ,p2     )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Kalman module
!
! Programmer:         J.Brouwer/J.Kuipers
!
! Module:             KAINS3 (KAlman Initialize Next Step 3)
!
! Module description: Copy the predicted covariances from array P2 to P1,
!                     in case there is no filter step in the current
!
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  1 np                I  Size of covariance matrix (2*ngrid+nnf+nnmu+1)
!  2 p1(np,np)         O  Matrix with covariances of waterlevels,
!                         discharges and uncertain correction parameters
!                         (bed friction, contraction in case of free gate
!                         flow and wind) on time level n+1|n+1 (filtered
!                         values) or n|n (previous time step).
!  3 p2(np,np)         I  Matrix with covariances of waterlevels,
!                         discharges and uncertain correction parameters
!                         (bed friction, contraction in case of free gate
!                         flow and wind) on time level n+1|n (predicted
!                         values).
!=======================================================================
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: kains3.pf,v $
! Revision 1.2  1996/04/12  13:05:07  kuipe_j
! headers, minor changes
!
! Revision 1.1  1996/04/11  08:24:41  kuipe_j
! Kalman module added
!
!
!***********************************************************************
!
!     Declaration of Parameters:
!
   integer np
   real    p1(np,np), p2(np,np)
!
!     Declaration of local variable:
!
   integer i, j
!
   do 20 i = 1, np
      do 10 j = 1, np
         p1(i,j) = p2(i,j)
10    continue
20 continue
end
