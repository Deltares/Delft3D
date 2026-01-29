subroutine KARHP1 (np     ,p1     ,p2     )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Kalman module
!
! Programmer:         J.Brouwer/J.Kuipers
!
! Module:             KARHP1 (KAlman add to Right Hand side Pq^t)
!
! Module description: The right hand side matrix for covariances
!                     B(.)Pp^t+Pq^t must be calculated. In this routine
!                     the last step of this calculation will be performed:
!                     Input: B(.)Pp^t (array P1)
!                            Pq.(array P2)
!                     Output: r.h.s (array P2)
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  1 np                I  Size of covariance matrix (2*ngrid+nnf+nnmu+1)
!  2 p1(np,np)         I  Matrix with covariances of waterlevels,
!                         discharges and uncertain correction parameters
!                         (bed friction, contraction in case of free gate
!                         flow and wind) on time level n+1|n+1 (filtered
!                         values) or n|n (previous time step).
!  3 p2(np,np)         IO Matrix with covariances of waterlevels,
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
! $Log: karhp1.pf,v $
! Revision 1.2  1996/04/12  13:05:21  kuipe_j
! headers, minor changes
!
! Revision 1.1  1996/04/11  08:24:54  kuipe_j
! Kalman module added
!
!
!***********************************************************************
!
!     Declaration of parameters
!
   integer np
   real    p1(np,np), p2(np,np)
!
!     Declaration of local variables
!
   integer i, j
   real    t1
!
   do 20 j = 2,np
      do 10 i = 1, j-1
         t1      = p2(j,i) + p1(i,j)
         p2(j,i) = p2(i,j) + p1(j,i)
         p2(i,j) = t1
10    continue
20 continue
!
   do 30 i = 1, np
      p2(i,i) = p2(i,i) + p1(i,i)
30 continue
!
end
