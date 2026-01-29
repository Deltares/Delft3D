subroutine KACVSM (np     ,matrix )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Kalman module
!
! Programmer:         J.Brouwer/J.Kuipers
!
! Module:             KACVSM (KALman make CoVariance SyMmetric)
!
! Module description: Make covariance matrix symmetric.
!
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  2 matrix            IO -
!  1 np                I  Size of covariance matrix (2*ngrid+nnf+nnmu+1)
!=======================================================================
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: kacvsm.pf,v $
! Revision 1.3  1999/03/15  15:51:38  kuipe_j
! tabs removed
!
! Revision 1.2  1996/04/12  13:04:42  kuipe_j
! headers, minor changes
!
! Revision 1.1  1996/04/11  08:24:18  kuipe_j
! Kalman module added
!
!
!***********************************************************************
!
!     Declaration of Parameters:
!
   integer  np
   real     matrix(np,np)
!
!     Declaration of local variables
!
   integer  i, j
!
!     Make covariance matrix synmetric.
!     [ Doc. S-FO-004.2PB / Eq. 2-18 ]
!
   do 20 i = 2, np
      do 10 j = 1, i-1
         matrix(i,j) = (matrix(i,j) + matrix(j,i)) / 2.
         matrix(j,i) = matrix(i,j)
10    continue
20 continue
!
end
