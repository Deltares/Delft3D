subroutine gsdscp (nfrac  ,nbran  ,dis1d  )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Graded Sediment Transport Module
!
! Programmer:         J.Kuipers
!
! Module:             GSDSCP (Graded Sediment DiStribute COpy)
!
! Module description: Total (distributed) transports are stored
!                     according to declaration DISSED. They are
!                     copied in the same array in such a manner
!                     that it satisfies the declaration DISGSE.
!                     The arrays
!                     DISSED(4,nbran),
!                     DISGSE(nfrac,2,nbran) and
!                     DIS1D(*)
!                     share the same space.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  3 nbran             I  Number of branches.
!=======================================================================
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: $
!
!***********************************************************************
!
!     Declaration of parameters
!
   integer    nfrac   ,nbran
   real       dis1d(*)
!
!     Declaration of local parameters
!
   integer    ibr
   real       sbegin,seind
   if (nfrac .gt. 2) then
      do 10 ibr=nbran,1,-1
!                    dissed(1,ibr)
         sbegin = dis1d (1+(ibr-1)*4)
!                    dissed(3,ibr)
         seind  = dis1d (3+(ibr-1)*4)
!           disgse(1,ibr)
         dis1d (1+      (ibr-1)*nfrac*2) = sbegin
!           disgse(1+nfrac,ibr)
         dis1d (1+nfrac+(ibr-1)*nfrac*2) = seind
10    continue
   else if (nfrac.eq.1) then
      do 20 ibr=1,nbran
!                    dissed(1,ibr)
         sbegin = dis1d (1+(ibr-1)*4)
!                    dissed(3,ibr)
         seind  = dis1d (3+(ibr-1)*4)
!           disgse(1,ibr)
         dis1d (ibr*2-1) = sbegin
!           disgse(1+nfrac,ibr)
         dis1d (ibr*2)   = seind
20    continue
   endif

end
