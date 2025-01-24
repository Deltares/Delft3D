subroutine FLKAWP(nbran  ,ngrid  ,branch ,wfrict ,tauwi  ,pw     )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Flow Module
!
! Programmer:         J.Brouwer
!
! Module:             FLKAWP (FLow KAlman Wind correction Parameter)
!
! Module description: Correction on wind friction due to uncertain
!                     correction parameter.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  3 branch(4,nbran)   I  Branch information:
!                         (1,i) = Node number n1 at begin of branch i.
!                         (2,i) = Node number n2 at end of branch i.
!                         (3,i) = Grid point i1 at begin of branch i.
!                         (4,i) = Grid point i2 at end of branch i.
!  1 nbran             I  Number of branches.
!  2 ngrid             I  Number of grid points in network.
!  6 pw                I  Uncertain wind stress parameter.
!  5 tauwi(ngrid)      IO Calculated wind friction for each gridpoint.
!  4 wfrict(3,nbran)   I  Wind friction parameters in branch.
!                         (1,i) = Indicates wind defined for branch:
!                                 cnwndf (0) : No wind defined
!                                 cywndf (1) : Wind defined
!                         (2,i) = Table pointer for wind direction as a
!                                 function of time.
!                         (3,i) = Table pointer for wind velocity as a
!                                 function of time.
!=======================================================================
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: flkawp.pf,v $
! Revision 1.2  1996/04/12  13:04:03  kuipe_j
! headers, minor changes
!
! Revision 1.1  1996/04/11  08:23:35  kuipe_j
! Kalman module added
!
!
!***********************************************************************
!
!
!     Declaration of Parameters:
!
   integer nbran, ngrid
   integer branch(4,nbran), wfrict(3,nbran)
   real    pw(1), tauwi(ngrid)
!
!     Declaration of local variables:
!
   integer i, i1, i2, ibr
!
!     Include sobek constants
!
   include '..\include\sobcon.i'
!
   do 20 ibr = 1, nbran
      if (wfrict(1,ibr) .eq. cywndf) then
         i1 = branch (3,ibr)
         i2 = branch (4,ibr)
         do 10 i = i1, i2-1
            tauwi(i) = tauwi(i) * pw(1)
10       continue
      endif
20 continue
!
end
