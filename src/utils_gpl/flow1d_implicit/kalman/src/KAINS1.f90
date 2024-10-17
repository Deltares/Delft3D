subroutine KAINS1(cpredn ,predin ,lfilt  ,ngrid  ,wt1    ,wt     )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Kalman module
!
! Programmer:         J.Brouwer/J.Kuipers
!
! Module:             KAINS1 (KAlman Initialize Next Step 1)
!
! Module description: Shift predicted covariances to new time step if the
!                     previous step was not a filter step.
!                     Increment current prediction interval.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  1 cpredn            IO Current prediction interval.
!  3 lfilt             O  = True if a filter step must be performed.
!  4 ngrid             I  Number of grid points in network.
!  2 predin            I  Prediction interval.
!  6 wt(ngrid)         I  Actual total width at every grid point.
!  5 wt1(ngrid)        O  Total width in every grid point i on time n
!                         (previous time).
!=======================================================================
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: kains1.pf,v $
! Revision 1.3  1999/03/15  15:51:55  kuipe_j
! tabs removed
!
! Revision 1.2  1996/04/12  13:05:04  kuipe_j
! headers, minor changes
!
! Revision 1.1  1996/04/11  08:24:38  kuipe_j
! Kalman module added
!
!
!***********************************************************************
!
!     Declaration of Parameters:
!
   integer cpredn, predin,  ngrid
   real    wt1(ngrid), wt(ngrid)
   logical lfilt
!
!     Declaration of local variable:
!
   integer i

   cpredn = cpredn + 1
   if ( cpredn .gt. predin ) then
!
!        Set current prediction interval to zero.
!
      cpredn = 1
   endif
   if ( cpredn .eq. predin ) then
      lfilt = .true.
   else
      lfilt = .false.
   endif
!
   do 30 i = 1, ngrid
      wt1(i) = wt(i)
30 continue
!
end
