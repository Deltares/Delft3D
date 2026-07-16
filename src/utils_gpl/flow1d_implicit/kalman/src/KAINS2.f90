subroutine KAINS2(ngrid  ,h      ,q      )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Kalman module
!
! Programmer:         J.Brouwer/J.Kuipers
!
! Module:             KAINS2 (KAlman Initialize Next Step 2)
!
! Module description: Exchange the filtered and predicted discharges and
!                     water levels if it is a filter step. So in a next
!                     step the filtered values will become the values of
!                     the previous time step (h2 --> h1, q2 --> q1). i
!                     As a result also the predicted values are kept (h*,
!                     q*) for output purposes.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  2 h(ngrid)          IO Contains water levels in every grid point. It is
!                         stored on index 2 of the packed array hpack.
!                         Flow:        current values during iteration
!                         (h*).
!                         Prediction:  last iterated value.
!                         Update:      filtered value (n+1|n+1)
!  1 ngrid             I  Number of grid points in network.
!  3 q(ngrid)          IO Contains discharges in every grid point. It is
!                         stored on index 2 of the packed array qpack.
!                         Flow:        current values during iteration
!                         (h*).
!                         Prediction:  last iterated value.
!                         Update:      filtered value (n+1|n+1)
!=======================================================================
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: kains2.pf,v $
! Revision 1.3  1999/03/15  15:51:56  kuipe_j
! tabs removed
!
! Revision 1.2  1996/04/12  13:05:06  kuipe_j
! headers, minor changes
!
! Revision 1.1  1996/04/11  08:24:39  kuipe_j
! Kalman module added
!
!
!***********************************************************************
!
!     Declaration of Parameters:
!
   integer ngrid
   double precision h(ngrid,3), q(ngrid,3)
!
!     Declaration of local variable:
!
   integer i
   real    rdum
!
!     Exchange:  h2 <----> h*  and  q2 <----> q*
!
   do 10 i = 1, ngrid
      rdum   = h(i,2)
      h(i,2) = h(i,3)
      h(i,3) = rdum
      rdum   = q(i,2)
      q(i,2) = q(i,3)
      q(i,3) = rdum
10 continue
end
