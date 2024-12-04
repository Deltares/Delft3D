subroutine KASWPR(n      ,ngridm ,&
&b1     ,a1     ,d1     ,c1     ,e1     ,&
&b2     ,a2     ,d2     ,c2     ,e2     ,&
&v2     ,&
&v1     )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Kalman module
!
! Programmer:         J.Brouwer/J.Kuipers
!
! Module:             KASWPR (KAlman double SWeeP R.h.side)
!
! Module description: In routine KASWPR a double sweep algorithm is
!                     performed on the coefficients of the "covariance
!                     equations" similar to the algorithm used by DSWEEP.
!                     Only the coefficients to be used in the right hand
!                     side will be calcuated (v1, v2).
!
!                     The formulaes used are described in eq. 9-5 till
!                     9-9 in [S-FO-001]. Remind that here Delta-Q and
!                     h are replaced by the covariances of Q and h.
!
!                     From version 1.20 on the columns of the
!                     matrix are interchanged so closed structures do
!                     not cause problems anymore.
!                     Columns: H Q Q H Q H Q H ....
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  3 a1(ngridm)        I  A1-coefficient of continuity eq. per gridpnt.
!  8 a2(ngridm)        I  A2-coefficient of momentum equation c.q. sta-
!                         ge-discharge equation per gridpoint.
!  4 b1(ngridm)        I  B1-coefficient of continuity eq. per gridpnt.
!  9 b2(ngridm)        I  B2-coefficient of momentum equation c.q. sta-
!                         ge-discharge equation per gridpoint.
!  5 c1(ngridm)        I  C1-coefficient of continuity eq. per gridpnt.
! 10 c2(ngridm)        I  C2-coefficient of momentum equation c.q. sta-
!                         ge-discharge equation per gridpoint.
!  6 d1(ngridm)        I  D1-coefficient of continuity eq. per gridpnt.
! 11 d2(ngridm)        I  D2-coefficient of momentum equation c.q. sta-
!                         ge-discharge equation per gridpoint.
!  7 e1(ngridm)        IO E1-coefficient of continuity eq. per gridpnt.
!                         A1 = A1(1)+A1(2)+A1(3)  , etc.
! 12 e2(ngridm)        IO E2-coefficient of momentum equation c.q. sta-
!                         ge-discharge equation per gridpoint.
!  1 n                 I  -
!  2 ngridm            I  Maximum number of gridpoints in a branch.
! 13 v1(ngrid)         IO Right-hand-sides (2*i-1) of set of branch
!                         equations. One value per grid point.
! 14 v2(ngrid)         IO Right-hand-sides (2*i) of set of branch equa-
!                         tions. One value per grid point.
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
! $Log: kaswpr.pf,v $
! Revision 1.5  1999/03/15  15:52:26  kuipe_j
! tabs removed
!
! Revision 1.4  1997/05/06  07:35:29  kuipe_j
! comments
!
! Revision 1.3  1997/05/05  14:31:05  kuipe_j
! Q and H coefficients are interchaged
!
! Revision 1.2  1996/04/12  13:05:33  kuipe_j
! headers, minor changes
!
! Revision 1.1  1996/04/11  08:25:07  kuipe_j
! Kalman module added
!
!
!***********************************************************************
!
!     Declaration of parameters
!
   integer           n, ngridm
   double precision  a1(n),&
   &b1(n),&
   &c1(n),&
   &d1(n),&
   &e1(ngridm)
   double precision  a2(n),&
   &b2(n),&
   &c2(n),&
   &d2(n),&
   &e2(ngridm)
   double precision  v1(n)
   double precision  v2(n)
!
!     Declaration of local variables
!
   integer           i ,i1
   double precision  d2im1, e2im1
   double precision  fact, fact1, fact2
   double precision  v1t ,v2t
!
!     The first 2 columns are already interchanged in routine
!     KASWPC (a1,b1 and a2,b2)
!
!     *******************
!     *  Forward Sweep  *
!     *******************
!
!     Assign starting values (i=0)
!
!     Doc: S-FO-001.5KV  / Eq. 9-6
!
   d2im1 =  0.0D0
   e2im1 =  0.0D0
!
!     Doc: S-FO-001.5KV  / Eq. 9-5
!
   do 10 i = 1, n
!
!        Compute (reversed) common factor
!
      fact  = 1.D0 / ( b1(i) - a1(i) * d2im1 )
!
!        Compute e1
!
      e1(i) = ( e1(i) - a1(i) * e2im1 ) * fact
!
!        Compute common factor
!
      fact1 = b2(i) - a2(i) * d2im1
      fact2 = 1.D0 / ( c2(i) - c1(i) * fact1 )
!
!        Compute e2
!
      e2(i) = ( e2(i) - a2(i) * e2im1 - fact1 * e1(i) ) * fact2
!
!        Assign d2im1 and e2im1
!
      d2im1 = d2(i)
      e2im1 = e2(i)
10 continue
!
!     ********************
!     *  Backward Sweep  *
!     ********************
!
!     Assign starting values (i = n)
!
!     Doc: S-FO-001.5KV  / Eq. 9-10
!
   v2t   = e2(n)
   v1t   = -c1(n) * v2t + e1(n)
!
!     Doc: S-FO-001.5KV  / Eq. 9-9
!
   do 20 i = n-1, 1, -1
!
      v1(i)  = v1t
      i1     = i + 1
      v2(i1) = e2(i) - v1t * d2(i)

      v1t    = e1(i) - v1t * d1(i) -  c1(i) * v2(i1)

20 continue
!
   v2(1) = v1t
   v1(n) = v2t
!
end
