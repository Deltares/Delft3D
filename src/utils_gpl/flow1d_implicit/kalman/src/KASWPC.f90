subroutine KASWPC(n      ,ngridm ,&
&b1     ,a1     ,d1     ,c1     ,&
&b2     ,a2     ,d2     ,c2     ,&
&r2     ,f2     ,g1     ,&
&r1     ,f1     ,g2     )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Kalman module
!
! Programmer:         J.Brouwer/J.Kuipers
!
! Module:             KASWPC (KAlman double SWeeP coefficients)
!
! Module description: In routine KASWPC a double sweep algorithm is
!                     performed on the coefficients of the "covariance
!                     equations" similar to the algorithm used by DSWEEP.
!                     Only the coefficients to be used in the coefficient
!                     matrix will be calcuated (r1, r2, f1, f2).
!
!                     The formulaes used are described in eq. 9-5 till
!                     9-9 in [S-FO-001]. Remind that here Delta-Q and h
!                     are replaced by the covariances of Q and h.
!
!                     From version 1.20 on the columns of the
!                     matrix are interchanged so closed structures do
!                     not cause problems anymore.
!                     Columns: H Q Q H Q H Q H ....
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  3 a1(ngridm)        I  A1-coefficient of continuity eq. per gridpnt.
!  7 a2(ngridm)        I  A2-coefficient of momentum equation c.q. sta-
!                         ge-discharge equation per gridpoint.
!  4 b1(ngridm)        I  B1-coefficient of continuity eq. per gridpnt.
!  8 b2(ngridm)        I  B2-coefficient of momentum equation c.q. sta-
!                         ge-discharge equation per gridpoint.
!  5 c1(ngridm)        IO C1-coefficient of continuity eq. per gridpnt.
!  9 c2(ngridm)        I  C2-coefficient of momentum equation c.q. sta-
!                         ge-discharge equation per gridpoint.
!  6 d1(ngridm)        IO D1-coefficient of continuity eq. per gridpnt.
! 10 d2(ngridm)        IO D2-coefficient of momentum equation c.q. sta-
!                         ge-discharge equation per gridpoint.
! 12 f1(ngrid)         IO f1-coefficients (2*i-1,N) of set of branch
!                         equations. One value per grid point.
! 15 f2(ngrid)         IO F2 coefficient of momentum equation
! 13 g1(ngridm)        IO Coefficient g1 after forward sweep.
! 16 g2(ngridm)        IO Coefficient g2 after forward sweep.
!  1 n                 I  -
!  2 ngridm            I  Maximum number of gridpoints in a branch.
! 11 r1(ngrid)         IO r1-coefficients (2*i-1,1) of set of branch
!                         equations. One value per grid point.
! 14 r2(ngrid)         IO r2-coefficients (2*i,1) of set of branch equa-
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
! $Log: kaswpc.pf,v $
! Revision 1.5  1999/03/15  15:52:25  kuipe_j
! tabs removed
!
! Revision 1.4  1997/05/06  07:35:28  kuipe_j
! comments
!
! Revision 1.3  1997/05/05  14:31:14  kuipe_j
! Q and H coefficients are interchaged
!
! Revision 1.2  1996/04/12  13:05:32  kuipe_j
! headers, minor changes
!
! Revision 1.1  1996/04/11  08:25:06  kuipe_j
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
   &g1(ngridm)
   double precision  a2(n),&
   &b2(n),&
   &c2(n),&
   &d2(n),&
   &g2(ngridm)
   double precision  r1(n), f1(n)
   double precision  r2(n), f2(n)
!
!     Declaration of local variables
!
   integer           i  ,i1
   double precision  d2im1, g2im1
   double precision  fact, fact1, fact2
   double precision  r1t, r2t, f1t, f2t, t
!
   t     = a1(1)
   a1(1) = b1(1)
   b1(1) = t
   t     = a2(1)
   a2(1) = b2(1)
   b2(1) = t
!
!     *******************
!     *  Forward Sweep  *
!     *******************
!
!     Assign starting values (i=0)
!
!     Doc: S-FO-001.5KV  / Eq. 9-6
!
   g2im1 = -1.0D0
   d2im1 =  0.0D0
!
!     Doc: S-FO-001.5KV  / Eq. 9-5
!
   do 10 i = 1, n
!
!        Compute (reversed) common factor
!
      fact  = 1.D0 / ( b1(i) - a1(i) * d2im1 )
!
!        Compute g1 and d1
!
      g1(i) = -a1(i) * g2im1 * fact
      c1(i) = c1(i) * fact
      d1(i) = d1(i) * fact
!
!        Compute common factor
!
      fact1 = b2(i) - a2(i) * d2im1
      fact2 = 1.D0 / ( c2(i) - c1(i) * fact1 )
!
!        Compute g2 and d2
!
      g2(i) = ( -a2(i) * g2im1 - g1(i) * fact1 ) * fact2
      d2(i) = ( d2(i) - d1(i) * fact1 ) * fact2
!
!        Assign g2im1 and d2im1
!
      g2im1 = g2(i)
      d2im1 = d2(i)
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
   r2t   = g2(n)
   f2t   = d2(n)
   r1t   = -c1(n) * r2t + g1(n)
   f1t   = -c1(n) * f2t + d1(n)
!
!     Doc: S-FO-001.5KV  / Eq. 9-9
!
   do 20 i = n-1, 1, -1
      r1(i)  = r1t
      f1(i)  = f1t
      i1     = i+1
!
      r2(i1) = g2(i) - r1t * d2(i)
!
      f2(i1) = -f1t * d2(i)
!
      r1t    =  g1(i) - r1t * d1(i) - c1(i) * r2(i1)
!
      f1t    =  f1t * (c1(i) * d2(i) - d1(i))
!
20 continue
!
   r2(1) = r1t
   f2(1) = f1t
   r1(n) = r2t
   f1(n) = f2t
!
end
