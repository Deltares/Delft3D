      subroutine KASWPR(n      ,ngridm ,
     +                  b1     ,a1     ,d1     ,c1     ,e1     ,
     +                  b2     ,a2     ,d2     ,c2     ,e2     ,
     +                  v2     ,
     +                  v1     )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Kalman module
c
c Programmer:         J.Brouwer/J.Kuipers
c
c Module:             KASWPR (KAlman double SWeeP R.h.side)
c
c Module description: In routine KASWPR a double sweep algorithm is
c                     performed on the coefficients of the "covariance
c                     equations" similar to the algorithm used by DSWEEP.
c                     Only the coefficients to be used in the right hand
c                     side will be calcuated (v1, v2).
c
c                     The formulaes used are described in eq. 9-5 till
c                     9-9 in [S-FO-001]. Remind that here Delta-Q and
c                     h are replaced by the covariances of Q and h.
c
c                     From version 1.20 on the columns of the
c                     matrix are interchanged so closed structures do 
c                     not cause problems anymore.
c                     Columns: H Q Q H Q H Q H .... 
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  3 a1(ngridm)        I  A1-coefficient of continuity eq. per gridpnt.
c  8 a2(ngridm)        I  A2-coefficient of momentum equation c.q. sta-
c                         ge-discharge equation per gridpoint.
c  4 b1(ngridm)        I  B1-coefficient of continuity eq. per gridpnt.
c  9 b2(ngridm)        I  B2-coefficient of momentum equation c.q. sta-
c                         ge-discharge equation per gridpoint.
c  5 c1(ngridm)        I  C1-coefficient of continuity eq. per gridpnt.
c 10 c2(ngridm)        I  C2-coefficient of momentum equation c.q. sta-
c                         ge-discharge equation per gridpoint.
c  6 d1(ngridm)        I  D1-coefficient of continuity eq. per gridpnt.
c 11 d2(ngridm)        I  D2-coefficient of momentum equation c.q. sta-
c                         ge-discharge equation per gridpoint.
c  7 e1(ngridm)        IO E1-coefficient of continuity eq. per gridpnt.
c                         A1 = A1(1)+A1(2)+A1(3)  , etc.
c 12 e2(ngridm)        IO E2-coefficient of momentum equation c.q. sta-
c                         ge-discharge equation per gridpoint.
c  1 n                 I  -
c  2 ngridm            I  Maximum number of gridpoints in a branch.
c 13 v1(ngrid)         IO Right-hand-sides (2*i-1) of set of branch
c                         equations. One value per grid point.
c 14 v2(ngrid)         IO Right-hand-sides (2*i) of set of branch equa-
c                         tions. One value per grid point.
c=======================================================================
c
c
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: kaswpr.pf,v $
c Revision 1.5  1999/03/15  15:52:26  kuipe_j
c tabs removed
c
c Revision 1.4  1997/05/06  07:35:29  kuipe_j
c comments
c
c Revision 1.3  1997/05/05  14:31:05  kuipe_j
c Q and H coefficients are interchaged
c
c Revision 1.2  1996/04/12  13:05:33  kuipe_j
c headers, minor changes
c
c Revision 1.1  1996/04/11  08:25:07  kuipe_j
c Kalman module added
c
c
c***********************************************************************
c
c     Declaration of parameters
c
      integer           n, ngridm
      double precision  a1(n),
     +                  b1(n),
     +                  c1(n),
     +                  d1(n),
     +                  e1(ngridm)
      double precision  a2(n),
     +                  b2(n),
     +                  c2(n),
     +                  d2(n),
     +                  e2(ngridm)
      double precision  v1(n)
      double precision  v2(n)
c
c     Declaration of local variables
c
      integer           i ,i1
      double precision  d2im1, e2im1
      double precision  fact, fact1, fact2
      double precision  v1t ,v2t            
c
c     The first 2 columns are already interchanged in routine 
c     KASWPC (a1,b1 and a2,b2)
c
c     *******************
c     *  Forward Sweep  *
c     *******************
c
c     Assign starting values (i=0)
c
c     Doc: S-FO-001.5KV  / Eq. 9-6
c
      d2im1 =  0.0D0
      e2im1 =  0.0D0
c
c     Doc: S-FO-001.5KV  / Eq. 9-5
c
      do 10 i = 1, n
c
c        Compute (reversed) common factor
c
         fact  = 1.D0 / ( b1(i) - a1(i) * d2im1 )
c
c        Compute e1
c
         e1(i) = ( e1(i) - a1(i) * e2im1 ) * fact
c
c        Compute common factor
c
         fact1 = b2(i) - a2(i) * d2im1
         fact2 = 1.D0 / ( c2(i) - c1(i) * fact1 )
c
c        Compute e2
c
         e2(i) = ( e2(i) - a2(i) * e2im1 - fact1 * e1(i) ) * fact2
c
c        Assign d2im1 and e2im1
c
         d2im1 = d2(i)
         e2im1 = e2(i)
   10 continue
c
c     ********************
c     *  Backward Sweep  *
c     ********************
c
c     Assign starting values (i = n)
c
c     Doc: S-FO-001.5KV  / Eq. 9-10
c
      v2t   = e2(n)
      v1t   = -c1(n) * v2t + e1(n)
c
c     Doc: S-FO-001.5KV  / Eq. 9-9
c
      do 20 i = n-1, 1, -1
c
         v1(i)  = v1t
         i1     = i + 1
         v2(i1) = e2(i) - v1t * d2(i)

         v1t    = e1(i) - v1t * d1(i) -  c1(i) * v2(i1)

   20 continue
c
      v2(1) = v1t
      v1(n) = v2t
c
      end
