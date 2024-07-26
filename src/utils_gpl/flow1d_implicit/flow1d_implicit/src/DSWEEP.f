      subroutine DSWEEP(n      ,ngridm ,
     +                  b1     ,a1     ,d1     ,c1     ,e1     ,
     +                  b2     ,a2     ,d2     ,c2     ,e2     ,
     +                  r2     ,f2     ,v2     ,
     +                  r1     ,f1     ,v1     )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          General routines Module
c
c Programmer:         J.Brouwer
c
c Module:             DSWEEP (Double SWEEP coefficients)
c
c Module description: Solve abcde coefficient arrays for one branch.
c
c                     Routine DSWEEP is programmed according to the
c                     algorithm described in the functional design for
c                     the water flow module. This routine will be used
c                     in the salt module and morphology module as well.
c                     
c                     From version 1.20 on the columns of the
c                     matrix are interchanged so closed structures do 
c                     not cause problems anymore.
c                     Columns: H Q Q H Q H Q H .... 
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  3 a1(ngridm)        IO A1-coefficient of continuity eq. per gridpnt.
c  8 a2(ngridm)        IO A2-coefficient of momentum equation c.q. sta-
c                         ge-discharge equation per gridpoint.
c  4 b1(ngridm)        I  B1-coefficient of continuity eq. per gridpnt.
c  9 b2(ngridm)        I  B2-coefficient of momentum equation c.q. sta-
c                         ge-discharge equation per gridpoint.
c  5 c1(ngridm)        IO C1-coefficient of continuity eq. per gridpnt.
c 10 c2(ngridm)        I  C2-coefficient of momentum equation c.q. sta-
c                         ge-discharge equation per gridpoint.
c  6 d1(ngridm)        IO D1-coefficient of continuity eq. per gridpnt.
c 11 d2(ngridm)        IO D2-coefficient of momentum equation c.q. sta-
c                         ge-discharge equation per gridpoint.
c  7 e1(ngridm)        IO E1-coefficient of continuity eq. per gridpnt.
c                         A1 = A1(1)+A1(2)+A1(3)  , etc.
c 12 e2(ngridm)        IO E2-coefficient of momentum equation c.q. sta-
c                         ge-discharge equation per gridpoint.
c 14 f1(ngrid)         IO f1-coefficients (2*i-1,N) of set of branch
c                         equations. One value per grid point.
c 17 f2(ngrid)         IO f2-coefficients (2*i,N) of set of branch equa-
c                         tions. One value per grid point.
c  1 n                 I  Number of piers.
c  2 ngridm            I  Maximum number of gridpoints in a branch.
c 13 r1(ngrid)         IO r1-coefficients (2*i-1,1) of set of branch
c                         equations. One value per grid point.
c 16 r2(ngrid)         IO r2-coefficients (2*i,1) of set of branch equa-
c                         tions. One value per grid point.
c 15 v1(ngrid)         IO Right-hand-sides (2*i-1) of set of branch
c                         equations. One value per grid point.
c 18 v2(ngrid)         IO Right-hand-sides (2*i) of set of branch equa-
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
c $Log: dsweep.pf,v $
c Revision 1.6  1999/03/15  15:51:14  kuipe_j
c tabs removed
c
c Revision 1.5  1997/05/06  07:36:39  kuipe_j
c comments
c
c Revision 1.4  1997/05/05  14:32:54  kuipe_j
c Q and H coefficients are interchaged(2)
c
c Revision 1.3  1997/02/17  10:07:02  kuipe_j
c Q and H coefficients are interchanged
c
c Revision 1.2  1995/05/30  07:02:20  hoeks_a
c file converted from dos to ux
c
c Revision 1.1  1995/04/13  07:08:24  hoeks_a
c Initial check-in
c
c Revision 1.1.1.1  1993/07/21  14:43:58  kuipe_j
c Initial version
c
c
c***********************************************************************
c
c     Declaration of parameters
c
      integer           n, ngridm
      double precision  a1(ngridm),
     +                  b1(ngridm),
     +                  c1(ngridm),
     +                  d1(ngridm),
     +                  e1(ngridm)
      double precision  a2(ngridm),
     +                  b2(ngridm),
     +                  c2(ngridm),
     +                  d2(ngridm),
     +                  e2(ngridm)
      double precision  r1(n), f1(n), v1(n)
      double precision  r2(n), f2(n), v2(n)
c
c     Declaration of local variables
c
      integer           i    ,i1
      double precision  d2im1, e2im1, g2im1, g1i, g2i, g1n, g2n
      double precision  fact , fact1, fact2
      double precision  t    , r1t  , f1t  , v1t, r2t, f2t, v2t 

      t     = a1(1)
      a1(1) = b1(1)
      b1(1) = t
      t     = a2(1)
      a2(1) = b2(1)
      b2(1) = t    
c
c     *******************
c     *  Forward Sweep  *
c     *******************
c
c     Assign starting values (i=0)
c
c     Doc: S-FO-001.5KV  / Eq. 9-6
c
      g2im1 = -1.0D0
      d2im1 =  0.0D0
      e2im1 =  0.0D0
c
c     Doc: S-FO-001.5KV  / Eq. 9-5
c
      do 10 i = 1, n
c
c        Compute (reversed) common factor
c
         if ( b1(i) - a1(i) * d2im1 .EQ. 0.D0)  then
            write (*,*) 'fact',i
            stop
         endif
         fact  = 1.D0 / ( b1(i) - a1(i) * d2im1 )
c
c        Compute g1,d1 and e1
c
         g1i   = -a1(i) * g2im1 * fact
         c1(i) = c1(i) * fact
         d1(i) = d1(i) * fact
         e1(i) = ( e1(i) - a1(i) * e2im1 ) * fact
c
c        Compute common factor
c
         fact1 = b2(i) - a2(i) * d2im1

         if ( c2(i) - c1(i) * fact1 .EQ. 0.D0)  then
            write (*,*) 'fact2',i
            stop
         endif
         fact2 = 1.D0 / ( c2(i) - c1(i) * fact1 )
c
c        Compute g2,d2 and e2
c
         g2i   = ( -a2(i) * g2im1 - g1i * fact1 ) * fact2
         d2(i) = ( d2(i) - d1(i) * fact1 ) * fact2
         e2(i) = ( e2(i) - a2(i) * e2im1 - fact1 * e1(i) ) * fact2
c
c        Assign g1i and g2i to a1(i) and a2(i)
c
         a1(i) = g1i
         a2(i) = g2i
         g2im1 = g2i
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
      g1n = a1(n)
      g2n = a2(n)
c
      r2t = g2n 
      f2t = d2(n)
      v2t = e2(n)
      r1t = -c1(n) * r2t + g1n
      f1t = -c1(n) * f2t + d1(n)
      v1t = -c1(n) * v2t + e1(n)
c
c     Doc: S-FO-001.5KV  / Eq. 9-9
c
      do 20 i = n-1, 1, -1
c
         r1(i)  = r1t
         f1(i)  = f1t
         v1(i)  = v1t

         g1i    = a1(i)
         g2i    = a2(i)
         i1     = i+1
c
         r2(i1) = g2i - r1t * d2(i)

         f2(i1) = -f1t * d2(i)
        
         v2(i1) = e2(i) - v1t * d2(i)

         r1t    = g1i - r1t * d1(i) - c1(i) * r2(i1) 
         
         f1t    = f1t * (c1(i) * d2(i) - d1(i))

         v1t    = e1(i) - v1t * d1(i) - c1(i) * v2(i1)

   20 continue

      r2(1) = r1t
      f2(1) = f1t
      v2(1) = v1t
      r1(n) = r2t
      f1(n) = f2t
      v1(n) = v2t

      end
