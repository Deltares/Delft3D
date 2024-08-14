      function FLQHAW(g      ,istru  ,strsta ,strclo ,hu     ,hd     ,
     +                uu     ,h0     ,ka     ,kp     ,n      ,p      ,
     +                wn     ,zs     ,waw    ,elu    ,c0     ,ch     ,
     +                ct     ,x1     ,x12    ,x13    ,x2     ,ta     ,
     +                ta2    ,tb3    ,tb4    )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Flow Module
c
c Programmer:         J.Brouwer
c
c Module:             FLQHAW (FLow QH relation for Advanced Weir)
c
c Module description: Subroutine FLQHAW defines the QH-relationship for
c                     an advanced weir.
c
c                     In subroutine FLQHAW for given downstream and
c                     upstream water levels the discharge across the
c                     weir will be computed according to the specific
c                     stage-discharge equation (QH-relation) for the
c                     weir.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c 17 c0                IO -
c 18 ch                IO -
c 19 ct                IO -
c 16 elu               IO Upstream energy level.
c  0 flqhaw            O  Discharge across the advanced weir.
c  1 g                 I  Acceleration of gravity.
c  8 h0                I  Design head of the advanced weir.
c  6 hd                I  Downstream water level.
c  5 hu                I  Upstream water level.
c  2 istru             I  Number of structure.
c  9 ka                I  Abutment contraction coefficient.
c 10 kp                I  Pier contraction coefficient.
c 11 n                 I  Number of piers.
c 12 p                 I  Height of upstream face P.
c  4 strclo(nstru)     O  True if structure is closed.
c  3 strsta            I  Logical indicator.
c 25 ta2               IO -
c 24 ta                IO -
c 26 tb3               O  -
c 27 tb4               IO -
c  7 uu                I  Upstream velocity.
c 15 waw               O  -
c 13 wn                I  Total net width.
c 21 x12               IO -
c 20 x1                IO X-coordinate at begin of trajectory.
c 22 x13               IO -
c 23 x2                IO X-coordinate at end of trajectory.
c 14 zs                I  Bed level at centre of structure.
c=======================================================================
c
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: flqhaw.pf,v $
c Revision 1.9  1999/03/15  15:50:32  kuipe_j
c tabs removed
c
c Revision 1.8  1998/07/01  11:21:26  kuipe_j
c Improve test E < crest
c
c Revision 1.7  1997/11/26  14:50:33  kuipe_j
c x2 shoul not become zero
c
c Revision 1.6  1996/04/12  13:04:15  kuipe_j
c headers, minor changes
c
c Revision 1.5  1996/04/11  08:23:48  kuipe_j
c Kalman module added
c
c Revision 1.4  1995/08/23  14:29:26  overmar
c Lelystad juli 95 ingebracht
c
c Revision 1.3  1995/05/30  09:55:20  hoeks_a
c Minor changes
c
c Revision 1.2  1995/05/30  06:59:20  hoeks_a
c file converted from dos to ux
c
c Revision 1.1  1995/04/13  07:08:01  hoeks_a
c Initial check-in
c
c Revision 1.1.1.1  1993/07/21  14:43:53  kuipe_j
c Initial version
c
c
c***********************************************************************
c
c     Declaration of function
c
      real    FLQHAW
c
c     Declaration of parameters:
c
      integer istru
      logical strsta, strclo(*)
      real    g, hd, hu, uu, h0, ka, kp, n, p, wn, zs, waw, elu
      real    ch, ct, c0, x1, x12, x13, ta, ta2, tb3, tb4,  x2,
     +        elucrs 
c
c     Declaration of local variables:
c
      real    c, cp, corr, w, x, x3, x4, x5
c
c     [Doc: S-DO-001.2AK  Eqs. 11 - 24]
c
      elu = hu + uu*uu / (2.0*g)
      elucrs = elu - zs
      if ( elucrs .le. 0.0) then
         if ( strsta ) then
            strclo(istru) = .true.
         endif
         FLQHAW = 0.
      else
         if ( strsta ) then
            strclo(istru) = .false.
         endif

         w = wn - 2.0 * (n*kp + ka) * elucrs
         x = p / h0

         x2 = x * x
         x3 = x2 * x
         x4 = x2 * x2
         x5 = x3 * x2
c
c        Calculate basic correction coefficient -c0-
c
         if ( x .lt. 2.0 ) then
            corr = (-0.052)*x3 + (0.145)*x2 -(0.096)*x + 1.01
            c0   = ( (0.1256)*x5 - (1.0178)*x4 + 3.0*x3 - (3.94)*x2 +
     +              (2.28)*x +1.66 ) * corr / sqrt(2.0*g)
         else
            c0   = 2.1549008 / sqrt(2.0*g)
         endif

         x1 = elucrs / h0

         x12 = x1 * x1
         x13 = x12 * x1
c
c        Correction coefficient for head / design head effects -ch-
c
         if ( x1 .lt. 1.6 ) then
            ch = (0.1394)*x13 - (0.416)*x12 + (0.488)*x1 + 0.785
         else
            ch = 1.0718224
         endif
c
c        Correction coefficient for apron effect -cp-
c
         cp = 1.0

         x2 = (elu-hd) / elucrs

         if ( x2 .le. 1.0e-10 ) then
            ct = 0.
         else if ( x2 .le. 0.7 ) then
            ta  =  x2-0.7
            ta2 =  ta * ta
            tb3 = -ta2 * ta
            tb4 =  ta2 * ta2
            ct = sqrt(1.0 - ta2 / 0.49) + 27 * tb4 * x2 * sqrt(x2)
         else
            ct = 1.0
         endif

         c = c0 * ch * cp
c
c        Compute discharge Q for an advanced weir
c
         FLQHAW = c * ct * w * sqrt(2.0*g) * elucrs * sqrt(elucrs)
      endif
      waw = w
c
      end
