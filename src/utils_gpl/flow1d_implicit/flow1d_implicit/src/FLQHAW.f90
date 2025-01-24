function FLQHAW(g      ,istru  ,strsta ,strclo ,hu     ,hd     ,&
&uu     ,h0     ,ka     ,kp     ,n      ,p      ,&
&wn     ,zs     ,waw    ,elu    ,c0     ,ch     ,&
&ct     ,x1     ,x12    ,x13    ,x2     ,ta     ,&
&ta2    ,tb3    ,tb4    )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Flow Module
!
! Programmer:         J.Brouwer
!
! Module:             FLQHAW (FLow QH relation for Advanced Weir)
!
! Module description: Subroutine FLQHAW defines the QH-relationship for
!                     an advanced weir.
!
!                     In subroutine FLQHAW for given downstream and
!                     upstream water levels the discharge across the
!                     weir will be computed according to the specific
!                     stage-discharge equation (QH-relation) for the
!                     weir.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
! 17 c0                IO -
! 18 ch                IO -
! 19 ct                IO -
! 16 elu               IO Upstream energy level.
!  0 flqhaw            O  Discharge across the advanced weir.
!  1 g                 I  Acceleration of gravity.
!  8 h0                I  Design head of the advanced weir.
!  6 hd                I  Downstream water level.
!  5 hu                I  Upstream water level.
!  2 istru             I  Number of structure.
!  9 ka                I  Abutment contraction coefficient.
! 10 kp                I  Pier contraction coefficient.
! 11 n                 I  Number of piers.
! 12 p                 I  Height of upstream face P.
!  4 strclo(nstru)     O  True if structure is closed.
!  3 strsta            I  Logical indicator.
! 25 ta2               IO -
! 24 ta                IO -
! 26 tb3               O  -
! 27 tb4               IO -
!  7 uu                I  Upstream velocity.
! 15 waw               O  -
! 13 wn                I  Total net width.
! 21 x12               IO -
! 20 x1                IO X-coordinate at begin of trajectory.
! 22 x13               IO -
! 23 x2                IO X-coordinate at end of trajectory.
! 14 zs                I  Bed level at centre of structure.
!=======================================================================
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: flqhaw.pf,v $
! Revision 1.9  1999/03/15  15:50:32  kuipe_j
! tabs removed
!
! Revision 1.8  1998/07/01  11:21:26  kuipe_j
! Improve test E < crest
!
! Revision 1.7  1997/11/26  14:50:33  kuipe_j
! x2 shoul not become zero
!
! Revision 1.6  1996/04/12  13:04:15  kuipe_j
! headers, minor changes
!
! Revision 1.5  1996/04/11  08:23:48  kuipe_j
! Kalman module added
!
! Revision 1.4  1995/08/23  14:29:26  overmar
! Lelystad juli 95 ingebracht
!
! Revision 1.3  1995/05/30  09:55:20  hoeks_a
! Minor changes
!
! Revision 1.2  1995/05/30  06:59:20  hoeks_a
! file converted from dos to ux
!
! Revision 1.1  1995/04/13  07:08:01  hoeks_a
! Initial check-in
!
! Revision 1.1.1.1  1993/07/21  14:43:53  kuipe_j
! Initial version
!
!
!***********************************************************************
!
!     Declaration of function
!
   real    FLQHAW
!
!     Declaration of parameters:
!
   integer istru
   logical strsta, strclo(*)
   real    g, hd, hu, uu, h0, ka, kp, n, p, wn, zs, waw, elu
   real    ch, ct, c0, x1, x12, x13, ta, ta2, tb3, tb4,  x2,&
   &elucrs
!
!     Declaration of local variables:
!
   real    c, cp, corr, w, x, x3, x4, x5
!
!     [Doc: S-DO-001.2AK  Eqs. 11 - 24]
!
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
!
!        Calculate basic correction coefficient -c0-
!
      if ( x .lt. 2.0 ) then
         corr = (-0.052)*x3 + (0.145)*x2 -(0.096)*x + 1.01
         c0   = ( (0.1256)*x5 - (1.0178)*x4 + 3.0*x3 - (3.94)*x2 +&
         &(2.28)*x +1.66 ) * corr / sqrt(2.0*g)
      else
         c0   = 2.1549008 / sqrt(2.0*g)
      endif

      x1 = elucrs / h0

      x12 = x1 * x1
      x13 = x12 * x1
!
!        Correction coefficient for head / design head effects -ch-
!
      if ( x1 .lt. 1.6 ) then
         ch = (0.1394)*x13 - (0.416)*x12 + (0.488)*x1 + 0.785
      else
         ch = 1.0718224
      endif
!
!        Correction coefficient for apron effect -cp-
!
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
!
!        Compute discharge Q for an advanced weir
!
      FLQHAW = c * ct * w * sqrt(2.0*g) * elucrs * sqrt(elucrs)
   endif
   waw = w
!
end
