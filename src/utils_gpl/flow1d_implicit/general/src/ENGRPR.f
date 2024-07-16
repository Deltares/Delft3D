      subroutine engrpr(engpar ,d90    ,u     ,hrad  ,chezy )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          General routines Module
c
c Programmer:         J.Brouwer
c
c Module:             ENGRPR (ENGelund Roughness PRedictor)
c
c Module description: Calculation of the Chezy value in case of the
c                     Engelund roughness predictor. (only applicable in
c                     the main section)
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  5 chezy             O  Chezy value
c  2 d90               I  D90 value.
c  1 engpar(9)         I  9 Engelund parameters:
c                         (1) = Parameter DELTA-d.
c                         (2) = Parameter THETA-Eng1.
c                         (3) = Parameter THETA-Eng2.
c                         (4) = Parameter as11.
c                         (5) = Parameter as21.
c                         (6) = Parameter as31.
c                         (7) = Parameter as12.
c                         (8) = Parameter as22.
c                         (9) = Parameter as32.
c  4 hrad              I  Hydraulic radius
c  3 u                 I  Velocity
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
c $Log: engrpr.pf,v $
c Revision 1.5  1999/03/15  15:51:15  kuipe_j
c tabs removed
c
c Revision 1.4  1997/06/04  11:12:13  kuipe_j
c Improvement for low Shields factor
c
c Revision 1.3  1995/10/18  08:59:38  kuipe_j
c Changes concerning aux. ouput and IVR adjustments
c
c Revision 1.2  1995/05/30  07:02:21  hoeks_a
c file converted from dos to ux
c
c Revision 1.1  1995/04/13  07:08:25  hoeks_a
c Initial check-in
c
c Revision 1.1.1.1  1993/07/21  14:43:58  kuipe_j
c Initial version
c
c
c***********************************************************************
c
c     Declaration of Parameters:
c
      real      d90      ,u   ,hrad ,chezy
      real      engpar(9)
c
c     Declaration of local variables:
c
      real      c90, ths, thsg,deltad,
     &          as1, as2, as3 ,as11  ,as21, as31, as12, as22, as32
      real      theng(2)
      logical   low, moder, high
c
c     Engelund-like roughness predictor
c     [Doc. S-FO-001.5KV  Eq. 3-5]
c
c     Extraction of 'Engelund' parameters from -engpar-
c
c     theng(1) : Shields parameter on threshold of sediment movement
c     theng(2) : Shields parameter at transition point from dunes to
c                   flat bed
c     deltad   : relative density of bed material
c
      deltad   = engpar(1)
      theng(1) = engpar(2)
      theng(2) = engpar(3)
      as11     = engpar(4)
      as21     = engpar(5)
      as31     = engpar(6)
      as12     = engpar(7)
      as22     = engpar(8)
      as32     = engpar(9)
c
c     Compute C90 = Chezy coeff. w.r.t. grains
c     [Doc. S-FO-001.5KV  Eq. 3-6]
c
      c90  = 18.0 * log10(4.0*hrad/d90)
c
c     Compute thsg = Shields parameter w.r.t. grains
c     [Doc. S-FO-001.5KV  (17)]
c
      thsg = u*u / (c90*c90 * deltad * d90)
c
c     Determine flow condition
c
c     low   = low with flat bed
c     moder = moderate with dunes
c     high  = high flow with anti-dunes
c     thsg     : Shields parameter w.r.t. grains
c     [Doc. S-FO-001.5KV  Eq. 3-10 / 3-11 / 3-12]
c
c                 thsg  < theng(1) : low flow       [flat bed  ]
c     theng(1) <  thsg  < theng(2) : moderate flow  [dunes     ]
c                 thsh  > theng(2) : high flow      [anti-dunes]
c
      low   = thsg .le. theng(1)
      moder = thsg .gt. theng(1) .and. thsg.le.theng(2)
      high  = thsg .gt. theng(2)
c
      if      ( low ) then
c
c        Low flow
c        [Doc. S-FO-001.5KV  Eq. 3-10]
c
c        as1 = 0.0
c        as2 = 1.0
c        as3 = 0.0
c
      else if ( moder ) then
c
c        Moderate flow
c        [Doc. S-FO-001.5KV  Eq. 3-11]
c
         as1 = as11
         as2 = as21
         as3 = as31
c
      else if ( high ) then
c
c        High flow
c        [Doc. S-FO-001.5KV  Eq. 3-12]
c
         as1 = as12
         as2 = as22
         as3 = as32
c
      endif
c
c     Compute Shields parameter thsh
c     [Doc. S-FO-001.5KV  Eq. 3-9]
c
      ths = as1 * thsg * thsg + as2 * thsg + as3
c
c     Compute Engelund-like roughness predictor
c     [Doc. S-FO-001.5KV  Eq. 3-5]
c
      if (low) then
         chezy = c90
      else
         chezy = c90 * sqrt( thsg / ths )
      endif
c
      end
