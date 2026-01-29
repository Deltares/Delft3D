subroutine engrpr(engpar ,d90    ,u     ,hrad  ,chezy )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          General routines Module
!
! Programmer:         J.Brouwer
!
! Module:             ENGRPR (ENGelund Roughness PRedictor)
!
! Module description: Calculation of the Chezy value in case of the
!                     Engelund roughness predictor. (only applicable in
!                     the main section)
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  5 chezy             O  Chezy value
!  2 d90               I  D90 value.
!  1 engpar(9)         I  9 Engelund parameters:
!                         (1) = Parameter DELTA-d.
!                         (2) = Parameter THETA-Eng1.
!                         (3) = Parameter THETA-Eng2.
!                         (4) = Parameter as11.
!                         (5) = Parameter as21.
!                         (6) = Parameter as31.
!                         (7) = Parameter as12.
!                         (8) = Parameter as22.
!                         (9) = Parameter as32.
!  4 hrad              I  Hydraulic radius
!  3 u                 I  Velocity
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
! $Log: engrpr.pf,v $
! Revision 1.5  1999/03/15  15:51:15  kuipe_j
! tabs removed
!
! Revision 1.4  1997/06/04  11:12:13  kuipe_j
! Improvement for low Shields factor
!
! Revision 1.3  1995/10/18  08:59:38  kuipe_j
! Changes concerning aux. ouput and IVR adjustments
!
! Revision 1.2  1995/05/30  07:02:21  hoeks_a
! file converted from dos to ux
!
! Revision 1.1  1995/04/13  07:08:25  hoeks_a
! Initial check-in
!
! Revision 1.1.1.1  1993/07/21  14:43:58  kuipe_j
! Initial version
!
!
!***********************************************************************
!
!     Declaration of Parameters:
!
   real      d90      ,u   ,hrad ,chezy
   real      engpar(9)
!
!     Declaration of local variables:
!
   real      c90, ths, thsg,deltad,&
   &as1, as2, as3 ,as11  ,as21, as31, as12, as22, as32
   real      theng(2)
   logical   low, moder, high
!
!     Engelund-like roughness predictor
!     [Doc. S-FO-001.5KV  Eq. 3-5]
!
!     Extraction of 'Engelund' parameters from -engpar-
!
!     theng(1) : Shields parameter on threshold of sediment movement
!     theng(2) : Shields parameter at transition point from dunes to
!                   flat bed
!     deltad   : relative density of bed material
!
   deltad   = engpar(1)
   theng(1) = engpar(2)
   theng(2) = engpar(3)
   as11     = engpar(4)
   as21     = engpar(5)
   as31     = engpar(6)
   as12     = engpar(7)
   as22     = engpar(8)
   as32     = engpar(9)
!
!     Compute C90 = Chezy coeff. w.r.t. grains
!     [Doc. S-FO-001.5KV  Eq. 3-6]
!
   c90  = 18.0 * log10(4.0*hrad/d90)
!
!     Compute thsg = Shields parameter w.r.t. grains
!     [Doc. S-FO-001.5KV  (17)]
!
   thsg = u*u / (c90*c90 * deltad * d90)
!
!     Determine flow condition
!
!     low   = low with flat bed
!     moder = moderate with dunes
!     high  = high flow with anti-dunes
!     thsg     : Shields parameter w.r.t. grains
!     [Doc. S-FO-001.5KV  Eq. 3-10 / 3-11 / 3-12]
!
!                 thsg  < theng(1) : low flow       [flat bed  ]
!     theng(1) <  thsg  < theng(2) : moderate flow  [dunes     ]
!                 thsh  > theng(2) : high flow      [anti-dunes]
!
   low   = thsg .le. theng(1)
   moder = thsg .gt. theng(1) .and. thsg.le.theng(2)
   high  = thsg .gt. theng(2)
!
   if      ( low ) then
!
!        Low flow
!        [Doc. S-FO-001.5KV  Eq. 3-10]
!
!        as1 = 0.0
!        as2 = 1.0
!        as3 = 0.0
!
   else if ( moder ) then
!
!        Moderate flow
!        [Doc. S-FO-001.5KV  Eq. 3-11]
!
      as1 = as11
      as2 = as21
      as3 = as31
!
   else if ( high ) then
!
!        High flow
!        [Doc. S-FO-001.5KV  Eq. 3-12]
!
      as1 = as12
      as2 = as22
      as3 = as32
!
   endif
!
!     Compute Shields parameter thsh
!     [Doc. S-FO-001.5KV  Eq. 3-9]
!
   ths = as1 * thsg * thsg + as2 * thsg + as3
!
!     Compute Engelund-like roughness predictor
!     [Doc. S-FO-001.5KV  Eq. 3-5]
!
   if (low) then
      chezy = c90
   else
      chezy = c90 * sqrt( thsg / ths )
   endif
!
end
