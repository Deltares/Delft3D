function FLQH10 (g     , hu    , hd    , uu    , le    ,&
&appwid, gapwid, gaplen, ks    , c1    ,&
&c3    , applen, itype ,&
&qin   )
!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Flow module
!
! Programmer:         P.R. Evans
!
! Module:             FLow QH relation type 10 (abutments)
!
! Module description: Calculate discharge through structure. This
!                     routine is taken from WENDY and altered to suit
!                     SOBEK. Functionality is unchanged. WENDY history
!                     is:
!                        Project: Construction-Module
!                        Programmer: G. van Driel
!                        Function: Discharge through structure type 10
!                        Updates: None
!                     Ref: WENDY v3.00 reference guide, appendix A
!
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! FLFRST  Function: calculates the friction in the structure
! FLSQRT  Function: returns sqrt(a) for a > 0  and -sqrt(-a) for a < 0
!=======================================================================
!
!***********************************************************************
! CVS log information:
!
!
! $Id$
!
! History:
! $Log: flqh10.pf,v $
! Revision 1.2  1998/06/08  12:35:46  kuipe_j
! log added
!
!
!
!***********************************************************************
! Parameters:
! name     I/O     what
! applen    I      length of approach section
! appwid    I      width of approach section
! c1        I      base coefficient type I
! c3        I      base coefficient type III
!(ccr       I      coefficient)
! g         P      acceleration due to gravity
! gaplen    I      gap length
! gapwid    I      gap width
! hd        I      downstream water level
! hu        I      upstream water level
! itype     I      type of abutment (I or III)
! ks        I      approach section roughness
! le        I      bed level at and near gap
!(teken     I      flow direction)
! uu        I      upstream velocity
!
!***********************************************************************
!
! declare arguments
   real    g     , hu    , hd    , uu    , le    , appwid,&
   &gapwid, gaplen, ks    , c1    , c3    ,&
   &applen, qin
   integer itype
!
! declare variables
   real    elu   , qstr  , x     , x2    , coef6 , coef7 ,&
   &a1    , r     , r1    , ai    , ri    , c     ,&
   &h1enf , hf    , hf1   , hf2   , rmax  , crit
   integer kflow

!
! declare functions
   real    FLQH10, FLSQRT, FLFRST
   logical  EQUAL
   external EQUAL

   elu = hu + uu * uu / (2. * g)
   if (elu .lt. le) then
      qstr = 0.
      goto 10
   endif

   x       = 1. - gapwid / appwid
   x2      = x * x

   if (itype .eq. 1) then
      if (EQUAL(c1 , 0.)) then
         coef6 = -.1146 * x2 * x + .5787 * x2 - .776 * x + 1.
      else
         coef6 = c1
      endif
   else
      if (EQUAL(c3 , 0.)) then
         coef7 = -.527 * x2 * x + 1.256 * x2 - 1.019 * x + 1.
      else
         coef7 = c3
      endif
   endif

!
! calculate friction terms
   a1  = appwid *  (hu - le)
   r   = appwid + 2 * ( hu - le )
   r1 = a1 / r

!
! set minimum value for r1
   rmax = MAX(0.1, 10 * ks)

   if (r1 .lt. rmax) then
      r1 = rmax
      a1 = r1 * appwid**2 / (appwid - 2 * r1)
   endif

   ai = gapwid * ( hd - le )
   ri = gapwid + 2 * ( hd - le )
   ri = ai / ri

!
! set minimum value for ri
   rmax = MAX(0.1, 10 * ks)

   if (ri .lt. rmax) then
      ri = rmax
      ai = ri * gapwid**2 / (gapwid - 2 * ri)
   endif

   hf1 = FLFRST (qin, applen, r1, a1, ks, ri, ai, ks)
   hf2 = FLFRST (qin, gaplen, ri, ai, ks, ri, ai, ks)
   hf  = hf1 + hf2

!
! discharge calculation
   if (itype .eq. 1) then
      c = coef6
   else
      c = coef7
   endif

   h1enf = elu - hf - le
   crit = 2 * h1enf / 3.

!
! determine flow type
   if ((hd - le) .ge. crit) then
      kflow = 1
   else
      kflow = 3
   endif

   if (kflow .eq. 1) then

!
! eqn. X-22
      qstr = c * gapwid * (hd - le)
      qstr = qstr * FLSQRT(2 * g * (elu - hf - hd))
   else

!
! eqn. X-23
      qstr = c * gapwid * 2/3 * h1enf
      qstr = qstr * SQRT(2 * g * h1enf / 3 )
   endif

10 FLQH10 = qstr

   return
end
