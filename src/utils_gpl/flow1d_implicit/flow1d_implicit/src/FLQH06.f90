function FLQH06 (istru , nstru , strsta, strclo, g     ,&
&hu    , hd    , uu    , qin   , nculv ,&
&wi    , di    , wo    , do    , lg    ,&
&alpha , go    , gm    , wg    ,&
&li    , lo    , ksa   , ksi   , kso   ,&
&ksipos, lw    , beta  , bot   , lpress)
!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Flow module
!
! Programmer:         P.R. Evans
!
! Module:             FLow QH relation type 06 (culvert with pressure
!                     flow)
!
! Module description: Calculate discharge through structure. This
!                     routine is taken from WENDY and altered to suit
!                     SOBEK. Functionality is unchanged. WENDY history
!                     is:
!                        Project: Construction-Module
!                        Programmer: G. van Driel
!                        Function: Discharge throgh structure type 6
!                        Updates: None
!
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
!=======================================================================
!
!***********************************************************************
! CVS log information:
!
!
! $Id$
!
! History:
! $Log: flqh06.pf,v $
! Revision 1.2  1998/06/08  12:35:40  kuipe_j
! log added
!
!
!
!***********************************************************************
!  Description array COEF :
!  ------------------------
!     coef (1) : width  upstream of gate-house W
!     coef (2) : culvert height upstream of gate-house D
!     coef (3) : width  downstream of gate-house Wo
!     coef (4) : culvert height downstream of gate-house Do
!     coef (5) : level of gate seat Lg
!     coef (6) : ratio width of approach section vs. Wi
!     coef (7) : height of gate opening as function of time Go(table nr)
!     coef (8) : height of flow section in front of gate Gm
!     coef (9) : width of gate and gate house Wg
!    (coef (10): inlet loss coefficient ksiin for reverse flow)
!     coef (11): culvert length upstream gate Li
!     coef (12): culvert length downstream gate Lo
!     coef (13): roughness value approach section ksa
!     coef (14): roughness value culvert section upstream ksi
!     coef (15): roughness value culvert section downstream kso
!     coef (16): inlet loss coefficient ksiin
!     coef (17): length of approach section Lw
!     coef (18): number of culverts Nc
!     coef (19): beta
!     coef (20): condition: 1=pressure flow / 2=free surface flow
!
!***********************************************************************

!
! declare arguments
   integer istru , nstru , nculv
   logical lpress, strsta, strclo(nstru)
   real    hu    , hd    , uu    , qin   ,&
   &wi    , di    , wo    , do    , lg    ,&
   &alpha , go    , gm    , wg    ,&
   &li    , lo    , ksa   , ksi   , kso   ,&
   &ksipos, lw    , beta  , bot   , g
!
! declare variables
   real    r1    , hnrg  , qstr  , ai    , ri    ,&
   &a1    , hf1i  , hfig  , c12   , c89   ,&
   &rinlet, c34   , outlet, gateh , ao    ,&
   &ro    , hfgo  , x     , x2    , x3    ,&
   &cc    , t1    , t1t1  , qw    , term  ,&
   &gio   , frict , rloss , a0mu  , arg
!
! declare functions
   real FLQH06, FLFRST

   r1   = (hu - bot) * beta
   hnrg = hu + uu * uu / (2. * g)

!
! if energy head is lower than gate seat level or if the gate opening
! is 0 then q = 0:
   if ((hnrg .lt. lg) .or. (ABS(go) .lt. 1.0e-10)) then
      if (strsta) strclo(istru) = .true.
      qstr = 0
      goto 10
   else
      if (strsta) strclo(istru) = .false.
   endif

   ai = wi * di
   ri = ai / (2 * (wi + di))
   a1 = wi * alpha * r1

   hf1i = FLFRST (qin,lw,r1,a1,ksa,ri,ai,ksi)
   hfig = FLFRST (qin,li,ri,ai,ksi,ri,ai,ksi)

   c12 = wi * di
   c89 = gm * wg
   rinlet = ksipos * (c12/c89)**2


!
! pressure or free flow?
   if (lpress) then
      c34    = wo * do
      outlet = (c34/c89)**2
      gateh  = (1. - (c89/c34))**2
      ao     = wo * do
      ro     = ao / (2 * (wo + do))
      hfgo   = FLFRST (qin,lo,ro,ao,kso,ro,ao,kso)
      x    = go / gm
      x2   = x * x
      x3   = x2 * x
      cc   = 29 * x3 * x3 - 79 * x3 * x2 + 80 * x2 * x2
      cc   = cc - 37 * x3 + 7.8 * x2 - .55 * x + .615
      t1   = gm/(cc * go)
      t1t1 = t1 * t1
      qw   = qin / c89

      if (lo/do .gt. 5.) then
         gio   = gateh + rinlet + outlet

         if (ABS(qw) .lt. 1.0e-10) then
            frict = 0.
         else
            frict = (hfig + hf1i + hfgo) * 2 * g / (qw * qw)
         endif

         rloss = SQRT ((t1-1.)**2 + gio + frict)
      else
         if (ABS(qw) .lt. 1.0e-10) then
            frict = 0.
         else
            frict = (hfig + hf1i) * 2 * g / (qw * qw)
         endif

         rloss = SQRT(t1t1 + rinlet + frict)
      endif

      a0mu = c89 / rloss

!
! eqn. VI-12.3
      arg = hnrg - hd
      if (arg .lt. 0.) arg = 0.
      qstr = nculv * a0mu * SQRT(arg * 2 * g)

   else
      x = go / gm
      x2 = x * x
      cc = .323 * x2 * x - .134 * x2 + .046 * x + .61
      term = hnrg - hf1i - hfig - cc * go - lg
      term = term - ksipos * qin * qin /(2 * g * c12**2)
      if (term .lt. 0.) term = 0.

!
! eqn. VI-7
      qstr = nculv * cc * go * wg * SQRT(2 * g * term)

   endif

10 FLQH06 = qstr

   return
end
