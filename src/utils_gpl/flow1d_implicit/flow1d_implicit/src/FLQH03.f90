function FLQH03 (teken , strsta, strclo, istru , nstru ,&
&wn    , le    , cg    , leg   , ksii  ,&
&npier , kp    , ka    , lw    , li    ,&
&lo    , ksa   , ksi   , kso   , wp    ,&
&alpha , beta  , qin   , bot   , g     ,&
&hu    , hd    , uu    )
!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Flow module
!
! Programmer:         P.R. Evans
!
! Module:             FLQH03, Flow QH relation type 03 (Sluice with
!                     bottom hinged gate
!
! Module description: Calculate discharge through structure. This
!                     routine is taken from WENDY and altered to suit
!                     SOBEK. Functionality is unchanged. WENDY history
!                     is:
!                        Project: Construction-Module
!                        Programmer: G. van Driel
!                        Function: Discharge through structure type 3
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
! $Log: flqh03.pf,v $
! Revision 1.2  1998/06/08  12:35:37  kuipe_j
! log added
!
!
!
!***********************************************************************
!
!  Description array COEF :
!  ------------------------
!     coef (1) : total net width of sluice Wn
!     coef (2) : level of sluice bottom Le
!     coef (3) : gate coefficient Cg
!     coef (4) : crest level as function of time Leg (table number)
!     coef (5) : inlet loss coefficient ksii
!     coef (6) : number of piers N
!     coef (7) : pier contraction coefficient Kp
!     coef (8) : abutment contraction coefficient Ka
!     coef (9) : length of approach section Lw
!     coef (10): sluice length upstream of gate Li
!     coef (11): sluice length downstream of gate Lo
!     coef (12): roughness value approach section ksa
!     coef (13): roughness value sluice section upstream ksi
!     coef (14): roughness value sluice section downstream kso
!     coef (15): total pier width Wp
!     coef (16): ratio width of approach section vs. Wp + Wn
!     coef (17): beta
!
!***********************************************************************

!
! declare arguments
   integer teken , istru , nstru , npier
   logical strsta, strclo(nstru)
   real    wn    , le    , cg    , leg   , ksii  , kp    ,&
   &ka    , lw    , li    , lo    , ksa   , ksi   ,&
   &kso   , wp    , alpha , beta  , qin   , bot   ,&
   &g     , hu    , hd    , uu

!
! declare local variables
   real    hnrg  , qstr  , deltah, hule  , hdle  , wpwn  ,&
   &w     , r1    , ri    , ro    , rlen  , hf1i  ,&
   &hfio  , z     , ct    , z07   , t1    , t2    ,&
   &arg
!
! declare functions
   real    FLF31I, FLF3IO, FLQH03

   hnrg = hu + uu * uu / (2. * g)

!
! check if waterlevel is below the bed or gate level
   if ( ( hnrg .lt. leg ) .or. (hnrg .le. le) ) then
      if (strsta) strclo(istru) = .true.
      qstr = 0.
      goto 10
   else
      if (strsta) strclo(istru) = .false.
   endif

   deltah = hnrg - le
   hule   = hu - le
   hdle   = hd - le

   wpwn   = wn + wp
   w      = wn - 2 * (npier * kp + ka) * deltah

   r1     = beta * (hu - bot)
   ri     = w * hule / ( w + 2 * (npier + 1) * hule )
   ro     = w * hdle / ( w + 2 * (npier + 1) * hdle )
   rlen   = li + lo

   hf1i   = FLf31I(w, wpwn, qin, lw, r1, ksa, alpha, ri,&
   &ksi, npier, hule)
   ri     = w * hule / ( w + 2 * (npier + 1) * hule )
   hfio   = FLF3IO(wpwn, qin, rlen, ri, ksi, ro,&
   &kso, npier, hule, hdle)

   z      = (hnrg - hd) / deltah

   if (z .le. 0.) then
      ct = 0.
   else
      if ( z .gt. 0.7 ) then
         ct = 1.
      else
         z07 = (z - 0.7)**2
         t1  = SQRT (1. - z07/0.49)
         t2  = 27. * z07 * z07 * z * SQRT(z)
         ct  = t1 + t2
      endif
   endif

   arg = hnrg - leg - hf1i - hfio - ksii * qin * qin /&
   &(2. * g * (w * hule )**2)
   if (arg .lt. 0.) arg = 0.

   qstr = 2./3. * cg * ct * w * arg * SQRT(2. * g * arg)

   if (teken .eq. -1) qstr = 0.7 * qstr
10 FLQH03 = qstr

   return
end
