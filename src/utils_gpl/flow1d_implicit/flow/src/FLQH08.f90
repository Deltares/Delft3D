function FLQH08 (teken , npier , istru , nstru , g     ,&
&qin   , hu    , hd    , uu    ,&
&wn    , kp    , ka    , le    , go    ,&
&gu    , lw    , li    , lo    , c2    ,&
&c3    , wp    , ksa   , ks    , alpha ,&
&ksii  , cga   , beta  , strsta, strclo,&
&bot   )
!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Flow Module
!
! Programmer:         P.R. Evans
!
! Module:             FLQH08, FLow QH relation type 08 (sluice with
!                     overflow/underflow gate.
!
! Module description: Calculate discharge through structure. This
!                     routine is taken from WENDY and altered to suit
!                     SOBEK. Functionality is unchanged. WENDY history
!                     is:
!                        Project: Construction-Module
!                        Programmer: G. van Driel
!                        Function: Discharge through structure type 8
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
! $Log: flqh08.pf,v $
! Revision 1.2  1998/06/08  12:35:43  kuipe_j
! log added
!
!
!
!***********************************************************************
!  Beschrijving array COEF:
!  ------------------------
!     coef (1) : total net width  of the bay(s) Wn
!     coef (2) : number of piers N
!     coef (3) : pier contraction coefficient Kp
!     coef (4) : abutment contraction coefficient Ka
!     coef (5) : bottom level of sluice Le
!     coef (6) : gate opening Go as function of time (table number)
!     coef (7) : gate crest Gu   as function of time (table number)
!     coef (8) : length of approach section Lw
!     coef (9) : sluice length upstream of gate Li
!     coef (10): sluice length downstream of gate Lo
!     coef (11): coefficient C2
!     coef (12): coefficient C3
!     coef (13): total pier width Wp
!     coef (14): roughness value approach section ksa
!     coef (15): roughness value sluice section ks
!     coef (16): ratio width of approach section vs. Wp + Wn
!     coef (17): inlet loss coefficient ksii
!     coef (18): coefficient Cga
!     coef (19): beta
!
!**********************************************************************

!
! declare arguments
   integer teken , npier , istru , nstru
   double precision qin
   real    g     , hu, hd    , uu    , qstr  ,&
   &wn    , kp    , ka    , le    , go    , gu    ,&
   &lw    , li    , lo    , c2    , c3    , wp    ,&
   &ksa   , ks    , alpha , ksii  , cga   , beta  ,&
   &bot
   logical strsta, strclo(nstru)

!
! declare local variables
   integer i     , iflow
   real    hnrg  , deltah, w     , w2    , gh    , hule  ,&
   &hdle  , crit  , x     , qw    , r1    , rmax  ,&
   &a1    , ai    , ri    , hf1i  , hfig  , yco   ,&
   &ao    , ro    , hfgo  , hfric , ycoi  , hg    ,&
   &z     , ct    , z07   , t1    , t2    , x2    ,&
   &x3    , cc    , qa    , qb    , hulp  , arg   ,&
   &a0mu
!
! declare functions
   real    FLQH08, FLFRST, FLSQRT
   logical  EQUAL
   external EQUAL

   hnrg = hu + uu * uu / (2. * g)
   deltah  = hnrg - le
   if ( deltah .lt. 1.0e-10 ) then
      if (strsta) strclo(istru) = .true.
      qstr = 0
      goto 10
   else
      if (strsta) strclo(istru) = .false.
   endif

   w     = wn - 2 * (npier*kp + ka) * deltah
   w2    = w * w
   gh    = gu - le - go
   hule  = hu - le
   hdle  = hd - le
   if (EQUAL(go,0.)) then
      crit = 0.
   else
      x     = hule / go

!
! set criterium
      if ( x .gt. 1.5) then
         if ( x .lt. 4.) then
            crit = .39467 * ((x - 1.5) ** (1./4.)) / x
         else
            crit  = 1.75 / SQRT(x + 1.75)
         endif
      endif
   endif

   qw = qin / (w * hule)

!---  De eerste keer per tijdstap wordt het flowtype bepaald:  ---------
! this loop disabled
!      if (k .eq. 0) then

!
! determine r's and w's for friction terms
   r1      = (hu - bot) * beta
   rmax    = max(0.1, 10 * ksa)
   if (r1 .lt. rmax) r1 = rmax
   a1 = alpha * r1 * (wn + wp)

   ai   = w * hule
   ri   = ai / (2 * hule * (npier + 1) + w)
   rmax = max(0.1, 10 * ks)
   if (ri .lt. rmax) then
      ri = rmax
      ai = w2 * ri / ( w - 2 * (npier +1) * ri)
   endif

!
! calculate friction for Yco
   hf1i = FLFRST(sngl(qin), lw, r1, a1, ksa, ri, ai, ks)
   hfig = FLFRST(sngl(qin), li, ri, ai, ks, ri, ai, ks)

!
! calculate yco iteratively. Initial value is yco = 2/3*(hnrg-Le)
   yco = deltah * 2. / 3.
   do 31 i = 1 , 10
      ao = w * yco
      ro = ao / (w + 2 * yco * (npier + 1) )

      rmax  = max(0.1, 10 * ks)
      if (ro .lt. rmax) then
         ro = rmax
         ao = w2 * ro / ( w - 2 * ro * (npier + 1) )
      endif

      hfgo  = FLFRST(sngl(qin), lo, ro, ao, ks, ro, ao, ks)
      hfric = hf1i + hfgo + hfig

      ycoi  = (deltah - hfric) * 2. / 3
      if (ABS(ycoi - yco) .lt. 0.0001 * ycoi) goto 32
31 continue
32 yco = ycoi
   ao = w * yco

!         if (itel .eq. 1) then
   if  (go .lt. hule / 1.5) then
      if ( (crit*hule) .lt. hdle ) then
         iflow = 1
      else
         iflow = 2
      endif

   else

      if (hdle .lt. yco) then
         iflow = 3
      else
         iflow = 4
      endif
   endif
!         endif
!      endif

   goto (1,2,3,4,5) iflow

!
! flowtype 1
1  hg = 1.6 * go/gh
   hg = hdle - hg * (hu -hd)

   z = (hnrg - hg - le) / deltah

   if (z .le. 0.) then
      ct = 0.
   else
      if ( z .gt. 0.7 ) then
         ct = 1.
      else
         z07 = (z - 0.7) ** 2
         t1 = SQRT (1. - z07 / 0.49)
         t2 = 27. * z07 * z07 * z * SQRT(z)
         ct = t1 + t2
      endif
   endif

   x = go / hule
   x2 = x * x
   x3 = x2 * x
   cc = 29 * x3 * x3 - 79 * x3 * x2 + 80 * x2 * x2 - 37 * x3
   cc = cc+ 7.8 * x2 - .55 * x + .615

   if (teken .lt. 0) then
      cga = .7 * cga
      cc = .9 * cc
   endif


!
! eqn VIII-14.1 and VIII-14.2
   qa = w * ct * cga * SQRT(2.*g) * 2. / 3.
   hulp = hnrg - gu - hf1i

   if (hulp .lt. 0.) then
      qa = 0.
   else
      qa = qa * hulp * SQRT(hulp)
   endif

   qb = w * cc * go
   hulp = hnrg - hf1i - ksii*qw*qw/(2.*g) - hg - le

   if (hulp .lt. 0.) then
      qb = 0.
   else
      qb = qb * SQRT(2.*g*(hulp))
   endif

   qstr = qa + qb
   goto 10

!
! flowtype 2
2  x = go/hule
   x2 = x * x
   cc = .323 * x2 * x -.134 * x2 + .046 * x + .61

   if (teken .lt. 0) then
      cga = .7 * cga
      cc = .9 * cc
   endif

   qa = w * cga * SQRT(2.*g) * 2. / 3.
   hulp = hnrg - gu - hf1i

!
! eqn VIII-17.1 and VIII-17.2
   if (hulp .lt. 0) then
      qa = 0
   else
      qa = qa * hulp * FLSQRT(hulp)
   endif

   qb = w * cc * go
   hulp = deltah -hf1i- ksii*qw*qw/(2.*g) - cc * go

   if (hulp .lt. 0.) then
      qb = 0.
   else
      qb = qb * SQRT(2.*g * (hulp))
   endif

   qstr = qa + qb
   goto 10

!
! flowtype 3
! eqn VIII-18.3
3  arg =  2.*g * (deltah - yco - hfric)
   if (arg .lt. 0.) arg = 2. * g * (deltah - yco)
   qstr =  c2 * w * yco * SQRT(arg)
   goto 10

!
! flowtype 4
! check on calculation of head loss or discharge
4  a0mu = c3 * w * hdle

!
! eqn VIII-19.7
   arg = 2. * g * (hnrg - hd - hfric)

   if (arg .lt. 0.) arg = 1.0e-6
   qstr = a0mu * SQRT(arg)
   goto 10

5  continue
   qstr = 0.
10 FLQH08 = qstr

   return
end
