function FLQH07 (g     , istru , nstru , strsta, strclo,&
&qin   , hu    , hd    , alpha , beta  ,&
&uu    , teken , bot   , wn    , npier ,&
&kp    , ka    , level , hgate , lw    ,&
&li    , lo    , c2    , c3    , ksa   ,&
&ks    , ksii  , wp    )
!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Flow module
!
! Programmer:         P.R. Evans
!
! Module:             FLow QH relation type 07 (sluice with gate)
!
! Module description: Calculate discharge through structure. This
!                     routine is taken from WENDY and altered to suit
!                     SOBEK. Functionality is unchanged. WENDY history
!                     is:
!                        Project: Construction-Module
!                        Programmer: G. van Driel
!                        Function: Discharge through structure type 7
!                        Updates: 6-6-91  J.Kuipers/G.van.Driel
!                          HG=SQRT(HG) is set to zero for HG<0
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
! $Log: flqh07.pf,v $
! Revision 1.2  1998/06/08  12:35:42  kuipe_j
! log added
!
!
!
!***********************************************************************
!
! Parameters:
! name     I/O    what
! wn        I     total net width of the bay(s) Wn
! npier     I     number of piers N
! kp        I     pier contraction coefficient Kp
! ka        I     abutment contraction coefficient Ka
! level     I     level of sluice bottom Le
! hgate     I     opening height gate Go as function of time (table nr)
! lw        I     length of approach section Lw
! li        I     sluice length upstream of gate Li
! lo        I     sluice length downstream of gate Lo
! c2        I     coefficient C2
! c3        I     coefficient C3
! ksa       I     roughness value approach section ksa
! ks        I     roughness value flume section ks
! ksii      I     inlet loss coefficient ksii
! wp        I     total pier width wp
! alpha     I     ratio between width of approach section vs. Wp + Wn
! beta      I     beta factor for hu - bot
! teken     I     flow direction
! qin       I     discharge
!
!***********************************************************************

!
! declare arguments
   integer teken , npier , istru , nstru
   logical strsta, strclo(nstru)
   real    g     , hu    , hd    , alpha , beta  ,&
   &uu    , bot   , wn    , kp    , ka    ,&
   &level , hgate , lw    , li    , lo    ,&
   &c2    , c3    , ksa   , ks    , ksii  ,&
   &wp    , qin
!
! declare local variables
   real    elu   , hule  , hdle  , deltah, r1    ,&
   &qstr  , w     , w2    , x     , crit  ,&
   &rmax  , a1    , ai    , ri    , hf1i  ,&
   &yco   , hfgo  , hfric , ycoi  , x2    ,&
   &x3    , cc    , t1    , t2    , t3    ,&
   &hulp  , arg   , a0mu  , ao    , ro    ,&
   &qw    , hfig  , t4    , hg
   integer iflow , i

!
! declare functions
   real    FLQH07, FLFRST



!
   elu    = hu + uu * uu / (2. * g)
   hule   = hu - level
   hdle   = hd - level
   deltah = elu - level

!
! is energy head lower than gate sill, or gate closed?
   if ((deltah .lt. 1.0e-10) .or. (hgate .lt. 1.0e-10)) then
      if (strsta) strclo(istru) = .true.
      qstr = 0.
      goto 10
   else
      if (strsta) strclo(istru) = .false.
   endif

   w    = wn - 2 * (npier*kp + ka) * deltah
   w2   = w * w
   x    = hule / hgate

!
! set criterium
   if (x .gt. 1.5) then
      if (x .lt. 4.) then
         crit = .39467 * ((x - 1.5)**(1./4.)) / x
      else
         crit = 1.75 / SQRT(x + 1.75)
      endif
   endif

   qw   = qin / w

!---  De eerste keer per iteratie wordt het flowtype bepaald:  ---------
! this loop is no longer used
!      if (k .eq. 0) then

!
! calculate r and a for the friction terms
   r1      = (hu - bot) * beta
   rmax    = max(0.1, 10 * ksa)
   if (r1 .lt. rmax) r1 = rmax
   a1 = alpha * r1 * (wn + wp)

   ai   = w * hule
   ri   = ai / (2 * hule * (npier + 1) + w)
   rmax = max(0.1, 10 * ks)

   if (ri .lt. rmax) then
      ri = rmax
      ai = w2 * ri / (w - 2 * (npier +1) * ri)
   endif

!
! get friction terms for Yco
   hf1i = FLFRST(qin, lw, r1, a1, ksa, ri, ai, ks)
   hfig = FLFRST(qin, li, ri, ai, ks, ri, ai, ks)

!
! get Yco by iteration
   yco = deltah * 2. / 3.
   do 21 i = 1 , 10
      ao = w * yco
      ro = ao / (w + 2 * yco * (npier + 1))
      rmax  = max(0.1, 10 * ks)

      if (ro .lt. rmax) then
         ro = rmax
         ao = w2 * ro / (w - 2 * ro * (npier + 1))
      endif

      hfgo  = FLFRST(qin, lo, ro, ao, ks, ro, ao, ks)
      hfric = hf1i + hfgo + hfig

      ycoi  = (deltah - hfric) * 2. / 3
      if (ABS(ycoi - yco) .lt. 0.0001 * ycoi) goto 22
21 continue

22 yco = ycoi
   ao = w * yco

!
! determine flow type
   if (hgate .lt. hule/1.5) then
      if ((crit*hule) .lt. hdle) then
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
!      endif

   if (iflow == 1) then
      goto 1
   else if (iflow == 2) then
      goto 2
   else if (iflow == 3) then
      goto 3
   else if (iflow == 4) then
      goto 4
   end if
!
! flowtype 1
1  x  = hgate / hule
   x2 = x * x
   x3 = x2 * x
   cc = 29 * x3 * x3 - 79 * x3 * x2 + 80 * x2 * x2
   cc = cc - 37 * x3 + 7.8 * x2 - .55 * x + .615

   t1 = hdle * hdle
   t2 = (1 / hdle - 1 / (cc*hgate))
   t3 = qw * qw * 2 / g
   t4 = t3 * t2

!      if (k .eq. 0) then
   hg = t1 + t4
   if (hg .le. 0.) then
      hg = 0.
   else
      hg = SQRT(hg)
   endif
!      endif

!
! eqn VII-11
   qstr   = w * cc * hgate
   hulp = elu - hf1i - ksii * qw * qw&
   &/ (hule * hule * 2. * g) - hg - level

   if (hulp .lt. 0.) then
      qstr = 0.
   else
      qstr = qstr * SQRT(2. * g * hulp)
   endif

   goto 10

!
! flowtype 2
2  x  = hgate / hule
   x2 = x * x
   cc = .323 * x2 * x - .134 * x2 +.046 * x + .61

!
! eqn VII-14
   qstr   = w * cc * hgate
   hulp = 2. * g * (deltah - hf1i - ksii * qw * qw&
   &/ (hule * hule * 2. * g) - cc * hgate )

   if (hulp .lt. 0.) then
      qstr = 0.
   else
      qstr   = qstr * SQRT(hulp)
   endif

   goto 10

!
! flowtype 3
3  continue
   ao = w * yco

!
! eqn VII-15.13
   arg = 2. * g * (deltah - yco - hfric)
   if (arg .lt. 0.) arg = 2. * g * (deltah - yco)
   qstr =  c2 * ao * SQRT(arg)
   goto 10

!
! flowtype 4
4  a0mu = c3 * w * hdle

!
! eqn VII-16.7
   arg = 2. * g * (elu - hd - hfric)
   if (arg .lt. 0.) arg = 1.0e-6
   qstr = a0mu * SQRT(arg)
   goto 10
!cc   endif

10 continue
   if (teken .eq. -1) qstr = 0.9 * qstr
   FLQH07 = qstr

   return
end
