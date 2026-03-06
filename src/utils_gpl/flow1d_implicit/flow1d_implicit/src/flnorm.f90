subroutine FLNORM(ngrid  ,ngridm ,i1     ,i2     ,g      ,&
&dt1    ,steady ,psi    ,theta  ,exrstp ,&
&h1     ,q1     ,h      ,qlatgr ,&
&grid   ,x      ,&
&wf     ,af     ,wt     ,at     ,at1    ,&
&chz    ,r      ,alfab  ,tauwi  ,ksi    ,&
&lsalt  ,rho    ,rhow   ,a1m    ,&
&a1     ,b1     ,c1     ,d1     ,e1     ,&
&a2     ,b2     ,c2     ,d2     ,e2     ,&
&o      ,wfp    ,omr    ,cflpse ,&
&q      ,wfex   ,wtex   ,h2     ,&
&cflpsa ,omcfl  ,dhtyp  ,iter   ,ibuf3  ,&
&istep  ,lfrou  ,solbuf ,q2     )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Flow Module
!
! Programmer:         J.Brouwer
!
! Module:             FLNORM (FLow abcde coefficients 'NORMal'gridpnts.)
!
! Module description: In subroutine FLNORM the matrix coefficients A1-E1
!                     and A2-E2 will be computed for 'normal'
!                     gridpoints.
!
!                     For each grid point the coefficients A1-E1 from
!                     the continuity equation follow from formulae (7-4)
!                     until (7-6) of S-FO-001.5KV. The coefficients
!                     A2-E2 from the momentum equation follow from for-
!                     mulae (7-7) until (7-13) of S-FO-001.5KV. The time
!                     step used when dividing should be the separate
!                     time step passed.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
! 30 a1(ngridm)        IO A1-coefficient of continuity eq. per gridpnt.
! 29 a1m               I  parameter a1m
! 35 a2(ngridm)        IO A2-coefficient of momentum equation c.q. sta-
!                         ge-discharge equation per gridpoint.
! 17 af(ngrid)         I  Flow area per grid point at n + theta
! 23 alfab(ngrid)      I  Bousinessq coefficient at grid point i
! 20 at1(ngrid)        I  Total area per grid point at n
! 19 at(ngrid)         I  Total area per grid point at n + 1
! 31 b1(ngridm)        IO B1-coefficient of continuity eq. per gridpnt.
! 36 b2(ngridm)        IO B2-coefficient of momentum equation c.q. sta-
!                         ge-discharge equation per gridpoint.
! 32 c1(ngridm)        IO C1-coefficient of continuity eq. per gridpnt.
! 37 c2(ngridm)        IO C2-coefficient of momentum equation c.q. sta-
!                         ge-discharge equation per grid point
! 50 cflpsa(ngrid)     IO pseudo Courant number per grid point
! 44 cflpse            I  (initial) pseudo Courant number
! 21 chz(ngrid)        I  Chezy coefficient per grid point
! 33 d1(ngridm)        IO D1-coefficient of continuity eq. per gridpnt.
! 38 d2(ngridm)        IO D2-coefficient of momentum equation c.q. sta-
!                         ge-discharge equation per gridpoint.
! 52 dhtyp             I  -
!  6 dt1               I  Time step.
! 34 e1(ngridm)        IO E1-coefficient of continuity eq. per gridpnt.
!                         A1 = A1(1)+A1(2)+A1(3)  , etc.
! 39 e2(ngridm)        IO E2-coefficient of momentum equation c.q. sta-
!                         ge-discharge equation per gridpoint.
!  5 g                 I  Acceleration of gravity.
! 14 grid(ngrid)       I  Grid cell type definition:
!                         cgrdcl (1) : Normal grid cell
!                         cstrcl (2) : Structure cell
! 10 h1(ngrid)         I  water level per grid point at n
! 49 h2(ngrid)         I  water level per grid point at n + 1, previous iter.
! 12 h(ngrid)          I  water level per grid point at n + 1, last iteration
!  3 i1                I  index of first grid point in actual branch
!  4 i2                I  index of last grid point in actual branch
! 55 istep             I  current time step number n + 1
! 53 iter              I  iteration step
! 56 juer              I  -
! 54 jufrou            I  Unit number of file froude
! 57 ker               O  Error code:
!                         ok     (0) : No error
!                         info   (1) : Informative message
!                         warnng (2) : Warning
!                         fatal  (3) : Fatal error (processing stops)
! 25 ksi(ngrid)        I  extra resistance at grid points i+1/2
! 58 lfrou             O  Flag if Froude numbers during simulation are
!                         high
! 26 lsalt             I  Logical indicator for salt computation
!                         = .true.  : with salt computation
!                         = .false. : no salt computation
!  1 ngrid             I  Number of grid points in network.
!  2 ngridm            I  Maximum number of gridpoints in a branch.
! 40 o(ngrid)          I  Wetted perimeter for total cross section.
! 51 omcfl             I  -
! 43 omr               I  Underrelaxation factor omega for the hydraulic
!                         radius R.
!  8 psi               I  Space weight factor in Preissmann scheme.
! 10 q1(ngrid)         I  discharge per grid point at n
! 58 q2(ngrid)         I  discharge per grid point at n + 1, previous iteration
! 12 q(ngrid)          I  discharge per grid point at n + 1, last iteration
! 13 qlatgr(ngrid)     I  (i) = Actual lateral discharge in grid point
!                         i+1/2.
! 22 r(ngrid)          I  Actual hydraulic radius for total channel in
!                         every grid point.
! 27 rho(ngrid)        I  Density of diluted water per grid point.
! 28 rhow              I  Density of fresh water.
!  7 steady            I  Calculation mode (0 or 1)
!                         0 = steady calculation
!                         1 = unsteady calculation
! 24 tauwi(ngrid)      I  Calculated wind friction for each gridpoint.
!  9 theta             I  Time weight factor in Preissmann scheme.
! 16 wf(ngrid)         I  flow width per grid point at n + theta
! 47 wfex(ngrid)       I  extra flow width due to zomerkaden per grid point
! 42 wfp(ngrid)        I  flow width per grid point at n + theta
!                         Calculated with underrelaxation.
!                         Input parameter on the former iteration level
!                         Output parameter on the actual iteration level (???)
! 18 wt(ngrid)         I  total width per grid point at n + 1
! 48 wtex(ngrid)       I  extra total width due to zomerkaden per grid point
! 15 x(ngrid)          I  x(i) = X-coordinate of grid point i.
!
! Local variables:
!    dhi, dhip1   water level convergence error or difference at some grid point
!    dqi, dqip1   discharge convergence error or difference at some grid point
!    dci          convergence error in celerity at some grid point
!    dui          convergence error in alfab times velocity at some grid point
!    dxdtnw       next value of Dx / Dt_pseu at some grid point
!    cflmax       maximum value of pseudo Courant number
!    cflmin       minimum value of pseudo Courant number
!    hi, hip1     theta-averaged water level at some grid point
!    ci, cip1     celerity at some grid point
!    qi, qip1     theta-averaged discharge at some grid point
!    ui, uip1     alfab times velocity at some grid point
!    wfti, wftip1 sum of flow width and extra flow width at some grid point
!    wtti, wttip1 sum of total width and extra total width at some grid point
!    afgem        flow area at some grid cell
!=======================================================================
! Subroutine calls:
! Name    Description
! error   write an ERROR to the error file
! getloc  GET LOCation of gridpoint
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: flnorm.pf,v $
! Revision 1.17  1999/09/15  14:21:41  borsb_m
! Pseudo timestepping repaired
!
! Revision 1.16  1999/03/15  14:21:41  kuipe_j
! Improve writing Froude file
!
! Revision 1.15  1997/11/04  14:09:09  kuipe_j
! theta set for steady flow
!
! Revision 1.14  1997/05/26  07:44:02  kuipe_j
! Computation Froude number improved
!
! Revision 1.13  1997/02/17  10:20:51  kuipe_j
! Lateral Q in m3/s in cont equation now
!
! Revision 1.12  1997/01/23  08:29:10  kuipe_j
! Make flow module robust
!
! Revision 1.11  1996/10/31  10:30:21  kuipe_j
! Extra resistance finished
!
! Revision 1.10  1996/01/17  14:38:38  kuipe_j
! header update
!
! Revision 1.9  1996/01/16  15:01:21  kuipe_j
! Restart improvements
!
! Revision 1.8  1995/11/21  11:07:55  kuipe_j
! Added features are: Special morphology output for IVR; Improvement of
!     auxilliary output; Automatic speudo time stepping; general structure
!     improvement (Q-dependent lin, relax. of Q only, changed weir Q-H
!     relation); removal of grid points in messages; etc.
!
! Revision 1.7  1995/09/22  10:01:55  kuipe_j
! variable dimensions, new headers
!
! Revision 1.6  1995/09/12  08:10:57  overmar
! - Option "zomerkaden" added
! - Better linearization
! - Pseudo time
! - Iterative matrix solution
!
! Revision 1.5  1995/08/30  12:36:43  kuipe_j
! Triggers + controllers for BOS
!
! Revision 1.4  1995/08/23  14:29:23  overmar
! Lelystad juli 95 ingebracht
!
! Revision 1.3  1995/05/30  09:55:13  hoeks_a
! Minor changes
!
! Revision 1.2  1995/05/30  06:59:13  hoeks_a
! file converted from dos to ux
!
! Revision 1.1  1995/04/13  07:07:55  hoeks_a
! Initial check-in
!
! Revision 1.3  1994/11/28  08:37:39  kuipe_j
! Time and timestep in double precision.
!
! Revision 1.2  1993/11/26  15:31:13  kuipe_j
! Update after finishing Sobeksel.
!
! Revision 1.1.1.1  1993/07/21  14:43:52  kuipe_j
! Initial version
!
!
!***********************************************************************
!
!    Include sobek constants
!
   include '../include/sobcon.i'
   include '../include/sobdim.i'
!
!     Declaration of Parameters:
!
   integer ngrid, ngridm, i1, i2 ,iter ,istep ,exrstp, ibuf3
   real    g, psi, rhow, theta, omcfl,dhtyp
   real    x(ngrid), at1(ngrid)
   real    wf(ngrid), af(ngrid), wt(ngrid), at(ngrid)
   real    chz(ngrid), r(ngrid), alfab(ngrid)
   real    cflpsa(ngrid)
   real    qlatgr(ngrid)
   real    a1m(ngrid)
   real    tauwi(ngrid), ksi(ngrid)
   real    rho(ngrid)
   real    omr,o(ngrid),wfp(ngrid)
   real    cflpse
   real    wtex(ngrid), wfex(ngrid)
   real    solbuf(dmbuf2,7,ngrid)
   integer grid(ngrid)
   logical lsalt, steady, lfrou
!
   double precision h(ngrid), h1(ngrid), h2(ngrid)
   double precision q(ngrid), q1(ngrid), q2(ngrid)

   double precision dt1
   double precision a1(ngridm),&
   &b1(ngridm),&
   &c1(ngridm),&
   &d1(ngridm),&
   &e1(ngridm)
   double precision a2(ngridm),&
   &b2(ngridm),&
   &c2(ngridm),&
   &d2(ngridm),&
   &e2(ngridm)
!
!     Declaration of local variables
!
   integer          i, j, k, ifroud
   double precision a(8), b(8), c(8), d(8), e(8)
   real  ompsi, onemth, dxodt, dt2, dx, e11, e12
   real  hi, hip1, qi, qip1, wfti, wftip1, wtti, wttip1
   real  ui, uip1, ci, cip1
   real  dhi, dqi, dhip1, dqip1, dui, dci, dxdtnw, cflmin, cflmax
   real  afgem, woaf, woor
   real  t1i, t2i, wfowt
   real  ugem, cgem, ccmin, ccplus, smin, splus
   real  ccunl, froude, froud1
   logical    equal
   external   equal
!
   if ( steady ) then
      dt2    = 1.0E6
   else
      dt2    = real(dt1, kind=kind(dt2))
   endif
!
!     Calculate 1 - psi and 1 - theta
!
   ompsi  = 1.0 - psi
   onemth = 1.0 - theta
!
   cflmax = 1.0E9                 !  MB      was 1.0E6
   cflmin = 0.001                 !JK  was 0.1
!
! MB: only theta is used, i.e., theta and theta2 are assumed to be equal!
!
! MB: IMPORTANT: coefficient 1 - wft * alfab / wtt in expression of celerity
!     (see ci and cip1 below) should be > 0 (depends on correct calculation
!     of flow area aft and hence flow width wft)
!
! MB: !!! cflpsa is *not* pseudo Courant number anymore, but Dx/Dt_pseu !!!
!
!     Adaptation of pseudo courant number
!
   if ( iter.eq.1 .or. equal(dhtyp,0.) ) then
!
!        Set Dx/Dt_pseu to initial or to constant value
!        Note! if Omcfl = 0 then cflpsa will remain constant.
!
      do 10 i = i1, i2
         qi = theta * q(i) + onemth * q1(i)
         ui = alfab(i) * qi / af(i)
         wfti = wf(i) + wfex(i)
         wtti = wt(i) + wtex(i)
         wfowt = min(1.0 , wfti / alfab(i) / wtti)
         ci = SQRT( g * af(i) / wtti + (1.0 - wfowt) * ui**2 )
         cflpsa(i)= ( ABS(ui) + ci ) / cflpse
10    continue
!
   else
!
!        Adapt at successive iteration steps
!
      do 15 i = i1, i2
         qi = theta * q(i) + onemth * q1(i)
         ui = alfab(i) * qi / af(i)
         wfti = wf(i) + wfex(i)
         wtti = wt(i) + wtex(i)
         wfowt = min(1.0 , wfti / alfab(i) / wtti)
         ci = SQRT( g * af(i) / wtti + (1.0 - wfowt) * ui**2 )
!
! simple averaging of convergence error
!
         IF ( i .EQ. i1 ) THEN
            dhi = 0.5 * ( ABS(h(i) - h2(i)) + ABS(h(i+1) - h2(i+1)) )
            dqi = 0.5 * ( ABS(q(i) - q2(i)) + ABS(q(i+1) - q2(i+1)) )
         ELSE IF ( i .EQ. i2 ) THEN
            dhi = 0.5 * ( ABS(h(i-1) - h2(i-1)) + ABS(h(i) - h2(i)) )
            dqi = 0.5 * ( ABS(q(i-1) - q2(i-1)) + ABS(q(i) - q2(i)) )
         ELSE
            dhi = 0.25 * ( ABS(h(i-1) - h2(i-1))&
            &+ ABS(h(i+1) - h2(i+1)) )&
            &+ 0.5 * ABS( h(i) - h2(i) )
            dqi = 0.25 * ( ABS(q(i-1) - q2(i-1))&
            &+ ABS(q(i+1) - q2(i+1)) )&
            &+ 0.5 * ABS( q(i) - q2(i) )
         ENDIF
!
         dui = ( alfab(i) * dqi + ABS(ui) * wfti * dhi ) / af(i)
         dci = ( 0.5 * g * wfti * dhi / wtti&
         &+ (1.0 - wfti / alfab(i) / wtti) * ABS(ui) * dui )&
         &/ ci
!
         dxdtnw = omcfl * theta * ( 10.0 * dhtyp ) * ( dui + dci )&
         &+ ( 1.0 - omcfl ) * cflpsa(i)
!
         dxdtnw = MIN(      (ABS(ui) + ci) / cflmin,&
         &MAX( (ABS(ui) + ci) / cflmax, dxdtnw ) )
!
         cflpsa(i) = dxdtnw
15    continue
!
   endif
!
!     NB, wfex and wtex may have been set equal to zero in FLARWI!
!         ----     ----
!
!     Loop over grid points in branch ibr
!
   hip1 = theta * h(i1) + onemth * h1(i1)
   qip1 = theta * q(i1) + onemth * q1(i1)
   uip1 = alfab(i1) * qip1 / af(i1)
   wftip1 = wf(i1) + wfex(i1)
   wttip1 = wt(i1) + wtex(i1)
   wfowt = min(1.0 , wftip1 / alfab(i1) / wttip1)
   cip1 = SQRT( g * af(i1) / wttip1 + (1.0 - wfowt) * uip1**2 )
!
   dhip1 = h(i1) - h1(i1)
   dqip1 = q(i1) - q1(i1)
!
   k = 0
   do 100 i = i1, i2-1
!
      hi = hip1
      qi = qip1
      ui = uip1
      wfti = wftip1
      wtti = wttip1
      ci = cip1
      dhi = dhip1
      dqi = dqip1
!
      hip1 = theta * h(i+1) + onemth * h1(i+1)
      qip1 = theta * q(i+1) + onemth * q1(i+1)
      uip1 = alfab(i+1) * qip1 / af(i+1)
      wftip1 = wf(i+1) + wfex(i+1)
      wttip1 = wt(i+1) + wtex(i+1)
      wfowt = min (1.0 , wftip1 / alfab(i+1) / wttip1)
      cip1 = SQRT( g * af(i+1) / wttip1 + (1.0 - wfowt) * uip1**2 )
!
      dhip1 = h(i+1) - h1(i+1)
      dqip1 = q(i+1) - q1(i+1)
!
      ugem   = 0.5 * ( ui + uip1 )
      cgem   = 0.5 * ( ci + cip1 )
!
!        JK 11-4-97: Limiter aangezet
      ccunl  = cgem
      cgem   = max (cgem, abs(ugem))
!
!       Calculation of Froude number
!
      froude = ABS( ugem / ccunl )
      froud1 = ABS( ui / ci )
!
      ifroud = i
17    continue
      if ( froud1 .ge. 1. .or. froude .ge. 0.8) then
!           ibuf3n = mod(ibuf3,ngrid)+1
!           if (nint(solbuf(2,7,ibuf3n)).eq.-1) then
!c             Next record contains header so store it
!              istepb = solbuf(3,7,ibuf3n)
!              iterb  = solbuf(4,7,ibuf3n)
!           endif
!           indsp = solbuf(1,7,ibuf3)
!           if (istep.ne.nint(solbuf(3,7,indsp)) .or.
!     +         iter .ne.nint(solbuf(4,7,indsp))) then
!c
!c             A new iteration step
!c
!c             write header
!c
!              solbuf(1,7,ibuf3n) = ibuf3n
!              solbuf(2,7,ibuf3n) = -1
!              solbuf(3,7,ibuf3n) = istep
!              solbuf(4,7,ibuf3n) = iter
!              ibuf3 = ibuf3n
!              ibuf3n = mod(ibuf3,ngrid)+1
!              if (nint(solbuf(2,7,ibuf3n)).eq.-1) then
!c                Next record contains header so store it
!                 istepb = solbuf(3,7,ibuf3n)
!                 iterb  = solbuf(4,7,ibuf3n)
!              endif
!           endif
!c
!c          store Froude numbers
!c
!           solbuf(1,7,ibuf3n) = solbuf(1,7,ibuf3)
!           solbuf(2,7,ibuf3n) = ifroud
!           solbuf(3,7,ibuf3n) = froude
!           solbuf(4,7,ibuf3n) = froud1
!           ibuf3 = ibuf3n
!           ibuf3n = mod(ibuf3,ngrid)+1
!           if (nint(solbuf(2,7,ibuf3n)).gt.0) then
!c             Next record contains froude numbers so
!c             overwrite with iteration and step numbers.
!              solbuf(1,7,ibuf3n) = ibuf3n
!              solbuf(2,7,ibuf3n) = -1
!              solbuf(3,7,ibuf3n) = istepb
!              solbuf(4,7,ibuf3n) = iterb
!           endif
         lfrou = .true.
         ifroud = ifroud + 1
         if ( ifroud .eq. i2 ) then
!
!             Calculate and store froude number of last grid point
!
            froud1 = ABS( uip1 / cip1 )
            froude = 0.
            goto 17
         endif
      endif
!
      dx = x(i+1) - x(i)
!
!       Calculate dx / dt
!
      dxodt = dx / dt2
!
      k = k + 1
!
!       Set some values
!
      afgem  = 0.5 * ( af(i) + af(i+1) )
      woaf = 0.5 * ( wfti / af(i) + wftip1 / af(i+1) )
      woor = 0.5 * ( wfti / o(i) / r(i) + wftip1 / o(i+1) / r(i+1) )
!
      ccmin  = cgem - ugem
      ccplus = cgem + ugem
!
!********************************
!        Continuity equation    *
!********************************
!
!        1. First term continuity equation
!        [ Doc. S-FO-001.5KV / Eq. 7-4]
!
      a(1)  = dble (ompsi * wtti   * dxodt)
      b(1)  = 0.0D0
      c(1)  = dble (  psi * wttip1 * dxodt)
      d(1)  = 0.0D0
      e(1)  = dble (-ompsi * (at(i  ) - at1(i  )) * dxodt&
      &- psi * (at(i+1) - at1(i+1)) * dxodt&
      &+ a(1) * dhi&
      &+ c(1) * dhip1 )
!
!        2. Second term continuity equation
!        [ Doc.  S-FO-001.5KV / Eq. 7-5]
!
      a(2)  = 0.0D0
      b(2)  = -theta
      c(2)  = 0.0D0
      d(2)  = theta
      e(2)  = q1(i) - q1(i+1)
!
!        3. Last term continuity equation
!        [ Doc. S-FO-001.5KV / Eq. 7-6]
!        These terms are all zero, the correct
!        values are added later in FLQLAB
!
      a(3) = 0.0D0
      b(3) = 0.0D0
      c(3) = 0.0D0
      d(3) = 0.0D0
      e(3) = dble (qlatgr(i))
!
      if (grid(i) .eq. cgrdcl) then
!
!        Normal grid cell
!
!        4. Addition of pseudo time terms.
!        [ Doc. S-FO-001.6KV / Eq. Z-9]
!
!        Pseudo time stepping extended with provision to account for
!        supercritical flow;  alfsup is range around abs(ugem) = cgem
!        over which pseudo time stepping switches smoothly from subcri-
!        tical to supercritical mode
!
!        smin  = max( 0.0, min( 1.0,
!    +                0.5 - 0.5 * (ugem/cgem + 1.0) / alfsup ) )
!        splus = max( 0.0, min( 1.0,
!    +                0.5 + 0.5 * (ugem/cgem - 1.0) / alfsup ) )
         smin  = 0.0
         splus = 0.0
!
         a(4) = DBLE( theta * cflpsa(i) * wtti&
         &* (ccplus * (1.0 - splus) + ccmin * smin)&
         &/ (cgem + cgem) )
         b(4) = DBLE( theta * cflpsa(i)&
         &* (splus - 1.0 + smin)&
         &/ (cgem + cgem) )
         c(4) = DBLE( theta * cflpsa(i+1) * wttip1&
         &* (ccmin * (1.0 - smin) + ccplus * splus)&
         &/ (cgem + cgem) )
         d(4) = DBLE( theta * cflpsa(i+1)&
         &* (1.0 - smin - splus)&
         &/ (cgem + cgem) )
!
         e(4) = dble ( a(4) * dhi   + b(4) * dqi&
         &+ c(4) * dhip1 + d(4) * dqip1 )
      else
!
!        Structure cell
!
         a(4) = 0.0D0
         b(4) = 0.0D0
         c(4) = 0.0D0
         d(4) = 0.0D0
         e(4) = 0.0D0
      endif
!
!        Evaluation of coefficients a1,b1,c1,d1 and e1.
!
      a1(k) = 0.0D0
      b1(k) = 0.0D0
      c1(k) = 0.0D0
      d1(k) = 0.0D0
      e1(k) = 0.0D0
      do 20 j = 1, 4
         a1(k) = a1(k) + a(j)
         b1(k) = b1(k) + b(j)
         c1(k) = c1(k) + c(j)
         d1(k) = d1(k) + d(j)
         e1(k) = e1(k) + e(j)
20    continue
!
!        Normal grid cell
!
      if (grid(i) .eq. cgrdcl) then
!
!***********************************
!           Momentum equation      *
!***********************************
!
!           Normal gridpoint
!
!           1. Acceleration term momentum equation
!           [ Doc. S-FO-001.5KV / Eq. 7-7]
!
         a(1)  = 0.0D0
         b(1)  = dble (ompsi * dxodt)
         c(1)  = 0.0D0
         d(1)  = dble (psi * dxodt)
         e(1)  = 0.0D0
!
!           2. Convective term momentum equation
!           [ Doc. S-FO-001.5KV / Eq. 7-8]
!
         a(2)  = dble ( theta * ui   * qi   * wfti   / af(i  ) )
         b(2)  = dble (-2.0 * theta * ui   )
         c(2)  = dble (-theta * uip1 * qip1 * wftip1 / af(i+1) )
         d(2)  = dble ( 2.0 * theta * uip1 )
         e(2)  = dble ( ui * qi - uip1 * qip1&
         &+ a(2) * dhi   + b(2) * dqi&
         &+ c(2) * dhip1 + d(2) * dqip1 )
!
!           3. Water level gradient term in momentum equation
!           [ Doc. S-FO-001.5KV / Eq. 7-9]
!
         a(3) = dble ( theta * g&
         &* (0.5 * wfti   * (hip1 - hi) - afgem) )
         b(3) = 0.0D0
         c(3) = dble ( theta * g&
         &* (0.5 * wftip1 * (hip1 - hi) + afgem) )
         d(3) = 0.0D0
!           e(3) = dble (g * afgem * ( h1(i) - h1(i+1) ))
         e(3) = dble ( g * afgem * (hi - hip1)&
         &+ a(3) * dhi&
         &+ c(3) * dhip1 )
!
!           4. Bottom friction term in momentum equation
!           [ Doc. S-FO-001.5KV / Eq. 7-10]
!
         e11  = ompsi * g * dx * abs(qi  )&
         &/ (chz(i  ) * chz(i  ) * r(i  ) * af(i  ))
         e12  =   psi * g * dx * abs(qip1)&
         &/ (chz(i+1) * chz(i+1) * r(i+1) * af(i+1))
         t1i   = woaf
         if (abs(omr - 1.0) .lt. 0.00001) then
!             linearization for R
            t2i   = woor
         else
            t2i   = 0.0
         endif
         a(4) = dble (-theta * e11 * qi * (t1i + t2i))
         b(4) = dble (2.0 * theta * e11)
         c(4) = dble (-theta * e12 * qip1 * (t1i + t2i))
         d(4) = dble (2.0 * theta * e12)
         e(4) = dble (-e11 * qi    - e12 * qip1&
         &+  a(4) * dhi   +  b(4) * dqi&
         &+  c(4) * dhip1 +  d(4) * dqip1 )
!
!           5. Wind friction term in momentum equation
!           [ Doc. S-FO-001.5KV / Eq. 7-11]
!
         a(5)  = 0.0D0
         b(5)  = 0.0D0
         c(5)  = 0.0D0
         d(5)  = 0.0D0
         e(5)  = dble (ompsi * wfp(i  ) * tauwi(i  ) / rhow * dx +&
         &psi * wfp(i+1) * tauwi(i+1) / rhow * dx )
!
!           6. Extra resistance term in momentum equation
!           [ Doc. S-FO-001.5KV / Eq. 7-12]
!
!JK         if (exrstp .eq. 0) then
!JK ARS 8271: Use 'Ksi linearisation always'
         if (exrstp .eq. -999) then
            e11  = ompsi * g * af(i  ) * ksi(i) * abs(q1(i  ))
            e12  =   psi * g * af(i+1) * ksi(i) * abs(q1(i+1))
            a(6) = dble ( e11 * theta * woaf * q1(i  ) )
            b(6) = 0.0D0
            c(6) = dble ( e12 * theta * woaf * q1(i+1) )
            d(6) = 0.0D0
            e(6) = dble ( -e11 * q1(i  ) - e12 * q1(i+1)&
            &+ a(6) * dhi&
            &+ c(6) * dhip1 )
         else
            e11  = ompsi * g * af(i  ) * ksi(i) * abs(qi  )
            e12  =   psi * g * af(i+1) * ksi(i) * abs(qip1)
            a(6) = dble ( e11 * theta * woaf * qi )
            b(6) = dble ( e11 * theta * 2.)
            c(6) = dble ( e12 * theta * woaf * qip1)
            d(6) = dble ( e12 * theta * 2.)
            e(6) = dble ( -e11 * qi  - e12 * qip1&
            &+ a(6) * dhi   + b(6) * dqi&
            &+ c(6) * dhip1 + d(6) * dqip1 )
         endif
!
!           7. Density term in momentum equation
!           [ Doc. S-FO-001.5KV / Eq. 7-13]
!
         a(7)   = 0.0D0
         b(7)   = 0.0D0
         c(7)   = 0.0D0
         d(7)   = 0.0D0
         if ( lsalt ) then
            a(7) = dble (theta * ompsi * g * af(i)&
            &* (rho(i+1) - rho(i)) / rhow)
            c(7) = dble (theta * psi * g * af(i+1)&
            &* (rho(i+1) - rho(i)) / rhow)
            e(7) = dble ( -g * ( ompsi*a1m(i) + psi*a1m(i+1) )&
            &* ( rho(i+1) - rho(i) )/rhow&
            &+ a(7) * dhi&
            &+ c(7) * dhip1 )
         else
            e(7) = 0.0D0
         endif
!
!           8. Addition of pseudo time terms.
!           [ Doc. S-FO-001.6KV / Eq. Z-10]
!
!           Pseudo time stepping extended with provision to account for
!           supercritical flow
!
         a(8) = DBLE( theta * cflpsa(i) * wtti&
         &* ccmin * ccplus * (splus - 1.0 + smin)&
         &/ (cgem + cgem) )
         b(8) = DBLE( theta * cflpsa(i)&
         &* (ccmin * (1.0 - splus) + ccplus * smin)&
         &/ (cgem + cgem) )
         c(8) = DBLE( theta * cflpsa(i+1) * wttip1&
         &* ccplus * ccmin * (1.0 - smin - splus)&
         &/ (cgem + cgem) )
         d(8) = DBLE( theta * cflpsa(i+1)&
         &* (ccplus * (1.0 - smin) + ccmin * splus)&
         &/ (cgem + cgem) )
!
         e(8) = dble ( a(8) * dhi   + b(8) * dqi&
         &+ c(8) * dhip1 + d(8) * dqip1 )
!
!           Evaluation of coefficients a2,b2,c2,d2 and e2.
!
         a2(k) = 0.0D0
         b2(k) = 0.0D0
         c2(k) = 0.0D0
         d2(k) = 0.0D0
         e2(k) = 0.0D0
         do 40 j = 1, 8
            a2(k) = a2(k) + a(j)
            b2(k) = b2(k) + b(j)
            c2(k) = c2(k) + c(j)
            d2(k) = d2(k) + d(j)
            e2(k) = e2(k) + e(j)
40       continue
!
!           Structure cell
!
      else if (grid(i) .eq. cstrcl) then
!
!           structure (computation in routine FLSTRU)
!           Initially fill e2 with Q on time level n
!
         a2(k) =  0.0D0
         b2(k) = -1.0D0
         c2(k) =  0.0D0
         d2(k) =  0.0D0
         e2(k) = dble (q1(i))
      endif
100 continue
!
end
