!!  Copyright (C)  Stichting Deltares, 2012-2023.
!!
!!  This program is free software: you can redistribute it and/or modify
!!  it under the terms of the GNU General Public License version 3,
!!  as published by the Free Software Foundation.
!!
!!  This program is distributed in the hope that it will be useful,
!!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
!!  GNU General Public License for more details.
!!
!!  You should have received a copy of the GNU General Public License
!!  along with this program. If not, see <http://www.gnu.org/licenses/>.
!!
!!  contact: delft3d.support@deltares.nl
!!  Stichting Deltares
!!  P.O. Box 177
!!  2600 MH Delft, The Netherlands
!!
!!  All indications and logos of, and references to registered trademarks
!!  of Stichting Deltares remain the property of Stichting Deltares. All
!!  rights reserved.
      module m_dldifu

      implicit none

      contains


      subroutine dldifu(icreep    ,timest    ,lundia    ,nst       ,icx       ,
     &                  icy       ,j         ,nmmaxj    ,nmmax     ,kmax      ,
     &                  lstsci    ,lstsc     ,lsal      ,ltem      ,lsecfl    ,
     &                  lsec      ,lsed      ,lsts      ,norow     ,irocol    ,
     &                  kcs       ,kcu       ,kfs       ,kfu       ,kfv       ,
     &                  kadu      ,kadv      ,s0        ,s1        ,hu        ,
     &                  hv        ,dps       ,qxk       ,qyk       ,qzk       ,
     &                  guu       ,gvv       ,guv       ,gvu       ,gsqs      ,
     &                  rbnd      ,sigdif    ,sigmol    ,r0        ,r1        ,
     &                  sour      ,sink      ,ws        ,sedtyp    ,thick     ,
     &                  sig       ,dicuv     ,dicww     ,dsdksi    ,dsdeta    ,
     &                  dtdksi    ,dtdeta    ,aak       ,bbk       ,cck       ,
     &                  bdddx     ,bddx      ,bdx       ,bux       ,buux      ,
     &                  buuux     ,uvdwk     ,vvdwk     ,areau     ,areav     ,
     &                  aakl      ,bbkl      ,cckl      ,ddkl      ,kmxsed    ,
     &                  eqmbcsand ,eqmbcmud  ,seddif    ,volum0    ,volum1    ,
     &                  rscale    ,bruvai    ,
!                 this comes out of the gdp tree in the FLOW code
     &                                        eps       ,vicmol    ,
!                 this is WAQ specific
     &                                                              difx      ,
     &                  dify      , intsrt   , dfluxx   , dfluxy              )

!     Deltares - Delft Software Centre

!     Function  : Performs ADI integration in an advanced way (3rd order)

!     Subroutines called: difacr performs horizontal diffusion with anti-creep

!     The problem with holding this routine up-to-date is in the fact that
!         it has its own dynamics in the Delft3D-FLOW environment with a
!         complete team working at the software
!         furthermore the following aspect prevent a one to one correspondence
!         1) the rigorous wish to use a free formatted source code for FLOW
!            that is substantially incompatible with fixed form sources
!            where DELWAQ tries to maintain similarity where possible
!         2) the wish to have all DELWAQ variables documented in the subroutine
!            header whereas FLOW refers to other specific files for this
!         3) the incorporation of the global data pointer gdp that is not
!            available in DELWAQ (this affects all array declarations)
!         4) the use of the 'precision' file that is not available in DELWAQ
!         5) the domain decomposition feature and the associated include file
!            flow_steps_f.inc that are not available in DELWAQ
!         6) the whish to include numereous subtimers where DELWAQ only times
!            the total

      use m_difacr
      use m_srstop
      use timers
      implicit none

!     Arguments           :

!     Kind        Function         Name                             Description

      integer  (4), intent(in   ) :: icreep                       ! If not 0 then anticreep
      real     (4), intent(in   ) :: timest                       ! Half Integration time step [sec.]
      integer  (4), intent(in   ) :: lundia                       ! Unit number of diagnostic file
      integer  (4), intent(in   ) :: nst                          ! Time step number for error message only
      integer  (4), intent(in   ) :: icx                          ! Array increment = nmax in x-direction
      integer  (4), intent(in   ) :: icy                          !                 = 1    in y-direction
      integer  (4), intent(in   ) :: j                            ! Begin pointer for arrays  (=-2*nmax+1)
      integer  (4), intent(in   ) :: nmmaxj                       ! End pointer for arrays    (=nmmax+2*nmax)
      integer  (4), intent(in   ) :: nmmax                        ! Total number of grid pts. (=nmax*mmax)
      integer  (4), intent(in   ) :: kmax                         ! Number of layers in the z-dir.
      integer  (4), intent(in   ) :: lstsci                       ! Total number of constituents
      integer  (4), intent(in   ) :: lstsc                        ! = lstsci without secondary flow
      integer  (4), intent(in   ) :: lsal                         ! Substance nr salinity, 0 or 1
      integer  (4), intent(in   ) :: ltem                         ! Substance nr temperature, 0 or lsal + 1
      integer  (4), intent(in   ) :: lsecfl                       ! Substance nr first secondary flow subst.
      integer  (4), intent(in   ) :: lsec                         ! 1: no equilib; 2 eqiulib. sec flow
      integer  (4), intent(in   ) :: lsed                         ! Number of sediment constituents
      integer  (4), intent(in   ) :: lsts                         ! Total number constituents
      integer  (4), intent(in   ) :: norow                        ! Number of comp. rows  in IROCOL-array
      integer  (4), intent(in   ) :: irocol(5, norow)             ! For boundary cells and boundary types
      integer  (4), intent(in   ) :: kcs   (j:nmmaxj)             ! Constant mask array zeta points (2=bnd)
      integer  (4), intent(in   ) :: kcu   (j:nmmaxj)             ! Constant mask array U-velocity points
      integer  (4), intent(in   ) :: kfs   (j:nmmaxj)             ! Variable mask array zeta points
      integer  (4), intent(in   ) :: kfu   (j:nmmaxj)             ! Variable mask array U-velocity points
      integer  (4), intent(in   ) :: kfv   (j:nmmaxj)             ! Variable mask array V-velocity points
      integer  (4), intent(in   ) :: kadu  (j:nmmaxj,  kmax)      ! Structures in U-velocity points 1 is NOT
      integer  (4), intent(in   ) :: kadv  (j:nmmaxj,  kmax)      ! Structures in V-velocity points 1 is NOT
      real     (4), intent(in   ) :: s0    (j:nmmaxj)             ! Zeta at old time level
      real     (4), intent(in   ) :: s1    (j:nmmaxj)             ! Zeta at new time level
      real     (4), intent(in   ) :: hu    (j:nmmaxj)             ! Total water depth in u-point [m]
      real     (4), intent(in   ) :: hv    (j:nmmaxj)             ! Total water depth in v-point [m]
      real     (4), intent(in   ) :: dps   (j:nmmaxj)             ! Water depth below ref. in zeta-point [m]
      real     (4), intent(in   ) :: qxk   (j:nmmaxj,  kmax)      ! Flow in the X-dir. through U-surface
      real     (4), intent(in   ) :: qyk   (j:nmmaxj,  kmax)      ! Flow in the Y-dir. through V-surface
      real     (4), intent(in   ) :: qzk   (j:nmmaxj,0:kmax)      ! Flow in the Z-dir. through zeta-surface
      real     (4), intent(in   ) :: guu   (j:nmmaxj)             ! Grid distance in y-direction over U-point
      real     (4), intent(in   ) :: gvv   (j:nmmaxj)             ! Grid distance in x-direction over V-point
      real     (4), intent(in   ) :: guv   (j:nmmaxj)             ! Grid distance in y-direction over V-point
      real     (4), intent(in   ) :: gvu   (j:nmmaxj)             ! Grid distance in x-direction over U-point
      real     (4), intent(in   ) :: gsqs  (j:nmmaxj)             ! Horizontal surface area of the grid cell
      real     (4), intent(in   ) :: rbnd  (kmax,lstsc,2,norow)   ! Boundary conditions
      real     (4), intent(in   ) :: sigdif( lstsci )             ! Prandtl/schmidt-numbers for const.
      real     (4), intent(in   ) :: sigmol( lstsci )             ! Moleculair Prandtl-numbers for const.
      real     (4), intent(inout) :: r0    (j:nmmaxj,kmax,lstsci) ! Concentrations at old time level
      real     (4), intent(  out) :: r1    (j:nmmaxj,kmax,lstsci) ! Concentrations at new time level
      real     (4), intent(in   ) :: sour  (j:nmmaxj,kmax,lstsci) ! Source terms
      real     (4), intent(in   ) :: sink  (j:nmmaxj,kmax,lstsci) ! Sink terms
      real     (4), intent(in   ) :: ws    (j:nmmaxj,0:kmax,lsed) ! Settling velocities sediments
      character(4), intent(in   ) :: sedtyp( lsed   )             ! Sediment type: Sand/Floc/User def.
      real     (4), intent(in   ) :: thick ( kmax   )             ! Relative layer thickness
      real     (4), intent(in   ) :: sig   ( kmax   )             ! Sigma coordinate gravity point
      real     (4), intent(in   ) :: dicuv (j:nmmaxj,  kmax)      ! Horizontal Diffusion coeff. [m2/s]
      real     (4), intent(in   ) :: dicww (j:nmmaxj,0:kmax)      ! Vertical   Diffusion coeff. [m2/s]
      real     (4), intent(in   ) :: dsdksi(j:nmmaxj,  kmax)      ! Horizontal gradient salinity (ksi-dir)
      real     (4), intent(in   ) :: dsdeta(j:nmmaxj,  kmax)      ! Horizontal gradient salinity (eta-dir)
      real     (4), intent(in   ) :: dtdksi(j:nmmaxj,  kmax)      ! Horizontal gradient temperature (ksi-dir)
      real     (4), intent(in   ) :: dtdeta(j:nmmaxj,  kmax)      ! Horizontal gradient temperature (eta-dir)
      real     (4), intent(inout) :: aak   (j:nmmaxj,  kmax)      ! Internal work array
      real     (4), intent(inout) :: bbk   (j:nmmaxj,  kmax)      ! Internal work array
      real     (4), intent(inout) :: cck   (j:nmmaxj,  kmax)      ! Internal work array
      real     (4), intent(inout) :: bdddx (j:nmmaxj,  kmax)      ! Internal work array
      real     (4), intent(inout) :: bddx  (j:nmmaxj,  kmax)      ! Internal work array
      real     (4), intent(inout) :: bdx   (j:nmmaxj,  kmax)      ! Internal work array
      real     (4), intent(inout) :: bux   (j:nmmaxj,  kmax)      ! Internal work array
      real     (4), intent(inout) :: buux  (j:nmmaxj,  kmax)      ! Internal work array
      real     (4), intent(inout) :: buuux (j:nmmaxj,  kmax)      ! Internal work array
      real     (4), intent(inout) :: uvdwk (j:nmmaxj,  kmax)      ! Internal work array Jacobi iteration
      real     (4), intent(inout) :: vvdwk (j:nmmaxj,  kmax)      ! Internal work array Jacobi iteration
      real     (4), intent(in   ) :: areau (j:nmmaxj,  kmax)      ! Vertical exchange areas through u-point
      real     (4), intent(in   ) :: areav (j:nmmaxj,  kmax)      ! Vertical exchange areas through v-point
      real     (4), intent(inout) :: aakl  (j:nmmaxj,  kmax,lstsci) ! Internal first lower diagonal
      real     (4), intent(inout) :: bbkl  (j:nmmaxj,  kmax,lstsci) ! Internal diagonal
      real     (4), intent(inout) :: cckl  (j:nmmaxj,  kmax,lstsci) ! Internal first upper diagonal
      real     (4), intent(inout) :: ddkl  (j:nmmaxj,  kmax,lstsci) ! Internal ???
      real     (4), intent(inout) :: kmxsed(j:nmmaxj,  lsed)      ! ???
      logical     , intent(in   ) :: eqmbcsand                    !
      logical     , intent(in   ) :: eqmbcmud                     !
      real     (4), intent(in   ) :: seddif(j:nmmaxj,0:kmax,lsed  ) ! Vertical sediment diffusion coeff.
      real     (4), intent(in   ) :: volum0(j:nmmaxj,  kmax)      ! Volumes at the old time level
      real     (4), intent(in   ) :: volum1(j:nmmaxj,  kmax)      ! Volumes at the new time level
      real     (4), intent(inout) :: rscale(j:nmmaxj,  kmax)      ! Internal work array, row scaling parameter in difu
      real     (4), intent(inout) :: bruvai(j:nmmaxj,0:kmax)      ! Probably an internal wave effect
      real     (4), intent(in   ) :: eps                          ! Error criterium
      real     (4), intent(in   ) :: vicmol                       ! Moleculair viscosity of water
      real     (4), intent(in   ) :: difx  (j:nmmaxj,  kmax)      ! diffusion in x direction
      real     (4), intent(in   ) :: dify  (j:nmmaxj,  kmax)      ! diffusion in y direction
      integer  (4), intent(in   ) :: intsrt                       ! upwind (1) or central (0) differences *WAQ*

      real     (4), intent(  out) :: dfluxx(j:nmmaxj,kmax,lstsci) ! Diffusive flux for anticreep, x direction
      real     (4), intent(  out) :: dfluxy(j:nmmaxj,kmax,lstsci) ! Diffusive flux for anticreep, y direction

!     Local declaration

      integer  (4)  icxy        !  is the maximu of icx and icy
      real     (4)  timesti     !  is 1.0 / timest
      integer  (4)  k           !  loop counter for layers
      integer  (4)  nm          !  loop counter for horizontal gridcells
      integer  (4)  l           !  loop counter for substances
      integer  (4)  nmd         !  accumulation counter within loops
      integer  (4)  nmdd        !  accumulation counter within loops
      integer  (4)  nmu         !  accumulation counter within loops
      integer  (4)  nmuu        !  accumulation counter within loops
      real     (4)  qxu         !  help variable = qxk / 6.0
      real     (4)  qyv         !  help variable = qyk
      real     (4)  qzw         !  help variable = qzk
      integer  (4)  iad1        !  help integer first  term upwind weight
      integer  (4)  iad2        !  help integer second term upwind weight
      integer  (4)  iad3        !  help integer third  term upwind weight
      integer  (4)  j1          !  help integer first  term upwind weight
      integer  (4)  j2          !  help integer second term upwind weight
      integer  (4)  j3          !  help integer third  term upwind weight
      integer  (4)  ndm         !  accumulation counter within loops
      integer  (4)  num         !  accumulation counter within loops
      real     (4)  d0k         !  help variable offdiagonal terms y-direction
      real     (4)  cl          !  help variable concentration of from cell
      real     (4)  cr          !  help variable concentration of to   cell
      real     (4)  flux        !  help variable transport flux
      integer  (4)  maskval     !  help variable 0 if no flux wanted
      integer  (4)  kfw         !  help variable central or backward differences
      real     (4)  adza        !  help variable vertical transport flux from
      real     (4)  adzc        !  help variable vertical transport flux to
      real     (4)  tsg         !  help variable sum of two vertical half distances
      real     (4)  h0          !  help variable waterdepth at old time level
      real     (4)  h1          !  help variable waterdepth at new time level
      real     (4)  h0i         !  help variable 1.0 /h0
      real     (4)  h0new       !  help variable waterdepth at old time level
      real     (4)  h0old       !  help variable waterdepth at new time level
      real     (4)  diz1        !  help variable vertical diffusion
      real     (4)  ddzc        !  help variable vertical diffusion flux
      real     (4)  bi          !  help variable for the double sweep solver
      integer  (4)  ddb         !  domain decomposition effect, set to zero
      integer  (4)  ls          !  help variable for sediment substance nr
      integer  (4)  lst         !  help variable for last non-sediment substance nr
      integer  (4)  ll          !  help variable for accumulation of sediment substance nr
      integer  (4)  n           !  help variable for boundary condition determination
      integer  (4)  mf          !  help variable for boundary condition determination
      integer  (4)  ml          !  help variable for boundary condition determination
      integer  (4)  nmf         !  help variable for boundary condition determination
      integer  (4)  nml         !  help variable for boundary condition determination
      integer  (4)  nmfu        !  help variable for boundary condition determination
      integer  (4)  nmlu        !  help variable for boundary condition determination
      integer  (4)  ic          !  loop counter in the irocol table
      integer  (4)  iter        !  outer iteration counter
      integer  (4)  itr         !  inner iteration counter
      real     (4)  epsitr      !  iteration stop criterion, concentration dependent

      integer(4) ithandl /0/
      if ( timon ) call timstrt ( "dldifu", ithandl )

      icxy  = max (icx,icy)
    !
    !  INITIALIZE
    !
    !
    ! Initialise arrays aak - cck for all (nm,k)
    !
      aak   = 0.0
      buuux = 0.0
      buux  = 0.0
      bux   = 0.0
      bdx   = 0.0
      bddx  = 0.0
      bdddx = 0.0
      cck   = 0.0
    !
      timesti = 1.0 / timest
      do k = 1, kmax
         do nm = 1, nmmax
            if (kfs(nm) == 1 ) then
               bbk(nm, k) = volum1(nm, k) * timesti
            else
               bbk(nm, k) = 1.0
               if (lsec > 0) r0(nm, k, lsecfl) = 0.0
            endif
         enddo
      enddo
      do l = 1, lstsci
         if (lsec==2 .and. l==lsecfl) then
            cycle
         endif
         do k = 1, kmax
            do nm = 1, nmmax
               if ( (kfs(nm)==1) .and. (kcs(nm)==1) ) then
!1st difference: WAQ gives the right hand side (WQ processes) to difu through ddkl !!!!
                  ddkl(nm, k, l) = ddkl(nm, k, l) + volum0(nm, k) * r0(nm, k, l) * timesti
               else
                  ddkl(nm, k, l) = r0(nm, k, l)
               endif
            enddo
         enddo
      enddo
    !
    ! CONTRIBUTION OF ADVECTION IN X-DIRECTION
    !
      do k = 1, kmax
       !
       ! CONTRIBUTION TO VOLUME NM AND NMU
       !
         nmd  = -icx
         nmdd = -icx - icx
         nmu  =  icx
         nmuu =  icx + icx
         do nm = 1, nmmax
            nmd  = nmd  + 1
            nmdd = nmdd + 1
            nmu  = nmu  + 1
            nmuu = nmuu + 1
            qxu  = qxk(nm, k)/6.0
            if (qxu > 0.0) then
               iad1 =      kfu(nm)  *kadu(nm, k)
               iad2 = iad1*kfu(nmd) *kadu(nmd, k)
               iad3 = iad2*kfu(nmdd)*kadu(nmdd, k)
             !
               j1 = 6*iad1 + 3*iad2 +   iad3
               j2 =        - 3*iad2 - 2*iad3
               j3 =                     iad3
             !
               bbk  (nm , k) = bbk  (nm , k) + qxu*j1
               bdx  (nm , k) = bdx  (nm , k) + qxu*j2
               bddx (nm , k) = bddx (nm , k) + qxu*j3
               bdx  (nmu, k) = bdx  (nmu, k) - qxu*j1
               bddx (nmu, k) = bddx (nmu, k) - qxu*j2
               bdddx(nmu, k) = bdddx(nmu, k) - qxu*j3
            else
               iad1 =        kfu(nm)   * kadu(nm  , k)
               iad2 = iad1 * kfu(nmu)  * kadu(nmu , k)
               iad3 = iad2 * kfu(nmuu) * kadu(nmuu, k)
             !
               j1 = 6*iad1 + 3*iad2 +   iad3
               j2 =        - 3*iad2 - 2*iad3
               j3 =                     iad3
             !
               bux  (nm , k) = bux  (nm , k) + qxu*j1
               buux (nm , k) = buux (nm , k) + qxu*j2
               buuux(nm , k) = buuux(nm , k) + qxu*j3
               bbk  (nmu, k) = bbk  (nmu, k) - qxu*j1
               bux  (nmu, k) = bux  (nmu, k) - qxu*j2
               buux (nmu, k) = buux (nmu, k) - qxu*j3
            endif
         enddo
      enddo
    !
    ! CONTRIBUTION OF ADVECTION IN Y-DIRECTION
    !
      do l = 1, lstsci
         if (lsec==2 .and. l==lsecfl) then
            cycle
         endif
         do k = 1, kmax
          !
          ! CONTRIBUTION TO VOLUME NM AND NUM
          !
            ndm = -icy
            num =  icy
            do nm = 1, nmmax
               ndm = ndm + 1
               num = num + 1
               qyv  = qyk(nm, k)
               iad1 = kfv(nm)*kadv(nm, k)
               iad2 = iad1*kfv(num)*kadv(num, k)*kfv(ndm)*kadv(ndm, k)
               if (qyv > 0.0) then
                  d0k = 0.5*qyv*( (2*iad1 - iad2)*r0(nm , k, l)
     &                           +          iad2 *r0(num, k, l))
               else
                  d0k = 0.5*qyv*( (2*iad1 - iad2)*r0(num, k, l)
     &                           +          iad2 *r0(nm , k, l))
               endif
               if (kcs(nm)  == 1) ddkl(nm , k, l) = ddkl(nm , k, l) - d0k
               if (kcs(num) == 1) ddkl(num, k, l) = ddkl(num, k, l) + d0k
            enddo
         enddo
      enddo
    !
    !
    ! Explicit algoritm (call DIFACR) leads to extra stablity criterium
    ! DT <= (DX**2)/(2*DICUV)
    !
    ! This diffusion part (loop 410) is constituent independent.
    ! The value of SIGDIF(L) = 0.7 (see TKECOF) for all LSTSCI
    !
      if (icreep==0 .or. kmax==1) then
       !
       ! HORIZONTAL DIFFUSION IN X-DIRECTION ALONG SIGMA PLANES
       !
         do k = 1, kmax
            !
            ! CONTRIBUTION TO VOLUME NM AND NMU
            !
            nmu = icx
            do nm = 1, nmmax
               nmu = nmu + 1
               if (kfu(nm)*kadu(nm, k) /= 0) then
!2nd difference, WAQ gives diffusion per exchange area directly
                  flux    = difx(nm,k)/(0.7*gvu(nm))        ! check on 0.7 is still pending
                  maskval = max(0, 2 - kcs(nm))
                  bbk(nm, k) = bbk(nm, k) + areau(nm, k)*flux*maskval
                  bux(nm, k) = bux(nm, k) - areau(nm, k)*flux*maskval
                  maskval = max(0, 2 - kcs(nmu))
                  bbk(nmu, k) = bbk(nmu, k) + areau(nm, k)*flux*maskval
                  bdx(nmu, k) = bdx(nmu, k) - areau(nm, k)*flux*maskval
               endif
            enddo
         enddo
       !
       ! HORIZONTAL DIFFUSION IN Y-DIRECTION ALONG SIGMA PLANES
       !
         do l = 1, lstsci
            if (lsec==2 .and. l==lsecfl) then
               cycle
            endif
            do k = 1, kmax
               !
               ! CONTRIBUTION TO VOLUME NM AND NUM
               !
               num = icy
               do nm = 1, nmmax
                  num = num + 1
                  if (kfv(nm)*kadv(nm, k) /= 0) then
                     cl      = r0(nm, k, l)
!3rd difference, WAQ gives values per area directly
!                    difl    = dicuv(nm, k)                              ! WAQ gives values per area
                     cr      = r0(num, k, l)
!                    difr    = dicuv(num, k)
!                    flux    = 0.5*(cr - cl)*(difl + difr)/(0.7*guv(nm))
                     flux    = (cr-cl)* dify(nm,k)/(0.7*guv(nm))   ! check on 0.7 is still pending
                     maskval = max(0, 2 - kcs(nm))
                     ddkl(nm, k, l) = ddkl(nm, k, l) + areav(nm, k)*flux*maskval
                     maskval = max(0, 2 - kcs(num))
                     ddkl(num, k, l) = ddkl(num, k, l) - areav(nm, k)*flux*maskval
                  endif
               enddo
            enddo
         enddo
      else
       !
       ! Explicit algoritm (call DIFACR) leads to extra stablity criterium
       ! dt <= (dx**2)/(2*dicuv)
       !
       ! HORIZONTAL DIFFUSION ALONG Z-PLANES (only if KMAX > 1 and Anti Creep)
       !
         call difacr(icx       ,icy       ,j         ,nmmaxj    ,nmmax     ,
     &               kmax      ,lstsci    ,lsal      ,ltem      ,kcs       ,
     &               kfu       ,kfv       ,kadu      ,kadv      ,s0        ,
     &               dps       ,r0        ,ddkl      ,guu       ,gvv       ,
     &               guv       ,gvu       ,thick     ,sig       ,dicuv     ,
     &               sigdif    ,dsdksi    ,dtdksi    ,dsdeta    ,dtdeta    ,
!4th difference, WAQ has no gdp and dfluxx and dfluxy have been added for mass balances
     &               dfluxx    ,dfluxy                                     )
!                  & gdp       )                             WAQ has no gdp
      endif
      if (kmax > 1) then
         do k = 1, kmax - 1
            if (k==1 .or. k==kmax - 1) then
               kfw = 1
            else
               kfw = 0
            endif
            kfw = max ( kfw , intsrt )    !  WAQ to distinguish backward 1 or central 0 differences
            do nm = 1, nmmax
               !
               ! ADVECTION IN VERTICAL DIRECTION; W*DC/DZ
               !
               if (kfs(nm) == 1) then
                  qzw = qzk(nm, k)
                  if (qzw > 0.0) then
                     adza = 0.5*qzw*(1 - kfw)
                     adzc = 0.5*qzw*(1 + kfw)
                  else
                     adza = 0.5*qzw*(1 + kfw)
                     adzc = 0.5*qzw*(1 - kfw)
                  endif
                  aak(nm, k + 1) = aak(nm, k + 1) + adza
                  bbk(nm, k + 1) = bbk(nm, k + 1) + adzc
                  bbk(nm, k    ) = bbk(nm, k    ) - adza
                  cck(nm, k    ) = cck(nm, k    ) - adzc
               endif
            enddo
         enddo
      endif
      do l = 1, lstsci
         if (lsec==2 .and. l==lsecfl) then
            cycle
         endif
         do k = 1, kmax
            do nm = 1, nmmax
               if (kfs(nm)==1) then
!5th difference WAQ initializes these arrays outside with vertical fluxes per substance
                  aakl(nm, k, l) = aak(nm, k) + aakl(nm, k, l)   ! WAQ initialises these arrays
                  bbkl(nm, k, l) = bbk(nm, k) + bbkl(nm, k, l)   ! with vertical fluxes per substance
                  cckl(nm, k, l) = cck(nm, k) + cckl(nm, k, l)   ! like settling velocities
               endif
!6th difference, WAQ still has processes in temporally dry points
               if ( kfs(nm) .eq. 0 .and. kcs(nm) .eq. 1 ) then
                  bbkl(nm,k,l)=bbkl(nm,k,l) + bbk(nm,k)          ! WAQ still has processes in temp
               endif                                             ! dry points
            enddo
         enddo
      enddo
    !
    ! DIFFUSION IN VERTICAL DIRECTION
    !
      if (kmax > 1) then
         do l = 1, lstsci
          !
          ! l = sediment: ls > 0
          ! else        : ls = 0
          !
            ls = 0
            if ((l>max(lsal, ltem)) .and. (l<=lsts)) ls = l - max(lsal, ltem)
            do k = 1, kmax - 1
               tsg = 0.5 * (thick(k) + thick(k+1))
               do nm = 1, nmmax
                  if (kfs(nm) == 1) then
                     h0  = max(0.1, s0(nm) + dps(nm) )   ! NB. for WAQ at call s0(nm) is 0 and h0 = dps
                     h0i = 1.0 / h0                      !     because WAQ does not know dps
                   !
                   ! Internal wave contribution
                   !
                     diz1 = vicmol/sigmol(l) + dicww(nm, k)/sigdif(l)

                     ddzc             = gsqs(nm) * diz1 * h0i / tsg
                     aakl(nm, k+1, l) = aakl(nm, k+1, l) - ddzc
                     bbkl(nm, k+1, l) = bbkl(nm, k+1, l) + ddzc
                     bbkl(nm, k  , l) = bbkl(nm, k  , l) + ddzc
                     cckl(nm, k  , l) = cckl(nm, k  , l) - ddzc
                  endif
               enddo
            enddo
         enddo
      endif

      do nm = 1, nmmax
         if (kcs(nm) == 2) then
            do l = 1, lstsc
               do k = 1, kmax
                  ddkl(nm, k, l) = r0(nm, k, l)
                  aakl(nm, k, l) = 0.0
                  bbkl(nm, k, l) = 1.0
                  cckl(nm, k, l) = 0.0
               enddo
            enddo
         endif
      enddo
    !
    ! BOUNDARY CONDITIONS
    !     On open boundary no seconday flow (=> loop over LSTSC)
    !
!7th difference: WAQ has no DD features included
      ddb = 0
      do ic = 1, norow
         n    = irocol(1, ic)
         mf   = irocol(2, ic) - 1
         ml   = irocol(3, ic)
         nmf  = (n + ddb)*icy + (mf + ddb)*icx - icxy
         nml  = (n + ddb)*icy + (ml + ddb)*icx - icxy
         nmfu = nmf + icx
         nmlu = nml + icx
         !
         ! IMPLEMENTATION OF BOUNDARY CONDITIONS
         !
         if (kcu(nmf) == 1) then
            do k = 1, kmax
               do l = 1, lstsc
                  ddkl(nmf, k, l) = rbnd(k, l, 1, ic)
               enddo
            enddo
         endif
         if (kcu(nml) == 1) then
            do k = 1, kmax
               do l = 1, lstsc
                  ddkl(nmlu, k, l) = rbnd(k, l, 2, ic)
               enddo
            enddo
         endif
       !
       ! optional Neumann boundary condition for suspended sediment fractions
       !
      enddo
      do l = 1, lstsci
       !
       ! SOURCES AND SINK TERMS
       !
       ! SINKS ARE TREATED IMPLICITLY
       !
       if (lsec==2 .and. l==lsecfl) then
          !
          ! secondary flow (equilibrium equals to new intensity)
          !       start-up problems SINK might be 0.0 when
          !       UMOD = 0.0 in SECRHS
          !
            do k = 1, kmax
               do nm = 1, nmmax
                  if (kfs(nm)*kcs(nm) == 1) then
                     h0new = s1(nm) +      dps(nm)
                     if (abs(sink(nm,k,l)*h0new) > eps) then
                        h0old = s0(nm) +      dps(nm)
                        r1(nm, k, l) = sour(nm, k, l)*h0old/(sink(nm, k, l)*h0new)
                     else
                        r1(nm, k, l) = 0.0
                     endif
                  endif
               enddo
            enddo
         else
            do k = 1, kmax
               do nm = 1, nmmax
                  if (kfs(nm)*kcs(nm) == 1) then
                     bbkl(nm, k, l) = bbkl(nm, k, l) + sink(nm, k, l)*volum1(nm, k)
                     ddkl(nm, k, l) = ddkl(nm, k, l) + sour(nm, k, l)*volum0(nm, k)
                  endif
               enddo
            enddo
          !
          ! set concentrations in dry points and in open boundary points
          !
            do k = 1, kmax
               do nm = 1, nmmax
                  if ((kfs(nm)==0 .and. kcs(nm)==1) .or. kcs(nm)==2) then
                     r1(nm, k, l) = ddkl(nm, k, l)
                  endif
               enddo
            enddo
         endif

         do k = 1, kmax
            do nm = 1, nmmax
               if (kfs(nm)==1) then
                  rscale(nm, k)    = 1.0    / bbkl(nm, k, l)
                  aakl  (nm, k, l) = aakl(nm, k, l) * rscale(nm, k)
                  bbkl  (nm, k, l) = 1.0
                  cckl  (nm, k, l) = cckl(nm, k, l) * rscale(nm, k)
                  ddkl  (nm, k, l) = ddkl(nm, k, l) * rscale(nm, k)
               endif
            enddo
         enddo

         do nm = 1, nmmax
            if ( (kfs(nm)==1) .and. (kcs(nm)==1) ) then
               do k = 2, kmax
                  bi             = 1.0/(bbkl(nm, k, l) - aakl(nm, k, l)*cckl(nm, k - 1, l))
                  bbkl(nm, k, l) = bi
                  cckl(nm, k, l) = cckl(nm, k, l)*bi
               enddo
            endif
         enddo
       !
       ! ITERATION LOOP
       !
         iter = 0
         do k = 1, kmax
            do nm = 1, nmmax
               if (kfs(nm)*kcs(nm) == 1) then
                  r1(nm, k, l) = r0(nm, k, l)
                  uvdwk(nm, k) = r0(nm, k, l)
               endif
            enddo
         enddo
       !
       ! DD code added end
       !
 1100    continue
         iter = iter + 1
       !
       ! ITERATIVE SOLUTION METHOD USING CHECKERBOARD JACOBI
       ! IN HORIZONTAL DIRECTION
       !
         itr = 0
       !
       !   set concentrations in coupling points
       !
         do k = 1, kmax
            do nm = 1, nmmax
               if (kcs(nm) == 3) then
                  r1(nm, k, l) = ddkl(nm, k, l)
               endif
            enddo
         enddo
       !
       ! loop starts at red or black point depending on own subdomain
       !
         do k = 1, kmax
            do nm = 1, nmmax, 2
             !
             ! COMPUTE RIGHT HAND SIDE
             ! ( CHECK FOR KCS TO AVOID AN ARRAY INDEX OUT OF BOUNDS )
             !
               if ( (kfs(nm)==1) .and. (kcs(nm)==1) ) then
                  uvdwk(nm, k) =   bdddx(nm, k) * r1(nm - icx - icx - icx, k, l)
     &                           + bddx (nm, k) * r1(nm - icx - icx, k, l)
     &                           + bdx  (nm, k) * r1(nm - icx, k, l)
     &                           + bux  (nm, k) * r1(nm + icx, k, l)
     &                           + buux (nm, k) * r1(nm + icx + icx, k, l)
     &                           + buuux(nm, k) * r1(nm + icx + icx + icx, k, l)
                  uvdwk(nm, k) = ddkl(nm, k, l) - rscale(nm, k)*uvdwk(nm, k)
               endif
            enddo
         enddo

         do nm = 1, nmmax, 2
            if (kfs(nm)*kcs(nm) == 1) vvdwk(nm, 1) = uvdwk(nm, 1)*bbkl(nm, 1, l)
         enddo
         do k = 2, kmax
            do nm = 1, nmmax, 2
               if (kfs(nm)*kcs(nm) == 1)
     &            vvdwk(nm, k) = (uvdwk(nm, k) - aakl(nm, k, l)*vvdwk(nm, k - 1))
     &            *bbkl(nm, k, l)
            enddo
         enddo
         do k = kmax - 1, 1, -1
            do nm = 1, nmmax, 2
               if (kfs(nm)*kcs(nm) == 1)
     &            vvdwk(nm, k) = vvdwk(nm, k) - cckl(nm, k, l) *vvdwk(nm, k + 1)
            enddo
         enddo
       !
       ! CHECK FOR CONVERGENCE
       !
         do k = 1, kmax
            do nm = 1, nmmax, 2
               if (kfs(nm)*kcs(nm) == 1) then
                  epsitr = max(1.e-8   , 0.5e-3*abs(r1(nm, k, l)))
                  if (abs(vvdwk(nm, k) - r1(nm, k, l)) > epsitr) itr = 1
                  r1(nm, k, l) = vvdwk(nm, k)
               endif
            enddo
         enddo
       !
       ! loop starts at point of other color now (black respectively red)
       !
         do k = 1, kmax
            do nm = 2, nmmax, 2
             !
             ! COMPUTE RIGHT HAND SIDE
             !
               if (kfs(nm)*kcs(nm) == 1) then
                  uvdwk(nm, k) =   bdddx(nm, k) * r1(nm - icx - icx - icx, k, l)
     &                           + bddx (nm, k) * r1(nm - icx - icx, k, l)
     &                           + bdx  (nm, k) * r1(nm - icx, k, l)
     &                           + bux  (nm, k) * r1(nm + icx, k, l)
     &                           + buux (nm, k) * r1(nm + icx + icx, k, l)
     &                           + buuux(nm, k) * r1(nm + icx + icx + icx, k, l)
                  uvdwk(nm, k) = ddkl(nm, k, l) - rscale(nm, k)*uvdwk(nm, k)
               endif
            enddo
         enddo

         do nm = 2, nmmax, 2
            if (kfs(nm)==1 .and. kcs(nm) == 1) then
               vvdwk(nm, 1) = uvdwk(nm, 1)*bbkl(nm, 1, l)
            endif
         enddo
         do k = 2, kmax
            do nm = 2, nmmax, 2
               if ( (kfs(nm)==1) .and. (kcs(nm)==1) ) then
                  vvdwk(nm, k) = (uvdwk(nm,k) -  aakl(nm,k,l)*vvdwk(nm,k-1)) * bbkl(nm,k,l)
               endif
            enddo
         enddo
         do k = kmax - 1, 1, -1
            do nm = 2, nmmax, 2
               if ( (kfs(nm)==1) .and. (kcs(nm)==1) ) then
                  vvdwk(nm, k) = vvdwk(nm, k) - cckl(nm, k, l) *vvdwk(nm, k + 1)
               endif
            enddo
         enddo
       !
       ! CHECK FOR CONVERGENCE
       !
         do k = 1, kmax
            do nm = 2, nmmax, 2
               if (kfs(nm)*kcs(nm) == 1) then
                  epsitr = max(1.e-8   , 0.5e-3*abs(r1(nm, k, l)))
                  if (abs(vvdwk(nm, k) - r1(nm, k, l)) > epsitr) itr = 1
                  r1(nm, k, l) = vvdwk(nm, k)
               endif
            enddo
         enddo
       !
       ! determine global maximum of 'itr' over all nodes
       ! Note: this enables to synchronize the iteration process
       !
         if (itr>0 .and. iter<50) goto 1100

         WRITE(LUNDIA,*) ' NUMBER OF ITERATIONS IN DLDIFU: ',
     &                   ITER,'   FOR CONST. NO. ',L
       !
         if ( iter .ge. 50 ) then                  ! special WAQ version
            write ( lundia, * ) 'max number of iterations exceeded'
            write ( * , * )     'max number of iterations exceeded'
            call srstop(1)
         endif
      enddo

      if ( timon ) call timstop ( ithandl )
      return
      end subroutine dldifu

      end module m_dldifu
