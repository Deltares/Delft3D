!----- AGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2017-2024.                                
!                                                                               
!  This file is part of Delft3D (D-Flow Flexible Mesh component).               
!                                                                               
!  Delft3D is free software: you can redistribute it and/or modify              
!  it under the terms of the GNU Affero General Public License as               
!  published by the Free Software Foundation version 3.                         
!                                                                               
!  Delft3D  is distributed in the hope that it will be useful,                  
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                
!  GNU Affero General Public License for more details.                          
!                                                                               
!  You should have received a copy of the GNU Affero General Public License     
!  along with Delft3D.  If not, see <http://www.gnu.org/licenses/>.             
!                                                                               
!  contact: delft3d.support@deltares.nl                                         
!  Stichting Deltares                                                           
!  P.O. Box 177                                                                 
!  2600 MH Delft, The Netherlands                                               
!                                                                               
!  All indications and logos of, and references to, "Delft3D",                  
!  "D-Flow Flexible Mesh" and "Deltares" are registered trademarks of Stichting 
!  Deltares, and remain the property of Stichting Deltares. All rights reserved.
!                                                                               
!-------------------------------------------------------------------------------

! 
! 

   subroutine fm_erosed()
   !!--description-----------------------------------------------------------------
   !!
   !!    Function: Computes sediment fluxes at the bed using
   !!              the Partheniades-Krone formulations.
   !!              Arrays SOURSE and SINKSE are filled and added
   !!              to arrays SOUR and SINK
   !!              Computes bed load transport for sand sediment
   !!              Arrays SBUU and SBVV are filled.
   !!              Computes vertical sediment diffusion coefficient
   !!              Array SEDDIF is filled
   !!              Includes wave asymmetry effects on sand bed-load
   !!              transport
   !!              Bed slope effects computed at the U and V velocity
   !!              points
   !! Method used: Attention: pointer ll for 'standard' FLOW
   !!              arrays is shifted with lstart
   !!
   !!
   !!--pseudo code and references--------------------------------------------------
   ! NONE
   !!--declarations----------------------------------------------------------------
   use precision
   use mathconsts, only: pi
   use bedcomposition_module
   use morphology_data_module
   use sediment_basics_module
   use m_physcoef, only: ag, vonkar, sag, ee, backgroundsalinity, backgroundwatertemperature, vismol
   use m_sediment, only: stmpar, sedtra, stm_included, mtd, jatranspvel, sbcx_raw,sbcy_raw,sswx_raw,sswy_raw,sbwx_raw,sbwy_raw
   use m_flowgeom, only: bl, lnxi, lnx, ln, dxi, ndx, csu, snu, wcx1, wcx2, wcy1, wcy2, acl, nd, csu, snu, wcl
   use m_flow, only: s0, s1, u1, kmx, zws, hs, &
      iturbulencemodel, z0urou, ifrcutp, hu, spirint, spiratx, spiraty, u_to_umain, frcu_mor, javeg,jabaptist,cfuhi, epshs, taubxu, epsz0
   use m_flowtimes, only: julrefdat, dts, time1
   use unstruc_files, only: mdia
   use unstruc_channel_flow, only: t_branch, t_node, nt_LinkNode
   use message_module, only: write_error
   use MessageHandling, only: LEVEL_INFO, LEVEL_FATAL, mess, setmessage
   use m_transport, only: ised1, constituents, isalt, itemp
   use dfparall
   use m_alloc
   use m_missing
   use m_physcoef, only: frcuni, ifrctypuni
   use m_turbulence, only: vicwws, turkinepsws, rhowat
   use m_flowparameters, only: jasal, jatem, jawave, jasecflow, jasourcesink, v2dwbl, flowWithoutWaves, epshu
   use m_fm_erosed
   use m_bedform
   use m_xbeach_data
   use m_waves
   use m_xbeach_paramsconst
   use m_tables, only: interpolate
   use m_partitioninfo
   use compbsskin_module, only: compbsskin, get_alpha_fluff
   use m_debug
   use m_sand_mud
   !
   implicit none
   !
   real(fp)                                       :: eps = 1.0e-6_fp
   logical                                        :: scour = .false.
   logical                                        :: ubot_from_com
   logical                                        :: flmd2l = .false.
   logical                                        :: wave

   integer                              , pointer :: iunderlyr
   real(prec)       , dimension(:,:)    , pointer :: bodsed
   !
   ! Local parameters
   !
   integer, parameter :: kmax2d = 20
   !
   ! Global variables
   !
   integer                                        :: ltur
   !
   !
   ! Local variables
   !
   integer                       :: i
   integer                       :: istat
   integer                       :: j
   integer                       :: k
   integer                       :: k2d
   integer                       :: kbed
   integer                       :: kmaxsd
   integer                       :: l
   integer                       :: ll
   integer                       :: lstart
   integer                       :: nm
   logical                       :: error
   integer                       :: klc
   integer                       :: kmaxlc
   integer                       :: k1, k2
   logical                       :: suspfrac  ! includes suspended transport via advection-diffusion equation
   logical                       :: javegczu
   real(fp)                      :: afluff
   real(fp)                      :: aks_ss3d
   real(fp)                      :: caks
   real(fp)                      :: caks_ss3d
   real(fp)                      :: chezy
   real(fp)                      :: conc2d
   real(fp)                      :: delr
   real(fp)                      :: di50
   real(fp)                      :: difbot
   real(fp)                      :: drho
   real(fp)                      :: dtmor
   real(fp)                      :: fracf
   real(fp)                      :: maxslope
   real(fp)                      :: grkg
   real(fp)                      :: grm2
   real(fp)                      :: grlyrs
   real(fp)                      :: h0
   real(fp)                      :: h1
   real(fp)                      :: rc
   real(fp)                      :: mfltot
   real(fp)                      :: salinity
   real(fp)                      :: sinktot
   real(fp)                      :: sourfluff
   real(fp)                      :: spirintnm   ! local variable for spiral flow intensity
   real(fp)                      :: taks
   real(fp)                      :: taks0
   real(fp)                      :: tauadd
   real(fp)                      :: tdss      ! temporary variable for dss
   real(fp)                      :: temperature
   real(fp), dimension(max(kmx,1))      :: thicklc
   real(fp), dimension(max(kmx,1))      :: siglc
   real(fp)                      :: thick0
   real(fp)                      :: thick1
   real(fp)                      :: timhr
   real(fp)                      :: trsedeq   ! temporary variable for rsedeq
   real(fp)                      :: tsd
   real(fp)                      :: tsigmol   ! temporary variable for sigmol
   real(fp)                      :: twsk
   real(fp)                      :: ulocal
   real(fp)                      :: ubed
   real(fp)                      :: umean
   real(fp)                      :: ustarc
   real(fp)                      :: utot
   real(fp)                      :: vbed
   real(fp)                      :: velb
   real(fp)                      :: velm
   real(fp)                      :: vlocal
   real(fp)                      :: vmean
   real(fp)                      :: z0rou
   real(fp)                      :: zvelb
   real(fp)                      :: poros
   real(fp)                      :: wstau                 ! dummy for erosilt
   real(fp), dimension(:), allocatable :: evel            ! erosion velocity [m/s]
   real(fp), dimension(0:kmax2d) :: dcww2d
   real(fp), dimension(0:kmax2d) :: sddf2d
   real(fp), dimension(0:kmax2d) :: ws2d
   real(fp), dimension(kmax2d)   :: rsdq2d
   real(fp), dimension(kmax2d), save :: sig2d = &
      (/ -0.0874, -0.2472, -0.3797, -0.4897, -0.5809, -0.6565, -0.7193, &
      & -0.7713, -0.8145, -0.8503, -0.8800, -0.9046, -0.9250, -0.9419, -0.9560,&
      & -0.9676, -0.9773, -0.9854, -0.9920, -0.9975 /)

   real(fp), dimension(kmax2d), save :: thck2d = &
      (/ 0.1747, 0.1449, 0.1202, 0.0997, 0.0827, 0.0686, 0.0569, 0.0472, &
      & 0.0391, 0.0325, 0.0269, 0.0223, 0.0185, 0.0154, 0.0127, 0.0106, 0.0088,&
      & 0.0073, 0.0060, 0.0050 /)

   real(fp), dimension(max(kmx,1))     :: concin3d
   real(fp), dimension(kmax2d)         :: concin2d
   character(256)                      :: errmsg
   double precision                    :: zcc, maxdepfrac
   double precision                    :: ubot
   integer                             :: ierr, kk, Lf, kmxvel, kb, kt
   double precision, allocatable       :: dzdx(:), dzdy(:), u1_tmp(:), ucxq_tmp(:), ucyq_tmp(:)
   double precision, allocatable       :: z0rouk(:), z0curk(:), deltas(:), ua(:), va(:)
   double precision                    :: dzdn, dzds
   double precision                    :: z0u, czu
   !
   real(fp), dimension(:), allocatable :: localpar        !< local array for sediment transport parameters
   !! executable statements -------------------------------------------------------
   !
   !   exit the routine immediately if sediment transport (and morphology) is not included in the simulation
   !
   error = .false.
   if (.not.stm_included) return
   ubot_from_com = jauorbfromswan>0
   timhr = time1/3600.0_fp
   !
   ! Allocate memory
   allocate(dzdx(1:ndx), dzdy(1:ndx), stat=istat)
   if (istat == 0) allocate(localpar (npar), stat = istat)
   if (istat == 0) allocate(ua(1:ndx), va(1:ndx), stat=istat)
   if (istat == 0) allocate(z0rouk(1:ndx), z0curk(1:ndx), deltas(1:ndx), stat=istat)
   if ((istat == 0) .and. (.not. allocated(u1_tmp))) allocate(u1_tmp(1:lnx), ucxq_tmp(1:ndx), ucyq_tmp(1:ndx), stat=ierr)

   localpar = 0.0_fp
   ua = 0d0; va = 0d0; z0rouk = 0d0; z0curk=0d0; 

   if (istat /= 0) then
      error = .true.
      write(errmsg,'(a)') 'fm_erosed::error allocating memory.'
      call mess(LEVEL_FATAL, errmsg)
   endif
   !
   wave = (jawave>0) .and. .not. flowWithoutWaves
   !
   ! Mass conservation; s1 is updated before entering fm_erosed
   !
   if (varyingmorfac) then
      call updmorfac(stmpar%morpar, timhr, julrefdat)
   endif
   !
   ! Reset some arrays before next iteration
   spirintnm = 0.0_fp
   ust2 = 0.0_fp
   !
   ! Use Eulerian velocities if jatranspvel > 0
   !
   u1_tmp = u1 * u_to_umain

   if (jatranspvel > 0 .and. jawave > 0 .and. .not. flowWithoutWaves) then
      u1_tmp = u1 - ustokes
      call setucxucy_mor (u1_tmp)
   else
   !   Calculate cell centre velocities ucxq, ucyq
      if (maxval(u_to_umain) /= 1d0 .or. minval(u_to_umain) /= 1d0) then
         call setucxucy_mor (u1_tmp)
      endif
   endif
   ucxq_tmp = ucxq_mor
   ucyq_tmp = ucyq_mor
   call init_1dinfo()
   call setucxqucyq_mor(u1_tmp, ucxq_tmp, ucyq_tmp)
   !
   if (jawave>2) then
      if ((.not. (jawave==4 .or. jawave==3 .or. jawave==6)) .or. flowWithoutWaves) then
         ktb=0d0     ! no roller turbulence
      else
         do k=1, ndx
            call rollerturbulence(k)  ! sets ktb values
         end do
      end if
   endif
   !
   ! Determine total thickness of the mud layers
   ! to be used in computation of skin friction
   ! (Soulsby&Clarke 2004, EstProc report TR137)
   ! will be used in compbsskin.f90
   !
   if (bsskin) then
      call detthcmud(stmpar%morlyr, thcmud)
   endif
   !
   ! Initialisation:
   ! reset sediment sources and sinks
   !     set default kmxsed layer
   !     set kfsed
   !
   lstart = ISED1 - 1
   !
   ! Reset Sourse and Sinkse arrays for all (l,nm)
   !
   do k = 1, ndx
      call getkbotktop(k, kb, kt)
      kmxsed(k,:)  = kb
   end do
   sinkse  = 0.0_fp
   sourse  = 0.0_fp
   sour_im = 0.0_fp
   ! source and sink terms fluff layer
   if (iflufflyr>0) then
      sinkf = 0.0_fp
      sourf = 0.0_fp
   endif
   !
   ! Reset Sediment diffusion arrays for (l,nmk)
   !
   seddif  = 0.0_fp
   rca     = 0.0_fp
   !
   ! Reset Bed Shear Ratio for all nm and l = 1:lsedtot
   !
   taurat = 0.0_fp
   !
   ! Set zero bedload transport for all nm and l = 1:lsedtot
   !
   sbcx   = 0.0_fp
   sbcy   = 0.0_fp
   e_sbcn = 0.0_fp
   e_sbct = 0.0_fp
   sbwx   = 0.0_fp
   sbwy   = 0.0_fp
   e_sbwn = 0.0_fp
   e_sbwt = 0.0_fp
   sswx   = 0.0_fp
   sswy   = 0.0_fp
   e_sswn = 0.0_fp
   e_sswt = 0.0_fp
   sxtot  = 0.0_fp
   sytot  = 0.0_fp
   rsedeq = 0.0_fp

   ! Set ltur
   ltur = 0
   if (kmx>0) then
      select case (iturbulencemodel)
         case (0,1,2)
            ltur = 0
         case (3,4)
            ltur = 2
      end select
   end if

   do nm = 1, ndx
      if ((s1(nm) - bl(nm)) > sedthr) then
         kfsed(nm) = 1
      else
         kfsed(nm) = 0
      endif
   enddo
   !
   ! Determine fractions of all sediments the top layer and
   ! compute the mud fraction.
   !
   if (lsedtot > 1) then
      call getfrac(stmpar%morlyr,frac      ,anymud    ,mudcnt    , &
         & mudfrac      ,1         ,ndx)
   endif

   ! 3D:
   ! Calculate cell centre velocity components and magnitude
   ! based on velocity in the bottom computational layer
   ! Note: uses downwind velocity at any internal point,
   ! uses internal velocity at any open boundary, uses
   ! half of internal velocity in direction of any
   ! closed boundary or dry point.
   !
   javegczu = (javeg==1 .and. jabaptist>1 .and. kmx==0 )
   !
   do k = 1,ndx                            ! This interpolation is done by considering constant waterdepth per each flow-cell
      h1 = s1(k) - bl(k)                   ! To ensure to get the same results from interpolation based on constant frcu and ifrcutp in the cell centre
                                           ! with considering hs
      if (nd(k)%lnx==0) then
         z0curk(k) = 1d-5                  ! safety if nd(k)%lnx==0. Happens sometimes in case of thin dams
         cycle
      endif

      do LL = 1,nd(k)%lnx
         Lf = nd(k)%ln(LL)
         L = abs( Lf )
         if (javegczu) then 
            if (cfuhi(L)>0d0) then         ! use bed contribution of baptist>1
               czu = 1d0/(cfuhi(L)*max(hu(L),epshu))
               czu = sqrt(czu*ag)
            else
               call getcz(hu(L), frcuni, ifrctypuni, czu, L)
            endif
         else
            if (frcu_mor(L)>0) then
               call getcz(hu(L), frcu_mor(L), ifrcutp(L), czu, L)
            else
               call getcz(hu(L), frcuni, ifrctypuni, czu, L)
            end if
         endif
         !
         z0u = hu(L)*exp(-vonkar*czu/sag - 1d0)         ! differs from delft3d
         if( Lf < 0 ) then
            z0curk(k) = z0curk(k)+wcl(1,L)*z0u
         else
            z0curk(k) = z0curk(k)+wcl(2,L)*z0u
         endif
      enddo
      z0curk(k)=max(epsz0,z0curk(k))
   enddo
   !
   taub = 0d0
   do L=1,lnx
      k1=ln(1,L); k2=ln(2,L)
      z0rouk(k1) = z0rouk(k1)+wcl(1,L)*z0urou(L)   
      z0rouk(k2) = z0rouk(k2)+wcl(2,L)*z0urou(L)
      taub(k1)   = taub(k1)+wcl(1,L)*taubxu(L)        
      taub(k2)   = taub(k2)+wcl(2,L)*taubxu(L)        
   end do
   !
   if (kmx > 0) then            ! 3D
      deltas = 0.05d0
      maxdepfrac = 0.05
      if (jawave>0 .and. v2dwbl>0) then
         deltas = 0d0
         do L=1,lnx
            k1=ln(1,L); k2=ln(2,L)
            deltas(k1) =  deltas(k1) + wcl(1,L)*wblt(L)
            deltas(k2) =  deltas(k2) + wcl(2,L)*wblt(L)
         end do
         maxdepfrac = 0.5d0                        ! cases where you want 2D velocity above the wbl, make sure 2nd criterion applies
      endif
      zcc = 0d0

      do kk = 1, ndx
         call getkbotktop(kk,kb,kt)
         do k = kb, kt
            zcc  = 0.5d0*(zws(k-1)+zws(k))         ! cell centre position in vertical layer admin, using absolute height
            kmxvel = k
            if (zcc>=(bl(kk)+maxdepfrac*hs(kk)) .or. zcc>=(bl(kk)+deltas(kk))) then
               exit
            endif
         enddo

         uuu(kk)   = ucxq_tmp(kmxvel)                  ! discharge based cell centre velocities
         vvv(kk)   = ucyq_tmp(kmxvel)
         umod(kk)  = sqrt(uuu(kk)*uuu(kk) + vvv(kk)*vvv(kk))
         zumod(kk) = zcc-bl(kk)
      end do

      ! If secondary flow, then we consider the bed shear stress magnitude as computed in 3D,
      ! but the direction as computed by the 1DV solution of the secondary flow. Here the
      ! near bed vector is projected back to the original depth averaged direction of the flow.
      if (jasecflow > 0) then
         do kk = 1, ndx
            uuu(kk) = spiratx(kk)*umod(kk)
            vvv(kk) = spiraty(kk)*umod(kk)
         enddo
      end if

   else
      do kk = 1, ndx
         uuu(kk)   = ucxq_mor(kk)
         vvv(kk)   = ucyq_mor(kk)
         umod(kk)  = sqrt(uuu(kk)*uuu(kk) + vvv(kk)*vvv(kk))
         zumod(kk) = hs_mor(kk)/ee
      enddo
   end if
   !
   ! set velocities to zero if not active point for transport
   !
   do nm = 1, ndx
      if (kfsed(nm) == 0) then
         uuu  (nm) = 0.0_fp
         vvv  (nm) = 0.0_fp
         umod (nm) = 0.0_fp
         zumod(nm) = 0.0_fp
      endif
   end do
   !
   ! Get the reduction factor if thickness of sediment at bed is less than
   ! user specified threshold. Also get maximum erosion source SRCMAX
   ! (used for cohesive sediments).
   !
   dtmor = dts * morfac
   !
   call getfixfac(stmpar%morlyr, 1        , ndx     , lsedtot, &                    ! Update underlayer bookkeeping system for erosion/sedimentation
                & ndx          , fixfac   , ffthresh  )
   !
   ! Set fixfac to 1.0 for tracer sediments and adjust frac
   !
   istat = bedcomp_getpointer_integer(stmpar%morlyr, 'IUnderLyr', iunderlyr)        ! iunderlayer=1: mixed, 2: layer bookkeeping
   if (ffthresh>0.0_hp .or. iunderlyr/=1) then
      srcmax = 1.0e+10_fp
   elseif (iunderlyr==1) then
      istat = bedcomp_getpointer_realprec(stmpar%morlyr,'bodsed',bodsed)
      do l = 1, lsed
         if (ffthresh<1.0e-10_fp) then
            !
            ! Compute SRCMAX (only used for cohesive sediments)
            !
            do nm = 1, ndx
               !
               ! If user-specified THRESH is <= 0.0, the erosion flux is effectively not limited by FIXFAC since ffthresh is 1e-10
               ! but by the amount of sediment that is available
               !
               !srcmax(nm, l) = bodsed(l, nm)*cdryb(l)/dtmor
               srcmax(nm, l) = bodsed(l, nm)/dtmor
            enddo
         endif
         !
         if (sedtrcfac(l)>0.0_fp) then
            grkg = 1.0_fp / (rhosol(l)*pi*sedd50(l)**3/6.0_fp) ! Number of grains per kg
            grm2 = 0.5_fp / (pi*sedd50(l)**2) ! Number of grains per m^2 -- Not quite correct: maximum area of grain is pi*r^2 not pi*d^2, using porosity factor of 0.5
            do nm = 1, ndx
               fixfac(nm, l) = 1.0_fp
               grlyrs = bodsed(l, nm) * grkg / grm2 ! Number of grain layers
               frac(nm, l) = min(max(0.0_fp, grlyrs), 1.0_fp)*sedtrcfac(l)
            enddo
         endif
      enddo
   endif

   dzdx = 0d0; dzdy = 0d0
   !
   !================================================================
   !    Start of sand part
   !================================================================
   !
   ! Start of main loop over sediment fractions for suspended sediment
   ! sources, sinks, equilibrium concentrations and vertical diffusion
   ! coefficients, and bed-load transport vector components at water
   ! level points
   !
   do nm = 1, ndx
      !
      ! do not calculate sediment sources, sinks, and bed load
      ! transport in areas with very shallow water.
      !
      if ((s1(nm)-bl(nm))<=epshu) cycle     ! dry
      !
      call getkbotktop(nm, kb, kt)
      if (kfsed(nm) == 0) then              ! shallow but not dry, ie ]epshu sedthresh]
         !
         ! Very shallow water:
         ! set sediment diffusion coefficient
         ! and set zero equilibrium concentrations
         !
         if (kmx>0) then              ! 3D only
            ! at layer interfaces, but not at bed and surface  ! to check...
            do l = 1,lsed
               do k = kb, kt-1
                  !seddif(l, k) = max(vicwws(k),dicoww)
                  seddif(l, k) = vicwws(k)    ! background dico is added in solve_vertical
               enddo
            enddo
            !
            rsedeq(nm,:) = 0d0
         endif
         cycle
      endif
      !
      ! kfsed(nm) == 1
      !
      h0   = max(0.01_fp, s0(nm) - bl(nm))
      h1   = max(0.01_fp, s1(nm) - bl(nm))

      kmaxlc = kmx
      if (kmx>0) then
         !
         ! 3D CASE
         !
         kbed    = kb                                   ! okay, this is safe for Z-layers
         thicklc = 0.0_fp
         klc     = 1
         do k = kt,kb,-1                                ! counts from surface to bottom
            thicklc(klc)   = (zws(k)-zws(k-1))/h1       ! depth fraction, this works for z layers. If only sigma: m_flow::laycof can be used
            klc            = klc+1
         enddo
         siglc    = 0.0_fp
         kmaxlc   = klc-1                                 ! needed for z layers eventually. For sigma, equals kmx
         siglc(1) = -0.5_fp*thicklc(1)
         do klc = 2, kmaxlc
            siglc(klc) = siglc(klc-1) - 0.5_fp*(thicklc(klc) + thicklc(klc-1))
         enddo
      else                       ! 2D
         kbed    = nm            ! okay, kbed index 2D nodes from now on
         kmaxlc  = 1
         thicklc = 1.0_fp
      endif
      !
      ! Compute depth-averaged velocity components at cell centre, discharge based cc velocities
      !
      umean = ucxq_tmp(nm)      ! ok, calculated in getucxucyandsoon
      vmean = ucyq_tmp(nm)
      !
      ! bed shear stress as used in flow, or
      ! skin fiction following Soulsby; "Bed shear stress under
      ! combined waves and currents on rough and smooth beds"
      ! Estproc report TR137, 2004
      !
      if (bsskin) then
         !
         ! Compute bed stress resulting from skin friction
         !
         if (iflufflyr>0) then
            afluff = get_alpha_fluff(iflufflyr, lsed, nm, mfluff(:,nm), stmpar%trapar, stmpar%sedpar, timhr)
         else
            afluff = 0d0
         endif
         !
         if (wave) then
            call compbsskin(umean, vmean, h1, wave, uorb(nm), twav(nm), &
                             & phiwav(nm), thcmud(nm), mudfrac(nm), taub(nm), &
                             & rhowat(kbed), vismol, stmpar%sedpar, afluff)
         else
            call compbsskin(umean, vmean, h1, wave, 0d0, 0d0, &
                             & phiwav(nm), thcmud(nm), mudfrac(nm), taub(nm), &
                             & rhowat(kbed), vismol, stmpar%sedpar, afluff)
         endif
      endif
      !
      ! Input parameters are passed via dll_reals/integers/strings-arrays
      !
      if (max_reals < MAX_RP) then
         write(errmsg,'(a)') 'fm_erosed::Insufficient space to pass real values to transport routine.'
         call mess(LEVEL_FATAL, errmsg)
         error = .true.
         return
      endif
      dll_reals(RP_TAUB ) = real(taub(nm)       ,hp)
      !
      if (max_integers < MAX_IP) then
         write(errmsg,'(a)') 'fm_erosed::Insufficient space to pass integer values to transport routine.'
         call mess(LEVEL_FATAL, errmsg)
         error = .true.
         return
      endif
      dll_integers(IP_NM   ) = nm
      !
      if (max_strings < MAX_SP) then
         write(errmsg,'(a)') 'fm_erosed::Insufficient space to pass strings to transport routine.'
         call mess(LEVEL_FATAL, errmsg)
         error = .true.
         return
      endif
      !
      ! total mass in fluff layer
      !
      mfltot = 0.0_fp
      if (iflufflyr>0) then
         do l = 1, lsed
            mfltot = mfltot + max(0.0_fp,mfluff(l,nm))
         enddo
      endif
      !
      do l = 1, lsedtot
         ll = lstart + l
         !
         ! Copy the globally defined l-dependent parameters of array par to localpar.
         ! All nm-/l-based redefinitions of these parameters are performed
         ! on localpar, thus ensuring that the global array par is not
         ! messed up with specific, nm-/l-dependent data.
         !
         call get_transport_parameters(stmpar%trapar, l, nm, timhr, localpar)
         !
         ! fraction specific quantities
         !
         dll_reals(RP_HIDEX)    = real(hidexp(nm,l) ,hp)
         dll_reals(RP_RHOSL)    = real(rhosol(l) ,hp)
         dll_integers(IP_ISED ) = l
         dll_strings(SP_USRFL)  = dll_usrfil(l)
         !
         if (.not.has_bedload(tratyp(l))) then
            !
            ! sediment transport governed by erosion and deposition fluxes
            !
            dll_reals(RP_D50  ) = 0.0_hp
            dll_reals(RP_DSS  ) = 0.0_hp
            dll_reals(RP_DSTAR) = 0.0_hp
            dll_reals(RP_SETVL) = real(ws(kb, l)  ,hp)
            !
            if (kmx > 0) then
               klc = 0
               wslc   = 0.0_fp
               do kk = kt, kb-1, -1                  ! should follow sigma conventions
                  wslc(klc)   = ws(kk, l)
                  klc=klc+1
               enddo
            else
               klc = 1
               wslc(klc)   = ws(nm, l)
            end if
            !
            ! Fluff layer parameters
            !
            fracf   = 0.0_fp
            if (iflufflyr>0) then
               if (mfltot>0.0_fp) fracf = max(0.0_fp,mfluff(l,nm))/mfltot
            endif
            !
            kmaxsd        = 1                       ! for mud fractions kmaxsd points to the grid cell at the bottom of the water column 
            thick0        = max(thicklc(kmaxsd) * h0, epshs)
            thick1        = max(thicklc(kmaxsd) * h1, epshs)
            !
            call erosilt(thicklc        ,kmaxlc       , wslc        , mdia          , &
                       & thick1         ,thick1       , fixfac(nm,l), srcmax(nm, l) , &                         ! mass conservation
                       & frac(nm,l)     ,oldmudfrac   , flmd2l      , iform(l)      , &
                       & npar           ,localpar     ,max_integers , max_reals     , &
                       & max_strings    ,dll_function(l),dll_handle(l), dll_integers, &
                       & dll_reals      ,dll_strings  ,iflufflyr    , mfltot        , &
                       & fracf          ,maxslope     ,wetslope     , &
                       & error          ,wstau        , sinktot     , sourse(nm,l)  , sourfluff)
            if (error) then
               write(errmsg,'(a)') 'fm_erosed::erosilt returned an error. Check your inputs.'
               call mess(LEVEL_FATAL, errmsg)
            end if
            !
            if (stmpar%morpar%moroutput%sedpar) then
               do i = 1,stmpar%trapar%noutpar(l)
                  j = stmpar%trapar%ioutpar(i,l)
                  stmpar%trapar%outpar(j, nm) = localpar(i)
               enddo
            endif
            !
            if (iflufflyr>0) then
               if (iflufflyr==2) then
                  sinkf(l,nm)  = sinktot*(1.0_fp - depfac(l,nm))
                  sinkse(nm,l) = sinktot*depfac(l,nm)
               else
                  sinkf(l,nm)  = sinktot
                  sinkse(nm,l) = 0.0_fp
               endif
               !
               sourf(l,nm)  = sourfluff
            else
               sinkse(nm,l) = sinktot
               sourse(nm,l) = sourse(nm,l) + sourfluff
            endif
            !
            if (kmx>0) then
               !
               ! For 3D model set sediment diffusion coefficient
               ! NOTE THAT IF ALGEBRAIC TURBULENCE MODEL IS USED THEN WAVES
               ! ONLY AFFECT THE VERTICAL TURBULENT MIXING VIA THE ENHANCED BED
               ! ROUGHNESS
               !
               klc    = 0
               do k = kt, kb-1, -1
                  !seddif(l, k) = max(vicwws(k),dicoww)
                  seddif(l, k) = vicwws(k)
                  klc=klc+1
               enddo
            endif
            !
            ! l runs from 1 to lsedtot, kmxsed is defined for 1:lsed
            ! The first lsed fractions are the suspended fractions (including cohesive ones),
            ! so this works
            !
            kmxsed(nm, l) = kb ! to check
         endif
      enddo ! next sediment fraction
      ua(nm) = real(dll_reals(RP_UAU), fp)                 ! cartesian
      va(nm) = real(dll_reals(RP_VAU), fp)                 ! values checked 20230310 JRE    
      !
   enddo ! next nm point
   !
   ! Reduce the source and sink terms to avoid large bed level changes
   !
   call fm_red_soursin()
   !
   if (stmpar%morpar%moroutput%rawtransports) then
      sbcx_raw = sbcx; sbcy_raw = sbcy;    ! save transports before upwinding and bed slope effects
      sbwx_raw = sbwx; sbwy_raw = sbwy;    ! to compare with analytical solutions
      sswx_raw = sswx; sswy_raw = sswy;
   endif
   !
   ! Add implicit part of source term to sinkse
   !
   do l = 1, lsed
      do nm = 1, ndx
         sinkse(nm, l) = sinkse(nm, l) + sour_im(nm, l)
      enddo
   enddo
   !
   if (jasourcesink==0) then
      sourse=0d0
      sinkse=0d0
   elseif (jasourcesink==1) then
      !
   elseif (jasourcesink==2) then
      sinkse=0d0
   elseif (jasourcesink==3) then
      sourse=0d0
   endif
   !

   deallocate(dzdx, dzdy, stat = istat)
   if (istat == 0) deallocate(localpar, stat = istat)
   if (istat /= 0) then
      error = .true.
      write(errmsg,'(a)') 'fm_erosed::error deallocating memory.'
      call mess(LEVEL_FATAL, errmsg)
   endif

   end subroutine fm_erosed
