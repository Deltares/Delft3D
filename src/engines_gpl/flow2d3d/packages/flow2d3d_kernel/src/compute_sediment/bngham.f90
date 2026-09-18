subroutine bngham(j         ,nmmaxj    ,kmax      ,nmmax     ,lstsci    , &
                & lsed      ,icx       ,icy       ,kfushr    ,kfvshr    , &
                & kcs       ,kfs       ,dudz      ,dvdz      ,u1        , &
                & v1        ,vicmud    ,thick     ,rhowat    ,rho       , &
                & r1        ,dps       ,s1        ,clyint    ,sltint    , &
                & sndint    ,gdp       )
!----- GPL ---------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2011-2023.                                
!                                                                               
!  This program is free software: you can redistribute it and/or modify         
!  it under the terms of the GNU General Public License as published by         
!  the Free Software Foundation version 3.                                      
!                                                                               
!  This program is distributed in the hope that it will be useful,              
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                
!  GNU General Public License for more details.                                 
!                                                                               
!  You should have received a copy of the GNU General Public License            
!  along with this program.  If not, see <http://www.gnu.org/licenses/>.        
!                                                                               
!  contact: delft3d.support@deltares.nl                                         
!  Stichting Deltares                                                           
!  P.O. Box 177                                                                 
!  2600 MH Delft, The Netherlands                                               
!                                                                               
!  All indications and logos of, and references to, "Delft3D" and "Deltares"    
!  are registered trademarks of Stichting Deltares, and remain the property of  
!  Stichting Deltares. All rights reserved.                                      
!                                                                               
!-------------------------------------------------------------------------------
!
!
!!--description-----------------------------------------------------------------
!
!             Module: Subroutine BNGHAM
!           Function: Copied from subroutine STRENGTH
!
!                     Equivalent kinematic viscosity of fluid mud
!                     Eq. B.33 in Ph.D. Thesis of Han Winterwerp.
!
!        Method used: VICMUD is defined at layer interfaces of the
!               Date: 1-05-2006
!                     15-8-2014  Correction for more than 1 fraction (cohesive and/or non-cohesive)
!                                index 1 replaced by lsed 
!         Initial Programmer: R.E. Uittenbogaard in prototype in 1DV.
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use mathconsts
    use globaldata
    use sediment_basics_module
    use morphology_data_module
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    integer                           , pointer :: lundia
    integer                           , pointer :: rheo
    integer         , dimension(:)    , pointer :: sedtyp
    real(fp)                          , pointer :: bin_abingh
    real(fp)                          , pointer :: bin_cnvisco
    real(fp)                          , pointer :: bin_cvisco
    real(fp)                          , pointer :: bin_cyield
    real(fp)                          , pointer :: pow_bng_mix
    real(fp)                          , pointer :: power
    real(fp)        , dimension(:)    , pointer :: rhosol
    real(fp)        , dimension(:,:)  , pointer :: rhocf
    real(fp)        , dimension(:,:)  , pointer :: cfvic
    real(fp)        , dimension(:,:)  , pointer :: cfmu
    real(fp)        , dimension(:,:)  , pointer :: xmu
    real(fp)        , dimension(:,:)  , pointer :: cfty
    real(fp)        , dimension(:,:)  , pointer :: cftau
    real(fp)        , dimension(:,:)  , pointer :: tyield
    real(fp)        , dimension(:,:)  , pointer :: taubh
    real(fp)                          , pointer :: frcdim
    real(fp)                          , pointer :: powa
    real(fp)                          , pointer :: phisim
    real(fp)                          , pointer :: betv
    real(fp)                          , pointer :: bety
    real(fp)                          , pointer :: ayield
    real(fp)                          , pointer :: yieldk
    real(fp)                          , pointer :: watmu
    real(fp)                          , pointer :: avic
    real(fp)                          , pointer :: bvic
    real(fp)                          , pointer :: visck
    real(fp)                          , pointer :: shrco
    real(fp)        , dimension(:,:)  , pointer :: phiclay
    real(fp)        , dimension(:,:)  , pointer :: phisand
    real(fp)                          , pointer :: SluSettParam1
    real(fp)                          , pointer :: SluSettParam2
!
! Global variables
!
    integer                                                 , intent(in)  :: j      !  Begin pointer for arrays which have been transformed into 1D arrays. Due to the shift in the 2nd (M-) index, J = -2*NMAX + 1
    integer                                                 , intent(in)  :: kmax   !  Description and declaration in iidim.f90
    integer                                                 , intent(in)  :: lsed   !  Description and declaration in iidim.f90
    integer                                                 , intent(in)  :: icx    !  Description and declaration in iidim.f90
    integer                                                 , intent(in)  :: icy    !  Description and declaration in iidim.f90
    integer                                                 , intent(in)  :: lstsci !  Description and declaration in iidim.f90
    integer                                                 , intent(in)  :: nmmax  !  Description and declaration in dimens.igs
    integer                                                 , intent(in)  :: nmmaxj !  Description and declaration in dimens.igs
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: kcs    !  Description and declaration in iidim.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: kfs    !  Description and declaration in iidim.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax, lstsci), intent(in)  :: r1     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax)                    :: vicmud
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax)                    :: dudz   !  Description and declaration in rjdim.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax)                    :: dvdz   !  Description and declaration in rjdim.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax)                    :: kfushr    !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax)                    :: kfvshr    !  Description and declaration in esm_alloc_int.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)        , intent(in)  :: u1     !  Description and declaration in rjdim.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)        , intent(in)  :: v1     !  Description and declaration in rjdim.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)        , intent(in)  :: rhowat !  Description and declaration in rjdim.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)        , intent(in)  :: rho    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(kmax)                               , intent(in)  :: thick  !  Description and declaration in rjdim.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: s1
    real(hp), dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: dps
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                            :: clyint
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                            :: sltint
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                            :: sndint
!
! Local variables
!
    integer                             :: ised
    integer                             :: istat
    integer                             :: k
    integer                             :: k0
    integer                             :: ku
    integer                             :: kd
    integer                             :: nm
    integer                             :: nmd
    integer                             :: ndm
    integer                             :: nmu
    integer                             :: num
    integer                             :: claycnt
    integer                             :: silcnt
    integer                             :: sandcnt
    real(fp)                            :: janst
    real(fp)                            :: actcl
    real(fp)                            :: cl1
    real(fp)                            :: cl2
    real(fp)                            :: h1
    real(fp)                            :: depth
    real(fp)                            :: powyie        ! Corresponds with "beta_y" in Hugo's overview
    real(fp)                            :: powvic
    real(fp)                            :: powshr
    real(fp)                            :: rhosol_clay
    real(fp)                            :: rhosol_sand
    real(fp)                            :: rhoclay
    real(fp)                            :: rhosand
    real(fp)                            :: rhosolids
    real(fp)                            :: visbin
    real(fp)                            :: taubin
    real(fp)                            :: cgel
    real(fp)                            :: ts
    real(fp)                            :: volk
    !real(fp)                            :: wku
    !real(fp)                            :: wkk
    real(fp)                            :: wlo
    real(fp)                            :: conlin     ! Corresponds with "lambda" in Hugo's overview
    real(fp)                            :: shear
    real(fp)                            :: solfri
    real(fp)                            :: ssinvi
    real(fp)                            :: ssinyi
    real(fp)                            :: xmu1
    real(fp)                            :: xmu2
    real(fp)                            :: xmu3
    real(fp)                            :: xmusol
    real(fp)                            :: xmuwat
    real(fp)                            :: cffrci
    real(fp)                            :: siinyi
    real(fp)                            :: siinvi
    real(fp)                            :: rhocfi
    real(fp)                            :: actyiei
    real(fp)                            :: cfactyi
    real(fp)                            :: cfactvi
    real(fp)                            :: actvii
    real(fp)                            :: cfmuwa
    real(fp)                            :: cfmuso
    real(fp)                            :: cl3
    real(fp)                            :: cfclin
    real(fp)                            :: cfsafi
    real(fp)                            :: phicli
    real(fp)                            :: cfty1
    real(fp)                            :: ty1
    real(fp)                            :: safri
    real(fp)                            :: phisoi
    real(fp)                            :: shear_u
    real(fp)                            :: shear_v
    real(fp)                            :: tyield_u
    real(fp)                            :: tyield_v
    real(fp)                            :: part1
    real(fp)                            :: part2
    real(fp), dimension(:), allocatable :: rhoint
    real(fp), dimension(:), allocatable :: zkcs
    real(fp), dimension(:), allocatable :: zkw
    real(fp), dimension(:), allocatable :: wup
    real(fp), dimension(:), allocatable :: volcon
    real(fp), dimension(:), allocatable :: phisol
    real(fp), dimension(:), allocatable :: solfrac
    real(fp), dimension(:), allocatable :: ssinfy
    real(fp), dimension(:), allocatable :: ssinfv
    real(fp), dimension(:), allocatable :: cffrc
    real(fp), dimension(:), allocatable :: siinfy
    real(fp), dimension(:), allocatable :: siinfv
    real(fp), dimension(:), allocatable :: cfsafr
    real(fp), dimension(:), allocatable :: safrc
    real(fp), dimension(:), allocatable :: actyie
    real(fp), dimension(:), allocatable :: actvic
    real(fp), dimension(:), allocatable :: cfacty
    real(fp), dimension(:), allocatable :: cfactv
!
!! executable statements -------------------------------------------------------
!
    lundia              => gdp%gdinout%lundia
    rheo                => gdp%gdsedpar%rheologymodel
    sedtyp              => gdp%gdsedpar%sedtyp
    bin_abingh          => gdp%gdsedpar%bin_abingh
    bin_cnvisco         => gdp%gdsedpar%bin_cnvisco
    bin_cvisco          => gdp%gdsedpar%bin_cvisco
    bin_cyield          => gdp%gdsedpar%bin_cyield
    pow_bng_mix         => gdp%gdsedpar%pow_bng_mix
    power               => gdp%gdsedpar%power
    rhosol              => gdp%gdsedpar%rhosol
    rhocf               => gdp%gdsedpar%rhocf
    cfvic               => gdp%gdsedpar%cfvic
    cfmu                => gdp%gdsedpar%cfmu
    xmu                 => gdp%gdsedpar%xmu
    cfty                => gdp%gdsedpar%cfty
    cftau               => gdp%gdsedpar%cftau
    tyield              => gdp%gdsedpar%tyield
    taubh               => gdp%gdsedpar%taubh
    frcdim              => gdp%gdsedpar%rheo_frcdim
    powa                => gdp%gdsedpar%rheo_powa
    phisim              => gdp%gdsedpar%rheo_phisim
    betv                => gdp%gdsedpar%rheo_betv
    bety                => gdp%gdsedpar%rheo_bety
    ayield              => gdp%gdsedpar%rheo_ayield
    yieldk              => gdp%gdsedpar%rheo_yieldk
    watmu               => gdp%gdsedpar%rheo_watmu
    avic                => gdp%gdsedpar%rheo_avic
    bvic                => gdp%gdsedpar%rheo_bvic
    visck               => gdp%gdsedpar%rheo_visck
    shrco               => gdp%gdsedpar%rheo_shrco
    phiclay             => gdp%gdsedpar%phiclay
    phisand             => gdp%gdsedpar%phisand
    SluSettParam1       => gdp%gdsedpar%SluSettParam1
    SluSettParam2       => gdp%gdsedpar%SluSettParam2




    !
    allocate(rhoint  (0:kmax), stat=istat)
    allocate(zkcs    (1:kmax), stat=istat)
    allocate(zkw     (0:kmax), stat=istat)
    allocate(wup     (0:kmax), stat=istat)
    allocate(volcon  (0:kmax), stat=istat)
    allocate(phisol  (0:kmax), stat=istat)
    allocate(solfrac (0:kmax), stat=istat)
    allocate(ssinfy  (0:kmax), stat=istat)
    allocate(ssinfv  (0:kmax), stat=istat)
    allocate(cffrc   (0:kmax), stat=istat)
    allocate(siinfy  (0:kmax), stat=istat)
    allocate(siinfv  (0:kmax), stat=istat)
    allocate(cfsafr  (0:kmax), stat=istat)
    allocate(safrc   (0:kmax), stat=istat)
    allocate(actyie  (0:kmax), stat=istat)
    allocate(actvic  (0:kmax), stat=istat)
    allocate(cfacty  (0:kmax), stat=istat)
    allocate(cfactv  (0:kmax), stat=istat)
    !
    ! Carrier fluid is based on water and non-cohesive sediments:
    ! Use arrays rhocf and cfvic
    ! See Jill::unesco and Jill::cflvic
    !
    do nm =1, nmmax
       do k = 1, kmax
          rhocf(nm,k) = rhowat(nm,k)
       enddo
    enddo
    cfvic = 0.0_fp
    clyint = 0.0_fp
    !
    if (rheo == RHEOLOGY_WINTERWERP_KRANENBURG) then
       powyie = 2.0_fp / (3.0_fp-frcdim)
       powvic = 2.0_fp * (powa+1.0_fp) / 3.0_fp
       powshr = ((powa+1.0_fp)*(3.0_fp-frcdim)) / 3.0_fp
       cl1    = 1.0_fp/3.0_fp
    elseif (rheo == RHEOLOGY_JACOBS_VANKESTEREN) then
       powyie = gdp%gdsedpar%rheo_powyie
       powvic = gdp%gdsedpar%rheo_powvic
       cl1    = 1.0_fp/3.0_fp
!jill  actcl  = 0.38_fp
    elseif (rheo == RHEOLOGY_THOMAS) then
       powyie = gdp%gdsedpar%rheo_powyie
!jill  powvic = gdp%gdsedpar%rheo_powvic       not used in this option
       !
       ! Is this correct? Yes, it seems correct: The carrierfluid does not contain sand
       ! To do: simplify
       cfsafr = 0.0_fp
    endif
    !
    rhosol_clay = 0.0_fp
    rhosol_sand = 0.0_fp
    claycnt   = 0
    silcnt    = 0
    sandcnt   = 0
    do ised = 1, lsed
       if (sedtyp(ised) == SEDTYP_SAND) then
          sandcnt = sandcnt + 1
          rhosol_sand = rhosol_sand + rhosol(ised)
       endif
       if (sedtyp(ised) == SEDTYP_CLAY) then
          claycnt = claycnt + 1
          rhosol_clay = rhosol_clay + rhosol(ised)
       endif
    enddo
    rhosol_sand = rhosol_sand / real(max(1,sandcnt),fp)
    rhosol_clay = rhosol_clay / real(max(1,claycnt),fp)
    !
    do nm =1, nmmax
      nmu = nm + icx
      num = nm + icy
      nmd = nm - icx
      ndm = nm - icy
      !tyield = 0.0_fp

      if (kfs(nm)==1 .and. kcs(nm)<=2) then
        !
        !
        ! Define z-levels of w-points in the conventional Sigma system:
        ! ZKW(0)= free surface and ZKW(Kmax) = bottom
        !
        h1         = max(s1(nm)+real(dps(nm),fp),0.01_fp)
        zkw(0)     = s1(nm)
        do k = 1,kmax
           zkw(k)  = zkw(k-1)-thick(k)*h1
        enddo
        zkw(kmax) = -real(dps(nm),fp) ! for accuracy only      !
        !
        ! Define z-levels of concentration-points
        !
        zkcs(1)    = s1(nm)-0.5*thick(1)*h1
        do k = 2,kmax
           zkcs(k)  = zkcs(k-1)-0.5*(thick(k)+thick(k-1))*h1
        enddo
        !
        do k = 1, kmax
           ! Stress following Bingham model for mud.,
           !
           vicmud(nm,0)    = 0.0_fp
           !
           ku  = min(k+1, kmax)
           wup(k)  = thick(k) / (thick(k)+thick(ku))
           wlo  = 1.0_fp - wup(k)
           if (k == kmax) then
              clyint(nm)   = 0.0_fp
              sltint(nm)    = 0.0_fp
              sndint(nm)   = 0.0_fp
              rhoint(kmax) = rho(nm,kmax)
              do ised = 1, lsed
                 if (sedtyp(ised) == SEDTYP_SAND) then
                    sndint(nm) = sndint(nm) + r1(nm,kmax,ised)
                 endif
                 if (sedtyp(ised) == SEDTYP_CLAY) then
                    clyint(nm) = clyint(nm) + r1(nm,kmax,ised)
                    !
                    ! Contribution of this clay constituent to the density of the carrier fluid
                    !
                    rhocf(nm,k) = rhocf(nm,k) + r1(nm,kmax,ised) * (1.0_fp - rhowat(nm,kmax)/rhosol(ised))
                    !rhocf(nm,k) = rhocf(nm,k) + r1(nm,kmax,ised) * (1.0_fp - rhowat(nm,kmax)/rhosol_clay)
                 endif
              enddo
              ! based on no slip condition at the bed
              dudz(nm,kmax) = 2.0_fp * u1(nm,kmax) / (thick(kmax)*h1)
              dvdz(nm,kmax) = 2.0_fp * v1(nm,kmax) / (thick(kmax)*h1)
           else
              !
              ! k /= kmax
              !
              clyint(nm) = 0.0_fp
              sltint(nm) = 0.0_fp
              sndint(nm) = 0.0_fp
              do ised = 1, lsed
                 if (sedtyp(ised) == SEDTYP_SAND) then
                    sndint(nm) = sndint(nm) + wlo*r1(nm,k,ised) + wup(k)*r1(nm,ku,ised)
                 endif
                 if (sedtyp(ised) == SEDTYP_CLAY) then
                    clyint(nm) = clyint(nm) + wlo*r1(nm,k,ised) + wup(k)*r1(nm,ku,ised)
                    !
                    ! Contribution of this clay constituent to the density of the carrier fluid
                    !
                    rhocf(nm,k) = rhocf(nm,k) + (wlo*r1(nm,k,ised)+wup(k)*r1(nm,ku,ised)) * (1.0_fp - rhowat(nm,k)/rhosol(ised))
                    !rhocf(nm,k) = rhocf(nm,k) + (wlo*r1(nm,k,ised)+wup(k)*r1(nm,ku,ised)) * (1.0_fp - rhowat(nm,k)/rhosol_clay)
                 endif
              enddo
              !
              ! Linear interpolation of RHOWAT in Concentration points in the conventional U-Sigma system
              ! to w-point of the convential U-Sigma system:
              !!
              !kd          = max(k-1,1)
              !ts          = thick(k)+thick(kd)
              !wku         = thick(k )/ts
              !wkk         = thick(kd)/ts
              rhoint(k)   = wlo*rho(nm,k) + wup(k)*rho(nm,ku)
           endif
           !
           if ( clyint(nm) <= eps_fp) exit  ! if clay concentration close to zero, exit from the loop.
           clyint(nm)   = max (0.0_fp,clyint(nm))
           sltint(nm)   = max (0.0_fp,sltint(nm))
           sndint(nm)   = max (0.0_fp,sndint(nm))
           !
           ! clayint/rhosol_clay => Jill::unesco::phicl
           ! sandint/rhosol_sand => Jill::unesco::phisa
           !
           phiclay(nm,k) = clyint(nm) / max(rhosol_clay , eps_fp)
           phisand(nm,k) = sndint(nm) / max(rhosol_sand , eps_fp)
           ! the Bingham model including viscosity of the mixture:
           !
           ! volcon => Jill::mudvic::phiss
           !
           volcon(k) = 0.0_fp
           if (sandcnt > 0) then
              volcon(k) = volcon(k) + phisand(nm,k)
           endif
           phisol(k) = volcon(k)
           if (claycnt > 0) then
              phisol(k) = phisol(k) + phiclay(nm,k)
           endif
           !
           !if (rheo /= RHEOLOGY_WINTERWERP_KRANENBURG) then
           !   !
           !   ! CHECK ON VOLCON 
           !   !ss
           !   volcon(k) = MIN( 0.67_fp , volcon(k) )
           !   !
           !   volk      = volcon(k) / (1.0_fp-volcon(k))
           !   visbin    = bin_cvisco * volk**bin_cnvisco
           !   taubin    = bin_cyield * volk**pow_bng_mix
           !   !
           !   ! CHECK ON Bingham stress
           !   !
           !   !   taubin = MIN( 1.0 , taubin )  ???? date 24 april 2014
           !   !vicmud(nm,k) =   visbin + bin_abingh*taubin &
           !   !            &             / (1.0+bin_abingh*sqrt(dudz(nm,k)**2+dvdz(nm,k)**2))
           !   !vicmud(nm,k) = vicmud(nm,k) / rhoint(k)
           !endif
        enddo !k-loop
        !
        do k = 1, kmax
           if (rheo == RHEOLOGY_WINTERWERP_KRANENBURG) then
              solfrac(k) = phiclay(nm,k) / (1.0_fp-volcon(k))
              !
              ! Linear concentration sand        
              !
              if (volcon(k) == 0) then
                  conlin = 0
              else
                 cl2 = ((phisim/volcon(k))**cl1) - 1.0_fp
                 conlin = 1.0_fp / cl2
              endif
              ssinfy(k) = exp(bety*conlin)
              ssinfv(k) = exp(betv*conlin)
              cffrc(k)  = phiclay(nm,k)
              siinfy(k) = 1.0_fp
              siinfv(k) = 1.0_fp
           elseif (rheo == RHEOLOGY_JACOBS_VANKESTEREN) then
              ! next line differs from RHEOLOGY_WINTERWERP_KRANENBURG
              solfrac(k) = (1.0_fp-phisol(k)) / phiclay(nm,k)
              !
              ! Linear concentration sand        
              !
              if (volcon(k) == 0) then
                 conlin = 0
              else
                 cl2 = ((phisim/volcon(k))**cl1) - 1.0_fp
                 conlin = 1.0_fp / cl2
              endif
              ssinfy(k) = exp(bety*conlin)
              ssinfv(k) = exp(betv*conlin)
              !
              rhoclay   = phiclay(nm,k) / (phiclay(nm,k)+phisand(nm,k)) * rhosol_clay
              rhosand   = phisand(nm,k) / (phiclay(nm,k)+phisand(nm,k)) * rhosol_sand
              rhosolids = rhoclay + rhosand
!jill         actyie(k) = (rhowat(nm,k)/(actcl*rhosolids))**powyie
!jill         actvic(k) = (rhowat(nm,k)/(actcl*rhosolids))**powvic
              ! next line differs from RHEOLOGY_WINTERWERP_KRANENBURG
              cffrc(k)  = (1.0_fp-phiclay(nm,k)) / phiclay(nm,k)
              siinfy(k) = 1.0_fp
              siinfv(k) = 1.0_fp
!jill         cfacty(k) = (rhowat(nm,k)/(actcl*rhosol_clay))**powyie
!jill         cfactv(k) = (rhowat(nm,k)/(actcl*rhosol_clay))**powvic
           elseif (rheo == RHEOLOGY_THOMAS) then
              safrc(k) = phisand(nm,k) / phisol(k)
           endif
        enddo !k-loop
        !
        do k = 1, kmax
           kd = max(1,k)
           ku = min(k+1,kmax)
           wlo = 1.0_fp - wup(ku)
           !
           solfri = solfrac(kd)*wlo + solfrac(ku)*wup(ku)
           ssinyi = ssinfy (kd)*wlo + ssinfy (ku)*wup(ku)
           ssinvi = ssinfv (kd)*wlo + ssinfv (ku)*wup(ku)
           if (rheo == RHEOLOGY_THOMAS) then
              cfsafi       = cfsafr (   kd)*wlo + cfsafr (   ku)*wup(ku)
              safri        = safrc  (   kd)*wlo + safrc  (   ku)*wup(ku)
              phicli       = phiclay(nm,kd)*wlo + phiclay(nm,ku)*wup(ku)
              phisoi       = phisol    (kd)*wlo + phisol    (ku)*wup(ku)
              cfty1        = ayield * ((1.0_fp-cfsafi)*(phicli/(1.0_fp-cfsafi*phicli)))**powyie
              cfty(nm,k)   = cfty1  * ((1.0_fp-((cfsafi*phicli)/(yieldk*phisim))))**(-2.5_fp)
              ty1          = ayield * ((1.0_fp-safri)*(phisoi/(1.0_fp-safri*phisoi)))**powyie
              tyield(nm,k) = ty1    * ((1.0_fp-((safri*phisoi)/(yieldk*phisim))))**(-2.5_fp)
           else
              !
              ! RHEOLOGY_WINTERWERP_KRANENBURG or RHEOLOGY_JACOBS_VANKESTEREN
              !
              cffrci = cffrc   (kd)*wlo + cffrc   (ku)*wup(ku)
              siinyi = siinfy  (kd)*wlo + siinfy  (ku)*wup(ku)
              if (rheo == RHEOLOGY_WINTERWERP_KRANENBURG) then
                 tyield(nm,k) = ayield*ssinyi*solfri**powyie  ! solfri is different
                 cfty(nm,k)   = ayield*siinyi*cffrci**powyie
              elseif (rheo == RHEOLOGY_JACOBS_VANKESTEREN) then
!jill            actyiei      = actyie(kd)*wlo + actyie(ku)*wup(ku)
!jill            tyield(nm,k) = ayield*ssinyi*actyiei*solfri**powyie
                 tyield(nm,k) = ayield*ssinyi*solfri**powyie   ! solfri is different 
                 janst = 1.0
!jill            cfactyi      = cfacty(kd)*wlo + cfacty(ku)*wup(ku)
!jill            cfty(nm,k)   = siinyi*ayield*cfactyi*cffrci**powyie
                 cfty(nm,k)   = ayield*siinyi*cffrci**powyie
                 !write(11,*)tyield(nm,k),cfty(nm,k)
!jill            cfactvi      = cfactv(kd)*wlo + cfactv(ku)*wup(ku)
!jill            actvii       = actvic(kd)*wlo + actvic(ku)*wup(ku)
              endif
              siinvi = siinfv  (kd)*wlo + siinfv  (ku)*wup(ku)
           endif
           rhocfi = rhocf(nm,kd)*wlo + rhocf(nm,ku)*wup(ku)
           !
           part1=0.5*(dudz(nm,k)+dudz(nmd,k))
           part2=0.5*(dvdz(nm,k)+dvdz(ndm,k))
           shear = sqrt(part1**2+part2**2) 
           if (shear < 1.0e-10_fp) then
              if (rheo == RHEOLOGY_WINTERWERP_KRANENBURG) then
                 vicmud(nm,k) = 1.0e4_fp
                 xmuwat       = ssinvi * watmu 
                 xmusol       = ssinvi * avic * (solfri**powvic) * (shear**(-powshr))
                 xmu(nm,k)    = xmuwat + xmusol
                 taubh(nm,k)  = tyield(nm,k) 
              elseif (rheo == RHEOLOGY_JACOBS_VANKESTEREN) then
                 vicmud(nm,k) = 1.0e4_fp
                 xmuwat       = ssinvi * watmu
                 xmusol       = ssinvi * avic * (solfri**powvic)
                 xmu(nm,k)    = xmuwat + xmusol
                 taubh(nm,k)  = tyield(nm,k) 
              elseif (rheo == RHEOLOGY_THOMAS) then
                 vicmud(nm,k) = 1.0e4_fp
                 xmu1       = ((cfsafi*phicli)/(1.0_fp-phicli)) / (1.0_fp+((phicli)/(1.0_fp-phicli)))
                 xmu2       = 1.0_fp - xmu1*(1.0_fp/(visck*phisim))
                 xmu3       = (xmu2)**(-2.5_fp)
                 cfmu(nm,k) = (1.0_fp/10.0_fp) * (xmu3) * exp((bvic*(1.0_fp-cfsafi)*(phicli)/(1.0_fp-phicli)))
              endif
              cfvic (nm,k) = 1.0e4_fp
           else
              if (rheo == RHEOLOGY_WINTERWERP_KRANENBURG) then
                 xmuwat       = ssinvi * watmu 
                 xmusol       = ssinvi * avic * (solfri**powvic) * (shear**(-powshr))
                 xmu(nm,k)    = xmuwat + xmusol
                 taubh(nm,k)  = tyield(nm,k) * (1-exp(-shrco*shear)) + xmu(nm,k)* shear      
                 vicmud(nm,k) = taubh(nm,k) / shear      
                 vicmud(nm,k) = vicmud(nm,k) / rhoint(k)
              elseif (rheo == RHEOLOGY_JACOBS_VANKESTEREN) then
                 xmuwat       = ssinvi * watmu
!jill            xmusol       = ssinvi * avic * actvii * (solfri**powvic)
                 xmusol       = ssinvi * avic * (solfri**powvic)
                 xmu(nm,k)    = xmuwat + xmusol
                 taubh(nm,k)  = tyield(nm,k) * (1-exp(-shrco*shear)) + xmu(nm,k)*shear
                 vicmud(nm,k) = taubh(nm,k) / shear      
                 vicmud(nm,k) = vicmud(nm,k) / rhoint(k)
              elseif (rheo == RHEOLOGY_THOMAS) then
                 xmu1         = ((safri*phisoi)/(1.0_fp-phisoi)) / (1.0_fp+((phisoi)/(1.0_fp-phisoi)))
                 xmu2         = 1.0_fp - xmu1*(1.0_fp/(visck*phisim))
                 xmu3         = (xmu2)**(-2.5_fp)  
                 xmu(nm,k)    = (1.0_fp/10.0_fp) * (xmu3) * exp((bvic*(1.0_fp-safri)*(phisoi)/(1.0_fp-phisoi)))
                 taubh(nm,k)  = tyield(nm,k) * (1-exp(-shrco*shear)) + xmu(nm,k)*shear
                 vicmud(nm,k) = taubh(nm,k) / shear      
                 vicmud(nm,k) = vicmud(nm,k) / rhoint(k)
              endif
              if (rheo == RHEOLOGY_WINTERWERP_KRANENBURG) then
                 cfmuwa     = siinvi * watmu 
                 cfmuso     = siinvi * avic * (cffrci**powvic) * (shear**(-powshr))
                 cfmu(nm,k) = cfmuwa + cfmuso
              elseif (rheo == RHEOLOGY_JACOBS_VANKESTEREN) then
                 cfmuwa     = siinvi * watmu  
!jill            cfmuso     = siinvi * avic * cfactvi * (cffrci**powvic) * shear
                 cfmuso     = siinvi * avic * (cffrci**powvic) ! viscosity solid total sediment
                 cfmu(nm,k) = cfmuwa + cfmuso
              elseif (rheo == RHEOLOGY_THOMAS) then
                 xmu1       = ((cfsafi*phicli)/(1.0_fp-phicli)) / (1.0_fp+((phicli)/(1.0_fp-phicli)))
                 xmu2       = 1.0_fp - xmu1*(1.0_fp/(visck*phisim))
                 xmu3       = (xmu2)**(-2.5_fp)
                 cfmu(nm,k) = (1.0_fp/10.0_fp) * (xmu3) * exp((bvic*(1.0_fp-cfsafi)*(phicli)/(1.0_fp-phicli)))
              endif
              cftau(nm,k) = cfty(nm,k) * (1-exp(-shrco*shear)) + cfmu(nm,k)* shear
              ! Divide cfmu by shear to write the correct value to output
              cfmu(nm,k)  = cfmu(nm,k)  / shear   ! carrier fluid dynamic viscosity
              cfvic(nm,k) = cftau(nm,k) / shear   ! carrier fluid apparent viscosity
              cfvic(nm,k) = cfvic(nm,k) / rhocfi  ! carrier fluid apparent viscosity/density = kinematic viscosity
           endif
        enddo !k-loop
        !
        do k = 1, kmax
           !
           ! CHECK NAN's
           !
           if (isnan(vicmud(nm,k))) then
              write (lundia,'(a,3i5,3f10.3)') '*** ERROR: vicmud NaN:',nm,k,nm+icx, &
              & vicmud(nm,k),h1,s1(nm) + dps(nm)
              !write (lundia,*) visbin,bin_abingh,taubin,rhoint(k),dudz(nm,k),dvdz(nm,k)
              !write (lundia,*) 'xxx',taubin,bin_cyield,volk,power
              !write (lundia,*) 'yyy',volcon(k),silint,rhosol(1)
              !write (lundia,*) 'yyy',rho(nm,k),rho(nm,k+1),rho(nm+icx,k),rho(nm+icx,k+1)
              !write (lundia,*) 'zzz',rhocf(nm,k),phisand(nm,k),phiclay(nm,k)
              !write (lundia,*) 'zzz2',r1(nm,k,1),r1(nm,k,2),r1(nm+icx,k,1),r1(nm+icx,k,2)
              
              call d3stop(1, gdp)
           endif
        enddo !k-loop  
       endif ! KFS-check
    enddo  ! nm-loop
        !
        ! baroclinic term
        !
    do nm = 1,nmmax  
       nmu=nm+icx
       num=nm+icy  
       if (kfs(nm)*kfs(nmu)==1) then
          do k = 1, kmax
             shear_u  = min(vicmud(nm,k),vicmud(nmu,k))*dudz(nm,k)**2
             tyield_u = 0.5_fp*(tyield(nm,k)+tyield(nmu,k))
              if (tyield_u > shear_u) then
                 kfushr(nm,k) = 0
             else
                kfushr(nm,k) = 1
             endif    
          enddo !k-loop  
       endif ! two neighbouring cells are wet
       if (kfs(nm) *kfs(num)==1) then
         do k = 1,kmax
             shear_v  = min(vicmud(nm,k),vicmud(num,k))* dvdz(nm,k)**2
             tyield_v = 0.5_fp*(tyield(nm,k)+tyield(num,k))
             if (tyield_v > shear_v) then
                 kfvshr(nm,k) = 0
             else
                kfvshr(nm,k) = 1
             endif
          enddo !k-loop  
       endif ! two neighbouring cells are wet
    enddo  ! nm-loop
    !
    deallocate(rhoint , stat=istat)
    deallocate(zkcs   , stat=istat)
    deallocate(zkw    , stat=istat)
    deallocate(wup    , stat=istat)
    deallocate(volcon , stat=istat)
    deallocate(phisol , stat=istat)
    deallocate(solfrac, stat=istat)
    deallocate(ssinfy , stat=istat)
    deallocate(ssinfv , stat=istat)
    deallocate(cffrc  , stat=istat)
    deallocate(siinfy , stat=istat)
    deallocate(siinfv , stat=istat)
    deallocate(cfsafr , stat=istat)
    deallocate(safrc  , stat=istat)
    deallocate(actyie , stat=istat)
    deallocate(actvic , stat=istat)
    deallocate(cfactv , stat=istat)
    deallocate(cfacty , stat=istat)
    !
end subroutine bngham
