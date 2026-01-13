subroutine limrhg(nst,     hdt       ,icx       ,icy       , &
              & nmmax     ,kmax      , &
              & gsqs      ,guu       ,gvu       ,gvv       ,guv       , &
              & guz       ,gvz       ,gud       ,gvd       , &
              & fcorio    ,norow     ,irocol    ,kcs       ,kfs       , &
              & kspu      ,kspv      ,s1        ,pship     ,precip    , &
              & kfsice    ,kfssnw    ,kfu       ,kfv       , &
              & u1        ,v1        ,u_ice     ,v_ice     ,a_ice     , &
              & h_ice     ,h_snow    ,t_ice     ,t_snow    ,wght      , &
              & zs1       ,zs2       ,zs12      ,ut_ice    ,vt_ice    , &
              & vol_ice   ,vol_snow  , &
              & uuu       ,vvv       ,uuu_ice   ,vvv_ice   , &
              & zpresh    ,zpreshc   ,zfrld1    ,zfrld2    , &
              & zmass1    ,zmass2    ,za1ct     ,za2ct     , &
              & zf1       ,zf2       ,zc1       , &     
              & gdp       )
!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2013.                                     
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
!  $Id: limrhg.f90 64423 2019-07-24 11:30:09Z goede $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20190705_ice_modelling/src/engines_gpl/flow2d3d/packages/kernel/src/compute_ice/limrhg.f90 $
!!--description-----------------------------------------------------------------
!
!    Function: Computes sea ice rheology
!              Taken from LIM3 (see routine LIM_RHG.F90)
! Method used:                    EVP-C-grid
! 
!  ** purpose: determines sea ice drift from wind stress, ice-ocean
!   stress and sea-surface slope. Ice-ice interaction is described by 
!   a non-linear elasto-viscous-plastic (EVP) law including shear 
!   strength and a bulk rheology (Hunke and Dukowicz, 2002).	
! 
!  ** Inputs : - wind forcing (stress), oceanic currents
!                ice total volume (vol_ice) per unit area
!                snow total volume (vol_snow) per unit area
!
!  ** Action : - compute u_ice, v_ice : the components of the 
!                sea-ice velocity vector
!              - compute delta_i, shear_i, divu_i, which are inputs
!                of the ice thickness distribution
! 
!  ** Steps  : 1) Compute ice snow mass, ice strength 
!              2) Compute wind, oceanic stresses, mass terms and
!                 coriolis terms of the momentum equation
!              3) Solve the momentum equation (iterative procedure)
!              4) Prevent high velocities if the ice is thin
! 
!!--pseudo code and references--------------------------------------------------
!                 Hunke and Dukowicz, JPO97
!                 Bouillon et al., 08, in prep (update this when published)
!                 Vancoppenolle et al., OM08
!!--declarations----------------------------------------------------------------
    use precision
    use mathconsts
    use timers
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    real(fp)                , pointer :: eps
    integer                 , pointer :: lice
    integer                 , pointer :: lsnow
    integer                 , pointer :: lundia
    real(fp)                , pointer :: ag
    real(fp)                , pointer :: cp_ice
    real(fp)                , pointer :: cp_snow
    real(fp)                , pointer :: k_ice
    real(fp)                , pointer :: k_snow
    real(fp)                , pointer :: lh_ice
    real(fp)                , pointer :: lh_snow
    real(fp)                , pointer :: rhow
    real(fp)                , pointer :: rho_ice
    real(fp)                , pointer :: rho_snow
    real(fp)                , pointer :: timjan
    real(fp)                , pointer :: timhr
    real(fp)                , pointer :: tair
!
! Global variables
!
integer                                                               :: icx, nst
integer                                                               :: icy
integer, dimension(5, norow)                                          :: irocol
integer                                                               :: kmax
integer, dimension(gdp%d%nmlb:gdp%d%nmub)                             :: kfsice
integer, dimension(gdp%d%nmlb:gdp%d%nmub)                             :: kfssnw
integer, dimension(gdp%d%nmlb:gdp%d%nmub)                             :: kfu
integer, dimension(gdp%d%nmlb:gdp%d%nmub)                             :: kfv
integer, dimension(gdp%d%nmlb:gdp%d%nmub,0:kmax)                      :: kspu
integer, dimension(gdp%d%nmlb:gdp%d%nmub,0:kmax)                      :: kspv
integer                                                               :: nmmax
integer                                                               :: norow
integer, dimension(gdp%d%nmlb:gdp%d%nmub)                             :: kcs
integer, dimension(gdp%d%nmlb:gdp%d%nmub)                             :: kfs
!
real(fp)                                                , intent(in)  :: hdt
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: fcorio
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                            :: h_ice
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                            :: h_snow
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                            :: guu  ! array guu in LIM3
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                            :: gvu  ! array E1U in LIM3
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                            :: guv  ! array E2V in LIM3
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                            :: gvv  ! array E1V in LIM3
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                            :: guz  ! array E2T in LIM3
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                            :: gvz  ! array E1T in LIM3
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                            :: gud  ! array E2F in LIM3
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                            :: gvd  ! array E1F in LIM3
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                            :: gsqs ! array AREA in LIM3
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: precip
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                            :: pship
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                            :: s1
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                            :: t_ice
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                            :: t_snow
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub,kmax)                       :: u1
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub,kmax)                       :: v1
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                            :: uuu
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                            :: vvv
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                            :: uuu_ice
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                            :: vvv_ice
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: ut_ice
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: vt_ice
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub,2,2)                        :: wght
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                            :: vol_ice
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                            :: vol_snow
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                            :: u_ice
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                            :: v_ice
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                            :: a_ice
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                            :: zs1  !! Diagonal stress tensor components zs1 and zs2
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                            :: zs2
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                            :: zs12 !! Non-diagonal stress tensor component zs12

real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                            :: &
         zf1, zf2                      !: arrays for internal stresses

real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                            :: &
         zpresh        ,             & !: temporary array for ice strength
         zpreshc       ,             & !: Ice strength on grid cell corners (zpreshc)
         zfrld1, zfrld2,             & !: lead fraction on U/V points                                    
         zmass1, zmass2,             & !: ice/snow mass on U/V points                                    
         za1ct, za2ct  ,             & !: temporary arrays
         zc1                           !: ice mass

!
! Local variables
!
    integer            :: ic
    integer            :: icxy
    integer            :: kfd
    integer            :: mf
    integer            :: ml
    integer            :: n
    integer            :: nm
    integer            :: nmd, nmu, ndm, num, numd, ndmu, numu
    integer            :: nmin
    integer            :: nmf
    integer            :: nmfu
    integer            :: nml
    integer            :: nmlu
    real(fp)           :: tm
    real(fp) :: strength               ! now scalar variable (by replacinf ZPRESH computation)
    real(fp) :: &
         zt11, zt12, zt21, zt22,     & !: temporary scalars
         ztagnx, ztagny,             & !: wind stress on U/V points                       
         delta,                      & !
         deltat,                     & !: Delta at centre of grid cells
         deltac,                     & !: Delta on corners
         zdd, zdt,                   & !: Divergence and tension at centre of grid cells
         zds,                        & !: Shear on northeast corner of grid cells
         zusw                          !: temporary weight for the computation of ice strength

    real(fp) :: &
         za,                         & !:
         zstms,                      & !: temporary scalar for ice strength
         zsang,                      & !: temporary scalar for coriolis term
         zmask                         !: mask for the computation of ice mass

    real(fp) :: &
         ecc2,                       & !: square of yield ellipse eccenticity
         z0,                         & !: temporary scalar
         zr,                         & !: temporary scalar
         zcca, zccb,                 & !: temporary scalars
         zuuu_ice,                   & !: 
         zvvv_ice,                   & !:
         zddc, zdtc,                 & !: temporary array for delta on corners
         zdst,                       & !: temporary array for delta on centre
         zdsshx, zdsshy,             & !: term for the gradient of ocean surface
         sigma1, sigma2                !: internal ice stress

    real(fp) :: &
         zresm            ,          & !: Maximal error on ice velocity
         zindb            ,          & !: ice (1) or not (0)      
         zdummy                        !: dummy argument
    real(fp)           :: rzero, rone
    real(fp)           :: alphaevp, creepl, c_rhg, pstar, pstarh, rhoco 
    real(fp)           :: angvg, cangvg, cw, dtotel, ecc, rad, sangvg, telast, usecc2, zusden
    real(fp)           :: wl_d, wl_u
    logical :: prints
!
!! executable statements -------------------------------------------------------
!
    eps         => gdp%gdconst%eps
    cp_ice      => gdp%gdice%cp_ice
    cp_snow     => gdp%gdice%cp_snow
    k_ice       => gdp%gdice%k_ice
    k_snow      => gdp%gdice%k_snow
    lh_ice      => gdp%gdice%lh_ice
    lh_snow     => gdp%gdice%lh_snow
    lice        => gdp%gdice%lice
    lsnow       => gdp%gdice%lsnow
    lundia      => gdp%gdinout%lundia
    rho_ice     => gdp%gdice%rho_ice
    rho_snow    => gdp%gdice%rho_snow
    timjan      => gdp%gdheat%timjan
    timhr       => gdp%gdinttim%timhr
    tair        => gdp%gdheat%tair
    ag          => gdp%gdphysco%ag
    rhow        => gdp%gdphysco%rhow
    !
    ! DEFINITIONS:
    !
    !  zfrld1: lead fraction on U-points                                      
    !  zfrld2: lead fraction on V-points                                     
    !  zmass1: ice/snow mass on U-points                                    
    !  zmass2: ice/snow mass on V-points                                   
    !  (ztagnx,ztagny): wind stress on U/V points                       
    !
    ! INITIALISATION
    !
    prints = .false.
    icxy = max(icx, icy)
    !
    rzero  = 0.0_fp
    rone   = 1.0_fp
    !
    cw     = 5.0e-3_fp          ! drag coefficient for oceanic stress
    c_rhg  = 20.0_fp            ! determines changes in ice strength
    pstar  = 1.0e+4_fp          ! determines ice strength (N/M), Hibler JPO79
    ecc    = 2.0_fp             ! eccentricity of the elliptical yield curve
    usecc2 = 1.0 / ( ecc * ecc )
    rhoco  = rhow * cw          ! RHOW = TAU0 in LIM3
    angvg  = 0.0_fp             ! turning angle for oceanic stress 
    rad    = 2.0_fp * 3.14159_fp / 360.0_fp 
    angvg  = angvg * rad
    sangvg = SIN(angvg )        ! sin of the turning angle for ocean stress
    cangvg = COS(angvg )        ! cos of the turning angle for ocean stress
    pstarh = pstar / 2.0
    telast = 2880.0_fp          ! is not used (ASK LIM3 team)
    creepl = 2.0e-8_fp          ! creep limit
    alphaevp = 1.0_fp           ! coefficient of the internal stresses
    !
    if (prints) then
       write(lundia,*)
       write(lundia,*) 'lim_dyn_init: ice parameters for ice dynamics '
       write(lundia,*) '~~~~~~~~~~~~'
       write(lundia,*) '   drag coefficient for oceanic stress              cw     = ', cw
       write(lundia,*) '   turning angle for oceanic stress                 angvg  = ', angvg
       write(lundia,*) '   first bulk-rheology parameter                    pstar  = ', pstar
       write(lundia,*) '   second bulk-rhelogy parameter                    c_rhg  = ', c_rhg
       write(lundia,*) '   creep limit                                      creepl = ', creepl
       write(lundia,*) '   eccentricity of the elliptical yield curve       ecc    = ', ecc
       write(lundia,*) '   coefficient for the solution of int. stresses  alphaevp = ', alphaevp
    endif
    !
    !------------------------------------------------------------------------------!
    ! 0) Initialize ice status arrays; see routine LIMMSH
    !------------------------------------------------------------------------------!
    !
    wght = 0.0_fp
    vol_ice = 0.0_fp
    vol_snow = 0.0_fp
    !
    do nm = 1, nmmax
       nmd = nm - icx
       ndm = nm - icy
       if (kcs(nm)*kfs(nm) == 1 .and. kfsice(nm) == 1)  then
          !
          ! ==  metric coefficients for sea ice dynamic  ==
          !
          zusden = 1.e0 / (  ( gvz(nm) + gvz(nmd) )   &
             &             * ( guz(nm) + guz(ndm) ) )
          wght(nm,1,1) = zusden * gvz(nm ) * guz(nm )
          wght(nm,1,2) = zusden * gvz(nm ) * guz(ndm)
          wght(nm,2,1) = zusden * gvz(nmd) * guz(nm )
          wght(nm,2,2) = zusden * gvz(nmd) * guz(ndm)
          !
          ! compute volume of ice per unit area
          !
          vol_ice(nm)  =  h_ice(nm)  * a_ice(nm)
          vol_snow(nm) =  h_snow(nm) * a_ice(nm)
       endif
    enddo
    !
    !------------------------------------------------------------------------------!
    ! 1) Ice-Snow mass (zc1), ice strength (zpresh)
    !------------------------------------------------------------------------------!
    !
    ! Initialize work arrays
    !
    zpresh      = 0.0_fp 
    zpreshc     = 0.0_fp
    uuu         = 0.0_fp
    vvv         = 0.0_fp
    uuu_ice     = 0.0_fp
    vvv_ice     = 0.0_fp
    zfrld1      = 0.0_fp
    zfrld2      = 0.0_fp
    zmass1      = 0.0_fp
    zmass2      = 0.0_fp
    za1ct       = 0.0_fp
    za2ct       = 0.0_fp
    zf1         = 0.0_fp
    zf2         = 0.0_fp
    !
    !------------------------------------------------------------------------------!
    ! Ice strength on T-points based on Hibler (1979)' method
    !------------------------------------------------------------------------------!
    !
    do nm = 1, nmmax
       if (kcs(nm)*kfs(nm) == 1 .and. kfsice(nm) == 1)  then
          strength = pstar*vol_ice(nm)*exp(-C_rhg*(1.0-a_ice(nm)))
          zpresh(nm) = kfsice(nm) *  strength / 2.
       endif
    enddo
    !
    ! Ice mass and temp variables
    !
    do nm = 1, nmmax
       if (kcs(nm)*kfs(nm) == 1 .and. kfsice(nm) == 1)  then
          zc1(nm)   = kfsice(nm) * ( rho_snow * vol_snow(nm) + rho_ice * vol_ice(nm) )
          ! tmi = 1 where there is ice or on land
          ! not needed, because KFSICE already exists (NB. What's the difference between TMI and TMS?)
          !tmi(nm)    = 1.0 - ( 1.0 - MAX( 0.0 , SIGN ( 1.0 , vol_ice(nm) - &
          !           & eps ) ) ) * kfsice(nm)
       endif
    enddo
    !
    ! Ice strength on grid cell corners (zpreshc)
    ! needed for calculation of shear stress 
    !
    do nm = 1, nmmax
       nmu  = nm + icx
       num  = nm + icy
       numu = nm + icx + icy
       if (kcs(nm)*kfs(nm) == 1 .and. kfsice(nm) == 1)  then
            zstms          =  kfs(numu) * gsqs(numu) + &
               &              kfs(num)  * gsqs(num ) + &
               &              kfs(nmu)  * gsqs(nmu ) + &
               &              kfs(nm)   * gsqs(nm  )
            zusw        = 1.0 / MAX( zstms, eps )
            zpreshc(nm) = (  zpresh(numu) * gsqs(numu) + &
               &             zpresh(num)  * gsqs(num ) + &
               &             zpresh(nmu)  * gsqs(nmu ) + & 
               &             zpresh(nm)   * gsqs(nm  )   &
               &             ) * zusw
       endif
    enddo
    !
    !------------------------------------------------------------------------------!
    ! 2) Wind / ocean stress, mass terms, coriolis terms
    !  Wind stress, coriolis and mass terms on the sides of the squares  
    !------------------------------------------------------------------------------!
    !
   do nm = 1, nmmax
       if (kcs(nm)*kfs(nm) == 1)  then
          nmd  = nm - icx
          nmu  = nm + icx
          ndm  = nm - icy
          num  = nm + icy
          numd = nm - icx + icy
          ndmu = nm + icx - icy
          zt11 = kfs(nm)*gvz(nm)
          zt12 = kfs(nmu)*gvz(nmu)
          zt21 = kfs(nm)*guz(nm)
          zt22 = kfs(num)*guz(num)
          !
          ! Leads area.
          zfrld1(nm) = ( zt12 * ( 1.0 - a_ice(nm) ) + &
             &                        zt11 * ( 1.0 - a_ice(nmu) ) ) / ( zt11 + zt12 + eps )
          zfrld2(nm) = ( zt22 * ( 1.0 - a_ice(nm) ) + &
             &                        zt21 * ( 1.0 - a_ice(num) ) ) / ( zt21 + zt22 + eps )

          ! Mass, coriolis coeff. and currents
          zmass1(nm) = ( zt12*zc1(nm) + zt11*zc1(nmu) ) / (zt11+zt12+eps)
          zmass2(nm) = ( zt22*zc1(nm) + zt21*zc1(num) ) / (zt21+zt22+eps)
          !
          ! Ocean has no slip boundary condition
          vvv(nm)  = 0.5*( (v1(nm,1)+v1(ndm,1)) * gvz(nm)    &
             &                 +(v1(nmu,1)+v1(ndmu,1)) * gvz(nmu)) &
             &               /(gvz(nmu)+gvz(nm)) * kfu(nm)  

          uuu(nm)  = 0.5*((u1(nm,1)+u1(nmd,1)) * guz(nm)     &
             &                 +(u1(num,1)+u1(numd,1)) * guz(num)) &
             &                / (guz(num)+guz(nm)) * kfv(nm)

          ! Wind stress at U,V-point
          ztagnx = ( 1. - zfrld1(nm) ) * ut_ice(nm)
          ztagny = ( 1. - zfrld2(nm) ) * vt_ice(nm)

          ! Computation of the velocity field taking into account the ice internal interaction.
          ! Terms that are independent of the velocity field.

          wl_u = s1(nmu) + pship(nmu) / (ag * rhow)
          wl_d = s1(nm)  + pship(nm)  / (ag * rhow)
          zdsshx =  (wl_u - wl_d) / gvu(nm)
          wl_u = s1(num) + pship(num) / (ag * rhow)
          zdsshy =  (wl_u - wl_d) / guv(nm)

          za1ct(nm) = ztagnx - zmass1(nm) * ag * zdsshx
          za2ct(nm) = ztagny - zmass2(nm) * ag * zdsshy
       endif
    enddo
    !
    !------------------------------------------------------------------------------!
    ! 3) Solution of the momentum equation (NO iterative procedure compared to LIM3)
    !------------------------------------------------------------------------------!
    !
    !! dtevp  = rdt_ice / nevp   ! HDT is used (ASK LIM3 TEAM)
    dtotel = hdt / ( 2.0 * telast )

    !-ecc2: square of yield ellipse eccenticrity (reminder: must become a namelist parameter)
    ecc2 = ecc*ecc
    !
    do nm = 1, nmmax
       if (kcs(nm)*kfs(nm) == 1 .and. kfsice(nm) == 1) then
          !  
          !- Divergence, tension and shear (Section a. Appendix B of Hunke & Dukowicz, 2002)
          !- zdd(:,:), zdt(:,:): divergence and tension at centre of grid cells
          !- zds(:,:): shear on northeast corner of grid cells
          !          !
          ndm  = nm - icy
          ndmu = nm + icx - icy
          nmd  = nm - icx
          nmu  = nm + icx
          num  = nm + icy
          numd = nm - icx + icy
          numu = nm + icx + icy
          !
          zdd = (   guu(nm) * u_ice(nm)  &
              &   - guu(nmd)* u_ice(nmd) &
              &   + gvv(nm) * v_ice(nm)  &
              &   -gvv(ndm) * v_ice(ndm) &
              & ) / gsqs(nm)

          zdt = (  ( u_ice(nm) / guu(nm)   &
              &     -u_ice(nmd) / guu(nmd) &
              &    ) * guz(nm) * guz(nm)   &
              &   -( v_ice(nm) / gvv(nm)   &
              &     -v_ice(ndm) / gvv(ndm) &
              &    ) * gvz(nm) * gvz(nm)   &
              &  ) / gsqs(nm)
          !
          kfd = kfs(nm) * kfs(nmu) * kfs(num) * kfs(numu)
          !
          if (kfd .eq. 1) then
             zds = (  ( u_ice(num) / gvu(num) &
                 &     -u_ice(nm) / gvu(nm)   &
                 &    ) * gvd(nm) * gvd(nm)   &
                 &   +( v_ice(nmu) / guv(nmu) &
                 &     -v_ice(nm) / guv(nm)   &
                 &     ) * gud(nm) * gud(nm)  &
                 &  ) &
                 &  / ( gvd(nm) * gud(nm) ) * ( 2.0 - kfd ) &
                 &    * kfsice(nm) * kfsice(num) &
                 &    * kfsice(nmu) * kfsice(numu)
          else
             zds = 0.0_fp
          endif
          !   
          vvv_ice(nm) = 0.5*( ( v_ice(nm)+v_ice(ndm))*gvz(nmu)   &
                  &           +(v_ice(nmu)+v_ice(ndmu))*gvz(nm)) &
                  &         / ( gvz(nmu)+gvz(nm) ) * kfu(nm) 

          uuu_ice(nm) = 0.5*( ( u_ice(nm)+u_ice(nmd))*guz(num)   &
                  &           +(u_ice(num)+u_ice(numd))*guz(nm)) &
                  &         / ( guz(num)+guz(nm) ) * kfv(nm)
          !        
          !- Calculate Delta at centre of grid cells
          zdst = (   guu(nm ) * vvv_ice(nm)  &
             &     - guu(nmd) * vvv_ice(nmd) &
             &     + gvv(nm ) * uuu_ice(nm)  &
             &     - gvv(ndm) * uuu_ice(ndm) &
             &   ) / gsqs(nm)
          !
          delta  = SQRT( zdd*zdd + (zdt*zdt + zdst*zdst) * usecc2 )  
          deltat = MAX( SQRT(zdd**2 + (zdt**2 + zdst**2)*usecc2), creepl )
          !
          !-Calculate stress tensor components zs1 and zs2 
          !-at centre of grid cells (see section 3.5 of CICE user's guide)
          !
          zs1(nm) = ( zs1(nm) &
             &          - dtotel*( ( 1.0 - alphaevp) * zs1(nm) +    &
             &            ( delta / deltat - zdd / deltat ) &
             &      * zpresh(nm) ) )                          &       
             &        / ( 1.0 + alphaevp * dtotel )
          !
          zs2(nm) = ( zs2(nm)   &
             &          - dtotel*((1.0-alphaevp)*ecc2*zs2(nm) -  &
             &          zdt/deltat*zpresh(nm)) ) &
             &        / ( 1.0 + alphaevp*ecc2*dtotel )
          !
          !- Calculate Delta on corners
          !
          if (kfd .eq. 1) then
             zddc = ( ( vvv_ice(num) / gvu(num) &
                &      -vvv_ice(nm) / gvu(nm)   &
                &     ) * gvd(nm) * gvd(nm)     &
                &    +( uuu_ice(nmu) / guv(nmu) &
                &      -uuu_ice(nm) / guv(nm)   &
                &      ) * gud(nm )* gud(nm)    &
                &    ) / ( gvd(nm) * gud(nm) )
             !
             zdtc = (-( vvv_ice(num) / gvu(num) &
                &      -vvv_ice(nm) / gvu(nm)   &
                &      ) * gvd(nm) * gvd(nm)    &
                &    +( uuu_ice(nmu) / guv(nmu) &
                &      -uuu_ice(nm) / guv(nm)   &
                &     ) * gud(nm) * gud(nm)     &
                &   ) / ( gvd(nm) * gud(nm) )
             !
             deltac   = SQRT(zddc**2+(zdtc**2+zds**2)*usecc2) + creepl
             !
             !-Calculate stress tensor component zs12 at corners (see section 3.5 of CICE user's guide).
             zs12(nm) = ( zs12(nm)      &
                &        - dtotel*( (1.0-alphaevp)*ecc2*zs12(nm) - zds / &
                &          ( 2.0*deltac ) * zpreshc(nm))) &
                &         / ( 1.0 + alphaevp*ecc2*dtotel ) 
          else
             zs12(nm) = 0.0_fp
          endif         
       else
          zs1(nm)  = 0.0_fp
          zs2(nm)  = 0.0_fp
          zs12(nm) = 0.0_fp
       endif
    enddo
    !
    do nm = 1, nmmax
       if (kcs(nm)*kfs(nm) == 1 .and. kfsice(nm) == 1)  then
          nmd = nm - icx
          nmu = nm + icx
          ndm = nm - icy
          num = nm + icy
          !
          ! Ice internal stresses (Appendix C of Hunke and Dukowicz, 2002)
          !- contribution of zs1, zs2 and zs12 to zf1
          zf1(nm) = 0.5*( (zs1(nmu)-zs1(nm))*guu(nm) &
             &              +(zs2(nmu)*guz(nmu)**2-zs2(nm)*guz(nm)**2)/guu(nm) &
             &              +2.0*(zs12(nm)*gvd(nm)**2-zs12(ndm)*gvd(ndm)**2)/gvu(nm) &
             &             ) / ( gvu(nm)*guu(nm) )
          !
          ! contribution of zs1, zs2 and zs12 to zf2
          zf2(nm) = 0.5*( (zs1(num)-zs1(nm))*gvv(nm) &
             &              -(zs2(num)*gvz(num)**2 - zs2(nm)*gvz(nm)**2)/gvv(nm) &
             &              + 2.0*(zs12(nm)*gud(nm)**2-zs12(nmd)*gud(nmd)**2)/guv(nm) &
             &             ) / ( gvv(nm)*guv(nm) )
       endif
    enddo
    !
    ! Computation of ice velocity
    !
    ! Both the Coriolis term and the ice-ocean drag are solved semi-implicitly.
    !
    do nm = 1, nmmax
       if (kcs(nm)*kfs(nm) == 1 .and. kfsice(nm) == 1) then
          nmu  = nm + icx
          ndmu = nm + icx - icy
          num  = nm + icy
          zmask        = (1.0-MAX(rzero,SIGN(rone,-zmass1(nm))))*kfu(nm)
          zsang        = SIGN ( 1.0 , fcorio(nm) ) * sangvg
          z0           = zmass1(nm)/hdt
          ! SB modif because ocean has no slip boundary condition
          zvvv_ice       = 0.5*( (v_ice(nm)+v_ice(ndm))*gvz(nm)         &
             &                 +(v_ice(nmu)+v_ice(ndmu))*gvz(nmu))   &
             &               /(gvz(nmu)+gvz(nm)) * kfu(nm)
          za           = rhoco*SQRT((u_ice(nm)-u1(nm,1))**2 + &
             (zvvv_ice-vvv(nm))**2) * (1.0-zfrld1(nm))
          zr           = z0*u_ice(nm) + zf1(nm) + za1ct(nm) + &
             za*(cangvg*u1(nm,1)-zsang*vvv(nm))
          zcca         = z0+za*cangvg
          zccb         = fcorio(nm)+za*zsang
          u_ice(nm) = (zr+zccb*zvvv_ice)/(zcca+eps)*zmask 
       endif
    enddo
    !
    do nm = 1, nmmax
       nmd  = nm - icx
       num  = nm + icy
       numd = nm + icy - icx
       if (kcs(nm)*kfs(nm) == 1 .and. kfsice(nm) == 1) then
          zmask        = (1.0-MAX(rzero,SIGN(rone,-zmass2(nm))))*kfv(nm)
          zsang        = SIGN(1.0,fcorio(nm))*sangvg
          z0           = zmass2(nm)/hdt
          ! SB modif because ocean has no slip boundary condition
          zuuu_ice       = 0.5*( (u_ice(nm)+u_ice(nmd))*guz(nm)     &
             &                 + (u_ice(num)+u_ice(numd))*guz(num))   &
             &               /(guz(num)+guz(nm)) * kfv(nm)
          za           = rhoco*SQRT((zuuu_ice-uuu(nm))**2 + & 
             (v_ice(nm)-v1(nm,1))**2)*(1.0-zfrld2(nm))
          zr           = z0*v_ice(nm) + zf2(nm) + &
             za2ct(nm) + za*(cangvg*v1(nm,1)+zsang*uuu(nm))
          zcca         = z0+za*cangvg
          zccb         = fcorio(nm)+za*zsang
          v_ice(nm) = (zr-zccb*zuuu_ice)/(zcca+eps)*zmask
       endif
    enddo
    !
    !------------------------------------------------------------------------------!
    ! 4) Prevent ice velocities when the ice is thin
    !------------------------------------------------------------------------------!
    !
    ! If the ice thickness is below 5 cm then ice velocity should equal the
    ! ocean velocity, 
    ! This prevents high velocity when ice is thin
    !
    do nm = 1, nmmax
       if (kcs(nm)*kfs(nm) == 1 .and. kfsice(nm) == 1) then
          zindb  = MAX( 0.0, SIGN( 1.0, a_ice(nm) - 1.0e-6 ) ) 
          zdummy = zindb * vol_ice(nm) / MAX(a_ice(nm) , 1.0e-06 )
          if ( zdummy .le. 5.0e-2 ) then
             u_ice(nm) = u1(nm,1)
             v_ice(nm) = v1(nm,1)
          endif
       endif
    enddo
    !
    !------------------------------------------------------------------------------!
    ! 4b) Avoid large ice currents
    !------------------------------------------------------------------------------!
    !
    do nm = 1, nmmax
       if (kfs(nm) == 1) then
          u_ice(nm) = min( 5.0_fp, u_ice(nm))
          u_ice(nm) = max(-5.0_fp, u_ice(nm))
          v_ice(nm) = min( 5.0_fp, v_ice(nm))
          v_ice(nm) = max(-5.0_fp, v_ice(nm))
       endif
    enddo      
    !    
    !------------------------------------------------------------------------------!
    ! 5) Compute ice velocity at borders
    !------------------------------------------------------------------------------!
    !
    do nm = 1, nmmax
       nmu = nm + icx
       num = nm + icy
       if (kfsice(nm) + kfsice(nmu) == 1) then
          u_ice(nm) = u1(nm,1)
       endif
       if (kfsice(nm) + kfsice(num) == 1) then
          v_ice(nm) = v1(nm,1)
       endif
    enddo  
    !
end subroutine limrhg
