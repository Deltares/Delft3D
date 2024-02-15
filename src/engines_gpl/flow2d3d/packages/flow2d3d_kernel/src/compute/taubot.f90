subroutine taubot(j         ,nmmaxj    ,nmmax     ,kmax      ,icx       , &
                & icy       ,rouflo    ,rouwav    ,kfu       ,kfv       , &
                & kfumin    ,kfumax    ,kspu      ,kcs       ,kcscut    , &
                & dps       ,s1        ,u1        ,v1        , &
                & guu       ,xcor      ,ycor      ,rho       , &
                & taubpu    ,taubsu    ,taubxu    ,dis       ,rlabda    , &
                & teta      ,uorb      ,tp        ,wsu       ,wsv       , &
                & grmasu    ,dfu       ,deltau    ,hrms      , &
                & cfurou    ,z0urou    ,hu        ,dzu1      ,sig       , &
                & z0ucur    ,cvalu0    ,grmsur    ,grfacu    ,gdp       )
!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2024.                                
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
!    Function: Computation coefficients bottom stresses in U-
!              points. Wave stresses may be taken into account.
!              For KMAX>1 a logarithmic velocity profile is
!              assumed. If (IRO>1) then the bottom is assumed to
!              be rough. Otherwise it is assumed to behave as a
!              smooth surface. For a rough bottom the bottom
!              stress is dependent of the coeff. stored in Z0UROU
!              and Z0VROU. Bottom assumed at z0.
!              The coefficients are stored in the arrays TAUBPU
!              and TAUBSU (TAUBOT = TAUBPU*U + TAUBSU).
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use mathconsts, only: pi, ee, degrad
    !
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    real(fp)               , pointer :: eps
    real(fp)               , pointer :: fmud
    real(fp)               , pointer :: taubng
    real(fp)               , pointer :: fwfac
    real(fp)               , pointer :: ftauw
    logical                , pointer :: cstbnd
    logical                , pointer :: chz_k2d
    logical                , pointer :: v2dwbl
    real(fp)               , pointer :: rhow
    real(fp)               , pointer :: ag
    real(fp)               , pointer :: z0
    real(fp)               , pointer :: vonkar
    logical                , pointer :: const
    logical                , pointer :: wave
    logical                , pointer :: mudlay
    logical                , pointer :: zmodel
    logical                , pointer :: roller
    integer                , pointer :: rolcorr
    real(fp), dimension(:) , pointer :: rksr
    real(fp), dimension(:) , pointer :: rksmr
!
! Global variables
!
    integer                                                         :: icx    !!  Increment in the X-dir., if ICX= NMAX
                                                                              !!  then computation proceeds in the X-
                                                                              !!  dir. If icx=1 then computation pro-
                                                                              !!  ceeds in the Y-dir.
    integer                                           , intent(in)  :: icy    !!  Increment in the Y-dir. (see ICX)
    integer                                                         :: j      !!  Begin pointer for arrays which have
                                                                              !!  been transformed into 1D arrays.
                                                                              !!  Due to the shift in the 2nd (M-)
                                                                              !!  index, J = -2*NMAX + 1
    integer                                                         :: kmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                         :: nmmax  !  Description and declaration in dimens.igs
    integer                                                         :: nmmaxj !  Description and declaration in dimens.igs
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: kcs    !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)                    :: kfu    !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: kfumax !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: kfumin !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: kfv    !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax)            :: kspu   !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub, kmax), intent(in)  :: kcscut !  Description and declaration in esm_alloc_int.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(out) :: cvalu0 !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                    :: deltau !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(out) :: dfu    !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub,4)                  :: dis    !  Description and declaration in esm_alloc_real.f90
    real(prec), dimension(gdp%d%nmlb:gdp%d%nmub)                    :: dps    !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: grmasu !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: grmsur !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: grfacu !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                    :: guu    !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: hrms   !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                    :: hu     !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                    :: rlabda !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                    :: s1     !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                    :: taubpu !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                    :: taubsu !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(out) :: taubxu !!  Primary maximal bottom stress   term
                                                                              !!  in the u-point (scalar)
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: teta   !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: tp     !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: uorb   !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                    :: wsu    !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                    :: wsv    !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                    :: xcor   !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                    :: ycor   !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                    :: z0ucur
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                    :: z0urou !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, 3)                 :: cfurou !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax), intent(in)  :: dzu1   !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)              :: rho    !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax), intent(in)  :: u1     !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax), intent(in)  :: v1     !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(kmax)                       , intent(in)  :: sig    !  Description and declaration in esm_alloc_real.f90
    character(4)                                      , intent(in)  :: rouflo !  Description and declaration in esm_alloc_char.f90
    character(4)                                      , intent(in)  :: rouwav !  Description and declaration in tricom.igs
!
! Local variables
!
    integer                              :: ierror
    integer                              :: k
    integer                              :: kmaxx
    integer  , dimension(:), allocatable :: kmaxxx
    integer                              :: modind     ! Index of friction model (1=FR84, 2=MS90, 3=HT91, 4=GM79, 5=DS88, 6=BK67) 
    integer                              :: ndm
    integer                              :: ndmu
    integer                              :: nm
    integer                              :: nmu
    integer                              :: svvv
    logical                              :: actual_avg
    logical                              :: kcscuttest
    real(fp)                             :: a
    real(fp)                             :: abscos
    real(fp)                             :: alfaw
    real(fp)                             :: astar      ! Coefficient in expression for wave friction factor 
    real(fp)                             :: astarc     ! 30 pi (critical value for astar) 
    real(fp)                             :: ca         ! Appararent Chezy value (Van Rijn, 2004) 
    real(fp)                             :: cdrag      ! Drag coefficient 
    real(fp)                             :: ci         ! Cos(phi)^I 
    real(fp)                             :: cj         ! Cos(phi)^J 
    real(fp)                             :: coeffa     ! Coefficient a in expression for parametrized models 
    real(fp)                             :: coeffb     ! Coefficient b in expression for parametrized models 
    real(fp)                             :: coeffm     ! Coefficient m in expression for parametrized models 
    real(fp)                             :: coeffn     ! Coefficient n in expression for parametrized models 
    real(fp)                             :: coeffp     ! Coefficient p in expression for parametrized models 
    real(fp)                             :: coeffq     ! Coefficient q in expression for parametrized models 
    real(fp)                             :: costu
    real(fp)                             :: cu
    real(fp)                             :: cvalue
    real(fp)                             :: cwall
    real(fp)                             :: dz         ! Distance of the bottom layer from the wall, used to calculate bottom stress vel. 
    real(fp)                             :: fmrat
    real(fp)                             :: fw         ! Wave friction factor 
    real(fp)                             :: gamma
    real(fp)                             :: hrmsu
    real(fp)                             :: hurou
    real(fp)                             :: ks
    real(fp)                             :: ksc
    real(fp)                             :: lfc        ! Log(fw/cd)
    real(fp)                             :: omega
    real(fp)                             :: phi        ! Angle between wave and current direction (Van Rijn, 2004)
    real(fp)                             :: rksru
    real(fp)                             :: rksmru
    real(fp)                             :: rz
    real(fp)                             :: sag
    real(fp)                             :: sintu
    real(fp)                             :: sixth
    real(fp)                             :: su
    real(fp)                             :: taucur     ! Bottom friction due to current alone 
    real(fp)                             :: tauwav     ! Bottom friction due to waves alone 
    real(fp)                             :: tauwci     ! Bottom friction for combined waves and current 
    real(fp)                             :: tpu
    real(fp)                             :: u2dh
    real(fp)                             :: uratio    
    real(fp)                             :: ubot
    real(fp)                             :: uc
    real(fp)                             :: umod
    real(fp)                             :: umodsq     ! Magnitude of velocity squared 
    real(fp)                             :: uorbhs
    real(fp)                             :: uorbu
    real(fp)                             :: ust
    real(fp)                             :: rr
    real(fp)                             :: umax
    real(fp)                             :: t1
    real(fp)                             :: rlabdau
    real(fp)                             :: u11
    real(fp)                             :: a11
    real(fp)                             :: raih
    real(fp)                             :: rmax
    real(fp)                             :: uon
    real(fp)                             :: uoff
    real(fp)                             :: uwbih 
    real(fp)                             :: ustokes
    real(fp)                             :: uuu
    real(fp)                             :: vvv
    real(fp)                             :: waveps     ! Small positive number 
    real(fp)                             :: xpar       ! Variable x in in expression for parametrized models 
    real(fp)                             :: ymxpar     ! Variable Y(max) in in expression for parametrized models 
    real(fp)                             :: ypar       ! Variable y in in expression for parametrized models 
    real(fp), dimension(8)               :: coeffi     ! Coefficient i in expression for parametrized models 
    real(fp), dimension(8)               :: coeffj     ! Coefficient j in expression for parametrized models 
    real(fp), dimension(8, 4)            :: aa         ! Coefficient a(i) in expression for parameter a 
    real(fp), dimension(8, 4)            :: bb         ! Coefficient b(i) in expression for parameter b 
    real(fp), dimension(8, 4)            :: mm         ! Coefficient m(i) in expression for parameter n 
    real(fp), dimension(8, 4)            :: nn         ! Coefficient n(i) in expression for parameter n 
    real(fp), dimension(8, 4)            :: pp         ! Coefficient p(i) in expression for parameter p 
    real(fp), dimension(8, 4)            :: qq         ! Coefficient q(i) in expression for parameter q 
    real(fp), dimension(:),allocatable   :: ka         ! Apparent bed roughness (Van Rijn, 2004)
    real(fp)                             :: arg
    real(fp)                             :: hsu
    real(fp)                             :: awb
    real(fp)                             :: delw
    real(fp)                             :: zcc
    real(fp)                             :: uuu0
    real(fp)                             :: vvv0
    real(fp)                             :: umod0
!
! Data statements
!
    data bb/      0.29,  0.65,  0.27,  0.73,  0.22,  0.32,  0.47, -0.06, & 
                  0.55,  0.29,  0.51,  0.40,  0.73,  0.55,  0.29,  0.26, &
                 -0.10, -0.30, -0.10, -0.23, -0.05,  0.00, -0.09,  0.08, &
                 -0.14, -0.21, -0.24, -0.24, -0.35,  0.00, -0.12, -0.03/
    !
    data pp/     -0.77, -0.60, -0.75, -0.68, -0.86, -0.63, -0.70, -1.00, &
                  0.10,  0.10,  0.13,  0.13,  0.26,  0.05,  0.13,  0.31, &
                  0.27,  0.27,  0.12,  0.24,  0.34,  0.00,  0.28,  0.25, &
                  0.14, -0.06,  0.02, -0.07, -0.07,  0.00, -0.04, -0.26/
    !
    data qq/      0.91,  1.19,  0.89,  1.04, -0.89,  1.14,  1.65,  0.38, &
                  0.25, -0.68,  0.40, -0.56,  2.33,  0.18, -1.19,  1.19, &
                  0.50,  0.22,  0.50,  0.34,  2.60,  0.00, -0.42,  0.25, &
                  0.45, -0.21, -0.28, -0.27, -2.50,  0.00,  0.49, -0.66/
    !
    data coeffj/  3.00,  0.50,  2.70,  0.50,  2.70,  3.00,  0.60,  1.50/
    !
    !-----for Tau_max
    data aa/     -0.06, -0.01, -0.07,  0.11,  0.05,  0.00, -0.01, -0.45, &
                  1.70,  1.84,  1.87,  1.95,  1.62,  2.00,  1.58,  2.24, &
                 -0.29, -0.58, -0.34, -0.49, -0.38,  0.00, -0.52,  0.16, &
                  0.29, -0.22, -0.12, -0.28,  0.25,  0.00,  0.09, -0.09/
    !
    data mm/      0.67,  0.63,  0.72,  0.65,  1.05,  0.00,  0.65,  0.71, &
                 -0.29, -0.09, -0.33, -0.22, -0.75,  0.50, -0.17,  0.27, &
                  0.09,  0.23,  0.08,  0.15, -0.08,  0.00,  0.18, -0.15, &
                  0.42, -0.02,  0.34,  0.06,  0.59,  0.00,  0.05,  0.03/
    !
    data nn/      0.75,  0.82,  0.78,  0.71,  0.66,  0.00,  0.47,  1.19, &
                 -0.27, -0.30, -0.23, -0.19, -0.25,  0.50, -0.03, -0.66, &
                  0.11,  0.19,  0.12,  0.17,  0.19,  0.00,  0.59, -0.13, &
                 -0.02, -0.21, -0.12, -0.15, -0.03,  0.00, -0.50,  0.12/
    !
    data coeffi/  0.80,  0.67,  0.82,  0.67,  0.82,  1.00,  0.64,  0.77/
!
!! executable statements -------------------------------------------------------
!
    eps        => gdp%gdconst%eps
    fmud       => gdp%gdmudcoe%fmud
    taubng     => gdp%gdmudcoe%taubng
    fwfac      => gdp%gdnumeco%fwfac
    ftauw      => gdp%gdnumeco%ftauw
    cstbnd     => gdp%gdnumeco%cstbnd
    v2dwbl     => gdp%gdnumeco%v2dwbl
    chz_k2d    => gdp%gdrivpro%chz_k2d
    rhow       => gdp%gdphysco%rhow
    ag         => gdp%gdphysco%ag
    z0         => gdp%gdphysco%z0
    vonkar     => gdp%gdphysco%vonkar
    const      => gdp%gdprocs%const
    wave       => gdp%gdprocs%wave
    mudlay     => gdp%gdprocs%mudlay
    zmodel     => gdp%gdprocs%zmodel
    roller     => gdp%gdprocs%roller
    rolcorr    => gdp%gdbetaro%rolcorr
    rksr       => gdp%gdbedformpar%rksr
    rksmr      => gdp%gdbedformpar%rksmr
    !
    ! INITIALISATION
    !
    allocate(ka(nmmax), stat = ierror)
    allocate(kmaxxx(gdp%d%nmlb:gdp%d%nmub), stat = ierror)
    !
    alfaw  = 20.
    sixth  = 1./6.
    sag    = sqrt(ag)
    astarc = 30.*pi**2
    waveps = 1.E-4
    !
    taubsu = 0.0
    do nm = 1, nmmax
       hu(nm)     = max(hu(nm), 0.01_fp)
    enddo
    !
    ! First part only for sigma layer model
    !
    if (.not.zmodel) then
       if (mudlay) then
          fmrat = taubng/(rhow*fmud)
          !
          ! For mud layer, Bingham yield stress added
          !
          ndm  = -icy
          nmu  =  icx
          ndmu =  icx - icy
          do nm = 1, nmmax
             nmu  = nmu  + 1
             ndm  = ndm  + 1
             ndmu = ndmu + 1
             if (kfu(nm)==1) then
                uuu = u1(nm, kmax)
                vvv = 0.25*(v1(nm, kmax) + v1(ndm, kmax) + v1(ndmu, kmax)       &
                    & + v1(nmu, kmax))
                umod = sqrt(uuu*uuu + vvv*vvv)
                if (umod<0.01) then
                   taubpu(nm) = fmud*umod*(1. + fmrat/0.0001)/8.
                else
                   taubpu(nm) = fmud*umod*(1. + fmrat/umod**2)/8.
                endif
             endif
          enddo
          !
          ! Taubot is finished for mudlayer
          !
          return
       endif
    endif
    !
    ! LOOP OVER GRID ROWS
    !   Z0UROU(NM)   will be filled with roughness height
    !   CFUROU(NM,1) will be filled with 2D chezy coefficient for
    !                KMAX = 1 (VIHSEC & SECRHS)
    !                and for KMAX > 1, at end with
    !                U/USTAR for KMAX>1 (INITUR,TRATUR&TURCLO)
    !
    !***COMPUTATION BOTTOM STRESS DUE TO FLOW
    !
    ndm  = -icy
    nmu  =  icx
    ndmu =  icx - icy
    do nm = 1, nmmax
       nmu  = nmu  + 1
       ndm  = ndm  + 1
       ndmu = ndmu + 1
       if (kfu(nm)==1) then
          !
          ! Avoid division by zero at tangential coupling points:
          ! (this might happen due to the parallel Linux implementation)
          !
          if (kcs(nm)==3 .and. cfurou(nm,2) < 1.0e-8 ) cfurou(nm,2) = 1.0
          !
          if (rouflo=='MANN') then
             !
             ! Manning's Formula
             !
             cfurou(nm, 1) = hu(nm)**sixth/cfurou(nm, 2)
          elseif (rouflo=='CHEZ') then
             !
             ! Chezy Formula, do nothing just copy the coefficients
             !
             cfurou(nm, 1) = cfurou(nm, 2)
          elseif (rouflo=='WHIT') then
             !
             ! Formula for White Colebrook, more consistent with 3D
             !
             if (chz_k2d) then
                hurou         = max(0.5_fp, hu(nm)/cfurou(nm, 2))
                cfurou(nm, 1) = 18.0_fp*log10(12.0_fp*hurou)
             else
                rz            = 1.0 + hu(nm)/(ee*cfurou(nm, 2)/30.)
                cfurou(nm, 1) = sag*log(rz)/vonkar
             endif
          elseif (rouflo=='Z   ') then
             !
             ! 3D
             !
             rz            = 1.0 + hu(nm)/(ee*cfurou(nm, 2))
             cfurou(nm, 1) = sag*log(rz)/vonkar
          else
          endif
          kmaxx      = kmax
          kcscuttest = .false.
          if (zmodel) then
             kmaxx      = kfumin(nm)
             kcscuttest = kcscut(nm, kmaxx)==1
          endif
          actual_avg =      (cstbnd .and. .not.zmodel .and. (kcs(nm)==2 .or. kcs(nmu)==2)) &
                     & .or. (kcs(nm)==3 .or. kcs(nmu)==3)                                  &
                     & .or. (zmodel .and. kcscuttest)
          uuu = u1(nm, kmaxx)
          if (actual_avg) then
             svvv = max(kfv(ndm) + kfv(ndmu) + kfv(nm) + kfv(nmu), 1)
             vvv  = (v1(ndm, kmaxx)*kfv(ndm) + v1(ndmu, kmaxx)*kfv(ndmu)         &
                  & + v1(nm, kmaxx)*kfv(nm) + v1(nmu, kmaxx)*kfv(nmu))/svvv
          else
             vvv = 0.25*(v1(nm, kmaxx) + v1(ndm, kmaxx) + v1(ndmu, kmaxx)       &
                 & + v1(nmu, kmaxx))
          endif
          umod       = sqrt(uuu*uuu + vvv*vvv)
          cvalue     = (1./cfurou(nm, 1))**2
          taubpu(nm) = cvalue*ag*umod
          taubxu(nm) = taubpu(nm)*umod*rhow
       endif
    enddo
    do nm = 1, nmmax
       !
       ! store chezy coefficients
       !
       cvalu0(nm) = cfurou(nm, 1)
       !
       ! store current-roughness values for use in EROSED
       !
       if (kfu(nm)==1) then
          z0ucur(nm) = hu(nm)/(ee*(exp(vonkar*cfurou(nm, 1)/sag) - 1.0))
       endif
    enddo
    !
    ! parameterized bottom friction models
    !
    if (rouwav=='FR84') then
       modind = 1
    elseif (rouwav=='MS90') then
       modind = 2
    elseif (rouwav=='HT91') then
       modind = 3
    elseif (rouwav=='GM79') then
       modind = 4
    elseif (rouwav=='DS88') then
       modind = 5
    elseif (rouwav=='BK67') then
       modind = 6
    elseif (rouwav=='CJ85') then
       modind = 7
    elseif (rouwav=='OY88') then
       modind = 8
    elseif (rouwav=='VR04') then
       modind = 9
    else
       modind = 0
       goto 999
    endif
    !
    ! initialize grid indices
    !
    ndm  = -icy
    nmu  =  icx
    ndmu =  icx - icy
    do nm = 1, nmmax
       nmu        = nmu  + 1
       ndm        = ndm  + 1
       ndmu       = ndmu + 1
       !
       kmaxx      = kmax
       !       
       if (v2dwbl .and. kmax>1 .and. .not.zmodel) then
          !
          ! Determine representative 2Dh velocity based on velocities in first layer above wave boundary layer 
          ! kmaxx is the first layer with its centre above the wave boundary layer
          ! Use deltau from previous time step !
          ! Only implemented for sigma layers
          !
          if (tpu>1.0) then
             do k = kmax, 1, -1
                zcc = (1.0 + sig(k))*hu(nm)
                if (zcc>deltau(nm) .or. zcc>0.5*hu(nm)) then
                   kmaxx = k
                   exit
                endif
             enddo
          endif
       endif
       !
       kmaxxx(nm) = kmaxx ! kmaxxx to be used in following loop where apparent roughness is determined
       !
       kcscuttest = .false.
       if (zmodel) then
          kmaxx      = kfumin(nm)
          kcscuttest = kcscut(nm, kmaxx)==1
       endif
       actual_avg =      (cstbnd .and. .not.zmodel .and. (kcs(nm)==2 .or. kcs(nmu)==2)) &
                  & .or. (kcs(nm)==3 .or. kcs(nmu)==3)                                  &
                  & .or. (zmodel .and. kcscuttest)
       uuu        = u1(nm, kmaxx)
       if (actual_avg) then
          svvv = max(kfv(ndm) + kfv(ndmu) + kfv(nm) + kfv(nmu), 1)
          vvv  = (v1(ndm, kmaxx)*kfv(ndm) + v1(ndmu, kmaxx)*kfv(ndmu)            &
               & + v1(nm, kmaxx)*kfv(nm) + v1(nmu, kmaxx)*kfv(nmu))/svvv
       else
          vvv = 0.25*(v1(nm, kmaxx) + v1(ndm, kmaxx) + v1(ndmu, kmaxx)          &
              & + v1(nmu, kmaxx))
       endif
       umodsq     = uuu*uuu + vvv*vvv
       umod       = max(1.0e-4_fp , sqrt(umodsq))
       !
       if (kmaxx/=kmax .and. .not.zmodel) then
          !
          ! Also compute current magnitude (umod0) in bottom layer, because it's needed to
          ! compute taubpu
          !
          uuu0       = u1(nm, kmax)
          if (actual_avg) then
             svvv = max(kfv(ndm) + kfv(ndmu) + kfv(nm) + kfv(nmu), 1)
             vvv0 = (v1(ndm, kmax)*kfv(ndm) + v1(ndmu, kmax)*kfv(ndmu)            &
                  & + v1(nm, kmax)*kfv(nm) + v1(nmu, kmax)*kfv(nmu))/svvv
          else
             vvv0 = 0.25*(v1(nm, kmax) + v1(ndm, kmax) + v1(ndmu, kmax)          &
                 & + v1(nmu, kmax))
          endif
          umodsq     = uuu0*uuu0 + vvv0*vvv0
          umod0      = max(1.0e-4_fp , sqrt(umodsq))
       else
          !
          ! kmaxx equals kmax
          !
          umod0 = umod
       endif
       !       
       taubpu(nm) = 0.0
       if (kfu(nm)==1) then
          !
          ! angle between waves and current
          !
          cu = 0.5*(cos(degrad*teta(nm)) + cos(degrad*teta(nmu)))
          su = 0.5*(sin(degrad*teta(nm)) + sin(degrad*teta(nmu)))
          !
          ! Need to check which direction
          !
          if (icx>1) then
             !
             ! computing in X direction
             !
             costu = cu
             sintu = su
          else
             !
             ! computing in Y direction
             !
             costu = su
             sintu = cu
          endif
          abscos = abs(uuu*costu + vvv*sintu)/(umod + waveps)
          !
          ! wave friction factor and drag coefficient
          !
          tpu    = 0.5*(tp(nm) + tp(nmu))
          uorbu  = 0.5*(uorb(nm) + uorb(nmu))
          uorbhs = sqrt(2.0)*uorbu
          astar  = tpu*uorbu/z0ucur(nm)
          if (astar>astarc) then
             fw = 0.00251*exp(14.1/(astar**0.19))
          else
             fw = 0.3
          endif
          !
          ! magnitude of bottom friction due to waves alone
          ! and due to current alone
          !
          tauwav = 0.5*rhow*fw*ftauw*uorbu**2
          if (kmax>1) then
             !
             ! depth-averaged current for current and waves
             !
             if (zmodel) then
                u2dh = (umod/hu(nm)                                             &
                     & *((hu(nm) + z0urou(nm))*log(1.0 + hu(nm)/z0urou(nm))     &
                     & - hu(nm)))/log(1.0 + 0.5*(max(dzu1(nm, kfumin(nm)),0.01_fp))/z0urou(nm))
             else
                u2dh = (umod/hu(nm)                                             &
                     & *((hu(nm) + z0urou(nm))*log(1.0 + hu(nm)/z0urou(nm))     &
                     & - hu(nm)))/log(1.0 + (1.0 + sig(kmaxx))*hu(nm)/z0urou(nm))
             endif
          else
             u2dh = umod
          endif
          taucur = rhow*ag*u2dh**2/(cfurou(nm, 1)**2)
          cdrag  = ag/(cfurou(nm, 1)**2)
          !
          ! parameterized models
          !
          if (tauwav<1.0E-8) then
             xpar   = 1.0
             ypar   = 1.0
             ymxpar = 1.0
          else
             xpar = taucur/(taucur + tauwav)
             if (xpar<1.0E-8 .or. modind==9) then
                ypar   = 0.0
                ymxpar = 1.0
             else
                lfc    = log10(fw/cdrag)
                !
                cj     = abscos**coeffj(modind)
                coeffb = (bb(modind, 1) + bb(modind, 2)*cj)                     &
                       & + (bb(modind, 3) + bb(modind, 4)*cj)*lfc
                coeffp = (pp(modind, 1) + pp(modind, 2)*cj)                     &
                       & + (pp(modind, 3) + pp(modind, 4)*cj)*lfc
                coeffq = (qq(modind, 1) + qq(modind, 2)*cj)                     &
                       & + (qq(modind, 3) + qq(modind, 4)*cj)*lfc
                ypar   = xpar*(1.0 + coeffb*(xpar**coeffp)*((1.0 - xpar)**coeffq))
                !
                ci     = abscos**coeffi(modind)
                coeffa = (aa(modind, 1) + aa(modind, 2)*ci)                     &
                       & + (aa(modind, 3) + aa(modind, 4)*ci)*lfc
                coeffm = (mm(modind, 1) + mm(modind, 2)*ci)                     &
                       & + (mm(modind, 3) + mm(modind, 4)*ci)*lfc
                coeffn = (nn(modind, 1) + nn(modind, 2)*ci)                     &
                       & + (nn(modind, 3) + nn(modind, 4)*ci)*lfc
                ymxpar = 1.0 + coeffa*(xpar**coeffm)*((1.0 - xpar)**coeffn)
             endif
          endif
          !
          ! bottom friction for combined waves and current
          !
          taubxu(nm) = ymxpar*(taucur + tauwav)
          !
          if (modind < 9) then
             tauwci = ypar*(taucur + tauwav)
             !
             ! primary and secondary bottom friction terms
             !
             taubpu(nm) = tauwci/(umod0*rhow + waveps)
          else
             hrmsu    = 0.5_fp * (hrms  (nm) + hrms  (nmu))
             tpu      = 0.5_fp * (tp    (nm) + tp    (nmu))
             rlabdau  = 0.5_fp * (rlabda(nm) + rlabda(nmu))
             rr       = -0.4_fp * sqrt(2.0_fp) / hu(nm) + 1.0_fp
             umax     = rr * 2.0_fp * uorbhs
             t1       = tpu  * sqrt(ag/hu(nm))
             u11      = umax / sqrt(ag*hu(nm))
             a11      = -0.0049_fp*t1**2 - 0.069_fp*t1 + 0.2911_fp
             raih     = max(0.5_fp , -5.25_fp-6.1_fp*tanh(a11*u11-1.76_fp))
             rmax     = max(0.62_fp , min(0.75_fp , -2.5_fp*hu(nm)/max(rlabdau,1.0e-20_fp) + 0.85_fp))
             uon      = umax * (0.5_fp + (rmax-0.5_fp)*tanh((raih-0.5_fp)/(rmax-0.5_fp)))
             uoff     = umax - uon
             uon      = max(1.0e-5_fp , uon)
             uoff     = max(1.0e-5_fp , uoff)
             uwbih    = (0.5_fp*uon**3.0_fp + 0.5_fp*uoff**3.0_fp)**(1.0_fp/3.0_fp)   
             rksru    = 0.5_fp*(rksr (nm) + rksr (nmu))
             rksmru   = 0.5_fp*(rksmr(nm) + rksmr(nmu))
             !
             ! Van Rijn 2004 formulation
             !
             phi        = acos((uuu*costu+vvv*sintu) / (umod+waveps))
             gamma      = 0.8_fp + phi - 0.3_fp*phi**2
             ksc        = sqrt(rksru**2 + rksmru**2)
             uratio     = min(uwbih/(u2dh+waveps) , 5.0_fp)
             ka(nm)     = ksc * exp(gamma*uratio)
             ka(nm)     = min(ka(nm) , 10.0_fp*ksc , 0.2_fp*hu(nm))
             ca         = 18.0_fp * log10(12.0_fp*hu(nm)/ka(nm))
             taubpu(nm) = ag * (u2dh * u2dh / umod0) / ca**2
          endif
          !
          ! Wave dissipation due to bottom friction
          !
          if (tpu > 0.1_fp) then
             !
             ! User-specified calibration factor FWFAC included to allow
             ! tuning of streaming stresses. Default = 1.0, off = 0.0
             !
             ! Updated expression for bottom dissipation of wave energy
             ! is now consistent with Unibest-TC
             !
             ks         = z0ucur(nm) * 33.0_fp
             omega      = 2.0_fp * pi / tpu
             a          = uorbu / omega
             !
             ! Change back to original formulation Manual Eq. (9.183) but still with calibration option fwfac
             !
             if (a > 0.0_fp) then
                fw = min(1.39_fp*(a/z0ucur(nm))**(-0.52_fp) , 0.3_fp)
             else
                fw = 0.3_fp
             endif
             fw = fw*fwfac
             !
             ! Change of definition dfu: dimension [W/m**2]
             ! to make it more consistent with definition dissipation wave breaking
             !
             dfu(nm)    = rhow * costu * fw * uorbu**3 / (sqrt(pi))
             deltau(nm) = 0.09_fp * alfaw * (ks/hu(nm)) * (a/ks)**0.82_fp
             deltau(nm) = max(alfaw*ee*z0ucur(nm)/hu(nm) , deltau(nm))
             !
             ! Change of definition deltau: dimension [m]
             !
             deltau(nm) = min(0.5_fp, deltau(nm))*hu(nm)
          else
             dfu(nm)    = 0.0_fp
             deltau(nm) = 0.0_fp
          endif
       else ! kfu(nm)<>1
          !
          !  this is essential for TRATUR
          !
          dfu(nm)    = 0.0_fp
          deltau(nm) = 0.0_fp
       endif
    enddo
    !
  999 continue
    if (modind == 0) then
       ndm  = -icy
       nmu  =  icx
       ndmu =  icx - icy
       do nm = 1, nmmax
          nmu  = nmu  + 1
          ndm  = ndm  + 1
          ndmu = ndmu + 1
          if (kfu(nm) == 1) then
             if (zmodel) then
                !
                ! Z-model
                ! Bottom is assumed at Z0
                ! use Ustar formulation (TAUBPU is upper bounded)
                !
                if (kfumin(nm) < kfumax(nm)) then
                   kmaxx      = kfumin(nm) + 1
                   uuu        = u1(nm, kmaxx)
                   kcscuttest = kcscut(nm, kmaxx)==1
                   actual_avg = (zmodel .and. kcscuttest)
                   if (actual_avg) then
                      svvv = max(kfv(ndm) + kfv(ndmu) + kfv(nm) + kfv(nmu), 1)
                      vvv  = (v1(ndm, kmaxx)*kfv(ndm) + v1(ndmu, kmaxx)*kfv(ndmu)&
                           & + v1(nm, kmaxx)*kfv(nm) + v1(nmu, kmaxx)*kfv(nmu))  &
                           & /svvv
                   else
                      vvv = 0.25*(v1(nm, kmaxx) + v1(ndm, kmaxx)                &
                          & + v1(ndmu, kmaxx) + v1(nmu, kmaxx))
                   endif
                   umod = sqrt(uuu*uuu + vvv*vvv)
                   !
                   ! kmaxx-1 point
                   !
                   kmaxx      = kmaxx - 1
                   uuu        = u1(nm, kmaxx)
                   kcscuttest = kcscut(nm, kmaxx)==1
                   actual_avg = (zmodel .and. kcscuttest)
                   if (actual_avg) then
                      svvv = max(kfv(ndm) + kfv(ndmu) + kfv(nm) + kfv(nmu), 1)
                      vvv  = (v1(ndm, kmaxx)*kfv(ndm) + v1(ndmu, kmaxx)*kfv(ndmu)&
                           & + v1(nm, kmaxx)*kfv(nm) + v1(nmu, kmaxx)*kfv(nmu))  &
                           & /svvv
                   else
                      vvv = 0.25*(v1(nm, kmaxx) + v1(ndm, kmaxx)                &
                          & + v1(ndmu, kmaxx) + v1(nmu, kmaxx))
                   endif
                   ubot = sqrt(uuu*uuu + vvv*vvv)
                   dz   = dzu1(nm, kfumin(nm)) + 0.5_fp*dzu1(nm, kfumin(nm)+1)
                else
                   !
                   ! 2D case: kfumin(nm) = kfumax(nm)
                   ! or dry point: kfumax(nm) = -1; velocities are all zero
                   !
                   kmaxx      = kfumin(nm)
                   uuu        = u1(nm, kmaxx)
                   kcscuttest = kcscut(nm, kmaxx)==1
                   actual_avg = (zmodel .and. kcscuttest)
                   if (actual_avg) then
                      svvv = max(kfv(ndm) + kfv(ndmu) + kfv(nm) + kfv(nmu), 1)
                      vvv  = (v1(ndm, kmaxx)*kfv(ndm) + v1(ndmu, kmaxx)*kfv(ndmu)&
                           & + v1(nm, kmaxx)*kfv(nm) + v1(nmu, kmaxx)*kfv(nmu))  &
                           & /svvv
                   else
                      vvv = 0.25*(v1(nm, kmaxx) + v1(ndm, kmaxx)                &
                          & + v1(ndmu, kmaxx) + v1(nmu, kmaxx))
                   endif
                   umod = sqrt(uuu*uuu + vvv*vvv)
                   ubot = umod
                   dz   = hu(nm)/ee
                endif
                z0urou(nm)    = hu(nm)/(ee*(exp(vonkar*cfurou(nm, 1)/sag) - 1.0))
                rz            = 1.0 + dz/z0urou(nm)
                cfurou(nm, 1) = log(rz)/vonkar
                ust           = umod/cfurou(nm, 1)
                taubpu(nm)    = ust*ust/(max(ubot, 0.0001_fp))
                taubxu(nm)    = rhow*taubpu(nm)*ubot
             else
                !
                ! sigma model; Apply law of the wall
                !
                kmaxx      = kmax
                uuu        = u1(nm, kmaxx)
                actual_avg =      (cstbnd .and. (kcs(nm)==2 .or. kcs(nmu)==2)) &
                           & .or. (kcs(nm)==3 .or. kcs(nmu)==3)
                if (actual_avg) then
                   svvv = max(kfv(ndm) + kfv(ndmu) + kfv(nm) + kfv(nmu), 1)
                   vvv  = (v1(ndm, kmaxx)*kfv(ndm) + v1(ndmu, kmaxx)*kfv(ndmu)   &
                        & + v1(nm, kmaxx)*kfv(nm) + v1(nmu, kmaxx)*kfv(nmu))/svvv
                else
                   vvv  = 0.25*(v1(nm, kmaxx) + v1(ndm, kmaxx) + v1(ndmu, kmaxx) &
                        & + v1(nmu, kmaxx))
                endif
                umod       = sqrt(uuu*uuu + vvv*vvv)
                z0urou(nm) = hu(nm)/(ee*(exp(vonkar*cfurou(nm, 1)/sag) - 1.0))
                !
                ! Bottom is assumed at Z0
                !
                if (kmax > 1) then
                   rz = 1.0 + (1. + sig(kmax))*hu(nm)/z0urou(nm)
                else
                   rz = 1.0 + hu(nm)/(ee*z0urou(nm))
                endif
                cfurou(nm, 1) = log(rz)/vonkar
                cwall         = 1./(cfurou(nm, 1)**2)
                taubpu(nm)    = cwall*umod
                taubxu(nm)    = rhow*taubpu(nm)*umod
             endif
          endif
       enddo
    else
       !
       ! modind .ne. 0
       !
       ndm  = -icy
       nmu  =  icx
       ndmu =  icx - icy
       do nm = 1, nmmax
          nmu  = nmu  + 1
          ndm  = ndm  + 1
          ndmu = ndmu + 1
          kmaxx = kmaxxx(nm)
          if (kfu(nm) == 1) then
             !
             ! Z-model
             !
             if (zmodel) then
                if (kfumin(nm) < kfumax(nm)) then
                   kmaxx = kfumin(nm) + 1
                   dz    = dzu1(nm, kfumin(nm)) + 0.5_fp*dzu1(nm, kfumin(nm)+1)
                else
                   kmaxx = kfumin(nm)
                   dz    = hu(nm)/ee
                endif
                kcscuttest = kcscut(nm, kmaxx)==1
             else
                kmaxx = kmaxxx(nm)
                kcscuttest = .false.
             endif
             uuu        = u1(nm, kmaxx)
             actual_avg =      (cstbnd .and. .not.zmodel .and.(kcs(nm)==2 .or. kcs(nmu)==2)) &
                        & .or. (kcs(nm)==3 .or. kcs(nmu)==3) &
                        & .or. (zmodel .and. kcscuttest)
             if (actual_avg) then
                svvv = max(kfv(ndm) + kfv(ndmu) + kfv(nm) + kfv(nmu), 1)
                vvv  = (v1(ndm, kmaxx)*kfv(ndm) + v1(ndmu, kmaxx)*kfv(ndmu)      &
                     & + v1(nm, kmaxx)*kfv(nm) + v1(nmu, kmaxx)*kfv(nmu))/svvv
             else
                vvv  = 0.25*(v1(nm, kmaxx) + v1(ndm, kmaxx) + v1(ndmu, kmaxx)    &
                     & + v1(nmu, kmaxx))
             endif
             umod = sqrt(uuu*uuu + vvv*vvv)
             ust  = sqrt(umod*taubpu(nm))
             if (ust > waveps) then
                !
                ! Limit exponent used in computation of z0urou to 40 to avoid over/underflow
                ! 
                cfurou(nm, 1) = min(umod/ust,40.0_fp)
                if (kmax > 1) then
                   if (zmodel) then
                      z0urou(nm) = dz/(exp(vonkar*cfurou(nm, 1)) - 1.0)
                   else
                      z0urou(nm) = (1.0 + sig(kmaxx))*hu(nm)                     &
                                 & /(exp(vonkar*cfurou(nm, 1)) - 1.0)
                   endif
                else
                   z0urou(nm) = hu(nm)/((exp(vonkar*cfurou(nm, 1)) - 1.0)*ee)
                endif
                !
                ! Limit z0rou to 10 m (required in large water depths when umod/ust is small)
                !
                z0urou(nm) = min(z0urou(nm), 10.0_fp)
                !
             endif
             if (modind == 9) then
                z0urou(nm) = max(3.33e-5_fp , ka(nm)/30.0)
             endif
          endif
       enddo
    endif
    !
    ! Bed stress is corrected in case of mass fluxes
    ! Modified in case of Waves
    ! use wave angle teta from communication file
    ! interpolate wave parameters to velocity points
    !
    if (wave .and. kmax>1) then
       do nm = 1, nmmax
          if (kfu(nm) == 1) then
             nmu = nm + icx
             !
             ! Need to check which direction
             !
             if (icx > 1) then
                !
                ! computing in X direction
                !
                costu = 0.5*(cos(degrad*teta(nm)) + cos(degrad*teta(nmu)))
             else
                !
                ! computing in Y direction
                !
                costu = 0.5*(sin(degrad*teta(nm)) + sin(degrad*teta(nmu)))
             endif
             hrmsu = 0.5*(hrms(nm) + hrms(nmu))
             tpu   = 0.5*(tp(nm) + tp(nmu))
             call stoktb(hrmsu     ,tpu       ,hu(nm)    ,ustokes   ,gdp       )
             !
             ! Added breaker delay adjustment
             !
             if (rolcorr==1) then
                taubsu(nm) = taubpu(nm)*(costu*ustokes + (grfacu(nm) + grmsur(nm))/hu(nm))
             else
                taubsu(nm) = taubpu(nm)*(costu*ustokes + grfacu(nm)/hu(nm) )
             endif
             !
          endif
       enddo
    else
       do nm = 1, nmmax
          if (kfu(nm) == 1) then
             taubsu(nm) = taubpu(nm) * (grmasu(nm) + grfacu(nm)) / hu(nm)
          endif
       enddo
    endif
    deallocate(ka, stat=ierror)
    deallocate(kmaxxx, stat=ierror)
end subroutine taubot
