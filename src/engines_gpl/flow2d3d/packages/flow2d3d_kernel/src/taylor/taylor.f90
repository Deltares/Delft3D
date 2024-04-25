subroutine taylor(kmax      ,kmxdt     ,kmxt      ,np        ,nfreqs    , &
                & luniwe    ,wsp10     ,siglim    ,wvlbed    ,wvlsur    , &
                & frcbed    ,frcsur    ,ustbx     ,ustby     ,ustwix    , &
                & ustwiy    ,sig       ,thick     ,zeta      ,dps       , &
                & dudz      ,dvdz      ,rich      ,bruvai    ,rtur0     , &
                & vicww     ,u0        ,v0        ,tkepro    ,tkedis    , &
                & fuiwe     ,fviwe     ,d2u0      ,d2v0      ,atg       , &
                & btg       ,ctg       ,utg       ,vtg       ,d2u       , &
                & d2v       ,bv2       ,qz        ,r1tg      ,tke       , &
                & eps       ,dijdij    ,edvis     ,zlw       ,tlw       , &
                & ttkiw     ,tiwtk     ,futg      ,fvtg      ,freqlo    , &
                & frequp    ,gdp       )
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
!    Function:-Turbulence production,
!             -turbulence dissipation,
!             -turbulence diffusion and
!             -momentum exchange (not yet installed)
!              due to Internal Waves (IW's) generated
!
!             -internally by turbulence or
!             -externally by surface perturbations or
!             -externally by bed topography.
!
!              These processes are estimated by solving real-
!              valued eigenmodes of the Taylor-Goldstein second-
!              order differential equation for internal waves in
!              an arbitrary shear flow.
!
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
   !
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    real(fp)               , pointer :: alfaz
    real(fp)               , pointer :: xmu0
    real(fp)               , pointer :: viscof
    integer                , pointer :: nrange
    logical                , pointer :: iwedia
    real(fp)               , pointer :: ag
    real(fp)               , pointer :: vonkar
!
! Global variables
!
    integer                                      :: kmax   !  Description and declaration in esm_alloc_int.f90
    integer                                      :: kmxdt  !  Description and declaration in dimens.igs
    integer                                      :: kmxt   !  Description and declaration in dimens.igs
    integer                                      :: luniwe !!  Unit number for diagnostic reports of this subprogram as well as called subroutines
    integer                                      :: nfreqs !  Description and declaration in dimens.igs
    integer                                      :: np     !!  Dimension for IWE frequency arrays MAX (1,NFREQS)
    real(prec)                                   :: dps    !  Description and declaration in esm_alloc_real.f90
    real(fp)                                     :: frcbed !  Description and declaration in iwepar.igs
    real(fp)                                     :: frcsur !  Description and declaration in iwepar.igs
    real(fp)                                     :: siglim !  Description and declaration in iwepar.igs
    real(fp)                                     :: ustbx  !!  X/u-component of bed-shear stress velocity (zeta point)
    real(fp)                                     :: ustby  !!  Y/v-component of bed-shear stress velocity (zeta point)
    real(fp)                                     :: ustwix !!  X/u-component of wind-shear stress velocity
    real(fp)                                     :: ustwiy !!  Y/v-component of wind-shear stress velocity
    real(fp)                                     :: wsp10  !!  Windspeed at 10 m
    real(fp)                        , intent(in) :: wvlbed !  Description and declaration in iwepar.igs
    real(fp)                        , intent(in) :: wvlsur !  Description and declaration in iwepar.igs
    real(fp)                                     :: zeta   !!  Water surface elevation
    real(fp) , dimension(0:kmax)                 :: bruvai !  Description and declaration in esm_alloc_real.f90
    real(fp) , dimension(0:kmax)                 :: d2u0
    real(fp) , dimension(0:kmax)                 :: d2v0
    real(fp) , dimension(0:kmax)                 :: dudz   !  Description and declaration in esm_alloc_real.f90
    real(fp) , dimension(0:kmax)                 :: dvdz   !  Description and declaration in esm_alloc_real.f90
    real(fp) , dimension(0:kmax)                 :: rich   !  Description and declaration in esm_alloc_real.f90
    real(fp) , dimension(0:kmax)                 :: vicww  !  Description and declaration in esm_alloc_real.f90
    real(fp) , dimension(0:kmax, 2)              :: rtur0  !  Description and declaration in esm_alloc_real.f90
    real(fp) , dimension(0:kmxdt)                :: atg
    real(fp) , dimension(0:kmxdt)                :: btg
    real(fp) , dimension(0:kmxdt)                :: bv2
    real(fp) , dimension(0:kmxdt)                :: ctg
    real(fp) , dimension(0:kmxdt)                :: d2u
    real(fp) , dimension(0:kmxdt)                :: d2v
    real(fp) , dimension(0:kmxdt)                :: dijdij
    real(fp) , dimension(0:kmxdt)                :: edvis
    real(fp) , dimension(0:kmxdt)                :: eps    !  Description and declaration in numeco.igs
    real(fp) , dimension(0:kmxdt)                :: futg
    real(fp) , dimension(0:kmxdt)                :: fvtg
    real(fp) , dimension(0:kmxdt)                :: qz
    real(fp) , dimension(0:kmxdt)                :: r1tg
    real(fp) , dimension(0:kmxdt)                :: tiwtk
    real(fp) , dimension(0:kmxdt)                :: tke
    real(fp) , dimension(0:kmxdt)                :: tlw
    real(fp) , dimension(0:kmxdt)                :: ttkiw
    real(fp) , dimension(0:kmxdt)                :: utg
    real(fp) , dimension(0:kmxdt)                :: vtg
    real(fp) , dimension(0:kmxdt)                :: zlw
    real(fp) , dimension(kmax)                   :: fuiwe  !  Description and declaration in esm_alloc_real.f90
    real(fp) , dimension(kmax)                   :: fviwe  !  Description and declaration in esm_alloc_real.f90
    real(fp) , dimension(kmax)                   :: sig    !  Description and declaration in esm_alloc_real.f90
    real(fp) , dimension(kmax)                   :: thick  !  Description and declaration in esm_alloc_real.f90
    real(fp) , dimension(kmax)                   :: tkedis !  Description and declaration in esm_alloc_real.f90
    real(fp) , dimension(kmax)                   :: tkepro !  Description and declaration in esm_alloc_real.f90
    real(fp) , dimension(kmax)                   :: u0     !  Description and declaration in esm_alloc_real.f90
    real(fp) , dimension(kmax)                   :: v0     !  Description and declaration in esm_alloc_real.f90
    real(fp) , dimension(np, 2)                  :: freqlo
    real(fp) , dimension(np, 2)                  :: frequp
!
!
! Local variables
!
    integer                        :: k
    integer                        :: kbed                 ! K-value on TG grid: upper level stratified layer 
    integer                        :: kcrit                ! K-value on TG grid of the critical level associated with a given IW own mode (R1TG) 
    integer                        :: ktg
    integer                        :: ktgcd                ! KTG-value of upper most critical level 
    integer                        :: ktgcu                ! KTG-value of lowest critical level 
    integer                        :: ktop                 ! K-value on TG grid: lower level stratified layer 
    integer                        :: n
    integer                        :: ncrit
    integer                        :: nd
    integer                        :: ndir                 ! If NDIR=1 then horizontal Direction of IW propagation is in wavenumber direction, and in opposite direction for NDIR=2 
    integer                        :: nr
    integer                        :: nrd
    integer                        :: nru
    integer                        :: nu
    integer  , dimension(2, 2)     :: kcri                 ! K-value on TG grid of most upper and lower critical levels of turbulence- generated IW's 
    integer  , dimension(2, 2)     :: ncri                 ! Number of critical layers found per minimal or maximal K-value as well as sign of frequency of turbulence- generated IW's 
    logical                        :: barcli               ! presence of subgrid barcolcinic modes 
    logical                        :: frfind               ! If .true. then find root of TG equation as function of angular frequency, else find root as function of angle of horizontal wave number vector 
    logical                        :: singul               ! Indicator of singular solution of TG equation 
    logical                        :: tgfind               ! Indicator for instructing subroutine TG to yield the eigen mode for given input 
    logical                        :: top                  ! Indicator for instructing subroutine TG to yield the eigen mode by sweeping from surface to bed, or vice versa 
    logical                        :: useref               ! Switch instructing INTPOL using reference velocity 
    logical  , dimension(2, 2)     :: sing                 ! Indicator of singular solutions of TG equation 
    real(fp)                       :: angle                ! Angle [degrees] between horizontal IW wavenumber vector and x-u-axis 
    real(fp)                       :: az
    real(fp)                       :: azmax                ! Maximal rms IW vertical displacement amplitude 
    real(fp)                       :: bvav                 ! Mean bouyancy frequency 
    real(fp)                       :: bvmx                 ! Maximal buoyancy frequency 
    real(fp)                       :: bz                   ! Bandwidth of IW vertical wavenumber components 
    real(fp)                       :: crit                 ! Minimal value found after convergence of root-finding procedure ZBRENT 
    real(fp)                       :: cs2
    real(fp)                       :: delf                 ! Increment in non-dimensional angular frequency 
    real(fp)                       :: epsiwe               ! Depth integral of IWE dissipation rate 
    real(fp)                       :: factor
    real(fp)                       :: frlo                 ! Left bracket of frequency interval with largest root of TG equation 
    real(fp)                       :: frup                 ! Right bracket of frequency interval with largest root of TG equation 
    real(fp)                       :: h0                   ! Water depth 
    real(fp)                       :: hstrat               ! Thickness of stratified layer 
    real(fp)                       :: omeg                 ! Angular frequency of IW with respect to ground (root of TG equation) 
    real(fp)                       :: pi2
    real(fp)                       :: ratiod               ! Ratio between depth-averaged kin. energy in internal wave and kin. energy of vertical motions, based on lowest critical layer 
    real(fp)                       :: ratiou               ! Ratio between depth-averaged kin. energy in internal wave and kin. energy of vertical motions, based on upper critical layer 
    real(fp)                       :: ratiow               ! Mean ratio between depth-averaged kin energy in internal wave and kin. energy of vertical motions 
    real(fp)                       :: riav                 ! Mean value of all RICH with RICH > RICRIT 
    real(fp)                       :: rilz                 ! Vertical interval with RICH > RICRIT 
    real(fp)                       :: scale                ! Length scale for scaling 
    real(fp)                       :: term
    real(fp)                       :: umag
    real(fp)                       :: umean                ! Depth-average of u-velocity component 
    real(fp)                       :: ustmx
    real(fp)                       :: vmean                ! Depth-average of v-velocity component 
    real(fp)                       :: wvltke               ! First estimate of typical wave length of the turbulence that excites IW's 
    real(fp)                       :: xk                   ! Magnitude of horizntal wave number vector, in [m^-1] 
    real(fp)                       :: xkh                  ! Non-dimensional XK 
    real(fp)                       :: zlo                  ! Ozmidov length scale, depth-averaged over stratified layer 
    real(fp)                       :: zzlu
    character(132)                 :: stage                ! Text string to define in which part of the calibration we are 
!
!
!! executable statements -------------------------------------------------------
!
    !
    ag       => gdp%gdphysco%ag
    vonkar   => gdp%gdphysco%vonkar
    alfaz    => gdp%gdiwearr%alfaz
    xmu0     => gdp%gdiwearr%xmu0
    viscof   => gdp%gdiwearr%viscof
    nrange   => gdp%gdiwearr%nrange
    iwedia   => gdp%gdiwearr%iwedia
    !
    pi2 = 8.*atan(1.)
    !
    ! Initialise IWE-TKE exchange rates:
    !
    do k = 1, kmxt
       ttkiw(k) = 0.0
       tiwtk(k) = 0.0
    enddo
    do k = 1, kmax
       tkepro(k) = 0.0
       tkedis(k) = 0.0
    enddo
    !
    ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! IWE generation by turbulence implying internal       +
    ! re-distribution of TKE.                              +
    ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++
    !
    h0     = zeta + real(dps,fp)
    zzlu   = vonkar*h0/8
    wvltke = pi2*zzlu
    xk     = pi2/wvltke
    useref = .true.
    !
    call intpol(kmax      ,kmxdt     ,kmxt      ,h0        ,zeta      , &
              & dps       ,siglim    ,barcli    ,riav      ,rilz      , &
              & xk        ,u0        ,dudz      ,d2u0      ,ustbx     , &
              & ustwix    ,v0        ,dvdz      ,d2v0      ,ustby     , &
              & ustwiy    ,sig       ,thick     ,rich      ,bruvai    , &
              & rtur0     ,vicww     ,scale     ,bvav      ,bvmx      , &
              & xkh       ,kbed      ,ktop      ,umean     ,vmean     , &
              & utg       ,vtg       ,d2u       ,d2v       ,bv2       , &
              & tke       ,eps       ,edvis     ,zlw       ,tlw       , &
              & zlo       ,useref    ,gdp       )
    !
    hstrat = max(1, kbed - ktop)*h0/kmxt
    !
    ! Diagnostics
    !
    if (iwedia) then
       write (luniwe, *)
       write (luniwe, '(a)') '++++++++++++++++++++++++++++++++++'
       write (luniwe, '(a)') ' TAYLOR                          :'
       write (luniwe, '(a,1x,g9.2)') ' Mean Ri in layer             [-]:', riav
       write (luniwe, '(a,1x,g9.2)') ' Interval with Ri>0.25        [m]:', rilz
    endif
    !
    if (.not.barcli) then
       if (iwedia) write (luniwe, '(a)')                                        &
                     & ' TAYLOR                          : IWE model not applied'
       goto 9999
    else
       if (iwedia) then
          write (luniwe, '(a,1x,g9.2)') ' Upper level stratified layer [m]:',   &
                                      & h0*(1. - ktop/real(kmxt,sp))
          write (luniwe, '(a,1x,g9.2)') ' Lower level stratified layer [m]:',   &
                                      & h0*(1. - kbed/real(kmxt,sp))
          write (luniwe, '(a,1x,g9.2)') ' Thickness strat. layer       [m]:',   &
                                      & hstrat
          write (luniwe, '(a,1x,g9.2)') ' Mean Ozmidov length scale    [m]:',   &
                                      & zlo
          write (luniwe, '(a,1x,g9.2)') ' Minimal IW period            [s]:',   &
                                      & pi2/(bvav*bvmx)
       endif
       !
       ! IW vertical displacement amplitude and IWE-dissipation rate:
       !
       bz = hstrat/(xmu0*zlo)
       if (bz>1.01) then
          term   = 0.5*(1. - 1./(bz*bz))/log(bz)
          azmax  = hstrat*alfaz*sqrt(term)/pi2
          cs2    = 5.6
          factor = viscof*cs2*(bvav**2)*log(bz)*h0/kmxt
          !
          epsiwe = 0.0
          do ktg = 1, kmxt - 1
             if (ktop<=ktg .and. ktg<=kbed) then
                epsiwe = epsiwe + bv2(ktg)
             endif
          enddo
          epsiwe = epsiwe*factor
          !
          !
          if (iwedia) then
             write (luniwe, '(a,1x,g9.2)') &
                 & ' Maximal rms IWE Z-ampl.      [m]:', azmax
             write (luniwe, '(a,1x,g9.2)') &
                 & ' Bandwidth vertical wave numbers :', bz
             write (luniwe, '(a,1x,g9.2)') &
                 & ' Integral of IWE diss.  [m^3/s^3]:', epsiwe
          endif
       elseif (iwedia) then
          write (luniwe, '(a,1x,g9.2)') ' Bandwidth vertical wave numbers :', bz
          write (luniwe, '(2a       )')                                            &
                                & ' Therefore no bandwidth of vertical IWE modes', &
                                & ' inside stratified layer'
       else
       endif
    endif
    !
    ! Detect upper and lower critical layers:
    !
    call disper(kmxdt     ,kmxt      ,np        ,nfreqs    ,angle     , &
              & bvmx      ,freqlo    ,frequp    ,kcri      ,ncri      , &
              & sing      ,umean     ,vmean     ,atg       ,btg       , &
              & bv2       ,ctg       ,d2u       ,d2v       ,h0        , &
              & kbed      ,ktop      ,qz        ,r1tg      ,scale     , &
              & utg       ,vtg       ,xkh       ,gdp       )
    !
    !
    ! Find most upper and lowest levels with critical layers
    ! defined by:
    ! - KTGCU is KTG-level of highest critical layer
    !   (nearest to water surface)
    ! - KTGCD is KTG-level of lowest  critical layer
    !   (nearest to the bed)
    !
    ktgcu = kmxt
    ktgcd = 0
    ncrit = 0
    !
    nrd = 0
    nru = 0
    !
    do nr = 1, 2
       do ndir = 1, 2
          if (sing(nr, ndir)) then
             ncrit = ncrit + 1
             if (ktgcu>kcri(nr, ndir)) then
                ktgcu = kcri(nr, ndir)
                nru = nr
                nu = ncri(nr, ndir)
             endif
             if (ktgcd<kcri(nr, ndir)) then
                ktgcd = kcri(nr, ndir)
                nrd = nr
                nd = ncri(nr, ndir)
             endif
          endif
       enddo
    enddo
    !
    ! Upper most critical layer (ktgcu):
    !
    if (nru>0) then
       n      = nu
       nr     = nru
       top    = .true.
       frfind = .true.
       !
       stage  = ' Upper most critical layer, first step'
       !
       call zbrent(kmxdt     ,kmxt      ,omeg      ,angle     ,frfind    , &
                 & tgfind    ,stage     ,freqlo(n, nr)        ,frequp(n, nr)        ,atg       , &
                 & btg       ,bv2       ,ctg       ,crit      ,d2u       , &
                 & d2v       ,h0        ,kbed      ,ktop      ,qz        , &
                 & r1tg      ,scale     ,top       ,utg       ,vtg       , &
                 & xkh       ,luniwe    ,gdp       )
       !
       ! Force normalisation:
       !
       tgfind = .true.
       call tg(kmxdt     ,kmxt      ,angle     ,atg       ,btg       , &
             & bv2       ,ctg       ,crit      ,d2u       ,d2v       , &
             & h0        ,kbed      ,ktop      ,omeg      ,qz        , &
             & r1tg      ,scale     ,tgfind    ,top       ,utg       , &
             & vtg       ,xkh       ,gdp       )
       !
       !
       call seliwe(angle     ,kbed      ,ktop      ,kmxt      ,omeg      , &
                 & r1tg      ,utg       ,vtg       ,xkh       ,singul    , &
                 & top       ,kcrit     ,gdp       )
       !
       !
       if (singul .and. .not.top) then
          !
          stage = ' Upper most critical layer, second step'
          !
          call zbrent(kmxdt     ,kmxt      ,omeg      ,angle     ,frfind    , &
                    & tgfind    ,stage     ,freqlo(n, nr)        ,frequp(n, nr)        ,atg       , &
                    & btg       ,bv2       ,ctg       ,crit      ,d2u       , &
                    & d2v       ,h0        ,kbed      ,ktop      ,qz        , &
                    & r1tg      ,scale     ,top       ,utg       ,vtg       , &
                    & xkh       ,luniwe    ,gdp       )
          !
          ! Force normalisation:
          !
          tgfind = .true.
          call tg(kmxdt     ,kmxt      ,angle     ,atg       ,btg       , &
                & bv2       ,ctg       ,crit      ,d2u       ,d2v       , &
                & h0        ,kbed      ,ktop      ,omeg      ,qz        , &
                & r1tg      ,scale     ,tgfind    ,top       ,utg       , &
                & vtg       ,xkh       ,gdp       )
          !
          !
          call seliwe(angle     ,kbed      ,ktop      ,kmxt      ,omeg      , &
                    & r1tg      ,utg       ,vtg       ,xkh       ,singul    , &
                    & top       ,kcrit     ,gdp       )
       !
       !
       endif
       !
       ! Optional, correction on position upper most critical layer:
       !
       if (singul) ktgcu = min(ktgcu, kcrit)
       !
       call totiwe(kmxdt     ,kmxt      ,ratiou    ,singul    ,top       , &
                 & r1tg      ,h0        ,scale     ,kcrit     ,xkh       )
    endif
    !
    ! Lowest critical layer (ktgcd):
    !
    if (nrd>0) then
       n      = nd
       nr     = nrd
       top    = .true.
       frfind = .true.
       !
       stage  = ' Lowest critical layer, first step'
       !
       call zbrent(kmxdt     ,kmxt      ,omeg      ,angle     ,frfind    , &
                 & tgfind    ,stage     ,freqlo(n, nr)        ,frequp(n, nr)        ,atg       , &
                 & btg       ,bv2       ,ctg       ,crit      ,d2u       , &
                 & d2v       ,h0        ,kbed      ,ktop      ,qz        , &
                 & r1tg      ,scale     ,top       ,utg       ,vtg       , &
                 & xkh       ,luniwe    ,gdp       )
       !
       ! Force normalisation:
       !
       tgfind = .true.
       call tg(kmxdt     ,kmxt      ,angle     ,atg       ,btg       , &
             & bv2       ,ctg       ,crit      ,d2u       ,d2v       , &
             & h0        ,kbed      ,ktop      ,omeg      ,qz        , &
             & r1tg      ,scale     ,tgfind    ,top       ,utg       , &
             & vtg       ,xkh       ,gdp       )
       !
       !
       call seliwe(angle     ,kbed      ,ktop      ,kmxt      ,omeg      , &
                 & r1tg      ,utg       ,vtg       ,xkh       ,singul    , &
                 & top       ,kcrit     ,gdp       )
       !
       !
       if (singul .and. .not.top) then
          !
          stage = ' Lowest critical layer, second step'
          !
          call zbrent(kmxdt     ,kmxt      ,omeg      ,angle     ,frfind    , &
                    & tgfind    ,stage     ,freqlo(n, nr)        ,frequp(n, nr)        ,atg       , &
                    & btg       ,bv2       ,ctg       ,crit      ,d2u       , &
                    & d2v       ,h0        ,kbed      ,ktop      ,qz        , &
                    & r1tg      ,scale     ,top       ,utg       ,vtg       , &
                    & xkh       ,luniwe    ,gdp       )
          !
          ! Force normalisation:
          !
          tgfind = .true.
          call tg(kmxdt     ,kmxt      ,angle     ,atg       ,btg       , &
                & bv2       ,ctg       ,crit      ,d2u       ,d2v       , &
                & h0        ,kbed      ,ktop      ,omeg      ,qz        , &
                & r1tg      ,scale     ,tgfind    ,top       ,utg       , &
                & vtg       ,xkh       ,gdp       )
          !
          !
          call seliwe(angle     ,kbed      ,ktop      ,kmxt      ,omeg      , &
                    & r1tg      ,utg       ,vtg       ,xkh       ,singul    , &
                    & top       ,kcrit     ,gdp       )
       !
       !
       endif
       !
       ! Optional correction on position lowest critical layer:
       !
       if (singul) ktgcd = max(ktgcd, kcrit)
       !
       call totiwe(kmxdt     ,kmxt      ,ratiod    ,singul    ,top       , &
                 & r1tg      ,h0        ,scale     ,kcrit     ,xkh       )
    endif
    !
    ! In case of turbulence-generated critical layers:
    !
    if (ncrit>=2) then
       ktgcu  = max(1, ktgcu)
       ktgcd  = min(kmxt - 1, ktgcd)
       !ratiow = SQRT (ratiod*ratiou)
       ratiow = 2.0
       call turiwe(kmxdt     ,kmxt      ,h0        ,scale     ,bvav      , &
                 & xkh       ,utg       ,vtg       ,ratiow    ,kbed      , &
                 & ktop      ,ktgcd     ,ktgcu     ,tke       ,zlw       , &
                 & tlw       ,ttkiw     ,tiwtk     ,luniwe    ,gdp       )
    !
    !
    ! Without turbulence-generated critical layers:
    !
    else
       ktgcu  = ktop
       ktgcd  = kbed
       ratiow = 2.0
       call turiwe(kmxdt     ,kmxt      ,h0        ,scale     ,bvav      , &
                 & xkh       ,utg       ,vtg       ,ratiow    ,kbed      , &
                 & ktop      ,ktgcd     ,ktgcu     ,tke       ,zlw       , &
                 & tlw       ,ttkiw     ,tiwtk     ,luniwe    ,gdp       )
       !
       if (iwedia) write (luniwe, '(a)') ' No turb.-gen. crit. layers      '
    endif
    !
    ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! IWE generation by bed topograhy; using               +
    ! zero reference velocity                              +
    ! minimal velocity for notable wave generation set at  +
    ! 1 [cm/s]/15, see below:                              +
    ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++
    !
    umag  = ustbx*ustbx + ustby*ustby
    umag  = sqrt(umag)
    ustmx = 0.01/15.
    if (frcbed<1E-10 .or. umag<ustmx) goto 300
    !
    xk     = pi2/wvlbed
    useref = .false.
    !
    call intpol(kmax      ,kmxdt     ,kmxt      ,h0        ,zeta      , &
              & dps       ,siglim    ,barcli    ,riav      ,rilz      , &
              & xk        ,u0        ,dudz      ,d2u0      ,ustbx     , &
              & ustwix    ,v0        ,dvdz      ,d2v0      ,ustby     , &
              & ustwiy    ,sig       ,thick     ,rich      ,bruvai    , &
              & rtur0     ,vicww     ,scale     ,bvav      ,bvmx      , &
              & xkh       ,kbed      ,ktop      ,umean     ,vmean     , &
              & utg       ,vtg       ,d2u       ,d2v       ,bv2       , &
              & tke       ,eps       ,edvis     ,zlw       ,tlw       , &
              & zlo       ,useref    ,gdp       )
    !
    !
    ! Bracketing of angle intervals with lee waves i.e. with OMEG=0,
    ! the relevant angle intervals are stored in FREQLO(N,1) and FREQUP(N,1).
    !
    call brkbed(kmxdt     ,kmxt      ,np        ,nfreqs    ,atg       , &
              & btg       ,bv2       ,ctg       ,d2u       ,d2v       , &
              & h0        ,kbed      ,ktop      ,qz        ,r1tg      , &
              & scale     ,tgfind    ,utg       ,vtg       ,xkh       , &
              & freqlo    ,frequp    ,luniwe    ,gdp       )
    !
    !
    if (.not.tgfind) goto 300
    !
    ! Start from last bracketed angle interval (phase speed most
    ! oppposite to currebt speed) and search for solution which is either:
    ! - non-singular or, if singular,
    ! - has critical layer from bed upwards.
    !
    nr = 1
    n = 0
    !
  310 continue
    n      = n + 1
    top    = .false.
    frfind = .false.
    !
    stage  = ' Lee wave'
    !
    call zbrent(kmxdt     ,kmxt      ,omeg      ,angle     ,frfind    , &
              & tgfind    ,stage     ,freqlo(n, nr)        ,frequp(n, nr)        ,atg       , &
              & btg       ,bv2       ,ctg       ,crit      ,d2u       , &
              & d2v       ,h0        ,kbed      ,ktop      ,qz        , &
              & r1tg      ,scale     ,top       ,utg       ,vtg       , &
              & xkh       ,luniwe    ,gdp       )
    !
    ! Force normalisation:
    !
    tgfind = .true.
    call tg(kmxdt     ,kmxt      ,angle     ,atg       ,btg       , &
          & bv2       ,ctg       ,crit      ,d2u       ,d2v       , &
          & h0        ,kbed      ,ktop      ,omeg      ,qz        , &
          & r1tg      ,scale     ,tgfind    ,top       ,utg       , &
          & vtg       ,xkh       ,gdp       )
    !
    !
    call seliwe(angle     ,kbed      ,ktop      ,kmxt      ,omeg      , &
              & r1tg      ,utg       ,vtg       ,xkh       ,singul    , &
              & top       ,kcrit     ,gdp       )
    !
    !
    if (top .and. n<nrange) goto 310
    if (top) then
       write (luniwe, '(a)') 'TAYLOR : no bed-generated IW?'
    else
       !
       call bediwe(kmax      ,kmxdt     ,kmxt      ,h0        ,scale     , &
                 & bvav      ,xkh       ,angle     ,singul    ,kbed      , &
                 & ktop      ,kcrit     ,thick     ,ustbx     ,ustby     , &
                 & d2u       ,d2v       ,dudz      ,dvdz      ,utg       , &
                 & vtg       ,frcbed    ,edvis     ,qz        ,r1tg      , &
                 & dijdij    ,ttkiw     ,tiwtk     ,az        ,atg       , &
                 & btg       ,luniwe    ,gdp       )
    !
    !
    endif
    !
    ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! IWE generation by surface perturbations.             +
    ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++
    !
  300 continue
    umag = ustwix*ustwix + ustwiy*ustwiy
    umag = sqrt(umag)
    if (frcsur<1E-10 .or. umag<1.E-6) goto 400
    !
    xk = pi2/wvlsur
    useref = .true.
    !
    call intpol(kmax      ,kmxdt     ,kmxt      ,h0        ,zeta      , &
              & dps       ,siglim    ,barcli    ,riav      ,rilz      , &
              & xk        ,u0        ,dudz      ,d2u0      ,ustbx     , &
              & ustwix    ,v0        ,dvdz      ,d2v0      ,ustby     , &
              & ustwiy    ,sig       ,thick     ,rich      ,bruvai    , &
              & rtur0     ,vicww     ,scale     ,bvav      ,bvmx      , &
              & xkh       ,kbed      ,ktop      ,umean     ,vmean     , &
              & utg       ,vtg       ,d2u       ,d2v       ,bv2       , &
              & tke       ,eps       ,edvis     ,zlw       ,tlw       , &
              & zlo       ,useref    ,gdp       )
    !
    !
    ! Loop until IW is found which is either non-singular or
    ! singular with critical layer extending down from water surface:
    !
    frup = bvmx
    delf = bvmx/nfreqs
  410 continue
    call brksur(kmxdt     ,kmxt      ,nfreqs    ,angle     ,atg       , &
              & btg       ,bv2       ,ctg       ,d2u       ,d2v       , &
              & h0        ,kbed      ,ktop      ,qz        ,r1tg      , &
              & scale     ,tgfind    ,utg       ,vtg       ,xkh       , &
              & ustwix    ,ustwiy    ,frlo      ,frup      ,bvmx      , &
              & luniwe    ,gdp       )
    !
    !
    top = .true.
    frfind = .true.
    !
    stage = ' Wind-generated internal waves'
    !
    call zbrent(kmxdt     ,kmxt      ,omeg      ,angle     ,frfind    , &
              & tgfind    ,stage     ,frlo      ,frup      ,atg       , &
              & btg       ,bv2       ,ctg       ,crit      ,d2u       , &
              & d2v       ,h0        ,kbed      ,ktop      ,qz        , &
              & r1tg      ,scale     ,top       ,utg       ,vtg       , &
              & xkh       ,luniwe    ,gdp       )
    !
    ! Force normalisation:
    !
    tgfind = .true.
    call tg(kmxdt     ,kmxt      ,angle     ,atg       ,btg       , &
          & bv2       ,ctg       ,crit      ,d2u       ,d2v       , &
          & h0        ,kbed      ,ktop      ,omeg      ,qz        , &
          & r1tg      ,scale     ,tgfind    ,top       ,utg       , &
          & vtg       ,xkh       ,gdp       )
    !
    !
    call seliwe(angle     ,kbed      ,ktop      ,kmxt      ,omeg      , &
              & r1tg      ,utg       ,vtg       ,xkh       ,singul    , &
              & top       ,kcrit     ,gdp       )
    !
    !
    if (frlo>delf .and. .not.top) then
       frup = frlo
       goto 410
    elseif (.not.top) then
       write (luniwe, '(a)') 'TAYLOR : no wind-generated IW ?'
    else
       !
       call suriwe(kmax      ,kmxdt     ,kmxt      ,h0        ,scale     , &
                 & bvav      ,xkh       ,angle     ,omeg      ,singul    , &
                 & kbed      ,ktop      ,kcrit     ,wsp10     ,ustwix    , &
                 & ustwiy    ,utg       ,vtg       ,frcsur    ,edvis     , &
                 & qz        ,r1tg      ,dijdij    ,ttkiw     ,tiwtk     , &
                 & az        ,atg       ,btg       ,luniwe    ,gdp       )
    !
    !
    endif
    !
    !
    ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! The collected transfer rates are redistributed along +
    ! the DPM grid and used in TRATUR for the k-e model:   +
    !                                                      +
    ! Currently, the momentum fluxes/forces FUTG-FUIWE     +
    ! and FVTG-FVIWE are not yet installed.                +
    ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++
    !
  400 continue
    call distri(ttkiw     ,tiwtk     ,tkepro    ,tkedis    ,futg      , &
              & fvtg      ,fuiwe     ,fviwe     ,h0        ,thick     , &
              & bv2       ,ktop      ,kbed      ,kmax      ,kmxdt     , &
              & kmxt      ,luniwe    ,gdp       )
              !
              !
 9999 continue
end subroutine taylor
