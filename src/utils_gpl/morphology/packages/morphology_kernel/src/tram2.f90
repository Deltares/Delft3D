subroutine tram2 (numrealpar,realpar   ,wave      ,i2d3d     ,npar      , &
                & par       ,num_layers_grid      ,bed       ,dzduu     ,dzdvv     , &
                & rksrs     ,tauadd    ,taucr0    ,aks       ,eps       , &
                & camax     ,frac      ,sig       ,thick     ,ws        , &
                & dicww     ,ltur      ,aks_ss3d  ,iform     , &
                & kmaxsd    ,taurat    ,caks      ,caks_ss3d ,concin    , &
                & seddif    ,sigmol    ,rsedeq    ,scour     ,bedw      , &
                & susw      ,sbcu      ,sbcv      ,sbwu      ,sbwv      , &
                & sswu      ,sswv      ,tetacr    ,conc2d    ,error     , &
                & message   )
!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2025.                                     
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
! computes sediment transport according to
! the formula of Van Rijn 2004
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use morphology_data_module
    use sediment_basics_module, only: dsand, dgravel
    !
    implicit none
!
! Arguments
!
    logical                         , intent(in)   :: scour
    logical                         , intent(in)   :: wave
    integer                         , intent(in)   :: i2d3d
    integer                         , intent(in)   :: iform    ! transport formula number -2 for standard Van Rijn (2007), -4 for SANTOSS extended version
    integer                         , intent(in)   :: num_layers_grid
    integer                         , intent(in)   :: ltur     !  Description and declaration in iidim.f90
    integer                         , intent(in)   :: npar
    integer                         , intent(in)   :: numrealpar
    real(fp)                        , intent(in)   :: bed
    real(fp)                        , intent(in)   :: bedw
    real(fp)                        , intent(in)   :: camax
    real(fp), dimension(0:num_layers_grid)     , intent(in)   :: dicww    !  Description and declaration in rjdim.f90
    real(fp)                        , intent(in)   :: dzduu    !  Description and declaration in rjdim.f90
    real(fp)                        , intent(in)   :: dzdvv    !  Description and declaration in rjdim.f90
    real(fp)                        , intent(in)   :: eps
    real(fp)                        , intent(in)   :: frac     !  Description and declaration in rjdim.f90
    real(fp)                        , intent(in)   :: rksrs    !  Description and declaration in rjdim.f90
    real(fp), dimension(num_layers_grid)       , intent(in)   :: sig      !  Description and declaration in rjdim.f90
    real(fp)                        , intent(in)   :: sigmol   !  Description and declaration in rjdim.f90
    real(fp)                        , intent(in)   :: susw
    real(fp)                        , intent(in)   :: tauadd
    real(fp)                        , intent(in)   :: taucr0
    real(fp)                        , intent(in)   :: tetacr
    real(fp), dimension(num_layers_grid)       , intent(in)   :: thick    !  Description and declaration in rjdim.f90
    real(fp), dimension(0:num_layers_grid)     , intent(in)   :: ws       !  Description and declaration in rjdim.f90
    !
    real(fp), dimension(num_layers_grid)       , intent(inout):: concin   ! if (i2d3d==2 .or. epspar) then output else input
    real(fp), dimension(npar)       , intent(inout):: par
    real(hp), dimension(numrealpar) , intent(inout):: realpar
    !
    logical                         , intent(out)  :: error
    integer                         , intent(out)  :: kmaxsd
    real(fp)                        , intent(out)  :: aks
    real(fp)                        , intent(out)  :: aks_ss3d
    real(fp)                        , intent(out)  :: caks
    real(fp)                        , intent(out)  :: caks_ss3d
    real(fp)                        , intent(out)  :: conc2d
    real(fp), dimension(num_layers_grid)       , intent(out)  :: rsedeq   ! undefined if (i2d3d==3 .and. .not. epspar .and. caks>1e-6)
    real(fp)                        , intent(out)  :: sbcu
    real(fp)                        , intent(out)  :: sbcv
    real(fp)                        , intent(out)  :: sbwu
    real(fp)                        , intent(out)  :: sbwv
    real(fp)                        , intent(out)  :: sswu
    real(fp)                        , intent(out)  :: sswv
    real(fp), dimension(0:num_layers_grid)     , intent(out)  :: seddif   !  Description and declaration in rjdim.f90
    real(fp)                        , intent(out)  :: taurat
    character(*)                    , intent(out)  :: message     ! Contains error message
!
! Local variables
!

    logical  :: epspar
    
    integer  :: iopsus
    integer  :: k
    integer  :: subiw
    integer  :: wform

    ! SANTOSS flags
    integer  :: as_effects
    integer  :: pl_effects
    integer  :: sl_effects
    integer  :: sw_effects
    
    real(fp) :: ag       
    real(fp) :: as
    real(fp) :: avgcu
    real(fp) :: avgu
    real(fp) :: awb
    real(fp) :: bakdif
    real(fp) :: betam
    real(fp) :: chezy    
    real(fp) :: d10      
    real(fp) :: d90      
    real(fp) :: delm
    real(fp) :: deltas
    real(fp) :: delw
    real(fp) :: di50     
    real(fp) :: drho
    real(fp) :: dss
    real(fp) :: dstar
    real(fp) :: dz
    real(fp) :: epsmax
    real(fp) :: epsmxc
    real(fp) :: fc1
    real(fp) :: fcc
    real(fp) :: fcwc
    real(fp) :: fcwt
    real(fp) :: ff
    real(fp) :: fi
    real(fp) :: fpco
    real(fp) :: fsilt
    real(fp) :: fw1
    real(fp) :: gamtcr
    real(fp) :: h1       
    real(fp) :: hidexp   
    real(fp) :: hrms     
    real(fp) :: ksc
    real(fp) :: ksw
    real(fp) :: lci
    real(fp) :: muc
    real(fp) :: mudfrac  
    real(fp) :: occ
    real(fp) :: oct
    real(fp) :: otc
    real(fp) :: ott
    real(fp) :: pangle
    real(fp) :: pc
    real(fp) :: phi_phase
    real(fp) :: phicur
    real(fp) :: phicx
    real(fp) :: phitx
    real(fp) :: psi
    real(fp) :: pt
    real(fp) :: qsu
    real(fp) :: r
    real(fp) :: ra
    real(fp) :: rh
    real(fp) :: rhosol   
    real(fp) :: rhowat   
    real(fp) :: rlabda   
    real(fp) :: salinity 
    real(fp) :: salmax
    real(fp) :: screpr
    real(fp) :: sk
    real(fp) :: strepr
    real(fp) :: ta
    real(fp) :: taubcw
    real(fp) :: tauc
    real(fp) :: taucr1
    real(fp) :: tauwav
    real(fp) :: tc
    real(fp) :: teta     
    real(fp) :: tp       
    real(fp) :: tt
    real(fp) :: u
    real(fp) :: u2dhim
    real(fp) :: ucrepr
    real(fp) :: umod     
    real(fp) :: uoff
    real(fp) :: uon
    real(fp) :: uorb     
    real(fp) :: ustarc
    real(fp) :: usus
    real(fp) :: utot
    real(fp) :: utrepr
    real(fp) :: uuu      
    real(fp) :: uwb
    real(fp) :: uwbih
    real(fp) :: uwc
    real(fp) :: uwt
    real(fp) :: v
    real(fp) :: vcr
    real(fp) :: vicmol   
    real(fp) :: vonkar   
    real(fp) :: vvv      
    real(fp) :: ws0
    real(fp) :: z
    real(fp) :: z0cur    
    real(fp) :: z0rou    
    real(fp) :: zumod    
    real(fp) :: zusus

!
!! executable statements -------------------------------------------------------
!
    uuu       = real(realpar(RP_UCHAR),fp)
    vvv       = real(realpar(RP_VCHAR),fp)
    umod      = real(realpar(RP_VELCH),fp)
    zumod     = real(realpar(RP_ZVLCH),fp)
    h1        = real(realpar(RP_DEPTH),fp)
    chezy     = real(realpar(RP_CHEZY),fp)
    hrms      = real(realpar(RP_HRMS) ,fp)
    tp        = real(realpar(RP_TPEAK),fp)
    teta      = real(realpar(RP_TETA) ,fp)
    rlabda    = real(realpar(RP_RLAMB),fp)
    uorb      = real(realpar(RP_UORB) ,fp)
    di50      = real(realpar(RP_D50)  ,fp)
    dss       = real(realpar(RP_DSS)  ,fp)
    dstar     = real(realpar(RP_DSTAR),fp)
    d10       = real(realpar(RP_D10MX),fp)
    d90       = real(realpar(RP_D90MX),fp)
    mudfrac   = real(realpar(RP_MUDFR),fp)
    hidexp    = real(realpar(RP_HIDEX),fp)
    !ws        = real(realpar(RP_SETVL),fp)
    rhosol    = real(realpar(RP_RHOSL),fp)
    rhowat    = real(realpar(RP_RHOWT),fp)
    salinity  = real(realpar(RP_SALIN),fp)
    !temp      = real(realpar(RP_TEMP) ,fp)
    ag        = real(realpar(RP_GRAV) ,fp)
    vicmol    = real(realpar(RP_VICML),fp)
    !taub      = real(realpar(RP_TAUB) ,fp)
    !ubed      = real(realpar(RP_UBED ),fp)
    !vbed      = real(realpar(RP_VBED ),fp)
    !velb      = real(realpar(RP_VELBD),fp)
    !zvelb     = real(realpar(RP_ZVLBD),fp)
    vonkar    = real(realpar(RP_VNKAR),fp)
    z0cur     = real(realpar(RP_Z0CUR),fp)
    z0rou     = real(realpar(RP_Z0ROU),fp)
    !
    iopsus = int(par(11))
    pangle = par(12)
    fpco   = par(13)
    subiw  = int(par(14))
    epspar = par(15)>0.0_fp
    gamtcr = par(16)
    salmax = par(17)
    betam  = par(18)
    wform  = int(par(19))
    ! ----
    ! SANTOSS only
    sw_effects = int(par(20))
    as_effects = int(par(21))
    pl_effects = int(par(22))
    sl_effects = int(par(23))
    ! ----
    !
    tp = max(tp, 1e-2_fp)
    drho  = (rhosol-rhowat) / rhowat
    !
    if (di50 < 1.5_fp*dsand) then
       ws0 = drho*ag*di50**2/(18.0_fp*vicmol)
    elseif (di50 < 0.5_fp*dgravel) then
       ws0 = 10.0_fp*vicmol/di50 &
          & *(sqrt(1.0_fp + drho*ag*di50**3/(100.0_fp*vicmol**2)) - 1.0_fp)
    else
       ws0 = 1.1_fp*sqrt(drho*ag*di50)
    endif
    !
    call bedbc2004(tp        ,rhowat    , &
                 & h1        ,umod      ,d10       ,zumod     ,di50      , &
                 & d90       ,z0cur     ,z0rou     ,drho      ,dstar     , &
                 & taucr0    ,u2dhim    ,aks       ,ra        ,usus      , &
                 & zusus     ,uwb       ,muc       ,tauwav    ,ustarc    , &
                 & tauc      ,taurat    ,ta        ,caks      ,dss       , &
                 & uwc       ,uuu       ,vvv       ,rlabda    ,taubcw    , &
                 & hrms      ,delw      ,uon       ,uoff      ,uwbih     , &
                 & delm      ,fc1       ,fw1       ,phicur    ,rksrs     , &
                 & i2d3d     ,mudfrac   ,fsilt     ,taucr1    ,psi       , &
                 & dzduu     ,dzdvv     ,eps       ,camax     ,iopsus    , &
                 & ag        ,wave      ,tauadd    ,gamtcr    ,betam     , &
                 & awb       ,wform     ,phi_phase ,r         ) 
    realpar(RP_DSS)   = real(dss    ,hp)
    !
    ! Find bottom cell for SAND sediment calculations and store for use
    ! in DIFU and DIF_WS
    !
    kmaxsd = 1
    do k = num_layers_grid - 1, 1, -1
       !
       ! Calculate level of lower cell interface
       !
       lci = (1.0_fp + sig(k) - thick(k)/2.0_fp) * h1
       if (lci >= aks) then
          kmaxsd = k
          exit
       endif
    enddo
    !
    ! Adjust caks for presence of multiple sediment fractions.
    !
    caks    = caks * frac
    !
    ! Calculate sediment mixing due to waves following
    ! Van Rijn 2004 - intra-wave approach for bed load (original TR2004)
    !
    caks_ss3d = caks
    aks_ss3d  = aks
    if (caks > 1.0e-6_fp) then
       call calseddf2004(ustarc    ,ws        ,tp        ,hrms      ,h1        , &
                       & seddif    ,num_layers_grid      ,sig       ,thick     ,dicww     , &
                       & tauwav    ,tauc      ,ltur      ,delw      ,rhowat    , &
                       & uwbih     ,aks       ,caks      ,caks_ss3d ,deltas    , &
                       & aks_ss3d  ,di50      ,salinity  ,ws0       ,psi       , &
                       & epspar    ,eps       ,vonkar    ,salmax    ,wave      , &
                       & epsmax    ,epsmxc    )
    else
       do k = 1, num_layers_grid
          seddif(k) = dicww(k)
       enddo
       deltas = 0.05_fp
       epsmax = 0.0_fp
       epsmxc = missing_value
    endif
    !
    ! Calculate equilibrium concentration profile for sediment
    ! Note: option of selecting either Rouse profile or solution
    ! by numerical integration has been removed; only numerical
    ! integration.
    ! set background diffusion and effective beta factor
    !
    bakdif = vicmol / sigmol
    !
    ! Use simple expression based on upwind approximation for
    ! concentration and fall velocity, and central difference for
    ! concentration gradient.
    ! solution to stationary advection/diffusion equation in vertical.
    !
    if (i2d3d==2 .or. epspar) then
       !
       ! write concentration concin in an empty array if 2D or
       ! Van Rijn's parametric model and susp. transp. due to waves is used       
       !
       if (caks>1.0e-6_fp) then
          rsedeq(kmaxsd+1) = caks_ss3d
          do k = kmaxsd, 1, -1
             if (rsedeq(k+1) > 1.0e-6_fp) then
                dz        = h1 * (sig(k)-sig(k+1))
                fi        = max(1.0_fp + ((rsedeq(k+1)/0.65_fp)**0.8_fp) - 2.0_fp*((rsedeq(k+1)/0.65_fp)**0.4_fp), 0.01_fp)
                seddif(k) = seddif(k) * fi                      
                fcc       = -ws(k) * rsedeq(k+1) * (max(1.0_fp-rsedeq(k+1),0.5_fp))**5.0_fp / seddif(k)
                ff        = 1.0_fp / rsedeq(k+1) * fcc
                rsedeq(k) = exp(log(rsedeq(k+1))+dz*ff)
             else
                rsedeq(k) = 0.0_fp
             endif
          enddo
          !
          ! And then work down
          !
          if (num_layers_grid > kmaxsd+1) then
             do k = kmaxsd + 2, num_layers_grid
                rsedeq(k) = rsedeq(k-1)
             enddo
          endif
          do k = 1,num_layers_grid
             rsedeq(k) = rsedeq(k) * rhosol
             concin(k) = rsedeq(k)
          enddo
       else
          !
          ! caks<=1.0e-6_fp
          !
          do k = 1, num_layers_grid
             rsedeq(k) = 0.0_fp
             concin(k) = 0.0_fp
          enddo
       endif
    else
       !
       ! use the r0 values copied into concin array by erosed
       !
       if (caks<=1.0e-6_fp) then
          do k = 1, num_layers_grid
             rsedeq(k) = 0.0_fp
          enddo
       endif
    endif
    !
    ! Compute depth-averaged velocity, representative concentration and transport
    !
    ! imaginary "depth-averaged current" which has a logarithmic
    ! velocity profile, and a velocity at the bottom zeta point equivalent
    ! to that calculated by the model for 3D current and waves is
    ! calculated in bedbc2004/ (also 1993??) = u2dhim
    !
    avgu     = 0.0_fp
    avgcu    = 0.0_fp
    if (zumod > 0.0_fp) then
       do k = 1, num_layers_grid
          z     = (1.0_fp + sig(k)) * h1
          u     = log(1.0_fp + z/z0rou)
          avgu  = avgu  + u*thick(k)
          avgcu = avgcu + u*rsedeq(k)*thick(k)
       enddo
       conc2d = avgcu / max(avgu,eps)
       avgu   = avgu * umod / log(1.0_fp + zumod/z0rou)
    else
       conc2d = 0.0_fp
    endif
    !
    if (scour) then
       utot = ustarc * chezy / sqrt(ag)
    else
       utot = avgu
    endif
    u     = utot * uuu / (umod+eps)
    v     = utot * vvv / (umod+eps)
    !
    ! Instantaneous bed load
    !
    if ((bed>0.0_fp .or. bedw>0.0_fp .or. susw>0.0_fp) .and. caks>0.0_fp) then
       if (iform == -2) then
           ! standard Van Rijn (2007) bed load
           call bedtr2004(u2dhim    ,di50      ,d90       ,h1        ,rhosol    , &
                        & tp        ,teta      ,uon       ,uoff      ,uwb       , &
                        & taucr1    ,delm      ,ra        ,z0cur     ,fc1       , &
                        & fw1       ,dstar     ,drho      ,phicur    ,sbcu      , &
                        & sbcv      ,sbwu      ,sbwv      ,sswu      ,sswv      , &
                        & tetacr    ,aks       ,fsilt     ,sig       ,thick     , &
                        & concin    ,num_layers_grid      ,deltas    ,ws(1)     ,rksrs     , &
                        & dzduu     ,dzdvv     ,rhowat    ,ag        ,bedw      , &
                        & pangle    ,fpco      ,susw      ,wave      ,eps       , &
                        & subiw     ,vcr       ,error     ,message   ,wform     , &
                        & r         ,phi_phase ,uwbih     )
           ! van Rijn (2004) specific output
           par     = missing_value
           par( 1) = tauc
           par( 2) = tauwav
           par( 3) = taubcw
           par( 4) = usus
           par( 5) = zusus
           par( 6) = dss
           par( 7) = caks
           par( 8) = aks
           par( 9) = deltas
           par(10) = epsmxc
           par(11) = epsmax
           par(12) = uon
           par(13) = uoff
           par(14) = vcr
           par(15) = uwb
           par(16) = awb
           par(17) = rksrs
       elseif (iform == -4) then
           ! extended SANTOSS bed load
           call santoss(h1, di50, d90, hrms, tp, uorb, teta, uuu, vvv, umod, zumod, &
                      & ag, vicmol, rhosol, rhowat, sw_effects, as_effects, &
                      & pl_effects, sl_effects, dzduu, dzdvv, i2d3d, &
                      & sbcu, sbcv, sbwu, sbwv, sswu, sswv , &
                      & uwc, uwt, rh, ksw, ksc, ucrepr, utrepr, fcwc, fcwt, &
                      & screpr, strepr, pc, pt, occ, otc, ott, oct, tc, tt, &
                      & phicx, phitx, qsu, sk, as, &
                      & error, message)
           par     = -999.0_fp
           par( 1) = uwc
           par( 2) = uwt
           par( 3) = rh
           par( 4) = ksw
           par( 5) = ksc
           par( 6) = ucrepr
           par( 7) = utrepr
           par( 8) = fcwc
           par( 9) = fcwt
           par(10) = screpr
           par(11) = strepr
           par(12) = pc
           par(13) = pt
           par(14) = occ
           par(15) = otc
           par(16) = ott
           par(17) = oct
           par(18) = tc
           par(19) = tt
           par(20) = phicx
           par(21) = phitx
           par(22) = qsu
           par(23) = sk
           par(24) = as
       endif
    else
       !par   = -999.0_fp
       error = .false.
    endif
end subroutine tram2
