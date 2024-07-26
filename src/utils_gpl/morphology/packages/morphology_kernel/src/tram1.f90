subroutine tram1 (numrealpar,realpar   ,wave      ,npar      ,par       , &
                & num_layers_grid      ,bed       , &
                & tauadd    ,taucr0    ,aks       ,eps       ,camax     , &
                & frac      ,sig       ,thick     ,ws        , &
                & dicww     ,ltur      , &
                & kmaxsd    ,taurat    ,caks      , &
                & seddif    ,sigmol    ,rsedeq    ,scour     ,bedw      , &
                & susw      ,sbcu      ,sbcv      ,sbwu      ,sbwv      , &
                & sswu      ,sswv                 ,conc2d    ,error     , &
                & message   )
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
! computes sediment transport according to
! the formula of Van Rijn 1993
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use morphology_data_module
    !
    implicit none
!
! Arguments
!
    logical                         , intent(in)   :: scour
    logical                         , intent(in)   :: wave
    integer                         , intent(in)   :: num_layers_grid
    integer                         , intent(in)   :: ltur     !  Description and declaration in iidim.f90
    integer                         , intent(in)   :: npar
    integer                         , intent(in)   :: numrealpar
    real(fp)                        , intent(in)   :: bed
    real(fp)                        , intent(in)   :: bedw
    real(fp)                        , intent(in)   :: camax
    real(fp), dimension(0:num_layers_grid)     , intent(in)   :: dicww    !  Description and declaration in rjdim.f90
    real(fp)                        , intent(in)   :: eps
    real(fp)                        , intent(in)   :: frac     !  Description and declaration in rjdim.f90
    real(fp), dimension(num_layers_grid)       , intent(in)   :: sig      !  Description and declaration in rjdim.f90
    real(fp)                        , intent(in)   :: sigmol   !  Description and declaration in rjdim.f90
    real(fp)                        , intent(in)   :: susw
    real(fp)                        , intent(in)   :: tauadd
    real(fp)                        , intent(in)   :: taucr0
    real(fp), dimension(num_layers_grid)       , intent(in)   :: thick    !  Description and declaration in rjdim.f90
    real(fp), dimension(0:num_layers_grid)     , intent(in)   :: ws       !  Description and declaration in rjdim.f90
    real(fp), dimension(npar)       , intent(inout):: par
    !
    real(hp), dimension(numrealpar) , intent(inout):: realpar
    !
    logical                         , intent(out)  :: error
    integer                         , intent(out)  :: kmaxsd
    real(fp)                        , intent(out)  :: aks
    real(fp)                        , intent(out)  :: caks
    real(fp)                        , intent(out)  :: conc2d
    real(fp), dimension(num_layers_grid)       , intent(out)  :: rsedeq   !  Description and declaration in rjdim.f90
    real(fp)                        , intent(out)  :: sbcu
    real(fp)                        , intent(out)  :: sbcv
    real(fp)                        , intent(out)  :: sbwu
    real(fp)                        , intent(out)  :: sbwv
    real(fp), dimension(0:num_layers_grid)     , intent(out)  :: seddif   !  Description and declaration in rjdim.f90
    real(fp)                        , intent(out)  :: sswu
    real(fp)                        , intent(out)  :: sswv
    real(fp)                        , intent(out)  :: taurat
    character(*)                    , intent(out)  :: message     ! Contains error message
!
! Local variables
!
    integer :: iopsus
    real(fp):: aksfac
    real(fp):: rwave
    real(fp):: rdc
    real(fp):: rdw
    integer :: iopkcw
    logical :: epspar
    !
    real(fp):: ag       
    real(fp):: chezy    
    real(fp):: d10      
    real(fp):: d90      
    real(fp):: di50     
    real(fp):: dstar    
    real(fp):: h1       
    real(fp):: hidexp   
    real(fp):: hrms     
    real(fp):: mudfrac  
    real(fp):: rhosol   
    real(fp):: rhowat   
    real(fp):: rlabda   
    real(fp):: salinity 
    real(fp):: teta     
    real(fp):: tp       
    real(fp):: umod     
    real(fp):: uorb     
    real(fp):: uuu      
    real(fp):: vicmol   
    real(fp):: vonkar   
    real(fp):: vvv      
    real(fp):: z0cur    
    real(fp):: z0rou    
    real(fp):: zumod    
    !
    integer  :: k
    real(fp) :: avgcu
    real(fp) :: avgu
    real(fp) :: awb
    real(fp) :: bakdif
    real(fp) :: betam
    real(fp) :: delr
    real(fp) :: deltas
    real(fp) :: diffbt
    real(fp) :: dss
    real(fp) :: dz
    real(fp) :: epsbed
    real(fp) :: epsmax
    real(fp) :: epsmxc
    real(fp) :: fact1
    real(fp) :: gambr
    real(fp) :: hs
    real(fp) :: lci
    real(fp) :: muc
    real(fp) :: sag
    real(fp) :: ta
    real(fp) :: taubcw
    real(fp) :: tauc
    real(fp) :: tauwav
    real(fp) :: u
    real(fp) :: uoff
    real(fp) :: uon
    real(fp) :: ustarc
    real(fp) :: usus
    real(fp) :: utot
    real(fp) :: uwb
    real(fp) :: v
    real(fp) :: vcr
    real(fp) :: z
    real(fp) :: zusus
    logical  :: difvr
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
    !realpar(RP_DSS) = real(dss,hp)
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
    aksfac = par(12)
    rwave  = par(13)
    rdc    = par(14)
    rdw    = par(15)
    iopkcw = int(par(16))
    epspar = par(17)>0.0_fp
    betam  = par(18)
    !
    sag    = sqrt(ag)
    !
    call bedbc1993(tp        ,uorb      ,rhowat    ,h1        ,umod      , &
                 & zumod     ,di50      ,d90       ,z0cur     ,z0rou     , &
                 & dstar     ,taucr0    ,aks       ,usus      ,zusus     , &
                 & uwb       ,delr      ,muc       ,tauwav    ,ustarc    , &
                 & tauc      ,taubcw    ,taurat    ,ta        ,caks      , &
                 & dss       ,mudfrac   ,eps       ,aksfac    ,rwave     , &
                 & camax     ,rdc       ,rdw       ,iopkcw    ,iopsus    , &
                 & vonkar    ,wave      ,tauadd    ,betam     ,awb       )
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
    caks     = caks * frac
    !
    ! Calculate vertical sediment diffusion coefficient
    !
    if (tp>0.0_fp .and. wave) then
       hs = hrms * sqrt(2.0_fp)
       !
       ! calculate sediment mixing due to waves following Van Rijn 1993
       !
       deltas = 3.0_fp*delr
       deltas = min(max(0.05_fp, deltas), 0.2_fp)
       epsbed = 0.004_fp * dstar * deltas * uwb
       epsmax = 0.035_fp * h1 * hs / tp
    else
       deltas = 0.05_fp
       epsbed = 0.0_fp
       epsmax = 0.0_fp
    endif
    difvr = epspar .and. wave
    call calseddf1993(ustarc    ,ws        ,h1        ,num_layers_grid      ,sig       , &
                    & thick     ,dicww     ,tauwav    ,tauc      ,ltur      , &
                    & eps       ,vonkar    ,difvr     ,deltas    ,epsbed    , &
                    & epsmax    ,epsmxc    ,seddif    )
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
    if (caks>1.0e-6_fp) then
       !
       ! Put concentration in kmaxsd cell
       !
       dz     = h1*(1.0_fp + sig(kmaxsd))-aks
       diffbt = seddif(kmaxsd) + bakdif
       diffbt = max(diffbt , 0.1_fp*ws(kmaxsd)*dz)
       fact1  = 1.0_fp + dz * ws(kmaxsd) / diffbt
       rsedeq(kmaxsd) = caks / fact1 * rhosol
       !
       ! Now work upward
       !
       do k = kmaxsd - 1, 1, -1
          !
          ! Set diffusion coefficient at bottom of layer
          !
          dz        = h1 * (sig(k)-sig(k+1))
          diffbt    = seddif(k) + bakdif
          diffbt    = max(diffbt , 0.1_fp*ws(k)*dz)
          fact1     = 1.0_fp + dz * ws(k) / diffbt
          rsedeq(k) = rsedeq(k+1) / fact1
       enddo
       !
       ! And then work down
       !
       do k = kmaxsd + 1, num_layers_grid
          rsedeq(k) = rsedeq(k-1)
       enddo
    else
       !
       ! if caks <= 1.0e-6
       !
       do k = 1, num_layers_grid
          rsedeq(k) = 0.0_fp
       enddo
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
    if (scour) then
       utot = ustarc * chezy / sag
    else
       utot = avgu
    endif
    u     = utot * uuu / (umod+eps)
    v     = utot * vvv / (umod+eps)
    !
    if (bed>0.0_fp .or. bedw>0.0_fp .or. susw>0.0_fp) then
       call bedtr1993(uuu       ,vvv       ,utot      ,di50      ,d90       , &
                    & h1        ,taurat    ,ustarc    ,muc       ,rhosol    , &
                    & dstar     ,ws(1)     ,hrms      ,tp        ,teta      , &
                    & rlabda    ,umod      ,sbcu      ,sbcv      ,sbwu      , &
                    & sbwv      ,sswu      ,sswv      ,rhowat    ,ag        , &
                    & wave      ,eps       ,uon       ,uoff      ,vcr       , &
                    & error     ,message   )
       if (error) return
    else
       error = .false.
       uon   = missing_value
       uoff  = missing_value
       vcr   = missing_value
    endif

    ! van Rijn (1993) specific output
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
end subroutine tram1
