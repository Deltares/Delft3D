subroutine bedbc1993(tp        ,uorb      ,rhowat    ,h1        ,umod      , &
                   & zumod     ,d50       ,d90       ,z0cur     ,z0rou     , &
                   & dstar     ,taucr     ,aks       ,usus      ,zusus     , &
                   & uwb       ,delr      ,muc       ,tauwav    ,ustarc    , &
                   & tauc      ,taubcw    ,taurat    ,ta        ,caks      , &
                   & dss       ,mudfrac   ,eps       ,aksfac    ,rwave     , &
                   & camax     ,rdc       ,rdw       ,iopkcw    ,iopsus    , &
                   & vonkar    ,wave      ,tauadd    ,betam     ,awb       )
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
! Compute bed roughness and shear stress parameters
! Van Rijn (1993,2000)
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use mathconsts
    !
    implicit none
!
! Arguments
!
    real(fp), intent(out)   :: aks    !  Description and declaration in esm_alloc_real.f90
    real(fp), intent(out)   :: awb
    real(fp), intent(in)    :: betam
    real(fp), intent(out)   :: caks
    real(fp), intent(in)    :: d50
    real(fp), intent(in)    :: d90
    real(fp), intent(out)   :: delr
    real(fp), intent(inout) :: dss    !  Description and declaration in esm_alloc_real.f90
    real(fp), intent(in)    :: dstar
    real(fp), intent(in)    :: h1
    real(fp), intent(out)   :: muc
    real(fp), intent(in)    :: mudfrac
    real(fp), intent(in)    :: rhowat !  Description and declaration in esm_alloc_real.f90
    real(fp), intent(out)   :: ta
    real(fp), intent(out)   :: taubcw
    real(fp), intent(out)   :: tauc
    real(fp), intent(in)    :: taucr
    real(fp), intent(out)   :: taurat
    real(fp), intent(out)   :: tauwav
    real(fp), intent(in)    :: tp     !  Description and declaration in esm_alloc_real.f90
    real(fp), intent(in)    :: umod
    real(fp), intent(in)    :: uorb   !  Description and declaration in esm_alloc_real.f90
    real(fp), intent(out)   :: ustarc
    real(fp), intent(out)   :: usus   !  Description and declaration in esm_alloc_real.f90
    real(fp), intent(out)   :: uwb
    real(fp), intent(in)    :: z0cur
    real(fp), intent(in)    :: z0rou
    real(fp), intent(in)    :: zumod
    real(fp), intent(out)   :: zusus
    real(fp), intent(in)    :: eps
    real(fp), intent(in)    :: aksfac
    real(fp), intent(in)    :: rwave
    real(fp), intent(in)    :: camax
    real(fp), intent(in)    :: rdc
    real(fp), intent(in)    :: rdw
    integer , intent(in)    :: iopkcw
    integer , intent(in)    :: iopsus
    real(fp), intent(in)    :: vonkar
    logical , intent(in)    :: wave
    real(fp), intent(in)    :: tauadd
!
! Local variables
!
    real(fp) :: delm
    real(fp) :: delw
    real(fp) :: f1c
    real(fp) :: f1w
    real(fp) :: fc
    real(fp) :: fw
    real(fp) :: muw
    real(fp) :: muwa
    real(fp) :: ra
    real(fp) :: rc
    real(fp) :: rw
    real(fp) :: taucr1   ! critical shear stress corrected for mud fraction
!
!! executable statements -------------------------------------------------------
!
    delr = 0.0_fp
    uwb  = 0.0_fp
    awb  = 0.0_fp
    !
    ! G. Lesser's implementation of Van Rijn's pick-up function for waves and currents.
    !
    ! Set Nikuradse roughness length using Z0CUR
    ! (current-only Z0 values transferred from TAUBOT).
    ! Expression limits aks to minimum of 0.01*h1 for accuracy
    !
    rc   = 30.0_fp*z0cur
    delr = 0.025_fp
    !
    usus  = umod
    zusus = zumod
    !
    ! Inserted options for determining Rc and Rw
    !
    if (iopkcw==1) then
       if (wave) then
          !
          ! calculate wave-related roughness height from ripple height
          !
          ! note: rwave is a user-specified constant
          ! (in range 1-3 according to Van Rijn 1993)
          !
          rw = min(max(0.01_fp, rwave*delr), 0.1_fp)
       endif
    else
       rc = rdc
       if (wave) rw = rdw
    endif
    !
    ! Calculate Van Rijn's reference height
    !
    aks = max(aksfac*rc, 0.01_fp*h1)
    !
    ! Adjust velocity to top of wave mixing layer and calculate other
    ! wave parameters (if waves are present)
    !
    tauwav = 0.0_fp
    muwa   = 0.0_fp
    muw    = 0.0_fp
    !
    if (wave) then
       if (tp>0.0_fp) then
          !
          ! Calculate apparent (enhanced) bed roughness ra
          !
          ! method of Van Rijn not implemented because it is more
          ! consistent to use the apparent roughness calculated by
          ! TAUBOT dependent on the chosen wave-current interaction
          ! model.
          !
          ra = 30.0_fp*z0rou
          !
          ! still limit according to Van Rijn
          !
          ra = min(10.0_fp*rc, ra)
          !
          ! Calculate wave parameters
          !
          uwb = sqrt(2.0_fp)*uorb
          awb = tp*uwb/(2.0_fp*pi)
          !
          ! Note: need this check to avoid floating overflow errors in
          ! calculation of fw and f1w
          !
          awb = max(awb, eps)
          !
          ! Check aks height
          !
          aks = max(delr/2.0_fp, aks)
          !
          ! Compute wave boundary laver thickness
          !
          delw = 0.072_fp*awb*(awb/rw)**(-0.25_fp)
          !
          ! Thickness of wave boundary mixing layer (Van Rijn (1993))
          !
          delm = 3.0_fp*delw
          !
          ! Limit minimum delm thickness
          !
          delm = max(delm, ra/30.0_fp)
          !
          ! Convert velocity to velocity at top of wave mixing layer,
          ! based on ENHANCED bed roughness
          ! Note that this means that Van Rijn's wave-current
          ! interaction factor alfacw is no longer required.
          ! Set this as the reference velocity and height
          !
          usus  = umod*log(1.0_fp + delm/z0rou)/log(1.0_fp + zumod/z0rou)
          zusus = delm
          !
          ! Calculate tauwav and muwa
          ! Calculate bed-shear stress due to waves
          !
          fw     = min(exp( - 6.0_fp + 5.2_fp*(awb/rw)**( - 0.19_fp)), 0.3_fp)
          tauwav = 0.25_fp*rhowat*fw*uwb**2
          !
          ! Calculate efficiency factor for waves
          ! (at reference level aks)
          !
          muwa = 0.6_fp/dstar
          !
          ! And for bed-load slope effects
          !
          f1w = exp( - 6.0_fp + 5.2_fp*(awb/(3*d90))**(-0.19_fp))
          muw = f1w/fw
       endif
    endif
    !
    ! Limit maximum aks to 20% of water depth
    ! (may be used when water depth becomes very small)
    !
    aks = min(aks, 0.2_fp * h1)
    !
    ! Calculate bed-shear stress due to currents
    ! Note: this expression uses the current-only roughness (z0 value)
    ! and is based on the velocity USUS at the height ZUSUS.
    ! Note that alfacw is not required in wave and current situations.
    !
    ustarc = usus*vonkar/log(1. + zusus/z0cur)
    tauc   = rhowat*ustarc**2
    if (tauadd>0.0_fp) then
       !
       ! extra stress
       !
       tauc   = sqrt(tauc**2 + tauadd**2)
       !
       ! update
       !
       ustarc = sqrt(tauc/rhowat)
    endif
    !
    ! Calculate efficiency factor currents
    !
    if (d90>0.0_fp) then
        f1c = 0.24_fp*log10(12.0_fp*h1/(3.0_fp*d90))**(-2)
    else
        f1c = 0.0_fp
    endif
    fc  = 0.24_fp*log10(12.0_fp*h1/rc)**(-2)
    muc = f1c/fc
    !
    ! Calculate bed shear stress ratio for bed-load slope effects
    ! Note: this ignores bed-slope effects on initiation of motion
    !
    taubcw = muc*tauc + muw*tauwav
    taucr1 = taucr*(1.0_fp + mudfrac)**betam
    taurat = taubcw/taucr1
    !
    ! Calculate Van Rijn's Dimensionless bed-shear stress for reference
    ! concentration at z=a
    !
    ta = max(0.0_fp, (muc*tauc + muwa*tauwav)/taucr1 - 1.0_fp)
    !
    ! Equilibrium concentration at reference level aks
    ! following Van Rijn.
    !
    if (ta>eps) then
       caks = min(0.015_fp*d50*ta**1.5_fp/(aks*dstar**0.3_fp), camax)
    else
       caks = 0.0_fp
    endif
    !
    ! Determination of suspended sediment size dss
    !
    if (iopsus==1) then
       if (ta<=1.0_fp) then
          dss = d50*0.64_fp
       elseif (ta>=25.0_fp) then
          dss = d50
       else
          dss = d50*(1.0_fp + 0.015_fp*(ta - 25.0_fp))
       endif
    endif
end subroutine bedbc1993
