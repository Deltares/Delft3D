subroutine trabwc(utot      ,di        ,taub      ,npar      ,par       , &
                & sbot      ,ssus      ,dg        ,fs        ,chezy     )
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
!  Computes sediment transport according to the Wilcock and Crowe sediment
!  transport formula. See: Wilcock and Crowe "Surface based transport for mixed 
!  size sediment", Journal of Hydraulic Engineering, Feb 2003
! 
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use morphology_data_module, only: missing_value
    !
    implicit none
!
! Arguments
!
    integer                  , intent(in)    :: npar
    real(fp)                 , intent(in)    :: chezy  ! local Chézy value [m1/2/s]
    real(fp)                 , intent(in)    :: dg     ! mean surface grain size [m]
    real(fp)                 , intent(in)    :: di     ! Grain size specified as d50
    real(fp)                 , intent(in)    :: fs     ! sand fraction on surface
    real(fp), dimension(npar), intent(inout) :: par    ! sediment parameter list
    real(fp)                 , intent(in)    :: taub   ! bed shear stress [N/m2]
    real(fp)                 , intent(in)    :: utot   ! flow velocity
    !
    real(fp)                 , intent(out)   :: sbot   ! bed load transport, magnitude [m3/m/s]
    real(fp)                 , intent(out)   :: ssus   ! suspended sediment transport
!
! Local variables
!
    real(fp) :: ag
    real(fp) :: rhosol
    real(fp) :: rhow
    real(fp) :: delta  ! relative density of sediment particle
    real(fp) :: a
    real(fp) :: ustar  ! shear velocity (m/s)
    real(fp) :: wistar ! 
    real(fp) :: phi    ! 
    real(fp) :: taurm  ! 
    real(fp) :: tauri  ! 
    real(fp) :: b      ! 
    real(fp) :: sag    ! 
!
!! executable statements -------------------------------------------------------
!
    sbot  = 0.0_fp
    ssus  = 0.0_fp
    if (chezy < 1.0e-6_fp) then
        return
    endif
    if (dg < 1.0e-6_fp) then
        return
    endif
    ag     = par(1)
    rhow   = par(2)
    rhosol = par(3)
    delta  = par(4)      ! delta = (rhosol - rhow) / rhow = rhosol / rhow - 1
    a      = par(11)     ! alpha: calibration coeficient specified by user
    sag    = sqrt(ag)
    ustar  = sag * utot / chezy
    !ustar   = sqrt(taub / rhow)
    b       = 0.67_fp / (1.0_fp + exp(1.5_fp - (di / dg)))
    taurm   = (0.021_fp + 0.015_fp * exp(-20.0_fp * fs)) * (rhosol - rhow) * ag * dg
    tauri   = taurm * (di / dg)**b
    phi     = taub / tauri
    if (phi < 1.35_fp) then
        wistar  = 0.002_fp * phi**7.5_fp
    else
        wistar  = 14.0_fp * (1.0_fp - (0.894_fp / sqrt(phi)))**4.5_fp
    endif
    ! bed load magnitude [m3/m/s]
    sbot    = a * wistar * ustar**3 / (delta * ag)
    ! note: proportion of size fraction on surface (fi) is included elsewhere
    !
    par     = missing_value
    par( 1) = wistar
    par( 2) = ustar
    par( 3) = phi
    par( 4) = tauri
    par( 5) = taurm
    par( 6) = b
end subroutine trabwc
