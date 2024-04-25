subroutine dens_eck (temp, sal, rho ,rhods ,rhodt )
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
! calculation salinity ---> density
! eckart (1958): salt formula in FLOW
! seawater:
! temperature range: 0 - 40 DGR Celsius
! salinity range: 0 - 40
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
!
    use precision
    implicit none
!
! Global variables
!
    real(fp), intent(in)  :: temp
    real(fp), intent(in)  :: sal
    real(fp), intent(out) :: rho
    real(fp), intent(out) :: rhods
    real(fp), intent(out) :: rhodt
!
! Local variables
!
    real(fp)   ::   cp0
    real(fp)   ::   clam
    real(fp)   ::   clam0
    real(fp)   ::   cp1
    real(fp)   ::   clam1
    real(fp)   ::   alph0
    real(fp)   ::   cp1ds
    real(fp)   ::   cp1dt
    real(fp)   ::   cladt
    real(fp)   ::   rhom
!
! Data statements
!
    data     alph0/0.698_fp/, rhom/0.0_fp/
!
!! executable statements -------------------------------------------------------
!
    cp0   = 5890.0_fp + 38.00_fp*temp - 0.3750_fp*temp*temp
    clam  = 1779.5_fp + 11.25_fp*temp - 0.0745_fp*temp*temp
    clam0 =    3.8_fp +  0.01_fp*temp
    cp1   = cp0  + 3.0_fp*sal
    clam1 = clam - clam0 *sal
    rho   = 1000.0_fp*cp1/(alph0*cp1+clam1) - rhom
    !
    cp1ds = 3.0_fp
    rhods = 1000.0_fp * (cp1ds*clam1+cp1*clam0) / (alph0*cp1+clam1)**2
    !
    cp1dt = 38.0_fp - 0.75_fp*temp
    cladt = 11.25_fp - 0.149_fp*temp - 0.01_fp*sal
    rhodt = 1000.0_fp * (cp1dt*clam1-cp1*cladt) / (alph0*cp1+clam1)**2
end subroutine dens_eck
