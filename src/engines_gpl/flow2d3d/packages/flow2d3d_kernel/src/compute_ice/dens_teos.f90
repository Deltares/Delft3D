subroutine dens_teos(temp, sal, rhoteos10, rhods, rhodt, depth)
!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011.                                     
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
!  $Id: dens_teos.f90 64423 2019-07-24 11:30:09Z goede $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20190705_ice_modelling/src/engines_gpl/flow2d3d/packages/kernel/src/compute_ice/dens_teos.f90 $
!!--description-----------------------------------------------------------------
!
!    Function: Computes water density from temperature and
!              salinity using equation of state (rhowat).
!              
! Method used: Equation of state following UNESCO, (UNESCO,
!              Algorithms for computation of fundamental 
!              properties of seawater, UNESCO technical papers
!              in marine science, 1983)
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
    real(fp), intent(in)     :: temp
    real(fp), intent(in)     :: sal
    real(fp), intent(out)    :: rhoteos10
    real(fp), intent(out)    :: rhods
    real(fp), intent(out)    :: rhodt
    real(fp)                 :: gsw_rho 
    real(fp), intent(in)     :: depth
!
! Local variables
!
    real(fp)                            :: P

    P      = max(0.0_fp, 0.980665_fp*depth)
    rhoteos10 = gsw_rho(sal, temp, P)
    !
    ! This density formulation does noty work in combination with anti-creep
    !
    rhods  = 0.0_fp
    !
    rhodt  = 0.0_fp
end subroutine dens_teos
