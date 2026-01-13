subroutine dens_chnmil(temp, sal, rhochnmil, rhods, rhodt, depth)
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
!  $Id: dens_chnmil.f90 64423 2019-07-24 11:30:09Z goede $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20190705_ice_modelling/src/engines_gpl/flow2d3d/packages/kernel/src/compute_ice/dens_chnmil.f90 $
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
    real(fp), intent(out)    :: rhochnmil
    real(fp), intent(out)    :: rhods
    real(fp), intent(out)    :: rhodt
    real(fp), intent(in)     :: depth
!
! Local variables
!
    real(fp)                            :: P
    real(fp)                            :: K
    real(fp)                            :: T2
    real(fp)                            :: s
    real(fp)                            :: sq
    real(fp)                            :: rhwa

    real(fp)                , parameter :: a0 = 999.842594_fp
    real(fp)                , parameter :: a1 = 6.793952E-2_fp
    real(fp)                , parameter :: a2 = -9.095290E-3_fp
    real(fp)                , parameter :: a3 = 1.001685E-4_fp
    real(fp)                , parameter :: a4 = -1.120083E-6_fp
    real(fp)                , parameter :: a5 = 6.536332E-9_fp
    real(fp)                , parameter :: b0 =  8.24493E-1_fp
    real(fp)                , parameter :: b1 = -4.0899E-3_fp
    real(fp)                , parameter :: b2 =  7.6438E-5_fp
    real(fp)                , parameter :: b3 = -8.2467E-7_fp
    real(fp)                , parameter :: b4 =  5.3875E-9_fp
    real(fp)                , parameter :: d0 = 5.72466E-3_fp
    real(fp)                , parameter :: d1 = 1.0227E-4_fp
    real(fp)                , parameter :: d2 = 1.6546E-6_fp
    real(fp)                , parameter :: d3 = 4.8314E-4_fp
!
!! executable statements -------------------------------------------------------
!
    s      = abs(sal)
    sq     = sqrt(s)
    P      = max(0.0_fp, 0.980665_fp*depth)

    T2     = temp*temp
    !
    rhwa   = a0+temp*(a1+temp*(a2+temp*(a3+temp*(a4+temp*a5))))
    K      = 19652.17_fp+148.113_fp*temp-2.293_fp*T2+1.256E-2_fp*temp*T2-4.18E-5_fp*T2*T2+ &
           & (3.2726_fp-2.147E-4_fp*temp+1.128E-4_fp*T2)*P+ &
           & (53.238_fp-0.313_fp*temp+5.728E3_fp*P)*s
    !
    rhochnmil = (rhwa +s*(b0+temp*(b1+temp*(b2+temp*(b3+temp*b4))))-(d0-d1*temp+s*(d2*T2*sq+d3*s)))/(1.0-P/K)
    !
    ! This density formulation does noty work in combination with anti-creep
    !
    rhods  = 0.0_fp
    !
    rhodt  = 0.0_fp
end subroutine dens_chnmil
