module m_density
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
!  $Id$
!  $HeadURL$
!-------------------------------------------------------------------------------
use precision

implicit none

integer, parameter :: DENS_NO     = 0
integer, parameter :: DENS_ECKART = 1
integer, parameter :: DENS_UNESCO = 2
integer, parameter :: DENS_NACL   = 3

public :: density

private:: density_eckart, density_unesco, density_nacl

contains
!
!
!==============================================================================
real(hp) function density(method, temp, salt)
    !
    ! Parameters
    integer  :: method
    real(hp) :: temp
    real(hp) :: salt
    !
    ! Body
    select case(method)
    case(DENS_ECKART)
        density = density_eckart(temp, salt)
    case(DENS_UNESCO)
        density = density_unesco(temp, salt)
    case(DENS_NACL)
        density = density_nacl(temp, salt)
    case default
        density = 0.0_hp
    end select
end function density
!
!
!==============================================================================
function density_eckart (temp, sal) result (rho)
!!--description-----------------------------------------------------------------
!
! calculation salinity ---> density
! eckart (1958): salt formula in FLOW
! seawater:
! temperature range: 0 - 40 DGR Celsius
! salinity range: 0 - 40
!
!-------------------------------------------------------------------------------
    !
    ! Global variables
    real(hp), intent(in)  :: temp
    real(hp), intent(in)  :: sal
    !
    ! Local variables
    real(hp)   ::   cp0
    real(hp)   ::   clam
    real(hp)   ::   clam0
    real(hp)   ::   cp1
    real(hp)   ::   clam1
    real(hp)   ::   alph0
    real(hp)   ::   cp1ds
    real(hp)   ::   cp1dt
    real(hp)   ::   cladt
    real(hp)   ::   rhom
    real(hp)  :: rhods
    real(hp)  :: rhodt
    real(hp) :: rho
    !
    ! Data statements
    data     alph0/0.698_hp/, rhom/0.0_hp/
    !
    !! executable statements -------------------------------------------------------
    cp0   = 5890.0_hp + 38.00_hp*temp - 0.3750_hp*temp*temp
    clam  = 1779.5_hp + 11.25_hp*temp - 0.0745_hp*temp*temp
    clam0 =    3.8_hp +  0.01_hp*temp
    cp1   = cp0  + 3.0_hp*sal
    clam1 = clam - clam0 *sal
    rho   = 1000.0_hp*cp1/(alph0*cp1+clam1) - rhom
    !
    cp1ds = 3.0_hp
    rhods = 1000.0_hp * (cp1ds*clam1+cp1*clam0) / (alph0*cp1+clam1)**2
    !
    cp1dt = 38.0_hp - 0.75_hp*temp
    cladt = 11.25_hp - 0.149_hp*temp - 0.01_hp*sal
    rhodt = 1000.0_hp * (cp1dt*clam1-cp1*cladt) / (alph0*cp1+clam1)**2
end function density_eckart
!
!
!==============================================================================
function density_unesco(temp, salt) result (rho)
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
!-------------------------------------------------------------------------------
    !
    ! Return value
    real(hp) :: rho
    !
    ! Global variables
    real(hp), intent(in)     :: temp
    real(hp), intent(in)     :: salt
!
! Local variables
!
    real(hp)                            :: s
    real(hp)                            :: sq
    real(hp)                            :: rhwa
    real(hp)                            :: asal
    real(hp)                            :: bsal
    real(hp), dimension(5)              :: t
    real(hp), dimension(0:5), parameter :: cf = &
    (/ 999.842594_hp   ,&
         6.793952e-2_hp  ,&
        -9.095290e-3_hp  ,&
         1.001685e-4_hp  ,&
        -1.120083e-6_hp  ,&
         6.536332e-9_hp  /)
    real(hp), dimension(0:4), parameter :: ca = &
    (/   8.24493e-1_hp ,&
        -4.0899e-3_hp  ,&
         7.6438e-5_hp  ,&
        -8.2467e-7_hp  ,&
         5.3875e-9_hp  /)
    real(hp), dimension(0:2), parameter :: cb = &
    (/  -5.72466e-3_hp  ,&
         1.0227e-4_hp   ,&
        -1.6546e-6_hp   /)
    real(hp)                , parameter :: csal = 4.8314e-4_hp
    real(hp)                            :: rhouns
    real(hp)                            :: rhods
    real(hp)                            :: rhodt
!
!! executable statements -------------------------------------------------------
!
    t(1)   = temp
    t(2)   = temp*t(1)
    t(3)   = temp*t(2)
    t(4)   = temp*t(3)
    t(5)   = temp*t(4)
    !
    s      = salt
    sq     = sqrt(max(0.0_hp,s))
    !
    rhwa   = cf(0) + cf(1)*t(1) + cf(2)*t(2) + cf(3)*t(3) + cf(4)*t(4) &
           &       + cf(5)*t(5)
    asal   = ca(0) + ca(1)*t(1) + ca(2)*t(2) + ca(3)*t(3) + ca(4)*t(4)
    bsal   = cb(0) + cb(1)*t(1) + cb(2)*t(2)
    !
    rhouns = rhwa + (asal+bsal*sq+csal*s) * s
    !
    rhods  = asal + 1.5_hp*bsal*sq + 2.0_hp*csal*s
    !
    rhodt  = cf(1) +  2.0_hp*cf(2)*t(1) + 3.0_hp*cf(3)*t(2) &
           &       +  4.0_hp*cf(4)*t(3) + 5.0_hp*cf(5)*t(4) &
           &       + (ca(1) + 2.0_hp*ca(2)*t(1) + 3.0_hp*ca(3)*t(2) &
           &       +  4.0_hp*ca(4)*t(3)) * s &
           &       + (cb(1) + 2.0_hp*cb(2)*t(1)) * sq * s 
    rho = rhouns
end function density_unesco
!
!
!==============================================================================
function density_nacl(temp, salt) result (rho)
!!--description-----------------------------------------------------------------
!
!    Function: Computes water density from temperature and
!              salinity using equation of state (rhowat).
!
! Method used: Equation of state following Millero/Delft Hydraulics
!              Valid for NaCl solutions
!
!-------------------------------------------------------------------------------
    !
    ! Return value
    real(hp) :: rho
    !
    ! Global variables
    real(hp), intent(in)     :: salt
    real(hp), intent(in)     :: temp
    !
    ! Local variables
    real(hp)   :: rhonacl
    real(hp)   :: rhods
    real(hp)   :: rhodt
!
! Local variables
!
!
!! executable statements -------------------------------------------------------
!
    rhonacl  = 999.904_hp                  + 4.8292E-2_hp * temp           - &
     &         7.2312E-3_hp * temp**2      + 2.9963E-5_hp * temp**3        + &
     &         7.6427E-1_hp * salt         - 3.1490E-3_hp * salt * temp    + &
     &         3.1273E-5_hp * salt * temp**2

    rhods    = 7.6427E-1_hp                - 3.1490E-3_hp * temp           + &
     &         2*3.1273E-5_hp * salt * temp

    rhodt    = 4.8292E-2_hp                - 2_hp*7.2312E-3_hp * temp      + &
     &         3_hp*2.9963E-5_hp * temp**2 - 3.1490E-3_hp * salt           + &
     &         2_hp*3.1273E-5_hp * salt * temp
    rho = rhonacl 
end function density_nacl

end module m_density
