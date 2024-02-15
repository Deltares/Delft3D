!----- AGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2017-2024.                                
!                                                                               
!  This file is part of Delft3D (D-Flow Flexible Mesh component).               
!                                                                               
!  Delft3D is free software: you can redistribute it and/or modify              
!  it under the terms of the GNU Affero General Public License as               
!  published by the Free Software Foundation version 3.                         
!                                                                               
!  Delft3D  is distributed in the hope that it will be useful,                  
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                
!  GNU Affero General Public License for more details.                          
!                                                                               
!  You should have received a copy of the GNU Affero General Public License     
!  along with Delft3D.  If not, see <http://www.gnu.org/licenses/>.             
!                                                                               
!  contact: delft3d.support@deltares.nl                                         
!  Stichting Deltares                                                           
!  P.O. Box 177                                                                 
!  2600 MH Delft, The Netherlands                                               
!                                                                               
!  All indications and logos of, and references to, "Delft3D",                  
!  "D-Flow Flexible Mesh" and "Deltares" are registered trademarks of Stichting 
!  Deltares, and remain the property of Stichting Deltares. All rights reserved.
!                                                                               
!-------------------------------------------------------------------------------

! 
! 

 module m_heatfluxes
 use physicalconsts
 implicit none

 double precision             :: albedo             ! reflection coefficient of water () at average incidence angle of 60 deg,
                                                    ! (albedo is .025 at angle 0 deg, 0.13 at angle 70 deg)
 double precision             :: em                 ! Emissivity ()
 double precision             :: cpa                ! Specific heat air   [J/kg/K]
 double precision             :: rcpa               !
 double precision             :: cpw                ! Specific heat water [J/kg/K]
 double precision             :: rcpi               ! m3K/J
 double precision             :: emstf              ! Em*Stf [W/m^2/K^4]
 double precision, parameter  :: tkelvn = CtoKelvin ! Absolute zero

 double precision                  :: QSUNav          ! Solar influx              (W/m2)
 double precision                  :: QEVAav          ! Evaporative heat loss     (W/m2)
 double precision                  :: QCONav          ! Convective heat loss      (W/m2)
 double precision                  :: QLongav         ! Long wave back radiation  (W/m2)
 double precision                  :: Qfreeav         ! Free conv + evap heat loss (W/m2)
 double precision                  :: Qfrconav        ! Free convection heat loss (W/m2)
 double precision                  :: Qfrevaav        ! Free evaporation heat loss (W/m2)

 double precision                  :: sarea           ! Only for excess temp model jatem=3, lake area
 double precision                  :: fwind           ! Only for excess temp model jatem=3, wind factor

 integer                           :: jamapheatflux   !< write heatfluxes to map
 integer                           :: jaRichardsononoutput !< write Richardson nr to his
 integer                           :: jaSecchisp           !< Spatial Secchi 0,1
 integer                           :: jaRoro               !< Use roair(n)/rho(ntop) in windstress 0,1

 double precision, allocatable, target :: Qsunmap(:) !< [W/m2] solar radiation reaching water surface {"location": "face", "shape": ["ndx"]}
 double precision, allocatable     :: Qevamap(:)
 double precision, allocatable     :: Qconmap(:)
 double precision, allocatable     :: Qlongmap(:)
 double precision, allocatable     :: Qfrevamap(:)
 double precision, allocatable     :: Qfrconmap(:)
 double precision, allocatable     :: Qtotmap(:)

 double precision, allocatable     :: Rich(:)
 double precision, allocatable     :: Secchisp(:)
 double precision, allocatable     :: Roair(:)

contains

subroutine default_heatfluxes()
use m_physcoef, only : rhomean
use m_wind    , only : rhoair
                                      !< Heat flux model constants
albedo  = 0.06d0                      !< reflection coefficient of water () at average incidence angle of 60 deg,
                                      !< (albedo is .025 at angle 0 deg, 0.13 at angle 70 deg)
em      = 0.985d0                     !< Emissivity ()
cpa     = 1004d0                      !< Specific heat air   [J/kg/K]
rcpa    = rhoair*cpa                  !
cpw     = 3986d0                      !< Specific heat water [J/kg/K]
rcpi    = 1d0/(rhomean*cpw)           !< [m3K/J] or mKs2/kg
emstf   = em*stf

jamapheatflux = 0
jaRichardsononoutput = 0
jaroro = 0

end subroutine default_heatfluxes


end module m_heatfluxes
