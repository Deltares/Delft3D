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

!> Module for computing spatial and time-varying air density.
module m_airdensity
   use precision_basics, only: hp
   use physicalconsts, only: CtoKelvin

   implicit none

   private
   public :: get_airdensity

! physical constants
! from IFS documentation, https://www.ecmwf.int/en/publications/ifs-documentation ,
! part IV: PHYSICAL PROCESSES, Chapter 12
   real(kind=hp), parameter :: R = 8.31451_hp !< universal gas constant (J K-1 mol-1)
   real(kind=hp), parameter :: M_d = 28.9644e-3_hp !< dry air molar mass (kg mol-1)
   real(kind=hp), parameter :: M_v = 18.0153e-3_hp; !< water vapour molar mass (kg mol-1)
   real(kind=hp), parameter :: R_d = R / M_d !< gas constant for dry air (J kg-1 K-1)
   real(kind=hp), parameter :: R_v = R / M_v !< gas constant for water vapor (J kg-1 K-1)
   real(kind=hp), parameter :: eps_star = R_v / R_d - 1 !< modified ratio of R_d and R_v
   real(kind=hp), parameter :: T_0 = 273.16_hp !< triple point temperature [K]
   real(kind=hp), parameter :: e_0 = 611.21_hp !< water vapor saturation pressure over water [Pa] at T0
! part IV: PHYSICAL PROCESSES, Chapter 7
   real(kind=hp), parameter :: a_3 = 17.502_hp !< parameter for saturation over water, Buck (1981)
   real(kind=hp), parameter :: a_4 = 32.19_hp !< parameter for saturation over water, Buck (1981)

contains

!> Fills values array with air density computed as specified in
!! IFS documentation, part IV, section 12.6
   subroutine get_airdensity(p, T, T_dewpoint, air_density, ierror)
      use m_alloc, only: aerr
      use MessageHandling, only: mess, LEVEL_ERROR

      real(kind=hp), intent(in) :: p(:) !< total atmospheric pressure (Pa)
      real(kind=hp), intent(in) :: T(:) !< temperature [degrees_Celsius]
      real(kind=hp), intent(in) :: T_dewpoint(:) !< dewpoint [degrees_Celsius]
      real(kind=hp), intent(inout) :: air_density(:) !< air density [kg m-3]
      integer, intent(out) :: ierror !< error (1) or not (0)

      real(kind=hp), allocatable :: e_sat(:) !< water vapour saturation pressure
      real(kind=hp), allocatable :: q_v(:) !< specific humidity
      real(kind=hp), allocatable :: T_virtual(:) !< virtual temperature
      real(kind=hp), allocatable :: T_kelvin(:) !< temperature [K]
      real(kind=hp), allocatable :: Td_kelvin(:) !< temperature [K]

      integer :: nelem !< number of elements in array
      integer :: ierr !< error code

      nelem = size(air_density)
      if (nelem /= size(p) .or. nelem /= size(T)) then
         call mess(LEVEL_ERROR, 'Programming error, please report: Size of arrays in m_airdensity::get_airdensity do no match, computation of air density is not possible.')
         ierror = 1
         return
      end if

      allocate (e_sat(nelem), stat=ierr)
      call aerr('e_sat ', ierr, nelem)
      allocate (q_v(nelem), stat=ierr)
      call aerr('q_v ', ierr, nelem)
      allocate (T_virtual(nelem), stat=ierr)
      call aerr('T_virtual ', ierr, nelem)
      allocate (T_kelvin(nelem), stat=ierr)
      call aerr('T_kelvin ', ierr, nelem)
      allocate (Td_kelvin(nelem), stat=ierr)
      call aerr('Td_kelvin ', ierr, nelem)

      T_kelvin(1:nelem) = T(1:nelem) + CtoKelvin
      Td_kelvin(1:nelem) = T_dewpoint(1:nelem) + CtoKelvin

      call get_saturation_pressure
      call get_specific_humidity
      call get_virtual_temperature

      air_density(1:nelem) = p(1:nelem) / (R_d * T_virtual(1:nelem))

      deallocate (e_sat)
      deallocate (q_v)
      deallocate (T_virtual)
      deallocate (T_kelvin)
      deallocate (Td_kelvin)

      ierror = 0

   contains

!< returns water vapour saturation pressure over water [Pa]
!! cf section 7.2.1 of IFS documentation
      subroutine get_saturation_pressure
         e_sat(1:nelem) = e_0 * exp(a_3 * ((Td_kelvin(1:nelem) - T_0) / (Td_kelvin(1:nelem) - a_4)))
      end subroutine get_saturation_pressure

!< returns the specific humidity [g kg-1] =
!! ratio of the mass of water vapor to the total mass of air (dry air and water vapour combined)
      subroutine get_specific_humidity
         q_v(1:nelem) = e_sat(1:nelem) / (p(1:nelem) + eps_star * (p(1:nelem) - e_sat(1:nelem)))
      end subroutine get_specific_humidity

!< returns the temperature [K] of a moist air parcel at which a theoretical dry air parcel
!! would have a total pressure and density equal to the moist parcel of air
      subroutine get_virtual_temperature
         T_virtual(1:nelem) = T_Kelvin(1:nelem) * (1 + eps_star * q_v(1:nelem))
      end subroutine get_virtual_temperature

   end subroutine get_airdensity

end module m_airdensity
