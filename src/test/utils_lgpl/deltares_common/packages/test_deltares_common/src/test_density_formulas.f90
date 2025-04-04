!!  Copyright (C)  Stichting Deltares, 2025-2025.
!!
!!  This program is free software: you can redistribute it and/or modify
!!  it under the terms of the GNU General Public License version 3,
!!  as published by the Free Software Foundation.
!!
!!  This program is distributed in the hope that it will be useful,
!!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
!!  GNU General Public License for more details.
!!
!!  You should have received a copy of the GNU General Public License
!!  along with this program. If not, see <http://www.gnu.org/licenses/>.
!!
!!  contact: delft3d.support@deltares.nl
!!  Stichting Deltares
!!  P.O. Box 177
!!  2600 MH Delft, The Netherlands
!!
!!  All indications and logos of, and references to registered trademarks
!!  of Stichting Deltares remain the property of Stichting Deltares. All
!!  rights reserved.
module test_density_formulas
   use ftnunit
   use precision, only: dp

   implicit none
   private

   public :: tests_density_formulas

   real(dp), parameter :: tolerance = 1.0e-4_dp

contains

   subroutine tests_density_formulas
      call test(test_unesco_83, 'Test computation of water density using UNESCO 83 description.')
   end subroutine tests_density_formulas

   !> tests computation of varying air density
   subroutine test_unesco_83
      use precision, only: dp
      use m_density_formulas, only: calculate_density_unesco83
      real(kind=dp) :: salinity, temperature, pressure

      salinity = 30.0_dp
      temperature = 30.0_dp
      pressure = 0.0_dp
      call assert_comparable(calculate_density_unesco83(salinity, temperature, pressure), 1017.9854_dp, tolerance, '1')

      salinity = 30.0_dp
      temperature = 30.0_dp
      pressure = 1e5_dp
      call assert_comparable(calculate_density_unesco83(salinity, temperature, pressure), 1018.0283_dp, tolerance, '2')

      salinity = 8.0_dp
      temperature = 10.0_dp
      pressure = 1e6_dp
      call assert_comparable(calculate_density_unesco83(salinity, temperature, pressure), 1006.4180_dp, tolerance, '3')

      salinity = 0.0_dp
      temperature = 0.0_dp
      pressure = 0.0_dp
      call assert_comparable(calculate_density_unesco83(salinity, temperature, pressure), 999.8426_dp, tolerance, '4')

      salinity = 0.0_dp
      temperature = 0.0_dp
      pressure = 1e8_dp
      call assert_comparable(calculate_density_unesco83(salinity, temperature, pressure), 1045.3371_dp, tolerance, '5')

      salinity = 40.0_dp
      temperature = 0.0_dp
      pressure = 0.0_dp
      call assert_comparable(calculate_density_unesco83(salinity, temperature, pressure), 1032.1471_dp, tolerance, '6')

      salinity = 40.0_dp
      temperature = 0.0_dp
      pressure = 1e8_dp
      call assert_comparable(calculate_density_unesco83(salinity, temperature, pressure), 1074.6498_dp, tolerance, '7')

      salinity = 0.0_dp
      temperature = 40.0_dp
      pressure = 0.0_dp
      call assert_comparable(calculate_density_unesco83(salinity, temperature, pressure), 992.2204_dp, tolerance, '8')

      salinity = 0.0_dp
      temperature = 40.0_dp
      pressure = 1e8_dp
      call assert_comparable(calculate_density_unesco83(salinity, temperature, pressure), 1031.9486_dp, tolerance, '9')

      salinity = 40.0_dp
      temperature = 40.0_dp
      pressure = 0.0_dp
      call assert_comparable(calculate_density_unesco83(salinity, temperature, pressure), 1021.6788_dp, tolerance, '10')

      salinity = 40.0_dp
      temperature = 40.0_dp
      pressure = 1e8_dp
      call assert_comparable(calculate_density_unesco83(salinity, temperature, pressure), 1059.8204_dp, tolerance, '11')
   end subroutine test_unesco_83
end module test_density_formulas
