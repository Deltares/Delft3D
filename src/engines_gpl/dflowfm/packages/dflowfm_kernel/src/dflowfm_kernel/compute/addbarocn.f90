!----- AGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2017-2026.
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

! "Baroclinic" here refers to density-driven pressure components.
! Variables like baroclinic_pressures are intermediate values. They are converted to physical pressures
! later by multiplying with gravity (ag) in baroclinic_pressure_link_time_integration (addbarocl.f90).

module m_add_baroclinic_pressure_cell
   use precision, only: dp
   use m_density_parameters, only: thermobaricity_in_pressure_gradient
   use m_turbulence, only: in_situ_density, potential_density

   implicit none

   private

   real(kind=dp), parameter :: MIN_BAROCLINIC_PRESSURE = 1e-10_dp ! Small value (to avoid zero) at cells with small waterdepth
   real(kind=dp), dimension(:), pointer :: density ! local pointer

   public :: add_baroclinic_pressure_cell_original, add_baroclinic_pressure_cell, &
             add_baroclinic_pressure_cell_interface, add_baroclinic_pressure_cell_use_rho_directly

contains

   !> Computes baroclinic pressure gradients across layers for a horizontal cell.
   !! Original method that was used when Baroczlaybed was set to 0.
   !! Uses linear interpolation of density at vertical interfaces and precomputes the density at the layer interfaces.
   !! This method is used for backward compatibility with existing models, but the other methods are investigated as more accurate alternatives.
   subroutine add_baroclinic_pressure_cell_original(cell_index_2d)
      use m_turbulence, only: integrated_baroclinic_pressures, baroclinic_pressures, kmxx
      use m_flowparameters, only: epshu
      use m_flow, only: zws
      use m_physcoef, only: rhomean
      use m_get_kbot_ktop, only: getkbotktop

      integer, intent(in) :: cell_index_2d !< horizontal cell index

      integer :: cell_index_3d, k_bot, k_top
      real(kind=dp) :: baroclinic_pressure_up, baroclinic_pressure_down, integrated_baroclinic_pressure, delta_z
      real(kind=dp) :: weight_up, weight_down, baroclinic_pressure, vertical_density_difference
      real(kind=dp) :: density_at_layer_interface(0:kmxx)

      call getkbotktop(cell_index_2d, k_bot, k_top)
      if (zws(k_top) - zws(k_bot - 1) < epshu) then
         baroclinic_pressures(k_bot:k_top) = MIN_BAROCLINIC_PRESSURE
         integrated_baroclinic_pressures(k_bot:k_top) = 0.0_dp
         return
      end if

      ! Associate density with the potential density or in-situ density
      if (thermobaricity_in_pressure_gradient) then
         density => in_situ_density
      else
         density => potential_density
      end if

      if (k_top > k_bot) then
         do cell_index_3d = k_bot, k_top - 1
            weight_down = (zws(cell_index_3d + 1) - zws(cell_index_3d)) / (zws(cell_index_3d + 1) - zws(cell_index_3d - 1))
            weight_up = 1.0_dp - weight_down
            density_at_layer_interface(cell_index_3d - k_bot + 1) = weight_up * density(cell_index_3d + 1) + weight_down * density(cell_index_3d) - rhomean
         end do
         density_at_layer_interface(0) = 2.0_dp * (density(k_bot) - rhomean) - density_at_layer_interface(1)
         density_at_layer_interface(k_top - k_bot + 1) = 2.0_dp * (density(k_top) - rhomean) - density_at_layer_interface(k_top - k_bot)
      else
         density_at_layer_interface(0) = density(k_bot) - rhomean
         density_at_layer_interface(1) = density_at_layer_interface(0)
      end if

      baroclinic_pressures(k_top) = 0.0_dp
      integrated_baroclinic_pressures(k_top) = 0.0_dp
      baroclinic_pressure_down = 0.0_dp
      baroclinic_pressure = 0.0_dp
      do cell_index_3d = k_top, k_bot, -1
         delta_z = zws(cell_index_3d) - zws(cell_index_3d - 1)
         baroclinic_pressure = 0.5_dp * (density_at_layer_interface(cell_index_3d - k_bot + 1) + density_at_layer_interface(cell_index_3d - k_bot)) * delta_z
         baroclinic_pressure_up = baroclinic_pressure_down
         baroclinic_pressure_down = baroclinic_pressure_up + baroclinic_pressure
         baroclinic_pressures(cell_index_3d) = baroclinic_pressure_down
         vertical_density_difference = density_at_layer_interface(cell_index_3d - k_bot) - density_at_layer_interface(cell_index_3d - k_bot + 1)
         baroclinic_pressure_down = baroclinic_pressure_up + density_at_layer_interface(cell_index_3d - k_bot + 1) * delta_z + 0.5_dp * vertical_density_difference * delta_z
         integrated_baroclinic_pressure = baroclinic_pressure_up * delta_z + 0.5_dp * density_at_layer_interface(cell_index_3d - k_bot + 1) * delta_z * delta_z + vertical_density_difference * delta_z * delta_z / 6.0_dp ! your left  wall
         integrated_baroclinic_pressures(cell_index_3d) = integrated_baroclinic_pressure
      end do
   end subroutine add_baroclinic_pressure_cell_original

   !> Computes baroclinic pressure gradients across layers for a horizontal cell.
   !! Density is based on linear interpolation of density at vertical interfaces.
   subroutine add_baroclinic_pressure_cell(cell_index_2d)
      use m_turbulence, only: integrated_baroclinic_pressures, baroclinic_pressures
      use m_flowparameters, only: epshu
      use m_flow, only: zws
      use m_physcoef, only: rhomean
      use m_get_kbot_ktop, only: getkbotktop

      integer, intent(in) :: cell_index_2d !< horizontal cell index

      integer :: cell_index_3d, k_bot, k_top
      real(kind=dp) :: baroclinic_pressure_up, baroclinic_pressure_down, integrated_baroclinic_pressure, delta_z
      real(kind=dp) :: rho_up_weight_up, rho_up_weight_down, rho_down_weight_up, rho_down_weight_down, delta_z_up, delta_z_down, rho_up, rho_down, baroclinic_pressure

      call getkbotktop(cell_index_2d, k_bot, k_top)
      if (zws(k_top) - zws(k_bot - 1) < epshu) then
         baroclinic_pressures(k_bot:k_top) = MIN_BAROCLINIC_PRESSURE
         integrated_baroclinic_pressures(k_bot:k_top) = 0.0_dp
         return
      end if

      ! Associate density with the potential density or in-situ density
      if (thermobaricity_in_pressure_gradient) then
         density => in_situ_density
      else
         density => potential_density
      end if

      baroclinic_pressures(k_top) = 0.0_dp
      integrated_baroclinic_pressures(k_top) = 0.0_dp
      baroclinic_pressure_down = 0.0_dp
      do cell_index_3d = k_top, k_bot, -1
         delta_z = zws(cell_index_3d) - zws(cell_index_3d - 1)
         if (k_bot == k_top) then
            rho_up = density(cell_index_3d) - rhomean
            rho_down = rho_up
         else if (cell_index_3d > k_bot .and. cell_index_3d < k_top) then
            delta_z_up = zws(cell_index_3d + 1) - zws(cell_index_3d)
            delta_z_down = zws(cell_index_3d - 1) - zws(cell_index_3d - 2)
            rho_up_weight_down = delta_z_up / (delta_z_up + delta_z)
            rho_up_weight_up = 1.0_dp - rho_up_weight_down
            rho_down_weight_down = delta_z / (delta_z_down + delta_z)
            rho_down_weight_up = 1.0_dp - rho_down_weight_down
            rho_up = rho_up_weight_up * density(cell_index_3d + 1) + rho_up_weight_down * density(cell_index_3d) - rhomean
            rho_down = rho_down_weight_up * density(cell_index_3d) + rho_down_weight_down * density(cell_index_3d - 1) - rhomean
         else if (cell_index_3d == k_bot) then
            delta_z_up = zws(cell_index_3d + 1) - zws(cell_index_3d)
            rho_up_weight_down = delta_z_up / (delta_z_up + delta_z)
            rho_up_weight_up = 1.0_dp - rho_up_weight_down
            rho_up = rho_up_weight_up * density(cell_index_3d + 1) + rho_up_weight_down * density(cell_index_3d) - rhomean
            rho_down = 2.0_dp * (density(cell_index_3d) - rhomean) - rho_up
         else if (cell_index_3d == k_top) then
            delta_z_down = zws(cell_index_3d - 1) - zws(cell_index_3d - 2)
            rho_down_weight_down = delta_z / (delta_z_down + delta_z)
            rho_down_weight_up = 1.0_dp - rho_down_weight_down
            rho_down = rho_down_weight_up * density(cell_index_3d) + rho_down_weight_down * density(cell_index_3d - 1) - rhomean
            rho_up = 2.0_dp * (density(cell_index_3d) - rhomean) - rho_down
         end if
         baroclinic_pressure = 0.5_dp * (rho_up + rho_down) * delta_z
         baroclinic_pressure_up = baroclinic_pressure_down
         baroclinic_pressure_down = baroclinic_pressure_up + baroclinic_pressure
         baroclinic_pressures(cell_index_3d) = baroclinic_pressure_down
         integrated_baroclinic_pressure = baroclinic_pressure_up * delta_z + 0.5_dp * ((2.0_dp * rho_up + rho_down) / 3.0_dp) * delta_z * delta_z ! your left  wall
         integrated_baroclinic_pressures(cell_index_3d) = integrated_baroclinic_pressure
      end do
   end subroutine add_baroclinic_pressure_cell

   !> Computes baroclinic pressure gradients across layers for a horizontal cell.
   !! Density is based on linear interpolation of recomputed density (from salinity, temperature (and pressure)) at vertical interfaces.
   subroutine add_baroclinic_pressure_cell_interface(cell_index_2d)
      use m_turbulence, only: integrated_baroclinic_pressures, baroclinic_pressures, kmxx, rhosww
      use m_flowparameters, only: epshu
      use m_flow, only: zws
      use m_transport, only: ISALT, ITEMP, constituents
      use m_physcoef, only: rhomean, ag
      use m_density_parameters, only: max_iterations_pressure_density, apply_thermobaricity
      use m_get_kbot_ktop, only: getkbotktop
      use m_density, only: calculate_density

      integer, intent(in) :: cell_index_2d !< horizontal cell index

      integer :: cell_index_3d, k_bot, k_top, i
      real(kind=dp) :: salinity_at_interface(0:kmxx), temperature_at_interface(0:kmxx) ! salinity and temperature at pressure point layer interfaces
      real(kind=dp) :: weight_up, weight_down, baroclinic_pressure_up, baroclinic_pressure_down, delta_z, total_pressure, barotropic_pressure, density_anomaly

      call getkbotktop(cell_index_2d, k_bot, k_top)
      if (zws(k_top) - zws(k_bot - 1) < epshu) then
         integrated_baroclinic_pressures(k_bot:k_top) = 0.0_dp
         baroclinic_pressures(k_bot:k_top) = MIN_BAROCLINIC_PRESSURE
         return
      end if

      if (k_top > k_bot) then
         do cell_index_3d = k_bot, k_top - 1
            weight_down = (zws(cell_index_3d + 1) - zws(cell_index_3d)) / (zws(cell_index_3d + 1) - zws(cell_index_3d - 1))
            weight_up = 1.0_dp - weight_down
            salinity_at_interface(cell_index_3d - k_bot + 1) = weight_up * constituents(isalt, cell_index_3d + 1) + weight_down * constituents(isalt, cell_index_3d)
            temperature_at_interface(cell_index_3d - k_bot + 1) = weight_up * constituents(itemp, cell_index_3d + 1) + weight_down * constituents(itemp, cell_index_3d)
         end do
         salinity_at_interface(0) = 2.0_dp * constituents(isalt, k_bot) - salinity_at_interface(1)
         temperature_at_interface(0) = 2.0_dp * constituents(itemp, k_bot) - temperature_at_interface(1)
         salinity_at_interface(k_top - k_bot + 1) = 2.0_dp * constituents(isalt, k_top) - salinity_at_interface(k_top - k_bot)
         temperature_at_interface(k_top - k_bot + 1) = 2.0_dp * constituents(itemp, k_top) - temperature_at_interface(k_top - k_bot)
      else
         salinity_at_interface(0) = constituents(isalt, k_bot)
         temperature_at_interface(0) = constituents(itemp, k_bot)
         salinity_at_interface(1) = salinity_at_interface(0)
         temperature_at_interface(1) = temperature_at_interface(0)
      end if

      ! Associate density with the potential density or in-situ density
      if (thermobaricity_in_pressure_gradient) then
         density => in_situ_density
      else
         density => potential_density
      end if

      baroclinic_pressure_up = 0.0_dp
      barotropic_pressure = 0.0_dp

      rhosww(k_top) = calculate_density(salinity_at_interface(k_top - k_bot + 1), temperature_at_interface(k_top - k_bot + 1)) - rhomean ! density at interface

      do cell_index_3d = k_top, k_bot, -1
         delta_z = zws(cell_index_3d) - zws(cell_index_3d - 1)
         baroclinic_pressure_up = baroclinic_pressure_down
         if (.not. apply_thermobaricity) then
            rhosww(cell_index_3d - 1) = calculate_density(salinity_at_interface(cell_index_3d - k_bot), temperature_at_interface(cell_index_3d - k_bot)) - rhomean
         else
            barotropic_pressure = barotropic_pressure + rhomean * delta_z
            do i = 1, max_iterations_pressure_density
               baroclinic_pressure_down = baroclinic_pressure_up + 0.5_dp * (rhosww(cell_index_3d) + rhosww(cell_index_3d - 1)) * delta_z ! start with previous step estimate
               total_pressure = ag * (baroclinic_pressure_down + barotropic_pressure)
               rhosww(cell_index_3d - 1) = calculate_density(salinity_at_interface(cell_index_3d - k_bot), temperature_at_interface(cell_index_3d - k_bot), total_pressure) - rhomean
            end do
         end if
         density_anomaly = 0.5_dp * (rhosww(cell_index_3d) + rhosww(cell_index_3d - 1))
         density(cell_index_3d) = rhomean + density_anomaly
         baroclinic_pressure_down = baroclinic_pressure_up + delta_z * density_anomaly
         baroclinic_pressures(cell_index_3d) = baroclinic_pressure_down
         integrated_baroclinic_pressures(cell_index_3d) = (baroclinic_pressure_up + 0.5_dp * delta_z * (2.0_dp * rhosww(cell_index_3d) + rhosww(cell_index_3d - 1)) / 3.0_dp) * delta_z ! wall contribution
      end do
   end subroutine add_baroclinic_pressure_cell_interface

   !> Computes baroclinic pressure gradients across layers for a horizontal cell.
   !! Cell density (i.e. rho(cell_index_3d)) is used
   subroutine add_baroclinic_pressure_cell_use_rho_directly(cell_index_2d)
      use m_turbulence, only: integrated_baroclinic_pressures, baroclinic_pressures
      use m_flowparameters, only: epshu
      use m_flow, only: zws
      use m_physcoef, only: rhomean
      use m_get_kbot_ktop, only: getkbotktop

      integer, intent(in) :: cell_index_2d !< horizontal cell index

      integer :: cell_index_3d, k_bot, k_top
      real(kind=dp) :: baroclinic_pressure_up, baroclinic_pressure_down, integrated_baroclinic_pressure, delta_z, baroclinic_pressure

      call getkbotktop(cell_index_2d, k_bot, k_top)
      if (zws(k_top) - zws(k_bot - 1) < epshu) then
         baroclinic_pressures(k_bot:k_top) = MIN_BAROCLINIC_PRESSURE
         integrated_baroclinic_pressures(k_bot:k_top) = 0.0_dp
         return
      end if

      ! Associate density with the potential density or in-situ density
      if (thermobaricity_in_pressure_gradient) then
         density => in_situ_density
      else
         density => potential_density
      end if

      baroclinic_pressures(k_top) = 0.0_dp
      integrated_baroclinic_pressures(k_top) = 0.0_dp
      baroclinic_pressure_down = 0.0_dp
      do cell_index_3d = k_top, k_bot, -1
         delta_z = zws(cell_index_3d) - zws(cell_index_3d - 1)
         baroclinic_pressure = (density(cell_index_3d) - rhomean) * delta_z
         baroclinic_pressure_up = baroclinic_pressure_down
         baroclinic_pressure_down = baroclinic_pressure_up + baroclinic_pressure
         baroclinic_pressures(cell_index_3d) = baroclinic_pressure_down
         integrated_baroclinic_pressure = baroclinic_pressure_up * delta_z + 0.5_dp * (density(cell_index_3d) - rhomean) * delta_z * delta_z ! your left  wall
         integrated_baroclinic_pressures(cell_index_3d) = integrated_baroclinic_pressure
      end do
   end subroutine add_baroclinic_pressure_cell_use_rho_directly
end module m_add_baroclinic_pressure_cell
