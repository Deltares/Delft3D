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

module m_add_baroclinic_pressure_link
   use precision, only: dp
   use m_density_parameters, only: thermobaricity_in_pressure_gradient
   use m_turbulence, only: in_situ_density, potential_density

   implicit none

   private

   real(kind=dp), parameter :: MIN_LAYER_THICKNESS = 0.1_dp ! Minimum layer thickness for baroclinic pressure calculation
   real(kind=dp), dimension(:), pointer :: density ! local pointer

   public :: add_baroclinic_pressure_link_original, add_baroclinic_pressure_link, &
             add_baroclinic_pressure_link_interface, add_baroclinic_pressure_link_use_rho_directly

contains

   !> Computes baroclinic pressure gradients across layers for a horizontal link.
   !! Original method that was used when Baroczlaybed was set to 0.
   subroutine add_baroclinic_pressure_link_original(link_index_2d, l_bot, l_top)
      use m_turbulence, only: kmxx, rhou, baroclinic_pressures, integrated_baroclinic_pressures
      use m_flowgeom, only: ln, dx
      use m_flow, only: zws, ktop
      use m_flowparameters, only: jarhoxu

      integer, intent(in) :: link_index_2d !< Horizontal link index
      integer, intent(in) :: l_bot !< bottom link
      integer, intent(in) :: l_top !< top link

      integer :: link_index_3d, k1, k2, k1t, k2t
      real(kind=dp) :: baroclinic_pressure_gradients(kmxx), volume_averaged_density(kmxx), baroclinic_pressure3

      ! Associate density with the potential density or in-situ density
      if (thermobaricity_in_pressure_gradient) then
         density => in_situ_density
      else
         density => potential_density
      end if

      do link_index_3d = l_bot, l_top
         k1 = ln(1, link_index_3d)
         k1t = k1
         k2 = ln(2, link_index_3d)
         k2t = k2
         if (link_index_3d == l_top) then
            k1t = ktop(ln(1, link_index_2d))
            k2t = ktop(ln(2, link_index_2d))
         end if

         volume_averaged_density(link_index_3d - l_bot + 1) = 0.5_dp * (zws(k1t) - zws(k1 - 1) + zws(k2t) - zws(k2 - 1)) * dx(link_index_2d) * 0.5_dp * (density(k1) + density(k2))
         if (jarhoxu > 0) then
            rhou(link_index_3d) = 0.5_dp * (density(k1) + density(k2))
         end if

         baroclinic_pressure3 = 0.5_dp * (baroclinic_pressures(k1) + baroclinic_pressures(k2)) * (zws(k1 - 1) - zws(k2 - 1))
         baroclinic_pressure_gradients(link_index_3d - l_bot + 1) = integrated_baroclinic_pressures(k1) - integrated_baroclinic_pressures(k2) + baroclinic_pressure3
         if (link_index_3d > l_bot) then
            baroclinic_pressure_gradients(link_index_3d - l_bot) = baroclinic_pressure_gradients(link_index_3d - l_bot) - baroclinic_pressure3 ! ceiling of cell below
         end if
      end do

      call baroclinic_pressure_link_time_integration(baroclinic_pressure_gradients, volume_averaged_density, link_index_2d, l_bot, l_top)
   end subroutine add_baroclinic_pressure_link_original

   !> Computes baroclinic pressure gradients across layers for a horizontal link.
   !! Density is based on linear interpolation of density at vertical interfaces.
   subroutine add_baroclinic_pressure_link(link_index_2d, l_bot, l_top)
      use m_turbulence, only: kmxx, rhou, baroclinic_pressures, integrated_baroclinic_pressures
      use m_flowgeom, only: ln, dx
      use m_flow, only: zws, numtopsig, kmxn, ktop, layertype, LAYTP_Z
      use m_flowparameters, only: jarhoxu
      use m_physcoef, only: rhomean

      integer, intent(in) :: link_index_2d !< Horizontal link index
      integer, intent(in) :: l_bot !< bottom link
      integer, intent(in) :: l_top !< top link

      integer, parameter :: LEFT = 1, RIGHT = 2, EQUAL = 0
      integer :: link_index_3d, k1, k2, k1t, k2t, k_deep_side, k_top_deep_side, k_shallow_side, k_top_shallow_side, more_active_layers_on_the
      logical :: in_sigma_part
      real(kind=dp) :: baroclinic_pressure_gradients(kmxx), volume_averaged_density(kmxx), baroclinic_pressure3
      real(kind=dp) :: baroclinic_pressure1, baroclinic_pressure2, integrated_baroclinic_pressure1, integrated_baroclinic_pressure2, baroclinic_pressure, integrated_baroclinic_pressure, weight_up, weight_down, delta_z, rho_down, rho_up

      baroclinic_pressure_gradients(1:l_top - l_bot + 1) = 0.0_dp

      if (zws(ln(1, l_top)) - zws(ln(1, l_bot)) < MIN_LAYER_THICKNESS .or. zws(ln(2, l_top)) - zws(ln(2, l_bot)) < MIN_LAYER_THICKNESS) then
         return
      end if

      ! Associate density with the potential density or in-situ density
      if (thermobaricity_in_pressure_gradient) then
         density => in_situ_density
      else
         density => potential_density
      end if

      in_sigma_part = .false.
      if (layertype == LAYTP_Z .and. numtopsig > 0) then
         if (kmxn(ln(1, link_index_2d)) <= numtopsig .or. kmxn(ln(2, link_index_2d)) <= numtopsig) then
            in_sigma_part = .true. ! one of the nodes is in the sigma part
         end if
      end if

      if (kmxn(ln(1, link_index_2d)) > kmxn(ln(2, link_index_2d))) then
         more_active_layers_on_the = LEFT
      else if (kmxn(ln(1, link_index_2d)) < kmxn(ln(2, link_index_2d))) then
         more_active_layers_on_the = RIGHT
      else
         more_active_layers_on_the = EQUAL
      end if

      do link_index_3d = l_top, l_bot, -1
         k1 = ln(1, link_index_3d)
         k1t = k1
         k2 = ln(2, link_index_3d)
         k2t = k2
         if (link_index_3d == l_top) then
            k1t = ktop(ln(1, link_index_2d))
            k2t = ktop(ln(2, link_index_2d))
         end if

         volume_averaged_density(link_index_3d - l_bot + 1) = 0.5_dp * ((zws(k1t) - zws(k1 - 1)) * density(k1) + (zws(k2t) - zws(k2 - 1)) * density(k2))
         if (jarhoxu > 0) then
            rhou(link_index_3d) = volume_averaged_density(link_index_3d - l_bot + 1) / (0.5_dp * (zws(k1t) - zws(k1 - 1) + zws(k2t) - zws(k2 - 1)))
         end if
         volume_averaged_density(link_index_3d - l_bot + 1) = volume_averaged_density(link_index_3d - l_bot + 1) * dx(link_index_2d)

         baroclinic_pressure1 = baroclinic_pressures(k1)
         baroclinic_pressure2 = baroclinic_pressures(k2)
         integrated_baroclinic_pressure1 = integrated_baroclinic_pressures(k1)
         integrated_baroclinic_pressure2 = integrated_baroclinic_pressures(k2)

         if (link_index_3d == l_bot .and. more_active_layers_on_the /= EQUAL) then ! extrapolate at 'bed' layer of deepest side

            if (more_active_layers_on_the == LEFT) then
               k_deep_side = k1
               k_top_deep_side = ktop(ln(1, link_index_2d))
               k_shallow_side = k2
               k_top_shallow_side = ktop(ln(2, link_index_2d))
            else
               k_deep_side = k2
               k_top_deep_side = ktop(ln(2, link_index_2d))
               k_shallow_side = k1
               k_top_shallow_side = ktop(ln(1, link_index_2d))
            end if

            if (k_top_shallow_side - k_shallow_side > 0) then ! Shallow side extrapolates, coeffs based on shallow side:
               weight_down = (zws(k_shallow_side + 1) - zws(k_shallow_side)) / (zws(k_shallow_side + 1) - zws(k_shallow_side - 1))
               weight_up = 1.0_dp - weight_down
               rho_up = weight_up * density(k_deep_side + 1) + weight_down * density(k_deep_side)
               rho_down = 2.0_dp * density(k_deep_side) - rho_up
            else ! one layer
               rho_up = density(k_deep_side)
               rho_down = rho_up
            end if
            rho_up = rho_up - rhomean
            rho_down = rho_down - rhomean

            if (.not. in_sigma_part) then
               delta_z = zws(k_shallow_side) - zws(k_shallow_side - 1)
               volume_averaged_density(1) = delta_z * 0.5_dp * (density(k_deep_side) + density(k_shallow_side)) * dx(link_index_2d)
               if (jarhoxu > 0) then
                  rhou(link_index_3d) = 0.5_dp * (density(k_deep_side) + density(k_shallow_side))
               end if
            else
               delta_z = zws(k_deep_side) - zws(k_deep_side - 1)
            end if

            baroclinic_pressure = baroclinic_pressures(k_deep_side + 1) + 0.5_dp * delta_z * (rho_up + rho_down)
            integrated_baroclinic_pressure = (baroclinic_pressures(k_deep_side + 1) + 0.5_dp * delta_z * (2.0_dp * rho_up + rho_down) / 3.0_dp) * delta_z

            if (more_active_layers_on_the == LEFT) then ! k1=deepest
               baroclinic_pressure1 = baroclinic_pressure
               integrated_baroclinic_pressure1 = integrated_baroclinic_pressure
            else
               baroclinic_pressure2 = baroclinic_pressure
               integrated_baroclinic_pressure2 = integrated_baroclinic_pressure
            end if

            if (.not. in_sigma_part) then
               baroclinic_pressure3 = 0.0_dp ! no skewness for zlay jump at bed
            else
               baroclinic_pressure3 = 0.5_dp * (baroclinic_pressure1 + baroclinic_pressure2) * (zws(k1 - 1) - zws(k2 - 1))
            end if

         else
            baroclinic_pressure3 = 0.5_dp * (baroclinic_pressure1 + baroclinic_pressure2) * (zws(k1 - 1) - zws(k2 - 1))
         end if

         baroclinic_pressure_gradients(link_index_3d - l_bot + 1) = baroclinic_pressure_gradients(link_index_3d - l_bot + 1) + integrated_baroclinic_pressure1 - integrated_baroclinic_pressure2 + baroclinic_pressure3
         if (link_index_3d > l_bot) then
            baroclinic_pressure_gradients(link_index_3d - l_bot) = baroclinic_pressure_gradients(link_index_3d - l_bot) - baroclinic_pressure3 ! ceiling of cell below
         end if
      end do

      call baroclinic_pressure_link_time_integration(baroclinic_pressure_gradients, volume_averaged_density, link_index_2d, l_bot, l_top)
   end subroutine add_baroclinic_pressure_link

   !> Computes baroclinic pressure gradients across layers for a horizontal link.
   !! Density is based on linear interpolation of recomputed density (from salinity, temperature (and pressure)) at vertical interfaces.
   subroutine add_baroclinic_pressure_link_interface(link_index_2d, l_bot, l_top)
      use m_turbulence, only: kmxx, rhou, baroclinic_pressures, integrated_baroclinic_pressures, rhosww
      use m_flowgeom, only: ln, dx
      use m_flow, only: zws, numtopsig, kmxn, ktop, layertype, LAYTP_Z
      use m_flowparameters, only: jarhoxu
      use m_transport, only: ISALT, ITEMP, constituents
      use m_physcoef, only: rhomean, ag
      use m_density_parameters, only: max_iterations_pressure_density, apply_thermobaricity
      use m_density, only: calculate_density

      integer, intent(in) :: link_index_2d !< Horizontal link index
      integer, intent(in) :: l_bot !< bottom link
      integer, intent(in) :: l_top !< top link

      integer, parameter :: LEFT = 1, RIGHT = 2, EQUAL = 0
      integer :: link_index_3d, k1, k2, k1t, k2t, k_deep_side, k_top_deep_side, k_shallow_side, k_top_shallow_side, more_active_layers_on_the, i
      logical :: in_sigma_part
      real(kind=dp) :: baroclinic_pressure_gradients(kmxx), volume_averaged_density(kmxx), baroclinic_pressure3
      real(kind=dp) :: baroclinic_pressure1, baroclinic_pressure2, integrated_baroclinic_pressure1, integrated_baroclinic_pressure2, baroclinic_pressure, integrated_baroclinic_pressure, salinity_down, salinity_up, temperature_down, temperature_up, weight_up, weight_down, delta_z, rho_down, rho_up, barotropic_pressure, total_pressure

      baroclinic_pressure_gradients(1:l_top - l_bot + 1) = 0.0_dp

      if (zws(ln(1, l_top)) - zws(ln(1, l_bot)) < MIN_LAYER_THICKNESS .or. zws(ln(2, l_top)) - zws(ln(2, l_bot)) < MIN_LAYER_THICKNESS) then
         return
      end if

      ! Associate density with the potential density or in-situ density
      if (thermobaricity_in_pressure_gradient) then
         density => in_situ_density
      else
         density => potential_density
      end if

      in_sigma_part = .false.
      if (layertype == LAYTP_Z .and. numtopsig > 0) then
         if (kmxn(ln(1, link_index_2d)) <= numtopsig .or. kmxn(ln(2, link_index_2d)) <= numtopsig) then
            in_sigma_part = .true. ! one of the nodes is in the sigma part
         end if
      end if

      if (kmxn(ln(1, link_index_2d)) > kmxn(ln(2, link_index_2d))) then
         more_active_layers_on_the = LEFT
      else if (kmxn(ln(1, link_index_2d)) < kmxn(ln(2, link_index_2d))) then
         more_active_layers_on_the = RIGHT
      else
         more_active_layers_on_the = EQUAL
      end if

      do link_index_3d = l_top, l_bot, -1
         k1 = ln(1, link_index_3d)
         k1t = k1
         k2 = ln(2, link_index_3d)
         k2t = k2
         if (link_index_3d == l_top) then
            k1t = ktop(ln(1, link_index_2d))
            k2t = ktop(ln(2, link_index_2d))
         end if

         volume_averaged_density(link_index_3d - l_bot + 1) = 0.5_dp * ((zws(k1t) - zws(k1 - 1)) * density(k1) + (zws(k2t) - zws(k2 - 1)) * density(k2))
         if (jarhoxu > 0) then
            rhou(link_index_3d) = volume_averaged_density(link_index_3d - l_bot + 1) / (0.5_dp * (zws(k1t) - zws(k1 - 1) + zws(k2t) - zws(k2 - 1)))
         end if
         volume_averaged_density(link_index_3d - l_bot + 1) = volume_averaged_density(link_index_3d - l_bot + 1) * dx(link_index_2d)

         baroclinic_pressure1 = baroclinic_pressures(k1)
         baroclinic_pressure2 = baroclinic_pressures(k2)
         integrated_baroclinic_pressure1 = integrated_baroclinic_pressures(k1)
         integrated_baroclinic_pressure2 = integrated_baroclinic_pressures(k2)

         if (link_index_3d == l_bot .and. more_active_layers_on_the /= EQUAL) then ! extrapolate at 'bed' layer of deepest side

            if (more_active_layers_on_the == LEFT) then
               k_deep_side = k1
               k_top_deep_side = ktop(ln(1, link_index_2d))
               k_shallow_side = k2
               k_top_shallow_side = ktop(ln(2, link_index_2d))
            else
               k_deep_side = k2
               k_top_deep_side = ktop(ln(2, link_index_2d))
               k_shallow_side = k1
               k_top_shallow_side = ktop(ln(1, link_index_2d))
            end if

            if (k_top_shallow_side - k_shallow_side > 0) then ! Shallow side extrapolates, coeffs based on shallow side:
               weight_down = (zws(k_shallow_side + 1) - zws(k_shallow_side)) / (zws(k_shallow_side + 1) - zws(k_shallow_side - 1))
               weight_up = 1.0_dp - weight_down
               rho_up = weight_up * density(k_deep_side + 1) + weight_down * density(k_deep_side)
               rho_down = 2.0_dp * density(k_deep_side) - rho_up
            else ! one layer
               rho_up = density(k_deep_side)
               rho_down = rho_up
            end if
            rho_up = rho_up - rhomean
            rho_down = rho_down - rhomean

            if (.not. in_sigma_part) then
               delta_z = zws(k_shallow_side) - zws(k_shallow_side - 1)
               volume_averaged_density(1) = delta_z * 0.5_dp * (density(k_deep_side) + density(k_shallow_side)) * dx(link_index_2d)
               if (jarhoxu > 0) then
                  rhou(link_index_3d) = 0.5_dp * (density(k_deep_side) + density(k_shallow_side))
               end if
            else
               delta_z = zws(k_deep_side) - zws(k_deep_side - 1)
            end if

            salinity_up = weight_up * constituents(isalt, k_deep_side + 1) + weight_down * constituents(isalt, k_deep_side)
            temperature_up = weight_up * constituents(itemp, k_deep_side + 1) + weight_down * constituents(itemp, k_deep_side)
            salinity_down = 2.0_dp * constituents(isalt, k_deep_side) - salinity_up
            temperature_down = 2.0_dp * constituents(itemp, k_deep_side) - temperature_up

            if (.not. apply_thermobaricity) then
               rho_down = calculate_density(salinity_down, temperature_down) - rhomean
            else
               barotropic_pressure = (zws(k_top_shallow_side) - zws(k_shallow_side - 1)) * rhomean
               baroclinic_pressure = baroclinic_pressures(k_deep_side + 1) + 0.5_dp * delta_z * (rhosww(k_deep_side) + rhosww(k_deep_side - 1))
               do i = 1, max_iterations_pressure_density
                  total_pressure = ag * (baroclinic_pressure + barotropic_pressure)
                  rho_down = calculate_density(salinity_down, temperature_down, total_pressure) - rhomean
                  baroclinic_pressure = baroclinic_pressures(k_deep_side + 1) + 0.5_dp * delta_z * (rhosww(k_deep_side) + rho_down)
               end do
            end if

            baroclinic_pressure = baroclinic_pressures(k_deep_side + 1) + 0.5_dp * delta_z * (rhosww(k_deep_side) + rho_down)
            integrated_baroclinic_pressure = (baroclinic_pressures(k_deep_side + 1) + 0.5_dp * delta_z * (2.0_dp * rhosww(k_deep_side) + rho_down) / 3.0_dp) * delta_z

            if (more_active_layers_on_the == LEFT) then ! k1=deepest
               baroclinic_pressure1 = baroclinic_pressure
               integrated_baroclinic_pressure1 = integrated_baroclinic_pressure
            else
               baroclinic_pressure2 = baroclinic_pressure
               integrated_baroclinic_pressure2 = integrated_baroclinic_pressure
            end if

            if (.not. in_sigma_part) then
               baroclinic_pressure3 = 0.0_dp ! no skewness for zlay jump at bed
            else
               baroclinic_pressure3 = 0.5_dp * (baroclinic_pressure1 + baroclinic_pressure2) * (zws(k1 - 1) - zws(k2 - 1))
            end if

         else
            baroclinic_pressure3 = 0.5_dp * (baroclinic_pressure1 + baroclinic_pressure2) * (zws(k1 - 1) - zws(k2 - 1))
         end if

         baroclinic_pressure_gradients(link_index_3d - l_bot + 1) = baroclinic_pressure_gradients(link_index_3d - l_bot + 1) + integrated_baroclinic_pressure1 - integrated_baroclinic_pressure2 + baroclinic_pressure3
         if (link_index_3d > l_bot) then
            baroclinic_pressure_gradients(link_index_3d - l_bot) = baroclinic_pressure_gradients(link_index_3d - l_bot) - baroclinic_pressure3 ! ceiling of cell below
         end if
      end do

      call baroclinic_pressure_link_time_integration(baroclinic_pressure_gradients, volume_averaged_density, link_index_2d, l_bot, l_top)
   end subroutine add_baroclinic_pressure_link_interface

   !> Computes baroclinic pressure gradients across layers for a horizontal link.
   !! Cell density (i.e. rho(k_deep_side)) is used
   subroutine add_baroclinic_pressure_link_use_rho_directly(link_index_2d, l_bot, l_top)
      use m_turbulence, only: kmxx, rhou, baroclinic_pressures, integrated_baroclinic_pressures
      use m_flowgeom, only: ln, dx
      use m_flow, only: zws, numtopsig, kmxn, ktop, layertype, LAYTP_Z
      use m_flowparameters, only: jarhoxu
      use m_physcoef, only: rhomean

      integer, intent(in) :: link_index_2d !< Horizontal link index
      integer, intent(in) :: l_bot !< bottom link
      integer, intent(in) :: l_top !< top link

      integer, parameter :: LEFT = 1, RIGHT = 2, EQUAL = 0
      integer :: link_index_3d, k1, k2, k1t, k2t, k_deep_side, k_top_deep_side, k_shallow_side, k_top_shallow_side, more_active_layers_on_the
      logical :: in_sigma_part
      real(kind=dp) :: baroclinic_pressure_gradients(kmxx), volume_averaged_density(kmxx), baroclinic_pressure3
      real(kind=dp) :: baroclinic_pressure1, baroclinic_pressure2, integrated_baroclinic_pressure1, integrated_baroclinic_pressure2, baroclinic_pressure, integrated_baroclinic_pressure, delta_z

      baroclinic_pressure_gradients(1:l_top - l_bot + 1) = 0.0_dp

      if (zws(ln(1, l_top)) - zws(ln(1, l_bot)) < MIN_LAYER_THICKNESS .or. zws(ln(2, l_top)) - zws(ln(2, l_bot)) < MIN_LAYER_THICKNESS) then
         return
      end if

      ! Associate density with the potential density or in-situ density
      if (thermobaricity_in_pressure_gradient) then
         density => in_situ_density
      else
         density => potential_density
      end if

      in_sigma_part = .false.
      if (layertype == LAYTP_Z .and. numtopsig > 0) then
         if (kmxn(ln(1, link_index_2d)) <= numtopsig .or. kmxn(ln(2, link_index_2d)) <= numtopsig) then
            in_sigma_part = .true. ! one of the nodes is in the sigma part
         end if
      end if

      if (kmxn(ln(1, link_index_2d)) > kmxn(ln(2, link_index_2d))) then
         more_active_layers_on_the = LEFT
      else if (kmxn(ln(1, link_index_2d)) < kmxn(ln(2, link_index_2d))) then
         more_active_layers_on_the = RIGHT
      else
         more_active_layers_on_the = EQUAL
      end if

      do link_index_3d = l_top, l_bot, -1
         k1 = ln(1, link_index_3d)
         k1t = k1
         k2 = ln(2, link_index_3d)
         k2t = k2
         if (link_index_3d == l_top) then
            k1t = ktop(ln(1, link_index_2d))
            k2t = ktop(ln(2, link_index_2d))
         end if

         volume_averaged_density(link_index_3d - l_bot + 1) = 0.5_dp * ((zws(k1t) - zws(k1 - 1)) * density(k1) + (zws(k2t) - zws(k2 - 1)) * density(k2))
         if (jarhoxu > 0) then
            rhou(link_index_3d) = volume_averaged_density(link_index_3d - l_bot + 1) / (0.5_dp * (zws(k1t) - zws(k1 - 1) + zws(k2t) - zws(k2 - 1)))
         end if
         volume_averaged_density(link_index_3d - l_bot + 1) = volume_averaged_density(link_index_3d - l_bot + 1) * dx(link_index_2d)

         baroclinic_pressure1 = baroclinic_pressures(k1)
         baroclinic_pressure2 = baroclinic_pressures(k2)
         integrated_baroclinic_pressure1 = integrated_baroclinic_pressures(k1)
         integrated_baroclinic_pressure2 = integrated_baroclinic_pressures(k2)

         if (link_index_3d == l_bot .and. more_active_layers_on_the /= EQUAL) then ! extrapolate at 'bed' layer of deepest side

            if (more_active_layers_on_the == LEFT) then
               k_deep_side = k1
               k_top_deep_side = ktop(ln(1, link_index_2d))
               k_shallow_side = k2
               k_top_shallow_side = ktop(ln(2, link_index_2d))
            else
               k_deep_side = k2
               k_top_deep_side = ktop(ln(2, link_index_2d))
               k_shallow_side = k1
               k_top_shallow_side = ktop(ln(1, link_index_2d))
            end if

            if (.not. in_sigma_part) then
               delta_z = zws(k_shallow_side) - zws(k_shallow_side - 1)

               volume_averaged_density(1) = delta_z * 0.5_dp * (density(k_deep_side) + density(k_shallow_side)) * dx(link_index_2d)
               if (jarhoxu > 0) then
                  rhou(link_index_3d) = 0.5_dp * (density(k_deep_side) + density(k_shallow_side))
               end if

            else
               delta_z = zws(k_deep_side) - zws(k_deep_side - 1) ! deep side
            end if

            baroclinic_pressure = baroclinic_pressures(k_deep_side + 1) + delta_z * (density(k_deep_side) - rhomean)
            integrated_baroclinic_pressure = (baroclinic_pressures(k_deep_side + 1) + 0.5_dp * delta_z * (density(k_deep_side) - rhomean)) * delta_z

            if (more_active_layers_on_the == LEFT) then ! k1=deepest
               baroclinic_pressure1 = baroclinic_pressure
               integrated_baroclinic_pressure1 = integrated_baroclinic_pressure
            else
               baroclinic_pressure2 = baroclinic_pressure
               integrated_baroclinic_pressure2 = integrated_baroclinic_pressure
            end if

            if (.not. in_sigma_part) then
               baroclinic_pressure3 = 0.0_dp ! no skewness for zlay jump at bed
            else
               baroclinic_pressure3 = 0.5_dp * (baroclinic_pressure1 + baroclinic_pressure2) * (zws(k1 - 1) - zws(k2 - 1))
            end if

         else
            baroclinic_pressure3 = 0.5_dp * (baroclinic_pressure1 + baroclinic_pressure2) * (zws(k1 - 1) - zws(k2 - 1))
         end if

         baroclinic_pressure_gradients(link_index_3d - l_bot + 1) = baroclinic_pressure_gradients(link_index_3d - l_bot + 1) + integrated_baroclinic_pressure1 - integrated_baroclinic_pressure2 + baroclinic_pressure3
         if (link_index_3d > l_bot) then
            baroclinic_pressure_gradients(link_index_3d - l_bot) = baroclinic_pressure_gradients(link_index_3d - l_bot) - baroclinic_pressure3 ! ceiling of cell below
         end if
      end do

      call baroclinic_pressure_link_time_integration(baroclinic_pressure_gradients, volume_averaged_density, link_index_2d, l_bot, l_top)
   end subroutine add_baroclinic_pressure_link_use_rho_directly

   !> Applies time integration of baroclinic pressure gradients and updates the momentum terms.
   subroutine baroclinic_pressure_link_time_integration(baroclinic_pressure_gradients, volume_averaged_density, link_index_2d, l_bot, l_top)
      use m_flow, only: adve, kmxL
      use m_flowtimes, only: dts, dtprev
      use m_turbulence, only: baroclinic_force_prev, kmxx
      use m_physcoef, only: ag

      real(kind=dp), dimension(1:kmxx), intent(in) :: baroclinic_pressure_gradients !> Baroclinic pressure gradient for each layer
      real(kind=dp), dimension(1:kmxx), intent(in) :: volume_averaged_density !> Volume-averaged density for each layer
      integer, intent(in) :: link_index_2d !< Horizontal link index
      integer, intent(in) :: l_bot !< bottom link
      integer, intent(in) :: l_top !< top link

      integer :: link_index_3d
      real(kind=dp) :: baroclinic_force, time_weight_factor

      time_weight_factor = 0.5_dp * dts / dtprev
      do link_index_3d = l_bot, l_top
         if (volume_averaged_density(link_index_3d - l_bot + 1) > 0.0_dp) then
            baroclinic_force = ag * baroclinic_pressure_gradients(link_index_3d - l_bot + 1) / volume_averaged_density(link_index_3d - l_bot + 1)
            if (baroclinic_force_prev(link_index_3d) /= 0.0_dp) then
               adve(link_index_3d) = adve(link_index_3d) - (1.0_dp + time_weight_factor) * baroclinic_force + time_weight_factor * baroclinic_force_prev(link_index_3d)
            else
               adve(link_index_3d) = adve(link_index_3d) - baroclinic_force
            end if
            baroclinic_force_prev(link_index_3d) = baroclinic_force
         end if
      end do

      do link_index_3d = l_top + 1, l_bot + kmxL(link_index_2d) - 1
         baroclinic_force_prev(link_index_3d) = 0.0_dp
      end do
   end subroutine baroclinic_pressure_link_time_integration
end module m_add_baroclinic_pressure_link
