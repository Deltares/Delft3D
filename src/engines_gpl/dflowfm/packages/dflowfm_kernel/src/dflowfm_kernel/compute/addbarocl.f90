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

! "Baroclinic" here refers to density-driven pressure components.
! Variables like baroclinic_pressures are intermediate values. They are converted to physical pressures
! later by multiplying with gravity (ag) in baroclinic_pressure_link_time_integration (addbarocl.f90).

module m_add_baroclinic_pressure_link
   use precision, only: dp
   use m_physcoef, only: thermobaricity_in_baroclinic_pressure_gradient
   use m_turbulence, only: in_situ_density, potential_density

   implicit none

   private

   real(kind=dp), parameter :: MIN_LAYER_THICKNESS = 0.1_dp ! Minimum layer thickness for baroclinic pressure calculation
   real(kind=dp), dimension(:), pointer :: density ! local pointer

   public :: add_baroclinic_pressure_link, add_baroclinic_pressure_link_interface, add_baroclinic_pressure_link_use_rho_directly

contains

   !> Computes baroclinic pressure gradients across layers for a horizontal link.
   !! Density is based on linear interpolation of density at vertical interfaces.
   subroutine add_baroclinic_pressure_link(link_index_2d, l_bot, l_top)
      use m_turbulence, only: kmxx, rhou, baroclinic_pressures, integrated_baroclinic_pressures
      use m_flowgeom, only: ln, dx
      use m_flow, only: zws, numtopsig, kmxn, ktop
      use m_flowparameters, only: jarhoxu
      use m_physcoef, only: rhomean

      integer, intent(in) :: link_index_2d !< Horizontal link index
      integer, intent(in) :: l_bot !< bottom link
      integer, intent(in) :: l_top !< top link

      integer :: link_index_3d, k1, k2, k1t, k2t, cell_index_3d, k_top, kz, ktz, insigpart, morelayersleft
      real(kind=dp) :: baroclinic_pressure_gradients(kmxx), volume_averaged_density(kmxx), baroclinic_pressure3
      real(kind=dp) :: baroclinic_pressure1, baroclinic_pressure2, integrated_baroclinic_pressure1, integrated_baroclinic_pressure2, baroclinic_pressure, integrated_baroclinic_pressure, weight_up, weight_down, delta_z, rho_down, rho_up

      baroclinic_pressure_gradients(1:l_top - l_bot + 1) = 0.0_dp

      if (zws(ln(1, l_top)) - zws(ln(1, l_bot)) < MIN_LAYER_THICKNESS .or. zws(ln(2, l_top)) - zws(ln(2, l_bot)) < MIN_LAYER_THICKNESS) then
         return
      end if

      ! Associate density with the potential density or in-situ density
      if (thermobaricity_in_baroclinic_pressure_gradient) then
         density => in_situ_density
      else
         density => potential_density
      end if

      insigpart = 0
      if (numtopsig > 0) then
         if (kmxn(ln(1, link_index_2d)) <= numtopsig .or. kmxn(ln(2, link_index_2d)) <= numtopsig) then
            insigpart = 1 ! one of the nodes is in the sigma part
         end if
      end if

      if (kmxn(ln(1, link_index_2d)) > kmxn(ln(2, link_index_2d))) then
         morelayersleft = 1
      else if (kmxn(ln(1, link_index_2d)) < kmxn(ln(2, link_index_2d))) then
         morelayersleft = 2
      else
         morelayersleft = 0
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

         if (link_index_3d == l_bot .and. morelayersleft /= 0) then ! extrapolate at 'bed' layer of deepest side

            if (morelayersleft == 1) then ! cell_index_3d=deep side, kz=shallow side
               cell_index_3d = k1
               k_top = ktop(ln(1, link_index_2d))
               kz = k2
               ktz = ktop(ln(2, link_index_2d))
            else
               cell_index_3d = k2
               k_top = ktop(ln(2, link_index_2d))
               kz = k1
               ktz = ktop(ln(1, link_index_2d))
            end if

            if (ktz - kz > 0) then ! shallow side extrapolates, coeffs based on shallow side:
               weight_up = (zws(kz + 1) - zws(kz)) / (zws(kz + 1) - zws(kz - 1))
               weight_down = 1.0_dp - weight_up
               rho_up = weight_up * density(cell_index_3d + 1) + weight_down * density(cell_index_3d)
               rho_down = 2.0_dp * density(cell_index_3d) - rho_up
            else ! one layer
               rho_up = density(cell_index_3d)
               rho_down = rho_up
            end if

            rho_up = rho_up - rhomean
            rho_down = rho_down - rhomean
            if (insigpart == 0) then
               delta_z = zws(kz) - zws(kz - 1) ! shallow side

               volume_averaged_density(1) = delta_z * 0.5_dp * (density(cell_index_3d) + density(kz)) * dx(link_index_2d)
               if (jarhoxu > 0) then
                  rhou(link_index_3d) = 0.5_dp * (density(cell_index_3d) + density(kz))
               end if

            else
               delta_z = zws(cell_index_3d) - zws(cell_index_3d - 1) ! deep side
            end if

            baroclinic_pressure = baroclinic_pressures(cell_index_3d + 1) + 0.5_dp * delta_z * (rho_up + rho_down)
            integrated_baroclinic_pressure = (baroclinic_pressures(cell_index_3d + 1) + 0.5_dp * delta_z * (2.0_dp * rho_up + rho_down) / 3.0_dp) * delta_z

            if (morelayersleft == 1) then ! k1=deepest
               baroclinic_pressure1 = baroclinic_pressure
               integrated_baroclinic_pressure1 = integrated_baroclinic_pressure
            else
               baroclinic_pressure2 = baroclinic_pressure
               integrated_baroclinic_pressure2 = integrated_baroclinic_pressure
            end if

            if (insigpart == 0) then
               baroclinic_pressure3 = 0.0_dp ! no skewness for zlay jump at bed
            else
               baroclinic_pressure3 = 0.5_dp * (baroclinic_pressure1 + baroclinic_pressure2) * (zws(k1 - 1) - zws(k2 - 1))
            end if

         else
            baroclinic_pressure3 = 0.5_dp * (baroclinic_pressure1 + baroclinic_pressure2) * (zws(k1 - 1) - zws(k2 - 1))
         end if

         baroclinic_pressure_gradients(link_index_3d - l_bot + 1) = baroclinic_pressure_gradients(link_index_3d - l_bot + 1) + integrated_baroclinic_pressure1 - integrated_baroclinic_pressure2 + baroclinic_pressure3
         if (link_index_3d > l_bot) then
            baroclinic_pressure_gradients(link_index_3d - l_bot) = baroclinic_pressure_gradients(link_index_3d - l_bot) - baroclinic_pressure3 ! ceiling of ff# downstairs neighbours
         end if
      end do

      call baroclinic_pressure_link_time_integration(baroclinic_pressure_gradients, volume_averaged_density, link_index_2d, l_bot, l_top)
   end subroutine add_baroclinic_pressure_link

   !> Computes baroclinic pressure gradients across layers for a horizontal link.
   !! Density is based on linear interpolation of recomputed density (from salinity, temperature (and pressure)) at vertical interfaces.
   subroutine add_baroclinic_pressure_link_interface(link_index_2d, l_bot, l_top)
      use m_turbulence, only: kmxx, rhou, baroclinic_pressures, integrated_baroclinic_pressures, rhosww
      use m_flowgeom, only: ln, dx
      use m_flow, only: zws, numtopsig, kmxn, ktop
      use m_flowparameters, only: jarhoxu
      use m_transport, only: ISALT, ITEMP, constituents
      use m_physcoef, only: rhomean, max_iterations_pressure_density, ag, apply_thermobaricity
      use m_density, only: calculate_density

      integer, intent(in) :: link_index_2d !< Horizontal link index
      integer, intent(in) :: l_bot !< bottom link
      integer, intent(in) :: l_top !< top link

      integer :: link_index_3d, k1, k2, k1t, k2t, cell_index_3d, k_top, kz, ktz, insigpart, morelayersleft, i
      real(kind=dp) :: baroclinic_pressure_gradients(kmxx), volume_averaged_density(kmxx), baroclinic_pressure3
      real(kind=dp) :: baroclinic_pressure1, baroclinic_pressure2, integrated_baroclinic_pressure1, integrated_baroclinic_pressure2, baroclinic_pressure, integrated_baroclinic_pressure, salinity_down, salinity_up, temperature_down, temperature_up, weight_up, weight_down, delta_z, rho_down, rho_up, barotropic_pressure, total_pressure

      baroclinic_pressure_gradients(1:l_top - l_bot + 1) = 0.0_dp

      if (zws(ln(1, l_top)) - zws(ln(1, l_bot)) < MIN_LAYER_THICKNESS .or. zws(ln(2, l_top)) - zws(ln(2, l_bot)) < MIN_LAYER_THICKNESS) then
         return
      end if

      ! Associate density with the potential density or in-situ density
      if (thermobaricity_in_baroclinic_pressure_gradient) then
         density => in_situ_density
      else
         density => potential_density
      end if

      insigpart = 0
      if (numtopsig > 0) then
         if (kmxn(ln(1, link_index_2d)) <= numtopsig .or. kmxn(ln(2, link_index_2d)) <= numtopsig) then
            insigpart = 1 ! one of the nodes is in the sigma part
         end if
      end if

      if (kmxn(ln(1, link_index_2d)) > kmxn(ln(2, link_index_2d))) then
         morelayersleft = 1
      else if (kmxn(ln(1, link_index_2d)) < kmxn(ln(2, link_index_2d))) then
         morelayersleft = 2
      else
         morelayersleft = 0
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

         if (link_index_3d == l_bot .and. morelayersleft /= 0) then ! extrapolate at 'bed' layer of deepest side

            if (morelayersleft == 1) then ! cell_index_3d=deep side, kz=shallow side
               cell_index_3d = k1
               k_top = ktop(ln(1, link_index_2d))
               kz = k2
               ktz = ktop(ln(2, link_index_2d))
            else
               cell_index_3d = k2
               k_top = ktop(ln(2, link_index_2d))
               kz = k1
               ktz = ktop(ln(1, link_index_2d))
            end if

            if (ktz - kz > 0) then ! shallow side extrapolates, coeffs based on shallow side:
               weight_up = (zws(kz + 1) - zws(kz)) / (zws(kz + 1) - zws(kz - 1))
               weight_down = 1.0_dp - weight_up
               rho_up = weight_up * density(cell_index_3d + 1) + weight_down * density(cell_index_3d)
               rho_down = 2.0_dp * density(cell_index_3d) - rho_up
            else ! one layer
               rho_up = density(cell_index_3d)
               rho_down = rho_up
            end if

            rho_up = rho_up - rhomean
            rho_down = rho_down - rhomean
            if (insigpart == 0) then
               delta_z = zws(kz) - zws(kz - 1) ! shallow side

               volume_averaged_density(1) = delta_z * 0.5_dp * (density(cell_index_3d) + density(kz)) * dx(link_index_2d)
               if (jarhoxu > 0) then
                  rhou(link_index_3d) = 0.5_dp * (density(cell_index_3d) + density(kz))
               end if

            else
               delta_z = zws(cell_index_3d) - zws(cell_index_3d - 1) ! deep side
            end if

            salinity_up = weight_up * constituents(isalt, cell_index_3d + 1) + weight_down * constituents(isalt, cell_index_3d)
            temperature_up = weight_up * constituents(itemp, cell_index_3d + 1) + weight_down * constituents(itemp, cell_index_3d)
            salinity_down = 2.0_dp * constituents(isalt, cell_index_3d) - salinity_up
            temperature_down = 2.0_dp * constituents(itemp, cell_index_3d) - temperature_up

            if (.not. apply_thermobaricity) then
               rho_down = calculate_density(salinity_down, temperature_down) - rhomean
            else
               barotropic_pressure = (zws(ktz) - zws(kz - 1)) * rhomean
               baroclinic_pressure = baroclinic_pressures(cell_index_3d + 1) + 0.5_dp * delta_z * (rhosww(cell_index_3d) + rhosww(cell_index_3d - 1))
               do i = 1, max_iterations_pressure_density
                  total_pressure = ag * (baroclinic_pressure + barotropic_pressure)
                  rho_down = calculate_density(salinity_down, temperature_down, total_pressure) - rhomean
                  baroclinic_pressure = baroclinic_pressures(cell_index_3d + 1) + 0.5_dp * delta_z * (rhosww(cell_index_3d) + rho_down)
               end do
            end if

            baroclinic_pressure = baroclinic_pressures(cell_index_3d + 1) + 0.5_dp * delta_z * (rhosww(cell_index_3d) + rho_down)
            integrated_baroclinic_pressure = (baroclinic_pressures(cell_index_3d + 1) + 0.5_dp * delta_z * (2.0_dp * rhosww(cell_index_3d) + rho_down) / 3.0_dp) * delta_z

            if (morelayersleft == 1) then ! k1=deepest
               baroclinic_pressure1 = baroclinic_pressure
               integrated_baroclinic_pressure1 = integrated_baroclinic_pressure
            else
               baroclinic_pressure2 = baroclinic_pressure
               integrated_baroclinic_pressure2 = integrated_baroclinic_pressure
            end if

            if (insigpart == 0) then
               baroclinic_pressure3 = 0.0_dp ! no skewness for zlay jump at bed
            else
               baroclinic_pressure3 = 0.5_dp * (baroclinic_pressure1 + baroclinic_pressure2) * (zws(k1 - 1) - zws(k2 - 1))
            end if

         else
            baroclinic_pressure3 = 0.5_dp * (baroclinic_pressure1 + baroclinic_pressure2) * (zws(k1 - 1) - zws(k2 - 1))
         end if

         baroclinic_pressure_gradients(link_index_3d - l_bot + 1) = baroclinic_pressure_gradients(link_index_3d - l_bot + 1) + integrated_baroclinic_pressure1 - integrated_baroclinic_pressure2 + baroclinic_pressure3
         if (link_index_3d > l_bot) then
            baroclinic_pressure_gradients(link_index_3d - l_bot) = baroclinic_pressure_gradients(link_index_3d - l_bot) - baroclinic_pressure3 ! ceiling of ff# downstairs neighbours
         end if
      end do

      call baroclinic_pressure_link_time_integration(baroclinic_pressure_gradients, volume_averaged_density, link_index_2d, l_bot, l_top)
   end subroutine add_baroclinic_pressure_link_interface

   !> Computes baroclinic pressure gradients across layers for a horizontal link.
   !! Cell density (i.e. rho(cell_index_3d)) is used
   subroutine add_baroclinic_pressure_link_use_rho_directly(link_index_2d, l_bot, l_top)
      use m_turbulence, only: kmxx, rhou, baroclinic_pressures, integrated_baroclinic_pressures
      use m_flowgeom, only: ln, dx
      use m_flow, only: zws, numtopsig, kmxn, ktop
      use m_flowparameters, only: jarhoxu
      use m_physcoef, only: rhomean

      integer, intent(in) :: link_index_2d !< Horizontal link index
      integer, intent(in) :: l_bot !< bottom link
      integer, intent(in) :: l_top !< top link

      integer :: link_index_3d, k1, k2, k1t, k2t, cell_index_3d, k_top, kz, ktz, insigpart, morelayersleft
      real(kind=dp) :: baroclinic_pressure_gradients(kmxx), volume_averaged_density(kmxx), baroclinic_pressure3
      real(kind=dp) :: baroclinic_pressure1, baroclinic_pressure2, integrated_baroclinic_pressure1, integrated_baroclinic_pressure2, baroclinic_pressure, integrated_baroclinic_pressure, delta_z

      baroclinic_pressure_gradients(1:l_top - l_bot + 1) = 0.0_dp

      if (zws(ln(1, l_top)) - zws(ln(1, l_bot)) < MIN_LAYER_THICKNESS .or. zws(ln(2, l_top)) - zws(ln(2, l_bot)) < MIN_LAYER_THICKNESS) then
         return
      end if

      ! Associate density with the potential density or in-situ density
      if (thermobaricity_in_baroclinic_pressure_gradient) then
         density => in_situ_density
      else
         density => potential_density
      end if

      insigpart = 0
      if (numtopsig > 0) then
         if (kmxn(ln(1, link_index_2d)) <= numtopsig .or. kmxn(ln(2, link_index_2d)) <= numtopsig) then
            insigpart = 1 ! one of the nodes is in the sigma part
         end if
      end if

      if (kmxn(ln(1, link_index_2d)) > kmxn(ln(2, link_index_2d))) then
         morelayersleft = 1
      else if (kmxn(ln(1, link_index_2d)) < kmxn(ln(2, link_index_2d))) then
         morelayersleft = 2
      else
         morelayersleft = 0
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

         if (link_index_3d == l_bot .and. morelayersleft /= 0) then ! extrapolate at 'bed' layer of deepest side

            if (morelayersleft == 1) then ! cell_index_3d=deep side, kz=shallow side
               cell_index_3d = k1
               k_top = ktop(ln(1, link_index_2d))
               kz = k2
               ktz = ktop(ln(2, link_index_2d))
            else
               cell_index_3d = k2
               k_top = ktop(ln(2, link_index_2d))
               kz = k1
               ktz = ktop(ln(1, link_index_2d))
            end if

            if (insigpart == 0) then
               delta_z = zws(kz) - zws(kz - 1) ! shallow side

               volume_averaged_density(1) = delta_z * 0.5_dp * (density(cell_index_3d) + density(kz)) * dx(link_index_2d)
               if (jarhoxu > 0) then
                  rhou(link_index_3d) = 0.5_dp * (density(cell_index_3d) + density(kz))
               end if

            else
               delta_z = zws(cell_index_3d) - zws(cell_index_3d - 1) ! deep side
            end if

            baroclinic_pressure = baroclinic_pressures(cell_index_3d + 1) + delta_z * (density(cell_index_3d) - rhomean)
            integrated_baroclinic_pressure = (baroclinic_pressures(cell_index_3d + 1) + 0.5_dp * delta_z * (density(cell_index_3d) - rhomean)) * delta_z

            if (morelayersleft == 1) then ! k1=deepest
               baroclinic_pressure1 = baroclinic_pressure
               integrated_baroclinic_pressure1 = integrated_baroclinic_pressure
            else
               baroclinic_pressure2 = baroclinic_pressure
               integrated_baroclinic_pressure2 = integrated_baroclinic_pressure
            end if

            if (insigpart == 0) then
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
      use m_turbulence, only: kmxx, baroclinic_force_prev
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
