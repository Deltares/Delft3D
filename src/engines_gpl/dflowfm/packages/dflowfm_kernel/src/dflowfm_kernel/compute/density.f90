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

module m_density
   use precision, only: dp

   implicit none

   private

   real(kind=dp), parameter, public :: RHO_MIN = 990.0_dp !< lower limit of density [kg/m3]
   real(kind=dp), parameter, public :: RHO_MAX = 1250.0_dp !< upper limit of density [kg/m3]

   interface calculate_density
      module procedure calculate_density_from_salinity_and_temperature
      module procedure calculate_density_from_salinity_temperature_and_pressure
   end interface

   public :: set_potential_density, set_pressure_dependent_density, density_at_cell, salinity_and_temperature_at_cell
   public :: calculate_density

contains

   function calculate_density_from_salinity_and_temperature(salinity, temperature) result(density)
      use m_physcoef, only: rhomean
      use m_flow, only: idensform
      use MessageHandling, only: LEVEL_ERROR, mess
      use m_density_formulas, only: calculate_density_eckart, calculate_density_unesco, calculate_density_unesco83, calculate_density_baroclinic, calculate_density_nacl, &
                                    DENSITY_OPTION_UNIFORM, DENSITY_OPTION_ECKART, DENSITY_OPTION_UNESCO, DENSITY_OPTION_UNESCO83, DENSITY_OPTION_BAROCLINIC, DENSITY_OPTION_DELTARES_FLUME

      real(kind=dp), intent(in) :: salinity
      real(kind=dp), intent(in) :: temperature
      real(kind=dp) :: density

      select case (idensform)
      case (DENSITY_OPTION_UNIFORM) ! Uniform density
         density = rhomean
      case (DENSITY_OPTION_ECKART) ! Carl Henry Eckart, 1958
         density = calculate_density_eckart(salinity, temperature)
      case (DENSITY_OPTION_UNESCO) ! Unesco org
         density = calculate_density_unesco(salinity, temperature)
      case (DENSITY_OPTION_UNESCO83) ! Unesco83 at surface, call with 0.0_dp for early exit
         density = calculate_density_unesco83(salinity, temperature, 0.0_dp)
      case (DENSITY_OPTION_BAROCLINIC) ! baroclinic instability
         density = calculate_density_baroclinic(salinity)
      case (DENSITY_OPTION_DELTARES_FLUME) ! For Deltares flume experiment IJmuiden , Kees Kuipers saco code 1
         density = calculate_density_nacl(salinity, temperature)
      case default
         call mess(LEVEL_ERROR, 'Unknown density formula. Found idensform = ', idensform)
      end select
   end function calculate_density_from_salinity_and_temperature

   function calculate_density_from_salinity_temperature_and_pressure(salinity, temperature, pressure) result(density)
      use m_flow, only: idensform
      use MessageHandling, only: LEVEL_ERROR, mess
      use m_density_formulas, only: DENSITY_OPTION_UNESCO83, calculate_density_unesco83

      real(kind=dp), intent(in) :: salinity
      real(kind=dp), intent(in) :: temperature
      real(kind=dp), intent(in) :: pressure
      real(kind=dp) :: density

      select case (idensform)
      case (DENSITY_OPTION_UNESCO83)
         density = calculate_density_unesco83(salinity, temperature, pressure)
      case default
         call mess(LEVEL_ERROR, 'Unknown pressure-dependent density formula. Found idensform = ', idensform)
      end select
   end function calculate_density_from_salinity_temperature_and_pressure

   !> Fill potential density of one column
   subroutine set_potential_density(potential_density, cell_index_2d)
      use m_flow, only: kmxn
      use m_get_kbot_ktop, only: getkbotktop

      real(kind=dp), dimension(:), intent(out) :: potential_density !< Potential density of fluid
      integer, intent(in) :: cell_index_2d !< Horizontal cell index (1:ndx)

      real(kind=dp) :: salinity, temperature
      integer :: k_bot, k_top
      integer :: cell_index_3d ! vertical cell index (e.g., k_bot:k_top)

      call getkbotktop(cell_index_2d, k_bot, k_top)
      if (k_top < k_bot) then
         return
      end if

      do cell_index_3d = k_bot, k_top
         call salinity_and_temperature_at_cell(cell_index_3d, salinity, temperature)
         potential_density(cell_index_3d) = calculate_density(salinity, temperature)
         call add_sediment_effect_to_density(potential_density(cell_index_3d), cell_index_3d)
         ! check overshoots at thin water layers
         potential_density(cell_index_3d) = min(potential_density(cell_index_3d), RHO_MAX)
         potential_density(cell_index_3d) = max(potential_density(cell_index_3d), RHO_MIN)
      end do

      do cell_index_3d = k_top + 1, k_bot + kmxn(cell_index_2d) - 1
         potential_density(cell_index_3d) = potential_density(k_top)
      end do

   end subroutine set_potential_density

   !> Fill in-situ density of one column
   subroutine set_pressure_dependent_density(in_situ_density, cell_index_2d)
      use m_flow, only: kmxn, zws
      use m_get_kbot_ktop, only: getkbotktop
      use m_physcoef, only: Maxitpresdens, ag

      real(kind=dp), dimension(:), intent(out) :: in_situ_density !< Pressure dependent density of fluid
      integer, intent(in) :: cell_index_2d !< Horizontal cell index (1:ndx)

      real(kind=dp) :: salinity, temperature, cell_pressure_upper_interface, cell_pressure_lower_interface, dz
      integer :: k_bot, k_top, i
      integer :: cell_index_3d ! vertical cell index (e.g., k_bot:k_top)

      call getkbotktop(cell_index_2d, k_bot, k_top)
      if (k_top < k_bot) then
         return
      end if

      cell_pressure_upper_interface = 0.0_dp ! surface value is 0 bar in unesco, not 1 bar
      do cell_index_3d = k_top, k_bot, -1
         call salinity_and_temperature_at_cell(cell_index_3d, salinity, temperature)
         dz = zws(cell_index_3d) - zws(cell_index_3d - 1)
         do i = 1, Maxitpresdens
            cell_pressure_lower_interface = cell_pressure_upper_interface + ag * dz * in_situ_density(cell_index_3d)
            in_situ_density(cell_index_3d) = calculate_density(salinity, temperature, 0.5_dp * (cell_pressure_lower_interface + cell_pressure_upper_interface))
         end do
         cell_pressure_upper_interface = cell_pressure_lower_interface
         call add_sediment_effect_to_density(in_situ_density(cell_index_3d), cell_index_3d)
         ! Check overshoots at thin water layers
         in_situ_density(cell_index_3d) = min(in_situ_density(cell_index_3d), RHO_MAX)
         in_situ_density(cell_index_3d) = max(in_situ_density(cell_index_3d), RHO_MIN)
      end do

      do cell_index_3d = k_top + 1, k_bot + kmxn(cell_index_2d) - 1
         in_situ_density(cell_index_3d) = in_situ_density(k_top)
      end do
   end subroutine set_pressure_dependent_density

   function density_at_cell(cell_index_3d, pressure) result(density)
      integer, intent(in) :: cell_index_3d !< cell number
      real(kind=dp), intent(in) :: pressure !< some given pressure
      real(kind=dp) :: density

      real(kind=dp) :: salinity, temperature

      call salinity_and_temperature_at_cell(cell_index_3d, salinity, temperature)

      density = calculate_density(salinity, temperature, pressure)

      call add_sediment_effect_to_density(density_at_cell, cell_index_3d)
   end function density_at_cell

   subroutine salinity_and_temperature_at_cell(cell_index_3d, salinity, temperature)
      use m_flow, only: jasal, jatem, backgroundsalinity, backgroundwatertemperature
      use m_transport, only: isalt, itemp, constituents

      implicit none

      integer, intent(in) :: cell_index_3d !< cell index
      real(kind=dp), intent(out) :: salinity, temperature

      if (jasal > 0) then
         salinity = max(0.0_dp, constituents(isalt, cell_index_3d))
      else
         salinity = backgroundsalinity
      end if

      if (jatem > 0) then
         temperature = max(-5.0_dp, constituents(itemp, cell_index_3d))
      else
         temperature = backgroundwatertemperature
      end if
   end subroutine salinity_and_temperature_at_cell

   !> Adds the effect of sediment on the density of a cell
   subroutine add_sediment_effect_to_density(rho, cell)
      use m_sediment, only: jased, jaseddenscoupling, jasubstancedensitycoupling, mxgr, rhosed, sed, stmpar, stm_included
      use m_transport, only: constituents, ised1, itra1, itran
      use m_turbulence, only: rhowat
      use sediment_basics_module, only: has_advdiff
      use messagehandling, only: LEVEL_ERROR, mess
      use unstruc_model, only: check_positive_value

      implicit none

      real(kind=dp), intent(inout) :: rho !< density in a cell [kg/m3]
      integer, intent(in) :: cell !< cell index
      real(kind=dp), parameter :: SEDIMENT_DENSITY = 2600.0_dp !< default/typical sediment density [kg/m3]
      real(kind=dp) :: rhom !< density in a cell [kg/m3] before adding sediment effects
      integer :: i, lsed !< loop indices

      if (jased > 0 .and. stm_included) then
         rhom = rho ! UNST-5170 for mor, only use salt+temp, not sediment effect
         rhom = min(rhom, RHO_MAX) ! check overshoots at thin water layers
         rhom = max(rhom, RHO_MIN) !
         rhowat(cell) = rhom
         if (stmpar%morpar%densin) then ! sediment density effects
            i = ised1
            rhom = rho
            do lsed = 1, stmpar%lsedtot
               if (has_advdiff(stmpar%sedpar%tratyp(lsed))) then ! has suspended component
                  rho = rho + constituents(i, cell) * (stmpar%sedpar%rhosol(lsed) - rhom) / stmpar%sedpar%rhosol(lsed)
                  i = i + 1
               end if
            end do
         end if
      else if (jasubstancedensitycoupling > 0) then ! for now, only works for DELWAQ sediment fractions (concentrations in g/m3 and density of SEDIMENT_DENSITY)
         if (itra1 == 0) then
            call mess(LEVEL_ERROR, 'SubstanceDensityCoupling was set to 1, but there are no substances.')
         end if
         rhom = rho
         do i = itra1, itran
            rho = rho + (1d-3) * constituents(i, cell) * (SEDIMENT_DENSITY - rhom) / SEDIMENT_DENSITY
         end do
      else if (jaseddenscoupling > 0) then ! jased < 4
         rhom = rho
         do i = 1, mxgr
            call check_positive_value('rhosed', rhosed(i))
            rho = rho + sed(i, cell) * (rhosed(i) - rhom) / rhosed(i)
         end do

      end if
   end subroutine add_sediment_effect_to_density
end module m_density
