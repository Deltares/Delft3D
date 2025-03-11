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

module m_setrho
   use precision_basics, only: dp

   implicit none

   private

   public :: set_density, set_pressure_dependent_density, setrhofixedp, get_sal_and_temp, RHO_MIN, RHO_MAX

   real(kind=dp), parameter :: RHO_MIN = 990.0_dp !< lower limit of density [kg/m3]
   real(kind=dp), parameter :: RHO_MAX = 1250.0_dp !< upper limit of density [kg/m3]

contains

   !> fill rho of one column
   subroutine set_density(cell_index)
      use m_densfm, only: densfm, add_sediment_effect_to_density
      use precision, only: dp
      use m_flow, only: rho, density_is_pressure_dependent, kmxn
      use m_get_kbot_ktop, only: getkbotktop

      integer, intent(in) :: cell_index !< horizontal cell index (1:ndx)

      integer :: k_bot, k_top, k
      real(kind=dp) :: sal, temp

      call getkbotktop(cell_index, k_bot, k_top)
      if (k_top < k_bot) then
         return
      end if

      do k = k_bot, k_top
         call get_sal_and_temp(cell_index, sal, temp)
         rho(k) = densfm(sal, temp)
         call add_sediment_effect_to_density(rho(k), k)
         rho(k) = min(rho(k), RHO_MAX) ! check overshoots at thin water layers
         rho(k) = max(rho(k), RHO_MIN) !
      end do

      do k = k_top + 1, k_bot + kmxn(cell_index) - 1
         rho(k) = rho(k_top)
      end do

   end subroutine set_density

   !> Fill rho of one column
   subroutine set_pressure_dependent_density(cell_index)
      use precision, only: dp
      use m_flow, only: rho, density_is_pressure_dependent, kmxn, zws
      use m_get_kbot_ktop, only: getkbotktop
      use m_physcoef, only: Maxitpresdens, ag
      use m_densfm, only: densfm, add_sediment_effect_to_density

      integer, intent(in) :: cell_index !< horizontal cell index (1:ndx)

      real(kind=dp) :: sal, temp, cell_pressure_upper_interface, cell_pressure_lower_interface, dz
      integer :: k_bot, k_top, k, i

      call getkbotktop(cell_index, k_bot, k_top)
      if (k_top < k_bot) then
         return
      end if

      cell_pressure_upper_interface = 0.0_dp ! surface value is 0 bar in unesco, not 1 bar
      do k = k_top, k_bot, -1
         call get_sal_and_temp(cell_index, sal, temp)
         dz = zws(k) - zws(k - 1)
         do i = 1, Maxitpresdens
            cell_pressure_lower_interface = cell_pressure_upper_interface + ag * dz * rho(k)
            rho(k) = densfm(sal, temp, 0.5_dp * (cell_pressure_lower_interface + cell_pressure_upper_interface))
         end do
         cell_pressure_upper_interface = cell_pressure_lower_interface
         call add_sediment_effect_to_density(rho(k), k)
         rho(k) = min(rho(k), RHO_MAX) ! check overshoots at thin water layers
         rho(k) = max(rho(k), RHO_MIN)
      end do

      do k = k_top + 1, k_bot + kmxn(cell_index) - 1
         rho(k) = rho(k_top)
      end do
   end subroutine set_pressure_dependent_density

   real(kind=dp) function setrhofixedp(k, p0)
      use precision, only: dp
      use m_densfm, only: densfm, add_sediment_effect_to_density

      implicit none

      integer, intent(in) :: k !< cell number
      real(kind=dp), intent(in) :: p0 !< some given pressure

      real(kind=dp) :: sal, temp

      call get_sal_and_temp(k, sal, temp)

      setrhofixedp = densfm(sal, temp, p0)

      call add_sediment_effect_to_density(setrhofixedp, k)
   end function setrhofixedp

   subroutine get_sal_and_temp(cell_index, sal, temp)
      use precision, only: dp
      use m_flow, only: jasal, jatem, backgroundsalinity, backgroundwatertemperature
      use m_transport, only: isalt, itemp, constituents

      implicit none

      integer :: cell_index
      real(kind=dp) :: sal, temp

      if (jasal > 0) then
         sal = max(0.0_dp, constituents(isalt, cell_index))
      else
         sal = backgroundsalinity
      end if

      if (jatem > 0) then
         temp = max(-5.0_dp, constituents(itemp, cell_index))
      else
         temp = backgroundwatertemperature
      end if
   end subroutine get_sal_and_temp

end module m_setrho
