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

module m_add_baroclinic_pressure

   implicit none

   private

   public :: add_baroclinic_pressure

contains

   !> Computes and adds the baroclinic pressure gradient contributions to the momentum equations
   subroutine add_baroclinic_pressure()
      use precision, only: dp, comparereal
      use m_add_baroclinic_pressure_link, only: add_baroclinic_pressure_link, add_baroclinic_pressure_link_interface, add_baroclinic_pressure_link_use_rho_directly
      use m_add_baroclinic_pressure_cell, only: add_baroclinic_pressure_cell, add_baroclinic_pressure_cell_interface, add_baroclinic_pressure_cell_use_rho_directly
      use m_add_baroclinic_pressure_2d, only: add_baroclinic_pressure_2d
      use m_flowgeom, only: lnxi, lnx, ndx
      use m_flow, only: hu, kmx
      use m_turbulence, only: baroclinic_pressures, integrated_baroclinic_pressures
      use m_physcoef, only: jabarocponbnd, rhointerfaces
      use m_get_Lbot_Ltop, only: getLbotLtop

      implicit none

      integer :: link_index_2d, l_bot, l_top, cell_index_2d, nr_of_links

      if (jabarocponbnd == 0) then
         nr_of_links = lnxi
      else
         nr_of_links = lnx
      end if

      if (kmx == 0) then
         !$OMP PARALLEL DO &
         !$OMP PRIVATE(link_index_2d)
         do link_index_2d = 1, nr_of_links
            if (comparereal(hu(link_index_2d), 0.0_dp) == 0) then
               cycle
            end if
            call add_baroclinic_pressure_2d(link_index_2d)
         end do
         !$OMP END PARALLEL DO
      else

         baroclinic_pressures(:) = 0.0_dp
         integrated_baroclinic_pressures(:) = 0.0_dp

         if (rhointerfaces == 0) then

            !$OMP PARALLEL DO &
            !$OMP PRIVATE(cell_index_2d)
            do cell_index_2d = 1, ndx
               call add_baroclinic_pressure_cell(cell_index_2d)
            end do
            !$OMP END PARALLEL DO

            !$OMP PARALLEL DO &
            !$OMP PRIVATE(link_index_2d,l_bot,l_top)
            do link_index_2d = 1, nr_of_links
               if (comparereal(hu(link_index_2d), 0.0_dp) == 0) then
                  cycle
               end if
               call getLbotLtop(link_index_2d, l_bot, l_top)
               if (l_top < l_bot) then
                  cycle
               end if
               call add_baroclinic_pressure_link(link_index_2d, l_bot, l_top)
            end do
            !$OMP END PARALLEL DO

         elseif (rhointerfaces == 1) then

            !$OMP PARALLEL DO &
            !$OMP PRIVATE(cell_index_2d)
            do cell_index_2d = 1, ndx
               call add_baroclinic_pressure_cell_interface(cell_index_2d)
            end do
            !$OMP END PARALLEL DO

            !$OMP PARALLEL DO &
            !$OMP PRIVATE(link_index_2d,l_bot,l_top)
            do link_index_2d = 1, nr_of_links
               if (comparereal(hu(link_index_2d), 0.0_dp) == 0) then
                  cycle
               end if
               call getLbotLtop(link_index_2d, l_bot, l_top)
               if (l_top < l_bot) then
                  cycle
               end if
               call add_baroclinic_pressure_link_interface(link_index_2d, l_bot, l_top)
            end do
            !$OMP END PARALLEL DO

         elseif (rhointerfaces == 2) then

            !$OMP PARALLEL DO &
            !$OMP PRIVATE(cell_index_2d)
            do cell_index_2d = 1, ndx
               call add_baroclinic_pressure_cell_use_rho_directly(cell_index_2d)
            end do
            !$OMP END PARALLEL DO

            !$OMP PARALLEL DO &
            !$OMP PRIVATE(link_index_2d,l_bot,l_top)
            do link_index_2d = 1, nr_of_links
               if (comparereal(hu(link_index_2d), 0.0_dp) == 0) then
                  cycle
               end if
               call getLbotLtop(link_index_2d, l_bot, l_top)
               if (l_top < l_bot) then
                  cycle
               end if
               call add_baroclinic_pressure_link_use_rho_directly(link_index_2d, l_bot, l_top)
            end do
            !$OMP END PARALLEL DO

         end if
      end if
   end subroutine add_baroclinic_pressure
end module m_add_baroclinic_pressure
