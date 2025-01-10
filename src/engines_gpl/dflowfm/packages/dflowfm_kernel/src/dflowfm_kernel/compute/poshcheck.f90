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
module m_poshcheck
   use m_fill_onlywetlinks, only: fill_onlywetlinks

   implicit none

contains

   subroutine poshcheck(key)
      use m_rcirc
      use m_flow
      use m_flowgeom
      use m_flowtimes
      use m_partitioninfo
      use m_timer
      use m_gui
      use m_okay
      use m_set_col

      implicit none

      integer, intent(out) :: key

      integer :: reduced_data(2)
      logical :: is_hu_changed

      if (jaGUI == 1) then
         call setcol(221) ! white
      end if

      call set_water_level_and_hu_for_dry_cells(s1, hu)

      if (jampi == 1) then
         reduced_data = (/key, nodneg/)

         if (jatimer == 1) call starttimer(IMPIREDUCE)
         call reduce_int_max(2, reduced_data)
         if (jatimer == 1) call stoptimer(IMPIREDUCE)

         key = reduced_data(1)
         nodneg = reduced_data(2)
      end if

      if (nodneg /= 0 .and. jposhchk /= -1) then
         if (jposhchk == 1 .or. jposhchk == 3 .or. jposhchk == 5 .or. jposhchk == 7) then
            dts = 0.7d0 * dts
         end if
         dsetb = dsetb + 1 ! total nr of setbacks
         s1 = s0
         vol1 = vol0
         if (dts < dtmin) then
            s1 = max(s1, bl) ! above bottom
            call okay(0)
            key = 1 ! for easier mouse interrupt
         end if
      end if

      if (is_hu_changed) then
         call fill_onlyWetLinks()
      end if

   contains

      !> set_water_level_and_hu_for_dry_cells
      subroutine set_water_level_and_hu_for_dry_cells(water_level, upwind_waterheight)
         use precision, only: dp

         real(kind=dp), intent(inout) :: water_level(:) !< water_level
         real(kind=dp), intent(inout) :: upwind_waterheight(:) !< upwind_waterheight

         integer :: node, link, link_index
         real(kind=dp) :: threshold
         real(kind=dp), parameter :: WATER_LEVEL_TOLERANCE = 1d-10
         real(kind=dp), parameter :: DELFT3D_MIN = 1d-9
         real(kind=dp), parameter :: DELFT3D_MAX = 1d-3
         real(kind=dp), parameter :: REDUCTION_FACTOR = 0.2d0
         integer, parameter :: FLAG_REDO_TIMESTEP = 2
         real(kind=dp), parameter :: SET_VALUE = 0d0
         integer, parameter :: DELFT3D_FLOW_ALGORITHM_TO_PREVENT_VERY_THIN_LAYERS = 1

         Nodneg = 0
         key = 0
         is_hu_changed = .false.

         if (jposhchk == 0) return

         if (testdryflood == DELFT3D_FLOW_ALGORITHM_TO_PREVENT_VERY_THIN_LAYERS) then
            threshold = max(DELFT3D_MIN, min(epshu, DELFT3D_MAX))
         else
            threshold = 0d-0
         end if

         do node = 1, ndxi
            if (abs(kfs(node)) /= 0) then ! Also check ghost nodes for posh/setbacks
               if (water_level(node) < bl(node) + threshold) then
                  if (water_level(node) < bl(node) + threshold - WATER_LEVEL_TOLERANCE) then
                     nodneg = node
                     numnodneg = numnodneg + 1
                     if (jaGUI == 1) then
                        call rcirc(xz(node), yz(node))
                     end if
                     select case (jposhchk)
                     case (-1) ! only detect dry cells and return (for Nested Newton restart)
                        key = FLAG_REDO_TIMESTEP
                     case (1) ! only timestep reduction
                        key = FLAG_REDO_TIMESTEP
                        exit
                     case (2, 3) ! set dry all attached links
                        key = FLAG_REDO_TIMESTEP
                        do link_index = 1, nd(node)%lnx
                           link = abs(nd(node)%ln(link_index))
                           upwind_waterheight(link) = SET_VALUE
                           is_hu_changed = .true.
                        end do
                     case (4, 5) ! reduce links au
                        do link_index = 1, nd(node)%lnx
                           link = abs(nd(node)%ln(link_index))
                           if (upwind_waterheight(link) > 0) then
                              if (REDUCTION_FACTOR * au(link) < eps6) then
                                 upwind_waterheight(link) = SET_VALUE
                                 key = FLAG_REDO_TIMESTEP
                                 is_hu_changed = .true.
                              end if
                              au(link) = REDUCTION_FACTOR * au(link)
                           end if
                        end do
                     case (6, 7) ! only set dry outflowing links
                        do link_index = 1, nd(node)%lnx
                           link = abs(nd(node)%ln(link_index))
                           if (nd(node)%ln(link_index) < 0 .and. u1(link) > 0 .or. &
                               nd(node)%ln(link_index) > 0 .and. u1(link) < 0) then
                              upwind_waterheight(link) = SET_VALUE
                              key = FLAG_REDO_TIMESTEP
                              is_hu_changed = .true.
                           end if
                        end do
                     end select
                  end if

                  if (jamapFlowAnalysis > 0) then
                     negativeDepths(node) = negativeDepths(node) + 1
                  end if

                  water_level(node) = bl(node)

               end if
            end if

         end do

      end subroutine set_water_level_and_hu_for_dry_cells

   end subroutine poshcheck

end module m_poshcheck
