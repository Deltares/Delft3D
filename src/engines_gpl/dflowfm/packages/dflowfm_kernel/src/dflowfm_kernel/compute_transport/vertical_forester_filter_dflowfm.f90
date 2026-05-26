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

module m_vertical_forester_filter_dflowfm ! _dflowfm suffix added to avoid name clash with WAQ module
   use precision, only: dp
   use m_flow, only: ndkx
   use m_transportdata, only: constituents, numconst, const_names, ITEMP

   implicit none(type, external)

   private

   public :: apply_vertical_forester_filter_to_all_constituents

contains

   !> Applies the Forester vertical filter to all constituents in the model
   subroutine apply_vertical_forester_filter_to_all_constituents()
      use m_flow, only: kbot, ktop, max_iterations_vertical_forester, vol1, kmxn
      use m_flowgeom, only: ndxi
      use m_physcoef, only: use_salinity_freezing_point
      use string_module, only: str_tolower
      use timers, only: timon, timstrt, timstop

      ! Local variables
      integer :: i_bottom_layer
      integer :: i_constituent
      integer :: i_flowcell
      integer :: number_of_layers
      integer(4) :: timer_handle

      ! Initialization
      timer_handle = 0

      ! Start timer for Forester filter if timing is enabled
      if (timon) then
         call timstrt("apply_vertical_forester_filter_to_all_constituents", timer_handle)
      end if

      ! Loop over flow cells and apply the Forester vertical filter for all constituents in each vertical column of flow cells
      do i_flowcell = 1, ndxi
         ! Determine the bottom layer index and number of layers in the vertical column of flow cells
         i_bottom_layer = kbot(i_flowcell)
         number_of_layers = ktop(i_flowcell) - i_bottom_layer + 1

         ! Apply the Forester vertical filter for each constituent in this vertical column of flow cells
         do i_constituent = 1, numconst

            ! Skip the vertical Forester filter for oxygen, since negative values are allowed
            if (trim(str_tolower(const_names(i_constituent))) == 'oxy') then
               cycle 
            end if

            ! Skip the vertical Forester filter for temperature if the salinity freezing point is enabled, since negative values are allowed
            if (i_constituent == ITEMP .and. use_salinity_freezing_point) then
               cycle
            end if

            call apply_vertical_forester_filter_per_column_and_constituent(i_constituent, vol1(i_bottom_layer:), number_of_layers, kmxn(i_flowcell), i_bottom_layer, max_iterations_vertical_forester)
         end do
      end do

      ! Stop timer for Forester filter if timing is enabled
      if (timon) then
         call timstop(timer_handle)
      end if

   end subroutine apply_vertical_forester_filter_to_all_constituents

   !> Applies the Forester vertical filter to a single constituent in a vertical column of flow cells
   subroutine apply_vertical_forester_filter_per_column_and_constituent(i_constituent, cell_volume, number_of_layers, number_of_active_layers, i_bottom_layer, max_iterations)
      use m_flow, only: EPS6, EPS10

      ! Parameters
      integer, intent(in) :: i_constituent !< Index of the constituent to apply the Forester filter to
      real(kind=dp), intent(in) :: cell_volume(number_of_active_layers) !< Volume of the flow cells
      integer, intent(in) :: number_of_layers !< Number of layers in the vertical column
      integer, intent(in) :: number_of_active_layers !< Maximum number of active layers in the model
      integer, intent(in) :: i_bottom_layer !< Index of the bottom layer in the constituents array
      integer, intent(in) :: max_iterations !< Maximum number of iterations for Forester filter

      ! Local variables
      real(kind=dp), dimension(number_of_layers) :: updated_constituent !< Array to hold the updated constituent values during filtering
      real(kind=dp), dimension(number_of_layers) :: previous_constituent !< Array to hold the constituent values from the previous iteration for comparison
      real(kind=dp) :: difference !< Difference in constituent values between adjacent layers
      integer :: k !< Layer index
      integer :: m !< Iteration index
      logical :: filtered_this_iteration !< Flag to track if any filtering was done in the current iteration

      ! Copy constituent values for the vertical column to a local array
      updated_constituent(1:number_of_layers) = constituents(i_constituent, i_bottom_layer:i_bottom_layer + number_of_layers - 1)

      ! Iteratively apply the Forester filter until no more filtering is needed or the maximum number of iterations is reached
      do m = 1, max_iterations

         ! Copy the current constituent values to the reference array for this iteration
         previous_constituent(1:number_of_layers) = updated_constituent(1:number_of_layers)
         filtered_this_iteration = .false.

         ! Loop over layers in the vertical column and apply the Forester filter based on the difference between adjacent layers
         do k = 1, number_of_layers - 1
            difference = previous_constituent(k + 1) - previous_constituent(k)
            if (difference > EPS6 .or. previous_constituent(k) < 0.0_dp .or. previous_constituent(k + 1) < 0.0_dp) then
               if (cell_volume(k) > EPS10 .and. cell_volume(k + 1) > EPS10) then
                  filtered_this_iteration = .true.
                  difference = difference / 6.0_dp * (cell_volume(k + 1) + cell_volume(k))
                  updated_constituent(k) = updated_constituent(k) + difference / cell_volume(k)
                  updated_constituent(k + 1) = updated_constituent(k + 1) - difference / cell_volume(k + 1)
               else
                  difference = 0.0_dp
               end if
            end if
         end do

         ! Exit the iteration loop if no filtering was needed in this iteration
         if (.not. filtered_this_iteration) then
            exit
         end if

      end do

      ! Copy the filtered constituent values back to the main constituents array
      constituents(i_constituent, i_bottom_layer:i_bottom_layer + number_of_layers - 1) = updated_constituent(1:number_of_layers)

      ! If the number of active layers is larger than the number of layers in this column,
      ! fill the remaining layers with the value of the last layer (which is the value at the water surface)
      if (number_of_active_layers > number_of_layers) then
         constituents(i_constituent, i_bottom_layer + number_of_layers:i_bottom_layer + number_of_active_layers - 1) = updated_constituent(number_of_layers)
      end if

   end subroutine apply_vertical_forester_filter_per_column_and_constituent

end module m_vertical_forester_filter_dflowfm
