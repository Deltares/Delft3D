!----- AGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2024-2024.
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

!> Calculate statistics through time for an array of variables
module m_temporal_statistics
   use stdlib_kinds, only: dp
   implicit none
   private

   !> Type holding information to calculate the moving averages for an array of variables over an interval of fixed number of time steps
   type, public :: t_moving_average_data
      private
      integer                                :: variable_size       !< Spatial size of the variable of which a moving average is requested
      integer                                :: window_size         !< Number of time steps included in the moving average
      integer                                :: oldest_step         !< Latest entry in the work array. mod(oldest_step, window_size) + 1 is the next
      real(dp), allocatable, dimension(:)    :: time_steps          !< Rank (window_size) array of time steps belonging to samples in samples array
      real(dp)                               :: time_step_sum       !< Sum of time steps in the window
      real(dp), allocatable, dimension(:, :) :: weighted_samples    !< Rank (variable_size, window_size) array of samples weighted by their respective time steps
      real(dp), allocatable, dimension(:)    :: moving_weighted_sum !< Rank (variable_size) array of the sum of the samples weighted by their time steps in the window
      real(dp), allocatable, dimension(:)    :: last_sample         !< Rank (variable_size) the most recent sample, used when window_size equals one step
   end type t_moving_average_data

   public :: create_moving_average_data, update_moving_average_data, calculate_moving_average

   contains

   !> Initialize and return a moving average type
   pure function create_moving_average_data(variable_size, window_size) result(moving_average_data)
      integer, intent(in)         :: variable_size !< Spatial size of the variable of which a moving average is requested
      integer, intent(in)         :: window_size   !< Number of time steps included in the moving average
      type(t_moving_average_data) :: moving_average_data

      moving_average_data%window_size = window_size
      moving_average_data%variable_size = variable_size
      allocate(moving_average_data%last_sample(window_size), source = 0.0_dp)
      if (window_size > 1) then
         moving_average_data%oldest_step = window_size ! One before position 1
         moving_average_data%time_step_sum = 0.0_dp
         allocate(moving_average_data%time_steps(window_size), moving_average_data%weighted_samples(variable_size, window_size), moving_average_data%moving_weighted_sum(variable_size), source = 0.0_dp)
      end if
   end function create_moving_average_data

   !> Provide a new set of samples, update the moving average
   pure subroutine update_moving_average_data(moving_average_data, new_sample, new_time_step)
      type(t_moving_average_data),                            intent(inout) :: moving_average_data !< The moving average data to be updated
      real(dp), dimension(moving_average_data%variable_size), intent(in)    :: new_sample          !< A new sample to be added to the moving average
      real(dp),                                               intent(in)    :: new_time_step       !< Time step associated with the new sample

      real(dp), dimension(moving_average_data%variable_size) :: new_weighted_sample

      moving_average_data%last_sample = new_sample
      new_weighted_sample = new_sample * new_time_step

      if (moving_average_data%window_size > 1) then
         moving_average_data%oldest_step = mod(moving_average_data%oldest_step, moving_average_data%window_size) + 1
         ! Update sums before updating samples, since old sample needs to be removed
         moving_average_data%time_step_sum = moving_average_data%time_step_sum + new_time_step - moving_average_data%time_steps(moving_average_data%oldest_step)
         moving_average_data%moving_weighted_sum = moving_average_data%moving_weighted_sum + new_weighted_sample - moving_average_data%weighted_samples(:, moving_average_data%oldest_step)
         !> Replace then old samples with then new ones
         moving_average_data%weighted_samples(:, moving_average_data%oldest_step) = new_weighted_sample
         moving_average_data%time_steps(moving_average_data%oldest_step) = new_time_step
      end if
   end subroutine update_moving_average_data

   !> Calculate the moving average over a window with a fixed number of time steps
   pure function calculate_moving_average(moving_average_data) result(moving_average)
      type(t_moving_average_data), intent(in)                :: moving_average_data !< The data to be used
      real(dp), dimension(moving_average_data%variable_size) :: moving_average

      if (moving_average_data%window_size <= 1 .or. moving_average_data%time_step_sum <= 0.0_dp) then
         moving_average = moving_average_data%last_sample
      else
         moving_average = moving_average_data%moving_weighted_sum / moving_average_data%time_step_sum
      endif
   end function calculate_moving_average

end module m_temporal_statistics
