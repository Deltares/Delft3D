!!  Copyright (C)  Stichting Deltares, 2024-2024.
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

module test_temporal_statistics
   use ftnunit
   use m_temporal_statistics
   use stdlib_kinds, only: dp
   implicit none
   private

   real(dp), parameter :: test_tolerance = 1e-3_dp

   public :: tests_temporal_statistics

contains

subroutine tests_temporal_statistics
   call test(test_create_moving_average, 'Tests moving average without any samples')
   call test(test_moving_average_one_sample_larger_window, 'Tests moving average update with one sample and a larger window')
   call test(test_moving_average_one_sample_small_window, 'Tests moving average update with one sample and a window of 1 step')
   call test(test_moving_average_multiple_samples, 'Tests moving average update with 5 samples and a window of 4')
   call test(test_moving_average_varying_time_steps, 'Tests moving average update with 5 samples and a window of 4 and varying time steps')
end subroutine tests_temporal_statistics

subroutine test_create_moving_average()
   type(t_moving_average_data) :: moving_average_data
   integer, parameter :: variable_size = 1, window_size = 3

   moving_average_data = create_moving_average_data(variable_size, window_size)
   call assert_comparable(calculate_moving_average(moving_average_data), [0.0_dp], test_tolerance, '')
end subroutine test_create_moving_average

subroutine test_moving_average_one_sample_larger_window()
   type(t_moving_average_data) :: moving_average_data
   integer, parameter :: variable_size = 1, window_size = 3
   real(dp) :: time_step, sample

   time_step = 0.1_dp
   sample = 3.14
   moving_average_data = create_moving_average_data(variable_size, window_size)
   call update_moving_average_data(moving_average_data, [sample], time_step)
   call assert_comparable(calculate_moving_average(moving_average_data), [sample], test_tolerance, '')
end subroutine test_moving_average_one_sample_larger_window

subroutine test_moving_average_one_sample_small_window()
   type(t_moving_average_data) :: moving_average_data
   integer, parameter :: variable_size = 1, window_size = 1
   real(dp) :: time_step, sample

   time_step = 0.1_dp
   sample = 3.14
   moving_average_data = create_moving_average_data(variable_size, window_size)
   call update_moving_average_data(moving_average_data, [sample], time_step)
   call assert_comparable(calculate_moving_average(moving_average_data), [sample], test_tolerance, '')
end subroutine test_moving_average_one_sample_small_window

subroutine test_moving_average_multiple_samples()
   type(t_moving_average_data) :: moving_average_data
   integer, parameter :: variable_size = 1, window_size = 4, number_of_steps = 5
   integer :: step
   real(dp) :: time_step
   real(dp), dimension(variable_size, number_of_steps) :: samples
   real(dp), dimension(variable_size) :: expected_data

   time_step = 0.1_dp
   samples = reshape([39.0_dp, 13.11_dp, -25.64_dp, -2.29_dp, 111.73_dp], [variable_size, number_of_steps])
   moving_average_data = create_moving_average_data(variable_size, window_size)
   do step = 1, number_of_steps
      call update_moving_average_data(moving_average_data, samples(:, step), time_step)
   end do
   expected_data = [sum(samples(:, number_of_steps - window_size + 1 : number_of_steps)) / 4.0_dp]
   call assert_comparable(calculate_moving_average(moving_average_data), expected_data, test_tolerance, '')
end subroutine test_moving_average_multiple_samples

subroutine test_moving_average_varying_time_steps
   type(t_moving_average_data) :: moving_average_data
   integer, parameter :: variable_size = 2, window_size = 4, number_of_steps = 5
   integer :: step, variable
   real(dp), dimension(number_of_steps) :: time_steps
   real(dp), dimension(variable_size, number_of_steps) :: samples
   real(dp), dimension(variable_size) :: expected_data

   time_steps = [1.0_dp, 3.0_dp, 0.0_dp, 0.13_dp, 0.53_dp]
   samples = reshape([129.3_dp, -52.1_dp, 0.33_dp, 13.8_dp, -7.1855_dp, 0.993_dp, -32.5_dp, -6.618_dp, 894.4_dp, 222.8_dp], [variable_size, number_of_steps])
   moving_average_data = create_moving_average_data(variable_size, window_size)
   do step = 1, number_of_steps
      call update_moving_average_data(moving_average_data, samples(:, step), time_steps(step))
   end do
   do variable = 1, variable_size
      expected_data(variable) = sum(samples(variable, number_of_steps - window_size + 1 : number_of_steps) * time_steps(number_of_steps - window_size + 1 : number_of_steps)) &
                    / sum(time_steps(number_of_steps - window_size + 1 : number_of_steps))
   end do
   call assert_comparable(calculate_moving_average(moving_average_data), expected_data, test_tolerance, '')
end subroutine test_moving_average_varying_time_steps

end module test_temporal_statistics
