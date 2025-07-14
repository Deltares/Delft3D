!----- LGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2023-2025.
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

module test_date_time_from_ref_date
   use ftnunit
   use m_date_time_from_ref_date
   use precision, only: dp
   implicit none
   private

   public :: tests_date_time_from_ref_date

contains

   subroutine tests_date_time_from_ref_date
      call test(test_zero_time_offset, "Test zero time offset")
      call test(test_add_one_day_crossing_month, "Test add one day crossing month")
      call test(test_add_time_crossing_minute, "Test add time crossing minute")
      call test(test_add_time_with_seconds, "Test add time including seconds")
      call test(test_add_half_a_second_on_minute_border, "Test add half a second on minute border")
   end subroutine tests_date_time_from_ref_date

   subroutine test_zero_time_offset()
      real(kind=dp) :: time_since_ref_date
      character(len=ref_date_len) :: ref_date
      integer :: year, month, day, hour, minute, second

      ref_date = "19000101"
      time_since_ref_date = 0.0_dp
      call date_time_from_ref_date(time_since_ref_date, ref_date, year, month, day, hour, minute, second)
      call assert_equal(year, 1900, "year")
      call assert_equal(month, 1, "month")
      call assert_equal(day, 1, "day")
      call assert_equal(hour, 0, "hour")
      call assert_equal(minute, 0, "minute")
      call assert_equal(second, 0, "second")
   end subroutine test_zero_time_offset

   subroutine test_add_one_day_crossing_month()
      real(kind=dp) :: time_since_ref_date
      character(len=ref_date_len) :: ref_date
      integer :: year, month, day, hour, minute, second

      ref_date = "20240731"
      time_since_ref_date = real(60 * 60 * 24, kind=dp)
      call date_time_from_ref_date(time_since_ref_date, ref_date, year, month, day, hour, minute, second)
      call assert_equal(year, 2024, "year")
      call assert_equal(month, 8, "month")
      call assert_equal(day, 1, "day")
      call assert_equal(hour, 0, "hour")
      call assert_equal(minute, 0, "minute")
      call assert_equal(second, 0, "second")
   end subroutine test_add_one_day_crossing_month

   subroutine test_add_time_crossing_minute()
      real(kind=dp) :: time_since_ref_date
      character(len=ref_date_len) :: ref_date
      integer :: year, month, day, hour, minute, second

      ref_date = "20201211"
      time_since_ref_date = real(60 * 60 * 5 + 60 * 3, kind=dp)
      call date_time_from_ref_date(time_since_ref_date, ref_date, year, month, day, hour, minute, second)
      call assert_equal(year, 2020, "year")
      call assert_equal(month, 12, "month")
      call assert_equal(day, 11, "day")
      call assert_equal(hour, 5, "hour")
      call assert_equal(minute, 3, "minute")
      call assert_equal(second, 0, "second")
   end subroutine test_add_time_crossing_minute

   subroutine test_add_time_with_seconds()
      real(kind=dp) :: time_since_ref_date
      character(len=ref_date_len) :: ref_date
      integer :: year, month, day, hour, minute, second

      ref_date = "19991230"
      time_since_ref_date = real(60 * 60 * 50 + 60 * 48 + 13, kind=dp)
      call date_time_from_ref_date(time_since_ref_date, ref_date, year, month, day, hour, minute, second)
      call assert_equal(year, 2000, "year")
      call assert_equal(month, 1, "month")
      call assert_equal(day, 1, "day")
      call assert_equal(hour, 2, "hour")
      call assert_equal(minute, 48, "minute")
      call assert_equal(second, 13, "second")
   end subroutine test_add_time_with_seconds

   subroutine test_add_half_a_second_on_minute_border()
      real(kind=dp) :: time_since_ref_date
      character(len=ref_date_len) :: ref_date
      integer :: year, month, day, hour, minute, second

      ref_date = "19920206"
      time_since_ref_date = 59.6
      call date_time_from_ref_date(time_since_ref_date, ref_date, year, month, day, hour, minute, second)
      call assert_equal(year, 1992, "year")
      call assert_equal(month, 2, "month")
      call assert_equal(day, 6, "day")
      call assert_equal(hour, 0, "hour")
      call assert_equal(minute, 1, "minute")
      call assert_equal(second, 0, "second")
   end subroutine test_add_half_a_second_on_minute_border
end module test_date_time_from_ref_date
