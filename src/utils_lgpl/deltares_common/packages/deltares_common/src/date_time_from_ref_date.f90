!----- AGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2017-2025.
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

module m_date_time_from_ref_date
   implicit none
   private

   public :: date_time_from_ref_date
   integer, parameter, public :: ref_date_len = 8

contains
   !> Calculate absolute date time values, given a time in seconds since ref_date.
   !! \see seconds_to_datetimestring
   subroutine date_time_from_ref_date(time_since_ref, ref_date, year, month, day, hour, minute, second)
      use m_julday
      use precision, only: dp
      real(kind=dp), intent(in) :: time_since_ref !< Time in seconds since ref_date
      character(len=ref_date_len), intent(in) :: ref_date !< Reference date
      integer, intent(out) :: year, month, day, hour, minute, second !< Actual date, split up in year/month, etc.

      integer :: ref_julian_day, ref_year, ref_month, ref_day, days_since_ref, hours_since_ref, minutes_since_ref, seconds_since_ref

      ! Round to seconds/integer to avoid precision issues and ensure datestamps like 20240828_003000 instead of 20240828_002960.
      ! 59.7 seconds is simply rounded to a full minute.
      seconds_since_ref = nint(time_since_ref)

      read (ref_date(1:4), *) ref_year
      read (ref_date(5:6), *) ref_month
      read (ref_date(7:8), *) ref_day

      ref_julian_day = julday(ref_month, ref_day, ref_year)
      minutes_since_ref = seconds_since_ref / 60
      hours_since_ref = minutes_since_ref / 60
      days_since_ref = hours_since_ref / 24

      call caldat(ref_julian_day + days_since_ref, month, day, year)

      hour = mod(hours_since_ref, 24)
      minute = mod(minutes_since_ref, 60)
      second = mod(seconds_since_ref, 60)

   end subroutine date_time_from_ref_date
end module m_date_time_from_ref_date
