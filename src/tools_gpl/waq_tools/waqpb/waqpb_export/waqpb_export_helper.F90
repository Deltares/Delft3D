!----- GPL ---------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2011-2026.
!
!  This program is free software: you can redistribute it and/or modify
!  it under the terms of the GNU General Public License as published by
!  the Free Software Foundation version 3.
!
!  This program is distributed in the hope that it will be useful,
!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!  GNU General Public License for more details.
!
!  You should have received a copy of the GNU General Public License
!  along with this program.  If not, see <http://www.gnu.org/licenses/>.
!
!  contact: delft3d.support@deltares.nl
!  Stichting Deltares
!  P.O. Box 177
!  2600 MH Delft, The Netherlands
!
!  All indications and logos of, and references to, "Delft3D" and "Deltares"
!  are registered trademarks of Stichting Deltares, and remain the property of
!  Stichting Deltares. All rights reserved.
!
module m_waqpb_export_helper
   implicit none

contains

   !> Generate the version number based on the current waq "major.minor" version
   pure function generate_version() result(version)
      use waq_static_version_info, only: major, minor

      real :: version !> The generated version
      character(len=20) version_str

      version_str = trim(major)//'.'//trim(minor)
      read (version_str, *) version
   end function generate_version

   !> generates a serial integer number consisting of year, month, day and hour
   !! yyyyMMddHH
   !! where:
   !! yyyy is the current year
   !! MM   is the current month number
   !! dd   is the current day number
   !! HH   is the current hour in 24 hours format
   function generate_serial() result(serial_number)
      integer :: serial_number !> The generated serial number

      character(len=14) :: datetime_string
      integer :: year, month, day, hour
      integer, dimension(8) :: date_time_values !> array that will retrieve (/year, month, day, timezone, hour, minute, second, millisecond/))

      call date_and_time(values=date_time_values)

      ! Assign useful values from the array to individual variables
      year = date_time_values(1)
      month = date_time_values(2)
      day = date_time_values(3)
      hour = date_time_values(5)

      ! Create a string with the date and time in yyyyMMddHH format
      write (datetime_string, '(I4, I2.2, I2.2, I2.2, I2.2)') year, month, day, hour

      ! Convert the string to an integer
      read (datetime_string, *) serial_number
   end function generate_serial

end module m_waqpb_export_helper
