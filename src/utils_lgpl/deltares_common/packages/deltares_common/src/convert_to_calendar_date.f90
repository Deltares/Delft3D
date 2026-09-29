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

module m_convert_to_calender_date
   use precision, only: dp

   implicit none(type, external)

contains

   !> Convert a Julian day number to a calendar date (year, month, day).
   subroutine convert_julian_day_number_to_calender_date(julian_day_number, year, month, day)

   ! Arguments
   integer, intent(in) :: julian_day_number !< Integer Julian day number (days since 4713 BC)
   integer, intent(out) :: year !< Converted year
   integer, intent(out) :: month !< Converted month of the year
   integer, intent(out) :: day !< Converted day of the month

   ! Local variables
   integer :: alpha
   integer :: a
   integer :: b
   integer :: c
   integer :: d
   integer :: e
   integer, parameter :: GREGORIAN_CALENDAR_REFORM_DATE = 2299161 !< Gregorian calendar reform date (October 15, 1582)

   if (julian_day_number >= GREGORIAN_CALENDAR_REFORM_DATE) then
      alpha = int(((julian_day_number - 1867216) - 0.25) / 36524.25)
      a = julian_day_number + 1 + alpha - int(0.25 * alpha)
   else
      a = julian_day_number
   end if

   ! Convert Julian day number to calendar date using an unknown algorithm
   b = a + 1524
   c = int(6680.0_dp + ((b - 2439870) - 122.1_dp) / 365.25_dp)
   d = 365 * c + int(0.25_dp * c)
   e = int((b - d) / 30.6001_dp)
   day = b - d - int(30.6001_dp * e)
   month = e - 1

   if (month > 12) then
      month = month - 12
   end if

   year = c - 4715

   if (month > 2) then
      year = year - 1
   end if

   ! Correction for years before the common era (BC)
   if (year <= 0) then 
      year = year - 1
   end if

   end subroutine convert_julian_day_number_to_calender_date
end module m_convert_to_calender_date
