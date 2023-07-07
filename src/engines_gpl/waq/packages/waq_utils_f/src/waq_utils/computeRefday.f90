!!  Copyright (C)  Stichting Deltares, 2012-2023.
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

! @file
!     Auxiliary routines to compute the reference day number (refday),
!     needed for the dayl process.
!     Refday varies between 1 and 365 (or 366 for leap years)
!
!>
module computeRefday

    implicit none
      interface compute_refday
         module procedure compute_refday_from_string
         module procedure compute_refday_from_integers
      end interface
    contains






      !     Compute reference day, varying from 1 till 365 (or 366 for leap years)
      subroutine compute_refday_from_string(refdat, refday)
        IMPLICIT  NONE
        character (len=*), intent(in)    ::   refdat             !  refdate
        integer  ( 4)                    ::   iyear              !  year of the time offset
        integer  ( 4)                    ::   imonth             !  month of the time offset
        integer  ( 4)                    ::   iday               !  day of the time offset
        integer  ( 4), intent(out)       ::   refday             !  refday
        
        read(refdat, '(i4,i2,i2)' ) iyear, imonth, iday
        call compute_refday_from_integers(iyear, imonth, iday, refday)
        
      end subroutine compute_refday_from_string
      
      
      !     Compute reference day, varying from 1 till 365 (or 366 for leap years)
      subroutine compute_refday_from_integers(iyear, imonth, iday, refday)
        IMPLICIT  NONE
        integer  ( 4), intent(in)  ::   iyear              !  year of the time offset
        integer  ( 4), intent(in)  ::   imonth             !  month of the time offset
        integer  ( 4), intent(in)  ::   iday               !  day of the time offset
        integer  ( 4), intent(out) ::   refday             !  refday
        
        integer, dimension(12)     ::   daysPerMonth       !  # days in each month
        logical                    ::   leapYear           !  is iyear a leap year, yes or no
        
        refday = 0
        call checkLeapYear(iyear, leapYear)
        
        daysPerMonth = (/31,28,31,30,31,30,31,31,30,31,30,31/)
        if (leapYear) then
           daysPerMonth(2) = 29
        endif
        
        refday = sum(daysPerMonth(1:imonth-1)) + iday
        
      end subroutine compute_refday_from_integers
      
      
!     Check if year is a leap year
      subroutine checkLeapYear(iyear, leapYear)
        IMPLICIT  NONE
        integer  ( 4), intent(in)  ::   iyear              !  year of the time offset
        logical , intent(out)      ::   leapYear           !  is iyear a leap year, yes or no
        
        if (mod(iyear,4) == 0)   leapYear = .TRUE.
        if (mod(iyear,100) == 0) leapYear = .FALSE.
        if (mod(iyear,400) == 0) leapYear = .TRUE.

      end subroutine checkLeapYear

end module computeRefday
