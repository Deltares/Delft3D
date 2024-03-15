!!  Copyright (C)  Stichting Deltares, 2012-2024.
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
      module m_read_time_delay
      use m_waq_precision


      implicit none

      contains


      subroutine read_time_delay ( ierr  )

!     Deltares Software Centre

!>\File
!>               Reads and handles time_delay tokens
!>
!>               The time delay is subtracted from the reference time to
!>               obtain a new, local reference time in 'deltim'.\n
!>               The time delay is read as 2 integers: yyyymmdd and hhmmss.\n
!>               Applying a time series starting at 01-01-2003 with a time
!>               delay of 1 year then means that the model applies it as if
!>               specified from 01-01-2004 on. This will cause a gap at the
!>               29th of February 2004. Applying the year 2004 for 2005 will
!>               case the value of 29-02-2004 being skipped in 2005.\n
!>               A negative time delay may be specified. Both integers should
!>               then be negative

!     Created            : March 1999  by Jan van Beek
!     Modified           :       2009  by Jan van Beek, Fortran 90 look and feel

!     Logical units      : lunut   = unit formatted output file

!     Global declarations

      use time_module
      use rd_token       ! for definition and storage of data
      use timers         ! performance timers
      use m_sysi          ! Timer characteristics

      implicit none

!     Arguments

!     kind           function         name       Descriptipon

      integer(kind=int_wp), intent(inout) ::  ierr      !< Cumulative error count

!     local declarations

      integer(kind=int_wp) :: idate    ! date integer from the input file
      integer(kind=int_wp) :: itime    ! time integer from the input file
      integer(kind=int_wp) :: ierr2    ! local error variable
      integer(kind=int_wp) :: iyear    ! year of delayed reference time
      integer(kind=int_wp) :: imonth   ! month of delayed reference time
      integer(kind=int_wp) :: iday     ! day of delayed reference time
      integer(kind=int_wp) :: ihour    ! hour of delayed reference time
      integer(kind=int_wp) :: imin     ! minute of delayed reference time
      integer(kind=int_wp) :: isec     ! second of delayed reference time
      real(kind=dp) :: dummy    ! second in double precision (not used)
      integer(kind=int_wp) ::  ithndl = 0
      if (timon) call timstrt( "read_time_delay", ithndl )

!     tell what you are doing here

      write ( lunut , 1000 )

!     get two integers date and time

      if ( gettoken(idate,ierr2) /= 0 ) goto 900
      if ( gettoken(itime,ierr2) /= 0 ) goto 900

      write ( lunut , 1010 ) idate, itime

!     convert Julian time offset of the system time to integers

      call gregor ( otime  , iyear  , imonth , iday   , ihour  , & 
                   imin   , isec   , dummy)

!     subtract the time delay

      isec   = isec   - mod(itime,  100)
      if ( isec < 0 ) then
         imin  = imin - 1
         isec  = isec +60
      endif
      imin   = imin   - mod(itime,10000)/100
      if ( imin < 0 ) then
         ihour = ihour- 1
         imin  = imin +60
      endif
      ihour  = ihour  -     itime       /10000
      if ( ihour < 0 ) then
         iday  = iday - 1
         ihour = ihour+24
      endif
      iday   = iday   - mod(idate,  100)
      if ( iday <= 0 ) then
         imonth = imonth - 1
         select case ( imonth )
            case ( 1, 3, 5, 7, 8, 10, 12 )
               iday = iday + 31
            case ( 2 )
               if ( mod(iyear,4) == 0 ) then
                  iday = iday + 29
               else
                  iday = iday + 28
               endif
            case default
               iday = iday + 30
         end select
      endif
      imonth = imonth - mod(idate,10000)/100
      if ( imonth<= 0 ) then
         iyear = iyear - 1
         imonth= imonth+12
      endif
      iyear  = iyear  -     idate       /10000
      idate  = iyear*10000 + imonth*100 + iday
      itime  = ihour*10000 + imin  *100 + isec

!     compute the Julian time of the result

      deltim = julian_with_leapyears ( idate , itime )

!       write meaningfull message to check procedure

      write ( lunut , 1020 ) iday , imonth, iyear , ihour, imin  , isec

      if (timon) call timstop( ithndl )
      return

!     error handling

  900 write ( lunut , 1030 )
      ierr = ierr + 1
      if (timon) call timstop( ithndl )
      return

 1000 format (  ' TIME_DELAY for ODS file input' )
 1010 format (  ' Delay integers: IDATE = ',i6,', ITIME = ',i6,'.' )
 1020 format (  ' New reference time is day: ',i2,'-',i2,'-',i4, & 
               ' / ',i2,'H-',i2,'M-',i2,'S.' )
 1030 format ( /' ERROR: the TIME_DELAY keyword is specified without', & 
               ' a valid value string for the delay !'/' 2 integers', & 
               ' are expected in YYMMDD and HHMMSS format !')
      end

      end module m_read_time_delay
