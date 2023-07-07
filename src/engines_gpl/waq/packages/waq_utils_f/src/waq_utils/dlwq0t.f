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

      module dlwq0t_data
      use m_zoek
      use m_julian

      
      integer :: dlwq0t_itstrt   ! Simulation start time ( scu )
      integer :: dlwq0t_itstop   ! Simulation stop time ( scu )
      integer :: dlwq0t_isfact   ! System timer in seconds
      real*8  :: dlwq0t_otime    ! Time base in Julian time
      
      end module dlwq0t_data

      subroutine dlwq0t ( chulp  , ihulp  , dtflg1 , dtflg3 , ierr   )

!     Deltares Software Centre
!>\File
!>            Detects absolute time string and converts to amount of sytem time after time offset
!>
!>            Output in ihulp can have the following forms:
!>            - in seconds if both flags are false, maximum 68 year
!>            - in YYYDDDHH if dtflg3 is true, maximum 21475 year
!>            - in DDHHMMSS if dtflq1 is true, maximum  2147 days = 5.88 year !

!     Created            : April '98 by L. Postma

!     Modified           : June  '08 by L. Postma to support long integers

      use timers       !   performance timers
      use dlwq0t_data

      implicit none

!     Subroutines called : none

!     External functions :

      integer        mod      !  modulo operation

!     Logical units      : none

!     Parameters    :

      character*( *), intent(in   ) :: chulp   !< string to be analised
      integer   ( 4), intent(  out) :: ihulp   !< system timer to get out
      logical       , intent(in   ) :: dtflg1  !< TRUE if date format
      logical       , intent(in   ) :: dtflg3  !< TRUE if HH instead of SS
      integer   ( 4), intent(inout) :: ierr    !< OUTPUT = 1 if string is no timer

!     Local

      character*(20) key      !  used to test content of chulp
      integer   ( 4) ikey     !  return value of 'zoek'
      integer   ( 4) iyear    !  workspace year value
      integer   ( 4) imonth   !  workspace month value
      integer   ( 4) iday     !  workspace day value
      integer   ( 4) ihour    !  workspace hour value
      integer   ( 4) iminut   !  workspace minute value
      integer   ( 4) isecnd   !  workspace second value
      integer   ( 4) idate    !  workspace date value
      integer   ( 4) itime    !  workspace time value
      real      ( 8) otim2    !  to compute distance from otime
      real      ( 8) afact    !  system clock in days
      real      ( 8) rhulp    !  help variable

      integer(4) :: ithndl = 0
      if (timon) call timstrt( "dlwq0t", ithndl )

      ierr = 0
      ihulp = 0
      
!     search for presence of special keywords start and stop

      key = 'START'
      call zoek( key, 1, chulp, 20, ikey )
      if ( ikey .gt. 0 ) then                       !  'start' string found
         ihulp = dlwq0t_itstrt
         goto 9999
      endif

      key = 'STOP'
      call zoek( key, 1, chulp, 20, ikey )
      if ( ikey .gt. 0 ) then                       !  'stop' string found
         ihulp = dlwq0t_itstop
         goto 9999
      endif

      ierr = 1
      if ( chulp( 5: 5) .ne. '/' .or. chulp( 8: 8) .ne. '/' .or.
     &     chulp(11:11) .ne. '-' .or. chulp(14:14) .ne. ':' .or.
     &     chulp(17:17) .ne. ':' ) goto 9999           ! the date string is VERY strict

      read ( chulp( 1: 4) , '(i4)' ) iyear
      read ( chulp( 6: 7) , '(i2)' ) imonth
      read ( chulp( 9:10) , '(i2)' ) iday
      read ( chulp(12:13) , '(i2)' ) ihour
      read ( chulp(15:16) , '(i2)' ) iminut
      read ( chulp(18:19) , '(i2)' ) isecnd
      idate  = iyear*10000+imonth*100+iday
      itime  = ihour*10000+iminut*100+isecnd
      otim2  = julian ( idate , itime )
      afact  = dlwq0t_isfact/864.0d+02
      if ( dlwq0t_isfact .lt. 0 ) afact = -1.0d+00/dlwq0t_isfact/864.0d+02     ! this should support
                                                                 ! time steps < 1 second
!     check if time will fit in the integer

      rhulp = (otim2 - dlwq0t_otime)/afact

!     no format maximum is 2**31 which is 2.147.483.648 or around 2.1E9 scu

      if ( abs(rhulp) .gt. 2.147e9 ) then
         ihulp = -999
         goto 9999
      endif

!     time will be set in format if necessary

      ihulp = rhulp + 0.5                                        ! for the round-off error
      if ( dtflg3 ) then                                         ! YYDDDHH format
         ihulp =  ihulp/3600                                     ! the hours
         ihulp = (ihulp/8760)*100000 + (mod(ihulp,8760)/24)*100  ! years of 365 days assumed
     &                               +  mod(ihulp,24)
      endif

      if ( dtflg1 ) then                                         ! DDHHMMSS format
         ihulp = (ihulp/86400)*1000000 + (mod(ihulp,86400)/3600)*10000
     &         + (mod(ihulp,3600)/60)*100 + mod(ihulp,60)
      endif

      ierr = 0
 9999 if (timon) call timstop( ithndl )
      return
      end
