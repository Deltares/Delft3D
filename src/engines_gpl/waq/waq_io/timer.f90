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
      module m_waq_timer
      use m_waq_precision

      implicit none

      contains


      subroutine timer  ( dtflg1 , it1    , it2    , it3    , noopt  , & 
                         dtflg3 , ierr   )

!     Deltares Software Centre

!>\File
!>        Reads and reports timers
!>
!>        The start time, stop time and time step are read for\n
!>        noopt = 1 : the monitoring file\n
!>        noopt = 2 : the map        file\n
!>        noopt = 3 : the history    file

!     Created       : April 1988 by Marjolein Sileon / Leo Postma

!     Modified      : April 1997 by Rinze Bruinsma
!                                   Tokenized data file reading added
!     Modified      : April 2011 by Leo Postma
!                                   Fortran 90 look and feel

!     Logical units : lunut = unitnumber formatted output file

!     Subroutines called : conver  converts a 'DATE' integer to seconds
!                          convert_string_to_time_offset  converts an absolute time string to seconds

      use rd_token     !   for the reading of tokens
      use timers       !   performance timers
      use date_time_utils, only : convert_string_to_time_offset, convert_relative_time

      implicit none

!     Parameters

!     kind           function         name             Descriptipon

      logical      , intent(in   ) :: dtflg1         !< 'date'-format
      integer(kind=int_wp), intent(  out) ::  it1             !< start time
      integer(kind=int_wp), intent(  out) ::  it2             !< stop  time
      integer(kind=int_wp), intent(  out) ::  it3             !< time step
      integer(kind=int_wp), intent(in   ) ::  noopt           !< kind of timer
      logical      , intent(in   ) :: dtflg3         !< yydddhh instead of ddhhmmss
      integer(kind=int_wp), intent(  out) ::  ierr            !< not zero if error

!     Local

      integer(kind=int_wp) :: itype           !  help variable for tokenized reading
      character*255 cdummy         !  help variable for tokenized reading
      character*12  txt(3)
      integer(kind=int_wp) :: ierr2           !  local error variable
      data          txt / ' Monitoring ',' Output     ',' History    ' /
      integer(kind=int_wp) ::  ithndl = 0
      if (timon) call timstrt( "timer", ithndl )

!       Read timings

      ierr = 0
      if ( gettoken( cdummy, it1   , itype, ierr2 ) > 0 ) goto 9999
      if ( itype == 1 ) then
         call convert_string_to_time_offset ( cdummy, it1   , .false., .false., ierr )
         if ( it1 == -999. ) then
            write ( lunut , 2030 ) trim(cdummy)
            goto 9999
         endif
         if ( ierr /= 0 ) then
            write ( lunut , 2040 ) trim(cdummy)
            goto 9999
         endif
      else
         call convert_relative_time ( it1   , 1      , dtflg1 , dtflg3 )
      endif

      if ( gettoken( cdummy, it2   , itype, ierr  ) > 0 ) goto 9999
      if ( itype == 1 ) then
         call convert_string_to_time_offset ( cdummy, it2   , .false., .false., ierr )
         if ( it2 == -999. ) then
            write ( lunut , 2030 ) trim(cdummy)
            goto 9999
         endif
         if ( ierr /= 0 ) then
            write ( lunut , 2040 ) trim(cdummy)
            goto 9999
         endif
      else
         call convert_relative_time ( it2   , 1      , dtflg1 , dtflg3 )
      endif

      if ( gettoken( cdummy, it3   , itype, ierr  ) > 0 ) goto 9999
      if ( itype == 1 ) then
         call convert_string_to_time_offset ( cdummy, it3   , .false., .false., ierr )
         if ( it3 == -999. ) then
            write ( lunut , 2030 ) trim(cdummy)
            goto 9999
         endif
         if ( ierr /= 0 ) then
            write ( lunut , 2040 ) trim(cdummy)
            goto 9999
         endif
      else
         call convert_relative_time ( it3   , 1      , dtflg1 , dtflg3 )
      endif

      write ( lunut , 2000 ) txt(noopt)
      if ( dtflg1 ) then
         write (lunut,2010)it1/31536000       , mod(it1,31536000)/86400, & 
                          mod(it1,86400)/3600, mod(it1,3600)/60       , & 
                          mod(it1,60)        , & 
                          it2/31536000       , mod(it2,31536000)/86400, & 
                          mod(it2,86400)/3600, mod(it2,3600)/60       , & 
                          mod(it2,60)        , & 
                          it3/31536000       , mod(it3,31536000)/86400, & 
                          mod(it3,86400)/3600, mod(it3,3600)/60       , & 
                          mod(it3,60)
      else
           write ( lunut,2020 ) it1, it2, it3
      endif
      if (timon) call timstop( ithndl )
      return
 9999 ierr = ierr + 1
      if (timon) call timstop( ithndl )
      return

!       Output formats

 2000 format (//A12,' timings :')
 2010 format (  ' Start time :',I2,'Y-',I3,'D-',I2,'H-',I2,'M-',I2,'S ' & 
              /' Stop time  :',I2,'Y-',I3,'D-',I2,'H-',I2,'M-',I2,'S ' & 
              /' Time step  :',I2,'Y-',I3,'D-',I2,'H-',I2,'M-',I2,'S ')
 2020 format (  ' Start time :',I8, & 
              /' Stop time  :',I8     ,/,' Time step  :',I8)
 2030 format ( /' ERROR: Absolute timer does not fit in timer format :',A,/ & 
               ' Is your T0 setting in block #1 correct?'/, & 
               ' Allowed difference with T0 is usually ca. 68 years.' )
 2040 format ( /' ERROR: String is not a valid absolute timer :',A)
      end

      end module m_waq_timer
