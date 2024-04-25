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
      module m_write_error_message
      use m_srstop
      use m_monsys


      implicit none

      contains


      subroutine write_error_message(string)
!

      integer :: LUNREP
      character*(*) string
!
      call getmlu(LUNREP)
      if ( LUNREP .ne. 0 ) then
         write(LUNREP,'(/,1x,a,/)') string
      else
         write(*,'(/,1x,a,/)') string
      endif
!
      call srstop(1)
!

      end subroutine write_error_message



      subroutine write_error_message_with_values ( name  , value , iseg  , module )
      character*(*) name
      real          value
      integer       iseg
      character*(*) module

      integer       lunrep

!     message to screen

      write (*,*) ' '
      write (*,*) ' coefficient value out of range'
      write (*,*) ' coefficient name:',name
      write (*,*) ' coefficient value',value
      if ( iseg .gt. 0 ) write(*,*) ' in segment number:',iseg
      write (*,*) ' in subroutine',module

!     message to monitor or report file

      call getmlu(lunrep)
      if ( lunrep .gt. 0 ) then
         write (lunrep,*) ' '
         write (lunrep,*) ' coefficient value out of range'
         write (lunrep,*) ' coefficient name:',name
         write (lunrep,*) ' coefficient value',value
         if ( iseg .gt. 0 ) write(lunrep,*) ' in segment number:',iseg
         write (lunrep,*) ' in subroutine ',module
      endif

      call srstop(1)

      end subroutine write_error_message_with_values

      end module m_write_error_message



