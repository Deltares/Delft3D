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

module openfl_mod
!
!  module declarations
!
!
!  data definition module(s)
!
use m_stop_exit
use m_waq_precision              ! single and double precision
use timers
!
!  module procedure(s)
!
use delete_file_mod        ! explicit interface
!
implicit none              ! force explicit typing
!
contains
      subroutine openfl ( lun, finam, iopt)
!
!
!     Deltares
!
!     subroutines called : delete_file
!                          stop_exit
!
!     function           : open file
!
!
!*********************************************************************
!     system dependent routine
!     configuration
!
!     unix systems
!*********************************************************************
!
!     logical units      : -
!
!     parameters    :
!
!     name    kind       length      funct.  description
!     ---------------------------------------------------------
!     finam   character(len=80) 1         input   file name
!     ierror  integer      1         local   error in delete_file
!     iopt    integer      1         input   if 1 delete existing file
!                                            with same name
!     lun     integer      1         input   unit number file
!     ---------------------------------------------------------
!
      implicit none

      character(len=256) :: finam
!
!     local scalars
!
      integer(int_wp ) :: iopt, ierror
      integer(int_wp ) :: lun
      integer(4) ithndl              ! handle to time this subroutine
      data       ithndl / 0 /
      if ( timon ) call timstrt( "openfl", ithndl )

!
      select case ( iopt )
         case ( 1 )
            call delete_file ( finam, ierror )
            open ( newunit = lun, file = finam, access='stream', form='unformatted', err = 99)
         case ( 0 )
            open ( newunit = lun, file = finam, access='stream', form='unformatted', status = 'old', err = 99)
         case ( 3 )
            open ( newunit = lun, file = finam, access='stream', form='unformatted', status = 'old', err = 10)
      end select
!
      if ( timon ) call timstop ( ithndl )
      return

   10 write(*,'(/a,a40/)') ' Warning, file does not exist: ',finam
      lun = 0
      if ( timon ) call timstop ( ithndl )
      return
!
 99   write(*,'(//a,a40)') ' Error on opening file: ',finam
      write(*,'(  a    )') ' Please check if file exists'
      write(*,'(  a    )') ' Please check correct file type'
      call stop_exit(1)
!
      end subroutine
end module
