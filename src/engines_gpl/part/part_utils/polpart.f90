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
module m_polpart

implicit none

contains


      subroutine polpart( pol_file, nrowsmax, xpol, ypol, nrows, lunpr   )

!
!     programmer : michelle jeuken
!     function   : read a single polygon from a file
!     date       : september 2013
!
!
!     method     : read a single polygon from a tekal formated file (only the first is used!)
!
      use m_skip_comments
      use m_waq_precision ! single/double precision
      use timers
      use m_stop_exit

      implicit none ! force explicit typing

!     Arguments

!     kind           function         name                      description

      character( * ), intent(in   ) :: pol_file                !< polygon file
      integer  ( int_wp ), intent(in   ) :: nrowsmax                !< dimension of poligons
      real     ( real_wp), intent(  out) :: xpol  (nrowsmax)        !< xvalues polygons
      real     ( real_wp), intent(  out) :: ypol  (nrowsmax)        !< yvalues polygons
      integer  ( int_wp ), intent(  out) :: nrows                   !< dimension of poligon read
      integer  ( int_wp ), intent(in   ) :: lunpr                   !< unit nr of the diagnostics file

      integer(int_wp ), parameter            :: max_len_line=200
      integer(int_wp ), parameter            :: max_len_blockname=4
      integer(int_wp ), parameter            :: max_len_key=20

      integer(int_wp )                       :: lun_pol
      integer(int_wp )                       :: ios, ncols
      integer(int_wp )                       :: i, len_file

      logical                           :: polygone_complete
      logical                           :: end_of_file,read_error

      character(len=max_len_blockname) :: blok

      integer(4) ithndl              ! handle to time this subroutine
      data       ithndl / 0 /
      if ( timon ) call timstrt( "polpart", ithndl )

      len_file          =  len_trim(pol_file)

      open(newunit=lun_pol,file=pol_file,status='old',iostat=ios)
      if (ios /= 0) go to 900

      write(lunpr,*) ' '
      write(lunpr,*) 'Reading polygon from: ', trim(pol_file)
      write(lunpr,*) 'Only the first polygon will be read!'
      write(lunpr,*) ' '
!
!        read polygone (tekal format)
!
      call skip_comment_lines(lun_pol,ios)
      end_of_file = ios < 0
      read_error  = ios > 0
      if (end_of_file) go to 920
      if (read_error ) go to 930

      read(lun_pol,'(a)',iostat=ios) blok
      end_of_file = ios < 0
      read_error  = ios > 0
      if (end_of_file) go to 920
      if (read_error ) go to 930

      call skip_comment_lines(lun_pol,ios)
      end_of_file = ios < 0
      read_error  = ios > 0
      if (end_of_file) go to 920
      if (read_error ) go to 930

      read(lun_pol,*,iostat=ios) nrows,ncols
      end_of_file = ios < 0
      read_error  = ios > 0
      if (end_of_file) go to 920
      if (read_error ) go to 930

      do i=1,nrows
         polygone_complete=.false.
         read(lun_pol,*,iostat=ios) xpol(i), ypol(i)
         end_of_file = ios < 0
         read_error  = ios > 0
         if (end_of_file) go to 920
         if (read_error ) go to 930
      enddo
      close(lun_pol)

      if ( timon ) call timstop ( ithndl )
      return
!     error handling

  900 write(*,'(//a,a)')       ' Error: problem with pol-file ',pol_file(:len_file)
      write(*,'(a)')           ' Could not open/find pol-file ??'
      write(lunpr,'(//a,a)')   ' Error: problem with pol-file ',pol_file(:len_file)
      write(lunpr,'(a,a)')     ' Could not open/find pol-file ??'
      call stop_exit(1)

  920 write(*,'(//a,a)')       ' Error: problem with pol-file ',pol_file(:len_file)
      write(*,'(//a,a)')       ' End-of-file found on pol-file '
      write(lunpr,'(//a,a)')   ' Error: problem with pol-file ',pol_file(:len_file)
      write(lunpr,'(//a,a)')   ' End-of-file found on pol-file '
      call stop_exit(1)

  930 write(*,'(//a,a)')       ' Error: problem with pol-file ',pol_file(:len_file)
      write(*,'(//a,a)')       ' Error while reading pol-file'
      write(lunpr,'(//a,a)')   ' Error: problem with pol-file ',pol_file(:len_file)
      write(lunpr,'(//a,a)')   ' Error while reading pol-file'
      call stop_exit(1)

      end subroutine polpart

end module m_polpart
