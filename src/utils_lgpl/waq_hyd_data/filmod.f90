!----- LGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2011-2024.
!
!  This library is free software; you can redistribute it and/or
!  modify it under the terms of the GNU Lesser General Public
!  License as published by the Free Software Foundation version 2.1.
!
!  This library is distributed in the hope that it will be useful,
!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
!  Lesser General Public License for more details.
!
!  You should have received a copy of the GNU Lesser General Public
!  License along with this library; if not, see <http://www.gnu.org/licenses/>.
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
!-------------------------------------------------------------------------------
!  
!  

      module filmod
      use m_srstop
      use m_monsys


      ! module contains everything for the files
      ! created June 2004 by Jan van Beek

      implicit none

      integer, parameter, private :: FILE_NAME_SIZE    = 256          ! length filenames
      integer, parameter, private :: NAME_SIZE         =  20          ! size of descriptive names
      integer, parameter, private :: TEXT_SIZE         =  40          ! descriptive text size

      ! platform types

      integer, parameter          :: PL_DOS            =   1          ! DOS kind of platform
      integer, parameter          :: PL_UNX            =   2          ! UNIX kind of platform

      ! file system types

      integer, parameter          :: FS_DOS            =   1          ! DOS kind of files
      integer, parameter          :: FS_UNX            =   2          ! UNIX kind of files
      integer, parameter          :: FS_ASC            =   3          ! ASCII kind of files

      ! file types

      integer, parameter          :: FT_ASC            =   1          ! ASCII kind of file
      integer, parameter          :: FT_UNF            =   2          ! UNFORMATTED kind of file
      integer, parameter          :: FT_BIN            =   3          ! BINARY kind of file
      integer, parameter          :: FT_SDS            =   4          ! SIMONA kind of file
      integer, parameter          :: FT_NEF            =   5          ! ASCII kind of file

      ! file status

      integer, parameter          :: FILE_STAT_UNOPENED=   0          ! file not opened
      integer, parameter          :: FILE_STAT_OPENED  =   1          ! file openend
      integer, parameter          :: FILE_STAT_INIT    =   2          ! file initialised (header)
      integer, parameter          :: FILE_STAT_CLOSED  =   3          ! file closed

      ! data type to define a single file

      type t_dlwqfile
         character(len=FILE_NAME_SIZE)          :: name                   ! name of file
         character(len=TEXT_SIZE)               :: description            ! description of file
         integer                                :: unit_nr                ! unit number
         integer                                :: type                   ! file type to be used
         integer                                :: status                 ! status
      end type t_dlwqfile

      contains

      subroutine dlwqfile_open(dlwqfile)

      type(t_dlwqfile)                       :: dlwqfile               ! the file to be opened

      integer                                :: io_error               ! error indicator
      character(len=1000)                    :: io_msg                 ! error message
      integer                                :: lunrep                 ! unit number report file

      if ( dlwqfile%status .eq. 0 ) then
         if ( dlwqfile%type .eq. FT_ASC ) then
            open(newunit=dlwqfile%unit_nr,file=dlwqfile%name,status='unknown',iostat=io_error,iomsg=io_msg)
         elseif ( dlwqfile%type .eq. FT_BIN ) then
            open(newunit=dlwqfile%unit_nr,file=dlwqfile%name,status='unknown',access='stream',iostat=io_error,iomsg=io_msg)
         elseif ( dlwqfile%type .eq. FT_UNF ) then
            open(newunit=dlwqfile%unit_nr,file=dlwqfile%name,status='unknown',form='unformatted',iostat=io_error,iomsg=io_msg)
         else
            call getmlu(lunrep)
            write(*,*) 'ERROR opening file:',trim(dlwqfile%name)
            write(lunrep,*) 'ERROR opening file:',trim(dlwqfile%name)
            write(*,*) 'unknown filetype:', dlwqfile%type
            write(lunrep,*) 'unknown filetype:', dlwqfile%type
            call srstop(1)
         endif
         if ( io_error .ne. 0 ) then
            call getmlu(lunrep)
            write(*,*) 'ERROR opening file:',trim(dlwqfile%name)
            write(lunrep,*) 'ERROR opening file:',trim(dlwqfile%name)
            write(*,*) 'ERROR message: ', trim(io_msg)
            write(lunrep,*) 'ERROR message: ', trim(io_msg)
            call srstop(1)
         endif
         dlwqfile%status = 1
      endif

      end subroutine dlwqfile_open

      subroutine dlwqfile_close(dlwqfile)

      type(t_dlwqfile)                       :: dlwqfile               ! the file to be closed

      if ( dlwqfile%status .ne. 0 ) then
         close(dlwqfile%unit_nr)
         dlwqfile%status = 0
      endif

      end subroutine dlwqfile_close

      function dlwq_platform() result(platform)
         integer                                :: platform               ! result platform type
         platform = PL_DOS
      end function dlwq_platform

      end module filmod
