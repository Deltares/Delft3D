!----- GPL ---------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2011-2024.
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
!-------------------------------------------------------------------------------
!
!

      subroutine read_srf(file_srf, num_columns  , num_rows  , nosegl, surf )

      ! read a srf file and check dimensions
      use m_logger_helper, only : stop_with_error, get_log_unit_number
      use m_waq_file                   ! module contains everything for the files
      implicit none

      ! declaration of the arguments

      type(t_file)                       :: file_srf               ! aggregation-file
      integer                                :: num_columns                   ! grid cells m direction
      integer                                :: num_rows                   ! grid cells n direction
      integer                                :: nosegl                 ! nosegl
      real                                   :: surf(nosegl)           ! property of the cells per layer

      ! local declarations

      integer                                :: mmaxd                  ! grid cells m direction from srf file
      integer                                :: nmaxd                  ! grid cells n direction from srf file
      integer                                :: i3,i4,i5,i6            ! nosegl from srf file
      integer                                :: ioerr                  ! error on file
      integer                                :: i                      ! loop counter
      integer                                :: lunrep                 ! unit number report file
      logical                                :: exists                 ! file should exist

      call get_log_unit_number(lunrep)

      inquire( file = file_srf%name, exist = exists )
      if ( .not. exists ) then
         write(lunrep,*) ' file does not exist: ', trim(file_srf%name)
         call stop_with_error()
      endif

      call file_srf%open()
      read(file_srf%unit,iostat=ioerr) nmaxd, mmaxd, i3, i4, i5, i6
      if ( ioerr .ne. 0 ) then
         write(lunrep,*) ' error reading file: ', trim(file_srf%name)
         call stop_with_error()
      endif

      if ( num_columns*num_rows .ne. mmaxd*nmaxd ) then
         write(lunrep,*) ' dimensions file ', trim(file_srf%name), ' differ from input hydrodynamics'
         call stop_with_error()
      endif

      read(file_srf%unit,iostat=ioerr) (surf(i),i=1,nosegl)
      if ( ioerr .ne. 0 ) then
         write(lunrep,*) ' error reading file: ', trim(file_srf%name)
         call stop_with_error()
      endif

      close(file_srf%unit)
      file_srf%status = FILE_STAT_UNOPENED

      return
      end

      subroutine read_hsrf(file_hsrf, num_cells, surf )

      ! read a horizontal srf file
      use m_logger_helper, only : stop_with_error, get_log_unit_number
      use m_waq_file                   ! module contains everything for the files
      implicit none

      ! declaration of the arguments

      type(t_file)                       :: file_hsrf              ! aggregation-file
      integer                                :: num_cells                  ! number of segments
      real                                   :: surf(num_cells)            ! horizontal surfaces

      ! local declarations

      integer                                :: i                      ! loop counter
      integer                                :: lunrep                 ! unit number report file
      integer                                :: idum                   ! dummy time label
      integer                                :: ioerr                  ! error on file
      logical                                :: exists                 ! file should exist

      call get_log_unit_number(lunrep)

      inquire( file = file_hsrf%name, exist = exists )
      if ( .not. exists ) then
         write(lunrep,*) ' file does not exist: ', trim(file_hsrf%name)
         call stop_with_error()
      endif

      call file_hsrf%open()
      read(file_hsrf%unit,iostat=ioerr) idum, (surf(i),i=1,num_cells)
      if ( ioerr .ne. 0 ) then
         write(lunrep,*) ' error reading horizontal srf file'
         call stop_with_error()
      endif

      close(file_hsrf%unit)
      file_hsrf%status = FILE_STAT_UNOPENED

      return
      end
