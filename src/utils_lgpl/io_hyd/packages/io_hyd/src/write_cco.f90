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

      subroutine write_cco(file_cco, num_columns  , num_rows  , xdepth, ydepth, num_layers)

      ! function : write a cco file

      ! global declarations

      use m_logger_helper, only : stop_with_error
      use m_waq_file                   ! module contains everything for the files
      implicit none

      ! declaration of the arguments

      type(t_file)                       :: file_cco               ! aggregation-file
      integer                                :: num_columns                   ! grid cells m direction
      integer                                :: num_rows                   ! grid cells n direction
      real                                   :: xdepth(num_rows,num_columns)      ! x coordinate depth points
      real                                   :: ydepth(num_rows,num_columns)      ! y coordinate depth points
      integer                                :: num_layers                  ! num_layers

      ! local declarations

      real                                   :: x0                     ! x coordinate origin
      real                                   :: y0                     ! y coordinate origin
      real                                   :: alpha                  ! alpha
      integer                                :: npart                  ! npart
      integer                                :: ioerr                  ! error on file
      integer                                :: i                      ! loop counter
      real                                   :: rdum                   ! dummy
      integer       lun
      integer       filtyp
      integer       plform
      character*6   binary
      binary = 'BINARY'
      plform = PL_DOS

      x0    = 0.0
      y0    = 0.0
      alpha = 0.0
      npart = 0
      rdum  = 0.0

      call file_cco%open()
      lun    = file_cco%unit
      filtyp = file_cco%type

      if ( filtyp .ne. FT_ASC ) then
         write(file_cco%unit,iostat=ioerr) num_columns, num_rows, x0, y0, alpha, npart, num_layers
         if ( ioerr .ne. 0 ) then
            write(*,*) ' error writing cco file header record'
            call stop_with_error()
         endif

         do i=1 , 2*npart+9
            write(file_cco%unit,iostat=ioerr) rdum
            if ( ioerr .ne. 0 ) then
               write(*,*) ' error writing cco file dummy records'
               call stop_with_error()
            endif
         enddo

         write(file_cco%unit,iostat=ioerr) xdepth
         if ( ioerr .ne. 0 ) then
            write(*,*) ' error writing cco file xdepth'
            call stop_with_error()
         endif
         write(file_cco%unit,iostat=ioerr) ydepth
         if ( ioerr .ne. 0 ) then
            write(*,*) ' error writing cco file ydepth'
            call stop_with_error()
         endif
      else
         write(file_cco%unit,*,iostat=ioerr) num_columns, num_rows, x0, y0, alpha, npart, num_layers
         if ( ioerr .ne. 0 ) then
            write(*,*) ' error writing cco file header record'
            call stop_with_error()
         endif
         write(file_cco%unit,*,iostat=ioerr) xdepth
         if ( ioerr .ne. 0 ) then
            write(*,*) ' error writing cco file xdepth'
            call stop_with_error()
         endif
         write(file_cco%unit,*,iostat=ioerr) ydepth
         if ( ioerr .ne. 0 ) then
            write(*,*) ' error writing cco file ydepth'
            call stop_with_error()
         endif
      endif

      close(file_cco%unit)
      file_cco%status = FILE_STAT_UNOPENED

      return
      end
