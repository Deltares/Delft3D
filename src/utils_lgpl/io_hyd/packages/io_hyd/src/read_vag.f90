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

      subroutine read_vag(file_vag, nolay, ipnt, lunrep )

      ! function : read a vag file (vertical aggregation) and check dimensions
      use m_logger_helper, only : stop_with_error
      use m_waq_file        ! module contains everything for the files
      implicit none

      ! declaration of the arguments
      type(t_file)                       :: file_vag               ! aggregation-file
      integer                                :: nolay                  ! number of layers
      integer                                :: ipnt(nolay)            ! aggregation pointer
      integer                                :: lunrep                 ! unit number report file

      ! local declarations
      integer                                :: nolayd                 ! number of layers from vag file
      integer                                :: i                      ! loop counter
      integer                                :: ioerr                  ! error on file
      logical                                :: ex                     ! existence of file

      inquire(file=file_vag%name, exist=ex)
      if(.not.ex) then
         write(lunrep,*) 'ERROR vertical aggregation file does not exist:',trim(file_vag%name)
         write(*,*) 'ERROR vertical aggregation file does not exist:',trim(file_vag%name)
         call stop_with_error()
      endif

      call file_vag%open()
      read(file_vag%unit,*,iostat=ioerr) nolayd
      if ( ioerr .ne. 0 ) then
         write(lunrep,*) ' error reading vag file'
         write(*,*) ' error reading vag file'
         call stop_with_error()
      endif

      if ( nolayd .eq. -1 ) then
         write(lunrep,*) ' found nolay of -1 in vertical aggregation file'
         write(lunrep,*) ' automatic aggregation of all layers (3D to 2D)'
         write(*,*) ' found nolay of -1 in vertical aggregation file'
         write(*,*) ' automatic aggregation of all layers (3D to 2D)'
         do i=1,nolay
            ipnt(i) = 1
         end do
      else
         if ( nolayd .ne. nolay ) then
            write(lunrep,*) ' dimensions grid on vertical aggregation file differ from input hydrodynamics'
            write(*,*) ' dimensions grid on vertical aggregation file differ from input hydrodynamics'
            call stop_with_error()
         endif

         read(file_vag%unit,*,iostat=ioerr) (ipnt(i),i=1,nolay)
         if ( ioerr .ne. 0 ) then
            write(lunrep,*) ' error reading vag file'
            write(*,*) ' error reading vag file'
            call stop_with_error()
         endif
      endif

      return
      end
