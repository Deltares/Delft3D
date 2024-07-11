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

      subroutine read_poi(file_poi, num_exchanges   , num_exchanges_u_dir    , num_exchanges_v_dir  , num_exchanges_z_dir  , ipoint  )
      ! function : read a poi file and check dimensions
      use m_logger_helper, only : stop_with_error, get_log_unit_number
      use m_waq_file                   ! module contains everything for the files
      implicit none

      ! declaration of the arguments

      type(t_file)                       :: file_poi               ! pointer-file
      integer                                :: num_exchanges                    ! num_exchanges
      integer                                :: num_exchanges_u_dir                   ! num_exchanges_u_dir
      integer                                :: num_exchanges_v_dir                   ! num_exchanges_v_dir
      integer                                :: num_exchanges_z_dir                   ! num_exchanges_z_dir
      integer                                :: ipoint(4,num_exchanges)          ! pointer table

      ! local declarations

      integer                                :: i,j,ip1,ip2            ! indxes in pointer table
      integer                                :: ioerr                  ! error on file
      integer                                :: lunrep                 ! unit number report file

      call get_log_unit_number(lunrep)

      call file_poi%open()

      if ( num_exchanges_u_dir .gt. 0 ) then
         read(file_poi%unit,iostat=ioerr) ((ipoint(i,j),i=1,4),j=1,num_exchanges_u_dir)
         if ( ioerr .ne. 0 ) then
            write(lunrep,*) ' error reading poi file'
            call stop_with_error()
         endif
      endif
      if ( num_exchanges_v_dir .gt. 0 ) then
         ip1 = num_exchanges_u_dir + 1
         ip2 = num_exchanges_u_dir + num_exchanges_v_dir
         read(file_poi%unit,iostat=ioerr) ((ipoint(i,j),i=1,4),j=ip1,ip2)
         if ( ioerr .ne. 0 ) then
            write(lunrep,*) ' error reading poi file'
            call stop_with_error()
         endif
      endif
      if ( num_exchanges_z_dir .gt. 0 ) then
         ip1 = num_exchanges_u_dir + num_exchanges_v_dir + 1
         ip2 = num_exchanges_u_dir + num_exchanges_v_dir + num_exchanges_z_dir
         read(file_poi%unit,iostat=ioerr) ((ipoint(i,j),i=1,4),j=ip1,ip2)
         if ( ioerr .ne. 0 ) then
            write(lunrep,*) ' error reading poi file'
            call stop_with_error()
         endif
      endif

      close(file_poi%unit)
      file_poi%status = FILE_STAT_UNOPENED

      return
      end
