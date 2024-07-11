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

      subroutine read_lga(file_lga, num_columns  , num_rows  , num_layers , nosegl, &
                          num_exchanges_u_dir    , num_exchanges_v_dir  , num_exchanges_z_dir  , lgrid )

      ! read a lga file and check dimensions
      use m_logger_helper, only : stop_with_error, get_log_unit_number

      use m_waq_file                   ! module contains everything for the files
      implicit none

      ! declaration of the arguments

      type(t_file)                       :: file_lga               ! aggregation-file
      integer                                :: num_columns                   ! grid cells m direction
      integer                                :: num_rows                   ! grid cells n direction
      integer                                :: num_layers                  ! num_layers
      integer                                :: nosegl                 ! nosegl
      integer                                :: num_exchanges_u_dir                   ! num_exchanges_u_dir
      integer                                :: num_exchanges_v_dir                   ! num_exchanges_v_dir
      integer                                :: num_exchanges_z_dir                   ! num_exchanges_z_dir
      integer                                :: lgrid(num_rows,num_columns)       ! active grid table

      ! local declarations

      integer                                :: mmaxd                  ! grid cells m direction from lga file
      integer                                :: nmaxd                  ! grid cells n direction from lga file
      integer                                :: ioerr                  ! error on file
      integer                                :: m                      ! loop counter
      integer                                :: n                      ! loop counter
      integer                                :: lunrep                 ! unit number report file

      call get_log_unit_number(lunrep)

      call file_lga%open()
      read(file_lga%unit,iostat=ioerr) nmaxd, mmaxd, nosegl, num_layers, num_exchanges_u_dir, num_exchanges_v_dir, num_exchanges_z_dir
      if ( ioerr .ne. 0 ) then
         write(lunrep,*) ' error reading lga file'
         call stop_with_error()
      endif

      if ( nmaxd.ne.num_rows .or. mmaxd.ne.num_columns ) then
         write(lunrep,*) ' dimensions lga file differ from input hydrodynamics'
         write(lunrep,*) ' num_columns hyd file:',num_columns
         write(lunrep,*) ' num_rows hyd file:',num_rows
         write(lunrep,*) ' num_columns lga file:',mmaxd
         write(lunrep,*) ' num_rows lga file:',nmaxd
         call stop_with_error()
      endif

      read(file_lga%unit,iostat=ioerr) ((lgrid(n,m),n=1,num_rows),m=1,num_columns)
      if ( ioerr .ne. 0 ) then
         write(lunrep,*) ' error reading lga file'
         call stop_with_error()
      endif

      return
      end
