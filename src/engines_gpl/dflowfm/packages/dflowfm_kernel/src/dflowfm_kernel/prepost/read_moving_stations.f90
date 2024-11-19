!----- AGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2017-2024.
!
!  This file is part of Delft3D (D-Flow Flexible Mesh component).
!
!  Delft3D is free software: you can redistribute it and/or modify
!  it under the terms of the GNU Affero General Public License as
!  published by the Free Software Foundation version 3.
!
!  Delft3D  is distributed in the hope that it will be useful,
!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!  GNU Affero General Public License for more details.
!
!  You should have received a copy of the GNU Affero General Public License
!  along with Delft3D.  If not, see <http://www.gnu.org/licenses/>.
!
!  contact: delft3d.support@deltares.nl
!  Stichting Deltares
!  P.O. Box 177
!  2600 MH Delft, The Netherlands
!
!  All indications and logos of, and references to, "Delft3D",
!  "D-Flow Flexible Mesh" and "Deltares" are registered trademarks of Stichting
!  Deltares, and remain the property of Stichting Deltares. All rights reserved.
!
!-------------------------------------------------------------------------------

submodule(m_observations) m_read_moving_stations

   implicit none

contains

!> reads moving observation points from *.ini file and sets EC module calls.
   module subroutine read_moving_stations(obs_filenames)
      use precision, only: hp
      use m_GlobalParameters, only: maxlenpar
      use properties
      use timespace_parameters
      use m_meteo, only: ec_addtimespacerelation, initialize_ec_module
      use unstruc_files, only: basename
      use m_missing, only: dmiss
      use string_module, only: strcmpi, strsplit
      use messagehandling, only: Idlen, LEVEL_INFO, LEVEL_ERROR, msgbuf, msg_flush, warn_flush, SetMessage

      character(len=*), intent(in) :: obs_filenames

      character(len=200), dimension(:), allocatable :: file_names

      ! Observation points file current version: 2.01
      integer, parameter :: ObsFileMajorVersion = 2
      integer, parameter :: ObsFileMinorVersion = 1
      logical :: is_successful, is_existed
      type(tree_data), pointer :: md_ptr
      integer :: istat
      integer :: numstr
      integer :: i, j

      character(len=IdLen) :: location_file
      character(len=IdLen) :: quantity
      character(len=IdLen) :: objid !< Id of the object for which this relation is set.
      character(len=IdLen) :: station_name
      integer :: major, minor
      integer, parameter :: VECTOR_MAX = 2
      integer, dimension(1) :: kdum
      real(hp), dimension(1) :: xdum, ydum
      integer :: num_mov_obs

      if (len_trim(obs_filenames) <= 0) then
         return
      end if

      kdum = 1
      xdum = 1.0
      ydum = 1.0
      quantity = 'movingstationtxy'

      call strsplit(obs_filenames, 1, file_names, 1)
      do j = 1, size(file_names)
         inquire (file=file_names(j), exist=is_existed)

         if (.not. is_existed .or. index(file_names(j), '.ini') <= 0) then
            cycle
         end if

         call SetMessage(LEVEL_INFO, 'Reading Moving Observation Points from '''//trim(file_names(j))//'''...')

         call tree_create(trim(file_names(j)), md_ptr, maxlenpar)
         call prop_file('ini', trim(file_names(j)), md_ptr, istat)

         ! check FileVersion
         major = 0
         minor = 0
         call get_version_number(md_ptr, major=major, minor=minor, success=is_successful)
         if (.not. is_successful .or. major < ObsFileMajorVersion) then
            write (msgbuf, '(a,i0,".",i2.2,a,i0,".",i2.2,a)') 'Unsupported format of observation point file detected in '''// &
               trim(file_names(j))//''': v', major, minor, '. Current format: v', ObsFileMajorVersion, ObsFileMinorVersion, &
               '. Ignoring this file.'
            call warn_flush()
            call tree_destroy(md_ptr)
            return
         end if

         numstr = 0
         if (associated(md_ptr%child_nodes)) then
            numstr = size(md_ptr%child_nodes)
         end if

         num_mov_obs = nummovobs

         do i = 1, numstr
            if (strcmpi(tree_get_name(md_ptr%child_nodes(i)%node_ptr), 'ObservationPoint')) then
               call prop_get(md_ptr%child_nodes(i)%node_ptr, '', 'locationFile', location_file, is_successful)
               if (.not. is_successful) then
                  cycle
               end if

               station_name = ' '
               call basename(location_file, station_name)
               call addObservation(dmiss, dmiss, station_name, isMoving=.true.)

               ! Time-interpolated value will be placed in target array (e.g., qplat(n)) when calling ec_gettimespacevalue.
               if (index(trim(location_file)//'|', '.tim|') > 0) then
                  ! Converter will put 'x' in array(2*nummovobs-1) and 'y' in array(2*nummovobs).
                  is_successful = ec_addtimespacerelation(quantity, xdum, ydum, kdum, VECTOR_MAX, location_file, &
                                                          filetype=UNIFORM, method=SPACEANDTIME, operand='O', &
                                                          targetIndex=nummovobs)
                  if (.not. is_successful) then
                     call SetMessage(LEVEL_ERROR, 'Error initializing locationFile '''//trim(location_file)//''', referenced in file''' &
                                     //trim(file_names(j))//''' .')
                  end if
               elseif (index(trim(location_file)//'|', '.bc|') > 0) then
                  ! not yet operational
                  is_successful = ec_addtimespacerelation(quantity, xdum, ydum, kdum, VECTOR_MAX, objid, &
                                                          filetype=bcascii, &
                                                          method=spaceandtime, &
                                                          operand='O', &
                                                          targetIndex=nummovobs, &
                                                          forcingFile=location_file)
               else
                  call SetMessage(LEVEL_ERROR, 'Error reading locationFile '''//trim(location_file)//''', referenced in file''' &
                                  //trim(file_names(j))//''' . Only .bc and .tim formats are supported.')
               end if

            end if
         end do

         write (msgbuf, '(i10,2a)') nummovobs - num_mov_obs, ' moving observation points have been read from file ', &
            trim(file_names(j))
         call msg_flush()

         call tree_destroy(md_ptr)
      end do

      deallocate (file_names)

   end subroutine read_moving_stations

end submodule m_read_moving_stations
