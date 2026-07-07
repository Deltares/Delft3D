!----- AGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2017-2026.
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

!
!

module m_setbedlevelfromextfile
   use m_setbedlevelfromnetfile, only: setbedlevelfromnetfile

   implicit none

   private

   public :: setbedlevelfromextfile

contains

   subroutine setbedlevelfromextfile() ! setbedlevels()  ! check presence of old cell centre bottom level file
      use precision, only: dp
      use timespace_data
      use timespace
      use unstruc_model
      use m_flowgeom
      use m_flow
      use m_netw !  only : xk, yk, zk
      use m_missing
      use system_utils, only: split_filename
      use unstruc_files, only: resolvePath
      use string_module, only: strcmpi, str_tolower
      use dfm_error
      use unstruc_netcdf
      use fm_location_types, only: UNC_LOC_S, UNC_LOC_U, UNC_LOC_CN, SPATIAL_LOCATION_1D, SPATIAL_LOCATION_2D, SPATIAL_LOCATION_ALL, SPATIAL_LOCATION_INVALID, parse_spatial_location_type
      use m_delpol
      use m_timespaceinitialfield_mpi
      use m_spatial_field, only: t_spatial_field_input, read_spatial_field_block, validate_spatial_field_input
      use tree_structures, only: tree_create, tree_destroy, tree_num_nodes, tree_get_name

      logical :: bl_set_from_zkuni = .false.
      integer :: ja, ja1, method, iprimpos
      integer :: k, L, k1, k2, mx
      integer, allocatable :: kcc(:), kc1D(:), kc2D(:)
      integer :: ibathyfiletype
      integer :: kc_size_store

      character(len=256) :: filename
      character(len=64) :: varname
! character(len=1)   :: operand
! real(kind=dp)   :: transformcoef(25) !< Transform coefficients a+b*x

      type(tree_data), pointer :: inifield_ptr !< tree of inifield-file's [Initial] or [Parameter] blocks
      type(tree_data), pointer :: node_ptr
      integer :: istat
      integer :: num_items_in_file
      character(len=255) :: fnam
      character(len=255) :: basedir
      integer :: i, iLocType
      character(len=:), allocatable :: groupname
      logical :: parse_ok
      type(t_spatial_field_input) :: input
      character(len=256) :: ext_file_name
      character(len=256) :: preferred_bedlevel_file

      kc_size_store = 0
      inifield_ptr => null()

      ! Attempt to read cell centred bed levels directly from net file:
      call setbedlevelfromnetfile()
      call mess(LEVEL_INFO, 'setbedlevelfromextfile: Using bedlevel as specified in net-file.')

      ! ibedlevtyp determines from which source data location the bed levels are used to derive bobs and bl.
      ! These types need to be mapped to one of three possible primitive locations (center/edge/corner).
      select case (ibedlevtyp)
      case (1) ! position = waterlevelpoint, cell centre
         iprimpos = UNC_LOC_S
         mx = max(numk, ndx)
      case (2) ! position = velocitypoint, cellfacemid
         iprimpos = UNC_LOC_U
         mx = max(numk, lnx)
      case (3, 4, 5, 6) ! position = netnode, cell corner
         iprimpos = UNC_LOC_CN
         mx = numk
      end select

      if (mext /= 0 .or. len_trim(md_inifieldfile) > 0 .or. len_trim(md_extfile_new) > 0) then
         ! 0.a Prepare masks for 1D/2D distinctions
         kc_size_store = size(kc)
         allocate (kcc(mx), kc1d(mx), kc2d(max(lnxi, mx)))
         kcc = 1
         kc1D = 0
         kc2D = 0
         call realloc(kc, mx, keepExisting=.false., fill=0)

         do L = 1, numL1D
            if (kn(3, L) == 1 .or. kn(3, L) == 6) then ! TODO: AvD: why not also type 3/4/5/7?
               k1 = kn(1, L)
               k2 = kn(2, L)
               if (nmk(k1) > 1) then
                  kc1D(k1) = 1
               end if
               if (nmk(k2) > 1) then
                  kc1D(k2) = 1
               end if
            end if
         end do

         if (iprimpos == 3) then
            do L = 1, numL
               if (kn(3, L) == 2) then
                  k1 = kn(1, L)
                  k2 = kn(2, L)
                  kc2D(k1) = 1
                  kc2D(k2) = 1
               end if
            end do
         else if (iprimpos == 1) then
            kc2D(lnx1d + 1:lnxi) = 1
         else if (iprimpos == 2) then
            kc2D(1:ndx2D) = 1
         end if

         ja = 0
         ja1 = 0
         ! 0.b Prepare loop across old ext file:
         if (mext /= 0) then
            rewind (mext)
            ja1 = 1
         end if

         ! 0.c Read new-format initial/spatial fields from IniFieldFile and ExtForceFileNew.
         do ibathyfiletype = 1, 2
            if (ibathyfiletype == 1) then
               ext_file_name = trim(md_inifieldfile)
            else
               ext_file_name = trim(md_extfile_new)
            end if

            if (len_trim(ext_file_name) == 0) then
               cycle
            end if

            call tree_create(ext_file_name, inifield_ptr)
            call prop_file('ini', ext_file_name, inifield_ptr, istat)
            if (istat /= 0) then
               call tree_destroy(inifield_ptr)
               cycle
            end if

            call split_filename(ext_file_name, basedir, fnam)
            num_items_in_file = tree_num_nodes(inifield_ptr)

            do i = 1, num_items_in_file
               node_ptr => inifield_ptr%child_nodes(i)%node_ptr
               groupname = trim(tree_get_name(node_ptr))

               select case (str_tolower(groupname))
               case ('spatial', 'meteo', 'parameter', 'initial')
                  ! supported
               case default
                  cycle
               end select

               input = read_spatial_field_block(node_ptr)
               parse_ok = validate_spatial_field_input(input, ext_file_name, groupname, basedir)
               if (.not. parse_ok) then
                  cycle
               end if

               qid = input%quantity
               filename = input%forcing_file
               filetype = input%filetype
               method = input%method
               operand = input%oper
               varname = input%variable_name
               iLocType = parse_spatial_location_type(trim(input%location_type))

               success = .true.
                  if (strcmpi(qid, 'bedlevel1D') .or. (strcmpi(qid, 'bedlevel') .and. iLocType == SPATIAL_LOCATION_1D)) then
                  call mess(LEVEL_INFO, 'setbedlevelfromextfile: Setting 1D bedlevel from file '''//trim(filename)//''' (from '''//trim(ext_file_name)//''').')
                  kc(1:mx) = kc1D
                  success = timespaceinitialfield_mpi(xk, yk, zk, numk, filename, filetype, method, operand, transformcoef, UNC_LOC_CN, kc)
               else if (strcmpi(qid, 'bedlevel', 8)) then
                     if (strcmpi(qid, 'bedlevel') .and. (iLocType == SPATIAL_LOCATION_ALL .or. iLocType == SPATIAL_LOCATION_INVALID)) then
                     call mess(LEVEL_INFO, 'setbedlevelfromextfile: Setting both 1D and 2D bedlevel from file '''//trim(filename)//''' (from '''//trim(ext_file_name)//''').')
                     kc(1:mx) = kcc
                  else if (strcmpi(qid, 'bedlevel2D') .or. (strcmpi(qid, 'bedlevel') .and. iLocType == SPATIAL_LOCATION_2D)) then
                     call mess(LEVEL_INFO, 'setbedlevelfromextfile: Setting 2D bedlevel from file '''//trim(filename)//''' (from '''//trim(ext_file_name)//''').')
                     kc(1:mx) = kc2D
                  else
                     cycle
                  end if

                  if (ibedlevtyp == 3) then
                     success = timespaceinitialfield_mpi(xk, yk, zk, numk, filename, filetype, method, operand, transformcoef, iprimpos, kc)
                  else if (ibedlevtyp == 2) then
                     success = timespaceinitialfield_mpi(xu, yu, blu, lnx, filename, filetype, method, operand, transformcoef, iprimpos, kc)
                  else if (ibedlevtyp == 1) then
                     success = timespaceinitialfield_mpi(xz, yz, bl, ndx, filename, filetype, method, operand, transformcoef, iprimpos, kc)
                  end if
               end if

               if (.not. success) then
                  call mess(LEVEL_FATAL, 'Error reading '//trim(qid)//' from '//trim(filename)//'.')
               end if
            end do

            call tree_destroy(inifield_ptr)
         end do

         ! 0.d Legacy support for old *.ext format.
         if (ja1 == 1) then
            call split_filename(md_extfile, basedir, fnam)
            ja = 1
         end if

         do while (ja == 1)
            call delpol()
            call readprovider(mext, qid, filename, filetype, method, operand, transformcoef, ja, varname)

            ! Initialize bedlevel based on the read provider info
            if (ja == 1) then
               call resolvePath(filename, basedir)
               if (index(qid, 'bedlevel') > 0 .and. (len_trim(md_inifieldfile) > 0 .or. len_trim(md_extfile_new) > 0)) then
                  ! Prefer new-format files over old *.ext when both are present.
                  preferred_bedlevel_file = trim(md_extfile_new)
                  if (len_trim(preferred_bedlevel_file) == 0) then
                     preferred_bedlevel_file = trim(md_inifieldfile)
                  end if
                  call mess(LEVEL_WARN, 'Bed level info should be defined in file '''//trim(preferred_bedlevel_file)//'''. Quantity '//trim(qid)//' ignored in external forcing file '''//trim(md_extfile)//'''.')
                  cycle
               end if
               success = .true.
               if (strcmpi(qid, 'bedlevel1D')) then
                  call mess(LEVEL_INFO, 'setbedlevelfromextfile: Setting 1D bedlevel from file '''//trim(filename)//'''.')
                  kc(1:mx) = kc1D
                  success = timespaceinitialfield_mpi(xk, yk, zk, numk, filename, filetype, method, operand, transformcoef, UNC_LOC_CN, kc)
               else if (strcmpi(qid, 'bedlevel', 8)) then
                  if (strcmpi(qid, 'bedlevel')) then
                     call mess(LEVEL_INFO, 'setbedlevelfromextfile: Setting both 1D and 2D bedlevel from file '''//trim(filename)//'''.')
                     kc(1:mx) = kcc
                  else if (strcmpi(qid, 'bedlevel2D')) then
                     call mess(LEVEL_INFO, 'setbedlevelfromextfile: Setting 2D bedlevel from file '''//trim(filename)//'''.')
                     kc(1:mx) = kc2D
                  end if

                  if (ibedlevtyp == 3) then
                     success = timespaceinitialfield_mpi(xk, yk, zk, numk, filename, filetype, method, operand, transformcoef, iprimpos, kc)
                  else if (ibedlevtyp == 2) then
                     success = timespaceinitialfield_mpi(xu, yu, blu, lnx, filename, filetype, method, operand, transformcoef, iprimpos, kc)
                  else if (ibedlevtyp == 1) then
                     success = timespaceinitialfield_mpi(xz, yz, bl, ndx, filename, filetype, method, operand, transformcoef, iprimpos, kc)
                  end if
               end if
               if (.not. success) then
                  call mess(LEVEL_FATAL, 'Error reading '//trim(qid)//' from '//trim(filename)//'.')
               end if
            end if

         end do ! ja==1 provider loop

         ! Clean up *.ext file
         if (mext /= 0) then
            rewind (mext)
         end if

         ! Interpreted values for debugging.
         if (md_exportnet_bedlevel == 1) then
!      save network
            select case (ibedlevtyp)
            case (3, 4, 5, 6) ! primitime position = netnode, cell corner
               call unc_write_net('DFM_interpreted_network_'//trim(md_ident)//'_net.nc')
            end select
         end if

         deallocate (kcc, kc1d, kc2d)

      end if

      if (ibedlevtyp == 1) then
         do k = 1, ndxi
            if (bl(k) == dmiss) then
               bl(k) = zkuni
               bl_set_from_zkuni = .true.
            end if
         end do
         if (bl_set_from_zkuni) then
            call mess(LEVEL_INFO, 'setbedlevelfromextfile: Unspecified bedlevels replaced using value from BedlevUni.')
         end if

         ! To improve: bed levels at boundary to be set from net file, instead of mirroring
         do L = Lnxi + 1, Lnx
            k1 = ln(1, L)
            k2 = ln(2, L)
            bl(k1) = bl(k2)
         end do
         call mess(LEVEL_INFO, 'setbedlevelfromextfile: Mirroring input bedlevels at open boundaries.')

      end if

      if (kc_size_store > 0) then
         call realloc(kc, kc_size_store, keepExisting=.false., fill=0)
      end if

   end subroutine setbedlevelfromextfile ! setbottomlevels

end module m_setbedlevelfromextfile
