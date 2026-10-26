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

!> Generate total, per-branch, and per-gridpoint volume tables for a D-Flow FM model.
program dfm_volume_tool
!-----------------------------------------------------------------------------------------------------------------------------------

   use, intrinsic :: ieee_arithmetic, only: ieee_is_finite
   use ir_precision ! Integers and reals precision definition.
   use precision
   use iso_c_utils
   use string_module
   use messagehandling
   use dfm_volume_tool_version_module
   use dfm_volume_tool_static_version_info, only: TOOL_BASE_NAME, TOOL_VERSION
   use data_type_command_line_interface, only: type_command_line_interface ! Definition of Type_Command_Line_Interface.
   use m_netcdf_output
   use m_volumetables
   use m_network
   use m_storage_table
   use unstruc_channel_flow, only: tableincrement
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
   implicit none(type, external)

   character(len=IDLEN) :: dll_name
   character(len=IDLEN) :: mdufile
   integer(PNTRSIZE) :: dfm
   integer :: dia
   integer :: i, j, l
   integer :: id_index
   integer :: num_ids
   integer :: num_branches
   integer :: ioutput, ncid
   real(kind=DP) :: increment
   real(kind=DP) :: bedlevel, toplevel
   character(len=IDLEN) :: output_type
   character(len=IDLEN) :: output_file, grid_output_file
   type(c_ptr) :: xptr
   integer :: num_nodes_1d, num_links, lnx, num_boundaries
   integer, pointer :: integer_pointer
   logical :: compute_total
   logical :: compute_on_branches
   logical :: compute_on_gridpoints

   real(kind=DP), dimension(:, :), pointer :: volume
   real(kind=DP), dimension(:, :), pointer :: surface
   real(kind=DP), dimension(:, :), pointer :: storage
   real(kind=DP), dimension(:, :), pointer :: dead_storage
   real(kind=DP), dimension(:), pointer :: levels

   character(len=IDLEN), dimension(:), allocatable :: ids
   integer :: num_levels
   type(t_voltable), dimension(:), pointer :: volume_table
   type(t_voltable), dimension(:, :), pointer :: volume_table_on_links
   type(t_network), pointer :: network
   type(t_branch), dimension(:), pointer :: branches
   integer, dimension(:), allocatable :: mask, table_count
   integer, dimension(:, :), pointer :: bnd_index
   integer, dimension(:, :), allocatable :: ln2nd
   integer, dimension(:, :), pointer :: lnog !Don't modify this!!!
   integer, dimension(:), pointer :: kcu2
   integer, dimension(:), allocatable :: shape_array
   integer, pointer :: ndx2d
   real(kind=DP), dimension(:), pointer :: bnd_values
   real(kind=DP), dimension(:, :), pointer :: inslevtube
   real(kind=DP), dimension(:, :), allocatable :: grid_volume, grid_surface, grid_dead_storage, grid_storage
   real(kind=DP), dimension(:), allocatable :: wl_dead_storage, bedlevels, topheights
   integer, parameter :: MAXDIMS = 6
! externals

   integer, external :: open_shared_library, bmi_initialize
   external :: dfm_generate_volume_tables, bmi_get_var_pointer, bmi_get_var_shape, bmi_get_var, write_1d_flowgeom_ugrid

   type(type_command_line_interface) :: cli !< Command Line Interface (CLI).
   integer(I4P) :: ierr !< Error trapping flag.

!-----------------------------------------------------------------------------------------------------------------------------------
   open (newunit=dia, file='dfm_volume_tool.dia')
   call setmessagehandling(write2screen=.true., lunmessages=dia)

   call getfullversionstring_dfm_volume_tool(msgbuf)
   call msg_flush()
   call getbranch_dfm_volume_tool(msgbuf)
   call msg_flush()

   num_ids = 0
!-----------------------------------------------------------------------------------------------------------------------------------
!! initializing Command Line Interface
   call cli%init(progname=TOOL_BASE_NAME, &
                 version=TOOL_VERSION, &
                 description='Tool for generating volume output for D-Flow FM')
!
!! setting Command Line Arguments
   call cli%add(switch='--mdufile', switch_ab='-f', help='Name of the input mdu file.', &
                required=.true., act='store', def=char(0), valname='FILE')
   call cli%add(switch='--increment', switch_ab='-i', help='finite positive interval for the volume tables', &
                required=.true., act='store', def='', valname='INCREMENT')
   call cli%add(switch='--output', switch_ab='-o', &
                help='output type: "Total" (default), "Branches", "Gridpoints" (separate file only), "All"', &
                required=.false., act='store', def='Total', valname='OUTPUT_TYPE')
   call cli%add(switch='--outputfile', switch_ab='-p', help='name of the output file', &
                required=.false., act='store', def='', valname='OUTPUT_FILE')
   call cli%add(switch='--gridoutputfile', switch_ab='-g', help='name of the per-gridpoint output file', &
                required=.false., act='store', def='', valname='GRID_OUTPUT_FILE')

! parsing Command Line Interface
   call cli%parse(error=ierr)
   call cli%errored(error=ierr)

   if (ierr /= 0) then
      call setmessage(LEVEL_ERROR, 'Error reading input parameters')
      error stop 1
   end if

! Check options
   output_type = 'Total'
   output_file = ''
   if (ierr == 0) then
      call cli%get(switch='--mdufile', val=mdufile, error=ierr)
   end if
   if (ierr == 0) then
      call cli%get(switch='--increment', val=increment, error=ierr)
   end if
   if (ierr == 0) then
      if (.not. ieee_is_finite(increment) .or. increment <= tiny(1.0_dp)) then
         call setmessage(LEVEL_ERROR, 'Volume-table increment must be finite and greater than tiny(1.0_DP).')
         error stop 1
      end if
   end if
   if (ierr == 0) then
      call cli%get(switch='--output', val=output_type, error=ierr)
   end if
   if (ierr == 0) then
      if (.not. (strcmpi(output_type, 'Total') .or. strcmpi(output_type, 'Branches') .or. &
                 strcmpi(output_type, 'Gridpoints') .or. strcmpi(output_type, 'All'))) then
         call setmessage(LEVEL_ERROR, 'Invalid --output value: "'//trim(output_type)// &
                         '". Expected Total, Branches, Gridpoints, or All.')
         stop 1
      end if
   end if
   if (ierr == 0) then
      output_file = ''
      call cli%get(switch='--outputfile', val=output_file, error=ierr)
      if (len_trim(output_file) == 0) then
         i = index(mdufile, '.', .true.)
         output_file = mdufile(1:i - 1)//'.nc'
      end if
   end if

   if (ierr == 0) then
      grid_output_file = ''
      call cli%get(switch='--gridoutputfile', val=grid_output_file, error=ierr)
      if (len_trim(grid_output_file) == 0) then
         grid_output_file = "PerGridpoint_"//output_file
      end if
   end if

   if (ierr /= 0) then
      call setmessage(LEVEL_ERROR, 'Error retrieving input parameters')
      error stop 1
   end if

#ifdef WIN32
   dll_name = 'dflowfm'
#else
   dll_name = 'libdflowfm.so'
#endif
   if (ierr == 0) then
      ierr = open_shared_library(dfm, dll_name)
      if (ierr /= 0) then
         msgbuf = 'Cannot open'//trim(dll_name)//'.'
         call msg_flush()
         error stop 1
      end if

   end if

   if (ierr == 0) then
      ! Initialise the model
      ierr = bmi_initialize(dfm, mdufile)
      if (ierr /= 0) then
         msgbuf = 'Cannot initialize '''//trim(mdufile)//'''.'
         call msg_flush()
         error stop 1
      end if

      ! Generate the volume tables
      call dfm_generate_volume_tables(dfm, increment)

      ! Retrieve data from the D-FLOW FM dll
      call bmi_get_var_pointer(dfm, string_to_char_array('lnx1D'), xptr)
      call c_f_pointer(xptr, integer_pointer)
      num_links = integer_pointer

      call bmi_get_var_pointer(dfm, string_to_char_array('lnx'), xptr)
      call c_f_pointer(xptr, integer_pointer)
      lnx = integer_pointer

      allocate (shape_array(MAXDIMS))
      call bmi_get_var_shape(dfm, string_to_char_array('zbndz'), shape_array)
      num_boundaries = shape_array(1)

      call bmi_get_var_pointer(dfm, string_to_char_array('ndx2d'), xptr)
      call c_f_pointer(xptr, ndx2d)

      call bmi_get_var_pointer(dfm, string_to_char_array('ndxi'), xptr)
      call c_f_pointer(xptr, integer_pointer)
      num_nodes_1d = integer_pointer - ndx2d

      allocate (inslevtube(2, lnx), ln2nd(2, lnx), bnd_values(num_boundaries))
      call bmi_get_var(dfm, 'bob', inslevtube, 2 * lnx)
      if (num_boundaries > 0) then
         call bmi_get_var(dfm, 'zbndz', bnd_values, num_boundaries)
         !call get_variable_pointer(dfm, string_to_char_array('kbndz'), xptr)
      end if
      call bmi_get_var_pointer(dfm, string_to_char_array('kcu'), xptr)
      call c_f_pointer(xptr, kcu2, (/lnx/))

      call bmi_get_var_pointer(dfm, string_to_char_array('ln'), xptr)
      call c_f_pointer(xptr, lnog, (/2, lnx/))

      call bmi_get_var_pointer(dfm, string_to_char_array('kbndz'), xptr)
      call c_f_pointer(xptr, bnd_index, (/MAXDIMS, num_boundaries/))

      call bmi_get_var_pointer(dfm, string_to_char_array('vltb'), xptr)
      call c_f_pointer(xptr, volume_table, (/num_nodes_1d/))
      call bmi_get_var_pointer(dfm, string_to_char_array('vltbOnLinks'), xptr)
      call c_f_pointer(xptr, volume_table_on_links, (/2, num_links/))
      call bmi_get_var_pointer(dfm, string_to_char_array('network'), xptr)
      call c_f_pointer(xptr, network)

      ! Determine the number of levels for the aggregated volume tables
      call get_bed_top_level(volume_table, num_nodes_1d, toplevel, bedlevel)

      ln2nd = lnog

      do l = 1, lnx
         if (abs(kcu2(l)) == 1) then
            ln2nd(:, l) = lnog(:, l) - ndx2d
         end if
      end do

      num_levels = 0
      do i = 1, num_nodes_1d
         num_levels = max(num_levels, volume_table(i)%count)
      end do

      compute_total = .false.
      compute_on_branches = .false.
      compute_on_gridpoints = .false.
      num_branches = network%brs%count
      if (strcmpi(output_type, 'Total')) then
         compute_total = .true.
         num_ids = 1
      else if (strcmpi(output_type, 'Branches')) then
         compute_on_branches = .true.
         num_ids = num_branches
      else if (strcmpi(output_type, 'Gridpoints')) then
         compute_on_gridpoints = .true.
      else if (strcmpi(output_type, 'All')) then
         compute_total = .true.
         compute_on_branches = .true.
         compute_on_gridpoints = .true.
         num_ids = num_branches + 1
      end if

      allocate (surface(num_levels, num_ids), volume(num_levels, num_ids), storage(num_levels, num_ids), &
                dead_storage(num_levels, num_ids))
      allocate (levels(num_levels), ids(num_ids), mask(num_nodes_1d), wl_dead_storage(num_nodes_1d))

      surface = 0.0_dp
      volume = 0.0_dp
      storage = 0.0_dp
      dead_storage = 0.0_dp

      do i = 1, num_levels
         levels(i) = real(i - 1, kind=DP) * increment
      end do
      tableincrement = increment

      call calculate_dead_storage(wl_dead_storage, network, bnd_values, inslevtube, bnd_index, &
                                  ln2nd, kcu2, num_links, num_nodes_1d, num_boundaries)

      id_index = 0
      if (compute_total) then
         id_index = 1
         !> setup the mask array for the complete model
         do i = 1, num_nodes_1d
            mask(i) = i
         end do

         !> Calculate the total volumes and surfaces for this model
         ids(1) = 'Total'
         call generate_total_volume_table(volume(:, 1), surface(:, 1), storage(:, 1), dead_storage(:, 1), &
                                          wl_dead_storage, volume_table, bedlevel, increment, num_nodes_1d, num_levels, mask)

      end if

      if (compute_on_branches) then
         branches => network%brs%branch
         do i = 1, num_branches
            id_index = id_index + 1
            ids(id_index) = branches(i)%id
            call generate_volume_table_on_branches(volume(:, id_index), surface(:, id_index), &
                                                   storage(:, id_index), dead_storage(:, id_index), &
                                                   wl_dead_storage, volume_table_on_links, bedlevel, &
                                                   increment, branches(i)%upointscount, num_levels, num_nodes_1d, branches(i)%lin, ln2nd)
         end do
      end if

      if ((compute_total .or. compute_on_branches) .and. num_ids > 0) then
         ioutput = nc_create(output_file)
         call write_volume_surface_arrays(ioutput, ids, levels, volume, surface, storage, dead_storage, num_levels, num_ids)
         call nc_close(ioutput)
      end if

      if (compute_on_gridpoints) then !Gridpoints are written separately
         !compute maximum volume table length

         allocate (grid_surface(num_levels, num_nodes_1d), grid_dead_storage(num_levels, num_nodes_1d), &
                   grid_volume(num_levels, num_nodes_1d), grid_storage(num_levels, num_nodes_1d), &
                   bedlevels(num_nodes_1d), topheights(num_nodes_1d), table_count(num_nodes_1d))

         grid_surface = 0.0_dp
         grid_volume = 0.0_dp
         grid_storage = 0.0_dp
         grid_dead_storage = 0.0_dp

         do i = 1, num_nodes_1d
            bedlevels(i) = volume_table(i)%bedlevel
            topheights(i) = volume_table(i)%topheight
            table_count(i) = volume_table(i)%count
            call add_volume_and_surface(grid_volume(:, i), grid_surface(:, i), grid_dead_storage(:, i), &
                                        wl_dead_storage(i), volume_table(i), volume_table(i)%bedlevel, increment, num_levels)
            do j = 1, volume_table(i)%count
               grid_surface(j, i) = volume_table(i)%sur(j)
               grid_volume(j, i) = volume_table(i)%vol(j)
               grid_storage(j, i) = grid_volume(j, i) - grid_dead_storage(j, i)
            end do
         end do

         ncid = nc_create(grid_output_file)
         ! Write the output to the netcdf file
         call write_1d_flowgeom_ugrid(dfm, ncid)

         call write_volume_table_gridpoint_data(ncid, bedlevels, topheights, &
                                                grid_volume, grid_surface, grid_storage, grid_dead_storage, &
                                                wl_dead_storage, table_count, increment)
         call nc_close(ncid)

         deallocate (grid_surface, grid_dead_storage, grid_volume, grid_storage, bedlevels, topheights, table_count)
      end if

      deallocate (surface, volume, storage, dead_storage, levels, ids, mask, wl_dead_storage)
   end if

end program dfm_volume_tool
