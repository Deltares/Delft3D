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

!> Module for writing storage table information in the DFM_VOLUME tool
module m_netcdf_output

   use netcdf
   use precision, only: DP
   implicit none(type, external)
   private

   public :: nc_create
   public :: nc_close
   public :: write_volume_surface_arrays
   public :: write_volume_table_gridpoint_data

contains

!> Create the Netcdf file with name NCFILE
!> and fill in the common fields.
   function nc_create(ncfile) result(ioutput)
      use dfm_volume_tool_version_module
      character(len=*), intent(in) :: ncfile !< name of the new netcdf-file to be created
      integer :: ioutput !< Netcdf ID given to this file

      character(len=8) :: cdate
      character(len=10) :: ctime
      character(len=5) :: czone
      integer :: ierr
      integer :: oldfillmode
      ierr = NF90_NOERR

      ierr = nf90_create(trim(ncfile), 0, ioutput)
      call nc_error(ierr, 'nc_create')
      ierr = nf90_set_fill(ioutput, NF90_NOFILL, oldfillmode)
      call nc_error(ierr, 'nc_create')
      ierr = nf90_put_att(ioutput, NF90_GLOBAL, 'institution', 'Deltares') !<-----RL666----------------- Aanpassen aan sobek
      call nc_error(ierr, 'nc_create')
      ierr = nf90_put_att(ioutput, NF90_GLOBAL, 'references', 'http://www.deltares.nl')
      call nc_error(ierr, 'nc_create')
      ierr = nf90_put_att(ioutput, NF90_GLOBAL, 'source', trim(DFM_VOLUME_TOOL_VERSION_FULL))
      call nc_error(ierr, 'nc_create')

      call date_and_time(cdate, ctime, czone)
      ierr = nf90_put_att(ioutput, NF90_GLOBAL, 'history', &
                          'Created on '//cdate(1:4)//'-'//cdate(5:6)//'-'//cdate(7:8)//'T'// &
                          ctime(1:2)//':'//ctime(3:4)//':'//ctime(5:6)//czone(1:5)// &
                          ',  DFM Volume Tool')
      call nc_error(ierr, 'nc_create')

!   ierr = nf90_put_att(ioutput, nf90_global,  'simulation_start_date', 'Startdate: '//RefDate(1:4)//'-'//RefDate(6:7)//'-'//RefDate(9:10))
!   call nc_error(ierr, 'nc_create')

      ierr = nf90_put_att(ioutput, NF90_GLOBAL, 'Conventions', 'CF-1.7')
      call nc_error(ierr, 'nc_create')
      ! ierr = nf90_put_att(ioutput, nf90_global,  'featureType', 'timeSeries')
      ! call nc_error(ierr, 'nc_create')

   end function nc_create

!> Write the volumes and surfaces per gridpoint to an open Netcdf file
   subroutine write_volume_table_gridpoint_data(ioutput, bedlevel, topheight, volume, surface, storage, &
                                                dead_storage, wl_dead_storage, table_count, increment)
      use messagehandling

      integer, intent(in) :: ioutput !< NetCDF dataset ID for the already opened output file.
      real(kind=DP), dimension(:), intent(in) :: bedlevel !< (num_nodes) Absolute bed levels for each gridpoint.
      real(kind=DP), dimension(:), intent(in) :: topheight !< (num_nodes) Highest tabulated water level relative to bed level.
      real(kind=DP), dimension(:, :), intent(in) :: volume !< (levels, num_nodes) Tabulated volumes.
      real(kind=DP), dimension(:, :), intent(in) :: surface !< (levels, num_nodes) Tabulated wet surface area.
      real(kind=DP), dimension(:, :), intent(in) :: storage !< (levels, num_nodes) Tabulated storage.
      real(kind=DP), dimension(:, :), intent(in) :: dead_storage !< (levels, num_nodes) Tabulated dead storage.
      real(kind=DP), dimension(:), intent(in) :: wl_dead_storage !< (num_nodes) dead storage water level on gridpoint
      integer, dimension(:), intent(in) :: table_count !< (num_nodes) Number of levels in the volume/surface table
      real(kind=DP), intent(in) :: increment !< increment between two volume table levels

      integer :: dimid_levels, dimid_nodes, dimid_increment
      integer :: varid_volume, varid_surface, varid_num_levels, varid_bedlevel, varid_topheight, varid_increment
      integer :: varid_storage, varid_dead_storage, varid_wl_dead_storage
      integer :: ierr
      integer :: num_levels, num_nodes, mesh1d_num_nodes
      character(len=NF90_MAX_NAME) :: mesh1d_num_nodes_name

      num_nodes = size(bedlevel)
      num_levels = maxval(table_count)

      ierr = nf90_redef(ioutput)
      if (ierr /= NF90_EINDEFINE) then
         call nc_error(ierr, 'nc_write_redef')
      end if
      ierr = nf90_def_dim(ioutput, 'levels', num_levels, dimid_levels) ! ... if not, make this an unlimited dimension
      call nc_error(ierr, 'nc_write_def_dim')
      ierr = nf90_inq_dimid(ioutput, 'mesh1d_nNodes', dimid_nodes)
      call nc_error(ierr, 'nc_write_inq_dimid')
      ierr = nf90_inquire_dimension(ioutput, dimid_nodes, mesh1d_num_nodes_name, mesh1d_num_nodes)
      call nc_error(ierr, 'nc_write_inq_dim')
      if (mesh1d_num_nodes /= num_nodes) then
         call nc_error(-57, 'nc_write')
      end if
      ierr = nf90_def_dim(ioutput, 'increment', 1, dimid_increment)
      call nc_error(ierr, 'nc_write')

      varid_volume = setup_variable(ioutput, 'volume', NF90_DOUBLE, (/dimid_levels, dimid_nodes/), '', 'Volume', 'm3')
      varid_surface = setup_variable(ioutput, 'surface', NF90_DOUBLE, (/dimid_levels, dimid_nodes/), '', 'Surface', 'm2')
      varid_num_levels = setup_variable(ioutput, 'numlevels', NF90_INT, (/dimid_nodes/), '', 'Number of levels', '')
      varid_bedlevel = setup_variable(ioutput, 'bedlevel', NF90_DOUBLE, (/dimid_nodes/), '', 'Bedlevel', 'm')
      varid_topheight = setup_variable(ioutput, 'topheight', NF90_DOUBLE, (/dimid_nodes/), '', 'TopHeight', 'm')
      varid_increment = setup_variable(ioutput, 'increment', NF90_DOUBLE, (/dimid_increment/), '', 'Increment', 'm')
      varid_storage = setup_variable(ioutput, 'storage', NF90_DOUBLE, (/dimid_levels, dimid_nodes/), '', 'Storage', 'm3')
      varid_dead_storage = setup_variable(ioutput, 'deadstorage', NF90_DOUBLE, &
                                          (/dimid_levels, dimid_nodes/), '', 'Dead Storage', 'm3')
      varid_wl_dead_storage = setup_variable(ioutput, 'wl_deadstorage', NF90_DOUBLE, &
                                             (/dimid_nodes/), '', 'Dead Storage Water Level', 'm')

      ierr = nf90_enddef(ioutput)
      call nc_error(ierr, 'nc_write_enddef')

      ierr = nf90_put_var(ioutput, varid_volume, volume, start=(/1, 1/), count=(/num_levels, num_nodes/))
      call nc_error(ierr, 'nc_write_put_vol')
      ierr = nf90_put_var(ioutput, varid_surface, surface, start=(/1, 1/), count=(/num_levels, num_nodes/))
      call nc_error(ierr, 'nc_write_put_surf')
      ierr = nf90_put_var(ioutput, varid_num_levels, table_count)
      call nc_error(ierr, 'nc_write_put_numl')
      ierr = nf90_put_var(ioutput, varid_bedlevel, bedlevel, start=(/1/), count=(/num_nodes/))
      call nc_error(ierr, 'nc_write_put_bedl')
      ierr = nf90_put_var(ioutput, varid_topheight, topheight, start=(/1/), count=(/num_nodes/))
      call nc_error(ierr, 'nc_write_put_toph')
      ierr = nf90_put_var(ioutput, varid_increment, increment)
      call nc_error(ierr, 'nc_write_put_incr')
      ierr = nf90_put_var(ioutput, varid_storage, storage, start=(/1, 1/), count=(/num_levels, num_nodes/))
      call nc_error(ierr, 'nc_write_put_stor')
      ierr = nf90_put_var(ioutput, varid_dead_storage, dead_storage, start=(/1, 1/), count=(/num_levels, num_nodes/))
      call nc_error(ierr, 'nc_write_put_deadstor')
      ierr = nf90_put_var(ioutput, varid_wl_dead_storage, wl_dead_storage, start=(/1/), count=(/num_nodes/))
      call nc_error(ierr, 'nc_write_put_wl_deadstor')

   end subroutine write_volume_table_gridpoint_data

!> Close the Netcdf file
   subroutine nc_close(ioutput)
      integer, intent(in) :: ioutput !< Handle to the output file.

      integer :: ierr

      ierr = nf90_close(ioutput)
      call nc_error(ierr, 'nc_close')
   end subroutine nc_close

!> Write the volumes and surfaces to the Netcdf file
   subroutine write_volume_surface_arrays(ioutput, ids, levels, volume, surface, storage, dead_storage, num_levels, num_ids)

      use messagehandling

      integer, intent(in) :: ioutput !< Handle to the output file.
      character(len=IDLEN), dimension(:), intent(in) :: ids !< Ids of the entries in the volume table.
      real(kind=DP), dimension(:), intent(in) :: levels !< Levels w.r.t. the bedlevel.
      real(kind=DP), dimension(:, :), intent(in) :: volume !< Volumes.
      real(kind=DP), dimension(:, :), intent(in) :: surface !< Wet surface area.
      real(kind=DP), dimension(:, :), intent(in) :: dead_storage !< Dead storage
      real(kind=DP), dimension(:, :), intent(in) :: storage !< Storage (=volume - dead_storage)
      integer, intent(in) :: num_levels !< Number of levels in the volume table
      integer, intent(in) :: num_ids !< Number of volume tables (per Id)

      integer :: dimid_levels, dimid_ids, dimid_chars
      integer :: varid_id, varid_levels, varid_volume, varid_surface, varid_storage, varid_dead_storage
      integer :: ierr

      ierr = nf90_def_dim(ioutput, 'levels', num_levels, dimid_levels) ! ... if not, make this an unlimited dimension
      call nc_error(ierr, 'nc_write')

      ierr = nf90_def_dim(ioutput, 'id', num_ids, dimid_ids)
      call nc_error(ierr, 'nc_write')
      ierr = nf90_def_dim(ioutput, 'id_charlen', IDLEN, dimid_chars)
      call nc_error(ierr, 'nc_write')

      ! Create 1D-array of location ID's
      varid_id = setup_variable(ioutput, 'id', NF90_CHAR, (/dimid_chars, dimid_ids/), '', 'Id', '-')
      ierr = nf90_put_att(ioutput, varid_id, 'cf_role', 'timeseries_id')
      call nc_error(ierr, 'nc_write_cf_role')
      varid_levels = setup_variable(ioutput, 'levels', NF90_DOUBLE, (/dimid_levels/), '', 'Levels', 'm')
      varid_volume = setup_variable(ioutput, 'volume', NF90_DOUBLE, (/dimid_levels, dimid_ids/), '', 'Volume', 'm3')
      varid_surface = setup_variable(ioutput, 'surface', NF90_DOUBLE, (/dimid_levels, dimid_ids/), '', 'Surface', 'm2')
      varid_dead_storage = setup_variable(ioutput, 'dead_storage', NF90_DOUBLE, (/dimid_levels, dimid_ids/), '', 'Dead storage', 'm3')
      varid_storage = setup_variable(ioutput, 'storage', NF90_DOUBLE, (/dimid_levels, dimid_ids/), '', 'Storage', 'm3')
      ierr = nf90_put_att(ioutput, varid_volume, 'coordinates', 'id levels')
      call nc_error(ierr, 'nc_write')
      ierr = nf90_put_att(ioutput, varid_surface, 'coordinates', 'id levels') ! if present, standard_name should obey CF-convention
      call nc_error(ierr, 'nc_write')
      ierr = nf90_put_att(ioutput, varid_levels, 'positive', 'up')
      call nc_error(ierr, 'nc_write')

      ierr = nf90_enddef(ioutput)

      call nc_error(ierr, 'nc_write')

      ierr = nf90_put_var(ioutput, varid_id, ids(1:num_ids), start=(/1, 1/), count=(/IDLEN, num_ids/))
      call nc_error(ierr, 'nc_write')
      ierr = nf90_put_var(ioutput, varid_levels, levels, start=(/1/), count=(/num_levels/))
      call nc_error(ierr, 'nc_write')
      ierr = nf90_put_var(ioutput, varid_volume, volume, start=(/1, 1/), count=(/num_levels, num_ids/))
      call nc_error(ierr, 'nc_write')
      ierr = nf90_put_var(ioutput, varid_surface, surface, start=(/1, 1/), count=(/num_levels, num_ids/))
      call nc_error(ierr, 'nc_write')
      ierr = nf90_put_var(ioutput, varid_dead_storage, dead_storage, start=(/1, 1/), count=(/num_levels, num_ids/))
      call nc_error(ierr, 'nc_write')
      ierr = nf90_put_var(ioutput, varid_storage, storage, start=(/1, 1/), count=(/num_levels, num_ids/))
      call nc_error(ierr, 'nc_write')

   end subroutine write_volume_surface_arrays

!> Define the variable with VAR_NAME, including the attributes.
   function setup_variable(ioutput, var_name, var_type, dimensions, std_name, lng_name, units, &
                           fill_double, fill_int) result(varid)
      integer, intent(in) :: ioutput !< ID of the netcdf file, already opened for writing
      character(len=*), intent(in) :: var_name !< Variable name in the file
      integer, intent(in) :: var_type !< NetCDF variable type (as defined in the netCDF API)
      character(len=*), intent(in) :: std_name !< Standard name attribute
      character(len=*), intent(in) :: lng_name !< Long name attribute
      character(len=*), intent(in) :: units !< Units attribute
      real(kind=DP), intent(in), optional :: fill_double !< Default value representing missing: Double
      integer, intent(in), optional :: fill_int !< Default value representing missing: Int
      integer, dimension(:), intent(in) :: dimensions !< Vector with Netcdf dimension Ids

      integer :: ierr
      integer :: varid

      ierr = nf90_def_var(ioutput, trim(var_name), var_type, dimensions, varid)
      call nc_error(ierr, 'SetupVariable')

      if (len_trim(std_name) > 0) then
         ierr = nf90_put_att(ioutput, varid, 'standard_name', trim(std_name)) ! if present, standard_name should obey CF-convention
         call nc_error(ierr, 'SetupVariable')
      end if

      ierr = nf90_put_att(ioutput, varid, 'long_name', trim(lng_name))
      call nc_error(ierr, 'SetupVariable')
      ierr = nf90_put_att(ioutput, varid, 'units', trim(units))
      call nc_error(ierr, 'SetupVariable')

      select case (var_type)
      case (NF90_DOUBLE)
         if (present(fill_double)) then
            ierr = nf90_put_att(ioutput, varid, '_FillValue', fill_double)
         end if
      case (NF90_INT)
         if (present(fill_int)) then
            ierr = nf90_put_att(ioutput, varid, '_FillValue', fill_int)
         end if
      end select
      call nc_error(ierr, 'HisSetupVariable')
   end function setup_variable

!> In case of an error generate an error message
   subroutine nc_error(ierr, proc_name)

      use messagehandling

      integer, intent(in) :: ierr
      character(len=*), intent(in) :: proc_name

      character(len=5) :: netcdf_error_code = '(   )'

      if (ierr == 0) then
         return
      else
         write (netcdf_error_code(2:4), '(i3)') ierr
         msgbuf = nf90_strerror(ierr)
         msgbuf = 'NETCDF-ERROR in '//proc_name//', Error Code: '//netcdf_error_code//', '//trim(msgbuf)
         call fatal_flush()
      end if

   end subroutine nc_error

end module m_netcdf_output
