module m_file_helpers
   use precision, only: dp
   implicit none

   integer, parameter :: strlen_netcdf = 256 ! string length for station names

contains
   !> Create a file with the specified name and content. If the file already exists it will be replaced.
   subroutine create_file(file_name, lines)
      implicit none
      character(len=*), intent(in) :: file_name
      character(len=*), intent(in) :: lines(:)
      integer :: error_code, i, file_lun
      open (newunit=file_lun, file=file_name, status='replace', action='write', &
            form='formatted', iostat=error_code)
      if (error_code /= 0) then
         error stop "Failed to open file "//trim(file_name)
      end if
      do i = 1, size(lines)
         write (file_lun, '(A)', iostat=error_code) trim(lines(i))
         if (error_code /= 0) then
            close (file_lun)
            error stop "Failed to write to file "//trim(file_name)
         end if
      end do
      close (file_lun)
   end subroutine create_file

   !> Initialize a NetCDF history file and write waterlevel data from an array
   !! 
   !! This subroutine creates a NetCDF file following the structure of DFlowFM history files
   !! and writes waterlevel (water surface elevation) data for observation stations.
   !!
   !! @param[in] file_name      Name of the NetCDF file to create
   !! @param[in] station_names  Array of station names
   !! @param[in] station_x      Array of station x-coordinates
   !! @param[in] station_y      Array of station y-coordinates
   !! @param[in] time_values    Array of time values (in seconds since reference)
   !! @param[in] waterlevel     2D array (ntimes, nstations) of waterlevel values
   !! @param[in] reference_time Reference time string (e.g., "seconds since 2001-01-01 00:00:00 +00:00")
   subroutine initialize_his_waterlevel(file_name, station_names, station_x, station_y, &
                                         time_values, waterlevel, reference_time)
      use netcdf
      use netcdf_utils, only: check_netcdf_error
      use unstruc_netcdf, only: unc_create, definencvar, unc_addcoordatts, unc_addcoordmapping
      use m_missing, only: dmiss
      
      implicit none
      
      ! Arguments
      character(len=*), intent(in) :: file_name
      character(len=*), intent(in) :: station_names(:)
      real(kind=dp), intent(in)    :: station_x(:)
      real(kind=dp), intent(in)    :: station_y(:)
      real(kind=dp), intent(in)    :: time_values(:)
      real(kind=dp), intent(in)    :: waterlevel(:,:) ! (ntimes, nstations)
      character(len=*), intent(in) :: reference_time

      ! Local variables
      integer :: ihisfile, ierr
      integer :: nstations, ntimes
      integer :: id_timedim, id_twodim, id_strlendim, id_statdim, id_statgeomnodesdim
      integer :: id_time, id_timebds, id_timestep
      integer :: id_crs, id_statname, id_statgeom, id_statnodecount
      integer :: id_statx, id_staty, id_statid
      integer :: id_stat_x_coord, id_stat_y_coord  ! station coordinate variables
      integer :: id_waterlevel
      integer :: i, j
      integer, allocatable :: node_count(:)
      
      nstations = size(station_names)
      ntimes = size(time_values)
      
      ! Validate inputs
      if (size(station_x) /= nstations .or. size(station_y) /= nstations) then
         error stop "Station coordinate arrays must match station_names size"
      end if
      if (size(waterlevel, 1) /= ntimes .or. size(waterlevel, 2) /= nstations) then
         error stop "Waterlevel array dimensions must be (ntimes, nstations)"
      end if
      
      ! Create NetCDF file
      ierr = unc_create(trim(file_name), 0, ihisfile)
      if (ierr /= nf90_noerr) then
         error stop "Failed to create NetCDF file: "//trim(file_name)
      end if
      
      ! Define dimensions
      call check_netcdf_error(nf90_def_dim(ihisfile, 'time', nf90_unlimited, id_timedim))
      call check_netcdf_error(nf90_def_dim(ihisfile, 'two', 2, id_twodim))
      call check_netcdf_error(nf90_def_dim(ihisfile, 'name_len', strlen_netcdf, id_strlendim))
      call check_netcdf_error(nf90_def_dim(ihisfile, 'station', nstations, id_statdim))
      call check_netcdf_error(nf90_def_dim(ihisfile, 'station_geom_nNodes', nstations, id_statgeomnodesdim))
      
      ! Define time variable
      call definencvar(ihisfile, id_time, nf90_double, [id_timedim], 'time', '', &
                      trim(reference_time), '', fillVal=dmiss)
      call check_netcdf_error(nf90_put_att(ihisfile, id_time, 'standard_name', 'time'))
      call check_netcdf_error(nf90_put_att(ihisfile, id_time, 'bounds', 'time_bds'))
      
      ! Define time_bds variable
      call definencvar(ihisfile, id_timebds, nf90_double, [id_twodim, id_timedim], 'time_bds', &
                      'Time interval for each point in time.', trim(reference_time), '', fillVal=dmiss)
      call check_netcdf_error(nf90_put_att(ihisfile, id_timebds, 'standard_name', 'time'))
      
      ! Define timestep variable
      call definencvar(ihisfile, id_timestep, nf90_double, [id_timedim], 'timestep', &
                      'latest computational timestep size in each output interval', 's')
      
      ! Define coordinate reference system (placeholder)
      ierr = unc_addcoordmapping(ihisfile, 0) ! jsferic=0 for Cartesian
      
      ! Define station_name variable
      call definencvar(ihisfile, id_statname, nf90_char, [id_strlendim, id_statdim], &
                      'station_name', 'name of observation station')
      call check_netcdf_error(nf90_put_att(ihisfile, id_statname, 'cf_role', 'timeseries_id'))
      
      ! Define station_geom (geometry container)
      call check_netcdf_error(nf90_def_var(ihisfile, 'station_geom', nf90_int, id_statgeom))
      call check_netcdf_error(nf90_put_att(ihisfile, id_statgeom, 'geometry_type', 'point'))
      call check_netcdf_error(nf90_put_att(ihisfile, id_statgeom, 'node_count', 'station_geom_node_count'))
      call check_netcdf_error(nf90_put_att(ihisfile, id_statgeom, 'node_coordinates', &
                             'station_geom_node_coordx station_geom_node_coordy'))
      
      ! Define station_geom_node_count
      call definencvar(ihisfile, id_statnodecount, nf90_int, [id_statdim], 'station_geom_node_count', &
                      'Count of nodes per observation station')
      
      ! Define station geometry coordinates
      call definencvar(ihisfile, id_statx, nf90_double, [id_statgeomnodesdim], &
                      'station_geom_node_coordx', 'x-coordinate of observation station', 'm')
      call check_netcdf_error(nf90_put_att(ihisfile, id_statx, 'standard_name', 'projection_x_coordinate'))
      call check_netcdf_error(nf90_put_att(ihisfile, id_statx, 'axis', 'X'))
      
      call definencvar(ihisfile, id_staty, nf90_double, [id_statgeomnodesdim], &
                      'station_geom_node_coordy', 'y-coordinate of observation station', 'm')
      call check_netcdf_error(nf90_put_att(ihisfile, id_staty, 'standard_name', 'projection_y_coordinate'))
      call check_netcdf_error(nf90_put_att(ihisfile, id_staty, 'axis', 'Y'))
      
      ! Define station_id variable
      call definencvar(ihisfile, id_statid, nf90_char, [id_strlendim, id_statdim], &
                      'station_id', 'id of station')
      call check_netcdf_error(nf90_put_att(ihisfile, id_statid, 'cf_role', 'timeseries_id'))
      
      ! Define station coordinate variables (non-snapped)
      call definencvar(ihisfile, id_stat_x_coord, nf90_double, [id_statdim], &
                      'station_x_coordinate', 'x-coordinate', 'm', '', fillVal=dmiss)
      call check_netcdf_error(nf90_put_att(ihisfile, id_stat_x_coord, 'standard_name', 'projection_x_coordinate'))
      
      call definencvar(ihisfile, id_stat_y_coord, nf90_double, [id_statdim], &
                      'station_y_coordinate', 'y-coordinate', 'm', '', fillVal=dmiss)
      call check_netcdf_error(nf90_put_att(ihisfile, id_stat_y_coord, 'standard_name', 'projection_y_coordinate'))
      
      ! Define waterlevel variable
      call definencvar(ihisfile, id_waterlevel, nf90_double, [id_timedim, id_statdim], &
                      'waterlevel', 'water level', 'm', 'station_x_coordinate station_y_coordinate station_name', &
                      fillVal=dmiss, add_gridmapping=.true.)
      call check_netcdf_error(nf90_put_att(ihisfile, id_waterlevel, 'standard_name', 'sea_surface_height'))
      call check_netcdf_error(nf90_put_att(ihisfile, id_waterlevel, 'cell_methods', 'time: point'))
      call check_netcdf_error(nf90_put_att(ihisfile, id_waterlevel, 'geometry', 'station_geom'))
      
      ! End definition mode
      call check_netcdf_error(nf90_enddef(ihisfile))
      
      ! Write time data
      call check_netcdf_error(nf90_put_var(ihisfile, id_time, time_values))
      
      ! Write station names
      do i = 1, nstations
         call check_netcdf_error(nf90_put_var(ihisfile, id_statname, trim(station_names(i)), &
                                start=[1, i], count=[strlen_netcdf, 1]))
         call check_netcdf_error(nf90_put_var(ihisfile, id_statid, trim(station_names(i)), &
                                start=[1, i], count=[strlen_netcdf, 1]))
      end do
      
      ! Write station coordinates
      call check_netcdf_error(nf90_put_var(ihisfile, id_statx, station_x))
      call check_netcdf_error(nf90_put_var(ihisfile, id_staty, station_y))
      
      ! Write station coordinate variables (non-snapped)
      call check_netcdf_error(nf90_put_var(ihisfile, id_stat_x_coord, station_x))
      call check_netcdf_error(nf90_put_var(ihisfile, id_stat_y_coord, station_y))
      
      ! Write node count (1 node per station for point geometry)
      allocate(node_count(nstations))
      node_count = 1
      call check_netcdf_error(nf90_put_var(ihisfile, id_statnodecount, node_count))
      deallocate(node_count)
      
      ! Write waterlevel data
      call check_netcdf_error(nf90_put_var(ihisfile, id_waterlevel, waterlevel, &
                             start=[1, 1], count=[ntimes, nstations]))
      
      ! Close file
      call check_netcdf_error(nf90_close(ihisfile))
      
   end subroutine initialize_his_waterlevel

end module