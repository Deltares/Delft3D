subroutine write_wave_map_netcdf(sg, sof, sif, n_swan_grids, wavedata, casl, &
                                 prevtime, singleprecision, sif_mmax, sif_nmax, &
                                 sif_veg, output_ice, output_veg, &
                                 nautical_convention, north_direction)
!----- GPL ---------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2011-2026.
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
!!--description-----------------------------------------------------------------
! NONE
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
   use wave_data
   use swan_flow_grid_maps
   use netcdf
   use nc_check, only : nc_check_err
   use precision_basics
   use dwaves_version_module
   use angle_convention, only : reflect_between_nautical_and_cartesian
   !
   implicit none
!
! Global variables
!
   integer, intent(in) :: n_swan_grids ! number of swan grids
   character(*), intent(in) :: casl ! runid
   type(grid), intent(in) :: sg ! swan grid
   type(output_fields), intent(in) :: sof ! output fields defined on swan grid
   type(input_fields), intent(in) :: sif ! input fields defined on swan grid
   type(wave_data_type), intent(in) :: wavedata
   logical, intent(in) :: prevtime ! true: the time to be written is the "previous time"
   logical, intent(in) :: singleprecision
   logical, intent(in) :: nautical_convention ! true: angles are according to the nautical convention
   integer, intent(in) :: output_ice ! switch for writing ice quantities
   integer, intent(in) :: output_veg ! switch for writing vegetation quantities
   integer, intent(in) :: sif_mmax
   integer, intent(in) :: sif_nmax
   real   , intent(in) :: north_direction ! direction of north in degrees, used to convert to nautical convention if nautical_convention is true
   real, dimension(sif_mmax, sif_nmax) :: sif_veg
!
! Local variables
!
   integer :: m, n
   integer :: epsg
   integer :: i
   integer :: idfile
   integer :: iddim_mmax
   integer :: iddim_nmax
   integer :: iddim_time
   integer :: idvar_coordmap
   integer :: idvar_x
   integer :: idvar_y
   integer :: idvar_time
   integer :: idvar_kcs
   integer :: idvar_hsign
   integer :: idvar_dir
   integer :: idvar_pdir
   integer :: idvar_period
   integer :: idvar_rtp
   integer :: idvar_depth
   integer :: idvar_velx
   integer :: idvar_vely
   integer :: idvar_transpx
   integer :: idvar_transpy
   integer :: idvar_dspr
   integer :: idvar_dissip
   integer :: idvar_leak
   integer :: idvar_qb
   integer :: idvar_ubot
   integer :: idvar_steepw
   integer :: idvar_wlength
   integer :: idvar_tps
   integer :: idvar_tm02
   integer :: idvar_tmm10
   integer :: idvar_dhsign
   integer :: idvar_drtm01
   integer :: idvar_setup
   integer :: idvar_fx
   integer :: idvar_fy
   integer :: idvar_windu
   integer :: idvar_windv
   integer :: idvar_icefrac
   integer :: idvar_floedia
   integer, dimension(:), allocatable :: idvar_outpars
   integer :: idvar_nstems
   integer :: ierror
   integer :: ind
   integer :: precision
   integer :: year
   integer :: month
   integer :: day
   real(hp), dimension(1) :: idummy ! Help array to read/write Nefis files
   integer, external :: nc_def_var
   integer :: count_xymiss
   real(hp) :: dearthrad
   character(100) :: string
   character(256) :: filename
   character(256) :: gridnam
   character(256) :: full_version
   character(8) :: cdate
   character(10) :: ctime
   character(5) :: czone
   character(11) :: epsgstring
   real(kind=hp), dimension(:, :), allocatable :: tmp_x ! dummy x-coordinates cell center to write to netCDF
   real(kind=hp), dimension(:, :), allocatable :: tmp_y ! dummy y-coordinates cell center to write to netCDF
   real, dimension(:, :), allocatable :: tmp_dir ! dummy directions to write to netCDF, converted to nautical convention if needed
!
!! executable statements -------------------------------------------------------
!
   if (sif_mmax /= sof%mmax) then
      write (*, '(a,i0,a,i0,a)') "ERROR: sif_mmax(", sif_mmax, ") is assumed to be identical to sof%mmax(", sof%mmax, ") but isn't. Vegetation arrays may contain wrong information."
   end if
   if (sif_nmax /= sof%nmax) then
      write (*, '(a,i0,a,i0,a)') "ERROR: sif_nmax(", sif_nmax, ") is assumed to be identical to sof%nmax(", sof%nmax, ") but isn't. Vegetation arrays may contain wrong information."
   end if

   dearthrad = 6378137.0_hp
   call getfullversionstring_dwaves(full_version)
   call date_and_time(cdate, ctime, czone)
   year = wavedata%time%refdate / 10000
   month = (wavedata%time%refdate - year * 10000) / 100
   day = wavedata%time%refdate - year * 10000 - month * 100
   if (singleprecision) then
      precision = nf90_float
      write (*, *) "Writing data to netcdf file in single precision (except the grid)"
   else
      ! default
      precision = nf90_double
   end if
   allocate (idvar_outpars(sof%n_outpars), stat=ierror)
   if (ierror /= 0) write (*, *) "ERROR allocating idvar_outpars in write_wave_map_netcdf"
   !
   ! define name of output file
   !
   if (n_swan_grids == 1) then
      write (filename, '(3a)') 'wavm-', trim(casl), '.nc'
   else
      gridnam = sg%grid_name
      ind = index(gridnam, '/', back=.true.)
      if (ind > 0) gridnam = gridnam(ind + 1:)
      ind = index(gridnam, '\', back=.true.)
      if (ind > 0) gridnam = gridnam(ind + 1:)
      ind = index(gridnam, '.', back=.true.)
      if (ind > 0) gridnam = gridnam(:ind - 1)
      write (filename, '(5a)') 'wavm-', trim(casl), '-', trim(gridnam), '.nc'
   end if
   if (sg%unstructured) then
      call write_wave_map_netcdf_unstructured(sg, sof, sif, wavedata, filename, prevtime, precision, &
                                              sif_mmax, sif_nmax, sif_veg, output_ice, output_veg, &
                                              nautical_convention, north_direction)
      deallocate (idvar_outpars, stat=ierror)
      return
   end if
   !
   ! replace the grid _FillValue with NF90_FILL_FLOAT in the x,y coordinates
   allocate (tmp_x(sg%mmax, sg%nmax), stat=ierror)
   allocate (tmp_y(sg%mmax, sg%nmax), stat=ierror)
   tmp_x = sg%x
   tmp_y = sg%y
   count_xymiss = 0
   do m = 1, sg%mmax
      do n = 1, sg%nmax
         if (.not. abs(comparereal(sg%x(m, n), sg%xymiss)) .and. .not. abs(comparereal(sg%y(m, n), sg%xymiss))) then
            tmp_x(m, n) = NF90_FILL_FLOAT
            tmp_y(m, n) = NF90_FILL_FLOAT
            count_xymiss = count_xymiss + 1
         end if
      end do
   end do
   write (*, *) 'Number of non-existing grid points detected: ', count_xymiss
   !
   if (wavedata%output%count == 1) then
      !
      ! create file
      !
      ierror = nf90_create(filename, wavedata%output%ncmode, idfile); call nc_check_err(ierror, "creating file", filename)
      !
      ! global attributes
      !
      ierror = nf90_put_att(idfile, nf90_global, 'institution', trim(company)); call nc_check_err(ierror, "put_att global institution", filename)
      ierror = nf90_put_att(idfile, nf90_global, 'references', trim(company_url)); call nc_check_err(ierror, "put_att global references", filename)
      ierror = nf90_put_att(idfile, nf90_global, 'source', trim(full_version)); call nc_check_err(ierror, "put_att global source", filename)
      ierror = nf90_put_att(idfile, nf90_global, 'history', &
                            'Created on '//cdate(1:4)//'-'//cdate(5:6)//'-'//cdate(7:8)//'T'//ctime(1:2)//':'//ctime(3:4)//':'//ctime(5:6)//czone(1:5)// &
                            ', '//trim(product_name)); call nc_check_err(ierror, "put_att global history", filename)
      if (nautical_convention) then
         ierror = nf90_put_att(idfile, nf90_global, 'Directional_convention', 'nautical'); call nc_check_err(ierror, "put_att global institution", filename)
      else
         ierror = nf90_put_att(idfile, nf90_global, 'Directional_convention', 'cartesian'); call nc_check_err(ierror, "put_att global institution", filename)
      end if      
      !
      ! dimensions
      !
      ierror = nf90_def_dim(idfile, 'mmax', sof%mmax, iddim_mmax); call nc_check_err(ierror, "def_dim mmax", filename)
      ierror = nf90_def_dim(idfile, 'nmax', sof%nmax, iddim_nmax); call nc_check_err(ierror, "def_dim nmax", filename)
      ierror = nf90_def_dim(idfile, 'time', nf90_unlimited, iddim_time); call nc_check_err(ierror, "def_dim time", filename)
      !
      ! define vars
      !
      !
      ! coordinate mapping
      !
      ierror = nf90_def_var(idfile, 'projected_coordinate_system', nf90_int, idvar_coordmap); call nc_check_err(ierror, "def_var coordinate mapping", filename)
      if (sg%sferic) then
         epsg = 4326
         epsgstring = 'EPSG:4326'
         ierror = nf90_put_att(idfile, idvar_coordmap, 'name', 'WGS84'); call nc_check_err(ierror, "coordinate mapping put_att", filename)
         ierror = nf90_put_att(idfile, idvar_coordmap, 'grid_mapping_name', 'latitude_longitude'); call nc_check_err(ierror, "coordinate mapping put_att", filename)
         string = 'deg'
      else
         epsg = 28992
         epsgstring = 'EPSG:28992'
         ierror = nf90_put_att(idfile, idvar_coordmap, 'name', 'Unknown projected'); call nc_check_err(ierror, "coordinate mapping put_att", filename)
         ierror = nf90_put_att(idfile, idvar_coordmap, 'grid_mapping_name', 'Unknown projected'); call nc_check_err(ierror, "coordinate mapping put_att", filename)
         string = 'm'
      end if
      ierror = nf90_put_att(idfile, idvar_coordmap, 'epsg', epsg); call nc_check_err(ierror, "coordinate mapping put_att", filename)
      ierror = nf90_put_att(idfile, idvar_coordmap, 'longitude_of_prime_meridian', 0d0); call nc_check_err(ierror, "coordinate mapping put_att", filename)
      ierror = nf90_put_att(idfile, idvar_coordmap, 'semi_major_axis', dearthrad); call nc_check_err(ierror, "coordinate mapping put_att", filename)
      ierror = nf90_put_att(idfile, idvar_coordmap, 'semi_minor_axis', 6356752.314245d0); call nc_check_err(ierror, "coordinate mapping put_att", filename)
      ierror = nf90_put_att(idfile, idvar_coordmap, 'inverse_flattening', 298.257223563d0); call nc_check_err(ierror, "coordinate mapping put_att", filename)
      ierror = nf90_put_att(idfile, idvar_coordmap, 'proj4_params', ' '); call nc_check_err(ierror, "coordinate mapping put_att", filename)
      ierror = nf90_put_att(idfile, idvar_coordmap, 'EPSG_code', trim(epsgstring)); call nc_check_err(ierror, "coordinate mapping put_att", filename)
      ierror = nf90_put_att(idfile, idvar_coordmap, 'projection_name', ' '); call nc_check_err(ierror, "coordinate mapping put_att", filename)
      ierror = nf90_put_att(idfile, idvar_coordmap, 'wkt', ' '); call nc_check_err(ierror, "coordinate mapping put_att", filename)
      ierror = nf90_put_att(idfile, idvar_coordmap, 'comment', ' '); call nc_check_err(ierror, "coordinate mapping put_att", filename)
      ierror = nf90_put_att(idfile, idvar_coordmap, 'value', 'value is equal to EPSG code'); call nc_check_err(ierror, "coordinate mapping put_att", filename)
      !
      ! name, type, dims, standardname, longname, unit, xycoordinates
      idvar_x = nc_def_var(idfile, 'x', nf90_double, 2, (/iddim_mmax, iddim_nmax/), 'projection_x_coordinate', 'x-coordinate of cell centres', trim(string), .false., filename)
      ierror = nf90_put_att(idfile, idvar_x, 'grid_mapping', 'projected_coordinate_system'); call nc_check_err(ierror, "put_att x grid_mapping", filename)
      ierror = nf90_put_att(idfile, idvar_x, '_FillValue', NF90_FILL_DOUBLE); call nc_check_err(ierror, "put_att x _FillValue", filename)
      idvar_y = nc_def_var(idfile, 'y', nf90_double, 2, (/iddim_mmax, iddim_nmax/), 'projection_y_coordinate', 'y-coordinate of cell centres', trim(string), .false., filename)
      ierror = nf90_put_att(idfile, idvar_y, 'grid_mapping', 'projected_coordinate_system'); call nc_check_err(ierror, "put_att y grid_mapping", filename)
      ierror = nf90_put_att(idfile, idvar_y, '_FillValue', NF90_FILL_DOUBLE); call nc_check_err(ierror, "put_att y _FillValue", filename)
      write (string, '(a,i0.4,a,i0.2,a,i0.2,a)') 'seconds since ', year, '-', month, '-', day, ' 00:00:00'
      idvar_time = nc_def_var(idfile, 'time', nf90_double, 1, (/iddim_time/), 'time', 'time', trim(string), .false., filename)
      idvar_kcs = nc_def_var(idfile, 'kcs', nf90_int, 2, (/iddim_mmax, iddim_nmax/), '', 'Active(1), Inactive(0), boundary(2) indicator', '-', .true., filename)
      idvar_hsign = nc_def_var(idfile, 'hsign', precision, 3, (/iddim_mmax, iddim_nmax, iddim_time/), '', 'Significant wave height', 'm', .true., filename)
      if (nautical_convention) then
         idvar_dir = nc_def_var(idfile, 'dir', precision, 3, (/iddim_mmax, iddim_nmax, iddim_time/), 'sea_surface_wave_from_direction', 'Mean wave direction', 'deg', .true., filename)
         idvar_pdir = nc_def_var(idfile, 'pdir', precision, 3, (/iddim_mmax, iddim_nmax, iddim_time/), 'sea_surface_wave_from_direction', 'Peak wave direction', 'deg', .true., filename)
      else
         idvar_dir = nc_def_var(idfile, 'dir', precision, 3, (/iddim_mmax, iddim_nmax, iddim_time/), 'sea_surface_wave_to_direction', 'Mean wave direction', 'deg', .true., filename)
         idvar_pdir = nc_def_var(idfile, 'pdir', precision, 3, (/iddim_mmax, iddim_nmax, iddim_time/), 'sea_surface_wave_to_direction', 'Peak wave direction', 'deg', .true., filename)
      end if
      idvar_period = nc_def_var(idfile, 'period', precision, 3, (/iddim_mmax, iddim_nmax, iddim_time/), '', 'Mean wave period', 'sec', .true., filename)
      idvar_rtp = nc_def_var(idfile, 'rtp', precision, 3, (/iddim_mmax, iddim_nmax, iddim_time/), '', 'Relative peak wave period', 'sec', .true., filename)
      idvar_depth = nc_def_var(idfile, 'depth', precision, 3, (/iddim_mmax, iddim_nmax, iddim_time/), '', 'Water depth', 'm', .true., filename)
      ierror = nf90_put_att(idfile, idvar_depth, 'coordinates', 'x y'); call nc_check_err(ierror, "put_att depth", filename)
      ierror = nf90_put_att(idfile, idvar_depth, 'grid_mapping', 'projected_coordinate_system'); call nc_check_err(ierror, "put_att depth", filename)
      idvar_velx = nc_def_var(idfile, 'veloc-x', precision, 3, (/iddim_mmax, iddim_nmax, iddim_time/), '', 'Current velocity (x-component)', 'm/s', .true., filename)
      idvar_vely = nc_def_var(idfile, 'veloc-y', precision, 3, (/iddim_mmax, iddim_nmax, iddim_time/), '', 'Current velocity (y-component)', 'm/s', .true., filename)
      idvar_transpx = nc_def_var(idfile, 'transp-x', precision, 3, (/iddim_mmax, iddim_nmax, iddim_time/), '', 'Energy transport vector (x-component)', 'w/m', .true., filename)
      idvar_transpy = nc_def_var(idfile, 'transp-y', precision, 3, (/iddim_mmax, iddim_nmax, iddim_time/), '', 'Energy transport vector (y-component)', 'w/m', .true., filename)
      idvar_dspr = nc_def_var(idfile, 'dspr', precision, 3, (/iddim_mmax, iddim_nmax, iddim_time/), '', 'Directional spread of the waves', 'deg', .true., filename)
      idvar_dissip = nc_def_var(idfile, 'dissip', precision, 3, (/iddim_mmax, iddim_nmax, iddim_time/), '', 'Energy dissipation', 'n/m/sec', .true., filename)
      idvar_leak = nc_def_var(idfile, 'leak', precision, 3, (/iddim_mmax, iddim_nmax, iddim_time/), '', 'Leakage of energy over sector boundaries', 'j/m2/s', .true., filename)
      idvar_qb = nc_def_var(idfile, 'qb', precision, 3, (/iddim_mmax, iddim_nmax, iddim_time/), '', 'Fraction of breaking waves', '-', .true., filename)
      idvar_ubot = nc_def_var(idfile, 'ubot', precision, 3, (/iddim_mmax, iddim_nmax, iddim_time/), '', 'Rms value maximum of the orbital velocity near bed level', 'm/s', .true., filename)
      idvar_steepw = nc_def_var(idfile, 'steepw', precision, 3, (/iddim_mmax, iddim_nmax, iddim_time/), '', 'Mean wave steepness', '-', .true., filename)
      idvar_wlength = nc_def_var(idfile, 'wlength', precision, 3, (/iddim_mmax, iddim_nmax, iddim_time/), '', 'Mean wave length', 'm', .true., filename)
      idvar_tps = nc_def_var(idfile, 'tps', precision, 3, (/iddim_mmax, iddim_nmax, iddim_time/), '', 'Smoothed peak period', 'sec', .true., filename)
      idvar_tm02 = nc_def_var(idfile, 'tm02', precision, 3, (/iddim_mmax, iddim_nmax, iddim_time/), '', 'Mean absolute zero-crossing period', 'sec', .true., filename)
      idvar_tmm10 = nc_def_var(idfile, 'tmm10', precision, 3, (/iddim_mmax, iddim_nmax, iddim_time/), '', 'Mean absolute wave period', 'sec', .true., filename)
      idvar_dhsign = nc_def_var(idfile, 'dhsign', precision, 3, (/iddim_mmax, iddim_nmax, iddim_time/), '', 'Difference in significant wave height (last iterations)', 'm', .true., filename)
      idvar_drtm01 = nc_def_var(idfile, 'drtm01', precision, 3, (/iddim_mmax, iddim_nmax, iddim_time/), '', 'Difference in average wave period (last iterations)', 'sec', .true., filename)
      idvar_setup = nc_def_var(idfile, 'setup', precision, 3, (/iddim_mmax, iddim_nmax, iddim_time/), '', 'Set-up due to waves', 'm', .true., filename)
      idvar_fx = nc_def_var(idfile, 'fx', precision, 3, (/iddim_mmax, iddim_nmax, iddim_time/), '', 'Wave induced force (x-component)', 'n/m2', .true., filename)
      idvar_fy = nc_def_var(idfile, 'fy', precision, 3, (/iddim_mmax, iddim_nmax, iddim_time/), '', 'Wave induced force (y-component)', 'n/m2', .true., filename)
      idvar_windu = nc_def_var(idfile, 'windu', precision, 3, (/iddim_mmax, iddim_nmax, iddim_time/), '', 'Wind velocity (x-component)', 'm/s', .true., filename)
      idvar_windv = nc_def_var(idfile, 'windv', precision, 3, (/iddim_mmax, iddim_nmax, iddim_time/), '', 'Wind velocity (y-component)', 'm/s', .true., filename)
      if (output_veg > 0) then
         idvar_nstems = nc_def_var(idfile, 'nstems', precision, 3, (/iddim_mmax, iddim_nmax, iddim_time/), '', 'Stem density', '1/m2', .true., filename)
      end if
      if (output_ice > 0) then
         idvar_icefrac = nc_def_var(idfile, 'icefrac', precision, 3, (/iddim_mmax, iddim_nmax, iddim_time/), '', 'Area fraction covered by ice', '1', .true., filename)
         if (output_ice == 1) then
            idvar_floedia = nc_def_var(idfile, 'floedia', precision, 3, (/iddim_mmax, iddim_nmax, iddim_time/), '', 'Ice floe diameter', 'm', .true., filename)
         end if
      end if
      do i = 1, sof%n_outpars
         idvar_outpars(i) = nc_def_var(idfile, sof%add_out_names(i), precision, 3, (/iddim_mmax, iddim_nmax, iddim_time/), '', sof%add_out_names(i), 'unknown', .true., filename)
      end do
      !
      ierror = nf90_enddef(idfile); call nc_check_err(ierror, "enddef", filename)
      !
      ! put vars (time independent)
      !
      ierror = nf90_put_var(idfile, idvar_x, tmp_x, start=(/1, 1/), count=(/sof%mmax, sof%nmax, 1/)); call nc_check_err(ierror, "put_var x", filename)
      ierror = nf90_put_var(idfile, idvar_y, tmp_y, start=(/1, 1/), count=(/sof%mmax, sof%nmax, 1/)); call nc_check_err(ierror, "put_var y", filename)
      ierror = nf90_put_var(idfile, idvar_kcs, sg%kcs, start=(/1, 1/), count=(/sof%mmax, sof%nmax, 1/)); call nc_check_err(ierror, "put_var kcs", filename)
   else
      !
      ! open file
      !
      ierror = nf90_open(filename, NF90_WRITE, idfile); call nc_check_err(ierror, "opening file", filename)
      !
      ierror = nf90_inq_varid(idfile, 'time', idvar_time); call nc_check_err(ierror, "inq_varid time   ", filename)
      ierror = nf90_inq_varid(idfile, 'hsign', idvar_hsign); call nc_check_err(ierror, "inq_varid hsign  ", filename)
      ierror = nf90_inq_varid(idfile, 'dir', idvar_dir); call nc_check_err(ierror, "inq_varid dir    ", filename)
      ierror = nf90_inq_varid(idfile, 'pdir', idvar_pdir); call nc_check_err(ierror, "inq_varid pdir   ", filename)
      ierror = nf90_inq_varid(idfile, 'period', idvar_period); call nc_check_err(ierror, "inq_varid period ", filename)
      ierror = nf90_inq_varid(idfile, 'rtp', idvar_rtp); call nc_check_err(ierror, "inq_varid rtp    ", filename)
      ierror = nf90_inq_varid(idfile, 'depth', idvar_depth); call nc_check_err(ierror, "inq_varid depth  ", filename)
      ierror = nf90_inq_varid(idfile, 'veloc-x', idvar_velx); call nc_check_err(ierror, "inq_varid velx   ", filename)
      ierror = nf90_inq_varid(idfile, 'veloc-y', idvar_vely); call nc_check_err(ierror, "inq_varid vely   ", filename)
      ierror = nf90_inq_varid(idfile, 'transp-x', idvar_transpx); call nc_check_err(ierror, "inq_varid transpx", filename)
      ierror = nf90_inq_varid(idfile, 'transp-y', idvar_transpy); call nc_check_err(ierror, "inq_varid transpy", filename)
      ierror = nf90_inq_varid(idfile, 'dspr', idvar_dspr); call nc_check_err(ierror, "inq_varid dspr   ", filename)
      ierror = nf90_inq_varid(idfile, 'dissip', idvar_dissip); call nc_check_err(ierror, "inq_varid dissip ", filename)
      ierror = nf90_inq_varid(idfile, 'leak', idvar_leak); call nc_check_err(ierror, "inq_varid leak   ", filename)
      ierror = nf90_inq_varid(idfile, 'qb', idvar_qb); call nc_check_err(ierror, "inq_varid qb     ", filename)
      ierror = nf90_inq_varid(idfile, 'ubot', idvar_ubot); call nc_check_err(ierror, "inq_varid ubot   ", filename)
      ierror = nf90_inq_varid(idfile, 'steepw', idvar_steepw); call nc_check_err(ierror, "inq_varid steepw ", filename)
      ierror = nf90_inq_varid(idfile, 'wlength', idvar_wlength); call nc_check_err(ierror, "inq_varid wlength", filename)
      ierror = nf90_inq_varid(idfile, 'tps', idvar_tps); call nc_check_err(ierror, "inq_varid tps    ", filename)
      ierror = nf90_inq_varid(idfile, 'tm02', idvar_tm02); call nc_check_err(ierror, "inq_varid tm02   ", filename)
      ierror = nf90_inq_varid(idfile, 'tmm10', idvar_tmm10); call nc_check_err(ierror, "inq_varid tmm10  ", filename)
      ierror = nf90_inq_varid(idfile, 'dhsign', idvar_dhsign); call nc_check_err(ierror, "inq_varid dhsign ", filename)
      ierror = nf90_inq_varid(idfile, 'drtm01', idvar_drtm01); call nc_check_err(ierror, "inq_varid drtm01 ", filename)
      ierror = nf90_inq_varid(idfile, 'setup', idvar_setup); call nc_check_err(ierror, "inq_varid setup  ", filename)
      ierror = nf90_inq_varid(idfile, 'fx', idvar_fx); call nc_check_err(ierror, "inq_varid fx     ", filename)
      ierror = nf90_inq_varid(idfile, 'fy', idvar_fy); call nc_check_err(ierror, "inq_varid fy     ", filename)
      ierror = nf90_inq_varid(idfile, 'windu', idvar_windu); call nc_check_err(ierror, "inq_varid windu  ", filename)
      ierror = nf90_inq_varid(idfile, 'windv', idvar_windv); call nc_check_err(ierror, "inq_varid windv  ", filename)
      if (output_veg > 0) then
         ierror = nf90_inq_varid(idfile, 'nstems', idvar_nstems); call nc_check_err(ierror, "inq_varid nstems ", filename)
      end if
      if (output_ice > 0) then
         ierror = nf90_inq_varid(idfile, 'icefrac', idvar_icefrac); call nc_check_err(ierror, "inq_varid icefrac", filename)
         if (output_ice == 1) then
            ierror = nf90_inq_varid(idfile, 'floedia', idvar_floedia); call nc_check_err(ierror, "inq_varid floedia", filename)
         end if
      end if
      do i = 1, sof%n_outpars
         ierror = nf90_inq_varid(idfile, sof%add_out_names(i), idvar_outpars(i)); call nc_check_err(ierror, "inq_varid "//sof%add_out_names(i), filename)
      end do
   end if
   !
   ! put vars (time dependent)
   !
   if (prevtime) then
      idummy(1) = real(wavedata%time%calctimtscale_prev,hp) * wavedata%time%tscale
   else
      idummy(1) = real(wavedata%time%calctimtscale,hp) * wavedata%time%tscale
   end if

   ierror = nf90_put_var(idfile, idvar_time, idummy(1), start=(/wavedata%output%count/)); call nc_check_err(ierror, "put_var time", filename)
   ierror = nf90_put_var(idfile, idvar_hsign, sof%hs, start=(/1, 1, wavedata%output%count/), count=(/sof%mmax, sof%nmax, 1/)); call nc_check_err(ierror, "put_var hsign", filename)
   if (nautical_convention) then
      ! dir: Nautical convention in SWAN output is converted to cartesian convention when read by D-Waves
      ! Here we convert it back to nautical convention for output to netCDF
      allocate (tmp_dir(sof%mmax, sof%nmax), stat=ierror)
      tmp_dir = reflect_between_nautical_and_cartesian(sof%dir, north_direction)
      ierror = nf90_put_var(idfile, idvar_dir, tmp_dir, start=(/1, 1, wavedata%output%count/), count=(/sof%mmax, sof%nmax, 1/)); call nc_check_err(ierror, "put_var dir    ", filename)
   else
      ierror = nf90_put_var(idfile, idvar_dir, sof%dir, start=(/1, 1, wavedata%output%count/), count=(/sof%mmax, sof%nmax, 1/)); call nc_check_err(ierror, "put_var dir    ", filename)   
   end if
   if (nautical_convention) then
      ! pdir: Nautical convention in SWAN output is converted to cartesian convention when read by D-Waves
      ! Here we convert it back to nautical convention for output to netCDF
      ! Already allocated: tmp_dir
      tmp_dir = reflect_between_nautical_and_cartesian(sof%pdir, north_direction)
      ierror = nf90_put_var(idfile, idvar_pdir, tmp_dir, start=(/1, 1, wavedata%output%count/), count=(/sof%mmax, sof%nmax, 1/)); call nc_check_err(ierror, "put_var pdir   ", filename)
   else
      ierror = nf90_put_var(idfile, idvar_pdir, sof%pdir, start=(/1, 1, wavedata%output%count/), count=(/sof%mmax, sof%nmax, 1/)); call nc_check_err(ierror, "put_var pdir   ", filename)
   end if
   ierror = nf90_put_var(idfile, idvar_period, sof%period, start=(/1, 1, wavedata%output%count/), count=(/sof%mmax, sof%nmax, 1/)); call nc_check_err(ierror, "put_var period ", filename)
   ierror = nf90_put_var(idfile, idvar_rtp, sof%rtp, start=(/1, 1, wavedata%output%count/), count=(/sof%mmax, sof%nmax, 1/)); call nc_check_err(ierror, "put_var rtp    ", filename)
   ierror = nf90_put_var(idfile, idvar_depth, sof%depth, start=(/1, 1, wavedata%output%count/), count=(/sof%mmax, sof%nmax, 1/)); call nc_check_err(ierror, "put_var depth  ", filename)
   ierror = nf90_put_var(idfile, idvar_velx, sof%u, start=(/1, 1, wavedata%output%count/), count=(/sof%mmax, sof%nmax, 1/)); call nc_check_err(ierror, "put_var velx   ", filename)
   ierror = nf90_put_var(idfile, idvar_vely, sof%v, start=(/1, 1, wavedata%output%count/), count=(/sof%mmax, sof%nmax, 1/)); call nc_check_err(ierror, "put_var vely   ", filename)
   ierror = nf90_put_var(idfile, idvar_transpx, sof%mx, start=(/1, 1, wavedata%output%count/), count=(/sof%mmax, sof%nmax, 1/)); call nc_check_err(ierror, "put_var transpx", filename)
   ierror = nf90_put_var(idfile, idvar_transpy, sof%my, start=(/1, 1, wavedata%output%count/), count=(/sof%mmax, sof%nmax, 1/)); call nc_check_err(ierror, "put_var transpy", filename)
   ierror = nf90_put_var(idfile, idvar_dspr, sof%dspr, start=(/1, 1, wavedata%output%count/), count=(/sof%mmax, sof%nmax, 1/)); call nc_check_err(ierror, "put_var dspr   ", filename)
   ierror = nf90_put_var(idfile, idvar_dissip, sof%dissip, start=(/1, 1, wavedata%output%count/), count=(/sof%mmax, sof%nmax, 1/)); call nc_check_err(ierror, "put_var dissip ", filename)
   ierror = nf90_put_var(idfile, idvar_leak, sof%rleak, start=(/1, 1, wavedata%output%count/), count=(/sof%mmax, sof%nmax, 1/)); call nc_check_err(ierror, "put_var leak   ", filename)
   ierror = nf90_put_var(idfile, idvar_qb, sof%qb, start=(/1, 1, wavedata%output%count/), count=(/sof%mmax, sof%nmax, 1/)); call nc_check_err(ierror, "put_var qb     ", filename)
   ierror = nf90_put_var(idfile, idvar_ubot, sof%ubot, start=(/1, 1, wavedata%output%count/), count=(/sof%mmax, sof%nmax, 1/)); call nc_check_err(ierror, "put_var ubot   ", filename)
   ierror = nf90_put_var(idfile, idvar_steepw, sof%steep, start=(/1, 1, wavedata%output%count/), count=(/sof%mmax, sof%nmax, 1/)); call nc_check_err(ierror, "put_var steepw ", filename)
   ierror = nf90_put_var(idfile, idvar_wlength, sof%wlen, start=(/1, 1, wavedata%output%count/), count=(/sof%mmax, sof%nmax, 1/)); call nc_check_err(ierror, "put_var wlength", filename)
   ierror = nf90_put_var(idfile, idvar_tps, sof%tps, start=(/1, 1, wavedata%output%count/), count=(/sof%mmax, sof%nmax, 1/)); call nc_check_err(ierror, "put_var tps    ", filename)
   ierror = nf90_put_var(idfile, idvar_tm02, sof%tm02, start=(/1, 1, wavedata%output%count/), count=(/sof%mmax, sof%nmax, 1/)); call nc_check_err(ierror, "put_var tm02   ", filename)
   ierror = nf90_put_var(idfile, idvar_tmm10, sof%tmm10, start=(/1, 1, wavedata%output%count/), count=(/sof%mmax, sof%nmax, 1/)); call nc_check_err(ierror, "put_var tmm10  ", filename)
   ierror = nf90_put_var(idfile, idvar_dhsign, sof%dhsign, start=(/1, 1, wavedata%output%count/), count=(/sof%mmax, sof%nmax, 1/)); call nc_check_err(ierror, "put_var dhsign ", filename)
   ierror = nf90_put_var(idfile, idvar_drtm01, sof%drtm01, start=(/1, 1, wavedata%output%count/), count=(/sof%mmax, sof%nmax, 1/)); call nc_check_err(ierror, "put_var drtm01 ", filename)
   ierror = nf90_put_var(idfile, idvar_setup, sof%setup, start=(/1, 1, wavedata%output%count/), count=(/sof%mmax, sof%nmax, 1/)); call nc_check_err(ierror, "put_var setup  ", filename)
   ierror = nf90_put_var(idfile, idvar_fx, sof%fx, start=(/1, 1, wavedata%output%count/), count=(/sof%mmax, sof%nmax, 1/)); call nc_check_err(ierror, "put_var fx     ", filename)
   ierror = nf90_put_var(idfile, idvar_fy, sof%fy, start=(/1, 1, wavedata%output%count/), count=(/sof%mmax, sof%nmax, 1/)); call nc_check_err(ierror, "put_var fy     ", filename)
   ierror = nf90_put_var(idfile, idvar_windu, sof%windu, start=(/1, 1, wavedata%output%count/), count=(/sof%mmax, sof%nmax, 1/)); call nc_check_err(ierror, "put_var windu  ", filename)
   ierror = nf90_put_var(idfile, idvar_windv, sof%windv, start=(/1, 1, wavedata%output%count/), count=(/sof%mmax, sof%nmax, 1/)); call nc_check_err(ierror, "put_var windv  ", filename)
   if (output_veg > 0) then
      ierror = nf90_put_var(idfile, idvar_nstems, sif_veg, start=(/1, 1, wavedata%output%count/), count=(/sof%mmax, sof%nmax, 1/)); call nc_check_err(ierror, "put_var nstems ", filename)
   end if
   if (output_ice > 0) then
      ierror = nf90_put_var(idfile, idvar_icefrac, sif%ice_frac, start=(/1, 1, wavedata%output%count/), count=(/sof%mmax, sof%nmax, 1/)); call nc_check_err(ierror, "put_var icefrac", filename)
      if (output_ice == 1) then
         ierror = nf90_put_var(idfile, idvar_floedia, sif%floe_dia, start=(/1, 1, wavedata%output%count/), count=(/sof%mmax, sof%nmax, 1/)); call nc_check_err(ierror, "put_var floedia", filename)
      end if
   end if
   do i = 1, sof%n_outpars
      ierror = nf90_put_var(idfile, idvar_outpars(i), sof%add_out_vals(:, :, i), start=(/1, 1, wavedata%output%count/), count=(/sof%mmax, sof%nmax, 1/)); call nc_check_err(ierror, "put_var "//sof%add_out_names(i), filename)
   end do
   !
   ierror = nf90_sync(idfile); call nc_check_err(ierror, "sync file", filename)
   ierror = nf90_close(idfile); call nc_check_err(ierror, "closing file", filename)

   deallocate (idvar_outpars, stat=ierror)
   deallocate (tmp_x, stat=ierror)
   deallocate (tmp_y, stat=ierror)
end subroutine write_wave_map_netcdf


subroutine write_wave_map_netcdf_unstructured(sg, sof, sif, wavedata, filename, prevtime, precision, &
                                              sif_mmax, sif_nmax, sif_veg, output_ice, output_veg, &
                                              nautical_convention, north_direction)
!----- GPL ---------------------------------------------------------------------
!!--declarations----------------------------------------------------------------
   use wave_data
   use swan_flow_grid_maps
   use netcdf
   use nc_check, only : nc_check_err
   use precision_basics
   use dwaves_version_module
   use angle_convention, only : reflect_between_nautical_and_cartesian
   !
   implicit none
!
! Global variables
!
   type(grid), intent(in) :: sg ! swan grid
   type(output_fields), intent(in) :: sof ! output fields defined on swan grid
   type(input_fields), intent(in) :: sif ! input fields defined on swan grid
   type(wave_data_type), intent(in) :: wavedata
   character(*), intent(in) :: filename
   logical, intent(in) :: prevtime ! true: the time to be written is the "previous time"
   integer, intent(in) :: precision
   logical, intent(in) :: nautical_convention ! true: angles are according to the nautical convention
   integer, intent(in) :: output_ice ! switch for writing ice quantities
   integer, intent(in) :: output_veg ! switch for writing vegetation quantities
   integer, intent(in) :: sif_mmax
   integer, intent(in) :: sif_nmax
   real   , intent(in) :: north_direction ! direction of north in degrees, used to convert to nautical convention if nautical_convention is true
   real, dimension(sif_mmax, sif_nmax), intent(in) :: sif_veg
!
! Local variables
!
   integer :: epsg
   integer :: i
   integer :: idfile
   integer :: iddim_node
   integer :: iddim_face
   integer :: iddim_max_face_nodes
   integer :: iddim_time
   integer :: idvar_coordmap
   integer :: idvar_mesh
   integer :: idvar_node_x
   integer :: idvar_node_y
   integer :: idvar_face_nodes
   integer :: idvar_time
   integer :: idvar_kcs
   integer :: idvar_hsign
   integer :: idvar_dir
   integer :: idvar_pdir
   integer :: idvar_period
   integer :: idvar_rtp
   integer :: idvar_depth
   integer :: idvar_velx
   integer :: idvar_vely
   integer :: idvar_transpx
   integer :: idvar_transpy
   integer :: idvar_dspr
   integer :: idvar_dissip
   integer :: idvar_leak
   integer :: idvar_qb
   integer :: idvar_ubot
   integer :: idvar_steepw
   integer :: idvar_wlength
   integer :: idvar_tps
   integer :: idvar_tm02
   integer :: idvar_tmm10
   integer :: idvar_dhsign
   integer :: idvar_drtm01
   integer :: idvar_setup
   integer :: idvar_fx
   integer :: idvar_fy
   integer :: idvar_windu
   integer :: idvar_windv
   integer :: idvar_nstems
   integer :: idvar_icefrac
   integer :: idvar_floedia
   integer :: ierror
   integer :: year
   integer :: month
   integer :: day
   integer :: nnode
   integer :: nface
   integer, dimension(:), allocatable :: idvar_outpars
   integer, dimension(:, :), allocatable :: face_nodes
   integer, external :: nc_def_var
   real(hp) :: dearthrad
   real(hp), dimension(1) :: idummy
   real(hp), dimension(:), allocatable :: node_x
   real(hp), dimension(:), allocatable :: node_y
   real, dimension(:), allocatable :: tmp_dir
   character(8) :: cdate
   character(10) :: ctime
   character(5) :: czone
   character(256) :: epsgstring
   character(256) :: full_version
   character(256) :: string
!
!! executable statements -------------------------------------------------------
!
   nnode = sg%mmax
   nface = sg%ncell
   if (nnode <= 0 .or. nface <= 0 .or. .not. associated(sg%kvertc)) then
      write (*, '(a)') 'ERROR: unstructured WAVE NetCDF map output requires triangular connectivity.'
      return
   end if

   dearthrad = 6378137.0_hp
   call getfullversionstring_dwaves(full_version)
   call date_and_time(cdate, ctime, czone)
   year = wavedata%time%refdate / 10000
   month = (wavedata%time%refdate - year * 10000) / 100
   day = wavedata%time%refdate - year * 10000 - month * 100

   allocate (idvar_outpars(sof%n_outpars), stat=ierror)
   if (ierror /= 0) write (*, *) 'ERROR allocating idvar_outpars in write_wave_map_netcdf_unstructured'

   if (wavedata%output%count == 1) then
      allocate (node_x(nnode), stat=ierror)
      allocate (node_y(nnode), stat=ierror)
      allocate (face_nodes(3, nface), stat=ierror)
      if (ierror /= 0) write (*, *) 'ERROR allocating unstructured WAVE NetCDF map arrays'
      node_x = sg%x(1:nnode, 1)
      node_y = sg%y(1:nnode, 1)
      face_nodes = sg%kvertc(1:3, 1:nface)
      !
      ! create file
      !
      ierror = nf90_create(filename, wavedata%output%ncmode, idfile); call nc_check_err(ierror, 'creating file', filename)
      !
      ! global attributes
      !
      ierror = nf90_put_att(idfile, nf90_global, 'institution', trim(company)); call nc_check_err(ierror, 'put_att global institution', filename)
      ierror = nf90_put_att(idfile, nf90_global, 'references', trim(company_url)); call nc_check_err(ierror, 'put_att global references', filename)
      ierror = nf90_put_att(idfile, nf90_global, 'source', trim(full_version)); call nc_check_err(ierror, 'put_att global source', filename)
      ierror = nf90_put_att(idfile, nf90_global, 'Conventions', 'CF-1.8 UGRID-1.0'); call nc_check_err(ierror, 'put_att global conventions', filename)
      ierror = nf90_put_att(idfile, nf90_global, 'gridType', 'unstructured'); call nc_check_err(ierror, 'put_att global gridType', filename)
      ierror = nf90_put_att(idfile, nf90_global, 'history', &
                            'Created on '//cdate(1:4)//'-'//cdate(5:6)//'-'//cdate(7:8)//'T'//ctime(1:2)//':'//ctime(3:4)//':'//ctime(5:6)//czone(1:5)// &
                            ', '//trim(product_name)); call nc_check_err(ierror, 'put_att global history', filename)
      if (nautical_convention) then
         ierror = nf90_put_att(idfile, nf90_global, 'Directional_convention', 'nautical'); call nc_check_err(ierror, 'put_att global direction', filename)
      else
         ierror = nf90_put_att(idfile, nf90_global, 'Directional_convention', 'cartesian'); call nc_check_err(ierror, 'put_att global direction', filename)
      end if
      !
      ! dimensions
      !
      ierror = nf90_def_dim(idfile, 'nMesh2d_node', nnode, iddim_node); call nc_check_err(ierror, 'def_dim nMesh2d_node', filename)
      ierror = nf90_def_dim(idfile, 'nMesh2d_face', nface, iddim_face); call nc_check_err(ierror, 'def_dim nMesh2d_face', filename)
      ierror = nf90_def_dim(idfile, 'nMaxMesh2d_face_nodes', 3, iddim_max_face_nodes); call nc_check_err(ierror, 'def_dim nMaxMesh2d_face_nodes', filename)
      ierror = nf90_def_dim(idfile, 'time', nf90_unlimited, iddim_time); call nc_check_err(ierror, 'def_dim time', filename)
      !
      ! coordinate mapping
      !
      ierror = nf90_def_var(idfile, 'projected_coordinate_system', nf90_int, idvar_coordmap); call nc_check_err(ierror, 'def_var coordinate mapping', filename)
      if (sg%sferic) then
         epsg = 4326
         epsgstring = 'EPSG:4326'
         ierror = nf90_put_att(idfile, idvar_coordmap, 'name', 'WGS84'); call nc_check_err(ierror, 'coordinate mapping put_att', filename)
         ierror = nf90_put_att(idfile, idvar_coordmap, 'grid_mapping_name', 'latitude_longitude'); call nc_check_err(ierror, 'coordinate mapping put_att', filename)
         string = 'deg'
      else
         epsg = 28992
         epsgstring = 'EPSG:28992'
         ierror = nf90_put_att(idfile, idvar_coordmap, 'name', 'Unknown projected'); call nc_check_err(ierror, 'coordinate mapping put_att', filename)
         ierror = nf90_put_att(idfile, idvar_coordmap, 'grid_mapping_name', 'Unknown projected'); call nc_check_err(ierror, 'coordinate mapping put_att', filename)
         string = 'm'
      end if
      ierror = nf90_put_att(idfile, idvar_coordmap, 'epsg', epsg); call nc_check_err(ierror, 'coordinate mapping put_att', filename)
      ierror = nf90_put_att(idfile, idvar_coordmap, 'longitude_of_prime_meridian', 0d0); call nc_check_err(ierror, 'coordinate mapping put_att', filename)
      ierror = nf90_put_att(idfile, idvar_coordmap, 'semi_major_axis', dearthrad); call nc_check_err(ierror, 'coordinate mapping put_att', filename)
      ierror = nf90_put_att(idfile, idvar_coordmap, 'semi_minor_axis', 6356752.314245d0); call nc_check_err(ierror, 'coordinate mapping put_att', filename)
      ierror = nf90_put_att(idfile, idvar_coordmap, 'inverse_flattening', 298.257223563d0); call nc_check_err(ierror, 'coordinate mapping put_att', filename)
      ierror = nf90_put_att(idfile, idvar_coordmap, 'proj4_params', ' '); call nc_check_err(ierror, 'coordinate mapping put_att', filename)
      ierror = nf90_put_att(idfile, idvar_coordmap, 'EPSG_code', trim(epsgstring)); call nc_check_err(ierror, 'coordinate mapping put_att', filename)
      ierror = nf90_put_att(idfile, idvar_coordmap, 'projection_name', ' '); call nc_check_err(ierror, 'coordinate mapping put_att', filename)
      ierror = nf90_put_att(idfile, idvar_coordmap, 'wkt', ' '); call nc_check_err(ierror, 'coordinate mapping put_att', filename)
      ierror = nf90_put_att(idfile, idvar_coordmap, 'comment', ' '); call nc_check_err(ierror, 'coordinate mapping put_att', filename)
      ierror = nf90_put_att(idfile, idvar_coordmap, 'value', 'value is equal to EPSG code'); call nc_check_err(ierror, 'coordinate mapping put_att', filename)
      !
      ! UGRID mesh topology and coordinates
      !
      ierror = nf90_def_var(idfile, 'Mesh2d', nf90_int, idvar_mesh); call nc_check_err(ierror, 'def_var Mesh2d', filename)
      ierror = nf90_put_att(idfile, idvar_mesh, 'cf_role', 'mesh_topology'); call nc_check_err(ierror, 'put_att Mesh2d cf_role', filename)
      ierror = nf90_put_att(idfile, idvar_mesh, 'topology_dimension', 2); call nc_check_err(ierror, 'put_att Mesh2d topology_dimension', filename)
      ierror = nf90_put_att(idfile, idvar_mesh, 'node_coordinates', 'Mesh2d_node_x Mesh2d_node_y'); call nc_check_err(ierror, 'put_att Mesh2d node_coordinates', filename)
      ierror = nf90_put_att(idfile, idvar_mesh, 'face_node_connectivity', 'Mesh2d_face_nodes'); call nc_check_err(ierror, 'put_att Mesh2d face_node_connectivity', filename)
      ierror = nf90_put_att(idfile, idvar_mesh, 'node_dimension', 'nMesh2d_node'); call nc_check_err(ierror, 'put_att Mesh2d node_dimension', filename)
      ierror = nf90_put_att(idfile, idvar_mesh, 'face_dimension', 'nMesh2d_face'); call nc_check_err(ierror, 'put_att Mesh2d face_dimension', filename)

      idvar_node_x = nc_def_var(idfile, 'Mesh2d_node_x', nf90_double, 1, (/iddim_node/), 'projection_x_coordinate', 'x-coordinate of mesh nodes', trim(string), .false., filename)
      ierror = nf90_put_att(idfile, idvar_node_x, 'grid_mapping', 'projected_coordinate_system'); call nc_check_err(ierror, 'put_att Mesh2d_node_x grid_mapping', filename)
      idvar_node_y = nc_def_var(idfile, 'Mesh2d_node_y', nf90_double, 1, (/iddim_node/), 'projection_y_coordinate', 'y-coordinate of mesh nodes', trim(string), .false., filename)
      ierror = nf90_put_att(idfile, idvar_node_y, 'grid_mapping', 'projected_coordinate_system'); call nc_check_err(ierror, 'put_att Mesh2d_node_y grid_mapping', filename)
      idvar_face_nodes = nc_def_var(idfile, 'Mesh2d_face_nodes', nf90_int, 2, (/iddim_max_face_nodes, iddim_face/), '', 'Maps every face to its three corner nodes', '', .false., filename)
      ierror = nf90_put_att(idfile, idvar_face_nodes, 'cf_role', 'face_node_connectivity'); call nc_check_err(ierror, 'put_att Mesh2d_face_nodes cf_role', filename)
      ierror = nf90_put_att(idfile, idvar_face_nodes, 'start_index', 1); call nc_check_err(ierror, 'put_att Mesh2d_face_nodes start_index', filename)
      ierror = nf90_put_att(idfile, idvar_face_nodes, '_FillValue', -1); call nc_check_err(ierror, 'put_att Mesh2d_face_nodes _FillValue', filename)

      write (string, '(a,i0.4,a,i0.2,a,i0.2,a)') 'seconds since ', year, '-', month, '-', day, ' 00:00:00'
      idvar_time = nc_def_var(idfile, 'time', nf90_double, 1, (/iddim_time/), 'time', 'time', trim(string), .false., filename)
      idvar_kcs = define_node_int_var('kcs', '', 'Active(1), Inactive(0), boundary(2) indicator', '-')
      idvar_hsign = define_node_var('hsign', '', 'Significant wave height', 'm')
      if (nautical_convention) then
         idvar_dir = define_node_var('dir', 'sea_surface_wave_from_direction', 'Mean wave direction', 'deg')
         idvar_pdir = define_node_var('pdir', 'sea_surface_wave_from_direction', 'Peak wave direction', 'deg')
      else
         idvar_dir = define_node_var('dir', 'sea_surface_wave_to_direction', 'Mean wave direction', 'deg')
         idvar_pdir = define_node_var('pdir', 'sea_surface_wave_to_direction', 'Peak wave direction', 'deg')
      end if
      idvar_period = define_node_var('period', '', 'Mean wave period', 'sec')
      idvar_rtp = define_node_var('rtp', '', 'Relative peak wave period', 'sec')
      idvar_depth = define_node_var('depth', '', 'Water depth', 'm')
      idvar_velx = define_node_var('veloc-x', '', 'Current velocity (x-component)', 'm/s')
      idvar_vely = define_node_var('veloc-y', '', 'Current velocity (y-component)', 'm/s')
      idvar_transpx = define_node_var('transp-x', '', 'Energy transport vector (x-component)', 'w/m')
      idvar_transpy = define_node_var('transp-y', '', 'Energy transport vector (y-component)', 'w/m')
      idvar_dspr = define_node_var('dspr', '', 'Directional spread of the waves', 'deg')
      idvar_dissip = define_node_var('dissip', '', 'Energy dissipation', 'n/m/sec')
      idvar_leak = define_node_var('leak', '', 'Leakage of energy over sector boundaries', 'j/m2/s')
      idvar_qb = define_node_var('qb', '', 'Fraction of breaking waves', '-')
      idvar_ubot = define_node_var('ubot', '', 'Rms value maximum of the orbital velocity near bed level', 'm/s')
      idvar_steepw = define_node_var('steepw', '', 'Mean wave steepness', '-')
      idvar_wlength = define_node_var('wlength', '', 'Mean wave length', 'm')
      idvar_tps = define_node_var('tps', '', 'Smoothed peak period', 'sec')
      idvar_tm02 = define_node_var('tm02', '', 'Mean absolute zero-crossing period', 'sec')
      idvar_tmm10 = define_node_var('tmm10', '', 'Mean absolute wave period', 'sec')
      idvar_dhsign = define_node_var('dhsign', '', 'Difference in significant wave height (last iterations)', 'm')
      idvar_drtm01 = define_node_var('drtm01', '', 'Difference in average wave period (last iterations)', 'sec')
      idvar_setup = define_node_var('setup', '', 'Set-up due to waves', 'm')
      idvar_fx = define_node_var('fx', '', 'Wave induced force (x-component)', 'n/m2')
      idvar_fy = define_node_var('fy', '', 'Wave induced force (y-component)', 'n/m2')
      idvar_windu = define_node_var('windu', '', 'Wind velocity (x-component)', 'm/s')
      idvar_windv = define_node_var('windv', '', 'Wind velocity (y-component)', 'm/s')
      if (output_veg > 0) then
         idvar_nstems = define_node_var('nstems', '', 'Stem density', '1/m2')
      end if
      if (output_ice > 0) then
         idvar_icefrac = define_node_var('icefrac', '', 'Area fraction covered by ice', '1')
         if (output_ice == 1) then
            idvar_floedia = define_node_var('floedia', '', 'Ice floe diameter', 'm')
         end if
      end if
      do i = 1, sof%n_outpars
         idvar_outpars(i) = define_node_var(sof%add_out_names(i), '', sof%add_out_names(i), 'unknown')
      end do
      !
      ierror = nf90_enddef(idfile); call nc_check_err(ierror, 'enddef', filename)
      !
      ! put vars (time independent)
      !
      ierror = nf90_put_var(idfile, idvar_mesh, 0); call nc_check_err(ierror, 'put_var Mesh2d', filename)
      ierror = nf90_put_var(idfile, idvar_node_x, node_x, start=(/1/), count=(/nnode/)); call nc_check_err(ierror, 'put_var Mesh2d_node_x', filename)
      ierror = nf90_put_var(idfile, idvar_node_y, node_y, start=(/1/), count=(/nnode/)); call nc_check_err(ierror, 'put_var Mesh2d_node_y', filename)
      ierror = nf90_put_var(idfile, idvar_face_nodes, face_nodes, start=(/1, 1/), count=(/3, nface/)); call nc_check_err(ierror, 'put_var Mesh2d_face_nodes', filename)
      call put_node_int_var(idvar_kcs, 'kcs', sg%kcs)
      deallocate (node_x, stat=ierror)
      deallocate (node_y, stat=ierror)
      deallocate (face_nodes, stat=ierror)
   else
      !
      ! open file
      !
      ierror = nf90_open(filename, NF90_WRITE, idfile); call nc_check_err(ierror, 'opening file', filename)
      !
      ierror = nf90_inq_varid(idfile, 'time', idvar_time); call nc_check_err(ierror, 'inq_varid time   ', filename)
      ierror = nf90_inq_varid(idfile, 'hsign', idvar_hsign); call nc_check_err(ierror, 'inq_varid hsign  ', filename)
      ierror = nf90_inq_varid(idfile, 'dir', idvar_dir); call nc_check_err(ierror, 'inq_varid dir    ', filename)
      ierror = nf90_inq_varid(idfile, 'pdir', idvar_pdir); call nc_check_err(ierror, 'inq_varid pdir   ', filename)
      ierror = nf90_inq_varid(idfile, 'period', idvar_period); call nc_check_err(ierror, 'inq_varid period ', filename)
      ierror = nf90_inq_varid(idfile, 'rtp', idvar_rtp); call nc_check_err(ierror, 'inq_varid rtp    ', filename)
      ierror = nf90_inq_varid(idfile, 'depth', idvar_depth); call nc_check_err(ierror, 'inq_varid depth  ', filename)
      ierror = nf90_inq_varid(idfile, 'veloc-x', idvar_velx); call nc_check_err(ierror, 'inq_varid velx   ', filename)
      ierror = nf90_inq_varid(idfile, 'veloc-y', idvar_vely); call nc_check_err(ierror, 'inq_varid vely   ', filename)
      ierror = nf90_inq_varid(idfile, 'transp-x', idvar_transpx); call nc_check_err(ierror, 'inq_varid transpx', filename)
      ierror = nf90_inq_varid(idfile, 'transp-y', idvar_transpy); call nc_check_err(ierror, 'inq_varid transpy', filename)
      ierror = nf90_inq_varid(idfile, 'dspr', idvar_dspr); call nc_check_err(ierror, 'inq_varid dspr   ', filename)
      ierror = nf90_inq_varid(idfile, 'dissip', idvar_dissip); call nc_check_err(ierror, 'inq_varid dissip ', filename)
      ierror = nf90_inq_varid(idfile, 'leak', idvar_leak); call nc_check_err(ierror, 'inq_varid leak   ', filename)
      ierror = nf90_inq_varid(idfile, 'qb', idvar_qb); call nc_check_err(ierror, 'inq_varid qb     ', filename)
      ierror = nf90_inq_varid(idfile, 'ubot', idvar_ubot); call nc_check_err(ierror, 'inq_varid ubot   ', filename)
      ierror = nf90_inq_varid(idfile, 'steepw', idvar_steepw); call nc_check_err(ierror, 'inq_varid steepw ', filename)
      ierror = nf90_inq_varid(idfile, 'wlength', idvar_wlength); call nc_check_err(ierror, 'inq_varid wlength', filename)
      ierror = nf90_inq_varid(idfile, 'tps', idvar_tps); call nc_check_err(ierror, 'inq_varid tps    ', filename)
      ierror = nf90_inq_varid(idfile, 'tm02', idvar_tm02); call nc_check_err(ierror, 'inq_varid tm02   ', filename)
      ierror = nf90_inq_varid(idfile, 'tmm10', idvar_tmm10); call nc_check_err(ierror, 'inq_varid tmm10  ', filename)
      ierror = nf90_inq_varid(idfile, 'dhsign', idvar_dhsign); call nc_check_err(ierror, 'inq_varid dhsign ', filename)
      ierror = nf90_inq_varid(idfile, 'drtm01', idvar_drtm01); call nc_check_err(ierror, 'inq_varid drtm01 ', filename)
      ierror = nf90_inq_varid(idfile, 'setup', idvar_setup); call nc_check_err(ierror, 'inq_varid setup  ', filename)
      ierror = nf90_inq_varid(idfile, 'fx', idvar_fx); call nc_check_err(ierror, 'inq_varid fx     ', filename)
      ierror = nf90_inq_varid(idfile, 'fy', idvar_fy); call nc_check_err(ierror, 'inq_varid fy     ', filename)
      ierror = nf90_inq_varid(idfile, 'windu', idvar_windu); call nc_check_err(ierror, 'inq_varid windu  ', filename)
      ierror = nf90_inq_varid(idfile, 'windv', idvar_windv); call nc_check_err(ierror, 'inq_varid windv  ', filename)
      if (output_veg > 0) then
         ierror = nf90_inq_varid(idfile, 'nstems', idvar_nstems); call nc_check_err(ierror, 'inq_varid nstems ', filename)
      end if
      if (output_ice > 0) then
         ierror = nf90_inq_varid(idfile, 'icefrac', idvar_icefrac); call nc_check_err(ierror, 'inq_varid icefrac', filename)
         if (output_ice == 1) then
            ierror = nf90_inq_varid(idfile, 'floedia', idvar_floedia); call nc_check_err(ierror, 'inq_varid floedia', filename)
         end if
      end if
      do i = 1, sof%n_outpars
         ierror = nf90_inq_varid(idfile, sof%add_out_names(i), idvar_outpars(i)); call nc_check_err(ierror, 'inq_varid '//sof%add_out_names(i), filename)
      end do
   end if

   if (prevtime) then
      idummy(1) = real(wavedata%time%calctimtscale_prev, hp) * wavedata%time%tscale
   else
      idummy(1) = real(wavedata%time%calctimtscale, hp) * wavedata%time%tscale
   end if

   ierror = nf90_put_var(idfile, idvar_time, idummy(1), start=(/wavedata%output%count/)); call nc_check_err(ierror, 'put_var time', filename)
   call put_node_var(idvar_hsign, 'hsign', sof%hs)
   if (nautical_convention) then
      allocate (tmp_dir(nnode), stat=ierror)
      tmp_dir = reflect_between_nautical_and_cartesian(sof%dir(1:nnode, 1), north_direction)
      call put_node_vector(idvar_dir, 'dir', tmp_dir)
      tmp_dir = reflect_between_nautical_and_cartesian(sof%pdir(1:nnode, 1), north_direction)
      call put_node_vector(idvar_pdir, 'pdir', tmp_dir)
      deallocate (tmp_dir, stat=ierror)
   else
      call put_node_var(idvar_dir, 'dir', sof%dir)
      call put_node_var(idvar_pdir, 'pdir', sof%pdir)
   end if
   call put_node_var(idvar_period, 'period', sof%period)
   call put_node_var(idvar_rtp, 'rtp', sof%rtp)
   call put_node_var(idvar_depth, 'depth', sof%depth)
   call put_node_var(idvar_velx, 'velx', sof%u)
   call put_node_var(idvar_vely, 'vely', sof%v)
   call put_node_var(idvar_transpx, 'transpx', sof%mx)
   call put_node_var(idvar_transpy, 'transpy', sof%my)
   call put_node_var(idvar_dspr, 'dspr', sof%dspr)
   call put_node_var3(idvar_dissip, 'dissip', sof%dissip)
   call put_node_var(idvar_leak, 'leak', sof%rleak)
   call put_node_var(idvar_qb, 'qb', sof%qb)
   call put_node_var(idvar_ubot, 'ubot', sof%ubot)
   call put_node_var(idvar_steepw, 'steepw', sof%steep)
   call put_node_var(idvar_wlength, 'wlength', sof%wlen)
   call put_node_var(idvar_tps, 'tps', sof%tps)
   call put_node_var(idvar_tm02, 'tm02', sof%tm02)
   call put_node_var(idvar_tmm10, 'tmm10', sof%tmm10)
   call put_node_var(idvar_dhsign, 'dhsign', sof%dhsign)
   call put_node_var(idvar_drtm01, 'drtm01', sof%drtm01)
   call put_node_var(idvar_setup, 'setup', sof%setup)
   call put_node_var(idvar_fx, 'fx', sof%fx)
   call put_node_var(idvar_fy, 'fy', sof%fy)
   call put_node_var(idvar_windu, 'windu', sof%windu)
   call put_node_var(idvar_windv, 'windv', sof%windv)
   if (output_veg > 0) then
      call put_node_var(idvar_nstems, 'nstems', sif_veg)
   end if
   if (output_ice > 0) then
      call put_node_var(idvar_icefrac, 'icefrac', sif%ice_frac)
      if (output_ice == 1) then
         call put_node_var(idvar_floedia, 'floedia', sif%floe_dia)
      end if
   end if
   do i = 1, sof%n_outpars
      call put_node_var3(idvar_outpars(i), sof%add_out_names(i), sof%add_out_vals(:, :, i:i))
   end do
   !
   ierror = nf90_sync(idfile); call nc_check_err(ierror, 'sync file', filename)
   ierror = nf90_close(idfile); call nc_check_err(ierror, 'closing file', filename)

   deallocate (idvar_outpars, stat=ierror)

contains

   integer function define_node_var(varname, standardname, longname, units) result(varid)
      character(*), intent(in) :: varname
      character(*), intent(in) :: standardname
      character(*), intent(in) :: longname
      character(*), intent(in) :: units
      varid = nc_def_var(idfile, varname, precision, 2, (/iddim_node, iddim_time/), standardname, longname, units, .true., filename)
      ierror = nf90_put_att(idfile, varid, 'mesh', 'Mesh2d'); call nc_check_err(ierror, 'put_att '//trim(varname)//' mesh', filename)
      ierror = nf90_put_att(idfile, varid, 'location', 'node'); call nc_check_err(ierror, 'put_att '//trim(varname)//' location', filename)
      ierror = nf90_put_att(idfile, varid, 'coordinates', 'Mesh2d_node_x Mesh2d_node_y'); call nc_check_err(ierror, 'put_att '//trim(varname)//' coordinates', filename)
      ierror = nf90_put_att(idfile, varid, 'grid_mapping', 'projected_coordinate_system'); call nc_check_err(ierror, 'put_att '//trim(varname)//' grid_mapping', filename)
   end function define_node_var

   integer function define_node_int_var(varname, standardname, longname, units) result(varid)
      character(*), intent(in) :: varname
      character(*), intent(in) :: standardname
      character(*), intent(in) :: longname
      character(*), intent(in) :: units
      varid = nc_def_var(idfile, varname, nf90_int, 1, (/iddim_node/), standardname, longname, units, .true., filename)
      ierror = nf90_put_att(idfile, varid, 'mesh', 'Mesh2d'); call nc_check_err(ierror, 'put_att '//trim(varname)//' mesh', filename)
      ierror = nf90_put_att(idfile, varid, 'location', 'node'); call nc_check_err(ierror, 'put_att '//trim(varname)//' location', filename)
      ierror = nf90_put_att(idfile, varid, 'coordinates', 'Mesh2d_node_x Mesh2d_node_y'); call nc_check_err(ierror, 'put_att '//trim(varname)//' coordinates', filename)
      ierror = nf90_put_att(idfile, varid, 'grid_mapping', 'projected_coordinate_system'); call nc_check_err(ierror, 'put_att '//trim(varname)//' grid_mapping', filename)
   end function define_node_int_var

   subroutine put_node_var(varid, varname, values)
      integer, intent(in) :: varid
      character(*), intent(in) :: varname
      real, dimension(:, :), intent(in) :: values
      ierror = nf90_put_var(idfile, varid, values(1:nnode, 1), &
                            start=(/1, wavedata%output%count/), count=(/nnode, 1/))
      call nc_check_err(ierror, 'put_var '//trim(varname), filename)
   end subroutine put_node_var

   subroutine put_node_var3(varid, varname, values)
      integer, intent(in) :: varid
      character(*), intent(in) :: varname
      real, dimension(:, :, :), intent(in) :: values
      ierror = nf90_put_var(idfile, varid, values(1:nnode, 1, 1), &
                            start=(/1, wavedata%output%count/), count=(/nnode, 1/))
      call nc_check_err(ierror, 'put_var '//trim(varname), filename)
   end subroutine put_node_var3

   subroutine put_node_vector(varid, varname, values)
      integer, intent(in) :: varid
      character(*), intent(in) :: varname
      real, dimension(:), intent(in) :: values
      ierror = nf90_put_var(idfile, varid, values(1:nnode), &
                            start=(/1, wavedata%output%count/), count=(/nnode, 1/))
      call nc_check_err(ierror, 'put_var '//trim(varname), filename)
   end subroutine put_node_vector

   subroutine put_node_int_var(varid, varname, values)
      integer, intent(in) :: varid
      character(*), intent(in) :: varname
      integer, dimension(:, :), intent(in) :: values
      ierror = nf90_put_var(idfile, varid, values(1:nnode, 1), start=(/1/), count=(/nnode/))
      call nc_check_err(ierror, 'put_var '//trim(varname), filename)
   end subroutine put_node_int_var

end subroutine write_wave_map_netcdf_unstructured
