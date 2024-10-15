subroutine write_wave_map_netcdf(sg, sof, sif, n_swan_grids, wavedata, casl, prevtime, singleprecision, sif_mmax, sif_nmax, sif_veg, output_ice, output_veg)
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
!!--description-----------------------------------------------------------------
! NONE
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
   use wave_data
   use swan_flow_grid_maps
   use netcdf
   use precision_basics
   use dwaves_version_module
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
   integer, intent(in) :: output_ice ! switch for writing ice quantities
   integer, intent(in) :: output_veg ! switch for writing vegetation quantities
   integer, intent(in) :: sif_mmax
   integer, intent(in) :: sif_nmax
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
   integer, dimension(1) :: idummy ! Help array to read/write Nefis files
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
      idvar_dir = nc_def_var(idfile, 'dir', precision, 3, (/iddim_mmax, iddim_nmax, iddim_time/), '', 'Mean wave direction', 'deg', .true., filename)
      idvar_pdir = nc_def_var(idfile, 'pdir', precision, 3, (/iddim_mmax, iddim_nmax, iddim_time/), '', 'Peak wave direction', 'deg', .true., filename)
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
      idummy(1) = wavedata%time%calctimtscale_prev * wavedata%time%tscale
   else
      idummy(1) = wavedata%time%calctimtscale * wavedata%time%tscale
   end if

   ierror = nf90_put_var(idfile, idvar_time, idummy(1), start=(/wavedata%output%count/)); call nc_check_err(ierror, "put_var time", filename)
   ierror = nf90_put_var(idfile, idvar_hsign, sof%hs, start=(/1, 1, wavedata%output%count/), count=(/sof%mmax, sof%nmax, 1/)); call nc_check_err(ierror, "put_var hsign", filename)
   ierror = nf90_put_var(idfile, idvar_dir, sof%dir, start=(/1, 1, wavedata%output%count/), count=(/sof%mmax, sof%nmax, 1/)); call nc_check_err(ierror, "put_var dir    ", filename)
   ierror = nf90_put_var(idfile, idvar_pdir, sof%pdir, start=(/1, 1, wavedata%output%count/), count=(/sof%mmax, sof%nmax, 1/)); call nc_check_err(ierror, "put_var pdir   ", filename)
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
