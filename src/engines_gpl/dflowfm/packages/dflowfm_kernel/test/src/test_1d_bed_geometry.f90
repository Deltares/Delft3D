!> Exercise link-end geometry and conservation through 1D timesteps and restarts.
program test_1d_bed_geometry
   use precision, only: dp
   use unstruc_api, only: init_core, api_loadmodel, flowinit, flowstep, flowfinalize
   use unstruc_netcdf, only: unc_create, unc_close, unc_write_rst_filepointer
   use unstruc_channel_flow, only: network
   use unstruc_display, only: jagui
   use m_flowgeom, only: bl, bob, bob0, ndxi, lnx1d
   use m_flow, only: vol1
   use m_flowtimes, only: time_user, tstop_user
   use m_partitioninfo, only: jampi, numranks, my_rank
   use m_mass_balance_area_data, only: mbabedmassend, mbabedshortmassend, mbamassend, mbavolumeend, &
                                      water_flow, const_flux, nombs, imbs2sed, mbsname
   use m_subsidence, only: jasubsupl
   use m_cross_helper, only: getbobs
   use m_file_helpers, only: create_file
   use dfm_error, only: DFM_NOERR
   use netcdf

   implicit none(type, external)

   integer, parameter :: nodes = 5, links = 4
   real(dp), parameter :: geometry_tol = 1.0e-10_dp, forcing_tol = 1.0e-8_dp, balance_tol = 1.0e-8_dp
   real(dp), parameter :: initial_bed(nodes) = [-1.0_dp, -0.75_dp, -0.5_dp, -0.25_dp, 0.0_dp]
   character(len=*), parameter :: restart_path = 'seed_rst.nc', profile_name = 'mesh1d_mor_crs_z'
   character(len=32) :: mode, scenario, argument
   integer :: direction, update_s1, status, link, unit
   real(dp) :: saved_bed(nodes), saved_bob(2, links), saved_bob0(2, links), saved_volume
   real(dp), allocatable :: saved_mass(:)

   call get_command_argument(1, mode)
   call get_command_argument(2, scenario)
   direction = 0
   update_s1 = 1
   if (mode == 'ground') then
      call get_command_argument(3, argument)
      read (argument, *) update_s1
      call get_command_argument(4, argument)
      read (argument, *) direction
   else if (scenario == 'displaced') then
      direction = 1
   end if

   call create_model()
   if (mode == 'restart' .and. index(scenario, 'missing') == 1) then
      call remove_restart_profiles()
   end if
   jampi = 0
   numranks = 1
   my_rank = 0
   jagui = 0
   call init_core()
   call api_loadmodel('reach.mdu')
   status = flowinit()
   call require(status == DFM_NOERR, 'Model initialization failed')
   call require(scenario /= 'missing_changed', 'Accepted a changed bed without the saved profiles')
   call require(ndxi == nodes .and. lnx1d == links, 'Unexpected reach dimensions')
   allocate (saved_mass(nombs))

   select case (mode)
   case ('ground', 'seed')
      call require(maxval(abs(bl(1:nodes) - initial_bed)) < geometry_tol, 'Incorrect initial nodal bed levels')
      do link = 1, links
         call require(maxval(abs(bob(:, link) - initial_bed(link:link + 1))) < geometry_tol, &
                      'Incorrect initial section levels at the link ends')
      end do
      call check_steps()
      if (mode == 'seed') then
         if (scenario == 'small_change') then
            call require(maxval(abs(bl(1:nodes) - initial_bed)) > geometry_tol .and. &
                         maxval(abs(bl(1:nodes) - initial_bed)) < 2.0e-7_dp, &
                         'The seed must change the bed by less than the former restart tolerance')
         else if (scenario /= 'fixed') then
            call require(maxval(abs(bl(1:nodes) - initial_bed - displacement())) > 1.0e-6_dp, &
                         'The moving-bed seed did not change the cross sections')
         end if
         open (newunit=unit, file='seed_geometry.txt', status='replace', action='write')
         write (unit, *) bl(1:nodes), bob(:, 1:links), bob0(:, 1:links), sum(vol1(1:nodes)), total_mass()
         close (unit)
         call write_restart(restart_path)
      end if
   case ('restart')
      open (newunit=unit, file='seed_geometry.txt', status='old', action='read')
      read (unit, *) saved_bed, saved_bob, saved_bob0, saved_volume, saved_mass
      close (unit)
      call require(maxval(abs(bl(1:nodes) - saved_bed)) < geometry_tol, 'Nodal bed changed at restart')
      call require(maxval(abs(bob(:, 1:links) - saved_bob)) < geometry_tol, 'Link-end levels changed at restart')
      call require(maxval(abs(bob0(:, 1:links) - saved_bob0)) < geometry_tol, 'Initial link-end levels changed at restart')
      call require(abs(sum(vol1(1:nodes)) - saved_volume) < balance_tol, 'Water volume changed at restart')
      call require(maxval(abs(total_mass() - saved_mass)) < balance_tol, 'Constituent mass changed at restart')
      call check_steps()
      if (scenario /= 'missing_fixed') then
         call check_retained_profiles()
      end if
   case default
      error stop 'Unknown test mode'
   end select
   call flowfinalize()

contains

   real(dp) function displacement()
      displacement = direction * 0.001_dp * time_user
   end function displacement

   function total_mass() result(mass)
      real(dp) :: mass(nombs)
      integer :: quantity, fraction

      mass = mbamassend(:, 1)
      do quantity = 1, nombs
         fraction = imbs2sed(quantity)
         if (fraction > 0) then
            mass(quantity) = mass(quantity) + mbabedmassend(fraction, 1) + mbabedshortmassend(fraction, 1)
         end if
      end do
   end function total_mass

   subroutine check_steps()
      real(dp) :: expected_bob(2), initial_mass(nombs), link_tol
      integer :: i, step, stop_requested

      initial_mass = total_mass()
      ! The forcing reader introduces rounding when interpolating the specified levels.
      link_tol = geometry_tol
      if (direction /= 0) then
         link_tol = forcing_tol
      end if
      call require(any(mbsname(1:nombs) == 'Dye'), 'Expected a conservative tracer in the mass balance')
      if (scenario /= 'off') then
         call require(any(imbs2sed(1:nombs) > 0), 'Expected sediment in the mass balance')
      end if
      if (mode == 'ground' .or. scenario == 'displaced') then
         call require(jasubsupl > 0, 'Ground displacement forcing was not enabled')
      end if

      do step = 1, 30
         if (time_user >= tstop_user) then
            exit
         end if
         call flowstep(stop_requested, status)
         call require(status == DFM_NOERR .and. stop_requested == 0, 'Time step failed')
         do i = 1, links
            expected_bob = getbobs(network, i) + displacement()
            call require(maxval(abs(bob(:, i) - expected_bob)) < link_tol, 'Incorrect bed level at a link end')
            call require(maxval(abs(bob0(:, i) - expected_bob)) < link_tol, 'Incorrect reference level at a link end')
         end do
         if (scenario == 'off' .or. scenario == 'fixed') then
            call require(maxval(abs(bl(1:nodes) - initial_bed - displacement())) < link_tol, &
                         'Nodal bed did not follow the prescribed ground displacement')
         end if
         call require(abs(sum(vol1(1:nodes)) - mbavolumeend(1)) < balance_tol, 'Water balance uses a different volume')
         call require(maxval(abs(water_flow%bal_error)) < balance_tol, 'Water balance did not close')
         do i = 1, nombs
            call require(maxval(abs(const_flux(i)%bal_error)) < balance_tol, 'Mass balance did not close for '//trim(mbsname(i)))
         end do
         call require(maxval(abs(total_mass() - initial_mass)) < balance_tol, 'Total constituent mass changed')
      end do
      call require(abs(time_user - tstop_user) < geometry_tol, 'The model did not finish')
   end subroutine check_steps

   subroutine create_model()
      character(len=32) :: start_time, end_time, s1_setting, end_displacement, bed_update, morfac

      start_time = 'TStart = 0'
      end_time = 'TStop = 10'
      if (mode == 'restart') then
         start_time = 'TStart = 10'
         end_time = 'TStop = 20'
      end if
      write (s1_setting, '("SubsUplUpdateS1 = ",i0)') update_s1
      write (end_displacement, '(a,g0)') '20 ', direction * 0.02_dp
      bed_update = 'BedUpd = false'
      morfac = 'MorFac = 1'
      if (scenario == 'moving' .or. (mode == 'seed' .and. scenario /= 'fixed')) then
         bed_update = 'BedUpd = true'
      end if
      if (scenario == 'small_change') then
         morfac = 'MorFac = 0.01'
      end if
      call create_file('reach.mdu', [character(len=96) :: &
                       '[model]', 'Program = D-Flow FM', 'FileVersion = 1.09', &
                       '[geometry]', 'NetFile = reach_net.nc', 'CrossLocFile = cross_locations.ini', &
                       'CrossDefFile = cross_definitions.ini', 'frictFile = roughness.ini', &
                       'BedlevType = 1', 'Conveyance2D = 0', 'WaterDepthIni1D = 1.5', 'NonLin1D = 1', 'IniFieldFile = initial.ini', &
                       '[numerics]', s1_setting, '[time]', 'RefDate = 20000101', 'Tunit = S', &
                       start_time, end_time, 'DtUser = 1', 'DtInit = 1', 'DtMax = 1', &
                       '[output]', 'OutputDir = output', 'MapInterval = 0', 'HisInterval = 0', 'RstInterval = 0', &
                       'MbaFile = reach_mba.ini', 'MbaInterval = 1', 'MbaWriteNetCDF = 1'])
      if (mode == 'restart') then
         call append_mdu([character(len=96) :: '[restart]', 'RestartFile = '//restart_path])
      end if
      if (mode == 'ground' .or. scenario == 'displaced') then
         call append_mdu([character(len=96) :: '[external forcing]', 'ExtForceFileNew = ground.ext'])
         call create_file('ground.ext', [character(len=96) :: &
                          '[General]', 'fileVersion = 2.01', 'fileType = extForce', &
                          '[Spatial]', 'quantity = bedrock_surface_elevation', 'forcingFile = ground.bc', 'forcingFileType = bcascii'])
         call create_file('ground.bc', [character(len=96) :: &
                          '[General]', 'fileVersion = 1.01', 'fileType = boundConds', &
                          '[Forcing]', 'name = global', 'function = timeSeries', 'timeInterpolation = linear', &
                          'quantity = time', 'unit = seconds since 2000-01-01 00:00:00', &
                          'quantity = bedrock_surface_elevation', 'unit = m', '0 0', end_displacement])
      end if
      if (scenario /= 'off') then
         call append_mdu([character(len=96) :: '[sediment]', 'SedimentModelNr = 4', &
                          'SedFile = reach.sed', 'MorFile = reach.mor'])
         call create_file('reach.mor', [character(len=96) :: &
                          '[MorphologyFileInformation]', 'FileVersion = 02.00', '[Morphology]', &
                          morfac, 'MorStt = 0', bed_update, 'CmpUpd = true', 'Bed = 1', 'DensIn = false', &
                          '[Underlayer]', 'IUnderLyr = 2', 'ExchLyr = false', 'TTLForm = 2', &
                          'TTLAlpha = 0.1', 'TTLMin = 0.01', 'MxNULyr = 2', 'ThUnLyr = 0.005'])
      end if
   end subroutine create_model

   subroutine append_mdu(lines)
      character(len=*), intent(in) :: lines(:)
      integer :: file_unit, i

      open (newunit=file_unit, file='reach.mdu', status='old', position='append', action='write')
      do i = 1, size(lines)
         write (file_unit, '(a)') trim(lines(i))
      end do
      close (file_unit)
   end subroutine append_mdu

   subroutine remove_restart_profiles()
      integer :: ncid, varid, nc_status

      call check_nc(nf90_open(restart_path, NF90_WRITE, ncid))
      nc_status = nf90_inq_varid(ncid, profile_name, varid)
      if (nc_status /= nf90_enotvar) then
         call check_nc(nc_status)
         call check_nc(nf90_redef(ncid))
         call check_nc(nf90_rename_var(ncid, varid, 'omitted_profile_levels'))
         call check_nc(nf90_enddef(ncid))
      end if
      call check_nc(nf90_close(ncid))
   end subroutine remove_restart_profiles

   subroutine write_restart(filename)
      character(len=*), intent(in) :: filename
      integer :: ncid

      call check_nc(unc_create(filename, 0, ncid))
      call unc_write_rst_filepointer(ncid, time_user)
      call check_nc(unc_close(ncid))
   end subroutine write_restart

   subroutine check_retained_profiles()
      integer :: ncid, varid

      call write_restart('frozen_rst.nc')
      call check_nc(nf90_open('frozen_rst.nc', NF90_NOWRITE, ncid))
      call check_nc(nf90_inq_varid(ncid, profile_name, varid))
      call check_nc(nf90_close(ncid))
   end subroutine check_retained_profiles

   subroutine check_nc(nc_status)
      integer, intent(in) :: nc_status

      call require(nc_status == nf90_noerr, nf90_strerror(nc_status))
   end subroutine check_nc

   subroutine require(condition, message)
      logical, intent(in) :: condition
      character(len=*), intent(in) :: message

      if (.not. condition) then
         write (*, '(a)') message
         error stop 1
      end if
   end subroutine require
end program test_1d_bed_geometry
