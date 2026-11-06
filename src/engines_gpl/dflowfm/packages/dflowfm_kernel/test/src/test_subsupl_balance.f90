!> End-to-end water, transported-mass and NetCDF inventory checks for ground movement.
program test_subsupl_balance
   use precision, only: dp
   use unstruc_api, only: init_core, api_loadmodel, flowinit, flowstep, flowfinalize
   use m_flowtimes, only: time_user, tstop_user
   use m_flowgeom, only: ndxi, ba, bl
   use m_flow, only: s1, vol1
   use m_partitioninfo, only: jampi, numranks, my_rank
   use unstruc_display, only: jagui
   use m_mass_balance_area_data
   use mass_balance_areas_routines, only: mba_final
   use m_get_kbot_ktop, only: getkbotktop
   use m_fm_erosed, only: iflufflyr
   use dfm_error, only: DFM_NOERR
   use netcdf

   implicit none(type, external)

   real(dp), parameter :: tolerance = 1.0e-9_dp
   character(len=32) :: argument, morphology
   integer :: dimensions, update_s1, direction, status, stop_requested, step, imbs, ised, k, kb, kt, ncid
   real(dp) :: actual_volume, expected_volume, bed_volume_change
   real(dp), allocatable :: initial_mass(:)

   call get_command_argument(1, argument)
   if (argument == 'volume_remap') then
      call check_volume_remap()
      stop
   end if
   if (argument == 'sigma_boundary') then
      call check_sigma_boundary()
      stop
   end if
   read (argument, *) dimensions
   call get_command_argument(2, morphology)
   call get_command_argument(3, argument)
   read (argument, *) update_s1
   call get_command_argument(4, argument)
   read (argument, *) direction
   call require(dimensions == 2 .or. dimensions == 3, 'Expected 2D or 3D')
   call require(update_s1 == 0 .or. update_s1 == 1, 'Expected SubsUplUpdateS1=0 or 1')
   call require(abs(direction) == 1, 'Expected subsidence (-1) or uplift (1)')
   call require(any(morphology == [character(len=16) :: 'off', 'bedload', 'suspended', 'composition']), 'Unknown morphology configuration')

   jampi = 0
   numranks = 1
   my_rank = 0
   jagui = 0
   call init_core()
   call create_input()
   call api_loadmodel('box.mdu')
   status = flowinit()
   call require(status == DFM_NOERR, 'Model initialization failed')
   initial_mass = total_mass()

   do step = 1, 64
      if (time_user >= tstop_user) then
         exit
      end if
      call flowstep(stop_requested, status)
      call require(status == DFM_NOERR .and. stop_requested == 0, 'Model timestep failed')
      call check_balances()
      call require(maxval(abs(total_mass() - initial_mass)) < tolerance, 'Physical constituent mass changed')

      actual_volume = 0.0_dp
      do k = 1, ndxi
         call getkbotktop(k, kb, kt)
         actual_volume = actual_volume + sum(vol1(kb:kt))
      end do
      call require(abs(actual_volume - sum(ba(1:ndxi) * (s1(1:ndxi) - bl(1:ndxi)))) < tolerance, &
                   'Layer volumes disagree with bed elevation and water depth')
      call require(abs(actual_volume - mbavolumeend(1)) < tolerance, 'The water balance uses incorrect volumes')
   end do
   call require(abs(time_user - 6.0_dp) < tolerance, 'The model did not finish')
   if (morphology == 'suspended' .or. morphology == 'composition') then
      call require(abs(sum(mbabedmassend - mbabedmassbegintot)) > 1.0e-4_dp, &
                   'The regression must exercise sediment exchange with the bed')
   end if
   call require(maxval(abs(s1(1:ndxi) - (0.12_dp + direction * 0.006_dp * update_s1))) < tolerance, &
                'SubsUplUpdateS1 did not apply the prescribed water-level movement')
   bed_volume_change = 0.0_dp
   if (morphology /= 'off' .and. morphology /= 'composition') then
      bed_volume_change = sum(mbabedmassend - mbabedmassbegintot) / 1600.0_dp
   end if
   expected_volume = 0.36_dp - direction * 0.018_dp * (1 - update_s1) - bed_volume_change
   call require(abs(actual_volume - expected_volume) < tolerance, 'Incorrect water displacement')
   call mba_final(time_user)
   call check_balances()
   call flowfinalize()

   call check_nc(nf90_open('output/box_mass_balances.nc', NF90_NOWRITE, ncid))
   do imbs = 1, nombs
      ised = imbs2sed(imbs)
      if (ised <= 0) then
         cycle
      end if
      write (argument, '("const",i0)') imbs
      call check_inventory(trim(argument)//'_bed_mass', mbabedmassbegintot(ised, 1), mbabedmassend(ised, 1))
      call check_inventory(trim(argument)//'_bedshort_mass', mbabedshortmassbegintot(ised, 1), mbabedshortmassend(ised, 1))
      if (iflufflyr > 0) then
         call check_inventory(trim(argument)//'_fluff_mass', mbafluffmassbegintot(ised, 1), mbafluffmassend(ised, 1))
      end if
   end do
   call check_nc(nf90_close(ncid))

contains

   !> Boundary sigma volumes must follow subsidence and uplift with either water-level setting.
   subroutine check_sigma_boundary()
      use m_flowgeom, only: ndx
      use m_flow, only: kmx, layertype, LAYTP_SIGMA, jased, nbndz, kbndz, kbot, ktop, kmxn, zslay, zws, s0
      use m_subsidence, only: jasubsupl
      use m_set_kbot_ktop, only: update_vertical_coordinates_boundary

      integer :: sediment, setting, sign
      real(dp) :: displacement, depth

      ndx = 1
      kmx = 2
      layertype = LAYTP_SIGMA
      jasubsupl = 1
      nbndz = 1
      allocate (kbndz(1, 1), zslay(2, 1))
      kbndz = 1
      zslay(:, 1) = [0.5_dp, 1.0_dp]
      kbot = [2]
      ktop = [3]
      kmxn = [2]
      ba = [2.0_dp]
      do sediment = 0, 1
         jased = sediment
         do setting = 0, 1
            do sign = -1, 1, 2
               displacement = sign * 0.006_dp
               bl = [displacement]
               s0 = [0.12_dp + setting * displacement]
               zws = [0.0_dp, 0.06_dp, 0.12_dp]
               vol1 = [0.24_dp, 0.12_dp, 0.12_dp]
               depth = s0(1) - bl(1)
               call update_vertical_coordinates_boundary()
               call require(abs(zws(1) - bl(1)) < tolerance, 'Boundary bottom interface did not follow the ground')
               call require(abs(vol1(1) - 2.0_dp * depth) < tolerance, 'Incorrect boundary-column volume')
               call require(maxval(abs(vol1(2:3) - depth)) < tolerance, 'Incorrect boundary sigma-layer volumes')
            end do
         end do
      end do
   end subroutine check_sigma_boundary

   !> Preserve layer mass during unequal volume changes, layer collapse and rewetting.
   subroutine check_volume_remap()
      use m_flow, only: kmx, kbot, ktop, ktop0
      use m_subsidence, only: sdu_volp
      use m_subsupl_update_s1, only: subsupl_update_constituents
      use m_transport, only: numconst, constituents

      ndxi = 2
      kmx = 2
      numconst = 2
      kbot = [3, 5]
      ktop = [4, 6]
      ktop0 = ktop
      vol1 = [0.0_dp, 0.0_dp, 2.0_dp, 2.0_dp, 0.0_dp, 4.0_dp]
      sdu_volp = [0.0_dp, 0.0_dp, 1.0_dp, 3.0_dp, 2.0_dp, 2.0_dp]
      allocate (constituents(2, 6))
      constituents = 0.0_dp
      constituents(:, 3) = [2.0_dp, 5.0_dp]
      constituents(:, 4) = [6.0_dp, 3.0_dp]
      constituents(:, 5) = [4.0_dp, 2.0_dp]
      constituents(:, 6) = [1.0_dp, 7.0_dp]
      call subsupl_update_constituents()
      call require(maxval(abs(constituents(:, 3) - [1.0_dp, 2.5_dp])) < tolerance, 'Incorrect expanding-layer concentrations')
      call require(maxval(abs(constituents(:, 4) - [9.0_dp, 4.5_dp])) < tolerance, 'Incorrect contracting-layer concentrations')
      call require(maxval(abs(constituents(:, 5))) < tolerance, 'A collapsed layer retained concentration')
      call require(maxval(abs(constituents(:, 6) - [2.5_dp, 4.5_dp])) < tolerance, 'Collapsed-layer mass was lost')

      sdu_volp = vol1
      vol1(5:6) = [1.0_dp, 3.0_dp]
      constituents(:, 5) = 42.0_dp
      constituents(:, 6) = [2.0_dp, 3.0_dp]
      call subsupl_update_constituents()
      call require(maxval(abs(constituents(:, 5))) < tolerance, 'A newly wet layer created mass')
      call require(maxval(abs(constituents(:, 6) - [8.0_dp / 3.0_dp, 4.0_dp])) < tolerance, 'Rewetting changed column mass')
   end subroutine check_volume_remap

   !> Include suspension, bed stratigraphy and pending shortages in each constituent inventory.
   function total_mass() result(mass)
      real(dp) :: mass(nombs)
      integer :: i, fraction

      mass = mbamassend(:, 1)
      do i = 1, nombs
         fraction = imbs2sed(i)
         if (fraction > 0) then
            mass(i) = mass(i) + mbabedmassend(fraction, 1) + mbabedshortmassend(fraction, 1)
            if (iflufflyr > 0) then
               mass(i) = mass(i) + mbafluffmassend(fraction, 1)
            end if
         end if
      end do
   end function total_mass

   !> Check both the individual area and whole-model budgets, including interval resets.
   subroutine check_balances()
      integer :: i

      call require(maxval(abs(water_flow%bal_error)) < tolerance, 'Water balance did not close')
      do i = 1, nombs
         call require(maxval(abs(const_flux(i)%bal_error)) < tolerance, 'Mass balance did not close for '//trim(mbsname(i)))
      end do
   end subroutine check_balances

   !> Compare exported sediment inventories against the correct fraction at both ends and areas.
   subroutine check_inventory(name, first, last)
      character(len=*), intent(in) :: name
      real(dp), intent(in) :: first, last
      integer :: varid, dimids(2), records, area
      real(dp) :: value(1, 1)

      call check_nc(nf90_inq_varid(ncid, name, varid))
      call check_nc(nf90_inquire_variable(ncid, varid, dimids=dimids))
      call check_nc(nf90_inquire_dimension(ncid, dimids(2), len=records))
      do area = 1, 2
         call check_nc(nf90_get_var(ncid, varid, value, start=[area, 1], count=[1, 1]))
         call require(abs(value(1, 1) - first) < tolerance, name//': incorrect initial inventory')
         call check_nc(nf90_get_var(ncid, varid, value, start=[area, records], count=[1, 1]))
         call require(abs(value(1, 1) - last) < tolerance, name//': incorrect final inventory')
      end do
   end subroutine check_inventory

   !> Build a closed three-cell domain with a tracer, salinity and optional sediment fractions.
   subroutine create_input()
      use m_file_helpers, only: create_file

      character(len=32) :: layers, end_elevation, sediment_type, transport_formula, bed_update

      call create_network()
      write (layers, '("Kmx = ",i0)') 2 * (dimensions - 2)
      write (end_elevation, '(g0)') direction * 0.006_dp
      sediment_type = 'SedTyp = bedload'
      transport_formula = 'TraFrm = 1'
      bed_update = 'BedUpd = true'
      if (morphology == 'composition') then
         bed_update = 'BedUpd = false'
      end if
      if (morphology == 'suspended' .or. morphology == 'composition') then
         sediment_type = 'SedTyp = sand'
         transport_formula = 'TraFrm = -1'
      end if
      call create_file('box.mdu', [character(len=96) :: &
                       '[model]', 'Program = D-Flow FM', 'FileVersion = 1.09', &
                       '[geometry]', 'NetFile = box_net.nc', 'Bedlevuni = 0', 'BedlevType = 1', 'Conveyance2D = 0', 'Nonlin2D = 0', &
                       'WaterLevIni = 0.12', 'IniFieldFile = initial.ini', layers, &
                       '[physics]', 'Salinity = 1', 'InitialSalinity = 1', &
                       '[numerics]', 'SubsUplUpdateS1 = '//achar(iachar('0') + update_s1), &
                       '[time]', 'RefDate = 20000101', 'Tunit = S', 'TStart = 0', 'TStop = 6', &
                       'DtUser = 0.3', 'DtMax = 0.3', 'DtInit = 0.3', &
                       '[external forcing]', 'ExtForceFileNew = ground.ext', &
                       '[output]', 'OutputDir = output', 'MapInterval = 0', 'HisInterval = 0', 'RstInterval = 0', &
                       'MbaFile = box_mba.ini', 'MbaInterval = 0.3', 'MbaWriteTxt = 1', 'MbaWriteNetCDF = 1'])
      if (morphology /= 'off') then
         call append_file('box.mdu', [character(len=96) :: &
                          '[sediment]', 'SedimentModelNr = 4', 'SedFile = box.sed', 'MorFile = box.mor'])
      end if
      call create_file('initial.ini', [character(len=96) :: &
                       '[General]', 'fileVersion = 2.00', 'fileType = iniField', &
                       '[Initial]', 'quantity = initialtracerDye', 'dataValue = 2'])
      if (morphology == 'suspended' .or. morphology == 'composition') then
         call append_file('initial.ini', [character(len=96) :: '[Initial]', 'quantity = initialsedfracfine', 'dataValue = 2'])
      end if
      call create_file('ground.ext', [character(len=96) :: &
                       '[General]', 'fileVersion = 2.01', 'fileType = extForce', &
                       '[Spatial]', 'quantity = bedrock_surface_elevation', 'forcingFile = ground.bc', 'forcingFileType = bcascii'])
      call create_file('ground.bc', [character(len=96) :: &
                       '[General]', 'fileVersion = 1.01', 'fileType = boundConds', &
                       '[Forcing]', 'name = global', 'function = timeSeries', 'timeInterpolation = linear', &
                       'quantity = time', 'unit = seconds since 2000-01-01 00:00:00', &
                       'quantity = bedrock_surface_elevation', 'unit = m', '0 0', '6 '//trim(end_elevation)])
      call create_file('box_mba.ini', [character(len=96) :: '[General]', 'fileVersion = 1.00', 'fileType = massBalanceAreas'])
      call create_file('box.sed', [character(len=96) :: &
                       '[SedimentFileInformation]', 'FileVersion = 02.00', '[SedimentOverall]', 'Cref = 1e10', &
                       '[Sediment]', 'Name = fine', sediment_type, transport_formula, 'RhoSol = 2600', 'SedDia = 0.0001', &
                       'CDryB = 1600', 'IniSedThick = 0.02', 'ACAL = 1.21', 'FacDSS = 1', 'SettleFrm = function_of_conditions', &
                       'SalMax = 0', 'WS0 = 0.0086', 'WSM = 0.0086', 'TCoefWS = 1', 'CFlocCr = 0.1', 'CPowWS = 0', &
                       '[Sediment]', 'Name = coarse', 'SedTyp = bedload', 'TraFrm = 1', 'RhoSol = 2650', &
                       'SedDia = 0.011', 'CDryB = 1600', 'IniSedThick = 0.01', 'ACAL = 1.21'])
      call create_file('box.mor', [character(len=96) :: &
                       '[MorphologyFileInformation]', 'FileVersion = 02.00', '[Morphology]', &
                       'MorFac = 1', 'MorStt = 0', bed_update, 'CmpUpd = true', 'Bed = 0', &
                       'SedThr = 0.005', 'Thresh = 0.001', 'DensIn = false', &
                       '[Underlayer]', 'IUnderLyr = 2', 'ExchLyr = false', 'TTLForm = 2', &
                       'TTLAlpha = 0.1', 'TTLMin = 0.01', 'MxNULyr = 2', 'ThUnLyr = 0.005'])
   end subroutine create_input

   !> Generate a minimal network independently of model initialization and output caches.
   subroutine create_network()
      integer :: fileid, node_dim, link_dim, pair_dim, x_var, y_var, z_var, link_var, type_var
      integer, parameter :: links(2, 10) = reshape([1, 2, 2, 3, 3, 4, 5, 6, 6, 7, 7, 8, 1, 5, 2, 6, 3, 7, 4, 8], [2, 10])

      call check_nc(nf90_create('box_net.nc', NF90_CLOBBER, fileid))
      call check_nc(nf90_def_dim(fileid, 'nNetNode', 8, node_dim))
      call check_nc(nf90_def_dim(fileid, 'nNetLink', 10, link_dim))
      call check_nc(nf90_def_dim(fileid, 'nNetLinkPts', 2, pair_dim))
      call check_nc(nf90_def_var(fileid, 'NetNode_x', NF90_DOUBLE, [node_dim], x_var))
      call check_nc(nf90_def_var(fileid, 'NetNode_y', NF90_DOUBLE, [node_dim], y_var))
      call check_nc(nf90_def_var(fileid, 'NetNode_z', NF90_DOUBLE, [node_dim], z_var))
      call check_nc(nf90_def_var(fileid, 'NetLink', NF90_INT, [pair_dim, link_dim], link_var))
      call check_nc(nf90_def_var(fileid, 'NetLinkType', NF90_INT, [link_dim], type_var))
      call check_nc(nf90_enddef(fileid))
      call check_nc(nf90_put_var(fileid, x_var, [0.0_dp, 1.0_dp, 2.0_dp, 3.0_dp, 0.0_dp, 1.0_dp, 2.0_dp, 3.0_dp]))
      call check_nc(nf90_put_var(fileid, y_var, [0.0_dp, 0.0_dp, 0.0_dp, 0.0_dp, 1.0_dp, 1.0_dp, 1.0_dp, 1.0_dp]))
      call check_nc(nf90_put_var(fileid, z_var, [0.0_dp, 0.0_dp, 0.0_dp, 0.0_dp, 0.0_dp, 0.0_dp, 0.0_dp, 0.0_dp]))
      call check_nc(nf90_put_var(fileid, link_var, links))
      call check_nc(nf90_put_var(fileid, type_var, [2, 2, 2, 2, 2, 2, 2, 2, 2, 2]))
      call check_nc(nf90_close(fileid))
   end subroutine create_network

   !> Append optional model sections without duplicating fixture generation.
   subroutine append_file(name, lines)
      character(len=*), intent(in) :: name, lines(:)
      integer :: unit, i

      open (newunit=unit, file=name, status='old', position='append', action='write')
      do i = 1, size(lines)
         write (unit, '(a)') trim(lines(i))
      end do
      close (unit)
   end subroutine append_file

   subroutine require(condition, description)
      logical, intent(in) :: condition
      character(len=*), intent(in) :: description

      if (.not. condition) then
         error stop description
      end if
   end subroutine require

   subroutine check_nc(code)
      integer, intent(in) :: code

      if (code /= NF90_NOERR) then
         error stop nf90_strerror(code)
      end if
   end subroutine check_nc
end program test_subsupl_balance
