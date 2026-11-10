!> Exercise bedload balance accounting through the complete production timestep sequence.
module test_bedload_balance
   use assertions_gtest
   use precision, only: dp
   use m_file_helpers, only: create_file

   implicit none(type, external)
   private
   public :: test_bedload_order

contains

   !$gtest
   !> Corrected boundary bedload and sediment layer mass must balance at every output time.
   subroutine test_bedload_order() bind(C)
      use unstruc_api, only: init_core, api_loadmodel, flowinit, flowstep, flowfinalize
      use m_flowtimes, only: time_user, tstop_user
      use m_mass_balance_area_data, only: const_flux, mbabedmassend, mbasedfluxtot
      use mass_balance_areas_routines, only: mba_final
      use dfm_error, only: DFM_NOERR
      use m_partitioninfo, only: jampi, numranks, my_rank
      use unstruc_display, only: jagui

      real(dp) :: initial_mass(2)
      integer :: status, stop_requested, step

      ! Match the serial command-line driver without initializing MPI or a display.
      jampi = 0
      numranks = 1
      my_rank = 0
      jagui = 0
      call create_bedload_input()
      call init_core()
      call api_loadmodel('bedload.mdu')
      status = flowinit()
      call f90_expect_eq(status, DFM_NOERR, 'The three-cell bedload model must initialize')
      if (status /= DFM_NOERR) then
         return
      end if
      initial_mass = mbabedmassend(:, 1)

      ! Include fixed-composition spin-up, activation at 1.2 s and changing boundary fluxes.
      do step = 1, 32
         if (time_user >= tstop_user) then
            exit
         end if
         call flowstep(stop_requested, status)
         call f90_expect_eq(status, DFM_NOERR, 'The model timestep must succeed')
         call f90_expect_eq(stop_requested, 0, 'The model must reach the requested end time')
         if (status /= DFM_NOERR .or. stop_requested /= 0) then
            exit
         end if
         call f90_expect_near(const_flux(1)%bal_error, [0.0_dp, 0.0_dp], 1.0e-9_dp, 'Fine sediment must balance per output interval')
         call f90_expect_near(const_flux(2)%bal_error, [0.0_dp, 0.0_dp], 1.0e-9_dp, 'Coarse sediment must balance per output interval')
         if (time_user < 1.2_dp - 1.0e-9_dp) then
            call f90_expect_near(sum(abs(mbasedfluxtot)), 0.0_dp, 1.0e-12_dp, 'Spin-up potential flux must not transfer sediment mass')
         end if
      end do
      call f90_expect_near(time_user, tstop_user, 1.0e-9_dp)
      call f90_expect_true(sum(abs(mbasedfluxtot)) > 1.0e-4_dp, 'The regression must exercise nonzero boundary bedload')
      call f90_expect_true(maxval(abs(mbabedmassend(:, 1) - initial_mass)) > 1.0e-4_dp, 'The regression must change bed composition')
      call mba_final(time_user)
      call flowfinalize()
      call f90_expect_near(const_flux(1)%bal_error, [0.0_dp, 0.0_dp], 1.0e-9_dp, 'The fine sediment whole-period budget must close')
      call f90_expect_near(const_flux(2)%bal_error, [0.0_dp, 0.0_dp], 1.0e-9_dp, 'The coarse sediment whole-period budget must close')
   end subroutine test_bedload_order

   !> Generate a portable three-cell channel with two bedload fractions and a layered bed.
   subroutine create_bedload_input()
      call create_bedload_network()
      call create_file('bedload.mdu', [character(len=80) :: &
                       '[model]', 'Program = D-Flow FM', 'FileVersion = 1.09', &
                       '[geometry]', 'NetFile = bedload_net.nc', 'Bedlevuni = 0', 'BedlevType = 1', &
                       'WaterLevIni = 0.12', 'Conveyance2D = 0', 'Nonlin2D = 0', &
                       '[time]', 'RefDate = 20000101', 'Tunit = S', 'TStart = 0', 'TStop = 4.8', &
                       'DtUser = 0.3', 'DtMax = 0.3', 'DtInit = 0.3', &
                       '[sediment]', 'SedimentModelNr = 4', 'SedFile = bedload.sed', 'MorFile = bedload.mor', &
                       '[external forcing]', 'ExtForceFileNew = bedload.ext', &
                       '[output]', 'MapInterval = 0', 'HisInterval = 0', 'RstInterval = 0', &
                       'MbaFile = bedload_mba.ini', 'MbaInterval = 0.3', 'MbaWriteTxt = 0', 'MbaWriteNetCDF = 0'])
      call create_file('bedload.sed', [character(len=80) :: &
                       '[SedimentFileInformation]', 'FileVersion = 02.00', '[SedimentOverall]', 'Cref = 1600', &
                       '[Sediment]', 'Name = fine', 'SedTyp = bedload', 'RhoSol = 2650', 'SedDia = 0.00045', &
                       'CDryB = 1600', 'IniSedThick = 0.02', 'Trafrm = 1', 'ACAL = 1.21', &
                       '[Sediment]', 'Name = coarse', 'SedTyp = bedload', 'RhoSol = 2650', 'SedDia = 0.011', &
                       'CDryB = 1600', 'IniSedThick = 0', 'Trafrm = 1', 'ACAL = 1.21'])
      call create_file('bedload.mor', [character(len=80) :: &
                       '[MorphologyFileInformation]', 'FileVersion = 02.00', '[Morphology]', &
                       'MorFac = 1', 'MorStt = 1.2', 'BedUpd = true', 'CmpUpd = true', 'Bed = 0.94', &
                       'SedThr = 0.005', 'Thresh = 0.001', 'DensIn = false', &
                       '[Underlayer]', 'IUnderLyr = 2', 'ExchLyr = false', 'TTLForm = 2', &
                       'TTLAlpha = 0.1', 'TTLMin = 0.01', 'MxNULyr = 2', 'ThUnLyr = 0.005'])
      call create_file('bedload_mba.ini', [character(len=80) :: &
                       '[General]', 'fileVersion = 1.00', 'fileType = massBalanceAreas'])
      call create_file('bedload.ext', [character(len=80) :: &
                       '[General]', 'fileVersion = 2.01', 'fileType = extForce', &
                       '[Boundary]', 'quantity = dischargebnd', 'locationFile = inflow.pli', 'forcingFile = bedload.bc', &
                       '[Boundary]', 'quantity = waterlevelbnd', 'locationFile = outflow.pli', 'forcingFile = bedload.bc'])
      call create_file('inflow.pli', ['inflow', '2 2   ', '0 0   ', '0 1   '])
      call create_file('outflow.pli', ['outflow', '2 2    ', '3 0    ', '3 1    '])
      call create_file('bedload.bc', [character(len=80) :: &
                       '[General]', 'fileVersion = 1.01', 'fileType = boundConds', &
                       '[Forcing]', 'name = inflow_0001', 'function = constant', 'quantity = dischargebnd', 'unit = m3/s', '0.12', &
                       '[Forcing]', 'name = inflow_0002', 'function = constant', 'quantity = dischargebnd', 'unit = m3/s', '0.12', &
                       '[Forcing]', 'name = outflow_0001', 'function = constant', 'quantity = waterlevelbnd', 'unit = m', '0.12', &
                       '[Forcing]', 'name = outflow_0002', 'function = constant', 'quantity = waterlevelbnd', 'unit = m', '0.12'])
   end subroutine create_bedload_input

   !> Write eight nodes and ten links defining three square 2D cells.
   subroutine create_bedload_network()
      use netcdf

      integer :: ncid, node_dim, link_dim, pair_dim, x_var, y_var, z_var, link_var, type_var
      integer, parameter :: links(2, 10) = reshape([1, 2, 2, 3, 3, 4, 5, 6, 6, 7, 7, 8, 1, 5, 2, 6, 3, 7, 4, 8], [2, 10])

      call check_netcdf(nf90_create('bedload_net.nc', NF90_CLOBBER, ncid))
      call check_netcdf(nf90_def_dim(ncid, 'nNetNode', 8, node_dim))
      call check_netcdf(nf90_def_dim(ncid, 'nNetLink', 10, link_dim))
      call check_netcdf(nf90_def_dim(ncid, 'nNetLinkPts', 2, pair_dim))
      call check_netcdf(nf90_def_var(ncid, 'NetNode_x', NF90_DOUBLE, [node_dim], x_var))
      call check_netcdf(nf90_def_var(ncid, 'NetNode_y', NF90_DOUBLE, [node_dim], y_var))
      call check_netcdf(nf90_def_var(ncid, 'NetNode_z', NF90_DOUBLE, [node_dim], z_var))
      call check_netcdf(nf90_def_var(ncid, 'NetLink', NF90_INT, [pair_dim, link_dim], link_var))
      call check_netcdf(nf90_def_var(ncid, 'NetLinkType', NF90_INT, [link_dim], type_var))
      call check_netcdf(nf90_enddef(ncid))
      call check_netcdf(nf90_put_var(ncid, x_var, [0.0_dp, 1.0_dp, 2.0_dp, 3.0_dp, 0.0_dp, 1.0_dp, 2.0_dp, 3.0_dp]))
      call check_netcdf(nf90_put_var(ncid, y_var, [0.0_dp, 0.0_dp, 0.0_dp, 0.0_dp, 1.0_dp, 1.0_dp, 1.0_dp, 1.0_dp]))
      call check_netcdf(nf90_put_var(ncid, z_var, [0.0_dp, 0.0_dp, 0.0_dp, 0.0_dp, 0.0_dp, 0.0_dp, 0.0_dp, 0.0_dp]))
      call check_netcdf(nf90_put_var(ncid, link_var, links))
      call check_netcdf(nf90_put_var(ncid, type_var, [2, 2, 2, 2, 2, 2, 2, 2, 2, 2]))
      call check_netcdf(nf90_close(ncid))
   end subroutine create_bedload_network

   !> Preserve NetCDF error details when creating the fixture fails.
   subroutine check_netcdf(status)
      use netcdf, only: nf90_noerr, nf90_strerror

      integer, intent(in) :: status

      if (status /= nf90_noerr) then
         error stop nf90_strerror(status)
      end if
   end subroutine check_netcdf
end module test_bedload_balance
