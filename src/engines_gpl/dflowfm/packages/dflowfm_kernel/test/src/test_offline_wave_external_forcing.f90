module test_offline_wave_external_forcing
   use assertions_gtest
   use precision, only: dp

   implicit none(type, external)

   character(len=*), parameter :: POSITIVE_EXT_FILE = 'test_offline_wave_minimal.ext'
   character(len=*), parameter :: NEGATIVE_EXT_FILE = 'test_offline_wave_missing_force.ext'
   character(len=*), parameter :: PERIOD_BC_FILE = 'test_offline_wave_period.bc'
   character(len=*), parameter :: FORCE_X_BC_FILE = 'test_offline_wave_force_x.bc'
   character(len=*), parameter :: FORCE_Y_BC_FILE = 'test_offline_wave_force_y.bc'

contains

   subroutine create_scalar_bc_file(file_name, quantity, unit, value_at_t0, value_at_t100)
      use m_file_helpers, only: create_file

      character(len=*), intent(in) :: file_name
      character(len=*), intent(in) :: quantity
      character(len=*), intent(in) :: unit
      real(dp), intent(in) :: value_at_t0
      real(dp), intent(in) :: value_at_t100

      character(len=64) :: value_at_t0_text, value_at_t100_text

      write (value_at_t0_text, '(g0)') value_at_t0
      write (value_at_t100_text, '(g0)') value_at_t100
      call create_file(file_name, [ &
                       '[General]', &
                       '    fileVersion           = 1.01', &
                       '    fileType              = boundConds', &
                       '', &
                       '[Forcing]', &
                       '    name                  = global', &
                       '    function              = timeSeries', &
                       '    timeInterpolation     = linear', &
                       '    quantity              = time', &
                       '    unit                  = seconds since 2000-01-01 00:00:00', &
                       '    quantity              = '//trim(quantity), &
                       '    unit                  = '//trim(unit), &
                       '    0    '//trim(value_at_t0_text), &
                       '    100  '//trim(value_at_t100_text)])
   end subroutine create_scalar_bc_file

   subroutine create_offline_wave_ext_file(file_name, include_y_force)
      use m_file_helpers, only: create_file

      character(len=*), intent(in) :: file_name
      logical, intent(in) :: include_y_force

      if (include_y_force) then
         call create_file(file_name, [ &
                          '[Spatial]', &
                          '    quantity        = waveperiod', &
                          '    forcingFile     = '//PERIOD_BC_FILE, &
                          '    forcingFileType = bcascii', &
                          '', &
                          '[Spatial]', &
                          '    quantity        = xwaveforce', &
                          '    forcingFile     = '//FORCE_X_BC_FILE, &
                          '    forcingFileType = bcascii', &
                          '', &
                          '[Spatial]', &
                          '    quantity        = ywaveforce', &
                          '    forcingFile     = '//FORCE_Y_BC_FILE, &
                          '    forcingFileType = bcascii'])
      else
         call create_file(file_name, [ &
                          '[Spatial]', &
                          '    quantity        = waveperiod', &
                          '    forcingFile     = '//PERIOD_BC_FILE, &
                          '    forcingFileType = bcascii', &
                          '', &
                          '[Spatial]', &
                          '    quantity        = xwaveforce', &
                          '    forcingFile     = '//FORCE_X_BC_FILE, &
                          '    forcingFileType = bcascii'])
      end if
   end subroutine create_offline_wave_ext_file

   subroutine setup_offline_radiation_stress_case(ext_file)
      use m_alloc, only: realloc
      use m_cell_geometry, only: ndx, xz, yz
      use m_flow, only: flow_without_waves, jawave, modind
      use m_flowgeom, only: kcs, ndxi
      use m_flowparameters, only: jawaveStokes, jawavedelta, jawaveforces, jawavestreaming, waveforcing
      use m_meteo, only: initialize_ec_module
      use m_unstruc_model_data, only: extfile_new_list
      use m_waveconst, only: NO_STOKES_DRIFT, WAVE_BOUNDARYLAYER_OFF, WAVE_FORCES_ON, WAVE_NC_OFFLINE, &
                             WAVE_STREAMING_OFF, WAVEFORCING_RADIATION_STRESS, get_offline_wave_input_requirements
      use m_waves, only: default_waves, offline_wave_input_requirements, sxwav, sywav, twavcom

      character(len=*), intent(in) :: ext_file

      ! init_new only needs the target S-point geometry. Do not use flow_geominit:
      ! that routine requires a fully initialized model, while this test exercises
      ! only current-format external-forcing registration.
      ndx = 1
      ndxi = ndx
      call realloc(xz, ndx, fill=0.0_dp, keepExisting=.false.)
      call realloc(yz, ndx, fill=0.0_dp, keepExisting=.false.)
      call realloc(kcs, ndx, fill=1, keepExisting=.false.)

      jawave = WAVE_NC_OFFLINE
      waveforcing = WAVEFORCING_RADIATION_STRESS
      jawaveforces = WAVE_FORCES_ON
      jawaveStokes = NO_STOKES_DRIFT
      jawavestreaming = WAVE_STREAMING_OFF
      jawavedelta = WAVE_BOUNDARYLAYER_OFF
      modind = 0
      flow_without_waves = .false.
      call default_waves()
      offline_wave_input_requirements = get_offline_wave_input_requirements(waveforcing, jawaveforces, jawaveStokes, &
                                                                            jawavestreaming, jawavedelta, .false., &
                                                                            flow_without_waves)
      call realloc(twavcom, ndx, fill=0.0_dp, keepExisting=.false.)
      call realloc(sxwav, ndx, fill=0.0_dp, keepExisting=.false.)
      call realloc(sywav, ndx, fill=0.0_dp, keepExisting=.false.)
      call initialize_ec_module()

      extfile_new_list = [trim(ext_file)]
   end subroutine setup_offline_radiation_stress_case

   subroutine teardown_offline_radiation_stress_case()
      use m_cell_geometry, only: xz, yz
      use m_ec_module, only: ecFreeInstance
      use m_flowgeom, only: kcs
      use m_meteo, only: ecInstancePtr
      use m_resetfullflowmodel, only: resetFullFlowModel
      use m_waves, only: sxwav, sywav, twavcom

      logical :: success

      if (associated(ecInstancePtr)) then
         success = ecFreeInstance(ecInstancePtr)
         call f90_expect_true(success, 'EC instance cleanup should succeed')
      end if

      call resetFullFlowModel()

      if (allocated(xz)) deallocate (xz)
      if (allocated(yz)) deallocate (yz)
      if (allocated(kcs)) deallocate (kcs)
      if (allocated(twavcom)) deallocate (twavcom)
      if (allocated(sxwav)) deallocate (sxwav)
      if (allocated(sywav)) deallocate (sywav)
   end subroutine teardown_offline_radiation_stress_case

   !$f90tw TESTCODE(TEST, test_offline_wave_external_forcing, test_radiation_stress_accepts_bcascii_without_inactive_inputs, test_radiation_stress_accepts_bcascii_without_inactive_inputs,
   !> A Wavemodelnr=7 radiation-stress configuration needs only period and the
   !! two force components. This verifies that current-format bcascii fields can
   !! supply those inputs without NetCDF and without inactive height/direction fields.
   subroutine test_radiation_stress_accepts_bcascii_without_inactive_inputs() bind(C)
      use dfm_error, only: DFM_NOERR
      use fm_external_forcings, only: init_new
      use m_meteo, only: ec_gettimespacevalue, ecInstancePtr, item_fx, item_fy, item_tp
      use m_waveconst, only: WAVE_INPUT_DIRECTION, WAVE_INPUT_FORCE_X, WAVE_INPUT_FORCE_Y, WAVE_INPUT_PERIOD, &
                             WAVE_INPUT_SIGNIFICANT_HEIGHT, wave_input_is_required
      use m_waves, only: offline_wave_input_providers, offline_wave_input_requirements, sxwav, sywav, twavcom
      use unstruc_messages, only: threshold_abort
      use messagehandling, only: LEVEL_FATAL

      integer :: iresult
      logical :: success

      call create_scalar_bc_file(PERIOD_BC_FILE, 'waveperiod', 's', 4.0_dp, 6.0_dp)
      call create_scalar_bc_file(FORCE_X_BC_FILE, 'xwaveforce', 'N/m2', 1.0_dp, 3.0_dp)
      call create_scalar_bc_file(FORCE_Y_BC_FILE, 'ywaveforce', 'N/m2', -2.0_dp, 2.0_dp)
      call create_offline_wave_ext_file(POSITIVE_EXT_FILE, include_y_force=.true.)
      threshold_abort = LEVEL_FATAL
      call setup_offline_radiation_stress_case(POSITIVE_EXT_FILE)

      iresult = DFM_NOERR
      call init_new(iresult)

      call f90_expect_eq(iresult, DFM_NOERR, &
                         'offline radiation-stress forcing should initialize without wave height or direction')
      call f90_expect_eq(offline_wave_input_providers, offline_wave_input_requirements, &
                         'configured wave providers should match the active requirements exactly')
      call f90_expect_true(wave_input_is_required(offline_wave_input_providers, WAVE_INPUT_PERIOD), &
                           'waveperiod provider should be registered')
      call f90_expect_true(wave_input_is_required(offline_wave_input_providers, WAVE_INPUT_FORCE_X), &
                           'xwaveforce provider should be registered')
      call f90_expect_true(wave_input_is_required(offline_wave_input_providers, WAVE_INPUT_FORCE_Y), &
                           'ywaveforce provider should be registered')
      call f90_expect_false(wave_input_is_required(offline_wave_input_providers, WAVE_INPUT_SIGNIFICANT_HEIGHT), &
                            'wave height must remain absent from the minimal forcing file')
      call f90_expect_false(wave_input_is_required(offline_wave_input_providers, WAVE_INPUT_DIRECTION), &
                            'wave direction must remain absent from the minimal forcing file')

      success = ec_gettimespacevalue(ecInstancePtr, item_tp, 20000101, 0.0_dp, 1, 50.0_dp)
      call f90_expect_true(success, 'waveperiod bcascii connection should return a value')
      success = ec_gettimespacevalue(ecInstancePtr, item_fx, 20000101, 0.0_dp, 1, 50.0_dp)
      call f90_expect_true(success, 'xwaveforce bcascii connection should return a value')
      success = ec_gettimespacevalue(ecInstancePtr, item_fy, 20000101, 0.0_dp, 1, 50.0_dp)
      call f90_expect_true(success, 'ywaveforce bcascii connection should return a value')
      call f90_expect_near(twavcom(1), 5.0_dp, 1.0e-6_dp, 'waveperiod should be linearly interpolated from bcascii')
      call f90_expect_near(sxwav(1), 2.0_dp, 1.0e-6_dp, 'xwaveforce should be linearly interpolated from bcascii')
      call f90_expect_near(sywav(1), 0.0_dp, 1.0e-6_dp, 'ywaveforce should be linearly interpolated from bcascii')

      call teardown_offline_radiation_stress_case()
   end subroutine test_radiation_stress_accepts_bcascii_without_inactive_inputs
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_offline_wave_external_forcing, test_radiation_stress_detects_missing_active_force_provider, test_radiation_stress_detects_missing_active_force_provider,
   !> Omitting an inactive quantity is allowed, but omitting an active quantity
   !! leaves the provider set incomplete after current-format forcing initialization.
   subroutine test_radiation_stress_detects_missing_active_force_provider() bind(C)
      use dfm_error, only: DFM_NOERR
      use fm_external_forcings, only: init_new
      use m_waveconst, only: WAVE_INPUT_FORCE_Y, wave_input_is_required
      use m_waves, only: offline_wave_input_providers, offline_wave_input_requirements
      use unstruc_messages, only: threshold_abort
      use messagehandling, only: LEVEL_FATAL

      integer :: iresult

      call create_scalar_bc_file(PERIOD_BC_FILE, 'waveperiod', 's', 4.0_dp, 6.0_dp)
      call create_scalar_bc_file(FORCE_X_BC_FILE, 'xwaveforce', 'N/m2', 1.0_dp, 3.0_dp)
      call create_offline_wave_ext_file(NEGATIVE_EXT_FILE, include_y_force=.false.)
      threshold_abort = LEVEL_FATAL
      call setup_offline_radiation_stress_case(NEGATIVE_EXT_FILE)

      iresult = DFM_NOERR
      call init_new(iresult)

      call f90_expect_eq(iresult, DFM_NOERR, 'current-format external forcing initialization should succeed')
      call f90_expect_true(wave_input_is_required(offline_wave_input_requirements, WAVE_INPUT_FORCE_Y), &
                           'ywaveforce must be required by the configured radiation-stress forcing')
      call f90_expect_false(wave_input_is_required(offline_wave_input_providers, WAVE_INPUT_FORCE_Y), &
                            'omitting ywaveforce must leave its required provider unregistered')
      call f90_expect_false(offline_wave_input_providers == offline_wave_input_requirements, &
                            'the incomplete forcing file must not satisfy all offline wave requirements')

      call teardown_offline_radiation_stress_case()
   end subroutine test_radiation_stress_detects_missing_active_force_provider
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_offline_wave_external_forcing, test_flow_without_waves_recalculates_derived_ht, test_flow_without_waves_recalculates_derived_ht,
   !> Raw EC wave height and period must remain unchanged while FM recalculates
   !! the derived fields for the current water depth, including open boundaries.
   subroutine test_flow_without_waves_recalculates_derived_ht() bind(C)
      use m_alloc, only: realloc
      use m_compute_wave_parameters, only: compute_wave_parameters
      use m_flow, only: epshu, flow_without_waves, hs, jawave, jawavestokes, kmx, s1
      use m_flowgeom, only: bl, ndx
      use fm_external_forcings_data, only: nbndn, nbndt, nbndu, nbndz, kbndz
      use m_physcoef, only: ag
      use m_sferic, only: pi
      use m_waveconst, only: NO_STOKES_DRIFT, WAVE_INPUT_PERIOD, WAVE_INPUT_SIGNIFICANT_HEIGHT, WAVE_NC_OFFLINE
      use m_waves, only: default_waves, gammax, hwav, hwavcom, jauorbfromswan, offline_wave_input_requirements, rlabda, &
                         twav, twavcom, uorb

      call default_waves()
      ndx = 2
      call realloc(bl, ndx, fill=0.0_dp, keepExisting=.false.)
      call realloc(hs, ndx, fill=0.0_dp, keepExisting=.false.)
      call realloc(s1, ndx, fill=0.0_dp, keepExisting=.false.)
      call realloc(hwav, ndx, fill=0.0_dp, keepExisting=.false.)
      call realloc(twav, ndx, fill=0.0_dp, keepExisting=.false.)
      call realloc(hwavcom, ndx, fill=0.0_dp, keepExisting=.false.)
      call realloc(twavcom, ndx, fill=0.0_dp, keepExisting=.false.)
      call realloc(uorb, ndx, fill=0.0_dp, keepExisting=.false.)
      call realloc(rlabda, ndx, fill=0.0_dp, keepExisting=.false.)
      call realloc(kbndz, [6, 1], fill=0, keepExisting=.false.)

      jawave = WAVE_NC_OFFLINE
      flow_without_waves = .true.
      jawavestokes = NO_STOKES_DRIFT
      kmx = 0
      epshu = 1.0e-4_dp
      ag = 9.81_dp
      pi = acos(-1.0_dp)
      gammax = 1.0_dp
      jauorbfromswan = 0
      offline_wave_input_requirements = ior(WAVE_INPUT_SIGNIFICANT_HEIGHT, WAVE_INPUT_PERIOD)
      hwavcom = [0.0_dp, 4.0_dp]
      twavcom = [0.0_dp, 8.0_dp]
      hs = 10.0_dp
      s1 = 10.0_dp
      nbndu = 0
      nbndn = 0
      nbndt = 0
      nbndz = 1
      kbndz(1:2, 1) = [1, 2]

      call compute_wave_parameters()

      call f90_expect_near(hwavcom(1), 0.0_dp, 1.0e-12_dp, 'raw boundary wave height must remain unchanged after calculation')
      call f90_expect_near(twavcom(1), 0.0_dp, 1.0e-12_dp, 'raw boundary wave period must remain unchanged after calculation')
      call f90_expect_near(hwavcom(2), 4.0_dp, 1.0e-12_dp, 'raw inner wave height must remain unchanged after calculation')
      call f90_expect_near(twavcom(2), 8.0_dp, 1.0e-12_dp, 'raw inner wave period must remain unchanged after calculation')
      call f90_expect_near(hwav(1), 4.0_dp / sqrt(2.0_dp), 1.0e-12_dp, 'derived boundary wave height must copy the inner value')
      call f90_expect_near(twav(1), 8.0_dp, 1.0e-12_dp, 'derived boundary wave period must copy the inner value')

      hs = 1.0_dp
      s1 = 1.0_dp
      call compute_wave_parameters()

      call f90_expect_near(hwavcom(1), 0.0_dp, 1.0e-12_dp, 'raw boundary wave height must survive a second calculation')
      call f90_expect_near(twavcom(1), 0.0_dp, 1.0e-12_dp, 'raw boundary wave period must survive a second calculation')
      call f90_expect_near(hwavcom(2), 4.0_dp, 1.0e-12_dp, 'raw inner wave height must survive a second calculation')
      call f90_expect_near(twavcom(2), 8.0_dp, 1.0e-12_dp, 'raw inner wave period must survive a second calculation')
      call f90_expect_near(hwav(1), 1.0_dp, 1.0e-12_dp, 'derived boundary wave height must be recalculated and depth-limited')
      call f90_expect_near(twav(1), 8.0_dp, 1.0e-12_dp, 'derived boundary wave period must be recalculated from the inner value')

      if (allocated(bl)) deallocate (bl)
      if (allocated(hs)) deallocate (hs)
      if (allocated(s1)) deallocate (s1)
      if (allocated(hwav)) deallocate (hwav)
      if (allocated(twav)) deallocate (twav)
      if (allocated(hwavcom)) deallocate (hwavcom)
      if (allocated(twavcom)) deallocate (twavcom)
      if (allocated(uorb)) deallocate (uorb)
      if (allocated(rlabda)) deallocate (rlabda)
      if (allocated(kbndz)) deallocate (kbndz)
      ndx = 0
      nbndz = 0
      flow_without_waves = .false.
      call default_waves()
   end subroutine test_flow_without_waves_recalculates_derived_ht
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_offline_wave_external_forcing, test_wave_requirements_survive_flow_state_reset, test_wave_requirements_survive_flow_state_reset,
   subroutine test_wave_requirements_survive_flow_state_reset() bind(C)
      use m_waveconst, only: WAVE_INPUT_FORCE_X, WAVE_INPUT_FORCE_Y, WAVE_INPUT_PERIOD
      use m_waves, only: default_waves, reset_waves, offline_wave_input_requirements

      integer :: expected_requirements

      expected_requirements = ior(WAVE_INPUT_PERIOD, ior(WAVE_INPUT_FORCE_X, WAVE_INPUT_FORCE_Y))

      call default_waves()
      offline_wave_input_requirements = expected_requirements
      call reset_waves()

      call f90_expect_eq(offline_wave_input_requirements, expected_requirements, &
                         'flow-state reset must retain offline wave requirements derived from the MDU')

      call default_waves()
      call f90_expect_eq(offline_wave_input_requirements, 0, &
                         'full model reset must clear offline wave requirements')
   end subroutine test_wave_requirements_survive_flow_state_reset
   !$f90tw)

end module test_offline_wave_external_forcing
