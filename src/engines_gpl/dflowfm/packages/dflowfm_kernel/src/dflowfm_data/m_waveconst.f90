module m_waveconst
   implicit none

   ! wavemodelnr
   integer, parameter :: NO_WAVES = 0
   integer, parameter :: WAVE_FETCH_HURDLE = 1
   integer, parameter :: WAVE_FETCH_YOUNG = 2
   integer, parameter :: WAVE_SWAN_ONLINE = 3
   integer, parameter :: WAVE_SURFBEAT = 4
   integer, parameter :: WAVE_UNIFORM = 5
   integer, parameter :: WAVE_NC_OFFLINE = 7

   ! wave forcing
   integer, parameter :: WAVE_FORCES_OFF = 0
   integer, parameter :: WAVE_FORCES_ON = 1

   ! offline wave force calculation
   integer, parameter :: WAVEFORCING_NO_WAVEFORCES = 0
   integer, parameter :: WAVEFORCING_RADIATION_STRESS = 1
   integer, parameter :: WAVEFORCING_DISSIPATION_TOTAL = 2
   integer, parameter :: WAVEFORCING_DISSIPATION_3D = 3

   ! Offline wave input quantities. Individual vector components have separate
   ! flags so initialization can report exactly which provider is missing.
   integer, parameter :: WAVE_INPUT_SIGNIFICANT_HEIGHT = 1
   integer, parameter :: WAVE_INPUT_PERIOD = 2
   integer, parameter :: WAVE_INPUT_DIRECTION = 4
   integer, parameter :: WAVE_INPUT_FORCE_X = 8
   integer, parameter :: WAVE_INPUT_FORCE_Y = 16
   integer, parameter :: WAVE_INPUT_DISSIPATION_TOTAL = 32
   integer, parameter :: WAVE_INPUT_DISSIPATION_SURFACE = 64
   integer, parameter :: WAVE_INPUT_DISSIPATION_WHITE_CAPPING = 128

   ! Stokes drift profile
   integer, parameter :: NO_STOKES_DRIFT = 0
   integer, parameter :: STOKES_DRIFT_DEPTHUNIFORM = 1
   integer, parameter :: STOKES_DRIFT_2NDORDER = 2
   integer, parameter :: STOKES_DRIFT_2NDORDER_VISC = 3
   integer, parameter :: STOKES_DRIFT_2NDORDER_VISC_ADVE = 4

   ! Wave boundary layer streaming
   integer, parameter :: WAVE_STREAMING_OFF = 0
   integer, parameter :: WAVE_STREAMING_ON = 1

   ! Wave breaker turbulence
   integer, parameter :: WAVE_BREAKER_TURB_OFF = 0
   integer, parameter :: WAVE_BREAKER_TURB_ON = 1

   ! WAQ coupling shear stress
   integer, parameter :: WAVE_WAQ_SHEAR_STRESS_HYD = 0
   integer, parameter :: WAVE_WAQ_SHEAR_STRESS_LINEAR_SUM = 1
   integer, parameter :: WAVE_WAQ_SHEAR_STRESS_MAX_SHEAR_STRESS = 2

   ! Wave boundary layer formulation
   integer, parameter :: WAVE_BOUNDARYLAYER_OFF = 0
   integer, parameter :: WAVE_BOUNDARYLAYER_SANA = 1

   ! Euler velocities
   integer, parameter :: WAVE_EULER_VELOCITIES_OUTPUT_OFF = 0
   integer, parameter :: WAVE_EULER_VELOCITIES_OUTPUT_ON = 1

contains

   !> Derive the required offline wave input quantities from active processes.
   pure integer function get_offline_wave_input_requirements(waveforcing, wave_forces, stokes_drift, wave_streaming, &
                                                              wave_boundary_layer, bottom_shear, flow_without_waves) result(requirements)
      integer, intent(in) :: waveforcing
      integer, intent(in) :: wave_forces
      integer, intent(in) :: stokes_drift
      integer, intent(in) :: wave_streaming
      integer, intent(in) :: wave_boundary_layer
      logical, intent(in) :: bottom_shear
      logical, intent(in) :: flow_without_waves

      logical :: wave_kinematics_required

      requirements = 0

      wave_kinematics_required = stokes_drift > NO_STOKES_DRIFT .or. &
                                 wave_streaming > WAVE_STREAMING_OFF .or. &
                                 wave_boundary_layer > WAVE_BOUNDARYLAYER_OFF .or. &
                                 bottom_shear .or. flow_without_waves

      if (wave_kinematics_required) then
         requirements = ior(requirements, WAVE_INPUT_SIGNIFICANT_HEIGHT)
         requirements = ior(requirements, WAVE_INPUT_PERIOD)
         requirements = ior(requirements, WAVE_INPUT_DIRECTION)
      end if

      if (wave_forces > WAVE_FORCES_OFF) then
         select case (waveforcing)
         case (WAVEFORCING_RADIATION_STRESS)
            requirements = ior(requirements, WAVE_INPUT_PERIOD)
            requirements = ior(requirements, WAVE_INPUT_FORCE_X)
            requirements = ior(requirements, WAVE_INPUT_FORCE_Y)
         case (WAVEFORCING_DISSIPATION_TOTAL)
            requirements = ior(requirements, WAVE_INPUT_SIGNIFICANT_HEIGHT)
            requirements = ior(requirements, WAVE_INPUT_PERIOD)
            requirements = ior(requirements, WAVE_INPUT_DIRECTION)
            requirements = ior(requirements, WAVE_INPUT_DISSIPATION_TOTAL)
         case (WAVEFORCING_DISSIPATION_3D)
            requirements = ior(requirements, WAVE_INPUT_SIGNIFICANT_HEIGHT)
            requirements = ior(requirements, WAVE_INPUT_PERIOD)
            requirements = ior(requirements, WAVE_INPUT_DIRECTION)
            requirements = ior(requirements, WAVE_INPUT_FORCE_X)
            requirements = ior(requirements, WAVE_INPUT_FORCE_Y)
            requirements = ior(requirements, WAVE_INPUT_DISSIPATION_SURFACE)
            requirements = ior(requirements, WAVE_INPUT_DISSIPATION_WHITE_CAPPING)
         end select
      end if
   end function get_offline_wave_input_requirements

   !> Return whether one quantity flag is present in a wave input mask.
   pure logical function wave_input_is_required(requirements, quantity_flag)
      integer, intent(in) :: requirements
      integer, intent(in) :: quantity_flag

      wave_input_is_required = iand(requirements, quantity_flag) /= 0
   end function wave_input_is_required

end module m_waveconst
