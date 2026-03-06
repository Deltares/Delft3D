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

end module m_waveconst
