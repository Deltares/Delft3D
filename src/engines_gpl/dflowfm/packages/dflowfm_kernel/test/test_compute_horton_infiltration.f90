!> This module contains unit tests for the compute_horton_infiltration function in the horton module.
!! It verifies the correct behavior of infiltration capacity under various conditions:
!! 1) Return of HORTON_CAPSTATE_NOCHANGE when min_inf_cap > max_inf_cap.
!! 2) Infiltration capacity decrease due to rainfall exceeding minimum infiltration capacity.
!! 3) Infiltration capacity decrease due to presence of waterdepth.
!! 4) Infiltration capacity recovery in dry conditions with rainfall below minimum infiltration capacity.
!! 5) Infiltration capacity recovery in dry conditions with no rainfall and no waterdepth
module test_compute_horton_infiltration
   use assertions_gtest
   use precision, only: dp
   use precision_basics, only: comparereal
   use m_horton

   implicit none(external)

   ! Conversion factors
   real(kind=dp), parameter :: SECOND_TO_HOUR = 1.0_dp / 3600.0_dp !< Number of seconds per hour
   real(kind=dp), parameter :: MPS_TO_MMPHR = 1000.0_dp / SECOND_TO_HOUR !< Conversion factor from m/s to mm/hr

contains

   !$f90tw TESTCODE(TEST, test_compute_horton_infiltration, test_horton_infiltration_nochange, test_horton_infiltration_nochange,
   !> Test whether HORTON_CAPSTATE_NOCHANGE infiltration capacity state is returned when min_inf_cap > max_inf_cap
   subroutine test_horton_infiltration_nochange() bind(C)
      ! Declare variables
      type(t_HortonInfiltrationConfig) :: config !< [-] Horton infiltration configuration
      integer :: ierr !< [-] error code
      integer :: n !< [-] number of cells
      integer :: include_rain !< [-] flag to include rainfall (0/1)
      real(kind=dp) :: time_step !< [s] time step
      real(kind=dp), dimension(:), allocatable :: inf_cap !< [m/s] infiltration capacity
      real(kind=dp), dimension(:), allocatable :: waterdepth !< [m] waterdepth
      real(kind=dp), dimension(:), allocatable :: rainfall !< [mm/day] rainfall
      integer, dimension(:), allocatable :: inf_cap_state !< [-] infiltration capacity state

      ! Initialize variables
      call initialize_horton_test_suite(config, ierr, n, include_rain, time_step, inf_cap, waterdepth, rainfall, inf_cap_state)

      ! Set min_inf_cap greater than max_inf_cap to trigger NOCHANGE state
      config%min_inf_cap = 2.0_dp

      ! Compute horton infiltration
      ierr = compute_horton_infiltration(config, n, include_rain, time_step, inf_cap, waterdepth, rainfall, inf_cap_state)

      ! Compare results
      call f90_expect_eq(inf_cap_state(1), HORTON_CAPSTAT_NOCHANGE, "Infiltration capacity state should be HORTON_CAPSTAT_NOCHANGE (0)")

   end subroutine test_horton_infiltration_nochange
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_compute_horton_infiltration, test_horton_infiltration_decreasing_rain, test_horton_infiltration_decreasing_rain,
   !> Test decreasing infiltration capacity in wet conditions (rainfall > min_inf_cap, no waterdepth)
   subroutine test_horton_infiltration_decreasing_rain() bind(C)
      ! Declare variables
      type(t_HortonInfiltrationConfig) :: config !< [-] Horton infiltration configuration
      integer :: ierr !< [-] error code
      integer :: n !< [-] number of cells
      integer :: include_rain !< [-] flag to include rainfall (0/1)
      real(kind=dp) :: time_step !< [s] time step
      real(kind=dp) :: expected_result !< [mm/hr] expected infiltration capacity result
      real(kind=dp), dimension(:), allocatable :: inf_cap !< [m/s] infiltration capacity
      real(kind=dp), dimension(:), allocatable :: waterdepth !< [m] waterdepth
      real(kind=dp), dimension(:), allocatable :: rainfall !< [mm/day] rainfall
      integer, dimension(:), allocatable :: inf_cap_state !< [-] infiltration capacity state

      ! Initialize variables
      call initialize_horton_test_suite(config, ierr, n, include_rain, time_step, inf_cap, waterdepth, rainfall, inf_cap_state)

      ! Set rainfall greater than min_inf_cap to trigger DECREASE state
      ! Keep in mind: rainfall is in [mm/day] while minimum infiltration capacity is in [mm/hr]
      rainfall = 20.0_dp

      ! Compute horton infiltration
      ierr = compute_horton_infiltration(config, n, include_rain, time_step, inf_cap, waterdepth, rainfall, inf_cap_state)

      ! Compute analytical result
      expected_result = config%min_inf_cap(1) + (config%max_inf_cap(1) - config%min_inf_cap(1)) * exp(-1.0_dp * config%decrease_rate(1) * time_step * SECOND_TO_HOUR)

      ! Convert inf_cap to mm/hr for comparison
      inf_cap = inf_cap * MPS_TO_MMPHR

      ! Compare results
      call f90_expect_true(comparereal(inf_cap(1), expected_result) == 0, "Infiltration capacity does not match expected value")
      call f90_expect_eq(inf_cap_state(1), HORTON_CAPSTAT_DECREASE, "Infiltration capacity state should be HORTON_CAPSTAT_DECREASE (1)")

   end subroutine test_horton_infiltration_decreasing_rain
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_compute_horton_infiltration, test_horton_infiltration_decreasing_waterdepth, test_horton_infiltration_decreasing_waterdepth,
   !> Test decreasing infiltration capacity in wet conditions (no rainfall, but waterdepth present)
   subroutine test_horton_infiltration_decreasing_waterdepth() bind(C)
      ! Declare variables
      type(t_HortonInfiltrationConfig) :: config !< [-] Horton infiltration configuration
      integer :: ierr !< [-] error code
      integer :: n !< [-] number of cells
      integer :: include_rain !< [-] flag to include rainfall (0/1)
      real(kind=dp) :: time_step !< [s] time step
      real(kind=dp) :: expected_result !< [mm/hr] expected infiltration capacity result
      real(kind=dp), dimension(:), allocatable :: inf_cap !< [m/s] infiltration capacity
      real(kind=dp), dimension(:), allocatable :: waterdepth !< [m] waterdepth
      real(kind=dp), dimension(:), allocatable :: rainfall !< [mm/day] rainfall
      integer, dimension(:), allocatable :: inf_cap_state !< [-] infiltration capacity state

      ! Initialize variables
      call initialize_horton_test_suite(config, ierr, n, include_rain, time_step, inf_cap, waterdepth, rainfall, inf_cap_state)

      ! Set waterdepth greater than 0 to trigger DECREASE state
      waterdepth = 1.0_dp

      ! Compute horton infiltration
      ierr = compute_horton_infiltration(config, n, include_rain, time_step, inf_cap, waterdepth, rainfall, inf_cap_state)

      ! Compute analytical result
      expected_result = config%min_inf_cap(1) + (config%max_inf_cap(1) - config%min_inf_cap(1)) * exp(-1.0_dp * config%decrease_rate(1) * time_step * SECOND_TO_HOUR)

      ! Convert inf_cap to mm/hr for comparison
      inf_cap = inf_cap * MPS_TO_MMPHR

      ! Compare results
      call f90_expect_true(comparereal(inf_cap(1), expected_result) == 0, "Infiltration capacity does not match expected value")
      call f90_expect_eq(inf_cap_state(1), HORTON_CAPSTAT_DECREASE, "Infiltration capacity state should be HORTON_CAPSTAT_DECREASE (1)")

   end subroutine test_horton_infiltration_decreasing_waterdepth
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_compute_horton_infiltration, test_horton_infiltration_recovering_rain, test_horton_infiltration_recovering_rain,
   !> Test recovering infiltration capacity in dry conditions (rainfall < min_inf_cap and no waterdepth)
   subroutine test_horton_infiltration_recovering_rain() bind(C)
      ! Declare variables
      type(t_HortonInfiltrationConfig) :: config !< [-] Horton infiltration configuration
      integer :: ierr !< [-] error code
      integer :: n !< [-] number of cells
      integer :: include_rain !< [-] flag to include rainfall (0/1)
      real(kind=dp) :: time_step !< [s] time step
      real(kind=dp) :: expected_result !< [mm/hr] expected infiltration capacity result
      real(kind=dp), dimension(:), allocatable :: inf_cap !< [m/s] infiltration capacity
      real(kind=dp), dimension(:), allocatable :: waterdepth !< [m] waterdepth
      real(kind=dp), dimension(:), allocatable :: rainfall !< [mm/day] rainfall
      integer, dimension(:), allocatable :: inf_cap_state !< [-] infiltration capacity state

      ! Initialize variables
      call initialize_horton_test_suite(config, ierr, n, include_rain, time_step, inf_cap, waterdepth, rainfall, inf_cap_state)

      ! Set initial infiltration capacity to min_inf_cap
      inf_cap = config%min_inf_cap(1) / MPS_TO_MMPHR

      ! Set rainfall below min_inf_cap to trigger RECOVERY state
      ! Keep in mind: rainfall is in [mm/day] while minimum infiltration capacity is in [mm/hr]
      rainfall = 10.0_dp

      ! Compute horton infiltration
      ierr = compute_horton_infiltration(config, n, include_rain, time_step, inf_cap, waterdepth, rainfall, inf_cap_state)

      ! Compute analytical result
      expected_result = config%max_inf_cap(1) - (config%max_inf_cap(1) - config%min_inf_cap(1)) * exp(-1.0_dp * config%recovery_rate(1) * time_step * SECOND_TO_HOUR)

      ! Convert inf_cap to mm/hr for comparison
      inf_cap = inf_cap * MPS_TO_MMPHR

      ! Compare results
      call f90_expect_true(comparereal(inf_cap(1), expected_result) == 0, "Infiltration capacity does not match expected value")
      call f90_expect_eq(inf_cap_state(1), HORTON_CAPSTAT_RECOVERY, "Infiltration capacity state should be HORTON_CAPSTAT_RECOVERY (2)")

   end subroutine test_horton_infiltration_recovering_rain
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_compute_horton_infiltration, test_horton_infiltration_recovering_dry, test_horton_infiltration_recovering_dry,
   !> Test recovering infiltration capacity in dry conditions (no rainfall and no waterdepth)
   subroutine test_horton_infiltration_recovering_dry() bind(C)
      ! Declare variables
      type(t_HortonInfiltrationConfig) :: config !< [-] Horton infiltration configuration
      integer :: ierr !< [-] error code
      integer :: n !< [-] number of cells
      integer :: include_rain !< [-] flag to include rainfall (0/1)
      real(kind=dp) :: time_step !< [s] time step
      real(kind=dp) :: expected_result !< [mm/hr] expected infiltration capacity result
      real(kind=dp), dimension(:), allocatable :: inf_cap !< [m/s] infiltration capacity
      real(kind=dp), dimension(:), allocatable :: waterdepth !< [m] waterdepth
      real(kind=dp), dimension(:), allocatable :: rainfall !< [mm/day] rainfall
      integer, dimension(:), allocatable :: inf_cap_state !< [-] infiltration capacity state

      ! Initialize variables
      call initialize_horton_test_suite(config, ierr, n, include_rain, time_step, inf_cap, waterdepth, rainfall, inf_cap_state)

      ! Set initial infiltration capacity to min_inf_cap
      inf_cap = config%min_inf_cap(1) / MPS_TO_MMPHR

      ! Compute horton infiltration
      ierr = compute_horton_infiltration(config, n, include_rain, time_step, inf_cap, waterdepth, rainfall, inf_cap_state)

      ! Compute analytical result
      expected_result = config%max_inf_cap(1) - (config%max_inf_cap(1) - config%min_inf_cap(1)) * exp(-1.0_dp * config%recovery_rate(1) * time_step * SECOND_TO_HOUR)

      ! Convert inf_cap to mm/hr for comparison
      inf_cap = inf_cap * MPS_TO_MMPHR

      ! Compare results
      call f90_expect_true(comparereal(inf_cap(1), expected_result) == 0, "Infiltration capacity does not match expected value")
      call f90_expect_eq(inf_cap_state(1), HORTON_CAPSTAT_RECOVERY, "Infiltration capacity state should be HORTON_CAPSTAT_RECOVERY (2)")

   end subroutine test_horton_infiltration_recovering_dry
   !$f90tw)

   !> Initializes the Horton infiltration test suite with default values
   subroutine initialize_horton_test_suite(config, ierr, n, include_rain, time_step, inf_cap, waterdepth, rainfall, inf_cap_state) bind(C)
      ! Declare variables
      type(t_HortonInfiltrationConfig), intent(out) :: config
      integer, intent(out) :: ierr
      integer, intent(out) :: n !< [-] number of cells
      integer, intent(out) :: include_rain !< [-] flag to include rainfall (0/1)
      real(kind=dp), intent(out) :: time_step !< [s] time step
      real(kind=dp), dimension(:), allocatable, intent(out) :: inf_cap !< [m/s] infiltration capacity
      real(kind=dp), dimension(:), allocatable, intent(out) :: waterdepth !< [m] waterdepth
      real(kind=dp), dimension(:), allocatable, intent(out) :: rainfall !< [mm/day] rainfall
      integer, dimension(:), allocatable, intent(out) :: inf_cap_state !< [-] infiltration capacity state

      ! Initialize number of gridcells
      n = 1

      ! Initialize configuration
      allocate (config%max_inf_cap(n))
      allocate (config%min_inf_cap(n))
      allocate (config%decrease_rate(n))
      allocate (config%recovery_rate(n))
      config%max_inf_cap = 1.0_dp
      config%min_inf_cap = 0.5_dp
      config%decrease_rate = 0.5_dp
      config%recovery_rate = 0.5_dp

      ! Initialize error and state variables
      ierr = -1
      include_rain = 1
      time_step = 3600.0_dp
      allocate (inf_cap(n))
      allocate (waterdepth(n))
      allocate (rainfall(n))
      allocate (inf_cap_state(n))
      inf_cap = config%max_inf_cap(1) / MPS_TO_MMPHR ! Convert from mm/hr to m/s
      waterdepth = 0.0_dp
      rainfall = 0.0_dp
      inf_cap_state = -1

   end subroutine initialize_horton_test_suite

end module test_compute_horton_infiltration
