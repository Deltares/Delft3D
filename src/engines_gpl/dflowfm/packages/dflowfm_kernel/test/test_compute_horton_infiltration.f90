!> This module contains unit tests for the compute_horton_infiltration function in the horton module.
!! It verifies the correct behavior of infiltration capacity under various conditions:
!! 1) Return of HORTON_CAPSTATE_NOCHANGE when min_inf_cap > max_inf_cap.
!! 2) Infiltration capacity decrease due to rainfall exceeding minimum infiltration capacity.
!! 3) Infiltration capacity decrease due to presence of water level.
!! 4) Infiltration capacity recovery in dry conditions with rainfall below minimum infiltration capacity.
!! 5) Infiltration capacity recovery in dry conditions with no rainfall and no water level
module test_compute_horton_infiltration
    use assertions_gtest
    use precision, only: dp
    use m_horton

    implicit none

contains

    !> Test whether HORTON_CAPSTATE_NOCHANGE infiltration capacity state is returned when min_inf_cap > max_inf_cap
    subroutine test_horton_infiltration_nochange() bind(C)
        ! Declare variables
        type(t_HortonInfiltrationConfig) :: config
        integer :: ierr
        integer :: n  !< [-] number of cells
        integer :: include_rain  !< [-] flag to include rainfall
        real(kind=dp) :: time_step  !< [s] time step
        real(kind=dp), dimension(:), allocatable :: inf_cap !< [m/s] infiltration capacity
        real(kind=dp), dimension(:), allocatable :: waterlevel !< [m] water level
        real(kind=dp), dimension(:), allocatable :: rainfall !< [mm/day] rainfall
        integer, dimension(:), allocatable :: inc_cap_state !< [-] infiltration capacity state

        ! Initialize configuration
        config%max_inf_cap = 1.0_dp
        config%min_inf_cap = 2.0_dp
        config%decrease_rate = 0.5_dp
        config%recovery_rate = 0.5_dp

        ! Initialize error and state variables
        ierr = -1
        n = 1
        include_rain = 1
        time_step = 1.0_dp
        allocate(inf_cap(n))
        allocate(waterlevel(n))
        allocate(rainfall(n))
        allocate(inc_cap_state(n))
        inf_cap = config%max_inf_cap
        waterlevel = 0.0_dp
        rainfall = 20.0_dp
        inf_cap_state = -1

        ! Compute horton infiltration
        ierr = compute_horton_infiltration(config, n, include_rain, time_step, inf_cap, waterlevel, rainfall, inc_cap_state)

        ! Compare results
        call f90_expect_eq(inc_cap_state(1), HORTON_CAPSTAT_NOCHANGE, "Infiltration capacity state should be HORTON_CAPSTAT_NOCHANGE (0)")
    
        ! Cleanup
        deallocate(config, ierr, n, include_rain, time_step, inf_cap, waterlevel, rainfall, inc_cap_state)
        
    end subroutine test_horton_infiltration_nochange

    !> Test decreasing infiltration capacity in wet conditions (rainfall > min_inf_cap, no water level)
    subroutine test_horton_infiltration_decreasing_rain() bind(C)
        ! Declare variables
        type(t_HortonInfiltrationConfig) :: config 
        integer :: ierr
        integer :: n  !< [-] number of cells
        integer :: include_rain  !< [-] flag to include rainfall
        real(kind=dp) :: time_step  !< [s] time step
        real(kind=dp) :: analytical_result 
        real(kind=dp), dimension(:), allocatable :: inf_cap !< [m/s] infiltration capacity
        real(kind=dp), dimension(:), allocatable :: waterlevel !< [m] water level
        real(kind=dp), dimension(:), allocatable :: rainfall !< [mm/day] rainfall
        integer, dimension(:), allocatable :: inc_cap_state !< [-] infiltration capacity state

        ! Initialize configuration
        config%max_inf_cap = 1.0_dp
        config%min_inf_cap = 0.5_dp
        config%decrease_rate = 0.5_dp
        config%recovery_rate = 0.5_dp

        ! Initialize error and state variables
        ierr = -1
        n = 1
        include_rain = 1
        time_step = 1.0_dp
        allocate(inf_cap(n))
        allocate(waterlevel(n))
        allocate(rainfall(n))
        allocate(inc_cap_state(n))
        inf_cap = config%max_inf_cap
        waterlevel = 0.0_dp ! No water on the cell
        rainfall = 20.0_dp ! Rainfall should be higher than minimum infiltration capacity to trigger 
                           ! Keep in mind: rainfall is in [mm/day] while minimum infiltration capacity is in [mm/hr]
        inf_cap_state = -1

        ! Compute horton infiltration
        ierr = compute_horton_infiltration(config, n, include_rain, time_step, inf_cap, waterlevel, rainfall, inc_cap_state)

        ! Compute analytical result
        analytical_result = config%min_inf_cap + (config%max_inf_cap - config%min_inf_cap) * exp(-config%decrease_rate * time_step)

        ! Compare results
        call f90_expect_eq(inf_cap(1), analytical_result, "Infiltration capacity does not match expected value")
        call f90_expect_eq(inc_cap_state(1), HORTON_CAPSTAT_DECREASING, "Infiltration capacity state should be HORTON_CAPSTAT_DECREASING (1)")
        
        ! Cleanup
        deallocate(config, ierr, n, include_rain, time_step, inf_cap, waterlevel, rainfall, inc_cap_state, analytical_result)
        
    end subroutine test_horton_infiltration_decreasing_rain

    !> Test decreasing infiltration capacity in wet conditions (no rainfall, but water level present)
    subroutine test_horton_infiltration_decreasing_waterlevel() bind(C)
        ! Declare variables
        type(t_HortonInfiltrationConfig) :: config
        integer :: ierr
        integer :: n  !< [-] number of cells
        integer :: include_rain  !< [-] flag to include rainfall
        real(kind=dp) :: time_step  !< [s] time step
        real(kind=dp) :: analytical_result 
        real(kind=dp), dimension(:), allocatable :: inf_cap !< [m/s] infiltration capacity
        real(kind=dp), dimension(:), allocatable :: waterlevel !< [m] water level
        real(kind=dp), dimension(:), allocatable :: rainfall !< [mm/day] rainfall
        integer, dimension(:), allocatable :: inc_cap_state !< [-] infiltration capacity state

        ! Initialize configuration
        config%max_inf_cap = 1.0_dp
        config%min_inf_cap = 0.5_dp
        config%decrease_rate = 0.5_dp
        config%recovery_rate = 0.5_dp

        ! Initialize error and state variables
        ierr = -1
        n = 1
        include_rain = 1
        time_step = 1.0_dp
        allocate(inf_cap(n))
        allocate(waterlevel(n))
        allocate(rainfall(n))
        allocate(inc_cap_state(n))
        inf_cap = config%max_inf_cap
        waterlevel = 1.0_dp ! Water is standing on the cell
        rainfall = 0.0_dp ! No rainfall
        inf_cap_state = -1

        ! Compute horton infiltration
        ierr = compute_horton_infiltration(config, n, include_rain, time_step, inf_cap, waterlevel, rainfall, inc_cap_state)

        ! Compute analytical result
        analytical_result = config%min_inf_cap + (config%max_inf_cap - config%min_inf_cap) * exp(-config%decrease_rate * time_step)

        ! Compare results
        call f90_expect_eq(inf_cap(1), analytical_result, "Infiltration capacity does not match expected value")
        call f90_expect_eq(inc_cap_state(1), HORTON_CAPSTAT_DECREASING, "Infiltration capacity state should be HORTON_CAPSTAT_DECREASING (1)")
        
        ! Cleanup
        deallocate(config, ierr, n, include_rain, time_step, inf_cap, waterlevel, rainfall, inc_cap_state, analytical_result)
        
    end subroutine test_horton_infiltration_decreasing_waterlevel

    !> Test recovering infiltration capacity in dry conditions (rainfall < min_inf_cap and no water level)
    subroutine test_horton_infiltration_recovering_rain() bind(C)
        ! Declare variables
        type(t_HortonInfiltrationConfig) :: config
        integer :: ierr
        integer :: n  !< [-] number of cells
        integer :: include_rain  !< [-] flag to include rainfall
        real(kind=dp) :: time_step  !< [s] time step
        real(kind=dp) :: analytical_result 
        real(kind=dp), dimension(:), allocatable :: inf_cap !< [m/s] infiltration capacity
        real(kind=dp), dimension(:), allocatable :: waterlevel !< [m] water level
        real(kind=dp), dimension(:), allocatable :: rainfall !< [mm/day] rainfall
        integer, dimension(:), allocatable :: inc_cap_state !< [-] infiltration capacity state

        ! Initialize configuration
        config%max_inf_cap = 1.0_dp
        config%min_inf_cap = 0.5_dp
        config%decrease_rate = 0.5_dp
        config%recovery_rate = 0.5_dp

        ! Initialize error and state variables
        ierr = -1
        n = 1
        include_rain = 1
        time_step = 1.0_dp
        allocate(inf_cap(n))
        allocate(waterlevel(n))
        allocate(rainfall(n))
        allocate(inc_cap_state(n))
        inf_cap = config%min_inf_cap
        waterlevel = 0.0_dp ! No water on the cell
        rainfall = 10.0_dp ! Rainfall is below the minimum infiltration capacity to trigger recovery
                           ! Keep in mind: rainfall is in [mm/day] while minimum infiltration capacity is in [mm/hr]
        inf_cap_state = -1

        ! Compute horton infiltration
        ierr = compute_horton_infiltration(config, n, include_rain, time_step, inf_cap, waterlevel, rainfall, inc_cap_state)

        ! Compute analytical result
        analytical_result = config%max_inf_cap - (config%max_inf_cap - config%min_inf_cap) * exp(-config%recovery_rate * time_step)

        ! Compare results
        call f90_expect_eq(inf_cap(1), analytical_result, "Infiltration capacity does not match expected value")
        call f90_expect_eq(inc_cap_state(1), HORTON_CAPSTAT_RECOVERING, "Infiltration capacity state should be HORTON_CAPSTAT_RECOVERING (2)")

        ! Cleanup
        deallocate(config, ierr, n, include_rain, time_step, inf_cap, waterlevel, rainfall, inc_cap_state, analytical_result)

    end subroutine test_horton_infiltration_recovering_rain

    !> Test recovering infiltration capacity in dry conditions (no rainfall and no water level)
    subroutine test_horton_infiltration_recovering_dry() bind(C)
        ! Declare variables
        type(t_HortonInfiltrationConfig) :: config
        integer :: ierr
        integer :: n  !< [-] number of cells
        integer :: include_rain  !< [-] flag to include rainfall
        real(kind=dp) :: time_step  !< [s] time step
        real(kind=dp) :: analytical_result 
        real(kind=dp), dimension(:), allocatable :: inf_cap !< [m/s] infiltration capacity
        real(kind=dp), dimension(:), allocatable :: waterlevel !< [m] water level
        real(kind=dp), dimension(:), allocatable :: rainfall !< [mm/day] rainfall
        integer, dimension(:), allocatable :: inc_cap_state !< [-] infiltration capacity state

        ! Initialize configuration
        config%max_inf_cap = 1.0_dp
        config%min_inf_cap = 0.5_dp
        config%decrease_rate = 0.5_dp
        config%recovery_rate = 0.5_dp

        ! Initialize error and state variables
        ierr = -1
        n = 1
        include_rain = 1
        time_step = 1.0_dp
        allocate(inf_cap(n))
        allocate(waterlevel(n))
        allocate(rainfall(n))
        allocate(inc_cap_state(n))
        inf_cap = config%min_inf_cap
        waterlevel = 0.0_dp ! No water on the cell
        rainfall = 0.0_dp ! No rainfall
        inf_cap_state = -1

        ! Compute horton infiltration
        ierr = compute_horton_infiltration(config, n, include_rain, time_step, inf_cap, waterlevel, rainfall, inc_cap_state)

        ! Compute analytical result
        analytical_result = config%max_inf_cap - (config%max_inf_cap - config%min_inf_cap) * exp(-config%recovery_rate * time_step)

        ! Compare results
        call f90_expect_eq(inf_cap(1), analytical_result, "Infiltration capacity does not match expected value after recovery")
        call f90_expect_eq(inc_cap_state(1), HORTON_CAPSTAT_RECOVERING, "Infiltration capacity state should be HORTON_CAPSTAT_RECOVERING (2)")

        ! Cleanup
        deallocate(config, ierr, n, include_rain, time_step, inf_cap, waterlevel, rainfall, inc_cap_state, analytical_result)

    end subroutine test_horton_infiltration_recovering_dry

end module test_compute_horton_infiltration