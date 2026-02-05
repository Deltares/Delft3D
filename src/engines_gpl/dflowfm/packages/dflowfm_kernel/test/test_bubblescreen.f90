module test_bubblescreen
    use assertions_gtest
    use precision, only: dp
    use precision_basics, only: comparereal
    use m_bubblescreen
    use m_alloc, only: realloc

    implicit none(type, external)

contains 

    !$f90tw TESTCODE(TEST, test_bubblescreen, test_convert_discharge_air_to_water_default, test_convert_discharge_air_to_water_default,
    !> Test conversion of air discharge to water discharge with default parameters
    subroutine test_convert_discharge_air_to_water_default() bind(C)
        ! Local variables
        real(kind=dp) :: air_discharge
        real(kind=dp) :: expected_water_discharge
        real(kind=dp) :: computed_water_discharge

        ! Setup
        air_discharge = 0.1_dp
        expected_water_discharge = (1000.0_dp * air_discharge) ** (2.0_dp / 3.0_dp)

        ! Call function to test
        computed_water_discharge = convert_discharge_air_to_water(air_discharge)

        ! Compare results
        call f90_expect_true(comparereal(computed_water_discharge, expected_water_discharge) == 0, "Converted water discharge should match expected value")

    end subroutine test_convert_discharge_air_to_water_default
    !$f90tw)

    !$f90tw TESTCODE(TEST, test_bubblescreen, test_convert_discharge_air_to_water_custom, test_convert_discharge_air_to_water_custom,
    !> Test conversion of air discharge to water discharge with custom parameters
    subroutine test_convert_discharge_air_to_water_custom() bind(C)
        ! Local variables
        real(kind=dp) :: air_discharge
        real(kind=dp) :: alpha
        real(kind=dp) :: expected_water_discharge
        real(kind=dp) :: computed_water_discharge

        ! Setup
        air_discharge = 0.1_dp
        alpha = 100.0_dp
        expected_water_discharge = (alpha * air_discharge) ** (2.0_dp / 3.0_dp)

        ! Call function to test
        computed_water_discharge = convert_discharge_air_to_water(air_discharge, alpha)

        ! Compare results
        call f90_expect_true(comparereal(computed_water_discharge, expected_water_discharge) == 0, "Converted water discharge should match expected value")

    end subroutine test_convert_discharge_air_to_water_custom
    !$f90tw)

    !$f90tw TESTCODE(TEST, test_bubblescreen, test_compute_bubblescreen_area, test_compute_bubblescreen_area,
    !> Test computation of bubble screen area based on flow cell areas
    subroutine test_compute_bubblescreen_area() bind(C)
        use fm_external_forcings_data, only: t_BubbleScreen
        use m_cell_geometry, only: ba

        ! Local variables
        type(t_BubbleScreen) :: bubblescreen !< Bubble screen data structure
        integer :: i !< Loop index
        integer :: n !< Number of flow cells in bubble screen
        real(kind=dp) :: expected_area !< Expected area of bubble screen
        real(kind=dp) :: computed_area !< Computed area of bubble screen

        ! Setup 
        n = 5
        expected_area = 10.0_dp * n

        ! Setup - create and fill bubble screen object with n flow cells
        bubblescreen%num_flow_cells = n
        allocate(bubblescreen%flow_cells(n))
        do i = 1, n
            bubblescreen%flow_cells(i)%cell_index = i
        end do

        ! Setup - fill cell areas for flow cells in bubble screen
        call realloc(ba, n, fill=10.0_dp)

        ! Call function to test
        computed_area = compute_bubblescreen_area(bubblescreen)

        ! Compare results
        call f90_expect_true(comparereal(computed_area, expected_area) == 0, "Computed bubble screen area should match expected value")

        ! Cleanup
        deallocate(ba)

    end subroutine test_compute_bubblescreen_area
    !$f90tw)

    !$f90tw TESTCODE(TEST, test_bubblescreen, test_find_active_layer_interfaces, test_find_active_layer_interfaces,
    !> Test active layer interfaces computation for a flow cell with 10 layers.
    !! The bubble screen is located at z = -7.8 m, the water level is at z = -0.4 m.
    !! The subroutine should return the correct start, stop, and max velocity layer interfaces.
    subroutine test_find_active_layer_interfaces() bind(C)
        use m_flow, only: kmx, zws, kbot, ktop, s1

        ! Local variables
        character(len=:), allocatable :: bubblescreen_id !< ID of bubble screen (not used in this test but required by function signature)
        integer :: flow_cell_index !< 2D flow cell index
        integer :: expected_k_start !< Expected starting layer interface for bubble screen
        integer :: expected_k_stop !< Expected stopping layer interface for bubble screen
        integer :: expected_k_max_velocity !< Expected layer interface of maximum velocity for bubble screen
        integer :: computed_k_start !< Computed starting layer interface for bubble screen
        integer :: computed_k_stop !< Computed stopping layer interface for bubble screen
        integer :: computed_k_max_velocity !< Computed layer interface of maximum velocity for bubble screen
        real(kind=dp) :: z_bubblescreen !< z coordinate of bubble screen

        ! Setup - inputs
        bubblescreen_id = "TestBubbleScreen"
        flow_cell_index = 1
        z_bubblescreen = -7.8_dp

        ! Setup - globals
        kmx = 10
        call realloc(kbot, 1, fill=2)
        call realloc(ktop, 1, fill=11)
        call realloc(s1, 1, fill=-0.4_dp) ! z_top = waterlevel = -0.4 m
        call realloc(zws, 11, fill=0.0_dp)
        zws(1:11) = [-10.0_dp, -9.0_dp, -8.0_dp, -7.0_dp, -6.0_dp, -5.0_dp, -4.0_dp, -3.0_dp, -2.0_dp, -1.0_dp, 0.0_dp] ! Layers are 1 m thick

        ! Setup - expected values
        expected_k_start = 3 ! z_bot = z_bubblescreen = -7.8 m => closest to interface with z = -8 m
        expected_k_stop = 11 ! z_top = -0.4 m => closest to interface with z = 0 m
        expected_k_max_velocity = 9 ! z_max_velocity = -1.88 m => closest to interface with z = -2 m

        ! Call function to test
        call find_active_layer_interfaces(flow_cell_index, z_bubblescreen, bubblescreen_id, computed_k_start, computed_k_stop, computed_k_max_velocity)

        ! Compare results
        call f90_expect_true(computed_k_start == expected_k_start, "Computed k_start should match expected value")
        call f90_expect_true(computed_k_stop == expected_k_stop, "Computed k_stop should match expected value")
        call f90_expect_true(computed_k_max_velocity == expected_k_max_velocity, "Computed k_max_velocity should match expected value")

        ! Cleanup
        deallocate(kbot)
        deallocate(ktop)
        deallocate(s1)
        deallocate(zws)

    end subroutine test_find_active_layer_interfaces
    !$f90tw)

    !$f90tw TESTCODE(TEST, test_bubblescreen, test_compute_water_discharge, test_compute_water_discharge,
    !> Test computation of water discharge for a flow cell with 10 layers
    subroutine test_compute_water_discharge() bind(C)
        use m_cell_geometry, only: ba
        use m_flow, only: kmx, zws, kbot

        ! Local variables
        character(len=5) :: str_i !< String for constructing error messages with layer index
        integer :: i !< Loop index
        integer :: flow_cell_index !< 2D flow cell index
        integer :: k_start !< Starting layer interface for bubble screen
        integer :: k_stop !< Stopping layer interface for bubble screen 
        integer :: k_max_velocity !< Layer interface of maximum velocity for bubble screen
        real(kind=dp) :: max_velocity !< Maximum velocity in bubble screen flow cell
        real(kind=dp), dimension(1, 10) :: expected_discharge !< Expected water discharge for each layer in flow cell (assuming 10 layers)
        real(kind=dp), dimension(1, 10) :: computed_discharge !< Computed water discharge for each layer in flow cell (assuming 10 layers)

        ! Setup - inputs
        flow_cell_index = 1
        k_start = 3
        k_stop = 11
        k_max_velocity = 9
        max_velocity = -0.6_dp

        ! Setup - globals
        kmx = 10
        call realloc(kbot, 1, fill=2)
        call realloc(ba, 1, fill=10.0_dp) ! Flow cell area = 10 m2
        call realloc(zws, 11, fill=0.0_dp)
        zws(1:11) = [-10.0_dp, -9.0_dp, -8.0_dp, -7.0_dp, -6.0_dp, -5.0_dp, -4.0_dp, -3.0_dp, -2.0_dp, -1.0_dp, 0.0_dp]

        ! Setup - expected values
        expected_discharge(1, 1:10) = [0.0_dp, 0.0_dp, -1.0_dp, -1.0_dp, -1.0_dp, -1.0_dp, -1.0_dp, -1.0_dp, 3.0_dp, 3.0_dp]

        ! Call function to test
        call compute_water_discharge(flow_cell_index, k_start, k_stop, k_max_velocity, max_velocity, computed_discharge)

        ! Compare results
        do i = 1, 10
            write(str_i, "(I0)") i
            call f90_expect_true(comparereal(computed_discharge(1, i), expected_discharge(1, i), eps=1.0e-7_dp) == 0, &
                "Computed discharge for layer "//trim(str_i)//" should match expected value")
        end do

        ! Cleanup
        deallocate(kbot)
        deallocate(ba)
        deallocate(zws)

    end subroutine test_compute_water_discharge
    !$f90tw)

    !$f90tw TESTCODE(TEST, test_bubblescreen, test_compute_constituent_discharge, test_compute_constituent_discharge,
    !> Test computation of constituent discharge for a flow cell with 10 layers and 1 constituent
    subroutine test_compute_constituent_discharge() bind(C)
        use m_cell_geometry, only: ba
        use m_flow, only: kmx, zws, kbot
        use m_transport, only: numconst, constituents

        ! Local variables
        character(len=5) :: str_i !< String for constructing error messages with layer index
        integer :: i !< Loop index
        integer :: flow_cell_index !< 2D flow cell index
        integer :: k_start !< Starting layer interface for bubble screen
        integer :: k_stop !< Stopping layer interface for bubble screen 
        integer :: k_max_velocity !< Layer interface of maximum velocity for bubble screen
        real(kind=dp), dimension(2, 10) :: expected_discharge
        real(kind=dp), dimension(2, 10) :: computed_discharge

        ! Setup - inputs
        flow_cell_index = 1
        k_start = 3
        k_stop = 11
        k_max_velocity = 9
        computed_discharge(1, 1:10) = [0.0_dp, 0.0_dp, -1.0_dp, -1.0_dp, -1.0_dp, -1.0_dp, -1.0_dp, -1.0_dp, 3.0_dp, 3.0_dp]

        ! Setup - globals
        kmx = 10
        numconst = 1
        call realloc(ba, 1, fill=10.0_dp)
        call realloc(zws, 11, fill=0.0_dp)
        call realloc(kbot, 1, fill=2)
        call realloc(constituents, [1, 11], fill=1.0_dp)
        zws(1:11) = [-10.0_dp, -9.0_dp, -8.0_dp, -7.0_dp, -6.0_dp, -5.0_dp, -4.0_dp, -3.0_dp, -2.0_dp, -1.0_dp, 0.0_dp]

        ! Setup - expected values
        expected_discharge(1, 1:10) = [0.0_dp, 0.0_dp, -1.0_dp, -1.0_dp, -1.0_dp, -1.0_dp, -1.0_dp, -1.0_dp, 3.0_dp, 3.0_dp]
        expected_discharge(2, 1:10) = [0.0_dp, 0.0_dp, -0.1_dp, -0.1_dp, -0.1_dp, -0.1_dp, -0.1_dp, -0.1_dp, 0.3_dp, 0.3_dp]

        ! Call function to test
        call compute_constituent_discharge(flow_cell_index, k_start, k_stop, k_max_velocity, computed_discharge)

        ! Compare results
        do i = 1, 10
            write(str_i, "(I0)") i
            call f90_expect_true(comparereal(computed_discharge(2, i), expected_discharge(2, i), eps=1.0e-7_dp) == 0, &
                "Computed constituent discharge for layer "//trim(str_i)//" should match expected value")
        end do

        ! Cleanup
        deallocate(ba)
        deallocate(zws)
        deallocate(kbot)
        deallocate(constituents)

    end subroutine test_compute_constituent_discharge
    !$f90tw)

end module test_bubblescreen
