module test_longculverts
    use assertions_gtest
    use m_longculverts, only: convert1D2DLongCulverts
    use m_network_helpers, only: t_grid_helper
    use m_file_helpers, only: generate_square_grid, cleanup_network_data    
    implicit none

contains
    function to_c_string(string) result(res)
        use iso_c_binding, only: c_null_char
        implicit none
        character(len=*), intent(in) :: string
        character(len=:), allocatable :: res
        res = trim(string) // c_null_char
    end function to_c_string
    

    !$f90tw TESTCODE(TEST, test_longculvert, test_convert1d2dlongculverts__single_four_point, test_convert1d2dlongculverts__single_four_point,
    subroutine test_convert1d2dlongculverts__single_four_point() bind(C)
        use precision, only: dp
        use network_data, only: numk, numl, kn
        use m_missing, only: dmiss
        use m_polygon, only: xpl, ypl, zpl, npl
        use m_longculverts, only: convert1D2DLongCulverts
        use m_longculverts_data, only: longculverts

        integer, parameter :: COORD_COUNT = 4
        type(t_grid_helper) :: grid_helper
        real(kind=dp) :: x_coords(COORD_COUNT)
        real(kind=dp) :: y_coords(COORD_COUNT)
        real(kind=dp) :: z_coords(COORD_COUNT)
        integer :: i

        ! Arrange
        grid_helper = t_grid_helper()
        call grid_helper%make_square_grid( &
            bottom_left_x=0.0_dp, bottom_left_y=0.0_dp, &
            rows=1, columns=2, side_length=10.0_dp, array_size_margin=16 &
        )

        x_coords = [5._dp, 9._dp, 11._dp, 15._dp]
        y_coords = [6._dp, 6._dp, 4._dp, 4._dp]
        z_coords = -1.0_dp

        ! Subroutine `longculvert_create_endpiont` requires these arrays in `m_polygon` to be allocated.
        xpl = x_coords
        ypl = y_coords
        zpl = z_coords
        npl = COORD_COUNT

        allocate(longculverts(1))
        allocate(longculverts(1)%netlinks(3))
        ! Act
        call convert1D2DLongCulverts(x_coords, y_coords, z_coords, COORD_COUNT)

        ! Assert
        call F90_ASSERT_DOUBLE_EQ(x_coords(1), 5._dp) ! First and last point snapped to cell centers.
        call F90_ASSERT_DOUBLE_EQ(y_coords(1), 5._dp)
        call F90_ASSERT_DOUBLE_EQ(x_coords(COORD_COUNT), 15._dp)
        call F90_ASSERT_DOUBLE_EQ(y_coords(COORD_COUNT), 5._dp)
        
        call F90_ASSERT_EQ(numk, 10) ! 6 Netnodes for the grid, 4 For the long culvert.
        call F90_ASSERT_EQ(numl, 10) ! 7 Netlinks for the grid, 3 For the long culvert.

        call F90_ASSERT_EQ(kn(3, longculverts(1)%netlinks(1)), 5, to_c_string("Expected first new link to be a 1D2D link."))
        call F90_ASSERT_EQ(kn(3, longculverts(1)%netlinks(2)), 1, to_c_string("Expected middle link to be a 1D link."))
        call F90_ASSERT_EQ(kn(3, longculverts(1)%netlinks(3)), 5, to_c_string("Expected last new link to be a 1D2D link."))
    end subroutine test_convert1d2dlongculverts__single_four_point
    !$f90tw )

    !$f90tw TESTCODE(TEST, test_longculvert, test_convert1d2dlongculverts__single_two_point, test_convert1d2dlongculverts__single_two_point,
    subroutine test_convert1d2dlongculverts__single_two_point() bind(C)
        use precision, only: dp
        use network_data, only: numk, numl, kn
        use m_missing, only: dmiss
        use m_polygon, only: xpl, ypl, zpl, npl
        use m_longculverts, only: convert1D2DLongCulverts
        use m_longculverts_data, only: longculverts

        implicit none

        integer, parameter :: COORD_COUNT = 2
        type(t_grid_helper) :: grid_helper
        real(kind=dp) :: x_coords(COORD_COUNT)
        real(kind=dp) :: y_coords(COORD_COUNT)
        real(kind=dp) :: z_coords(COORD_COUNT)
        integer :: i

        npl = 0
        ! Arrange
        grid_helper = t_grid_helper()
        call grid_helper%make_square_grid( &
            bottom_left_x=0.0_dp, bottom_left_y=0.0_dp, &
            rows=1, columns=2, side_length=10.0_dp, array_size_margin=16 &
        )

        x_coords = [5._dp, 15._dp]
        y_coords = [6._dp, 4._dp]
        z_coords = -1.0_dp

        ! `longculvert_create_endpiont` requires these arrays in `m_polygon` to be allocated.
        xpl = x_coords
        ypl = y_coords
        zpl = z_coords
        npl = COORD_COUNT
        if (allocated(longculverts)) then
            deallocate(longculverts)
        end if
        allocate(longculverts(1))
        allocate(longculverts(1)%netlinks(1))
        ! Act
        call convert1D2DLongCulverts(x_coords, y_coords, z_coords, COORD_COUNT)

        ! Assert
        call F90_ASSERT_DOUBLE_EQ(x_coords(1), 5._dp) ! First and last point snapped to cell centers.
        call F90_ASSERT_DOUBLE_EQ(y_coords(1), 5._dp)
        call F90_ASSERT_DOUBLE_EQ(x_coords(COORD_COUNT), 15._dp)
        call F90_ASSERT_DOUBLE_EQ(y_coords(COORD_COUNT), 5._dp)
        
        call F90_ASSERT_EQ(numk, 8) ! 6 Netnodes for the grid, 2 For the long culvert.
        call F90_ASSERT_EQ(numl, 8) ! 7 Netlinks for the grid, 1 For the long culvert.

        call F90_ASSERT_EQ(kn(3, longculverts(1)%netlinks(1)), 5, to_c_string("Expected first new link to be a 1D2D link."))
    end subroutine test_convert1d2dlongculverts__single_two_point
    !$f90tw )

    !$f90tw TESTCODE(TEST, test_longculvert, test_convert1d2dlongculverts__multiple_culverts, test_convert1d2dlongculverts__multiple_culverts,
    subroutine test_convert1d2dlongculverts__multiple_culverts() bind(C)
        use precision, only: dp
        use network_data, only: numk, numl, kn
        use m_missing, only: dmiss
        use m_polygon, only: xpl, ypl, zpl, npl
        use m_longculverts, only: convert1D2DLongCulverts
         use m_longculverts_data, only: longculverts
        use m_save_ugrid_state, only: meshgeom1d

        implicit none

        integer, parameter :: COORD_COUNT_LC1 = 4
        integer, parameter :: COORD_COUNT_LC2 = 2
        integer, parameter :: ARRAY_SIZE = COORD_COUNT_LC1 + COORD_COUNT_LC2 + 1
        type(t_grid_helper) :: grid_helper
        real(kind=dp) :: x_coords(ARRAY_SIZE)
        real(kind=dp) :: y_coords(ARRAY_SIZE)
        real(kind=dp) :: z_coords(ARRAY_SIZE)
        integer :: i

        npl = 0
        ! Arrange
        ! 2 x 2 grid.
        grid_helper = t_grid_helper()
        call grid_helper%make_square_grid( &
            bottom_left_x=0.0_dp, bottom_left_y=0.0_dp, &
            rows=2, columns=2, side_length=10.0_dp, array_size_margin=16 &
        )

        x_coords = [5._dp, 9._dp, 11._dp, 15._dp, dmiss, 5._dp, 15._dp]
        y_coords = [6._dp, 6._dp, 4._dp, 4._dp, dmiss, 16._dp, 14._dp]
        z_coords = -1.0_dp
        z_coords(5) = dmiss

        ! Subroutine `longculvert_create_endpiont` requires these arrays in `m_polygon` to be allocated.
        xpl = x_coords
        ypl = y_coords
        zpl = z_coords
        npl = ARRAY_SIZE

        !> ensure meshgeom1d state is disregarded
        meshgeom1d%numnode = -1 
        meshgeom1d%nnodes = -1

        if (allocated(longculverts)) then
            deallocate(longculverts)
        end if
        allocate(longculverts(2))
        allocate(longculverts(1)%netlinks(3))
        allocate(longculverts(2)%netlinks(1))

        ! Act
        call convert1D2DLongCulverts(x_coords, y_coords, z_coords, ARRAY_SIZE)

        ! Assert
        call F90_ASSERT_DOUBLE_EQ(x_coords(1), 5._dp) ! First and last point snapped to cell centers.
        call F90_ASSERT_DOUBLE_EQ(y_coords(1), 5._dp)
        call F90_ASSERT_DOUBLE_EQ(x_coords(COORD_COUNT_LC1), 15._dp)
        call F90_ASSERT_DOUBLE_EQ(y_coords(COORD_COUNT_LC1), 5._dp)
        call F90_ASSERT_DOUBLE_EQ(x_coords(COORD_COUNT_LC1 + 2), 5._dp) ! First and last point snapped to cell centers.
        call F90_ASSERT_DOUBLE_EQ(y_coords(COORD_COUNT_LC1 + 2), 15._dp)
        call F90_ASSERT_DOUBLE_EQ(x_coords(ARRAY_SIZE), 15._dp)
        call F90_ASSERT_DOUBLE_EQ(y_coords(ARRAY_SIZE), 15._dp)
        
        call F90_ASSERT_EQ(numk, 9 + 4 + 2) ! 9 Netnodes for the grid, 4 for LC1, 2 for LC2.
        call F90_ASSERT_EQ(numl, 12 + 3 + 1) ! 12 Netlinks for the grid, 3 for LC1, 1 for LC2.
    end subroutine test_convert1d2dlongculverts__multiple_culverts
    !$f90tw )

end module test_longculverts