module test_longculverts
    use assertions_gtest
    use m_longculverts, only: convert1D2DLongCulverts
    implicit none

contains
    function to_c_string(string) result(res)
        use iso_c_binding, only: c_null_char
        implicit none
        character(len=*), intent(in) :: string
        character(len=:), allocatable :: res
        res = trim(string) // c_null_char
    end function to_c_string

    !> Sets up minimal network_data with a single rectangular netcell
    !! centered at (center_x, center_y) with given side length.
    !! This is useful for testing routines like incells that depend on network_data.
    subroutine setup_single_rectangular_netcell(bottom_left_x, bottom_left_y, side_length, array_size_margin)
        use precision, only: dp
        use network_data, only: xk, yk, zk, kc, nmk, numk, kn, nump, nump1d2d, netcell, tface, lc, numl, xzw, yzw, nod, rnod, LINK_2D
        use m_cell_geometry, only: xz, yz, ndx
        use m_alloc, only: realloc
        use m_dimens, only: kmax, lmax
        use m_set_nod_adm, only: setnodadm
        use gridoperations, only: findcells
        implicit none
        
        real(kind=dp), intent(in) :: bottom_left_x !< X-coordinate of cell center
        real(kind=dp), intent(in) :: bottom_left_y !< Y-coordinate of cell center
        real(kind=dp), intent(in) :: side_length !< Side length of square cell
        integer, optional, intent(in) :: array_size_margin
        integer :: array_size_margin_
        integer :: istat
        integer :: i

        array_size_margin_ = 0
        if (present(array_size_margin)) then
            array_size_margin_ = array_size_margin
        end if
        
        ! Set up 4 net nodes for a rectangular cell
        numk = 6
        call realloc(xk, numk + array_size_margin_, stat=istat, fill=0.0_dp)
        call realloc(yk, numk + array_size_margin_, stat=istat, fill=0.0_dp)
        call realloc(zk, numk + array_size_margin_, stat=istat, fill=0.0_dp)
        call realloc(kc, numk + array_size_margin_, stat=istat, fill=1)
        call realloc(nmk, numk + array_size_margin_, stat=istat, fill=2)
        allocate(nod(numk + array_size_margin_))
        
        ! Define corners of rectangle (counter-clockwise from bottom-left)
        ! Node 1: bottom-left
        xk(1) = bottom_left_x
        yk(1) = bottom_left_y
        
        ! Node 2: bottom-middle
        xk(2) = bottom_left_x + side_length
        yk(2) = bottom_left_y
        
        ! Node 3: top-middle
        xk(3) = bottom_left_x + side_length
        yk(3) = bottom_left_y + side_length
        
        ! Node 4: top-left
        xk(4) = bottom_left_x
        yk(4) = bottom_left_y + side_length

        ! Node 5: bottom-right
        xk(5) = bottom_left_x + 2 * side_length
        yk(5) = bottom_left_y

        ! Node 6: top-right
        xk(6) = bottom_left_x + 2 * side_length
        yk(6) = bottom_left_y + side_length
        
        ! Set up 4 net links connecting the nodes
        numl = 7
        call realloc(kn, [3, numl + array_size_margin_], stat=istat, fill=0)
        call realloc(lc, numl + array_size_margin_, stat=istat, fill=1)
        
        kn(:, 1) = [1, 2, LINK_2D] ! Bottom-left edge
        kn(:, 2) = [2, 3, LINK_2D] ! Middle edge
        kn(:, 3) = [3, 4, LINK_2D] ! Top-left edge
        kn(:, 4) = [4, 1, LINK_2D] ! Left edge
        kn(:, 5) = [2, 5, LINK_2D] ! Bottom-right edge
        kn(:, 6) = [5, 6, LINK_2D] ! Right edge
        kn(:, 7) = [6, 3, LINK_2D] ! Top-right edge

        ! Initializes node, face and flow geometry stuff.
        call setnodadm(0)
        call findcells(0)
        
        kmax = 100
        lmax = 100
    end subroutine setup_single_rectangular_netcell
    
    !> Cleanup network_data arrays allocated by setup_single_rectangular_netcell
    subroutine cleanup_network_data()
        use network_data, only: xk, yk, zk, kc, nmk, numk, kn, nump, nump1d2d, netcell, tface, lc, numl, xzw, yzw, nod, rnod
        use m_cell_geometry, only: xz, yz, ndx
        implicit none
        
        integer :: i
        
        ! Deallocate node arrays
        if (allocated(xk)) then
            deallocate(xk)
        end if
        if (allocated(yk)) then
            deallocate(yk)
        end if
        if (allocated(zk)) then
            deallocate(zk)
        end if
        if (allocated(kc)) then
            deallocate(kc)
        end if
        if (allocated(nmk)) then
            deallocate(nmk)
        end if
        if (allocated(nod)) then
            ! Deallocate nod%lin arrays first
            do i = 1, size(nod)
                if (allocated(nod(i)%lin)) then
                    deallocate(nod(i)%lin)
                end if
            end do
            deallocate(nod)
        end if
        
        ! Deallocate link arrays
        if (allocated(kn)) then
            deallocate(kn)
        end if
        if (allocated(lc)) then
            deallocate(lc)
        end if
        if (allocated(rnod)) then
            deallocate(rnod)
        end if

        ! Deallocate cell arrays
        if (allocated(netcell)) then
            do i = 1, size(netcell)
                if (allocated(netcell(i)%nod)) then
                    deallocate(netcell(i)%nod)
                end if
                if (allocated(netcell(i)%lin)) then
                    deallocate(netcell(i)%lin)
                end if
            end do
            deallocate(netcell)
        end if
        
        if (allocated(xzw)) then
            deallocate(xzw)
        end if
        if (allocated(yzw)) then
            deallocate(yzw)
        end if
        
        ! Reset counters
        numk = 0
        numl = 0
        nump = 0
        nump1d2d = 0

        ! Reset flow administration
        if (allocated(xz)) then
            deallocate(xz)
        end if
        ndx = 0
    end subroutine cleanup_network_data

    !$f90tw TESTCODE(TEST, test_longculvert, test_convert1d2dlongculverts, test_convert1d2dlongculverts,
    subroutine test_convert1d2dlongculverts() bind(C)
        use precision, only: dp
        use m_polygon, only: xpl, ypl, zpl, npl
        use m_longculverts, only: convert1D2DLongCulverts
        implicit none

        integer, parameter :: COORD_COUNT = 9
        real(kind=dp) :: x_coords(COORD_COUNT + 1)
        real(kind=dp) :: y_coords(COORD_COUNT + 1)
        real(kind=dp) :: z_coords(COORD_COUNT + 1)
        integer :: links(COORD_COUNT + 1)
        integer :: i

        ! Arrange
        
        ! Single celled unit rectangle
        call setup_single_rectangular_netcell( &
            bottom_left_x=0.0_dp, bottom_left_y=0.0_dp, side_length=1.0_dp, &
            array_size_margin=16 &
        )

        x_coords(1:COORD_COUNT) = [(real(i + 5, kind=dp) / 10.0_dp, i=1, COORD_COUNT)]
        x_coords(COORD_COUNT+1) = 0.0_dp
        y_coords = 0.6_dp
        z_coords = 0.0_dp

        xpl = x_coords
        ypl = y_coords
        zpl = z_coords
        npl = COORD_COUNT

        ! Act
        call convert1D2DLongCulverts(x_coords, y_coords, z_coords, COORD_COUNT, links)

        ! Assert
        call cleanup_network_data()
    end subroutine test_convert1d2dlongculverts
    !$f90tw)

    !$f90tw TESTCODE(TEST, test_longculvert, test_longculvert_check_polyline__on_cell_center__shift_x_coord, test_longculvert_check_polyline__on_cell_center__shift_x_coord,
    subroutine test_longculvert_check_polyline__on_cell_center__shift_x_coord() bind(C)
        use precision, only: dp
        use m_cell_geometry, only: xz, yz
        use m_longculverts, only: longculvert_check_polyline
        implicit none

        real(kind=dp) :: x_coords(1)
        real(kind=dp) :: y_coords(1)
        real(kind=dp), parameter :: tolerance = 1e-8_dp

        ! Arrange
        call setup_single_rectangular_netcell(bottom_left_x=41.0_dp, bottom_left_y=41.0_dp, side_length=2.0_dp)
        x_coords(1) = 42_dp
        y_coords(1) = 42_dp

        ! Act
        call longculvert_check_polyline(1, y_coords, x_coords)
        
        ! Assert
        call f90_expect_near( &
            x_coords(1), 42.1_dp, tolerance, &
            to_c_string("The x coordinate has not been shifted away from the cell center by the expected amount.") &
        )

        call cleanup_network_data()
    end subroutine test_longculvert_check_polyline__on_cell_center__shift_x_coord
    !$f90tw)

    !$f90tw TESTCODE(TEST, test_longculvert, test_longculvert_check_polyline__off_cell_center, test_longculvert_check_polyline__off_cell_center,
    subroutine test_longculvert_check_polyline__off_cell_center() bind(C)
        use precision, only: dp
        use m_cell_geometry, only: xz, yz
        use m_longculverts, only: longculvert_check_polyline
        implicit none

        real(kind=dp) :: x_coords(1)
        real(kind=dp) :: y_coords(1)
        real(kind=dp), parameter :: tolerance = 1e-8_dp

        ! Arrange
        call setup_single_rectangular_netcell(bottom_left_x=41.0_dp, bottom_left_y=41.0_dp, side_length=2.0_dp)
        x_coords(1) = 42.0_dp
        y_coords(1) = 42.001_dp

        ! Act
        call longculvert_check_polyline(1, y_coords, x_coords)
        
        ! Assert
        call f90_expect_near( &
            x_coords(1), 42.0_dp, tolerance, &
            to_c_string("The x coordinate has been shifted away from the cell center.") &
        )

        call cleanup_network_data()
    end subroutine test_longculvert_check_polyline__off_cell_center
    !$f90tw)
end module test_longculverts