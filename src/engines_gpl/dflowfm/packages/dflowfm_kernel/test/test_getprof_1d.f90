module m_test_getprof_1d
   use assertions_gtest
   use precision, only: dp
   use m_missing, only: dmiss
   use m_get_prof_1D

   implicit none
contains

    !> Initialize a simple circular cross section for testing
    subroutine setup_circular_cross_section(network, diameter, friction_type, friction_value)
        use m_network
        use m_CrossSections, only: AddRoundCrossSectionDefinition
        use m_GlobalParameters, only: CS_CIRCLE
        
        type(t_network), intent(inout) :: network
        real(kind=dp), intent(in) :: diameter
        integer, intent(in) :: friction_type
        real(kind=dp), intent(in) :: friction_value
        
        integer :: idef, icrs
        type(t_CrossSection), pointer :: pcrs
        type(t_CSType), pointer :: pcsDef
        
        ! Add a cross section definition (the geometry)
        idef = AddRoundCrossSectionDefinition(
            network%CSDefinitions, id='test_circle', &
            diameter=diameter, shape=CS_CIRCLE, & 
            groundLayerUsed=.false., groundLayer=0.0_dp &
        )
        
        ! Allocate space for one cross section
        call realloc(network%crs)
        network%crs%count = 1
        
        ! Link the cross section to the definition
        icrs = 1
        pcrs => network%crs%cross(icrs)
        pcsDef => network%CSDefinitions%cs(idef)
        
        pcrs%csid = 'test_circle'
        pcrs%iTabDef = idef
        pcrs%tabDef => pcsDef
        pcrs%shift = 0.0_dp  ! bed level adjustment
        
        ! Set up friction parameters
        pcrs%frictionSectionsCount = 1
        allocate(pcrs%frictionSectionID(1))
        allocate(pcrs%frictionSectionFrom(1))
        allocate(pcrs%frictionSectionTo(1))
        allocate(pcrs%frictionTypePos(1))
        allocate(pcrs%frictionValuePos(1))
        allocate(pcrs%frictionTypeNeg(1))
        allocate(pcrs%frictionValueNeg(1))
        
        pcrs%frictionSectionID(1) = 'main'
        pcrs%frictionSectionFrom(1) = 0.0_dp
        pcrs%frictionSectionTo(1) = diameter
        pcrs%frictionTypePos(1) = friction_type  ! 1=Chezy, 2=Manning, etc.
        pcrs%frictionValuePos(1) = friction_value
        pcrs%frictionTypeNeg(1) = friction_type
        pcrs%frictionValueNeg(1) = friction_value
        
        ! Finalize the cross section setup
        call SetParsCross(pcsDef, pcrs)
        
    end subroutine setup_circular_cross_section

    !> Sets up minimal network_data with a single rectangular netcell
    !! centered at (center_x, center_y) with given side length.
    !! This is useful for testing routines like incells that depend on network_data.
    subroutine generate_square_grid(bottom_left_x, bottom_left_y, side_length, rows, columns, array_size_margin)
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
        integer, intent(in) :: rows !< Number of rows
        integer, intent(in) :: columns !< Number of columns
        integer, optional, intent(in) :: array_size_margin
        integer :: array_size_margin_
        integer :: istat
        integer :: i, row, col, link_index, bottom_left_node_index, up_node_index, right_node_index, up_right_node_index

        array_size_margin_ = 0
        if (present(array_size_margin)) then
            array_size_margin_ = array_size_margin
        end if
        
        ! Set up 4 net nodes for a rectangular cell
        numk = (rows + 1) * (columns + 1)
        call realloc(xk, numk + array_size_margin_, stat=istat, fill=0.0_dp)
        call realloc(yk, numk + array_size_margin_, stat=istat, fill=0.0_dp)
        call realloc(zk, numk + array_size_margin_, stat=istat, fill=0.0_dp)
        call realloc(kc, numk + array_size_margin_, stat=istat, fill=1)
        call realloc(nmk, numk + array_size_margin_, stat=istat, fill=2)
        allocate(nod(numk + array_size_margin_))

        ! Place (rows+1)x(columns+1) grid nodes (the cell corners).
        do row = 0, rows
            do col = 0, columns
                xk(row * (columns + 1) + col + 1) = bottom_left_x + col * side_length
                yk(row * (columns + 1) + col + 1) = bottom_left_y + row * side_length
            end do
        end do
 
        ! Place links between nodes
        numl = 2 * rows * columns + rows + columns
        call realloc(kn, [3, numl + array_size_margin_], stat=istat, fill=0)
        call realloc(lc, numl + array_size_margin_, stat=istat, fill=1)
        link_index = 1
        do row = 0, rows - 1
            do col = 0, columns - 1
                bottom_left_node_index = row * (columns + 1) + col + 1
                right_node_index = bottom_left_node_index + 1
                up_node_index = bottom_left_node_index + columns + 1
                up_right_node_index = bottom_left_node_index + columns + 2

                if (row == 0) then
                    kn(:, link_index) = [bottom_left_node_index, right_node_index, LINK_2D]
                    link_index = link_index + 1
                end if
                kn(:, link_index) = [right_node_index, up_right_node_index, LINK_2D]
                kn(:, link_index + 1) = [up_right_node_index, up_node_index, LINK_2D]
                link_index = link_index + 2
                if (col == 0) then
                    kn(:, link_index) = [up_node_index, bottom_left_node_index, LINK_2D]
                    link_index = link_index + 1
                end if
            end do
        end do

        ! Initializes node, face and flow geometry stuff.
        ! call setnodadm(0)
        call findcells(0)
        
        kmax = 100
        lmax = 100
    end subroutine generate_square_grid
    
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

    !$f90tw TESTCODE(TEST, test_getprof_1d, test_getprof_1d, test_getprof_1d,
    subroutine test_getprof_1d() bind(C)
        use m_flowgeom
        use m_flow_geominit, only: flow_geominit
        use network_data
        use Timers, only: timini, timon
        use m_partitioninfo, only: jampi
        use gridoperations, only: incells, setnewpoint, connectdbn, findcells
        use m_network, only: initialize_1dadmin
        use unstruc_channel_flow, only: network
        implicit none

        integer :: left_center, right_center, left_node, right_node, new_link
        real(kind=dp) :: area, width

        ! Initialize timers (required by flow_geominit)
        call timini()
        timon = .false.  ! Keep timers disabled for testing
        
        ! Disable MPI (required to avoid MPI calls in test)
        jampi = 0

        call generate_square_grid( &
            bottom_left_x=0.0_dp, bottom_left_y=0.0_dp, side_length=10.0_dp, &
            rows=1, columns=2, array_size_margin=2 &
        )
        
        call incells(5.0_dp, 5.0_dp, left_center)
        call incells(15.0_dp, 5.0_dp, right_center)
        if (left_center == 0 .or. right_center == 0) then
            call F90_ASSERT_FALSE(.true., "Failed to find cell centers." // c_null_char)
            return
        end if
        
        call setnewpoint(xzw(left_center), yzw(left_center), 0.0_dp, left_node)
        call setnewpoint(xzw(right_center), yzw(right_center), 0.0_dp, right_node)
        
        kn3typ = 5
        call connectdbn(left_node, right_node, new_link)
        call flow_geominit(0)
        call initialize_1dadmin(network, network%numl, numl)
        network%loaded = .true.
        

        call getprof_1D(1, 0.0_dp, area, width, 0, 0, 0.0_dp)

        call cleanup_network_data()

    end subroutine test_getprof_1d
    !$f90tw )

end module m_test_getprof_1d

