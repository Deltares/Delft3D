module m_test_getprof_1d
   use assertions_gtest
   use precision, only: dp
   use precision_basics, only: comparereal
   use m_missing, only: dmiss
   use m_get_prof_1D
   use m_alloc, only: realloc
   use m_network_helpers, only: t_grid_helper

   implicit none
contains

    subroutine disable_timers_logging_and_mpi()
        use Timers, only: timini, timon
        use m_partitioninfo, only: jampi
        use MessageHandling, only: SetMessageHandling

        call timini() ! Initialize timers (otherwise `flow_geominit` crashes)
        timon = .false. ! Disable timers because we're running unit tests.
        jampi = 0 ! Disable MPI because we're running unit tests.
        call SetMessageHandling(write2screen=.false.) ! Disable logging.
    end subroutine

    !> Initialize a simple rectangular cross section for testing
    subroutine setup_network_rectangular_cross_section(network, width, height, friction_type, friction_value)
        use m_network
        use m_CrossSections, only: AddCrossSectionDefinition, CS_RECTANGLE
        use m_roughness, only: R_MANNING
        
        type(t_network), intent(inout) :: network
        real(kind=dp), intent(in) :: width
        real(kind=dp), intent(in) :: height
        integer, optional, intent(in) :: friction_type
        real(kind=dp), optional, intent(in) :: friction_value
        
        integer :: idef, icrs
        type(t_CrossSection), pointer :: pcrs
        type(t_CSType), pointer :: pcsDef
        
        integer, parameter :: numLevels = 2
        real(kind=dp), allocatable :: level(:), flowWidth(:), totalWidth(:)
        real(kind=dp) :: plains(3), crestLevel, baseLevel, flowArea, totalArea
        
        allocate(level(numLevels), flowWidth(numLevels), totalWidth(numLevels))
        level(:) = [0.0_dp, height]
        flowWidth(:) = width
        totalWidth(:) = width
        
        plains = 0.0_dp
        crestLevel = 0.0_dp
        baseLevel = 0.0_dp
        flowArea = 0.0_dp
        totalArea = 0.0_dp
        
        ! Add a cross section definition (the geometry)
        idef = AddCrossSectionDefinition( &
            network%CSDefinitions, &
            'rect_closed', &         ! id
            numLevels, &             ! 2 levels
            level, &
            flowWidth, &
            totalWidth, &
            plains, &
            crestLevel, &
            baseLevel, &
            flowArea, &
            totalArea, &
            .true., &                ! closed = true
            .false., 0.0_dp &
        )
        
        ! Allocate space for one cross section
        call realloc(network%crs)
        network%crs%count = 1
        
        ! Link the cross section to the definition
        icrs = 1
        pcrs => network%crs%cross(icrs)
        pcsDef => network%CSDefinitions%cs(idef)
        
        pcrs%csid = 'rect_closed'
        pcrs%iTabDef = idef
        pcrs%tabDef => pcsDef
        pcrs%shift = 0.0_dp  ! bed level adjustment
        
        ! Set up friction parameters
        if (present(friction_type) .and. present(friction_value)) then
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
            pcrs%frictionSectionTo(1) = width  ! For rectangles, this is the channel width
            pcrs%frictionTypePos(1) = friction_type  ! 1=Chezy, 2=Manning, etc.
            pcrs%frictionValuePos(1) = friction_value
            pcrs%frictionTypeNeg(1) = friction_type
            pcrs%frictionValueNeg(1) = friction_value
        end if
        
        ! Finalize the cross section setup
        call SetParsCross(pcsDef, pcrs)
        
        deallocate(level, flowWidth, totalWidth)
    end subroutine setup_network_rectangular_cross_section

    !> Set up a closed rectangular cross section using prof1D and profiles1D arrays
    !! This sets up a direct profile (not interpolated) for link L=1
    !! Profile type 2 (closed rectangular): width x height
    subroutine setup_prof1d_rectangular_cross_section_with_profile(width, height, friction_type, friction_value)
        use m_flow, only: frcu, ifrcutp
        use m_flowgeom, only: prof1D, lnx1D, lnx
        use m_profiles, only: profiles1D, nprofdefs
        use m_roughness, only: R_MANNING
        use m_missing, only: DMISS
        use m_alloc, only: realloc

        real(kind=dp), intent(in) :: width
        real(kind=dp), intent(in) :: height
        integer, optional, intent(in) :: friction_type
        real(kind=dp), optional, intent(in) :: friction_value
        
        integer :: ierr
        integer :: friction_type_
        real(kind=dp) :: friction_value_

        friction_type_ = DMISS
        if (present(friction_type)) then
            friction_type_ = friction_type
        end if

        friction_value_ = DMISS
        if (present(friction_value)) then
            friction_value_ = friction_value
        end if
        
        ! Allocate prof1D array for at least 1 link
        if (.not. allocated(prof1D)) then
            lnx1D = 1
            allocate(prof1D(3, lnx1D), stat=ierr)
        end if
        
        ! Set up direct profile mode (positive values in prof1D(1:2, L))
        ! For a closed rectangular profile:
        prof1D(1, 1) = -1 ! Negative index in `profiles1D` array
        prof1D(2, 1) = -1 ! Refers to the same profile, so skip interpolation.
        prof1D(3, 1) = 1.0_dp ! In this case this is the fraction used for interpolation.
        
        ! Optionally, also set up profiles1D array for friction parameters
        ! This is used when prof1D(1,L) < 0 (interpolation mode)
        ! But we can still allocate it for completeness
        if (.not. allocated(profiles1D)) then
            nprofdefs = 1
            allocate(profiles1D(nprofdefs), stat=ierr)
            
            profiles1D(1)%ityp = -2          ! -2 = closed rectangular
            profiles1D(1)%width = width
            profiles1D(1)%height = height
            profiles1D(1)%frctp = friction_type_
            profiles1D(1)%frccf = friction_value_
            profiles1D(1)%zmin = 0.0_dp
        end if

        call realloc(frcu, lnx, fill=DMISS)
        call realloc(ifrcutp, lnx, fill=0)
    end subroutine setup_prof1d_rectangular_cross_section_with_profile

    subroutine setup_prof1d_rectangular_cross_section_without_profile(width, height, friction_type, friction_value)
        use m_flow, only: frcu, ifrcutp
        use m_flowgeom, only: prof1D, lnx1D, lnx
        use m_profiles, only: profiles1D, nprofdefs
        use m_roughness, only: R_MANNING
        use m_missing, only: DMISS

        real(kind=dp), intent(in) :: width
        real(kind=dp), intent(in) :: height
        integer, optional, intent(in) :: friction_type
        real(kind=dp), optional, intent(in) :: friction_value
        
        integer :: ierr
        integer :: friction_type_
        real(kind=dp) :: friction_value_

        friction_type_ = DMISS
        if (present(friction_type)) then
            friction_type_ = friction_type
        end if

        friction_value_ = DMISS
        if (present(friction_value)) then
            friction_value_ = friction_value
        end if
        
        ! Allocate prof1D array for at least 1 link
        if (.not. allocated(prof1D)) then
            lnx1D = 1
            allocate(prof1D(3, lnx1D), stat=ierr)
        end if
        
        ! Set up direct profile mode (positive values in prof1D(1:2, L))
        ! For a closed rectangular profile:
        prof1D(1, 1) = width      ! Profile width (m)
        prof1D(2, 1) = height     ! Profile height (m)
        prof1D(3, 1) = -2.0_dp    ! Profile type: -2 = closed rectangular
                                  ! Negative means "closed" profile
        
        call realloc(frcu, lnx, fill=friction_value_)
        call realloc(ifrcutp, lnx, fill=friction_type_)
    end subroutine setup_prof1d_rectangular_cross_section_without_profile

    subroutine place_2d2d_link(p, q, new_link, error_code)
        use gridoperations, only: incells, setnewpoint, connectdbn
        use network_data, only: kn3typ, xzw, yzw

        real(kind=dp), intent(in) :: p(2), q(2)
        integer, intent(out) :: new_link
        integer, intent(out) :: error_code

        integer :: p_netcell, q_netcell, p_centernode, q_centernode

        error_code = 0
        call incells(p(1), p(2), p_netcell)
        call incells(q(1), q(2), q_netcell)
        if (p_netcell == 0 .or. q_netcell == 0) then
            error_code = 1
            return
        end if
        
        call setnewpoint(xzw(p_netcell), yzw(p_netcell), 0.0_dp, p_centernode)
        call setnewpoint(xzw(q_netcell), yzw(q_netcell), 0.0_dp, q_centernode)
        
        kn3typ = 5  ! Used in `connectdbn` to set the link type of the new link. This type is for 1D2D/2D2D links.
        call connectdbn(p_centernode, q_centernode, new_link)
    end subroutine place_2d2d_link

    !$f90tw TESTCODE(TEST, test_getprof_1d, test_getprof_1d__network1d, test_getprof_1d__network1d,
    subroutine test_getprof_1d__network1d() bind(C)
        use network_data, only: numl
        use m_flow_geominit, only: flow_geominit
        use m_network, only: initialize_1dadmin
        use unstruc_channel_flow, only: network, default_channel_flow

        type(t_grid_helper) :: grid_helper
        integer, parameter :: japerim = 1
        integer, parameter :: calcconv = 0
        integer :: i, new_link, error_code
        real(kind=dp) :: area, width, perim
        
        call disable_timers_logging_and_mpi()
        grid_helper = t_grid_helper()
        call grid_helper%make_square_grid( &
            bottom_left_x=0.0_dp, bottom_left_y=0.0_dp, side_length=10.0_dp, &
            rows=1, columns=2, array_size_margin=2 &
        )
        call place_2d2d_link([5.0_dp, 5.0_dp], [15.0_dp, 5.0_dp], new_link=new_link, error_code=error_code)
        call f90_assert_eq(error_code, 0, "Failed to place 2D2D link" // c_null_char)
        
        call flow_geominit(0)

        call default_channel_flow()
        call initialize_1dadmin(network, network%numl, numl)
        call setup_network_rectangular_cross_section(network, width=2.0_dp, height=1.5_dp)

        allocate(network%adm%line2cross(1, 3))
        do i = 1, 3
            network%adm%line2cross(1, i)%c1 = 1
            network%adm%line2cross(1, i)%c2 = 1
            network%adm%line2cross(1, i)%f = 1.0_dp
        end do
        network%loaded = .true.
        
        call getprof_1D(1, 1.0_dp, area, width, japerim, calcconv, perim)

        ! We have a rectangular cross section 2 units wide. The water is 1 unit deep.
        ! Surface water width: 2 (Same as rectangular cross section width)
        ! Water cross section area: 2x1 = 2
        ! Wet perimeter: 1+2+1 = 4
        call f90_assert_near(width, 2.0_dp, 1e-7_dp, "Unexpected width result" // c_null_char)
        call f90_assert_near(area, 2.0_dp, 1e-7_dp, "Unexpected area result" // c_null_char)
        call f90_assert_near(perim, 4.0_dp, 1e-7_dp, "Unexpected perim result" // c_null_char)
    end subroutine test_getprof_1d__network1d
    !$f90tw )

    !$f90tw TESTCODE(TEST, test_getprof_1d, test_getprof_1d__network1d__friction, test_getprof_1d__network1d__friction,
    subroutine test_getprof_1d__network1d__friction() bind(C)
        use network_data, only: numl
        use m_flow, only: u1, q1, hu, cfuhi, frcu, frcu_mor, u_to_umain, q1_main
        use m_flowgeom, only: lnx
        use m_flow_geominit, only: flow_geominit
        use m_network, only: initialize_1dadmin
        use m_physcoef, only: default_physcoef, ag
        use unstruc_channel_flow, only: network, default_channel_flow
        use m_Roughness, only: R_MANNING
        use m_get_chezy, only: get_chezy

        type(t_grid_helper) :: grid_helper
        integer, parameter :: japerim = 1
        integer, parameter :: calcconv = 1
        integer :: i, new_link, error_code
        real(kind=dp) :: area, width, perim, hydrad, chezy

        call disable_timers_logging_and_mpi()
        call default_physcoef()
        grid_helper = t_grid_helper()
        call grid_helper%make_square_grid( &
            bottom_left_x=0.0_dp, bottom_left_y=0.0_dp, side_length=10.0_dp, &
            rows=1, columns=2, array_size_margin=2 &
        )
        call place_2d2d_link([5.0_dp, 5.0_dp], [15.0_dp, 5.0_dp], new_link=new_link, error_code=error_code)
        call f90_assert_eq(error_code, 0, "Failed to place 2D2D link" // c_null_char)
        
        call flow_geominit(0)

        call realloc(u1, lnx, fill=1.0_dp)
        call realloc(q1, lnx, fill=1.0_dp)
        call realloc(hu, lnx, fill=1.0_dp)
        call realloc(cfuhi, lnx)
        call realloc(u_to_umain, lnx)
        call realloc(frcu, lnx)
        call realloc(frcu_mor, lnx)
        call realloc(q1_main, lnx)

        call default_channel_flow()
        call initialize_1dadmin(network, network%numl, numl)
        call setup_network_rectangular_cross_section( &
            network, width=2.0_dp, height=1.5_dp, friction_type=R_MANNING, friction_value=0.013_dp)

        allocate(network%adm%line2cross(1, 3))
        do i = 1, 3
            network%adm%line2cross(1, i)%c1 = 1
            network%adm%line2cross(1, i)%c2 = 1
            network%adm%line2cross(1, i)%f = 1.0_dp
        end do
        network%loaded = .true.
        
        call getprof_1D(1, 1.0_dp, area, width, japerim, calcconv, perim)

        ! We have a rectangular cross section 2 units wide. The water is 1 unit deep.
        ! Surface water width: 2 (Same as rectangular cross section width)
        ! Water cross section area: 2x1 = 2
        ! Wet perimeter: 1+2+1 = 4
        call f90_assert_near(width, 2.0_dp, 1e-7_dp, "Unexpected width result" // c_null_char)
        call f90_assert_near(area, 2.0_dp, 1e-7_dp, "Unexpected area result" // c_null_char)
        call f90_assert_near(perim, 4.0_dp, 1e-7_dp, "Unexpected perim result" // c_null_char)

        hydrad = area / perim
        chezy = get_chezy(hydrad, 0.013_dp, 1.0_dp, 1.0_dp, R_MANNING)
        call f90_assert_near(cfuhi(1), ag / (hydrad * chezy * chezy), 1e-7_dp, "Unexpected friction result" // c_null_char) 
    end subroutine test_getprof_1d__network1d__friction
    !$f90tw )

    !$f90tw TESTCODE(TEST, test_getprof_1d, test_getprof_1d__network1d__full, test_getprof_1d__network1d__full,
    subroutine test_getprof_1d__network1d__full() bind(C)
        use network_data, only: numl
        use m_flow_geominit, only: flow_geominit
        use m_network, only: initialize_1dadmin
        use unstruc_channel_flow, only: network, default_channel_flow

        type(t_grid_helper) :: grid_helper
        integer, parameter :: japerim = 1
        integer, parameter :: calcconv = 0
        real(kind=dp), parameter :: PREISMANN_SLOT_AREA = 1e-7_dp
        integer :: i, new_link, error_code
        real(kind=dp) :: area, width, perim

        
        call disable_timers_logging_and_mpi()
        grid_helper = t_grid_helper()
        call grid_helper%make_square_grid( &
            bottom_left_x=0.0_dp, bottom_left_y=0.0_dp, side_length=10.0_dp, &
            rows=1, columns=2, array_size_margin=2 &
        )
        call place_2d2d_link([5.0_dp, 5.0_dp], [15.0_dp, 5.0_dp], new_link=new_link, error_code=error_code)
        call f90_assert_eq(error_code, 0, "Failed to place 2D2D link" // c_null_char)
        
        call flow_geominit(0)

        call default_channel_flow()
        call initialize_1dadmin(network, network%numl, numl)
        call setup_network_rectangular_cross_section(network, width=2.0_dp, height=1.5_dp)

        allocate(network%adm%line2cross(1, 3))
        do i = 1, 3
            network%adm%line2cross(1, i)%c1 = 1
            network%adm%line2cross(1, i)%c2 = 1
            network%adm%line2cross(1, i)%f = 1.0_dp
        end do
        network%loaded = .true.
        
        call getprof_1D(1, 10.0_dp, area, width, japerim, calcconv, perim)

        ! We have a rectangular cross section 2m wide. The water is 3m deep, but the cross section is only 1.5m high.
        ! Surface water width is 0 for some reason. Maybe due to Preismann slot?
        ! Water cross section area: 2x1.5 = 3m^2
        ! Wet perimeter: 2*(1.5 + 2) = 7m
        call f90_assert_near(width, 0.0_dp, 1e-7_dp, "Unexpected width result" // c_null_char)
        call f90_assert_near(area, 3.0_dp + PREISMANN_SLOT_AREA, 1e-7_dp, "Unexpected area result" // c_null_char)
        call f90_assert_near(perim, 7.0_dp, 1e-7_dp, "Unexpected perim result" // c_null_char)
    end subroutine test_getprof_1d__network1d__full
    !$f90tw )

    !$f90tw TESTCODE(TEST, test_getprof_1d, test_getprof_1d__prof1d_without_profile, test_getprof_1d__prof1d_without_profile,
    subroutine test_getprof_1d__prof1d_without_profile() bind(C)
        use m_flow_geominit, only: flow_geominit
        use unstruc_channel_flow, only: network

        type(t_grid_helper) :: grid_helper
        integer, parameter :: japerim = 1
        integer, parameter :: calcconv = 0
        integer :: new_link, error_code
        real(kind=dp) :: area, width, perim

        ! Arrange
        call disable_timers_logging_and_mpi()
        ! Generate mesh
        grid_helper = t_grid_helper()
        call grid_helper%make_square_grid( &
            bottom_left_x=0.0_dp, bottom_left_y=0.0_dp, side_length=10.0_dp, &
            rows=1, columns=2, array_size_margin=2 &
        )
        call place_2d2d_link([5.0_dp, 5.0_dp], [15.0_dp, 5.0_dp], new_link=new_link, error_code=error_code)
        call f90_assert_eq(error_code, 0, "Failed to place 2D2D link" // c_null_char)

        ! Initialize flow geometry
        call flow_geominit(0)

        ! Initialize `prof1d` without `profiles1D`
        call setup_prof1d_rectangular_cross_section_without_profile(width=2.0_dp, height=1.5_dp)
        network%loaded = .false. ! Skip the channel_flow branch
        
        ! Act
        call getprof_1D(1, 1.0_dp, area, width, japerim, calcconv, perim)

        ! We have a rectangular cross section 2 units wide. The water is 1 unit deep.
        ! Surface water width: 2 (Same as rectangular cross section width)
        ! Water cross section area: 2x1 = 2
        ! Wet perimeter: 1+2+1 = 4
        call f90_assert_near(width, 2.0_dp, 1e-7_dp, "Unexpected width result" // c_null_char)
        call f90_assert_near(area, 2.0_dp, 1e-7_dp, "Unexpected area result" // c_null_char)
        call f90_assert_near(perim, 4.0_dp, 1e-7_dp, "Unexpected perim result" // c_null_char)
    end subroutine test_getprof_1d__prof1d_without_profile
    !$f90tw )

    !$f90tw TESTCODE(TEST, test_getprof_1d, test_getprof_1d__prof1d_without_profile__friction, test_getprof_1d__prof1d_without_profile__friction,
    subroutine test_getprof_1d__prof1d_without_profile__friction() bind(C)
        use m_flow, only: u1, v, cfuhi, frcu, ifrcutp
        use m_flowgeom, only: lnx
        use m_flow_geominit, only: flow_geominit
        use m_physcoef, only: default_physcoef, ag
        use unstruc_channel_flow, only: network
        use m_roughness, only: R_MANNING
        use m_get_chezy, only: get_chezy

        type(t_grid_helper) :: grid_helper
        integer, parameter :: japerim = 1
        integer, parameter :: calcconv = 0
        integer :: new_link, error_code
        real(kind=dp) :: area, width, perim, hydrad, chezy

        ! Arrange
        call disable_timers_logging_and_mpi()
        ! Generate mesh
        grid_helper = t_grid_helper()
        call grid_helper%make_square_grid( &
            bottom_left_x=0.0_dp, bottom_left_y=0.0_dp, side_length=10.0_dp, &
            rows=1, columns=2, array_size_margin=2 &
        )
        call place_2d2d_link([5.0_dp, 5.0_dp], [15.0_dp, 5.0_dp], new_link=new_link, error_code=error_code)
        call f90_assert_eq(error_code, 0, "Failed to place 2D2D link" // c_null_char)

        ! Initialize flow geometry
        call flow_geominit(0)

        ! Initialize `prof1d` without `profiles1D`
        call setup_prof1d_rectangular_cross_section_without_profile( &
            width=2.0_dp, height=1.5_dp, friction_type=R_MANNING, friction_value=0.013_dp)
        network%loaded = .false. ! Skip the channel_flow branch

        call realloc(u1, lnx, fill=1.0_dp)
        call realloc(v, lnx, fill=1.0_dp)
        call realloc(frcu, lnx, fill=0.013_dp)
        call realloc(ifrcutp, lnx, fill=R_MANNING)
        call realloc(cfuhi, lnx)
        
        ! Act
        call getprof_1D(1, 1.0_dp, area, width, japerim, calcconv, perim)

        ! We have a rectangular cross section 2 units wide. The water is 1 unit deep.
        ! Surface water width: 2 (Same as rectangular cross section width)
        ! Water cross section area: 2x1 = 2
        ! Wet perimeter: 1+2+1 = 4
        call f90_assert_near(width, 2.0_dp, 1e-7_dp, "Unexpected width result" // c_null_char)
        call f90_assert_near(area, 2.0_dp, 1e-7_dp, "Unexpected area result" // c_null_char)
        call f90_assert_near(perim, 4.0_dp, 1e-7_dp, "Unexpected perim result" // c_null_char)

        hydrad = area / perim
        chezy = get_chezy(hydrad, 0.013_dp, 1.0_dp, 1.0_dp, R_MANNING)
        call f90_assert_near(cfuhi(1), ag / (hydrad * chezy * chezy), 1e-7_dp, "Unexpected friction result" // c_null_char) 
    end subroutine test_getprof_1d__prof1d_without_profile__friction
    !$f90tw )

    !$f90tw TESTCODE(TEST, test_getprof_1d, test_getprof_1d__prof1d_without_profile__full, test_getprof_1d__prof1d_without_profile__full,
    subroutine test_getprof_1d__prof1d_without_profile__full() bind(C)
        use m_flow_geominit, only: flow_geominit
        use unstruc_channel_flow, only: network
        use m_longculverts_data, only: newculverts

        type(t_grid_helper) :: grid_helper
        integer, parameter :: japerim = 1
        integer, parameter :: calcconv = 0
        integer :: new_link, error_code
        real(kind=dp) :: area, width, perim

        ! Arrange
        call disable_timers_logging_and_mpi()
        ! Generate mesh
        grid_helper = t_grid_helper()
        call grid_helper%make_square_grid( &
            bottom_left_x=0.0_dp, bottom_left_y=0.0_dp, side_length=10.0_dp, &
            rows=1, columns=2, array_size_margin=2 &
        )
        call place_2d2d_link([5.0_dp, 5.0_dp], [15.0_dp, 5.0_dp], new_link=new_link, error_code=error_code)
        call f90_assert_eq(error_code, 0, "Failed to place 2D2D link" // c_null_char)

        ! Initialize flow geometry
        call flow_geominit(0)

        ! Initialize `prof1d` without `profiles1D`
        call setup_prof1d_rectangular_cross_section_without_profile(width=2.0_dp, height=1.5_dp)
        network%loaded = .false. ! Skip the channel_flow branch
        newculverts = .true. ! The perimiter calculation in `rectan` is different if this global is set to true
        
        ! Act
        call getprof_1D(1, 10.0_dp, area, width, japerim, calcconv, perim)

        ! We have a rectangular cross section 2m wide. The water is 3m deep, but the cross section is only 1.5m high.
        ! Surface water width: 2m (Same as rectangular cross section width)
        ! Water cross section area: 2x1.5 = 3m^2
        ! Wet perimeter: 2*(1.5 + 2) = 7m
        call f90_assert_near(width, 2.0_dp, 1e-7_dp, "Unexpected width result" // c_null_char)
        call f90_assert_near(area, 3.0_dp, 1e-7_dp, "Unexpected area result" // c_null_char)
        call f90_assert_near(perim, 7.0_dp, 1e-7_dp, "Unexpected perim result" // c_null_char)
    end subroutine test_getprof_1d__prof1d_without_profile__full
    !$f90tw )

    !$f90tw TESTCODE(TEST, test_getprof_1d, test_getprof_1d__prof1d_with_profile, test_getprof_1d__prof1d_with_profile,
    subroutine test_getprof_1d__prof1d_with_profile() bind(C)
        use m_flow_geominit, only: flow_geominit
        use unstruc_channel_flow, only: network
        implicit none

        type(t_grid_helper) :: grid_helper
        integer, parameter :: japerim = 1
        integer, parameter :: calcconv = 0
        integer :: new_link, error_code
        real(kind=dp) :: area, width, perim

        call disable_timers_logging_and_mpi()
        grid_helper = t_grid_helper()
        call grid_helper%make_square_grid( &
            bottom_left_x=0.0_dp, bottom_left_y=0.0_dp, side_length=10.0_dp, &
            rows=1, columns=2, array_size_margin=2 &
        )
        call place_2d2d_link([5.0_dp, 5.0_dp], [15.0_dp, 5.0_dp], new_link=new_link, error_code=error_code)
        call f90_assert_eq(error_code, 0, "Failed to place 2D2D link" // c_null_char)

        call flow_geominit(0)
        call setup_prof1d_rectangular_cross_section_with_profile(width=2.0_dp, height=1.5_dp)
        network%loaded = .false. ! Trick to skip the branch
        
        call getprof_1D(1, 1.0_dp, area, width, japerim, calcconv, perim)

        ! We have a rectangular cross section 2 units wide. The water is 1 unit deep.
        ! Surface water width: 2 (Same as rectangular cross section width)
        ! Water cross section area: 2x1 = 2
        ! Wet perimeter: 1+2+1 = 4
        call f90_assert_near(width, 2.0_dp, 1e-7_dp, "Unexpected width result" // c_null_char)
        call f90_assert_near(area, 2.0_dp, 1e-7_dp, "Unexpected area result" // c_null_char)
        call f90_assert_near(perim, 4.0_dp, 1e-7_dp, "Unexpected perim result" // c_null_char)
    end subroutine test_getprof_1d__prof1d_with_profile
    !$f90tw )

    !$f90tw TESTCODE(TEST, test_getprof_1d, test_getprof_1d__prof1d_with_profile__friction, test_getprof_1d__prof1d_with_profile__friction,
    subroutine test_getprof_1d__prof1d_with_profile__friction() bind(C)
        use m_flow, only: u1, v, cfuhi
        use m_flowgeom, only: lnx
        use m_flow_geominit, only: flow_geominit
        use m_physcoef, only: default_physcoef, ag
        use unstruc_channel_flow, only: network
        use m_roughness, only: R_MANNING
        use m_get_chezy, only: get_chezy

        type(t_grid_helper) :: grid_helper
        integer, parameter :: japerim = 1
        integer, parameter :: calcconv = 1
        integer :: new_link, error_code
        real(kind=dp) :: area, width, perim, hydrad, chezy

        call disable_timers_logging_and_mpi()
        call default_physcoef()

        grid_helper = t_grid_helper()
        call grid_helper%make_square_grid( &
            bottom_left_x=0.0_dp, bottom_left_y=0.0_dp, side_length=10.0_dp, &
            rows=1, columns=2, array_size_margin=2 &
        )
        call place_2d2d_link([5.0_dp, 5.0_dp], [15.0_dp, 5.0_dp], new_link=new_link, error_code=error_code)
        call f90_assert_eq(error_code, 0, "Failed to place 2D2D link" // c_null_char)

        call flow_geominit(0)
        call setup_prof1d_rectangular_cross_section_with_profile( &
            width=2.0_dp, height=1.5_dp, friction_type=R_MANNING, friction_value=0.013_dp)
        network%loaded = .false. ! Trick to skip the branch

        call realloc(u1, lnx, fill=1.0_dp)
        call realloc(v, lnx, fill=1.0_dp)
        call realloc(cfuhi, lnx)
        
        call getprof_1D(1, 1.0_dp, area, width, japerim, calcconv, perim)

        ! We have a rectangular cross section 2 units wide. The water is 1 unit deep.
        ! Surface water width: 2 (Same as rectangular cross section width)
        ! Water cross section area: 2x1 = 2
        ! Wet perimeter: 1+2+1 = 4
        call f90_assert_near(width, 2.0_dp, 1e-7_dp, "Unexpected width result" // c_null_char)
        call f90_assert_near(area, 2.0_dp, 1e-7_dp, "Unexpected area result" // c_null_char)
        call f90_assert_near(perim, 4.0_dp, 1e-7_dp, "Unexpected perim result" // c_null_char)

        hydrad = area / perim
        chezy = get_chezy(hydrad, 0.013_dp, 1.0_dp, 1.0_dp, R_MANNING)
        call f90_assert_near(cfuhi(1), ag / (hydrad * chezy * chezy), 1e-7_dp, "Unexpected friction result" // c_null_char) 
    end subroutine test_getprof_1d__prof1d_with_profile__friction
    !$f90tw )

    !$f90tw TESTCODE(TEST, test_getprof_1d, test_getprof_1d__prof1d_with_profile__full, test_getprof_1d__prof1d_with_profile__full,
    subroutine test_getprof_1d__prof1d_with_profile__full() bind(C)
        use m_flow_geominit, only: flow_geominit
        use unstruc_channel_flow, only: network
        use m_longculverts_data, only: newculverts
        implicit none

        type(t_grid_helper) :: grid_helper
        integer, parameter :: japerim = 1
        integer, parameter :: calcconv = 0
        integer :: new_link, error_code
        real(kind=dp) :: area, width, perim

        call disable_timers_logging_and_mpi()
        grid_helper = t_grid_helper()
        call grid_helper%make_square_grid( &
            bottom_left_x=0.0_dp, bottom_left_y=0.0_dp, side_length=10.0_dp, &
            rows=1, columns=2, array_size_margin=2 &
        )
        call place_2d2d_link([5.0_dp, 5.0_dp], [15.0_dp, 5.0_dp], new_link=new_link, error_code=error_code)
        call f90_assert_eq(error_code, 0, "Failed to place 2D2D link" // c_null_char)

        call flow_geominit(0)
        call setup_prof1d_rectangular_cross_section_with_profile(width=2.0_dp, height=1.5_dp)
        network%loaded = .false. ! Trick to skip the branch
        newculverts = .true. ! The perimiter calculation in `rectan` is different if this global is set to true
        
        call getprof_1D(1, 10.0_dp, area, width, japerim, calcconv, perim)

        ! We have a rectangular cross section 2m wide. The water is 3m deep, but the cross section is only 1.5m high.
        ! Surface water width: 2m (Same as rectangular cross section width)
        ! Water cross section area: 2x1.5 = 3m^2
        ! Wet perimeter: 2*(1.5 + 2) = 7m
        call f90_assert_near(width, 2.0_dp, 1e-7_dp, "Unexpected width result" // c_null_char)
        call f90_assert_near(area, 3.0_dp, 1e-7_dp, "Unexpected area result" // c_null_char)
        call f90_assert_near(perim, 7.0_dp, 1e-7_dp, "Unexpected perim result" // c_null_char)
    end subroutine test_getprof_1d__prof1d_with_profile__full
    !$f90tw )

end module m_test_getprof_1d

