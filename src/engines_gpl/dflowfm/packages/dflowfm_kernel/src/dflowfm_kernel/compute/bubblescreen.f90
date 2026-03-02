module m_bubblescreen
    use precision_basics, only: dp, comparereal
    use fm_external_forcings_data, only: t_BubbleScreen, bubblescreens, source_sink_indices, source_sink_all_discharges, bubblescreen_air_discharge, source_sink_z_bottom, source_sink_z_top
    use m_alloc, only: realloc
    use m_cell_geometry, only: ba
    use m_flow, only: kmx, zws, kbot, s1, vol1
    use m_get_kbot_ktop, only: getkbotktop
    use m_transport, only: numconst, constituents
    use messageHandling, only: err_flush, msgbuf, msg_flush

    implicit none(type, external)

    private

    public :: update_bubblescreen_discharge_wrapper
    public :: update_bubblescreen_discharge
    public :: convert_discharge_air_to_water
    ! public :: compute_bubblescreen_area
    public :: find_active_layer_interfaces
    ! public :: compute_water_discharge_across_layers
    ! public :: compute_constituent_discharge
    ! public :: write_discharge_to_source_sinks

contains

    !> Wrapper subroutine to update the discharges for all bubble screens; loops over all bubble screens and calls update_bubblescreen_discharge for each of them
    subroutine update_bubblescreen_discharge_wrapper()
        ! Parameters

        ! Local variables
        integer :: i !< Loop index

        do i = 1, size(bubblescreens)
            call update_bubblescreen_discharge(bubblescreens(i), bubblescreen_air_discharge(i))
        end do

    end subroutine update_bubblescreen_discharge_wrapper

    !> Updates the discharges for a single bubble screen object
    subroutine update_bubblescreen_discharge(bubblescreen, air_discharge)       
        ! Parameters
        type(t_BubbleScreen), intent(in) :: bubblescreen !< Bubble screen data structure
        real(kind=dp), intent(in) :: air_discharge !< Air discharge for this bubble screen

        ! Local variables
        integer :: i_flowcell !< Bubblescreen flow cell index
        integer :: k_start !< Start active layer index (bottom)
        integer :: k_stop !< Stop active layer index (top) (inclusive)
        integer :: k_max_velocity !< Layer index for maximum downward velocity
        integer :: n !< 2D flow cell index; in {network_data::netcell}
        real(kind=dp) :: area_fraction !< Area fraction of the flow cell
        ! real(kind=dp) :: max_velocity !< Maximum downward vertical velocity for this flow cell
        ! real(kind=dp) :: total_area !< Total area of the bubble screen
        real(kind=dp) :: water_discharge !< Water discharge for this bubble screen
        real(kind=dp) :: local_water_discharge !< Water discharge for this flow cell

        ! Convert air discharge to water discharge
        water_discharge = convert_discharge_air_to_water(air_discharge)

        ! Set linked source/sink discharges for each flow cell in the bubble screen
        do i_flowcell = 1, bubblescreen%num_flowcells
            n = bubblescreen%flowcell_indices(i_flowcell)

            ! Compute water discharge for this flow cell based on the area fraction of the flow cell
            area_fraction = ba(n) / bubblescreen%total_area
            local_water_discharge = water_discharge * area_fraction

            call update_bubblescreen_source_sink_discharge(bubblescreen%source_sink_indices(i_flowcell), local_water_discharge)

            call find_active_layer_interfaces(n, bubblescreen%z_level, bubblescreen%id, k_start, k_stop, k_max_velocity)
            
            call update_bubblescreen_source_sink_layer_indices(bubblescreen%source_sink_indices(i_flowcell), n, k_start, k_stop, k_max_velocity)


            ! ! Compute maximum downward vertical velocity based on area fraction
            ! area_fraction = ba(n) / total_area
            ! max_velocity = -1.0_dp * water_discharge * area_fraction / ba(n)

            ! call find_active_layer_interfaces(n, bubblescreen%z_level, bubblescreen%id, k_start, k_stop, k_max_velocity)

            ! call compute_water_discharge_across_layers(n, k_start, k_stop, k_max_velocity, max_velocity, discharge_water)

            ! call compute_constituent_discharge(n, k_start, k_stop, k_max_velocity, discharge_constituents)

            ! call write_discharge_to_source_sinks(flowcell, discharge_water, discharge_constituents)

        end do

    end subroutine update_bubblescreen_discharge

    !> Converts the injected air discharge rate to entrained water discharge rate using an empirical formula.
    pure function convert_discharge_air_to_water(air_discharge, alpha) result(water_discharge)
        ! Parameters
        real(kind=dp), intent(in) :: air_discharge !< [m3/s] Air discharge rate
        real(kind=dp), intent(in), optional :: alpha !< Empirical factor α used in the conversion between the amount of entrained water and the amount of injected air (default 1000).
        real(kind=dp) :: water_discharge !< [m3/s] Resulting water discharge rate

        ! Local variables
        real(kind=dp) :: alpha0

        ! Check if alpha is provided, otherwise use default value
        if (present(alpha)) then
            alpha0 = alpha
        else
            alpha0 = 1000.0_dp
        end if

        water_discharge = (alpha0 * air_discharge) ** (2.0_dp / 3.0_dp)

    end function convert_discharge_air_to_water

    ! !> Computes the total area of a bubble screen based on its flow cells
    ! pure function compute_bubblescreen_area(bubblescreen) result(area)
    !     ! Parameters
    !     type(t_BubbleScreen), intent(in) :: bubblescreen !< Bubble screen data structure
    !     real(kind=dp) :: area !< [m2] Area of the bubble screen

    !     ! Local variables
    !     integer :: i

    !     area = 0.0_dp

    !     do i = 1, bubblescreen%num_flow_cells
    !         area = area + ba(bubblescreen%flow_cells(i)%flownode_nr)
    !     end do

    ! end function compute_bubblescreen_area

    !> Finds the layer interfaces of the bottom (k_start), top (k_stop) and maximum velocity (k_max_velocity) for a bubble screen in a flow cell
    subroutine find_active_layer_interfaces(flow_cell_index, z_bot, bubblescreen_id, k_start, k_stop, k_max_velocity)
        ! Parameters
        integer, intent(in) :: flow_cell_index !< 2D flow cell index; in {network_data::netcell}
        real(kind=dp), intent(in) :: z_bot !< [m] Bottom elevation of the flow cell
        character(len=*), intent(in) :: bubblescreen_id !< Bubble screen id 
        integer, intent(out) :: k_start !< Layer interface of lowest active source/sink in bubble screen; in {m_flow::zws}
        integer, intent(out) :: k_stop !< Layer interface of highest active source/sink in bubble screen; in {m_flow::zws}
        integer, intent(out) :: k_max_velocity !< Layer interface with maximum downward velocity; in {m_flow::zws}

        ! Local variables
        integer :: k !< Layer interface {in m_flow::zws}
        integer :: k_bot !< Bottom layer interface from getkbotktop {in m_flow::zws}
        integer :: k_top !< Top layer interface from getkbotktop {in m_flow::zws}
        real(kind=dp) :: z_top !< [m] Top elevation of the flow cell
        real(kind=dp) :: z_max_velocity !< [m] Elevation of maximum downward velocity


        ! A visual illustrating the difference between layer indices (K) and layer interfaces (k) for 3D cells
        ! A 2D flow cell is shown with kmx=4 layers
        ! The method that is used to find k_start, k_stop, and k_max_velocity is illustrated as well
        ! 
        !   ----------- k = 4 <----- if z_top (the water level) is defined here
        !                            k=4 will be selected as the closest interface
        !       K=4
        !                 v--------- if z_max_velocity is here (defined as 20% from z_top down to z_bot)
        !   ----------- k = 3        k=3 will be selected as the closest interface
        !
        !       K=3
        !
        !   ----------- k = 2
        !
        !       K=2
        !
        !   ----------- k = 1
        !                 ^--------- if z_bot (the z-level of the bubblescreen) is here
        !       K=1                  k=1 will be selected as the closest interface
        !   
        !   ----------- k = 0


        ! Get bottom and top layer interfaces of the flow cell
        call getkbotktop(flow_cell_index, k_bot, k_top)

        ! Start all interfaces at bottom layer interface
        k_start = k_bot - 1
        k_stop = k_bot - 1
        k_max_velocity = k_bot - 1

        z_top = s1(flow_cell_index) ! Top elevation is set to water level in the flow cell
        z_max_velocity = z_top - 0.2_dp * (z_top - z_bot) ! Max velocity is located at 20% below z_top down to z_bot

        ! Find for each z value (bot, max_velocity, top) the closest layer interface
        do k = k_bot, k_top
            if (abs(zws(k) - z_bot) < abs(zws(k_start) - z_bot)) then
                k_start = k
            end if

            if (abs(zws(k) - z_max_velocity) < abs(zws(k_max_velocity) - z_max_velocity)) then
                k_max_velocity = k
            end if

            if (abs(zws(k) - z_top) < abs(zws(k_stop) - z_top)) then
                k_stop = k
            end if
        end do
        
        ! Require at least 3 active layers in the bubble screen
        if (k_stop - k_start < 3) then
            write(msgbuf, '(A,A,A,I0,A,F7.2,A,F7.2,A)') 'Bubble screen "', trim(bubblescreen_id), '" in flow cell ', flow_cell_index, ' has insufficient active layers (min 3) between z=', &
                zws(k_start), ' and z=', zws(k_stop), '. Increase bubble screen vertical extent or check flow cell water level.'
            call err_flush()
        end if

        ! Require at least 1 layer between k_max_velocity and k_stop; if not adjust k_max_velocity
        if (k_stop - k_max_velocity < 1) then
            k_max_velocity = k_stop - 1
        end if

    end subroutine find_active_layer_interfaces

    ! !> Computes the vertical distribution of water discharges for a bubble screen in a flow cell
    ! pure subroutine compute_water_discharge_across_layers(flow_cell_index, k_start, k_stop, k_max_velocity, max_velocity, discharge_water)
    !     ! Parameters
    !     integer, intent(in) :: flow_cell_index !< 2D flow cell index; in {network_data::netcell}
    !     integer, intent(in) :: k_start !< Start active layer index (bottom); in {m_flow::zws}
    !     integer, intent(in) :: k_stop !< Stop active layer index (top) (inclusive); in {m_flow::zws}
    !     integer, intent(in) :: k_max_velocity !< Layer index with maximum downward velocity; in {m_flow::zws}
    !     real(kind=dp), intent(in) :: max_velocity !< Maximum downward vertical velocity for this flow cell
    !     real(kind=dp), dimension(kmx), intent(inout) :: discharge_water !< Water discharge for all layers in 2D flow cell; size={kmx}

    !     ! Local variables
    !     integer :: k_local !< Local layer index within flow cell
    !     integer :: k_global !< Global layer index
    !     real(kind=dp) :: velocity_gradient !< [1/s] Vertical velocity gradient
    !     real(kind=dp) :: vertical_fraction !< Fractional vertical position within bubble screen
    !     real(kind=dp), dimension(kmx+1) :: vertical_velocity !< Vertical velocity array (at layer interfaces) size:{kmx+1}


    !     ! It is assumed that a bubble screen induces a triangular downward vertical velocity profile
    !     ! The maximum velocity is 20% from z_top down to z_bot, at z_top and z_bot the velocity is zero
    !     ! A visual illustrating the triangular vertical velocity profile is shown for a 2D flow cell with kmx=4 layers
    !     ! The velocities are defined at the layer interfaces (k)
    !     ! This visual continues from the visual in find_active_layer_interfaces
    !     !
    !     !    <-- velocity magnitude
    !     !  0 m/s          --- k = 4 (k_top; velocity = 0)
    !     !              ==  |
    !     !            ====  |
    !     ! -6 m/s   ====== --- k = 3 (k_max_velocity; velocity = maximal)
    !     !           =====  |
    !     !            ====  |
    !     ! -3 m/s      === --- k = 2
    !     !              ==  |
    !     !               =  |
    !     !  0 m/s          --- k = 1 (k_start; velocity = 0)
    !     !                  |
    !     !                  |
    !     !  0 m/s          --- k = 0
    !     !
    !     ! Using the velocity distribution, delta velocities are computed at layer indices (K)
    !     ! The delta velocities are then multiplied by the flow cell area to get the water discharge per layer
    !     !
    !     ! This results in the following water discharge distribution per layer index (K):
    !     ! + indicates a source, - indicates a sink
    !     !
    !     !    <-- water discharge magnitude
    !     !                 ---
    !     ! +6 m3/s  ++++++  |  K=4
    !     !                  |
    !     !                 ---
    !     ! -3 m3/s     ---  |  K=3
    !     !                  |
    !     !                 ---
    !     ! -3 m3/s     ---  |  K=2
    !     !                  |
    !     !                 ---
    !     !  0 m3/s          |  K=1
    !     !                  |
    !     !                 ---
    !     !
    !     ! The water discharge distribution always sums to zero for all 3D layers in a 2D cell


    !     ! Initialize discharge_water and vertical_velocity arrays to zero
    !     discharge_water = 0.0_dp
    !     vertical_velocity = 0.0_dp

    !     ! Fill vertical velocity array
    !     do k_local = 1, kmx+1
    !         k_global = kbot(flow_cell_index) + k_local - 2
    !         if (k_global < k_start .or. k_global > k_stop) then
    !             vertical_velocity(k_local) = 0.0_dp ! Outside bubble screen active layers
    !         else if (k_global <= k_max_velocity) then
    !             vertical_fraction = (zws(k_global) - zws(k_start)) / (zws(k_max_velocity) - zws(k_start))
    !             vertical_velocity(k_local) = max_velocity * vertical_fraction
    !         else
    !             vertical_fraction = (zws(k_stop) - zws(k_global)) / (zws(k_stop) - zws(k_max_velocity))
    !             vertical_velocity(k_local) = max_velocity * vertical_fraction
    !         end if
    !     end do

    !     ! Compute water discharge using vertical velocity profile
    !     do k_local = 1, kmx
    !         k_global = kbot(flow_cell_index) + k_local - 1 ! Convert local layer index to global layer index

    !         velocity_gradient = (vertical_velocity(k_local+1) - vertical_velocity(k_local)) / (zws(k_global) - zws(k_global-1))
    !         discharge_water(k_local) = velocity_gradient * vol1(k_global) ! Water discharge = velocity gradient * layer volume
    !     end do

    ! end subroutine compute_water_discharge_across_layers

    ! !> Computes the vertical distribution of constituent discharges for a bubble screen in a flow cell
    ! pure subroutine compute_constituent_discharge(flow_cell_index, k_start, k_stop, k_max_velocity, discharge_constituents)
    !     ! Parameters
    !     integer, intent(in) :: flow_cell_index !< 2D flow cell index; in {network_data::netcell}
    !     integer, intent(in) :: k_start !< Start active layer index (bottom); in {m_flow::zws}
    !     integer, intent(in) :: k_stop !< Stop active layer index (top); in {m_flow::zws}
    !     integer, intent(in) :: k_max_velocity !< Layer index with maximum downward velocity; in {m_flow::zws}
    !     real(kind=dp), dimension(numconst, kmx), intent(inout) :: discharge_constituents !< [kg/m3, ppt, degC] Constituent discharge concentration/temperature for all layers in 2D flow cell; size={numconst,kmx}

    !     ! Local variables
    !     integer :: i !< Loop index
    !     integer :: k_local !< Local layer index within flow cell
    !     integer :: k_global !< Global layer index
    !     real(kind=dp), dimension(numconst) :: source_constituents !< [kg/m3, ppt, degC] Constituent concentration/temperature for source layers; size={numconst}
    !     real(kind=dp), dimension(numconst) :: sum_sink_constituents !< [kg/m3, ppt, degC] Sum of constituent concentrations in sink layers; size={numconst}

    !     ! Initialize variables to zero
    !     discharge_constituents = 0.0_dp
    !     sum_sink_constituents = 0.0_dp

    !     ! First compute constituent discharges for sink layers
    !     do k_global = k_start+1, k_max_velocity
    !         k_local = k_global - kbot(flow_cell_index) + 1 ! Convert to local layer index in flow cell

    !         do i = 1, numconst
    !             discharge_constituents(i, k_local) = constituents(i, k_global)
    !             sum_sink_constituents(i) = sum_sink_constituents(i) + discharge_constituents(i, k_local)
    !         end do
    !     end do

    !     ! Source constituents is average of sink constituents concentration/temperature
    !     do i = 1, numconst
    !         source_constituents(i) = sum_sink_constituents(i) / (k_max_velocity - k_start)
    !     end do

    !     ! Then compute constituent discharges for source layers
    !     do k_global = k_max_velocity+1, k_stop
    !         k_local = k_global - kbot(flow_cell_index) + 1 ! Convert to local layer index in flow cell

    !         do i = 1, numconst
    !             discharge_constituents(i, k_local) = source_constituents(i)
    !         end do
    !     end do

    ! end subroutine compute_constituent_discharge

    ! !> Writes the computed discharges for a bubble screen in a flow cell to the source/sink discharge array {fm_external_forcings_data::source_sink_all_discharges}
    ! subroutine write_discharge_to_source_sinks(flow_cell, discharge_water, discharge_constituents)
    !     ! Parameters
    !     type(t_BubbleScreenFlowCell), intent(in) :: flow_cell !< Flow cell data structure
    !     real(kind=dp), dimension(kmx), intent(in) :: discharge_water !< Discharge array for water for all layers in 2D flow cell
    !     real(kind=dp), dimension(numconst, kmx), intent(in) :: discharge_constituents !< Discharge array for constituents for all layers in 2D flow cell

    !     ! Local variables
    !     integer :: i !< Constituent index
    !     integer :: k_local !< Local layer index within flow cell
    !     integer :: k_global !< Global layer index

    !     do k_local = 1, kmx
    !         k_global = k_local + kbot(flow_cell%flownode_nr) - 1 ! Convert local layer index to global layer index

    !         ! Set source/sink as source or sink depending on the sign of the discharge
    !         call set_source_or_sink_for_bubblescreen(discharge_water(k_local), flow_cell%start_index + k_local - 1)

    !         ! Write water discharge to source/sink discharge array
    !         source_sink_all_discharges(1, flow_cell%start_index + k_local - 1) = abs(discharge_water(k_local))

    !         ! Write constituent discharges to source/sink discharge array
    !         do i = 1, numconst
    !             source_sink_all_discharges(i + 1, flow_cell%start_index + k_local - 1) = discharge_constituents(i, k_local)
    !         end do
    !     end do

    ! end subroutine write_discharge_to_source_sinks

    ! !> Set source/sink for bubble screen as source or sink depending on the sign of the discharge
    ! subroutine set_source_or_sink_for_bubblescreen(discharge, source_sink_index)
    !     ! Parameters
    !     real(kind=dp), intent(in) :: discharge !< Discharge for this layer; if positive, this layer is a source; if negative, this layer is a sink
    !     integer, intent(in) :: source_sink_index !< Index in source_sink_indices/source_sink_all_discharges arrays corresponding to this layer

    !     if (source_sink_indices(4, source_sink_index) > 0) then ! Check if this layer is a source
    !             if (comparereal(discharge, 0.0_dp) == -1) then ! Check if discharge is negative (sink); if true set source to sink
    !                 source_sink_indices(1, source_sink_index) = source_sink_indices(4, source_sink_index)
    !                 source_sink_indices(2, source_sink_index) = source_sink_indices(5, source_sink_index)
    !                 source_sink_indices(3, source_sink_index) = source_sink_indices(6, source_sink_index)

    !                 source_sink_indices(4, source_sink_index) = 0
    !                 source_sink_indices(5, source_sink_index) = 0
    !                 source_sink_indices(6, source_sink_index) = 0
    !             end if
    !         end if

    !         if (source_sink_indices(1, source_sink_index) > 0) then ! Check if this layer is a sink
    !             if (comparereal(discharge, 0.0_dp) == 1) then ! Check if discharge is positive (source); if true set sink to source
    !                 source_sink_indices(4, source_sink_index) = source_sink_indices(1, source_sink_index)
    !                 source_sink_indices(5, source_sink_index) = source_sink_indices(2, source_sink_index)
    !                 source_sink_indices(6, source_sink_index) = source_sink_indices(3, source_sink_index)

    !                 source_sink_indices(1, source_sink_index) = 0
    !                 source_sink_indices(2, source_sink_index) = 0
    !                 source_sink_indices(3, source_sink_index) = 0
    !             end if
    !         end if

    ! end subroutine set_source_or_sink_for_bubblescreen

    !> Update the discharge for a bubble screen source/sink
    subroutine update_bubblescreen_source_sink_discharge(source_sink_index, discharge)
        ! Parameters
        integer, intent(in) :: source_sink_index !< [-] Index of source/sink in {fm_external_forcings_data::source_sink_indices}
        real(kind=dp), intent(in) :: discharge !< [m3/s] Water discharge for this source/sink

        source_sink_all_discharges(1, source_sink_index) = abs(discharge)

        ! Set constituent discharges to zero for bubble screen source/sinks
        if (numconst /= 0) then
            source_sink_all_discharges(2:numconst+1, source_sink_index) = 0.0_dp 
        end if

    end subroutine update_bubblescreen_source_sink_discharge

    !> Updates the vertical layer indices for a bubble screen linked source/sink
    subroutine update_bubblescreen_source_sink_layer_indices(source_sink_index, n, k_start, k_stop, k_max_velocity)
        ! Parameters
        integer, intent(in) :: source_sink_index !< [-] Index of source/sink in {fm_external_forcings_data::source_sink_indices}
        integer, intent(in) :: n !< [-] 2D flow cell index; in {network_data::netcell}
        integer, intent(in) :: k_start !< [-] Start active layer index (bottom); in {m_flow::zws}
        integer, intent(in) :: k_stop !< [-] Stop active layer index (top); in {m_flow::zws}
        integer, intent(in) :: k_max_velocity !< [-] Layer index with maximum downward velocity; in {m_flow::zws}

        ! Local variables
        ! real(kind=dp), parameter :: OFFSET = 1.0e-5_dp !< Offset to ensure that source_sink_z_[bottom,top] are no exactly at the layer interfaces

        ! Update source/sink indices
        source_sink_indices(1, source_sink_index) = n
        source_sink_indices(2, source_sink_index) = k_start + 1
        source_sink_indices(3, source_sink_index) = k_max_velocity
        source_sink_indices(4, source_sink_index) = n
        source_sink_indices(5, source_sink_index) = k_max_velocity + 1
        source_sink_indices(6, source_sink_index) = k_stop

        ! Update source/sink top and bottom z-levels
        source_sink_z_bottom(1, source_sink_index) = (zws(k_start) + zws(k_start+1)) / 2.0_dp
        source_sink_z_bottom(2, source_sink_index) = (zws(k_max_velocity) + zws(k_max_velocity-1)) / 2.0_dp
        source_sink_z_top(1, source_sink_index) = (zws(k_max_velocity) + zws(k_max_velocity+1)) / 2.0_dp
        source_sink_z_top(2, source_sink_index) = (zws(k_stop) + zws(k_stop-1)) / 2.0_dp

    end subroutine update_bubblescreen_source_sink_layer_indices

end module m_bubblescreen
