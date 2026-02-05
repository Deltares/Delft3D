module m_bubblescreen
    use precision_basics, only: dp, comparereal
    use fm_external_forcings_data, only: t_BubbleScreen, t_BubbleScreenFlowCell, bubblescreens, ksrc, qstss
    use m_alloc, only: realloc
    use m_cell_geometry, only: ba
    use m_flow, only: kmx, zws, kbot, s1
    use m_get_kbot_ktop, only: getkbotktop
    use m_transport, only: numconst, constituents
    use messageHandling, only: err_flush, msgbuf, msg_flush

    implicit none(type, external)

    private

    public :: update_bubblescreens_discharge
    public :: update_bubblescreen_discharge

contains

    !> Wrapper subroutine to update discharges for all bubble screens
    subroutine update_bubblescreens_discharge()
        ! Parameters

        ! Local variables
        integer :: i !< Loop index for bubble screens

        do i = 1, size(bubblescreens)
            call update_bubblescreen_discharge(bubblescreens(i))
        end do

    end subroutine update_bubblescreens_discharge

    !> Updates the discharges for a single bubble screen based on user-specified air discharge rates
    subroutine update_bubblescreen_discharge(bubblescreen)       
        ! Parameters
        type(t_BubbleScreen), intent(in) :: bubblescreen !< Bubble screen data structure

        ! Local variables
        integer :: i_flow_cell !< Flow cell index
        integer :: k_start !< Start active layer index (bottom)
        integer :: k_stop !< Stop active layer index (top) (inclusive)
        integer :: k_max_velocity !< Layer index for maximum downward velocity
        integer :: n !< Flow cell index
        real(kind=dp) :: area_fraction !< Area fraction of the flow cell
        real(kind=dp) :: max_velocity !< Maximum downward vertical velocity for this flow cell
        real(kind=dp) :: total_area !< Total area of the bubble screen
        real(kind=dp) :: air_discharge !< Air discharge for this bubble screen
        real(kind=dp) :: water_discharge !< Water discharge for this bubble screen
        real(kind=dp), dimension(1+numconst, kmx) :: discharge !< Discharge array for water and constituents for all layers in 2D flow cell
        type(t_BubbleScreenFlowCell) :: flow_cell !< Current flow cell

        ! Initialize all discharges to zero
        discharge = 0.0_dp

        ! Lookup total discharge air for this bubble screen and compute water discharge
        ! ====================================================================================================
        ! TODO: switch to correct lookup of air discharge for this bubble screen when ready (found qstss array)

        air_discharge = 3.0e-2_dp ! Placeholder value
        ! air_discharge = qstss(flow_cell%start_index) ! Get air discharge from first source/sink in array (all source/sinks are set to the same value by the EC module)
        ! ====================================================================================================
        water_discharge = convert_discharge_air_to_water(air_discharge)

        total_area = compute_bubblescreen_area(bubblescreen)

        ! Compute vertical distribution for each flow cell
        do i_flow_cell = 1, bubblescreen%num_flow_cells
            flow_cell = bubblescreen%flow_cells(i_flow_cell)
            n = flow_cell%cell_index

            ! Compute maximum downward vertical velocity based on area fraction
            area_fraction = ba(n) / total_area
            max_velocity = -1.0_dp * water_discharge * area_fraction / ba(n)

            ! Get start and stop indices of active layers in the bubble screen and layer index with maximum downward velocity
            call find_active_layer_interfaces(n, bubblescreen%z_level, bubblescreen%id, k_start, k_stop, k_max_velocity)

            ! Compute water discharges for this flow cell
            call compute_water_discharge(n, k_start, k_stop, k_max_velocity, max_velocity, discharge)

            ! Compute constituent discharges for this flow cell
            call compute_constituent_discharge(n, k_start, k_stop, k_max_velocity, discharge)

            ! Write discharges to source/sink discharge array
            call write_discharge_to_source_sinks(flow_cell, discharge)

        end do

    end subroutine update_bubblescreen_discharge

    !> Converts the air discharge rate to water discharge rate using an empirical formula
    function convert_discharge_air_to_water(air_discharge, alpha) result(water_discharge)
        ! Parameters
        real(kind=dp), intent(in) :: air_discharge !< [m3/s] Air discharge rate
        real(kind=dp), intent(in), optional :: alpha !< Empirical coefficient (default 1000)
        real(kind=dp) :: water_discharge !< [m3/s] Resulting water discharge rate

        ! Local variables
        real(kind=dp) :: alpha0

        ! Check if alpha is provided, otherwise use default value
        if (present(alpha)) then
            alpha0 = alpha
        else
            alpha0 = 1000.0_dp
        end if

        ! Compute water discharge using empirical formula
        water_discharge = (alpha0 * air_discharge) ** (2.0_dp / 3.0_dp)

    end function convert_discharge_air_to_water

    !> Computes the total area of a bubble screen based on its flow cells
    function compute_bubblescreen_area(bubblescreen) result(area)
        ! Parameters
        type(t_BubbleScreen), intent(in) :: bubblescreen !< Bubble screen data structure
        real(kind=dp) :: area !< [m2] Area of the bubble screen

        ! Local variables
        integer :: i

        area = 0.0_dp

        do i = 1, bubblescreen%num_flow_cells
            area = area + ba(bubblescreen%flow_cells(i)%cell_index)
        end do

    end function compute_bubblescreen_area

    !> Finds the layer interfaces of the bottom (k_start), top (k_stop) and maximum velocity (k_max_velocity) for a bubble screen in a flow cell
    subroutine find_active_layer_interfaces(flow_cell_index, z_bot, bubblescreen_id, k_start, k_stop, k_max_velocity)
        ! Parameters
        integer, intent(in) :: flow_cell_index !< 2D flow cell index {in network_data::netcell}
        real(kind=dp), intent(in) :: z_bot !< [m] Bottom elevation of the flow cell
        character(len=*), intent(in) :: bubblescreen_id !< Bubble screen id 
        integer, intent(out) :: k_start !< Layer interface of lowest active source/sink in bubble screen {in m_flow::zws}
        integer, intent(out) :: k_stop !< Layer interface of highest active source/sink in bubble screen {in m_flow::zws}
        integer, intent(out) :: k_max_velocity !< Layer interface with maximum downward velocity {in m_flow::zws}

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

    !> Computes the vertical distribution of water discharges for a bubble screen in a flow cell
    subroutine compute_water_discharge(flow_cell_index, k_start, k_stop, k_max_velocity, max_velocity, discharge)
        ! Parameters
        integer, intent(in) :: flow_cell_index !< 2D flow cell index {in network_data::netcell}
        integer, intent(in) :: k_start !< Start active layer index (bottom) {in m_flow::zws}
        integer, intent(in) :: k_stop !< Stop active layer index (top) (inclusive) {in m_flow::zws}
        integer, intent(in) :: k_max_velocity !< Layer index with maximum downward velocity {in m_flow::zws}
        real(kind=dp), intent(in) :: max_velocity !< Maximum downward vertical velocity for this flow cell
        real(kind=dp), dimension(1+numconst, kmx), intent(inout) :: discharge !< Discharge array for water and constituents for all layers in 2D flow cell 

        ! Local variables
        integer :: l !< Local layer index within flow cell
        integer :: k !< Layer index
        real(kind=dp) :: delta_velocity !< Change in vertical velocity per layer
        real(kind=dp) :: vertical_fraction !< Fractional vertical position within bubble screen
        real(kind=dp), dimension(kmx+1) :: vertical_velocity !< Vertical velocity array (at layer interfaces) size:{kmx+1}


        ! It is assumed that a bubble screen induces a triangular downward vertical velocity profile
        ! The maximum velocity is 20% from z_top down to z_bot, at z_top and z_bot the velocity is zero
        ! A visual illustrating the triangular vertical velocity profile is shown for a 2D flow cell with kmx=4 layers
        ! The velocities are defined at the layer interfaces (k)
        ! This visual continues from the visual in find_active_layer_interfaces
        !
        !    <-- velocity magnitude
        !  0 m/s          --- k = 4 (k_top; velocity = 0)
        !              ==  |
        !            ====  |
        ! -6 m/s   ====== --- k = 3 (k_max_velocity; velocity = maximal)
        !           =====  |
        !            ====  |
        ! -3 m/s      === --- k = 2
        !              ==  |
        !               =  |
        !  0 m/s          --- k = 1 (k_start; velocity = 0)
        !                  |
        !                  |
        !  0 m/s          --- k = 0
        !
        ! Using the velocity distribution, delta velocities are computed at layer indices (K)
        ! The delta velocities are then multiplied by the flow cell area to get the discharge per layer
        !
        ! This results in the following discharge distribution per layer index (K):
        ! + indicates a source, - indicates a sink
        !
        !    <-- discharge magnitude
        !                 ---
        ! +6 m3/s  ++++++  |  K=4
        !                  |
        !                 ---
        ! -3 m3/s     ---  |  K=3
        !                  |
        !                 ---
        ! -3 m3/s     ---  |  K=2
        !                  |
        !                 ---
        !  0 m3/s          |  K=1
        !                  |
        !                 ---
        !
        ! The discharge distribution always sums to zero for all 3D layers in a 2D cell


        ! Initialize discharge and vertical velocity arrays
        discharge = 0.0_dp
        vertical_velocity = 0.0_dp

        ! Fill vertical velocity array
        do l = 1, kmx+1
            k = kbot(flow_cell_index) + l - 2
            if (k < k_start .or. k > k_stop) then
                vertical_velocity(l) = 0.0_dp ! Outside bubble screen active layers
            else if (k <= k_max_velocity) then
                vertical_fraction = (zws(k) - zws(k_start)) / (zws(k_max_velocity) - zws(k_start))
                vertical_velocity(l) = max_velocity * vertical_fraction
            else
                vertical_fraction = (zws(k_stop) - zws(k)) / (zws(k_stop) - zws(k_max_velocity))
                vertical_velocity(l) = max_velocity * vertical_fraction
            end if
        end do

        ! Compute discharge using vertical velocity profile
        do l = 1, kmx
            delta_velocity = vertical_velocity(l+1) - vertical_velocity(l)
            discharge(1, l) = delta_velocity * ba(flow_cell_index)
        end do

    end subroutine compute_water_discharge

    !> Computes the vertical distribution of constituent discharges for a bubble screen in a flow cell
    subroutine compute_constituent_discharge(flow_cell_index, k_start, k_stop, k_max_velocity, discharge)
        ! Parameters
        integer, intent(in) :: flow_cell_index !< 2D flow cell index {in network_data::netcell}
        integer, intent(in) :: k_start !< Start active layer index (bottom) {in m_flow::zws}
        integer, intent(in) :: k_stop !< Stop active layer index (top) {in m_flow::zws}
        integer, intent(in) :: k_max_velocity !< Layer index with maximum downward velocity {in m_flow::zws}
        real(kind=dp), dimension(1+numconst, kmx), intent(inout) :: discharge !< Discharge for transported substances in concentration (m3/s) size:{numconst, kmx}

        ! Local variables
        integer :: i !< Loop index
        integer :: k !< Layer index
        integer :: l !< Local layer index within flow cell
        real(kind=dp) :: layer_volume !< Volume of the layer
        real(kind=dp) :: source_fraction !< Fraction of source for constituent discharges
        real(kind=dp) :: total_water_discharge !< Total water discharge
        real(kind=dp), dimension(numconst) :: total_constituent_discharge !< Total constituent discharge per constituent

        ! Initialize constituents_discharge array
        discharge(2:numconst+1, :) = 0.0_dp
        total_water_discharge = 0.0_dp
        total_constituent_discharge = 0.0_dp

        ! First compute constituent discharges for sink layers
        do k = k_start+1, k_max_velocity
            l = k - kbot(flow_cell_index) + 1 ! Convert to local layer index in flow cell
            total_water_discharge = total_water_discharge + discharge(1, l)

            do i = 1, numconst
                layer_volume = (zws(k) - zws(k-1)) * ba(flow_cell_index)
                discharge(i+1, l) = discharge(1, l) * constituents(i, k) / layer_volume
                total_constituent_discharge(i) = total_constituent_discharge(i) + discharge(i+1, l)
            end do
        end do

        ! Then compute constituent discharges for source layers
        do k = k_max_velocity+1, k_stop
            l = k - kbot(flow_cell_index) + 1 ! Convert to local layer index in flow cell
            source_fraction = -1.0_dp * discharge(1, l) / total_water_discharge ! Fraction of source is proportional to water discharge
            do i = 1, numconst
                discharge(i+1, l) = -1.0_dp * total_constituent_discharge(i) * source_fraction
            end do
        end do

    end subroutine compute_constituent_discharge

    !> Writes the computed discharges for a bubble screen in a flow cell to the source/sink discharge array {fm_external_forcings_data::qstss}
    subroutine write_discharge_to_source_sinks(flow_cell, discharge)
        ! Parameters
        type(t_BubbleScreenFlowCell), intent(in) :: flow_cell !< Flow cell data structure
        real(kind=dp), dimension(1+numconst, kmx), intent(in) :: discharge !< Discharge array for water and constituents for all layers in 2D flow cell

        ! Local variables
        integer :: i !< Source/sink index
        integer :: j !< Constituent index
        integer :: k !< Layer index
        
        do i = 1, flow_cell%num_sources_sinks
            k = ksrc(5, flow_cell%start_index + i - 1) - kbot(flow_cell%cell_index) + 1 ! Convert source/sink layer index to local layer index in flow cell
            do j = 1, numconst+1
                qstss((1+numconst)*(flow_cell%start_index + i - 2) + j) = discharge(j, k) ! Write discharge to source/sink discharge array
            end do
        end do

    end subroutine write_discharge_to_source_sinks

end module m_bubblescreen
