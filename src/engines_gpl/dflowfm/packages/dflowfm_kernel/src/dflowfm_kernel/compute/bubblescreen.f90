module m_bubble_screen
    use precision_basics, only: dp
    use fm_external_forcings_data, only: t_BubbleScreen, t_BubbleScreenFlowCell, bubble_screens, zsrc, zsrc2, ksrc, qstss
    use m_alloc, only: realloc
    use m_cell_geometry, only: ba
    use m_flow, only: s1, zws, zws0
    use m_get_kbot_ktop, only: getkbotktop
    use m_transport, only: numconst
    use messageHandling, only: err_flush, msgbuf, msg_flush

    implicit none(type, external)

    private

contains

    subroutine compute_bubble_screen_vertical_distribution(bubble_screen)       
        ! Parameters
        type(t_BubbleScreen), intent(in) :: bubble_screen !< Bubble screen data structure

        ! Local variables
        integer :: i_flow_cell !< Flow cell index
        integer :: i_source_sink !< Source/sink index
        integer :: k_bot !< Bottom layer index
        integer :: k_src !< Source/sink layer index
        integer :: k_top !< Top layer index
        integer :: k_max_velocity !< Layer index for maximum downward velocity
        integer :: n !< Flow cell index
        real(kind=dp) :: area_frac !< Area fraction of the flow cell
        real(kind=dp) :: delta_velocity !< Change in vertical velocity per layer
        real(kind=dp) :: max_velocity !< Maximum downward vertical velocity for this flow cell
        real(kind=dp) :: total_area !< Total area of the bubble screen
        real(kind=dp) :: total_discharge_air !< Air discharge for this bubble screen
        real(kind=dp) :: total_discharge_water !< Water discharge for this bubble screen
        real(kind=dp) :: vertical_fraction !< Fractional vertical position within bubble screen
        real(kind=dp), dimension(:), allocatable :: vertical_velocity 
        type(t_BubbleScreenFlowCell) :: flow_cell !< Current flow cell

        ! Lookup total discharge air for this bubble screen and compute water discharge
        ! ====================================================================================================
        ! TODO: Implement actual lookup logic here to EC module (UNST-9564)

        total_discharge_air = 100.0_dp ! Placeholder value
        ! total_discharge_air = qstss(flow_cell%start_index) ! Get air discharge from first source/sink in array (all source/sinks are set to the same value by the EC module)
        ! ====================================================================================================
        total_discharge_water = compute_bubble_screen_water_discharge_from_air(total_discharge_air)

        total_area = compute_bubble_screen_area(bubble_screen)

        ! Compute vertical distribution for each flow cell
        do i_flow_cell = 1, bubble_screen%num_flow_cells
            flow_cell = bubble_screen%flow_cells(i_flow_cell)
            n = flow_cell%cell_index

            ! TODO: Check if the flow cell contains enough vertical layers (min 3)
            ! ====================================================================================================

            ! Compute maximum downward vertical velocity based on area fraction
            area_frac = ba(n) / total_area
            max_velocity = -1.0_dp * total_discharge_water * area_frac / ba(n)

            ! Get top and bottom indices of active layers in the bubble screen and layer index with maximum downward velocity
            call find_active_layer_indices(flow_cell, bubble_screen%id, k_bot, k_top, k_max_velocity)

            ! Reset vertical velocity array
            if (.not. allocated(vertical_velocity)) then
                allocate(vertical_velocity(flow_cell%num_sources_sinks))
            else if (size(vertical_velocity) /= flow_cell%num_sources_sinks) then
                call realloc(vertical_velocity, flow_cell%num_sources_sinks)
            end if
            vertical_velocity = 0.0_dp

            ! Set vertical velocity profile (simple triangular profile with max at k_max_velocity)
            do i_source_sink = flow_cell%start_index, flow_cell%start_index + flow_cell%num_sources_sinks - 1
                k_src = ksrc(2, i_source_sink)
                if (k_src < k_bot .or. k_src > k_top) then
                    cycle ! Source/sink is outside the bubble screen active layers
                end if

                if (k_src < k_max_velocity) then
                    vertical_fraction = (zws(k_src) - zws0(k_bot)) / (zws(k_max_velocity) - zws0(k_bot))
                    vertical_velocity(i_source_sink) = max_velocity * vertical_fraction
                else
                    vertical_fraction = (zws(k_top) - zws0(k_src)) / (zws(k_top) - zws0(k_max_velocity))
                    vertical_velocity(i_source_sink) = max_velocity * vertical_fraction
                end if                
            end do

            ! TODO: Compute sinks and sources for transported substances (heat, salt, tracers, etc.)
            ! ====================================================================================================

            ! Set discharges to source/sinks
            do i_source_sink = flow_cell%start_index + 1, flow_cell%start_index + flow_cell%num_sources_sinks - 1
                delta_velocity = vertical_velocity(i_source_sink) - vertical_velocity(i_source_sink - 1)
                qstss((1 + numconst) * (i_source_sink - 1) + 1) = delta_velocity * ba(n)
            end do
        end do

    end subroutine compute_bubble_screen_vertical_distribution

    !> Computes the water discharge rate from the air discharge rate for a bubble screen.
    !! Uses an empirical formula based on an empirical coefficient alpha (defaults to 1000)
    function compute_bubble_screen_water_discharge_from_air(discharge_air) result(discharge_water)
        ! Parameters
        real(kind=dp), intent(in) :: discharge_air !< [m3/s] Air discharge rate
        real(kind=dp) :: discharge_water !< [m3/s] Resulting water discharge rate

        ! Compute water discharge using empirical formula
        discharge_water = (1000.0_dp * discharge_air) ** (2.0_dp / 3.0_dp)

    end function compute_bubble_screen_water_discharge_from_air

    !> Computes the total area of a bubble screen based on its flow cells
    function compute_bubble_screen_area(bubble_screen) result(area)
        ! Parameters
        type(t_BubbleScreen), intent(in) :: bubble_screen !< Bubble screen data structure
        real(kind=dp) :: area !< [m2] Area of the bubble screen

        ! Local variables
        integer :: i

        area = 0.0_dp

        do i = 1, bubble_screen%num_flow_cells
            area = area + ba(bubble_screen%flow_cells(i)%cell_index)
        end do

    end function compute_bubble_screen_area

    !> Finds the layer index of the lowest and highest active source/sinks in the bubble screen and the layer index with maximum downward vertical velocity in a flow cell
    subroutine find_active_layer_indices(flow_cell, bubble_screen_id, k_low, k_high, k_max_velocity)
        ! Parameters
        type(t_BubbleScreenFlowCell), intent(in) :: flow_cell !< Flow cell data structure
        character(len=*), intent(in) :: bubble_screen_id !< Bubble screen id
        integer, intent(out) :: k_low !< Layer index of lowest active source/sink in bubble screen
        integer, intent(out) :: k_high !< Layer index of highest active source/sink in bubble screen
        integer, intent(out) :: k_max_velocity !< Layer index with maximum downward velocity

        ! Local variables
        integer :: k
        integer :: k_bot !< Bottom layer index from getkbotktop
        integer :: k_top !< Top layer index from getkbotktop
        integer :: n !< Flow cell index
        real(kind=dp) :: z_top !< Top elevation of the flow cell
        real(kind=dp) :: z_bottom !< Bottom elevation of the flow cell
        real(kind=dp) :: z_max_velocity !< Elevation of maximum downward velocity

        n = flow_cell%cell_index
        k_low = -1
        k_high = -1
        k_max_velocity = -1

        z_top = s1(n) ! Top elevation is set to water level in the flow cell
        z_bottom = flow_cell%z_level ! Bottom elevation is set to bubble screen vertical level      
        z_max_velocity = z_bottom + 0.2_dp * (z_top - z_bottom)
        
        call getkbotktop(n, k_bot, k_top)

        do k = k_bot, k_top
            if (z_bottom > zws0(k) .and. z_bottom < zws(k)) then
                k_low = k
            else if (z_max_velocity > zws0(k) .and. z_max_velocity < zws(k)) then
                k_max_velocity = k
            else if (z_top > zws0(k) .and. z_top < zws(k)) then
                k_high = k
            end if
        end do

        if (k_low == -1 .or. k_high == -1 .or. k_max_velocity == -1) then
            write (msgbuf, '(a)') 'Error: Unable to find active layer indices for bubble screen with id '//trim(bubble_screen_id)//'.'
            call err_flush()
        end if

    end subroutine find_active_layer_indices

end module m_bubble_screen
