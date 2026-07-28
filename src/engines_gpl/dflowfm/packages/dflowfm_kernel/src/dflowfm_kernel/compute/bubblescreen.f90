module m_bubblescreen
   use precision_basics, only: dp, comparereal
   use fm_external_forcings_data, only: t_BubbleScreen, bubblescreens, bubblescreen_air_discharge
   use m_source_sink, only: source_sinks, source_sink_all_discharges
   use m_alloc, only: realloc
   use messageHandling, only: err_flush, msgbuf, msg_flush, warn_flush

   implicit none(type, external)

   private

   public :: update_bubblescreen_discharge_wrapper
   public :: update_bubblescreen_discharge
   public :: convert_discharge_air_to_water
   public :: find_active_layer_interfaces
   public :: compute_bubblescreen_area

contains

   !> Wrapper subroutine to update the discharges for all bubble screens; loops over all bubble screens and calls update_bubblescreen_discharge for each of them
   subroutine update_bubblescreen_discharge_wrapper()
      ! Local variables
      integer :: i !< Loop index

      do i = 1, size(bubblescreens)
         call update_bubblescreen_discharge(bubblescreens(i), bubblescreen_air_discharge(i))
      end do

   end subroutine update_bubblescreen_discharge_wrapper

   !> Updates the discharges for a single bubble screen object
   subroutine update_bubblescreen_discharge(bubblescreen, air_discharge)
      use m_cell_geometry, only: ba

      ! Parameters
      type(t_BubbleScreen), intent(inout) :: bubblescreen !< Bubble screen data structure
      real(kind=dp), intent(in) :: air_discharge !< Air discharge for this bubble screen

      ! Local variables
      integer :: i_flowcell !< Bubblescreen flow cell index
      integer :: k_start !< Start active layer index (bottom)
      integer :: k_stop !< Stop active layer index (top) (inclusive)
      integer :: k_max_velocity !< Layer index for maximum downward velocity
      integer :: n !< 2D flow cell index; in {network_data::netcell}
      real(kind=dp) :: area_fraction !< Area fraction of the flow cell
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

         call find_active_layer_interfaces(bubblescreen, n, i_flowcell, k_start, k_stop, k_max_velocity)

         if (bubblescreen%is_active(i_flowcell)) then
            call update_bubblescreen_source_sink_layer_indices(bubblescreen%source_sink_indices(i_flowcell), k_start, k_stop, k_max_velocity)
         else 
            local_water_discharge = 0.0_dp
         end if
         call update_bubblescreen_source_sink_discharge(bubblescreen%source_sink_indices(i_flowcell), local_water_discharge)

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

      water_discharge = (alpha0 * air_discharge)**(2.0_dp / 3.0_dp)

   end function convert_discharge_air_to_water

   !> Finds the layer interfaces of the bottom (k_start), top (k_stop) and maximum velocity (k_max_velocity) for a bubble screen in a flow cell
   subroutine find_active_layer_interfaces(bubblescreen, flow_cell_index, flowcell_index, k_start, k_stop, k_max_velocity)
      use m_flow, only: zws, s1
      use m_get_kbot_ktop, only: getkbotktop

      ! Parameters
      type(t_BubbleScreen), intent(inout) :: bubblescreen !< Bubble screen data structure
      integer, intent(in) :: flow_cell_index !< 2D flow cell index; in {network_data::netcell}
      integer, intent(in) :: flowcell_index !< Index of this flow cell within the bubble screen
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
      z_max_velocity = z_top - 0.2_dp * (z_top - bubblescreen%z_level) ! Max velocity is located at 20% below z_top down to z_bot

      ! Find for each z value (bot, max_velocity, top) the closest layer interface
      do k = k_bot, k_top
         if (abs(zws(k) - bubblescreen%z_level) < abs(zws(k_start) - bubblescreen%z_level)) then
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
         ! Currently has insufficient layers - show warning only if this is a state change
         if (bubblescreen%is_active(flowcell_index)) then
            if (zws(k_start) < bubblescreen%z_level) then
               write (msgbuf, '(A,A,A,I0,A,F7.2,A,F7.2,A)') 'Bubble screen "', trim(bubblescreen%id), '" in flow cell ', flow_cell_index, &
                  ' computation no longer possible: water level is below bubble screen z=', &
                  bubblescreen%z_level, ' and z=', zws(k_stop), '.'
            else
               write (msgbuf, '(A,A,A,I0,A,F7.2,A,F7.2,A)') 'Bubble screen "', trim(bubblescreen%id), '" in flow cell ', flow_cell_index, &
                  ' computation no longer possible: insufficient active layers (min 3) between bubble screen z=', &
                  bubblescreen%z_level, ' and water level z=', zws(k_stop), '.'
            end if
            call warn_flush()
            bubblescreen%is_active(flowcell_index) = .false.
         end if
         return
      else
         ! Currently has sufficient layers - show message if recovering from error state
         if (.not. bubblescreen%is_active(flowcell_index)) then
            write (msgbuf, '(A,A,A,I0,A)') 'Bubble screen "', trim(bubblescreen%id), '" in flow cell ', flow_cell_index, ' computation is now possible again.'
            call msg_flush()
            bubblescreen%is_active(flowcell_index) = .true.
         end if
      end if

      ! Require at least 1 layer between k_max_velocity and k_stop; if not adjust k_max_velocity
      if (k_stop - k_max_velocity < 1) then
         k_max_velocity = k_stop - 1
      end if

   end subroutine find_active_layer_interfaces

   !> Update the discharge for a bubble screen source/sink
   subroutine update_bubblescreen_source_sink_discharge(source_sink_index, discharge)
      use m_transport, only: numconst

      ! Parameters
      integer, intent(in) :: source_sink_index !< [-] Index of source/sink
      real(kind=dp), intent(in) :: discharge !< [m3/s] Water discharge for this source/sink

      source_sink_all_discharges(1, source_sink_index) = abs(discharge)

      ! Set constituent discharges to zero for bubble screen source/sinks
      if (numconst /= 0) then
         source_sink_all_discharges(2:numconst + 1, source_sink_index) = 0.0_dp
      end if

   end subroutine update_bubblescreen_source_sink_discharge

   !> Updates the vertical layer indices for a bubble screen linked source/sink
   subroutine update_bubblescreen_source_sink_layer_indices(source_sink_index, k_start, k_stop, k_max_velocity)
      use m_flow, only: zws
      ! Parameters
      integer, intent(in) :: source_sink_index !< [-] Index of source/sink
      integer, intent(in) :: k_start !< [-] Start active layer index (bottom); in {m_flow::zws}
      integer, intent(in) :: k_stop !< [-] Stop active layer index (top); in {m_flow::zws}
      integer, intent(in) :: k_max_velocity !< [-] Layer index with maximum downward velocity; in {m_flow::zws}

      ! Update source/sink top and bottom z-levels
      source_sinks%z_bottom(source_sink_index, 1) = (zws(k_start) + zws(k_start + 1)) / 2.0_dp
      source_sinks%z_bottom(source_sink_index, 2) = (zws(k_max_velocity) + zws(k_max_velocity + 1)) / 2.0_dp
      source_sinks%z_top(source_sink_index, 1) = (zws(k_max_velocity) + zws(k_max_velocity - 1)) / 2.0_dp
      source_sinks%z_top(source_sink_index, 2) = (zws(k_stop) + zws(k_stop - 1)) / 2.0_dp

   end subroutine update_bubblescreen_source_sink_layer_indices

!> Computes the total area of a bubble screen based on its flow cells
   function compute_bubblescreen_area(bubblescreen) result(area)
      use m_partitioninfo, only: jampi, reduce_double_sum, idomain, my_rank
      use m_cell_geometry, only: ba
      ! Parameters
      type(t_BubbleScreen), intent(in) :: bubblescreen !< Bubble screen data structure
      real(kind=dp) :: area !< [m2] Area of the bubble screen
      real(kind=dp), dimension(1) :: global_area
      ! Local variables
      integer :: i
      integer :: flownode_nr !< Flow node number

      area = 0.0_dp

      if (jampi == 1 .and. allocated(idomain)) then ! If partitioned, only sum the area of flow cells that are owned by the current partition
         do i = 1, bubblescreen%num_flowcells
            flownode_nr = bubblescreen%flowcell_indices(i)
            if (idomain(flownode_nr) == my_rank) then ! Check if flow cell is owned by current partition
               area = area + ba(flownode_nr)
            end if
         end do
         call reduce_double_sum(1, [area], global_area)

         area = global_area(1) ! Set area to total area across all partitions

      else ! If not partitioned, simply sum the area of all flow cells in the bubble screen
         do i = 1, bubblescreen%num_flowcells
            flownode_nr = bubblescreen%flowcell_indices(i)
            area = area + ba(flownode_nr)
         end do
      end if

   end function compute_bubblescreen_area

end module m_bubblescreen
