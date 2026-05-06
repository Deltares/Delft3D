module m_source_sink
   use precision, only: dp
   use m_missing, only: dmiss

   implicit none(type, external)

   private

   public :: source_sinks

   !> Type that contains all data for a single (linked) source/sink.
   type :: SourceSink
      private

      character(len=255) :: name !< [-] Name of the source/sink.
      integer :: num_points !< [-] Number of x,y points in source/sink polyline.
      real(kind=dp), dimension(:), allocatable :: x !< [m] x-coordinates of source/sink.
      real(kind=dp), dimension(:), allocatable :: y !< [m] y-coordinates of source/sink.
      real(kind=dp), dimension(2) :: z_bottom !< [m] bottom z-level on sink side (1) and source side (2).
      real(kind=dp), dimension(2) :: z_top !< [m] top z-level on sink side (1) and source side (2).
      integer, dimension(2) :: flowcell_indices = 0 !< [-] Flowcell indices of sink side (1) and source side (2).
      integer, dimension(6) :: indices = 0 !< [-] Flow cell and layer indices of source/sink. 
                                           !< 1) flowcell sink, 2) bottom layer sink, 3) top layer sink,
                                           !< 4) flowcell source, 5) bottom layer source, 6) top layer source.

      real(kind=dp) :: area = 0.0_dp !< [m2] area of source/sink. If zero, source/sink transports no momentum.
      real(kind=dp), dimension(2) :: discharge_cosine !< [-] Cosine of discharge on sink side (1) and source side (2).
      real(kind=dp), dimension(2) :: discharge_sine !< [-] Sine of discharge on sink side (1) and source side (2).

      real(kind=dp) :: water_discharge = 0.0_dp !< [m3/s] Water discharge of source/sink.
      real(kind=dp), dimension(:), allocatable :: constituents = 0.0_dp !< [ppt,degC,kg/m3] Constituents of source/sink discharges on sink side (1) and source side (2).
   
      integer, dimension(:), allocatable :: extraction_warning !< [-] Issue a warning message if the extraction flux exceeds the cell volume (0 = no message, 1 = sink extraction too large, 2 = source extraction too large).
      logical :: add_k_to_turkin !< [-] Add k of sources to turkin (.false. = no, .true. = yes).

      real(kind=dp), dimension(:), allocatable :: cumulative_volume !< [m3] Cumulative volume at source/sink from Tstart to now.
      real(kind=dp), dimension(:), allocatable :: cumulative_volume_previous !< [m3] Cumulative volume at source/sink from Tstart to the previous His-output time.
      real(kind=dp), dimension(:), allocatable :: average_discharge_previous !< [m3/s] Average discharge in the past his-interval at source/sink.
      integer, dimension(:), allocatable :: waq_index !< [-] Index array to map source/sink to waq source/sink arrays.
      real(kind=dp), dimension(:), allocatable :: cumulative_discharge_waq !< [m3/s] Cumulative discharge at source/sink within current waq-timestep.
      real(kind=dp), dimension(:), allocatable :: cumulative_discharge_waq_previous !< [m3/s] Cumulative discharge at source/sink within current waq-timestep at the beginning of the time step before possible reduction.
   
   end type SourceSink

   !> Type that contains all source/sinks in the model.
   type :: AllSourceSinks
      private

      integer :: number = 0 !< [-] Number of source/sinks in the model.
      type(SourceSink), dimension(:), allocatable :: list !< [-] List of source/sinks in the model.
      real(kind=dp), dimension(:,:), allocatable :: discharge_administration = 0.0_dp !< [m3/s,ppt,degC,kg/m3] Discharge administration for all source/sinks used for partitioned models.
      real(kind=dp), dimension(:,:), allocatable :: all_discharges = 0.0_dp !< [m3/s,ppt,degC,kg/m3] Source/sink water discharge (1) and constituent concentrations of discharge (2:).
   
   contains
      procedure :: initialize => initialize_all_source_sinks
      procedure :: add => add_source_sink
      procedure :: update_layer_indices => update_source_sink_layer_indices
      procedure :: set_discharges => set_source_sink_discharges

   end type AllSourceSinks

   type(AllSourceSinks), target :: source_sinks !< [-] All source/sinks in the model.
   
   contains

   ! Type-bound precedures for AllSourceSinks
   ! ====================================================================================================

   !> Initializes the AllSourceSinks type by allocating the list of source/sinks based on the number of source/sinks in the model.
   subroutine initialize_all_source_sinks(self, number_source_sink)
      use m_transport, only: numconst

      ! Parameters
      class(AllSourceSinks), intent(inout) :: self !< All source/sinks in the model.
      integer, intent(in) :: number_source_sink !< [-] Number of source/sinks in the model.

      self%number = number_source_sink
      allocate(self%list(number_source_sink))
      allocate(self%discharge_administration(2*(1+numconst),number_source_sink))
      allocate(self%all_discharges(numconst+1,number_source_sink))

   end subroutine initialize_all_source_sinks

   !> Adds a single source/sink to the AllSourceSinks type by filling in the data for the specified source/sink number.
   function add_source_sink(self, number, name, x, y, z_bottom, z_top, area) result(err)
      use dfm_error, only: DFM_NOERR, DFM_WRONGINPUT
      use geometry_module, only: normalin
      use messageHandling, only: msgbuf, err_flush, warn_flush
      use m_find_flownode, only: find_nearest_flownodes
      use m_GlobalParameters, only: INDTP_ALL
      use m_missing, only: dxymis
      use m_sferic, only: jsferic, jasfer3D

      ! Parameters
      class(AllSourceSinks), intent(inout) :: self !< All source/sinks in the model.
      integer, intent(in) :: number !< [-] Source/sink number (index in list).
      character(len=*), intent(in) :: name !< [-] Name of the source/sink.
      real(kind=dp), dimension(:), intent(in) :: x !< [m] x-coordinates of source/sink.
      real(kind=dp), dimension(:), intent(in) :: y !< [m] y-coordinates of source/sink.
      real(kind=dp), dimension(2), intent(in) :: z_bottom !< [m] bottom z-level on sink side (1) and source side (2).
      real(kind=dp), dimension(2), intent(in) :: z_top !< [m] top z-level on sink side (1) and source side (2).
      real(kind=dp), intent(in), optional :: area !< [m2] area of source/sink. If zero, source/sink transports no momentum.
      integer :: err !< [-] Error code. 0 = no error, 1 = wrong input.

      ! Local variables
      integer :: i !< [-] Loop variable.
      integer :: jakdtree !< [-] Flag to indicate if the kd-tree for finding nearest flownodes has been constructed (0 = no, 1 = yes).

      ! Initialization
      err = DFM_NOERR
      jakdtree = 0

      ! Check if only 1 or 2 points are specified
      if (size(x) /= 1 .and. size(x) /= 2) then
         err = DFM_WRONGINPUT
         write (msgbuf, '(a)') 'Invalid length of polyline specified for source/sink: ['//trim(name)//']. Only 1- or 2-length polylines are allowed.'
         call err_flush()
      end if

      associate (source_sink => self%list(number))
         ! Fill source/sink data
         source_sink%name = name
         source_sink%num_points = size(x)
         allocate(source_sink%x(size(x)))
         allocate(source_sink%y(size(y)))
         source_sink%x = x
         source_sink%y = y
         source_sink%z_bottom = z_bottom
         source_sink%z_top = z_top

         ! If num_points = 1, create point source/sink; if num_points = 2, create linked source/sink.
         if (source_sink%num_points == 1) then
            ! Point source/sinks are created as point source (therefore only indices(4) is filled)
            call find_nearest_flownodes(1, source_sink%x(1), source_sink%y(1), [name], source_sink%indices(4), jakdtree, -1, INDTP_ALL)

            if (present(area)) then
               if (area /= dmiss .and. area /= 0.0_dp) then
                  write (msgbuf, '(a)') 'Area specified for point source/sink ['//trim(name)//'] will be ignored since momentum transport only applies to linked sources/sinks.'
                  call warn_flush()
               end if
            end if

         else
            ! Get flow cell indices for sink and source side.
            call find_nearest_flownodes(1, source_sink%x(1), source_sink%y(1), [name], source_sink%indices(1), jakdtree, -1, INDTP_ALL)
            call find_nearest_flownodes(1, source_sink%x(2), source_sink%y(2), [name], source_sink%indices(4), jakdtree, -1, INDTP_ALL)

            ! Momentum transport only applies if both sink and source are in a flowcell. 
            ! If so, assign area to source/sink and compute discharge angles at sink and source side.
            if (source_sink%indices(1) /= 0 .and. source_sink%indices(4) /= 0) then
               if (present(area)) then
                  source_sink%area = area
               end if

               ! Only momentum transport for non-zero area.
               if (source_sink%area > 0.0_dp) then
                  call normalin(source_sink%x(1), source_sink%y(1), source_sink%x(2), source_sink%y(2), source_sink%discharge_cosine(1), &
                     source_sink%discharge_sine(1), source_sink%x(1), source_sink%y(1), jsferic, jasfer3D, dxymis)
                  call normalin(source_sink%x(1), source_sink%y(1), source_sink%x(2), source_sink%y(2), source_sink%discharge_cosine(2), &
                     source_sink%discharge_sine(2), source_sink%x(2), source_sink%y(2), jsferic, jasfer3D, dxymis)
               end if
            end if
         end if

         ! Check if source/sink is outside model area (i.e. both flow cell indices are zero). If so, raise an error.
         if (source_sink%indices(1) == 0 .and. source_sink%indices(4) == 0) then
            err = DFM_WRONGINPUT
            write (msgbuf, '(a)') 'Source/sink is outside model area for '//trim(name)
            call err_flush()
         end if
      end associate

      ! Check if the sink location of the current source/sink coincides with the sink or source location of any existing source/sinks. If so, raise a warning.
      do i = 1, number - 1
         if (self%list(i)%indices(1) /= 0 .and. self%list(i)%indices(1) == self%list(number)%indices(1)) then
            write (msgbuf, '(a)') 'Sink location of '//trim(self%list(number)%name)//' coincides with sink location of '//trim(self%list(i)%name)
            call warn_flush()
         else if (self%list(i)%indices(4) /= 0 .and. self%list(i)%indices(4) == self%list(number)%indices(1)) then
            write (msgbuf, '(a)') 'Sink location of '//trim(self%list(number)%name)//' coincides with source location of '//trim(self%list(i)%name)
            call warn_flush()
         end if 
      end do

   end function add_source_sink

   !> Updates the layer indices administration for all source/sinks.
   subroutine update_source_sink_layer_indices(self)
      ! Parameters
      class(AllSourceSinks), intent(inout) :: self !< All source/sinks in the model.

      ! Local variables
      integer :: i_source_sink !< [-] Index of source/sink in list.

      ! Loop over all source/sinks to update layer indices based on z-levels and flow cell indices.
      do i_source_sink = 1, self%number
         associate (source_sink => self%list(i_source_sink))
            ! Source-side
            if (source_sink%indices(1) /= 0) then
               source_sink%indices(2) = find_layer_index(source_sink%indices(1), source_sink%z_bottom(1))
               source_sink%indices(3) = find_layer_index(source_sink%indices(1), source_sink%z_top(1))
            end if

            ! Sink-side
            if (source_sink%indices(4) /= 0) then
               source_sink%indices(5) = find_layer_index(source_sink%indices(4), source_sink%z_bottom(2))
               source_sink%indices(6) = find_layer_index(source_sink%indices(4), source_sink%z_top(2))
            end if
         end associate
      end do

   end subroutine update_source_sink_layer_indices


   subroutine set_source_sink_discharges(self)
      ! Parameters
      class(AllSourceSinks), intent(inout) :: self !< All source/sinks in the model.

      ! Local variables
      integer :: i_source_sink !< [-] Index of source/sink in list.

      ! Loop over all source/sinks to set water discharge and constituent concentrations.
      do i_source_sink = 1, self%number
         associate (source_sink => self%list(i_source_sink))
            
            if (self%all_discharges(1,i_source_sink) > 0.0_dp) then
               qin(source_sink%indices(4)) = self%all_discharges(1,i_source_sink)
            end if
            
         end associate
      end do

   end subroutine set_source_sink_discharges

   ! Helper functions
   ! ====================================================================================================

   function find_layer_index(n, z) result(layer_index)
      use m_flow, only: kmx, zws
      use m_get_kbot_ktop, only: getkbotktop

      ! Parameters
      integer, intent(in) :: n !< [-] Flowcell index.
      real(kind=dp), intent(in) :: z !< [m] z-level to find layer index for.
      integer :: layer_index !< [-] Layer index corresponding to z-level.

      ! Local variables
      integer :: k !< [-] Layer index.
      integer :: k_bottom !< [-] Bottom layer index of flow cell n.
      integer :: k_top !< [-] Top layer index of flow cell n.

      if (kmx == 0) then ! 2D, return layer_index = flowcell index (n)
         layer_index = n
      else ! 3D, find layer index based on z-level and flow cell n's bottom and top layer indices.
         call getkbotktop(n, k_bottom, k_top)
         do k = k_bottom, k_top-1
            if (z >= zws(k) .and. z < zws(k+1)) then
               layer_index = k
               return
            end if
         end do

         layer_index = 0 ! Return 0 if z-level is outside the vertical extent of the flow cell.
      end if

   end function find_layer_index

end module m_source_sink
