module m_source_sink
   use precision, only: dp
   use messageHandling, only: msgbuf, err_flush, warn_flush
   use m_flow, only: kmx, zws, vol1
   use m_missing, only: dmiss
   use m_transport, only: numconst, constituents

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

      integer :: n_sink = 0 !< [-] Flow cell index of sink.
      integer :: n_source = 0 !< [-] Flow cell index of source.
      integer :: k_bottom_sink = 0 !< [-] Bottom layer index of sink flow cell.
      integer :: k_top_sink = 0 !< [-] Top layer index of sink flow cell.
      integer :: k_bottom_source = 0 !< [-] Bottom layer index of source flow cell.
      integer :: k_top_source = 0 !< [-] Top layer index of source flow cell.

      real(kind=dp) :: area = 0.0_dp !< [m2] area of source/sink. If zero, source/sink transports no momentum.
      real(kind=dp), dimension(2) :: discharge_cosine !< [-] Cosine of discharge on sink side (1) and source side (2).
      real(kind=dp), dimension(2) :: discharge_sine !< [-] Sine of discharge on sink side (1) and source side (2).

      real(kind=dp) :: discharge = 0.0_dp !< [m3/s] Water discharge of source/sink.
      real(kind=dp), dimension(:), allocatable :: constituents !< [ppt,degC,kg/m3] Constituents of source/sink discharges on sink side (1) and source side (2).
   
      logical :: add_k_to_turkin !< [-] Add k of sources to turkin (.false. = no, .true. = yes).

      real(kind=dp), dimension(:), allocatable :: cumulative_volume !< [m3] Cumulative volume at source/sink from Tstart to now.
      real(kind=dp), dimension(:), allocatable :: cumulative_volume_previous !< [m3] Cumulative volume at source/sink from Tstart to the previous His-output time.
      real(kind=dp), dimension(:), allocatable :: average_discharge_previous !< [m3/s] Average discharge in the past his-interval at source/sink.
      integer, dimension(:), allocatable :: waq_index !< [-] Index array to map source/sink to waq source/sink arrays.
      real(kind=dp), dimension(:), allocatable :: cumulative_discharge_waq !< [m3/s] Cumulative discharge at source/sink within current waq-timestep.
      real(kind=dp), dimension(:), allocatable :: cumulative_discharge_waq_previous !< [m3/s] Cumulative discharge at source/sink within current waq-timestep at the beginning of the time step before possible reduction.
   
   contains
      procedure :: update_layer_indices => update_source_sink_layer_indices
      procedure :: set_discharge => set_source_sink_discharge
      procedure :: set_constituents => set_source_sink_constituents
      procedure :: write_discharge_to_qin => write_source_sink_discharge_to_qin

   end type SourceSink

   !> Type that contains all source/sinks.
   type :: AllSourceSinks
      private

      integer :: number = 0 !< [-] Number of source/sinks.
      type(SourceSink), dimension(:), allocatable :: list !< [-] Array of source/sinks.
      real(kind=dp), dimension(:,:), allocatable :: discharge_administration !< [m3/s,ppt,degC,kg/m3] Discharge and constituent administration for all source/sinks, used for partitioned models.
      real(kind=dp), dimension(:,:), allocatable, public :: all_discharges !< [m3/s,ppt,degC,kg/m3] Source/sink water discharge (1) and constituent concentrations of discharge (2:).
   
   contains
      procedure :: initialize => initialize_all_source_sinks
      procedure :: add => add_source_sink
      procedure :: set_discharges => set_all_source_sink_discharges

   end type AllSourceSinks

   type(AllSourceSinks), target :: source_sinks !< [-] All source/sinks in the model.
   
   contains

   ! Type-bound precedures for AllSourceSinks
   ! ====================================================================================================

   !> Initializes the AllSourceSinks type by allocating the list of source/sinks based on the number of source/sinks.
   subroutine initialize_all_source_sinks(self, number_source_sink)
      ! Parameters
      class(AllSourceSinks), intent(inout) :: self !< All source/sinks in the model.
      integer, intent(in) :: number_source_sink !< [-] Number of source/sinks.

      self%number = number_source_sink
      allocate(self%list(number_source_sink))
      allocate(self%discharge_administration(2*(1+numconst),number_source_sink))
      allocate(self%all_discharges(numconst+1,number_source_sink))

      self%discharge_administration = 0.0_dp
      self%all_discharges = 0.0_dp

   end subroutine initialize_all_source_sinks

   !> Adds a single source/sink to the AllSourceSinks type by filling in the data for the specified source/sink number.
   function add_source_sink(self, number, name, x, y, z_bottom, z_top, area) result(err)
      use dfm_error, only: DFM_NOERR, DFM_WRONGINPUT
      use geometry_module, only: normalin
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
      integer, dimension(1) :: n_dummy !< [-] Dummy variable for flow cell index when finding nearest flownodes.

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

         if (numconst > 0) then
            allocate(source_sink%constituents(numconst))
            source_sink%constituents = 0.0_dp
         end if

         if (source_sink%num_points == 1) then ! Create point source/sink.
            ! Point source/sinks are created as sources, so only the source flow cell is filled.
            call find_nearest_flownodes(1, source_sink%x(1), source_sink%y(1), [name], n_dummy, jakdtree, -1, INDTP_ALL)
            source_sink%n_source = n_dummy(1)

            if (present(area)) then
               if (area /= dmiss .and. area /= 0.0_dp) then
                  write (msgbuf, '(a)') 'Area specified for point source/sink ['//trim(name)//'] will be ignored since momentum transport only applies to linked sources/sinks.'
                  call warn_flush()
               end if
            end if

         else ! Create linked source/sink.
            ! Get flow cell indices for sink and source side.
            call find_nearest_flownodes(1, source_sink%x(1), source_sink%y(1), [name], n_dummy, jakdtree, -1, INDTP_ALL)
            source_sink%n_sink = n_dummy(1)
            call find_nearest_flownodes(1, source_sink%x(2), source_sink%y(2), [name], n_dummy, jakdtree, -1, INDTP_ALL)
            source_sink%n_source = n_dummy(1)

            ! Momentum transport only applies if both sink and source are in a flowcell. 
            ! If so, assign area to source/sink and compute discharge angles at sink and source side.
            if (source_sink%n_sink > 0 .and. source_sink%n_source > 0) then
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
         if (source_sink%n_sink == 0 .and. source_sink%n_source == 0) then
            err = DFM_WRONGINPUT
            write (msgbuf, '(a)') 'Source/sink is outside model area for '//trim(name)
            call err_flush()
         end if
      end associate

      ! Check if the sink location of the current source/sink coincides with the sink or source location of any existing source/sinks. If so, raise a warning.
      do i = 1, number - 1
         if (self%list(i)%n_sink /= 0 .and. self%list(i)%n_sink == self%list(number)%n_sink) then
            write (msgbuf, '(a)') 'Sink location of '//trim(self%list(number)%name)//' coincides with sink location of '//trim(self%list(i)%name)
            call warn_flush()
         else if (self%list(i)%n_source /= 0 .and. self%list(i)%n_source == self%list(number)%n_sink) then
            write (msgbuf, '(a)') 'Sink location of '//trim(self%list(number)%name)//' coincides with source location of '//trim(self%list(i)%name)
            call warn_flush()
         end if 
      end do

   end function add_source_sink

   !> Sets the discharges and constituent concentrations of all source/sinks.
   subroutine set_all_source_sink_discharges(self)
      ! Parameters
      class(AllSourceSinks), intent(inout) :: self !< All source/sinks in the model.

      ! Local variables
      integer :: i !< [-] Source/sink index.
      integer :: k !< [-] Layer index.
      real(kind=dp) :: water_column_height !< [m] Height of water column in sink flow cell.
      real(kind=dp) :: layer_discharge !< [m3/s] Discharge in layer of sink flow cell.

      ! Loop over all source/sinks to set water discharge and constituent concentrations.
      do i = 1, self%number
         associate (source_sink => self%list(i))

            ! Update source/sink layer indices
            call source_sink%update_layer_indices()

            ! Set discharge of source/sink, checking if it exceeds maximum extraction and adjusting if necessary.
            call source_sink%set_discharge(self%all_discharges(1, i))

            ! Set constituent concentrations of source/sink discharge.
            if (numconst > 0) then
               call source_sink%set_constituents(self%all_discharges(2:numconst+1, i))
            end if

            ! Write discharge of source/sink to qin variable.
            call source_sink%write_discharge_to_qin()
            
         end associate
      end do

   end subroutine set_all_source_sink_discharges

   ! Type-bound precedures for SourceSink
   ! ====================================================================================================

   !> Updates the layer indices administration for a source/sink.
   subroutine update_source_sink_layer_indices(self)
      ! Parameters
      class(SourceSink), intent(inout) :: self !< Source/sink.

      ! Sink-side
      if (self%n_sink > 0) then
         self%k_bottom_sink = find_layer_index(self%n_sink, self%z_bottom(1))
         self%k_top_sink = find_layer_index(self%n_sink, self%z_top(1))
      end if

      ! Source-side
      if (self%n_source > 0) then
         self%k_bottom_source = find_layer_index(self%n_source, self%z_bottom(2))
         self%k_top_source = find_layer_index(self%n_source, self%z_top(2))
      end if

   end subroutine update_source_sink_layer_indices

   !> Sets the discharge of the source/sink and checks if it exceeds the maximum extraction, if so adjusts discharge and issues warning message.
   subroutine set_source_sink_discharge(self, specified_discharge)
      use m_flowtimes, only: dts

      ! Parameters
      class(SourceSink), intent(inout) :: self !< Source/sink for which to check maximum extraction.
      real(kind=dp), intent(in) :: specified_discharge !< [m3/s] User-specified discharge of source/sink.

      ! Local variables
      integer :: k !< [-] Layer index.
      real(kind=dp) :: maximum_extraction !< [m3/s] Maximum allowed extraction based on cell volume.
      real(kind=dp) :: sink_volume !< [m3] Volume of flow cell corresponding to sink side of source/sink.
      real(kind=dp), parameter :: FRAC = 0.5_dp !< [-] Fraction of cell volume that can be extracted at most in one time step to prevent numerical instability.

      ! Initialization
      sink_volume = 0.0_dp

      self%discharge = specified_discharge

      ! If sink side acts as sink, check if maximum extraction is exceeded.
      if (self%n_sink > 0 .and. self%discharge > 0.0_dp) then

         ! Get total volume of sink layer(s)
         if (kmx == 0) then
            sink_volume = vol1(self%n_sink)
         else
            do k = self%k_bottom_sink, self%k_top_sink
               sink_volume = sink_volume + vol1(k)
            end do
         end if

         maximum_extraction = FRAC * sink_volume / dts
         if (maximum_extraction < self%discharge) then
            self%discharge = maximum_extraction
            write (msgbuf, '(a)') 'Discharge of source/sink ['//trim(self%name)//'] exceeds maximum extraction. Discharge has been reduced to maximum extraction.'
            call warn_flush()
         end if         
      end if

      ! If source side acts as sink, check if maximum extraction is exceeded.
      if (self%n_source > 0 .and. self%discharge < 0.0_dp) then

         ! Get total volume of sink layer(s)
         if (kmx == 0) then
            sink_volume = vol1(self%n_source)
         else
            do k = self%k_bottom_source, self%k_top_source
               sink_volume = sink_volume + vol1(k)
            end do
         end if

         maximum_extraction = FRAC * sink_volume / dts
         if (maximum_extraction < abs(self%discharge)) then
            self%discharge = -maximum_extraction
            write (msgbuf, '(a)') 'Discharge of source/sink ['//trim(self%name)//'] exceeds maximum extraction. Discharge has been reduced to maximum extraction.'
            call warn_flush()
         end if         
      end if

   end subroutine set_source_sink_discharge

   !> Sets the constituent concentrations of source/sink discharge based on user-specified constituent concentrations.
   subroutine set_source_sink_constituents(self, specified_constituents)
      ! Parameters
      class(SourceSink), intent(inout) :: self !< Source/sink for which to compute constituent content.
      real(kind=dp), dimension(numconst), intent(in) :: specified_constituents !< [ppt,degC,kg/m3] User-specified constituent concentrations for the source/sink.

      ! Local variables
      integer :: i_constituent !< [-] Constituent index.
      integer :: k !< [-] Layer index.
      real(kind=dp) :: sink_volume !< [m3] Volume of flow cell corresponding to sink side of source/sink.
      real(kind=dp), dimension(numconst) :: constituent_content !< [ppt,degC,kg/m3] Constituent content of source/sink discharge.

      if (self%n_sink > 0 .neqv. self%n_source > 0) then ! Only source or sink, apply constituent concentrations of discharge directly

         self%constituents(1:numconst) = specified_constituents(1:numconst)

      else if (self%n_sink > 0 .and. self%n_source > 0) then ! Linked source/sink, apply delta constituents between source and sink

         if (kmx == 0) then ! 2D

            self%constituents(1:numconst) = constituents(self%n_sink, 1:numconst) + specified_constituents(1:numconst)

         else ! 3D

            sink_volume = 0.0_dp
            constituent_content(1:numconst) = 0.0_dp

            if (self%discharge > 0.0_dp) then ! Flow from source to sink
               do k = self%k_bottom_sink, self%k_top_sink
                  sink_volume = sink_volume + vol1(k)
                  constituent_content(1:numconst) = constituent_content(1:numconst) + constituents(k,1:numconst) * vol1(k)
               end do

            else ! Flow from sink to source
               do k = self%k_bottom_source, self%k_top_source
                  sink_volume = sink_volume + vol1(k)
                  constituent_content(1:numconst) = constituent_content(1:numconst) + constituents(k,1:numconst) * vol1(k)
               end do

            end if

            self%constituents(1:numconst) = constituent_content(1:numconst) / sink_volume + specified_constituents(1:numconst)

         end if
      end if

   end subroutine set_source_sink_constituents

   !> Writes the discharge of the source/sink to the qin variable.
   subroutine write_source_sink_discharge_to_qin(self)
      use m_flow, only: qin, epshs

      ! Parameters
      class(SourceSink), intent(in) :: self !< Source/sink for which to write discharge.

      ! Local variables
      integer :: k !< [-] Layer index.
      real(kind=dp) :: water_column_height !< [m] Height of water column in sink flow cell.
      real(kind=dp) :: layer_discharge !< [m3/s] Discharge in layer of sink flow cell.

      ! Sink side
      if (self%n_sink > 0) then
         qin(self%n_sink) = qin(self%n_sink) - self%discharge

         if (kmx > 0) then
            water_column_height = zws(self%k_top_sink) - zws(self%k_bottom_sink - 1)

            do k = self%k_bottom_sink, self%k_top_sink

               if (water_column_height > epshs) then
                  layer_discharge = self%discharge * (zws(k) - zws(k-1)) / water_column_height
               else
                  layer_discharge = self%discharge / (self%k_top_sink - self%k_bottom_sink + 1)
               end if

               qin(k) = qin(k) - layer_discharge
            end do
         end if
      end if
      
      ! Source side
      if (self%n_source > 0) then
         qin(self%n_source) = qin(self%n_source) + self%discharge

         if (kmx > 0) then
            water_column_height = zws(self%k_top_source) - zws(self%k_bottom_source - 1)

            do k = self%k_bottom_source, self%k_top_source

               if (water_column_height > epshs) then
                  layer_discharge = self%discharge * (zws(k) - zws(k-1)) / water_column_height
               else
                  layer_discharge = self%discharge / (self%k_top_source - self%k_bottom_source + 1)
               end if

               qin(k) = qin(k) + layer_discharge
            end do
         end if
      end if

   end subroutine write_source_sink_discharge_to_qin

   ! Helper functions
   ! ====================================================================================================

   function find_layer_index(n, z) result(layer_index)
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
