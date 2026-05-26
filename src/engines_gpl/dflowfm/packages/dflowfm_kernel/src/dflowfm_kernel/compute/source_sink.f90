module m_source_sink
   use precision, only: dp
   use dfm_error, only: DFM_NOERR, DFM_WRONGINPUT
   use messagehandling, only: IDLEN, LEVEL_WARN, msgbuf, warn_flush, mess
   use m_alloc, only: realloc
   use m_missing, only: dmiss, dxymis
   use m_transport, only: numconst, constituents

   implicit none(type, external)

   private

   public :: source_sinks
   public :: num_source_sink
   public :: source_sink_all_discharges

   public :: num_source_sink_oldfile, num_source_sink_for_nearfield, max_source_sink_polyline_points
   public :: source_sink_cumulative_volume, source_sink_cumulative_volume_previous, source_sink_average_discharge_previous, source_sink_waq_index, source_sink_cumulative_discharge_waq, source_sink_cumulative_discharge_waq_previous

   public :: addsorsin_from_polyline_file
   public :: addsorsin
   public :: setsorsin

   ! Type containing all source/sink data.
   type :: SourceSinks

      ! Number of source/sinks and name.
      integer :: number = 0 !< [-] Number of source/sinks in the model.
      character(len=255), dimension(:), allocatable :: name !< [-] Name of the source/sink.

      ! Source/sink x,y,z coordinates, and indices.
      real(kind=dp), dimension(:,:), allocatable :: x !< [m] x-coordinates of source/sink.
      real(kind=dp), dimension(:,:), allocatable :: y !< [m] y-coordinates of source/sink.
      real(kind=dp), dimension(:,:), allocatable :: z_bottom !< [m] z-level of bottom source/sink.
      real(kind=dp), dimension(:,:), allocatable :: z_top !< [m] z-level of top source/sink.
      integer, dimension(:,:), allocatable :: indices !< [-] Indices of source and sink side.
                                                      !< (1) flowcell sink, (2) bottom layer index sink, (3) top layer index sink,
                                                      !< (4) flowcell source, (5) bottom layer index source, (6) top layer index source.

      ! Source/sink momentum transport variables.
      real(kind=dp), dimension(:), allocatable :: area !< [m2] area of source/sink. If zero, source/sink transport no momentum.
      real(kind=dp), dimension(:,:), allocatable :: discharge_cosine !< [-] Cosine of discharge on sink side (1) and source side (2).
      real(kind=dp), dimension(:,:), allocatable :: discharge_sine !< [-] Sine of discharge on sink side (1) and source side (2).

      ! Source/sink discharge variables.
      real(kind=dp), dimension(:), allocatable :: discharge !< [m3/s] Water discharge of source/sink.
      real(kind=dp), dimension(:,:), allocatable :: constituents !< [ppt,degC,kg/m3] Constituents of source/sink discharges.

      logical :: add_k_to_turkin = .false. !< [-] Add k of source/sink to turkin.
      integer, dimension(:), allocatable :: max_xy_points !< [-] Maximum number of points per source/sink in x, y. Used for array dimensions.

   contains

      procedure :: initialize => initialize_source_sinks
      procedure :: resize => resize_source_sinks

   end type SourceSinks

   ! Object containing all source/sink data.
   type(SourceSinks) :: source_sinks

   integer :: num_source_sink !< [-] number of source/sinks in the model. {former:numsrc}
   real(kind=dp), dimension(:,:), allocatable, target :: source_sink_all_discharges !< [m3/s] All source/sink discharges in one array for partitioned models. {size=(num_source_sink,2), former:srcall}
   real(kind=dp), dimension(:,:), allocatable :: source_sink_reduction !< [-] Source/sink reduction array for partitioned models. {size=(2*(numconst+1),num_source_sink), former:srsn}   


   ! Source/sink counters.
   integer :: num_source_sink_oldfile !< [-] number of source/sinks in old extforce file. {former:numsrc_old}
   integer :: num_source_sink_for_nearfield !< [-] number of source/sinks added for near field. {former:numsrc_nf}
   integer :: max_source_sink_polyline_points !< [-] maximum number of points in source_sink_x, source_sink_y over all sources/sinks. Used for array dimensions. {former:msrc}

   ! Cumulative volume and discharge variables. Used in output and for waq coupling.
   real(kind=dp), dimension(:), target, allocatable :: source_sink_cumulative_volume !< [m3] Cumulative volume at each source/sink from Tstart to now. {size=(num_source_sink), former:vsrccum}
   real(kind=dp), dimension(:), target, allocatable :: source_sink_cumulative_volume_previous !< [m3] Cumulative volume at each source/sink from Tstart to the previous His-output time. {size=(num_source_sink), former:vsrccum_pre}
   real(kind=dp), dimension(:), target, allocatable :: source_sink_average_discharge_previous !< [m3/s] Average discharge in the past his-interval at each source/sink. {size=(num_source_sink), former:qsrcavg}
   integer, dimension(:), allocatable :: source_sink_waq_index !< [-] Index array to map source/sink to waq source/sink arrays. {size=(num_source_sink), former:ksrcwaq}
   real(kind=dp), dimension(:), allocatable :: source_sink_cumulative_discharge_waq !< [m3/s] Cumulative discharge at each source/sink within current waq-timestep. {size=(num_source_sink), former:qsrcwaq}
   real(kind=dp), dimension(:), allocatable :: source_sink_cumulative_discharge_waq_previous !< [m3/s] Cumulative discharge at each source/sink within current waq-timestep at the beginning of the time step before possible reduction. {size=(num_source_sink), former:qsrcwaq0}

contains

   ! SourceSinks type-bound procedures.
   ! ====================================================================================================

   !> Allocates and initializes the SourceSinks attributes to size.
   subroutine initialize_source_sinks(self, size)
      ! Parameters
      class(SourceSinks), intent(inout) :: self
      integer, intent(in) :: size

      ! Initialize source/sink counter to zero.
      num_source_sink = 0
      num_source_sink_for_nearfield = 0

      ! Allocate and intialize global source/sink arrays.
      allocate (source_sink_all_discharges(numconst+1, size))
      allocate (source_sink_reduction(2*(numconst+1), size))

      source_sink_all_discharges = 0.0_dp
      source_sink_reduction = 0.0_dp

      allocate (source_sink_cumulative_volume(size))
      allocate (source_sink_cumulative_volume_previous(size))
      allocate (source_sink_average_discharge_previous(size))
      allocate (source_sink_waq_index(size))
      allocate (source_sink_cumulative_discharge_waq(size))
      allocate (source_sink_cumulative_discharge_waq_previous(size))

      source_sink_cumulative_volume = 0.0_dp
      source_sink_cumulative_volume_previous = 0.0_dp
      source_sink_average_discharge_previous = 0.0_dp
      source_sink_waq_index = 0
      source_sink_cumulative_discharge_waq = 0.0_dp
      source_sink_cumulative_discharge_waq_previous = 0.0_dp

      ! Allocate all source/sink attributes.
      allocate (self%name(size))
      allocate (self%x(size, 2))
      allocate (self%y(size, 2))
      allocate (self%z_bottom(size, 2))
      allocate (self%z_top(size, 2))
      allocate (self%indices(size, 6))

      allocate (self%area(size))
      allocate (self%discharge_cosine(size, 2))
      allocate (self%discharge_sine(size, 2))

      allocate (self%discharge(size))
      allocate (self%constituents(size, numconst))

      allocate (self%max_xy_points(size))

      ! Initialize all source/sink attributes.
      self%name = ''
      self%x = dmiss
      self%y = dmiss
      self%z_bottom = dmiss
      self%z_top = dmiss
      self%indices = 0

      self%area = 0.0_dp
      self%discharge_cosine = 0.0_dp
      self%discharge_sine = 0.0_dp

      self%discharge = 0.0_dp
      self%constituents = 0.0_dp

      self%add_k_to_turkin = .false.
      self%max_xy_points = 0

   end subroutine initialize_source_sinks

   !> Resizes the SourceSinks object to size, keeping existing values if possible.
   subroutine resize_source_sinks(self, new_size)
      ! Parameters
      class(SourceSinks), intent(inout) :: self
      integer, intent(in) :: new_size

      ! Only resize if the new size is larger than the current size.
      if (new_size > size(self%name)) then

         ! Resize global source/sink arrays.
         call realloc(source_sink_all_discharges, [numconst+1, new_size], keepExisting=.true., fill=0.0_dp)
         call realloc(source_sink_reduction, [2*(numconst+1), new_size], keepExisting=.true., fill=0.0_dp)
         call realloc(source_sink_cumulative_volume, new_size, keepExisting=.true., fill=0.0_dp)
         call realloc(source_sink_cumulative_volume_previous, new_size, keepExisting=.true., fill=0.0_dp)
         call realloc(source_sink_average_discharge_previous, new_size, keepExisting=.true., fill=0.0_dp)
         call realloc(source_sink_waq_index, new_size, keepExisting=.true., fill=0)
         call realloc(source_sink_cumulative_discharge_waq, new_size, keepExisting=.true., fill=0.0_dp)
         call realloc(source_sink_cumulative_discharge_waq_previous, new_size, keepExisting=.true., fill=0.0_dp)

         ! Resize all source/sink arrays.
         call realloc(self%name, new_size, keepExisting=.true., fill='')
         call realloc(self%x, [new_size, 2], keepExisting=.true., fill=dmiss)
         call realloc(self%y, [new_size, 2], keepExisting=.true., fill=dmiss)
         call realloc(self%z_bottom, [new_size, 2], keepExisting=.true., fill=dmiss)
         call realloc(self%z_top, [new_size, 2], keepExisting=.true., fill=dmiss)
         call realloc(self%indices, [new_size, 6], keepExisting=.true., fill=0)

         call realloc(self%area, new_size, keepExisting=.true., fill=0.0_dp)
         call realloc(self%discharge_cosine, [new_size, 2], keepExisting=.true., fill=0.0_dp)
         call realloc(self%discharge_sine, [new_size, 2], keepExisting=.true., fill=0.0_dp)

         call realloc(self%discharge, new_size, keepExisting=.true., fill=0.0_dp)
         call realloc(self%constituents, [new_size, numconst], keepExisting=.true., fill=0.0_dp)

         call realloc(self%max_xy_points, new_size, keepExisting=.true., fill=0)

      end if

   end subroutine resize_source_sinks

   ! Source/sink subroutines.
   ! ====================================================================================================

   !> Add a source(-sink) to the model based on geometry given in a polyline file.
   !! This subroutine is a wrapper around addsorsin, mainly taking care of reading the polyline file.
   subroutine addsorsin_from_polyline_file(polyline_file, name, z_source, z_sink, area, ierr)
      use m_filez, only: oldfil
      use m_polygon, only: xpl, ypl, zpl, npl, dzL, colpl
      use m_reapol, only: reapol
      use system_utils, only: split_filename

      ! Parameters
      character(len=*), intent(in) :: polyline_file !< Name of the polyline file, either with x,y values only (*.pli), or including z-values (*.pliz).
      character(len=*), optional, intent(in) :: name !< Name of the source-sink. When not present, name is based on the polyline filename instead.
      real(kind=dp), dimension(:), optional, intent(in) :: z_source !< Vertical position of the source, Z-value(s) in m (1 for point or 2 for range).
      real(kind=dp), dimension(:), optional, intent(in) :: z_sink !< Vertical position of the source, Z-value(s) in m (1 for point or 2 for range).
      real(kind=dp), intent(in) :: area !< Area of the source/sink, in m2. Set to 0.0 for momentum-free point sources.
      integer, intent(out) :: ierr !< Error code, DFM_NOERR if no error occurred.

      ! Local variables
      integer :: istat
      integer :: pli_lun
      integer, parameter :: Z_SIZE = 2
      real(kind=dp), dimension(:), allocatable :: z_source_
      real(kind=dp), dimension(:), allocatable :: z_sink_
      logical :: have_z_range
      character(len=0) :: path
      character(len=0) :: ext
      character(len=IDLEN) :: name_

      ierr = DFM_WRONGINPUT

      call oldfil(pli_lun, polyline_file)
      call reapol(pli_lun, 0)

      if (npl == 0) then
         return
      end if

      have_z_range = colpl > 3

      ! Either take the z-source values from input, or from polyline's last point.
      if (present(z_source)) then
         allocate (z_source_, source=z_source, stat=istat)
         if (istat /= 0) then
            return
         end if
      else
         allocate (z_source_(Z_SIZE), source=dmiss, stat=istat)
         if (istat /= 0) then
            return
         end if
         z_source_(1) = zpl(npl)

         if (have_z_range) then
            z_source_(2) = dzL(npl)
         end if
      end if

      ! Either take the z-sink values from input, or from polyline's first point.
      if (present(z_sink)) then
         allocate (z_sink_, source=z_sink, stat=istat)
         if (istat /= 0) then
            return
         end if
      else
         allocate (z_sink_(Z_SIZE), source=dmiss, stat=istat)
         if (istat /= 0) then
            return
         end if
         z_sink_(1) = zpl(1)
         if (have_z_range) then
            z_sink_(2) = dzL(1)
         end if
      end if

      if (present(name)) then
         name_ = name
      else
         call split_filename(polyline_file, path, name_, ext)
      end if

      ! Add the source/sink to the model based on prepared polyline data.
      call addsorsin(trim(name_), xpl(1:npl), ypl(1:npl), z_source_, z_sink_, area, ierr)

   end subroutine addsorsin_from_polyline_file

   !> Add a source-sink to the model.
   subroutine addsorsin(name, x_points, y_points, z_source, z_sink, area, ierr)
      use m_GlobalParameters, only: INDTP_ALL
      use geometry_module, only: normalin
      use m_sferic, only: jsferic, jasfer3D
      use m_find_flownode, only: find_nearest_flownodes

      ! Parameters
      character(len=*), intent(in) :: name !< Name of the source/sink.
      real(kind=dp), dimension(:), intent(in) :: x_points !< x-coordinates of the source/sink (polyline from sink to source point).
      real(kind=dp), dimension(:), intent(in) :: y_points !< y-coordinates of the source/sink (polyline from sink to source point).
      real(kind=dp), dimension(2), intent(in) :: z_source !< Vertical position of the source, Z-values in m. (second point is dmiss if not a range)
      real(kind=dp), dimension(2), intent(in) :: z_sink !< Vertical position of the sink, Z-values in m. (second point is dmiss if not a range)
      real(kind=dp), intent(in) :: area !< Area of the source/sink, in m2. Set to 0.0 for momentum-free point sources.
      integer, intent(out) :: ierr !< Error code, DFM_NOERR if no error occurred.

      ! Local variables
      integer :: kk
      integer :: kk2
      integer :: i
      integer :: jakdtree
      integer :: num_points
      integer, dimension(1) :: kdum
      character(len=IdLen), dimension(1) :: tmpname

      ierr = DFM_WRONGINPUT

      ! Check if number of points is larger than zero.
      num_points = size(x_points)
      if (num_points == 0) then
         return
      end if

      ! Increment source/sink counter.
      num_source_sink = num_source_sink + 1
      
      ! If the number of source/sinks exceeds the current array size, double the array size.
      if (num_source_sink > size(source_sinks%name)) then
         call source_sinks%resize((num_source_sink - 1) * 2)
      end if

      ! Set the coordinates of the source/sink, only the first 2 points of the polyline file are actually used.
      source_sinks%x(num_source_sink, 1:num_points) = x_points(1:num_points)
      source_sinks%y(num_source_sink, 1:num_points) = y_points(1:num_points)
      source_sinks%max_xy_points(num_source_sink) = num_points
      kk = 0
      kk2 = 0

      ! Set source/sink name.
      source_sinks%name(num_source_sink) = name

      tmpname(1) = name//' source'
      jakdtree = 0
      kdum(1) = 0
      if (source_sinks%x(num_source_sink, num_points) /= dmiss) then
         call find_nearest_flownodes(1, source_sinks%x(num_source_sink, num_points), source_sinks%y(num_source_sink, num_points), tmpname(1), kdum(1), jakdtree, -1, INDTP_ALL)
         kk2 = kdum(1)
      end if

      ! Support point source/sinks in a single cell if polyline has just one point (npl==1)
      if (num_points == 1) then

         kk = 0 ! Only keep the source-side (kk2), and disable momentum discharge
         if (area /= dmiss .and. area /= 0.0_dp) then
            ! User specified an area for momentum discharge, but that does not apply to POINT sources.
            write (msgbuf, '(a,a,a,f8.2,a)') 'Source-sink ''', trim(name), ''' is a POINT-source. Nonzero area was specified: ', area, ', but area will be ignored (no momentum discharge).'
            call warn_flush()
         end if
         source_sinks%area(num_source_sink) = 0.0_dp

      else ! Default: linked source-sink, with 2 or more polyline points
         tmpname = name//' sink'
         kdum(1) = 0
         if (source_sinks%x(num_source_sink, 1) /= dmiss) then
            call find_nearest_flownodes(1, source_sinks%x(num_source_sink, 1), source_sinks%y(num_source_sink, 1), tmpname(1), kdum(1), jakdtree, -1, INDTP_ALL)
            kk = kdum(1)
         end if

         if (kk /= 0 .or. kk2 /= 0) then
            source_sinks%area(num_source_sink) = area
         end if
      end if

      if (kk == 0 .and. kk2 == 0) then
         write (msgbuf, '(a,a)') 'Source+sink is outside model area for ', trim(name)
         call warn_flush()
         ierr = DFM_NOERR
         goto 8888
      end if

      source_sinks%indices(1, num_source_sink) = kk
      source_sinks%z_bottom(1, num_source_sink) = z_sink(1)
      source_sinks%z_top(1, num_source_sink) = z_sink(1)

      source_sinks%indices(4, num_source_sink) = kk2
      source_sinks%z_bottom(2, num_source_sink) = z_source(1)
      source_sinks%z_top(2, num_source_sink) = z_source(1)

      if (kk > 0) then
         if (z_sink(2) /= dmiss) then
            source_sinks%z_top(1, num_source_sink) = z_sink(2)
         end if
         ! Determine angle (sin/cos) of 'from' link (=first segment of polyline)
         if (num_points > 1) then
            call normalin(source_sinks%x(num_source_sink, 1), source_sinks%y(num_source_sink, 1), source_sinks%x(num_source_sink, 2), source_sinks%y(num_source_sink, 2), source_sinks%discharge_cosine(1, num_source_sink), source_sinks%discharge_sine(1, num_source_sink), source_sinks%x(num_source_sink, 1), source_sinks%y(num_source_sink, 1), jsferic, jasfer3D, dxymis)
         end if

         do i = 1, num_source_sink - 1
            if (source_sinks%indices(1, i) /= 0 .and. kk == source_sinks%indices(1, i)) then
               write (msgbuf, '(4a)') 'FROM point of ', trim(source_sinks%name(num_source_sink)), ' coincides with FROM point of ', trim(source_sinks%name(i))
               call warn_flush()
            else if (source_sinks%indices(4, i) /= 0 .and. kk == source_sinks%indices(4, i)) then
               write (msgbuf, '(4a)') 'FROM point of ', trim(source_sinks%name(num_source_sink)), ' coincides with TO   point of ', trim(source_sinks%name(i))
               call warn_flush()
            end if
         end do

      end if

      if (kk2 > 0) then
         if (z_source(2) /= dmiss) then
            source_sinks%z_top(2, num_source_sink) = z_source(2)
         end if
         
         ! Determine angle (sin/cos) of 'to' link (= first segment of polyline)
         if (num_points > 1) then
            call normalin(source_sinks%x(num_source_sink, num_points - 1), source_sinks%y(num_source_sink, num_points - 1), source_sinks%x(num_source_sink, num_points), source_sinks%y(num_source_sink, num_points), source_sinks%discharge_cosine(2, num_source_sink), source_sinks%discharge_sine(2, num_source_sink), source_sinks%x(num_source_sink, num_points), source_sinks%y(num_source_sink, num_points), jsferic, jasfer3D, dxymis)
         end if
      end if

      ierr = DFM_NOERR

8888  continue

   end subroutine addsorsin

   !> Compute and set source and sink values for the 'intake-outfall' structures.
   subroutine setsorsin()
      use m_flow, only: kmx, zws, vol1, qin, epshs
      use m_get_kbot_ktop, only: getkbotktop
      use m_flowtimes, only: dts
      use m_partitioninfo, only: jampi, reduce_srsn

      integer :: n
      integer :: kk
      integer :: k
      integer :: kb
      integer :: kt
      integer :: kk2
      integer :: ku
      integer :: numvals
      integer :: L
      real(kind=dp) :: qsrck
      real(kind=dp) :: qsrckk
      real(kind=dp) :: dzss
      real(kind=dp), parameter :: FRAC = 0.5_dp ! cell volume fraction that can at most be extracted in one step

      source_sink_reduction = 0.0_dp
      do n = 1, num_source_sink
         kk = source_sinks%indices(n, 1) ! 2D pressure cell nr, From side, 0 = out of all, -1 = in other domain, > 0, own domain
         kk2 = source_sinks%indices(n, 4) ! 2D pressure cell nr, To   side, 0 = out of all, -1 = in other domain, > 0, own domain
         source_sinks%discharge(n) = source_sink_all_discharges(1, n)
         if (kk > 0) then ! FROM point
            if (kmx > 0) then
               call getkbotktop(kk, kb, kt)
               if (source_sinks%z_bottom(n, 1) == dmiss) then
                  k = kb
                  ku = kt
               else
                  do k = kb, kt
                     if (zws(k) > source_sinks%z_bottom(n, 1) .or. k == kt) then
                        exit
                     end if
                  end do
                  if (source_sinks%z_top(n, 1) == dmiss) then
                     ku = k
                  else
                     do ku = kb, kt
                        if (zws(ku) > source_sinks%z_top(n, 1) .or. ku == kt) then
                           exit
                        end if
                     end do
                  end if
               end if
            else
               k = kk
               kt = kk
               ku = kk ! in 2D, volume cell nr = pressure cell nr
            end if
            source_sinks%indices(n, 2) = k ! store kb of src
            source_sinks%indices(n, 3) = ku !
            if (source_sinks%discharge(n) > 0) then ! Reduce if flux pos

               do k = source_sinks%indices(n, 2), source_sinks%indices(n, 3)
                  source_sink_reduction(1, n) = source_sink_reduction(1, n) + vol1(k)
                  do L = 1, numconst
                     source_sink_reduction(1 + L, n) = source_sink_reduction(1 + L, n) + constituents(L, k) * vol1(k)
                  end do
               end do
               if (source_sink_reduction(1, n) > 0.0_dp) then
                  do L = 1, numconst
                     source_sink_reduction(1 + L, n) = source_sink_reduction(1 + L, n) / source_sink_reduction(1, n)
                  end do
               end if
            end if
         end if

         if (kk2 > 0) then ! TO point
            if (kmx > 0) then
               call getkbotktop(kk2, kb, kt)
               if (source_sinks%z_bottom(n, 2) == dmiss) then
                  k = kb
                  ku = kt
               else
                  do k = kb, kt
                     if (zws(k) > source_sinks%z_bottom(n, 2) .or. k == kt) then
                        exit
                     end if
                  end do
                  if (source_sinks%z_top(n, 2) == dmiss) then
                     ku = k
                  else
                     do ku = kb, kt
                        if (zws(ku) > source_sinks%z_top(n, 2) .or. ku == kt) then
                           exit
                        end if
                     end do
                  end if
               end if
            else
               k = kk2
               kt = kk2
               ku = kk2 ! in 2D, volume cell nr = pressure cell nr
            end if
            source_sinks%indices(n, 5) = k
            source_sinks%indices(n, 6) = ku
            if (source_sinks%discharge(n) < 0) then ! Reduce if flux neg

               do k = source_sinks%indices(n, 5), source_sinks%indices(n, 6)
                  source_sink_reduction(1 + numconst + 1, n) = source_sink_reduction(1 + numconst + 1, n) + vol1(k)
                  do L = 1, numconst
                     source_sink_reduction(1 + numconst + 1 + L, n) = source_sink_reduction(1 + numconst + 1 + L, n) + constituents(L, k) * vol1(k)
                  end do
               end do
               if (source_sink_reduction(1 + numconst + 1, n) > 0.0_dp) then
                  do L = 1, numconst
                     source_sink_reduction(1 + numconst + 1 + L, n) = source_sink_reduction(1 + numconst + 1 + L, n) / source_sink_reduction(1 + numconst + 1, n)
                  end do
               end if
            end if
         end if

      end do

      if (jampi > 0) then
         numvals = 2 * (1 + numconst)
         call reduce_srsn(numvals, num_source_sink, source_sink_reduction)
      end if

      do n = 1, num_source_sink
         source_sinks%discharge(n) = source_sink_all_discharges(1, n)
         do L = 1, numconst
            source_sinks%constituents(L, n) = source_sink_all_discharges(L + 1, n)
         end do

         kk = source_sinks%indices(n, 1) ! 2D pressure cell nr
         qsrck = source_sinks%discharge(n)
         if (kk /= 0 .and. qsrck > 0) then ! Extract FROM 1
            if (FRAC * source_sink_reduction(1, n) / dts < abs(qsrck)) then
               qsrck = FRAC * source_sink_reduction(1, n) / dts

               write (msgbuf, *) 'Extraction flux larger than cell volume at point 1 of : ', trim(source_sinks%name(n))
               call mess(LEVEL_WARN, msgbuf)
            end if
         end if

         kk2 = source_sinks%indices(n, 4) ! 2D pressure cell nr
         if (kk2 /= 0 .and. qsrck < 0) then ! Extract From 2
            if (FRAC * source_sink_reduction(1 + numconst + 1, n) / dts < abs(qsrck)) then
               qsrck = -FRAC * source_sink_reduction(1 + numconst + 1, n) / dts

               write (msgbuf, *) 'Extraction flux larger than cell volume at point 2 of : ', trim(source_sinks%name(n))
               call mess(LEVEL_WARN, msgbuf)
            end if
         end if

         source_sinks%discharge(n) = qsrck

         if (kk * kk2 /= 0) then ! Coupled stuff
            if (qsrck > 0) then ! FROM k to k2
               do L = 1, numconst
                  source_sinks%constituents(L, n) = source_sinks%constituents(L, n) + source_sink_reduction(1 + L, n)
               end do
            else if (qsrck < 0) then ! FROM k2 to k
               do L = 1, numconst
                  source_sinks%constituents(L, n) = source_sinks%constituents(L, n) + source_sink_reduction(1 + numconst + 1 + L, n)
               end do
            end if
         end if

         if (kk > 0) then ! FROM Point
            qsrckk = source_sinks%discharge(n)
            qin(kk) = qin(kk) - qsrckk ! add to 2D pressure cell nr
            do k = source_sinks%indices(n, 2), source_sinks%indices(n, 3)
               if (kmx > 0) then
                  dzss = zws(source_sinks%indices(n, 3)) - zws(source_sinks%indices(n, 2) - 1)
                  if (dzss > epshs) then
                     qsrck = qsrckk * (zws(k) - zws(k - 1)) / dzss
                  else
                     qsrck = qsrckk / (source_sinks%indices(n, 3) - source_sinks%indices(n, 2) + 1)
                  end if
                  qin(k) = qin(k) - qsrck
               end if
            end do
         end if

         if (kk2 > 0) then ! TO Point
            qsrckk = source_sinks%discharge(n)
            qin(kk2) = qin(kk2) + qsrckk ! add to 2D pressure cell nr
            do k = source_sinks%indices(n, 5), source_sinks%indices(n, 6)
               if (kmx > 0) then
                  dzss = zws(source_sinks%indices(n, 6)) - zws(source_sinks%indices(n, 5) - 1)
                  if (dzss > epshs) then
                     qsrck = qsrckk * (zws(k) - zws(k - 1)) / dzss
                  else
                     qsrck = qsrckk / (source_sinks%indices(n, 6) - source_sinks%indices(n, 5) + 1)
                  end if
                  qin(k) = qin(k) + qsrck
               end if
            end do
         end if

      end do

   end subroutine setsorsin

end module m_source_sink