!----- AGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2017-2026.
!
!  This file is part of Delft3D (D-Flow Flexible Mesh component).
!
!  Delft3D is free software: you can redistribute it and/or modify
!  it under the terms of the GNU Affero General Public License as
!  published by the Free Software Foundation version 3.
!
!  Delft3D  is distributed in the hope that it will be useful,
!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!  GNU Affero General Public License for more details.
!
!  You should have received a copy of the GNU Affero General Public License
!  along with Delft3D.  If not, see <http://www.gnu.org/licenses/>.
!
!  contact: delft3d.support@deltares.nl
!  Stichting Deltares
!  P.O. Box 177
!  2600 MH Delft, The Netherlands
!
!  All indications and logos of, and references to, "Delft3D",
!  "D-Flow Flexible Mesh" and "Deltares" are registered trademarks of Stichting
!  Deltares, and remain the property of Stichting Deltares. All rights reserved.
!
!-------------------------------------------------------------------------------

module m_source_sink
   use precision, only: dp
   use dfm_error, only: DFM_NOERR, DFM_WRONGINPUT
   use messagehandling, only: IDLEN, LEVEL_WARN, msgbuf, warn_flush, mess
   use m_alloc, only: realloc
   use m_missing, only: dmiss, dxymis
   use m_transport, only: numconst, constituents

   implicit none(type, external)

   private

   public :: SourceSinks

   public :: SINK_SIDE, SOURCE_SIDE
   public :: FLOWCELL_SINK, BOTTOM_LAYER_SINK, TOP_LAYER_SINK, FLOWCELL_SOURCE, BOTTOM_LAYER_SOURCE, TOP_LAYER_SOURCE

   public :: source_sinks
   public :: source_sink_all_discharges

   ! Type containing all source/sink data.
   type :: SourceSinks

      ! Source/sink counters.
      integer :: num_total = 0 !< [-] Total number of source/sinks in the model.
      integer :: num_normal = 0 !< [-] Number of normal source/sinks, i.e. excluding bubblescreens.
      integer :: num_oldfile = 0 !< [-] Number of source/sinks in old extforce file.
      integer :: num_nearfield = 0 !< [-] Number of source/sinks added for near field.
      integer :: max_polyline_points = 2 !< [-] Maximum number of points in source_sinks%x, source_sinks%y over all sources/sinks. Used for array dimensions.

      ! Source/sink name, x,y,z coordinates, and indices.
      character(len=255), dimension(:), allocatable :: name !< [-] Name of the source/sink.
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

      ! Source/sink miscellaneous variables.
      integer, dimension(:), allocatable :: max_xy_points !< [-] Maximum number of points per source/sink in x, y. Used for array dimensions.
      logical, dimension(:), allocatable :: is_normal !< [-] Logical array indicating if a source/sink is normal (excluding bubblescreens).
      logical :: add_k_to_turkin = .false. !< [-] Add k of source/sink to turkin.

      ! Cumulative volume and discharge variables. Used in output and for waq coupling.
      real(kind=dp), dimension(:), allocatable :: cumulative_volume !< [m3] Cumulative volume at each source/sink from Tstart to now. {size=(self%num_total)}
      real(kind=dp), dimension(:), allocatable :: cumulative_volume_previous !< [m3] Cumulative volume at each source/sink from Tstart to the previous His-output time. {size=(self%num_total)}
      real(kind=dp), dimension(:), allocatable :: average_discharge_previous !< [m3/s] Average discharge in the past his-interval at each source/sink. {size=(self%num_total)}
      integer, dimension(:), allocatable :: waq_index !< [-] Index array to map source/sink to waq source/sink arrays. {size=(self%num_total)}
      real(kind=dp), dimension(:), allocatable :: cumulative_discharge_waq !< [m3/s] Cumulative discharge at each source/sink within current waq-timestep. {size=(self%num_total)}
      real(kind=dp), dimension(:), allocatable :: cumulative_discharge_waq_previous !< [m3/s] Cumulative discharge at each source/sink within current waq-timestep at the beginning of the time step before possible reduction. {size=(self%num_total)}

   contains

      procedure :: initialize => initialize_source_sinks
      procedure :: realloc => realloc_source_sinks
      procedure, private :: realloc_xy => realloc_xy_source_sinks

      procedure :: add => add_source_sink
      procedure :: add_from_polyline_file => add_source_sink_from_polyline_file
      procedure :: update_discharges => update_source_sink_discharges

   end type SourceSinks

   ! Enums for the SourceSinks%z_bottom, %z_top, %discharge_cosine, and %discharge_sine arrays
   integer, parameter :: SINK_SIDE = 1 !< Sink side identifier for SourceSinks%z_bottom, %z_top, %discharge_cosine, and %discharge_sine arrays
   integer, parameter :: SOURCE_SIDE = 2 !< Source side identifier for SourceSinks%z_bottom, %z_top, %discharge_cosine, and %discharge_sine arrays

   ! Enums for the SourceSinks%indices array
   integer, parameter :: FLOWCELL_SINK = 1 !< Flowcell identifier of sink in SourceSinks%indices array
   integer, parameter :: BOTTOM_LAYER_SINK = 2 !< Bottom layer identifier of sink in SourceSinks%indices array
   integer, parameter :: TOP_LAYER_SINK = 3 !< Top layer identifier of sink in SourceSinks%indices array
   integer, parameter :: FLOWCELL_SOURCE = 4 !< Flowcell identifier of source in SourceSinks%indices array
   integer, parameter :: BOTTOM_LAYER_SOURCE = 5 !< Bottom layer identifier of source in SourceSinks%indices array
   integer, parameter :: TOP_LAYER_SOURCE = 6 !< Top layer identifier of source in SourceSinks%indices array

   ! Object containing all source/sink data.
   type(SourceSinks), target :: source_sinks

   ! Global source/sink arrays used for EC module and partitioned models.
   real(kind=dp), dimension(:,:), allocatable, target :: source_sink_all_discharges !< [m3/s] All source/sink discharges in one array for partitioned models. {size=(numconst+1,source_sinks%num_total)}
   real(kind=dp), dimension(:,:), allocatable :: source_sink_reduction !< [-] Source/sink reduction array for partitioned models. {size=(2*(numconst+1),source_sinks%num_total)}   

contains

   ! SourceSinks type-bound procedures.
   ! ====================================================================================================

   !> Allocates and initializes the SourceSinks attributes to size.
   subroutine initialize_source_sinks(self, size)
      ! Parameters
      class(SourceSinks), intent(out) :: self
      integer, intent(in) :: size

      ! Allocate and intialize global source/sink arrays.
      allocate (source_sink_all_discharges(numconst+1, size))
      allocate (source_sink_reduction(2*(numconst+1), size))

      source_sink_all_discharges = 0.0_dp
      source_sink_reduction = 0.0_dp

      ! Allocate all source/sink attributes.
      allocate (self%name(size))
      allocate (self%x(size, self%max_polyline_points))
      allocate (self%y(size, self%max_polyline_points))
      allocate (self%z_bottom(size, 2))
      allocate (self%z_top(size, 2))
      allocate (self%indices(size, 6))

      allocate (self%area(size))
      allocate (self%discharge_cosine(size, 2))
      allocate (self%discharge_sine(size, 2))

      allocate (self%discharge(size))
      allocate (self%constituents(size, numconst))

      allocate (self%max_xy_points(size))
      allocate (self%is_normal(size))

      allocate (self%cumulative_volume(size))
      allocate (self%cumulative_volume_previous(size))
      allocate (self%average_discharge_previous(size))
      allocate (self%waq_index(size))
      allocate (self%cumulative_discharge_waq(size))
      allocate (self%cumulative_discharge_waq_previous(size))

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

      self%max_xy_points = 0
      self%is_normal = .true.

      self%cumulative_volume = 0.0_dp
      self%cumulative_volume_previous = 0.0_dp
      self%average_discharge_previous = 0.0_dp
      self%waq_index = 0
      self%cumulative_discharge_waq = 0.0_dp
      self%cumulative_discharge_waq_previous = 0.0_dp

   end subroutine initialize_source_sinks

   !> Reallocates the SourceSinks object arrays to new size, keeping existing values.
   subroutine realloc_source_sinks(self, new_size)
      ! Parameters
      class(SourceSinks), intent(inout) :: self
      integer, intent(in) :: new_size

      if (new_size > size(self%name)) then
         ! Reallocate global source/sink arrays.
         call realloc(source_sink_all_discharges, [numconst+1, new_size], keepExisting=.true., fill=0.0_dp)
         call realloc(source_sink_reduction, [2*(numconst+1), new_size], keepExisting=.true., fill=0.0_dp)

         ! Reallocate all source/sink arrays.
         call realloc(self%name, new_size, keepExisting=.true., fill='')
         call realloc(self%x, [new_size, self%max_polyline_points], keepExisting=.true., fill=dmiss)
         call realloc(self%y, [new_size, self%max_polyline_points], keepExisting=.true., fill=dmiss)
         call realloc(self%z_bottom, [new_size, 2], keepExisting=.true., fill=dmiss)
         call realloc(self%z_top, [new_size, 2], keepExisting=.true., fill=dmiss)
         call realloc(self%indices, [new_size, 6], keepExisting=.true., fill=0)

         call realloc(self%area, new_size, keepExisting=.true., fill=0.0_dp)
         call realloc(self%discharge_cosine, [new_size, 2], keepExisting=.true., fill=0.0_dp)
         call realloc(self%discharge_sine, [new_size, 2], keepExisting=.true., fill=0.0_dp)

         call realloc(self%discharge, new_size, keepExisting=.true., fill=0.0_dp)
         call realloc(self%constituents, [new_size, numconst], keepExisting=.true., fill=0.0_dp)

         call realloc(self%max_xy_points, new_size, keepExisting=.true., fill=0)
         call realloc(self%is_normal, new_size, keepExisting=.true., fill=.true.)

         call realloc(self%cumulative_volume, new_size, keepExisting=.true., fill=0.0_dp)
         call realloc(self%cumulative_volume_previous, new_size, keepExisting=.true., fill=0.0_dp)
         call realloc(self%average_discharge_previous, new_size, keepExisting=.true., fill=0.0_dp)
         call realloc(self%waq_index, new_size, keepExisting=.true., fill=0)
         call realloc(self%cumulative_discharge_waq, new_size, keepExisting=.true., fill=0.0_dp)
         call realloc(self%cumulative_discharge_waq_previous, new_size, keepExisting=.true., fill=0.0_dp)
      end if

   end subroutine realloc_source_sinks

   !> Reallocates the x and y arrays of the SourceSinks object to fit new_max_polyline_points, keeping existing values if possible.
   subroutine realloc_xy_source_sinks(self, new_max_polyline_points)
      ! Parameters
      class(SourceSinks), intent(inout) :: self
      integer, intent(in) :: new_max_polyline_points

      ! Update max_polyline_points to new value if larger than current value.
      self%max_polyline_points = max(self%max_polyline_points, new_max_polyline_points)

      ! Reallocate x and y arrays to fit new max_polyline_points.
      call realloc(self%x, [size(self%name), self%max_polyline_points], keepExisting=.true., fill=dmiss)
      call realloc(self%y, [size(self%name), self%max_polyline_points], keepExisting=.true., fill=dmiss)

   end subroutine realloc_xy_source_sinks

   ! Source/sink subroutines.
   ! ====================================================================================================

   !> Add a source(-sink) to the model based on geometry given in a polyline file.
   !! This subroutine is a wrapper around source_sinks%add, mainly taking care of reading the polyline file.
   subroutine add_source_sink_from_polyline_file(self, polyline_file, name, z_source, z_sink, area, ierr)
      use m_filez, only: oldfil
      use m_polygon, only: xpl, ypl, zpl, npl, dzL, colpl
      use m_reapol, only: reapol
      use system_utils, only: split_filename

      ! Parameters
      class(SourceSinks), intent(inout) :: self !< Source/sink object instance
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

      ! Initialize self (source_sinks) if not already done.
      if (.not. allocated(self%name)) then
         call self%initialize(1)
      end if

      ! Add the source/sink to the model based on prepared polyline data.
      call self%add(trim(name_), xpl(1:npl), ypl(1:npl), z_source_, z_sink_, area, ierr)

   end subroutine add_source_sink_from_polyline_file

   !> Add a source-sink to the model.
   subroutine add_source_sink(self, name, x_points, y_points, z_source, z_sink, area, ierr)
      use m_GlobalParameters, only: INDTP_ALL
      use geometry_module, only: normalin
      use m_sferic, only: jsferic, jasfer3D
      use m_find_flownode, only: find_nearest_flownodes

      ! Parameters
      class(SourceSinks), intent(inout) :: self !< Source/sink object instance
      character(len=*), intent(in) :: name !< Name of the source/sink.
      real(kind=dp), dimension(:), intent(in) :: x_points !< x-coordinates of the source/sink (polyline from sink to source point).
      real(kind=dp), dimension(:), intent(in) :: y_points !< y-coordinates of the source/sink (polyline from sink to source point).
      real(kind=dp), dimension(2), intent(in) :: z_source !< Vertical position of the source, Z-values in m. (second point is dmiss if not a range)
      real(kind=dp), dimension(2), intent(in) :: z_sink !< Vertical position of the sink, Z-values in m. (second point is dmiss if not a range)
      real(kind=dp), intent(in) :: area !< Area of the source/sink, in m2. Set to 0.0 for momentum-free point sources.
      integer, intent(out) :: ierr !< Error code, DFM_NOERR if no error occurred.

      ! Local variables
      integer :: i !< Source/sink index
      integer :: n_sink !< Flowcell index of sink
      integer :: n_source !< Flowcell index of source
      integer :: jakdtree
      integer :: num_points !< Number of points in the polyline representing the source/sink.
      integer, dimension(1) :: n_dummy !< Dummy flowcell index for readout
      character(len=IdLen), dimension(1) :: tmp_name !< Temporary name of the source/sink

      ierr = DFM_WRONGINPUT

      ! Check if number of points is larger than zero.
      num_points = size(x_points)
      if (num_points == 0) then
         return
      end if

      ! Increment source/sink counter.
      self%num_total = self%num_total + 1
      
      ! If the number of source/sinks exceeds the current array size, double the array size.
      if (self%num_total > size(self%name)) then
         call self%realloc((self%num_total - 1) * 2)
      end if

      ! If the number of points in the polyline exceeds the current max_polyline_points, reallocate the arrays to fit the new number of points.
      if (num_points > self%max_polyline_points) then
         call self%realloc_xy(num_points)
      end if

      ! Set the coordinates of the source/sink, only the first 2 points of the polyline file are actually used.
      self%x(self%num_total, 1:num_points) = x_points(1:num_points)
      self%y(self%num_total, 1:num_points) = y_points(1:num_points)
      self%max_xy_points(self%num_total) = num_points
      n_sink = 0
      n_source = 0

      ! Set source/sink name.
      self%name(self%num_total) = name

      tmp_name(1) = name//' source'
      jakdtree = 0
      n_dummy(1) = 0
      if (self%x(self%num_total, num_points) /= dmiss) then
         call find_nearest_flownodes(1, self%x(self%num_total, num_points), self%y(self%num_total, num_points), tmp_name(1), n_dummy(1), jakdtree, -1, INDTP_ALL)
         n_source = n_dummy(1)
      end if

      ! Support point source/sinks in a single cell if polyline has just one point (npl==1)
      if (num_points == 1) then

         n_sink = 0 ! Only keep the source-side (n_source), and disable momentum discharge
         if (area /= dmiss .and. area /= 0.0_dp) then
            ! User specified an area for momentum discharge, but that does not apply to POINT sources.
            write (msgbuf, '(a,a,a,f8.2,a)') 'Source-sink ''', trim(name), ''' is a POINT-source. Nonzero area was specified: ', area, ', but area will be ignored (no momentum discharge).'
            call warn_flush()
         end if
         self%area(self%num_total) = 0.0_dp

      else ! Default: linked source-sink, with 2 or more polyline points
         tmp_name = name//' sink'
         n_dummy(1) = 0
         if (self%x(self%num_total, 1) /= dmiss) then
            call find_nearest_flownodes(1, self%x(self%num_total, 1), self%y(self%num_total, 1), tmp_name(1), n_dummy(1), jakdtree, -1, INDTP_ALL)
            n_sink = n_dummy(1)
         end if

         if (n_sink /= 0 .or. n_source /= 0) then
            self%area(self%num_total) = area
         end if
      end if

      if (n_sink == 0 .and. n_source == 0) then
         write (msgbuf, '(a,a)') 'Source+sink is outside model area for ', trim(name)
         call warn_flush()
         ierr = DFM_NOERR
         return
      end if

      self%indices(self%num_total, FLOWCELL_SINK) = n_sink
      self%z_bottom(self%num_total, SINK_SIDE) = z_sink(1)
      self%z_top(self%num_total, SINK_SIDE) = z_sink(1)

      self%indices(self%num_total, FLOWCELL_SOURCE) = n_source
      self%z_bottom(self%num_total, SOURCE_SIDE) = z_source(1)
      self%z_top(self%num_total, SOURCE_SIDE) = z_source(1)

      if (n_sink > 0) then
         if (z_sink(2) /= dmiss) then
            self%z_top(self%num_total, SINK_SIDE) = z_sink(2)
         end if
         ! Determine angle (sin/cos) of 'from' link (=first segment of polyline)
         if (num_points > 1) then
            call normalin( &
               self%x(self%num_total, 1), &
               self%y(self%num_total, 1), &
               self%x(self%num_total, 2), &
               self%y(self%num_total, 2), &
               self%discharge_cosine(self%num_total, SINK_SIDE), &
               self%discharge_sine(self%num_total, SINK_SIDE), &
               self%x(self%num_total, 1), &
               self%y(self%num_total, 1), &
               jsferic, jasfer3D, dxymis &
            )
         end if

         do i = 1, self%num_total - 1
            if (self%indices(i, FLOWCELL_SINK) /= 0 .and. n_sink == self%indices(i, FLOWCELL_SINK)) then
               write (msgbuf, '(4a)') 'FROM point of ', trim(self%name(self%num_total)), ' coincides with FROM point of ', trim(self%name(i))
               call warn_flush()
            else if (self%indices(i, FLOWCELL_SOURCE) /= 0 .and. n_sink == self%indices(i, FLOWCELL_SOURCE)) then
               write (msgbuf, '(4a)') 'FROM point of ', trim(self%name(self%num_total)), ' coincides with TO   point of ', trim(self%name(i))
               call warn_flush()
            end if
         end do

      end if

      if (n_source > 0) then
         if (z_source(2) /= dmiss) then
            self%z_top(self%num_total, SOURCE_SIDE) = z_source(2)
         end if
         
         ! Determine angle (sin/cos) of 'to' link (= first segment of polyline)
         if (num_points > 1) then
            call normalin( &
               self%x(self%num_total, num_points - 1), &
               self%y(self%num_total, num_points - 1), &
               self%x(self%num_total, num_points), &
               self%y(self%num_total, num_points), &
               self%discharge_cosine(self%num_total, SOURCE_SIDE), &
               self%discharge_sine(self%num_total, SOURCE_SIDE), &
               self%x(self%num_total, num_points), &
               self%y(self%num_total, num_points), &
               jsferic, jasfer3D, dxymis &
            )
         end if
      end if

      ierr = DFM_NOERR

   end subroutine add_source_sink

   !> Compute and set source and sink values for the 'intake-outfall' structures.
   subroutine update_source_sink_discharges(self)
      use m_flow, only: kmx, zws, vol1, qin, epshs
      use m_get_kbot_ktop, only: getkbotktop
      use m_flowtimes, only: dts
      use m_partitioninfo, only: jampi, reduce_srsn

      ! Arguments
      class(SourceSinks), intent(inout) :: self !< Source/sink object instance

      ! Local variables
      integer :: i !< Source/sink index
      integer :: i_const !< Constituent index
      integer :: k !< Layer index
      integer :: k_bottom !< Bottom layer index
      integer :: k_top !< Top layer index
      integer :: ku !< Upper layer index
      integer :: n_sink !< Flowcell index of sink
      integer :: n_source !< Flowcell index of source
      integer :: numvals
      real(kind=dp) :: flowcell_discharge !< Source/sink discharge of the (whole) flowcell
      real(kind=dp) :: layer_discharge !< Source/sink discharge of a layer
      real(kind=dp) :: flowcell_height !< Vertical height of the flowcell
      real(kind=dp), parameter :: FRAC = 0.5_dp ! cell volume fraction that can at most be extracted in one step

      source_sink_reduction = 0.0_dp

      do i = 1, self%num_total

         n_sink = self%indices(i, FLOWCELL_SINK) ! 2D pressure cell nr, From side, 0 = out of all, -1 = in other domain, > 0, own domain
         n_source = self%indices(i, FLOWCELL_SOURCE) ! 2D pressure cell nr, To   side, 0 = out of all, -1 = in other domain, > 0, own domain
         self%discharge(i) = source_sink_all_discharges(1, i)

         if (n_sink > 0) then ! FROM point
            if (kmx > 0) then
               call getkbotktop(n_sink, k_bottom, k_top)
               if (self%z_bottom(i, SINK_SIDE) == dmiss) then
                  k = k_bottom
                  ku = k_top
               else
                  do k = k_bottom, k_top
                     if (zws(k) > self%z_bottom(i, SINK_SIDE) .or. k == k_top) then
                        exit
                     end if
                  end do
                  if (self%z_top(i, SINK_SIDE) == dmiss) then
                     ku = k
                  else
                     do ku = k_bottom, k_top
                        if (zws(ku) > self%z_top(i, SINK_SIDE) .or. ku == k_top) then
                           exit
                        end if
                     end do
                  end if
               end if
            else
               k = n_sink
               k_top = n_sink
               ku = n_sink ! in 2D, volume cell nr = pressure cell nr
            end if

            self%indices(i, BOTTOM_LAYER_SINK) = k ! store k_bottom of src
            self%indices(i, TOP_LAYER_SINK) = ku !

            if (self%discharge(i) > 0) then ! Reduce if flux pos

               do k = self%indices(i, BOTTOM_LAYER_SINK), self%indices(i, TOP_LAYER_SINK)
                  source_sink_reduction(1, i) = source_sink_reduction(1, i) + vol1(k)
                  do i_const = 1, numconst
                     source_sink_reduction(1 + i_const, i) = source_sink_reduction(1 + i_const, i) + constituents(i_const, k) * vol1(k)
                  end do
               end do
               if (source_sink_reduction(1, i) > 0.0_dp) then
                  do i_const = 1, numconst
                     source_sink_reduction(1 + i_const, i) = source_sink_reduction(1 + i_const, i) / source_sink_reduction(1, i)
                  end do
               end if
            end if
         end if

         if (n_source > 0) then ! TO point
            if (kmx > 0) then
               call getkbotktop(n_source, k_bottom, k_top)
               if (self%z_bottom(i, SOURCE_SIDE) == dmiss) then
                  k = k_bottom
                  ku = k_top
               else
                  do k = k_bottom, k_top
                     if (zws(k) > self%z_bottom(i, SOURCE_SIDE) .or. k == k_top) then
                        exit
                     end if
                  end do
                  if (self%z_top(i, SOURCE_SIDE) == dmiss) then
                     ku = k
                  else
                     do ku = k_bottom, k_top
                        if (zws(ku) > self%z_top(i, SOURCE_SIDE) .or. ku == k_top) then
                           exit
                        end if
                     end do
                  end if
               end if
            else
               k = n_source
               k_top = n_source
               ku = n_source ! in 2D, volume cell nr = pressure cell nr
            end if

            self%indices(i, BOTTOM_LAYER_SOURCE) = k
            self%indices(i, TOP_LAYER_SOURCE) = ku

            if (self%discharge(i) < 0) then ! Reduce if flux neg

               do k = self%indices(i, BOTTOM_LAYER_SOURCE), self%indices(i, TOP_LAYER_SOURCE)
                  source_sink_reduction(1 + numconst + 1, i) = source_sink_reduction(1 + numconst + 1, i) + vol1(k)
                  do i_const = 1, numconst
                     source_sink_reduction(1 + numconst + 1 + i_const, i) = source_sink_reduction(1 + numconst + 1 + i_const, i) + constituents(i_const, k) * vol1(k)
                  end do
               end do
               if (source_sink_reduction(1 + numconst + 1, i) > 0.0_dp) then
                  do i_const = 1, numconst
                     source_sink_reduction(1 + numconst + 1 + i_const, i) = source_sink_reduction(1 + numconst + 1 + i_const, i) / source_sink_reduction(1 + numconst + 1, i)
                  end do
               end if
            end if
         end if

      end do

      if (jampi > 0) then
         numvals = 2 * (1 + numconst)
         call reduce_srsn(numvals, self%num_total, source_sink_reduction)
      end if

      do i = 1, self%num_total
         self%discharge(i) = source_sink_all_discharges(1, i)
         do i_const = 1, numconst
            self%constituents(i, i_const) = source_sink_all_discharges(i_const + 1, i)
         end do

         n_sink = self%indices(i, FLOWCELL_SINK) ! 2D pressure cell nr
         flowcell_discharge = self%discharge(i)
         if (n_sink /= 0 .and. flowcell_discharge > 0) then ! Extract FROM 1
            if (FRAC * source_sink_reduction(1, i) / dts < abs(flowcell_discharge)) then
               flowcell_discharge = FRAC * source_sink_reduction(1, i) / dts

               write (msgbuf, *) 'Extraction flux larger than cell volume at point 1 of : ', trim(self%name(i))
               call mess(LEVEL_WARN, msgbuf)
            end if
         end if

         n_source = self%indices(i, FLOWCELL_SOURCE) ! 2D pressure cell nr
         if (n_source /= 0 .and. flowcell_discharge < 0) then ! Extract From 2
            if (FRAC * source_sink_reduction(1 + numconst + 1, i) / dts < abs(flowcell_discharge)) then
               flowcell_discharge = -FRAC * source_sink_reduction(1 + numconst + 1, i) / dts

               write (msgbuf, *) 'Extraction flux larger than cell volume at point 2 of : ', trim(self%name(i))
               call mess(LEVEL_WARN, msgbuf)
            end if
         end if

         self%discharge(i) = flowcell_discharge

         if (n_sink * n_source /= 0) then ! Coupled stuff
            if (flowcell_discharge > 0) then ! FROM k to k2
               do i_const = 1, numconst
                  self%constituents(i, i_const) = self%constituents(i, i_const) + source_sink_reduction(1 + i_const, i)
               end do
            else if (flowcell_discharge < 0) then ! FROM k2 to k
               do i_const = 1, numconst
                  self%constituents(i, i_const) = self%constituents(i, i_const) + source_sink_reduction(1 + numconst + 1 + i_const, i)
               end do
            end if
         end if

         if (n_sink > 0) then ! FROM Point
            layer_discharge = self%discharge(i)
            qin(n_sink) = qin(n_sink) - layer_discharge ! add to 2D pressure cell nr
            do k = self%indices(i, BOTTOM_LAYER_SINK), self%indices(i, TOP_LAYER_SINK)
               if (kmx > 0) then
                  flowcell_height = zws(self%indices(i, TOP_LAYER_SINK)) - zws(self%indices(i, BOTTOM_LAYER_SINK) - 1)
                  if (flowcell_height > epshs) then
                     flowcell_discharge = layer_discharge * (zws(k) - zws(k - 1)) / flowcell_height
                  else
                     flowcell_discharge = layer_discharge / (self%indices(i, TOP_LAYER_SINK) - self%indices(i, BOTTOM_LAYER_SINK) + 1)
                  end if
                  qin(k) = qin(k) - flowcell_discharge
               end if
            end do
         end if

         if (n_source > 0) then ! TO Point
            layer_discharge = self%discharge(i)
            qin(n_source) = qin(n_source) + layer_discharge ! add to 2D pressure cell nr
            do k = self%indices(i, BOTTOM_LAYER_SOURCE), self%indices(i, TOP_LAYER_SOURCE)
               if (kmx > 0) then
                  flowcell_height = zws(self%indices(i, TOP_LAYER_SOURCE)) - zws(self%indices(i, BOTTOM_LAYER_SOURCE) - 1)
                  if (flowcell_height > epshs) then
                     flowcell_discharge = layer_discharge * (zws(k) - zws(k - 1)) / flowcell_height
                  else
                     flowcell_discharge = layer_discharge / (self%indices(i, TOP_LAYER_SOURCE) - self%indices(i, BOTTOM_LAYER_SOURCE) + 1)
                  end if
                  qin(k) = qin(k) + flowcell_discharge
               end if
            end do
         end if

      end do

   end subroutine update_source_sink_discharges

end module m_source_sink
