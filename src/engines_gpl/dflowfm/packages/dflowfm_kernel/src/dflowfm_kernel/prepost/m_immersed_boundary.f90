module m_immersed_boundary
   use, intrinsic :: ieee_arithmetic, only: ieee_is_finite
   use precision, only: dp
   use precision_basics, only: comparereal
   implicit none(type, external)
   private

   public :: t_ibm_cartesian
   public :: ibm_check_cartesian
   public :: t_ibm_cut_cell
   public :: t_ibm_cut_link
   public :: t_ibm_state
   public :: ibm_state
   public :: ibm_reset_state
   public :: ibm_save_full_area
   public :: ibm_save_full_width
   public :: ibm_save_full_link_metrics
   public :: ibm_validate_cutcelllist
   public :: ibm_measure_wet_polygon
   public :: ibm_build_geometry
   public :: ibm_set_perot_weights
   public :: ibm_set_wet_link_metrics
   public :: ibm_set_wet_bed

   !> Result of validating a Cartesian mesh for the immersed-boundary method.
   type :: t_ibm_cartesian
      logical :: is_cartesian = .false. !< Whether the mesh satisfies the Cartesian criteria.
      real(kind=dp) :: x0 = 0.0_dp !< Minimum x coordinate used as the lattice origin.
      real(kind=dp) :: y0 = 0.0_dp !< Minimum y coordinate used as the lattice origin.
      real(kind=dp) :: dx = 0.0_dp !< Uniform horizontal lattice spacing.
      real(kind=dp) :: dy = 0.0_dp !< Uniform vertical lattice spacing.
      real(kind=dp) :: max_dev = 0.0_dp !< Maximum dimensionless deviation found.
      integer :: offending_node = 0 !< First node that violates a Cartesian criterion, if any.
      integer :: offending_link = 0 !< First link that violates a Cartesian criterion, if any.
      integer :: offending_cell = 0 !< First cell that violates a Cartesian criterion, if any.
      character(len=256) :: reason = '' !< Criterion that failed, or empty when accepted.
   end type t_ibm_cartesian

   !> Geometry and area metrics for one cut flow cell.
   type :: t_ibm_cut_cell
      integer :: k = 0 !< Flow-cell index.
      real(kind=dp) :: area_frac = 1.0_dp !< Geometric active area divided by the full area.
      real(kind=dp) :: ba_used = 0.0_dp !< Active area after the configured minimum-area floor.
      real(kind=dp), dimension(2) :: xbi = 0.0_dp !< Representative immersed-segment endpoints.
      real(kind=dp), dimension(2) :: ybi = 0.0_dp !< Representative immersed-segment endpoints.
      real(kind=dp) :: xc = 0.0_dp !< Centroid of the active polygon.
      real(kind=dp) :: yc = 0.0_dp !< Centroid of the active polygon.
      real(kind=dp) :: nx = 0.0_dp !< Unit normal pointing into the active fluid.
      real(kind=dp) :: ny = 0.0_dp !< Unit normal pointing into the active fluid.
      real(kind=dp) :: seg_len = 0.0_dp !< Total immersed-segment length in the cell.
      integer :: nseg = 0 !< Number of immersed segments in the cell.
      logical :: valid = .false. !< Whether the geometry and area cross-check passed.
      character(len=256) :: reason = '' !< Failure reason when valid is false.
   end type t_ibm_cut_cell

   !> Geometry and wet metrics for one link next to an IBM cut cell.
   type :: t_ibm_cut_link
      integer :: L = 0 !< Flow-link index.
      real(kind=dp) :: edge_frac = 1.0_dp !< Wet width divided by the full width.
      real(kind=dp) :: xm = 0.0_dp !< Midpoint of the wet part of the face.
      real(kind=dp) :: ym = 0.0_dp !< Midpoint of the wet part of the face.
      real(kind=dp) :: dxi_wet = 0.0_dp !< Inverse normal distance between wet centroids.
      real(kind=dp), dimension(2) :: tvec = 0.0_dp !< Unit tangent along the face.
      real(kind=dp) :: dt12 = 0.0_dp !< Tangential offset between wet centroids.
      real(kind=dp) :: acl_wet = 0.0_dp !< Interpolation weight at the wet face midpoint.
      real(kind=dp), dimension(2) :: bob_wet = 0.0_dp !< Bed levels at wet-face endpoints.
      logical :: small_edge = .false. !< Whether this link is below the small-edge threshold.
   end type t_ibm_cut_link

   !> Persistent immersed-boundary geometry for the current model mesh.
   type :: t_ibm_state
      logical :: active = .false. !< Whether IBM is enabled for this geometry.
      type(t_ibm_cartesian) :: grid !< Validated Cartesian lattice.
      integer, dimension(:), allocatable :: cell_class !< 0 fluid, 1 cut, 2 solid or absent.
      real(kind=dp), dimension(:), allocatable :: edge_frac !< Wet fraction for each flow link.
      real(kind=dp), dimension(:), allocatable :: wu_full !< Link widths before cut-cell clipping.
      real(kind=dp), dimension(:), allocatable :: ba_full !< Cell areas before cut-cell clipping.
      real(kind=dp), dimension(:), allocatable :: acl_full !< Link weights before IBM overrides.
      real(kind=dp), dimension(:), allocatable :: dxi_full !< Link distances before IBM overrides.
      type(t_ibm_cut_cell), dimension(:), allocatable :: cut !< Cut-cell metadata.
      type(t_ibm_cut_link), dimension(:), allocatable :: clink !< Links adjacent to cut cells.
      integer, dimension(:), allocatable :: cut_link_index !< Cut-link metadata index for each flow link, zero otherwise.
      integer :: ncut = 0 !< Number of cut cells.
      integer :: nghost = 0 !< Number of solid cells referenced by a flow stencil.
      integer :: nfloored = 0 !< Number of cut cells whose area was floored.
      integer :: ndegenerate = 0 !< Number of cells with multiple immersed segments.
      integer :: nclipped_dn = 0 !< Number of wet-centroid link distances clipped to the minimum.
      real(kind=dp) :: min_area_frac = 1.0_dp !< Minimum geometric cut-cell area fraction.
      real(kind=dp) :: area_deficit = 0.0_dp !< Total area added by the minimum-area floor.
   end type t_ibm_state

   type(t_ibm_state), save :: ibm_state

contains

   !> Clear persistent IBM data before a new geometry initialisation.
   subroutine ibm_reset_state()
      ibm_state = t_ibm_state()
   end subroutine ibm_reset_state

   !> Save full cell areas before cut-cell clipping.
   subroutine ibm_save_full_area(area)
      real(kind=dp), dimension(:), intent(in) :: area !< Uncut cell areas.

      if (.not. ibm_state%active) then
         return
      end if
      ibm_state%ba_full = area
   end subroutine ibm_save_full_area

   !> Save full link widths before cut-cell clipping.
   subroutine ibm_save_full_width(width)
      real(kind=dp), dimension(:), intent(in) :: width !< Uncut link widths.

      if (.not. ibm_state%active) then
         return
      end if
      ibm_state%wu_full = width
   end subroutine ibm_save_full_width

   !> Save unmodified interpolation weights and center distances.
   subroutine ibm_save_full_link_metrics(acl, dxi)
      real(kind=dp), dimension(:), intent(in) :: acl !< Unmodified link interpolation weights.
      real(kind=dp), dimension(:), intent(in) :: dxi !< Unmodified inverse center distances.

      if (.not. ibm_state%active) then
         return
      end if
      ibm_state%acl_full = acl
      ibm_state%dxi_full = dxi
   end subroutine ibm_save_full_link_metrics

   !> Verify that a Cutcelllist exists and names at least one non-empty polygon file.
   subroutine ibm_validate_cutcelllist(filename, valid, reason)
      character(len=*), intent(in) :: filename !< Cutcelllist filename.
      logical, intent(out) :: valid !< Whether the list and all named polygon files are usable.
      character(len=*), intent(out) :: reason !< Failure explanation, empty on success.

      integer :: unit, io_status, file_size, file_count
      logical :: list_exists, polygon_exists
      character(len=1024) :: polygon_file

      valid = .false.
      reason = ''
      if (len_trim(filename) == 0) then
         reason = 'ImmersedBoundary=1 requires [geometry] Cutcelllist.'
         return
      end if
      inquire (file=trim(filename), exist=list_exists)
      if (.not. list_exists) then
         reason = 'ImmersedBoundary=1 Cutcelllist does not exist: '//trim(filename)
         return
      end if
      open (newunit=unit, file=trim(filename), status='old', action='read', iostat=io_status)
      if (io_status /= 0) then
         reason = 'ImmersedBoundary=1 could not open Cutcelllist: '//trim(filename)
         return
      end if

      file_count = 0
      do
         read (unit, '(a)', iostat=io_status) polygon_file
         if (io_status < 0) then
            exit
         end if
         if (io_status > 0) then
            reason = 'ImmersedBoundary=1 could not read Cutcelllist: '//trim(filename)
            close (unit)
            return
         end if
         if (len_trim(polygon_file) == 0) then
            cycle
         end if

         inquire (file=trim(polygon_file), exist=polygon_exists)
         if (.not. polygon_exists) then
            reason = 'ImmersedBoundary=1 polygon file does not exist: '//trim(polygon_file)
            close (unit)
            return
         end if
         inquire (file=trim(polygon_file), size=file_size, iostat=io_status)
         if (io_status /= 0) then
            reason = 'ImmersedBoundary=1 could not inspect polygon file: '//trim(polygon_file)
            close (unit)
            return
         end if
         if (file_size <= 0) then
            reason = 'ImmersedBoundary=1 requires a non-empty polygon file: '//trim(polygon_file)
            close (unit)
            return
         end if
         file_count = file_count + 1
      end do
      close (unit, iostat=io_status)
      if (io_status /= 0) then
         reason = 'ImmersedBoundary=1 could not close Cutcelllist: '//trim(filename)
         return
      end if
      if (file_count == 0) then
         reason = 'ImmersedBoundary=1 requires Cutcelllist with at least one polygon.'
         return
      end if
      valid = .true.
   end subroutine ibm_validate_cutcelllist

   !> Compute the area, centroid, immersed segments, and inward normal of a wet polygon.
   pure subroutine ibm_measure_wet_polygon(cell_x, cell_y, wet_x, wet_y, full_area, used_area, baorgfracmin, bamin, snap_tol, cell)
      real(kind=dp), dimension(:), intent(in) :: cell_x !< Full-cell corner x coordinates.
      real(kind=dp), dimension(:), intent(in) :: cell_y !< Full-cell corner y coordinates.
      real(kind=dp), dimension(:), intent(in) :: wet_x !< Active-polygon x coordinates.
      real(kind=dp), dimension(:), intent(in) :: wet_y !< Active-polygon y coordinates.
      real(kind=dp), intent(in) :: full_area !< Area before clipping.
      real(kind=dp), intent(in) :: used_area !< Area after clipping and flooring.
      real(kind=dp), intent(in) :: baorgfracmin !< Configured area-fraction floor.
      real(kind=dp), intent(in) :: bamin !< Absolute minimum cell area.
      real(kind=dp), intent(in) :: snap_tol !< Coordinate tolerance for recognizing cell faces.
      type(t_ibm_cut_cell), intent(out) :: cell !< Computed cut-cell metadata.

      integer :: i, j, nwet
      real(kind=dp) :: xmin, xmax, ymin, ymax, signed_area, area, cxnum, cynum
      real(kind=dp) :: x1, y1, x2, y2, edge_dx, edge_dy, edge_length
      real(kind=dp) :: nx_edge, ny_edge, nx_sum, ny_sum, normal_length
      real(kind=dp) :: segment_length, longest_segment, segment_mid_x, segment_mid_y
      real(kind=dp) :: expected_area, comparison_scale, area_frac_tolerance
      logical :: on_cell_face

      cell = t_ibm_cut_cell()
      if (size(cell_x) /= 4 .or. size(cell_y) /= 4 .or. size(wet_x) /= size(wet_y) .or. size(wet_x) < 3) then
         cell%reason = 'invalid full-cell or wet-polygon coordinates'
         return
      end if
      if (full_area <= 0.0_dp .or. used_area <= 0.0_dp .or. snap_tol <= 0.0_dp .or. baorgfracmin < 0.0_dp .or. bamin < 0.0_dp) then
         cell%reason = 'invalid area or coordinate tolerance'
         return
      end if
      if (any(.not. ieee_is_finite(cell_x)) .or. any(.not. ieee_is_finite(cell_y)) .or. &
          any(.not. ieee_is_finite(wet_x)) .or. any(.not. ieee_is_finite(wet_y))) then
         cell%reason = 'non-finite cell or wet-polygon coordinate'
         return
      end if

      nwet = size(wet_x)
      if (nwet > 3) then
         if (sqrt((wet_x(nwet) - wet_x(1))**2 + (wet_y(nwet) - wet_y(1))**2) <= snap_tol) then
            nwet = nwet - 1
         end if
      end if
      if (nwet < 3) then
         cell%reason = 'wet polygon has fewer than three unique vertices'
         return
      end if

      signed_area = 0.0_dp
      cxnum = 0.0_dp
      cynum = 0.0_dp
      do i = 1, nwet
         j = i + 1
         if (j > nwet) then
            j = 1
         end if
         x1 = wet_x(i)
         y1 = wet_y(i)
         x2 = wet_x(j)
         y2 = wet_y(j)
         signed_area = signed_area + x1 * y2 - x2 * y1
         cxnum = cxnum + (x1 + x2) * (x1 * y2 - x2 * y1)
         cynum = cynum + (y1 + y2) * (x1 * y2 - x2 * y1)
      end do
      signed_area = 0.5_dp * signed_area
      area = abs(signed_area)
      if (area <= tiny(1.0_dp)) then
         cell%reason = 'wet polygon has zero area'
         return
      end if

      cell%xc = cxnum / (6.0_dp * signed_area)
      cell%yc = cynum / (6.0_dp * signed_area)
      cell%area_frac = area / full_area
      cell%ba_used = used_area
      area_frac_tolerance = 1.0e-10_dp
      if (cell%area_frac < 0.0_dp .or. cell%area_frac > 1.0_dp + area_frac_tolerance) then
         cell%reason = 'wet-polygon area is outside the full-cell area'
         return
      end if
      cell%area_frac = min(cell%area_frac, 1.0_dp)

      expected_area = max(area, baorgfracmin * full_area, bamin)
      comparison_scale = max(expected_area, used_area, tiny(1.0_dp))
      if (abs(expected_area - used_area) > 1.0e-10_dp * comparison_scale) then
         cell%reason = 'wet-polygon area does not match cutcelwu area'
         return
      end if

      xmin = minval(cell_x)
      xmax = maxval(cell_x)
      ymin = minval(cell_y)
      ymax = maxval(cell_y)
      nx_sum = 0.0_dp
      ny_sum = 0.0_dp
      longest_segment = 0.0_dp
      do i = 1, nwet
         j = i + 1
         if (j > nwet) then
            j = 1
         end if
         x1 = wet_x(i)
         y1 = wet_y(i)
         x2 = wet_x(j)
         y2 = wet_y(j)
         edge_dx = x2 - x1
         edge_dy = y2 - y1
         edge_length = sqrt(edge_dx * edge_dx + edge_dy * edge_dy)
         if (edge_length <= snap_tol) then
            cycle
         end if
         on_cell_face = (abs(x1 - xmin) <= snap_tol .and. abs(x2 - xmin) <= snap_tol) .or. &
                        (abs(x1 - xmax) <= snap_tol .and. abs(x2 - xmax) <= snap_tol) .or. &
                        (abs(y1 - ymin) <= snap_tol .and. abs(y2 - ymin) <= snap_tol) .or. &
                        (abs(y1 - ymax) <= snap_tol .and. abs(y2 - ymax) <= snap_tol)
         if (on_cell_face) then
            cycle
         end if

         cell%nseg = cell%nseg + 1
         cell%seg_len = cell%seg_len + edge_length
         if (signed_area > 0.0_dp) then
            nx_edge = -edge_dy / edge_length
            ny_edge = edge_dx / edge_length
         else
            nx_edge = edge_dy / edge_length
            ny_edge = -edge_dx / edge_length
         end if
         nx_sum = nx_sum + nx_edge * edge_length
         ny_sum = ny_sum + ny_edge * edge_length
         if (edge_length > longest_segment) then
            longest_segment = edge_length
            cell%xbi = [x1, x2]
            cell%ybi = [y1, y2]
         end if
      end do

      if (cell%area_frac < 1.0_dp - area_frac_tolerance .and. cell%nseg == 0) then
         cell%reason = 'cut cell has no immersed segment'
         return
      end if
      if (cell%nseg > 2) then
         cell%reason = 'active cell has more than two immersed segments'
         return
      end if
      if (cell%nseg > 0) then
         normal_length = sqrt(nx_sum * nx_sum + ny_sum * ny_sum)
         if (normal_length <= tiny(1.0_dp)) then
            cell%reason = 'immersed-segment normals cancel'
            return
         end if
         cell%nx = nx_sum / normal_length
         cell%ny = ny_sum / normal_length
      end if

      cell%valid = .true.
   end subroutine ibm_measure_wet_polygon

   !> Build per-cell and per-link IBM metadata from the wet polygons produced by cutcelwu.
   pure subroutine ibm_build_geometry(cell_corner_x, cell_corner_y, wet_x, wet_y, wet_count, area_full, area_used, &
                                      link_cells, face_x, face_y, width_full, width_used, dx, dy, baorgfracmin, bamin, &
                                      cartesian_tol, small_edge_frac, state, valid, error_index, reason)
      real(kind=dp), dimension(:, :), intent(in) :: cell_corner_x !< Four full-cell corner coordinates.
      real(kind=dp), dimension(:, :), intent(in) :: cell_corner_y !< Four full-cell corner coordinates.
      real(kind=dp), dimension(:, :), intent(in) :: wet_x !< Wet-polygon x coordinates, one column per cell.
      real(kind=dp), dimension(:, :), intent(in) :: wet_y !< Wet-polygon y coordinates, one column per cell.
      integer, dimension(:), intent(in) :: wet_count !< Number of wet-polygon vertices for each cell.
      real(kind=dp), dimension(:), intent(in) :: area_full !< Cell areas before clipping.
      real(kind=dp), dimension(:), intent(in) :: area_used !< Cell areas after clipping and flooring.
      integer, dimension(:, :), intent(in) :: link_cells !< Flow-cell endpoints for each link.
      real(kind=dp), dimension(:, :), intent(in) :: face_x !< Link-face endpoint x coordinates.
      real(kind=dp), dimension(:, :), intent(in) :: face_y !< Link-face endpoint y coordinates.
      real(kind=dp), dimension(:), intent(in) :: width_full !< Link widths before clipping.
      real(kind=dp), dimension(:), intent(in) :: width_used !< Link widths after clipping.
      real(kind=dp), intent(in) :: dx !< Cartesian lattice spacing in x.
      real(kind=dp), intent(in) :: dy !< Cartesian lattice spacing in y.
      real(kind=dp), intent(in) :: baorgfracmin !< Configured minimum cut-cell area fraction.
      real(kind=dp), intent(in) :: bamin !< Absolute minimum cell area.
      real(kind=dp), intent(in) :: cartesian_tol !< Relative tolerance of the Cartesian check.
      real(kind=dp), intent(in) :: small_edge_frac !< Threshold for classifying a small edge.
      type(t_ibm_state), intent(inout) :: state !< IBM state receiving the geometry metadata.
      logical, intent(out) :: valid !< Whether every cell and link passed the geometry checks.
      integer, intent(out) :: error_index !< Cell or link index associated with the first failure.
      character(len=*), intent(out) :: reason !< Failure explanation, empty on success.

      integer :: ncell, nlink, k, L, i, count_cut, count_clink, cell1, cell2, wet_cell
      real(kind=dp) :: snap_tol, area_tolerance, edge_fraction, face_fraction, xm, ym
      type(t_ibm_cut_cell), allocatable :: cell_data(:)
      logical :: face_valid, adjacent_cut
      character(len=256) :: cell_reason

      valid = .false.
      error_index = 0
      reason = ''
      ncell = size(wet_count)
      nlink = size(width_full)
      if (size(cell_corner_x, 1) /= 4 .or. size(cell_corner_y, 1) /= 4 .or. &
          size(cell_corner_x, 2) /= ncell .or. size(cell_corner_y, 2) /= ncell .or. &
          size(wet_x, 2) /= ncell .or. size(wet_y, 2) /= ncell .or. &
          size(area_full) < ncell .or. size(area_used) < ncell .or. &
          size(link_cells, 1) /= 2 .or. size(link_cells, 2) /= nlink .or. &
          size(face_x, 1) /= 2 .or. size(face_y, 1) /= 2 .or. &
          size(face_x, 2) /= nlink .or. size(face_y, 2) /= nlink .or. &
          size(width_used) /= nlink) then
         reason = 'inconsistent IBM geometry array dimensions'
         return
      end if
      if (dx <= 0.0_dp .or. dy <= 0.0_dp .or. baorgfracmin < 0.0_dp .or. bamin < 0.0_dp .or. &
          cartesian_tol <= 0.0_dp .or. small_edge_frac < 0.0_dp .or. small_edge_frac > 1.0_dp) then
         reason = 'invalid IBM geometry setting'
         return
      end if

      snap_tol = 1.0e-8_dp * min(dx, dy)
      area_tolerance = max(1.0e-12_dp, cartesian_tol)
      allocate (cell_data(ncell))
      if (allocated(state%cell_class)) then
         deallocate (state%cell_class)
      end if
      if (allocated(state%edge_frac)) then
         deallocate (state%edge_frac)
      end if
      if (allocated(state%cut)) then
         deallocate (state%cut)
      end if
      if (allocated(state%clink)) then
         deallocate (state%clink)
      end if
      allocate (state%cell_class(size(area_full)))
      state%cell_class = 2
      state%ncut = 0
      state%nghost = 0
      state%nfloored = 0
      state%ndegenerate = 0
      state%min_area_frac = 1.0_dp
      state%area_deficit = 0.0_dp

      do k = 1, ncell
         if (area_full(k) <= 0.0_dp) then
            cycle
         end if
         if (wet_count(k) < 3 .or. wet_count(k) > size(wet_x, 1)) then
            error_index = k
            reason = 'invalid wet-polygon vertex count'
            return
         end if
         call ibm_measure_wet_polygon(cell_corner_x(:, k), cell_corner_y(:, k), wet_x(1:wet_count(k), k), &
                                      wet_y(1:wet_count(k), k), area_full(k), area_used(k), baorgfracmin, bamin, &
                                      snap_tol, cell_data(k))
         if (.not. cell_data(k)%valid) then
            error_index = k
            reason = cell_data(k)%reason
            return
         end if
         cell_data(k)%k = k
         if (cell_data(k)%area_frac <= area_tolerance) then
            state%cell_class(k) = 2
         else if (cell_data(k)%area_frac < 1.0_dp - area_tolerance) then
            state%cell_class(k) = 1
            state%ncut = state%ncut + 1
            state%min_area_frac = min(state%min_area_frac, cell_data(k)%area_frac)
            if (area_used(k) > cell_data(k)%area_frac * area_full(k) + 1.0e-10_dp * area_full(k)) then
               state%nfloored = state%nfloored + 1
               state%area_deficit = state%area_deficit + area_used(k) - cell_data(k)%area_frac * area_full(k)
            end if
            if (cell_data(k)%nseg > 1) then
               state%ndegenerate = state%ndegenerate + 1
            end if
         else
            state%cell_class(k) = 0
         end if
      end do
      if (state%ncut == 0) then
         state%min_area_frac = 1.0_dp
      end if
      allocate (state%cut(state%ncut))
      count_cut = 0
      do k = 1, ncell
         if (state%cell_class(k) == 1) then
            count_cut = count_cut + 1
            state%cut(count_cut) = cell_data(k)
         end if
      end do

      allocate (state%edge_frac(nlink))
      state%edge_frac = 0.0_dp
      do L = 1, nlink
         if (width_full(L) > tiny(1.0_dp)) then
            edge_fraction = width_used(L) / width_full(L)
            if (edge_fraction < -area_tolerance .or. edge_fraction > 1.0_dp + area_tolerance) then
               error_index = L
               reason = 'cut link width is outside its full width'
               return
            end if
            state%edge_frac(L) = min(1.0_dp, max(0.0_dp, edge_fraction))
         end if
      end do

      count_clink = 0
      do L = 1, nlink
         cell1 = link_cells(1, L)
         cell2 = link_cells(2, L)
         adjacent_cut = .false.
         if (cell1 >= 1 .and. cell1 <= ncell) then
            adjacent_cut = state%cell_class(cell1) == 1
         end if
         if (cell2 >= 1 .and. cell2 <= ncell) then
            adjacent_cut = adjacent_cut .or. state%cell_class(cell2) == 1
         end if
         if (adjacent_cut) then
            count_clink = count_clink + 1
         end if
      end do
      allocate (state%clink(count_clink))
      count_clink = 0
      do L = 1, nlink
         cell1 = link_cells(1, L)
         cell2 = link_cells(2, L)
         wet_cell = 0
         if (cell1 >= 1 .and. cell1 <= ncell) then
            if (state%cell_class(cell1) == 1) then
               wet_cell = cell1
            end if
         end if
         if (wet_cell == 0 .and. cell2 >= 1 .and. cell2 <= ncell) then
            if (state%cell_class(cell2) == 1) then
               wet_cell = cell2
            end if
         end if
         if (wet_cell == 0) then
            cycle
         end if

         call ibm_measure_wet_face(face_x(:, L), face_y(:, L), wet_x(1:wet_count(wet_cell), wet_cell), &
                                   wet_y(1:wet_count(wet_cell), wet_cell), snap_tol, face_fraction, xm, ym, face_valid)
         if (state%edge_frac(L) > area_tolerance .and. .not. face_valid) then
            error_index = L
            reason = 'wet link face could not be recovered from the cut-cell polygon'
            return
         end if
         if (face_valid) then
            if (abs(face_fraction - state%edge_frac(L)) > 1.0e-10_dp) then
               error_index = L
               reason = 'wet link face does not match cutcelwu width'
               return
            end if
         else
            xm = 0.5_dp * (face_x(1, L) + face_x(2, L))
            ym = 0.5_dp * (face_y(1, L) + face_y(2, L))
         end if
         count_clink = count_clink + 1
         state%clink(count_clink)%L = L
         state%clink(count_clink)%edge_frac = state%edge_frac(L)
         state%clink(count_clink)%xm = xm
         state%clink(count_clink)%ym = ym
         state%clink(count_clink)%small_edge = state%edge_frac(L) < small_edge_frac
      end do

      valid = .true.
   end subroutine ibm_build_geometry

   pure subroutine ibm_set_perot_weights(link_cells, width_used, wcx1, wcy1, wcx2, wcy2, state, valid, error_index, reason)
      integer, dimension(:, :), intent(in) :: link_cells !< Flow-cell endpoints for each link.
      real(kind=dp), dimension(:), intent(in) :: width_used !< Active link widths after clipping.
      real(kind=dp), dimension(:), intent(inout) :: wcx1, wcy1, wcx2, wcy2 !< Perot weights to override on cut-cell sides.
      type(t_ibm_state), intent(in) :: state !< Cut-cell metadata and active face midpoints.
      logical, intent(out) :: valid !< Whether the wet-polygon weights were computed.
      integer, intent(out) :: error_index !< Link or cell associated with the first failure.
      character(len=*), intent(out) :: reason !< Failure explanation, empty on success.

      integer, allocatable :: cut_index(:)
      logical, allocatable :: has_face_midpoint(:)
      real(kind=dp), allocatable :: face_mid_x(:), face_mid_y(:)
      integer :: ncell, nlink, i, L, k1, k2, cut_index1, cut_index2
      real(kind=dp) :: wet_area

      valid = .false.
      error_index = 0
      reason = ''
      if (.not. allocated(state%cell_class) .or. .not. allocated(state%ba_full) .or. .not. allocated(state%cut) .or. &
          .not. allocated(state%clink)) then
         reason = 'IBM Perot weights require initialized cut-cell geometry'
         return
      end if
      ncell = size(state%cell_class)
      nlink = size(width_used)
      if (size(link_cells, 1) /= 2 .or. size(link_cells, 2) /= nlink .or. &
          size(wcx1) /= nlink .or. size(wcy1) /= nlink .or. size(wcx2) /= nlink .or. size(wcy2) /= nlink) then
         reason = 'inconsistent IBM Perot-weight array dimensions'
         return
      end if
      if (size(state%ba_full) < ncell) then
         reason = 'IBM full-cell areas are shorter than the cut-cell state'
         return
      end if

      valid = .true.
      if (state%ncut == 0) then
         return
      end if

      allocate (cut_index(ncell), has_face_midpoint(nlink), face_mid_x(nlink), face_mid_y(nlink))
      cut_index = 0
      has_face_midpoint = .false.
      do i = 1, size(state%cut)
         k1 = state%cut(i)%k
         if (k1 < 1 .or. k1 > ncell) then
            valid = .false.
            error_index = k1
            reason = 'cut-cell index is outside the IBM Perot grid'
            return
         end if
         cut_index(k1) = i
      end do
      do i = 1, size(state%clink)
         L = state%clink(i)%L
         if (L < 1 .or. L > nlink) then
            valid = .false.
            error_index = L
            reason = 'cut-link index is outside the IBM Perot grid'
            return
         end if
         has_face_midpoint(L) = .true.
         face_mid_x(L) = state%clink(i)%xm
         face_mid_y(L) = state%clink(i)%ym
      end do

      do L = 1, nlink
         k1 = link_cells(1, L)
         k2 = link_cells(2, L)
         cut_index1 = 0
         cut_index2 = 0
         if (k1 >= 1 .and. k1 <= ncell) then
            cut_index1 = cut_index(k1)
         end if
         if (k2 >= 1 .and. k2 <= ncell) then
            cut_index2 = cut_index(k2)
         end if
         if (cut_index1 == 0 .and. cut_index2 == 0) then
            cycle
         end if
         if (.not. has_face_midpoint(L)) then
            valid = .false.
            error_index = L
            reason = 'cut link has no wet-face midpoint'
            return
         end if
         if (width_used(L) < 0.0_dp) then
            valid = .false.
            error_index = L
            reason = 'cut link has a negative wet width'
            return
         end if

         if (cut_index1 > 0) then
            wet_area = state%cut(cut_index1)%area_frac * state%ba_full(k1)
            if (wet_area <= tiny(1.0_dp)) then
               valid = .false.
               error_index = k1
               reason = 'cut cell has no geometric wet area for Perot weights'
               return
            end if
            if (width_used(L) > 0.0_dp) then
               wcx1(L) = (face_mid_x(L) - state%cut(cut_index1)%xc) * width_used(L) / wet_area
               wcy1(L) = (face_mid_y(L) - state%cut(cut_index1)%yc) * width_used(L) / wet_area
            else
               wcx1(L) = 0.0_dp
               wcy1(L) = 0.0_dp
            end if
         end if
         if (cut_index2 > 0) then
            wet_area = state%cut(cut_index2)%area_frac * state%ba_full(k2)
            if (wet_area <= tiny(1.0_dp)) then
               valid = .false.
               error_index = k2
               reason = 'cut cell has no geometric wet area for Perot weights'
               return
            end if
            if (width_used(L) > 0.0_dp) then
               wcx2(L) = -(face_mid_x(L) - state%cut(cut_index2)%xc) * width_used(L) / wet_area
               wcy2(L) = -(face_mid_y(L) - state%cut(cut_index2)%yc) * width_used(L) / wet_area
            else
               wcx2(L) = 0.0_dp
               wcy2(L) = 0.0_dp
            end if
         end if
      end do
   end subroutine ibm_set_perot_weights

   !> Compute wet-centroid link metrics for pressure gradients and interpolation.
   subroutine ibm_set_wet_link_metrics(link_cells, cell_x, cell_y, normal_x, normal_y, width_used, acl_full, dxi_full, &
                                       state, valid, error_index, reason)
      integer, dimension(:, :), intent(in) :: link_cells !< Flow-cell endpoints for each link.
      real(kind=dp), dimension(:), intent(in) :: cell_x !< Full-cell center x coordinates.
      real(kind=dp), dimension(:), intent(in) :: cell_y !< Full-cell center y coordinates.
      real(kind=dp), dimension(:), intent(in) :: normal_x !< Link normal x components, oriented from cell 1 to 2.
      real(kind=dp), dimension(:), intent(in) :: normal_y !< Link normal y components, oriented from cell 1 to 2.
      real(kind=dp), dimension(:), intent(in) :: width_used !< Active link widths after clipping.
      real(kind=dp), dimension(:), intent(in) :: acl_full !< Unmodified interpolation weights.
      real(kind=dp), dimension(:), intent(in) :: dxi_full !< Unmodified inverse center distances.
      type(t_ibm_state), intent(inout) :: state !< Cut-cell and cut-link metadata.
      logical, intent(out) :: valid !< Whether the wet-link metrics were computed.
      integer, intent(out) :: error_index !< Link or cell associated with the first failure.
      character(len=*), intent(out) :: reason !< Failure explanation, empty on success.

      integer, allocatable :: cut_index(:)
      integer :: ncell, nlink, i, L, k1, k2, cut1, cut2
      real(kind=dp) :: c1x, c1y, c2x, c2y, nx, ny, normal_length
      real(kind=dp) :: dn, dmin, acl_wet

      valid = .false.
      error_index = 0
      reason = ''
      if (.not. allocated(state%cell_class) .or. .not. allocated(state%cut) .or. .not. allocated(state%clink)) then
         reason = 'IBM wet-link metrics require initialized cut-cell geometry'
         return
      end if
      ncell = size(state%cell_class)
      nlink = size(width_used)
      if (size(link_cells, 1) /= 2 .or. size(link_cells, 2) /= nlink .or. size(cell_x) < ncell .or. &
          size(cell_y) < ncell .or. size(normal_x) /= nlink .or. size(normal_y) /= nlink .or. &
          size(acl_full) /= nlink .or. size(dxi_full) /= nlink) then
         reason = 'inconsistent IBM wet-link metric array dimensions'
         return
      end if
      if (.not. ieee_is_finite(state%grid%dx) .or. .not. ieee_is_finite(state%grid%dy) .or. &
          state%grid%dx <= tiny(1.0_dp) .or. state%grid%dy <= tiny(1.0_dp)) then
         reason = 'IBM wet-link metrics require positive Cartesian grid spacing'
         return
      end if

      allocate (cut_index(ncell))
      cut_index = 0
      do i = 1, size(state%cut)
         k1 = state%cut(i)%k
         if (k1 < 1 .or. k1 > ncell) then
            error_index = k1
            reason = 'cut-cell index is outside the IBM wet-link grid'
            return
         end if
         cut_index(k1) = i
      end do

      if (allocated(state%cut_link_index)) then
         deallocate (state%cut_link_index)
      end if
      allocate (state%cut_link_index(nlink))
      state%cut_link_index = 0
      state%nclipped_dn = 0

      do i = 1, size(state%clink)
         L = state%clink(i)%L
         if (L < 1 .or. L > nlink) then
            error_index = L
            reason = 'cut-link index is outside the IBM wet-link grid'
            return
         end if
         if (state%cut_link_index(L) /= 0) then
            error_index = L
            reason = 'duplicate IBM cut-link metadata'
            return
         end if
         state%cut_link_index(L) = i
         if (width_used(L) < 0.0_dp) then
            error_index = L
            reason = 'cut link has a negative wet width'
            return
         end if

         k1 = link_cells(1, L)
         k2 = link_cells(2, L)
         if (k1 < 1 .or. k1 > ncell .or. k2 < 1 .or. k2 > ncell) then
            if (width_used(L) > tiny(1.0_dp)) then
               error_index = L
               reason = 'active IBM cut link has an invalid cell endpoint'
               return
            end if
            state%clink(i)%dxi_wet = dxi_full(L)
            state%clink(i)%acl_wet = acl_full(L)
            cycle
         end if

         cut1 = cut_index(k1)
         cut2 = cut_index(k2)
         c1x = cell_x(k1)
         c1y = cell_y(k1)
         c2x = cell_x(k2)
         c2y = cell_y(k2)
         if (cut1 > 0) then
            c1x = state%cut(cut1)%xc
            c1y = state%cut(cut1)%yc
         else if (state%cell_class(k1) == 2 .and. width_used(L) > tiny(1.0_dp)) then
            error_index = L
            reason = 'active IBM cut link is adjacent to a solid cell'
            return
         end if
         if (cut2 > 0) then
            c2x = state%cut(cut2)%xc
            c2y = state%cut(cut2)%yc
         else if (state%cell_class(k2) == 2 .and. width_used(L) > tiny(1.0_dp)) then
            error_index = L
            reason = 'active IBM cut link is adjacent to a solid cell'
            return
         end if

         normal_length = sqrt(normal_x(L) * normal_x(L) + normal_y(L) * normal_y(L))
         if (.not. ieee_is_finite(normal_length) .or. normal_length <= tiny(1.0_dp)) then
            error_index = L
            reason = 'IBM cut link has an invalid normal'
            return
         end if
         nx = normal_x(L) / normal_length
         ny = normal_y(L) / normal_length
         dn = (c2x - c1x) * nx + (c2y - c1y) * ny
         if (.not. ieee_is_finite(dn) .or. dn <= tiny(1.0_dp)) then
            error_index = L
            reason = 'IBM wet centroids are not ordered along the link normal'
            return
         end if

         dmin = 0.25_dp * sqrt((nx * state%grid%dx)**2 + (ny * state%grid%dy)**2)
         state%clink(i)%tvec = [-ny, nx]
         state%clink(i)%dt12 = (c2x - c1x) * state%clink(i)%tvec(1) + (c2y - c1y) * state%clink(i)%tvec(2)
         state%clink(i)%dxi_wet = 1.0_dp / max(dn, dmin)
         if (dn < dmin) then
            state%nclipped_dn = state%nclipped_dn + 1
         end if

         acl_wet = ((state%clink(i)%xm - c1x) * nx + (state%clink(i)%ym - c1y) * ny) / dn
         if (.not. ieee_is_finite(acl_wet)) then
            error_index = L
            reason = 'IBM wet interpolation weight is not finite'
            return
         end if
         if (acl_wet <= 0.0_dp .or. acl_wet >= 1.0_dp) then
            if (width_used(L) > tiny(1.0_dp)) then
               error_index = L
               reason = 'IBM wet interpolation weight is outside (0, 1)'
               return
            end if
            acl_wet = acl_full(L)
         end if
         state%clink(i)%acl_wet = acl_wet
      end do

      valid = .true.
   end subroutine ibm_set_wet_link_metrics

   !> Re-evaluate cut-cell bed levels by least-squares extrapolation to wet centroids.
   subroutine ibm_set_wet_bed(link_cells, width_used, cell_x, cell_y, missing_value, bed_level, state, valid, error_index, reason)
      integer, dimension(:, :), intent(in) :: link_cells !< Flow-cell endpoints for each link.
      real(kind=dp), dimension(:), intent(in) :: width_used !< Active link widths after clipping.
      real(kind=dp), dimension(:), intent(in) :: cell_x !< Full-cell center x coordinates.
      real(kind=dp), dimension(:), intent(in) :: cell_y !< Full-cell center y coordinates.
      real(kind=dp), intent(in) :: missing_value !< Missing bed-level sentinel.
      real(kind=dp), dimension(:), intent(inout) :: bed_level !< Cell bed levels, updated at cut cells.
      type(t_ibm_state), intent(in) :: state !< Cut-cell metadata and active link geometry.
      logical, intent(out) :: valid !< Whether all cut-cell bed levels were updated.
      integer, intent(out) :: error_index !< Cell associated with the first failure.
      character(len=*), intent(out) :: reason !< Failure explanation, empty on success.

      integer, allocatable :: degree(:), offsets(:), neighbors(:), cursor(:), queue(:), queue_depth(:), visited(:)
      integer :: ncell, nlink, i, L, k1, k2, nentries, cell, next_cell, head, tail, depth, sample_count
      integer :: row, column
      real(kind=dp), dimension(3, 3) :: normal_matrix
      real(kind=dp), dimension(3) :: rhs, basis, solution
      real(kind=dp) :: xc, yc, bed_value
      logical :: solve_valid

      valid = .false.
      error_index = 0
      reason = ''
      if (.not. allocated(state%cell_class) .or. .not. allocated(state%cut)) then
         reason = 'IBM wet-bed update requires initialized cut-cell geometry'
         return
      end if
      ncell = size(state%cell_class)
      nlink = size(width_used)
      if (size(link_cells, 1) /= 2 .or. size(link_cells, 2) /= nlink .or. size(cell_x) < ncell .or. &
          size(cell_y) < ncell .or. size(bed_level) < ncell) then
         reason = 'inconsistent IBM wet-bed array dimensions'
         return
      end if
      if (.not. ieee_is_finite(state%grid%dx) .or. .not. ieee_is_finite(state%grid%dy) .or. &
          state%grid%dx <= tiny(1.0_dp) .or. state%grid%dy <= tiny(1.0_dp)) then
         reason = 'IBM wet-bed update requires positive Cartesian grid spacing'
         return
      end if

      valid = .true.
      if (state%ncut == 0) then
         return
      end if

      allocate (degree(ncell), offsets(ncell + 1), cursor(ncell), queue(ncell), queue_depth(ncell), visited(ncell))
      degree = 0
      do L = 1, nlink
         if (width_used(L) < 0.0_dp) then
            valid = .false.
            error_index = L
            reason = 'IBM wet-bed adjacency contains a negative link width'
            return
         end if
         if (width_used(L) <= tiny(1.0_dp)) then
            cycle
         end if
         k1 = link_cells(1, L)
         k2 = link_cells(2, L)
         if (k1 < 1 .or. k1 > ncell .or. k2 < 1 .or. k2 > ncell .or. k1 == k2) then
            cycle
         end if
         if (state%cell_class(k1) == 2 .or. state%cell_class(k2) == 2) then
            cycle
         end if
         degree(k1) = degree(k1) + 1
         degree(k2) = degree(k2) + 1
      end do

      offsets(1) = 1
      do cell = 1, ncell
         offsets(cell + 1) = offsets(cell) + degree(cell)
      end do
      nentries = offsets(ncell + 1) - 1
      allocate (neighbors(nentries))
      cursor = offsets(1:ncell)
      do L = 1, nlink
         if (width_used(L) <= tiny(1.0_dp)) then
            cycle
         end if
         k1 = link_cells(1, L)
         k2 = link_cells(2, L)
         if (k1 < 1 .or. k1 > ncell .or. k2 < 1 .or. k2 > ncell .or. k1 == k2) then
            cycle
         end if
         if (state%cell_class(k1) == 2 .or. state%cell_class(k2) == 2) then
            cycle
         end if
         neighbors(cursor(k1)) = k2
         neighbors(cursor(k2)) = k1
         cursor(k1) = cursor(k1) + 1
         cursor(k2) = cursor(k2) + 1
      end do

      visited = 0
      do i = 1, size(state%cut)
         k1 = state%cut(i)%k
         if (k1 < 1 .or. k1 > ncell) then
            valid = .false.
            error_index = k1
            reason = 'cut-cell index is outside the IBM wet-bed grid'
            return
         end if
         xc = state%cut(i)%xc
         yc = state%cut(i)%yc
         if (.not. ieee_is_finite(xc) .or. .not. ieee_is_finite(yc)) then
            valid = .false.
            error_index = k1
            reason = 'cut-cell wet centroid is not finite'
            return
         end if

         normal_matrix = 0.0_dp
         rhs = 0.0_dp
         sample_count = 0
         head = 1
         tail = 1
         queue(1) = k1
         queue_depth(1) = 0
         visited(k1) = i
         do while (head <= tail)
            cell = queue(head)
            depth = queue_depth(head)
            if (depth < 2) then
               do L = offsets(cell), offsets(cell + 1) - 1
                  next_cell = neighbors(L)
                  if (visited(next_cell) == i) then
                     cycle
                  end if
                  visited(next_cell) = i
                  if (state%cell_class(next_cell) == 0) then
                     bed_value = bed_level(next_cell)
                     if (ieee_is_finite(bed_value) .and. comparereal(bed_value, missing_value) /= 0) then
                        basis(1) = 1.0_dp
                        basis(2) = (cell_x(next_cell) - xc) / state%grid%dx
                        basis(3) = (cell_y(next_cell) - yc) / state%grid%dy
                        do row = 1, 3
                           rhs(row) = rhs(row) + basis(row) * bed_value
                           do column = 1, 3
                              normal_matrix(row, column) = normal_matrix(row, column) + basis(row) * basis(column)
                           end do
                        end do
                        sample_count = sample_count + 1
                     end if
                  end if
                  tail = tail + 1
                  queue(tail) = next_cell
                  queue_depth(tail) = depth + 1
               end do
            end if
            head = head + 1
         end do

         if (sample_count < 3) then
            valid = .false.
            error_index = k1
            reason = 'fewer than three uncut wet neighbours are available for the bed-level fit'
            return
         end if
         call ibm_solve_3x3(normal_matrix, rhs, solution, solve_valid)
         if (.not. solve_valid .or. .not. ieee_is_finite(solution(1))) then
            valid = .false.
            error_index = k1
            reason = 'uncut wet neighbours do not span a plane for the bed-level fit'
            return
         end if
         bed_level(k1) = solution(1)
      end do
   end subroutine ibm_set_wet_bed

   pure subroutine ibm_solve_3x3(matrix, rhs, solution, valid)
      real(kind=dp), dimension(3, 3), intent(in) :: matrix !< Linear-system matrix.
      real(kind=dp), dimension(3), intent(in) :: rhs !< Linear-system right-hand side.
      real(kind=dp), dimension(3), intent(out) :: solution !< Solution vector.
      logical, intent(out) :: valid !< Whether the system is nonsingular.

      real(kind=dp), dimension(3, 3) :: work
      real(kind=dp), dimension(3) :: work_rhs, row_temp
      real(kind=dp) :: scale, pivot_value, factor, rhs_temp
      integer :: column, row, pivot

      work = matrix
      work_rhs = rhs
      solution = 0.0_dp
      valid = .false.
      scale = maxval(abs(work))
      if (scale <= tiny(1.0_dp)) then
         return
      end if
      do column = 1, 3
         pivot = column
         do row = column + 1, 3
            if (abs(work(row, column)) > abs(work(pivot, column))) then
               pivot = row
            end if
         end do
         pivot_value = work(pivot, column)
         if (abs(pivot_value) <= 100.0_dp * epsilon(1.0_dp) * scale) then
            return
         end if
         if (pivot /= column) then
            row_temp = work(pivot, :)
            work(pivot, :) = work(column, :)
            work(column, :) = row_temp
            rhs_temp = work_rhs(pivot)
            work_rhs(pivot) = work_rhs(column)
            work_rhs(column) = rhs_temp
         end if
         pivot_value = work(column, column)
         work(column, :) = work(column, :) / pivot_value
         work_rhs(column) = work_rhs(column) / pivot_value
         do row = 1, 3
            if (row == column) then
               cycle
            end if
            factor = work(row, column)
            work(row, :) = work(row, :) - factor * work(column, :)
            work_rhs(row) = work_rhs(row) - factor * work_rhs(column)
         end do
      end do
      solution = work_rhs
      valid = .true.
   end subroutine ibm_solve_3x3

   pure subroutine ibm_measure_wet_face(face_x, face_y, wet_x, wet_y, tolerance, face_fraction, xm, ym, valid)
      real(kind=dp), dimension(2), intent(in) :: face_x !< Face endpoint x coordinates.
      real(kind=dp), dimension(2), intent(in) :: face_y !< Face endpoint y coordinates.
      real(kind=dp), dimension(:), intent(in) :: wet_x !< Wet-polygon x coordinates.
      real(kind=dp), dimension(:), intent(in) :: wet_y !< Wet-polygon y coordinates.
      real(kind=dp), intent(in) :: tolerance !< Coordinate tolerance.
      real(kind=dp), intent(out) :: face_fraction !< Recovered wet fraction of the full face.
      real(kind=dp), intent(out) :: xm !< Midpoint x coordinate of the wet face interval.
      real(kind=dp), intent(out) :: ym !< Midpoint y coordinate of the wet face interval.
      logical, intent(out) :: valid !< Whether at least two wet face vertices were found.

      integer :: i
      real(kind=dp) :: dx_face, dy_face, face_length, face_length_squared
      real(kind=dp) :: projection, distance, projection_min, projection_max
      integer :: point_count

      valid = .false.
      face_fraction = 0.0_dp
      xm = 0.0_dp
      ym = 0.0_dp
      dx_face = face_x(2) - face_x(1)
      dy_face = face_y(2) - face_y(1)
      face_length_squared = dx_face * dx_face + dy_face * dy_face
      face_length = sqrt(face_length_squared)
      if (face_length <= tiny(1.0_dp) .or. tolerance <= 0.0_dp .or. size(wet_x) /= size(wet_y)) then
         return
      end if

      projection_min = huge(1.0_dp)
      projection_max = -huge(1.0_dp)
      point_count = 0
      do i = 1, size(wet_x)
         projection = ((wet_x(i) - face_x(1)) * dx_face + (wet_y(i) - face_y(1)) * dy_face) / face_length_squared
         distance = abs((wet_x(i) - face_x(1)) * dy_face - (wet_y(i) - face_y(1)) * dx_face) / face_length
         if (distance <= tolerance .and. projection >= -tolerance / face_length .and. projection <= 1.0_dp + tolerance / face_length) then
            projection_min = min(projection_min, projection)
            projection_max = max(projection_max, projection)
            point_count = point_count + 1
         end if
      end do
      if (point_count < 2 .or. projection_max <= projection_min) then
         return
      end if
      projection_min = max(0.0_dp, projection_min)
      projection_max = min(1.0_dp, projection_max)
      face_fraction = projection_max - projection_min
      xm = face_x(1) + 0.5_dp * (projection_min + projection_max) * dx_face
      ym = face_y(1) + 0.5_dp * (projection_min + projection_max) * dy_face
      valid = .true.
   end subroutine ibm_measure_wet_face

   !> Check a node/link/cell mesh against an axis-aligned uniform Cartesian lattice.
   pure subroutine ibm_check_cartesian(x, y, link_nodes, cell_offsets, cell_nodes, tolerance, result)
      real(kind=dp), dimension(:), intent(in) :: x !< Net-node x coordinates.
      real(kind=dp), dimension(:), intent(in) :: y !< Net-node y coordinates.
      integer, dimension(:, :), intent(in) :: link_nodes !< Endpoint node indices for each link.
      integer, dimension(:), intent(in) :: cell_offsets !< One-based CSR offsets for cell_nodes.
      integer, dimension(:), intent(in) :: cell_nodes !< Flattened cell-to-node connectivity.
      real(kind=dp), intent(in) :: tolerance !< Relative tolerance, in (0, 1e-3].
      type(t_ibm_cartesian), intent(out) :: result !< Cartesian check result.

      integer :: i, link, cell, node1, node2, node_count
      logical :: has_horizontal_link, has_vertical_link
      real(kind=dp) :: link_dx, link_dy, link_length, horizontal_min, horizontal_max
      real(kind=dp) :: vertical_min, vertical_max, lattice_coordinate, deviation

      result = t_ibm_cartesian()

      if (tolerance <= 0.0_dp .or. tolerance > 1.0e-3_dp) then
         result%reason = 'tolerance must be in (0, 1e-3]'
         return
      end if
      if (size(x) == 0 .or. size(x) /= size(y)) then
         result%reason = 'invalid node coordinates'
         return
      end if
      if (size(link_nodes, 1) < 2 .or. size(cell_offsets) == 0) then
         result%reason = 'invalid mesh connectivity'
         return
      end if
      if (cell_offsets(1) /= 1 .or. cell_offsets(size(cell_offsets)) /= size(cell_nodes) + 1) then
         result%reason = 'invalid cell connectivity offsets'
         return
      end if

      do i = 1, size(x)
         if (.not. ieee_is_finite(x(i)) .or. .not. ieee_is_finite(y(i))) then
            result%offending_node = i
            result%reason = 'non-finite node coordinate'
            return
         end if
      end do

      if (size(cell_offsets) <= 1) then
         result%reason = 'no 2D cells'
         return
      end if
      do cell = 1, size(cell_offsets) - 1
         node_count = cell_offsets(cell + 1) - cell_offsets(cell)
         if (node_count /= 4) then
            result%offending_cell = cell
            result%reason = 'each 2D cell must have 4 nodes'
            return
         end if
         if (cell_offsets(cell) < 1 .or. cell_offsets(cell + 1) > size(cell_nodes) + 1) then
            result%offending_cell = cell
            result%reason = 'invalid cell connectivity offsets'
            return
         end if
         do i = cell_offsets(cell), cell_offsets(cell + 1) - 1
            if (cell_nodes(i) < 1 .or. cell_nodes(i) > size(x)) then
               result%offending_cell = cell
               result%reason = 'invalid cell node index'
               return
            end if
         end do
      end do

      result%x0 = minval(x)
      result%y0 = minval(y)
      horizontal_min = huge(1.0_dp)
      horizontal_max = 0.0_dp
      vertical_min = huge(1.0_dp)
      vertical_max = 0.0_dp
      has_horizontal_link = .false.
      has_vertical_link = .false.

      do link = 1, size(link_nodes, 2)
         node1 = link_nodes(1, link)
         node2 = link_nodes(2, link)
         if (node1 < 1 .or. node1 > size(x) .or. node2 < 1 .or. node2 > size(x)) then
            result%offending_link = link
            result%reason = 'invalid link node index'
            return
         end if

         link_dx = abs(x(node2) - x(node1))
         link_dy = abs(y(node2) - y(node1))
         link_length = max(link_dx, link_dy)
         if (link_length <= tiny(1.0_dp)) then
            result%offending_link = link
            result%offending_node = node2
            result%reason = 'zero-length link'
            return
         end if

         if (link_dy <= tolerance * link_length) then
            horizontal_min = min(horizontal_min, link_dx)
            horizontal_max = max(horizontal_max, link_dx)
            has_horizontal_link = .true.
         else if (link_dx <= tolerance * link_length) then
            vertical_min = min(vertical_min, link_dy)
            vertical_max = max(vertical_max, link_dy)
            has_vertical_link = .true.
         else
            result%offending_link = link
            result%offending_node = node2
            result%max_dev = max(result%max_dev, min(link_dx, link_dy) / link_length)
            result%reason = 'axis alignment'
            return
         end if
      end do

      if (.not. has_horizontal_link .or. .not. has_vertical_link) then
         result%reason = 'lattice must have links in both axis directions'
         return
      end if

      result%dx = horizontal_min
      result%dy = vertical_min
      deviation = max((horizontal_max - horizontal_min) / horizontal_min, (vertical_max - vertical_min) / vertical_min)
      result%max_dev = max(result%max_dev, deviation)
      if (deviation > tolerance) then
         result%reason = 'non-uniform lattice spacing'
         return
      end if

      do i = 1, size(x)
         lattice_coordinate = (x(i) - result%x0) / result%dx
         deviation = abs(lattice_coordinate - anint(lattice_coordinate))
         result%max_dev = max(result%max_dev, deviation)
         if (deviation > tolerance) then
            result%offending_node = i
            result%reason = 'node is off the Cartesian lattice'
            return
         end if

         lattice_coordinate = (y(i) - result%y0) / result%dy
         deviation = abs(lattice_coordinate - anint(lattice_coordinate))
         result%max_dev = max(result%max_dev, deviation)
         if (deviation > tolerance) then
            result%offending_node = i
            result%reason = 'node is off the Cartesian lattice'
            return
         end if
      end do

      result%is_cartesian = .true.
      result%reason = ''
   end subroutine ibm_check_cartesian

end module m_immersed_boundary
