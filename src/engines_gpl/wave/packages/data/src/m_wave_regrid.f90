module m_wave_regrid
   use kdtree2Factory, only: build_kdtree, delete_kdtree2, ITREE_EMPTY, kdtree_instance, &
                            kdtree2_n_nearest, kdtree2_r_count, make_queryvector_kdtree, &
                            realloc_results_kdtree
   use mathconsts, only: degrad_hp
   use m_ec_basic_interpolation, only: TRIINTfast
   use m_ec_triangle, only: indxx, jagetwf, wfxx
   use precision_basics, only: hp
   use stdlib_sorting, only: sort_index

   implicit none(type, external)

   private

   public :: generate_triangle_regrid_weights
   public :: generate_regrid_weights
   public :: quadrilateral_bilinear_weights
   public :: triangle_barycentric_weights

   interface
      subroutine bilin5(xa, ya, x0, y0, weights, error_code)
         use precision_basics, only: hp

         real(kind=hp), dimension(4), intent(in) :: xa
         real(kind=hp), dimension(4), intent(in) :: ya
         real(kind=hp), intent(in) :: x0
         real(kind=hp), intent(in) :: y0
         real(kind=hp), dimension(4), intent(out) :: weights
         integer, intent(out) :: error_code
      end subroutine bilin5

      subroutine wavestop(exit_code, message)
         integer, intent(in) :: exit_code
         character(*), intent(in) :: message
      end subroutine wavestop
   end interface

contains

   !> Generate sparse interpolation weights using quad lookup and Delaunay triangle fallback.
   subroutine generate_regrid_weights(source_x, source_y, quadrilaterals, target_x, target_y, spherical, &
                                      columns, rows, weights, number_of_weights, triangles)
      real(kind=hp), dimension(:), intent(in) :: source_x !< Source point x or longitude coordinates.
      real(kind=hp), dimension(:), intent(in) :: source_y !< Source point y or latitude coordinates.
      integer, dimension(:, :), intent(in) :: quadrilaterals !< Four source indices per quadrilateral.
      real(kind=hp), dimension(:), intent(in) :: target_x !< Target point x or longitude coordinates.
      real(kind=hp), dimension(:), intent(in) :: target_y !< Target point y or latitude coordinates.
      logical, intent(in) :: spherical !< Whether coordinates are longitude and latitude in degrees.
      integer, dimension(:), allocatable, intent(out) :: columns !< Source indices of sparse entries.
      integer, dimension(:), allocatable, intent(out) :: rows !< Target indices of sparse entries.
      real(kind=hp), dimension(:), allocatable, intent(out) :: weights !< Sparse interpolation weights.
      integer, intent(out) :: number_of_weights !< Number of sparse entries.
      integer, dimension(:, :), optional, intent(in) :: triangles !< Three source indices per triangle.

      integer, dimension(:), allocatable :: candidate_columns
      integer, dimension(:), allocatable :: candidate_rows
      real(kind=hp), dimension(:), allocatable :: candidate_weights
      logical, dimension(:), allocatable :: target_is_mapped

      if (size(source_x) /= size(source_y)) then
         call wavestop(1, 'Source coordinate arrays must have equal sizes.')
         return
      end if
      if (size(target_x) /= size(target_y)) then
         call wavestop(1, 'Target coordinate arrays must have equal sizes.')
         return
      end if
      if (size(quadrilaterals, 1) /= 4) then
         call wavestop(1, 'Quadrilateral connectivity must have four rows.')
         return
      end if
      if (present(triangles)) then
         if (size(triangles, 1) /= 3) then
            call wavestop(1, 'Triangle connectivity must have three rows.')
            return
         end if
      end if

      allocate(candidate_columns(4 * size(target_x)))
      allocate(candidate_rows(4 * size(target_x)))
      allocate(candidate_weights(4 * size(target_x)))
      allocate(target_is_mapped(size(target_x)), source=.false.)
      number_of_weights = 0

      call append_quadrilateral_weights(source_x, source_y, quadrilaterals, target_x, target_y, spherical, &
                                        candidate_columns, candidate_rows, candidate_weights, &
                                        number_of_weights, target_is_mapped)
      if (present(triangles)) then
         call append_triangle_weights(source_x, source_y, triangles, target_x, target_y, spherical, &
                                      candidate_columns, candidate_rows, candidate_weights, &
                                      number_of_weights, target_is_mapped)
      else
         call append_delaunay_weights(source_x, source_y, target_x, target_y, spherical, &
                                      candidate_columns, candidate_rows, candidate_weights, &
                                      number_of_weights, target_is_mapped)
      end if

      allocate(columns(number_of_weights), source=candidate_columns(:number_of_weights))
      allocate(rows(number_of_weights), source=candidate_rows(:number_of_weights))
      allocate(weights(number_of_weights), source=candidate_weights(:number_of_weights))
   end subroutine generate_regrid_weights

   subroutine append_quadrilateral_weights(source_x, source_y, quadrilaterals, target_x, target_y, spherical, &
                                           columns, rows, weights, number_of_weights, target_is_mapped)
      real(kind=hp), dimension(:), intent(in) :: source_x
      real(kind=hp), dimension(:), intent(in) :: source_y
      integer, dimension(:, :), intent(in) :: quadrilaterals
      real(kind=hp), dimension(:), intent(in) :: target_x
      real(kind=hp), dimension(:), intent(in) :: target_y
      logical, intent(in) :: spherical
      integer, dimension(:), intent(inout) :: columns
      integer, dimension(:), intent(inout) :: rows
      real(kind=hp), dimension(:), intent(inout) :: weights
      integer, intent(inout) :: number_of_weights
      logical, dimension(:), intent(inout) :: target_is_mapped

      integer :: candidate_index
      integer :: number_of_candidates
      integer :: source_index
      integer :: target_index
      integer :: tree_error
      integer, dimension(4) :: vertices
      real(kind=hp), dimension(size(quadrilaterals, 2)) :: center_x
      real(kind=hp), dimension(size(quadrilaterals, 2)) :: center_y
      real(kind=hp), dimension(4) :: quad_weights
      real(kind=hp) :: search_radius_squared
      logical :: is_inside
      type(kdtree_instance) :: quad_tree

      if (size(quadrilaterals, 2) == 0) then
         return
      end if

      call quadrilateral_centers(source_x, source_y, quadrilaterals, spherical, center_x, center_y)
      search_radius_squared = quadrilateral_search_radius_squared(source_x, source_y, quadrilaterals, &
                                                                  center_x, center_y, spherical)
      call build_kdtree(quad_tree, size(center_x), center_x, center_y, tree_error, merge(1, 0, spherical), &
                        -huge(1.0_hp))
      if (tree_error /= 0) then
         call wavestop(1, 'Unable to build quadrilateral search tree.')
         return
      end if

      do target_index = 1, size(target_x)
         call make_queryvector_kdtree(quad_tree, target_x(target_index), target_y(target_index), &
                                      merge(1, 0, spherical))
         number_of_candidates = kdtree2_r_count(quad_tree%tree, quad_tree%qv, search_radius_squared)
         if (number_of_candidates == 0) then
            cycle
         end if
         call realloc_results_kdtree(quad_tree, number_of_candidates)
         call kdtree2_n_nearest(quad_tree%tree, quad_tree%qv, number_of_candidates, quad_tree%results)

         do candidate_index = 1, number_of_candidates
            vertices = quadrilaterals(:, quad_tree%results(candidate_index)%idx)
            if (any(vertices < 1) .or. any(vertices > size(source_x))) then
               cycle
            end if
            call quadrilateral_bilinear_weights(source_x(vertices), source_y(vertices), &
                                                target_x(target_index), target_y(target_index), &
                                                quad_weights, is_inside, spherical)
            if (.not. is_inside) then
               cycle
            end if
            do source_index = 1, 4
               number_of_weights = number_of_weights + 1
               columns(number_of_weights) = vertices(source_index)
               rows(number_of_weights) = target_index
               weights(number_of_weights) = quad_weights(source_index)
            end do
            target_is_mapped(target_index) = .true.
            exit
         end do
      end do

      if (quad_tree%itreestat /= ITREE_EMPTY) then
         call delete_kdtree2(quad_tree)
      end if
   end subroutine append_quadrilateral_weights

   subroutine append_triangle_weights(source_x, source_y, triangles, target_x, target_y, spherical, &
                                      columns, rows, weights, number_of_weights, target_is_mapped)
      real(kind=hp), dimension(:), intent(in) :: source_x
      real(kind=hp), dimension(:), intent(in) :: source_y
      integer, dimension(:, :), intent(in) :: triangles
      real(kind=hp), dimension(:), intent(in) :: target_x
      real(kind=hp), dimension(:), intent(in) :: target_y
      logical, intent(in) :: spherical
      integer, dimension(:), intent(inout) :: columns
      integer, dimension(:), intent(inout) :: rows
      real(kind=hp), dimension(:), intent(inout) :: weights
      integer, intent(inout) :: number_of_weights
      logical, dimension(:), intent(inout) :: target_is_mapped

      integer :: candidate_index
      integer :: number_of_candidates
      integer :: source_index
      integer :: target_index
      integer :: tree_error
      integer, dimension(3) :: vertices
      real(kind=hp), dimension(size(triangles, 2)) :: center_x
      real(kind=hp), dimension(size(triangles, 2)) :: center_y
      real(kind=hp), dimension(3) :: triangle_weights
      real(kind=hp) :: search_radius_squared
      logical :: is_inside
      type(kdtree_instance) :: triangle_tree

      if (size(triangles, 2) == 0) then
         return
      end if

      call triangle_centers(source_x, source_y, triangles, spherical, center_x, center_y)
      search_radius_squared = triangle_search_radius_squared(source_x, source_y, triangles, &
                                                             center_x, center_y, spherical)
      call build_kdtree(triangle_tree, size(center_x), center_x, center_y, tree_error, &
                        merge(1, 0, spherical), -huge(1.0_hp))
      if (tree_error /= 0) then
         call wavestop(1, 'Unable to build triangle search tree.')
         return
      end if

      do target_index = 1, size(target_x)
         if (target_is_mapped(target_index)) then
            cycle
         end if
         call make_queryvector_kdtree(triangle_tree, target_x(target_index), target_y(target_index), &
                                      merge(1, 0, spherical))
         number_of_candidates = kdtree2_r_count(triangle_tree%tree, triangle_tree%qv, search_radius_squared)
         if (number_of_candidates == 0) then
            cycle
         end if
         call realloc_results_kdtree(triangle_tree, number_of_candidates)
         call kdtree2_n_nearest(triangle_tree%tree, triangle_tree%qv, number_of_candidates, triangle_tree%results)

         do candidate_index = 1, number_of_candidates
            vertices = triangles(:, triangle_tree%results(candidate_index)%idx)
            if (any(vertices < 1) .or. any(vertices > size(source_x))) then
               cycle
            end if
            call triangle_barycentric_weights(source_x(vertices), source_y(vertices), &
                                              target_x(target_index), target_y(target_index), &
                                              triangle_weights, is_inside, spherical)
            if (.not. is_inside) then
               cycle
            end if
            do source_index = 1, 3
               number_of_weights = number_of_weights + 1
               columns(number_of_weights) = vertices(source_index)
               rows(number_of_weights) = target_index
               weights(number_of_weights) = triangle_weights(source_index)
            end do
            target_is_mapped(target_index) = .true.
            exit
         end do
      end do

      if (triangle_tree%itreestat /= ITREE_EMPTY) then
         call delete_kdtree2(triangle_tree)
      end if
   end subroutine append_triangle_weights

   subroutine append_delaunay_weights(source_x, source_y, target_x, target_y, spherical, &
                                      columns, rows, weights, number_of_weights, target_is_mapped)
      real(kind=hp), dimension(:), intent(in) :: source_x
      real(kind=hp), dimension(:), intent(in) :: source_y
      real(kind=hp), dimension(:), intent(in) :: target_x
      real(kind=hp), dimension(:), intent(in) :: target_y
      logical, intent(in) :: spherical
      integer, dimension(:), intent(inout) :: columns
      integer, dimension(:), intent(inout) :: rows
      real(kind=hp), dimension(:), intent(inout) :: weights
      integer, intent(inout) :: number_of_weights
      logical, dimension(:), intent(inout) :: target_is_mapped

      integer :: delaunay_status
      integer :: index
      integer :: source_index
      integer :: target_index
      integer :: use_kdtree
      integer :: previous_jagetwf
      integer, dimension(size(source_x)) :: source_order
      real(kind=hp), dimension(1, size(source_x)) :: source_values
      real(kind=hp), dimension(1, size(target_x)) :: target_values
      real(kind=hp), dimension(1) :: dummy_coordinates
      real(kind=hp), dimension(6) :: transform_coefficients
      real(kind=hp), dimension(3) :: triangle_weights
      real(kind=hp), dimension(size(source_x)) :: sorted_source_x
      real(kind=hp), dimension(size(source_y)) :: sorted_source_y
      logical :: is_inside

      if (size(source_x) < 3) then
         return
      end if

      source_values = 0.0_hp
      target_values = -huge(1.0_hp)
      do target_index = 1, size(target_x)
         if (target_is_mapped(target_index)) then
            target_values(1, target_index) = 0.0_hp
         end if
      end do
      dummy_coordinates = 0.0_hp
      transform_coefficients = -huge(1.0_hp)
      call canonical_coordinate_order(source_x, source_y, source_order)
      sorted_source_x = source_x(source_order)
      sorted_source_y = source_y(source_order)

      previous_jagetwf = jagetwf
      jagetwf = 1
      if (allocated(indxx)) then
         deallocate(indxx)
      end if
      if (allocated(wfxx)) then
         deallocate(wfxx)
      end if
      allocate(indxx(3, size(target_x)), source=0)
      allocate(wfxx(3, size(target_x)), source=0.0_hp)

      delaunay_status = 1
      use_kdtree = 1
      call TRIINTfast(sorted_source_x, sorted_source_y, source_values, size(source_x), 1, &
                target_x, target_y, target_values, &
                      size(target_x), delaunay_status, use_kdtree, merge(1, 0, spherical), 0, 1, &
                      -huge(1.0_hp), merge(1, 0, spherical), dummy_coordinates, dummy_coordinates, &
                      dummy_coordinates, transform_coefficients)

      do target_index = 1, size(target_x)
         if (target_is_mapped(target_index)) then
            cycle
         end if
         if (any(indxx(:, target_index) <= 0)) then
            cycle
         end if
         call triangle_barycentric_weights(sorted_source_x(indxx(:, target_index)), &
                                           sorted_source_y(indxx(:, target_index)), &
                                           target_x(target_index), target_y(target_index), &
                                           triangle_weights, is_inside, spherical)
         if (.not. is_inside) then
            cycle
         end if
         do index = 1, 3
            source_index = source_order(indxx(index, target_index))
            number_of_weights = number_of_weights + 1
            columns(number_of_weights) = source_index
            rows(number_of_weights) = target_index
            weights(number_of_weights) = triangle_weights(index)
         end do
         target_is_mapped(target_index) = .true.
      end do

      deallocate(indxx)
      deallocate(wfxx)
      jagetwf = previous_jagetwf
   end subroutine append_delaunay_weights

   subroutine canonical_coordinate_order(source_x, source_y, source_order)
      real(kind=hp), dimension(:), intent(in) :: source_x
      real(kind=hp), dimension(:), intent(in) :: source_y
      integer, dimension(:), intent(out) :: source_order

      integer, dimension(size(source_x)) :: primary_order
      integer, dimension(size(source_x)) :: secondary_order
      real(kind=hp), dimension(size(source_x)) :: sortable_x
      real(kind=hp), dimension(size(source_y)) :: sortable_y

      sortable_y = source_y
      call sort_index(sortable_y, secondary_order)
      sortable_x = source_x(secondary_order)
      call sort_index(sortable_x, primary_order)
      source_order = secondary_order(primary_order)
   end subroutine canonical_coordinate_order

   subroutine triangle_centers(source_x, source_y, triangles, spherical, center_x, center_y)
      real(kind=hp), dimension(:), intent(in) :: source_x
      real(kind=hp), dimension(:), intent(in) :: source_y
      integer, dimension(:, :), intent(in) :: triangles
      logical, intent(in) :: spherical
      real(kind=hp), dimension(:), intent(out) :: center_x
      real(kind=hp), dimension(:), intent(out) :: center_y

      integer :: element_index
      integer, dimension(3) :: vertices
      real(kind=hp) :: cartesian_x
      real(kind=hp) :: cartesian_y
      real(kind=hp) :: cartesian_z

      do element_index = 1, size(triangles, 2)
         vertices = triangles(:, element_index)
         if (spherical) then
            cartesian_x = sum(cos(source_y(vertices) * degrad_hp) * cos(source_x(vertices) * degrad_hp))
            cartesian_y = sum(cos(source_y(vertices) * degrad_hp) * sin(source_x(vertices) * degrad_hp))
            cartesian_z = sum(sin(source_y(vertices) * degrad_hp))
            center_x(element_index) = atan2(cartesian_y, cartesian_x) / degrad_hp
            center_y(element_index) = atan2(cartesian_z, sqrt(cartesian_x**2 + cartesian_y**2)) / degrad_hp
         else
            center_x(element_index) = sum(source_x(vertices)) / 3.0_hp
            center_y(element_index) = sum(source_y(vertices)) / 3.0_hp
         end if
      end do
   end subroutine triangle_centers

   function triangle_search_radius_squared(source_x, source_y, triangles, center_x, center_y, spherical) &
      result(search_radius_squared)
      use geometry_module, only: dbdistance

      real(kind=hp), dimension(:), intent(in) :: source_x
      real(kind=hp), dimension(:), intent(in) :: source_y
      integer, dimension(:, :), intent(in) :: triangles
      real(kind=hp), dimension(:), intent(in) :: center_x
      real(kind=hp), dimension(:), intent(in) :: center_y
      logical, intent(in) :: spherical
      real(kind=hp) :: search_radius_squared

      integer :: element_index
      integer :: vertex_index
      integer :: vertex
      real(kind=hp) :: distance_squared

      search_radius_squared = 0.0_hp
      do element_index = 1, size(triangles, 2)
         do vertex_index = 1, 3
            vertex = triangles(vertex_index, element_index)
            if (spherical) then
               distance_squared = dbdistance(center_x(element_index), center_y(element_index), &
                                             source_x(vertex), source_y(vertex), 1, 1, -huge(1.0_hp))**2
            else
               distance_squared = (center_x(element_index) - source_x(vertex))**2 &
                                + (center_y(element_index) - source_y(vertex))**2
            end if
            search_radius_squared = max(search_radius_squared, distance_squared)
         end do
      end do
      search_radius_squared = search_radius_squared * (1.0_hp + 64.0_hp * epsilon(1.0_hp))
   end function triangle_search_radius_squared

   subroutine quadrilateral_centers(source_x, source_y, quadrilaterals, spherical, center_x, center_y)
      real(kind=hp), dimension(:), intent(in) :: source_x
      real(kind=hp), dimension(:), intent(in) :: source_y
      integer, dimension(:, :), intent(in) :: quadrilaterals
      logical, intent(in) :: spherical
      real(kind=hp), dimension(:), intent(out) :: center_x
      real(kind=hp), dimension(:), intent(out) :: center_y

      integer :: element_index
      integer, dimension(4) :: vertices
      real(kind=hp) :: cartesian_x
      real(kind=hp) :: cartesian_y
      real(kind=hp) :: cartesian_z

      do element_index = 1, size(quadrilaterals, 2)
         vertices = quadrilaterals(:, element_index)
         if (spherical) then
            cartesian_x = sum(cos(source_y(vertices) * degrad_hp) * cos(source_x(vertices) * degrad_hp))
            cartesian_y = sum(cos(source_y(vertices) * degrad_hp) * sin(source_x(vertices) * degrad_hp))
            cartesian_z = sum(sin(source_y(vertices) * degrad_hp))
            center_x(element_index) = atan2(cartesian_y, cartesian_x) / degrad_hp
            center_y(element_index) = atan2(cartesian_z, sqrt(cartesian_x**2 + cartesian_y**2)) / degrad_hp
         else
            center_x(element_index) = sum(source_x(vertices)) / 4.0_hp
            center_y(element_index) = sum(source_y(vertices)) / 4.0_hp
         end if
      end do
   end subroutine quadrilateral_centers

   function quadrilateral_search_radius_squared(source_x, source_y, quadrilaterals, center_x, center_y, spherical) &
      result(search_radius_squared)
      use geometry_module, only: dbdistance

      real(kind=hp), dimension(:), intent(in) :: source_x
      real(kind=hp), dimension(:), intent(in) :: source_y
      integer, dimension(:, :), intent(in) :: quadrilaterals
      real(kind=hp), dimension(:), intent(in) :: center_x
      real(kind=hp), dimension(:), intent(in) :: center_y
      logical, intent(in) :: spherical
      real(kind=hp) :: search_radius_squared

      integer :: element_index
      integer :: vertex_index
      integer :: vertex
      real(kind=hp) :: distance_squared

      search_radius_squared = 0.0_hp
      do element_index = 1, size(quadrilaterals, 2)
         do vertex_index = 1, 4
            vertex = quadrilaterals(vertex_index, element_index)
            if (spherical) then
               distance_squared = dbdistance(center_x(element_index), center_y(element_index), &
                                             source_x(vertex), source_y(vertex), 1, 1, -huge(1.0_hp))**2
            else
               distance_squared = (center_x(element_index) - source_x(vertex))**2 &
                                + (center_y(element_index) - source_y(vertex))**2
            end if
            search_radius_squared = max(search_radius_squared, distance_squared)
         end do
      end do
      search_radius_squared = search_radius_squared * (1.0_hp + 64.0_hp * epsilon(1.0_hp))
   end function quadrilateral_search_radius_squared

   !> Compute linear interpolation weights for a point in a triangle.
   subroutine triangle_barycentric_weights(vertex_x, vertex_y, target_x, target_y, weights, is_inside, spherical)
      real(kind=hp), dimension(3), intent(in) :: vertex_x !< Triangle x coordinates.
      real(kind=hp), dimension(3), intent(in) :: vertex_y !< Triangle y coordinates.
      real(kind=hp), intent(in) :: target_x !< Target x coordinate.
      real(kind=hp), intent(in) :: target_y !< Target y coordinate.
      real(kind=hp), dimension(3), intent(out) :: weights !< Barycentric weights.
      logical, intent(out) :: is_inside !< Whether the target lies in the triangle.
      logical, optional, intent(in) :: spherical !< Whether coordinates are longitude and latitude in degrees.

      real(kind=hp), dimension(4) :: quadrilateral_weights
      real(kind=hp), dimension(4) :: quadrilateral_x
      real(kind=hp), dimension(4) :: quadrilateral_y
      logical :: spherical_local

      quadrilateral_x = [vertex_x, vertex_x(3)]
      quadrilateral_y = [vertex_y, vertex_y(3)]
      spherical_local = .false.
      if (present(spherical)) then
         spherical_local = spherical
      end if

      call quadrilateral_bilinear_weights(quadrilateral_x, quadrilateral_y, target_x, target_y, &
                                          quadrilateral_weights, is_inside, spherical_local)
      if (.not. is_inside) then
         weights = 0.0_hp
         return
      end if
      weights = [quadrilateral_weights(1), quadrilateral_weights(2), &
                 quadrilateral_weights(3) + quadrilateral_weights(4)]
   end subroutine triangle_barycentric_weights

   !> Compute bilinear interpolation weights for a point in a quadrilateral.
   subroutine quadrilateral_bilinear_weights(vertex_x, vertex_y, target_x, target_y, weights, is_inside, spherical)
      real(kind=hp), dimension(4), intent(in) :: vertex_x !< Quadrilateral x or longitude coordinates.
      real(kind=hp), dimension(4), intent(in) :: vertex_y !< Quadrilateral y or latitude coordinates.
      real(kind=hp), intent(in) :: target_x !< Target x or longitude coordinate.
      real(kind=hp), intent(in) :: target_y !< Target y or latitude coordinate.
      real(kind=hp), dimension(4), intent(out) :: weights !< Bilinear weights.
      logical, intent(out) :: is_inside !< Whether the target lies in the quadrilateral.
      logical, intent(in) :: spherical !< Whether coordinates are longitude and latitude in degrees.

      integer :: error_code
      real(kind=hp), dimension(4) :: interpolation_x
      real(kind=hp), dimension(4) :: interpolation_y
      real(kind=hp) :: tolerance

      weights = 0.0_hp
      if (spherical) then
         call project_to_tangent_plane(vertex_x, vertex_y, target_x, target_y, interpolation_x, interpolation_y)
         call bilin5(interpolation_x, interpolation_y, 0.0_hp, 0.0_hp, weights, error_code)
      else
         call bilin5(vertex_x, vertex_y, target_x, target_y, weights, error_code)
      end if
      if (error_code /= 0) then
         is_inside = .false.
         return
      end if

      tolerance = 64.0_hp * epsilon(1.0_hp)
      is_inside = all(weights >= -tolerance) .and. all(weights <= 1.0_hp + tolerance)
      if (.not. is_inside) then
         weights = 0.0_hp
         return
      end if

      where (abs(weights) <= tolerance)
         weights = 0.0_hp
      elsewhere (abs(weights - 1.0_hp) <= tolerance)
         weights = 1.0_hp
      end where
      weights = weights / sum(weights)
   end subroutine quadrilateral_bilinear_weights

   subroutine project_to_tangent_plane(longitudes, latitudes, target_longitude, target_latitude, projected_x, projected_y)
      real(kind=hp), dimension(:), intent(in) :: longitudes
      real(kind=hp), dimension(:), intent(in) :: latitudes
      real(kind=hp), intent(in) :: target_longitude
      real(kind=hp), intent(in) :: target_latitude
      real(kind=hp), dimension(size(longitudes)), intent(out) :: projected_x
      real(kind=hp), dimension(size(latitudes)), intent(out) :: projected_y

      integer :: index
      real(kind=hp) :: latitude
      real(kind=hp) :: longitude
      real(kind=hp) :: target_latitude_radians
      real(kind=hp) :: target_longitude_radians
      real(kind=hp), dimension(3) :: east
      real(kind=hp), dimension(3) :: north
      real(kind=hp), dimension(3) :: position

      target_longitude_radians = target_longitude * degrad_hp
      target_latitude_radians = target_latitude * degrad_hp
      east = [-sin(target_longitude_radians), cos(target_longitude_radians), 0.0_hp]
      north = [-sin(target_latitude_radians) * cos(target_longitude_radians), &
               -sin(target_latitude_radians) * sin(target_longitude_radians), &
               cos(target_latitude_radians)]

      do index = 1, size(longitudes)
         longitude = longitudes(index) * degrad_hp
         latitude = latitudes(index) * degrad_hp
         position = [cos(latitude) * cos(longitude), cos(latitude) * sin(longitude), sin(latitude)]
         projected_x(index) = dot_product(position, east)
         projected_y(index) = dot_product(position, north)
      end do
   end subroutine project_to_tangent_plane

   !> Build an ESMF-compatible sparse map from triangulated source points to target points.
   subroutine generate_triangle_regrid_weights(source_x, source_y, triangles, target_x, target_y, &
                                               columns, rows, weights, number_of_weights)
      real(kind=hp), dimension(:), intent(in) :: source_x !< Source point x coordinates.
      real(kind=hp), dimension(:), intent(in) :: source_y !< Source point y coordinates.
      integer, dimension(:, :), intent(in) :: triangles !< Three source indices per triangle.
      real(kind=hp), dimension(:), intent(in) :: target_x !< Target point x coordinates.
      real(kind=hp), dimension(:), intent(in) :: target_y !< Target point y coordinates.
      integer, dimension(:), allocatable, intent(out) :: columns !< Source indices of sparse entries.
      integer, dimension(:), allocatable, intent(out) :: rows !< Target indices of sparse entries.
      real(kind=hp), dimension(:), allocatable, intent(out) :: weights !< Sparse interpolation weights.
      integer, intent(out) :: number_of_weights !< Number of sparse entries.

      integer :: source_index
      integer :: target_index
      integer :: triangle_index
      integer, dimension(3) :: vertices
      integer, dimension(:), allocatable :: candidate_columns
      integer, dimension(:), allocatable :: candidate_rows
      real(kind=hp), dimension(3) :: triangle_weights
      real(kind=hp), dimension(:), allocatable :: candidate_weights
      logical :: is_inside

      if (size(source_x) /= size(source_y)) then
         call wavestop(1, 'Source coordinate arrays must have equal sizes.')
         return
      end if
      if (size(target_x) /= size(target_y)) then
         call wavestop(1, 'Target coordinate arrays must have equal sizes.')
         return
      end if
      if (size(triangles, 1) /= 3) then
         call wavestop(1, 'Triangle connectivity must have three rows.')
         return
      end if

      allocate(candidate_columns(3 * size(target_x)))
      allocate(candidate_rows(3 * size(target_x)))
      allocate(candidate_weights(3 * size(target_x)))
      number_of_weights = 0

      do target_index = 1, size(target_x)
         do triangle_index = 1, size(triangles, 2)
            vertices = triangles(:, triangle_index)
            if (any(vertices < 1) .or. any(vertices > size(source_x))) then
               cycle
            end if

            call triangle_barycentric_weights(source_x(vertices), source_y(vertices), &
                                              target_x(target_index), target_y(target_index), &
                                              triangle_weights, is_inside)
            if (.not. is_inside) then
               cycle
            end if

            do source_index = 1, 3
               number_of_weights = number_of_weights + 1
               candidate_columns(number_of_weights) = vertices(source_index)
               candidate_rows(number_of_weights) = target_index
               candidate_weights(number_of_weights) = triangle_weights(source_index)
            end do
            exit
         end do
      end do

      allocate(columns(number_of_weights), source=candidate_columns(:number_of_weights))
      allocate(rows(number_of_weights), source=candidate_rows(:number_of_weights))
      allocate(weights(number_of_weights), source=candidate_weights(:number_of_weights))
   end subroutine generate_triangle_regrid_weights

end module m_wave_regrid