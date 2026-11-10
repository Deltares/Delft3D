!> @brief Head routine for writing SWAN nesting boundary information
!>
!> @details Wrapper subroutine that calls write_bnd to write SWAN nesting boundary
!>          coordinates and write_swan_grid to output the grid definition.
!>
!> @param[in] inest SWAN nested grid number
!> @param[in] sg    Actual SWAN grid structure containing coordinates and mask data
subroutine write_bnd_loc (inest,sg)
!
! Head routine for calling write_bnd
!
use swan_flow_grid_maps
use read_grids
implicit none
   type (grid) :: sg       ! actual swan grid
   integer     :: inest    ! swan nested grid no.
   call write_bnd(sg%x        ,sg%y       ,sg%kcs     ,sg%xymiss  ,sg%mmax   ,sg%nmax   , &
                & inest      )
   call write_swan_grid (sg%x,sg%y,sg%mmax,sg%nmax,inest,sg%tmp_name)
end subroutine write_bnd_loc


!> @brief Write SWAN nesting boundary coordinates to file
!>
!> @details This subroutine writes SWAN nesting boundary information to a file for nested
!>          grids (inest > 1). It validates the active grid topology, traces the boundary
!>          of the active region, and writes offset coordinates to prevent SWAN from
!>          interpreting corner points as belonging to multiple nesting segments.
!>
!> @param[in] xc     X-coordinates of grid points (mc x nc)
!> @param[in] yc     Y-coordinates of grid points (mc x nc)
!> @param[in] kcs    Cell status mask array (mc x nc)
!> @param[in] xymiss Missing value indicator for coordinates
!> @param[in] mc     Number of grid points in m-direction
!> @param[in] nc     Number of grid points in n-direction
!> @param[in] inest  SWAN nested grid number
subroutine write_bnd(xc        ,yc        ,kcs       ,xymiss    ,mc        ,nc        , &
                & inest      )
!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2026.                                
!                                                                               
!  This program is free software: you can redistribute it and/or modify         
!  it under the terms of the GNU General Public License as published by         
!  the Free Software Foundation version 3.                                      
!                                                                               
!  This program is distributed in the hope that it will be useful,              
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                
!  GNU General Public License for more details.                                 
!                                                                               
!  You should have received a copy of the GNU General Public License            
!  along with this program.  If not, see <http://www.gnu.org/licenses/>.        
!                                                                               
!  contact: delft3d.support@deltares.nl                                         
!  Stichting Deltares                                                           
!  P.O. Box 177                                                                 
!  2600 MH Delft, The Netherlands                                               
!                                                                               
!  All indications and logos of, and references to, "Delft3D" and "Deltares"    
!  are registered trademarks of Stichting Deltares, and remain the property of  
!  Stichting Deltares. All rights reserved.                                     
!                                                                               
!-------------------------------------------------------------------------------
!  
!  
!!--description-----------------------------------------------------------------
!
! This subroutine writes SWAN nesting boundary information to a file for nested
! grids (inest > 1). It performs the following operations:
!
! 1. Validates the active grid topology to ensure it meets SWAN requirements:
!    - Checks for presence of active points
!    - Verifies active regions are not disconnected
!    - Ensures no unsupported one-cell-wide topologies exist
!
! 2. Traces the boundary of the active grid region by:
!    - Finding a suitable starting point on the grid boundary
!    - Following the outline of active cells in a counter-clockwise direction
!    - Building an ordered list of boundary grid points
!
! 3. Writes boundary coordinates to file SWANIN_NGRIDXXX where XXX is the nest
!    number. Each boundary point is written with a small offset applied along
!    the incoming edge direction to prevent SWAN from interpreting corner points
!    as belonging to multiple nesting segments when BOUN NEST is CLOSED.
!
! The routine handles various edge cases including grids with inactive outer
! rings, missing coordinate values, and complex boundary geometries.
!
!!--pseudo code and references--------------------------------------------------
!
! - IF nested grid (inest > 1):
!   - CALL validate_active_topology to check grid validity
!   - IF topology invalid THEN
!     - CALL wavestop with appropriate error message
!     - RETURN
!   - ENDIF
!   - Allocate boundary point arrays
!   - CALL trace_active_boundary to extract ordered boundary points
!   - IF insufficient boundary points THEN
!     - CALL wavestop
!   - ENDIF
!   - Open output file SWANIN_NGRIDXXX
!   - FOR each boundary point:
!     - CALL write_bnd_point with current, previous, and next point data
!       to write coordinates with appropriate offset
!   - ENDFOR
!   - Close output file
!   - Deallocate boundary point arrays
! - ENDIF
!
!!--declarations----------------------------------------------------------------
    use precision_basics
    !
    implicit none
!
! Global variables
!
    integer                   , intent(in)  :: inest
    integer                   , intent(in)  :: mc
    integer                   , intent(in)  :: nc
    integer                   , dimension(mc, nc), intent(in) :: kcs
    real(kind=hp), dimension(mc, nc), intent(in) :: xc
    real(kind=hp), dimension(mc, nc), intent(in) :: yc
    real(kind=hp)   , intent(in)            :: xymiss
!
! Local variables
!
    integer           :: current_point
    integer           :: lunbot
    integer           :: next_point
    integer           :: num_boundary_points
    integer           :: previous_point
    integer           :: topology_status
    integer, dimension(:), allocatable :: boundary_m
    integer, dimension(:), allocatable :: boundary_n
    character(37)     :: fname
    integer, parameter :: TOPOLOGY_VALID = 0
    integer, parameter :: TOPOLOGY_NO_ACTIVE_POINTS = 1
    integer, parameter :: TOPOLOGY_DISCONNECTED_REGIONS = 2
    integer, parameter :: TOPOLOGY_UNSUPPORTED_ONE_CELL = 3
    integer, parameter :: ORIENTATION_RIGHT = 1
    integer, parameter :: ORIENTATION_UP = 2
    integer, parameter :: ORIENTATION_LEFT = 3
    integer, parameter :: ORIENTATION_DOWN = 4
    real(kind=hp), parameter :: TOL = 1.0e-6_hp
    ! Move each location by a small fraction of its incoming edge. This makes
    ! every child boundary point belong to one nesting segment only; SWAN can
    ! otherwise see a point at the end of both adjacent segments when
    ! BOUN NEST is CLOSED (most visibly at corners and local edge kinks).
    real(kind=hp), parameter :: BOUNDARY_OFFSET_FRACTION = 0.001_hp
!
!! executable statements -------------------------------------------------------
!
    if (inest>1) then
       call validate_active_topology(topology_status)
       select case (topology_status)
       case (TOPOLOGY_NO_ACTIVE_POINTS)
          call wavestop(1, '*** ERROR: SWAN nesting grid has no active points')
          return
       case (TOPOLOGY_DISCONNECTED_REGIONS)
          call wavestop(1, '*** ERROR: SWAN nesting grid has disconnected active regions')
          return
       case (TOPOLOGY_UNSUPPORTED_ONE_CELL)
          call wavestop(1, '*** ERROR: SWAN nesting grid has unsupported one-cell-wide active topology')
          return
       end select

       allocate(boundary_m(mc*nc), boundary_n(mc*nc))
       call trace_active_boundary(boundary_m, boundary_n, num_boundary_points)
       if (num_boundary_points < 3) then
          call wavestop(1, '*** ERROR: Unable to trace active SWAN nesting boundary')
       endif

       fname      = ' '
       fname(1:12) = 'SWANIN_NGRID'
       write (fname(13:15),'(I3.3)') inest
       open (newunit=lunbot, file=fname(1:15))
       do current_point = 1, num_boundary_points
          previous_point = modulo(current_point - 2, num_boundary_points) + 1
          next_point = modulo(current_point, num_boundary_points) + 1
          call write_bnd_point( &
              xc(boundary_m(current_point),boundary_n(current_point)), &
              yc(boundary_m(current_point),boundary_n(current_point)), &
              kcs(boundary_m(current_point),boundary_n(current_point)), &
              xc(boundary_m(previous_point),boundary_n(previous_point)), &
              yc(boundary_m(previous_point),boundary_n(previous_point)), &
              kcs(boundary_m(previous_point),boundary_n(previous_point)), &
              xc(boundary_m(next_point),boundary_n(next_point)), &
              yc(boundary_m(next_point),boundary_n(next_point)), &
              kcs(boundary_m(next_point),boundary_n(next_point)))
       enddo
       close(lunbot)
       deallocate(boundary_m, boundary_n)
    endif
contains
    !> @brief Check if a grid point has valid boundary coordinates
    !>
    !> @details Validates that a point has a positive mask value and coordinates
    !>          that are not equal to the missing value indicator.
    !>
    !> @param[in] x       X-coordinate to validate
    !> @param[in] y       Y-coordinate to validate
    !> @param[in] mask    Cell status mask value
    !> @param[in] missing Missing value indicator
    !> @return .true. if point has valid coordinates, .false. otherwise
    logical function valid_bnd_point(x, y, mask, missing)
        real(kind=hp), intent(in) :: x
        real(kind=hp), intent(in) :: y
        real(kind=hp), intent(in) :: missing
        integer, intent(in) :: mask

        valid_bnd_point = .false.
        if (mask <= 0) then
            return
        end if
        valid_bnd_point = .not. (abs(x - missing) < TOL .and. &
                                abs(y - missing) < TOL)
    end function valid_bnd_point

    !> @brief Check if a grid point is active
    !>
    !> @details Determines if a grid point at indices (m,n) is within grid bounds
    !>          and has valid boundary coordinates.
    !>
    !> @param[in] m M-index of grid point
    !> @param[in] n N-index of grid point
    !> @return .true. if point is active and valid, .false. otherwise
    logical function active_grid_point(m, n)
        integer, intent(in) :: m
        integer, intent(in) :: n

        active_grid_point = .false.
        if (m < 1 .or. m > mc .or. n < 1 .or. n > nc) then
            return
        end if
        active_grid_point = valid_bnd_point(xc(m,n), yc(m,n), kcs(m,n), xymiss)
    end function active_grid_point

    !> @brief Check if a point satisfies SWAN boundary topology requirements
    !>
    !> @details Validates that an active grid point has sufficient active neighbours
    !>          to form a valid SWAN boundary. Rejects isolated points, straight-through
    !>          corridors, and points with missing diagonal neighbours when required.
    !>          This prevents one-cell-wide topologies that SWAN cannot handle.
    !>
    !> @param[in] m M-index of grid point
    !> @param[in] n N-index of grid point
    !> @return .true. if point satisfies SWAN boundary requirements, .false. otherwise
    logical function swan_valid_boundary_point(m, n)
        integer, intent(in) :: m
        integer, intent(in) :: n
        integer :: num_active_neighbours
        logical :: down
        logical :: left
        logical :: lower_left
        logical :: lower_right
        logical :: right
        logical :: up
        logical :: upper_left
        logical :: upper_right

        swan_valid_boundary_point = .false.
        if (.not. active_grid_point(m, n)) then
            return
        end if

        left = active_grid_point(m - 1, n)
        down = active_grid_point(m, n - 1)
        right = active_grid_point(m + 1, n)
        up = active_grid_point(m, n + 1)
        lower_left = active_grid_point(m - 1, n - 1)
        lower_right = active_grid_point(m + 1, n - 1)
        upper_left = active_grid_point(m - 1, n + 1)
        upper_right = active_grid_point(m + 1, n + 1)
        num_active_neighbours = 0
        if (left) then
            num_active_neighbours = num_active_neighbours + 1
        end if
        if (down) then
            num_active_neighbours = num_active_neighbours + 1
        end if
        if (right) then
            num_active_neighbours = num_active_neighbours + 1
        end if
        if (up) then
            num_active_neighbours = num_active_neighbours + 1
        end if

        if (num_active_neighbours <= 1) then
            return
        end if
        if (num_active_neighbours == 2) then
            if ((down .and. up) .or. (left .and. right)) then
                return
            end if
            if (left .and. up .and. .not. upper_left) then
                return
            end if
            if (left .and. down .and. .not. lower_left) then
                return
            end if
            if (right .and. down .and. .not. lower_right) then
                return
            end if
            if (right .and. up .and. .not. upper_right) then
                return
            end if
        endif
        if (num_active_neighbours == 3) then
            if (.not. left .and. .not. lower_right .and. .not. upper_right) then
                return
            end if
            if (.not. right .and. .not. lower_left .and. .not. upper_left) then
                return
            end if
            if (.not. down .and. .not. upper_left .and. .not. upper_right) then
                return
            end if
            if (.not. up .and. .not. lower_left .and. .not. lower_right) then
                return
            end if
        endif
        swan_valid_boundary_point = .true.
    end function swan_valid_boundary_point

    !> @brief Validate the topology of active grid points
    !>
    !> @details Performs comprehensive validation of the active grid region topology:
    !>          - Verifies all active points satisfy SWAN boundary requirements
    !>          - Checks that active points form a single connected region
    !>          - Ensures no isolated or disconnected active regions exist
    !>          Uses breadth-first search to determine connectivity.
    !>
    !> @param[out] status Topology validation status code:
    !>                    TOPOLOGY_VALID (0) - Valid topology
    !>                    TOPOLOGY_NO_ACTIVE_POINTS (1) - No active points found
    !>                    TOPOLOGY_DISCONNECTED_REGIONS (2) - Multiple disconnected regions
    !>                    TOPOLOGY_UNSUPPORTED_ONE_CELL (3) - One-cell-wide topology
    subroutine validate_active_topology(status)
        integer, intent(out) :: status
        integer :: region_count
        integer :: current_m
        integer :: current_n
        integer :: head
        integer :: m
        integer :: n
        integer :: neighbour_m
        integer :: neighbour_n
        integer :: tail
        integer :: topology_direction
        logical :: neighbour_is_valid(ORIENTATION_RIGHT:ORIENTATION_DOWN)
        logical, allocatable :: visited(:,:)
        integer, dimension(:), allocatable :: queue_m
        integer, dimension(:), allocatable :: queue_n

        status = TOPOLOGY_VALID
        region_count = 0
        allocate(visited(mc,nc), queue_m(mc*nc), queue_n(mc*nc))
        visited = .false.

        do m = 1, mc
            do n = 1, nc
                if (.not. active_grid_point(m, n)) then
                    cycle
                end if
                if (.not. swan_valid_boundary_point(m, n)) then
                    status = TOPOLOGY_UNSUPPORTED_ONE_CELL
                    deallocate(visited, queue_m, queue_n)
                    return
                endif
                do topology_direction = ORIENTATION_RIGHT, ORIENTATION_DOWN
                    call neighbouring_point(m, n, topology_direction, &
                                            neighbour_m, neighbour_n)
                    neighbour_is_valid(topology_direction) = &
                        swan_valid_boundary_point(neighbour_m, neighbour_n)
                enddo
                if (.not. any(neighbour_is_valid)) then
                    status = TOPOLOGY_UNSUPPORTED_ONE_CELL
                    deallocate(visited, queue_m, queue_n)
                    return
                endif
            enddo
        enddo

        do m = 1, mc
            do n = 1, nc
                if (.not. active_grid_point(m, n) .or. visited(m,n)) then
                    cycle
                end if
                region_count = region_count + 1
                if (region_count > 1) then
                    status = TOPOLOGY_DISCONNECTED_REGIONS
                    deallocate(visited, queue_m, queue_n)
                    return
                endif
                head = 1
                tail = 1
                queue_m(1) = m
                queue_n(1) = n
                visited(m,n) = .true.
                do while (head <= tail)
                    current_m = queue_m(head)
                    current_n = queue_n(head)
                    head = head + 1
                    do topology_direction = ORIENTATION_RIGHT, ORIENTATION_DOWN
                        call neighbouring_point(current_m, current_n, &
                                                topology_direction, &
                                                neighbour_m, neighbour_n)
                        if (.not. active_grid_point(neighbour_m, neighbour_n)) then
                            cycle
                        end if
                        if (visited(neighbour_m,neighbour_n)) then
                            cycle
                        end if
                        tail = tail + 1
                        queue_m(tail) = neighbour_m
                        queue_n(tail) = neighbour_n
                        visited(neighbour_m,neighbour_n) = .true.
                    enddo
                enddo
            enddo
        enddo

        if (region_count == 0) then
            status = TOPOLOGY_NO_ACTIVE_POINTS
        end if
        deallocate(visited, queue_m, queue_n)
    end subroutine validate_active_topology

    !> @brief Find a suitable starting point for boundary tracing
    !>
    !> @details Searches for an active grid point to begin boundary tracing.
    !>          Prioritizes points on the outer grid edges in the following order:
    !>          bottom edge, right edge, top edge, left edge. If no edge points
    !>          are active, searches for an interior point with an inactive left
    !>          neighbour to support grids with entirely inactive outer rings.
    !>
    !> @param[out] mstart    M-index of starting point
    !> @param[out] nstart    N-index of starting point
    !> @param[out] direction Initial tracing direction (ORIENTATION_*)
    !> @param[out] found     .true. if starting point found, .false. otherwise
    subroutine find_boundary_start(mstart, nstart, direction, found)
        integer, intent(out) :: mstart
        integer, intent(out) :: nstart
        integer, intent(out) :: direction
        logical, intent(out) :: found
        integer :: m
        integer :: n

        found = .true.
        do m = 1, mc
            if (active_grid_point(m, 1)) then
                mstart = m
                nstart = 1
                direction = ORIENTATION_DOWN
                return
            endif
        enddo
        do n = 2, nc
            if (active_grid_point(mc, n)) then
                mstart = mc
                nstart = n
                direction = ORIENTATION_RIGHT
                return
            endif
        enddo
        do m = mc - 1, 1, -1
            if (active_grid_point(m, nc)) then
                mstart = m
                nstart = nc
                direction = ORIENTATION_UP
                return
            endif
        enddo
        do n = nc - 1, 2, -1
            if (active_grid_point(1, n)) then
                mstart = 1
                nstart = n
                direction = ORIENTATION_LEFT
                return
            endif
        enddo

        ! Support grids with an entirely inactive outer ring. SWAN starts an
        ! outline at an active point with an inactive neighbour to its left.
        do m = 1, mc
            do n = 1, nc
                if (active_grid_point(m, n) .and. &
                    .not. active_grid_point(m - 1, n)) then
                    mstart = m
                    nstart = n
                    direction = ORIENTATION_DOWN
                    return
                endif
            enddo
        enddo
        found = .false.
    end subroutine find_boundary_start

    !> @brief Calculate neighbouring point indices in a given direction
    !>
    !> @details Computes the grid indices of the neighbouring point in the specified
    !>          direction (RIGHT, UP, LEFT, or DOWN) from the current point.
    !>
    !> @param[in]  m         M-index of current point
    !> @param[in]  n         N-index of current point
    !> @param[in]  direction Direction to neighbour (ORIENTATION_RIGHT/UP/LEFT/DOWN)
    !> @param[out] mnext     M-index of neighbouring point
    !> @param[out] nnext     N-index of neighbouring point
    subroutine neighbouring_point(m, n, direction, mnext, nnext)
        integer, intent(in) :: m
        integer, intent(in) :: n
        integer, intent(in) :: direction
        integer, intent(out) :: mnext
        integer, intent(out) :: nnext

        mnext = m
        nnext = n
        select case (direction)
        case (ORIENTATION_RIGHT)
            mnext = m + 1
        case (ORIENTATION_UP)
            nnext = n + 1
        case (ORIENTATION_LEFT)
            mnext = m - 1
        case (ORIENTATION_DOWN)
            nnext = n - 1
        end select
    end subroutine neighbouring_point

    !> @brief Trace the outline of the active grid region
    !>
    !> @details Follows the boundary of active cells in a counter-clockwise direction,
    !>          building an ordered list of boundary grid points. Uses a wall-following
    !>          algorithm that turns left when possible, continuing straight or turning
    !>          right as needed. Tracks visited points to detect closure and prevent
    !>          infinite loops.
    !>
    !> @param[out] boundary_m M-indices of boundary points
    !> @param[out] boundary_n N-indices of boundary points
    !> @param[out] num_points Number of boundary points traced (0 if failed)
    subroutine trace_active_boundary(boundary_m, boundary_n, num_points)
        integer, dimension(:), intent(out) :: boundary_m
        integer, dimension(:), intent(out) :: boundary_n
        integer, intent(out) :: num_points
        integer :: current_m
        integer :: current_n
        integer :: direction
        integer :: mnext
        integer :: nnext
        integer :: start_m
        integer :: start_n
        integer :: turn
        logical :: closed
        logical :: found
        logical, allocatable :: visited(:,:)

        call find_boundary_start(start_m, start_n, direction, found)
        if (.not. found) then
            num_points = 0
            return
        endif

        allocate(visited(mc,nc))
        visited = .false.
        num_points = 1
        boundary_m(1) = start_m
        boundary_n(1) = start_n
        visited(start_m,start_n) = .true.
        current_m = start_m
        current_n = start_n
        closed = .false.

        do while (.not. closed)
            found = .false.
            do turn = ORIENTATION_RIGHT, ORIENTATION_DOWN
                call neighbouring_point(current_m, current_n, direction, mnext, nnext)
                if (active_grid_point(mnext, nnext)) then
                    if (mnext == start_m .and. nnext == start_n) then
                        closed = .true.
                    else if (visited(mnext,nnext)) then
                        num_points = 0
                    else
                        num_points = num_points + 1
                        if (num_points > size(boundary_m)) then
                            num_points = 0
                        else
                            boundary_m(num_points) = mnext
                            boundary_n(num_points) = nnext
                            visited(mnext,nnext) = .true.
                            current_m = mnext
                            current_n = nnext
                            direction = direction - 1
                            if (direction < ORIENTATION_RIGHT) then
                                direction = ORIENTATION_DOWN
                            end if
                        endif
                    endif
                    found = .true.
                    exit
                endif
                direction = direction + 1
                if (direction > ORIENTATION_DOWN) then
                    direction = ORIENTATION_RIGHT
                end if
            enddo
            if (.not. found .or. num_points == 0) then
                exit
            end if
        enddo

        if (.not. closed) then
            num_points = 0
        end if
        deallocate(visited)
    end subroutine trace_active_boundary

    !> @brief Write a single boundary point with offset to output file
    !>
    !> @details Writes boundary point coordinates with a small offset applied along
    !>          the incoming edge direction. This prevents SWAN from interpreting corner
    !>          points as belonging to multiple nesting segments when BOUN NEST is CLOSED.
    !>          If the previous point is invalid, uses the next point to compute offset.
    !>
    !> @param[in] x             X-coordinate of current point
    !> @param[in] y             Y-coordinate of current point
    !> @param[in] mask          Cell status mask of current point
    !> @param[in] xprevious     X-coordinate of previous point
    !> @param[in] yprevious     Y-coordinate of previous point
    !> @param[in] previous_mask Cell status mask of previous point
    !> @param[in] xnext         X-coordinate of next point
    !> @param[in] ynext         Y-coordinate of next point
    !> @param[in] next_mask     Cell status mask of next point
    subroutine write_bnd_point(x, y, mask, xprevious, yprevious, previous_mask, &
                               xnext, ynext, next_mask)
        real(kind=hp), intent(in) :: x
        real(kind=hp), intent(in) :: y
        integer, intent(in) :: mask
        real(kind=hp), intent(in)    :: xprevious
        real(kind=hp), intent(in)    :: yprevious
        integer, intent(in) :: previous_mask
        real(kind=hp), intent(in)    :: xnext
        real(kind=hp), intent(in)    :: ynext
        integer, intent(in) :: next_mask
        real(kind=hp) :: xout
        real(kind=hp) :: yout

        if (.not. valid_bnd_point(x, y, mask, xymiss)) then
            return
        end if

        xout = x
        yout = y
        ! Coordinates at inactive points are undefined. Use the other local
        ! edge when the predecessor is invalid, so the point remains offset
        ! without using undefined coordinates.
        if (valid_bnd_point(xprevious, yprevious, previous_mask, xymiss)) then
            xout = xout + BOUNDARY_OFFSET_FRACTION * (xout - xprevious)
            yout = yout + BOUNDARY_OFFSET_FRACTION * (yout - yprevious)
        else if (valid_bnd_point(xnext, ynext, next_mask, xymiss)) then
            xout = xout + BOUNDARY_OFFSET_FRACTION * (xnext - xout)
            yout = yout + BOUNDARY_OFFSET_FRACTION * (ynext - yout)
        endif
        write(lunbot,'(2(F15.6,3X))') xout, yout
    end subroutine write_bnd_point
end subroutine write_bnd
