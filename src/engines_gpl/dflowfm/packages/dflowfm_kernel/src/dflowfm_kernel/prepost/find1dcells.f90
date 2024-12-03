!----- AGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2017-2024.
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

module m_find1dcells
   use network_data
   use m_alloc
   use m_flowgeom, only: xz, yz, ba
   use gridoperations
   use MessageHandling
   use m_save_ugrid_state
   use m_inquire_flowgeom
   implicit none

   private

   public find1dcells

contains

!> find one-dimensional net cells
!>    it is assumed that kc has been allocated
!>    it is assumed that findcells has already been called (for 2d cells)
   subroutine find1dcells()
#ifdef _OPENMP
      use omp_lib
#endif
      implicit none

      integer :: k1, k2, k3, L, N, new_cell
      integer :: i, ierr, k, kcell
      integer, dimension(:), allocatable :: left_2D_cells, right_2D_cells
      logical :: Lisnew
      integer :: ierror
      integer :: nump1d, nump1d_i
#ifdef _OPENMP
	  integer :: temp_threads
#endif
      ierror = 1

      allocate (left_2D_cells(NUML1D), right_2D_cells(NUML1D))
#ifdef _OPENMP
      temp_threads = omp_get_max_threads() !> Save old number of threads
      call omp_set_num_threads(OMP_GET_NUM_PROCS()) !> Set number of threads to max for this O(N^2) operation
#endif
      !$OMP PARALLEL DO
      do L = 1, NUML1D
         if (KN(1, L) /= 0 .and. kn(3, L) /= IFLTP_1D .and. kn(3, L) /= 6) then
            call INCELLS(Xk(KN(1, L)), Yk(KN(1, L)), left_2D_cells(L))
            call INCELLS(Xk(KN(2, L)), Yk(KN(2, L)), right_2D_cells(L))
         end if
      end do
      !$OMP END PARALLEL DO
#ifdef _OPENMP
      call omp_set_num_threads(temp_threads)
#endif

!     BEGIN COPY from flow_geominit
      KC = 2 ! ONDERSCHEID 1d EN 2d NETNODES

      do L = 1, NUML
         k1 = KN(1, L); k2 = KN(2, L); k3 = KN(3, L)
         if (k3 >= 1 .and. k3 <= 7) then
            KC(k1) = 1; KC(k2) = 1
         end if
      end do

      ! Create branch node index array and inverse.
      nump1d = size(meshgeom1d%nodebranchidx) !< Old number of nodes contained in meshgeom1d
      if (.not. associated(meshgeom1d%nodeidx)) then ! assume that the nodes were put at the front in order during network reading.
         allocate (meshgeom1d%nodeidx(nump1d))
         meshgeom1d%nodeidx = [(nump1d_i, nump1d_i=1, nump1d)]
      end if
      allocate (meshgeom1d%nodeidx_inverse(size(kc)))
      do i = 1, nump1d
         meshgeom1d%nodeidx_inverse(meshgeom1d%nodeidx(i)) = i
      end do

      nump1d2d = nump !> start from 2D cells
      !> two passes, second one in case branch order cannot be preserved.
      call construct_lne_array(lne, nump1d2d, left_2D_cells, right_2D_cells, preserve_branch_order=.true.)
      call construct_lne_array(lne, nump1d2d, left_2D_cells, right_2D_cells, preserve_branch_order=.false.)

!     fill 1D netcell administration and set cell centers
      call realloc(xzw, nump1d2d)
      call realloc(yzw, nump1d2d)
      call realloc(xz, nump1d2d)
      call realloc(yz, nump1d2d)
      call realloc(ba, nump1d2d, KeepExisting=.true., fill=0d0) ! 1D ba's will be filled halfway through flow_geominit, just allocate and initialize 1D part here
      call increasenetcells(nump1d2d, 1.0, .true.)
      do k = nump + 1, nump1d2d
         netcell(k)%N = 0
         call realloc(netcell(k)%NOD, 1, stat=ierr, keepExisting=.false., fill=0)
         call realloc(netcell(k)%LIN, 1, stat=ierr, keepExisting=.false., fill=0)
      end do

      do k = 1, numk
         if (kc(k) < 0) then ! 1d cell
            new_cell = -kc(k) ! cell number
            N = netcell(new_cell)%N
!           check if this node is new in this cell
            Lisnew = .true.
            do i = 1, N
               if (netcell(new_cell)%nod(i) == k) then
                  Lisnew = .false.
                  exit
               end if
            end do
            if (Lisnew) then ! new node for this cell
               N = N + 1
               if (N > 1) then
                  call realloc(netcell(new_cell)%NOD, N, stat=ierr, keepExisting=.true., fill=0)
                  call realloc(netcell(new_cell)%LIN, N, stat=ierr, keepExisting=.true., fill=0)
               end if
               netcell(new_cell)%N = N
               netcell(new_cell)%nod(N) = k
            end if
         end if
      end do

      do k = 1, numk
         if (kc(k) < 0) then ! 1d cell associated with net node k
            kcell = -kc(k)
            xzw(kcell) = xk(k)
            yzw(kcell) = yk(k)
            xz(kcell) = xk(k)
            yz(kcell) = yk(k)
         end if
      end do

!     safety: 1D-cells can have negative lne, which will cause problems
      if (nump1d2d > nump) then
         netstat = NETSTAT_CELLS_DIRTY
      end if

      ierror = 0
1234  continue

      return
   end subroutine find1dcells

   !> check if the node is touched in the permutation array, has enough links and is type 1 or 6
   logical function is_new_1D_cell(k, l)
      integer, intent(in) :: k !>  netnode number
      integer, intent(in) :: l !>  netlink number

      is_new_1D_cell = .false.

      if (KC(k) == 1) then !Node not yet touched
         if (NMK(k) > 1 .or. (kn(3, l) == IFLTP_1D .or. kn(3, l) == 6)) then
            is_new_1D_cell = .true.
         end if
      end if
   end function is_new_1D_cell

   !> Check for a new cell and set the global LNE array that administrates link-cell connectivity.
   !  Will skip any cell that doesn't match the branch order + chainage sequence.
   subroutine set_lne(LNE, NC, K, L, i_lne, nump1d2d, preserve_branch_order)
      use precision_basics, only: comparereal
      integer, dimension(:, :), intent(inout) :: LNE !< link-cell connectivity array, shape (2,numlinks). left and right cell that a netlink connects.
      integer, intent(in) :: NC !< 2D cell number
      integer, intent(in) :: K !< new node number
      integer, intent(in) :: L !< index in LNE array to set
      integer, intent(in) :: i_lne !< index specifying if the left cell (1) or right cell (2) in LNE array is to be set
      integer, intent(inout) :: nump1d2d !< 1D netnode counter (starts at nump)
      logical, intent(in) :: preserve_branch_order !< flag specifying whether branch order must be preserved
      logical :: branches_first
      integer :: next_found_node, next_branch_node

      if (NC == 0) then
         branches_first = .true.
         if (preserve_branch_order) then
            ! if the branch order is to be preserved, check if the next found node matches the next node in the branchorder.
            next_found_node = meshgeom1d%nodeidx_inverse(k)
            next_branch_node = nump1d2d - nump + 1
            if (next_found_node /= 0 .and. next_branch_node <= size(meshgeom1d%nodebranchidx)) then
               if (meshgeom1d%nodebranchidx(next_found_node) == meshgeom1d%nodebranchidx(next_branch_node) .and. &
                   comparereal(meshgeom1d%nodeoffsets(next_found_node), meshgeom1d%nodeoffsets(next_branch_node), 1d-6) == 0) then
                  branches_first = .true.
               else
                  branches_first = .false.
               end if
            end if
         end if
         if (is_new_1D_cell(K, l) .and. branches_first) then ! NIEUWE 1d CELL
            nump1d2d = nump1d2d + 1
            KC(K) = -nump1d2d ! MARKEREN ALS OUD
            LNE(i_lne, L) = -abs(KC(K)) ! NEW 1D CELL flag 1D links through negative lne ref
            LNN(L) = LNN(L) + 1
         else if (KC(K) /= 1) then
            LNE(i_lne, L) = -abs(KC(K)) ! NEW 1D CELL flag 1D links through negative lne ref
            LNN(L) = LNN(L) + 1
         end if
      else
         LNE(i_lne, L) = NC ! ALREADY EXISTING 2D CELL
         LNN(L) = LNN(L) + 1
      end if

   end subroutine set_lne

   !> if the link is not type 1 or 6, checks the previously filled cell_array for the 2D cell value
   integer pure function get_2D_cell(L, K, cell_array)
      integer, intent(in) :: L !< current link
      integer, dimension(:), intent(in) :: cell_array !< array of previously found 2D cells.
      integer, intent(in) :: K !< node (attached to link)

      get_2D_cell = 0
      if (kn(3, L) /= IFLTP_1D .and. kn(3, L) /= 6) then !These link types are allowed to have no 2D cells
         if (NMK(K) == 1) then
            get_2D_cell = cell_array(L)
         end if
      end if

   end function get_2D_cell

   !> Loop through all netlinks and fill the link-cell connectivity array.
   subroutine construct_lne_array(lne, nump1d2d, left_2D_cells, right_2D_cells, preserve_branch_order)
      integer, dimension(:, :), intent(inout) :: lne !< link-cell connectivity array, shape (2,numlinks). left and right cell that a netlink connects.
      integer, intent(out) :: nump1d2d !> total number of nodes, 2D nodes (nump) + 1D nodes (nump1d)
      integer, dimension(:), intent(in) :: left_2D_cells !< 2D-cell numbers belonging to the left side of the lne array. 0 if not a 2D cell.
      integer, dimension(:), intent(in) :: right_2D_cells !< 2D-cell numbers belonging to the right side of the lne array. 0 if not a 2D cell.
      logical, intent(in) :: preserve_branch_order !< whether the 1D nodes need to be found in the branch order or not.

      integer :: L, k1, k2, left_cell, right_cell

      do L = 1, NUML1D
         k1 = KN(1, L); k2 = KN(2, L)
         if (k1 == 0) cycle
         left_cell = get_2D_cell(L, k1, left_2D_cells)
         right_cell = get_2D_cell(L, k2, right_2D_cells)
         if (left_cell /= 0 .and. left_cell == right_cell) then !Both net nodes inside 2D cell, but assume that the first is then the 1D net node.
            left_cell = 0
         end if
         LNN(L) = 0
         call set_lne(lne, left_cell, k1, L, 1, nump1d2d, preserve_branch_order)
         call set_lne(lne, right_cell, k2, L, 2, nump1d2d, preserve_branch_order)
      end do

   end subroutine construct_lne_array

end module
