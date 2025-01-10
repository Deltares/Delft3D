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

!
!

!> Fills in the geometry arrays of laterals for history output.
!! In parallel models, only process with rank 0 will have the complete geometry arrays filled.
module m_fill_geometry_arrays_lateral

   implicit none

   private

   public :: fill_geometry_arrays_lateral

contains

   subroutine fill_geometry_arrays_lateral()
      use precision, only: dp
      use m_laterals, only: numlatsg, nodecountlat, geomXLat, geomYLat, nlatnd, n1latsg, n2latsg, nnlat, nNodesLat, model_has_laterals_across_partitions
      use m_alloc
      use m_partitioninfo
      use m_cell_geometry, only: xz, yz

      integer, allocatable :: nodeCountLatGat(:), nlatndGat(:), displs(:)
      real(kind=dp), allocatable :: xGat(:), yGat(:) ! Coordinates that are gatherd data from all subdomains
      integer :: i, j, jprev, k, k1, ierror, is, ie, n, nnode, n1gat, n2gat
      integer :: nlatnd_noghosts, nlatndMPI
      integer, allocatable :: nodeCountLatMPI(:) ! Count of nodes per lateral after mpi communication.
      real(kind=dp), allocatable :: geomXLatMPI(:) ! [m] x coordinates of laterals after mpi communication.
      real(kind=dp), allocatable :: geomYLatMPI(:) ! [m] y coordinates of laterals after mpi communication.

      ! Allocate and construct geometry variable arrays (on one subdomain)
      call realloc(nodeCountLat, numlatsg, keepExisting=.false., fill=0)
      call realloc(geomXLat, nlatnd, keepExisting=.false., fill=0d0)
      call realloc(geomyLat, nlatnd, keepExisting=.false., fill=0d0)

      ! Count number of nodes per lateral in current subdomain, exclude ghosts.
      jprev = 0
      j = 0
      do i = 1, numlatsg
         do k1 = n1latsg(i), n2latsg(i)
            k = nnlat(k1)
            if (k > 0) then
               if (.not. is_ghost_node(k)) then
                  j = j + 1
                  geomXLat(j) = xz(k)
                  geomYLat(j) = yz(k)
               end if
            end if
         end do
         nodeCountLat(i) = j - jprev ! In current domain, for each lateral, number of non-ghost flow nodes.
         jprev = j
      end do
      nlatnd_noghosts = j

      ! Check if any laterals lie across multiple partitions
      ! (needed to disable possibly invalid statistical output items)
      model_has_laterals_across_partitions = any_structures_lie_across_multiple_partitions(nodeCountLat)

      ! For parallel simulation: since only process 0000 writes the history output, the related arrays
      ! are only made on 0000.
      if (jampi > 0) then
         call reduce_int_sum(nlatnd_noghosts, nlatndMPI) ! Get total number of nodes among all subdomains

         if (my_rank == 0) then
            ! Allocate history output arrays
            call realloc(nodeCountLatMPI, numlatsg, keepExisting=.false., fill=0)
            call realloc(geomXLatMPI, nlatndMPI, keepExisting=.false., fill=0d0)
            call realloc(geomyLatMPI, nlatndMPI, keepExisting=.false., fill=0d0)

            ! Allocate arrays that gather information from all subdomains
            ! Data on all subdomains will be gathered in a contiguous way
            call realloc(nodeCountLatGat, numlatsg * ndomains, keepExisting=.false., fill=0)
            call realloc(xGat, nlatndMPI, keepExisting=.false., fill=0d0)
            call realloc(yGat, nlatndMPI, keepExisting=.false., fill=0d0)
            call realloc(displs, ndomains, keepExisting=.false., fill=0)
            call realloc(nlatndGat, ndomains, keepExisting=.false., fill=0)
         else
            ! NOTE: dummy allocate to prevent crash in Debug-model on Intel MPI, even though receive buffers are officially not needed on non-root.
            allocate (nodeCountLatGat(0), xGat(0), yGat(0), displs(0), nlatndgat(0))
         end if

         ! Gather integer data, where the same number of data, i.e. numlatsg, are gathered from each subdomain to process 0000
         call gather_int_data_mpi_same(numlatsg, nodeCountLat, numlatsg * ndomains, nodeCountLatGat, numlatsg, 0, ierror)

         if (my_rank == 0) then
            ! To gather different number of nodes from each subdomain, construct displs, and nlatndGat (used as receive count for mpi_gatherv call)
            displs(1) = 0
            do n = 1, ndomains
               is = (n - 1) * numlatsg + 1 ! Starting index in nodeCountLatGat
               ie = is + numlatsg - 1 ! Ending index in nodeCountLatGat
               nlatndGat(n) = sum(nodeCountLatGat(is:ie)) ! Total number of nodes on subdomain n
               if (n > 1) then
                  displs(n) = displs(n - 1) + nlatndGat(n - 1)
               end if
            end do
         end if

         ! Gather real(kind=dp) data, here, different number of data are gatherd from different subdomains to process 0000
         call gatherv_double_data_mpi_dif(nlatnd_noghosts, geomXLat(1:nlatnd_noghosts), nlatndMPI, xGat, ndomains, nlatndGat, displs, 0, ierror)
         call gatherv_double_data_mpi_dif(nlatnd_noghosts, geomYLat(1:nlatnd_noghosts), nlatndMPI, yGat, ndomains, nlatndGat, displs, 0, ierror)

         if (my_rank == 0) then
            ! Construct nodeCountLatMPI for history output
            ! Construct geomXLatMPI and geomyLatMPI for history output
            j = 0
            do i = 1, numlatsg
               do n = 1, ndomains
                  is = (n - 1) * numlatsg + 1 ! Starting index in nodeCountLatGat
                  k = (n - 1) * numlatsg + i ! Current  index in nodeCountLatGat
                  nnode = nodeCountLatGat(k) ! lateral i on subdomain n has nnode nodes
                  nodeCountLatMPI(i) = nodeCountLatMPI(i) + nnode ! Total number of nodes for lateral i among all subdomains

                  n1gat = displs(n) + sum(nodeCountLatGat(is:k - 1)) + 1 ! starting index in xGat for domain n and lateral i
                  n2gat = n1gat + nnode - 1
                  do k1 = n1gat, n2gat
                     ! No k/ghost check needed here anymore: x/yGat contains only valid points.
                     j = j + 1
                     geomXLatMPI(j) = xGat(k1)
                     geomyLatMPI(j) = yGat(k1)
                  end do
               end do
            end do
            ! Copy the MPI-arrays to nodeCountLat, geomXLat and geomYLat for the his-output
            nNodesLat = nlatndMPI
            nodeCountLat(1:numlatsg) = nodeCountLatMPI(1:numlatsg)
            call realloc(geomXLat, nNodesLat, keepExisting=.false., fill=0d0)
            call realloc(geomyLat, nNodesLat, keepExisting=.false., fill=0d0)
            geomXLat(1:nNodesLat) = geomXLatMPI(1:nNodesLat)
            geomYLat(1:nNodesLat) = geomYLatMPI(1:nNodesLat)
         end if

         nNodesLat = nlatndMPI
      else
         nNodesLat = nlatnd_noghosts ! Might be less than nlatnd, if some laterals lie outside of grid.
         ! In sequential mode, geomXLat, etc. are already correctly filled.
      end if
   end subroutine fill_geometry_arrays_lateral

end module m_fill_geometry_arrays_lateral
