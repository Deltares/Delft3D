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

submodule(m_partition_METIS_to_idomain) m_partition_METIS_to_idomain_

   implicit none

contains

!> generate partition numbering with METIS
!!   1D links are supported now
   module subroutine partition_METIS_to_idomain(Nparts, jacontiguous, method, iseed)
      use network_data, only: netcell, nump1d2d, numk, numl, lne, lnn
      use m_partitioninfo, only: idomain
      use m_metis, only: opts, metis_ok
      use m_alloc, only: realloc, aerr
      use MessageHandling, only: LEVEL_ERROR, LEVEL_INFO, LEVEL_WARN, mess
      use m_qnerror, only: qnerror

      integer, intent(in) :: Nparts !< number of partitions
      integer, intent(in) :: method !< partition method. 1: K-Way, 2: Recursive, 3: Mesh-dual
      integer, intent(in) :: jacontiguous !< enforce contiguous domains (1) or not (0)
      integer, intent(in) :: iseed !< User defined random seed, passed to METIS'option "SEED". Useful for reproducible partitionings, but only used when /= 0.

      integer :: ierror

      integer :: Ne ! number of elements
      integer :: Nn ! number of nodes
      integer, allocatable, dimension(:) :: eptr, eind ! mesh
      integer, allocatable, dimension(:) :: vwgt ! vertex weights, dim(Ne)
      integer, allocatable, dimension(:) :: vsize ! communication volume, dim(Ne)
      integer :: ncommon ! number of common nodes between elements
      real, allocatable, dimension(:) :: tpwgts ! target weight of partitions, dim(Nparts)
      integer :: objval ! edgecut or total communication volume
      integer, allocatable, dimension(:) :: npart ! node    partition number, dim(Nn)
      integer :: ic, k, N, ipoint, icursize
      integer, allocatable, dimension(:) :: ncon ! number of balancing constrains, at least 1
      real, allocatable, dimension(:) :: ubvec ! specify the allowed load imbalance tolerance for each constraint.=1.001 when ncon=1
      integer :: L, k1, k2

      integer, allocatable, dimension(:) :: xadj_tmp
      integer, allocatable, dimension(:) :: xadj, adjncy, adjw

      integer, external :: METIS_PartGraphKway, METIS_PartGraphRecursive, METIS_PARTMESHDUAL

      ierror = 1

!     check validity of objected number of subdomains
      if (Nparts < 1) then
         call qnerror('partition_METIS_to_idomain: number of subdomains < 1', ' ', ' ')
         goto 1234
      end if

      Ne = nump1d2d

!     deallocate
      if (allocated(idomain)) deallocate (idomain)

!     allocate
      allocate (idomain(Ne), stat=ierror)
      call aerr('idomain(Ne)', ierror, Ne)

#ifdef HAVE_METIS
!     allocate
      allocate (tpwgts(Nparts), stat=ierror)
      call aerr('tpwgts(Nparts)', ierror, Nparts)
      if (method == 3) then
         Nn = numk
         allocate (eptr(nump1d2d + 1), eind(4 * max(Ne, Nn)), vwgt(max(Ne, Nn)), vsize(max(Ne, Nn)), npart(max(Ne, Nn)), stat=ierror)
         call aerr('eptr(...)', ierror, nump1d2d + 1 + 7 * max(Ne, Nn))
      else
         allocate (vwgt(Ne), vsize(Ne), npart(Ne), ncon(1), ubvec(1), stat=ierror)
         call aerr('vwgt(...)', ierror, 2 + 3 * Ne)
      end if

!     set default options
      call METIS_SetDefaultOptions(opts)

      if (jacontiguous == 1) then
         if (method == 1 .or. method == 0) then ! K-way (method = 1) is the default (method = 0) now
            ierror = metisopts(opts, "CONTIG", 1) ! enforce contiguous domains, observation: number of cells per domain becomes less homogeneous
            if (ierror /= 0) goto 1234
         else if (method == 2) then
            call mess(LEVEL_WARN, 'Contiguous option is not available for Recursive Bisection method (method = 2). To enforce contiguous option, use K-way partitioning (default) method (method = 1).')
         end if
      end if
      ierror = metisopts(opts, "DBGLVL", 1) ! output
      if (ierror /= 0) goto 1234

      ierror = metisopts(opts, "UFACTOR", 1) ! allowed load imbalance TODO, MJ: should be an integer x, and tolerance is (1+x)/1000 according to manual, but 1+x/1000 according to us and "macros.h"
      if (ierror /= 0) goto 1234

      ierror = metisopts(opts, "NITER", 100) ! observation: increasing this number will visually improve the partitioning
      if (ierror /= 0) goto 1234

      if (iseed /= 0) then
         ierror = metisopts(opts, "SEED", iseed) ! User-defined seed value, for reproducible partitionings.
         if (ierror /= 0) goto 1234
      end if

      vwgt = 1 ! weights of vertices
      vsize = 1 ! size of vertices for computing the total communication volume
      tpwgts = 1d0 / dble(Nparts) ! desired weight for each partition

!!     make mesh
      if (method == 3) then
         ncommon = 2 !  number of nodes shared by two cells on each side of an edge
         ipoint = 1
         icursize = size(eind)
         do ic = 1, nump1d2d
            eptr(ic) = ipoint
            N = netcell(ic)%N
            do k = 1, N
!!              reallocate if necessary
               if (ipoint > icursize) then
                  icursize = int(1.2d0 * ipoint) + 1
                  call realloc(eind, icursize, keepExisting=.true.)
               end if
               eind(ipoint) = netcell(ic)%nod(k)
               ipoint = ipoint + 1
            end do
         end do
         eptr(nump1d2d + 1) = ipoint
!
!!       make mesh arrays zero-based
         eptr = eptr - 1
         eind = eind - 1
      else

         ncon = 1 ! number of balancing constraints
         ubvec = 1.001 ! allowed load imbalance tolerance

!        generate adjacency structure in CSR format
         allocate (xadj(nump1d2d + 1), stat=ierror)
         call aerr('xadj(nump1d2d+1)', ierror, nump1d2d + 1)
         xadj = 0
         allocate (xadj_tmp(nump1d2d + 1), stat=ierror)
         call aerr('xadj_tmp(nump1d2d+1)', ierror, nump1d2d + 1)

!        count number of connection per vertex
         xadj_tmp = 0
         do L = 1, numL
            if (lnn(L) > 1) then
               k1 = abs(lne(1, L))
               k2 = abs(lne(2, L))
               xadj_tmp(k1) = xadj_tmp(k1) + 1
               xadj_tmp(k2) = xadj_tmp(k2) + 1
            end if
         end do

!        set startpointers
         xadj(1) = 1
         do k = 1, nump1d2d
            xadj(k + 1) = xadj(k) + xadj_tmp(k)
         end do

!        set connections
         allocate (adjncy(xadj(nump1d2d + 1) - 1), stat=ierror)
         call aerr('adjncy(xadj(nump1d2d+1)-1)', ierror, xadj(nump1d2d + 1) - 1)
         adjncy = 0

         xadj_tmp = xadj
         do L = 1, numL
            if (lnn(L) > 1) then
               k1 = abs(lne(1, L))
               k2 = abs(lne(2, L))
               adjncy(xadj_tmp(k1)) = k2
               adjncy(xadj_tmp(k2)) = k1
               xadj_tmp(k1) = xadj_tmp(k1) + 1
               xadj_tmp(k2) = xadj_tmp(k2) + 1
            end if
         end do

         allocate (adjw(xadj(nump1d2d + 1) - 1), stat=ierror)
         call aerr('jadjw(xadj(nump1d2d+1)-1)', ierror, xadj(nump1d2d + 1) - 1)
         call set_edge_weights_and_vsize_for_METIS(nump1d2d, Nparts, xadj(nump1d2d + 1) - 1, xadj, adjncy, vsize, adjw)

!        make CSR arrays zero-based
         xadj = xadj - 1
         adjncy = adjncy - 1
      end if

      select case (method)
      case (0, 1)
         ierror = METIS_PartGraphKway(Ne, Ncon, xadj, adjncy, vwgt, vsize, adjw, Nparts, tpwgts, ubvec, opts, objval, idomain)
         if (ierror /= METIS_OK .and. jacontiguous == 1) then
            call mess(LEVEL_INFO, 'The above METIS error message is not a problem.')
            call mess(LEVEL_INFO, 'It means that partitioning failed for k-way method with option contiguous=1')
            call mess(LEVEL_INFO, 'because the input graph is not contiguous. Retrying partitioning now with')
            call mess(LEVEL_INFO, 'contiguous=0.')
            ierror = metisopts(opts, "CONTIG", 0) ! Fallback, allow non-contiguous domains in case of non-contiguous network.
            if (ierror == 0) then ! Note: metisopts does not use METIS_OK status, but simply 0 instead.
               ierror = METIS_PartGraphKway(Ne, Ncon, xadj, adjncy, vwgt, vsize, adjw, Nparts, tpwgts, ubvec, opts, objval, idomain)
            else
               call mess(LEVEL_ERROR, 'Fallback fails.')
            end if
         end if
      case (2)
         ierror = METIS_PartGraphRecursive(Ne, Ncon, xadj, adjncy, vwgt, vsize, adjw, Nparts, tpwgts, ubvec, opts, objval, idomain)
      case (3)
         ierror = METIS_PARTMESHDUAL(Ne, Nn, eptr, eind, vwgt, vsize, ncommon, Nparts, tpwgts, opts, objval, idomain, npart)
      case default
         call mess(LEVEL_ERROR, 'Unknown partitioning method number', method)
      end select

      if (ierror /= METIS_OK) then
         call mess(LEVEL_ERROR, 'Metis returns with error code: ', ierror)
      end if

#else
      idomain = 0
      call mess(LEVEL_ERROR, 'This version was built without the METIS mesh partitioner support, ' &
                //'so the option of partitioning a mesh is not available.')
#endif

1234  continue

   end subroutine partition_METIS_to_idomain

!>  set METIS options, returns error (1) or no error (0)
   integer function metisopts(opts, optionname, optionval)

      integer, intent(inout) :: opts(*) !< options array
      character(len=*), intent(in) :: optionname !< option name
      integer, intent(in) :: optionval !< option value
#ifdef HAVE_METIS
      integer :: i

      integer, external :: metisoptions

      i = metisoptions(opts, trim(optionname)//char(0), optionval)

      metisopts = i
#else
      metisopts = 1
#endif
   end function metisopts

!> Sets weights of edges and vsize on mesh that is to be partitioned by METIS
!! If there are structures defined by polylines, then for structure related cells and edges, it gives special values to edges weights and vertex size.
!! The purpose is to avoid structures intercross partition boundaries including ghost cells.
!! NOTE: It uses "Ne/Nparts" as the special weights on structures, othere weight values can also be investigated.
!! Now ONLY support structures defined by polylines. TODO: support setting special weights on structures that are defined by other ways.
   subroutine set_edge_weights_and_vsize_for_METIS(Ne, Nparts, njadj, xadj, adjncy, vsize, adjw)
      use m_find_netcells_for_structures, only: find_netcells_for_structures

      integer, intent(in) :: Ne !< Number of vertices
      integer, intent(in) :: Nparts !< Number of partition subdomains
      integer, intent(in) :: njadj !< Length of array adjncy
      integer, dimension(Ne), intent(in) :: xadj !< The adjacency structure of the graph as described in Section 5.5. of METIS manual
      integer, dimension(njadj), intent(in) :: adjncy !< The adjacency structure of the graph as described in Section 5.5. of METIS manual
      integer, dimension(njadj), intent(inout) :: adjw !< Edge weight used to minimize edge cut (see METIS manual)
      integer, dimension(Ne), intent(inout) :: vsize !< Vertex size used to minimize total communication volume (see METIS manual)

      integer :: number_of_vertices_related_to_structures
      integer, dimension(Ne) :: list_of_vertices_related_to_structures
      integer :: vertex_index, vertex, higher_weight
      integer, parameter :: DEFAULT_WEIGHT_VALUE = 1
      integer, parameter :: INITIAL_HALO_LEVEL = 0

      adjw(:) = DEFAULT_WEIGHT_VALUE
      vsize(:) = DEFAULT_WEIGHT_VALUE

      call find_netcells_for_structures(Ne, number_of_vertices_related_to_structures, list_of_vertices_related_to_structures)

      if (number_of_vertices_related_to_structures > 0) then
         higher_weight = int(Ne / Nparts)
         do vertex_index = 1, number_of_vertices_related_to_structures
            vertex = list_of_vertices_related_to_structures(vertex_index)
            call set_edge_weights_and_vsize_with_halo(INITIAL_HALO_LEVEL, vertex, higher_weight, DEFAULT_WEIGHT_VALUE, &
                                                      Ne, xadj, njadj, adjncy, adjw, vsize)
         end do
      end if
   end subroutine set_edge_weights_and_vsize_for_METIS

!> set edge weight and vsize for vertex and associated edges with halo around structures
   recursive subroutine set_edge_weights_and_vsize_with_halo(halo_level, vertex, higher_weight, default_weight_value, &
                                                             size_xadj, xadj, size_jadj, adjncy, adjw, vsize)
      integer, intent(in) :: halo_level !< halo_level around the structures
      integer, intent(in) :: vertex !< vertex of the graph
      integer, intent(in) :: higher_weight !< higher_weight to be assigned to vsize and adjw around structures
      integer, intent(in) :: default_weight_value !< default_weight_value
      integer, intent(in) :: size_xadj !< size of xadj array
      integer, dimension(size_xadj), intent(in) :: xadj !< starting points for adjacency list, the adjacency structure of the graph is described in Section 5.5. of METIS manual
      integer, intent(in) :: size_jadj !< size of adjacency list array
      integer, dimension(size_jadj), intent(in) :: adjncy !< adjacency list, the adjacency structure of the graph is described in Section 5.5. of METIS manual
      integer, dimension(size_jadj), intent(inout) :: adjw !< Edge weight used to minimize edge cut (see METIS manual)
      integer, dimension(size_xadj), intent(inout) :: vsize !< Vertex size used to minimize total communication volume (see METIS manual)

      integer, parameter :: MAX_HALO_LEVEL = 6 ! perhaps, it is too strong, some experiments are needed on MAX_HALO_LEVEL and MAX_GHOST_LEVEL
      integer, parameter :: MAX_GHOST_LEVEL = 4 ! maxghostlev_sall is not defined yet at this stage
      integer :: edge, next_halo_level, next_halo_level_higher_weight

      if (halo_level <= MAX_HALO_LEVEL .and. higher_weight > default_weight_value) then
         next_halo_level = halo_level + 1
         if (halo_level > MAX_GHOST_LEVEL) then
            next_halo_level_higher_weight = higher_weight / 2 ! an attemp to smooth constraints
         else
            next_halo_level_higher_weight = higher_weight
         end if

         if (vsize(vertex) < higher_weight) then
            vsize(vertex) = higher_weight
         end if
         do edge = xadj(vertex), xadj(vertex + 1) - 1
            if (adjw(edge) < higher_weight) then
               adjw(edge) = higher_weight
            end if
            call set_edge_weights_and_vsize_with_halo(next_halo_level, adjncy(edge), next_halo_level_higher_weight, &
                                                      default_weight_value, size_xadj, xadj, size_jadj, adjncy, adjw, vsize)
         end do
      end if
   end subroutine set_edge_weights_and_vsize_with_halo

end submodule m_partition_METIS_to_idomain_
