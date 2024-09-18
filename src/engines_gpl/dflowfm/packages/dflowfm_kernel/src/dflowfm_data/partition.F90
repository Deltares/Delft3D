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

! $Id: partition.F90 142612 2023-03-01 18:35:31Z markelov $
! : https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20230301_UNST_4401_neumann/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_data/partition.F90 $

!------------------------------------------------------------------------
!  THOUGHTS:
!    Given that
!      -domain and ghostlevel numbering is in network administration, and
!      -handshaking is in flow administration,
!    the parallelization seems to rely on an identity mapping of internal
!    flownodes to netcells.
!    Note that the boundary flownodes are always in the own subdomain.
!
!------------------------------------------------------------------------

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

!> @file partition.f90
!! Data and parameter modules for partitioning & parallelizing D-Flow FM models.
!!
!! Includes FM's own partition data as well as some handling of METIS and PETSc.

!> Interface module to METIS's native error codes.
module m_metis
! the parameters and enumerators are taken from
!   ../third_party_open/metis-<version>/include/metis.h
   integer, parameter :: METIS_NOPTIONS = 40

   integer, dimension(METIS_NOPTIONS) :: opts

   !> FORTRAN interface to native METIS return codes
   integer, parameter :: METIS_OK = 1 !< Returned normally
   integer, parameter :: METIS_ERROR_INPUT = -2 !< Returned due to erroneous inputs and/or options
   integer, parameter :: METIS_ERROR_MEMORY = -3 !< Returned due to insufficient memory
   integer, parameter :: METIS_ERROR = -4 !< Some other errors
end module

!> Administration data for D-Flow FM's partitions and MPI-communication patterns.
module m_partitioninfo

   use m_tpoly
   use precision_basics, only: hp
   use meshdata, only: ug_idsLen, ug_idsLongNamesLen
   use gridoperations, only: dlinkangle
   use m_qnerror
   use m_delpol

#ifdef HAVE_MPI
   use mpi, only: NAMECLASH_MPI_COMM_WORLD => MPI_COMM_WORLD ! Apparently PETSc causes a name clash, see commit #28532.
#endif

   implicit none

#ifdef HAVE_MPI
   !> The MPI communicator for dflowfm. Default: MPI_COMM_WORLD.
   !! May be changed from outside in a coupled/library run, *before* initialize().
   !! In case of C/C++ calling side, construct an MPI communicator, and call
   !! MPI_Fint MPI_Comm_c2f(MPI_Comm comm) to convert the C comm handle
   !! to a FORTRAN comm handle.
   integer, target :: DFM_COMM_DFMWORLD = NAMECLASH_MPI_COMM_WORLD !< [-] The MPI communicator for dflowfm (FORTRAN handle). {"rank": 0}
   integer :: DFM_COMM_ALLWORLD !< [-] The MPI communicator for dflowfm including the fetch proc (FORTRAN handle). {"rank": 0}#endif
#endif
   type tghost
      integer, allocatable :: list(:) !< list of ghost nodes or links, in order of their corresponding other domain, dim(1:number of ghost nodes/links)
      integer, allocatable :: N(:) !< cumulative number of ghost nodes/links per domain in the list, starts with fictitious domain 0, dim(0:numdomains)
      integer, allocatable :: neighdmn(:) !< list of neighboring domains, dim(1:numneighdmn)
      integer :: num !< number of ghost nodes or links
      integer :: numdomains !< number of domains
      integer :: numneighdmn !< number of neighboring domains
   end type

   integer :: ndomains = 0 !< number of domains
   integer :: numranks = 1 !< number of ranks
   integer :: my_rank !< own rank

   integer :: use_fetch_proc = 0 !< if 1, then a separate proc is dedicated to calculate fetch parlength and depth
   integer :: fetch_proc_rank = -1 !< the rank of the fetch proc. it is the last proc of the entire proc group when it is active

   character(len=4) :: sdmn !< domain number string

   type(tpoly), allocatable :: partition_pol(:) !< polygons that define the partition
   integer :: npartition_pol !< number of partition polygons

   integer, allocatable :: idomain(:) !< cell-based domain number, dim(nump1d2d or ndx)
   integer, allocatable :: idomain0(:) !< backup of idomain
   integer, allocatable :: numndx(:) !< number of cells in a domain
   integer, allocatable, target :: ighostlev(:) !< ghost-cell level, combination of node-based and cell-based
   integer, allocatable, target :: ighostlev_nodebased(:) !< ghost-cell level, neighboring cells connected through netnodes
   integer, allocatable, target :: ighostlev_cellbased(:) !< ghost-cell level, neighboring cells connected through netlinks

   integer, parameter :: ITYPE_S = 0 !< water-level communication identifier, for Poisson equation (water level), first ghost level only
   integer, parameter :: ITYPE_U = 1 !< flow link communication identifier
   integer, parameter :: ITYPE_Sall = 2 !< water-level communcation identifier, all ghost levels
   integer, parameter :: ITYPE_S3D = 3 !< 3D water-level communication identifier, for Poisson equation (water level), first ghost level only
   integer, parameter :: ITYPE_U3D = 4 !< 3D flow link communication identifier
   integer, parameter :: ITYPE_Sall3D = 5 !< 3D water-level communcation identifier, all ghost levels
   integer, parameter :: ITYPE_SallTheta = 6 !< water-level communcation identifier, all ghost levels, theta-grid (for XBeach waves)
   integer, parameter :: ITYPE_Snonoverlap = 7 !< non-overlappling ghost nodes (for solver)
   integer, parameter :: ITYPE_U3DW = 8 !< 3D flow link communication identifier, starting at 0, for interfaces
   integer, parameter :: ITYPE_CN = 9 !< corners communication identificator

   integer, parameter :: IGHOSTTYPE_CELLBASED = 0 !< cell-based ghostlevels
   integer, parameter :: IGHOSTTYPE_NODEBASED = 1 !< node-based ghostlevels
   integer, parameter :: IGHOSTTYPE_COMBINED = 2 !< combined ghostlevels

!  the non-parameter variables that are initialized with 0 are set in "partition_setghost_params"
   integer :: numlay_cellbased = 0 !< number of cell-based ghost-cell layers
   integer :: numlay_nodebased = 0 !< number of node-based ghost-cell layers

   integer :: minghostlev_s = 0 !< minimum ghost-cell layer level of water-level nodes, used for overlap in solver
   integer :: maxghostlev_s = 0 !< maximum ghost-cell layer level of water-level nodes, used for overlap in solver
   integer :: ighosttype_s = IGHOSTTYPE_CELLBASED

   integer :: minghostlev_u = 0 !< minimum ghost-cell layer level of links
   integer :: maxghostlev_u = 0 !< maximum ghost-cell layer level of links
   integer, parameter :: IGHOSTTYPE_U = IGHOSTTYPE_COMBINED

   integer :: minghostlev_sall = 0 !< minimum ghost-cell layer level of water-level nodes, all ghost levels
   integer :: maxghostlev_sall = 0 !< maximum ghost-cell layer level of water-level nodes, all ghost levels
   integer, parameter :: IGHOSTTYPE_SALL = IGHOSTTYPE_COMBINED

   integer, parameter :: ITAG_S = 1 !< communication tag
   integer, parameter :: ITAG_U = 2 !< communication tag
   integer, parameter :: ITAG_SALL = 3 !< communication tag
   integer, parameter :: ITAG_SNONOVERLAP = 4 !< communication tag
   integer, parameter :: ITAG_CN = 5 !< communication tag

   integer :: numghost_s !< number of water-level ghost nodes
   integer, allocatable, target :: ighostlist_s(:) !< list of water-level ghost nodes, in order of their corresponding domain
   integer, allocatable, target :: nghostlist_s(:) !< pointer to last s-node of a certain ghost domain in the ighostlist_s array, first domain first, etc., includes fictitious domain '0' (0, n1, n1+n2, n1+n1+n3, ...) {"shape": ["-1:numdomains-1"]}

   integer :: numghost_u !< number of ghost links
   integer, allocatable, target :: ighostlist_u(:) !< list of ghost links, in order of their corresponding domain
   integer, allocatable, target :: nghostlist_u(:) !< pointer to last link of a certain ghost domain in the ighostlist_u array, first domain first, etc., includes fictitious domain '0' (0, n1, n1+n2, n1+n1+n3, ...) {"shape": ["-1:numdomains-1"]}

   integer :: numghost_sall !< number of water-level ghost nodes
   integer, allocatable, target :: ighostlist_sall(:) !< list of water-level ghost nodes, in order of their corresponding domain
   integer, allocatable, target :: nghostlist_sall(:) !< pointer to last s-node of a certain ghost domain in the ighostlist_s array, first domain first, etc., includes fictitious domain '0' (0, n1, n1+n2, n1+n1+n3, ...) {"shape": ["-1:numdomains-1"]}

   integer :: numghost_snonoverlap !< number of water-level ghost nodes
   integer, allocatable, target :: ighostlist_snonoverlap(:) !< list of water-level ghost nodes, in order of their corresponding domain
   integer, allocatable, target :: nghostlist_snonoverlap(:) !< pointer to last s-node of a certain ghost domain in the ighostlist_s array, first domain first, etc., includes fictitious domain '0' (0, n1, n1+n2, n1+n1+n3, ...) {"shape": ["-1:numdomains-1"]}

   integer :: numghost_cn !< number of water-level ghost node corners
   integer, allocatable, target :: ighostlist_cn(:) !< list of water-level ghost node corners, in order of their corresponding domain
   integer, allocatable, target :: nghostlist_cn(:) !< pointer to last s-node of a certain ghost domain in the ighostlist_cn array, first domain first, etc. {"shape": ["-1:numdomains-1"]}

   integer :: numsend_s !< number of water-level send nodes
   integer, allocatable, target :: isendlist_s(:) !< list of water-level internal nodes to be sent to other domains
   integer, allocatable, target :: nsendlist_s(:) !< pointer to last s-node of a certain other domain in the isendlist_s array, first domain first, etc., includes fictitious domain '0' (0, n1, n1+n2, n1+n1+n3, ...) {"shape": ["-1:numdomains-1"]}

   integer :: numsend_u !< number of send links
   integer, allocatable, target :: isendlist_u(:) !< list of internal links to be sent to other domains
   integer, allocatable, target :: nsendlist_u(:) !< pointer to last link of a certain other domain in the isendlist_u array, first domain first, etc., includes fictitious domain '0' ('0, n1, n1+n2, n1+n1+n3, ...) {"shape": ["-1:numdomains-1"]}

   integer :: numsend_sall !< number of water level send nodes, all ghostlevels.
   integer, allocatable, target :: isendlist_sall(:) !< list of water level internal nodes to be sent to other domains, all ghostlevels.
   integer, allocatable, target :: nsendlist_sall(:) !< pointer to last s-node of a certain other domain in the isendlist_sall array, first domain first, etc., includes fictitious domain '0' ('0, n1, n1+n2, n1+n1+n3, ...) {"shape": ["-1:numdomains-1"]}

   integer :: numsend_snonoverlap !< number of water level send nodes, all non-overlapping ghostlevels (for solver).
   integer, allocatable, target :: isendlist_snonoverlap(:) !< list of water level internal nodes to be sent to other domains, all non-overlapping ghostlevels (for solver).
   integer, allocatable, target :: nsendlist_snonoverlap(:) !< pointer to last s-node of a certain other domain in the isendlist_snonoverlap array, first domain first, etc., includes fictitious domain '0' ('0, n1, n1+n2, n1+n1+n3, ...) {"shape": ["-1:numdomains-1"]}

   integer :: numsend_cn !< number of water level send node corners, all ghostlevels.
   integer, allocatable, target :: isendlist_cn(:) !< list of water level internal node corners to be sent to other domains, all ghostlevels.
   integer, allocatable, target :: nsendlist_cn(:) !< pointer to last s-node of a certain other domain in the isendlist_cn array, first domain first, etc., {"shape": ["-1:numdomains-1"]}

   integer, allocatable :: iglobal(:) !< [-] unique flow node numbers for matrix solver, does not exactly correspond with the original unpartitioned cell/flow node numbers! (duplicate boundary cells cause some addtional cell number increments.)
   integer, allocatable, target :: iglobal_s(:) !< [-] global flow node numbers to help output aggregation later. Should exactly correspond with the original unpartitioned flow node numbers! (as opposed to iglobal) {"shape": ["ndx"]}
   integer :: nglobal_s !< total number of global net cells, equals unpartitioned nump1d2d, ndxi
   integer, allocatable :: numcells(:) !< number of active cells per domain, dim(0:ndomains-1)

   double precision, allocatable, private :: work(:), workrec(:) !< work array

   double precision, allocatable :: workmatbd(:, :) ! for overlap (solver): matrix (bbr,ddr)
   double precision, allocatable :: workmatc(:, :) ! for overlap (solver): matrix (ccr)

   integer, allocatable :: nghostlist_s_3D(:)
   integer, allocatable :: nsendlist_s_3D(:)

   integer, allocatable :: nghostlist_sall_3D(:)
   integer, allocatable :: nsendlist_sall_3D(:)

   integer, allocatable :: nghostlist_u_3D(:)
   integer, allocatable :: nsendlist_u_3D(:)

   integer, allocatable :: nghostlist_u_3Dw(:)
   integer, allocatable :: nsendlist_u_3Dw(:)

   integer, allocatable :: nghostlist_sall_theta(:)
   integer, allocatable :: nsendlist_sall_theta(:)

   integer :: jaoverlap !< overlap in solver (1) or not (0)

#ifdef HAVE_MPI
   integer :: jampi = 1 !< use MPI (1) or not (0)
   integer :: ja_mpi_init_by_fm !< MPI_Init called by FM (1) or not (0). Not/0 in case FM runs in library mode and an outside runner program has taken care of MPI_Init.
#else
   integer :: jampi = 0 !< use MPI (1) or not (0)
#endif

   double precision, allocatable :: reducebuf(:) !< work array for mpi-reduce
   integer :: nreducebuf !< size of work array 'reducebuf'

!   for test solver:  Schwarz method with Robin-Robin coupling
   integer :: nbndint ! number of interface links
   integer, allocatable :: kbndint(:, :) ! interface administration, similar to kbndz, etc., dim(3,nbndint)
   double precision, allocatable :: zbndint(:, :) ! (1,:): beta value, (2,:): interface value, dim(2,nbndint)
   double precision :: stoptol = 1d-4 ! parameter of stopping criteria for subsolver of Schwarz method
   double precision :: sbeta = 10d0 ! beta value in Robin-Robin coupling for Schwarz iterations
   double precision :: prectol = 0.50d-2 ! tolerance for drop of preconditioner
   integer :: jabicgstab = 1 !
   integer :: Nsubiters = 1000

!  1D global arrays that are stored during partitioning
   character(len=ug_idsLen), private, allocatable :: nodeids_g(:) !< backup for nodeids during partitioning
   character(len=ug_idsLongNamesLen), private, allocatable :: nodelongnames_g(:) !< backup for nodelongnames during partitioning
   real(kind=hp), private, pointer :: nodeoffsets_g(:) !< backup for nodeoffsets during partitioning
   integer, private, pointer :: nodebranchidx_g(:) !< backup for nodebranchidx during partitioning
   integer, private, pointer :: edgebranchidx_g(:) !< backup for edgebranchidx during partitioning
   real(kind=hp), private, pointer :: edgeoffsets_g(:) !< backup for edgeoffsets during partitioning
   logical, private :: branches_partitioned = .false. !< 1D arrays above are in use

   private :: partition_make_1dugrid_in_domain, get_1d_edges_in_domain
   private :: hp, ug_idsLen

contains

!> generate partition numbering from partition polygons and
!! determine number of partitions.
!!
!! A cell/flow node is assigned to the last partition for which it lies
!! inside that partition's polygon.
!!
!! A single partition may be defined by multiple polygons if they have
!! the same z-value in their first polygon point.
!! Therefore ndomains <= num polygons+1
!!
!! For a single partition, polygons may also overlap (partially or fully),
!! to allow 'holes' inside the outer region or even 'islands' inside 'holes'.
   subroutine partition_pol_to_idomain(janet, jafindcells)

      use network_data
      use m_flowgeom, only: Ndx, xz, yz
      use m_missing
      use m_alloc
      use gridoperations
      
      implicit none

      integer, intent(in) :: janet !< for network (1) or flow geom (0) or add 1D net (2)
      integer, intent(in), optional :: jafindcells !< findcells when applicable (1) or not (0)

      integer, allocatable :: idomain_prev(:)
      integer, allocatable :: idum(:)

      double precision :: zval

      integer :: i, idmn, ipol, in
      integer :: ierror, numcells
      integer :: istart, iend

      logical :: Lfindcells

      ierror = 1

!     deallocate module arrays
      if (allocated(numndx)) deallocate (numndx)

      Lfindcells = .true.
      if (present(jafindcells)) then
         Lfindcells = jafindcells == 1
      end if

      if (Lfindcells) then
         if (janet == 1) then
            call findcells(0)
            call find1dcells()
         else if (janet == 2) then ! add 1D net
            call find1dcells()
         end if

         call delete_dry_points_and_areas()
      end if

!     determine number of cells
      if (janet == 1 .or. janet == 2) then
         numcells = nump1d2d ! netcells
         if (janet == 1) then
            istart = 1
         else
            istart = nump + 1
         end if
         iend = numcells
      else
         numcells = Ndx !flownodes
         istart = 1
         iend = numcells
      end if

!     allocate
      if (janet == 0 .or. janet == 1) then
         call realloc(idomain, numcells, keepExisting=.false., fill=0) ! default domain number
      else
         call realloc(idomain, numcells, keepExisting=.true., fill=0)
      end if

      allocate (idomain_prev(numcells))
      idomain_prev = idomain
      allocate (idum(npartition_pol))

!     assign domain numbers to the polygons (if not assigned already)
!       by setting the first node z-value and determine number of partitions

!     make all domain numbers available
      idum = (/(i, i=1, npartition_pol)/)

!     see wich domain numbers are already used
      do ipol = 1, npartition_pol
         if (partition_pol(ipol)%len > 0) then ! non-empty polygons only
!           see if this polygon already has a domain number assigned
            zval = partition_pol(ipol)%z(1)
            if (zval /= DMISS) then
               idmn = int(zval)
               if (idmn < 0 .or. idmn > npartition_pol) then
                  call qnerror('partition_pol_to_idomain: numbering error', ' ', ' ')
               else
                  idum(idmn) = -1 ! deactivated
               end if
            end if
         end if
      end do

!     assign unused (active) domain numbers to remaining polygons and
!       determine number of domains
      ndomains = 1
      do ipol = 1, npartition_pol
         if (partition_pol(ipol)%len > 0) then
            zval = partition_pol(ipol)%z(1)
            if (zval == DMISS) then
               idmn = minval(idum, mask=idum > 0) ! smallest of active domain numbers
               partition_pol(ipol)%z(1) = idmn
               idum(idmn) = -1 ! deactivated
            else
               idmn = int(zval)
            end if
            ndomains = max(ndomains, idmn + 1)
         end if
      end do

!     in future, maybe check for unused domain numbers and shift

!     determine domain number
      do idmn = 1, ndomains
         do ipol = 1, npartition_pol
            if (partition_pol(ipol)%len <= 0) cycle ! non-zero polygons only
            if (int(partition_pol(ipol)%z(1)) /= idmn) cycle ! polygon belonging to this domain only

            in = -1
            do i = istart, iend

               if (i <= nump1d2d) then
                  call dbpinpol_tpoly(partition_pol(ipol), xzw(i), yzw(i), in)
!                 SPvdP: (xzw,yzw) are not safe for spherical, periodic coordinates, use (xz, yz) instead
!                  call dbpinpol_tpoly(partition_pol(ipol), xz(i), yz(i), in)
               else ! fictitious boundary cells: use xz, yz
                  call dbpinpol_tpoly(partition_pol(ipol), xz(i), yz(i), in)
               end if
               if (in == 1) then
!                 check if this cell is already inside the domain
                  if (idomain(i) == idmn) then
!                    outside this domain again
                     idomain(i) = idomain_prev(i)
                  else
!                    inside this domain (again)
                     idomain_prev(i) = idomain(i)
                     idomain(i) = idmn
                  end if

                  !if ( i.eq.115 .and. my_rank.eq.0 ) then
                  !   write(6,*) '-->', i, idomain(i), ipol
                  !end if

               end if

            end do
         end do ! do ipol=1,npartition_pol
      end do ! do idmn=0,ndomains

      ierror = 0
1234  continue

!     deallocate
      if (allocated(idomain_prev)) deallocate (idomain_prev)
      if (allocated(idum)) deallocate (idum)

      return
   end subroutine partition_pol_to_idomain

!> generate partition domain-numbers from selecting polygons
   subroutine generate_partitioning_from_pol()
      use m_polygon
      use network_data

      implicit none

!     copy polygons to partition polygons
      call pol_to_tpoly(npartition_pol, partition_pol)

!     save/delete selecting polygon (partitioning of whole net)
      call savepol()
      call delpol()

!     generate partition domain-numbers from partition polygons
      call partition_pol_to_idomain(1) ! for net

!     restore selecting polygon
      call restorepol()

      return
   end subroutine generate_partitioning_from_pol

!> set ghostlevel parameters
   subroutine partition_setghost_params(icgsolver)
      implicit none

      integer, intent(in) :: icgsolver !< solver type

!     set overlap for Schwarz solver, if uninitialized (0)
      if (icgsolver == 9 .or. icgsolver > 90) then
         numlay_cellbased = 4
         numlay_nodebased = 3
         ighosttype_s = IGHOSTTYPE_NODEBASED

         if (icgsolver > 90) then
            minghostlev_s = icgsolver - 90
            maxghostlev_s = minghostlev_s

!           for Robin-Robin interface conditions
            ighosttype_s = IGHOSTTYPE_NODEBASED
            minghostlev_s = max(minghostlev_s - 1, 1)

!            numlay_cellbased = max(numlay_cellbased,maxghostlev_s)
            numlay_nodebased = max(numlay_nodebased, maxghostlev_s)
         else
            minghostlev_s = numlay_cellbased
            maxghostlev_s = numlay_cellbased
         end if

         minghostlev_sall = 1
         maxghostlev_sall = max(numlay_cellbased, numlay_nodebased) + 1

         minghostlev_u = 1
         maxghostlev_u = max(numlay_cellbased, numlay_nodebased) + 1
      else
         numlay_cellbased = 4
         numlay_nodebased = 3

         minghostlev_s = 1
         maxghostlev_s = 1

         minghostlev_sall = 1
         maxghostlev_sall = 5

         minghostlev_u = 1
         maxghostlev_u = 5
      end if

      return
   end subroutine partition_setghost_params

!> initialize partitioning
   subroutine partition_init_1D2D(md_ident, ierror)
      use m_flowparameters, only: icgsolver
      use m_polygon
      use m_missing
      use m_alloc

      implicit none

      character(len=*), intent(in) :: md_ident
      integer, intent(out) :: ierror

      integer :: idmn
      character(len=128) :: mesg
      ierror = 1

      call partition_setghost_params(icgsolver)

!     the following subroutine will determine the number of domains and generate the domain numbering
      if (npartition_pol > 0) then
         call partition_pol_to_idomain(0)
      end if

      call set_idomain_for_all_open_boundaries()

!     check the number of ranks
      if (ndomains /= numranks .and. jampi == 1) then
         write (mesg, "('partition_init: numdomains = ', I0, ' <> numranks = ', I0)") ndomains, numranks
         call qnerror(trim(mesg), ' ', ' ')
         jampi = 0
         goto 1234
      end if

      if (ndomains <= 1) then ! only one domain
         jampi = 0 ! turn off parallel computing
         ierror = 0
         goto 1234
      end if

      idmn = my_rank

      call partition_set_ghostlevels(idmn, numlay_cellbased + 1, numlay_nodebased + 1, 1, ierror)
      if (ierror /= 0) goto 1234

!     make 2D ghost- and sendlists
      call partition_make_ghostlists(idmn, ierror)
      if (ierror /= 0) goto 1234

      call partition_make_sendlists(idmn, md_ident, ierror)
      if (ierror /= 0) goto 1234

!     flow links: check and fix orientation of send list
      call partition_fixorientation_ghostlist(ierror)
      if (ierror /= 0) goto 1234

!     make non-overlapping ghost- and sendlists (for solver)
      call partition_fill_ghostsendlist_nonoverlap(ierror)
      if (ierror /= 0) goto 1234

      call partition_make_globalnumbers(ierror)
      if (ierror /= 0) goto 1234

      ierror = 0
1234  continue

      return
   end subroutine partition_init_1D2D

   subroutine partition_init_3D(ierror)
      implicit none

      integer, intent(out) :: ierror

!     make 3D ghost- and sendlists
      call partition_make_ghostsendlists_3d(ierror)

      return
   end subroutine partition_init_3D

!> make a domain, including the ghostcells, by removing the other parts of the network
!>   based on combined ghostlevels
   subroutine partition_make_domain(idmn, numlay_cell, numlay_node, jacells, ierror)
      use MessageHandling
      use dfm_error
      use m_polygon
      use network_data
      use m_alloc
      use gridoperations

      implicit none

      integer, intent(in) :: idmn !< domain number
      integer, intent(in) :: numlay_cell !< number of cell-based ghostcell layers
      integer, intent(in) :: numlay_node !< number of node-based ghostcell layers
      integer, intent(in) :: jacells !< generate partition domain and cell numbers
      integer, intent(out) :: ierror !< error code

      integer :: ic1, ic2, k1, k2, L
      integer :: i
      integer, dimension(:, :), allocatable :: lne_org
      integer :: nLink2Dhang ! number of hanging 2D links found
      integer :: i_old
      character(len=128) :: message

      ierror = DFM_GENERICERROR

!     set the ghostcells level in module variables ighostlev, ighostlev_cellbased and ighostlev_nodebased
      call partition_set_ghostlevels(idmn, numlay_cell, numlay_node, 0, ierror)
      if (ierror /= 0) goto 1234

!     delete other part of network, by using the node mask (network_data variable kc)
!      if ( allocated(kc) ) deallocate(kc)
!      allocate(kc(numk))
!      kc = 0

!     make link mask
      Lc = 0
      nLink2Dhang = 0
      do L = 1, numL
!        check for hanging 2D links (not supported)
         if (kn(3, L) == 2 .and. lnn(L) == 0) then
!            call qnerror('Hanging 2D links not supported', ' ', ' ')
!            call teklink(L,NCOLWARN1)
!            ierror=1
!            goto 1234

            Lc(L) = 0 ! deactive link
            nLink2Dhang = nLink2dhang + 1
            cycle
         end if

         ic1 = abs(lne(1, L))
         ic2 = abs(lne(min(lnn(L), 2), L))
         if (kn(3, L) == 2) then ! 2D links
            if (idomain(ic1) == idmn .or. ighostlev(ic1) /= 0 .or. &
                idomain(ic2) == idmn .or. ighostlev(ic2) /= 0) then
               !kc(kn(1,L)) = 1
               !kc(kn(2,L)) = 1
               Lc(L) = 1
            end if
         else if (kn(3, L) /= 0) then ! kn(3,L)==1 .or. kn(3,L)==3 .or. kn(3,L)==4 ) then ! 1D link
!           need to check connected netcells, since the netnode may not be associated with a 1D netcell (1D-2D coupling)
            k1 = kn(1, L)
            k2 = kn(2, L)
            if ((idomain(ic1) == idmn .or. ighostlev(ic1) /= 0) .and. &
                (idomain(ic2) == idmn .or. ighostlev(ic2) /= 0)) then ! active 1D cell
               !kc(k1) = 1
               !kc(k2) = 1
               Lc(L) = 1
            end if
         end if
      end do

!!     deactivate nodes
!      do k=1,numk
!         if ( kc(k).ne.1 ) then
!            call delnode(k)
!         end if
!      end do

!     deactive links
      do L = 1, numL
         if (Lc(L) == 0) then
            kn(1, L) = 0
            kn(2, L) = 0
            kn(3, L) = 0
         end if
      end do

      if (jacells == 1) then
         Nglobal_s = nump1d2d

!        save lne
         allocate (lne_org(2, numL))
         lne_org = 0
         do i = 1, numL
            lne_org(1, i) = abs(lne(1, i))
            lne_org(2, i) = abs(lne(2, i))
         end do
      end if

!     physically remove nodes and links from network
      NPL = 0 ! number of polygon points, we set no polygon
      call findcells(100000) ! output link permutation array "Lperm" (only used if jacells.eq.1)
      call find1dcells()
      netstat = NETSTAT_OK

      call delete_dry_points_and_areas()

      if (numk == 0 .or. numl == 0) then
         write (message, "('While making partition domain #', I0, ': empty domain (', I0, ' net nodes, ', I0, ' net links).')") idmn, numk, numl
         call mess(LEVEL_WARN, trim(message))
         ierror = DFM_GENERICERROR
         goto 1234
      end if

!     output number of hanging 2D links
      if (idmn == 0) then
         if (nLink2Dhang > 0) then
            write (message, "(I0, ' hanging 2D links disregarded.')") nLink2Dhang
            call mess(LEVEL_WARN, trim(message))
         end if
      end if

      if (jacells == 1) then
!        allocate cellmask array (used to flag illegal cells)
         call realloc(cellmask, nump1d2d, keepExisting=.false., fill=0)

!        (re)allocate global cell numbers array
         call realloc(iglobal_s, nump1d2d, keepExisting=.false., fill=0)

!        find original cell numbers
         call find_original_cell_numbers(Lperm, Lne_org, iglobal_s)

         !  generate idomain for the current subdomain so that idomain can be written to the partition file
         do i = 1, nump1d2d
            i_old = iglobal_s(i)

            if (i_old > 0) then
               idomain(i) = idomain0(i_old)
               iglobal_s(i) = i_old
            else
!              cell was created by partitioning, "level-five" cell, mark it for removal
               netcell(i)%N = 0
               idomain(i) = -999 ! not necessary, safety
               iglobal_s(i) = -999 ! not necessary, safety
!              mask illegal cell
               cellmask(i) = 1
            end if
         end do

!        remove masked netcells
         call remove_masked_netcells()

         call partition_make_1dugrid_in_domain(idmn, numl1d, Lperm, ierror)
         if (ierror /= 0) goto 1234
      end if
      ierror = DFM_NOERR
1234  continue

      if (allocated(lne_org)) deallocate (lne_org)

      return
   end subroutine partition_make_domain

   !> partition (only) the 1D mesh part of the 1D UGRID, not the 1D network part.
   !! NOTE: mesh1dNodeIds is not partitioned.
   subroutine partition_make_1dugrid_in_domain(idmn, numl1d, L2Lorg, ierror)
      use m_save_ugrid_state, only: meshgeom1d, nodeids, nodelongnames
      use network_data, only: netcell, netcell0, nump, nump1d2d
      implicit none
      integer, intent(in) :: idmn !< domain number
      integer, intent(in) :: numl1d !< number of 1D links
      integer, intent(in) :: L2Lorg(:) !< Mapping table current (new) to original net link numbers
      integer, intent(out) :: ierror !< (allocation) error code. 0=success

      integer, allocatable :: edge_nodes(:, :)
      integer, allocatable :: iglobal_edge(:) !< Original global number of all current 1D edges (that is: excluding 1d2d)
      integer, allocatable :: kperm(:) !< Permutation table for net nodes (from old to new numbering)
      integer :: i, ii, ic_p, ic_g, i_p, i_g, n1dedges, numk1d
      character(len=ug_idsLen), allocatable :: nodeids_p(:)
      character(len=ug_idsLongNamesLen), allocatable :: nodelongnames_p(:)
      real(kind=hp), pointer :: nodeoffsets_p(:)
      integer, pointer :: nbranchids_p(:)
      integer, pointer :: edgebranchidx_p(:)
      real(kind=hp), pointer :: edgeoffsets_p(:)

      ierror = 0
      if (meshgeom1d%numnode < 0) return

      ! set pointers to keep global arrays, and allow restore (see subroutine restore_1dugrid_state below)
      if (idmn == 0) then
         call move_alloc(nodeids, nodeids_g)
         call move_alloc(nodelongnames, nodelongnames_g)
         nodeoffsets_g => meshgeom1d%nodeoffsets
         nodebranchidx_g => meshgeom1d%nodebranchidx
         edgebranchidx_g => meshgeom1d%edgebranchidx
         edgeoffsets_g => meshgeom1d%edgeoffsets
         branches_partitioned = .true.
      else
         ! clean up arrays from previous domain
         deallocate (nodeids, nodelongnames)
         deallocate (meshgeom1d%nodeoffsets, meshgeom1d%nodebranchidx, meshgeom1d%edgebranchidx, meshgeom1d%edgeoffsets)
      end if

      ! create edge_nodes (Edge-to-node mapping array)
      ! Prepare 1D edges and 1d nodes for UGRID mesh1d writing, and obtain the original global numbers, so that edge/node (branch)ids can be set correctly.
      call get_1d_edges_in_domain(numl1d, L2Lorg, numk1d, n1dedges, edge_nodes, iglobal_edge, kperm, ierror)
      if (ierror /= 0) return

      ! partition node arrays
      allocate (nbranchids_p(numk1d), nodeids_p(numk1d), nodeoffsets_p(numk1d), nodelongnames_p(numk1d), stat=ierror)
      if (ierror /= 0) return
      ! 1D cells have already been sorted in original global order
      ! and 1D net should be written in original order (with other partitions removed, that is: in the current 1:numk1d order)
      do ic_p = nump + 1, nump1d2d
         ! UNST-6571: deliberately not using kperm here, under the assumption that in input 1D netnodes are already sorted by branch,
         !            and this loop should stay consistent with the one in unc_write_net_ugrid2().
         !            This subroutine only deals with edge-ordering and partition-to-global mapping (i.e., no netnode remapping).
         i_p = netcell(ic_p)%nod(1) ! 1D netcell -> 1D net node
         ic_g = iglobal_s(ic_p)
         i_g = netcell0(ic_g)%nod(1) ! netcell0 currently still contains backup of unpartitioned original full grid.
         nodeids_p(i_p) = nodeids_g(i_g)
         nbranchids_p(i_p) = nodebranchidx_g(i_g)
         nodeoffsets_p(i_p) = nodeoffsets_g(i_g)
         nodelongnames_p(i_p) = nodelongnames_g(i_g)
      end do

      ! partition edge arrays
      n1dedges = size(edge_nodes, 2)

      allocate (edgebranchidx_p(n1dedges), edgeoffsets_p(n1dedges), stat=ierror)
      if (ierror /= 0) return
      do i = 1, n1dedges
         ii = iglobal_edge(i)
         edgebranchidx_p(i) = edgebranchidx_g(ii)
         edgeoffsets_p(i) = edgeoffsets_g(ii)
      end do

      ! finally, set pointers and allocatables in meshgeom1d and m_save_ugrid_state
      call move_alloc(nodeids_p, nodeids)
      call move_alloc(nodelongnames_p, nodelongnames)
      meshgeom1d%nodeoffsets => nodeoffsets_p
      meshgeom1d%nodebranchidx => nbranchids_p
      meshgeom1d%edgebranchidx => edgebranchidx_p
      meshgeom1d%edgeoffsets => edgeoffsets_p
   end subroutine partition_make_1dugrid_in_domain

   !> Helper routine to get 1D edge_nodes and original edge number for current domain while partitioning.
   !! The net link and net node numbering is returned in a "1D only" numbering, intended for UGRID mesh1d output.
   !! A 1D edge is a true 1D netlink, i.e., not a 1D2D link.
   subroutine get_1d_edges_in_domain(numl1d, L2Lorg, numk1d, n1dedges, edge_nodes, Lorg, kperm, ierror)
      use network_data, only: kn, LNE
      implicit none
      integer, intent(in) :: numl1d !< number of 1D(+1D2D) links in current partition mesh
      integer, intent(in) :: L2Lorg(:) !< Mapping table current (new) to original global net link numbers
      integer, intent(out) :: numk1d !< number of 1D nodes returned
      integer, intent(out) :: n1dedges !< number of 1D edges returned
      integer, allocatable, intent(out) :: edge_nodes(:, :) !< Edge-to-node mapping array.
      integer, allocatable, intent(out) :: Lorg(:) !< Original edge numbers for current (new) edges (edges can be subset of 1D net links, namely: excluding the 1D2D net links).
      integer, allocatable, intent(out) :: kperm(:) !< Permutation table for net node numbers (from old to new numbers).
      integer, intent(out) :: ierror !< error code

      integer :: k1, k2, l, size

      size = maxval(kn(1:2, 1:numl1d))
      allocate (kperm(size), stat=ierror)
      if (ierror /= 0) return
      kperm(:) = 0

      n1dedges = 0
      do l = 1, numl1d
         if (kn(3, l) == 1 .or. kn(3, l) == 6) then
            n1dedges = n1dedges + 1
         end if
      end do
      allocate (edge_nodes(2, n1dedges), stat=ierror)
      if (ierror /= 0) return
      allocate (Lorg(n1dedges), stat=ierror)
      if (ierror /= 0) return

      n1dedges = 0
      numk1d = 0
      do l = 1, numl1d
         if (kn(3, l) == 1 .or. kn(3, l) == 6) then
            n1dedges = n1dedges + 1

            k1 = kn(1, l)
            k2 = kn(2, l)
            if (kperm(k1) == 0) then
               numk1d = numk1d + 1
               kperm(k1) = numk1d ! remember new node number
            end if
            if (kperm(k2) == 0) then
               numk1d = numk1d + 1
               kperm(k2) = numk1d ! remember new node number
            end if

            edge_nodes(1, n1dedges) = kperm(kn(1, l))
            edge_nodes(2, n1dedges) = kperm(kn(2, l))
            Lorg(n1dedges) = L2Lorg(L) ! Note: this assumes that in original net, all 1D net links come first in the range 1:numl1d, and the 1D2D links are all together at the end of that numl1d range.
         else if (kn(3, l) >= 3 .and. kn(3, l) <= 7) then
            ! We may encounter an orphan 1D2D link, where the 1D side has no own true 1D links.
            ! In that case, only increment the numk1d counter, but don't add any 1d edge.
            if (lne(1, L) < 0) then ! #1 is the 1D side, as set in find1dcells()
               k1 = kn(1, L)
            else if (lne(2, L) < 0) then ! #2 is the 1D side, as set in find1dcells()
               k1 = kn(2, L)
            else
               cycle
            end if
            if (kperm(k1) == 0) then
               numk1d = numk1d + 1
               kperm(k1) = numk1d ! remember new node number
            end if
         end if
      end do
   end subroutine get_1d_edges_in_domain

   !> restore 1D arrays that are partitioned in partition_make_1dugrid_in_domain
   !! also clean up of 1D arrays from the last domain
   subroutine restore_1dugrid_state()
      use m_save_ugrid_state, only: meshgeom1d, nodeids, nodelongnames

      implicit none

      if (branches_partitioned) then
         ! clean up last domain
         deallocate (nodeids, nodelongnames)
         deallocate (meshgeom1d%nodeoffsets, meshgeom1d%nodebranchidx, meshgeom1d%edgebranchidx, meshgeom1d%edgeoffsets)
         ! restore global arrays
         call move_alloc(nodeids_g, nodeids)
         call move_alloc(nodelongnames_g, nodelongnames)
         meshgeom1d%nodeoffsets => nodeoffsets_g
         meshgeom1d%nodebranchidx => nodebranchidx_g
         meshgeom1d%edgebranchidx => edgebranchidx_g
         meshgeom1d%edgeoffsets => edgeoffsets_g
         branches_partitioned = .false.
         nullify (nodeoffsets_g, nodebranchidx_g, edgebranchidx_g, edgeoffsets_g)
      end if
   end subroutine restore_1dugrid_state

!> find original cell numbers for the current subset of cells.
!! Typically used for reconstructing the global cell numbers for all cells in the current partition.
   subroutine find_original_cell_numbers(L2Lorg, Lne_org, iorg)
      use unstruc_messages
      use network_data, only: nump, nump1d2d, numL, lnn, lne, numl1d, netcell, xzw, yzw
      use m_flowgeom, only: xz, yz
      use unstruc_channel_flow, only: network
      use stdlib_sorting, only: sort_index
      implicit none

      integer, intent(in) :: L2Lorg(:) !< Mapping table current (new) to original net link numbers
      integer, intent(in) :: Lne_org(:, :) !< Original Lne netlink-netcell connectivity (of global model)
      integer, intent(out) :: iorg(:) !< Original cell numbers for current (new) cells.

      integer, dimension(:, :), allocatable :: icandidate ! two original cell number candidates, dim(nump1d2d)
      integer, dimension(:, :), allocatable :: tmp_lne
      integer, dimension(:), allocatable :: indx, indxinv, tmpNetcellNod
      real(kind=hp), dimension(:), allocatable :: tmpCoord
      integer :: i, k, kother, LL, L, L_org, nc
      integer :: ic1, ic2, ic3, ic4
      integer :: nump1d2d_org

!     get original number of cells
      nump1d2d_org = 0
      do L = 1, numL
         L_org = L2Lorg(L)
         ic3 = abs(lne_org(1, L_org))
         ic4 = abs(lne_org(2, L_org))
         nump1d2d_org = max(max(nump1d2d_org, ic3), ic4)
      end do

      allocate (icandidate(2, nump1d2d_org))

      icandidate = -1
      do L = 1, numL
         L_org = L2Lorg(L)
         ic3 = abs(lne_org(1, L_org)) ! first new candidate
         ic4 = abs(lne_org(2, L_org)) ! second new candiate, can be 0 if lnn_org==1
         do i = 1, 2
            k = abs(lne(i, L)) ! this cell number
            if (k == 0) cycle
            ic1 = icandidate(1, k) ! first stored candidate
            ic2 = icandidate(2, k) ! second stored candidate

            if (ic1 == -1 .and. ic2 == -1) then ! initialization: accept both new candidates
               icandidate(1, k) = ic3
               icandidate(2, k) = ic4
            else ! compare two new candidates (ic3,ic4) with the two stored ones (ic1,ic2)
               if (ic3 /= ic1 .and. ic4 /= ic1) then ! compare with c1
                  ! it's not ic1
                  icandidate(1, k) = 0
               end if

               if (ic3 /= ic2 .and. ic4 /= ic2) then
                  ! it's not ic2
                  icandidate(2, k) = 0
               end if
            end if
         end do
      end do

!     retrieve original cell numbers, 0 if no cell found
      iorg = 0
      do k = 1, nump1d2d
         if (icandidate(1, k) > 0 .and. icandidate(2, k) > 0 .and. icandidate(1, k) /= icandidate(2, k)) then
!           cell is endnode on a branch
         else if (icandidate(1, k) > 0) then
            iorg(k) = icandidate(1, k)
         else if (icandidate(2, k) > 0) then
            iorg(k) = icandidate(2, k)
         end if
      end do

!     set cells connected through one link/edge
      do L = 1, numL
         do i = 1, 2
            k = abs(lne(i, L)) ! this cell number
            kother = abs(lne(3 - i, L)) ! other cell number

            if (k == 0 .or. kother == 0) cycle

            if (iorg(k) == 0 .and. iorg(kother) > 0) then ! unassigned original cell number
               ic1 = icandidate(1, k)
               ic2 = icandidate(2, k)
               if (ic1 == iorg(kother)) then
                  iorg(k) = ic2
               else if (ic2 == iorg(kother)) then
                  iorg(k) = ic1
               end if
            end if

         end do
      end do

!!     check if all orginal cell number are assigned
!      do k=1,nump1d2d
!         if ( iorg(k).eq.0 ) then
!            continue
!         end if
!      end do

      if (network%loaded .and. numl1d > 0) then
         ! sort the 1D flow nodes (and their coordinates + underlying net nodes),
         ! such that the network principles continue to be satisfied:
         ! * calculation points (flow nodes) are order such that branch indices are always increasing,
         ! * and calculation points (flow nodes) within a branch are always ordered by increasing offset/chainage.

         allocate (indx(nump + 1:nump1d2d), indxinv(nump + 1:nump1d2d), tmpCoord(nump + 1:nump1d2d), tmpNetcellNod(nump + 1:nump1d2d))
         call sort_index(iorg(nump + 1:nump1d2d), indx(nump + 1:nump1d2d))
         do i = nump + 1, nump1d2d
            indx(i) = indx(i) + nump
         end do

         do i = nump + 1, nump1d2d
            indxinv(indx(i)) = i ! Construct helper table with inverse of sorting permutation indx.
         end do

         do i = nump + 1, nump1d2d
            tmpNetcellNod(i) = netcell(i)%nod(1) ! Note: AvD: Can we ever have more than 1?? I don't think so, but find1dcells() has generic code for %N>1...
         end do

         ! 1D netcells reordering only needs re-assignment of the %nod(1) values, no other changed fields.
         do i = nump + 1, nump1d2d
            netcell(i)%nod(1) = tmpNetCellNod(indx(i))
         end do

         ! 1D flow nodes (net cells) have been reordered. Also update xz(w)/yz(w) coords. Not needed:ba and tnod, comes later in flow_geominit().
         tmpCoord(nump + 1:nump1d2d) = xzw(nump + 1:nump1d2d)
         do i = nump + 1, nump1d2d
            xzw(i) = tmpCoord(indx(i))
         end do
         tmpCoord(nump + 1:nump1d2d) = yzw(nump + 1:nump1d2d)
         do i = nump + 1, nump1d2d
            yzw(i) = tmpCoord(indx(i))
         end do
         tmpCoord(nump + 1:nump1d2d) = xz(nump + 1:nump1d2d)
         do i = nump + 1, nump1d2d
            xz(i) = tmpCoord(indx(i))
         end do
         tmpCoord(nump + 1:nump1d2d) = yz(nump + 1:nump1d2d)
         do i = nump + 1, nump1d2d
            yz(i) = tmpCoord(indx(i))
         end do

         ! New (reordered) netcell numbers in LNE array for netlink-netcell connectivity
         allocate (tmp_lne(2, numl1d))
         tmp_lne(1:2, 1:numl1d) = lne(1:2, 1:numl1d)
         do LL = 1, numl1D
            do i = 1, LNN(LL) ! 0/1/2
               nc = tmp_lne(i, LL) !cell number
               if (abs(nc) > nump) then
                  lne(i, LL) = sign(indxinv(abs(nc)), nc) ! Use sorted cell numbers, but keep original sign ('-' denoting 1D cells)
               else
                  continue ! 2d cell numbers remain unchanged
               end if
            end do
         end do

      end if

   end subroutine find_original_cell_numbers

!>  combine cell-based ghostlevels with node-based ghostlevels
!>    note: ghostlevel should never be put to 1 by node-based ghostlevel (otherwise "s"-ghostlist becomes unnecessary large)
!>          therefore use max(ighostlev_nodebased(ic),2) if ighostlev_nodebased(ic).gt.0
!>
!>    update: node-based ghostlevel may now be 1 (did not do it yet), since we have introduced ghostlevel types
!             and "s"-type ghosts are cell-based only
   subroutine partition_set_ghostlevels(idmn, numlay_cell, numlay_node, jaboundary, ierror)
      use network_data, only: nump1d2d
      use unstruc_messages
      implicit none

      integer, intent(in) :: idmn !< domain number
      integer, intent(in) :: numlay_cell !< number of cell-based ghost-cell layers
      integer, intent(in) :: numlay_node !< number of node-based ghost-cell layers
      integer, intent(in) :: jaboundary !< include boundary flownodes (1) or not (0)
      integer, intent(out) :: ierror !< error (1) or not (0)

      integer :: numcells, num, ic

      ierror = 1

!     determine number of cells from idomain
      numcells = size(idomain)

      if (allocated(ighostlev)) deallocate (ighostlev)
      allocate (ighostlev(numcells)) ! make ighostlev same size as idomain

!     compute node-based ghostlevels
      call partition_set_ghostlevels_cellbased(idmn, numlay_cell, ierror)

      if (ierror /= 0) goto 1234

!     compute node-based ghostlevels
      call partition_set_ghostlevels_nodebased(idmn, numlay_node, ierror)
      if (ierror /= 0) goto 1234

      num = 0

      ighostlev = ighostlev_cellbased

      do ic = 1, nump1d2d
!         if ( ighostlev_nodebased(ic).gt.0 .and. ighostlev_nodebased(ic).le.MAX_NODEBASED_GHOSTLEVEL ) then
         if (ighostlev_nodebased(ic) > 0) then
            if (ighostlev_cellbased(ic) == 0) then
               num = num + 1
               ighostlev(ic) = max(ighostlev_nodebased(ic), 2)
            else
               ighostlev(ic) = min(max(ighostlev_nodebased(ic), 2), ighostlev_cellbased(ic))
            end if
         end if
      end do

      call mess(LEVEL_INFO, 'added node-based ghostcells:', num)

!     set ghostlevels in boundaries (if applicable)
      if (jaboundary == 1) then
         call partition_set_ghostlevels_boundaries()
      end if

      ierror = 0
1234  continue

      if (ierror /= 0) then
         call mess(LEVEL_ERROR, 'partition_set_ghostlevels: error')
      end if

      return
   end subroutine partition_set_ghostlevels

!> find the ghost cells for a certain domain, given a cell-based domain numbering idomain
!>    it is assumed that module variable idomain has been filled
!>    ghost cells are masked in module variable ighostlev_cellbased
   subroutine partition_set_ghostlevels_cellbased(idmn, numlay_loc, ierror)
      use network_data
      implicit none

      integer, intent(in) :: idmn !< domain number
      integer, intent(in) :: numlay_loc !< number of ghost-cell layers
      integer, intent(out) :: ierror !< error (1) or not (0)

      integer :: ilay, ic, icother, L, n, numcells

      ierror = 1

!     determine number of cells from idomain
      numcells = size(idomain)

      if (allocated(ighostlev_cellbased)) deallocate (ighostlev_cellbased)
      allocate (ighostlev_cellbased(numcells)) ! make ighostlev_cellbased same size as idomain

      ighostlev_cellbased = 0

      do ilay = 1, numlay_loc ! loop over the number of layers
         do L = 1, numL ! loop over the netlinks
            if (lnn(L) < 2) cycle ! 1d-links: relies on proper lnn (set by find1dcells)
            do n = 1, 2 ! loop over the cells attached to this link
               ic = abs(lne(n, L))
               if (idomain(ic) == idmn .or. (ilay > 1 .and. ighostlev_cellbased(ic) == ilay - 1)) then
!                activate inactive other cell
                  icother = abs(lne(1, L)) + abs(lne(2, L)) - ic
                  if (idomain(icother) /= idmn .and. ighostlev_cellbased(icother) == 0) then ! other cell not active
                     ighostlev_cellbased(icother) = ilay ! set ighostlev_cellbased to the layer number
                  end if
               end if
            end do
         end do ! do L=1,numL
      end do ! do ilay=1,numlay_loc

      ierror = 0
1234  continue

      return
   end subroutine partition_set_ghostlevels_cellbased

!> find the node-based ghost levels (e.g. for horizonal momentum diffusion) for a certain domain, given a cell-based domain numbering idomain
!>    it is assumed that module variable idomain has been filled
!>    ghost cells are masked in module variable ighostlev_nodebased
   subroutine partition_set_ghostlevels_nodebased(idmn, numlay_loc, ierror)
      use network_data
      implicit none

      integer, intent(in) :: idmn !< domain number
      integer, intent(in) :: numlay_loc !< number of ghost-cell layers
      integer, intent(out) :: ierror !< error (1) or not (0)

      integer :: ilay, ic, icother, i, ip1, kk, k, L, Lp1, numcells

      integer, external :: common_cell_for_two_net_links

      ierror = 1

!     determine number of cells from idomain
      numcells = size(idomain)

      if (allocated(ighostlev_nodebased)) deallocate (ighostlev_nodebased)
      allocate (ighostlev_nodebased(numcells)) ! make ighostlev_nodebased same size as idomain

      ighostlev_nodebased = 0

      do ilay = 1, numlay_loc
         do ic = 1, nump
            if (idomain(ic) == idmn .or. (ilay > 1 .and. ighostlev_nodebased(ic) == ilay - 1)) then
               do kk = 1, netcell(ic)%N
                  k = netcell(ic)%nod(kk)
                  do i = 1, nmk(k)
                     ip1 = i + 1; if (ip1 > nmk(k)) ip1 = ip1 - nmk(k)
                     L = nod(k)%lin(i)
                     Lp1 = nod(k)%lin(ip1)
                     icother = common_cell_for_two_net_links(L, Lp1)

                     if (icother == 0) cycle ! boundary links

                     if (idomain(icother) /= idmn .and. ighostlev_nodebased(icother) == 0) then ! other cell not active
                        ighostlev_nodebased(icother) = ilay ! set ighostlev to the layer number
                     end if
                  end do
               end do
            end if
         end do
      end do

      ierror = 0
1234  continue

      return
   end subroutine partition_set_ghostlevels_nodebased

!> set ghostlevels in boundary flownodes (copy from inner nodes)
   subroutine partition_set_ghostlevels_boundaries()
      use network_data
      use m_flowgeom
      implicit none

      integer :: kb, ki, L
      integer :: ierror

      ierror = 1

!      if ( Ndx.gt.nump ) then
      do L = Lnxi + 1, Lnx
         kb = ln(1, L)
         ki = ln(2, L)
         ighostlev_cellbased(kb) = ighostlev_cellbased(ki)
         ighostlev_nodebased(kb) = ighostlev_nodebased(ki)
         ighostlev(kb) = ighostlev(ki)
      end do
!      end if

      ierror = 0
1234  continue

      return
   end subroutine partition_set_ghostlevels_boundaries

!> make the lists of ghost-water-levels and velocities
!>    it is assumed that idomain and ighostlev are filled
   subroutine partition_make_ghostlists(domain_number, error)
      use m_alloc
      use m_flowgeom

      implicit none

      integer, intent(in) :: domain_number
      integer, intent(out) :: error !< error (1) or not (0)

      error = 0

!     get ghost lists for all ghost levels at flownodes, links and corners
      call make_ghost_list(domain_number, ITYPE_S, minghostlev_s, maxghostlev_s, numghost_s, &
                           ighostlist_s, nghostlist_s, error)
      call make_ghost_list(domain_number, ITYPE_Sall, minghostlev_sall, maxghostlev_sall, numghost_sall, &
                           ighostlist_sall, nghostlist_sall, error)
      call make_ghost_list(domain_number, ITYPE_U, minghostlev_u, maxghostlev_u, numghost_u, &
                           ighostlist_u, nghostlist_u, error)
      call make_ghost_list(domain_number, ITYPE_CN, minghostlev_sall, maxghostlev_sall, numghost_cn, &
                           ighostlist_cn, nghostlist_cn, error)

      return
   end subroutine partition_make_ghostlists

!> deallocate partitioning arrays
   recursive subroutine partition_cleanup(iwhat, ierror)
      implicit none

      integer, intent(in) :: iwhat !< 0: all, 1: cell-based only, 2: polygon only, 3: s-ghostlists, 4: u-ghostlists, 5: s-sendlist, 6: u-sendlist, 7: 3D lists
      integer, intent(out) :: ierror !< error (1) or not (0)

      integer :: i, ierror_

      ierror = 0

      if (iwhat == 0) then
         ierror_ = 0
         i = 0
         do while (ierror_ == 0)
            i = i + 1
            call partition_cleanup(i, ierror_)
         end do
      else if (iwhat == 1) then
         if (allocated(idomain)) deallocate (idomain)
         if (allocated(ighostlev)) deallocate (ighostlev)
         if (allocated(ighostlev_cellbased)) deallocate (ighostlev_cellbased)
         if (allocated(ighostlev_nodebased)) deallocate (ighostlev_nodebased)
      else if (iwhat == 2) then
         if (allocated(partition_pol)) then
            call dealloc_tpoly(partition_pol)
            npartition_pol = 0
         end if
      else if (iwhat == 3) then
         if (allocated(ighostlist_s)) deallocate (ighostlist_s)
         if (allocated(nghostlist_s)) deallocate (nghostlist_s)
      else if (iwhat == 4) then
         if (allocated(ighostlist_u)) deallocate (ighostlist_u)
         if (allocated(nghostlist_u)) deallocate (nghostlist_u)
      else if (iwhat == 5) then
         if (allocated(isendlist_s)) deallocate (isendlist_s)
         if (allocated(nsendlist_s)) deallocate (nsendlist_s)
      else if (iwhat == 6) then
         if (allocated(isendlist_u)) deallocate (isendlist_u)
         if (allocated(nsendlist_u)) deallocate (nsendlist_u)
      else if (iwhat == 7) then
         if (allocated(nghostlist_s_3d)) deallocate (nghostlist_s_3d)
         if (allocated(nsendlist_s_3d)) deallocate (nsendlist_s_3d)

         if (allocated(nghostlist_sall_3d)) deallocate (nghostlist_sall_3d)
         if (allocated(nsendlist_sall_3d)) deallocate (nsendlist_sall_3d)

         if (allocated(nghostlist_u_3d)) deallocate (nghostlist_u_3d)
         if (allocated(nsendlist_u_3d)) deallocate (nsendlist_u_3d)

         if (allocated(nghostlist_u_3dw)) deallocate (nghostlist_u_3dw)
         if (allocated(nsendlist_u_3dw)) deallocate (nsendlist_u_3dw)
      else
         ierror = 1
      end if

      if (iwhat == 0 .or. iwhat == 2) then
      end if

      return
   end subroutine partition_cleanup

!> write ghostcell information to file
   subroutine write_ghosts(FNAM)
      use network_data, only: xzw, yzw
      use m_flowgeom, only: xu, yu
      use m_alloc
      use m_missing
      use m_wrildb

      implicit none

      character(len=*), intent(in) :: FNAM

      character(len=64), dimension(:), allocatable :: nampli

      double precision, dimension(:), allocatable :: xpl, ypl

      integer, dimension(:), allocatable :: ipl

      integer :: ndmn, nums, numu, num, numnampli, NPL, MPOL

      integer :: i, j
      integer :: ic, Lf

!     get the number of domains in the water-level ghost list
      ndmn = size(nghostlist_s) - 1
!     get the number of water-level ghost nodes
      nums = nghostlist_s(ndmn - 1)
!     get the number of domains in the velocity ghost list
      ndmn = size(nghostlist_u) - 1
!     get the number of velocity ghost links
      numu = nghostlist_u(ndmn - 1)

      num = nums + numu + 1

      allocate (xpl(num), ypl(num), ipl(num), nampli(1))

!     fill polygon with ghostcell information

      NPL = 0
      numnampli = 0

!     water-level ghost nodes
      numnampli = numnampli + 1
      if (numnampli > size(nampli)) call realloc(nampli, int(1.2d0 * dble(numnampli) + 1d0), keepExisting=.true., fill='')
      nampli(numnampli) = 'water-level'

      do i = 0, ubound(nghostlist_s, 1)
         do j = nghostlist_s(i - 1) + 1, nghostlist_s(i)
            NPL = NPL + 1
            ic = ighostlist_s(j)
            xpl(NPL) = xzw(ic)
            ypl(NPL) = yzw(ic)
            ipl(NPL) = i
         end do
      end do

!     add dmiss
      if (NPL > 0) then
         NPL = NPL + 1
         xpl(NPL) = DMISS
         ypl(NPL) = DMISS
         ipl(NPL) = 0
      end if

!     velocity ghost links
      numnampli = numnampli + 1
      if (numnampli > size(nampli)) call realloc(nampli, int(1.2d0 * dble(numnampli) + 1d0), keepExisting=.true., fill='')
      nampli(numnampli) = 'velocity'

      do i = 0, ubound(nghostlist_u, 1)
         do j = nghostlist_u(i - 1) + 1, nghostlist_u(i)
            NPL = NPL + 1
            Lf = abs(ighostlist_u(j))
            xpl(NPL) = xu(Lf)
            ypl(NPL) = yu(Lf)
            ipl(NPL) = i
         end do
      end do

!     write polygon
      call newfil(MPOL, FNAM)
      call wrildb(MPOL, xpl, ypl, NPL, ipl, NPL, xpl, 0, nampli, 64, numnampli)

!     deallocate
      if (allocated(xpl)) deallocate (xpl)
      if (allocated(ypl)) deallocate (ypl)
      if (allocated(ipl)) deallocate (ipl)
      if (allocated(nampli)) deallocate (nampli)

      return
   end subroutine write_ghosts

!  allocate and initialize tghost-type
   subroutine alloc_tghost(ghost, Nmax, Nmin)
      implicit none

      type(tghost), dimension(:), allocatable, intent(inout) :: ghost !< array of tghost-type
      integer, intent(in) :: Nmax !< objective array upper bound
      integer, optional, intent(in) :: Nmin !< objective array lower bound

      integer :: i, Nmin_

      Nmin_ = 1
      if (present(Nmin)) then
         Nmin_ = Nmin
      end if

      if (allocated(ghost)) call dealloc_tghost(ghost)

      allocate (ghost(Nmin_:Nmax))

      do i = Nmin_, Nmax
         ghost(i)%num = 0
         ghost(i)%numdomains = 0
         ghost(i)%numneighdmn = 0
         allocate (ghost(i)%list(1))
         allocate (ghost(i)%N(-1:0))
         allocate (ghost(i)%neighdmn(1))
         ghost(i)%list = 0
         ghost(i)%N = 0
         ghost(i)%neighdmn = 0
      end do

      return
   end subroutine alloc_tghost

!  deallocate tghost-type
   subroutine dealloc_tghost(ghost)
      implicit none

      type(tghost), dimension(:), allocatable, intent(inout) :: ghost !< array of tghost-type

      integer :: i

      if (.not. allocated(ghost)) return

      do i = lbound(ghost, 1), ubound(ghost, 1)
         if (allocated(ghost(i)%list)) deallocate (ghost(i)%list)
         if (allocated(ghost(i)%N)) deallocate (ghost(i)%N)
         if (allocated(ghost(i)%neighdmn)) deallocate (ghost(i)%neighdmn)
      end do

      deallocate (ghost)

      return
   end subroutine dealloc_tghost

!> make the lists of ghost-water-levels and velocities
!>    it is assumed that idomain and ighostlev are filled
   subroutine partition_get_ghosts(domain_number, itype, ghost_list, ierror)
      use m_alloc

      implicit none

      integer, intent(in) :: domain_number
      integer, intent(in) :: itype !< type: flow node, flow link, flow node corner
      type(tghost), allocatable, intent(out) :: ghost_list(:)
      integer, intent(out) :: ierror !< error (1) or not (0)

      ierror = 1

      if (allocated(ghost_list)) call dealloc_tghost(ghost_list)

      if (itype == ITYPE_Sall) then ! flow node
         call get_ghost_cells(domain_number, minghostlev_sall, maxghostlev_sall, IGHOSTTYPE_SALL, ghost_list)
      else if (itype == ITYPE_S) then
         call get_ghost_cells(domain_number, minghostlev_s, maxghostlev_s, ighosttype_s, ghost_list)
      else if (itype == ITYPE_U) then ! flow link
         call get_ghost_links(domain_number, minghostlev_u, maxghostlev_u, IGHOSTTYPE_U, ghost_list)
      else if (itype == ITYPE_CN) then ! flow node corners
         call get_ghost_corners(domain_number, minghostlev_sall, maxghostlev_sall, IGHOSTTYPE_SALL, ghost_list)
      else ! unknown type
         call qnerror('partition_get_ghosts: unsupported ghost type', ' ', ' ')
      end if

      ierror = 0

      return
   end subroutine partition_get_ghosts

!> determine if flow link is ghost link, flow link domain number and ghost level
!>
!>   a flow link is owned by the adjacent cell with the smallest domain number
!>   thus, a link is a ghost link if:
!>                it connects two ghost cells, or
!> ACTIVATED ->   it connects only one ghost cell, and the other domain number is smaller than the own domain number, or
!                 it connects connects a cell in the own subdomain with ghostlevel >0 (at the boundary)
   subroutine link_ghostdata(idmn, idmnL, idmnR, jaghost, idmn_link, ighostlevL, ighostlevR, iglev)
      use m_flowgeom
      implicit none

      integer, intent(in) :: idmn !< domain number based on which the ghost-checking is done (typically my_rank)
      integer, intent(in) :: idmnL !< domain number of left neighboring cell
      integer, intent(in) :: idmnR !< domain number of right neighboring cell
      integer, intent(out) :: jaghost !< flow link is ghost link (1) or not (0)
      integer, intent(out) :: idmn_link !< flow link domain number
      integer, intent(in), optional :: ighostlevL !< ghost level of left neighboring cell
      integer, intent(in), optional :: ighostlevR !< ghost level of right neighboring cell
      integer, intent(out), optional :: iglev !< flow link ghost level (if ghostlevels specified, otherwise 0)

      jaghost = 0
      idmn_link = idmn
      if (present(iglev)) then
         iglev = 0
      end if

      if ((idmnL /= idmn .and. idmnR /= idmn) .or. &
          (idmnL == idmn .and. idmnR < idmn) .or. &
          (idmnL < idmn .and. idmnR == idmn) &
          ) then
         jaghost = 1
      else if (present(ighostlevL) .and. present(ighostlevR)) then
         if ((idmnL == idmn .and. ighostlevL > 0) .or. &
             (idmnR == idmn .and. ighostlevR > 0) &
             ) then
            jaghost = 1
         end if
      end if

      if (jaghost == 1) then

         if (present(ighostlevL) .and. present(ighostlevR) .and. present(iglev)) then
            idmn_link = min(idmnL, idmnR) ! a choice

!           ghost domain cannot be own domain
            if (idmn_link == idmn) idmn_link = idmnL + idmnR - idmn
            iglev = min(ighostlevL, ighostlevR)

!           ghost level may be zero
            if (iglev == 0) iglev = max(ighostlevL, ighostlevR)
         end if
      end if

      return
   end subroutine link_ghostdata

   !> Tells whether a particular flow node is a ghost node in the current domain.
   !! In sequential models, result is always .false.
   pure function is_ghost_node(k) result(is_ghost)
      integer, intent(in) :: k !< Flow node number
      logical :: is_ghost !< Whether the flow node is a ghost
      is_ghost = .false.
      if (jampi == 1) then
         is_ghost = (my_rank /= idomain(k))
      end if
   end function is_ghost_node

!> fill sendghostlist
   subroutine partition_fill_sendlist(own_domain_number, other_domain_number, itype, N_req, x_req, y_req, ghost_list, &
                                      send_list, nr_send_list, ierror)

      use m_alloc
      use network_data, only: numk, xzw, yzw, xk, yk
      use m_flowgeom, only: xu, yu
      use geometry_module, only: dbdistance
      use m_missing, only: dmiss
      use m_sferic, only: jsferic, jasfer3D

      implicit none

      integer, intent(in) :: own_domain_number
      integer, intent(in) :: other_domain_number
      integer, intent(in) :: itype !< ITYPE_*
      integer, intent(in) :: N_req !< number of flownodes/links/corners requested by other domain
      double precision, intent(in) :: x_req(N_req), y_req(N_req) !< coordinates of requested flownodes/links/corners
      type(tghost), intent(in) :: ghost_list(:)
      integer, allocatable, intent(inout) :: send_list(:) !< send list
      integer, allocatable, intent(inout) :: nr_send_list(:) !< cumulative number of flownodes/links/corners per domain in the list, starts with fictitious domain 0, dim(-1:ndomains-1)
      integer, intent(out) :: ierror !< error (1) or not (0)

      integer, allocatable :: temp_list(:) !< temp list holds a list of flow nodes/links/corners
      double precision, pointer :: x_local(:), y_local(:) !< pointers on flow nodes/links/corners
      integer :: i, ii, j, ghost_level, num, numnew
      integer :: numdomains, idum
      integer :: node

      integer :: jafound
      double precision, parameter :: TOLERANCE = 1d-4
      character(len=80) :: message2, message3

      ierror = 1
      if (own_domain_number < 0) then
         return
      end if

      if (itype == ITYPE_S .or. itype == ITYPE_SALL .or. itype == ITYPE_SNONOVERLAP) then ! flownodes
         x_local => xzw
         y_local => yzw
      else if (itype == ITYPE_U) then ! flowlinks
         x_local => xu
         y_local => yu
      else if (itype == ITYPE_CN) then ! corners
         x_local => xk
         y_local => yk
      else
         call qnerror('partition_fill_sendlist: unknown ghost type', ' ', ' ')
         return
      end if

!     allocate
      allocate (temp_list(N_req))
      temp_list(:) = 0

      num = 0
      do j = 1, N_req
         jafound = 0
         search_loop: &
            do ghost_level = lbound(ghost_list, 1), ubound(ghost_list, 1)
            if (own_domain_number > ghost_list(ghost_level)%numdomains - 1) cycle
            do ii = ghost_list(ghost_level)%N(own_domain_number - 1) + 1, ghost_list(ghost_level)%N(own_domain_number)
               i = ghost_list(ghost_level)%list(ii)
               if (dbdistance(x_req(j), y_req(j), x_local(i), y_local(i), jsferic, jasfer3D, dmiss) < TOLERANCE) then ! found
                  num = num + 1
                  temp_list(num) = i
                  jafound = 1
                  exit search_loop
               end if
            end do
         end do search_loop
!        check if flownode/link/corner was found
         if (jafound == 0) then
            write (message2, *) 'my_rank=', my_rank, ' itype=', itype
            write (message3, *) 'j=', j, ' N_req=', N_req, ' num=', num
            if (itype == ITYPE_CN) then
               do node = 1, numk
                  if (dbdistance(x_req(j), y_req(j), xk(node), yk(node), jsferic, jasfer3D, dmiss) < TOLERANCE) then ! found
                     num = num + 1
                     temp_list(num) = node
                     jafound = 1
                     exit
                  end if
               end do
               if (jafound == 0) then
                  call qnerror('partition_fill_sendlist: numbering error', message2, message3)
                  goto 1234
               end if
            end if
         end if
      end do

!     (re)allocate
      if (.not. allocated(nr_send_list)) then
         numdomains = ndomains
         allocate (nr_send_list(-1:numdomains - 1))
         nr_send_list(:) = 0
      else
         numdomains = ubound(nr_send_list, 1) + 1 ! number of domains so far
         if (other_domain_number > numdomains - 1) then ! number of domains needs to be increased
            idum = nr_send_list(numdomains - 1)
            call realloc(nr_send_list, other_domain_number, -1, fill=idum, keepExisting=.true.)
            numdomains = other_domain_number + 1
         end if
      end if

      if (.not. allocated(send_list)) then
         allocate (send_list(num))
      else
         numnew = nr_send_list(numdomains - 1) + num
         if (numnew > ubound(send_list, 1)) then
            call realloc(send_list, int(1.2d0 * dble(numnew) + 1d0), fill=0, keepExisting=.true.)
         end if
      end if

!     making a gap
      do i = nr_send_list(numdomains - 1), nr_send_list(other_domain_number) + 1, -1
         send_list(i + num) = send_list(i)
      end do
      ! put temp_list values in the gap
      do i = 1, num
         send_list(nr_send_list(other_domain_number) + i) = temp_list(i)
      end do
      nr_send_list(other_domain_number:numdomains - 1) = nr_send_list(other_domain_number:numdomains - 1) + num

      ierror = 0

1234  continue
      if (allocated(temp_list)) deallocate (temp_list)

      return
   end subroutine partition_fill_sendlist

!>  make the sendlists
!>    include additional layer to detect all flownodes and flowlinks ("enclosed, level-five" cells)
   subroutine partition_make_sendlists(idmn, fnam, ierror)
      use m_polygon
      use m_alloc
      use m_reapol

      implicit none

      integer, intent(in) :: idmn !< own domain number
      character(len=*), intent(in) :: fnam !< basename (not used in mpi-mode)

      integer, intent(out) :: ierror !< error (1) or not (0)

      type(tghost), dimension(:), allocatable :: sghost
      type(tghost), dimension(:), allocatable :: sallghost
      type(tghost), dimension(:), allocatable :: ughost

      integer, parameter :: MAXNAMELEN = 255

      integer :: idmn_other, minp

      character(len=MAXNAMELEN) :: filename
      character(len=4) :: sdmn ! domain number string
      integer, allocatable :: nghostlist_cn_temp(:) !< temporal storige of ghost list reference data
      integer, allocatable :: ighostlist_cn_temp(:) !< temporal storige of ghost list

      ierror = 1

      if (allocated(isendlist_s)) deallocate (isendlist_s)
      if (allocated(nsendlist_s)) deallocate (nsendlist_s)

      if (allocated(isendlist_sall)) deallocate (isendlist_sall)
      if (allocated(nsendlist_sall)) deallocate (nsendlist_sall)

      if (allocated(isendlist_u)) deallocate (isendlist_u)
      if (allocated(nsendlist_u)) deallocate (nsendlist_u)

      if (allocated(isendlist_cn)) deallocate (isendlist_cn)
      if (allocated(nsendlist_cn)) deallocate (nsendlist_cn)

      if (jampi == 0) then
!        save polygon
         call savepol()

         do idmn_other = 0, ndomains - 1
            write (sdmn, '(I4.4)') idmn_other
            filename = trim(trim(fnam)//'_'//sdmn//'_gst.pli')
            call oldfil(minp, filename)
            call reapol(minp, 0)

            if (NPL < 1) cycle

!           make ghostcells of other domain
            call partition_set_ghostlevels(idmn_other, numlay_cellbased + 1, numlay_nodebased + 1, 1, ierror)
            call partition_get_ghosts(idmn_other, ITYPE_S, sghost, ierror)
            call partition_get_ghosts(idmn_other, ITYPE_Sall, sallghost, ierror)
!           find the send cells
            call partition_fill_sendlist(idmn, idmn_other, ITYPE_SALL, NPL, XPL, YPL, sallghost, isendlist_sall, nsendlist_sall, ierror)
            call partition_fill_sendlist(idmn, idmn_other, ITYPE_S, NPL, XPL, YPL, sghost, isendlist_s, nsendlist_s, ierror)

            call partition_get_ghosts(idmn_other, ITYPE_U, ughost, ierror)
            call partition_fill_sendlist(idmn, idmn_other, ITYPE_U, NPL, XPL, YPL, ughost, isendlist_u, nsendlist_u, ierror)
         end do

         !     restore polygon
         call restorepol()

         if (allocated(sghost)) call dealloc_tghost(sghost)
         if (allocated(sallghost)) call dealloc_tghost(sallghost)
         if (allocated(ughost)) call dealloc_tghost(ughost)
      else
!        fill send lists
         call partition_make_sendlist_MPI(ITYPE_S, numlay_cellbased + 1, numlay_nodebased + 1, isendlist_s, nsendlist_s)
         call partition_make_sendlist_MPI(ITYPE_Sall, numlay_cellbased + 1, numlay_nodebased + 1, isendlist_sall, nsendlist_sall)
         call partition_make_sendlist_MPI(ITYPE_U, numlay_cellbased + 1, numlay_nodebased + 1, isendlist_u, nsendlist_u)

!        communicate sendlist back to obtain (possibly) reduced ghostlist in own domain
!        deallocate first
         nghostlist_s = 0
         if (allocated(ighostlist_s)) deallocate (ighostlist_s)
         nghostlist_sall = 0
         if (allocated(ighostlist_sall)) deallocate (ighostlist_sall)
         nghostlist_u = 0
         if (allocated(ighostlist_u)) deallocate (ighostlist_u)

!        fill ghostlists
         call partition_make_sendlist_MPI(ITYPE_S, numlay_cellbased + 1, numlay_nodebased + 1, ighostlist_s, nghostlist_s, ifromto=1)
         call partition_make_sendlist_MPI(ITYPE_Sall, numlay_cellbased + 1, numlay_nodebased + 1, ighostlist_sall, nghostlist_sall, ifromto=1)
         call partition_make_sendlist_MPI(ITYPE_u, numlay_cellbased + 1, numlay_nodebased + 1, ighostlist_u, nghostlist_u, ifromto=1)

         nghostlist_cn_temp = nghostlist_cn
         ighostlist_cn_temp = ighostlist_cn
         call partition_make_sendlist_MPI(ITYPE_CN, numlay_cellbased + 1, numlay_nodebased + 1, isendlist_cn, nsendlist_cn)
         nghostlist_cn = nghostlist_cn_temp
         ighostlist_cn = ighostlist_cn_temp

      end if

!     set number of send nodes/links
      numsend_s = get_list_size(nsendlist_s, nsendlist_sall)
      numsend_sall = get_list_size(nsendlist_sall, nsendlist_sall)
      numsend_u = get_list_size(nsendlist_u, nsendlist_sall)
      numsend_cn = get_list_size(nsendlist_cn, nsendlist_sall)

      numghost_s = get_list_size(nghostlist_s, nghostlist_sall)
      numghost_sall = get_list_size(nghostlist_sall, nghostlist_sall)
      numghost_u = get_list_size(nghostlist_u, nghostlist_sall)
      numghost_cn = get_list_size(nghostlist_cn, nghostlist_sall)

!     safety: make dummy empty lists
      call realloc(nghostlist_s, ndomains - 1, -1, keepExisting=.true., fill=0)
      call realloc(nghostlist_sall, ndomains - 1, -1, keepExisting=.true., fill=0)
      call realloc(nghostlist_u, ndomains - 1, -1, keepExisting=.true., fill=0)
      call realloc(nghostlist_cn, ndomains - 1, -1, keepExisting=.true., fill=0)

      call realloc(nsendlist_s, ndomains - 1, -1, keepExisting=.true., fill=0)
      call realloc(nsendlist_sall, ndomains - 1, -1, keepExisting=.true., fill=0)
      call realloc(nsendlist_u, ndomains - 1, -1, keepExisting=.true., fill=0)
      call realloc(nsendlist_cn, ndomains - 1, -1, keepExisting=.true., fill=0)

      call realloc(ighostlist_s, max(numghost_s, 1), keepExisting=.true., fill=0)
      call realloc(ighostlist_sall, max(numghost_sall, 1), keepExisting=.true., fill=0)
      call realloc(ighostlist_u, max(numghost_u, 1), keepExisting=.true., fill=0)
      call realloc(ighostlist_cn, max(numghost_cn, 1), keepExisting=.true., fill=0)

      call realloc(isendlist_s, max(numsend_s, 1), keepExisting=.true., fill=0)
      call realloc(isendlist_sall, max(numsend_sall, 1), keepExisting=.true., fill=0)
      call realloc(isendlist_u, max(numsend_u, 1), keepExisting=.true., fill=0)
      call realloc(isendlist_cn, max(numsend_cn, 1), keepExisting=.true., fill=0)

      ierror = 0
1234  continue

      return
   end subroutine partition_make_sendlists

!> communicate ghost cells to other domains
   subroutine partition_make_sendlist_MPI(itype, numlay_cell, numlay_node, i_list, n_list, ifromto)
      use m_alloc
      use m_missing
      use network_data, only: xzw, yzw, xk, yk
      use m_flowgeom, only: xu, yu
#ifdef HAVE_MPI
      use mpi
#endif

      implicit none
      integer, intent(in) :: itype !< type: 0: flow node, 1: flow link, 9: corners
      integer, intent(in) :: numlay_cell !< number of cell-based ghost-levels considered (normally numlay_cellbased+1)
      integer, intent(in) :: numlay_node !< number of node-based ghost-levels considered (normally numlay_nodebased+1)
      integer, allocatable, intent(inout) :: i_list(:) !< isendlist or ighostlist (consistent with ifromto)
      integer, allocatable, intent(inout) :: n_list(:) !< nsendlist or nghostlist (consistent with ifromto)
      integer, optional, intent(in) :: ifromto !< fill sendlists from ghostlists (0, default) or ghostlists from sendlists (1)

#ifdef HAVE_MPI

      type(tghost), allocatable, target :: ghost_list(:)

      double precision, pointer :: x_coords(:), y_coords(:)
      double precision, allocatable :: xy_send(:, :) ! send buffer for cell/link center or corner coordinates (first x, then y)
      double precision, allocatable :: xy_recv(:, :) ! receive buffer for cell/link center or corner coordinates (first x, then y)

      integer, allocatable :: ighostlev_bak(:) ! store of the combined ghost levelslevels
      integer, allocatable :: ighostlev_cellbased_bak(:) ! store of the cell-based ghost levels
      integer, allocatable :: ighostlev_nodebased_bak(:) ! store of the node-based ghost levels

      integer, pointer :: nfromlist(:)
      integer, pointer :: ifromlist(:)

      integer :: status(MPI_STATUS_SIZE)
      integer :: irequest(ndomains)
      integer :: my_proc_send_to(0:ndomains - 1) ! number of cells/links/corners send by my_rank proc to others
      integer :: all_proc_recv(0:ndomains - 1, 0:ndomains - 1) ! number of cells/links/corners received from other domains
      integer :: nrequest, itag = 1, error

      integer :: i, icount, num, other_domain
      integer :: from_send_list

      character(len=1024) :: str

      integer, target :: idumzero(-1:ndomains - 1)

      if (present(ifromto)) then
         from_send_list = ifromto
      else
         from_send_list = 0
      end if

      if (itype == ITYPE_S) then ! flow nodes
         if (from_send_list == 0) then
            nfromlist => nghostlist_s
            ifromlist => ighostlist_s
         else
            nfromlist => nsendlist_s
            ifromlist => isendlist_s
         end if
         x_coords => xzw
         y_coords => yzw
      else if (itype == ITYPE_Sall) then ! all flow nodes
         if (from_send_list == 0) then
            nfromlist => nghostlist_sall
            ifromlist => ighostlist_sall
         else
            nfromlist => nsendlist_sall
            ifromlist => isendlist_sall
         end if
         x_coords => xzw
         y_coords => yzw
      else if (itype == ITYPE_Snonoverlap) then ! all non-overlapping flow nodes (for solver)
         if (from_send_list == 0) then
            nfromlist => nghostlist_snonoverlap
            ifromlist => ighostlist_snonoverlap
         else
            nfromlist => nsendlist_snonoverlap
            ifromlist => isendlist_snonoverlap
         end if
         x_coords => xzw
         y_coords => yzw
      else if (itype == ITYPE_U) then ! flow links
         if (from_send_list == 0) then
            nfromlist => nghostlist_u
            ifromlist => ighostlist_u
         else
            nfromlist => nsendlist_u
            ifromlist => isendlist_u
         end if
         x_coords => xu
         y_coords => yu
      else if (itype == ITYPE_CN) then ! flow node corners
         if (from_send_list == 0) then
            nfromlist => nghostlist_cn
            ifromlist => ighostlist_cn
         else
            nfromlist => nsendlist_cn
            ifromlist => isendlist_cn
         end if
         x_coords => xk
         y_coords => yk
      else
         call qnerror('partition_make_sendlist_MPI: unknown ghost type', ' ', ' ')
         return
      end if

!     store ghost levels of this domain
      allocate (ighostlev_bak(ubound(ighostlev, 1)))
      ighostlev_bak = ighostlev
      allocate (ighostlev_cellbased_bak(ubound(ighostlev_cellbased, 1)))
      ighostlev_cellbased_bak = ighostlev_cellbased
      allocate (ighostlev_nodebased_bak(ubound(ighostlev_nodebased, 1)))
      ighostlev_nodebased_bak = ighostlev_nodebased

      if (.not. associated(nfromlist)) then
         idumzero(:) = 0
         nfromlist => idumzero
      end if

      num = nfromlist(ndomains - 1)
      allocate (xy_send(2, max(num, 1)))
      allocate (xy_recv(2, max(num, 1)))

      do i = 1, num
         xy_send(1, i) = x_coords(ifromlist(i))
         xy_send(2, i) = y_coords(ifromlist(i))
      end do

!     count other domains connected and number of cells to be sent
      my_proc_send_to(:) = 0
      do other_domain = 0, ndomains - 1
         if (other_domain == my_rank .or. (nfromlist(other_domain) - nfromlist(other_domain - 1) < 1)) cycle
         my_proc_send_to(other_domain) = nfromlist(other_domain) - nfromlist(other_domain - 1)
      end do

!     compose other-domain administration
      all_proc_recv(:, :) = 0
      call mpi_allgather(my_proc_send_to, ndomains, MPI_INTEGER, all_proc_recv, ndomains, MPI_INTEGER, DFM_COMM_DFMWORLD, error)

!     send own ghost data to other domains
      nrequest = 0
      irequest(:) = 0
      do other_domain = 0, ndomains - 1
         if (other_domain == my_rank .or. all_proc_recv(other_domain, my_rank) < 1) cycle
         nrequest = nrequest + 1
         call mpi_isend(xy_send(1, nfromlist(other_domain - 1) + 1), 2 * all_proc_recv(other_domain, my_rank), mpi_double_precision, &
                        other_domain, itag, DFM_COMM_DFMWORLD, irequest(nrequest), error)
      end do

!     make ghosts if we need to search in own subdomain (only once for all other subdomains)
      if (from_send_list == 1) then
         call partition_get_ghosts(my_rank, itype, ghost_list, error)
      end if

!     receive ghost data from other domains
      do other_domain = 0, ndomains - 1
         if (all_proc_recv(my_rank, other_domain) < 1) cycle
         num = all_proc_recv(my_rank, other_domain)
!        get message length
         call mpi_probe(other_domain, itag, DFM_COMM_DFMWORLD, status, error)
         call mpi_get_count(status, mpi_double_precision, icount, error)

!        check message length (safety)
         if (icount /= 2 * num) then
            write (str, *) 'partition_make_sendlist_MPI: icount.ne.2*num, domain: ', my_rank, &
               ', other domain: ', other_domain, ' icount: ', icount, ', 2*num: ', 2 * num
            call qnerror(str, ' ', ' ')
            call qnerror('partition_communicate_ghosts: message length error', ' ', ' ')
         end if

!        check recv array size
         if (icount > 2 * ubound(xy_recv, 2)) then ! reallocate if necessary
            call realloc(xy_recv, (/2, int(1.2d0 * dble(icount / 2) + 1d0)/), keepExisting=.false., fill=0d0)
         end if

         call mpi_recv(xy_recv, icount, mpi_double_precision, other_domain, MPI_ANY_TAG, DFM_COMM_DFMWORLD, status, error)

!        make ghostcells of other domain in this domain
         if (from_send_list == 0) then
            call partition_set_ghostlevels(other_domain, numlay_cell, numlay_node, 1, error)
            if (itype /= ITYPE_Snonoverlap) then
               call partition_get_ghosts(other_domain, itype, ghost_list, error)
            else
               call partition_get_ghosts(other_domain, ITYPE_Sall, ghost_list, error)
            end if
         end if

!       find the send cells
         if (from_send_list == 0) then
            call partition_fill_sendlist(my_rank, other_domain, itype, num, xy_recv(1, 1:num), xy_recv(2, 1:num), ghost_list, i_list, n_list, error)
         else
            call partition_fill_sendlist(other_domain, other_domain, itype, num, xy_recv(1, 1:num), xy_recv(2, 1:num), ghost_list, i_list, n_list, error)
         end if

         if (error /= 0) goto 1234
      end do ! other_domain=0,ndomains-1

!     restore ghost levels
      ighostlev = ighostlev_bak
      ighostlev_cellbased = ighostlev_cellbased_bak
      ighostlev_nodebased = ighostlev_nodebased_bak

!     wait for isends to finish
      call mpi_waitall(nrequest, irequest, MPI_STATUSES_IGNORE, error)

1234  continue

      ! in principal, the deallocation is not needed because the variables are local.
      if (allocated(xy_send)) deallocate (xy_send)
      if (allocated(xy_recv)) deallocate (xy_recv)
      if (allocated(ghost_list)) call dealloc_tghost(ghost_list)
      if (allocated(ighostlev_bak)) deallocate (ighostlev_bak)
      if (allocated(ighostlev_nodebased_bak)) deallocate (ighostlev_nodebased_bak)

#endif
      return
   end subroutine partition_make_sendlist_MPI

!> fix orientation of send flow-links, opposite orientation will have negative indices
   subroutine partition_fixorientation_ghostlist(ierror)
      use m_flowgeom, only: Lnx, Lnxi, csu, snu, Ln, Ndx
      use unstruc_messages
      implicit none

      character(len=128) :: message
      integer, intent(out) :: ierror !< error (1) or not (0)

      double precision, dimension(:), allocatable :: csu_loc, snu_loc

      integer, dimension(:), allocatable :: imask

      double precision :: dum

      integer :: i, k, k1, k2, L, num

      double precision, parameter :: dtol = 1d-8

      ierror = 1

!     allocate
      allocate (csu_loc(Lnx), snu_loc(Lnx))
      allocate (imask(Ndx))

!     fill local copy of cosine and sine of flow links
      csu_loc = csu
      snu_loc = snu

!     copy csu and snu from other domains
      call update_ghosts(ITYPE_U, 1, Lnx, csu_loc, ierror)
      call update_ghosts(ITYPE_U, 1, Lnx, snu_loc, ierror)

      if (ierror /= 0) goto 1234

!     mark the flownodes in the ghostlist
      imask = 0
      do i = 1, nghostlist_sall(ndomains - 1)
         k = ighostlist_sall(i)
         imask(k) = 1
      end do

!     mark the own flownodes
      do k = 1, Ndx
         if (idomain(k) == my_rank) then
            imask(k) = 1
         end if
      end do

!     check and fix orientation
      num = 0
      do i = 1, nghostlist_u(ndomains - 1)
         L = abs(ighostlist_u(i))

!        safety check
         dum = abs(abs(csu_loc(L) * csu(L) + snu_loc(L) * snu(L)) - 1d0)
         if (dum >= dtol) then
!           check if this is a valid ghostlink (see also subroutine "disable_invalid_ghostcells_with_wu")
            k1 = ln(1, L)
            k2 = ln(2, L)
            if (imask(k1) == 1 .and. imask(k2) == 1 .and. L <= Lnxi) then
               write (message, "('partition_fixorientation_ghostlist: flowlink mismatch, val=', E15.5, ', link=', I0)") dum, L
               call mess(LEVEL_ERROR, trim(message))
            else
               write (message, "('partition_fixorientation_ghostlist: dummy flowlink mismatch, val=', E15.5, ', link=', I0)") dum, L
               call mess(LEVEL_INFO, trim(message))
            end if
         end if

         if (abs(csu_loc(L) * csu(L) + snu_loc(L) * snu(L) + 1d0) < dtol) then
            ighostlist_u(i) = -ighostlist_u(i)
            num = num + 1
         end if
      end do

      call mess(LEVEL_INFO, 'partition_fixorientation_ghostlist: number of reversed flowlinks=', num)

      ierror = 0
1234  continue

!     deallocate
      if (allocated(csu_loc)) deallocate (csu_loc)
      if (allocated(snu_loc)) deallocate (snu_loc)
      if (allocated(imask)) deallocate (imask)

      return
   end subroutine partition_fixorientation_ghostlist

!> copy sendlist to samples
   subroutine copy_sendlist_to_sam()
      use m_samples
      use network_data, only: xzw, yzw
      use m_flowgeom, only: xu, yu
      use m_delsam

      implicit none

      integer :: i, j, k, numdomains, num

      call savesam()
      call delsam(0)

      numdomains = ubound(nsendlist_s, 1) + 1
      num = nsendlist_s(numdomains - 1)
      numdomains = ubound(nsendlist_sall, 1) + 1
      num = num + nsendlist_sall(numdomains - 1)
      numdomains = ubound(nsendlist_u, 1) + 1
      num = num + nsendlist_u(numdomains - 1)

      call increasesam(num)

      NS = 0
      do i = 0, ubound(nsendlist_s, 1)
         do j = nsendlist_s(i - 1) + 1, nsendlist_s(i)
            NS = NS + 1
            k = isendlist_s(j)
            xs(NS) = xzw(k)
            ys(NS) = yzw(k)
            zs(NS) = dble(i)
         end do
      end do

      do i = 0, ubound(nsendlist_sall, 1)
         do j = nsendlist_sall(i - 1) + 1, nsendlist_sall(i)
            NS = NS + 1
            k = isendlist_sall(j)
            xs(NS) = xzw(k)
            ys(NS) = yzw(k)
            zs(NS) = dble(i)
         end do
      end do

      do i = 0, ubound(nsendlist_u, 1)
         do j = nsendlist_u(i - 1) + 1, nsendlist_u(i)
            NS = NS + 1
            k = abs(isendlist_u(j))
            xs(NS) = xu(k)
            ys(NS) = yu(k)
            zs(NS) = -dble(i)
         end do
      end do

   end subroutine copy_sendlist_to_sam

!> derive 3d ghost- and sendlists from 2d counterparts and 3d layer information
!>    only nghostlist_XXX_3d will be generated, actual ghost flow node/link numbers
!>       can be derived from 2d ighostlist/isendlist and layer information
!>    it is assumed that 3d layer information (kmxn, kmxL) is available
   subroutine partition_make_ghostsendlists_3d(ierror)
      use m_flowgeom, only: Ndx, Lnx
      use m_flow, only: kmx, kmxn, kmxL
      implicit none

      integer, intent(out) :: ierror

      integer :: L
      integer, allocatable :: kmxL1(:)

      ierror = 0 ! so far, so good

      if (kmx == 0) goto 1234

      ierror = 1

      if (allocated(nghostlist_s_3d)) deallocate (nghostlist_s_3d)
      if (allocated(nsendlist_s_3d)) deallocate (nsendlist_s_3d)
!      allocate(nghostlist_s_3d(-1:ndomains-1))    ! not used
!      allocate(nsendlist_s_3d(-1:ndomains-1))     ! not used

      if (allocated(nghostlist_sall_3d)) deallocate (nghostlist_sall_3d)
      if (allocated(nsendlist_sall_3d)) deallocate (nsendlist_sall_3d)
      allocate (nghostlist_sall_3d(-1:ndomains - 1))
      allocate (nsendlist_sall_3d(-1:ndomains - 1))

      if (allocated(nghostlist_u_3d)) deallocate (nghostlist_u_3d)
      if (allocated(nsendlist_u_3d)) deallocate (nsendlist_u_3d)
      allocate (nghostlist_u_3d(-1:ndomains - 1))
      allocate (nsendlist_u_3d(-1:ndomains - 1))

      if (allocated(nghostlist_u_3dw)) deallocate (nghostlist_u_3dw)
      if (allocated(nsendlist_u_3dw)) deallocate (nsendlist_u_3dw)
      allocate (nghostlist_u_3dw(-1:ndomains - 1))
      allocate (nsendlist_u_3dw(-1:ndomains - 1))

      call partition_fill_ghostsendlist_3d(nghostlist_sall(ndomains - 1), ighostlist_sall, nghostlist_sall, Ndx, kmxn, nghostlist_sall_3d)
      call partition_fill_ghostsendlist_3d(nsendlist_sall(ndomains - 1), isendlist_sall, nsendlist_sall, Ndx, kmxn, nsendlist_sall_3d)

      call partition_fill_ghostsendlist_3d(nghostlist_u(ndomains - 1), ighostlist_u, nghostlist_u, Lnx, kmxL, nghostlist_u_3d)
      call partition_fill_ghostsendlist_3d(nsendlist_u(ndomains - 1), isendlist_u, nsendlist_u, Lnx, kmxL, nsendlist_u_3d)

      if (allocated(kmxL1)) deallocate (kmxL1)
      allocate (kmxL1(lnx))
      do L = 1, lnx
         kmxL1(L) = kmxL(L) + 1
      end do
      call partition_fill_ghostsendlist_3d(nghostlist_u(ndomains - 1), ighostlist_u, nghostlist_u, Lnx, kmxL1, nghostlist_u_3dw)
      call partition_fill_ghostsendlist_3d(nsendlist_u(ndomains - 1), isendlist_u, nsendlist_u, Lnx, kmxL1, nsendlist_u_3dw)
      deallocate (kmxL1)

      ierror = 0
1234  continue

      return
   end subroutine partition_make_ghostsendlists_3d

!> fill 3d ghost/send flow node/link list
   subroutine partition_fill_ghostsendlist_3d(Nghost, ilist2d, nlist2d, N, kmxn, nlist3d)
      implicit none

      integer, intent(in) :: Nghost !< number of ghost/send flow nodes/links
      integer, dimension(Nghost), intent(in) :: ilist2d !< 2d ghost/send list
      integer, dimension(-1:ndomains - 1), intent(in) :: nlist2d !< cumulative number of flow nodes/links in 2d ghost/send list
      integer, intent(in) :: N !< number of flow nodes/links
      integer, dimension(N), intent(in) :: kmxn !< number of layers
      integer, dimension(-1:ndomains - 1), intent(out) :: nlist3d !< 3d ghost/send list

      integer :: i, idmn, k, num

      nlist3d(-1) = 0
      do idmn = 0, ndomains - 1
!        add number of 3d ghost/send nodes/links from/to this domain
         num = 0
         do i = nlist2d(idmn - 1) + 1, nlist2d(idmn)
            k = abs(ilist2d(i))
            num = num + kmxn(k)
         end do
         nlist3d(idmn) = nlist3d(idmn - 1) + num
      end do

      return
   end subroutine partition_fill_ghostsendlist_3d

!> generate non-overlapping ghost/sendlists (for solver)
   subroutine partition_fill_ghostsendlist_nonoverlap(error)
      use m_alloc
      implicit none

      integer, intent(out) :: error !< error (1) or not (0)

      integer :: i, idmn, iglev, inum, k, lenold

      if (allocated(nghostlist_snonoverlap)) deallocate (nghostlist_snonoverlap)
      if (allocated(ighostlist_snonoverlap)) deallocate (ighostlist_snonoverlap)

      if (allocated(nsendlist_snonoverlap)) deallocate (nsendlist_snonoverlap)
      if (allocated(isendlist_snonoverlap)) deallocate (isendlist_snonoverlap)

      allocate (nghostlist_snonoverlap(-1:ndomains - 1))
      nghostlist_snonoverlap = 0
      allocate (nsendlist_snonoverlap(-1:ndomains - 1))
      nsendlist_snonoverlap = 0

!     check for overlap (in solver)
      if (maxghostlev_s == 1) then
         jaoverlap = 0
         error = 0
         return
      end if

!     safety: allocate lists with full overlapping length
      lenold = ubound(ighostlist_sall, 1)
      allocate (ighostlist_snonoverlap(lenold))
      ighostlist_snonoverlap = 0
      lenold = ubound(isendlist_sall, 1)
      allocate (isendlist_snonoverlap(lenold))
      isendlist_snonoverlap = 0

      jaoverlap = 1

!     select nodes from ghostlist
      inum = 0
      do idmn = 0, ndomains - 1
         if (idmn == my_rank) cycle

         do i = nghostlist_sall(idmn - 1) + 1, nghostlist_sall(idmn)
            k = ighostlist_sall(i)

!           get appropriate ghostlevel
            if (ighosttype_s == IGHOSTTYPE_CELLBASED) then
               iglev = ighostlev_cellbased(k)
            else if (ighosttype_s == IGHOSTTYPE_NODEBASED) then
               iglev = ighostlev_nodebased(k)
            else ! combined
               iglev = ighostlev(k)
            end if

!!           only add ghost nodes with ghostlevel >= minghostlev_s
!           only add ghost nodes with ghostlevel >= maxghostlev_s
!            if ( iglev.ge.minghostlev_s .or. iglev.eq.0 ) then
            if (iglev >= maxghostlev_s .or. iglev == 0) then
               nghostlist_snonoverlap(idmn) = nghostlist_snonoverlap(idmn) + 1
               inum = inum + 1
               ighostlist_snonoverlap(inum) = k
            end if
         end do
      end do

!     make nghostlist_snonoverlap cumulative
      nghostlist_snonoverlap(-1) = 0
      do i = 0, ndomains - 1
         nghostlist_snonoverlap(i) = nghostlist_snonoverlap(i - 1) + nghostlist_snonoverlap(i)
      end do

!     make sendlist
      call partition_make_sendlist_MPI(ITYPE_Snonoverlap, numlay_cellbased + 1, numlay_nodebased + 1, isendlist_snonoverlap, nsendlist_snonoverlap)

!     (re)set number of ghost nodes/links
      numghost_snonoverlap = nghostlist_snonoverlap(ndomains - 1)
      numsend_snonoverlap = nsendlist_snonoverlap(ndomains - 1)

!     safety: make dummy empty lists
      call realloc(nghostlist_snonoverlap, ndomains - 1, -1, keepExisting=.true., fill=0)
      call realloc(nsendlist_snonoverlap, ndomains - 1, -1, keepExisting=.true., fill=0)

      call realloc(ighostlist_snonoverlap, max(numghost_snonoverlap, 1), keepExisting=.true., fill=0)
      call realloc(isendlist_snonoverlap, max(numsend_snonoverlap, 1), keepExisting=.true., fill=0)

!     check number of non-overlapping ghost nodes
!      if ( numghost_snonoverlap .ne. numghost_sall-noverlap ) then
!         call mess(LEVEL_ERROR, 'partition_fill_ghostsendlist_nonoverlap gave error')
!         goto 1234
!      end if

      error = 0

      return
   end subroutine partition_fill_ghostsendlist_nonoverlap

   subroutine update_ghosts(itype, ndim, n, solution, error, ignore_orientation)
#ifdef HAVE_MPI
      use mpi
#endif
      use m_flowgeom
      use m_flow, only: kmxn, kmxL, kbot, Lbot, Ndkx, Lnkx
      use network_data, only: numk
      use messageHandling

      implicit none

      integer, intent(in) :: itype !< type: 0: flownode, 1: flowlink
      integer, intent(in) :: ndim !< number of unknowns per flownode/link
      integer, intent(in) :: n !< number of flownodes/links
      double precision, dimension(ndim*n), intent(inout) :: solution !< solution
      integer, intent(out) :: error !< error (1) or not (0)
      logical, optional, intent(in) :: ignore_orientation !< Ignore orientation of ghost and own location, useful for directionless quantities on u-points. Default: .false.

      error = 1

      if (.not. allocated(isendlist_sall)) goto 1234 ! safety

      if (itype == ITYPE_S) then
         if (n /= ndx) then
            call qnerror('update_ghosts, ITYPE_S: numbering error', ' ', ' ')
            goto 1234
         end if
         call update_ghost_loc(ndomains, ndim, n, solution, nghostlist_s(ndomains - 1), ighostlist_s, nghostlist_s, &
                               nsendlist_s(ndomains - 1), isendlist_s, nsendlist_s, ITAG_S, error, ignore_orientation=ignore_orientation)
      else if (itype == ITYPE_SALL) then
         if (n /= ndx) then
            call qnerror('update_ghosts, ITYPE_Sall: numbering error', ' ', ' ')
            goto 1234
         end if
         call update_ghost_loc(ndomains, ndim, n, solution, nghostlist_sall(ndomains - 1), ighostlist_sall, nghostlist_sall, &
                               nsendlist_sall(ndomains - 1), isendlist_sall, nsendlist_sall, ITAG_Sall, error, ignore_orientation=ignore_orientation)
      else if (itype == ITYPE_U) then
         if (n /= lnx) then
            call qnerror('update_ghosts, ITYPE_U: numbering error', ' ', ' ')
            goto 1234
         end if
         call update_ghost_loc(ndomains, ndim, n, solution, nghostlist_u(ndomains - 1), ighostlist_u, nghostlist_u, &
                               nsendlist_u(ndomains - 1), isendlist_u, nsendlist_u, ITAG_U, error, ignore_orientation=ignore_orientation)
      else if (itype == ITYPE_CN) then
         if (n /= numk) then
            call qnerror('update_ghosts, ITYPE_CN: numbering error', ' ', ' ')
            goto 1234
         end if
         call update_ghost_loc(ndomains, ndim, n, solution, nghostlist_cn(ndomains - 1), ighostlist_cn, &
                               nghostlist_cn, nsendlist_cn(ndomains - 1), isendlist_cn, nsendlist_cn, ITAG_CN, error)
!
!     3D-extension
      else if (itype == ITYPE_S3D) then
         if (n /= ndkx) then
            call qnerror('update_ghosts, ITYPE_S3D: numbering error', ' ', ' ')
            goto 1234
         end if
         call update_ghost_loc(ndomains, ndim, n, solution, nghostlist_s(ndomains - 1), ighostlist_s, nghostlist_s, &
                               nsendlist_s(ndomains - 1), isendlist_s, nsendlist_s, ITAG_S, error, nghostlist_s_3D, nsendlist_s_3D, &
                               kmxn, kbot, ignore_orientation=ignore_orientation)
      else if (itype == ITYPE_SALL3D) then
         if (n /= ndkx) then
            call qnerror('update_ghosts, ITYPE_Sall3D: numbering error', ' ', ' ')
            goto 1234
         end if
         call update_ghost_loc(ndomains, ndim, n, solution, nghostlist_sall(ndomains - 1), ighostlist_sall, &
                               nghostlist_sall, nsendlist_sall(ndomains - 1), isendlist_sall, nsendlist_sall, ITAG_Sall, error, nghostlist_sall_3D, nsendlist_sall_3D, &
                               kmxn, kbot, ignore_orientation=ignore_orientation)
      else if (itype == ITYPE_U3D) then
         if (n /= lnkx) then
            call qnerror('update_ghosts, ITYPE_U3D: numbering error', ' ', ' ')
            goto 1234
         end if
         call update_ghost_loc(ndomains, ndim, n, solution, nghostlist_u(ndomains - 1), ighostlist_u, nghostlist_u, &
                               nsendlist_u(ndomains - 1), isendlist_u, nsendlist_u, ITAG_U, error, nghostlist_u_3D, nsendlist_u_3D, &
                               kmxL, Lbot, ignore_orientation=ignore_orientation)
      else if (itype == ITYPE_U3DW) then
         if (n /= lnkx) then
            call qnerror('update_ghosts, ITYPE_U3DW: numbering error', ' ', ' ')
            goto 1234
         end if
         call update_ghost_loc(ndomains, ndim, n, solution, nghostlist_u(ndomains - 1), ighostlist_u, nghostlist_u, &
                               nsendlist_u(ndomains - 1), isendlist_u, nsendlist_u, ITAG_U, error, nghostlist_u_3Dw, nsendlist_u_3Dw, &
                               kmxL + 1, Lbot - 1, ignore_orientation=ignore_orientation)

!     overlap
      else if (itype == ITYPE_Snonoverlap) then
         if (n /= ndx) then
            call qnerror('update_ghosts, ITYPE_Snonoverlap: numbering error', ' ', ' ')
            goto 1234
         end if
         if (jaoverlap == 1) then
            call update_ghost_loc(ndomains, ndim, n, solution, nghostlist_snonoverlap(ndomains - 1), ighostlist_snonoverlap, &
                                  nghostlist_snonoverlap, nsendlist_snonoverlap(ndomains - 1), isendlist_snonoverlap, nsendlist_snonoverlap, &
                                  ITAG_Snonoverlap, error, ignore_orientation=ignore_orientation)
         else ! no overlap: use sall
            call update_ghost_loc(ndomains, ndim, n, solution, nghostlist_sall(ndomains - 1), ighostlist_sall, &
                                  nghostlist_sall, nsendlist_sall(ndomains - 1), isendlist_sall, nsendlist_sall, ITAG_Sall, error, &
                                  ignore_orientation=ignore_orientation)
         end if
      else
         call qnerror('update_ghosts: unknown ghost type', ' ', ' ')
      end if

      error = 0
1234  continue

      if (error /= 0) call mess(LEVEL_ERROR, 'update_ghosts gave error')

      return
   end subroutine update_ghosts

!> update ghost values
!>   3D extension: we assume that kbot/Lbot and kmxn/kmxL match their counterparts in the other domain(s)
   subroutine update_ghost_loc(ndomains, NDIM, N, s, numghost, ighost, nghost, numsend, isend, nsend, itag, ierror, nghost3d, nsend3d, kmxnL, kbot, ignore_orientation)
#ifdef HAVE_MPI
      use mpi
#endif
      use m_flowgeom
      use m_alloc

      implicit none

      integer, intent(in) :: ndomains !< number of subdomains
      integer, intent(in) :: NDIM !< number of unknowns per flownode/link
      integer, intent(in) :: N !< number of flownodes/links
      double precision, dimension(NDIM*N), intent(inout) :: s !< Solution. Note: will correct for orientation between ghost and own location if needed (typically only for u-points).
      integer, intent(in) :: numghost !< number of ghost nodes/links
      integer, dimension(numghost), intent(in) :: ighost !< ghost nodes/links
      integer, dimension(-1:ndomains - 1), intent(in) :: nghost !< ghost list pointers
      integer, intent(in) :: numsend !< number of send nodes/links
      integer, dimension(numsend), intent(in) :: isend !< ghost nodes/links
      integer, dimension(-1:ndomains - 1), intent(in) :: nsend !< ghost list pointers
      integer, intent(in) :: itag !< message tag
      integer, intent(out) :: ierror !< error (1) or not (0)
!     3D-extension
      integer, dimension(-1:ndomains - 1), intent(in), optional :: nghost3d !< number of unknowns to be received per domain
      integer, dimension(-1:ndomains - 1), intent(in), optional :: nsend3d !< number of unknowns to be send     per domain
      integer, dimension(N), intent(in), optional :: kmxnL !< number of layers
      integer, dimension(N), intent(in), optional :: kbot !< bottom layer indices
      logical, intent(in), optional :: ignore_orientation !< Ignore orientation of ghost and own location, useful for directionless quantities on u-points. Default: .false.

!      double precision, dimension(:), allocatable                :: work         ! work array
#ifdef HAVE_MPI
      integer, dimension(MPI_STATUS_SIZE) :: istat

      integer, dimension(ndomains) :: irequest

      integer :: other_rank, ierr, i, ii, ib, it, nreq
      integer :: i2d, istart, iend, icount, num
      integer :: j

      integer :: ja3d ! 3D (1) or not (0)

      integer, parameter :: INIWORKSIZE = 1000
!      double precision, parameter                                 :: DNOCELL = -1234.5678

      character(len=1024) :: str
      logical :: ignore_orientation_
#endif
      ierror = 1
#ifdef HAVE_MPI

!     check for 3D
      ja3d = 0
      if (present(nghost3d) .and. present(nsend3d) .and. present(kmxnL) .and. present(kbot)) then
         ja3d = 1
      end if

      ignore_orientation_ = .false.
      if (present(ignore_orientation)) then
         ignore_orientation_ = ignore_orientation
      end if

!     allocate work array (will be reallocated later)
      if (.not. allocated(work)) allocate (work(INIWORKSIZE))

      if (ja3d /= 1) then
         num = nsend(ndomains - 1) * NDIM
      else
         num = nsend3d(ndomains - 1) * NDIM
      end if

      if (ubound(work, 1) < num) then
         call realloc(work, int(1.2d0 * dble(num) + 1d0))
      end if

!     fill work array
      if (ja3d /= 1) then
         if (NDIM == 1) then
            do i = 1, nsend(ndomains - 1)
!               if ( isend(i).gt.0 ) then
               work(i) = s(isend(i))
!               else  ! if ( isend(i).lt.0 ) then
!                  work(i) = -s(-isend(i))
!!               else  ! no cell was found during the handshake, send missing value
!!                  work(i) = DNOCELL
!               end if
            end do
         else ! NDIM.ne.1
            do i = 1, nsend(ndomains - 1)
!               if ( isend(i).gt.0 ) then
               do j = 1, NDIM
                  work(NDIM * (i - 1) + j) = s(NDIM * (isend(i) - 1) + j)
               end do
!               else  ! if ( isend(i).lt.0 ) then
!                  do j=1,NDIM
!                     work(NDIM*(i-1)+j) = -s(NDIM*(-isend(i)-1)+j)
!                  end do
!               else  ! no cell was found during the handshake, send missing value
!                  do j=1,NDIM
!                     work(NDIM*(i-1)+j) = DNOCELL
!                  end do
!               end if
            end do
         end if
      else ! 3D extension
         if (NDIM == 1) then
            icount = 0
            do i = 1, nsend(ndomains - 1)
               i2d = abs(isend(i))
               ib = kbot(i2d)
               it = ib + kmxnL(i2d) - 1 ! override it
               do ii = ib, it
                  icount = icount + 1
!                  if ( isend(i).gt.0 ) then
                  do j = 1, NDIM
                     work(icount) = s(ii)
                  end do
!                  else !   if ( isend(i).lt.0 ) then
!                     do j=1,NDIM
!                        work(icount) = -s(-ii)
!                     end do
!                  else    ! no cell was found during the handshake, send missing value
!                     do j=1,NDIM
!                        work(icount) = DNOCELL
!                     end do
!                  end if
               end do ! do i=ib,it
            end do
         else ! NDIM.ne.1
            icount = 0
            do i = 1, nsend(ndomains - 1)
               i2d = abs(isend(i))
               ib = kbot(i2d)
               it = ib + kmxnL(i2d) - 1 ! override it
               do ii = ib, it
                  icount = icount + 1
                  !                 if ( isend(i).gt.0 ) then
                  do j = 1, NDIM
                     work(NDIM * (icount - 1) + j) = s(NDIM * (ii - 1) + j)
                  end do
!                  else  ! if ( isend(i).lt.0 ) then
!                     do j=1,NDIM
!                        work(NDIM*(icount-1)+j) = -s(NDIM*(-ii-1)+j)
!                     end do
!                  else    ! no cell was found during the handshake, send missing value
!                     do j=1,NDIM
!                        work(NDIM*(icount-1)+j) = DNOCELL
!                     end do
!                  end if
               end do ! do i=ib,it
            end do
         end if
         if (icount /= nsend3d(ndomains - 1)) then
            call qnerror('update_ghost_loc: 3d numbering error', ' ', ' ')
         end if
      end if

!     we need to make sure that all processes are at this point right now, since we are using one global work array for sending and receiving
      call mpi_barrier(DFM_COMM_DFMWORLD, ierr)

!     send
      if (ja3d == 0) then
         nreq = 0
         do other_rank = 0, ndomains - 1
            istart = NDIM * nsend(other_rank - 1) + 1 ! nsend(other_rank-1)+1
            iend = NDIM * nsend(other_rank) ! nsend(other_rank)
            num = iend - istart + 1

            if (num > 0) then
               nreq = nreq + 1
               call mpi_isend(work(istart), num, mpi_double_precision, other_rank, itag, DFM_COMM_DFMWORLD, irequest(nreq), ierr)
            end if
         end do
      else
         nreq = 0
         do other_rank = 0, ndomains - 1
            istart = NDIM * nsend3d(other_rank - 1) + 1 ! nsend3d(other_rank-1)+1
            iend = NDIM * nsend3d(other_rank) ! nsend3d(other_rank)
            num = iend - istart + 1

            if (num > 0) then
!               write(6, "('update_ghost_loc:    send, domain: ', I3, ', other domain: ', I3, ', num: ', I7)") my_rank, other_rank, num
               nreq = nreq + 1
               call mpi_isend(work(istart), num, mpi_double_precision, other_rank, itag, DFM_COMM_DFMWORLD, irequest(nreq), ierr)
            end if
         end do
      end if

!     recieve
!     allocate work array (will be reallocated later)
      if (.not. allocated(workrec)) allocate (workrec(INIWORKSIZE))

      if (ja3d /= 1) then
         num = NDIM * nghost(ndomains - 1)
      else
         num = NDIM * nghost3d(ndomains - 1)
      end if

      if (ubound(workrec, 1) < num) then
         call realloc(workrec, int(1.2d0 * dble(num) + 1d0))
      end if

      if (ja3d /= 1) then
         do other_rank = 0, ndomains - 1
            istart = NDIM * nghost(other_rank - 1) + 1 ! nghost(other_rank-1)+1
            iend = NDIM * nghost(other_rank) ! nghost(other_rank)
            num = iend - istart + 1
            if (num > 0) then
               !call mpi_recv(s(ighost(istart)),num,mpi_double_precision,other_rank,itag,DFM_COMM_DFMWORLD,istat,ierr)

               !           ghost cells are NOT ordered
               call mpi_recv(workrec(istart), num, mpi_double_precision, other_rank, itag, DFM_COMM_DFMWORLD, istat, ierr)

               !           check message size
               call mpi_get_count(istat, mpi_double_precision, icount, ierr)
               if (icount /= num) then
                  write (str, *) 'update_ghost_loc: icount.ne.num, domain: ', my_rank, ', other domain: ', other_rank, ' icount: ', icount, ', num: ', num
                  call qnerror(str, ' ', ' ')
                  goto 1234
               end if
            end if
         end do
      else
         do other_rank = 0, ndomains - 1
            istart = NDIM * nghost3d(other_rank - 1) + 1 ! nghost3d(other_rank-1)+1
            iend = NDIM * nghost3d(other_rank) ! nghost3d(other_rank)
            num = iend - istart + 1

            if (num > 0) then
               !call mpi_recv(s(ighost(istart)),num,mpi_double_precision,other_rank,itag,DFM_COMM_DFMWORLD,istat,ierr)

!               write(6, "('update_ghost_loc: recieve, domain: ', I3, ', other domain: ', I3, ', num: ', I7)") my_rank, other_rank, num

               !           ghost cells are NOT ordered
               call mpi_recv(workrec(istart), num, mpi_double_precision, other_rank, itag, DFM_COMM_DFMWORLD, istat, ierr)

               !           check message size
               call mpi_get_count(istat, mpi_double_precision, icount, ierr)
               if (icount /= num) then
                  write (str, *) 'update_ghost_loc: icount.ne.num, domain: ', my_rank, ', other domain: ', other_rank, ' icount: ', icount, ', num: ', num
                  call qnerror(str, ' ', ' ')
                  goto 1234
               end if
            end if
         end do
      end if

!     copy work array to ghost nodes
      if (ja3d /= 1) then
         if (NDIM == 1) then
            do i = 1, nghost(ndomains - 1)
!               if ( workrec(i).ne.DNOCELL ) then

               if (ighost(i) > 0) then
                  s(ighost(i)) = workrec(i)
               else if (ighost(i) < 0) then
                  ! Some quantities may not need an orientation fix on u-points:
                  if (ignore_orientation_) then
                     s(-ighost(i)) = workrec(i)
                  else
                     s(-ighost(i)) = -workrec(i)
                  end if
               end if

!               else  ! no cell was found during handshake
!!                 do nothing
!                  continue
!               end if
            end do
         else ! NDIM.ne.1
            do i = 1, nghost(ndomains - 1)
!               if ( workrec(NDIM*(i-1)+1).ne.DNOCELL ) then ! check first element only
               if (ighost(i) > 0) then
                  do j = 1, NDIM
                     s(NDIM * (ighost(i) - 1) + j) = workrec(NDIM * (i - 1) + j)
                  end do
               else if (ighost(i) < 0) then
                  do j = 1, NDIM
                     s(NDIM * (-ighost(i) - 1) + j) = -workrec(NDIM * (i - 1) + j)
                  end do
               end if
!               else  ! no cell was found during handshake
!!                 do nothing
!               end if
            end do
         end if
      else ! 3D extension, we assume that bot and top matches the ibotsend in the other subdomain(s)
         icount = 0
         if (NDIM == 1) then
            do i = 1, nghost(ndomains - 1)
               if (ighost(i) > 0) then
                  i2d = ighost(i)
                  ib = kbot(i2d)
                  it = ib + kmxnL(i2d) - 1 ! override it
                  do ii = ib, it
                     icount = icount + 1
                     !                  if ( workrec(icount).ne.DNOCELL ) then
                     s(ii) = workrec(icount)
                     !                  else  ! no cell was found during handshake
   !!                    do nothing
                     !                  end if
                  end do
               else if (ighost(i) < 0) then
                  i2d = -ighost(i)
                  ib = kbot(i2d)
                  it = ib + kmxnL(i2d) - 1 ! override it
                  do ii = ib, it
                     icount = icount + 1
                     !                  if ( workrec(icount).ne.DNOCELL ) then
                     s(ii) = -workrec(icount)
                     !                  else  ! no cell was found during handshake
   !!                    do nothing
                     !                  end if
                  end do
               end if
            end do
         else ! NDIM.ne.1
            do i = 1, nghost(ndomains - 1)
               if (ighost(i) > 0) then
                  i2d = ighost(i)
                  ib = kbot(i2d)
                  it = ib + kmxnL(i2d) - 1 ! override it
                  do ii = ib, it
                     icount = icount + 1
                     !                  if ( workrec(NDIM*(icount-1)+j).ne.DNOCELL ) then  ! check first element only
                     do j = 1, NDIM
                        s(NDIM * (ii - 1) + j) = workrec(NDIM * (icount - 1) + j)
                     end do
                     !                  else  ! no cell was found during handshake
   !!                    do nothing
                     !                  end if
                  end do
               else if (ighost(i) < 0) then
                  i2d = -ighost(i)
                  ib = kbot(i2d)
                  it = ib + kmxnL(i2d) - 1 ! override it
                  do ii = ib, it
                     icount = icount + 1
                     !                  if ( workrec(NDIM*(icount-1)+j).ne.DNOCELL ) then  ! check first element only
                     do j = 1, NDIM
                        s(NDIM * (ii - 1) + j) = -workrec(NDIM * (icount - 1) + j)
                     end do
                     !                  else  ! no cell was found during handshake
   !!                    do nothing
                     !                  end if
                  end do
               end if
            end do
         end if

         if (icount /= nghost3d(ndomains - 1)) then
            call qnerror('update_ghost_loc: 3d numbering error', ' ', ' ')
         end if
      end if

!     terminate send (safety)
      do i = 1, nreq
         call mpi_wait(irequest(i), istat, ierr)
      end do

      ierror = 0
1234  continue

!     deallocate
!      if ( allocated(work) ) deallocate(work)
#endif
      return
   end subroutine update_ghost_loc

!> Makes the global flow node numbers for this domain's own flow nodes and links.
!! These numbers for all partitions together will form a continuous list
!! of flow node/link numbers, which could be used for aggregation of
!! partitioned output files.
   subroutine partition_make_globalnumbers(ierror)
      use m_flowgeom, only: Ndxi, Ndx, Ln, Lnxi, Lnx
      use m_alloc
      use unstruc_messages
      implicit none

      integer, intent(out) :: ierror

      integer :: i, istat, k, numlist
      integer :: L, ki
      integer, dimension(:), allocatable :: inums
      integer, dimension(:), allocatable :: numcells_s, idum

      integer :: Ndxi_glob

      ierror = 1

      allocate (inums(Ndx), stat=istat)
      if (istat /= 0) then
         goto 1234
      end if

      allocate (idum(Ndx), stat=istat)
      if (istat /= 0) then
         goto 1234
      end if

!  initialize
      inums = 0
      idum = 0

! Get global numbers for this domain's own internal flow nodes

      if (.not. allocated(iglobal_s)) then
!     set internal numbers (non-boundary nodes)
         numlist = 0
         do k = 1, Ndxi
            if (idomain(k) == my_rank) then
               numlist = numlist + 1
               inums(numlist) = k
            end if
         end do

         call get_global_numbers(numlist, inums(1:Ndxi), iglobal_s, numcells_s, 0)

!     get the number of global cells (global Ndxi)
         Ndxi_glob = sum(numcells_s(0:ndomains - 1))
      else
!     interal (2D) cells already read from file
         Ndxi_glob = Nglobal_s

         call realloc(iglobal_s, Ndx, keepExisting=.true., fill=0)
      end if

!  set boundary numbers
!  first add non-ghost boundary nodes to the list
      numlist = 0
      do L = Lnxi + 1, Lnx
!     find boundary node (>Ndxi)
         k = max(ln(1, L), ln(2, L))
!     find connected internal node (<=Ndxi)
         ki = min(ln(1, L), ln(2, L))
!     only add boundary node if the connected internal node is not a ghost node
         if (idomain(ki) == my_rank) then
            numlist = numlist + 1
            inums(numlist) = k
         end if
      end do

!  get the global numbering of the boundary nodes
      call get_global_numbers(numlist, inums(1:numlist), idum, numcells_s, 0)

!  add boundary nodes to global numbering
      do i = 1, numlist
         k = inums(i)

!     check if this node did not already have a global number assigned
         if (iglobal_s(k) /= 0) then
            call mess(LEVEL_ERROR, 'partition_make_globalnumbers: numbering error')
            goto 1234
         end if

         iglobal_s(k) = Ndxi_glob + idum(k)
      end do

      ierror = 0
1234  continue

      if (allocated(inums)) deallocate (inums)
      if (allocated(idum)) deallocate (idum)

      return
   end subroutine partition_make_globalnumbers

!>  get the global cell numbering for a given list of local flow node numbers (e.g., CG flow nodes only)
!!
!! The iactive list could contain only the CG flow nodes, or for example, only the ndxi internal flow nodes.
!! Results are stored in the iglobnum array, which is typically the 'iglobal' array.
   subroutine get_global_numbers(numactive, iactive, iglobnum, numglobcells, jatime)
      use m_flowgeom, only: Ndx
#ifdef HAVE_MPI
      use mpi
#endif
      use m_timer

      implicit none

      integer, intent(in) :: numactive !< number of active cells
      integer, dimension(numactive), intent(in) :: iactive !< The flow node numbers of the active cells.
      integer, dimension(:), allocatable, intent(out) :: iglobnum !< Target array in which the global numbers will be stored.
      integer, dimension(:), allocatable, intent(out) :: numglobcells !< number of global cells per subdomain, dim(0:ndomains-1)
      integer, intent(in) :: jatime !< time MPI communication (1) or not (0)

#ifdef HAVE_MPI
      double precision, dimension(:), allocatable :: dum

      integer :: i, n, num
      integer :: ierror

      ierror = 1

      if (allocated(iglobnum)) deallocate (iglobnum)
      if (allocated(numglobcells)) deallocate (numglobcells)

!     allocate
      allocate (iglobnum(Ndx))
      allocate (numglobcells(0:max(ndomains - 1, 0)))
      allocate (dum(Ndx))

!     mark active cells
      iglobnum = 0
      do i = 1, numactive
         iglobnum(iactive(i)) = 1
      end do

      if (jampi == 1) then
         if (jaoverlap == 0) then
            !        unmark ghost cells
            do i = 1, numghost_sall
               iglobnum(ighostlist_sall(i)) = 0
            end do
         else
            !        unmark non-overlapping ghost cells
            do i = 1, numghost_snonoverlap
               iglobnum(ighostlist_snonoverlap(i)) = 0
            end do
         end if

!!        unmark ghost cells, alternative based on ghost levels
!         do i=1,Ndx
!            if ( ighostlev(i).ge.minghostlev_s ) then
!               iglobnum(i) = 0
!            end if
!         end do

!        compute number of active non-ghost cells
         num = count(iglobnum == 1)
!         write(6,*) 'my_rank', my_rank, 'num=', num, 'numghosts_sall=', numghost_sall

!        communicate active non-ghost cell numbers

         if (jatime == 1) call starttimer(IMPICOMM)
         call mpi_allgather(num, 1, MPI_INTEGER, numglobcells, 1, MPI_INTEGER, DFM_COMM_DFMWORLD, ierror)
         if (jatime == 1) call stoptimer(IMPICOMM)

!        compute global cell numbers for own non-ghost cells
         num = 0
         if (my_rank > 0) then
            num = sum(numglobcells(0:my_rank - 1))
         end if
      else ! jampi.eq.0
         numglobcells(0) = numactive
         num = 0
      end if

      do i = 1, numactive
         n = iactive(i)
         if (iglobnum(n) /= 0) then
            num = num + 1
            iglobnum(n) = num
         end if
      end do

      if (jampi == 1) then
!        update global ghost-cell numbers
         dum = dble(iglobnum)
         if (jatime == 1) call starttimer(IMPICOMM)
         !call update_ghost(dum,ierror)
         if (jampi == 1) then
            if (jaoverlap == 0) then
               call update_ghosts(ITYPE_Sall, 1, Ndx, dum, ierror)
            else
               call update_ghosts(ITYPE_Snonoverlap, 1, Ndx, dum, ierror)
            end if
         end if
         if (jatime == 1) call stoptimer(IMPICOMM)

         iglobnum = int(dum)

!!        fill in global cell number of masked cells
!         do i=1,Ndx
!            if ( iglobnum(i).eq.0 ) then
!               iglobnum(i) = int(dum(i))
!            end if
!         end do
      end if

!      open(newunit=lunfil,file='globalnrs_'//sdmn//'.xyz')
!      do i=1,Ndx
!         write(lunfil,"(2E15.7, I7)") xz(i), yz(i), iglobnum(i)
!      end do
!      close(lunfil)

      ierror = 0
1234  continue

!     deallocate
      if (allocated(dum)) deallocate (dum)

#endif

      return
   end subroutine get_global_numbers

!!> reduce time step
!   subroutine reduce_dt()
!      use m_flowtimes, only: dts, dti
!
!      implicit none
!
!      call reduce_double_min(dts)
!
!      return
!   end subroutine reduce_dt

!> reduce a double, take global min
   subroutine reduce_double_min(var)
#ifdef HAVE_MPI
      use mpi
#endif

      implicit none

      double precision, intent(inout) :: var !< variable
#ifdef HAVE_MPI

      double precision :: var_all

      integer :: ierror

      call mpi_allreduce(var, var_all, 1, mpi_double_precision, mpi_min, DFM_COMM_DFMWORLD, ierror)
      var = var_all
#endif

      return
   end subroutine reduce_double_min

!> reduce a double, take global max
   subroutine reduce_double_max(var)
#ifdef HAVE_MPI
      use mpi
#endif

      implicit none

      double precision, intent(inout) :: var !< variable
#ifdef HAVE_MPI

      double precision :: var_all

      integer :: ierror

      call mpi_allreduce(var, var_all, 1, mpi_double_precision, mpi_max, DFM_COMM_DFMWORLD, ierror)
      var = var_all
#endif

      return
   end subroutine reduce_double_max

!> reduce a double, take global sum
   subroutine reduce_double_sum(N, varin, varout)
#ifdef HAVE_MPI
      use mpi
#endif

      implicit none

      integer, intent(in) :: N !< array size
      double precision, dimension(N), intent(in) :: varin !< variable
      double precision, dimension(N), intent(out) :: varout !< reduced variable
#ifdef HAVE_MPI

      integer :: ierror

!      write(6,*) 'my_rank=', my_rank, 'N=', N

      call mpi_allreduce(varin, varout, N, mpi_double_precision, mpi_sum, DFM_COMM_DFMWORLD, ierror)
#endif

      return
   end subroutine reduce_double_sum

!> reduce a double array, take global max
   subroutine reduce_double2_max(var1, var2)
#ifdef HAVE_MPI
      use mpi
#endif

      implicit none

      double precision, intent(inout) :: var1 !< variables
      double precision, intent(inout) :: var2 !< variables
#ifdef HAVE_MPI

      double precision, dimension(2) :: dum, var_all

      integer :: ierror

      dum = (/var1, var2/)
      call mpi_allreduce(dum, var_all, 2, mpi_double_precision, mpi_max, DFM_COMM_DFMWORLD, ierror)
      var1 = var_all(1)
      var2 = var_all(2)
#endif

      return
   end subroutine reduce_double2_max

!> reduce a double array, take global max
   subroutine reduce_double3_max(var1, var2, var3)
#ifdef HAVE_MPI
      use mpi
#endif

      implicit none

      double precision, intent(inout) :: var1 !< variables
      double precision, intent(inout) :: var2 !< variables
      double precision, intent(inout) :: var3 !< variables
#ifdef HAVE_MPI

      double precision, dimension(3) :: dum, var_all

      integer :: ierror

      dum = (/var1, var2, var3/)
      call mpi_allreduce(dum, var_all, 3, mpi_double_precision, mpi_max, DFM_COMM_DFMWORLD, ierror)
      var1 = var_all(1)
      var2 = var_all(2)
      var3 = var_all(3)
#endif

      return
   end subroutine reduce_double3_max

!> reduce an integer array, take global max
   subroutine reduce_int4_max(var1, var2, var3, var4)
#ifdef HAVE_MPI
      use mpi
#endif

      implicit none

      integer, intent(inout) :: var1 !< variables
      integer, intent(inout) :: var2 !< variables
      integer, intent(inout) :: var3 !< variables
      integer, intent(inout) :: var4 !< variables
#ifdef HAVE_MPI

      integer, dimension(4) :: dum, var_all

      integer :: ierror

      dum = (/var1, var2, var3, var4/)
      call mpi_allreduce(dum, var_all, 4, mpi_integer, mpi_max, DFM_COMM_DFMWORLD, ierror)
      var1 = var_all(1)
      var2 = var_all(2)
      var3 = var_all(3)
      var4 = var_all(4)
#endif

      return
   end subroutine reduce_int4_max

   !> reduce an integer, take global sum
   subroutine reduce_int_sum(varin, varout)
#ifdef HAVE_MPI
      use mpi
#endif

      implicit none

      integer, intent(in) :: varin
      integer, intent(out) :: varout

#ifdef HAVE_MPI
      integer :: ierror

      call mpi_allreduce(varin, varout, 1, mpi_integer, mpi_sum, DFM_COMM_DFMWORLD, ierror)
#endif

      return
   end subroutine reduce_int_sum

!> reduce an array of integers, take global sum
   subroutine reduce_intN_sum(N, varin, varout)
#ifdef HAVE_MPI
      use mpi
#endif

      implicit none

      integer, intent(in) :: N !< array size
      integer, dimension(N), intent(in) :: varin !< variable
      integer, dimension(N), intent(out) :: varout !< reduced variable
#ifdef HAVE_MPI

      integer :: ierror

!      write(6,*) 'my_rank=', my_rank, 'N=', N

      call mpi_allreduce(varin, varout, N, mpi_integer, mpi_sum, DFM_COMM_DFMWORLD, ierror)
#endif

      return
   end subroutine reduce_intN_sum

   !> for an array over integers, take global sum over all subdomains (not over the array itself)
   subroutine reduce_int_array_sum(N, var)
#ifdef HAVE_MPI
      use mpi
#endif

      implicit none

      integer, intent(in) :: N !< array size
      integer, dimension(N), intent(inout) :: var !< array with values to be summed over the subdomains (not an array summation)

      integer, dimension(N) :: dum

      integer :: ierror

#ifdef HAVE_MPI
      call MPI_allreduce(var, dum, N, mpi_integer, mpi_sum, DFM_COMM_DFMWORLD, ierror)
#endif
      var = dum
      return
   end subroutine reduce_int_array_sum

!> reduce key (also used for nonlin in setprofs1D)
!>   take maximum
   subroutine reduce_key(key)
#ifdef HAVE_MPI
      use mpi
#endif

      implicit none

      integer, intent(inout) :: key !< key
#ifdef HAVE_MPI

      integer :: key_all

      integer :: ierror
      if (jampi == 1) then
         !     take maximum key of all domains
         call mpi_allreduce(key, key_all, 1, mpi_integer, mpi_max, DFM_COMM_DFMWORLD, ierror)
         key = key_all
      else
         ! No reduction: key is the only key.
         ! NOTE: mpi_allreduce is unpredictable when dflowfm is a library (no exe), when jampi==0 (uninitialized result).
         continue
      end if

#endif

      return
   end subroutine reduce_key

!> sum at_all for q-boundaries
   subroutine reduce_at_all()
      use fm_external_forcings_data
#ifdef HAVE_MPI
      use mpi
#endif

      implicit none

      integer :: ierror

#ifdef HAVE_MPI
      call MPI_allreduce(at_all, at_sum, nqbnd, mpi_double_precision, mpi_sum, DFM_COMM_DFMWORLD, ierror)
      at_all = at_sum
#endif
      return
   end subroutine reduce_at_all

!> sum wwssav_all for q-boundaries
   subroutine reduce_wwssav_all()
      use fm_external_forcings_data
#ifdef HAVE_MPI
      use mpi
#endif

      implicit none

      integer :: ierror

#ifdef HAVE_MPI
      call MPI_allreduce(wwssav_all, wwssav_sum, 2 * nqbnd, mpi_double_precision, mpi_sum, DFM_COMM_DFMWORLD, ierror)
      wwssav_all = wwssav_sum
#endif
      return
   end subroutine reduce_wwssav_all

!> sum atqh_all for qh-boundaries
   subroutine reduce_atqh_all()
      use fm_external_forcings_data
#ifdef HAVE_MPI
      use mpi
#endif

      implicit none

      integer :: ierror

      if (nqhbnd == 0) then
         return
      end if
#ifdef HAVE_MPI
      call MPI_allreduce(atqh_all, atqh_sum, nqhbnd, mpi_double_precision, mpi_sum, DFM_COMM_DFMWORLD, ierror)
      atqh_all = atqh_sum
#endif
      return
   end subroutine reduce_atqh_all

   !> sum an array over all subdomains (not over the array itself)
   subroutine reduce_sum(N, var)
#ifdef HAVE_MPI
      use mpi
#endif

      implicit none

      integer, intent(in) :: N !< array size
      double precision, dimension(N), intent(inout) :: var !< array with values to be summed over the subdomains (not an array summation)

      double precision, dimension(N) :: dum

      integer :: ierror

#ifdef HAVE_MPI
      call MPI_allreduce(var, dum, N, mpi_double_precision, mpi_sum, DFM_COMM_DFMWORLD, ierror)
      var = dum
#endif
      return
   end subroutine reduce_sum

   !> take maximum integer over all subdomains
   subroutine reduce_int1_max(var)
#ifdef HAVE_MPI
      use mpi
#endif

      implicit none

      integer, intent(inout) :: var !< array with values to be summed over the subdomains (not an array summation)

      integer :: dum

      integer :: ierror

#ifdef HAVE_MPI
      call MPI_allreduce(var, dum, 1, mpi_integer, mpi_max, DFM_COMM_DFMWORLD, ierror)
      var = dum
#endif
      return
   end subroutine reduce_int1_max

   !> for an array over integers, take maximum over all subdomains (not over the array itself)
   subroutine reduce_int_max(N, var)
#ifdef HAVE_MPI
      use mpi
#endif

      implicit none

      integer, intent(in) :: N !< array size
      integer, dimension(N), intent(inout) :: var !< array with values to be summed over the subdomains (not an array summation)

      integer, dimension(N) :: dum

      integer :: ierror

#ifdef HAVE_MPI
      call MPI_allreduce(var, dum, N, mpi_integer, mpi_max, DFM_COMM_DFMWORLD, ierror)
      var = dum
#endif
      return
   end subroutine reduce_int_max

!> reduce an integer, take global min
   subroutine reduce_int_min(var)
#ifdef HAVE_MPI
      use mpi
#endif
      implicit none

      integer, intent(inout) :: var !< array with values to be summed over the subdomains (not an array summation)

      integer :: dum

      integer :: ierror

#ifdef HAVE_MPI
      call MPI_allreduce(var, dum, 1, mpi_integer, mpi_min, DFM_COMM_DFMWORLD, ierror)
      var = dum
#endif
      return
   end subroutine reduce_int_min

!> reduce observation stations
!>   in principle only one subdomain may own the observation station
!>   however, for observation stations that are on equal distance from the
!>     two nearest flow nodes, and both flow nodes are on either side
!>     of a subdomain interface, it can happen that different flow nodes
!>     are selected in different subdomains and consequently:
!>       either no or two subdomains are owner of the observation station
!>     in that case: use flow node in subdomain with lowest number
!>
!>   we give the preference to the following order
!>     inside cell in own domain:    dist=0d0
!>     inside cell in other domain:  dist=DPENALTY
!>     outside cell in own domain:   dist=DPENALTY + distance
!>     outside cell in other domain: dist=DREJECT (>DPENALTY) never
!>   reduce: take global cell with lowest dist
   subroutine reduce_kobs(N, kobs, xobs, yobs, jaoutside)
      use m_flowgeom, only: xz, yz, nd
      use unstruc_messages
      use geometry_module, only: pinpok, dbdistance
      use m_missing, only: jins, dmiss
      use m_sferic, only: jsferic, jasfer3D
#ifdef HAVE_MPI
      use mpi
#endif

      implicit none

      integer, intent(in) :: N !< number of observation stations
      integer, dimension(N), intent(inout) :: kobs !< observation station flow_node numbers, >0: in own subdomain, -1: in other subdomain, 0: not found in any subdomain
      double precision, dimension(N), intent(in) :: xobs, yobs !< observation station coordinates
      integer, intent(in) :: jaoutside !< allow outside cells (for 1D) (1) or not (0)

#ifdef HAVE_MPI
      double precision, dimension(:), allocatable :: dist ! distances from flow nodes to observation stations
      double precision, dimension(:, :), allocatable :: dist_all

      double precision :: xp, yp

      integer :: i, other_domain, in, k1, ierror

      double precision, parameter :: DPENALTY = 1d10 ! should be smaller than DREJECT
      double precision, parameter :: DREJECT = 2d99 ! should be larger than DPENALTY

      if (N < 1) return

      allocate (dist(N))
      dist = DREJECT

      allocate (dist_all(N, 0:ndomains - 1))

!     set distances to observation stations
      do i = 1, N
         k1 = kobs(i)
         if (k1 == 0) cycle

!        check if the observation station is inside the cell
         call pinpok(xobs(i), yobs(i), size(nd(k1)%x), nd(k1)%x, nd(k1)%y, in, jins, dmiss)

!        determine preference
         if (in == 1) then
            if (idomain(k1) == my_rank) then
               dist(i) = 0d0
            else
               dist(i) = DPENALTY
            end if
         else if (jaoutside == 1) then
            if (idomain(k1) == my_rank) then
               xp = xz(k1)
               yp = yz(k1)
               dist(i) = DPENALTY + dbdistance(xobs(i), yobs(i), xp, yp, jsferic, jasfer3D, dmiss)
            else
               dist(i) = DREJECT
            end if
         else
            dist(i) = DREJECT
         end if
      end do

!      BEGIN DEBUG
!       call MPI_barrier(DFM_COMM_DFMWORLD,ierror)
!       do idmn=0,ndomains-1
!          if ( idmn.eq.my_rank) then
!             open(newunit=lunfil,file='deleteme.txt', access='append')
!
!             if ( my_rank.eq.0 ) then
!                write(lunfil,"('reduce_kobs')")
!                write(lunfil,"('before reduce')")
!             end if
!
!             write(lunfil,"('my_rank=',I0)") idmn
!             do i=1,N
!                write(lunfil,"(I4, I8, E17.4)") i, kobs(i), dist(i)
!             end do
!
!             close(lunfil)
!          end if
!
!          call MPI_barrier(DFM_COMM_DFMWORLD,ierror)
!       end do
!      END DEBUG

!     globally reduce
      call mpi_allgather(dist, N, MPI_DOUBLE_PRECISION, dist_all, N, MPI_DOUBLE_PRECISION, DFM_COMM_DFMWORLD, ierror)

!     select domain
      do i = 1, N
         k1 = 0
         dist(i) = DREJECT

!        compare distance with distances in other subdomains
         do other_domain = 0, ndomains - 1
            if (dist_all(i, other_domain) == DREJECT) then
               cycle
            else if (dist_all(i, other_domain) < dist(i) .or. dist(i) == DREJECT) then
               if (other_domain == my_rank) then
                  k1 = kobs(i) ! use value in this subdomain
               else
                  k1 = -1 ! in another subdomain
               end if
               dist(i) = dist_all(i, other_domain)
            end if
         end do

!        fill reduced value
         kobs(i) = k1
      end do

!     safety: check uniqueness
      dist = 0d0
      do i = 1, N
         if (kobs(i) > 0) then
            dist(i) = 1d0
         end if
      end do
      call mpi_allreduce(dist, dist_all, N, MPI_DOUBLE_PRECISION, MPI_SUM, DFM_COMM_DFMWORLD, ierror) ! re-use (part of) dist_all
      do i = 1, N
         if (dist_all(i, 0) > 1d0) then
            call mess(LEVEL_ERROR, 'reduce_kobs: non-unique observation station(s)')
         end if
      end do

!      BEGIN DEBUG
!       call MPI_barrier(DFM_COMM_DFMWORLD,ierror)
!       do idmn=0,ndomains-1
!          if ( idmn.eq.my_rank) then
!             open(newunit=lunfil,file='deleteme.txt', access='append')
!
!             if ( my_rank.eq.0 ) then
!                write(lunfil,"('after reduce')")
!             end if
!
!             write(lunfil,"('my_rank=',I0)") idmn
!             do i=1,N
!                write(lunfil,"(I4, I8, 2E17.4)") i, kobs(i), dist(i), dist_all(i)
!             end do
!
!             close(lunfil)
!          end if
!
!          call MPI_barrier(DFM_COMM_DFMWORLD,ierror)
!       end do
!      END DEBUG

      if (allocated(dist)) deallocate (dist)
      if (allocated(dist_all)) deallocate (dist_all)

#endif
      return
   end subroutine reduce_kobs

!> reduce outputted values at observation stations
!! NOTE: It seems that, now that we reduce the statistical output before writing, this routine is
!!       only needed to maintain functionality in unstruc_bmi/get_compound_field
   subroutine reduce_valobs(numvals, numobs, valobs)
      use m_missing
#ifdef HAVE_MPI
      use mpi
#endif

      implicit none

      integer, intent(in) :: numvals !< number of values
      integer, intent(in) :: numobs !< number of observation stations
      double precision, dimension(numobs, numvals), intent(inout) :: valobs !< values at obervations stations to be output.

      double precision, parameter :: dsmall = -huge(1d0)
      integer :: iobs, ival
      integer :: ierror

#ifdef HAVE_MPI

!     disable observation stations with missing values in this domain
      do iobs = 1, numobs
         do ival = 1, numvals
            if (valobs(iobs, ival) == DMISS) then
               valobs(iobs, ival) = dsmall
            end if
         end do
      end do

      call MPI_allreduce(MPI_in_place, valobs, numobs * numvals, mpi_double_precision, mpi_max, DFM_COMM_DFMWORLD, ierror)

!     set values of observation stations that were not found in any subdomain
      do iobs = 1, numobs
         do ival = 1, numvals
            if (valobs(iobs, ival) == dsmall) then ! safety, check all vals until not found (not necessary)
               valobs(iobs, ival) = DMISS
            end if
         end do
      end do
!      write(6,"(I4, ':', E12.5)") my_rank, valobs(10,1)

#endif
      return
   end subroutine reduce_valobs

   !> Reduce the statistical output values to MPI root process #0.
   subroutine reduce_statistical_output(output_set)
      use m_statistical_output_types, only: t_output_variable_set
      use m_missing
      use m_alloc, only: realloc
#ifdef HAVE_MPI
      use mpi
#endif

      type(t_output_variable_set), intent(inout) :: output_set !< Output set that we wish to update.

      double precision, parameter :: dsmall = -huge(1d0)
      integer :: i_stat, i_loc
      double precision, pointer :: stat_output(:) !< pointer to statistical output data array that is to be written to the Netcdf file after reduction across partitions.
      double precision, allocatable :: send_buffer(:) !< send buffer for mpi reduction because MPI_IN_PLACE does not work for unknown reasons.
      integer :: ierr

#ifdef HAVE_MPI
      do i_stat = 1, output_set%count

         stat_output => output_set%statout(i_stat)%stat_output

         ! Set values for locations outside this partition to -huge so mpi_max will work
         send_buffer = stat_output ! initial copy of local output data into the send buffer
         do i_loc = 1, size(stat_output)
            if (stat_output(i_loc) == dmiss) then
               send_buffer(i_loc) = dsmall ! set missing values to dsmall to enable mpi_max
            end if
         end do

         ! reduce with stat_output as receive buffer
         call MPI_reduce(send_buffer, stat_output, size(stat_output), mpi_double_precision, mpi_max, 0, DFM_COMM_DFMWORLD, ierr)

         ! Set values for locations outside this partition back to DMISS
         do i_loc = 1, size(stat_output)
            if (stat_output(i_loc) == dsmall) then
               stat_output(i_loc) = dmiss
            end if
         end do

      end do
#endif

   end subroutine reduce_statistical_output

!> reduce source-sinks
!>   it is assumed that the source-sinks are unique
   subroutine reduce_srsn(numvals, numsrc, srsn)
      use m_missing
#ifdef HAVE_MPI
      use mpi
#endif

      implicit none

      integer, intent(in) :: numvals, numsrc !< number of sources/sinks
      double precision srsn(numvals, numsrc) !< values associated with sources/sinks

      double precision, dimension(NUMVALS, numsrc) :: srsn_all

      integer :: ierror

#ifdef HAVE_MPI
      call MPI_allreduce(srsn, srsn_all, numsrc * NUMVALS, mpi_double_precision, mpi_sum, DFM_COMM_DFMWORLD, ierror)
      srsn = srsn_all
#endif
      return
   end subroutine reduce_srsn

!> reduce XLSAM in setprofs1d
   subroutine reduce_xlsam(Nproflocs, xlsam, distsam, iconnsam)
#ifdef HAVE_MPI
      use mpi
#endif

      implicit none

      integer, intent(in) :: Nproflocs !< number of profile locations (samples)
      double precision, dimension(:), intent(inout) :: xlsam !< branch-coordinate of sample points
      double precision, dimension(:), intent(inout) :: distsam !< distance from sample to branch
      integer, dimension(:), intent(inout) :: iconnsam !< connected-branch associated with sample

#ifdef HAVE_MPI
      double precision, dimension(:), allocatable :: dum

      integer, dimension(:), allocatable :: idum

      double precision, parameter :: dtol = 1d-8
      double precision, parameter :: DLARGE = 1d99
      double precision, parameter :: ILARGE = 10000

      integer :: i, ierror

      allocate (dum(Nproflocs))
      dum = 0d0

      allocate (idum(Nproflocs))
      idum = 0d0

      call MPI_allreduce(distsam, dum, Nproflocs, MPI_DOUBLE_PRECISION, MPI_MIN, DFM_COMM_DFMWORLD, ierror)
      if (ierror /= 0) goto 1234

      do i = 1, Nproflocs
         if (distsam(i) > dum(i) + dtol) then
            xlsam(i) = DLARGE
            iconnsam(i) = ILARGE
         end if
      end do

      distsam = dum

      call MPI_allreduce(xlsam, dum, Nproflocs, MPI_DOUBLE_PRECISION, MPI_MIN, DFM_COMM_DFMWORLD, ierror)
      if (ierror /= 0) goto 1234

      call MPI_allreduce(iconnsam, idum, Nproflocs, MPI_INTEGER, MPI_MIN, DFM_COMM_DFMWORLD, ierror)
      if (ierror /= 0) goto 1234

      do i = 1, Nproflocs
         if (xlsam(i) == DLARGE) then
            xlsam(i) = dum(i)
            iconnsam(i) = idum(i)
         end if
      end do

1234  continue
      deallocate (dum)
      deallocate (idum)
#endif
      return
   end subroutine reduce_xlsam

!> Reduce (sum) the partial sum for each cross section across all MPI partitions.
   subroutine reduce_crs(resu, ncrs, numvals)
#ifdef HAVE_MPI
      use mpi
#endif
      implicit none

      integer, intent(in) :: ncrs !< number of cross-sections
      integer, intent(in) :: numvals !< which values to sum (1=discharge)
      double precision, dimension(numvals, ncrs), intent(inout) :: resu !< cross-section data, note: ncrs from module m_monitoring_crosssections

      double precision, dimension(:, :), allocatable :: resu_all

      integer :: ierror

#ifdef HAVE_MPI
!     allocate
      allocate (resu_all(numvals, ncrs))

      call mpi_allreduce(resu, resu_all, numvals * ncrs, mpi_double_precision, mpi_sum, DFM_COMM_DFMWORLD, ierror)

      resu = resu_all

!     deallocate
      if (allocated(resu_all)) deallocate (resu_all)
#endif
      return
   end subroutine reduce_crs

!> Reduce runup for each runup gauge across all MPI partitions.
!> Returns max across domains (1,:), and domain number of max (2,:)
   subroutine reduce_rug(resu, num_rugs)
#ifdef HAVE_MPI
      use mpi
#endif
      implicit none

      integer, intent(in) :: num_rugs !< number of gauges
      double precision, dimension(2, num_rugs), intent(inout) :: resu !< runup data
      double precision, dimension(:, :), allocatable :: resu_all

      integer :: ierror

#ifdef HAVE_MPI
!     allocate
      allocate (resu_all(2, num_rugs))
      resu_all = 0d0

      call mpi_allreduce(resu, resu_all, num_rugs, mpi_2double_precision, mpi_maxloc, DFM_COMM_DFMWORLD, ierror)
      if (ierror /= 0) then
         goto 1234
      end if
      resu = resu_all

!     deallocate
1234  continue
      if (allocated(resu_all)) deallocate (resu_all)
#endif
      return
   end subroutine reduce_rug

!> exclude water-level ghostnodes from solution vector via kfs
   subroutine partition_setkfs()
      use m_flowgeom, only: kfs, xz, yz, Ndxi, nd, wu
      use m_plotdots
      implicit none

      integer :: i
      integer :: k, L, LL

      if (jaoverlap == 0) then
         do i = 1, nghostlist_sall(ndomains - 1)
            kfs(ighostlist_sall(i)) = -abs(kfs(ighostlist_sall(i))) !0
         end do
      else
         do i = 1, nghostlist_snonoverlap(ndomains - 1)
            kfs(ighostlist_snonoverlap(i)) = -abs(kfs(ighostlist_snonoverlap(i))) ! 0
         end do
      end if

      return ! I do not understand the next code, switching it off did not alter results of my test computation, lets try it for the testbench

!     disable disabled ghostnodes
      numdots = 0
      klp: do k = 1, Ndxi
         do LL = 1, nd(k)%lnx
            L = abs(nd(k)%ln(LL))
            if (wu(L) /= 0d0) then
               cycle klp
            end if
         end do
         if (kfs(k) > 0) then
            numdots = numdots + 1
            call reallocdots(numdots)
            xdots(numdots) = xz(k)
            ydots(numdots) = yz(k)

            kfs(k) = 0
         end if
      end do klp

      return
   end subroutine partition_setkfs

!> finalize before exit
   subroutine partition_finalize()
#ifdef HAVE_MPI
      use mpi
#endif

      implicit none

      integer :: ierr
#ifdef HAVE_PETSC
      call stoppetsc()
#endif

#ifdef HAVE_PARMS
      call deallocparms()
#endif

#ifdef HAVE_MPI
      if (ja_mpi_init_by_fm == 1) then
         call mpi_finalize(ierr)
      end if
#endif
      return
   end subroutine partition_finalize

!> make global 1D netbranch numbering
!>    affects netbr, NRLB and K1BR
!>    uses:
!>       netbr, mxnetbr from network_data
!>       numranks
!>    note: partition_init not called, i.e. ndomains, ghostlists, sendlists unavailable
   subroutine global_netbranch_numbering()
      use network_data
      use m_missing
      use m_sferic, only: pi
      use geometry_module, only: dbdistance
#ifdef HAVE_MPI
      use mpi
#endif

      implicit none

      integer, dimension(:), allocatable :: iglobalbranch ! global branch number, dim(numnetbr)
      integer, dimension(:), allocatable :: numbranches ! number of branches per domain, dim(0:numranks-1)

#ifdef HAVE_MPI
      double precision, dimension(:, :), allocatable :: xyL_all ! center coordinates and angle of left  link of branch, dim(3,numallnetbr)
      double precision, dimension(:, :), allocatable :: xyR_all ! center coordinates and angle of right link of branch, dim(3,numallnetbr)
      double precision, dimension(:, :), allocatable :: xyL_loc, xyR_loc

      integer, dimension(:), allocatable :: ibr_glob_left ! global number of left  connected branch, dim(numallnetbr)
      integer, dimension(:), allocatable :: ibr_glob_right ! global number of right connected branch, dim(numallnetbr)
      integer, dimension(:), allocatable :: Lother_left ! other link of left  connected branch, dim(numallnetbr)
      integer, dimension(:), allocatable :: Lother_right ! other link of right connected branch, dim(numallnetbr)
      integer, dimension(:), allocatable :: idum
      integer, dimension(:), allocatable :: inew ! new global branch number, dim(numallnetbr)
      integer, dimension(:), allocatable :: iorient ! branch orientation, left-right (1), or reverse/right-left (0), dim(numallnetbr)
      integer, dimension(:), allocatable :: iordened_branches ! ordened branched, dim(numallnetbr)
      integer, dimension(:), allocatable :: ipoint ! pointer in iordened_branches list, dim(numallnetbr+1)

      double precision, dimension(:), allocatable :: dlL, dlR, dLtot ! left-part, right-part and total branch length
      double precision, dimension(:), allocatable :: doffset ! offset length of branch
      double precision, dimension(:), allocatable :: ddum

      integer :: numnetbr ! number of branches in this domain
      integer :: numallnetbr ! number of branches summed over all domains
      integer :: numnew ! new number of (connected) branches

      double precision :: xloc, yloc
      double precision :: dabsangle
      double precision :: dlength, dleft, dconnected

      integer :: iglobalbranch_first
      integer :: ibr, ibrr, idmn, i, iglob, k, N, num, L, LL, LR
      integer :: istart, iend, ioff, ibr_other, ibr_glob
      integer :: icount, Lconnect

      integer :: ierror ! error (1) or not (0)

      logical :: Lleftfound, Lrightfound

      double precision, external :: dlinklength

      double precision, parameter :: dtol = 1d-8

!     count the number of branches
      numnetbr = mxnetbr

!     allocate
      allocate (numbranches(0:numranks - 1))
      allocate (iglobalbranch(1:numnetbr))
      allocate (doffset(numnetbr))

!     make global branch numbering
      call MPI_allgather(numnetbr, 1, MPI_INTEGER, numbranches, 1, MPI_INTEGER, DFM_COMM_DFMWORLD, ierror)
      if (ierror /= 0) goto 1234

      numallnetbr = sum(numbranches(0:numranks - 1))

      iglobalbranch_first = 1
      do idmn = 0, my_rank - 1
         iglobalbranch_first = iglobalbranch_first + numbranches(idmn)
      end do

      do ibr = 1, numnetbr
         iglobalbranch(ibr) = ibr + iglobalbranch_first - 1
      end do

!     make the global left/right coordinate lists
      allocate (xyL_loc(3, numallnetbr), xyR_loc(3, numallnetbr))
      allocate (xyL_all(3, numallnetbr), xyR_all(3, numallnetbr))
      allocate (ibr_glob_left(numallnetbr), ibr_glob_right(numallnetbr))
      allocate (Lother_left(numallnetbr), Lother_right(numallnetbr))
      allocate (dlL(numallnetbr), dlR(numallnetbr), dltot(numallnetbr))
      allocate (idum(numallnetbr))
      allocate (ddum(numallnetbr))
      allocate (inew(numallnetbr))
      allocate (iorient(numallnetbr))
      allocate (iordened_branches(numallnetbr))
      allocate (ipoint(numallnetbr + 1))

      xyL_loc = 0d0
      xyR_loc = 0d0

      do ibr = 1, numnetbr
         iglob = ibr + iglobalbranch_first - 1
         num = netbr(ibr)%NX
         LL = netbr(ibr)%LN(1)
         LR = netbr(ibr)%LN(num)
         xyL_loc(1, iglob) = 0.5d0 * (xk(kn(1, abs(LL))) + xk(kn(2, abs(LL))))
         xyL_loc(2, iglob) = 0.5d0 * (yk(kn(1, abs(LL))) + yk(kn(2, abs(LL))))
         xyR_loc(1, iglob) = 0.5d0 * (xk(kn(1, abs(LR))) + xk(kn(2, abs(LR))))
         xyR_loc(2, iglob) = 0.5d0 * (yk(kn(1, abs(LR))) + yk(kn(2, abs(LR))))

         xyL_loc(3, iglob) = dLinkangle(LL)
         xyR_loc(3, iglob) = dLinkangle(LR)
      end do

!     gather information from all domains
!        note that this can also be achieved by using mpi_allgatherv,
!        but now we do not need to use an offset in the global array
      call MPI_allreduce(xyL_loc, xyL_all, 3 * numallnetbr, MPI_DOUBLE_PRECISION, MPI_SUM, DFM_COMM_DFMWORLD, ierror)
      if (ierror /= 0) goto 1234
      call MPI_allreduce(xyR_loc, xyR_all, 3 * numallnetbr, MPI_DOUBLE_PRECISION, MPI_SUM, DFM_COMM_DFMWORLD, ierror)
      if (ierror /= 0) goto 1234

!     find the global branch connectivity
      call find_branch_conn(ibr_glob_left, ibr_glob_right, Lother_left, Lother_right, ierror)
      if (ierror /= 0) goto 1234

!     connect branches and make new global branch numbering
      inew = 0
      numnew = 0
      dlL = 0d0
      dlR = 0d0
      dltot = 0d0
      iorient = 0
      ipoint(1) = 1
      do ibr = 1, numallnetbr
         if (inew(ibr) /= 0) cycle ! branch has already new global number assigned
         numnew = numnew + 1
         idum = 0
         icount = 0
!        walk left
         call connect_branches(ibr, numnew, 0, icount)
!        reverse order of branches
         idum(1:icount) = idum(icount:1:-1)
         iorient(idum(1:icount)) = 1 - iorient(idum(1:icount)) ! walking in opposite direction
!        enable this branch again by removing starting (now last) branch
         inew(ibr) = 0
         icount = icount - 1
!        walk right
         call connect_branches(ibr, numnew, 1, icount)

         if (my_rank == 0) then
            write (6, "(I4, ':', 100I4)") numnew, (idum(i), i=1, icount)
            write (6, "(I4, ':', 100I4)") numnew, (iorient(abs(idum(i))), i=1, icount)
         end if

!        add to the ordered branch list
         ipoint(numnew + 1) = ipoint(numnew) + icount
         iordened_branches(ipoint(numnew):ipoint(numnew + 1) - 1) = idum(1:icount)

!        measure lengths in the connected branch
         do i = 1, icount
            ibr_glob = abs(idum(i))
            ibrr = ibr_glob - iglobalbranch_first + 1

            if (ibrr < 1 .or. ibrr > numnetbr) cycle ! local branches only

            N = netbr(ibrr)%NX
!           check orientation of this branch
            if (iorient(ibr_glob) /= 1) then
!              swap orientation
               call swap_branch(ibrr)
            end if

!           find the link that is connected to the start/end of the other link
            if (i > 1) then
               Lconnect = Lother_right(abs(idum(i - 1))) ! we need the right connection of the previous branch, since the orientation is always from left to right
            else
               Lconnect = 0
            end if

            dleft = 0d0
            dlength = 0d0
            do k = 1, N
               L = netbr(ibrr)%LN(k)
               dlength = dlength + dlinklength(L)
               if (abs(L) == abs(Lconnect)) then ! offset link
                  dleft = dlength
               end if
            end do

            dlL(ibr_glob) = dleft
            dltot(ibr_glob) = dlength
!            dlR(ibr_glob) = dlength-dleft
         end do

      end do

!     gather information from all domains
      call MPI_allreduce(dlL, ddum, numallnetbr, MPI_DOUBLE_PRECISION, MPI_SUM, DFM_COMM_DFMWORLD, ierror)
      if (ierror /= 0) goto 1234
      dlL = ddum

      call MPI_allreduce(dltot, ddum, numallnetbr, MPI_DOUBLE_PRECISION, MPI_SUM, DFM_COMM_DFMWORLD, ierror)
      if (ierror /= 0) goto 1234
      dltot = ddum

      if (my_rank == 0) write (6, *) (dlL(i), i=1, numallnetbr)
      if (my_rank == 0) write (6, *) (dltot(i), i=1, numallnetbr)

      dlR = dltot - dlL

!     compute the offset lengths and fill local branch properties
      do i = 1, numnew
         dconnected = 0d0
         do k = ipoint(i), ipoint(i + 1) - 1
            ibr_glob = abs(iordened_branches(k))
            ibrr = ibr_glob - iglobalbranch_first + 1

!           fill local branch properties
            if (ibrr >= 1 .and. ibrr <= numnetbr) then
               netbr(ibrr)%iconn = i
               netbr(ibrr)%doff = dconnected - dlL(ibr_glob)
            end if

!           add to length of connected branch
            dconnected = dconnected + dltot(ibr_glob) - dlL(ibr_glob)
         end do
      end do

1234  continue

!     deallocate
      if (allocated(numbranches)) deallocate (numbranches)
      if (allocated(iglobalbranch)) deallocate (iglobalbranch)
      if (allocated(xyL_loc)) deallocate (xyL_loc)
      if (allocated(xyR_loc)) deallocate (xyR_loc)
      if (allocated(xyL_all)) deallocate (xyL_all)
      if (allocated(xyR_all)) deallocate (xyR_all)
      if (allocated(ibr_glob_left)) deallocate (ibr_glob_left)
      if (allocated(ibr_glob_right)) deallocate (ibr_glob_right)
      if (allocated(idum)) deallocate (idum)
      if (allocated(inew)) deallocate (inew)
      if (allocated(Lother_left)) deallocate (Lother_left)
      if (allocated(Lother_right)) deallocate (Lother_right)
      if (allocated(dlL)) deallocate (dlL)
      if (allocated(dlR)) deallocate (dlR)
      if (allocated(dltot)) deallocate (dltot)
      if (allocated(doffset)) deallocate (doffset)
      if (allocated(ddum)) deallocate (ddum)
      if (allocated(iorient)) deallocate (iorient)
      if (allocated(iordened_branches)) deallocate (iordened_branches)
      if (allocated(ipoint)) deallocate (ipoint)

      return

   contains

!>    reverse order of branch
!>       affects netbr(ibr), NRLB and K1BR
      subroutine swap_branch(ibr)
         implicit none

         integer, intent(in) :: ibr

         integer :: i, ibr_glob, idum, La, Ldum, Lnew, N

         ibr_glob = ibr + iglobalbranch_first - 1

         N = netbr(ibr)%NX
         netbr(ibr)%LN(1:N) = -netbr(ibr)%LN(N:1:-1)

         Ldum = Lother_left(ibr_glob)
         Lother_left(ibr_glob) = Lother_right(ibr_glob)
         Lother_right(ibr_glob) = Ldum

         Ldum = ibr_glob_left(ibr_glob)
         ibr_glob_left(ibr_glob) = ibr_glob_right(ibr_glob)
         ibr_glob_right(ibr_glob) = Ldum

         iorient(ibr_glob) = 1 - iorient(ibr_glob)

         do i = 1, N / 2
            Ldum = abs(netbr(ibr)%LN(i))
            Lnew = abs(netbr(ibr)%LN(N - i + 1))
            idum = NRLB(Ldum)
            NRLB(Ldum) = NRLB(Lnew)
            NRLB(Lnew) = idum
         end do

         do i = 1, N
            Ldum = netbr(ibr)%LN(i)
            La = abs(Ldum)
            if (Ldum > 0) then
               K1BR(NRLB(La)) = kn(1, La)
            else
               K1BR(NRLB(La)) = kn(2, La)
            end if
         end do

         return
      end subroutine swap_branch

!>    make connected branch numbering
!>       sets: inew, idum, iorient
!>       uses: ibr_glob_left, ibr_glob_right
      recursive subroutine connect_branches(ibr, numcur, idir, icount)
         implicit none

         integer, intent(in) :: ibr !< global branch number, can be <0 to indicate that the orientation has switched
         integer, intent(in) :: numcur !< current new branch number
         integer, intent(in) :: idir !< walk right (1) or left (0)
         integer, intent(inout) :: icount !< counter of branch in path

         integer :: inext, ibra

         ibra = abs(ibr)

         if (inew(ibra) /= 0) return ! branch has already new global number assigned

         inew(ibra) = numcur

         icount = icount + 1
         idum(icount) = ibra
         iorient(ibra) = idir

         if (idir == 1) then ! walk right
            inext = ibr_glob_right(ibra)
         else ! walk left
            inext = ibr_glob_left(ibra)
         end if

         if (inext > 0) then
            call connect_branches(inext, numcur, idir, icount)
         else if (inext < 0) then ! swap orientation
            call connect_branches(inext, numcur, 1 - idir, icount)
         end if

         return
      end subroutine connect_branches

!> find the global branch connectivity
!>    sets ibr_glob_left, ibr_glob_right, Lother_left, Lother_right
      subroutine find_branch_conn(ibr_glob_left, ibr_glob_right, Lother_left, Lother_right, ierror)

         use m_missing, only: dmiss
         use m_sferic, only: jsferic, jasfer3D
         use geometry_module, only: dbdistance

         implicit none

         integer, dimension(:), intent(out) :: ibr_glob_left, ibr_glob_right, Lother_left, Lother_right
         integer, intent(out) :: ierror !< error (1) or not (0)

         integer :: La
         integer :: ibr, inext

!     see if any of the left/right links of other domain's branches is in a netbranch in this domain
         ibr_glob_left = -huge(0)
         ibr_glob_right = -huge(0)
         Lother_left = -huge(0)
         Lother_right = -huge(0)
         ioff = 0
         do idmn = 0, numranks - 1
            istart = 1 + ioff ! global branch number of first branch in other domain
            iend = numbranches(idmn) + ioff ! global branch number of last  branch in other domain

            if (idmn /= my_rank) then
               do ibr_other = istart, iend ! global branch number
                  !              compare start(called left) and end (called right) of branch in other domain with branches in own domain
                  Lleftfound = .false.
                  Lrightfound = .false.

                  do ibr = 1, numnetbr ! local branch number
                     ibr_glob = ibr + iglobalbranch_first - 1 ! global branch number
                     num = netbr(ibr)%NX
                     do i = 1, num
                        L = netbr(ibr)%LN(i)
                        La = abs(L)
                        xloc = 0.5d0 * (xk(kn(1, La)) + xk(kn(2, La)))
                        yloc = 0.5d0 * (yk(kn(1, La)) + yk(kn(2, La)))
                        if (dbdistance(xloc, yloc, xyL_all(1, ibr_other), xyL_all(2, ibr_other), jsferic, jasfer3D, dmiss) < dtol) then
                           !                       left match found
                           Lleftfound = .true.
                           Lother_left(ibr_other) = L

                           !                       check orientation
                           dabsangle = abs(dLinkangle(L) - xyL_all(3, ibr_other))
                           if (dabsangle < dtol) then
                              !                          same orientation
                              ibr_glob_left(ibr_other) = ibr_glob
                           else if (abs(dabsangle - pi) < dtol) then
                              !                          opposite orientation
                              ibr_glob_left(ibr_other) = -ibr_glob
                           else
                              !                          orientation error
                              call qnerror('global_netbranch_numbering: orientation error', ' ', ' ')
                              goto 1234
                           end if
                        end if

                        if (dbdistance(xloc, yloc, xyR_all(1, ibr_other), xyR_all(2, ibr_other), jsferic, jasfer3D, dmiss) < dtol) then
                           !                       right match found
                           Lrightfound = .true.
                           Lother_right(ibr_other) = L

                           !                       check orientation
                           dabsangle = abs(dLinkangle(L) - xyR_all(3, ibr_other))
                           if (dabsangle < dtol) then
                              !                          same orientation
                              ibr_glob_right(ibr_other) = ibr_glob
                           else if (abs(dabsangle - pi) < dtol) then
                              !                          opposite orientation
                              write (6, *) 'ibr_other=', ibr_other
                              write (6, *) 'ibr=', ibr
                              write (6, *) 'angle_other=', xyR_all(3, ibr_other)
                              write (6, *) 'angle=', dLinkangle(L)
                              ibr_glob_right(ibr_other) = -ibr_glob
                           else
                              !                          orientation error
                              call qnerror('global_netbranch_numbering: orientation error', ' ', ' ')
                              goto 1234
                           end if
                        end if

                        if (Lleftfound .and. Lrightfound) exit
                     end do
                  end do
               end do
            end if

            ioff = iend
         end do

!        gather information from all domains
         call MPI_allreduce(ibr_glob_left, idum, numallnetbr, MPI_INTEGER, MPI_MAX, DFM_COMM_DFMWORLD, ierror)
         if (ierror /= 0) goto 1234
         ibr_glob_left = idum

         call MPI_allreduce(ibr_glob_right, idum, numallnetbr, MPI_INTEGER, MPI_MAX, DFM_COMM_DFMWORLD, ierror)
         if (ierror /= 0) goto 1234
         ibr_glob_right = idum

         call MPI_allreduce(Lother_left, idum, numallnetbr, MPI_INTEGER, MPI_MAX, DFM_COMM_DFMWORLD, ierror)
         if (ierror /= 0) goto 1234
         Lother_left = idum

         call MPI_allreduce(Lother_right, idum, numallnetbr, MPI_INTEGER, MPI_MAX, DFM_COMM_DFMWORLD, ierror)
         if (ierror /= 0) goto 1234
         Lother_right = idum

         where (ibr_glob_left == -huge(0))
            ibr_glob_left = 0
         end where
         where (ibr_glob_right == -huge(0))
            ibr_glob_right = 0
         end where
         where (Lother_left == -huge(0))
            Lother_left = 0
         end where
         where (Lother_right == -huge(0))
            Lother_right = 0
         end where

         !     check for mutual connectivity
         do ibr = 1, numallnetbr
            !        check right
            inext = ibr_glob_right(ibr)
            if (inext > 0) then
               !           same orientation: should connect with next left
               if (ibr_glob_left(inext) /= ibr) then
                  !              deactivate connection
                  ibr_glob_right(ibr) = 0
               end if
            else if (inext < 0) then
               !           opposite orientation: should connect with next right
               if (ibr_glob_right(-inext) /= -ibr) then
                  ibr_glob_right(ibr) = 0
               end if
            end if

            !        check left
            inext = ibr_glob_left(ibr)
            if (inext > 0) then
               !           same orientation: should connect with next right
               if (ibr_glob_right(inext) /= ibr) then
                  !              deactivate connection
                  ibr_glob_left(ibr) = 0
               end if
            else if (inext < 0) then
               !           opposite orientation: should connect with next left
               if (ibr_glob_left(-inext) /= -ibr) then
                  ibr_glob_left(ibr) = 0
               end if
            end if
         end do

         ierror = 0
1234     continue

         return
      end subroutine find_branch_conn

#endif

   end subroutine global_netbranch_numbering

   subroutine diff_ghosts(itype, var)
      use m_flowgeom, only: Lnxi, ln
      implicit none

      integer, intent(in) :: itype

      double precision, dimension(:), intent(inout) :: var

      double precision, dimension(:), allocatable :: dum

      integer :: i, k, L, N
      integer :: ierr

      ierr = 1

      write (6, *) 'XXX'

      N = ubound(var, 1)
      if (N < 1) goto 1234

      allocate (dum(N))
      dum = var

      if (itype == ITYPE_U) then
         call update_ghosts(itype, 1, N, dum, ierr)
         do L = 1, Lnxi
            if (idomain(ln(1, L)) == my_rank .or. idomain(ln(2, L)) == my_rank) then
               if (abs(dum(L) - var(L)) > 1d-12) then
                  write (6, *) 'XXX: ', my_rank, L, dum(L), var(L), dum(L) - var(L)
               end if
            end if
         end do
      else if (itype == ITYPE_S .or. itype == ITYPE_Sall) then
         call update_ghosts(itype, 1, N, dum, ierr)
         do i = 1, nghostlist_sall(ndomains - 1)
            k = ighostlist_sall(i)
            if (ighostlev_cellbased(k) > 3 .or. ighostlev_nodebased(k) > 2) cycle
            if (abs(dum(k) - var(k)) > 1d-12) then
               write (6, *) 'XXX: ', my_rank, k, dum(k), var(k), dum(k) - var(k)
            end if
         end do
      end if

      ierr = 0

1234  continue

      if (allocated(dum)) deallocate (dum)

      return
   end subroutine

   !> Generic function for weighted average on a quantities defined on cells or links, also applying optional filters
   !> optional firstFilter is defined on a global weights array [1, ncells/nlinks]
   !> optional secondFilter is defined on a local array [1,size(secondFilter)]
   !> works also across multiple MPI ranks
   function getAverageQuantityFromLinks(startLinks, endLinks, weights, indsWeight, quantity, indsQuantity, results, quantityType, &
                                        firstFilter, firstFilterValue, secondFilter, secondFilterValue) result(ierr)

      use mpi
      use fm_external_forcings_data
      use m_timer

      !inputs
      integer, intent(in), dimension(:) :: startLinks !< start indexes [1,nsegments]
      integer, intent(in), dimension(:) :: endLinks !< end   indexes [1,nsegments]
      double precision, intent(in), dimension(:) :: weights !< global weights array
      integer, intent(in), dimension(:) :: indsWeight !< local indexes on global weights array
      double precision, intent(in), dimension(:) :: quantity !< global quantity array
      integer, intent(in), dimension(:) :: indsQuantity !< local indexes on global quantity array
      integer, intent(in) :: quantityType !< The type of quantity: 0 scalar, 1 array (edge orientation matters)

      double precision, intent(in), dimension(:), optional :: firstFilter !< filter to apply on the global weights array
      double precision, intent(in), optional :: firstFilterValue !< value to activate the first filter (activated if larger than filter value)

      integer, intent(in), dimension(:), optional :: secondFilter !< filter to apply on the local weights array
      integer, intent(in), optional :: secondFilterValue !< value to activate the second filter (activated if larger than filter value)
      !locals
      integer :: ns, nsegments, nl, indWeight, indQuantity
      double precision :: sumQuantitiesByWeight, sumWeights
      double precision :: quantitiesByWeight, weight
      double precision, allocatable :: resultsSum(:, :)

      !outputs
      double precision, dimension(:, :), intent(inout) :: results
      integer :: ierr

      ierr = 0
      nsegments = size(startLinks)
      allocate (resultsSum(2, nsegments))
      results = 0.0d0

      do ns = 1, nsegments

         sumQuantitiesByWeight = 0d0
         sumWeights = 0d0

         do nl = startLinks(ns), endLinks(ns)

            indWeight = abs(indsWeight(nl))
            indQuantity = abs(indsQuantity(nl))
            quantitiesByWeight = 0.0d0
            weight = 0.0d0

            if (jampi == 1) then
               ! Exclude ghost nodes
               if (idomain(indQuantity) /= my_rank) then
                  cycle
               end if
            end if

            ! weights are always positive
            weight = weights(indWeight)
            if (quantityType == 1) then
               if (indsQuantity(nl) < 0) then
                  quantitiesByWeight = -quantity(indQuantity) * weight
               else
                  quantitiesByWeight = quantity(indQuantity) * weight
               end if
            else
               quantitiesByWeight = quantity(indQuantity) * weight
            end if

            if (present(firstFilter) .and. present(firstFilterValue)) then
               if (firstFilter(indWeight) <= firstFilterValue) then
                  quantitiesByWeight = 0.0d0
                  weight = 0.0d0
               end if
            end if

            if (present(secondFilter) .and. present(secondFilterValue)) then
               if (secondFilter(nl) <= secondFilterValue) then
                  quantitiesByWeight = 0.0d0
                  weight = 0.0d0
               end if
            end if

            sumQuantitiesByWeight = sumQuantitiesByWeight + quantitiesByWeight
            sumWeights = sumWeights + weight

         end do
         results(1, ns) = sumQuantitiesByWeight
         results(2, ns) = sumWeights
      end do

      if (jampi == 1) then
         ! Here we reduce the results
         if (jatimer == 1) call starttimer(IMPIREDUCE)
#ifdef HAVE_MPI
         call MPI_allreduce(results, resultsSum, 2 * nsegments, mpi_double_precision, mpi_sum, DFM_COMM_DFMWORLD, ierr)
         results = resultsSum
#endif
         if (jatimer == 1) call stoptimer(IMPIREDUCE)
      end if

   end function getAverageQuantityFromLinks

   !> generate partitioning polygons
   ! Moved to module because of optional argument
   ! Adaptations from original version to fix coupling with SWAN:
   ! - myrank: write only polygon for points that are strictly in domain myrank (no ghostcell polygons)
   subroutine generate_partition_pol_from_idomain(ierror, myrank)
      use network_data
      use m_polygon
      use m_tpoly
      use m_sferic
      use unstruc_messages
      use gridoperations
      use m_copynetboundstopol
      implicit none

      integer, intent(out) :: ierror
      integer, optional, intent(in) :: myrank

      integer, dimension(:), allocatable :: lnn_sav

      integer :: icL, icR, idmn, L

      integer :: jabound
      integer :: myrank_
      integer :: nstart, nstop

      ierror = 1

      myrank_ = -1

      if (present(myrank)) then
         myrank_ = myrank
      end if

      if (myrank_ >= 0) then
         nstart = myrank_
         nstop = myrank_
      else
         nstart = 1
         nstop = Ndomains - 1
      end if

      if (netstat == NETSTAT_CELLS_DIRTY) then
         call findcells(0)
         call find1Dcells()

         call delete_dry_points_and_areas()
      end if

!     check for 1D cells (not supported)
      if (nump1d2d > nump) then
         call mess(LEVEL_WARN, 'generate_partition_pol_from_idomain: 1D not supported')
         goto 1234
      end if

!     allocate
      allocate (lnn_sav(numL))

!     store
      call savepol()

      do L = 1, numL
         lnn_sav(L) = lnn(L)
      end do

!     generate polygons
!        note: domain 0 is the default domain
!     clean up previous tpoly-type partitioning polygon
      call dealloc_tpoly(partition_pol)
      npartition_pol = 0 ! current number
      do idmn = nstart, nstop
         do L = numL1D + 1, numL ! only 2D supported
!           check if this netlink is a domain boundary
            jabound = 0 ! default
            icL = lne(1, L)
            icR = lne(2, L)
            if (lnn(L) == 1) then
               if (idomain(icL) == idmn) jabound = 1
            else if (lnn(L) == 2) then
               if ((idomain(icL) /= idomain(icR)) .and. &
                   ((idomain(icL) == idmn) .or. (idomain(icR) == idmn))) then
                  jabound = 1
               end if
            end if

!           modify lnn
            if (jabound == 1) then
               lnn(L) = 1
            else
               lnn(L) = 2
            end if
         end do

!        delete current polygon (if applicable)
         call delpol()

!        copy netbounds based on modified lnn to polygon
         call copynetboundstopol(0, 0, 0, 1)

!        set polygon nodal value to domain number
         zpl(1:NPL) = dble(idmn)

!        add polygon to tpoly-type partitioning polygons
         call pol_to_tpoly(npartition_pol, partition_pol, keepExisting=.true.)

!        check number of partitioning polygons
!         if ( npartition_pol.ne.idmn ) then
!            call qnerror('generate_partition_pol_from_idomain: number of polygons and domains do not match', ' ', ' ')
!         end if

!         call cls1()
!         rlin(1:numL) = lnn(1:numL)
!         call teknetstuff(key)
!         call tekpolygon()
!         write(message,"('domain ', I)") idmn
!         call qnerror(trim(message), ' ', ' ')

!        restore lnn
         do L = 1, numL
            lnn(L) = lnn_sav(L)
         end do
      end do ! do idmn=0,Ndomains-1

!     delete polygon
      call delpol()

!     copy tpoly-type partition polygons to polygon
      call tpoly_to_pol(partition_pol)

!     fix polygon for spheric, periodic coordinates
!!!      call fix_global_polygons(0,1)

      ierror = 0

1234  continue

!     deallocate
      if (allocated(lnn_sav)) deallocate (lnn_sav)

      return
   end subroutine generate_partition_pol_from_idomain

!> Gathers integer data from all processes and delivers it to a speicified root process.
!! Note: the same number of data from each subdomain are sent.
   subroutine gather_int_data_mpi_same(ndata_send, data_send, ndata_gat, data_gat, ndata_recv, root, ierror)
#ifdef HAVE_MPI
      use mpi
#endif

      implicit none
      integer, intent(in) :: ndata_send !< Number of data in array data_send
      integer, dimension(ndata_send), intent(in) :: data_send !< Array of data on one subdomain to send
      integer, intent(in) :: ndata_gat !< Number of data in array data_gat
      integer, dimension(ndata_gat), intent(inout) :: data_gat !< Array of data gathered from all subdomains to receive
      integer, intent(in) :: ndata_recv !< Number of data to receive from each subdomain
      integer, intent(in) :: root !< Rank of receiving process
      integer, intent(inout) :: ierror !< Error index

      ierror = -1
#ifdef HAVE_MPI
      call mpi_gather(data_send, ndata_send, mpi_integer, data_gat, ndata_recv, mpi_integer, root, DFM_COMM_DFMWORLD, ierror)
#endif

   end subroutine gather_int_data_mpi_same

!> Gathers double precision data into specified locations from all processes in a group and delivers to a specified root process.
!! Note: Different numbers of data on different subdomains can be sent.
   subroutine gatherv_double_data_mpi_dif(ndata_send, data_send, ndata_gat, data_gat, ngroups, recvCount, displs, root, ierror)
#ifdef HAVE_MPI
      use mpi
#endif

      implicit none
      integer, intent(in) :: ndata_send !< Number of data in array data_send
      double precision, dimension(ndata_send), intent(in) :: data_send !< Array of data on one subdomain to send
      integer, intent(in) :: ndata_gat !< Number of data in array data_gat
      double precision, dimension(ndata_gat), intent(inout) :: data_gat !< Array of data gathered from all subdomains to receive
      integer, intent(in) :: ngroups !< Number of groups (subdomains)
      integer, dimension(ngroups), intent(in) :: recvCount !< Array containing the number of elements that are received from each subdomain
      integer, dimension(ngroups), intent(in) :: displs !< Entry i in this array specifies the displacement (relative to data_gat) at which
      !< to place the incoming data from process i (significant only at root)
      integer, intent(in) :: root !< Rank of receiving process
      integer, intent(inout) :: ierror !< Error index

      ierror = -1
#ifdef HAVE_MPI
      call mpi_gatherv(data_send, ndata_send, mpi_double_precision, data_gat, recvCount, displs, mpi_double_precision, root, DFM_COMM_DFMWORLD, ierror)
#endif

   end subroutine gatherv_double_data_mpi_dif

!> Gathers integer data into specified locations from all processes in a group and delivers to a specified root process.
!! Note: Different numbers of data on different subdomains can be sent.
   subroutine gatherv_int_data_mpi_dif(ndata_send, data_send, ndata_gat, data_gat, ngroups, recvCount, displs, root, ierror)
#ifdef HAVE_MPI
      use mpi
#endif

      implicit none
      integer, intent(in) :: ndata_send !< Number of data in array data_send
      integer, dimension(ndata_send), intent(in) :: data_send !< Array of data on one subdomain to send
      integer, intent(in) :: ndata_gat !< Number of data in array data_gat
      integer, dimension(ndata_gat), intent(inout) :: data_gat !< Array of data gathered from all subdomains to receive
      integer, intent(in) :: ngroups !< Number of groups (subdomains)
      integer, dimension(ngroups), intent(in) :: recvCount !< Array containing the number of elements that are received from each subdomain
      integer, dimension(ngroups), intent(in) :: displs !< Entry i in this array specifies the displacement (relative to data_gat) at which
      !< to place the incoming data from process i (significant only at root)
      integer, intent(in) :: root !< Rank of receiving process
      integer, intent(inout) :: ierror !< Error index

      ierror = -1
#ifdef HAVE_MPI
      call mpi_gatherv(data_send, ndata_send, mpi_integer, data_gat, recvCount, displs, mpi_integer, root, DFM_COMM_DFMWORLD, ierror)
#endif

   end subroutine gatherv_int_data_mpi_dif

!> Abort all processes
   subroutine abort_all()
      use dfm_error, only: DFM_GENERICERROR
      implicit none
      integer :: ierr
#ifdef HAVE_MPI
      call MPI_Abort(DFM_COMM_ALLWORLD, DFM_GENERICERROR, ierr)
#endif
      return
   end subroutine abort_all

!> counts a number of ghost data
   integer function count_list_size(min_ghost_level, max_ghost_level, ghost_list)
      implicit none
      integer, intent(in) :: min_ghost_level, max_ghost_level
      type(tghost), intent(in) :: ghost_list(:)

      integer :: ghost_level, domain_number

      count_list_size = 0
      do ghost_level = min_ghost_level, max_ghost_level
         if (ghost_level < lbound(ghost_list, 1) .or. ghost_level > ubound(ghost_list, 1)) cycle ! should not happen
         do domain_number = 0, ghost_list(ghost_level)%numdomains - 1
            if (ghost_list(ghost_level)%N(domain_number) < ghost_list(ghost_level)%N(domain_number - 1)) cycle
            count_list_size = count_list_size + ghost_list(ghost_level)%N(domain_number) - ghost_list(ghost_level)%N(domain_number - 1)
         end do
      end do
   end function count_list_size

!> allocates ghost lists
   subroutine allocate_ghost_data_lists(list_size, ghost_data_list, nr_ghost_data_list)
      implicit none
      integer, intent(in) :: list_size
      integer, allocatable, intent(inout) :: ghost_data_list(:)
      integer, allocatable, intent(inout) :: nr_ghost_data_list(:)

!     deallocate
      if (allocated(ghost_data_list)) deallocate (ghost_data_list)
      if (allocated(nr_ghost_data_list)) deallocate (nr_ghost_data_list)

!     allocate
      allocate (ghost_data_list(list_size))
      allocate (nr_ghost_data_list(-1:ndomains - 1))
      ghost_data_list = 0
      nr_ghost_data_list = 0
   end subroutine allocate_ghost_data_lists

!> fills ghost data list
   subroutine fill_ghost_data_list(min_ghost_level, max_ghost_level, ghost_list, ghost_data_list, nr_ghost_data_list)
      implicit none
      integer, intent(in) :: min_ghost_level, max_ghost_level
      type(tghost), dimension(:), allocatable, intent(in) :: ghost_list
      integer, allocatable, intent(inout) :: ghost_data_list(:)
      integer, allocatable, intent(inout) :: nr_ghost_data_list(:)

      integer :: num, domain_number, ghost_level, index

      num = 0
      do domain_number = 0, ndomains - 1
         do ghost_level = min_ghost_level, max_ghost_level
            if (ghost_level < lbound(ghost_list, 1) .or. ghost_level > ubound(ghost_list, 1)) cycle
            if (domain_number > ghost_list(ghost_level)%numdomains - 1) cycle
            do index = ghost_list(ghost_level)%N(domain_number - 1) + 1, ghost_list(ghost_level)%N(domain_number)
               num = num + 1
               ghost_data_list(num) = ghost_list(ghost_level)%list(index)
               nr_ghost_data_list(domain_number:ndomains - 1) = nr_ghost_data_list(domain_number:ndomains - 1) + 1
            end do
         end do
      end do
   end subroutine fill_ghost_data_list

!> get ghost cells
!! get the sorted ghost flow node list and count the number of flow nodes per ghost domain
   subroutine get_ghost_cells(domain_number, min_ghost_level, max_ghost_level, ghost_type, ghost_list)
      use m_flowgeom, only: ndx
      use m_alloc

      implicit none
      integer, intent(in) :: domain_number
      integer, intent(in) :: min_ghost_level, max_ghost_level, ghost_type
      type(tghost), allocatable, intent(out) :: ghost_list(:)

      integer, pointer :: ghost_level(:)
      integer :: cell

      if (ghost_type == IGHOSTTYPE_CELLBASED) then
         ghost_level => ighostlev_cellbased
      else if (ghost_type == IGHOSTTYPE_NODEBASED) then
         ghost_level => ighostlev_nodebased
      else ! combined
         ghost_level => ighostlev
      end if

      call alloc_tghost(ghost_list, max_ghost_level, min_ghost_level)

      do cell = 1, ndx ! also include fictitious boundary node
         if (idomain(cell) /= domain_number .and. &
             ghost_level(cell) >= min_ghost_level .and. ghost_level(cell) <= max_ghost_level) then
            call add_data_to_ghost_list(ghost_level(cell), idomain(cell), ghost_list, cell)
         end if
      end do

      call accumulate_ghost_N(min_ghost_level, max_ghost_level, ghost_list)
      call count_neighboring_domains(min_ghost_level, max_ghost_level, ghost_list)

   end subroutine get_ghost_cells

!> add value (flow node, flow link, corner) to ghost_list
   subroutine add_data_to_ghost_list(ghost_level, domain_number, ghost_list, value)
      use m_alloc

      implicit none
      integer, intent(in) :: ghost_level, domain_number, value
      type(tghost), allocatable, intent(inout) :: ghost_list(:)

      integer :: number_of_data, number_of_domains

      number_of_data = ghost_list(ghost_level)%num + 1
      number_of_domains = max(ghost_list(ghost_level)%numdomains, domain_number + 1) ! domain numbers are zero-based

      if (number_of_domains - 1 > ubound(ghost_list(ghost_level)%N, 1)) then
         call realloc(ghost_list(ghost_level)%N, number_of_domains - 1, -1, fill=0, keepExisting=.true.)
      end if
      if (number_of_data > ubound(ghost_list(ghost_level)%list, 1)) then
         call realloc(ghost_list(ghost_level)%list, int(1.2d0 * dble(number_of_data)) + 1, fill=0, keepExisting=.true.)
      end if

      ghost_list(ghost_level)%N(domain_number) = ghost_list(ghost_level)%N(domain_number) + 1
      call insert_in_ghost_list(ghost_level, domain_number, ghost_list, number_of_data, value)
      ghost_list(ghost_level)%num = number_of_data
      ghost_list(ghost_level)%numdomains = number_of_domains

   end subroutine add_data_to_ghost_list

!> insert node/link/corner into ghost list
   subroutine insert_in_ghost_list(ghost_level, domain_number, ghost_list, number_of_data, value)
      implicit none
      integer, intent(in) :: ghost_level, domain_number, number_of_data, value
      type(tghost), allocatable, intent(inout) :: ghost_list(:)

      integer :: number_of_data_in_list, i

      number_of_data_in_list = sum(ghost_list(ghost_level)%N(0:domain_number))
      do i = number_of_data, number_of_data_in_list + 1, -1
         ghost_list(ghost_level)%list(i) = ghost_list(ghost_level)%list(i - 1)
      end do
      ghost_list(ghost_level)%list(number_of_data_in_list) = value

   end subroutine insert_in_ghost_list

!> get ghost links
   subroutine get_ghost_links(domain_number, min_ghost_level, max_ghost_level, ghost_type, ghost_list)
      use m_flowgeom, only: Lnx, ln
      use m_alloc

      implicit none
      integer, intent(in) :: domain_number
      integer, intent(in) :: min_ghost_level, max_ghost_level, ghost_type
      type(tghost), allocatable, intent(out) :: ghost_list(:)

      integer, pointer :: ghost_level(:)
      integer :: link, link_ghost_level, ghost_domain_number, jaghost

      if (ghost_type == IGHOSTTYPE_CELLBASED) then
         ghost_level => ighostlev_cellbased
      else if (ghost_type == IGHOSTTYPE_NODEBASED) then
         ghost_level => ighostlev_nodebased
      else ! combined
         ghost_level => ighostlev
      end if

      call alloc_tghost(ghost_list, max_ghost_level, min_ghost_level)

      do link = 1, lnx
!   determine if flow link is a ghost link and get ghost domain number and ghost level of link
         call link_ghostdata(domain_number, idomain(ln(1, link)), idomain(ln(2, link)), jaghost, ghost_domain_number, &
                             ghost_level(ln(1, link)), ghost_level(ln(2, link)), link_ghost_level)

         if (jaghost > 0 .and. link_ghost_level >= min_ghost_level .and. link_ghost_level <= max_ghost_level) then
            call add_data_to_ghost_list(link_ghost_level, ghost_domain_number, ghost_list, link)
         end if
      end do

      call accumulate_ghost_N(min_ghost_level, max_ghost_level, ghost_list)
      call count_neighboring_domains(min_ghost_level, max_ghost_level, ghost_list)

   end subroutine get_ghost_links

!>     accumulate ghost_list()%N
   subroutine accumulate_ghost_N(min_ghost_level, max_ghost_level, ghost_list)
      implicit none
      integer, intent(in) :: min_ghost_level, max_ghost_level
      type(tghost), allocatable, intent(inout) :: ghost_list(:)

      integer :: ghost_level, domain_number, j

      do ghost_level = min_ghost_level, max_ghost_level
         do domain_number = ghost_list(ghost_level)%numdomains - 1, 0, -1
            do j = 0, domain_number - 1
               ghost_list(ghost_level)%N(domain_number) = ghost_list(ghost_level)%N(domain_number) + ghost_list(ghost_level)%N(j)
            end do
         end do
      end do
   end subroutine accumulate_ghost_N

!>     count the neighboring domains
   subroutine count_neighboring_domains(min_ghost_level, max_ghost_level, ghost_list)
      use m_alloc
      implicit none
      integer, intent(in) :: min_ghost_level, max_ghost_level
      type(tghost), allocatable, intent(inout) :: ghost_list(:)

      integer :: ghost_level, domain_number, num

      do ghost_level = min_ghost_level, max_ghost_level
         num = 0
         do domain_number = 0, ghost_list(ghost_level)%numdomains - 1
            if (ghost_list(ghost_level)%N(domain_number) - ghost_list(ghost_level)%N(domain_number - 1) > 0) then
               num = num + 1
               if (num > ubound(ghost_list(ghost_level)%neighdmn, 1)) then
                  call realloc(ghost_list(ghost_level)%neighdmn, int(1.2d0 * dble(num) + 1d0), keepExisting=.true., fill=0)
               end if
               ghost_list(ghost_level)%neighdmn(num) = domain_number
            end if
         end do
         ghost_list(ghost_level)%numneighdmn = num
      end do
   end subroutine count_neighboring_domains

!> get ghost corners
   subroutine get_ghost_corners(domain_number, min_ghost_level, max_ghost_level, ghost_type, ghost_list)
      use network_data, only: kn, numk, nmk, nod
      use m_alloc

      implicit none
      integer, intent(in) :: domain_number
      integer, intent(in) :: min_ghost_level, max_ghost_level, ghost_type
      type(tghost), allocatable, intent(out) :: ghost_list(:)

      integer, external :: common_cell_for_two_net_links

      integer, pointer :: ghost_level(:)
      integer :: node, index, first_link, second_link, cell, number_of_cells, min_ghost_level_for_cell
      integer, allocatable :: list_of_cells(:)

      if (ghost_type == IGHOSTTYPE_CELLBASED) then
         ghost_level => ighostlev_cellbased
      else if (ghost_type == IGHOSTTYPE_NODEBASED) then
         ghost_level => ighostlev_nodebased
      else ! combined
         ghost_level => ighostlev
      end if

      call alloc_tghost(ghost_list, max_ghost_level, min_ghost_level)

      allocate (list_of_cells(maxval(nmk(1:numk))))

      loop_over_nodes: &
         do node = 1, numk
         number_of_cells = 0
         do index = 1, nmk(node)
            first_link = nod(node)%lin(index)
            if (index < nmk(node)) then
               second_link = nod(node)%lin(index + 1)
            else
               second_link = nod(node)%lin(1)
            end if

            cell = common_cell_for_two_net_links(first_link, second_link)

            if (cell < 1) cycle

            if (idomain(cell) == domain_number) cycle loop_over_nodes

            if (ghost_level(cell) < min_ghost_level .or. ghost_level(cell) > max_ghost_level) cycle

            number_of_cells = number_of_cells + 1
            list_of_cells(number_of_cells) = cell
         end do

         if (number_of_cells == 0) cycle

         min_ghost_level_for_cell = max_ghost_level + 1
         do index = 1, number_of_cells
            if (min_ghost_level_for_cell > ghost_level(list_of_cells(index))) then
               min_ghost_level_for_cell = ghost_level(list_of_cells(index))
               cell = list_of_cells(index)
            else if (min_ghost_level_for_cell == ghost_level(list_of_cells(index))) then
               if (abs(domain_number - idomain(cell)) > abs(domain_number - idomain(list_of_cells(index)))) then
                  cell = list_of_cells(index)
               end if
            end if
         end do

         if (min_ghost_level_for_cell <= max_ghost_level) then
            call add_data_to_ghost_list(ghost_level(cell), idomain(cell), ghost_list, node)
         end if
      end do loop_over_nodes

      call accumulate_ghost_N(min_ghost_level, max_ghost_level, ghost_list)
      call count_neighboring_domains(min_ghost_level, max_ghost_level, ghost_list)

   end subroutine get_ghost_corners

!> makes ghost list
   subroutine make_ghost_list(domain_number, itype, min_ghost_level, max_ghost_level, list_size, &
                              ghost_data_list, nr_ghost_data_list, error)

      integer, intent(in) :: domain_number
      integer, intent(in) :: itype ! type of ghost
      integer, intent(in) :: min_ghost_level
      integer, intent(in) :: max_ghost_level
      integer, intent(inout) :: list_size
      integer, allocatable, intent(inout) :: ghost_data_list(:)
      integer, allocatable, intent(inout) :: nr_ghost_data_list(:)
      integer, intent(inout) :: error !< error (1) or not (0)

      type(tghost), allocatable :: ghost_list(:)

      call partition_get_ghosts(domain_number, itype, ghost_list, error)
      list_size = count_list_size(min_ghost_level, max_ghost_level, ghost_list)
      call allocate_ghost_data_lists(list_size, ghost_data_list, nr_ghost_data_list)
      call fill_ghost_data_list(min_ghost_level, max_ghost_level, ghost_list, ghost_data_list, nr_ghost_data_list)

   end subroutine make_ghost_list

   function get_list_size(list, list_sall) result(size)
      integer, allocatable, intent(in) :: list(:)
      integer, allocatable, intent(in) :: list_sall(:)
      integer :: size

      if (allocated(list_sall) .and. allocated(list)) then
         size = list(ndomains - 1)
      else
         size = 0
      end if

   end function get_list_size

!> Check if any structures lie across multiple partitions
!! This is currently used for statistical output, to disable output items that
!! would in this case produce incorrect results (as integrated values on structures
!! are only reduced every user time step, so min/max in time would not be valid)
   function any_structures_lie_across_multiple_partitions(node_count_per_structure) result(res)
#ifdef HAVE_MPI
      use mpi
#endif
      use MessageHandling, only: mess, LEVEL_ERROR, LEVEL_WARN

      integer, intent(in) :: node_count_per_structure(:) !< Total number of nodes for each instance of a type of structure
      logical :: res !< True if any instance of this type of structure lies across multiple partitions, false otherwise

      integer :: number_of_structures, i_struc, n_partitions_with_nodes, ierr
      logical :: has_nodes_in_current_partition, structure_lies_across_partitions

      res = .false. !Default return value

      if (jampi == 0) then
         return
      end if

      number_of_structures = size(node_count_per_structure)

      if (number_of_structures == 0) then
         return
      end if

#ifdef HAVE_MPI
      loop_over_structures: do i_struc = 1, number_of_structures

         ! The number of nodes for each structure that is provided as input, is different for each process. If none of the structure's
         ! geometry nodes lie in the partition of the current process, then nodeCountStru(i_struc) will be zero on this process.
         has_nodes_in_current_partition = node_count_per_structure(i_struc) > 0

         n_partitions_with_nodes = 0
         if (has_nodes_in_current_partition) then
            n_partitions_with_nodes = 1
         end if
         call mpi_allreduce(mpi_in_place, n_partitions_with_nodes, 1, mpi_integer, mpi_sum, DFM_COMM_DFMWORLD, ierr)
         if (ierr /= MPI_SUCCESS) then
            call mess(LEVEL_ERROR, 'Programming error, please report: mpi_allreduce failed in any_structures_lie_across_multiple_partitions')
         end if

         ! If more than one process lists >0 geometry nodes for a structure, then that structure lies across multiple partitions.
         structure_lies_across_partitions = .false.
         if (n_partitions_with_nodes == 0) then
            call mess(LEVEL_WARN, 'Programming error, please report: any_structures_lie_across_multiple_partitions found no partitions with nodes!')
         elseif (n_partitions_with_nodes > 1) then
            structure_lies_across_partitions = .true.
         end if

         if (structure_lies_across_partitions) then
            res = .true.
            exit loop_over_structures
         end if

      end do loop_over_structures
#endif

   end function any_structures_lie_across_multiple_partitions

!> Check if any cross-sections lie across multiple partitions
!! This is currently used for statistical output, to disable output items that
!! would in this case produce incorrect results (as integrated values on cross-sections
!! are only reduced every user time step, so min/max in time would not be valid)
   function model_has_crosssections_across_partitions result(res)
      use m_monitoring_crosssections, only: ncrs, crs

      logical :: res !< True if the crs array contains any cross-sections that lie across multiple partitions, false otherwise

      integer :: icrs

      res = .false.

      do icrs = 1, ncrs
         if (flowlinks_are_across_multiple_partitions(crs(icrs)%path%ln)) then
            res = .true.
         end if
      end do

   end function model_has_crosssections_across_partitions

!> Check if a set of flowlinks lies across multiple partitions
   function flowlinks_are_across_multiple_partitions(flowlinks) result(res)
      use mpi
      use MessageHandling, only: mess, LEVEL_ERROR, LEVEL_WARN

      integer, dimension(:), intent(in) :: flowlinks !< Array of flowlink indices to check
      logical :: res !< True if the flowlinks lie across multiple partitions, false otherwise

      logical :: has_flowlinks_in_current_partition
      integer :: n_partitions_with_flowlinks
      integer :: ierr

      res = .false. !Default return value

      if (jampi == 0) then
         return
      end if

      ! The array of flowlinks that is provided as input, is different for each process. For example, if we are checking if a cross-section
      ! lies across multiple partitions, we look at the flowlinks that are listed for that cross-section on each process. As each process
      ! "knows" only the grid of its own partition, it lists only the flowlinks crossing the cross-section in its own partition.
      ! If the cross-section lies entirely outside a process' partition, it will not list any flowlinks, so the size of crs%path%ln will
      ! be zero (note: as can be seen in crspath_on_flowgeom, ghost links are not listed!).
      has_flowlinks_in_current_partition = size(flowlinks) > 0

      n_partitions_with_flowlinks = 0
      if (has_flowlinks_in_current_partition) then
         n_partitions_with_flowlinks = 1
      end if
      call mpi_allreduce(mpi_in_place, n_partitions_with_flowlinks, 1, mpi_integer, mpi_sum, DFM_COMM_DFMWORLD, ierr)
      if (ierr /= MPI_SUCCESS) then
         call mess(LEVEL_ERROR, 'Programming error, please report: mpi_allreduce failed in flowlinks_are_across_multiple_partitions')
      end if

      if (n_partitions_with_flowlinks == 0) then
         call mess(LEVEL_WARN, 'Programming error, please report: flowlinks_are_across_multiple_partitions found no partitions with flowlinks!')
      elseif (n_partitions_with_flowlinks > 1) then
         res = .true.
      end if

   end function flowlinks_are_across_multiple_partitions

!> Fill in the geometry arrays of cross sections for history output
!! Two special situations are also treated:
!! 1. The flowlinks of one cross section are not successive on one subdomain, which can happen in both sequential and parallel simulations.
!! 2. In parallel simulations, a cross section lies on multiple subdomains.
   subroutine fill_geometry_arrays_crs()
      use m_alloc
      use m_GlobalParameters
      use m_flowparameters, only: eps6
      use precision_basics
      use m_monitoring_crosssections, only: nodecountcrs, ncrs, crs, nnodescrs, geomxcrs, geomycrs
      implicit none

      real(dp), allocatable :: xGat(:), yGat(:) ! Coordinates that are gathered data from all subdomains
      integer, allocatable :: nodeCountCrsMPI(:) ! Count of nodes per cross section after mpi communication.
      real(dp), allocatable :: geomXCrsMPI(:) ! [m] x coordinates of cross sections after mpi communication.
      real(dp), allocatable :: geomYCrsMPI(:) ! [m] y coordinates of cross sections after mpi communication.
      integer, allocatable :: nodeCountCrsGat(:), nNodesCrsGat(:), displs(:)
      real(dp), allocatable :: geomX(:), geomY(:)
      integer :: nlinks, i, j, j1, k, k1, ierror, is, ie, n, ii, nNodes, nNodesCrsMPI, L, L0, ks, ke, nPar, nNodesAdd, jaexist, nb, nbLast, kk
      real(dp) :: xNew, yNew, xOld, yOld
      integer, allocatable :: maskBnd(:), maskBndAll(:), maskBndGat(:), indBndMPI(:), jaCoincide(:) ! Arrays for boundary nodes, only used in parallel run

      ! Allocate and construct geometry variable arrays (on one subdomain)
      call realloc(nodeCountCrs, ncrs, keepExisting=.false., fill=0)

      do i = 1, ncrs
         nlinks = crs(i)%path%lnx
         if (nlinks > 0) then
            nodeCountCrs(i) = nlinks + 1 ! Here assumes that the flowlinks of the cross section are successive.
            ! The situation when they are not successive will be handled later in this subroutine.
         end if
      end do
      nNodesCrs = sum(nodeCountCrs)
      call realloc(geomXCrs, nNodesCrs, keepExisting=.false., fill=0d0)
      call realloc(geomYCrs, nNodesCrs, keepExisting=.false., fill=0d0)
      if (jampi > 0) then
         ! In parallel runs, one cross section might lie on multiple subdomains. To handle this situation,
         ! we will need to know which nodes are on boundaries of a cross section on each subdomain, and the boundary nodes will be handled separately.
         ! This will avoid having duplicated (boundary) nodes in the arrays of coordinates of a cross section among all subdomains.
         call realloc(maskBndAll, nNodesCrs, keepExisting=.false., fill=0) ! If the node is a boundary node then the value will be 1
      end if
      is = 0
      ie = 0
      do i = 1, ncrs
         nNodes = nodeCountCrs(i)
         nlinks = crs(i)%path%lnx
         if (nNodes > 0) then
            call realloc(geomX, max(allocSize(geomX), nNodes), keepExisting=.false.)
            call realloc(geomY, max(allocSize(geomY), nNodes), keepExisting=.false.)
            L = crs(i)%path%iperm(1)
            geomX(1) = crs(i)%path%xk(1, L)
            geomX(2) = crs(i)%path%xk(2, L)
            geomY(1) = crs(i)%path%yk(1, L)
            geomY(2) = crs(i)%path%yk(2, L)

            if (jampi > 0) then
               ! Determine the 1st boundary node (Boundary nodes are only useful for parallel simulations).
               call realloc(maskBnd, nNodes, keepExisting=.false., fill=0)
               if (nlinks == 1) then
                  ! If there is only one link, then the two nodes are the boundary nodes.
                  maskBnd(1) = 1
                  maskBnd(2) = 1
               else
                  ! If there are more than one link, then (geomX(1),geomY(1)) is a boundary node.
                  maskBnd(1) = 1
               end if
            end if

            if (nlinks > 1) then
               ! If there is more than one link, adding more nodes to coordinates arrays.
               k = 3
               nNodesAdd = 0
               do L0 = 2, nlinks
                  L = crs(i)%path%iperm(L0)

                  if (comparereal(crs(i)%path%xk(1, L), geomX(k - 1), eps6) /= 0 .or. comparereal(crs(i)%path%yk(1, L), geomY(k - 1), eps6) /= 0) then
                     ! If the 1st node of link L is not the ending node of the previous link,
                     ! then this means that the flowlinks for this cross section are not successive, and
                     ! they have a break between Link L and the previous link.
                     ! In this situation, one more node, i.e. the 1st node of link L, should be included.
                     nNodes = nNodes + 1
                     nodeCountCrs(i) = nNodes
                     call realloc(geomX, max(allocSize(geomX), nNodes), keepExisting=.true.)
                     call realloc(geomY, max(allocSize(geomY), nNodes), keepExisting=.true.)
                     geomX(k) = crs(i)%path%xk(1, L)
                     geomY(k) = crs(i)%path%yk(1, L)

                     if (jampi > 0) then ! Mark this node and the previous node as boundary nodes.
                        call realloc(maskBnd, nNodes, keepExisting=.true.)
                        maskBnd(k - 1) = 1
                        maskBnd(k) = 1
                     end if

                     k = k + 1
                     nNodesAdd = nNodesAdd + 1
                  end if

                  ! We take the 2nd node of link L, because the orientation of all links has been
                  ! guaranteed in subroutine crspath_on_singlelink.
                  geomX(k) = crs(i)%path%xk(2, L)
                  geomY(k) = crs(i)%path%yk(2, L)

                  if (jampi > 0 .and. L0 == nlinks) then ! The 2nd node of the last link is a boundary node.
                     maskBnd(k) = 1
                  end if

                  k = k + 1
               end do

               if (nNodesAdd > 0) then
                  nNodesCrs = nNodesCrs + nNodesAdd
                  call realloc(geomXCrs, nNodesCrs, keepExisting=.true.)
                  call realloc(geomYCrs, nNodesCrs, keepExisting=.true.)
                  call realloc(maskBndAll, nNodesCrs, keepExisting=.true.)
               end if

               is = ie + 1
               ie = is + nNodes - 1
               geomXCrs(is:ie) = geomX(1:nNodes)
               geomYCrs(is:ie) = geomY(1:nNodes)
               if (jampi > 0) then
                  maskBndAll(is:ie) = maskBnd(1:nNodes)
               end if
            end if
         end if
      end do

   !! The codes below are similar to subroutine "fill_geometry_arrays_lateral".
   !! They work for cross sections, including the situation that a cross section lies on multiple subdomains.
      ! For parallel simulation: since only process 0000 writes the history output, the related arrays
      ! are only made on 0000.
      if (jampi > 0) then
         call reduce_int_sum(nNodesCrs, nNodesCrsMPI) ! Get total number of nodes among all subdomains

         if (my_rank == 0) then
            ! Allocate arrays
            call realloc(nodeCountCrsMPI, ncrs, keepExisting=.false., fill=0)
            call realloc(geomXCrsMPI, nNodesCrsMPI, keepExisting=.false., fill=0d0)
            call realloc(geomYCrsMPI, nNodesCrsMPI, keepExisting=.false., fill=0d0)

            ! Allocate arrays that gather information from all subdomains
            ! Data on all subdomains will be gathered in a contiguous way
            call realloc(nodeCountCrsGat, ncrs * ndomains, keepExisting=.false., fill=0)
            call realloc(xGat, nNodesCrsMPI, keepExisting=.false., fill=0d0)
            call realloc(yGat, nNodesCrsMPI, keepExisting=.false., fill=0d0)
            call realloc(displs, ndomains, keepExisting=.false., fill=0)
            call realloc(nNodesCrsGat, ndomains, keepExisting=.false., fill=0)
            call realloc(maskBndGat, nNodesCrsMPI, keepExisting=.false., fill=0)
         else
            ! NOTE: dummy allocate to prevent crash in Debug-model on Intel MPI, even though receive buffers are officially not needed on non-root.
            allocate (nodeCountCrsGat(0), xGat(0), yGat(0), displs(0), nNodesCrsGat(0), maskBndGat(0))
         end if

         ! Gather integer data, where the same number of data, i.e. ncrs, are gathered from each subdomain to process 0000
         call gather_int_data_mpi_same(ncrs, nodeCountCrs, ncrs * ndomains, nodeCountCrsGat, ncrs, 0, ierror)

         if (my_rank == 0) then
            ! To use mpi gather call, construct displs, and nNodesCrsGat (used as receive count for mpi gather call)
            displs(1) = 0
            do i = 1, ndomains
               is = (i - 1) * ncrs + 1 ! Starting index in nodeCountCrsGat
               ie = is + ncrs - 1 ! Ending   index in nodeCountCrsGat
               nNodesCrsGat(i) = sum(nodeCountCrsGat(is:ie)) ! Total number of nodes on subdomain i
               if (i > 1) then
                  displs(i) = displs(i - 1) + nNodesCrsGat(i - 1)
               end if
            end do
         end if

         ! Gather double precision data, here, different number of data can be gatherd from different subdomains to process 0000
         call gatherv_double_data_mpi_dif(nNodesCrs, geomXCrs, nNodesCrsMPI, xGat, ndomains, nNodesCrsGat, displs, 0, ierror)
         call gatherv_double_data_mpi_dif(nNodesCrs, geomYCrs, nNodesCrsMPI, yGat, ndomains, nNodesCrsGat, displs, 0, ierror)
         call gatherv_int_data_mpi_dif(nNodesCrs, maskBndAll, nNodesCrsMPI, maskBndGat, ndomains, nNodesCrsGat, displs, 0, ierror)
         if (my_rank == 0) then
            ! Construct nodeCountCrsMPI for history output
            do i = 1, ncrs
               do n = 1, ndomains
                  k = (n - 1) * ncrs + i
                  nodeCountCrsMPI(i) = nodeCountCrsMPI(i) + nodeCountCrsGat(k) ! Total number of nodes for cross section i among all subdomains
               end do
            end do

            ! Construct geomXCrsMPI and geomYCrsMPI for history output
            j = 0
            do i = 1, ncrs ! for each cross section
               nPar = 0 ! Number of subdomains that contain this cross section
               nb = 0 ! Number of boundary nodes for this cross section
               nbLast = 0 ! Number of boundary nodes for this cross section in the previous subdomains
               call realloc(indBndMPI, nodeCountCrsMPI(i), keepExisting=.false., fill=0)
               call Realloc(jaCoincide, nodeCountCrsMPI(i), keepExisting=.false., fill=0)
               do n = 1, ndomains ! on each sudomain
                  k = (n - 1) * ncrs + i ! index in nodeCountCrsGat
                  nNodes = nodeCountCrsGat(k) ! cross section i on sumdomain n has nNodes nodes
                  if (nNodes > 0) then
                     nPar = nPar + 1
                     ii = (n - 1) * ncrs
                     is = sum(nNodesCrsGat(1:n - 1)) + sum(nodeCountCrsGat(ii + 1:ii + i - 1)) ! starting index in xGat
                     ks = 1
                     ke = nNodes
                     if (nPar > 1) then ! This cross section lies on multiple subdomains.
                        ! Select and add the nodes of this cross section on the current subdomain
                        do k1 = ks, ke
                           kk = is + k1
                           if (maskBndGat(kk) == 1) then ! If it is a boundary node, need to check if it already exists in the coordinate arrays, i.e. GeomXCrsMPI and GeomYCrsMPI
                              xNew = xGat(kk)
                              yNew = yGat(kk)
                              jaexist = 0
                              do j1 = 1, nbLast ! Loop over all the boundary nodes of the previous subdomains that have been added in the coordinate arrays
                                 if (jaCoincide(j1) == 0) then
                                    ! If the j1 boundary node is not coincide with any boundary node, then check if node (xNew,yNew) is coincide with it or not.
                                    ! If jaCoincide(j1) == 1, then no need to check this node because one boundary node can be
                                    ! coincide with another boundary node maximal ONCE.
                                    xOld = geomXCrsMPI(indBndMPI(j1))
                                    yOld = geomYCrsMPI(indBndMPI(j1))
                                    if (comparereal(xNew, xOld, eps6) == 0 .and. comparereal(xNew, xOld, eps6) == 0) then
                                       jaexist = 1
                                       jaCoincide(j1) = 1
                                       exit
                                    end if
                                 end if
                              end do
                              if (jaexist == 0) then ! If the new candidate node does not exist in the coordinate arrays, then add it
                                 j = j + 1
                                 geomXCrsMPI(j) = xNew
                                 geomYCrsMPI(j) = yNew
                                 nb = nb + 1 ! add one boundary node
                                 indBndMPI(nb) = j ! store its index in geomXCrsMPI (and geomYCrsMPI)
                              else
                                 nodeCountCrsMPI(i) = nodeCountCrsMPI(i) - 1 ! adjust the node counter
                                 nNodesCrsMPI = nNodesCrsMPI - 1
                              end if
                           else ! If it is not a boundary node, then add it directly
                              j = j + 1
                              geomXCrsMPI(j) = xGat(kk)
                              geomYCrsMPI(j) = yGat(kk)
                           end if
                        end do
                     else
                        do k1 = ks, ke
                           j = j + 1
                           kk = is + k1
                           geomXCrsMPI(j) = xGat(kk)
                           geomYCrsMPI(j) = yGat(kk)
                           if (maskBndGat(kk) == 1) then
                              nb = nb + 1
                              indBndMPI(nb) = j ! store the index in geomXCrsMPI for the boundary nodes
                           end if
                        end do
                     end if
                     nbLast = nb ! update nbLast when the current subdomain is finished.
                  end if
               end do
            end do

            ! Copy the MPI-arrays to nodeCountCrs, geomXCrs and geomYCrs for the his-output
            nNodesCrs = nNodesCrsMPI
            nodeCountCrs(1:ncrs) = nodeCountCrsMPI(1:ncrs)
            call realloc(geomXCrs, nNodesCrs, keepExisting=.false., fill=0d0)
            call realloc(geomYCrs, nNodesCrs, keepExisting=.false., fill=0d0)
            geomXCrs(1:nNodesCrs) = geomXCrsMPI(1:nNodesCrs)
            geomYCrs(1:nNodesCrs) = geomYCrsMPI(1:nNodesCrs)
         end if
      end if
   end subroutine fill_geometry_arrays_crs

end module m_partitioninfo

subroutine pressakey()
#ifdef HAVE_MPI
   use mpi
   use m_partitioninfo

   implicit none

   integer :: ierr

   call MPI_barrier(DFM_COMM_ALLWORLD, ierr)

   if (my_rank == 0) then
      write (6, *) "press a key from rank 0..."
      read (5, *)
   end if

   call MPI_barrier(DFM_COMM_ALLWORLD, ierr)
#else
   write (6, *) "press a key..."
   read (5, *)
#endif

   return

end subroutine pressakey

!  print timing information to file
subroutine print_timings(FNAM, dtime)
#ifdef HAVE_MPI
   use mpi
#endif
   use m_timer
   use m_partitioninfo

   implicit none

   character(len=*), intent(in) :: FNAM !< file name
   double precision, intent(in) :: dtime !< time

   integer :: ierr
   integer :: i, j, lenstr

   logical :: Lexist

   integer, parameter :: ISTRLEN = 20
   integer :: MFILE

   integer, parameter :: Ntvarlist = 13
   integer, dimension(Ntvarlist), parameter :: itvarlist = (/1, 5, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17/)

   double precision, dimension(:, :), allocatable :: t_max, t_ave, tcpu_max, tcpu_ave
   integer :: itsol_max

   integer :: jadoit

   character(len=128) :: FORMATSTRING, FORMATSTRINGINT, dum

!     allocate local arrays
!      if ( my_rank.eq.0 ) then
   allocate (t_max(3, NUMT), t_ave(3, NUMT), tcpu_max(3, NUMT), tcpu_ave(3, NUMT))
!      end if

   if (jampi == 1) then
#ifdef HAVE_MPI
!        reduce timings
      call mpi_reduce(t, t_max, 3 * NUMT, MPI_DOUBLE_PRECISION, MPI_MAX, 0, DFM_COMM_DFMWORLD, ierr)
      call mpi_reduce(t, t_ave, 3 * NUMT, MPI_DOUBLE_PRECISION, MPI_SUM, 0, DFM_COMM_DFMWORLD, ierr)
      t_ave = t_ave / dble(ndomains)
      call mpi_reduce(tcpu, tcpu_max, 3 * NUMT, MPI_DOUBLE_PRECISION, MPI_MAX, 0, DFM_COMM_DFMWORLD, ierr)
      call mpi_reduce(tcpu, tcpu_ave, 3 * NUMT, MPI_DOUBLE_PRECISION, MPI_SUM, 0, DFM_COMM_DFMWORLD, ierr)
      tcpu_ave = tcpu_ave / dble(ndomains)
#endif
   else
      t_ave = t
      tcpu_ave = tcpu
      t_max = t
      tcpu_max = tcpu
   end if

!     reduce number of iterations
   if (jampi == 1) then
#ifdef HAVE_MPI
      call mpi_reduce(numcgits, itsol_max, 1, MPI_INTEGER, MPI_MAX, 0, DFM_COMM_DFMWORLD, ierr)
#endif
      jadoit = 0
      if (my_rank == 0) jadoit = 1
   else
      itsol_max = numcgits
      jadoit = 1
   end if

   if (jadoit == 1) then
      inquire (FILE=FNAM, EXIST=Lexist)
      open (newunit=MFILE, FILE=trim(FNAM), access='APPEND')

      if (.not. Lexist) then
!           print header
         lenstr = 4
         do j = 1, ISTRLEN - lenstr
            write (MFILE, '(" ")', advance="no")
         end do
         write (MFILE, '(A)', advance="no") 'time'

         lenstr = 16
         do j = 1, ISTRLEN - lenstr
            write (MFILE, '(" ")', advance="no")
         end do
         write (MFILE, '(A)', advance="no") 'number_of_tsteps'

         do i = 1, Ntvarlist
            lenstr = len_trim(tnams(itvarlist(i)))
            do j = 1, ISTRLEN - (lenstr + 4)
               write (MFILE, '(" ")', advance="no")
            end do
            write (MFILE, "(A, '_ave')", advance="no") trim(tnams(itvarlist(i)))

            do j = 1, ISTRLEN - (lenstr + 4)
               write (MFILE, '(" ")', advance="no")
            end do
            write (MFILE, "(A, '_max')", advance="no") trim(tnams(itvarlist(i)))

            do j = 1, ISTRLEN - (lenstr + 8)
               write (MFILE, '(" ")', advance="no")
            end do
            write (MFILE, "(A, '_CPU_ave')", advance="no") trim(tnams(itvarlist(i)))

            do j = 1, ISTRLEN - (lenstr + 8)
               write (MFILE, '(" ")', advance="no")
            end do
            write (MFILE, "(A, '_CPU_max')", advance="no") trim(tnams(itvarlist(i)))
         end do

         lenstr = 8
         do j = 1, ISTRLEN - lenstr
            write (MFILE, '(" ")', advance="no")
         end do
         write (MFILE, "(A)", advance="no") 'cg_iters'
         write (MFILE, *)
      end if

!        make format strings
      write (dum, '(I0)') ISTRLEN
      FORMATSTRING = '(E'//trim(adjustl(dum))//'.5, $)'
      FORMATSTRINGINT = '(I'//trim(adjustl(dum))//',   $)'
!         write(FORMATSTRING,   '("(E", I0, ".5, $)")') ISTRLEN
!         write(FORMATSTRINGINT,'("(I", I0, ", $)"  )') ISTRLEN

!        write time
      write (MFILE, trim(FORMATSTRING)) dtime

!        write number of timesteps
      write (MFILE, trim(FORMATSTRINGINT)) numtsteps

!        wite timings
      do i = 1, Ntvarlist
         write (MFILE, FORMATSTRING) t_ave(3, itvarlist(i))
         write (MFILE, FORMATSTRING) t_max(3, itvarlist(i))
         write (MFILE, FORMATSTRING) tcpu_ave(3, itvarlist(i))
         write (MFILE, FORMATSTRING) tcpu_max(3, itvarlist(i))
      end do

!        write number of iterations
      write (MFILE, FORMATSTRINGINT) itsol_max

      write (MFILE, *)

      close (MFILE)
   end if

!     deallocate local arrays
!      if ( my_rank.eq.0 ) then
   if (allocated(t_max)) deallocate (t_max)
   if (allocated(t_ave)) deallocate (t_ave)
   if (allocated(tcpu_max)) deallocate (tcpu_max)
   if (allocated(tcpu_ave)) deallocate (tcpu_ave)
!      end if

   return
end subroutine print_timings

!> generate partition numbering with METIS
!!   1D links are supported now
subroutine partition_METIS_to_idomain(Nparts, jacontiguous, method, iseed)
   use network_data
   use m_partitioninfo
   use m_metis
   use m_alloc
   use MessageHandling
   use unstruc_messages
   implicit none

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

   integer, external :: metisopts, METIS_PartGraphKway, METIS_PartGraphRecursive, METIS_PARTMESHDUAL

   ierror = 1

!     check validity of objected number of subdomains
   if (Nparts < 1) then
      call qnerror('partition_METIS_to_idomain: number of subdomains < 1', ' ', ' ')
      goto 1234
   end if

   !if ( netstat.eq.NETSTAT_CELLS_DIRTY ) then
   !   call findcells(0)
   !   call find1dcells()
   !endif

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
!      i = metisopts(opts,"NCUTS",10)
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

   ! netstat = NETSTAT_CELLS_DIRTY

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

1234 continue

   return
end subroutine partition_METIS_to_idomain

!  set METIS options, returns error (1) or no error (0)
integer function metisopts(opts, optionname, optionval)
   implicit none

   integer, intent(inout) :: opts(*) ! options array
   character(len=*), intent(in) :: optionname ! option name
   integer, intent(in) :: optionval ! option value
#ifdef HAVE_METIS
   integer :: i

   integer, external :: metisoptions

   i = metisoptions(opts, trim(optionname)//char(0), optionval)

   metisopts = i
#else
   metisopts = 1
#endif
end function metisopts

!> generate partition numbers from polygons, or with METIS of no polygons are present
subroutine partition_to_idomain()

   use m_polygon
   use m_partitioninfo
   use MessageHandling
   use gridoperations
   use m_getint

   implicit none

   integer :: japolygon
   integer :: i, jacontiguous, method
   integer :: NPL_save
   integer :: ierror

   character(len=100) :: message

!     save polygons
   NPL_save = NPL

!     disable polygons
   NPL = 0

   call findcells(0)
   call find1dcells()

!     reenable polygons
   NPL = NPL_save

   call delete_dry_points_and_areas()

   call cosphiunetcheck(1)

   if (NPL > 1) then ! use the polygons
      call generate_partitioning_from_pol()
   else ! use metis
      Ndomains = 0
      do while (Ndomains < 1)
         call getint('Number of domains', Ndomains)
      end do
      method = -1
      do while (method < 0 .or. method > 3)
         method = 1
         call getint('Partition method? (1: K-Way, 2: Recursive bisection, 3: Mesh-dual)', method) ! default method is K-way
      end do
      jacontiguous = -1
      if (method == 1 .or. method == 0) then ! K-Way (default) method enables contiguous
         do while (jacontiguous /= 0 .and. jacontiguous /= 1)
            jacontiguous = 1
            call getint('Enforce contiguous domains? (0:no, 1:yes)', jacontiguous)
         end do
      end if
      call partition_METIS_to_idomain(Ndomains, jacontiguous, method, 0)

      japolygon = -1
      do while (japolygon /= 1 .and. japolygon /= 0)
         japolygon = 0
         call getint('Generate polygon? (0: no, 1: yes)', japolygon)
      end do
      if (japolygon == 1) then
!            generate partitioning polygons
         call generate_partition_pol_from_idomain(ierror)

         if (ierror == 0) then
!               get 1D domain numbers from polygons
            call partition_pol_to_idomain(2)
         else
            japolygon = 0
         end if
      end if
   end if

!     deallocate
   if (allocated(numndx)) deallocate (numndx)

!     allocate numndx
   allocate (numndx(0:ndomains - 1))

!     count and output number of cells
   do i = 0, ndomains - 1
      numndx(i) = count(idomain == i)
      write (message, "('domain', I5, ' contains', I7, ' cells.')") i, numndx(i)
      call mess(LEVEL_INFO, message)
   end do

!     BEGIN DEBUG
!      other_domain=0
!      call getint('Other domain: ', other_domain)
!      call partition_setghost_params(9)
!      call partition_set_ghostlevels(other_domain,numlay_cellbased+1,numlay_nodebased+1,ierror)
!     END DEBUG

   return
end subroutine partition_to_idomain

!!> get overlapping nodes (in solver only)
!!>     {k| 1 <= ghostlev_cellbased(k) < minghostlev_s}
!!>   overlapping nodes are put in solution vector
!   subroutine partition_getoverlappingnodes()
!      use m_partitioninfo
!      use m_flowgeom, only: Ndx
!      use m_alloc
!      implicit none
!
!      integer :: iglev, k, num, nsize
!
!      nsize = 10  ! array size
!
!      if ( allocated(ioverlap) ) deallocate(ioverlap)
!      allocate(ioverlap(nsize))
!      ioverlap=0
!
!      noverlap = 0
!
!      num = size(idomain)
!
!      do k=1,num
!         if ( idomain(k).eq.my_rank ) cycle ! ghost cells only
!
!!        select nodes with 1<=cell-based ghostlevel<minghostlev_s
!         if ( ighosttype_s.eq.IGHOSTTYPE_CELLBASED ) then
!            iglev = ighostlev_cellbased(k)
!         else if ( ighosttype_s.eq.IGHOSTTYPE_NODEBASED ) then
!            iglev = ighostlev_nodebased(k)
!         else  ! combined
!            iglev = ighostlev(k)
!         end if
!
!         if ( iglev.ge.1 .and. iglev.lt.minghostlev_s ) then
!            noverlap = noverlap+1
!
!!           reallocate if necessary
!            if ( nsize.lt.noverlap ) then
!               nsize = int(1.2d0*dble(nsize)+1d0)
!               call realloc(ioverlap,nsize,keepExisting=.true.,fill=0)
!            end if
!
!            ioverlap(noverlap) = k
!         end if
!      end do
!!
!      return
!   end subroutine partition_getoverlappingnodes

subroutine writemesg(mesg)
#ifdef HAVE_MPI
   use mpi
#endif
   use m_partitioninfo

   implicit none

   character(len=*), intent(in) :: mesg

   integer :: ierr

#ifdef HAVE_MPI
   call mpi_comm_rank(DFM_COMM_DFMWORLD, my_rank, ierr)
#endif
   flush (6)

   if (my_rank == 0) then
      write (6, *) trim(mesg)
   end if

   flush (6)
#ifdef HAVE_MPI
   call MPI_barrier(DFM_COMM_DFMWORLD, ierr)
#endif
   return
end subroutine writemesg

!> reduce balences
subroutine reduce_bal(voltotal, numidx)
#ifdef HAVE_MPI
   use mpi
#endif
   use m_partitioninfo

   implicit none

   integer, intent(in) :: numidx !< which values to sum (1=discharge)
   double precision, dimension(numidx), intent(inout) :: voltotal !< cross-section data, note: ncrs from module m_monitoring_crosssections
   double precision, dimension(:), allocatable :: voltot_all
   integer :: ierror

#ifdef HAVE_MPI
!     allocate
   allocate (voltot_all(numidx))

   call mpi_allreduce(voltotal, voltot_all, numidx, mpi_double_precision, mpi_sum, DFM_COMM_DFMWORLD, ierror)

   voltotal = voltot_all

!     deallocate
   if (allocated(voltot_all)) deallocate (voltot_all)
#endif
   return
end subroutine reduce_bal

!> disable mirror cells that are not mirror cells in the whole model by setting kce=0
!!    note: partition information not available yet, need to perform a manual handshake
subroutine partition_reduce_mirrorcells(Nx, kce, ke, ierror)
   use m_partitioninfo
   use network_data
   use m_alloc
   use unstruc_messages
   use geometry_module, only: dbdistance
   use m_missing, only: dmiss
   use m_sferic, only: jsferic, jasfer3D
   use m_wall_clock_time

#ifdef HAVE_MPI
   use mpi
#endif
   implicit none

   integer, intent(in) :: Nx !< number of links
   integer, dimension(Nx), intent(inout) :: kce !< flag
   integer, dimension(Nx), intent(in) :: ke !< boundary cells

   integer, intent(out) :: ierror !< error (1) or not (0)

#ifdef HAVE_MPI
   double precision, dimension(:, :), allocatable :: xysnd ! send     cell-center coordinates (first x, then y)
   double precision, dimension(:, :), allocatable :: xyrec ! recieved cell-center coordinates (first x, then y)

   integer, dimension(:, :), allocatable :: numrequest ! number of cells requested from other domains (message size)
   integer, dimension(:), allocatable :: numrequest_loc

   integer, dimension(:), allocatable :: irequest
   integer, dimension(:), allocatable :: kcesnd
   integer, dimension(:), allocatable :: kcerec
   integer, dimension(:), allocatable :: jafound

   integer, dimension(MPI_STATUS_SIZE) :: istat

   character(len=1024) :: str

   double precision :: xL, yL, dis

   double precision :: t0, t1, t2, t3, timefind1, timefind2

   integer :: Nbnd ! number of boundary links

   integer :: i, L, k3, k4, num
   integer :: idmn, other_domain
   integer :: nrequest, itag, icount
   integer :: numfound
   integer :: istart

   integer :: numdisabled

   double precision, parameter :: dtol = 1d-4

   call wall_clock_time(t0)

   ierror = 1

   itag = 2

!     get subdomain numbers in netcell
   if (npartition_pol > 0) then
      call partition_pol_to_idomain(1, jafindcells=0)
   end if

!     allocate
   allocate (numrequest(0:ndomains - 1, 0:ndomains - 1))
   numrequest = 0
   allocate (numrequest_loc(0:ndomains - 1))
   numrequest_loc = 0
   allocate (irequest(0:2 * ndomains - 1))
!     allocate xysnd sufficiently large
   allocate (xysnd(3, numL))
   xysnd = 0d0
!     allocate kcesnd sufficiently large
   call realloc(kcesnd, numL, keepExisting=.false., fill=0)

!     count number of requested boundary links from other subdomains
   Nbnd = 0
   do L = 1, numL
      if (kce(L) == 1) then
         Nbnd = Nbnd + 1
         idmn = idomain(ke(L))
         if (idmn /= my_rank .and. idmn >= 0 .and. idmn <= ndomains - 1) then
            numrequest_loc(idmn) = numrequest_loc(idmn) + 1
         end if
      end if
   end do

!     globally reduce
   call mpi_allgather(numrequest_loc, ndomains, MPI_INTEGER, numrequest, ndomains, MPI_INTEGER, DFM_COMM_DFMWORLD, ierror)

!     send own ghost boundary net links to other domain
   nrequest = 0 ! number of outgoing requests
   istart = 1 ! start index in xysnd
   do other_domain = 0, ndomains - 1
      if (other_domain == my_rank) cycle
      num = numrequest(other_domain, my_rank)
      if (num < 1) cycle

!        get link center coordinates
      num = 0
      do L = 1, numL
         if (kce(L) == 1) then
            if (idomain(ke(L)) == other_domain) then
               k3 = kn(1, L)
               k4 = kn(2, L)
               xysnd(1, istart + num) = 0.5d0 * (xk(k3) + xk(k4))
               xysnd(2, istart + num) = 0.5d0 * (yk(k3) + yk(k4))
               xysnd(3, istart + num) = dble(kn(3, L)) ! also send type of netlink
               num = num + 1
            end if
         end if
      end do

!        send link center coordinates
      nrequest = nrequest + 1
      call mpi_isend(xysnd(1, istart), 3 * num, mpi_double_precision, other_domain, itag, DFM_COMM_DFMWORLD, irequest(nrequest), ierror)

!        update start index
      istart = istart + num
   end do

!     recieve requests from other domains
   timefind1 = 0d0 ! time spent in finding netlinks
   timefind2 = 0d0 ! time spent in finding netlinks
   istart = 1
   do other_domain = 0, ndomains - 1
      num = numrequest(my_rank, other_domain)

!        BEGIN DEBUG
!         if ( my_rank.eq.1 .and. other_domain.eq.18 ) then
!            write(6,*) 'my_rank=', my_rank, 'other_domain=', other_domain, 'num=', num, 'jafound=', jafound(i)
!         end if
!        END DEBUG

      if (num < 1) cycle
!        get message length
      call mpi_probe(other_domain, itag, DFM_COMM_DFMWORLD, istat, ierror)
      call mpi_get_count(istat, mpi_double_precision, icount, ierror)

!        check message length (safety)
      if (icount /= 3 * num) then
         write (str, *) 'partition_reduce_mirrorcells: icount.ne.3*num, domain: ', my_rank, ', other domain: ', other_domain, ' icount: ', icount, ', 3*num: ', 3 * num
         call mess(LEVEL_ERROR, str)
      end if

!        realloc
      call realloc(xyrec, (/3, num/), keepExisting=.false., fill=0d0)

!        recieve
      call mpi_recv(xyrec, icount, mpi_double_precision, other_domain, MPI_ANY_TAG, DFM_COMM_DFMWORLD, istat, ierror)

!        realloc
      call realloc(jafound, num, keepExisting=.false., fill=0)
      numfound = 0
      call wall_clock_time(t2)
      Lloop: do L = 1, numL
         if (kce(L) /= 1) cycle ! boundary links only
         if (idomain(ke(L)) /= my_rank) cycle ! in own domain only

         do i = 1, num
            if (jafound(i) == 1) cycle

            if (int(xyrec(3, i)) /= kn(3, L)) cycle ! check netlink type

!              get netlink coordinates
            k3 = kn(1, L)
            k4 = kn(2, L)
            xL = 0.5d0 * (xk(k3) + xk(k4))
            yL = 0.5d0 * (yk(k3) + yk(k4))

!              measure distance
            dis = dbdistance(xL, yL, xyrec(1, i), xyrec(2, i), jsferic, jasfer3D, dmiss)
            if (dis < dtol) then ! found
               kcesnd(istart - 1 + i) = 1
               jafound(i) = 1
               numfound = numfound + 1
               if (numfound >= num) exit Lloop
            end if
         end do
      end do Lloop
      call wall_clock_time(t3)
      timefind2 = timefind2 + t3 - t2

!!        BEGIN DEBUG
!         write(6,*) '-----------DEBUG-----------'
!         do i=1,num
!            if ( xyrec(1,i).ge.-1.87d0 .and. xyrec(1,i).le.-1.47d0 .and. xyrec(2,i).ge.63.9d0 .and. xyrec(2,i).le.64.0d0 ) then
!               write(6,*) 'my_rank=', my_rank, 'other_domain=', other_domain, 'i=', i, xyrec(1,i), xyrec(2,i), xyrec(3,i), jafound(i)
!            end if
!         end do
!
!         if ( my_rank.eq.1 .and. other_domain.eq.18 ) then
!            open(newunit=lunfil,file='temp.xyz', status='replace', action='write')
!            do i=1,num
!               write(lunfil,"(3E15.5)") xyrec(1:3,i)
!            end do
!            close(lunfil)
!            stop
!         end if
!!        END DEBUG

!        send kce to other subdomains
      nrequest = nrequest + 1
      call mpi_isend(kcesnd(istart), num, mpi_integer, other_domain, itag, DFM_COMM_DFMWORLD, irequest(nrequest), ierror)

      istart = istart + num
   end do ! other_domain=0,ndomains-1

!      write(str,"('partition_reduce_mirrorcells, time spent in finding netlinks, method 1: ', G15.5, 's.')") timefind1
!      call mess(LEVEL_INFO, trim(str))
!      write(str,"('partition_reduce_mirrorcells, time spent in finding netlinks, method 2: ', G15.5, 's.')") timefind2
!      call mess(LEVEL_INFO, trim(str))

   numdisabled = 0

!     recieve kcesnd from other domains
   do other_domain = 0, ndomains - 1
      num = numrequest(other_domain, my_rank)
      if (num < 1) cycle
!        get message length
      call mpi_probe(other_domain, itag, DFM_COMM_DFMWORLD, istat, ierror)
      call mpi_get_count(istat, mpi_integer, icount, ierror)

!        check message length (safety)
      if (icount /= num) then
         write (str, *) 'partition_reduce_mirrorcells: icount.ne.num, domain: ', my_rank, ', other domain: ', other_domain, ' icount: ', icount, ', num: ', num
         call mess(LEVEL_ERROR, str)
      end if

!        realloc
      call realloc(kcerec, icount, keepExisting=.false., fill=0)

!        recieve
      call mpi_recv(kcerec, icount, mpi_double_precision, other_domain, MPI_ANY_TAG, DFM_COMM_DFMWORLD, istat, ierror)

!        update kce
      num = 0
      do L = 1, numL
         if (kce(L) == 1) then
            if (idomain(ke(L)) == other_domain) then
               num = num + 1
               if (kce(L) /= kcerec(num)) then
!                     write(str, "('my_rank: ', I0, ' setting kce(',I0,') = ', I0, ' (was: ',I0,'). And ke(',I0,') = ', I0, ', with idomain(ke(L))=',I0,'.')") my_rank, L, kcerec(num), kce(L), L, ke(L), idomain(ke(L))
!                     call mess(LEVEL_INFO, trim(str))
                  numdisabled = numdisabled + 1
                  kce(L) = kcerec(num)
               end if
            end if
         end if
      end do
   end do

!     terminate send (safety)
   do i = 1, nrequest
      call mpi_wait(irequest(i), istat, ierror)
   end do

   if (numdisabled > 0) then
      write (str, "('disabled ', I0, ' boundary links')") numdisabled
      call mess(LEVEL_INFO, trim(str))
   end if

   call wall_clock_time(t1)

   write (str, "('partition_reduce_mirrorcells, elapsed time: ', G15.5, 's.')") t1 - t0
   call mess(LEVEL_INFO, trim(str))

   ierror = 0
1234 continue

!     deallocate
   if (allocated(irequest)) deallocate (irequest)
   if (allocated(numrequest)) deallocate (numrequest)
   if (allocated(numrequest_loc)) deallocate (numrequest_loc)
   if (allocated(xysnd)) deallocate (xysnd)
   if (allocated(xyrec)) deallocate (xyrec)
   if (allocated(kcesnd)) deallocate (kcesnd)
   if (allocated(kcerec)) deallocate (kcerec)
   if (allocated(jafound)) deallocate (jafound)

#endif
   return
end subroutine partition_reduce_mirrorcells

!> see if a discharge boundary is partitioned and set japartqbnd
subroutine set_japartqbnd()
   use fm_external_forcings_data
   use m_partitioninfo
#ifdef HAVE_MPI
   use mpi
#endif
   implicit none

   integer :: n, nq
   integer :: ki
   integer :: japartqbnd_all

   integer :: ierror

   japartqbnd = 0

#ifdef HAVE_MPI

   mnlp: do nq = 1, nqbnd
      do n = L1qbnd(nq), L2qbnd(nq) ! apart
         ki = kbndu(2, n)

         if (idomain(ki) /= my_rank) then
            japartqbnd = 1
            exit mnlp
         end if
      end do
   end do mnlp

!     not all subdomains may have detected a partitioned q boundary: reduce japartqbnd
   call mpi_allreduce(japartqbnd, japartqbnd_all, 1, mpi_integer, mpi_max, DFM_COMM_DFMWORLD, ierror)
   japartqbnd = japartqbnd_all

#endif

   return
end subroutine set_japartqbnd

!> update values in boundaries (these are not in ghost lists), 2D only
subroutine update_ghostboundvals(itype, NDIM, N, var, jacheck, ierror)
   use m_partitioninfo
   use m_flowgeom
   use m_alloc
   use m_missing
   use unstruc_messages
   implicit none

   integer, intent(in) :: itype !< type: 0: flownode, 1: flowlink
   integer, intent(in) :: NDIM !< number of unknowns per flownode/link
   integer, intent(in) :: N !< number of flownodes/links
   double precision, dimension(NDIM*N), intent(inout) :: var !< solution
   integer, intent(in) :: jacheck !< check if boundary flowlinks are updated (1) or not (0)
   integer, intent(out) :: ierror !< error (1) or not (0)

   double precision, dimension(:), allocatable :: dum

   integer, dimension(:), allocatable :: Lbndmask

   integer :: i, L

   ierror = 1

   if (jacheck == 1 .and. Lnx > Lnxi) then
!        check if all ghost boundary flowlinks are being update
      allocate (Lbndmask(1:Lnx - Lnxi + 1))
      Lbndmask = 0

!        mask updated boundary flowlinks
      do i = 1, nghostlist_u(ndomains - 1)
         L = ighostlist_u(i)
         if (L > Lnxi) then
            Lbndmask(L - Lnxi + 1) = 1
         end if
      end do

!        check if all ghost boundary flowlinks are being updated
      do L = Lnxi + 1, Lnx ! boundary links
         if (idomain(ln(2, L)) /= my_rank) then ! ghost link
            if (Lbndmask(L - Lnxi + 1) /= 1) then ! not masked
               call mess(LEVEL_ERROR, 'update_ghostboundvals: not all ghost boundary flowlinks are being updated')
               goto 1234
            end if
         end if
      end do

!        deallocate
      if (allocated(Lbndmask)) deallocate (Lbndmask)
   end if

!     allocate and initialize
   call realloc(dum, NDIM * Lnx, fill=DMISS)

!     fill internal boundary-node values with boundary values
   if (itype == ITYPE_S .or. itype == ITYPE_Sall) then
      do L = Lnxi + 1, Lnx
         do i = 1, NDIM
            dum(NDIM * (L - 1) + i) = var(NDIM * (ln(1, L) - 1) + i)
         end do
      end do
   else if (itype == ITYPE_U) then
      do L = Lnxi + 1, Lnx
         do i = 1, NDIM
            dum(NDIM * (L - 1) + i) = var(NDIM * (L - 1) + i)
         end do
      end do
   else
      call qnerror(' update_ghostboundvals: unknown ghost type', ' ', ' ')
      goto 1234
   end if

!     update ghost values
   call update_ghosts(ITYPE_U, NDIM, Lnx, dum, ierror)
   if (ierror /= 0) goto 1234

!     copy internal boundary-node values to boundary values
   if (itype == ITYPE_S .or. itype == ITYPE_Sall) then
      do L = Lnxi + 1, Lnx
         do i = 1, NDIM
            var(NDIM * (ln(1, L) - 1) + i) = dum(NDIM * (L - 1) + i)
         end do
      end do
   else if (itype == ITYPE_U) then
      do L = Lnxi + 1, Lnx
         do i = 1, NDIM
            var(NDIM * (L - 1) + i) = dum(NDIM * (L - 1) + i)
         end do
      end do
   else
      call qnerror(' update_ghostboundvals: unknown ghost type', ' ', ' ')
      goto 1234
   end if

   ierror = 0
1234 continue

!     deallocate
   if (allocated(dum)) deallocate (dum)

   return
end subroutine update_ghostboundvals

! =================================================================================================
! =================================================================================================
!> Sets weights of edges and vsize on mesh that is to be partitioned by METIS
!! If there are structures defined by polylines, then for structure related cells and edges, it gives special values to edges weights and vertex size.
!! The purpose is to avoid structures intercross partition boundaries including ghost cells.
!! NOTE: It uses "Ne/Nparts" as the special weights on structures, othere weight values can also be investigated.
!! Now ONLY support structures defined by polylines. TODO: support setting special weights on structures that are defined by other ways.
subroutine set_edge_weights_and_vsize_for_METIS(Ne, Nparts, njadj, xadj, adjncy, vsize, adjw)
   implicit none
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
   implicit none
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

!> set idomain values for all open boundary cells
subroutine set_idomain_for_all_open_boundaries()
   use fm_external_forcings_data, only: nbndz, kez, nbndu, keu, ke1d2d
   use m_sobekdfm, only: nbnd1d2d
   use m_cell_geometry, only: ndx
   use m_partitioninfo, only: idomain
   use m_alloc, only: realloc
   implicit none

   if (size(idomain) < ndx) then
      call realloc(idomain, ndx, keepExisting=.true.)
   end if
   call set_idomain_for_open_boundary_points(nbndz, size(kez), kez, ndx, idomain)
   call set_idomain_for_open_boundary_points(nbndu, size(keu), keu, ndx, idomain)
   call set_idomain_for_open_boundary_points(nbnd1d2d, size(ke1d2d), ke1d2d, ndx, idomain)

end subroutine set_idomain_for_all_open_boundaries

!> set idomain values for a set of open boundary cells
subroutine set_idomain_for_open_boundary_points(number_of_boundary_points, links_array_size, &
                                                links_to_boundary_points, ndx, idomain)
   use m_flowgeom, only: ln, lne2ln
   implicit none

   integer, intent(in) :: number_of_boundary_points !< number of boundary points
   integer, intent(in) :: links_array_size !< size of the links array
   integer, intent(in) :: links_to_boundary_points(links_array_size) !< links to boundary cells
   integer, intent(in) :: ndx !< number of flow nodes (internal + boundary)
   integer, intent(inout) :: idomain(ndx) !< cell-based domain number

   integer :: boundary_cell, boundary_point_number, internal_cell, link

   do boundary_point_number = 1, number_of_boundary_points
      link = links_to_boundary_points(boundary_point_number)
      boundary_cell = ln(1, lne2ln(link))
      internal_cell = ln(2, lne2ln(link))
      idomain(boundary_cell) = idomain(internal_cell)
   end do

end subroutine set_idomain_for_open_boundary_points
