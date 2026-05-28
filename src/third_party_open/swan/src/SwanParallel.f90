! This file contains data and routines for high performance computing based on parallel distributed-memory paradigm
!
! --- supplementary to swanparll.f ---
!
module SwanParallel
!
!   --|-----------------------------------------------------------|--
!     | Delft University of Technology                            |
!     | Faculty of Civil Engineering and Geosciences              |
!     | Environmental Fluid Mechanics Section                     |
!     | P.O. Box 5048, 2600 GA  Delft, The Netherlands            |
!     |                                                           |
!     | Programmer: Marcel Zijlema                                |
!   --|-----------------------------------------------------------|--
!
!
!     SWAN (Simulating WAves Nearshore); a third generation wave model
!     Copyright (C) 1993-2024  Delft University of Technology
!
!     This program is free software: you can redistribute it and/or modify
!     it under the terms of the GNU General Public License as published by
!     the Free Software Foundation, either version 3 of the License, or
!     (at your option) any later version.
!
!     This program is distributed in the hope that it will be useful,
!     but WITHOUT ANY WARRANTY; without even the implied warranty of
!     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
!     GNU General Public License for more details.
!
!     You should have received a copy of the GNU General Public License
!     along with this program. If not, see <http://www.gnu.org/licenses/>.
!
!
!   Authors
!
!   42.10: Marcel Zijlema
!
!   Updates
!
!   42.10, June 2023: New subroutine
!
!   Purpose
!
!   Module containing data for parallel computing using Metis and MPI
!
!   Method
!
!   Data with respect to grid partition properties based on Metis
!
!   Modules used
!
!   none
!
   implicit none
!
!   Module variables
!
   ! size of integers and reals (must match those of metis.h)
   !
   integer, parameter :: kint = 4 ! size of integer
   integer, parameter :: krea = 4 ! size of real
   !
   ! relevant constants taken from metis.h
   !
   integer(kind=kint), parameter :: METIS_NOPTIONS = 40 ! number of options used in Metis
   !
   integer(kind=kint), parameter :: METIS_OK = 1 ! returned normally
   integer(kind=kint), parameter :: METIS_ERROR_INPUT = -2 ! returned due to erroneous inputs and/or options
   integer(kind=kint), parameter :: METIS_ERROR_MEMORY = -3 ! returned due to insufficient memory
   integer(kind=kint), parameter :: METIS_ERROR = -4 ! some other errors
   !
   ! data structures to be used for Metis
   !
   integer(kind=kint), dimension(:), save, allocatable :: ipown ! array giving the subdomain number assigned to each grid vertex
   !
   ! data structures belonging to own sudomain
   !
   logical, dimension(:), save, allocatable :: vres ! vertex-based identification list
   ! = .true. ; resident vertex
   ! = .false.; ghost vertex
   !
   ! data structures to be used for MPI communication
   !

   integer :: maxrvg ! maximum number of ghost vertices to receive of any communicating subdomains
   integer :: maxsvg ! maximum number of ghost vertices to send of any communicating subdomains
   !
   integer :: nvcomm ! number of subdomains communicating with current subdomain
   !
   integer, dimension(:), save, allocatable :: vsubcm ! vertex-based list of communicating subdomains
   !
   integer, dimension(:, :), save, allocatable :: ivrecv ! vertex-based receive list
   ! = (i,j);  local index of i-th ghost vertex to receive from neighbour subdomain j
   integer, dimension(:, :), save, allocatable :: ivsend ! vertex-based send list
   ! = (i,j);  local index of i-th ghost vertex to send to neighbour subdomain j
   integer, dimension(:), save, allocatable :: nvrecv ! number of ghost vertices to receive in each communicating subdomain
   integer, dimension(:), save, allocatable :: nvsend ! number of ghost vertices to send in each communicating subdomain
   !
   integer, dimension(:, :), save, allocatable :: irbuf ! buffer to store integers to receive temporarily
   integer, dimension(:, :), save, allocatable :: isbuf ! buffer to store integers to send temporarily
   integer, dimension(:), save, allocatable :: rrqst ! request handles for the MPI_IRECV command
   integer, dimension(:), save, allocatable :: srqst ! request handles for the MPI_ISEND command
   !
   real, dimension(:, :), save, allocatable :: rbuf ! buffer to store reals to receive temporarily
   real, dimension(:, :), save, allocatable :: sbuf ! buffer to store reals to send temporarily
!
!   Source text
!
contains
!
#ifdef USE_MPI
   integer function SwanParallelComm()
!
!   Return the communicator used by SWAN in the current build mode.
!
      use mpi
#ifndef SWANEXE
      use m_parall, only: COMM
#endif
      implicit none
!
#ifdef SWANEXE
      SwanParallelComm = MPI_COMM_WORLD
#else
      SwanParallelComm = COMM
#endif
!
   end function SwanParallelComm
!
#endif
!
   subroutine SwanDecomposition(logcom)
!
!   --|-----------------------------------------------------------|--
!     | Delft University of Technology                            |
!     | Faculty of Civil Engineering and Geosciences              |
!     | Environmental Fluid Mechanics Section                     |
!     | P.O. Box 5048, 2600 GA  Delft, The Netherlands            |
!     |                                                           |
!     | Programmer: Marcel Zijlema                                |
!   --|-----------------------------------------------------------|--
!
!
!     SWAN (Simulating WAves Nearshore); a third generation wave model
!     Copyright (C) 1993-2024  Delft University of Technology
!
!     This program is free software: you can redistribute it and/or modify
!     it under the terms of the GNU General Public License as published by
!     the Free Software Foundation, either version 3 of the License, or
!     (at your option) any later version.
!
!     This program is distributed in the hope that it will be useful,
!     but WITHOUT ANY WARRANTY; without even the implied warranty of
!     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
!     GNU General Public License for more details.
!
!     You should have received a copy of the GNU General Public License
!     along with this program. If not, see <http://www.gnu.org/licenses/>.
!
!
!   Authors
!
!   42.10: Marcel Zijlema
!
!   Updates
!
!   42.10, June 2023: New subroutine
!
!   Purpose
!
!   Carries out domain decomposition meant for distributed-memory approach
!
!   Method
!
!   Carry out the partitioning of the unstructured mesh first and then do the administration for MPI communication
!
!   Modules used
!
      use ocpcomm4
      use m_parall
      use SwanGriddata
!
      implicit none
!
!   Argument variables
!
      logical, dimension(7), intent(inout) :: logcom ! indicates which commands have been given to know if all the
      ! information for a certain command is available. Meaning:
      ! (1) no meaning
      ! (2) command CGRID has been carried out
      ! (3) command READINP BOTTOM has been carried out
      ! (4) command READ COOR has been carried out
      ! (5) command READ UNSTRUC has been carried out
      ! (6) arrays s1, u1 and v1 have been allocated
      ! (7) mesh partitioning has been carried out
!
!   Local variables
!
      integer, save :: ient = 0 ! number of entries in this subroutine
      logical :: STPNOW ! indicates that program must stop
!
!   Structure
!
!   Description of the pseudo code
!
!   Source text
!
      if (ltrace) call strace(ient, 'SwanDecomposition')
      !
      if (.not. PARLL) return
      !
      ! store sizes of the global computational mesh
      !
      nvertsg = nverts
      ncellsg = ncells
      !
      ! carry out the mesh partitioning
      !
      if (allocated(ipown)) deallocate(ipown)
      allocate (ipown(nvertsg))
      ipown = 0
      call SwanMeshPartition
      if (STPNOW()) return
      !
      ! carry out the administration
      !
      call SwanCommAdmin
      if (STPNOW()) return
      !
      logcom(7) = .true.
      !
   end subroutine SwanDecomposition
!
   subroutine SwanMeshPartition
!
!   --|-----------------------------------------------------------|--
!     | Delft University of Technology                            |
!     | Faculty of Civil Engineering and Geosciences              |
!     | Environmental Fluid Mechanics Section                     |
!     | P.O. Box 5048, 2600 GA  Delft, The Netherlands            |
!     |                                                           |
!     | Programmer: Marcel Zijlema                                |
!   --|-----------------------------------------------------------|--
!
!
!     SWAN (Simulating WAves Nearshore); a third generation wave model
!     Copyright (C) 1993-2024  Delft University of Technology
!
!     This program is free software: you can redistribute it and/or modify
!     it under the terms of the GNU General Public License as published by
!     the Free Software Foundation, either version 3 of the License, or
!     (at your option) any later version.
!
!     This program is distributed in the hope that it will be useful,
!     but WITHOUT ANY WARRANTY; without even the implied warranty of
!     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
!     GNU General Public License for more details.
!
!     You should have received a copy of the GNU General Public License
!     along with this program. If not, see <http://www.gnu.org/licenses/>.
!
!
!   Authors
!
!   42.10: Marcel Zijlema
!
!   Updates
!
!   42.10, June 2023: New subroutine
!
!   Purpose
!
!   Carries out the partitioning of the unstructured triangular mesh
!
!   Method
!
!   the Metis graph partitioning library is used to partition the given mesh among the subdomains
!   using multilevel k-way partitioning and places the subdomain numbers into the ipown array
!
!   Note: to compute adjacency lists efficient, the algorithm of metis.F taken from the ADCIRC suite is employed
!
!   Modules used
!
      use ocpcomm2
      use ocpcomm4
      use m_parall
      use SwanGriddata
!
      implicit none
!
!   Parameter variables
!
      integer, parameter :: nov = 3 ! number of vertices in cell (triangles only)
      integer(kind=kint), parameter :: ncon = 1 ! number of balancing constraints
      real(kind=krea), dimension(ncon), parameter :: ubvec = 1.001 ! specifies the allowed load imbalance for each constraint
!
!   Local variables
!
      integer :: i ! loop counter
      integer, save :: ient = 0 ! number of entries in this subroutine
      integer(kind=kint) :: ierr ! Metis error indicator
      integer :: iostat ! I/O status in call FOR
      integer :: istat ! indicate status of allocation
      integer(kind=kint) :: ivert ! vertex index
      integer :: j ! loop counter
      integer(kind=kint) :: jvert ! index of neighbouring vertex
      integer(kind=kint) :: k ! counter
      integer(kind=kint) :: maxfv ! maximum number of faces for any vertex
      integer :: ndsd ! unit reference number of file
      integer(kind=kint) :: ne ! number of edges in the graph
      integer(kind=kint) :: nf2 ! twice the number of faces
      integer(kind=kint) :: np ! number of partitions
      integer(kind=kint) :: nv ! number of vertices in the graph
      integer(kind=kint) :: edgecut ! total edges cut of the partitioning
      !
      integer(kint), dimension(0:METIS_NOPTIONS - 1) :: options ! Metis options as described in Section 5.4 of the Metis manual v 5.0
      !
      integer(kind=kint), dimension(:), allocatable :: adjncy ! adjacency structure of the graph as described in Section 5.5 of the Metis manual v 5.0
      integer(kind=kint), dimension(:), allocatable :: adjw ! weights of the faces as described in Section 5.5 of the Metis manual v 5.0
      integer(kind=kint), dimension(:, :), allocatable :: covts ! adjacency list of neighbouring vertices for each vertex
      ! = (j,i); index of j-th neighbouring vertex linked to vertex i
      integer(kind=kint), dimension(:), allocatable :: nfacev ! number of faces for each vertex
      integer(kind=kint), dimension(:), allocatable :: vneigh ! sorted neighbouring vertices
      integer(kind=kint), dimension(:), allocatable :: vsize ! size of the vertices for computing the total communication volume as described in Section 5.7 of the Metis manual v 5.0
      ! (not used)
      integer(kind=kint), dimension(:), allocatable :: vwgt ! weights of the vertices as described in Section 5.5 of the Metis manual v 5.0
      integer(kind=kint), dimension(:), allocatable :: xadj ! adjacency structure of the graph as described in Section 5.5 of the Metis manual v 5.0
      !
      real(kind=krea), dimension(:), allocatable :: tpwgts ! specifies the desired weight for each partition and constraint
      !
      character(120) :: msgstr ! string to pass message
      !
      logical :: found ! true if desired vertex for symmetry check is found
      logical :: symmetric ! true if adjacency vertex matrix is symmetric
      !
      integer(kind=kint), external :: METIS_PartGraphKway ! function to partition a graph into a number of parts using multilevel k-way partitioning
      integer(kind=kint), external :: METIS_SetDefaultOptions ! function to set default Metis options
!
!   Structure
!
!   Description of the pseudo code
!
!   Source text
!
      if (ltrace) call strace(ient, 'SwanMeshPartition')
      !
      ! set size of input graph based on vertices of the global mesh
      !
      nv = nvertsg
      !
      ! set number of partitions equal to the number of parallel cores
      !
      np = NPROC
      !
      if (ITEST >= 50 .and. IAMMASTER) then
         write (PRINTF, '(a)')
         write (PRINTF, 201) nv, np
      end if
      !
      ! allocate data for mesh partitioning
      !
      istat = 0
      if (.not. allocated(xadj)) allocate (xadj(1 + nv), stat=istat)
      if (istat == 0 .and. .not. allocated(vwgt)) allocate (vwgt(nv), stat=istat)
      if (istat == 0 .and. .not. allocated(vsize)) allocate (vsize(nv), stat=istat)
      if (istat == 0 .and. .not. allocated(tpwgts)) allocate (tpwgts(np), stat=istat)
      if (istat == 0 .and. .not. allocated(nfacev)) allocate (nfacev(nv), stat=istat)
      if (istat == 0 .and. .not. allocated(vneigh)) allocate (vneigh(nv), stat=istat)
      !
      ne = 0
      nfacev = 0
      !
      ! compute the number of faces for each vertex
      !
      do j = 1, nov
         do i = 1, ncellsg
            ivert = kvertc(j, i)
            ne = ne + 2
            k = nfacev(ivert) + 2
            nfacev(ivert) = k
         end do
      end do
      !
      ! compute the maximum number of faces for any vertex
      !
      maxfv = 0
      do i = 1, nvertsg
         if (nfacev(i) > maxfv) maxfv = nfacev(i)
      end do
      !
      ! allocate remaining data for mesh partitioning
      !
      if (istat == 0 .and. .not. allocated(adjncy)) allocate (adjncy(ne), stat=istat)
      if (istat == 0 .and. .not. allocated(adjw)) allocate (adjw(ne), stat=istat)
      if (istat == 0 .and. .not. allocated(covts)) allocate (covts(maxfv, nv), stat=istat)
      !
      if (istat /= 0) then
         write (msgstr, '(a,i6)') 'allocation problem: mesh partitioning data and return code is ', istat
         call msgerr(4, trim(msgstr))
         return
      end if
      !
      ! compute list of neighbouring vertices and number of faces containing a vertex
      !
      nfacev = 0
      !
      do j = 1, nov
         do i = 1, ncellsg
            ivert = kvertc(j, i)
            covts(nfacev(ivert) + 1, ivert) = kvertc(mod(j, 3) + 1, i)
            covts(nfacev(ivert) + 2, ivert) = kvertc(mod(j + 1, 3) + 1, i)
            k = nfacev(ivert) + 2
            nfacev(ivert) = k
         end do
      end do
      !
      ! remove multiple entries from the adjacency vertex list
      !
      nf2 = 0
      do i = 1, nvertsg
         do j = 1, nfacev(i)
            vneigh(j) = covts(j, i)
         end do
         if (nfacev(i) > 1) then
            k = nfacev(i)
            call hpsort(k, vneigh)
            jvert = vneigh(1)
            covts(1, i) = jvert
            k = 1
            do j = 2, nfacev(i)
               if (vneigh(j) /= jvert) then
                  k = k + 1
                  jvert = vneigh(j)
                  covts(k, i) = jvert
               end if
            end do
         else
            write (msgstr, '(a,i7)') 'vertex ', i, ' is isolated'
            call msgerr(4, trim(msgstr))
            return
         end if
         nfacev(i) = k
         nf2 = nf2 + k
      end do
      if (ITEST >= 50 .and. IAMMASTER) write (PRTEST, 202) nf2 / 2
      !
      ! check that adjacency matrix is symmetric
      !
      symmetric = .true.
      !
      do i = 1, nvertsg
         do j = 1, nfacev(i)
            jvert = covts(j, i)
            found = .false.
            do k = 1, nfacev(jvert)
               if (covts(k, jvert) == i) then
                  found = .true.
                  exit
               end if
            end do
            if (.not. found) then
               symmetric = .false.
               write (msgstr, '(a,i7,a,i7,a)') 'vertex ', i, ' adjacent to ', jvert, ' but not vice versa'
               call msgerr(1, trim(msgstr))
            end if
         end do
      end do
      !
      if (.not. symmetric) then
         call msgerr(4, 'inconsistency found in SwanMeshPartition: adjacency matrix is not symmetric')
         return
      end if
      !
      ! compute weights of the graph vertices
      !
      vwgt = nfacev
      !
      ! compute adjacency list of graph and its face weights
      !
      xadj(1) = 1
      !
      k = 0
      !
      do ivert = 1, nvertsg
         do j = 1, nfacev(ivert)
            jvert = covts(j, ivert)
            k = k + 1
            adjncy(k) = jvert
            adjw(k) = vwgt(ivert) + vwgt(jvert)
         end do
         xadj(ivert + 1) = k + 1
      end do
      !
      ! by default, the Metis numbering starts from zero
      !
      xadj = xadj - 1
      adjncy = adjncy - 1
      !
      ! vsize not used, set to NULL
      !
      vsize = 0
      !
      ! the graph will be equally distributed among the processors
      !
      tpwgts = 1./real(np)
      !
      ! set the Metis defaults, in particular edge-cut minimization and C-style numbering
      !
      ierr = METIS_SetDefaultOptions(options)
      if (ierr /= METIS_OK) then
         if (ierr == METIS_ERROR_INPUT) then
            write (msgstr, '(a)') 'function METIS_SetDefaultOptions failed due to erroneous inputs or options'
         else if (ierr == METIS_ERROR_MEMORY) then
            write (msgstr, '(a)') 'function METIS_SetDefaultOptions failed due to insufficient memory'
         else if (ierr == METIS_ERROR) then
            write (msgstr, '(a)') 'function METIS_SetDefaultOptions failed with another Metis error'
         else
            write (msgstr, '(a,i2)') 'function METIS_SetDefaultOptions failed with unknown error: ', ierr
         end if
         call msgerr(4, trim(msgstr))
         return
      end if
      !
      ! partition the graph and create the ipown array
      !
      ierr = METIS_PartGraphKway(nv, ncon, xadj, adjncy, vwgt, vsize, adjw, np, tpwgts, ubvec, options, edgecut, ipown)
      !
      if (ierr /= METIS_OK) then
         if (ierr == METIS_ERROR_INPUT) then
            write (msgstr, '(a)') 'function METIS_PartGraphKway failed due to erroneous inputs or options'
         else if (ierr == METIS_ERROR_MEMORY) then
            write (msgstr, '(a)') 'function METIS_PartGraphKway failed due to insufficient memory'
         else if (ierr == METIS_ERROR) then
            write (msgstr, '(a)') 'function METIS_PartGraphKway failed with another Metis error'
         else
            write (msgstr, '(a,i2)') 'function METIS_PartGraphKway failed with unknown error: ', ierr
         end if
         call msgerr(4, trim(msgstr))
         return
      end if
      !
      ! use Fortran-style numbering
      !
      ipown = ipown + 1
      !
      ! deallocate auxiliary arrays
      !
      deallocate (xadj, adjncy, vwgt, vsize, adjw, tpwgts, nfacev, covts, vneigh)
      !
      if (ITEST >= 50 .and. IAMMASTER) then
         write (PRTEST, 203) edgecut
         write (PRINTF, '(a)') ' writing mesh partition to file partit.mesh'
         ndsd = 0
         iostat = 0
         FILENM = 'partit.mesh'
         call FOR(ndsd, FILENM, 'UF', iostat)
         do i = 1, nvertsg
            write (ndsd, '(i6)') ipown(i)
         end do
         close (ndsd)
      end if
      !
201   format(' unstructured mesh containing ', i7, ' vertices is partitioned into ', i4, ' subdomains')
202   format(' total number of faces found during mesh partitioning = ', i7)
203   format(' total edges cut = ', i8)
      !
   end subroutine SwanMeshPartition
!
   subroutine SwanCommAdmin
!
!   --|-----------------------------------------------------------|--
!     | Delft University of Technology                            |
!     | Faculty of Civil Engineering and Geosciences              |
!     | Environmental Fluid Mechanics Section                     |
!     | P.O. Box 5048, 2600 GA  Delft, The Netherlands            |
!     |                                                           |
!     | Programmer: Marcel Zijlema                                |
!   --|-----------------------------------------------------------|--
!
!
!     SWAN (Simulating WAves Nearshore); a third generation wave model
!     Copyright (C) 1993-2024  Delft University of Technology
!
!     This program is free software: you can redistribute it and/or modify
!     it under the terms of the GNU General Public License as published by
!     the Free Software Foundation, either version 3 of the License, or
!     (at your option) any later version.
!
!     This program is distributed in the hope that it will be useful,
!     but WITHOUT ANY WARRANTY; without even the implied warranty of
!     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
!     GNU General Public License for more details.
!
!     You should have received a copy of the GNU General Public License
!     along with this program. If not, see <http://www.gnu.org/licenses/>.
!
!
!   Authors
!
!   42.10: Marcel Zijlema
!
!   Updates
!
!   42.10, June 2023: New subroutine
!
!   Purpose
!
!   Carries out the administration for message passing
!
!   Method
!
!   - subdomains need to be augmented with a layer of ghost vertices and cells
!   - create data structures for message passing
!   - create connectivity table for each subdomain
!
!   The algorithm for setting up the data structures is copied from decomp.F of the ADCIRC suite
!
!   Modules used
!
      use ocpcomm4
      use m_parall
      use SwanGriddata
!
      implicit none
!
!   Parameter variables
!
      integer, parameter :: nov = 3 ! number of vertices in cell (triangles only)
!
!   Local variables
!
      integer :: i ! loop counter
      integer, dimension(2, NPROC) :: iarrc ! auxiliary array for collecting data
      integer, dimension(2) :: iarrl ! auxiliary array containing some data of each subdomain
      integer :: icell ! global cell index
      integer, save :: ient = 0 ! number of entries in this subroutine
      integer :: ip ! actual subdomain number
      integer :: ip1 ! subdomain number of first vertex
      integer :: ip2 ! subdomain number of second vertex
      integer :: ip3 ! subdomain number of third vertex
      integer :: istat ! indicate status of allocation
      integer :: ivert ! global vertex index
      integer :: j ! loop counter
      integer :: jvert ! local vertex index
      integer :: k ! (loop) counter
      integer :: maxncp ! maximum number of cells of any subdomain
      integer :: maxnvp ! maximum number of vertices of any subdomain
      integer, dimension(NPROC) :: ncellp ! number of cells assigned to each subdomain including ghost cells
      integer :: nl ! length of unsorted list
      integer, dimension(NPROC) :: nvertp ! number of vertices assigned to each subdomain including ghost vertices
      integer, dimension(NPROC) :: nvresp ! number of resident vertices in each subdomain
      integer, dimension(NPROC) :: plistcm ! list of communicating subdomains in current subdomain
      integer :: v1 ! first vertex of present cell
      integer :: v2 ! second vertex of present cell
      integer :: v3 ! third vertex of present cell
      !
      integer, dimension(:, :), allocatable :: clistlg ! local-to-global cell-based list
      ! = (j,i);  global cell index of local cell j in subdomain i
      integer, dimension(:), allocatable :: ivertl ! local vertex index of global vertex
      integer, dimension(:), allocatable :: jlist ! auxiliary array to store a list of entries temporarily
      integer, dimension(:, :), allocatable :: kvtcp ! connectivity table in current subdomain
      integer, dimension(:, :), allocatable :: vlistlg ! local-to-global vertex-based list
      ! = (j,i); global vertex index of local vertex j in subdomain i
      !
      logical :: stpnow ! indicate whether program must be terminated or not
      !
      character(120) :: msgstr ! string to pass message
      !
      type clistpt ! linked list for local-to-global cell-based list
         integer :: clg
         type(clistpt), pointer :: nextcl
      end type clistpt
      type(clistpt), target :: frstcl
      type(clistpt), pointer :: currcl, tmpcl
      !
      type vlistpt ! linked list for local-to-global vertex-based list
         integer :: vlg
         type(vlistpt), pointer :: nextvl
      end type vlistpt
      type(vlistpt), target :: frstvl
      type(vlistpt), pointer :: currvl, tmpvl
      !
      type rlistpt ! linked list for receive list
         integer :: jrl
         type(rlistpt), pointer :: nextrl
      end type rlistpt
      type(rlistpt), target :: frstrl
      type(rlistpt), pointer :: currrl, tmprl
      !
      type slistpt ! linked list for send list
         integer :: jsl
         type(slistpt), pointer :: nextsl
      end type slistpt
      type(slistpt), target :: frstsl
      type(slistpt), pointer :: currsl, tmpsl
!
!   Structure
!
!   Description of the pseudo code
!
!   Source text
!
      if (ltrace) call strace(ient, 'SwanCommAdmin')
      !
      istat = 0
      if (.not. allocated(jlist)) allocate (jlist(nov * ncellsg), stat=istat)
      if (istat == 0 .and. .not. allocated(ivertl)) allocate (ivertl(nvertsg), stat=istat)
      !
      if (istat /= 0) then
         write (msgstr, '(a,i6)') 'allocation problem: comm administration data and return code is ', istat
         call msgerr(4, trim(msgstr))
         return
      end if
      ivertl = 0
      !
      ! use Metis partition to compute the number of resident vertices in each subdomain
      !
      nvresp = 0
      !
      do i = 1, nvertsg
         k = nvresp(ipown(i)) + 1
         nvresp(ipown(i)) = k
      end do
      !
      ! create local-to-global list containing global cell index for each local cell
      ! by adding a cell to the list if it has a resident vertex
      ! in effect, a layer of ghost cells is included
      !
      maxncp = 0
      !
      frstcl%clg = 0
      nullify (frstcl%nextcl)
      currcl => frstcl
      do i = 1, NPROC
         ncellp(i) = 0
         do j = 1, ncellsg
            v1 = kvertc(1, j)
            v2 = kvertc(2, j)
            v3 = kvertc(3, j)
            ip1 = ipown(v1)
            ip2 = ipown(v2)
            ip3 = ipown(v3)
            if (ip1 == i .or. ip2 == i .or. ip3 == i) then
               ncellp(i) = ncellp(i) + 1
               allocate (tmpcl)
               tmpcl%clg = j
               nullify (tmpcl%nextcl)
               currcl%nextcl => tmpcl
               currcl => tmpcl
            end if
         end do
         if (ncellp(i) > maxncp) maxncp = ncellp(i)
      end do
      !
      allocate (clistlg(maxncp, NPROC))
      clistlg = 0
      !
      currcl => frstcl%nextcl
      do i = 1, NPROC
         do j = 1, ncellp(i)
            clistlg(j, i) = currcl%clg
            currcl => currcl%nextcl
         end do
      end do
      deallocate (tmpcl)
      !
      ncells = ncellp(INODE)
      !
      ! using local-to-global cell-based list create local-to-global vertex-based list
      ! and construct global-to-local vertex-based list
      !
      maxnvp = 0
      !
      frstvl%vlg = 0
      nullify (frstvl%nextvl)
      currvl => frstvl
      do i = 1, NPROC
         nl = 0
         do j = 1, ncellp(i)
            icell = clistlg(j, i)
            do k = 1, nov
               nl = nl + 1
               jlist(nl) = kvertc(k, icell)
            end do
         end do
         ! sort and remove multiple entries from vertex list
         call hpsort(nl, jlist)
         k = 1
         ivert = jlist(1)
         allocate (tmpvl)
         tmpvl%vlg = ivert
         nullify (tmpvl%nextvl)
         currvl%nextvl => tmpvl
         currvl => tmpvl
         if (ipown(ivert) == i) ivertl(ivert) = k
         do j = 2, nl
            if (jlist(j) /= ivert) then
               k = k + 1
               ivert = jlist(j)
               allocate (tmpvl)
               tmpvl%vlg = ivert
               nullify (tmpvl%nextvl)
               currvl%nextvl => tmpvl
               currvl => tmpvl
               if (ipown(ivert) == i) ivertl(ivert) = k
            end if
         end do
         nvertp(i) = k
         if (nvertp(i) > maxnvp) maxnvp = nvertp(i)
      end do
      !
      allocate (vlistlg(maxnvp, NPROC))
      vlistlg = 0
      !
      currvl => frstvl%nextvl
      do i = 1, NPROC
         do j = 1, nvertp(i)
            vlistlg(j, i) = currvl%vlg
            currvl => currvl%nextvl
         end do
      end do
      deallocate (tmpvl)
      !
      nverts = nvertp(INODE)
      !
      allocate (ivertg(nverts))
      ivertg(1:nverts) = vlistlg(1:nverts, INODE)
      !
      ! create identification list for vertices (resident/ghost)
      !
      allocate (vres(nverts))
      !
      do j = 1, nverts
         ivert = ivertg(j)
         ip = ipown(ivert)
         if (ip == INODE) then
            vres(j) = .true.
         else
            vres(j) = .false.
         end if
      end do
      !
      ! create vertex-based list of communicating subdomains
      !
      nvcomm = 0
      plistcm = 0
      !
      nl = 0
      do j = 1, nverts
         ivert = ivertg(j)
         ip = ipown(ivert)
         if (ip /= INODE) then
            nl = nl + 1
            jlist(nl) = ip
         end if
      end do
      if (nl == 0) then
         nvcomm = 0
      else
         ! sort and remove duplicates
         call hpsort(nl, jlist)
         k = 1
         ip = jlist(1)
         plistcm(1) = ip
         do j = 2, nl
            if (jlist(j) /= ip) then
               k = k + 1
               ip = jlist(j)
               plistcm(k) = ip
            end if
         end do
         nvcomm = k
      end if
      !
      allocate (vsubcm(nvcomm))
      do j = 1, nvcomm
         vsubcm(j) = plistcm(j)
      end do
      !
      ! create vertex-based receive list
      !
      allocate (nvrecv(nvcomm))
      maxrvg = 0
      !
      frstrl%jrl = 0
      nullify (frstrl%nextrl)
      currrl => frstrl
      do j = 1, nvcomm
         ip = vsubcm(j)
         nvrecv(j) = 0
         do jvert = 1, nverts
            ivert = ivertg(jvert)
            if (ipown(ivert) == ip) then
               nvrecv(j) = nvrecv(j) + 1
               allocate (tmprl)
               tmprl%jrl = jvert
               nullify (tmprl%nextrl)
               currrl%nextrl => tmprl
               currrl => tmprl
            end if
         end do
         if (nvrecv(j) > maxrvg) maxrvg = nvrecv(j)
      end do
      !
      allocate (ivrecv(maxrvg, nvcomm))
      ivrecv = 0
      !
      currrl => frstrl%nextrl
      do j = 1, nvcomm
         do i = 1, nvrecv(j)
            ivrecv(i, j) = currrl%jrl
            currrl => currrl%nextrl
         end do
      end do
      deallocate (tmprl)
      !
      ! create vertex-based send list
      !
      allocate (nvsend(nvcomm))
      maxsvg = 0
      !
      frstsl%jsl = 0
      nullify (frstsl%nextsl)
      currsl => frstsl
      do j = 1, nvcomm
         ip = vsubcm(j)
         nvsend(j) = 0
         do k = 1, nvertp(ip)
            ivert = vlistlg(k, ip)
            if (ipown(ivert) == INODE) then
               nvsend(j) = nvsend(j) + 1
               allocate (tmpsl)
               tmpsl%jsl = ivertl(ivert)
               nullify (tmpsl%nextsl)
               currsl%nextsl => tmpsl
               currsl => tmpsl
            end if
         end do
         if (nvsend(j) > maxsvg) maxsvg = nvsend(j)
      end do
      !
      allocate (ivsend(maxsvg, nvcomm))
      ivsend = 0
      !
      currsl => frstsl%nextsl
      do j = 1, nvcomm
         do i = 1, nvsend(j)
            ivsend(i, j) = currsl%jsl
            currsl => currsl%nextsl
         end do
      end do
      deallocate (tmpsl)
      !
      ! reconstruct connectivity table for current subdomain
      !
      allocate (kvtcp(3, ncells))
      kvtcp = 0
      !
      ! create global-to-local vertex lookup including ghost ones
      ivertl = 0
      do j = 1, nverts
         ivertl(ivertg(j)) = j
      end do
      ! look for vertices for each local cell and put them in table
      do j = 1, ncells
         icell = clistlg(j, INODE)
         do k = 1, nov
            ivert = kvertc(k, icell)
            jvert = ivertl(ivert)
            if (jvert > 0) then
               kvtcp(k, j) = jvert
            else
               write (msgstr, '(a,i7,a,i7)') 'inconsistency found in SwanCommAdmin: global vertex ', ivert, &
                                           & ' missing in subdomain ', INODE
               call msgerr(4, trim(msgstr))
               return
            end if
         end do
      end do
      !
      ! delete other parts of connectivity table while keeping local table
      !
      deallocate (kvertc)
      allocate (kvertc(3, ncells))
      kvertc = kvtcp
      !
      ! create copy of parts of vmark for each subdomain
      !
      ! use array ivertl to store vmark temporarily
      ivertl = vmark
      deallocate (vmark)
      allocate (vmark(nverts))
      !
      do jvert = 1, nverts
         ivert = ivertg(jvert)
         vmark(jvert) = ivertl(ivert)
         ! ghost vertices are marked with exception value
         if (.not. vres(jvert)) vmark(jvert) = excmark
      end do
      !
      ! allocate arrays for MPI communication
      !
      allocate (rrqst(nvcomm))
      allocate (srqst(nvcomm))
      allocate (irbuf(maxrvg, nvcomm))
      allocate (isbuf(maxsvg, nvcomm))
      allocate (rbuf(maxrvg, nvcomm))
      allocate (sbuf(maxsvg, nvcomm))
      !
      deallocate (ivertl, jlist, kvtcp, clistlg, vlistlg)
      !
      iarrl(1) = nvcomm
      iarrl(2) = sum(nvrecv)
      call SWGATHER(iarrc, 2 * NPROC, iarrl, 2, SWINT)
      if (STPNOW()) return
      !
      if (ITEST >= 50 .and. IAMMASTER) then
         write (PRINTF, '(a)')
         write (PRINTF, '(a)') ' communication data'
         write (PRINTF, '(a)') ' subdomain  # communicating PEs  comm. vol/calc. vol (%)'
         write (PRINTF, '(a)') ' ---------  -------------------  -----------------------'
         do i = 1, NPROC
            write (PRINTF, 201) i, iarrc(1, i), (real(iarrc(2, i)) / real(nvresp(i))) * 100.
         end do
         write (PRINTF, '(a)')
      end if
      !
201   format(1x, i9, 2x, i19, 2x, f8.2)
      !
   end subroutine SwanCommAdmin
!
   subroutine SwanUvExchgI(ifld)
!
!   --|-----------------------------------------------------------|--
!     | Delft University of Technology                            |
!     | Faculty of Civil Engineering and Geosciences              |
!     | Environmental Fluid Mechanics Section                     |
!     | P.O. Box 5048, 2600 GA  Delft, The Netherlands            |
!     |                                                           |
!     | Programmer: Marcel Zijlema                                |
!   --|-----------------------------------------------------------|--
!
!
!     SWAN (Simulating WAves Nearshore); a third generation wave model
!     Copyright (C) 1993-2024  Delft University of Technology
!
!     This program is free software: you can redistribute it and/or modify
!     it under the terms of the GNU General Public License as published by
!     the Free Software Foundation, either version 3 of the License, or
!     (at your option) any later version.
!
!     This program is distributed in the hope that it will be useful,
!     but WITHOUT ANY WARRANTY; without even the implied warranty of
!     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
!     GNU General Public License for more details.
!
!     You should have received a copy of the GNU General Public License
!     along with this program. If not, see <http://www.gnu.org/licenses/>.
!
!
!   Authors
!
!   42.10: Marcel Zijlema
!
!   Updates
!
!   42.10, June 2023: New subroutine
!
!   Purpose
!
!   Updates field array containing vertex entries of type integer through exchanging values between neighbouring subdomains
!
!   Method
!
!   Make use of MPI non-blocking recv/send commands and also administration (stored in ivsend/ivrecv)
!
!   Modules used
!
#ifdef USE_MPI
      use mpi
#endif
      use ocpcomm4
      use m_parall
      use SwanGriddata
!
      implicit none
!
!   Argument variables
!
      integer, dimension(nverts), intent(inout) :: ifld ! field array for which values in ghost vertices must be copied from neighbouring subdomains
!
!   Parameter variables
!
      integer, parameter :: itag = 2 ! message tag for sending and receiving
!
!   Local variables
!
      integer :: i ! loop counter
      integer :: idom ! subdomain number
      integer, save :: ient = 0 ! number of entries in this subroutine
      integer :: ierr ! error value of MPI call
      integer :: j ! loop counter
      integer :: ndata ! number of variables to send/receive
      character(120) :: msgstr ! string to pass message
!
!   Structure
!
!   Description of the pseudo code
!
!   Source text
!
      if (ltrace) call strace(ient, 'SwanUvExchgI')
      !
      if (.not. PARLL) return
#ifndef USE_MPI
      call msgerr(4, 'SwanUvExchgI requires an MPI-enabled SWAN build')
      return
#else
      !
      ! store data to be sent
      !
      do j = 1, nvcomm
         do i = 1, nvsend(j)
            isbuf(i, j) = ifld(ivsend(i, j))
         end do
      end do
      !
      ! for all neighbouring subdomains do
      !
      do j = 1, nvcomm
         !
         ! get subdomain number and number of ghost values to receive
         !
         idom = vsubcm(j)
         ndata = nvrecv(j)
         !
         ! post recvs
         !
         call MPI_IRECV ( irbuf(1,j), ndata, SWINT, idom-1, itag, SwanParallelComm(), rrqst(j), ierr )
         if ( ierr /= MPI_SUCCESS ) then
            write (msgstr, '(a,i3,a,i4)') 'MPI produces some internal error - return code is ',ierr,' and node number is ',INODE
            call msgerr ( 4, trim(msgstr) )
            return
         endif
         !
         ! get number of ghost values to send
         !
         ndata = nvsend(j)
         !
         ! post sends
         !
         call MPI_ISEND ( isbuf(1,j), ndata, SWINT, idom-1, itag, SwanParallelComm(), srqst(j), ierr )
         if ( ierr /= MPI_SUCCESS ) THEN
            write (msgstr, '(a,i3,a,i4)') 'MPI produces some internal error - return code is ',ierr,' and node number is ',INODE
            call msgerr ( 4, trim(msgstr) )
            return
         endif
         !
      end do
      !
      ! wait for receives to complete
      !
      call MPI_WAITALL ( nvcomm, rrqst, MPI_STATUSES_IGNORE, ierr )
      if ( ierr /= MPI_SUCCESS ) THEN
         write (msgstr, '(a,i3,a,i4)') 'MPI produces some internal error - return code is ',ierr,' and node number is ',INODE
         call msgerr ( 4, trim(msgstr) )
         return
      endif
      !
      ! store the received data
      !
      do j = 1, nvcomm
         do i = 1, nvrecv(j)
            ifld(ivrecv(i, j)) = irbuf(i, j)
         end do
      end do
      !
      ! wait for sends to complete
      !
      call MPI_WAITALL ( nvcomm, srqst, MPI_STATUSES_IGNORE, ierr)
      if ( ierr /= MPI_SUCCESS ) THEN
         write (msgstr, '(a,i3,a,i4)') 'MPI produces some internal error - return code is ',ierr,' and node number is ',INODE
         call msgerr ( 4, trim(msgstr) )
         return
      endif
#endif
      !
   end subroutine SwanUvExchgI
!
   subroutine SwanUvExchgR(fld)
!
!   --|-----------------------------------------------------------|--
!     | Delft University of Technology                            |
!     | Faculty of Civil Engineering and Geosciences              |
!     | Environmental Fluid Mechanics Section                     |
!     | P.O. Box 5048, 2600 GA  Delft, The Netherlands            |
!     |                                                           |
!     | Programmer: Marcel Zijlema                                |
!   --|-----------------------------------------------------------|--
!
!
!     SWAN (Simulating WAves Nearshore); a third generation wave model
!     Copyright (C) 1993-2024  Delft University of Technology
!
!     This program is free software: you can redistribute it and/or modify
!     it under the terms of the GNU General Public License as published by
!     the Free Software Foundation, either version 3 of the License, or
!     (at your option) any later version.
!
!     This program is distributed in the hope that it will be useful,
!     but WITHOUT ANY WARRANTY; without even the implied warranty of
!     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
!     GNU General Public License for more details.
!
!     You should have received a copy of the GNU General Public License
!     along with this program. If not, see <http://www.gnu.org/licenses/>.
!
!
!   Authors
!
!   42.10: Marcel Zijlema
!
!   Updates
!
!   42.10, June 2023: New subroutine
!
!   Purpose
!
!   Updates field array containing vertex entries of type real through exchanging values between neighbouring subdomains
!
!   Method
!
!   Make use of MPI non-blocking recv/send commands and also administration (stored in ivsend/ivrecv)
!
!   Modules used
!
#ifdef USE_MPI
      use mpi
#endif
      use ocpcomm4
      use m_parall
      use SwanGriddata
!
      implicit none
!
!   Argument variables
!
      real, dimension(nverts), intent(inout) :: fld ! field array for which values in ghost vertices must be copied from neighbouring subdomains
!
!   Parameter variables
!
      integer, parameter :: itag = 2 ! message tag for sending and receiving
!
!   Local variables
!
      integer :: i ! loop counter
      integer :: idom ! subdomain number
      integer, save :: ient = 0 ! number of entries in this subroutine
      integer :: ierr ! error value of MPI call
      integer :: j ! loop counter
      integer :: ndata ! number of variables to send/receive
      character(120) :: msgstr ! string to pass message
!
!   Structure
!
!   Description of the pseudo code
!
!   Source text
!
      if (ltrace) call strace(ient, 'SwanUvExchgR')
      !
      if (.not. PARLL) return
#ifndef USE_MPI
      call msgerr(4, 'SwanUvExchgR requires an MPI-enabled SWAN build')
      return
#else
      !
      ! store data to be sent
      !
      do j = 1, nvcomm
         do i = 1, nvsend(j)
            sbuf(i, j) = fld(ivsend(i, j))
         end do
      end do
      !
      ! for all neighbouring subdomains do
      !
      do j = 1, nvcomm
         !
         ! get subdomain number and number of ghost values to receive
         !
         idom = vsubcm(j)
         ndata = nvrecv(j)
         !
         ! post recvs
         !
         call MPI_IRECV ( rbuf(1,j), ndata, SWREAL, idom-1, itag, SwanParallelComm(), rrqst(j), ierr )
         if ( ierr /= MPI_SUCCESS ) then
            write (msgstr, '(a,i3,a,i4)') 'MPI produces some internal error - return code is ',ierr,' and node number is ',INODE
            call msgerr ( 4, trim(msgstr) )
            return
         endif
         !
         ! get number of ghost values to send
         !
         ndata = nvsend(j)
         !
         ! post sends
         !
         call MPI_ISEND ( sbuf(1,j), ndata, SWREAL, idom-1, itag, SwanParallelComm(), srqst(j), ierr )
         if ( ierr /= MPI_SUCCESS ) THEN
            write (msgstr, '(a,i3,a,i4)') 'MPI produces some internal error - return code is ',ierr,' and node number is ',INODE
            call msgerr ( 4, trim(msgstr) )
            return
         endif
         !
      end do
      !
      ! wait for receives to complete
      !
      call MPI_WAITALL ( nvcomm, rrqst, MPI_STATUSES_IGNORE, ierr )
      if ( ierr /= MPI_SUCCESS ) THEN
         write (msgstr, '(a,i3,a,i4)') 'MPI produces some internal error - return code is ',ierr,' and node number is ',INODE
         call msgerr ( 4, trim(msgstr) )
         return
      endif
      !
      ! store the received data
      !
      do j = 1, nvcomm
         do i = 1, nvrecv(j)
            fld(ivrecv(i, j)) = rbuf(i, j)
         end do
      end do
      !
      ! wait for sends to complete
      !
      call MPI_WAITALL ( nvcomm, srqst, MPI_STATUSES_IGNORE, ierr)
      if ( ierr /= MPI_SUCCESS ) THEN
         write (msgstr, '(a,i3,a,i4)') 'MPI produces some internal error - return code is ',ierr,' and node number is ',INODE
         call msgerr ( 4, trim(msgstr) )
         return
      endif
#endif
      !
   end subroutine SwanUvExchgR
!
   subroutine SwanCollBpntlist
!
!   --|-----------------------------------------------------------|--
!     | Delft University of Technology                            |
!     | Faculty of Civil Engineering and Geosciences              |
!     | Environmental Fluid Mechanics Section                     |
!     | P.O. Box 5048, 2600 GA  Delft, The Netherlands            |
!     |                                                           |
!     | Programmer: Marcel Zijlema                                |
!   --|-----------------------------------------------------------|--
!
!
!     SWAN (Simulating WAves Nearshore); a third generation wave model
!     Copyright (C) 1993-2024  Delft University of Technology
!
!     This program is free software: you can redistribute it and/or modify
!     it under the terms of the GNU General Public License as published by
!     the Free Software Foundation, either version 3 of the License, or
!     (at your option) any later version.
!
!     This program is distributed in the hope that it will be useful,
!     but WITHOUT ANY WARRANTY; without even the implied warranty of
!     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
!     GNU General Public License for more details.
!
!     You should have received a copy of the GNU General Public License
!     along with this program. If not, see <http://www.gnu.org/licenses/>.
!
!
!   Authors
!
!   43.01: Marcel Zijlema
!
!   Updates
!
!   43.01, August 2024: New subroutine
!
!   Purpose
!
!   Gathers the lists of boundary points in the ascending order from each processor to all the processes
!
!   Modules used
!
#ifdef USE_MPI
      use mpi
#endif
      use ocpcomm4
      use m_parall
      use SwanGriddata
      use SwanCompdata
!
      implicit none
!
!   Local variables
!
      integer, dimension(:), allocatable :: blistg ! list of unordered boundary vertices in global grid
      integer, dimension(:), allocatable :: bmarkg ! list of corresponding boundary markers in global grid
      integer :: i ! loop counter
      integer, dimension(:), allocatable :: iarr1 ! temporary array to store genuine global boundary indices
      integer, dimension(:), allocatable :: iarr2 ! temporary array to store corresponding boundary markers
      integer, dimension(:), allocatable :: iarr3 ! temporary array to store corresponding sequence number of boundary polygons
      integer, dimension(:), allocatable :: iarrg1 ! same as iarr1 but global
      integer, dimension(:), allocatable :: iarrg2 ! same as iarr2 but global
      integer, dimension(:), allocatable :: iarrg3 ! same as iarr3 but global
      integer, dimension(:), allocatable :: icount ! array specifying array size of data received from each processor
      integer, dimension(:), allocatable :: idsplc ! array specifying the starting address of the incoming data from
      ! each processor, relative to the global array
      integer, save :: ient = 0 ! number of entries in this subroutine
      integer                              :: ierr     ! error value of MPI call
      integer :: ii ! help index
      integer :: istat ! indicate status of allocation
      integer :: itmp ! temporary stored integer for swapping
      integer :: j ! loop counter
      integer, dimension(:), allocatable :: jlist ! list of ascending order of boundary indices
      integer :: k ! counter
      integer, dimension(1) :: kd ! location of minimum value in array ang
      integer :: maxnbp ! maximum number of boundary vertices in set of polygons
      integer :: nbp ! number of boundary vertices in present boundary polygon
      integer :: nbptot ! total number of boundary vertices
      integer :: nbpgl ! number of boundary vertices in global grid
      integer :: nbpolgl ! number of boundary polygons in global grid
      !
      real, dimension(:), allocatable :: ang ! angle of boundary points with respect to centroid
      real, dimension(:), allocatable :: dxc ! distance in x-direction with respect to centroid
      real, dimension(:), allocatable :: dyc ! distance in y-direction with respect to centroid
      real :: rtmp ! temporary stored real for swapping
      real :: sumx ! sum of all x-coordinates of boundary points
      real :: sumy ! sum of all y-coordinates of boundary points
      real :: xc ! x-coordinate of centroid
      real :: yc ! y-coordinate of centroid
      !
      character(120)                       :: msgstr   ! string to pass message
!
!   Structure
!
!   Description of the pseudo code
!
!   for each boundary polygon do
!       complete the list of genuine boundary points and scatter to all processes
!       re-order this list counterclockwise
!
      if (ltrace) call strace(ient, 'SwanCollBpntlist')
      !
      ! if not parallel, return
      !
      if (.not. PARLL) return
#ifndef USE_MPI
      call msgerr(4, 'SwanCollBpntlist requires an MPI-enabled SWAN build')
      return
#else
      !
      allocate (icount(0:NPROC - 1))
      allocate (idsplc(0:NPROC - 1))
      !
      maxnbp = size(blist) / nbpol
      !
      nbptot = maxnbp * nbpol
      !
      ! store the global indices, markers and polygon sequence number of genuine boundary points
      !
      allocate (iarr1(nbptot))
      allocate (iarr2(nbptot))
      allocate (iarr3(nbptot))
      !
      k = 0
      !
      do j = 1, nbpol
         !
         do i = 1, maxnbp
            !
            ii = blist(i, j)
            !
            if (ii > 0 .and. vmark(ii) < excmark) then
               k = k + 1
               iarr1(k) = ivertg(ii)
               iarr2(k) = vmark(ii)
               iarr3(k) = j
               !
            end if
            !
         end do
         !
      end do
      !
      deallocate (blist)
      !
      nbptot = k
      !
      ! calculate the global size as sum of the local sizes
      !
      call MPI_ALLREDUCE ( nbptot, nbpgl, 1, SWINT, SWSUM, SwanParallelComm(), ierr )
      if ( ierr /= MPI_SUCCESS ) then
         write (msgstr, '(a,i3,a,i4)') 'MPI produces some internal error - return code is ',ierr,' and node number is ',INODE
         call msgerr ( 4, trim(msgstr) )
         return
      endif
      !
      ! allocate temporary global data
      !
      istat = 0
      if (.not. allocated(iarrg1)) allocate (iarrg1(nbpgl), stat=istat)
      if (istat == 0 .and. .not. allocated(iarrg2)) allocate (iarrg2(nbpgl), stat=istat)
      if (istat == 0 .and. .not. allocated(iarrg3)) allocate (iarrg3(nbpgl), stat=istat)
      if (istat /= 0) then
         call msgerr(4, 'Allocation problem in SwanCollBpntlist: temporary global data ')
         return
      end if
      !
      ! gather the array sizes to all the processes
      !
      call MPI_ALLGATHER ( nbptot, 1, SWINT, icount, 1, SWINT, SwanParallelComm(), ierr )
      if ( ierr /= MPI_SUCCESS ) then
         write (msgstr, '(a,i3,a,i4)') 'MPI produces some internal error - return code is ',ierr,' and node number is ',INODE
         call msgerr ( 4, trim(msgstr) )
         return
      endif
      !
      ! calculate starting address of each local array with respect to the global array
      !
      idsplc(0) = 0
      do i = 1, NPROC - 1
         idsplc(i) = icount(i - 1) + idsplc(i - 1)
      end do
      !
      ! gather blist from each processor to all the processes
      !
      call MPI_ALLGATHERV ( iarr1(1:nbptot), nbptot, SWINT, iarrg1, icount, idsplc, SWINT, SwanParallelComm(), ierr )
      if ( ierr /= MPI_SUCCESS ) then
         write (msgstr, '(a,i3,a,i4)') 'MPI produces some internal error - return code is ',ierr,' and node number is ',INODE
         call msgerr ( 4, trim(msgstr) )
         return
      endif
      !
      ! gather corresponding markers from each processor to all the processes
      !
      call MPI_ALLGATHERV ( iarr2(1:nbptot), nbptot, SWINT, iarrg2, icount, idsplc, SWINT, SwanParallelComm(), ierr )
      if ( ierr /= MPI_SUCCESS ) then
         write (msgstr, '(a,i3,a,i4)') 'MPI produces some internal error - return code is ',ierr,' and node number is ',INODE
         call msgerr ( 4, trim(msgstr) )
         return
      endif
      !
      ! gather corresponding polygon sequence number from each processor to all the processes
      !
      call MPI_ALLGATHERV ( iarr3(1:nbptot), nbptot, SWINT, iarrg3, icount, idsplc, SWINT, SwanParallelComm(), ierr )
      if ( ierr /= MPI_SUCCESS ) then
         write (msgstr, '(a,i3,a,i4)') 'MPI produces some internal error - return code is ',ierr,' and node number is ',INODE
         call msgerr ( 4, trim(msgstr) )
         return
      endif
      !
      deallocate (iarr1, iarr2, iarr3, icount, idsplc)
      !
      ! determine maximum number of boundary polygons in global grid
      !
      nbpolgl = maxval(iarrg3)
      !
      ! determine maximum number of boundary vertices in set of polygons
      !
      maxnbp = -999
      !
      do j = 1, nbpolgl
         nbp = count(mask=iarrg3 == j)
         if (nbp > maxnbp) maxnbp = nbp
      end do
      !
      ! allocate and initialize blist and bmark
      !
      istat = 0
      if (.not. allocated(blist)) allocate (blist(maxnbp, nbpolgl), stat=istat)
      if (istat == 0 .and. .not. allocated(bmark)) allocate (bmark(maxnbp, nbpolgl), stat=istat)
      if (istat /= 0) then
         call msgerr(4, 'Allocation problem in SwanCollBpntlist: array blist and bmark ')
         return
      end if
      !
      blist = 0
      bmark = 0
      !
      ! allocate global data to store unordered boundary indices and corresponding markers
      !
      istat = 0
      if (.not. allocated(blistg)) allocate (blistg(nbpgl), stat=istat)
      if (istat == 0 .and. .not. allocated(bmarkg)) allocate (bmarkg(nbpgl), stat=istat)
      if (istat /= 0) then
         call msgerr(4, 'Allocation problem in SwanCollBpntlist: array blistg and bmarkg ')
         return
      end if
      !
      blistg = 0
      bmarkg = 0
      !
      ! complete the list of genuine boundary vertices in ascending order for all boundary polygons
      !
      do j = 1, nbpolgl
         !
         k = 0
         !
         do i = 1, nbpgl
            !
            if (iarrg3(i) == j) then
               !
               k = k + 1
               !
               blistg(k) = iarrg1(i)
               bmarkg(k) = iarrg2(i)
               !
            end if
            !
         end do
         !
         nbp = k
         !
         ! allocate data for computing the order of boundary points
         !
         istat = 0
         if (.not. allocated(ang)) allocate (ang(nbp), stat=istat)
         if (istat == 0 .and. .not. allocated(dxc)) allocate (dxc(nbp), stat=istat)
         if (istat == 0 .and. .not. allocated(dyc)) allocate (dyc(nbp), stat=istat)
         if (istat == 0 .and. .not. allocated(jlist)) allocate (jlist(nbp), stat=istat)
         if (istat /= 0) then
            call msgerr(4, 'Allocation problem in SwanCollBpntlist: array ang, dxc, dyc and jlist ')
            return
         end if
         !
         ! first determine centroid
         !
         sumx = 0.
         sumy = 0.
         do i = 1, nbp
            ii = blistg(i)
            sumx = sumx + xcugrdgl(ii)
            sumy = sumy + ycugrdgl(ii)
         end do
         !
         xc = sumx / real(nbp)
         yc = sumy / real(nbp)
         !
         do i = 1, nbp
            ii = blistg(i)
            dxc(i) = xcugrdgl(ii) - xc
            dyc(i) = ycugrdgl(ii) - yc
         end do
         !
         ! determine angle of each boundary point with respect to centroid
         !
         ang(1:nbp) = atan2(dyc(1:nbp), dxc(1:nbp))
         !
         ! sort angles counterclockwise
         !
         do i = 1, nbp
            jlist(i) = i
         end do
         !
         do i = 1, nbp - 1
            !
            kd = minloc(ang(i:nbp))
            k = kd(1) + i - 1
            !
            if (k /= i) then
               !
               rtmp = ang(i)
               ang(i) = ang(k)
               ang(k) = rtmp
               !
               itmp = jlist(i)
               jlist(i) = jlist(k)
               jlist(k) = itmp
               !
            end if
            !
         end do
         !
         ! sort blist and corresponding markers accordingly
         !
         do i = 1, nbp
            blist(i, j) = blistg(jlist(i))
            bmark(i, j) = bmarkg(jlist(i))
         end do
         !
         ! store number of boundary vertices for present polygon
         !
         nbpt(j) = nbp
         !
         deallocate (ang, dxc, dyc, jlist)
         !
      end do
      !
      deallocate (iarrg1, iarrg2, iarrg3, blistg, bmarkg)
#endif
      !
   end subroutine SwanCollBpntlist
!
   subroutine hpsort(n, ia)
!
!     SWAN (Simulating WAves Nearshore); a third generation wave model
!     Copyright (C) 1993-2024  Delft University of Technology
!
!     This program is free software: you can redistribute it and/or modify
!     it under the terms of the GNU General Public License as published by
!     the Free Software Foundation, either version 3 of the License, or
!     (at your option) any later version.
!
!     This program is distributed in the hope that it will be useful,
!     but WITHOUT ANY WARRANTY; without even the implied warranty of
!     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
!     GNU General Public License for more details.
!
!     You should have received a copy of the GNU General Public License
!     along with this program. If not, see <http://www.gnu.org/licenses/>.
!
!
!   Purpose
!
!   Sorts an integer array IA of length N in ascending order by the heapsort method
!
!   Method
!
!   The routine is copied from: Press et al., Numerical Recipes
!
!   Note that this method has O(N log2 N) complexity and thus
!   can be used for very large arrays
!
!   Modules used
!
      use ocpcomm4, only: ltrace
!
      implicit none
!
!   Argument variables
!
      integer(kind=kint), intent(in) :: n ! size of array
      integer(kind=kint), dimension(n), intent(inout) :: ia ! array to be sorted
!
!   Local variables
!
      integer, save :: ient = 0 ! number of entries in this subroutine
      integer(kind=kint) :: i ! counter
      integer(kind=kint) :: iia ! auxiliary value
      integer(kind=kint) :: ip ! position array
      integer(kind=kint) :: j ! counter
      integer(kind=kint) :: l ! counter
!
!   Source text
!
      if (ltrace) call strace(ient, 'hpsort')
      !
      l = n / 2 + 1
      ip = n
10    continue
      if (l > 1) then
         l = l - 1
         iia = ia(l)
      else
         iia = ia(ip)
         ia(ip) = ia(1)
         ip = ip - 1
         if (ip == 1) then
            ia(1) = iia
            return
         end if
      end if
      i = l
      j = l + l
20    if (j <= ip) then
         if (j < ip) then
            if (ia(j) < ia(j + 1)) j = j + 1
         end if
         if (iia < ia(j)) then
            ia(i) = ia(j)
            i = j
            j = j + j
         else
            j = ip + 1
         end if
         go to 20
      end if
      ia(i) = iia
      go to 10
      !
   end subroutine hpsort
!
end module SwanParallel
