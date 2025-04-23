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

!> find meshline nearest to land boundary
module m_find_nearest_meshline
   use m_toland, only: toland
   use m_linkcrossedbyland, only: linkcrossedbyland

   implicit none

   private

   public :: find_nearest_meshline

contains

   subroutine find_nearest_meshline(jasnap)
      use precision, only: dp
      use m_connect_boundary_paths, only: connect_boundary_paths
      use m_admin_landboundary_segments, only: admin_landboundary_segments
      use m_clnabs
      use m_netw
      use m_landboundary
      use m_missing
      use m_alloc
      use m_missing

      integer :: jasnap !< same as japroject

      integer :: netboundonly ! consider only the net boundary (1) or not (0)

      integer, dimension(:), allocatable :: nodemask, linkmask ! note that cellmask is in module
      integer, dimension(:), allocatable :: klink ! link connected to the node in the shortest path
      real(kind=dp), dimension(:), allocatable :: dismin ! minimum distance to whole land boundary

      integer :: numnodes ! in connect_boundary_paths: number of nodes found so far
      integer, dimension(:), allocatable :: nodelist ! in connect_boundary_paths: nodes found so far

      integer :: MXLAN_sav
      integer :: k, L, ja

      integer :: numseg ! land boundary segment number
      integer :: jstart, jend, jstart1
      integer :: kstart, kend ! start and end node resp.

      integer :: kk, knode
      integer :: j, j_prev, jstart_prev, jend_prev, numseg_prev
      integer :: num, numrejected, numcellsmasked, maskdepth
      integer :: ierr

      real(kind=dp) :: xp, yp
      real(kind=dp) :: xn, yn, ddis, rL, ddismin ! in toland
      real(kind=dp) :: xn_prev, yn_prev, ddis_prev, rL_prev

      real(kind=dp), parameter :: DISNEAREST = 2d0

      logical, parameter :: LMASK = .true.

      logical, parameter :: LDEBUG = .false.

!  save MXLAN
      MXLAN_sav = MXLAN

!  set netboundonly
      if (jasnap == 2 .or. jasnap == 3) then
         netboundonly = 1
      else
         netboundonly = 0
      end if

!  set the close-to-landboundary tolerance, measured in meshwidths
      if (netboundonly /= 1) then
         DCLOSE = DCLOSE_whole
      else
         DCLOSE = DCLOSE_bound
      end if

      call admin_landboundary_segments()

!  allocate arrays
      if (allocated(cellmask)) then
         deallocate (cellmask)
      end if
      allocate (nodemask(numk), linkmask(numL), cellmask(nump), klink(numk), stat=ierr)

      if (allocated(lanseg_map)) then
         deallocate (lanseg_map)
      end if
      allocate (lanseg_map(numk))
      lanseg_map = 0

      allocate (dismin(numk))

      allocate (nodelist(1))

!   nodemask = IMISS  ! not necessary
!   linkmask = IMISS  ! not necessary, appears in masknodes
!   cellmask = IMISS  ! not necessary, appears in masknodes
      dismin = DMISS ! necessary

!  loop over the segments of the land boundary
!     the segments are from jstart to jend
      do numseg = 1, Nlanseg
         call make_path(numseg, num, numrejected)
         if (netboundonly == 1 .and. jasnap == 3 .and. &
             !           numrejected.gt.num .and. numrejected.gt.2 ) then ! num.lt.2 gives problems
             numrejected > 0) then
!        land boundary is an internal land boundary -> snap to it
            netboundonly = 0
            DCLOSE = DCLOSE_whole
            call make_path(numseg, num, numrejected)

!        restore setting
            netboundonly = 1
            DCLOSE = DCLOSE_bound
         end if
      end do

!  make the connection between boundary paths
      if (netboundonly == 1) then
         do L = 1, numL
            if (lnn(L) == 1) then
               if (.not. allocated(nodelist)) then ! safety
                  allocate (nodelist(1))
                  numnodes = 1
               end if
               call connect_boundary_paths(L, nodemask, 1, numnodes, nodelist)
            end if
         end do
      end if

!  error handling, memory deallocation and return
1234  continue

!  deallocate
      deallocate (nodemask, linkmask, cellmask, klink, dismin, nodelist)

!  set actual MXLAN
      MXLAN_loc = MXLAN
!  restore MXLAN
      MXLAN = MXLAN_sav

      return

   contains

!> make path for land boundary segment
      subroutine make_path(numseg, num, numrejected)
         use unstruc_colors, only: ncolhl
         use m_qnerror
         use m_tek_link
         use m_cirr
         use m_movabs

         implicit none

         integer, intent(in) :: numseg !< land boundary segment number
         integer, intent(out) :: num !< number of nodes in path
         integer, intent(out) :: numrejected !< number of rejected nodes

         integer :: numpart ! number of connected nodes

         integer :: klast, numseglast ! remember last added node

         logical :: Lstopped ! the path has been temporarily stopped (.true.) or not (.false.)

         !  initialize
         num = 0
         numpart = 0
         numrejected = 0

         !  find jstart and jend of landboundary segment
         jstart = lanseg_startend(1, numseg)
         jend = lanseg_startend(2, numseg)

         !  set the outer land boundary points
         jleft = jend - 1
         jright = jstart
         rLleft = 1d0
         rLright = 0d0

         if (jstart < 1 .or. jstart > MXLAN .or. jstart > jend) return

         call masknodes(numseg) ! will set jleft, jright, rLleft and rLright

!      write(6,*) numseg, jstart, jend, jleft, jright, rLleft, rLright

         if (LDEBUG) then
            do k = 1, numk
               if (nodemask(k) == numseg) then
                  call cirr(xk(k), yk(k), 212)
               end if
            end do
         end if

!     find start- and endnode
!      call get_kstartend(jstart,jend,kstart,kend)   ! will use jleft, jright, rLleft and rLright
         call get_kstartend2(jend, kstart, kend) ! will use jleft, jright, rLleft and rLright
         if (kstart < 1 .or. kend < 1) goto 1234 ! no start and/or end node found

         if (kstart == kend) goto 1234 ! no path can be found

         call cirr(xk(kstart), yk(kstart), 191)
         call cirr(xk(kend), yk(kend), 191)

         if (Ldebug) then
            write (6, *) numseg, kstart, kend
         end if

!     shortest path
         call shortest_path(numseg, jstart, jend, kstart, nodemask, netboundonly, klink)

!     construct path from end to start node
         k = kend
         numseglast = lanseg_map(k)
         klast = 0
         do
!         klast      = k
!         numseglast = lanseg_map(k)
            Lstopped = .true.

!        fill the node-to-boundary-segment-map array
            if (lanseg_map(k) > 0) then ! netboundsonly check not necessary
!           multiple boundary segments: take the nearest
               numseg_prev = lanseg_map(k)
               jstart_prev = lanseg_startend(1, numseg_prev)
               jend_prev = lanseg_startend(2, numseg_prev)
               call toland(xk(k), yk(k), jstart_prev, jend_prev, 0, xn_prev, yn_prev, ddis_prev, j_prev, rL_prev)
               call toland(xk(k), yk(k), jstart, jend, 0, xn, yn, ddis, j, rL)
               ddismin = dismin(k)
               if (ddis <= ddis_prev .and. ddis <= DISNEAREST * ddismin) then
                  Lstopped = .false.
!               num = num+1
!               numpart = numpart+1
!               lanseg_map(k) = numseg
               end if
            else
!           check if land boundary segment is not redicuously further than other parts of the land boundary
               if (dismin(k) == DMISS) then
                  call toland(xk(k), yk(k), 1, MXLAN, 0, xn, yn, ddismin, j, rL)
                  dismin(k) = ddismin
               else
                  ddismin = dismin(k)
               end if

               call toland(xk(k), yk(k), jstart, jend, 0, xn, yn, ddis, j, rL)
               if (ddis <= DISNEAREST * ddismin) then

!              check for netboundonly
                  if (netboundonly /= 1 .or. nb(k) == 2 .or. nb(k) == 3) then
                     Lstopped = .false.
!                  num = num+1
!                  numpart = numpart+1
!                  lanseg_map(k) = numseg
                  end if
               end if
            end if

            if (Lstopped) then
!           the path has been (temporarily) stopped
               if (numpart == 1 .and. numseglast /= 0) then ! prevent one-node paths
                  lanseg_map(klast) = numseglast
               end if

               numpart = 0
               numrejected = numrejected + 1
            else

               klast = k
               numseglast = lanseg_map(k)

               num = num + 1
               numpart = numpart + 1
               lanseg_map(k) = numseg
            end if

!        prevent one-node-paths
!         if ( Lstopped .and. numpart.eq.1 .and. klast.gt.0 ) then
!        25-05-12: lanseg_map may be reset to 0 for one-node-paths
            if (Lstopped .and. numpart == 1) then
               lanseg_map(klast) = numseglast
            end if

!        exit if the start node is reached
            if (k == kstart) goto 1234

!        proceed to the next node
            L = klink(k)
            if (L < 1 .or. L > numL) exit
            if (netboundonly == 0 .or. lnn(L) == 1) call teklink(L, ncolhl)
            k = kn(1, L) + kn(2, L) - k
            if (k < 1 .or. k > numk) exit
         end do

!     plot landboundary segment
         do j = jstart, jend - 1
            call movabs(xlan(j), ylan(j))
            call clnabs(xlan(j + 1), ylan(j + 1), 204)
         end do

1234     continue

!     prevent one-node-paths
         if (numpart == 1) then
            lanseg_map(klast) = numseglast
         end if

         if (LDEBUG) then
!         if ( num.gt.0 ) call qnerror(' ', ' ', ' ')
            call qnerror(' ', ' ', ' ')

            do k = 1, numk
               call cirr(xk(k), yk(k), 0)
            end do
            !     whipe out landboundary segment
            do j = jstart, jend - 1
               call movabs(xlan(j), ylan(j))
               call clnabs(xlan(j + 1), ylan(j + 1), 0)
            end do
            ja = 0
            !call teknet(1,ja)
         end if

         return
      end subroutine make_path

!> mask the nodes that are considered in the shortest path algorithm
      subroutine masknodes(numseg)
         use precision, only: dp

         use m_cellcrossedbyland, only: cellcrossedbyland
         use m_missing
         use m_polygon, only: NPL, xpl, ypl, zpl
         use geometry_module, only: dbpinpol
         use geometry_module, only: pinpok
         use m_qnerror

         implicit none

         integer, intent(in) :: numseg !< land boundary segment number

         integer, parameter :: M = 10 ! maximum number of nodes per netcell
         integer, dimension(M) :: nlist
         real(kind=dp), dimension(M) :: xlist, ylist

         integer, allocatable, dimension(:) :: listnext ! next-cell list in maskcells
         integer :: numnext ! size of next-cell list in maskcells

         integer :: N

         integer :: in, j

!     clear nodemask
         nodemask = IMISS

!     find the start cell for node masking
         kstart = 0
         in = -1
         jstart1 = jstart ! first node of land boundary segment in mesh
         do while (in <= 0 .and. jstart1 < jend)
            xp = xlan(jstart1)
            yp = ylan(jstart1)
            !     find the startcell
            kstart = 0
            do while (in <= 0 .and. kstart < nump)
               kstart = kstart + 1

               N = netcell(kstart)%N

               if (N < 1) cycle

               if (N > M) then
                  call qnerror('masknodes: N>M', ' ', ' ')
               end if
               nlist(1:N) = netcell(kstart)%nod
               xlist(1:N) = xk(nlist(1:N))
               ylist(1:N) = yk(nlist(1:N))
               call pinpok(xp, yp, N, xlist, ylist, in, jins, dmiss)
            end do

            if (in == 0) jstart1 = jstart1 + 1
         end do

!     no startcell found that contains a land boundary point:
!        try to find a boundary cell that is crossed by the land boundary segment
!        by checking the net boundaries (boundary links)
         if (in <= 0) then
            kstart = 0
            j = jstart
            do L = 1, numL
               if (lnn(L) /= 1) cycle
!           get the boundary cell number
               k = lne(1, L)
!           check the cell
               call cellcrossedbyland(k, jstart, jend, in)
               if (in == 1) then
!              crossed: startcell found
                  kstart = k
                  exit
               end if
            end do
         end if

         if (LMASK) then ! startcell found -> masking possible
!        mask cells
            cellmask = IMISS
            linkmask = IMISS
            if (kstart > 0 .and. kstart <= nump) then
               cellmask(kstart) = 1 ! sub maskcells assumes startcell has already been done.
            end if
            numcellsmasked = 0
            maskdepth = 0

!        cell masking
            numnext = 1
            allocate (listnext(numnext))
            listnext(1) = kstart
            call maskcells(listnext, numnext) ! will deallocate listnext
            if (allocated(listnext)) then ! safety, should not happen
               deallocate (listnext)
            end if

!        mask nodes
            do k = 1, nump
               N = netcell(k)%N
               if (cellmask(k) == 1) then
                  do kk = 1, N
                     knode = netcell(k)%nod(kk)
                     nodemask(knode) = numseg
                  end do
               end if
            end do
         else ! startcell not found
            do k = 1, numk
               nodemask(k) = numseg
            end do
         end if

!     take selecting polygon into account
         in = -1
         do k = 1, numk
            if (nodemask(k) > 0) then
               call dbpinpol(xk(k), yk(k), in, dmiss, JINS, NPL, xpl, ypl, zpl)
               if (in /= 1) nodemask(k) = 0
            end if
         end do

      end subroutine masknodes

!> mask the cells that are intersected by the land boundary
      recursive subroutine maskcells(listcur, numcur)
         use m_alloc

         implicit none

         integer, allocatable, dimension(:), intent(inout) :: listcur !< current-node list, will be deallocated here
         integer, intent(inout) :: numcur !< number of cells in current-cell list

         integer :: numnext ! number of cells in next-cell list
         integer, allocatable, dimension(:) :: listnext ! next-node list
         integer :: jacross, jacell
         integer :: ic, k1, k2, kk
         integer :: kcell, kothercell

         integer :: j

         integer :: i, L, LL, N, in, NN
         integer :: jalinkcrossed(6)

!      if ( numcellsmasked.gt.2000) then
!         continue
!      end if

         numnext = 0

!     allocate next-cell list, set size initially to current-cell list size
         allocate (listnext(numcur))

!     process the current-cell list
         do ic = 1, numcur
            kcell = listcur(ic)

            jalinkcrossed = 0

!        write(6,'(I, $)') kcell
!         write(6,*) kcell, numcellsmasked, maskdepth

!        no startcell specified (kcell.eq.0): mask boundary cells only
!           these are boundary cells that are crossed ( up to a certain tolerance ) by a land boundary
            if (kcell == 0) then
               j = 1
               do L = 1, numL
                  if (lnn(L) /= 1) cycle
                  kothercell = lne(1, L)
                  k1 = kn(1, L)
                  k2 = kn(2, L)
                  if (k1 < 1 .or. k2 < 1) cycle
                  !            cellmask(kothercell) = 1

                  !           proximity check
                  !            call cellcrossedbyland(kothercell, jstart, jend, j, jacross)

                  jacross = 0
                  do kk = 1, netcell(kothercell)%N
                     LL = netcell(kothercell)%lin(kk)
                     call linkcrossedbyland(LL, jstart, jend, j, jacross)
                     if (jacross == 1) exit
                  end do

                  if (jacross == 1) then
                     cellmask(kothercell) = 1
                  end if
               end do
            else

!            j = max(min(jland,jend-1), jstart)
               j = jstart

               N = netcell(kcell)%N
               if (N < 3) cycle ! not a valid cell
               do i = 1, N ! Loop over all links (i.e. towards neighbouring cells)
                  L = netcell(kcell)%lin(i)
                  jacross = 0
                  jacell = 0

                  ! If boundary cell: no further checking in that direction.
                  if (LNN(L) <= 1) then
                     cycle
                  end if

                  ! There is a neighbour cell: compute its cellmask
                  kothercell = lne(1, L) + lne(2, L) - kcell

                  ! If neighbour cell already visited, continue to next neighbour.
                  if (cellmask(kothercell) /= IMISS) then
                     cycle
                     ! Important: jalinkcrossed(i) stays 0 now,
                     ! so that no recursion for this kothercell is done.
                  end if

                  NN = netcell(kothercell)%N
                  do in = 1, NN
                     LL = netcell(kothercell)%lin(in)

                     if (linkmask(LL) == 1) then
                        !           previously visited crossed link
                        jacross = 1
                        jacell = 1
                     else if (linkmask(LL) == 0) then
                        !           previously visited uncrossed link - check next (kothercell) link
                        cycle
                     else ! linkmask is IMISS, i.e. the link is unvisited
                        !           unvisited links
                        linkmask(LL) = 0

                        call linkcrossedbyland(LL, jstart, jend, j, jacross)
                        if (jacross == 1) then
                           linkmask(LL) = 1
                           jacell = 1
!                        exit ! in
                        end if
                     end if
                  end do

                  ! jacell is now either 0 or 1, directly defines cellmask for kothercell.
                  cellmask(kothercell) = jacell
                  if (jacell == 1) then
                     numcellsmasked = numcellsmasked + 1

!                 add the neighboring cell to the next-cell list, but only when it is unvisited
                     kothercell = lne(1, L) + lne(2, L) - kcell
                     numnext = numnext + 1

!                 check array size and increase if necessary
                     if (numnext > ubound(listnext, 1)) then
                        call realloc(listnext, max(int(1.2 * numnext), 10))
                     end if

!                 fill next-cell list
                     listnext(numnext) = kothercell
                  end if
               end do

            end if

         end do ! ic=1,numcur

!     deallocate the current-cell list
         deallocate (listcur)
         numcur = 0

!     process the next-cell list
         if (numnext > 0) then
            maskdepth = maskdepth + 1
            call maskcells(listnext, numnext) ! will deallocate listnext
            maskdepth = maskdepth - 1
         else
            deallocate (listnext)
         end if

!     deallocate, safety, should not happen
         if (allocated(listnext)) then
            deallocate (listnext)
         end if

         return
      end subroutine maskcells

!> Dijkstra's shortest path algorithm
      subroutine shortest_path(numseg, jstart, jend, kstart, nodemask, netboundonly, klink)
         use precision, only: dp

         use network_data
         use geometry_module, only: dbdistance, dlinedis
         use m_missing
         use m_sferic, only: jsferic, jasfer3D

         implicit none

         integer, intent(in) :: numseg !< land boundary segment number
         integer, intent(in) :: jstart, jend !< land boundary segment start and end point
         integer, intent(in) :: kstart !< start node
         integer, dimension(numk), intent(inout) :: nodemask !< 1 for active nodes, 0 otherwise
         integer, intent(in) :: netboundonly !< consider only the net boundary (1) or not (0)
         integer, dimension(numk), intent(out) :: klink !< link connected to the node in the shortest path

         real(kind=dp), allocatable, dimension(:) :: dist

         integer :: i, j, kcur, kneighbor, L
         integer :: j1, j2, j3
         integer :: ja

         real(kind=dp) :: x1, y1, x2, y2, x3, y3
         real(kind=dp) :: xn1, yn1, xn2, yn2, xn3, yn3 ! projection on land boundary
         real(kind=dp) :: ddis1, ddis2, ddis3
         real(kind=dp) :: rL1, rL2, rL3
         real(kind=dp) :: dl1, dl2, dL, ddmax
         real(kind=dp) :: dlinklength, dist_alt

         real(kind=dp), parameter :: DMAX = 1d99
         real(kind=dp), parameter :: fsixth = 1d0 / 6d0
         integer, parameter :: alpha = 1

!      integer, parameter                                         :: IMISS = -999

         allocate (dist(numk))

!     nodemask < 1 deactivates nodes

         dL = 0d0

         dist = DMAX
         klink = 0

         kcur = kstart
         dist(kcur) = 0d0
         do
            nodemask(kcur) = -nodemask(kcur)
            x1 = xk(kcur)
            y1 = yk(kcur)

            call toland(x1, y1, jstart, jend, 0, xn1, yn1, ddis1, j1, rL1)

            if (j1 < 1) then
               continue
            end if

            do i = 1, nmk(kcur)
               L = nod(kcur)%lin(i)
               if (kn(1, L) < 1 .or. kn(2, L) < 1) cycle

               kneighbor = kn(1, L) + kn(2, L) - kcur

               if (nodemask(kneighbor) < 1) cycle

               x2 = xk(kneighbor)
               y2 = yk(kneighbor)
               x3 = 0.5d0 * (x1 + x2)
               y3 = 0.5d0 * (y1 + y2)

               dlinklength = dbdistance(x1, y1, x2, y2, jsferic, jasfer3D, dmiss)

               call toland(x2, y2, jstart, jend, 0, xn2, yn2, ddis2, j2, rL2)
               call toland(x3, y3, jstart, jend, 0, xn3, yn3, ddis3, j3, rL3)

               dl1 = dbdistance(xn1, yn1, xn3, yn3, jsferic, jasfer3D, dmiss)
               dl2 = dbdistance(xn2, yn2, xn3, yn3, jsferic, jasfer3D, dmiss)

!           determine maximum distance to the landboundary for a link
               ddmax = max(ddis1, ddis2) ! maximum distance to land boundary between n1 and n2
               if (j1 < j2) then
                  do j = j1 + 1, j2
                     call dlinedis(xlan(j), ylan(j), x1, y1, x2, y2, ja, ddis3, xn3, yn3, jsferic, jasfer3D, dmiss)
                     if (ddis3 > ddmax) ddmax = ddis3
                  end do
                  dL = dL + dbdistance(xlan(j2), ylan(j2), xn2, yn2, jsferic, jasfer3D, dmiss)
               else if (j1 > j2) then
                  do j = j1, j2 + 1, -1
                     call dlinedis(xlan(j), ylan(j), x1, y1, x2, y2, ja, ddis3, xn3, yn3, jsferic, jasfer3D, dmiss)
                     if (ddis3 > ddmax) ddmax = ddis3
                  end do
               end if

               if (dL < dlinklength) then
                  continue
               end if

!           in case of netboundaries only: set penalty on weights when the link is not a boundary link
               if (netboundonly == 1 .and. lnn(L) /= 1) ddmax = 1d6 * ddmax

               dist_alt = dist(kcur) + dlinklength * ddmax

               if (dist_alt < dist(kneighbor)) then
                  dist(kneighbor) = dist_alt
                  klink(kneighbor) = L
               end if
            end do

            kcur = minloc(dist, MASK=nodemask == numseg, DIM=1)
            if (kcur < 1 .or. kcur > numk .or. dist(kcur) == DMAX .or. nodemask(kcur) < 1) exit

         end do

!     reset nodemask
         where (nodemask < 0 .and. nodemask /= IMISS) nodemask = -nodemask

         deallocate (dist)

      end subroutine shortest_path

!>  find start and end node for a land boundary segment
!>    these are the nodes that are:
!>       closest to the start and end node of the boundary segment respectively, and
!>       within a certain distance from the land boundary segment
!>  note: will use jleft, jright, rLleft and rLright
      subroutine get_kstartend(jstart, jend, kstart, kend)
         use precision, only: dp

         use m_missing, only: dmiss, JINS
         use m_polygon, only: NPL, xpl, ypl, zpl
         use geometry_module, only: dbpinpol, dbdistance
         use m_sferic, only: jsferic, jasfer3D

         implicit none

         integer, intent(in) :: jstart, jend !< land boundary segment start and end point
         integer, intent(out) :: kstart, kend !< start and end node

         integer :: k, instart, inend

         integer :: ja ! for toland

         integer :: kend_prev, disendmin_prev, dislandend_prev

         real(kind=dp) :: xstart, ystart, xend, yend ! coordinates of begin and end point of land boundary segment respectively
         real(kind=dp) :: x3, y3
         real(kind=dp) :: xn, yn, rl ! for toland

         real(kind=dp) :: dis, disstart, disend
         real(kind=dp) :: disstartmin, disendmin, dislandstart, dislandend
         real(kind=dp) :: dismax

!     default values
         kstart = 0
         kend = 0

         xstart = xlan(jleft) + rLleft * (xlan(min(jleft + 1, jend)) - xlan(jleft))
         ystart = ylan(jleft) + rLleft * (ylan(min(jleft + 1, jend)) - ylan(jleft))

         xend = xlan(jright) + rLright * (xlan(min(jright + 1, jend)) - xlan(jright))
         yend = ylan(jright) + rLright * (ylan(min(jright + 1, jend)) - ylan(jright))

         instart = -1
         inend = 0
         call dbpinpol(xstart, ystart, instart, dmiss, JINS, NPL, xpl, ypl, zpl)
         call dbpinpol(xend, yend, inend, dmiss, JINS, NPL, xpl, ypl, zpl)

         if (instart /= 1) then
            xstart = xlan(jleft + 1)
            ystart = ylan(jleft + 1)
         end if

         if (inend /= 1) then
            xend = xlan(jright)
            yend = ylan(jright)
         end if

         disstartmin = 1d99
         disendmin = 1d99

         disendmin_prev = 0d0
         dislandend_prev = 0d0

         kend = 0 ! for kend_prev
         kend_prev = 0

         do k = 1, numk
            if (k == 47065) then
               continue
            end if

            if (nodemask(k) < 1) cycle

            x3 = xk(k)
            y3 = yk(k)

!        compute minimum distance to the land boundary segment
            call toland(x3, y3, jstart, jend, 0, xn, yn, dis, ja, rl)

!        consider only nodes that are within a certain range from the land boundary segment

            dismax = dmeshwidth(k)

            if (dismax /= DMISS .and. dis < DCLOSE * dismax) then

!            write(6,*) dis, dmeshwidth(k), dbdistance(x3,y3,xstart,ystart), disstartmin

!           check distance to start of land boundary segment
               disstart = dbdistance(x3, y3, xstart, ystart, jsferic, jasfer3D, dmiss)
               if (disstart < disstartmin) then
                  kstart = k
                  disstartmin = disstart
                  dislandstart = dis
               end if

!           check distance to end of land boundary segment
               disend = dbdistance(x3, y3, xend, yend, jsferic, jasfer3D, dmiss)
               if (disend < disendmin) then
                  kend_prev = kend
                  kend = k
                  disendmin = disend
                  dislandend = dis
               end if
            end if
         end do

         if (kend == kstart .and. kend_prev > 0) then
            kend = kend_prev
            disendmin = disendmin_prev
            dislandend = dislandend_prev
         end if

      end subroutine get_kstartend

!>  find start and end nodes for a land boundary segment
!>    these are nodes that are
!>       on a link that is closest to the start and end node of the boundary segment respectively
!>  note: will use jleft, jright, rLleft and rLright
      subroutine get_kstartend2(jend, kstart, kend)
         use precision, only: dp

         use m_missing, only: dmiss, JINS
         use m_polygon, only: NPL, xpl, ypl, zpl
         use geometry_module, only: dbpinpol, dbdistance
         use m_sferic, only: jsferic, jasfer3D
         use m_d_line_dis3

         implicit none

         integer, intent(in) :: jend !< land boundary segment end point
         integer, intent(out) :: kstart, kend !< start and end node

         integer :: ja ! for toland

         integer :: instart, inend, ierror

         integer :: L, Lstart, Lend
         integer :: ka, kb, kd, ke

         real(kind=dp) :: xstart, ystart, xend, yend ! coordinates of begin and end point of land boundary segment respectively
         real(kind=dp) :: xa, ya, xb, yb
         real(kind=dp) :: xn, yn, r ! for toland

         real(kind=dp) :: disstart, disend
         real(kind=dp) :: disstartmin, disendmin

         ierror = 1

!     default values
         kstart = 0
         kend = 0

         Lstart = 0
         Lend = 0

!     compute the start and end point of the land boundary respectively
         xstart = xlan(jleft) + rLleft * (xlan(min(jleft + 1, jend)) - xlan(jleft))
         ystart = ylan(jleft) + rLleft * (ylan(min(jleft + 1, jend)) - ylan(jleft))

         xend = xlan(jright) + rLright * (xlan(min(jright + 1, jend)) - xlan(jright))
         yend = ylan(jright) + rLright * (ylan(min(jright + 1, jend)) - ylan(jright))

         instart = -1
         inend = 0
         call dbpinpol(xstart, ystart, instart, dmiss, JINS, NPL, xpl, ypl, zpl)
         call dbpinpol(xend, yend, inend, dmiss, JINS, NPL, xpl, ypl, zpl)

         if (instart /= 1) then
            xstart = xlan(jleft + 1)
            ystart = ylan(jleft + 1)
         end if

         if (inend /= 1) then
            xend = xlan(jright)
            yend = ylan(jright)
         end if

         disstartmin = 1d99
         disendmin = 1d99

!     get the links that are closest the land boundary start and end respectively
         do L = 1, numL
            ka = kn(1, L)
            kb = kn(2, L)

            if (ka < 1 .or. kb < 1) cycle ! safety

            if (nodemask(ka) < 1 .or. nodemask(kb) < 1) cycle

!        compute distance from link to land boundary start- and end-point
            xa = xk(ka)
            ya = yk(ka)
            xb = xk(kb)
            yb = yk(kb)
            call dlinedis3(xstart, ystart, xa, ya, xb, yb, ja, disstart, xn, yn, r)
            call dlinedis3(xend, yend, xa, ya, xb, yb, ja, disend, xn, yn, r)

            if (disstart < disstartmin) then
               Lstart = L
               disstartmin = disstart
            end if

            if (disend < disendmin) then
               Lend = L
               disendmin = disend
            end if
         end do

!     check if start and end link are found
         if (Lstart == 0) then
!         call qnerror('get_kstartend2: Lstart=0', ' ', ' ')
            goto 1234
         end if
         if (Lend == 0) then
!         call qnerror('get_kstartend2: Lend=0', ' ', ' ')
            goto 1234
         end if

!     find start and end node on the start and end link respectively
         ka = kn(1, Lstart)
         kb = kn(2, Lstart)
         if (dbdistance(xk(ka), yk(ka), xstart, ystart, jsferic, jasfer3D, dmiss) <= dbdistance(xk(kb), yk(kb), xstart, ystart, jsferic, jasfer3D, dmiss)) then
            kstart = ka
         else
            kstart = kb
         end if

         kd = kn(1, Lend)
         ke = kn(2, Lend)
         if (dbdistance(xk(kd), yk(kd), xend, yend, jsferic, jasfer3D, dmiss) <= dbdistance(xk(ke), yk(ke), xend, yend, jsferic, jasfer3D, dmiss)) then
            kend = kd
         else
            kend = ke
         end if

         ierror = 0
1234     continue

         return
      end subroutine get_kstartend2

!> compute typical mesh width for a node, which is the maximum length of the connected links
      real(kind=dp) function dmeshwidth(k)
         use precision, only: dp

         use m_missing
         use m_polygon, only: NPL, xpl, ypl, zpl
         use geometry_module, only: dbpinpol, dbdistance
         use m_sferic, only: jsferic, jasfer3D

         implicit none

         integer, intent(in) :: k !< node number

         integer :: kother, kk, L, in

         real(kind=dp) :: x1, y1, x2, y2

!     not-in-polygon value
         dmeshwidth = DMISS

         x1 = xk(k)
         y1 = yk(k)

!     check if node itself is in selecting polygon
         in = -1
         call dbpinpol(x1, y1, in, dmiss, JINS, NPL, xpl, ypl, zpl)
         if (in /= 1) return

         dmeshwidth = 0d0
         do kk = 1, nmk(k)
            L = nod(k)%lin(kk)
            kother = kn(1, L) + kn(2, L) - k

            x2 = xk(kother)
            y2 = yk(kother)

!        check if the connected node is in selecting polygon
            call dbpinpol(x2, y2, in, dmiss, JINS, NPL, xpl, ypl, zpl)
            if (in /= 1) cycle

            dmeshwidth = max(dmeshwidth, dbdistance(x1, y1, x2, y2, jsferic, jasfer3D, dmiss))
         end do
      end function dmeshwidth

   end subroutine find_nearest_meshline

end module m_find_nearest_meshline
