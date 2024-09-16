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

!> connect netboundary paths
recursive subroutine connect_boundary_paths(Lstart, nodemask, init, numnodes, nodelist)
   use m_netw
   use m_alloc
   use m_missing
   use m_landboundary
   use unstruc_colors, only: ncolhl
   use m_qnerror
   use m_set_col

   implicit none

   integer, intent(in) :: Lstart !< initial netlink
   integer, dimension(numk), intent(in) :: nodemask !< nodemask
   integer, intent(in) :: init !< initialize (1) or not (0)

   integer, intent(in) :: numnodes !< number of nodes found so far
   integer, dimension(numnodes), intent(in) :: nodelist !< nodes found so far

   integer :: numnodes_loc
   integer, allocatable, dimension(:) :: nodelist_loc ! local copy of nodes found so far

   integer :: maxnodes ! large enough, depends on DCLOSE

   integer :: i, j, k, kother, k1, k2, kk, L
   integer :: jstart, jend, numseg

   double precision :: xn, yn, ddis, rL ! for toland

!  note: in allocation of nodelist_loc, add space for new node
!        nodelist_loc will be allocated and deallocated here
   if (init == 1) then
      L = Lstart
!     find first node
      if (lnn(L) /= 1 .or. kn(1, L) < 1 .or. kn(2, l) < 1) return
      k1 = kn(1, L)
      k2 = kn(2, L)
      if (lanseg_map(k1) >= 1 .and. lanseg_map(k2) < 1 .and. nodemask(k1) > 0 .and. nodemask(k2) > 0) then
!        allocate
         allocate (nodelist_loc(3))
         nodelist_loc(1:2) = (/k1, k2/)
         numnodes_loc = 2
      else if (lanseg_map(k1) < 1 .and. lanseg_map(k2) >= 1 .and. nodemask(k1) > 0 .and. nodemask(k2) > 0) then
!        allocate
         allocate (nodelist_loc(3))
         nodelist_loc(1:2) = (/k2, k1/)
         numnodes_loc = 2
      else ! not a valid link
         return
      end if
   else
!     allocate
      allocate (nodelist_loc(numnodes + 1))

      numnodes_loc = numnodes
      nodelist_loc(1:numnodes_loc) = nodelist
   end if

!  get the array size
   maxnodes = ubound(nodelist_loc, 1)

!  get last node visited
   k = nodelist_loc(numnodes_loc)

!  loop over all connected links and check if a valid path exists
   kklp: do kk = 1, nmk(k)
      L = nod(k)%lin(kk)
      if (lnn(L) /= 1) cycle kklp ! boundary links only

      kother = kn(1, L) + kn(2, L) - k

!     check if next node is already in nodelist
      do i = numnodes_loc, 1, -1
         if (kother == nodelist_loc(i)) cycle kklp
      end do

      if (nodemask(kother) < 1) cycle kklp ! path stopped

      if (numnodes_loc >= MAXNODES) then
         call qnerror('connect_boundary_paths: numnodes > MAXNODES', ' ', ' ')
         goto 1234
      end if

!     add new node
      nodelist_loc(numnodes_loc + 1) = kother

!     check and see if the next node completes a connection
      if (lanseg_map(kother) > 0) then
!        connection found!
!        make the node-to-land boundary segment mapping
         do i = 2, numnodes_loc
!            write(6,"(I5,$)") nodelist_loc(i)
            k1 = nodelist_loc(i)

!           find the land boundary segment
            call toland(xk(k1), yk(k1), 1, MXLAN, 0, xn, yn, ddis, j, rL)
            numseg = 1
            do while ((j < lanseg_startend(1, numseg) .or. j >= lanseg_startend(2, numseg)) .and. numseg < Nlanseg)
               numseg = numseg + 1
            end do

            jstart = lanseg_startend(1, numseg)
            jend = lanseg_startend(2, numseg)

            if (j < jstart .or. j > jend) then
!              land boundary segment not found, this should not happen
!               call qnerror('connect_boundary_paths: land boundary segment not found', ' ', ' ')
               goto 1234
            end if

            if ((j == jstart .and. rL < 0d0) .or. (j == jend - 1 .and. rL > 1d0)) then
!              prevent projection to end points of land boundary segments:
               if (Ladd_land) then
!                 add new land boundary segment that connects the two others and project
                  call add_land()
                  lanseg_map(k1) = numseg
               end if
            else
               lanseg_map(k1) = numseg
            end if
         end do

!         write(6,*)

!        plot the boundary path
         k1 = nodelist_loc(1)
         call setcol(ncolhl)
         call movabs(xk(k1), yk(k1))
         do i = 2, numnodes_loc + 1
            k1 = nodelist_loc(i)
            call lnabs(xk(k1), yk(k1))
         end do

!        done, clean up
         goto 1234
      else
!        no connection found -> continue path
         call connect_boundary_paths(L, nodemask, 0, numnodes_loc + 1, nodelist_loc)
      end if

   end do kklp

!  done, clean up
   goto 1234

!  error handling
1234 continue
   deallocate (nodelist_loc)
   return

contains

!>  add new land boundary segment that connects two others
   subroutine add_land()

      use geometry_module, only: dbdistance
      use m_missing, only: dmiss
      use m_sferic, only: jsferic, jasfer3D

      implicit none

      integer :: numseg1, numseg2
      double precision :: xL1, yL1, xL2, yL2

!     find segments numbers
      numseg1 = lanseg_map(nodelist_loc(1))
      numseg2 = lanseg_map(nodelist_loc(numnodes_loc + 1))

      if (numseg1 < 1 .or. numseg1 > Nlanseg .or. numseg2 < 1 .or. numseg2 > Nlanseg) then
!        this should never happen
         return
      end if

!     find start/end
      jstart = lanseg_startend(1, numseg1)
      jend = lanseg_startend(2, numseg1)
      if (dbdistance(xk(k), yk(k), xlan(jstart), ylan(jstart), jsferic, jasfer3D, dmiss) <= dbdistance(xk(k), yk(k), xlan(jend), ylan(jend), jsferic, jasfer3D, dmiss)) then
         xL1 = xlan(jstart)
         yL1 = ylan(jstart)
      else
         xL1 = xlan(jend)
         yL1 = ylan(jend)
      end if

      if (numseg2 /= numseg1) then
         jstart = lanseg_startend(1, numseg2)
         jend = lanseg_startend(2, numseg2)
         if (dbdistance(xk(k), yk(k), xlan(jstart), ylan(jstart), jsferic, jasfer3D, dmiss) <= dbdistance(xk(k), yk(k), xlan(jend), ylan(jend), jsferic, jasfer3D, dmiss)) then
            xL2 = xlan(jstart)
            yL2 = ylan(jstart)
         else
            xL2 = xlan(jend)
            yL2 = ylan(jend)
         end if
      else
         xL2 = xlan(jstart) + xlan(jend) - xL1
         yL2 = ylan(jstart) + ylan(jend) - yL1
      end if

!     add to landboundary
      if (xlan(MXLAN) /= DMISS) then
         MXLAN = MXLAN + 1
         if (MXLAN > ubound(xlan, 1)) call increaselan(MXLAN + 2)
         xlan(MXLAN) = dmiss
         ylan(MXLAN) = dmiss
      end if
      MXLAN = MXLAN + 2

      if (MXLAN > ubound(xlan, 1)) call increaselan(MXLAN)

      xlan(MXLAN - 1) = xL1
      ylan(MXLAN - 1) = yL1
      xlan(MXLAN) = xL2
      ylan(MXLAN) = yL2

!     update administration
      Nlanseg = Nlanseg + 1

      if (Nlanseg > ubound(lanseg_startend, 2)) then
         call realloc(lanseg_startend, (/2, Nlanseg/))
      end if

      lanseg_startend(:, Nlanseg) = (/MXLAN - 1, MXLAN/)
      numseg = Nlanseg
      return
   end subroutine add_land

end subroutine connect_boundary_paths
