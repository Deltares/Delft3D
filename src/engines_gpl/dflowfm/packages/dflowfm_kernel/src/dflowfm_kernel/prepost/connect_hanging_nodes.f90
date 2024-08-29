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

!> make inner links in a cell with hanging nodes
subroutine connect_hanging_nodes(linkbrother)
   use m_netw
   use m_qnerror

   implicit none

   integer, dimension(numL), intent(in) :: linkbrother !< brotherlink, that shares a (hanging) node, dim: numL

   integer :: ic, kk, kkm1, kkm2, kkp1, kkp2
   integer :: L, Lm1, Lp1, Lnew, N
   integer :: num

   integer, parameter :: MMAX = 6 ! maximum number of link per netcell
   integer, dimension(MMAX) :: knode ! counterclockwise end-node of link, excluding the hanging nodes
   integer, dimension(MMAX) :: khang ! hanging nodes per unrefined link

   integer :: ierror

   ierror = 0

   do ic = 1, nump
!     find the counterclockwise end-nodes, excluding the hanging nodes, and the hanging nodes
      knode = 0
      khang = 0
      N = netcell(ic)%N
      if (N > MMAX) then
!         call qnerror('connect_hanging_nodes: unsupported cell', ' ', ' ')
!         goto 1234
         ierror = 1
         cycle
      end if

      num = 0
      do kk = 1, N
         kkm1 = kk - 1; if (kkm1 < 1) kkm1 = kkm1 + N
         kkp1 = kk + 1; if (kkp1 > N) kkp1 = kkp1 - N
         L = netcell(ic)%lin(kk)
         Lm1 = netcell(ic)%lin(kkm1)
         Lp1 = netcell(ic)%lin(kkp1)
         if (linkbrother(L) /= Lp1) then
            num = num + 1
            if (num > MMAX) goto 1234

!           store ccw end node of this link
            call find_common_node(L, Lp1, knode(num))

!           check of the counterclockwise start-node of this link is a hanging node
            if (linkbrother(L) == Lm1) then
               call find_common_node(L, linkbrother(L), khang(num))
            end if
         end if
      end do ! do kk=1,N

      if (num == N) cycle ! no hanging nodes

      if (num == 4) then ! quads
         if (N - num == 1) then ! quad with one hanging node
            do kk = 1, num
               if (khang(kk) /= 0) then
                  kkm1 = kk - 1; if (kkm1 < 1) kkm1 = kkm1 + num
                  kkm2 = kk - 2; if (kkm2 < 1) kkm2 = kkm2 + num
                  kkp1 = kk + 1; if (kkp1 > num) kkp1 = kkp1 - num
                  call newlink(knode(kkm2), khang(kk), Lnew)
                  call newlink(knode(kkp1), khang(kk), Lnew)
                  exit ! done with this cell
               end if ! if ( khang(kk).ne.0 ) then
            end do
         else if (N - num == 2) then ! quad with two hanging nodes
            do kk = 1, num
               if (khang(kk) /= 0) then
                  kkm1 = kk - 1; if (kkm1 < 1) kkm1 = kkm1 + num
                  kkp1 = kk + 1; if (kkp1 > num) kkp1 = kkp1 - num
                  kkp2 = kk + 2; if (kkp2 > num) kkp2 = kkp2 - num
!                 check if the two hanging nodes are neighbors
                  if (khang(kkm1) /= 0) then ! left neighbor
                     call newlink(khang(kkm1), khang(kk), Lnew)
                     call newlink(khang(kk), knode(kkp1), Lnew)
                     call newlink(knode(kkp1), khang(kkm1), Lnew)
                  else if (khang(kkp1) /= 0) then ! right neighbor
                     call newlink(khang(kk), khang(kkp1), Lnew)
                     call newlink(khang(kkp1), knode(kkp2), Lnew)
                     call newlink(knode(kkp2), khang(kk), Lnew)
                  else if (khang(kkp2) /= 0) then ! hanging nodes must be oposing
                     call newlink(khang(kk), khang(kkp2), Lnew)
                  end if
                  exit ! done with this cell
               end if ! if ( khang(kk).ne.0 ) then
            end do
         else if (N - num == 3) then ! quad with three hanging nodes
!           N.eq.7 can never happen if MMAX = 6
         end if
      else if (num == 3) then ! triangles
         if (N - num == 1) then ! triangle with one hanging node
            do kk = 1, num
               if (khang(kk) /= 0) then
                  kkp1 = kk + 1; if (kkp1 > num) kkp1 = kkp1 - num
                  call newlink(khang(kk), knode(kkp1), Lnew)
                  exit ! done with this cell
               end if ! if ( khang(kk).ne.0 ) then
            end do
         else if (N - num == 2) then ! triangle with two hanging nodes
!           split_cell should prevent this
            do kk = 1, num
               if (khang(kk) /= 0) then
                  kkm1 = kk - 1; if (kkm1 < 1) kkm1 = kkm1 + num
                  kkp1 = kk + 1; if (kkp1 > num) kkp1 = kkp1 - num
                  if (khang(kkm1) /= 0) then
                     call newlink(khang(kk), khang(kkm1), Lnew)
                  else
                     call newlink(khang(kk), khang(kkp1), Lnew)
                  end if
!                  call teklink(Lnew, 22)
                  call qnerror('connect_hanging_nodes: triangle with two hanging nodes', ' ', ' ')
                  exit ! done with this cell
               end if ! if ( khang(kk).ne.0 ) then
            end do
         end if
      else
!         call qnerror('connect_hanging_nodes: unsupported cell', ' ', ' ')
!         goto 1234
         ierror = 1
      end if
   end do ! do ic=1,nump

   if (ierror == 1) then
      call qnerror('connect_hanging_nodes: unsupported cell(s)', ' ', ' ')
   end if

!  error handling
1234 continue

   return
end subroutine connect_hanging_nodes
