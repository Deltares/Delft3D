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

     subroutine gridtonet()
        use m_netw
        use m_grid
        use m_missing
        use gridoperations
        use m_mergenodes
        implicit none
        double precision :: af

        integer, allocatable :: mn(:, :)
        integer :: k0, l0, ja, jadoorladen, i, j, k, l, ierr

        jadoorladen = 1

        if (JADOORLADEN == 0) then
           K0 = 0
           L0 = 0
        else
           K0 = NUMK
           L0 = NUML
        end if

        K = 0
        do I = 1, MC ! COUNT NR OF NODES
           do J = 1, NC
              if (Xc(I, J) /= dXYMIS) then
                 K = K + 1
              end if
           end do
        end do

        !IF (K0+K .GT. SIZE(XK) ) THEN
        call INCREASENETW(K0 + K, L0 + 4 * K)
        !ENDIF

        K = K0
        L = L0 ! COUNT MAX NR OF ATTACHED LINKS PER NODE

        call READYY('Arranging curvilinear grid-in network', 0d0)

        if (allocated(mn)) deallocate (mn)
        allocate (mn(mc, nc), stat=ierr); mn = 0
        call aerr('mn(mc,nc)', ierr, mc * nc)

        do I = 1, MC
           do J = 1, NC
              if (xc(i, j) /= dxymis) then
                 call addnetpointnocheck(xc(i, j), yc(i, j), zc(i, j), k)
                 mn(i, j) = k
              end if
           end do
        end do
        numk = k

        af = 0.2d0
        call READYY('Arranging curvilinear grid-in network', af)

        do I = 1, MC - 1
           do J = 1, NC
              if (mn(i, j) /= 0 .and. mn(i + 1, j) /= 0) then
                 L = L + 1
                 kn(1, L) = mn(i, j)
                 kn(2, L) = mn(i + 1, j)
                 KN(3, L) = 2
              end if
           end do
        end do

        af = 0.4d0
        call READYY('Arranging curvilinear grid-in network', af)

        do I = 1, MC
           do J = 1, NC - 1
              if (mn(i, j) /= 0 .and. mn(i, j + 1) /= 0) then
                 L = L + 1
                 kn(1, L) = mn(i, j)
                 kn(2, L) = mn(i, j + 1)
                 KN(3, L) = 2
              end if
           end do
        end do

        af = 0.6d0
        call READYY('Arranging curvilinear grid-in network', af)

        numl = l
        call setnodadm(0)

        call READYY('Arranging curvilinear grid-in network', -1d0)

        ! call copydeptosam()

        if (k0 > 0) then

           JA = 1

           call readyy('Merging networks', 0d0)
           call findcells(0)

!     merge nodes

           if (tooclose > 1d-16 .and. k0 > 0) then
              call CONFRM('MERGE NODES ? ', JA)
              if (JA == 1) call MERGENODESINPOLYGON()
           end if

!     merge boundary nodes
!      call mergenet()

           call readyy('Merging networks', -1d0)

        end if

!     set network status
        netstat = NETSTAT_CELLS_DIRTY

     end subroutine gridtonet
