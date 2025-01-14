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

!> assign node-based indices (ic,jc) in the net
module m_assign_icjc
   use m_grow_ijc, only: grow_ijc

   implicit none

   private

   public :: assign_icjc

contains

   subroutine assign_icjc(xp, yp, ic, jc, iexit)
      use precision, only: dp
      use m_assignijgrid, only: assignijgrid
      use m_netw
      use m_grid
      use m_alloc
      use m_missing
      use m_polygon, only: NPL, xpl, ypl, zpl
      use geometry_module, only: pinpok, dbpinpol, get_startend
      use m_find_common_node

      real(kind=dp) :: xp, yp !< coordinates of starting point

      integer, dimension(numk) :: ic, jc !< node indices (i,j)
      integer :: iexit !< 1 on success, 0 otherwise

      integer :: k, kk, in
      integer :: L1, L2, L3, L4

      real(kind=dp) :: xh(4), yh(4)

      !integer, parameter                 :: IMISS = -999999

      integer :: knode, ik, lowold(2), uppold(2)

      logical :: linpoly

      iexit = 0

!---------------------------------------------------------
! allocate and initialize indices arrays
!---------------------------------------------------------
      ic = IMISS
      jc = IMISS
      in = 0

! allocate and initialize cellmask array
      call realloc(cellmask, numP)

! allocate and initialize ijc array
      if (allocated(ijc)) deallocate (ijc)
      call realloc(ijc, (/3, 3/), (/0, 0/), fill=IMISS)

      if (nump < 1) return

!---------------------------------------------------------
! find first cell k from input (xh, yh)
!---------------------------------------------------------
      do k = 1, nump
         if (netcell(k)%n == 4) then
            do kk = 1, 4
               xh(kk) = xk(netcell(k)%nod(kk))
               yh(kk) = yk(netcell(k)%nod(kk))
            end do
            call pinpok(xp, yp, 4, xh, yh, in, jins, dmiss)
            if (in == 1) then
               in = k
               if (netcell(k)%n /= 4) in = 0
               exit
            end if
         end if
      end do

      if (in == 0) return
!---------------------------------------------------------
! initialize cellmask
!---------------------------------------------------------
      cellmask = 1; ! init active
! remove netcells that have one or more nodes outside the polygon
      ik = -1
      do k = 1, nump
         linpoly = .true.
         do kk = 1, netcell(k)%n
            knode = netcell(k)%nod(kk)
            call dbpinpol(xk(knode), yk(knode), ik, dmiss, JINS, NPL, xpl, ypl, zpl)
            if (ik == 0) then
               linpoly = .false.
               exit
            end if
         end do
         if (.not. linpoly) cellmask(k) = 0
      end do

!---------------------------------------------------------
! start with first cell
!---------------------------------------------------------
      k = in
      L1 = netcell(k)%lin(1)
      L2 = netcell(k)%lin(2)
      L3 = netcell(k)%lin(3)
      L4 = netcell(k)%lin(4)

! assign (i,j)=(1,1) to common node of links 1 and 1
      call find_common_node(L1, L2, kk)
      ic(kk) = 1
      jc(kk) = 1
      ijc(1, 1) = kk
! assign (i,j)=(2,1) to common node of links 2 and 3
      call find_common_node(L2, L3, kk)
      ic(kk) = 2
      jc(kk) = 1
      ijc(2, 1) = kk
! assign (i,j)=(2,2) to common node of links 3 and 4
      call find_common_node(L3, L4, kk)
      ic(kk) = 2
      jc(kk) = 2
      ijc(2, 2) = kk
! assign (i,j)=(1,2) to common node of links 4 and 1
      call find_common_node(L4, L1, kk)
      ic(kk) = 1
      jc(kk) = 2
      ijc(1, 2) = kk

      lowold = lbound(ijc)
      uppold = ubound(ijc)

      call grow_ijc(lowold, uppold, lbound(ijc) - 1, ubound(ijc) + 1, 1) ! check, compileert bij mij niet
!---------------------------------------------------------
! proceed with remaining cells
!---------------------------------------------------------
      call assignijgrid(k, ic, jc)

      iexit = 1

   end subroutine assign_icjc

end module m_assign_icjc
