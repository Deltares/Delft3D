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

!> derefine mesh
subroutine derefine_mesh(xp, yp, Lconfirm)

   use m_confrm
   use m_netw
   use geometry_module, only: pinpok
   use m_missing, only: jins, dmiss
   use gridoperations
   use m_qnerror
   use m_makenetnodescoding
   use m_cirr

   implicit none

   double precision, intent(in) :: xp, yp !< coordinates of input point ( not used with Lconfirm .eq. .true. )
   logical, intent(in) :: Lconfirm !< prompt for cell deletion (.true.) or not (.false.)

   integer, parameter :: NMAX = 100 !< array size
   integer :: ndirect !< number of directly connected cells
   integer :: nindirect !< number of indirectly connected cells
   integer, dimension(NMAX) :: kdirect !< directly connected cells, i.e. cells sharing a link with cell k
   integer, dimension(NMAX) :: kindirect !< indirectly connected cells, i.e. cells sharing a node, but not a link, with cell k
   integer, dimension(2, NMAX) :: kne !< left and right neighboring (in)direct cell that neighbors the directly connected cells

   double precision :: xx, yy

   integer :: numfront, numfrontnew
   integer, dimension(:), allocatable :: ifront, ifrontnew, icellmask

   integer :: i, in, j, ja, k, k1, kk, L
   integer :: kother, newsize, iter, N

   logical :: Liscell, Lplot

   integer, parameter :: MAXNUMFRONT = 1000, MAXITER = 1000

   call findcells(100)
   call makenetnodescoding()

   if (Lconfirm) then
      Lplot = .true.
!     find the cell
      in = -1
      do k = 1, nump
         if (netcell(k)%N < 1) cycle
         call pinpok(xp, yp, netcell(k)%N, xk(netcell(k)%nod), yk(netcell(k)%nod), in, jins, dmiss)
         if (in > 0) exit
      end do

      if (in == 0) then ! no cell found
         call qnerror('derefine_mesh: no cell found', ' ', ' ')
         return
      end if
   else
      Lplot = .false.
!     give preference to a cell near a boundary
      in = -1
      k = 0 ! default
      do L = 1, numL
         if (lnn(L) /= 1) cycle
!        check if this link is a true boundary link, or a link near the selecting polygon
         if (nb(kn(1, L)) /= 2 .or. nb(kn(2, L)) /= 2) cycle
!        get the adjacent cell
         k1 = lne(1, L)
!        check if the adjacent cell is a quad
         if (netcell(k1)%N /= 4) cycle
!        check if all nodes are inside the selecting polygon
         Liscell = .true.
         do i = 1, netcell(k1)%N
            if (nb(netcell(k1)%nod(i)) == 0) then
               Liscell = .false.
               exit
            end if
         end do
         if (.not. Liscell) cycle

!        link found: get the adjacent cell and exit
         k = k1
         exit
      end do

      if (k < 1) then ! no cell found: take the first quad inside the selecting polygon
         do k1 = 1, nump
!           check if the cell is a quad
            if (netcell(k1)%N /= 4) cycle
!           check if all nodes are inside the selecting polygon
            Liscell = .true.
            do i = 1, netcell(k1)%N
               if (nb(netcell(k1)%nod(i)) == 0) then
                  Liscell = .false.
                  exit
               end if
            end do
            if (.not. Liscell) cycle

!           cell found: get the cell and exit
            k = k1
            exit
         end do
      end if

!     still no cell found: take the first
      if (k < 1) k = 1
   end if

!  allocate
   allocate (ifront(MAXNUMFRONT), ifrontnew(MAXNUMFRONT), icellmask(nump))

   icellmask = 0

!  make the cellmask
!    1 : front, 'A' cell (used to be node, delete it)
!    2 : front, 'B' cell (used to be link, keep it)
!    3 : 'C' cell (used to be cell, keep it)
!   -1 : not in front, 'A' cell
!   -2 : not in front, 'B' cell
!    0 : unassigned

!  fill the frontlist with the selected cell
   numfront = 1
   ifront(1) = k
   icellmask(k) = 1

   iter = 0

   do while (numfront > 0 .and. iter < MAXITER)
      iter = iter + 1
      numfrontnew = 0

      do i = 1, numfront
         k = ifront(i)

!        get the connected cells
         call find_surrounding_cells(k, NMAX, ndirect, nindirect, kdirect, kindirect, kne)

         if (icellmask(k) == 1) then ! 'A' cell
            do j = 1, ndirect
               kother = kdirect(j)
               if (netcell(kother)%N /= 4) cycle ! quads only
               if (abs(icellmask(kother)) /= 1 .and. abs(icellmask(kother)) /= 2) then
                  icellmask(kother) = 2
                  call update_frontlist(kother)
               end if
            end do
            do j = 1, nindirect
               kother = kindirect(j)
               if (netcell(kother)%N /= 4) cycle ! quads only
               if (icellmask(kother) /= 3) then
                  icellmask(kother) = 3
               end if
            end do
            icellmask(k) = -1
         else if (icellmask(k) == 2) then ! 'B' cell
            do j = 1, ndirect
               kother = kdirect(j)
               if (netcell(kother)%N /= 4) cycle ! quads only
               if (icellmask(kother) /= 3 .and. abs(icellmask(kother)) /= 1 .and. abs(icellmask(kother)) /= 2) then
                  icellmask(kother) = 1
                  call update_frontlist(kother)
               end if
            end do
            do j = 1, nindirect
               kother = kindirect(j)
               if (netcell(kother)%N /= 4) cycle ! quads only
               if (abs(icellmask(kother)) /= 2 .and. abs(icellmask(kother)) /= 1 .and. icellmask(kother) /= 3) then
                  icellmask(kother) = 2
                  call update_frontlist(kother)
               end if
            end do
            icellmask(k) = -2
         end if

      end do ! do i=1,numfront

      numfront = numfrontnew
      ifront = ifrontnew

   end do ! do while ( numfront.gt.0 )

   if (Lconfirm .or. .not. Lconfirm) then
!     plot
      do k = 1, nump
         N = netcell(k)%N
         if (N < 1) cycle
         xx = sum(xk(netcell(k)%nod(1:N))) / dble(N)
         yy = sum(yk(netcell(k)%nod(1:N))) / dble(N)
         if (abs(icellmask(k)) == 1) then
            call cirr(xx, yy, 31)
         else if (abs(icellmask(k)) == 2) then
!           call cirr(xx, yy, 211)
         else if (icellmask(k) == 3) then
!           call cirr(xx, yy, 204)
         else if (icellmask(k) == 0) then
!           call cirr(xx, yy, 0)
         else
            continue
         end if
      end do
      call confrm('Delete cells?', ja)
   else
      ja = 1
   end if

   if (ja == 1) then
!     delete the appropriate cells
      do k = 1, nump
         if (icellmask(k) == -1 .and. netcell(k)%N > 0) then
            call find_surrounding_cells(k, NMAX, ndirect, nindirect, kdirect, kindirect, kne)

            if (Lplot) then
!              plot
               do kk = 1, netcell(k)%N
                  call teknode(netcell(k)%nod(kk), 0)
               end do
            end if

            k1 = netcell(k)%nod(1)
!           delete cell and update administration
            call deletecell(k, ndirect, nindirect, kdirect, kindirect, kne, .false., ja)

            if (Lplot) then
               if (netcell(k)%N == 0) then ! cell removed: draw remaining node and links connected to it
                  call teknode(k1, 1)
               else ! cell not removed: draw whole cell and links connected to it
                  do kk = 1, netcell(k)%N
                     call teknode(netcell(k)%nod(kk), 1)
                  end do
               end if
            end if

         end if
      end do
   end if

!  deallocate
   deallocate (ifront, ifrontnew, icellmask)

!  set network status
   netstat = NETSTAT_CELLS_DIRTY

   return

contains

   subroutine update_frontlist(knew)
      implicit none

      integer, intent(in) :: knew !< number of cell to be added to frontlist

      integer :: i

!     add to new front list
      if (knew > 0) then

!        check number of nodes
         if (netcell(knew)%N /= 4) return ! quads only

!        check if cell is already in frontlist
         do i = 1, numfront
            if (ifrontnew(i) == knew) return
         end do
         numfrontnew = numfrontnew + 1

!        realloc if necessary
         if (numfrontnew > ubound(ifrontnew, 1)) then
            newsize = ceiling(1.2 * ubound(ifrontnew, 1))
            call realloc(ifrontnew, newsize)
            call realloc(ifront, newsize)
         end if

!        store
         ifrontnew(numfrontnew) = knew
      end if
   end subroutine update_frontlist

end subroutine derefine_mesh
