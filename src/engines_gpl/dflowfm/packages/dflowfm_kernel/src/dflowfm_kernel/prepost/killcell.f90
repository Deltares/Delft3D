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

!> delete cell by merging all its nodes and update administration
subroutine killcell(xp, yp)
   use m_netw
   use m_missing, only: dmiss, jins
   use geometry_module, only: pinpok
   use gridoperations
   use m_qnerror

   implicit none

   double precision, intent(in) :: xp, yp !< coordinates of input point

   integer, parameter :: NMAX = 100 !< array size
   integer :: ndirect !< number of directly connected cells
   integer :: nindirect !< number of indirectly connected cells
   integer, dimension(NMAX) :: kdirect !< directly connected cells, i.e. cells sharing a link with cell k
   integer, dimension(NMAX) :: kindirect !< indirectly connected cells, i.e. cells sharing a node, but not a link, with cell k
   integer, dimension(2, NMAX) :: kne !< left and right neighboring (in)direct cell that neighbors the directly connected cells

   integer :: k, kk, k1
   integer :: in, ja

   integer, save :: NEEDFINDCELLS = 1

   if (nump < 1) NEEDFINDCELLS = 1

   if (NEEDFINDCELLS /= 0 .or. netstat /= NETSTAT_OK) then
      call findcells(100)
      call makenetnodescoding()
      NEEDFINDCELLS = 0
   end if

!  find the cell
   in = 0
   do k = 1, nump
      if (netcell(k)%N < 1) cycle
      call pinpok(xp, yp, netcell(k)%N, xk(netcell(k)%nod), yk(netcell(k)%nod), in, jins, dmiss)
      if (in > 0) exit
   end do

   if (in == 0) then ! no cell found
      call qnerror('killcell: no cell found', ' ', ' ')
      return
   end if

!   write(6,*) 'Cell ', k, 'N=', netcell(k)%N

!  get the connected cells
   call find_surrounding_cells(k, NMAX, ndirect, nindirect, kdirect, kindirect, kne)

!  whipe out previous net image
!   call teknet(0,ja)
   do kk = 1, netcell(k)%N
      call teknode(netcell(k)%nod(kk), 0)
   end do

!  delete cell and update administration
   k1 = netcell(k)%nod(1)
   call deletecell(k, ndirect, nindirect, kdirect, kindirect, kne, .true., ja)

!   call pfiller(xk(netcell(k)%nod), yk(netcell(k)%nod), netcell(k)%N, ncolhl, 30)

!   call teknet(ncolhl,ja)
   if (netcell(k)%N == 0) then ! cell removed: draw remaining node and links connected to it
      call teknode(k1, 1)
   else ! cell not removed: draw whole cell and links connected to it
      do kk = 1, netcell(k)%N
         call teknode(netcell(k)%nod(kk), 1)
      end do
   end if

   return
end subroutine killcell
