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

!> coarsen the net
subroutine coarsen_mesh()
   use m_netw
   use unstruc_colors, only: ncolhl
   use stdlib_sorting, only: sort_index
   use m_sferic, only: dtol_pole
   use gridoperations
   use m_readlocator

   implicit none

   integer, parameter :: NMAX = 100 !< array size
   integer :: ndirect !< number of directly connected cells
   integer :: nindirect !< number of indirectly connected cells
   integer, dimension(NMAX) :: kdirect !< directly connected cells, i.e. cells sharing a link with cell k
   integer, dimension(NMAX) :: kindirect !< indirectly connected cells, i.e. cells sharing a node, but not a link, with cell k
   integer, dimension(2, NMAX) :: kne !< left and right neighboring (in)direct cell that neighbors the directly connected cells

   integer, dimension(:), allocatable :: kmask ! masking array

   integer :: k, k_, kk, kkk, k1, ja, N
!   integer                                     :: i, j, indx, isgn   ! for sort_heap
   integer, dimension(:), allocatable :: perm ! for sorting
!   integer                                     :: p1                 ! for sort_heap

   integer :: KEY ! for putget_un

   integer :: iter
   integer, parameter :: MAXITER = 1000
   integer :: numchanged ! number of cells deleted

   double precision, dimension(:), allocatable :: areas ! cell areas

   double precision :: area, area_tot, Darea
   double precision :: xc, yc, funct
   double precision :: x, y ! for putget_un

   double precision :: area_opt

   logical :: Lstepbystep, Ldoit

   Lstepbystep = .false.

!   call findcells(100)
!
!   call triangulate_cells()

   call findcells(100)

   call makenetnodescoding()

   if (nump < 1) return

   iter = 0
   numchanged = 1
   do while (iter < MAXITER .and. numchanged > 0)
      numchanged = 0
      allocate (kmask(nump))
      kmask = 1
      allocate (areas(nump), perm(nump))
      areas = 0

      !  compute cell areas
      do k = 1, nump
         call getcellsurface(k, areas(k), xc, yc)
      end do

      !  determine order: smallest cells first
      call sort_index(areas, perm)

      !  set optimal area as the average area
      area_opt = sum(areas) / dble(nump)

      k1 = 0
      do k_ = 1, nump
         k = perm(k_)

         if (kmask(k) == 1 .and. netcell(k)%N >= 3) then
            Ldoit = .true.
         else
            Ldoit = .false.
         end if

         if (Ldoit) then
            call find_surrounding_cells(k, NMAX, ndirect, nindirect, kdirect, kindirect, kne)

            do kk = 1, ndirect
               if (kmask(kdirect(kk)) /= 1 .or. netcell(kdirect(kk))%N < 3) then
                  Ldoit = .false.
                  exit
               end if
            end do
         end if

         if (Ldoit) then
            do kk = 1, nindirect
               if (kmask(kindirect(kk)) /= 1 .or. netcell(kindirect(kk))%N < 3) then
                  Ldoit = .false.
                  exit
               end if
            end do
         end if

         if (Ldoit) then
            area_tot = 0d0
            funct = 0d0

            call getcellsurface(k, area, xc, yc)
            area_tot = area_tot + area
            funct = funct - (area - area_opt)**2
            do kk = 1, ndirect
               call getcellsurface(kdirect(kk), area, xc, yc)
               area_tot = area_tot + area
               funct = funct - (area - area_opt)**2
            end do

            !     compute the area increase of the indirectly connected cells
            if (nindirect > 0) then
               Darea = area_tot / dble(nindirect)
            else
               Darea = 0d0
            end if

            !     compute the change in the functional
            do kk = 1, nindirect
               call getcellsurface(kindirect(kk), area, xc, yc)
               funct = funct - (area - area_opt)**2
               area = area + Darea
               funct = funct + (area - area_opt)**2
            end do

            !        funct = -1d0

            if (funct < 0d0) then ! delete cell
               !     if (k.eq.395 ) then
               if (Lstepbystep) then
                  !           unhighlight mesh
                  if (k1 >= 1 .and. k1 <= nump) call teknode(k1, 1)
                  !           whipe out previous net image
                  do kk = 1, netcell(k)%N
                     call teknode(netcell(k)%nod(kk), 211)
                  end do
               end if
               k1 = netcell(k)%nod(1) ! this node is kept

               !        delete the cell and update administration
               call deletecell(k, ndirect, nindirect, kdirect, kindirect, kne, .false., ja)
               if (ja == 1) numchanged = numchanged + 1

               if (Lstepbystep) then
                  !           new net image
                  if (netcell(k)%N == 0) then ! cell removed: draw remaining node and links connected to it
                     call teknode(k1, ncolhl)
                  else ! cell not removed: draw whole cell and links connected to it
                     do kk = 1, netcell(k)%N
                        call teknode(netcell(k)%nod(kk), 1)
                     end do
                  end if
               end if

               !     deactive cells
               kmask(k) = 0
               kmask(kdirect(1:ndirect)) = 0
               !            kmask(kindirect(1:nindirect)) = 0

               do kk = 1, ndirect
                  kkk = kdirect(kk)
                  N = netcell(kkk)%N
                  if (N > 0) then
                     xc = sum(xk(netcell(kkk)%nod(1:N))) / dble(N)
                     yc = sum(yk(netcell(kkk)%nod(1:N))) / dble(N)
                     call cirr(xc, yc, ncolhl)
                  end if
               end do

               !            do kk=1,nindirect
               !               kkk = kindirect(kk)
               !               N = netcell(kkk)%N
               !               if ( N.gt.0 ) then
               !                  xc = sum(xk(netcell(kkk)%nod(1:N))) / dble(N)
               !                  yc = sum(yk(netcell(kkk)%nod(1:N))) / dble(N)
               !                  call cirr(xc, yc, ncolhl)
               !               end if
               !            end do

            end if

            !        user interface
            if (Lstepbystep) then ! wait for key or mouse button press

               call READLOCATOR(X, Y, KEY)

               if (key == 21) then
                  Lstepbystep = .not. Lstepbystep
               else if (key == 22) then
                  exit
               end if

            else !
               call halt3(ja)

               if (ja == 1) then
                  Lstepbystep = .true.
               else if (ja == 3) then
                  exit
               end if

            end if
            !      else
            !         N  = netcell(k)%N
            !         if ( N.gt.0 ) then
            !            xc = sum(xk(netcell(k)%nod(1:N))) / dble(N)
            !            yc = sum(yk(netcell(k)%nod(1:N))) / dble(N)
            !            call cirr(xc, yc, ncolhl)
            !
            !            if ( Lstepbystep ) then
   !!               call READLOCATOR(X,Y,KEY)
            !            end if
            !
            !         end if

         end if
      end do

      call inflush()

      deallocate (kmask)
      deallocate (areas, perm)

      write (6, *) 'coarsen mesh: ', area_opt, iter, numchanged
   end do

   if (Lstepbystep) call READLOCATOR(X, Y, KEY)

   return
end subroutine coarsen_mesh
