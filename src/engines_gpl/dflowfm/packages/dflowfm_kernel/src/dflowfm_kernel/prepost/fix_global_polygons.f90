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

module m_fix_global_polygons

   implicit none

   private

   public :: fix_global_polygons

contains

   subroutine fix_global_polygons(jaalwayscopyleftright, japartpols)
      use precision, only: dp

      use m_sferic
      use m_polygon
      use m_missing
      use m_partitioninfo
      use geometry_module, only: get_startend

      use network_data, only: numk, nump, xk, xzw, yzw
      use messagehandling, only: LEVEL_INFO, mess
      use m_delpol

      implicit none

      integer, intent(in) :: jaalwayscopyleftright !< always copy polygons to left and right (1) or not (0)
      integer, intent(in) :: japartpols !< partitioning polygons (1) or not (0)

      integer :: i, j, k

      real(kind=dp) :: x1, x2
      real(kind=dp) :: dist, dist1, dist2
      real(kind=dp) :: xmin, xmax

      integer :: jpoint, jstart, jend
      integer :: i1, i2, num, NPLnew, NPLnewest
      integer :: im1, ip1, isign
      integer :: jaleft, jaright, japole
      integer :: numshifted ! number of shifted polygon nodes
      integer :: in, idmn

!     check for spherical coordinates
      if (jsferic /= 1 .or. NPL <= 2) return

      call mess(LEVEL_INFO, 'fixing global polygon... ')

!     fix the polygon nodes on the poles
      jpoint = 1 ! first polygon node
      jstart = 1
      jend = 0
      NPLnew = NPL
      do while (jpoint <= NPL)
         call get_startend(NPL - jpoint + 1, xpl(jpoint:NPL), ypl(jpoint:NPL), jstart, jend, dmiss)
         jstart = jstart + jpoint - 1
         jend = jend + jpoint - 1
         jpoint = max(jend + 2, jpoint + 1) ! min: make sure we advance the pointer

         i = jstart - 1
         do while (i < jend)
            i = i + 1
            if (abs(abs(ypl(i)) - 90d0) < dtol_pole) then
!              add node
               call increasepol(NPL + 1, 1)
               do j = NPL, i, -1
                  xpl(j + 1) = xpl(j)
                  ypl(j + 1) = ypl(j)
                  zpl(j + 1) = zpl(j)
               end do
               NPL = NPL + 1
               jend = jend + 1
               jpoint = jpoint + 1
               i = i + 1

               im1 = i - 2; if (im1 < jstart) im1 = im1 + jend - jstart + 1
               ip1 = i + 1; if (ip1 > jend) ip1 = ip1 - (jend - jstart + 1)

!              shift current node above previous node
               xpl(i - 1) = xpl(im1)

!              place new node above next node
               xpl(i) = xpl(ip1)
               ypl(i) = ypl(i)
               zpl(i) = zpl(i)
            end if
         end do
      end do

      numshifted = 0

      jpoint = 1 ! first polygon node
      jstart = 1
      jend = 0
      NPLnew = NPL
      do while (jpoint <= NPL)
         call get_startend(NPL - jpoint + 1, xpl(jpoint:NPL), ypl(jpoint:NPL), jstart, jend, dmiss)
         jstart = jstart + jpoint - 1
         jend = jend + jpoint - 1
         jpoint = max(jend + 2, jpoint + 1) ! min: make sure we advance the pointer

         jaleft = 0
         jaright = 0
         japole = 0
         do i = jstart, jend
            ip1 = i + 1
            if (ip1 > jend) ip1 = ip1 - (jend - jstart + 1)

!           check if the linesegment (i,i+1) exists
            if (xpl(i) == DMISS .or. ypl(i) == DMISS .or. xpl(ip1) == DMISS .or. ypl(ip1) == DMISS) cycle

!           compute two other canditates for xpl(i+1)
            x1 = xpl(ip1) - 360
            x2 = xpl(ip1) + 360

!           select candidate that is closest to xpl(i)
            dist = abs(xpl(ip1) - xpl(i)) ! did not use getdx intentionally
            dist1 = abs(x1 - xpl(i))
            dist2 = abs(x2 - xpl(i))

            if (dist1 < dist .and. dist1 < dist2) then
               if (ip1 /= jstart) then
                  xpl(ip1) = x1 ! keep first polygon node, polygon around pole if it needs to be moved
               else
                  japole = 1
               end if
               jaright = 1
            else if (dist2 < dist .and. dist2 < dist1) then
               if (ip1 /= jstart) then
                  xpl(ip1) = x2 ! keep first polygon node, polygon around pole if it needs to be moved
               else
                  japole = 1
               end if
               jaleft = 1
            end if
         end do

         if (jaalwayscopyleftright == 1) then
            jaleft = 1
            jaright = 1
         end if

         if (japole == 1) then ! special treatment
            jaleft = 0
            jaright = 0
         end if

         if (jaleft == 1 .or. jaright == 1) then
!           copy polygons to the left and to the right
            num = jend - jstart + 1

            i1 = NPLnew + 1
            if (jaleft == 1) then
               i2 = i1 + num + 1
            else
               i2 = i1
            end if

!           find new array size
            if (jaright == 1) then
               NPLnewest = i2 + num + 1
            else
               NPLnewest = i2
            end if

            call increasepol(NPLnewest, 1)

            xpl(NPLnew + 1:) = DMISS
            ypl(NPLnew + 1:) = DMISS
            zpl(NPLnew + 1:) = DMISS

            do i = jstart, jend
               if (xpl(i) /= DMISS .and. ypl(i) /= DMISS) then
                  i1 = i1 + 1
                  i2 = i2 + 1

                  if (jaleft == 1) then
                     xpl(i1) = xpl(i) - 360
                     ypl(i1) = ypl(i)
                     zpl(i1) = zpl(i)
                  end if

                  if (jaright == 1) then
                     xpl(i2) = xpl(i) + 360
                     ypl(i2) = ypl(i)
                     zpl(i2) = zpl(i)
                  end if
               end if
            end do

            NPLnew = NPLnewest
         end if
      end do

      NPL = NPLnew

!     check for poles
      jpoint = 1 ! first polygon node
      jstart = 1
      jend = 0
      do
         call get_startend(NPL - jpoint + 1, xpl(jpoint:NPL), ypl(jpoint:NPL), jstart, jend, dmiss)
         jstart = jstart + jpoint - 1
         jend = jend + jpoint - 1
         jpoint = max(jend + 2, jpoint + 1) ! min: make sure we advance the pointer

!        check if a polygon covers a pole
         i = jend
         x1 = xpl(jstart) - 360
         x2 = xpl(jstart) + 360
         dist = abs(xpl(jstart) - xpl(jend))
         dist1 = abs(x1 - xpl(jend))
         dist2 = abs(x2 - xpl(jend))
         if (dist1 < dist .or. dist2 < dist) then

            if (dist1 < dist2) then
               isign = -1
            else
               isign = 1
            end if

!           copy to left, to right and add two points at pole
            num = jend - jstart + 1
            call increasepol(NPL + 2 * num + 2, 1)

            do i = NPL, jend + 1, -1
               xpl(i + 2 * num + 2) = xpl(i)
               ypl(i + 2 * num + 2) = ypl(i)
               zpl(i + 2 * num + 2) = zpl(i)
            end do

            do i = jend, jstart, -1
!              copy to right
               xpl(2 * num + i) = xpl(i) + isign * 360
               ypl(2 * num + i) = ypl(i)
               zpl(2 * num + i) = zpl(i)

!              move original to center
               xpl(num + i) = xpl(i)
               ypl(num + i) = ypl(i)
               zpl(num + i) = zpl(i)

!              copy to left
               xpl(i) = xpl(i) - isign * 360
               ypl(i) = ypl(i)
               zpl(i) = zpl(i)
            end do

!           add two points at poles
            xpl(jend + 2 * num + 1) = xpl(jend + 2 * num)
            xpl(jend + 2 * num + 2) = xpl(jstart)

            if (ypl(jend + 2 * num) > 0) then
               ypl(jend + 2 * num + 1) = 90d0
               ypl(jend + 2 * num + 2) = 90d0
            else
               ypl(jend + 2 * num + 1) = -90d0
               ypl(jend + 2 * num + 2) = -90d0
            end if

            zpl(jend + 2 * num + 1) = zpl(jend + 2 * num)
            zpl(jend + 2 * num + 2) = zpl(jstart)

            NPL = NPL + 2 * num + 2
            jend = jend + 2 * num + 2
            jpoint = jpoint + 2 * num + 2
         end if

         if (jpoint > NPL) exit
      end do

      if (japartpols == 1) then
!        check if the right areas are selected and add bounding polygon if not so
         xmin = 1d99
         xmax = -xmin
         do k = 1, numk
            xmin = min(xk(k), xmin)
            xmax = max(xk(k), xmax)
         end do
         xmin = 0.5d0 * (xmin + xmax) - 180d0
         xmax = xmin + 360d0

!        clean up
         call dealloc_tpoly(partition_pol)
!        copy back to tpoly-type again
         call pol_to_tpoly(npartition_pol, partition_pol)

         do idmn = 1, Ndomains - 1
!           get polygons of this subdomain
!            call delpol()
!            call tpoly_to_pol(partition_pol,dble(idmn))

!           find a cell in this subdomain
            do i = 1, nump
               if (idomain(i) == idmn) then

!                 check if cell is inside
!                  in = -1
!                  call dbpinpol(xzw(i), yzw(i), in)
                  call dbpinpol_tpolies(partition_pol, xzw(i), yzw(i), in, dble(idmn))

!                  write(6,*) i, xzw(i), yzw(i), idomain(i)

!                 if cell is not inside: add bounding polygon
                  if (in == 0) then
                     call mess(LEVEL_INFO, 'swapping in/out for partitioning polygons of subdomain ', idmn)

!                     write(6,*) i, xzw(i), yzw(i), idomain(i)

                     call delpol()
                     NPL = 5
                     call increasepol(NPL, 0)
                     xpl(1:NPL) = (/xmin - 90d0, xmin - 90d0, xmin + 360d0 + 90d0, xmin + 360d0 + 90d0, xmin - 90d0/)
                     ypl(1:NPL) = (/90d0, -90d0, -90d0, 90d0, 90d0/)
                     zpl(1:NPL) = dble(idmn)
                     call pol_to_tpoly(npartition_pol, partition_pol, keepExisting=.true.)
                  end if

                  exit
               end if
            end do
         end do

!        copy tpoly-type partition polygons to polygon
         call delpol()
         call tpoly_to_pol(partition_pol)
      end if

      call mess(LEVEL_INFO, 'done')

      return
   end subroutine fix_global_polygons

end module m_fix_global_polygons
