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

!>  smooth the link-based aspect ratios (SLR/R01) along parallel and perpendicular mesh lines (obsolete)
subroutine orthonet_smooth_aspect(aspect, iexit)
   use m_netw
   use m_orthosettings
   use unstruc_messages
   use m_alloc
   use geometry_module, only: dcosphi
   use m_sferic, only: jsferic, jasfer3D
   use m_missing, only: dxymis

   implicit none

   double precision, dimension(numL) :: aspect !< aspect ratio at the links
   integer :: iexit !< 1 if no errors have occured

   integer, save :: NMKMAX = 4 ! maximum of neighbors considered
   integer, dimension(:), allocatable :: nmkx ! number  of neighbors considered
   integer, dimension(:, :), allocatable :: kkL ! neighboring link
   integer, dimension(:, :), allocatable :: ww ! weights of neighboring links

   double precision :: ATPF1, cosphi, maxcosphi
   double precision :: x1, y1, x2, y2, x3, y3
   double precision :: dum, ww1

   character(len=51) :: numstr

   double precision, parameter :: COSMIN = 0.5d0, COSMAX = 0.0d0, EPS = 1d-8

   integer :: kk, num, k1, k2, k3, nn, kk1, L1, nummax

   integer, parameter :: ITAPSM = 2

   iexit = 1

   if (ATPF == 1d0) return ! no smoothing

   if (ITAPSM < 1) then ! set aspect=1d0
      aspect = 1d0
      return
   end if

   iexit = 0

   allocate (nmkx(numL), kkL(NMKMAX, numL), ww(NMKMAX, numL))
   nmkx = 0
   kkL = 0
   ww = 0

   ATPF1 = 1 - ATPF

!-----------------------------------------------------------
!  determine connected links that have the same orientation

   nummax = 0 ! maximum of connected links

   chsz: do ! while array size not exceeded
      do kk = 1, numL
         !     find nodes 1 and 2
         k1 = kn(1, kk)
         k2 = kn(2, kk)

         x1 = xk(k1)
         y1 = yk(k1)

         x2 = xk(k2)
         y2 = yk(k2)

         !     find neighboring link connected to node k1
         maxcosphi = COSMIN
         nn = nmk(k1)
         do kk1 = 1, nn
            num = 0
            L1 = nod(k1)%lin(kk1)
            k3 = sum(kn(1:2, L1)) - k1
            if ((k3 == k2) .or. (kc(k3) == 0)) cycle

            x3 = xk(k3)
            y3 = yk(k3)
            cosphi = dcosphi(x2, y2, x1, y1, x1, y1, x3, y3, jsferic, jasfer3D, dxymis)

            !        find parallel links connected to node 1
            !           set weight ww1 > 0
            if (cosphi > COSMIN) then
               nmkx(kk) = nmkx(kk) + 1
               num = nmkx(kk)

               !           check array size
               if (num > NMKMAX) then
                  NMKMAX = num
                  call realloc(kkL, (/NMKMAX, numL/))
                  call realloc(ww, (/NMKMAX, numL/))
               end if

               kkL(num, kk) = L1
               !            ww(num, kk) = 1d0
               ww(num, kk) = 1d0

               !        find perpendicular links connected to node 1
               !           set weight ww1 < 0
            else if (abs(cosphi) < COSMAX) then
               nmkx(kk) = nmkx(kk) + 1
               num = nmkx(kk)

               !           check array size
               if (num > NMKMAX) then
                  NMKMAX = num
                  call realloc(kkL, (/NMKMAX, numL/))
                  call realloc(ww, (/NMKMAX, numL/))
               end if

               kkL(num, kk) = L1
               ww(num, kk) = -1d0
            end if
         end do

         !     find neighboring link connected to node k2
         maxcosphi = COSMIN
         nn = nmk(k2)
         do kk1 = 1, nn
            L1 = nod(k2)%lin(kk1)
            k3 = sum(kn(1:2, L1)) - k2
            if ((k3 == k1) .or. (kc(k3) == 0)) cycle

            x3 = xk(k3)
            y3 = yk(k3)
            cosphi = dcosphi(x1, y1, x2, y2, x2, y2, x3, y3, jsferic, jasfer3D, dxymis)

            !        find parallel links connected to node 2
            !           set weight ww1 > 0
            if (cosphi > COSMIN) then
               nmkx(kk) = nmkx(kk) + 1
               num = nmkx(kk)

               !           check array size
               if (num > NMKMAX) then
                  NMKMAX = num
                  call realloc(kkL, (/NMKMAX, numL/))
                  call realloc(ww, (/NMKMAX, numL/))
               end if

               kkL(num, kk) = L1
               ww(num, kk) = 1d0

               !        find perpendicular links connected to node 2
               !           set weight ww1 < 0
            else if (abs(cosphi) < COSMAX) then
               nmkx(kk) = nmkx(kk) + 1
               num = nmkx(kk)

               !           check array size
               if (num > NMKMAX) then
                  NMKMAX = num
                  call realloc(kkL, (/NMKMAX, numL/))
                  call realloc(ww, (/NMKMAX, numL/))
               end if

               kkL(num, kk) = L1
               ww(num, kk) = -1d0
            end if
            nummax = max(nummax, num)
         end do

      end do

      !-----------------------------------------------------------
      !  smooth aspect ratio
      do num = 1, ITAPSM
         do kk = 1, numL
            dum = aspect(kk) ! summed contribution
            nn = 1 ! number of links used
            do kk1 = 1, nmkx(kk)
               ww1 = ww(kk1, kk)
               L1 = kkL(kk1, kk)

               !           parallel link: ww1>0
               if (ww1 > 0d0) then
                  nn = nn + 1
                  dum = dum + ww1 * aspect(L1)

                  !           perpendicular link: ww1<0
               else if (aspect(L1) > EPS) then
                  nn = nn + 1
                  dum = dum - ww1 / aspect(L1)
               end if
            end do

            !        partial smoothing/orthogonalization
            aspect(kk) = ATPF * aspect(kk) + ATPF1 * dum / nn

         end do
      end do

      if (nummax < NMKMAX) then
         write (numstr, "('orthonet_smooth_aspect: NMKMAX may be reduced to ', I2)") nummax
         call ktext(numstr, 1, 3, 11)
      end if

      iexit = 1
      exit
   end do chsz

   if (iexit /= 1) then
      write (MSGBUF, '(A)') 'orthonet_smooth_aspect: nmkx > NMKMAX'
      write (numstr, "('orthonet_smooth_aspect: nmkx =', I2, ' > NMKMAX =', I2)") num, NMKMAX
      call msg_flush()
!      call qnerror('orthonet_smooth_aspect: nmkx > NMKMAX', ' ', ' ')
      call qnerror(numstr(1:45), ' ', ' ')
   end if

   deallocate (nmkx, kkL, ww)
end subroutine
