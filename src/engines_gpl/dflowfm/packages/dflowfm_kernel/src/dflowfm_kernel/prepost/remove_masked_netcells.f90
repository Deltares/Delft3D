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

   !> remove "dry"masked netcells (cellmask==1) from netcell administration
   !> typically used in combination with a drypoints file (samples or polygons)
   !> \see samples_to_cellmask and \see polygon_to_cellmask
   !> note: we do not want to alter the netnodes and netlinks and will therefore not change kn and nod%lin
   subroutine remove_masked_netcells()
      use network_data
      use m_flowgeom, only: xz, yz, ba
      use m_alloc
      use m_partitioninfo, only: idomain, iglobal_s
      implicit none

      integer, dimension(:), allocatable :: numnew ! permutation array

      integer :: i, ic, icL, icR, icnew, isL, isR, L, num, N, numpnew

      integer :: jaidomain
      integer :: jaiglobal_s

      num = 0

!     check if cellmask array is allocated
      if (.not. allocated(cellmask)) goto 1234

!     check if cellmask array is sufficiently large
      if (ubound(cellmask, 1) < nump1d2d) goto 1234

!     see if we can update idomain
      jaidomain = 0
      if (allocated(idomain)) then
         if (ubound(idomain, 1) >= nump1d2d) then
            jaidomain = 1
         end if
      end if

!     see if we can update iglobal
      jaiglobal_s = 0
      if (allocated(iglobal_s)) then
         if (ubound(iglobal_s, 1) >= nump1d2d) then
            jaiglobal_s = 1
         end if
      end if

      allocate (numnew(nump1d2d))
      numnew = 0
      numpnew = 0

      do ic = 1, nump1d2d

         if (cellmask(ic) == 0) then ! keep cell
            num = num + 1
            numnew(ic) = num

!           write entry in netcell, use property num<=ic
            if (num /= ic) then
               N = netcell(ic)%N
               netcell(num)%N = N

!              reallocate if necessary
               if (ubound(netcell(num)%nod, 1) < N) then
                  call realloc(netcell(num)%nod, N, keepExisting=.false.)
               end if
               if (ubound(netcell(num)%lin, 1) < N) then
                  call realloc(netcell(num)%lin, N, keepExisting=.false.)
               end if

!              move data
               do i = 1, N
                  netcell(num)%nod(i) = netcell(ic)%nod(i)
                  netcell(num)%lin(i) = netcell(ic)%lin(i)
               end do
               if (jaidomain == 1) then
                  idomain(num) = idomain(ic)
               end if
               if (jaiglobal_s == 1) then
                  iglobal_s(num) = iglobal_s(ic)
               end if
            end if
         end if

         if (ic == nump) then
!           determine new nump
            numpnew = num
         end if
      end do

!     clean up remainder of netcell
      do ic = num + 1, nump1d2d
         netcell(ic)%N = 0
         deallocate (netcell(ic)%nod)
         deallocate (netcell(ic)%lin)
      end do

      if (jaidomain == 1) then
         call realloc(idomain, num, keepExisting=.true.)
      end if

      if (jaiglobal_s == 1) then
         call realloc(iglobal_s, num, keepExisting=.true.)
      end if

!     change lnn, lne
      do L = 1, numL
         if (lnn(L) > 1) then
!           check if right cell still exists
            icR = lne(2, L)
            isR = sign(1, icR)
            if (numnew(abs(icR)) == 0) then
!              remove right cell
               lnn(L) = lnn(L) - 1
               lne(2, L) = 0
            else
!              use new right cell number
               lne(2, L) = isR * numnew(abs(icR))
            end if
         end if

!        check if left cell still exists
         icL = lne(1, L)
         isL = sign(1, icL)
         if (icL /= 0) then
            if (numnew(abs(icL)) == 0) then
!              remove left cell
               lnn(L) = lnn(L) - 1
               if (lnn(L) == 1) then
!                 move right cell
                  lne(1, L) = lne(2, L)
                  lne(2, L) = 0
               else
                  lne(1, L) = 0
               end if
            else
!              use new left cell number
               lne(1, L) = isL * numnew(abs(icL))
            end if
         end if
      end do

!     update cell centers and bed areas
      do ic = 1, nump1d2d
         icnew = numnew(ic)
         if (icnew /= 0) then ! use property icnew<=ic
            xz(icnew) = xz(ic)
            yz(icnew) = yz(ic)
            xzw(icnew) = xzw(ic)
            yzw(icnew) = yzw(ic)
            ba(icnew) = ba(ic)
         end if
      end do

!     update number of cells
      nump1d2d = num
      nump = numpnew

1234  continue

!     deallocate
      if (allocated(numnew)) deallocate (numnew)

      return
   end subroutine remove_masked_netcells
