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

module m_tekfaces

   implicit none

contains

   subroutine TEKFACES()
      use precision, only: dp

      use unstruc_colors, only: ncolln
      use m_netw
      use stdlib_sorting, only: sort_index
      use m_three_two
      use m_pfiller

      implicit none
      integer :: ierr
      integer :: k
      integer :: l
      integer :: n
      integer :: ncol
      integer :: ni

      real(kind=dp) XX, YY, ZZ, XH(10), YH(10), ZH(10)
      integer, allocatable, save :: NP(:)
      real(kind=dp) :: XP, YP
      real(kind=dp), allocatable, save :: ZP(:)
      real(kind=dp), allocatable :: zp_copy(:)

      if (size(NP) < NUMP) then
         if (allocated(NP)) deallocate (NP, ZP)
         allocate (NP(NUMP), ZP(NUMP), STAT=IERR)
      end if

      if (NUMP /= 0) then
         do N = 1, NUMP
            XX = 0
            YY = 0
            ZZ = 0
            do K = 1, netcell(N)%N
               XX = XX + XK(netcell(N)%NOD(K))
               YY = YY + YK(netcell(N)%NOD(K))
               ZZ = ZZ + ZK(netcell(N)%NOD(K))
            end do
            XX = XX / netcell(N)%N
            YY = YY / netcell(N)%N
            ZZ = ZZ / netcell(N)%N
            call DRIETWEE(XX, YY, ZZ, XP, YP, ZP(N))
         end do
         zp_copy = zp(1:nump)
         call sort_index(zp_copy, NP(1:nump))

         do L = NUMP, 1, -1
            N = NP(L)
            NI = netcell(N)%N
            do K = 1, NI
               XH(K) = XK(netcell(N)%NOD(K))
               YH(K) = YK(netcell(N)%NOD(K))
               ZH(K) = ZK(netcell(N)%NOD(K))
            end do
            if (NI == 6) then
               NCOL = 221
            end if
            if (NI == 5) then
               NCOL = 111
            end if
            if (NI == 4) then
               NCOL = 31
            end if
            if (NI == 3) then
               NCOL = 171
            end if
            call PFILLER(XH, YH, NI, NCOL, NCOLLN)
         end do
      end if

      return
   end subroutine TEKFACES

end module m_tekfaces
