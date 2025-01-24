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

module m_teknodenums

   implicit none

contains

   subroutine TEKNODENUMS(MET, NCOL)
      use precision, only: dp
      use m_dhtext
      use m_dhitext
      use M_MISSING
      use m_netw
      use m_drawthis
      use m_halt2
      use m_set_col
      use m_invnod
      implicit none
      integer :: MET, NCOL
      integer :: k
      integer :: k1
      integer :: k2
      integer :: key
      integer :: l
      integer :: n
      real(kind=dp) X, Y, Z

      call SETCOL(NCOL)
      KMOD = max(1, NUMK / 100)
      do K = 1, NUMK
         if (.not. INVNOD(K)) cycle
         X = XK(K)
         Y = YK(K)
         Z = ZK(K)

         if (mod(K, KMOD) == 0) then
            call HALT2(KEY)
            if (KEY == 1) then
               return
            end if
         end if

         if (RNOD(K) /= dmiss) then
            if (MET == 2 .or. MET >= 6) then
               if (NDRAW(8) == 2 .or. NDRAW(8) == 3 .or. NDRAW(8) == 5) then
                  call DHITEXT(int(RNOD(K)), X, Y)
               else if (MET == 4) then
                  do N = 1, NMK(K)
                     L = NOD(K)%LIN(N)
                     K1 = KN(1, L)
                     K2 = KN(2, L)
                     X = 0.5d0 * (XK(K1) + 0.5d0 * XK(K2))
                     Y = 0.5d0 * (YK(K1) + 0.5d0 * YK(K2))
                     Z = 0.5d0 * (ZK(K1) + 0.5d0 * ZK(K2))
                     call DHITEXT(L, X, Y)
                  end do
               else
                  call dHTEXT(dble(RNOD(K)), X, Y, Z)
               end if
            end if
         end if
      end do

      return
   end subroutine TEKNODENUMS

end module m_teknodenums
