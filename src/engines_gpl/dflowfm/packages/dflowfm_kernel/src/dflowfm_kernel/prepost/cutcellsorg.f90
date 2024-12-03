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

module m_cutcellsorg

   implicit none

   private

   public :: cutcellsorg

contains

   subroutine CUTCELLSORG()
      use precision, only: dp

      use m_crosslinkpoly, only: crosslinkpoly
      use m_netw
      use m_missing, only: dmiss, JINS
      use m_readyy
      use m_set_nod_adm
      use m_new_link

      integer :: ja, KMOD
      integer :: k
      integer :: k1
      integer :: k2
      integer :: k3
      integer :: k4
      integer :: km
      integer :: l
      integer :: ll
      integer :: lnu
      integer :: n
      integer :: n1
      integer :: n2
      integer :: nn
      integer :: nr
      integer, allocatable :: KNP(:), KNEW(:), LDIN(:), LD1(:), LD2(:)
      integer :: KK(4)

      real(kind=dp) :: XM, YM

      if (MXLAN == 0) return

      call READYY('CUTCELLS', 0d0)

      call SAVEPOL()

      allocate (LDIN(MXLAN), LD1(1000), LD2(1000))
      LDIN = 0; LD1 = 0; LD2 = 0

      LDIN(1) = -1
      do K = 1, MXLAN
         call DBPINPOL(XLAN(K), YLAN(K), LDIN(K), dmiss, jins, NPL, xpl, ypl, zpl) ! ALL LDB POINTS INSIDE POLYGON
      end do

      NR = 0; N1 = 0; N2 = 0
      do K = 1, MXLAN ! + 1 ! TODO [AvD] allocate met +1 en even doorlopen
         if (XLAN(K) /= -999d0 .and. LDIN(K) == 1) then
            if (N1 == 0) N1 = K
            N2 = K
            if (LDIN(K) == 1) JA = 1 ! SOME POINT OF LDB IS INSIDE POL,
         else if (N1 /= 0) then ! THIS LDB SEGMENT WILL BE HANDLED
            if (JA == 1) then
               NR = NR + 1; LD1(NR) = N1; LD2(NR) = N2
            end if
            N1 = 0; N2 = 0
         end if
      end do

      do NN = 1, NR

         N1 = LD1(NN); N2 = LD2(NN)
         call COPYLDBPIECETOPOL(N1, N2)
         call FINDCELLS(4) ! ALL FACES INSIDE LANDBOUNDARY PIECE

         allocate (KNP(NUMP)); KNP = 0
         allocate (KNEW(NUML)); KNEW = 0

         do N = 1, NUMP
            if (netcell(N)%N == 4) then
               K1 = netcell(N)%NOD(1)
               K2 = netcell(N)%NOD(2)
               K3 = netcell(N)%NOD(3)
               K4 = netcell(N)%NOD(4)
               KNP(N) = KC(K1) * KC(K2) * KC(K3) * KC(K4) ! COMPLETELY INSIDE = 1
            end if
         end do

         KMOD = max(1, NUMP / 100)
         do N = 1, NUMP

            if (mod(n, KMOD) == 0) call READYY('CUTCELLS', dble(n) / dble(nump))

            if (KNP(N) == 0) then ! AT LEAST 1 POINT OUTSIDE POLYGON

               do LL = 1, 4

                  L = netcell(N)%LIN(LL)

                  if (KNEW(L) == 0) then

                     call CROSSLINKPOLY(L, 0, 0, (/0/), (/0/), XM, YM, JA)

                     if (JA == 1) then
                        call DSETNEWPOINT(XM, YM, KM)
                        KNEW(L) = KM
                     end if

                  end if

               end do

            end if

         end do

         do N = 1, NUMP

            K = 0
            do LL = 1, 4

               L = netcell(N)%LIN(LL)
               K1 = KN(1, L); K2 = KN(2, L)

               if (KNP(N) == 0) then ! SHOULD BE HANDLED

                  if (KNEW(L) /= 0) then ! NIEUW PUNT KOPPELEN

                     if (KNEW(L) > 0) then
                        if (KC(K1) == 1) then
                           call NEWLINK(KNEW(L), K2, LNU)
                        else
                           call NEWLINK(KNEW(L), K1, LNU)
                        end if
                        KNEW(L) = -1 * KNEW(L)
                     end if
                     K = K + 1; KK(K) = abs(KNEW(L))

                  end if

               end if

               if (K1 /= 0 .and. K2 /= 0) then
                  if (KC(K1) == 1 .or. KC(K2) == 1) then
                     KN(1, L) = 0; KN(2, L) = 0
                  end if
               end if

            end do

            if (K >= 2) then
               call NEWLINK(KK(1), KK(2), LNU)
            end if

            if (K >= 3) then
               call NEWLINK(KK(2), KK(3), LNU)
            end if

            if (K >= 4) then
               call NEWLINK(KK(3), KK(4), LNU)
            end if

         end do

         call SETNODADM(0)

         deallocate (KNP, KNEW)

      end do

      deallocate (LDIN, LD1, LD2)

      call RESTOREPOL()

      call SETNODADM(0)

      call READYY('CUTCELLS', -1d0)

   end subroutine CUTCELLSORG

end module m_cutcellsorg
