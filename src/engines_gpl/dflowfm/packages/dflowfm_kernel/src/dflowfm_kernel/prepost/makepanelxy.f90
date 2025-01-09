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

module m_makepanelxy
   use m_dputar, only: dputar

   implicit none

   private

   public :: makepanelxy

contains

   subroutine MAKEPANELXY(JPANEL)
      use precision, only: dp
      use m_netw
      use M_AFMETING
      use gridoperations
      use m_howtoview
      use m_cconstants

      integer :: JPANEL

      real(kind=dp) :: ael
      real(kind=dp) :: cs
      real(kind=dp) :: dx
      real(kind=dp) :: dy
      integer :: i
      integer :: i2
      integer :: j
      integer :: k
      integer :: k1
      integer :: k2
      integer :: l
      integer :: l0
      integer :: ld
      integer :: ll
      integer :: lld
      integer :: llu
      integer :: lr
      integer :: lrd
      integer :: lru
      integer :: lu
      integer :: n
      integer :: numdik
      integer :: numels
      integer :: numh
      integer :: numrb
      integer :: numv
      real(kind=dp) :: rmas
      real(kind=dp) :: rmk
      real(kind=dp) :: rml
      real(kind=dp) :: rnl
      real(kind=dp) :: sn
      real(kind=dp) :: x
      real(kind=dp) :: xkk
      real(kind=dp) :: y
      real(kind=dp) :: ykk

      !    common / SET2 / REKMAX, DRUKMAX, NUMDIK, JOFREEZE
      real(kind=dp) DX1, DY1, DZ1
      real(kind=dp) :: DR(4)
      integer :: INI, JaNET
      data INI/0/
      JaNET = 1 - JPANEL

      if (NPL <= 1 .or. NPL > 4) return

      call SAVENET()

      do I = 1, NPL
         I2 = I + 1
         if (I == 4) I2 = 1
         DX = XPL(I) - XPL(I2)
         DY = YPL(I) - YPL(I2)
         DR(I) = sqrt(DX * DX + DY * DY)
      end do

      if (NPL == 4) then
         RML = (DR(1) + DR(3)) / 2
         RNL = (DR(2) + DR(4)) / 2
      else if (NPL == 2) then
         RML = DR(1)
         RNL = min(RML, RWIDTH)
      else if (NPL == 3) then
         RML = DR(1)
         RNL = DR(2)
      end if

      if (NPL == 2 .and. JANET == 0 .or. NPL == 3) then
         DX = XPL(2) - XPL(1)
         DY = YPL(2) - YPL(1)
         SN = DY / RML
         CS = DX / RML
         XPL(3) = XPL(2) - RNL * SN
         YPL(3) = YPL(2) + RNL * CS
         XPL(4) = XPL(1) - RNL * SN
         YPL(4) = YPL(1) + RNL * CS
         NPL = 4
      end if

      RLENGTH = max(RML, RNL)
      RWIDTH = min(RML, RNL)
      if (JVAST == 0) then
         RTHICK = RWIDTH / 4
      end if
      if (RNL < RML) then
         NC = NUMDIK
         MC = (NUMDIK - 1) * RLENGTH / RWIDTH + 1
      else
         MC = NUMDIK
         NC = (NUMDIK - 1) * RLENGTH / RWIDTH + 1
      end if

      if (JANET == 1) then ! Netstructuur
         AEL = PI * RDIAM * RDIAM / 4 ! RDIAM in mm
      else
         NUMELS = (NC + 2 * (NC - 1)) ! Net plus kruisverbinding
         AEL = 1e6 * RWIDTH * RTHICK / NUMELS ! oppervlakte in mm2, BREEDTE in m
         RMK = RHO * RLENGTH * RWIDTH * RTHICK / (MC * NC) !knoop massa (kg)
      end if

      K0 = NUMK
      L0 = NUML

      if (NPL == 2 .and. JANET == 1) then ! Toevoegen lijnelement

         return
      end if

      !  IF (NPL .EQ. 4) THEN
      !     ZZ = 0
      !     CALL ADDBLOCK(XPL,YPL,ZZ,JANET)
      !     RETURN
      !  ENDIF

!     knoopnummers uitdelen
      do J = 1, NC
         Y = dble(J - 1) / dble(NC - 1)
         do I = 1, MC
            X = dble(I - 1) / dble(MC - 1)
            K = (J - 1) * MC + I + K0
            XKK = XPL(1) * (1 - X) * (1 - Y) + XPL(2) * (X) * (1 - Y) + &
                  XPL(3) * (X) * (Y) + XPL(4) * (1 - X) * (Y)
            YKK = YPL(1) * (1 - X) * (1 - Y) + YPL(2) * (X) * (1 - Y) + &
                  YPL(3) * (X) * (Y) + YPL(4) * (1 - X) * (Y)
            if (JVIEW == 1) then
               XK(K) = XKK
               YK(K) = YKK
               ZK(K) = 0d0
            else if (JVIEW == 2) then
               XK(K) = XKK
               YK(K) = 0d0
               ZK(K) = YKK
            else if (JVIEW == 3) then
               XK(K) = 0d0
               YK(K) = XKK
               ZK(K) = YKK
            end if
         end do
      end do
      NUMK = K0 + MC * NC

!     horizontale elementen krijgen twee knoopnummers
      L = L0
      do J = 1, NC
         do I = 1, MC - 1
            L = L + 1
            K1 = (J - 1) * MC + I + K0
            K2 = (J - 1) * MC + I + 1 + K0
            KN(1, L) = K1
            KN(2, L) = K2
         end do
      end do
      NUMH = L

!     verticale elementen
      do J = 1, NC - 1
         do I = 1, MC
            L = L + 1
            K1 = (J - 1) * MC + I + K0
            K2 = (J) * MC + I + K0
            KN(1, L) = K1
            KN(2, L) = K2
         end do
      end do
      NUMV = L

      if (JPANEL == 1) then
!         diagonalen naar rechtsboven
         do J = 1, NC - 1
            do I = 1, MC - 1
               L = L + 1
               K1 = (J - 1) * MC + I + K0
               K2 = (J) * MC + I + 1 + K0
               KN(1, L) = K1
               KN(2, L) = K2
            end do
         end do
         NUMRB = L

!         diagonalen naar linksboven
         do J = 1, NC - 1
            do I = 2, MC
               L = L + 1
               K1 = (J - 1) * MC + I + K0
               K2 = (J) * MC + I - 1 + K0
               KN(1, L) = K1
               KN(2, L) = K2
            end do
         end do
      end if
      NUML = L

      do J = 1, NC
         do I = 1, MC
            K = (J - 1) * MC + I + K0
            if (I < MC) then ! ELEMENT NAAR RECHTS
               NMK(K) = NMK(K) + 1
               LR = L0 + (J - 1) * (MC - 1) + I
               call SETNODLIN(K, NMK(K), LR)
            end if
            if (I > 1) then ! LINKS
               NMK(K) = NMK(K) + 1
               LL = L0 + (J - 1) * (MC - 1) + I - 1
               call SETNODLIN(K, NMK(K), LL)
            end if
            if (J < NC) then ! BOVEN
               NMK(K) = NMK(K) + 1
               LU = NUMH + (J - 1) * MC + I
               call SETNODLIN(K, NMK(K), LU)
            end if
            if (J > 1) then ! ONDER
               NMK(K) = NMK(K) + 1
               LD = NUMH + (J - 2) * MC + I
               call SETNODLIN(K, NMK(K), LD)
            end if
            if (JPANEL == 1) then
               if (J < NC .and. I < MC) then ! RECHTS BOVEN
                  NMK(K) = NMK(K) + 1
                  LRU = NUMV + (J - 1) * (MC - 1) + I
                  call SETNODLIN(K, NMK(K), LRU)
               end if
               if (J > 1 .and. I > 1) then ! LINKSONDER
                  NMK(K) = NMK(K) + 1
                  LLD = NUMV + (J - 2) * (MC - 1) + I - 1
                  call SETNODLIN(K, NMK(K), LLD)
               end if
               if (I > 1 .and. J < NC) then ! LINKSBOVEN
                  NMK(K) = NMK(K) + 1
                  LLU = NUMRB + (J - 1) * (MC - 1) + I - 1
                  call SETNODLIN(K, NMK(K), LLU)
               end if
               if (J > 1 .and. I < MC) then ! RECHTSONDER
                  NMK(K) = NMK(K) + 1
                  LRD = NUMRB + (J - 2) * (MC - 1) + I
                  call SETNODLIN(K, NMK(K), LRD)
               end if
            end if
         end do
      end do

      do L = L0 + 1, NUML
         K1 = KN(1, L)
         K2 = KN(2, L)
         DX1 = XK(K1) - XK(K2)
         DY1 = YK(K1) - YK(K2)
         DZ1 = ZK(K1) - ZK(K2)
         ! RL(L) = SQRT(DX1*DX1+DY1*DY1+DZ1*DZ1)
         ! EA(L) = AEL ! Voorlopig alle elementen even grote doorsnede
      end do

      do K = K0 + 1, NUMK
         if (NETFLOW == 1) then
            do N = 1, NMK(K)
               L = NOD(K)%LIN(N)
               RMAS = RHO ! *RL(L)*EA(L)*1E-6
               ! RM(K) = RM(K) + 0.5d0*RMAS
            end do
            ! RM(K)  = RMK
         end if
         KC(K) = 1
      end do

      if (INI == 0) then
         do J = 1, NC ! Uiteinden LINKS VASTZETTEN
            K = (J - 1) * (MC) + 1 + K0
            KC(K) = -1
         end do
      end if

      call DPUTAR(XK, XK1, KMAX)
      call DPUTAR(YK, YK1, KMAX)
      call DPUTAR(ZK, ZK1, KMAX)

      return
   end subroutine MAKEPANELXY

end module m_makepanelxy
