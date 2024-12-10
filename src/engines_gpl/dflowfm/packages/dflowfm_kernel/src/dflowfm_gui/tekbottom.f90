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

module m_tekbottom

   implicit none

contains

   subroutine TEKBOTTOM(MET)
      use precision, only: dp
      use m_dmovabs
      use m_dlnabs
      use m_wearelt
      use m_howtoview
      use m_set_col
      implicit none
      real(kind=dp) :: dz
      integer :: i
      integer :: k
      integer :: k1
      integer :: k2
      integer :: nz
      real(kind=dp) :: wd
      real(kind=dp) :: ybot
      real(kind=dp) :: ytop
      integer :: MET
      real(kind=dp) XD, YD, ZD, XX1, XX2, ZZ1, ZZ2
      call SETCOL(160)
      if (MET == 1) return

      WD = 1000
      XX2 = WD / 2
      XX1 = -XX2
      ZZ2 = WD / 2
      ZZ1 = -ZZ2
      DZ = 0
      NZ = 1

      if (JVIEW >= 3) then
         NZ = 11
         DZ = WD / (NZ - 1)
      end if

      if (MET == 2) then
         K1 = 1
         K2 = 2
      else if (MET == 3) then
         K1 = 2
         K2 = 2
      else if (MET == 4) then
         K1 = 1
         K2 = 1
      end if

      YD = YTOP
      call SETCOL(128) ! (112)
      do K = K1, K2
         if (K == 2) then
            YD = YBOT
            call SETCOL(89) ! 128)
         end if
         XD = XX1
         ZD = ZZ1
         do I = 1, NZ
            call DMOVABS(XX1, YD, ZD)
            call DLNABS(XX2, YD, ZD)
            call DMOVABS(XD, YD, ZZ1)
            call DLNABS(XD, YD, ZZ2)
            ZD = ZD + DZ
            XD = XD + DZ
         end do
      end do

      return
   end subroutine TEKBOTTOM

end module m_tekbottom
