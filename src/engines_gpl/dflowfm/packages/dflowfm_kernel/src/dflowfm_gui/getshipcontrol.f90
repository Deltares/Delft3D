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

module m_getshipcontrol

   implicit none

contains

   subroutine GETSHIPCONTROL()
      use m_ship
      use m_drawthis
      implicit none

      integer :: key, n

      call InKeyEventIMM(KEY)

      n = 0
      !        pijltjesbeweging
      if (KEY == 128) then
         fstuw(1) = min(1d0, fstuw(1) + 0.02)
         n = 1
      else if (KEY == 129) then
         fstuw(1) = max(-1d0, fstuw(1) - 0.02)
         n = 1
      else if (KEY == 130) then
         fROER(1) = min(1d0, fROER(1) + 0.02)
         n = 1
      else if (KEY == 131) then
         fROER(1) = max(-1d0, fROER(1) - 0.02)
         n = 1
      else if (KEY == 53) then
         FSTUW(1) = 0d0
         FROER(1) = 0d0
         n = 1
      end if

      if (KEY == 87 .or. KEY == 87 + 32) then ! W
         fstuw(2) = min(1d0, fstuw(2) + 0.02)
         n = 2
      else if (KEY == 83 .or. KEY == 83 + 32) then ! S
         fstuw(2) = max(-1d0, fstuw(2) - 0.02)
         n = 2
      else if (KEY == 68 .or. KEY == 68 + 32) then
         fROER(2) = min(1d0, fROER(2) + 0.02)
         n = 2
      else if (KEY == 65 .or. KEY == 65 + 32) then
         fROER(2) = max(-1d0, fROER(2) - 0.02)
         n = 2
      else if (KEY == 81 .or. KEY == 81 + 32) then
         FSTUW(2) = 0d0
         FROER(2) = 0d0
         n = 2
      end if

      if (n > 0) then
         ndraw(1) = 0 ! no CLS
         call tekship()
      end if

   end subroutine getshipcontrol

end module m_getshipcontrol
