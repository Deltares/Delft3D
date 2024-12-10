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
module m_inview2
   implicit none
contains
   logical function INVIEW2(X, Y, XX, YY)
      use precision, only: dp
      use M_MISSING
      use m_wearelt
      use m_dproject

      real(kind=dp) :: x, y, xx, yy

      ! ZIT IK IN ZOOMGEBIED? NULLEN EN DEFAULTS NIET, IN WERELDCOORD

      INVIEW2 = .false.
      if (X /= XYMIS) then
         call dPROJECT(X, Y, XX, YY, 1)
         if (XX > X1 .and. XX < X2 .and. &
             YY > Y1 .and. YY < Y2) then
            INVIEW2 = .true.
         end if
      else
         XX = XYMIS
         YY = XYMIS
      end if
      return
   end function INVIEW2
end module m_inview2
