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
module m_three_two
use m_dview

   implicit none
contains
   subroutine DRIETWEE(XD, YD, ZD, X, Y, Z)
      use m_howtoview

      double precision XD, YD, ZD, X, Y, Z

      if (JVIEW == 1) then ! NORMAL
         X = XD
         Y = YD
         Z = ZD
      else if (JVIEW == 2) then ! FROM LEFT
         X = ZD
         Y = YD
         Z = XD
      else if (JVIEW == 3) then ! FROM TOP
         X = XD
         Y = -ZD
         Z = YD
      else if (JVIEW == 4) then
         !    CALL DVIEW(XD,YD,-ZD,X,Y,Z)
         call DVIEW(XD, YD, -ZD, X, Y, Z)
      else !In all other cases (e.g. when HOWTOVIEW is not set, e.g. in the gridgeom library)
         x = xd
         y = yd
         z = zd
      end if
      return
   end subroutine DRIETWEE
end module m_three_two
