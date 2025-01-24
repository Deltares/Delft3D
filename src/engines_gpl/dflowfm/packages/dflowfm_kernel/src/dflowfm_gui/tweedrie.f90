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

module m_tweedrie

   implicit none

contains

   subroutine TWEEDRIE(X, Y, XD, YD, ZD)
      use precision, only: dp
      use m_howtoview
      implicit none
      real(kind=dp) :: X, Y, XD, YD, ZD

      if (JVIEW == 1) then
         XD = X
         YD = Y
         ZD = XYZ
      else if (JVIEW == 2) then
         ZD = X
         YD = Y
         XD = XYZ
      else if (JVIEW == 3) then
         XD = X
         ZD = -Y
         YD = XYZ
      else if (JVIEW == 4) then
         !    CALL DVIEW(XD,YD,ZD,X,Y,Z)  ! MOET NOG INVERS MAKEN
         XD = X
         YD = Y
         ZD = XYZ
      else !In all other cases (e.g. when HOWTOVIEW is not set, e.g. in the gridgeom library)
         xd = x
         yd = y
         zd = xyz
      end if

      return
   end subroutine TWEEDRIE

end module m_tweedrie
