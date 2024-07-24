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

  subroutine DRIEEEN(XD, YD, ZD, Z)
     implicit none
     integer :: jav
     integer :: jview
     double precision :: xyz
     common / HOWTOVIEW / JVIEW, JAV, XYZ ! 1,2,3 OF 4
     double precision :: XD, YD, ZD, Z
     if (JVIEW == 1) then ! TEGEN Z-AS
        Z = ZD
     else if (JVIEW == 2) then ! VAN LINKS
        Z = XD
     else if (JVIEW == 3) then ! NORMAAL
        Z = YD
     else if (JVIEW == 4) then
        Z = XYZ
     end if
     return
  end subroutine DRIEEEN
