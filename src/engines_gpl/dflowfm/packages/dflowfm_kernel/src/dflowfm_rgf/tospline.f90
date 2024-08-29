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

       subroutine TOSPLINE(XX, YY, XV, YV)
          use M_SPLINES
          use m_spline
          implicit none

          double precision :: XX, YY, XV, YV

          double precision :: XI(maxsplen), XI2(maxsplen), YI(maxsplen), YI2(maxsplen)
          double precision :: TV, DIS
          integer :: IN, NUMPI

          IN = 1 ! Pick first spline
          call NUMP(IN, NUMPI)
          TV = NUMPI / 2d0
          call GETIJ(XSP, XI, maxspl, maxsplen, maxsplen, IN, IN, 1, NUMPI)
          call GETIJ(YSP, YI, maxspl, maxsplen, maxsplen, IN, IN, 1, NUMPI)
          call SPLINE(XI, NUMPI, XI2)
          call SPLINE(YI, NUMPI, YI2)
          call DISMIN(XI, XI2, YI, YI2, XX, YY, NUMPI, DIS, TV, XV, YV)
          return
       end subroutine tospline
