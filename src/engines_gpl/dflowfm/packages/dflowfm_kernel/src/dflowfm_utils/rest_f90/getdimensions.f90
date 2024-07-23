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

      subroutine GETDIMENSIONS(MXD, NXD, MXLN, NSX)
         implicit none
         integer :: mout
         integer :: mxd
         integer :: mxln
         integer :: nsx
         integer :: nxd
         character GETAL * 100
         logical THISISANUMBER, JAWEL

         MXD = 500 ! ROOSTERS EN SPLINES M-RICHTING
         NXD = 500 ! ROOSTERS EN SPLINES N-RICHTING
         MXLN = 100000 ! land boundary
         NSX = 100000 ! SAMPLES

         GETAL = ' '
         call get_command_argument(1, GETAL)
         if (THISISANUMBER(GETAL)) read (GETAL, *) MXD

         GETAL = ' '
         call get_command_argument(2, GETAL)
         if (THISISANUMBER(GETAL)) read (GETAL, *) NXD

         GETAL = ' '
         call get_command_argument(3, GETAL)
         if (THISISANUMBER(GETAL)) read (GETAL, *) MXLN

         GETAL = ' '
         call get_command_argument(4, GETAL)
         if (THISISANUMBER(GETAL)) read (GETAL, *) NSX

         inquire (FILE='rgfdim', EXIST=JAWEL)
         if (JAWEL) then
            MOUT = 10
            call oldfil(MOUT, 'rgfdim')
            read (MOUT, *, ERR=999) MXD, NXD, MXLN, NSX
            call doclose(MOUT)
         end if
999      continue
         return
      end
