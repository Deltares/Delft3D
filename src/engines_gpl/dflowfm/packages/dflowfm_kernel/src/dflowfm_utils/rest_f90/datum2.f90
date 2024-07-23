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

      subroutine DATUM2(DATE)
         use unstruc_display, only: jadatetime
         use system_utils, only: FILESEP
         implicit none
         integer :: iyear, month, iday, ihour, minute, isecnd
         character DATE * 20

!              1  4  7   11 14 17

         if (jadatetime == 0) then
            DATE = FILESEP
         else
            DATE = '_yymmddhhmmss'//FILESEP

            call dateandtimenow(iyear, month, iday, ihour, minute, isecnd)

            write (DATE(2:3), '(I2.2)') IYEAR - 2000
            write (DATE(4:5), '(I2.2)') month
            write (DATE(6:7), '(I2.2)') iday
            write (DATE(8:9), '(I2.2)') Ihour
            write (DATE(10:11), '(I2.2)') minute
            write (DATE(12:13), '(I2.2)') isecnd
         end if
         return
      end
