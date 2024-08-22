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

  subroutine DISPFORMscale(value, fmt, NDEC)
     implicit none
     integer :: n1
     integer :: n2
     integer :: n3 ! nr of digits behind decimal dot
     integer :: ndec
     double precision :: value
     character fmt * (*)

     fmt = '(f10.3)'

     if (value == 0d0) then
        fmt = '(f3.1)'
        return
     end if

     n1 = int(log10(abs(value)))

     if (n1 < 6 .and. n1 > 0) then
        n2 = min(9, n1 + 3)
        n3 = 9 - n2
     else if (n1 >= -5 .and. n1 < 0) then
        n3 = 6
     else if (n1 == 0) then
        n3 = 6
     else
        fmt = '(e10.3)'
        return
     end if

     if (NDEC > 0) then
        n3 = min(n3, NDEC) ! try ndec, but only if it fits
     end if

     write (fmt(6:6), '(i1)') n3
     return
  end
