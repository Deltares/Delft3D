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
module m_datum
   implicit none
contains
   subroutine DATUM(DATE)
      use m_dateandtimenow

      integer :: iyear, month, iday, ihour, minute, isecnd
      character DATE * 20
!              1  4  7   11 14 17
      DATE = 'hh:mm:ss, dd-mm-yyyy'

      call dateandtimenow(iyear, month, iday, ihour, minute, isecnd)

      write (DATE(1:2), '(I2.2)') IHOUR
      write (DATE(4:5), '(I2.2)') MINUTE
      write (DATE(7:8), '(I2.2)') ISECND
      write (DATE(11:12), '(I2.2)') IDAY
      write (DATE(14:15), '(I2.2)') MONTH
      write (DATE(17:20), '(I4)') IYEAR
      return
   end
end module m_datum
