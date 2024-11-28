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

module m_closeenough

implicit none

private

public :: closeenough

contains

  subroutine closeenough(x1, y1, x2, y2, r, ja)

     use m_missing, only: dmiss
     use m_sferic, only: jsferic, jasfer3D
     use geometry_module, only: dbdistance

     double precision :: x1, y1, x2, y2, r2, r
     integer :: ja

     ja = 0
     r2 = dbdistance(x1, y1, x2, y2, jsferic, jasfer3D, dmiss)
     if (r2 < r) then
        ja = 1
     end if
  end subroutine

end module m_closeenough
