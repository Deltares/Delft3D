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

!> copy polygon to spline
module m_copypoltospline

implicit none

private

public :: copypoltospline

contains

  subroutine COPYPOLTOSPLINE()
     use m_polygon
     use m_splines
     use m_missing
     use geometry_module, only: get_startend
     use m_delpol

     integer :: jstart, jend, jpoint

     jpoint = 1
     do while (jpoint <= NPL)
        call get_startend(NPL - jpoint + 1, xpl(jpoint:NPL), ypl(jpoint:NPL), jstart, jend, dmiss)

        jstart = jstart + jpoint - 1
        jend = jend + jpoint - 1

        if (jend - jstart + 1 > 1) then
           call addSplinePoints(mcs + 1, xpl(jstart:jend), ypl(jstart:jend))
        end if

        jpoint = jend + 1
     end do
     call delpol()

     return
  end subroutine COPYPOLTOSPLINE

end module m_copypoltospline
