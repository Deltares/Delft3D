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

module m_wriarcsam

implicit none

private

public :: wriarcsam

contains

      subroutine WRIARCsam(MARC, data_array, MMAX, NMAX, MC, NC, X0, Y0, DX, DY, dmiss)
         use m_writearcinfoheader
         use precision, only: dp

         real(kind=dp) :: dmiss
         integer :: mmax
         integer :: nmax
         real(kind=dp), dimension(MMAX, NMAX) :: data_array
         real(kind=dp) :: dx, dy
         integer :: i
         integer :: j
         integer :: marc
         integer :: mc
         integer :: nc
         real(kind=dp) :: x0
         real(kind=dp) :: y0

         call WRITEARCINFOHEADER(MARC, MC, NC, X0, Y0, DX, DY, dmiss)

         do J = 1, NC
            write (MARC, '(99999999F10.2)') (data_array(J, I), I=1, MC) ! SPvdP: j-index is fastest running in sample arrays
         end do

         return
      end

end module m_wriarcsam
