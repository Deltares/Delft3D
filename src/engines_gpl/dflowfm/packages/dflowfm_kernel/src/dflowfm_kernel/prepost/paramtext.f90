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
module m_paramtext
   implicit none
contains
   subroutine PARAMTEXT(OPTION, NR)
      use M_isoscaleunit

      integer :: l1
      integer :: l2
      character(len=*) OPTION
      integer NR
      L1 = index(OPTION, '(')
      L2 = index(OPTION, ')')
      UNIT(NR) = ' '; PARAMTEX(NR) = ' '
      if (L1 /= 0) write (UNIT(NR) (1:L2 - L1 + 1), '(A)') OPTION(L1:L2)
      write (PARAMTEX(NR) (1:14), '(A)') OPTION(1:14)
      return
   end subroutine PARAMTEXT
end module m_paramtext
