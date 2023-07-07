!!  Copyright (C)  Stichting Deltares, 2012-2023.
!!
!!  This program is free software: you can redistribute it and/or modify
!!  it under the terms of the GNU General Public License version 3,
!!  as published by the Free Software Foundation.
!!
!!  This program is distributed in the hope that it will be useful,
!!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
!!  GNU General Public License for more details.
!!
!!  You should have received a copy of the GNU General Public License
!!  along with this program. If not, see <http://www.gnu.org/licenses/>.
!!
!!  contact: delft3d.support@deltares.nl
!!  Stichting Deltares
!!  P.O. Box 177
!!  2600 MH Delft, The Netherlands
!!
!!  All indications and logos of, and references to registered trademarks
!!  of Stichting Deltares remain the property of Stichting Deltares. All
!!  rights reserved.

      subroutine pop(ij)
!           5-69
!           eliminate the ij entry from the matrix list.  move all
!        entries up by one
      use m_char1
      naij = naij - 1
      do 10 i=ij,naij
         irow(i) = irow(i+1)
         jcol(i) = jcol(i+1)
         aij(i) = aij(i+1)
   10 continue
      irow(naij+1) = 0
      jcol(naij+1) = 0
      aij(naij+1) = 0.0
      return
      end
