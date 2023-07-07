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

      subroutine del(w, q)
!           4-69
!           compute for each species j the sum of the products of each
!        matrix entry for the species times q(i), where i is the row
!        number.
      use m_char1
      dimension w(1), q(1)
      do 10 j=1,n
         w(j) = 0.0
   10 continue
      do 20 ij=1,naij
         j = jcol(ij)
         i = irow(ij)
         w(j) = w(j) + aij(ij)*q(i)
   20 continue
      return
      end
