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

      subroutine push(ij)
!           4-69
!           move matrix data in arrays aij, irow, and jcol down by one
!        position.  move entries starting with position 'ij' in the
!        array.
!           push uses subprogram
!              leave
      use m_char1
      if (ij.gt.naij) go to 20
      i = naij
      if (i.ge.maxaij) go to 40
      do 10 j=ij,naij
         irow(i+1) = irow(i)
         jcol(i+1) = jcol(i)
         aij(i+1) = aij(i)
         i = i - 1
   10 continue
   20 naij = naij + 1
   30 return
   40 write (not,99999) maxaij
      call leave(ipush, not)
      go to 30
99999 format ('0the number of matrix entries exceeds the limit of',i7,
     1 /1h )
      end
