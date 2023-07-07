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

      subroutine find(i, j, ijloc, iflag)
!           4-69
!           given  the row and column numbers find the location of the
!        number (ijloc) in the matrix array.  if there is no entry,
!        exit with iflag non zero and ijloc as the location into which
!        to insert the entry after the other values have been pushed
!        down.
!           if i=0, find the first and last locations for column j
!           ijloc will have the first location and iflag the last
!           the entries are stored in ascending order of j.  the order
!        of i within j is immaterial.
!           if j has no entries and i equals zero, set ijloc = 0
!           if j has no entries and i not equal zero, set ijloc equal
!        to current value.
!           in both cases set iflag equal to one
      use m_char1
      iflag = 0
      ij = 1
      if (j.lt.jcol(1)) go to 100
      if (j.gt.jcol(naij)) go to 110
      if (jcol(1).eq.j) go to 50
!        compute a guess for start of column, using average density
      ij = (naij/ntot)*(j-1)
      if (ij.gt.naij) ij = naij
!        find first entry for column j
   10 if (jcol(ij).lt.j) go to 20
      ij = ij - 1 - naij/ntot/2
      if (ij.le.0) ij = 1
      go to 10
   20 ijk = ij + 1
      do 30 ij=ijk,naij
         if (jcol(ij).ge.j) go to 40
   30 continue
      go to 110
   40 if (jcol(ij).gt.j) go to 120
   50 if (i.eq.0) ijloc = ij
!        look for row number in column storage
   60 if (jcol(ij).gt.j) go to 70
      if (irow(ij).eq.i) go to 80
      ij = ij + 1
      if (ij.le.naij) go to 60
   70 if (i.eq.0) go to 90
      iflag = 1
   80 ijloc = ij
      goto 999
   90 iflag = ij - 1
      goto 999
  100 ijloc = 1
      go to 130
  110 ijloc = naij + 1
      go to 130
  120 ijloc = ij
  130 iflag = 1
      if (i.eq.0) ijloc = 0
999   continue
      return
      end
