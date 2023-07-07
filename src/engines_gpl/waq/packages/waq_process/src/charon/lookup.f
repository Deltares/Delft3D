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

      subroutine lookup(ka, k, l, ll, kb)
!           4-69
!
!           look in the array kb(k) from k equal l to ll for a match
!        with the word in ka.  if ka is in the array exit with the
!        value in k.  if ka is not in the array set k equal to ll+1.
!
      character*6 ka, kb(1)
      do 10 i = l,ll
         if (ka.eq.kb(i)) go to 20
10    continue
      i = ll+1
20    k = i
      return
      end
