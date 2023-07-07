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

      subroutine xlog(w, wbar)
!           4-69
!           compute c(j) + log(w(j)/wbar(k)), where j is a species in
!        compartment k.
!           clog uses double precision version of
!              alog
      use m_char1
      dimension w(1), wbar(1), alpha(1)
      do 20 k=1,ncomp
         kla = kl(k)
         klb = kl(k+1) - 1
         do 10 j=kla,klb
            alpha(j) = c(j)
            xxx = w(j)/wbar(k)
            if (xxx.gt.0.0) alpha(j) = c(j) + dlog(xxx)
   10    continue
   20 continue
      return
      end
