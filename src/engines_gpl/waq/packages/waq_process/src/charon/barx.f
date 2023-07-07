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

      subroutine barx(w, wbar)
!           4-69
!           for each compartment k compute wbar(k) as the sum of w(j),
!        where j is each substance in the compartment
      use m_char1
      use m_char5
      
      dimension w(1), wbar(1)
      do 20 k=1,ncomp
         kta = kl(k)
         ktb = kl(k+1) - 1
         wbar(k) = slacks
         if(bslack) wbar(k) = slack(k)
         do 10 j=kta,ktb
            wbar(k) = wbar(k) + w(j)
   10    continue
   20 continue
      return
      end
