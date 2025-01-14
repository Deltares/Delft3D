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
module m_makesr
   implicit none
contains

   subroutine MAKESR(AR, S0, S1, SR, MFAC)
      use precision, only: dp
      integer :: mfac
      real(kind=dp) :: ar, s0, s1
      real(kind=dp) :: SR(MFAC + 1)

      real(kind=dp) :: ds, fac
      integer :: k
      DS = 1
      SR(1) = 0
      do K = 1, MFAC
         SR(K + 1) = SR(K) + DS
         DS = DS * AR
      end do

      FAC = (S1 - S0) / SR(MFAC + 1)
      do K = 0, MFAC
         SR(K + 1) = S0 + FAC * SR(K + 1)
      end do
      return
   end subroutine makesr
end module m_makesr
