!----- AGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2017-2026.
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
module m_smeerfunctie
   implicit none
contains

   subroutine SMEERFUNCTIE(I, J, MP, NP, FR, IN, JN)
      use precision, only: dp
      use m_grid_block, only: mb, nb

      integer :: i, j, mp, np, in, jn
      real(kind=dp) :: fr
      real(kind=dp) :: pi, phi, fri, frj
      PI = acos(-1.0_dp)

      if (I == MP) then
         PHI = 0
      else if (I > MP .and. I < MB(4)) then
         PHI = PI * real(I - MP, kind=dp) / real(MB(4) - MP, kind=dp)
      else if (I < MP .and. I > MB(3)) then
         PHI = PI * real(MP - I, kind=dp) / real(MP - MB(3), kind=dp)
      else
         PHI = PI
      end if
      FRI = (1 + cos(PHI)) / 2

      if (J == NP) then
         PHI = 0
      else if (J > NP .and. J < NB(4)) then
         PHI = PI * real(J - NP, kind=dp) / real(NB(4) - NP, kind=dp)
      else if (J < NP .and. J > NB(3)) then
         PHI = PI * real(NP - J, kind=dp) / real(NP - NB(3), kind=dp)
      else
         PHI = PI
      end if
      FRJ = (1 + cos(PHI)) / 2

      if (IN == 1 .and. JN == 1) then
         FR = sqrt(FRI * FRJ)
      else if (JN == 1) then
         FR = FRJ
      else if (IN == 1) then
         FR = FRI
      end if

      return
   end subroutine smeerfunctie

end module m_smeerfunctie
