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

! =================================================================================================
! =================================================================================================
module m_equili_spiralintensity

   implicit none

contains

   subroutine equili_spiralintensity()
      use precision, only: dp
      use m_flow
      use m_flowgeom
      use m_sferic, only: jsferic, fcorio
      implicit none
      integer :: kk
      real(kind=dp) :: spir_ce, spir_be, fcoriocof

      do kk = 1, ndx
         fcoriocof = fcorio
         if (icorio > 0 .and. jsferic == 1) fcoriocof = fcoris(kk)
         spir_ce = fcorio * hs(kk) * 0.5d0
         spir_be = hs(kk) * spircrv(kk) * spirucm(kk)
         spirint(kk) = spir_be - spir_ce
      end do

   end subroutine equili_spiralintensity

end module m_equili_spiralintensity
