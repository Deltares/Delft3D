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

module m_wrixyz

   implicit none

   private

   public :: WRIXYZ

contains

   subroutine WRIXYZ(FILNAM, XS, YS, ZS, NS)
      use precision, only: dp
      implicit none
      character(LEN=*) :: FILNAM
      integer :: NS
      real(kind=dp) :: XS(NS), YS(NS), ZS(NS)
      integer :: I, MOUT

      call NEWFIL(MOUT, FILNAM)
      do I = 1, NS
         write (MOUT, '(3(F16.7))') XS(I), YS(I), ZS(I)
      end do
      call DOCLOSE(MOUT)
   end subroutine WRIXYZ

end module m_wrixyz
