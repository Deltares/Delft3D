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
module m_fieldopt
   use m_orthogrid, only: orthogrid

   implicit none
contains
   subroutine FIELDOPT(NFLD)
      use M_GRID
      use m_menuv2
      use m_local_refine
      use m_fieldop

      integer :: nfld
      integer, parameter :: MAXOP = 64
      integer :: nwhat2, maxopt, i
      character(len=40) OPTION(MAXOP), exp(MAXOP)
      exp(1) = 'MENU 10                                 '
      exp(2) = 'GRID EDIT OPTIONS                       '
      MAXOPT = 22
      do I = 1, MAXOPT
         OPTION(I) = FIELDOP(I)
      end do
      NWHAT2 = NFLD
      call MENUV2(NWHAT2, OPTION, MAXOPT)
      if (NWHAT2 >= 1) then
         if (NWHAT2 == 19) then
            call ORTHOGRID(1, 1, MC, NC)
         else if (NWHAT2 == 20) then
            call LOCALREFINE(Nwhat2, 1, 1, mc, nc, 1)
         else if (NWHAT2 == 21) then
            call LOCALREFINE(Nwhat2, 1, 1, mc, nc, 2)
         else
            NFLD = NWHAT2
         end if
      end if
      return
   end subroutine fieldopt
end module m_fieldopt
