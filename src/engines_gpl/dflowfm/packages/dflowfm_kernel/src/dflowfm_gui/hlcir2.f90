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
module m_hlcir2
   use m_cir

   implicit none
contains
   !> Draw a filled circle at current position.
      !! Filled means: one colour for inside, one colour for edge.
   subroutine HLCIR2(R, icolfill, icoledge)
      use precision, only: dp
      use m_set_col

      real(kind=dp), intent(in) :: R !< Radius in world coords.
      integer, intent(in) :: icolfill !< Colour number for inner fill
      integer, intent(in) :: icoledge !< Colour number for edge

      call IGRFILLPATTERN(4, 0, 0)
      call SETCOL(icolfill)
      call CIR(R)
      call IGRFILLPATTERN(0, 0, 0)
      call SETCOL(icoledge)
      call CIR(R)
      call IGRFILLPATTERN(4, 0, 0)
   end subroutine HLCIR2
end module m_hlcir2
