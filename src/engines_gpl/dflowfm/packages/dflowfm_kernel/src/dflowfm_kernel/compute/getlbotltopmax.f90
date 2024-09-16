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
module m_get_Lbot_Ltop_max
    implicit none
contains
 subroutine getLbotLtopmax(LL, Lb, Ltx)
! Variation on getLbotLtop. Always returns the maximum possible layer range in stead of the actual range, even for dry links.
    use m_flow, only: kmx, kmxl, lbot

    integer :: LL, Lb, Ltx
    if (kmx == 0) then
       Lb = LL
       Ltx = LL
    else
       Lb = Lbot(LL); Ltx = Lbot(LL) + kmxL(LL) - 1
    end if
 end subroutine getLbotLtopmax
end module m_get_Lbot_Ltop_max
