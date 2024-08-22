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

 subroutine dumpnetlink(tex, L)
    use m_netw
    use m_flowgeom
    use m_flow
    use unstruc_messages
    implicit none
    integer :: L, k1, k2
    character(len=*) tex
    character(len=14) tex2

    write (tex2, '(i14.0)') L
    k1 = kn(1, L); k2 = kn(2, L)
    call mess(LEVEL_INFO, trim(tex), trim(tex2), ' ')
    call mess(LEVEL_INFO, ' 2  2 ', ' ', ' ')
    call mess(LEVEL_INFO, xk(k1), yk(k1))
    call mess(LEVEL_INFO, xk(k2), yk(k2))
 end subroutine dumpnetlink
