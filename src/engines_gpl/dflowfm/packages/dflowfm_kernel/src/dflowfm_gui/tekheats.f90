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

module m_tekheats

implicit none

contains

  subroutine TEKHEATS(TIMNOW)
     use m_heatfluxes
     use m_gtext
     implicit none
     double precision :: TIMNOW, tday

     TDAY = modulo(TIMNOW, 1440d0 * 60d0)
     call GTEXT('SUN', TDAY, QSunav, 221)
     call GTEXT('LWR', TDAY, QLongav, 221)
     call GTEXT('CON', TDAY, QEVAav, 221)
     call GTEXT('EVA', TDAY, QCONav, 221)
     call GTEXT('fre', TDAY, Qfreeav, 221)

     return
  end

end module m_tekheats
