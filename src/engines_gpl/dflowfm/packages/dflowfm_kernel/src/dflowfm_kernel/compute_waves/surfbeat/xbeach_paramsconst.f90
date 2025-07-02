!----- AGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2017-2025.
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

module m_xbeach_paramsconst
   integer, parameter :: TURB_NONE = 0
   integer, parameter :: TURB_BORE_AVERAGED = 1
   integer, parameter :: TURB_WAVE_AVERAGED = 2

   integer, parameter :: WAVEFORM_RUESSINK_VANRIJN = 0
   integer, parameter :: WAVEFORM_VANTHIEL = 1

   integer, parameter :: TURBADV_NONE = 0
   integer, parameter :: TURBADV_LAGRANGIAN = 1
   integer, parameter :: TURBADV_EULERIAN = 2

   integer, parameter :: INSTAT_STAT = 0
   integer, parameter :: INSTAT_BICHROM = 1
   integer, parameter :: INSTAT_TS_1 = 2
   integer, parameter :: INSTAT_TS_2 = 3
   integer, parameter :: INSTAT_JONS = 4
   integer, parameter :: INSTAT_SWAN = 5
   integer, parameter :: INSTAT_VARDENS = 6
   integer, parameter :: INSTAT_REUSE = 7
   integer, parameter :: INSTAT_TS_NONH = 8
   integer, parameter :: INSTAT_OFF = 9
   integer, parameter :: INSTAT_STAT_TABLE = 10
   integer, parameter :: INSTAT_JONS_TABLE = 11

end module m_xbeach_paramsconst
