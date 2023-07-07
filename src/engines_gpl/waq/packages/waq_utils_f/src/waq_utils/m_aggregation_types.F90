!!  Copyright (C)  Stichting Deltares, 2022-2023.
!!
!!  This program is free software: you can redistribute it and/or modify
!!  it under the terms of the GNU General Public License version 3,
!!  as published by the Free Software Foundation.
!!
!!  This program is distributed in the hope that it will be useful,
!!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
!!  GNU General Public License for more details.
!!
!!  You should have received a copy of the GNU General Public License
!!  along with this program. If not, see <http://www.gnu.org/licenses/>.
!!
!!  contact: delft3d.support@deltares.nl
!!  Stichting Deltares
!!  P.O. Box 177
!!  2600 MH Delft, The Netherlands
!!
!!  All indications and logos of, and references to registered trademarks
!!  of Stichting Deltares remain the property of Stichting Deltares. All
!!  rights reserved.

module m_aggregation_types



      integer, parameter  :: IAGTYP_ACCUM = 1 ! aggregation using accumulation
      integer, parameter  :: IAGTYP_AVG   = 2 ! aggregation using averaging
      integer, parameter  :: IAGTYP_WAVG  = 3 ! aggregation using averaging with a weight variable
      integer, parameter  :: IAGTYP_MIN   = 4 ! aggregation using minimum value
      integer, parameter  :: IAGTYP_ACSGN = 5 ! aggregation using a signed accumulation (for combining flows in opposite directions)


end module m_aggregation_types
