!!  Copyright (C)  Stichting Deltares, 2012-2023.
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
      module m_dhrmis

      implicit none

      contains


      LOGICAL FUNCTION DHRMIS (VALUE)
!
!
!     Deltares        SECTOR WATERRESOURCES AND ENVIRONMENT
!
!     CREATED       : june  1993 BY J.K.L. van Beek
!
!     FUNCTION      : Checks if a real value is "missing"
!
!     LOGICAL UNITS      : none
!
!     PARAMETERS         : 1
!
!     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
!     ----    -----    ------     ------- -----------
!     VALUE   REAL        1       INPUT   Value to be checked
!
      REAL    VALUE
      integer RMISS
!
!     Local declaration
!
      PARAMETER ( RMISS = -999. )
!
!     Check
!
      IF ( VALUE .EQ. RMISS ) THEN
         DHRMIS = .TRUE.
      ELSE
         DHRMIS = .FALSE.
      ENDIF
!
      RETURN
      END
      end module m_dhrmis
