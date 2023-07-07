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
      module m_dhswtr

      implicit none

      contains


      SUBROUTINE DHSWTR ( ISWITR , NOQ3   , IOK    )
!
!     Deltares     SECTOR WATERRESOURCES AND ENVIRONMENT
!
!     CREATED             : december 1994 by Jan van Beek
!
!     FUNCTION            : utility that evaluates the TRswitch
!                           for the target model dimension
!
!     PARAMETERS          :
!
!     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
!     ----    -----    ------     ------- -----------
!     ISWITR  INTEGER       1     INPUT   Target dimension indicator
!     NOQ3    INTEGER       1     INPUT   Number of exchanges in 3 dir.
!     IOK     LOGICAL       1     OUTPUT  dimension match indicator
!
      INTEGER ISWITR, NOQ3
      LOGICAL IOK
!
!     Local
!
      IOK = .TRUE.
      IF ( ISWITR .EQ. 3 .AND. NOQ3   .EQ. 0 ) THEN
         IOK = .FALSE.
      ENDIF
      IF ( ISWITR .EQ. 12 .AND. NOQ3   .GT. 0 ) THEN
         IOK = .FALSE.
      ENDIF
!
      RETURN
      END
      end module m_dhswtr
