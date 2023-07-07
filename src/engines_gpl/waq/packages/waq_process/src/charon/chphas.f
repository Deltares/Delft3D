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

!
!     Deltares     SECTOR WATERRESOURCES AND ENVIRONMENT
!
!     CREATED:            :
!
!     V0.01  040894  Jos van Gils  First version
!
!     MODULE              : CHPHAS
!
!     FUNCTION            : Find index of last water component
!                           in Charon-arrays with dimension NAIJ
!
!     SUBROUTINES CALLED  :
!
!     FILES               : -
!
!     COMMON BLOCKS       : -
!
!     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
!     ----    -----    ------     ------- -----------
!
      SUBROUTINE CHPHAS ( NAIJ2 )
      
      use m_charon
!
!     Declarations
!
      INTEGER         NAIJ2 , IFPS , J

!     IFPS: number of first non water transportable phase

      IFPS  = 2
      NAIJ2 = -1
      DO 20 J = 1,NAIJ
          IF ( JCOL(J) .EQ. KL(IFPS) ) THEN
              NAIJ2 = J - 1
              GOTO 25
          ENDIF
   20 CONTINUE
   25 CONTINUE
      IF ( NAIJ2 .LT. 0 ) NAIJ2 = NAIJ

      RETURN
      END
