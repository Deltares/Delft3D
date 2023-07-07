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
      module m_dhdelf

      implicit none

      contains


      SUBROUTINE DHDELF ( FILNAM, IERROR )
!
!     Deltares     SECTOR WATERRESOURCES AND ENVIRONMENT
!
!     CREATED: aug   1993 by Jan van Beek
!
!     FUNCTION            : deletes a file by name
!
!     PARAMETERS          :
!
!     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
!     ----    -----    ------     ------- -----------
!     FILNAM  CHAR*(*)      1     INPUT   file to be deleted
!     IERROR  INTEGER       1     OUTPUT  Error indication
!
      CHARACTER*(*) FILNAM
      INTEGER       IERROR
!
!     Local
!
      INTEGER IOLUN, ILUN
      LOGICAL LOPEN, LEXIST
!
!     Init
!
      IERROR = 0
      IOLUN  = 0
!
!     If file exist
!
      INQUIRE ( FILE=FILNAM, EXIST = LEXIST )
      IF ( .NOT. LEXIST ) RETURN
!
!     Select availeble ubitnumber
!
      DO 100 ILUN = 10, 99
         INQUIRE ( UNIT=ILUN, OPENED = LOPEN )
         IF ( .NOT. LOPEN ) THEN
            IOLUN = ILUN
            GOTO 101
         ENDIF
  100 CONTINUE
  101 CONTINUE
!
!     Open and close file
!
      IF ( IOLUN .NE. 0 ) THEN
         OPEN  ( IOLUN, FILE = FILNAM  , ERR = 900 )
         CLOSE ( IOLUN, STATUS='DELETE', ERR = 900 )
      ELSE
         IERROR = 1
      ENDIF
!
      RETURN
!
!     Errors close anyway
!
  900 CONTINUE
      CLOSE (IOLUN)
      IERROR = 1
      RETURN
      END
      end module m_dhdelf
