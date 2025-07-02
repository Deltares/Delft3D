!!  Copyright (C)  Stichting Deltares, 2012-2025.
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

      SUBROUTINE delete_file ( FILNAM, IERROR )
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
      CHARACTER(len=*) FILNAM
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
!     Open and close file
!
      OPEN  ( NEWUNIT = IOLUN, FILE = FILNAM  , ERR = 900 )
      CLOSE ( IOLUN, STATUS='DELETE', ERR = 900 )
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
