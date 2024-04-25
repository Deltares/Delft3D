!!  Copyright (C)  Stichting Deltares, 2012-2024.
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
      module m_get_filepath_and_pathlen

      implicit none

      contains


      SUBROUTINE get_filepath_and_pathlen ( FILNAM, FILPATH, PATHLEN)
!
!
!     Deltares
!
!     FUNCTION      : get file path and path length including last separator
!
!     SUBROUTINE CALLED  : none
!
!     LOGICAL UNITS      : none
!
!     PARAMETERS         :
!
!     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
!     ----    -----    ------     ------- -----------
!     FILNAM  CHAR*(*) 1          I       filename
!     FILPATH CHAR*(*) 1          O       file path
!     PATHLEN INT      1          O       path length
!
      IMPLICIT NONE
!
!     Declaration of arguments
!
      INTEGER       PATHLEN
      CHARACTER*(*) FILNAM, FILPATH
!
!     Local declaration
!
      INTEGER       LENNAM, ICH
      CHARACTER     DIRSEP_DOS, DIRSEP_UX
!
      DIRSEP_DOS = CHAR(92)
      DIRSEP_UX  = CHAR(47)
!
!     blank name get out of here
!
      FILPATH = ' '
      PATHLEN = 0
      IF ( FILNAM .EQ. ' ' ) RETURN
!
      LENNAM = LEN(FILNAM)
!
!     get last directory seperator
!
      DO ICH = LENNAM , 1 , -1
         IF ( FILNAM(ICH:ICH) .EQ. DIRSEP_DOS .OR. FILNAM(ICH:ICH) .EQ. DIRSEP_UX       ) THEN
            PATHLEN = ICH
            EXIT
         ENDIF
      ENDDO
!
      IF ( PATHLEN .GT. 0 ) THEN
         FILPATH = FILNAM(1:PATHLEN)
      ENDIF
!
      RETURN
      END
      end module m_get_filepath_and_pathlen
