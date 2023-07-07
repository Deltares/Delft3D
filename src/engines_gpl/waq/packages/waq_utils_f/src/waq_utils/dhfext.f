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
      module m_dhfext

      implicit none

      contains


      SUBROUTINE DHFEXT ( FILNAM, FILEXT, EXTPOS, EXTLEN)
!
!
!     Deltares
!
!     CREATED       : june  2002 BY J.K.L. van Beek
!
!     FUNCTION      : get file extension and extension dot position an extension length without dot
!                     if no extension position is last blank
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
!     FILEXT  CHAR*(*) 1          O       file extension
!     EXTPOS  INT      1          O       postion of extension dot, if no extension position is last blank
!     EXTLEN  INT      1          O       extension length without dot
!
      IMPLICIT NONE
!
!     Declaration of arguments
!
      INTEGER       EXTPOS, EXTLEN
      CHARACTER*(*) FILNAM, FILEXT
!
!     Local declaration
!
      INTEGER       LENNAM, ICH_LAST, ICH
      CHARACTER     DIRSEP_DOS, DIRSEP_UX
!
      DIRSEP_DOS = CHAR(92)
      DIRSEP_UX  = CHAR(47)
!
!     blank name get out of here
!
      FILEXT = ' '
      EXTPOS = 1
      EXTLEN = 0
      IF ( FILNAM .EQ. ' ' ) RETURN
!
      LENNAM = LEN(FILNAM)
!
!     get last non blank, last point after directory seperator
!
      EXTPOS = 0
      ICH_LAST  = 0
      DO ICH = LENNAM , 1 , -1
         IF ( ICH_LAST .EQ. 0 .AND. FILNAM(ICH:ICH) .NE. ' ' ) THEN
            ICH_LAST = ICH
         ENDIF
         IF ( FILNAM(ICH:ICH) .EQ. DIRSEP_DOS .OR.
     +        FILNAM(ICH:ICH) .EQ. DIRSEP_UX       ) EXIT
         IF ( FILNAM(ICH:ICH) .EQ. '.' ) THEN
            EXTPOS = ICH
            EXIT
         ENDIF
      ENDDO
!
      IF ( EXTPOS .GT. 0 ) THEN
         EXTLEN = ICH_LAST-EXTPOS
         IF ( EXTLEN .GT. 0 ) FILEXT = FILNAM(EXTPOS+1:ICH_LAST)
      ELSE
         EXTPOS = MIN(ICH_LAST+1,LENNAM)
      ENDIF
!
      RETURN
      END
      end module m_dhfext
