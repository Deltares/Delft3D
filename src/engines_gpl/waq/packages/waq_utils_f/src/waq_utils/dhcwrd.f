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
      module m_dhcwrd

      implicit none

      contains


      INTEGER FUNCTION DHCWRD ( LINE  )
!
!
!     Deltares        SECTOR WATERRESOURCES AND ENVIRONMENT
!
!     CREATED       : june  1993 BY J.K.L. van Beek
!
!     FUNCTION      : Counts number of words, blank delimitted
!
!     SUBROUTINE CALLED  : none
!
!     LOGICAL UNITS      : none
!
!     PARAMETERS         : none
!
!
!     Local declaration
!
      CHARACTER*(*) LINE
      integer :: IA
      integer :: LENLIN
      integer :: ICH
!
      IA = 0
      LENLIN = LEN(LINE)
!
!     Count the number of first blanks in a row
!
      DO 100 ICH = 1 , LENLIN
         IF ( LINE(ICH:ICH)     .NE. ' ' .AND.
     +        LINE(ICH+1:ICH+1) .EQ. ' '       ) THEN
            IA = IA + 1
         ENDIF
  100 CONTINUE
      IF ( LINE(LENLIN:LENLIN) .NE. ' ' ) THEN
         IA = IA + 1
      ENDIF
!
      DHCWRD = IA
!
      RETURN
      END
      end module m_dhcwrd
