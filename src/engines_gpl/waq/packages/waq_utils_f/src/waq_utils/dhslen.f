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
      module m_dhslen

      implicit none

      contains


      SUBROUTINE DHSLEN ( STRING, ILEN  )
!
!     Deltares
!
!     Created             : November 1998 by Jan van Beek
!
!     Function            : Retrun length of trimmed string
!                           Always >= 1
!
!     Parameters          :
!
!     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
!     ----    -----    ------     ------- -----------
!     STRING  CHAR*(*) 1          IN      String
!     ILEN    INTEGER  1          OUT     Length of trimmed string
!
!     Declaration of arguments
!
      INTEGER       ILEN
      CHARACTER*(*) STRING
!
!     Local declaration
!
!     ISLEN   INTEGER  1          LOCAL   length of variable
!
      INTEGER    ISLEN, I
!
      ISLEN = LEN(STRING)
      ILEN  = 1
      DO I = 1 , ISLEN
         IF ( STRING(I:I) .NE. ' ' ) ILEN = I
      ENDDO
!
      RETURN
      END
      end module m_dhslen
