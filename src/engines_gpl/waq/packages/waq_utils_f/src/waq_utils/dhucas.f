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
      module m_dhucas

      implicit none

      contains


      SUBROUTINE DHUCAS( STR1 , STR2 , NOCHR )
!
!
!     Deltares        SECTOR WATERRESOURCES AND ENVIRONMENT
!
!     CREATED       : aug  1993  by Jan van Beek (DELWAQ style, origin?)
!
!     FUNCTION      : Subroutine to set a string in uppercase.
!
!     NAME    KIND     LENGTH      FUNCT.  DESCRIPTION
!     ---------------------------------------------------------
!     STR1    CHAR(*)   1         INPUT   string to be converted
!     STR2    CHAR(*)   1         OUTPUT  string in uppercase
!     NOCHR   INTEGER   1         INPUT   length string
!
!     Declaration of arguments
!
      INTEGER       NOCHR , IC , NN
      CHARACTER*(*) STR1  , STR2
!
!     Local declaration
!
      CHARACTER*1   CC
      CHARACTER*26  ALFABH, ALFABK
      SAVE          ALFABH, ALFABK
      DATA          ALFABH, ALFABK /
     &              'ABCDEFGHIJKLMNOPQRSTUVWXYZ',
     &              'abcdefghijklmnopqrstuvwxyz'/
!
      STR2 = STR1
!
      DO 100 IC = 1,NOCHR
         CC = STR2(IC:IC)
         NN = INDEX( ALFABK , CC )
         IF ( NN .EQ. 0 ) GOTO 100
         STR2(IC:IC) = ALFABH(NN:NN)
  100 CONTINUE
!
      RETURN
      END
      end module m_dhucas
