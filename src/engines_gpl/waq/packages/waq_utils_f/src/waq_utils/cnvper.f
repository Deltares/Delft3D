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
      module m_cnvper

      implicit none

      contains


      SUBROUTINE CNVPER ( CHULP  , IHULP  , DTFLG1 , DTFLG3 , IERR   )
!
!
!     Deltares        SECTOR MARINE AND COASTAL MANAGEMENT
!
!     CREATED            : August '02 by Jan van Beek
!
!     MODIFIED           :
!
!     FUNCTION           : Detects standard timer string period
!                          converts to system timer
!
!     SUBROUTINES CALLED : none
!
!     LOGICAL UNITS      : none
!
!     PARAMETERS    :
!
!     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
!     ---------------------------------------------------------
!     CHULP   CHAR*(*)   1         INPUT   string to be analised
!     IHULP   INTEGER    1         OUTPUT  system timer to get out
!     DTFLG1  LOGICAL    1         INPUT   TRUE if date format
!     DTFLG3  LOGICAL    1         INPUT   TRUE if HH instead of SS
!     IERR    INTEGER    1         OUTPUT  = 1 string is no timer
!
!     IN THE COMMON BLOCK:
!
!     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
!     ---------------------------------------------------------
!     block sysi.inc
!     ITSTRT  INTEGER    1         INPUT   Simulation start time ( scu )
!     ITSTOP  INTEGER    1         INPUT   Simulation stop time ( scu )
!     ISFACT  INTEGER    1         INPUT   system clock in seconds
!     OTIME   REAL*8     1         INPUT   Julian offset of the real time
!
!
      USE DLWQ0T_DATA

      IMPLICIT NONE

      CHARACTER*(*) CHULP
      REAL*8        OTIM2  , OTIM3  , AFACT
      LOGICAL       DTFLG1 , DTFLG3
      CHARACTER*20  KEY
      INTEGER       IERR, IYEAR, IMONTH, IDAY, IHOUR, IMINUT, ISECND, ISEC, IHULP

      IERR = 1
      IF ( CHULP( 5: 5) .NE. '/' .OR. CHULP( 8: 8) .NE. '/' .OR.
     *     CHULP(11:11) .NE. '-' .OR. CHULP(14:14) .NE. ':' .OR.
     *     CHULP(17:17) .NE. ':' ) RETURN
      READ ( CHULP( 1: 4) , '(I4)' ) IYEAR
      READ ( CHULP( 6: 7) , '(I2)' ) IMONTH
      READ ( CHULP( 9:10) , '(I2)' ) IDAY
      READ ( CHULP(12:13) , '(I2)' ) IHOUR
      READ ( CHULP(15:16) , '(I2)' ) IMINUT
      READ ( CHULP(18:19) , '(I2)' ) ISECND
!
      ISEC   = IYEAR*31536000 + IMONTH*2592000+IDAY*86400+
     +         IHOUR*3600+IMINUT*60+ISECND
      IF ( DLWQ0T_ISFACT .LT. 0 ) THEN
         IHULP = -ISEC*DLWQ0T_ISFACT
      ELSE
         IHULP = ISEC/DLWQ0T_ISFACT
      ENDIF
!
      IF ( DTFLG3 ) THEN
         IHULP =  IHULP/3600
         IHULP = (IHULP/8760)*100000 + (MOD(IHULP,8760)/24)*100
     *                               +  MOD(IHULP,24)
      ELSE IF ( DTFLG1 ) THEN
         IHULP = (IHULP/86400)*1000000 + (MOD(IHULP,86400)/3600)*10000
     *         + (MOD(IHULP,3600)/60)*100 + MOD(IHULP,60)
      ENDIF
      IERR = 0
      RETURN
      END
      end module m_cnvper
