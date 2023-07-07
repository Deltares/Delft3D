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
      module m_reptim

      implicit none

      contains


      SUBROUTINE REPTIM ( LUN , ITIME, ISFLAG, PERCIT )
!
!     Deltares        SECTOR WATERRESOURCES AND ENVIRONMENT
!
!     CREATED       : August 1993 by Jan van Beek
!
!     FUNCTION      : Write time to unit according ISFLAG fromat
!
!     LOGICAL UNITS : LUN
!
!     PARAMETERS    :
!
!     NAME    KIND     LENGTH      FUNCT.  DESCRIPTION
!     ---------------------------------------------------------
!     LUN     INTEGER  1           INPUT   Unit number output
!     ITIME   INTEGER  1           INPUT   Time in system clock units
!     ISFLAG  INTEGER  1           INPUT   Output format indicator
!                                          0 = integer format
!                                          1 = dd:hh:mm:ss
!                                          2 = yy:ddd:hh:mm:ss
!     PERCIT  REAL     1           INPUT   IF >= 0 then percentage to be printed
!
!     Declaration of arguments
!
      INTEGER LUN   , ITIME , ISFLAG
      REAL    PERCIT
!
      IF ( ISFLAG .EQ. 0 ) THEN
         IF ( PERCIT .GE. 0.0 ) THEN
            WRITE (LUN,2030) ITIME,PERCIT
         ELSE
            WRITE (LUN,2000) ITIME
         ENDIF
      ELSEIF ( ISFLAG .EQ. 1 ) THEN
         IF ( ITIME .LT. 31536000 ) THEN
            IF ( PERCIT .GE. 0.0 ) THEN
               WRITE (LUN,2040)     ITIME/86400           ,
     *                          MOD(ITIME,86400)/3600     ,
     *                          MOD(ITIME,3600)/60        ,
     *                          MOD(ITIME,60)             ,
     *                          PERCIT
            ELSE
               WRITE (LUN,2010)     ITIME/86400           ,
     *                          MOD(ITIME,86400)/3600     ,
     *                          MOD(ITIME,3600)/60        ,
     *                          MOD(ITIME,60)
            ENDIF
         ELSE
            IF ( PERCIT .GE. 0.0 ) THEN
               WRITE (LUN,2050)     ITIME/31536000        ,
     *                          MOD(ITIME,31536000)/86400 ,
     *                          MOD(ITIME,86400)/3600     ,
     *                          MOD(ITIME,3600)/60        ,
     *                          MOD(ITIME,60)             ,
     *                          PERCIT
            ELSE
               WRITE (LUN,2020)     ITIME/31536000        ,
     *                          MOD(ITIME,31536000)/86400 ,
     *                          MOD(ITIME,86400)/3600     ,
     *                          MOD(ITIME,3600)/60        ,
     *                          MOD(ITIME,60)
            ENDIF
         ENDIF
      ELSEIF ( ISFLAG .EQ. 2 ) THEN
         IF ( PERCIT .GE. 0.0 ) THEN
            WRITE (LUN,2050)     ITIME/31536000        ,
     *                       MOD(ITIME,31536000)/86400 ,
     *                       MOD(ITIME,86400)/3600     ,
     *                       MOD(ITIME,3600)/60        ,
     *                       MOD(ITIME,60)             ,
     *                       PERCIT
         ELSE
            WRITE (LUN,2020)     ITIME/31536000        ,
     *                       MOD(ITIME,31536000)/86400 ,
     *                       MOD(ITIME,86400)/3600     ,
     *                       MOD(ITIME,3600)/60        ,
     *                       MOD(ITIME,60)
         ENDIF
      ENDIF
!
      RETURN
 2000 FORMAT ('  TIME = ',I12,' .')
 2010 FORMAT ('  TIME = ',I3,'D ',I2,'H ',I2,'M ',I2,'S .')
 2020 FORMAT ('  TIME = ',I2,'Y ',I3,'D ',I2,'H ',I2,'M ',I2,'S .')
 2030 FORMAT ('  TIME = ',I12,' . ',F6.2,'% Completed')
 2040 FORMAT ('  TIME = ',I3,'D ',I2,'H ',I2,'M ',I2,'S . ',F6.2,'% Completed')
 2050 FORMAT ('  TIME = ',I2,'Y ',I3,'D ',I2,'H ',I2,'M ',I2,'S . ',F6.2,'% Completed')
      END
      end module m_reptim
