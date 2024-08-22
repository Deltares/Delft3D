!----- AGPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2024.                                
!                                                                               
!  This program is free software: you can redistribute it and/or modify         
!  it under the terms of the GNU Affero General Public License as               
!  published by the Free Software Foundation version 3.                         
!                                                                               
!  This program is distributed in the hope that it will be useful,              
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                
!  GNU Affero General Public License for more details.                          
!                                                                               
!  You should have received a copy of the GNU Affero General Public License     
!  along with this program.  If not, see <http://www.gnu.org/licenses/>.        
!                                                                               
!  contact: delft3d.support@deltares.nl                                         
!  Stichting Deltares                                                           
!  P.O. Box 177                                                                 
!  2600 MH Delft, The Netherlands                                               
!                                                                               
!  All indications and logos of, and references to, "Delft3D" and "Deltares"    
!  are registered trademarks of Stichting Deltares, and remain the property of  
!  Stichting Deltares. All rights reserved.                                     
!                                                                               
!-------------------------------------------------------------------------------

      DOUBLE PRECISION FUNCTION JULIAN ( IDATE , ITIME )
!***********************************************************************
!  August 2000: change C in column 1 to a ! in column 1 (for McCabe)
!***********************************************************************
!
!     +----------------------------------------------------------------+
!     |    W A T E R L O O P K U N D I G   L A B O R A T O R I U M     |
!     |               Sector Waterbeheer & Milieu                      |
!     +----------------------------------------------------------------+
!
!***********************************************************************
!
!     Project : T0467
!     Author  : Andre Hendriks
!     Date    : 891215             Version : 1.00
!
!     Changes in this module :
!
!     Date    Author          Description
!     ------  --------------  -----------------------------------
!     ......  ..............  ..............................
!     891215  Andre Hendriks  Version 1.00
!
!***********************************************************************
!
!     Description of module :
!
!        This functions returns the so called Julian day of a date, or
!        the value -1.0 if an error occurred.
!
!        The Julian day of a date is the number of days that has passed
!        since January 1, 4712 BC at 12h00 ( Gregorian). It is usefull
!        to compute differces between dates. ( See SUBROUTINE GREGOR
!        for the reverse proces ).
!
!***********************************************************************
!
!     Arguments :
!
!     Name   Type     In/Out Size            Description
!     ------ -----    ------ -------         ---------------------------
!     IDATE  integer  in     -               Date as YYYYMMDD
!     ITIME  integer  in     -               Time as HHMMSS
!
!     Local variables :
!
!     Name   Type     Size   Description
!     ------ -----    ------ ------------------------
!     TEMP1  real*8   -      Temporary variable
!     TEMP2  real*8   -      Temporary variable
!     IYEAR  integer  -      Year   ( -4713-.. )
!     IMONTH integer  -      Month  ( 1-12 )
!     IDAY   integer  -      Day    ( 1-28,29,30 or 31 )
!     IHOUR  integer  -      Hour   ( 0-23 )
!     IMIN   integer  -      Minute ( 0-59 )
!     ISEC   integer  -      Second ( 0-59 )
!     MONLEN integer  12     Length of month in days
!
!     Calls to : none
!
!***********************************************************************
!
!     Variables :
!
      IMPLICIT NONE !!!

      INTEGER          IYEAR, IMONTH, IDAY, IHOUR, IMIN, ISEC, IDATE, ITIME, MONLEN(12)
      DOUBLE PRECISION TEMP1, TEMP2
!
!***********************************************************************
!
!     Initialize lenghts of months :
!
      DATA MONLEN / 31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 /
!
!***********************************************************************
!
!
!
      IYEAR  = IDATE/10000
      IMONTH = IDATE/100 - IYEAR*100
      IDAY   = IDATE - IYEAR*10000 - IMONTH*100
      IHOUR  = ITIME/10000
      IMIN   = ITIME/100 - IHOUR*100
      ISEC   = ITIME - IHOUR*10000 - IMIN*100
      IF (( IYEAR  .LT. -4713 ) .OR. ( IMONTH .LT.  1 ) .OR. &
          ( IMONTH .GT.    12 ) .OR. ( IDAY   .LT.  1 ) .OR. &
          ( IHOUR  .LT.     0 ) .OR. ( IHOUR  .GT. 23 ) .OR. &
          ( IMIN   .LT.     0 ) .OR. ( IMIN   .GT. 59 ) .OR. &
          ( ISEC   .LT.     0 ) .OR. ( ISEC   .GT. 60 )) THEN
!!!GP original check was op 59 seconden, gaf afrondingsproblemen; daarom nu op 60 gezet
!!!  5    ( ISEC   .LT.     0 ) .OR. ( ISEC   .GT. 59 )) THEN
         JULIAN = -1.0
         GOTO 999
! GP 2008 Intel Fortran compiler
      ELSEIF (  IDAY   .GT. MONLEN(IMONTH) ) THEN
         JULIAN = -1.0
         GOTO 999
      ELSE
         TEMP1  = INT (( IMONTH-14.0) / 12.0 )
!-----------------------------
!        WRITE(*,*) 'TEMP1 : ', TEMP1
!        WRITE(*,*) 'dbg 1 : ',
!    1          INT ( 1461.0 * ( IYEAR + 4800.0 + TEMP1 ) / 4.0 )
!        WRITE(*,*) 'dbg 2 : ',
!    2          INT ( 367.0 * ( IMONTH - 2.0 - TEMP1 * 12.0 ) / 12.0 )
!        WRITE(*,*) 'dbg 3 : ',
!    1          INT ( 3.0 * INT ( ( IYEAR + 4900.0 + TEMP1 ) / 100.0 ) /
!    2          4.0 )
!-----------------------------
         TEMP2  = IDAY - 32075.0 + &
                INT ( 1461.0 * ( IYEAR + 4800.0 + TEMP1 ) / 4.0 ) + &
                INT ( 367.0 * ( IMONTH - 2.0 - TEMP1 * 12.0 ) / 12.0 ) - &
                INT ( 3.0 * INT ( ( IYEAR + 4900.0 + TEMP1 ) / 100.0 ) / &
                4.0 )
         TEMP1  = FLOAT ( IHOUR ) * 3600.0 + &
                  FLOAT ( IMIN  ) *   60.0 + &
                  FLOAT ( ISEC  ) - 43200.0
         JULIAN = TEMP2 + ( TEMP1 / 86400.0 )
      ENDIF
  999 RETURN
      END


      DOUBLE PRECISION FUNCTION Modified_Julian (IDATE , ITIME)
!
      IMPLICIT NONE
      INTEGER          IDATE, ITIME
      DOUBLE PRECISION JulianDate, Julian

      JulianDate = Julian (IDate,ITime)
      Modified_Julian = JulianDate - 2400000.5D0

      RETURN
      END



      DOUBLE PRECISION FUNCTION Modified_Julian_fromJulian (JulianDate)
      DOUBLE PRECISION JulianDate

      Modified_Julian_fromJulian = JulianDate - 2400000.5D0

      RETURN
      END
