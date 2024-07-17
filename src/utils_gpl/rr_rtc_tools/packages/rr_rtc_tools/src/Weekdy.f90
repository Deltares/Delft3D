! Last changed 
! by:               $Author:: Schrier           $
! at:               $Modtime:: 22-07-97 11:53a  $
!
! current revision: $Revision:: 3               $


      INTEGER FUNCTION WEEKDY ( IYEAR , IMONTH, IDAY  )
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
!        This functions returns the day of week of a date. It uses
!        Zeller's congruence.
!
!        The day of week of a date is a number from 0 to 6,
!        0 corresponding to Sunday, 1 to Monday ... and 6 to Saturday.
!
!***********************************************************************
!
!     Arguments :
!
!     Name   Type     In/Out Size            Description
!     ------ -----    ------ -------         ---------------------------
!     IYEAR  integer  in     -               Year   ( 0-.. )
!     IMONTH integer  in     -               Month  ( 1-12 )
!     IDAY   integer  in     -               Day    ( 1-28,29,30 or 31 )
!
!     Local variables :
!
!     Name   Type     Size   Description
!     ------ -----    ------ ------------------------
!     CENTUR integer  -      Century
!     IMONT2 integer  -      Month
!     IYEAR2 integer  -      Year
!     YEAR   integer  -      Year
!     TEMP   integer  -      Temporary var.
!     MONLEN integer  12     Length in day of month
!
!     Calls to : none
!
!***********************************************************************
!
!     Variables :
!
      INTEGER          IYEAR, IYEAR2, IMONTH, IMONT2, IDAY, CENTUR
      Integer          YEAR, TEMP, MONLEN(12)
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
      IF (( IYEAR  .LT.   0 ) .OR. ( IMONTH .LT.  1 ) .OR.    &
         ( IMONTH .GT.  12 ) .OR. ( IDAY   .LT.  1 ) .OR.    &
         ( IDAY   .GT. MONLEN(IMONTH) )) THEN
         WEEKDY = -1
         GOTO 999
      ELSE
         IYEAR2 = IYEAR
         IMONT2 = IMONTH - 2
         IF ( ( IMONT2 .LT. 1 ) .OR. ( IMONT2 .GT. 10 )) THEN
            IMONT2 = IMONT2 + 12
            IYEAR2 = IYEAR2 -  1
         ENDIF
         CENTUR = IYEAR2 / 100
         YEAR   = MOD ( IYEAR2, 100 )
         TEMP   = MOD ( INT ( 2.6 * IMONT2 - 0.1999 ) + IDAY + YEAR +  &
                        INT ( YEAR / 4 ) + INT ( CENTUR / 4 ) - CENTUR -   &
                        CENTUR , 7 )
         IF ( TEMP .LT. 0 ) TEMP = TEMP + 7
         WEEKDY = TEMP
      ENDIF
  999 RETURN
      END
