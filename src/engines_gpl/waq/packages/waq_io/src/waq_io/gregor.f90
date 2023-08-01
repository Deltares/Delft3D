module m_gregor

    implicit none

    contains

    SUBROUTINE GREGOR ( JULIAN, IYEAR , IMONTH, IDAY  , IHOUR , &
                        IMIN  , ISEC  , DSEC   )
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
!        This functions returns the Gregorian date and the time of a so
!        called Julian day, or iyear -9999 if an error occurred.
!
!        The Julian day of a date is the number of days that has passed
!        since January 1, 4712 BC at 12h00 ( Gregorian). It is usefull
!        to compute differces between dates. ( See DOUBLE PRECISION
!        FUNCTION JULIAN for the reverse proces ).
!
!***********************************************************************
!
!     Arguments :
!
!     Name   Type     In/Out Size            Description
!     ------ -----    ------ -------         ---------------------------
!     JULIAN real*8   in     -               Julian day
!     IYEAR  integer  out    -               Year   ( -4713-.. )
!     IMONTH integer  out    -               Month  ( 1-12 )
!     IDAY   integer  out    -               Day    ( 1-28,29,30 or 31 )
!     IHOUR  integer  out    -               Hour   ( 0-23 )
!     IMIN   integer  out    -               Minute ( 0-59 )
!     ISEC   integer  out    -               Second ( 0-59 )
!     DSEC   real*8   out    -               Second as double
!
!     Local variables :
!
!     Name   Type     Size   Description
!     ------ -----    ------ ------------------------
!     TEMP1  real*8   -      Temporary variable
!     TEMP2  real*8   -      Temporary variable
!     TEMP3  real*8   -      Temporary variable
!     TEMP4  real*8   -      Temporary variable, JULIAN
!     TEMP5  real*8   -      Temporary variable, fractional part JULIAN
!
!     Calls to : none
!
!***********************************************************************
!
!     Variables :
!
    INTEGER           :: IYEAR , IMONTH, IDAY  , IHOUR , IMIN  , ISEC
    DOUBLE PRECISION  :: JULIAN, TEMP1 , TEMP2 , TEMP3 , TEMP4 , TEMP5
    DOUBLE PRECISION  :: DSEC
    DOUBLE PRECISION  :: myJULIAN, delta
    integer           :: nTry
!
!***********************************************************************
!
!
!
    delta = 0.0D+00

    IF ( JULIAN .LT. 0.0 ) THEN
        IYEAR = -9999
    ELSE
        nTry = 1
        DO WHILE ( nTry <= 2 )
            myJULIAN= JULIAN + delta
            TEMP4 = myJULIAN
            TEMP5 = DMOD ( myJULIAN, 1.0D0 )
            IF ( TEMP5 .LT. 0.5 ) THEN
            TEMP3  = 0.5 + TEMP5
            TEMP4  = DINT ( TEMP4 )
            ELSE
            TEMP3  = TEMP5 - 0.5
            TEMP4  = DINT ( TEMP4 ) + 1.0
            ENDIF
            TEMP1  = TEMP4 + 68569.0
            TEMP2  = DINT  ( 4.0 * TEMP1 / 146097.0 )
            TEMP1  = TEMP1 - DINT ( ( 146097.0 * TEMP2 + 3.0 ) / 4.0 )
            IYEAR  = INT   ( 4000.0 * ( TEMP1 + 1.0 ) / 1461001.0 )
            TEMP1  = TEMP1 - DINT ( (1461.0D0 * IYEAR) / 4.0 ) + 31.0
            IMONTH = INT   ( 80.0 * TEMP1 / 2447.0 )
            IDAY   = INT   ( TEMP1 - AINT ( 2447.0 * IMONTH / 80.0 ) )
            TEMP1  = DINT  ( dble(IMONTH / 11.0D0) )
            IMONTH = INT   ( IMONTH + 2.0 - 12.0 * TEMP1 )
            IYEAR  = INT   ( 100.0 * ( TEMP2 - 49.0 ) + IYEAR + TEMP1 )
            IHOUR  = INT   ( TEMP3 * 24.0 )
            IMIN   = INT   ( TEMP3 * 1440.0 - 60.0 * IHOUR )
            DSEC   =         TEMP3 * 86400.0 - 3600.0 * IHOUR - 60.0*IMIN
            ISEC   = NINT  ( DSEC )

            if ( isec >= 60 ) then
                if ( nTry < 2 ) then
                    delta = 0.49999D+00 / 86400.0D+00
                    nTry = nTry + 1
                else
                    IYEAR = -9999
                    exit
                endif
            else
                exit
            endif
        ENDDO

    ENDIF

    END
end module m_gregor