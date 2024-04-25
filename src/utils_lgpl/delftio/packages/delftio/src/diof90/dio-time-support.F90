!----- LGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2024.                                
!                                                                               
!  This library is free software; you can redistribute it and/or                
!  modify it under the terms of the GNU Lesser General Public                   
!  License as published by the Free Software Foundation version 2.1.                 
!                                                                               
!  This library is distributed in the hope that it will be useful,              
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU            
!  Lesser General Public License for more details.                              
!                                                                               
!  You should have received a copy of the GNU Lesser General Public             
!  License along with this library; if not, see <http://www.gnu.org/licenses/>. 
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
!  
!  
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!
!!! DIO-time-support: time string support functions
!!!
!!! (c) Deltares, apr 2000
!!!
!!! Stef.Hummel@deltares.nl
!!!
!!! DIO time : input : yyyy.dd.mm.hh.mm.ss.cc   ( . means any char)
!!!            output: yyyy/dd/mm hh:mm:ss.cc
!!!
!!! HIS time :         2002.01.01 00:00:00  (scu=       1s)
!!!
!!!                    123456789012345678901234567890123456
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


!!
!! Support Functions called from dio-ds
!!

!
! Convert HIS time string to DIO time string
!

subroutine DioHisTime2DioTime(hisTime, dioTime)

    ! arguments
    
    character(Len=*),intent(IN)  :: hisTime  ! HIS time format
    character(Len=*),intent(OUT) :: dioTime  ! DIO time format

    ! body
    
    dioTime = ' '
    write(dioTime, '(a4,''.'',a2,''.'',a2,'' '',a2,'':'',a2,'':'',a2,''.'',a2)' ) &
                     hisTime(1:4), &
                     hisTime(6:7), &
                     hisTime(9:10), &
                     hisTime(12:13), &
                     hisTime(15:16), &
                     hisTime(18:19), &
                     '00'

end subroutine DioHisTime2DioTime


!
! Convert HIS time string to DIO time string
!

subroutine DioTime2HisTime(dioTime, hisTime)

    ! arguments
    
    character(Len=*),intent(IN)  :: dioTime  ! HIS time format
    character(Len=*),intent(OUT) :: hisTime  ! DIO time format

    ! body
    
    hisTime = ' '
    write(hisTime, '(a4,''.'',a2,''.'',a2,'' '',a2,'':'',a2,'':'',a2)' ) &
                     dioTime(1:4), &
                     dioTime(6:7), &
                     dioTime(9:10), &
                     dioTime(12:13), &
                     dioTime(15:16), &
                     dioTime(18:19)

end subroutine DioTime2HisTime


function DioTimeString2Julian ( dioTime ) result(julian)

!     This functions returns the so called Julian day of a date, or
!     the value -1.0 if an error occurred.
!
!     The Julian day of a date is the number of days that has passed
!     since January 1, 4712 BC at 12h00 ( Gregorian). It is usefull
!     to compute differces between dates.

      ! return value
      double precision            :: julian   ! julian date

      ! arguments
      character(Len=*),intent(IN) :: dioTime  ! DIO time format

      ! locals
      integer          :: iyear, imonth, iday, ihour, imin, isec, icentiSec
      integer          :: monlen(12)
      double precision :: temp1 , temp2
!
!     Initialize lenghts of months :
!
      data monlen / 31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 /
!
!
      julian = -1.0D+00
      icentiSec = 0
      read ( dioTime(1:4),  *, err=999) iyear
      read ( dioTime(6:7),  *, err=999) imonth
      read ( dioTime(9:10), *, err=999) iday
      read ( dioTime(12:13),*, err=999) ihour
      read ( dioTime(15:16),*, err=999) imin
      read ( dioTime(18:19),*, err=999) isec
      if ( len_trim(dioTime) .ge. 22 ) then
          read ( dioTime(21:22),*, err=998) icentiSec
      endif

 998  continue

      if (( iyear  .lt. -4713 ) .or. ( imonth .lt.  1 ) .or.    &
          ( imonth .gt.    12 ) .or. ( iday   .lt.  1 ) .or.    &
          ( iday   .gt. monlen(imonth) ) .or.                   &
          ( ihour  .lt.     0 ) .or. ( ihour  .gt. 23 ) .or.    &
          ( imin   .lt.     0 ) .or. ( imin   .gt. 59 ) .or.    &
          ( isec   .lt.     0 ) .or. ( isec   .gt. 59 )) then
         julian = -1.0D+00
         goto 999
      else
         temp1  = int (( imonth-14.0) / 12.0 )
!---------debug---------------
!        write(*,*) 'temp1 : ', temp1
!        write(*,*) 'DBG 1 : ', &
!               int ( 1461.0 * ( iyear + 4800.0 + temp1 ) / 4.0 )
!        write(*,*) 'DBG 2 : ', &
!               int ( 367.0 * ( imonth - 2.0 - temp1 * 12.0 ) / 12.0 )
!        write(*,*) 'DBG 3 : ', &
!               int ( 3.0 * int ( ( iyear + 4900.0 + temp1 ) / 100.0 ) / &
!               4.0 )
!-----------------------------
         temp2  = iday - 32075.0 +                                        &
                int ( 1461.0 * ( iyear + 4800.0 + temp1 ) / 4.0 ) +       &
                int ( 367.0 * ( imonth - 2.0 - temp1 * 12.0 ) / 12.0 ) -  &
                int ( 3.0 * int ( ( iyear + 4900.0 + temp1 ) / 100.0 ) /  &
                4.0 )
         temp1  = float ( ihour ) * 3600.0 +                                   &
                  float ( imin  ) *   60.0 +                                   &
                  float ( isec  ) + ( float ( icentiSec ) / 100.0 ) - 43200.0
         julian = temp2 + ( temp1 / 86400.0 )
      endif

      return

      ! error handling: no action (jul already 0)
  999 continue

end function DioTimeString2Julian



function DioDeltaTimeString2Julian ( dioTime ) result(julian)

!     This functions returns the so called Julian day of a date, or
!     the value -1.0 if an error occurred.
!
!     The Julian day of a date is the number of days that has passed
!     since January 1, 4712 BC at 12h00 ( Gregorian). It is usefull
!     to compute differces between dates. ( See SUBROUTINE GREGOR
!     for the reverse proces ).

      ! return value

      double precision    ::  julian

      ! arguments
    
      character(Len=*),intent(IN)  :: dioTime  ! DIO time format

      ! locals

      integer          :: iday  , ihour , imin  , isec, icentiSec

      double precision ::  temp1 , temp2
!
!
!      read ( dioTime(1:4),  *) iyear
!      read ( dioTime(6:7),  *) imonth
!      read ( dioTime(9:10), *) iday
!      read ( dioTime(12:13),*) ihour
!      read ( dioTime(15:16),*) imin
!      read ( dioTime(18:19),*) isec
!      read ( dioTime(21:22),*) icentiSec

      read ( dioTime(1:2), *) iday
      read ( dioTime(4:5),*) ihour
      read ( dioTime(7:8),*) imin
      read ( dioTime(10:11),*) isec
      read ( dioTime(13:14),*) icentiSec

      if (( iday   .lt.  0 ) .or.    &
          ( ihour  .lt.  0 ) .or.  &
          ( imin   .lt.  0 ) .or.   &
          ( isec   .lt.  0 )) then
         julian = -1.0
         goto 999
      else
         temp2  = float ( iday  )
         temp1  = float ( ihour ) * 3600.0 +                                   &
                  float ( imin  ) *   60.0 +                                   &
                  float ( isec  ) + ( float ( icentiSec ) / 100.0 )
         julian = temp2 + ( temp1 / 86400.0 )
      endif
  999 return

end function DioDeltaTimeString2Julian 


function DioJulian2DioTime ( julian ) result(dioTime)

    use dio_ds

    implicit  none

    ! return value
    character(len=DioMaxTimLen) :: dioTime

    ! arguments
    double precision, intent(IN) :: julian

    ! locals
    character(len=100)          :: dioTimeFormat
    integer  :: iyear , imonth, iday  , ihour , imin, isec
    double precision :: dsec

    ! body

    dioTime = 'Invalid_time'
    call Gregor(julian, iyear , imonth, iday  , ihour , imin, isec, dsec)
    if ( iyear .ne. -9999 ) then
        if ( dsec .lt. 9.995) then
            dioTimeFormat = '(i4.4,''/'',i2.2,''/'',i2.2,'' '',i2.2,'':'',i2.2,'':0'',f4.2)'
        else
            dioTimeFormat = '(i4.4,''/'',i2.2,''/'',i2.2,'' '',i2.2,'':'',i2.2,'':'',f5.2)'
        endif
        write(dioTime, dioTimeFormat) &
                         iyear, &
                         imonth, &
                         iday, &
                         ihour, &
                         imin, &
                         dsec
    endif
end


function DioJulian2HisTime ( julian ) result(hisTime)

    use dio_ds

    implicit  none

    ! return value
    character(len=DioMaxTimLen) :: hisTime

    ! arguments
    double precision, intent(IN) :: julian

    ! locals
    integer          :: iyear , imonth, iday  , ihour , imin, isec
    double precision :: dummy
    ! body

    hisTime = '0000 00 00 00:00:00'
    call Gregor(julian, iyear , imonth, iday  , ihour , imin, isec, dummy)
    if ( iyear .ne. -9999 ) then
        write(hisTime, &
            '(i4.4,''.'',i2.2,''.'',i2.2,'' '',i2.2,'':'',i2.2,'':'',i2.2)') &
              iyear,     imonth,    iday,      ihour,     imin,      isec
    endif
end


function DioJulian2DioDeltaTime ( julian ) result(dioDeltaTime)

    use dio_ds                  ! for DioMaxTimLen

    implicit  none

    ! return value

    character(len=DioMaxTimLen) :: dioDeltaTime
    character(len=100)          :: dioDeltaTimeFormat

    ! arguments

    double precision, intent(IN) :: julian

    ! locals

    integer          :: iday  , ihour , imin
    double precision :: sec, temp

    dioDeltaTime='Invalid_Time'

    if ( .not. (julian .lt. 0.0) ) then
        iday      = int   ( julian )
        temp      = julian - dble(iday)
        ihour     = int   ( temp * 24.0D+00 )
        imin      = int   ( temp * 1440.0D+00 - 60.0D+00 * dble(ihour) )
        sec       = temp * 86400.0D+00 - 3600.0D+00 * dble(ihour) - 60.0D+00*dble(imin)

        if ( sec .lt. 9.995) then
            dioDeltaTimeFormat = '(i2.2,'' '',i2.2,'':'',i2.2,'':0'',f4.2)'
        else
            dioDeltaTimeFormat = '(i2.2,'' '',i2.2,'':'',i2.2,'':'',f5.2)'
        endif
        write(dioDeltaTime, dioDeltaTimeFormat) &
                         iday, &
                         ihour, &
                         imin, &
                         sec
    endif

end function DioJulian2DioDeltaTime


!***********************************************************************
!***********************************************************************
!    Date:       29 Apr 1993 / 11 Dec 2002 (adjusted)
!    Time:       09:35
!    Program:    GREGOR.FOR
!    Version:    1.01
!    Programmer: Andre Hendriks
!    Previous version(s):
!    1.00 -- 29 Apr 1993 -- 09:23 -- Operating System: DOS
!    0.0 -- 29 Apr 1993 --  9:22 -- Operating System: DOS
!    Project:    T1234.56
!    Module:     GREGOR
!    Function:
!    Comment:
!    Reference:
!    Review:
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

