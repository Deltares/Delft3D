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

  Subroutine SetInitialDatesAndTimes (Ievent, JulianDate1, JulianDate2, NrDaysSinceStartFirstEvent, NrDaysSinceStart, &
                                     JulianDateTime1, JulianDateTime2, &
                                     NrSecondsSinceStartFirstEvent, NrTimestepsSinceStartFirstEvent, &
                                     ITime, TimeD, TimeE, tmpYear, tmpMonth, tmpDay, tmpHour, tmpMin, tmpSec)

!*********************************************************************
!***                D E L F T         H Y D R A U L I C S
!***
!***              WATER RESOURCES AND ENVIRONMENT DIVISION
!*********************************************************************
!*** Program :  DELFT-3B version 2.00                Date: Nov   1996
!*** Module  :
!*********************************************************************
!*** Created    : Sept   1999                     By : Geert Prinsen
!*********************************************************************
!*** Brief description:
!*** ------------------
!***    Initialisatie van enkele datum/tijd en tijdstap zaken
!*********************************************************************
!*** Input/output parameters:
!*** ------------------------
!*********************************************************************
!*** ADRESS     : Deltares,
!***              P.O.BOX 177,
!***              2600 MH DELFT, THE NETHERLANDS
!**********************************************************************

  USE CONF_FIL
  USE CONF_ARR
  use Network

!*** OTHER DATA

  IMPLICIT NONE

  Double Precision  NrSecondsSinceStartFirstEvent
  Integer       NrTimestepsSinceStartFirstEvent
  Integer       iEvent, iTime, TimeD, timeE
  Integer       NrDaysSinceStartFirstEvent, NrDaysSinceStart, NrsEvap
  Integer       tmpYear, tmpDay, tmpMonth
  Integer       tmpHour, tmpMin, tmpSec

!extra variables for correct computation of days in BndFlTot.HIS file
  Integer          Date1, Date2, Time1, Time2
  Double precision JulianDate1, JulianDate2, Julian
  Double precision JulianDateTime1, JulianDateTime2
!
  Integer iDebug



!*** Set initial values at start of event
!*** for rainfall data: mm/hour assumed;
!*** for evaporation data: mm/day assumed;

        call ConfArr_set_iYear(EVSTRT(1))

        call ConfArr_set_iMonth(EVSTRT(2))
        call ConfArr_set_iDay(EVSTRT(3))
        call ConfArr_set_iHour(EVSTRT(4))
        call ConfArr_set_iMinute(EVSTRT(5))
        call ConfArr_set_iSecond(EVSTRT(6))

        idebug = Conffil_get_idebug()
        IF (ConfArr_get_IHOUR() .EQ. 24) THEN
           call ConfArr_set_IHOUR(0)
           tmpYear = ConfArr_get_IYEAR()
           tmpMonth = ConfArr_get_iMonth()
           tmpDay = ConfArr_get_IDAY()
           CALL NXTDAY (Idebug, tmpYear, tmpMonth, tmpDay)
           call ConfArr_set_iYear(tmpYear)
           call ConfArr_set_iMonth(tmpMonth)
           call ConfArr_set_iDay(tmpDay)
        ENDIF
        tmpYear = ConfArr_get_IYEAR()
        tmpMonth = ConfArr_get_iMonth()
        tmpDay = ConfArr_get_IDAY()
        tmpHour  = ConfArr_get_iHour()
        tmpMin   = ConfArr_get_iMinute ()
        tmpSec   = ConfArr_get_iSecond ()

        iTIME = ConfArr_get_iSecond() + (ConfArr_get_iMinute() * NRSMIN) + &
                (ConfArr_get_iHour() * NRSHR) - timeSettings%timestepSize
        TIMED = MOD (iTIME, NRSDAY) ! nrsDay was tot float gecast
        NrsEvap = ConfArr_get_NrsEvap()
        TIMEE = MOD (iTIME, NrsEvap)


!set date, julian date etc.
        If (Ievent .eq. 1) then
            NrDaysSinceStartFirstEvent = 0
            NrSecondsSinceStartFirstEvent = 0
            NrTimestepsSinceStartFirstEvent = 0
!           Date1 = EVSTRT(1) * 10000 + EVSTRT(2) * 100 + EVSTRT(3)
            Date1 = tmpYear * 10000 + tmpMonth * 100 + TmpDay
!           Time1 = EVSTRT(4) * 10000 + EVSTRT(5) * 100 + EVSTRT(6)
            Time1 = TmpHour * 10000 + tmpMin * 100 + tmpSec
            if (idebug .ne. 0) write(Idebug,*) ' Ievent', Ievent
            JulianDate1 = Julian (Date1,0)
            JulianDateTime1 = Julian (Date1,Time1)
        else
!           Date2 = EVSTRT(1) * 10000 + EVSTRT(2) * 100 + EVSTRT(3)
            Date2 = tmpYear * 10000 + tmpMonth * 100 + TmpDay
!           Time2 = EVSTRT(4) * 10000 + EVSTRT(5) * 100 + EVSTRT(6)
            Time2 = TmpHour * 10000 + tmpMin * 100 + tmpSec
            if (idebug .ne. 0) write(Idebug,*) ' Ievent', Ievent
            if (idebug .ne. 0) write(Idebug,*) ' SetDate2 and Time2', Date2, Time2
            JulianDate2 = Julian (Date2,0)
            JulianDateTime2 = Julian (Date2,Time2)
            NrDaysSinceStartFirstEvent = INT (JulianDate2-JulianDate1)
            if (idebug .ne. 0) write(Idebug,*) ' NrDaysSinceStartFirstEvent',NrDaysSinceStartFirstEvent
            NrSecondsSinceStartFirstEvent = 86400. * (JulianDateTime2-JulianDateTime1)
            if (idebug .ne. 0) write(Idebug,*) ' JulianDateTime2/1', JulianDateTime2, JulianDateTime1
            if (idebug .ne. 0) write(Idebug,*) ' NrSecondsSinceStartFirstEvent',NrSecondsSinceStartFirstEvent
            NrTimestepsSinceStartFirstEvent = INT (NrSecondsSinceStartFirstEvent / timeSettings%timestepSize)
            if (idebug .ne. 0) write(Idebug,*) ' NrTimestepsSinceStartFirstEvent',NrTimestepsSinceStartFirstEvent
        endif
        NrDaysSinceStart=NrDaysSinceStartFirstEvent
!end date


 Return
END Subroutine SetInitialDatesAndTimes

