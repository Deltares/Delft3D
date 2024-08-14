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

  Subroutine SetDatesAndTimes (Idebug, ITmstp, TimeD, TimeE, TimeR, TimerRunoff, TimerTemperature, TimeS, IdH, IdM, IdS)


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
!***    Set times
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

  Integer  TimeD, TimeE, TimeR, TimerRunoff, TimerTemperature, TimeS, Itmstp
  Integer  IdH, IdM, IdS

  Integer iDebug


!***    Set times

        TIMED = TIMED + timeSettings%timestepSize
        TIMEE = TIMEE + timeSettings%timestepSize
        TIMER = TIMER + timeSettings%timestepSize
        TIMERRunoff = TIMERRunoff + timeSettings%timestepSize
        TIMERTemperature = TIMERTemperature + timeSettings%timestepSize
        TIMES = TIMES + timeSettings%timestepSize
        If (ITMSTP .GT. 1) Then
           call ConfArr_set_IHOUR(ConfArr_get_IHOUR() + IDH)
           call ConfArr_set_iMinute(ConfArr_get_iMinute()  + IDM)
           call ConfArr_set_iSecond(ConfArr_get_iSecond()  + IDS)
           If (ConfArr_get_iSecond() .GE. NRSMIN) Then
               call ConfArr_set_iSecond(ConfArr_get_iSecond()- NRSMIN)
               call ConfArr_set_iMinute(ConfArr_get_iMinute() + 1)
           Endif
! aantal min per uur=aantal sec. per minuut!; lees NRMINHR ipv NRSMIN

           If (ConfArr_get_iMinute() .GE. NRSMIN) Then
               call ConfArr_set_iMinute(ConfArr_get_iMinute() - NRSMIN)
               call ConfArr_set_IHOUR(ConfArr_get_IHOUR() + 1)
           Endif
        Endif
        If (iDebug /=  0)  then
          WRITE(IDEBUG,*) ' Timers', TimeD, TIMEE, TIMER, TimerRunoff, TimerTemperature
          WRITE(IDEBUG, *)  ConfArr_get_iMonth(), ConfArr_get_IDAY(), &
                                          ConfArr_get_IHOUR(), ConfArr_get_iMinute()
        Endif


 Return
END Subroutine SetDatesAndTimes

