!----- AGPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2025.                                
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

  Subroutine SetControlDatesAndTimes (tmpYear, tmpMonth, tmpDay, tmpHour, tmpMin, tmpSec, &
                                     IdH, IdM, IDS, TimNew, Idebug)


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

  Integer  tmpYear, tmpDay, tmpMonth, tmpHour, tmpMin, tmpSec
  Double Precision TimNew
  Integer  IdH, IdM, IdS

  Integer iDebug

            tmpYear = ConfArr_get_IYEAR()
            tmpMonth = ConfArr_get_iMonth()
            tmpDay = ConfArr_get_IDAY()
            tmpHour = ConfArr_get_iHour() + IDH
            tmpMin = ConfArr_get_IMinute() + IDM
            tmpSec = ConfArr_get_ISecond() + IDS
            IF (tmpSec .GE. NRSMIN) THEN
               tmpSec = tmpSec - NRSMIN
               tmpMin = tmpMin + 1
            ENDIF
            IF (tmpMin .GE. NRSMIN) THEN
               tmpMin = tmpMin - NRSMIN
               tmpHour = tmpHour + 1
            ENDIF
            if (tmpHour .ge. 24) Then
                CALL NXTDAY (Idebug, tmpYear, tmpMonth, tmpDay)
                tmpHour = 0
            endif
            Call SetTimNew (tmpYear, tmpMonth, tmpDay, tmpHour, tmpMin, tmpSec, TimNew)

 Return
END Subroutine SetControlDatesAndTimes




 Subroutine SetTimNew (tmpYear, tmpMonth, tmpDay, tmpHour, tmpMin, tmpSec, TimNew)

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
!***    Set TimNew op basis variaberlen tmpYear ... tmpSec
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

  Integer  tmpYear, tmpDay, tmpMonth, tmpHour, tmpMin, tmpSec
  Double Precision TimNew, rhelp


  rhelp = tmpYear * 10000.
  timnew = rhelp
  rhelp = tmpMonth * 100.
  timnew = timnew + rhelp
  rhelp = tmpDay
  timnew = timnew + rhelp
  rhelp = tmpHour / 100.
  timnew = timnew + rhelp
  rhelp = tmpMin / 10000.
  timnew = timnew + rhelp
  rhelp = tmpSec / 1000000.
  timnew = timnew + rhelp

 Return
END Subroutine SetTimNew

