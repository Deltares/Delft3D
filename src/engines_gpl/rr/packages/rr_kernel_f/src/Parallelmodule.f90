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

 
module ParallelData

  ! variables

  Integer       MaxItr, Makelogfile, MessageInundation, MessageVolumeCheck, MessagePerTimestep, NrTimestepsNegativeVolume
  Real          AbsoluteConvergenceCheck, RelativeConvergenceCheck

  Logical       CheckBalance, Crashed, OutputOwAndGwToRTC
  Logical       EmulateUnixOnPC, HeaderRunoffOutAlways, NwrwContinuous
  Integer       NrsRaiRks, NrOutputPeriods, IoutPeriod
  Integer, Pointer, save ::  OutputEventStartDateTime (:,:), OutputEventDuration(:,:)
  Double Precision TimeInEvent, JulianTimestep

  Integer       iDh, iDm, iDs
  Integer       Idebug, IdebugCapsimFromTimestep, IdebugCapsimToTimestep
  Integer       IdebugFromTimestep, IdebugToTimestep, Idebug2FromTimestep, Idebug2ToTimestep
  Integer       IBar0, NrEvapStations
  Integer       iout2,iout3,iout4,iout5,iout6,iout7,iout8

  Integer       iOutPl, iOutCB, IoutAbrTot, Inrestart, Ioutrestart
  Integer       InModFlow, IOutModFlow

  Integer       NrDaysSinceStartFirstEvent,  NrTimestepsSinceStartFirstEvent
  Double Precision NrSecondsSinceStartFirstEvent, NrSecondsSincestart
  Integer       NrSecondsIoutAbr
  Integer       IScren
! Okt 1999      Addition for starting daily values not at 0 o'clock, but at x o'clock
  Real          StartSecDailyRainfall
!huidige datum/tijd
  CHARACTER(len=10) CDATE, CTIME, CZONE
  INTEGER          time_fields(8)
  Double Precision JulianStartSimulation, JulianNowSimulation, JulianStartEventSimulation
  Double Precision JulianStart, JulianNow,  &
                   JulianStartOutputDate, JulianEndOutputDate
  Real             EstimatedRemainingDuration
  Integer          IDateAct, ITimeAct, RemHours, RemMinutes, RemSeconds, TotalNrTimesteps, AlreadySimulated
  Character(len=9) RemTime
  Logical          EstimateRemainingDuration, TestSaveState
  Integer          HisConvergence, RestoreTimeStep, RestoreInTimestep





!Controller Simultaan draaien
  Double Precision TIMOLD0,  TIMOLD, TIMNEW, DELTAT
  INTEGER IDCNT
  INTEGER ITSTU
  LOGICAL CHKSTU, FirstProc
  Logical InitMode

end Module ParallelData
