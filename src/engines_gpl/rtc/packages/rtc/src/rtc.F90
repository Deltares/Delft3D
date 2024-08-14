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

      PROGRAM RTC

! *********************************************************************
! ***                D E L F T         H Y D R A U L I C S
! ***
! ***         REGIONAL WATER RESOURCES AND ENVIRONMENT DIVISION
! *********************************************************************
! *** Program :  SOBEK-RTC version 2.09               Date: Mar 2004
! *** Module  :  RTC
! *********************************************************************
! *** Created    : June   1997       By : Geert Prinsen
! *********************************************************************
! *********************************************************************
! *** Brief description:
! *** ------------------
! ***   Real time control module for use in SOBEK-LITE.
! ***   Development based on MAALSTOP module and BESYS module
! ***   which were applied in North-Holland and Geleenbeek projects.
! ***   (T1622, T2021; T1659)
! ***   See memo STURING3.DOC June 1997.
! *********************************************************************
! *** Input/output parameters:     none.
! *** ------------------------
! *********************************************************************
! *** ADRESS     : Deltares,
! ***              P.O.BOX 177,
! ***              2600 MH DELFT, THE NETHERLANDS
! **********************************************************************

! **********************************************************************
  use RTCModule

#if (defined(HAVE_CONFIG_H))
! no action under Unix
#else
  use Kernel32
#endif

  implicit none


  Integer          RTC_Runid, RTC_Nevent, RTC_Ievent, RTC_NTimestepEvent
  Integer          ReturnCode
  Double Precision RTCTimeNow, RTCTimePreviousProgressMsg

  Integer, parameter  :: MaxArgLength = 256
  Integer, parameter  :: ArgCount = 3
  Character(Len=MaxArgLength), dimension(ArgCount) :: ArgsToRTC

  Integer                     :: LunRTCSimulate
  Character(Len=MaxArgLength) :: RTCSimulateFileName

  Integer  NrArg, i

#if (defined(HAVE_CONFIG_H))
! no action under Unix
#else
! For nice Progress bar also in Chinese Windows
  ReturnCode = SetConsoleOutputCP(1252)
#endif

  NrArg = COMMAND_ARGUMENT_COUNT()
  NrArg = min(NrArg,2)

  Do i = 0, NrArg
     call GET_COMMAND_ARGUMENT(i, ArgsToRTC(i + 1))
  Enddo

  ReturnCode = RTCCreate(RTC_RunId,ArgsToRTC)
  If (ReturnCode .ne. 0) goto 9999

  ReturnCode = RTCInitialize(RTC_RunId,RTC_Nevent)
  If (ReturnCode .ne. 0) goto 9999

  ReturnCode = RTCInitExternals(RTC_RunId)
  If (ReturnCode .ne. 0) goto 9999


  Do RTC_IEvent=1,RTC_Nevent

     ReturnCode = RTCInitializeEvent(RTC_RunId,RTC_IEvent,RTC_NTimestepEvent)
     If (ReturnCode .ne. 0) goto 9999

     RTCSimulateFileName = ' '
     RTCSimulateFileName = NAMFIL(29)
     RTCTimePreviousProgressMsg = 0

     Do RTC_ITimestep=1,RTC_NTimestepEvent
        ReturnCode = RTCPerformTimestep(RTC_RunId,RTC_IEvent,RTC_ITimestep)
        If (ReturnCode .ne. 0) goto 9999
        If (RTCSimulateFileName .ne. '') then
           RTCTimeNow = RTCJulianNow
           If (RTCTimeNow-RTCTimePreviousProgressMsg .gt. 0.10/86400. .or. RTC_Itimestep .eq. RTC_NTimestepEvent) then
             LunRTCSimulate =  DioNewLun()
             OPEN (LunRtcSimulate,FILE=RTCSimulateFileName, status='unknown')
             write (LunRTCSimulate, * ) 1, RTC_NTimestepEvent, RTC_ITimestep , &
                                           RTC_Nevent, RTC_IEvent, 0.0
             close(LunRTCSimulate)
             RTCTimePreviousProgressMsg = RTCTimeNow
           Endif
        Endif
     Enddo

     ReturnCode = RTCFinalizeEvent(RTC_Runid,RTC_IEvent)
     If (ReturnCode .ne. 0) goto 9999

  Enddo


  ReturnCode = RTCFinalize(RTC_Runid)

  9999 Continue
  if (RTCCrashed .and. .not. RTCSelfCrash) then
     Call CrashCt (IdCnt, .false. )
!    other module has crashed, so RTC has return code 0
     ReturnCode = RTCWriteReturnCodeFile(RTC_Runid)
  endif

Stop
END PROGRAM RTC

