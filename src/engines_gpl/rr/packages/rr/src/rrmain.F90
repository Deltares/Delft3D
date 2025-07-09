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

  PROGRAM Sobek_3B

!*********************************************************************
!***                D E L F T         H Y D R A U L I C S
!***
!***              WATER RESOURCES AND ENVIRONMENT DIVISION
!*********************************************************************
!*** Program :  Sobek-RR version 2.09                Date: Jan   2004
!*** Module  :
!*********************************************************************
!*** Created    : March  1995                     By : Geert Prinsen
!*********************************************************************
!*** Last update: 19 February 1997                 By: Peter Schrier
!*********************************************************************
!*** Brief description:
!*** ------------------
!***    Hoofdmodule
!*********************************************************************
!*** Input/output parameters:     none.
!*** ------------------------
!*********************************************************************
!*** ADRESS     : Deltares,
!***              P.O.BOX 177,
!***              2600 MH DELFT, THE NETHERLANDS
!**********************************************************************

  use RRModule
  use Balance
  use globals
  use rrmain_version_module
  use timers

#if defined(WIN32)
  use Kernel32
! use DfWinty
#else
! no action under Unix
#endif

  implicit none

  Integer          RR_RunId, RR_Nevent, RR_Ievent, RR_NTimeStepEvent
  Integer          ReturnCode
  Double Precision TimeNow, TimePreviousProgressMsg

  Integer, parameter  :: MaxArgLength = 256
  Integer, parameter  :: ArgCount = 3
  Character(Len=MaxArgLength), dimension(ArgCount) :: ArgsToRR

  Integer                     :: LunRRSimulate
  Character(Len=MaxArgLength) :: RRSimulateFileName

  Integer  NrArg, i

  character(len=120) :: cident


  call timini()
  timon = .true.

#if defined(WIN32)
  NrArg = nargs()
! For nice Progress bar also in Chinese Windows
  ReturnCode = SetConsoleOutputCP(1252)
! aanpassen kleuren; nog niet ok
!  Iconsole = GetConsoleWindow()
!  ReturnCode = SetConsoleTextAttribute(Iconsole,COMMON_LVB_REVERSE_VIDEO)
!  ReturnCode = SetConsoleTextAttribute(Iconsole,FOREGROUND_RED)
#elif defined(X64)
  NrArg = nargs()
! For nice Progress bar also in Chinese Windows
!  ReturnCode = SetConsoleOutputCP(1252)
! aanpassen kleuren; nog niet ok
!  Iconsole = GetConsoleWindow()
!  ReturnCode = SetConsoleTextAttribute(Iconsole,COMMON_LVB_REVERSE_VIDEO)
!  ReturnCode = SetConsoleTextAttribute(Iconsole,FOREGROUND_RED)
#else
  NrArg = command_argument_count()
#endif

  ! Runs as F90-Program
  in_f90_runner = .true.

  NrArg = min(NrArg,3)

  Do i = 0, NrArg - 1
     call getarg(i, ArgsToRR(i + 1))
  Enddo

  ReturnCode = RRCreate(RR_RunId,ArgsToRR)
  If (ReturnCode .ne. 0) goto 9999

!
! Identification string
!
  call getfullversionstring_rrmain(cident)
  call getfullversionstring_rrmain(rr_version_string)

  ReturnCode = RRInitialize(RR_RunId,RR_Nevent)
  If (ReturnCode .ne. 0) goto 9999


#if (defined(SOBEK_PARALLEL))
!  Unix version
!  Parallell draaien van de buien op Unix
   Call Parallell (Nevent)

#else

  Do RR_IEvent=1,RR_Nevent

     ReturnCode = RRInitializeEvent(RR_RunId,RR_IEvent,RR_NTimestepEvent)
     If (ReturnCode .ne. 0) goto 9999

     RRSimulateFileName = ' '
     RRSimulateFileName = ConfFil_get_NAMFIL(120)
     If (RRSimulateFileName .ne. '') CheckBalance = .true.
     TimePreviousProgressMsg = 0.

     Do RR_ITimestep=1,RR_NTimestepEvent
        ReturnCode = RRPerformTimestep(RR_RunId,RR_IEvent,RR_ITimestep)
        If (ReturnCode .ne. 0) goto 9999
        If (RRSimulateFileName .ne. '') then
           TimeNow = JulianNow
           If (TimeNow-TimePreviousProgressMsg .gt. 0.10/86400. .or. RR_Itimestep .eq. RR_NTimestepEvent) then
              Call OpenFl(LunRRSimulate, RRSimulateFileName,1,2)
!             VersionNumber(=1), nStepInBui, iStepInBui, nEvents, iEvent, BalanceError, %balance_Error
              write (LunRRSimulate, * ) 1.01, RR_NTimestepEvent, RR_ITimestep , &
                                        RR_Nevent, RR_IEvent, Balance_error, bal_error2
              close(LunRRSimulate)
              TimePreviousProgressMsg = TimeNow
           Endif
        Endif
     Enddo

     ReturnCode = RRFinalizeEvent(RR_Runid,RR_IEvent)
     If (ReturnCode .ne. 0) goto 9999

  Enddo

#endif

  ReturnCode = RRFinalize(RR_Runid)

!test: do everything once again, starting with RRCreate upto RRFinalize
!end test

  9999 Continue
  if (crashed)  then
      Call CrashCt (IdCnt, .false. )
!     other module has crashed, RR should write zero return code
      String = ' Succesfull logout of Sobek_RR '
      OutputString = TranslateString(LanguageHandle, String)
      ReturnCode = RRWriteReturnCodeFile(RR_RunId)
  endif

Stop
END PROGRAM Sobek_3B

