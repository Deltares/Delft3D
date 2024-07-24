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

      module M_control

      implicit none
      
      CHARACTER*256   IODir
      CHARACTER*26    Procs, Deps, IniDeps
      INTEGER   ::    NrDep, NrProc, IniNrDep
      LOGICAL   ::    ProcFirst = .false.
      logical   ::    IniFirst  = .false.    !hk: is this ok?

      end module M_control
      
      SUBROUTINE INITFP (FirstProc, Initmode)

      implicit none

      logical                        :: firstproc
      logical                        :: initmode
      
      RETURN
      END


      SUBROUTINE INITCT (CmdLine, Id, Status)
      
      implicit none
      
      CHARACTER*(*) CmdLine
      INTEGER*4     Id
      INTEGER*2     Status

      Status = 0
      !HVP Temporarily, since file i/o will vanish
      Id = 1

      RETURN
      END

      SUBROUTINE CrashCt (Id, selfcrash)
      implicit none
      INTEGER*4 Id
      LOGICAL   Selfcrash

      RETURN
      END


      SUBROUTINE STEPCT (timold, timnew, Id, Status, InitMode, crashed)
      implicit none
      Double Precision  timold, timnew
      INTEGER*4 Id
      INTEGER*2 Status
      LOGICAL   InitMode, Crashed

      RETURN
      END

      SUBROUTINE ENDCT (Id, Status)
      implicit none
      INTEGER*4 Id
      INTEGER*2 Status

      Status = 0

      RETURN
      END
