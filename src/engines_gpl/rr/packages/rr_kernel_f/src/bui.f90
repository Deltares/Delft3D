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

       Subroutine Bui (RR_IEvent)


!*********************************************************************
!*** Program :  DELFT-3B version 2.00                Date: March 2000
!*** Module  :
!*********************************************************************
!*** Created    : Maart 2000                      By : Geert Prinsen
!*********************************************************************
!*********************************************************************
!*** Subroutine to do calculations for event Ievent
!*********************************************************************

     use RRModule

     implicit none

! Simulatie van 1 bui, nodig voor Unix versie
! Voorlopig even met dummy RR_RunId

  Integer RR_Ievent, RR_RunId, RR_NTimestepEvent, RR_Timestep, ReturnCode

  RR_RunId = 1

  ReturnCode = RRInitializeEvent(RR_RunId, RR_Ievent,RR_NTimestepEvent)
  If (ReturnCode .ne. 0) goto 9999

  Do RR_Timestep=1,RR_NTimestepEvent
     ReturnCode = RRPerformTimestep (RR_RunId,RR_Ievent,RR_Timestep)
     If (ReturnCode .ne. 0) goto 9999
  Enddo

  Returncode = RRFinalizeEvent(RR_Runid,RR_Ievent)

  9999 Continue
  if (.not. dimr_mode .and. crashed) Call CrashCt(IdCnt, .false. )

  Return
  End Subroutine Bui

