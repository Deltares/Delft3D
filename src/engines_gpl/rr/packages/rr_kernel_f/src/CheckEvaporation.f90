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

  Subroutine CheckEvaporation  (IdH, IdM, IdS, EvapFormat2)

!*********************************************************************
!***                D E L F T         H Y D R A U L I C S
!***
!***              WATER RESOURCES AND ENVIRONMENT DIVISION
!*********************************************************************
!*** Check of verdampingsperiode en simulatietijdstap passend zijn
!*** zodat bv. geen verdamping wordt 'vergeten' (bij te grote tijdstapgrootte)
!*********************************************************************

   use Conf_arr
   use Network
   use Messages

   implicit none
   
   integer NrEvapHrs, IHour, IdH, IdM, IdS
   logical InEvapPeriod, warning, EvapFormat2

   If (EvapFormat2) then
      nrEvapHrs = 24
      TmEvap = 1.0
      TimeSettings%EvaporationFromHr=0
      TimeSettings%EvaporationToHr=24
   Else
      NrEvapHrs = 0
      if (Tmevap .gt. 0) then
         NrEvapHrs = ABS (timeSettings%evaporationToHr-timeSettings%evaporationFromHr)
      else
         call ErrMsgStandard (970, 0, ' CheckEvaporation', ' Evaporation from and to hours are equal, so evaporation is put to zero!' )
      endif

      Ihour = ConfArr_Get_Ihour()
      InEvapPeriod = (ihour .le. timeSettings%evaporationToHr .and. ihour .ge. timeSettings%evaporationFromHr)
      Warning = .false.

      If (Idh .ge. NrEvapHrs .and. .not. InEvapPeriod .and. NrEvapHrs .gt. 0) warning = .true.
      If (Idh .eq. 0 .and. idm .eq. 0 .and. ids .eq. 0 .and. .not. InEvapPeriod .and. NrEvapHrs .gt. 0) warning = .true.

      If (warning) call ErrMsgStandard (977, 0, &
      ' CheckEvaporation', &
      ' Evaporation may not be taken into account completely; Check timestep size and evaporation hours')
   Endif

  return
  End subroutine CheckEvaporation
