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

 ! Last changed
! by:               $Author:: Schrier           $
! at:               $Modtime:: 19-08-97 4:36p   $
!
! current revision: $Revision:: 4               $


      SUBROUTINE TestDate (ChYear, ChMonth, ChDay, RdYear, RdMonth, RdDay, IExit)

! *********************************************************************
! ***                D E L F T         H Y D R A U L I C S
! ***
! ***              WATER RESOURCES AND ENVIRONMENT DIVISION
! *********************************************************************
! *** Program :  DELFT_3B version 1.0.                 Date: March 2000
! *********************************************************************
! *** Brief description:
! *** ------------------
! ***   Test if date read (RdYear .. RdDay) equal to Check Date (ChYear .. ChDay)
! *********************************************************************
! *** Input/output parameters:
! *** ------------------------
! ***  Iexit  = 90   read date before check date, so read next record
! ***  Iexit  = 999  read date equal to check date, so found
! ***  Iexit  = 915  read date after check date, so check date can not be found (stop error)
! *********************************************************************
!
      INTEGER        ChYear, ChMonth, ChDay
      INTEGER        RdYear, RdMonth, RdDay
      Integer        Iexit


      IF  (ChYear .GT. Rdyear) THEN
          Iexit = 90
      ELSEIF (ChYear .eq. Rdyear .AND. ChMonth .gt. Rdmonth) THEN
          IExit = 90
      ELSEIF (ChYear .eq. Rdyear .AND. ChMonth .eq. Rdmonth .AND. &
                                                ChDay .gt. Rdday) THEN
          IExit = 90
      ELSEIF (ChYear .eq. Rdyear .AND. ChMonth .eq. Rdmonth .AND. &
                                                ChDay .eq. Rdday) THEN
          IExit = 999
      ELSE
          IExit = 915
      ENDIF

      RETURN
      END
