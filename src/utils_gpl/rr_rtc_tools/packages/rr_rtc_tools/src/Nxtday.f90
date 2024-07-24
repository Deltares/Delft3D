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
! at:               $Modtime:: 15-08-97 11:31a  $
!
! current revision: $Revision:: 3               $


      SUBROUTINE NXTDAY (IDEBUG, Y, M, D)

! *********************************************************************
! ***                D E L F T         H Y D R A U L I C S
! ***
! ***              WATER RESOURCES AND ENVIRONMENT DIVISION
! *********************************************************************
! *** Program :  DELFT_3B version 1.0.                 Date: March 1995
! *********************************************************************
! *** Last update: March  1995       By : Geert Prinsen
! *********************************************************************
! *** Brief description:
! *** ------------------
! ***   Zet nieuwe waarden jaar, maand, dag na elke dag.
! *********************************************************************
!
      implicit none
      
      INTEGER IDEBUG, Y, M, D, iLeap1, iLeap2, iLeap3

      IF (IDEBUG /= 0) WRITE (IDEBUG,1)
    1 FORMAT (' NXTDAY')
!
! *********************************************************************
! *** next day
! *********************************************************************
!
      IF (D .EQ. 31 .AND. (M .EQ. 1 .OR. M .EQ. 3 .OR. M .EQ. 5 .OR.    &
                          M .EQ. 7 .OR. M .EQ. 8 .OR. M .EQ. 10)) THEN
          D = 1
          M = M+1
      ELSEIF (D .EQ. 30 .AND. (M .EQ. 4 .OR. M .EQ. 6 .OR. M .EQ. 9 .OR.    &
                          M .EQ. 11))  THEN
          D = 1
          M = M+1
      ELSEIF (D .EQ. 31 .AND. M .EQ. 12) THEN
          D = 1
          M = 1
          Y = Y+1
      ELSEIF (D .GE. 28 .AND. M .EQ. 2) THEN
          ILEAP1 = (Y/4) * 4
          ILEAP2 = (Y/100) * 100
          ILEAP3 = (Y/400) * 400
          IF (D .EQ. 28 .AND. ILEAP1 .EQ. Y .AND. ILEAP2 .NE. Y) THEN
             D = D+1
          ELSEIF (D .EQ. 28 .AND. ILEAP3 .EQ. Y) THEN
             D=D+1
          ELSE
             D = 1
             M = M+1
          ENDIF
      ELSE
          D = D + 1
      ENDIF


      RETURN
      END subroutine NxtDay
