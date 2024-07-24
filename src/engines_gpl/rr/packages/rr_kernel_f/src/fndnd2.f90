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
! at:               $Modtime:: 22-07-97 11:53a  $
!
! current revision: $Revision:: 3               $


      SUBROUTINE FNDND2 (INDEX, ID)
! *********************************************************************
! ***                D E L F T         H Y D R A U L I C S
! ***
! ***              WATER RESOURCES AND ENVIRONMENT DIVISION
! *********************************************************************
! *** Program :  DELFT_3B version 1.0                  Date: March 1996
! *********************************************************************
! *** Last update:  March 1996       By : Geert Prinsen
! *********************************************************************
! *********************************************************************
! *** Brief description:
! *** ------------------
! ***    Zoek string ID in array ID_NOD;
! ***    Voor testen eerst alles in UPPERCASE zetten
! ***    Gevonden op positie INDEX. (-1 = not found)
! *********************************************************************
!
      USE CONF_FIL
      USE CONF_ARR
      use Network

      use Hash

      Integer index
      LOGICAL allownotfound

      CHARACTER(CharIdLength) ID

!
!new: use hashing

      INDEX = -1
      ALLOWNOTFOUND = .TRUE.
      index = Hashsearch (id, ALLOWNOTFOUND)

      RETURN
      END
