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
! at:               $Modtime:: 15-08-97 11:26a  $
!
! current revision: $Revision:: 3               $


      SUBROUTINE SPLFIL (IN, STRING)

      use ReadLib
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
! ***   Spool input file to first record next event
! *********************************************************************
! *** Input/output parameters:
! *** ------------------------
! ***  IDEBUG = file unit number of debug file
! ***  IN     = file unit number of input file
! ***  IOUT1  = file unit number of output file with messages
! ***  STRING = character string used in error messages
! *********************************************************************

      USE CONF_FIL
      USE CONF_ARR
      use Messages

!
      INTEGER       IN
      LOGICAL       ENDFIL
      CHARACTER*(*) STRING
      Integer iDebug
!
      iDebug = ConfFil_get_iDebug()
      IF (iDebug /= 0) WRITE (IDEBUG,1)
    1 FORMAT (' SPLFIL')
!
! *********************************************************************
! *** spool file
! *********************************************************************
!
      CALL SKPCOM (IN, ENDFIL, 'ODS ')
      IF (ENDFIL) call ErrMsgStandard (911, 0, '  Splfil', STRING)
!
      RETURN
      END
