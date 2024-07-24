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

      SUBROUTINE INITAR (NAME, RESNOW, RESULTS, NLOC, NHIS, NTIMS, IDEBUG)

! *********************************************************************
! ***                D E L F T         H Y D R A U L I C S
! ***
! ***              WATER RESOURCES AND ENVIRONMENT DIVISION
! *********************************************************************
! *** Program :  RTC  version 1.0.                   Date: June 1997
! *********************************************************************
! *** Last update: June   1997       By : Geert Prinsen
! *********************************************************************
! *** Brief description:
! *** ------------------
! ***   Initialise arrays (at -9999.)
! *********************************************************************
! *** Input/output parameters:
! *** ------------------------
! ***   NAME   = naam/indicatie soort resultaten (Sobek/3B/etc.)
! ***   RESNOW = array with results current timestep
! ***   RESULTS= array with results all timesteps
! ***   NLOC   = max. number of locations
! ***   NHIS   = max. number of series from HIS file
! ***   NTIMS  = max. number of timesteps
! ***   IDEBUG = unit nr. van debugfile
! *********************************************************************
!
      CHARACTER*6 NAME
      INTEGER     NLOC, NHIS, NTIMS, IDEBUG
      Double Precision RESNOW(NLOC,NHIS), RESULTS (NLOC,NHIS,NTIMS)
      INTEGER     ILOC, IPAR, IT
!
      IF (IDEBUG .GT. 0) WRITE (IDEBUG,1) NAME, NLOC, NHIS, NTIMS
    1 FORMAT (' Initar result arrays ',A,3I4)
!
! *********************************************************************
! ** Initialisatie Current and Previous results
! ********************************************************************

      DO ILOC=1,NLOC
        DO IPAR=1,NHIS
          RESNOW(ILOC,IPAR) = -9999.
          DO IT=1,NTIMS
            RESULTS(ILOC,IPAR,IT) = -9999.
          ENDDO
        ENDDO
      ENDDO


      RETURN
      END
