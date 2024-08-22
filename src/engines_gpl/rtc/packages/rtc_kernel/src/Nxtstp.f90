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

      SUBROUTINE NXTSTP (IDEBUG, IYEAR, IMO, IDAY, IHOUR, IMIN, ISEC, RSEC, IDHR, IDMIN, RDSEC)

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
! ***   Zet nieuwe waarden tijdstap
! *********************************************************************
!
      INTEGER IDEBUG, IYEAR, IMO, IDAY, IHOUR, IMIN, IDHR, IDMIN, ISEC
      DOUBLE PRECISION RSEC, RDSEC
!
      IF (IDEBUG .GT. 0) WRITE (IDEBUG,1)
    1 FORMAT (' NXTSTP')
!
      IHOUR = IHOUR + IDHR
      IMIN  = IMIN  + IDMIN
      RSEC  = RSEC  + RDSEC
      IF (RSEC .GE. 60D0) THEN
         RSEC = RSEC - 60D0
         IMIN = IMIN + 1
      ENDIF
      ISEC = INT(RSEC)
      IF (IMIN .GE. 60) THEN
         IMIN = IMIN - 60
         IHOUR= IHOUR + 1
      ENDIF
      IF (IHOUR .GE. 24) THEN
         IHOUR = IHOUR - 24
         CALL NXTDAY (IDEBUG, IYEAR, IMO, IDAY)
      ENDIF
!
      RETURN
      END
