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

      SUBROUTINE UPD_AR (NAME, RESNOW, RESULTS, ID, NLOCA, &
                         NLOC, NHIS, NTIMS, IDEBUG, MxShift)

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
! ***   Update arrays with results in every timestep
! *********************************************************************
! *** Input/output parameters:
! *** ------------------------
! ***   NAME   = name/indicatie soort results (Sobek/3B/etc)
! ***   RESNOW = results current timestep
! ***   RESULTS= results all timesteps
! ***   ID     = id's of all locations
! ***   NLOCA  = actual nr. of locations volgens ASCII input file
! ***   NLOC   = max. number of locations
! ***   NHIS   = max. number of series
! ***   NTIMS  = max. number of timesteps
! ***   IDEBUG = unit nr. debug file
! ***   MxShift = maximale verschuiving (geheugen) nodig
! *********************************************************************
!
      CHARACTER(*) NAME
      INTEGER      NLOC, NHIS, NTIMS, NLOCA, IDEBUG, MxShift
      Double Precision  RESNOW(NLOC,NHIS), RESULTS (NLOC,NHIS,NTIMS)
!      CHARACTER*32 ID(NLOC)
      CHARACTER*256 ID(NLOC)
      INTEGER      ILOC, IPAR, IT, ITeller
!
      IF (IDEBUG .GT. 0) WRITE (IDEBUG,1) NAME
    1 FORMAT (' Update/timeshift result arrays ',A)
!
! *********************************************************************
! ** Time shift previous results one timestep
! ** Store results of current timestep also in overall result array
! ********************************************************************

      DO ILOC=1,NLOC
        DO IPAR=1,NHIS
!          DO IT=1,NTIMS-1
          DO IT=NTIMS+MxShift-1, NTIMS-1
            RESULTS(ILOC,IPAR,IT) = RESULTS(ILOC,IPAR,IT+1)
          ENDDO
          RESULTS(ILOC,IPAR,NTIMS) = RESNOW(ILOC,IPAR)
        ENDDO
      ENDDO

! *********************************************************************
! *** Debug
! *********************************************************************

      IF (IDEBUG .GT. 0) THEN
        WRITE(IDEBUG,*) ' Na time shift arrays', NAME
        WRITE(IDEBUG,'(15X,A2,15X,A5,1X,A21)') ' id','tijd ','  parameter1   2  etc'
        ITeller = min (3, NTims)
        DO ILOC=1,NLOCA
           WRITE(IDEBUG,'(A32,A5,1X,999F10.3)') ID(ILOC), ' t   ', (RESNOW(ILOC,IPAR),IPAR=1,NHIS)
           DO IT=1,NTIMS
             IF (IT .LE. ITeller .OR. IT .GT. NTIMS-iTeller) &
                  wrITE(IDEBUG,'(A32,A3,I3,1X,I3,999F10.3)') &
                        ID(ILOC), ' t-',NTIMS-IT,IT, (RESULTS(ILOC,IPAR,IT),IPAR=1,NHIS)
           ENDDO
        ENDDO
      ENDIF


      RETURN
      END
