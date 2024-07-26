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

  Subroutine CheckMessages (makelogfile, Ievent, Itmstp, Iout1, MessageVolumeCheck, MessageInundation, MessagePerTimestep)

!*********************************************************************
!***                D E L F T         H Y D R A U L I C S
!***
!***              WATER RESOURCES AND ENVIRONMENT DIVISION
!*********************************************************************
!*** Program :  DELFT-3B version 2.00                Date: Nov   1996
!*** Module  :
!*********************************************************************
!*** Created    : March  1995                     By : Geert Prinsen
!*********************************************************************
!*** Last update: 19 February 1997                 By: Peter Schrier
!*********************************************************************
!*** Brief description:
!*** ------------------
!***    Hoofdmodule
!*********************************************************************
!*** Input/output parameters:     none.
!*** ------------------------
!*********************************************************************
!*** ADRESS     : Deltares,
!***              P.O.BOX 177,
!***              2600 MH DELFT, THE NETHERLANDS
!**********************************************************************

  USE CONF_FIL
  USE CONF_ARR
  USE Network
  use Structures

!*** OTHER DATA

  IMPLICIT NONE !!!!!!!!!

  INTEGER       makelogfile, Ievent, Itmstp, Iout1, MessageVolumeCheck, MessageInundation, MessagePerTimestep
  INTEGER       Inod, Istr

  CHARACTER(80)  STRING
  LOGICAL        NEGFLW



! Melding op severe maximum volumecheck
          if (SevereVolumeCheck .and. MessageVolumeCheck .ne. 0) then
             STRING(1:33) = ' Friction/Weir/Orifice '
             if (.not. MaxVolChkFrictionBoundary) String(1 :10) = ' '
             if (.not. MaxVolChkWeirBoundary)     String(11:15) = ' '
             if (.not. MaxVolChkOrificeBoundary)  String(16:22) = ' '
             call ErrMsgStandard (936, 0, ' Severe Volume Check flows to/from CF/SF boundaries', STRING(1:33))
          endif
          if (SimpleVolumeCheck .or. SevereVolumeCheck) NrVolumeCheck = NrVolumeCheck + 1

! Melding op inundaties
          IF (ConfArr_get_FLDOVH() .OR. ConfArr_get_FLDVHG() .OR. ConfArr_get_FLDKAS()) THEN
             ITIMEF = ITIMEF + 1
             ! Er wordt al of niet een melding per knoop gegeven; hier afhankelijk daarvan 1 melding
             If (iOut1 .ne. 0 .and. MessageInundation .eq. 0 .and. MessagePerTimestep .gt. 0) then
                WRITE(Iout1,'(A,2I5)')   &
                 ' Inundation paved and/or unpaved and/or greenhouse area in event/timestep', IEVENT, ITMSTP
             Endif
          ENDIF

! Message als negatief debiet over structure
          NEGFLW = .FALSE.
          DO INOD=1,NCNODE
             IF (EINODE(INOD,3) .EQ. 5) THEN
               ISTR = EINODE(INOD,2)
               IF (QSTRU(ISTR) .LT. .0) THEN
                 NEGFLW = .TRUE.
                 if (makelogfile .gt. 0) then
                    WRITE(IOUT1,'(A,2I5,A,A)') ' Negative flow in event/timestep', IEVENT,ITMSTP, &
                                               ' at structure ', Id_Nod(INOD)
                 end if
               ENDIF
             ENDIF
          ENDDO
          IF (NEGFLW .AND. IOPT1(5) .EQ. 1)  STOP 999


  Return
END Subroutine CheckMessages

