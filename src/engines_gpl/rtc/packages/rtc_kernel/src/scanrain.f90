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

      Function ScanRain (IDEBUG, IN, IOUT1) Result(RetVal)

! *********************************************************************
! ***                D E L F T         H Y D R A U L I C S
! ***
! ***              WATER RESOURCES AND ENVIRONMENT DIVISION
! *********************************************************************
! *** Program :  RTC version 1.0.                    Date: June  1997
! *********************************************************************
! *** Last update: June  1997        By : Geert Prinsen
! *********************************************************************
! *** Brief description:
! *** ------------------
! ***   Scan rainfall file and set dimensions
! *********************************************************************
! *** Input/output parameters:
! *** ------------------------
! ***  IDEBUG = file unit number of debug file
! ***  IN     = file unit number of input file
! ***  IOUT1  = file unit number of output file with messages
! *********************************************************************

      Use ParameterModule
      Use LocationDataModule
      Use OtherData
      Use ReadLib

      implicit none
      
      Integer :: RetVal

      Double Precision Rdum
      INTEGER      FRSTTM, LASTT, IDEBUG, IN,IOUT1,IECODE, IDEFLT, IDUM, IT, I, IEVENT, ITMSTP
      LOGICAL      ENDFIL
      CHARACTER*1   QUOTE
      CHARACTER*999 STRING

      INTEGER  LocalEvStrt (6), LocalEvDuration(4), LocalMxTimesteps


      RetVal = 0

      QUOTE = ''''
      IF (IDEBUG .GT. 0) WRITE (IDEBUG,1)
    1 FORMAT (' ScanRain')
!
! *********************************************************************
! *** skip header of file
! *********************************************************************
!
      CALL SKPCOM (IN, ENDFIL, 'RTC')
      IF (ENDFIL) then
         Call ERRMSG (911, 0, 'ScanRain', ' Precipitation file', IOUT1)
         RetVal = 911
         Return
      Endif

! *********************************************************************
! *** Read bui file (routine based on 3B routines)
! *********************************************************************

      CALL SKPCOM (IN, ENDFIL, 'RTC')
      READ(IN,*,END=991,ERR=991,IOSTAT=IECODE) IDEFLT
      CALL SKPCOM (IN, ENDFIL, 'RTC')
      READ(IN,*,END=991,ERR=981,IOSTAT=IECODE) NCSTAT
      CALL SKPCOM (IN, ENDFIL, 'RTC')

      DO IDUM=1,NCSTAT
        READ(IN,'(A)',END=991,IOSTAT=IECODE) STRING
        IF  (STRING(1:1) .NE. QUOTE) THEN
           WRITE(IOUT1,*) ' Rainfall file in old format?'
           WRITE(*,*) ' Rainfall file in old format?'
           GOTO 982
        ENDIF
      ENDDO

      GOTO 9900
  981 CONTINUE
      BACKSPACE(IN)
      GOTO 9900
  982 CONTINUE
 9900 CONTINUE
! nr. events, rainfall timestep size
      CALL SKPCOM (IN, ENDFIL, 'RTC')
      NRSRAI = 3600
      READ(IN,*,END=991,ERR=990,IOSTAT=IECODE) NEVENT, NRSRAI
      IF (IDEBUG .GT. 0) WRITE (IDEBUG,*) ' Nr. events:',NEVENT,NRSRAI
      GOTO 9901
  990 CONTINUE
      BACKSPACE(IN)
 9901 CONTINUE

! Set Dimensions for Number of Events, number of Stations

      NEVNT = Nevent
      NSTAT = NcStat

      LocalMxTimesteps = 1

! *********************************************************************
! *** Determine max. number of timesteps in a rainfall event
! *********************************************************************

      DO IEVENT=1,NEVENT

        CALL SKPCOM (IN, ENDFIL, 'RTC')
        IF (ENDFIL) then
           CALL ERRMSG (911, 0, '  ScanRain', STRING, IOUT1)
           RetVal = 911
           Return
        Endif

        STRING = ' rainfall file'
        READ(IN,*,END=991,ERR=150,IOSTAT=IECODE) (LocalEvStrt(IT),IT=1,6), (LocalEvDuration(I),I=1,4)

! ***   Bepaal aantal tijdstappen per event

        FRSTTM = 1
        LASTT  = LocalEvDuration(1)*86400  + LocalEvDuration(2)*3600 + &
                                   LocalEvDuration(3)*60     + LocalEvDuration(4)

        TIMSTB = LocalEvStrt(1) * 10000. + LocalEvStrt(2) * 100. + LocalEvStrt(3) + &
                    LocalEvStrt(4) / 100. + LocalEvStrt(5) / 10000.
        LASTT  = LASTT  / NRSRAI
        LocalMxTimesteps = max (LastT, LocalMxTimesteps)
        DO ITMSTP=FRSTTM, LASTT
          READ(IN,*,END=991,ERR=150,IOSTAT=IECODE)  rdum
        ENDDO
      Enddo

      GOTO 999
!
! *********************************************************************
! *** Error during reading of file
! *********************************************************************
!
  991 CONTINUE
      CALL ERRMSG (911, IECODE, 'ScanRain', ' neerslag file', IOUT1)
      RetVal = 911
      Return
  150 CONTINUE
      CALL ERRMSG (902, IECODE, 'ScanRain', ' neerslag file', IOUT1)
      RetVal = 902

! *********************************************************************
! *** end of file
! *********************************************************************
!
  999 CONTINUE

! Set max. number of rainfall timesteps
      NTIM = LocalMxTimesteps


      RETURN
      END Function Scanrain
