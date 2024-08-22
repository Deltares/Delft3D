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

      Function RDRAIN (IDEBUG, IN, IOUT1) result(RetVal)

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
! ***   Inlezen bui data
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
      
! Return Value 0=ok
      Integer :: RetVal

      INTEGER      FRSTTM, LASTT, IDEBUG, IN,IOUT1,IECODE, IDEFLT, IDUM, IPre, IT, I, IEVENT, ITMSTP, ILoc
      LOGICAL      ENDFIL
      CHARACTER*1   QUOTE
      CHARACTER*999 STRING


      RetVal = 0

      QUOTE = ''''
      IF (IDEBUG .GT. 0) WRITE (IDEBUG,1)
    1 FORMAT (' RDRAIN')

! *********************************************************************
! *** skip header of file
! *********************************************************************

      CALL SKPCOM (IN, ENDFIL, 'RTC')
      IF (ENDFIL) then
         CALL ERRMSG (911, 0, 'Rdrain', ' Precipitation file', IOUT1)
         Retval = 911
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

! Default initialisatie koppeling stations in rainfall file aan lokaties in precip lokatiefile; voor geval old format
      DO IPRE=1,NPRECP
         Loc2Stat(ipre) = ipre
      Enddo

      DO IDUM=1,NCSTAT
        READ(IN,'(A)',END=991,Err=991,IOSTAT=IECODE) STRING
        IF  (STRING(1:1) .NE. QUOTE) THEN
           WRITE(IOUT1,*) ' Rainfall file in old format?'
           WRITE(*,*) ' Rainfall file in old format?'
           GOTO 982
        ENDIF
        read(string,*,Err=150) NameStat(idum)
      ENDDO

! Zet definitieve koppeling stations aan precip data
      Do IPre = 1,NPrecp
         Loc2Stat(ipre) = -1
         DO IDUM = 1,NCSTAT
            if (NameStat(idum) .eq. Id_pre(ipre)) Loc2Stat(ipre) = idum
         Enddo
         if (loc2Stat(ipre) .eq. -1) then
            CALL ERRMSG (938, 0, '  Rdrain', Id_pre(ipre), IOUT1)
            Retval = 938
            Return
         Endif
      Enddo

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


! *********************************************************************
! *** Read date/time record for each event
! *********************************************************************

      DO 850 IEVENT=1,NEVENT

        CALL SKPCOM (IN, ENDFIL, 'RTC')
        IF (ENDFIL) then
           CALL ERRMSG (911, 0, '  Rdrain', STRING, IOUT1)
           Retval = 911
           Return
        Endif

        STRING = ' rainfall file'
        READ(IN,*,END=991,ERR=150,IOSTAT=IECODE) (EVSTRT(IEVENT,IT),IT=1,6), (EVDURA(IEVENT,I),I=1,4)
        IF (IDEBUG .GT. 0) THEN
           WRITE(IDEBUG,*) ' Start   ',(EVSTRT(IEVENT,I),I=1,6)
           WRITE(IDEBUG,*) ' Duration',(EVDURA(IEVENT,I),I=1,4)
        ENDIF

! ***   Bepaal aantal tijdstappen per event

        FRSTTM = 1
        LASTT  = EVDURA(IEVENT,1)*86400  + EVDURA(IEVENT,2)*3600 + &
                 EVDURA(IEVENT,3)*60     + EVDURA(IEVENT,4)

        TIMSTB = EVSTRT(IEVENT,1) * 10000. + EVSTRT(IEVENT,2) * 100. +  EVSTRT(IEVENT,3) + &
                    EVSTRT(IEVENT,4) / 100. + EVSTRT(IEVENT,5) / 10000.
!       IF (.NOT. TIMF3B) THEN
!          IF (TIMSTB .NE. RTC_TIMNEW) THEN
!             WRITE(IOUT1,*) ' Start rainfall and INI file inconsistent'
!             WRITE(*,*) ' Starttime rainfall and INI file inconsistent'
!             WRITE(IOUT1,*) ' This may cause strange results'
!             WRITE(*,*) ' This may cause strange results'
!          ENDIF
!       ENDIF
        LASTT  = LASTT  / NRSRAI
        EVTIMS(IEVENT) = LASTT

        IF (IDEBUG .GT. 0) WRITE(IDEBUG,*) ' Loc2Stat    ', (Loc2Stat(iloc),Iloc=1,nprecp)
        IF (IDEBUG .GT. 0) WRITE(IDEBUG,*) ' Buidata bui ', IEVENT
        DO ITMSTP=FRSTTM, LASTT
          READ(IN,*,END=991,ERR=150,IOSTAT=IECODE)  (BUIDAT(IEVENT,I,ITMSTP),I=1,NCSTAT)
          IF (IDEBUG .GT. 0) THEN
             WRITE(IDEBUG,*)   (BUIDAT(IEVENT,I,ITMSTP),I=1,NCSTAT)
          ENDIF
        ENDDO

  850 CONTINUE

      GOTO 999
!
! *********************************************************************
! *** Error during reading of file
! *********************************************************************
!
  991 CONTINUE
      CALL ERRMSG (911, IECODE, 'Rdrain', ' neerslag file', IOUT1)
      RetVal = 911
      Return
  150 CONTINUE
      CALL ERRMSG (902, IECODE, 'Rdrain', ' neerslag file', IOUT1)
      RetVal = 902

! *********************************************************************
! *** end of file
! *********************************************************************
!
  999 CONTINUE

      RETURN
      END Function RdRain
