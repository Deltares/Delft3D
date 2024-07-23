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

      Function RDWIND (IDEBUG, IN, IOUT1, IflRtnRtc) result(RetVal)

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
! ***   Inlezen wind data
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

      INTEGER    NHLP
      PARAMETER (NHLP=10)
!
      INTEGER      IDEBUG, IN, IOUT1, IflRtnRtc, IWIND, IECODE, INDX, I,J, IT
      INTEGER      IDUM(NHLP), POS1
      REAL         RDUM(NHLP)
      Double Precision Ddum(NHLP)
      Double Precision TIMRD
      LOGICAL      ENDFIL, ALLOW, FOUND, FoundTble, FoundPdin
      
      CHARACTER(len=CharIdLength) :: CDUM(NHLP)
      CHARACTER*999               :: STRING


      RetVal = 0

      If (IDEBUG .GT. 0) Write (IDEBUG,1)
    1 FORMAT (' Rdwind')
!
! *********************************************************************
! *** skip header of file
! *********************************************************************
!
      ALLOW  = .False.
      NTIMWV  = 1
      NTIMWD  = 1
      Do IWind=1,NWind
         VALCWD(Iwind) = 0
         VALCWV(IWind) = 0
         ConsWD(IWind) = .False.
         ConsWV(IWind) = .False.
         WindInterpVel(iwind) = .False.
         WindInterpDir(iwind) = .False.
      Enddo

      Call SKPCOM (IN, ENDFIL,'RTC')
      If (ENDFIL) then
         Call ERRMSG (911, 0, 'RDWIND', ' Constant wind file', IOUT1)
         RetVal = 911
         Return
      Endif
!
! *********************************************************************
! *** read data
! *** format:     GLMT MTEO  ss id ci lc wu  wv tv wd td
! *** etc.
! *** bij constante wind: wv tv 0 getal 9999   (velocity)
! *** bij constante wind: wd td 0 getal 9999   (direction)
! ***
! *** bij variabele wind: wv tv 1 ...          (velocity)
! ***                      TBLE
! ***                         data records (1 per regel)
! ***                         'yyyy/mm/dd;hh:mm:ss' value <
! ***                      tble
! *** bij variabele wind: wd td 1 ...          (direction)
! ***                      TBLE
! ***                         data records (1 per regel)
! ***                         'yyyy/mm/dd;hh:mm:ss' value <
! ***                      tble
! *** Aannamen: slechts 1 waarde per regel;
! ***           TBLE en tble op aparte regels.
! *** NB kan nu tegen lege regels (Rijnland 51_97.WRS file)
! *********************************************************************
!
      If (IDEBUG .GT. 0)  Write(IDEBUG,*)  ' Time:', RTC_TIMNEW, TIMEND
!     Write(*,*)  ' Rdwind Time:', RTC_TIMNEW, TIMEND

      Do IWIND=1,NWIND
         If (IDEBUG .GT. 0)  Write(IDEBUG,*)  ' Iwind=',Iwind
         Call SKPCOM (IN, ENDFIL,'RTC')
         If (ENDFIL) GoTo 21
         Found = .false.
         Do While (.not. found)
            Read(IN,'(A)',END=21,ERR=150,IOSTAT=IECODE)  STRING
!           Write(*,*)  ' Rdwind Read:', String(1:80)
! skip regel als hij niet begint met juist keyword
            If (STRING(1:4) .EQ. 'GLMT') FOUND = .true.
            If (STRING(1:4) .EQ. 'MTEO') FOUND = .true.
            If (STRING(1:4) .EQ. 'WSTA') FOUND = .true.
         Enddo

! check dimensies
         If (IWIND .GT. NWIND) Then
            Call ERRMSG (913, 0,'Rdwind',' wind data lokaties',IOUT1)
            RetVal = 913
            Return
         EndIf
! end check

! *********************************************************************
! ** Wind velocity
! *********************************************************************
! eerste keer: string is al gevuld, dus niet nog eens lezen uit file
!              direct zoeken op keyword wv tv

         GoTo 1901
  190    Continue
         Read(IN,'(A)',END=21,ERR=150,IOSTAT=IECODE)  STRING
!        Write(*,*)  ' Rdwind Read at 190:', String(1:80)
!        If (IDEBUG .GT. 0) Write(IDEBUG,*)  STRING
 1901    Continue
         ALLOW = .True.
         RetVal = GETVAR3 (STRING,'wv tv ',2,' Rdwind',' Wind data file', &
                       IOUT1, CDUM(1), RDUM(1), IDUM(1), DDum(1), ALLOW, FOUND, IflRtnRtc)
         If (RetVal .ne. 0) Return
         If (.NOT. FOUND) GoTo 190
         RetVal = GETVRS3 (STRING,'wv tv ',2,' Rdwind',' Wind data file', &
                       IOUT1, CDUM(1), RDUM(1), IDUM(1), DDum(1), 2, allow, found, IflRtnRtc)
         If (RetVal .ne. 0) Return
         If (IDEBUG .GT. 0) Write(IDEBUG,*)  ' Found wv tv', RDUM(1)
         If (RDUM(1) .EQ. 0) Then
! constante wind velocity
            CONSWV(Iwind) = .True.
            VALCWV(Iwind) = RDUM(2)
         ELSE
! variabele wind velocity
            CONSWV(Iwind) = .FALSE.
! read until PDIN or TBLE is found
            FoundPdin = .false.
            ALLOW = .True.
            RetVal = GETVAR3 (STRING,'PDIN ',3,' Rdwind',' Wind data file', &
                          IOUT1, CDUM(1), RDUM(1), IDUM(1), DDum(1), ALLOW, FoundPdin, IflRtnRtc)
            If (RetVal .ne. 0) Return
            If (FoundPdin) WindInterpVel(iwind) = (Idum(1) .eq. 0)
            FoundTble = .false.
            Do While (.not. FoundTble)
              Read(IN,'(A)',END=21,ERR=150,IOSTAT=IECODE)  STRING
!             Write(*,*)  ' Rdwind Read in loop FoundTbl:', String(1:80)
              ALLOW = .True.
              RetVal = GETVAR3 (STRING,'PDIN ',3,' Rdwind',' Wind data file', &
                            IOUT1, CDUM(1), RDUM(1), IDUM(1), DDum(1), ALLOW, FoundPdin, IflRtnRtc)
              If (RetVal .ne. 0) Return
              If (FoundPdin) WindInterpVel(iwind) = (Idum(1) .eq. 0)
              FoundPdin = .false.
              If (STRING(1:4) .EQ. 'TBLE') FoundTble = .true.
            ENDDo
! read data until tble is found
            INDX = 0
            FoundTble = .false.
            Do While (.not. FoundTble)
              Read(IN,'(A)',END=21,ERR=150,IOSTAT=IECODE)  STRING
!             Write(*,*)  ' Rdwind Read in loop FoundTbl tble:', String(1:80)
              If (STRING(1:4) .EQ. 'tble') Then
                 FoundTble = .true.
              ElseIf (String(1:5) .ne. '     ') Then
                Read (STRING,192) (IDUM(J),J=1,5)
  192           FORMAT (1X,I4,1X,I2,1X,I2,1X,I2,1X,I2)
! ARS XXXX May 21, 2002: check correctness date/time; IHour =0..23, IMinute = 0..59
                If (Idum(5) .ge. 60) then
                    IDum(5) = 0
                    IDum(4) = IDUm(4) + 1
                Endif
                If (Idum(4) .ge. 24) then
                   IDum(4) = 0
                   Call NxtDay (Idebug, Idum(1), Idum(2), Idum(3))
                Endif
! End ARS
                TIMRD = IDUM(1)*10000. +IDUM(2)*100.+IDUM(3)+ IDUM(4)/100.+IDUM(5)/10000.
!               If (IDEBUG .GT. 0) Write(IDEBUG,*)  ' Time:', TIMRD, RTC_TIMNEW, TIMEND
                If (TIMRD .GE. RTC_TIMNEW .AND. TIMRD .LE. TIMEND) then
                  INDX = INDX+1
                  If (INDX .GT. NTIMW) Then
                     Call ERRMSG (913, 0,'Rdwind',' wind tijdserie',IOUT1)
                     RetVal = 913
                     Return
                  EndIf
                  WNDVEL(IWIND,INDX,1) = TIMRD
!                 bepaal positie van < teken om zeker te weten dat free format lezen goed gaat
                  POS1 = INDEX(STRING, '<')
                  If (POS1 .LE. 0) GoTo 150
                  Read (STRING(22:POS1-1),*,Err=150)  WNDVEL(IWIND,INDX,2)
                Endif
              Endif
            EndDo
            NTIMWV = INDX
            SearchFromIndex(IWind,2) = 1
            SearchToIndex(IWind,2) = NTimWV
         EndIf

! *********************************************************************
! ** wind direction
! *********************************************************************
! eerste keer: string is al gevuld, dus niet nog eens lezen uit file
!              direct zoeken op keyword wd td

         GoTo 1951
  195    Continue
         Read(IN,'(A)',END=21,ERR=150,IOSTAT=IECODE)  STRING
!        Write(*,*)  ' Rdwind Read at 195:', String(1:80)
!        If (IDEBUG .GT. 0) Write(IDEBUG,*)  STRING
 1951    Continue
         ALLOW = .True.
         RetVal = GETVAR3 (STRING,'wd td ',2,' Rdwind',' Wind data file', &
                       IOUT1, CDUM(1), RDUM(1), IDUM(1), DDum(1), ALLOW, FOUND, IflRtnRtc)
         If (RetVal .ne. 0) Return
         If (.NOT. FOUND) GoTo 195
         RetVal = GETVRS3 (STRING,'wd td ',2,' Rdwind',' Wind data file', &
                       IOUT1, CDUM(1), RDUM(1), IDUM(1), DDum(1), 2, allow, found, IflRtnRtc)
         If (RetVal .ne. 0) Return

         If (IDEBUG .GT. 0) Write(IDEBUG,*)  ' Found wd td', RDUM(1)
         If (RDUM(1) .EQ. 0) Then
! constante wind direction
            CONSWD(Iwind) = .True.
            VALCWD(Iwind) = RDUM(2)
         ELSE
! variabele wind direction
            CONSWD(Iwind) = .FALSE.
! read until PDIN or TBLE is found
            FoundPdin = .false.
            ALLOW = .True.
            RetVal = GETVAR3 (STRING,'PDIN ',3,' Rdwind',' Wind data file', &
                          IOUT1, CDUM(1), RDUM(1), IDUM(1), DDum(1), ALLOW, FoundPdin, IflRtnRtc)
            If (RetVal .ne. 0) Return
            If (FoundPdin) WindInterpDir(iwind) = (Idum(1) .eq. 0)
            FoundTble = .false.
            Do While (.not. FoundTble)
              Read(IN,'(A)',END=21,ERR=150,IOSTAT=IECODE)  STRING
!             Write(*,*)  ' Rdwind Read in loop FoundTbl:', String(1:80)
              ALLOW = .True.
              RetVal = GETVAR3 (STRING,'PDIN ',3,' Rdwind',' Wind data file', &
                            IOUT1, CDUM(1), RDUM(1), IDUM(1), DDum(1), ALLOW, FoundPdin, IflRtnRtc)
              If (RetVal .ne. 0) Return
!             Write(*,*) ' FoundPdIn direction = ', FoundPdIn
              If (FoundPdin) WindInterpDir(iwind) = (Idum(1) .eq. 0)
              FoundPdin = .false.
              If (STRING(1:4) .EQ. 'TBLE') FoundTble = .true.
            ENDDo
! read data until tble is found
            INDX = 0
            FoundTble = .false.
            Do While (.not. FoundTble)
              Read(IN,'(A)',END=21,ERR=150,IOSTAT=IECODE)  STRING
!             Write(*,*)  ' Rdwind Read in loop FoundTbl tble:', String(1:80)
              If (STRING(1:4) .EQ. 'tble') Then
                 FoundTble = .true.
              ElseIf (String(1:5) .ne. '     ') Then
                Read (STRING,192) (IDUM(J),J=1,5)
! ARS XXXX May 21, 2002: check correctness date/time; IHour =0..23, IMinute = 0..59
                If (Idum(5) .ge. 60) then
                    IDum(5) = 0
                    IDum(4) = IDUm(4) + 1
                Endif
                If (Idum(4) .ge. 24) then
                   IDum(4) = 0
                   Call NxtDay (Idebug, Idum(1), Idum(2), Idum(3))
                Endif
! End ARS
                TIMRD = IDUM(1)*10000. +IDUM(2)*100.+IDUM(3)+ IDUM(4)/100.+IDUM(5)/10000.
                If (TIMRD .GE. RTC_TIMNEW .AND. TIMRD .LE. TIMEND) then
                  INDX = INDX+1
                  If (INDX .GT. NTIMW) Then
                     Call ERRMSG (913, 0,'Rdwind',' wind tijdserie',IOUT1)
                     RetVal = 913
                     Return
                  EndIf
                  WNDDIR(IWIND,INDX,1) = TIMRD
!                 bepaal positie van < teken om zeker te weten dat free format lezen goed gaat
                  POS1 = INDEX(STRING, '<')
                  If (POS1 .LE. 0) GoTo 150
                  Read (STRING(22:POS1-1),*,Err=150)  WNDDIR(IWIND,INDX,2)
                Endif
                If (TIMRD .GT. TIMEND .AND. idebug .gt. 0)  write(idebug,*) ' goto 1961; set foundtble=true'
                If (TIMRD .GT. TIMEND .AND. INDX .GT. 1) FoundTble = .true.
              Endif
            EndDo
            NTIMWD = INDX
            SearchFromIndex(IWind,1) = 1
            SearchToIndex(IWind,1) = NTimWD  !! er stond nog foutief NTIMWV  op 28 aug 2000
         EndIf
      Enddo
   21 Continue

! *********************************************************************
! *** Debug
! *********************************************************************

      If (IDEBUG .GT. 0) Then
        Write(IDEBUG,*)  ' Time:', RTC_TIMNEW, TIMEND
        Do IWIND=1,NWIND
           Write(IDEBUG,*) WindNameStat(IWIND)
           If (CONSWD(Iwind)) Then
             Write(IDEBUG,*) ' Constant direction', VALCWD(Iwind)
           ELSE
             Write(IDEBUG,*) ' Variable direction: time & value   Interpolation code '
             Do IT=1,NTIMWD
                Write(IDEBUG,*) IT, (WNDDIR(IWIND,IT,I),I=1,2), WindInterpDir(Iwind)
             EndDo
           EndIf
           If (CONSWV(Iwind)) Then
             Write(IDEBUG,*) ' Constant velocity',CONSWV, VALCWV
           ELSE
             Write(IDEBUG,*) ' Variable velocity:  time & value   Interpolation code '
             Do IT=1,NTIMWV
                Write(IDEBUG,*) IT, (WNDVEL(IWIND,IT,I),I=1,2), WindInterpVEL(Iwind)
             EndDo
           EndIf
        EndDo
      EndIf
      GoTo 999
!
! *********************************************************************
! *** Error during reading of file
! *********************************************************************
!
  150 Continue
      Call ERRMSG (902, IECODE, 'Rdwind', ' wind data file', IOUT1)
      RetVal = 902

! *********************************************************************
! *** end of file
! *********************************************************************
!
  999 Continue

      RETURN
      END Function RdWind
