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

      Function ScanWind (IDEBUG, IN, IOUT1, IflRtnRtc) Result(RetVal)

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
! ***   Scan wind file om lengte tabellen te bepalen
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

      Integer    NHLP
      Parameter (NHLP=10)
!
      Integer      IDEBUG, IN, IOUT1, IflRtnRtc, IWIND, IECODE, INDX, J, IExt,ILoc
      Integer      IDUM(NHLP), LocalMxTimesteps, Ievent
      Real         RDUM(NHLP)
      Double Precision Ddum(NHLP)
      Double Precision TIMRD, TimStart  ! TimEnd al on OtherData module
      Logical      ENDFIL, ALLOW, FOUND, FoundTble
!      Character*32 CDUM(NHLP)
      Character(len=CharIdLength) :: CDUM(NHLP)
      Character*999               :: STRING


      RetVal = 0

      If (IDEBUG .GT. 0) Write (IDEBUG,1)
    1 Format (' ScanWind')
!
! *********************************************************************
! *** skip header of file
! *********************************************************************
!
      ALLOW  = .False.
      NTIMWV  = 1
      NTIMWD  = 1
      LocalMxTimesteps=NTim

      Call SKPCOM (IN, ENDFIL,'RTC')
      If (ENDFIL) then
         Call ERRMSG (911, 0, 'ScanWind', ' Constant wind file', IOUT1)
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
      If (IDEBUG .GT. 0)  Write(IDEBUG,*)  ' ScanWind'
! Initialisatie koppeling stations in wind file aan lokaties in Ext lokatiefile
      DO IExt=1,NExtd
         WindLoc2Stat(iext) = -1
      Enddo

      Do IWIND=1,NWIND
         Call SKPCOM (IN, ENDFIL,'RTC')
         If (ENDFIL) GOTO 21
         Found = .false.
         Do While (.not. found)
            Read(IN,'(A)',END=21,ERR=150,IOSTAT=IECODE)  STRING
! skip regel als hij niet begint met juist keyword: GLMT of MTEO of WSTA
            If (STRING(1:4) .EQ. 'GLMT') FOUND = .true.
            If (STRING(1:4) .EQ. 'MTEO') FOUND = .true.
            If (STRING(1:4) .EQ. 'WSTA') FOUND = .true.
         Enddo

! check dimensies
         If (IWIND .GT. NWIND) Then
            Call ERRMSG (913, 0,'ScanWind',' wind data lokaties',IOUT1)
            RetVal = 913
            Return
         EndIf
! end check

! Get wind station id from wind file
! NB In old versions wind files id might be an integer instead of character!!!
         RetVal = GetVar3 (STRING,' id ',1,' ScanWind',' Wind data file', &
                       IOUT1, CDUM(1), RDUM(1), IDUM(1), Ddum(1), ALLOW, FOUND, IflRtnRtc)
         If (RetVal .ne. 0) Return
         WindNameStat(IWind) = CDum(1)

! Zet WindData voor stations IExt
         Do IExt = 1,NExtd
            If (Cdum(1) .eq. Id_ext(iext)) then
               WindLoc2Stat(iext) = IWind
            Elseif (Cdum(1) .eq. '-1' .and. Id_ext(iext) .eq. 'Global wind') then
               WindLoc2Stat(iext) = IWind
            Elseif (Cdum(1) .eq. '0' .and. Id_ext(iext) .eq. 'Global wind') then
               WindLoc2Stat(iext) = IWind
            Elseif (Cdum(1) .eq. '-1' .and. Id_ext(iext) .eq. 'Public wind') then
               WindLoc2Stat(iext) = IWind
            Elseif (Cdum(1) .eq. '0' .and. Id_ext(iext) .eq. 'Public wind') then
               WindLoc2Stat(iext) = IWind
            Endif
         Enddo

! *********************************************************************
! ** Scan lengte tabel Wind velocity
! *********************************************************************
! eerste keer: string is al gevuld, dus niet nog eens lezen uit file
!              direct zoeken op keyword wv tv

         GOTO 1901
  190    Continue
         Read(IN,'(A)',END=21,ERR=150,IOSTAT=IECODE)  STRING
         If (IDEBUG .GT. 0) Write(IDEBUG,*)  STRING
 1901    Continue
         ALLOW = .True.
         RetVal = GetVar3 (STRING,'wv tv ',2,' ScanWind',' Wind data file', &
                       IOUT1, CDUM(1), RDUM(1), IDUM(1), Ddum(1), ALLOW, FOUND, IflRtnRtc)
         If (RetVal .ne. 0) Return
         If (.NOT. FOUND) GOTO 190
         RetVal = GetVrs3 (STRING,'wv tv ',2,' ScanWind',' Wind data file', &
                       IOUT1, CDUM(1), RDUM(1), IDUM(1), Ddum(1), 2, allow, found, IflRtnRtc)
         If (RetVal .ne. 0) Return
         If (IDEBUG .GT. 0) Write(IDEBUG,*)  ' Found wv tv', RDUM(1)
         If (RDUM(1) .EQ. 0) Then
! constante wind velocity; geen actie
         Else
! variabele wind velocity: read until TBLE is found
           FoundTble = .false.
           Do While (.not. FoundTble)
              Read(IN,'(A)',END=21,ERR=150,IOSTAT=IECODE)  STRING
              If (STRING(1:4) .EQ. 'TBLE') FoundTble = .true.
           EndDo
! read data until tble is found; check maximum nr of timestep per event
           Do IEvent=1,NEvent
            TimStart = EvStrt(Ievent,1)*10000. +  EvStrt(Ievent,2)*100. +  EvStrt(Ievent,3) + &
                       EvStrt(Ievent,4)/100. +  EvStrt(Ievent,5)/10000. +  EvStrt(Ievent,6)/1000000.
            Call SetTimEnd (TimEnd,Ievent, Idebug)
            INDX = 0
            FoundTble = .false.
            Do While (.not. FoundTble)
              Read(IN,'(A)',END=21,ERR=150,IOSTAT=IECODE)  STRING
              If (STRING(1:4) .EQ. 'tble') Then
                 FoundTble = .true.
              ElseIf (String(1:5) .ne. '     ') Then
                Read (STRING,192) (IDUM(J),J=1,5)
  192           Format (1X,I4,1X,I2,1X,I2,1X,I2,1X,I2)
                TIMRD = IDUM(1)*10000. +IDUM(2)*100.+IDUM(3)+ IDUM(4)/100.+IDUM(5)/10000.
                If (TIMRD .GE. RTC_TIMNEW .AND. TIMRD .LE. TIMEND) Then
                   INDX = INDX+1
                Endif
              Endif
              If (TIMRD .GT. TIMEND .AND. INDX .GT. 1) FoundTble= .true.
            EndDo
            NTIMWV = INDX
            LocalMxTimesteps=max(NtimWV,LocalMxTimesteps)
           Enddo
         EndIf

! *********************************************************************
! ** wind direction
! *********************************************************************
! eerste keer: string is al gevuld, dus niet nog eens lezen uit file
!              direct zoeken op keyword wd td

         GOTO 1951
  195    Continue
         Read(IN,'(A)',END=21,ERR=150,IOSTAT=IECODE)  STRING
!        If (IDEBUG .GT. 0) Write(IDEBUG,*)  STRING
 1951    Continue
         ALLOW = .True.
         RetVal = GetVar3 (STRING,'wd td ',2,' ScanWind',' Wind data file', &
                       IOUT1, CDUM(1), RDUM(1), IDUM(1), Ddum(1), ALLOW, FOUND, IflRtnRtc)
         If (RetVal .ne. 0) Return
         If (.NOT. FOUND) GOTO 195
         RetVal = GetVrs3 (STRING,'wd td ',2,' ScanWind',' Wind data file', &
                       IOUT1, CDUM(1), RDUM(1), IDUM(1), Ddum(1), 2, allow, found, IflRtnRtc)
         If (RetVal .ne. 0) Return

         If (IDEBUG .GT. 0) Write(IDEBUG,*)  ' Found wd td', RDUM(1)
         If (RDUM(1) .EQ. 0) Then
! constante wind direction, doe niets
         Else
! variabele wind direction; read until TBLE is found
           FoundTble = .false.
           Do While (.not. FoundTble)
              Read(IN,'(A)',END=21,ERR=150,IOSTAT=IECODE)  STRING
              If (STRING(1:4) .EQ. 'TBLE') FoundTble = .true.
           EndDo
! read data until tble is found
           Do Ievent=1,Nevent
            TimStart = EvStrt(Ievent,1)*10000. + EvStrt(Ievent,2)*100. +  EvStrt(Ievent,3) + &
                       EvStrt(Ievent,4)/100. + EvStrt(Ievent,5)/10000. + EvStrt(Ievent,6)/1000000.
            Call SetTimEnd (TimEnd,Ievent, Idebug)
            INDX = 0
            FoundTble = .false.
            Do While (.not. FoundTble)
              Read(IN,'(A)',END=21,ERR=150,IOSTAT=IECODE)  STRING
              If (STRING(1:4) .EQ. 'tble') Then
                 FoundTble = .true.
              ElseIf (String(1:5) .ne. '     ') Then
                Read (STRING,192) (IDUM(J),J=1,5)
                TIMRD = IDUM(1)*10000. +IDUM(2)*100.+IDUM(3)+ IDUM(4)/100.+IDUM(5)/10000.
                If (TIMRD .GE. RTC_TIMNEW .AND. TIMRD .LE. TIMEND) Then
                  INDX = INDX+1
                Endif
              Endif
              If (TIMRD .GT. TIMEND .AND. INDX .GT. 1) FoundTble= .true.
            EndDo
            NTIMWD = INDX
            LocalMxTimesteps=max(NtimWD,LocalMxTimesteps)
           EndDo
         EndIf
      Enddo
   21 Continue

      GOTO 999
!
! *********************************************************************
! *** Error during reading of file
! *********************************************************************
!
  150 Continue
      Call ERRMSG (902, IECODE, 'ScanWind', ' wind data file', IOUT1)
      RetVal = 902

! *********************************************************************
! *** end of file
! *********************************************************************
!
  999 Continue

      NTimW = Max (NTim, LocalMxTimesteps)
      If (Idebug .gt. 0) Then
         Write(idebug,*) ' Wind station names'
         Do ILoc = 1,NWind
            Write(idebug,*) ILoc, WindNameStat(Iloc)
         Enddo
         Write(idebug,*) ' External names and wind stations'
         Do IExt = 1,NExtd
            Write(idebug,*) Iext, ID_Ext(Iext), WindLoc2Stat(iext)
         Enddo
      Endif
      Do IExt = 1,NExtd
         if (WindLoc2Stat(iext) .eq. -1) CALL ERRMSG (942, 0, ' ScanWind', Id_ext(iext), IOUT1)
      Enddo

      RETURN
      END Function ScanWind
