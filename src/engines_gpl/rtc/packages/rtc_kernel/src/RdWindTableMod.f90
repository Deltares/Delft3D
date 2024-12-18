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

      Function RdWindTableMod (IDEBUG, IN, IOUT1) result(RetVal)

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
      Use NewTables
      Use ReadLib

      implicit none
      
      Integer :: RetVal

      INTEGER    NHLP
      PARAMETER (NHLP=10)
!
      INTEGER      IDEBUG, IN, IOUT1, IWIND, IECODE
      Integer      NrColumns, TableNr, ILeftBuf, iRightBuf
      REAL         RDUM(NHLP)
      Double Precision TimeLocal
      LOGICAL      ENDFIL, ALLOW, FOUND, TabYesNo, success
!      Character*32 TableName
      Character(len=CharIdLength) :: TableName
!
      CHARACTER*4   KeyWrd
      CHARACTER(len=9999) STRING
      type (Date)   StartDate, EndDate
      type (Time)   StartTime, EndTime

      RetVal = 0

      If (IDEBUG .GT. 0) Write (IDEBUG,1)
    1 FORMAT (' RdWindTableMod')
!
      StartDate%year   = EvStrt(1,1)
      StartDate%month  = EvStrt(1,2)
      StartDate%day    = EvStrt(1,3)
      StartTime%hour   = EvStrt(1,4)
      StartTime%minute = EvStrt(1,5)
      StartTime%second = EvStrt(1,6)
!
      TimeLocal      = TimEnd
      EndDate%year   = TimEnd / 10000
      TimeLocal      = (TimeLocal - 10000 * EndDate%Year)
      EndDate%month  = TimeLocal / 100
      TimeLocal      = (TimeLocal - 100   * EndDate%Month)
      EndDate%day    = TimeLocal
      TimeLocal      = TimeLocal - EndDate%Day
      EndTime%hour   = TimeLocal * 100
      TimeLocal      = TimeLocal - 0.01 * EndTime%Hour
      EndTime%minute = TimeLocal * 10000
      EndTime%second = 0
! ARS XXXX May 21, 2002: check correctness date/time; IHour =0..23, IMinute = 0..59
      If (EndTime%Minute .ge. 60) then
          EndTime%Minute = 0
          EndTime%Hour = EndTime%Hour + 1
      Endif
      If (EndTime%hour .ge. 24) then
         EndTime%hour = 0
         Call NxtDay (Idebug, EndDate%Year, EndDate%Month, EndDate%Day)
      Endif
! End ARS

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
         call write_error_message_rtc (911, 0, 'RdWindTableMod', ' Constant wind file', IOUT1)
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
!     Write(*,*)  ' RdWindTableMod Time:', RTC_TIMNEW, TIMEND

      ALLOW = .false.
      IWind = 0
      Endfil = .false.
      Call SKPCOM (IN, ENDFIL,'RTC')
      Do While (.not. endfil)
  102    Continue
         Read(IN,'(A)',END=21,ERR=150,IOSTAT=IECODE)  STRING
         If (STRING(1:4) .EQ. 'GLMT' .or. STRING(1:4) .EQ. 'MTEO' .or. STRING(1:4) .EQ. 'WSTA') Then
             FOUND = .true.
             KeyWrd = String(1:4)
         Else
             Goto 102
         Endif
         BackSpace(In)
         success = GetRecord (In, KeyWrd, Endfil, Idebug, IOut1)
         iWind = IWind + 1
         If (IWIND .GT. NWIND .or. .not. success) Then
            call write_error_message_rtc (913, 0,'RdWindTableMod',' wind data lokaties',IOUT1)
            RetVal = 913
            Return
         EndIf
         Success = GetStringFromBuffer (String)
!     Get wind velocity data
         ILeftBuf = Index(String(1:nbuf), ' wv tv ')
         If (ILeftBuf .le. 0 .or. .not. success) then
            call write_error_message_rtc (913, 0,'RdWindTableMod',' Error finding wv tv string',IOUT1)
            RetVal = 913
            Return
         Endif
         Read(String(IleftBuf+6:),*,Err=150) Rdum(1), Rdum(2)
         If (IDEBUG .GT. 0) Write(IDEBUG,*)  ' Found wv tv', RDUM(1)
         If (RDUM(1) .EQ. 0) Then
! constante wind velocity
            CONSWV(Iwind) = .True.
            VALCWV(Iwind) = RDUM(2)
         ELSE
! variabele wind velocity
            CONSWV(Iwind) = .FALSE.
            success = GetTableName (TabYesNo, TableName, ' id ', Iout1)
            If (.not. success) then
               call write_error_message_rtc (913, 0,'RdWindTableMod',' Error getting table name',IOUT1)
               RetVal = 913
               Return
            Endif
            If (TabYesNo .and. TableName .ne. '') Then
               NrColumns = 1
!      Get table with name TableName, Nrcolumns data fields, result in global arrays; tabel nummer is TableNr
!      Only use part of buffer
!       usually wv tv is before wd td; in that case use wv tv up to wd td, else from wv tv to end
               ILeftBuf = Index(String(1:nbuf), ' wv tv ')
               IrightBuf = Index(String(1:nbuf), ' wd td ')
               If (ILeftBuf .gt. IRightBuf) IRightBuf = nBuf
               Success = GetTableFromBuffer (RtcTableHandle, TableName, NrColumns, TableNr, idebug, Iout1, ILeftBuf, IRightBuf)
               If (.not. success) then
                  call write_error_message_rtc (913, 0,'RdWindTableMod',' Error getting table data',IOUT1)
                  RetVal = 913
                  Return
               Endif
!      Set references
               WindTable(iWind,1) = TableNr
!              Try to reduce table length
               If (ReduceWindTable) success= ReduceTable(RtcTableHandle, TableNr, NrColumns, StartDate, StartTime, &
                                                         EndDate, EndTime, Idebug, Iout1)
               If (.not. success) then
                  call write_error_message_rtc (913, 0,'RdWindTableMod',' Error Reducing Table ',IOUT1)
                  RetVal = 913
                  Return
               Endif
            Endif
         Endif
! ** wind direction
         ILeftBuf = Index(String(1:nbuf), ' wd td ')
         If (ILeftBuf .le. 0) Then
            call write_error_message_rtc (913, 0,'RdWindTableMod',' Error finding wd td string',IOUT1)
            RetVal = 913
            Return
         Endif
         Read(String(IleftBuf+6:),*,Err=150) Rdum(1), Rdum(2)
         If (IDEBUG .GT. 0) Write(IDEBUG,*)  ' Found wd td', RDUM(1)
         If (RDUM(1) .EQ. 0) Then
! constante wind direction
            CONSWD(Iwind) = .True.
            VALCWD(Iwind) = RDUM(2)
         ELSE
! variabele wind direction
            CONSWD(Iwind) = .FALSE.
            Success = GetTableName (TabYesNo, TableName, ' id ', Iout1)
            If (.not. success) then
               call write_error_message_rtc (913, 0,'RdWindTableMod',' Error getting table name',IOUT1)
               RetVal = 913
               Return
            Endif
            If (TabYesNo .and. TableName .ne. '') Then
               NrColumns = 1
!      Get table with name TableName, Nrcolumns data fields, result in global arrays; tabel nummer is TableNr
!      Only use part of buffer
!       usually wv tv is before wd td; in that case use wd td up to end, else from wd td to wv tv
               ILeftBuf = Index(String(1:nbuf), ' wd td ')
               IRightBuf = Index(String(1:nbuf), ' wv tv ')
               If (ILeftBuf .gt. IRightBuf) then
                  iRightBuf = nBuf
               Else
                  Call SwapInt (IleftBuf,IRightBuf)
               Endif
               Success = GetTableFromBuffer (RtcTableHandle, TableName, NrColumns, TableNr, idebug, Iout1, ILeftBuf, IRightBuf)
               If (.not. success) then
                  call write_error_message_rtc (913, 0,'RdWindTableMod',' Error getting table data',IOUT1)
                  RetVal = 913
                  Return
               Endif
!      Set references
               WindTable(iWind,2) = TableNr
!              Try to reduce table length
               If (ReduceWindTable) Success = ReduceTable(RtcTableHandle, TableNr, NrColumns, StartDate, StartTime, &
                                                          EndDate, EndTime, Idebug, Iout1)
               If (.not. success) then
                  call write_error_message_rtc (913, 0,'RdWindTableMod',' Error Reducing Table ',IOUT1)
                  RetVal = 913
                  Return
               Endif
            Endif
         Endif
      Enddo
  21  Continue

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
!             Do IT=1,NTIMWD
!                Write(IDEBUG,*) IT, (WNDDIR(IWIND,IT,I),I=1,2), WindInterpDir(Iwind)
!             EndDo
           EndIf
           If (CONSWV(Iwind)) Then
             Write(IDEBUG,*) ' Constant velocity',CONSWV, VALCWV
           ELSE
             Write(IDEBUG,*) ' Variable velocity:  time & value   Interpolation code '
!             Do IT=1,NTIMWV
!                Write(IDEBUG,*) IT, (WNDVEL(IWIND,IT,I),I=1,2), WindInterpVEL(Iwind)
!             EndDo
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
      call write_error_message_rtc (902, IECODE, 'RdWindTableMod', ' wind data file', IOUT1)
      RetVal = 902

! *********************************************************************
! *** end of file
! *********************************************************************
!
  999 Continue

      RETURN
      END Function RdWindTableMod



      Subroutine SwapInt (X, Y)
      
      implicit none
      
      Integer X, Y
      Integer IHelp

      IHelp = X
      X = Y
      Y = IHelp

      RETURN
      END Subroutine SwapInt
