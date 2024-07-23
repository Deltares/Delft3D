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

      Function RTC_INIT1 (IOUT1,  IDEBUG, TmSize, &
                        PrecipitationDataset, WindDataset, &
                        ParameterDataSet, OutRRDataSet, OutSbkDataSet, &
                        IEVENT, IBar0, IScren)  result(RetVal)

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
! ***   Initialise arrays for first timestep for each event
! *********************************************************************
! *** Input/output parameters:
! *** ------------------------
! ***   IDEBUG = unit nr. van debugfile
! ***   ....   = Datawset for RR output, Sbk Output, Parameteroutput, precipitation and wind output
! ***   IEVENT = event number
! *********************************************************************

      Use ParameterModule
      Use LanguageModule
      Use FileModule
      Use LocationDataModule
      Use DecisionModule
      Use MeasureModule
      Use OtherData
      Use Dh_Alloc

! DIO
      use Dio_Plt_Rw

      implicit none

      type(DioPltType) :: PrecipitationDataset, WindDataset, ParameterDataSet, &
                          OutRRDataSet, OutSbkDataSet
      Character(Len=DioMaxStreamLen)  :: outName   ! name of out dataset
      character(len=HisRunIdSize), dimension(HisRunIdDim) :: runId         ! (HIS) runid
      Character(Len=DioMaxParLen), pointer, dimension(:)  :: parNames      ! variable(s) in dataset
      Character(Len=DioMaxLocLen), pointer, dimension(:)  :: locNames, locDescr ! locations(s) in dataset

      Integer :: RetVal

      INTEGER   TmSize

      INTEGER   ISCREN, IN, IOUT1,IDEBUG,IEVENT
      INTEGER   IBar0, NrHisLoc, NrHisPar, Allocation_error
      Double Precision   ChkTime
      logical success
      Integer RdWind, RdWindTableMod

      CHARACTER(999) Messg  !, TranslateString

      RetVal = 0
      ISCREN = 6

! ********************************************************************
! *** Write start/stop timesteps in header
! ********************************************************************

!     WRITE (*,*) ' check wrlogo'
      IF (NEVENT .GT. 1 .AND. IEVENT .EQ. 1) THEN
!       WRITE (*,*) ' init1 call 1'
        CALL RTC_WRLOGO (IScren, 0,NEVENT,NEVENT,IBar0,SOBEKMODE)
      ELSEIF (NEVENT .EQ. 1) THEN
!       WRITE (*,*) ' init1 call 2'
        CALL RTC_WRLOGO (IScren, 0,LASTTM,NEVENT,IBar0,SOBEKMODE)
      ENDIF
!     WRITE (*,*) ' after check wrlogo'

! ********************************************************************
! *** Zet Iyear (if necessary); RTC_TIMOLD, RTC_TIMNEW
! ********************************************************************

        If (Timf3B .or. Nevent .gt. 1) then
           IYear = Evstrt(ievent,1)
           IMo   = Evstrt(ievent,2)
           IDay  = Evstrt(ievent,3)
           IHour = Evstrt(ievent,4)
           IMin  = Evstrt(ievent,5)
           ISec  = Evstrt(ievent,6)
           RSec  = ISec
        else
!          Bij CF-RTC 1 event: LASTTM, IYear etc. reeds via INI file gezet
        Endif

        if (idebug .gt. 0) then
           write(idebug,*) ' Init1 Start Event', Iyear, Imo, Iday, Ihour, IMin, ISec
        Endif

!  Corrections igv afrondingen ipv .0100000 bv. 0.009999999; bv. interpretatie 9 uur 0 minuten als 8 uur 99 minuten
!  April 1998; ARS 924
        IF (ISEC .GT. 60) THEN
            IF (IDEBUG .GT. 0) WRITE(IDEBUG,*) ' Correct ISEC', ISEC
            ISEC= 0
            RSEC= 0
            IMIN=IMIN + 1
        ENDIF
        IF (IMIN .GT. 60) THEN
            IF (IDEBUG .GT. 0) WRITE(IDEBUG,*) ' Correct IMIN', IMIN
            IMIN= 0
            IHOUR=IHOUR + 1
            IF (IHOUR .GT. 24) THEN
!               WRITE(*,*) ' Correct IHOUR', IHOUR
               IHOUR = 24
            ENDIF
        ENDIF
! End April 1998
      IF (IDEBUG .GT. 0) THEN
        WRITE(IDEBUG,*) RTC_TIMNEW
        WRITE(IDEBUG,*) IYEAR, IMO, IDAY
        WRITE(IDEBUG,*) IHOUR, IMIN, ISEC
      ENDIF
!30 okt
      IF (IHOUR .GE. 24) THEN
         IHOUR = IHOUR - 24
         CALL NXTDAY (IDEBUG, IYEAR, IMO, IDAY)
      ENDIF

      RTC_TimOld = IYear * 10000. + IMo * 100. + IDay + &
                Dble ( Float(IHour) / 100.   + Float(IMin)/ 10000. + Float(ISec) / 1000000.)
      RTC_TimNew = RTC_TimOld

! ********************************************************************
! *** Altijd: Determine Ihour, Delta IDHR etc
! ********************************************************************

      DELTA2 = DELTAT - 0.0000001
      IFYEAR = IYEAR
      IFMO   = IMO
      IFDAY  = IDAY
      IFHOUR = IHOUR
      IFMIN  = IMIN
      IFSEC  = ISEC
      RFSEC  = RSEC

! ********************************************************************
! *** Convert LastTm
! ********************************************************************
!
      If (Timf3B .or. NEvent .gt. 1) Then
        LASTTM = EVDURA(Ievent,1)*86400 + EVDURA(Ievent,2)*3600 + EVDURA(Ievent,3)*60 + EVDURA(Ievent,4)
        LASTTM = LASTTM / ITMSIZ
      Endif

! ********************************************************************
! ** Debug
! ********************************************************************

      IF (IDEBUG .GT. 0) THEN
        WRITE(IDEBUG,*) ' After correction'
        WRITE(IDEBUG,*) RTC_TIMNEW, RTC_TIMOLD
        WRITE(IDEBUG,*) IYEAR, IMO, IDAY
        WRITE(IDEBUG,*) IHOUR, IMIN, ISEC
        WRITE(IDEBUG,*) DELTAT
        WRITE(IDEBUG,*) IDHR, IDMIN, IDSEC, ITMSIZ
      ENDIF

! ********************************************************************
! *** gegeven starttijdstip = tmfile
! ***  en deltat (tijdstap) en aantal tijdstappen (lasttm)
! *** bepaal eindtijdstip+TIJDHORIZON = timend
! ********************************************************************

       Call SetTimEnd (ChkTime, IEvent, Idebug)
       TimEnd = ChkTime

       IF (IDEBUG .GT. 0) THEN
           WRITE(IDEBUG,*) ' Start & Endtimes      :', RTC_TIMNEW, TIMEND
           WRITE(IDEBUG,*) ' Nr. timesteps, horizon:', LASTTM, NTIMH
       ENDIF

! ********************************************************************
! *** Read Wind file to find relevant data for current period
! ***   between RTC_TIMNEW en TIMEND
! ********************************************************************

      IF (USEW) THEN
         IN = DioNewLun()
         Call OPENFL(IN, NAMFIL(24), IOUT1, 1)
         If (.not. WindUseTableModule) then
             RetVal = RdWind (IDEBUG, IN, IOUT1, IflRtnRtc)
         Else
             RetVal = RdWindTableMod (IDEBUG, IN, IOUT1)
         Endif
         if (RetVal .ne. 0) Return
         Close(IN)
      ENDIF

! ********************************************************************
! ** Initialisatie Current and Previous results; MISSING VALUE -999 hard coded
! ********************************************************************
! Sobek
      ResSbk = -999.
      AlRSbk = -999.
! RR
      Res3B  = -999.
      AlRS3B = -999.
!     CALL INITAR (' 3B   ', RES3B , ALRS3B, ND3B, NPAR3, NTIMS, IDEBUG)
! Precipitation: of met NTIMH ipv NTIMS?
      ResPre = -999.
      AlRSPr = -999.
!     CALL INITAR (' Rain ', RESPRE, ALRSPR, NPRE, NPARP, NTIMS, IDEBUG)
! Wind: of met NTIMH ipv NTIMS?
      ResExt = -999.
      AlRSEX = -999.
!     CALL INITAR ('Extern', RESEXT, ALRSEX, NEXT+NEXTH, NPARE, NTIMS, IDEBUG)
! WQ
      ResWQ  = -999.
      AlRSWQ = -999.
!     CALL INITAR (' WQ   ', RESWQ , ALRSWQ, NSWQ, NPARQ, NTIMS, IDEBUG)
! D3DFlow
      ResD3D = -999.
      AlRS3D = -999.

! ********************************************************************
! ** Initialisatie status measures 3B/Maalstop
! ********************************************************************

      MSSTAT = 0
      MSSTA0 = 0

      D3BSTA = 0
      MS3BST = 0

! ********************************************************************
! **  Create datasets using DIO
! **  Write header using DIO of all output to RR
! ********************************************************************

      IF (IEVENT .EQ. 1) THEN
!       Write(*,*) ' Inititalise for event 1', Ievent
        IF (USE3B) THEN
          outName = Namfil(18)
          IF (outName .NE. ' ') THEN
             Messg = ' RTC Maalstop HIS file uitvoer'
             Messg = TranslateString (RTCLanguageHandle,Messg)
             RunId = ' '
             RunId(1) = Messg(1:40)
             RunId(2) = ' '
             RunId(3) = ' TITLE:  RTC output'
             Call RtcWriteT0String (RunId(4),IFYEAR, IFMO, IFDAY, IFHOUR, IFMIN, IFSEC, TmSize)
!             WRITE (RunId(4), 1000 ) IFYEAR, IFMO, IFDAY, IFHOUR, IFMIN, IFSEC
! 1000        FORMAT ('T0: ',I4.4,'.',I2.2,' ',I2.2,' ',I2.2,':',I2.2,':',I2.2, '  (scu=       1s)')

             NrHisLoc = Max(1,N3bMs)
             NrHisPar = 7
             success = Dh_AllocInit(NrHisLoc, LocNames, ' ')
             success = success .and. Dh_AllocInit(NrHisLoc, LocDescr, ' ')
             success = success .and. Dh_AllocInit(NrHisPar, ParNames, ' ')
             If (.not. success) then
                 Call ErrMsg (929, Allocation_Error, ' Allocating arrays in INIT1', ' ', IOUT1)
                 RetVal = 929
                 Return
             EndIf
             Messg =  ' Maalstop (1=ja)'
             Messg = TranslateString (RTCLanguageHandle,Messg)
             D3BPara(1) = Messg(1:)
             Messg =  ' # actieve maatregln'
             Messg = TranslateString (RTCLanguageHandle,Messg)
             D3BPara(2) = Messg(1:)
             Messg =  ' Matlab actief 0/1  '
             D3BPara(3) = Messg(1:)
             Messg =  ' Low on             '
             D3BPara(4) = Messg(1:)
             Messg =  ' Low off            '
             D3BPara(5) = Messg(1:)
             Messg =  ' High on            '
             D3BPara(6) = Messg(1:)
             Messg =  ' High off           '
             D3BPara(7) = Messg(1:)
             ParNames(1:7) = D3bPara(1:7)
             If (N3BMs .gt. 0) then
                LocNames(1:N3bMs) = ID3BML(1:N3bMs)
                LocDescr(1:N3bMs) = Descr3BML(1:N3bMs)
             Else
                LocNames(1)= 'no-output-desired'
             Endif
             if (N3BMs .gt. 0)  then
                OutRRDataSet = DioPltDefine(outName, runId, Dio_Plt_Double, parNames, locNames)
                Call DioPltAddDescriptions(OutRRDataSet, dio_plt_locs, LocDescr)
             endif
!             If (Idebug .gt. 0) Write(Idebug,*) 'OutRRDataset defined as Dio_Plt_Double =', OutRRDataSet
             Deallocate (ParNames, LocNames, LocDescr)
          ENDIF
        ENDIF

! ********************************************************************
! **  Write header using DIO of all output to Sobek
! ********************************************************************

        IF (USESBK) THEN
          outName = Namfil(19)
          IF (outName .NE. ' ') THEN
             Messg = ' RTC Sobek HIS file uitvoer'
             Messg = TranslateString (RTCLanguageHandle,Messg)
             RunId = ' '
             RunId(1) = Messg(1:40)
             RunId(2) = ' '
             RunId(3) = ' TITLE:  RTC output'
             Call RtcWriteT0String (RunId(4),IFYEAR, IFMO, IFDAY, IFHOUR, IFMIN, IFSEC, TmSize)
!            WRITE (RunId(4), 1000 ) IFYEAR, IFMO, IFDAY, IFHOUR, IFMIN, IFSEC
             NrHisLoc = Max(1,NsMsId_SBK)
             NrHisPar = 1
             success = Dh_AllocInit(NrHisLoc, LocNames, ' ')
             success = success .and. Dh_AllocInit(NrHisLoc, LocDescr, ' ')
             success = success .and. Dh_AllocInit(NrHisPar, ParNames, ' ')
             If (.not. success) then
                 Call ErrMsg (929, Allocation_Error, ' Allocating arrays in INIT1', ' ', IOUT1)
                 RetVal = 929
                 Return
             EndIf
             Messg = 'Sobek Setpoints'
             Messg = TranslateString (RTCLanguageHandle,Messg)
             SbkPara(1) = Messg
             ParNames(1)  =  SbkPara(1)
             If (NsMsId_SBK .gt. 0) then
                LocNames(1:NsMsId_SBK) = MSSBID(1:NsMsId_SBK)
                LocDescr(1:NsMsId_SBK) = MsSbDescr(1:NsMsId_SBK)
             Else
                LocNames(1)= 'no-output-desired'
             Endif
             if (NSMeas_SBK .gt. 0) then
                OutSbkDataSet = DioPltDefine(outName, runId, Dio_Plt_Double, parNames, locNames)
                Call DioPltAddDescriptions (OutSbkDataSet,dio_plt_locs, LocDescr)
             endif
             Deallocate (ParNames, LocNames, LocDescr)
           Endif
        ENDIF

!       write(*,*) ' Header Pre-Rain file NTimHP NPrecP=',NTimHp, NPrecP

        IF (USEP) THEN
          outName = Namfil(25)
          IF (outName .NE. ' ') THEN
             Messg = ' RTC Neerslagvoorspelling'
             Messg = TranslateString (RTCLanguageHandle,Messg)
             RunId = ' '
             RunId(1) = Messg(1:40)
             RunId(2) = ' '
             RunId(3) = ' TITLE:  RTC output'
             Call RtcWriteT0String (RunId(4),IFYEAR, IFMO, IFDAY, IFHOUR, IFMIN, IFSEC, TmSize)
!            WRITE (RunId(4), 1000 ) IFYEAR, IFMO, IFDAY, IFHOUR, IFMIN, IFSEC
             NrHisLoc = NPrecp
             NrHisPar = NTimHp
             Messg =  ' Voorspelling T+i'
             Messg = TranslateString (RTCLanguageHandle,Messg)
             success = Dh_AllocInit(NrHisLoc, LocNames, ' ')
             success = success .and. Dh_AllocInit(NrHisPar, ParNames, ' ')
             If (.not. success) then
                 Call ErrMsg (929, Allocation_Error, ' Allocating arrays in INIT1', ' ', IOUT1)
                 RetVal = 929
                 Return
             EndIf
             ParNames = Messg
             LocNames(1:NPrecP) = ID_Pre(1:NPrecP)
             PrecipitationDataSet = DioPltDefine(outName, runId, Dio_Plt_Double, parNames, locNames)
             Deallocate (ParNames, LocNames)
           ENDIF
        ENDIF

!       write(*,*) ' Header Pre-Wind file NTimHW NWind=',NTimHW, NWind
! wind
        IF (USEW) THEN
          outName = Namfil(26)
          IF (outName .NE. ' ') THEN
             Messg = ' RTC Windvoorspelling'
             Messg = TranslateString (RTCLanguageHandle,Messg)
             RunId = ' '
             RunId(1) = Messg(1:40)
             RunId(2) = ' '
             RunId(3) = ' TITLE:  RTC output'
             Call RtcWriteT0String (RunId(4),IFYEAR, IFMO, IFDAY, IFHOUR, IFMIN, IFSEC, TmSize)
!            WRITE (RunId(4), 1000 ) IFYEAR, IFMO, IFDAY, IFHOUR, IFMIN, IFSEC
             NrHisLoc = NExt+NExtHD
             NrHisPar = NTimHw*2
             success = Dh_AllocInit(NrHisLoc, LocNames, ' ')
             success = success .and. Dh_AllocInit(NrHisPar, ParNames, ' ')
             If (.not. success) then
                 Call ErrMsg (929, Allocation_Error, ' Allocating arrays in INIT1', ' ', IOUT1)
                 RetVal = 929
                 Return
             EndIf
             Messg =  ' Windrichting T+i'
             Messg = TranslateString (RTCLanguageHandle,Messg)
             ParNames(1:NtimHw) = Messg
             Messg =  ' Windsnelheid T+i'
             Messg = TranslateString (RTCLanguageHandle,Messg)
             ParNames(NtimHw+1:NtimHw*2) = Messg
             LocNames(1:NExt+NExtHd) = ID_Ext(1:NExt+NExtHd)
             WindDataSet = DioPltDefine(outName, runId, Dio_Plt_Double, parNames, locNames)
             Deallocate (ParNames, LocNames)
           ENDIF
        ENDIF

! ********************************************************************
! **  Write header Beslisparameter HIS file voor
! ********************************************************************

!       write(*,*) ' Header Para file NPara=',NPara
        outName = Namfil(20)
        IF (outName .NE. ' ' .AND. NPara .GT. 0) THEN
           Messg = ' RTC Beslisparameter waarden'
           Messg = TranslateString (RTCLanguageHandle,Messg)
           RunId = ' '
           RunId(1) = Messg(1:40)
           RunId(2) = ' '
           RunId(3) = ' TITLE:  RTC output'
           Call RtcWriteT0String (RunId(4),IFYEAR, IFMO, IFDAY, IFHOUR, IFMIN, IFSEC, TmSize)
!          WRITE (RunId(4), 1000 ) IFYEAR, IFMO, IFDAY, IFHOUR, IFMIN, IFSEC
           NrHisLoc = NPara
           NrHisPar = 1
           success = Dh_AllocInit(NrHisLoc, LocNames, ' ')
           success = success .and. Dh_AllocInit(NrHisLoc, LocDescr, ' ')
           success = success .and. Dh_AllocInit(NrHisPar, ParNames, ' ')
           If (.not. success) then
              Call ErrMsg (929, Allocation_Error, ' Allocating arrays in INIT1', ' ', IOUT1)
              RetVal = 929
              Return
           EndIf
           Messg = ' Parameter waarde'
           Messg = TranslateString (RTCLanguageHandle,Messg)
           ParNames(1) = Messg
           LocNames(1:NPara) = ParaId(1:NPara)
           LocDescr(1:NPara) = ParaDescr(1:NPara)
           ParameterDataSet = DioPltDefine(outName, runId, Dio_Plt_Double, parNames, locNames)
           Call DioPltAddDescriptions (ParameterDataSet, dio_plt_locs, LocDescr)
           Deallocate (ParNames, LocNames, LocDescr)
        ENDIF
      ENDIF


      RETURN
      END Function RTC_Init1



    Subroutine  RTCWriteT0String (Name, IYear, Imo,Iday,Ihour,Imin,Isec,TmSize)

      Character*40  Name
      Integer       IYear, Imo, Iday, Ihour,Imin,Isec,TmSize

      IF (TMSIZE .LT. 10) THEN
        WRITE(NAME(1:40),101) IYEAR,IMO,IDAY,IHOUR,IMIN,ISEC,TMSIZE
      ELSEIF (TMSIZE .LT. 100) THEN
        WRITE(NAME(1:40),102) IYEAR,IMO,IDAY,IHOUR,IMIN,ISEC,TMSIZE
      ELSEIF (TMSIZE .LT. 1000) THEN
        WRITE(NAME(1:40),103) IYEAR,IMO,IDAY,IHOUR,IMIN,ISEC,TMSIZE
      ELSEIF (TMSIZE .LT. 10000) THEN
        WRITE(NAME(1:40),104) IYEAR,IMO,IDAY,IHOUR,IMIN,ISEC,TMSIZE
      ELSEIF (TMSIZE .LT. 100000) THEN
        WRITE(NAME(1:40),105) IYEAR,IMO,IDAY,IHOUR,IMIN,ISEC,TMSIZE
      ELSEIF (TMSIZE .LT. 1000000) THEN
        WRITE(NAME(1:40),106) IYEAR,IMO,IDAY,IHOUR,IMIN,ISEC,TMSIZE
      ELSEIF (TMSIZE .LT. 10000000) THEN
        WRITE(NAME(1:40),107) IYEAR,IMO,IDAY,IHOUR,IMIN,ISEC,TMSIZE
      ELSE
        WRITE(NAME(1:40),108) IYEAR,IMO,IDAY,IHOUR,IMIN,ISEC,TMSIZE
      ENDIF
  101 FORMAT ('T0: ',I4,'.',I2,'.',I2,1X,I2,':',I2,':',I2,'  (scu=       ',I1,'s)')
  102 FORMAT ('T0: ',I4,'.',I2,'.',I2,1X,I2,':',I2,':',I2,'  (scu=      ',I2 ,'s)')
  103 FORMAT ('T0: ',I4,'.',I2,'.',I2,1X,I2,':',I2,':',I2,'  (scu=     ',I3  ,'s)')
  104 FORMAT ('T0: ',I4,'.',I2,'.',I2,1X,I2,':',I2,':',I2,'  (scu=    ',I4   ,'s)')
  105 FORMAT ('T0: ',I4,'.',I2,'.',I2,1X,I2,':',I2,':',I2,'  (scu=   ',I5    ,'s)')
  106 FORMAT ('T0: ',I4,'.',I2,'.',I2,1X,I2,':',I2,':',I2,'  (scu=  ',I6     ,'s)')
  107 FORMAT ('T0: ',I4,'.',I2,'.',I2,1X,I2,':',I2,':',I2,'  (scu= ',I7      ,'s)')
  108 FORMAT ('T0: ',I4,'.',I2,'.',I2,1X,I2,':',I2,':',I2,'  (scu=',I8       ,'s)')

      Return
    End Subroutine RTCWriteT0String



