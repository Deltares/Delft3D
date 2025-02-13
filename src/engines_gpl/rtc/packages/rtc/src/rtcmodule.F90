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

      Module RTCModule

! *********************************************************************
! ***                D E L F T         H Y D R A U L I C S
! ***
! ***         REGIONAL WATER RESOURCES AND ENVIRONMENT DIVISION
! *********************************************************************
! *** Program :  SOBEK-LITE  version 1.00             Date: June  1997
! *** Module  :  RTC
! *********************************************************************
! *** Created    : June   1997       By : Geert Prinsen
! *********************************************************************
! *********************************************************************
! *** Brief description:
! *** ------------------
! ***   Real time control module for use in SOBEK-LITE.
! ***   Development based on MAALSTOP module and BESYS module
! ***   which were applied in North-Holland and Geleenbeek projects.
! ***   (T1622, T2021; T1659)
! ***   See memo STURING3.DOC June 1997.
! *********************************************************************
! *** Input/output parameters:     none.
! *** ------------------------
! *********************************************************************
! *** ADRESS     : Deltares,
! ***              P.O.BOX 177,
! ***              2600 MH DELFT, THE NETHERLANDS
! **********************************************************************

! **********************************************************************
! DIO/SHM
      use Dio_Plt_Rw
! **********************************************************************
      Use ParameterModule
      Use FileModule
      Use LanguageModule_rtc
      Use LocationDataModule
      Use DecisionModule
      Use ReservoirModule
      Use MeasureModule
      Use OtherData
      Use SyncRtcFlow
      Use ReadLib_rtc
      Use ExternalDLLModule
      Use system_utils, only: FILESEP
      Use time_module, only: mjd2date, jul2mjd, ymd2jul
      use rtc_version_module

      implicit none

! *** OTHER DATA
      CHARACTER*160 HEADER

! Controller
      INTEGER*4 IDCNT
      INTEGER*2 ISTAT
      LOGICAL   RTCFirstProc, RTCInitMode, RTCCrashed, FnmExt

      INTEGER   IDEBUG,IOUT1,IN,ISCREN,IdebugLun, Iout1Lun
      INTEGER   NWST1, NWST2, NWST3
      INTEGER   IEVENT,ITMSTP
      Real      NrSecondsSinceStartFirstEvent
      Integer   NrTimestepsSinceStartFirstEvent
      INTEGER   IBar0

      CHARACTER(Len=CharIdLength) OpenMiExeName, OpenMiModelName, OpenMiReturnName
      Character(Len=1024) OpenMICommandLine
!     CHARACTER*256  CMDLINE   too short for Delft-3D; enlarged on request Adri Mourits March 2002
      CHARACTER*1024 CMDLINE
      CHARACTER*40  STRING
      CHARACTER*1  QUOTE, comma, DblQuote

      Integer IBui_Itmstp, IBui_Rem
      Double Precision  Time_Ratio, RSecondsSinceStartFirstEvent

      Integer IExitCode
      Integer SetMaxTabNr, SetMaxDataNr

!     Variables related to D3D-Mode
      Character*120 cident
      Integer n2steps
      Logical lfout
      Integer D3DStatus
      Integer i3ddat, itstart
      Character*25 filsim   ! Trigger file for D3D-Mode
      Logical      lexist   ! to check existance of trigger file
      Integer      icheck   ! Check value from trigger file
      Character    rtcpath*256       ! Path for RTC-files
      Integer      rtnval            ! Return code of CACCESS
      Integer      lrtcpath          ! Length of RTC-path
      Integer      namlen            ! Length of trimmed file name
!     Flag to read barrier data first time in loop
      Logical FirstBarrier

      ! like currentTimestep in OMI_CF_control
      Integer, public :: RTC_ITimestep

      CHARACTER*10     RTCCDATE, RTCCTIME, RTCCZONE
      INTEGER          RTCtime_fields(8)
      Double Precision RTCJulianStart, RTCJulianNow
      Integer          RTCIDateAct, RTCITimeAct

! *********************************************************************
! DIO datasets
! *********************************************************************
! DIO incoming datasets
      type(DioPltType) :: inPlt_3b, inPlt_Sbk, inPlt_WQ, inPlt_D3D
! DIO outgoing dataset
      type(DioPltType) :: outPlt_3b, OutPlt_Sbk
! Other datasets
      type(DioPltType) :: PrecipitationDataset, WindDataset, ParameterDataSet, &
                          OutRRDataSet, OutSbkDataSet
! External datasets (HIS files, at most NEXTH
      type(DioPltType) :: ExtHisDataset(22)   ! NParI=11 defined in ParameterModule

      Logical             DoHeaderInRR, DoHeaderInSbk, DoHeaderInWQ, DoHeaderInD3D, DoHeaderExt(22)

      Double precision, allocatable :: DioResult (:,:)
      Integer  Allocation_error, ITempFileUnit

      Double Precision RTCJulianStartDate, RTCModJulianTime, RTCJulianTimestep
!  license checking
      Integer          RTCLicenseReturnCode
      Character(Len=600) RTCFlexlmErrorMessage


  contains

    Subroutine Set_Commandline()
!     Use ReadLib_rtc

      implicit none

      Character(Len=CharIdLength) exename, modelname, returnname
      Integer   length1, length2, length3

      exename    = OpenMIExename
      modelname  = OpenMIModelname
      returnname = OpenMIReturnname
      length1 = len_trim (exename)
      length2 = len_trim (modelname)
      length3 = len_trim (returnname)
      OpenMICommandline = exename(1:length1) // ' ' // modelname(1:length2) // ' ' // returnname(1:length3) // ' ctrl.ini' // Char(0)

    return
    end subroutine Set_Commandline

! *********************************************************************
! *** Getfil - set command line, read file with filenames
! *********************************************************************

      Function GETFIL (Idebug, IflRtnRtc) result(RetVal)

      Integer :: RetVal

      INTEGER       IDEBUG, IN, I, IECODE, length
      LOGICAL       ENDFIL
      CHARACTER*256 FILNAM, FILNM2, STRING
      CHARACTER*8   NAMSUB
! voor controller!
      INTEGER        IOUT1, IflRtnRtc, NRARG
! end voor controller!

      RetVal = 0
      NAMSUB = '  Getfil'
      if (idebug > 0) WRITE (IDEBUG,*) ' GETFIL'

! *** Check if there are Command line arguments

!MSF
      NrArg = 3
!controller: Stel string CMDLINE samen voor de stuurmodule
!GP April 1998: aanpassing voor situatie dat aanroep altijd met dezelfde command line
!               en stuurfile CTRL.INI via filenamen file opgegeven.
!Mar 2004 adjusted HarmonIT
      Filnam  = OpenMiModelName
      FilNm2  = OpenMiReturnName
      CmdLine = OpenMiCommandLine
!
!     File with Filenames
      if (idebug > 0) WRITE (IDEBUG,'(A)') ' FILNAM = ',FILNAM

! *** second argument: file name of return code file
      if (idebug > 0) WRITE (IDEBUG,'(A)') ' FILNM2 = ',FILNM2
      IFlRtnRtc =  DioNewLun()
      OPEN (IflRtnRtc,FILE=FILNM2)

! *** OPEN AND READ  FILE WITH FILE NAMES

      IN = DioNewLun()
      OPEN (IN,FILE=FILNAM)
      CALL SKPCOM (IN,ENDFIL,'RTC')
      IF (ENDFIL) GOTO 23
      DO I=1,NFILE
         READ(IN, *, END=23, ERR=555, IOSTAT=IECODE) NAMFIL(I)
         if (idebug > 0)  WRITE (IDEBUG,'(A)') NAMFIL(I)
      ENDDO

! April 1998
! lees extra file CTRL.INI uit FNM file, indien aanwezig
!  (nb. aantal files is dus NFILE+1; indien NFILE vergroot zou worden zou dit bij oude FNM files foutmeldingen geven.
!  HarmonIt March 2004 - RTCNArgs vervangen door NrArgs
      IF ( NrArg .EQ. 3) THEN
         NAMFIL(NFILE+1) = ' '
         READ(IN, *, END=23, ERR=551, IOSTAT=IECODE) NAMFIL(NFILE+1)
  551    CONTINUE
         LENGTH = len_trim (NAMFIL(NFILE+1))
         IF (LENGTH .GT. 0) THEN
                CMDLINE = CMDLINE(1:len_trim(CMDLINE))  // ' ' // NAMFIL(NFILE+1)(1:LENGTH) // CHAR(0)
         ELSE
           WRITE(*,*) ' Error in Command line'
           call write_error_message_rtc (999, IECODE, NamSub, ' Error in RTC command line', IOUT1)
           RetVal = 999
           Return
!           STOP ' Error in Command line'
         ENDIF
      ENDIF
! End April 1998

!     Try to read file name for barrier heights (D3D-RTC only coupling; no Sobek)
      NAMFIL(NFILE+2) = ' '
      NAMFIL(NFILE+3) = ' '
      NAMFIL(NFILE+4) = ' '
      NAMFIL(NFILE+5) = ' '
      READ(IN, *, END=100, ERR=100, IOSTAT=IECODE) NAMFIL(NFILE+2)
!     Try to read 2 additional files for D3D-Sobek-Rtc coupling
      READ(IN, *, END=100, ERR=100, IOSTAT=IECODE) NAMFIL(NFILE+3)
      READ(IN, *, END=100, ERR=100, IOSTAT=IECODE) NAMFIL(NFILE+4)
!     And one more for Delft3D-FLOW measures
      READ(IN, *, END=100, ERR=100, IOSTAT=IECODE) NAMFIL(NFILE+5)

      goto 200

!     No Barrier Heights file found
  100 continue
!     Set (for security) name to blanks to determine later
      NAMFIL(NFILE+2) = ' '


  200 CLOSE(IN)

      RETURN

! *** Error messages

  555 CONTINUE
         call write_error_message_rtc (902, IECODE, NAMSUB, STRING, IOUT1)
         RetVal = 902
!
   23 CONTINUE
         IF (I .GE. NFILE) THEN
            NAMFIL(17) = 'RTC_OUT.OUT'
         ELSE
           call write_error_message_rtc (902, IECODE, NAMSUB, STRING, IOUT1)
           RetVal = 902
         ENDIF
!
    Return
    End Function Getfil




! ************************************************************************************
!    RTCCreate
! ************************************************************************************

  Integer function RTCCreate (RTC_RunId,ArgsToRTC)

  use wl_open_mi_support
  use rtc_open_mi_support


  Implicit none
  Integer  RTC_RunId

    character (Len=*), dimension(:) :: argsToRTC

    RTC_Runid = 1
    RTCCreate = -1

     if (size(ArgsToRTC) .ge. 3) then
        OpenMiExeName = argstoRTC(1)      ! exe
        OpenMIModelName = argstoRTC(2)    ! fnm file
        OpenMIReturnName = argstoRTC(3)   ! return code file
        Call Set_CommandLine ()           ! set command line voor controller
        RTC_Runid = OesModelFindOrCreate(RTCComponent, argstoRTC(2))
        if ( RTC_Runid > 0) then
            RTCCreate = 0
        endif
     endif

  return
  end function RTCCreate

! ************************************************************************************
!    RTCInitialize
! ************************************************************************************


  Integer function RTCInitialize (RTC_RunId, RTC_Nevent)

  Implicit none
  Integer  RTC_RunId, RTC_Nevent
  Integer  i, NWindRec
  Integer  NrFlowRRMeasures
  Integer  LengthA, lengthB

  Integer ScanRain, ScanWind, RdRain, RdPara, RdSMeas, Rd3BMs, OpenMatlab
  Integer RdSMeas_OldFormat, Rd3BMs_OldFormat
  Logical Success, FNMEXT

  Double Precision Julian

  Integer, parameter :: ArrayLength=7  ! 6; extended for Singapore RTC-D3D salinity exchange with Sobek
  Integer      MaxNumberArray(ArrayLength)
  Character*10 KeywordArray(ArrayLength)
  Logical      CheckArray(ArrayLength)

      RTCInitialize = 0
      NWindRec = 0

! *********************************************************************
! *** Initialization of cident for Delft_3D mode
! *********************************************************************

      call getfullversionstring_RTC(cident)

! *********************************************************************
! *** Initialization of file references
! *********************************************************************

! Zet vaste dimensies ParameterModule
      Call SetFixedParameters
      RTCInitialize = InitFiles ()
      If (RTCInitialize .ne. 0) Return
! Default DIO setting headers
      DoHeaderInRR  = .true.
      DoHeaderInSbk = .true.
      DoHeaderInD3D = .true.
      DoHeaderInWQ  = .true.
      DoHeaderExt  = .true.

!     NFILE  = 28       ! voor Fryslan
      NFILE  = 29       ! march 2005: added 1 file for RTC-Simulate messages
                        ! March 2002: increase with 1 for reservoir input data file
                        ! Oct   2002: increase with 1 for DioConfig.Ini file

! Jaap Zeekant vlag DebugMode
      IDEBUGLun = DioNewLun()
      IDEBUG = IdebugLun
      OPEN (IDEBUG, FILE='RTC.DBG', STATUS='UNKNOWN')
      Idebug = 0

#ifdef DEBUGMODE
      IDEBUG = IdebugLun
#else
      IDEBUG = 0
#endif
      FirstBarrier = .True.
      IOUT1Lun = DioNewLun()
      IOUT1  = Iout1Lun
      OPEN (IOUT1, FILE='rtc.Log', STATUS='UNKNOWN')

!for testing debug should be on     IDEBUG = 0
      IDEBUG = 0

      DO I=1,NFIL
!        INXFIL(I) = 10+i
        NAMFIL(i) = ' '
      ENDDO

      QUOTE = ''''
      DblQuote = '"'
      COMMA = ','
      RTCInitMode = .false.
      RTCCrashed  = .false.
      RTCSelfCrash = .false.
      IdControlModule = -1

!-----------------------------------------------------------------------
!-----File checks to determine if RTC is running together with D3DFlow
!-----------------------------------------------------------------------
      filsim = 'coupled-dflow2d3d-rtc-run'
      inquire(file=filsim,exist=lexist)
      if (lexist) then
        CoupledToDelft3D = .true.
      else
        CoupledToDelft3D = .false. ! unless TMP_SYNC.RUN exists
      endif

      filsim = 'TMP_SYNC.RUN'
      if (CoupledToDelft3D) then
        ! wait for Delft3D-FLOW to be ready
        lexist = .false.
        do while (.not. lexist)
          inquire(file=filsim,exist=lexist)
        enddo
      else
        ! check once
        inquire(file=filsim,exist=lexist)
      endif

      if (lexist) then
        In = DioNewLun()
        open (IN,file=filsim,form='unformatted', &
              status='unknown')
        read (IN) icheck
        close(IN)
!-------Check 'RUNRTC' by telephone
        if (icheck .eq. 786782) then
           CoupledToDelft3D = .true.
        else
           call write_error_message_rtc (946, 0, 'RTC', ' Trigger-file TMP_SYNC.RUN not made by Delft3D-FLOW ', IOUT1)
           RtcInitialize = 946
           Return
        endif
      else
        CoupledToDelft3D = .false.
      endif

!-----------------------------------------------------------------------
!-----Get the Delft3D environment variables and create RTC path
!-----------------------------------------------------------------------
      If (CoupledToDelft3D) then
        RtcInitialize = RtnVal
        if (RtcInitialize .ne. 0) Return

!       instead of this, use path as specified in RTC.FNM by finding last slash in the specified RTC.FNM
        rtcpath = OpenMIModelName
        lrtcpath = len_trim(rtcpath)
        do i=lrtcpath,1,-1
           if (rtcpath(i:i) .eq. FILESEP) then
             rtcpath(i:) = ''
             lrtcpath = i-1
             goto 101
           endif
        enddo
!       if no slash found, no path!
        lrtcpath = 0
 101    continue
     Else
        rtcpath = ' '
        lrtcpath = 0
     Endif
!
! *********************************************************************
! *** Get command line arguments
! ***  initialiseer stuurmodule
! *********************************************************************

! March 2004
! oud CALL GETFIL (NAMFIL, NFILE, IDEBUG, IOUT1, IFlRtnRtc, CMDLINE)
      RtcInitialize = GETFIL (Idebug, IflRtnRtc)
      If (RTCInitialize .ne. 0) Return
! make sure Rtc.Log file is opened on place specified in fnm file
      Close(Iout1,Status='Delete')
      IOUT1Lun = DioNewLun()
      IOUT1  = Iout1Lun
      OPEN (IOUT1, FILE=Namfil(21), STATUS='UNKNOWN')

!-----------------------------------------------------------------------
!---- Add RTC-Path to the files for Delf3D-Flow
!---- Adjusted April 2010: added check for lrtcpath>0
!-----------------------------------------------------------------------
      If (CoupledToDelft3D) then
!------ Language file
        Inquire (FILE = NamFil(22), EXIST = FNMEXT)
        if (.not. FNMEXT) then
           call NOEXTSPACES(NAMFIL(22), namlen)
           if (lrtcpath .gt. 0) NAMFIL(22) = rtcpath(1:lrtcpath)//FILESEP//NAMFIL(22)(1:namlen)
        endif
!------ INI-file
        Inquire (FILE = NamFil(1), EXIST = FNMEXT)
        if (.not. FNMEXT) then
           call NOEXTSPACES(NAMFIL(1), namlen)
           if (lrtcpath .gt. 0) NAMFIL(1) = rtcpath(1:lrtcpath)//FILESEP//NAMFIL(1)(1:namlen)
        endif
      Endif
!
! *********************************************************************
! *** Zet scherm message file unit
! *********************************************************************

      ISCREN = 6
#if (defined(HAVE_CONFIG_H))
      Open (Iscren,FORM='FORMATTED')
#else
      Open (Iscren,carriagecontrol='FORTRAN')
#endif

      NEVENT = 1

! *********************************************************************
! *** Read language file
! *** Read RTC INI file
! *** Write logo
! *** Continue RTC only if run together with Sobek and/or 3B
! *********************************************************************

      IN = DioNewLun()
      Call OPENFL(IN, NAMFIL(22), IOUT1, 1)
      Call LanguagesCreate
      RtcLanguageHandle = LanguagesModelFindOrCreate(0)
      Call ReadLanguageFile (RtcLanguageHandle, IN, IDEBUG, IOUT1,RTCInitialize)
      If (RTCInitialize .eq. 981) then
          call write_error_message_rtc (981, 0, ' Error allocating arrays in subroutine ', ' ReadLanguage ',Iout1)
      ElseIf (RTCInitialize .eq. 972) then
          call write_error_message_rtc (972, 0,'Error in language file',' ',IOUT1)
      Endif
      if (RtcInitialize .ne. 0) Return
      Close(IN)

      IN = DioNewLun()
      Call OPENFL(IN, NAMFIL(1), IOUT1, 1)
      Call RTC_RDINI (IDEBUG, IDebugLun, IN, IOUT1)
      Call SetActiveLanguage(RtcLanguageHandle, ActiveLanguage)
      Close(IN)
! Nov 2002 Taiwan: Wind altijd via Tabellenmodule
!     WindUseTableModule = .true.

! ARS 14556
! Default initialisations, overruling Rtc.Dat file
! Later adjust this, after reading actual input files.
      if (SOBEKMODE) then
         UseSbk =.true.
         Use3B  =.true.
         UsePre =.true.
         UseExt =.true.
         UseWQ  =.true.
         UseP   =.true.
         UseW   =.true.
      elseif (Use3D) then
         CoupledToDelft3D = .true.
      endif

!     RTC must be in SOBEK-Mode or in Delft3D-Mode
      If (.NOT. SOBEKMODE .AND. .NOT. CoupledToDelft3D) Then
         call write_error_message_rtc (921, 0, ' RTC', ' RTC-Ini file', IOUT1)
         RTCInitialize = 921
         Return
      Endif

!     write(*,*) ' call 1'
      IBar0 = 0
      Call RTC_WRLOGO (IScren, 0,0,0, IBar0,SOBEKMODE)

!  DIO: all communication set to HIS files, other options not supported anymore
      If (IOptS .ne. 2. .or. Iopt3 .ne. 2 .or. IoptWq .ne. 2) Then
         Iopts = 2
         Iopt3 = 2
         IoptWq = 2
         call write_error_message_rtc (953, 0, ' RTC', ' RTC-Ini file', IOUT1)
         RTCInitialize = 953
         Return
      Endif

!     Wind is only possible together wit External Locations
      If (USEW .AND. .NOT. USEEXT) Then
         call write_error_message_rtc (944, 0, ' RTC', ' RTC-Ini file', IOUT1)
         RTCInitialize = 944
         Return
      Endif

!     Idebug = IdebugLun
!     Zet overige parameters, via scannen van de invoerfiles.
      Call SetOtherParameters

! Locations
! should be at least 1 for dimensioning arrays
      NSPAR = 1
      N3PAR = 1
      NPPAR = 1
      NEPAR = 1
      NQPAR = 1
      NDPAR = 1
      NRPAR = 1
      N3DPAR = 1

      NSBK = 1
      ND3B = 1
      NPRE = 1
      NEXT = 1
      NEXTH = 1
      NSWQ = 1
      N3D  = 0
! scan Sobek files
      In = DioNewLun()
      Inquire (FILE = NamFil(32), EXIST = FNMEXT)
      if (FNMEXT) then
         Call ScanFile (In, NamFil(32), Iout1, 1, 'D3DO', N3D, .false. )
      endif
      if (SOBEKMODE) then
         if (UseSbk) Call ScanFile (In, NamFil(2), Iout1, 1, 'SBKO', NSBK, .true. )
         if (Use3B)  Call ScanFile (In, NamFil(3), Iout1, 1, '3BO ', ND3B, .true. )
         if (UsePre) Call ScanFile (In, NamFil(4), Iout1, 1, 'PREC', NPRE, .true. )
         if (UseExt) Call ScanFile (In, NamFil(5), Iout1, 1, 'EXT ', NEXT, .true. )
         if (UseExt) Call ScanFile (In, NamFil(5), Iout1, 1, 'HEXT', NEXTH, .true. )
         if (UseWQ)  Call ScanFile2 (In, NamFil(13), Iout1, 1, 'Name:', NSWQ)
         if (idebug > 0) then
            Write(Idebug,*) ' ND3B = ', ND3B
            Write(Idebug,*) ' NSBK = ', NSBK
            Write(Idebug,*) ' NPRE = ', NPRE
            Write(Idebug,*) ' NEXT = ', NEXT
            Write(Idebug,*) ' NEXTH= ', NEXTH
            Write(Idebug,*) ' NSWQ = ', NSWQ
            Write(Idebug,*) ' N3D  = ', N3D
         Endif
      Endif
! Sobek Rain
      If ((SOBEKMODE .and. USEP) .or. TimF3B  .or. Nevent .gt. 1) Then
        IN = DioNewLun()
        Call OPENFL(IN, NAMFIL(23), IOUT1, 1)
        RTCInitialize = ScanRain (IDEBUG, IN, IOUT1)
        If (RTCInitialize .ne. 0) Return
        Close(IN)
      Else
        NEVNT = Nevent
        NTIM  = 1
        NSTAT = 1
      Endif
! Sobek Wind;
      if (SOBEKMODE .and. (UseW .or. UseExt)) then
         In = DioNewLun()
         Call ScanFile (In, NamFil(24), Iout1, 1, 'GLMT', NWST1, .false. )
         Call ScanFile (In, NamFil(24), Iout1, 1, 'MTEO', NWST2, .false. )
         Call ScanFile (In, NamFil(24), Iout1, 1, 'WSTA', NWST3, .false. )
!         NWIND = Max(1, NWST1+NWST2+NWST3-1)
         NWIND = Max(1, NWST1+NWST2+NWST3)
         Call ScanFile2 (In, NamFil(24), Iout1, 1, '  <  ', NWindRec)
      else
         NWIND = 1
      endif

! Decision parameters
      Inquire (FILE = NamFil(8), EXIST = FNMEXT)
      if (FNMEXT .OR. SOBEKMODE) then
        In = DioNewLun()
        Call ScanFile (In, NamFil(8), Iout1, 1, 'PARA', NPAR1, .true. )
        Call ScanFile (In, NamFil(8), Iout1, 1, 'PAR2', NPAR2, .true. )
        Call ScanFile (In, NamFil(8), Iout1, 1, 'PAR3', NPAR3Para, .true. )
        Call ScanFile (In, NamFil(8), Iout1, 1, 'RSVP', NPAR3RSVP, .true. )
        MaxNumberArray = 1
        KeywordArray(1) = 'FlowLoc'
        KeywordArray(2) = 'RRLoc'
        KeywordArray(3) = 'PrecipLoc'
        KeywordArray(4) = 'ExtLoc'
        KeywordArray(5) = 'WQLoc'
        KeywordArray(6) = 'ParLoc'
        KeywordArray(7) = 'D3DLoc'
        Call ScanFile3b (In, NamFil(8), Idebug, Iout1, 1, 'PARA', KeywordArray, MaxNumberArray, ArrayLength)
        Call ScanFile3b (In, NamFil(8), Idebug, Iout1, 1, 'PAR2', KeywordArray, MaxNumberArray, ArrayLength)
        NSPAR = MaxNumberArray(1)
        N3PAR = MaxNumberArray(2)
        NPPAR = MaxNumberArray(3)
        NEPAR = MaxNumberArray(4)
        NQPAR = MaxNumberArray(5)
        NDPAR = MaxNumberArray(6) + NParI
        N3DPAR = MaxNumberArray(7)
        KeywordArray(1) = 'no'
        MaxNumberArray(1) = 1
        Call ScanFile3 (In, NamFil(8), Idebug, Iout1, 1, 'RSVP', KeywordArray, MaxNumberArray, 1)
        NRPar = 3 + 2*MaxNumberArray(1)
      endif
! Add predefined Time parameters (NPARI=11);
! and assume at most MaxTypeGates(3) * MatSameGates(3) parameters per RSVP record
      NPAR1 = NPAR1 + NPARI
      NDECV = NPAR1 + NPAR2 + NPAR3Para + MaxTypeGates * MaxSameGates * NPAR3RSVP

!     max. number of tables (time tables)
!     NPAR3Para = nr.  PAR3 records in Decispar file
!     NWind*2   = nr.  windstations times 2 (wind velocity and wind direction)
!     NPAR3RSVP = nr.  reservoirs
!                 for each reservoir:  max. flow time tables (MaxTypesGates *MaxSameGates)
!                                      energy flow demand time tables (turbine gates only)
      SetMaxTabNr = NPar3Rsvp * (MaxTypeGates * MaxSameGates + MaxSameGates) + NPar3Para + NWind * 2
      SetMaxDataNr = max (SetMaxTabNr * 1000 + 163820,  NWindRec + SetMaxTabNr*100)

      Call NewTablesCreate
      RTCTableHandle = NewTablesModelFindOrCreate(0)
      success = NewTableConfar(RtcTableHandle, SetMaxTabNr, SetMaxDataNr)
      if (.not. success) then
         RtcInitialize = 929
         call write_error_message_rtc (RtcInitialize, 0,' Allocating Table data',' NewTableConfAr',IOUT1)
         Return
      Endif

! Measures Sobek-CF
      NSMES_SBK = 0
      NSCV_SBK = 0
      if (SOBEKMODE) then
        In = DioNewLun()
        If (OldFormatMeasureFiles) then
           Call ScanFile (In, NamFil(9), Iout1, 1, 'SBMS', NSMES_SBK, .true. )
           NSCV_SBK=10
        else
           Call ScanFile2 (In, NamFil(9), Iout1, 1, 'SBMS ', NSMES_SBK )
           KeywordArray(1) = 'ty'
           KeywordArray(2) = 'nv'
           MaxNumberArray(1) = 2
           MaxNumberArray(2) = 1
           CheckArray(1) = .true.
           CheckArray(2) = .false.
           Call ScanFile4 (In, NamFil(9), Idebug, Iout1, 1, 'SBMS', KeywordArray, MaxNumberArray, CheckArray,2)
           NSCV_SBK = max (1, MaxNumberArray(2))
        endif
      endif

! Measures Sobek-RR
      if (SOBEKMODE .and. USE3B) then
        In = DioNewLun()
        If (OldFormatMeasureFiles) then
           Call ScanFile (In, NamFil(10), Iout1, 1, 'MLST', N3MES, .true. )
           Call ScanFile (In, NamFil(10), Iout1, 1, '3BML', N3LOC, .true. )
           Call ScanFile (In, NamFil(10), Iout1, 1, 'MATL', N3MAT, .true. )
           ! ivm mogelijke Matlab maatregelen RR: reset N3Mes, N3Loc
           N3Mes = N3Mes + N3Mat*4
           N3Loc = N3Loc + N3Mat
        else
           Call ScanFile (In, NamFil(10), Iout1, 1, 'MLST', N3MES, .true. )
           Call ScanFile2 (In, NamFil(10), Iout1, 1, 'RRMS ', N3LOC )
           ! ivm mogelijke Matlab maatregelen RR: reset N3Mes, N3Loc
           N3Mat = N3Loc
           N3Mes = N3Mes + N3Loc*4
           N3Loc = N3Loc + N3Mat
        endif
      endif

! Measures Delft3D-FLOW
      NSMES_D3D = 0
      NSCV_D3D = 0
      Inquire (FILE = NamFil(34), EXIST = FNMEXT)
      if (FNMEXT) then
         In = DioNewLun()
         Call ScanFile2 (In, NamFil(34), Iout1, 1, 'SBMS ', NSMES_D3D )
         KeywordArray(1) = 'ty'
         KeywordArray(2) = 'nv'
         MaxNumberArray(1) = 2
         MaxNumberArray(2) = 1
         CheckArray(1) = .true.
         CheckArray(2) = .false.
         Call ScanFile4 (In, NamFil(34), Idebug, Iout1, 1, 'SBMS', KeywordArray, MaxNumberArray, CheckArray,2)
         NSCV_D3D = max (1, MaxNumberArray(2))
      endif
      NSMes = NSMes_SBK + NSMes_D3D
      NSCV = max(NSCV_SBK, NSCV_D3D)

! ivm mogelijke Matlab maatregelen Sobek: reset NDecV
      NDecV = NDecV + NSMes + N3Mat*4
! als geen WQ, dan max. aantal parameters in HIS file lager dan oorspr. NParQ
      If (.not. useWQ) NParQ = NParS

!     Alloceer arrays
      RtcInitialize = AllocLocationArrays (Iout1)
      if (RtcInitialize .eq. 0) RtcInitialize = AllocDecisionArrays (IOut1)
      if (RtcInitialize .eq. 0) RtcInitialize = AllocMeasureArrays  (Iout1)
      if (RtcInitialize .ne. 0) Return
      If (NPar3Rsvp .gt.0) then
         RtcInitialize = AllocReservoirArrays (Iout1, NPar3Rsvp)
         If (RtcInitialize .ne. 0) then
            call write_error_message_rtc (RtcInitialize, 0,' Allocating Reservoir data',' ',IOUT1)
            Return
         Endif
      Endif

! September 2001 (moved from GETFIL): Initialise stuurmodule
!     WRITE(*,*) ' Command line: ', CMDLINE(1:len_trim(CMDLINE))
      IF (SOBEKMODE) THEN
        CALL INITCT (CMDLINE(1:len_trim(CMDLINE)), IDCNT, ISTAT)
        IF (ISTAT .LT. 0)  THEN
            WRITE (STRING,'(A, I3)') 'From INITCT: IOStatus: ', ISTAT
            call write_error_message_rtc (990, 0, ' RTC', STRING, IOUT1)
            RtcInitialize = 990
            Return
        ENDIF

        IdControlModule = IDCNT

        RTCInitMode = .true.
        RTCFirstProc= .false.

        Call InitFp (RTCFirstProc, RTCInitmode)
      ENDIF

! *********************************************************************
! *** Read Lokatie datafiles 3B, Sobek, Precipitation, External
! *********************************************************************

      If (SOBEKMODE) then
! Sobek lokaties
         NSOBEK = 0
         If (USESBK) Then
           IN = DioNewLun()
           Call OPENFL(IN, NAMFIL(2), IOUT1, 1)
           RtcInitialize = RDL_SB (IDEBUG, IN, IOUT1)
           If (RTCInitialize .ne. 0) Return
           Close(IN)
         Endif

! 3B lokaties
         ND3BID = 0
         If (USE3B) Then
           IN = DioNewLun()
           Call OPENFL(IN, NAMFIL(3), IOUT1, 1)
           RtcInitialize = RDL_3B  (IDEBUG, IN, IOUT1)
           If (RTCInitialize .ne. 0) Return
           Close(IN)
         Endif
      Endif

! Delft3D lokaties
      ND3D = 0
      If (N3D>0) Then
         IN = DioNewLun()
         Call OPENFL(IN, NAMFIL(32), IOUT1, 1)
         RtcInitialize = RDL_3D (IDEBUG, IN, IOUT1)
         If (RTCInitialize .ne. 0) Return
         Close(IN)
      Endif

      If (SOBEKMODE) then
! Precipitation lokaties
         NPRECP = 0
         If (USEPRE) Then
           IN = DioNewLun()
           Call OPENFL(IN, NAMFIL(4), IOUT1, 1)
           RtcInitialize = RDL_PR (IDEBUG, IN, IOUT1)
           If (RTCInitialize .ne. 0) Return
           Close(IN)
         Endif

! Externe data lokaties: = wind data lokaties
         NEXTD = 0
         NEXTHD = 0
         If (USEEXT .or. USEW) Then
           IN = DioNewLun()
           Call OPENFL(IN, NAMFIL(5), IOUT1, 1)
           RtcInitialize = RDL_EX (IDEBUG, IN, IOUT1)
           Close(IN)
           If (RTCInitialize .ne. 0) Return
         Endif
! Check number of allocated possible datasets, at the moment linked to NParI=11
         if (NExtHd .gt. 2*NParI) then
            call write_error_message_rtc (913, 0, 'Rtc',' NEXTHD datasets for external HIS files',IOUT1)
            RtcInitialize = 913
            If (RTCInitialize .ne. 0) Return
         Endif

      Endif   ! SOBEKMODE
! Wind
      if (idebug > 0) Write(Idebug,*) ' RTC wind0 ', NExtD
      If (USEW .and. NExtD .eq. 0) Then
         NWIND = 1
         ID_EXT(NWIND) ='Global wind'
         NEXTD = NWIND
         if (idebug > 0) Write(Idebug,*) ' RTC wind1 ', NWind, Id_Ext(Nwind)
      Endif

      If (SOBEKMODE) then
! Sobek WQ lokaties
         NSOBWQ = 0
         If (USEWQ) Then
           IN = DioNewLun()
           Call OPENFL(IN, NAMFIL(13), IOUT1, 1)
           RtcInitialize = RDL_WQ (IDEBUG, IN, IOUT1)
           If (RTCInitialize .ne. 0) Return
           Close(IN)
         Endif

! Read Bui file: als tijden events uit BUI/RKS file (Timf3B=true) en/of als rainfall prediction
         If (USEP .or. TimF3B  .or. Nevent .gt. 1) Then
           IN = DioNewLun()
           Call OPENFL(IN, NAMFIL(23), IOUT1, 1)
           RTCInitialize = RDRAIN (IDEBUG, IN, IOUT1)
           If (RTCInitialize .ne. 0) Return
           Close(IN)
         Else
           IEvent = 1
           EvStrt(Ievent,1) = IYear
           EvStrt(Ievent,2) = IMo
           EvStrt(Ievent,3) = IDay
           EvStrt(Ievent,4) = IHour
           EvStrt(Ievent,5) = IMin
           EvStrt(Ievent,6) = ISec
         Endif

! Scan Wind file and Allocate Wind arrays, given start dates and duration of events
         If (USEW) Then
            IN = DioNewLun()
            Call OPENFL(IN, NAMFIL(24), IOUT1, 1)
            RtcInitialize = ScanWind (IDEBUG, IN, IOUT1, IflRtnRtc)
            If (RTCInitialize .ne. 0) Return
            Close(IN)
         EndIf
!     write(*,*) ' NTim NTIMW = ',NTIM, NTIMW
         If (.not. WindUseTableModule) RtcInitialize = AllocWindArrays (Iout1)
         If (RTCInitialize .ne. 0) Return

      Endif  ! SOBEKMODE

! *********************************************************************
! *** Read beslisparameter file
! *********************************************************************

! now put wind/precipitation, WQ off, and switch on in RdPara if they are used
      UsePre =.false.
      UseP   =.false.
      UseExt =.false.
      UseW   =.false.
      UseWQ  =.false.
      Inquire (FILE = NamFil(8), EXIST = FNMEXT)
      if (FNMEXT .OR. SOBEKMODE) then
        IN = DioNewLun()
        Call OPENFL(IN, NAMFIL(8), IOUT1, 1)
        RtcInitialize = RDPARA (IDEBUG, IN, IOUT1, IflRtnRtc)
        if (Matwq .and. MatlabNrWQPar .gt. 0) UseWQ  =.true.
        if (MatRain .or. MatRainPredict) UsePre =.true.
        UseP = UsePre
        If (RTCInitialize .ne. 0) Return
        Close(IN)
        If (NPar3Rsvp .gt. 0) then
            In = DioNewLun()
            Call Openfl(IN, NAMFIL(27), IOUT1, 1)
            RtcInitialize = ReadReservoirInput (RtcTableHandle, In, Iout1, Idebug, NPar3Rsvp)
            If (RtcInitialize .ne. 0) then
               call write_error_message_rtc (RtcInitialize, 0,' Reservoir data ',' ',IOUT1)
               Return
            Endif
            Close(In)
        Endif
      Endif

! *********************************************************************
! *** Read Sobek measure file
! *********************************************************************

      NSMSID = 0
      NSMEAS = 0
      LOWSPRI= 1
      NSMEAS_SBK = 0
      nrSbkDllmeasures = 0
      MATUSE = .FALSE.
      If (SOBEKMODE .and. USESBK) Then
        IN = DioNewLun()
        Call OPENFL(IN, NAMFIL(9), IOUT1, 1)
        if (OldFormatMeasureFiles) then
          RtcInitialize = RdSMeas_OldFormat (IDEBUG, IN, IOUT1, NSMEAS_SBK)
        else
          RtcInitialize = RdSMeas (IDEBUG, IN, IOUT1, NSMEAS_SBK)
        endif
        If (RTCInitialize .ne. 0) Return
        Close(IN)
      Endif
      NSMSID_SBK = NSMSID
      if (idebug .gt. 0) write(idebug,*) ' NsMeas =', nsmeas

! *********************************************************************
! *** Read 3B measure file
! *********************************************************************

      N3MLOC = 0
      N3MEAS = 0
      LOW3PRI= 0
      nrRRDllmeasures = 0
      If (SOBEKMODE .and. USE3B) Then
        IN = DioNewLun()
        Call OPENFL(IN, NAMFIL(10), IOUT1, 1)
        if (OldFormatMeasureFiles) then
          RtcInitialize = Rd3BMs_OldFormat (IDEBUG, IN, IOUT1)
        else
          RtcInitialize = Rd3BMs (IDEBUG, IN, IOUT1)
        endif
        If (RTCInitialize .ne. 0) Return
        Close(IN)
      Endif
      if (idebug .gt. 0) write(idebug,*) ' N3Meas =', n3meas
      if (idebug .gt. 0) write(idebug,*) ' N3MLoc =', n3mloc
      if (idebug .gt. 0) write(idebug,*) ' N3MatLoc =', n3matloc


! *********************************************************************
! *** Read Delft3D-FLOW measure file
! *********************************************************************

      NSMEAS_D3D = 0
      Inquire (FILE = NamFil(34), EXIST = FNMEXT)
      If (FNMEXT) Then
        IN = DioNewLun()
        Call OPENFL(IN, NAMFIL(34), IOUT1, 1)
        RtcInitialize = RdSMeas (IDEBUG, IN, IOUT1, NSMEAS_D3D)
        If (RTCInitialize .ne. 0) Return
        Close(IN)
      Endif
      NSMSID_D3D = NSMSID - NSMSID_SBK

      NSMEAS = NSMEAS_SBK + NSMEAS_D3D
! *** first initialisations before first event
      Idebug = 0
! TCN
! for safety, allocate with dimension NSMeas*2
      if (UseTCN .and. ReadCsvFile .ne. '') then
          Allocate (CsvReadid(NsMeas*2))
          Allocate (CsvReadValue(NsMeas*2))
          CsvReadId = ''
          CsvReadValue = -999.
      endif

! Check license Sobek
! ARS 12158/12162
! Set everything allowed (Taiwan March 2004, temporary)
      RTCLicenseReturnCode = 0
      if (SOBEKMODE) then
         RtcInitialize = 0
         NrFlowRRMeasures = NSMsId + N3BMs
      Endif
      If (RTCInitialize .ne. 0) Return

!     Open MATLAB if MATUSE is TRUE
#if (defined(USE_MATLAB))
      RTCInitialize = OpenMatlab (IdebugLun)
      If (RTCInitialize .ne. 0) then
         call write_error_message_rtc (994, 0, ' MATLAB could not be started', ' ',Iout1)
         Return
      Endif
#endif

      RTCJulianTimestep = RtmSiz / 86400D0

      RTCCDATE = ' '
      RTCCTIME = ' '
      CALL DATE_AND_TIME (RTCCDATE,RTCCTIME,RTCCZONE,RTCtime_fields)
      RTCIDateAct   = RTCtime_fields(1)*10000 + RTCtime_fields(2) * 100 + RTCtime_fields(3)
      RTCITimeAct   = RTCtime_fields(5)*10000 + RTCtime_fields(6) * 100 + RTCtime_fields(7)
      RTCJulianStart = Julian (RTCIDateAct, RTCITimeAct)

      RTC_NEvent = Nevent

      RTCInitialize = 0
      If (Nevent .le. 0) RTCInitialize = -1

      if ( RTCInitialize == 0 ) then
          RTCInitialize = RTCInitExternals (RTC_RunId)
      endif

  return
  end function RTCInitialize


! ************************************************************************************
!    RTCInitExternals - initialize for HarmonIT
! ************************************************************************************

  Integer function RTCInitExternals (RTC_RunId)

  use wl_open_mi_support
  use rtc_open_mi_support

  Implicit none

  Integer RTC_Runid

  character(Len=oes_id_len), dimension(:), pointer :: Stations

  Integer teller, oesRTCElmset, oesCFElmSet, oesRRQuant, oesCFQuant
  Logical Success

  RTCInitExternals = 0

! HarmonIT RR OpenWaterLevels (incoming) and Groundwater levels
  success = DH_AllocInit (ND3BID, Stations, '')
  if (Success .and. ND3BID .gt. 0) then
      Do teller = 1, nD3BID
        Stations(teller) = ID_D3B(teller)
      Enddo
      oesRTCElmset = OesElmsetFindOrCreate(RTC_RunId,RTCRRLocationsElmSet,stations)
      if (oesRTCElmset > 0) then
         OesRRQuant = OesExchItemCreate(RTC_RunId,RTCOpenwaterlevel,RTCRRLocationsElmSet,oes_accepting)
         OesRRQuant = OesExchItemCreate(RTC_RunId,RTCGroundwaterlevel,RTCRRLocationsElmSet,oes_accepting)
      endif
      DeAllocate(Stations)
  endif
! End HarmonIT RR OpenWaterLevels, Groundwater levels

! RTC CF Calcpoint quantities
  success = DH_AllocInit (NSobek, Stations, '')
  if (Success .and. NSobek .gt. 0) then
      Do teller = 1, nSobek
        Stations(teller) = ID_Sbk(teller)
      Enddo
      oesCFElmset = OesElmsetFindOrCreate(RTC_RunId,RTCCalcPointElmSet,stations)
      if (oesCFElmset > 0) then
         OesCFQuant = OesExchItemCreate(RTC_RunId,RTCWaterlevel,RTCCalcPointElmSet,oes_accepting)
         OesCFQuant = OesExchItemCreate(RTC_RunId,RTCSurfaceArea,RTCCalcPointElmSet,oes_accepting) ! CF storage surface area
         OesCFQuant = OesExchItemCreate(RTC_RunId,RTCWaterDepth, RTCCalcPointElmSet,oes_accepting) ! CF water depth
      endif
      DeAllocate(Stations)
  endif
! End CF water levels

! CF discharges reach segments
  success = DH_AllocInit (NSobek, Stations, '')
  if (Success .and. NSobek .gt. 0) then
      Do teller = 1, nSobek
        Stations(teller) = ID_Sbk(teller)
      Enddo
      oesCFElmset = OesElmsetFindOrCreate(RTC_RunId,RTCReachSegElmSet,stations)
      if (oesCFElmset > 0) then
         OesCFQuant = OesExchItemCreate(RTC_RunId,RTCDischarge,RTCReachSegElmSet,oes_accepting)
      endif
      DeAllocate(Stations)
  endif
! End CF discharges reach segments

! RTC CF structure quantities
  success = DH_AllocInit (NSobek, Stations, '')
  if (Success .and. NSobek .gt. 0) then
      Do teller = 1, nSobek
        Stations(teller) = ID_Sbk(teller)
      Enddo
      oesCFElmset = OesElmsetFindOrCreate(RTC_RunId,RTCStructures,stations)
      if (oesCFElmset > 0) then
         OesCFQuant = OesExchItemCreate(RTC_RunId,RTCForcedifference,   RTCStructures,oes_accepting) ! CF force difference per unit width
         OesCFQuant = OesExchItemCreate(RTC_RunId,RTCStructurePar,      RTCStructures,oes_accepting) ! CF structure par
         OesCFQuant = OesExchItemCreate(RTC_RunId,RTCHead,              RTCStructures,oes_accepting) ! CF head
         OesCFQuant = OesExchItemCreate(RTC_RunId,RTCWaterleveldown,    RTCStructures,oes_accepting) ! CF water level down
         OesCFQuant = OesExchItemCreate(RTC_RunId,RTCWaterlevelup,      RTCStructures,oes_accepting) ! CF water level up
         OesCFQuant = OesExchItemCreate(RTC_RunId,RTCVelocity,          RTCStructures,oes_accepting) ! CF velocity structure
         OesCFQuant = OesExchItemCreate(RTC_RunId,RTCDischarge,         RTCStructures,oes_accepting) ! CF discharge structure
         OesCFQuant = OesExchItemCreate(RTC_RunId,RTCFlowArea,          RTCStructures,oes_accepting) ! CF flow area
         OesCFQuant = OesExchItemCreate(RTC_RunId,RTCGateOpeningHeight, RTCStructures,oes_accepting) ! CF gate opening height
         OesCFQuant = OesExchItemCreate(RTC_RunId,RTCGateLowerEdgeLevel,RTCStructures,oes_accepting) ! CF gate lower edge level
         OesCFQuant = OesExchItemCreate(RTC_RunId,RTCCrestWidth,        RTCStructures,oes_accepting) ! CF crest width
         OesCFQuant = OesExchItemCreate(RTC_RunId,RTCCrestLevel,        RTCStructures,oes_accepting) ! CF crest level
      endif
      DeAllocate(Stations)
  endif
! End CF structure quantities
! CF pump capacity
  success = DH_AllocInit (NSobek, Stations, '')
  if (Success .and. NSobek .gt. 0) then
      Do teller = 1, nSobek
        Stations(teller) = ID_Sbk(teller)
      Enddo
      oesCFElmset = OesElmsetFindOrCreate(RTC_RunId,RTCPumps,stations)
      if (oesCFElmset > 0) then
         OesCFQuant = OesExchItemCreate(RTC_RunId,RTCDischarge,RTCPumps,oes_accepting)
      endif
      DeAllocate(Stations)
  endif
! End CF pump capacity

! Other external incoming data through HarmonIT
!
!  to be added
!
!  external data
!  WQ data
!
! end other external incoming data


! HarmonIT outgoing data
    RTCInitExternals = InitOutCFSetpoints(RTC_RunId)
    If (RtcInitExternals .ne. 0) Return
    RtcInitExternals = InitOutRRSetpoints(RTC_RunId)
  ! more? -> to be added
! End HarmonIT outgoing data



  Return
  End function RTCInitExternals


! ************************************************************************************
!    RTCInitializeEvent
! ************************************************************************************

  Integer function RTCInitializeEvent  (RTC_RunId,RTC_Ievent,RTC_NTimestepEvent)


  Implicit none
  Integer  RTC_RunId, RTC_Ievent, RTC_NTimestepEvent

  Integer          Rtc_Init1, ReadDioPltExt
  Integer          iExt, IDateAct, ITimeAct, ijulian, success
  Double Precision RDUM, CalcDiffDays, RTC_Timnul

  Double Precision Julian, Modified_Julian_fromJulian

        Ievent = RTC_IEvent
        IDEBUG = IdebugLun
        RTCInitializeEvent = 0

        RTCInitMode = .true.
        IF (SOBEKMODE) THEN
          RTCFirstProc = .false.
          Call InitFp (RTCFirstProc, RTCInitmode)
        ENDIF

        If (IEvent .eq. 1) then
           RTC_TIMNUL = 0.0
           RTC_TIMOLD = -1.
           RTC_TIMNEW =  0.
        Else
!          Bij volgende event in reeks: zet Timold(event2) = Timold (eind event1) + 2 rekentijdstappen (2*Deltat)
!     ARS xxxx Dec 1999: ipv 1 dag ertussen, minimaal 2 rekentijdstappen
!           Netjes doen:
            Call NXTSTP (IDEBUG, IYEAR,IMO,IDAY, IHOUR, IMIN, ISEC, RSEC, IDHR, IDMIN, RDSEC)
            Call NXTSTP (IDEBUG, IYEAR,IMO,IDAY, IHOUR, IMIN, ISEC, RSEC, IDHR, IDMIN, RDSEC)
            RTC_TIMNEW = dble(IYEAR) * 10000.0d0 + dble(IMO) * 100.0d0 + dble(IDAY) + &
                         dble(IHOUR) / 100.0d0 + dble(IMIN) / 10000.0d0 + dble(ISEC) / 1000000.0d0
            RTC_TIMOLD = RTC_TIMNEW
            RTC_TIMNUL = RTC_TIMNEW
            if (idebug > 0) write(idebug,'(A,3F16.6)') ' RTC_TimNul etc',RTC_TimNul, RTC_TimOld, RTC_TimNew
!
        Endif

        if (idebug > 0) write(idebug,'(A,I5,2F16.6)')  ' Start Ievent',ievent, RTC_TimOld, RTC_TimNew
!       pause

        IF (SOBEKMODE) THEN
          If (RTCFirstProc) then
!           RTC eerste in initialisatiefase: komt niet voor
!           Write(*,*) ' RTC first not possible'
            call write_error_message_rtc (941, 0,' ',' ',IOUT1)
            RTCInitializeEvent = 941
            Return
          else
!           Write(*,*) ' RTC not first; wait'
            if (idebug > 0) write(idebug,'(A,2F16.6)')  ' Stepct RTCFirstProc ',RTC_TimOld, RTC_TimNew
            Call STEPCT (RTC_TIMOLD, RTC_TIMNEW, IDCNT, ISTAT, RTCInitMode, RTCcrashed)
            if (RTCcrashed) goto 9999
          endif
        ENDIF

!       Test D3D coupling:  wait a little moment
!        CALL GPSLEEP (1)
!
        ITMSTP = 0
        RSecondsSinceStartFirstEvent = 0D0

!       Initialisation communication with D3D-Flow
        If (CoupledToDelft3D .and. .not. SOBEKMODE) then
          call SyncRtcFlow_Init(n2steps, lfout, ND3D > 0, i3ddat, itstart, RTMSIZ)
          If (lfout) then
            If (n2steps .eq. -1) then
              call write_error_message_rtc (945, 0, ' SyncRtcFlow_Init', ' Shutdown from Flow', IOUT1)
              RTCInitializeEvent = 945
            Elseif (ND3D > 0 .and. .not. commFLOWtoRTC) then
              call write_error_message_rtc (946, 0, ' SyncRtcFlow_Init', ' Delft3D-FLOW does not provide required quantities', IOUT1)
              RTCInitializeEvent = 946
            Elseif (commFLOWtoRTC .and. ND3D == 0) then
              call write_error_message_rtc (946, 0, ' SyncRtcFlow_Init', ' Delft3D-FLOW provides data not required by RTC', IOUT1)
              RTCInitializeEvent = 946
            Else
              call write_error_message_rtc (946, 0, ' SyncRtcFlow_Init', ' Init Error', IOUT1)
              RTCInitializeEvent = 946
            Endif
            Return
          Else
            D3DActive = .TRUE.
          Endif
          LASTTM = n2steps
          !
          ! set date/time based on i3ddat received from Delft3D-FLOW
          !
          RTCJulianTimestep = RtmSiz / 86400d0 ! pre-compute RTCJulianTimestep for computing current time
          !
          ijulian  = ymd2jul(i3ddat)
          success  = mjd2date(jul2mjd(ijulian,itstart*RTCJulianTimestep),iyear,imo,iday,ihour,imin,rsec)
          isec     = int(rsec)
        Endif
! Endif moved to here because of issue 49886
        NrSecondsSinceStartFirstEvent = RSecondsSinceStartFirstEvent
        NrTimestepsSinceStartFirstEvent = NINT(RSecondsSinceStartFirstEvent/RtmSiz)
        !
        ! set time step based on rtmsiz received from Delft3D-FLOW
        !
        RTCJulianTimestep = RtmSiz / 86400d0
        rdsec = rtmsiz
        if (rdsec >= 60d0) then
           idmin = int(rdsec/60d0)
           rdsec = rdsec - real(idmin,8)*60d0  ! adjusted GP, issue 49886
        else
           idmin = 0
        endif
        idsec = nint(rdsec)
        if (idmin >= 60) then
           idhr = idmin/60
           idmin = idmin-60*idhr
        else
           idhr = 0
        endif
        !
!       EvStrt(1,1) = iyear
!       EvStrt(1,2) = imo
!       EvStrt(1,3) = iday
!       EvStrt(1,4) = ihour
!       EvStrt(1,5) = imin
!       EvStrt(1,6) = isec

!       afsluiting initialisatiemode (t=0 voor idum=1);
        If (ievent .eq. 1) then
           RTC_TIMNEW = 0.1
        Else
           RTC_TIMNEW = RTC_TimNul + DELTAT
        Endif
!       pause ' Voor Stepct einde Initialisatiemode'
        if (idebug > 0) write(idebug,'(A,2F16.6)')  ' Stepct Initialisatie ',RTC_TimNul, RTC_TimNew
        IF (SOBEKMODE) THEN
          Call STEPCT (RTC_TimNul, RTC_TimNew,IDCNT,ISTAT,RTCInitMode, RTCcrashed)
        ENDIF
!       pause ' Na Stepct einde Initialisatiemode'
!       reset voor rekenmode weer op de echte RTC_TimOld
        RTC_TIMNEW = RTC_TIMOLD
        RTCInitMode = .false.
        RTCFirstProc= .true.
        IF (SOBEKMODE) THEN
          Call InitFp (RTCFirstProc, RTCInitmode)
        ENDIF

! set JulianStarttime
        IDateAct   = iyear *10000 + imo* 100 + iday
        ITimeAct   = ihour *10000 + imin * 100 + isec
!       IDateAct   = EvStrt(1,1)*10000 + EvStrt(1,2)* 100 + EvStrt(1,3)
!       ITimeAct   = EvStrt(1,4)*10000 + EvStrt(1,5)* 100 + EvStrt(1,6)
        RTCJulianStartDate = Julian (IDateAct, ITimeAct)
        RTCModJulianTime = Modified_Julian_fromJulian(RTCJulianStartDate)

!       write(*,*) ' Init1'
!       Idebug = IdebugLun
        RtcInitializeEvent = RTC_INIT1 (IOUT1,  IDEBUG, INT(RtmSiz), &
                    PrecipitationDataset, WindDataset, &
                    ParameterDataSet, OutRRDataSet, OutSbkDataSet, &
                    IEVENT, IBar0, IScren)
        If (RTCInitializeEvent .ne. 0) Return
        if (idebug > 0) write(Idebug,'(A,2F16.6)')  ' Na Init1', RTC_TimOld, RTC_TimNew

! compute factor between calculation timestep ITMSIZ and rainfall timestep NRSRAI
! of course only if rainfall file is used (USEP=TRUE)
        If (USEP) Then
          If (ITMSIZ .LE. NRSRAI) Then
             IBUI_ITMSTP = 1
! ARS 10827, correction if one simulates subset of event
             if (idebug > 0)  Write(IDEBUG,*) ' RTC_TimNew TimStb', RTC_TimNew, TimStb
             If (RTC_TimNew .gt. TimStb) then
                Time_Ratio = (RTC_TimNew-TimStb) * 86400. / NrsRai
                Ibui_itmstp = 1 + Time_Ratio
             Endif
! End ARS 10827
             Time_Ratio = NRSRAI / ITMSIZ
             IBUI_rem = Time_Ratio
             if (idebug > 0)  Write(IDEBUG,*) ' IBui_itmstp', IBUI_itmstp
             if (idebug > 0)  Write(IDEBUG,*) ' Time_Ratio', Time_Ratio, IBUI_rem
          ELSE
             WRITE (*,*)  ITMSIZ, NRSRAI
             call write_error_message_rtc (923, 0,'Predict',' ',IOUT1)
             RTCInitializeEvent = 923
          Endif
        Endif
        If (RTCInitializeEvent .ne. 0) Return

        If (IEVENT .GT. 1) Then
           RDUM = CalcDiffDays (1, ievent, evstrt, nevnt)
           RSecondsSinceStartFirstEvent = 86400 * RDUM
           NrSecondsSinceStartFirstEvent = RSecondsSinceStartFirstEvent
           NrTimestepsSinceStartFirstEvent = NINT(RSecondsSinceStartFirstEvent/RtmSiz)
!          if (idebug > 0) write(*,*) ' rdum NrSecondsSinceStartFirstEvent ', rdum, NrSecondsSinceStartFirstEvent
        Endif

! *********************************************************************
! Init DIO on-line Streams
! *********************************************************************

! DIO initialisation
        Inquire (FILE = Namfil(28), EXIST = FnmExt)
        If (FnmExt) then
           Call DioInit (Namfil(28))
           ITempFileUnit = DioNewLun()
           Open (ITempFileUnit,File='DioDumpRTC.Txt',Status='unknown')
           Call DioConfigDump(ITempFileUnit)
           Close(ITempFileUnit)
        else
           write(*,*) ' DioConfigIni file not found'
        endif

! on-line streams
        If (SOBEKMODE) then
           if (Use3B)  call InitDioPlt(inPlt_3B)
           if (UseSbk) call InitDioPlt(inPlt_Sbk)
           If (UseWq)  call InitDioPlt(inPlt_Wq)
        Endif
        If (ND3D > 0)  call InitDioPlt(inPlt_D3D)
        If (SOBEKMODE) then
           If (UseExt .and. NExtHDataset .gt. 0) then
              Do IExt=1,NExtHDataset
                 Call InitDioPlt(ExtHisDataset(iExt))
                 If (Ievent .eq. 1) then
                    ! Map desired locations and parameters to HIS files
                    ! And get information which timesteps are available (JulianDates)
                    RtcInitializeEvent = ReadDioPltExt (ExtHisDataSet(IExt), HisDataSetFile(IExt), IExt,  &
                                        IDEBUG, ITMSTP, IOUT1, ResExt,  DoHeaderExt(IExt), &
                                        HisDataSet, HisParExt, HisLocExt, &
                                        HisDataSetTimes, HisDataSetNParLocTimes, &
                                        nExtH, NextHD, NExt, NExtD, NParE, NTimHis)
                    if (RtcInitializeEvent .ne. 0) Return
                 Endif
              Enddo
           Endif

           if (Use3B)  call InitDioPlt(outPlt_3B)
           if (UseSbk) call InitDioPlt(outPlt_Sbk)
        Endif


   9999 Continue
        RTC_NTimestepEvent= LastTm
        RTCInitializeEvent = 0
        IDEBUG = 0
        If (RTCCrashed) RTCInitializeEvent = -1

  return
  end function RTCInitializeEvent


! ************************************************************************************
!    RTCInitializeTimestep
! ************************************************************************************

  Integer function RTCInitializeTimestep  (RTC_RunId,RTC_Ievent,RTC_Timestep)

  Implicit none
  Integer  RTC_RunId, RTC_Ievent, RTC_Timestep

  Integer  RdBar, ReadDioPlt, ReadDioPltExtSelection, CheckAndSetSeriesReference
  Integer  Idum, iloc, iext, i, i2

  Integer           idummy
  Double precision  rdummy

        IEvent = RTC_IEvent
        Itmstp = RTC_Timestep-1
        Idum   = RTC_Timestep


!         write(*,*) ' RTC timestep Idum',idum
!         If (IEvent .eq. 2 .and. Idum .gt. 84) Idebug=Idebuglun
!         If (IDum .gt. 120) Idebug=0

          Idebug = 0
          If (IDum .ge. DebugFromTimestep  .and. Idum .le. DebugToTimestep ) Idebug = IdebugLun
          If (IDum .ge. DebugFromTimestep2 .and. Idum .le. DebugToTimestep2) Idebug = IdebugLun

! *********************************************************************
! ***     Wachten op RR-CF resultaten?
! *********************************************************************

          if (idebug > 0) WRITE(IDEBUG,*) ' Tijdstap =',IDUM, ITMSTP, Lasttm
          IF (SOBEKMODE) THEN
            If (RTCFirstProc .and. idum .eq. 1) then
!             RTC eerste in rekenfase eerst; komt niet voor !!!! of toch wel, om RTC uitvoer t=0 te genereren?
!             Write(*,*) ' RTC first is not allowed'
!             STOP
            else
              RTC_TIMOLD = RTC_TIMNEW
!             RTC_TIMNEW = RTC_TIMNEW + DELTA2
              if (idebug > 0) WRITE(IDEBUG,*) ' Stepct2 Timold',RTC_TIMOLD,RTC_TIMNEW,DELTA2
              if (idebug > 0) Write(Idebug,*) ' RTC wait timestep', Idum, RTC_TIMOLD,RTC_TIMNEW,DELTA2
!             Write(*,*) ' RTC wait timestep', Idum, RTC_TIMOLD,RTC_TIMNEW,DELTA2
              Call STEPCT (RTC_TimOld, RTC_TimNew,IDCNT,ISTAT,RTCInitMode, RTCcrashed)
              if (idebug > 0) WRITE(IDEBUG,*) ' Exited Stepct2; continue computations '
!             Write(*,*) ' Continue computations', RTC_TIMOLD,RTC_TIMNEW,DELTA2
              If (RTCcrashed)  Goto 9999
              If (ISTAT .LT. 0)  Then
                WRITE (*,'(A,I3)') ' Error 2 Stepct in RTC', ISTAT
                WRITE (IFlRtnRtc,'(I5)') ISTAT
              Endif
            endif
          ENDIF

! May 1998: addition for different calculation timestep and rainfall timestep
          If ((IDUM .GT. 1) .AND. USEP) Then
             IBUI_rem = IBUI_rem -1
             If (Ibui_rem .LE. 0) Then
                IBUI_itmstp = IBUI_Itmstp + 1
                IBUI_rem = Time_Ratio
             Endif
          Endif
! End May 1998

          If (NEVENT .EQ. 1 .AND. IDUM .GT. 1) Then
!            write(*,*) ' call 2'
             Call RTC_WRLOGO (IScren, IDUM-1,LASTTM,NEVENT,IBar0,SOBEKMODE)
          Endif

! *********************************************************************
! *** Read Sobek results, if necessary
! *** Store all SOBEK results in array ALRSBK
! *** Timeshift the previous results
! *********************************************************************
          If (USESBK .AND. NSOBEK .GT. 0) Then
! old
!           IN = INXFIL(11)
!           Call OPENFL(IN, NAMFIL(11), IOUT1, 2)
!           Write(*,*) ' Voor RdHis ITmstp=',ITmStp
!           Call RDHIS (IN, IDEBUG, ITMSTP, IOPTS, IOUT1,  &
!                       NSBK, NLocHis, NPARS, NSOBEK,      &
!                       ' Sobek', ID_SBF, IDPARS,          &
!                       ID_SBK, SbkLoc, RESSBK, ResRead)
!           Close (IN)
! using DIO
!            Write(*,*) ' Before reading Sobek results'

! test voor RTC-1D2D probleem
!           RTCInitializeTimestep = 999
!           RtcCrashed   = .true.
!           RtcSelfCrash = .true.
!           Call CrashCt (IdCnt, .false. )
!           Goto 9999
!
            RTCInitializeTimestep = ReadDioPlt (inPlt_Sbk, NAMFIL(11), &
                            IDEBUG, ITMSTP, IOUT1,        &
                            NSBK, NLocHis, NPARS, NSobek, &
                            ' Sobek', ID_SBF , IDPARS,    &
                            ID_SBK, SbkLoc, RESSBK, DoHeaderInSbk)
            if (RTCInitializeTimestep .ne. 0) Return
            If (Itmstp .eq. 0) Then
              RtcInitializeTimestep = CheckAndSetSeriesReference (Iout1, Idebug, 1)
              if (RTCInitializeTimestep .ne. 0) Return
            Endif
!            Write(*,*) ' Before UPDAR Sobek results'
            Call UPD_AR (' Sobek', RESSBK, ALRSBK, ID_SBK, NSOBEK, &
                           NSBK, NPARS, NTIMS, IDEBUG, MxTmShift(1))
!            Write(*,*) ' After UPDAR Sobek results'
            Endif

! *********************************************************************
! *** Read 3B results, if necessary
! *** Store all 3B results in array
! *** Timeshift the previous results
! *********************************************************************
          If (USE3B .AND. ND3BID .GT. 0) Then
!            Write(*,*) ' Before reading RR results'
            RTCInitializeTimestep = ReadDioPlt (inPlt_3B, NAMFIL(12), &
                            IDEBUG, ITMSTP, IOUT1,        &
                            ND3B, NLocHis, NPAR3, ND3BID, &
                            ' 3B   ', ID_3B , IDPAR3,     &
                            ID_D3B, BBBLoc, RES3B, DoHeaderInRR)
            if (RTCInitializeTimestep .ne. 0) Return
            If (Itmstp .eq. 0) Then
               RtcInitializeTimestep = CheckAndSetSeriesReference (Iout1, Idebug, 2)
               if (RTCInitializeTimestep .ne. 0) Return
            Endif
!            Write(*,*) ' Before UPDAR RR results'
            Call UPD_AR (' 3B   ', RES3B , ALRS3B, ID_D3B, ND3BID, &
                          ND3B, NPAR3, NTIMS, IDEBUG, MxTmShift(2))
!            Write(*,*) ' After UPDAR RR results'
          Endif

! *********************************************************************
! *** Read D3D results, if necessary
! *** Store all D3D results in array ALRSBK
! *** Timeshift the previous results
! *********************************************************************
!
!           Initialisation communication with D3D-Flow (only if first timestep)
!
          If (CoupledToDelft3D .and. SOBEKMODE .and. Itmstp > 0) then
              write(iout1,*) ' SyncRtcFlow_Init'
              call SyncRtcFlow_Init(n2steps, lfout, ND3D > 0, idummy, idummy, rdummy)
              write(iout1,*) ' after SyncRtcFlow_Init'
              If (lfout) then
                If (n2steps .eq. -1) then
                  call write_error_message_rtc (945, 0, ' SyncRtcFlow_Init', ' Shutdown from Flow', IOUT1)
                  RTCInitializeTimestep = 945
                Elseif (ND3D > 0 .and. .not. commFLOWtoRTC) then
                  call write_error_message_rtc (946, 0, ' SyncRtcFlow_Init', ' Delft3D-FLOW does not provide required quantities', IOUT1)
                  RTCInitializeTimestep = 946
                Elseif (ND3D == 0 .and. commFLOWtoRTC) then
                  call write_error_message_rtc (946, 0, ' SyncRtcFlow_Init', ' Delft3D-FLOW provides data not required by RTC', IOUT1)
                  RTCInitializeTimestep = 946
                Else
                  call write_error_message_rtc (946, 0, ' SyncRtcFlow_Init', ' Init Error', IOUT1)
                  RTCInitializeTimestep = 946
                Endif
                Return
             else
                D3DACTIVE = .true.
             Endif
          Endif
!-----------------------------------------------------------------------
!         Read barrier data, is done here to be synchronised in comm.
!-----------------------------------------------------------------------
          If (firstbarrier) then
            If (CoupledToDelft3D .and. numBarriers .gt. 0) then
              In = DioNewLun()
              Inquire (FILE = NamFil(31), EXIST = FNMEXT)
              if (FNMEXT) then
                 RtcInitializeTimestep = RDBAR(NAMFIL(31), IN, IOUT1)
              else
                 RtcInitializeTimestep = ALLOC3DBAR(NumBarriers, 0, IOUT1)
              Endif
              If (RtcInitializeTimestep .ne. 0) Return
            Endif
            firstbarrier = .false.
          Endif
          If (ND3D > 0) Then
!           Now get the D3D data
            RTCInitializeTimestep = ReadDioPlt (inPlt_D3D, NAMFIL(33), &
                            IDEBUG, ITMSTP, IOUT1,        &
                            N3D, NLocHis, NPAR3D, ND3D, &
                            ' D3DFlow', ID_D3DHIS , IDPAR3D,    &
                            ID_D3D, D3DLoc, RESD3D, DoHeaderInD3D)
            if (RTCInitializeTimestep .ne. 0) Return
            If (Itmstp .eq. 0) Then
              RtcInitializeTimestep = CheckAndSetSeriesReference (Iout1, Idebug, 9)
              if (RTCInitializeTimestep .ne. 0) Return
            Endif
            Call UPD_AR (' D3D', RESD3D, ALRS3D, ID_D3D, ND3D, &
                           N3D, NPAR3D, NTIMS, IDEBUG, MxTmShift(9))
          Endif

! *********************************************************************
! *** Read precipitation prediction results, if necessary
! *** Store all precipitation results
! *** Timeshift the previous results
! *********************************************************************

          If (USEPRE .AND. NPRECP .GT. 0) Then
!             Write(*,*) ' Start CmpRainPredicion ', itmstp, ibui_itmstp
             Call CmpRainPrediction (Ievent, Itmstp, Ibui_Itmstp, &
                                     Ibui_Rem, Time_Ratio, Idebug)
!             Write(*,*) ' After CmpRainPredicion ', itmstp, ibui_itmstp
             Call UPD_AR (' Rain', RESPRE, ALRSPR, ID_PRE, NPRECP, &
                             NPRE, NPARP, NTIMS, IDEBUG, MxTmShift(3))
!             Write(*,*) ' After UPDAR_Rain ', itmstp, ibui_itmstp
          Endif

! *********************************************************************
! *** Read external data, if necessary
! *** Store all external data
! *** Timeshift the previous results
! *********************************************************************

          If (USEEXT .AND. NEXTD+NEXTHD .GT. 0) Then
!            External locations - wind prediction
!             Write(*,*) ' Before CmpWindPrediction'
             If (NExtD .gt. 0) then
               if (idebug > 0) Then
                  Write(idebug,*) ' Wind station names'
                  Do ILoc = 1,NWind
                     Write(idebug,*) ILoc, WindNameStat(Iloc)
                  Enddo
                  Write(idebug,*) ' External names and wind stations'
                  Do IExt = 1,NExtd
                     Write(idebug,*) Iext, ID_Ext(Iext), WindLoc2Stat(iext)
                  Enddo
               Endif
               If (.not. WindUseTableModule) Then
                   Call CmpWindPrediction (IEvent, Itmstp, Idebug)
               Else
                   Call CmpWindTablePrediction (IEvent, Itmstp, Idebug, Iout1)
               Endif
             Endif
!            External locations - data from external HIS file
             If (NExtHDataSet .gt. 0) then
                Do IExt=1,NExtHDataSet
!                  Call ReadDioPltExt (ExtHisDataSet(IExt), HisDataSetFile(IExt), IExt,   &
!                                      IDEBUG, ITMSTP, IOUT1, ResExt,  DoHeaderExt(IExt),  &
!                                      HisDataSet, HisParExt, HisLocExt, &
!                                      nExtH, NextHD, NExt, NExtD, NParE)
!                  Get info for current timestep, selected parameters and locations)
!                  If time stamp is not available in HIS file, interpolate!
                   RTCInitializeTimestep = ReadDioPltExtSelection (Ievent, Itmstp, Idebug, Iout1, &
                                                                   ExtHisDataSet(IExt), IExt)
                   if (RTCInitializeTimestep .ne. 0) Return
                Enddo
             Endif
!             Write(*,*) ' After CmpWindPrediction'
             Call UPD_AR  ('Extern', RESEXT, ALRSEX, ID_EXT, NEXT+NEXTHD, &
                            NEXT+NEXTH, NPARE, NTIMS, IDEBUG, MxTmShift(4))
!             Write(*,*) ' After UPDAR-WindPrediction'
          Endif

! *********************************************************************
! *** Read WQ results, if necessary
! *** Store all WQ results in array ALRSWQ
! *** Timeshift the previous results
! *********************************************************************

          If (USEWQ .AND. NSOBWQ .GT. 0) Then
! old
!           IN = INXFIL(14)
!           Call OPENFL(IN, NAMFIL(14), IOUT1, 2)
!!          Write(*,*) ' Voor RdHis WQ ITmstp=',ITmStp
!           Call RDHIS (IN, IDEBUG, ITMSTP, IOPTWQ, IOUT1,  &
!                       NSWQ, NLocHis, NPARQ, NSOBWQ,       &
!                       ' WQ ', ID_WQF, IDPARQ,           &
!                       ID_SWQ, WQLoc, RESWQ, ResRead)
!           Close (IN)
! using DIO
!  gaat nog fout omdat er geen T0string in de Delwaq file staat!!!!
            RTCInitializeTimestep = ReadDioPlt (inPlt_WQ, NAMFIL(14),  &
                             IDEBUG, ITMSTP, IOUT1,        &
                             NSWQ, NLocHis, NPARQ, NSobWq, &
                             ' WQ ', ID_WQF , IDPARQ,     &
                             ID_SWQ, WQLoc, RESWQ, DoHeaderInWQ)
            if (RTCInitializeTimestep .ne. 0) Return
            If (Itmstp .eq. 0) Then
              RtcInitializeTimestep = CheckAndSetSeriesReference (Iout1, Idebug, 5)
              if (RTCInitializeTimestep .ne. 0) Return
!             ARS 9711; also set seriesreference for Matlab
              If (MatWq) then
                Do i=1,MatlabNrWqPar
                  If (MatlabWqParId(i) .NE. '') Then
                     Do I2=1,NParQ
                        Call UpperC(IdParQ(i2))
                        If (MatlabWqParId(i) .eq.  IdParQ(i2)) MatlabWQ(i) = i2
                     Enddo
                     If (MatlabWq(I) .GT. NPARQ .or. MatlabWQ (i) .le. 0) Then
                         Write(IOUT1,*) ' Matlab WQ parameter ', MatlabWqParId(I)
                         call write_error_message_rtc (916, NPARQ, 'Rtc', ' Matlab WQ parameterfile', IOUT1)
                         RtcInitializeTimestep = 916
                         Return
                     Endif
                  Endif
                ENDDO
              Endif
!             End ARS 9711
            Endif
            Call UPD_AR (' WQ ', RESWQ, ALRSWQ, ID_SWQ, NSOBWQ, &
                           NSWQ, NPARQ, NTIMS, IDEBUG, MxTmShift(5))
!           Write(*,*) ' Na Upd_ar'
          Endif



        RTCInitializeTimestep=0
  9999  Continue
        if (RTCCrashed) RTCInitializeTimestep = -1

  return
  end function RTCInitializeTimestep


! ************************************************************************************
!    RTCGetExternals (use RTCGetValues)   To be added
! ************************************************************************************


  Integer Function RTCGetExternals(RTC_RunId,RTC_IEvent,RTC_Timestep)

    use wl_open_mi_support
    use rtc_open_mi_support

    Implicit none
    Integer  RTC_RunId, RTC_Ievent, RTC_Timestep
!
    real, dimension(:), pointer        :: RRValues, CFValues
    logical, dimension(:), pointer     :: mask
    integer                            :: numConnected
    logical                            :: allocSuccess
    integer                            :: i

    ! In OpenMI, always write debug info
    integer                            :: currIDebugSettings

    currIDebugSettings = idebug
    idebug = IdebugLun

! RR open water levels
! Expected unit in input: m
! converted to array ...

    numConnected = 0
    allocSuccess = DH_AllocInit(ND3BID, RRValues, 0.0)
    allocSuccess = DH_AllocInit(ND3BID, mask, .false.)

    if (idebug > 0) write(idebug, *) 'GetExternals, timestep ', RTC_Timestep
    if ( allocSuccess) then
        numConnected = OesGetConnected(RTC_RunId,RTCOpenwaterlevel,RTCRRLocationsElmSet, RRValues, mask)
        if (idebug > 0) write(idebug, *) '    #RR nodes : ', ND3BID, &
                         '    #connected: ', numConnected
        if ( numConnected > 0 ) then
           DO I=1, ND3BID
              if ( mask(I) ) then
                if (idebug > 0) write(idebug, '(A,I4,A,F16.8)') 'i=', i, 'value=', RRValues(I)
                ALRS3B(I,1,NTims) = RRValues(I)
              endif
           ENDDO
        endif
    endif
    deallocate(RRValues, mask)
! End HarmonIT RR-Openwater levels


! RR groundwater levels
! Expected unit in input: m
! converted to array ....
    numConnected = 0
    allocSuccess = DH_AllocInit(ND3BID, RRValues, 0.0)
    allocSuccess = DH_AllocInit(ND3BID, mask, .false.)

    if (idebug > 0) write(idebug, *) 'GetExternals, timestep ', RTC_Timestep
    if ( allocSuccess) then
        numConnected = OesGetConnected(RTC_RunId,RTCGroundwaterlevel,RTCRRLocationsElmSet,RRValues, mask)
        if (idebug > 0) write(idebug, *) '    #RR nodes : ', ND3BID, &
                         '    #connected: ', numConnected
        if ( numConnected > 0 ) then
           DO I=1, ND3BID
              if ( mask(I) ) then
                if (idebug > 0) write(idebug, '(A,I4,A,F16.8)') 'i=', i, 'value=', RRValues(I)
                ALRS3B(i,2,NTims) = RRValues(i)
              endif
           ENDDO
        endif
    endif
    deallocate(RRValues, mask)
! End OpenMI Unpaved groundwater levels

! Flow: water levels
! Expected unit in input: m
! converted to array
    numConnected = 0
    allocSuccess = DH_AllocInit(nSobek, CFValues, 0.0)
    allocSuccess = DH_AllocInit(nSobek, mask, .false.)

    if (idebug > 0) write(idebug, *) 'GetExternals, timestep ', RTC_Timestep
    if ( allocSuccess) then
        numConnected = OesGetConnected(RTC_RunId,RTCWaterlevel,RTCCalcPointElmSet,CFValues, mask)
        if (idebug > 0) write(idebug, *) '    #CalculationPoints: ', nSobek, &
                         '    #connected: ', numConnected
        if ( numConnected > 0 ) then
           DO I=1, nSobek
              if ( mask(I) ) then
                if (idebug > 0) write(idebug, '(A,I4,A,F16.8)') 'i=', i, 'value=', CFValues(I)
                ALRSBK(i,1,NTims) = CFValues(I)
              endif
           ENDDO
        endif
    endif
    deallocate(CFValues, mask)
! End OpenMI water levels CF

! Flow: discharges
! Expected unit in input: m3/s
! converted to array
    numConnected = 0
    allocSuccess = DH_AllocInit(nSobek, CFValues, 0.0)
    allocSuccess = DH_AllocInit(nSobek, mask, .false.)

    if (idebug > 0) write(idebug, *) 'GetExternals, timestep ', RTC_Timestep
    if ( allocSuccess) then
        numConnected = OesGetConnected(RTC_RunId,RTCDischarge,RTCReachsegElmSet,CFValues, mask)
        if (idebug > 0) write(idebug, *) '    #ReachSegments: ', nSobek, &
                         '    #connected: ', numConnected
        if ( numConnected > 0 ) then
           DO I=1, nSobek
              if ( mask(I) ) then
                if (idebug > 0) write(idebug, '(A,I4,A,F16.8)') 'i=', i, 'value=', CFValues(I)
                ALRSBK(i,2,NTims) = CFValues(I)
              endif
           ENDDO
        endif
    endif
    deallocate(CFValues, mask)
! End OpenMI CF reach segment discharges


! Flow: storage surface area
! Expected unit in input: m
! converted to array
    numConnected = 0
    allocSuccess = DH_AllocInit(nSobek, CFValues, 0.0)
    allocSuccess = DH_AllocInit(nSobek, mask, .false.)

    if (idebug > 0) write(idebug, *) 'GetExternals, timestep ', RTC_Timestep
    if ( allocSuccess) then
        numConnected = OesGetConnected(RTC_RunId,RTCSurfaceArea,RTCCalcPointElmSet,CFValues, mask)
        if (idebug > 0) write(idebug, *) '    #Structures: ', nSobek, &
                         '    #connected: ', numConnected
        if ( numConnected > 0 ) then
           DO I=1, nSobek
              if ( mask(I) ) then
                if (idebug > 0) write(idebug, '(A,I4,A,F16.8)') 'i=', i, 'value=', CFValues(I)
                AlRSBK(i,3,NTims) = CFValues(I)
              endif
           ENDDO
        endif
    endif
    deallocate(CFValues, mask)
! End OpenMI CF storage surface area

! Flow: water depth
! Expected unit in input: m
! converted to array
    numConnected = 0
    allocSuccess = DH_AllocInit(nSobek, CFValues, 0.0)
    allocSuccess = DH_AllocInit(nSobek, mask, .false.)

    if (idebug > 0) write(idebug, *) 'GetExternals, timestep ', RTC_Timestep
    if ( allocSuccess) then
        numConnected = OesGetConnected(RTC_RunId,RTCWaterDepth,RTCCalcPointElmSet,CFValues, mask)
        if (idebug > 0) write(idebug, *) '    #Structures: ', nSobek, &
                         '    #connected: ', numConnected
        if ( numConnected > 0 ) then
           DO I=1, nSobek
              if ( mask(I) ) then
                if (idebug > 0) write(idebug, '(A,I4,A,F16.8)') 'i=', i, 'value=', CFValues(I)
                AlRSBK(i,4,NTims) = CFValues(I)
              endif
           ENDDO
        endif
    endif
    deallocate(CFValues, mask)
! End OpenMI CF storage surface area

! Flow: crest level
! Expected unit in input: m AD
! converted to array
    numConnected = 0
    allocSuccess = DH_AllocInit(nSobek, CFValues, 0.0)
    allocSuccess = DH_AllocInit(nSobek, mask, .false.)

    if (idebug > 0) write(idebug, *) 'GetExternals, timestep ', RTC_Timestep
    if ( allocSuccess) then
        numConnected = OesGetConnected(RTC_RunId,RTCCrestLevel,RTCStructures,CFValues, mask)
        if (idebug > 0) write(idebug, *) '    #Structures: ', nSobek, &
                         '    #connected: ', numConnected
        if ( numConnected > 0 ) then
           DO I=1, nSobek
              if ( mask(I) ) then
                if (idebug > 0) write(idebug, '(A,I4,A,F16.8)') 'i=', i, 'value=', CFValues(I)
                AlRSBK(i,5,NTims) = CFValues(I)
              endif
           ENDDO
        endif
    endif
    deallocate(CFValues, mask)
! End OpenMI CF crest level

! Flow: crest width
! Expected unit in input: m
! converted to array
    numConnected = 0
    allocSuccess = DH_AllocInit(nSobek, CFValues, 0.0)
    allocSuccess = DH_AllocInit(nSobek, mask, .false.)

    if (idebug > 0) write(idebug, *) 'GetExternals, timestep ', RTC_Timestep
    if ( allocSuccess) then
        numConnected = OesGetConnected(RTC_RunId,RTCCrestWidth,RTCStructures,CFValues, mask)
        if (idebug > 0) write(idebug, *) '    #Structures: ', nSobek, &
                         '    #connected: ', numConnected
        if ( numConnected > 0 ) then
           DO I=1, nSobek
              if ( mask(I) ) then
                if (idebug > 0) write(idebug, '(A,I4,A,F16.8)') 'i=', i, 'value=', CFValues(I)
                AlRSBK(i,6,NTims) = CFValues(I)
              endif
           ENDDO
        endif
    endif
    deallocate(CFValues, mask)
! End OpenMI CF crest width

! Flow: gate lower edge level
! Expected unit in input: m
! converted to array
    numConnected = 0
    allocSuccess = DH_AllocInit(nSobek, CFValues, 0.0)
    allocSuccess = DH_AllocInit(nSobek, mask, .false.)

    if (idebug > 0) write(idebug, *) 'GetExternals, timestep ', RTC_Timestep
    if ( allocSuccess) then
        numConnected = OesGetConnected(RTC_RunId,RTCGateLowerEdgeLevel,RTCStructures,CFValues, mask)
        if (idebug > 0) write(idebug, *) '    #Structures: ', nSobek, &
                         '    #connected: ', numConnected
        if ( numConnected > 0 ) then
           DO I=1, nSobek
              if ( mask(I) ) then
                if (idebug > 0) write(idebug, '(A,I4,A,F16.8)') 'i=', i, 'value=', CFValues(I)
                AlRSBK(i,7,NTims) = CFValues(I)
              endif
           ENDDO
        endif
    endif
    deallocate(CFValues, mask)
! End OpenMI CF gate lower edge level

! Flow: gate opening height
! Expected unit in input: m
! converted to array
    numConnected = 0
    allocSuccess = DH_AllocInit(nSobek, CFValues, 0.0)
    allocSuccess = DH_AllocInit(nSobek, mask, .false.)

    if (idebug > 0) write(idebug, *) 'GetExternals, timestep ', RTC_Timestep
    if ( allocSuccess) then
        numConnected = OesGetConnected(RTC_RunId,RTCGateOpeningHeight,RTCStructures,CFValues, mask)
        if (idebug > 0) write(idebug, *) '    #Structures: ', nSobek, &
                         '    #connected: ', numConnected
        if ( numConnected > 0 ) then
           DO I=1, nSobek
              if ( mask(I) ) then
                if (idebug > 0) write(idebug, '(A,I4,A,F16.8)') 'i=', i, 'value=', CFValues(I)
                AlRSBK(i,8,NTims) = CFValues(I)
              endif
           ENDDO
        endif
    endif
    deallocate(CFValues, mask)
! End OpenMI CF gate opening height

! Flow: flow area
! Expected unit in input: m2
! converted to array
    numConnected = 0
    allocSuccess = DH_AllocInit(nSobek, CFValues, 0.0)
    allocSuccess = DH_AllocInit(nSobek, mask, .false.)

    if (idebug > 0) write(idebug, *) 'GetExternals, timestep ', RTC_Timestep
    if ( allocSuccess) then
        numConnected = OesGetConnected(RTC_RunId,RTCFlowArea,RTCStructures,CFValues, mask)
        if (idebug > 0) write(idebug, *) '    #Structures: ', nSobek, &
                         '    #connected: ', numConnected
        if ( numConnected > 0 ) then
           DO I=1, nSobek
              if ( mask(I) ) then
                if (idebug > 0) write(idebug, '(A,I4,A,F16.8)') 'i=', i, 'value=', CFValues(I)
                AlRSBK(i,9,NTims) = CFValues(I)
              endif
           ENDDO
        endif
    endif
    deallocate(CFValues, mask)
! End OpenMI CF flow area

! Flow: discharge structure
! Expected unit in input: m3/s
! converted to array
    numConnected = 0
    allocSuccess = DH_AllocInit(nSobek, CFValues, 0.0)
    allocSuccess = DH_AllocInit(nSobek, mask, .false.)

    if (idebug > 0) write(idebug, *) 'GetExternals, timestep ', RTC_Timestep
    if ( allocSuccess) then
        numConnected = OesGetConnected(RTC_RunId,RTCDischarge,RTCStructures,CFValues, mask)
        if (idebug > 0) write(idebug, *) '    #Structures: ', nSobek, &
                         '    #connected: ', numConnected
        if ( numConnected > 0 ) then
           DO I=1, nSobek
              if ( mask(I) ) then
                if (idebug > 0) write(idebug, '(A,I4,A,F16.8)') 'i=', i, 'value=', CFValues(I)
                AlRSBK(i,10,NTims) = CFValues(I)
              endif
           ENDDO
        endif
    endif
    deallocate(CFValues, mask)
! End OpenMI CF discharge structure

! Flow: velocity structure
! Expected unit in input: m/s
! converted to array
    numConnected = 0
    allocSuccess = DH_AllocInit(nSobek, CFValues, 0.0)
    allocSuccess = DH_AllocInit(nSobek, mask, .false.)

    if (idebug > 0) write(idebug, *) 'GetExternals, timestep ', RTC_Timestep
    if ( allocSuccess) then
        numConnected = OesGetConnected(RTC_RunId,RTCVelocity,RTCStructures,CFValues, mask)
        if (idebug > 0) write(idebug, *) '    #Structures: ', nSobek, &
                         '    #connected: ', numConnected
        if ( numConnected > 0 ) then
           DO I=1, nSobek
              if ( mask(I) ) then
                if (idebug > 0) write(idebug, '(A,I4,A,F16.8)') 'i=', i, 'value=', CFValues(I)
                AlRSBK(i,11,NTims) = CFValues(I)
              endif
           ENDDO
        endif
    endif
    deallocate(CFValues, mask)
! End OpenMI CF velocity structure

! Flow: water level up
! Expected unit in input: m AD
! converted to array
    numConnected = 0
    allocSuccess = DH_AllocInit(nSobek, CFValues, 0.0)
    allocSuccess = DH_AllocInit(nSobek, mask, .false.)

    if (idebug > 0) write(idebug, *) 'GetExternals, timestep ', RTC_Timestep
    if ( allocSuccess) then
        numConnected = OesGetConnected(RTC_RunId,RTCWaterlevelup,RTCStructures,CFValues, mask)
        if (idebug > 0) write(idebug, *) '    #Structures: ', nSobek, &
                         '    #connected: ', numConnected
        if ( numConnected > 0 ) then
           DO I=1, nSobek
              if ( mask(I) ) then
                if (idebug > 0) write(idebug, '(A,I4,A,F16.8)') 'i=', i, 'value=', CFValues(I)
                AlRSBK(i,12,NTims) = CFValues(I)
              endif
           ENDDO
        endif
    endif
    deallocate(CFValues, mask)
! End OpenMI CF water level up

! Flow: water level down
! Expected unit in input: m AD
! converted to array
    numConnected = 0
    allocSuccess = DH_AllocInit(nSobek, CFValues, 0.0)
    allocSuccess = DH_AllocInit(nSobek, mask, .false.)

    if (idebug > 0) write(idebug, *) 'GetExternals, timestep ', RTC_Timestep
    if ( allocSuccess) then
        numConnected = OesGetConnected(RTC_RunId,RTCWaterleveldown,RTCStructures,CFValues, mask)
        if (idebug > 0) write(idebug, *) '    #Structures: ', nSobek, &
                         '    #connected: ', numConnected
        if ( numConnected > 0 ) then
           DO I=1, nSobek
              if ( mask(I) ) then
                if (idebug > 0) write(idebug, '(A,I4,A,F16.8)') 'i=', i, 'value=', CFValues(I)
                AlRSBK(i,13,NTims) = CFValues(I)
              endif
           ENDDO
        endif
    endif
    deallocate(CFValues, mask)
! End OpenMI CF water level down

! Flow: head
! Expected unit in input: m
! converted to array
    numConnected = 0
    allocSuccess = DH_AllocInit(nSobek, CFValues, 0.0)
    allocSuccess = DH_AllocInit(nSobek, mask, .false.)

    if (idebug > 0) write(idebug, *) 'GetExternals, timestep ', RTC_Timestep
    if ( allocSuccess) then
        numConnected = OesGetConnected(RTC_RunId,RTCHead,RTCStructures,CFValues, mask)
        if (idebug > 0) write(idebug, *) '    #Structures: ', nSobek, &
                         '    #connected: ', numConnected
        if ( numConnected > 0 ) then
           DO I=1, nSobek
              if ( mask(I) ) then
                if (idebug > 0) write(idebug, '(A,I4,A,F16.8)') 'i=', i, 'value=', CFValues(I)
                AlRSBK(i,14,NTims) = CFValues(I)
              endif
           ENDDO
        endif
    endif
    deallocate(CFValues, mask)
! End OpenMI CF head

! Flow: force difference per unit width
! Expected unit in input: N/m
! converted to array
    numConnected = 0
    allocSuccess = DH_AllocInit(nSobek, CFValues, 0.0)
    allocSuccess = DH_AllocInit(nSobek, mask, .false.)

    if (idebug > 0) write(idebug, *) 'GetExternals, timestep ', RTC_Timestep
    if ( allocSuccess) then
        numConnected = OesGetConnected(RTC_RunId,RTCForcedifference,RTCStructures,CFValues, mask)
        if (idebug > 0) write(idebug, *) '    #Structures: ', nSobek, &
                         '    #connected: ', numConnected
        if ( numConnected > 0 ) then
           DO I=1, nSobek
              if ( mask(I) ) then
                if (idebug > 0) write(idebug, '(A,I4,A,F16.8)') 'i=', i, 'value=', CFValues(I)
                AlRSBK(i,15,NTims) = CFValues(I)
              endif
           ENDDO
        endif
    endif
    deallocate(CFValues, mask)
! End OpenMI CF force difference per unit width

! Flow: pump capacity
! Expected unit in input: m3/s
! converted to array
    numConnected = 0
    allocSuccess = DH_AllocInit(nSobek, CFValues, 0.0)
    allocSuccess = DH_AllocInit(nSobek, mask, .false.)

    if (idebug > 0) write(idebug, *) 'GetExternals, timestep ', RTC_Timestep
    if ( allocSuccess) then
        numConnected = OesGetConnected(RTC_RunId,RTCDischarge,RTCPumps,CFValues, mask)
        if (idebug > 0) write(idebug, *) '    #Structures: ', nSobek, &
                         '    #connected: ', numConnected
        if ( numConnected > 0 ) then
           DO I=1, nSobek
              if ( mask(I) ) then
                if (idebug > 0) write(idebug, '(A,I4,A,F16.8)') 'i=', i, 'value=', CFValues(I)
                AlRSBK(i,15,NTims) = CFValues(I)
              endif
           ENDDO
        endif
    endif
    deallocate(CFValues, mask)
! End OpenMI CF pump capacity

! More external data may be added

! Flow: discharges
! Expected unit in input: m3/s
! converted to array
    numConnected = 0
    allocSuccess = DH_AllocInit(nSobek, CFValues, 0.0)
    allocSuccess = DH_AllocInit(nSobek, mask, .false.)

    if (idebug > 0) write(idebug, *) 'GetExternals, timestep ', RTC_Timestep
    if ( allocSuccess) then
        numConnected = OesGetConnected(RTC_RunId,RTCVelocity,RTCReachsegElmSet,CFValues, mask)
        if (idebug > 0) write(idebug, *) '    #ReachSegments: ', nSobek, &
                         '    #connected: ', numConnected
        if ( numConnected > 0 ) then
           DO I=1, nSobek
              if ( mask(I) ) then
                if (idebug > 0) write(idebug, '(A,I4,A,F16.8)') 'i=', i, 'value=', CFValues(I)
                ALRSBK(i,21,NTims) = CFValues(I)
              endif
           ENDDO
        endif
    endif
    deallocate(CFValues, mask)
! End OpenMI CF reach segment discharges

!
!
! End other external data

    ! Reset OpenMI 'always write debug'
    idebug = currIDebugSettings

    RTCGetExternals=0

  return
  end function RTCGetExternals


! ************************************************************************************
!    RTCPerformTimestep
! ************************************************************************************

  Integer function RTCPerformTimestep (RTC_RunId,RTC_Ievent,RTC_Timestep)

  Implicit none
  Integer  RTC_RunId, RTC_Ievent, RTC_Timestep
  Integer  Returncode

!       restore state?
!        If (TestSaveState .and. RTC_timestep .eq. RestoreInTimestep)  Call RTC_RestoreState(RR_Timestep)

!       initialisatie per tijdstap
!        write(*,*) ' Initialise timestep', RTC_Timestep
        ReturnCode = RTCInitializeTimestep(RTC_RunId,RTC_IEvent,RTC_Timestep)
        If (ReturnCode .ne. 0) goto 9999

!       get data from externals, using HarmonIT
        ReturnCode = RTCGetExternals(RTC_RunId,RTC_IEvent,RTC_Timestep)
        If (ReturnCode .ne. 0) goto 9999

!       compute the timestep
!        write(*,*) ' Compute timestep', RTC_Timestep
        ReturnCode = RTCComputeTimestep(RTC_RunId,RTC_IEvent,RTC_Timestep)
        If (ReturnCode .ne. 0) goto 9999

!       finalise the timestep, including putting output data to Delft-IO HIS files
!        write(*,*) ' Finalise timestep', RTC_Timestep
        ReturnCode = RTCFinalizeTimestep(RTC_RunId,RTC_IEvent,RTC_Timestep)
        If (ReturnCode .ne. 0) goto 9999

!       save state?
!       If (TestSaveState) Call RTC_SaveState(RTC_Timestep)

  9999  Continue
        RTCPerformTimestep = ReturnCode


  return
  end function RTCPerformTimestep



! ************************************************************************************
!    RTCComputeTimestep
! ************************************************************************************

  Integer function RTCComputeTimestep (RTC_RunId,RTC_Ievent,RTC_Timestep)


  Implicit none
  Integer  RTC_RunId, RTC_Ievent, RTC_Timestep

  Character*999 StringCmd, LineString
  Logical(4) Result, SystemQq, FnmExt

  Integer  CmpDecV, ChSbMeas, Ch3BMeas, CalBar
  Integer  idum, Id3B, i, ipara, iret

  Integer  returnIndex

         IEvent = RTC_IEvent
         Itmstp = RTC_Timestep
         Idum   = RTC_Timestep

! gp Aug 2004 RR 'nr of active measures' should be initialised to 0
!             within iteration loop, so in perform timestep
! *********************************************************************
! *** Reset D3B status at unique node id's (MAALSTOP)
! *********************************************************************

         DO ID3B=1,N3BMS
            MS3BST(ID3B,1) = 0
            MS3BST(ID3B,2) = 0
         ENDDO
! *********************************************************************
! ** Compute Decision variables; add to HIS result file
! *********************************************************************

         if (UseTCN .and. WriteCsvFile .ne. '') IOutCsv =  DioNewLun()

!        Write(*,*) ' Start Compute Decision vars'
         RTCComputeTimestep = CMPDECV (IDEBUG, IOUT1, IDUM, IflRtnRtc)
         if (RTCComputeTimestep .ne. 0) Return
!        Putting data to output (every x timesteps) moved to finalizeTimestep

         if (UseTCN) then
! Check external exe
! put csv file (to TCN) is done at the end of CmpDecV
! call external exe
           StringCmd(1:) = ' '
           StringCmd(1:) = RunCommand(1:len_trim(RunCommand))
           Write(StringCmd(len_trim(RunCommand)+2:),*)  ievent, itmstp
           if (itmstp .eq. 1) Write(StringCmd(len_trim(StringCmd)+2:),*) 'firstrun'
           if (RunCommand .ne. '') Write(*,*) ' run command:',StringCmd(1:len_trim(StringCmd))
			  call execute_command_line (StringCmd, wait=.true., cmdstat=iret)
           if (iret .ne. 0) call write_error_message_rtc (RtcComputeTimestep, 0,' Error executing RunCommand TCN',' ComputeTimestep',IOUT1)

!get csv file with results (from TCN / external exe)
           IF (ReadCsvFile .ne. '') then
               IInCsv =  DioNewLun()
               INQUIRE (FILE = ReadCsvFile(1:len_trim(ReadCsvFile)), EXIST = FNMEXT)
               IF (.NOT. FNMEXT) THEN
                   write(*,*) ' ReadCsvFile could not be found for reading TCN results', ReadCsvFile
                   write(iout1,*) ' ReadCsvFile could not be found', ReadCsvFile
               !   call write_error_message_rtc (RtcComputeTimestep, 0,' Error finding CSV file from TCN',' ComputeTimestep',IOUT1)
               endif
               CsvReadId = ''
               CsvReadValue = -999.
               Open (IInCsv,File=ReadCsvFile(1:len_trim(ReadCsvFile)))
               ! skip header
               Read(IinCsv,'(A1)',Err=9981,End=999) LineString
               i = 0
 998           Continue
                 i = i+1
!                Read (IInCsv,*,Err=9981,End=999) CsvReadId(i), CsvReadValue(i)
                 Read (IInCsv,'(A999)',Err=9981,End=999) LineString
                 Call Rplstr(LineString, DblQuote, Quote)
                 Call Rplstr(LineString, Comma,' ')
 !               Write(*,*) LineString
                 Read (LineString,*,Err=9981) CsvReadId(i), CsvReadValue(i)
 !               Write(*,*) i, CsvReadid(i), CsvReadValue(i)
                 goto 998
 9981            Continue
                 call write_error_message_rtc (RtcComputeTimestep, 0,' Error reading CSV file from TCN',' ComputeTimestep',IOUT1)
  999            Continue
               Close(IInCsv)
               DO IPARA=12,NPARA
                  IF (ParaId(ipara)(1:4) .eq. 'TCN_') THEN
                     Call FindIndexInCsv (returnindex, Iout1, NsMeas, ParaId(ipara)(5:))
                     if (returnindex .le. 0) then
                         write(iout1,*) ' Timestep ', itmstp
                         if (Ntims .ge. 2) then
                            ! set at previous value
                            write(*,*) ' value for parameter ', ParaId(ipara)(5:len_trim(ParaId(ipara)(5:))),' not found in ReadCsvFile and set to previous value'
                            write(iout1,*) ' value for parameter ', ParaId(ipara)(5:len_trim(ParaId(ipara)(5:))), ' not found in ReadCsvFile and set to previous value'
                            DcvVal(ipara,1) = DcvVal(ipara,2)
                         else
                            ! set at -999
                            write(*,*) ' value for parameter ', ParaId(ipara)(5:len_trim(ParaId(ipara)(5:))),' not found in ReadCsvFile and set to -999'
                            write(iout1,*) ' value for parameter ', ParaId(ipara)(5:len_trim(ParaId(ipara)(5:))), ' not found in ReadCsvFile and set to -999'
                            DcvVal(ipara,1) = -999.
                         endif
                     else
                        DcvVal(ipara,1) = CsvReadValue(returnindex)
                     endif
                  ENDif
               ENDDO
               Close (IOutCsv)
           ENDIF

         Endif    ! (useTCN)
!
! *********************************************************************
! ** Check measures for SOBEK-CF/SF and Delft3D-FLOW
! *********************************************************************

         If (NSMEAS .GT. 0) Then
!           Set Sobek measures
!           Write(*,*) ' Check Sobek measures'
            RTCComputeTimestep = CHSBMEAS (IDEBUG, IOUT1)
            if (RTCComputeTimestep .ne. 0) Return
!           Write(*,*) ' After Sobek measures'
         Endif
!        Write(*,*) ' After checking CF measures and putting to HIS'

! *********************************************************************
! ** Check measures for 3B (MLST and 3BML records in 3BMEAS inputfile)
! *********************************************************************

         If (SOBEKMODE) then
            If (N3BMS .GT. 0) Then
!               Set RR measures
!               Write(*,*) ' Check RR measures'
                RTCComputeTimestep = CH3BMEAS (IDEBUG, IOUT1)
                if (RTCComputeTimestep .ne. 0) Return
!               Write(*,*) ' After RR measures'
            Endif
!           Write(*,*) ' After checking RR measures and putting to HIS'

          Endif

!         Write(*,*) ' End of Sobek mode '
! *********************************************************************
! *** Calculate and send data to D3DFlow
!     The data must be stored in array VALBAR
! *********************************************************************
         If (numBarriers > 0) then
!
!---------- Calculate the new barrier heights
!
            RtcComputeTimestep = CALBAR(RTCModJulianTime, iout1)
            if (RtcComputeTimestep .ne. 0) Return
!
!---------- Send them to flow, data stored in VALBAR
!
            call SyncRtcFlow_Send(1)
          Endif

!
        RTCComputeTimestep = 0
        If (RTCCrashed) RTCComputeTimestep = -1

  return
  end function RTCComputeTimestep



! ************************************************************************************
!    RTCFinalizeTimestep
! ************************************************************************************

  Integer function RTCFinalizeTimestep (RTC_RunId,RTC_Ievent,RTC_Timestep)

  Implicit none
  Integer  RTC_RunId, RTC_Ievent, RTC_Timestep

  Integer  WriteDioPltSbk, WriteDioPlt3B
  integer  idum, imeas, id3b, iloc, ipar

  Double Precision Julian

         IEvent = RTC_IEvent
         Itmstp = RTC_Timestep
         Idum   = RTC_Timestep
         RTCFinalizeTimestep = 0

!        Write(*,*) ' Put Decision vars to output'
! Add results to HIS output for First Event always, for all events if switch WriteRtcHisfiles=-1 in INI file
! ARS 7688: Add results to HIS output every x timesteps (x=OutputTimestep)
!        If (WRTHIS .OR. IEVENT .EQ. 1) Then
         If (WRTHIS .OR. IEVENT .LE. NEVENT) Then
           ! always output for all events
           If (Mod(Idum, OutputTimestep) .eq. 0) then
              ! decision parameter output
              if (NPara>0) then
                 Allocate  ( DioResult(1, NPara), Stat=Allocation_Error )
                 If (Allocation_Error .ne. 0) then
                    call write_error_message_rtc (981, Allocation_Error, ' RTC', ' Error allocating arrays ', Iout1)
                    RTCFinalizeTimestep = 981
                    Return
                 Endif
                 Do ipar=1,NPara
                    DioResult(1,ipar) = DcvVal(ipar,1)
                 Enddo
                 Call DioPltPut (ParameterDataSet, NrTimestepsSinceStartFirstEvent, DioResult)
                 Deallocate (DioResult)
              Endif
              ! precipitation output
              If (USEP) Then
                 HEADER = ' '
                 Allocate  ( DioResult(NTimHp,NPrecP), Stat=Allocation_Error )
                 If (Allocation_Error .ne. 0) then
                     call write_error_message_rtc (981, Allocation_Error, ' RTC', ' Error allocating arrays ', Iout1)
                     RTCFinalizeTimestep = 981
                     Return
                 Endif
                 Do ipar=1,NTimHp
                    Do iloc=1,NPrecP
                       DioResult(ipar,iloc) = ResPre(iloc,ipar)
                    Enddo
                 Enddo
                 Call DioPltPut (PrecipitationDataSet,NrTimestepsSinceStartFirstEvent, DioResult )
                 Deallocate (DioResult)
              Endif
              ! ** Wind output
              If (USEW) Then
                 HEADER = ' '
                 Allocate  ( DioResult(NTimHw*2,NExt+NextHd),Stat=Allocation_Error )
                 If (Allocation_Error .ne. 0) Then
                     call write_error_message_rtc (981, Allocation_Error, ' RTC', ' Error allocating arrays ', Iout1)
                     RTCFinalizeTimestep = 981
                     Return
                 Endif
                 Do ipar=1,NTimHw*2
                   Do iloc=1,NExt+NExtHD
                      DioResult(ipar,iloc) = ResExt(iloc,ipar)
                   Enddo
                 Enddo
                 Call DioPltPut (WindDataSet,NrTimestepsSinceStartFirstEvent,  DioResult )
                 Deallocate (DioResult)
              Endif
           Endif
         Endif
!        Write(*,*) ' Put Sobek vars to output'
         If (NSMEAS_SBK .GT. 0) Then
             RTCFinalizeTimestep = WriteDioPltSbk (outPlt_sbk, NrTimestepsSinceStartFirstEvent, Namfil(16), Iout1, Idebug)
             if (RTCFinalizeTimestep .ne. 0) Return
!            overall output; tijdstapnr=NrTimestepsSinceStartFirstEvent (opnieuw bij reeks)
!            If (WRTHIS .OR. IEVENT .EQ. 1) Then
             If (WRTHIS .OR. IEVENT .LE. NEVENT) Then
                ! always output for all events
                If (Mod(Idum, OutputTimestep) .eq. 0) then
                  ! use temporary DioResult array instead of Reshape
                  Allocate  ( DioResult(1,NsMsId_SBK), Stat=Allocation_Error )
                  If (Allocation_Error .ne. 0) Then
                      call write_error_message_rtc (981, Allocation_Error, ' RTC', ' Error allocating arrays ', Iout1)
                      RTCFinalizeTimestep = 981
                      Return
                  Endif
                  Do iloc=1,NsMsId_SBK
                     DioResult(1,iloc) = MsSbSt(iloc)
                  Enddo
                  Call DioPltPut (OutSbkDataSet,NrTimestepsSinceStartFirstEvent,  DioResult )
                  Deallocate (DioResult)
                Endif
             Endif
         Endif
         If (N3BMS .GT. 0) Then
!            Set RR measures
!            Output in on-line communication with RR
             RTCFinalizeTimestep = WriteDioPlt3B (outPlt_3b, NrTimestepsSinceStartFirstEvent, Namfil(15), Iout1, Idebug)
             if (RTCFinalizeTimestep .ne. 0) Return
!            overall output; tijdstapnr=NrTimestepsSinceStartFirstEvent (opnieuw bij reeks)
!            If (WRTHIS .OR. IEVENT .EQ. 1) Then
             If (WRTHIS .OR. IEVENT .LE. NEVENT) Then
               ! always output for all events
               If (Mod(Idum, OutputTimestep) .eq. 0) then
                   if (idebug > 0) Write(iDEBUG,*) ' mS3bST before DioPltPut all timesteps', Ms3BSt
                   ! use temporary DioResult array instead of Reshape
                   Allocate  ( DioResult(7,N3BMs), Stat=Allocation_Error )
                   If (Allocation_Error .ne. 0) then
                       call write_error_message_rtc (981, Allocation_Error, ' RTC', ' Error allocating arrays ', Iout1)
                       RTCFinalizeTimestep = 981
                       Return
                   Endif
                   Do ipar=1,7
                      Do iloc=1,N3BMS
                         DioResult(ipar,iloc) = Ms3bSt(iloc,ipar)
                      Enddo
                   Enddo
                   Call DioPltPut (OutRRDataSet,NrTimestepsSinceStartFirstEvent,  DioResult )
                   Deallocate (DioResult)
!                  Met Reshape
!                   Call DioPltPut (OutRRDataSet, NrTimestepsSinceStartFirstEvent, Reshape (Transpose(Ms3BSt), (/7,N3BMS/)) )
                   if (idebug > 0) Write(iDEBUG,*) ' mS3bST after DioPltPut all timesteps', Ms3BSt
!                  Call WRHIS (IOMSTP, IDEBUG, NrSecondsSinceStartFirstEvent, 2, IOUT1, .FALSE.,  &
!                              HEADER, D3BPara,             &
!                              ID3BML, MS3BST , N3LOC,  7, N3BMS )
               Endif
             Endif
         ELSE
!  ook output indien geen 3B-maatregel, want anders crasht 3B !!
             Ms3BSt = -999.
             RtcFinalizeTimestep = WriteDioPlt3B (outPlt_3b, itmstp, Namfil(15), Iout1, Idebug)
             if (RTCFinalizeTimestep .ne. 0) Return
         Endif
!        Write(*,*) ' After checking RR measures and putting to HIS'

! *********************************************************************
! *** Update/store measure status   (MAALSTOP)
! *********************************************************************

         DO IMEAS=1,N3MEAS
            MSSTA0(IMEAS) = MSSTAT(IMEAS)
         ENDDO

! *********************************************************************
! *** Reset D3B status at unique node id's (MAALSTOP)
! *********************************************************************

         DO ID3B=1,N3BMS
            MS3BST(ID3B,1) = 0
            MS3BST(ID3B,2) = 0
         ENDDO

! *********************************************************************
! *** Set Time for Next timestep
! *********************************************************************

         RSecondsSinceStartFirstEvent = RSecondsSinceStartFirstEvent + RTMSIZ
         NrSecondsSinceStartFirstEvent = RSecondsSinceStartFirstEvent
         NrTimestepsSinceStartFirstEvent = NINT(RSecondsSinceStartFirstEvent/RtmSiz)

!        write(*,*) ' In Finalise timestep before NXTstp'
         Call NXTSTP (IDEBUG,IfYEAR,IfMO,IfDAY,IfHOUR,IfMIN,IfSEC, RfSEC, IDHR, IDMIN, RDSEC)
         Call NXTSTP (IDEBUG, IYEAR,IMO,IDAY, IHOUR, IMIN, ISEC, RSEC, IDHR, IDMIN, RDSEC)
         RTC_TIMNEW = dble(IYEAR) * 10000.0d0 + dble(IMO) * 100.0d0 + dble(IDAY) + &
                      dble(IHOUR) / 100.0d0 + dble(IMIN) / 10000.0d0 + dble(ISEC) / 1000000.0d0
         ITMSTP = ITMSTP + NSTEP


  9999  Continue

        RTCModJulianTime = RTCModJulianTime + RTCJulianTimestep
        CALL DATE_AND_TIME (RTCCDATE,RTCCTIME,RTCCZONE,RTCtime_fields)
        RTCIDateAct   = RTCtime_fields(1)*10000 + RTCtime_fields(2) * 100 + RTCtime_fields(3)
        RTCITimeAct   = RTCtime_fields(5)*10000 + RTCtime_fields(6) * 100 + RTCtime_fields(7)
        RTCJulianNow = Julian (RTCIDateAct, RTCITimeAct)

        RTCFinalizeTimestep = 0
        If (RTCCrashed) RTCFinalizeTimestep = -1

  return
  end function RTCFinalizeTimestep



! ************************************************************************************
!    RTCFinaliseEvent
! ************************************************************************************

  Integer function RTCFinalizeEvent (RTC_RunId,RTC_Ievent)

  Implicit none
  Integer  RTC_RunId, RTC_Ievent


        IEvent = RTC_IEvent

! *********************************************************************
! Close DIO Streams
! *********************************************************************
! extra wait at end of event
        RTC_TIMOLD = RTC_TIMNEW
        if (Ievent .lt. Nevent) Call STEPCT (RTC_TimOld, RTC_TimNew,IDCNT,ISTAT,RTCInitMode, RTCcrashed)
!
        call CloseDioPlt(inPlt_3B)
        call CloseDioPlt(inPlt_Sbk)
        call CloseDioPlt(inPlt_Wq)
        call CloseDioPlt(inPlt_D3D)
        call CloseDioPlt(outPlt_3b)
        call CloseDioPlt(outPlt_Sbk)

! Aug 2008: only if case of last event close the datasets
        if (RTC_Ievent .eq. NEvent) then
           call CloseDioPlt(ParameterDataSet)
           call CloseDioPlt(PrecipitationDataSet)
           call CloseDioPlt(WindDataSet)
           call CloseDioPlt(OutRRDataSet)
           call CloseDioPlt(OutSbkDataSet)
        endif

        If (NEVENT .GT. 1) Then
!            write(*,*) ' call 3'
             Call RTC_WRLOGO (IScren, IEVENT,NEVENT,NEVENT,IBar0,SOBEKMODE)
        Endif


    RTCFinalizeEvent = 0
    If (RTCCrashed) RTCFinalizeEvent = -1

  return
  end function RTCFinalizeEvent


! ************************************************************************************
!    RTCFinalize
! ************************************************************************************

  Integer function RTCFinalize (RTC_RunId)

  Implicit none
  Integer  RTC_RunId

!     afsluitende balk schrijven
      If (NEVENT .EQ. 1) Then
          Call RTC_WRLOGO (IScren, LASTTM,LASTTM,NEVENT,IBar0,SOBEKMODE)
      Endif

! *********************************************************************
! *** Beeindig stuurmodule controller
! *********************************************************************

      RTC_TIMNEW = -1
      IF (SOBEKMODE) THEN
        Call STEPCT (RTC_TIMOLD, RTC_TIMNEW, IDCNT, ISTAT, RTCInitMode, RTCcrashed)
        If (RTCcrashed) Goto 9999
        If (ISTAT .LT. 0)  Then
           WRITE (*,'(A,I3)') ' Error 3 Stepct in RTC', ISTAT
           WRITE (IFlRtnRtc,'(I5)') ISTAT
        Endif
        Call ENDCT  (IDCNT, ISTAT)
        If (ISTAT .LT. 0)  Then
           WRITE (*,'(A,I3)') ' Error 4 Stepct in RTC', ISTAT
           WRITE (IFlRtnRtc,'(I5)') ISTAT
        Endif
      ENDIF

 9999 Continue

      If (RtcCrashed) Call CrashCt(IdCnt, .false.)
      RTCFinalize = RTCWriteReturnCodeFile (RTC_RunId)

      If (RTCCrashed) RTCFinalize = -1

  return
  end function RTCFinalize


! ************************************************************************************
!    RTCWriteReturnCodeFile
! ************************************************************************************

  Integer function RTCWriteReturnCodeFile (RTC_RunId)

  Implicit none
  Integer  RTC_RunId, istat
  Character(Len=80) String

  integer, external :: rtc_close_shared_library
  integer           :: error

      IF (SOBEKMODE) THEN
        If (RTCcrashed)  Call CrashCt (IdCnt, .false. )
      ENDIF

! *********************************************************************
! *** Delete RTCSimulateFile and Trigger file
!     and close communication with D3DFlow if required
! *********************************************************************
      If (CoupledToDelft3D) then
        if (.not.SOBEKMODE) then
          In = DioNewLun()
          open (IN,file=NAMFIL(29),status='OLD',iostat=istat)
          if (istat==0) close(IN,status='delete')
        endif
        !
        In = DioNewLun()
        open (IN,file=filsim,status='OLD',iostat=istat)
        if (istat==0) close(IN,status='delete')
        !
        If (D3DACTIVE) then
           call SyncRtcFlow_Close
        endif
      endif

!     close MATLAB if opened
#if (defined(USE_MATLAB))
      Call CloseMatlab(IDEBUG)
#endif
!     close DLL if opened
      if (dll_handle .ne. 0) then
         error = rtc_close_shared_library(dll_handle)
      endif

!  Return code file

      WRITE (IFlRtnRtc,'(I5)') 0
! Afsluitmelding in logfile schrijven
      if (iOut1 .gt. 0) then
         String = ' Successfull logout of RTC'
         WRITE (IOUT1,'(A)') String
         CLOSE (IOUT1)
      Endif

      RTCWriteReturnCodeFile = 0

  return
  end function RTCWriteReturnCodeFile


! ************************************************************************************
!    RTCGetCurrentTime, RTCGetInputTime
! ************************************************************************************

! RTCModJulianTime, RTCJulianTimestep to be added

  function RTCGetCurrentTime(RTC_RunId) result(currentTime)
      integer RTC_RunId
      double precision:: currentTime
      currentTime = RTCModJulianTime
  end function RTCGetCurrentTime

  function RTCGetInputTime(RTC_RunId) result(inputTime)
      integer RTC_RunId
      double precision:: inputTime
      inputTime = RTCModJulianTime + RTCJulianTimestep
  end function RTCGetInputTime

  function RTCGetTimeHorizon(RTC_RunId, startTime, endTime) result(retVal)
      integer retVal
      integer RTC_RunId
      integer Frsttm
      double precision :: startTime, endTime
      double Precision, external :: modified_julian_fromJulian
      startTime = Modified_Julian_fromJulian(RTCJulianStartDate)
      Frsttm = 1
      endTime = startTime + (Lasttm - Frsttm + 1) * RTCJulianTimestep
      retVal = 0
  end function RTCGetTimeHorizon

  function RTCGetDeltaT(RTC_RunId) result(deltaTAsMJD)
      double precision:: deltaTAsMJD
      integer RTC_RunId
      deltaTAsMJD = RTCJulianTimestep
  end function RTCGetDeltaT

!!---------------------
!! Added for OpenMI
!!---------------------

   Function InitOutCFSetpoints(RTC_RunId) result(RetVal)

      use wl_open_mi_support
      use rtc_open_mi_support

      IMPLICIT NONE

      integer :: RetVal
      integer :: RTC_RunId
      Logical :: Success
      Integer :: teller, OesCFElmset, OesCFQuant

      Character(Len=CharIdLength), dimension(:), pointer  :: openMINode

      RetVal = 0
      If (OesInitialized() ) then
          success = DH_AllocInit (NsMsId_SBK, openMINode, ' ')
          If (.not. Success) then
              call write_error_message_rtc (981, 0, ' Error alloc.openMI-arrs in subroutine ', &
                                   ' InitOutCFSetpoints', Iout1)
              RetVal = 981
              Return
          Endif
          Do teller = 1, NsMsId_SBK
             openMINode(teller) = MsSbId(teller)
          Enddo
          oesCFElmset = OesElmSetFindOrCreate(RTC_RunId,RTCCFsetpoints,openMINode)
          If (oesCFElmset > 0) then
          OesCFQuant=OesExchItemCreate(RTC_RunId,RTCCFSetpoint,RTCCFSetpoints,oes_providing)
          Endif
          DeAllocate(openMINode)
      Endif

   Return
   End Function InitOutCFSetpoints


   Function InitOutRRSetpoints(RTC_RunId) result(RetVal)

      use wl_open_mi_support
      use rtc_open_mi_support

      IMPLICIT NONE

      integer :: RetVal
      integer :: RTC_RunId
      Logical :: Success
      Integer :: teller, OesRRElmset, OesRRQuant

      Character(Len=CharIdLength), dimension(:), pointer  :: openMINode

      RetVal = 0
      If (OesInitialized() ) then
         success = DH_AllocInit (N3BMs, openMINode, ' ')
         If (.not. Success) then
              call write_error_message_rtc (981, 0, ' Error alloc.openMI-arrs in subroutine ', &
                                              ' InitOutRRSetpoints', Iout1)
              RetVal = 981
              Return
         Endif
         Do teller = 1, N3BMs
            openMINode(teller) = Id3BML(teller)
         Enddo
         oesRRElmset = OesElmSetFindOrCreate(RTC_RunId,RTCRRStructures,openMINode)
         if (oesRRElmset > 0) then
            OesRRQuant=OesExchItemCreate(RTC_RunId,RTCPumpstop,RTCRRStructures,oes_providing)
            OesRRQuant=OesExchItemCreate(RTC_RunId,RTCRRHighOn,RTCRRStructures,oes_providing)
            OesRRQuant=OesExchItemCreate(RTC_RunId,RTCRRHighOff,RTCRRStructures,oes_providing)
            OesRRQuant=OesExchItemCreate(RTC_RunId,RTCRRLowOn,RTCRRStructures,oes_providing)
            OesRRQuant=OesExchItemCreate(RTC_RunId,RTCRRLowOff,RTCRRStructures,oes_providing)
         endif
         DeAllocate(openMINode)
      Endif

   Return
   End function InitOutRRSetpoints

   function OMI_RTC_GetData(QuantityID, ElementsetID, ElementCount, Values) result(retVal)

        use wl_open_mi_support
        use RTC_open_mi_support

        IMPLICIT NONE

        ! return value
        logical  :: retVal

        !arguments
        character(Len=*), intent(in)     :: QuantityID   ! quant. identifier
        character(Len=*), intent(in)     :: ElementsetID ! elem.set. identifier
        integer         , intent(in)     :: ElementCount ! #elems in elementset
        double precision, intent(out), &
               dimension(1:ElementCount) :: Values       ! values in elemenset

        ! locals

        Values  = 0
        retVal = .true.

        ! CF Setpoints
        if (ElementsetID == RTCCFSetpoints) then
            ! Subdivide for various variables
            if (QuantityID == RTCCFSetpoint) then
                Values = MsSbSt(1:NsMsId)
            ! Something is wrong
            else
                retVal = .false.
            endif

        ! RR Setpoints
        elseif (ElementsetID == RTCRRStructures) then
            ! Subdivide for various variables
            if (QuantityID == RTCPumpstop) then
                Values = Ms3BSt(1:N3BMs,1)
            ! Something is wrong
            else
                retVal = .false.
            endif

        ! Something is wrong
        else
            retVal = .false.
        endif

    end function

    subroutine FindIndexInCsv (returnindex, Iout1, NsMeas, SearchString)
    character(len=*) SearchString
    integer returnindex, iout1, i, Nsmeas

     returnindex = 0

     Do i=1,NsMeas*2
        if (SearchString(1:len_trim(SearchString)) .eq. CsvReadId(i)(1:len_trim(CsvReadId(i))) ) then
            returnindex = i
            goto 999
        endif
     enddo
     write(IOut1,*) SearchString(1:len_trim(SearchString))
     write(IOut1,*) ' not found in info read from csv file'

 999 continue

    return
    end subroutine

END module RTCModule

