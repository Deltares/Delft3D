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

      Subroutine RTC_RDINI (IDEBUG, IdebugLun, IN, IOUT1)
! *********************************************************************
! ***                D E L F T         H Y D R A U L I C S
! ***
! ***              WATER RESOURCES AND ENVIRONMENT DIVISION
! *********************************************************************
! *** Program :  RTC version 1.0.                     Date: June 1997
! *********************************************************************
! *** Last update: June   1997       By : Geert Prinsen
! *********************************************************************
! *** Brief description:
! *** ------------------
! ***   Inlezen INI file
! *********************************************************************
! *** Input/output parameters:
! *** ------------------------
! ***  IDEBUG = file unit number of debug file
! ***  IN     = file unit number of input file
! ***  IOUT1  = file unit number of output file with messages
! *********************************************************************

      Use ParameterModule
      Use ReservoirModule
      Use OtherData
      Use MeasureModule
      Use ReadLib_rtc
      Use ExternalDLLModule

! voor gebruik current dir functie (PC only)
#if (defined(HAVE_CONFIG_H))
#else
      Use DfPort  ! for GetCwd function
      Use DfLib   ! for FullPathQQ function
#endif

!     voor gebruik DLL
      integer, external  :: rtc_open_shared_library
      Character(256)  :: dllname

      Logical      ENDFIL
      Integer      POS1, LEN, IECODE, IDum, IResult
      Character*12 CDUM
      Character*999 STRING
      Character*999 STRING_org
      Double Precision TimChk
      Character(len=256) CurDirName, FullPathMatDir
!
      Integer IDEBUG, IdebugLun, IN, IOUT1
      If (IDEBUG .GT. 0) Write (IDEBUG,1)
    1 Format (' RDINI')
!
! *********************************************************************
! *** skip header of file
! *********************************************************************
!
      Call SKPCOM (IN, ENDFIL,'RTC')
      If (ENDFIL)  call write_error_message_rtc (911, 0, 'Rdini', ' RTC-INIfile', IOUT1)
!
! *********************************************************************
! *** Initialisatie
! *********************************************************************

      VERSION='0.00'
      DATEVersion  = 'Prototype'
      USE3B  = .False.
      USESBK = .False.
      USEPRE = .False.
      USEEXT = .False.
      USEWQ  = .False.
      USE3D  = .False.
      USETCN = .False.
      TIMF3B = .False.
      WRTHIS = .True.

      SOBEKMODE = .False.
      D3DACTIVE = .False.
      UseInitValues = 0

      NEVENT = 1
      NSTEP  = 1
      DELTAT = 0.01
      LASTTM = 1

!     Default initialisatie Active language
      ActiveLanguage = 1

      IOPTS  = 2    ! Oct 2002: was default 1=Ascii
      IOPT3  = 2    ! Oct 2002: was default 1=Ascii
      IOPTP  = 2
      IOPTE  = 2
      IOPTWQ = 2    ! default HIS file

      NTIMHP = 1
      NTIMHW = 1
!     Initialisatie -NtimsH backwards compatible;
!     kan door nieuwe regel in INI file overschreven worden
      NtimsH = -720

      IMODEP = 0
      IMODEW = 0
      UseP   = .False.
      UseW   = .False.
      WithoutSobekCString = .True.

      MATDBG = 0
      MATDIR = ' '
      MATFIL = ' '
      MatWQ   = .false.
      MatRR   = .false.
      MatRain = .false.
      MatRainPredict  = .false.
      MatWindPredict  = .false.
      MatPredictMulti = .false.
      MatlabNrWqPar   = 0

      DebugFromTimestep=0
      DebugToTimestep=0
      DebugFromTimestep2=0
      DebugToTimestep2=0
      OutputTimestep=1
      IDEBUG = 0
! since ARS 15784-785 (August 2006) default SetSequenceDecisionParameters=True
      SetSequenceDecisionParameters = .true.
! since Nov2002 Taiwan: Default use Wind Table module, but overrulable through RTC.Dat
      WindUseTableModule = .true.
!     WindUseTableModule = .false.     ! Fryslan: uitgezet
      ReduceWindTable    = .true.
      OnMatlabErrorQuit  = .false.

!     OldFormatMeasureFiles = .true.   ! 209003 patch for Fryslan   v3.209.14b&o, 3.209.18, 3.209.21/22 Fryslan
      OldFormatMeasureFiles = .false.  ! development version        v3.209.14, 18, 23
      MatlabCommunicationOldStyle = .false.  ! default by group, if fails: try 1By1 by changing switch
      MaxCountGeneral=100
      MaxCountRRLocations=100
      MaxCountMeasLocations=30
      MaxCountStructures=10
      MaxCountNodes=50
      MaxCountReachSegments=50
      MaxCountBiLcLocations=30
      MaxCountRRSetpoints=100
      MaxCountCFSetpoints=100
      MaxCount1D2DLocations=10
      ToMatlab1D2DH  = 1
      ToMatlab1D2DWD = 1
      ToMatlab1D2DBL = 1
      ToMatlab1D2DU  = 1
      ToMatlab1D2DV  = 1
      ToMatlab1D2DC  = 1

      dllname=' '
      dll_function = ' '
      dll_handle   = 0
      dll_test     = 0

      WriteCsvFile = ''
      ReadCsvFile = ''
      RunCommand  = ''

! *********************************************************************
! *** Assume order of blocks is fixed: General, Options, Control, CouplingMatlab
! *** Within a block the order is free.
! *********************************************************************
! *** read data General block
! *********************************************************************

  101 Read(IN,'(A)',END=21,ERR=150,IOSTAT=IECODE)  STRING
      Call UPPERC (STRING)
      POS1 = INDEX(STRING, '[GENERAL]')
      If (POS1 .LE. 0) GOTO 101

  102 Read(IN,'(A)',END=21,ERR=150,IOSTAT=IECODE)  STRING
      Call UPPERC (STRING)
      Call CHRTRIM (STRING,' ')
      POS1 = INDEX(STRING, '=')
      If (POS1 .GT. 0) Then
        If (STRING(1:POS1-1) .EQ. 'VERSION') Then
           VERSION=STRING(POS1+1:)
        ElseIf (STRING(1:POS1-1) .EQ. 'DATE') Then
           DATEVersion=STRING(POS1+1:)
        ElseIf (STRING(1:POS1-1) .EQ. 'DEBUG') Then
           CDUM=STRING(POS1+1:)
           Call CHRTRIM (CDUM,' ')
           LEN = 2
           If (CDUM(1:LEN) .EQ. '-1') IDEBUG = IdebugLun
        Endif
      Endif
      POS1 = INDEX(STRING, '[OPTIONS]')
      If (POS1 .LE. 0) GOTO 102

! *********************************************************************
! *** read data Options block
! *********************************************************************
!
  103 Read(IN,'(A)',END=21,ERR=150,IOSTAT=IECODE)  STRING
      Call UPPERC (STRING)
      Call CHRTRIM (STRING,' ')
      POS1 = INDEX(STRING, '=')
!c    Write (IDEBUG,*) STRING
      If (POS1 .GT. 0) Then
        If (STRING(1:POS1-1) .EQ. '3B') Then
           CDUM=STRING(POS1+1:)
           Call CHRTRIM (CDUM,' ')
           LEN = 2
           If (CDUM(1:LEN) .EQ. 'ON' .OR. CDUM(1:LEN) .EQ. 'On' .OR. &
               CDUM(1:LEN) .EQ. '-1') Then
             USE3B = .True.
             SOBEKMODE = .True.
           Endif
        ElseIf (STRING(1:POS1-1) .EQ. 'SOBEK')  Then
           CDUM=STRING(POS1+1:)
           Call CHRTRIM (CDUM,' ')
           LEN = 2
           If (CDUM(1:LEN) .EQ. 'ON' .OR.  CDUM(1:LEN) .EQ. 'On' .OR. &
               CDUM(1:LEN) .EQ. '-1') Then
             USESBK = .True.
             SOBEKMODE = .True.
           Endif
        ElseIf (STRING(1:POS1-1) .EQ. 'WQ')  Then
           CDUM=STRING(POS1+1:)
           Call CHRTRIM (CDUM,' ')
           LEN = 2
           If (CDUM(1:LEN) .EQ. 'ON' .OR.  CDUM(1:LEN) .EQ. 'On' .OR. &
                  CDUM(1:LEN) .EQ. '-1') USEWQ=.True.
        ElseIf (STRING(1:POS1-1) .EQ. '3DFLOW')  Then
           CDUM=STRING(POS1+1:)
           Call CHRTRIM (CDUM,' ')
           LEN = 2
           If (CDUM(1:LEN) .EQ. 'ON' .OR.  CDUM(1:LEN) .EQ. 'On' .OR. &
               CDUM(1:LEN) .EQ. '-1') Then
             USE3D = .True.
           Endif
        ElseIf (STRING(1:POS1-1) .EQ. 'USETCN')  Then
           CDUM=STRING(POS1+1:)
           Call CHRTRIM (CDUM,' ')
           USETCN=.true.
           LEN = 2
           If (CDUM(1:LEN) .EQ. 'OF' .OR. CDUM(1:LEN) .EQ. 'Of' .OR.  &
                  CDUM(1:LEN) .EQ. '0') USETCN=.False.
        ElseIf (STRING(1:POS1-1) .EQ. 'PRECIPITATION')  Then
           CDUM=STRING(POS1+1:)
           Call CHRTRIM (CDUM,' ')
           LEN = 2
           If (CDUM(1:LEN) .EQ. 'ON' .OR. CDUM(1:LEN) .EQ. 'On' .OR.  &
                  CDUM(1:LEN) .EQ. '-1') USEPRE=.True.
        ElseIf (STRING(1:POS1-1) .EQ. 'EXTERN')  Then
           CDUM=STRING(POS1+1:)
           Call CHRTRIM (CDUM,' ')
           LEN = 2
           If (CDUM(1:LEN) .EQ. 'ON' .OR. CDUM(1:LEN) .EQ. 'On' .OR. CDUM(1:LEN) .EQ. '-1') USEEXT=.True.
        ElseIf (STRING(1:POS1-1) .EQ. 'CONTROL') Then
           CDUM=STRING(POS1+1:)
           Call CHRTRIM (CDUM,' ')
           LEN = 2
           If (CDUM(1:LEN) .EQ. '3B' .OR. CDUM(1:LEN) .EQ. '3b') TIMF3B = .True.
        ElseIf (STRING(1:POS1-1) .EQ. '3BFORMAT') Then
           CDUM=STRING(POS1+1:)
           Call CHRTRIM (CDUM,' ')
           LEN = 3
           If (CDUM(1:LEN) .EQ. 'HIS' .OR. CDUM(1:LEN) .EQ. 'his') IOPT3 = 2
!          If (CDUM(1:LEN) .EQ. 'ASC' .OR. CDUM(1:LEN) .EQ. 'asc') IOPT3 = 1
        ElseIf (STRING(1:POS1-1) .EQ. 'SBKFORMAT') Then
           CDUM=STRING(POS1+1:)
           Call CHRTRIM (CDUM,' ')
           LEN = 3
           If (CDUM(1:LEN) .EQ. 'HIS' .OR. CDUM(1:LEN) .EQ. 'his') IOPTS = 2
!          If (CDUM(1:LEN) .EQ. 'ASC' .OR. CDUM(1:LEN) .EQ. 'asc') IOPTS = 1
        ElseIf (STRING(1:POS1-1) .EQ. 'WQFORMAT') Then
           CDUM=STRING(POS1+1:)
           Call CHRTRIM (CDUM,' ')
           LEN = 3
           If (CDUM(1:LEN) .EQ. 'HIS' .OR. CDUM(1:LEN) .EQ. 'his') IOPTWQ = 2
!          If (CDUM(1:LEN) .EQ. 'ASC' .OR. CDUM(1:LEN) .EQ. 'asc') IOPTWQ = 1
        ElseIf (STRING(1:POS1-1) .EQ. 'MODEPRECIPITATION') Then
           CDUM=STRING(POS1+1:)
           Call CHRTRIM (CDUM,' ')
           LEN = 6
           If (CDUM(1:LEN) .EQ. 'Actual' .OR. CDUM(1:LEN) .EQ. 'ACTUAL') IMODEP = 0
           If (IMODEP .NE. 0) Then
              Write(IOUT1,*) ' Option Mode<>Actual not yet supported'
              call write_error_message_rtc (999, 0, 'Rdini', ' Option Mode<>Actual not yet supported', IOUT1)
!              STOP 999
           Endif
        ElseIf (STRING(1:POS1-1) .EQ. 'MODEWIND') Then
           CDUM=STRING(POS1+1:)
           Call CHRTRIM (CDUM,' ')
           LEN = 6
           If (CDUM(1:LEN) .EQ. 'Actual' .OR. CDUM(1:LEN) .EQ. 'ACTUAL') IMODEW = 0
           If (IMODEW .NE. 0) Then
              call write_error_message_rtc (999, 0, 'Rdini', ' Option Mode<>Actual not yet supported', IOUT1)
!              STOP 999
           Endif
        ElseIf (STRING(1:POS1-1) .EQ. 'USEWIND') Then
           CDUM=STRING(POS1+1:)
           Call CHRTRIM (CDUM,' ')
           LEN = 2
           If (CDUM(1:LEN) .EQ. 'ON' .OR. CDUM(1:LEN) .EQ. 'On' .OR. CDUM(1:LEN) .EQ. '-1') USEW = .True.
        ElseIf (STRING(1:POS1-1) .EQ. 'USEPRECIPITATION') Then
           CDUM=STRING(POS1+1:)
           Call CHRTRIM (CDUM,' ')
           LEN = 2
           If (CDUM(1:LEN) .EQ. 'ON' .OR. CDUM(1:LEN) .EQ. 'On' .OR. CDUM(1:LEN) .EQ. '-1') USEP = .True.
        ElseIf (STRING(1:POS1-1) .EQ. 'PRECIPTIMEHORIZON')  Then
           Call CHRTRIM (STRING,' ')
           POS1 = INDEX(STRING, '=')
           If (POS1 .GT. 0) Read(STRING(POS1+1:),*) NTIMHP
!          If (NTIMH .LT. NTIMHP) Then
!             Write(IOUT1,*) ' Time horizon should be <=', NTIMH
!             call write_error_message_rtc (913, 0, 'Rdini', ' Time horizon', IOUT1)
!          Endif
        ElseIf (STRING(1:POS1-1) .EQ. 'WINDTIMEHORIZON')  Then
           Call CHRTRIM (STRING,' ')
           POS1 = INDEX(STRING, '=')
           If (POS1 .GT. 0) Read(STRING(POS1+1:),*) NTIMHW
!          If (NTIMH .LT. NTIMHW*2) Then
!          If (NTIMH .LT. NTIMHW*2) Then
!             Write(IOUT1,*) ' Time horizon should be <=', NTIMH/2
!             call write_error_message_rtc (913, 0, 'Rdini', ' Time horizon', IOUT1)
!          Endif
        ElseIf (STRING(1:POS1-1) .EQ. 'DECISIONHORIZON')  Then
!          New: read max. decision time horizon from INI file
           Call CHRTRIM (STRING,' ')
           POS1 = INDEX(STRING, '=')
           If (POS1 .GT. 0) Read(STRING(POS1+1:),*) NTIMSH
           NTIMSH = -1 *  ABS (NTIMSH)
        ElseIf (STRING(1:POS1-1) .EQ. 'WRITERTCHISFILES')  Then
           CDUM=STRING(POS1+1:)
           Call CHRTRIM (CDUM,' ')
           LEN = 2
           WRTHIS = .False.
           If (CDUM(1:LEN) .EQ. '-1') WRTHIS=.True.
        ElseIf (STRING(1:POS1-1) .EQ. 'OUTPUTTIMESTEP')  Then
           CDUM=STRING(POS1+1:)
           Read(CDUM,*) OutputTimestep
        ElseIf (STRING(1:POS1-1) .EQ. 'NLOCHIS')  Then
           CDUM=STRING(POS1+1:)
           Read(CDUM,*) NLocHis  ! Max. nr. locations from RR/CF/WQ HIS file
        ElseIf (STRING(1:POS1-1) .EQ. 'NTIMHIS')  Then
           CDUM=STRING(POS1+1:)
           Read(CDUM,*) NTimHis  ! Max. nr. timesteps from external HIS file
        ElseIf (STRING(1:POS1-1) .EQ. 'NPARQ')  Then
           CDUM=STRING(POS1+1:)
           Read(CDUM,*) NPARQ    ! Max. nr. parameters in WQ HIS file
        ElseIf (STRING(1:POS1-1) .EQ. 'LANGUAGE')  Then
           CDUM=STRING(POS1+1:)
           Read(CDUM,*) ActiveLanguage
        ElseIf (STRING(1:POS1-1) .EQ. 'DEBUG')  Then
           CDUM=STRING(POS1+1:)
           Call CHRTRIM (CDUM,' ')
           LEN = 2
           If (CDUM(1:LEN) .EQ. '-1') IDEBUG = IdebugLun
        ElseIf (STRING(1:POS1-1) .EQ. 'DEBUGTIME') Then
           CDUM=STRING(POS1+1:)
           Read(CDUM,*) DebugFromTimestep, DebugToTimestep
        ElseIf (STRING(1:POS1-1) .EQ. 'DEBUGTIME2') Then
           CDUM=STRING(POS1+1:)
           Read(CDUM,*) DebugFromTimestep2, DebugToTimestep2
        ElseIf (STRING(1:POS1-1) .EQ. 'SETSEQUENCEDECISIONPARAMETERS') Then
           CDUM=STRING(POS1+1:)
           Read(CDUM,*) Idum
           If (IDum .ne. 0) SetSequenceDecisionParameters = .true.
           If (IDum .eq. 0) SetSequenceDecisionParameters = .false.
        ElseIf (STRING(1:POS1-1) .EQ. 'WINDUSETABLEMODULE') Then
           CDUM=STRING(POS1+1:)
           Read(CDUM,*) Idum
           If (IDum .ne. 0) WindUseTableModule = .true.
           If (IDum .eq. 0) WindUseTableModule = .false.
        ElseIf (STRING(1:POS1-1) .EQ. 'REDUCEWINDTABLE') Then
           CDUM=STRING(POS1+1:)
           Read(CDUM,*) Idum
           If (IDum .eq. 0) ReduceWindTable = .false.
        ElseIf (STRING(1:POS1-1) .EQ. 'RESERVOIRMAXITERATIONS') Then
           CDUM=STRING(POS1+1:)
           Read(CDUM,*) MxIter
        ElseIf (STRING(1:POS1-1) .EQ. 'RESERVOIRVOLUMECONVERGENCECRITERION') Then
           CDUM=STRING(POS1+1:)
           Read(CDUM,*) EpsVolume
        ElseIf (STRING(1:POS1-1) .EQ. 'RESERVOIRFLOWCONVERGENCECRITERION') Then
           CDUM=STRING(POS1+1:)
           Read(CDUM,*) EpsFlow
        ElseIf (STRING(1:POS1-1) .EQ. 'FORMATMEASUREFILES209003') Then
           CDUM=STRING(POS1+1:)
           Call CHRTRIM (CDUM,' ')
           LEN = 2
           OldFormatMeasureFiles = .False.
           If (CDUM(1:LEN) .EQ. '-1') OldFormatMeasureFiles=.True.
        Endif
      Endif
      POS1 = INDEX(STRING, '[CONTROL]')
      If (POS1 .LE. 0) GOTO 103

! *********************************************************************
! *** read data Control block If necessary
! *********************************************************************
!
!altijd dit blok lezen  104 If (TIMF3B) GOTO 106
  105 Read(IN,'(A)',END=21,ERR=150,IOSTAT=IECODE)  STRING
      Call CHRTRIM (STRING,' ')
      Call UPPERC (STRING)
      POS1 = INDEX(STRING, '=')
      If (POS1 .GT. 0) Then
        If (STRING(1:POS1-1) .EQ. 'NUMBEROFEVENTS') Then
           Read(STRING(POS1+1:),*) NEVENT
        ElseIf (STRING(1:POS1-1) .EQ. 'NUMBEROFSTEPSPERKEER')  Then
           Read(STRING(POS1+1:),*) NSTEP
        ElseIf (STRING(1:POS1-1) .EQ. 'DELTAT') Then
           Read(STRING(POS1+1:),*) DELTAT
        ElseIf (STRING(1:POS1-1) .EQ. 'TIMESTART') Then
           Read(STRING(POS1+1:),*) RTC_TIMNEW
           Read(STRING(POS1+1:),'(I4,I2,I2,1X,I2,I2,I2)') IYEAR, IMO, IDAY, IHOUR, IMIN, ISEC
           RSEC = ISEC
        ElseIf (STRING(1:POS1-1) .EQ. 'LASTTIMESTEP') Then
           Read(STRING(POS1+1:),*) LASTTM
        Endif
      Endif
!     Go on to Matlab data. PJ van Overloop 11-5-1998
      POS1 = INDEX(STRING, '[COUPLINGMATLAB]')
      If (POS1 .GT. 0) GOTO 107
      GOTO 105
!
! *********************************************************************
! *** read data Matlab block If necessary
! *********************************************************************
!
  106 Read(IN,'(A)',END=21,ERR=150,IOSTAT=IECODE)  STRING
      Call UPPERC (STRING)
      Call CHRTRIM (STRING, ' ')
      POS1 = INDEX(STRING, '[COUPLINGMATLAB]')
      If (POS1 .LE. 0) GOTO 106

  107 Read(IN,'(A)',END=21,ERR=150,IOSTAT=IECODE)  STRING
      STRING_org = STRING
      Call UPPERC (STRING)
      Call CHRTRIM (STRING,' ')
      POS1 = INDEX(STRING, '=')
      If (POS1 .GT. 0) Then
        If (STRING(1:POS1-1) .EQ. 'MATLABDEBUGMODE') Then
           Read(STRING(POS1+1:),*) MATDBG
           MATDBG = ABS (MATDBG)
        ElseIf (STRING(1:POS1-1) .EQ. 'MATLABEXEC')  Then
           MATEXE = STRING_org(POS1+1:)
        ElseIf (STRING(1:POS1-1) .EQ. 'MATLABMFILEDIR')  Then
           MATDIR = STRING_org(POS1+1:)
        ElseIf (STRING(1:POS1-1) .EQ. 'MATLABMFILENAME') Then
           MATFIL = STRING_org(POS1+1:)
        ElseIf (STRING(1:POS1-1) .EQ. 'MATLABRRDATA') Then
           Read(STRING(POS1+1:),*) Idum
           If (IDum .ne. 0) MatRR = .true.
        ElseIf (STRING(1:POS1-1) .EQ. 'MATLABRAINDATA') Then
           Read(STRING(POS1+1:),*) Idum
           If (IDum .ne. 0) MatRain = .true.
        ElseIf (STRING(1:POS1-1) .EQ. 'MATLABRAINPREDICT') Then
           Read(STRING(POS1+1:),*) Idum
           If (IDum .ne. 0) MatRainPredict = .true.
        ElseIf (STRING(1:POS1-1) .EQ. 'MATLABWINDPREDICT') Then
           Read(STRING(POS1+1:),*) Idum
           If (IDum .ne. 0) MatWindPredict = .true.
        ElseIf (STRING(1:POS1-1) .EQ. 'MATLABPREDICTMULTI') Then
           Read(STRING(POS1+1:),*) Idum
           If (IDum .ne. 0) MatPredictMulti = .true.
        ElseIf (STRING(1:POS1-1) .EQ. 'MATLABWQDATA') Then
           Read(STRING(POS1+1:),*) Idum
           If (IDum .ne. 0) MatWQ = .true.
        ElseIf (STRING(1:POS1-1) .EQ. 'MATLABNRWQPAR') Then
           Read(STRING(POS1+1:),*) MatlabNrWqPar
        ElseIf (STRING(1:POS1-1) .EQ. 'MATLABWITHSOBEKCSTRING') Then
           Read(STRING(POS1+1:),*) Idum
           If (IDum .ne. 0) WithoutSobekCString = .false.
        ElseIf (STRING(1:POS1-1) .EQ. 'MATLABCOMMUNICATIONOLDSTYLE') Then
           CDUM=STRING(POS1+1:)
           Call CHRTRIM (CDUM,' ')
           LEN = 2
           MatlabCommunicationOldStyle = .False.
           If (CDUM(1:LEN) .EQ. '-1') MatlabCommunicationOldStyle = .True.
        ElseIf (STRING(1:POS1-1) .EQ. 'MAXCOUNTGENERAL') Then
           Read(STRING(POS1+1:),*) MaxCountGeneral
        ElseIf (STRING(1:POS1-1) .EQ. 'MAXCOUNTMEASLOCATIONS') Then
           Read(STRING(POS1+1:),*) MaxCountMeasLocations
        ElseIf (STRING(1:POS1-1) .EQ. 'MAXCOUNTBILCLOCATIONS') Then
           Read(STRING(POS1+1:),*) MaxCountBiLcLocations
        ElseIf (STRING(1:POS1-1) .EQ. 'MAXCOUNTSTRUCTURES') Then
           Read(STRING(POS1+1:),*) MaxCountStructures
        ElseIf (STRING(1:POS1-1) .EQ. 'MAXCOUNT1D2DLOCATIONS') Then
           Read(STRING(POS1+1:),*) MaxCount1D2DLocations
        ElseIf (STRING(1:POS1-1) .EQ. 'TOMATLAB1D2DH') Then
           Read(STRING(POS1+1:),*) ToMatlab1D2DH
           ToMatlab1D2DH = Abs(ToMatlab1D2DH)
        ElseIf (STRING(1:POS1-1) .EQ. 'TOMATLAB1D2DWD') Then
           Read(STRING(POS1+1:),*) ToMatlab1D2DWD
           ToMatlab1D2DWD = Abs(ToMatlab1D2DWD)
        ElseIf (STRING(1:POS1-1) .EQ. 'TOMATLAB1D2DBL') Then
           Read(STRING(POS1+1:),*) ToMatlab1D2DBL
           ToMatlab1D2DBL = Abs(ToMatlab1D2DBL)
        ElseIf (STRING(1:POS1-1) .EQ. 'TOMATLAB1D2DU') Then
           Read(STRING(POS1+1:),*) ToMatlab1D2DU
           ToMatlab1D2DU = Abs(ToMatlab1D2DU)
        ElseIf (STRING(1:POS1-1) .EQ. 'TOMATLAB1D2DV') Then
           Read(STRING(POS1+1:),*) ToMatlab1D2DV
           ToMatlab1D2DV = Abs(ToMatlab1D2DV)
        ElseIf (STRING(1:POS1-1) .EQ. 'TOMATLAB1D2DC') Then
           Read(STRING(POS1+1:),*) ToMatlab1D2DC
           ToMatlab1D2DC = Abs(ToMatlab1D2DC)
        ElseIf (STRING(1:POS1-1) .EQ. 'MAXCOUNTNODES') Then
           Read(STRING(POS1+1:),*) MaxCountNodes
        ElseIf (STRING(1:POS1-1) .EQ. 'MAXCOUNTREACHSEGMENTS') Then
           Read(STRING(POS1+1:),*) MaxCountReachSegments
        ElseIf (STRING(1:POS1-1) .EQ. 'MAXCOUNTRRLOCATIONS') Then
           Read(STRING(POS1+1:),*) MaxCountRRLocations
        ElseIf (STRING(1:POS1-1) .EQ. 'MAXCOUNTRRSETPOINTS') Then
           Read(STRING(POS1+1:),*) MaxCountRRSetpoints
        ElseIf (STRING(1:POS1-1) .EQ. 'MAXCOUNCFSETPOINTS') Then
           Read(STRING(POS1+1:),*) MaxCountCFSetpoints
        ElseIf (STRING(1:POS1-1) .EQ. 'ONMATLABERRORQUIT')  Then
           CDUM=STRING(POS1+1:)
           Call CHRTRIM (CDUM,' ')
           LEN = 2
           ONMatlabErrorQuit = .False.
           If (CDUM(1:LEN) .EQ. '-1') OnMatlabErrorQuit=.True.
        Endif
      Endif
!     Go on to External dll data
      POS1 = INDEX(STRING, '[COUPLINGEXTERNALDLL]')
      If (POS1 .GT. 0) GOTO 109
      POS1 = INDEX(STRING, '[COUPLINGEXTERNALEXE]')
      If (POS1 .GT. 0) GOTO 111
      GOTO 107
!
! *********************************************************************
! *** read data DLL block If necessary
! *********************************************************************
!
  108 Read(IN,'(A)',END=21,ERR=150,IOSTAT=IECODE)  STRING
      Call UPPERC (STRING)
      Call CHRTRIM (STRING, ' ')
      POS1 = INDEX(STRING, '[COUPLINGEXTERNALDLL]')
      If (POS1 .LE. 0) GOTO 108

  109 Read(IN,'(A)',END=21,ERR=150,IOSTAT=IECODE)  STRING
      Call UPPERC (STRING)
      Call CHRTRIM (STRING,' ')
      POS1 = INDEX(STRING, '=')
      If (POS1 .GT. 0) Then
        If (STRING(1:POS1-1) .EQ. 'DLLNAME') Then
           Read(STRING(POS1+1:),*) dllname
#if (defined(HAVE_CONFIG_H))
           dllname(len_trim(dllname)+1:) = '.so'
#else
           dllname(len_trim(dllname)+1:) = '.dll'
#endif
           istat = 0
           istat = rtc_open_shared_library(dll_handle, dllname)
           if (istat /= 0) then
              call write_error_message_rtc (956, 0, 'Rdini', dllname(1:len_trim(dllname)), IOUT1)
           endif
        ElseIf (STRING(1:POS1-1) .EQ. 'DLLFUNCTION')  Then
           Read(STRING(POS1+1:),*) dll_function
        ElseIf (STRING(1:POS1-1) .EQ. 'DLLTEST')  Then
           CDUM=STRING(POS1+1:)
           Call CHRTRIM (CDUM,' ')
           LEN = 2
           dll_test = 0
           If (CDUM(1:LEN) .EQ. '-1' .or. CDUM(1:1) .eq. '1') dll_test = -1
        Endif
      Endif

!     Go on to External exe or batch
      POS1 = INDEX(STRING, '[COUPLINGEXTERNALEXE]')
      If (POS1 .GT. 0) GOTO 111
      GOTO 109
!
! *********************************************************************
! *** read data exe block If necessary
! *********************************************************************
!
  110 Read(IN,'(A)',END=21,ERR=150,IOSTAT=IECODE)  STRING
      Call UPPERC (STRING)
      Call CHRTRIM (STRING, ' ')
      POS1 = INDEX(STRING, '[COUPLINGEXTERNALEXE]')
      If (POS1 .LE. 0) GOTO 110

  111 Read(IN,'(A)',END=21,ERR=150,IOSTAT=IECODE)  STRING
      Call UPPERC (STRING)
      Call CHRTRIM (STRING,' ')
      POS1 = INDEX(STRING, '=')
      If (POS1 .GT. 0) Then
        If (STRING(1:POS1-1) .EQ. 'RUNCOMMAND') Then
           if (String(Pos1+1:) .ne. '') Runcommand = STRING(POS1+1:)
!           Write(*,*)  ' Runcommand Read in RdIni: ', Runcommand(1:len_trim(Runcommand))
        ElseIf (STRING(1:POS1-1) .EQ. 'WRITECSV')  Then
!           if (String(Pos1+1:) .ne. '') Read(STRING(POS1+1:),*) WriteCSVFile
           if (String(Pos1+1:) .ne. '') WriteCSVFile = STRING(POS1+1:)
!           Write(*,*)  ' WriteCSVFile Read in RdIni: ', WriteCsvFile(1:len_trim(WriteCSVFile))
        ElseIf (STRING(1:POS1-1) .EQ. 'READCSV')  Then
!           if (String(Pos1+1:) .ne. '') Read(STRING(POS1+1:),*) ReadCSVFile
           if (String(Pos1+1:) .ne. '') ReadCSVFile = STRING(POS1+1:)
!           Write(*,*)  ' ReadCSVFile Read in RdIni: ', ReadCsvFile(1:len_trim(ReadCSVFile))
        Endif
      Endif
      GOTO 111

! *********************************************************************
! *** Error during reading of file
! *********************************************************************
!
  150 CONTINUE
      call write_error_message_rtc (902, IECODE, 'Rdini', ' RTC_INIfile', IOUT1)

! *********************************************************************
! *** end of file
! *********************************************************************
!
   21 CONTINUE

      If (dll_handle .ne. 0 .and. dll_function .eq. '') then
         call write_error_message_rtc (957, 0, 'Rdini', ' open shared library function not defined ', iout1)
      endif

! ARS 9711
      If (MatWq .and. UseWq .and. (MatlabNrWqPar .gt. 0)) then
         Allocate ( MatlabWqParId(MatlabNrWqPar))
         Allocate ( MatlabWq     (MatlabNrWqPar))
         MatlabWqParId=''
         MatlabWq     =0
         Rewind(In)
  201    Read(IN,'(A)',END=202,ERR=150,IOSTAT=IECODE)  STRING
         Call UPPERC (STRING)
         Call CHRTRIM (STRING,' ')
         POS1 = INDEX(STRING, '=')
         If (STRING(1:11) .EQ. 'MATLABWQPAR') Then
            Read(String(12:POS1-1),*) Idum
            if (idum .le. MatlabNrWqPar) Read(String(Pos1+1:),*) MatlabWqParId(Idum)
         Endif
         Goto 201
   202   Continue
         Do iDum=1,MatlabNrWqPar
            If (MatlabWqParId(idum) .eq. '') then
                call write_error_message_rtc (952, Idum, 'Rdini', ' ', IOUT1)
            Endif
         EndDo
      Else
         MatWq = .false.
      Endif
! End ARS 9711

      NTimH  = Max ( NTimHp, NTimHw * 2 )
      NTimsH = Min ( -1, NTimsH)
      NTimS  = - NTimsH + 1
      UseP   = (UsePre .or. UseP)
      UsePre = (UsePre .or. UseP)

! Adjust Matlab Directory to include the full path; only on PC!!
#if (defined(HAVE_CONFIG_H))
      ! no adjustment of MatDir
#else
!     write(*,*) ' MatDir = ', MatDir
      IResult = GetCwd (CurDirName)
!     write(*,*) ' GetCwd result ', Iresult, CurDirName
      IResult = FullPathQQ (MatDir, FullPathMatDir)
!     write(*,*) ' FullPathQQ result ', Iresult, FullPathMatDir
!     Iresult = length of FullPathMatDir, or 0 in case of failure
      if (IResult .gt. 0) MatDir = FullPathMatDir
!     write(*,*) ' New MatDir = ', MatDir
#endif

      If (IDEBUG .GT. 0) Then
        Write (IDEBUG,*) ' Version             =',VERSION
        Write (IDEBUG,*) ' Date                =',DATEVersion
        Write (IDEBUG,*) ' UseSbk              =',USESBK
        Write (IDEBUG,*) ' Use3B               =',USE3B
        Write (IDEBUG,*) ' Use3D               =',USE3D
        Write (IDEBUG,*) ' UseWQ               =',USEWQ
        Write (IDEBUG,*) ' SobekMode           =',SOBEKMODE
        Write (IDEBUG,*) ' CoupledToDelft3D    =',CoupledToDelft3D
        Write (IDEBUG,*) ' UsePrecipitation    =',USEPRE
        Write (IDEBUG,*) ' ModePrecipitation   =',IMODEP
        Write (IDEBUG,*) ' UseExt              =',USEEXT
        Write (IDEBUG,*) ' UseWind             =',USEW
        Write (IDEBUG,*) ' ModeWind            =',IMODEW
        Write (IDEBUG,*) ' IOPTS               =',IOPTS
        Write (IDEBUG,*) ' IOPT3               =',IOPT3
        Write (IDEBUG,*) ' IOPTWQ              =',IOPTWQ
!       Write (IDEBUG,*) ' IOPTP               =',IOPTP
!       Write (IDEBUG,*) ' IOPTE               =',IOPTE
        Write (IDEBUG,*) ' DecisionHorizon NTimsH  =',NTimsH
        Write (IDEBUG,*) ' DecisionHorizon NTims   =',NTims
        Write (IDEBUG,*) ' PrecipHorizon       =',NTimHP
        Write (IDEBUG,*) ' WindHorizon         =',NTimHW
        Write (IDEBUG,*) ' Max.Horizon         =',NTimH
        Write (IDEBUG,*) ' TimeControlFrom3B   =',TIMF3B
        Write (IDEBUG,*) ' NumberofEvents      =', NEVENT
        Write (IDEBUG,*) ' NumberofStepsperkeer=', NSTEP
        Write (IDEBUG,*) ' Deltat              =', DELTAT
        Write (IDEBUG,*) ' Timestart           =', RTC_TIMNEW
        Write (IDEBUG,*) ' Timestart           =', IYEAR,IMO,IDAY,IHOUR,IMIN,ISEC
        Write (IDEBUG,*) ' LastTimestep        =', LASTTM
        Write (IDEBUG,*) ' WriteHisfile        =', WRTHIS
        Write (IDEBUG,*) ' MatlabDebugMode     =', MATDBG
        Write (IDEBUG,*) ' MatlabMfileDir      =', MATDIR
        Write (IDEBUG,*) ' MatlabMfileName     =', MATFIL
        Write (IDEBUG,*) ' WQtoMatlab          =', MatWQ
        Write (IDEBUG,*) ' RRtoMatlab          =', MatRR
        Write (IDEBUG,*) ' RaintoMatlab        =', MatRain
        Write (IDEBUG,*) ' MxIter              =', MxIter
        Write (IDEBUG,*) ' EpsVolume           =', EpsVolume
        Write (IDEBUG,*) ' EpsFlow             =', EpsFlow
        Write (IDEBUG,*) ' Language            =', ActiveLanguage
        Write (IDEBUG,*) ' NLocHis             =', NLocHis
        Write (IDEBUG,*) ' NTimHis             =', NTimHis
        Write (IDEBUG,*) ' NParQ               =', NParQ
        Write (IDEBUG,*) ' MaxCountGeneral     =', MaxCountGeneral
        Write (IDEBUG,*) ' MaxCountRRLocations =', MaxCountRRLocations
        Write (IDEBUG,*) ' MaxCountBiLcLocs    =', MaxCountBiLcLocations
        Write (IDEBUG,*) ' MaxCountMeasLocs    =', MaxCountMeasLocations
        Write (IDEBUG,*) ' MaxCountStructures  =', MaxCountStructures
        Write (IDEBUG,*) ' MaxCountNodes       =', MaxCountNodes
        Write (IDEBUG,*) ' MaxCountReachSegmts =', MaxCountReachSegments
        Write (IDEBUG,*) ' MaxCountRRSetpoints =', MaxCountRRSetpoints
        Write (IDEBUG,*) ' MaxCountCFSetpoints =', MaxCountCFSetpoints
        Write (IDEBUG,*) ' DLL_name            =', dllname(1:Len_trim(dllname))
        Write (IDEBUG,*) ' DLL_Handle          =', DLL_handle
        Write (IDEBUG,*) ' DLL_Function        =', DLL_Function(1:Len_trim(Dll_Function))
      Endif

!
! Stuk uit INIT1 overgenomen, nu Deltat altijd uit INI file wordt gelezen
!
! ********************************************************************
! *** Determine op basis van DELTAT de tijdstap in IDHR, IDM en IDS
! ***  (increment in uren, minuten seconden per berekeningstijdstap)
! ********************************************************************

      IF (DELTAT .LE. 1.) THEN
        IDHR   = DELTAT * 100.
        IF (IDEBUG .GT. 0) WRITE(IDEBUG,*) DELTAT, IDHR
        TIMCHK = (DELTAT - IDHR/100.) * 100.
        IDMIN  = TIMCHK * 100.
        IF (IDEBUG .GT. 0) WRITE(IDEBUG,*) TIMCHK, IDMIN
        TIMCHK = (TIMCHK - IDMIN/100.) * 100.
        IDSEC  = TIMCHK * 100.
        IF (IDEBUG .GT. 0) WRITE(IDEBUG,*) TIMCHK, IDSEC
!
!  Corrections igv afrondingen ipv .0100000 bv. 0.009999999
!   zou tijdstappen van 99 min en 99 sec. geven ipv 1 uur.
!
        IF (IDSEC .GT. 60) THEN
            IF (IDEBUG .GT. 0) WRITE(IDEBUG,*) ' Correct IDSEC', IDSEC, DELTAT
!           IF (DELTAT .LE. .0001) IDMIN = 1
            IDMIN = IDMIN + 1
            IDSEC = 0
        ENDIF
        IF (IDMIN .GT. 60) THEN
            IF (IDEBUG .GT. 0) WRITE(IDEBUG,*) ' Correct IDMIN', IDMIN, DELTAT
!           IF (DELTAT .LE. .01)  IDHR = 1
            IDHR  = IDHR  + 1
            IDMIN= 0
        ENDIF
        IF (IDHR .GT. 24) THEN
!            WRITE(*,*) ' Correct IDHR'
            IDHR = 24
        ENDIF
      ELSE
        WRITE(*,*) ' Deltat > 1 day not yet tested'
        WRITE(IOUT1,*) ' Deltat > 1 day not yet tested'
!         Bij DELTAT > 1 dag, neem aan dat het altijd een geheel aantal uren is!
          IDHR = DELTAT * 24
          IDMIN = 0
          IDSEC = 0
!       STOP 99
      ENDIF
      ITMSIZ = IDHR * 3600 + IDMIN * 60 + IDSEC
      RTMSIZ = ITMSIZ

      RETURN
      END
