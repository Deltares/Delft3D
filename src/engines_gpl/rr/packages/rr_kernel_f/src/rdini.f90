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

       SUBROUTINE RR_RDINI (IN, IDEBUG, RRCaseSens, DefaultT0OutputValue, &
                   RestartFileEachTimestep, RestartFileNameEachTimestepOption, RestartPrefix, SkipStorageCoefFromRestartFile, &
                   OutputDesird, Ndim, VullingsgraadMaximum100Percent, &
                   MessageInundation, MessageVolumeCheck, MessagePerTimestep,&
                   IdebugFromTimestep, IdebugToTimestep, &
                   Idebug2FromTimestep, Idebug2ToTimestep, &
                   IdebugCapsimFromTimestep, IdebugCapsimToTimestep, &
                   MaxItr, CheckBalance, SkipBinFile, &
                   OpenWaterLevelComp, StructureOperation, &
                   LowestRestartGroundwaterLevel,SkipBoundLevelFromRestartFile, DrownedWeirDepth, &
                   OpenWaterPrecipitation, OpenwaterSeepage, EvapYear, GreenHousYear, &
                   EmulateUnixOnPc, HeaderRunoffOutAlways, &
                   VolumeCheckFactorToCF, CFBoundaryConstantInTimestep, VolumeCheckFactorToOW, &
                   MinimumDepthCF, StartSecDailyRainfall, TimeMessage, &
                   IniNewFormatCropFactors, IniCropDefinition, IniOpenWaterCropDefinition, &
                   IniNewFormatSoildata, IniSoilDefinition, IniNewFormatKasdata, IniKasDefinition,  &
                   ParseTokenMethod, StructComp, FixArsControllerLvlCheck, &
                   EstimateRemainingDuration, LargeBalanceErrorPerc, GenerateAanvoerAbr, HisConvergence, &
                   WriteRestartFileWithAdimC, ReadAdimCInRestartFile, &
                   MaxNrVShapeWeirReductionPoints, NrVShapeWeirReductionPoints, &
                   VShapeWeirH2H1Ratio, VShapeWeirFlowReductionFactor, OutputOwAndGwToRtc, &
                   NwrwContinuous, RestartOrderStrict, FixARS13964OW, ReduceRROpenWaterInfiltrationAtNegativeVolume,&
                   TestSaveSTate, RestoreTimeStep, RestoreInTimestep, ActiveLanguage, &
                   AbsoluteConvergenceCheck, RelativeConvergenceCheck, AddLinkPrefix, LinkPrefix, SaltConcentrationDWF, &
                   GlobalNamAlfa, GenerateOldWagmodOutputFiles, OldPavedComputations, UseOpenDAFile, UseWalrus, WalrusZeta1, WalrusZeta2, &
                   Walrus_min_deltime, Walrus_maxhchange, Walrus_minh, Walrus_max_Pstep, Walrus_Max_Substeps)


      use Unpaved
      use NWRW
      use NetCdfData
      use nctimeseries, only : PrecipitationSeriesId, EvaporationSeriesId, TemperatureSeriesId


! *********************************************************************
! ***                D E L F T         H Y D R A U L I C S
! ***
! ***              WATER RESOURCES AND ENVIRONMENT DIVISION
! *********************************************************************
! *** Program :  3B version 2.25.03                Date: June 1997
! *********************************************************************
! *** Last update: June   1997       By : Geert Prinsen
! *********************************************************************
! *** Brief description:
! *** ------------------
! ***   Inlezen INI file; based on RTC routine RDINI
! *********************************************************************
! *** Input/output parameters:
! *** ------------------------
! ***  IDEBUG = file unit number of debug file
! ***  IN     = file unit number of input file
! *********************************************************************
!
      implicit none

      LOGICAL      CheckBalance, RRCaseSens, GenerateOldWagmodOutputFiles, OldPavedComputations, UseOpenDAFile, UseWalrus
      INTEGER      IdebugFromTimestep, IdebugToTimestep, &
                   Idebug2FromTimestep, Idebug2ToTimestep, &
                   IdebugCapsimFromTimestep, IdebugCapsimToTimestep, MaxItr
      Integer      MessageInundation, MessageVolumeCheck, MessagePerTimestep

      Real         WalrusZeta1, WalrusZeta2
      Double precision Walrus_min_deltime, Walrus_maxhchange, Walrus_minh, Walrus_max_Pstep, Walrus_Max_Substeps
      Real         AbsoluteConvergenceCheck, RelativeConvergenceCheck
      Real         SaltConcentrationDWF
      Real         DefaultT0OutputValue
      Integer      RestartFileEachTimeStep, RestartFileNameEachTimeStepOption

      Real         LowestRestartGroundwaterLevel, LargeBalanceErrorPerc
      Logical      SkipBoundLevelFromRestartFile, SkipStorageCoefFromRestartFile, RestartOrderStrict
      Integer      DrownedWeirDepth, OpenWaterLevelComp, OpenWaterPrecipitation, OpenwaterSeepage, &
                   StructureOperation, CFBoundaryConstantInTimestep
      INTEGER      POS1, IECODE, Idum
      Logical      SkipBinfile, EmulateUnixOnPC, HeaderRunoffOutAlways, TimeMessage
      Integer      NDim
      Logical      OutputDesird(Ndim)
      INTEGER      EvapYear, GreenHousYear, StructComp
      Real         VolumeCheckFactorToCF, MinimumDepthCF, VolumeCheckFactorToOW
      Real         StartSecDailyRainfall
      Logical      VullingsgraadMaximum100Percent
      Logical      FixARS13964OW, ReduceRROpenWaterInfiltrationAtNegativeVolume

      CHARACTER(len=100) STRING, TempString
      CHARACTER(len=80)  RestartPrefix
      CHARACTER(len=4)   RRVersion
      Logical       SmallOutput
! Nov 2001 NewFormatCropFactorfile
      Logical       IniNewFormatCropFactors
      Character(Len=*) IniCropDefinition, IniOpenWaterCropDefinition
! Feb 2002: NewFormatSoildata and Kasdata
      Logical       IniNewFormatSoildata, IniNewFormatKasdata
      Character(Len=*) IniSoilDefinition, IniKasDefinition

      LOGICAL       EstimateRemainingDuration, ParseTokenMethod
      Logical       FixARSControllerLvlCheck  !FixArs12253
      Logical       OutputOwAndGwToRtc

      Integer       GenerateAanvoerAbr, HisConvergence

      Logical WriteRestartFileWithAdimC
      Logical ReadAdimCInRestartFile

      Integer MaxNrVShapeWeirReductionPoints
      Integer NrVShapeWeirReductionPoints
      REAL    VShapeWeirH2H1Ratio(MaxNrVShapeWeirReductionPoints), &
              VShapeWeirFlowReductionFactor(MaxNrVShapeWeirReductionPoints)
!      Integer RunOffOutHis, SeparateRwa_Dwa
!      Character*(*) DwaString, RwaString
      Logical NwrwContinuous
! HarmonIT-states
      Logical TestSaveState
      Integer RestoreTimeStep, RestoreInTimestep
! Language
      Integer ActiveLanguage
! ARS 12867
      Logical AddLinkPrefix
      Character(Len=*) LinkPrefix

!
      Real   GlobalNAMAlfa
!
      INTEGER IDEBUG, IN
      if (idebug .ne. 0) WRITE (IDEBUG,1)
    1 FORMAT (' RDINI')
!
! *********************************************************************
! *** Initialisatie
! ********************************************************************

      CleanRRFiles=.false.

      RRVersion='2.04'
      SmallOutput=.false.
      TimeMessage = .false.
      RRCaseSens = .true.
      IdebugFromTimestep = 0
      IdebugToTimestep = 0
      Idebug2FromTimestep = 0
      Idebug2ToTimestep = 0
      IdebugCapsimFromTimestep = 0
      IdebugCapsimToTimestep = 0
      SkipBinfile = .true.
      EmulateUnixOnPc = .false.
      HeaderRunoffOutAlways = .false.
      LowestRestartGroundwaterLevel = 100  ! 100 m beneden maaiveld
      SkipBoundLevelFromRestartFile = .true.   ! ARS8471 default waarde true, is wel afwijkend van oude versies!!
      SkipStorageCoefFromRestartFile = .false. ! SOBEK3-1521 default waarde false (backwards compatible)
      RestartOrderStrict = .false.  ! ARSxxxxx default waarde false, is afwijkend van oude versies
      DrownedWeirDepth = 0
      CFBoundaryConstantInTimestep = 1
      OpenWaterLevelComp = 0
      InitCapsimOption = 0        ! zet default CapsimInitialisatie=op oude methode
      StepGwlStorageCoefficient = 0.01
      HalfStepGwl = 0.5 * StepGwlStorageCoefficient
      CheckRootzoneGwl = .False.
! default DetailedGwlComputation=1 nav probleem MaartenPoort/Klaas-JanvanHeeringen Scurve en Capsim te hoge gwl
! April 2002: speed up, use gw Dh-DV relation: set DetailedGwlComputation=2 default
      DetailedGwlComputation = 2
      CumGroundwaterExceedanceOption  = 0   ! default: sommatie van de positieve afwijkingen t.o.v. de drempelwaarde

!      DrainageDeltaH = 0         ! old: default rekenen met gestapelde systemen
      DrainageDeltaH = -1         ! July 2001: default rekenen met parallelle systemen
      InitBcOption = 0            ! ARS 5120: zet default op -1 of 0; 15 mei 2000: toch op nul zetten ivm problemen Update
      InitGwlOption = -1          ! ARS 5041: extra optie op verzoek Land van Nassau
      UnSatZoneOption = 0
!      CapsimPerCropArea = 0       ! backwards compatibel: 1 keer Capsim per onverhard gebied
                                  ! (gem. wortelzone, crop van grootste areaal)
      CapsimPerCropArea = 1       ! default CapsimperCrop aangezet!  ARS 8922 jan 2002
      StructureOperation = 0
      VolumeCheckFactorToCF = 1.0
      VolumeCheckFactorToOW = 1.0
      MinimumDepthCF=0.0
!     Default Interpolatiecoefficienten
      CoefRz   = 0.1
      CoefGwl  = 100.
      PowerRz  = 2.
      PowerGwl = 2.

      KvdLVariatieopenwater = 0
      KvdLInitopenwater = 0
      KvdLDimDays = 0
      Do idum=1,ndim
         OutputDesird(idum) = .true.
      Enddo
      EstimateKvdLHistory = .false.

      UseUnpavedSCurve = 100
      PositionAlfa = 0
      FixARS5176 = .true.
      FixARS8842 = .true.
      FixARS10084 = .true.
      FixARS12253 = .true.
      FixARS13964OW = .true.
      FixARS14669 = .true.
!    Jira 24759: Capsim percolation limiter using Ksat
      FixJira24759 = .false.
      OutputOwAndGwToRtc = .true.
! ARS 11610 stability on-line RR-CF coupling; extend volumecheck
! ARS 11831: default FixArs11610=true
! ARS 12253: NWRW correction NtRain
      FixARS11610 = .true.
      LargeBalanceErrorPerc = 1.0
      EstimateRemainingDuration = .false.
      GenerateAanvoerAbr = 0
      HisConvergence     = 0
      FixARSControllerLvlCheck = .true.
      ConstantHdeZBergC = .false.
! HarmonIT
      TestSaveState = .false.
      RestoreTimeStep = 0
      RestoreInTimeStep = 0
! ARS 11831: specify some iteration criteria possible in INI file
      MaxItrGwlMaaiveld = 5
      MaxItrSeepage     = 4
! ARS 12518
      MaxItr = 10
      RelativeConvergenceCheck = 0.001
      AbsoluteConvergenceCheck = 0.000001

      NoInfiltrationWhileGwOnSurface = .true.

! Default berekening open water peilen met rekening houden met berging op geinundeerd land
! Dit geeft realistische open water peilen; de 'simple' option kan peilstijgingen tot maaiveld +0.50 m laten zien.
      OpenWaterLevelComp = 1
! Default berekening open water neerslag met areaal bij actueel peil
      OpenWaterPrecipitation = 0
! idem berekening seepage met areaal bij actueel peil, ARS 12349
      OpenWaterSeepage = 0

! ARS 2978: default altijd jaar gelijk nemen aan jaar uit buifile; tenzij anders gedefinieerd in INI file
      EvapYear      = -1
      GreenHousYear = -1
! ARS xxxx: Startdagneerslag op uur X
      StartSecDailyRainfall = 0
! Nov 2001 NewFormat CropFactorFile
      IniNewFormatCropFactors = .false.
      IniCropDefinition = 'DEFAULT'
      IniOpenWaterCropDefinition = 'DEFAULT'
! Feb 2002 NewFormat Soildata and Kasdata
      IniNewFormatSoildata = .false.
      IniSoilDefinition = 'DEFAULT'
      IniNewFormatKasdata = .false.
      IniKasDefinition = 'DEFAULT'
! April 2002: ParseToken module
      ParseTokenMethod   = .false.
      StructComp         = 0

      WriteRestartFileWithAdimC = .true.
      ReadAdimCInRestartFile    = .false.

! Default Vshape weir relation data
!   VShapeWeirH2H1Ratio should be in increasing order!!
      NrVShapeWeirReductionPoints = 14
      VShapeWeirH2H1Ratio (14) =   1.00
      VShapeWeirH2H1Ratio (13) =   0.995
      VShapeWeirH2H1Ratio (12) =   0.99
      VShapeWeirH2H1Ratio (11) =   0.985
      VShapeWeirH2H1Ratio (10) =   0.978
      VShapeWeirH2H1Ratio (9) =   0.97
      VShapeWeirH2H1Ratio (8) =   0.96
      VShapeWeirH2H1Ratio (7) =   0.95
      VShapeWeirH2H1Ratio (6) =   0.945
      VShapeWeirH2H1Ratio (5) =  0.925
      VShapeWeirH2H1Ratio (4) =  0.90
      VShapeWeirH2H1Ratio (3) =  0.885
      VShapeWeirH2H1Ratio (2) =  0.85
      VShapeWeirH2H1Ratio (1) =  0.80
      VShapeWeirFlowReductionFactor (14)  = 0.00
      VShapeWeirFlowReductionFactor (13)  = 0.10
      VShapeWeirFlowReductionFactor (12)  = 0.20
      VShapeWeirFlowReductionFactor (11)  = 0.30
      VShapeWeirFlowReductionFactor (10)  = 0.40
      VShapeWeirFlowReductionFactor (9)  = 0.50
      VShapeWeirFlowReductionFactor (8)  = 0.60
      VShapeWeirFlowReductionFactor (7)  = 0.67
      VShapeWeirFlowReductionFactor (6)  = 0.70
      VShapeWeirFlowReductionFactor (5) = 0.80
      VShapeWeirFlowReductionFactor (4) = 0.87
      VShapeWeirFlowReductionFactor (3) = 0.90
      VShapeWeirFlowReductionFactor (2) = 0.96
      VShapeWeirFlowReductionFactor (1) = 1.00
!
            ! Ksat Capsim values in cm/day
      KSatCapsim(1)  = 2.93
      KSatCapsim(2)  = 2.93
      KSatCapsim(3)  = 2.93
      KSatCapsim(4)  = 2.93
      KSatCapsim(5)  = 1.07
      KSatCapsim(6)  = 1.02
      KSatCapsim(7)  = 15.22
      KSatCapsim(8)  = 15.22
      KSatCapsim(9)  = 12.68
      KSatCapsim(10) = 12.68
      KSatCapsim(11) = 12.68
      KSatCapsim(12) = 12.68
      KSatCapsim(13) = 10.87
      KSatCapsim(14) = 25.0
      KSatCapsim(15) = 2.23
      KSatCapsim(16) = 2.12
      KSatCapsim(17) = 4.37
      KSatCapsim(18) = 4.37
      KSatCapsim(19) = 2.12
      KSatCapsim(20) = 2.12
      KSatCapsim(21) = 3.7
      ! convert to mm/day
      KSatCapsim = KSatCapsim * 10.
      UnpavedPercolationLikeSobek213=.false.
! Taiwan Nov2002
!  Added options to write RunoffOut as a His file, and to separate RWA and DWA output
!  defaults: Runoff.Out not a His file, no separation of RWA and DWA output
      RunOffOutHis = 0
      SeparateRwa_Dwa = 0
      DwaString = ' '
      RwaString = ' '
      NWRWContinuous=.false.

! ARS 12867
      AddLinkPrefix = .true.
      LinkPrefix = 'l_'
      CALL UPPERC (LinkPrefix)
!
      VullingsgraadMaximum100Percent = .false.
      ReduceRROpenWaterInfiltrationAtNegativeVolume = .false.
!     Default language
      ActiveLanguage = 1

!     CapsimPlus
      UserCoefKSat = 1.0
      UserFactVRZ  = 0.5  ! Default value 0.5 Sept 2014
      CapsimPlusFlag = 2

      SaltConcentrationDWF = 400.

      DefaultT0OutputValue = 0.0
      RestartFileEachTimeStep = 0
      RestartFileNameEachTimeStepOption = 1
      RestartPreFix = '..\fixed\'

!     WagMod
      GenerateOldWagModOutputFiles = .false.

!     Paved
!     OldPavedComputations = .false.
      OldPavedComputations = .true.
      UseOpenDAFile = .false.
      UseWalrus     = .true.
      Walrus_min_deltime  = 60.D0
      Walrus_maxhchange   = 10.D0
      Walrus_minh         = 1.D-3
      Walrus_max_Pstep    = 10.D0
      Walrus_max_substeps = 288.D0
      NetCdfTimestep       = 'Seconds'
      GenerateNetCdfOutput = .true.  ! SOBEK-50713 default changed from false to true
      GenerateHISOutput    = .true.  ! Default HIS output also on
      MeteoNetCdfInput     = .false. ! UNST-5103 added Netcdf option for meteo data, default switched off
      call nc_init ()
      GlobalNAMAlfa = 0.0


      PrecipitationSeriesId = ''
      EvaporationSeriesId = ''
      TemperatureSeriesId = ''


! *********************************************************************
! *** find record InitBcOption=
! ***             InitCapsimOption=
! ***             UnsaturatedZone=
! *********************************************************************
!
  101 READ(IN,'(A100)',END=150,ERR=150,IOSTAT=IECODE)  STRING

      TempString = String    ! Tempstring is de originele string in de originele case (upper/lower/mixed)
      CALL UPPERC (STRING)
      CALL CHRTRIM (STRING,' ')
      POS1 = INDEX(STRING, '=')
      IF (POS1 .GT. 0) Then
         IF (STRING(1:POS1-1) .EQ. 'INITBCOPTION')  THEN
            READ(STRING(POS1+1:),*) InitBcOption
            InitBcOption = ABS (InitBcOption)
         ELSEIF (STRING(1:POS1-1) .EQ. 'USEWALRUSORWAGMOD')  THEN
            useWalrus = .false.
            if (STRING(POS1+1:) .eq. 'WALRUS') UseWalrus = .true.
            if (STRING(POS1+1:) .eq. 'Walrus') UseWalrus = .true.
         ELSEIF (STRING(1:POS1-1) .EQ. 'ZETA1')  THEN
            if (useWalrus) READ(STRING(POS1+1:),*) WalrusZeta1
         ELSEIF (STRING(1:POS1-1) .EQ. 'ZETA2')  THEN
            if (useWalrus) READ(STRING(POS1+1:),*) WalrusZeta2
         ELSEIF (STRING(1:POS1-1) .EQ. 'WALRUS_MIN_DELTIME')  THEN
            if (useWalrus) READ(STRING(POS1+1:),*) Walrus_min_deltime
         ELSEIF (STRING(1:POS1-1) .EQ. 'WALRUS_MAXHCHANGE')  THEN
            if (useWalrus) READ(STRING(POS1+1:),*) Walrus_maxhchange
         ELSEIF (STRING(1:POS1-1) .EQ. 'WALRUS_MINH')  THEN
            if (useWalrus) READ(STRING(POS1+1:),*) Walrus_minh
         ELSEIF (STRING(1:POS1-1) .EQ. 'WALRUS_MAX_PSTEP')  THEN
            if (useWalrus) READ(STRING(POS1+1:),*) Walrus_max_pStep
         ELSEIF (STRING(1:POS1-1) .EQ. 'WALRUS_MAX_SUBSTEPS')  THEN
            if (useWalrus) READ(STRING(POS1+1:),*) Walrus_max_substeps
         ELSEIF (STRING(1:POS1-1) .EQ. 'CLEANRRFILES')  THEN
            READ(STRING(POS1+1:),*) IDUM
            IDUM = ABS (IDUM)
            IF (IDUM .ne. 0) CleanRRFiles = .true.
         ELSEIF (STRING(1:POS1-1) .EQ. 'INITGWLOPTION')  THEN
            READ(STRING(POS1+1:),*) InitGWLOption
            InitGWLOption = ABS (InitGWLOption)
         ELSEIF (STRING(1:POS1-1) .EQ. 'DRAINAGEDELTAH')  THEN
            READ(STRING(POS1+1:),*) DrainageDeltaH
            DrainageDeltaH = ABS (DrainageDeltaH)
         ELSEIF (STRING(1:POS1-1) .EQ. 'GLOBALNAMALFA')  THEN
            READ(STRING(POS1+1:),*) GlobalNamAlfa
            GlobalNAMAlfa = ABS (GlobalNAMAlfa)
         ELSEIF (STRING(1:POS1-1) .EQ. 'GENERATEOLDWAGMODOUTPUTFILES')  THEN
            READ(STRING(POS1+1:),*) Idum     ! 0 = false, 1 or -1 = true
            GenerateOldWagModOutputFiles = (idum .eq. 1) .or. (idum .eq. -1)
         ELSEIF (STRING(1:POS1-1) .EQ. 'OLDPAVEDCOMPUTATIONS')  THEN
            READ(STRING(POS1+1:),*) Idum     ! 0 = false, 1 or -1 = true
            OldPavedComputations = (idum .eq. 1) .or. (idum .eq. -1)
         ELSEIF (STRING(1:POS1-1) .EQ. 'USEOPENDAFILE')  THEN
            READ(STRING(POS1+1:),*) Idum     ! 0 = false, 1 or -1 = true
            UseOpenDAFile = (idum .eq. 1) .or. (idum .eq. -1)
         ELSEIF (STRING(1:POS1-1) .EQ. 'NETCDFTIMESTEP')  THEN
            READ(STRING(POS1+1:),*) NetCdfTimestep
            Call UpperC (NetCdfTimestep(1:1))
            Call LowerC (NetCdfTimestep(2:))
! March2025 switched-off   GenerateNetCdfOutput = (idum .eq. 1) .or. (idum .eq. -1)
         ELSEIF (STRING(1:POS1-1) .EQ. 'GENERATENETCDFOUTPUT')  THEN
            READ(STRING(POS1+1:),*) Idum     ! 0 = false, 1 or -1 = true
            GenerateNetCdfOutput = (idum .eq. 1) .or. (idum .eq. -1)
         ELSEIF (STRING(1:POS1-1) .EQ. 'GENERATEHISOUTPUT')  THEN
            READ(STRING(POS1+1:),*) Idum     ! 0 = false, 1 or -1 = true
            GenerateHISOutput = (idum .eq. 1) .or. (idum .eq. -1)
         ELSEIF (STRING(1:POS1-1) .EQ. 'METEONETCDFINPUT')  THEN
            READ(STRING(POS1+1:),*) Idum     ! 0 = false, 1 or -1 = true
            MeteoNetCdfInput = (idum .eq. 1) .or. (idum .eq. -1)
         ELSEIF (STRING(1:POS1-1) .EQ. 'PRECIPITATIONNETCDFSERIESID')  THEN
            READ(TempString(POS1+1:),*) PrecipitationSeriesId
         ELSEIF (STRING(1:POS1-1) .EQ. 'EVAPORATIONNETCDFSERIESID')  THEN
            READ(TempString(POS1+1:),*) EvaporationSeriesId
         ELSEIF (STRING(1:POS1-1) .EQ. 'TEMPERATURENETCDFSERIESID')  THEN
            READ(TempString(POS1+1:),*) TemperatureSeriesId
         ELSEIF (STRING(1:POS1-1) .EQ. 'CUMULATIVEGROUNDWATEREXCEEDANCE')  THEN
            READ(STRING(POS1+1:),*) CumGroundwaterExceedanceOption
            CumGroundwaterExceedanceOption  = ABS (CumGroundwaterExceedanceOption)
         ELSEIF (STRING(1:POS1-1) .EQ. 'INITCAPSIMOPTION') THEN
            READ(STRING(POS1+1:),*) InitCapsimOption
         ELSEIF (STRING(1:POS1-1) .EQ. 'STEPGWLSTORAGECOEFFICIENT') THEN
            READ(STRING(POS1+1:),*) StepGwlStorageCoefficient
         ELSEIF (STRING(1:POS1-1) .EQ. 'DETAILEDGWLCOMPUTATION') THEN
            READ(STRING(POS1+1:),*) DetailedGwlComputation
         ELSEIF (STRING(1:POS1-1) .EQ. 'CHECKROOTZONEGWL') THEN
            READ(STRING(POS1+1:),*) IDum
            IF (IDUM .NE. 0)  CheckRootzoneGwl =  .true.
         ELSEIF (STRING(1:POS1-1) .EQ. 'UNSATURATEDZONE')  THEN
            READ(STRING(POS1+1:),*) UnSatZoneOption
            UnSatZoneOption = ABS (UnSatZoneOption)
         ELSEIF (STRING(1:POS1-1) .EQ. 'CAPSIMPERCROPAREA')  THEN
            READ(STRING(POS1+1:),*) CapsimPerCropArea
            CapsimPerCropArea = ABS (CapsimPerCropArea)
         ELSEIF (STRING(1:POS1-1) .EQ. 'VOLUMECHECKFACTORTOCF') THEN
            READ(STRING(POS1+1:),*) VolumeCheckFactorToCF
            VolumeCheckFactorToCF = max (1.0, VolumeCheckFactorToCF)
         ELSEIF (STRING(1:POS1-1) .EQ. 'VOLUMECHECKFACTORTOOW') THEN
            READ(STRING(POS1+1:),*) VolumeCheckFactorToOW
            VolumeCheckFactorToOW = max (1.0, VolumeCheckFactorToOW)
         ELSEIF (STRING(1:POS1-1) .EQ. 'MINIMUMDEPTHCF') THEN
            READ(STRING(POS1+1:),*) MinimumDepthCF
         ELSEIF (STRING(1:POS1-1) .EQ. 'COEFRZ')  THEN
            READ(STRING(POS1+1:),*) CoefRz
         ELSEIF (STRING(1:POS1-1) .EQ. 'COEFGWL')  THEN
            READ(STRING(POS1+1:),*) COEFGWL
         ELSEIF (STRING(1:POS1-1) .EQ. 'POWERRZ')  THEN
            READ(STRING(POS1+1:),*) POWERRZ
         ELSEIF (STRING(1:POS1-1) .EQ. 'POWERGWL')  THEN
            READ(STRING(POS1+1:),*) POWERGWL
         ELSEIF (STRING(1:POS1-1) .EQ. 'DEBUGTIME')  THEN
            READ(STRING(POS1+1:),*) IdebugFromTimestep, IdebugToTimestep
         ELSEIF (STRING(1:POS1-1) .EQ. 'DEBUGTIME2')  THEN
            READ(STRING(POS1+1:),*) Idebug2FromTimestep, Idebug2ToTimestep
         ELSEIF (STRING(1:POS1-1) .EQ. 'DEBUGTIMECAPSIM')  THEN
            READ(STRING(POS1+1:),*) IdebugCapsimFromTimestep,IdebugCapsimToTimestep
         ELSEIF (STRING(1:POS1-1) .EQ. 'KVDLVARIATIEOPENWATER')  THEN
            READ(STRING(POS1+1:),*) KvdLVariatieOpenwater
            KvdLVariatieOpenwater = ABS (KvdLVariatieOpenwater)
         ELSEIF (STRING(1:POS1-1) .EQ. 'KVDLINITOPENWATER')  THEN
            READ(STRING(POS1+1:),*) KvdLInitOpenwater
            KvdLInitOpenwater = ABS (KvdLInitOpenwater)
         ELSEIF (STRING(1:POS1-1) .EQ. 'KVDLDIMENSIONINDAYS')  THEN
            READ(STRING(POS1+1:),*) KvdLDimDays
            KvdLDimDays = Max (1, KvdLDimDays)
         ELSEIF (STRING(1:POS1-1) .EQ. 'ESTIMATEKVDLHISTORY') THEN
            READ(STRING(POS1+1:),*) Idum
            IF (IDUM .EQ. -1)  EstimateKvdLHistory = .true.
! ARS 1887: Scurve unpaved area
         ELSEIF (STRING(1:POS1-1) .EQ. 'UNPAVEDSCURVEAREAS') THEN
            READ(STRING(POS1+1:),*) UseUnpavedSCurve
            UseUnpavedSCurve = ABS (UseUnpavedSCurve)
            UseUnpavedScurve = max (UseUnpavedScurve, 1)
            UseUnpavedScurve = min(UseUnpavedScurve,UseUnpavedScurveMax)
         ELSEIF (STRING(1:POS1-1) .EQ. 'UNPAVEDSCURVEALFAOPTION') THEN
            READ(STRING(POS1+1:),*) PositionAlfa
            PositionAlfa = ABS (PositionAlfa)
!  end ARS 1887
! ARS 12518
          ELSEIF (STRING(1:POS1-1) .EQ. 'MAXITERATIONS')  THEN
             READ(STRING(POS1+1:),*) MaxItr
          ELSEIF (STRING(1:POS1-1) .EQ. 'RELATIVECONVERGENCECHECK')  THEN
             READ(STRING(POS1+1:),*) RelativeConvergenceCheck
          ELSEIF (STRING(1:POS1-1) .EQ. 'ABSOLUTECONVERGENCECHECK')  THEN
             READ(STRING(POS1+1:),*) AbsoluteConvergenceCheck
          ELSEIF (STRING(1:POS1-1) .EQ. 'MAXITRGWLMAAIVELD')  THEN
             READ(STRING(POS1+1:),*) MaxItrGwlMaaiveld
             MaxitrGwlMaaiveld = max(2,MaxItrGwlMaaiveld)
          ELSEIF (STRING(1:POS1-1) .EQ. 'MAXITRSEEPAGE')  THEN
             READ(STRING(POS1+1:),*) MaxItrSeepage
             MaxitrSeepage = max(2,MaxItrSeepage)
          ELSEIF (STRING(1:POS1-1) .EQ. 'NOINFILTRATIONWHILEGWONSURFACE')  THEN
             READ(STRING(POS1+1:),*) Idum
             IF (IDUM .EQ. 0) NoInfiltrationWhileGwOnSurface = .false.
             IF (IDUM .NE. 0) NoInfiltrationWhileGwOnSurface = .true.
          ELSEIF (STRING(1:POS1-1) .EQ. 'LOWESTRESTARTGWL')  THEN
             READ(STRING(POS1+1:),*) LowestRestartGroundwaterLevel
!  LowestRestartGroundwaterLevel moet positief zijn, want in m beneden maaiveld opgegeven
             LowestRestartGroundwaterLevel= max (0.0, LowestRestartGroundwaterLevel)
          ELSEIF (STRING(1:POS1-1) .EQ. 'SKIPBOUNDLEVELFROMRESTARTFILE')  THEN
!  ARS 8471 SkipBoundLevelFromRestartFile
             READ(STRING(POS1+1:),*) idum
             IF (IDUM .EQ. 0) SkipBoundLevelFromRestartFile = .false.
!            IF (IDUM .NE. 0) SkipBoundLevelFromRestartFile = .true.
          ELSEIF (STRING(1:POS1-1) .EQ. 'SKIPSTORAGECOEFFROMRESTARTFILE')  THEN
!  SOBEK3-1521 SkipStorageCoefFromRestartFile
             READ(STRING(POS1+1:),*) idum
             IF (IDUM .EQ. 0) SkipStorageCoefFromRestartFile = .false.
             IF (IDUM .NE. 0) SkipStorageCoefFromRestartFile = .true.
          ELSEIF (STRING(1:POS1-1) .EQ. 'RESTARTORDERSTRICT')  THEN
!  ARS      RestartOrderStrict
             READ(STRING(POS1+1:),*) idum
             IF (IDUM .EQ. 0) RestartOrderStrict = .false.
             IF (IDUM .NE. 0) RestartOrderStrict = .true.
          ELSEIF (STRING(1:POS1-1) .EQ. 'CHECKBALANCE')  THEN
!            Checkbalance=true geeft balansfout per tijdstap in IOBAL file
             READ(STRING(POS1+1:),*) Idum
             IF (IDUM .NE. 0) CheckBalance = .true.
          ELSEIF (STRING(1:POS1-1) .EQ. 'LARGEBALANCEERRORPERCENTAGE')  THEN
             READ(STRING(POS1+1:),*) LargeBalanceErrorPerc
         ELSEIF (STRING(1:POS1-1) .EQ. 'SKIPBINFILE')  THEN
            READ(STRING(POS1+1:),*) Idum
            IF (IDUM .EQ. 0)  SkipBinfile = .false.
          ELSEIF (STRING(1:POS1-1) .EQ. 'CASESENSITIVE')  THEN
             READ(STRING(POS1+1:),*) Idum
             IF (IDUM .EQ. 0)  RRCaseSens = .false.
          ELSEIF (STRING(1:POS1-1) .EQ. 'TIMEMESSAGE')  THEN
             READ(STRING(POS1+1:),*) Idum
             IF (IDUM .NE. 0)  TimeMessage = .true.
          ELSEIF (STRING(1:POS1-1) .EQ. 'LANGUAGE')  THEN
             READ(STRING(POS1+1:),*) ActiveLanguage
          ELSEIF (STRING(1:POS1-1) .EQ. 'EMULATEUNIXONPC')  THEN
             READ(STRING(POS1+1:),*) Idum
             IF (IDUM .NE. 0) EmulateUnixOnPc = .true.
          ELSEIF (STRING(1:POS1-1) .EQ. 'HEADERRUNOFFOUTALWAYS') THEN
             READ(STRING(POS1+1:),*) Idum
             IF (IDUM .NE. 0) HeaderRunoffOutAlways = .true.
          ELSEIF (STRING(1:POS1-1) .EQ. 'DROWNEDWEIRDEPTH')  THEN
             READ(STRING(POS1+1:),*) DrownedWeirDepth
             DrownedWeirDepth = ABS (DrownedWeirDepth)
          ELSEIF (STRING(1:POS1-1) .EQ. 'CFBOUNDARYCONSTANTINTIMESTEP')  THEN
             READ(STRING(POS1+1:),*) CFBoundaryConstantInTimestep
             CFBoundaryConstantInTimestep = ABS (CFBoundaryConstantInTimestep)
          ELSEIF (STRING(1:POS1-1) .EQ. 'STRUCTUREOPERATION')  THEN
             READ(STRING(POS1+1:),*) StructureOperation
             StructureOperation = ABS (StructureOperation)
          ELSEIF (STRING(1:POS1-1) .EQ. 'MESSAGEINUNDATION')  THEN
             READ(STRING(POS1+1:),*) MessageInundation
             MessageInundation = ABS (MessageInundation)
          ELSEIF (STRING(1:POS1-1) .EQ. 'MESSAGEPERTIMESTEP')  THEN
             READ(STRING(POS1+1:),*) MessagePerTimestep
             MessagePerTimestep = ABS (MessagePerTimestep)
          ELSEIF (STRING(1:POS1-1) .EQ. 'MESSAGEVOLUMECHECK')  THEN
             READ(STRING(POS1+1:),*) MessageVolumeCheck
             MessageVolumeCheck= ABS (MessageVolumeCheck)
          ELSEIF (STRING(1:POS1-1) .EQ. 'ADDLINKPREFIX')  THEN
            READ(STRING(POS1+1:),*) Idum
            IF (IDUM .EQ. 0) AddLinkPrefix = .false.
            IF (ABS(IDUM) .EQ. 1) AddLinkPrefix = .true.
          ELSEIF (STRING(1:POS1-1) .EQ. 'LINKPREFIX')  THEN
             READ(STRING(POS1+1:),*) LinkPrefix
         ELSEIF (STRING(1:POS1-1) .EQ. 'FIXARS5176') THEN
            READ(STRING(POS1+1:),*) Idum
            IF (IDUM .EQ. 0) FixARS5176 = .false.
         ELSEIF (STRING(1:POS1-1) .EQ. 'FIXARS8842') THEN
            READ(STRING(POS1+1:),*) Idum
            IF (IDUM .EQ. 0) FixARS8842 = .false.
         ELSEIF (STRING(1:POS1-1) .EQ. 'FIXARS10084') THEN
            READ(STRING(POS1+1:),*) Idum
            IF (IDUM .EQ. 0) FixARS10084 = .false.
            IF (IDUM .NE. 0) FixARS10084 = .true.
         ELSEIF (STRING(1:POS1-1) .EQ. 'FIXARS11610') THEN
            READ(STRING(POS1+1:),*) Idum
            IF (IDUM .EQ. 0) FixARS11610 = .false.
            IF (IDUM .NE. 0) FixARS11610 = .true.
         ELSEIF (STRING(1:POS1-1) .EQ. 'FIXARS12253') THEN
            READ(STRING(POS1+1:),*) Idum
            IF (IDUM .EQ. 0) FixARS12253 = .false.
            IF (IDUM .NE. 0) FixARS12253 = .true.
         ELSEIF (STRING(1:POS1-1) .EQ. 'FIXARS13964') THEN
            READ(STRING(POS1+1:),*) Idum
            IF (IDUM .EQ. 0) FixARS13964OW = .false.
            IF (IDUM .NE. 0) FixARS13964OW = .true.
         ELSEIF (STRING(1:POS1-1) .EQ. 'FIXARS14669') THEN
            READ(STRING(POS1+1:),*) Idum
            IF (IDUM .EQ. 0) FixARS14669 = .false.
            IF (IDUM .NE. 0) FixARS14669 = .true.
         ELSEIF (STRING(1:POS1-1) .EQ. 'FIXJIRA24759') THEN
            READ(STRING(POS1+1:),*) Idum
            IF (IDUM .EQ. 0) FixJira24759 = .false.
            IF (IDUM .NE. 0) FixJira24759 = .true.
         ELSEIF (STRING(1:POS1-1) .EQ. 'UNPAVEDPERCOLATIONLIKESOBEK213') THEN
            UnpavedPercolationLikeSobek213=.false.
            READ(STRING(POS1+1:),*) Idum
            IF (IDUM .EQ. 0) UnpavedPercolationLikeSobek213=.false.
            IF (IDUM .NE. 0) UnpavedPercolationLikeSobek213=.true.
         ELSEIF (STRING(1:POS1-1) .EQ. 'KSATCAPSIM1') THEN
            READ(STRING(POS1+1:),*) KSatCapsim(1)
            KSatCapsim(1)  = KSatCapsim(1) * 10.
         ELSEIF (STRING(1:POS1-1) .EQ. 'KSATCAPSIM2') THEN
            READ(STRING(POS1+1:),*) KSatCapsim(2)
            KSatCapsim(2)  = KSatCapsim(2) * 10.
         ELSEIF (STRING(1:POS1-1) .EQ. 'KSATCAPSIM3') THEN
            READ(STRING(POS1+1:),*) KSatCapsim(3)
            KSatCapsim(3)  = KSatCapsim(3) * 10.
         ELSEIF (STRING(1:POS1-1) .EQ. 'KSATCAPSIM4') THEN
            READ(STRING(POS1+1:),*) KSatCapsim(4)
            KSatCapsim(4)  = KSatCapsim(4) * 10.
         ELSEIF (STRING(1:POS1-1) .EQ. 'KSATCAPSIM5') THEN
            READ(STRING(POS1+1:),*) KSatCapsim(5)
            KSatCapsim(5)  = KSatCapsim(5) * 10.
         ELSEIF (STRING(1:POS1-1) .EQ. 'KSATCAPSIM6') THEN
            READ(STRING(POS1+1:),*) KSatCapsim(6)
            KSatCapsim(6)  = KSatCapsim(6) * 10.
         ELSEIF (STRING(1:POS1-1) .EQ. 'KSATCAPSIM7') THEN
            READ(STRING(POS1+1:),*) KSatCapsim(7)
            KSatCapsim(7)  = KSatCapsim(7) * 10.
         ELSEIF (STRING(1:POS1-1) .EQ. 'KSATCAPSIM8') THEN
            READ(STRING(POS1+1:),*) KSatCapsim(8)
            KSatCapsim(8)  = KSatCapsim(8) * 10.
         ELSEIF (STRING(1:POS1-1) .EQ. 'KSATCAPSIM9') THEN
            READ(STRING(POS1+1:),*) KSatCapsim(9)
            KSatCapsim(9)  = KSatCapsim(9) * 10.
         ELSEIF (STRING(1:POS1-1) .EQ. 'KSATCAPSIM10') THEN
            READ(STRING(POS1+1:),*) KSatCapsim(10)
            KSatCapsim(10) = KSatCapsim(10) * 10.
         ELSEIF (STRING(1:POS1-1) .EQ. 'KSATCAPSIM11') THEN
            READ(STRING(POS1+1:),*) KSatCapsim(11)
            KSatCapsim(11) = KSatCapsim(11) * 10.
         ELSEIF (STRING(1:POS1-1) .EQ. 'KSATCAPSIM12') THEN
            READ(STRING(POS1+1:),*) KSatCapsim(12)
            KSatCapsim(12) = KSatCapsim(12) * 10.
         ELSEIF (STRING(1:POS1-1) .EQ. 'KSATCAPSIM13') THEN
            READ(STRING(POS1+1:),*) KSatCapsim(13)
            KSatCapsim(13) = KSatCapsim(13) * 10.
         ELSEIF (STRING(1:POS1-1) .EQ. 'KSATCAPSIM14') THEN
            READ(STRING(POS1+1:),*) KSatCapsim(14)
            KSatCapsim(14) = KSatCapsim(14) * 10.
         ELSEIF (STRING(1:POS1-1) .EQ. 'KSATCAPSIM15') THEN
            READ(STRING(POS1+1:),*) KSatCapsim(15)
            KSatCapsim(15) = KSatCapsim(15) * 10.
         ELSEIF (STRING(1:POS1-1) .EQ. 'KSATCAPSIM16') THEN
            READ(STRING(POS1+1:),*) KSatCapsim(16)
            KSatCapsim(16) = KSatCapsim(16) * 10.
         ELSEIF (STRING(1:POS1-1) .EQ. 'KSATCAPSIM17') THEN
            READ(STRING(POS1+1:),*) KSatCapsim(17)
            KSatCapsim(17) = KSatCapsim(17) * 10.
         ELSEIF (STRING(1:POS1-1) .EQ. 'KSATCAPSIM18') THEN
            READ(STRING(POS1+1:),*) KSatCapsim(18)
            KSatCapsim(18) = KSatCapsim(18) * 10.
         ELSEIF (STRING(1:POS1-1) .EQ. 'KSATCAPSIM19') THEN
            READ(STRING(POS1+1:),*) KSatCapsim(19)
            KSatCapsim(19) = KSatCapsim(19) * 10.
         ELSEIF (STRING(1:POS1-1) .EQ. 'KSATCAPSIM20') THEN
            READ(STRING(POS1+1:),*) KSatCapsim(20)
            KSatCapsim(20) = KSatCapsim(20) * 10.
         ELSEIF (STRING(1:POS1-1) .EQ. 'KSATCAPSIM21') THEN
            READ(STRING(POS1+1:),*) KSatCapsim(21)
            KSatCapsim(21) = KSatCapsim(21) * 10.
         ELSEIF (STRING(1:POS1-1) .EQ. 'FIXARSCONTROLLERLVLCHECK') THEN
            READ(STRING(POS1+1:),*) Idum
            IF (IDUM .EQ. 0) FixARSControllerLvlCheck = .false.
         ELSEIF (STRING(1:POS1-1) .EQ. 'USERCOEFKSAT')  THEN
            READ(STRING(POS1+1:),*) UserCoefKSat
         ELSEIF (STRING(1:POS1-1) .EQ. 'USERFACTVRZ')  THEN
            READ(STRING(POS1+1:),*) UserFactVRZ
            UserFactVRZ = min (UserFactVRZ, 1.0d0)
            UserFactVRZ = max (UserFactVRZ, 0.0d0)
         ELSEIF (STRING(1:POS1-1) .EQ. 'CAPSIMPLUSFLAG')  THEN
            READ(STRING(POS1+1:),*) CapsimPlusFlag    ! 1 = initial, 2 = initial and final
         ELSEIF (STRING(1:POS1-1) .EQ. 'EVAPORATIONYEAR')  THEN
            READ(STRING(POS1+1:),*) EvapYear
         ELSEIF (STRING(1:POS1-1) .EQ. 'GREENHOUSEYEAR')  THEN
            READ(STRING(POS1+1:),*) GreenHousYear
         ELSEIF (STRING(1:POS1-1) .EQ. 'GENERATEAANVOERABR') THEN
            READ(STRING(POS1+1:),*) GenerateAanvoerAbr
         ELSEIF (STRING(1:POS1-1) .EQ. 'SALTCONCENTRATIONDWF') THEN
            READ(STRING(POS1+1:),*) SaltConcentrationDWF
         ELSEIF (STRING(1:POS1-1) .EQ. 'HISCONVERGENCE') THEN
            READ(STRING(POS1+1:),*) HisConvergence
         ELSEIF (STRING(1:POS1-1) .EQ. 'DAILYRAINFALLSTARTHOUR')  THEN
            READ(STRING(POS1+1:),*) StartSecDailyRainfall
            StartSecDailyRainfall = StartSecDailyRainfall*3600.
         ELSEIF (STRING(1:POS1-1) .EQ. 'OPENWATERLEVELCOMPUTATIONS') THEN
            If (STRING(POS1+1:POS1+6) .eq. 'SIMPLE') THEN
               OpenWaterLevelComp = 0
            ElseIf (STRING(POS1+1:POS1+8) .eq. 'ADVANCED') THEN
               OpenWaterLevelComp = 1
            Endif
         ELSEIF (STRING(1:POS1-1) .EQ. 'OPENWATERPRECIPITATION') THEN
            READ(STRING(POS1+1:),*) OpenWaterPrecipitation
         ELSEIF (STRING(1:POS1-1) .EQ. 'OPENWATERSEEPAGE') THEN
            READ(STRING(POS1+1:),*) OpenWaterSeepage
         ELSEIF (STRING(1:POS1-1) .EQ. 'NEWFORMATCROPFACTORS') THEN
            READ(STRING(POS1+1:),*) Idum
            IF (IDUM .NE. 0)  IniNewFormatCropFactors = .true.
         ELSEIF (STRING(1:POS1-1) .EQ. 'CROPDEFINITION') THEN
            READ(STRING(POS1+1:),*) IniCropDefinition
         ELSEIF (STRING(1:POS1-1) .EQ. 'OPENWATERCROPDEFINITION') THEN
            READ(STRING(POS1+1:),*) IniOpenWaterCropDefinition
! Feb 2002: optional NewFormat Soildata and Kasdata
         ELSEIF (STRING(1:POS1-1) .EQ. 'NEWFORMATSOILDATA') THEN
            READ(STRING(POS1+1:),*) Idum
            IF (IDUM .NE. 0)  IniNewFormatSoildata = .true.
         ELSEIF (STRING(1:POS1-1) .EQ. 'SOILDEFINITION') THEN
            READ(STRING(POS1+1:),*) IniSoilDefinition
         ELSEIF (STRING(1:POS1-1) .EQ. 'NEWFORMATKASDATA') THEN
            READ(STRING(POS1+1:),*) Idum
            IF (IDUM .NE. 0)  IniNewFormatKasdata = .true.
         ELSEIF (STRING(1:POS1-1) .EQ. 'KASDEFINITION') THEN
            READ(STRING(POS1+1:),*) IniKasDefinition
! end Soildata and Kasdata
         ELSEIF (STRING(1:POS1-1) .EQ. 'ESTIMATEREMAININGDURATION') THEN
            READ(STRING(POS1+1:),*) Idum
            IF (IDUM .EQ. -1)  EstimateRemainingDuration = .true.
         ELSEIF (STRING(1:POS1-1) .EQ. 'PARSETOKENMETHOD') THEN
            READ(STRING(POS1+1:),*) Idum
            IF (IDUM .EQ. -1)  ParseTokenMethod   = .true.
         ELSEIF (STRING(1:POS1-1) .EQ. 'STRUCTCOMP') THEN
            READ(STRING(POS1+1:),*) StructComp
         ELSEIF (STRING(1:POS1-1) .EQ. 'WRITERESTARTFILEWITHADIMC') THEN
            READ(STRING(POS1+1:),*) Idum
            IF (IDUM .EQ. 0)  WriteRestartFileWithAdimC  = .false.
         ELSEIF (STRING(1:POS1-1) .EQ. 'VNOTCHWEIRNRINTERPOLATIONPOINTS') then
            READ(STRING(POS1+1:),*) NrVShapeWeirReductionPoints
         ELSEIF (STRING(1:POS1-1) .EQ. 'VNOTCHWEIRH2H1RATIO') then
            READ(STRING(POS1+1:),*) (VShapeWeirH2H1Ratio(idum),idum=1,NrVShapeWeirReductionPoints)
         ELSEIF (STRING(1:POS1-1) .EQ. 'VNOTCHWEIRDROWNEDFLOWREDUCTIONFACTOR') then
            READ(STRING(POS1+1:),*) (VShapeWeirFlowReductionFactor(idum),idum=1,NrVShapeWeirReductionPoints)
!
         ELSEIF (STRING(1:POS1-1) .EQ. 'RUNOFFOUTHIS') then
            READ(STRING(POS1+1:),*) RunoffOutHis
         ELSEIF (STRING(1:POS1-1) .EQ. 'SEPARATERWA_DWA') then
            READ(STRING(POS1+1:),*) SeparateRwa_Dwa
         ELSEIF (STRING(1:POS1-1) .EQ. 'DWASTRING') then
            READ(TempString(POS1+1:),*) DwaString       ! read string in original input case (not converted to upper case)
         ELSEIF (STRING(1:POS1-1) .EQ. 'RWASTRING') then
            READ(TempString(POS1+1:),*) RwaString       ! read string in original input case (not converted to upper case)
         ELSEIF (STRING(1:POS1-1) .EQ. 'NWRWCONTINUOUS') THEN
            READ(STRING(POS1+1:),*) Idum
            IF (IDUM .NE. 0)  NwrwContinuous = .true.
         ELSEIF (STRING(1:POS1-1) .EQ. 'CONSTANTHDEZBERGC') THEN
            READ(STRING(POS1+1:),*) Idum
            IF (IDUM .NE. 0)  ConstantHdeZBergC = .true.
         ELSEIF (STRING(1:POS1-1) .EQ. 'TESTSAVESTATE') THEN
            READ(STRING(POS1+1:),*) Idum
            IF (IDUM .NE. 0)  TestSaveState = .true.
         ELSEIF (STRING(1:POS1-1) .EQ. 'OUTPUTOWANDGWTORTC') THEN
            READ(STRING(POS1+1:),*) Idum
            IF (IDUM .NE. 0)  OutputOwAndGwToRTC = .true.
            IF (IDUM .EQ. 0)  OutputOwAndGwToRTC = .false.
         ELSEIF (STRING(1:POS1-1) .EQ. 'DEFAULTT0OUTPUTVALUE') THEN
            READ(STRING(POS1+1:),*) DefaultT0OutputValue
         ELSEIF (STRING(1:POS1-1) .EQ. 'RESTARTFILEEACHTIMESTEP') THEN
            READ(STRING(POS1+1:),*) RestartFileEachTimeStep            ! 0 = no, <>0  = yes
         ELSEIF (STRING(1:POS1-1) .EQ. 'RESTARTFILENAMEEACHTIMESTEPOPTION') THEN
            READ(STRING(POS1+1:),*) RestartFileNameEachTimeStepOption  ! 0 = fixed file name, 1 = date/time
         ELSEIF (STRING(1:POS1-1) .EQ. 'RESTARTFILENAMEPREFIX') THEN
            READ(STRING(POS1+1:),*) RestartPrefix
         ELSEIF (STRING(1:POS1-1) .EQ. 'RESTORETIMESTEP') THEN
            READ(STRING(POS1+1:),*) RestoreTimeStep
         ELSEIF (STRING(1:POS1-1) .EQ. 'RESTOREINTIMESTEP') THEN
            READ(STRING(POS1+1:),*) RestoreInTimeStep
         ELSEIF (STRING(1:POS1-1) .EQ. 'VULLINGSGRAADMAXIMUM100') THEN
            READ(STRING(POS1+1:),*) Idum
            IF (IDUM .NE. 0)  VullingsGraadMaximum100Percent = .true.
         ELSEIF (STRING(1:POS1-1) .EQ. 'REDUCERROPENWATERINFILTRATIONATNEGATIVEVOLUME') THEN
            READ(STRING(POS1+1:),*) Idum
            IF (IDUM .NE. 0)  ReduceRROpenWaterInfiltrationAtNegativeVolume = .true.
         Else
!           hier de nette manier (ook via Settings beschikbaar) om uitvoer opties in te stellen
            Call RR_RDINI_part2 (String, Pos1, OutputDesird, Ndim, SmallOutput, RRVersion)
         Endif
      Endif

      if (UnpavedPercolationLikeSobek213) then
         KSatCapsim = 9999.
      endif


      GOTO 101

! *********************************************************************
! *** Error during reading of file
! *********************************************************************
!
  150 CONTINUE
!
      HalfStepGwl = 0.5 * StepGwlStorageCoefficient

      If (RRVersion .eq. '2.03' .and. Smalloutput ) then
         If (ndim .ge. 12) then
           OutputDesird(1) = .false.
           OutputDesird(2) = .false.
           OutputDesird(3) = .false.
           OutputDesird(6) = .false.
           OutputDesird(7) = .false.
           OutputDesird(9) = .false.
           OutputDesird(10) = .false.
           OutputDesird(11) = .false.
           OutputDesird(12) = .false.
         Endif
      Endif
!     If output RWA and DWA separated, then always in HIS file
      If (SeparateRwa_Dwa .ne. 0) RunoffOutHis = -1
!
! *** Set file units for Sobek-Capsim Starting Centrum
!
      If (UnSatZoneOption .ge. 1) then
         message_unit = Conffil_get_INXFIL(95)
         debug_unit = Conffil_get_INXFIL(96)
         file_unit = Conffil_get_INXFIL(93)
      endif

      if (idebug .ne. 0) THEN
        WRITE (IDEBUG,*) ' InitBcOption    ',InitBcOption
        WRITE (IDEBUG,*) ' InitGWLOption   ',InitGWLOption
        WRITE (IDEBUG,*) ' InitCapsimOption',InitCapsimOption
        WRITE (IDEBUG,*) ' DrainageDeltaH  ',DrainageDeltaH
        WRITE (IDEBUG,*) ' CumulativeGroundwaterExceedance',CumGroundwaterExceedanceOption
        WRITE (IDEBUG,*) ' UnSatZoneOption ',UnSatZoneOption
        WRITE (IDEBUG,*) ' CapsimPerCropArea',CapsimPerCropArea
        WRITE (IDEBUG,*) ' VolCheckFactorCF',VolumeCheckFactorToCF
        WRITE (IDEBUG,*) ' MinimumDepthCF  ',MinimumDepthCF
        WRITE (IDEBUG,*) ' CoefRz          ',CoefRz
        WRITE (IDEBUG,*) ' PowerRz         ',PowerRz
        WRITE (IDEBUG,*) ' CoefGwl         ',CoefGwl
        WRITE (IDEBUG,*) ' PowerGwl        ',PowerGwl
        WRITE (IDEBUG,*) ' IdebugFrom      ',IdebugFromTimestep
        WRITE (IDEBUG,*) ' IdebugTo        ',IdebugToTimestep
        WRITE (IDEBUG,*) ' IdebugFrom2     ',Idebug2FromTimestep
        WRITE (IDEBUG,*) ' IdebugTo2       ',Idebug2ToTimestep
        WRITE (IDEBUG,*) ' IdebugCapsimFrom',IdebugCapsimFromTimestep
        WRITE (IDEBUG,*) ' IdebugCapsimTo  ',IdebugCapsimToTimestep
        WRITE (IDEBUG,*) ' MaxIterations   ',MaxItr
        WRITE (IDEBUG,*) ' CheckBalance    ',CheckBalance
        WRITE (IDEBUG,*) ' KvdLVariatieOw  ',KvdLVariatieOpenwater
        WRITE (IDEBUG,*) ' KvdLInitOw      ',KvdLInitOpenwater
        WRITE (IDEBUG,*) ' KvdlDimDays     ',KvdlDimDays
        WRITE (IDEBUG,*) ' SkipBinfile     ',SkipBinfile
        WRITE (IDEBUG,*) ' EmulateUnixOnPc ',EmulateUnixOnPc
        WRITE (IDEBUG,*) ' HeaderRunoffOutAlways ',HeaderRunoffOutAlways
        WRITE (IDEBUG,*) ' LowestRestartgwl',LowestRestartGroundwaterLevel
        WRITE (IDEBUG,*) ' UseUnpavedScurve',UseUnpavedScurve
        WRITE (IDEBUG,*) ' PositionAlfa    ',PositionAlfa
        WRITE (IDEBUG,*) ' DrownedWeirDepth',DrownedWeirDepth
        WRITE (IDEBUG,*) ' OpenWaterLevelComp',OpenwaterLevelComp
        WRITE (IDEBUG,*) ' OpenWaterPrecip.  ',OpenWaterPrecipitation
        WRITE (IDEBUG,*) ' OpenWaterSeepage  ',OpenWaterSeepage
        WRITE (IDEBUG,*) ' StructureOperation',StructureOperation
        WRITE (IDEBUG,*) ' CFBoundaryConstant',CFBoundaryConstantInTimestep
        WRITE (IDEBUG,*) ' EvaporationYear   ',EvapYear
        WRITE (IDEBUG,*) ' GreenhouseYear    ',GreenHousYear
        WRITE (IDEBUG,*) ' MessageVolumeCheck',MessageVolumeCheck
        WRITE (IDEBUG,*) ' MessageInundation ',MessageInundation
        WRITE (IDEBUG,*) ' MessagePerTimestep',MessagePerTimestep
      ENDIF

      RETURN
      END Subroutine RR_RdIni



      SUBROUTINE RR_RDINI_part2 (String, Pos1, OutputDesird, Ndim, SmallOutput, RRVersion)

! *********************************************************************
! ***                D E L F T         H Y D R A U L I C S
! ***
! ***              WATER RESOURCES AND ENVIRONMENT DIVISION
! *********************************************************************
! *** Program :  3B version 2.25.03                Date: June 1997
! *********************************************************************
! *** Last update: June   1997       By : Geert Prinsen
! *********************************************************************
! *** Brief description:
! *** ------------------
! ***   Inlezen INI file part : Output options
! *********************************************************************
! *** Input/output parameters:
! *** ------------------------
! ***  IDEBUG = file unit number of debug file
! ***  IN     = file unit number of input file
! *********************************************************************


      implicit none

      INTEGER      POS1, Idum
      Integer      NDim
      Logical      OutputDesird(Ndim)

      CHARACTER(len=100) STRING
      CHARACTER(len=4)   RRVersion
      Logical       SmallOutput

      IF (STRING(1:POS1-1) .EQ. 'OUTPUTRRPAVED') THEN
         READ(STRING(POS1+1:),*) Idum
         If (IDum .eq. 0) OutputDesird(1) = .false.
      ELSEIF (STRING(1:POS1-1) .EQ. 'OUTPUTRRUNPAVED') THEN
         READ(STRING(POS1+1:),*) Idum
         If (IDum .eq. 0) OutputDesird(2) = .false.
      ELSEIF (STRING(1:POS1-1) .EQ. 'OUTPUTRRGREENHOUSE') THEN
         READ(STRING(POS1+1:),*) Idum
         If (IDum .eq. 0) OutputDesird(3) = .false.
      ELSEIF (STRING(1:POS1-1) .EQ. 'OUTPUTRROPENWATER') THEN
         READ(STRING(POS1+1:),*) Idum
         If (IDum .eq. 0) OutputDesird(4) = .false.
      ELSEIF (STRING(1:POS1-1) .EQ. 'OUTPUTRRSTRUCTURE') THEN
         READ(STRING(POS1+1:),*) Idum
         If (IDum .eq. 0) OutputDesird(5) = .false.
      ELSEIF (STRING(1:POS1-1) .EQ. 'OUTPUTRRBOUNDARY') THEN
         READ(STRING(POS1+1:),*) Idum
         If (IDum .eq. 0) OutputDesird(6) = .false.
      ELSEIF (STRING(1:POS1-1) .EQ. 'OUTPUTRRNWRW') THEN
         READ(STRING(POS1+1:),*) Idum
         If (IDum .eq. 0) OutputDesird(7) = .false.
      ELSEIF (STRING(1:POS1-1) .EQ. 'OUTPUTRRWWTP') THEN
         READ(STRING(POS1+1:),*) Idum
         If (IDum .eq. 0) OutputDesird(10) = .false.
      ELSEIF (STRING(1:POS1-1) .EQ. 'OUTPUTRRINDUSTRY') THEN
         READ(STRING(POS1+1:),*) Idum
         If (IDum .eq. 0) OutputDesird(11) = .false.
      ELSEIF (STRING(1:POS1-1) .EQ. 'OUTPUTRRSACRAMENTO') THEN
         READ(STRING(POS1+1:),*) Idum
         If (IDum .eq. 0) OutputDesird(12) = .false.
      ELSEIF (STRING(1:POS1-1) .EQ. 'OUTPUTRRRUNOFF') THEN
         READ(STRING(POS1+1:),*) Idum
         If (IDum .eq. 0) OutputDesird(15) = .false.
      ELSEIF (STRING(1:POS1-1) .EQ. 'OUTPUTRRBALANCE') THEN
         READ(STRING(POS1+1:),*) Idum
         If (IDum .eq. 0) OutputDesird(8) = .false.
      ELSEIF (STRING(1:POS1-1) .EQ. 'OUTPUTRRSALT') THEN
         READ(STRING(POS1+1:),*) Idum
         If (IDum .eq. 0) OutputDesird(9) = .false.
! ARS 10329; next line was missing
      ELSEIF (STRING(1:POS1-1) .EQ. 'OUTPUTRRLINKFLOWS') THEN
         READ(STRING(POS1+1:),*) Idum
         If (IDum .eq. 0) OutputDesird(13) = .false.
      ELSEIF (STRING(1:POS1-1) .EQ. 'REDUCED OUTPUT') THEN
         READ(STRING(POS1+1:),*) Idum
         If (IDum .ne. 0) SmallOutput = .true.
      ELSEIF (STRING(1:POS1-1) .EQ. 'VERSION') THEN
         RRVersion=STRING(POS1+1:POS1+4)
      ENDIF


      Return
      End Subroutine RR_RdIni_part2
