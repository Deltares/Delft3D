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

 ! Last changed
! by:               $Author:: Schrier           $
! at:               $Modtime:: 4-09-97 5:15p    $
!
! current revision: $Revision:: 18              $



 Module RRmodule

!*********************************************************************
!***                D E L F T         H Y D R A U L I C S
!***
!***              WATER RESOURCES AND ENVIRONMENT DIVISION
!*********************************************************************
!*** Program :  DELFT-3B version 2.09                Date: Jan   2004
!*** Module  :
!*********************************************************************
!*** Created    : March  1995                     By : Geert Prinsen
!*********************************************************************
!*** Last update: Nov 2007
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

  use BinFile
!  use Boundary
  USE CONF_FIL
  USE CONF_ARR
  use Crop
  use DataToFlowModule
  use Files
  use Structures
  use Greenhouse
  use Link
  use RR_Meteo
  use Network
  use Unpaved
  use Balance
  use ParallelData
  use PMInterface
  use Restart
  use RRRunoff
  use globals

  !! DIO
  use dio_plt_rw, prop_file_unused => prop_file
  use DH_Alloc
  use ReadLib

! NetCdf
  use NetCdfData
  use nctimeseries
  use NetCdf, only : nf90_close

  use MessageHandling
  use m_rrerror

  use timers


!*** OTHER DATA

  IMPLICIT NONE !!!!!!!!!

  integer, save :: timerRRSleep    = 0
  integer, save :: timerRRInitialize  = 0
  integer, save :: timerRRInitializeT = 0
  integer, save :: timerRRComputeT    = 0
  integer, save :: timerRRFinalizeT   = 0
  integer, save :: timerRRRunSimult   = 0
  integer, save :: timerRRFinalize    = 0

  INTEGER       IDAYWK
  INTEGER       IMAAND, IDAG, inLang, InIni
  Integer       Iout1, iout9, Iout1LunRR
  Integer       inRain, inRunoff, inTemperature
  Integer       inEvap, inKini, inGebr, inCrf, inCrOw
  Integer       iEvent
  Integer       ActiveLanguage
  Integer(2)    iStat
  Logical       SkipBinfile, TimeMessage, ParseTokenMethod, FnmExt
  Logical       FirstCallRRInitializeEvent
  Logical       EvapFormat2

  ! like currentTimestep in OMI_CF_control
  Integer, public :: RR_ITimestep

!
  CHARACTER(len=3*CharIdLength) RRCMDLINE
  CHARACTER(FilCharIdLength) RRNamefile, BuiFil, RksFil, RestartFileName, OpenDAFileName, LGSICacheFileName

! Netcdf input meteo
  CHARACTER(FilCharIdLength) Meteo_RainfallNCFileName, Meteo_EvapNCFileName, Meteo_TemperatureNCFileName

  CHARACTER(80)  STRING, OutputString, RestartPrefix
  CHARACTER(len=32) RestartVersion
  Real           Rdum, DefaultT0OutputValue
  Integer        GenerateAanvoerAbr, OpenDAFileUnit, ICache
  Integer        RestartFileEachTimeStep, RestartFileNameEachTimeStepOption
  Logical        Update, UseOpenDAFile, ReadLGSICacheFile

  Integer        IExitCode

  Double Precision JulianStartDate, JulianEndDate, ModJulianTime
! extra variables for correct computation of days in BndFlTot.HIS file using Julian dates
  Double precision JulianDate1, JulianDate2, JulianDateTime1, JulianDateTime2, JulianCurrent, JulianDiff

  Double Precision RtmSz
  REAL          EPSCRI, EPS, eps2, epsCr2, QtotalBoundaries, RLastTm, GlobalNAMAlfa
  INTEGER       FRSTTM, LASTTM, ItmstpCheck
  Integer       teller, HisIndexRunoffOut
  Integer       iloc, iTime, timeD, timeE, timer, timerRunoff, timerTemperature, &
                times, iTmStp, BuiTmstp, EvapTmstp, RunoffTmstp, TemperatureTmstp, DayInEvent, deler
  Integer       NrDaysSinceStart, iter, DaysCounter
  Integer       tmpYear, tmpMonth, tmpDay, tmpHour, tmpMin, tmpSec
  Logical       UpdateDailyValues, NewYear

!extra variables for Modflow coupling using Process Manager
  Real          ModflowTimestep, ModflowTime, SobekTime
  Real,          pointer, save  ::  ModFlowHeads(:), ModFlowFluxes(:)
  Character(len=20), pointer, save ::  ModFlowIds  (:)

!
  CHARACTER(2)   cMAA, cDAG, cUUR, cMIN, cSEC
  LOGICAL        WARN4, Converged, ResetIterationCounter
  Integer        NrTimestepsNoConvergence

  Logical NegativeVolumeInCurrentTimestep
!
! *********************************************************************
! DIO datasets for on-line coupling (to RTC: OW_LEVEL.HIS, from RTC: RTC_3B.His)
! *********************************************************************
! DIO outgoing datasets for on-line coupling
      type(DioPltType) :: outPlt_OpenWater
! DIO incoming dataset
      type(DioPltType) :: inPlt_Rtc, InPlt_CFSF, inPlt_Salt
! DIO outgoing dataset for Runoff.Out defined in DatatoFlowModule



  contains

! ************************************************************************************
!    RRCreate
! ************************************************************************************

  Integer function RRCreate (RR_RunId,ArgsToRR)

    use wl_open_mi_support
    use rr_open_mi_support

    Implicit none
    Integer  RR_RunId

    character (Len=*), dimension(:) :: argsToRR

    RR_Runid = -1
    RRCreate = -1
    if (size(ArgsToRR) .ge. 3) then
        Call Conffil_set_OpenMIExeName (argstoRR(1))       ! exe
        Call Conffil_set_OpenMIModelName (argstoRR(2))     ! fnm file
        Call Conffil_set_OpenMIReturnName (argstoRR(3))    ! return code file
        Call Conffil_set_CommandLine ()                    ! command line voor controller
        RR_Runid = OesModelFindOrCreate(RRComponent, argstoRR(2))  ! corrected aug 2005
        if ( RR_Runid > 0) then
            RRCreate = 0
        endif
     endif

  return
  end function RRCreate

! ************************************************************************************
!    RRInitialize
! ************************************************************************************

  Integer function RRInitialize (RR_RunId, RR_Nevent)

  Implicit none
  Integer RR_RunId, RR_Nevent

  Double Precision Julian, modified_julian_fromJulian
  Integer          idum, i, LicenseReturnCode
  Integer          iSecStart, ISecDuration
  Logical          Success

! Avoid Digital Visual Fortran underflow problem on operating systems Windows 95/98:
#if (defined(HAVE_CONFIG_H))
!     Unix version: do nothing
#else
!     Pc version Win95/98: call Avoid underflow problem
  Call Avundf
#endif


  call timstrt('RRInitialize',TimerRRInitialize)
!Get date and time
  TimeMessage = .false.
  CDate = ' '
  CTime = ' '
  CALL DATE_AND_TIME (CDATE,CTIME,CZONE,time_fields)
! Write(*,'(A,A10)') ' Start date ',CDate
! Write(*,'(A,A10)') ' Start Time ',CTIME
! Write(*,'(A,8I4)') ' Values', (Values(i),i=1,8)

  SkipBinfile = .False.
  RunSimultaneous = .FALSE.
  NrDaysSinceStartFirstEvent = 0
  DaysCounter  = 0
  NrSecondsSinceStart = 0
  NrSecondsIoutAbr = 0
  NrVolumeCheck    = 0
  VolumeCheckFactorToCF = 1.0
  VolumeCheckFactorToOW = 1.0
  InitMode = .false.
  Crashed  = .false.
  IdControlModule = -1

! DO i=1,MaxBalTerms
!    Bal3B(I) = 0.0
! ENDDO
! Array/Vector initialisation
  Bal3B = 0.0

  CheckBalance  = .False.
  CaseSensitive = .True.

! write logo on screen
  if (in_f90_runner) then
     IScren = 6
#if (defined(HAVE_CONFIG_H))
     Open (Iscren,FORM='FORMATTED')
#else
     Open (Iscren,carriagecontrol='FORTRAN')
#endif
    call WriteHeader(Iscren)
  endif

! initialise buffer NewTables
!!  RRInitialize = -1
!!  Success = InitializeBuffer()
!!  if (Success) RRInitialize = 0
!!  If (RRInitialize .ne. 0) write(*,*) ' Error initialising buffer'

!*********************************************************************
!*** Start initialization. file ref
!*** Zet vaste dimensies
!*** Get command line
!*** Read control file, node file and initialize variable array dimensions
!*********************************************************************

! set files, fixed dimensions

  MakeLogFile = 0
  MessageInundation  = 0
  MessageVolumeCheck = 0
  MessagePerTimestep = 0

  CALL CONFFIL

  RRcmdline = GETFIL()

   CALL OPENFL(Iout1LunRR, ConfFil_get_NAMFIL(22),1,2)

   ! Start logging
   call SetMessageHandling(write2screen = in_f90_runner, lunMessages = Iout1LunRR, useLog = .true., &
                           callback = rrError, reset_counters = .true., thresholdLevel = LEVEL_INFO)

   call SetMessage(LEVEL_INFO, trim(rr_version_string))

   ! Check if threshold level already has been set.
   if (isMessLevelSet) then
      call SetMessageHandling(thresholdLevel = messLevelSet)
   endif

   call ConfFil_set_iOut1(Iout1LunRR)

  call NWRW_confAr0
  call Unpaved_confAr0
  call Structures_confAr0

! DIO initialisation
  Inquire (FILE = Conffil_get_Namfil(114), EXIST = FnmExt)
  If (FnmExt) then
     Call DioInit (Conffil_Get_Namfil(114))
  else
     call SetMessage(LEVEL_WARN, 'DioConfigIni file not found')
  endif
!enkele initialisaties moeten voor call RdIni
  MAXITR = 10
  NewFormatKasData = .false.
  NewFormatSoilData = .false.
  NewFormatCropFactors = .false.
! write(*,*) ' NewFormatKasData =',NewFormatKasData

!inlezen RdIni hier gezet ivm optie EmulateUnixOnPc
  CALL OpenFl(InIni, ConfFil_get_NAMFIL(1),1,1)
  call ConfFil_set_iDebug(0)

! Uit RdBinf naar voren gehaald: zet vaste dimensies
  call confar0

  CALL RR_RdIni (InIni, ConfFil_get_iDebug(), CaseSensitive, DefaultT0OutputValue, &
              RestartFileEachTimeStep, RestartFileNameEachTimeStepOption, RestartPrefix, SkipStorageCoefFromRestartFile, &
              OutputDesired, NMap, VullingsgraadMaximum100, &
              MessageInundation, MessageVolumeCheck, MessagePerTimestep,&
              IdebugFromTimestep, IdebugToTimestep,&
              Idebug2FromTimestep, Idebug2ToTimestep,&
              IdebugCapsimFromTimestep, IdebugCapsimToTimestep, MaxItr, &
              CheckBalance, SkipBinFile, OpenwaterLevelComputations, StructureOperation, &
              LowestRestartGroundwaterLevel, SkipBoundLevelFromRestartFile, DrownedWeirDepth, &
              OpenWaterPrecipitationComp, OpenwaterSeepageComp, EvaporationYear, GreenhouseYear, &
              EmulateUnixOnPc, HeaderRunoffOutAlways, &
              VolumeCheckFactorToCF, CFBoundaryConstantInTimestep, VolumeCheckFactorToOW, &
              MinimumDepthCF, StartSecDailyRainfall, TimeMessage, &
              NewFormatCropFactors, CropDefinition, OpenWaterCropDefinition, &
              NewFormatSoildata, SoilDefinition, NewFormatKasdata, KasDefinition,  &
              ParseTokenMethod, StructComp, FixARSControllerLvlCheck, &
              EstimateRemainingDuration, LargeBalanceErrorPercentage, GenerateAanvoerAbr, HisConvergence, &
              WriteRestartFileWithAdimC, ReadAdimCInRestartFile, &
              MaxNrVShapeWeirReductionPoints, NrVShapeWeirReductionPoints, &
              VShapeWeirH2H1Ratio, VShapeWeirFlowReductionFactor, OutputOwAndGwToRtc, &
              NwrwContinuous, RestartOrderStrict, FixARS13964, ReduceInfiltrationAtNegativeOpenWaterVolume,&
              TestSaveState, RestoreTimeStep, RestoreInTimeStep, ActiveLanguage, &
              AbsoluteConvergenceCheck, RelativeConvergenceCheck, AddLinkPrefix, LinkPrefix, SaltConcentrationDWF, &
              GlobalNAMAlfa, GenerateOldWagmodOutputFiles, OldPavedComputations, UseOpenDAFile, UseWalrus, WalrusZeta1, WalrusZeta2, &
              Walrus_min_deltime, Walrus_maxhchange, Walrus_minh, Walrus_max_Pstep, Walrus_Max_Substeps)

  Call CloseGP (InIni)
! write(*,*) ' after RdIni: NewFormatKasData =',NewFormatKasData
! Write(*,*) ' After RdIni, look for KasDefinition', KasDefinition(1:Len_trim(KasDefinition))

! UNST-5103  Meteo data from NetCdf - check if NetCdf file exists and switched on in Delft_3b.Ini file (see RR_RdIni above)
     Meteo_RainfallNCFileName = ConfFil_get_NamFil(125)
     Meteo_EvapNCFileName = ConfFil_get_NamFil(126)
     Meteo_TemperatureNCFileName = ConfFil_get_NamFil(127)
     If (Meteo_RainfallNCFileName .eq. '') Meteo_RainfallNCFileName = 'Meteo_Rainfall.nc'
     If (Meteo_EvapNCFileName .eq. '')      Meteo_EvapNCFileName = 'Meteo_Evap.nc'
     If (Meteo_TemperatureNCFileName .eq. '') Meteo_TemperatureNCFileName = 'Meteo_Temperature.nc'
     Inquire (FILE = Meteo_RainfallNCFileName, EXIST = FnmExt)
     If (FnmExt .and. MeteoNetCdfInput) then
         success = OpenNetCdf (RainfallNcid, Meteo_RainfallNCFileName)
         if (success) MeteoNetCdfInput = .true.
         success = OpenNetCdf (EvapNcid, Meteo_EvapNCFileName)
         success = OpenNetCdf (TemperatureNcid, Meteo_TemperatureNCFileName)
         call SetMessage(LEVEL_INFO, 'MeteoNetCdfInputFile found')
         ! TODO set number of meteo stations based on the NetCdf file
         if (RainfallNcid .gt. 0) then
         ! get rainfall Netcdf data
            success = GetNCSeriesid (RainfallNcid, 'precipitation', PrecipitationSeriesId, PrecipitationSeriesNr, PrecipitationTimeNr)
            if (.not. success) call SetMessage(LEVEL_WARN, ' Precipitation NetCdfInputFile not CF-compliant, will try using standard names time and station_id')
            success = GetNCTimeDimension (RainfallNcid, 'precipitation', PrecipitationSeriesId, PrecipitationTimesteps, PrecipitationTimeNr, PrecipitationTimeUnitString)
            success = GetNCLocationDimension (RainfallNcid, 'precipitation', PrecipitationSeriesId, PrecipitationSeriesNr, NrPrecipitationStations)
            ! get 1 data serie with values and dates
            allocate(Date_arr(1:PrecipitationTimesteps))
            allocate(Data_arr(1:PrecipitationTimesteps))
            success = GetNcTimeSeries (RainfallNcId, 'precipitation', PrecipitationSeriesId, PrecipitationSeriesNr, 1, 1, PrecipitationTimesteps, Date_Arr, Data_Arr)
            success = GetNcTimestepSize (RainfallNcId, nrSecsRai, 1, PrecipitationTimesteps, Date_Arr, PrecipitationTimeUnitString, PrecipJulianStartDate, PrecipJulianEndDate)
            call Confarr_set_nrsRai (nrSecsRai)
!           success = GetNcTimeSeriesbyId (RainfallNcId, 'precipitation', PrecipitationSeriesId, '1025', 1, PrecipitationTimesteps, Date_Arr, Data_Arr)
            deallocate(Date_arr)
            deallocate(Data_arr)
         endif
         if (EvapNcid .gt. 0) then
         ! get evaporation Netcdf data
            success = GetNCSeriesid (EvapNcid, 'evaporation', EvaporationSeriesId, EvaporationSeriesNr, EvaporationTimeNr)
            if (.not. success) call SetMessage(LEVEL_WARN, ' Evaporation NetCdfInputFile not CF-compliant, will try using standard series names time and station_id')
            success = GetNCTimeDimension (EvapNcid, 'evaporation', EvaporationSeriesId, EvaporationTimesteps, EvaporationTimeNr, EvaporationTimeUnitString)
            success = GetNCLocationDimension (EvapNcid, 'evaporation', EvaporationSeriesId, EvaporationSeriesNr, NrEvaporationStations)
            ! get 1 data serie with values and dates
            allocate(Date_arr(1:EvaporationTimesteps))
            allocate(Data_arr(1:EvaporationTimesteps))
            success = GetNcTimeSeries (EvapNcId, 'evaporation', EvaporationSeriesId, EvaporationSeriesNr, 1, 1, EvaporationTimesteps, Date_Arr, Data_Arr)
            success = GetNcTimestepSize (EvapNcId, nrSecsEvap, 1, EvaporationTimesteps, Date_Arr, EvaporationTimeUnitString, EvapJulianStartDate, EvapJulianEndDate)
            call Confarr_set_nrsEvap (nrSecsEvap)
!           success = GetNcTimeSeriesbyId (EvapNcId, 'evaporation', EvaporationSeriesId, '1025', 1, EvaporationTimesteps, Date_Arr, Data_Arr)
            deallocate(Date_arr)
            deallocate(Data_arr)
         endif
         NcMet = max (NrPrecipitationStations, NrEvaporationStations)
         if (TemperatureNcid .gt. 0) then
         ! get temperature Netcdf data
            success = GetNCSeriesid (TemperatureNcid, 'temperature', TemperatureSeriesId, TemperatureSeriesNr, TemperatureTimeNr)
            if (.not. success) call SetMessage(LEVEL_WARN, ' Temperature NetCdfInputFile not CF-compliant, will try using standard names time and station_id')
            success = GetNCTimeDimension (TemperatureNcid, 'temperature', TemperatureSeriesId, TemperatureTimesteps, TemperatureTimeNr, TemperatureTimeUnitString)
            success = GetNCLocationDimension (TemperatureNcid, 'temperature', TemperatureSeriesId, TemperatureSeriesNr, NrTemperatureStations)
            ! get 1 data serie with values and dates
            allocate(Date_arr(1:TemperatureTimesteps))
            allocate(Data_arr(1:TemperatureTimesteps))
            success = GetNcTimeSeries (TemperatureNcId, 'temperature', TemperatureSeriesId, TemperatureSeriesNr, 1, 1, TemperatureTimesteps, Date_Arr, Data_Arr)
            success = GetNcTimestepSize (TemperatureNcId, nrSecsTemperature, 1, TemperatureTimesteps, Date_Arr, TemperatureTimeUnitString, TemperatureJulianStartDate, TemperatureJulianEndDate)
            call Confarr_set_nrsTemperature (nrSecsTemperature)
!           success = GetNcTimeSeriesbyId (TemperatureNcId, 'temperature', TemperatureSeriesId, '1025', 1, TemperatureTimesteps, Date_Arr, Data_Arr)
            deallocate(Date_arr)
            deallocate(Data_arr)
         endif

     Else
         MeteoNetCdfInput = .false.
         call SetMessage(LEVEL_WARN, 'MeteoNetCdfInputFile not found, will read meteo data from ASCII rainfall file')
     Endif
! end UNST-5103

! ARS 16442: Uit RdBinf naar voren gehaald: lees overige control data, en initialiseer controller simultaan draaien
  Call Openfl (InIni, ConfFil_get_namFil(1),1,1)   !Delft_3B.Ini file
  Call Network_ReadAscii (InIni, MeteoNetCdfInput)
  Call CloseGP (InIni)

! check NetCdfTimestep
  If (trim(NetCdfTimestep) .eq. 'Seconds')  then
     If (TimeSettings%Timestepsize .ge. 60) then
       NetCdfTimestep = 'Minutes'
     Elseif (TimeSettings%Timestepsize .ge. 3600) then
       NetCdfTimestep = 'Hours'
     Elseif (TimeSettings%Timestepsize .ge. 86400) then
       NetCdfTimestep = 'Days'
     endif
  ElseIf (trim(NetCdfTimestep) .eq. 'Minutes')  then
     if (TimeSettings%Timestepsize .ge. 3600) then
       NetCdfTimestep = 'Hours'
     Elseif (TimeSettings%Timestepsize .ge. 86400) then
       NetCdfTimestep = 'Days'
     endif
  ElseIf (trim(NetCdfTimestep) .eq. 'Hours')  then
     if (TimeSettings%Timestepsize .ge. 86400) then
       NetCdfTimestep = 'Days'
     endif
  Endif

!*********************************************************************
!***  Initialiseer controller voor simultaan draaien met CF/SF e.a.
!*********************************************************************

! UNST-4751 test
  IF (RunSimultaneous .and. .not. dimr_mode) THEN
     CALL INITCT (RRCMDLINE(1:Len_Trim(RRCMDLINE)), IDCNT, ISTAT)
     IF (ISTAT .LT. 0)  THEN
          WRITE (STRING,'(I3)') ISTAT
          call ErrMsgStandard (990, 0, 'Sobek_RR', STRING)
     ENDIF
     IdControlModule = IdCnt
     InitMode = .true.
     FirstProc = .true.  ! true als alleen met RTC, maar met CF False? check
     CALL INITFP (FirstProc, InitMode)
  ENDIF
! End ARS 16442

!  If (TestSaveState) Call RRStatesCreate('RRStates')
! ARS 11610, 15474
  UnpVolumeCheckFactorToOW = VolumeCheckFactorToOW
  UnpVolumeCheckFactorToCF = VolumeCheckFactorToCF
  FixArs11610Struct = FixArs11610

!    If NWRW continuous switch on, match meteo station names to nodes using the continuous Rainfall BUI file
     If (NwrwContinuous) then
        RKSFIL = ConfFil_get_NAMFIL(13)
        BUIFIL = ConfFil_get_NAMFIL(115)
        Call Conffil_set_Namfil(13,Rksfil)
     Endif

#if (defined(SOBEK_PARALLEL))
!     Unix version
!     SOMMIGE filenamen aanpassen door uitgang _0000 er achter te plakken
      Call SetFileNames ( 0 )
      EstimateRemainingDuration = .false.
#else
!     Pc version: do nothing
      if (EmulateUnixOnPC) then
! test UX versie
         Call SetFileNames ( 0 )
      endif
#endif

  STRFIL = ConfFil_get_NAMFIL(77)
  MSFIL  = ConfFil_get_NAMFIL(78)
  MSFIL2 = ConfFil_get_NAMFIL(79)

   ! altijd zorgen dat de debug file open staat
   idebug = 0
   Call OpenFl (IDebugLunRR,ConfFil_get_NAMFIL(32),1,2)
   call ConfFil_set_iDebug(0)

! language file
  CALL OPENFL(INLANG, ConfFil_get_NAMFIL(82),1,1)
  Call LanguagesCreate
  LanguageHandle = LanguagesModelFindOrCreate(0)
  CALL ReadLanguageFile (LanguageHandle, INLANG, ConfFil_get_iDebug(),Conffil_get_IOUT1(),iExitCode)
  If (IExitCode .eq. 981) then
      call ErrMsgStandard (981, 0, ' Error allocating arrays in subroutine ', ' ReadLanguage ')
  ElseIf (IExitCode .eq. 972) then
      call ErrMsgStandard(972, 0,'Error in language file',' ')
  Endif
  Call CloseGP (INLANG)
  INLANG = 0
  Call SetActiveLanguage(LanguageHandle, ActiveLanguage)

! Input checks INI file should be done after reading language file
   if (InitCapsimOption .eq. 0) then
     call ErrMsgStandard (974, 0, ' Initialisation option Unsaturated Zone not specified in Settings ', 'Sobek_RR')
   endif
! check VShapeWeirRelation
   Do idum=1,NrVShapeWeirReductionPoints-1
      if (VShapeWeirH2H1Ratio(idum) .ge. VShapeWeirH2H1Ratio(idum+1)) then
          call ErrMsgStandard (972, 0, 'VShapeWeirH1H1Ratio should be specified in increasing order', 'Sobek_RR')
      endif
   Enddo


! set node, meteostation, kasklasse,cropfactor dimensions
  BINFIL = ConfFil_get_NAMFIL(44)

  IOUT1 = ConfFil_get_IOUT1()
  call SetMessage(LEVEL_INFO, 'Following dimensions have been set:')

  call ConfFil_set_iDebug(0)
! Read input from Binary file or from ASCII files
  String = ' SkipBinfile should be -1'
  if (.not. SkipBinFile) call ErrMsgStandard (905, 0, 'Sobek_RR', String)

  If (TimeMessage) then
    CALL DATE_AND_TIME (CDATE,CTIME,CZONE,time_fields)
    Write(*,'(A,A10)') ' Date/Time before reading input files RdBinf'
    Write(*,'(A,8I4)') ' Values', (time_fields(i),i=1,8)
  Endif
  Call RdBinf(SkipBinfile, ParseTokenMethod, MeteoNetCdfInput)
  If (TimeMessage) then
    CALL DATE_AND_TIME (CDATE,CTIME,CZONE,time_fields)
    Write(*,'(A,A10)') ' Date/Time after reading input files RdBinf'
    Write(*,'(A,8I4)') ' Values', (time_fields(i),i=1,8)
  Endif

  If (CleanRRFiles) then
     call SetMessage(LEVEL_INFO, 'End of CleanRRFiles')
     stop
  endif

! ARS 10903 : RR output selection
  RRNamefile = ''
  RRNamefile = Conffil_get_Namfil(1)
  CALL ReadRROutputOptions (RRNameFile, MaxSeriesPerMap, NMAP)
  if (UnsatZoneOption .eq. 0) then
     OutputUnpaved(20) = 0
     OutputUnpaved(21) = 0
     OutputUnpaved(22) = 0
  endif
  RRNamefile = ''

! Consistentiechecks netwerk.
  Call CheckNetwork

! Check license
! ARS 12158/12161
  LicenseReturnCode = 0
!  write(*,*) ' ****************************************************** '
!  write(*,*) ' ******           GP temp noliccheck             ****** '
!  write(*,*) ' ****************************************************** '

! 12april2016 - switched license check off
!! Call RR_CheckLicense (NcNode, NcPluv, LicenseReturnCode, FlexlmErrorMessage)

  idebug = 0
  call ConfFil_set_IDEBUG(IdebugLunRR)
  Idebug = ConfFil_get_IDEBUG()

!*********************************************************************
!*** Initialization of file ref. and time conversion variables
!*********************************************************************

  CHKSTU= .FALSE.

  IF (IFLZT .GT. 0)  THEN
     CALL OpenFl (IflZt, ConfFil_get_NAMFIL(47),1,2)
  ENDIF

! Rekening houden met bergend oppervlak verhard/onverhard bij openwater peilberekening?

! Altijd call DetermineExtraStorageArea, om RainArea per open water node te bepalen
!    en ook bepaling Totale relatie peil - volume, inclusief ow en bergend opp. op land
  Call DetermineExtraStorageArea(iout1,Idebug)
! ARS 12349: initialise ow-area for seepage computations also on RainArea
  SeepageArea = RainArea


!*********************************************************************
!*** Open hydrological files and spool to first data record
!***   13: rainfall
!***   14: evaporation: net als
!***   15: kwel       : voorlopig constant in de tijd, variatie in ruimte
!***   16: wegzijging : idem
!***   17: initiele berging kasklassen volgens Staring centrum
!***   18: verbruik water kassen volgens Staring centrum
!***   19: crop factoren gewassen
!***   21: initialisatie grondwaterstand/zomerberging
!***       (wordt wel gelezen, echter door INITBC overruled)
!***   38: waterstanden randknopen (via Sobek)
!***   40: crop factoren open water
!***   79: temperature file
!***   80: external runoff file
!*********************************************************************

! Read Headers of Rainfall, evaporation file
! Read number of stations
! Read number of events NEvent
! Read rainfall timestep size
! Go to first data records of other fixed files
! Also Set delta timestep Idh, Idm, Ids

     Call InitFixedFilesAndSetTimestep  ( InRain, InRunoff, InTemperature, Inevap, NrEvapStations, &
                                          Inkini, Ingebr, Incrf,  InCrow,  InSbk, &
                                          IDH, IDM, IDS, EvapFormat2, MeteoNetcdfInput)

! UNST-5103
!     if (MeteoNetCdfInput) then
!        if (RainfallNcid .gt. 0) then
!
!        endif
!        ! TODO  Read meteo NetCdf Timeseries file, Rainfall, Evaporation and Temperature time series
!     endif

! Set dimensions output arrays, including EventStartDateTime and EventDuration
     Call Output_Confar
     Call ConfarFixedfiles1

! Read fixed files and dimension arrays
! Rainfall: NEvent events, max. MaxTimesteps timesteps
! Evaporation: NEvent events, max. MaxDays daily values per event (from EVP file) or MaxTimestepsEvap when read from NetCdf (maybe not daily, and not equal to precipitation timestep)
! KasInit: NEvent events, max. 1 set of daily values per event
! KasGebr: NEvent events, max. MaxDays daily values per event
! CropFactors: 1 year only, independent of event
     call ConfFil_set_IDEBUG(0)      !debug uitzetten
     Idebug = 0
     If (NcPluv .ne. NcNode) call SetMessage(LEVEL_INFO, 'Read Meteo files')
!    ARS xxxxx: optie om inloop ook continu uit te rekenen (crash actie Grontmij dec 2002)
!               adjustment for NwrwContinuous
     Success = Dh_AllocInit (Nevent,7, OutputEventStartDateTime, 0)
     Success = Success .and. Dh_AllocInit (Nevent,6, OutputEventDuration, 0)

! UNST-5103
     If (MeteoNetCdfInput) then
        ! Meteo data will be read from NetCdf file
          MaxTimesteps     = PrecipitationTimesteps
          MaxTimestepsEvap = EvaporationTimesteps
          MaxTimestepsTemperature = TemperatureTimesteps
        ! MaxTimestepsEvap = MaxDays
        ! MaxTimestepsRunoff =

        !   Set EventStartDateTime, EventEndDateTime etc  (1 event only)

          Call GregorSbk (PrecipJulianStartDate, tmpYear, tmpMonth, tmpDay, tmpHour, tmpMin, tmpSec)
          EventStartDateTime(1,1) = tmpyear
          EventStartDateTime(1,2) = tmpmonth
          EventStartDateTime(1,3) = tmpday
          EventStartDateTime(1,4) = tmphour
          EventStartDateTime(1,5) = tmpmin
          EventStartDateTime(1,6) = tmpsec
          EventDuration = 0
          EventDuration (1,4) = PrecipitationTimesteps * NrSecsRai
          RDum                = Float(PrecipitationTimesteps) * Float(NrSecsRai) / Float(NrsDay)
          EventDuration (1,5) = Floor (RDum)
          EventDuration (1,6) = PrecipitationTimesteps + 1

          Ievent = 1
          If (EventDuration(Ievent,5) .lt. Rdum) EventDuration(Ievent,5) = EventDuration(Ievent,5) + 1
          ISecStart = EventStartDateTime(Ievent,4)*3600 + EventStartDateTime(Ievent,5)*60 + EventStartDateTime(Ievent,6)
          ISecDuration = PrecipitationTimesteps * NrSecsRai - (Floor(Rdum)*NrsDay)
          If (ISecStart+ISecDuration .gt. NrsDay) then
!            event duration is not a whole nr. of days, so add 1 additional daily evaporation value is needed
             EventDuration(Ievent,5) = EventDuration(Ievent,5) + 1
          Elseif (EventStartDateTime(ievent,4) .ne. 0 .and. EventStartDateTime(ievent,4) .ne. 24 .and. EventDuration(ievent,5) .eq. RDum) then
!            event lastst whole number of days, but does not start at midnight
             EventDuration(Ievent,5) = EventDuration(Ievent,5) + 1
          Endif
          if (NcRRRunoffHBV .gt. 0 .and. TemperatureNcid .gt. 0) then
              Call GregorSbk (TemperatureJulianStartDate, tmpYear, tmpMonth, tmpDay, tmpHour, tmpMin, tmpSec)
              EventStartDateTimeTemperature(1,1) = tmpyear
              EventStartDateTimeTemperature(1,2) = tmpmonth
              EventStartDateTimeTemperature(1,3) = tmpday
              EventStartDateTimeTemperature(1,4) = tmphour
              EventStartDateTimeTemperature(1,5) = tmpmin
              EventStartDateTimeTemperature(1,6) = tmpsec
              EventDurationTemperature = 0
              EventDurationTemperature (1,4) = TemperatureTimesteps * NrSecsTemperature
              RDum                = Float(TemperatureTimesteps) * Float(NrSecsTemperature) / Float(NrsDay)
              EventDurationTemperature (1,5) = Floor (RDum)
              EventDurationTemperature (1,6) = TemperatureTimesteps + 1
             If (EventDurationTemperature(Ievent,5) .lt. Rdum) EventDurationTemperature(Ievent,5) = EventDurationTemperature(Ievent,5) + 1
             ISecStart = EventStartDateTimeTemperature(Ievent,4)*3600 + EventStartDateTimeTemperature(Ievent,5)*60 + EventStartDateTimeTemperature(Ievent,6)
             ISecDuration = TemperatureTimesteps * NrSecsTemperature - (Floor(Rdum)*NrsDay)
             If (ISecStart+ISecDuration .gt. NrsDay) then
!               event duration is not a whole nr. of days, so add 1 additional daily value is needed
                EventDurationTemperature(Ievent,5) = EventDurationTemperature(Ievent,5) + 1
             Elseif (EventStartDateTimeTemperature(ievent,4) .ne. 0 .and. EventStartDateTimeTemperature(ievent,4) .ne. 24 .and. EventDurationTemperature(ievent,5) .eq. RDum) then
!               event lastst whole number of days, but does not start at midnight
                EventDurationTemperature(Ievent,5) = EventDurationTemperature(Ievent,5) + 1
             Endif
          endif
          MaxDays      = Max (EventDuration(Ievent,5), MaxDays     )
          MaxTimesteps = Max (EventDuration(Ievent,6), MaxTimesteps)
          MaxTimestepsEvap = MaxTimestepsEvap + 1
          MaxTimestepsTemperature = TemperatureTimesteps + 1
          NCStat = max (NrPrecipitationStations, NrEvaporationStations)
          NCStatTemperature = NrTemperatureStations
          NeventTemperature = Nevent
          NeventRunoff      = Nevent
     endif

     If (NwrwContinuous .and. .not. MeteoNetCdfInput) then   ! NWRWContinous only with old ASCII input (RKS)
!       Read data from RKS file and save output periods
        Deallocate(EventStartDateTime, EventDuration)
        Call ConfarFixedfiles1
        Call Conffil_set_Namfil(13,Rksfil)
        CALL OPENFL(INRAIN, ConfFil_get_NAMFIL(13),1,1)
        Call ReadRainfallFile  (Idebug, InRain, Iout1, 1)  ! also closes the file
        Call DetermEventStartHisIdxROffOut (Idebug)
        if (NcRRRunoffExternal .gt. 0) then
           CALL OPENFL(INRunoff, ConfFil_get_NAMFIL(80),1,1)
           Call ReadRunoffFile  (Idebug, InRunoff, Iout1, 1)  ! also closes the file
        endif
        if (NcRRRunoffHBV .gt. 0) then
           CALL OPENFL(INTemperature, ConfFil_get_NAMFIL(79),1,1)
           Call ReadTemperatureFile  (Idebug, InTemperature, Iout1, 1)  ! also closes the file
        endif
!       save data
        NrOutputPeriods = Nevent
        NrsRaiRks = ConfArr_get_nrSRai()
!       allocation moved to just before, since array is used in call WrHdrRunoffOut while it is possibly not yet allocated
!       Success = Dh_AllocInit (Nevent,7, OutputEventStartDateTime, 0)
!       Success = Success .and. Dh_AllocInit (Nevent,6, OutputEventDuration, 0)
        If (.not. success .or. IEvent .ne. 0) &
           call ErrMsgStandard (981, IEvent, ' Error allocating arrays in subroutine ', ' Sobek_RR' )
        OutputEventStartDateTime = EventStartDateTime
        OutputEventDuration = EventDuration
!       switch to Rainfall BUI file
        Call Conffil_set_Namfil(13,Buifil)
        CALL OPENFL(INRAIN, ConfFil_get_NAMFIL(13),1,1)
        Call ReadRainfallFile  (Idebug, InRain, Iout1, 1)  ! also closes the file
        if (NcRRRunoffExternal .gt. 0) then
           CALL OPENFL(INRunoff, ConfFil_get_NAMFIL(80),1,1)
           Call ReadRunoffFile  (Idebug, InRunoff, Iout1, 1)  ! also closes the file
        endif
        if (NcRRRunoffHBV .gt. 0) then
           CALL OPENFL(INTemperature, ConfFil_get_NAMFIL(79),1,1)
           Call ReadTemperatureFile  (Idebug, InTemperature, Iout1, 1)  ! also closes the file
        endif
!       Check that - RKS periods fall within Bui period
!       Check beginning of first event
        IDateAct   = EventStartDateTime(1,1)*10000 + EventStartDateTime(1,2)* 100 + EventStartDateTime(1,3)
        ITimeAct   = EventStartDateTime(1,4)*10000 + EventStartDateTime(1,5)* 100 + EventStartDateTime(1,6)
        JulianStartDate = Julian (IDateAct, ITimeAct)
        IDateAct   = OutputEventStartDateTime(1,1)*10000 + OutputEventStartDateTime(1,2)* 100 + &
                                                                           OutputEventStartDateTime(1,3)
        ITimeAct   = OutputEventStartDateTime(1,4)*10000 + OutputEventStartDateTime(1,5)* 100 + &
                                                                           OutputEventStartDateTime(1,6)
        JulianStartOutputDate = Julian (IDateAct, ITimeAct)
        if (JulianStartDate .gt. JulianStartOutputDate) then
            call ErrMsgStandard (972, 0, 'Rks file starts at earlier date than Continuous bui file', 'Sobek_RR')
        Endif
!       Check end of last event
        IDateAct   = EventStartDateTime(1,1)*10000 + EventStartDateTime(1,2)* 100 + EventStartDateTime(1,3)
        ITimeAct   = EventStartDateTime(1,4)*10000 + EventStartDateTime(1,5)* 100 + EventStartDateTime(1,6)
        TimeInEvent  = Dble ((EventDuration(1,6)-1)) * Dble(NRSecsRai) / Dble (NrsDay)
        JulianEndDate = Julian (IDateAct, ITimeAct) + Int(TimeInEvent)
        IDateAct   = OutputEventStartDateTime(NrOutputPeriods,1)*10000 + &
                        OutputEventStartDateTime(NrOutputPeriods,2)* 100 + &
                          OutputEventStartDateTime(NrOutputPeriods,3)
        ITimeAct   = OutputEventStartDateTime(NrOutputPeriods,4)*10000 + &
                        OutputEventStartDateTime(NrOutputPeriods,5)* 100 + &
                          OutputEventStartDateTime(NrOutputPeriods,6)
        TimeInEvent  = Dble ((OutputEventDuration(NrOutputPeriods,6)-1)) * Dble(NRSecsRai) / Dble (NrsDay)
        JulianEndOutputDate = Julian(IDateAct,ITimeAct) + Int(TimeInEvent)
        if (JulianEndDate .lt. JulianEndOutputDate) then
            call ErrMsgStandard (972, 0, 'Rks file ends at later date than Continuous bui file', 'Sobek_RR')
        Endif
!       Check that RKS and Bui timestep is the same
        if (NrsRaiRks .ne. ConfArr_get_nrSRai()) then
            call ErrMsgStandard (972, 0, 'Rks file and Continuous bui file do not have the same timestep', 'Sobek_RR')
        Endif
!       No check that actual data for overlapping periods is the same !!!!
     elseif (.not. NwrwContinuous) then
        if (RainfallNcid .le. 0) then  ! rainfall ASCII file
           CALL OPENFL(INRAIN, ConfFil_get_NAMFIL(13),1,1)
           Call ReadRainfallFile  (Idebug, InRain, Iout1, 1)  ! also closes the file
        endif
        if (NcRRRunoffExternal .gt. 0) then
           CALL OPENFL(INRunoff, ConfFil_get_NAMFIL(80),1,1)
           Call ReadRunoffFile  (Idebug, InRunoff, Iout1, 1)  ! also closes the file
        endif
        if (TemperatureNcid .le. 0 .and. NcRRRunoffHBV .gt. 0) then
           CALL OPENFL(INTemperature, ConfFil_get_NAMFIL(79),1,1)
           Call ReadTemperatureFile  (Idebug, InTemperature, Iout1, 1)  ! also closes the file
        endif
!       ARS xxxxx: end adjustment for NwrwContinuous
     Endif
!    end UNST-5103

!    Set dimensions for BUIDATA etc
     Call ConfarFixedfiles2 (NcStat, NcStatRunoff, NcStatTemperature, NcKkl)

!    UNST-5103
     if (MeteoNetCdfInput) then
         if (RainfallNcid .gt. 0) then
            ! Read Netcdf time series and put in BuiData array
            call SetMessage(LEVEL_INFO, ' Reading precipitation from NetCdf input file')
            write(iout1,*) ' Reading precipitation from Netcdf input file'
            write(iout1,*) '  nr stations = ', NrPrecipitationStations
            write(iout1,*) '  nr timesteps = ', PrecipitationTimesteps
            allocate(Date_arr(1:PrecipitationTimesteps))
            allocate(Data_arr(1:PrecipitationTimesteps))
            do iloc=1,NrPrecipitationStations
               success = GetNcTimeSeries (RainfallNcId, 'precipitation', PrecipitationSeriesId, PrecipitationSeriesNr, iloc, 1, PrecipitationTimesteps, Date_Arr, Data_Arr)
               do itime=1,PrecipitationTimesteps
                  Buidata (1,iloc,itime) = Data_arr(itime)
               enddo
            enddo
            deallocate(Date_arr)
            deallocate(Data_arr)
            ! set additional items, check ReadRainfallFile: EventDuration, .....

         endif
         if (EvapNcid .gt. 0) then
            ! Read Netcdf time series and put in EvapData array
            call SetMessage(LEVEL_INFO, ' Reading evaporation from NetCdf input file')
            write(iout1,*) ' Reading evaporation from Netcdf input file'
            write(iout1,*) '  nr stations = ', NrEvaporationStations
            write(iout1,*) '  nr timesteps = ', EvaporationTimesteps
            allocate(Date_arr(1:EvaporationTimesteps))
            allocate(Data_arr(1:EvaporationTimesteps))
            do iloc=1,NrEvaporationStations
               success = GetNcTimeSeries (EvapNcId, 'evaporation', EvaporationSeriesId, EvaporationSeriesNr, 1, 1, EvaporationTimesteps, Date_Arr, Data_Arr)
               do itime=1,EvaporationTimesteps
                  Evapdata (1,iloc,itime) = Data_arr(itime)
               enddo
            enddo
            deallocate(Date_arr)
            deallocate(Data_arr)
            ! set additional items, check ReadEvapFile .....

         endif
         if (TemperatureNcid .gt. 0 .and. NcRRRunoffHBV .gt. 0) then
            ! Read Netcdf time series and put in TemperatureData array
            call SetMessage(LEVEL_INFO, ' Reading temperature from NetCdf input file')
            write(iout1,*) ' Reading temperature from Netcdf input file'
            write(iout1,*) '  nr stations = ', NrTemperatureStations
            write(iout1,*) '  nr timesteps = ', TemperatureTimesteps
            allocate(Date_arr(1:TemperatureTimesteps))
            allocate(Data_arr(1:TemperatureTimesteps))
            do iloc=1,NrTemperatureStations
               success = GetNcTimeSeries (TemperatureNcId, 'temperature', TemperatureSeriesId, TemperatureSeriesNr, 1, 1, TemperatureTimesteps, Date_Arr, Data_Arr)
               do itime=1,TemperatureTimesteps
                  Temperaturedata (1,iloc,itime) = Data_arr(itime)
               enddo
            enddo
            deallocate(Date_arr)
            deallocate(Data_arr)
            ! set additional items, check ReadEvapFile .....

         endif
     else  ! read BUI/RKS file
        CALL OPENFL(INRAIN, ConfFil_get_NAMFIL(13),1,1)
        Call ReadRainfallFile  (Idebug, InRain, Iout1, 2)  ! also closes the file
        if (NcRRRunoffHBV .gt. 0) then
           CALL OPENFL(INTemperature, ConfFil_get_NAMFIL(79),1,1)
           Call ReadTemperatureFile  (Idebug, InTemperature, Iout1, 2)  ! also closes the file
        endif
     endif
!    end UNST-5103

     if (NcRRRunoffExternal .gt. 0) then
        CALL OPENFL(INRunoff, ConfFil_get_NAMFIL(80),1,1)
        Call ReadRunoffFile  (Idebug, InRunoff, Iout1, 2)  ! also closes the file
     endif
     Call SelectedSubsetRainfallData (Idebug, iout1)
     if (NcRRRunoffExternal .gt. 0) Call SelectedSubsetRunoffData (Idebug)
     if (NcRRRunoffHBV .gt. 0) Call SelectedSubsetTemperatureData (Idebug)
     Call VerifyOutput2CFUserDefPeriod (Idebug)
     if (RunoffOutHis .ne. 0) Call DetermEventStartHisIdxROffOut (Idebug)
! UNST-5103
     if (.not. MeteoNetCdfInput) Call ReadEvapFile (Inevap, nrEvapStations, EvapFormat2)
! end UNST-5103
     if (NcKas .gt. 0) then
       call SetMessage(LEVEL_INFO, 'Read Greenhouse initialisation and water use files')
       Call ReadKasInit (InKini)
       Call ReadKasgebruik (Ingebr)
     endif
     Do i=1,6
        EvStrt(i) = EventStartDateTime (1,i)
     Enddo
     If (Ncovhg + NcOw + NcCell .gt. 0) call SetMessage(LEVEL_INFO, 'Read Crop factor data')
     IF (NCOVHG + NcCell .GT. 0) Call ReadCropFactorFile (INCRF, iDebug, iDefLt)
     IF (NcNode .ne. NcPluv) Call ReadOpenWaterCropFactorFile(INCROW, iDefLt)
     call ConfFil_set_IDEBUG(0)
     Idebug = 0
! ARS 16550: check that events are chronological
     if (NEvent .gt. 1) call VerifyRainfallData
!    Check that Runoff data covers whole simulation period
     if (NcRRRunoff .gt. 0) call VerifyRainfallAndRunoffAndTemperatureData

! HarmonIT, make sure JulianStartDate is always computed
     IDateAct   = EventStartDateTime(1,1)*10000 + EventStartDateTime(1,2)* 100 + EventStartDateTime(1,3)
     ITimeAct   = EventStartDateTime(1,4)*10000 + EventStartDateTime(1,5)* 100 + EventStartDateTime(1,6)
     JulianStartDate = Julian (IDateAct, ITimeAct)
     ModJulianTime = Modified_Julian_fromJulian(JulianStartDate)
!
!*********************************************************************
!*** Schrijf headers van uitvoer (indien gewenst)
!*** IOUTPL          (37)    - rioolinloop Pluvius knopen en debieten randknopen
!*** IOUTCB          (46)    - concentraties randknopen
!*** IoutAbrTot      (45)    - totaal debiet per tijdstap naar rand, voor ABR Rijnland.
!*********************************************************************


     if (RunoffOutHis .eq. 0) then
         Call WrHdr (IOUTPL, IOUTCB)
     else
         Call WrHdrCbFile (IOUTCB)
     endif

     IoutAbrTot = 0
     IF (ConfFil_get_NAMFIL(45) .NE. ' ' .AND. NCBOUN .GT. 0 .and.&
           GenerateAanvoerAbr .ne. 0) then
        Call OpenFl (IoutAbrTot,ConfFil_get_NAMFIL(45),1,2)
        Write(IoutAbrTot,'(A,A9,A,2A10)') '*Sobek-RR version ',VersionNumberBinfile,' Run datum/tijd ',Cdate,CTime
     Endif

!*********************************************************************
!*** Determine Mappix locations  (RDLOCA)
!*** Initialize Mappix data and DIR files (INITDT)
!*** Write TST file for events
!*********************************************************************

      CALL RDLOCA

! if no salt computations, no locations on salt map needed.
      IF (ISLCMP .EQ. 0)  NLCMAP (9) = 0

! Initialize HIS output files, names of series etc.
      CALL INITDT (GenerateNetCdfOutput)
      Call InitDtLong      ! to initialise LongDSRMAP array

      Call RROpenWater_IndexEvapPrecip

        Do i=1,6
           EvStrt(i) = EventStartDateTime (1,i)
        Enddo
        call ConfArr_set_IYEAR(EVSTRT(1))
        call ConfArr_set_iMonth(EVSTRT(2))
        call ConfArr_set_iDay(EVSTRT(3))
        call ConfArr_set_iHour(EVSTRT(4))
        call ConfArr_set_iMinute(EVSTRT(5))
        call ConfArr_set_iSecond(EVSTRT(6))

! vaste HIS file met arealen; bepaal ook Bal3B(1); fixed area NWRW nodes

      TotalArea = 0.0
      if (NcNode .gt. 0) Call WriteFixedAreaNodes (TotalArea)
      Bal3B(1) = TotalArea

      If (NcPluv .gt. 0) Call WriteFixedAreaNWRW

      IF (NCSTRU .GT. 0)  CALL WriteFixedStructuredata
!*********************************************************************
!*** Globale Initialisaties
!***   o.a conversie kwel/wegzijging naar standaardeenheden (m/s)
!***  Zet bergingscoeff. op waarde corr. met 15 juni, 00:00 uur
!***  (IMO=6,IDAY=15)
!*********************************************************************


!aug 95: Bij simulatie reeks: dan geen detail uitvoer
!aug 95: Bij simulatie gebeurtenis: geen maxima uitvoer
      IF (NEVENT .GT. 1) THEN
         IOPT1(1) = 0
         IOPT1(2) = 1
! nav probleem Robin van Assem: bij reeks altijd maxima, ongeacht optie in INI file
         if (Iopt1(3) .ne. 1) OutputAtTimestepOption = 3
      ELSEIF (NEVENT .EQ. 1) THEN
         IOPT1(1) = 1
         IOPT1(2) = 0
         IOPT1(3) = 0     ! if Nevent=1, then option OutputSeriesFullDetail is irrelevant
      ENDIF
! June 2004: if option OutputSeriesFullDetail=-1
      If (IOpt1(3) .eq. 1) then
         IOPT1(1) = 1
         IOPT1(2) = 0
      Endif
      If (Nevent .eq. 1) Iopt1(3) = 1

! zet kwel, wegzijging per onverhard gebied en open water in SI eenheid
!     DO I=1,NCOVHG
!        KWEL (I) = KWEL(I) * MM2M / NRSDAY
!        WEGZG(I) = WEGZG(I) * MM2M / NRSDAY
!     ENDDO
!     DO I=1,NCOW
!        OWKWEL (I) = OWKWEL(I) * MM2M / NRSDAY
!        OWWEGZ (I) = OWWEGZ(I) * MM2M / NRSDAY
!     ENDDO
! zet kwel, wegzijging per onverhard gebied en open water in SI eenheid using Vector/Array manipulations
      KWEL  = KWEL * MM2M / NRSDAY
      WEGZG = WEGZG * MM2M / NRSDAY
      OWKWEL  = OWKWEL * MM2M / NRSDAY
      OWWEGZ  = OWWEGZ * MM2M / NRSDAY

! peilen op randen via Sobek?
      IF (INSBK /= 0) THEN
        !Nov 96: variabel peil volgens HIS file, on-line gegenereerd
        INSHIS = 1
        INSBK = ConfFil_get_INXFIL(38)
      else
        INSHIS = 0
        ! peil en initiele zoutconcentratie volgens tabel
        call rdSbk()
      ENDIF
! GP 30 okt 1997: altijd via Tabel Rdsbk
      call rdSbk()


!     Unpaved_Initbc voor call WrData (uitvoer tabellen) en Call InitCapsim
!     if Sobek-Capsim mode read additional tables from Staring Centrum
      IMAAND = ConfArr_get_iMonth()
      IDAG = ConfArr_get_iDay()
      call ConfFil_set_IDEBUG(IdebugLunRR)
      CALL Unpaved_INITBC(IMAAND, IDAG)
 !     CALL Cel_INITBC(IMAAND, IDAG)

      If (UnSatZoneOption .ge. 1) then
         call SetMessage(LEVEL_INFO, 'Read Capsim data')
         Call InitCapsim (IdebugCapsimFromTimestep, IdebugCapsimToTimestep)
      Endif


! *********************************************************************
! *** Always directly write ASCII+HIS output from 3B (RESTIO(3)=0)
! *********************************************************************
! Get Directory names voor HIS files
      Call GetFil2(ConfFil_get_iDebug() )
      call ConfFil_set_iDebug(0)

! zet Iout2, tm Iout8, Iout9 in sub Wrdata, open files en schrijf fixed data
      Call WrData (Iout9, iout2,iout3,iout4,iout5,iout6,iout7,iout8)

      IBar0 = 0
      RemTime = '000:01:00'
      CDate = ' '
      CTime = ' '
      CALL DATE_AND_TIME (CDATE,CTIME,CZONE,time_fields)
      IDateAct   = time_fields(1)*10000 + time_fields(2) * 100 + time_fields(3)
      ITimeAct   = time_fields(5)*10000 + time_fields(6) * 100 + time_fields(7)
      JulianStart = Julian (IDateAct, ITimeAct)
      IF (NEVENT .GT. 1) then
         TotalNrTimesteps = 0
         Do IEvent=1,Nevent
            RDum = Float(EventDuration(Ievent,1))*NRSDAY + &
                    Float(EventDuration(Ievent,2))*NRSHR + &
                     Float(EventDuration(Ievent,3))*NRSMIN + &
                      Float(EventDuration(Ievent,4))
            Rdum = RDum / timeSettings%timestepSize
            TotalNrTimesteps = TotalNrTimesteps + Rdum
         Enddo
         CALL WRLOGO (IScren, 0,NEVENT,NEVENT,IBar0, EstimateRemainingDuration,RemTime)
      Endif

! open balance file and write header

      simulated_period = 0.
      Call WriteHdrBalanceFile
!     If (Nevent .eq. 1) Call WriteHdrBalanceHisFile (NwrwContinuous)  moved to InitialiseEvent

!*********************************************************************
!*** Voorbereiding voor event loop do 850
!*********************************************************************

      IDEBUG = 0

      IF (NEVENT .LE. 0) THEN
         call SetMessage(LEVEL_INFO, ' No simulation computations since no Events defined; NEVENT <= 0')
      ELSE

! Modflow wordt on-line gebruikt als het door unpaved en/of RR open water gebruikt wordt
        OnLineModFlowUsed= (OnLineModFlowUsed .or. OwOnlineModflowUsed)

        If (RunSimultaneous .and. .not. OnlineModflowUsed .and. .not. OnlineSobekLevelUsed .and. (ImlStp .eq. 0)) then
!           Warning: RunSimultaneousodule staat aan, maar er zijn geen online data uitwisselingen met Modflow, CF/SF en RTC.
            call ErrMsgStandard (979, 0, 'Sobek_RR', STRING)
!
!dec 2000; uitbreiding checks voor Process Manager
!       ElseIf (.Not. RunSimultaneous .and. (OnlineModflowUsed .or. OnlineSobekLevelUsed .or. (ImlStp .ne. 0)) ) then
!           Fatal Error: RunSimultaneousodule staat niet aan, maar er zijn wel online data uitwisselingen gewenst
!           call ErrMsgStandard (980, 0, 'Sobek_RR', STRING)
!dec 2000; nieuwe checks
        ElseIf (.Not. RunSimultaneous .and. (OnlineSobekLevelUsed .or. (ImlStp .ne. 0)) ) then
!           Fatal Error: RunSimultaneousodule staat niet aan, maar er zijn wel online data uitwisselingen met CF of RTC gewenst
            call ErrMsgStandard (980, 0, 'Sobek_RR', STRING)
        ElseIf (OnLineModflowUsed .and. .Not. RunSimultaneous .and. .not. UsePM) then
!           Fatal Error: OnLine uitwisselingen met Modflow, maar niet met stuurmodule en niet met ProcessManager
            call ErrMsgStandard (980, 0, 'Sobek_RR', STRING)
        ElseIf (OnLineModflowUsed .and. RunSimultaneous .and. UsePM) then
!           Inconsistentie: OnLine uitwisselingen met Modflow, via stuurmodule en via ProcessManager
!           Zet op uitwisseling via RunSimultaneousodule
            UsePm = .false.
            call SetMessage(LEVEL_FATAL, 'Iconsistense RunSimultaneousModule - ProcessManager in Delft_3B.f90')
!dec 2000; einde checks
        endif
! Alleen letten op Exchange tijdstap als ALLEEN Modflow online meedraait; anders ItmStu=default 1
        if (.not. OnLineModflowUsed .or. OnlineSobekLevelUsed .or. Imlstp .ne. 0) Itmstu = 1


! HarmonIT
        if (OnLineModFlowUsed) then
           Success = Dh_AllocInit (Novh+Now, ModFlowHeads, ModFlowFluxes, 0.E0)
           Success = success .and. Dh_AllocInit (Novh+Now, ModFlowIds, '')
           If (.not. success) call ErrMsgStandard (981, 0, ' Error allocating ModFlow arrays in subroutine ', ' Sobek_RR' )
        endif

!*********************************************************************
!***  Voorbereiding loop over alle buien in de reeks
!*********************************************************************

        JulianTimestep = Dble (TimeSettings%TimestepSize / 86400D0)
        if (NcNode .gt. 0 .and. HisConvergence .ne. 0) Call WriteHisHeaderConvergence

#if (defined(SOBEK_PARALLEL))
!       Unix version
!       Parallell draaien van de buien op Unix
!       Eerst sluiten alle _00 uitvoer files
!       bij Parallell draaien van een reeks: GEEN RR restart file genereren
!
         Call Close00Files
         If (Nevent .gt. 1) RestIo(2) = 0

#else
        if (EmulateUnixOnPC) then
!           test UX versie
            Call Close00Files
        endif

       !Get date and time
        CDate = ' '
        CTime = ' '
        CALL DATE_AND_TIME (CDATE,CTIME,CZONE,time_fields)
        If (TimeMessage) WRITE(*,'(A,A10)') ' After reading fixed data; before simulation ',CTIME
        AlreadySimulated = 0

#endif

      Endif

      RR_NEvent = Nevent

      RRInitialize = 0
      If (Nevent .le. 0) RRInitialize = -1

      if ( RRInitialize == 0 ) then
          RRInitialize = RRInitExternals (RR_RunId)
      endif
      FirstCallRRInitializeEvent = .true.

      call timstop(TimerRRInitialize)
  return
  end function RRInitialize

! ************************************************************************************
!    RRInitExternals - initialize for HarmonIT
! ************************************************************************************

  Integer function RRInitExternals (RR_RunId)

  ! Coupling of rainfall, seepage through HarmonIT/OpenMI
  use wl_open_mi_support
  use RR_open_mi_support

  Implicit none

  character(Len=oes_id_len), dimension(:), pointer :: Stations

  Integer RR_RunId

  Integer teller, oesStationsElmset, oesRainfallQuant, oesSeepageQuant, oesStrQuant

  Logical Success


! HarmonIT Rainfall (incoming)
  success = DH_AllocInit (NcStat, Stations, '')
  if (Success .and. NcStat .gt. 0) then
      Do teller = 1, ncStat
        Stations(teller) = StationName(teller)
      Enddo
      oesStationsElmset = OesElmsetFindOrCreate(RR_RunId,RRRainfallElmSet,stations)
      if (oesStationsElmset > 0) then
         OesRainfallQuant = OesExchItemCreate(RR_RunId,RRRainfall,RRRainfallElmSet,oes_accepting)
      endif
      DeAllocate(Stations)
  endif
! End HarmonIT Rainfall


! HarmonIT Runoff (incoming)
!  success = DH_AllocInit (NcStat, Stations, '')
!  if (Success .and. NcStat .gt. 0) then
!      Do teller = 1, ncStat
!        Stations(teller) = StationName(teller)
!      Enddo
!      oesStationsElmset = OesElmsetFindOrCreate(RR_RunId,RRRainfallElmSet,stations)
!      if (oesStationsElmset > 0) then
!         OesRainfallQuant = OesExchItemCreate(RR_RunId,RRRainfall,RRRainfallElmSet,oes_accepting)
!      endif
!      DeAllocate(Stations)
!  endif
! End HarmonIT Runoff


! Unpaved area seepage (incoming)
  success = DH_AllocInit (NcOvhg, Stations, '')
  if (Success .and. NcOvhg .gt. 0) then
      Do teller = 1, ncOvhg
        Stations(teller) = Id_Nod(UnpNam(teller))
      Enddo
      oesStationsElmset = OesElmsetFindOrCreate(RR_RunId,RRUnpavedElmSet,stations)
      if (oesStationsElmset > 0) then
         OesSeepageQuant = OesExchItemCreate(RR_RunId,RRSeepage,RRUnpavedElmSet,oes_accepting)
      endif
      DeAllocate(Stations)
  endif
! End Unpaved area seepage

! Openwater area seepage (incoming)
  success = DH_AllocInit (NcOw, Stations, '')
  if (Success .and. NcOw .gt. 0) then
      Do teller = 1, ncOw
        Stations(teller) = Id_Nod(OwNam(teller))
      Enddo
      oesStationsElmset = OesElmsetFindOrCreate(RR_RunId,RROpenwaterElmSet,stations)
      if (oesStationsElmset > 0) then
         OesSeepageQuant = OesExchItemCreate(RR_RunId,RRSeepage,RROpenwaterElmSet,oes_accepting)
      endif
      DeAllocate(Stations)
  endif
! End Unpaved area seepage


! RR structures, pumpstop yes/no
  success = DH_AllocInit (NcStru, Stations, '')
  if (Success .and. NcStru .gt. 0) then
      Do teller = 1, ncStru
        Stations(teller) = Id_Nod(StrNam(teller))
      Enddo
      oesStationsElmset = OesElmsetFindOrCreate(RR_RunId,RRStructureElmset,stations)
      if (oesStationsElmset > 0) then
         OesStrQuant = OesExchItemCreate(RR_RunId,RRPumpstop,RRStructureElmSet,oes_accepting)
      endif
      DeAllocate(Stations)
  endif
! End RR structures pumpstop


! Boundary nodes: water levels, surface area (incoming), salt
  success = DH_AllocInit (NcBoun, Stations, '')
  if (Success .and. NcBoun .gt. 0) then
      Do teller = 1, ncBoun
        Stations(teller) = Id_Nod(BndNam(teller))
      Enddo
      oesStationsElmset = OesElmsetFindOrCreate(RR_RunId,RRBoundaryElmSet,stations)
      if (oesStationsElmset > 0) then
         OesSeepageQuant = OesExchItemCreate(RR_RunId,RRBndLevels,RRBoundaryElmSet,oes_accepting)
         OesSeepageQuant = OesExchItemCreate(RR_RunId,RRBndDepths,RRBoundaryElmSet,oes_accepting)
         OesSeepageQuant = OesExchItemCreate(RR_RunId,RRBndAreas ,RRBoundaryElmSet,oes_accepting)
         OesSeepageQuant = OesExchItemCreate(RR_RunId,RRBndSaltConcentrations ,RRBoundaryElmSet,oes_accepting)
      endif
      DeAllocate(Stations)
  endif
! End Boundary area seepage

! Other external incoming data through HarmonIT
!
!    to be added
!
! end other external incoming data


! HarmonIT Outflows (outgoing)
    call InitOutOpenWaterLevels(RR_RunId)
    call InitOutUnpavedResults(RR_RunId)
    call InitOutStructureFlows(RR_RunId)
    call InitOutBoundaryFlows(RR_RunId)
    call InitOutNwrwFlows(RR_RunId)
! End HarmonIT Outflows

  RRInitExternals = 0

  return
  end function RRInitExternals


! ************************************************************************************
!    RRInitializeEvent
! ************************************************************************************

  Integer function RRInitializeEvent (RR_RunId,RR_Ievent,RR_NTimestepEvent)

  use wl_open_mi_support
  use rr_open_mi_support

  Implicit none
  Integer          RR_RunId, RR_Ievent, RR_NTimestepEvent
  Double Precision Julian, DNetCdfMaxNrDays
  Integer          NetCdfMaxNrDaysAdd
  Integer          idum, i
  Logical          FnmOpened

!extra variables for Modflow coupling using Process Manager
!  Real          ModFlowHeads(NOvh+NOw), ModFlowFluxes(NOvh+NOw)
!  Character*20  ModFlowIds  (NOvh+NOw)


      IEvent = RR_IEvent

!     write(*,*) ' Initialize event IEvent', IEvent

!*********************************************************************
!*** Initialisation of local variables
!*********************************************************************

      NrTimestepsNoConvergence = 0
      NrTimestepsNegativeVolume = 0
      Iout1 = ConfFil_get_iOut1()
!     EpsCri = RelativeConvergenceCheck
!     EpsCr2 = AbsoluteConvergenceCheck
       EPSCRI = 0.001
       EPSCR2 = 0.000001
! *********************************************************************
! Init DIO Streams
! *********************************************************************

      call InitDioPlt(outPlt_OpenWater)
      call InitDioPlt(inPlt_Rtc)
      call InitDioPlt(inPlt_CFSF)
      call InitDioPlt(inPlt_Salt)


#if (defined(SOBEK_PARALLEL))
! Unix version: open output files for event Ievent  separately
      Call SetFileNames (Ievent)
      ! Log file
      if (Iout1 .gt. 0) CALL OPENFL(IOUT1, ConfFil_get_NAMFIL(22),1,2)
      ! Debug file altijd openen
      Call OPENFL(IdebugLunRR, ConfFil_get_NAMFIL(32),1,2)
      ! Ascii Zout uitvoer
      if (IFlZt .gt. 0) CALL OPENFL(IFlZt, ConfFil_get_NAMFIL(47),1,2)

      ! Runoff.Out file altijd openen: IoutPl=IoutQb
      If (RunoffOutHis .eq. 0) then
         ! Runoff.Out file, old format
         if (HeaderRunoffOutAlways) then
            Call WrHdr (IOUTPL, IOUTCB)
         else
            Call OPENFL(IOutPl, ConfFil_get_NAMFIL(37),1,2)
         endif
      Else
         ! Runoff.Out file, HIS format using DIO
!        Call InitDioPlt(Runoff_Out)
         Call WrHdrCbFile (IOUTCB)
      Endif
      ! Zout op rand, daily totals op rand, ABR total, balance file
      if (NcBoun .gt. 0) Call OPENFL(IOutCb, ConfFil_get_NAMFIL(46),1,2)
      if (IOutAbrTot .gt. 0) CALL OPENFL(IOutAbrTot, ConfFil_get_NAMFIL(45),1,2)
      Call OpenFl (IoBal,ConfFil_get_NAMFIL(85),1,2)
      ! Ascii output Iout2 tm Iout8
      If (ncvhg .gt. 0)  Call OpenFl (IOut2,ConfFil_get_NAMFIL(24),1,2)
      if (ncovhg .gt. 0) Call OpenFl (IOut3,ConfFil_get_NAMFIL(25),1,2)
      if (nckas .gt. 0)  Call OpenFl (IOut4,ConfFil_get_NAMFIL(26),1,2)
      if (ncow .gt. 0)   Call OpenFl (IOut5,ConfFil_get_NAMFIL(27),1,2)
      if (ncstru .gt. 0) Call OpenFl (IOut6,ConfFil_get_NAMFIL(28),1,2)
      if (ncboun .gt. 0) Call OpenFl (IOut7,ConfFil_get_NAMFIL(29),1,2)
      if (ncpluv .gt. 0) Call OpenFl (IOut8,ConfFil_get_NAMFIL(30),1,2)
      ! overige HIS files
      if (Ievent .ne. 1) then
!        write(*,*) ' Call WriteHdr3BHisFiles 1', ievent
         Call WriteHdr3BHisfiles (Ievent, .false.)
         If (IWlmModule .gt. 0) Call WriteHdrWLMfile (Ievent, .false. )
      Endif
#else
!     pc version: do nothing

      if (EmulateUnixOnPC) then
!Test Unix version: open output files for event Ievent  separately
         Call SetFileNames (Ievent)
         if (Iout1 .gt. 0) CALL OPENFL(IOUT1, ConfFil_get_NAMFIL(22),1,2)
         Call OPENFL(IdebugLunRR, ConfFil_get_NAMFIL(32),1,2)
         if (IFlZt .gt. 0) CALL OPENFL(IFlZt, ConfFil_get_NAMFIL(47),1,2)
         If (RunoffOutHis .eq. 0) then
         ! Runoff.Out file, old format
            if (HeaderRunoffOutAlways) then
               Call WrHdr (IOUTPL, IOUTCB)
            else
               Call OPENFL(IOutPl, ConfFil_get_NAMFIL(37),1,2)
            endif
         Else
            ! Runoff.Out file, HIS format using DIO
!           Call InitDioPlt(Runoff_Out)
            Call WrHdrCbFile (IOUTCB)
         Endif
         if (NcBoun .gt. 0) Call OPENFL(IOutCb, ConfFil_get_NAMFIL(46),1,2)
         if (IOutAbrTot .gt. 0) CALL OPENFL(IOutAbrTot, ConfFil_get_NAMFIL(45),1,2)
         Call OpenFl (IoBal,ConfFil_get_NAMFIL(85),1,2)
         If (ncvhg .gt. 0)  Call OpenFl (IOut2,ConfFil_get_NAMFIL(24),1,2)
         if (ncovhg .gt. 0) Call OpenFl (IOut3,ConfFil_get_NAMFIL(25),1,2)
         if (nckas .gt. 0)  Call OpenFl (IOut4,ConfFil_get_NAMFIL(26),1,2)
         if (ncow .gt. 0)   Call OpenFl (IOut5,ConfFil_get_NAMFIL(27),1,2)
         if (ncstru .gt. 0) Call OpenFl (IOut6,ConfFil_get_NAMFIL(28),1,2)
         if (ncboun .gt. 0) Call OpenFl (IOut7,ConfFil_get_NAMFIL(29),1,2)
         if (ncpluv .gt. 0) Call OpenFl (IOut8,ConfFil_get_NAMFIL(30),1,2)
         if (Ievent .ne. 1) then
!            write(*,*) ' Call WriteHdr3BHisFiles 2', ievent
             Call WriteHdr3BHisfiles (Ievent, .false.)
             If (IWlmModule .gt. 0) Call WriteHdrWLMfile (Ievent, .false. )
         Endif
      endif
#endif


!*********************************************************************
!*** Spool files:
!***  - for rainfall: data should be consistent with start date of event
!***  - for verdamping, kas_initiele berging en gebruiksdata: spool naar start
!*********************************************************************

         IF (IOUT1 .ne. 0) WRITE (IOUT1,*) ' Start event ', Ievent
!        if (idebug .ne. 0) WRITE (Idebug,*) ' Start event ', Ievent

         Do i=1,6
           EvStrt(i) = EventStartDateTime (IEvent,i)
           if (i .le. 4) EvDura(i) = EventDuration (IEvent,i)
         Enddo
         IDateAct   = EventStartDateTime(IEvent,1)*10000 + EventStartDateTime(IEvent,2) * 100 + EventStartDateTime(IEvent,3)
         ITimeAct   = EventStartDateTime(IEvent,4)*10000 + EventStartDateTime(IEvent,5) * 100 + EventStartDateTime(IEvent,6)
         JulianStartEventSimulation = Julian (IDateAct, ITimeAct)

!***  Bepaal aantal tijdstappen per event

        FRSTTM = 1
! ARS 8845 gebruik real ivm integer overflow problemen
        RLASTTM = Float(EVDURA(1))*NRSDAY + Float(EVDURA(2))*NRSHR+Float(EVDURA(3))*NRSMIN + Float(EVDURA(4))
        LASTTM = RLASTTM / timeSettings%timestepSize
        simulated_period = simulated_period + ( float(lasttm) * timeSettings%timestepSize / nrshr)
        WagMod_ActNrTimestepsSimulation = Lasttm

        ! voor NetCdf output
        if (RR_Ievent .eq. 1) then
           NetCdfMaxNrDays = 0
           if (Nevent .eq. 1) then
              ! only 1 event
              idum = LastTm / OutputAtTimestep
              NetCdfMaxNrTimesteps = 1 + LastTm / OutputAtTimestep
              if (idum*OutputAtTimestep .lt. LastTm) NetCdfMaxNrTimesteps = NetCdfMaxNrTimesteps + 1
              DNetCdfMaxNrDays = NetCdfMaxNrTimesteps * (TimeSettings%TimestepSize * OutputAtTimestep) / 86400.
              NetCdfMaxNrDays = NetCdfMaxNrTimesteps * (TimeSettings%TimestepSize * OutputAtTimestep) / 86400.
              if ( (DNetCdfMaxNrDays-NetCdfMaxNrDays) .gt. 1D-6) NetCdfMaxNrDays = NetCdfMaxNrDays+1
              ! if not start at zero o'clock, nr days may be 1 more
              if (EventStartDateTime(Ievent,4) .ne. 0 .or. EventStartDateTime(Ievent,5) .ne. 0 .or. EventStartDateTime(Ievent,6) .ne. 0) NetCdfMaxNrDays = NetCdfMaxNrDays + 1
!             write(*,*) ' NetCdfMaxNrDays ', NetCdfMaxNrDays
!             write(*,*) ' NetCdfMaxNrTimesteps ', NetCdfMaxNrTimesteps
!             write(*,*) ' Timestep size ', Timesettings%TimestepSize
!             write(*,*) ' OutputAtTimestep ', OutputAtTimestep
           elseif (Nevent .ne. 1) then
              NetCdfMaxNrTimesteps = 1  ! for t=0  of first event
              ! all output timesteps of all events
              do ievent = 1, nevent
                 RLASTTM = Float(EventDuration(ievent,1))*NRSDAY + Float(EventDuration(ievent,2))*NRSHR+Float(EventDuration(ievent,3))*NRSMIN + Float(EventDuration(ievent,4))
                 LASTTM = RLASTTM / timeSettings%timestepSize
                 idum = LastTm / OutputAtTimestep
                 NetCdfMaxNrTimesteps = NetCdfMaxNrTimesteps + LastTm / OutputAtTimestep
                 if (idum*OutputAtTimestep .lt. LastTm) NetCdfMaxNrTimesteps = NetCdfMaxNrTimesteps + 1
                 DNetCdfMaxNrDays = Lasttm * (TimeSettings%TimestepSize * OutputAtTimestep) / 86400.D0
                 NetCdfMaxNrDaysAdd = Lasttm * (TimeSettings%TimestepSize * OutputAtTimestep) / 86400.
                 if ( (DNetCdfMaxNrDays-NetCdfMaxNrDaysAdd) .gt. 1D-6) NetCdfMaxNrDaysAdd = NetCdfMaxNrDaysAdd+1
                 NetCdfMaxNrDays = NetCdfMaxNrDays + NetCdfMaxNrDaysAdd + 1
              ! if not start at zero o'clock, nr days may be 1 more  ?
                 if (EventStartDateTime(Ievent,4) .ne. 0 .or. EventStartDateTime(Ievent,5) .ne. 0 .or. EventStartDateTime(Ievent,6) .ne. 0) NetCdfMaxNrDays = NetCdfMaxNrDays + 1
!                write(*,*) ' NetCdfMaxNrDays ', NetCdfMaxNrDays
!                write(*,*) ' DNetCdfMaxNrDays ', DNetCdfMaxNrDays
!                write(*,*) ' NetCdfMaxNrDaysAdd ', NetCdfMaxNrDaysAdd
!                write(*,*) ' NetCdfMaxNrTimesteps ', NetCdfMaxNrTimesteps
!                write(*,*) ' Timestep size ', Timesettings%TimestepSize
!                write(*,*) ' OutputAtTimestep ', OutputAtTimestep
              enddo
              Ievent = RR_IEvent
              if (iopt1(3) .eq. 0) then
                 ! if no detailed output, only max. output per event (so NEvent timesteps in output)
                 NetCdfMaxNrTimesteps = NEvent
!                write(*,*) ' NetCdfMaxNrTimesteps ', NetCdfMaxNrTimesteps
              endif
           endif
           if (NetCdfMaxNrDays .lt. 1) NetCdfMaxNrDays = 1
!          write(*,*) ' NetCdfMaxNrDays ', NetCdfMaxNrDays
        endif
!       recompute for current event again Lasttm
        RLASTTM = Float(EVDURA(1))*NRSDAY + Float(EVDURA(2))*NRSHR+Float(EVDURA(3))*NRSMIN + Float(EVDURA(4))
        LASTTM = RLASTTM / timeSettings%timestepSize

        CDate = ' '
        CTime = ' '
        CALL DATE_AND_TIME (CDATE,CTIME,CZONE,time_fields)
        IDateAct   = time_fields(1)*10000 + time_fields(2) * 100 + time_fields(3)
        ITimeAct   = time_fields(5)*10000 + time_fields(6) * 100 + time_fields(7)
        If (NEvent .eq. 1) then
           JulianStart = Julian (IDateAct, ITimeAct)
           RemTime = '000:01:00'
        Else
           JulianNow = Julian (IDateAct, ITimeAct)
        Endif

        IF (NEVENT .EQ. 1) CALL WRLOGO (IScren, 0,LASTTM,NEVENT,IBar0,EstimateRemainingDuration,RemTime)

!Voor communicatie controller simultaan draaien met CF/SF/RTC
        IF (RunSimultaneous) THEN
!           Zet initmode per bui weer op true
            InitMode = .true.
            FirstProc = .true.
            if (.not. dimr_mode) CALL INITFP (FirstProc, InitMode)
            DELTAT = IDH/100.+IDM/10000. + IDS/1000000.
        ENDIF
!end communicatie simultaan draaien


!*** Set initial values at start of event
!*** for rainfall data: mm/hour assumed;
!*** for evaporation data: mm/day assumed;

        if (idebug .ne. 0) WRITE (Idebug,*) ' Before SetInitialDatesAndTimes'
!       Call Conffil_set_idebug(IdebugLunRR)
        Call SetInitialDatesAndTimes (Ievent, JulianDate1, JulianDate2, NrDaysSinceStartFirstEvent, NrDaysSinceStart, &
                                      JulianDateTime1, JulianDateTime2, &
                                      NrSecondsSinceStartFirstEvent, NrTimestepsSinceStartFirstEvent, &
                                      ITime, TimeD, TimeE, tmpYear, tmpMonth, tmpDay, tmpHour, tmpMin, tmpSec)
        JulianStartSimulation = JulianDateTime1
!       if (idebug .ne. 0) write(idebug,*) ' after SetInitialDatesAndTimes'

! Check of door keuze tijdstapgrootte en opgegeven verdampingsinterval de verdamping wel wordt meegenomen
        Call CheckEvaporation  (IdH, IdM, IdS, EvapFormat2)

!Sub-header in ASCII file Pluvius rioolinloop per Pluvius knoop
!bepaal eindtijdstip van de gebeurtenis


! ARS 7445: only open Runoff.Out file in case of running simultaneously
#if (defined(SOBEK_PARALLEL))
! Unix version: do nothing
#else
!  pc version:
        If (RunSimultaneous) then
          If (RunoffOutHis .eq. 0) then
!           Runoff.Out file not in HIS format, but old format
            INQUIRE (FILE = ConfFil_get_NAMFIL(37), Opened = FNMOpened)
            IF (.NOT. FnmOpened .and. &
              (NCPLUV .GT. 0 .or. NcBoun .gt. 0))  Call OpenFl (IoutPl,ConfFil_get_NAMFIL(37),1,2)
          Else
!           Runoff.Out in HIS format using DIO
!           Call InitDioPlt(Runoff_Out)
          Endif
        Endif
#endif

        if (NwrwContinuous) then
           IOutPeriod = 1
!          Call WrHdrRunoffOut (LastTm, Ievent, IOutPl, cMAA, cDAG, cUUR, cMIN, cSEC, HeaderRunoffOutAlways, &
!                               NwrwContinuous, EmulateUnixOnPC, &
!                               IOutPeriod, NrOutputPeriods, OutputEventStartDateTime, OutputEventDuration)
        elseif (.not. TimeSettings%Output2CFUserDefinedPeriod .and. FirstCallRRInitializeEvent) then
           Call WrHdrRunoffOut (LastTm, Ievent, IOutPl, cMAA, cDAG, cUUR, cMIN, cSEC, HeaderRunoffOutAlways, &
                                NwrwContinuous, EmulateUnixOnPC, &
                                IOutPeriod, NrOutputPeriods, OutputEventStartDateTime, OutputEventDuration)
        elseif (TimeSettings%OutputEvent .eq. IEvent .and. FirstCallRRInitializeEvent) then
           Call WrHdrRunoffOut (LastTm, Ievent, IOutPl, cMAA, cDAG, cUUR, cMIN, cSEC, HeaderRunoffOutAlways, &
                                NwrwContinuous, EmulateUnixOnPC, &
                                IOutPeriod, NrOutputPeriods, OutputEventStartDateTime, OutputEventDuration)
        endif

! Nov 1995: NRSRAI ipv NRSHR
        deler = ConfArr_get_NRSRAI()
        TIMER = MOD (iTIME, deler)
        If (deler .eq. 86400) then
           Timer = Timer - StartSecDailyRainfall
           TIMER = MOD (Timer, deler)
        Endif
        TIMES = MOD (iTIME, NRSSBK)

! Evap file
        deler = ConfArr_get_NRSEvap()
        TIMEE = MOD (iTIME, deler)

! idem Runoff file
        if (NcRRRunoffExternal .gt. 0) then
           deler = ConfArr_get_NRSRunoff()
           TIMERRunoff = MOD (iTIME, deler)
        endif
        if (NcRRRunoffHBV .gt. 0) then
           deler = ConfArr_get_NRSTemperature()
           TIMERTemperature = MOD (iTIME, deler)
        endif

        Idebug = ConfFil_get_iDebug()
        IF (Idebug .ne.  0)  WRITE(Idebug, *) ' Timers', TimeD, TIMEE, TIMER, TimerRunoff, TimerTemperature, TIMES

        If (IEvent .eq. 1 .and. OutputDesired(6) .and. NcBoun .gt. 0) then
            Call WriteHisHeaderDailyBndFlows (Conffil_get_NamFil(81))
        Endif

!*********************************************************************
!*** Bepaal initiele kasberging
!*** Bepaal initiele grondwaterstand (vgl RDKINI)
!*********************************************************************
!*** Initialisaties per event;
!*** Indien gewenst lees initial conditions uit restart file
!*********************************************************************

        IF (NCKAS .GT. 0) CALL RDKINI (IEvent)

! Sept 1998: Controlmodule changed to take account of initialisation first timestep
        ITSTU  = 0
        If (RunSimultaneous) THEN
           If (Ievent .eq. 1) then
              TimOld = -1.
              TimNew =  0.
           Else
!             Bij volgende events TimOld (event2) = Eind Timold (event1) + 2 rekentijdstappen
!ARS... Dec 1999: ipv 1 dag tussen opeenvolgende gebeurtenissen: minimaal 2 rekentijdstappen
               TimOld = TimOld + 2. * Deltat
               TimNew = TimNew + 2. * Deltat
           Endif

           if (.not. firstproc) then
!             Simultaan draaien, RR wacht voor initialisatie op CF
!             Write(*,*) ' RR not first, Before Stepct', Timold, Timnew
!             pause
! UNST-4751 test
              if (.not. dimr_mode) CALL STEPCT (TimOld, TimNew, IDCNT, ISTAT, InitMode, Crashed)
!             Write(*,*) ' RR not first, After Stepct', Timold, Timnew
              If (crashed) Goto 9999
              If (ncboun .gt. 0) then
                Call Rdsbk
              Endif
!             MessageString = ' dll_mode ='
!             Write(MessageString(13:),*) dll_mode
!             call SetMessage(LEVEL_INFO, MessageString(1:len_trim(MessageString)))

              if (idebug .ne. 0) write(IdebugLunRR,*) ' RRInitExternals : dll_mode', dll_mode
              If (INSHIS .eq. 1 .and. OnLineSobekLevelUsed .and. .not. dll_mode) then
                 Call ReadDioPltSobekFile (inPlt_CFSF, ConfFil_get_NAMFIL(38), 1)
              Endif
! UNST-4751 test
              If (ISLCMP .ne.  0 .and. IWQModule .ne. 0 .and. .not. dll_mode) Call ReadDioPltSaltFile (inPlt_Salt, ConfFil_get_NAMFIL(121), 1)
              If (NcOvhg+NcOw+NcCell .gt. 0) then
                If (OnLineModflowUsed) then
#if (!defined(HAVE_CONFIG_H))
                   Call OpenFl (InModFlow, ConfFil_get_NAMFIL(102),3,1)
                   CALL RdModflowLevels (InModflow, 1)
                   Call CloseGP(INModFlow)
#endif
                Endif
             Endif
           Else
!             Simultaan draaien, RR is eerste; dus RR-RTC; nog geen akties
           Endif

        ElseIf (UsePm) then
#if (!defined(HAVE_CONFIG_H))
!Dec 2000: Initialisatie igv gebruik van de Process Manager; only implemented for Sobek-Modflow!!!
!          Write(*,*) ' Call Pmc_def'
!!IntelFortran    Call Pmc_Def (PmcArgs)
!          Write(*,*) ' After call Pmc_def; Inport=', PmcArgs(1)(1:32)
           Call PmInterface_Alloc(Ncovhg+Ncow)
!Get and check Modflow Timestep: ModflowTimestep is multiple of RR timestep
!          Write(*,*) ' Call GetModFlowTimestep'
           Call GetModflowTimestep (ModflowTimestep)
!          Write(*,*) ' After Call GetModFlowTimestep'
           ExchangeSettings%TimestepSize = ModflowTimestep * NrsDay
           String = ' '
           If ( (timesettings%timestepsize .gt. ExchangeSettings%timestepsize)) then
              call ErrMsgStandard (933, 0, '  Online Modflow timestepsize input', STRING)
           else
              idum = ExchangeSettings%Timestepsize / TimeSettings%Timestepsize
              if (ExchangeSettings%Timestepsize .ne. idum * TimeSettings%Timestepsize ) then
                 call ErrMsgStandard (933, 0, ' Online Modflow timestepsize input', STRING)
              endif
              ITMSTU = idum
              ITmStpCheck = idum + 1
           endif
!Get Modflow Starting Heads
!          Write(*,*) ' Call GetModFlowHeads initial situation'
           Call GetModflowHeads(ModflowIds, ModflowHeads, NcOvhg+NcOw, ModflowTime)
!          Write(*,*) ' After Call GetModFlowTimestep'
           Call ModflowData_to_SobekArrays (NcOvhg+NcOw, ModflowHeads, ModflowIds)
!          Write(*,*) ' Converted ModflowData to SobekArrays'
#endif
        Endif
!End Dec 2000

        Call ConfFil_set_IDEBUG(0)
        If (IdebugFromTimestep .le. 1 .and. IdebugToTimestep .gt. 0) &
                             call ConfFil_set_IDEBUG(IdebugLunRR)
        Idebug = ConfFil_get_IDEBUG()
!       Write(*,*) ' Calling RR_INIT1'

        LGSICacheFileName = ConfFil_get_NamFil(124)
        If (LGSICacheFileName .eq. '') LGSICacheFileName = 'CacheFile.txt'
        Inquire (FILE = LGSICacheFileName, EXIST = FnmExt)
        If (FnmExt) then
            ReadLGSICacheFile = .true.
!           Write(*,*) ' Ievent, ReadLGSICacheFile ', Ievent, ReadLGSICachefile
            Call OpenFl (iCache,LGSICacheFileName,1,1)
        Else
            ReadLGSICacheFile = .false.
!           Write(*,*) ' Ievent, ReadLGSICacheFile ', Ievent, ReadLGSICachefile
            Call OpenFl (iCache,LGSICacheFileName,1,2)
            call SetMessage(LEVEL_WARN, 'LGSICacheFile not found, will be written')
         Endif

        CALL RR_INIT1 (IEVENT, Idebug, Iout1, ICache, FirstCallRRInitializeEvent, DefaultT0OutputValue, GlobalNAMAlfa, ReadLGSICacheFile)
!       Write(*,*) ' after INIT1 ievent, ReadLGSICacheFile ', Ievent, ReadLGSICachefile
        Call CloseGP(ICache)
!       Write(*,*) ' After call RR_INIT1'
        Idebug = 0
!       Write(*,*) ' End of Init1 in Bui'

! zet output voor ABR op uurbasis op nul bij begin van nieuwe gebeurtenis
        NrSecondsSinceStart = 0
        NrSecondsIoutAbr = 0
        QtotalBoundaries = 0.0

! read Restart file?
        If (RestIo(1) .EQ. 1)  THEN
           If (Idebug .ne. 0)  WRITE(Idebug, *) ' read restart file ', Conffil_Get_Namfil(41)
           CALL OPENFL(InRestart, ConfFil_get_NAMFIL(41),2,1)
           Call RdRest (InRestart, RestartVersion)
           Call CloseGP(InRestart)
!          Initialisatie unpaved, RRRunoff opnieuw
           Imaand = ConfArr_get_iMonth()
           Call Init1Unpaved (Idebug, Ievent, imaand, .true.)
!          Write(*,*) ' before Restart - INIT1 ievent, ReadLGSICacheFile ', Ievent, ReadLGSICachefile
           Call RRRunoffNode_Init1 (Ievent, Iout1, ICache, GlobalNAMAlfa, .false. , ReadLGSICacheFile)
!          Write(*,*) ' after Restart - INIT1 ievent, ReadLGSICacheFile ', Ievent, ReadLGSICachefile
!           Call Init1Cel     (Idebug, Ievent, imaand, .true.)
           idum = 0
           Call Sacramento_Init1 (Ievent, Iout1, idum, .false.)

! ******************************************************************
! 50626    ! additional OpenDA restart; for time being only unpaved!
! ******************************************************************
           OpenDAFileName = ConfFil_get_NamFil(123)
           if (OpenDAFileName .ne. '' .and. UseOpenDAFile) then
              Inquire (FILE = Conffil_get_Namfil(123), EXIST = FnmExt)
              If (FnmExt) then
                  Call OpenFl (OpenDAFileUnit, OpenDAFileName, 1,1)
                  Call ReadOpenDAPaved (OpenDAFileUnit, Iout1)
                  Call ReadOpenDAUnpaved (OpenDAFileUnit, Iout1, Update)
                  if (update) Call Init1Unpaved (Idebug, Ievent, imaand, .true.)
                  update = .false.
                  Call ReadOpenDAGreenhouse (OpenDAFileUnit, Iout1)
                  Call ReadOpenDAOpenWater (OpenDAFileUnit, Iout1)
                  Call ReadOpenDASacramento (OpenDAFileUnit, Iout1, Update)
                  if (update) Call Sacramento_Init1 (Ievent, Iout1, idum, .false.)
                  update = .false.
                  Call ReadOpenDAHBV        (OpenDAFileUnit, Iout1, update)
                  Call ReadOpenDADNAM        (OpenDAFileUnit, Iout1, update)
!                  Call ReadOpenDASCS           (OpenDAFileUnit, Iout1)
!                  Call ReadOpenDAWagmod        (OpenDAFileUnit, Iout1)
!                  Call ReadOpenDALGSI          (OpenDAFileUnit, Iout1)
                  Call ReadOpenDAWalrus        (OpenDAFileUnit, Iout1, update)
!                 Write(*,*) ' before update - INIT1 ievent, ReadLGSICacheFile ', Ievent, ReadLGSICachefile
                  if (update) Call RRRunoffNode_Init1 (Ievent, Iout1, ICache, GlobalNAMAlfa, .false. , ReadLGSICacheFile)
!                 Write(*,*) ' after update - INIT1 ievent, ReadLGSICacheFile ', Ievent, ReadLGSICachefile
                  Call CloseGP(OpenDAFileUnit)
              else
                  If (Idebug .ne. 0) write(Idebug,*)  ' OpenDafile not found, not read'
              endif
           else
              If (Idebug .ne. 0) write(Idebug,*) ' OpenDafile not read, filename: ', OpenDaFileName(1:30), ' use: ',UseOpenDaFile
           endif
        Endif  ! end of restart file plus optional OpenDA
!
!       Simultaan draaien: Initiele waterstanden RR in HIS file uitvoeren
        If (RunSimultaneous) then
          If (.not. OnlineModflowUsed .and. (ImlStp .eq. 0) .and. (IwqModule .eq. 0)) then
               ! do nothing
          elseif (OutputOwAndGwtoRtc) then
             Call WriteOpenWaterDio (outPlt_OpenWater, 0)
          else
             Call WriteOpenWaterDioOrg (outPlt_OpenWater, 0)
          endif
!         Einde initialisatie fase; nu wachten
          TimOld =  0.
          TimNew =  TimOld
!         IF (iDebug .ne.   0)  WRITE(IDEBUG, *) ' Before Stepct',TIMOLD,TIMNEW,ITMSTU,ITSTU
!         Write(*,*) ' Initialisatiefase Before Stepct', Timold, Timnew
!         pause
! UNST-4751 test
          if (.not. dimr_mode) CALL STEPCT (TimOld, TimNew, IDCNT, ISTAT, InitMode, Crashed)
!         Write(*,*) ' Initialisatiefase After Stepct', Timold, Timnew
          If (crashed) Goto 9999
!         Initialisatie afhankelijkheden voor rekenstappen
          InitMode = .false.
          FirstProc = .true.
! UNST-4751 test
          if (.not. dimr_mode) CALL INITFP (FirstProc, InitMode)
        Endif

! Sept97: vaste HIS file met structure data
!       IF (IEVENT .EQ. 1 .and. NCSTRU .GT. 0)  CALL WriteFixedStructuredata

! Write Headers HIS files bij eerste gebeurtenis
        IF (Ievent .EQ. 1) Then
          Call WriteHdrBalanceHisFile (NwrwContinuous)
!         write(*,*) ' Call WriteHdr3BHisFiles 3', ievent
          Call WriteHdr3BHisfiles (Ievent, .true.)
          If (IWlmModule .gt. 0) Call WriteHdrWLMfile (Ievent, .true. )
        Endif
!       Call ConfFil_set_IDEBUG(0)

  9999  Continue
        RR_NTimestepEvent= Lasttm - Frsttm + 1
        RRInitializeEvent = 0
! FirstCallRRInitializeEvent moet eigenlijk alleen false gezet worden als in_open_mi =true
! maar deze vlag is alleen in de wrapper omgeving met nieuwe WL_openMIsupport etc. gedefinieerd
! en nog niet in deze losse RR, moet nodig geintegreerd worden!!!!!
        ! FirstCallRRInitializeEvent = .false.
        FirstCallRRInitializeEvent = .true.
        If (Crashed) RRInitializeEvent = -1

        call Init3BHisVariables (Ievent, LastTm, DefaultT0OutputValue, RestartVersion)
  return
  end function RRInitializeEvent


! ************************************************************************************
!    RRInitializeTimestep
! ************************************************************************************

  Integer function RRInitializeTimestep (RR_RunId,RR_Ievent,RR_Timestep)

  use wl_open_mi_support
  use rr_open_mi_support

  Implicit none
  Integer  RR_RunId, RR_Ievent, RR_Timestep
  Integer  weekdy, idum
  Double Precision Julian

  call timstrt('RRInitializeT',TimerRRInitializeT)
!extra variables for Modflow coupling using Process Manager
!  Real          ModFlowHeads(NOvh+NOw), ModFlowFluxes(NOvh+NOw)
!  Character*20  ModFlowIds  (NOvh+NOw)

         IEvent = RR_IEvent
         Itmstp = RR_Timestep
!        write(*,*) ' Initialize timestep ', Itmstp
! Extra logical for ARS 6315 to determine daily max. gwlevels
         UpdateDailyValues = .false.
         ResetIterationCounter = .False.
         if (idebug .ne. 0) write(Idebug,*) ' InitialiseTimestep ',Itmstp

! HarmonIT - TestSaveState
!          If (TestSaveState) then
!             Call RR_SaveState(Itmstp)
!          Endif

! HarmonIT - TestSaveState / RestoreSaveState
!          If (TestSaveState .and. itmstp .eq. RestoreInTimeStep) then
!             Call RR_RestoreState(RestoreTimeStep)
!          Endif

! Set debug file and Capsim debug file, if desired
         Call ConfFil_set_IDEBUG(0)
         If (itmstp .ge. IdebugFromTimestep .and. itmstp .le. IdebugToTimestep) then
            call ConfFil_set_IDEBUG(IdebugLunRR)
         Elseif (itmstp .ge. Idebug2FromTimestep .and. itmstp .le. Idebug2ToTimestep) then
            call ConfFil_set_IDEBUG(IdebugLunRR)
         Endif
         debug_unit = 0
         if (itmstp .ge. IdebugCapsimFromTimestep .and. itmstp .le. IdebugCapsimToTimestep) then
            debug_unit = CapsimDebug_unit
         endif

         Idebug = ConfFil_get_IDEBUG()
         If (Idebug .ne.  0)  WRITE(Idebug, *) ' Timestep', ITMSTP

!***     Set times
         Call SetDatesAndTimes (Idebug, ITMSTP, TimeD, TimeE, TimeR, TimerRunoff, TimerTemperature, TimeS, IdH, IdM, IdS)

!***     Read meteo &kasgebruik data;
!***     Determine data for current timestep
         IF (ITMSTP .EQ.1) THEN
            UpdateDailyValues = .true.
            BuiTmstp = 1
            EvapTmstp = 1
            RunoffTmstp = 1
            TemperatureTmstp = 1
            DayInEvent = 1
            IF (NCKAS .GT. 0)  CALL RDGEBR (Ievent, DayInEvent)
            IF (NCOVHG+NcCell .GT. 0)  CALL RDCRF  (iDebug, ConfArr_get_iMonth(), ConfArr_get_iDay() )

! Crop factor open water wordt gebruikt voor open water, maar ook voor berging op land verhard, onverhard en kasgebied
! Daarom deze file eigenlijk altijd lezen, tenzij er alleen NWRW knopen zijn
            IF (NcNode .ne. NcPluv)   CALL RDCRFO (ConfArr_get_iMonth(), ConfArr_get_iDay() )
            If (EvapFormat2) then
               CALL RdEvap2 (nrEvapStations)
            Else
               CALL RDEVAP (nrEvapStations, 1, Ievent, EvapTmstp)
            Endif
            CALL RDRAIN (IEvent, BuiTmstp)
            CALL RDRunoff (IEvent, RunoffTmstp)
            CALL RDTemperature (IEvent, TemperatureTmstp)
! uitgecommentarieerd omdat er voor waterstanden randknopen
! apart iets geregeld is.
            IF (TIMES .GE. NRSSBK)  TIMES = TIMES-NRSSBK
            IF (TIMER .GE. ConfArr_get_NRSRAI())  TIMER = TIMER - ConfArr_get_NRSRAI()
            IF (TIMEE .GE. ConfArr_get_NRSEvap())  TIMEE = TIMEE - ConfArr_get_NRSEvap()
            IF (TIMED .GE. NRSDAY)  TIMED = TIMED-NRSDAY
            IF (TIMER .GE. ConfArr_get_NRSRunoff())  TIMERRunoff = TIMERRunoff - ConfArr_get_NRSRunoff()
            IF (TIMER .GE. ConfArr_get_NRSTemperature())  TIMERTemperature = TIMERTemperature - ConfArr_get_NRSTemperature()
         ELSE
            IF (TIMER .GE. ConfArr_get_NRSRAI())  THEN
               BuiTmstp = BuiTmstp + 1
               CALL RDRAIN (IEvent, BuiTmstp)
               TIMER = TIMER - ConfArr_get_NRSRAI()
            ENDIF
! UNST-5103
            IF (TIMEE .GE. ConfArr_get_NRSEVAP())  THEN
               EvapTmstp = EvapTmstp + 1
               CALL RDEvap (nrEvapStations, 0, IEvent, EvapTmstp)
               TIMEE = TIMEE - ConfArr_get_NRSEvap()
            ENDIF
!end UNST-5103
            IF (TIMERRunoff .GE. ConfArr_get_NRSRunoff() )  THEN
               RunoffTmstp = RunoffTmstp + 1
               CALL RDRunoff (IEvent, RunoffTmstp)
               TIMERRunoff = TIMERRunoff - ConfArr_get_NRSRunoff()
            ENDIF
            IF (TIMERTemperature .GE. ConfArr_get_NRSTemperature() )  THEN
               TemperatureTmstp = TemperatureTmstp + 1
               CALL RDTemperature (IEvent, TemperatureTmstp)
               TIMERTemperature = TIMERTemperature - ConfArr_get_NRSTemperature()
            ENDIF
            IF (TIMED .GE. NRSDAY) THEN
               UpdateDailyValues = .true.
               NewYear = .false.
               DayInEvent = DayInEvent + 1
               TIMED = TIMED-NRSDAY
               tmpYear = ConfArr_get_IYEAR()
               tmpDay = ConfArr_get_IDAY()
               tmpMonth = ConfArr_get_iMonth()
               NewYear = (tmpMonth .eq. 12 .and. tmpDay .eq. 31)
               If (NewYear .and. itmstp .gt. 1 .and. NwrwContinuous) then
                   Call WriteNWRWSystemBalance (Itmstp-1, LastTm)
               Endif
               CALL NXTDAY (IDEBUG, tmpYear, tmpMonth, tmpDay)
               call ConfArr_set_iYear(tmpYear)
               call ConfArr_set_iMonth(tmpMonth)
               call ConfArr_set_iDay(tmpDay)
! Nov 2001: direct ook IHour op nul zetten, en niet pas na call RdEvap; gaat anders fout in TablesModule/Julian/Gregor !!!
               call ConfArr_set_IHOUR(0)
! NB           IdayWk wordt voor Call INIT2 aangepast
               IF (NCKAS .GT. 0)  CALL RDGEBR (IEvent, DayInEvent)
               IF (NCOVHG+NcCell .GT. 0)  CALL RDCRF  (Idebug, ConfArr_get_iMonth(), ConfArr_get_iDay() )
               IF (NcNode .ne. NcPluv) CALL RDCRFO (ConfArr_get_iMonth(), ConfArr_get_iDay() )
!               If (.not. EvapFormat2)  CALL RDEVAP (nrEvapStations, 0, Ievent, EvapTmstp)

! okt 1996: extra uitvoer in HIS file op dagbasis als output RR-boundaries gewenst wordt
               IF (NCBOUN .GT. 0) THEN
                   if (OutputDesired(6) ) then
!                     NrDaysSinceStartFirstEvent = aantal dagen vanaf begin bui 1
                      NrDaysSinceStartFirstEvent = NrDaysSinceStartFirstEvent + 1
                      DaysCounter                = DaysCounter + 1
                      Call HisDailyBndFlows (NrDaysSinceStartFirstEvent, DaysCounter)
!                     WRITE(IOUTDY)  NrDaysSinceStartFirstEvent, (QDYBND(teller),teller=1,NCBOUN)
                   end if
                   DO teller=1,NCBOUN
                      QDYBND(teller) = 0.
                   ENDDO
               ENDIF
! end okt 1996
            ENDIF
         ENDIF
         IF (ITMSTP .EQ. Lasttm) UpdateDailyValues = .true.
! set irrigation season and daily period
         tmpDay = ConfArr_get_IDAY()
         tmpMonth = ConfArr_get_iMonth()
         tmpHour = ConfArr_get_iHour()
         tmpMin  = ConfArr_get_iMinute ()
         tmpSec  = ConfArr_get_isecond ()
! assume irrigation season in summer, so start month <= end month
         if (tmpMonth .lt. timeSettings%IrrigationStartMonth) IrrigationSeason = .false.
         if (tmpMonth .eq. timeSettings%IrrigationStartMonth .and. tmpDay .ge. timeSettings%IrrigationStartDay) IrrigationSeason = .true.
         if (tmpMonth .gt. timeSettings%IrrigationStartMonth .and. tmpMonth .lt. timeSettings%IrrigationEndMonth) IrrigationSeason = .true.
         if (tmpMonth .eq. timeSettings%IrrigationEndMonth .and. tmpDay .lt. timeSettings%IrrigationEndDay) IrrigationSeason = .true.
         if (tmpMonth .eq. timeSettings%IrrigationEndMonth .and. tmpDay .ge. timeSettings%IrrigationEndDay) IrrigationSeason = .false.
         if (tmpMonth .gt. timeSettings%IrrigationEndMonth) IrrigationSeason = .false.
! assume irrigation daily period from hour < irrigation daily period to hour
         if (tmpHour .lt. timeSettings%irrigationFromHr) IrrigationDailyPeriod=.false.
         if (tmpHour .ge. timeSettings%irrigationFromHr .and. tmpHour .lt. timeSettings%IrrigationToHr) IrrigationDailyPeriod=.true.
         if (tmpHour .ge. timeSettings%irrigationToHr) IrrigationDailyPeriod=.false.

         cMAA = INTCH2(ConfArr_get_iMonth())
         cDAG = INTCH2(ConfArr_get_IDAY())
         cUUR = INTCH2(ConfArr_get_iHour())
         cMIN = INTCH2(ConfArr_get_iMinute())
         cSEC = INTCH2(ConfArr_get_iSecond())
         If (EvapFormat2)  CALL RdEvap2 (nrEvapStations)


! schrijf uitvoer voor Aanvoer.Abr file: totale lozing per uur op boundaries per tijdstap
         IF (NCBOUN .GT. 0 .and. IoutAbrTot .gt. 0 .and.  NrSecondsIoutAbr .ge. NrSHr) then
            write(IoutAbrTot,1919) ConfArr_get_IYEAR(), cMAA, cDAG, cUUR, cMIN, cSEC, QtotalBoundaries/NrSecondsIoutAbr
!  1919      FORMAT(I4,5A2,1X,E10.4)
  1919      FORMAT(I4,5A2,1X,F7.2)
            QtotalBoundaries = 0.0
            NrSecondsIoutAbr = 0
         endif

!controller simultaan draaien
        ITSTU = ITSTU + 1
! Aanroep Stepct verplaatst naar einde van de tijdstaploop, dus altijd 1 tijdstap
! want Delft_3b loopt voor op SOBEK-CF/SF

!April 1998: als 3B not first: wait!!
        IF (ITMSTP .EQ.1 .AND. RunSimultaneous .AND. .NOT. FirstProc) THEN
            Call SetTimNew (tmpYear, tmpMonth, tmpDay, tmpHour, tmpMin, tmpSec, TimOld)
!           Write(*,*) ' RR not first in timestep 1; call wait Stepct', Timold, Timnew
!           pause
! UNST-4751 test
            if (.not. dimr_mode) CALL STEPCT (TIMOLD, TIMOLD, IDCNT, ISTAT, InitMode, crashed)
!           Write(*,*) ' RR not first in timestep 1; after wait Stepct', Timold, Timnew
            If (crashed) Goto 9999
!            write(*,*) ' After wait FirstProc'
        ENDIF

! Lezen waterstanden randkopen uit tabel of uit online His-file van Sobek
! EERST checken of er wel randknopen zijn (bij Pluvius-rioleringssommen zijn die er in het algemeen niet!)
! GP 30 okt 1997; 4 maart 1998: altijd Tabel Rdsbk aanroepen
        IF (NCBOUN .GT. 0)  CALL RDSBK
        IF (NCBOUN+NcPluv .gt. 0) THEN
          IF (INSBK .GT. 0) then !  .AND. TIMES .GE. NRSSBK) THEN
!           MessageString = ' dll_mode ='
!           Write(MessageString(13:),*) dll_mode
!           call SetMessage(LEVEL_INFO, MessageString(1:len_trim(MessageString)))
! UNST-4751 test
            If (INSHIS .eq. 1 .and. OnLineSobekLevelUsed .and. .not. dimr_mode) then
              ! lezen uit online HIS-file
              Call ReadDioPltSobekFile (inPlt_CFSF, ConfFil_get_NAMFIL(38), itmstp)
            ENDIF
            TIMES = TIMES-NRSSBK
          ENDIF
! UNST-4751 test
          If (ISLCMP .ne.  0 .and. IWQModule .ne. 0 .and. .not. dimr_mode) then
              Call ReadDioPltSaltFile (inPlt_Salt, ConfFil_get_NAMFIL(121), 1)
          Endif
        endif

! Lees on-line info uit Modflow, if necessary
        If (NcOvhg+NcOw+NcCell .gt. 0) then
           If (OnLineModflowUsed) then
#if (!defined(HAVE_CONFIG_H))
             If (.not. UsePM) then
!               Geen process manager: kijk on-line in de HIS file
                Call OpenFl (InModFlow, ConfFil_get_NAMFIL(102),3,1)
                CALL  RdModflowLevels (InModflow, Itmstp)
                Call CloseGP (INModFlow)
             ElseIf (UsePM .and. itmstp .ne. ITmstpCheck) then
!               Process manager: alleen lezen om de x tijdstappen!!
               ! Get and check new Modflow timestep
!               Write(*,*) ' Call GetModFlowTimestep'
                Call GetModflowTimestep (ModflowTimestep)
!               Write(*,*) ' After Call GetModFlowTimestep'
                ExchangeSettings%TimestepSize = ModflowTimestep * NrsDay
                String = ' '
                If ( (timesettings%timestepsize .gt. ExchangeSettings%timestepsize)) then
                   call ErrMsgStandard (933, 0, '  Online Modflow timestepsize input', STRING)
                else
                   idum = ExchangeSettings%Timestepsize / TimeSettings%Timestepsize
                   if (ExchangeSettings%Timestepsize .ne. idum * TimeSettings%Timestepsize ) then
                      call ErrMsgStandard (933, 0, ' Online Modflow timestepsize input', STRING)
                   endif
                   ITMSTU = idum
                   ITmStpCheck = ItmstpCheck + idum
                endif
                ! Get new Modflow Heads
!               Write(*,*) ' Call GetModFlowHeads for timestep', itmstp
                Call GetModflowHeads(ModflowIds, ModflowHeads, NcOvhg+NcOw, ModflowTime)
!               Write(*,*) ' After Call GetModFlowTimestep'
                Call ModflowData_to_SobekArrays (NcOvhg+NcOw, ModflowHeads, ModflowIds)
!               Write(*,*) ' Converted ModflowData to SobekArrays'
             else
             Endif
#endif
           Endif
        Endif

!check maalstop (lees RTC uitvoer van vorige tijdstap; dwz. voor t=1 wordt initiele uitvoer gelezen
        IF (iDebug .ne.   0)  WRITE(IDEBUG, *) ' IMLSTP', IMLSTP, MSFIL
!       write(*,*) ' InPlt_Rtc lun IMLSTP', InPlt_Rtc % ds % inStream % lun, IMLSTP, MSFIL
        IF (IMLSTP .ne.  0 .AND. MSFIL .NE. ' ')  Call ReadDioPltRtcFile (inPlt_Rtc, MsFil, ItmStp)

!***    Determine day of the week  (0=zondag, 1=maandag etc)
         IDAYWK = WEEKDY (ConfArr_get_IYEAR(), ConfArr_get_iMonth(), ConfArr_get_IDAY())

!***    Initialize arrays per timestep

!        Call Init2 (IDAYWK, ITMSTP, 1)

! Output at T=0 desired? alleen bij losse bui, niet bij reeks
! Alle flows zijn nul; initiele peilen en volumes.
          If (itmstp .eq. 1 .and. nevent .eq. 1) then
             IDateAct   = tmpYear*10000 + TmpMonth* 100 + tmpDay
             ITimeAct   = TmpHour*10000 + TmpMin* 100 + TmpSec
             JulianNowSimulation = Julian (IDateAct, ITimeAct)
!            Write(*,*) ' Write3BHisFilesTnul call 1'
             Call Write3BHisfilesTnul (Ievent, LastTm, DefaultT0OutputValue, RestartVersion)
             If (IWlmModule .gt. 0) Call Write3BWLMHisfile (Ievent, 0)
             Call Write3BBalanceHisfile (0, LastTm)
             PlvBalEvents = 0
             PlvBalHistory = 0
          ElseIf (itmstp .eq. 1 .and. Iopt1(3) .ne. 0 .and. ievent .eq. 1) then
          ! also Tnul output if OutputSeriesFullDetail switch is on
!            Write(*,*) ' Write3BHisFilesTnul call 2'
             Call Write3BHisfilesTnul (Ievent, LastTm, DefaultT0OutputValue, RestartVersion)
             If (IWlmModule .gt. 0) Call Write3BWLMHisfile (Ievent, 0)
             ! geen RRBalans bij reeksberekening
             ! Call Write3BBalanceHisfile (0, LastTm)
             PlvBalEvents = 0
             PlvBalHistory = 0
          Endif
!***    Initialize arrays per timestep
          Call Init2 (IDAYWK, ITMSTP, 1, 0)
! Schrijf tussentijdse restart file?
          If ( RestIo(2) .ne. 0 .and. ievent .eq. 1) Then
             if (RestIo(6) .eq. Int(NrSecondsSinceStart)) then
                Call OpenFl (IOutRestart, ConfFil_get_NAMFIL(97),2,2)
                Call WrRest (IoutRestart)
                Call CloseGP (IoutRestart)
             elseif (Restio(7) .eq. Int(NrSecondsSinceStart)) then
                Call OpenFl (IOutRestart, ConfFil_get_NAMFIL(98),2,2)
                Call WrRest (IoutRestart)
                Call CloseGP (IoutRestart)
             endif
          Endif
! Restart file each timestep?
          If (RestartFileEachTimestep .ne. 0) then
             ! write restart file each timestep
             if (RestartFileNameEachTimestepOption .eq. 0) then
                ! fixed file name
                Call OpenFl (IOutRestart, ConfFil_get_NAMFIL(42),2,2)
             else
                ! file name constructed from actual date/time
                RestartFileName = ''
                CALL DATE_AND_TIME (CDATE,CTIME,CZONE,time_fields)
                RestartFileName = RestartPrefix(1:len_trim(RestartPrefix)) // CDate(1:8) // '_' // CTime(1:6) // CTime(8:10) // '.rrr'
                Call OpenFl (IOutRestart, RestartFileName,2,2)
             endif
             Call WrRest (IoutRestart)
             Call CloseGP (IoutRestart)
          endif
          NrSecondsSinceStart = NrSecondsSinceStart + timeSettings%timestepSize

!*********************************************************************
!***     Loop over all configuration nodes
!***       met convergentiecriterium op outflow open water
!***                         huidige iteratie QOUTOW en vorige iteratie QOUT0
!              ook check op QIN0 (.,2) van onverhard gebied
!              ook check op QIN0 (.,4) van structures
!***       iteratief!
!*********************************************************************

          Converged = .false.
          Iter = 0
          NegativeVolumeInCurrentTimestep = .false.


        RRInitializeTimestep=0
  9999  Continue
        if (Crashed) RRInitializeTimestep = -1

        call timstop(TimerRRInitializeT)

  return
  end function RRInitializeTimestep


! ************************************************************************************
!    RRGetExternals (use RRGetValues)   To be added
! ************************************************************************************


  Integer Function RRGetExternals(RR_RunId,RR_IEvent,RR_Timestep)

! HarmonIT Rainfall
    use wl_open_mi_support
    use RR_open_mi_support

    Implicit none
    Integer  RR_RunId, RR_Ievent, RR_Timestep

    real, dimension(:), pointer        :: rainValues, seepValues, pumpValues, bndValues
    logical, dimension(:), pointer     :: mask
    integer                            :: numStations, numConnected
    logical                            :: allocSuccess
    integer                            :: i

    Idebug = Conffil_get_idebug()

! Rainfall data
! Expected unit in input: mm/hour.
! converted to array Rain(i) in m/s

    numConnected = 0
    numStations = Network_get_nrMeteo()
    allocSuccess = DH_AllocInit(numStations, rainValues, 0.0)
    allocSuccess = DH_AllocInit(numStations, mask, .false.)

    if (idebug .ne. 0) write(idebug, *) 'GetExternals, Rain, timestep', RR_Timestep
    if (allocSuccess) then
        numConnected = OesGetConnected(RR_RunId,RRRainfall,RRRainfallElmSet, rainValues, mask)
        if (idebug .ne. 0) write(idebug, *) '    #stations : ', numStations, &
                                            '    #connected: ', numConnected
        if ( numConnected > 0 ) then
           DO I=1, numStations
              if ( mask(I) ) then
                if (idebug .ne. 0) write(idebug, '(A,I4,A,F16.8)') 'i=', i, 'value=', rainValues(I)
                RAIN(I) = rainValues(I) * MM2M / 3600
              endif
           ENDDO
        endif
    endif
    deallocate(rainValues, mask)
! End HarmonIT Rainfall

! Runoff data
! Expected unit in input: ??
! converted to array Runoff(i) in m/s

!   numConnected = 0
!   numStations = Network_get_nrMeteo()
!   allocSuccess = DH_AllocInit(numStations, runoffValues, 0.0)
!   allocSuccess = DH_AllocInit(numStations, mask, .false.)
!
!   if (idebug .ne. 0) write(idebug, *) 'GetExternals, Runoff, timestep', RR_Timestep
!   if (allocSuccess) then
!       numConnected = OesGetConnected(RR_RunId,RRRunoff,RRRunoffElmSet, rainValues, mask)
!       if (idebug .ne. 0) write(idebug, *) '    #stations : ', numStations, &
!                                           '    #connected: ', numConnected
!       if ( numConnected > 0 ) then
!          DO I=1, numStations
!             if ( mask(I) ) then
!               if (idebug .ne. 0) write(idebug, '(A,I4,A,F16.8)') 'i=', i, 'value=', runoffValues(I)
!               Runoff(I) = runoffValues(I) * MM2M / 3600
!             endif
!          ENDDO
!       endif
!   endif
!   deallocate(runoffValues, mask)
! End HarmonIT Runoff


!  Unpaved area seepage
! Expected unit in input: mm/day .
! converted to array Kwel(i) and WegZg(i) in m/s
    numConnected = 0
    allocSuccess = DH_AllocInit(ncOvhg, seepValues, 0.0)
    allocSuccess = DH_AllocInit(ncOvhg, mask, .false.)

    if (idebug .ne. 0) write(idebug, *) 'GetExternals, Unpaved-Seepage, timestep ', RR_Timestep
    if ( allocSuccess) then
        numConnected = OesGetConnected(RR_RunId,RRSeepage,RRUnpavedElmSet, seepValues, mask)
        if (idebug .ne. 0) write(idebug, *) '    #Unpaved areas: ', ncOvhg, &
                                            '    #connected: ', numConnected
        if ( numConnected > 0 ) then
           DO I=1, ncOvhg
              if ( mask(I) ) then
                if (idebug .ne. 0) write(idebug, '(A,I4,A,F16.8)') 'i=', i, 'value=', seepValues(I)
                if (SeepValues(i) .ge. 0) then
                   Kwel(I) = seepValues(I) * MM2M / 86400
                   Wegzg(I) = 0.0
                Else
                   WegZg(I) = -1. * seepValues(I) * MM2M / 86400
                   Kwel (I) = 0.0
                Endif
              endif
           ENDDO
        endif
    endif
    deallocate(seepValues, mask)
! End OpenMI Unpaved seepage values

!  Open water seepage
! Expected unit in input: mm/day .
! converted to array OwKwel(i) and OwWegZ(i) in m/s
    numConnected = 0
    allocSuccess = DH_AllocInit(ncOw, seepValues, 0.0)
    allocSuccess = DH_AllocInit(ncOw, mask, .false.)

    if (idebug .ne. 0) write(idebug, *) 'GetExternals, OpenWater-Seepage, timestep ', RR_Timestep
    if ( allocSuccess) then
        numConnected = OesGetConnected(RR_RunId,RRSeepage,RROpenwaterElmSet,seepValues, mask)
        if (idebug .ne. 0) write(idebug, *) '    #OpenWaterNodes: ', ncOw, &
                                            '    #connected: ', numConnected
        if ( numConnected > 0 ) then
           DO I=1, ncOw
              if ( mask(I) ) then
                if (idebug .ne. 0) write(idebug, '(A,I4,A,F16.8)') 'i=', i, 'value=', seepValues(I)
                if (SeepValues(i) .ge. 0) then
                   OwKwel(I) = seepValues(I) * MM2M / 86400
                   OwWegz(I) = 0.0
                Else
                   OwWegZ(I) = -1. * seepValues(I) * MM2M / 86400
                   OwKwel(I) = 0.0
                Endif
              endif
           ENDDO
        endif
    endif
    deallocate(seepValues, mask)
! End OpenMI Open water seepage values

! Pumpstop factors
    numConnected = 0
    allocSuccess = DH_AllocInit(ncStru, pumpValues, 0.0)
    allocSuccess = DH_AllocInit(ncStru, mask, .false.)
    if (idebug .ne. 0) write(idebug, *) 'GetExternals, Pumpstop, timestep ', RR_Timestep
    if ( allocSuccess) then
        numConnected = OesGetConnected(RR_RunId,RRPumpstop,RRStructureElmSet,pumpValues, mask)
        if (idebug .ne. 0) write(idebug, *) '    #RRStructures: ', ncStru, &
                                            '    #connected: ', numConnected
        if ( numConnected > 0 ) then
           DO I=1, ncStru
              if ( mask(I) ) then
                if (idebug .ne. 0) write(idebug, '(A,I4,A,F16.8)') 'i=', i, 'value=', pumpValues(I)
                MsFact(i) = pumpValues(i)
              endif
           ENDDO
        endif
    endif
    deallocate(pumpValues, mask)
! End OpenMI Open water seepage values


!  Boundary nodes: water levels, depths, surface area
! Expected unit in input: m, m2
    numConnected = 0
    allocSuccess = DH_AllocInit(ncBoun, bndValues, 0.0)
    allocSuccess = DH_AllocInit(ncBoun, mask, .false.)

! levels
    if (idebug .ne. 0) write(idebug, *) 'GetExternals, BoundaryValues, timestep ', RR_Timestep
    if ( allocSuccess) then
        numConnected = OesGetConnected(RR_RunId,RRBndLevels,RRBoundaryElmSet, BndValues, mask)
        if (idebug .ne. 0) write(idebug, *) '    #Boundary nodes: ', ncBoun, &
                                            '    #connected: ', numConnected
        if ( numConnected > 0 ) then
           DO I=1, ncBoun
              if ( mask(I) ) then
                if (idebug .ne. 0) write(idebug, '(A,I4,A,F16.8)') 'i=', i, 'value=', bndValues(I)
                BndPar(I,1) = BndValues(i)
                if (RR_Timestep .le. 1) BndPar(I,4) = BndValues(i)
              endif
           ENDDO
        endif
    endif
! depths
    mask = .false.
    if ( allocSuccess) then
        numConnected = OesGetConnected(RR_RunId,RRBndDepths,RRBoundaryElmSet, BndValues, mask)
        if (idebug .ne. 0) write(idebug, *) '    #Boundary nodes: ', ncBoun, &
                                            '    #connected: ', numConnected
        if ( numConnected > 0 ) then
           DO I=1, ncBoun
              if ( mask(I) ) then
                if (idebug .ne. 0) write(idebug, '(A,I4,A,F16.8)') 'i=', i, 'value=', bndValues(I)
                BndPar(I,6) = max (0.0, BndValues(i))
              endif
           ENDDO
        endif
    endif
! areas
    mask = .false.
    if ( allocSuccess) then
        numConnected = OesGetConnected(RR_RunId,RRBndAreas,RRBoundaryElmSet, BndValues, mask)
        if (idebug .ne. 0) write(idebug, *) '    #Boundary nodes: ', ncBoun, &
                                            '    #connected: ', numConnected
        if ( numConnected > 0 ) then
           DO I=1, ncBoun
              if ( mask(I) ) then
                if (idebug .ne. 0) write(idebug, '(A,I4,A,F16.8)') 'i=', i, 'value=', bndValues(I)
                BndPar(I,5) = max(0.01, BndValues(i))
              endif
           ENDDO
        endif
    endif
    deallocate(bndValues, mask)
! End OpenMI Boundary values


! More external data may be added
!
!
! End other external data


    RRGetExternals=0

  return
  end function RRGetExternals


! ************************************************************************************
!    RRPerformTimestep
! ************************************************************************************

  Integer function RRPerformTimestep (RR_RunId,RR_Ievent,RR_Timestep)

  Implicit none
  Integer  RR_RunId, RR_Ievent, RR_Timestep
  Integer  Returncode


!!       restore state?
!        If (TestSaveState .and. RR_timestep .eq. RestoreInTimestep) Call RR_RestoreState(RR_Timestep)
! add error message if RR_Timesteps exceeds Lasttm (only possible in dll mode)
        If (RR_Timestep .gt. Lasttm) then
           call ErrMsgStandard (972, 0, 'RRPerformTimestep can not be executed since request for next timestep is outside available data period.', &
                              ' Probably DIMR simulation period is defined larger than Sobek_RR simulation period, check input!')
           ReturnCode = 972
           goto 9999
        Endif

!       initialisatie per tijdstap
        ReturnCode = RRInitializeTimestep(RR_RunId,RR_IEvent,RR_Timestep)
        If (ReturnCode .ne. 0) goto 9999

!       get data from externals, using HarmonIT
        ReturnCode = RRGetExternals(RR_RunId,RR_IEvent,RR_Timestep)
        If (ReturnCode .ne. 0) goto 9999

!       compute the timestep
!       write(*,*) ' Start compute timestep ', RR_Timestep
        ReturnCode = RRComputeTimestep(RR_RunId,RR_IEvent,RR_Timestep)
        If (ReturnCode .ne. 0) goto 9999

!       write(*,*) ' Start Finalize timestep ', RR_Timestep
!       finalise the timestep, including putting output data to Delft-IO HIS files
        ReturnCode = RRFinalizeTimestep(RR_RunId,RR_IEvent,RR_Timestep)
        If (ReturnCode .ne. 0) goto 9999

!!       save state?
!        If (TestSaveState) Call RR_SaveState(RR_Timestep)

  9999  Continue
        RRPerformTimestep = ReturnCode


  return
  end function RRPerformTimestep



! ************************************************************************************
!    RRComputeTimestep
! ************************************************************************************

  Integer function RRComputeTimestep (RR_RunId,RR_Ievent,RR_Timestep)

  Implicit none
  Integer  RR_RunId, RR_Ievent, RR_Timestep

        call timstrt('RRComputeT',TimerRRComputeT)

         IEvent = RR_IEvent
         Itmstp = RR_Timestep

!*********************************************************************
!***     Loop over all configuration nodes
!***       met convergentiecriterium op outflow open water
!***                         huidige iteratie QOUTOW en vorige iteratie QOUT0
!              ook check op QIN0 (.,2) van onverhard gebied
!              ook check op QIN0 (.,4) van structures
!***       iteratief!
!*********************************************************************

          Idebug = Conffil_get_idebug()
          Do While (ITER .lt. MAXITR .and. .not. Converged)
             Iter = Iter + 1
! minstens 2 iteraties vereist, uitgezonderd NWRW-Pluvius knopen en OW-precip knopen
             If (iter .gt. 2) Converged = .true.
             IF (iDebug .ne. 0) WRITE(IDEBUG,*) ' Simulation iteration nr',ITER

             ! simuleer alle knopen
!            write(*,*) ' Call NodeLp timestep ', Itmstp
             CALL NODELP (IEVENT, ITMSTP, ITER, IDAYWK, makelogfile, MessageInundation, &
                          NegativeVolumeInCurrentTimestep, NrTimestepsNegativeVolume, GlobalNAMAlfa)

! ***        igv alleen NWRW of Sacramento nodes: 1 iteratie is voldoende
             If (NCNODE .ne. NCPLUV .and. NcNode .ne. NcSacr) Then
                 Call CheckConvergence (Epscri, Epscr2, Iter, Converged)
             Else
                 Converged = .true.
             EndIf
          EndDo

!         additional gw links computations, after finalisation of the normal computations
          if (GWLinkExists) Call GWLinkComputations
!
        RRComputeTimestep = 0
        If (Crashed) RRComputeTimestep = -1

        call timstop(TimerRRComputeT)
  return
  end function RRComputeTimestep



! ************************************************************************************
!    RRFinalizeTimestep
! ************************************************************************************

  Integer function RRFinalizeTimestep (RR_RunId,RR_Ievent,RR_Timestep)

  Implicit none
  Integer            RR_RunId, RR_Ievent, RR_Timestep
  Double Precision   Julian

        call timstrt('RRFinalizeT',TimerRRFinalizeT)

         IEvent = RR_IEvent
         Itmstp = RR_Timestep

! Aug 1996: geef evt. melding bij geen convergentie
! check open water, debieten huidige en vorige iteratie
! melding als bepaalde nauwkeurigheid niet gehaald wordt
!
          If (.not. Converged) &
            Call NoConvergence (Iout1, Itmstp, Warn4, NrTimestepsNoConvergence, Iter, EPSCRI)
!           Call NoConvergence (Iout1, Itmstp, Warn4, NrTimestepsNoConvergence, Iter, EPSCRI, EPS, eps2, epsCr2)
          IF (iDebug .ne.   0) WRITE(IDEBUG,*) ' After 701'
          If (HisConvergence .ne. 0)  Call HisInfoConvergence (itmstp)

!Feb 1996
!zoutberekeningen, als gewenst
          IF (ISLCMP .ne.  0)  THEN
             RTMSZ = Dble(timeSettings%timestepSize)
             IF (iDebug .ne.   0)  WRITE(IDEBUG,*) ' Start Saltcomp', RTMSZ
             CALL SALINE (ITMSTP, RTMSZ, IFLZT)
             CALL DtCbnd
          ENDIF

          Call CheckMessages (makelogfile, Ievent, Itmstp, Iout1, MessageVolumeCheck, MessageInundation, MessagePerTimestep)

! ***     Store results of current timestep

          CALL WRTOUT (IEVENT, Bal3B, MaxBalTerms, UpdateDailyValues, Itmstp)

!         WRITE(*,*) ' Na WRTOUT timestep ', ItmStp
          IF (iDebug .ne.   0)  WRITE(IDEBUG,*) ' Na WRTOUT'

! ***     Write HIS file voor Modflow
          if (OnlineModflowUsed) then
#if (!defined(HAVE_CONFIG_H))
!           IoutModflow = DioNewLun()
            Call WrHdr_Modflow (IOutModflow, IEvent, Itstu )
            Call WrData_Modflow (IOutModFlow, Itstu)
            If (UsePM) then
!             uitvoer van fluxen via de Process Manager
               SobekTime = Itmstp * TimeSettings%Timestepsize / NrsDay
!              Write(*,*) ' Call SetSobekModflowFluxes for timestep', itmstp
               Call SetSobekModflowFluxes (ModflowIds, ModflowFluxes, NcOvhg+NcOw)
!              Write(*,*) ' Call PutSobekModflowFluxes'
               Call PutSobekModflowFluxes (ModflowIds, ModflowFluxes, NcOvhg+NcOw, SobekTime)
!              Write(*,*) ' PutSobebkModflowFluxes'
            Endif
#endif
          elseif (Ncovhg + NcOw .gt. 0) then
!   zonder Modflow, maar toch de file genereren als er onverharde gebieden of open water knopen zijn
! 4 feb 2001: genereren file uitgezet als Modflow niet gebruikt wordt
!            Call WrHdr_Modflow (IOutModflow,IEvent, Itmstp )
!            Call WrData_Modflow (IOutModFlow,Itmstp)
          endif

! ***     Write ASCII file Pluvius rioolinloop per Pluvius knoop
          HisIndexRunoffOut = Itmstp-1 + EventStartDateTime(Ievent,7)
          if (NwrwContinuous) then
             IDateAct = OutputEventStartDateTime(IOutPeriod,1)*10000 + &
                          OutputEventStartDateTime(IOutPeriod,2)* 100 + &
                            OutputEventStartDateTime(IOutPeriod,3)
             ITimeAct = OutputEventStartDateTime(IOutPeriod,4)*10000 + &
                          OutputEventStartDateTime(IoutPeriod,5)* 100 + &
                            OutputEventStartDateTime(IOutPeriod,6)
             JulianStartOutputDate = Julian (IDateAct, ITimeAct)
             TimeInEvent  = Dble ((OutputEventDuration(IOutPeriod,6)-1)) * Dble(NRSecsRai) / Dble(NrsDay)
             JulianEndOutputDate = Julian(IDateAct,ITimeAct) + TimeInEvent
! de JulianEndOutputDate is nog niet helemaal ok?
             IDateAct = ConfArr_get_IYEAR()*10000 + ConfArr_get_iMonth()* 100 + ConfArr_get_IDAY()
             ITimeAct = ConfArr_get_iHour()*10000 + ConfArr_get_iMinute()* 100 + ConfArr_get_iSecond()
             JulianCurrent = Julian (IDateAct, ITimeAct)
             JulianNow     = JulianCurrent
             JulianNowSimulation = JulianCurrent
             if (JulianCurrent .ge. JulianStartOutputDate .and.  &
                   (JulianCurrent .le. JulianEndOutputDate+0.1*JulianTimestep)) then
                if (JulianCurrent .eq. JulianStartOutputDate) then
                   PlvBalEvents(4) = PlvBal(9)
                   PlvBalEvents(5) = PlvBal(7)
                   PlvBalEvents(6) = PlvBal(8)
!                  If (IoutPeriod .gt. 1) then
                     Call WrHdrRunoffOut (LastTm, Ievent, IOutPl, cMAA, cDAG, cUUR, cMIN, cSEC, HeaderRunoffOutAlways, &
                                NwrwContinuous, EmulateUnixOnPC, &
                                IOutPeriod, NrOutputPeriods, OutputEventStartDateTime, OutputEventDuration)
!                  Endif
                Endif
                if (JulianCurrent .le. JulianEndOutputDate-0.1*JulianTimestep) then
                   Call WrDataRunoffOut (IOutPl, cMAA, cDAG, cUUR, cMIN, cSEC, HisIndexRunoffOut)
                endif
                JulianDiff = DABS(JulianCurrent-JulianEndOutputDate)
                If (DABS(JulianCurrent-JulianEndOutputDate) .le. 0.1*JulianTimestep) then
                   PlvBalEvents(1) = PlvBalEvents(1) + PlvBal(9) - PlvBalEvents(4)
                   PlvBalEvents(2) = PlvBalEvents(2) + PlvBal(7) - PlvBalEvents(5)
                   PlvBalEvents(3) = PlvBalEvents(3) + PlvBal(8) - PlvBalEvents(6)
                Endif
             elseif (JulianCurrent .gt. JulianEndOutputDate .and. IOutPeriod .lt. NrOutputPeriods) then
                IOutPeriod = IOutPeriod + 1
             endif
          elseif (.not. TimeSettings%Output2CFUserDefinedPeriod) then
!            write(*,*) ' WrDataRunoffOut (IoutPl, ....) '
             Call WrDataRunoffOut (IOutPl, cMAA, cDAG, cUUR, cMIN, cSEC, HisIndexRunoffOut)
!            write(*,*) ' Na WrDataRunoffOut (IoutPl, ....) '
          else
             ! user defined output period; only output if current timestep within output period
             IDateAct   = ConfArr_get_IYEAR()*10000 + ConfArr_get_iMonth()* 100 + ConfArr_get_IDAY()
             ITimeAct   = ConfArr_get_iHour()*10000 + ConfArr_get_iMinute()* 100 + ConfArr_get_iSecond()
             JulianCurrent = Julian (IDateAct, ITimeAct)
             JulianNowSimulation = JulianCurrent
             if (JulianCurrent .ge. TimeSettings%JulianStartOutput .and.  &
                   JulianCurrent .le. TimeSettings%JulianEndOutput) then
                Call WrDataRunoffOut (IOutPl, cMAA, cDAG, cUUR, cMIN, cSEC, HisIndexRunoffOut)
             endif
          endif

! altijd: uitvoer in HIS file van nivo's in open water knopen
!          Call WriteOpenWaterDio (outPlt_OpenWater, Itmstp)
! nav performance test DIO: alleen als simultaan gedraaid wordt
          If (RunSimultaneous .and. .not. dll_mode) then
            If (.not. OnlineModflowUsed .and. (ImlStp .eq. 0) .and. (IwqModule .eq. 0)) then
               ! do nothing
            elseif (OutputOwAndGwtoRtc) then
               Call WriteOpenWaterDio (outPlt_OpenWater, 0)
            else
               Call WriteOpenWaterDioOrg (outPlt_OpenWater, 0)
            endif
          endif

! write output for WLM, if necessary
!         If (IWlmModule .gt. 0) Call Write3BWLMHisFile (Ievent,Itmstp)
          If (IWlmModule .gt. 0) Call Write3BWLMHisFile (Ievent, Itmstp+NrTimestepsSinceStartFirstEvent)

! write direct output in 3B HIS files, if desired
          IF (IOPT1(1) .eq. 1 .and. IOPT1(3) .eq. 1)  &
!            Write(*,*) ' Write3BHisFiles call 3'
             Call Write3BHisfiles (Ievent, Itmstp+NrTimestepsSinceStartFirstEvent, LastTm)
!            Call Write3BHisfiles (Ievent, Itmstp, LastTm)
          IF (IOPT1(1) .eq. 1 .and. Ievent .eq. 1 .and. Nevent .eq. 1) then
!          IF (IOPT1(1) .eq. 1 .and. Ievent .eq. 1)  then
             If (NwrwContinuous) then
               If (Itmstp .eq. LastTm) then
                  Call Write3BBalanceHisfile (Itmstp, LastTm)
                  Call WriteNWRWSystemBalance (LastTm, LastTm)
               Endif
             else
               Call Write3BBalanceHisfile (Itmstp, LastTm)
             Endif
          Endif


! ***     Write ASCII file Q en C op randen indien gewenst
          if (NwrwContinuous) then
             IDateAct = OutputEventStartDateTime(IOutPeriod,1)*10000 + &
                          OutputEventStartDateTime(IOutPeriod,2)* 100 + &
                            OutputEventStartDateTime(IOutPeriod,3)
             ITimeAct = OutputEventStartDateTime(IOutPeriod,4)*10000 + &
                          OutputEventStartDateTime(IoutPeriod,5)* 100 + &
                            OutputEventStartDateTime(IOutPeriod,6)
             JulianStartOutputDate = Julian (IDateAct, ITimeAct)
             TimeInEvent  = Dble ((OutputEventDuration(IOutPeriod,6)-1)) * Dble(NRSecsRai) / Dble (NrsDay)
             JulianEndOutputDate = Julian(IDateAct,ITimeAct) + TimeInEvent
             IDateAct = ConfArr_get_IYEAR()*10000 + ConfArr_get_iMonth()* 100 + ConfArr_get_IDAY()
             ITimeAct = ConfArr_get_iHour()*10000 + ConfArr_get_iMinute()* 100 + ConfArr_get_iSecond()
             JulianCurrent = Julian (IDateAct, ITimeAct)
             if (JulianCurrent .ge. JulianStartOutputDate .and.  &
                 JulianCurrent .le. JulianEndOutputDate-0.1*JulianTimestep) then
                Call WrdataBoundaries (IOutPl, IOutCb, cMAA, cDAG, cUUR, cMIN, cSEC, HisIndexRunoffOut)
             endif
          elseif (.not. TimeSettings%Output2CFUserDefinedPeriod) then
             Call WrdataBoundaries (IOutPl, IOutCb, cMAA, cDAG, cUUR, cMIN, cSEC, HisIndexRunoffOut)
          else
             ! user defined output period; only output if current timestep within output period
             IDateAct   = ConfArr_get_IYEAR()*10000 + ConfArr_get_iMonth()* 100 + ConfArr_get_IDAY()
             ITimeAct   = ConfArr_get_iHour()*10000 + ConfArr_get_iMinute()* 100 + ConfArr_get_iSecond()
             JulianCurrent = Julian (IDateAct, ITimeAct)
             JulianNowSimulation = Julian (IDateAct, ITimeAct)
             if (JulianCurrent .ge. TimeSettings%JulianStartOutput .and.  &
                   JulianCurrent .le. TimeSettings%JulianEndOutput) then
                Call WrdataBoundaries (IOutPl, IOutCb, cMAA, cDAG, cUUR, cMIN, cSEC, HisIndexRunoffOut)
             endif
          endif


! uitvoer op randen:
!     totalen in Aanvoer.Abr file
!     bepaal uitvoer voor Aanvoer.Abr file: totale lozing per uur op boundaries per tijdstap
          IF (NCBOUN .GT. 0)  THEN
             if (ioutAbrTot .gt. 0) then
                do teller = 1, NCBOUN
                   if (RRCFConnect(BndNam(teller))) QtotalBoundaries = QtotalBoundaries + QBND(teller) * timeSettings%timestepSize
                Enddo
                NrSecondsIoutAbr = NrSecondsIoutAbr + timeSettings%timestepSize
             endif
! uitvoer voor dagtotalen in HIS file
!            Vector/Array manipulation
             QDYBND = QDYBND + QBND * timeSettings%timestepSize
          ENDIF
!
          CDate = ' '
          CTime = ' '
          CALL DATE_AND_TIME (CDATE,CTIME,CZONE,time_fields)
          IDateAct   = time_fields(1)*10000 + time_fields(2) * 100 + time_fields(3)
          ITimeAct   = time_fields(5)*10000 + time_fields(6) * 100 + time_fields(7)
          JulianNow = Julian (IDateAct, ITimeAct)
          If (NEvent .eq. 1) then
             EstimatedRemainingDuration = Float(LastTm-ITmstp) / Float(Itmstp+1) * (JulianNow-JulianStart) * 86400.
          Else
             EstimatedRemainingDuration = Float(TotalNrTimesteps-ITmstp-AlreadySimulated) / &
                                             Float(AlreadySimulated+Itmstp+1) * (JulianNow-JulianStart) * 86400.
          Endif
          RemHours = EstimatedRemainingDuration / 3600
          EstimatedRemainingDuration = EstimatedRemainingDuration - 3600 * RemHours
          RemMinutes = EstimatedRemainingDuration / 60
          EstimatedRemainingDuration = EstimatedRemainingDuration - 60 * RemMinutes
          RemSeconds = EstimatedRemainingDuration
          RemTime = '000:00:00'
          Write(RemTime,'(I3,1X,I2,1X,I2)') RemHours, RemMinutes, RemSeconds
          Remtime(4:4) = ':'
          Remtime(7:7) = ':'
          Do teller=1,9
            if (Remtime(teller:teller) .eq. ' ') RemTime(teller:teller) = '0'
          Enddo
          IF (NEVENT .EQ. 1) CALL WRLOGO (IScren, ITMSTP,LASTTM,NEVENT,IBar0,EstimateRemainingDuration,RemTime)
          IF (NEVENT .GT. 1 .and. ((Itmstp/12)*12 .eq. itmstp) .and. (EstimateRemainingDuration)) then
               teller = max (1,Ievent-1)
               CALL WRLOGO (IScren, Teller,Nevent,NEVENT,IBar0,EstimateRemainingDuration,RemTime)
          Endif


!controller
          CHKSTU = (ITSTU .GE. ITMSTU)
! parallel rekenen ???

          IF (RunSimultaneous .AND. CHKSTU) THEN
#if (!defined(HAVE_CONFIG_H))
            If (OnlineModflowUsed) Call CloseGP(IoutModflow)
#endif
            call timstrt('RunSimult', timerRRRunSimult)
            ITSTU = 0
            If (RunoffOutHis .eq. 0) then
            ! old format Runoff.Out file
               IF (NCPLUV .GT. 0 .Or. NcBoun .gt. 0) Call CloseGP (IOUTPL)
            Else
            ! Runoff.Out in HIS format using DIO
!              Call InitDioPlt(Runoff_Out)
            Endif
!           Bepaal nieuw tijdstip
            Call SetControlDatesAndTimes (tmpYear, tmpMonth, tmpDay, tmpHour, tmpMin, tmpSec, &
                                           IdH, IdM, IDS, TimNew, Idebug)
            TIMOLD0= TIMOLD
            TIMOLD = TIMNEW
            IF (IMLSTP .ne.  0) TIMNEW = TIMNEW + DELTAT-0.00000001

            IF (iDebug .ne.   0)  WRITE(IDEBUG, *) ' Before Stepct',TIMOLD,TIMNEW,ITMSTU,ITSTU

! RunSimultaneousodule test June 1998; ivm problemen met Runoff.out file bij reeks, bui 2 e.v.:
! wait 5 milli seconds using GPSLEEP from CONTROL.LIB, using MSFWIN.LIB
!           write(*,*) ' Before gpsleep ',timold, timnew
!!! ARS 9068/9081: slow performance
!            Do teller=1,5
!               CALL GPSLEEP (1)
!            Enddo
!!! end ARS 9068/9081
! end test June 98
!           Write(*,*) ' Before Stepct at end of timestep', Timold, Timnew
! UNST-4751 test
            if (.not. dll_mode) CALL STEPCT (TIMOLD, TIMNEW, IDCNT, ISTAT, InitMode, crashed)
!           Write(*,*) ' After Stepct at end of timestep', Timold, Timnew
            IF (ISTAT .LT. 0)  THEN
                WRITE (STRING,'(I3)') ISTAT
                call ErrMsgStandard (990, 0, 'Sobek_3B', STRING)
            ENDIF
            Do teller=1,2  ! Kampar 2008;  !was altijd 1  ! was 10
               call timstrt('Sleep', timerRRSleep)
               if (.not. dll_mode) CALL GPSLEEP (1)
               call timstop(timerRRSleep)
            Enddo
            If (crashed) goto 9999

!           write(*,*) ' Open Runoff.out again'
! ARS 7445  except if last timestep of event
! GP RR_DLL issue 2021, do it when run in dll_mode
            If (Itmstp .ne. LastTm .or. dll_mode) then
              If (RunoffOutHis .eq. 0) then
              ! Runoff.Out file, old format IoutPl=IoutQb
!                write(*,*) ' Open Runoff.Out again'
                 IF (NCPLUV .GT. 0 .or. NcBoun .gt. 0)  Call OpenFl (IoutPl,ConfFil_get_NAMFIL(37),1,2)
              Else
              ! Runoff.Out file, HIS format using DIO
!                Call InitDioPlt(Runoff_Out)
              Endif
            Else
! ARS 7445  wait a little moment
!!! ARS 9068/9081: slow performance
               Do teller=1,5  ! Kampar 2008;  !was altijd 1  ! was 10
                  call timstrt('Sleep', timerRRSleep)
                  if (.not. dll_mode) CALL GPSLEEP (1)
                  call timstop(timerRRSleep)
               Enddo
!!! end ARS 9068/9081
            Endif
            IF (iDebug .ne.   0)  WRITE(IDebug, *) ' After Stepct', TIMOLD, TIMNEW
            call timstop(timerRRRunSimult)

          ENDIF
!endController

!check balansfout per tijdstap?; ARS 1953
          IF (CheckBalance .and. (ncnode-ncpluv .gt. 0))  Call WriteCheckBalance (ITmstp)

!***     end of loop over the number of timesteps
! einde parallel rekenen



  9999  Continue

        ModJulianTime = ModJulianTime + JulianTimestep

        RRFinalizeTimestep = 0
        If (Crashed) RRFinalizeTimestep = -1

        call timstop(TimerRRFinalizeT)
  return
  end function RRFinalizeTimestep



! ************************************************************************************
!    RRFinaliseEvent
! ************************************************************************************

  Integer function RRFinalizeEvent (RR_RunId,RR_Ievent)

  Implicit none
  Integer  RR_RunId, RR_Ievent

  IEvent = RR_Ievent
! write(Iout1,*) ' RR finaliseEvent ', IEvent

! uitvoer laatste uur Aanvoer.Abr file: totale lozing per uur (of deel laatste uur) op boundaries
         IF (NCBOUN .GT. 0 .and. IoutAbrTot .gt. 0) then
            Call SetDatesAndTimes (Idebug, ITMSTP, TimeD, TimeE, TimeR, TimerRunoff, TimerTemperature, TimeS, IdH, IdM, IdS)
!           Nov2007: JIRA 19253; if hour=24, shift to the next day
            if (ConfArr_get_iHour() .eq. 24) then
               CALL NXTDAY (IDEBUG, tmpYear, tmpMonth, tmpDay)
               call ConfArr_set_iYear(tmpYear)
               call ConfArr_set_iMonth(tmpMonth)
               call ConfArr_set_iDay(tmpDay)
               call ConfArr_set_IHOUR(0)
            endif
!           end JIRA 19253
            cMAA = INTCH2(ConfArr_get_iMonth())
            cDAG = INTCH2(ConfArr_get_IDAY())
            cUUR = INTCH2(ConfArr_get_iHour())
            cMIN = INTCH2(ConfArr_get_iMinute())
            cSEC = INTCH2(ConfArr_get_iSecond())
            write(IoutAbrTot,1919) ConfArr_get_IYEAR(), cMAA, cDAG, cUUR, cMIN, cSEC, QtotalBoundaries/NrSecondsIoutAbr
  1919      FORMAT(I4,5A2,1X,F7.2)
         endif
! okt 1996: extra uitvoer in HIS file op dagbasis na afloop gebeurtenis
        IF (NCBOUN .GT. 0) THEN
           NrDaysSinceStartFirstEvent = NrDaysSinceStartFirstEvent + 1
           DaysCounter                = DaysCounter + 1
           if (OutputDesired(6) ) then
               Call HisDailyBndFlows (NrDaysSinceStartFirstEvent, DaysCounter)
!              WRITE(IOUTDY)  NrDaysSinceStartFirstEvent, (QDYBND(teller),teller=1,NCBOUN)
           end if
!          Vector/Array initialisation
           QDYBND = 0.
        ENDIF
! end okt 1996

! ***   Store and write overall results per event
        Call WROOUT (IEvent)
        If (Nevent .gt. 1 .and. Iopt1(3) .ne. 1) then
!           Write(*,*) ' Write3BHisFiles call 4'
            NrEventInSeries = IEvent
            Call Write3BHisfiles (0, NrDaysSinceStart, Nevent)
        endif

! Output maxima directly in ASCII files
        Call Wr1Out (iout2,iout3,iout4,iout5,iout6,iout7,iout8, IEvent)

!***   End of Event
        CDate = ' '
        CTime = ' '
        CALL DATE_AND_TIME (CDATE,CTIME,CZONE,time_fields)
        If (NEVENT .GT. 1) CALL WRLOGO (IScren, IEVENT,NEVENT,NEVENT,IBar0,EstimateRemainingDuration,RemTime)
!      warning in number of times no convergence o convergence
        If (NrTimestepsNoConvergence .ge. 10 .and. iOut1 .ne.  0) then
            Write(IOUT1,703) IEvent, NrTimestepsNoConvergence
        Endif
  703   FORMAT(' In Event ',I5,' for ',I6,' timesteps no convergence in computations. This may produce strange results')
        If (NrTimestepsNegativeVolume .ge. 10 .and. iOut1 .ne.  0) then
            Write(IOUT1,704) IEvent, NrTimestepsNegativeVolume
        Endif
  704   FORMAT(' In Event ',I5,' for ',I6,' timesteps one or more open water nodes had a negative volume or area. ')


! *** Close files
#if (defined(SOBEK_PARALLEL))
! Unix version:
! output simulated period in balance file and close file
! close all output files for any event IEvent

      Call WriteBalance (Ievent, .false.)

      Call CloseGP (Iout1)
      Call CloseGP (Idebug)
      if (Ncboun .gt. 0) Call CloseGP (IOutCb)
      if (IOutAbrTot .gt. 0) Call CloseGP (IOutAbrTot)
!     Iout2-8
      If (ncvhg .gt. 0)  Call CloseGP(IOUT2)
      if (ncovhg .gt. 0) Call CloseGP(IOUT3)
      if (nckas .gt. 0)  Call CloseGP(IOUT4)
      if (ncow .gt. 0)   Call CloseGP(IOUT5)
      if (ncstru .gt. 0) Call CloseGP(IOUT6)
      if (ncboun .gt. 0) Call CloseGP(IOUT7)
      if (ncpluv .gt. 0) Call CloseGP(IOUT8)
!     His files
      Call Close3BHisFiles

      If (RunoffOutHis .eq. 0) then
          Call CloseGP(ioutpl)
      Else
          Call CloseDioPlt(Runoff_Out)
      Endif
      if (Iflzt .gt. 0) Call CloseGP (iflzt)
#else
!     pc version:
      if (EmulateUnixOnPC) then
! test UX
         Call WriteBalance (Ievent, .false.)

         Call CloseGP (Iout1)
         Call CloseGP (Idebug)
         if (Ncboun .gt. 0) Call CloseGP (IOutCb)
         if (IOutAbrTot .gt. 0) Call CloseGP (IOutAbrTot)
!        Iout2-8
         If (ncvhg .gt. 0)  Call CloseGP(IOUT2)
         if (ncovhg .gt. 0) Call CloseGP(IOUT3)
         if (nckas .gt. 0)  Call CloseGP(IOUT4)
         if (ncow .gt. 0)   Call CloseGP(IOUT5)
         if (ncstru .gt. 0) Call CloseGP(IOUT6)
         if (ncboun .gt. 0) Call CloseGP(IOUT7)
         if (ncpluv .gt. 0) Call CloseGP(IOUT8)
!        His files
         Call Close3BHisFiles

         If (RunoffOutHis .eq. 0) then
             Call CloseGP(ioutpl)
         Else
             Call CloseDioPlt(Runoff_Out)
         Endif
         if (Iflzt .gt. 0) Call CloseGP (iflzt)
      else
!     pc version: if last event, then close output files
        if (Ievent .eq. NEvent) then
           IF (NCPLUV .GT. 0 .and. RunoffOutHis .eq. 0) Call CloseGP (IOUTPL)
           IF (IFLZT .GT. 0)  Call CloseGP(IFLZT)
           If (ncvhg .gt. 0)  Call CloseGP(IOUT2)
           if (ncovhg .gt. 0) Call CloseGP(IOUT3)
           if (nckas .gt. 0)  Call CloseGP(IOUT4)
           if (ncow .gt. 0)   Call CloseGP(IOUT5)
           if (ncstru .gt. 0) Call CloseGP(IOUT6)
           if (ncboun .gt. 0) Call CloseGP(IOUT7)
           if (ncpluv .gt. 0) Call CloseGP(IOUT8)
           Call Close3BHisFiles
        endif
      endif

#endif

 9999 Continue

! *********************************************************************
! Close DIO Streams (on-line)
! *********************************************************************

     call CloseDioPlt(outPlt_OpenWater)
     call CloseDioPlt(inPlt_Rtc)
     call CloseDioPlt(inPlt_Salt)

! uit Sobek_3B: update AlreadySimulated
    if (associated(EventDuration) .and. NrSecsRai > 0) then
       AlreadySimulated = AlreadySimulated + Int (EventDuration(Ievent,6) / Float(timeSettings%timestepSize) * Float(NrSecsRai))
    endif
!   If (Crashed) goto 9999

    RRFinalizeEvent = 0
    If (Crashed) RRFinalizeEvent = -1

  return
  end function RRFinalizeEvent


! ************************************************************************************
!    RRFinalise
! ************************************************************************************

  Integer function RRFinalize (RR_RunId)

  Implicit none
  Integer  RR_RunId
  Integer  weekdy, idum

   call timstrt('RRFinalize',TimerRRFinalize)

#if (defined(SOBEK_PARALLEL))
!     Unix version
!
!  File Iout1 moet nu weer opnieuw geopend worden! voor wat extra afsluitende meldingen;
       Call SetLastLogFileName(RRNameFile)
       if (Iout1 .gt. 0) CALL OPENFL(IOUT1, RRNameFile,1,2)
#else
     !Get date and time
      CDate = ' '
      CTime = ' '
      CALL DATE_AND_TIME (CDATE,CTIME,CZONE,time_fields)
      If (TimeMessage) WRITE(*,'(A,A10)') ' After simulation ',CTIME

#endif

   if (EmulateUnixOnPC) then
! test UX versie
!  File Iout1 eerst weer opnieuw openen!
      Call SetLastLogFileName (RRNameFile)
      if (Iout1 .gt. 0) CALL OPENFL(IOUT1, RRNameFile,1,2)
! End test Unix
    endif

!*********************************************************************
!***  Einde Loop over alle buien in de reeks; afsluitende uitvoer
!*********************************************************************

! Restart file aan einde berekeningen, alleen bij berekening van 1 bui, niet bij een reeks
       If (Nevent .eq. 1) then
         Call OpenFl (IoutRestart, ConfFil_get_NAMFIL(42),2,2)
         If ( RestIo(2) .ne. 0 ) Then
! call INIT2 eerst!, voor niet de eerste tijdstap
           IEVENT=1
           IDUM = 2
!***       Determine day of the week  (0=zondag, 1=maandag etc)
           IDAYWK = WEEKDY (ConfArr_get_IYEAR(), ConfArr_get_iMonth(), ConfArr_get_IDAY())
           Call INIT2 (IDAYWK, IDUM, 0, 0)
!          write(*,*) ' write restart file einde berekeningen'
           If (Idebug .ne. 0)  WRITE(Idebug, *) ' write final restart file ', Conffil_Get_Namfil(42)
           Call WrRest (IoutRestart)
         Endif
         Call CloseGP(IoutRestart)
         OpenDAFileName = ConfFil_get_NamFil(123)
         if (OpenDAFileName .ne. '' .and. UseOpenDAFile) then
             Call OpenFl (OpenDAFileUnit, OpenDAFileName, 1,2)
             Call WriteOpenDAPaved      (OpenDAFileUnit)
             Call WriteOpenDAUnpaved    (OpenDAFileUnit)
             Call WriteOpenDAGreenhouse (OpenDAFileUnit)
             Call WriteOpenDAOpenWater  (OpenDAFileUnit)
             Call WriteOpenDASacramento (OpenDAFileUnit)
             Call WriteOpenDAHBV        (OpenDAFileUnit)
             Call WriteOpenDADNAM       (OpenDAFileUnit)
!            Call WriteOpenSCS          (OpenDAFileUnit)
!            Call WriteOpenDAWagmod     (OpenDAFileUnit)
!            Call WriteOpenDALGSI       (OpenDAFileUnit)
             Call WriteOpenDAWalrus     (OpenDAFileUnit)
             Call CloseGP(OpenDAFileUnit)
             call SetMessage(LEVEL_INFO, ' OpenDAFiles generated')
         endif
       Endif

!*** End of test (NEVENT>0)
!     ENDIF

      RemTime = '000:00:00'
      IF (NEVENT .GE. 1) CALL WRLOGO (IScren, NEVENT,NEVENT,NEVENT,IBar0,EstimateRemainingDuration,RemTime)

!uitvoer gesimuleerde periode in balansfile
#if (defined(SOBEK_PARALLEL))
!     Unix version: DO NOTHING
#else
!     Pc version: write balance file
      if (.not. EmulateUnixOnPc) Call WriteBalance (Nevent, .true.)
#endif

! Controller netjes afsluiten
      IF (RunSimultaneous) THEN
          TIMNEW = -1
! UNST-4751 test
          if (.not. dimr_mode) CALL STEPCT (TIMOLD, TIMNEW, IDCNT, ISTAT, InitMode, crashed)
          IF (ISTAT .LT. 0)  THEN
              WRITE (STRING,'(I3)') ISTAT
              call ErrMsgStandard (990, 0, 'Sobek_RR', STRING)
          ENDIF
          If (crashed) goto 9999

          if (.not. dimr_mode) CALL ENDCT (IDCNT, ISTAT)
          IF (ISTAT .LT. 0)  THEN
              WRITE (STRING,'(I3)') ISTAT
              call ErrMsgStandard (990, 0, 'Sobek_RR', STRING)
          ENDIF
      ENDIF


 9999 Continue
       if (.not. dimr_mode .and. crashed)  Call CrashCt (IdCnt, .false. )

! alleen bij losse bui problemen volumecheck melden
       if (Nevent .eq. 1) then
         if (NrCapsimErrors .gt. 5) then
            call ErrMsgStandard (978, NrCapsimErrors,' In total', ' times a negative volume was returned from Alterra-routine CAPSIM')
         Endif
         if (NrVolumeCheck .gt. 1 .and. Iout1 .ne. 0 .and. Nevent .eq. 1) then
            call ErrMsgStandard (978, NrVolumeCheck, ' Simple Volume Check has been active ', ' timesteps')
         endif
       endif

!! HarmonIT - TestSaveState
!     If (TestSaveState) Call RRStatesDestroy('RRStates')

! close some NetCdfFiles (daily output only, others are already properly closed)
      if (GenerateNetCdfOutput) then
         if (iNetCdfFile(BndFloTotNetCdfFileNr) .gt. 0)  ierr = nf90_close(iNetCdfFile(BndFloTotNetCdfFileNr))
         if (iNetCdfFile(NWRWSysNetCdfFileNr) .gt. 0)    ierr = nf90_close(iNetCdfFile(NWRWSysNetCdfFileNr))
      endif
! Afsluitmelding in logfile schrijven
      String = ' Succesfull logout of Sobek_RR '
      OutputString = TranslateString(LanguageHandle, String)
      RRFinalize = RRWriteReturnCodeFile(RR_Runid)

! extra outputfile voor Rijnland-ABR (rr-ready); iflrtn can be used since return code file has just been written before
      Call Openfl (iflrtn, Conffil_get_Namfil(99),1,2)
      write(iflrtn,*) OutputString
      Call CloseGP(iflrtn)

! deallocate arrays / free memory
      Call Paved_DeAllocateArrays
      Call UnPaved_DeAllocateArrays
!      Call Greenhouse_DeAllocateArrays
!      Call OpenWater_DeAllocateArrays
      Call Structure_DeAllocateArrays
      Call Boundary_DeAllocateArrays
      Call NWRW_DeAllocateArrays
!      Call RWZI_DeAllocateArrays
!      Call Sacramento_DeAllocateArrays
      Call Industry_DeAllocateArrays
      Call RRConnection_DeAllocateArrays
      Call RRBifurcation_DeAllocateArrays
!      Call Link_DeAllocateArrays
#if (!defined(HAVE_CONFIG_H))
      Call PmInterFace_DeallocateArrays
#endif

#if (defined(SOBEK_PARALLEL))
!     Unix version
!     Parallell draaien van de buien op Unix: uitvoerfiles aan elkaar plakken
!
#else
!     Pc version: do nothing
#endif


     RRFinalize = 0
     if (.not. dimr_mode) then
        If (Crashed) RRFinalize = -1
     endif
     call timstop(TimerRRFinalize)

  end function RRFinalize

! ************************************************************************************
!    RR-WriteReturnCode file
! ************************************************************************************

  Integer function RRWriteReturnCodeFile(RR_Runid)

  Implicit none
  Integer RR_RunId
  Logical FileOpened

! Afsluitmelding in logfile schrijven
      if (iOut1 .ne. 0) then
         call SetMessage(LEVEL_INFO, trim(OutputString))
         Call CloseGP (IOUT1)
      Endif
      if (iflrtn .gt. 0) then
         WRITE (Iflrtn,'(I5)') 0
         Call CloseGP (Iflrtn)
      Endif
! n.a.v. problemen mbt Fort.10 ipv sobek_3b.rtn
      Inquire (FILE = 'sobek_3b.rtn', Opened = FileOpened)
      if (.not. FileOpened)  Call Openfl(iflrtn, 'sobek_3b.rtn', 1,2)
      WRITE (IFLRTN,'(I5)') 0
      Call CloseGP(iFlRtn)

      RRWriteReturnCodeFile = 0

  return
  end function RRWriteReturnCodeFile


! ************************************************************************************
!    RRGetCurrentTime, RRGetInputTime
! ************************************************************************************

  function RRGetCurrentTime(RR_RunId) result(currentTime)
      integer RR_RunId
      double precision:: currentTime
      currentTime = ModJulianTime
  end function RRGetCurrentTime


  function RRGetInputTime(RR_RunId) result(inputTime)
      integer RR_RunId
      double precision:: inputTime
      inputTime = ModJulianTime + JulianTimestep
  end function RRGetInputTime

  function RRGetTimeHorizon(RR_RunId, startTime, endTime) result(retVal)
      integer retVal
      integer RR_RunId
      double precision :: startTime, endTime
      double Precision, external :: modified_julian_fromJulian
      startTime = Modified_Julian_fromJulian(JulianStartDate)
      endTime = startTime + (Lasttm - Frsttm + 1) * JulianTimestep
      retVal = 0
  end function RRGetTimeHorizon

  function RRGetDeltaT(RR_RunId) result(deltaTAsMJD)
      double precision:: deltaTAsMJD
      integer RR_RunId
      deltaTAsMJD = JulianTimestep
  end function RRGetDeltaT

!!---------------------
!! Added for OpenMI
!!---------------------

   Subroutine InitOutOpenWaterLevels(RR_RunId)

      use wl_open_mi_support
      use RR_open_mi_support

      IMPLICIT NONE

      integer RR_RunId
      Logical :: Success
      Integer :: teller, OesOWLevelsElmset, OesOwLevelsQuant

      Character(Len=CharIdLength), dimension(:), pointer  :: openMINode

      if (OesInitialized() ) then
         success = DH_AllocInit (NCOW, openMINode, ' ')
         If (.not. Success) call ErrMsgStandard (981, 0, ' Error alloc.openMI-arrs in subroutine ', &
                                              ' InitOutOpenWaterLevels'  )
         Do teller = 1, ncOw
            openMINode(teller) = Id_Nod(OWNAM(teller))
         Enddo
         oesOWLevelsElmset = OesElmSetFindOrCreate(RR_RunId,RROpenwaterElmSet,openMINode)
         If (oesOWLevelsElmset > 0) then
            OesOWLevelsQuant=OesExchItemCreate(RR_RunId,RROpenwaterlevel,RROpenwaterElmSet,oes_providing)
         Endif
         DeAllocate(openMINode)
      Endif

   Return
   End subroutine InitOutOpenWaterLevels


   Subroutine InitOutUnpavedResults(RR_RunId)

      use wl_open_mi_support
      use RR_open_mi_support

      IMPLICIT NONE

      integer RR_RunId
      Logical :: Success
      Integer :: teller, OesGWLevelsElmset, OesGwLevelsQuant

      Character(Len=CharIdLength), dimension(:), pointer  :: openMINode

      if (OesInitialized() ) then
         success = DH_AllocInit (NCOVHG, openMINode, ' ')
         If (.not. Success) call ErrMsgStandard (981, 0, ' Error alloc.openMI-arrs in subroutine ', &
                                              ' InitOutUnpavedGWLevels'  )
         Do teller = 1, ncOvhg
            openMINode(teller) = Id_Nod(UNPNAM(teller))
         Enddo
         oesGWLevelsElmset = OesElmSetFindOrCreate(RR_RunId,RRUnpavedElmSet,openMINode)
         if (oesGWLevelsElmset > 0) then
            OesGWLevelsQuant=OesExchItemCreate(RR_RunId,RRGroundwaterlevel,RRUnpavedElmSet,oes_providing)
            OesGWLevelsQuant=OesExchItemCreate(RR_RunId,RRGwRecharge,RRUnpavedElmSet,oes_providing)
            OesGWLevelsQuant=OesExchItemCreate(RR_RunId,RRStoragecoeff,RRUnpavedElmSet,oes_providing)
         Endif
         DeAllocate(openMINode)
      Endif

   Return
   End subroutine InitOutUnpavedResults


   Subroutine InitOutStructureFlows(RR_RunId)

      use wl_open_mi_support
      use RR_open_mi_support

      IMPLICIT NONE

      integer RR_RunId
      Logical :: Success
      Integer :: teller, OesOutflowsElmset, OesOutflowQuant

      Character(Len=CharIdLength), dimension(:), pointer  :: openMINode

      if (.not. OesInitialized() ) return

      success = DH_AllocInit (NCSTRU, openMINode, ' ')
      If (.not. Success) call ErrMsgStandard (981, 0, ' Error alloc.openMI-arrs in subroutine ', ' InitOutStructureFlows'  )

      Do teller = 1, ncStru
         openMINode(teller) = Id_Nod(STRNAM(teller))
      Enddo
      oesOutflowsElmset = OesElmSetFindOrCreate(RR_RunId,RRStructureElmSet,openMINode)
      if (oesOutflowsElmset > 0) then
         OesOutflowQuant=OesExchItemCreate(RR_RunId,RRFlow,RRStructureElmSet,oes_providing)
      endif
      Deallocate(openMINode)

   Return
   End subroutine InitOutStructureFlows


   Subroutine InitOutBoundaryFlows(RR_RunId)

      use wl_open_mi_support
      use RR_open_mi_support

      IMPLICIT NONE

      integer RR_RunId
      Logical :: Success
      Integer :: teller, OesOutflowsElmset, OesOutflowQuant

      Character(Len=CharIdLength), dimension(:), pointer  :: openMINode

      if (.not. OesInitialized() ) return

      success = DH_AllocInit (NCBOUN, openMINode, ' ')
      If (.not. Success) call ErrMsgStandard (981, 0, ' Error alloc.openMI-arrs in subroutine ', ' InitOutBoundaryFlows'  )

      Do teller = 1, ncBoun
         openMINode(teller) = Id_Nod(BNDNAM(teller))
      Enddo
      oesOutflowsElmset = OesElmSetFindOrCreate(RR_RunId,RRBoundaryElmSet,openMINode)
      if (oesOutflowsElmset > 0) then
         OesOutflowQuant=OesExchItemCreate(RR_RunId,RRFlow,RRBoundaryElmSet,oes_providing)
      endif
      Deallocate(openMINode)

   Return
   End subroutine InitOutBoundaryFlows



   Subroutine InitOutNWRWFlows(RR_RunId)

      use wl_open_mi_support
      use RR_open_mi_support

      IMPLICIT NONE

      integer RR_RunId
      Logical :: Success
      Integer :: teller, OesOutflowsElmset, OesOutflowQuant

      Character(Len=CharIdLength), dimension(:), pointer  :: openMINode

      if (.not. OesInitialized() ) return

      success = DH_AllocInit (NCPLUV, openMINode, ' ')
      If (.not. Success) call ErrMsgStandard (981, 0, ' Error alloc.openMI-arrs in subroutine ', ' InitOutNWRWFlows'  )

      Do teller = 1, ncPluv
         openMINode(teller) = Id_Nod(PLVNAM(teller))
      Enddo
      oesOutflowsElmset = OesElmSetFindOrCreate(RR_RunId,RRNWRWElmSet,openMINode)
      if (oesOutflowsElmset > 0) then
         OesOutflowQuant=OesExchItemCreate(RR_RunId,RRFlow,RRNWRWElmSet,oes_providing)
      endif
      Deallocate(openMINode)

   Return
   End subroutine InitOutNWRWFlows

!> Returns Elementset and Quantity integer handles. Handles can be used
!! to obtain data from RR kernel by calling OMI_RR_GetDataByIntId
function  OMI_RR_GetIntHandles(QuantityID, ElementsetID, ElementsetHandle, QuantityHandle) result(success)

    use RR_open_mi_support

    IMPLICIT NONE

    ! return value
    logical  :: success

    !arguments
    character(Len=*) , intent(in)     :: QuantityID       !< input: quant. identifier
    character(Len=*) , intent(in)     :: ElementsetID     !< input: elem.set identifier
    integer          , intent(out)    :: ElementsetHandle !< output: elem.set integer handle
    integer          , intent(out)    :: QuantityHandle   !< output: quant. integer handle

    ! locals

    success = .true.

    ! Open water element set
    if (ElementsetID == RROpenwaterElmSet) then
        ElementsetHandle = RRiOpenwaterElmSet
        ! Subdivide for various variables
        if (QuantityID == RROpenwaterlevel) then
            QuantityHandle = RRiOpenwaterlevel
        ! Something is wrong
        else
            success = .false.
        endif

    ! Unpaved Results
    elseif (ElementsetID == RRUnpavedElmSet) then
        ElementsetHandle = RRiUnpavedElmSet
        ! Subdivide for various variables
        if (QuantityID == RRGroundwaterlevel) then
            !RR groundwaterlevels
            QuantityHandle = RRiGroundwaterlevel
        elseif (QuantityID == RRGWRecharge) then
            !RR groundwater recharge
            QuantityHandle = RRiGWRecharge
        elseif (QuantityID == RRUnsatZoneContent) then
            ! RR Unsat Zone Content
            QuantityHandle = RRiUnsatZoneContent
        elseif (QuantityID == RRStoragecoeff) then
            ! RR storage coefficient
            QuantityHandle = RRiStoragecoeff
        ! Something is wrong
        else
            success = .false.
        endif

    ! Structure Flows
    elseif (ElementsetID == RRStructureElmSet) then
        ElementsetHandle = RRiStructureElmSet
        ! Subdivide for various variables
        if (QuantityID == RRFlow) then
            QuantityHandle = RRiFlow
        ! Something is wrong
        else
            success = .false.
        endif

    ! Boundary Flows
    elseif (ElementsetID == RRBoundaryElmSet) then
        ElementsetHandle = RRiBoundaryElmSet
        ! Subdivide for various variables
        if (QuantityID == RRFlow) then
            QuantityHandle = RRiFlow
        ! Something is wrong
        else
            success = .false.
        endif

    ! NWRW Flows
    elseif (ElementsetID == RRNWRWElmSet) then
        ElementsetHandle = RRiNWRWElmSet
        ! Subdivide for various variables
        if (QuantityID == RRFlow) then
            QuantityHandle = RRiFlow
        ! Something is wrong
        else
            success = .false.
        endif

    ! Sacramento Results
    elseif (ElementsetID == RRSacramentoElmSet) then
        ElementsetHandle = RRiSacramentoElmSet
        ! Subdivide for various variables
        if (QuantityID == RRSacrUZTWC) then
            QuantityHandle = RRiSacrUZTWC
        elseif (QuantityID == RRSacrUZFWC) then
            QuantityHandle = RRiSacrUZFWC
        elseif (QuantityID == RRSacrLZTWC) then
            QuantityHandle = RRiSacrLZTWC
        elseif (QuantityID == RRSacrLZFPC) then
            QuantityHandle = RRiSacrLZFPC
        elseif (QuantityID == RRSacrLZFSC) then
            QuantityHandle = RRiSacrLZFSC
        elseif (QuantityID == RRSacrBaseFlow) then
            QuantityHandle = RRiSacrBaseFlow
        elseif (QuantityID == RRSacrSurfFlow) then
            QuantityHandle = RRiSacrSurfFlow
        elseif (QuantityID == RRSacrTotalRunoff) then
            QuantityHandle = RRiSacrTotalRunoff
        ! Something is wrong
        else
            success = .false.
        endif
    ! Something is wrong
    else
        success = .false.
    endif

end function

!> If success, function returns Values array of length ElementCount
!! for specific elementset and quantity handle combination
function OMI_RR_GetDataByIntId(QuantityHandle, ElementsetHandle, ElementCount, Values) result(success)

    use wl_open_mi_support
    use RR_open_mi_support

    IMPLICIT NONE

    ! return value
    logical  :: success

    !arguments
    integer,          intent(in)     :: QuantityHandle   !< quant. handle
    integer,          intent(in)     :: ElementsetHandle !< elem.set. handle
    integer,          intent(in)     :: ElementCount     !< #elems in elementset
    double precision, intent(out), &
            dimension(1:ElementCount) :: Values          !< values in elemenset

    ! locals
    Double precision, dimension(:), pointer :: openMIValues
    Double precision                        :: RInRi
    Integer                                 :: Inr, Iptyp, IpOpp, Iplv2, Iplv3, j

    Values  = 0
    success = .true.

    select case (ElementsetHandle)
    ! Open water element set
    case (RRiOpenwaterElmSet)
        success = RR_GetOpenWaterDataByIntId(QuantityHandle, ElementCount, Values)

    ! Unpaved Results
    case (RRiUnpavedElmSet)
        success = RR_GetUnpavedDataByIntId(QuantityHandle, ElementCount, Values)

    ! Unpaved Results
    case (RRiPavedElmSet)
        success = RR_GetPavedDataByIntId(QuantityHandle, ElementCount, Values)

    ! Greenhouse Results
    case (RRiGreenhouseElmSet)
        success = RR_GetGreenhouseDataByIntId(QuantityHandle, ElementCount, Values)

    ! WWTP Results
    case (RRiWWTPElmSet)
        success = RR_GetWWTPDataByIntId(QuantityHandle, ElementCount, Values)

    ! Link Results
    case (RRiLinkElmSet)
        success = RR_GetLinkDataByIntId(QuantityHandle, ElementCount, Values)

    !Balance Results per node
    case (RRiBalanceNodeElmSet)
        success = RR_GetNodeBalanceDataByIntId(QuantityHandle, ElementCount, Values)

    case (RRiBalanceModelElmSet)
    !Balance Results for whole model, ElementCount should be 1!
        if (ElementCount == 1) then
            success = RR_GetBalanceDataByIntId(QuantityHandle, ElementCount, Values(1))
        else
            success = .false.
        endif

    ! Structure Flows
    case (RRiStructureElmSet)
        ! Subdivide for various variables
        select case (QuantityHandle)
        case (RRiFlow)
            Values = QStru
        ! Something is wrong
        case default
            success = .false.
        end select

    ! Boundary Flows
    case (RRiBoundaryElmSet)
        ! Subdivide for various variables
        select case (QuantityHandle)
        case (RRiFlow)
            Values = QBnd
        ! Something is wrong
        case default
            success = .false.
        end select

    ! NWRW Flows
    case (RRiNWRWElmSet)
        ! Subdivide for various variables
        select case (QuantityHandle)
        case (RRiFlow)
            success = DH_AllocInit (NCPLUV, openMIValues, 0.0D+00)
            IF ( success ) THEN
                IF (NCPLUV .GT. 0) THEN
                    ! Pluvius/NWRW nodes
                    Do inr=1,NCPluv
                        RinRi = Dwa(inr) + QinPluv(inr)   ! DWA and RR-Rural inflow
                        ! add sewer inflow from 3*4 standard types
                        Iplv2 = IndikP(Inr)
                        Do IPTYP=1,NPTYP
                            Do IPOPP=1,NPOPP
                            RINRI = RINRI + AREAPV(INR,IPTYP,IPOPP) * INPR(IPLV2,IPTYP,IPOPP)/ timeSettings%timestepSize
                            Enddo
                        Enddo
                        ! add sewer inflow from special areas
                        if (NrSpecialNwrwAreas(inr) .gt. 0) then
                            Do j=1,NrSpecialNwrwAreas(Inr)
                                iplv3  = SpecialInDikP(inr,j)
                                RINRI = RINRI + SpecialNwrwAreas(inr,j) * SpecialINPR(IPLV3)/ timeSettings%timestepSize
                            Enddo
                        Endif
                        OpenMIValues(inr) = RinRi
                    EndDo
                Endif
            Endif
            Values = OpenMIValues
            Deallocate(OpenMIValues)
        ! Something is wrong
        case default
            success = .false.
        end select

    ! Sacramento nodes
    case (RRiSacramentoElmSet)
        success = RR_GetSacrDataByIntId(QuantityHandle, ElementCount, Values)

    ! HBV nodes
    case(RRiHBVElmSet)
    success = RR_GetHBVDataByIntId(QuantityHandle, ElementCount, Values)

    ! Something is wrong
    case default
        success = .false.
    end select

end function


END module RRModule



