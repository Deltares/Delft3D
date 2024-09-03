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

 !TODO: Add SetNnode to modelApi (=maximaal aantal knopen in RR-model, NNOD)

! Last changed
! by:               $Author:: Schrier           $
! at:               $Modtime:: 15-08-97 4:09p   $
!
! current revision: $Revision:: 10              $


 module Network

  ! use
  use Conf_Fil
  use Messages
  use PMInterface
  use ReadLib

  ! Contains all bounds on the node-types in Delft-3B
  ! and all control-variables


  IMPLICIT NONE
    logical, public :: ini_switch

  ! variables

  ! *** Control data
  ! - iDebug      : integer die aangeeft of debug info weggeschreven wordt (1)
  ! - timeSettings.timestepSize
  !               : timestep size in seconds
  ! - ExchangeSetting : timestepsize
  ! -                 : OptionString
  ! -                 : Option
  ! - PRCKAS      : percentage waarbeneden gebruik gekort wordt
  ! - IOPT1       : globale outputopties
  ! -               ( ,1) = per tijdstap per gebeurtenis
  ! -               ( ,2) = per gebeurtenis maxima etc.
  ! -               ( ,3) = statistiek
  ! -               ( ,4) = event nr.
  ! -               ( ,5) = op scherm ja/nee
  ! - IOPT2       : outputopties peilen gws en ow: 0=NAP, 1=maaiveld of ref.peil
  ! -               ( ,1) = voor gws
  ! -               ( ,2) = voor ow
  ! - outputUnits : output units per knooptype
  ! -               voor alle 7 knooptypen eenheden
  ! -               en eenheden voor balans en zoutconcentraties per knoop
  ! - APESLN      : uitvoer in dirfile.lst met @ en spatie of niet (1=ja)
  ! - UPDSTW      : update stuw instelling mbt bovenstrooms streefpeil (1=ja)
  ! - LNACHT      : per uur indicator ja/nee
  ! - timeSettings%weekendHrsAsNight
  !               : in weekend nachtstroomtarief ja/nee (weekend=zaterdag/zondag)
  ! - iSTUURM     : met of zonder stuurmodule draaien
  ! - ITMSTU      : aantal rekentijdstappen per call stuurmodule
  ! - STRFIL      : naam stuurfile tbv D3B2SBK
  ! - islcmp      : salt computations
  ! - iflzt       : salt output in ASCII-file
  ! - SLTRAI      : zoutconcentratie regenval (vast voor alle knopen)
  ! - RESTIO (1)  : inlezen restart file      (1=ja,0=nee)
  ! - RESTIO (2)  : wegschrijven restart file (1=ja)
  ! - RESTIO (3)  : wegschrijven TMP files voor Delft-3b Postprocessing; per 28/3/99 altijd=0, direct uitvoer in 3B genereren
  ! - RESTIO (4)  : wegschrijven binaire input file (1=ja,0=nee)
  ! - RESTIO (5)  : inlezen binaire file (1=ja)
  ! - RESTIO (6)  : nr seconds since start voor schrijven 1e tussentijdse restart file
  ! - RESTIO (7)  : nr seconds since start voor schrijven 2e tussentijdse restart file
  ! - BINFIL      : naam binaire file incl. pad
  !                 TRUE = in Pluvius file, FALSE = in Qrand file
  ! - HREVAP (1,2): verdamping gewassen en openwater van uur (1) tot uur (2)
  ! - TMEVAP      : factor bij conversie m/s (voor 24 uur) naar m/s voor uren dat
  ! - TMIRRI      : factor bij conversie m/s (voor 24 uur) naar m/s voor uren dat
  ! - IMLSTP      : indicator maalstop file gebruikjt ja/nee (1=ja, 0=nee)
  ! - IModFlow    : indicator ModFlow file gebruikt ja/nee (1=ja, 0=nee)
  ! - IWLMModule  : indicator WLM ja/nee (1=ja, 0=nee)
  ! - IWQModule   : indicator WQ module gebruikt ja/nee (1=ja, 0=nee)
  ! - MSFIL       : maalstop datafile
  ! - MSFIL2      : 2e file naar maalstop
  ! - IOhis       : output in MPX/HIS format
  ! - IDEFLT      : gebruik default evapor files etc: 1=ja, 0=nee

  ! types

  type outpUnits
    Integer pavedStorage
    Integer pavedFlow
    Integer unpavedGroundwater
    Integer unpavedFlow
    Integer greenhouseStorage
    Integer greenhouseFlow
    Integer openwaterLevel
    Integer openwaterExceedance
    Integer structureFlow
    Integer boundaryFlow
    Integer nwrwStorage
    Integer nwrwFlow
    Integer balance
    Integer saltConcentration
    Integer RwziFlow
    Integer IndustryFlow
    Integer SacramentoFlow
    Integer SacramentoStorage
  end type outpUnits

  type TimeSetting
    Integer timestepSize
    Real    timeWeightFactor
    Integer nightFromHr
    Integer nightToHr
    Logical weekendHrsAsNight
    Integer EvaporationFromHr
    integer EvaporationToHr
    Integer IrrigationFromHr
    integer IrrigationToHr
    integer IrrigationStartDay
    integer IrrigationStartMonth
    integer IrrigationEndDay
    integer IrrigationEndMonth
    Logical StartTimeFromEvent
    integer StartYear
    integer StartMonth
    integer StartDay
    Integer StartHour
    Integer StartMinute
    Integer StartSecond
    integer EndYear
    integer EndMonth
    integer EndDay
    Integer EndHour
    Integer EndMinute
    Integer EndSecond
    Logical Output2CFUserDefinedPeriod
    integer OutputStartYear
    integer OutputStartMonth
    integer OutputStartDay
    Integer OutputStartHour
    Integer OutputStartMinute
    Integer OutputStartSecond
    integer OutputEndYear
    integer OutputEndMonth
    integer OutputEndDay
    Integer OutputEndHour
    Integer OutputEndMinute
    Integer OutputEndSecond
    Double Precision  JulianStartOutput
    Double Precision  JulianEndOutput
    Integer OutputEvent
    Integer CurrentTimeStep
  end type TimeSetting

  type ExchangeSetting
    Integer timestepSize
    Integer      option
    character(len=20) StringOption
  end type ExchangeSetting

  ! variables
  type (outpUnits)  outputUnits
  type (TimeSetting) timeSettings
  type (ExchangeSetting) ExchangeSettings

  ! output options
  Integer, Pointer, Save::  OutputPaved(:), OutputUnpaved(:), OutputGreenhouse(:), OutputOpenWater(:), &
                OutputStructure(:), OutputBoundary(:), OutputNWRW(:), OutputBalance(:), &
                OutputSalt(:), OutputWWTP(:), OutputIndustry(:), OutputSacramento(:), &
                OutputCell(:), OutputLink(:), OutputRRRUnoff(:)

  Real          LargeBalanceErrorPercentage
  Logical       ExtendedBalanceOutput !als false dan uitvoer backwards compatible = niet compleet
  Logical       APESLN, UPDSTW
  Logical       LWKND
  Logical       qBPlv, qBPlv2, RunSimultaneous
  Logical       IOHIS
  LOGICAL       LNACHT(0:23)

  Integer       ITMSTU, iQCBND
  Integer       RESTIO(7)
  Integer       optionsBoundary
  Integer       HREVAP(2)
  Integer       IMLSTP, IModFlow, IWlmModule, IWQModule
  Integer       IDEFLT, ISTUURM, iRchID
  INTEGER       ISLCMP, iflzt
  INTEGER       IOPT1(5), IOPT2(2), OutputAtTimestep, OutputAtTimestepOption
!  Integer, allocatable :: iOptNd(:, :)
  Logical       CaseSensitive


  ! ***  Data Openwater
  ! ***  NOW   = max. aantal open water knopen
  ! ***  NCOW  = act. aantal open water knopen
  ! ***  NCOWRain = aantal open water knopen alleen Precip/Evap
  Integer       nOW, ncOW, nOwRain, ncOWRain

  ! *** Data verhard gebied
  ! ***  NVHG  = max. aantal verharde gebieden
  ! ***  NCVHG = act. aantal verharde gebieden
  Integer       nVhg, ncVhg


  Real          tmEvap, TmIrri
  Real          SLTRAI
  REAL          PRCKAS
  Logical       DetailedVolumeCheckMessages

  Character(FilCharIdLength)  STRFIL
  Character(FilCharIdLength)  BINFIL
  CHARACTER(FilCharIdLength)  CASENM
  CHARACTER(FilCharIdLength)  MSFIL, MSFIL2

  ! Binary Input/output record 2
  ! ***  NCNODE = act. aantal nodes (knopen)
  ! ***  NCLINK = actual number of links
  ! ***  NKIND = act. aantal type knopen
  ! ***  NCOVHG = act. aantal onverharde gebieden
  ! ***  NCKAS = act. aantal kasgebieden
  ! ***  NCSTRU = max. aantal structures (kunstwerken)
  ! ***  NCBOUN = act. aantal boundaries
  ! ***  NCPLUV = act. aantal Pluvius knopen
  ! ***  nrPumpNodes
  ! ***  nrWeirNodes
  ! ***  nrWeirNodes
  ! ***  nrManningNodes
  ! ***  nrQhRelationNodes
  ! ***  NrCulverts = act. aantal Cluverts
  ! ***  NcRwzi = act. aantal RWZI's
  ! ***  NcIndus = act. aantal industrie knopen
  ! ***  NcSacr  = act. aantal Sacramento knopen
  ! ***  NcConn  = act. aantal RRConnection knopen (confluence)
  ! ***  NcBifur = act. aantal RRBifurcations
  Integer ncNode, ncLink, nKind
  Integer ncOvhg, ncKas, ncStru
  Integer ncBoun, ncPluv, NcPluvDwa, nrPumpNodes
  Integer nrWeirNodes, nrGateNodes, nrManningNodes
  Integer nrQhRelationNodes
  Integer nrCulverts
  Integer NcRwzi, NcIndus, NcSacr, NcConn, NcBifur, NcCell, &
          NcRRRunoff, NcRRRunoffExternal, NcRRRunoffHBV, NcRRRunoffSCS, NcRRRunoffNAM, &
          NcRRRunoffLGSI, NcRRRunoffWagMod, NcRRRunoffWalrus

  ! maximum values of variables
  ! ***  NNOD = max. aantal nodes (knopen)
  ! ***  NKND = max. aantal type knopen
  ! ***  NTIM  = max. aantal tijdstappen
  ! ***  NSLT  = max. aantal bakjes zoutnetwerk
  ! ***  NEVNT = max. aantal events (gebeurtenissen)
  ! ***  NVAL  = max. aantal interpolatievalues (6)
  ! ***  NCSALT  = act. aantal bakjes zoutnetwerk
  ! ***  NEVENT = act. aantal events (gebeurtenissen) from rainfall file
  ! ***  NEVENTRunoff = idem from runoff file
  ! ***  NEVENTTemperature = idem from temperature file

  Integer nNod, nKnd, nTim, nSlt, nEvnt, nVal
  integer, save      :: curNod = 0
  INTEGER NCSALT, nEvent, NEventRunoff, NEventTemperature

  ! ***  NLNK = max. aantal links (takken)
  Integer nLnk


  ! *** Data boundaries
  ! ***  NBND = max. aantal boundaries
  ! ***  NSOBK= max. aantal SOBEK tijdseries voor boundaries
  ! ***  NCSOBK= act. aantal SOBEK tijdseries voor boundaries
  Integer nBnd, nSobk
  Integer ncSobk

  ! ***  Data kassen
  ! ***  NKAS = max. aantal kasgebieden
  ! ***  NKKL = max. aantal kasklassen
  ! ***  NCKKL = act. aantal kasklassen
  Integer nKas, nKkl
  Integer nckkl

  ! ***  Data Meteo + gewassen
  ! ***  NMET = max. aantal meteostations
  ! ***  NCMET = act. aantal meteostations
  Integer nMet
  Integer ncMet

  ! ***  Data NWRW (Pluvius)
  ! ***  NPLV = max. aantal Pluvius knopen
  ! ***  NPLV2= max. aantal verschillende Pluvius knopen (na indikken)
  ! ***  NPOPP= max. aantal typen Pluvius oppervlakken
  !              (hellend, vlak, uitgestrekt)
  ! ***  NPTP= max. aantal soorten Pluvius oppervlakken
  !              (gesloten verhard, open verhard, dak, onverhard)
  ! ***  NCPLV2= act. aantal verschillende Pluvius knopen (na indikken)
  ! ***  NPTYP= act. aantal soorten Pluvius oppervlakken
  !              (gesloten verhard, open verhard, dak, onverhard)
  Integer nPlv, nPlv2, nPlv3, nPOpp, nPTp
  Integer ncPlv2, ncPlv3, nPTyp


  ! ***  Structures data
  ! ***  NSTR = max. aantal structures (kunstwerken)
  ! ***  NVAL3 = max. aantal structure parameters (14)
  Integer nStr, nVal3

  ! *** Data onverharde gebieden
  ! ***  NOVH = max. aantal onverharde gebieden
  ! ***  NVAL2 = max. aantal bodemtypen etc. (BERGCOEF data)
  Integer nOvh, nVal2

  ! *** Data RWZI's
  ! ***  NRWZI = max. aantal RWZI
  Integer NRwzi

  ! *** Data Industrie
  ! ***  NIndus = max. aantal industrie knopen
  Integer NIndus

  ! *** Data Sacramento
  ! ***  NSacr  = max. aantal Sacramento knopen
  Integer NSacr
! RR Connection nodes, bifurcation nodes
  Integer NConn, NBifur

  ! *** Data cellen
  ! ***  NCel   = max. aantal Cell nodes
  Integer NCel

  ! *** Data RRRunoff
  ! ***  NRRRunoff = max. aantal RRRunoff knopen
  Integer NRRRunoff

 contains



  subroutine Network_readAscii(fileHandle, MeteoNetCdfInput)

    integer fileHandle, in
    logical MeteoNetCdfInput
    integer iDebug
    Character(FilCharIdLength) string
    INTEGER POS1, IECODE, Idum

    Character(len=1) quote, klteken, slash, puntkomma, dubbelpunt, teken

    klteken   = '<'
    slash     = '/'
    puntkomma = ';'
    dubbelpunt= ':'
    quote     = ''''

    !set default values control data

    Call Network_initpart1

    VersionNumberBinFile = '3.210.09 '
    in = fileHandle

!C *********************************************************************
!C *** Initialisatie
!C ********************************************************************
!lees enkele van de volgende records uit de Delft_3B.INI file in:


      DetailedVolumeCheckMessages = .false.
  101 READ(IN,'(A100)',END=150,ERR=150,IOSTAT=IECODE)  STRING

      CALL UPPERC (STRING)
      CALL CHRTRIM (STRING,' ')
      POS1 = INDEX(STRING, '=')
      IF (POS1 .GT. 0) Then
          IF (STRING(1:POS1-1) .EQ. 'TIMEWEIGHTFACTOR')  THEN
             READ(STRING(POS1+1:),*) timeSettings%timeweightFactor
          ELSEIF (STRING(1:POS1-1) .EQ. 'TIMESTEPSIZE')  THEN
             READ(STRING(POS1+1:),*) timeSettings%timestepSize
          ELSEIF (STRING(1:POS1-1) .EQ. 'PERIODFROMEVENT')  THEN
             READ(STRING(POS1+1:),*) idum
             IF (IDUM .EQ. 0)  then
                timeSettings%StartTimeFromEvent = .false.
             else
                timeSettings%StartTimeFromEvent = .true.
             endif
          ELSEIF (STRING(1:POS1-1) .EQ. 'STARTTIME')  THEN
! format van periode is of 'yyyy/mm/dd;hh:mm:ss'
!           Eerst even de /, ; en : verwijderen om dan free format te kunnen lezen
            Do idum=pos1, FilCharIdLength
               teken = String(idum:idum)
               if (teken .eq. dubbelpunt .or. teken .eq. puntkomma .or. &
                    teken .eq. quote .or. teken .eq. slash) String(idum:idum) = ' '
            Enddo
            Read (String(Pos1+1:),*) timeSettings%Startyear, timeSettings%StartMonth, timeSettings%StartDay, &
                                     timeSettings%StartHour, timeSettings%StartMinute, timeSettings%StartSecond
          ELSEIF (STRING(1:POS1-1) .EQ. 'ENDTIME')  THEN
! format van periode is of 'yyyy/mm/dd;hh:mm:ss'
!           Eerst even de /, ; en : verwijderen om dan free format te kunnen lezen
            Do idum= Pos1, FilCharIdLength
               teken = String(idum:idum)
               if (teken .eq. dubbelpunt .or. teken .eq. puntkomma .or. &
                    teken .eq. quote .or. teken .eq. slash) String(idum:idum) = ' '
            Enddo
            Read (String(Pos1+1:),*) timeSettings%Endyear, timeSettings%EndMonth, timeSettings%EndDay, &
                                     timeSettings%EndHour, timeSettings%EndMinute, timeSettings%EndSecond

          ELSEIF (STRING(1:POS1-1) .EQ. 'OUTPUT2CF_USERDEFINEDPERIOD')  THEN
             READ(STRING(POS1+1:),*) idum
             IF (IDUM .EQ. 0)  then
                timeSettings%Output2CFUserDefinedPeriod = .false.
             else
                timeSettings%Output2CFUserDefinedPeriod = .true.
             endif
          ELSEIF (STRING(1:POS1-1) .EQ. 'OUTPUT2CF_STARTTIME')  THEN
! format van periode is of 'yyyy/mm/dd;hh:mm:ss'
!           Eerst even de /, ; en : verwijderen om dan free format te kunnen lezen
            Do idum=pos1, FilCharIdLength
               teken = String(idum:idum)
               if (teken .eq. dubbelpunt .or. teken .eq. puntkomma .or. &
                    teken .eq. quote .or. teken .eq. slash) String(idum:idum) = ' '
            Enddo
            Read (String(Pos1+1:),*) timeSettings%OutputStartyear, timeSettings%OutputStartMonth,  &
                                     timeSettings%OutputStartDay, &
                                     timeSettings%OutputStartHour, timeSettings%OutputStartMinute, &
                                     timeSettings%OutputStartSecond
          ELSEIF (STRING(1:POS1-1) .EQ. 'OUTPUT2CF_ENDTIME')  THEN
! format van periode is of 'yyyy/mm/dd;hh:mm:ss'
!           Eerst even de /, ; en : verwijderen om dan free format te kunnen lezen
            Do idum= Pos1, FilCharIdLength
               teken = String(idum:idum)
               if (teken .eq. dubbelpunt .or. teken .eq. puntkomma .or. &
                    teken .eq. quote .or. teken .eq. slash) String(idum:idum) = ' '
            Enddo
            Read (String(Pos1+1:),*) timeSettings%OutputEndyear, timeSettings%OutputEndMonth, &
                                     timeSettings%OutputEndDay, &
                                     timeSettings%OutputEndHour, timeSettings%OutputEndMinute, &
                                     timeSettings%OutputEndSecond
          ELSEIF (STRING(1:POS1-1) .EQ. 'EVAPORATIONFROMHRS') THEN
             READ(STRING(POS1+1:),*) timeSettings%EvaporationFromHr
          ELSEIF (STRING(1:POS1-1) .EQ. 'EVAPORATIONTOHRS') THEN
             READ(STRING(POS1+1:),*) timeSettings%EvaporationToHr
          ELSEIF (STRING(1:POS1-1) .EQ. 'IRRIGATIONFROMHRS') THEN
             READ(STRING(POS1+1:),*) timeSettings%IrrigationFromHr
          ELSEIF (STRING(1:POS1-1) .EQ. 'IRRIGATIONTOHRS') THEN
             READ(STRING(POS1+1:),*) timeSettings%IrrigationToHr
          ELSEIF (STRING(1:POS1-1) .EQ. 'IRRIGATIONSTARTDAY') THEN
             READ(STRING(POS1+1:),*) timeSettings%IrrigationStartDay
          ELSEIF (STRING(1:POS1-1) .EQ. 'IRRIGATIONSTARTMONTH') THEN
             READ(STRING(POS1+1:),*) timeSettings%IrrigationStartMonth
          ELSEIF (STRING(1:POS1-1) .EQ. 'IRRIGATIONENDDAY') THEN
             READ(STRING(POS1+1:),*) timeSettings%IrrigationEndDay
          ELSEIF (STRING(1:POS1-1) .EQ. 'IRRIGATIONENDMONTH') THEN
             READ(STRING(POS1+1:),*) timeSettings%IrrigationEndMonth
          ELSEIF (STRING(1:POS1-1) .EQ. 'NIGHTFROMHRS') THEN
             READ(STRING(POS1+1:),*) timeSettings%nightFromHr
          ELSEIF (STRING(1:POS1-1) .EQ. 'NIGHTTOHRS') THEN
             READ(STRING(POS1+1:),*) timeSettings%nightToHr
          ELSEIF (STRING(1:POS1-1) .EQ. 'WEEKHRSASNIGHT') THEN
             READ(STRING(POS1+1:),*) IDUM
             IF (IDUM .NE. 0)   timeSettings%weekendHrsAsNight = .true.
          ELSEIF (STRING(1:POS1-1) .EQ. 'TIMESTEPEXCHANGE')  THEN
             READ(STRING(POS1+1:),*) ExchangeSettings%timestepSize
          ELSEIF (STRING(1:POS1-1) .EQ. 'EXCHANGEOPTION')  THEN
             READ(STRING(POS1+1:),*) ExchangeSettings%StringOption
          ELSEIF (STRING(1:POS1-1) .EQ. 'CASENM')  THEN
             READ(STRING(POS1+1:),*) CASENM
          ELSEIF (STRING(1:POS1-1) .EQ. 'DIRLISTING')  THEN
             READ(STRING(POS1+1:),*) IDUM
             IF (IDUM .NE. 0)  APESLN = .true.
          ELSEIF (STRING(1:POS1-1) .EQ. 'WEIRSETTINGTARGETLEVEL')  THEN
             READ(STRING(POS1+1:),*) IDUM
             IF (IDUM .NE. 0)  UPDSTW  = .true.
          ELSEIF (STRING(1:POS1-1) .EQ. 'MINFILLINGPERCENTAGE')  THEN
             READ(STRING(POS1+1:),*) PRCKAS
          ELSEIF (STRING(1:POS1-1) .EQ. 'CONTROLMODULE') THEN
             READ(STRING(POS1+1:),*) ISTUURM
             ISTUURM = ABS (ISTUURM)
          ELSEIF (STRING(1:POS1-1) .EQ. 'MAALSTOPMODULE') THEN
             READ(STRING(POS1+1:),*) IMLSTP
             IMLSTP = ABS (IMLSTP)
          ELSEIF (STRING(1:POS1-1) .EQ. 'WLMMODULE') THEN
             READ(STRING(POS1+1:),*) IWlmModule
             IWlmModule = ABS (IWLMModule)
          ELSEIF (STRING(1:POS1-1) .EQ. 'WQMODULE') THEN
             READ(STRING(POS1+1:),*) IWQModule
             IWQModule = ABS (IWQModule)
          ELSEIF (STRING(1:POS1-1) .EQ. 'MODFLOWMODULE') THEN
             READ(STRING(POS1+1:),*) IModFlow
             IModFlow = ABS (IModFlow)
          ELSEIF (STRING(1:POS1-1) .EQ. 'OUTPUTTIMESTEP') THEN
             READ(STRING(POS1+1:),*) IOPT1(1)
          ELSEIF (STRING(1:POS1-1) .EQ. 'OUTPUTEVENT') THEN
             READ(STRING(POS1+1:),*) IOPT1(2)
! June 2004; this switch is not used
!          ELSEIF (STRING(1:POS1-1) .EQ. 'OUTPUTOVERALL') THEN
!             READ(STRING(POS1+1:),*) IOPT1(3)
! June 2004; new switch introduced
          ELSEIF (STRING(1:POS1-1) .EQ. 'OUTPUTSERIESFULLDETAIL') THEN
             READ(STRING(POS1+1:),*) IOPT1(3)
          ELSEIF (STRING(1:POS1-1) .EQ. 'OUTPUTDETAIL') THEN
             READ(STRING(POS1+1:),*) IOPT1(4)
          ELSEIF (STRING(1:POS1-1) .EQ. 'OUTPUTSCREEN') THEN
             READ(STRING(POS1+1:),*) IOPT1(5)
          ELSEIF (STRING(1:POS1-1) .EQ. 'OUTPUTGROUNDWATER') THEN
             READ(STRING(POS1+1:),*) IOPT2(1)
          ELSEIF (STRING(1:POS1-1) .EQ. 'OUTPUTOPENWATER') THEN
             READ(STRING(POS1+1:),*) IOPT2(2)
          ELSEIF (STRING(1:POS1-1) .EQ. 'OUTPUTATTIMESTEP') THEN
             READ(STRING(POS1+1:),*) OutputAtTimestep
             OutputAtTimestep = max (1, OutputAtTimestep)
          ELSEIF (STRING(1:POS1-1) .EQ. 'OUTPUTATTIMESTEPOPTION') THEN
             READ(STRING(POS1+1:),*) OutputAtTimestepOption
             OutputAtTimestepOption = max (1, OutputAtTimestepOption)
! In Sobek 208 replaced by output options per node type
!          ELSEIF (STRING(1:POS1-1) .EQ. 'REDUCED OUTPUT')  THEN
!             READ(STRING(POS1+1:),*) IDUM
!             IF (IDUM .NE. 0)  SmallOutput = .true.
          ELSEIF (STRING(1:POS1-1) .EQ. 'EXTENDEDBALANCE')  THEN
             READ(STRING(POS1+1:),*) IDUM
             IF (IDUM .NE. 0)  ExtendedBalanceOutput = .true.
          ELSEIF (STRING(1:POS1-1) .EQ. 'OUTPUTBOUNDARY') THEN
             READ(STRING(POS1+1:),*) OptionsBoundary
          ELSEIF (STRING(1:POS1-1) .EQ. 'RESTARTIN') THEN
             READ(STRING(POS1+1:),*) RESTIO(1)
             RESTIO(1) = ABS ( RESTIO(1) )
          ELSEIF (STRING(1:POS1-1) .EQ. 'RESTARTOUT') THEN
             READ(STRING(POS1+1:),*) RESTIO(2)
             RESTIO(2) = ABS ( RESTIO(2) )
!          ELSEIF (STRING(1:POS1-1) .EQ. 'OUTPUTTO3BPOST') THEN
!             READ(STRING(POS1+1:),*) RESTIO(3)
!             RESTIO(3) = ABS ( RESTIO(3) )
! indien RestIo(3)=0, dan direct HIS files in 3B genereren.
! Indien RestIo(3)=1, dan TMP files genereren en 3BP draaien.
          ELSEIF (STRING(1:POS1-1) .EQ. 'BINARYINPUT') THEN
             READ(STRING(POS1+1:),*) RESTIO(5)
             RESTIO(5) = ABS ( RESTIO(5) )
          ELSEIF (STRING(1:POS1-1) .EQ. 'BINARYOUTPUT') THEN
             READ(STRING(POS1+1:),*) RESTIO(4)
             RESTIO(4) = ABS ( RESTIO(4) )
! Aug 2003: verwijderen inlezen OutputUnits
          ELSEIF (STRING(1:POS1-1) .EQ. 'DEFAULTDATASET') THEN
             READ(STRING(POS1+1:),*) IDEFLT
          ELSEIF (STRING(1:POS1-1) .EQ. 'SALTCOMPUTATION') THEN
             READ(STRING(POS1+1:),*) ISLCMP
          ELSEIF (STRING(1:POS1-1) .EQ. 'SALTCONCOUTPUT') THEN
             READ(STRING(POS1+1:),*) IFLZT
          ELSEIF (STRING(1:POS1-1) .EQ. 'SALTCONCRAINFALL') THEN
             READ(STRING(POS1+1:),*) SLTRAI
! Process Manager extensions
          ELSEIF (STRING(1:POS1-1) .EQ. 'USEPM') THEN
             READ(STRING(POS1+1:),*) Idum
             UsePM = (idum .ne. 0)
          ELSEIF (STRING(1:POS1-1) .EQ. 'PMHEADS') THEN
             READ(STRING(POS1+1:),*) PmStringHeads
          ELSEIF (STRING(1:POS1-1) .EQ. 'PMTIMESTEP') THEN
             READ(STRING(POS1+1:),*) PmStringTimestep
          ELSEIF (STRING(1:POS1-1) .EQ. 'PMFLUXES') THEN
             READ(STRING(POS1+1:),*) PmStringFluxes
          ELSEIF (STRING(1:POS1-1) .EQ. 'PMRRINPORT') THEN
             READ(STRING(POS1+1:),*) PmRRInPort
          ELSEIF (STRING(1:POS1-1) .EQ. 'PMRROUTPORT') THEN
             READ(STRING(POS1+1:),*) PmRROutPort
          ELSEIF (STRING(1:POS1-1) .EQ. 'DETAILEDVOLUMECHECKMESSAGES') THEN
             READ(STRING(POS1+1:),*) idum
             DetailedVolumeCheckMessages = (idum .ne. 0)
          ELSEIF (STRING(1:POS1-1) .EQ. 'USE_INI') THEN
             READ(STRING(POS1+1:),*) Idum
             IF (IDUM .NE. 0)   ini_switch = .true.
          ENDIF
      ENDIF
      GOTO 101

  150 CONTINUE

    Idebug = 0

!OutputHIS=-1
    IOHIS = .True.
! als elke tijdstap uitvoer, dan altijd current calue
    if (OutputAtTimestep .eq. 1)   OutputAtTimestepOption = 1


    Call Network_initpart2 (MeteoNetcdfInput)


  return
  end subroutine Network_readASCII



  subroutine Network_initpart1

    integer iuur

    !set default values control data
    DO IUUR=0,23
       LNACHT(IUUR) = .FALSE.
    ENDDO
    CaseNm = ''
    ISLCMP = 0
    IFLZT  = 0
    SLTRAI = 0.
! June 2004
    iOpt1 = 0
! end June 2004
    RESTIO(1) = 0
    RESTIO(2) = 0
    RESTIO(3) = 0  !was 1; nieuwe default init per eind maart 99: altijd direct uitvoer in HIS genereren
    RESTIO(4) = 0
    RESTIO(5) = 0
! RestIo(6) en (7) toegevoegd voor tussentijdse restartfiles; inlezen via RdIni
    RestIo(6) = -1
    RestIo(7) = -1

    optionsBoundary = 0
    TMEVAP = 1.0
    TMIRRI = 1.0
    IMLSTP = 0
    IModFlow   = 0
    IWLMModule = 0
    IWQModule  = 0
    IOHIS  = .FALSE.
    RunSimultaneous = .FALSE.

    ExtendedBalanceOutput = .FALSE.
    ini_switch = .false.
!ReachIdInNodefile=0
    IRCHID = 0

! default initialisatie OutputAtTimestepOptions
    OutputAtTimestep = 1
    OutputAtTimestepOption = 1
    OutputAtTimestepOption = 1

! August 2001: TimeSettings
    TimeSettings%StartTimeFromEvent=.true.   ! default start time from rainfall event
    TimeSettings%StartYear  = 0
    TimeSettings%StartMonth = 0
    TimeSettings%StartDay   = 0
    TimeSettings%StartHour  = 0
    TimeSettings%StartMinute= 0
    TimeSettings%StartSecond= 0
    TimeSettings%EndYear  = 0
    TimeSettings%EndMonth = 0
    TimeSettings%EndDay   = 0
    TimeSettings%EndHour  = 0
    TimeSettings%EndMinute= 0
    TimeSettings%EndSecond= 0
    TimeSettings%Output2CFUserDefinedPeriod=.false.   ! default output to CF for whole period
    TimeSettings%OutputStartYear  = 0
    TimeSettings%OutputStartMonth = 0
    TimeSettings%OutputStartDay   = 0
    TimeSettings%OutputStartHour  = 0
    TimeSettings%OutputStartMinute= 0
    TimeSettings%OutputStartSecond= 0
    TimeSettings%OutputEndYear  = 0
    TimeSettings%OutputEndMonth = 0
    TimeSettings%OutputEndDay   = 0
    TimeSettings%OutputEndHour  = 0
    TimeSettings%OutputEndMinute= 0
    TimeSettings%OutputEndSecond= 0
    TimeSettings%OutputEvent = 0

! Jan 2000: add ExchangeSettings
    ExchangeSettings%timestepSize = 0
    ExchangeSettings%StringOption = 'Detail'
    ExchangeSettings%Option = 1


! Dec 2000: Process Manager default initialisation
    Call PMInterface_Init1


! Aug2003  initialisatie OutputUnits
    OutputUnits%PavedStorage        =    2
    OutputUnits%PavedFlow           =    1
    OutputUnits%UnpavedGroundwater  =    3
    OutputUnits%UnPavedFlow         =    1
    OutputUnits%GreenhouseStorage   =    2
    OutputUnits%GreenhouseFlow      =    1
    OutputUnits%OpenwaterLevel      =    3
    OutputUnits%OpenwaterExceedance =    6
    OutputUnits%StructureFlow       =    1
    OutputUnits%BoundaryFlow        =    1
    OutputUnits%NwrwStorage         =    2
    OutputUnits%NwrwFlow            =    1   !GP dec2021; was 2 maar Govert vraag om 1
    OutputUnits%balance             =    2
    OutputUnits%SaltConcentration   =    8
    OutputUnits%RWZIFlow            =    1
    OutputUnits%IndustryFlow        =    1
    OutputUnits%SacramentoFlow      =    0   ! not used
    OutputUnits%SacramentoStorage   =    0   ! not used


  return
  end subroutine Network_initpart1



  subroutine Network_initpart2 (MeteoNetCdfInput)

    logical MeteoNetCdfInput
    integer teller, iuur, idum, ilen
    Character(FilCharIdLength) string


    iDeflt = abs(iDeflt)
    do teller = 1, 5
      iOpt1(teller) = abs(iOpt1(teller))
      restIO(teller) = abs(restIO(teller))
    enddo
    do teller = 1, 2
      iOpt2(teller) = abs(iOpt2(teller))
    enddo

! Tussentijdse restart files: indien restart files gewenst, dan tussentijds bij t=1 en 12 uur
    If (RestIo(2) .eq. 1) then
      RestIo(6) = 3600    ! 1 uur
      RestIo(7) = 43200   ! 12 uur
    Endif


!    ALLOCATE (IOPTND(NKind+2, 6), Stat=Allocation_Error )
!    If (Allocation_Error .ne. 0) &
!       call ErrMsgStandard (981, Allocation_Error, ' Error allocating arrays in subroutine ', &
!                                           ' NetWork_InitPart2')


!---------------------------------
    ! checks on input
      ! control data

    String = ' '
    ITMSTU = 0
! als ExchangeSettingsTimestepsize niet ingevuld (=nog gelijk aan init. waarde nul), zet dan gelijk aan timestepsize
    If (ExchangeSettings%timestepSize .le. 0)  then
       ExchangeSettings%timestepsize = timesettings%timestepsize
       ExchangeSettings%StringOption = 'DETAIL'
    Endif

    Call PMInterface_Init2

! consistency time and exchange settings
    If ( (timesettings%timestepsize .gt. ExchangeSettings%timestepsize)) then
        call ErrMsgStandard (933, 0, '  Network input', STRING)
    else
        idum = ExchangeSettings%Timestepsize / TimeSettings%Timestepsize
        if (ExchangeSettings%Timestepsize .ne. idum * TimeSettings%Timestepsize ) then
           call ErrMsgStandard (933, 0, '  Network input', STRING)
        endif
        ITMSTU = idum
    endif

    ilen = Len_trim (ExchangeSettings%StringOption)
    String = ExchangeSettings%StringOption
    if (ilen .le. 0) then
        call ErrMsgStandard (934, 0, '  Network input', STRING)
    elseif (String(1:ilen) .eq. 'DETAIL') then
             ExchangeSettings%Option = 1
    elseif (String(1:ilen) .eq. 'AVERAGE') then
              ExchangeSettings%Option = 2
    elseif (String(1:ilen) .eq. 'CURRENT') then
             ExchangeSettings%Option = 3
!    elseif (String(1:ilen) .ne. 'DETAIL' .and.  &
!              String(1:ilen) .ne. 'AVERAGE' .and. &
!                String(1:ilen) .ne. 'CURRENT') then
    else
        call ErrMsgStandard (934, 0, '  Network input', STRING)
    endif

    If (  timesettings%timeWeightFactor .lt. 0.0  .OR. &
          timesettings%timeWeightFactor .gt. 1.0  ) then
        call ErrMsgStandard (932, 0, '  Network input', STRING)
    endif

    IF (PRCKAS .GT. 100. .OR. PRCKAS .LT. 0) then
        call ErrMsgStandard (925, 0, '  Network input', STRING)
    endif

    PRCKAS = PRCKAS/100.

!----------------------------------
    ! initialising some things
      ! control data
    IF (timeSettings%nightFromHr .NE. timeSettings%nightToHr) THEN
      DO IUUR=0,23
       IF (IUUR .LE. timeSettings%nightToHr .AND. IUUR .GE. timeSettings%nightFromHr .AND. &
          timeSettings%nightToHr .GE. timeSettings%nightFromHr) THEN
          LNACHT(IUUR) = .TRUE.
       ELSEIF (IUUR .LE. timeSettings%nightToHr .AND. IUUR .GE. timeSettings%nightFromHr-24 .AND. &
          timeSettings%nightToHr .LT. timeSettings%nightFromHr) THEN
          LNACHT(IUUR) = .TRUE.
       ELSEIF (IUUR .GE. timeSettings%nightFromHr .AND. IUUR .LE. timeSettings%nightToHr+24 .AND. &
          timeSettings%nightToHr .LT. timeSettings%nightFromHr) THEN
          LNACHT(IUUR) = .TRUE.
       ENDIF
      ENDDO
    ENDIF

    IF (ISTUURM /= 0) THEN
      RunSimultaneous = .TRUE.
      ITMSTU = MAX (1, ITMSTU)
    endif
    IF (IWQModule .ne. 0) THEN
      RunSimultaneous = .TRUE.
      ITMSTU = MAX (1, ITMSTU)
    endif

    TMEVAP = ABS (timeSettings%evaporationToHr-timeSettings%evaporationFromHr) / 24.0
    TMIRRI = ABS (timeSettings%evaporationToHr-timeSettings%evaporationFromHr) / 24.0
   ! Jan 1998: message added in case To and From hours are equal, so no evaporation
    IF (TMEVAP .le. 0.0) then
      call ErrMsgStandard (970, 0, ' Networkmodule', ' Evaporation from and to hours are equal, so evaporation is put to zero!' )
      TMEVAP = 0.0
    else
      TMEVAP = 1.0 / TMEVAP
    endif
    IF (TMIRRI .le. 0.0) then
      call ErrMsgStandard (970, 0, ' Networkmodule', ' Irrigation from and to hours are equal, so irrigation is put to zero!' )
      TMIRRI = 0.0
    else
      TMIRRI = 1.0 / TMIRRI
    endif

    If (TimeSettings%TimestepSize .ge. 86400 .and. TmEvap .ne. 1) then
      call ErrMsgStandard (977, 0, ' Networkmodule', ' Timestepsize is one day or more, specified evaporation hours are adjusted!' )
      timeSettings%evaporationFromHr = 0
      timeSettings%evaporationToHr = 24
      TmEvap = 1.0
    Endif

    If (TimeSettings%TimestepSize .ge. 86400 .and. TmIrri .ne. 1) then
      call ErrMsgStandard (977, 0, ' Networkmodule', ' Timestepsize is one day or more, specified irrigation hours are adjusted!' )
      timeSettings%evaporationFromHr = 0
      timeSettings%evaporationToHr = 24
      TmIrri = 1.0
    Endif

    select case (optionsBoundary)
      case (0)  ! no output q, c for boundaries
        IQCBND = 0
        QBPLV = .FALSE.
      case (1) ! output q, c for boundaries, Rijnland format
        IQCBND = 1
        QBPLV = .FALSE.
      case (2) ! output q, c for boundaries, Pluvius format
        IQCBND = 1
        QBPLV = .TRUE.
    end select

    qbplv2 = .TRUE.

! Nov 2002: always OptionsBoundary=2
    IQCBnd = 1
    QBPLV  = .true.
    qbplv2 = .TRUE.
! end Nov 2002


! Dec 2000: Check Process Manager data; switch ControlModule Off
    If (UsePm) then
       RunSimultaneous = .false.
    EndIf

      ! end initialising control data

      ! setten van iOptND
!      iOptND(1, 1) = outputUnits%pavedStorage
!      iOptND(1, 2) = outputUnits%pavedFlow
!      iOptND(2, 1) = outputUnits%unpavedGroundwater
!      iOptND(2, 2) = outputUnits%unpavedFlow
!      iOptND(3, 1) = outputUnits%greenhouseStorage
!      iOptND(3, 2) = outputUnits%greenhouseFlow
!      iOptND(4, 1) = outputUnits%openwaterLevel
!      iOptND(4, 2) = outputUnits%openwaterExceedance
!      iOptND(5, 1) = outputUnits%structureFlow
!      iOptND(5, 2) = 1 ! was 0
!      iOptND(6, 1) = outputUnits%structureFlow
!      iOptND(6, 2) = 1 ! was 0
!      iOptND(7, 1) = outputUnits%nwrwStorage
!      iOptND(7, 2) = outputUnits%nwrwFlow
!      iOptND(8, 1) = outputUnits%balance
!      iOptND(8, 2) = 1 ! was 0
!      iOptND(9, 1) = outputUnits%saltConcentration
! !RWZI en Industry
!      iOptND(10, 1) = outputUnits%RwziFlow
!      iOptND(11, 1) = outputUnits%IndustryFlow
!      iOptND(10, 2) = outputUnits%RwziFlow
!      iOptND(11, 2) = outputUnits%IndustryFlow
! !Sacramento
!      iOptND(12, 1) = outputUnits%SacramentoStorage
!      iOptND(12, 2) = outputUnits%SacramentoFlow
! ! end setting iOptNd

      if (MeteoNetCdfInput) then
        ! number of MeteoStations NcMet already set based on Meteo Netcdf input file
      else
        call Network_miscInput
      endif

! Set nr of nodes to zero for all node types
      NCNODE = 0
      NCLINK = 0
      NCOW   = 0
      NCVHG  = 0
      NCOVHG = 0
      NCKAS  = 0
      NCSTRU = 0
      NCBOUN = 0
      NCPLUV = 0
      nrPumpNodes = 0
      nrWeirNodes = 0
      nrGateNodes = 0
      nrManningNodes = 0
      nrQhRelationNodes = 0
      nrCulverts = 0
      NcRwzi = 0
      NcIndus = 0
      NcSacr = 0
      NcCell = 0
      NcRRRunoff = 0
      NcRRRunoffExternal = 0
      NcRRRunoffHBV = 0
      NcRRRunoffSCS = 0
      NcRRRunoffNAM = 0
      NcRRRunoffLGSI = 0
      NcRRRunoffWagMod = 0
      NcRRRunoffWalrus = 0
      NcOWRain = 0

  return
  end subroutine Network_initpart2





  subroutine Network_miscInput
    ! variables
    Integer inRain
    Character(FilCHarIdLength) string !,deletespaces
    Logical  endFil
    Integer  iDummy

    ! body
    ! only called if not meteo data from NetCdf
    ! from file 13 (rainfall file) read nevent as 1st integer from 4th line
    CALL OPENFL(INRAIN, ConfFil_get_namFil(13),1,1)
    rewind(inRain)
    CALL SKPCOM (INRAIN, ENDFIL, 'ODS ')
    READ(INRAIN,*) iDummy    ! default dataset flag

    read(inRain, '(A17)') string
    call upperc(string)
    string = deleteSpaces(string)
    if (string(1:17) == "*AANTALSTATIONS") then
      read(inRain, *) ncMET
      CALL SKPCOM (INRAIN, ENDFIL, 'ODS ')
      read(inRain, *) string  ! namen van stations
    else
      call ErrMsgStandard (972, 0, ' Meteofile lacks stationnames.',  '(Hint, add: "* aantal stations" and stationnames)')
    endif
    close(inRain)


  return
  end subroutine Network_miscInput


  Integer function Network_get_nrNodes()
    Network_get_nrNodes = ncNode
  return
  end function Network_get_nrNodes

  Integer function Network_get_nrVhg()
    Network_get_nrVhg = ncVhg
  return
  end function Network_get_nrVhg

  Integer function Network_get_nrMeteo()
    Network_get_nrMeteo = ncMet
  return
  end function Network_get_nrMeteo

  Integer function Network_get_nrStruc()
    Network_get_nrStruc = ncStru
  return
  end function Network_get_nrStruc

  Integer function Network_get_nrBoun()
    Network_get_nrBoun = ncBoun
  return
  end function Network_get_nrBoun

  Integer function Network_get_nrRWZI()
    Network_get_nrRWZI = ncRwzi
  return
  end function Network_get_nrRWZI

  Integer function Network_get_nrIndus()
    Network_get_nrIndus = ncIndus
  return
  end function Network_get_nrIndus


  type (TimeSetting) function Network_get_TimeSettings()
    Network_get_TimeSettings = timeSettings
  return
  end function Network_get_TimeSettings



  Integer function Network_get_Default()
    Network_get_Default = iDeflt
  return
  end function Network_get_Default

 end module Network
