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
! at:               $Modtime:: 5-12-97 3:30p    $
!
! current revision: $Revision:: 11              $


module Unpaved

  use Crop
  use Network
  use Conf_fil
  use Conf_arr
  use NewTables
  use Openwater
  use Boundary
  use NWRW
  use RunoffFormulations
  use Messages
! Feb 2002: NewFormatSoildata
  use NewTables
! ARS 11505 May 2003
  use Dh_alloc
  use ReadLib


  ! variables
   implicit none

  ! *** Data onverharde gebieden
  ! *** UNPNAM       = geeft bij index IUnp de knoop index INODE
  ! *** AREAOH = areaal (m2)
  ! *** AREAGW = areaal per gewas (m2)
  ! *** NAMGW  = namen per gewas
  ! *** LVLOH  = maaiveld peil
  ! *** LVLOHMX= maximum maaiveld peil volgens Scurve
  ! *** BMAXOL = max.berging op land
  ! *** BINIOL = init.berging op land
  ! *** ONTWDP = ontwateringsdiepte
  ! *** INF_V  = infiltratiesnelheid (m/s)
  ! *** ALFAOH = alfa factoren voor berging land cq bodem
  ! ***           (.,1) = land
  ! ***           (.,2) = bodem drainage
  ! ***           (.,3) = bodem infiltratie
  ! *** ALFA2  = alfa factoren voor ontwateringsystemen
  ! ***           (.,1) = primair
  ! ***           (.,2) = secundair
  ! ***           (.,3) = tertiair
  ! *** LVLDRN  = niveau's primaire/secundaire/tertiare drains/afwatering
  ! ***           invoer in m beneden maaiveld, wordt direct omgerekend tov NAP
  ! ***           (.,1) = primair
  ! ***           (.,2) = secundair
  ! ***           (.,3) = tertiair
  ! **********************************************************
  ! *** Schematische weergave van alfa's en niveau's:
  ! ***
  ! ***      -- opp. afstroming -----------> alfaoh(.,1)
  ! ***   ------------------------- maaiveld
  ! ***
  ! ***      ------------------------> alfa2(.,3)
  ! ***   -------- tertiare ontwatering lvldrn(.,3)
  ! ***
  ! ***      ------------------> alfa2(.,2)
  ! ***   --------- secundaire ontwatering lvldrn(.,2)
  ! ***
  ! ***      ----------------> alfa2(.,1)
  ! ***   ---------- primaire ontwatering lvldrn(.,1)
  ! ***
  ! ***           ---------> alfaoh(.,2)
  ! ***   ----------------------- ontwateringsdiepte=open water streefpeil
  ! ***            <------- alfaoh(.,3)  infiltratie
  ! ***                                  voor alle situaties waar owpeil>gwlpeil
  ! **********************************************************
  ! *** BOTTYP = bodemtype
  ! *** BERGTB = Tabel bergingscoefficient (ontw.diepte,grondsoort)
  ! ***            (aantal ontw.diepten * grondsoorten)
  ! *** GROUND = grondsoortennrs. (NVAL2)
  ! *** OWDEPT = ontw.diepte voor tabel (NVAL2)
  ! *** gwTabel = tabel met initiele grondwaterstanden (datum, tijd, nivo)
  ! *** volgens definitie in module Tables
  ! *** MAXGWL  = max. toelaatbare grondwaterstand  (maaiveld)
  ! *** MAXGWL2 = max. toelaatbare grondwaterstand  (trheshold values)
  ! *** BINIBD = init.berging bodem
  ! *** AreaGwComp = areaal groundwater/unsaturated zone computations
  ! ***              Gebruik AreaGwComp voor: kwel/wegzijging mm naar volume
  ! ***                                       volume grondwater en grondwaterstandsverandering
  ! ***                                       uitstroming grondwater (Hellinga-deZeeuw / Krayenhoff vd Leur)
  ! ***              NIET voor onverzadigde zone / Capsim


  ! gebiedsgebonden kwel/wegzijging
  ! Cvalue = weerstandswaarde in dagen, bij variabele kwelbepaling via H0 als tabel hetzij uit Modflow
  Double precision, Pointer, SAVE ::  KWEL(:), WEGZG(:), iniGwl(:)
  Integer, Pointer, SAVE ::  SeepageCompOption(:)
  Double precision, Pointer, SAVE ::  CValue(:), H0Actual(:)

  Integer, Pointer, SAVE ::  UNPNam(:)
  Double precision, Pointer, SAVE ::     AREAOH(:), LVLOH(:), LVLOHMX(:), &
                                 AREAGW(:,:), &
                                 AREAGWComp(:), &
                                 BMAXOL(:), BINIOL(:), BINIBD(:), &
                                 ONTWDP(:),&
                                 INF_V(:), &
                                 ALFAOH(:,:), BERGTB(:,:), &
                                 OWDEPT(:),   MAXGWL(:), MAXGWL2(:), gwlIni(:), &
                                 ALFA2 (:,:), LVLDRN(:,:), ReservoirCoeff(:)
  INTEGER, Pointer, SAVE ::  BOTTYP(:), GROUND(:), CompOption(:)
  INTEGER                ::  NrInfMessages

  ! **********************************************************
  ! *** Schematische weergave van Ernst Resistance and drain levels
  ! ***
  ! ***      -- opp. afstroming -----------> ErnstResistance(.,1)
  ! ***   ------------------------- maaiveld
  ! ***
  ! ***      ------------------------> ErnstResistancealfa2(.,2)
  ! ***   -------- tertiare ontwatering Ernstlvldrn(.,3)
  ! ***
  ! ***      ------------------> ErnstResistance(.,3)
  ! ***   --------- secundaire ontwatering Ernstlvldrn(.,2)
  ! ***
  ! ***      ----------------> ErnstResistance(.,4)
  ! ***   ---------- primaire ontwatering Ernstlvldrn(.,1)
  ! ***
  ! ***           ---------> ErnstResistance(.,5)
  ! ***   ----------------------- ontwateringsdiepte=open water streefpeil
  ! ***            <------- ErnstResistance(.,6)  infiltratie
  ! ***                                  voor alle situaties waar owpeil>gwlpeil
  ! **********************************************************

  Double precision, Pointer, SAVE ::     ErnstResistance(:,:), ErnstLevelDrains(:,:)

! *** Type Unsat_zone represents the unsaturated zone

  type Unsat_Zone
    Double precision Max_Volume
    Double precision   Max_mm
    Double precision Min_Volume
    Double precision   Min_mm
    Double precision Init_Volume
    Double precision   Init_mm
    Double precision Actual_Volume
    Double precision   Actual_mm
  end type Unsat_Zone

  type Krayenhoff
      Double precision neerslag
      Double precision Kopbolling
      Double precision Kdebiet
  end type Krayenhoff

  type Krayenhoffnul
     Double precision Kopbollingnul
     Double precision Kdebietnul
     Double precision Kopbollinglast
     Double precision Kdebietlast
     Double precision peilnul
     Double precision openwaterlevel
  end type Krayenhoffnul


  type (Unsat_Zone), allocatable, save :: ONVZONE(:), CropOnvZone(:,:)
  type (Krayenhoff), allocatable, save :: KvdLeur(:,:) , KvdLeurRest(:)
  type (Krayenhoffnul), allocatable, save :: KvdLeurnul(:)
  integer KvdLVariatieOpenwater, KvdLInitOpenwater, KvdLDimDays
  Logical EstimateKvdLHistory

  integer  RestTermNr
  parameter (RestTermNr=59)
  Double precision ResttermFactor(RestTermNr), ResttermTijd(RestTermNr)
  Double precision, pointer, save :: KvdLH2Add(:,:), KvdLQ2Add(:,:)

  Double precision, pointer, save :: Vulling(:,:), Evapfact(:,:)

!ARS 1887: S curve voor maaiveld
! UseUnpavedScurve = Aantal subgebieden voor Scurve berekeningen
! PositionAlfa     = optie voor alfa's 0=absoluut tov maaiveld lv, 1 = relatief tov maaiveld voor de hele S curve
! UseScurve      = per onverhard gebied aangeven of Scurve gebruikt wordt of niet
! AreaSCurve     = de geinterpreteerde gegevens per deelgebied Scurve
! PercInundation = Percentage Inundatie

  integer UseUnpavedSCurve, PositionAlfa
  integer UseUnpavedScurveMax
  parameter (UseUnpavedScurveMax=100)
  type Scurve
     Double precision Level
     Double precision Area
     Double precision Percentage
     Double precision AlfaLevels(3)
     Double precision ActualSurfaceStorage
     Double precision ActualSoilStorage
     Double precision InitialSurfaceStorage
     Double precision InitialSoilStorage
     Double precision SurfaceOutflow
     Double precision SoilOutflow
  end type Scurve


  integer, pointer, save :: UseScurve (:)
  type (Scurve), allocatable, save :: AreaSCurve(:,:), AreaScurvePerCrop(:,:,:)
  Double precision, pointer, save :: PercInundation(:)
  logical, pointer, save :: SCurveDataToBeSetByRR(:), ScurveDataPerCropToBeSetByRR(:), useRestart210(:)

! April 2000: speed up and consistent gw level computations
! declaration for setup of relations dh - dV for groundwater computations per unpaved area
  Integer MaxGwDepth
  Integer, pointer, save :: GwlLastInterpIndex(:)
  Double precision, pointer, save :: GroundwaterDeltaLevel(:,:), GroundwaterDeltaVolume(:,:)

! Additional for Capsim in Sobek-RR
!HdeZBergC=Bergingscoefficient voor gebruik in Hellinga-deZeeuw formules
  integer, pointer, save :: CapsimCrop(:)
  Double precision, pointer, save :: RzEpot(:), CapsimDpRootz(:), CapsimBergc(:), HdeZBergC(:)
  Double precision    CoefRz, CoefGwl, PowerRz, PowerGwl
  integer NactRzClass, NActGwlClass, NactSoils
  integer InitCapsimOption, DetailedGwlComputation
  Double precision    StepGwlStorageCoefficient, HalfStepGwl
  Integer NrCapsimErrors, NrWLCapsimErrors
  Logical ConstantHdeZBergC

! inlezen ASCII files
  integer, pointer, save :: UnpRefIG_TTable(:)
  integer, pointer, save :: UnpRefH0_TTable(:), UnpRefSeepage_TTable(:), UnpRefSeepageConc_TTable(:)


! OnLineModflowUsed=True als er minstens 1 RR unpaved area is met H0 via Modflow, on-line uit Modflow
  Logical OnLineModflowUsed
  Logical CheckRootzoneGwl
  Integer MaxItrGwlMaaiveld, MaxItrSeepage, MaxItrUnsat
  Logical NoInfiltrationWhileGwOnSurface

! ARS 5176 fix on?
  Logical FixARS5176, FixArs8842, FixArs10084, FixArs11610, FixARS14669
  Logical FixJira24759, UnpavedPercolationLikeSobek213
  Double precision    KsatCapsim(21)
  Double precision    UnpVolumeCheckFactorToCF, UnpVolumeCheckFactorToOW

! irrigation data
  integer, pointer, save :: IrrigationSource(:)
  Double precision   , pointer, save :: IrrigationEfficiencyFactor(:)
  Double precision   , pointer, save :: IrrigationpFCrit(:)
  Double precision   , pointer, save :: IrrigationpFTarget(:)
  Double precision   , pointer, save :: IrrigationMaxSupply(:)
  Double precision   , pointer, save :: IrrigationInitSupply(:)
  Double precision   , pointer, save :: IrrigationInitDuration(:)
  Double precision   , pointer, save :: IrrigationCriticalSMValue(:)
  Double precision   , pointer, save :: IrrigationCriticalGWValue(:)
  Double precision   , pointer, save :: IrrigationTargetSMValue(:)
  Double precision   , pointer, save :: IrrigationTargetGWValue(:)
  logical, pointer, save :: FirstIrrigationInYear (:)
  logical, pointer, save :: IrrigationOngoing (:)
  Double precision   , pointer, save :: IrrigationDemand(:)
  Double precision   , pointer, save :: IrrigationSupply(:)
  Double precision   , pointer, save :: TimeSinceStartFirstIrrigation(:)
  Double precision   , pointer, save :: IrrigationSaltConcentration(:)

  Logical IrrigationSeason, IrrigationDailyPeriod

  Double precision ReduceSurfaceInfiltration
  Double precision,  pointer, save :: ReduceSurfaceInfiltrationCrop(:)

!Additional declarations for Sobek-Capsim from Staring Centrum
!
! Parameters
!
      integer Nxte
      parameter (nxte=25)
      integer Nxspun
      parameter(nxspun=50)
      integer nxrz
      parameter(nxrz=10)
      integer nxdpun
      parameter(nxdpun=50)
      integer nxfrsw
      parameter (nxfrsw=50)
!
! Variables
!
      integer debug_unit, CapsimDebug_unit
      integer message_unit
      character(len=132) CapsimDbgfile, CapsimMsgFile
      integer file_unit
      Double precision    dprz(nxspun, nxte)
      integer NUDPUN(nxspun, nxrz)
      Double precision    SRRZ(nxspun, nxrz, nxdpun)
      Double precision    FMCA(nxspun, nxrz, nxdpun)
      Double precision    SCSA(nxspun, nxrz, nxdpun)
      Double precision    DPRZUN(nxrz)
      Double precision    DPGWUN(nxspun, nxrz, nxdpun)
      Double precision    DPFRSW(nxfrsw)
      Double precision    FRSW(nxfrsw)
      Double precision    FREV(nxspun, nxte, 5)
! End additions for Sobek-Capsim; Staring Centrum

! Capsim+ additions WL
      Double precision    Kdoorlatendheid(nxspun, nxrz, nxdpun), AlfaFactor(nxspun,nxrz,nxdpun)

      Double precision, pointer, save ::    VRZH1(:,:,:), VRZH2(:,:,:), VRZH3_Low(:,:,:), &
                                VRZH3_High(:,:,:), VRZH4(:,:,:)
      Double precision, pointer, save ::    VRZ_Max(:)  !, VRZ_eq(:)
      Double precision    UserCoefKSat, UserFactVRZ
      integer CapsimPlusFlag   ! 1 = based on initial conditions only, 2=based on initial and final


  ! *** results onverhard gebied
  ! ***
  ! *** BOLND = berging op land aan eind huidige tijdstap
  ! *** BOLND0= berging op land aan eind vorige tijdstap
  ! *** GWL0  = grondwaterstand  aan eind vorige  tijdstap
  ! *** GWL   = grondwaterstand  aan eind huidige tijdstap
  ! *** BERGC = bergingscoefficient
  ! *** BOBD  = berging in bodem aan eind huidige tijdstap
  ! *** BOBD0 = berging in bodem aan eind vorige tijdstap
  ! *** RO    = regenval op land huidige tijdstap
  ! *** VO    = verdamping van land huidige tijdstap
  ! *** INO   = instroom bodem huidige tijdstap
  ! *** Q1O   = outflow over oppervlak huidige tijdstap
  ! *** Q2O   = outflow bodem huidige tijdstap
  ! *** QINB               = instroom naar grondwater vanuit onverzadigde zone (=netto percolatie)

  Double precision, pointer, SAVE ::  BOBD(:), BOBD0(:)
  Double precision, pointer, SAVE ::  BOLND(:), BOLND0(:), &
                              GWL0(:), GWL(:), BERGC(:), &
                              RO(:), VO(:), INO(:), VBO(:), QINB(:), &
                              VBOCROP(:,:), QinbCrop(:,:),&
                              Q1O(:), Q2O(:), iniDepthGwl(:)
  Double precision, pointer, SAVE ::  PreviousTimestepCapRis(:)

! InitGWLOption    0=bij SCurve toch init tov LvlOh lv. Default=-1= tov laagste punt Scurve
! InitBcOption     0=Init storage coefficient from target level open water, 1=from initial gw.
! UnSatZoneOption: 0=no unsaturated zone, 1=CapSim  2=Capsim+
! CumGroundwaterExceedanceOption = 0 = sum, 1=sum of squares from threshold
! DrainageDeltaH   0 = als voorheen (gestapelde systemen);
!                  -1=parallel systems; bepaling deltaH voor HdeZ en Ernst tussen gwl en drainagebasis, ipv gestapelde systemen

  INTEGER InitBcOption, UnSatZoneOption, CapsimPerCropArea, InitGwlOption, CumGroundwaterExceedanceOption
  INTEGER DrainageDeltaH

!Parameters Krayenhoff van de Leur
    Double precision    pi, pi_kwadraat, pi_derde32, pi_kwadraat8
    Integer NtermenKvdl

    Parameter (pi = 3.14159265)
    Parameter (pi_kwadraat  = pi * pi )
    Parameter (pi_kwadraat8 = pi_kwadraat / 8.0 )
    Parameter (pi_derde32   = pi_kwadraat * pi / 32.0 )
!   Parameter (NtermenKvdL  = 99 )
    Parameter (NtermenKvdL  = 10001 )
!End Krayenhoff vd Leur

!Unpaved output
  ! *** OvMxPercInund(iovh,Ievent) = max. Perc. Inundatie onverhard gebied, per event
  ! *** OVMGWS = maximum grondwaterstand (berging) onverhard gebied, per event
  ! *** OVMGWV = maximum volume grondwater onverhard gebied, per event
  ! *** OVMONL = maximum volume berging op land onverhard gebied, per event
  ! *** OVMONV = maximum vulling onv.zone in mm per onverhard gebied, per event
  ! *** OVMBGC = maximum bergingscoefficient
  ! *** OVMQOU = maximum flow onverhard gebied, per event
  ! ***          ( ,1) = via oppervlak
  ! ***          ( ,2) = via bodem
  ! ***          ( ,3) = regenval
  ! ***          ( ,4 ) =
  ! ***           ....  =
  ! ***          ( ,10) =
  ! *** GWEXC (,1) = 0/1 indicator whether reference groundwater level
  ! ***              is exceeded (1=yes, 0=no)
  ! ***       (,2) = duration of exceedance (in seconds),
  ! ***              total for each event
  ! ***       (,3) = cumulative sum of gwl-threshold level
  ! ***       (,4) = daily max. value to accumulate

   Double precision, pointer, SAVE ::   OvMxPercInund(:,:), OVMGWS(:,:), OVMGWV(:,:), OVMBGC(:,:), &
                                OVMONL(:,:,:), OVMONV(:,:,:), OVMQOU(:,:,:), GWEXC(:,:,:), IrrSupply(:,:),IrrGWDemand(:,:)

! Feb 2002      Soil data files in new format, with choice of Soil definition
      Logical       NewFormatSoildata
      Character(Len=CharIdLength) Soildefinition
      Character(Len=CharIdLength) SoilDefNoCapsim, SoilDefCapsim, SoilDepthDef, SoilStorageDef, SoilConversionDef
      Integer       NSoilNoCapsim, NSoilCapsim, NSoilDepths
      Integer, pointer, save:: SoilCnv(:), SoilCnv2(:)


contains

  Subroutine Unpaved_confAr1

     Implicit none
    ! variables
    Integer iunit, Iout1, Allocation_error
    Logical success

    ! body
    NOVH = MAX (1, NCOVHG ) !unpaved
    KvdLDimensie = Max (120, 50 * 86400 / timeSettings%timestepSize ) ! minstens 30 dagen historie meenemen in Krayenhoff vdLeur
    KvdLDimensie = Min (KvdLDimensie, 1999) ! maximaal 1999 tijdstappen historie meenemen in Krayenhoff vdLeur
    NrInfMessages = 0
! Aanpassing ARS 5625
! Bepaal Krayenhoff van de Leur dimensie arrays op basis van in INI file opgegeven aantal dagen mee te nemen historie
    If (KvdLDimDays .gt. 0)  KvdLDimensie = max(1, KvdLDimDays * 86400 / timeSettings%timestepSize)
    If (KvdLDimensie .gt. 1999) &
       call ErrMsgStandard (974, 0, ' Large Krayenhoff vdLeur array dimension may reduce performance ', ' Unpaved_ConfAr1' )
! End aanpassing ARS 5625
    if (VersionNumberBinFile .eq. '2.26.04  ') KvdLDimensie = 1  ! voor versie 2.26.04 niet geimplementeerd

    OnLineModflowUsed = .false.

    !crop factors
! Nov2001 Taiwan: Aanpassing CropFactor files: NCRP=16 momenteel (hard); NCROP mag minder zijn!
!   NCRP = 16 set in Crop_Confar1_part1
    If (NewFormatCropFactors) then
       CALL OPENFL(iunit, ConfFil_get_NAMFIL(111),1,1)
    Else
       CALL OPENFL(iunit, ConfFil_get_NAMFIL(19),1,1)
    Endif
    CALL RDHDR  (iunit, 1)
    Call CloseGP(iunit)

! Nov2001 Taiwan: Aanpassing CropFactor files: NCRP=16 momenteel (hard); NCROP mag minder zijn!
!   NCRP= MAX (1, NCROP ) =16 hard coded in CropModule
    IOut1 = ConfFil_get_iOut1()

    IF ((NCOVHG .GT. 0) .and. (iOut1 .ne. 0)) then
      WRITE(IOUT1,*) ' Unpaved areas         =',NOVH
      IF ((NCRP .GT. 0) .and. (IOut1 .ne. 0)) then
        WRITE(IOUT1,*) ' Crops                 =',NCRP
      Endif
      IF ((KvdLDimensie .gt. 1) .and. (Iout1 .ne. 0)) then
        WRITE(IOUT1,*) ' KvdL history          =',KvdLDimensie
      endif
    endif

    !*** Data onverharde gebieden

    success = DH_Allocinit (NOVH, Areaoh, LvlOh, LvlOhMx, 0D0)
    success = success .and. DH_allocinit (NOVH, UNPNAM, 0)
    success = success .and. DH_allocinit (NOVH, BMaxOl, BIniOl, BIniBd, 0D0)
    success = success .and. DH_allocinit (NOVH, AreaGwComp, 0D0)
    success = success .and. DH_allocinit (NOVH, ONTWDP, INF_V, 0D0)
    success = success .and. DH_allocinit (NOVH, GwlIni, MaxGwl, MaxGwl2, 0D0)
    success = success .and. DH_allocinit (NOVH, ReservoirCoeff, 0D0)
    success = success .and. DH_allocinit (NOVH, NCRP, AreaGw, 0D0)
    success = success .and. DH_allocinit (NOVH, 3, AlfaOH, Alfa2, LvlDrn,0D0)

    success = success .and. DH_allocinit (NCRP, ReduceSurfaceInfiltrationCrop, 0D0)

!   ALLOCATE ( AREAOH(NOVH), LVLOH(NOVH), LVLOHMX(NOVH), &
!              AREAGW(NOVH,NCRP), &
!              AREAGWComp(NOVH), &
!              BMAXOL(NOVH), BINIOL(NOVH), BINIBD(NOVH), &
!              ONTWDP(NOVH), INF_V(NOVH), &
!              ALFAOH(NOVH,3), MAXGWL(NOVH), MAXGWL2(NOVH), gwlIni(NOVH), &
!              ALFA2 (NOVH,3), LVLDRN(NOVH,3), ReservoirCoeff(NOVH), Stat=Allocation_Error )
    if (.not. success) call ErrMsgStandard (981, 0, ' Error allocating arrays in subroutine ',' Unpaved_ConfAr1')

    success = success .and. DH_allocinit (NOVH, 6, ErnstResistance, 0D0)
    success = success .and. DH_allocinit (NOVH, 3, ErnstLevelDrains, 0D0)
!   ALLOCATE ( ErnstResistance(NOVH,6), ErnstLevelDrains(Novh,3), Stat=Allocation_Error )
    If (.not. success) call ErrMsgStandard (981, 0, ' Error allocating arrays in subroutine ', ' Unpaved_ConfAr1' )

    success = success .and. DH_allocinit (NOVH, BotTyp, CompOption, 0)
!   ALLOCATE ( BOTTYP(NOVH), CompOption(NOVH), Stat=Allocation_Error )
    If (.not. success) call ErrMsgStandard (981, 0, ' Error allocating arrays in subroutine ',' Unpaved_ConfAr1')

    success = success .and. DH_allocinit (NOVH, Kwel, Wegzg, 0D0)
!   ALLOCATE ( KWEL(NOVH), WEGZG(NOVH), Stat=Allocation_Error )
    success = success .and. DH_allocinit (NOVH, SeepageCompOption, 0)
!   ALLOCATE ( SeepageCompOption(NOVH), Stat=Allocation_Error )
    If (.not. success) call ErrMsgStandard (981, 0, ' Error allocating arrays in subroutine ',' Unpaved_ConfAr1')
    success = success .and. DH_allocinit (NOVH, CValue,  H0Actual, 0D0)
!   ALLOCATE ( CValue(NOVH), H0Actual(NOVH), Stat=Allocation_Error )

    success = success .and. DH_allocinit (NOVH, IniDepthGwl, 0D0)
    If (.not. success) call ErrMsgStandard (981, 0, ' Error allocating arrays in subroutine ',' Unpaved_ConfAr1')
!   allocate (iniDepthGwl(nOvh), Stat=Allocation_Error )


   !*** Results onverhard gebied

    success = success .and. DH_allocinit (NOVH, BoBd, BoBd0, 0D0)
    If (.not. success) call ErrMsgStandard (981, 0, ' Error allocating arrays in subroutine ',' Unpaved_ConfAr1')
!   ALLOCATE ( BOBD (NOVH), BOBD0(NOVH), Stat=Allocation_Error  )
    success = success .and. DH_allocinit (NOVH, BoLnd, BoLnd0, 0D0)
    success = success .and. DH_allocinit (NOVH, GWL  , GWL0, 0D0)
    success = success .and. DH_allocinit (NOVH, BERGC, 0D0)
    success = success .and. DH_allocinit (NOVH, RO, VO, INO, 0D0)
    success = success .and. DH_allocinit (NOVH, VBO, QINB, 0D0)
    success = success .and. DH_allocinit (NOVH, NCROP, VBOCROP, QINBCROP, 0D0)
    success = success .and. DH_allocinit (NOVH, Q1O, Q2O, 0D0)
    success = success .and. DH_allocinit (NOVH, PreviousTimestepCapRis, 0D0)
    If (.not. success) call ErrMsgStandard (981, 0, ' Error allocating arrays in subroutine ',' Unpaved_ConfAr1')
!   ALLOCATE ( BOLND (NOVH), BOLND0(NOVH), &
!              GWL0  (NOVH), GWL  (NOVH), &
!              BERGC (NOVH), &
!              RO    (NOVH), VO   (NOVH), INO  (NOVH), &
!              VBO   (NOVH), QINB (NOVH), &
!              VBOCROP(NOVH,NCROP), QINBCROP(NOVH,NCROP), &
!              Q1O   (NOVH), Q2O  (NOVH), Stat=Allocation_Error )

    ALLOCATE ( ONVZONE(NOVH), Stat=Allocation_Error )
    success = success .and. (Allocation_error .eq. 0)
    ALLOCATE ( CropOnvZone(NOVH, NCRP), Stat=Allocation_Error )
    success = success .and. (Allocation_error .eq. 0)
    success = success .and. DH_allocinit (NVAL, NOVH, Vulling, EvapFact, 0D0)

! allocate irrigation arrays and set default values: no irrigation, efficiencyfactor 1.2, pFcrit=2.7, pFTarget=2.5, MaxSup=4 mm/day, InitSup=2.5 mm/day, InitSupDurataion=5 days
    success = success .and. DH_allocinit (NOVH, IrrigationSource, 0)
    success = success .and. DH_allocinit (NOVH, IrrigationEfficiencyFactor, 1.0D0)
    success = success .and. DH_allocinit (NOVH, IrrigationpFCrit, 2.6D0)
    success = success .and. DH_allocinit (NOVH, IrrigationpFTarget , 2.3D0)
    success = success .and. DH_allocinit (NOVH, IrrigationMaxSupply , 4.1D0)
    success = success .and. DH_allocinit (NOVH, IrrigationInitSupply, 2.6D0)
    success = success .and. DH_allocinit (NOVH, IrrigationInitDuration, 1.0D0)
    success = success .and. DH_allocinit (NOVH, IrrigationCriticalSMValue, -999.99D0)
    success = success .and. DH_allocinit (NOVH, IrrigationCriticalGWValue, -999.99D0)
    success = success .and. DH_allocinit (NOVH, IrrigationTargetSMValue, -999.99D0)
    success = success .and. DH_allocinit (NOVH, IrrigationTargetGWValue, -999.99D0)
    success = success .and. DH_allocinit (NOVH, FirstIrrigationInYear, .true.)
    success = success .and. DH_allocinit (NOVH, IrrigationOngoing, .false.)
    success = success .and. DH_allocinit (NOVH, IrrigationSupply, 0.0D0)
    success = success .and. DH_allocinit (NOVH, IrrigationDemand, 0.0D0)
    success = success .and. DH_allocinit (NOVH, TimeSinceStartFirstIrrigation, 0.0D0)
    success = success .and. DH_allocinit (NOVH, IrrigationSaltConcentration, 0.0D0)
! to be corrected / adjusted to proper defaults (for testing now some other values) !!!

!   Initialisations
    OnvZone%Max_Volume =0.
    OnvZone%Max_mm =0.
    OnvZone%Min_Volume =0.
    OnvZone%Min_mm =0.
    OnvZone%Init_Volume =0.
    OnvZone%Init_mm = 0.
    OnvZone%Actual_Volume = 0.
    OnvZone%Actual_mm = 0.
    CropOnvZone%Max_Volume =0.
    CropOnvZone%Max_mm =0.
    CropOnvZone%Min_Volume =0.
    CropOnvZone%Min_mm =0.
    CropOnvZone%Init_Volume =0.
    CropOnvZone%Init_mm = 0.
    CropOnvZone%Actual_Volume = 0.
    CropOnvZone%Actual_mm = 0.

!   ALLOCATE ( Vulling(NVAL,NOVH), Evapfact(NVAL,NOVH), Stat=Allocation_Error )
    success = success .and. DH_allocinit (NOVH, CapsimCrop, 0)
!   Allocate ( CapsimCrop (NOVH), Stat=Allocation_Error )
    success = success .and. DH_allocinit (NOVH, RzEPot, CapsimDpRootz, 0D0)
    success = success .and. DH_allocinit (NOVH, CapsimBergC, HdeZBergC, 0D0)
!   Allocate ( RzEpot(novh), CapsimDpRootz (NOVH), CapsimBergC (NOVH), HdeZBergC(NOVH), Stat=Allocation_Error )
    If (.not. success) call ErrMsgStandard (981, 0, ' Error allocating arrays in subroutine ',' Unpaved_ConfAr1')

    success = success .and. DH_allocinit (Nxspun, nxrz, ncrp, VRZH1, 0D0)
    success = success .and. DH_allocinit (Nxspun, nxrz, ncrp, VRZH2, 0D0)
    success = success .and. DH_allocinit (Nxspun, nxrz, ncrp, VRZH3_Low, 0D0)
    success = success .and. DH_allocinit (Nxspun, nxrz, ncrp, VRZH3_High, 0D0)
    success = success .and. DH_allocinit (Nxspun, nxrz, ncrp, VRZH4, 0D0)
    success = success .and. DH_allocinit (NOVH, VRZ_Max, 0D0)
!    success = success .and. DH_allocinit (NOVH, VRZ_eq, 0D0)

 !Additional declarations from Sobek-Capsim from Staring Centrum

    ALLOCATE ( KvdLeur(NOVH,KvdLDimensie), Stat=Allocation_Error )
    If (Allocation_Error .ne. 0) &
       call ErrMsgStandard (981, Allocation_Error, ' Error allocating arrays in subroutine ', &
                                           ' Unpaved_ConfAr1'  )
    ALLOCATE ( KvdLeurRest(NOVH), Stat=Allocation_Error )
    If (Allocation_Error .ne. 0) &
       call ErrMsgStandard (981, Allocation_Error, ' Error allocating arrays in subroutine ', &
                                           ' Unpaved_ConfAr1'  )
    ALLOCATE ( KvdLeurnul(NOVH), Stat=Allocation_Error )
    If (Allocation_Error .ne. 0) &
       call ErrMsgStandard (981, Allocation_Error, ' Error allocating arrays in subroutine ', &
                                           ' Unpaved_ConfAr1'  )
    ALLOCATE ( KvdLH2Add(NOVH,KvdLDimensie), Stat=Allocation_Error )
    If (Allocation_Error .ne. 0) &
       call ErrMsgStandard (981, Allocation_Error, ' Error allocating arrays in subroutine ', &
                                           ' Unpaved_ConfAr1'  )
    ALLOCATE ( KvdLQ2Add(NOVH,KvdLDimensie), Stat=Allocation_Error )
    If (Allocation_Error .ne. 0) &
       call ErrMsgStandard (981, Allocation_Error, ' Error allocating arrays in subroutine ', &
                                           ' Unpaved_ConfAr1'  )
! Initialisation
     KvdLeurRest%neerslag =0.
     KvdLeurRest%Kopbolling =0.
     KvdLeurRest%Kdebiet = 0.
     KvdLeur%neerslag =0.
     KvdLeur%Kopbolling =0.
     KvdLeur%Kdebiet = 0.
     KvdLeurNul%Kopbollingnul = 0.
     KvdLeurNul%Kdebietnul  = 0.
     KvdLeurNul%Kopbollinglast  = 0.
     KvdLeurNul%Kdebietlast  = 0.
     KvdLeurNul%peilnul  = 0.
     KvdLeurNul%openwaterlevel  = 0.

     KvdLH2Add = -999.9D0
     KvdLQ2Add = -999.9D0
!SCurve
    success = success .and. DH_allocinit (NOVH, UseScurve, 0)
    success = success .and. DH_allocinit (NOVH, SCurveDataToBeSetByRR, .true.)
    success = success .and. DH_allocinit (NOVH, SCurveDataPerCropToBeSetByRR, .true.)
    success = success .and. DH_allocinit (NOVH, UseRestart210, .false.)
    If (.not. success) call ErrMsgStandard (981, 0, ' Error allocating arrays in subroutine ',' Unpaved_ConfAr1')
!    ALLOCATE ( UseScurve(NOVH), Stat=Allocation_Error )
    ALLOCATE ( AreaScurve(NOVH,0:UseUnpavedScurveMax), Stat=Allocation_Error )
    If (Allocation_Error .ne. 0) &
       call ErrMsgStandard (981, Allocation_Error, ' Error allocating arrays in subroutine ', &
                                           ' Unpaved_ConfAr1'  )
!   Initialisation AreaScurve
     AreaSCurve%Level = 0.
     AreaSCurve%Area  = 0.
     AreaSCurve%Percentage =0.
     AreaSCurve%AlfaLevels(1) =0.
     AreaSCurve%AlfaLevels(2) =0.
     AreaSCurve%AlfaLevels(3) =0.
     AreaSCurve%ActualSurfaceStorage =0.
     AreaSCurve%ActualSoilStorage =0.
     AreaSCurve%InitialSurfaceStorage =0.
     AreaSCurve%InitialSoilStorage =0.
     AreaSCurve%SurfaceOutflow =0.
     AreaSCurve%SoilOutflow =0.


    success = success .and. DH_allocinit (NOVH, PercInundation, 0D0)
    If (.not. success) call ErrMsgStandard (981, 0, ' Error allocating arrays in subroutine ',' Unpaved_ConfAr1')
!   Allocate ( PercInundation(NOVH), Stat=Allocation_Error )

!   length of gw dh-dV relation tables: at least 500 elements,
!                                       or 10 meters below surface with a step size of StepGwlStorageCoefficient
    MaxGwDepth = max(500, int (10 / StepGwlStorageCoefficient) )
    success = success .and. DH_allocinit (NOVH, GwlLastInterpIndex, 1)
    success = success .and. DH_allocinit (MaxGwDepth, NOVH, GroundwaterDeltaLevel, GroundwaterDeltaVolume, 0D0)
    If (.not. success) call ErrMsgStandard (981, 0, ' Error allocating arrays in subroutine ',' Unpaved_ConfAr1')
!    Allocate ( GwlLastInterpIndex(NOVH), &
!               GroundwaterDeltaLevel(MaxGwDepth,NOVH), &
!               GroundwaterDeltaVolume(MaxGwDepth,NOVH), Stat=Allocation_Error )
!    GroundwaterDeltaLevel  = 0
!    GroundwaterDeltaLevel  = 0
!    GwlLastInterpIndex     = 1
    NrCapsimErrors=0
    NrWLCapsimErrors=0

! Tabellen via ASCII
    success = success .and. DH_allocinit (NOVH, UnpRefIG_TTable, UnpRefH0_TTable, UnpRefSeepage_TTable, 0)
    success = success .and. DH_allocinit (NOVH, UnpRefSeepageConc_TTable, 0)
    If (.not. success) call ErrMsgStandard (981, 0, ' Error allocating arrays in subroutine ',' Unpaved_ConfAr1')
!    Allocate (UnpRefIG_TTable(NOVH), Stat=Allocation_Error )
!    Allocate (UnpRefH0_TTable(NOVH), Stat=Allocation_Error )
!    Allocate (UnpRefSeepage_TTable(NOVH), Stat=Allocation_Error )

  Return
  End subroutine Unpaved_confAr1



  Subroutine UnPavedOutput_Confar (Nevnt)

    Implicit none
    Integer Nevnt
    Logical Success

! Unpaved output
    success = DH_allocinit (NOVH, Nevnt, OvMxPercInund, OvmGws, 0D0)
    success = success .and. DH_allocinit (NOVH, Nevnt, OvmGwv, OvmBGc, 0D0)
    success = success .and. DH_allocinit (NOVH, Nevnt, IrrSupply, IrrGWDemand, 0D0)
    success = success .and. DH_allocinit (NOVH, Nevnt, 2, OvMOnl, OvmOnv, 0D0)
    success = success .and. DH_allocinit (NOVH, 10, Nevnt, OvmQou, 0D0)
    success = success .and. DH_allocinit (NOVH,  4, Nevnt, GwExc, 0D0)
    If (.not. success) call ErrMsgStandard (981, 0, ' Error allocating arrays in subroutine ',' Unpaved_OutputConfAr')
!   ALLOCATE ( OvMxPercInund(NOVH, Nevnt), OVMGWS(NOVH,Nevnt), &
!              OVMGWV(NOVH,Nevnt), OVMONL(NOVH,Nevnt,2), &
!              OVMBGC(NOVH,NEvnt), OVMONV(NOVH,Nevnt,2), &
!              OVMQOU(NOVH,10,NEvnt), Stat=Allocation_Error )
!   ALLOCATE ( GWEXC(NOVH,4,Nevnt), Stat=Allocation_Error )


    Return
  End subroutine UnpavedOutput_Confar



  Subroutine Unpaved_confAr0
    NVAL2 = 12  ! unpaved
    NCRP  = 16  ! initialisation of NCRP
  Return
  End subroutine Unpaved_confAr0



  Subroutine Unpaved_readAsciiInput (Infile1, Infile2, Infile3, Infile4, &
                                     Infile5, Infile6)

    ! *********************************************************************
    ! Read Unpaved Ascii files
    ! *********************************************************************
! variables

    Integer :: RetVal

    Integer(4) Infile1, Infile2, Infile3, Infile4, Infile5, Infile6
    Integer teller, teller1, teller2, inod, numberOfAreasGW
    Integer NrSCurvePoints, ipoint, iUnp, iecode, iout1, idebug
    Double precision    ScurveIncrement, AreaLower, ALFADUM(6), LVLDUM(3), infdum
    Double precision    dummyKwel, DummyConc, DummyCValue
    Integer dummyCompOption
    Character(Len=CharIdLength) name, dummyH0Table, NodeId, dummyConcTable
    Character(Len=1000) String
    Character(Len=19999) BufString
    Integer        Nhlp, ileft, iright, idum1, idum2  !, CntStr, FindString
    Parameter     (Nhlp = 32)
    Integer       IDUM(NHLP), Tablenr, nrcolumns  !, Allocation_Error
    Real          RDUM(NHLP)
    Character(Len=CharIdLength) CDUM(NHLP), Tablename
    Logical       allow, found, endfil, occurs, tabyesno, Err969
    Double precision          ScurvePercentage(999), SCurveLevel(999), bmaxoldum, binioldum
    real                      c1dum
    Character(Len=CharIdLength), pointer :: STODEF(:), ALFDEF(:), ERNSTDef(:),SEPDEF(:), INFDEF(:), INIDEF(:), SCUDEF(:), H0Def(:), SaltConcDef(:)

    Character(Len=CharIdLength)  FileName
    Character(Len=1000000)       KeepBufString
    Integer                      IoUnit, LenString, ipos
    Character(len=1)   Klteken

    Logical, pointer :: AlreadyRead(:)
    Integer, pointer :: ReferenceToDefinition(:)
    INTEGER  CAPSIM2BOTTOMTYPE(21), BOTTOMTYPE2CAPSIM(12)
    logical  ScurveDefined, InitGwlDefined, H0Defined, ErnstDefined, SeepageTableDefined, SeepageConcTableDefined
    logical  success

    success = DH_Allocinit (NCOVHG, StoDef, AlfDef, ErnstDef, '')
    success = success .and. DH_allocinit (NCOVHG, SepDef, InfDef, IniDef, '')
    success = success .and. DH_allocinit (NCOVHG, ScuDef, H0Def, '')
    success = success .and. DH_allocinit (NCOVHG, SaltConcDef, '')
    if (.not. success) call ErrMsgStandard (981, 0, ' Error allocating arrays in subroutine ',' Unpaved_ReadAscii')

!   ALLOCATE    (STODEF(NCOVHG), ALFDEF(NCOVHG), ErnstDEF(NCOVHG),SEPDEF(NCOVHG), INFDEF(NCOVHG), &
!                INIDEF(NCOVHG), SCUDEF(NCOVHG), H0DEF(NCOVHG), Stat=Allocation_Error )
!   ALLOCATE   (ReferenceToDefinition(NCOVHG), Stat=Allocation_Error )
!   ALLOCATE   (AlreadyRead(NCOVHG), Stat=Allocation_Error )
    success = success .and. DH_allocinit (NCOVHG, ReferenceToDefinition, 0)
    success = success .and. DH_allocinit (NCOVHG, AlreadyRead, .false.)
    if (.not. success) call ErrMsgStandard (981, 0, ' Error allocating arrays in subroutine ',' Unpaved_ReadAscii')

    ScurveDefined = .false.
    H0Defined = .false.
    ErnstDefined = .false.
    InitGwlDefined = .false.
    SeepageTableDefined = .false.
    SeepageConcTableDefined = .false.
    allow = .false.
    found = .false.
    iOut1  = ConfFil_get_iOut1 ()
    iDebug = ConfFil_get_iDebug()

! Omzettabel capsim (tabel overgenomen uit 3bascbin)
    CAPSIM2BOTTOMTYPE = (/2,2,7,7,7,7,9,9,1,1,1,12,1,5,11,8,3,4,12,12,6 /)
    BOTTOMTYPE2CAPSIM = (/10,1,17,18,9,11,6,16,8,21,15,12 /)

!   Feb2002
    call SetMessage(LEVEL_INFO, 'Read Soildata file BergCoef')
    call RdBerg
    If (NewFormatSoilData) then
      if (idebug .ne. 0) then
          write(idebug,*) ' Soil conversion tables'
      endif
!     igv NewFormatSoilData: overschrijven Capsim2BottomType en BottomType2Capsim
!     of gebruik ipv Capsim2BottomType en BottomType2Capsim de gelezen SoilCnv en SoilCnv2
    else
!     OldFormat
      SoilCnv  = BottomType2Capsim
      SoilCnv2 = Capsim2BottomType
    endif

!   resetten parameters tbv. initialisatie
!   do iUnp = 1, ncOvhg
!       do teller = 1, NCRP
!         AREAGW (iUnp, teller) = 0
!       enddo
!       LVLOH (iUnp) = 0
!       BMAXOL (iUnp) = 0
!       BINIOL (iUnp) = 0
!       iniDepthGwl (iUnp) = 0
!       INF_V (iUnp) = 0
!       BOTTYP (iUnp) = 0
!       CompOption (iUnp) = 0
!       do teller = 1, 3
!         ALFAOH(iUnp, teller) = 0
!         ALFA2(iUnp, teller) = 0
!         LVLDRN(iUnp, teller) = 0
!       enddo
!       Reservoircoeff (iUnp) = 0
!       kwel (iUnp) = 0
!       wegzg (iUnp) = 0
!       UnpRefIG_TTable (iUnp) = 0
!       UnpRefH0_TTable (iUnp) = 0
!       UnpRefSeepage_TTable (iUnp) = 0
!       UseScurve(iUnp) = 0
!       STODEF(iUnp) = ''
!       SCUDEF(iUnp) = ''
!       ALFDEF(iUnp) = ''
!       ErnstDef(iUnp) = ''
!       SEPDEF(iUnp) = ''
!       INFDEF(iUnp) = ''
!       AlreadyRead(iUnp) = .false.
!   enddo
    numberOfAreasGW = 0
! einde reset


! *********************************************************************
! ***  If CleanRRFiles, also write cleaned input
! *********************************************************************
   if (CleanRRFiles) then
        FileName = ConfFil_get_namFil(15)
        FileName(1:) = Filename(1:Len_trim(FileName)) // '_cleaned'
        Call Openfl (iounit, FileName,1,2)  !unpaved.3b_cleaned
        Write(*,*) ' Cleaning unpaved.3b to file:', FileName
        Write(iout1,*) ' Cleaning unpaved.3b to file:', FileName
   endif
! *********************************************************************
! Read unpaved.3B file
! *********************************************************************
   call SetMessage(LEVEL_DEBUG, 'Read Unpaved.3b file')
   if (idebug .ne. 0) write(idebug,*) ' Read unpaved.3b file'
   Endfil = .false.
   teller = 0
   Retval = 0
   Call SKPCOM (INfile1, ENDFIL,'ODS')
   do while (.not. endfil)
    READ(Infile1,'(A1000)',END=21,ERR=150,IOSTAT=IECODE)  STRING
! skip regel als hij niet begint met juist keyword (UNPV)
    IF (STRING(1:4) .EQ. 'UNPV') then
! UNPV node id
     Retval = Retval + GetVAR2 (STRING,' id ',1,' UNPV-ReadAscii',' Unpaved.3B file',IOUT1, &
                   CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
     inod = 0
     call fndNd2(inod, CDUM(1))
     if (inod .gt. 0) then      ! knoop bestaat in schematisatie
      iunp = EiNode(inod,2)
      if (EiNode(inod,3) .eq. 2) then  ! en is onverhard gebied
       if (alreadyRead(iunp)) then
        call SetMessage(LEVEL_ERROR, 'Data for unpaved node '//cdum(1)(1:Len_trim(Cdum(1)))//' double in datafile Unpaved.3B')
       else
! cleaning RR files
        If (CleanRRFiles) write(Iounit,'(A)') String (1:len_trim(String))

        AlreadyRead(iunp) = .true.
        teller = teller + 1
        UNPNAM(iunp) = inod
! number of areas
        Retval = Retval + GetVAR2 (STRING,' na ',3,' UNPV-ReadAscii',' Unpaved.3B file',IOUT1, &
                      CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
        numberofAreasGW = IDUM(1)
! area per crop type
! bepaald direct totaal areaal
        Retval = Retval + GetVRS2 (STRING,' ar ',2,' UNPV-ReadAscii',' Unpaved.3b file', &
                      IOUT1, CDUM(1), RDUM(1), IDUM(1), NCRP, IflRtn)
        areaOh(iunp) = 0.0
        Do teller1 = 1, NCRP
           AREAGW(iunp, teller1) = RDUM(teller1)
           areaOh(iunp) = areaOh(iunp) + areaGw(iunp, teller1)
        Enddo
        areaGwComp(iunp) = AreaOh(iUnp)
! surface level
        Retval = Retval + GetVAR2 (STRING,' lv ',2,' UNPV-ReadAscii',' Unpaved.3B file',IOUT1, &
                      CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
        LVLOH(iunp) = RDUM(1)
! Scurve yes/no
        allow = .true. !mag ontbreken
        Retval = Retval + GetVAR2 (STRING,' su ',3,' UNPV-ReadAscii',' Unpaved.3B file',IOUT1, &
                      CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
        IF (FOUND) THEN
          if (IDUM(1) .EQ. 1) then
             ALLOW = .FALSE.
             Retval = Retval + GetVAR2 (STRING,' su 1 ',1,' UNPV-ReadAscii',' Unpaved.3B file',IOUT1, &
                           CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
             SCUDEF(iUnp) = CDUM(1)
             UseScurve(iUnp) = 1
             ScurveDefined = .true.
          else
             SCUDEF(iUnp) =''
          endif
        ENDIF
! Max. allowed groundwater level (threshold value for damage computations)
        allow = .true. !mag ontbreken
        Retval = Retval + GetVAR2 (STRING,' mg ',2,' UNPV-ReadAscii',' Unpaved.3B file',IOUT1, &
                      CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
        If (Found) then
           Maxgwl2(iunp) = RDUM(1)
        else
!          default value = surface level
           Maxgwl2(iunp) = LvlOh(iunp)
        endif
! Areaal GW computations, mag ontbreken
        allow = .true. !mag ontbreken
        Retval = Retval + GetVAR2 (STRING,' ga ',2,' UNPV-ReadAscii',' Unpaved.3B file',IOUT1, &
                      CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
        IF (FOUND) then
            AreaGwComp(iUnp) = Rdum(1)
            If (AreaGwComp(iUnp) .le. 0.0001) then
               call ErrMsgStandard(977, 0, ' Specified groundwater area =0 could cause problems node-id=', Id_Nod(Inod))
               ! Mar 2008: replace by sum of specified crop areas
               areaGwComp(iunp) = AreaOh(iunp)
            Endif
        Endif

! storage identification
        ALLOW = .FALSE.
        Retval = Retval + GetVAR2 (STRING,' sd ',1,' UNPV-ReadAscii',' Unpaved.3B file',IOUT1, &
                      CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
        STODEF(iUnp) = CDUM(1)
! Ernst / Krayenhoff-vdLeur / Hellinga- de Zeeuw
        ALLOW = .true.
        CompOption (iUnp) = 0
        ReservoirCoeff (iUnp) = 0.0
        Retval = Retval + GetVAR2 (STRING,' co ',3,' UNPV-ReadAscii',' Unpaved.3B file',IOUT1, &
                      CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
        If (FOUND) Then
! ARS 1478 formule van Ernst
          If (IDUM(1) .EQ. 3) Then
             Compoption(iUnp) = 3 ! Ernst
             ErnstDefined = .true.
          ElseIf (IDUM(1) .EQ. 2) Then
             ALLOW = .false.
             Retval = Retval + GetVAR2 (STRING,' rc ',2,' UNPV-ReadAscii',' Unpaved.3B file',IOUT1, &
                           CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
             ReservoirCoeff (iUnp) = RDUM(1)
             Compoption(iUnp) = 2 ! Krayenhoff-vdLeur
          Else
             Compoption(iUnp) = 1 ! Hellinga - de Zeeuw
          Endif
        Else
          CompOption(iUnp) = 1     ! default is Hellinga - de Zeeuw
        Endif
        If (CompOption(iUnp) .eq. 2 .and. ReservoirCoeff(iUnp) .le. 0.0) then
            call ErrMsgStandard(973, 0, ' ReservoirCoefficient should be positive', Id_Nod(Inod))
        Endif

! alfa-level, and Ernst identification
        ALLOW = .FALSE.
        ErnstDEF(iUnp) = ''
        ALFDEF(iUnp) = ''
        DO teller1 =1,3
           LVLDRN (iUnp,teller1) = 0
           ALFAOH (iUnp,teller1) = 0
           ALFA2 (iUnp,teller1) = 0
        Enddo
        DO teller1 =1,3
           ErnstLevelDrains (iUnp,teller1) = 0
        Enddo
        DO teller1 =1,6
           ErnstResistance  (iUnp,teller1) = 0
        Enddo
        IF (compoption(iUnp) .eq. 1) then !alfa def. alleen noodzakelijk in geval van Hellinga-deZeeuw
          Retval = Retval + GetVAR2 (STRING,' ad ',1,' UNPV-ReadAscii',' Unpaved.3B file',IOUT1, &
                        CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
          ALFDEF(iUnp) = CDUM(1)
        ENDIF
        IF (compoption(iUnp) .eq. 3) then !Ernst def. alleen noodzakelijk in geval van Ernst
          Retval = Retval + GetVAR2 (STRING,' ed ',1,' UNPV-ReadAscii',' Unpaved.3B file',IOUT1, &
                        CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
          ErnstDef(iUnp) = CDUM(1)
        ENDIF
! seepage identification
        H0DEF(iUnp) = ''
        Retval = Retval + GetVAR2 (STRING,' sp ',1,' UNPV-ReadAscii',' Unpaved.3B file',IOUT1, &
                      CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
        SEPDEF(iUnp) = CDUM(1)
! infiltration identification
        Retval = Retval + GetVAR2 (STRING,' ic ',1,' UNPV-ReadAscii',' Unpaved.3B file',IOUT1, &
                      CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
        INFDEF(iUnp) = CDUM(1)
! soil type
        Retval = Retval + GetVAR2 (STRING,' bt ',3,' UNPV-ReadAscii',' Unpaved.3B file',IOUT1, &
                      CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
        IF (IDUM(1) .gt. 100) THEN
          IDUM(1) = IDUM(1) - 100
          BotTyp(IUnp) = Idum(1)
          If (UnSatZoneOption .eq. 0) BOTTYP(iUnp) = SoilCnv2(IDUM(1))
          if (idebug .ne. 0) then
             write(idebug,*) ' Capsimtype ', IDUM(1), ' converted to bottomtype ', BOTTYP(iUnp)
          endif
        ELSE
          If (UnSatZoneOption .ge. 1) then
            BOTTYP(iUnp) = SoilCnv(IDUM(1))
            if (idebug .ne. 0) then
               write(idebug,*) ' bottomtype ', IDUM(1), ' converted to capsimtype ', BOTTYP(iUnp)
            endif
          ELSE
            BOTTYP(iUnp) = IDUM(1)
          endif
        ENDIF
! JIRA 20804: soil type should be a known number (either 1-21 (42) in case of Capsim use, or 1-12 if no Capsim is used
        IF (Bottyp(iunp) .le. 0 .or. Bottyp(iunp) .gt. 42 .or.  &
             (Bottyp(iunp) .gt. 12 .and. UnsatZoneOption .eq. 0) ) then
            call ErrMsgStandard (983, 0, ' Unknown soil type specified in unpaved record', String(1:Len_trim(String)) )
        Endif
! initial groundwaterlevel
        Retval = Retval + GetVAR2 (STRING,' ig ',3,' UNPV-ReadAscii',' Unpaved.3B file',IOUT1, &
                      CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
        IF (IDUM(1) .EQ. 0) THEN
          Retval = Retval + GetVAR2 (STRING,' ig 0 ',2,' Unpv_readAscii',' Unpaved.3b file',IOUT1, &
                        CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
          c1dum = Rdum(1)
          success = MakeConstTable(TableHandle, 1, TableNr, c1dum, c1dum, Idebug)
          if (.not. success) goto 21
          UnpRefIG_TTable(iUnp) = TableNr
          INIDEF(iUnp) = ''
        ELSE
          Retval = Retval + GetVAR2 (STRING,' ig 1 ',1,' UNPV-ReadAscii',' Unpaved.3B file',IOUT1, &
                        CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
          INIDEF(iUnp) = CDUM(1)
          InitGwlDefined = .true.
        ENDIF
! initial depth groundwaterlayer (salt)
        Retval = Retval + GetVAR2 (STRING,' gl ',2,' UNPV-ReadAscii',' Unpaved.3B file',IOUT1, &
                      CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
        iniDepthGWL (iUnp) = MAX (RDUM(1), 0.01) !MOET GROTER ZIJN DAN NUL
! Meteo station id
        Retval = Retval + GetVAR2 (STRING,' ms ',1,' Unpv_readAscii',' unpaved.3b file',IOUT1, &
                      CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
        NAMMET(inod) = CDUM(1)
! Initial salt concentration
        Retval = Retval + GetVAR2 (STRING,' is ',2,' Unpv_readAscii',' unpaved.3b file',IOUT1, &
                      CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
        SltIni(inod) = RDUM(1)
! areal adjustment factor rainfall on node, maybe missing,
        allow = .true.
        Retval = Retval + GetVAR2(STRING,' aaf ',2,' Unpv-readAscii',' unpaved.3b file',IOUT1, &
                         CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, Iflrtn)
        if (found) AAFNodeRainfall(inod) = max(0.0, RDUM(1))    ! AAF >= 0
        allow = .false.
! Irrigation input; if missing assume no irrigation
        allow = .true. ! alle irrigatie input mag ontbreken
        Retval = Retval + GetVAR2 (STRING,' irr ',3,' UNPV-ReadAscii',' Unpaved.3B file',IOUT1, &
                      CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
! irrigation source 0=no irrigation, 1=surface water, 2=groundwater, 3=external
        IF (FOUND) then
            IrrigationSource(iUnp) = Idum(1)
            If (Idum(1) .lt. 0 .or. Idum(1) .gt. 3)  then
               call ErrMsgStandard(977, 0, ' Specified irrigation input source not correct; node-id=', Id_Nod(Inod))
               IrrigationSource(iunp) = 0
            Endif
        Endif
! irrigation efficiency factor
        allow = .true. ! alle irrigatie input mag ontbreken
        Retval = Retval + GetVAR2 (STRING,' irreff ',2,' UNPV-ReadAscii',' Unpaved.3B file',IOUT1, &
                      CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
        IF (FOUND) then
            IrrigationEfficiencyFactor(iUnp) = Rdum(1)
            If (Rdum(1) .lt. 1)  then
               call ErrMsgStandard(977, 0, ' Specified irrigation efficiency factor input not correct; default will be used; node-id=', Id_Nod(Inod))
               IrrigationEfficiencyFactor(iunp) = 1.2
            Endif
        Endif
! irrigation pF critical
        allow = .true. ! alle irrigatie input mag ontbreken
        Retval = Retval + GetVAR2 (STRING,' pFcrit ',2,' UNPV-ReadAscii',' Unpaved.3B file',IOUT1, &
                      CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
        IF (FOUND) then
            IrrigationpFCrit(iUnp) = RDum(1)
            If (Rdum(1) .lt. 0 .or. Rdum(1) .gt. 4.2)  then
               call ErrMsgStandard(977, 0, ' Specified irrigation pF critical input not correct; default will be used; node-id=', Id_Nod(Inod))
               IrrigationpFCrit(iUnp) = 2.7
            Endif
        Endif
! irrigation pF target
        allow = .true. ! alle irrigatie input mag ontbreken
        Retval = Retval + GetVAR2 (STRING,' pFtarget ',2,' UNPV-ReadAscii',' Unpaved.3B file',IOUT1, &
                      CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
        IF (FOUND) then
            IrrigationpFTarget(iUnp) = RDum(1)
            If (Rdum(1) .lt. 0 .or. rDum(1) .gt. 4.2 .or. IrrigationpFTarget(iunp) .lt. IrrigationpFCrit(iunp) )  then
               call ErrMsgStandard(977, 0, ' Specified irrigation pF target input not correct; default will be used; node-id=', Id_Nod(Inod))
               IrrigationpFTarget(iUnp) = min (IrrigationpFCrit(iunp), 2.4d0)
            Endif
        Endif
! irrigation max supply
        allow = .true. ! alle irrigatie input mag ontbreken
        Retval = Retval + GetVAR2 (STRING,' maxsup ',2,' UNPV-ReadAscii',' Unpaved.3B file',IOUT1, &
                      CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
        IF (FOUND) then
            IrrigationMaxSupply(iUnp) = RDum(1)
            If (Rdum(1) .lt. 0) then
               call ErrMsgStandard(977, 0, ' Specified irrigation max. supply not correct; default will be used; node-id=', Id_Nod(Inod))
               IrrigationMaxSupply(iUnp) = 4.0
            Endif
        Endif
! irrigation initial supply
        allow = .true. ! alle irrigatie input mag ontbreken
        Retval = Retval + GetVAR2 (STRING,' initsup ',2,' UNPV-ReadAscii',' Unpaved.3B file',IOUT1, &
                      CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
        IF (FOUND) then
            IrrigationInitSupply(iUnp) = RDum(1)
            If (Rdum(1) .lt. 0 .or. IrrigationMaxSupply(iunp) .lt. IrrigationInitSupply(iunp) )  then
               call ErrMsgStandard(977, 0, ' Specified irrigation initial supply not correct; default will be used; node-id=', Id_Nod(Inod))
               IrrigationInitSupply(iUnp) = min (IrrigationMaxSupply(iunp), 2.5d0)
            Endif
        Endif
! irrigation initial supply duration
        allow = .true. ! alle irrigatie input mag ontbreken
        Retval = Retval + GetVAR2 (STRING,' initdur ',2,' UNPV-ReadAscii',' Unpaved.3B file',IOUT1, &
                      CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
        IF (FOUND) then
            IrrigationInitDuration(iUnp) = RDum(1)
            If (Rdum(1) .lt. 1)  then
               call ErrMsgStandard(977, 0, ' Specified irrigation initial duration (days) in input not correct; default will be used; node-id=', Id_Nod(Inod))
               IrrigationInitDuration(iUnp) = 5
            Endif
        Endif
! irrigation salt concentration
        allow = .true. ! alle irrigatie input mag ontbreken
        Retval = Retval + GetVAR2 (STRING,' irrsaltc ',2,' UNPV-ReadAscii',' Unpaved.3B file',IOUT1, &
                      CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
        IF (FOUND) then
            IrrigationSaltConcentration(iUnp) = RDum(1)
            If (Rdum(1) .lt. 0) then
               call ErrMsgStandard(977, 0, ' Specified irrigation salt concentration negative; default will be used; node-id=', Id_Nod(Inod))
               IrrigationSaltConcentration(iUnp) = 10.0
            Endif
        Endif


       Endif
      Endif
     Endif
    Endif
    Call SKPCOM (INfile1, ENDFIL,'ODS')
   Enddo
21 CONTINUE
  If (RetVal .gt. 0) call ErrMsgStandard (972, 0, ' Error reading unpaved input file Unpaved.3b',' Getting UNPV record')
  If (teller .lt. NcOvhg)  then
      Do inod=1,NcNode
        iunp = EiNode(inod,2)
        if (EiNode(inod,3) .eq. 2) then   ! en is unpaved node
          if (.not. AlReadyRead(iunp)) then
            NodeId = ' '
            NodeId = Id_Nod(inod)
            String = ' '
            String = ' ' // NodeId(1:Len_trim(NodeId)) // ' is missing'
            call ErrMsgStandard (977, 0, ' Data for unpaved node ',String(1:Len_trim(String)) )
          endif
        endif
      Enddo
      call ErrMsgStandard (972, 0, ' Not enough Unpaved data found', &
                           ' Some unpaved nodes in netwerk schematisation are not present in Unpaved.3b file')
  Endif

! cleaning RR files
   If (CleanRRFiles) Call closeGP (Iounit)

! *********************************************************************
! ***  If CleanRRFiles, also write cleaned input for unpaved.sto
! *********************************************************************
   if (CleanRRFiles) then
        FileName = ConfFil_get_namFil(16)
        FileName(1:) = Filename(1:Len_trim(FileName)) // '_cleaned'
        Call Openfl (iounit, FileName,1,2)  !unpaved.sto_cleaned
        Write(*,*) ' Cleaning unpaved.sto to file:', FileName
        Write(iout1,*) ' Cleaning unpaved.sto to file:', FileName
   endif
! *********************************************************************
! Read Unpaved.Sto file
! *********************************************************************
  Endfil = .false.
  teller = 0
  Retval = 0
  Call SetReferenceToDefZero (ReferenceToDefinition)

  call SetMessage(LEVEL_DEBUG, 'Read Unpaved.Sto file')
  if (idebug .ne. 0) write(idebug,*) ' Read unpaved.sto file'
  Call SKPCOM (Infile2, ENDFIL,'ODS')
  Do while (.not. endfil)
    READ (Infile2,'(A1000)',END=211,ERR=150,IOSTAT=IECODE) STRING
    IF (STRING(1:4) .EQ. 'STDF') Then
! Read storage id
      Retval = Retval + GetVAR2 (STRING,' id ',1,' Unpaved_readAscii',' unpaved.sto file',IOUT1, &
                    CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
      name = CDUM(1)
      teller = teller + 1
! Eerst testen of storage definition wel gebruikt wordt, dan pas verwerken
      IUnp = FindString (NcOvhg, Stodef, Name, NcOvhg, CaseSensitive)
      Occurs = (Iunp .gt. 0)
      if (IUnp .gt. 0) then
         if (ReferenceToDefinition(iunp) .gt. 0) then
           call SetMessage(LEVEL_ERROR, 'Storage Definition '//name(1:Len_trim(Name))//' double in datafile Unpaved.Sto')
           Occurs = .false. ! om verdere verwerking te stoppen
         else
!          cleaning RR files
           If (CleanRRFiles) write(Iounit,'(A)') String (1:len_trim(String))
         endif
      endif

      if (occurs) then
! Read maximum storage on streets
        Retval = Retval + GetVAR2 (STRING,' ml ',2,' unPaved_readAscii',' unpaved.sto file',IOUT1, &
                      CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
        bmaxoldum = RDUM(1)
! Read initial storage on streets
        Retval = Retval + GetVAR2 (STRING,' il ',2,' unPaved_readAscii',' unpaved.sto file',IOUT1, &
                      CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
        binioldum = RDUM(1)
! Assign definition to individual nodes
        Do iUnp = 1, ncOvhg
           if (StringComp(StoDef(Iunp), Name, CaseSensitive) )  then
              ReferenceToDefinition(iUnp) = teller
              BMAXOL(iUnp) = bmaxoldum     !nog in mm
              BINIOL(iUnp) = binioldum     !nog in mm
           endif
        Enddo
        if (idebug .ne. 0)   write(idebug,*) ' Read ',string(1:160)
      Endif
   Endif
   Call SKPCOM (Infile2, ENDFIL,'ODS')
  Enddo
211 Continue
   Err969 = .false.
   Do iUnp = 1, ncOvhg
     if (ReferenceToDefinition(iUnp) .eq. 0 .and. STODEF (iUNP) .ne. '')  then
         Err969 = .true.
         call ErrMsgStandard (969, 0, ' Storage definition not found in .Sto file.', StoDef(iUnp))
     endif
   Enddo
   If (RetVal .gt. 0)  call ErrMsgStandard (972, 0, ' Error reading unpaved input file Unpaved.Sto',' Getting STDF record')
   If (Err969) call ErrMsgStandard (972, 0, ' Not enough Unpaved data found', &
                              ' Some storage Definitions not present in Unpaved.Sto file')

! cleaning RR files
   If (CleanRRFiles) Call closeGP (Iounit)

! *********************************************************************
! ***  If CleanRRFiles, also write cleaned input for unpaved.alf
! *********************************************************************
   if (CleanRRFiles) then
        FileName = ConfFil_get_namFil(21)
        FileName(1:) = Filename(1:Len_trim(FileName)) // '_cleaned'
        Call Openfl (iounit, FileName,1,2)  !unpaved.alf_cleaned
        Write(*,*) ' Cleaning unpaved.alf to file:', FileName
        Write(iout1,*) ' Cleaning unpaved.alf to file:', FileName
   endif
! *********************************************************************
! ** Read Unpaved.Alf file
! **
! ** ALFA records
! *********************************************************************
  call SetMessage(LEVEL_DEBUG, 'Read Unpaved.Alf file')
  if (idebug .ne. 0) write(idebug,*) ' Read Unpaved.Alf file'
  Call SetReferenceToDefZero (ReferenceToDefinition)

  endfil = .false.
  teller = 0
  Retval = 0
  CALL SKPCOM (Infile3, ENDFIL, 'ODS')
  Do while (.not. endfil)
    READ (Infile3,'(A1000)',END=2111,ERR=150,IOSTAT=IECODE) STRING
    IF (STRING(1:4) .EQ. 'ALFA') Then
! Read alfa id
    Retval = Retval + GetVAR2 (STRING,' id ',1,' Unpaved_readAscii',' unpaved.alf file',IOUT1, &
                  CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
    name = CDUM(1)
    teller = teller + 1
! Eerst testen of alfa definition wel gebruikt wordt, dan pas verwerken
    IUnp = FindString (NcOvhg, Alfdef, Name, NcOvhg, CaseSensitive)
    Occurs = (Iunp .gt. 0)
    if (IUnp .gt. 0) then
       if (ReferenceToDefinition(iunp) .gt. 0) then
          call SetMessage(LEVEL_ERROR, 'Alfa Definition '//name(1:Len_trim(Name))//' double in datafile Unpaved.Alf')
          Occurs = .false. ! om verdere verwerking te stoppen
       else
           ! cleaning RR files
           If (CleanRRFiles) write(Iounit,'(A1000)') String
       endif
    endif
! Verwerk Alfa definition
    if (occurs) then
! Read alfa factors
      Retval = Retval + GetVRS2 (STRING,' af ',2,' UNPV-ReadAscii',' Unpaved.alf file', &
                    IOUT1, CDUM(1), RDUM(1), IDUM(1), 6, IflRtn)
      DO teller1 =1, 6
         ALFADUM(teller1) = RDUM(teller1)
      ENDDO
! Read three levels separating differents alfa layers
      Retval = Retval + GetVRS2 (STRING,' lv ',2,' UNPV-ReadAscii',' Unpaved.alf file', &
                    IOUT1, CDUM(1), RDUM(1), IDUM(1), 3, IflRtn)
      DO teller1 =1, 3
         LVLDUM(teller1) = RDUM(teller1)
      ENDDO
! Assign definition to individual nodes
      Do iUnp = 1, ncOvhg
        if (StringComp(AlfDef(Iunp), Name, CaseSensitive) )  then
          ReferenceToDefinition(iUnp) = teller
          ALFAOH(iUnp,1) = ALFADUM(1)
          ALFA2(iUnp,3)  = ALFADUM(2)
          ALFA2(iUnp,2)  = ALFADUM(3)
          ALFA2(iUnp,1)  = ALFADUM(4)
          ALFAOH(iUnp,2) = ALFADUM(5)
          ALFAOH(iUnp,3) = ALFADUM(6)
          LVLDRN(iUnp,3) = LVLDUM(1)
          LVLDRN(iUnp,2) = LVLDUM(2)
          LVLDRN(iUnp,1) = LVLDUM(3)
        endif
      Enddo
    Endif
   Endif
   CALL SKPCOM (Infile3, ENDFIL, 'ODS')
  Enddo
2111 Continue

   Err969 = .false.
   Do iUnp = 1, ncOvhg
     if (ReferenceToDefinition(iUnp) .eq. 0 .and. ALFDEF (iUNP) .ne. '')  then
         Err969 = .true.
         call ErrMsgStandard (969, 0, ' Alfa definition not found in .Alf file.', AlfDef(iUnp))
     endif
   Enddo
   If (RetVal .gt. 0) call ErrMsgStandard (972, 0, ' Error reading unpaved input file Unpaved.Alf',' Getting ALFA record')
   If (Err969) call ErrMsgStandard (972, 0, ' Not enough unpaved data found', &
                              ' Some ALFA Definitions not present in Unpaved.Alf file')

! *********************************************************************
! ** Read Unpaved.Alf file
! **
! ** ERNS records
! *********************************************************************
! ARS 1478 formule van Ernst; if Ernst formule is used extra info is read from the Alfa definition file

    Call SetReferenceToDefZero (ReferenceToDefinition)
    Rewind(infile3)
    endfil = .not. (ErnstDefined)
    if (.not. endfil) call SetMessage(LEVEL_DEBUG, 'Read Unpaved.Alf file; ERNS records')
    if (idebug .ne. 0) write(idebug,*) ' Read unpaved.Alf file; ERNS records'
    if (.not. endfil) Call SKPCOM (Infile3, ENDFIL,'ODS')
    Retval = 0

    Do while (.not. endfil)
      READ (Infile3,'(A1000)',END=2111,ERR=150,IOSTAT=IECODE) STRING
      IF (STRING(1:4) .EQ. 'ERNS') Then
! Read ERNS id
      Retval = Retval + GetVAR2 (STRING,' id ',1,' Unpaved_readAscii',' unpaved.alf file',IOUT1, &
                  CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
      name = CDUM(1)
      teller = teller + 1
! Eerst testen of Ernst definition wel gebruikt wordt, dan pas verwerken
      IUnp = FindString (NcOvhg, ErnstDef, Name, NcOvhg, CaseSensitive)
      Occurs = (Iunp .gt. 0)
      If (IUnp .gt. 0) then
         if (ReferenceToDefinition(iunp) .gt. 0) then
            call SetMessage(LEVEL_ERROR, 'Ernst Definition '//name(1:Len_trim(Name))//' double in datafile Unpaved.Alf')
            Occurs = .false. ! om verdere verwerking te stoppen
         else
            ! cleaning RR files
            If (CleanRRFiles) write(Iounit,'(A1000)') String
         endif
      endif
! Verwerk Ernst definition
      If (occurs) then
! Read Ernst resistances for surface runoff, infiltration inflow, and drainage outflow
        Retval = Retval + GetVRS2 (STRING,' cvs ',2,' UNPV-ReadAscii',' Unpaved.alf file', &
                      IOUT1, CDUM(1), RDUM(1), IDUM(1), 1, IflRtn)
        AlfaDum(1) = RDUM(1)
        Retval = Retval + GetVRS2 (STRING,' cvo ',2,' UNPV-ReadAscii',' Unpaved.alf file', &
                      IOUT1, CDUM(1), RDUM(1), IDUM(1), 4, IflRtn)
        DO teller1 =1,4
           AlfaDum(1+teller1) = RDUM(teller1)
        ENDDO
        Retval = Retval + GetVRS2 (STRING,' cvi ',2,' UNPV-ReadAscii',' Unpaved.alf file', &
                      IOUT1, CDUM(1), RDUM(1), IDUM(1), 1, IflRtn)
        AlfaDum(6) = RDUM(1)
! Read three levels separating differents alfa layers
        Retval = Retval + GetVRS2 (STRING,' lv ',2,' UNPV-ReadAscii',' Unpaved.alf file', &
                      IOUT1, CDUM(1), RDUM(1), IDUM(1), 3, IflRtn)
        DO teller1 =1, 3
           LvlDum(teller1) = RDUM(teller1)
        ENDDO
! Assign definition to individual nodes
        Do iUnp = 1, ncOvhg
          if (StringComp(ErnstDef(Iunp), Name, CaseSensitive) )  then
            ReferenceToDefinition(iUnp) = teller
            ErnstResistance(iUnp,1)  = ALFADUM(1)
            ErnstResistance(iUnp,2)  = ALFADUM(2)
            ErnstResistance(iUnp,3)  = ALFADUM(3)
            ErnstResistance(iUnp,4)  = ALFADUM(4)
            ErnstResistance(iUnp,5)  = ALFADUM(5)
            ErnstResistance(iUnp,6)  = ALFADUM(6)
            ErnstLevelDrains(iUnp,3) = LVLDUM(1)
            ErnstLevelDrains(iUnp,2) = LVLDUM(2)
            ErnstLevelDrains(iUnp,1) = LVLDUM(3)
            If (ErnstResistance(iUnp,4) .le. 0) ErnstResistance(iunp,4) = ErnstResistance(iunp,5)
            If (ErnstResistance(iUnp,3) .le. 0) ErnstResistance(iunp,3) = ErnstResistance(iunp,4)
            If (ErnstResistance(iunp,2) .le. 0) ErnstResistance(iunp,2) = ErnstResistance(iunp,3)
          endif
        Enddo
      Endif
     Endif
     CALL SKPCOM (Infile3, ENDFIL, 'ODS')
     Enddo
2611 Continue

     Err969 = .false.
     Do iUnp = 1, ncOvhg
       if (ReferenceToDefinition(iUnp) .eq. 0 .and. ErnstDef (iUNP) .ne. '')  then
           Err969 = .true.
           call ErrMsgStandard (969, 0, ' Ernst definition not found in .Alf file.', ErnstDef(iUnp))
       endif
     Enddo
     If (RetVal .gt. 0)  call ErrMsgStandard (972, 0, ' Error reading unpaved input file Unpaved.Alf',' Getting ERNS record')
     If (Err969) call ErrMsgStandard (972, 0, ' Not enough unpaved data found', &
                                ' Some ERNST Definitions not present in Unpaved.Alf file')

! cleaning RR files
     If (CleanRRFiles) Call closeGP (Iounit)

! *********************************************************************
! ***  If CleanRRFiles, also write cleaned input for unpaved.inf
! *********************************************************************
   if (CleanRRFiles) then
        FileName = ConfFil_get_namFil(31)
        FileName(1:) = Filename(1:Len_trim(FileName)) // '_cleaned'
        Call Openfl (iounit, FileName,1,2)  !unpaved.inf_cleaned
        Write(*,*) ' Cleaning unpaved.inf to file:', FileName
        Write(iout1,*) ' Cleaning unpaved.inf to file:', FileName
   endif
! *********************************************************************
! Read Unpaved.Inf file
! *********************************************************************
  call SetMessage(LEVEL_DEBUG, 'Read Unpaved.Inf file')
  if (idebug .ne. 0) write(idebug,*) ' Read unpaved.inf file'
  Call SetReferenceToDefZero (ReferenceToDefinition)

  endfil = .false.
  teller = 0
  Retval = 0
  CALL SKPCOM (Infile4, ENDFIL, 'ODS')
  Do while (.not. endfil)
    READ (Infile4,'(A1000)',END=3111,ERR=150,IOSTAT=IECODE) STRING
    IF (STRING(1:4) .EQ. 'INFC') Then
!   Read infiltration id
      Retval = Retval + GetVAR2 (STRING,' id ',1,' Unpaved_readAscii',' unpaved.inf file',IOUT1, &
                    CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
      name = CDUM(1)
      teller = teller + 1
!     Eerst testen of definitie wel gebruikt wordt
      IUnp = FindString (NcOvhg, Infdef, Name, NcOvhg, CaseSensitive)
      Occurs = (Iunp .gt. 0)
      if (IUnp .gt. 0) then
         if (ReferenceToDefinition(iunp) .gt. 0) then
            call SetMessage(LEVEL_ERROR, 'Infiltration Definition '//name(1:Len_trim(Name))//' double in datafile Unpaved.Inf')
            Occurs = .false. ! om verdere verwerking te stoppen
         else
            ! cleaning RR files
            If (CleanRRFiles) write(Iounit,'(A1000)') String
         endif
      endif
!     Verwerken infiltratie definitie
      if (occurs) then
!        Read infiltration definition
         Retval = Retval + GetVAR2 (STRING,' ic ',2,' unPaved_readAscii',' unpaved.inf file',IOUT1, &
                       CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
         infdum = RDUM(1)
!        Assign definition to individual nodes
         Do iUnp = 1, ncOvhg
           if (StringComp(InfDef(Iunp), Name, CaseSensitive) )  then
             ReferenceToDefinition(iunp) = teller
             INF_V(iUnp) = infdum
           endif
         enddo
      endif
    endif
    CALL SKPCOM (Infile4, ENDFIL, 'ODS')
  Enddo
3111 Continue

   Err969 = .false.
   Do iUnp = 1, ncOvhg
     if (ReferenceToDefinition(iUnp) .eq. 0 .and. INFDEF (iUNP) .ne. '')  then
         Err969 = .true.
         call ErrMsgStandard (969, 0, ' Infiltration definition not found in .INF file.', InfDef(iUnp))
     endif
   Enddo
   If (RetVal .gt. 0)  call ErrMsgStandard (972, 0, ' Error reading unpaved input file Unpaved.inf',' Getting INFC record')
   If (Err969) call ErrMsgStandard (972, 0, ' Not enough unpaved data found', &
                              ' Some infiltration Definitions not present in Unpaved.Inf file')

! cleaning RR files
     If (CleanRRFiles) Call closeGP (Iounit)

! *********************************************************************
! ***  If CleanRRFiles, also write cleaned input for unpaved.sep
! *********************************************************************
   if (CleanRRFiles) then
        FileName = ConfFil_get_namFil(33)
        FileName(1:) = Filename(1:Len_trim(FileName)) // '_cleaned'
        Call Openfl (iounit, FileName,1,2)  !unpaved.sep_cleaned
        Write(*,*) ' Cleaning unpaved.sep file:', FileName
        Write(iout1,*) ' Cleaning unpaved.sep file:', FileName
   endif

! *********************************************************************
! Read Unpaved.Sep file
! *********************************************************************
  call SetMessage(LEVEL_DEBUG, 'Read Unpaved.Sep file')
  if (idebug .ne. 0) write(idebug,*) ' Read unpaved.sep file'
  Call SetReferenceToDefZero (ReferenceToDefinition)

  endfil = .false.
  teller = 0
  Retval = 0
  CALL SKPCOM (Infile5, ENDFIL, 'ODS')
  Do while (.not. endfil)
    READ (Infile5,'(A1000)',END=4111,ERR=150,IOSTAT=IECODE) STRING
    IF (STRING(1:4) .EQ. 'SEEP') Then
!     Read seepage id
      Retval = Retval + GetVAR2 (STRING,' id ',1,' Unpaved_readAscii',' unpaved.sep file',IOUT1, &
                    CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
      name = CDUM(1)
      teller = teller + 1
!     Eerst testen of seepage definition wel gebruikt wordt, dan pas verwerken
      IUnp = FindString (NcOvhg, Sepdef, Name, NcOvhg, CaseSensitive)
      Occurs = (Iunp .gt. 0)
      if (IUnp .gt. 0) then
         if (ReferenceToDefinition(iunp) .gt. 0) then
           call SetMessage(LEVEL_ERROR, 'Seepage Definition '//name(1:Len_trim(Name))//' double in datafile Unpaved.Sep')
         endif
      endif
!     Verwerken Seepage definitie
      if (occurs) then
!     Read seepage or percolation
        allow = .true.
!       Computation option added Jan 2000;
!       If not present, set to default value 1 = constant seepage
        Retval = Retval + GetVAR2 (STRING,' co ',3,' unPaved_readAscii',' unpaved.sep file',IOUT1, &
                      CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
        if (.not. found) Idum(1) = 1
        dummyCompOption = Idum(1)
        allow = .false.
!       Read seepage data from file, depending on computation option
        dummyKwel    = 0.0
        dummyCvalue  = 1000.0
        dummyH0Table = ' '
        if (dummyCompOption .eq. 1) then
! 1 = constant seepage
           Retval = Retval + GetVAR2 (STRING,' sp ',2,' unPaved_readAscii',' unpaved.sep file',IOUT1, &
                         CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
           dummyKwel = RDUM(1)
           ! cleaning RR files
           If (CleanRRFiles) write(Iounit,'(A)') String (1:len_trim(String))
        elseif (dummyCompOption .ge. 2 .and. dummyCompOption .le. 3) then
! 2 = variable seepage with H0 from a table
! 3 = variable seepage with H0 on line from Modflow
           Retval = Retval + GetVAR2 (STRING,' cv ',2,' unPaved_readAscii',' unpaved.sep file',IOUT1, &
                         CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
           DummyCvalue = RDUM(1)
           if (dummyCompOption .eq. 2) then
             Retval = Retval + GetVAR2 (STRING,' h0 ',1,' unPaved_readAscii',' unpaved.sep file',IOUT1, &
                           CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
             DummyH0Table = CDUM(1)
             H0Defined = .true.
             ! cleaning RR files
             If (CleanRRFiles) write(Iounit,'(A)') String (1:len_trim(String))
           elseif (dummyCompOption .eq. 3) then
             OnLineModflowUsed = .true.
!            write(*,*) ' OnlineModFlowUsed= ',OnLineModflowUsed
             ! cleaning RR files
             If (CleanRRFiles) write(Iounit,'(A)') String (1:len_trim(String))
           endif
! 4 = variable seepage with seepage from a time table
        elseif (dummyCompOption .eq. 4) then
!            seepage directly specified as time series
             SeepageTableDefined = .true.
             UnpRefSeepage_TTable(iunp) = -1
! 5 = variable seepage with seepage from a time table and salt concentation from a separate time table
        elseif (dummyCompOption .eq. 5) then
!            seepage directly specified as time series
             SeepageTableDefined = .true.
             UnpRefSeepage_TTable(iunp) = -1
             SeepageConcTableDefined = .true.
             UnpRefSeepageConc_TTable(iunp) = -1
        else ! unknown value; error message
           call ErrMsgStandard (972, 0, ' Unknown seepage computation option in input file Unpaved.sep',' Error getting SEEP record')
        endif
!     Read seepage salt concentration
        if (dummyCompOption .le. 4) then
            ! constant
            Retval = Retval + GetVAR2 (STRING,' ss ',2,' unPaved_readAscii',' unpaved.sep file',IOUT1, &
                                       CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
            dummyConc = RDUM(1)
        else
            ! ref. to timetable of salt concentrations
            Retval = Retval + GetVAR2 (STRING,' ss ',1,' unPaved_readAscii',' unpaved.sep file',IOUT1, &
                                       CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
            dummyConc = 0.
            DummyConcTable = CDUM(1)
        endif
!       Assign definition to individual nodes
        Do iNod=1,ncNode
          if (EiNode(Inod, 3) .eq. 2) then
            iUnp = EiNode(Inod, 2)
            if (StringComp(SepDef(Iunp), Name, CaseSensitive) )  then
              ReferenceToDefinition(iunp) = teller
!             22-12-1997: kwel en wegzg beiden positief gedefinieerd
              if (dummyKwel .ge. 0) then
                kwel(iUnp) = dummyKwel
                wegzg(iUnp) = 0.0
                SltKwl(iNod) = dummyConc
              else
                wegzg(iUnp) = -dummyKwel
                kwel(iUnp) = 0.0
              endif
              SeepageCompOption(iUnp) = DummyCompOption
              CValue(iUnp) = max (0.001d0, DummyCValue)
              H0Def (iUnp) = DummyH0Table
              SaltConcDef(iUnp)= DummyConcTable
            endif
          endif
        enddo
      endif
   endif
   CALL SKPCOM (Infile5, ENDFIL, 'ODS')
  Enddo
4111 Continue

   Err969 = .false.
   Do iUnp = 1, ncOvhg
     if (ReferenceToDefinition(iUnp) .eq. 0 .and. SEPDEF (iUNP) .ne. '')  then
         Err969 = .true.
         call ErrMsgStandard (969, 0, ' Seepage definition not found in .SEP file.', SepDef(iUnp))
     endif
   Enddo
   If (RetVal .gt. 0)  call ErrMsgStandard (972, 0, ' Error reading unpaved input file Unpaved.sep',' Getting SEEP record')
   If (Err969) call ErrMsgStandard (972, 0, ' Not enough unpaved data found', &
                              ' Some Seepage Definitions not present in Unpaved.Sep file')


! Variable seepage definitions (time series)
! *********************************************************************
! SEEP records lezen met time series of seepage (computation option 4 or 5)
! *********************************************************************
    Rewind(infile5)
    endfil = .not. (SeepageTableDefined)
    if (.not. endfil) call SetMessage(LEVEL_DEBUG, 'Read Unpaved.Sep file; SEEP co 4 records')
    if (idebug .ne. 0) write(idebug,*) ' Read unpaved.Sep file; SEEP co 4 records'
    if (.not. endfil) Call SKPCOM (Infile5, ENDFIL,'ODS')
    Do while (.not. endfil)
       Success = GetRecord(Infile5, 'SEEP', Endfil, idebug, Iout1)     ! get record van keyword SEEP tot seep, zet in buffer
       IF (.not. success) GOTO 4611
       IF (ENDFIL) GOTO 4611
       Success = GetStringFromBuffer (KeepBufString)
       IF (.not. Success .and. CleanRRFiles)   then
           Write(*,*) 'local buffer UnpavedModule too small, SEEP record'
           Write(iout1,*) 'local buffer UnpavedModule too small, SEEP record'
           GOTO 4611
       Endif
       Success = GetTableName (TabYesNo, TableName, ' id ', Iout1)     ! get table name via keyword ' id ', TabYesNo=TBLE found
       IF (.not. success) GOTO 4611
       If (TabYesNo .and. TableName .ne. '') Then
!         Er is een tabel gedefinieerd, met een niet-lege naam
          NrColumns = 1
!         Eerst testen of SeepageDefinition wel gebruikt wordt, dan pas verwerken
          IUnp = FindString (NcOvhg, Sepdef, TableName, NcOvhg, CaseSensitive)
          Occurs = (Iunp .gt. 0)
          if (IUnp .gt. 0) then
             if (UnpRefSeepage_TTable(iunp) .eq. 0) then
                 occurs = .false.
             elseif (UnpRefSeepage_TTable(iunp) .gt. 0) then
                 call SetMessage(LEVEL_ERROR, 'Seepage Table Definition '//Tablename(1:Len_trim(TableName))//' double in datafile Unpaved.Tbl')
                 NrColumns = 0     ! om verdere verwerking uit te zetten
             endif
          endif
!         Verwerken Seepage definitie
          if (occurs .and. NrColumns .gt. 0) then
! Get table with name TableName, Nrcolumns data fields, result in global arrays; tabel nummer is TableNr
            Success = GetTable (TableHandle, TableName, NrColumns, TableNr, idebug, Iout1)
            IF (.not. success) GOTO 4611
! clean RR files
            If (CleanRRFiles) then
              ! use KeepBufString to write to file
              ! first till TBLE
              ! then till < characters
              ! then till the end of the buffer string
              lenstring = len_trim(KeepBufString)
              ipos  = FndFrst ('TBLE ',KeepBufString(1:lenstring),.false.)
              if (ipos .gt. 0) then
                 write(Iounit,'(A)') KeepBufString (1:ipos+4)
                 KeepBufString(1:) = KeepBufString(ipos+5:)
              else
                 ! error: no TBLE found
                   call SetMessage(LEVEL_ERROR, 'Structure Table Definition '//Tablename(1:Len_trim(TableName))//' TBLE not found')
              endif
 1031         continue
              lenstring = len_trim(KeepBufString)
              ipos  = FndFrst (' < ',KeepBufString(1:lenstring),.false.)
              if (ipos .gt. 0) then
                 write(Iounit,'(A)') KeepBufString (1:ipos+2)
                 KeepBufString(1:) = KeepBufString(ipos+3:)
                 goto 1031
              else
                 ! write remaining part
                 write(Iounit,'(A)') KeepBufString (1:lenstring)
              endif
            Endif
! Set references
            Do iUnp = 1, ncOvhg
              if (StringComp(SepDef(Iunp), TableName, CaseSensitive) )  UnpRefSeepage_TTable(iUnp) = TableNr
            Enddo
          Endif
       Endif
       Call SKPCOM (Infile5, ENDFIL,'ODS')
     Enddo
4611 Continue

     Err969 = .false.
     Do iUnp = 1, ncOvhg
        if (UnpRefSeepage_TTable(iUnp) .lt. 0) then
            Err969 = .true.
            call ErrMsgStandard (969, 0, ' Seepage table definition not found in Unpaved.Sep file.', SepDef(iUnp))
        endif
     Enddo

     If (Err969) call ErrMsgStandard (972, 0, ' Not enough unpaved data found', &
                                      ' Some Seepage Table Definitions not present in Unpaved.Sep file')


! cleaning RR files
     If (CleanRRFiles) Call closeGP (Iounit)

! *********************************************************************
! ***  If CleanRRFiles, also write cleaned input for unpaved.tbl
! *********************************************************************
   if (CleanRRFiles) then
        FileName = ConfFil_get_namFil(34)
        FileName(1:) = Filename(1:Len_trim(FileName)) // '_cleaned'
        Call Openfl (iounit, FileName,1,2)  !unpaved.tbl_cleaned
        Write(*,*) ' Cleaning unpaved.tbl file:', FileName
        Write(iout1,*) ' Cleaning unpaved.tbl file:', FileName
   endif

! *********************************************************************
! Read Unpaved.Tbl: de tabellen met initial groundwaterlevels van onverharde gebieden (IG_T records)
!                    en de S-curve tabellen (SC_T records)
! records kunnen over meerdere regels verspreid staan!!
! *********************************************************************

! IG_T records lezen als er verwezen wordt naar InitGwl definities (InitGwlDefined=.true.)
    endfil = .not. (InitGwlDefined)
    if (.not. endfil) call SetMessage(LEVEL_DEBUG, 'Read Unpaved.Tbl file; IG_T records')
    if (idebug .ne. 0) write(idebug,*) ' Read unpaved.Tbl file IG_T records'
    if (.not. endfil) Call SKPCOM (Infile6, ENDFIL,'ODS')
    Do while (.not. endfil)
       Success = GetRecord(Infile6, 'IG_T', Endfil, idebug, Iout1)     ! get record van keyword IG_T tot ig_t, zet in buffer
       IF (.not. success) GOTO 5111
       IF (ENDFIL) GOTO 5111
       Success = GetStringFromBuffer (KeepBufString)
       IF (.not. Success .and. CleanRRFiles)   then
           Write(*,*) 'local buffer UnpavedModule too small, IG_T record'
           Write(iout1,*) 'local buffer UnpavedModule too small, IG_T record'
           GOTO 5111
       Endif
       Success = GetTableName (TabYesNo, TableName, ' id ', Iout1)     ! get table name via keyword ' id ', TabYesNo=TBLE found
       IF (.not. success) GOTO 5111
       If (TabYesNo .and. TableName .ne. '') Then
!         Er is een tabel gedefinieerd, met een niet-lege naam
!         Eerst testen of tabel definition wel gebruikt wordt, dan pas verwerken
          NrColumns = 1
          IUnp = FindString (NcOvhg, Inidef, TableName, NcOvhg, CaseSensitive)
          Occurs = (Iunp .gt. 0)
          if (IUnp .gt. 0) then
             if (UnpRefIg_TTable(iunp) .gt. 0) then
               call SetMessage(LEVEL_ERROR, 'Init.gwl Table Definition '//Tablename(1:Len_trim(TableName))// ' double in datafile Unpaved.Tbl')
               NrColumns = 0     ! om verdere verwerking uit te zetten
             endif
          endif
!         Verwerken Init gwl definitie
          if (occurs .and. NrColumns .gt. 0) then
! Get table with name TableName, Nrcolumns data fields, result in global arrays; tabel nummer is TableNr
            Success = GetTable (TableHandle, TableName, NrColumns, TableNr, idebug, Iout1)
            IF (.not. success) GOTO 5111
! clean RR files
            If (CleanRRFiles) then
              ! use KeepBufString to write to file
              ! first till TBLE
              ! then till < characters
              ! then till the end of the buffer string
              lenstring = len_trim(KeepBufString)
              ipos  = FndFrst ('TBLE ',KeepBufString(1:lenstring),.false.)
              if (ipos .gt. 0) then
                 write(Iounit,'(A)') KeepBufString (1:ipos+4)
                 KeepBufString(1:) = KeepBufString(ipos+5:)
              else
                 ! error: no TBLE found
                   call SetMessage(LEVEL_ERROR, 'Structure Table Definition '//Tablename(1:Len_trim(TableName))//' TBLE not found')
              endif
 1041         continue
              lenstring = len_trim(KeepBufString)
              ipos  = FndFrst (' < ',KeepBufString(1:lenstring),.false.)
              if (ipos .gt. 0) then
                 write(Iounit,'(A)') KeepBufString (1:ipos+2)
                 KeepBufString(1:) = KeepBufString(ipos+3:)
                 goto 1041
              else
                 ! write remaining part
                 write(Iounit,'(A)') KeepBufString (1:lenstring)
              endif
            Endif
! Set references
            Do iUnp = 1, ncOvhg
              if (StringComp (IniDef(Iunp), TableName, CaseSensitive) ) UnpRefIG_TTable(iUnp) = TableNr
            Enddo
          Endif
       Endif
       Call SKPCOM (Infile6, ENDFIL,'ODS')
     Enddo
5111 Continue

! *********************************************************************
! H0_T records lezen als er verwezen wordt naar H0 definities (H0Defined=.true.)
! *********************************************************************
    Rewind(infile6)
    endfil = .not. (H0Defined)
    if (.not. endfil) call SetMessage(LEVEL_DEBUG, 'Read Unpaved.Tbl file; H0_T records')
    if (idebug .ne. 0) write(idebug,*) ' Read unpaved.Tbl file; H0_T records'
    if (.not. endfil) Call SKPCOM (Infile6, ENDFIL,'ODS')
    Do while (.not. endfil)
       Success = GetRecord(Infile6, 'H0_T', Endfil, idebug, Iout1)     ! get record van keyword H0_T tot h0_t, zet in buffer
       IF (.not. success) GOTO 5611
       IF (ENDFIL) GOTO 5611
       Success = GetStringFromBuffer (KeepBufString)
       IF (.not. Success .and. CleanRRFiles)   then
           Write(*,*) 'local buffer UnpavedModule too small, H0_T record'
           Write(iout1,*) 'local buffer UnpavedModule too small, H0_T record'
           GOTO 5611
       Endif
       Success = GetTableName (TabYesNo, TableName, ' id ', Iout1)     ! get table name via keyword ' id ', TabYesNo=TBLE found
       IF (.not. success) GOTO 5611
       If (TabYesNo .and. TableName .ne. '') Then
!         Er is een tabel gedefinieerd, met een niet-lege naam
          NrColumns = 1
!         Eerst testen of H0definition wel gebruikt wordt, dan pas verwerken
          IUnp = FindString (NcOvhg, H0def, TableName, NcOvhg, CaseSensitive)
          Occurs = (Iunp .gt. 0)
          if (IUnp .gt. 0) then
             if (UnpRefH0_TTable(iunp) .gt. 0) then
                 call SetMessage(LEVEL_ERROR, 'H0 Table Definition '//Tablename(1:Len_trim(TableName))//' double in datafile Unpaved.Tbl')
                 NrColumns = 0  ! om verdere verwerking te stoppen
             endif
          endif
!         Verwerken H0 definitie
          if (occurs .and. NrColumns .gt. 0) then
! Get table with name TableName, Nrcolumns data fields, result in global arrays; tabel nummer is TableNr
            Success = GetTable (TableHandle, TableName, NrColumns, TableNr, idebug, Iout1)
            IF (.not. success) GOTO 5611
! clean RR files
            If (CleanRRFiles) then
              ! use KeepBufString to write to file
              ! first till TBLE
              ! then till < characters
              ! then till the end of the buffer string
              lenstring = len_trim(KeepBufString)
              ipos  = FndFrst ('TBLE ',KeepBufString(1:lenstring),.false.)
              if (ipos .gt. 0) then
                 write(Iounit,'(A)') KeepBufString (1:ipos+4)
                 KeepBufString(1:) = KeepBufString(ipos+5:)
              else
                 ! error: no TBLE found
                   call SetMessage(LEVEL_ERROR, 'Structure Table Definition '//Tablename(1:Len_trim(TableName))//' TBLE not found')
              endif
 1051         continue
              lenstring = len_trim(KeepBufString)
              ipos  = FndFrst (' < ',KeepBufString(1:lenstring),.false.)
              if (ipos .gt. 0) then
                 write(Iounit,'(A)') KeepBufString (1:ipos+2)
                 KeepBufString(1:) = KeepBufString(ipos+3:)
                 goto 1051
              else
                 ! write remaining part
                 write(Iounit,'(A)') KeepBufString (1:lenstring)
              endif
            Endif
! Set references
            Do iUnp = 1, ncOvhg
              if (StringComp(H0Def(Iunp), TableName, CaseSensitive) )  UnpRefH0_TTable(iUnp) = TableNr
            Enddo
          Endif
       Endif
       Call SKPCOM (Infile6, ENDFIL,'ODS')
     Enddo
5611 Continue


! *********************************************************************
! SPCO records lezen als er verwezen wordt naar salt concentration time tables (SeepageSaltConcDefined = true)
! *********************************************************************
    Rewind(infile6)
    endfil = .not. (SeepageConcTableDefined)
    if (.not. endfil) call SetMessage(LEVEL_DEBUG, 'Read Unpaved.Tbl file; SPCO records')
    if (idebug .ne. 0) write(idebug,*) ' Read unpaved.Tbl file; SPCO records'
    if (.not. endfil) Call SKPCOM (Infile6, ENDFIL,'ODS')
    Do while (.not. endfil)
       Success = GetRecord(Infile6, 'SPCO', Endfil, idebug, Iout1)     ! get record van keyword SPCO tot spco, zet in buffer
       IF (.not. success) GOTO 5711
       IF (ENDFIL) GOTO 5711
       Success = GetStringFromBuffer (KeepBufString)
       IF (.not. Success .and. CleanRRFiles)   then
           Write(*,*) 'local buffer UnpavedModule too small, SPCO record'
           Write(iout1,*) 'local buffer UnpavedModule too small, SPCO record'
           GOTO 5711
       Endif
       Success = GetTableName (TabYesNo, TableName, ' id ', Iout1)     ! get table name via keyword ' id ', TabYesNo=TBLE found
       IF (.not. success) GOTO 5711
       If (TabYesNo .and. TableName .ne. '') Then
!         Er is een tabel gedefinieerd, met een niet-lege naam
          NrColumns = 1
!         Eerst testen of H0definition wel gebruikt wordt, dan pas verwerken
          IUnp = FindString (NcOvhg, SaltConcdef, TableName, NcOvhg, CaseSensitive)
          Occurs = (Iunp .gt. 0)
          if (IUnp .gt. 0) then
             if (UnpRefSeepageConc_TTable(iunp) .gt. 0) then
                 call SetMessage(LEVEL_ERROR, 'Seepage Salt concentration Table Definition '//Tablename(1:Len_trim(TableName))//' double in datafile Unpaved.Tbl')
                 NrColumns = 0     ! om verdere verwerking uit te zetten
             endif
          endif
!         Verwerken Salt conc definitie
          if (occurs .and. NrColumns .gt. 0) then
! Get table with name TableName, Nrcolumns data fields, result in global arrays; tabel nummer is TableNr
            Success = GetTable (TableHandle, TableName, NrColumns, TableNr, idebug, Iout1)
            IF (.not. success) GOTO 5711
! clean RR files
            If (CleanRRFiles) then
              ! use KeepBufString to write to file
              ! first till TBLE
              ! then till < characters
              ! then till the end of the buffer string
              lenstring = len_trim(KeepBufString)
              ipos  = FndFrst ('TBLE ',KeepBufString(1:lenstring),.false.)
              if (ipos .gt. 0) then
                 write(Iounit,'(A)') KeepBufString (1:ipos+4)
                 KeepBufString(1:) = KeepBufString(ipos+5:)
              else
                 ! error: no TBLE found
                   call SetMessage(LEVEL_ERROR, 'Structure Table Definition '//Tablename(1:Len_trim(TableName))//' TBLE not found')
              endif
 1061         continue
              lenstring = len_trim(KeepBufString)
              ipos  = FndFrst (' < ',KeepBufString(1:lenstring),.false.)
              if (ipos .gt. 0) then
                 write(Iounit,'(A)') KeepBufString (1:ipos+2)
                 KeepBufString(1:) = KeepBufString(ipos+3:)
                 goto 1061
              else
                 ! write remaining part
                 write(Iounit,'(A)') KeepBufString (1:lenstring)
              endif
            Endif
! Set references
            Do iUnp = 1, ncOvhg
              if (StringComp(SaltConcDef(Iunp), TableName, CaseSensitive) )  UnpRefSeepageConc_TTable(iUnp) = TableNr
            Enddo
          Endif
       Endif
       Call SKPCOM (Infile6, ENDFIL,'ODS')
     Enddo
5711 Continue


! *********************************************************************
!   Scurve records, alleen als ScurveDefined = .true.
! *********************************************************************
    Rewind(infile6)
    KlTeken = '<'
    endfil = .not. ScurveDefined
    if (.not. endfil) call SetMessage(LEVEL_INFO, 'Read Unpaved.Tbl file; Scurve')
    if (.not. endfil) Call SKPCOM (Infile6, ENDFIL,'ODS')
    Do while (.not. endfil)
       Success = GetRecord(Infile6, 'SC_T', Endfil, idebug, Iout1)    ! get record van keyword INST tot inst, zet in buffer
       IF (.not. success) GOTO 6111
       IF (ENDFIL) GOTO 6111
       Success = GetStringFromBuffer (KeepBufString)
       IF (.not. Success .and. CleanRRFiles)   then
           Write(*,*) 'local buffer UnpavedModule too small, SC_T record'
           Write(iout1,*) 'local buffer UnpavedModule too small, SC_T record'
           GOTO 6111
       Endif
       Success = GetTableName (TabYesNo, TableName, ' id ', Iout1)     ! get table name via keyword ' id ', if table defined
       IF (.not. success) GOTO 6111
       If (TabYesNo .and. TableName .ne. '') Then
!         Er is een tabel gedefinieerd, met een niet-lege naam
          NrColumns = 1   ! voor Scurve 1 kolom
!         Eerst testen of SCdefinition wel gebruikt wordt, dan pas verwerken
          IUnp = FindString (NcOvhg, SCuDef, TableName, NcOvhg, CaseSensitive)
          Occurs = (Iunp .gt. 0)
!         NB nog niet getest of Scurve definitie 2 keer voorkomt !!
!         Verwerken Scurve definitie
          if (occurs) then
! Get SCurve Table
             success = GetStringFromBuffer(BufString)
             IF (.not. success) GOTO 6111
             Ileft  = INDEX(BufString(1:nbuf), 'TBLE') +4
             Iright = INDEX(BufString(1:nbuf), 'tble')
             NrScurvePoints = Max (1, CntStr (klteken, BufString(ileft:iright)) )
             if (NrScurvePoints .gt. 999) then
                 call SetMessage(LEVEL_FATAL, 'Reduce input S curve to 999 points')
             endif
             ! get levels and percentages; eerst < tekens verwijderen, dan free format inlezen
             do idum1=ileft, iright
                if (BufString(idum1:idum1) .eq. klteken) BufString(idum1:idum1)=' '
             enddo
             read (BufString(ileft:),*)  (SCurvePercentage(ipoint), ScurveLevel (ipoint), ipoint=1, NrSCurvePoints)
! ARS 14203: check that Scurve start with 0% level and ends with 100% level
             If (ScurvePercentage(1) .gt. 0.0 .or. ScurvePercentage(NrSCurvePoints) .lt. 100.0) then
                String(1:) = ' '
                String(1:) = ' for Scurve Table: '
                String(1:) = String(1:Len_trim(String)) // TableName
                call ErrMsgStandard (974, 0, String, ' Unpaved_ConfAr1' )
                call ErrMsgStandard (974, 0, &
                            ' Scurve table does not start at 0% level or end at 100% level! SOBEK-RR will extrapolate.',&
                            ' Unpaved_ConfAr1' )
             Endif
! ARS 14203: also check Scurve is increasing
             do ipoint=2,NrScurvePoints
                If (ScurvePercentage(ipoint-1) .gt. ScurvePercentage(ipoint) .or. &
                     ScurveLevel(ipoint-1) .gt. ScurveLevel(ipoint)) then
                   String(1:) = ' '
                   String(1:) = ' for Scurve Table: '
                   String(1:) = String(1:Len_trim(String)) // TableName
                   call ErrMsgStandard (974, 0, String, ' Unpaved_ConfAr1' )
                   call ErrMsgStandard (972, 0, &
                              ' Scurve percentages or levels table not specified in increasing order! ', TableName )
                Endif
             enddo
! end ARS 14203
! clean RR files
             If (CleanRRFiles) then
               ! use KeepBufString to write to file
               ! first till TBLE
               ! then till < characters
               ! then till the end of the buffer string
               lenstring = len_trim(KeepBufString)
               ipos  = FndFrst ('TBLE ',KeepBufString(1:lenstring),.false.)
               if (ipos .gt. 0) then
                  write(Iounit,'(A)') KeepBufString (1:ipos+4)
                  KeepBufString(1:) = KeepBufString(ipos+5:)
               else
                  ! error: no TBLE found
                    call SetMessage(LEVEL_ERROR, 'Structure Table Definition '//Tablename(1:Len_trim(TableName))//' TBLE not found')
               endif
 1071          continue
               lenstring = len_trim(KeepBufString)
               ipos  = FndFrst (' < ',KeepBufString(1:lenstring),.false.)
               if (ipos .gt. 0) then
                  write(Iounit,'(A)') KeepBufString (1:ipos+2)
                  KeepBufString(1:) = KeepBufString(ipos+3:)
                  goto 1071
               else
                  ! write remaining part
                  write(Iounit,'(A)') KeepBufString (1:lenstring)
               endif
             Endif
             if (idebug .ne. 0) then
               do ipoint=1,NrScurvePoints
                  write(idebug,*) ' ScurvePoints % and lvl', SCurvePercentage(ipoint),ScurveLevel(ipoint)
               enddo
             endif
! Set references
             Do iUnp = 1, ncOvhg
               If (StringComp(SCuDef(Iunp), TableName, CaseSensitive) )  Then
                   ScuDef(iunp) = ''
!                  Convert input to computation input in array AreaScurve (Novh, UseUnpavedScurve)
!                  Input array met percentages en levels kan zowel langer als korter zijn!!
                   ScurveIncrement = 1. / float (UseUnpavedScurve) * 100.
                   AreaLower = 0.0
                   Do ipoint=0,UseUnpavedScurve
!                     interpoleer met cumulatieve percentages
                      AreaScurve(iUnp,ipoint)%Percentage = float (ipoint) * ScurveIncrement
                      CALL RR_D_INTERP (NrScurvePoints, SCurvePercentage, ScurveLevel, &
                                                     AreaScurve(iUnp,ipoint)%Percentage, AreaScurve(iUnp,ipoint)%Level,IDUM2)
                      AreaScurve(iUnp,ipoint)%Area = AreaScurve(iUnp,ipoint)%Percentage /100. * AreaOh(iUnp) - AreaLower
                      AreaLower = AreaLower + AreaScurve(iUnp,ipoint)%Area
!                     zet percentage nu op increment
                      AreaScurve(iUnp,ipoint)%Percentage = SCurveIncrement
                      if (CompOption(iUnp) .le. 2) then
                       if (PositionAlfa .eq. 0) then    !alfa's in absolute nivo's, tov LvlOh
                         AreaScurve(iUnp,ipoint)%AlfaLevels(1) = lvloh(iUnp) - lvldrn(iUnp,1)
                         AreaScurve(iUnp,ipoint)%AlfaLevels(2) = lvloh(iUnp) - lvldrn(iUnp,2)
                         AreaScurve(iUnp,ipoint)%AlfaLevels(3) = lvloh(iUnp) - lvldrn(iUnp,3)
                       else  ! alfa's in relatieve nivo's, tov actueel maaiveld
                         AreaScurve(iUnp,ipoint)%AlfaLevels(1) = AreaScurve(iUnp,ipoint)%Level - lvldrn(iUnp,1)
                         AreaScurve(iUnp,ipoint)%AlfaLevels(2) = AreaScurve(iUnp,ipoint)%Level - lvldrn(iUnp,2)
                         AreaScurve(iUnp,ipoint)%AlfaLevels(3) = AreaScurve(iUnp,ipoint)%Level - lvldrn(iUnp,3)
                       endif
                      Elseif (CompOption(iUnp) .eq. 3) then   ! Ernst
                       if (PositionAlfa .eq. 0) then    !alfa's in absolute nivo's, tov LvlOh
                         AreaScurve(iUnp,ipoint)%AlfaLevels(1) = lvloh(iUnp) - ErnstLevelDrains(iUnp,1)
                         AreaScurve(iUnp,ipoint)%AlfaLevels(2) = lvloh(iUnp) - ErnstLevelDrains(iUnp,2)
                         AreaScurve(iUnp,ipoint)%AlfaLevels(3) = lvloh(iUnp) - ErnstLevelDrains(iUnp,3)
                       else  ! alfa's in relatieve nivo's, tov actueel maaiveld
                         AreaScurve(iUnp,ipoint)%AlfaLevels(1) = AreaScurve(iUnp,ipoint)%Level - ErnstLevelDrains(iUnp,1)
                         AreaScurve(iUnp,ipoint)%AlfaLevels(2) = AreaScurve(iUnp,ipoint)%Level - ErnstLevelDrains(iUnp,2)
                         AreaScurve(iUnp,ipoint)%AlfaLevels(3) = AreaScurve(iUnp,ipoint)%Level - ErnstLevelDrains(iUnp,3)
                       endif
                      Endif
!                     Idebug = IdebugLunRR
                       if (idebug .ne. 0) then
                         write(idebug,*) ' AreaScurve (iUnp,ipoint)    ',iUnp, ipoint
                         write(idebug,*) '     Percentage ',AreaScurve (iUnp,ipoint)%Percentage
                         write(idebug,*) '     Level      ',AreaScurve (iUnp,ipoint)%Level
                         write(idebug,*) '     Area       ',AreaScurve (iUnp,ipoint)%Area
                         write(idebug,*) '     AlfaLevels ',(AreaScurve (iUnp,ipoint)%Alfalevels(teller),teller=1,3)
                       Endif
                      Idebug = 0
                   Enddo
               Endif
             Enddo
           endif
        Endif
        Call SKPCOM (Infile6, ENDFIL,'ODS')
     Enddo
 6111 Continue

! cleaning RR files
     If (CleanRRFiles) Call closeGP (Iounit)

! *********************************************************************
! Check of alle referenties naar tabellen opgelost
! NB Alle SCurves zijn gedefinieerd als er geen geen verwijzingen naar definities ScuDef meer zijn
! *********************************************************************
    Err969 = .false.
    Do iUnp = 1, ncOvhg
!      zet maximum Level maaiveld met of zonder S curve
       If (UseScurve(iUnp) .eq. 0) then
           LvlOhMx(iUnp) = LvlOh(iUnp)   !geen Scurve
       Else
           LvlOhMx(iUnp) = AreaScurve(iUnp,UseUnpavedScurve)%Level  !Met Scurve
       Endif
! Check of alle referenties naar tabellen opgelost
       if (UnpRefIG_TTable(iUnp) .eq. 0 .and. INIDEF (iUnp) .ne. '') then
           Err969 = .true.
           call ErrMsgStandard (969, 0, ' Initial gwl table not found in Unpaved.Tbl file.', IniDef(iUnp))
       endif
       if (UnpRefH0_TTable(iUnp) .eq. 0 .and. H0DEF (iUnp) .ne. '') then
           Err969 = .true.
           call ErrMsgStandard (969, 0, ' H0 table definition not found in Unpaved.Tbl file.', H0Def(iUnp))
       endif
       if (UseScurve(iUnp) .gt. 0 .and. SCUDEF (iUnp) .ne. '')  then
           Err969 = .true.
           call ErrMsgStandard (969, 0, ' Scurve definition not found in Unpaved.Tbl file.', SCUDef(iUnp))
       endif
    Enddo

    If (Err969) call ErrMsgStandard (972, 0, ' Not enough unpaved data found', &
                                     ' Some Table Definitions not present in Unpaved.Tbl file')


! allerlei checks, in een lus over alle onverharde gebieden
  do iUnp = 1, ncOvhg
!     GP sept 1997 conversie INF_V van mm/uur naar m/s.
      INF_V(iUnp) = INF_V(iUnp) / NRSHR * MM2M


!     bmaxol en bminol omzetten van mm naar m3
      bmaxol(iunp) = bmaxol(iunp) * areaOh(iunp) * MM2M
      biniol(iunp) = biniol(iunp) * areaOh(iunp) * MM2M

!     GP march 1999: useful definition of Maximum allowable groundwaterlevel = maaiveld;
!     Dan output aantal uren overschrijding geeft aan tijdsduur gw op maaiveld.
      MaxGwl(iUnp) = LvlOh(iUnp)

!     Aanpassing alfa-factors en nivo's van drains
      do teller2 = 1, 3
        alfaOh(iUnp, teller2) = max(0.000001d0, alfaOh(iUnp, teller2))
        alfaOh(iUnp, teller2) = alfaOh(iUnp, teller2) / nrsDay
        alfa2(iUnp, teller2) = max(0.000001d0, alfa2(iUnp, teller2))
        alfa2(iUnp, teller2) = alfa2(iUnp, teller2) / nrsDay
        lvlDrn(iUnp, teller2) = lvlOh(iUnp) - lvlDrn(iUnp, teller2)
        ErnstLevelDrains(iUnp, teller2) = lvlOh(iUnp) - ErnstLevelDrains(iUnp, teller2)
      end do

!     Als Krayenhoff-vdLeur en geen alfa's opgegeven
!     Zet dan alfa voor afstroming over oppervlak groot (directe afstroming)
      if (CompOption(iunp) .eq. 2 .and. alfaoh(iunp,1) .le. .000001) alfaOh(iunp, 1) = 100./NrsDay

!     BINIBD (iUnp) = AREAOH (iUnp) * iniDepthGwl (iUnp)
      BINIBD (iUnp) = AreaGwComp (iUnp) * iniDepthGwl (iUnp)

! ARS xxxx  3 dec 2001
! Als Ernst optie gebruikt wordt, zet ErnstLevelDrains ook in LVLDrn ivm correcte bepaling deltaH in subroutine SetDeltaH
      if (CompOption(iunp) .eq. 3) then
         do teller2 = 1, 3
           lvlDrn(iUnp, teller2) = ErnstLevelDrains(iUnp, teller2)
         end do
      endif
  enddo


  DeALLOCATE    (STODEF, ALFDEF, ErnstDef, SEPDEF, INFDEF, &
                 INIDEF, SCUDEF, H0DEF, ReferenceToDefinition)
  Deallocate (AlreadyRead)


! feb 2002: move call RdBerg to top of ReadUnpavedAscii
! call RdBerg

   goto 999

150 CONTINUE
    call SetMessage(LEVEL_FATAL, 'Read error in Unpaved ascii')

999 continue

  return
  end subroutine unpaved_readAsciiInput


  Subroutine SetReferenceToDefZero (ReferenceToDefinition)

  Integer ReferenceToDefinition(ncovhg)
  Integer iunp

  Do iunp = 1, ncovhg
     ReferenceToDefinition(iunp) = 0
  Enddo

  return
  end subroutine SetReferenceToDefZero



  SUBROUTINE Unpaved_INITBC (IMAAND, IDAG)
    !*********************************************************************
    !*** Last update: 6 March 1997       By : Peter Schrier
    !*********************************************************************
    !*** Brief description:
    !*** ------------------
    !***   Initialisatie bergingscoefficient
    !*********************************************************************
    !*** Input/output parameters:
    !*** ------------------------
    !***  IMAAND: maand van de tijdstap
    !***  IDAG: dag in de maand
    !***  hour: uur in de dag
    !***  minute: verstreken minuten in het uur
    !*********************************************************************

    ! declaration of variables

    INTEGER IMAAND, IDAG, INODE, IOVH, IOW, Ibnd, IBOT, rowNr, ipoint, NAverage
    Double precision    OWVAL, iniLevel, owLevel, HdeZBergCoef, AverageStorageCoefficient
    type (Date) currentDate
    type (Time) currentTime
    Integer iDebug, IOut1, TabelNr
    Logical DateTimeOutsideTable

    ! end declarations

    iDebug = ConfFil_get_iDebug()
    iOut1  = ConfFil_get_iOut1()

    if (idebug .ne. 0) WRITE (IDEBUG,1)
  1 FORMAT (' Unpaved_INITBC')

    !*********************************************************************
    !*** Initialiseer bergingscoefficienten bij zomerstreefpeil
    !*********************************************************************

    DO INODE=1,NCNODE
     IF (EiNode(INODE,3) .EQ. 2) THEN
       IOVH = EiNode(INODE,2)
!oud   OWVAL = - (LVLOH(IOVH) - ONTWDP(IOVH))
       IOW = EIOW(INODE)

       currentDate%year = ConfArr_get_IYear()  ! wel nodig
       currentDate%month = imaand
       currentDate%day = idag
       currentTime%hour = ConfArr_get_iHour()
       currentTime%minute = ConfArr_get_iMinute()
       currentTime%second = 0
       RowNr = -1
       TabelNr = UnpRefIG_TTable (iOvh)
       iniLevel = GetNewValue(TableHandle, TabelNr, 1, RowNr, CurrentDate, CurrentTime, &
                              Idebug, iout1, DateTimeOutsideTable, .true. )
    !  inilevel = initial gw level in m below surface
       if (idebug .ne. 0) then
          write(idebug,*) ' Nieuwe methode Node ', Id_Nod(INODE)
          write(idebug,*) ' iovh ovh-level, iow, initgwl'
          write(idebug,*) iovh, lvloh(iovh), iow, inilevel
       endif

       if (iow .gt. 0) then
        currentDate%year = ConfArr_get_IYear()  ! wel nodig
        currentDate%month = imaand
        currentDate%day = idag
        currentTime%hour = ConfArr_get_iHour()
        currentTime%minute = ConfArr_get_iMinute()
        currentTime%second = 0
        ! nieuwe methode: zoek in nieuwe open water tabel
        RowNr = -1
        TabelNr = OpwRefOW_TTABLE (iow)
        owLevel = GetNewValue(TableHandle, TabelNr, 1, RowNr, CurrentDate, CurrentTime, &
                              Idebug, iout1, DateTimeOutsideTable, .true.)
        if (idebug .ne. 0) write(idebug,*) ' owlevel iniLevel', owLevel, iniLevel
      endif


      if (iniLevel .lt. -999.)then
  !  initieel gwl niet via tabel, maar gelijk aan ow-streefpeil in m NAP.
        if (iow .gt. 0) then
            gwlIni(iovh) = owLevel
        else
  !       geen benedenstrooms open water, en init.gwl=ow-streefpeil: Error message
  !       eerst check of er soms een boundary is;
            Ibnd = EIBND(INODE)
            if (ibnd .le. 0) then
               call ErrMsgStandard(973, 0, 'Groundwater equal to targetlevel open water; but no ordinary RR-link to downstream open water/boundary node defined',&
                           Id_Nod(INode))
            else
               gwlIni(iovh) = BndPar(ibnd,4)
            endif
        endif
      else
  !  initieel gwl via tabel; zet GwlIni om naar m NAP.
        if (idebug .ne. 0) then
           write(Idebug,*) 'Initbc  UseScurve=',UseScurve(iovh)
           write(Idebug,*) 'Initbc  InitGwlOption=',InitGwlOption
           write(Idebug,*) 'Initbc  LvlOh =',LvlOh(iovh)
           write(Idebug,*) 'Initbc  IniLevel =',inilevel
           write(Idebug,*) 'Initbc  FixArs14669 =',FixArs14669
           write(Idebug,*) 'Initbc  AreaSCurve1and0 =',AreaScurve(iovh,1)%Level,AreaScurve(iovh,0)%Level
        endif
        If (UseScurve(iovh) .eq. 0) then
           gwlIni(iOVH) = LvlOh(iOvh) - iniLevel   ! want iniLevel tov maaiveld
        ElseIf (UseScurve(iovh) .eq. 1) then
           if (.not. FixArs14669) then
             gwlIni(iOVH) = AreaScurve(iovh,1)%Level - iniLevel  ! was tov het 1e niveau in de geinterpreteerde Scurve (default 1% nivo bij 100 stappen)
           else
             gwlIni(iOVH) = AreaScurve(iovh,0)%Level - iniLevel   ! Ars14669, June 2005: tov het 0% nivo
           Endif
           If (InitGwlOption .eq. 0) then
              gwlIni(iOVH) = LvlOh(iovh) - iniLevel   ! tov opgegeven maaiveld lv
           Endif
        endif
      endif
      gwl (iOvh) = gwlIni(iOvh)
      gwl0(Iovh) = gwl(iovh)

      if (idebug .ne. 0) then
          write(Idebug,*) 'Initbc  GwlIniLevel =',GwlIni(iovh)
          write(Idebug,*) 'Initbc  Gwlnul      =',Gwl0(iovh)
      endif

      IBOT   = BOTTYP (IOVH)
    !bepaal bergingscoefficient bij streefpeil=ontwateringsdiepte
      if (iow .gt. 0 .AND. InitBcOption .le. 0) then
         OWVAL = - (LVLOH(IOVH) - owLevel)
         ONTWDP(IOVH) = owLevel
      else
         owval = - ( lvloh(iovh) - gwl(iovh) )
         ontwdp(iovh) = gwl(iovh)
      endif

       if (idebug .ne. 0) then
           write(idebug,*) ' iovh ibot owval ontwdp'
           write(idebug,*) iovh, ibot, owval, ontwdp(iovh)
       endif

       If (UseScurve(iovh) .eq. 0) then
!         Geen S curve; gebruik de hierboven bepaalde OwVal
          Call BergCoefTab (Ibot, OwVal, HdeZBergCoef)
       ElseIf (UseScurve(iovh) .eq. 1) then
!         Met Scurve voor maaiveld
          NAverage = 0
          AverageStorageCoefficient = 0.0
          Do Ipoint=UseUnpavedSCurve, 1, -1
            if (iow .gt. 0 .and. InitBcOption .le. 0) then
               OwVal = - ( AreaScurve(iovh,ipoint)%Level - OwLevel )
            else
               OwVal = - ( AreaScurve(iovh,ipoint)%Level - gwl(iovh) )
            endif
            if (OwVal .le. 0) then
               Call BergCoefTab (Ibot, OwVal, HdeZBergCoef)
               AverageStorageCoefficient = AverageStorageCoefficient + HdeZBergCoef
               NAverage = NAverage + 1
            else
              goto 101
            endif
          Enddo
  101     Continue
          HdeZBergCoef = 0.05
          if (NAverage .ge. 1) HdeZBergCoef = AverageStorageCoefficient / NAverage
       Endif
!temp test nav vraag Edward; wat is bergcoef op basis OwVal?
!      OWVAL = - (LVLOH(IOVH) - owLevel)
!      Call BergCoefTab (Ibot, OwVal, HdeZBergCoef)
!temp dat lost inderdaad zijn vraag op

       BergC(iovh) = HdeZBergCoef
       if (idebug .ne. 0)  write(idebug,*) ' Average bergc', bergc(iovh)
! initialisatie bergingscoefficient voor gebruik in Hellinga-de Zeeuw formule
       HdeZBergC(iovh) = BergC(iovh)

     ENDIF
    ENDDO

    RETURN
  END subroutine Unpaved_initBc



  SUBROUTINE BergCoefTab (Ibot, OwVal, HdeZBergCoef)
    !*********************************************************************
    !*** Last update: Sept 1999
    !*********************************************************************
    !*** Brief description:
    !*** ------------------
    !***   Bepaal Bergingscoefficient uit simpele tabel met interpolatie
    !*********************************************************************
    !*** Input/output parameters:
    !*** ------------------------
    !***  IBOT: soil type
    !***  OWVal:dag in de maand
    !***  HdeZBergCoef: berekende bergingscoefficient volgens tabel 12 bodemtypen
    !*********************************************************************

    ! declaration of variables

    INTEGER IBOT, IVAL, idumbot
    Double precision    OWVAL, RHLP, RHLP2, HdeZBergCoef
    Integer iDebug, iout1

    ! end declarations

    iDebug = ConfFil_get_iDebug()
    iOut1  = ConfFil_get_iOut1 ()

    CALL D_LOCATE (OWDEPT, NVAL2, OWVAL, IVAL)
    IF (IVAL .EQ. 0 .OR. IVAL .EQ. NVAL2) THEN
       IVAL = MAX (1, IVAL)
       IF (IBOT .le. NVAL2) THEN
          HdeZBergCoef = BERGTB(IVAL,IBOT)
! ARS xxxx Jan 2003 HdezBergC; correctie initialisatie
       Elseif (UnSatZoneOption .ge. 1) then
          if (NewFormatSoilData) write(Iout1,*) ' To be checked: Capsim and NewFormatSoilData'
          Idumbot = SoilCnv2(Ibot)
          HdeZBergCoef = BERGTB(IVAL,IdumBOT)
       Else
          if (idebug .ne. 0) write(idebug,*) ' Bottom type not in HdeZ table '
          if (idebug .ne. 0) write(idebug,*) ' Bergingscoefficient gezet op 0.05'
          HdeZBergCoef  = 0.05
       Endif
    ELSE
       RHLP = OWVAL - OWDEPT(IVAL)
       RHLP2= OWDEPT(IVAL+1) - OWDEPT(IVAL)
       IF (IBOT .le. NVAL2) THEN
          HdeZBergCoef = BERGTB(IVAL,IBOT)
       Else
          if (idebug .ne. 0) write(idebug,*) ' Bottom type not in HdeZ table '
          if (idebug .ne. 0) write(idebug,*) ' Bergingscoefficient gezet op 0.05'
          HdeZBergCoef  = 0.05
       Endif
       IF (ABS(RHLP2) .GT. .0001) THEN
         IF (IBOT .le. NVAL2) THEN
            HdeZBergCoef  = BERGTB(IVAL,IBOT) + RHLP / RHLP2 * &
                         ( BERGTB(IVAL+1,IBOT)-BERGTB(IVAL,IBOT) )
         Else
            if (idebug .ne. 0) write(idebug,*) ' Bottom type not in HdeZ table '
            if (idebug .ne. 0) write(idebug,*) ' Bergingscoefficient gezet op 0.05'
            HdeZBergCoef  = 0.05
         Endif
       ENDIF
    ENDIF
    if (idebug .ne. 0)  write(idebug,*) ' OwVal bergc', OwVal, HdeZBergCoef

    RETURN
  END subroutine BergCoefTab


  Subroutine ReadOpenDAUnpaved (Infile1, iout1, update)

  ! read Unpaved restart info from ASCII OpenDA file
  integer      Infile1, iout1, idebug
  logical      update


  Integer      RetVal

  Integer       inod
  Integer       iovh, iecode, icrop
  Character(Len=1000) String
  Integer        Nhlp
  Parameter     (Nhlp = 32)
  Integer       IDUM(NHLP)
  Real          RDUM(NHLP)
  Character(Len=CharIdLength) CDUM(NHLP)
  Logical       allow, found, endfil
  double precision V_in, Vintodo, Gwl1, VOnvZoneOld, ratio
  Double precision    PercentageInundation, Area

  ! file is already opened, rewind it
  Rewind(Infile1)
  update = .false.
  iDebug = ConfFil_get_iDebug()
  retVal = 0

  Call SKPCOM (Infile1, ENDFIL,'ODS')
  call SetMessage(LEVEL_INFO, 'Read OpenDA Unpaved data')
  do while (.not. endfil)
    ! read OpenDA record
     Read(Infile1,'(A1000)',END=21,ERR=150,IOSTAT=IECODE)  STRING
     ! skip regel als hij niet begint met juist keyword (UNPV)
     ! UNPV id 'unpaved' gwvolume 1234 onvzonevolume 4567  bergingland 1234 unpv
     If (STRING(1:4) .EQ. 'UNPV') then
      ! UNPV node id
        allow = .false.
        Retval = Retval + GetVAR2 (STRING,' id ',1,' UNPV-ReadAscii',' OPENDA file',IOUT1, &
                                   CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
        call fndNd2(inod, CDUM(1))
        if (inod .gt. 0) then      ! knoop bestaat in schematisatie
            iovh = EiNode(inod,2)
            if (EiNode(inod,3) .eq. 2) then  ! en is onverhard gebied
                ! get the data
                ! update the corresponding RR variables and related variables
                allow = .true.
                Retval = Retval + GetVAR2 (STRING,' gwvolume ',2, ' UNPV-ReadAscii',' OPENDA file',IOUT1, &
                                              CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
                if (found) then
                   write(*,*) ' found unpaved id and gwvolume ', Id_nod(inod)(1:len_trim(id_nod(inod))), rdum(1)
                   V_In = Rdum(1) - BOBD (iovh)
                   BOBD (iovh)   = Rdum(1)
                   BINIBD (iovh) = Rdum(1)
                   Gwl1 = (LvlOhMx(iovh) - Gwl0(iovh))
                   VinToDo = V_In
                   if (UnsatZoneOption .eq. 0) then
                       AREA = MAX (AreaGwComp(IOVH), 0.0001d0)
                       GWL(IOVH) = GWL0(IOVH) + 1/BERGC(IOVH) * V_IN / AREA
                   else
                       Call FindNewGroundwaterLevel (Idebug, iovh, VinToDo, Gwl1)  ! GWL(iovh) is gezet
                   endif


                   RSLMAP2_OVH(11,iovh,1) = Gwl (iovh)
                   OVH_Tnul(11,iovh)= Gwl(iovh)
                   Call  DeterminePercentageInundationUnpaved (iovh, inod, PercentageInundation, iout1)
                   RSLMAP2_OVH(12,iovh,1) = PercentageInundation
                   OVH_Tnul(12,iovh)= PercentageInundation
                   RSLMAP2_OVH(14,iovh,1) = BOBD(iovh)
                   OVH_Tnul(14,iovh)= BOBD(iovh)
                   RSLMAP2_OVH(16,iovh,1) = Gwl(iovh) - LvlOh(iovh)
                   RSLMAP2_OVH(17,iovh,1) = Gwl(iovh) - MaxGwl2(iovh)
                   OVH_Tnul(16,iovh)= Gwl(iovh) - LvlOH(iovh)
                   OVH_Tnul(17,iovh)= Gwl(iovh) - MaxGwl2(iovh)
                   update = .true.
                else
                   V_In = 0D0
                endif
                allow = .true.
                Retval = Retval + GetVAR2 (STRING,' onvzonevolume ',2,' UNPV-ReadAscii',' OPENDA file',IOUT1, &
                                             CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
                if (found .and. UnsatZoneOption .ge. 1) then  ! found and CapsimUsed, so variable is relevant
                   write(*,*) ' found unpaved id and onvzonevolume ', Id_nod(inod)(1:len_trim(id_nod(inod))), rdum(1)
                   VOnvZoneOld = OnvZone(iovh)%Actual_Volume
                   OnvZone(iovh)%Actual_Volume = Rdum(1)
                   OnvZone(IOVH)%Actual_mm     = OnvZone(IOVH)%Actual_Volume / AREAOH(IOVH) / mm2m
                   OnvZone(IOVH)%Init_mm       = OnvZone(IOVH)%Actual_mm
                   RSLMAP2_OVH(21,iovh,1) = OnvZone(iovh)%Actual_mm
                   RSLMAP2_OVH(22,iovh,1) = OnvZone(iovh)%Actual_Volume
                   OVH_Tnul(21,iovh)= OnvZone(iovh)%Actual_mm
                   OVH_Tnul(22,iovh)= OnvZone(iovh)%Actual_Volume
                   update = .true.
                   ! also update info per crop
                   ratio = OnvZone(iovh)%Actual_Volume / VOnvZoneOld
                   Do ICrop=1,NCrop
                      CropOnvZone(IOVH,icrop)%Actual_Volume = ratio * CropOnvZone(IOVH,icrop)%Actual_Volume
                      CropOnvZone(IOVH,icrop)%Actual_mm = ratio * CropOnvZone(IOVH,icrop)%Actual_mm
                      CropOnvZone(IOVH,icrop)%init_mm   = CropOnvZONE(IOVH,icrop)%Actual_mm
                   Enddo
                   ! also update info for Scurve if used, and for Scurve and per Crop if used
                   if (useScurve(iovh) == 1) then
                       ScurveDataToBeSetByRR(iovh) = .true.
                       ScurveDataPerCropToBeSetByRR(iovh) = .true.
                   endif
                endif
                allow = .true.
                Retval = Retval + GetVAR2 (STRING,' bergingland ',2,' UNPV-ReadAscii',' OPENDA file',IOUT1, &
                                             CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
                if (found) then
                   write(*,*) ' found unpaved id and bergingland ', Id_nod(inod)(1:len_trim(id_nod(inod))), rdum(1)
                   BOLND(iovh)   = Rdum(1)
                   BINIOL(iovh) = Rdum(1)
                   RSLMAP2_OVH(13,iovh,1) = BOLND(iovh) / AreaOH(iovh) * 1000.
                   RSLMAP2_OVH(15,iovh,1) = BOLND(iovh)
                   OVH_Tnul(13,iovh)= BOLND(iovh) / AreaOH(iovh) * 1000.
                   OVH_Tnul(15,iovh)= BOLND(iovh)
                   update = .true.
                endif
            endif
        endif
     Endif
  enddo
  goto 21

150 continue
    call SetMessage(LEVEL_FATAL, 'Error reading OpenDA restart record '//String(1:len_trim(string)))
21  continue

  End subroutine ReadOpenDAUnpaved


  Subroutine WriteOpenDAUnpaved (Infile1)

  ! write Unpaved restart info to ASCII OpenDA file
  integer      Infile1, idebug


  Integer       inode, iovh
  Character(len=1) Quote

  ! file is already opened
  iDebug = ConfFil_get_iDebug()
  quote = ''''

  do inode=1,ncnode
     iovh = EiNode(inode,2)
     if (EiNode(inode,3) .eq. 2) then  ! en is onverhard gebied
         write(Infile1,'(A,A1,A,A1,3(1X,A,G15.8),A)') 'UNPV id ',&
                                       quote,id_nod(inode)(1:len_trim(id_nod(inode))),quote,&
                                       ' gwvolume ', BOBD(iovh), &
                                       ' onvzonevolume ', OnvZone(iovh)%Actual_Volume, &
                                       ' bergingland ', BOLND(iovh), ' unpv'
     Endif
  enddo

  End subroutine WriteOpenDAUnpaved


  SUBROUTINE CMPOVH (IEVENT, ITMSTP, IOVH, IMETEO, INODE, makelogfile, MessageInundation, MinDepthCF, IterNodelp)
    ! *********************************************************************
    ! *** Last update:  March 1995
    ! *********************************************************************
    ! *** Brief description:
    ! *** ------------------
    ! ***    This subroutine performs computations for onverhard gebied.
    ! *********************************************************************
    ! *** Input/output parameters:
    ! *** ------------------------
    ! ***  IEVENT = event number
    ! ***  ITMSTP = timestep number
    ! ***  IDEBUG = file unit number of debug file
    ! ***  IOUT1  = file unit number of output file with run messages
    ! ***  IOVH   = intern onverhard gebied nr
    ! ***  IMETEO = meteostation code
    ! ***  INODE  = knoopnr
    ! ***  makelogfile = indicator of logfile gewenst is (0=nee)
    ! *********************************************************************
    ! *** Berekeningen voor onverhard gebied
    ! *********************************************************************

     Implicit none


! Local arrays for interpolation
      Real PeilArray(6), AreaArray(6)  ! aanname dat NVAL=6 zoals in Conf_Arr.

    ! variables
    Integer makelogfile,iEvent, iTmStp, iOvh, iMeteo, iNode, IterGwlMaaiveld, IterUnsat, iCrop, IterNodelp
    Integer iOW, ibnd, iPluv, IowSWLink, IBndSWLink, i
    Integer MessageInundation
    Double precision    FactEvap, CapillaryRise, Percolation  !AvgVulling,
    Double precision    V_IN, RestVol, RKWEL, IN0, AREA, rMaxIn, Peil, qIn, TotalScurveSurfaceOutflow, AvGwLvl, GwlTemp
    Integer iDebug, iOut1, idum, IterSeepage
    LOGICAL Inundation, UnsatInfReduction
    Real                TmpAreaOw
    Double precision    MinDepthCF, Delta_BndPeil, &
            TotalScurveSurfaceOutflowMax, PrevTotalScurveSurfaceOutflowMax, PercentageInundation, &
            TotalInfReduction, PrevTotalInfReduction

    logical IncreasingGwl
    double precision VinTodo, VinTodoPrev
    Double precision    Gwl1, Gwl2, NewStorageCoefficient, DpIn, DpInPrev, Fact1, Fact2
    Integer IFlag, teller, itemp
    Character(Len=CharIdLength) NodeName
    Double precision    balterm1, balterm2, balterm3, balterm4, balterm5, balterm6
    Double precision    OldQ1O

!   irrigation
    Double precision    CriticalSoilMoistureValue, CriticalGWLevel
    Double precision    TargetSoilMoistureValue, TargetGWLevel

    iDebug = ConfFil_get_iDebug()
!   if (iovh .eq. 177) iDebug = idebugLunRR
    iOut1 = ConfFil_get_iOut1()
    if (idebug .ne. 0)  WRITE(IDEBUG,*) 'CMPOVH iovh=',IOVH

    ! related open water node and related boundary node
    IOW  = EIOW(INODE)
    IBND = EIBND(INODE)
    IPLUV= EIPLUV(INODE)
    IOWSWLink  = EiOwSWLink(INODE)
    IBNDSWLINK = EiBndSWLink(INODE)

    ! iteration loop in case of inundation
    RMAXIN= 9.999E+009
    PrevTotalScurveSurfaceOutflowMax = 9.999E+009
    TotalScurveSurfaceOutflowMax = 9.999E+009
    IterGwlMaaiveld = 0
    IterSeepage     = 0
    TotalInfReduction = 0.0
    IterUnsat       = 0
    MaxItrUnsat     = MaxItrGwlMaaiveld
    RKwel           = 0.

    ! *********************************************************************
    ! *** Irrigation based on initial soil moisture and irrigation possibilities
    ! *********************************************************************

    IrrigationSupply(iovh) = 0
    if (IrrigationSource(iovh) .gt. 0) then
       If (UnsatZoneOption .ge. 1) then
          ! Capsim (1) or own Capsim+ (2)
          Call FindSoilMoistureAtCriticalpFValue (IrrigationpFCrit(iovh), CriticalSoilMoistureValue, Bottyp(iovh), CapsimDpRootz(iovh),IrrigationCriticalSMValue(iovh))
          Call FindSoilMoistureAtCriticalpFValue (IrrigationpFTarget(iovh), TargetSoilMoistureValue, Bottyp(iovh), CapsimDpRootz(iovh),IrrigationTargetSMValue(iovh))
          if (OnvZone(IOVH)%Init_mm .le. CriticalSoilMoistureValue .or. IrrigationOngoing(iovh)) then
             if (IrrigationSeason .and. IrrigationDailyPeriod) then
               if (FirstIrrigationInYear(iovh)) then
                   IrrigationSupply(iovh) = IrrigationInitSupply(iovh)
               else
                   IrrigationSupply(iovh) = IrrigationMaxSupply(iovh)
               endif
             endif
             if (IterNodelp .eq. 1) TimeSinceStartFirstIrrigation(iovh) = TimeSinceStartFirstIrrigation(iovh) + TimeSettings%TimestepSize
          endif
          if ( IrrigationOngoing(iovh) .and. (OnvZone(iovh)%Init_mm .gt. TargetSoilMoistureValue)) then
             if (TimeSinceStartFirstIrrigation(iovh) .gt. 86400 * IrrigationInitDuration(iovh)) then
                IrrigationOngoing(iovh) = .false.
                FirstIrrigationInYear(iovh) = .false.
                IrrigationSupply(iovh) = 0.0
             endif
          endif
       elseIf (UnsatZoneOption .eq. 0) then
          ! No Capsim
          Call FindGWLevelAtCriticalpFValue (IrrigationpFCrit(iovh), CriticalGWLevel, IrrigationCriticalGWValue(iovh))
          Call FindGWLevelAtCriticalpFValue (IrrigationpFTarget(iovh), TargetGWLevel, IrrigationTargetGWValue(iovh))
          if (GWL0(iovh) .le. CriticalGWLevel .or. IrrigationOngoing(iovh)) then
             if (IrrigationSeason .and. IrrigationDailyPeriod) then
                if (FirstIrrigationInYear(iovh)) then
                    IrrigationSupply(iovh) = IrrigationInitSupply(iovh)
                else
                    IrrigationSupply(iovh) = IrrigationMaxSupply(iovh)
                endif
             endif
             if (IterNodelp .eq. 1) TimeSinceStartFirstIrrigation(iovh) = TimeSinceStartFirstIrrigation(iovh) + TimeSettings%TimestepSize
          endif
          if ( IrrigationOngoing(iovh) .and. (GWL0(iovh) .gt. TargetGWLevel) ) then
             if (TimeSinceStartFirstIrrigation(iovh) .gt. 86400 * IrrigationInitDuration(iovh)) then
                IrrigationOngoing(iovh) = .false.
                FirstIrrigationInYear(iovh) = .false.
                IrrigationSupply(iovh) = 0.0
             endif
          endif
       endif
    endif
    ! convert to mm/day to m3/s and computed demand to gw, sw or external
    IrrigationSupply(iovh) = IrrigationSupply(iovh) * TmIrri * AreaOh(iovh) / 86400. / 1000.
    IrrigationDemand(iovh) = IrrigationSupply(iovh) * IrrigationEfficiencyFactor(iovh)
    IrrigationOngoing(iovh) = (IrrigationSupply(iovh) .gt. 0)
    if (Idebug .gt. 0 .and. IrrigationSource(iovh) .gt. 0 .and. UnsatZoneOption .ge. 1 ) then
       WRITE(IDEBUG,*) ' Computed irrigation'
       WRITE(IDEBUG,*) ' CriticalpFValue - CriticalSoilMoistureValue:',IrrigationpFCrit(iovh), CriticalSoilMoistureValue
       WRITE(IDEBUG,*) ' TargetpFValue - TargetSoilMoistureValue:',IrrigationpFTarget(iovh), TargetSoilMoistureValue
       WRITE(IDEBUG,*) ' Init_mm ',OnvZone(iovh)%Init_mm
       WRITE(IDEBUG,*) ' Initsupply, MaxSupply',IrrigationInitSupply(iovh), IrrigationMaxSupply(iovh)
       WRITE(IDEBUG,*) ' Irrigation supply m3/s, m3 :',IrrigationSupply(iovh), IrrigationSupply(iovh) * timeSettings%timestepSize
       WRITE(IDEBUG,*) ' Irrigation demand m3/s, m3 :',IrrigationDemand(iovh), IrrigationDemand(iovh) * timeSettings%timestepSize
       WRITE(IDEBUG,*) ' Irrigation demand source 0=no,1=sw,2=gw,3=ext) :',IrrigationSource(iovh)
       WRITE(IDEBUG,*) ' Irrigation ongoing :',IrrigationOngoing(iovh)
    endif

    IN0 = 0.

  1 CONTINUE

    PrevTotalScurveSurfaceOutflowMax = TotalScurveSurfaceOutflowMax
    CapillaryRise = 0.0
    Percolation = 0.0
    PrevTotalInfReduction = TotalInfReduction
    TotalInfReduction = 0.0

    ! *********************************************************************
    ! *** Maaiveld: Verdamping op land (onverhard gebied) als berging>0
    ! *** Maaiveld in: precipitation + irrigation supply;
    ! *** Bepaal infiltratie, voorlopige berging op land
    ! *********************************************************************

    Call ComputeSurface (IOVH, INode, IMETEO, RMaxIn, IterGwlMaaiveld, IterUnsat)
    if (idebug .ne. 0)  WRITE(IDEBUG,*) ' INO(iovh) ',INO(iovh)

    ! *********************************************************************
    ! *** Bodem - welk gerelateerd peil?
    ! *********************************************************************

    Call SetUnpavedRelatedAveragePeil (Iovh, Iow, ibnd, ipluv, peil)

    ! *********************************************************************
    ! *** Potentiele Gewasverdamping
    ! *********************************************************************

    !Jan96:cropfactors per gewas
    VBO(IOVH) = 0.0
    Do ICrop=1,NCrop
       VBOCROP(IOVH,iCROP) = 0.0
       QinBCROP(IOVH,iCROP) = 0.0
    Enddo

    idum = 6
    FactEvap = 1.0

    DO ICROP=1,NCROP
      IF (AreaGw(Iovh,icrop) .gt. 0.0 .and. &
           ConfArr_get_iHour().GE. timeSettings%evaporationFromHr .AND. &
            ConfArr_get_IHOUR() .LT. timeSettings%evaporationToHr) THEN
          VBOCrop(IOVH,Icrop) = EVAP(IMETEO) * CROPF(ICROP) * FactEvap * &
                                AREAGW(IOVH,ICROP) * timeSettings%timestepSize * TMEVAP
          VBO(IOVH) = VBO(IOVH) + VBOCROP(IOVH,ICROP)
          if (idebug .ne. 0) WRITE(IDEBUG,*) ' after ICROP VBO:', VBO(IOVH)
      ENDIF
    ENDDO
    RzEPot (iovh) = VBO(iovh)

   ! *********************************************************************
   ! Balans onverzadigde zone
   ! *********************************************************************

    If (UnSatZoneOption .eq. 1) then
!      Onverzadigde zone met Capsim, eventueel ook met S curve maaiveld
       NodeName = Id_Nod(INODE)
       Call ComputeCapsim (iovh, idebug, NodeName)
    ElseIf (UnSatZoneOption .eq. 2) then
!      Onverzadigde zone met Capsim+, eventueel ook met S curve maaiveld
       NodeName = Id_Nod(INODE)
       Call ComputeCapsimPlus (iovh, idebug, TotalInfReduction, NodeName)
    Else
!      Geen Capsim, onverzadigde zone zonder berging
       Call ComputeUnsatZoneSimpleNoCapsim (iovh, Percolation, CapillaryRise)
    Endif


! *********************************************************************
! Check op inundaties ivm hoog open water peil
! Bij Scurve: Geef alleen melding als gwl boven hoogste maaiveld
! *********************************************************************

 11 Continue

    If (SeepageCompOption(iovh) .eq. 1 .or. SeepageCompOption(iovh) .ge. 4) then
!      constant seepage or seepage from a time table
       RKWEL = KWEL(IOVH)-WEGZG(IOVH)
    else
! Jan 2000: addition variable seepage, SeepageCompOption 2,3: use H0 from table or from Modflow
! H0, Gwl in m, Cvalue in dagen, RKwel in m/s
! instead of new GWL, use average of GWL0 and new GWL?
       AvGwLvl = (Gwl0(iovh) + Gwl(iovh) ) / 2.0
       RKwel = ( H0Actual(iovh) - AvGwLvl ) / CValue(iovh) / NrsDay
       if (idebug .ne. 0) THEN
         WRITE(IDEBUG,*) ' Compute RKwel using'
         WRITE(IDEBUG,*) ' H0Actual  Gwl  Cvalue',H0Actual(iovh), Gwl(iovh), Cvalue(iovh)
         WRITE(IDEBUG,*) ' Rkwel  ', Rkwel
       endif
! beperk RKwel (volumecheck):
! als H0 < gwl0 mag de wegzijging niet zo groot zijn dat gwl onder H0 zakt
! als H0 > gwl0 mag de kwel       niet zo groot zijn dat gwl boven H0 stijgt
       Gwltemp  = Gwl0(Iovh) + 1/BergC(Iovh) * RKwel * timeSettings%timestepSize
       if (Rkwel .gt. 0 .and. gwltemp .gt. H0Actual(iovh) .and. gwl0(iovh) .lt. H0Actual(iovh))  then
          Rkwel = min (Rkwel, (H0Actual(iovh) - gwl0(iovh)) * BergC(Iovh) / timeSettings%timestepSize)
       elseif (rkwel .lt. 0 .and. gwltemp .lt. H0Actual(iovh) .and. gwl0(iovh) .gt. H0Actual(iovh))  then
          Rkwel = max (Rkwel, (H0Actual(iovh) - gwl0(iovh)) * BergC(Iovh) / timeSettings%timestepSize)
       endif
       if (idebug .ne. 0)  WRITE(IDEBUG,*) ' Computed RKwel after VolumeCheck ', RKwel
    endif

    Inundation = .false.
    IF (IOW .GT. 0) THEN
      IF (LVLOW0(IOW) .GT. LVLOHMx(IOVH)) THEN
        !inundatie
        inundation = .true.
        call ConfArr_set_fldOvh(.TRUE.)
        if (idebug .ne. 0) WRITE(IDEBUG,'(A)') ' Inundation unpaved area '
        IF (IterGwlMaaiveld .GE. 2) then
          if (IOut1 .ne. 0 .and. MessageInundation .gt. 0) then
            WRITE(IOUT1,'(A,A,A,2I5)') ' Inundation unpaved area ', &
                  Id_Nod(INODE),' in event/tijdstap', IEVENT, ITMSTP
          endif
          Q2O(IOVH) = 0
        endif
       ENDIF
    ELSEIF (IBND .GT. 0) THEN
      IF (PEIL .GT. LVLOHMx(IOVH)) THEN
        !inundatie
        inundation = .true.
        call ConfArr_set_fldOvh(.TRUE.)
        if (idebug .ne. 0)  WRITE(IDEBUG,'(A)') ' Inundation unpaved area '
        IF (IterGwlMaaiveld .GE. 2) then
          if (IOut1 .ne. 0 .and. MessageInundation .gt. 0) then
            WRITE(IOUT1,'(A,A,A,2I5)') ' Inundation unpaved area ', &
                  Id_Nod(INODE),' in event/tijdstap', IEVENT, ITMSTP
          endif
          Q2O(IOVH) = 0
        endif
      ENDIF
    ELSEIF (IPluv .GT. 0) THEN
      IF (PEIL .GT. LVLOHMx(IOVH)) THEN
        !inundatie
        inundation = .true.
        call ConfArr_set_fldOvh(.TRUE.)
        if (idebug .ne. 0)  WRITE(IDEBUG,'(A)') ' Inundation unpaved area '
        IF (IterGwlMaaiveld .GE. 2) then
          if (IOut1 .ne. 0 .and. MessageInundation .gt. 0) then
            WRITE(IOUT1,'(A,A,A,2I5)') ' Inundation unpaved area ', &
                  Id_Nod(INODE),' in event/tijdstap', IEVENT, ITMSTP
          endif
          Q2O(IOVH) = 0
        endif
      ENDIF
    ENDIF

! ARS 15464 add option for separate surface runoff link to open water or boundary: no impact on Q2
! dus geen acties hier om peilen bij knopen IowSWLink en IBndSWLink op te vragen en check op inundatie te doen
!    If (IowSwlink .gt. 0 .or. IbndSWLink .gt. 0) then
!       Call SetUnpavedRelatedAveragePeil (Iovh, IowSwlink, IBndSwlink, ipluv, peil)
!    Endif
!    Call SetUnpavedRelatedAveragePeil (Iovh, Iow, IBnd, ipluv, peil)


!ARS1334: altijd afstroming van bodem naar open water uitrekenen, ook als owpeil > gwl
! *********************************************************************
!        afstroming van bodem naar open water; evt. negatief!
! *********************************************************************

!Juli 1998: nu niet de infiltratie vanaf het oppervlak nemen,
!              maar de netto percolatie vanuit de onverzadigde zone nemen
! QINB is a volume in m3,  QIN is a flow in m3/s

     QIN = QINB(IOVH)/timeSettings%timestepSize
!     QIN = QIN + RKWEL*AREAOH(IOVH)
     QIN = QIN + RKWEL*AreaGwComp(IOVH)
     If (IrrigationSource(iovh) .eq. 2) Qin = Qin - IrrigationDemand(iovh) ! gw irrigation

! addition of choice of computation option: Hellinga_de_Zeeuw or Krayenhoff_vd_Leur, February 1999

     TotalScurveSurfaceOutflow = 0.

     If (CompOption(IOVH) .eq. 1) Then

!      Hellinga de Zeeuw formulation voor uitstroming bodem naar open water (drainage of infiltratie)
!      Als met Scurve gewerkt wordt, dan kan een deel al afstroming van oppervlak zijn! (nl. bij hoog gwl boven laagste maaiveld)
       Call HellingaDeZ (Inode, Iovh, Ibnd, Iow, IPluv, Idebug, Peil, Qin, TotalScurveSurfaceOutflow, MinDepthCF)

     ElseIf (CompOption(IOVH) .eq. 2) Then

!      Krayenhoff vd Leur formulation
       Call KrayenhoffvdLeur (Iovh, Idebug, Peil, Qin, Itmstp)

     ElseIf (CompOption(IOVH) .eq. 3) Then

!      Ernst formulation
       Call Ernst (Inode, Iovh, Ibnd, Iow, Ipluv, Idebug, Peil, Qin, TotalScurveSurfaceOutflow, MinDepthCF)

     Endif


     if (idebug .ne. 0)  then
        write(idebug,*) ' TotalScurveSurfaceOutflow, TotalScurveSurfaceOutflowMax'
        write(idebug,*) TotalScurveSurfaceOutflow, TotalScurveSurfaceOutflowMax
     endif
     TotalScurveSurfaceOutflow = min (TotalScurveSurfaceOutflow, TotalScurveSurfaceOutflowMax)

! Bepaal totaal volume V_In = toename volume verzadigd grondwater
! houdt ook rekening met Scurve-surfaceoutflow

!    V_IN  = RKWEL * AREAOH(IOVH) * timeSettings%timestepSize &
    V_IN  = RKWEL * AreaGwComp(IOVH) * timeSettings%timestepSize &
            - Q2O(IOVH) * timeSettings%timestepSize &
            - TotalSCurveSurfaceOutflow  * timeSettings%timestepSize &
            + QINB(IOVH)
!  groundwater irrigation?
    If (IrrigationSource(iovh) .eq. 2) V_IN = V_IN - IrrigationDemand(iovh) * TimeSettings%TimestepSize
!
    if (idebug .ne. 0) then
      Write(IDEBUG,*) ' TotalScurveSurfaceOutflow in m3/s', TotalScurveSurfaceOutflow
      Write(IDEBUG,*) ' V_IN in m3', V_IN
      Write(IDEBUG,*) ' IrrigationDemand in m3/s',IrrigationDemand
      Write(IDEBUG,*) ' GWL0 and GWL ',GWL0(iovh), GWL(iovh)
    Endif

  ! *********************************************************************
  ! Nieuwe grondwaterstand
  ! *********************************************************************

  If (CompOption(IOVH) .eq. 2) Then

! for Krayenhoff van de Leur: bepaal opbolling etc. GEEN check op gw op maaiveld
! opbolling gerelateerd aan huidig open water peil (en dus niet aan intieel gw-peil)
! via initieel peil was het:
!   GWL(iovh) = gwlIni(iovh) + KvdLeur(iovh,KvdLDimensie)%KOpbolling - KvdLeur(iovh,KvdLDimensie-1)%KOpbolling

    GWL(iovh) = peil + KvdLeur(iovh,KvdLDimensie)%KOpbolling - KvdLeur(iovh,KvdLDimensie-1)%KOpbolling
    BOBD(IOVH) = BOBD0(IOVH) + V_IN
     if (idebug .ne. 0) then
       write(idebug,*) ' KvdLgw-level en opbolling t t-1 ',GWL(iovh),KvdLeur(iovh,KvdLDimensie)%KOpbolling,&
                                                                     KvdLeur(iovh,KvdLDimensie-1)%KOpbolling
       write(idebug,*) ' KvdLdebiet Q2O',Q2O(iovh)
    endif

  ElseIf (CompOption(IOVH) .eq. 1 .or. CompOption(iovh) .eq. 3) Then

  ! *********************************************************************
  ! for Hellinga de Zeeuw,  or Ernst:  Groundwater >= surface level checken
  ! *********************************************************************

    AREA = MAX (AreaGwComp(IOVH), 0.0001d0)
    GWL(IOVH) = GWL0(IOVH) + 1/BERGC(IOVH) * V_IN / AREA
    BOBD(IOVH) = BOBD0(IOVH) + V_IN
    if (idebug .ne. 0) Write(IDEBUG,*) ' recalc GWL: GWL0 and GWL ',GWL0(iovh), GWL(iovh)

! ARS 7274: bij gebruik Capsim met gwl in wortelzone en grote tijdstap grote gwl veranderingen agv zeer kleine storage coefficient
!   If (UnSatZoneOption .eq. 1 .and. (Abs(GWL(iovh)-GWL0(iovh)) .gt. 0.1) )  then     ! Sobek-Capsim
! ARS 7607: altijd storage coefficient bepalen, ook al gwl wijzigingen gering
    If (UnSatZoneOption .ge. 1)  then     ! Sobek-Capsim of Capsim+
!       Update storage coefficient BERGC(iovh) voor bepalen veranderingen in grondwaterstand;
!       BergC is de bergingscoefficient volgens Capsim bij gemiddelde grondwaterstand
        call CapsimStorageCoefficient (iovh)
        BergC (iovh) = 0.5 * BergC(Iovh) + 0.5 * CapsimBergC(iovh)
        if (idebug .ne. 0) then
          write(idebug,*) ' onverhard gebied BergC HdeZBergC', iovh, BergC(iovh),HdeZBergC(iovh)
        endif
    Endif

!   New groundwater level via gw-dh-DV relation
     If (UnSatZoneOption .ge. 1 .and. DetailedGwlComputation .eq. 2)  then
        Gwl1 = (LvlOhMx(iovh) - Gwl0(iovh))
        VinToDo = V_In
        Call FindNewGroundwaterLevel (Idebug, iovh, VinToDo, Gwl1)
     Else
!   Netter: bij gegeven GWL kijken per laagje van x cm hoeveel water er in past.
!   pm resultaat valt toch tegen, gevoeliger voor tijdstap!
!   juli 2001: als extra optie ingebouwd
      VInToDo = 0.0
      If (UnSatZoneOption .ge. 1 .and. DetailedGwlComputation .eq. 1)  then     ! Sobek-Capsim or Capsim+
        Gwl1 = Min (Gwl(iovh), GWL0(iovh))
        Gwl2 = Max (Gwl(iovh), GWL0(iovh))
        IncreasingGwl = (Gwl(iovh) .gt. GWL0(iovh))
        iFlag = 1
        ITemp = int ( (gwl2-gwl1) / StepGwlStorageCoefficient ) + 1
        ITemp = min (ITemp*2, Int((abs(lvlohmx(iovh)-gwl1)/StepGwlStorageCoefficient)) +1 )
!       DpIn = LvlOh(iovh) - Gwl1
        DpIn = LvlOhMx(iovh) - Gwl1
!       If (.not. IncreasingGwl)  DpIn = LvlOh(Iovh) - Gwl2
        If (.not. IncreasingGwl)  DpIn = LvlOhMx(Iovh) - Gwl2
        VInTodo = Abs (V_IN)
        if (idebug .ne. 0) then
           Write(Idebug,*) ' VInTodo   = ', VInTodo
           Write(Idebug,*) ' Gwl1      = ', Gwl1
           Write(Idebug,*) ' Gwl2      = ', Gwl2
           Write(Idebug,*) ' Increasing= ' , IncreasingGwl
           Write(Idebug,*) ' DpIn      = ' , DpIn
           Write(Idebug,*) ' ITemp     = ' , ITemp
           Write(Idebug,*) ' StepGwlStorageCoefficient = ', StepGwlStorageCoefficient
        Endif
        Do teller=1,ITemp
           DpInPrev = DpIn
           If (IncreasingGwl) then
! increasing gwl: because rising is fast, use storage coefficient at middle of layer
              DpIn = DpIn - StepGwlStorageCoefficient
! oud:         Call Capsim2StorageCoefficient (Bottyp(iovh), DpIn+HalfStepGwl, &
!                                              CapsimDpRootz(iovh)*100., IFlag, NewStorageCoefficient)
! nieuw: houdt ook rekening met CapsimPerCrop switch
              Call Capsim3StorageCoefficient (iovh, DpIn+HalfStepGwl, IFlag, NewStorageCoefficient)
           Else
! decreasing gwl: because fall is slow, use storage coefficient at top of layer
! oud:        Call Capsim2StorageCoefficient (Bottyp(iovh), DpIn,  &
!                                             CapsimDpRootz(iovh)*100., IFlag, NewStorageCoefficient)
! nieuw: houdt ook rekening met CapsimPerCrop switch
              Call Capsim3StorageCoefficient (iovh, DpIn, IFlag, NewStorageCoefficient)
              DpIn = DpIn + StepGwlStorageCoefficient
           Endif
           VInTodoPrev = VInTodo
           VInTodo = VInTodo - NewStorageCoefficient * AREA * StepGwlStorageCoefficient
           if (idebug .ne. 0) then
             Write(Idebug,*) ' teller     = ', teller
             Write(Idebug,*) ' DpIn       = ', DpIn
             Write(Idebug,*) ' DpInPrev   = ', DpInPrev
             Write(Idebug,*) ' NewStorCoef= ', NewStorageCoefficient
             Write(Idebug,*) ' VInTodo    = ', VInTodo
             Write(Idebug,*) ' VInTodoPrev= ', VInTodoPrev
           Endif
           If (VInTodo .lt. 0) then
              Fact2 = Abs (VInTodo) + Abs (VInTodoPrev)
              Fact1 = Abs (VInTodoPrev) / Fact2
              Fact2 = 1.0 - Fact1
              if (idebug .ne. 0) Write(Idebug,*) ' Fact1-2', Fact1, Fact2
!             Gwl(iovh) = LvlOh(iovh) - ( DpInPrev * Fact2 + DpIn * Fact1)
              Gwl(iovh) = LvlOhMx(iovh) - ( DpInPrev * Fact2 + DpIn * Fact1)
              Goto 21
           Endif
!           if (teller .eq. itemp) Gwl(iovh) = LvlOh(iovh) - DpIn
           if (teller .eq. itemp) Gwl(iovh) = LvlOhMx(iovh) - DpIn
        Enddo
      Endif
     Endif
  21 Continue
     if (idebug .ne. 0) then
        Write(Idebug,*) ' Computed Gwl = ', Gwl(iovh)
     Endif
! End ARS 7274
!   if (iovh .eq. 177) iDebug = idebugLunRR

    ! *** Reduction of infiltration due to unsaturated zone?
      UnsatInfReduction = .false.
      If (ReduceSurfaceInfiltration .gt. 0 .and. UnsatZoneOption .eq. 1 .and. (.not. UnpavedPercolationLikeSobek213)) then
         NrInfMessages = NrInfMessages + 1
         if (NrInfMessages .lt. 10) call SetMessage(LEVEL_WARN, 'Infiltration reduction due to limited UnsatZone capacity')
         IN0 = INO(IOVH)
         INO(iovh) = MAX (0.0d0, INO(iovh) - ReduceSurfaceInfiltration)   ! in m3
         BOLND(IOVH) = BOLND(IOVH) - INO(IOVH) + IN0
         RMAXIN = INO(IOVH)
         if (idebug .ne. 0)  WRITE(IDEBUG,*) ' Nieuwe RMAXIN=',RMAXIN
         IF ( abs(in0-INO(IOVH)) .gt. .1) UnsatInfReduction = .true.
      elseIf (TotalInfReduction .gt. 0 .and. UnsatZoneOption .eq. 2) then
         NrInfMessages = NrInfMessages + 1
         if (NrInfMessages .lt. 10) call SetMessage(LEVEL_WARN, 'Infiltration reduction due to limited UnsatZone capacity')
         IN0 = INO(IOVH)
         INO(iovh) = MAX (0.0d0, INO(iovh) - TotalInfReduction)   ! in m3
         BOLND(IOVH) = BOLND(IOVH) - INO(IOVH) + IN0
         RMAXIN = INO(IOVH)
         if (idebug .ne. 0)  WRITE(IDEBUG,*) ' Nieuwe RMAXIN=',RMAXIN
         IF ( abs(in0-INO(IOVH)) .gt. .1) UnsatInfReduction = .true.
! added else branch for initialisation IN0
      else
         IN0 = INO(iovh)
      Endif

    ! *********************************************************************
    ! *** Check grondwaterstand boven maaiveld?
    ! *********************************************************************

!    IF (GWL(IOVH) .GT. LVLOHMx(IOVH)) THEN
!    April 2002: test corrected, to include residual VinToDo
     IF (GWL(IOVH) .GT. LVLOHMx(IOVH) .or. VinToDo .gt. 0) THEN
    !  nieuwe grondwaterstand boven maaiveld; corrigeer dit
    !  tot juni 2001: aanname: dit komt door overvloedige infiltratie, en niet door overvloedige kwel!
    !  juni 2001: check of dit door overvloedige neerslag/infiltratie, of door overvloedige kwel!
      IF (INO(iovh) .gt. 0 .and. ItergwlMaaiveld .le. MaxItrGwlMaaiveld) then
        !  gw op maaiveld door overvloedige infiltratie
        if (idebug .ne. 0) WRITE(IDEBUG,*) ' Gw at surface level; reduce infiltration:'
        IF ((IterGwlMaaiveld .GE. 2) .and. (makelogfile .gt.  0)) then
          WRITE(IOUT1,'(A,A,A,2I5)') ' Groundwater above surface level: ', &
                Id_Nod(INODE),' in event/timestep', IEVENT, ITMSTP
        Endif
        Call ConfArr_set_FLDOVH(.TRUE.)
        if (idebug .ne. 0) WRITE(IDEBUG,*) ' Inflow soil in m3  :',INO(IOVH), QINB(IOVH), VinToDo
        IN0 = INO(IOVH)
        If (UnSatZoneOption .ge. 1 )  then     ! Sobek-Capsim  bergcoef=0.01 vanaf wortelzone hier hard in code
           if (idebug .ne. 0) WRITE(IDEBUG,*) ' UnsatZoneOption>=1'
           INO(IOVH) = MAX (0.0d0, INO(IOVH) - AreaGwComp(IOVH) * &
                              (GWL(IOVH)-LVLOHMx(IOVH)) * 0.01 - max(0.0d0, VinToDo) )
        Else
           INO(IOVH) = MAX (0.0d0, INO(IOVH) - AreaGwComp(IOVH) * &
                             (GWL(IOVH)-LVLOHMx(IOVH)) * BERGC(IOVH) )
        Endif
! Bijstellen balans onverzadigde zone
        QINB(IOVH) = QINB(IOVH) + INO(iovh) - IN0
        CapillaryRise = 0.0
        Percolation   = 0.0
        If (QinB(iovh) .lt. 0) then
           CapillaryRise = -QINB(iovh)
        Else
           Percolation   = QINB(iovh)
        endif
        if (idebug .ne. 0) WRITE(IDEBUG,*) ' preliminary CapRis Perc QinB:',CapillaryRise, Percolation, QinB(iovh)
        OnvZone(iovh)%Actual_Volume = OnvZone(iovh)%Init_Volume + INO(iovh) - VBO(iovh) - QINB(iovh)
        OnvZone(IOVH)%Actual_mm     = OnvZone(IOVH)%Actual_Volume / AREAOH(IOVH) / mm2m
        if (idebug .ne. 0) WRITE(IDEBUG,*) ' preliminary Actual Vol:',OnvZone(iovh)%Actual_Volume
        if (idebug .ne. 0) WRITE(IDEBUG,*) ' preliminary Actual mm :',OnvZone(iovh)%Actual_mm
        if (idebug .ne. 0) WRITE(IDEBUG,*) ' max Vol:',OnvZone(iovh)%max_Volume
        if (idebug .ne. 0) WRITE(IDEBUG,*) ' max mm :',OnvZone(iovh)%max_mm
        if (idebug .ne. 0) WRITE(IDEBUG,*) ' min Vol:',OnvZone(iovh)%min_Volume
        if (idebug .ne. 0) WRITE(IDEBUG,*) ' min mm :',OnvZone(iovh)%min_mm
        If ( OnvZone(IOVH)%Actual_mm .gt. OnvZone(IOVH)%Max_mm) then
          if (idebug .ne. 0) WRITE(IDEBUG,*) ' preliminary gt max'
          CapillaryRise = 0.0
          Percolation =  Qinb(iovh) + OnvZone(IOVH)%Actual_Volume - OnvZone(IOVH)%Max_Volume
          OnvZone(IOVH)%Actual_Volume =  OnvZone(IOVH)%Max_Volume
          OnvZone(IOVH)%Actual_mm =  OnvZone(IOVH)%Max_mm
        Elseif ( OnvZone(IOVH)%Actual_mm .lt. OnvZone(IOVH)%Min_mm ) then
          if (idebug .ne. 0) WRITE(IDEBUG,*) ' preliminary lt min'
          Percolation = 0.0
          CapillaryRise = -Qinb(iovh) + OnvZone(IOVH)%Min_Volume - OnvZone(IOVH)%Actual_Volume
          OnvZone(IOVH)%Actual_Volume = OnvZone(IOVH)%Min_Volume
          OnvZone(IOVH)%Actual_mm = OnvZone(IOVH)%Min_mm
        Endif
        QINB(IOVH) = (Percolation - CapillaryRise)
!end bijstellen balans onv.zone

        if (idebug .ne. 0) WRITE(IDEBUG,*) ' After correction for inundation:',INO(IOVH), QINB(IOVH)
! berg ing op land neemt dus juist toe
        BOLND(IOVH) = BOLND(IOVH) - INO(IOVH) + IN0
        if (idebug .ne. 0) write(idebug,*) ' BoLnd fourth ',BoLnd(iovh)
        GWL  (IOVH) = LVLOHMx(IOVH)
        V_IN  = RKWEL * AreaGwComp(IOVH) * timeSettings%timestepSize &
                - Q2O(IOVH) * timeSettings%timestepSize &
                - TotalSCurveSurfaceOutflow  * timeSettings%timestepSize &
                + QINB(IOVH)
!  groundwater irrigation?
        If (IrrigationSource(iovh) .eq. 2) V_IN = V_IN - IrrigationDemand(iovh) * TimeSettings%TimestepSize
        BOBD (IOVH) = BOBD0 (IOVH) + V_IN
        ! iterate if necessary
        IterGwlMaaiveld = IterGwlMaaiveld + 1
!27jun01  RMAXIN = MAX (0.0, INO(IOVH))
        RMAXIN = INO(IOVH)
        if (idebug .ne. 0)  WRITE(IDEBUG,*) ' Nieuwe RMAXIN=',RMAXIN
        IF ( (IterGwlMaaiveld .LE. 2) .or. (abs(in0-INO(IOVH)) .gt. .1 .and. IterGwlMaaiveld .le. MaxItrGwlMaaiveld) ) GOTO 1
      Endif

      IF (rkwel .gt. 0.0 .or. Q2O(iovh) .le. 0.0 .or. QinB(iovh) .gt. 0) then
      !  Ook meenemen dat Capsim te grote percolatie uitrekent terwijl infiltratie van maaiveld al op nul staat
      !  (bij te grote tijdstappen geval Querner komt dit voor)
      !  juni 2001: gw op maaiveld door overvloedige overvloedige kwel of zijwaartse instroming uit open water?
      !  schuif het resterende volume door naar maaiveld, volume unsatzone (Capsim) blijft ongewijzigd
      !  pas capris aan, pas ino aan, pas gwl aan, pas v_in aan
        if (idebug .ne. 0) WRITE(IDEBUG,*) ' GW at surface level; seepage'

        CapillaryRise = 0.0
        Percolation   = 0.0
        If (QinB(iovh) .lt. 0) then
           CapillaryRise = -QINB(iovh)
        else
           Percolation   = QINB(iovh)
        endif

        call ConfArr_set_FLDOVH(.TRUE.)
        if (idebug .ne. 0) WRITE(IDEBUG,*) ' Inflow soil in m3  :',INO(IOVH), QINB(IOVH)
        ! Bijstellen balanstermen; RestVol doorschuiven van gw naar surface
!       If (GWL0(iovh) .ge. LvlOhMx(iovh)) then
        If (GWL0(iovh) .gt. LvlOhMx(iovh)) then
           RestVol = V_IN
        else
          AREA = MAX (AreaGwComp(IOVH), 0.0001d0)
          If (UnSatZoneOption .ge. 1 )  then     ! Sobek-Capsim bergcoef=0.01 vanaf wortelzone hier hard in code
!           RestVol = V_In -  ( (LvlOhMx(iovh)-Gwl0(iovh))* 0.01 *Area )
            RestVol =  (Gwl(iovh)-LvlOhMx(iovh)) * 0.01 *Area + Max (0.0d0, VInToDo)
          Else
            RestVol = V_In -  ( (LvlOhMx(iovh)-Gwl0(iovh))*BergC(iovh)*Area )
          Endif
        Endif
        if (idebug .ne. 0) WRITE(IDEBUG,*) ' Check V_In VinToDo RestVol Gwl0 ',V_In, VinToDo, RestVol, Gwl0(iovh)
        QINB(IOVH) = QINB(IOVH) - RestVol
        INO (IOVH) = INO(IOVH) - RestVol
        If (CapillaryRise .gt. 0.0) then
           CapillaryRise = CapillaryRise + RestVol
        Else
           Percolation   = Percolation - Restvol
           If (Percolation .lt. 0.0) then
               CapillaryRise = -1 * Percolation
               Percolation   = 0.0
           Endif
        Endif
        QINB(IOVH) = (Percolation - CapillaryRise)

        if (idebug .ne. 0) WRITE(IDEBUG,*) ' After correction for inundation:',INO(IOVH), QINB(IOVH)
! berging op land neemt dus juist toe
        BOLND(IOVH) = BOLND(IOVH) + RestVol
        if (idebug .ne. 0) write(idebug,*) ' BoLnd fourth ',BoLnd(iovh)
        GWL  (IOVH) = LVLOHMx(IOVH)
        V_IN  = RKWEL * AreaGwComp(IOVH) * timeSettings%timestepSize &
              - Q2O(IOVH) * timeSettings%timestepSize &
              - TotalSCurveSurfaceOutflow  * timeSettings%timestepSize &
              + QINB(IOVH)
!  groundwater irrigation?
        If (IrrigationSource(iovh) .eq. 2) V_IN = V_IN - IrrigationDemand(iovh) * TimeSettings%TimestepSize
        BOBD (IOVH) = BOBD0 (IOVH) + V_IN
        ! iterate if necessary
        IterGwlMaaiveld = IterGwlMaaiveld + 1
!27jun01 RMAXIN = MAX (0.0, INO(IOVH))
        RMAXIN = INO(IOVH)
        if (idebug .ne. 0)  WRITE(IDEBUG,*) ' Nieuwe RMAXIN=',RMAXIN
        IF ( (IterGwlMaaiveld .LE. 2) .or. (abs(in0-INO(IOVH)) .gt. .1 .and. IterGwlMaaiveld .le. MaxItrGwlMaaiveld) ) GOTO 1
      ENDIF
    ENDIF
    IF (UnsatInfReduction) then
       IterUnsat = IterUnsat + 1
       If (IterUnsat .le. MaxItrUnsat) goto 1
    Endif

  Endif
! einde aanpassingen gw, gw op maaiveld voor Hellinga de Zeeuw

  ! *********************************************************************
  ! *** Iteratie voor variabele kwel, indien nodig
  ! *** niet afhankelijk van KvdL of HdeZ, maar alleen van SeepageCompOption
  ! *********************************************************************

    If (SeepageCompOption(iovh) .ge. 2 .and. IterSeepage .lt. MaxItrSeepage) then
       if (idebug .ne. 0) THEN
         WRITE(IDEBUG,*) ' Iteration Seepage computation', IterSeepage
         WRITE(IDEBUG,*) ' Kwel- wegzijging in m/s,m3 :',RKWEL, &
                                      RKWEL * AreaGwComp(IOVH) * timeSettings%timestepSize
         WRITE(IDEBUG,*) ' Final   groundwaterlevel m :',GWL   (IOVH)
       endif
       IterSeepage = IterSeepage + 1
       goto 11
    endif

! Bewaar berekende kwel in KWEL en WEGZG arrays
    If (SeepageCompOption(iovh) .ge. 2) then
       if (Rkwel .gt. 0) then
          Kwel(iovh)  = Rkwel
          WegZg(iovh) = 0.0
       else
          Kwel(iovh)  = 0.0
          WegZg(iovh) = -1 * RKwel
       endif
    endif


  ! *********************************************************************
  ! *** Bepaal berging op land, oppervlakkige afstroming
  ! ***   igv Krayenhoff vdLeur ook met de Hellinga-deZeeuw formule
  ! ***  (dus geen keus tussen HdeZeeuw of KvdLeur); Ernst wel afwijkend
  ! *********************************************************************

       If (IowSWLink .gt. 0 .or. IbndSWLink .gt. 0) then
          ! ARS 15464: with use of separate link for surface runoff
          Call SetUnpavedRelatedAveragePeil (Iovh, IowSwLink, IBndSWLink, ipluv, peil)
          If (CompOption(iovh) .le. 2) then
             Call HellingaDeZOppervlak (Inode, Iovh, Idebug, Peil, IowSwLink, IbndSWLink, ipluv)
          Elseif (CompOption(iovh) .eq. 3) then
             Call ErnstOppervlak (Inode, Iovh, Idebug, Peil, IowSWLink, IbndSWLink, ipluv)
          Endif
       Else
          ! original situation, only 1 downstream link, no separate surface runoff link
          If (CompOption(iovh) .le. 2) then
             Call HellingaDeZOppervlak (Inode, Iovh, Idebug, Peil, Iow, Ibnd, ipluv)
          Elseif (CompOption(iovh) .eq. 3) then
             Call ErnstOppervlak (Inode, Iovh, Idebug, Peil, Iow, Ibnd, ipluv)
          Endif
       Endif
  ! ARS 13651
       Q1O(IOVH) = min (Q1O(IOVH), TotalScurveSurfaceOutflowMax - TotalSCurveSurfaceOutflow)
       BOLND(IOVH) = BOLND0(IOVH) - Q1O(IOVH) * timeSettings%timestepSize - VO(IOVH) + RO(IOVH) - INO(IOVH) &
                                                            + IrrigationSupply(iovh) * TimeSettings%TimestepSize

  ! total surface outflow Q1O afstroming oppervlak plus reeds via laaggelegen delen van Scurve
  ! ARS 15464: GEEN aanpassing na introductie twee takken, waarvan 1 voor surface runoff
  !            aanname: Surface runoff en drainage toch naar praktisch dezelfde lokatie, alleen administratief scheiden van de flows
  !            geen effect op inundatie percentage berekeningen en Scurve-bodemdrainage berekeningen
       If (UseSCurve(iovh) .ne. 0)  then
          if (idebug .ne. 0) write(idebug,*) ' Q1O ', Q1O(iovh), TotalSCurveSurfaceOutflow
          Q1O(IOVH) = Q1O(IOVH) + TotalSCurveSurfaceOutflow
          OldQ1O = Q1O(iovh)
          ! nav ARS 12563 de layout en positie van Endifs verbeterd dd 3-3-2004, check impact HansHakvoort ARS 12483
          If (FixArs11610 .and. ibnd .gt. 0) then
            if (idebug .ne. 0) write(Idebug,*) ' ScurveSurfaceOutflow VolumeCheck FixArs11610 bnd'
            if (idebug .ne. 0) write(Idebug,*) ' BndPar5', BndPar(ibnd,5)
            if (idebug .ne. 0) write(Idebug,*) ' VolCheckFactor',UnpVolumeCheckFactorToCF
            Delta_bndpeil = Q1O(iovh)*TimeSettings%timestepSize / Bndpar(ibnd,5) / UnpVolumeCheckFactorToCF
            Delta_bndpeil = Delta_bndpeil + Peil
            ! March 2005 Taiwan: check on gwl level only if totalScurveSurfaceOutflow>0, only for ScurveSurfaceOutflowpart
            if (TotalScurveSurfaceOutflow .gt. 0 .and. Delta_bndpeil .gt. GWL0(iovh)) then  ! want ScurvesurfaceOutflow = flow a.g.v. gwl > maaiveld Scurve
              Q1O(iovh) = Q1O(IOVH) - TotalSCurveSurfaceOutflow
              if (idebug .ne. 0) write(Idebug,*) ' Gwl0',Gwl0(iovh)
              if (idebug .ne. 0) write(Idebug,*) ' Peil',Peil
              TotalScurveSurfaceOutflow = min (TotalScurveSurfaceOutflow, (Gwl0(iovh) - Peil)* BndPar(Ibnd,5) * &
                                              UnpVolumeCheckFactorToCF / TimeSettings%TimestepSize)
              TotalScurveSurfaceOutflow = max (TotalScurveSurfaceOutflow, 0.0d0)
              Q1O(iovh) = Q1O(iovh) + TotalScurveSurfaceOutflow
            endif
            if (idebug .ne. 0) Write(IDEBUG,*) ' Na VolumeCheck Berekende surface runoff ', Q1O(IOVH)
            Call UnpavedVolumeCheckActive (OldQ1O, Q1O(iovh), Inode, ibnd, iow)
            TotalScurveSurfaceOutflowMax = Q1O(iovh)
            IterGwlMaaiveld = IterGwlMaaiveld + 1
            IF ( (IterGwlMaaiveld .LE. 2) .or.  &
                   (abs(TotalScurveSurfaceOutflowMax-PrevTotalScurveSurfaceOutflowMax) .gt. .1 &
                      .and. IterGwlMaaiveld .le. MaxItrGwlMaaiveld) ) GOTO 1
          ElseIf (FixArs11610 .and. iow .gt. 0) then
            if (idebug .ne. 0) write(Idebug,*) ' ScurveSurfaceOutflow VolumeCheck FixArs11610 ow'
            Do i=1,NVal
               PeilArray(i) = PeilOw(i,iow)
               AreaArray(i) = AreaOw(i,iow)
            Enddo
            CALL RR_INTERP (NVAL, PEILArray, AreaArray, Sngl(Peil), TmpAreaOw, OwLastInterpIndex(iow))
            if (idebug .ne. 0) write(idebug,*) ' Q1O ', Q1O(iovh)
            Q1O(iovh) = min (Q1O(iovh), tmpAreaOw * (LVLOHMx(iovh)-lvlOw0(iOW)) * &
                                            UnpVolumeCheckFactorToCF / TimeSettings%TimestepSize)
            if (idebug .ne. 0) WRITE(IDEBUG,*) ' tmpAreaOw, ovhlevel, owlevel',tmpAreaOw, LVLOHMx(iovh), LVLOW0(iow)
            if (idebug .ne. 0) write(idebug,*) ' Q1O ', Q1O(iovh)
            Q1O(iovh) = max (Q1O(iovh), 0.0d0)
            if (idebug .ne. 0) Write(IDEBUG,*) ' Na VolumeCheck Berekende surface runoff ', Q1O(IOVH)
            Call UnpavedVolumeCheckActive (OldQ1O, Q1O(iovh), Inode, ibnd, iow)
            TotalScurveSurfaceOutflowMax = Q1O(iovh)
            IterGwlMaaiveld = IterGwlMaaiveld + 1
            IF ( (IterGwlMaaiveld .LE. 2) .or.  &
                   (abs(TotalScurveSurfaceOutflowMax-PrevTotalScurveSurfaceOutflowMax) .gt. .1 &
                      .and. IterGwlMaaiveld .le. MaxItrGwlMaaiveld) ) GOTO 1
          ElseIf (FixArs11610 .and. ipluv .gt. 0) then
            if (idebug .ne. 0) write(Idebug,*) ' ScurveSurfaceOutflow VolumeCheck FixArs11610 ipluv'
            ! no volumecheck added
            if (idebug .ne. 0) Write(IDEBUG,*) ' Na VolumeCheck Berekende surface runoff ', Q1O(IOVH)
            TotalScurveSurfaceOutflowMax = Q1O(iovh)
            IterGwlMaaiveld = IterGwlMaaiveld + 1
            IF ( (IterGwlMaaiveld .LE. 2) .or.  &
                   (abs(TotalScurveSurfaceOutflowMax-PrevTotalScurveSurfaceOutflowMax) .gt. .1 &
                      .and. IterGwlMaaiveld .le. MaxItrGwlMaaiveld) ) GOTO 1
          Endif
       Endif

    ! *********************************************************************
    ! *** Totaal naar open water of boundary, of NWRW
    ! *********************************************************************

!      temp. subtract irrigation from surface water from surface runoff Q1O (temporary) to determine flows to/from open water
       if (IrrigationSource(iovh) .eq. 1) then
          if (idebug .ne. 0) write(idebug,*) ' adjust q1o(iovh) with irrigation demand from sw ', q1o(iovh), IrrigationDemand(iovh)
          Q1O(iovh) = Q1O(iovh) - IrrigationDemand(iovh)
          if (idebug .ne. 0) write(idebug,*) ' adjusted q1o(iovh) ', q1o(iovh)
       endif

       If (.not. SWLinkFromExists(Inode))then
!         No separate surface runoff link, so all to one downstream link
          IF (IOW .GT. 0) THEN
            QINOW(IOW,2) = QINOW(IOW,2) + Q1O(IOVH) + Q2O(IOVH)
            if (idebug .ne. 0) THEN
               WRITE(IDEBUG,*) ' IOVH  INODE IOW  QINOW(IOW,2)'
               WRITE(IDEBUG,*) IOVH, INODE, IOW, QINOW(IOW,2)
            ENDIF
          ELSEIF (IBND .GT. 0) THEN
            QINBND(IBND)%totalUnpaved = QINBND(IBND)%totalUnpaved + Q1O(IOVH) + Q2O(IOVH)
            QBND(IBND) = QBND(IBND) + Q1O(IOVH) + Q2O(IOVH)
            if (idebug .ne. 0) THEN
               WRITE(IDEBUG,*) ' IOVH  INODE IBND  QINBND(ibnd,unpaved) QBND'
               WRITE(IDEBUG,*) IOVH, INODE, IBND, QINBND(IBND)%totalUnpaved, QBND(IBND)
            ENDIF
          ELSEIF (IPluv .GT. 0) THEN
            QINPluv(IPluv) = QINPluv(Ipluv) + Q1O(IOVH) + Q2O(IOVH)
            QPluv(IPluv)%totalUnpaved = QPluv(Ipluv)%totalUnpaved  + Q1O(IOVH) + Q2O(IOVH)
          ENDIF
       Else
!        ARS 15464: There is a separate SW link carrying only the surface runoff Q1O
!        JIRA Issue 22885: corrected index QInOW(IOW) to (IOSWLink)
          IF (IOWSWLink .GT. 0) THEN
            QINOW(IOWSWLink,2) = QINOW(IOWSWLink,2) + Q1O(IOVH)
            if (idebug .ne. 0) THEN
               WRITE(IDEBUG,*) ' IOVH  INODE IOWSWLink  QINOW(IOWSWLink,2)'
               WRITE(IDEBUG,*) IOVH, INODE, IOWSWLink, QINOW(IOWSWLink,2)
            ENDIF
          ELSEIF (IBndSWLink .GT. 0) THEN
            QINBND(IBNDSWLink)%totalUnpaved = QINBND(IBNDSWLink)%totalUnpaved + Q1O(IOVH)
            QBND(IBNDSWLink) = QBND(IBNDSWLink) + Q1O(IOVH)
            if (idebug .ne. 0) THEN
               WRITE(IDEBUG,*) ' IOVH  INODE IBNDSWLink  QINBND(ibndSwlink,unpaved) QBND'
               WRITE(IDEBUG,*) IOVH, INODE, IBNDSWLink, QINBND(IBNDSWLink)%totalUnpaved, QBND(IBNDSWLink)
            ENDIF
          Endif
          ! drainage flow (GW) Q2O
          IF (IOW .GT. 0) THEN
            QINOW(IOW,2) = QINOW(IOW,2) + Q2O(IOVH)
            if (idebug .ne. 0) THEN
               WRITE(IDEBUG,*) ' IOVH  INODE IOW  QINOW(IOW,2)'
               WRITE(IDEBUG,*) IOVH, INODE, IOW, QINOW(IOW,2)
            ENDIF
          ELSEIF (IBND .GT. 0) THEN
            QINBND(IBND)%totalUnpaved = QINBND(IBND)%totalUnpaved + Q2O(IOVH)
            QBND(IBND) = QBND(IBND) + Q2O(IOVH)
            if (idebug .ne. 0) THEN
               WRITE(IDEBUG,*) ' IOVH  INODE IBND  QINBND(ibnd,unpaved) QBND'
               WRITE(IDEBUG,*) IOVH, INODE, IBND, QINBND(IBND)%totalUnpaved, QBND(IBND)
            ENDIF
          ELSEIF (IPluv .GT. 0) THEN
            QINPluv(IPluv) = QINPluv(Ipluv) + Q2O(IOVH)
            QPluv(IPluv)%totalUnpaved = QPluv(Ipluv)%totalUnpaved  + Q2O(IOVH)
          ENDIF
       Endif
!      restore original valyue Q1O again (remove irrigation)
       if (IrrigationSource(iovh) .eq. 1) then
          if (idebug .ne. 0) write(idebug,*) ' reset q1o(iovh) to value without irrigation demand from sw ', q1o(iovh), IrrigationDemand(iovh)
          Q1O(iovh) = Q1O(iovh) + IrrigationDemand(iovh)
          if (idebug .ne. 0) write(idebug,*) ' resetted q1o(iovh) ', q1o(iovh)
       endif

! Bepaal % inundation, ARS 1887 Scurve
! without S curve: inundation = 0 or 100% (LvlOhMx < LvlOw
! with S curve: inundation if SurfaceOutflow >0 or LvlohMx < LvlOw
! gerelateerd peil volgt uit peil open water of peil op rand
! ARS 15464: GEEN aanpassing berekening percentage geinundeerd, altijd kijken naar peil via normale tak

       Call SetPercentageInundationUnpaved (iovh, iow, ibnd, ipluv, PercentageInundation, iout1)
       PercInundation(iovh) = PercentageInundation

    ! Check KvdL result, give warning in case gw level does not rise while gw-volume does
!    If (itmstp .gt. 1) then
!     If ( (GWL(iovh) .le. GWL0(iovh) .and. (BOBD(iovh)-BOBD0(iovh))/AreaOh(iovh) .gt. +1.E-6) .or. &
!         (GWL(iovh) .gt. GWL0(iovh) .and. (BOBD(iovh)-BOBD0(iovh))/AreaOh(iovh) .le. -1.E-6) ) then
!       write(Iout1,'(A,I6,A,A)') ' Warning for unpaved node computations for timestep', Itmstp, ' and node ', ID_Nod(Inode)
!       write(Iout1,'(A,2E12.5)') ' gwl level and volume differences:',GWL(iovh)-GWL0(iovh), BOBD(iovh)-BOBD0(iovh)
!       if (CompOption(iovh) .eq. 2) Then
!         write(Iout1,*) ' Krayenhoff-vdLeur formulation results in inconsistent behaviour of groundwater level and volume (e.g. gw-volume rises, while level remains equal or drops)'
!         write(Iout1,*) ' Check if you included enough terms in the Krayenhoff-vdLeur formulation using the KVDLDIMENSIONINDAYS= option in the INI file'
!       endif
!     Endif
!    Endif


    ! *********************************************************************
    ! *** debug
    ! *********************************************************************

    if (idebug .ne. 0) THEN
       WRITE(IDEBUG,*) ' Unpaved area', Id_Nod(INODE)
       WRITE(IDEBUG,*) ' Timestep nr                :',ITMSTP
       WRITE(IDEBUG,*) ' Timestepsize in s          :',timeSettings%timestepSize
       Write(Idebug,*) ' Perc. inundation           :', PercInundation(iovh)
       Write(Idebug,*) ' UseSCurve                  :', UseScurve(iovh)
       WRITE(IDEBUG,*) ' Storage-coefficient        :',BERGC(IOVH)
       WRITE(IDEBUG,*) ' Storage-coefficient HdeZ   :',HdeZBergC(IOVH)
       WRITE(IDEBUG,*) ' infiltration-rate          :',INF_V(IOVH), INF_V(IOVH) * NRSHR / MM2M
       WRITE(IDEBUG,*) ' Coeff. alfa land           :',ALFAOH(IOVH,1)
       WRITE(IDEBUG,*) ' Coeff. alfa soil drainage  :',ALFAOH(IOVH,2)
       WRITE(IDEBUG,*) ' Coeff. alfa soil infiltr.  :',ALFAOH(IOVH,3)
       WRITE(IDEBUG,*) ' Coeff. alfa soil 1         :',ALFA2 (IOVH,1)
       WRITE(IDEBUG,*) ' Coeff. alfa soil 2         :',ALFA2 (IOVH,2)
       WRITE(IDEBUG,*) ' Coeff. alfa soil 3         :',ALFA2 (IOVH,3)
       WRITE(IDEBUG,*) ' Ernst resistance 1         :',ErnstResistance(IOVH,1)
       WRITE(IDEBUG,*) ' Ernst resistance 2         :',ErnstResistance(IOVH,2)
       WRITE(IDEBUG,*) ' Ernst resistance 3         :',ErnstResistance(IOVH,3)
       WRITE(IDEBUG,*) ' Ernst resistance 4         :',ErnstResistance(IOVH,4)
       WRITE(IDEBUG,*) ' Ernst resistance 5         :',ErnstResistance(IOVH,5)
       WRITE(IDEBUG,*) ' Ernst resistance 6         :',ErnstResistance(IOVH,6)
       WRITE(IDEBUG,*) ' Reservoir coeff. KvdLeur   :',ReservoirCoeff(IOVH)
       IF (IOW .GT. 0) THEN
          WRITE(IDEBUG,*) ' Related open water level m :',LVLOW0(IOW), PEIL, LVLOW(IOW)
       ELSE
          WRITE(IDEBUG,*) ' Related boundary level m   :',PEIL
       ENDIF
       WRITE(IDEBUG,*) ' Surface level m            :',LVLOH (IOVH), LvlOhMx(iovh)
       WRITE(IDEBUG,*) ' Total surface in m2        :',AREAOH(IOVH)
       WRITE(IDEBUG,*) ' Area for seepage/gw comp.  :',AreaGwComp(IOVH)
       WRITE(IDEBUG,*) ' Initial  storage land   m3 :',BOLND0(IOVH)
       WRITE(IDEBUG,*) ' Final    storage land   m3 :',BOLND (IOVH)
       WRITE(IDEBUG,*) ' Initial  storage unsat  m3 :',OnvZone(IOVH)%Init_Volume
       WRITE(IDEBUG,*) ' Initial  storage unsat  mm :',OnvZone(IOVH)%Init_mm
       WRITE(IDEBUG,*) ' Final    storage unsat  m3 :',OnvZone(IOVH)%Actual_Volume
       WRITE(IDEBUG,*) ' Final    storage unsat  mm :',OnvZone(IOVH)%Actual_mm
       WRITE(IDEBUG,'(A,E15.8)') ' Initial groundwaterlevel m :',GWL0  (IOVH)
       WRITE(IDEBUG,'(A,E15.8)') ' Final   groundwaterlevel m :',GWL   (IOVH)
       WRITE(IDEBUG,*) ' Initial  berging soil   m3 :',BOBD0 (IOVH)
       WRITE(IDEBUG,*) ' Final    berging soil   m3 :',BOBD  (IOVH)
       WRITE(IDEBUG,*) ' Evaporation land        m3 :',VO    (IOVH)
       WRITE(IDEBUG,*) ' Precipitation           m3 :',RO    (IOVH)
       WRITE(IDEBUG,*) ' SeepageComputationOption   :',SeepageCompOption(iovh)
       If (SeepageCompOption(iovh) .ge. 2) then
          WRITE(IDEBUG,*) ' H0Actual for SeepageComp   :',H0Actual(iovh)
       Endif
       WRITE(IDEBUG,*) ' Kwel- wegzijging in m/s,m3 :',RKWEL, &
                                      RKWEL * AreaGwComp(IOVH) * timeSettings%timestepSize
       WRITE(IDEBUG,*) ' Inflow soil in m3          :',INO   (IOVH)
       WRITE(IDEBUG,*) ' Percolation unsat-gw in m3 :',QINB   (IOVH)
       WRITE(IDEBUG,*) ' Evaporation unsat in m3    :',VBO   (IOVH)
       WRITE(IDEBUG,*) ' Surface runoff in m3/s,m3  :',Q1O   (IOVH), &
                                                  Q1O(IOVH)*timeSettings%timestepSize
       WRITE(IDEBUG,*) ' Outflow soil m3/s,m3  :',Q2O   (IOVH), &
                                                  Q2O(IOVH)*timeSettings%timestepSize
       WRITE(IDEBUG,*) ' Irrigation supply m3/s, m3 :',IrrigationSupply(iovh), IrrigationSupply(iovh) * timeSettings%timestepSize
       WRITE(IDEBUG,*) ' Irrigation demand m3/s, m3 :',IrrigationDemand(iovh), IrrigationDemand(iovh) * timeSettings%timestepSize
       WRITE(IDEBUG,*) ' Irrigation demand source 0=no,1=sw,2=gw,3=ext) :',IrrigationSource(iovh)

!   Test ARS 13651: balansfout unpaved
       Rkwel = (Kwel(iovh) - wegzg (iovh)) * AreaGwComp(iovh) * timeSettings%TimestepSize
       BalTerm1 = Ro(iovh) + rkwel + IrrigationSupply(iovh) * TimeSettings%TimestepSize
       BalTerm2 = 0
       BalTerm3 = Vo(iovh) + vbo(iovh)
       If (IrrigationSource(iovh) .eq. 2) BalTerm3 = BalTerm3 + IrrigationDemand(iovh) * TimeSettings%TimestepSize
       BalTerm4 = timesettings%timestepSize * (q1o(iovh) + q2o(iovh))
       BalTerm5 = bolnd(iovh) + bobd(iovh) - bolnd0(iovh) - bobd0(iovh) &
                    + OnvZone(Iovh)%Actual_Volume - OnvZone(Iovh)%Init_Volume
       BalTerm6 = BalTerm1 + BalTerm2 - BalTerm3 - Balterm4 - Balterm5
       write(idebug,*) ' balansterm 1 in at node       ', balterm1
       write(idebug,*) ' balansterm 2 in through links ', balterm2
       write(idebug,*) ' balansterm 3 out at node      ', balterm3
       write(idebug,*) ' balansterm 4 out through links', balterm4
       write(idebug,*) ' balansterm 5 change of storage', balterm5
       write(idebug,*) ' balansfout   ', balterm6
    ENDIF

    idebug = 0

    RETURN
  END subroutine cmpovh


 ! *********************************************************************
 ! *** Routines for finding related open water/boundary and water level
 ! *********************************************************************

  Subroutine SetUnpavedRelatedAveragePeil (Iovh, Iow, ibnd, ipluv, peil)
  Integer Iovh, iow, ibnd, ipluv
  Double precision    Peil

    If (IOW .gt. 0) then
       PEIL = (1.0 - timesettings%timeWeightFactor) * LVLOW0(IOW) + &
                               timesettings%timeWeightFactor * LVLOW(IOW)
    Elseif (IBND .gt. 0) then
       PEIL = BNDPAR(IBND,1)
    Elseif (IPluv .gt. 0) then
       PEIL = GWLINI(iovh)
       if (SbkLvlPluv(ipluv) .gt. -999.) Peil = SbkLvlPluv(ipluv)
    Else
       call SetMessage(LEVEL_ERROR, 'Internal error 3B-CMPOVH: IOW=IBND=0')
       call ErrMsgStandard (981, 0, 'Unpaved node should have an RR-link to RR-open water, RR-boundary or RR-CF connection for drainage', ' ')
    Endif

    RETURN
  END subroutine SetUnpavedRelatedAveragePeil



  Subroutine SetUnpavedRelatedPeil (Iovh, Iow, ibnd, ipluv, peil)

  Implicit none
  Integer Iovh, iow, ibnd, ipluv
  Double precision    Peil

    If (IOW .gt. 0) then
       PEIL = (1.0 - timesettings%timeWeightFactor) * LVLOW0(IOW) + &
                               timesettings%timeWeightFactor * LVLOW(IOW)
    Elseif (IBND .gt. 0) then
       PEIL = BNDPAR(IBND,1)
    Elseif (IPluv .gt. 0) then
       PEIL = GWLINI(iovh)
       if (SbkLvlPluv(ipluv) .gt. -999.) Peil = SbkLvlPluv(ipluv)
    Else
       call SetMessage(LEVEL_ERROR, 'Internal error 3B-CMPOVH: IOW=IBND=0')
       call ErrMsgStandard (981, 0, 'Unpaved node should have an RR-link to RR-open water, RR-boundary or RR-CF connection for drainage', ' ')
    Endif

    RETURN
  END subroutine SetUnpavedRelatedPeil


    ! *********************************************************************
    ! *** Routines for computing inundation percentage
    ! *********************************************************************

  Subroutine DeterminePercentageInundationUnpaved (iovh, inode, PercentageInundation, iout1)

    Implicit none

    ! variables
    Integer iOvh, inode, iow, ibnd, ipluv, iout1
    Double precision    PercentageInundation

    ! related open water node and related boundary node
    IOW  = EIOW(INODE)
    IBND = EIBND(INODE)
    IPLUV= EIPLUV(INODE)

    Call SetPercentageInundationUnpaved (iovh, iow, ibnd, ipluv, PercentageInundation, iout1)

    RETURN
  END subroutine DeterminePercentageInundationUnpaved



  Subroutine SetPercentageInundationUnpaved (iovh, iow, ibnd, ipluv, PercentageInundation, iout1)

    Implicit none

    ! variables
    Integer iOvh, iow, ibnd, ipluv, iout1, ipoint
    Double precision    Rpeil, PercentageInundation

    Call SetUnpavedRelatedPeil (Iovh, Iow, ibnd, ipluv, Rpeil)

    PercentageInundation = 0.
    If ( LvlOhMx(iovh) .lt. Rpeil ) Then
        PercentageInundation = 100.
    Elseif (UseScurve(iovh) .ne. 0) Then
        Do ipoint=1,UseUnpavedScurve
!          If (AreaScurve(iovh,ipoint)%SurfaceOutflow .gt. 0 .or. AreaScurve(iovh,ipoint)%Level .lt. Rpeil ) Then
! ARS 5599: of als GWL > maaiveld van dit punt van de Scurve
           If (AreaScurve(iovh,ipoint)%SurfaceOutflow .gt. 0 .or. AreaScurve(iovh,ipoint)%Level .lt. Rpeil .or. &
                 AreaScurve(iovh,ipoint)%Level .lt. Gwl0(iovh) ) Then
               PercentageInundation = PercentageInundation + AreaScurve(iovh,ipoint)%Percentage
           Endif
        Enddo
    Endif

    RETURN
  END subroutine SetPercentageInundationUnpaved



  SUBROUTINE ComputeSurface (IOVH, INode, IMETEO, RMaxIn, IterGwlMaaiveld, IterUnsat)
    ! *********************************************************************
    ! *** Surface computations unpaved area
    ! ***   Iovh   = index unpaved node
    ! ***   Imeteo = index meteostation
    ! ***   RMaxin = max. infiltration
    ! ***   IterGwlMaaiveld = iteration counter on infiltration reduction
    ! ***   IrrigationSupply = irrigation in m3/s
    ! *********************************************************************

    Implicit none

    ! variables
    Integer iOvh, iNode, iMeteo
    Double precision    rMaxIn, Vpot, Infpot, Bnew, ratio
    Integer IterGwlMaaiveld, IterUnsat

    Integer iDebug, iOut1

    iDebug = ConfFil_get_iDebug()
    iOut1 = ConfFil_get_iOut1()

    ! *********************************************************************
    ! *** Verdamping op land (onverhard gebied)
    ! *** Verdamping alleen zolang berging>0
    ! *********************************************************************

!    RO(IOVH) = AAFNodeRainfall(inode) * RAIN(IMETEO) * AREAOH(IOVH) * timeSettings%timestepSize
!    VO(IOVH) = 0.0
!    IF (ConfArr_get_iHour() .GE. timeSettings%evaporationFromHr .AND. &
!        ConfArr_get_iHour() .LT. timeSettings%evaporationToHr) THEN
!!     IF (BOLND0(IOVH) .GT. 0) &         ! Sept 2010: less strict test on positive storage on land, cf irrigation supply results
!!     IF (BOLND0(IOVH) .GT. 1.0E-4) &
!! Issue 49802 April 2015: like paved, no check on initial storage,
!!                 it will be corrected immediately below if there is no surface storage, rain, or irrigation to provide for evaporation
!       VO(IOVH) = EVAP (IMETEO) * CROPO * AREAOH(IOVH) * timeSettings%timestepSize * TMEVAP
!    ENDIF
!    BOLND(IOVH) = BOLND0(IOVH) - VO(IOVH) + RO (IOVH) + IrrigationSupply(iovh) * timeSettings%timestepSize
!
!    IF (BOLND(IOVH) .LT. 0) THEN
!       VO(IOVH) = VO(IOVH) + BOLND(IOVH)
!       BOLND(IOVH) = 0.0
!    ENDIF
!
!    ! *********************************************************************
!    ! *** Bepaal infiltratie, voorlopige berging op land
!    ! *********************************************************************
!
!    INO(IOVH) = MIN (BOLND(IOVH), INF_V(IOVH)*AREAOH(IOVH)*timeSettings%timestepSize )
!    IF (IterGwlMaaiveld .GE. 1 .or. IterUnsat .ge. 1) INO(IOVH) = MIN (INO(IOVH), RMAXIN)
!    BOLND(IOVH) = BOLND(IOVH) - INO(IOVH)

!   Nieuwe versie
    RO(IOVH) = AAFNodeRainfall(inode) * RAIN(IMETEO) * AREAOH(IOVH) * timeSettings%timestepSize
    Bnew = BOLND0(IOVH) + RO (IOVH) + IrrigationSupply(iovh) * timeSettings%timestepSize
    If (BNew .le. 0) then
        Vpot   = 0.0
        InfPot = 0.0
    else
        IF (ConfArr_get_iHour() .GE. timeSettings%evaporationFromHr .AND. &
            ConfArr_get_iHour() .LT. timeSettings%evaporationToHr) THEN
           Vpot = EVAP (IMETEO) * CROPO * AREAOH(IOVH) * timeSettings%timestepSize * TMEVAP
        else
           Vpot = 0.0
        endif
        InfPot = INF_V(IOVH)*AREAOH(IOVH)*timeSettings%timestepSize
        IF (IterGwlMaaiveld .GE. 1 .or. IterUnsat .ge. 1) InfPot = MIN (Infpot, RMAXIN)

        Bnew = BOLND0(IOVH) - Vpot + RO (IOVH) + IrrigationSupply(iovh) * timeSettings%timestepSize - Infpot
        if (idebug .ne. 0) write(Idebug,*) ' Vpot Infpot Bnew', Vpot, Infpot, Bnew
        if (BNew .lt. 0) then
           If (InfPot .ge. 0)  then
              ratio  = Vpot / ( Vpot + Infpot )
              Vpot   = Vpot   +  Bnew * ratio
              Infpot = Infpot +  Bnew * ( 1D0 - ratio )
           else
              ! InfPot < 0, negatief door te hoge gwl (terugdrukken van water naar maaiveld), alleen Vpot is onttrekkende term
              Vpot = Vpot + BNew
           endif
        ENDIF
    Endif
    VO (IOVH) = Vpot
    INO (IOVH) = InfPot
    BOLND(IOVH)= BOLND0(IOVH) + RO (IOVH) + IrrigationSupply(iovh) * timeSettings%timestepSize - Vpot - Infpot
    if (idebug .ne. 0) write(Idebug,*)' Vpot Infpot Bnew', Vpot, Infpot, BoLnd(iovh)



    RETURN
  END subroutine ComputeSurface



    SUBROUTINE CapsimStorageCoefficient (IOVH)
    ! *********************************************************************
    ! *** Last update:  March 1995
    ! *********************************************************************
    ! *** Brief description:
    ! *** ------------------
    ! ***    This subroutine determines the storage coefficient
    ! *********************************************************************
    ! *** Input/output parameters:
    ! *** ------------------------
    ! ***  IEVENT = event number
    ! ***  ITMSTP = timestep number
    ! ***  IDEBUG = file unit number of debug file
    ! ***  IOUT1  = file unit number of output file with run messages
    ! ***  IOVH   = intern onverhard gebied nr
    ! ***  IMETEO = meteostation code
    ! ***  INODE  = knoopnr
    ! ***  makelogfile = indicator of logfile gewenst is (0=nee)
    ! *********************************************************************
    ! *** Berekeningen voor onverhard gebied - bergingscoefficient
    ! *********************************************************************

      Implicit none

    ! variables
      Integer iOvh, Isoil
      Integer iDebug, iOut1, IFlaglocal
      Double precision    DpIn, NewStoragecoefficient, GwLevel

      iFlaglocal = 1   ! Call Capsim2StorageCoefficient om storage coefficient te bepalen
      iDebug = ConfFil_get_iDebug()
      iOut1 = ConfFil_get_iOut1()
      if (idebug .ne. 0)  WRITE(IDEBUG,*) 'CapsimStorageCoeff iovh=',IOVH

 ! Set soil index, groundwater level
      ISoil  = BotTyp (iovh)
      GwLevel= 0.5 * GWL0 (iovh) + 0.5 * GWL(iovh)
! April2002 DpIn   = LVLOH(iovh) - GwLevel
      DpIn   = LVLOHMx(iovh) - GwLevel
 ! Compute storage coefficient
      if (idebug .ne. 0)  WRITE(IDEBUG,*) ' Call to Capsim3StorageCoeff with DpIn LvlOhMx GwLevel ', Dpin, LvlOhMx(iovh), GwLevel
      Call Capsim3StorageCoefficient (Iovh, DpIn, IFlaglocal, NewStorageCoefficient)
      CapsimBergC(iovh) = NewStorageCoefficient

    RETURN
  END subroutine CapsimStorageCoefficient



    SUBROUTINE Capsim3StorageCoefficient (Iovh, DpIn, IFlag, NewStorageCoefficient)
    ! *********************************************************************
    ! *** Last update:  March 1995
    ! *********************************************************************
    ! *** Brief description:
    ! *** ------------------
    ! ***    This subroutine determines the storage coefficient
    ! *********************************************************************
    ! *********************************************************************
    ! *** Berekeningen voor onverhard gebied - bergingscoefficient
    ! *********************************************************************

      Implicit none

    ! variables
      Integer iOvh, Icrop, Isoil
      Integer iDebug, iOut1, IFlag
      Double precision    DpIn, DpRootz, NewStoragecoefficient, GwLevel

      iDebug = ConfFil_get_iDebug()
      iOut1 = ConfFil_get_iOut1()
      if (idebug .ne. 0)  WRITE(IDEBUG,*) 'Capsim3StorageCoeff iovh=',IOVH

 ! Set soil index, groundwater level
      ISoil  = BotTyp (iovh)
      CapsimBergC(iovh) = 0.0
! ARS xxxx April 2002: GwLevel was not defined (so zero) before SetAverageStorageCoefficient was called
! met LvlOhMx of LvlOh?
      GwLevel = LvlOhMx(iovh) - DpIn
!     GwLevel = LvlOh(iovh) - DpIn

      If (CapsimPerCropArea .eq. 0) then
 ! Old method: call Capsim once per unpaved area, using average rootzone depth and main crop
 ! Set crop index = crop with largest area
 ! rootzonedepth  = area weighted crop average rootzone depth, determined in Init1
        Icrop  = CapsimCrop(iovh)
 ! Determine average rootzone depth in cm
        DpRootz  = CapsimDpRootz(iovh) * 100.
        If (UseScurve(iovh) .eq. 0) then
          ! Initialiseer Capsim bergingscoefficient, zonder S curve maaiveld
          Call Capsim2StorageCoefficient (ISoil, DpIn, DpRootz, IFlag, NewStorageCoefficient)
        ElseIf (UseScurve(iovh) .eq. 1) then
          ! Initialiseer Capsim bergingscoefficient, met S curve maaiveld
          Call SetAverageStorageCoefficient (Iovh, ISoil, GwLevel, DpRootZ, IFlag, NewStorageCoefficient)
        Endif
        CapsimBergC(iovh) = NewStorageCoefficient
      Else
!       May 2001
!       Apply Capsim in Detail, i.e. call it for all Crop areas separately
        Do Icrop =1,NCrop
           If (AreaGw(iovh,icrop) .gt. 0) then
!          Rootzone depth in cm for crop Icrop
             DpRootz  = DpRz(ISoil,Icrop) * 100.
             If (UseScurve(iovh) .eq. 0) then
               ! Initialiseer Capsim bergingscoefficient, zonder S curve maaiveld
               Call Capsim2StorageCoefficient (ISoil, DpIn, DpRootz, IFlag, NewStorageCoefficient)
             ElseIf (UseScurve(iovh) .eq. 1) then
               ! Initialiseer Capsim bergingscoefficient, met S curve maaiveld
               Call SetAverageStorageCoefficient (Iovh, ISoil, GwLevel, DpRootZ, IFlag, NewStorageCoefficient)
             Endif
             CapsimBergC(iovh) = CapSimBergC(iovh) + NewStorageCoefficient * AreaGw(Iovh,icrop)
           Endif
        Enddo
        CapsimBergC(iovh) = CapSimBergC(iovh) / AreaOh(iovh)
      Endif

! ARS 10693: prevent zero storage coefficient
      If (AreaOh(Iovh) .le. .0001) CapsimBergC(iovh) = 0.05

! return value
      NewStorageCoefficient = CapsimBergC(iovh)

    RETURN
    END subroutine Capsim3StorageCoefficient


     Subroutine SetAverageStorageCoefficient (Iovh, ISoil, GwLevel, DpRootZ, IFlag, NewStorageCoefficient)

     Double precision    GwLevel, DpRootZ, NewStorageCoefficient
     Integer Iovh, Isoil, IFlag

     Double precision    DpIn, AverageStorageCoefficient
     Integer IPoint, NAverage

      AverageStorageCoefficient = 0.0
      NAverage = 0
      Do Ipoint=UseUnpavedSCurve, 1, -1
        DpIn   = AreaScurve(iovh,ipoint)%Level - GwLevel
        if (DpIn .gt. 0) then
           Call Capsim2StorageCoefficient (ISoil, DpIn, DpRootz, IFlag, NewStorageCoefficient)
           AverageStorageCoefficient = AverageStorageCoefficient + NewStorageCoefficient
           NAverage = NAverage + 1
        else
          goto 101
        endif
      Enddo
  101 Continue
      If (NAverage .eq. 0) then
        NewStorageCoefficient = 0.01
      Else
        NewStorageCoefficient = AverageStorageCoefficient / NAverage
      Endif

    Return
    End subroutine SetAverageStorageCoefficient



  SUBROUTINE Capsim2StorageCoefficient (ISoil, DpInIn, DpRootz, IFlag, Result)

    ! *********************************************************************
    ! *** Last update:  March 1995
    ! *********************************************************************
    ! *** Brief description:
    ! *** ------------------
    ! ***    This subroutine performs computations for Capsim
    ! ***     IFLAG=1: determine storage coefficient
    ! ***     IFLAG=2: determine soil moisture content rootzone
    ! *********************************************************************
    ! *** Input/output parameters:
    ! *** ------------------------
    ! *********************************************************************
    ! *** Berekeningen voor onverhard gebied
    ! *********************************************************************

      Implicit none

    ! variables
      Integer Isoil, itemp, Iflag, D_IfReal, idum
      Integer iDebug, iOut1, ixrz, ixrz1, ixdpun, ixdpun1, ixdpun2, ixdpun3, ival, i
      Double precision    DpInIn, DpIn, DpRootz, Drz, Dgwl, result, NewStoragecoefficient, TempstorageCoefficient
      Double precision    Mu(4), Distance(4), Gewicht(4), TotalDistance, TotalGewicht


!local array for interpolation
      Double precision    XX (50)


      DpIn = Max (0.0d0, DpInIn)        !DpIn is lokale kopie van DpIn mits positief.

      iDebug = ConfFil_get_iDebug()
      iOut1 = ConfFil_get_iOut1()
      if (idebug .ne. 0)  WRITE(IDEBUG,*) 'Capsim2StorageCoeff'

 ! Juni 2001: als gw in de wortelzone staat, dan is Capsim storage coefficient 0.01     N.B. HARD in code
 ! via interpolatie zou toch een grotere waarde gevonden worden, dus niet interpoleren
 ! de check kan pas weg op het moment dat ALLE mogelijke wortelzonediktes volgens de RootSim.INP ook in de UNSASIM.INP voorkomen
      If (iflag .eq. 1 .and. DpIn .le. DpRootz/100. .and. CheckRootzoneGwl) then
         Result = 0.01
         Goto 999
      Endif

 ! Determine Root zone index ixrz and ixrz1

 ! Locate geeft index IVAl terug, waarvoor XX(ival) <= Dprootz <= XX(ival+1)
 ! met XX = array DpRzUn
 ! Ival=0 of N betekent out of range.
      CALL D_LOCATE (DPRZUN, NActRzClass, Dprootz, IVAL)
      ixrz = ival
      ixrz1 = ixrz + 1
      if (ival .le. 0) then
         ixrz  = 1
         ixrz1 = 1
      elseif (ival .ge. nActRzClass) then
         ixrz  = nActRzClass
         ixrz1 = nActRzClass
      endif
! DpRootZ komt voor in tabel?
! ARS **** bij verschillende dingen doen igv = en < of >: gebruik IfReal routine
!      if ( DpRootZ .le. DpRzUn(ixrz) )  ixrz1 = ixrz
!      if ( DpRootZ .ge. DpRzUn(ixrz1) ) ixrz  = ixrz1
      idum = D_IfReal (DpRootZ, DpRzUn(ixrz), 1D-8)
      if (Idum .le. 0) ixrz1 = ixrz
      idum = D_IfReal (DpRootZ, DpRzUn(ixrz1), 1D-8)
      if (Idum .ge. 0) ixrz  = ixrz1


! Determine gwl index bij ixrz: ixdpun and ixdpun+1
      Do i=1, nxdpun
        XX(i) = DPGWUN(isoil,ixrz,i)
      Enddo

      CALL D_LOCATE (XX, NActGwlClass, DpIn, IVAL)
      ixdpun = ival
      ixdpun1 = ixdpun + 1
      if (ival .le. 0) then
         ixdpun  = 1
         ixdpun1 = 1
      elseif (ival .ge. nActGwlClass) then
         ixdpun  = nActGwlClass
         ixdpun1 = nActGwlClass
      endif
! DpIn komt voor in tabel?
! ARS **** bij verschillende dingen doen igv = en < of >: gebruik IfReal routine
!     if ( DpIn .le. XX(ixdpun) )  ixdpun1 = ixdpun
!     if ( DpIn .ge. XX(ixdpun1) ) ixdpun  = ixdpun1
      idum = D_IfReal (DpIn, XX(ixdpun), 1D-8)
      if (Idum .le. 0) ixdpun1 = ixdpun
      idum = D_IfReal (DpIn, XX(ixdpun1), 1D-8)
      if (Idum .ge. 0) ixdpun = ixdpun1

! gwl indices bij punt ixrz+1: ixdpun2 en ixdpun3
      Do i=1, nxdpun
         XX(i) = DPGWUN(isoil,ixrz1,i)
      Enddo

      CALL D_LOCATE (XX, NActGwlClass, DpIn, IVAL)
      ixdpun2 = ival
      ixdpun3 = ixdpun2 + 1
      if (ival .le. 0) then
         ixdpun2 = 1
         ixdpun3 = 1
      elseif (ival .ge. nActGwlClass) then
         ixdpun2 = nActGwlClass
         ixdpun3 = nActGwlClass
      endif
! DpIn komt voor in tabel?
! ARS **** bij verschillende dingen doen igv = en < of >: gebruik IfReal routine
!     if ( DpIn .le. XX(ixdpun2) ) ixdpun3 = ixdpun2
!     if ( DpIn .ge. XX(ixdpun3) ) ixdpun2 = ixdpun3
      idum = D_IfReal (DpIn, XX(ixdpun2), 1D-8)
      if (Idum .le. 0) ixdpun3 = ixdpun2
      idum = D_IfReal (DpIn, XX(ixdpun3), 1D-8)
      if (Idum .ge. 0) ixdpun2 = ixdpun3


! Relevante punten dus  voor ixrz:   (ixrz ,ixdpun ), (ixrz , ixdpun1)
!                       voor ixrz1:  (ixrz1,ixdpun2), (ixrz1, ixdpun3)
! Op basis van deze punten voor SCSA de gemiddelde bepalen.
! Geen extrapolatie
! Interpolatie op basis van coefficienten ingelezen in RdIni (default coef=1, power=2)
! punten met grote afstand krijgen een laag gewicht.

! Optie IFlag: default = 1 = storage coefficient        gebruik SCSA
!                        2 = moisture content rootzone  gebruik SRRZ
      if (Iflag .ne. 2) Iflag =1
      if (idebug .ne. 0) write (idebug,*) ' IFlag= 1=StorageCoeff,2=RootZoneMoisture', IFlag
      if (Iflag .eq. 1) then
        mu(1) = SCSA (isoil, ixrz ,ixdpun)
        mu(2) = SCSA (isoil, ixrz ,ixdpun1)
        mu(3) = SCSA (isoil, ixrz1,ixdpun2)
        mu(4) = SCSA (isoil, ixrz1,ixdpun3)
      elseif (Iflag .eq. 2) then
        mu(1) = SRRZ (isoil, ixrz ,ixdpun)
        mu(2) = SRRZ (isoil, ixrz ,ixdpun1)
        mu(3) = SRRZ (isoil, ixrz1,ixdpun2)
        mu(4) = SRRZ (isoil, ixrz1,ixdpun3)
      endif

! Bepaal 'afstand' = CoefRz (delta rootzone in m ** PowerRz ) +
!                       CoefGwl (delta gwlevel in m ** PowerGwl)

      Drz  = ( DpRootz - DpRzUn(ixrz) ) / 100.
      Dgwl =   DpIn - DpGwUn(isoil,ixrz,ixdpun)
!     Distance(1) =  abs (CoefRz * (Drz ** PowerRz)) + abs (CoefGwl * (Dgwl ** PowerGwl))
      Distance(1) = 0.0
      If (Abs(Drz)  .gt. 0.0000001) Distance(1) = Distance(1) + abs (CoefRz  * (Abs(Drz) ** PowerRz))
      If (Abs(Dgwl) .gt. 0.0000001) Distance(1) = Distance(1) + abs (CoefGwl * (Abs(Dgwl) ** PowerGwl))

      Drz  = ( DpRootz - DpRzUn(ixrz) ) / 100.
      Dgwl =   DpIn - DpGwUn(isoil,ixrz,ixdpun1)
!     Distance(2) =  abs (CoefRz * (Drz ** PowerRz)) + abs (CoefGwl * (Dgwl ** PowerGwl))
      Distance(2) = 0.0
      if (idebug .ne. 0) then
         write(idebug,*) ' Unpavedmodule line 2397'
         write(idebug,*) ' CoefRz  DrZ  PowerRz  ', CoefRz, Drz, PowerRz
         write(idebug,*) ' CoefGwl Dgwl PowerGwl ', CoefGwl, Dgwl, PowerGwl
      Endif
      If (Abs(Drz)  .gt. 0.0000001) Distance(2) = Distance(2) + abs (CoefRz  * (Abs(Drz) ** PowerRz))
      If (Abs(Dgwl) .gt. 0.0000001) Distance(2) = Distance(2) + abs (CoefGwl * (Abs(Dgwl) ** PowerGwl))

      Drz  = ( DpRootz - DpRzUn(ixrz1) ) / 100.
      Dgwl =   DpIn - DpGwUn(isoil,ixrz1,ixdpun2)
!     Distance(3) =  abs (CoefRz * (Drz ** PowerRz)) + abs (CoefGwl * (Dgwl ** PowerGwl))
!     ARS 5981: foutieve index 1 ipv 3
!     Distance(1) = 0.0
      Distance(3) = 0.0
      If (Abs(Drz)  .gt. 0.0000001) Distance(3) = Distance(3) + abs (CoefRz  * (Abs(Drz) ** PowerRz))
      If (Abs(Dgwl) .gt. 0.0000001) Distance(3) = Distance(3) + abs (CoefGwl * (Abs(Dgwl) ** PowerGwl))

      Drz  = ( DpRootz - DpRzUn(ixrz1) ) / 100.
      Dgwl =   DpIn - DpGwUn(isoil,ixrz1,ixdpun3)
!     Distance(4) =  abs (CoefRz * (Drz ** PowerRz)) + abs (CoefGwl * (Dgwl ** PowerGwl))
!     ARS 5981: foutieve index 1 ipv 4
!     Distance(1) = 0.0
      Distance(4) = 0.0
      If (Abs(Drz)  .gt. 0.0000001) Distance(4) = Distance(4) + abs (CoefRz  * (Abs(Drz) ** PowerRz))
      If (Abs(Dgwl) .gt. 0.0000001) Distance(4) = Distance(4) + abs (CoefGwl * (Abs(Dgwl) ** PowerGwl))

      TotalDistance = 0.
      Do i=1,4
         TotalDistance = TotalDistance + Distance(i)
      Enddo

      TotalGewicht  = 0.
      Do i=1,4
        Gewicht(i) = 0
        if (Distance(i) .gt. 0) Gewicht(i)   = TotalDistance / Distance(i)
        TotalGewicht = TotalGewicht + Gewicht(i)
      Enddo

      NewStorageCoefficient = 0.
      Do i=1,4
          NewStorageCoefficient = NewStorageCoefficient + Gewicht(i) * mu(i)
      Enddo
      if (TotalGewicht .gt. 0) then
         NewStorageCoefficient = NewStorageCoefficient / TotalGewicht
      else
! blijkbaar zijn alle afstanden en gewichten nul, dus neem de 'gewone' gemiddelde mu.
         NewStorageCoefficient = (mu(1) + mu(2) + mu(3) + mu(4)) / 4.
      endif

!Overrulen van interpolatie als een van de punten exact klopt (distance=0)
!let weer op geval dat alle afstanden nul zijn: dan gewone gemiddelde nemen
      TempStorageCoefficient = 0.0
      itemp = 0
      Do i=1,4
         If (Distance(i) .le. 0.00000001) then
           TempStorageCoefficient = TempStorageCoefficient + Mu(i)
           itemp = itemp + 1
         Endif
      Enddo
      if (itemp .gt. 0)  NewStorageCoefficient = TempStorageCoefficient / itemp
      if (TotalGewicht .le. 0)  NewStorageCoefficient = (mu(1) + mu(2) + mu(3) + mu(4)) / 4.

! Overrulen als de NewStorageCoefficient nul is
      NewStorageCoefficient = max (NewStorageCoefficient, 0.01d0)

      if (idebug .ne. 0) then
         write(idebug,*) ' DpRootZ incm  DpIn in m ', DpRootZ, DpIn
         write(idebug,*) ' ixrz  ixrz1  ', ixrz, ixrz1
         write(idebug,*) ' ixdpun  ixdpun1 ', ixdpun , ixdpun1
         write(idebug,*) ' ixdpun2 ixdpun3 ', ixdpun2, ixdpun3
         write(idebug,*) ' mu values ', (mu(i),i=1,4)
         write(idebug,*) ' Distances ', (Distance(i),i=1,4)
         write(idebug,*) ' Gewichten ', (Gewicht (i),i=1,4)
         write(idebug,*) ' NewStorageCoefficient ', NewStorageCoefficient
      endif

      Result = NewStorageCoefficient

 999  Continue


    RETURN
  END subroutine Capsim2StorageCoefficient







  Subroutine RdBerg
    ! *********************************************************************
    ! *** Last update: March  1995       By : Geert Prinsen
    ! *********************************************************************
    ! *** Brief description:
    ! *** ------------------
    ! ***  Inlezen bergingscoefficient, afh. grondsoort en ontwateringsdiepte
    ! *********************************************************************
    ! *** Input/output parameters:
    ! *** ------------------------
    ! ***  IDEBUG = file unit number of debug file
    ! ***  IN     = file unit number of input file
    ! ***  IOUT1  = file unit number of output file with messages
    ! *********************************************************************

    Integer :: RetVal

    INTEGER fileHandle, iVal, i, iECode, iDebug, IOut1
    LOGICAL ENDFIL
! Feb 2002
    Logical       Allow, Found, TabYesNo, success
    Integer       NHLP, teller1, teller2, teller3, len1, len2
    Parameter     (NHLP=625)   ! moet >= NsoilNoCapsim*NSoilDepths zijn
    Integer       IDUM(NHLP)
    Real          RDUM(NHLP)
    Character(Len=CharIdLength) CDUM(NHLP), TableName
    Character(Len=9999) BufString
!    Integer       Allocation_error



    iDebug = ConfFil_get_iDebug()
    iOut1  = ConfFil_get_iOut1()
    if (idebug .ne. 0) WRITE (IDEBUG,1)
  1 FORMAT (' RdBerg')


    if (NewFormatSoilData) then
       call OpenFl(fileHandle, ConfFil_get_namFil(113),1,1)
    else
       call OpenFl(fileHandle, ConfFil_get_namFil(20),1,1)
    endif

    ! *********************************************************************
    ! *** skip header of file
    ! *********************************************************************


    CALL SKPCOM (fileHandle, ENDFIL, 'ODS ')
    IF (ENDFIL) call ErrMsgStandard (911,IECODE,'RdBerg', ' Soil data file')

    ! *********************************************************************
    ! *** read data: maximaal NVAL
    ! *********************************************************************

     If (.not. NewFormatSoilData) then
         !values for grondsoorten
         NSoilNoCapsim = NVal2
         NSoilCapsim   = 21
         NSoilDepths   = NVal2
         success = DH_Allocinit (NVal2, NVal2, BergTb, 0D0)
         success = success .and. DH_allocinit (NVal2, OwDept, 0D0)
         success = success .and. DH_allocinit (NVal2, Ground, SoilCnv, 0)
         success = success .and. DH_allocinit (21, SoilCnv2, 0)
         if (.not. success) call ErrMsgStandard (981, 0, ' Error allocating arrays in subroutine ',' Unpaved_RdBerg')

!        ALLOCATE ( BERGTB(NVAL2,NVAL2), OWDEPT(NVAL2), GROUND(NVAL2), &
!                   SoilCnv(NVal2), SoilCnv2(21), Stat=Allocation_Error )
!        If (Allocation_Error .ne. 0) &
!           call ErrMsgStandard (981, Allocation_Error, ' Error allocating arrays in subroutine ', ' RdBerg' )

         DO I=1,NVAL2
            READ(fileHandle,*,END=30,ERR=150,IOSTAT=IECODE) GROUND(I)
         ENDDO

         CALL SKPCOM(fileHandle,ENDFIL, 'ODS ')
         !values for ontwateringsdiepte
         READ(fileHandle,*,END=30,ERR=150,IOSTAT=IECODE) (OWDEPT(I),I=1,NVAL2)
         CALL SKPCOM(fileHandle,ENDFIL, 'ODS ')

         ! inlezen tabel
         ! per regel de waarde voor alle grondsoorten voor een ontw.diepte
         DO IVAL=1,NVAL2
           READ(fileHandle,*,END=30,ERR=150,IOSTAT=IECODE)  (BERGTB(IVAL,I),I=1,NVAL2)
           CALL SKPCOM (filehandle, ENDFIL, 'ODS ')
         EndDo
         GOTO 30
     Else
         Allow = .false.
         found = .false.
         Retval= 0
         Do while (.not. endfil)
            Success = GetRecord (FileHandle,'SLDF', Endfil, Idebug, Iout1)  ! get record van keyword SLDF until sldf
            IF (.not. success) goto 30
            If (Endfil) goto 30
            success = GetStringFromBuffer(BufString)
            IF (.not. success) GOTO 30
            if (idebug .ne. 0) Write(idebug,*) ' Read buffer: ', BufString(1:100)
            Success = GetTableName (TabYesNo, TableName, ' id ', IOut1)
            IF (.not. success) GOTO 150
            if (idebug .ne. 0) Write(idebug,*) ' TableName got from buffer: ', TableName(1:Len_trim(TableName))
            Call UpperC (TableName)   ! Id put in UPPERCASE, since all data from INI file also converted to UPPERCASE
            Len1 = Len_trim(TableName)
            Len2 = Len_trim(SoilDefinition)
            if (idebug .ne. 0) Write(idebug,*) ' TableName length and SoilDefinition length', len1, len2
            If (TableName(1:Len1) .eq. SoilDefinition(1:Len2)) then
! specified Soil definition (e.g. 'Default', 'Parbo', or 'Taiwan') found
              if (idebug .ne. 0) Write(idebug,*) ' TableName = SoilDefinition found!: ', &
                                                 SoilDefinition(1:Len_trim(SoilDefinition))
              TabYesNo = .true.
! keyword ns = aantal soiltypes without Capsim
              Retval = Retval + GetVAR2 (BufString(1:nbuf),' ns ',3,' Soil type',' Soil data file',IOUT1, &
                            CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
              NSoilNoCapsim = IDUM(1)
              if (idebug .ne. 0) Write(idebug,*) ' NSoilNoCapsim = ', NSoilNoCapsim
              IF (NSoilNoCapsim .gt. NVal2) &
                  call ErrMsgStandard (912, IECODE, '  RdBerg', ' Number of soiltypes no Capsim')
              Retval = Retval + GetVAR2 (BufString(1:nbuf),' nsc ',3,'  Soil type Capsim',' Soil data file',IOUT1, &
                            CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
              NSoilCapsim = IDUM(1)
              if (idebug .ne. 0) Write(idebug,*) ' NSoilCapsim = ', NSoilCapsim
              IF (NSoilCapsim .gt. NxSpun) &
                  call ErrMsgStandard (912, IECODE, '  RdBerg', ' Number of soiltypes Capsim')
              Retval = Retval + GetVAR2 (BufString(1:nbuf),' nd ',3,' Soil depths',' Soil data file',IOUT1, &
                            CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
              NSoilDepths = IDUM(1)
              if (idebug .ne. 0) Write(idebug,*) ' NSoilDepths = ', NSoilDepths
              IF (NSoilDepths .gt. NVal2) &
                  call ErrMsgStandard (912, IECODE, '  RdBerg', ' Number of soildepths no Capsim')
              SoilDefNoCapsim = ' '
              Success = GetTableName (TabYesNo, SoilDefNoCapsim, ' sn ', IOut1)
              IF (.not. success) GOTO 150
              if (idebug .ne. 0) Write(idebug,*) ' SoilDefNoCapsim ', SoilDefNoCapsim
              SoilDefCapsim = ' '
              Success = GetTableName (TabYesNo, SoilDefCapsim, ' snc ', IOut1)
              IF (.not. success) GOTO 150
              if (idebug .ne. 0) Write(idebug,*) ' SoilDefCapsim ', SoilDefCapsim
              SoilDepthDef = ' '
              Success = GetTableName (TabYesNo, SoilDepthDef, ' sd ', IOut1)
              IF (.not. success) GOTO 150
              if (idebug .ne. 0) Write(idebug,*) ' SoilDepthDef ', SoilDepthDef
              SoilStorageDef = ' '
              Success = GetTableName (TabYesNo, SoilStorageDef, ' st ', IOut1)
              IF (.not. success) GOTO 150
              if (idebug .ne. 0) Write(idebug,*) ' SoilStorageDef ', SoilStorageDef
              SoilConversionDef = ' '
              Success = GetTableName (TabYesNo, SoilConversionDef, ' scv', IOut1)
              IF (.not. success) GOTO 150
              if (idebug .ne. 0) Write(idebug,*) ' SoilConversionDef ', SoilConversionDef
              success = DH_Allocinit (NSoilDepths, NSoilNoCapsim, BergTb, 0D0)
              success = success .and. DH_allocinit (NSoilDepths, OwDept, 0D0)
              success = success .and. DH_allocinit (NSoilDepths, Ground, 0)
              success = success .and. DH_allocinit (NSoilNoCapsim, SoilCnv, 0)
              success = success .and. DH_allocinit (NSoilCapsim, SoilCnv2, 0)
              if (.not. success) call ErrMsgStandard (981, 0, ' Error allocating arrays in subroutine ',' Unpaved_RdBerg')

!              ALLOCATE ( BERGTB(NSoilDepths, NSoilNoCapsim), OWDEPT(NSoilDepths), GROUND(NSoilDepths), &
!                         SoilCnv(NSoilNoCapsim), SoilCnv2(NSoilCapsim), Stat=Allocation_Error )
!              If (Allocation_Error .ne. 0) &
!                 call ErrMsgStandard (981, Allocation_Error, ' Error allocating arrays in subroutine ', ' RdBerg' )
              Do teller1=1,NSoilNoCapsim
                 Ground(teller1) = teller1
              Enddo
              If (RetVal .gt. 0)  call ErrMsgStandard (972, 0, ' Error reading new soil definition data',' Getting SLDF record')
! get soil name definitions: not used
! get soil depths definitions
              Retval = 0
              Do While (.not. endfil)
                Success = GetRecord (FileHandle,'SDEF', Endfil, Idebug, Iout1)  ! get record van keyword SDEF tot sdef
                IF (.not. success) GOTO 30
                if (idebug .ne. 0) Write(idebug,*) ' Read buffer: ', BufString(1:100)
                Success = GetTableName (TabYesNo, TableName, ' id ', IOut1)
                IF (.not. success) GOTO 150
                if (idebug .ne. 0) Write(idebug,*) ' TableName got from buffer: ',&
                                                    TableName(1:Len_trim(TableName))
                if (idebug .ne. 0) Write(idebug,*) ' Test on SoilDepthDef : ', SoilDepthDef(1:100)
                Len1 = Len_trim(TableName)
                Len2 = Len_trim(SoilDepthDef)
                if (idebug .ne. 0) Write(idebug,*) ' TableName length and SoilDepthDef length', len1, len2
                If (TableName(1:Len1) .eq. SoilDepthDef(1:Len2)) then
                   if (idebug .ne. 0) Write(idebug,*) ' TableName = SoilDepthDef found!: ',&
                                      SoilDepthDef(1:Len_trim(SoilDepthDef))
                   Retval = Retval + GetVRS2 (BufString(1:nbuf),' dp ',2,' RdBerg ',' Soil data file', &
                                 IOUT1, CDUM(1), RDUM(1), IDUM(1), NSoilNoCapsim, IflRtn)
                   Do teller1 = 1, NSoilDepths
                      OwDept (teller1) = RDUM(teller1)
                   Enddo
                   TabYesNo = .true.
                   GOTO 991
                Endif
              Enddo
  991         Continue
              If (RetVal .gt. 0) call ErrMsgStandard (972, 0, ' Error reading new soil depths definition data' ,' Getting SDEF record')
              Rewind(FileHandle)
              Retval = 0
              Do While (.not. endfil)
                Success = GetRecord (FileHandle,'STAB', Endfil, Idebug, Iout1)  ! get record van keyword STAB tot Stab
                IF (.not. success) GOTO 30
                if (idebug .ne. 0) Write(idebug,*) ' Read buffer: ', BufString(1:100)
                Success = GetTableName (TabYesNo, TableName, ' id ', IOut1)
                IF (.not. success) GOTO 150
                if (idebug .ne. 0) Write(idebug,*) ' TableName got from buffer: ',&
                                                     TableName(1:Len_trim(TableName))
                if (idebug .ne. 0) Write(idebug,*) ' Test on SoilStorageDef : ', SoilStorageDef(1:100)
                Len1 = Len_trim(TableName)
                Len2 = Len_trim(SoilStorageDef)
                if (idebug .ne. 0) Write(idebug,*) ' TableName length and SoilStorageDef length', len1, len2
                If (TableName(1:Len1) .eq. SoilStorageDef(1:Len2)) then
                   if (idebug .ne. 0) Write(idebug,*) ' TableName = SoilStorageDef found!: ',&
                                      SoilStorageDef(1:Len_trim(SoilStorageDef))
                   teller3 = NSoilNoCapsim*NSoilDepths
                   if (idebug .ne. 0) Write(idebug,*) ' aantal values gezocht = ', NSoilNoCapsim*NSoilDepths
                   Retval = Retval + GetVRS2 (BufString(1:nbuf),' sc ',2,' RdBerg ',' Soil data file', &
                                 IOUT1, CDUM(1), RDUM(1), IDUM(1), teller3, IflRtn)
                   teller3 = 0
                   Do teller1 = 1, NSoilDepths
                     Do teller2 = 1, NSoilNoCapsim
                        teller3 = teller3 + 1
                        BERGTB (teller1,teller2) = Rdum(teller3)
                     Enddo
                   Enddo
                   TabYesNo = .true.
                   GOTO 992
                Endif
              Enddo
  992         Continue
              If (RetVal .gt. 0)  call ErrMsgStandard (972, 0, ' Error reading new soil depths definition data',' Getting STAB record')
              Rewind(FileHandle)
              Retval = 0
              Do While (.not. endfil)
                Success = GetRecord (FileHandle,'SCNV', Endfil, Idebug, Iout1)  ! get record van keyword SCNV tot scnv
                IF (.not. success) GOTO 30
                if (idebug .ne. 0) Write(idebug,*) ' Read buffer: ', BufString(1:100)
                Success = GetTableName (TabYesNo, TableName, ' id ', IOut1)
                IF (.not. success) GOTO 150
                if (idebug .ne. 0) Write(idebug,*) ' TableName got from buffer: ',&
                                                     TableName(1:Len_trim(TableName))
                if (idebug .ne. 0) Write(idebug,*) ' Test on SoilConversionDef : ', SoilConversionDef(1:100)
                Len1 = Len_trim(TableName)
                Len2 = Len_trim(SoilConversionDef)
                if (idebug .ne. 0) Write(idebug,*) ' TableName length and SoilConversionDef length', len1, len2
                If (TableName(1:Len1) .eq. SoilConversionDef(1:Len2)) then
                   if (idebug .ne. 0) Write(idebug,*) ' TableName = SoilConversionDef found!: ', &
                                      SoilConversionDef(1:Len_trim(SoilConversionDef))
                   Retval = Retval + GetVRS2 (BufString(1:nbuf),' c1 ',3,' RdBerg ',' Soil data file', &
                                 IOUT1, CDUM(1), RDUM(1), IDUM(1), NSoilNoCapsim, IflRtn)
                   Do teller1 = 1, NSoilNoCapsim
                      SoilCnv (teller1) = Idum(teller1)
                   Enddo
                   Retval = Retval + GetVRS2 (BufString(1:nbuf),' c2 ',3,' RdBerg ',' Soil data file', &
                                 IOUT1, CDUM(1), RDUM(1), IDUM(1), NSoilCapsim, IflRtn)
                   Do teller1 = 1, NSoilCapsim
                      SoilCnv2 (teller1) = Idum(teller1)
                   Enddo
                   TabYesNo = .true.
                   GOTO 999
                Endif
              Enddo
              If (RetVal .gt. 0)  call ErrMsgStandard (972, 0, ' Error reading new soil depths definition data',' Getting SNCV record')
           Else
              TabYesNo = .false.
           Endif
        Enddo
        If (.not. TabYesNo) GoTo 30
     Endif

    ! *********************************************************************
    ! *** Error during reading of file
    ! *********************************************************************

150 CONTINUE
    call ErrMsgStandard (902, IECODE, 'RdBerg', ' Soil data file')

    ! *********************************************************************
    ! *** end of file
    ! *********************************************************************

 30 CONTINUE
999 CONTINUE
    Call CloseGP (fileHandle)

    If (NewFormatSoilData .and. UnSatZoneOption .ge. 1 .and. NSoilCapsim .ne. 21) then
        call SetMessage(LEVEL_WARN, 'Capsim used with non-default number of soils')
        call SetMessage(LEVEL_WARN, 'This only works if you supplied your own Capsim input files !!!')
    Endif

    if (idebug .ne. 0) Then
       WRITE (IDEBUG,*) ' Data storage-coefficient'
       WRITE (IDEBUG,'(A15,6I7)') ' soiltypes ',(GROUND(I),I=1,NVAL2)
       Do IVAL=1,NVAL2
          WRITE(IDEBUG,'(F10.3,4X,6F7.5)') OWDEPT(IVAL),(BERGTB(IVAL,I),I=1,NVAL2)
       EndDo
    Endif

    RETURN
    END subroutine RdBerg



 Subroutine InitCapsim (IdebugCapsimFromTimestep, IdebugCapsimToTimestep)

!*********************************************************************
!***                D E L F T         H Y D R A U L I C S
!***
!***              WATER RESOURCES AND ENVIRONMENT DIVISION
!*********************************************************************
!*** Program :  DELFT-3B version 2.00                Date: Nov   1996
!*** Module  :
!*********************************************************************
!*** Created    : March  1995                     By : Geert Prinsen
!*********************************************************************
!*** Last update: March 2000                       By: Geert Prinsen
!*********************************************************************
!*** Brief description:
!*** ------------------
!***    Initialiseer Capsim: lees Capsim invoer etc.
!*********************************************************************
!*** ADRESS     : DELFT HYDRAULICS,
!***              P.O.BOX 177,
!***              2600 MH DELFT, THE NETHERLANDS
!**********************************************************************


  IMPLICIT NONE

  Integer       IStatus
  Integer       Idebug, i ,j,k
  Integer       IdebugCapsimFromTimestep, IdebugCapsimToTimestep
  Integer       Allocation_Error

!
  Character(132) CapsimFileName


     if (UnsatZoneOption .ge. 1 .and. CapsimPerCropArea .eq. 1) then   !CapsimPerCrop & Capsim or Capsim+
       ALLOCATE ( AreaScurvePerCrop(Novh,NCrop,UseUnpavedScurveMax), Stat=Allocation_Error )
       If (Allocation_Error .ne. 0) &
          call ErrMsgStandard (981, Allocation_Error, ' Error allocating arrays in subroutine ', &
                                              ' Unpaved_InitCapsim'  )
       AreaSCurvePerCrop%Level = 0.
       AreaSCurvePerCrop%Area  = 0.
       AreaSCurvePerCrop%Percentage =0.
       AreaSCurvePerCrop%AlfaLevels(1) =0.
       AreaSCurvePerCrop%AlfaLevels(2) =0.
       AreaSCurvePerCrop%AlfaLevels(3) =0.
       AreaSCurvePerCrop%ActualSurfaceStorage =0.
       AreaSCurvePerCrop%ActualSoilStorage =0.
       AreaSCurvePerCrop%InitialSurfaceStorage =0.
       AreaSCurvePerCrop%InitialSoilStorage =0.
       AreaSCurvePerCrop%SurfaceOutflow =0.
       AreaSCurvePerCrop%SoilOutflow =0.

     endif

     if (UnsatZoneOption .ge. 1) then   !Sobek-Capsim mode or Capsim+
         CapSimMsgFile = ConfFil_get_NAMFIL(95)
         CapSimDbgFile = ConfFil_get_NAMFIL(96)
         Call OpenFl (Message_unit,ConfFil_get_NAMFIL(95),1,2)
         CapsimDebug_unit = debug_unit
         Call OpenFl (Debug_unit,ConfFil_get_NAMFIL(96),1,2)
!         open (Message_unit,file=CapsimMsgFile,status='unknown')
!         open (Debug_unit,file=CapsimDbgFile,status='unknown')
         Call CloseGP (Message_unit)
         Call CloseGP (Debug_unit)
         write(*,*) ' Simulation using Capsim'
!         write(*,*) ' CapsimMsgFile =', CapsimMsgFile(1:132)
!         write(*,*) ' CapsimDbgFile =', CapsimDbgFile(1:132)
         IStatus = 0
         Debug_unit = 0
         if (IdebugCapsimFromTimestep .gt.0 .or. IdebugCapsimToTimestep .gt. 0) then
              debug_unit = CapsimDebug_unit
         endif
!  Rootsim.inp file; use OpenFl (with DioNewLun) to assign a LUN number; ReadRoot will open the file
         CapSimFileName = ConfFil_get_NAMFIL(93)
         Call OpenFl (File_Unit, CapsimFileName, 1,2)
         Call CloseGP(File_Unit)
!  Unsa_sim.inp file; use OpenFl (with DioNewLun) to assign a LUN number; ReadRoot will open the file
         call ReadRoot (File_unit, CapSimFileName, message_unit,CapSimMsgFile, &
                        debug_unit,CapSimDbgFile, IStatus,&
                        nxspun, nxte,  dprz, frev)
         if (IStatus .ne. 0) call ErrMsgStandard (969, 0, ' Sobek_RR', ' Error in Alterra-routine ReadRoot')
         IStatus = 0
         CapSimFileName = ConfFil_get_NAMFIL(94)
         Call OpenFl (File_Unit, CapsimFileName, 1,2)
         Call CloseGP(File_Unit)
         call ReadUnsa (File_unit, CapSimFileName, message_unit,CapSimMsgFile,&
                        Debug_unit, CapSimDbgFile, IStatus,  &
                        nxspun, nxte, nxrz, nxdpun, srrz, fmca, scsa,   &
                        dpgwun, dprzun,  nudpun)
         Debug_unit = 0
         if (IStatus .ne. 0) then
            Write(*,*) ' IStatus returned from ReadUnsa=', IStatus
            call ErrMsgStandard (969, 0, ' Sobek_RR', ' Error in Alterra-routine ReadUnsa')
         endif
!
! Set Actual maxima for Internal use
! NActRzClass = max. aantal root zone klasses
! NActGwlClass = max. aantal gwl klasses
         NActRzClass  = nxrz
         NActGwlClass = nxdpun
         do i=1,nxrz-1
            if (dprzun(i) .gt. dprzun(i+1)) then
                nActRzClass = i
                goto 301
            endif
         enddo
  301    continue
! voor NActGwlClass: slechts voor soiltype 1 en rootzoneklasse 1  bepalen
         do i=1,nxdpun-1
            if (dpgwun(1,1,i) .gt. dpgwun(1,1,i+1)) then
                nActGwlClass = i
                goto 302
            endif
         enddo
  302    continue
! voor NActSoils: slechts voor 1 array bepalen
         do i=1,nxspun
            if (dprz(i,1) .le. 0.000) then
                nActSoils = i-1
                goto 303
            endif
         enddo
  303    continue

!        ARS 8843: Read additional data from UnsaSim.Inp on K-values
!                  used in Capsim+ option
         if (UnsatZoneOption .eq. 2) then
             Call CloseGP(File_Unit)
             Call OpenFl (File_Unit, CapsimFileName, 1,2)
             Call ReadUnsa2 (File_unit, iStatus)
             if (IStatus .ne. 0) then
                Write(*,*) ' IStatus returned from ReadUnsa=', IStatus
                call ErrMsgStandard (969, 0, ' Sobek_RR', ' Error in routine ReadUnsa2')
             endif
         endif


!Debug uitgezet
        idebug = 0 ! idebugLunRR
        if (idebug .ne. 0) then
          write(idebug,*) ' NactRzClass = ', NactRzClass
          write(idebug,*) ' NactGwlClass = ', NactGwlClass
          write(idebug,*) ' NactSoils    = ', NactSoils
          write(idebug,*) ' Overview SC-tables read in 3B'
          write(idebug,*) ' DPRZ-depth Root zone'
          do i=1,nxspun
             write (idebug,'(A,I5,1X,50(G11.4,1X))')  ' i', i, (DPRZ(i,j),j=1,Nxte)
          enddo
          write(idebug,*) ' NUDPUN-groundwater depth data'
          do i=1,nxspun
             write (idebug,'(A,I5,50I5)')  ' i', i, (NUDPUN(i,j),j=1,Nxrz)
          enddo
          write(idebug,*) ' SRRZ-storage rootzone'
          do i=1,nxspun
             do j=1,nxrz
                write (idebug,'(A,2I5,1X,50(G11.4,1X))')  ' i j ', i,j, (SRRZ(i,j,k),k=1,Nxdpun)
             enddo
          enddo
          write(idebug,*) ' FMCA-cap.rise table'
          do i=1,nxspun
             do j=1,nxrz
                write (idebug,'(A,2I5,1X,50(G11.4,1X))')  ' i j', i,j, (FMCA(i,j,k),k=1,Nxdpun)
             enddo
          enddo
          write(idebug,*) ' SCSA-storage coeff.'
          do i=1,nxspun
             do j=1,nxrz
                write (idebug,'(A,2I5,1X,50(G11.4,1X))')  ' i j', i,j, (SCSA(i,j,k),k=1,Nxdpun)
             enddo
          enddo
          write(idebug,*) ' DPRZUN-depth root zone'
          write (idebug,'(50G12.5)')  (DPRZUN(i),i=1,Nxrz)
          write(idebug,*) ' DPGWUN-gw-depth table'
          do i=1,nxspun
             do j=1,nxrz
                write (idebug,'(A,2I5,1X,50(G11.4,1X))')  ' i j', i,j, (DPGWUN(i,j,k),k=1,Nxdpun)
             enddo
          enddo
          write(idebug,*) ' DPFRSW-gw-depth inundation table'
          write (idebug,'(50(G12.5,1X))')  (DPFRSW(i),i=1,Nxfrsw)
          write(idebug,*) ' FREV-evapotranspiration curves'
          do i=1,nxspun
             do j=1,nxte
                write (idebug,'(A,2I5,1X,5(G11.4,1X))')  ' i j', i,j, (FREV(i,j,k),k=1,5)
             enddo
          enddo
          write(idebug,*) ' Kdoorlatendheid'
          do i=1,nxspun
             do j=1,nxrz
                write (idebug,'(A,2I5,1X,50(G11.4,1X))')  ' i j', i,j, (Kdoorlatendheid(i,j,k),k=1,Nxdpun)
             enddo
          enddo
          write(idebug,*) ' Alfafactor'
          do i=1,nxspun
             do j=1,nxrz
                write (idebug,'(A,2I5,1X,50(G11.4,1X))')  ' i j', i,j, (AlfaFactor(i,j,k),k=1,Nxdpun)
             enddo
          enddo
       Endif

      Endif

      Return
   End Subroutine InitCapsim


    Subroutine ReadUnsa2 (File_unit, IStatus)

    Integer    IStatus
    Integer File_Unit, Iout1, icrop
    Integer i,j,k,idum
    Double precision    rdum

    Iout1 = Conffil_Get_Iout1()
    IStatus = 0

   ! Read KDoorlatendheid, AlfaFactor
    Do i=1,NActSoils
       Do j=1,NActRZClass
          Do k=1,NActGwlClass
             Read (File_unit,*,err=999,end=999) idum, rdum, rdum, rdum, rdum, rdum, Kdoorlatendheid (i,j,k), AlfaFactor(i,j,k)
          EndDo
       EndDo
    EndDo
    KDoorlatendheid = KDoorlatendheid * 10.   ! Input in cm/day, in datastructure in mm/day

   ! Bepaal VRZ_h1, h2, h3_low, h3_high, H4
    Do j=1,NActRZClass
       Do i=1,NActSoils
          Do icrop=1,NCrop
             VRZH1 (i,j,icrop)      = SRRZ(i,j,1) * FREV(i,icrop,1)
             VRZH2 (i,j,icrop)      = SRRZ(i,j,1) * FREV(i,icrop,2)
             VRZH3_High (i,j,icrop) = SRRZ(i,j,1) * FREV(i,icrop,3)
             VRZH3_Low (i,j,icrop)  = SRRZ(i,j,1) * FREV(i,icrop,4)
             VRZH4 (i,j,icrop)      = SRRZ(i,j,1) * FREV(i,icrop,5)
          Enddo
       Enddo
    Enddo

    ! and put in mm
    VRZH1 = VRZH1 * 1000.
    VRZH2 = VRZH2 * 1000.
    VRZH3_Low = VRZH3_Low * 1000.
    VRZH3_High= VRZH3_High * 1000.
    VRZH4 = VRZH4 * 1000.

    Return

999 Continue
    call SetMessage(LEVEL_FATAL, 'Unexpected end-of-file or read error in UNSA_Sim.INP when using Capsim+')
    IStatus = 999

    Return
    End Subroutine ReadUnsa2



    Subroutine ComputeUnsatZoneSimpleNoCapsim (iovh, Percolation, CapillaryRise)

! *********************************************************************
! Subroutine berekent onverzadigde zone voor onverhard gebied met index Iovh
!   simpele methode: nl. geen berging in onverzadigde zone.
!   Infiltratie gaat gelijk door naar grondwater
! *********************************************************************
!   iovh   = arrayindex in arrays voor onverhard gebied
!   idebug = debug file
! *********************************************************************

   Integer Iovh
   Double precision    Percolation, CapillaryRise

! Geen Capsim; zet onverzadigde zone tussen Min en Max;
! Geen onderscheid naar Scurve gebruik of niet

!      Voorlopig volume, zonder CapRise/Percolation
       OnvZone(IOVH)%Actual_Volume = OnvZone(IOVH)%Init_Volume + INO(IOVH) - VBO(IOVH)
       OnvZone(IOVH)%Actual_mm     = OnvZone(IOVH)%Actual_Volume / AREAOH(IOVH) / mm2m
!      Check minimum and maximum
       If ( OnvZone(IOVH)%Actual_mm .gt. OnvZone(IOVH)%Max_mm) then
          CapillaryRise = 0.0
          Percolation =  OnvZone(IOVH)%Actual_Volume - OnvZone(IOVH)%Max_Volume
          OnvZone(IOVH)%Actual_Volume =  OnvZone(IOVH)%Max_Volume
          OnvZone(IOVH)%Actual_mm =  OnvZone(IOVH)%Max_mm
       elseif ( OnvZone(IOVH)%Actual_mm .lt. OnvZone(IOVH)%Min_mm ) then
          Percolation = 0.0
          CapillaryRise =  OnvZone(IOVH)%Min_Volume - OnvZone(IOVH)%Actual_Volume
          OnvZone(IOVH)%Actual_Volume = OnvZone(IOVH)%Min_Volume
          OnvZone(IOVH)%Actual_mm = OnvZone(IOVH)%Min_mm
       endif
!      Bepaal netto percolation QINB in m3
       QINB(IOVH) = (Percolation - CapillaryRise)

   Return
   End subroutine ComputeUnsatZoneSimpleNoCapsim


   Subroutine ComputeCapsim (iovh, Idebug, NodeName)

! *********************************************************************
! Subroutine berekent onverzadigde zone voor onverhard gebied met index Iovh
! met de Capsim methode van Staring Centrum
!
! Houdt rekening met evt. S curve voor het oppervlak
!  In geval van S curve wordt met 1 grondwaterstand in NAP gerekend, en met 1 bergingscoefficient
!  Wel wordt de onverzadigde zone per deelgebiedje bijgehouden qua volume, en verschillende capillaire opstijging, verdamping etc.
!  Grondwaterstand volgt dan later uit totale balans.
!
! *********************************************************************
!   iovh   = arrayindex in arrays voor onverhard gebied
!   idebug = debug file
! *********************************************************************

   Integer Iovh, idebug
   Character(Len=CharIdLength) NodeName

! S curve. crop
   Integer Ipoint, icrop, IRootZ

!Capsim local Variables
    Integer Nt, Ns, IStatus
    Double precision    DpIn, DpRootz, Dt, Pn, VmRzIn, FmEvPt, FmEvAc, FmPe, VmRzAc, PercentArea
    Double precision    UnSatMax, FmPeMax
!End Capsim

    ReduceSurfaceInfiltration = 0.0

     ! Eerst algemene initialisaties Capsim, onafhankelijk van Scurve gebruik
     ! Set crop index, soil index, Rootzone depth in m
     ! Timestep size Dt in days
     ! Net precipitation Pn in m/d  (== use computed infiltration in m/d)
     ! Potential evapotranspiration FmEvPt in m/d
     Nt      = CapsimCrop(iovh)
     Ns      = BotTyp (iovh)
     DpRootz = CapsimDpRootz(iovh)
     Dt      = Float (TimeSettings%timestepSize)  / Float (NRSDay)
     Pn      = INO(iovh) / AreaOh(iovh) / Dt
     FmEvPt  = VBO(iovh) / AreaOh(iovh) / Dt
     OnvZone(iovh)%Actual_Volume = 0.0

     If (UseSCurve(iovh) .eq. 1) then
       Do IPoint=1,UseUnpavedSCurve
         AreaScurve(iovh,ipoint)%ActualSoilStorage = &
              AreaScurve(iovh,ipoint)%InitialSoilStorage &
                + INO(iovh) * AreaScurve(iovh,ipoint)%Area / AreaOh(iovh)
         If (CapsimPerCropArea .eq. 1) then
           Do ICrop=1,NCrop
              PercentArea = AreaGw(Iovh,Icrop) / AreaOh(iovh)
              If (PercentArea .gt. 0.0) then
                AreaScurvePerCrop(iovh,icrop,ipoint)%ActualSoilStorage = &
                  AreaScurvePerCrop(iovh,icrop,ipoint)%InitialSoilStorage &
                   + INO(iovh) * AreaScurve(iovh,ipoint)%Area / AreaOh(iovh) * PercentArea
              Endif
           Enddo
         Endif
       Enddo
     Endif

     If (CapsimPerCropArea .eq. 0) then
! Compute Capsim once per unpaved area, using average rootzone depth and main crop

      If (UseScurve(iovh) .eq. 0) then  ! geen Scurve gebruiken
         IStatus = 0
       ! Set initial groundwater level
         DpIn   = LVLOH(iovh) - GWL0 (iovh)
       ! Initial unsat. soil moisture content in m
         VmRzIn = OnvZone(Iovh)%Init_mm * mm2m

       ! max. root zone content and max. percolation
         IRootz = Int( DpRootz * 10)   ! assuming root zone depths in table 10, 20, 30, 40, 50, 60, 70, 80, 90 with index 1,2,3 etc.
         UnsatMax = SRRZ (Ns, Irootz, 1) * 1000.   ! maximum root zone content unsat zone (mm)
         FmPeMax  = -1. * KSatCapsim(Ns) / 1000.  ! maximum capris/perc in m/day   NB FmPeMax is negatief (percolatie)
         if (idebug .ne. 0) then
             write (Idebug,*) ' Irootz   ', Irootz
             write (Idebug,*) ' isoil    ', Ns
             write (Idebug,*) ' UnsatMax ', UnsatMax
             write (Idebug,*) ' Ksat     ', KsatCapsim(Ns)
             write (Idebug,*) ' FmPeMax  ', FmPeMax
             write (Idebug,*) ' VmRzIn   ', VmRzIn
             write (Idebug,*) ' Pn       ', Pn
         endif

       ! call Sobek-Capsim-Simgro routine
         if (Idebug .gt.0) call DebugSimgro (Idebug, Nt, Ns, DpIn, DpRootZ, Dt, Pn, VmRzIn, FmEvPt)
         call SIMGRO_OVZ(Debug_unit, CapsimDbgFile, Message_Unit, CapsimMsgFile, IStatus, &
                         NXSPUN, NXTE, NXRZ, NXDPUN, NXFRSW, &
                         Ns, Nt, DpIn, DpRootz, Dt, Pn, VmRzIn, FmEvPt, &
                         SRRZ, FMCA, SCSA, &
                         DPRZUN, DPGWUN, NUDPUN, DPFRSW, FRSW, FREV, &
                         FmEvAc, FmPe, VmRzAc)
         Call CheckSimgro (Istatus, VmRzAc, Iovh, NodeName)
         if (idebug .ne. 0) then
             write (Idebug,*) ' Output from Sobek-Capsim with:'
             write(idebug,*) ' Evapotranspiration ', FmEvAc, ' m/d'
             write(idebug,*) ' Actual volume      ', VmRzAc, ' m'
             write(idebug,*) ' CapRise-Percolation', FmPe, ' m/d'
         endif
       ! store Capsim output in Sobek-RR variables
       ! From Capsim:  actual evap FmEvAc in m/d,
       !               actual capRise-percolation FmPe in m/d, (+=capRise)
       !               actual storage in rootzone VmRzAc in m.
       ! Convert everything to volumes in m3.

       ! actual evapotranspiration from unsaturated zone
         VBO(iovh) = FmEvAc * Areaoh (iovh) * Dt
       ! net percolation
         if (.not. UnpavedPercolationLikeSobek213) then
            if (FmPe .lt. 0) then
               ! limit rate of change of VmRz by limiting FmPe
               FmPe = max (FmPe, UserFactVRZ * (VmRzAc - VmRzIn) / Dt )
            endif

            if (FmPe .lt. 0) then
                FmPe = max (FmPe, FmPeMax)
                if (idebug .ne. 0) write(idebug,*) ' CapRise-Percolation adjusted using Ksat =', FmPe, ' m/d'
            endif
         endif
         QINB(IOVH) = -1. * FmPe * AREAOH(iovh) * Dt
       ! Rootzone Storage
         OnvZone(iovh)%Actual_Volume = VmRzAc * AreaOh(iovh)
         if (idebug .ne. 0) write(idebug,*) ' Actual Vol OnvZone  via VmRzAc', OnvZone(iovh)%Actual_Volume
         OnvZone(iovh)%Actual_Volume = OnvZone(iovh)%Init_Volume + INO(iovh) - VBO(iovh) - QINB(iovh)
         OnvZone(Iovh)%Actual_mm     = OnvZone(Iovh)%Actual_Volume / AreaOh(Iovh) / mm2m
         if (idebug .ne. 0) write(idebug,*) ' Actual Vol OnvZone  via balans incl. FmPe', OnvZone(iovh)%Actual_Volume
         if (.not. UnpavedPercolationLikeSobek213) then
            ReduceSurfaceInfiltration = 0.0
            if (UnsatMax .le. OnvZone(iovh)%Actual_mm) then
                if (idebug .ne. 0) write(idebug,*) ' Actual mm before correction', OnvZone(iovh)%Actual_mm
                ReduceSurfaceInfiltration = OnvZone(iovh)%Actual_mm - UnsatMax  ! in mm
                OnvZone(Iovh)%Actual_mm = UnsatMax
                OnvZone(Iovh)%Actual_Volume = UnsatMax * mm2m * AreaOh(iovh)
                if (idebug .ne. 0) write(idebug,*) ' Adjusted Actual Vol OnvZone  via balans incl. UnsatMax', OnvZone(iovh)%Actual_Volume
                if (idebug .ne. 0) write(idebug,*) ' Actual mm after correction', OnvZone(iovh)%Actual_mm
            endif
            ReduceSurfaceInfiltration = ReduceSurfaceInfiltration * AreaOh(Iovh)* mm2m  ! in m3
            if (idebug .ne. 0) write(idebug,*) ' ReduceSurfaceInfiltration', ReduceSurfaceInfiltration
         endif

      Elseif (UseSCurve(iovh) .eq. 1) then
        ! Capsim, met gebruik S curve voor het oppervlak
         VBo (iovh) = 0.0
         QinB(iovh) = 0.0
         ! max. root zone content and max. percolation
         IRootz = Int( DpRootz * 10)   ! assuming root zone depths in table 10, 20, 30, 40, 50, 60, 70, 80, 90 with index 1,2,3 etc.
         UnsatMax = SRRZ (Ns, Irootz, 1) * 1000.   ! maximum root zone content unsat zone (mm)
         FmPeMax  = -1. * KSatCapsim(Ns) / 1000.  ! maximum capris/perc in m/day   NB FmPeMax is negatief (percolatie)
         if (idebug .ne. 0) then
!            write (Idebug,*) ' Ipoint Scurve', Ipoint
             write (Idebug,*) ' Irootz   ', Irootz
             write (Idebug,*) ' isoil    ', Ns
             write (Idebug,*) ' UnsatMax ', UnsatMax
             write (Idebug,*) ' Ksat     ', KsatCapsim(Ns)
             write (Idebug,*) ' FmPeMax  ', FmPeMax
         endif

         Do IPoint=1,UseUnpavedSCurve
            IStatus = 0
            ! Depth of groundwater layer; 1 fixed groundwaterlevel, but different surface levels
            DpIn   = max (0.0d0, AreaScurve(iovh,ipoint)%Level - GWL0 (iovh) )
            ! Initial unsat. soil moisture content in m
            VmRzIn = AreaScurve(iovh,ipoint)%InitialSoilStorage / AreaScurve(iovh,ipoint)%Area

            ! call Sobek-Capsim-Simgro routine
            if (Idebug .gt.0) call DebugSimgro (Idebug, Nt, Ns, DpIn, DpRootZ, Dt, Pn, VmRzIn, FmEvPt)
            call SIMGRO_OVZ(Debug_unit, CapsimDbgFile, Message_Unit, CapsimMsgFile, IStatus, &
                         NXSPUN, NXTE, NXRZ, NXDPUN, NXFRSW, &
                         Ns, Nt, DpIn, DpRootz, Dt, Pn, VmRzIn, FmEvPt, &
                         SRRZ, FMCA, SCSA, &
                         DPRZUN, DPGWUN, NUDPUN, DPFRSW, FRSW, FREV, &
                         FmEvAc, FmPe, VmRzAc)
            Call CheckSimgro (Istatus, VmRzAc, Iovh, NodeName)
            if (idebug .ne. 0) then
               write (Idebug,*) ' Output from Sobek-Capsim Scurve zone with:'
               write(idebug,*) ' Evapotranspiration ', FmEvAc, ' m/d'
               write(idebug,*) ' Actual volume      ', VmRzAc, ' m'
               write(idebug,*) ' CapRise-Percolation', FmPe, ' m/d'
            endif
          ! store Capsim output in Sobek-RR variables
          ! Convert everything to volumes in m3.

          ! actual evapotranspiration from unsaturated zone
            VBO(iovh) = VBO(iovh) + FmEvAc * AreaScurve(iovh,ipoint)%Area * Dt
          ! net percolation
            if (.not. UnpavedPercolationLikeSobek213) then
               if (FmPe .lt. 0) then
                  ! limit rate of change of VmRz by limiting FmPe
                  FmPe = max (FmPe, UserFactVRZ * (VmRzAc - VmRzIn) / Dt )
               endif
               if (FmPe .lt. 0) then
                   FmPe = max (FmPe, FmPeMax)
                   if (idebug .ne. 0) write(idebug,*) ' CapRise-Percolation adjusted using Ksat =', FmPe, ' m/d'
               endif
            endif
            QINB(IOVH) = QINB(iovh)  -1. * FmPe * AreaScurve(iovh,ipoint)%Area * Dt
            if (idebug .ne. 0) then
              write (Idebug,*) ' Output in loop in RR terms:'
              write(idebug,*) ' VBO  -verdamping bodem ', VBO(iovh)
              write(idebug,*) ' QINB Percolation-CapRise', QINB(iovh)
            endif
            AreaScurve(iovh,ipoint)%ActualSoilStorage = &
                AreaScurve(iovh,ipoint)%InitialSoilStorage &
                   + INO(iovh) * AreaScurve(iovh,ipoint)%Area / AreaOh(iovh) &
                     - FmEvAc * AreaScurve(iovh,ipoint)%Area * Dt &
                       + FmPe * AreaScurve(iovh,ipoint)%Area * Dt
         enddo
         ! Rootzone Storage
         OnvZone(iovh)%Actual_Volume = OnvZone(iovh)%Init_Volume + INO(iovh) - VBO(iovh) - QINB(iovh)
         OnvZone(Iovh)%Actual_mm     = OnvZone(Iovh)%Actual_Volume / AreaOh(Iovh) / mm2m
         if (.not. UnpavedPercolationLikeSobek213) then
            ReduceSurfaceInfiltration = 0.0
            if (UnsatMax .le. OnvZone(iovh)%Actual_mm) then
                if (idebug .ne. 0) write(idebug,*) ' Actual mm before correction', OnvZone(iovh)%Actual_mm
                ReduceSurfaceInfiltration = OnvZone(iovh)%Actual_mm - UnsatMax  ! in mm
                OnvZone(Iovh)%Actual_mm = UnsatMax
                OnvZone(Iovh)%Actual_Volume = UnsatMax * mm2m * AreaOh(iovh)
                if (idebug .ne. 0) write(idebug,*) ' Adjusted Actual Vol OnvZone  via balans incl. UnsatMax', OnvZone(iovh)%Actual_Volume
                if (idebug .ne. 0) write(idebug,*) ' Actual mm after correction', OnvZone(iovh)%Actual_mm
            endif
            ReduceSurfaceInfiltration = ReduceSurfaceInfiltration * AreaOh(Iovh)* mm2m  ! in m3
            if (idebug .ne. 0) write(idebug,*) ' ReduceSurfaceInfiltration', ReduceSurfaceInfiltration
         endif
      endif

    Else

! May 2001
! Compute Capsim for all crops per unpaved area, using crop rootzone depth
     VBo (iovh) = 0.0
     QinB(iovh) = 0.0
     Do Icrop=1,NCrop
      If (AreaGw(Iovh,icrop) .gt. 0) then
        PercentArea = AreaGw(Iovh,Icrop) / AreaOh(iovh)
      ! Set crop index, soil index, Rootzone depth in m
      ! Timestep size Dt in days
      ! Net precipitation Pn in m/d  (== use computed infiltration in m/d)
      ! Potential evapotranspiration FmEvPt in m/d
        Nt      = ICrop
        Ns      = BotTyp (iovh)
        DpRootz = DpRz(Ns,icrop)  ! er stond (iovh,icrop) ipv (Ns,icrop)
        Dt      = Float (TimeSettings%timestepSize)  / Float (NRSDay)
        Pn      = INO(iovh) / AreaOh(iovh) / Dt
        FmEvPt  = VBOCrop(iovh,Icrop) / AreaGw(iovh,icrop) / Dt


       If (UseScurve(iovh) .eq. 0) then  ! geen Scurve gebruiken
         IStatus = 0
         ! Set initial groundwater level
         DpIn   = LVLOH(iovh) - GWL0 (iovh)
         ! Initial unsat. soil moisture content in m
         VmRzIn = CropOnvZone(Iovh,icrop)%Init_mm * mm2m
         ! max. root zone content and max. percolation
         IRootz = Int( DpRootz * 10)   ! assuming root zone depths in table 10, 20, 30, 40, 50, 60, 70, 80, 90 with index 1,2,3 etc.
         if (irootz > 0) then
            UnsatMax = SRRZ (Ns, Irootz, 1) * 1000.  ! maximum root zone content unsat zone (mm)
         else
            UnsatMax = 0.0d0
         endif
         FmPeMax  = -1. * KSatCapsim(Ns) / 1000.  ! maximum capris/perc in m/day   NB FmPeMax is negatief (percolatie)
         if (idebug .ne. 0) then
             write (Idebug,*) ' Irootz   ', Irootz
             write (Idebug,*) ' isoil    ', Ns
             write (Idebug,*) ' UnsatMax ', UnsatMax
             write (Idebug,*) ' Ksat     ', KsatCapsim(Ns)
             write (Idebug,*) ' FmPeMax  ', FmPeMax
         endif
         ! call Sobek-Capsim-Simgro routine
         if (Idebug .gt.0) call DebugSimgro (Idebug, Nt, Ns, DpIn, DpRootZ, Dt, Pn, VmRzIn, FmEvPt)
         call SIMGRO_OVZ(Debug_unit, CapsimDbgFile, Message_Unit, CapsimMsgFile, IStatus, &
                         NXSPUN, NXTE, NXRZ, NXDPUN, NXFRSW, &
                         Ns, Nt, DpIn, DpRootz, Dt, Pn, VmRzIn, FmEvPt, &
                         SRRZ, FMCA, SCSA, &
                         DPRZUN, DPGWUN, NUDPUN, DPFRSW, FRSW, FREV, &
                         FmEvAc, FmPe, VmRzAc)
         Call CheckSimgro (Istatus, VmRzAc, Iovh, NodeName)
         if (idebug .ne. 0) then
             write (Idebug,*) ' Output from Sobek-Capsim with:'
             write(idebug,*) ' Evapotranspiration ', FmEvAc, ' m/d'
             write(idebug,*) ' Actual volume      ', VmRzAc, ' m'
             write(idebug,*) ' CapRise-Percolation', FmPe, ' m/d'
         endif
         ! store Capsim output in Sobek-RR variables
         ! From Capsim:  actual evap FmEvAc in m/d,
         !               actual capRise-percolation FmPe in m/d, (+=capRise)
         !               actual storage in rootzone VmRzAc in m.
         ! Convert everything to volumes in m3.

         ! actual evapotranspiration from unsaturated zone
         VBOCrop(iovh,icrop) = FmEvAc * AreaGw (iovh,icrop) * Dt
         VBO(iovh) = VBO(iovh) + FmEvAc * AreaGw (iovh,icrop) * Dt
         ! net percolation
         if (.not. UnpavedPercolationLikeSobek213) then
            if (FmPe .lt. 0) then
               ! limit rate of change of VmRz by limiting FmPe
               FmPe = max (FmPe, UserFactVRZ * (VmRzAc - VmRzIn)/Dt )
            endif
            if (FmPe .lt. 0) then
                FmPe = max (FmPe, FmPeMax)
                if (idebug .ne. 0) write(idebug,*) ' CapRise-Percolation adjusted using Ksat =', FmPe, ' m/d'
            endif
         endif
         QinBCrop(Iovh,icrop) =  -1. * FmPe * AREAGW(iovh,icrop) * Dt
         QinB(IOVH) = QinB(iovh) -1. * FmPe * AREAGW(iovh,icrop) * Dt
         ! Rootzone Storage
         CropOnvZone(iovh,icrop)%Actual_Volume = VmRzAc * AreaGW(iovh,icrop)
         if (idebug .ne. 0) write(idebug,*) ' Actual Vol OnvZone  via VmRzAc', CropOnvZone(iovh,icrop)%Actual_Volume
         CropOnvZone(iovh,icrop)%Actual_Volume = CropOnvZone(iovh,icrop)%Init_Volume + &
                                                   INO(iovh)*PercentArea - VBOCrop(iovh,icrop) - QinBCrop(iovh,icrop)
         if (idebug .ne. 0) write(idebug,*) ' Actual Vol OnvZone  via balans incl. FmPe', CropOnvZone(iovh,icrop)%Actual_Volume
         CropOnvZone(Iovh,icrop)%Actual_mm     = CropOnvZone(Iovh,icrop)%Actual_Volume / AreaGw(Iovh,icrop) / mm2m

         if (.not. UnpavedPercolationLikeSobek213) then
            ReduceSurfaceInfiltrationCrop(icrop) = 0.0
            if (UnsatMax .le. CropOnvZone(iovh,icrop)%Actual_mm) then
                if (idebug .ne. 0) write(idebug,*) ' Actual mm before correction', CropOnvZone(iovh,icrop)%Actual_mm
                ReduceSurfaceInfiltrationCrop(icrop) = CropOnvZone(iovh,icrop)%Actual_mm - UnsatMax  ! in mm
                CropOnvZone(Iovh,icrop)%Actual_mm = UnsatMax
                CropOnvZone(Iovh,icrop)%Actual_Volume = UnsatMax * mm2m * AreaGw(iovh,icrop)
                if (idebug .ne. 0) write(idebug,*) ' Adjusted Actual Vol OnvZone  via balans incl. UnsatMax', CropOnvZone(iovh,icrop)%Actual_Volume
                if (idebug .ne. 0) write(idebug,*) ' Actual mm after correction', CropOnvZone(iovh,icrop)%Actual_mm
            endif
            ReduceSurfaceInfiltration = ReduceSurfaceInfiltration + ReduceSurfaceInfiltrationCrop(icrop) * AreaGw(iovh,icrop) * mm2m ! in m3
            if (idebug .ne. 0) write(idebug,*) ' ReduceSurfaceInfiltration', ReduceSurfaceInfiltration
         endif


       Elseif (UseSCurve(iovh) .eq. 1) then
         ! Capsim, met gebruik S curve voor het oppervlak
         VBoCrop(iovh,icrop) = 0.0
         QinBCrop(iovh,icrop) = 0.0
         ! max. root zone content and max. percolation
         IRootz = Int( DpRootz * 10)   ! assuming root zone depths in table 10, 20, 30, 40, 50, 60, 70, 80, 90 with index 1,2,3 etc.
         if (irootz > 0) then
            UnsatMax = SRRZ (Ns, Irootz, 1) * 1000.0d0   ! maximum root zone content unsat zone (mm)
         else
            UnsatMax = 0.0d0
         endif
         FmPeMax  = -1. * KSatCapsim(Ns) / 1000.  ! maximum capris/perc in m/day   NB FmPeMax is negatief (percolatie)
         if (idebug .ne. 0) then
!            write (Idebug,*) ' Ipoint Scurve', Ipoint
             write (Idebug,*) ' Irootz   ', Irootz
             write (Idebug,*) ' isoil    ', Ns
             write (Idebug,*) ' UnsatMax ', UnsatMax
             write (Idebug,*) ' Ksat     ', KsatCapsim(Ns)
             write (Idebug,*) ' FmPeMax  ', FmPeMax
         endif
         Do IPoint=1,UseUnpavedSCurve
            IStatus = 0
            ! Depth of groundwater layer; 1 fixed groundwaterlevel, but different surface levels
            DpIn   = max (0.0d0, AreaScurve(iovh,ipoint)%Level - GWL0 (iovh) )
            ! Initial unsat. soil moisture content in m
            VmRzIn = AreaScurvePerCrop(iovh,icrop,ipoint)%InitialSoilStorage / AreaScurve(iovh,ipoint)%Area / PercentArea
            ! call Sobek-Capsim-Simgro routine
            if (Idebug .gt.0) call DebugSimgro (Idebug, Nt, Ns, DpIn, DpRootZ, Dt, Pn, VmRzIn, FmEvPt)
            call SIMGRO_OVZ(Debug_unit, CapsimDbgFile, Message_Unit, CapsimMsgFile, IStatus, &
                         NXSPUN, NXTE, NXRZ, NXDPUN, NXFRSW, &
                         Ns, Nt, DpIn, DpRootz, Dt, Pn, VmRzIn, FmEvPt, &
                         SRRZ, FMCA, SCSA, &
                         DPRZUN, DPGWUN, NUDPUN, DPFRSW, FRSW, FREV, &
                         FmEvAc, FmPe, VmRzAc)
            Call CheckSimgro (Istatus, VmRzAc, Iovh, NodeName)
            if (idebug .ne. 0) then
               write (Idebug,*) ' Output from Sobek-Capsim Scurve zone with:'
               write(idebug,*) ' Evapotranspiration ', FmEvAc, ' m/d'
               write(idebug,*) ' Actual volume      ', VmRzAc, ' m'
               write(idebug,*) ' CapRise-Percolation', FmPe, ' m/d'
            endif
          ! store Capsim output in Sobek-RR variables
          ! Convert everything to volumes in m3.

          ! actual evapotranspiration from unsaturated zone
            VBOCrop(iovh,icrop) = VBOCrop(iovh,icrop) + FmEvAc * AreaScurve(iovh,ipoint)%Area * Dt * PercentArea
            VBO(iovh) = VBO(iovh) + FmEvAc * AreaScurve(iovh,ipoint)%Area * Dt * PercentArea
          ! net percolation
            if (.not. UnpavedPercolationLikeSobek213) then
               if (FmPe .lt. 0) then
                  ! limit rate of change of VmRz by limiting FmPe
                  FmPe = max (FmPe, UserFactVRZ * (VmRzAc - VmRzIn)/Dt )
               endif
               if (FmPe .lt. 0) then
                   FmPe = max (FmPe, FmPeMax)
                   if (idebug .ne. 0) write(idebug,*) ' CapRise-Percolation adjusted using Ksat =', FmPe, ' m/d'
               endif
            endif
            QinBCrop(IOVH,icrop) = QINBCrop(iovh,icrop)  -1. * FmPe * AreaScurve(iovh,ipoint)%Area * Dt * PercentArea
            QINB(IOVH) = QINB(iovh)  -1. * FmPe * AreaScurve(iovh,ipoint)%Area * Dt * PercentArea
            if (idebug .ne. 0) then
              write (Idebug,*) ' Output in loop in RR terms:'
              write(idebug,*) ' VBO  -verdamping bodem ', VBO(iovh)
              write(idebug,*) ' QINB Percolation-CapRise', QINB(iovh)
            endif
            AreaScurvePerCrop(iovh,icrop,ipoint)%ActualSoilStorage = &
                AreaScurvePerCrop(iovh,icrop,ipoint)%ActualSoilStorage &
                     - FmEvAc * AreaScurve(iovh,ipoint)%Area * Dt * PercentArea &
                       + FmPe * AreaScurve(iovh,ipoint)%Area * Dt * PercentArea
         Enddo     ! end of loop over Scurve points
         ! Rootzone Storage
         CropOnvZone(iovh,icrop)%Actual_Volume = CropOnvZone(iovh,icrop)%Init_Volume + &
                                                   INO(iovh)*PercentArea - VBOCrop(iovh,icrop) - QINBCrop(iovh,icrop)
         ! Feb 2013: before incorrect use of AreaOh instead of AreaGw ?
         CropOnvZone(Iovh,icrop)%Actual_mm     = CropOnvZone(Iovh,icrop)%Actual_Volume / AreaGw(Iovh,icrop) / mm2m
         if (idebug .ne. 0) write(idebug,*) ' Actual Vol OnvZone  via balans incl. FmPe', CropOnvZone(iovh,icrop)%Actual_Volume
         if (idebug .ne. 0) write(idebug,*) ' Actual mm  OnvZone  via balans incl. FmPe', CropOnvZone(iovh,icrop)%Actual_mm

         if (.not. UnpavedPercolationLikeSobek213) then
            ReduceSurfaceInfiltrationCrop(icrop) = 0.0
            if (UnsatMax .le. CropOnvZone(iovh,icrop)%Actual_mm) then
                if (idebug .ne. 0) write(idebug,*) ' Actual mm before correction', CropOnvZone(iovh,icrop)%Actual_mm
                ReduceSurfaceInfiltrationCrop(icrop) = CropOnvZone(iovh,icrop)%Actual_mm - UnsatMax  ! in mm
                CropOnvZone(Iovh,icrop)%Actual_mm = UnsatMax
                CropOnvZone(Iovh,icrop)%Actual_Volume = UnsatMax * mm2m * AreaGw(iovh,icrop)
                if (idebug .ne. 0) write(idebug,*) ' Adjusted Actual Vol OnvZone  via balans incl. UnsatMax', CropOnvZone(iovh,icrop)%Actual_Volume
                if (idebug .ne. 0) write(idebug,*) ' Actual mm after correction', CropOnvZone(iovh,icrop)%Actual_mm
            endif
            ReduceSurfaceInfiltration = ReduceSurfaceInfiltration + ReduceSurfaceInfiltrationCrop(icrop) * AreaGw(iovh,icrop) * mm2m ! in m3
            if (idebug .ne. 0) write(idebug,*) ' ReduceSurfaceInfiltration', ReduceSurfaceInfiltration
         endif

       Endif
!     Determine totals for unpaved area (sum of crops)
       OnvZone(iovh)%Actual_Volume = OnvZone(Iovh)%Actual_Volume + CropOnvZone(iovh,icrop)%Actual_Volume

      Endif
     Enddo
     OnvZone(Iovh)%Actual_mm = OnvZone(Iovh)%Actual_Volume / AreaOh(Iovh) / mm2m

    Endif

!   debug output, independent of Scurve
    if (idebug .ne. 0) then
        write (Idebug,*) ' Output in RR terms:'
        write(idebug,*) ' VBO  -verdamping bodem ', VBO(iovh)
        write(idebug,*) ' Actual Vol OnvZone     ', OnvZone(iovh)%Actual_Volume
        write(idebug,*) ' Actual mm  OnvZone     ', OnvZone(iovh)%Actual_mm
        write(idebug,*) ' QINB Percolation-CapRise', QINB(iovh)
    endif

   Return
   End subroutine ComputeCapsim




   Subroutine ComputeCapsimPlus (iovh, Idebug, TotalInfReduction, NodeName)

! *********************************************************************
! Subroutine berekent onverzadigde zone voor onverhard gebied met index Iovh
! met de eigen Capsim+ methode
!
! Houdt rekening met evt. S curve voor het oppervlak
!  In geval van S curve wordt met 1 grondwaterstand in NAP gerekend, en met 1 bergingscoefficient
!  Wel wordt de onverzadigde zone per deelgebiedje bijgehouden qua volume, en verschillende capillaire opstijging, verdamping etc.
!  Grondwaterstand volgt dan later uit totale balans.
!
! *********************************************************************
!   iovh   = arrayindex in arrays voor onverhard gebied
!   idebug = debug file
! *********************************************************************

   Integer Iovh, idebug
   Character(Len=CharIdLength) NodeName

! S curve. crop
   Integer Ipoint, icrop
   Double precision    TotalInfReduction

!WL_Capsim local Variables
    Integer Nt, Ns, IStatus, ioption
    Double precision    DpIn, DpRootz, Dt, Pn, VmRzIn, FmEvPt, FmEvAc, FmPe, VmRzAc, PercentArea, InfReduction
    Double precision    DpIn1, VmRzIn1
    Double precision    PreviousCapRis
!End Capsim

     Ioption = 1    ! flag to specify if VRZ_New should be computed  1=yes, 0=no

     DpIn1  = LVLOH(iovh) - GWL  (iovh)
     VmRzIn1 = OnvZone(Iovh)%Actual_mm * mm2m

     ! Eerst algemene initialisaties Capsim, onafhankelijk van Scurve gebruik
     ! Set crop index, soil index, Rootzone depth in m
     ! Timestep size Dt in days
     ! Net precipitation Pn in m/d  (== use computed infiltration in m/d)
     ! Potential evapotranspiration FmEvPt in m/d
     Nt      = CapsimCrop(iovh)
     Ns      = BotTyp (iovh)
     DpRootz = CapsimDpRootz(iovh)
     Dt      = Float (TimeSettings%timestepSize)  / Float (NRSDay)
     Pn      = INO(iovh) / AreaOh(iovh) / Dt
     FmEvPt  = VBO(iovh) / AreaOh(iovh) / Dt
     OnvZone(iovh)%Actual_Volume = 0.0
     TotalInfReduction = 0.0
     PreviousCapRis = PreviousTimestepCapRis(iovh) / AreaOh(iovh) / Dt   ! in m/day

     If (UseSCurve(iovh) .eq. 1) then
       Do IPoint=1,UseUnpavedSCurve
         AreaScurve(iovh,ipoint)%ActualSoilStorage = &
              AreaScurve(iovh,ipoint)%InitialSoilStorage &
                + INO(iovh) * AreaScurve(iovh,ipoint)%Area / AreaOh(iovh)
         If (CapsimPerCropArea .eq. 1) then
           Do ICrop=1,NCrop
              PercentArea = AreaGw(Iovh,Icrop) / AreaOh(iovh)
              If (PercentArea .gt. 0.0) then
                AreaScurvePerCrop(iovh,icrop,ipoint)%ActualSoilStorage = &
                  AreaScurvePerCrop(iovh,icrop,ipoint)%InitialSoilStorage &
                   + INO(iovh) * AreaScurve(iovh,ipoint)%Area / AreaOh(iovh) * PercentArea
              Endif
           Enddo
         Endif
       Enddo
     Endif

     If (CapsimPerCropArea .eq. 0) then
! Compute Capsim once per unpaved area, using average rootzone depth and main crop

      If (UseScurve(iovh) .eq. 0) then  ! geen Scurve gebruiken
         IStatus = 0
       ! Set DpIn=initial groundwater level in meters below surface
         DpIn   = LVLOH(iovh) - GWL0 (iovh)
       ! Initial unsat. soil moisture content in m
         VmRzIn  = OnvZone(Iovh)%Init_mm * mm2m
       ! DpIn1 and VmRzIn1 already defined above
       ! call WL-SobekCapsim routine
         if (Idebug .gt.0) call DebugWL_Capsim (Idebug, Nt, Ns, DpIn, DpRootZ, Dt, Pn, VmRzIn, FmEvPt)
         Call WL_Capsim2 (Ns, Nt, DpIn, DpRootz, Dt, Pn, VmRzIn, FmEvPt, &
                          FmEvAc, FmPe, VmRzAc , InfReduction, iovh, &
                          DpIn1, VmRzIn1, idebug, PreviousCapRis)
         Call CheckWL_Capsim (Istatus, VmRzAc, Iovh)
         if (idebug .ne. 0) then
            write (Idebug,*) ' Output from WL-Capsim with:'
            write(idebug,*) ' Evapotranspiration ', FmEvAc, ' m/d'
            write(idebug,*) ' Actual volume      ', VmRzAc, ' m'
            write(idebug,*) ' CapRise-Percolation', FmPe, ' m/d'
         endif
       ! store Capsim output in Sobek-RR variables
       ! From Capsim:  actual evap FmEvAc in m/d,
       !               actual capRise-percolation FmPe in m/d, (+=capRise)
       !               actual storage in rootzone VmRzAc in m.
       ! Convert everything to volumes in m3.

       ! actual evapotranspiration from unsaturated zone
         VBO(iovh) = FmEvAc * Areaoh (iovh) * Dt
       ! net percolation
         QINB(IOVH) = -1. * FmPe * AREAOH(iovh) * Dt
       ! Reduction of infiltration
         TotalInfReduction = TotalInfReduction + InfReduction * AreaOH(iovh) * Dt
       ! Rootzone Storage
         OnvZone(iovh)%Actual_Volume = VmRzAc * AreaOh(iovh)
         if (idebug .ne. 0) write(idebug,*) ' Actual Vol OnvZone  via VmRzAc', OnvZone(iovh)%Actual_Volume
         OnvZone(iovh)%Actual_Volume = OnvZone(iovh)%Init_Volume + INO(iovh) - VBO(iovh) - QINB(iovh) - TotalInfReduction
         if (idebug .ne. 0) write(idebug,*) ' Actual Vol OnvZone  via balans', OnvZone(iovh)%Actual_Volume
         OnvZone(Iovh)%Actual_mm     = OnvZone(Iovh)%Actual_Volume / AreaOh(Iovh) / mm2m

      Elseif (UseSCurve(iovh) .eq. 1) then
        ! Capsim, met gebruik S curve voor het oppervlak
         VBo (iovh) = 0.0
         QinB(iovh) = 0.0
         Do IPoint=1,UseUnpavedSCurve
            IStatus = 0
            ! Depth of groundwater layer; 1 fixed groundwaterlevel, but different surface levels
            DpIn   = max (0.0d0, AreaScurve(iovh,ipoint)%Level - GWL0 (iovh) )
            DpIn1  = max (0.0d0, AreaScurve(iovh,ipoint)%Level - GWL  (iovh) )
            ! Initial unsat. soil moisture content in m
            VmRzIn = AreaScurve(iovh,ipoint)%InitialSoilStorage / AreaScurve(iovh,ipoint)%Area
            VmRzIn1 = AreaScurve(iovh,ipoint)%ActualSoilStorage / AreaScurve(iovh,ipoint)%Area
            ! call Sobek-Capsim-Simgro routine
            if (Idebug .gt.0) call DebugWL_Capsim (Idebug, Nt, Ns, DpIn, DpRootZ, Dt, Pn, VmRzIn, FmEvPt)
            Call WL_Capsim2 (Ns, Nt, DpIn, DpRootz, Dt, Pn, VmRzIn, FmEvPt, &
                            FmEvAc, FmPe, VmRzAc , InfReduction, iovh, &
                            DpIn1, VmRzIn1, idebug, PreviousCapRis)
            Call CheckWL_Capsim (Istatus, VmRzAc, Iovh)
            if (idebug .ne. 0) then
               write (Idebug,*) ' Output from WL-Capsim Scurve zone with:'
               write(idebug,*) ' Evapotranspiration ', FmEvAc, ' m/d'
               write(idebug,*) ' Actual volume      ', VmRzAc, ' m'
               write(idebug,*) ' CapRise-Percolation', FmPe, ' m/d'
            endif
          ! store Capsim output in Sobek-RR variables
          ! Convert everything to volumes in m3.

          ! actual evapotranspiration from unsaturated zone
            VBO(iovh) = VBO(iovh) + FmEvAc * AreaScurve(iovh,ipoint)%Area * Dt
          ! net percolation
            QINB(IOVH) = QINB(iovh)  -1. * FmPe * AreaScurve(iovh,ipoint)%Area * Dt
          ! Reduction of infiltration
            TotalInfReduction = TotalInfReduction + InfReduction * AreaSCurve(iovh,ipoint)%Area * Dt
            if (idebug .ne. 0) then
              write (Idebug,*) ' Output in loop in RR terms:'
              write(idebug,*) ' VBO  -verdamping bodem ', VBO(iovh)
              write(idebug,*) ' QINB Percolation-CapRise', QINB(iovh)
            endif
            AreaScurve(iovh,ipoint)%ActualSoilStorage = &
                AreaScurve(iovh,ipoint)%InitialSoilStorage &
                   + INO(iovh) * AreaScurve(iovh,ipoint)%Area / AreaOh(iovh) &
                     - FmEvAc * AreaScurve(iovh,ipoint)%Area * Dt &
                       + FmPe * AreaScurve(iovh,ipoint)%Area * Dt  &
                       - InfReduction * AreaScurve(iovh,ipoint)%Area * Dt
         enddo
         ! Rootzone Storage
         OnvZone(iovh)%Actual_Volume = OnvZone(iovh)%Init_Volume + INO(iovh) - VBO(iovh) - QINB(iovh) - TotalInfReduction
         OnvZone(Iovh)%Actual_mm     = OnvZone(Iovh)%Actual_Volume / AreaOh(Iovh) / mm2m
      endif

    Else

! May 2001
! Compute Capsim for all crops per unpaved area, using crop rootzone depth
     VBo (iovh) = 0.0
     QinB(iovh) = 0.0
     Do Icrop=1,NCrop
      If (AreaGw(Iovh,icrop) .gt. 0) then
        PercentArea = AreaGw(Iovh,Icrop) / AreaOh(iovh)
      ! Set crop index, soil index, Rootzone depth in m
      ! Timestep size Dt in days
      ! Net precipitation Pn in m/d  (== use computed infiltration in m/d)
      ! Potential evapotranspiration FmEvPt in m/d
        Nt      = ICrop
        Ns      = BotTyp (iovh)
        DpRootz = DpRz(Ns,icrop)  ! er stond (iovh,icrop) ipv (Ns,icrop)
        Dt      = Float (TimeSettings%timestepSize)  / Float (NRSDay)
        Pn      = INO(iovh) / AreaOh(iovh) / Dt
        FmEvPt  = VBOCrop(iovh,Icrop) / AreaGw(iovh,icrop) / Dt


       If (UseScurve(iovh) .eq. 0) then  ! geen Scurve gebruiken
         IStatus = 0
         ! Set initial groundwater level
         DpIn   = LVLOH(iovh) - GWL0 (iovh)
         ! Initial unsat. soil moisture content in m
         VmRzIn = CropOnvZone(Iovh,icrop)%Init_mm * mm2m
         ! same for final
         DpIn1  = LVLOH(iovh) - GWL  (iovh)
         VmRzIn1 = CropOnvZone(Iovh,icrop)%Actual_mm * mm2m
         ! call Sobek-Capsim-Simgro routine
         if (Idebug .gt.0) call DebugWL_Capsim (Idebug, Nt, Ns, DpIn, DpRootZ, Dt, Pn, VmRzIn, FmEvPt)
         Call WL_Capsim2 (Ns, Nt, DpIn, DpRootz, Dt, Pn, VmRzIn, FmEvPt, &
                          FmEvAc, FmPe, VmRzAc , InfReduction, iovh, &
                          DpIn1, VmRzIn1, idebug, PreviousCapRis)
         Call CheckWL_Capsim (Istatus, VmRzAc, Iovh)
         if (idebug .ne. 0) then
             write (Idebug,*) ' Output from WL-Capsim with:'
             write(idebug,*) ' Evapotranspiration ', FmEvAc, ' m/d'
             write(idebug,*) ' Actual volume      ', VmRzAc, ' m'
             write(idebug,*) ' CapRise-Percolation', FmPe, ' m/d'
         endif
         ! store Capsim output in Sobek-RR variables
         ! From Capsim:  actual evap FmEvAc in m/d,
         !               actual capRise-percolation FmPe in m/d, (+=capRise)
         !               actual storage in rootzone VmRzAc in m.
         ! Convert everything to volumes in m3.

         ! actual evapotranspiration from unsaturated zone
         VBOCrop(iovh,icrop) = FmEvAc * AreaGw (iovh,icrop) * Dt
         VBO(iovh) = VBO(iovh) + FmEvAc * AreaGw (iovh,icrop) * Dt
         ! net percolation
         QinBCrop(Iovh,icrop) =  -1. * FmPe * AREAGW(iovh,icrop) * Dt
         QinB(IOVH) = QinB(iovh) -1. * FmPe * AREAGW(iovh,icrop) * Dt
       ! Reduction of infiltration
         TotalInfReduction = TotalInfReduction + InfReduction * AreaGW(iovh,icrop) * Dt
         ! Rootzone Storage
         CropOnvZone(iovh,icrop)%Actual_Volume = VmRzAc * AreaGW(iovh,icrop)
         if (idebug .ne. 0) write(idebug,*) ' VmRzAc', VmRzAc
         if (idebug .ne. 0) write(idebug,*) ' Actual Vol CropOnvZone  via VmRzAc', CropOnvZone(iovh,icrop)%Actual_Volume
         CropOnvZone(iovh,icrop)%Actual_Volume = CropOnvZone(iovh,icrop)%Init_Volume + &
                                                   INO(iovh)*PercentArea - VBOCrop(iovh,icrop) - QinBCrop(iovh,icrop) &
                                                    - InfReduction * AreaGW(iovh,icrop) * Dt
         if (idebug .ne. 0) write(idebug,*) ' InitVol en mm', CropOnvZone(iovh,icrop)%Init_Volume,CropOnvZone(iovh,icrop)%Init_mm
         if (idebug .ne. 0) write(idebug,*) ' Ino          ', Ino(iovh)
         if (idebug .ne. 0) write(idebug,*) ' VBOCrop      ', VboCrop(iovh,icrop)
         if (idebug .ne. 0) write(idebug,*) ' QinBCrop     ', QinBCrop(iovh,icrop)
         if (idebug .ne. 0) write(idebug,*) ' InfRed       ', InfReduction
         if (idebug .ne. 0) write(idebug,*) ' AreaGW       ', AreaGW(iovh,icrop)
         if (idebug .ne. 0) write(idebug,*) ' PercentArea  ', PercentArea
         if (idebug .ne. 0) write(idebug,*) ' Dt           ', Dt
         if (idebug .ne. 0) write(idebug,*) ' Actual Vol CropOnvZone  via balans', CropOnvZone(iovh,icrop)%Actual_Volume
         CropOnvZone(Iovh,icrop)%Actual_mm     = CropOnvZone(Iovh,icrop)%Actual_Volume / AreaGw(Iovh,icrop) / mm2m

       Elseif (UseSCurve(iovh) .eq. 1) then
         ! Capsim, met gebruik S curve voor het oppervlak
         VBoCrop(iovh,icrop) = 0.0
         QinBCrop(iovh,icrop) = 0.0
         Do IPoint=1,UseUnpavedSCurve
            IStatus = 0
            ! Depth of groundwater layer; 1 fixed groundwaterlevel, but different surface levels
            DpIn   = max (0.0d0, AreaScurve(iovh,ipoint)%Level - GWL0 (iovh) )
            DpIn1  = max (0.0d0, AreaScurve(iovh,ipoint)%Level - GWL  (iovh) )
            ! Initial unsat. soil moisture content in m
            VmRzIn = AreaScurvePerCrop(iovh,icrop,ipoint)%InitialSoilStorage / AreaScurve(iovh,ipoint)%Area / PercentArea
            VmRzIn1= AreaScurvePerCrop(iovh,icrop,ipoint)%ActualSoilStorage / AreaScurve(iovh,ipoint)%Area / PercentArea
            ! call Sobek-Capsim-Simgro routine
            if (Idebug .gt.0) call DebugWL_Capsim (Idebug, Nt, Ns, DpIn, DpRootZ, Dt, Pn, VmRzIn, FmEvPt)
            Call WL_Capsim2 (Ns, Nt, DpIn, DpRootz, Dt, Pn, VmRzIn, FmEvPt, &
                            FmEvAc, FmPe, VmRzAc , InfReduction, iovh, &
                            DpIn1, VmRzIn1, idebug, PreviousCapRis)
            Call CheckWL_Capsim (Istatus, VmRzAc, Iovh)
            if (idebug .ne. 0) then
               write (Idebug,*) ' Output from WL-Capsim Scurve zone with:'
               write(idebug,*) ' Evapotranspiration ', FmEvAc, ' m/d'
               write(idebug,*) ' Actual volume      ', VmRzAc, ' m'
               write(idebug,*) ' CapRise-Percolation', FmPe, ' m/d'
            endif
          ! store Capsim output in Sobek-RR variables
          ! Convert everything to volumes in m3.

          ! actual evapotranspiration from unsaturated zone
            VBOCrop(iovh,icrop) = VBOCrop(iovh,icrop) + FmEvAc * AreaScurve(iovh,ipoint)%Area * Dt * PercentArea
            VBO(iovh) = VBO(iovh) + FmEvAc * AreaScurve(iovh,ipoint)%Area * Dt * PercentArea
          ! net percolation
            QinBCrop(IOVH,icrop) = QINBCrop(iovh,icrop)  -1. * FmPe * AreaScurve(iovh,ipoint)%Area * Dt * PercentArea
            QINB(IOVH) = QINB(iovh)  -1. * FmPe * AreaScurve(iovh,ipoint)%Area * Dt * PercentArea
          ! Reduction of infiltration
            TotalInfReduction = TotalInfReduction + InfReduction * AreaSCurve(iovh,ipoint)%Area * Dt * PercentArea
            if (idebug .ne. 0) then
              write (Idebug,*) ' Output in loop in RR terms:'
              write(idebug,*) ' VBO  -verdamping bodem ', VBO(iovh)
              write(idebug,*) ' QINB Percolation-CapRise', QINB(iovh)
            endif
            AreaScurvePerCrop(iovh,icrop,ipoint)%ActualSoilStorage = &
                AreaScurvePerCrop(iovh,icrop,ipoint)%ActualSoilStorage &
                     - FmEvAc * AreaScurve(iovh,ipoint)%Area * Dt * PercentArea &
                       + FmPe * AreaScurve(iovh,ipoint)%Area * Dt * PercentArea &
                         - InfReduction * AreaScurve(iovh,ipoint)%Area * Dt * PercentArea
         Enddo     ! end of loop over Scurve points
         ! Rootzone Storage
         CropOnvZone(iovh,icrop)%Actual_Volume = CropOnvZone(iovh,icrop)%Init_Volume + &
                                                   INO(iovh)*PercentArea - VBOCrop(iovh,icrop) - QINBCrop(iovh,icrop) - TotalInfReduction
         CropOnvZone(Iovh,icrop)%Actual_mm     = CropOnvZone(Iovh,icrop)%Actual_Volume / AreaOh(Iovh) / mm2m
       Endif
!     Determine totals for unpaved area (sum of crops)
       OnvZone(iovh)%Actual_Volume = OnvZone(Iovh)%Actual_Volume + CropOnvZone(iovh,icrop)%Actual_Volume

      Endif
     Enddo
     OnvZone(Iovh)%Actual_mm = OnvZone(Iovh)%Actual_Volume / AreaOh(Iovh) / mm2m

    Endif

!   debug output, independent of Scurve
    if (idebug .ne. 0) then
        write (Idebug,*) ' Output in RR terms:'
        write(idebug,*) ' VBO  -verdamping bodem ', VBO(iovh)
        write(idebug,*) ' Actual Vol OnvZone     ', OnvZone(iovh)%Actual_Volume
        write(idebug,*) ' Actual mm  OnvZone     ', OnvZone(iovh)%Actual_mm
        write(idebug,*) ' QINB Percolation-CapRise', QINB(iovh)
        write(idebug,*) ' TotalInfReduction       ', TotalInfReduction
    endif

   Return
   End subroutine ComputeCapsimPlus


   Subroutine WL_Capsim2 (Ns, Nt, DpIn, DpRootz, Dt, Pn, VmRzIn, FmEvPt, &
                          FmEvAc, FmPe, VmRzAc , InfReduction, iovh, &
                          DpIn1, VmRzIn1, idebug, PreviousCapRis)

   Integer Nt, Ns, IStatus, idebug, ioption, iovh
   Double precision    DpIn, DpRootz, Dt, Pn, VmRzIn, FmEvPt, FmEvAc, FmPe, VmRzAc, InfReduction
   Double precision    DpIn1, VmRzIn1, FmEvAc1, FmPe1, VmRzAc1, InfReduction1, VRZ_equilibrium, K_unsat
   Double precision    PreviousCapRis

   ! calls WL_Capsim for initial situation and final situation (DpIn, VmRzIn)
   ! computes FmPe, FmEvAc, VmRzAc

         InfReduction  = 0.0
         if (Idebug .gt.0) call DebugWL_Capsim (Idebug, Nt, Ns, DpIn, DpRootZ, Dt, Pn, VmRzIn, FmEvPt)
         ! call with initial data DPin and VmRzIn at beginning of timestep
         Ioption = 1
         call WL_CAPSIM (Ns, Nt, DpIn, DpRootz, Dt, Pn, VmRzIn, FmEvPt, &
                         FmEvAc, FmPe, VmRzAc, InfReduction, Ioption, iovh, &
                         VRZ_equilibrium, K_unsat, PreviousCapRis)
         Call CheckWL_Capsim (Istatus, VmRzAc, Iovh)
!         VRZ_eq(iovh) = VRZ_equilibrium/1000.
         if (idebug .ne. 0) then
             write (Idebug,*) ' Output from WL-Capsim with:'
             write(idebug,*) ' DpIn               ', DpIn  , ' m-sl'
             write(idebug,*) ' VmRzIn             ', VmRzIn, ' m'
             write(idebug,*) ' Evapotranspiration ', FmEvAc, ' m/d'
             write(idebug,*) ' Actual volume      ', VmRzAc, ' m'
             write(idebug,*) ' CapRise-Percolation', FmPe, ' m/d'
             write(idebug,*) ' Infreduction       ', InfReduction, ' m/d'
         endif
         if (CapsimPlusflag .le. 1) goto 999
         ! call with data DPin1 and VmRzIn1 at end of timestep
         Ioption = 2
         InfReduction1 = 0.0
         if (Idebug .gt.0) call DebugSimgro (Idebug, Nt, Ns, DpIn1, DpRootZ, Dt, Pn, VmRzIn1, FmEvPt)
         call WL_CAPSIM (Ns, Nt, DpIn1, DpRootz, Dt, Pn, VmRzIn1, FmEvPt, &
                         FmEvAc1, FmPe1, VmRzAc1, InfReduction1, Ioption, iovh, &
                         VRZ_equilibrium, K_unsat, PreviousCapRis)
         Call CheckWL_Capsim (Istatus, VmRzAc, Iovh)
!         VRZ_eq(iovh) = 0.5 * (VRZ_eq(iovh) + VRZ_equilibrium/1000.)
         if (idebug .ne. 0) then
             write (Idebug,*) ' Output from WL-Capsim with:'
             write(idebug,*) ' DpIn1              ', DpIn1  , ' m-sl'
             write(idebug,*) ' VmRzIn1            ', VmRzIn1, ' m'
             write(idebug,*) ' Evapotranspiration ', FmEvAc1, ' m/d'
             write(idebug,*) ' CapRise-Percolation', FmPe1, ' m/d'
             write(idebug,*) ' Infreduction1      ', InfReduction1, ' m/d'
         endif
         ! average / update balance
         InfReduction = 0.5 * (InfReduction + InfReduction1)
         FmEvAc       = 0.5 * (FmEvAc + FmEvAc1)
         FmPe         = 0.5 * (FmPe   + FmPe1)
         VmRzAc       = VmRzIn + Dt * (Pn - InfReduction + FmPe - FmEvAc)
         if (idebug .ne. 0) then
             write (Idebug,*) ' Average :'
             write(idebug,*) ' Infiltrationreduction', InfReduction, ' m/d'
             write(idebug,*) ' Evapotranspiration ', FmEvAc, ' m/d'
             write(idebug,*) ' CapRise-Percolation', FmPe,   ' m/d'
             write(idebug,*) ' New Actual Volume  ', VmRzAc, ' m'
             write(idebug,*) ' VRZ_MAX            ', VRZ_Max(iovh)
!             write(idebug,*) ' VRZ_eq             ', VRZ_eq(iovh)
         endif
         ! VmRzAc can not exceed maximum, reduce infiltration
         If (VmRzAc .gt. VRZ_Max(iovh) .and. Pn .gt. 0) then
            InfReduction = InfReduction + (VmRzAc - VRZ_Max(iovh))/Dt
            VmRzAc = VRZ_Max(iovh)
         endif
         ! No CapRis if VmRzAc above equilibrium, and max. perc. K_Unsat
!         If (VmRzAc .gt. VRZ_eq(iovh) .and. FmPe .gt. 0) then
!            FmPe0 = FmPe
!            FmPe  = FmPe - (VmRzAc - VRZ_eq(iovh))/Dt
!            if (FmPe  .lt. -1.* UserCoefKSat * K_Unsat) then
!               VmRzAc = VmRzAc + (FmPe - FmPe0) * Dt
!            else
!               VmRzAc = VRZ_eq(iovh)
!            Endif
!         Endif
!         ! No percolation if VmRzAc below equilibrium
!         If (VmRzAc .lt. VRZ_eq(iovh) .and. FmPe .lt. 0) then
!            FmPe  = FmPe + (VRZ_eq(iovh)-VmRzAc)/Dt
!            VmRzAc = VRZ_eq(iovh)
!         Endif
         if (idebug .ne. 0) then
             write (Idebug,*) ' Average after checks&corrections:'
             write(idebug,*) ' Infiltrationreduction', InfReduction, ' m/d'
             write(idebug,*) ' Evapotranspiration ', FmEvAc, ' m/d'
             write(idebug,*) ' CapRise-Percolation', FmPe,   ' m/d'
             write(idebug,*) ' New Actual Volume  ', VmRzAc, ' m'
         endif
 999     Continue
   Return
   End subroutine WL_Capsim2


   Subroutine WL_Capsim (Ns, Nt, DpIn, DpRootz, Dt, Pn, VmRzIn, FmEvPt, &
                         FmEvAc, FmPe, VmRzAc , InfReduction, Ioption, iovh, &
                         VRZ_Equilibrium, K_Unsat, PreviousCapRis)

   Integer Nt, Ns, ioption, iovh
   Double precision    DpIn, DpRootz, Dt, Pn, VmRzIn, FmEvPt, FmEvAc, FmPe, VmRzAc, InfReduction, PreviousCapRis

   !input:
   ! iovh = index unpaved area
   ! Nt = crop index
   ! Ns = soil index
   ! DpIn = gw level (m below surface)
   ! DpRootz = root zone depth (m below surface)
   ! Dt = timestepsize in days
   ! Pn = net precipitation in m/day
   ! VmRzIn = initial volume unsat zone (in m)
   ! FmEvPt = potential evaporation in m/day
   ! PreviousCapRis = Capillary Rise - Percolation of last timestep, in m/day  (+ = CapRis)
   !output:
   ! FmEvAc = actual evaporation in m/day
   ! FmPe   = caprise - percolation in m/day (positive = capillary rise)
   ! VmRzAc = final volume unsat zone (in m)
   ! InfReduction = infiltration reduction (m)
   ! Ioption = 1 means VRZ_New should be calculated, 0 means it should not be calculated
   !            (is relevant if two calculations are done, one using VmRzIn (t=0) and one using VmRz (t=1)
   ! iovh = index unpaved area

   Integer Isoil, Icrop, Irz, Irz1, Igw, Igw1, idebug
   Double precision    Epot, EAct, VRZ, VRZ_H1, VRZ_H2, VRZ_H3,VRZ_H4, VRZ_H3Low, VRZ_H3High, VRZTemp
   Double precision    alfa, AlfaFact
   Double precision    Inf, K_Unsat, CapRisMax, CapRis, VRZ_equilibrium, VRZ_new, VRZMax, &
           StorCoef, Perc, WeightIrz, WeightIgw1, WeightIgw2

   Isoil = Ns
   Icrop = Nt
   Irz  = FindRootzoneIndex (DpRootZ)                  ! root zone index
   Irz1 = max (1, min (irz+1,NactRzClass) )            ! root zone index +1
   Igw  = FindGwlIndex (ISoil,Irz,DpIn)                ! gw index
   Igw1 = max (1, min (igw+1,NactGwlClass) )           ! gw index+1
   ! set linear interpolation weights
   WeightIrz  = 1.0
   WeightIgw1 = 1.0
   WeightIgw2 = 1.0
   if (irz .ne. irz1)  WeightIrz = 1.0 - (DpRootz*100. - DpRzUn(irz)) / (DpRzUn(irz1) - DpRzUn(irz))
   if (igw .ne. igw1)  WeightIgw1 = 1.0 - (DpIn - DPGWUN(isoil,irz,igw)) / &
                                            (DPGWUN(isoil,irz,igw1) - DPGWUN(isoil,irz,igw))
   if (igw .ne. igw1)  WeightIgw2 = 1.0 - (DpIn - DPGWUN(isoil,irz1,igw)) / &
                                            (DPGWUN(isoil,irz1,igw1) - DPGWUN(isoil,irz1,igw))


   Inf  = Pn * 1000.        ! Inf  = infiltration from soil in mm/day
   Epot = FmEvPt * 1000.    ! Epot = potential evapotranspiration in mm/day
   if (Epot .le. 1.0) then
      VRZ_H3 = Weighted_Average (VRZH3_Low(ISoil,Irz,icrop), VRZH3_Low(ISoil,Irz1,icrop), WeightIrz)
   elseif (Epot .ge. 5.0) then
      VRZ_H3 = Weighted_Average (VRZH3_High(ISoil,Irz,icrop), VRZH3_High(ISoil,Irz1,icrop), WeightIrz)
   else   ! Epot between 1 and 5 mm
      VRZ_H3low  = Weighted_Average (VRZH3_High(ISoil,Irz,icrop), VRZH3_High(ISoil,Irz1,icrop), WeightIrz)
      VRZ_H3high = Weighted_Average (VRZH3_High(ISoil,Irz,icrop), VRZH3_High(ISoil,Irz1,icrop), WeightIrz)
      VRZ_H3 = VRZ_H3low + (VRZ_H3high - VRZ_H3low) * (Epot - 1.) / 4.
   endif
   VRZ_H1 = Weighted_Average (VRZH1(ISoil,Irz,icrop), VRZH1(ISoil,Irz1,icrop), WeightIrz)
   VRZ_H2 = Weighted_Average (VRZH2(ISoil,Irz,icrop), VRZH2(ISoil,Irz1,icrop), WeightIrz)
   VRZ_H4 = Weighted_Average (VRZH4(ISoil,Irz,icrop), VRZH4(ISoil,Irz1,icrop), WeightIrz)
   VRZ    = VmRzIn * 1000.

   if (VRZ .ge. VRZ_H1) then
       alfa = 0.0
   elseif (VRZ_H1 .ge. VRZ .and. VRZ .ge. VRZ_H2) then
       if (VRZ_H1 .gt. VRZ_H2) then
          alfa = (VRZ_H1 - VRZ) / (VRZ_H1 - VRZ_H2)
       else
          alfa = 1.0
       endif
   elseif (VRZ_H2 .ge. VRZ .and. VRZ .ge. VRZ_H3) then
       alfa = 1.0
   elseif (VRZ_H3 .ge. VRZ .and. VRZ .ge. VRZ_H4) then
       if (VRZ_H3 .gt. VRZ_H4) then
          alfa = (VRZ - VRZ_h4) / (VRZ_H3 - VRZ_H4)
       else
          alfa = 0.0
       endif
   elseif (VRZ_H4 .ge. VRZ) then
       alfa = 0.0
   endif
   EAct = alfa * EPot      ! Actual evap in mm/day

   K_Unsat = Weighted_Average4 (KDoorlatendheid(ISoil,Irz,igw), KDoorlatendheid(ISoil,Irz,igw1), &
                                 KDoorlatendheid(ISoil,Irz1,igw), KDoorlatendheid(ISoil,Irz1,igw1),&
                                  weightigw1, weightigw2, weightirz)

   VRZ_equilibrium  = 1000.* Weighted_Average4 (SRRZ(ISoil,Irz,igw), SRRZ(ISoil,Irz,igw1),&
                                        SRRZ(ISoil,Irz1,igw), SRRZ(ISoil,Irz1,igw1), weightigw1, weightigw2, weightirz)
   CapRisMax  = 1000.* Weighted_Average4 (FMCA(ISoil,Irz,igw), FMCA(ISoil,Irz,igw1),&
                                        FMCA(ISoil,Irz1,igw), FMCA(ISoil,Irz1,igw1), weightigw1, weightigw2, weightirz)
! Oct-Nov 2006
   AlfaFact  = Weighted_Average4 (AlfaFactor(ISoil,Irz,igw), AlfaFactor(ISoil,Irz,igw1),&
                                        AlfaFactor(ISoil,Irz1,igw), AlfaFactor(ISoil,Irz1,igw1), weightigw1, weightigw2, weightirz)
   CapRis  = Eact + (1000*PreviousCapRis - Eact) * (exp (-AlfaFact * TimeSettings%TimestepSize / 86400. / .1 ))  !/ abs (Eact*.1 - PreviousCapRis*100.) ) )
   CapRis  = min (CapRis, CapRisMax)  ! in mm/day
! end Nov 2006
   StorCoef= Weighted_Average4 (SCSA(ISoil,Irz,igw), SCSA(ISoil,Irz,igw1),&
                                 SCSA(ISoil,Irz1,igw), SCSA(ISoil,Irz1,igw1), weightigw1, weightigw2, weightirz)

!
!  UserCoefKsat = 0.1     ! vertical K = 0.1 * horizontal Kdoorlatendheid; 0.1=default coefficient ToineV
!  UserCoefKsat = 1000.   ! immediate percolation, to simulate as Capsim
!  UserCoefKsat  is read from INI file, default value put to 1 in RdIni

! voor bepaling Capris, Perc: use VRZ + infiltratie - act.evap
   VRZTemp = VRZ + Dt * (Inf - Eact)
   If (VRZTemp .gt. VRZ_equilibrium) then
      CapRis = 0.
      Perc   = min (UserCoefKsat * K_Unsat, UserFactVRZ * (VRZTemp - VRZ_equilibrium)/ Dt )  ! mm/day percolation
   ElseIf (VRZTemp .le. VRZ_equilibrium) then
      Perc = 0.
      CapRis = min (CapRis, (VRZ_equilibrium - VRZTemp)/ Dt )  ! mm/day capillary rise
   Endif

   VRZ_new = VRZ
   VRZMax = Weighted_Average (SRRZ(ISoil,Irz,1), SRRZ(ISoil,Irz1,1), WeightIrz) * 1000.
   InfReduction = 0.0
   if (IOption .ge. 1) then
      VRZ_new = VRZ + Dt * (Inf + CapRis - Eact - Perc)
      If (VRZ_new .gt. VRZMax) then
         InfReduction = (VRZ_New - VRZMax) / Dt  ! in m/day
         VRZ_new = VRZMax
      Endif
   Endif

   ! output in Capsim units
   ! FmEvAc = actual evaporation in m/day
   FmEvAc = alfa * FmEvPt
   ! FmPe   = caprise - percoloation in m/day (positive = capillary rise)
   FmPe   = (CapRis - Perc) / 1000.
   ! VmRzAc = final volume unsat zone (in m)
   VmRzAc = VRZ_New / 1000.
   ! InfReduction = infiltration reduction (m)
   InfReduction = InfReduction / 1000.
   ! max. root zone volume in m
   VRZ_Max(iovh) = VRZMax / 1000.

    idebug = 0 ! idebugLunRR
    if (idebug .ne. 0) then
        write(idebug,*) ' isoil     ', Isoil
        write(idebug,*) ' icrop     ', Icrop
        write(idebug,*) ' DpIn gwl  ', DpIn
        write(idebug,*) ' Dprootz   ', DpRootz
        write(idebug,*) ' irz  irz1 ', irz, irz1
        write(idebug,*) ' igw  igw1 ', igw, igw1
        write(idebug,*) ' weightirz ', weightirz
        write(idebug,*) ' weightigw1 weightigw2 ', weightigw1, weightigw2
        write(idebug,*) ' Epot ', Epot
        write(idebug,*) ' alfa ', alfa
        write(idebug,*) ' EAct   ', EAct
        write(idebug,*) ' VRzh1 tm h4 ', VRZ_H1, VRZ_H2, VRZ_H3, VRZ_H4
        write(idebug,*) ' VRzh3 low-high', VRZ_H3, VRZ_H3low, VRZ_H3high
        write(idebug,*) ' FMCA irz  ', FMCA(ISoil,Irz,igw), FMCA(ISoil,Irz,igw1)
        write(idebug,*) ' FMCA irz1 ', FMCA(ISoil,Irz1,igw), FMCA(ISoil,Irz1,igw1)
        write(idebug,*) ' CapRis ', CapRis
        write(idebug,*) ' SRRZ irz  ', SRRZ(ISoil,Irz,igw),  SRRZ(ISoil,Irz,igw1)
        write(idebug,*) ' SRRZ irz1 ', SRRZ(ISoil,Irz1,igw), SRRZ(ISoil,Irz1,igw1)
        write(idebug,*) ' VRZ_eq ', VRZ_equilibrium
        write(idebug,*) ' VRZ_ini', VRZ
        write(idebug,*) ' VRZ_new', VRZ_new
        write(idebug,*) ' VRZ_max', VRZmax
        write(idebug,*) ' Infreduction', Infreduction
    endif

   Return
   End subroutine WL_Capsim


    Integer Function FindRootzoneIndex (DpRootz)
    Double precision    DpRootz
    integer i

    Do i=1,nActRZClass
       if (DpRZUN(i)/100 .ge. DpRootz) goto 999
    Enddo
999 Continue
    FindRootzoneIndex = max(1, min (NActRzClass, i-1))

    return
    end function FindRootzoneIndex


    Integer Function FindGWLIndex (ISoil,Irz,DpIn)
    Integer ISoil, IRz
    Double precision    DpIn
    integer i

    Do i=1,NActGWLClass
       if (DpGWUN(isoil,irz,i) .ge. DpIn) goto 999
    Enddo
999 Continue
    FindGWLIndex = max(1, min (NActGWLClass, i-1))

    return
    end function FindGWLIndex


    Double precision Function Weighted_Average (x1, x2, weight)
    Double precision    x1, x2, weight

    Weighted_Average = weight * x1 + (1.0 - weight) * x2

    return
    end function Weighted_Average


    Double precision Function Weighted_Average4 (x1, x2, x3, x4, weight1, weight2, weight3)
    Double precision    x1, x2, x3,x4, weight1,weight2, weight3
    Double precision    weighted_average1,  weighted_average2

    Weighted_Average1 = weight1 * x1 + (1.0 - weight1) * x2
    Weighted_Average2 = weight2 * x3 + (1.0 - weight2) * x4
    Weighted_Average4 = weight3 * weighted_Average1 + (1.0-weight3) * weighted_Average2

    return
    end function Weighted_Average4



  Subroutine HellingaDeZ (Inode, Iovh, Ibnd, Iow, ipluv, Idebug, Peil, Qin, TotalScurveSurfaceOutflow, MinDepthCF)

! *********************************************************************
! Subroutine berekent Hellinga de Zeeuw uitstroming uit bodem onverhard gebied Iovh naar open water
! met de Hellinga de Zeeuw formules
! Stroming kan zowel drainage als infiltratie zijn
! Houdt rekening met evt. S curve voor het oppervlak;
! NB  Voor de S curve kan een deel van de uitstroming uit de bodem al afstroming over oppervlak zijn!
! *********************************************************************
!   iovh   = arrayindex in arrays voor onverhard gebied
!   idebug = debug file
!   peil   = open water peil of peil op de rand in m
!   Qin    = Verticale instroming  in m3/s
! *********************************************************************

   Integer             Iovh, Ibnd, Iow, IPluv, idebug, inode, i
   Double precision    Peil, Qin, TotalScurveSurfaceOutflow
   Double precision    Ratio, MaxMm, ActMm, OnSurface
   Real                TmpAreaOw
   Double precision    Delta_BndPeil

! Local arrays for interpolation
      Real PeilArray(6), AreaArray(6)  ! aanname dat NVAL=6 zoals in Conf_Arr.

!Scurve
   Integer Ipoint
   Double precision    Qin2

   Double precision dH1, dH2, dH3, dH4, dH5, dQ1, dQ2, dQ3, dQ4, dQ5, q1, q2, q3, q4, q5

   Double precision MinDepthCF
   Double precision OldQ2O

       If (UseScurve(iovh) .eq. 0) then
!       Hellinga de Zeeuw, zonder S curve
        if (AreaOh(iovh) .gt. 0.001) then
           OnSurface = BoLnd0 (iovh) / AreaOh(iovh)
        endif
        if (idebug .ne. 0) Write(Idebug,*) 'LvlDrn', LvlDrn(iovh,1), LvlDrn(iovh,2), LvlDrn(iovh,3)
        Call SetDeltaH (Idebug, Peil, Iovh, DH1, Dh2, DH3, DH4, Dq1, Dq2, Dq3, Dq4, Qin, OnSurface)
        Call HellingaDeZeeuwFormule (Q1, AreaGwComp(iovh), Alfaoh(iovh,2), HdeZBergC(Iovh), DH1, DQ1, timeSettings%timestepSize)
        Call HellingaDeZeeuwFormule (Q2, AreaGwComp(iovh), Alfa2 (iovh,1), HdeZBergC(Iovh), DH2, DQ2, timeSettings%timestepSize)
        Call HellingaDeZeeuwFormule (Q3, AreaGwComp(iovh), Alfa2 (iovh,2), HdeZBergC(Iovh), DH3, DQ3, timeSettings%timestepSize)
        Call HellingaDeZeeuwFormule (Q4, AreaGwComp(iovh), Alfa2 (iovh,3), HdeZBergC(Iovh), DH4, DQ4, timeSettings%timestepSize)

        if (idebug .ne. 0) write(iDebug, *) 'Q1, Q2, Q3, Q4, dh1, gwl0(iovh)'
        if (idebug .ne. 0) write(iDebug, *) Q1, Q2, Q3, Q4, dh1

       ! total soil outflow, omzetten naar m3/s
        Q2O(IOVH) = Q1 + Q2 + Q3 + Q4
        Q2O(IOVH) = Q2O(IOVH) / timeSettings%timestepSize

        ! correctie in geval van infiltratie
        IF (Q2O(IOVH) .LT.  0) THEN
          if (idebug .ne. 0) WRITE(IDEBUG,*) ' Q2O negative'
          if (idebug .ne. 0) WRITE(IDEBUG,*) ' Now with infiltration'
!          Call HellingaDeZeeuwFormule (Q2O(iovh), AreaOh(iovh), AlfaOh (iovh,3), &
!                                       HdeZBergC(Iovh), Gwl0(iovh)-Peil, QIn, timeSettings%timestepSize)
          Call HellingaDeZeeuwFormule (Q2O(iovh), AreaGwComp(iovh), AlfaOh (iovh,3), &
                                       HdeZBergC(Iovh), Gwl0(iovh)-Peil, QIn, timeSettings%timestepSize)
          Q2O(IOVH) = Q2O(IOVH) / timeSettings%timestepSize
! 1 sept 2003: als gwl0 >= maaiveld, dan geen infiltratie vanuit ow/bnd mogelijk
          if (NoInfiltrationWhileGwOnSurface) then
             if (Gwl0(iovh) .ge. LvlOhMx(iovh)) then
                Q2O(iovh) = 0.0
             elseif (Gwl0(iovh) - LvlOhMx(iovh) .gt. -0.1) then
!               2 sept 2003: als gwl0 vlak onder maaiveld, dan een quasi volumecheck
                if (UnSatZoneOption .ge. 1) then
                   Q2O(iovh) = max (Q2O(iovh), &
                                    -1.* AreaGwComp(iovh) * (LvlOhMx(iovh)-Gwl0(iovh))*0.01 / timeSettings%timestepSize )
                else
                   Q2O(iovh) = max (Q2O(iovh), &
                                    -1.* AreaGwComp(iovh) * (LvlOhMx(iovh)-Gwl0(iovh))*BergC(iovh) / timeSettings%timestepSize )
                endif
             endif
          endif
        ENDIF

       Elseif (UseSCurve(iovh) .eq. 1) then
! Hellinga de Zeeuw, met S curve oppervlak
! ARS 5599: houdt voor de 5e term (gwl boven oppervlak) ook rekening met berging op oppervlak
         Ratio = 1.0
         MaxMm = 1.0
         ActMm = 1.0
         if (AreaOh(iovh) .gt. 0.001) then
           Ratio = AreaGwComp(iovh) / AreaOh(iovh)
           MaxMm = BMaxOl(iovh) / AreaOh(iovh)       ! here in meters, not in mm
           ActMm = BoLnd (iovh) / AreaOh(iovh)       ! here in meters, not in mm
           if (idebug .ne. 0) write(Idebug,*) ' Qin Ratio Maxmm ActMM',Qin,Ratio, MaxMm,ActMm
           OnSurface = BoLnd0 (iovh) / AreaOh(iovh)
         endif
         Q2O (iovh) = 0.0
         Do IPoint=1,UseUnpavedSCurve
           if (idebug .ne. 0) WRITE(IDEBUG,*) ' HdeZ with Scurve for iovh ipoint', iovh, ipoint
!          Qin2 naar rato van S curve arealen verdelen
           Call SetDeltaHSCurve (Idebug, Peil, Iovh, Ipoint, DH1, Dh2, DH3, DH4, DH5, Dq1, Dq2, Dq3, Dq4, Dq5, &
                                                                            Qin, Qin2, MaxMm, ActMm, OnSurface)
! Voor Soil outflow (Q1 tm Q4): gebruik Scurve arealen * ratio AreaGwComp/AreaOh;
!                               dus aanname dat extra areaal zelfde verdeling heeft
! Voor Surface runoff Q5      : alleen Scurve areaal gebruiken
           Call HellingaDeZeeuwFormule (Q1, AreaScurve(iovh,ipoint)%Area *Ratio, Alfaoh(iovh,2), &
                                        HdeZBergC(Iovh), DH1, DQ1, timeSettings%timestepSize)
           Call HellingaDeZeeuwFormule (Q2, AreaScurve(iovh,ipoint)%Area *Ratio, Alfa2 (iovh,1), &
                                        HdeZBergC(Iovh), DH2, DQ2, timeSettings%timestepSize)
           Call HellingaDeZeeuwFormule (Q3, AreaScurve(iovh,ipoint)%Area *Ratio, Alfa2 (iovh,2), &
                                        HdeZBergC(Iovh), DH3, DQ3, timeSettings%timestepSize)
           Call HellingaDeZeeuwFormule (Q4, AreaScurve(iovh,ipoint)%Area *Ratio, Alfa2 (iovh,3), &
                                        HdeZBergC(Iovh), DH4, DQ4, timeSettings%timestepSize)
! extra zone Dh5, Dq5, voor GWL0 boven dit deel van maaiveld Scurve; debiet Q5 wordt al bij afstroming over oppervlak gerekend
! dit deel op oppervlak stroomt direct af met alfa-oppervlak; dus niet wachten tot max.berging BMaxOl voor dit deel is overschreden
! Sept 2000: ARS 5599: bij DH5 is nu de berging op land ook meegenomen
           If (Dh5 .gt. 0) then
             Call HellingaDeZeeuwFormule (Q5, AreaScurve(iovh,ipoint)%Area, Alfaoh(iovh,1), 1.0D0 ,&
                                          DH5, DQ5, timeSettings%timestepSize)
! ARS 5599: Q5 surface runoff Scurve >=0
! door correctie van de DH5 blijkt hij soms -0.001 te worden; dit moet voorkomen worden
             Q5 = max (0.0d0, Q5)
           else
             Q5 = 0.0
           endif

           if (idebug .ne. 0) write(iDebug, *) 'Q1, Q2, Q3, Q4, Q5, dh4, dh5, gwl0(iovh)'
           if (idebug .ne. 0) write(iDebug, *) Q1, Q2, Q3, Q4, Q5, dh4, dh5, gwl0(iovh)

          ! soil outflow per part, omzetten naar m3/s
           AreaScurve(iovh,ipoint)%SoilOutflow = Q1 + Q2 + Q3 + Q4
           AreaScurve(iovh,ipoint)%SoilOutflow =  AreaScurve(iovh,ipoint)%SoilOutflow / timeSettings%timestepSize

          ! afstroming oppervlak per part, omzetten naar m3/s
           AreaScurve(iovh,ipoint)%SurfaceOutflow = Q5 / timeSettings%timestepSize

           ! correctie in geval van infiltratie
           IF ( AreaScurve(iovh,ipoint)%SoilOutflow .LT.  0) THEN
             if (idebug .ne. 0) WRITE(IDEBUG,*) ' AreaScurve%SoilOutflow negative'
             if (idebug .ne. 0) WRITE(IDEBUG,*) ' Now with infiltration'
             If (GWL0(IOVH) .LT. AreaScurve(iovh,ipoint)%Level ) Then
!              Hellinga de Zeeuw op verschil GWL - peil
!              Soil Infiltration op SCurve areaal * Ratio
               if (idebug .ne. 0) WRITE(IDEBUG,*) ' infiltration based on Gwl0-peil'
               Call HellingaDeZeeuwFormule ( AreaScurve(iovh,ipoint)%SoilOutflow, AreaScurve(iovh,ipoint)%Area *Ratio, &
                                             AlfaOh (iovh,3), &
                                             HdeZBergC(Iovh), Gwl0(iovh)-Peil, QIn2, timeSettings%timestepSize)
             Else
!              Hellinga de Zeeuw op verschil Level - peil
!              Soil Infiltration op SCurve areaal * Ratio   ? Echter hier GWL al boven mv?
               if (idebug .ne. 0) WRITE(IDEBUG,*) ' infiltration based on AreaScure - peil'
               Call HellingaDeZeeuwFormule ( AreaScurve(iovh,ipoint)%SoilOutflow, AreaScurve(iovh,ipoint)%Area *Ratio, &
                                             AlfaOh (iovh,3), &
                                             HdeZBergC(Iovh), AreaScurve(iovh,ipoint)%Level-Peil, QIn2, timeSettings%timestepSize)
!  ARS 11277 - n.a.v. vraag HH performance problemen met Haarlemmermeer, HCP2 bui
!              was ten onrechte              HdeZBergC(Iovh), AreaScurve(iovh,ipoint)%Area-Peil, QIn2, timeSettings%timestepSize)

! 1 sept 2003: als gwl0 >= maaiveld, dan geen infiltratie vanuit ow/bnd mogelijk voor dit deel vd Scurve
               if (NoInfiltrationWhileGwOnSurface) then
                  if (Gwl0(iovh) .ge. AreaScurve(iovh,ipoint)%Level) then
                     AreaScurve(iovh,ipoint)%SoilOutflow = 0.0
                  elseif (Gwl0(iovh) - AreaScurve(Iovh,ipoint)%Level .gt. -0.1) then
!                  2 sept 2003: als gwl0 vlak onder maaiveld, dan een quasi volumecheck
                    if (UnSatZoneOption .ge. 1) then
                      AreaScurve(iovh,ipoint)%SoilOutflow = max (AreaScurve(iovh,ipoint)%SoilOutflow, &
                                       -1.* AreaGwComp(iovh) * (AreaScurve(iovh,ipoint)%Level-Gwl0(iovh)) *0.01 )
                    else
                      AreaScurve(iovh,ipoint)%SoilOutflow = max (AreaScurve(iovh,ipoint)%SoilOutflow, &
                                       -1.* AreaGwComp(iovh) * (AreaScurve(iovh,ipoint)%Level-Gwl0(iovh))*BergC(iovh) )
                    endif
                  endif
               endif
             Endif
             AreaScurve(iovh,ipoint)%SoilOutflow =  AreaScurve(iovh,ipoint)%SoilOutflow / timeSettings%timestepSize
           ENDIF

 ! total soil outflow Q2O
           Q2O(IOVH) = Q2O(IOVH) +  AreaScurve(iovh,ipoint)%SoilOutflow
           if (idebug .ne. 0) Then
             write(iDebug, *) 'AreaScurve(iovh,ipoint)%SoilOutflow'
             write(iDebug, *)  AreaScurve(iovh,ipoint)%SoilOutflow
             write(iDebug, *) ' Q2O (iovh) na optelling ', Q2O(iovh)
           ENDIF


 ! total Surface Outflow bij Scurve
           TotalScurveSurfaceOutflow = TotalScurveSurfaceOutflow + AreaScurve(iovh,ipoint)%SurfaceOutflow
         Enddo

       Endif

       if (idebug .ne. 0) write(Idebug,*) ' Q2O(iovh) voor checks ', Q2o(iovh)
       if (Q2O(Iovh) .lt. 0 .and. Ibnd .gt. 0) then
! Volumecheck bij infiltratie, en directe koppeling aan rand: voorkom droogval in CF
         ! boundary areaal=BNDPAR(ibnd,5), peil=PEIL, boundary depth=BndPar(ibnd,6)
            if (idebug .ne. 0) THEN
              WRITE(IDEBUG,*) ' Voor check bnd-areaal', Q2O(IOVH)
              WRITE(IDEBUG,*) ' AreaBnd, ovhlevel, bndlevel',BNDPAR(IBND,5), LVLOHMx(iovh), PEIL
              WRITE(IDEBUG,*) ' Depth at boundary          ',BNDPAR(IBND,6)
            ENDIF
! check als diepte bekend in His file, dat niet meer dan droogval-minimumdepth wordt onttrokken
            If (BndPar(Ibnd,6) .lt. 998.0) then
               Q2O(iovh) = max (Q2O(iovh),  -1. * BndPar(Ibnd,5) * (BndPar(Ibnd,6)-MinDepthCF) / TimeSettings%timestepSize )
               If (BndPar(Ibnd,6) .lt. MinDepthCF) Q2O(iovh) = 0.0
               If (BndPar(Ibnd,6) .lt. 0.0) Q2O(iovh) = 0.0
            Else
              ! Depth not in HIS file, so no check possible
            Endif
       Elseif (Q2O(Iovh) .lt. 0 .and. IPluv .gt. 0) then
! Volumecheck bij infiltratie, en directe koppeling aan rand: voorkom droogval in SF
! check als diepte bekend in His file, dat niet meer dan droogval-minimumdepth wordt onttrokken
            If (SbkLvlPluv(ipluv) .gt. -999.) then
               Q2O(iovh) = max (Q2O(iovh),  -1. * SbkAreaPluv(ipluv) * (SbkDepthPluv(Ipluv)-MinDepthCF) / TimeSettings%timestepSize )
               If (SbkDepthPluv(Ipluv) .lt. MinDepthCF) Q2O(iovh) = 0.0
               If (SbkDepthPluv(Ipluv) .lt. 0.0) Q2O(iovh) = 0.0
            Else
              ! Depth not in HIS file, so no check possible
            Endif
       Elseif (Q2O(Iovh) .gt. 0 .and. Iow .gt. 0) then
! n.a.v. ARS 8842 convergentieprobleem KJ - Capsim: Volumecheck bij drainage naar open water
            Do i=1,NVal
               PeilArray(i) = PeilOw(i,iow)
               AreaArray(i) = AreaOw(i,iow)
            Enddo
            CALL RR_INTERP (NVAL, PEILArray, AREAArray, Sngl(PEIL), TmpAreaOw, OwLastInterpIndex(iow))
            if (idebug .ne. 0) THEN
              WRITE(IDEBUG,*) ' NVAL, PEIL, Lastinterpindx iow,', NVAL, PEIL, OWLastInterpIndex(iow), iow
              WRITE(IDEBUG,'(A,6F10.3)') ' PEILOW array ', (PeilOw(i,iow),i=1,6)
              WRITE(IDEBUG,'(A,6F10.3)') ' AreaOW array ', (AreaOw(i,iow),i=1,6)
              WRITE(IDEBUG,*) ' Voor Volumecheck Berekende drainage ', Q2O(IOVH)
              WRITE(IDEBUG,*) ' tmpAreaOw, gwlevel, owlevel',tmpAreaOw, GWL0(iovh), LVLOW0(iow)
            ENDIF
            If (FixArs8842) then
              OldQ2O = Q2O(iovh)
              Q2O(iovh) = min (Q2O(iovh), tmpAreaOw * (GWL0(iovh)-lvlOw0(iOW)) / TimeSettings%timestepSize  )
              Q2O(iovh) = max (Q2O(iovh), 0.0d0)
              if (idebug .ne. 0) Write(IDEBUG,*) ' Na VolumeCheck Berekende drainage ', Q2O(IOVH)
              Call UnpavedVolumeCheckActive (OldQ2O, Q2O(iovh), Inode, ibnd, iow)
            Endif
! end n.a.v. ARS 8842
       Elseif (Q2O(Iovh) .gt. 0 .and. Ibnd .gt. 0) then
!  ARS 11610 Check of door deze lozing de peilstijging in CF niet boven gws uitkomt
         If (FixArs11610) then
           OldQ2O = Q2O(iovh)
           if (idebug .ne. 0) write(Idebug,*) ' HellingadeZ VolumeCheck FixArs11610'
           if (idebug .ne. 0) write(Idebug,*) ' BndPar5', BndPar(ibnd,5)
           if (idebug .ne. 0) write(Idebug,*) ' VolCheckFactor',UnpVolumeCheckFactorToCF
           Delta_bndpeil = Q2O(iovh)*TimeSettings%timestepSize / Bndpar(ibnd,5) / UnpVolumeCheckFactorToCF
           Delta_bndpeil = Delta_bndpeil + Peil
           if (Delta_bndpeil .gt. Gwl0(iovh)) then
              if (idebug .ne. 0) write(Idebug,*) ' BergC ',BergC(iovh)
              if (idebug .ne. 0) write(Idebug,*) ' Gwl0  ',Gwl0(iovh)
              if (idebug .ne. 0) write(Idebug,*) ' Peil  ',Peil
              Q2O(iovh) = min (Q2O(iovh), (Gwl0(iovh) - Peil)*BergC(iovh) * BndPar(Ibnd,5) * &
                                              UnpVolumeCheckFactorToCF / TimeSettings%TimestepSize)
              Q2O(iovh) = max (Q2O(iovh), 0.0d0)
           endif
           if (idebug .ne. 0) Write(IDEBUG,*) ' Na VolumeCheck Berekende drainage ', Q2O(IOVH)
           Call UnpavedVolumeCheckActive (OldQ2O, Q2O(iovh), Inode, ibnd, iow)
         Endif
       Elseif (Q2O(Iovh) .gt. 0 .and. Ipluv .gt. 0) then
!  ARS 11610 Check of door deze lozing de peilstijging in SF niet boven gws uitkomt
!        to be added
       Endif

   Return
  END subroutine HellingaDeZ




  Subroutine HellingaDeZOppervlak (Inode, Iovh, Idebug, Peil, Iow, Ibnd, Ipluv)

! *********************************************************************
! *** Subroutine berekent Hellinga de Zeeuw afstroming over oppervlak onverhard gebied Iovh
! ***  naar open water Iow of randknoop Ibnd
! ***  met de Hellinga de Zeeuw formule
! *** Het rekening houden met evt. Scurve gebeurt al bij de grondwateruitstroming (nl. daar kan de GWL al boven het maaiveld komen)
! *********************************************************************
!   iovh   = arrayindex in arrays voor onverhard gebied
!   idebug = debug file
!   peil   = open water peil of peil op de rand
!   iow    = index open water
!   ibnd   = index boundary
! *********************************************************************

   Integer Inode, Iovh, idebug, Iow, Ibnd, IPluv, i
   Double precision    Peil

   Real                TmpAreaOw
   Double precision    Rdum, Qin, Q0In, QInNu, BergingLand
   Double precision    OldQ1O

! Local arrays for interpolation
      Real PeilArray(6), AreaArray(6)  ! aanname dat NVAL=6 zoals in Conf_Arr.


!     Hellinga de Zeeuw afstroming oppervlak, zonder S curve
! ARS 25328
      BergingLand = BoLnd(IOVH) / AreaOh(Iovh)

      Q1O(IOVH) = 0.0
      If ( (BOLND(IOVH) .GT. BMAXOL(IOVH) .or. BOLND0(iovh) .gt. BMAXOL(iovh)) .AND. PEIL .LE. LVLOHMx(IOVH)+BergingLand) Then
      ! boven max.berging op land ook met alfa-factor
         QIN  = MAX (0.0d0, BOLND(IOVH) - BMAXOL(IOVH)) / timeSettings%timestepSize
         Q0IN = MAX (0.0d0, BOLND0(IOVH) - BMAXOL(IOVH)) / timeSettings%timestepSize
!
!        QINNU= MAX (0.0, QIN-Q0IN)
!        negatieve QinNu moet ook meegenomen worden (nav test 30 jaar HKV, waar negatieve berging werd berekend)
         QINNU= QIN-Q0IN
! ARS 1920
! Correctie april 1999: ARS 1920: alfa is toegevoegd
         Q1O(IOVH) =  (Q0IN * ALFAOH(IOVH,1) - QINNU) / ALFAOH(IOVH,1) * &
                         (1.0 - EXP(-ALFAOH(IOVH,1)*timeSettings%timestepSize) ) &
                          + QINNU * timeSettings%timestepSize
         if (idebug .ne. 0)  WRITE(IDEBUG,*) ' Opp. Afstroming oude methode' , Q1O(IOVH)
! Correctie Ars 5176, Mei 2000
! Via Call HellingaDeZeeuwFormula
! om te testen ARS 5379 soms weer even uitgezet
         if (FixARS5176) then
            Call HellingaDeZeeuwFormule (Q1O(iovh), Q0In*TimeSettings%TimestepSize, AlfaOh(Iovh,1), &
                                         1.0D0, 1.0D0, QinNu, TimeSettings%timestepSize)
         endif

!GP Aanpassing Oct97: ook hier checken op areaal open water/boundary en evt. beperking opp. afstroming (volumecheck)
         IF (IOW .GT. 0) THEN
            Do i=1,NVal
               PeilArray(i) = PeilOw(i,iow)
               AreaArray(i) = AreaOw(i,iow)
            Enddo
            CALL RR_INTERP (NVAL, PEILArray, AREAArray, Sngl(PEIL), TmpAreaOw, OwLastInterpIndex(iow))
            if (idebug .ne. 0) THEN
              WRITE(IDEBUG,*) ' Opp. Afstroming', Q1O(IOVH)
              WRITE(IDEBUG,*) ' Bolnd, Bolnd0 ', Bolnd(IOVH), Bolnd0(iovh)
              WRITE(IDEBUG,*) ' Qin Q0in Qinnu ', Qin, Q0in, Qinnu
              WRITE(IDEBUG,*) ' alfaoh ', alfaoh(iovh,1)
              WRITE(IDEBUG,*) ' emacht ', Exp(-alfaoh(iovh,1)*timeSettings%timestepSize)
              WRITE(IDEBUG,*) ' Voor check ow-areaal', Q1O(IOVH)
              WRITE(IDEBUG,*) ' tmpAreaOw, ovhlevel, owlevel',tmpAreaOw, LVLOHMx(iovh), LVLOW0(iow)
            ENDIF
            OldQ1O = Q1O(iovh)
            Q1O(iovh) = min (Q1O(iovh), tmpAreaOw * (LVLOHMx(iovh)+BergingLand -lvlOw0(iOW)) * UnpVolumeCheckFactorToOW) !! ARS 15351, 15358 (Feb2006), 15464 April 2006
            Call UnpavedVolumeCheckActive (OldQ1O, Q1O(iovh), Inode, ibnd, iow)
         ELSEIF (IBND .GT. 0) THEN
         ! boundary areaal=BNDPAR(ibnd,5), peil=PEIL
            if (idebug .ne. 0) THEN
              WRITE(IDEBUG,*) ' Voor check bnd-areaal', Q1O(IOVH)
              WRITE(IDEBUG,*) ' AreaBnd, ovhlevel, bndlevel',BNDPAR(IBND,5), LVLOHMx(iovh), PEIL
            ENDIF
            OldQ1O = Q1O(iovh)
            Q1O(iovh) = min (Q1O(iovh), BNDPAR(IBND,5) * (LVLOHMx(iovh)+BergingLand -PEIL) * UnpVolumeCheckFactorToCF)  !! ARS 15351, 15358, Feb2006
            if (idebug .ne. 0) Write(IDEBUG,*) ' Na VolumeCheck Berekende SurfRunoff', Q1O(IOVH)
            Call UnpavedVolumeCheckActive (OldQ1O, Q1O(iovh), Inode, ibnd, iow)
         ELSEIF (IPluv .GT. 0) THEN
            ! no volumecheck added
         ENDIF
! Afstroming oppervlak positief en in m3/s; nieuwe berging op land
         Q1O(iovh) = max (Q1O(iovh), 0.0d0)
         Q1O(IOVH) = Q1O(IOVH) / timeSettings%timestepSize
         BOLND(IOVH) = BOLND0(IOVH) - Q1O(IOVH) * timeSettings%timestepSize &
                      - VO(IOVH) + RO (IOVH) - INO(IOVH) + IrrigationSupply(iovh) * TimeSettings%TimestepSize

! Check dat berging op land niet negatief wordt door de berekende surface runoff
! Kleiner dan max. berging >0 mag wel (door verdamping)
         If (BoLnd(iovh) .lt. 0.0) then
            RDum = -1. * BoLnd(iovh) / TimeSettings%TimestepSize
            If (RDum .gt. Q1O(iovh)) then
               Q1O(iovh) = 0.0
               BOLND(IOVH) = BOLND0(IOVH) - VO(IOVH) + RO (IOVH) - INO(IOVH) + IrrigationSupply(iovh) * TimeSettings%TimestepSize
            Else
               Q1O(iovh) = Q1O(iovh)-RDUM
               BoLnd(Iovh) = 0.0
            Endif
         Endif
      Endif
      if (idebug .ne. 0)  WRITE(IDEBUG,*) ' Opp. Afstroming bij EXIT HellingaDeZOppervlak' , Q1O(IOVH)

   Return
  END subroutine HellingaDeZOppervlak



  Subroutine KrayenhoffvdLeur (Iovh, Idebug, Peil, Qin, Itmstp)

! *********************************************************************
! Subroutine berekent volgens Krayenhoff vd Leur
!  - uitstroming uit bodem onverhard gebied Iovh naar open water
!  - opbolling grondwaterstand
!
! Stroming kan zowel drainage als infiltratie zijn
!
! Bij Krayenhoff vd Leur wordt geen rekening gehouden met gw op maaiveld
! In formules geen afhankelijkheid van maaiveldnivo, dus Scurve maaiveld is niet van belang
!
! iovh = index onverhard gebied
! peil = open water peil in m
! qin  = instroming (neerslag en kwel) in m3/s
!
! *********************************************************************

   Integer Iovh, Idebug, Itmstp
   Double precision    Peil, Qin
   Double precision    Resttijd, ActualRestTermFactor

!Krayenhoff van de Leur lokale variabelen (pi, pi_kwadraat etc. globaal in deze module)
    Double precision    Opbolling, debiet, tijd, h2add, q2add, h2term, q2term, rhlp, rhlp2
    Integer n, h2sign
    Integer idum

!   idebug = 0
!       Krayenhoff vd Leur, zonder S curve

! Krayenhoff van de Leur formulation
! Qin is al in m3/s
! zet KvdLeur variabelen huidige tijdstap; neerslag in m/dag
!       KvdLeur(iovh,KvdLDimensie)%neerslag  = Qin *  NrsDay / Areaoh(iovh)
        KvdLeur(iovh,KvdLDimensie)%neerslag  = Qin *  NrsDay / AreaGwComp(iovh)
! correctie neerslag voor verandering open water peil
        if (idebug .ne. 0)  write(idebug,*) ' Krayenhoff vd Leur computations'
        if ( (KvdLvariatieopenwater .ne. 0) .and. (itmstp .gt. 1) ) then
           if (idebug .ne. 0)  write(idebug,*) ' neerslag in m/dag ',KvdLeur(iovh,KvdLDimensie)%neerslag
           KvdLeur(iovh,KvdLDimensie)%neerslag =  KvdLeur(iovh,KvdLDimensie)%neerslag - &
             (peil - KvdLeurnul(iovh)%peilnul) / timeSettings%timestepSize * NrsDay * BergC(iovh)
           if (idebug .ne. 0)  write(idebug,*) ' neerslag + owpeilvar ', KvdLeur(iovh,KvdLDimensie)%neerslag
        endif
        if (idebug .ne. 0)  write(idebug,*) ' in sub KvdL neerslag in m/dag ',KvdLeur(iovh,KvdLDimensie)%neerslag
        KvdLeur(iovh,KvdLDimensie)%Kopbolling = 0.
        KvdLeur(iovh,KvdLDimensie)%Kdebiet    = 0.
        KvdLeurnul(iovh)%Kopbollinglast = 0.
        KvdLeurnul(iovh)%Kdebietlast    = 0.
        KvdLeurnul(iovh)%openwaterlevel  = peil

        Do idum=1,KvdLDimensie  ! neem effect van historische neerslagen mee, tot KvdLDim tijdstappen terug
          tijd = float ( (KvdLDimensie-idum+1) * timeSettings%timestepSize ) / float (NrsDay) ! tijd in dagen
          q2add = 0.0
          h2add = 0.0
          h2sign = -1
          Debiet = 0.0
          Opbolling = 0.0
          If (KvdLeur(iovh,idum)%neerslag .ne. 0.0) then
            Opbolling = pi_kwadraat8 * KvdLeur(iovh,idum)%neerslag * ReservoirCoeff(Iovh) / BergC(iovh)
            Debiet    = KvdLeur(iovh,idum)%neerslag
            if (KvdLH2Add(iovh,idum) .le. -999.) then     ! not yet computed
               if (idebug .ne. 0) Then
                 write(idebug,*) idum, KvdLeur(iovh,idum)%neerslag, tijd, ' (dagen) '
                 write(idebug,*) '   n h2sign        rhlp       h2add        q2add        h2term        q2term'
               Endif
               Do n=1,NtermenKvdL,2   ! NtermenKvdL is het aantal termen (reservoirs) mee te nemen;
                                      ! ook check op bijdrage per term in de loop
                 h2sign = -1 * h2sign
                 rhlp   = exp (-n * n * tijd / ReservoirCoeff(iovh) )
                 rhlp2  = 1 - rhlp
                 h2term = float(h2sign) / float (n * n * n) * rhlp
!GPTVJun2014444  q2term = 1.0 / (n * n) *  rhlp
                 q2term = 1.0 / (n * n) *  rhlp2
                 h2add  = h2add + h2term
                 q2add  = q2add + q2term
                 if (idebug .ne. 0) Then
                    write(idebug,'(2I5,5E13.5)')  n, h2sign, rhlp, h2add, q2add, h2term, q2term
                 Endif
                 if ( (abs(h2term) .lt. 1E-10) .and. (abs(q2term*AreaGwComp(iovh)) .le. 1E-10) ) goto 123
               Enddo
               KvdLH2Add(iovh,idum) = h2add
               KvdLQ2Add(iovh,idum) = q2add
123            continue
            else
               h2add = KvdLH2Add(iovh,idum)
               q2add = KvdLQ2Add(iovh,idum)
            Endif
            h2add = h2add / pi_derde32
            Opbolling = Opbolling * (1-h2add)    ! opbolling in m
!GPTVJun2014Debiet    = Debiet * ( 1.0 - ( q2add / pi_kwadraat8) ) ! debiet in m/dag
!corrected to:
            Debiet    = Debiet * ( q2add / pi_kwadraat8) ! debiet in m/dag
            if (idum .eq. 1) then
              KvdLeurnul(iovh)%Kopbollinglast = Opbolling
              KvdLeurnul(iovh)%Kdebietlast = Debiet
            endif
          Endif
          KvdLeur(iovh,KvdLDimensie)%Kopbolling = KvdLeur(iovh,KvdLDimensie)%Kopbolling + opbolling
          KvdLeur(iovh,KvdLDimensie)%Kdebiet    = KvdLeur(iovh,KvdLDimensie)%Kdebiet + debiet
          If (idebug .gt. 0 .and. h2add .ne. 0.) Then
            write(idebug,'(A,5E13.5)') ' h2add opbolling debiet, KvdLeur(.,KvdlDim)%Opbolling en %debiet', &
                                         h2add, opbolling, debiet, KvdLeur(iovh,KvdLDimensie)%Kopbolling, KvdLeur(iovh,KvdLDimensie)%Kdebiet
          Endif
        Enddo
!add Kopbollingnul en KDebietNul: cumulatieve opbolling van oudere neerslagen
        KvdLeur(iovh,KvdLDimensie)%Kopbolling = KvdLeur(iovh,KvdLDimensie)%Kopbolling + KvdLeurnul(iovh)%Kopbollingnul
        KvdLeur(iovh,KvdLDimensie)%Kdebiet = KvdLeur(iovh,KvdLDimensie)%Kdebiet + KvdLeurnul(iovh)%Kdebietnul
        if (idebug .ne. 0) Then
           write(idebug,*) ' totalen plus Kopbollingnul en Kdebietnul'
           write(idebug,'(4E13.5)') KvdLeur(iovh,KvdLDimensie)%Kopbolling, KvdLeur(iovh,KvdLDimensie)%Kdebiet, &
                                     KvdLeurnul(iovh)%Kopbollingnul, KvdLeurnul(iovh)%Kdebietnul
        Endif
!       afschatting restterm Q; uit analyse Toine: relatie tussen n=99 en tijd (in dagen) voor de ontbrekende Q
!       schalen voor reservoircoefficient, relatie is afgeleid voor j=5.580651
        Resttijd = ( float (KvdLDimensie) * timeSettings%timestepSize ) / float (NrsDay) ! tijd in dagen
        if (idebug .ne. 0)  write(idebug,*) ' Resttijd                 ', Resttijd
        Resttijd = Resttijd * 5.580651 / ReservoirCoeff(iovh)
        if (idebug .ne. 0)  write(idebug,*) ' Resttijd geschaald voor j', Resttijd
        Idum = RestTermNr * 0.85   ! initieel uitgaan van n=99 en t=50 dagen
        CALL RR_D_INTERP (RestTermNr, RestTermTijd, RestTermFactor, Resttijd, ActualRestTermFactor,idum)
        KvdLeurRest(iovh)%Kdebiet = ActualRestTermFactor * KvdLeurRest(iovh)%neerslag
        if (idebug .ne. 0)  write(idebug,*) ' Found index              ', idum
        if (idebug .ne. 0)  write(idebug,*) ' ActualRestTermFactor     ', ActualRestTermFactor
        if (idebug .ne. 0)  write(idebug,*) ' KvdLeur restterm debiet  ', KvdLeurRest(iovh)%KDebiet
! zet debiet Q2O
! formule van Jmodel bepaalt cumulatieve q, dus hier corrigeren
        if (idebug .ne. 0) Then
           write(idebug,'(A,3E13.5)') ' KvdL bepaling Q2O: ',KvdLeur(iovh,KvdLDimensie)%Kdebiet, &
                                                  KvdLeur(iovh,KvdLDimensie-1)%Kdebiet, AreaGwComp(iovh)
!                                                  KvdLeur(iovh,KvdLDimensie-1)%Kdebiet, Areaoh(iovh)
        endif
        Q2O(IOVH)  = (KvdLeur(iovh,KvdLDimensie)%KDebiet-KvdLeur(iovh,KvdLDimensie-1)%KDebiet+KvdLeurRest(iovh)%Kdebiet)  &
                                                        * AreaGwComp(iovh) / NrsDay ! debiet in m3/s

        if (idebug .ne. 0)  write(idebug,*) ' KvdLeurNul OpbollingNul  ', KvdLeurnul(iovh)%KOpbollingNul
        if (idebug .ne. 0)  write(idebug,*) ' KvdLeurNul OpbollingLast ', KvdLeurnul(iovh)%KOpbollinglast
        if (idebug .ne. 0)  write(idebug,*) ' KvdLeurNul DebietNul     ', KvdLeurnul(iovh)%KDebietNul
        if (idebug .ne. 0)  write(idebug,*) ' KvdLeurNul DebietLast    ', KvdLeurnul(iovh)%KDebietlast
        if (idebug .ne. 0)  write(idebug,*) ' KvdLeurNul PeilNul       ', KvdLeurnul(iovh)%peilnul
        if (idebug .ne. 0)  write(idebug,*) ' KvdLeurNul Openwaterlevel', KvdLeurnul(iovh)%Openwaterlevel
        if (idebug .ne. 0)  write(idebug,*) ' KvdLeur neerslag  KvdLDim', KvdLeur(iovh,KvdLDimensie)%neerslag
        if (idebug .ne. 0)  write(idebug,*) ' KvdLeur opbolling KvdLDim', KvdLeur(iovh,KvdLDimensie)%Kopbolling
        if (idebug .ne. 0)  write(idebug,*) ' KvdLeur debiet    KvdLDim', KvdLeur(iovh,KvdLDimensie)%KDebiet
        if (idebug .ne. 0)  write(idebug,*) ' KvdLeur neerslag  KvdLDim-1', KvdLeur(iovh,KvdLDimensie-1)%neerslag
        if (idebug .ne. 0)  write(idebug,*) ' KvdLeur opbolling KvdLDim-1', KvdLeur(iovh,KvdLDimensie-1)%Kopbolling
        if (idebug .ne. 0)  write(idebug,*) ' KvdLeur debiet    KvdLDim-1', KvdLeur(iovh,KvdLDimensie-1)%KDebiet
        if (idebug .ne. 0)  write(idebug,*) ' Q2O(iovh)                ', Q2O(iovh)
        if (idebug .ne. 0)  write(idebug,*) ' end sub KrayenhoffvdLeur'

   Return
  END subroutine KrayenhoffvdLeur



  Subroutine Ernst (Inode, Iovh, Ibnd, Iow, IPluv, Idebug, Peil, Qin, TotalScurveSurfaceOutflow, MinDepthCF)

! *********************************************************************
! Subroutine berekent Ernst uitstroming uit bodem onverhard gebied Iovh naar open water met de formule van Ernst
! Stroming kan zowel drainage als infiltratie zijn
! Houdt rekening met evt. S curve voor het oppervlak;
! NB  Voor de S curve kan een deel van de uitstroming uit de bodem al afstroming over oppervlak zijn!
! *********************************************************************
!   iovh   = arrayindex in arrays voor onverhard gebied
!   idebug = debug file
!   peil   = open water peil of peil op de rand in m
!   Qin    = Verticale instroming  in m3/s
! *********************************************************************

   Integer             Inode, Iovh, Ibnd, Iow, IPluv, idebug, i
   Double precision    Peil, Qin, TotalScurveSurfaceOutflow
   real                TmpAreaOw
   Double precision    Ratio, MaxMm, ActMm
   Double precision    OnSurface

! Local arrays for interpolation
      Real PeilArray(6), AreaArray(6)  ! aanname dat NVAL=6 zoals in Conf_Arr.

!Scurve
   Integer Ipoint
   Double precision    Qin2

   Double precision dH1, dH2, dH3, dH4, dH5, dQ1, dQ2, dQ3, dQ4, dQ5, q1, q2, q3, q4, q5

   Double precision MinDepthCF
   Double precision Delta_BndPeil
   Double precision OldQ2O

       If (UseScurve(iovh) .eq. 0) then
!       Ernst, zonder S curve
        if (AreaOh(iovh) .gt. 0.001) then
           OnSurface = BoLnd0 (iovh) / AreaOh(iovh)
        endif
        Call SetDeltaH (Idebug, Peil, Iovh, DH1, Dh2, DH3, DH4, Dq1, Dq2, Dq3, Dq4, Qin, OnSurface)
        Call ErnstFormule (Q1, AreaGwComp(iovh), ErnstResistance(iovh,5), DH1, timeSettings%timestepSize, idebug)
        Call ErnstFormule (Q2, AreaGwComp(iovh), ErnstResistance(iovh,4), DH2, timeSettings%timestepSize, idebug)
        Call ErnstFormule (Q3, AreaGwComp(iovh), ErnstResistance(iovh,3), DH3, timeSettings%timestepSize, idebug)
        Call ErnstFormule (Q4, AreaGwComp(iovh), ErnstResistance(iovh,2), DH4, timeSettings%timestepSize, idebug)

        if (idebug .ne. 0) write(iDebug, *) 'Q1, Q2, Q3, Q4, dh1, gwl0(iovh)'
        if (idebug .ne. 0) write(iDebug, *) Q1, Q2, Q3, Q4, dh1

       ! total soil outflow, omzetten naar m3/s
        Q2O(IOVH) = Q1 + Q2 + Q3 + Q4
        Q2O(IOVH) = Q2O(IOVH) / timeSettings%timestepSize

        ! correctie in geval van infiltratie
        IF (Q2O(IOVH) .LT.  0) THEN
          if (idebug .ne. 0) WRITE(IDEBUG,*) ' Q2O negative'
          if (idebug .ne. 0) WRITE(IDEBUG,*) ' Now with infiltration'
          Call ErnstFormule (Q2O(iovh), AreaGwComp(iovh), ErnstResistance(iovh,6), &
                                                          Gwl0(iovh)-Peil, timeSettings%timestepSize, idebug)
          Q2O(IOVH) = Q2O(IOVH) / timeSettings%timestepSize
! 1 sept 2003: als gwl0 >= maaiveld, dan geen infiltratie vanuit ow/bnd mogelijk
          if (NoInfiltrationWhileGwOnSurface) then
            if (Gwl0(iovh) .ge. LvlOhMx(iovh))  then
               Q2O(iovh) = 0.0
            elseif (Gwl0(iovh) - LvlOhMx(iovh) .gt. -0.1) then
!              2 sept 2003: als gwl0 vlak onder maaiveld, dan een quasi volumecheck
               if (UnSatZoneOption .ge. 1) then
                  Q2O(iovh) = max (Q2O(iovh), &
                                   -1.* AreaGwComp(iovh) * (LvlOhMx(iovh)-Gwl0(iovh))*0.01 / timeSettings%timestepSize )
               else
                  Q2O(iovh) = max (Q2O(iovh), &
                                    -1.* AreaGwComp(iovh) * (LvlOhMx(iovh)-Gwl0(iovh))*BergC(iovh)/ timeSettings%timestepSize )
               endif
            endif
          endif
        ENDIF

       Elseif (UseSCurve(iovh) .eq. 1) then
         Ratio = 1.0
         MaxMm = 1.0
         ActMm = 1.0
         OnSurface = 1.0
         if (AreaOh(iovh) .gt. 0.001) then
           Ratio = AreaGwComp(iovh) / AreaOh(iovh)
           MaxMm = BMaxOl(iovh) / AreaOh(iovh)       ! here in meters, not in mm
           ActMm = BoLnd (iovh) / AreaOh(iovh)       ! here in meters, not in mm
           OnSurface = BoLnd0 (iovh) / AreaOh(iovh)
           if (idebug .ne. 0) write(Idebug,*) ' Qin Ratio Maxmm ActMM',Qin,Ratio, MaxMm,ActMm
         endif
         Q2O (iovh) = 0.0
         Do IPoint=1,UseUnpavedSCurve
           Call SetDeltaHSCurve (Idebug, Peil, Iovh, Ipoint, DH1, Dh2, DH3, DH4, DH5, Dq1, Dq2, Dq3, Dq4, Dq5, &
                                                                            Qin, Qin2, MaxMm, ActMm, OnSurface)
! Voor Soil outflow (Q1 tm Q4): gebruik Scurve arealen * ratio AreaGwComp/AreaOh;
!                               dus aanname dat extra areaal zelfde verdeling heeft
! Voor Surface runoff Q5      : alleen Scurve areaal gebruiken

           Call ErnstFormule (Q1, AreaScurve(iovh,ipoint)%Area *Ratio, ErnstResistance(iovh,5), &
                                                           DH1, timeSettings%timestepSize, idebug)
           Call ErnstFormule (Q2, AreaScurve(iovh,ipoint)%Area *Ratio, ErnstResistance(iovh,4), &
                                                           DH2, timeSettings%timestepSize, idebug)
           Call ErnstFormule (Q3, AreaScurve(iovh,ipoint)%Area *Ratio, ErnstResistance(iovh,3), &
                                                           DH3, timeSettings%timestepSize, idebug)
           Call ErnstFormule (Q4, AreaScurve(iovh,ipoint)%Area *Ratio, ErnstResistance(iovh,2), &
                                                           DH4, timeSettings%timestepSize, idebug)
! extra zone Dh5, Dq5, voor GWL0 boven dit deel van maaiveld Scurve; debiet Q5 wordt al bij afstroming over oppervlak gerekend
! dit deel op oppervlak, mits boven het maximum, stroomt af met Ernst-weerstand voor het oppervlak
           If (Dh5 .gt. 0) then
             Call ErnstFormule (Q5, AreaScurve(iovh,ipoint)%Area, ErnstResistance(iovh,1), &
                                                           DH5, timeSettings%timestepSize, idebug)
             Q5 = max (0.0d0, Q5)
           else
             Q5 = 0.0
           endif

           if (idebug .ne. 0) write(iDebug, *) 'Q1, Q2, Q3, Q4, Q5, dh4, dh5, gwl0(iovh)'
           if (idebug .ne. 0) write(iDebug, *) Q1, Q2, Q3, Q4, Q5, dh4, dh5, gwl0(iovh)

          ! soil outflow per part, omzetten naar m3/s
           AreaScurve(iovh,ipoint)%SoilOutflow = Q1 + Q2 + Q3 + Q4
           AreaScurve(iovh,ipoint)%SoilOutflow =  AreaScurve(iovh,ipoint)%SoilOutflow / timeSettings%timestepSize

          ! afstroming oppervlak per part, omzetten naar m3/s
           AreaScurve(iovh,ipoint)%SurfaceOutflow = Q5 / timeSettings%timestepSize

           ! correctie in geval van infiltratie
           IF ( AreaScurve(iovh,ipoint)%SoilOutflow .LT.  0) THEN
             if (idebug .ne. 0) WRITE(IDEBUG,*) ' Q2O negative'
             if (idebug .ne. 0) WRITE(IDEBUG,*) ' Now with infiltration'
             If (GWL0(IOVH) .LT. AreaScurve(iovh,ipoint)%Level ) Then
!              Ernst op verschil GWL - peil
!              Soil Infiltration op SCurve areaal * Ratio
               Call ErnstFormule ( AreaScurve(iovh,ipoint)%SoilOutflow, AreaScurve(iovh,ipoint)%Area *Ratio, &
                                   ErnstResistance (iovh,6), Gwl0(iovh)-Peil, timeSettings%timestepSize, Idebug)
             Else
!              Ernst op verschil Level - peil
!              Soil Infiltration op SCurve areaal * Ratio   ? Echter hier GWL al boven mv?
               Call ErnstFormule ( AreaScurve(iovh,ipoint)%SoilOutflow, AreaScurve(iovh,ipoint)%Area *Ratio, &
                                   ErnstResistance(iovh,6), AreaScurve(iovh,ipoint)%Level-Peil, timeSettings%timestepSize,Idebug)
! 1 sept 2003: als gwl0 >= maaiveld, dan geen infiltratie vanuit ow/bnd mogelijk voor dit deel vd Scurve
               if (NoInfiltrationWhileGwOnSurface) then
                  if (Gwl0(iovh) .ge. AreaScurve(iovh,ipoint)%Level) then
                    AreaScurve(iovh,ipoint)%SoilOutflow = 0.0
                  elseif (Gwl0(iovh) - AreaScurve(Iovh,ipoint)%Level .gt. -0.1) then
                    if (UnSatZoneOption .ge. 1) then
                      AreaScurve(iovh,ipoint)%SoilOutflow = max (AreaScurve(iovh,ipoint)%SoilOutflow, &
                                       -1.* AreaGwComp(iovh) * (AreaScurve(iovh,ipoint)%Level-Gwl0(iovh)) *0.01 )
                    else
                      AreaScurve(iovh,ipoint)%SoilOutflow = max (AreaScurve(iovh,ipoint)%SoilOutflow, &
                                       -1.* AreaGwComp(iovh) * (AreaScurve(iovh,ipoint)%Level-Gwl0(iovh))*BergC(iovh) )
                    endif
                  endif
               endif
             Endif
             AreaScurve(iovh,ipoint)%SoilOutflow =  AreaScurve(iovh,ipoint)%SoilOutflow / timeSettings%timestepSize
           ENDIF

 ! total soil outflow Q2O
           Q2O(IOVH) = Q2O(IOVH) +  AreaScurve(iovh,ipoint)%SoilOutflow

 ! total Surface Outflow bij Scurve
           TotalScurveSurfaceOutflow = TotalScurveSurfaceOutflow + AreaScurve(iovh,ipoint)%SurfaceOutflow
         Enddo

       Endif

! Volumecheck bij infiltratie, en directe koppeling aan rand: voorkom droogval in CF
       if (idebug .ne. 0) write(Idebug,*) ' Q2O(iovh) voor checks ', Q2o(iovh)
       If (Q2O(Iovh) .lt. 0 .and. Ibnd .gt. 0) then
         ! boundary areaal=BNDPAR(ibnd,5), peil=PEIL, boundary depth=BndPar(ibnd,6)
            if (idebug .ne. 0) THEN
              WRITE(IDEBUG,*) ' Voor check bnd-areaal', Q2O(IOVH)
              WRITE(IDEBUG,*) ' AreaBnd, ovhlevel, bndlevel',BNDPAR(IBND,5), LVLOHMx(iovh), PEIL
              WRITE(IDEBUG,*) ' Depth at boundary          ',BNDPAR(IBND,6)
            ENDIF
! check als diepte bekend in His file, dat niet meer dan droogval-minimumdepth wordt onttrokken
            If (BndPar(Ibnd,6) .lt. 998.0) then
               Q2O(iovh) = max (Q2O(iovh),  -1. * BndPar(Ibnd,5) * (BndPar(Ibnd,6)-MinDepthCF) / TimeSettings%timestepSize )
               If (BndPar(Ibnd,6) .lt. MinDepthCF) Q2O(iovh) = 0.0
               If (BndPar(Ibnd,6) .lt. 0.0) Q2O(iovh) = 0.0
            Else
              ! Depth not in HIS file, so no check possible
            Endif
       ElseIf (Q2O(Iovh) .lt. 0 .and. Ipluv .gt. 0) then
! Volumecheck bij infiltratie, en directe koppeling aan rand: voorkom droogval in SF
! check als diepte bekend in His file, dat niet meer dan droogval-minimumdepth wordt onttrokken
            If (SbkLvlPluv(ipluv) .gt. -999.) then
               Q2O(iovh) = max (Q2O(iovh),  -1. * SbkAreaPluv(ipluv) * (SbkDepthPluv(Ipluv)-MinDepthCF) / TimeSettings%timestepSize )
               If (SbkDepthPluv(Ipluv) .lt. MinDepthCF) Q2O(iovh) = 0.0
               If (SbkDepthPluv(Ipluv) .lt. 0.0) Q2O(iovh) = 0.0
            Else
              ! Depth not in HIS file, so no check possible
            Endif
       Elseif (Q2O(Iovh) .gt. 0 .and. Iow .gt. 0) then
! n.a.v. ARS 8842 convergentieprobleem KJ - Capsim: Volumecheck bij drainage naar open water
            OldQ2O = Q2O(iovh)
            Do i=1,NVal
               PeilArray(i) = PeilOw(i,iow)
               AreaArray(i) = AreaOw(i,iow)
            Enddo
            CALL RR_INTERP (NVAL, PEILArray, AREAArray, Sngl(PEIL), TmpAreaOw, OwLastInterpIndex(iow))
            if (idebug .ne. 0) THEN
              WRITE(IDEBUG,*) ' NVAL, PEIL, Lastinterpindx iow,', NVAL, PEIL, OWLastInterpIndex(iow), iow
              WRITE(IDEBUG,'(A,6F10.3)') ' PEILOW array ', (PeilOw(i,iow),i=1,6)
              WRITE(IDEBUG,'(A,6F10.3)') ' AreaOW array ', (AreaOw(i,iow),i=1,6)
              WRITE(IDEBUG,*) ' Voor Volumecheck Berekende drainage ', Q2O(IOVH)
              WRITE(IDEBUG,*) ' tmpAreaOw, gwlevel, owlevel',tmpAreaOw, GWL0(iovh), LVLOW0(iow)
            ENDIF
            Q2O(iovh) = min (Q2O(iovh), tmpAreaOw * (GWL0(iovh)-lvlOw0(iOW)) / TimeSettings%timestepSize  )
            Q2O(iovh) = max (Q2O(iovh), 0.0d0)
            if (idebug .ne. 0) Write(IDEBUG,*) ' Na VolumeCheck Berekende drainage ', Q2O(IOVH)
            Call UnpavedVolumeCheckActive (OldQ2O, Q2O(iovh), Inode, ibnd, iow)
! end n.a.v. ARS 8842
       Elseif (Q2O(Iovh) .gt. 0 .and. Ibnd .gt. 0) then
!  ARS 11610 Check of door deze lozing de peilstijging in CF niet boven gws uitkomt
         If (FixArs11610) then
           OldQ2O = Q2O(iovh)
           if (idebug .ne. 0) write(Idebug,*) ' Ernst VolumeCheck FixArs11610'
           if (idebug .ne. 0) write(Idebug,*) ' BndPar5', BndPar(ibnd,5)
           if (idebug .ne. 0) write(Idebug,*) ' VolCheckFactor',UnpVolumeCheckFactorToCF
           Delta_bndpeil = Q2O(iovh)*TimeSettings%timestepSize / Bndpar(ibnd,5) / UnpVolumeCheckFactorToCF
           Delta_bndpeil = Delta_bndpeil + Peil
           if (Delta_bndpeil .gt. Gwl0(iovh)) then
              if (idebug .ne. 0) write(Idebug,*) ' BergC ',BergC(iovh)
              if (idebug .ne. 0) write(Idebug,*) ' Gwl0  ',Gwl0(iovh)
              if (idebug .ne. 0) write(Idebug,*) ' Peil  ',Peil
              Q2O(iovh) = min (Q2O(iovh), (Gwl0(iovh) - Peil)*BergC(iovh) * BndPar(Ibnd,5) * &
                                              UnpVolumeCheckFactorToCF / TimeSettings%TimestepSize)
              Q2O(iovh) = max (Q2O(iovh), 0.0d0)
           endif
           if (idebug .ne. 0) Write(IDEBUG,*) ' Na VolumeCheck Berekende drainage ', Q2O(IOVH)
           Call UnpavedVolumeCheckActive (OldQ2O, Q2O(iovh), Inode, ibnd, iow)
         Endif
       Elseif (Q2O(Iovh) .gt. 0 .and. Ibnd .gt. 0) then
!  ARS 11610 Check of door deze lozing de peilstijging in SF niet boven gws uitkomt
           ! no volumecheck added
       Endif

   Return
  END subroutine Ernst




  Subroutine ErnstOppervlak (Inode, Iovh, Idebug, Peil, Iow, Ibnd, ipluv)

! *********************************************************************
! *** Subroutine berekent Ernst afstroming over oppervlak onverhard gebied Iovh
! ***  naar open water Iow of randknoop Ibnd
! ***  met de Ernst formule
! *** Het rekening houden met evt. Scurve gebeurt al bij de grondwateruitstroming (nl. daar kan de GWL al boven het maaiveld komen)
! *********************************************************************
!   iovh   = arrayindex in arrays voor onverhard gebied
!   idebug = debug file
!   peil   = open water peil of peil op de rand
!   iow    = index open water
!   ibnd   = index boundary
! *********************************************************************

   Integer Inode, Iovh, idebug, Iow, Ibnd, Ipluv, i
   Double precision    Peil

   Real                TmpAreaOw
   Double precision    Rdum, Qin, Q0In, QInNu, BergingLand
   Double precision    OldQ1O

! Local arrays for interpolation
      Real PeilArray(6), AreaArray(6)  ! aanname dat NVAL=6 zoals in Conf_Arr.

!     Ernst afstroming oppervlak, zonder S curve
! ARS 25328
      BergingLand = BoLnd(IOVH) / AreaOh(Iovh)

      if (idebug .ne. 0) then
          WRITE(IDEBUG,*) ' ErnstOppervlak', iovh
          WRITE(IDEBUG,*) ' BoLnd  BMAXOL ', BOLND(IOVH), BMAXOL(iovh)
          WRITE(IDEBUG,*) ' BoLnd0        ', BOLND0(IOVH)
          WRITE(IDEBUG,*) ' Peil          ', Peil
          WRITE(IDEBUG,*) ' LvlOhMx       ', LvlOhMx(iovh)
          WRITE(IDEBUG,*) ' BergingLand   ', BergingLand
      endif
      Q1O(IOVH) = 0.0
      If ( (BOLND(IOVH) .GT. BMAXOL(IOVH) .or. BOLND0(iovh) .gt. BMAXOL(iovh)) .AND. PEIL .LE. LVLOHMx(IOVH)+BergingLand) Then
      ! boven max.berging op land ook met Ernst-weerstand
         QIN  = MAX (0.0d0, BOLND(IOVH) - BMAXOL(IOVH)) / timeSettings%timestepSize
         Q0IN = MAX (0.0d0, BOLND0(IOVH) - BMAXOL(IOVH)) / timeSettings%timestepSize
         if (idebug .ne. 0) then
            WRITE(IDEBUG,*) ' Qin     ', Qin
            WRITE(IDEBUG,*) ' Q0IN    ', Q0In
            WRITE(IDEBUG,*) ' Q1O     ', Q1O(iovh)
         endif
         QINNU= QIN-Q0IN
         Call ErnstFormule (Q1O(iovh), Q0In*TimeSettings%TimestepSize, ErnstResistance(Iovh,1), &
                                                             1.0D0, TimeSettings%timestepSize, idebug)

!GP Aanpassing Oct97: ook hier checken op areaal open water/boundary en evt. beperking opp. afstroming (volumecheck)
         IF (IOW .GT. 0) THEN
            Do i=1,NVal
               PeilArray(i) = PeilOw(i,iow)
               AreaArray(i) = AreaOw(i,iow)
            Enddo
            CALL RR_INTERP (NVAL, PEILArray, AREAArray, Sngl(PEIL), TmpAreaOw, OwLastInterpIndex(iow))
            if (idebug .ne. 0) THEN
              WRITE(IDEBUG,*) ' Opp. Afstroming', Q1O(IOVH)
              WRITE(IDEBUG,*) ' Bolnd, Bolnd0 ', Bolnd(IOVH), Bolnd0(iovh)
              WRITE(IDEBUG,*) ' Q0in          ', Q0in
              WRITE(IDEBUG,*) ' Ernst resistance ', ErnstResistance(iovh,1)
              WRITE(IDEBUG,*) ' Voor check ow-areaal', Q1O(IOVH)
              WRITE(IDEBUG,*) ' tmpAreaOw, ovhlevel, owlevel',tmpAreaOw, LVLOHMx(iovh), LVLOW0(iow)
            ENDIF
            OldQ1O = Q1O(iovh)
!           Q1O(iovh) = min (Q1O(iovh), tmpAreaOw * (LVLOHMx(iovh)+BergingLand -lvlOw0(iOW)) )
! VolumeCheckFactor ontbrak nog bij Ernst! sep2021  Issue SOBEK-51020
            Q1O(iovh) = min (Q1O(iovh), tmpAreaOw * (LVLOHMx(iovh)+BergingLand -lvlOw0(iOW)) * UnpVolumeCheckFactorToOW) !! ARS 15351, 15358 (Feb2006), 15464 April 2006
            Call UnpavedVolumeCheckActive (OldQ1O, Q1O(iovh), Inode, ibnd, iow)
         ELSEIF (IBND .GT. 0) THEN
         ! boundary areaal=BNDPAR(ibnd,5), peil=PEIL
            if (idebug .ne. 0) THEN
              WRITE(IDEBUG,*) ' Voor check bnd-areaal', Q1O(IOVH)
              WRITE(IDEBUG,*) ' AreaBnd, ovhlevel, bndlevel',BNDPAR(IBND,5), LVLOHMx(iovh), PEIL
            ENDIF
            OldQ1O = Q1O(iovh)
!           Q1O(iovh) = min (Q1O(iovh), BNDPAR(IBND,5) * (LVLOHMx(iovh)+BergingLand -PEIL) )
! VolumeCheckFactor ontbrak nog bij Ernst! sep2021  Issue SOBEK-51020
            Q1O(iovh) = min (Q1O(iovh), BNDPAR(IBND,5) * (LVLOHMx(iovh)+BergingLand -PEIL) * UnpVolumeCheckFactorToCF)  !! ARS 15351, 15358, Feb2006
            Call UnpavedVolumeCheckActive (OldQ1O, Q1O(iovh), Inode, ibnd, iow)
         ELSEIF (IPluv .GT. 0) THEN
            ! no vlolume check added
         ENDIF
! Afstroming oppervlak positief en in m3/s; nieuwe berging op land
         Q1O(iovh) = max (Q1O(iovh), 0.0d0)
         Q1O(IOVH) = Q1O(IOVH) / timeSettings%timestepSize
         BOLND(IOVH) = BOLND0(IOVH) - Q1O(IOVH) * timeSettings%timestepSize &
                      - VO(IOVH) + RO (IOVH) - INO(IOVH) + IrrigationSupply(iovh) * TimeSettings%TimestepSize

! Check dat berging op land niet negatief wordt door de berekende surface runoff
! Kleiner dan max. berging >0 mag wel (door verdamping)
         If (BoLnd(iovh) .lt. 0.0) then
            RDum = -1. * BoLnd(iovh) / TimeSettings%TimestepSize
            If (RDum .gt. Q1O(iovh)) then
               Q1O(iovh) = 0.0
               BOLND(IOVH) = BOLND0(IOVH) - VO(IOVH) + RO (IOVH) - INO(IOVH) + IrrigationSupply(iovh) * TimeSettings%TimestepSize
            Else
               Q1O(iovh) = Q1O(iovh)-RDUM
               BoLnd(Iovh) = 0.0
            Endif
         Endif
      Endif

   Return
  END subroutine ErnstOppervlak


  Subroutine SetDeltaH (Idebug, Peil, Iovh, DH1, Dh2, DH3, DH4, Dq1, Dq2, Dq3, Dq4, Qin, OnSurface)
! Set the DH and DQ values for Hellinga de Zeeuw and Ernst computations, No Scurve

  Integer Idebug, Iovh, i, D_IfReal, idum
  Double precision    Peil, Qin
  Double precision    DH1, Dh2, DH3, DH4, Dq1, Dq2, Dq3, Dq4
  Double precision    OnSurface, GwLevel

  DH1 = 0
  DH2 = 0
  DH3 = 0
  DH4 = 0
  DQ1 = 0
  DQ2 = 0
  DQ3 = 0
  DQ4 = 0

! ARS 10084
  GwLevel = GWL0(Iovh)
!  If (FixArs10084) then
  if (idebug .ne. 0) Then
      write(idebug,*) ' FixArs10084', FixArs10084
      write(idebug,*) ' GwLevel    ', GwLevel
      write(idebug,*) ' LvlOh      ', LvlOh(iovh)
  Endif
! more precise check after issue Toine, March 2015
  If (FixArs10084) then
     idum = D_IfReal (GwLevel, LvlOh(iovh), 1D-6)
     if (idebug .ne. 0) write(idebug,*) ' idum ', idum
     if (Idum .ge. 0) then
        ! GwLevel >= LvlOh(iovh)
        GwLevel = GwLevel + OnSurface
        if (idebug .ne. 0) write(idebug,*) ' GwLevel    ', GwLevel
     else
        ! GwLevel < LvlOh(iovh)
        ! do nothing
     Endif
  Endif

  If (DrainageDeltaH .eq. 0) then
!    Gestapelde systemen
     IF (GwLevel .LT. LVLDRN(IOVH,1)) THEN
      DH1 = GwLevel - PEIL
      DQ1 = QIN
     ELSEIF (GwLevel .LT. LVLDRN(IOVH,2)) THEN
      DH1 = MAX (0.0d0, LVLDRN(IOVH,1) - PEIL)
      DH2 = GwLevel - MAX (LVLDRN(IOVH,1), PEIL)
      DQ2 = QIN
     ELSEIF (GwLevel .LT. LVLDRN(IOVH,3)) THEN
      DH1 = MAX (0.0d0, LVLDRN(IOVH,1) - PEIL)
      DH2 = MAX (0.0d0, LVLDRN(IOVH,2) - MAX(LVLDRN(IOVH,1),PEIL) )
      DH3 = GwLevel - MAX (LVLDRN(IOVH,2), PEIL)
      DQ3 = QIN
     ELSE
      DH1 = MAX (0.0d0, LVLDRN(IOVH,1) - PEIL)
      DH2 = MAX (0.0d0, LVLDRN(IOVH,2) - MAX(LVLDRN(IOVH,1),PEIL) )
      DH3 = MAX (0.0d0, LVLDRN(IOVH,3) - MAX(LVLDRN(IOVH,2),PEIL) )
      DH4 = GwLevel - MAX (LVLDRN(IOVH,3), PEIL)
      DQ4 = QIN
     ENDIF
  Else
!    Parallelle systemen
     DH1 = GwLevel - PEIL
     IF (GwLevel .LT. LVLDRN(IOVH,1)) THEN
      DQ1 = QIN
     ELSEIF (GwLevel .LT. LVLDRN(IOVH,2)) THEN
      DH2 = GwLevel - MAX (LVLDRN(IOVH,1), PEIL)
      DQ2 = QIN
     ELSEIF (GwLevel .LT. LVLDRN(IOVH,3)) THEN
      DH2 = GwLevel - MAX (LVLDRN(IOVH,1), PEIL)
      DH3 = GwLevel - MAX (LVLDRN(IOVH,2), PEIL)
      DQ3 = QIN
     ELSE
      DH2 = GwLevel - MAX (LVLDRN(IOVH,1), PEIL)
      DH3 = GwLevel - MAX (LVLDRN(IOVH,2), PEIL)
      DH4 = GwLevel - MAX (LVLDRN(IOVH,3), PEIL)
      DQ4 = QIN
     ENDIF
  Endif
  if (LvlDrn(Iovh,1) .ge. LvlOh(iovh)) DH2 = 0.0
  if (LvlDrn(Iovh,2) .ge. LvlOh(iovh)) DH3 = 0.0
  if (LvlDrn(Iovh,3) .ge. LvlOh(iovh)) DH4 = 0.0
  if (idebug .ne. 0) Then
    write(iDebug,'(A,3G16.7)')  ' LvlDrn  ', (LvlDrn(Iovh,i),i=1,3)
    write(iDebug,'(A,3G16.7)')  ' LvlOh   ', LvlOh(Iovh)
    write(iDebug,'(A,3G16.7)')  ' ow-peil gwlevel  OnSurface', peil, Gwl0(iovh), OnSurface
    write(iDebug,'(A,4G16.7)')  ' SetDeltaH DH1 tm Dh4: ', Dh1, Dh2, Dh3, Dh4
  Endif

   Return
  END subroutine SetDeltaH



  Subroutine SetDeltaHSCurve (Idebug, Peil, Iovh, Ipoint, DH1, Dh2, DH3, DH4, DH5, Dq1, Dq2, Dq3, Dq4, Dq5, &
                                                          Qin, Qin2, MaxMm, ActMm, OnSurface)
! Set the DH and DQ values for Hellinga de Zeeuw and Ernst computations, With Scurve

  Integer             Idebug, Iovh, ipoint, i, D_IfReal, idum
  Double precision    Peil, DH1, Dh2, DH3, DH4, DH5, Dq1, Dq2, Dq3, Dq4, Dq5, Qin, MaxMm, ActMm, OnSurface
  Double precision    Qin2, GwLevel

  DH1 = 0
  DH2 = 0
  DH3 = 0
  DH4 = 0
  DH5 = 0
  DQ1 = 0
  DQ2 = 0
  DQ3 = 0
  DQ4 = 0
  DQ5 = 0
! ARS 10084
  GwLevel = GWL0(Iovh)
  If (FixArs10084) then
     idum = D_IfReal (GwLevel, AreaScurve(iovh,ipoint)%Level, 1D-6)
     if (Idum .ge. 0) then
        ! GwLevel >= AreaScurve(iovh,ipoint)%Level
        GwLevel = GwLevel + OnSurface
        if (idebug .ne. 0) write(idebug,*) ' GwLevel    ', GwLevel
     else
        ! GwLevel < LvlOh(iovh)
        ! do nothing
     Endif
  Endif

!  If (FixArs10084 .and. (GWL0(Iovh) .ge. AreaSCurve(Iovh,ipoint)%Level) ) then
!     GwLevel = GwLevel + OnSurface
!  Endif

! Qin2 naar rato van S curve arealen verdelen
  Qin2 = AreaScurve(iovh,ipoint)%Area / AreaOh (iovh) * Qin
  If (DrainageDeltaH .eq. 0) Then
!    Gestapelde systemen
     IF (GwLevel .LT. AreaScurve(iovh,ipoint)%AlfaLevels(1) .and. GwLevel .LT. AreaScurve(iovh,ipoint)%Level ) Then
       DH1 = GwLevel - PEIL
       DQ1 = QIN2
     ELSEIF (GwLevel .LT. AreaScurve(iovh,ipoint)%AlfaLevels(2) .and. GwLevel .LT. AreaScurve(iovh,ipoint)%Level ) Then
       DH1 = MAX (0.0d0, AreaScurve(iovh,ipoint)%AlfaLevels(1) - PEIL)
       DH2 = GwLevel - MAX (AreaScurve(iovh,ipoint)%AlfaLevels(1), PEIL)
       DQ2 = QIN2
     ELSEIF (GwLevel .LT. AreaScurve(iovh,ipoint)%AlfaLevels(3) .and. GwLevel .LT. AreaScurve(iovh,ipoint)%Level ) Then
       DH1 = MAX (0.0d0, AreaScurve(iovh,ipoint)%AlfaLevels(1) - PEIL)
       DH2 = MAX (0.0d0, AreaScurve(iovh,ipoint)%AlfaLevels(2) - MAX(AreaScurve(iovh,ipoint)%AlfaLevels(1),PEIL) )
       DH3 = GwLevel - MAX (AreaScurve(iovh,ipoint)%AlfaLevels(2), PEIL)
       DQ3 = QIN2
     ELSEIF (GwLevel .LT. AreaScurve(iovh,ipoint)%Level ) Then
       DH1 = MAX (0.0d0, AreaScurve(iovh,ipoint)%AlfaLevels(1) - PEIL)
       DH2 = MAX (0.0d0, AreaScurve(iovh,ipoint)%AlfaLevels(2) - MAX(AreaScurve(iovh,ipoint)%AlfaLevels(1),PEIL) )
       DH3 = MAX (0.0d0, AreaScurve(iovh,ipoint)%AlfaLevels(3) - MAX(AreaScurve(iovh,ipoint)%AlfaLevels(2),PEIL) )
       DH4 = GwLevel - MAX (AreaScurve(iovh,ipoint)%AlfaLevels(3), PEIL)
       DQ4 = QIN2
     ELSE
! GWL0 staat al boven maaiveld nivo van dit deel van de Scurve; gebruik Dh5 en Dq5
! LET OP: bij Dh5 corrigeren voor BergC = de bergingscoefficient waarmee gwl wijziging bepaald wordt
       DH1 = MAX (0.0d0, AreaScurve(iovh,ipoint)%AlfaLevels(1) - PEIL)
       DH2 = MAX (0.0d0, AreaScurve(iovh,ipoint)%AlfaLevels(2) - MAX(AreaScurve(iovh,ipoint)%AlfaLevels(1),PEIL) )
       DH3 = MAX (0.0d0, AreaScurve(iovh,ipoint)%AlfaLevels(3) - MAX(AreaScurve(iovh,ipoint)%AlfaLevels(2),PEIL) )
       DH4 = MAX (0.0d0, AreaScurve(iovh,ipoint)%Level - MAX(AreaScurve(iovh,ipoint)%AlfaLevels(3),PEIL) )
!      DH5 nog steeds met GWL0, niet met GwLevel, want het gaat hier om de surface runoff
       DH5 = GWL0(iovh) - MAX (AreaScurve(iovh,ipoint)%Level, PEIL)
       DH5 = DH5 * BergC(Iovh)
       if (idebug .ne. 0) write(idebug,*) ' Dh5 etc ', Dh5, gwl0(iovh), AreaScurve(Iovh,ipoint)%Level, peil
! ARS 5599: houdt voor de 5e term (gwl boven oppervlak) ook rekening met berging op oppervlak
       DH5 = max (0.0d0, DH5 - MaxMm + ActMm)
       DQ5 = QIN2
       if (idebug .ne. 0) write(idebug,*) ' Dh5 Dq5 ', Dh5, Dq5
     ENDIF
    Else
!    Parallelle systemen
     DH1 = GwLevel - PEIL
     IF (GwLevel .LT. AreaScurve(iovh,ipoint)%AlfaLevels(1) .and. GwLevel .LT. AreaScurve(iovh,ipoint)%Level ) Then
       DQ1 = QIN2
     ELSEIF (GwLevel .LT. AreaScurve(iovh,ipoint)%AlfaLevels(2) .and. GwLevel .LT. AreaScurve(iovh,ipoint)%Level ) Then
       DH2 = GwLevel - MAX (AreaScurve(iovh,ipoint)%AlfaLevels(1), PEIL)
       DQ2 = QIN2
     ELSEIF (GwLevel .LT. AreaScurve(iovh,ipoint)%AlfaLevels(3) .and. GwLevel .LT. AreaScurve(iovh,ipoint)%Level ) Then
       DH2 = GwLevel - MAX (AreaScurve(iovh,ipoint)%AlfaLevels(1), PEIL)
       DH3 = GwLevel - MAX (AreaScurve(iovh,ipoint)%AlfaLevels(2), PEIL)
       DQ3 = QIN2
     ELSEIF (GwLevel .LT. AreaScurve(iovh,ipoint)%Level ) Then
       DH2 = GwLevel - MAX (AreaScurve(iovh,ipoint)%AlfaLevels(1), PEIL)
       DH3 = GwLevel - MAX (AreaScurve(iovh,ipoint)%AlfaLevels(2), PEIL)
       DH4 = GwLevel - MAX (AreaScurve(iovh,ipoint)%AlfaLevels(3), PEIL)
       DQ4 = QIN2
     ELSE
! ARS 14892
       DH2 = GwLevel - MAX (AreaScurve(iovh,ipoint)%AlfaLevels(1), PEIL)
       DH3 = GwLevel - MAX (AreaScurve(iovh,ipoint)%AlfaLevels(2), PEIL)
       DH4 = GwLevel - MAX (AreaScurve(iovh,ipoint)%AlfaLevels(3), PEIL)
! GWL0 staat al boven maaiveld nivo van dit deel van de Scurve; gebruik Dh5 en Dq5
! LET OP: bij Dh5 corrigeren voor BergC = de bergingscoefficient waarmee gwl wijziging bepaald wordt
!      DH5 nog steeds met GWL0, niet met GwLevel, want het gaat hier om de surface runoff
       DH5 = GWL0(IOVH) - MAX (AreaScurve(iovh,ipoint)%Level, PEIL)
       DH5 = DH5 * BergC(Iovh)
       if (idebug .ne. 0) write(idebug,*) ' Dh5 etc ', Dh5, gwl0(iovh), AreaScurve(Iovh,ipoint)%Level, peil
! ARS 5599: houdt voor de 5e term (gwl boven oppervlak) ook rekening met berging op oppervlak
       DH5 = max (0.0d0, DH5 - MaxMm + ActMm)
       DQ5 = QIN2
       if (idebug .ne. 0) write(idebug,*) ' Dh5 Dq5 ', Dh5, Dq5
     Endif
    Endif
    if (idebug .ne. 0) then
       write(idebug,*) ' DrainageDeltah=', DrainageDeltah , '(0=gestapeld,1=parallel)'
       write(iDebug,'(A,3G16.7)')  ' LvlDrnScurve  ', (AreaScurve(iovh,ipoint)%AlfaLevels(i),i=1,3)
       write(iDebug,'(A,3G16.7)')  ' LvlOh  LvlOhMx ', LvlOh(Iovh), LvlOhMx(iovh)
       write(iDebug,'(A,3G16.7)')  ' ScurveLevel (iovh,ipoint) ', ipoint, AreaScurve(iovh,ipoint)%Level
       write(iDebug,'(A,5G15.6)')  ' SetDeltaHScurve  DH1 tm Dh5: ', Dh1, Dh2, Dh3, Dh4, Dh5
    Endif

   Return
  END subroutine SetDeltaHScurve




! Addition Jan 2000

#if (!defined(USE_UNIX))
  SUBROUTINE RdModflowLevels (InModFlow, Itmstp)

  ! *********************************************************************
  ! ***                D E L F T         H Y D R A U L I C S
  ! ***
  ! ***              WATER RESOURCES AND ENVIRONMENT DIVISION
  ! *********************************************************************
  ! *** Program :  DELFT_3B version 1.0.                 Date: March 1995
  ! *********************************************************************
  ! *** Last update: March  1995       By : Geert Prinsen
  ! *********************************************************************
  ! *** Brief description:
  ! *** ------------------
  ! ***   Inlezen Modflow H0 waarden
  ! *********************************************************************
  ! *** Input/output parameters:
  ! *** ------------------------
  ! ***  IDEBUG    = file unit number of debug file
  ! ***  INModFlow = file unit number of input file from Modflow
  ! *********************************************************************

    INTEGER      IDUM, nPara, nLocF, i, iTime, iLoc, iECode, Itmstp
    Integer      iDebug, InModFlow, IUnp, Iow  !, Allocation_Error
    CHARACTER(40) CDUMMY
    Character(len=20), pointer :: ModflowNames(:)
    Real, pointer :: tmpModflowLevel(:)
    Logical success


    iDebug = ConfFil_get_iDebug()
    if (itmstp .eq. 1) idebug = idebugLunRR

    if (idebug .ne. 0) WRITE (IDEBUG,1)
  1 FORMAT (' RdModflowLevels')

  ! *********************************************************************
  ! *** read header
  ! *********************************************************************

!       PS: oct 97
!       1: read all locations from ModFlow_RR.his
!       2: compare all unpaved nodes in 3B-schematisation against read locations

!       step 1: read all locations from Modflow_RR.his file

    NLOCF = 0
    READ(InModFlow,END=102)  CDUMMY, CDUMMY, CDUMMY, CDUMMY
    READ(InModFlow)  NPARA, NLOCF

    success = DH_Allocinit (NLocF, ModFlowNames, ' ')
    success = success .and. DH_allocinit (NLocF, TmpModflowLevel, 0E0)
    if (.not. success) call ErrMsgStandard (981, 0, ' Error allocating arrays in subroutine ',' Unpaved_RdModflowLevels')

!    Allocate(ModflowNames(nLocF), tmpModflowLevel(nLocF), Stat=Allocation_Error )
!    If (Allocation_Error .ne. 0) &
!       call ErrMsgStandard (981, Allocation_Error, ' Error allocating arrays in subroutine ', &
!                                           ' Unpaved_RdModflowLevels'  )

    IF (NPARA .NE. 1) call ErrMsgStandard(971, 0,' Modflow HIS file must contain 1 parameter', '')

    READ(InModFlow)  (CDUMMY(1:20),I=1,NPARA)
    READ(InModFlow)  (IDUM, ModflowNames(i),I=1,NLOCF)


  ! *********************************************************************
  ! *** read data
  ! *********************************************************************

     READ (INModFlow) ITIME, ( tmpModflowLevel(ILOC),ILOC=1,NLOCF)

     goto 103

102  CONTINUE
!    Initialization in case of premature end of file
     NLOCF = 1
     success = DH_Allocinit (NLocF, ModFlowNames, ' ')
     success = success .and. DH_allocinit (NLocF, TmpModflowLevel, 0E0)
     if (.not. success) call ErrMsgStandard (981, 0, ' Error allocating arrays in subroutine ',' Unpaved_RdModflowLevels')
!     allocate(ModflowNames(nLocF), tmpModflowLevel(nLocF), Stat=Allocation_Error )
!     If (Allocation_Error .ne. 0) &
!       call ErrMsgStandard (981, Allocation_Error, ' Error allocating arrays in subroutine ', &
!                                           ' Unpaved_RdModFlowLevels'  )
     NLOCF = 0

103  CONTINUE

     Call ModflowData_to_SobekArrays (NLocF, tmpModflowLevel, ModflowNames)

     if (idebug .ne. 0) then
         write(idebug,*) ' RdModflowHis number of locations',NLOCF
         write(idebug,*) ' id-s',(ModflowNames(i),I=1,nlocf)
         write(idebug,*) ' time', ITIME
         write(idebug,*) ' levels',(tmpModflowLevel(i),i=1,nlocf)
         write(idebug,*) ' Unpaved area nodes',NCovhg
         write(idebug,*) ' Unpaved area H0Actual',(H0Actual(Iunp),iunp=1,ncovhg)
         write(idebug,*) ' Open water nodes',NCow
         write(idebug,*) ' Open water OwH0Actual',(OwH0Actual(Iow),iow=1,ncow)
     Endif


    Deallocate (ModflowNames, tmpModflowLevel)

    GOTO 999
  !
  ! *********************************************************************
  ! *** Error during reading of file
  ! *********************************************************************
  !
  150 CONTINUE
    call ErrMsgStandard (902, IECODE, 'RdModFlow', ' Sobek_results')

  ! *********************************************************************
  ! *** end of file
  ! *********************************************************************
  !
  999 CONTINUE

    RETURN
  END subroutine RdModflowLevels
#endif


  Subroutine GetH0fromTable (H0Lvl, Iovh)
    ! *********************************************************************
    ! *** Last update:  7 March 1997                      by: Geert Prinsen
    ! *********************************************************************
    ! *** Brief description:
    ! *** ------------------
    ! ***    Bepaal target level open water, en zomer/winter
    ! *********************************************************************
    ! *** Input/output parameters:
    ! *** ------------------------
    ! ***  TLVL   = target level
    ! ***  IOW    = index open water
    ! *********************************************************************

    INTEGER Iovh, rowNr, TabelNr, Idebug, Iout1
    Double precision H0Lvl
    type (Date) currentDate
    type (Time) currentTime
    Logical DateTimeOutsideTable

    IOut1  = ConfFil_get_IOut1()
    Idebug = ConfFil_get_IDEBUG()

    currentDate%year = ConfArr_get_IYear()
    currentDate%month = ConfArr_get_iMonth()
    currentDate%day = ConfArr_get_iDay()
    currentTime%hour = ConfArr_get_iHour()
    currentTime%minute = ConfArr_get_iMinute()
    currentTime%second = 0
! nieuwe tabellen
    RowNr = -1
    TabelNr = UnpRefH0_TTable(iovh)
    H0Lvl = GetNewValue(TableHandle, TabelNr, 1, RowNr, CurrentDate, CurrentTime, &
                        Idebug, iout1, DateTimeOutsideTable, .true.)
    if (idebug .ne. 0) WRITE (IDEBUG,*) ' H0 iovh rowNr h0lvl', iovh, RowNr, H0lvl

    RETURN
  END subroutine GetH0fromTable


  Subroutine GetSeepageFromTable (QSeep, QPerc, Iovh)
    ! *********************************************************************
    ! *** Last update:  April 2002                        by: Geert Prinsen
    ! *********************************************************************
    ! *** Brief description:
    ! *** ------------------
    ! ***    Get seepage/percolation from a time series
    ! *********************************************************************
    ! *** Input/output parameters:
    ! *** ------------------------
    ! ***  QSeep  = seepage in m/s
    ! ***  QPerc  = percolation in m/s
    ! ***             QSeep * QPerc = 0; one of them is zero, the other is positive (>=0)
    ! ***  Iovh   = index unpaved area
    ! *********************************************************************

    INTEGER Iovh, rowNr, TabelNr, Idebug, Iout1
    Double precision QSeep, Qperc
    type (Date) currentDate
    type (Time) currentTime
    Logical DateTimeOutsideTable

    IOut1  = ConfFil_get_IOut1()
    Idebug = ConfFil_get_IDEBUG()

    currentDate%year = ConfArr_get_IYear()
    currentDate%month = ConfArr_get_iMonth()
    currentDate%day = ConfArr_get_iDay()
    currentTime%hour = ConfArr_get_iHour()
    currentTime%minute = ConfArr_get_iMinute()
    currentTime%second = 0
! nieuwe tabellen
    RowNr = -1
    TabelNr = UnpRefSeepage_TTable(iovh)
    QPerc = 0
    QSeep = GetNewValue(TableHandle, TabelNr, 1, RowNr, CurrentDate, CurrentTime, &
                        Idebug, iout1, DateTimeOutsideTable, .true.)
!   convert to m/s
    QSeep = QSeep * Mm2M / NrsDay
!   assign separate values to Kwel and Wegzijging
    If (QSeep .lt. 0) then
        QPerc = -1. * QSeep
        QSeep = 0.
    Endif
    if (idebug .ne. 0) WRITE (IDEBUG,*) ' iovh rowNr QSeep QPerc ', iovh, RowNr, QSeep, QPerc

    RETURN
  END subroutine GetSeepageFromTable

  Subroutine GetSeepageConcFromTable (cSeep, Iovh)
    ! *********************************************************************
    ! *** Last update:  April 2002                        by: Geert Prinsen
    ! *********************************************************************
    ! *** Brief description:
    ! *** ------------------
    ! ***    Get seepage salt concentration from time series
    ! *********************************************************************
    ! *** Input/output parameters:
    ! *** ------------------------
    ! ***  CSeep  = seepage salt concentration in mg/l
    ! *********************************************************************

    INTEGER Iovh, rowNr, TabelNr, Idebug, Iout1
    double precision CSeep
    type (Date) currentDate
    type (Time) currentTime
    Logical DateTimeOutsideTable

    IOut1  = ConfFil_get_IOut1()
    Idebug = ConfFil_get_IDEBUG()

    currentDate%year = ConfArr_get_IYear()
    currentDate%month = ConfArr_get_iMonth()
    currentDate%day = ConfArr_get_iDay()
    currentTime%hour = ConfArr_get_iHour()
    currentTime%minute = ConfArr_get_iMinute()
    currentTime%second = 0
! nieuwe tabellen
    RowNr = -1
    TabelNr = UnpRefSeepageConc_TTable(iovh)
!   QPerc = 0
    CSeep = GetNewValue(TableHandle, TabelNr, 1, RowNr, CurrentDate, CurrentTime, &
                        Idebug, iout1, DateTimeOutsideTable, .true.)
    if (idebug .ne. 0) WRITE (IDEBUG,*) ' iovh rowNr Cseep', iovh, RowNr, CSeep

    RETURN
  END subroutine GetSeepageConcFromTable



  Subroutine Init1Unpaved (Idebug, IEvent, imaand, UseRestartIn)
    ! *********************************************************************
    ! *** Last update:
    ! *********************************************************************
    ! *** Brief description:
    ! *** ------------------
    ! ***    Stuk uit sub Init1: initialisie van onverhard gebied per tijdstap
    ! *********************************************************************

    Implicit none

    Integer iDebug, iout1, IEvent, IMaand
    Logical UseRestartIn
!   nav ARS 12450: UseRestartIn=true als net restart file is gelezen, false anders

    Integer Inode, Iovh, i, j, iow, ibnd, ipluv, iplus1
    Integer ISoil, iCrop
    Double precision    peil, rdum, DtCapsim, Pn, DpRootz, DpIn, VmRzIn, VmRzAc, FmEvPt, FmEvAc, FmPe, PercentArea, qstat

    Integer Istatus, Ipoint, IFlag
    Character(Len=CharIdLength) NodeName
    Logical   KvdLWarning

    Idebug = Conffil_get_idebug()
    IOut1  = ConfFil_get_iOut1()
    KvdLWarning = .false.

        DO INODE=1,NCNODE
         IF (EiNode(INODE,3).EQ. 2) THEN    ! kind 2 = unpaved area
          NodeName = Id_Nod(INODE)
          IOVH = EiNode(INODE,2)
          BOLND (IOVH) = BINIOL (IOVH)
          BOBD  (IOVH) = BINIBD(IOVH)
          PreviousTimestepCapRis(iovh) = 0.0

 ! Initialisation for Krayenhoff-vdLeur formulation, only 1st call (so not when UseRestartIn=true)
          If (CompOption(iovh) .eq. 2) then
            if (KvdLDimensie .lt. 1200 .and. TimeSettings%TimestepSize .lt. 3600 .and. .not. KvdLWarning) then
                KvdLWarning = .true.
                call SetMessage(LEVEL_WARN, 'Krayenhoff-vdLeur method better used with timestep of 1 hour or more and enough KvdL-history (50 days)')
            endif
            if (.not. UseRestartIn) Then
               do i=1,KvdLDimensie
                 KvdLeur(iovh,i)%neerslag   = 0.
                 KvdLeur(iovh,i)%Kopbolling = 0.
                 KvdLeur(iovh,i)%Kdebiet    = 0.
               enddo
               KvdLeurnul(iovh)%Kopbollingnul = 0.
               KvdLeurnul(iovh)%Kdebietnul    = 0.
               KvdLeurnul(iovh)%peilnul       = 0.
            endif
            KvdLeurnul(iovh)%Kopbollinglast= 0.
            KvdLeurnul(iovh)%Kdebietlast   = 0.
            KvdLeurnul(iovh)%openwaterlevel= 0.
            IOW  = EIOW(INODE)
            IBND = EIBND(INODE)
            IPluv = EIPluv(INODE)
            if (IOW .gt. 0) then
               PEIL = LVLOW0(IOW)
            elseif (IBND .gt. 0) then
               PEIL = BNDPAR(IBND,1)
            elseif (IPluv .gt. 0) then
               PEIL = GwlIni(Iovh)
            endif
            if ( (KvdLvariatieopenwater .ne. 0 .and. KvdlInitopenwater .ne. 0) ) then
              KvdLeurnul(iovh)%peilnul =  peil
              KvdLeurnul(iovh)%openwaterlevel =  peil
              KvdLeur(iovh,KvdLDimensie)%neerslag =  - (peil - gwlIni(iovh)) / timeSettings%timestepSize * NrsDay * BergC(iovh)
              if (idebug .ne. 0)  write(idebug,*) ' peil gwlIni KvdLNeerslag', &
                                                    peil, GwlIni(iovh), KvdLeur(iovh,KvdLDimensie)%neerslag
            endif
            if (RestIo(1) .eq. 0 .and. .not. UseRestartIn .and. EstimateKvdLHistory) Then
               ! only if restart file will not be read
               qstat = (GwlIni(iovh) - peil) * 8. * Bergc(iovh) / (pi_kwadraat) / ReservoirCoeff(iovh)     ! in m/dag
!               KvdLeur(iovh,KvdLDimensie)%Kopbolling = GwlIni(iovh) - peil
               do i=1,KvdLDimensie
                  KvdLeur(iovh,i)%neerslag  = qstat
!                  KvdLeur(iovh,i)%Kopbolling= GwlIni(iovh) - peil
               enddo
               ! initialisatie KvdL met constante neerslag, bereken hier de bijbehorende opbolling en debieten
               do j=1,KvdLDimensie
                  Write(Idebug,*) ' Call KvdL from Init1 with Qin=', Qstat*AreaGwComp(iovh)/86400.
                  Call KrayenhoffvdLeur (Iovh, Idebug, peil, Qstat*AreaGwComp(iovh)/86400., 0)
!                 KvdLeurnul(iovh)%Kopbollingnul = KvdLeurnul(iovh)%Kopbollingnul + KvdLeurnul(iovh)%KOpbollinglast
!                 KvdLeurnul(iovh)%Kdebietnul    = KvdLeurnul(iovh)%Kdebietnul    + KvdLeurnul(iovh)%KDebietlast
                  KvdLeurnul(iovh)%Kopbollingnul = 0.
                  KvdLeurnul(iovh)%Kdebietnul    = 0.
                  KvdLeurnul(iovh)%peilnul       = KvdLeurnul(iovh)%openwaterlevel
                  KvdLeurRest(iovh)%neerslag   = KvdLeur(iovh,1)%neerslag
                  KvdLeurRest(iovh)%KOpbolling = KvdLeur(iovh,1)%KOpbolling
                  KvdLeurRest(iovh)%KDebiet    = KvdLeur(iovh,1)%Kdebiet
                  do i=1,KvdLDimensie-1
                    Iplus1 = i+1
                    KvdLeur(iovh,i)%neerslag   = KvdLeur(iovh,iplus1)%neerslag
                    KvdLeur(iovh,i)%Kopbolling = KvdLeur(iovh,iplus1)%Kopbolling
                    KvdLeur(iovh,i)%Kdebiet    = KvdLeur(iovh,iplus1)%KDebiet
                  enddo
               enddo
            endif
            if (idebug .ne. 0)  write(idebug,*) ' peil gwlIni ', peil, GwlIni(iovh), KvdLeur(iovh,KvdLDimensie)%KOpbolling,KvdLeurnul(iovh)%Kopbollinglast
            if (idebug .ne. 0)  write(idebug,*) ' BergC       ', BergC(iovh)
            if (idebug .ne. 0)  write(idebug,*) ' KvdLeurNul OpbollingNul  ', KvdLeurnul(iovh)%KOpbollingNul
            if (idebug .ne. 0)  write(idebug,*) ' KvdLeurNul OpbollingLast ', KvdLeurnul(iovh)%KOpbollinglast
            if (idebug .ne. 0)  write(idebug,*) ' KvdLeurNul DebietNul     ', KvdLeurnul(iovh)%KDebietNul
            if (idebug .ne. 0)  write(idebug,*) ' KvdLeurNul DebietLast    ', KvdLeurnul(iovh)%KDebietlast
            if (idebug .ne. 0)  write(idebug,*) ' KvdLeurNul PeilNul       ', KvdLeurnul(iovh)%peilnul
            if (idebug .ne. 0)  write(idebug,*) ' KvdLeurNul Openwaterlevel', KvdLeurnul(iovh)%Openwaterlevel
          Endif

! Onverzadigde zone


!          OnvZone(IOVH)%Max_mm  = 240
!          OnvZone(IOVH)%Min_mm  = 50
           If (UnSatZoneOption .eq. 3) then   ! try out simple option, before option=2
              write(*,*) ' Set data for IOVH:',IOVH
              write(*,*) ' Set Max. Volume Unsaturated Zone in mm:'
              read (*,*)  rdum
              OnvZone(iovh)%Max_mm = rdum
              write(*,*) ' Set Min. Volume Unsaturated Zone in mm:'
              read (*,*)  rdum
              OnvZone(iovh)%Min_mm = min (rdum, OnvZone(iovh)%Max_mm)
              write(*,*) ' Set Init. Volume Unsaturated Zone in mm:'
              read (*,*)  rdum
              OnvZone(iovh)%Init_mm = min (rdum, OnvZone(iovh)%Max_mm)
              OnvZone(iovh)%Init_mm = max (rdum, OnvZone(iovh)%Min_mm)
              write(*,*) ' Specify 6 evap.factors for filling of 0-20-40-60-80-100%'
              write(*,*) ' evapfactor=1 voor act.evap = pot.evap.'
              read(*,*) (EvapFact(i,iovh),i=1,NVAL)
              write(*,*) ' ready for this node'

           Elseif (UnSatZoneOption .ge. 1) then   ! Sobek-Capsim or Capsim+
             EvapFact(1,Iovh) = 1.
             EvapFact(2,Iovh) = 1.
             EvapFact(3,Iovh) = 1.
             EvapFact(4,Iovh) = 1.
             EvapFact(5,Iovh) = 1.
             EvapFact(6,Iovh) = 1.
! Zet CapsimCrop(iovh) = de cropindex met het grootste cropareaal.
             CapsimCrop(iovh) = 1
             DO Icrop=1,Ncrop
                IF ( AreaGw(Iovh,Icrop) .gt. AreaGw(iovh,CapsimCrop(iovh)) ) then
                     CapsimCrop(iovh) = Icrop
                ENDIF
             ENDDO
             If (UseSCurve(iovh) .eq. 1) then
                Do Ipoint=1,UseUnpavedSCurve
                    AreaScurve(iovh,ipoint)%InitialSurfaceStorage = 0.0
                    AreaScurve(iovh,ipoint)%InitialSoilStorage = 0.0
                Enddo
             Endif
! Zet CapsimDpRootz = Average rootzone depth in m
             DpRootz = 0.0
             ISoil   = Bottyp (iovh)
             Do ICrop=1,NCrop
                if (AreaGw(Iovh,Icrop) .gt. 0) then
                    DpRootz = DpRootz + AreaGw(Iovh,Icrop) * DpRz(ISoil,Icrop)
                endif
             Enddo
             CapsimDpRootz(iovh) = DpRootz / AreaOh(iovh)
! Initialisatie Capsim
! Ars 1737: Initial Humidity in the Rootzone beter initialiseren
             Do Icrop=1,NCrop
                CropOnvZone(iovh,Icrop)%Max_mm  = 999.
                CropOnvZone(iovh,icrop)%Min_mm  = 0.1
             Enddo
             OnvZone(iovh)%Max_mm  = 999.
             OnvZone(iovh)%Min_mm  = 0.1
             DtCapsim              = 30.0
! april 2002: LvlOhMx ipv LvlOh
             DpIn                  = LvlOhMx(iovh) - GWL (iovh)
!            DpIn                  = LVLOH(iovh) - GWL (iovh)
! ARS 12450
             if (UseRestartIn) then
!               do nothing; use OnvZone()%Init_mm as determined from restart file
             else
                OnvZone(iovh)%Init_mm = max (10.0d0, Dpin * BergC(iovh) / mm2m )
             endif
! ARS 12450
             VmRzIn                = OnvZone(Iovh)%Init_mm * mm2m
             Pn                    = 0.0
             FmEvPt                = 0.0
             if (imaand .eq. 3 .or. imaand .eq. 10) then
                 FmEvPt = 0.5
             elseif (imaand .eq. 4 .or. imaand .eq.  9) then
                 FmEvPt = 1.0
             elseif (imaand .eq. 5 .or. imaand .eq.  8) then
                 FmEvPt = 1.5
             elseif (imaand .eq. 6 .or. imaand .eq.  7) then
                 FmEvPt = 2.0
             endif
             if (FmEvPt .gt. 0) FmEvPt = FmEvPt * mm2m
             if (Idebug .gt.0) then
!               correctie dec 2001: schrijf CapsimDpRootz ipv DpRootz naar debugfile
                call DebugSimgro (Idebug, CapsimCrop(iovh), ISoil, &
                                  DpIn, CapsimDpRootZ(iovh), DtCapsim, Pn, VmRzIn, FmEvPt)
                write(idebug,*) ' BergC           ', BergC(iovh)
             endif

            IFlag = 2
! Ars 1737: Initial Humidity in the Rootzone beter initialiseren

            If (CapsimPerCropArea .eq. 0) then
              If (UseRestartIn) then
!                ARS 12450, just read data from restart file
                  VmRzAc = OnvZone(Iovh)%Actual_mm * mm2m
                  Call FillOnvZoneInitData (VmRzAc, AreaOh(iovh), OnvZone(iovh))
                  If (UseScurve(iovh) .eq. 1 .and. ScurveDataToBeSetByRR(iovh)) Call Init1AreaScurve (iovh, VmRzAc, idebug)
! May 2001: with Capsim once per unpaved area
              ElseIf (InitCapsimOption .eq. 1) then
!                Initialiseer op evenwichtsvochtgehalte V(Equilibrium)
                  Call Capsim2StorageCoefficient (ISoil, DpIn, CapsimDpRootZ(iovh)*100., IFlag, VmRzAc)
                  Call FillOnvZoneInitData (VmRzAc, AreaOh(iovh), OnvZone(iovh))
                  If (UseScurve(iovh) .eq. 1) Call Init1AreaScurve (iovh, VmRzAc, idebug)
              ElseIf (InitCapsimOption .eq. 2) then
!                Initialiseer op vochtgehalte V(pF=2)
                  DpIn = 1.0
                  call Capsim2StorageCoefficient (ISoil, DpIn, CapsimDpRootZ(iovh)*100., IFlag, VmRzAc)
                  Call FillOnvZoneInitData (VmRzAc, AreaOh(iovh), OnvZone(iovh))
                  If (UseScurve(iovh) .eq. 1) Call Init1AreaScurve (iovh, VmRzAc, idebug)
              ElseIf (InitCapsimOption .eq. 3) then
!                Initialiseer op vochtgehalte V(pF=3)
                  DpIn = 10.0
                  call Capsim2StorageCoefficient (ISoil, DpIn, CapsimDpRootZ(iovh)*100., IFlag, VmRzAc)
                  Call FillOnvZoneInitData (VmRzAc, AreaOh(iovh), OnvZone(iovh))
                  If (UseScurve(iovh) .eq. 1) Call Init1AreaScurve (iovh, VmRzAc, idebug)
              Else
! Als voorheen,
! Zet initiele vulling wortelzone:
! bepaal dit door call Simgro_ovz met grote tijdstap (dt =10 dagen)
! voor een situatie met verdamping afhankelijk van Imaand varierend van 0 tot 2.0 mm/dag
! Resultaat van Call Simgro_ovz is VmRzAc in meters
! Dec 2001; als InitCapsimOptipon=1,2,3, dan op Veq, VpF2, VpF3, anders oude initialisatie
!           als InitCapsimOptipon=4      dan special: zet pot.evap op nul voor initialisatie
               If (InitCapsimOption .eq. 4) FmEvPt = 0.0
               if (Idebug .gt.0)  write(idebug,*) ' FmEvPt          ', FmEvPt
! End Dec2001
               If (UseScurve(iovh) .eq. 0) then
! Initialiseer Capsim, zonder S curve maaiveld
                 DpIn   = LVLOH(iovh) - GWL (iovh)
                 call SIMGRO_OVZ(Debug_unit, CapSimDbgFile, Message_Unit, CapsimMsgFile, Istatus, &
                             NXSPUN, NXTE, NXRZ, NXDPUN, NXFRSW, &
                             ISoil, CapsimCrop(iovh), DpIn, CapsimDpRootz(iovh), DtCapsim, Pn, VmRzIn, FmEvPt, &
                             SRRZ, FMCA, SCSA, &
                             DPRZUN, DPGWUN, NUDPUN, DPFRSW, FRSW, FREV, &
                             FmEvAc, FmPe, VmRzAc)
                  Call CheckSimgro (Istatus, VmRzAc, Iovh, NodeName)
                  Call FillOnvZoneInitData (VmRzAc, AreaOh(iovh), OnvZone(iovh))
               Elseif (UseScurve(iovh) .eq. 1) then
! Initialiseer Capsim, met S curve maaiveld
                  OnvZone(iovh)%Init_Volume = 0 ! OnvZone(iovh)%Init_mm * AreaOh(iovh) * mm2m
                  Do Ipoint=1,UseUnpavedSCurve
                    DpIn   = max ( 0.0d0, AreaScurve(iovh,ipoint)%Level - GWL(iovh) )
                    call SIMGRO_OVZ(Debug_unit, CapSimDbgFile, Message_Unit, CapsimMsgFile, Istatus, &
                             NXSPUN, NXTE, NXRZ, NXDPUN, NXFRSW, &
                             ISoil, CapsimCrop(iovh), DpIn, CapsimDpRootz(iovh), DtCapsim, Pn, VmRzIn, FmEvPt, &
                             SRRZ, FMCA, SCSA, &
                             DPRZUN, DPGWUN, NUDPUN, DPFRSW, FRSW, FREV, &
                             FmEvAc, FmPe, VmRzAc)
                    Call CheckSimgro (Istatus, VmRzAc, Iovh, NodeName)
!Initial surface storage als GWL boven Level Scurve
                    Call Init1AreaScurve (iovh, VmRzAc, idebug)
!!NOV2001             DpIn   = AreaScurve(iovh,ipoint)%Level - GWL(iovh)
!!                    If (DpIn .lt. 0) AreaScurve(iovh,ipoint)%InitialSurfaceStorage = -DpIn * AreaScurve(iovh,ipoint)%Area
!!                    AreaScurve(iovh,ipoint)%ActualSurfaceStorage  = AreaScurve(iovh,ipoint)%InitialSurfaceStorage
!!                    AreaScurve(iovh,ipoint)%InitialSoilStorage = VmRzAc * AreaScurve(iovh,ipoint)%Area
!!                    AreaScurve(iovh,ipoint)%ActualSoilStorage = VmRzAc * AreaScurve(iovh,ipoint)%Area
!!                    OnvZone(iovh)%Init_Volume = OnvZone(iovh)%Init_Volume + AreaScurve(iovh,ipoint)%InitialSoilStorage
!!                    if (idebug .ne. 0) then
!!                       write (Idebug,*) ' Results Sobek-Capsim with Scurve:'
!!                       write (Idebug,*) ' ipoint = ', ipoint, VmRzAc, OnvZone(iovh)%Init_Volume
!!                    endif
                  Enddo
                  OnvZone(Iovh)%Init_mm     = OnvZone(Iovh)%Init_Volume / AreaOh(Iovh) / mm2m
               Endif
              Endif

            ElseIf (CapsimPerCropArea .eq. 1) then
! May 2001: with Capsim per each crop per unpaved area

             OnvZone(IOVH)%Init_Volume = 0.0
             OnvZone(IOVH)%Init_mm     = 0.0
             DpIn   = LVLOHMx(iovh) - GWL (iovh)
! April2002  DpIn   = LVLOH(iovh) - GWL (iovh)
             Do ICrop=1,NCrop
              If (AreaGw(iovh,icrop) .gt. 0) then
               PercentArea = AreaGw(Iovh,Icrop) / AreaOh(iovh)
               if (idebug .ne. 0) Write(Idebug,*) ' CapsimPerCropArea for ',iovh, icrop, InitCapsimOption
               If (UseRestartIn) then
!                ARS 12450, just read data from restart file
                  VmRzAc = OnvZone(Iovh)%Actual_mm * mm2m
                  if (.not. UseRestart210(iovh)) Call FillOnvZoneInitData (VmRzAc, AreaGW(iovh,icrop), CropOnvZone(iovh,icrop))
                  if (idebug .ne. 0) Write(Idebug,*) ' Initcapsim=1 Initvol CropOnvZone=', CropOnvZone(Iovh,icrop)%Init_Volume
                  If (UseScurve(iovh) .eq. 1 .and. ScurveDataPerCropToBeSetByRR(iovh)) Call Init1AreaScurvePerCrop (iovh, icrop, VmRzAc, PercentArea)
               ElseIf (InitCapsimOption .eq. 1) then
!                Initialiseer op evenwichtsvochtgehalte V(Equilibrium)
                  Call Capsim2StorageCoefficient (ISoil, DpIn, DpRz(isoil,icrop)*100., IFlag, VmRzAc)
                  Call FillOnvZoneInitData (VmRzAc, AreaGW(iovh,icrop), CropOnvZone(iovh,icrop))
                  if (idebug .ne. 0) Write(Idebug,*) ' Initcapsim=1 Initvol CropOnvZone=', CropOnvZone(Iovh,icrop)%Init_Volume
                  If (UseScurve(iovh) .eq. 1) Call Init1AreaScurvePerCrop (iovh, icrop, VmRzAc, PercentArea)
               ElseIf (InitCapsimOption .eq. 2) then
!                Initialiseer op vochtgehalte V(pF=2)
                  DpIn = 1.0
                  call Capsim2StorageCoefficient (ISoil, DpIn, DpRz(isoil,icrop)*100., IFlag, VmRzAc)
                  Call FillOnvZoneInitData (VmRzAc, AreaGW(iovh,icrop), CropOnvZone(iovh,icrop))
                  if (idebug .ne. 0) Write(Idebug,*) ' Initcapsim=2 Initvol CropOnvZone=', CropOnvZone(Iovh,icrop)%Init_Volume
                  If (UseScurve(iovh) .eq. 1) Call Init1AreaScurvePerCrop (iovh, icrop, VmRzAc, PercentArea)
               ElseIf (InitCapsimOption .eq. 3) then
!                Initialiseer op vochtgehalte V(pF=3)
                  DpIn = 10.0
                  call Capsim2StorageCoefficient (ISoil, DpIn, DpRz(isoil,icrop)*100., IFlag, VmRzAc)
                  Call FillOnvZoneInitData (VmRzAc, AreaGW(iovh,icrop), CropOnvZone(iovh,icrop))
                  if (idebug .ne. 0) Write(Idebug,*) ' Initcapsim=3 Initvol CropOnvZone=', CropOnvZone(Iovh,icrop)%Init_Volume
                  If (UseScurve(iovh) .eq. 1) Call Init1AreaScurvePerCrop (iovh, icrop, VmRzAc, PercentArea)
               Else
! Als voorheen, Zet initiele vulling wortelzone afhankelijk van de maand
                If (UseScurve(iovh) .eq. 0) then
! Initialiseer Capsim, zonder S curve maaiveld
                 DpIn   = LVLOH(iovh) - GWL (iovh)
                 call SIMGRO_OVZ(Debug_unit, CapSimDbgFile, Message_Unit, CapsimMsgFile, Istatus, &
                             NXSPUN, NXTE, NXRZ, NXDPUN, NXFRSW, &
                             ISoil, ICrop, DpIn, DpRz(isoil,icrop), DtCapsim, Pn, VmRzIn, FmEvPt, &
                             SRRZ, FMCA, SCSA, &
                             DPRZUN, DPGWUN, NUDPUN, DPFRSW, FRSW, FREV, &
                             FmEvAc, FmPe, VmRzAc)
                  Call CheckSimgro (Istatus, VmRzAc, Iovh, NodeName)
                  Call FillOnvZoneInitData (VmRzAc, AreaGW(iovh,icrop), CropOnvZone(iovh,icrop))
                  if (idebug .ne. 0) Write(Idebug,*) ' Initcapsim=0 No Scurve Initvol CropOnvZone=', &
                                                        CropOnvZone(Iovh,icrop)%Init_Volume
                Elseif (UseScurve(iovh) .eq. 1) then
! Initialiseer Capsim, met S curve maaiveld
                  CropOnvZone(iovh,icrop)%Init_Volume = 0
                  Do Ipoint=1,UseUnpavedSCurve
                    DpIn   = max ( 0.0d0, AreaScurve(iovh,ipoint)%Level - GWL(iovh) )
                    call SIMGRO_OVZ(Debug_unit, CapSimDbgFile, Message_Unit, CapsimMsgFile, Istatus, &
                             NXSPUN, NXTE, NXRZ, NXDPUN, NXFRSW, &
                             ISoil, ICrop, DpIn, DpRz(isoil,icrop), DtCapsim, Pn, VmRzIn, FmEvPt, &
                             SRRZ, FMCA, SCSA, &
                             DPRZUN, DPGWUN, NUDPUN, DPFRSW, FRSW, FREV, &
                             FmEvAc, FmPe, VmRzAc)
                    Call CheckSimgro (Istatus, VmRzAc, Iovh, NodeName)
!Initial surface storage als GWL boven Level Scurve
                    Call Init1PointScurvePerCrop (iovh, icrop, VmRzAc, PercentArea, ipoint)
                  Enddo
                  CropOnvZone(Iovh,icrop)%Init_mm     = CropOnvZone(Iovh,icrop)%Init_Volume / AreaGw(Iovh,icrop) / mm2m
                  if (idebug .ne. 0) Write(Idebug,*) ' Initcapsim=0 Scurve not yet correct', CropOnvZone(Iovh,icrop)%Init_Volume
                Endif  ! InitCapsimOption0, UseScurve
               Endif   ! InitCapsimOption
              Endif    ! AreaGw > 0
             Enddo     ! end of loop over crops

             Do ICrop=1,NCrop
                 CropOnvZone(IOVH,icrop)%Max_Volume    = CropOnvZONE(IOVH,icrop)%Max_mm  * AREAGW(IOVH,icrop) * mm2m
                 CropOnvZone(IOVH,icrop)%Init_Volume   = CropOnvZONE(IOVH,icrop)%Init_mm * AREAGW(IOVH,icrop) * mm2m
                 CropOnvZone(IOVH,icrop)%Min_Volume    = CropOnvZONE(IOVH,icrop)%Min_mm  * AREAGW(IOVH,icrop) * mm2m
                 CropOnvZone(IOVH,icrop)%Actual_Volume = CropOnvZone(IOVH,icrop)%Init_Volume
                 CropOnvZone(IOVH,icrop)%Actual_mm     = CropOnvZONE(IOVH,icrop)%Init_mm
                 If (AreaGw(iovh,icrop) .gt. 0) then
                   OnvZone(IOVH)%Init_Volume   = OnvZone(IOVH)%Init_Volume  + CropOnvZONE(Iovh,icrop)%Init_Volume
                 Endif
              Enddo
              OnvZone(IOVH)%Init_mm = OnvZONE(IOVH)%Init_Volume / AREAOH(IOVH) / mm2m
              if (idebug .ne. 0) Write(Idebug,*) ' OnvZone Init Volume ', OnvZone(Iovh)%Init_Volume
            Endif

! Zet storage coefficient according to new SC_tables
             call CapsimStorageCoefficient (iovh)
             BergC(iovh) = CapsimBergC (iovh)
! ARS xxxx jan 2003 Option for constant HdeZBergC in case of Capsim
             if (.not. ConstantHdeZBergC)  HdeZBergC(iovh) = CapsimBergC (iovh)

           Elseif (UnSatZoneOption .eq. 0) then
! No unsaturated zone: make it inactive by putting max=min=initial
! Do not initialisze to zero because of salt computations.
             OnvZone(iovh)%Max_mm  = 1.
             OnvZone(iovh)%Min_mm  = 1.
             OnvZone(iovh)%Init_mm = 1.
             EvapFact(1,Iovh) = 1.
             EvapFact(2,Iovh) = 1.
             EvapFact(3,Iovh) = 1.
             EvapFact(4,Iovh) = 1.
             EvapFact(5,Iovh) = 1.
             EvapFact(6,Iovh) = 1.
           Endif

! Altijd:
           Vulling(1,IOVH) = 0.
           Vulling(2,IOVH) = 20.
           Vulling(3,IOVH) = 40.
           Vulling(4,IOVH) = 60.
           Vulling(5,IOVH) = 80.
           Vulling(6,IOVH) = 100.
           OnvZone(IOVH)%Max_Volume    = OnvZONE(IOVH)%Max_mm  * AREAOH(IOVH) * mm2m
           OnvZone(IOVH)%Init_Volume   = OnvZONE(IOVH)%Init_mm * AREAOH(IOVH) * mm2m
           OnvZone(IOVH)%Min_Volume    = OnvZONE(IOVH)%Min_mm  * AREAOH(IOVH) * mm2m
           OnvZone(IOVH)%Actual_Volume = OnvZone(IOVH)%Init_Volume
           OnvZone(IOVH)%Actual_mm     = OnvZONE(IOVH)%Init_mm
           if (idebug .ne. 0) Write(Idebug,*) ' Init1 Actual_Volume',iovh,OnvZone(IOVH)%Actual_mm,OnvZone(IOVH)%Actual_Volume

! April 2002: determine Dh-DV relation for groundwater, once only
           if (Ievent .eq. 1 .and. DetailedGwlComputation .eq. 2)  Call DetermineGwDhDVRelation (iovh, idebug)

         Endif
        ENDDO

! initialisatie Restterminfo
        ResttermTijd (1) = 0.041666667
        ResttermTijd (2) = 0.083333333
        ResttermTijd (3) = 0.125
        ResttermTijd (4) = 0.208333333
        ResttermTijd (5) = 0.291666667
        ResttermTijd (6) = 0.375
        ResttermTijd (7) = 0.5
        ResttermTijd (8) = 0.625
        ResttermTijd (9) = 0.75
        ResttermTijd (10) = 0.916666667
        ResttermTijd (11) = 1.083333333
        ResttermTijd (12) = 1.25
        ResttermTijd (13) = 1.458333333
        ResttermTijd (14) = 1.666666667
        ResttermTijd (15) = 1.875
        ResttermTijd (16) = 2.125
        ResttermTijd (17) = 2.375
        ResttermTijd (18) = 2.625
        ResttermTijd (19) = 2.916666667
        ResttermTijd (20) = 3.208333333
        ResttermTijd (21) = 3.5
        ResttermTijd (22) = 3.833333333
        ResttermTijd (23) = 4.166666667
        ResttermTijd (24) = 4.5
        ResttermTijd (25) = 4.875
        ResttermTijd (26) = 5.25
        ResttermTijd (27) = 5.625
        ResttermTijd (28) = 6.041666667
        ResttermTijd (29) = 6.458333333
        ResttermTijd (30) = 6.875
        ResttermTijd (31) = 7.5
        ResttermTijd (32) = 8.125
        ResttermTijd (33) = 8.75
        ResttermTijd (34) = 9.375
        ResttermTijd (35) = 10
        ResttermTijd (36) = 11
        ResttermTijd (37) = 12
        ResttermTijd (38) = 13
        ResttermTijd (39) = 15
        ResttermTijd (40) = 17
        ResttermTijd (41) = 19
        ResttermTijd (42) = 22
        ResttermTijd (43) = 25
        ResttermTijd (44) = 28
        ResttermTijd (45) = 32
        ResttermTijd (46) = 36
        ResttermTijd (47) = 40
        ResttermTijd (48) = 45
        ResttermTijd (49) = 50
        ResttermTijd (50) = 55
        ResttermTijd (51) = 60
        ResttermTijd (52) = 70
        ResttermTijd (53) = 80
        ResttermTijd (54) = 90
        ResttermTijd (55) = 100
        ResttermTijd (56) = 200
        ResttermTijd (57) = 300
        ResttermTijd (58) = 400
        ResttermTijd (59) = 500
        !                        n=10001       n=1000001          n=99
        ResttermFactor (1)  =  0.937969751   ! 0.937933283    0.941981943
        ResttermFactor (2)  =  0.912259196   ! 0.912222729    0.916271388
        ResttermFactor (3)  =  0.892530794   ! 0.892494326    0.896542986
        ResttermFactor (4)  =  0.86124606    ! 0.861209593    0.865258252
        ResttermFactor (5)  =  0.835816701   ! 0.835780233    0.839828892
        ResttermFactor (6)  =  0.813828212   ! 0.813791744    0.817840404
        ResttermFactor (7)  =  0.785021067   ! 0.7849846      0.789033259
        ResttermFactor (8)  =  0.759641464   ! 0.759604996    0.763653656
        ResttermFactor (9)  =  0.736696548   ! 0.736660081    0.74070874
        ResttermFactor (10) =  0.70890281    ! 0.708866342    0.712915002
        ResttermFactor (11) =  0.683540523   ! 0.683504055    0.687552715
        ResttermFactor (12) =  0.660065364   ! 0.660028897    0.664077556
        ResttermFactor (13) =  0.632827587   ! 0.632791119    0.636839779
        ResttermFactor (14) =  0.60748103    ! 0.607444562    0.611493222
        ResttermFactor (15) =  0.583688085   ! 0.583651617    0.587700277
        ResttermFactor (16) =  0.55685349    ! 0.556817023    0.560865682
        ResttermFactor (17) =  0.531615985   ! 0.531579517    0.535628177
        ResttermFactor (18) =  0.507764689   ! 0.507728221    0.511776881
        ResttermFactor (19) =  0.481486749   ! 0.481450281    0.485498941
        ResttermFactor (20) =  0.456706006   ! 0.456669538    0.460718198
        ResttermFactor (21) =  0.433286483   ! 0.433250016    0.437298675
        ResttermFactor (22) =  0.408052309   ! 0.408015841    0.412064501
        ResttermFactor (23) =  0.38432865    ! 0.384292182    0.388340842
        ResttermFactor (24) =  0.362008212   ! 0.361971744    0.366020404
        ResttermFactor (25) =  0.338459781   ! 0.338423314    0.342471973
        ResttermFactor (26) =  0.316452932   ! 0.316416464    0.320465124
        ResttermFactor (27) =  0.295882395   ! 0.295845927    0.299894587
        ResttermFactor (28) =  0.274594157   ! 0.274557689    0.278606349
        ResttermFactor (29) =  0.254839584   ! 0.254803116    0.258851776
        ResttermFactor (30) =  0.236507305   ! 0.236470837    0.240519496
        ResttermFactor (31) =  0.211452808   ! 0.211416341    0.215465
        ResttermFactor (32) =  0.189053311   ! 0.189016843    0.193065503
        ResttermFactor (33) =  0.16902722    ! 0.168990752    0.173039412
        ResttermFactor (34) =  0.151122964   ! 0.151086497    0.155135156
        ResttermFactor (35) =  0.135115695   ! 0.135079228    0.139127887
        ResttermFactor (36) =  0.112956146   ! 0.112919678    0.116968338
        ResttermFactor (37) =  0.09443195    ! 0.094395483    0.098444142
        ResttermFactor (38) =  0.078946714   ! 0.078910246    0.082958905
        ResttermFactor (39) =  0.055180696   ! 0.055144228    0.059192888
        ResttermFactor (40) =  0.038572844   ! 0.038536376    0.042585036
        ResttermFactor (41) =  0.026967166   ! 0.026930698    0.030979358
        ResttermFactor (42) =  0.015770126   ! 0.015733658    0.019782318
        ResttermFactor (43) =  0.009229206   ! 0.009192739    0.013241398
        ResttermFactor (44) =  0.00540823    ! 0.005371762    0.009420422
        ResttermFactor (45) =  0.002661738   ! 0.002625271    0.00667393
        ResttermFactor (46) =  0.001320542   ! 0.001284074    0.005332734
        ResttermFactor (47) =  0.000665594   ! 0.000629127    0.004677786
        ResttermFactor (48) =  0.000295687   ! 0.000259219    0.004307879
        ResttermFactor (49) =  0.000144684   ! 0.000108217    0.004156876
        ResttermFactor (50) =  8.30419E-05   ! 4.65743E-05    0.004095234
        ResttermFactor (51) =  5.78785E-05   ! 2.14109E-05    0.00407007
        ResttermFactor (52) =  4.3413E-05    ! 6.94536E-06    0.004055605
        ResttermFactor (53) =  4.10024E-05   ! 4.53479E-06    0.004053194
        ResttermFactor (54) =  4.06007E-05   ! 4.13309E-06    0.004052793
        ResttermFactor (55) =  4.05338E-05   ! 4.06615E-06    0.004052726
        ResttermFactor (56) =  4.05204E-05   ! 4.05277E-06    0.004052712
        ResttermFactor (57) =  4.05204E-05   ! 4.05277E-06    0.004052712
        ResttermFactor (58) =  4.05204E-05   ! 4.05277E-06    0.004052712
        ResttermFactor (59) =  4.05204E-05   ! 4.05277E-06    0.004052712


  Return
  END subroutine Init1Unpaved



  Subroutine DetermineGwDhDVRelation (iovh, idebug)

! Initialise the groundwater deltah - DeltaV relation
! This routine is called only once, for the first event
! Assumption: no changes in crops during the simulation period!!!!
! Level 0 = surface level, positive = going downward
! Relation stored in two tables, afterwards linear interpolation is used.

  integer iovh, idebug

  integer iflag, idum
  double precision  DeltaVolume, NewStorageCoefficient, DpIn, Area

  IFlag = 1
  Area = MAX (AreaGwComp(IOVH), 0.0001d0)

  ! all initial values are zero (incl. the values for idum=1)
  Do idum=2,MaxGwDepth
     GroundwaterDeltaLevel (idum,Iovh) = GroundwaterDeltaLevel(idum-1,Iovh) + StepGwlStorageCoefficient
     DpIn =  GroundwaterDeltaLevel (idum-1,Iovh) + HalfStepGwl
     Call Capsim3StorageCoefficient (iovh, DpIn, IFlag, NewStorageCoefficient)
     DeltaVolume = NewStorageCoefficient * Area * StepGwlStorageCoefficient
     GroundwaterDeltaVolume(idum,Iovh) = GroundwaterDeltaVolume(idum-1,Iovh) + DeltaVolume
  Enddo

! idebug = idebugLunRR
  if (idebug .ne. 0) then
      write(idebug,*) ' GwDhDV relation determined for iovh=',iovh
      Do idum=1,MaxGwDepth
         Write(Idebug,*) GroundwaterDeltaLevel(idum,iovh), GroundwaterDeltaVolume(idum,iovh)
      Enddo
  endif
!
! write(*,*) ' GwDhDV relation determined for iovh=',iovh
! Write(*,*) ' Gw Delta level ', (GroundwaterDeltaLevel(idum,iovh), idum=1,MaxGwDepth)
! Write(*,*) ' Gw Delta Volume', (GroundwaterDeltaVolume(idum,iovh), idum=1,MaxGwDepth)
! idebug = 0

  Return
  End subroutine DetermineGwDhDVRelation



  Subroutine FindNewGroundwaterLevel (Idebug, iovh, VinToDo, Dgwl1)

!  use the groundwater dh-DV relation, initial gwl, and deltaVolume to find new gwl

     Integer Iovh, idebug, i
     double precision VinToDo, Dgwl1
     Double precision InitVol, NewVolume, NewGwl
     Double precision GroundwaterDeltaLevelArray(MaxGwDepth), GroundwaterDeltaVolumeArray(MaxGwDepth)

!    if (iovh .eq. 177) idebug = idebugLunRR
     Do i=1,MaxGwDepth
        GroundwaterDeltaLevelArray(i) = GroundwaterDeltaLevel(i,iovh)
        GroundwaterDeltaVolumeArray(i) = GroundwaterDeltaVolume(i,iovh)
     Enddo
     CALL RR_D_INTERP (MaxGwDepth, GroundwaterDeltaLevelArray, GroundwaterDeltaVolumeArray, &
                    DGwl1, InitVol, GwlLastInterpIndex(iovh))
     if (idebug .ne. 0) then
        Write(Idebug,*) ' GwlDeltaLevel  = ', (GroundwaterDeltaLevel(i,iovh), i=1,132)
        Write(Idebug,*) ' GwlDeltaVolume = ', (GroundwaterDeltaVolume(i,iovh), i=1,132)
        Write(Idebug,*) ' GwlInit   = ', DGwl1
        Write(Idebug,*) ' GwlVol    = ', InitVol
        Write(Idebug,*) ' VInToDo   = ', VinToDO
     Endif

!    VInToDo >0 means water is added to the groundwater
!    This corresponds with lower Volume in the DeltaH-DeltaV relation table
     NewVolume = InitVol - VInToDo
     if (NewVolume .ge. 0) then
        CALL RR_D_INTERP (MaxGwDepth, GroundwaterDeltaVolumeArray, GroundwaterDeltaLevelArray, &
                                NewVolume, NewGwl, GwlLastInterpIndex(iovh))
        VInToDo= 0.
     Else
        NewGwl = 0.0
        VInToDo= -1. * NewVolume
        GwlLastInterpIndex(iovh) = 1
     Endif

     Gwl(iovh) = LvlOhMx(iovh) - NewGwl

     if (idebug .ne. 0) then
        Write(Idebug,*) ' VInToDo remaining      = ', VinToDo
        Write(Idebug,*) ' NewVolume in dh-dv rel.= ', NewVolume
        Write(Idebug,*) ' NewGwl m below surface = ', NewGwl
        Write(Idebug,*) ' NewGwl w.r.t. ref.lvl  = ', Gwl(iovh)
        Write(Idebug,*) ' GwlLastInterpIndex     = ', GwlLastInterpIndex(iovh)
     Endif
!    idebug = 0


  Return
  End subroutine FindNewGroundwaterLevel


  Subroutine Init1AreaScurve (iovh, VmRzAc, idebug)

! Initialisatie bij berekeningen met Scurve
! Initialisatie van AreaScurve %InitialSurfaceStorage etc.

  Integer iovh, ipoint, idebug
  Double precision    VmRzAc, Dpin

      Do Ipoint=1,UseUnpavedSCurve
        DpIn   = AreaScurve(iovh,ipoint)%Level - GWL(iovh)
        AreaScurve(iovh,ipoint)%InitialSurfaceStorage = 0.0
        If (DpIn .lt. 0) AreaScurve(iovh,ipoint)%InitialSurfaceStorage = -DpIn * AreaScurve(iovh,ipoint)%Area
        AreaScurve(iovh,ipoint)%ActualSurfaceStorage  = AreaScurve(iovh,ipoint)%InitialSurfaceStorage
        AreaScurve(iovh,ipoint)%InitialSoilStorage = VmRzAc * AreaScurve(iovh,ipoint)%Area
        AreaScurve(iovh,ipoint)%ActualSoilStorage = AreaScurve(iovh,ipoint)%InitialSoilStorage
        OnvZone(iovh)%Init_Volume = OnvZone(iovh)%Init_Volume + AreaScurve(iovh,ipoint)%InitialSoilStorage
        if (idebug .ne. 0) then
           write (Idebug,*) ' Results Sobek-Capsim with Scurve:'
           write (Idebug,*) ' ipoint = ', ipoint, VmRzAc, OnvZone(iovh)%Init_Volume
        endif
      Enddo

  Return
  END subroutine Init1AreaScurve



  Subroutine Init1AreaScurvePerCrop (iovh, icrop, VmRzAc, PercentArea)

! Initialisatie bij berekeningen met CapsimPerCrop
! Initialisatie bij berekeningen met Scurve
! Initialisatie van AreaScurve %InitialSurfaceStorage etc.

  Integer iovh, icrop, ipoint
  Double precision    VmRzAc, PercentArea

    Do Ipoint=1,UseUnpavedSCurve
      Call Init1PointScurvePerCrop (iovh, icrop, VmRzAc, PercentArea,ipoint)
    Enddo

  Return
  END subroutine Init1AreaScurvePerCrop



  Subroutine Init1PointScurvePerCrop (iovh, icrop, VmRzAc, PercentArea, ipoint)

! Initialisatie bij berekeningen met CapsimPerCrop
! Initialisatie bij berekeningen met Scurve
! Initialisatie van AreaScurve %InitialSurfaceStorage etc.voor point ipoint van de Scurve

  Integer iovh, icrop, ipoint
  Double precision    VmRzAc, Dpin, PercentArea

    DpIn   = AreaScurve(iovh,ipoint)%Level - GWL(iovh)
    If (DpIn .lt. 0) then
      AreaScurvePerCrop(iovh,icrop,ipoint)%InitialSurfaceStorage = - DpIn * AreaScurve(iovh,ipoint)%Area * PercentArea
    Endif
    AreaScurvePerCrop(iovh,icrop,ipoint)%InitialSoilStorage = VmRzAc * AreaScurve(iovh,ipoint)%Area * PercentArea
    AreaScurvePerCrop(iovh,icrop,ipoint)%ActualSurfaceStorage  = AreaScurvePerCrop(iovh,icrop,ipoint)%InitialSurfaceStorage
    AreaScurvePerCrop(iovh,icrop,ipoint)%ActualSoilStorage = AreaScurvePerCrop(iovh,icrop,ipoint)%InitialSoilStorage
    CropOnvZone(iovh,icrop)%Init_Volume = CropOnvZone(iovh,icrop)%Init_Volume + &
                                                VmRzAc * AreaScurve(iovh,ipoint)%Area * PercentArea

  Return
  END subroutine Init1PointScurvePerCrop



  Subroutine Init2Ovh (Itmstp,Iflag)
    ! *********************************************************************
    ! *** Last update:
    ! *********************************************************************
    ! *** Brief description:
    ! *** ------------------
    ! ***    Stuk uit sub Init2: initialisie van onverhard gebied per tijdstap
    ! *********************************************************************

    Implicit none

      INTEGER iNode, iovh, iow, ibnd, ipluv, teller
      Integer i, iplus1, ipoint
      Integer iDebug, ICrop, ISoil, itemp
      Double precision    DpIn, DpIn1, DpIn2, DpRootz, NewStorageCoefficient, TempStorageCoefficient, increment
      Double precision    temp1
      Integer Itmstp, IFlag

      iDebug = ConfFil_get_iDebug()

 ! Onverhard gebied
      DO INODE= 1,NCNODE
       IF (EiNode(INODE,3) .EQ. 2) THEN
         IOVH = EiNode(INODE,2)
         BOLND0 (IOVH) = BOLND (IOVH)
         BOBD0  (IOVH) = BOBD  (IOVH)
         GWL0   (IOVH) = GWL   (IOVH)
         PreviousTimestepCapRis(iovh) = -1. * QINB(iovh)
 ! Initialisation for variable seepage using a H0Table
         If (SeepageCompOption(iovh) .eq. 2) Call GetH0FromTable (H0Actual(iovh), iovh)
 ! Initialisation for variable seepage using a time table
         If (SeepageCompOption(iovh) .eq. 4)  Call GetSeepageFromTable (Kwel(iovh), WegZg(iovh), iovh)
         If (SeepageCompOption(iovh) .eq. 5)  then
            Call GetSeepageFromTable (Kwel(iovh), WegZg(iovh), iovh)
            Call GetSeepageConcFromTable (Temp1, iovh)
            SltKwl(inode) = Temp1
         Endif
 ! Initialisation for Krayenhoff-vdLeur formulation
         If (CompOption(iovh) .eq. 2) Then
!           If (Itmstp .gt. 1) KvdLeurnul(iovh)%Kopbollingnul = 0.0
            KvdLeurnul(iovh)%Kopbollingnul = KvdLeurnul(iovh)%Kopbollingnul + KvdLeurnul(iovh)%KOpbollinglast
            KvdLeurnul(iovh)%Kdebietnul    = KvdLeurnul(iovh)%Kdebietnul    + KvdLeurnul(iovh)%KDebietlast
            KvdLeurnul(iovh)%peilnul       = KvdLeurnul(iovh)%openwaterlevel
            KvdLeurRest(iovh)%neerslag   = KvdLeur(iovh,1)%neerslag
            KvdLeurRest(iovh)%KOpbolling = KvdLeur(iovh,1)%KOpbolling
            KvdLeurRest(iovh)%KDebiet    = KvdLeur(iovh,1)%Kdebiet
            do i=1,KvdLDimensie-1
              Iplus1 = i+1
              KvdLeur(iovh,i)%neerslag   = KvdLeur(iovh,iplus1)%neerslag
              KvdLeur(iovh,i)%Kopbolling = KvdLeur(iovh,iplus1)%Kopbolling
              KvdLeur(iovh,i)%Kdebiet    = KvdLeur(iovh,iplus1)%KDebiet
            enddo
            if (idebug .ne. 0)  write(idebug,*) ' KvdLeurNul OpbollingNul  ', KvdLeurnul(iovh)%KOpbollingNul
            if (idebug .ne. 0)  write(idebug,*) ' KvdLeurNul OpbollingLast ', KvdLeurnul(iovh)%KOpbollinglast
            if (idebug .ne. 0)  write(idebug,*) ' KvdLeurNul DebietNul     ', KvdLeurnul(iovh)%KDebietNul
            if (idebug .ne. 0)  write(idebug,*) ' KvdLeurNul DebietLast    ', KvdLeurnul(iovh)%KDebietlast
            if (idebug .ne. 0)  write(idebug,*) ' KvdLeurNul PeilNul       ', KvdLeurnul(iovh)%peilnul
            if (idebug .ne. 0)  write(idebug,*) ' KvdLeurNul Openwaterlevel', KvdLeurnul(iovh)%Openwaterlevel
            if (idebug .ne. 0)  write(idebug,*) ' KvdLeur neerslag  KvdLDim', KvdLeur(iovh,KvdLDimensie)%neerslag
            if (idebug .ne. 0)  write(idebug,*) ' KvdLeur opbolling KvdLDim', KvdLeur(iovh,KvdLDimensie)%Kopbolling
            if (idebug .ne. 0)  write(idebug,*) ' KvdLeur debiet    KvdLDim', KvdLeur(iovh,KvdLDimensie)%KDebiet
            if (idebug .ne. 0)  write(idebug,*) ' KvdLeur neerslag  KvdLDim-1', KvdLeur(iovh,KvdLDimensie-1)%neerslag
            if (idebug .ne. 0)  write(idebug,*) ' KvdLeur opbolling KvdLDim-1', KvdLeur(iovh,KvdLDimensie-1)%Kopbolling
            if (idebug .ne. 0)  write(idebug,*) ' KvdLeur debiet    KvdLDim-1', KvdLeur(iovh,KvdLDimensie-1)%KDebiet
            if (idebug .ne. 0)  write(idebug,*) ' KvdLeur neerslag  KvdLDim-2', KvdLeur(iovh,KvdLDimensie-2)%neerslag
            if (idebug .ne. 0)  write(idebug,*) ' KvdLeur opbolling KvdLDim-2', KvdLeur(iovh,KvdLDimensie-2)%Kopbolling
            if (idebug .ne. 0)  write(idebug,*) ' KvdLeur debiet    KvdLDim-2', KvdLeur(iovh,KvdLDimensie-2)%KDebiet
          Endif

         OnvZone(IOVH)%Init_Volume = OnvZone(IOVH)%Actual_Volume
         OnvZone(IOVH)%Init_mm     = OnvZONE(IOVH)%Actual_mm

         Do iCrop=1,NCrop
            CropOnvZone(IOVH,ICrop)%Init_Volume= CropOnvZone(IOVH,ICrop)%Actual_Volume
            CropOnvZone(IOVH,ICrop)%Init_mm    = CropOnvZONE(IOVH,iCrop)%Actual_mm
         Enddo


!        Indien S curve maaiveld onverhard gebied, dan ook updaten SoilStorage
         If (UseScurve(iovh) .eq. 1) then
            Do ipoint=1,UseUnpavedScurve
               AreaScurve(iovh,ipoint)%InitialSoilStorage = AreaScurve(iovh,ipoint)%ActualSoilStorage
               AreaScurve(iovh,ipoint)%InitialSurfaceStorage = AreaScurve(iovh,ipoint)%ActualSurfaceStorage
               AreaScurve(iovh,ipoint)%SurfaceOutflow = 0.0
               AreaScurve(iovh,ipoint)%SoilOutflow = 0.0
               If (CapsimPerCropArea .eq. 1 .and. UnSatZoneOption .ge. 1) then     ! extra check op UnsatZoneOption
                 Do Icrop=1,NCrop
                   AreaScurvePerCrop(iovh,icrop,ipoint)%InitialSoilStorage   = &
                        AreaScurvePerCrop(iovh,icrop,ipoint)%ActualSoilStorage
                   AreaScurvePerCrop(iovh,icrop,ipoint)%InitialSurfaceStorage= &
                        AreaScurvePerCrop(iovh,icrop,ipoint)%ActualSurfaceStorage
                 Enddo
               Endif
            Enddo
         Endif

         If (UnSatZoneOption .ge. 1) then     ! Sobek-Capsim or Capsim+
!            Update storage coefficient BERGC(iovh) voor bepalen veranderingen in grondwaterstand;
!            BergC is de bergingscoefficient volgens Capsim bij gemiddelde grondwaterstand
             call CapsimStorageCoefficient (iovh)
             BergC (iovh) = CapsimBergC(iovh)
!            Update storage coefficient HdeZBergC(iovh) for Hellinga-deZeeuw formula

! HdeZstorage stor.coeff still based on average rootzone depth,and main crop;
!                                    TO CHANGE depending on drainage formulation to use; decision MAY 2001
             Icrop  = CapsimCrop(iovh)
             ISoil  = BotTyp (iovh)
             DpIn1  = LVLOH(iovh) - GWL (iovh)
             DpRootz  = CapsimDpRootz(iovh) * 100.
             IOW  = EIOW(INODE)
             IBND = EIBND(INODE)
             IPluv = EIPluv(INODE)
             if (iow .gt. 0) then
                 DpIn2  = LVLOH(iovh) - LVLOW(iow)
             elseif (ibnd .gt. 0) then
                 DpIn2  = LVLOH(iovh) - BNDPAR(ibnd,1)
             elseif (ipluv .gt. 0) then
                 DpIn2  = LVLOH(iovh) - GwlIni(iovh)
             else
                call SetMessage(LEVEL_FATAL, 'Unpaved node should have an RR-link to RR-open water, RR-boundary or RR-CF connection for drainage')
             endif
! HdeZBergC is gemiddelde bergingscoefficient van grondwaterstand tot open water peil
! bepaald in stapjes van 1 cm
! De HdeZBergC is van belang voor de uitstroming van bodem naar open water volgens de Hellinga-deZeeuw formules
! ARS xxxx Jan 2003: option for constant HdeZBergC in case of Capsim
! ARS   August 2006: versnellen Init2Ovh: dit stukje alleen Als HdeZ gebruikt wordt met niet constante BergC
!                    tevens max. aantal stappen op 100 gezet
             if (.not. ConstantHdeZBergC .and. CompOption(iovh) .eq. 1) then
                 call Capsim2StorageCoefficient (ISoil, DpIn1, DpRootz, IFlag, NewStorageCoefficient)
                 TempStorageCoefficient = NewStorageCoefficient
                 call Capsim2StorageCoefficient (ISoil, DpIn2, DpRootz, IFlag, NewStorageCoefficient)
                 TempStorageCoefficient = TempStorageCoefficient + NewStorageCoefficient
                 DpIn = min (DpIn1, DpIn2)
                 Increment = 0.01
                 itemp = int ( (abs (DpIn2-DpIn1)) / Increment )
                 if (itemp .gt. 100) then
                    Increment = Increment * Itemp/100.
                    Itemp = 100
                 endif
                 Do teller=1,itemp
                    DpIn = DpIn + Increment
                    call Capsim2StorageCoefficient (ISoil, DpIn, DpRootz, IFlag, NewStorageCoefficient)
                    TempStorageCoefficient = TempStorageCoefficient + NewStorageCoefficient
                 Enddo
                 TempStorageCoefficient=TempStorageCoefficient / (itemp+2)
                 HdeZBergC(iovh) = TempStorageCoefficient
             endif
!        Elseif (UnSatZoneOption .eq. 0) then   ! old situation; do nothing
!            do nothing
!        Elseif (UnSatZoneOption .eq. 3) then   ! try-out option
!            to be added if necessary
         Endif
         if (idebug .ne. 0) then
            write(idebug,*) ' onverhard gebied BergC HdeZBergC', iovh, BergC(iovh),HdeZBergC(iovh)
            write(idebug,*) ' onverhard gebied Init_Vol       ', iovh, OnvZone(iovh)%Init_Volume
         endif

       Endif
      ENDDO

  Return
  END subroutine Init2Ovh



  Subroutine WrInputDataUnpaved (Iout9, Iout3, RnDate, RnTime)
    ! *********************************************************************
    ! *** Last update:
    ! *********************************************************************
    ! *** Brief description:
    ! *** ------------------
    ! ***    Stuk uit sub WrData: uitvoer van onverhard gebied in *.Out files
    ! *********************************************************************

    Implicit none

    Integer      INODE, IKIND, INR, i
    Integer      IOUT9, IOUT3
    Integer*2    RNDATE(3), RNTIME(4)


! Onverhard gebied
      IF (NCOVHG .GT. 0) THEN
         WRITE(IOUT9,12)
   12    FORMAT (//,' Summary input data unpaved area          ',//, &
           ' Node identification    Node   Surface   Surf.Level max.storage',               &
           ' Drainage     Infiltration Alfa-factors    Storagecoefficient',/,                    &
           '                        name                       on land    ',                &
           '  depth        velocity     land  soil   (based on ref.level summer)',/,             &
           '                                  (ha)     (m NAP)    (mm)    ',                &
           '  (m NAP)      (mm/hour)      (1/day)       (mm)',/, 134('='))
         DO INODE =1,NCNODE
          IKIND = EiNode(INODE,3)
          INR   = EiNode(INODE,2)
          IF (IKIND .EQ. 2) THEN
            IF (AREAOH(INR) .LE. .00001) AREAOH(INR) = 0.00001
            WRITE(IOUT9,22) Id_Nod(INODE),  &
                            NamNod(INODE),&
                            AREAOH(INR)/HA2M, LVLOH(INR), &
                            BMAXOL(INR)/AREAOH(INR)/MM2M,       &
                            ONTWDP(INR), INF_V(INR)*NRSHR/MM2M, &
                            ALFAOH(INR,1)*NRSDAY, ALFAOH(INR,2)*NRSDAY, BERGC(INR)
   22       FORMAT (A20,1X,A12,1X,F9.3,1X,6(F9.2,1X),F9.4)
          ENDIF
         ENDDO
      ENDIF

      If (ncovhg .gt. 0 .and. OutputDesired(2) ) then
        WRITE(IOUT3,51) RNDATE(3),RNDATE(2),RNDATE(1),(RNTIME(I),I=1,2), CASENM
   51   FORMAT (' Run datum (DD-MM-YY) and time:',I2,'-',I2,'-',I4,2X,I2,':',I2,//,&
                ' Case name                 ',A20,/)
        IF (IOPT2(1) .EQ. 0) THEN
          WRITE(IOUT3,1003) '[m]', '[m3/s]', '[m3/s]','[m3/s]', '[hour]'
        ELSE
          WRITE(IOUT3,10031) '[m]', '[m3/s]', '[m3/s]','[m3/s]', '[hour]'
        ENDIF
 1003   FORMAT(//,' Maxima per event',//, &
                 ' Event   Start     Node identification   Node  ',&
                 '    Groundwaterlevel    Surf_runoff    Disch_soil    Rainfall  ',&
                 ' Acceptable  gw   GWL on surface  Cum.Exc.Threshold GWL',/,&
                 '  nr  year-mon-day                       name',   &
                 '           ',A4,'NAP',9X,A6,10X,A6,8X,A6,7X,'NAP',10X,A6,/,163('='))
 10031  FORMAT(//,' Maxima per event      ',//,&
                 ' Event   Start     Node identification   Node   ',&
                 '    Groundwaterlevel    Surf_runoff   Discharge_soil Rainfall  ',&
                 ' Acceptable  gw   GWL on Surface  Cum.Exc.Threshold GWL ',/,&
                 '  nr  year-mon-day                       name',&
                 '         ',A4,' surface level',5X,A6,10X,A6,10X,A5,' wrt surf.level ',A6,/,163('='))
      Endif


  Return
  END subroutine WrInputDataUnpaved



    Subroutine OnvZoneVoltoMm (ActVolume, Area, ActMm)
!   Given soil moisture volume and area, determine soil moisture contents in mm
!   Call with OnvZone%Actual_Volume etc.
    Double precision ActVolume, Area, Actmm

    Actmm     = ActVolume / Area / mm2m

    Return
    END subroutine OnvZoneVoltoMm



    Subroutine OnvZoneMmtoVol (ActMm, Area, ActVolume)
!   Given soil moisture content and area, determine soil moisture volume
!   Call with OnvZone%Actual_Volume etc.
    Double precision ActVolume, Area, Actmm

    ActVolume = Actmm * Area * mm2m

    Return
    END subroutine OnvZoneMmtoVol



    Subroutine FillOnvZoneInitData (VmRzAc, Area, OnvZoneObject)
!   Fill OnvZoneObject with data
    Double precision VmRzAc, Area
    Type (Unsat_Zone)  OnvZoneObject

    OnvZoneObject%Init_Volume = VmRzAc * Area
    OnvZoneObject%Init_mm     = VmRzAc / mm2m

    Return
    END subroutine FillOnvZoneInitData


    Subroutine CheckWL_Capsim(Istatus, VmRzAc, Iovh)
!   Give error message if WL_Capsim routine returns an error or negative volume
    Integer Istatus, iovh
    Double precision    VmRzAc

    if (IStatus .ne. 0) call ErrMsgStandard (969, 0, ' ', ' Error in routine WL_CAPSIM')
    If (VmRzAc .lt. -0.001) then
       NrWLCapsimErrors = NrWLCapsimErrors +1
       if (NrWLCapsimErrors .le. 5) then
          call ErrMsgStandard (969, iovh, ' ', ' Negative Volume returned from routine WL_CAPSIM')
       Endif
    Endif

    Return
    End subroutine CheckWL_Capsim


    Subroutine CheckSimgro (Istatus, VmRzAc, Iovh, NodeName)
!   Give error message if Capsim routine returns an error or negative volume
    Integer Istatus, iovh, Len
    Double precision    VmRzAc
    Character(Len=CharIdLength) NodeName

    Len = Len_trim(NodeName)
    if (IStatus .ne. 0) call ErrMsgStandard (969, 0, NodeName(1:len), ' Error in Alterra-routine CAPSIM')
    If (VmRzAc .lt. -0.001) then
       NrCapsimErrors = NrCapsimErrors +1
       if (NrCapsimErrors .le. 5) then
          call ErrMsgStandard (969, iovh, NodeName(1:len), ' Negative Volume returned from Alterra-routine CAPSIM')
       Endif
    Endif

    Return
    End subroutine CheckSimgro



    Subroutine DebugSimgro (Idebug, Nt, Ns, DpIn, DpRootZ, Dt, Pn, VmRzIn, FmEvPt)

    Integer Idebug
    Integer Nt, Ns
    Double precision    DpIn, DpRootz, Dt, Pn, VmRzIn, FmEvPt

    if (idebug .ne. 0) then
        write (Idebug,*) ' Call Sobek-Capsim with:'
        write(idebug,*) ' Crop index      ', Nt
        write(idebug,*) ' Soil index      ', Ns
        write(idebug,*) ' DpIn initial gwl', Dpin, ' m below surface'
        write(idebug,*) ' DpRootZ         ', DpRootz, ' m'
        write(idebug,*) ' Dt timestep     ', Dt     , ' d'
        write(idebug,*) ' Pn Net infiltrat', Pn, ' m/d'
        write(idebug,*) ' VmRzIn init.Vol ', VmRzIn, ' m'
        write(idebug,*) ' FmEvPt  Epot    ', FmEvPt, ' m/d'
    endif

    Return
    End subroutine DebugSimgro

    Subroutine DebugWL_Capsim (Idebug, Nt, Ns, DpIn, DpRootZ, Dt, Pn, VmRzIn, FmEvPt)

    Integer Idebug
    Integer Nt, Ns
    Double precision    DpIn, DpRootz, Dt, Pn, VmRzIn, FmEvPt

    if (idebug .ne. 0) then
        write (Idebug,*) ' Call WL-Capsim with:'
        write(idebug,*) ' Crop index      ', Nt
        write(idebug,*) ' Soil index      ', Ns
        write(idebug,*) ' DpIn initial gwl', Dpin, ' m below surface'
        write(idebug,*) ' DpRootZ         ', DpRootz, ' m'
        write(idebug,*) ' Dt timestep     ', Dt     , ' d'
        write(idebug,*) ' Pn Net infiltrat', Pn, ' m/d'
        write(idebug,*) ' VmRzIn init.Vol ', VmRzIn, ' m'
        write(idebug,*) ' FmEvPt  Epot    ', FmEvPt, ' m/d'
    endif

    Return
    End subroutine DebugWL_Capsim



  Subroutine Wr1OutUnPaved (Iout3, Ievent, Month, INode, IOvh)
    ! *********************************************************************
    ! *** Last update:
    ! *********************************************************************
    ! *** Brief description:
    ! *** ------------------
    ! ***    Stuk uit sub Wr1Out: uitvoer van UnPaved node: maxima per event in OUT file
    ! *********************************************************************

      Implicit none

    ! variables

    Integer      INODE, IOvh, Iout3, Ievent
    Integer      ExcDur
    Double precision         ExcCumGwl
    Double precision         GWL, QOpper, QBodem, QRain, GWLMX
    CHARACTER(len=3) MONTH(12)

        if (.not. associated(OVMGWS)) return    ! If there is nothing, do nothing

! gwl tov ref.level
        GWL = OVMGWS(IOVH,IEVENT)
! flows in m3/s
        QOPPER = OVMQOU(IOVH,1,IEVENT)
        QBODEM = OVMQOU(IOVH,2,IEVENT)
        QRAIN  = OVMQOU(IOVH,3,IEVENT)
        GWLMX = MAXGWL2(IOVH)
! eenheid tov maaiveld of tov ref.level
!       IF (IOPT2(1) .EQ. 1) THEN
!          GWL = GWL - LVLOH(IOVH)
!          GWLMX  = GWLMX - LVLOH(IOVH)
!       ENDIF
! overschrijding max. peil in uren
       EXCDUR = GWEXC (IOVH,2,IEVENT)
       EXCCumGwl = GWEXC (IOVH,3,IEVENT)
       EXCDUR = EXCDUR / NRSHR

       WRITE(IOUT3,1004) IEVENT, EventStartDateTime(IEVENT,1), MONTH(EventStartDateTime(IEVENT,2)), &
         EventStartDateTime(IEVENT,3),&
         Id_Nod(INODE), NamNod(INODE), &
         GWL, QOPPER, QBODEM, QRAIN, GWLMX, EXCDUR, ExcCumGwl
 1004  FORMAT (I4,1X,I4,1X,A3,1X,I3,3X,A20,1X,A15,1X,F12.3,1X,4(F12.3,1X), I12,8X,F12.3)

  Return
  END subroutine Wr1OutUnPaved


  Subroutine Unpaved_DeAllocateArrays

    if (Allocated(ONVZone)) DeAllocate(OnvZone)
    if (Allocated(CropONVZone)) DeAllocate(CropOnvZone)
    if (Allocated(KvdLeur)) DeAllocate(KvdLeur)
    if (Allocated(KvdLeurRest)) DeAllocate(KvdLeurRest)
    if (Allocated(KvdLeurNul)) DeAllocate(KvdLeurNul)
    if (associated(KvdLH2Add)) then
       DeAllocate(KvdLH2Add)
       KvdLH2Add => null()
    endif
    if (associated(KvdLQ2Add)) Then
       DeAllocate(KvdLQ2Add)
       KvdLQ2Add => null()
    endif
    if (Allocated(AreaScurve)) DeAllocate(AreaSCurve)
    if (Allocated(AreaScurvePerCrop)) DeAllocate(AreaSCurvePerCrop)

  Return
  End subroutine Unpaved_DeallocateArrays


  Subroutine UnpavedVolumeCheckActive (Qold, Qnew, Inode, Ibnd, Iow)

! MaxVolChk = Maximum debiet volgens volume check
! CheckActive = true/false (output)
! istr = kunstwerk index
! ibnd = boundary index

! zet Volume Check active als debiet met factor 4 of meer geknepen wordt en Q >= 0.1 m3/s.
! Deze factor is hard gecodeerd

  Double precision    Qold, Qnew
  Integer Ibnd, inode, iow
  Character(len=CharIdLength) IdUnpaved, IdBoundary, IdOpenWater

  idUnpaved = ''
  If ( Abs (Qnew) .lt. Abs (0.25 * Qold) .and. Abs(Qold) .gt. .1) then
     If (ibnd .gt. 0) then
        if (DetailedVolumeCheckMessages) then
            if (inode .gt. 0) idunpaved  = Id_nod(inode)
            idboundary = Id_nod((bndnam(ibnd)))
            call ErrMsgStandard (982, 982, idunpaved, idboundary)
        endif
     Else
        if (DetailedVolumeCheckMessages .and. iow .gt. 0) then
            if (inode .gt. 0) idunpaved   = Id_nod(inode)
            idopenwater = Id_nod((ownam(iow)))
            call ErrMsgStandard (982, 982, idunpaved, idboundary)
        endif
     Endif
  Endif

  RETURN
  END subroutine UnpavedVolumeCheckActive


  Subroutine FindSoilMoistureAtCriticalpFValue (pFCrit, CriticalSoilMoistureValue, Soiltype, RootzoneThickness, IrrCriticalSMValue)

  integer    Soiltype, i, ixrz, ixrz1, ival, idum, D_IfReal
  Double precision       RootzoneThickness, CriticalGWLevel
  Double precision       pFCrit, CriticalSoilMoistureValue, IrrCriticalSMValue


  if (IrrCriticalSMValue .le. -999.) then

 ! First find index IXRZ corresponding with RootzoneThickness
 ! Locate geeft index IVAl terug, waarvoor XX(ival) <= Dprootz <= XX(ival+1)
 ! Ival=0 of N betekent out of range.
     CALL D_LOCATE (DPRZUN, NActRzClass, RootzoneThickness*100., IVAL)
     ixrz = ival
     ixrz1 = ixrz + 1
     if (ival .le. 0) then
        ixrz  = 1
        ixrz1 = 1
     elseif (ival .ge. nActRzClass) then
        ixrz  = nActRzClass
        ixrz1 = nActRzClass
     endif
     idum = D_IfReal (RootzoneThickness*100., DpRzUn(ixrz), 1D-8)
     if (Idum .le. 0) ixrz1 = ixrz
     idum = D_IfReal (RootzoneThickness*100., DpRzUn(ixrz1), 1D-8)
     if (Idum .ge. 0) ixrz  = ixrz1
     ! find from Capsim tables through interpolation
     CriticalGWLevel = 0.01 * (10 ** (pFCrit))
     CriticalSoilMoistureValue = 0
     Do i=1,Nxdpun-1
        If (DPGWUN(Soiltype, Ixrz,i) .le. CriticalGwLevel .and. DPGWUN(Soiltype,Ixrz,i+1) .gt. CriticalGwLevel) goto 99
     Enddo
  99 Continue
     if (i .lt. NxDPun) then
        CriticalSoilMoistureValue = SRRZ(SoilType,ixrz,i) + &
                                       (SRRZ(SoilType,ixrz,i+1)-SRRZ(SoilType,ixrz,i)) / &
                                        (DPGWUN(SoilType,ixrz,i+1)-DPGWUN(SoilType,ixrz,i)) &
                                * (CriticalGwLevel-DPGWUN(SoilType,ixrz,i))
     else
        i = NxDPUN-1
        CriticalSoilMoistureValue = SRRZ(SoilType,ixrz,i) + &
                                       (SRRZ(SoilType,ixrz,i+1)-SRRZ(SoilType,ixrz,i)) / &
                                        (DPGWUN(SoilType,ixrz,i+1)-DPGWUN(SoilType,ixrz,i)) &
                                * (CriticalGwLevel-DPGWUN(SoilType,ixrz,i))
     endif
     CriticalSoilMoistureValue = CriticalSoilMoistureValue*1000.  ! convert to mm
     IrrCriticalSMValue = CriticalSoilMoistureValue
  else
     ! already computed
     CriticalSoilMoistureValue = IrrCriticalSMValue
  endif

  RETURN
  END subroutine FindSoilMoistureAtCriticalpFValue



  Subroutine FindGWLevelAtCriticalpFValue (pFCrit, CriticalGWLevel, IrrCriticalGWLevel)
  Double precision       pFCrit, CriticalGWLevel, IrrCriticalGWLevel

  if (IrrCriticalGWLevel .le. -999.) then
     ! find from pF - gw level relation
     CriticalGWLevel = 0.01 * (10 ** (-1.*pFCrit))
     IrrCriticalGWLevel = CriticalGWLevel
  else
     ! already computed
     CriticalGWLevel = IrrCriticalGWLevel
  endif

  RETURN
  END subroutine FindGWLevelAtCriticalpFValue

  !> If success, function returns Values array of length ElementCount
  !! for unpaved elementset on specific quantity handle
  function RR_GetUnpavedDataByIntId(QuantityHandle, ElementCount, Values) result(success)

    use RR_open_mi_support
    use wl_open_mi_support

    implicit none

    ! return value
    logical  :: success

    ! arguments
    integer,          intent(in)      :: QuantityHandle   !< quant. handle
    integer,          intent(in)      :: ElementCount     !< #elems in unpaved elementset
    double precision, intent(out), &
            dimension(1:ElementCount) :: Values           !< values in unpaved elemenset

    ! locals
    Double precision, dimension(:), pointer :: openMIValues

    Values  = 0
    success = .true.

    select case (QuantityHandle)
    ! Subdivide for various variables
    case(RRiSurfRunoff)
    !RR Surface runoff in m3/s
            ! If a stack overflow occurs due to this array operation, use a do loop
            if (NOVH > 0) then
                Values(1:NOVH) = RSLMAP2_ovh(1, 1:NOVH, 1)
            else
                success = .false.
            endif
    case(RRiGwOutflow)
    !RR Groundwater outflow in m3/s
            ! If a stack overflow occurs due to this array operation, use a do loop
            if (NOVH > 0) then
                Values(1:NOVH) = RSLMAP2_ovh(2, 1:NOVH, 1)
            else
                success = .false.
            endif
    case (RRiGWRecharge)
    !RR groundwater recharge in m3/s
        success = DH_AllocInit(NCOVHG, openMIValues, 0.0D+00)
        if (success) then
            if (NCOVHG .GT. 0) then
                openMIValues(1:NCOvhg) = QinB(1:NCOvhg)/timeSettings%Timestepsize - Q2O(1:ncovhg) !+-UnpSeep(1:ncovhg)
            endif
            Values = openMIValues
            deallocate(openMIValues)
        endif
    case(RRiRainfall)
    !RR Rainfall in m3/s
            ! If a stack overflow occurs due to this array operation, use a do loop
            if (NOVH > 0) then
                Values(1:NOVH) = RSLMAP2_ovh(3, 1:NOVH, 1)
            else
                success = .false.
            endif
    case(RRiEvaporationSurface)
    !RR Evaporation on surface in m3/s
            ! If a stack overflow occurs due to this array operation, use a do loop
            if (NOVH > 0) then
                Values(1:NOVH) = RSLMAP2_ovh(4, 1:NOVH, 1)
            else
                success = .false.
            endif
    case(RRiInfiltration)
    !RR Infiltration in m3/s
            ! If a stack overflow occurs due to this array operation, use a do loop
            if (NOVH > 0) then
                Values(1:NOVH) = RSLMAP2_ovh(5, 1:NOVH, 1)
            else
                success = .false.
            endif
    case(RRiSeepage)
    !RR Net Seepage in m3/s
            ! If a stack overflow occurs due to this array operation, use a do loop
            if (NOVH > 0) then
                Values(1:NOVH) = RSLMAP2_ovh(6, 1:NOVH, 1)
            else
                success = .false.
            endif
    case(RRiEvaporationActual)
    !RR Actual Evap. in m3/s
            ! If a stack overflow occurs due to this array operation, use a do loop
            if (NOVH > 0) then
                Values(1:NOVH) = RSLMAP2_ovh(7, 1:NOVH, 1)
            else
                success = .false.
            endif
    case(RRiEvaporationPotential)
    !RR Potential Evaporation in m3/s
            ! If a stack overflow occurs due to this array operation, use a do loop
            if (NOVH > 0) then
                Values(1:NOVH) = RSLMAP2_ovh(8, 1:NOVH, 1)
            else
                success = .false.
            endif
    case(RRiPercolation)
    !RR Percolation in m3/s
            ! If a stack overflow occurs due to this array operation, use a do loop
            if (NOVH > 0) then
                Values(1:NOVH) = RSLMAP2_ovh(9, 1:NOVH, 1)
            else
                success = .false.
            endif
    case(RRiCapillaryRise)
    !RR Capillary Rise in m3/s
            ! If a stack overflow occurs due to this array operation, use a do loop
            if (NOVH > 0) then
                Values(1:NOVH) = RSLMAP2_ovh(10, 1:NOVH, 1)
            else
                success = .false.
            endif
    case (RRiGroundwaterlevel)
    !RR groundwaterlevels in m NAP
        Values = GWL
    case(RRiStorage_mm)
    !RR Storage Land in mm
            ! If a stack overflow occurs due to this array operation, use a do loop
            if (NOVH > 0) then
                Values(1:NOVH) = RSLMAP2_ovh(13, 1:NOVH, 1)
            else
                success = .false.
            endif
    case(RRiGroundwaterVolume)
    !RR Groundwater volume in m3
            ! If a stack overflow occurs due to this array operation, use a do loop
            if (NOVH > 0) then
                Values(1:NOVH) = RSLMAP2_ovh(14, 1:NOVH, 1)
            else
                success = .false.
            endif
    case(RRiStorage_m3)
    !RR Storage Land in m3
            ! If a stack overflow occurs due to this array operation, use a do loop
            if (NOVH > 0) then
                Values(1:NOVH) = RSLMAP2_ovh(15, 1:NOVH, 1)
            else
                success = .false.
            endif
    case(RRiGroundwaterLevelThreshold)
    !RR Groundwater level threshold in hour
            ! If a stack overflow occurs due to this array operation, use a do loop
            if (NOVH > 0) then
                Values(1:NOVH) = RSLMAP2_ovh(16, 1:NOVH, 1)
            else
                success = .false.
            endif
    case(RRiGroundwaterLevelSurface)
    !RR Groundwater level surface in m
            ! If a stack overflow occurs due to this array operation, use a do loop
            if (NOVH > 0) then
                Values(1:NOVH) = RSLMAP2_ovh(17, 1:NOVH, 1)
            else
                success = .false.
            endif
    case (RRiStoragecoeff)
    ! RR storage coefficient
        Values = BergC
    case (RRiUnsatZoneContent)
    ! RR Unsat Zone Content in mm
        Values = OnvZone%Actual_mm
    case(RRiUnsaturatedZoneVolume)
    !RR Unsaturated Zone Volume in m3
            ! If a stack overflow occurs due to this array operation, use a do loop
            if (NOVH > 0) then
                Values(1:NOVH) = RSLMAP2_ovh(22, 1:NOVH, 1)
            else
                success = .false.
            endif
    case default
    ! Something is wrong
        success = .false.
    end select

  end function

end module Unpaved
