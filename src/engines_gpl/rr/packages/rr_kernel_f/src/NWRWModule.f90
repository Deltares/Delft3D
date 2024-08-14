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
! at:               $Modtime:: 15-08-97 11:29a  $
!
! current revision: $Revision:: 4               $



module NWRW

  use Conf_fil
  use Conf_arr
  use Network
  use RR_Meteo
  use NewTables
  use DH_Alloc
  use RunoffFormulations
  use InfiltrationFormulations
  use Infiltration


  ! variables
    implicit none

  ! *** Data Pluvius knopen
  ! *** cf. Verhard gebied knopen
  ! areapv = oppervlak per knoop voor 4*3 klassen
  ! lvlpv  = maaiveldnivo
  ! bmaxop = max.berging op oppervlak voor 4*3 klassen
  ! bmxpri = max.berging Pluvius riool
  ! biniop = init.berging oppervlak per knoop
  ! binpri = init.berging Pluvius riool
  ! qrimax = max. capaciteit riool
  ! plvbnd = indicator riool naar randknoop of open water
  ! plvuse = lijst met indices klassen met areaal .gt. 0
  !  afvrtr = tijdfactor afstromingsvertraging voor 3*4 klassen
  !  afvrtr2 = geconverteerde afstromingsvertraging voor 3*4 klassen als dt<1min
  !  LnAfvrtr = de logaritme van Afvrtr
  ! tfincp = tijdfactor infiltratiecapaciteit voor 4 klassen, voor toe- en afname.
  !          (.,1) = AFname
  !          (.,2) = TOEfname
  ! infcap = min. en max. infiltratiecapaciteit voor 4 klassen
  !          (.,1) = maximim
  !          (.,2) = minimim
  ! infcp  = infiltratiecapaciteit per knoop/klasse
  !          (.,.,.,1) = maximim
  !          (.,.,.,2) = minimim
  ! iriool = indicator of riool wel(1) of niet(0) meegenomen wordt
  ! DWA - voor inwoners
  ! dwaopt = optie formulering (1, 2, 3 of 4)
  ! dwainw = aantal inwoners (=1 voor optie 3,4)
  ! dwadis(1) = constante DWA/capita in liter per uur (voor optie 1,3)
  ! dwadis(2) = dagwaarde DWA per capita in  liter/dag
  ! dwadis(3-26)= percentages voor verdeling DWADIS(2) over de dag
  ! DWA2 - voor bedrijven
  ! only DwaInw2, other parameters are not at NWRW node but at DWA definition level
  ! *** PLVBAL  (1) = total area                    (ha)
  !             (2) = total rain                    (m3)
  !             (3) = total evaporation             (m3)
  !             (4) = infiltration from depressions (m3)
  !             (5) = infiltration from runoff      (m3)
  !             (6) = delta storage                 (m3)
  !             (7) = inflow into sewer  excl. DWA  (m3)
  !             (8) = DWA inflow people into sewer  (m3)
  !             (9) = DWA inflow companies to sewer (m3)
  !             (10) = total inflow into sewer      (m3)

  REAL, Pointer, SAVE ::         AREAPV(:,:,:), &
                                     LVLPV(:), &
                                     INFCAP(:,:), &
                                     BMAXOP(:,:), &
!                                    BMXPRI(:), &
!                                    BINIOP(:,:,:), &
!                                    BINPRI(:), &
!                                    QRIMAX(:), &
                                     AFVRTR(:), &
                                     LnAFVRTR(:), &
                                     AFVRTR2(:), &
                                     TFINCP(:,:), &
                                     DWADIS(:,:)
  INTEGER, Pointer, SAVE ::      PLVBND(:), &
                                     PLVUSE(:,:,:), &
                                     IRIOOL(:), &
                                     PLVNAM(:), &
                                     DWAOPT(:)
  Real, Pointer, save ::         DWAINW(:), DWAINW2(:)
  Integer, Pointer, save ::      NwrwDWATable(:)
  Double Precision, pointer, save     ::      NWRW_MaxInfCap(:)
  Double Precision, pointer, save     ::      NWRW_MinInfCap(:)
  Double Precision, pointer, save     ::      NWRW_DecreaseInfCap(:)
  Double Precision, pointer, save     ::      NWRW_RecoveryInfCap(:)
  Double Precision, pointer, save     ::      NWRW_PreviousInfCap(:)
  Double Precision, pointer, save     ::      NWRW_NewInfCap(:)
  Double Precision, pointer, save     ::      NWRW_Dt(:)
  Double Precision, pointer, save     ::      NWRW_InitialStorage(:)
  Double Precision, pointer, save     ::      NWRW_Rainfall(:)
  Double Precision, pointer, save     ::      NWRW_InfiltrationMM(:)
  Integer, pointer, save     ::      NWRW_InfSts(:)

! LOGICAL, Pointer, SAVE ::      RI2OW(:)

  REAL, Pointer, SAVE ::         INFCP(:,:,:,:)

! ARS 11173-11174 *AFK record, NWRW area with special NWRW parameters
  Integer  MaxNrSpecialNwrwAreas, MaxTotNrSpecialNwrwAreas, TotNrSpecialNwrwAreas

  REAL, Pointer, SAVE ::                          SpecialNwrwAreas(:,:)
  INTEGER, Pointer, SAVE ::                       NrSpecialNwrwAreas(:), Reference2SpecialDef(:,:), SpecialNWRWAreaType(:,:)
  Character(Len=CharIdLength), Pointer, SAVE ::   SpecialDefinitions(:,:)

  INTEGER, Pointer, SAVE ::      SpecialNwrwAreaCompOption(:)      ! 0 = user defined special area, 1=green roof
  REAL, Pointer, SAVE ::         SpecialRunoffDelay(:), &
                                    SpecialRunoffDelay2(:), &
                                     SpecialMaxStorage(:), &
                                     SpecialMaxInfCap(:), &
                                     SpecialMinInfCap(:), &
                                     SpecialInfCapDecrease(:), &
                                     SpecialInfCapRecovery(:), &
                                     SpecialEvapFact(:)
  Logical, Pointer, SAVE ::      SpecialInfilDepression(:), SpecialInfilRunoff(:)
! options for green roofs
  Real   , Pointer, SAVE ::      SpecialNwrwAreaThetaMinPercnt(:), SpecialNwrwAreaThetaFieldCapPercnt(:), SpecialNwrwAreaThetaSatPercnt(:), &
                                 SpecialNwrwAreaKp(:), SpecialNwrwAreaKSat(:), SpecialNwrwAreaThetaInitPercnt(:)
  Real   , Pointer, SAVE ::      SpecialNwrwAreaThetaFinal(:), SpecialNwrwAreaEact(:), SpecialNwrwAreaTotalRunoff(:)
  Real   , Pointer, SAVE ::      SpecialNwrwAreaThetaMin(:), SpecialNwrwAreaThetaFieldCap(:), SpecialNwrwAreaThetaSat(:), &
                                 SpecialNwrwAreaThetaInit(:), SpecialNwrwAreaSoilThickness(:), SpecialNwrwAreaCropFact(:)
! End ARS 11173-74

! Wadi infiltration facility
  Integer, Pointer, SAVE ::      UseWadi(:),WadiDrainFormula(:)
  Real   , Pointer, SAVE ::      WadiSpillLvl(:), WadiDrainLvl(:),WadiBedLvl(:),WadiLength(:), WadiWidth(:),WadiSpillWidth(:),&
                                 WadiSpillDischargeCoef(:),WadiDrainExitWidth(:),WadiDrainExitHeight(:), WadiDrainDischargeCoef(:), WadiDrainContractionCoef(:),&
                                 WadiDrainInitialWaterLvl(:), WadiDrainGroundWaterLvl(:), WadiInfiltrationResistance(:), WadiPorosity(:)

  Real   , Pointer, SAVE ::      WadiInitialLevel(:), WadiFinalLevel(:)
  Real   , Pointer, SAVE ::      WadiInitialStorage(:), WadiFinalStorage(:), WadiInflow(:), WadiInfiltration(:), &
                                 WadiSpillOutflow(:), WadiDrainOutflow(:)

  Character(Len=CharIdLength), Pointer :: sobekNodeIDPluv(:)
  Integer, Pointer, SAVE ::  SobekHisLocPluv(:)
  REAL, Pointer, SAVE ::  SBKLVLPluv(:), SbkAreaPluv(:), SbkDepthPluv(:)

! ARS 12563 connection of other RR nodes to NWRW nodes
  REAL, Pointer, SAVE ::         QinPLUV(:)

  type QtoPluv
    Real totalPaved
    Real totalUnpaved
    Real totalGreenhouse
    Real totalStructure
    Real totalRwzi
    Real totalIndustry
    Real totalSacramento
  end type QtoPluv

  type (QtoPluv), allocatable :: QPluv(:)


  LOGICAL  INFDEP, INFAF, FixArs12253
  Real INW

! tbv waterbalans inloopmodel Pluvius

  Double Precision  PLVBAL(10)

  ! 26-03: resultaten per ingedikte knoop in mm; in uitvoer omzetten naar
  !        afzonderlijke resultaten
  !
  !        NPLV2 ipv NPLV
  !
  ! *** results Pluvius knopen
  ! ***
  ! *** INDIKP= relatie tussen pluvius knoop en 'ingedikt'
  ! *** COMPPV= logical, geeft aan of bij knoop IPLV de ingedikte knoop al afgehan
  ! *** BVOP  = berging oppervlak aan eind huidige tijdstap
  ! *** BVOP0 = berging oppervlak aan eind vorige tijdstap
  ! *** BPRL  = berging in Pluvius riool aan eind huidige tijdstap
  ! *** BPRL0 = berging in Pluvius riool aan eind vorige tijdstap
  ! *** RPV   = regenval oppervlak Pluvius huidige tijdstap
  ! *** VPV   = verdamping oppervlak Pluvius huidige tijdstap
  ! *** IPV   = infiltratie oppervlak Pluvius huidige tijdstap
  ! *** APV   = afstroming oppervlak Pluvius huidige tijdstap
  ! *** INPR  = instroom Pluvius riolering huidige tijdstap
  ! *** Q1PV  = overstort Pluvius riolering huidige tijdstap
  ! *** Q2PV  = uitmaling Pluvius riolering huidige tijdstap
  ! *** INFSTS= infiltratie status: 1 = afname, 2=toename
  ! ***         (.,.,.,1) voor infiltratie uit depressies
  ! ***         (.,.,.,2) voor infiltratie uit afstroming
  ! *** DT    = delta t in hours since last change of infiltr. curve
  ! *** NTRAIN= netto neerslag
  ! *** NTRRST= netto neerslag restant;
  ! ***          volume dynamisch opp. berging naar volgende tijdstap
  ! *** NTRRS0= netto neerslag restant eind vorige tijdstap;
  ! ***          volume dynamisch opp. berging naar huidige tijdstap
  ! *** INPRT = inflow pluvius riool in m3/s totaal
  ! *** INFDP  = infiltratie uit depressies m3/s, totaal
  ! *** INFLAF = infiltratie uit afstroming m3/s, per areaal
  ! *** VOLOP  = volume oppervlakte berging (voor netto neerslag)
  ! *** VOLDYN = dynamisch volume opp. berging (na netto neerslag)
  ! *** VOLOP0 = volume oppervlakte berging eind vorige tijdstap
  ! *** VOLDY0 = dynamisch volume opp. berging eind vorige tijdstap
  ! *** DWA    = droogweerafvoer personen
  ! *** DWA2   = droogweerafvoer bedrijven

  REAL, Pointer, SAVE :: &
               BVOP(:,:,:), BPRL(:,:,:), &
               BVOP0(:,:,:), BPRL0(:,:,:), &
               NTRAIN(:,:,:), NTRRST(:,:,:), NTRRS0(:,:,:), &
               INPR(:,:,:), INPRT(:), &
               RPV (:,:,:), VPV(:,:,:), &
               IPV (:,:,:), APV(:,:,:), &
               Q2PV(:,:,:), Q1PV(:,:,:), &
               INFDP(:), INFLAF(:,:,:), &
               VOLOP(:) , VOLDYN(:), &
               VOLOP0(:), VOLDY0(:), DWA(:), DWA2(:), DT(:,:,:,:)

  INTEGER, Pointer, SAVE ::    INFSTS(:,:,:,:), INDIKP(:)
  LOGICAL, Pointer, SAVE ::    COMPPV(:)

  Integer, Pointer, save ::  ReferenceToDwaDef(:), ReferenceToDwaDef2(:)

!Special area types
  Real, Pointer, Save ::  SpecialINFCP(:,:)

  REAL, Pointer, SAVE :: &
               SpecialBVOP(:), SpecialBPRL(:), &
               SpecialBVOP0(:), SpecialBPRL0(:), &
               SpecialNTRAIN(:), SpecialNTRRST(:), SpecialNTRRS0(:), &
               SpecialINPR(:), SpecialINPRT(:), &
               SpecialRPV (:), SpecialVPV(:), &
               SpecialIPV (:), SpecialAPV(:), &
               SpecialQ2PV(:), SpecialQ1PV(:), &
               SpecialINFDP(:), SpecialINFLAF(:), &
               SpecialVOLOP(:) , SpecialVOLDYN(:), &
               SpecialVOLOP0(:), SpecialVOLDY0(:), SpecialDT(:,:)

  INTEGER, Pointer, SAVE ::    SpecialINFSTS(:,:), SpecialINDIKP(:,:)
  LOGICAL, Pointer, SAVE ::    SpecialCOMPPV(:)

! NWRW Pluvius output
  ! *** PLVBPC = maximum bergingspercentage Pluvius oppervlak+riool
  ! ***          (3): 1 = oppervlak, 2=dynamisch, 3=riool
  ! *** PLVQOU = maximum debieten Pluvius knoop, per event
  ! ***          ( ,1) = Pluvius Rioolinloop
  ! ***          ( ,2) = Pluvius riool uitgemalen debiet
  ! ***          ( ,3) = Pluvius riool overstort
  ! ***          ( ,4) = regenval
  ! ***          ( ,5) = Evaporation
  ! ***          ( ,6) = RWA
  ! ***          ( ,7) = DWA people
  ! ***          ( ,8) = DWA companies to be added
    REAL, Pointer, SAVE ::    PLVBPC(:,:,:), PLVQOU(:,:,:)

! Nov 2002
! RunoffOutHis = 0  (old format)
!              =-1  (His format)
! SeparateRwa_Dwa = 0  (no)
!                 =-1  (yes)
! DwaString, RwaString = string to be added to id's NWRW runoff node in order to separate RWA and DWA
    Integer                 RunOffOutHis, SeparateRwa_Dwa
    Character(CharIdLength) DwaString, RwaString

contains



  subroutine NWRW_confAr0

    implicit none

!   standaard 3*4 typen oppervlak
    NPOPP =  3
    NPTYP =  4

!   max. extra speciale oppervlakken per object
    MaxNrSpecialNwrwAreas=6     ! was tot 5 juni =12; tot 13-3 2013 was het 4
    Return
  End subroutine NWRW_confAr0



  Subroutine NWRW_confAr1
    Integer iOut1, Allocation_error
    Logical success

    iOut1 = ConfFil_get_iOut1()

    NPLV = MAX (1, NCPLUV ) !NWRW
    MaxTotNrSpecialNwrwAreas = MaxNrSpecialNwrwAreas * NPlv * 2

    IF ((NCPLUV .GT. 0) .and. (ConfFil_get_iOut1() .gt. 0)) then
      WRITE(IOUT1,*) ' NWRW areas            =',NPLV
      WRITE(IOUT1,*) ' Max.Nr Special NWRW   =',MaxNrSpecialNwrwAreas
    end if

          !*** Data Pluvius knopen

    Success = DH_AllocInit (Nplv, NpTyp, NpOpp, AreaPv, 0E0)
    Success = success .and. DH_AllocInit (Nplv, LvlPv, 0E0)
    Success = success .and. DH_AllocInit (Nptyp, 2, InfCap, TfInCp,  0E0)
    Success = success .and. DH_AllocInit (Nptyp, NpOpp, BMaxOp, 0E0)
    Success = success .and. DH_AllocInit (NpOpp*Nptyp, AfVrTr, 0E0)
    Success = success .and. DH_AllocInit (NpOpp*Nptyp, AfVrTr2, 0E0)
    Success = success .and. DH_AllocInit (NpOpp*Nptyp, LnAfVrTr, 0E0)

    Success = success .and. DH_AllocInit (Nptyp*NpOpp, NWRW_MaxInfCap, 0D0)
    Success = success .and. DH_AllocInit (Nptyp*NpOpp, NWRW_MinInfCap, 0D0)
    Success = success .and. DH_AllocInit (Nptyp*NpOpp, NWRW_DecreaseInfCap, 0D0)
    Success = success .and. DH_AllocInit (Nptyp*NpOpp, NWRW_RecoveryInfCap, 0D0)
    Success = success .and. DH_AllocInit (Nptyp*NpOpp, NWRW_PreviousInfCap, 0D0)
    Success = success .and. DH_AllocInit (Nptyp*NpOpp, NWRW_NewInfCap, 0D0)
    Success = success .and. DH_AllocInit (Nptyp*NpOpp, NWRW_Dt, 0D0)
    Success = success .and. DH_AllocInit (Nptyp*NpOpp, NWRW_InitialStorage, 0D0)
    Success = success .and. DH_AllocInit (Nptyp*NpOpp, NWRW_Rainfall, 0D0)
    Success = success .and. DH_AllocInit (Nptyp*NpOpp, NWRW_InfiltrationMM, 0D0)
    Success = success .and. DH_AllocInit (Nptyp*NpOpp, NWRW_InfSts, 0)


    if (.not. success) call ErrMsgStandard (981, 0,' Error allocating arrays in subroutine ', ' NWRW_ConfAr1')

    Success = success .and. DH_AllocInit (NPlv, PlvBnd, IRiool, PlvNam, 0)
    Success = success .and. DH_AllocInit (NPlv, NPTyp, NpOpp, PlvUse, 0)

    Success = success .and. DH_AllocInit (NPlv, DwaInw, 0E0)
    Success = success .and. DH_AllocInit (NPlv, DwaInw2, 0E0)
    if (.not. success) call ErrMsgStandard (981, 0,' Error allocating arrays in subroutine ', ' NWRW_ConfAr1')

    Success = success .and. DH_AllocInit (NcPluvDwa, NwrwDwaTable, 0)
    Success = success .and. DH_AllocInit (NPlv, DWA, 0E0)
    Success = success .and. DH_AllocInit (NPlv, DWA2, 0E0)
    Success = success .and. DH_AllocInit (NPlv, InDikP, ReferenceToDwaDef, 0)
    Success = success .and. DH_AllocInit (NPlv, InDikP, ReferenceToDwaDef2, -1)
    if (.not. success) call ErrMsgStandard (981, 0,' Error allocating arrays in subroutine ', ' NWRW_ConfAr1')

! ARS 11173-11174 *AFK record, NWRW area with special NWRW parameters
    Success = success .and. DH_AllocInit (NPlv, MaxNrSpecialNwrwAreas, SpecialNwrwAreas, 0E0)
    Success = success .and. DH_AllocInit (NPlv, MaxNrSpecialNwrwAreas, SpecialNwrwAreaType, 0)
    Success = success .and. DH_AllocInit (NPlv, NrSpecialNwrwAreas, 0)
    Success = success .and. DH_AllocInit (NPlv, MaxNrSpecialNwrwAreas, Reference2SpecialDef, 0)
    Success = success .and. DH_AllocInit (NPlv, MaxNrSpecialNwrwAreas, SpecialDefinitions, ' ')
    Success = success .and. DH_AllocInit (MaxTotNrSpecialNwrwAreas, SpecialRunoffDelay, 0E0)
    Success = success .and. DH_AllocInit (MaxTotNrSpecialNwrwAreas, SpecialRunoffDelay2, 0E0)
    Success = success .and. DH_AllocInit (MaxTotNrSpecialNwrwAreas, SpecialMaxStorage, 0E0)
    Success = success .and. DH_AllocInit (MaxTotNrSpecialNwrwAreas, SpecialMaxInfCap, 0E0)
    Success = success .and. DH_AllocInit (MaxTotNrSpecialNwrwAreas, SpecialMinInfCap, 0E0)
    Success = success .and. DH_AllocInit (MaxTotNrSpecialNwrwAreas, SpecialInfCapDecrease, 0E0)
    Success = success .and. DH_AllocInit (MaxTotNrSpecialNwrwAreas, SpecialInfCapRecovery, 0E0)
    Success = success .and. DH_AllocInit (MaxTotNrSpecialNwrwAreas, SpecialEvapFact, 0E0)
    Success = success .and. DH_AllocInit (MaxTotNrSpecialNwrwAreas, SpecialInfilDepression, .false.)
    Success = success .and. DH_AllocInit (MaxTotNrSpecialNwrwAreas, SpecialInfilRunoff, .false. )
    if (.not. success) call ErrMsgStandard (981, 0,' Error allocating arrays in subroutine ', ' NWRW_ConfAr1')
    Success = success .and. DH_AllocInit (NPlv, MaxNrSpecialNwrwAreas, SpecialIndikP, 0)
    if (.not. success) call ErrMsgStandard (981, 0,' 27 Error allocating arrays in subroutine ', ' NWRW_ConfAr1')
! green roofs
    Success = success .and. DH_AllocInit (MaxTotNrSpecialNwrwAreas, SpecialNwrwAreaCompOption, 0)
    Success = success .and. DH_AllocInit (MaxTotNrSpecialNwrwAreas, SpecialNwrwAreaThetaMinPercnt, 0E0)
    Success = success .and. DH_AllocInit (MaxTotNrSpecialNwrwAreas, SpecialNwrwAreaThetaFieldCapPercnt, 0E0)
    Success = success .and. DH_AllocInit (MaxTotNrSpecialNwrwAreas, SpecialNwrwAreaThetaSatPercnt, 0E0)
    Success = success .and. DH_AllocInit (MaxTotNrSpecialNwrwAreas, SpecialNwrwAreaKp, 0E0)
    Success = success .and. DH_AllocInit (MaxTotNrSpecialNwrwAreas, SpecialNwrwAreaKSat, 0E0)
    Success = success .and. DH_AllocInit (MaxTotNrSpecialNwrwAreas, SpecialNwrwAreaThetaInitPercnt, 0E0)
    Success = success .and. DH_AllocInit (MaxTotNrSpecialNwrwAreas, SpecialNwrwAreaSoilThickness, 0E0)
    Success = success .and. DH_AllocInit (MaxTotNrSpecialNwrwAreas, SpecialNwrwAreaCropFact, 0E0)
    if (.not. success) call ErrMsgStandard (981, 0,' 271 Error allocating arrays in subroutine ', ' NWRW_ConfAr1')
! wadi infiltration
    Success = success .and. DH_AllocInit (NPlv, UseWadi, 0)
    Success = success .and. DH_AllocInit (NPlv, WadiDrainFormula, 1)
    Success = success .and. DH_AllocInit (NPlv, WadiSpillLvl, 0.)
    Success = success .and. DH_AllocInit (NPlv, WadiDrainLvl, 0.)
    Success = success .and. DH_AllocInit (NPlv, WadiBedLvl,0.)
    Success = success .and. DH_AllocInit (NPlv, WadiLength,0.)
    Success = success .and. DH_AllocInit (NPlv, WadiWidth,0.)
    Success = success .and. DH_AllocInit (NPlv, WadiSpillWidth,0.)
    Success = success .and. DH_AllocInit (NPlv, WadiSpillDischargeCoef,0.9)
    Success = success .and. DH_AllocInit (NPlv, WadiDrainExitWidth,0.)
    Success = success .and. DH_AllocInit (NPlv, WadiDrainExitHeight,0.)
    Success = success .and. DH_AllocInit (NPlv, WadiDrainDischargeCoef,0.9)
    Success = success .and. DH_AllocInit (NPlv, WadiDrainContractionCoef,0.63)
    Success = success .and. DH_AllocInit (NPlv, WadiDrainInitialWaterLvl,0.)
    Success = success .and. DH_AllocInit (NPlv, WadiDrainGroundWaterLvl,0.)
    Success = success .and. DH_AllocInit (NPlv, WadiInfiltrationResistance,1.)
    Success = success .and. DH_AllocInit (NPlv, WadiPorosity,0.5)

    Success = success .and. DH_AllocInit (NPlv, WadiInitialLevel,0.)
    Success = success .and. DH_AllocInit (NPlv, WadiInitialStorage,0.)
    Success = success .and. DH_AllocInit (NPlv, WadiFinalLevel,0.)
    Success = success .and. DH_AllocInit (NPlv, WadiFinalStorage,0.)
    Success = success .and. DH_AllocInit (NPlv, WadiInflow,0.)
    Success = success .and. DH_AllocInit (NPlv, WadiInfiltration,0.)
    Success = success .and. DH_AllocInit (NPlv, WadiSpillOutflow,0.)
    Success = success .and. DH_AllocInit (NPlv, WadiDrainOutflow,0.)
    if (.not. success) call ErrMsgStandard (981, 0,' 272 Error allocating arrays in subroutine ', ' NWRW_ConfAr1')

! ARS 12563
    Success = success .and. DH_AllocInit (Nplv, QinPluv, 0E0)
    Allocate (QPluv(NPlv), Stat=Allocation_error)
    Success = success .and. (Allocation_error .eq. 0)
    Success = success .and. Dh_AllocInit (NcPluv, SobekHisLocPluv, 0)
    Success = success .and. Dh_AllocInit (NcPluv, SobekNodeIdPluv, ' ')
    Success = success .and. DH_AllocInit (NcPluv, SbkLvlPluv, -999.99E0)
    Success = success .and. DH_AllocInit (NcPluv, SbkAreaPluv, 0E0)
    Success = success .and. DH_AllocInit (NcPluv, SbkDepthPluv, 0E0)
    if (.not. success) call ErrMsgStandard (981, 0,' 28 Error allocating arrays in subroutine ', ' NWRW_ConfAr1')

  Return
  End subroutine NWRW_confAr1


  Subroutine  NWRWOutput_Confar (Nevnt)
    Integer Nevnt
    Logical success

    Success = DH_AllocInit (Ncpluv, 3, Nevnt, PlvBpc, 0E0)
    Success = success .and. DH_AllocInit (NcPluv, 8, NEvnt, PlvQOu, 0E0)
    if (.not. success) call ErrMsgStandard (981, 0,' Error allocating arrays in subroutine ', ' NWRW_OutputConfAr')

! NWRW Pluvius output
!   ALLOCATE ( PLVBPC(NcPLUV,3,Nevnt), PLVQOU(NcPLUV,7,Nevnt), Stat=Allocation_Error )

   Return
  end subroutine NWRWOutput_confAr


  SUBROUTINE NWRW_CONFAR3
    ! detemine array size pluvius-NPLV2
    ! *** Data Pluvius knopen related to NPLV2

    Logical Success

    Success = Dh_AllocInit (Nplv2, NpTyp, NPOpp,2, InfCP, 0E0)
    Success = success .and. Dh_AllocInit (Nplv2, NpTyp, NPOpp, BVOP, BVOP0, 0E0)
    Success = success .and. Dh_AllocInit (Nplv2, NpTyp, NPOpp, BPRL, BPRL0, 0E0)
    Success = success .and. Dh_AllocInit (Nplv2, NpTyp, NPOpp, NtRain, NtrRst, NtrRs0, 0E0)
    Success = success .and. Dh_AllocInit (Nplv2, INPRT, 0E0)
    Success = success .and. Dh_AllocInit (Nplv2, NpTyp, NPOpp, INPR, RPV, VPV, 0E0)
    Success = success .and. Dh_AllocInit (Nplv2, NpTyp, NPOpp, IPV, APV, 0E0)
    Success = success .and. Dh_AllocInit (Nplv2, NpTyp, NPOpp, Q1PV, Q2PV, 0E0)
    Success = success .and. Dh_AllocInit (Nplv2, NpTyp, NPOpp, InflAf, 0E0)
    Success = success .and. Dh_AllocInit (Nplv2, InfDp, 0E0)
    Success = success .and. Dh_AllocInit (Nplv2, VolOp, VolOp0, 0E0)
    Success = success .and. Dh_AllocInit (Nplv2, VolDyn, VolDy0, 0E0)
    Success = success .and. Dh_AllocInit (Nplv2, NpTyp, NpOpp, 2, InfSts, 0)
    Success = success .and. Dh_AllocInit (Nplv2, NpTyp, NpOpp, 2, Dt, 0E0)
    Success = success .and. Dh_AllocInit (Nplv2, CompPv, .false. )
    if (.not. success)  call ErrMsgStandard (981, 0, ' Error allocating arrays in subroutine ',' NWRW_ConfAr3' )

!    ALLOCATE ( INFCP(NPLV2,NPTYP, NPOPP,2), Stat=Allocation_Error )
!    ALLOCATE ( BVOP (NPLV2,NPTYP,NPOPP), BPRL (NPLV2,NPTYP,NPOPP), &
!               BVOP0(NPLV2,NPTYP,NPOPP), BPRL0(NPLV2,NPTYP,NPOPP), &
!               NTRAIN(NPLV2,NPTYP,NPOPP), &
!               NTRRST(NPLV2,NPTYP,NPOPP), &
!               NTRRS0(NPLV2,NPTYP,NPOPP), &
!               INPR  (NPLV2,NPTYP,NPOPP), INPRT(NPLV2), &
!               RPV  (NPLV2,NPTYP,NPOPP), VPV  (NPLV2,NPTYP,NPOPP), &
!               IPV  (NPLV2,NPTYP,NPOPP), APV  (NPLV2,NPTYP,NPOPP), &
!               Q2PV (NPLV2,NPTYP,NPOPP), Q1PV (NPLV2,NPTYP,NPOPP), &
!               INFDP (NPLV2), INFLAF(NPLV2,NPTYP,NPOPP), &
!               VOLOP (NPLV2), VOLDYN(NPLV2), &
!               VOLOP0(NPLV2), VOLDY0(NPLV2), Stat=Allocation_Error )
!    ALLOCATE (INFSTS(NPLV2,NPTYP,NPOPP,2), &
!              DT(NPLV2,NPTYP,NPOPP,2), Stat=Allocation_Error )
!    ALLOCATE ( COMPPV(NPLV2), Stat=Allocation_Error )

    ! NPLV3 - for special area types

    Success = success .and. Dh_AllocInit (Nplv3, 2, SpecialInfCp, 0E0 )
    Success = success .and. Dh_AllocInit (Nplv3, SpecialBVOP, SpecialBVOP0, 0E0)
    Success = success .and. Dh_AllocInit (Nplv3, SpecialBPRL, SpecialBPRL0, 0E0)
    Success = success .and. Dh_AllocInit (Nplv3, SpecialNtRain, SpecialNtrRst, SpecialNtrRs0, 0E0)
    Success = success .and. Dh_AllocInit (Nplv3, SpecialINPRT, 0E0)
    Success = success .and. Dh_AllocInit (Nplv3, SpecialINPR, SpecialRPV, SpecialVPV, 0E0)
    Success = success .and. Dh_AllocInit (Nplv3, SpecialIPV, SpecialAPV, 0E0)
    Success = success .and. Dh_AllocInit (Nplv3, SpecialQ1PV, SpecialQ2PV, 0E0)
    Success = success .and. Dh_AllocInit (Nplv3, SpecialInflAf, 0E0)
    Success = success .and. Dh_AllocInit (Nplv3, SpecialInfDp, 0E0)
    Success = success .and. Dh_AllocInit (Nplv3, SpecialVolOp, SpecialVolOp0, 0E0)
    Success = success .and. Dh_AllocInit (Nplv3, SpecialVolDyn, SpecialVolDy0, 0E0)
    Success = success .and. Dh_AllocInit (Nplv3, 2, SpecialInfSts, 0)
    Success = success .and. Dh_AllocInit (Nplv3, 2, SpecialDt, 0E0)
    Success = success .and. Dh_AllocInit (Nplv3, SpecialCompPv, .false. )
! green roof calculation variables/results
    Success = success .and. DH_AllocInit (NPlv3, SpecialNwrwAreaThetaMin, 0E0)
    Success = success .and. DH_AllocInit (NPlv3, SpecialNwrwAreaThetaFieldCap, 0E0)
    Success = success .and. DH_AllocInit (NPlv3, SpecialNwrwAreaThetaSat, 0E0)
    Success = success .and. DH_AllocInit (Nplv3, SpecialNwrwAreaThetaInit, 0E0)
    Success = success .and. DH_AllocInit (NPlv3, SpecialNwrwAreaThetaFinal, 0E0)
    Success = success .and. DH_AllocInit (NPlv3, SpecialNwrwAreaEact, 0E0)
    Success = success .and. DH_AllocInit (NPlv3, SpecialNwrwAreaTotalRunoff, 0E0)
    if (.not. success)  call ErrMsgStandard (981, 0, ' Error allocating arrays in subroutine ',' NWRW_ConfAr3' )
!   ALLOCATE ( SpecialINFCP(NPLV3,2), Stat=Allocation_Error )
!   ALLOCATE ( SpecialBVOP (NPLV3), SpecialBPRL (NPLV3), &
!              SpecialBVOP0(NPLV3), SpecialBPRL0(NPLV3), &
!              SpecialNTRAIN(NPLV3), &
!              SpecialNTRRST(NPLV3), &
!              SpecialNTRRS0(NPLV3), &
!              SpecialINPR  (NPLV3), SpecialINPRT(NPLV3), &
!              SpecialRPV  (NPLV3), SpecialVPV  (NPLV3), &
!              SpecialIPV  (NPLV3), SpecialAPV  (NPLV3), &
!              SpecialQ2PV (NPLV3), SpecialQ1PV (NPLV3), &
!              SpecialINFDP (NPLV3), SpecialINFLAF(NPLV3), &
!              SpecialVOLOP (NPLV3), SpecialVOLDYN(NPLV3), &
!              SpecialVOLOP0(NPLV3), SpecialVOLDY0(NPLV3), Stat=Allocation_Error )
!   ALLOCATE (SpecialINFSTS(NPLV3,2), &
!             SpecialDT(NPLV3,2), Stat=Allocation_Error )
!   ALLOCATE ( SpecialCOMPPV(NPLV3), Stat=Allocation_Error )

    RETURN
  END subroutine NWRW_confar3





  subroutine NWRW_readAscii(infile1, infile2, infile3, Infile4)

    integer :: RetVal

    Integer(4)      infile1, infile2, infile3, infile4
    Integer         teller, i, j, k, iPTyp, iPOpp, iPlv, IDwa, Index, inod, nhlp, iout1, iecode, idebug
    Character(CharIdLength)   name, id, TableName, NodeId
    Character(1000) string
    Logical         allow, found, endfil, occurs, Err969, TabYesNo
    Integer         NrColumns, TableNr

    Parameter        (NHLP=30)
    Integer           IDUM(NHLP)
    Real              RDUM(NHLP)
    Double Precision  DDUM(NHLP)
    Character(CharIdLength) CDUM(NHLP)

    Logical, Pointer :: AlreadyRead(:)
    Character(CharIdLength), Pointer, save ::  DWADEF(:), DWATabDef(:), SpecialDefinitionRead(:)
    Character(CharIdLength), Pointer, save ::  DWADEF2(:)
    Logical Success

     Success = Dh_AllocInit (Nplv, DwaDef, ' ')
     Success = success .and. Dh_AllocInit (Nplv, DwaDef2, ' ')
     Success = success .and. Dh_AllocInit (NcPluvDwa, DwaTabDef, ' ')
     Success = success .and. Dh_AllocInit (MaxTotNrSpecialNwrwAreas, SpecialDefinitionRead, ' ')
     Success = success .and. Dh_AllocInit (NcPluv, AlreadyRead, .false.)
     if (.not. success)  call ErrMsgStandard (981, 0, ' Error allocating arrays in subroutine ',' NWRW_ReadAscii')

    iDebug = ConfFil_get_iDebug()
    PLVBAL = 0

! ARS 11173-74
    NrSpecialNwrwAreas = 0
    SpecialNwrwAreas = 0
    SpecialDefinitions = ''
    TotNrSpecialNwrwAreas = 0

    allow = .false.
    found = .false.
    iOut1 = ConfFil_get_iOut1()

! Read Pluvius.3B file
    call SetMessage(LEVEL_DEBUG, 'Read Pluvius.3b file')
    endfil = .false.
    teller = 0
    RetVal = 0
    Call SKPCOM (INfile1, ENDFIL,'ODS')
    Do while (.not. endfil)
       READ(Infile1,'(A1000)',END=21,ERR=150,IOSTAT=IECODE)  STRING
! skip regel als hij niet begint met juist keyword (NWRW)
       IF (STRING(1:4) .EQ. 'NWRW') Then
! NWRW node id
        RetVal = RetVal + GetVAR2 (STRING,' id ',1,' NWRW-ReadAscii',' Pluvius.3B file',IOUT1, &
                    ID, RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
        index = 0
        call fndNd2(index, id)
        if (index .gt. 0) then
         Inod = index
         iplv = EiNode(inod,2)
         if (EiNode(inod,3) .eq. 7) then   ! en is NWRW knoop
          if (AlreadyRead(iplv)) then
           call SetMessage(LEVEL_ERROR, 'Data for NWRW node '//id(1:Len_trim(id))//' double in datafile Pluvius.3B')
          else
           teller = teller + 1
           AlreadyRead(iplv) = .true.
           PLVNAM(iplv) = index
           id_nod2plvnam(index) = iplv
! NWRW surface level
           allow = .true.
           LVLPV(iplv) = -999.99
           RetVal = RetVal + GetVAR2(STRING,' sl ',2,' NWRW-ReadAscii',' Pluvius.3B file',IOUT1, &
                        ID, RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
           if (found) LVLPV(iplv) = RDUM(1)
           allow = .false.
! NWRW riool used?
           IRIOOL (iplv) = 0
! NWRW areas
           RetVal = RetVal + GetVRS2(STRING,' ar ',2,' NWRW-ReadAscii',' Pluvius.3B file', &
                  IOUT1, CDUM(1), RDUM(1), IDUM(1), nptyp*npopp, IflRtn)
           I = 0
           Do Iptyp=1,nptyp
             Do ipopp=1,npopp
               I = i+1
               Areapv(Iplv,iptyp,ipopp) = RDUM(I)
             Enddo
           Enddo
! NWRW number of people (april 2020: read as real instead of integer)
           RetVal = RetVal + GetVAR2(STRING,' np ',2,' NWRW-ReadAscii',' Pluvius.3B file',IOUT1, &
                        CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
           DWAINW(iplv) = RDUM(1)
! NWRW DWA definition people
           RetVal = RetVal + GetVAR2(STRING,' dw ',1,' NWRW-ReadAscii',' Pluvius.3B file',IOUT1, &
                        CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
           DWADEF(iplv) = CDUM(1)
           Call Upperc (DWADEF(iplv))
           ReferenceToDwaDef(iplv) = 0

! NWRW number of companies
           allow = .true.
           RetVal = RetVal + GetVAR2(STRING,' np2 ',2,' NWRW-ReadAscii',' Pluvius.3B file',IOUT1, &
                        CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
           DWAINW2(iplv) = 0
           if (found) DWAINW2(iplv) = RDUM(1)
! NWRW DWA definition companies (april 2020: read as real instead of integer)
           allow = .true.
           RetVal = RetVal + GetVAR2(STRING,' dw2 ',1,' NWRW-ReadAscii',' Pluvius.3B file',IOUT1, &
                        CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
           DWADEF2(iplv) = ''
           if (found) then
              DWADEF2(iplv) = CDUM(1)
              Call Upperc (DWADEF2(iplv))
              ReferenceToDwaDef2(iplv) = 0
           endif

! NWRW Meteostation definition
           allow = .false.
           RetVal = RetVal + GetVAR2(STRING,' ms ',1,' NWRW-ReadAscii',' Pluvius.3B file',IOUT1, &
                        CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, Iflrtn)
           NAMMET(index) = CDUM(1)
! areal adjustment factor rainfall on node, maybe missing,
           allow = .true.
           RetVal = RetVal + GetVAR2(STRING,' aaf ',2,' NWRW-readAscii',' paved.3b file',IOUT1, &
                            CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, Iflrtn)
           if (found) AAFNodeRainfall(index) = max(0.0, RDUM(1))     ! AAF >= 0
! additions for wadi infiltration facility
!          wadi use?
           allow = .true.
           RetVal = RetVal + GetVAR2(STRING,' uw ',3,' NWRW-readAscii',' paved.3b file',IOUT1, &
                                     CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, Iflrtn)
           UseWadi(iplv) = 0
           if (found) then
             if (idum(1) .ne. 0) UseWadi(iplv) = 1
           endif
           ! read wadi data
           if (UseWadi(iplv) .gt. 0) then
              allow = .false.
              RetVal = RetVal + GetVAR2(STRING,' wspl ',2,' NWRW-readAscii',' paved.3b file',IOUT1, &
                                        CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, Iflrtn)
              WadiSpillLvl(iplv) = RDUM(1)
              RetVal = RetVal + GetVAR2(STRING,' wdl ',2,' NWRW-readAscii',' paved.3b file',IOUT1, &
                                        CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, Iflrtn)
              WadiDrainLvl(iplv) = RDUM(1)
              RetVal = RetVal + GetVAR2(STRING,' wbl ',2,' NWRW-readAscii',' paved.3b file',IOUT1, &
                                        CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, Iflrtn)
              WadiBedLvl(iplv) = RDUM(1)
              RetVal = RetVal + GetVAR2(STRING,' wl ',2,' NWRW-readAscii',' paved.3b file',IOUT1, &
                                        CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, Iflrtn)
              WadiLength(iplv) = RDUM(1)
              RetVal = RetVal + GetVAR2(STRING,' ww ',2,' NWRW-readAscii',' paved.3b file',IOUT1, &
                                        CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, Iflrtn)
              WadiWidth(iplv) = RDUM(1)
              RetVal = RetVal + GetVAR2(STRING,' wsw ',2,' NWRW-readAscii',' paved.3b file',IOUT1, &
                                        CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, Iflrtn)
              WadiSpillWidth(iplv) = RDUM(1)
              RetVal = RetVal + GetVAR2(STRING,' wsdc ',2,' NWRW-readAscii',' paved.3b file',IOUT1, &
                                        CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, Iflrtn)
              WadiSpillDischargeCoef(iplv) = RDUM(1)
              RetVal = RetVal + GetVAR2(STRING,' wdf ',3,' NWRW-readAscii',' paved.3b file',IOUT1, &
                                        CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, Iflrtn)
              WadiDrainFormula(iplv) = IDUM(1)
              RetVal = RetVal + GetVAR2(STRING,' wddc ',2,' NWRW-readAscii',' paved.3b file',IOUT1, &
                                        CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, Iflrtn)
              WadiDrainDischargeCoef(iplv) = RDUM(1)
              RetVal = RetVal + GetVAR2(STRING,' wdew ',2,' NWRW-readAscii',' paved.3b file',IOUT1, &
                                        CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, Iflrtn)
              WadiDrainExitWidth(iplv) = RDUM(1)
              if (WadiDrainFormula(iplv) .eq. 0) then
                 RetVal = RetVal + GetVAR2(STRING,' wdcc ',2,' NWRW-readAscii',' paved.3b file',IOUT1, &
                                           CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, Iflrtn)
                 WadiDrainContractionCoef(iplv) = RDUM(1)
                 RetVal = RetVal + GetVAR2(STRING,' wdeh ',2,' NWRW-readAscii',' paved.3b file',IOUT1, &
                                           CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, Iflrtn)
                 WadiDrainExitHeight(iplv) = RDUM(1)
              endif
              RetVal = RetVal + GetVAR2(STRING,' wdiwl ',2,' NWRW-readAscii',' paved.3b file',IOUT1, &
                                        CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, Iflrtn)
              WadiDrainInitialWaterLvl(iplv) = RDUM(1)
              RetVal = RetVal + GetVAR2(STRING,' wgwl ',2,' NWRW-readAscii',' paved.3b file',IOUT1, &
                                        CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, Iflrtn)
              WadiDrainGroundWaterLvl(iplv) = RDUM(1)
              RetVal = RetVal + GetVAR2(STRING,' wir ',2,' NWRW-readAscii',' paved.3b file',IOUT1, &
                                        CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, Iflrtn)
              WadiInfiltrationResistance(iplv) = RDUM(1)
              RetVal = RetVal + GetVAR2(STRING,' wp ',2,' NWRW-readAscii',' paved.3b file',IOUT1, &
                                        CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, Iflrtn)
              WadiPorosity(iplv) = RDUM(1)
           endif
! ARS 11173-74 Feb 2003: add special area acc. to *AFK records SufHyd
           Allow = .true.
!          number of special definitions
           NrSpecialNwrwAreas(iplv) = 0
           RetVal = RetVal + GetVAR3(STRING,' na ',3,' NWRW-ReadAscii',' Pluvius.3B file',IOUT1, &
                        CDUM(1), RDUM(1), IDUM(1), DDUM(1), ALLOW, FOUND, IflRtn)
           if (found) NrSpecialNwrwAreas(iplv) = IDUM(1)
           Allow = .false.
           if (NrSpecialNwrwAreas(iplv) .gt. MaxNrSpecialNwrwAreas) then
               call ErrMsgStandard (912, 0, ' Nwrw-ReadAscii ',' NWRW areas with special parameters')
           endif
!          nwrw special area
           If (NrSpecialNwrwAreas(iplv) .gt. 0) then
              found = .false.
              allow = .false.
              RetVal = RetVal + GetVRS3(STRING,' aa ',2,' NWRW-ReadAscii',' Pluvius.3B file',IOUT1, &
                           CDUM(1), RDUM(1), IDUM(1), DDUM(1), NrSpecialNwrwAreas(iplv), allow, found, Iflrtn)
              Do j=1, NrSpecialNwrwAreas(iplv)
                 SpecialNwrwAreas (Iplv,j) = Rdum(j)
              Enddo
              allow = .true.
              RetVal = RetVal + GetVRS3(STRING,' at ',3,' NWRW-ReadAscii',' Pluvius.3B file',IOUT1, &
                           CDUM(1), RDUM(1), IDUM(1), DDUM(1), NrSpecialNwrwAreas(iplv), allow, found, Iflrtn)
              if (found) then
                 Do j=1, NrSpecialNwrwAreas(iplv)
                    SpecialNwrwAreaType(Iplv,j) = Idum(j)
                 Enddo
              else  !default:original special areatype = 0 meaning user defined special area  (1=greenroof)
                 Do j=1, NrSpecialNwrwAreas(iplv)
                    SpecialNwrwAreaType(Iplv,j) = 0
                 Enddo
              endif
              allow = .false.
!             nwrw definitions of special
              RetVal = RetVal + GetVRS3(STRING,' nw ',1,' NWRW-ReadAscii',' Pluvius.3B file',IOUT1, &
                           CDUM(1), RDUM(1), IDUM(1), DDUM(1), NrSpecialNwrwAreas(iplv), allow, found, Iflrtn)
              Do j=1, NrSpecialNwrwAreas(iplv)
                 SpecialDefinitions (Iplv,j) = Cdum(j)
              Enddo
           Endif

! bepaal totaal areaal voor waterbalans
           DO ipopp=1,npopp
              DO iptyp=1,nptyp
                 plvbal(1) = plvbal(1) + areapv(iplv,iptyp,ipopp)
              ENDDO
           ENDDO
           ! add special areas
           If (NrSpecialNwrwAreas(iplv) .gt. 0) then
              Do j=1, NrSpecialNwrwAreas(iplv)
                 plvbal(1) = plvbal(1) + SpecialNwrwAreas(iplv,j)
              Enddo
           Endif
          Endif
         Endif
        Endif
      Endif
      Call SKPCOM (INfile1, ENDFIL,'ODS')
    enddo
 21 Continue
    If (RetVal .gt. 0) call ErrMsgStandard(972,0,' Error reading Pluvius.3B file ', ' Getting NWRW records')
    If (teller .lt. NcPluv)  Then
        Do inod=1,NcNode
          iplv = EiNode(inod,2)
          if (EiNode(inod,3) .eq. 7) then   ! en is NWRW node
            if (.not. AlReadyRead(iplv)) then
              NodeId = ' '
              NodeId = Id_Nod(inod)
              String = ' '
              String = ' ' // NodeId(1:Len_trim(NodeId)) // ' is missing'
              call ErrMsgStandard (977, 0, ' Data for NWRW node ',String(1:Len_trim(String)) )
            endif
          endif
        Enddo
       call ErrMsgStandard (972, 0, ' Not enough data for all NWRW-nodes in schematisation found', &
                            ' Some NWRW-nodes from schematisation not present in Pluvius.3B file')
    Endif

!Pluvius.Dwa file
! first determine number records (=DWA-definitions) in DWA file
    endfil = .false.
    Allow = .false.
    call SetMessage(LEVEL_DEBUG, 'Read Pluvius.Dwa file')
    teller = 0
    do while (.not. endfil)
       Call SKPCOM (INfile3, ENDFIL,'ODS')
       If (endfil) goto 210
       READ(Infile3,'(A1)') String
       teller = teller+1
    enddo
210 Continue
    teller = max (1, teller)
    Rewind(infile3)

! test
    Success = success .and. Dh_AllocInit (Teller, DwaOpt, 0)
    Success = success .and. Dh_AllocInit (Teller, 26, DwaDis, 0E0)
    if (.not. success)  call ErrMsgStandard (981, 0, ' Error allocating arrays in subroutine ',' NWRW_ReadAscii')
! end test

    endfil = .false.
    teller = 0
    RetVal = 0
    Call SKPCOM (INfile3, ENDFIL,'ODS')
    do while (.not. endfil)
       READ(Infile3,'(A1000)',END=211,ERR=150,IOSTAT=IECODE)  STRING
       if (idebug .ne. 0) write(idebug,*) ' Read from DWA file', String(1:120)
       IF (STRING(1:4) .EQ. 'DWA') Then
! DWA definition node id
           RetVal = RetVal + GetVAR2 (STRING,' id ',1,' NWRW-ReadAscii',' Pluvius.DWA file',IOUT1, &
                        ID, RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
           Name = id
           Call Upperc (Name)
!          Eerst testen of DWA definition wel gebruikt wordt, dan pas verwerken
           if (idebug .ne. 0) Then
               Write(Idebug,*) ' Search for dwa definition ',Trim(Name)
               Write(Idebug,*) ' length of array           ',NcPluv
               Write(Idebug,*) ' CaseSensitive             ',CaseSensitive
               Write(Idebug,*) ' DWADEF                    '
               Write(Idebug,*) (i,DwaDef(i)(1:Len_trim(DwaDef(i))),i=1,ncpluv)
               Write(Idebug,*) ' DWADEF2                    '
               Write(Idebug,*) (i,DwaDef2(i)(1:Len_trim(DwaDef2(i))),i=1,ncpluv)
           Endif
           ! test DWA people
           Iplv = FindString (Ncpluv, DWAdef, Name, NcPluv, CaseSensitive)
           Occurs = (Iplv .gt. 0)
           if (Iplv .gt. 0) then
              if (ReferenceToDWADEF(iplv) .gt. 0) then
               call SetMessage(LEVEL_ERROR, 'DWA Definition '//name(1:Len_trim(Name))//' double in datafile Pluvius.Dwa')
              endif
           endif
           ! test DWA companies
           Iplv = FindString (Ncpluv, DWAdef2, Name, NcPluv, CaseSensitive)
           Occurs = occurs .or. (Iplv .gt. 0)
           if (Iplv .gt. 0) then
              if (ReferenceToDWADEF2(iplv) .gt. 0) then
               call SetMessage(LEVEL_ERROR, 'DWA Definition '//name(1:Len_trim(Name))//' double in datafile Pluvius.Dwa')
              endif
           endif

!          Verwerk DWA definitie
           if (occurs) then
              teller = teller + 1
! DWA keyword do: computation option
              RetVal = RetVal + GetVAR2 (STRING,' do ',3,' NWRW-ReadAscii',' Pluvius.DWA file',IOUT1, &
                           ID, RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
              DwaOpt(teller) = Idum(1)
! DWA keyword wc
              RetVal = RetVal + GetVAR2 (STRING,' wc ',2,' NWRW-ReadAscii',' Pluvius.DWA file',IOUT1, &
                           ID, RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
              DwaDis(teller,1) = rdum(1)
! DWA keyword wd
              RetVal = RetVal + GetVAR2 (STRING,' wd ',2,' NWRW-ReadAscii',' Pluvius.DWA file',IOUT1, &
                           ID, RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
              DwaDis(teller,2) = rdum(1)
! DWA 24 hours
              RetVal = RetVal + GetVRS2 (STRING,' wh ',2,' NWRW-ReadAscii',' Pluvius.DWA file', &
                           IOUT1, CDUM(1), RDUM(1), IDUM(1), 24, IflRtn)
              Do I=1,24
                 DwaDis(teller,i+2) = rdum(i)
              Enddo
! DWA keyword dt
              if (DwaOpt(teller) .eq. 5) then
                RetVal = RetVal + GetVAR2 (STRING,' dt ',1,' NWRW-ReadAscii',' Pluvius.DWA file',IOUT1, &
                             ID, RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
                DwaTabDef(teller) = ID
              endif

! Assign definition to individual nodes - DWA people and companies
              Do Iplv = 1, Ncpluv
                 if (StringComp(DWADEF(iplv),Name,CaseSensitive)) ReferenceToDwaDef(iplv) = teller
              Enddo
              Do Iplv = 1, Ncpluv
                 if (StringComp(DWADEF2(iplv),Name,CaseSensitive)) ReferenceToDwaDef2(iplv) = teller
              Enddo
           Endif
       Endif
       Call SKPCOM (INfile3, ENDFIL,'ODS')
    Enddo
211 CONTINUE

    If (RetVal .gt. 0) call ErrMsgStandard(972,0,' Error reading Pluvius.Dwa file ', ' Getting DWA  records')
    Err969 = .false.
    Do Iplv = 1, Ncpluv
       If (ReferenceToDwaDef(iplv) .le. 0)  Then
          if (idebug .ne. 0) write(idebug,*) ' Error for NWRW node ', Iplv, ReferenceToDwaDef(Iplv), Dwadef(iplv)
          call ErrMsgStandard (969, 0, ' DWA-definitions people not present in Pluvius.DWA data file.', DWADef(iplv))
          Err969 = .true.
       Endif
       ! for DWADef2 companies, >0 is found, 0 means it should have been found, while <0 means no DWAcompanies defined (default)
       If (ReferenceToDwaDef2(iplv) .eq. 0)  Then
          if (idebug .ne. 0) write(idebug,*) ' Error for NWRW node ', Iplv, ReferenceToDwaDef2(Iplv), Dwadef2(iplv)
          call ErrMsgStandard (969, 0, ' DWA-definitions companies not present in Pluvius.DWA data file.', DWADef2(iplv))
          Err969 = .true.
       Endif
    Enddo
    If (Err969) call ErrMsgStandard (972, 0, ' Not enough data for NWRW-nodes found', &
                                     ' Some DWA-definitions not present in NWRW data files')

!Pluvius.Tbl file
    call SetMessage(LEVEL_DEBUG, 'Read Pluvius.tbl file')
    if (idebug .ne. 0) write(idebug,*) ' Read Pluvius.Tbl file'
!   Vector/Array initialisation
    NwrwDWATable = 0
    AlreadyRead  = .false.
    Allow = .false.

    Endfil = .false.
    Call SKPCOM (Infile4, ENDFIL,'ODS')
    Do while (.not. endfil)
       Success = GetRecord(Infile4, 'DW_T', Endfil, idebug, Iout1)  ! get record van keyword DW_T tot dw_t, zet in buffer
       IF (ENDFIL) GOTO 5111
       Success = GetTableName (TabYesNo, TableName, ' id ', Iout1)     ! get table name via keyword ' id ', TabYesNo=TBLE found
       IF (.not. Success) GOTO 5111
       If (TabYesNo .and. TableName .ne. '') Then
!         Er is een tabel gedefinieerd, met een niet-lege naam
!         Eerst testen of tabel definition wel gebruikt wordt
          IPlv = FindString (NcPluvDwa, DWATabdef, TableName, NcPluvDwa, CaseSensitive)
          Occurs = (IPlv .gt. 0)
          NrColumns = 1
          if (IPlv .gt. 0) then
             if ( AlreadyRead (iPlv) ) then
               call SetMessage(LEVEL_ERROR, 'NWRW DWA table Definition '//Tablename(1:Len_trim(TableName))//' double in datafile Pluvius.Tbl')
             endif
             AlreadyRead (iPlv) = .true.
          endif
!         Verwerken tabel definitie
          if (occurs .and. NrColumns .gt. 0) then
! Get table with name TableName, Nrcolumns data fields, result in global arrays; tabel nummer is TableNr
             Success = GetTable (TableHandle, TableName, NrColumns, TableNr, idebug, Iout1)
             IF (.not. Success) GOTO 5111
! Set references
             Do iDwa=1, NcPluvDwa
                if ( (DWAOPT(iDwa) .eq. 5) .and. &
                      (StringComp (DWATabDef(iDwa), TableName, CaseSensitive) ) )  NwrwDWATable(iDwa) = TableNr
             Enddo
          Endif
       Endif
       Call SKPCOM (Infile4, ENDFIL,'ODS')
     Enddo
5111 Continue

    Err969 = .false.
    Do IDwa = 1, NcpluvDwa
       If (DWAOpt(Idwa) .eq. 5 .and. NwrwDwaTable(IDwa) .le. 0)  Then
          if (idebug .ne. 0) write(idebug,*) ' Error for NWRW DWA definition ', IDwa, NwrwDwaTable(Idwa), DwaTabdef(idwa)
          call ErrMsgStandard (969, 0, ' DWA-Table definitions not present in Pluvius.Tbl data file.', DWATabDef(idwa))
          Err969 = .true.
       Endif
    Enddo
    If (Err969) call ErrMsgStandard (972, 0, ' Not enough data for NWRW-nodes found', &
                                     ' Some DWA-Table definitions not present in NWRW table data file')




!Pluvius.Alg file
!Pluvius.Alg file, PLVG record
    Allow = .false.
    endfil = .false.
    call SetMessage(LEVEL_DEBUG, 'Read Pluvius.Alg file - PLVG')
    teller = 0
    RetVal = 0
    Call SKPCOM (INfile2, ENDFIL,'ODS')
    Do while (.not. endfil)
        READ(Infile2,'(A1000)',END=2111,ERR=150,IOSTAT=IECODE)  STRING
        if (idebug .ne. 0) write(idebug,*) ' Read from ALG file', String(1:120)
        IF (STRING(1:4) .Eq. 'PLVG')  Then
           teller = teller + 1
! ALG keyword ru: runoff coefficients
!           RetVal = RetVal + GetVRS2 (STRING,' ru ',2,' NWRW-ReadAscii',' Pluvius.ALG file', &
!                        IOUT1, CDUM(1), RDUM(1), IDUM(1), 3, IflRtn)
           allow = .true.
           RetVal = RetVal + GetVRS3 (STRING,' ru ',2,' NWRW-ReadAscii',' Pluvius.3B file',IOUT1, &
                               CDUM(1), RDUM(1), IDUM(1), DDUM(1), NpOpp, allow, found, Iflrtn)
           allow = .false.
           if (found) then
             AFVRTR(1) = RDUM(1)
             AFVRTR(2) = RDUM(2)
             AFVRTR(3) = RDUM(3)
! jan 2009: if ru defined, use for all types of surfaces the same value
             DO Iptyp=1,nptyp
                DO ipopp=1,npopp
                  Afvrtr ((iptyp-1)*npopp + ipopp) = RDUM(ipopp)
               ENDDO
             ENDDO
             Call VerifyAfVrTr
           endif
! ALG keyword ms: maximum storage surface
           RetVal = RetVal + GetVRS2 (STRING,' ms ',2,' NWRW-ReadAscii',' Pluvius.ALG file', &
                        IOUT1, CDUM(1), RDUM(1), IDUM(1), 12, IflRtn)
           I = 0
           DO Iptyp=1,nptyp
              DO ipopp=1,npopp
                I = I+1
                Bmaxop(iptyp,ipopp) = RDUM(I)
             ENDDO
           ENDDO
! jan 2009
! ALG keyword rf: runoff factors; if defined, overwrites ru keyword
!          RetVal = RetVal + GetVRS2 (STRING,' rf ',2,' NWRW-ReadAscii',' Pluvius.ALG file', &
!                       IOUT1, CDUM(1), RDUM(1), IDUM(1), 12, IflRtn)
           allow = .true.
           RetVal = RetVal + GetVRS3 (STRING,' rf ',2,' NWRW-ReadAscii',' Pluvius.3B file',IOUT1, &
                               CDUM(1), RDUM(1), IDUM(1), DDUM(1), Nptyp*NpOpp, allow, found, Iflrtn)
           allow = .false.
           if (found) then
              I = 0
              DO Iptyp=1,nptyp
                 DO ipopp=1,npopp
                   I = I+1
                   Afvrtr((iptyp-1)*npopp + ipopp) = RDUM(I)
                ENDDO
              ENDDO
              Call VerifyAfVrTr
           endif
! end jan 2009
! ALG keyword ix: maximum infiltration capacity
           RetVal = RetVal + GetVRS2 (STRING,' ix ',2,' NWRW-ReadAscii',' Pluvius.ALG file', &
                        IOUT1, CDUM(1), RDUM(1), IDUM(1), 4, IflRtn)
           DO iptyp=1,nptyp
              INFCAP(IPTYP, 1) = Rdum(iptyp)
           ENDDO

           DO ipopp=1,npopp
             DO Iptyp=1,nptyp
                NWRW_MaxInfCap((ipopp-1)*nptyp + iptyp) = dble (Rdum(iptyp))
             ENDDO
           ENDDO

!          write(*,*) ' Max. InfCap', InfCap(nptyp,1)
! ALG keyword im: minimum infiltration capacity
           RetVal = RetVal + GetVRS2 (STRING,' im ',2,' NWRW-ReadAscii',' Pluvius.ALG file', &
                        IOUT1, CDUM(1), RDUM(1), IDUM(1), 4, IflRtn)
           DO iptyp=1,nptyp
              INFCAP(IPTYP, 2) = Rdum(iptyp)
           ENDDO
           DO ipopp=1,npopp
             DO Iptyp=1,nptyp
                NWRW_MinInfCap((ipopp-1)*nptyp + iptyp) = dble (Rdum(iptyp))
             ENDDO
           ENDDO
! ALG keyword ic: DECrease infiltration capacity
           RetVal = RetVal + GetVRS2 (STRING,' ic ',2,' NWRW-ReadAscii',' Pluvius.ALG file',  &
                        IOUT1, CDUM(1), RDUM(1), IDUM(1), 4, IflRtn)
           DO iptyp=1,nptyp
              TFINCP(IPTYP, 1) = Rdum(iptyp)
           ENDDO
           DO ipopp=1,npopp
             DO Iptyp=1,nptyp
                NWRW_DecreaseInfCap((ipopp-1)*nptyp + iptyp) = dble (Rdum(iptyp))
             ENDDO
           ENDDO
! ALG keyword dc: INCrease infiltration capacity
           RetVal = RetVal + GetVRS2 (STRING,' dc ',2,' NWRW-ReadAscii',' Pluvius.ALG file',  &
                       IOUT1, CDUM(1), RDUM(1), IDUM(1), 4, IflRtn)
           DO iptyp=1,nptyp
             TFINCP(IPTYP, 2) = Rdum(iptyp)
           ENDDO
           DO ipopp=1,npopp
             DO Iptyp=1,nptyp
                NWRW_RecoveryInfCap((ipopp-1)*nptyp + iptyp) = dble (Rdum(iptyp))
             ENDDO
           ENDDO
! ALG od
           RetVal = RetVal + GetVAR2 (STRING,' od ',3,' NWRW-ReadAscii',' Pluvius.ALG file',IOUT1, &
                        ID, RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
           INFDEP = (idum(1) .ne. 0)
! ALG or
           RetVal = RetVal + GetVAR2 (STRING,' or ',3,' NWRW-ReadAscii',' Pluvius.ALG file',IOUT1, &
                        ID, RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
           INFAF = (idum(1) .ne. 0)
        Endif
        Call SKPCOM (INfile2, ENDFIL,'ODS')
     Enddo
2111 CONTINUE
     If (RetVal .gt. 0) call ErrMsgStandard(972,0,' Error reading Pluvius.Alg file ', ' Getting PLVG records')
     If (teller .le. 0)  then
         call ErrMsgStandard (972, 0, ' Not enough data for NWRW-nodes in schematisation found', &
                              ' No data in NWRW datafile Pluvius.Alg; PLVG record')
     Endif


!Pluvius.Alg file, PLVA, PLGR records
    rewind (infile2)
    Allow = .false.
    endfil = .false.
    call SetMessage(LEVEL_DEBUG, 'Read Pluvius.Alg file - PLVA')
    teller = 0
    RetVal = 0
    Reference2SpecialDef = 0
    Call SKPCOM (INfile2, ENDFIL,'ODS')
    Do while (.not. endfil)
        READ(Infile2,'(A1000)',END=3111,ERR=150,IOSTAT=IECODE)  STRING
        if (idebug .ne. 0) write(idebug,*) ' Read from ALG file', String(1:120)
        IF (STRING(1:4) .Eq. 'PLVA')  Then
           teller = teller + 1
           If (teller .gt. MaxTotNrSpecialNwrwAreas) then
              call ErrMsgStandard(972,0,' Error reading Pluvius.Alg file ', ' too many PLVA/PLGR records; delete unused special area definitions from file or distribute over larger number of NWRW nodes')
           endif
! id of PLVA record
           RetVal = RetVal + GetVAR2 (STRING,' id ',1,' NWRW-ReadAscii',' Pluvius.ALG file', &
                        IOUT1, CDUM(1), RDUM(1), IDUM(1), Allow, Found, IflRtn)
           SpecialDefinitionRead(teller) = CDUM(1)
           SpecialNwrwAreaCompOption(teller) = 0     ! PLVA record -> special area
!  keyword ru: runoff coefficients
           Allow = .false.
           endfil = .false.
           RetVal = RetVal + GetVAR2 (STRING,' ru ',2,' NWRW-ReadAscii',' Pluvius.ALG file', &
                        IOUT1, CDUM(1), RDUM(1), IDUM(1), Allow, Found, IflRtn)
           SpecialRunoffDelay(teller) = RDUM(1)
!  keyword ms: maximum storage surface
           RetVal = RetVal + GetVAR2 (STRING,' ms ',2,' NWRW-ReadAscii',' Pluvius.ALG file', &
                        IOUT1, CDUM(1), RDUM(1), IDUM(1), Allow, Found, IflRtn)
           SpecialMaxStorage(teller) = RDUM(1)
!  keyword ix: maximum infiltration capacity
           RetVal = RetVal + GetVAR2 (STRING,' ix ',2,' NWRW-ReadAscii',' Pluvius.ALG file', &
                        IOUT1, CDUM(1), RDUM(1), IDUM(1), Allow, Found, IflRtn)
           SpecialMaxInfCap(teller) = Rdum(1)
!  keyword im: minimum infiltration capacity
           RetVal = RetVal + GetVAR2 (STRING,' im ',2,' NWRW-ReadAscii',' Pluvius.ALG file', &
                        IOUT1, CDUM(1), RDUM(1), IDUM(1), Allow, Found, IflRtn)
           SpecialMinInfCap(teller) = Rdum(1)
!  keyword ic: DEcrease infiltration capacity
           RetVal = RetVal + GetVAR2 (STRING,' ic ',2,' NWRW-ReadAscii',' Pluvius.ALG file',  &
                        IOUT1, CDUM(1), RDUM(1), IDUM(1), Allow, Found, IflRtn)
           SpecialInfCapDecrease(teller) = Rdum(1)
!  keyword dc: INcrease infiltration capacity
           RetVal = RetVal + GetVAR2 (STRING,' dc ',2,' NWRW-ReadAscii',' Pluvius.ALG file',  &
                        IOUT1, CDUM(1), RDUM(1), IDUM(1), Allow, Found, IflRtn)
           SpecialInfCapRecovery(teller) = Rdum(1)
!  keyword ef: evaporation factor
           RetVal = RetVal + GetVAR2 (STRING,' ef ',2,' NWRW-ReadAscii',' Pluvius.ALG file',  &
                        IOUT1, CDUM(1), RDUM(1), IDUM(1), Allow, Found, IflRtn)
           SpecialEvapFact(teller) = Rdum(1)
!  keyword od
           RetVal = RetVal + GetVAR2 (STRING,' od ',3,' NWRW-ReadAscii',' Pluvius.ALG file',IOUT1, &
                        ID, RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
           SpecialInfilDepression(teller) = (idum(1) .ne. 0)
!  keyword or
           RetVal = RetVal + GetVAR2 (STRING,' or ',3,' NWRW-ReadAscii',' Pluvius.ALG file',IOUT1, &
                        ID, RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
           SpecialInfilRunoff(teller) = (idum(1) .ne. 0)
        ELSEIF (STRING(1:4) .Eq. 'PLGR')  Then
           teller = teller + 1
           If (teller .gt. MaxTotNrSpecialNwrwAreas) then
              call ErrMsgStandard(972,0,' Error reading Pluvius.Alg file ', ' too many PLVA/PLGR records; delete unused special area definitions from file or distribute over larger number of NWRW nodes')
           endif
! id of PLGR record
           RetVal = RetVal + GetVAR2 (STRING,' id ',1,' NWRW-ReadAscii',' Pluvius.ALG file', &
                        IOUT1, CDUM(1), RDUM(1), IDUM(1), Allow, Found, IflRtn)
           SpecialDefinitionRead(teller) = CDUM(1)
           SpecialNwrwAreaCompOption(teller) = 1     ! PLGR record -> green roof
! green roof data
!  keyword thetamin
              RetVal = RetVal + GetVAR2 (STRING,' thwp ', 2,' NWRW-ReadAscii',' Pluvius.ALG file',  &
                                        IOUT1, CDUM(1), RDUM(1), IDUM(1), Allow, Found, IflRtn)
              SpecialNwrwAreaThetaMinPercnt(teller) = Rdum(1)
!  keyword thetafieldcap
              RetVal = RetVal + GetVAR2 (STRING,' thfc ', 2,' NWRW-ReadAscii',' Pluvius.ALG file',  &
                                        IOUT1, CDUM(1), RDUM(1), IDUM(1), Allow, Found, IflRtn)
              SpecialNwrwAreaThetaFieldCapPercnt(teller) = Rdum(1)
!  keyword thetasat
              RetVal = RetVal + GetVAR2 (STRING,' thsat ', 2,' NWRW-ReadAscii',' Pluvius.ALG file',  &
                                        IOUT1, CDUM(1), RDUM(1), IDUM(1), Allow, Found, IflRtn)
              SpecialNwrwAreaThetaSatPercnt(teller) = Rdum(1)
              if (SpecialNwrwAreaThetaMinPercnt(teller) .gt. SpecialNwrwAreaThetaFieldCapPercnt(teller) .or. &
                    SpecialNwrwAreaThetaFieldCapPercnt(teller) .gt. SpecialNwrwAreaThetaSatPercnt(teller) ) then
                 call ErrMsgStandard (972, 0, ' ThetaMin should be <= ThetaFieldCapacity <= ThetaSat' , &
                                      ' Check green roof data NWRW datafile Pluvius.Alg; PLVA records')
              endif
!  keyword sl thickness of soil layer
              RetVal = RetVal + GetVAR2 (STRING,' sl ', 2,' NWRW-ReadAscii',' Pluvius.ALG file',  &
                                        IOUT1, CDUM(1), RDUM(1), IDUM(1), Allow, Found, IflRtn)
              SpecialNwrwAreaSoilThickness(teller) = Rdum(1)
!  keyword cf crop factor
              RetVal = RetVal + GetVAR2 (STRING,' cf ', 2,' NWRW-ReadAscii',' Pluvius.ALG file',  &
                                        IOUT1, CDUM(1), RDUM(1), IDUM(1), Allow, Found, IflRtn)
              SpecialNwrwAreaCropFact(teller) = Rdum(1)
!  keyword kp
              RetVal = RetVal + GetVAR2 (STRING,' kp ', 2,' NWRW-ReadAscii',' Pluvius.ALG file',  &
                                        IOUT1, CDUM(1), RDUM(1), IDUM(1), Allow, Found, IflRtn)
              SpecialNwrwAreaKp(teller) = Rdum(1)
!  keyword ksat
              RetVal = RetVal + GetVAR2 (STRING,' ksat ', 2,' NWRW-ReadAscii',' Pluvius.ALG file',  &
                                        IOUT1, CDUM(1), RDUM(1), IDUM(1), Allow, Found, IflRtn)
              SpecialNwrwAreaKSat(teller) = Rdum(1)
              if (SpecialNwrwAreaKp(teller) .gt. SpecialNwrwAreaKSat(teller)) then
                 call ErrMsgStandard (972, 0, ' Kp should be <= KSat' , ' Check green roof data NWRW datafile Pluvius.Alg; PLVA records')
              endif
!  keyword thetainit
              RetVal = RetVal + GetVAR2 (STRING,' thi ', 2,' NWRW-ReadAscii',' Pluvius.ALG file',  &
                                        IOUT1, CDUM(1), RDUM(1), IDUM(1), Allow, Found, IflRtn)
              SpecialNwrwAreaThetaInitPercnt(teller) = Rdum(1)
              SpecialNwrwAreaThetaInitPercnt(teller) = max (SpecialNwrwAreaThetaInitPercnt(teller), SpecialNwrwAreaThetaMinPercnt(teller))
              SpecialNwrwAreaThetaInitPercnt(teller) = min (SpecialNwrwAreaThetaInitPercnt(teller), SpecialNwrwAreaThetaSatPercnt(teller))
              if (idebug .ne. 0) then
                 write(idebug,*) ' Greenroof data read '
                 write(idebug,*) ' ThetaInit      ', SpecialNwrwAreaThetaInitPercnt(teller)
                 write(idebug,*) ' ThetaWiltingPt ', SpecialNwrwAreaThetaMinPercnt (teller)
                 write(idebug,*) ' ThetaFieldCap  ', SpecialNwrwAreaThetaFieldCapPercnt(teller)
                 write(idebug,*) ' ThetaSat       ', SpecialNwrwAreaThetaSatPercnt (teller)
              endif
        Endif
        Call SKPCOM (INfile2, ENDFIL,'ODS')
     Enddo
3111 CONTINUE
     TotNrSpecialNwrwAreas = teller
     teller = 0
     ! check if all used definitions are found, set references
     Do Iplv = 1, NcPluv
        if (NrSpecialNwrwAreas(iplv) .gt. 0) then
           Do j=1,NrSpecialNwrwAreas(iplv)
              Do k=1,TotNrSpecialNwrwAreas
                if (StringComp(SpecialDefinitions(iplv,j),SpecialDefinitionRead(k),CaseSensitive)) then
                   if (SpecialNWRWAreaType(iplv,j) .eq. SpecialNWRWAreaCompOption(k)) Reference2SpecialDef(iplv,j) = k
                endif
              Enddo
              If (Reference2SpecialDef(iplv,j) .eq. 0) teller = teller + 1
           Enddo
         endif
     Enddo
     If (RetVal .gt. 0) call ErrMsgStandard(972,0,' Error reading Pluvius.Alg file ', ' Getting PLVA/PLGR records')
     If (teller .gt. 0)  then
         call ErrMsgStandard (972, 0, ' Not all referred special definitions for NWRW-nodes in schematisation found', &
                              ' Not enough data in NWRW datafile Pluvius.Alg; PLVA/PLGR records')
     Endif

     iDebug = 0
     DeAllocate ( DWADEF )
     DeAllocate ( DWADEF2)
     Deallocate (AlreadyRead)
     Deallocate (DwaTabDef)
     Deallocate (SpecialDefinitionRead)
     return

 150 CONTINUE
     call SetMessage(LEVEL_FATAL, 'Read error in NWRW ASCII')

     return
  end subroutine NWRW_readASCII




  subroutine NWRW2_readAscii(infile1, infile2, infile3, Infile4)

!  ReadAscii using ParseToken module

    integer :: RetVal

    Integer(4)      infile1, infile2, infile3, infile4
    Integer         teller, i,j,k, iPTyp, iPOpp, iPlv, IDwa, Index, inod, nhlp, iout1, iecode, idebug
    Character(CharIdLength)   name, id, TableName
    Character(1000) string
    Logical         endfil, occurs, Err969, TabYesNo
    Integer         NrColumns, TableNr, ScanToTk, NrKeysFound

    Parameter        (NHLP=30)
    Integer           IDUM(NHLP)
    Real              RDUM(NHLP)
    Double Precision  DDUM(NHLP)
    Character(CharIdLength) CDUM(NHLP)
    Logical, Pointer:: AlreadyRead(:)
    Logical        success, allow, found

! Additional variables for ParseToken
    Integer IStart, ReturnIndx, NumberOfTokens
    Type (TokenArray) NwrwData
    Logical ReadError
! end of additional variables

    Character(CharIdLength), Pointer, save ::  DWADEF(:), DWATabDef(:), SpecialDefinitionRead(:)
    Character(CharIdLength), Pointer, save ::  DWADEF2(:)

    Success = Dh_AllocInit (Nplv, DwaDef, ' ')
    Success = success .and. Dh_AllocInit (Nplv, DwaDef2, ' ')
    Success = success .and. Dh_AllocInit (NcPluvDwa, DwaTabDef, ' ')
    Success = success .and. Dh_AllocInit (MaxTotNrSpecialNwrwAreas, SpecialDefinitionRead, ' ')
    Success = success .and. Dh_AllocInit (NcPluv, AlreadyRead, .false.)
    if (.not. success)  call ErrMsgStandard (981, 0, ' Error allocating arrays in subroutine ',' NWRW_ReadAscii')

    iDebug = ConfFil_get_iDebug()

!   Vector/Array initialisation
    PLVBAL = 0
! ARS 11173-74
    NrSpecialNwrwAreas = 0
    SpecialNwrwAreas = 0
    SpecialDefinitions = ''
    TotNrSpecialNwrwAreas = 0
    Reference2SpecialDef = 0

    ReadError = .false.
    iOut1 = ConfFil_get_iOut1()

! Read Pluvius.3B file

    call SetMessage(LEVEL_DEBUG, 'Read Pluvius.3b file')
    endfil = .false.
    teller = 0
    Call SKPCOM (INfile1, ENDFIL,'ODS')
    Do while (.not. endfil)
      READ(Infile1,'(A1000)',END=21,ERR=150,IOSTAT=IECODE)  STRING
! skip regel als hij niet begint met juist keyword (NWRW)
      IF (STRING(1:4) .EQ. 'NWRW') Then
! NWRW node id
        IStart = 1
        ScanToTk = 60
        Success = ParseTokenArrayWithKeywords (String, ScanToTk, NwrwData, NumberOfTokens, CaseSensitive)
        If (.not. Success) ReadError = .true.
        If (ReadError)  Then
          call ErrMsgStandard (972, 0, ' Error reading NWRW Definition data - NWRW ParseToken Pluvius.3B file', ' ')
        Endif
        id = ''
        ReadError = ReadError .and. .not. &
                       SetOneVariable ('id', IStart, NwrwData, NumberOfTokens, CaseSensitive, id)
        index = 0
        call fndNd2(index, id)
        if (index .gt. 0) then
         Inod = index
         iplv = EiNode(inod,2)
         if (EiNode(inod,3) .eq. 7) then   ! en is NWRW knoop
          if (AlreadyRead(iplv)) then
            call SetMessage(LEVEL_ERROR, 'Data for NWRW node '//id(1:Len_trim(id))//' double in datafile Pluvius.3B')
          else
           teller = teller + 1
           AlreadyRead(iplv) = .true.
           PLVNAM(iplv) = index
           id_nod2plvnam(index) = iplv

           LvlPv(Iplv) = -999.99
           IRIOOL (iplv) = 0
           NrKeysFound = 0
           Do iStart=1,NumberOfTokens
            ReturnIndx = IStart
            If (.not. NwrwData%IsEnclosedByQuotes(iStart)) then
              If (NwrwData%Token(iStart) .eq. 'sl') then
! NWRW surface level, is not obliged, so no ReadError
                success = SetOneVariable ('sl', IStart, NwrwData, NumberOfTokens, CaseSensitive, LvlPv(iplv))
! NWRW areas
              ElseIf (NwrwData%Token(iStart) .eq. 'ar') then
                NrKeysFound = NrKeysFound +1
                I = 0
                Do Iptyp=1,nptyp
                  Do ipopp=1,npopp
                    I = i+1
                    Read (NwrwData%Token(ReturnIndx+i),*) AreaPv(iplv,iptyp, ipopp)
                  Enddo
               Enddo
! NWRW number of people
              ElseIf (NwrwData%Token(iStart) .eq. 'np') then
                NrKeysFound = NrKeysFound +1
                ReadError = ReadError .and. .not. &
                          SetOneVariable ('np', iStart, NwrwData, NumberOfTokens, CaseSensitive, DWAINW(iplv))
! NWRW number of companies: optional
              ElseIf (NwrwData%Token(iStart) .eq. 'np2') then
                NrKeysFound = NrKeysFound +1
                ReadError = ReadError .and. .not. &
                          SetOneVariable ('np2', iStart, NwrwData, NumberOfTokens, CaseSensitive, DWAINW2(iplv))
! ARS 11173-74
              ElseIf (NwrwData%Token(iStart) .eq. 'dw') then
! NWRW DWA definition people
                NrKeysFound = NrKeysFound +1
                ReadError = ReadError .and. .not. &
                          SetOneVariable ('dw', IStart, NwrwData, NumberOfTokens, CaseSensitive, DWADEF(iplv))
                if (.not. ReadError) then
                    Call Upperc (DWADEF(iplv))
                    ReferenceToDwaDef(iplv) = 0
                endif
! NWRW DWA definition companies, optional, so no ReadError
              ElseIf (NwrwData%Token(iStart) .eq. 'dw2') then
                NrKeysFound = NrKeysFound +1
                ReadError = ReadError .and. .not. &
                          SetOneVariable ('dw2', IStart, NwrwData, NumberOfTokens, CaseSensitive, DWADEF2(iplv))
                if (.not. ReadError) then
                    Call Upperc (DWADEF2(iplv))
                    ReferenceToDwaDef2(iplv) = 0
                endif
! NWRW Meteostation definition
              ElseIf (NwrwData%Token(iStart) .eq. 'ms') then
                NrKeysFound = NrKeysFound +1
                ReadError = ReadError .and. .not. &
                          SetOneVariable ('ms', IStart, NwrwData, NumberOfTokens, CaseSensitive, NamMet(index))
              Endif
            Endif
           Enddo
           If (NrKeysFound .lt. 4) then
               Write(Iout1,*) ' NrKeysFound<4 for node NWRW node ', id(1:Len_trim(id)), NrKeysFound
               ReadError = .true.
           Endif
! NWRW special areas
           IStart = 1
           if (Getkey ('na', IStart, NwrwData, NumberOfTokens, ReturnIndx, CaseSensitive)) then
               Read (NwrwData%Token(ReturnIndx+1),*)   NrSpecialNwrwAreas(iplv)
               if (NrSpecialNwrwAreas(iplv) .gt. 0) then
                  ReadError = Readerror .and. .not. &
                         SetVariables ('aa', IStart, NwrwData, NumberOfTokens, CaseSensitive, SpecialNwrwAreas,NPlv, MaxNrSpecialNwrwAreas,Iplv,NrSpecialNwrwAreas(iplv))
                  ReadError = Readerror .and. .not. &
                         SetVariables ('nw', IStart, NwrwData, NumberOfTokens, CaseSensitive, SpecialDefinitions,NPlv, MaxNrSpecialNwrwAreas,Iplv,NrSpecialNwrwAreas(iplv))
               endif
           endif
! bepaal totaal areaal voor waterbalans
           DO ipopp=1,npopp
              DO iptyp=1,nptyp
                 plvbal(1) = plvbal(1) + areapv(iplv,iptyp,ipopp)
              ENDDO
           ENDDO
           ! add special areas
           If (NrSpecialNwrwAreas(iplv) .gt. 0) then
              Do j=1, NrSpecialNwrwAreas(iplv)
                 plvbal(1) = plvbal(1) + SpecialNwrwAreas(iplv,j)
              Enddo
           Endif
          Endif
         Endif
        Endif
      Endif
      Call SKPCOM (INfile1, ENDFIL,'ODS')
    enddo
 21 Continue
    If (teller .lt. NcPluv)  Then
       Write(*,*) ' Read ', Teller, ' Expected', ncPluv
       call ErrMsgStandard (972, 0, ' Not enough data for all NWRW-nodes in schematisation found', &
                            ' Some NWRW-nodes from schematisation not present in Pluvius.3B file')
    Endif
    If (ReadError)  Then
       call ErrMsgStandard (972, 0, ' ReadError reading NWRW data from Pluvius.3B file; 1/more items in record are missing', ' ')
    Endif

!Pluvius.Dwa file
! first determine number records (=DWA-definitions) in DWA file
    endfil = .false.
    ReadError = .false.
    call SetMessage(LEVEL_DEBUG, 'Read Pluvius.Dwa file')
    teller = 0
    do while (.not. endfil)
       Call SKPCOM (INfile3, ENDFIL,'ODS')
       If (endfil) goto 210
       READ(Infile3,'(A1)') String
       teller = teller+1
    enddo
210 Continue
    teller = max (1, teller)
    Rewind(infile3)

! Pas arrayallocaties DWAOpt en DWADIS aan
    Success = Dh_AllocInit (teller, DwaOpt, 0)
    Success = success .and. Dh_AllocInit (teller, 26, DwaDis, 0E0)
    If (.not. success) call ErrMsgStandard (981, 0, ' Error allocating arrays in subroutine ', &
                                            ' NWRW_ReadAscii' )

    endfil = .false.
    teller = 0
    Call SKPCOM (INfile3, ENDFIL,'ODS')
    do while (.not. endfil)
       READ(Infile3,'(A1000)',END=211,ERR=150,IOSTAT=IECODE)  STRING
       if (idebug .ne. 0) write(idebug,*) ' Read from DWA file', String(1:120)
       IF (STRING(1:4) .EQ. 'DWA') Then
! DWA definition node id
           IStart = 1
           ScanToTk = 100
           Success = ParseTokenArrayWithKeywords (String, ScanToTk, NwrwData, NumberOfTokens, CaseSensitive)
           If (.not. Success) ReadError = .true.
           If (ReadError)  Then
             call ErrMsgStandard (972, 0, ' Error reading DWA Definition data - DWA ParseToken Pluvius.Dwa file', ' ')
           Endif
           id = ''
           ReadError = ReadError .and. .not. &
                          SetOneVariable ('id', IStart, NwrwData, NumberOfTokens, CaseSensitive, id)
           Name = id
           Call Upperc (Name)
!          Eerst testen of DWA definition wel gebruikt wordt, dan pas verwerken
           ! people
           Iplv = FindString (Ncpluv, DWAdef, Name, NcPluv, CaseSensitive)
           Occurs = (Iplv .gt. 0)
           if (Iplv .gt. 0) then
              if (ReferenceToDWADEF(iplv) .gt. 0) then
                call SetMessage(LEVEL_ERROR, 'DWA Definition '//name(1:Len_trim(Name))//' double in datafile Pluvius.Dwa')
              endif
           endif
           ! companies
           Iplv = FindString (Ncpluv, DWAdef2, Name, NcPluv, CaseSensitive)
           Occurs = occurs .or. (Iplv .gt. 0)
           if (Iplv .gt. 0) then
              if (ReferenceToDWADEF2(iplv) .gt. 0) then
                call SetMessage(LEVEL_ERROR, 'DWA Definition '//name(1:Len_trim(Name))//' double in datafile Pluvius.Dwa')
              endif
           endif

!          Verwerk DWA definitie
           if (occurs) then
              teller = teller + 1
! DWA keyword do: computation option
              ReadError = ReadError .and. .not. &
                            SetOneVariable ('do', IStart, NwrwData, NumberOfTokens, CaseSensitive, DwaOpt(teller))
! DWA keyword wc
              ReadError = ReadError .and. .not. &
                            SetOneVariable ('wc', IStart, NwrwData, NumberOfTokens, CaseSensitive, DwaDis(teller,1))
! DWA keyword wd
              ReadError = ReadError .and. .not. &
                            SetOneVariable ('wd', IStart, NwrwData, NumberOfTokens, CaseSensitive, DwaDis(teller,2))
! DWA 24 hours
              if (Getkey ('wh', IStart, NwrwData, NumberOfTokens, ReturnIndx, CaseSensitive) ) then
                Do I=1,24
                   Read (NwrwData%Token(ReturnIndx+i),*) DwaDis(teller,i+2)
                Enddo
              else
                ReadError = .true.
              endif
! DWA keyword dt
              if (DwaOpt(teller) .eq. 5) then
                ReadError = ReadError .and. .not. &
                             SetOneVariable ('dt', IStart, NwrwData, NumberOfTokens, CaseSensitive, DwaTabDef(teller))
              endif

! Assign definition to individual nodes
              Do Iplv = 1, Ncpluv
                 if (StringComp(DWADEF(iplv),Name,CaseSensitive)) ReferenceToDwaDef(iplv) = teller
              Enddo
              Do Iplv = 1, Ncpluv
                 if (StringComp(DWADEF2(iplv),Name,CaseSensitive)) ReferenceToDwaDef2(iplv) = teller
              Enddo
           Endif
       Endif
       Call SKPCOM (INfile3, ENDFIL,'ODS')
    Enddo
211 CONTINUE

    Err969 = .false.
    Do Iplv = 1, Ncpluv
       If (ReferenceToDwaDef(iplv) .le. 0)  Then
          if (idebug .ne. 0) write(idebug,*) ' Error for NWRW node ', Iplv, ReferenceToDwaDef(Iplv), Dwadef(iplv)
          call ErrMsgStandard (969, 0, ' DWA-definitions people not present in Pluvius.DWA data file.', DWADef(iplv))
          Err969 = .true.
       Endif
       If (ReferenceToDwaDef2(iplv) .eq. 0)  Then
          if (idebug .ne. 0) write(idebug,*) ' Error for NWRW node ', Iplv, ReferenceToDwaDef2(Iplv), Dwadef2(iplv)
          call ErrMsgStandard (969, 0, ' DWA-definitions companies not present in Pluvius.DWA data file.', DWADef2(iplv))
          Err969 = .true.
       Endif
    Enddo
    If (Err969) call ErrMsgStandard (972, 0, ' Not enough data for NWRW-nodes found', &
                                     ' Some DWA-definitions not present in NWRW data files')
    If (ReadError)  Then
       call ErrMsgStandard (972, 0, ' ReadError reading NWRW data from Pluvius.DWA file', ' ')
    Endif

!Pluvius.Tbl file
    call SetMessage(LEVEL_INFO, 'Read Pluvius.tbl file')
    if (idebug .ne. 0) write(idebug,*) ' Read Pluvius.Tbl file'

!   Vector/Array initialisation
    NwrwDWATable = 0
    AlreadyRead  = .false.

    Endfil = .false.
    Call SKPCOM (Infile4, ENDFIL,'ODS')
    Do while (.not. endfil)
       Success = GetRecord(Infile4, 'DW_T', Endfil, idebug, Iout1)     ! get record van keyword DW_T tot dw_t, zet in buffer
       IF (ENDFIL) GOTO 5111
       Success = GetTableName (TabYesNo, TableName, ' id ', Iout1)     ! get table name via keyword ' id ', TabYesNo=TBLE found
       IF (.not. Success) GOTO 5111
       If (TabYesNo .and. TableName .ne. '') Then
!         Er is een tabel gedefinieerd, met een niet-lege naam
!         Eerst testen of tabel definition wel gebruikt wordt
          IPlv = FindString (NcPluvDwa, DWATabdef, TableName, NcPluvDwa, CaseSensitive)
          Occurs = (IPlv .gt. 0)
          NrColumns = 1
          if (IPlv .gt. 0) then
             if ( AlreadyRead (iPlv) ) then
               call SetMessage(LEVEL_ERROR, 'NWRW DWA table Definition '//Tablename(1:Len_trim(TableName))//' double in datafile Pluvius.Tbl')
             endif
             AlreadyRead (iPlv) = .true.
          endif
!         Verwerken tabel definitie
          if (occurs .and. NrColumns .gt. 0) then
! Get table with name TableName, Nrcolumns data fields, result in global arrays; tabel nummer is TableNr
             Success = GetTable (TableHandle, TableName, NrColumns, TableNr, idebug, Iout1)
             IF (.not. Success) GOTO 5111
! Set references
             Do iDwa=1, NcPluvDwa
                if ( (DWAOPT(iDwa) .eq. 5) .and. &
                      (StringComp (DWATabDef(iDwa), TableName, CaseSensitive) ) )  NwrwDWATable(iDwa) = TableNr
             Enddo
          Endif
       Endif
       Call SKPCOM (Infile4, ENDFIL,'ODS')
     Enddo
5111 Continue

    Err969 = .false.
    Do IDwa = 1, NcpluvDwa
       If (DWAOpt(Idwa) .eq. 5 .and. NwrwDwaTable(IDwa) .le. 0)  Then
          if (idebug .ne. 0) write(idebug,*) ' Error for NWRW DWa definition ', IDwa, NwrwDwaTable(Idwa), DwaTabdef(idwa)
          call ErrMsgStandard (969, 0, ' DWA-Table definitions not present in Pluvius.Tbl data file.', DWATabDef(idwa))
          Err969 = .true.
       Endif
    Enddo
    If (Err969) call ErrMsgStandard (972, 0, ' Not enough data for NWRW-nodes found', &
                                     ' Some DWA-Table definitions not present in NWRW table data file')


!Pluvius.Alg file, PLVG record
    endfil = .false.
    ReadError = .false.
    call SetMessage(LEVEL_DEBUG, 'Read Pluvius.Alg file - PLVG')
    teller = 0
    Call SKPCOM (INfile2, ENDFIL,'ODS')
    Do while (.not. endfil)
        READ(Infile2,'(A1000)',END=2111,ERR=150,IOSTAT=IECODE)  STRING
        if (idebug .ne. 0) write(idebug,*) ' Read from ALG file', String(1:120)
        IF (STRING(1:4) .Eq. 'PLVG')  Then
           teller = teller + 1
           ScanToTk = 60
           Success = ParseTokenArrayWithKeywords (String, ScanToTk, NwrwData, NumberOfTokens, CaseSensitive)
           If (.not. Success) ReadError = .true.
           If (ReadError)  Then
             call ErrMsgStandard (972, 0, ' Error reading Pluvius Definition data - PLVG ParseToken Pluvius.alg file', ' ')
           Endif
! ALG keyword ru: runoff coefficients
           ReadError = Readerror .and. .not. &
                       SetVariables ('ru', IStart, NwrwData, NumberOfTokens, CaseSensitive, AfVrTr, NpOpp*Nptyp)
           if (.not. ReadError) Call VerifyAfVrTr
! ALG keyword ms: maximum storage surface
           if (Getkey ('ms', IStart, NwrwData, NumberOfTokens, ReturnIndx, CaseSensitive)) then
             I = 0
             DO Iptyp=1,nptyp
                DO ipopp=1,npopp
                  I = I+1
                  Read (NwrwData%Token(ReturnIndx+i),*) Bmaxop(iptyp,ipopp)
               ENDDO
             ENDDO
           else
             ReadError = .true.
           endif
! ALG keyword ix: maximum infiltration capacity
           if (Getkey ('ix', IStart, NwrwData, NumberOfTokens, ReturnIndx, CaseSensitive)) then
             DO iptyp=1,nptyp
                Read (NwrwData%Token(ReturnIndx+iptyp),*) INFCAP(IPTYP,1)
             ENDDO
           else
             ReadError = .true.
           endif
! ALG keyword im: minimum infiltration capacity
           if (Getkey ('im', IStart, NwrwData, NumberOfTokens, ReturnIndx, CaseSensitive)) then
             DO iptyp=1,nptyp
                Read (NwrwData%Token(ReturnIndx+iptyp),*) INFCAP(IPTYP,2)
             ENDDO
           else
             ReadError = .true.
           endif
! ALG keyword ic: DEcrease infiltration capacity
           if (Getkey ('ic', IStart, NwrwData, NumberOfTokens, ReturnIndx, CaseSensitive)) then
             DO iptyp=1,nptyp
                Read (NwrwData%Token(ReturnIndx+iptyp),*) TFINCP(IPTYP, 1)
             ENDDO
           else
             ReadError = .true.
           endif
! ALG keyword dc: INcrease infiltration capacity
           if (Getkey ('dc', IStart, NwrwData, NumberOfTokens, ReturnIndx, CaseSensitive)) then
             DO iptyp=1,nptyp
                Read (NwrwData%Token(ReturnIndx+iptyp),*) TFINCP(IPTYP, 2)
             ENDDO
           else
             ReadError = .true.
           endif
! ALG od
           if (Getkey ('od', IStart, NwrwData, NumberOfTokens, ReturnIndx, CaseSensitive)) then
              Read (NwrwData%Token(ReturnIndx+1),*) idum(1)
              INFDEP = (idum(1) .ne. 0)
           else
             ReadError = .true.
           endif
! ALG or
           if (Getkey ('or', IStart, NwrwData, NumberOfTokens, ReturnIndx, CaseSensitive)) then
              Read (NwrwData%Token(ReturnIndx+1),*) idum(1)
              INFAF  = (idum(1) .ne. 0)
           else
             ReadError = .true.
           endif
        Endif
        Call SKPCOM (INfile2, ENDFIL,'ODS')
     Enddo
2111 CONTINUE
     If (teller .le. 0)  then
         call ErrMsgStandard (972, 0, ' Not enough data for NWRW-nodes in schematisation found', &
                              ' No data in NWRW datafile Pluvius.Alg')
     Endif
     If (ReadError)  Then
        call ErrMsgStandard (972, 0, ' ReadError reading NWRW data from Pluvius.ALG file', ' ')
     Endif


!Pluvius.Alg file, PLVA record
    rewind (infile2)
    endfil = .false.
    ReadError = .false.
    call SetMessage(LEVEL_DEBUG, 'Read Pluvius.Alg file - PLVA')
    teller = 0
    Call SKPCOM (INfile2, ENDFIL,'ODS')
    Do while (.not. endfil)
        READ(Infile2,'(A1000)',END=3111,ERR=150,IOSTAT=IECODE)  STRING
        if (idebug .ne. 0) write(idebug,*) ' Read from ALG file', String(1:120)
        IF (STRING(1:4) .Eq. 'PLVA')  Then
           teller = teller + 1
           ScanToTk = 60
           Success = ParseTokenArrayWithKeywords (String, ScanToTk, NwrwData, NumberOfTokens, CaseSensitive)
           If (.not. Success) ReadError = .true.
           If (ReadError)  Then
             call ErrMsgStandard (972, 0, ' Error reading PLVA Definition data - ParseToken Pluvius.Alg file', ' ')
           Endif
! id
           ReadError = ReadError .and. .not. &
                        SetOneVariable ('id', IStart, NwrwData, NumberOfTokens, CaseSensitive, &
                                         SpecialDefinitionRead(teller))
! keyword ru: runoff coefficients
           ReadError = ReadError .and. .not. &
                        SetOneVariable ('ru', IStart, NwrwData, NumberOfTokens, CaseSensitive, &
                                         SpecialRunoffDelay(teller))
! keyword ms: maximum storage surface
           ReadError = ReadError .and. .not. &
                        SetOneVariable ('ms', IStart, NwrwData, NumberOfTokens, CaseSensitive, &
                                        SpecialMaxStorage(teller))
! keyword ix: maximum infiltration capacity
           ReadError = ReadError .and. .not. &
                        SetOneVariable ('ix', IStart, NwrwData, NumberOfTokens, CaseSensitive, &
                                         SpecialMaxInfCap(teller))
! keyword im: minimum infiltration capacity
           ReadError = ReadError .and. .not. &
                        SetOneVariable ('im', IStart, NwrwData, NumberOfTokens, CaseSensitive, &
                                        SpecialMinInfCap(teller))
! keyword ic: DEcrease infiltration capacity
           ReadError = ReadError .and. .not. &
                        SetOneVariable ('ic', IStart, NwrwData, NumberOfTokens, CaseSensitive, &
                                        SpecialInfCapDecrease(teller))
! keyword dc: INcrease infiltration capacity
           ReadError = ReadError .and. .not. &
                        SetOneVariable ('dc', IStart, NwrwData, NumberOfTokens, CaseSensitive, &
                                        SpecialInfCapRecovery(teller))
! keyword ef: evaporation factor
           ReadError = ReadError .and. .not. &
                        SetOneVariable ('ef', IStart, NwrwData, NumberOfTokens, CaseSensitive, &
                                         SpecialEvapFact(teller))
! od
            if (Getkey ('od', IStart, NwrwData, NumberOfTokens, ReturnIndx, CaseSensitive)) then
               Read (NwrwData%Token(ReturnIndx+1),*) idum(1)
               SpecialInfilDepression(teller) = (idum(1) .ne. 0)
            else
              ReadError = .true.
            endif
! or
            if (Getkey ('or', IStart, NwrwData, NumberOfTokens, ReturnIndx, CaseSensitive)) then
               Read (NwrwData%Token(ReturnIndx+1),*) idum(1)
               SpecialInfilRunoff(teller) = (idum(1) .ne. 0)
            else
              ReadError = .true.
           endif
!  keyword co: computation option
           Allow = .true.
           endfil = .false.
           RetVal = RetVal + GetVAR2 (STRING,' co ',3,' NWRW-ReadAscii',' Pluvius.ALG file', &
                        IOUT1, CDUM(1), RDUM(1), IDUM(1), Allow, Found, IflRtn)
           SpecialNwrwAreaCompOption(teller) = 0
           if (found)  SpecialNwrwAreaCompOption(teller) = IDUM(1)
! green roof options only read if compoption <> 0
           if (SpecialNwrwAreaCompOption(teller) .ne. 0) then
!  keyword thetamin
              RetVal = RetVal + GetVAR2 (STRING,' thwp ', 2,' NWRW-ReadAscii',' Pluvius.ALG file',  &
                                        IOUT1, CDUM(1), RDUM(1), IDUM(1), Allow, Found, IflRtn)
              SpecialNwrwAreaThetaMinPercnt(teller) = Rdum(1)
!  keyword thetafieldcap
              RetVal = RetVal + GetVAR2 (STRING,' thfc ', 2,' NWRW-ReadAscii',' Pluvius.ALG file',  &
                                        IOUT1, CDUM(1), RDUM(1), IDUM(1), Allow, Found, IflRtn)
              SpecialNwrwAreaThetaFieldCapPercnt(teller) = Rdum(1)
!  keyword thetasat
              RetVal = RetVal + GetVAR2 (STRING,' thsat ', 2,' NWRW-ReadAscii',' Pluvius.ALG file',  &
                                        IOUT1, CDUM(1), RDUM(1), IDUM(1), Allow, Found, IflRtn)
              SpecialNwrwAreaThetaSatPercnt(teller) = Rdum(1)
              if (SpecialNwrwAreaThetaMinPercnt(teller) .gt. SpecialNwrwAreaThetaFieldCapPercnt(teller) .or. &
                    SpecialNwrwAreaThetaFieldCapPercnt(teller) .gt. SpecialNwrwAreaThetaSatPercnt(teller) ) then
                 call ErrMsgStandard (972, 0, ' ThetaMin should be <= ThetaFieldCapacity <= ThetaSat' , &
                                      ' Check green roof data NWRW datafile Pluvius.Alg; PLVA records')
              endif
!  keyword sl thickness of soil layer
              RetVal = RetVal + GetVAR2 (STRING,' sl ', 2,' NWRW-ReadAscii',' Pluvius.ALG file',  &
                                        IOUT1, CDUM(1), RDUM(1), IDUM(1), Allow, Found, IflRtn)
              SpecialNwrwAreaSoilThickness(teller) = Rdum(1)
!  keyword cf crop factor
              RetVal = RetVal + GetVAR2 (STRING,' cf ', 2,' NWRW-ReadAscii',' Pluvius.ALG file',  &
                                        IOUT1, CDUM(1), RDUM(1), IDUM(1), Allow, Found, IflRtn)
              SpecialNwrwAreaCropFact(teller) = Rdum(1)
!  keyword kp
              RetVal = RetVal + GetVAR2 (STRING,' kp ', 2,' NWRW-ReadAscii',' Pluvius.ALG file',  &
                                        IOUT1, CDUM(1), RDUM(1), IDUM(1), Allow, Found, IflRtn)
              SpecialNwrwAreaKp(teller) = Rdum(1)
!  keyword ksat
              RetVal = RetVal + GetVAR2 (STRING,' ksat ', 2,' NWRW-ReadAscii',' Pluvius.ALG file',  &
                                        IOUT1, CDUM(1), RDUM(1), IDUM(1), Allow, Found, IflRtn)
              SpecialNwrwAreaKSat(teller) = Rdum(1)
              if (SpecialNwrwAreaKp(teller) .gt. SpecialNwrwAreaKSat(teller)) then
                 call ErrMsgStandard (972, 0, ' Kp should be <= KSat' , ' Check green roof data NWRW datafile Pluvius.Alg; PLVA records')
              endif
!  keyword thetainit
              RetVal = RetVal + GetVAR2 (STRING,' thi ', 2,' NWRW-ReadAscii',' Pluvius.ALG file',  &
                                        IOUT1, CDUM(1), RDUM(1), IDUM(1), Allow, Found, IflRtn)
              SpecialNwrwAreaThetaInitPercnt(teller) = Rdum(1)
              SpecialNwrwAreaThetaInitPercnt(teller) = max (SpecialNwrwAreaThetaInitPercnt(teller), SpecialNwrwAreaThetaMinPercnt(teller))
              SpecialNwrwAreaThetaInitPercnt(teller) = min (SpecialNwrwAreaThetaInitPercnt(teller), SpecialNwrwAreaThetaSatPercnt(teller))
           endif
        Endif
        Call SKPCOM (INfile2, ENDFIL,'ODS')
     Enddo
3111 CONTINUE
     TotNrSpecialNwrwAreas = teller
     teller = 0
     ! check if all used definitions are found, set references
     Do Iplv = 1, NcPluv
        if (NrSpecialNwrwAreas(iplv) .gt. 0) then
           Do j=1,NrSpecialNwrwAreas(iplv)
              Do k=1,TotNrSpecialNwrwAreas
                if (StringComp(SpecialDefinitions(iplv,j),SpecialDefinitionRead(k),CaseSensitive)) then
                   Reference2SpecialDef(iplv,j) = k
                endif
              Enddo
              If (Reference2SpecialDef(iplv,j) .eq. 0) teller = teller + 1
           Enddo
         endif
     Enddo
     If (teller .gt. 0)  then
         call ErrMsgStandard (972, 0, ' Not enough data for NWRW-nodes in schematisation found', &
                              ' No data in NWRW datafile Pluvius.Alg; PLVA records')
     Endif
     If (ReadError)  Then
        call ErrMsgStandard (972,0,' ReadError reading NWRW data from Pluvius.ALG file; 1/more missing items in PLVA record', ' ')
     Endif

     iDebug = 0
     DeAllocate ( DWADEF )
     DeAllocate ( DWADEF2 )
     Deallocate (AlreadyRead)
     Deallocate (DwaTabDef)
     return

 150 CONTINUE
     call SetMessage(LEVEL_FATAL, 'Read error in NWRW ASCII')

     return
  end subroutine NWRW2_readASCII


  Subroutine VerifyAfVrTr
!    Verify that AfVrTr values are acceptable (should be <=1)
!    and set LnAfVrTr
     Real Temp
     If (AfVrTr(1) .gt. 1. .or. AfVrTr(2) .gt. 1.  .or. AfVrTr(3) .gt. 1. .or. &
         AfVrTr(4) .gt. 1. .or. AfVrTr(5) .gt. 1.  .or. AfVrTr(6) .gt. 1. .or. &
         AfVrTr(7) .gt. 1. .or. AfVrTr(8) .gt. 1.  .or. AfVrTr(9) .gt. 1. .or. &
         AfVrTr(10) .gt. 1. .or. AfVrTr(11) .gt. 1.  .or. AfVrTr(12) .gt. 1. .or. &
          AfVrTr(1) .lt. 0. .or. AfVrTr(2) .lt. 0.  .or. AfVrTr(3) .lt. 0. .or. &
          AfVrTr(4) .lt. 0. .or. AfVrTr(5) .lt. 0.  .or. AfVrTr(6) .lt. 0. .or. &
          AfVrTr(7) .lt. 0. .or. AfVrTr(8) .lt. 0.  .or. AfVrTr(9) .lt. 0. .or. &
          AfVrTr(10) .lt. 0. .or. AfVrTr(11) .lt. 0.  .or. AfVrTr(12) .lt. 0. ) then
        call ErrMsgStandard (972, 0, ' Runoff delay values should be between 0 and 1', ' VerifyAfVrTr')
     Else
       Temp = -Log (1.-(Min(AfVrTr(1),0.999999)))
       LnAfvrtr(1) = 1.- AfvrTr(1)/Max(0.000001,Temp)
       Temp = -Log (1.-(Min(AfVrTr(2),0.999999)))
       LnAfvrtr(2) = 1.- AfvrTr(2)/Max(0.000001,Temp)
       Temp = -Log (1.-(Min(AfVrTr(3),0.999999)))
       LnAfvrtr(3) = 1.- AfvrTr(3)/Max(0.000001,Temp)
       Temp = -Log (1.-(Min(AfVrTr(4),0.999999)))
       LnAfvrtr(4) = 1.- AfvrTr(4)/Max(0.000001,Temp)
       Temp = -Log (1.-(Min(AfVrTr(5),0.999999)))
       LnAfvrtr(5) = 1.- AfvrTr(5)/Max(0.000001,Temp)
       Temp = -Log (1.-(Min(AfVrTr(6),0.999999)))
       LnAfvrtr(6) = 1.- AfvrTr(3)/Max(0.000001,Temp)
       Temp = -Log (1.-(Min(AfVrTr(7),0.999999)))
       LnAfvrtr(7) = 1.- AfvrTr(3)/Max(0.000001,Temp)
       Temp = -Log (1.-(Min(AfVrTr(8),0.999999)))
       LnAfvrtr(8) = 1.- AfvrTr(3)/Max(0.000001,Temp)
       Temp = -Log (1.-(Min(AfVrTr(9),0.999999)))
       LnAfvrtr(9) = 1.- AfvrTr(3)/Max(0.000001,Temp)
       Temp = -Log (1.-(Min(AfVrTr(10),0.999999)))
       LnAfvrtr(10) = 1.- AfvrTr(10)/Max(0.000001,Temp)
       Temp = -Log (1.-(Min(AfVrTr(11),0.999999)))
       LnAfvrtr(11) = 1.- AfvrTr(11)/Max(0.000001,Temp)
       Temp = -Log (1.-(Min(AfVrTr(12),0.999999)))
       LnAfvrtr(12) = 1.- AfvrTr(12)/Max(0.000001,Temp)
     Endif

     return
  End subroutine VerifyAfVrTr



  SUBROUTINE CMPPLV (ITMSTP, IPLV, IMETEO, IPLV2, INODE)
    ! *********************************************************************
    ! *** Brief description:
    ! *** ------------------
    ! ***    This subroutine performs computations for Pluvius knopen
    ! ***   ! In vergelijking met overige type knopen is hier verdamping
    ! ***   ! nog niet afhankelijk van het uur op de dag! (niet nodig?)
    ! *********************************************************************
    ! *** Input/output parameters:
    ! *** ------------------------
    ! ***  IEVENT = event number
    ! ***  ITMSTP = timestep number
    ! ***  IDEBUG = file unit number of debug file
    ! ***  IOUT1  = file unit number of output file with run messages
    ! ***  IPLV   = intern nr. Pluvius knoop
    ! ***  IPLV2  = intern nr. ingedikte Pluvius knoop
    ! ***  IMETEO = meteostation code
    ! ***  INODE  = knoopnr
    ! *********************************************************************
    ! *** Berekeningen voor Pluvius rioolinstromingsmodel
    ! *********************************************************************

    Integer iTmStp, iPlv, iMeteo, iPlv2, iNode, iPTyp, iPOpp
    Real    rInf2(nptyp*npopp)
    Real    bMax, ratio, totUit, vNOw, vInf
    Integer iDebug, index1, index2
    Double Precision Infiltrationmm(nptyp*npopp)


!   write(*,*) ' Start Cmpplv'
    iDebug = ConfFil_get_iDebug()

    IF (iDebug .ne. 0)  WRITE(IDEBUG,*) 'CMPPLV iplv=',IPLV, IPLV2

    INPRT (IPLV2) = 0
    INFDP (IPLV2) = 0
    VOLOP (IPLV2) = 0
    VOLDYN(IPLV2) = 0

    IF (INFDEP) THEN
      DO IPTYP=1,NPTYP
         DO IPOPP=1,NPOPP
            index1 = (ipopp-1)*nptyp + iptyp
            index2 = (iplv2-1)*Npopp*Nptyp + (ipopp-1)*nptyp + iptyp
            NWRW_MinInfCap(index1) = InfCap(iptyp,2)
            NWRW_MaxInfCap(index1) = InfCap(iptyp,1)
            NWRW_RecoveryInfCap(index1) = TfinCp(iptyp,2)
            NWRW_DecreaseInfCap(index1) = TfinCp(iptyp,1)
            NWRW_PreviousInfCap(index1) = InfCp(iplv2,iptyp,ipopp,1)
            NWRW_NewInfCap(index1)      = InfCp(iplv2,iptyp,ipopp,1)
            NWRW_Dt(index1)             = Dt(iplv2,iptyp,ipopp,1)
            NWRW_InitialStorage(index1) = BVOP0(iplv2,iptyp,ipopp)
            NWRW_Rainfall(index1)       = AAFNodeRainfall(inode) * Rain(imeteo)
            NWRW_InfSts(index1)         = InfSts(iplv2,iptyp,ipopp,1)
         enddo
      Enddo
!     write(idebug,*) ' Before ArrayInfiltration_HortonFormula Depressions Standard types'
!     write(idebug,*)  ' MinInfCap  MaxInfCap  Recovery  Decrease Previous New Dt InitialStor Rainfall InfSts'
!     DO IPTYP=1,NPTYP
!        DO IPOPP=1,NPOPP
!           index1 = (ipopp-1)*nptyp + iptyp
!           index2 = (iplv2-1)*Npopp*Nptyp + (ipopp-1)*nptyp + iptyp
!           write(idebug,'(9F10.3,I3)')  NWRW_MinInfCap(index1), NWRW_MaxInfCap(index1), NWRW_RecoveryInfCap(index1),NWRW_DecreaseInfCap(index1), &
!                                          NWRW_PreviousInfCap(index1),NWRW_NewInfCap(index1), &
!                                            NWRW_Dt(index1),NWRW_InitialStorage(index1),NWRW_Rainfall(index1),NWRW_InfSts(index1)
!        enddo
!     Enddo
      Call Infiltration_HortonFormula  ( nptyp*npopp, &
                                         NWRW_MinInfCap,      NWRW_MaxInfCap,      &
                                         NWRW_DecreaseInfCap, NWRW_RecoveryInfCap, &
                                         NWRW_PreviousInfCap, NWRW_NewInfCap,      &
                                         Timesettings%TimestepSize,                                &
                                         NWRW_DT,             NWRW_InitialStorage, &
                                         NWRW_Rainfall,       NWRW_InfSts,         NWRW_InfiltrationMM)
      DO IPTYP=1,NPTYP
         DO IPOPP=1,NPOPP
            index1 = (ipopp-1)*nptyp + iptyp
            InfCp(iplv2,iptyp,ipopp,1)  = NWRW_NewInfCap(index1)
            Dt (iplv2,iptyp,ipopp,1)  = NWRW_Dt(index1)
            InfSts(iplv2,iptyp,ipopp,1) = NWRW_InfSts(index1)
            ! Infiltratie IPV in m3 per tijdstap
            IPV(IPLV2,IPTYP,IPOPP) = NWRW_NewInfCap(index1) * MM2M * timeSettings%timestepSize / NRSHR
         enddo
      Enddo
    ELSE
      DO IPTYP=1,NPTYP
         DO IPOPP=1,NPOPP
            IPV (IPLV2,IPTYP,IPOPP) = 0.0
         enddo
      Enddo
    ENDIF

    ! ************************************************************************
    ! ****** Verdamping op oppervlak
    ! ************************************************************************

    DO IPTYP=1,NPTYP
      DO IPOPP=1,NPOPP
        BMAX = BMAXOP (IPTYP,IPOPP) * MM2M

        RPV(IPLV2,IPTYP,IPOPP) = AAFNodeRainfall(inode) * RAIN(IMETEO) * timeSettings%timestepSize
        VPV(IPLV2,IPTYP,IPOPP) = 0.0
        IF (BVOP0(IPLV2,IPTYP,IPOPP) .GT. 0)  VPV(IPLV2,IPTYP,IPOPP) = EVAP (IMETEO) * timeSettings%timestepSize
        BVOP (IPLV2,IPTYP,IPOPP) = BVOP0(IPLV2,IPTYP,IPOPP) - &
              VPV(IPLV2,IPTYP,IPOPP) + RPV (IPLV2,IPTYP,IPOPP) - &
              IPV(IPLV2,IPTYP,IPOPP)

    ! ************************************************************************
    ! ****** Check minimum/maximum berging op oppervlak
    ! ******   Bepaal netto neerslag, beschikbaar als afstroming
    ! ************************************************************************

        IF (BVOP (IPLV2,IPTYP,IPOPP) .GT. BMAX) THEN
           NTRAIN(IPLV2,IPTYP,IPOPP) = (BVOP (IPLV2,IPTYP,IPOPP) - BMAX)
           BVOP (IPLV2,IPTYP,IPOPP) = BMAX
        ELSE
           NTRAIN(IPLV2,IPTYP,IPOPP) = 0.0
           IF (BVOP(IPLV2,IPTYP,IPOPP) .LT. 0) THEN
    ! herverdeel verdamping en infiltratie volgens ratio
              RATIO =  INFCP(IPLV2,IPTYP,IPOPP,1) *MM2M + EVAP(IMETEO)*NRSHR
              RATIO =  INFCP(IPLV2,IPTYP,IPOPP,1) * MM2M / RATIO
              if (idebug .ne. 0) write (idebug,*) ' ratio IPV', ratio, IPV(IPLV2,IPTYP,IPOPP)
              IF (.NOT. INFDEP) RATIO = 1.0
              VPV (IPLV2,IPTYP,IPOPP) = (1.0-RATIO) * &
                (RPV (IPLV2,IPTYP,IPOPP) + BVOP0(IPLV2,IPTYP,IPOPP))
              IPV (IPLV2,IPTYP,IPOPP) =   RATIO * &
                (RPV (IPLV2,IPTYP,IPOPP) + BVOP0(IPLV2,IPTYP,IPOPP))
              BVOP (IPLV2,IPTYP,IPOPP) = 0.0
              if (idebug .ne. 0) write (idebug,*) ' ratio IPV after', ratio, IPV(IPLV2,IPTYP,IPOPP)
           ENDIF
        ENDIF
      enddo
    enddo

    ! ************************************************************************
    ! ****** Infiltratie uit afstroming
    ! ************************************************************************

    IF (INFAF) THEN
       DO IPTYP=1,NPTYP
          DO IPOPP=1,NPOPP
             index1 = (ipopp-1)*nptyp + iptyp
             index2 = (iplv2-1)*Npopp*Nptyp + (ipopp-1)*nptyp + iptyp
             NWRW_MinInfCap(index1) = InfCap(iptyp,2)
             NWRW_MaxInfCap(index1) = InfCap(iptyp,1)
             NWRW_RecoveryInfCap(index1) = TfinCp(iptyp,2)
             NWRW_DecreaseInfCap(index1) = TfinCp(iptyp,1)
             NWRW_PreviousInfCap(index1) = InfCp(iplv2,iptyp,ipopp,2)
             NWRW_NewInfCap(index1) = InfCp(iplv2,iptyp,ipopp,2)
             NWRW_Dt(index1)        = Dt(iplv2,iptyp,ipopp,2)
             NWRW_InitialStorage(index1) = NTRRS0(iplv2,iptyp,ipopp)
             NWRW_Rainfall(index1) = NTRain(iplv2,iptyp,ipopp)
             NWRW_InfSts(index1) = InfSts(iplv2,iptyp,ipopp,2)
          enddo
       Enddo
!     write(idebug,*) ' Before ArrayInfiltration_HortonFormula Afstroming standard types'
!     write(idebug,*)  ' MinInfCap  MaxInfCap  Recovery  Decrease Previous New Dt InitialStor Rainfall InfSts'
!     DO IPTYP=1,NPTYP
!        DO IPOPP=1,NPOPP
!           index1 = (ipopp-1)*nptyp + iptyp
!           index2 = (iplv2-1)*Npopp*Nptyp + (ipopp-1)*nptyp + iptyp
!           write(idebug,'(9F10.3,I3)')  NWRW_MinInfCap(index1), NWRW_MaxInfCap(index1), NWRW_RecoveryInfCap(index1),NWRW_DecreaseInfCap(index1), &
!                                          NWRW_PreviousInfCap(index1),NWRW_NewInfCap(index1), &
!                                            NWRW_Dt(index1),NWRW_InitialStorage(index1),NWRW_Rainfall(index1),NWRW_InfSts(index1)
!        enddo
!     Enddo
       Call Infiltration_HortonFormula  ( nptyp*npopp, &
                                         NWRW_MinInfCap,      NWRW_MaxInfCap,      &
                                         NWRW_DecreaseInfCap, NWRW_RecoveryInfCap, &
                                         NWRW_PreviousInfCap, NWRW_NewInfCap,      &
                                         Timesettings%TimestepSize,                &
                                         NWRW_DT,             NWRW_InitialStorage, &
                                         NWRW_Rainfall,       NWRW_InfSts,         NWRW_InfiltrationMM)
       DO IPTYP=1,NPTYP
          DO IPOPP=1,NPOPP
             index1 = (ipopp-1)*nptyp + iptyp
             InfCp(iplv2,iptyp,ipopp,2)  = NWRW_NewInfCap(index1)
             Dt (iplv2,iptyp,ipopp,2)  = NWRW_Dt(index1)
             InfSts(iplv2,iptyp,ipopp,2) = NWRW_InfSts(index1)
             ! Infiltratie IPV in m3 per tijdstap
             RINF2(index1) = NWRW_NewInfCap(index1) * MM2M * timeSettings%timestepSize / NRSHR
          enddo
       Enddo
    ELSE
       DO IPTYP=1,NPTYP
          DO IPOPP=1,NPOPP
             index1 = (ipopp-1)*nptyp + iptyp
             RINF2(index1) = 0.0
          enddo
       Enddo
    ENDIF

    ! Inloop riool en rest in m3 per tijdstap
    !  loop over de minuten in de tijdstap
    !   veronderstel NTRAIN etc homogeen verdeeld

    DO IPTYP=1,NPTYP
       DO IPOPP=1,NPOPP
          index1 = (ipopp-1)*nptyp + iptyp
          TOTUIT = 0.0
          VNOW   = NTRRS0(IPLV2,IPTYP,IPOPP)
          IF (.not. FixArs12253) then
!             write(*,*) ' Call RunoffFactorFormulation 1'
              Call RunoffFactorFormulation (VNow, NtRain(Iplv2,Iptyp,IpOpp), AfVrTr((iptyp-1)*npopp + ipopp),Rinf2(index1), &
                                          Idebug, TimeSettings%TimestepSize, TotUit, Vinf, .false. )
              IF (iDebug .ne. 0) WRITE(IDEBUG,*) ' RunoffFactor ',AfVrTr((iptyp-1)*npopp + ipopp)
              IF (iDebug .ne. 0) WRITE(IDEBUG,*) ' Infiltration rate ',Rinf2(index1)
              IF (iDebug .ne. 0) WRITE(IDEBUG,*) ' Infiltrated volume',Vinf
              IF (iDebug .ne. 0) WRITE(IDEBUG,*) ' VNOW VNETR TOTUIT',VNOW,NtRain(Iplv2,Iptyp,IpOpp),TOTUIT
          ElseIf (FixArs12253) then
!           write(*,*) ' Call RunoffFactorFormulation 2'
            Call RunoffFactorFormulation (VNow, NtRain(Iplv2,Iptyp,IpOpp), AfVrTr((iptyp-1)*npopp + ipopp),Rinf2(index1), &
                                         Idebug, TimeSettings%TimestepSize, TotUit, Vinf, .true. )
            IF (iDebug .ne. 0) WRITE(IDEBUG,*) ' RunoffFactor ',AfVrTr((iptyp-1)*npopp + ipopp)
            IF (iDebug .ne. 0) WRITE(IDEBUG,*) ' Infiltration rate ',Rinf2(index1)
            IF (iDebug .ne. 0) WRITE(IDEBUG,*) ' Infiltrated volume',Vinf
            IF (iDebug .ne. 0) WRITE(IDEBUG,*) ' VNOW VNETR TOTUIT',VNOW,NtRain(Iplv2,Iptyp,IpOpp),TOTUIT
          ENDIF

          IF (iDebug .ne. 0) WRITE(IDEBUG,*) ' AFVRTR ((iptyp-1)*npopp + ipopp)', AFVRTR((iptyp-1)*npopp + ipopp), iptyp, ipopp
          IF (iDebug .ne. 0) WRITE(IDEBUG,*) ' AFVRTR2 ((iptyp-1)*npopp + ipopp)', AFVRTR2((iptyp-1)*npopp + ipopp), iptyp, IPOPP
          IF (iDebug .ne. 0) WRITE(IDEBUG,*) ' LnAFVRTR ((iptyp-1)*npopp + ipopp)', LnAFVRTR((iptyp-1)*npopp + ipopp), iptyp, ipopp
          IF (iDebug .ne. 0) WRITE(IDEBUG,*) ' VNOW TOTUIT',VNOW,TOTUIT

          INPR(IPLV2,IPTYP,IPOPP) = TOTUIT
          NTRRST(IPLV2,IPTYP,IPOPP) = VNOW
          INFLAF(IPLV2,IPTYP,IPOPP) = VINF / timeSettings%timestepSize

    ! Totalen infiltratie en Inloop riool m3 per seconde
          INPRT (IPLV2) = INPRT (IPLV2) + INPR(IPLV2,IPTYP,IPOPP)/ timeSettings%timestepSize
          INFDP (IPLV2) = INFDP (IPLV2) + IPV(IPLV2,IPTYP,IPOPP) / timeSettings%timestepSize
    ! Totalen berging in m3
          VOLOP (IPLV2) = VOLOP (IPLV2) + BVOP  (IPLV2,IPTYP,IPOPP)
          VOLDYN(IPLV2) = VOLDYN(IPLV2) + NTRRST(IPLV2,IPTYP,IPOPP)


    ! *********************************************************************
    ! *** debug
    ! *********************************************************************

!         Write(*,*) ' Exit CmpPlv'
          IF (iDebug .ne. 0) THEN
             WRITE(IDEBUG,*) ' Pluvius area', Id_Nod(INODE)
             WRITE(IDEBUG,*) ' IPTYP IPOPP', IPTYP, IPOPP
             WRITE(IDEBUG,*) ' Timestep nr                 :',ITMSTP
             WRITE(IDEBUG,*) ' Timestepsize in s           :',timeSettings%timestepSize
             WRITE(IDEBUG,*) ' Surface level m             :',LVLPV (IPLV)
             WRITE(IDEBUG,*) ' Total surface in m2         :', &
                   1
             WRITE(IDEBUG,*) ' Dry weather flow people  m3 :',DWA(IPLV)
             WRITE(IDEBUG,*) ' Dry weather flow firms   m3 :',DWA2(IPLV)
             WRITE(IDEBUG,*) ' Final   storage surface. m3 :', BVOP  (IPLV2,IPTYP,IPOPP)
             WRITE(IDEBUG,*) ' Evaporation street in m3    :', VPV   (IPLV2,IPTYP,IPOPP)
             WRITE(IDEBUG,*) ' Precipitation in m3         :',  RPV   (IPLV2,IPTYP,IPOPP)
             WRITE(IDEBUG,*) ' Infiltration cap. surf mm/h :',  INFCP (IPLV2,IPTYP,IPOPP,1)
             WRITE(IDEBUG,*) ' Infiltration surface.    m3 :',  IPV   (IPLV2,IPTYP,IPOPP)
             WRITE(IDEBUG,*) ' Infiltration cap.runoff mm/h :', INFCP (IPLV2,IPTYP,IPOPP,2)
             WRITE(IDEBUG,*) ' Net precipitation        m3 :',  NTRAIN(IPLV2,IPTYP,IPOPP)
             WRITE(IDEBUG,*) ' Remaining dyn.storage   m3  :',  NTRRST(IPLV2,IPTYP,IPOPP)
             WRITE(IDEBUG,*) ' Inflow in m3                :',  INPR  (IPLV2,IPTYP,IPOPP)
             WRITE(IDEBUG,*) ' Cum. inflow sewer in m3/s   :',  INPRT (IPLV2)
             WRITE(IDEBUG,*) ' Cum. inf.depressions in m3/s:',  INFDP (IPLV2)
             WRITE(IDEBUG,*) '      Inf.runoff in m3/s     :',  INFLAF (IPLV2,IPTYP,IPOPP)
             WRITE(IDEBUG,*) ' Init. Cum.surf.storage in m3:',  VOLOP0 (IPLV2)
             WRITE(IDEBUG,*) ' Init. Cum. dyn.storage in m3:',  VOLDY0 (IPLV2)
             WRITE(IDEBUG,*) ' Cum. surf.storage in m3     :',  VOLOP  (IPLV2)
             WRITE(IDEBUG,*) ' Cum. dyn.storage in m3      :',  VOLDYN (IPLV2)
          ENDIF

    ! end loop over gebiedsklassen Pluvius

       ENDDO
    ENDDO

    ! *********************************************************************
    ! *** End
    ! *********************************************************************

    RETURN
  END subroutine CMPPLV



  SUBROUTINE SpecialCmpPlv (ITMSTP, IPLV, IMETEO, IPLV2, IPLV3, INODE)
    ! *********************************************************************
    ! *** Brief description:
    ! *** ------------------
    ! ***
    ! ***    This subroutine performs computations for Pluvius knopen - special areas
    ! ***   ! In vergelijking met overige type knopen is hier verdamping
    ! ***   ! nog niet afhankelijk van het uur op de dag! (niet nodig?)
    ! *********************************************************************
    ! *** Input/output parameters:
    ! *** ------------------------
    ! ***  IEVENT = event number
    ! ***  ITMSTP = timestep number
    ! ***  IDEBUG = file unit number of debug file
    ! ***  IOUT1  = file unit number of output file with run messages
    ! ***  IPLV   = intern nr. Pluvius knoop
    ! ***  IPLV2  = intern nr. ingedikte Pluvius knoop (=used special definitions * weather stations combinations)
    ! ***  IPLV3  = intern nr. van referentie naar Special Definitions
    ! ***  IMETEO = meteostation code
    ! ***  INODE  = knoopnr
    ! *********************************************************************
    ! *** Berekeningen voor Pluvius rioolinstromingsmodel
    ! *********************************************************************

    Integer iTmStp, iPlv, iMeteo, iPlv2, iNode, Iplv3
    Real dT1, dT2, rFrac
    Real bMax, ratio, rInf2, totUit, vNOw, vInf
    Real SpecialEvapGreenRoof
    Integer iDebug, index1

!   write(*,*) ' Start SpecialCmpPlv'

    Vinf = 0.0

    iDebug = ConfFil_get_iDebug()

    IF (iDebug .ne. 0)  WRITE(IDEBUG,*) 'SpecialCMPPLV iplv=',IPLV, IPLV2, IPLV3

    SpecialINPRT (IPLV2) = 0
    SpecialINFDP (IPLV2) = 0
    SpecialVOLOP (IPLV2) = 0
    SpecialVOLDYN(IPLV2) = 0

    if (SpecialNwrwAreaCompOption(iplv3) .ne. 0) then
       if (ConfArr_get_iHour().GE. timeSettings%evaporationFromHr .AND. &
            ConfArr_get_IHOUR() .LT. timeSettings%evaporationToHr) THEN
            SpecialEvapGreenRoof = EVAP(IMETEO) * timeSettings%timestepSize * 1000. * TMEVAP
       else
            SpecialEvapGreenRoof = 0.0
       endif
       Call GreenRoofBalance (SpecialNwrwAreaThetaInit(iplv2), SpecialNwrwAreaThetaFinal(iplv2), &
                              SpecialNwrwAreaThetaMin(iplv2), SpecialNwrwAreaThetaFieldCap(iplv2), SpecialNwrwAreaThetaSat(iplv2), &
                              SpecialNwrwAreaKSat(iplv3), SpecialNwrwAreaKp(iplv3), SpecialNwrwAreaCropFact(iplv3), &
                              AAFNodeRainfall(Inode)*Rain(imeteo)*timeSettings%timestepSize*1000., &
                              SpecialEvapGreenRoof, &
                              SpecialNwrwAreaEact(iplv2), SpecialNwrwAreaTotalRunoff(iplv2), Idebug)
       SpecialRPV(IPLV2) = AAFNodeRainfall(Inode) * RAIN(IMETEO) * timeSettings%timestepSize
       SpecialVPV(IPLV2) = SpecialNwrwAreaEact(iplv2) / 1000.
       SpecialINPR(IPLV2) = SpecialNwrwAreaTotalRunoff(iplv2) / 1000.
       SpecialINPRT (IPLV2) = SpecialINPRT (IPLV2) + SpecialINPR(IPLV2)/ timeSettings%timestepSize
       SpecialNTRRST(iplv2) = SpecialNwrwAreaThetaFinal(iplv2) / 1000.
    else

    ! ************************************************************************
    ! ****** Infiltratie uit berging op oppervlak (depressies)
    ! ******   max. berging (in invoer in mm) in volume omzetten
    ! *****    Infiltratiecap. in mm/uur
    ! ************************************************************************

       IF (SpecialINFilDepression(iplv3)) THEN
          index1 = 1
          NWRW_MinInfCap(index1) = SpecialMinInfCap(iplv3)
          NWRW_MaxInfCap(index1) = SpecialMaxInfCap(iplv3)
          NWRW_RecoveryInfCap(index1) = SpecialInfCapRecovery(iplv3)
          NWRW_DecreaseInfCap(index1) = SpecialInfCapDecrease(iplv3)
          NWRW_PreviousInfCap(index1) = SpecialInfCp(iplv2,1)
          NWRW_NewInfCap(index1)      = SpecialInfCp(iplv2,1)
          NWRW_Dt(index1)             = SpecialDt(iplv2,1)
          NWRW_InitialStorage(index1) = SpecialBVOP0(iplv2)
          NWRW_Rainfall(index1)       = AAFNodeRainfall(inode) * Rain(imeteo)
          NWRW_InfSts(index1)         = SpecialInfSts(iplv2,1)
!         write(idebug,*) ' Before Infiltration_HortonFormula Depressions special area', iplv3
!         write(idebug,*)  ' MinInfCap  MaxInfCap  Recovery  Decrease Previous  New   Dt   InitialStor Rainfall InfSts'
!         write(idebug,'(9F10.3,I3)')  NWRW_MinInfCap(index1), NWRW_MaxInfCap(index1), NWRW_RecoveryInfCap(index1),NWRW_DecreaseInfCap(index1), &
!                                        NWRW_PreviousInfCap(index1),NWRW_NewInfCap(index1), &
!                                          NWRW_Dt(index1),NWRW_InitialStorage(index1),NWRW_Rainfall(index1),NWRW_InfSts(index1)

          Call Infiltration_HortonFormula (1,NWRW_MinInfCap, NWRW_MaxInfCap, &
                                           NWRW_DecreaseInfCap, NWRW_RecoveryInfCap, &
                                           NWRW_PreviousInfCap, NWRW_NewInfCap, &
                                           Timesettings%TimestepSize, NWRW_Dt,&
                                           NWRW_InitialStorage, NWRW_Rainfall, NWRW_InfSts,NWRW_InfiltrationMM)

          SpecialInfCp(iplv2,1)  = NWRW_NewInfCap(index1)
          SpecialDt (iplv2,1)    = NWRW_Dt(index1)
          SpecialInfSts(iplv2,1) = NWRW_InfSts(index1)
         ! Infiltratie IPV in m3 per tijdstap
          SpecialIPV(IPLV2) = SpecialINFCP(IPLV2,1) * MM2M * timeSettings%timestepSize / NRSHR
       ELSE
          SpecialIPV(IPLV2) = 0.0
       ENDIF

    ! ************************************************************************
    ! ****** Verdamping op oppervlak
    ! ************************************************************************

       BMAX = SpecialMaxStorage(Iplv3) * MM2M
       SpecialRPV(IPLV2) = AAFNodeRainfall(Inode) * RAIN(IMETEO) * timeSettings%timestepSize
       SpecialVPV(IPLV2) = 0.0
       IF (SpecialBVOP0(IPLV2) .GT. 0) SpecialVPV(IPLV2) = EVAP (IMETEO) * timeSettings%timestepSize * SpecialEvapFact(Iplv3)
       SpecialBVOP (IPLV2) = SpecialBVOP0(IPLV2) - SpecialVPV(IPLV2) + SpecialRPV (IPLV2) - SpecialIPV(IPLV2)

    ! ************************************************************************
    ! ****** Check minimum/maximum berging op oppervlak
    ! ******   Bepaal netto neerslag, beschikbaar als afstroming
    ! ************************************************************************

       IF (SpecialBVOP (IPLV2) .GT. BMAX) THEN
          SpecialNTRAIN(IPLV2) = (SpecialBVOP (IPLV2) - BMAX)
          SpecialBVOP (IPLV2) = BMAX
       ELSE
          SpecialNTRAIN(IPLV2) = 0.0
          IF (SpecialBVOP(IPLV2) .LT. 0) THEN
    ! herverdeel verdamping en infiltratie volgens ratio
             RATIO =  SpecialINFCP(IPLV2,1) * MM2M + EVAP(IMETEO)*NRSHR
             RATIO =  SpecialINFCP(IPLV2,1) * MM2M / RATIO
             if (idebug .ne. 0) write (idebug,*) ' ratio IPV', ratio, SpecialIPV(IPLV2)
             IF (.NOT. SpecialINFilDEPression(iplv2)) RATIO = 1.0
             SpecialVPV (IPLV2) = (1.0-RATIO) * (SpecialRPV (IPLV2) + SpecialBVOP0(IPLV2))
             SpecialIPV (IPLV2) =   RATIO * (SpecialRPV (IPLV2) + SpecialBVOP0(IPLV2))
             SpecialBVOP (IPLV2) = 0.0
             if (idebug .ne. 0) write (idebug,*) ' ratio IPV after', ratio, SpecialIPV(IPLV2)
          ENDIF
       ENDIF

    ! ************************************************************************
    ! ****** Infiltratie uit afstroming
    ! ************************************************************************

!       write(idebug,*) ' Before Infiltration_HortonFormula Depressions special area', iplv3
!       write(idebug,*)  ' MinInfCap  MaxInfCap  Recovery  Decrease Previous  New   Dt   InitialStor Rainfall InfSts'
!       write(idebug,'(9F10.3,I3)')  NWRW_MinInfCap(index1), NWRW_MaxInfCap(index1), NWRW_RecoveryInfCap(index1),NWRW_DecreaseInfCap(index1), &
!                                        NWRW_PreviousInfCap(index1),NWRW_NewInfCap(index1), &
!                                          NWRW_Dt(index1),NWRW_InitialStorage(index1),NWRW_Rainfall(index1),NWRW_InfSts(index1)

       IF (SpecialINFilRunoff(iplv3)) THEN
          NWRW_MinInfCap(index1)      = SpecialMinInfCap(iplv3)
          NWRW_MaxInfCap(index1)      = SpecialMaxInfCap(iplv3)
          NWRW_RecoveryInfCap(index1) = SpecialInfCapRecovery(iplv3)
          NWRW_DecreaseInfCap(index1) = SpecialInfCapDecrease(iplv3)
          NWRW_PreviousInfCap(index1) = SpecialInfCp(iplv2,2)
          NWRW_NewInfCap(index1)      = SpecialInfCp(iplv2,2)
          NWRW_Dt(index1)             = SpecialDt(iplv2,2)
          NWRW_InitialStorage(index1) = SpecialNTRRS0(iplv2)
          NWRW_Rainfall(index1)       = SpecialNTRain(iplv2)
          NWRW_InfSts(index1)         = SpecialInfSts(iplv2,2)

          Call Infiltration_HortonFormula (1, NWRW_MinInfCap, NWRW_MaxInfCap, &
                                           NWRW_DecreaseInfCap, NWRW_RecoveryInfCap, &
                                           NWRW_PreviousInfCap, NWRW_NewInfCap, &
                                           Timesettings%TimestepSize, NWRW_Dt,&
                                           NWRW_InitialStorage, NWRW_Rainfall, NWRW_InfSts,NWRW_InfiltrationMM)

          SpecialInfCp(iplv2,2)  = NWRW_NewInfCap(index1)
          SpecialDt (iplv2,2)    = NWRW_Dt(index1)
          SpecialInfSts(iplv2,2) = NWRW_InfSts(index1)
    ! Infiltratie uit afstroming RINF2 in m3 per tijdstap
          RINF2 = SpecialINFCP(IPLV2,2) * MM2M * timeSettings%timestepSize / NRSHR
          if (Idebug .ne. 0) write(idebug,'(A,I4,2E11.4)')  ' Special',index1, NWRW_InfiltrationMM(index1), RINF2

       ELSE
         RINF2 = 0.0
       ENDIF
    ! Inloop riool en rest in m3 per tijdstap
    !  loop over de minuten in de tijdstap
    !   veronderstel NTRAIN etc homogeen verdeeld

       TOTUIT = 0.0
       VNOW   = SpecialNTRRS0(IPLV2)
       IF (.not. FixArs12253) then
           IF (iDebug .ne. 0) WRITE(IDEBUG,*) ' RunoffFactor ',SpecialRunoffDelay(iplv3)
           IF (iDebug .ne. 0) WRITE(IDEBUG,*) ' Infiltration rate ',Rinf2
           IF (iDebug .ne. 0) WRITE(IDEBUG,*) ' Infiltrated volume',Vinf
           IF (iDebug .ne. 0) WRITE(IDEBUG,*) ' VNOW VNETR TOTUIT',VNOW,SpecialNtRain(Iplv2),TOTUIT
!          write(*,*) ' Call RunoffFactorFormulation 3'
           Call RunoffFactorFormulation (VNow, SpecialNtRain(Iplv2), SpecialRunoffDelay(Iplv3),Rinf2, &
                                        Idebug, TimeSettings%TimestepSize, TotUit, Vinf, .false. )
       ElseIf (FixArs12253) then
         IF (iDebug .ne. 0) WRITE(IDEBUG,*) ' RunoffFactor ',SpecialRunoffDelay(iplv3)
         IF (iDebug .ne. 0) WRITE(IDEBUG,*) ' Infiltration rate ',Rinf2
         IF (iDebug .ne. 0) WRITE(IDEBUG,*) ' Infiltrated volume',Vinf
         IF (iDebug .ne. 0) WRITE(IDEBUG,*) ' VNOW VNETR TOTUIT',VNOW,SpecialNtRain(Iplv2),TOTUIT
!          write(*,*) ' Call RunoffFactorFormulation 4'
         Call RunoffFactorFormulation (VNow, SpecialNtRain(Iplv2), SpecialRunoffDelay(iplv3),Rinf2, &
                                      Idebug, TimeSettings%TimestepSize, TotUit, Vinf, .true. )
       Endif
       IF (iDebug .ne. 0) WRITE(IDEBUG,*) ' SpecialRunoffDelay(Iplv3)', SpecialRunoffDelay(Iplv3), IPlv2
       IF (iDebug .ne. 0) WRITE(IDEBUG,*) ' SpecialRunoffDelay2(Iplv3)', SpecialRunoffDelay2(Iplv3), IPlv2
       IF (iDebug .ne. 0) WRITE(IDEBUG,*) ' RINF2   VINF ', RINF2, VINF
       IF (iDebug .ne. 0) WRITE(IDEBUG,*) ' VNOW TOTUIT',VNOW,TOTUIT

       SpecialINPR(IPLV2) = TOTUIT
       SpecialNTRRST(IPLV2) = VNOW
       SpecialINFLAF(IPLV2) = VINF / timeSettings%timestepSize

    ! Totalen infiltratie en Inloop riool m3 per seconde
       SpecialINPRT (IPLV2) = SpecialINPRT (IPLV2) + SpecialINPR(IPLV2)/ timeSettings%timestepSize
       SpecialINFDP (IPLV2) = SpecialINFDP (IPLV2) + SpecialIPV(IPLV2) / timeSettings%timestepSize
    ! Totalen berging in m3
       SpecialVOLOP (IPLV2) = SpecialVOLOP (IPLV2) + SpecialBVOP  (IPLV2)
       SpecialVOLDYN(IPLV2) = SpecialVOLDYN(IPLV2) + SpecialNTRRST(IPLV2)

    ! *********************************************************************
    ! *** debug
    ! *********************************************************************

      IF (iDebug .ne. 0) THEN
         WRITE(IDEBUG,*) ' Pluvius area', Id_Nod(INODE)
         WRITE(IDEBUG,*) ' Timestep nr                 :',ITMSTP
         WRITE(IDEBUG,*) ' Timestepsize in s           :',timeSettings%timestepSize
         WRITE(IDEBUG,*) ' Surface level m             :',LVLPV (IPLV)
         WRITE(IDEBUG,*) ' Total surface in m2         :', &
               1
         WRITE(IDEBUG,*) ' Dry weather flow people  m3 :',DWA(IPLV)
         WRITE(IDEBUG,*) ' Dry weather flow firms   m3 :',DWA2(IPLV)
         WRITE(IDEBUG,*) ' Final   storage surface. m3 :',  SpecialBVOP  (IPLV2)
         WRITE(IDEBUG,*) ' Evaporation street in m3    :',  SpecialVPV   (IPLV2)
         WRITE(IDEBUG,*) ' Precipitation in m3         :',  SpecialRPV   (IPLV2)
         WRITE(IDEBUG,*) ' Infiltration cap. surf mm/h :',  SpecialINFCP (IPLV2,1)
         WRITE(IDEBUG,*) ' Infiltration surface.    m3 :',  SpecialIPV   (IPLV2)
         WRITE(IDEBUG,*) ' Infiltration cap.runoff mm/h :', SpecialINFCP (IPLV2,2)
         WRITE(IDEBUG,*) ' Net precipitation        m3 :',  SpecialNTRAIN(IPLV2)
         WRITE(IDEBUG,*) ' Remaining dyn.storage   m3  :',  SpecialNTRRST(IPLV2)
         WRITE(IDEBUG,*) ' Inflow in m3                :',  SpecialINPR  (IPLV2)
         WRITE(IDEBUG,*) ' Cum. inflow sewer in m3/s   :',  SpecialINPRT (IPLV2)
         WRITE(IDEBUG,*) ' Cum. inf.depressions in m3/s:',  SpecialINFDP (IPLV2)
         WRITE(IDEBUG,*) '      Inf.runoff in m3/s     :',  SpecialINFLAF (IPLV2)
         WRITE(IDEBUG,*) ' Init. Cum.surf.storage in m3:',  SpecialVOLOP0 (IPLV2)
         WRITE(IDEBUG,*) ' Init. Cum. dyn.storage in m3:',  SpecialVOLDY0 (IPLV2)
         WRITE(IDEBUG,*) ' Cum. surf.storage in m3     :',  SpecialVOLOP  (IPLV2)
         WRITE(IDEBUG,*) ' Cum. dyn.storage in m3      :',  SpecialVOLDYN (IPLV2)
      ENDIF

    ! end loop over gebiedsklassen Pluvius

    ENDIF

    ! *********************************************************************
    ! *** End
    ! *********************************************************************

    RETURN
  END subroutine SpecialCmpPlv


  SUBROUTINE ComputeWadi(Iplv, inode, RWadiInflow)

  Integer    Itmstp, Iplv, Inode, idebug
  Real       RWadiInflow
  real       WadiArea, WadiStor, WadiDepth, head

  iDebug = ConfFil_get_iDebug()

  ! Wadi balance
  ! July 2011: adjusted order of computation (spill first!), and in volume check take into account porosity

  WadiArea = max(0.001, WadiLength(iplv) * WadiWidth(iplv))
  WadiStor = WadiInitialStorage(iplv)
  WadiDepth = WadiStor / WadiArea

  if (idebug .ne. 0)  then
     write(idebug,*) ' ComputeWadi', iplv
     write(idebug,*) ' initial storage (m3)', WadiInitialStorage(iplv)
     write(idebug,*) ' inflow       (m3/s)  ', RWadiInflow
     write(idebug,*) ' area storage depth (m) ', WadiArea, WadiStor, WadiDepth
     write(idebug,*) ' wadi initial level (m) ', WadiInitialLevel(iplv)
  endif

  ! spill
  WadiSpillOutflow(iplv) = 0.
  head = WadiInitialLevel(iplv) - WadiSpillLvl(iplv)
  if (Head .gt. 0)  then
     WadiSpillOutflow(iplv) = WadiSpillDischargeCoef(iplv) * WadiSpillWidth(iplv) * 2./3. * Sqrt(2./3.*9.81) * Head * Sqrt(Head)
     WadiSpillOutflow(iplv) = min (Head * WadiArea * WadiPorosity(iplv) / TimeSettings%TimestepSize, WadiSpillOutflow(iplv))
  endif
  if (idebug .ne. 0)  then
     write(idebug,*) ' spill head (m) and flow (m3/s)', head, WadiSpillOutflow(iplv)
     write(idebug,*) ' wadi area  (m2) porosity      ', WadiArea, WadiPorosity(iplv)
     write(idebug,*) ' init level, spill level       ', WadiInitialLevel(iplv), WadiSpillLvl(iplv)
  endif

  ! infiltration
  Head = WadiInitialLevel(iplv) - WadiDrainGroundwaterLvl(iplv)
  WadiInfiltration(iplv) = Head / WadiInfiltrationResistance(iplv) / 86400. * WadiArea
  if (idebug .ne. 0)  then
     write(idebug,*) ' wadi infiltration head (m) and inf.flow (m3/s)', head, WadiInfiltration(iplv)
     write(idebug,*) ' wadi infiltration in m/s                      ', WadiInfiltration(iplv) / WadiArea
  endif

  ! drain
  head  = WadiInitialLevel(iplv) - WadiDrainLvl(iplv)
  WadiDrainOutflow(iplv) = 0.0
  if (Head .gt. 0)  then
      if (WadiDrainFormula(iplv) .eq. 0) then
         if (Head - WadiDrainContractionCoef(iplv) * WadiDrainExitHeight(iplv) .gt. 0) then ! orifice formula
            WadiDrainOutflow(iplv) = WadiDrainDischargeCoef(iplv) * WadiDrainContractionCoef(iplv)* WadiDrainExitWidth(iplv)* WadiDrainExitHeight(iplv) * Sqrt(2.*9.81) * Sqrt(Head - WadiDrainContractionCoef(iplv)*WadiDrainExitHeight(iplv))
            WadiDrainOutflow(iplv) = min (Head * WadiArea * WadiPorosity(iplv) / TimeSettings%TimestepSize, WadiDrainOutflow(iplv) )
         else ! july 2011:  was nul, nu weir formule met contr. coeff. erbij?
            WadiDrainOutflow(iplv) = WadiDrainDischargeCoef(iplv) * WadiDrainContractionCoef(iplv)* WadiDrainExitWidth(iplv) * 2./3. * Sqrt(2./3.*9.81) * Head * Sqrt(Head)
            WadiDrainOutflow(iplv) = min (Head * WadiArea * WadiPorosity(iplv) / TimeSettings%TimestepSize, WadiDrainOutflow(iplv) )
         endif
      elseif (WadiDrainFormula(iplv) .eq. 1) then
          ! weir formula
         WadiDrainOutflow(iplv) = WadiDrainDischargeCoef(iplv) * WadiDrainExitWidth(iplv) * 2./3. * Sqrt(2./3.*9.81) * Head * Sqrt(Head)
         WadiDrainOutflow(iplv) = min (Head * WadiArea * WadiPorosity(iplv) / TimeSettings%TimestepSize, WadiDrainOutflow(iplv))
      endif
  endif
  if (idebug .ne. 0)  then
     write(idebug,*) ' drain head (m) and flow (m3/s)', head, WadiDrainOutflow(iplv)
     write(idebug,*) ' wadi area  (m2) porosity      ', WadiArea, WadiPorosity(iplv)
     write(idebug,*) ' init level, drain level       ', WadiInitialLevel(iplv), WadiDrainLvl(iplv)
     write(idebug,*) ' wadi drain disch coeff        ', WadiDrainDischargeCoef(iplv)
     write(idebug,*) ' wadi drain contr coeff        ', WadiDrainContractionCoef(iplv)
     write(idebug,*) ' wadi drain exit width         ', WadiDrainExitWidth(iplv)
     write(idebug,*) ' wadi drain exit height        ', WadiDrainExitHeight(iplv)
  endif
  ! balance
  WadiFinalStorage(iplv) = WadiInitialStorage(iplv) + RWadiInflow * TimeSettings%TimestepSize - (WadiSpillOutflow(iplv) + WadiDrainOutflow(iplv) + WadiInfiltration(iplv)) * TimeSettings%TimestepSize
  WadiFinalLevel(iplv) = WadiFinalStorage(iplv) / WadiArea / WadiPorosity(iplv) + WadiBedLvl(iplv)

  if (idebug .ne. 0)  then
     write(idebug,*) ' wadi infiltration ', WadiInfiltration(iplv)
     write(idebug,*) ' wadi storage ', WadiInitialStorage(iplv), WadiFinalStorage(iplv)
     write(idebug,*) ' wadi final level  ', WadiFinalLevel(iplv)
  endif

  Return
  End subroutine ComputeWadi


  SUBROUTINE AGGPLV (ICall)
    ! *********************************************************************
    ! *** Last update: Mar 1996       By : Geert Prinsen
    ! *********************************************************************
    ! *** Brief description:
    ! *** ------------------
    ! ***   Aggregeert Pluvius knopen
    ! *********************************************************************
    ! *** Input/output parameters:
    ! *** ------------------------
    ! ***  IDEBUG = file unit number of debug file
    ! ***  IOUT1  = file unit number of output file with messages
    ! ***  ICall  = 1 = eerste call, alleen bepaling NCPLV2 en NcPlv3
    ! ***           2 = tweede call, alles.
    ! *********************************************************************

    Integer iCall, index, iNod, iType, iMeteo, iPlv, iRio, j, k
    Integer jNod, jType, jPlv, jMeteo, iDebug
    Character(Len=CharIdLength) IdOfNode


    iDebug = ConfFil_get_iDebug()

    IF (iDebug .ne. 0) WRITE (IDEBUG,1)
  1 FORMAT (' AGGPLV')


    ! *********************************************************************
    ! *** Bepaal 'identieke' Pluvius knopen
    ! ***  - zelfde weerstation
    ! ***  - geen riool in Delft_3B meenemen
    ! ***
    ! ***  - zelfde inloopparameters (nog per definitie!)
    ! ***  - zelfde DWA hoeft niet!
    ! *********************************************************************

    INDEX = 0
    DO 10  INOD=1,NCNODE
       ITYPE  = EiNode(INOD,3)
       IF (ITYPE .EQ. 7) THEN
         IMETEO = NODMET(INOD)
         IPLV   = EiNode(INOD,2)
         IRIO   = IRIOOL(IPLV)
         DO JNOD=1,INOD-1
           JTYPE  = EiNode(JNOD,3)
           JMETEO = NODMET(JNOD)
           ! check that type of node is the same, and that used meteostations are the same, !jan2009: and area adjustment factors
           IF (ITYPE .EQ. JTYPE .AND. IMETEO .EQ. JMETEO .and. AafNodeRainfall(jnod) .eq. AafNodeRainfall(inod)) THEN
              JPLV   = EiNode(JNOD,2)
              IF (IRIO .EQ. IRIOOL(JPLV)) THEN
                 IF (ICall .EQ. 2) INDIKP(IPLV) = INDIKP(JPLV)
                 GOTO 10
              ENDIF
            ENDIF
          ENDDO
          INDEX = INDEX + 1
          IF (ICall .EQ. 2) INDIKP(IPLV) = INDEX
       ENDIF
 10 CONTINUE
    ncPLV2 = INDEX
    nPlv2 = ncPlv2

!  ARS 11173-74 add special areas
    ! ***  - zelfde weerstation
    ! ***  - zelfde inloopparameters
    INDEX = 0
    If (TotNrSpecialNwrwAreas .gt. 0) then
        DO INOD=1,NCNODE
           ITYPE  = EiNode(INOD,3)
           IF (ITYPE .EQ. 7) THEN
             IMETEO = NODMET(INOD)
             IPLV   = EiNode(INOD,2)
             IRIO   = IRIOOL(IPLV)
             If (NrSpecialNwrwAreas(Iplv) .gt. 0) then
              Do J=1,NrSpecialNwrwAreas(iplv)
                DO JNOD=1,INOD-1
                  JTYPE  = EiNode(JNOD,3)
                  JMETEO = NODMET(JNOD)
                  ! check that type of node is the same, and that used meteostations are the same  !jan2009: and area adjustment factors
                  IF (ITYPE .EQ. JTYPE .AND. IMETEO .EQ. JMETEO .and. AafNodeRainfall(jnod) .eq. AafNodeRainfall(inod)) THEN
                     JPLV   = EiNode(JNOD,2)
                     IF (IRIO .EQ. IRIOOL(JPLV)) THEN
                        If (NrSpecialNwrwAreas(jplv) .gt. 0) then
                           Do k=1,NrSpecialNwrwAreas(jplv)
                              ! same special definition should be used as well
                              IF (Reference2SpecialDef(iplv,j) .eq. Reference2SpecialDef(jplv,k)) then
                                 IF (ICall .EQ. 2) SpecialINDIKP(IPLV,j) = SpecialINDIKP(JPLV,k)
                                 GOTO 21
                              ENDIF
                           Enddo
                        ENDIF
                     ENDIF
                   ENDIF
                ENDDO
                INDEX = INDEX + 1
                IF (ICall .EQ. 2) SpecialINDIKP(IPLV,j) =  Index ! eerst foutief Reference2SpecialDef(iplv,j)
     21         CONTINUE
              Enddo
             Endif
           ENDIF
     20    CONTINUE
        Enddo
        ncPLV3 = INDEX
        NPlv3 = max (Ncplv3, TotNrSpecialNwrwAreas)
    Else
        NcPlv3 = 0
        NPlv3 = max (1, TotNrSpecialNwrwAreas)
    Endif
!  End

    IF (ICall .EQ. 2 .and. Idebug .gt. 0) then
       WRITE (IDEBUG,*) ' Generalising Pluvius nodes'
       WRITE (IDEBUG,*) ' Number of generalised nodes ', NCPLV2, NcPlv3
       WRITE (IDEBUG,*) '  INODE    NodeIdentification    IPLV   INDIK_index SpecialIndik_indices'
       DO INOD=1,NCNODE
         IF (EiNode(INOD,3) .EQ. 7) THEN
           IPLV = EiNode(INOD,2)
           IDOfNode = Id_Nod(INOD)
           WRITE(IDEBUG,'(I8,1X,A32,9I8)') INOD, IdOfNode, IPLV, INDIKP(IPLV), &
                                   (SpecialIndikP(Iplv,j),j=1,NrSpecialNwrwAreas(iplv))
         ENDIF
       ENDDO
    ENDIF

    iDebug = 0

    RETURN
  END subroutine aggPlv





  Subroutine Init1NWRW (Idebug)

    ! *********************************************************************
    ! *** Last update:
    ! *********************************************************************
    ! *** Brief description:
    ! *** ------------------
    ! ***    Stuk uit sub Init1: initialisie van NWRW nodes per event
    ! *********************************************************************

      Implicit none

      Integer Idebug, iplv, Iplv2, IpTyp, IpOpp, iplv3, j
      Real    Dt

!     Idebug = 801


 !NWRW inloopknopen

! Vector/array initialisation
! Default zero
        VOLOP  = 0
        VOLDYN = 0
        BVOP   = 0.0
        DT     = 0
        INFSTS = 1
        NTRAIN = 0.0
        NTRRST = 0.0
        QInPluv= 0.0
!   Initialisatie
        QPluv%totalPaved      = 0.0
        QPluv%totalUnpaved    = 0.0
        QPluv%totalGreenhouse = 0.0
        QPluv%totalStructure  = 0.0
        QPluv%totalRwzi       = 0.0
        QPluv%totalIndustry   = 0.0
        QPluv%totalSacramento = 0.0


        DO IPLV2 = 1,NCPLV2
!         VOLOP  (IPLV2) = 0
!         VOLDYN (IPLV2) = 0
          DO IPTYP= 1,NPTYP
            DO IPOPP= 1,NPOPP
! initiele berging op opp=0 of maximaal?
!              BVOP (IPLV2,IPTYP, IPOPP) = 0.0
!!             BVOP (IPLV2,IPTYP, IPOPP) = BMAXOP (IPTYP, IPOPP) * MM2M
! 1 regel gecorrigeerd; VOLOP0 moest zijn VOLOP
               VOLOP (IPLV2) = VOLOP (IPLV2) + BVOP(IPLV2,IPTYP,IPOPP)
               if (idebug .ne. 0)  WRITE(IDEBUG,*) 'INIT1', IPTYP, IPOPP, BVOP(IPLV2,IPTYP,IPOPP)
!              DT (IPLV2,IPTYP, IPOPP,1) = 0
!              DT (IPLV2,IPTYP, IPOPP,2) = 0
               INFCP  (IPLV2,IPTYP, IPOPP,1) = INFCAP(IPTYP,1)
               INFCP  (IPLV2,IPTYP, IPOPP,2) = INFCAP(IPTYP,1)
! April 2002 !!
! maakt geen verschil
!!             INFCP  (IPLV2,IPTYP, IPOPP,2) = INFCAP(IPTYP,2)
!              INFSTS (IPLV2,IPTYP, IPOPP,1) = 1
!              INFSTS (IPLV2,IPTYP, IPOPP,2) = 1
!              NTRAIN (IPLV2,IPTYP, IPOPP) = 0.0
!              NTRRST (IPLV2,IPTYP, IPOPP) = 0.0
               IF (.NOT. INFDEP)  INFCP(IPLV2,IPTYP,IPOPP,1) = 0.0
               IF (.NOT. INFAF)   INFCP(IPLV2,IPTYP,IPOPP,2) = 0.0
            ENDDO
          ENDDO
        ENDDO

! ARS 11173 Feb 2003
! Add special areas
        SpecialVolOp  = 0
        SpecialVOLDYN = 0
        SpecialBVOP   = 0.0
        SpecialDT     = 0
        SpecialINFSTS = 1
        SpecialNTRAIN = 0.0
        SpecialNTRRST = 0.0
!       DO IPLV2 = 1,NCPLV3
!       DO IPLV2 = 1,NPLV3
        Do iplv =1,Ncpluv
           Do j=1,NrSpecialNwrwAreas(iplv)
              Iplv2 = SpecialInDikP(Iplv,j)
              Iplv3 = Reference2SpecialDef(Iplv,j)
              SpecialVOLOP (IPLV2) = SpecialVOLOP (IPLV2) + SpecialBVOP(IPLV2)
              SpecialInfCp(IPLV2,1) = SpecialMaxINFCAP(IPlv3)
              SpecialInfCp(IPLV2,2) = SpecialMaxINFCAP(IPlv3)
              IF (.NOT. SpecialInfilDepression(iplv3))  SpecialINFCP(IPLV2,1) = 0.0
              IF (.NOT. SpecialInfilRunoff(iplv3))      SpecialINFCP(IPLV2,2) = 0.0
              if (idebug .ne. 0) then
                 write(idebug,*) ' SpecialInfDep  SpecialInfCp ', SpecialInfilDepression(iplv3), SpecialInfCp(iplv2,1)
                 write(idebug,*) ' SpecialInfRof  SpecialInfCp ', SpecialInfilRunoff(iplv3), SpecialInfCp(iplv2,2)
              Endif
              If (SpecialNwrwAreaCompOption(iplv3) .ne. 0) then
                 Call  GreenRoofInit (SpecialNwrwAreaThetaInit(iplv2), SpecialNwrwAreaThetaMin(iplv2), &
                                      SpecialNwrwAreaThetaFieldCap(iplv2), SpecialNwrwAreaThetaSat(iplv2), &
                                      SpecialNwrwAreaThetaInitPercnt(iplv3), SpecialNwrwAreaThetaMinPercnt(iplv3), &
                                      SpecialNwrwAreaThetaFieldCapPercnt(iplv3), SpecialNwrwAreaThetaSatPercnt(iplv3), &
                                      SpecialNwrwAreaSoilThickness(iplv3) , Idebug)
                  SpecialNwrwAreaThetaFinal(iplv2) = SpecialNwrwAreaThetaInit(iplv2)
                  SpecialNTRRST(iplv2) = SpecialNwrwAreaThetaInit(iplv2) / 1000.
                  SpecialNTRRS0(IPLV2) = SpecialNTRRST(IPLV2)
              Endif
           Enddo
        Enddo
!       ENDDO
! end ARS 11173

! Add Afvrtr2 and specialRunoffDelay2 initialisation
       if (Timesettings%timestepsize .lt. 60) then
           dt = (1.0 * Timesettings%timestepsize/ NrsMin)
           Do ipOpp=1,NpOpp
            Do iptyp=1,NpTyp
              Afvrtr2((iptyp-1)*npopp + ipopp) = (1. - (1.-Afvrtr((iptyp-1)*npopp + ipopp)) ** dt) /  dt
              if (idebug .ne. 0) then
                 write(idebug,*) ' Afvrtr  dt ', Afvrtr((iptyp-1)*npopp + ipopp), dt
                 write(idebug,*) ' Afvrtr2    ', Afvrtr2((iptyp-1)*npopp + ipopp)
              Endif
            Enddo
           Enddo
           Do IPLV2 = 1,NPLV3
!             SpecialRunoffDelay2(Iplv2) = (1.- (1.-SpecialRunoffDelay(iplv2)) ** dt) / (1.-SpecialRunoffDelay(iplv2))
              SpecialRunoffDelay2(Iplv2) = (1.- (1.-SpecialRunoffDelay(iplv2)) ** dt) / dt
           Enddo
           iDebug = 0
       Endif

       DO iplv=1,ncpluv
          WadiInitialLevel(iplv) = WadiDrainInitialWaterLvl(iplv)
          WadiInitialStorage(iplv) = (WadiDrainInitialWaterLvl(iplv) - WadiBedLvl(iplv)) * WadiLength(iplv) * WadiWidth(iplv) * WadiPorosity(iplv)
          WadiFinalLevel(iplv)   = WadiInitialLevel(iplv)
          WadiFinalStorage(iplv) = WadiInitialStorage(iplv)
       Enddo

  Return
  END subroutine Init1NWRW




  Subroutine Init2NWRW (Idebug, Iout1, IHour, NRSHR, Itmstp)

    ! *********************************************************************
    ! *** Last update:
    ! *********************************************************************
    ! *** Brief description:
    ! *** ------------------
    ! ***    Stuk uit sub Init2: initialisie van NWRW nodes per tijdstap
    ! *********************************************************************

      Implicit none

      Integer Idebug, Iout1, iHour, Nrshr, Itmstp
      Integer IPLV, IPlv2, IpTyp, IPOpp, Idwa
      Real    RFrac
      Integer RowNr, TabelNr
      Logical DateTimeOutsideTable

      type (Date) currentDate
      type (Time) currentTime

! *********************************************************************
! *** bereken droogweerafvoer
! ***   optie 1: aantal inwoners * constante dwa per capita per uur
! ***   optie 2: aantal inwoners * variabele dwa per capita per uur
! ***   optie 3:   1  *            constante dwa per uur
! ***   optie 4:   1  *            variabele dwa per uur
! *** input in liter per hour, liter per day, 24 percentages.
! ***   optie 5:   DWA in m3/s uit een tabel
! *********************************************************************

      IF (iDebug .ne. 0) WRITE(IDEBUG,*) ' NCPLUV NCPLV2', NCPLUV, NCPLV2

 !NWRW inloopknopen
      ! DWA people
      DO IPLV = 1,NCPLUV
        IDwa = ReferenceToDwaDef(iplv)
        ! DWA people is obliged, so idwa is here always > 0
        IF (DWAOPT(Idwa) .EQ. 1) THEN
            DWA(IPLV) = DWAINW(IPLV) * DWADIS(IDwa,1) * 0.001 / NRSHR
        ELSEIF (DWAOPT(Idwa) .EQ. 2) THEN
            DWA(IPLV) = DWAINW(IPLV) * DWADIS(IDwa,2) * 0.001 / nrsHr   &
                                      * 0.01 * DWADIS(IDwa, IHour + 3)
        !+ 3 omdat de eerste 2 elementen uit het array ergens anders voor worden gebruikt !
        ELSEIF (DWAOPT(Idwa) .EQ. 3) THEN
            DWA(IPLV) = DWADIS(IDwa,1) * 0.001 / NRSHR
        ELSEIF (DWAOPT(Idwa) .EQ. 4) THEN
            DWA(IPLV) = DWADIS(IDwa,2) * 0.001 / nrsHr  &
                                      * 0.01 * DWADIS(IDwa,IHour + 3)
        !+ 3 omdat de eerste 2 elementen uit het array ergens anders voor worden gebruikt ! en ihour=0 tm 23
        ELSEIF (DWAOPT(Idwa) .EQ. 5) THEN
! August 2001: DWA through a time table
          DWA(Iplv) = 0.0
          currentDate%year = ConfArr_get_iYear()
          currentDate%month = ConfArr_get_iMonth()
          currentDate%day = ConfArr_get_iDay()
          currentTime%hour = Ihour
          currentTime%minute = ConfArr_get_iMinute()
          currentTime%second = 0
          RowNr = -1
          TabelNr = NwrwDWATable (idwa)
          If (TabelNr .gt. 0) then
              DWA(Iplv) = GetNewValue(TableHandle, TabelNr, 1, RowNr, CurrentDate, CurrentTime, &
                                      Idebug, Iout1, DateTimeOutsideTable, .true. )
          Endif
        ENDIF
        IF (iDebug .ne. 0) WRITE(IDEBUG,*) ' IPLV  IDWA  DwaOpt(idwa) DWA', Iplv, Idwa, DwaOpt(Idwa), Dwa(Iplv)
      ENDDO

      ! DWA companies
      DO IPLV = 1,NCPLUV
        IDwa = ReferenceToDwaDef2(iplv)
        if (Idwa .le. 0) goto 101  ! DWA companies is optional (for people it is obliged), so skip if not active (<=0) DWA2=0 by default
        IF (DWAOPT(Idwa) .EQ. 1) THEN
            DWA2(IPLV) = DWAINW2(IPLV) * DWADIS(IDwa,1) * 0.001 / NRSHR
        ELSEIF (DWAOPT(Idwa) .EQ. 2) THEN
            DWA2(IPLV) = DWAINW2(IPLV) * DWADIS(IDwa,2) * 0.001 / nrsHr   &
                                      * 0.01 * DWADIS(IDwa, IHour + 3)
        !+ 3 omdat de eerste 2 elementen uit het array ergens anders voor worden gebruikt !
        ELSEIF (DWAOPT(Idwa) .EQ. 3) THEN
            DWA2(IPLV) = DWADIS(IDwa,1) * 0.001 / NRSHR
        ELSEIF (DWAOPT(Idwa) .EQ. 4) THEN
            DWA2(IPLV) = DWADIS(IDwa,2) * 0.001 / nrsHr  &
                                      * 0.01 * DWADIS(IDwa,IHour + 3)
        !+ 3 omdat de eerste 2 elementen uit het array ergens anders voor worden gebruikt ! en ihour=0 tm 23
        ELSEIF (DWAOPT(Idwa) .EQ. 5) THEN
! August 2001: DWA through a time table
          DWA2(Iplv) = 0.0
          currentDate%year = ConfArr_get_iYear()
          currentDate%month = ConfArr_get_iMonth()
          currentDate%day = ConfArr_get_iDay()
          currentTime%hour = Ihour
          currentTime%minute = ConfArr_get_iMinute()
          currentTime%second = 0
          RowNr = -1
          TabelNr = NwrwDWATable (idwa)
          If (TabelNr .gt. 0) then
              DWA2(Iplv) = GetNewValue(TableHandle, TabelNr, 1, RowNr, CurrentDate, CurrentTime, &
                                      Idebug, Iout1, DateTimeOutsideTable, .true. )
          Endif
        ENDIF
        IF (iDebug .ne. 0) WRITE(IDEBUG,*) ' IPLV  IDWA  DwaOpt(idwa) DWA2', Iplv, Idwa, DwaOpt(Idwa), Dwa2(Iplv)
  101   continue
      ENDDO

      RFRAC = FLOAT(timeSettings%timestepSize) / FLOAT(NRSHR)

      DO IPLV2 = 1,NCPLV2
        VOLOP0 (IPLV2) = VOLOP (IPLV2)
        VOLDY0 (IPLV2) = VOLDYN(IPLV2)
        DO IPTYP= 1,NPTYP
          DO IPOPP= 1,NPOPP
            BVOP0 (IPLV2,IPTYP,IPOPP) = BVOP  (IPLV2,IPTYP,IPOPP)
            NTRRS0(IPLV2,IPTYP,IPOPP) = NTRRST(IPLV2,IPTYP,IPOPP)
!           IF (iDebug .ne. 0) WRITE(IDEBUG,*) ' Init2 DT',IPLV2, IPTYP, IPOPP, RFRAC
            IF (ITMSTP .GT. 1) THEN
              DT(IPLV2,IPTYP,IPOPP,1) = DT(IPLV2,IPTYP,IPOPP,1) + RFRAC
              DT(IPLV2,IPTYP,IPOPP,2) = DT(IPLV2,IPTYP,IPOPP,2) + RFRAC
            ENDIF
          ENDDO
        ENDDO
      ENDDO

! Add special areas Feb 2003; ARS 11173-74
!     NcPlv3 = number of different definitions in use
!     NPlv3  = number of different definitions actually read (maybe only the last ones are in use!!)
      DO IPLV2 = 1,NPLV3
        SpecialVOLOP0 (IPLV2) = SpecialVOLOP (IPLV2)
        SpecialVOLDY0 (IPLV2) = SpecialVOLDYN(IPLV2)
        SpecialBVOP0 (IPLV2) = SpecialBVOP  (IPLV2)
        SpecialNTRRS0(IPLV2) = SpecialNTRRST(IPLV2)
        IF (ITMSTP .GT. 1) THEN
          SpecialDT(IPLV2,1) = SpecialDT(IPLV2,1) + RFRAC
          SpecialDT(IPLV2,2) = SpecialDT(IPLV2,2) + RFRAC
        ENDIF
      ENDDO
! End ARS 11173-74

      QInPluv = 0.
      QPluv%totalPaved      = 0.0
      QPluv%totalUnpaved    = 0.0
      QPluv%totalGreenhouse = 0.0
      QPluv%totalStructure  = 0.0
      QPluv%totalRwzi       = 0.0
      QPluv%totalIndustry   = 0.0
      QPluv%totalSacramento = 0.0
! green roofs
      SpecialNwrwAreaThetaInit = SpecialNwrwAreaThetaFinal
! wadi
      WadiInitialStorage = WadiFinalStorage
      WadiInitialLevel   = WadiFinalLevel
      WadiInflow         = 0.0
      WadiSpillOutflow   = 0.0
      WadiDrainOutflow   = 0.0
      WadiInfiltration   = 0.0
  Return
  END subroutine Init2NWRW


      Subroutine WrInputDataNWRW (Iout9, Iout8, RnDate, RnTime)
        ! *********************************************************************
        ! *** Last update:
        ! *********************************************************************
        ! *** Brief description:
        ! *** ------------------
        ! ***    Stuk uit sub WrData: uitvoer van NWRW nodes in *.Out files
        ! *********************************************************************

        Implicit none

        Integer      INODE, IKIND, INR, IPTyp, IPOpp, i
        Integer      IOUT9, IOUT8
        Integer*2    RNDATE(3), RNTIME(4)
        CHARACTER*35 STRING(3,4)

!                              1         2         3
!                     12345678901234567890123456789012345
      STRING (1,1) = ' With a slope, closed paved'
      STRING (1,2) = ' With a slope, open paved'
      STRING (1,3) = ' With a slope, roofs'
      STRING (1,4) = ' With a slope, unpaved'
      STRING (2,1) = ' Flat, closed paved'
      STRING (2,2) = ' Flat, open paved'
      STRING (2,3) = ' Flat, roofs'
      STRING (2,4) = ' Flat, unpaved'
      STRING (3,1) = ' Stretched flat, closed paved'
      STRING (3,2) = ' Stretched flat, open paved'
      STRING (3,3) = ' Stretched flat, roofs'
      STRING (3,4) = ' Stretched flat, unpaved'


! Pluvius-NWRW inloopmodel; uitvoer in m2 ipv in hectare
      IF (NCPLUV .GT. 0) THEN
         WRITE(IOUT9,17)
   17    FORMAT (//,' Summary input data NWRW nodes          ',//, &
             ' Node identification   Node     Sewer   ', &
             ' 4 Surfaces sloping    4 Surfaces flat        4 Surfaces flat stretched ',/,    &
             '                       name     (0=no )  in m2 per type:',&
             ' closed   paved  , open paved  ,roof, unpaved  ',/, 121('='))
         DO INODE =1,NCNODE
          IKIND = EiNode(INODE,3)
          INR   = EiNode(INODE,2)
          IF (IKIND .EQ. 7) THEN
            WRITE(IOUT9,27) Id_Nod(INODE),&
                            NamNod(INODE),&
                            IRIOOL(INR), &
                           (( AREAPV(INR,IPTYP,IPOPP),IPTYP=1,NPTYP), IPOPP=1,NPOPP)
   27       FORMAT (A20,1X,A15,1X,I2,12(F7.1,1X))
          ENDIF
         ENDDO


         WRITE(IOUT9,171)
  171    FORMAT (//,' Common data NWRW nodes',//,  &
              ' Surface type                  Infiltrationcap ',' Timefactors  inf.cap.',&
              ' Runoff delay   Infiltration from     Max.Storage surface  ',/,&
              '                               min max (mm/uur)',' in/decrease (1/uur)  ', &
              '   (1/minuut)   depressions/runoff         (mm)           ',/,             &
              150('='))
          DO IPTYP=1,NPTYP
            DO IPOPP=1,NPOPP
              WRITE(IOUT9,271) STRING(IPOPP,IPTYP),      &
                              (INFCAP (IPTYP,I),I=1,2), (TFINCP (IPTYP,I),I=1,2), &
                               AFVRTR ((iptyp-1)*npopp + ipopp), INFDEP, INFAF, BMAXOP (IPTYP,IPOPP)
  271          FORMAT (A30,2F8.2,3X,2F8.2,3X,F8.2,5X,2L9,10X,F9.2)
            ENDDO
          ENDDO
      ENDIF


! NWRW nodes
      If (ncpluv .gt. 0 .and. OutputDesired(7) ) then
        WRITE(IOUT8,51) RNDATE(3),RNDATE(2),RNDATE(1),(RNTIME(I),I=1,2), CASENM
   51   FORMAT (' Run datum (DD-MM-YY) and time:',I2,'-',I2,'-',I4,2X,I2,':',I2,//,&
                ' Case name                 ',A20,/)
        WRITE(IOUT8,1013) '[m3/s]'
 1013   FORMAT(//,' Maxima per event',//,& ! PLuvius
                  ' Event   Start     Node identification   Node ',10X,'Maximum_flow',/, &
                  '  nr  year-mon-day', 23X, 'name ',14X,A6,/,70('='))
      Endif

      Return
      END subroutine WrInputDataNWRW


  Subroutine Wr1OutNWRW (Iout8, Ievent, Month, INode, IPlv)
    ! *********************************************************************
    ! *** Last update:
    ! *********************************************************************
    ! *** Brief description:
    ! *** ------------------
    ! ***    Stuk uit sub Wr1Out: uitvoer van NWRW nodes: maxima per event in OUT file
    ! *********************************************************************

      Implicit none
      Integer     Iout8, Ievent, Inode, Iplv
      CHARACTER*3 MONTH(12)
      Integer     Iptyp, ipopp
      Real        RArea, QRin

           if (.not. associated(PLVQOU)) return  ! If there is nothing, do nothing

! Area in m2
           RAREA = 0.
           DO IPTYP=1,NPTYP
             DO IPOPP=1,NPOPP
               RAREA = RAREA + AREAPV(IPLV,IPTYP,IPOPP)
             ENDDO
           ENDDO
! flow in m3
           QRIN = PLVQOU(IPLV,1,IEVENT)
           WRITE(IOUT8,1014) IEVENT, EventStartDateTime(IEVENT,1),MONTH(EventStartDateTime(IEVENT,2)), &
                EventStartDateTime(IEVENT,3),&
                Id_Nod(INODE),NamNod(INODE),QRIN
 1014      FORMAT (I4,1X,I4,1X,A3,1X,I3,3X,A20,1X,A15,1X,F12.3)

  Return
  END subroutine Wr1OutNWRW


  Subroutine NWRW_DeAllocateArrays

    if (Allocated(QPluv)) DeAllocate(QPluv)

  Return
  End subroutine NWRW_DeallocateArrays


end module NWRW
