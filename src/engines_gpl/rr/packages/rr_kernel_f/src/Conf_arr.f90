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
! at:               $Modtime:: 22-07-97 11:53a  $
!
! current revision: $Revision:: 3               $
!

MODULE CONF_ARR

  use Network
  use DH_Alloc

  implicit none

  PRIVATE   ! alles is default private
            ! alleen operaties worden publiek beschikbaar gesteld

  ! public variabelen
  PUBLIC :: nrSMin, nrSHr, nrSDay, mm2m, ha2m, nKind, nVal, nMap, nKaart, nmFl, nmFl2, &
            nmSr, MaxSeriesPerMap, rslMap1_vhg, rslmap2_ovh, rslmap3_kas, &
            rslmap4_ow,  rslmap5_str, rslmap6_bnd, &
            rslmap7_plv, rslmap8_bal, cumrslmap8_bal, rslmap9_slt, &
            rslmap14_rwzi, rslmap15_ind, rslmap16_flows, rslmap17_sacr, rslmap18_cel, rslmap19_RRRunoff, iiNode, &
            Vhg_Tnul, Ovh_Tnul, Kas_Tnul, OW_Tnul, Str_Tnul, Bnd_Tnul, &
            Plv_Tnul, Slt_Tnul, RWZI_Tnul, IND_Tnul, Sacr_Tnul, RRRUnoff_Tnul, &
            MaxTimesteps, MaxDays, MaxTimestepsEvap, &
            MaxTimestepsRunoff, MaxDaysRunoff, &
            MaxTimestepsTemperature, MaxDaysTemperature, &
            nflMap, nlcMap, nsrMap, evStrt, simSeq, owDokW, &
            DownstreamLinkNr, NrDownstreamLinks, NrDownstreamRoutingLinks, NrUpstreamConnections, &
            NrUpstreamGwLinks, NrDownstreamGwLinks, NrUpstreamGwLinksToBeHandled, NrDownstreamGwLinksToBeHandled, SeqGwLink
  PUBLIC    NStartHBV, NStartSCS, NStartNAM, NStartLGSI, NStartWagMod, NStartWalrus
  PUBLIC    MapType
  PUBLIC    xCoor, yCoor, iXfMap, ixlMap
  PUBLIC    SWLinkFromExists
  PUBLIC    UNITS, nmfMap, nmpMap, DirMap, dsrMap, LongDsrMap
  PUBLIC    nrssbk, iTimEf, evDura, KvdLDimensie
  PUBLIC    UrbanRuralConnected

  PUBLIC    SltKwl, SltIni, UseNewTables
  PUBLIC    OutputDesired

  Public    EventStartDateTime, EventDuration
  Public    EventStartDateTimeRunoff, EventDurationRunoff
  Public    EventStartDateTimeTemperature, EventDurationTemperature
  Public    Buidata, Evapdata,  KasInitdata, KasGebruikdata, RunoffData, TemperatureData
  Public    CharIdLength

  Public    EINODE, Upnode, DoNode, EiOW, EiPluv, EiBnd, EiConn, EiBifur, EiRWZI, EiOwSWLink, EiBndSwLink
! PUBLIC    IYEAR, IMO, IDAY, IHOUR, IMIN, ISEC

  Public    ID_NOD, NamNod, id_nod2plvnam, id_nod2bndnam


  ! public operaties
  PUBLIC :: ConfAr0, ConfAr1, ConfAr2, ConfArFixedfiles1, ConfArFixedfiles2, &
            ConfArr_get_eiNode,   ConfArr_set_eiNode,  &
            ConfArr_get_nameNode, ConfArr_set_nameNode, &
            ConfArr_get_idNode,   ConfArr_set_idNode, &
            ConfArr_get_nrSRai,   ConfArr_set_nrSRai, &
            ConfArr_get_nrSEvap,  ConfArr_set_nrSEvap, &
            ConfArr_get_nrSRunoff,ConfArr_set_nrSRunoff, &
            ConfArr_get_nrSTemperature,ConfArr_set_nrSTemperature, &
            ConfArr_get_iYear,    ConfArr_set_iYear,  &
            ConfArr_get_iMonth,   ConfArr_set_iMonth, &
            ConfArr_get_iDay,     ConfArr_set_iDay, &
            ConfArr_get_iHour,    ConfArr_set_iHour, &
            ConfArr_get_iMinute,  ConfArr_set_iMinute, &
            ConfArr_get_iSecond,  ConfArr_set_iSecond, &
            ConfArr_get_upNode,   ConfArr_set_upNode, &
            ConfArr_get_doNode,   ConfArr_set_doNode, &
            ConfArr_get_eiOW,     ConfArr_set_eiOW, &
            ConfArr_get_eiBnd,    ConfArr_set_eiBnd, &
            ConfArr_get_eiOWSWLink,   ConfArr_set_eiOWSWLink, &
            ConfArr_get_eiBndSWLink,  ConfArr_set_eiBndSWLink, &
            ConfArr_get_eiPluv,   ConfArr_set_eiPluv, &
            ConfArr_get_eiConn,   ConfArr_set_eiConn, &
            ConfArr_get_eiBifur,  ConfArr_set_eiBifur, &
            ConfArr_get_eiRwzi,   ConfArr_set_eiRwzi, &
            ConfArr_get_fldVhg,   ConfArr_set_fldVhg, &
            ConfArr_get_fldOvh,   ConfArr_set_fldOvh, &
            ConfArr_get_fldKas,   ConfArr_set_fldKas

  ! *** Configure work arrays module voor Delft_3b
  ! ***
  ! ***  NMAP  = max. aantal Mappix kaarten
  ! ***  NMFL  = max. aantal Mappix files
  ! ***  NMFL2 = helft hiervan
  ! ***  NMSR  = max. aantal Mappix series
  ! ***  NLOC  = max. aantal Mappix lokaties

  ! *** Conversiedata tijd en arealen
  !
  ! *** NRSRAI = aantal seconden in regenval invoerfile
  ! ***           (default: 3600 nl data op uurbasis)
  ! *** NRSEVAP= aantal seconden in verdamping invoerfile
  ! ***           (default: 864000 nl data op dagbasis)
  ! *** NRSRunoff = aantal seconden in runoff invoerfile
  ! *** NRSTemperature = aantal seconden in temperatuur invoerfile
  ! *** NRSSBK = aantal seconden in SOBEK variabele waterstanden/zoutconcentraties
  ! *** NRSMIN = aantal seconden in 1 minuut
  ! *** NRSHR  = aantal seconden in 1 uur
  ! *** NRSDAY = aantal seconden in 1 dag
  ! *** MM2M   = factor van mm naar m        (0.001)
  ! *** HA2M   = factor van hectare naar m2  (10000)
  ! KvdLDimensie = dimensie Krayenhoff vdLeur arrays
  Integer nrSRai, NrSEvap, nrsRunoff, nrSSbk, NrsTemperature
  Integer, parameter :: NRSMIN = 60
  Integer, parameter :: NRSHR  = 3600
  Integer, parameter :: NRSDAY = 86400
  Real   , parameter :: MM2M   = 1/1000.
  Integer, parameter :: HA2M   = 10000
  Integer, parameter :: CharIdLength = 128   !was 256


  INTEGER  NMAP, NMFL, NMFL2, NMSR, nLoc, KvdLDimensie
  Logical  UrbanRuralConnected

  Integer  NStartHBV, NStartSCS, NStartNAM, NStartLGSI, NStartWagMod, NStartWalrus

  ! NKAART = actual number of Maps
  Integer nKaart

  Logical UseNewTables



  ! *** MAPPIX MAP DATA
  ! *** NFLMAP (.)   = NUMBER OF MAPPIX FILES PER MAP
  ! *** NLCMAP (.)   = NUMBER OF USED LOCATIONS IN MAPPIX FILES PER MAP
  ! *** IXLMAP (.,.) = MAPPIX LOCATION INDEX PER NODE AND MAP
  ! *** NSRMAP (.)   = NUMBER OF SERIES PER MAP
  ! *** NMFMAP (.,.) = NAME OF .MPX-FILES PER MAPPIX FILE AND PER MAP
  ! *** DIRMAP (.,.) = Directory NAME OF .MPX-FILES PER MAPPIX FILE AND PER MAP
  ! *** IXFMAP (.,.) = FILE REF.INDEX OF .MPX-FILES PER MAPPIX FILE AND PER MAP
  ! *** NMPMAP (.,.) = NAME OF PARAMETER IN SERIES PER MAPPIX FILE AND PER MAP
  ! *** DSRMAP (.,.) = DESCRIPTION OF SERIES PER SERIES AND MAP
  ! *** LongDsrMAP (.,.) = Long DESCRIPTION OF SERIES PER SERIES AND MAP
  ! *** RSLMAP (.,.,.,.,.) = the simulation result per map, parameter,location and time step

  INTEGER, Pointer, SAVE ::  MaxSeriesPerMap(:), &
                                 NFLMAP(:), &
                                 NSRMAP(:), &
                                 IXFMAP(:,:), &
                                 NLCMAP(:), &
                                 IXLMAP(:,:)
  INTEGER, Pointer, SAVE ::  MapType(:)

  REAL, Pointer, SAVE ::     RSLMAP1_vhg(:,:,:)
  REAL, Pointer, SAVE ::     RSLMAP2_ovh(:,:,:)
  REAL, Pointer, SAVE ::     RSLMAP3_kas(:,:,:)
  REAL, Pointer, SAVE ::     RSLMAP4_ow (:,:,:)
  REAL, Pointer, SAVE ::     RSLMAP5_str(:,:,:)
  REAL, Pointer, SAVE ::     RSLMAP6_bnd(:,:,:)
  REAL, Pointer, SAVE ::     RSLMAP7_plv(:,:,:)
  REAL, Pointer, SAVE ::     RSLMAP8_bal(:,:,:)
  REAL, Pointer, SAVE ::     CUMRSLMAP8_bal(:,:,:)
  REAL, Pointer, SAVE ::     RSLMAP9_slt(:,:,:)
  REAL, Pointer, SAVE ::     RSLMAP14_RWZI(:,:,:)
  REAL, Pointer, SAVE ::     RSLMAP15_ind(:,:,:)
  REAL, Pointer, SAVE ::     RSLMAP16_Flows(:,:,:)
  REAL, Pointer, SAVE ::     RSLMAP17_Sacr (:,:,:)
  REAL, Pointer, SAVE ::     RSLMAP18_Cel  (:,:,:)
  REAL, Pointer, SAVE ::     RSLMAP19_RRRunoff (:,:,:)

  REAL, Pointer, SAVE ::     VHG_Tnul (:,:)
  REAL, Pointer, SAVE ::     OVH_Tnul (:,:)
  REAL, Pointer, SAVE ::     KAS_TNul (:,:)
  REAL, Pointer, SAVE ::     OW_Tnul  (:,:)
  REAL, Pointer, SAVE ::     STR_TNul (:,:)
  REAL, Pointer, SAVE ::     BND_Tnul (:,:)
  REAL, Pointer, SAVE ::     PLV_Tnul (:,:)
  REAL, Pointer, SAVE ::     SLT_Tnul (:,:)
  REAL, Pointer, SAVE ::     RWZI_Tnul(:,:)
  REAL, Pointer, SAVE ::     IND_Tnul (:,:)
  REAL, Pointer, SAVE ::     Sacr_Tnul(:,:)
  REAL, Pointer, SAVE ::     RRRunoff_Tnul (:,:)

  CHARACTER(24) , Pointer, SAVE :: NMFMAP(:,:)
  CHARACTER(Len=CharIdLength), Pointer, SAVE :: NMPMAP(:,:), DIRMAP(:,:), DSRMAP(:,:), UNITS(:,:), LongDSRMAP(:,:)


  !  SLTINI: initiele zoutconcentraties op alle knopen
  !          verondersteld is dat deelbakjes op een knoop dezelfde init.concentratie
  !          dus init. conc. berging op land en berging bodem met zelfde zoutconcentratie
  !  SLTKWL: zoutconcentratie kwel per knoop; alleen gevuld voor onverhard gebied en open water

  Real, Pointer, Save ::      SLTKWL(:), SLTINI(:)


! data from fixed data files
  Integer, Pointer, save ::  EventStartDateTime (:,:), EventDuration(:,:)
  Integer, Pointer, save ::  EventStartDateTimeRunoff (:,:), EventDurationRunoff(:,:)
  Integer, Pointer, save ::  EventStartDateTimeTemperature (:,:), EventDurationTemperature(:,:)

  REAL, Pointer, SAVE ::  Buidata (:,:,:), Evapdata (:,:,:), &
                              KasInitdata (:,:), KasGebruikdata (:,:)
  REAL, Pointer, SAVE ::  Runoffdata (:,:,:)
  REAL, Pointer, SAVE ::  Temperaturedata (:,:,:)



  ! Node data
  ! *** ID_NOD (  ) = knoop id
  ! *** EINODE (,1) = extern nummer
  ! *** EINODE (,2) = volgnummer per type
  ! *** EINODE (,3) = knooptype
  !                              1=verhard gebied
  !                              2=onverhard gebied
  !                              3=kas
  !                              4=open water
  !                              5=structure
  !                              6=boundary
  ! *** SIMSEQ      = simulatieindex knoop
  ! *** SeqGWLink   = simulatieindex gwlinks
  ! *** XCOOR       = xcoordinaat
  ! *** YCOOR       = ycoordinaat
  ! *** NAMNOD      = naam
  ! *** UPNODE      = upstream node
  ! *** DONODE      = downstream node
  ! *** OWDOKW      = logische var, geeft aan of open water
  ! ***               benedenstrooms van een kunstwerk ligt
  ! *** EIOW        = index downstream open water
  !                      (alleen voor verhard/onverhard/kasgebied)
  ! *** EIBND       = index downstream boundary
  ! *** EIOWSWLINK  = index downstream open water via UnpavedSWlink
  !                      (alleen voor verhard/onverhard/kasgebied)
  ! *** EIBNDSWLINK = index downstream boundary via UnpavedSWlink
  ! *** EIPLUV      = index downstream NWRW node
  ! *** EIConn      = index downstream Connection node
  ! *** EIBifur     = index downstream Bifurcation node
  ! *** EIRWZI      = index downstream RWZI
  ! *** eis: voor verhard/onverhard/kasgebied: downstream node= openwater of boundary of (voor verhard) RWZI
  ! ***      voor open water: downstream node= structure
  ! ***      voor structure: downstream node = open water of boundary
  ! *** IINODE: conversie van externe nrs. naar interne nrs

  CHARACTER(Len=CharIdLength), Pointer, SAVE :: ID_NOD(:), NAMNOD(:)
  integer                    , pointer, save :: id_nod2plvnam(:)
  integer                    , pointer, save :: id_nod2bndnam(:)
  REAL, Pointer, SAVE ::  XCOOR(:), YCOOR(:)

  INTEGER, Pointer, SAVE ::  EINODE(:,:), IINODE(:),&
                             UPNODE(:), DONODE(:), &
                             DownstreamLinkNr(:), &
                             NrUpstreamGWLinks(:), &
                             NrUpstreamGWLinksToBeHandled(:), &
                             NrDownstreamGWLinks(:), &
                             NrDownstreamGWLinksToBeHandled(:), &
                             NrDownstreamLinks(:), &
                             NrDownstreamRoutingLinks(:), &
                             NrUpstreamConnections(:), &
                             EIOwSWLink(:), EIBndSWLink(:), &
                             EIOW(:), EIBND(:), EiPluv(:), EIConn(:),EIRWZI(:), EIBifur(:), &
                             SIMSEQ(:), SeqGWLink(:)
  LOGICAL, Pointer, SAVE ::  OWDOKW(:), OutputDesired(:), SWLinkFromExists(:)


  ! Run data
  !
  ! - UNITS  = eenheden
  ! - EVSTRT(1)   :  year
  !         (2)   :  month
  !         (3)   :  day
  !         (4)   :  hour
  !         (5)   :  minute
  !         (6)   :  seconds
  ! - EVDURA(1)   :  event duration in days
  ! -       (2)   :  event duration in hours
  ! -       (3)   :  event duration in minutes
  ! -       (4)   :                 in seconds
  ! - FLDOVH      : flooding onverhard gebied 1=yes,0=no
  ! - FLDVHG      : flooding verhard gebied
  ! - FLDKAS      : flooding kasgebied
  ! - ITIMEF      : nr. of timesteps flooding message in event
  ! - INCHT1/2    : nachtstroom  van uur / tot uur
  ! - IRCHID      : indicator of knopenfile met of zonder reach-id (1=met)


  INTEGER  EVSTRT(6), EVDURA(4)
  INTEGER  MaxTimesteps, MaxDays, MaxTimestepsEvap, MaxTimestepsRunoff, MaxDaysRunoff, MaxTimestepsTemperature, MaxDaysTemperature

  INTEGER  ITIMEF, INCHT1, INCHT2



  LOGICAL    FLDVHG, FLDOVH, FLDKAS

  ! time data

  INTEGER  IYEAR, IMO, IDAY, IHOUR, IMIN, ISEC



contains


  Subroutine CONFAR0
    ! Zet vaste dimensies

    Logical Success

    NKND  =  12      ! was 7; nu plus RWZI en Industry !10=Sacramento  11=Cel 12=RRRunoff
    NKIND =  NKND
    NMAP  =  NKND + 3  !nl. + balans, zout, en linkflows
    NKAART=  NMAP
! map 1 tm 7 knooptypen, 8=balans, 9=zout, dan extra knooptypen; laatste map=takken!

    NVAL  =  6         !   if (NVAL .ne. 6) write(*,*) ' adjust local PeilArrays dimension in OpenWaterModule, UnpavedModule etc'

    NMFL  =  5    ! was 4
    NMFL2 =  2
!   NMSR  = 12    ! was 4; GP Jan 1998 ivm extra uitvoer DWA en 2 riooltypen verhard gebied.
!   NMSR  = 13    ! +1     GP Apr 2000 ivm extra uitvoer paved area: evaporation from surface
!   NMSR  = 15    ! +1     GP Apr/Aug 2001 ivm extra Sacramento output
    NMSR  = 105   ! was 19, voor cel en RRRunoff enorm opgehoogd


    Success = Dh_AllocInit (NMap, OutputDesired, .false. )
    Success = Dh_AllocInit (32, MapType, 0 )
    if (.not. Success)  call ErrMsgStandard (981, 0, ' Error allocating arrays in subroutine ', ' ConfAr0')
!    ALLOCATE ( OutputDesired(NMap), Stat=Allocation_Error )

    ! mapType geeft bij knooptype (1=paved, 7=NWRW, 14=WWTP, 16=Sacr etc) het map nr
    Maptype(1) = 1     ! paved
    Maptype(2) = 2     ! unpaved
    Maptype(3) = 3     ! greenhouse
    Maptype(4) = 4     ! open water
    Maptype(5) = 5     ! RRstructure
    Maptype(6) = 6     ! RR boundary
    Maptype(7) = 7     ! NWRW
    Maptype(8) = 5     ! RRStructure
    Maptype(9) = 5     ! RRStructure
    Maptype(10) = 5    ! RRStructure
    Maptype(11) = 5    ! RRStructure
    Maptype(12) = 5    ! RRStructure
    Maptype(13) = 5    ! RRStructure
    Maptype(14) = 10   ! WWTP
    Maptype(15) = 11   ! Industry
    Maptype(16) = 12   ! Sacramento
    Maptype(17) = 14   ! Cell
    Maptype(18) = 15   ! RR runoff - RR External runoff
    Maptype(19) = 15   ! RR runoff - HBV
    Maptype(20) = 15   ! RR runoff - SCS
    Maptype(21) = 13  ! ow_precip/evap Sobek 3 to link map
    Maptype(22) = 15   ! RR runoff - LGSI
    Maptype(23) = 15   ! RR runoff - Wagmod/Walrus
    Maptype(24) = 0   ! not used
    Maptype(25) = 0
    Maptype(26) = 0
    Maptype(27) = 0
    Maptype(28) = 0
    Maptype(29) = 0
    Maptype(30) = 0    ! RR connection, no output (only via link flows)
    Maptype(31) = 15   ! RR runoff - NAM
    Maptype(32) = 0    ! bifurcation, no output (only via link flows)



  Return
  End subroutine confar0




  SUBROUTINE CONFAR1

   !Vaste dimensies zijn al in Confar0 gezet
!    Integer Allocation_error
    Logical Success

    NLOC = NNOD

   !*** MAP DATA

    Success = Dh_AllocInit (NMap, MaxSeriesPerMap, 0)
    Success = Success .and. Dh_AllocInit (NMap, NFlMap, NSrMap, NLcMap, 0)
    Success = Success .and. Dh_AllocInit (NMap, NMFl, IXFMap,0)
    Success = Success .and. Dh_AllocInit (NMap, 2*NNOD, IXLMap,0)
    If (.not. Success) call ErrMsgStandard (981, 0, ' Error allocating arrays in subroutine ', ' ConfAr1')
! juli 1999: bij toevoegen link flows array IXLMAP opgerekt tot 2*NNOD,
! zodat de links er ook inpassen.


    Success = Success .and. Dh_AllocInit (NMap, NMFL, NMFMAP, ' ')
    Success = Success .and. Dh_AllocInit (NMap, NMFL, NMPMAP, DIRMAP, ' ')
    Success = Success .and. Dh_AllocInit (NMap, NMSR, DSRMAP, ' ')
    Success = Success .and. Dh_AllocInit (NMap, NMSR, LongDSRMAP, ' ')
    Success = Success .and. Dh_AllocInit (NMap, NMSR, UNITS , ' ')
    If (.not. Success) call ErrMsgStandard (981, 0, ' Error allocating arrays in subroutine ', ' ConfAr1')

   !*** NODE DATA

    Success = Success .and. Dh_AllocInit (NNod, 3, EiNode, 0)
    Success = Success .and. Dh_AllocInit (2*NNOD, IINODE, 0)
    Success = Success .and. Dh_AllocInit (NNOD, UPNODE, DONODE, SIMSEQ, 0)
    Success = Success .and. Dh_AllocInit (NNOD, EIOW, EIBND, EIRWZI, 0)
    Success = Success .and. Dh_AllocInit (NNOD, EIOWSwlink, EIBNDSwlink, 0)
    Success = Success .and. Dh_AllocInit (NNOD, EIPluv, 0)
    Success = Success .and. Dh_AllocInit (NNOD, EIConn, 0)
    Success = Success .and. Dh_AllocInit (NNOD, EIBifur, 0)
    Success = Success .and. Dh_AllocInit (NNOD, DownstreamLinkNr, 0)
    Success = Success .and. Dh_AllocInit (NNOD, NrUpstreamGWLinks, 0)
    Success = Success .and. Dh_AllocInit (NNOD, NrUpstreamGWLinksToBeHandled, 0)
    Success = Success .and. Dh_AllocInit (NNOD, NrDownstreamGWLinks, 0)
    Success = Success .and. Dh_AllocInit (NNOD, NrDownstreamGWLinksToBeHandled, 0)
    Success = Success .and. Dh_AllocInit (NNOD, NrDownstreamLinks, 0)
    Success = Success .and. Dh_AllocInit (NNOD, NrDownstreamRoutingLinks, 0)
    Success = Success .and. Dh_AllocInit (NNOD, NrUpstreamConnections,0)
    Success = Success .and. Dh_AllocInit (NNOD, XCOOR, YCOOR, 0E0)
    Success = Success .and. Dh_AllocInit (NNOD, OWDOKW, .false. )
    Success = Success .and. Dh_AllocInit (NNOD, SWLinkFromExists, .false. )
    Success = Success .and. Dh_AllocInit (NNOD, NamNod, Id_Nod, ' ' )
    Success = Success .and. Dh_AllocInit (NNOD, id_nod2plvnam, id_nod2bndnam, 0)
    Success = Success .and. Dh_AllocInit (NNOD, SltKwl, SltIni, 0E0 )
    If (.not. Success) call ErrMsgStandard (981, 0, ' Error allocating arrays in subroutine ', ' ConfAr1')

  !time data: not here

! UrbanRuralConnected: default false
    UrbanRuralConnected = .false.

    RETURN
    END SUBROUTINE CONFAR1



  SUBROUTINE CONFAR2
  ! Zet RSLMAP op kleine afmeting (max. 4 series per file)
  !Overige dimensies zijn al in Confar0 en CONFAR1 gezet
    Logical Success

!  dimensies van RSLMAP:
!   - number of series,
!   - number of locations,
!   - 2: 1 for current timestep t, 2 for current(sum)/average/maximum output at timestep ..
! Adjusted March 1999
! Adjusted October 2001; alle series per knooptype in 1 HIS file

  MaxSeriesPerMap(1) = 17  ! Paved;  March 2004 Taiwan; increased from 16 to 17
  MaxSeriesPerMap(2) = 22  ! 19  ! Unpaved   April 2001: 1 extra (kwel in mm/day)
                           ! 19  ! Unpaved   July 2010 : additional irrigation supply, abstraction from gw
  MaxSeriesPerMap(3) = 5   ! Greenhouse
  MaxSeriesPerMap(4) = 9   ! Openwater;
                           !         filling% =(actpeil-streefpeil)/(maxpeil-streefpeil)
  MaxSeriesPerMap(5) = 4   ! Structures
  MaxSeriesPerMap(6) = 2   ! RR-Boundary  4
  MaxSeriesPerMap(7) = 54  ! NWRW     ! Nov 2009: extended from 11 to 35, to cope with separate outflows from 12 standard areas and 12 special areas
                           ! NWRW     ! Feb 2011: extended to 47 to add storage of special areas
                           ! NWRW     ! Mar 2011: extended to 53 to add wadi results (inflow, infiltration, spill, drain, storage)
                           ! NWRW     ! Jan 2020: extended to 54 to add DWF companies
  MaxSeriesPerMap(8) = 12  ! Balance
  MaxSeriesPerMap(9) = 1   ! Salt    4
  MaxSeriesPerMap(10) = 2  ! WWTP    4
  MaxSeriesPerMap(11) = 4  ! Industry
  MaxSeriesPerMap(12) = 21 ! Sacramento    increased Aug2018 to add more relevant fluxes
  MaxSeriesPerMap(13) = 5  ! Flows
  MaxSeriesPerMap(14) = 55 ! Cel
  MaxSeriesPerMap(15) = 105 ! RRRunoff    was 33, upgrade naar 65 Sept 2014, 72 Mei 2015, 73 June 2015, 79 August 2015, 82 Nov 2015, 83 Dec 2015, 86 Sept2016, 105 Mar2018 Walrus

  success = DH_AllocInit (MaxSeriesPerMap(1), NVHG, 2, RslMap1_vhg, 0E0)
  success = success .and. Dh_AllocInit (MaxSeriesPerMap(1), NVHG, 2, RslMap1_vhg, 0E0)
  success = success .and. DH_AllocInit (MaxSeriesPerMap(2), NOVH, 2, RslMap2_ovh, 0E0)
  success = success .and. DH_AllocInit (MaxSeriesPerMap(3), NKAS, 2, RslMap3_kas, 0E0)
  success = success .and. DH_AllocInit (MaxSeriesPerMap(4), NOW , 2, RslMap4_ow , 0E0)
  If (.not. success) call ErrMsgStandard (981, 0, ' Error allocating arrays in subroutine ', ' ConfAr2')
  success = success .and. DH_AllocInit (MaxSeriesPerMap(5), NSTR, 2, RslMap5_str, 0E0)
  success = success .and. DH_AllocInit (MaxSeriesPerMap(6), NBND, 2, RslMap6_bnd, 0E0)
  success = success .and. DH_AllocInit (MaxSeriesPerMap(7), NPLV, 2, RslMap7_plv, 0E0)
  success = success .and. DH_AllocInit (MaxSeriesPerMap(8), NNOD, 2, RslMap8_bal, CumRslMap8_bal, 0E0)
  If (.not. success) call ErrMsgStandard (981, 0, ' Error allocating arrays in subroutine ', ' ConfAr2')
  IF (ISLCMP .NE. 0) THEN
     success = success .and. DH_AllocInit (MaxSeriesPerMap(9), NNOD, 2, RslMap9_slt, 0E0)
  ELSE
     success = success .and. DH_AllocInit (MaxSeriesPerMap(9), 1   , 2, RslMap9_slt, 0E0)
  ENDIF

  success = success .and. DH_AllocInit (MaxSeriesPerMap(10), NRWZI , 2, RslMap14_rwzi, 0E0)
  success = success .and. DH_AllocInit (MaxSeriesPerMap(11), NINDUS, 2, RslMap15_ind, 0E0)
  If (.not. success) call ErrMsgStandard (981, 0, ' Error allocating arrays in subroutine ', ' ConfAr2')
  success = success .and. DH_AllocInit (MaxSeriesPerMap(13), NLNK  , 2, RslMap16_flows, 0E0)
  success = success .and. DH_AllocInit (MaxSeriesPerMap(12), NSACR , 2, RslMap17_sacr, 0E0)
  success = success .and. DH_AllocInit (MaxSeriesPerMap(14), NCEL  , 2, RslMap18_cel, 0E0)
  success = success .and. DH_AllocInit (MaxSeriesPerMap(15), NRRRunoff , 2, RslMap19_RRRunoff, 0E0)
  If (.not. success) call ErrMsgStandard (981, 0, ' Error allocating arrays in subroutine ', ' ConfAr2')

  success = DH_AllocInit               (MaxSeriesPerMap(1), NVHG, VHG_Tnul   , 0E0)
  success = success .and. DH_AllocInit (MaxSeriesPerMap(2), NOVH, OVH_Tnul   , 0E0)
  success = success .and. DH_AllocInit (MaxSeriesPerMap(3), NKAS, KAS_Tnul   , 0E0)
  success = success .and. DH_AllocInit (MaxSeriesPerMap(4), NOW , OW_Tnul    , 0E0)
  success = success .and. DH_AllocInit (MaxSeriesPerMap(5), NSTR, Str_Tnul   , 0E0)
  success = success .and. DH_AllocInit (MaxSeriesPerMap(6), NBND, BND_Tnul   , 0E0)
  success = success .and. DH_AllocInit (MaxSeriesPerMap(7), NPLV, Plv_Tnul   , 0E0)
  IF (ISLCMP .NE. 0) THEN
     success = success .and. DH_AllocInit (MaxSeriesPerMap(9), NNOD, SLT_Tnul, 0E0)
  ELSE
     success = success .and. DH_AllocInit (MaxSeriesPerMap(9), 1   , SLT_Tnul, 0E0)
  ENDIF

  success = success .and. DH_AllocInit (MaxSeriesPerMap(10), NRWZI , RWZI_Tnul, 0E0)
  success = success .and. DH_AllocInit (MaxSeriesPerMap(11), NINDUS, IND_Tnul , 0E0)
  success = success .and. DH_AllocInit (MaxSeriesPerMap(12), NSACR , SACR_Tnul, 0E0)
  success = success .and. DH_AllocInit (MaxSeriesPerMap(15), NRRRunoff , RRRunoff_Tnul, 0E0)
  If (.not. success) call ErrMsgStandard (981, 0, ' Error allocating arrays in subroutine ', ' ConfAr2')

  Return
  End Subroutine CONFAR2





  Subroutine ConfArFixedFiles1
  ! Configureer first arrays voor Fixed files
    Logical Success



! EventStartDateTime 91:6) read from Rainfall file
  ! -       (1)   :  year
  !         (2)   :  month
  !         (3)   :  day
  !         (4)   :  hour
  !         (5)   :  minute
  !         (6)   :  seconds
! EventDuration (1:4) read from Rainfall file; (5:6) computed
  ! -       (1)   :  event duration in days
  ! -       (2)   :  event duration in hours
  ! -       (3)   :  event duration in minutes
  ! -       (4)   :                 in seconds
  !         (5)   :  max. number of days + 1
  ! -       (6)   :  max. number of rainfall timesteps
  !
!   Allocate ( EventStartDateTime (Nevent,7), Stat=Allocation_Error )
    success = DH_AllocInit (Nevent,7, EventStartDateTime, 0)
    success = DH_AllocInit (NeventRunoff,7, EventStartDateTimeRunoff, 0)
    success = DH_AllocInit (NeventTemperature,7, EventStartDateTimeTemperature, 0)
!   Allocate ( EventDuration(Nevent,6), Stat=Allocation_Error )
    success = success .and. Dh_AllocInit (Nevent,6, EventDuration, 0)
    success = success .and. Dh_AllocInit (NeventRunoff,6, EventDurationRunoff, 0)
    success = success .and. Dh_AllocInit (NeventTemperature,6, EventDurationTemperature, 0)
    If (.not. success) call ErrMsgStandard (981, 0, ' Error allocating arrays in subroutine ', ' ConfArFixedFiles1')

    Return
  End Subroutine ConfArFixedFiles1


  Subroutine ConfArFixedFiles2 (NrStations, NrStationsRunoff, NrStationsTemperature, NcKkl)
  ! Configureer first arrays voor Fixed files
    Integer NrStations, NrStationsRunoff, NrStationsTemperature, NcKkl
    Logical Success

    success = DH_AllocInit (Nevent,NrStations, MaxTimesteps, BuiData, 0E0)
    success = success .and. Dh_AllocInit (Nevent,NrStations, MaxTimestepsEvap, EvapData, 0E0)  !
    success = success .and. Dh_AllocInit (Nevent,NcKkl, KasInitData, 0E0)
    success = success .and. Dh_AllocInit (Nevent,MaxDays, KasGebruikData, 0E0)
    success = success .and. Dh_AllocInit (NeventRunoff,NrStationsRunoff, MaxTimestepsRunoff, RunoffData, 0E0)
    success = success .and. Dh_AllocInit (NeventTemperature,NrStationsTemperature, &
                                             MaxTimestepsTemperature, TemperatureData,0E0)
    If (.not. success) call ErrMsgStandard (981, 0, ' Error allocating arrays in subroutine ', ' ConfArFixedFiles2')

    Return
  End Subroutine ConfArFixedFiles2



  Integer function ConfArr_get_eiNode(iNode, nr)

    Integer iNode, nr

    ConfArr_get_eiNode = eiNode(iNode, nr)
  end function ConfArr_get_eiNode

  subroutine ConfArr_set_eiNode(iNode, nr, waarde)
    Integer iNode, nr, waarde

    eiNode(iNode, nr) = waarde
  end subroutine ConfArr_set_eiNode



  Character(Len=CharIdLength) function ConfArr_get_nameNode(iNode)

    Integer iNode

    ConfArr_get_nameNode = namNod(iNode)
  end function ConfArr_get_nameNode

  subroutine ConfArr_set_nameNode(iNode, name)
    Integer iNode
    Character(Len=CharIdLength) name

    namNod(iNode) = name
  end subroutine ConfArr_set_nameNode



  Character(Len=CharIdLength) function ConfArr_get_idNode(iNode)
    Integer iNode

    ConfArr_get_idNode = ID_Nod(iNode)
  end function ConfArr_get_idNode


  subroutine ConfArr_set_idNode(iNode, name)
    Integer iNode
    Character(Len=CharIdLength) name

    ID_Nod(iNode) = name
  end subroutine ConfArr_set_idNode



  Integer function ConfArr_get_nrSRai()
    ConfArr_get_nrSRai = nrSRai
  end function ConfArr_get_nrSRai

  subroutine ConfArr_set_nrsRai(nrSeconds)
    Integer nrSeconds

    nrsRai = nrSeconds
  end subroutine ConfArr_set_nrsRai

  Integer function ConfArr_get_nrSEvap()
    ConfArr_get_nrSEvap = nrSEvap
  end function ConfArr_get_nrSEvap

  subroutine ConfArr_set_nrsEvap(nrSeconds)
    Integer nrSeconds
    nrsEvap = nrSeconds
  end subroutine ConfArr_set_nrsEvap


  Integer function ConfArr_get_nrSRunoff()
    ConfArr_get_nrSRunoff = nrSRunoff
  end function ConfArr_get_nrSRunoff

  subroutine ConfArr_set_nrsRunoff(nrSeconds)
    Integer nrSeconds

    nrsRunoff = nrSeconds
  end subroutine ConfArr_set_nrsRunoff

  Integer function ConfArr_get_nrSTemperature()
    ConfArr_get_nrSTemperature = nrSTemperature
  end function ConfArr_get_nrSTemperature

  subroutine ConfArr_set_nrsTemperature(nrSeconds)
    Integer nrSeconds

    nrsTemperature = nrSeconds
  end subroutine ConfArr_set_nrsTemperature



  Integer function ConfArr_get_iYear()
    ConfArr_get_iYear = iYear
  end function ConfArr_get_iYear



  subroutine ConfArr_set_iYear(year)
    Integer year

    iYear = year
  end subroutine ConfArr_set_iYear



  Integer function ConfArr_get_iMonth()
    ConfArr_get_iMonth = iMo
  end function ConfArr_get_iMonth

  subroutine ConfArr_set_iMonth(month)
    Integer month
    iMo = month
  end subroutine ConfArr_set_iMonth



  Integer function ConfArr_get_iDay()
    ConfArr_get_iDay = iDay
  end function ConfArr_get_iDay

  subroutine ConfArr_set_iDay(day)
    Integer day
    iDay = day
  end subroutine ConfArr_set_iDay



  Integer function ConfArr_get_iHour()
    ConfArr_get_iHour = iHour
  end function ConfArr_get_iHour

  subroutine ConfArr_set_iHour(hour)
    Integer hour
    iHour = hour
  end subroutine ConfArr_set_iHour



  Integer function ConfArr_get_iMinute()
    ConfArr_get_iMinute = iMin
  end function ConfArr_get_iMinute

  subroutine ConfArr_set_iMinute(minute)
    Integer minute
    iMin = minute
  end subroutine ConfArr_set_iMinute



  Integer function ConfArr_get_iSecond()
    ConfArr_get_iSecond = iSec
  end function ConfArr_get_iSecond

  subroutine ConfArr_set_iSecond(second)
    Integer second
    iSec = second
  end subroutine ConfArr_set_iSecond




  Integer function ConfArr_get_upNode(knoopNr)
    Integer knoopNr

    ConfArr_get_upNode = upNode(knoopNr)
  end function ConfArr_get_upNode



  subroutine ConfArr_set_upNode(knoopNr, upNodeNr)
    Integer knoopNr, upNodeNr

    upNode (knoopNr) = upNodeNr
  end subroutine ConfArr_set_upNode



  Integer function ConfArr_get_doNode(knoopNr)
    Integer knoopNr

    ConfArr_get_doNode = doNode(knoopNr)
  end function ConfArr_get_doNode

  subroutine ConfArr_set_doNode(knoopNr, doNodeNr)
    Integer knoopNr, doNodeNr

    doNode (knoopNr) = doNodeNr
  end subroutine ConfArr_set_doNode



  Integer function ConfArr_get_eiOW(knoopNr)
    Integer knoopNr

    ConfArr_get_eiOW = eiOW(knoopNr)
  end function ConfArr_get_eiOW

  subroutine ConfArr_set_eiOW(knoopNr, owNr)
    Integer knoopNr, owNr

    eiOW(knoopNr) = owNr
  end subroutine ConfArr_set_eiOW


  Integer function ConfArr_get_eiOWSWLink(knoopNr)
    Integer knoopNr

    ConfArr_get_eiOWSWLink = eiOWSWLink(knoopNr)
  end function ConfArr_get_eiOWSWLink


  subroutine ConfArr_set_eiOWSWLink(knoopNr, owNr)
    Integer knoopNr, owNr

    eiOWSWLink(knoopNr) = owNr
  end subroutine ConfArr_set_eiOWSWLink


   Integer function ConfArr_get_eiBnd(knoopNr)
    Integer knoopNr

    ConfArr_get_eiBnd = eiBnd(knoopNr)
  end function ConfArr_get_eiBnd


  subroutine ConfArr_set_eiBnd(knoopNr, bndNr)
    Integer knoopNr, bndNr

    eiBnd(knoopNr) = bndNr
  end subroutine ConfArr_set_eiBnd

   Integer function ConfArr_get_eiBndSWLink(knoopNr)
    Integer knoopNr

    ConfArr_get_eiBndSWLink = eiBndSWLink(knoopNr)
  end function ConfArr_get_eiBndSWLink


  subroutine ConfArr_set_eiBndSwlink(knoopNr, bndNr)
    Integer knoopNr, bndNr

    eiBndSwlink(knoopNr) = bndNr
  end subroutine ConfArr_set_eiBndSwlink

   Integer function ConfArr_get_eiPluv(knoopNr)
    Integer knoopNr

    ConfArr_get_eiPluv = eiPluv(knoopNr)
  end function ConfArr_get_eiPluv

  subroutine ConfArr_set_eiPluv(knoopNr, PluvNr)
    Integer knoopNr, PluvNr

    eiPluv(knoopNr) = PluvNr
  end subroutine ConfArr_set_eiPluv

   Integer function ConfArr_get_eiConn(knoopNr)
    Integer knoopNr

    ConfArr_get_eiConn = eiConn(knoopNr)
  end function ConfArr_get_eiConn

  subroutine ConfArr_set_eiConn(knoopNr, ConnNr)
    Integer knoopNr, ConnNr

    eiConn(knoopNr) = ConnNr
  end subroutine ConfArr_set_eiConn

   Integer function ConfArr_get_eiBifur(knoopNr)
    Integer knoopNr

    ConfArr_get_eiBifur = eiBifur(knoopNr)
  end function ConfArr_get_eiBifur

  subroutine ConfArr_set_eiBifur(knoopNr, BifurNr)
    Integer knoopNr, BifurNr

    eiBifur(knoopNr) = BifurNr
  end subroutine ConfArr_set_eiBifur


   Integer function ConfArr_get_eiRwzi(knoopNr)
    Integer knoopNr

    ConfArr_get_eiRwzi = eiRwzi(knoopNr)
  end function ConfArr_get_eiRwzi


  subroutine ConfArr_set_eiRwzi(knoopNr, RwziNr)
    Integer knoopNr, rwziNr

    eiRwzi(knoopNr) = rwziNr
  end subroutine ConfArr_set_eiRwzi


  Logical function ConfArr_get_fldVhg()
    ConfArr_get_fldVhg = fldVhg
  end function ConfArr_get_fldVhg



  subroutine ConfArr_set_fldVhg(waarde)
    Logical waarde

    fldVhg = waarde
  end subroutine ConfArr_set_fldVhg



  Logical function ConfArr_get_fldOvh()
    ConfArr_get_fldOvh = fldOvh
  end function ConfArr_get_fldOvh



  subroutine ConfArr_set_fldOvh(waarde)
    Logical waarde

    fldOvh = waarde
  end subroutine ConfArr_set_fldOvh



  Logical function ConfArr_get_fldKas()
    ConfArr_get_fldKas = fldKas
  end function ConfArr_get_fldKas



  subroutine ConfArr_set_fldKas(waarde)
    Logical waarde

    fldKas = waarde
  end subroutine ConfArr_set_fldKas



END MODULE CONF_ARR
