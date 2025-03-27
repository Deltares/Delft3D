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
! at:               $Modtime:: 16-12-97 12:44p  $
!
! current revision: $Revision:: 13              $


module Structures

  use Boundary
  use Conf_fil
  use Conf_arr
  use Openwater
  use NewTables
  use DH_alloc
  use ReadLib


  ! variables
   implicit none

  ! *** Data structures
  ! *** MSFACT = maalstopfactor (alleen als maalstop module gebruikt wordt)
  ! *** OnOffMatLab = switch on/off levels trhough MMatlab
  ! ***      (istr,1)= 0 not used, 1=used
  ! ***           ,2)= low on
  ! ***           ,3)= low off
  ! ***           ,4)= high on
  ! ***           ,5)= high off
  ! *** STRTYP = structure type
  ! ***            1=gemaal, 2=stuw (V,recht of Cipoletti)
  ! ***            3=onderlaat, 4=wwerstand (Manning of Chezy)
  ! ***            5=Q-h relatie,
  ! ***            6 =inlaatstuw, 7=inlaatonderlaat, 8=inlaatgemaal
  ! *** STRPAR = structure parameters
  ! ***  TYP=1:  ( ,1) = aanslagpeil tov streefpeil zomer -laag toeren
  ! ***          ( ,2) = afslagpeil tov streefpeil zomer  -laag toeren
  ! ***          ( ,3) = aanslagpeil tov streefpeil winter-laag toeren
  ! ***          ( ,4) = afslagpeil tov streefpeil winter -laag toeren
  ! ***          ( ,5) = eerste toerental capaciteit      -laag toeren
  ! ***          ( ,6) = aanslagpeil tov streefpeil zomer -hoog toeren
  ! ***          ( ,7) = afslagpeil tov streefpeil zomer  -hoog toeren
  ! ***          ( ,8) = aanslagpeil tov streefpeil winter-hoog toeren
  ! ***          ( ,9) = afslagpeil tov streefpeil winter -hoog toeren
  ! ***          ( ,10) = 2e toerental capaciteit         -hoog toeren
  ! ***          ( ,11) = aanslagpeil zomer, nacht        -laag toeren
  ! ***          ( ,12) = afslagpeil zomer, nacht         -laag toeren
  ! ***          ( ,13) = aanslagpeil winter, nacht       -laag toeren
  ! ***          ( ,14) = afslagpeil winter, nacht        -laag toeren
  ! ***          ( ,15) = aanslagpeil zomer, nacht        -hoog toeren
  ! ***          ( ,16) = afslagpeil zomer, nacht         -hoog toeren
  ! ***          ( ,17) = aanslagpeil winter, nacht       -hoog toeren
  ! ***          ( ,18) = afslagpeil winter, nacht        -hoog toeren
  ! ***          ( ,19) = automatische beheer: 0=nee, 5 = time controller
  ! ***          ( ,20) = pump reduction curve 0=nee, 1=ja
  ! ***  TYP=2:  ( ,1) = coefficient C
  ! ***          ( ,2) = breedte     B
  ! ***          ( ,3) = init.stuwinstelling Z
  ! ***          ( ,4) = power coefficient   U
  ! ***          ( ,5) = automatische beheer: 0=nee,
  ! ***                                       1=PID regelaar,
  ! ***                                       2=vast bovenstrooms peil,
  ! ***                                       3=vast peilverschil
  ! ***                                       4=minimum peilverschil
  ! ***                                       5= time controller
  ! ***                                       6= vullingsgraad controller
  ! ***       1  ( ,6) = P coefficient PI regelaar
  ! ***          ( ,7) = I coefficient PI regelaar
  ! ***       2  ( ,6) = max.debiet bij streefpeilregelaar
  ! ***          ( ,7) = max.peil; bij bereiken dit peil mag max.debiet overschred
  ! ***          ( ,8) = max.benedenstrooms peil; als benedenstrooms boven dit pei
  ! ***          ( ,10) = min. crest level  (optioneel)
  ! ***          ( ,14) = max. crest level  (optioneel)
  ! ***       3        = vast peilverschil regelaar: geen extra invoer
  ! ***       4  ( ,6) = min.peilverschil bij min.peilverschilregelaar
  ! ***       6  ( ,6) = max.debiet bij streefpeil
  ! ***          ( ,7) = max.debiet bij max. peil
  ! ***          ( ,10) = min. crest level  (optioneel)
  ! ***          ( ,14) = max. crest level  (optioneel)
  ! ***    allen ( ,9) = retourstroom mogelijk (0=nee)
  ! ***          ( ,10) = optie minimum kruinhoogte
  ! ***          ( ,11) = weir type  (1=default rechthoekig, 2=V-stuw, 3=getrapt)
  ! ***          ( ,12) = slope bij V-stuw
  ! ***                 = 2e crest level bij getrapte stuw
  ! ***          ( ,13) = 2e crest width bij getrapte stuw
  ! ***          ( ,14) = optie minimum kruinhoogte
  ! ***  TYP=3:  ( ,1) = contractiecoefficient n (default 0.63)
  ! ***          ( ,2) = bodembreedte B  (m)
  ! ***          ( ,3) = bodempeil (tov NAP)
  ! ***          ( ,4) = initiele onderlaatstand (tov NAP)
  ! ***          ( ,5) = power coefficient (default 0.5)
  ! ***          ( ,6) = discharge parameter
  ! ***          ( ,7) = retourstroom mogelijk (1=ja, 0=nee)
  ! ***          ( ,8) = automatische beheer: 0=nee, 5=time controller
  ! ***  TYP=4:  ( ,1) = Manning coefficient
  ! ***          ( ,2) = karakteristieke lengte (meters)
  ! ***          ( ,3) = diepte tov bovenstrooms streefpeil ///   bodempeil (tov N
  ! ***          ( ,4) = bodembreedte (m)
  ! ***          ( ,5) = helling talud (0=bak, 1=45 graden)
  ! ***  TYP=5:  ( ,1...6): 6 values Q
  ! ***          ( ,7..12): 6 values h (absollut? of tov streefpeil?)
  ! ***  TYP=6:  ( ,1) = coefficient C
  ! ***          ( ,2) = breedte     B
  ! ***          ( ,3) = init.stuwinstelling Z
  ! ***          ( ,4) = power coefficient   U
  ! ***          ( ,5) = aanslagpeil zomer tov streefpeil ben.str. openwater
  ! ***          ( ,6) = afslagpeil zomer  tov streefpeil ben.str. ow
  ! ***          ( ,7) = aanslagpeil winter tov streefpeil ben.str. ow
  ! ***          ( ,8) = afslagpeil winter  tov streefpeil ben.str. ow
  ! ***          (  9) = continu doorspoeldebiet
  ! ***          ( ,10) = automatische beheer: 0=nee, 1=, 2=, 3=, 4=,  5=time controller 6=vullingsgraad controller
  ! ***          ( ,11) = weir type  (1=default rechthoekig, 2=V-stuw, 3=getrapt)
  ! ***          ( ,12) = slope bij V-stuw
  ! ***                 = 2e crest level bij getrapte stuw
  ! ***          ( ,13) = 2e crest width bij getrapte stuw
  ! ***          ( ,14) = retourstroom mogelijk (0=nee)
  ! ***  TYP=7:  ( ,1) = contractiecoefficient n (default 0.63)
  ! ***          ( ,2) = bodembreedte B  (m)
  ! ***          ( ,3) = bodempeil (tov NAP)
  ! ***          ( ,4) = initiele onderlaatstand (tov NAP)
  ! ***          ( ,5) = power coefficient (default 0.5)
  ! ***          ( ,6) = aanslagpeil zomer tov streefpeil ben.str. openwater
  ! ***          ( ,7) = afslagpeil zomer  tov streefpeil ben.str. ow
  ! ***          ( ,8) = aanslagpeil winter tov streefpeil ben.str. ow
  ! ***          ( ,9) = afslagpeil winter  tov streefpeil ben.str. ow
  ! ***          ( ,10) = continu doorspoeldebiet
  ! ***          ( ,11) = automatische beheer: 0=nee, 5 = time controller
  ! ***          ( ,12) = retourstrooms mogelijk
  !  **  TYP=8:  ( ,1) = als structure type 1, maar dan voor inlaatgemaal
  ! ***          (m 18)  ipv uitlaatgemaal
  ! ***                  (dwz sturen op benedenstrooms peil
  ! ***                              ipv op bovenstrooms peil)
  ! ***          ( ,19) = automatische beheer: 0=nee, 5 = time controller


! RTCHISLOC  = coupling RR structures to RTC His output
! MSFACT     = output from RTC to RR (switch on/off structures)
! OnOffMatlab= output from RTC to RR (switch on/off levels computed with Matlab)
! STRNAM     = geeft bij index istr de knoop index inode
  INTEGER, Pointer, SAVE ::      STRNAM(:)
  INTEGER, Pointer, SAVE ::      STRTYP(:)
  REAL, Pointer, SAVE ::         STRPAR(:,:), MSFACT(:), OnOffMatlab(:,:)
  Integer, Pointer, SAVE ::      RTCHisLoc(:)
  Integer, Pointer, Save ::      StrRefOnOffTable(:), StrRefInitTable(:), StrRefTimeTable(:)
  Integer DrownedWeirDepth, StructureOperation, CFBoundaryConstantInTimestep
  Real flushFlow
  Logical MaxVolChkFrictionBoundary, MaxVolChkWeirBoundary, MaxVolChkOrificeBoundary, SevereVolumeCheck, SimpleVolumeCheck
! April 2002
  Logical FixARSControllerLvlCHeck
  Integer NrVolumeCheck
  Real    VolumeCheckFactorToCF, MinimumDepthCF, VolumeCheckFactorToOW
  Integer StructComp

! addition of V-shape weir reduction factor curve
   Integer MaxNrVShapeWeirReductionPoints
   Integer NrVShapeWeirReductionPoints
   REAL, Pointer, SAVE ::  VShapeWeirH2H1Ratio(:), VShapeWeirFlowReductionFactor(:)

! Pump reduction data
   Integer, Pointer, SAVE ::  StrLastInterpIndex(:)
   REAL, Pointer, SAVE ::  LevelPumpFact(:,:), PumpFact(:,:)
!
! ARS 11610
   Logical FixArs11610Struct

   Real, parameter :: CrestMissingValue = -999.99

!  VolumeCheckFactorToCF is a multiplier on the normal computed volume check for structure discharges to CF; default value=1
!  MinimumDepthCF is a minimum remainging depth in CF; default =0.0
!  VolumeCheckFactorToOW is a multiplier on the normal computed volume check for structure discharges to RR open water, default=1

  ! *** Results structures
  ! ***
  ! *** STRSTA = status structure
  ! *** STRST0 = previous status structure
  ! *** QSTRU  = computed flow over/under structure
  ! *** ActCrestLevel = actual crest level (weir) or opening height (orifice)
  ! *** BottomCrestLevel = lowest crest level (weir)
  ! *** QSTRU1 = computed flow over structure 1
  ! *** QSTRU2 = computed flow over structure 2
  ! *** QSTRU01 = computed flow previous iteration over structure 1
  ! *** QSTRU02 = computed flow previous iteration over structure 2
  ! *** QOWUPR = Remaining outflow over other structurers for upper openwater
  !               previous iteration
  ! *** QOWDWR = Remaining inflow over other structurers for downstream openwater
  !               previous iteration

  !types

  type CapacityStruct
    Real low
    Real high
  end type CapacityStruct

  type ControllerVars
    Integer controlType
    Real Level ! maximum level if ctrlType == 12,
               ! minimum level difference if ctrlType == 14
    Real maxFlow
    Real maxFlowAtMaxLvl
    real maxDownstreamLvl
    real minCrestLevel
    real maxCrestLevel
  end type ControllerVars

  type GeneralWeirVars
    Integer weirType
    Real dischargeCoeff
    Real crestLevel
    Real crestWidth
    Real powerCoeff
    Real slopeV_shape
    Integer backflowPossible
  end type GeneralWeirVars

  type GeneralGateVars
    Real contractionCoeff
    Real crestWidth ! of bottom
    Real crestLevel ! upperside bottom
    Real gateHeight
    Real dischargeCoeff
    Integer backflowPossible
  end type GeneralGateVars

  type ManningVars
    Real coefficient
    Real charactLength
    Real depthUpstream
    Real bottomWidth
    Real slopeTalud
  end type ManningVars

  type QhRelationVars
    Real Q(6)
    Real h(6)
    Integer reference
  end type QhRelationVars

  type dayNightLevels
    Real startlevelHighDay  ! day
    Real stopLevelHighDay
    Real startLevelLowDay
    Real stopLevelLowDay
    Real startlevelHighNight  ! night
    Real stopLevelHighNight
    Real startLevelLowNight
    Real stopLevelLowNight
  end type dayNightLevels

  type SimpleLevels
    Real startlevel
    Real stopLevel
  end type SimpleLevels

  ! variables
  INTEGER, Pointer, SAVE ::      STRSTA(:), STRST0(:), typeControl(:)
  REAL, Pointer, SAVE ::         ActCrestLevel(:), BottomCrestLevel(:), QSTRU(:), &
                                 QSTRU1(:), QSTRU2(:), QOWUPR(:), QOWDWR(:)
  REAL, Pointer, SAVE ::         QSTRU01(:), QSTRU02(:), PumpReductionFactor(:)


  type (dayNightLevels) :: onOffLevels
  type (SimpleLevels) :: simpleOnOffLevels
  type (CapacityStruct), allocatable :: pumpCapacities(:)
  type (ControllerVars) controller
  type (GeneralWeirVars) generalWeir
  type (GeneralGateVars) generalGate
  type (ManningVars) manningR
  type (QhRelationVars) qhRelation

 !Structures output
  ! *** QSTRMX = maximum flow over structure for each event
   REAL, Pointer, SAVE ::    QSTRMX(:,:), ActCrestLevelMx(:,:), QSTRMX1(:,:), QSTRMX2(:,:)


contains
  ! operations


  Subroutine Structures_confAr1

    implicit none

    Integer iOut1, Allocation_Error
    Logical Success

    iOut1 = ConfFil_get_iOut1()

    NSTR = MAX (1, NCSTRU ) !structures
    IF ((NCSTRU .GT. 0) .and. (iOut1 .ne. 0)) then
      WRITE(IOUT1,*) ' Structures            =',NSTR
    end if

   !*** Data structures

    Success = Dh_AllocInit (Nstr, StrTyp, 0)
    Success = success .and. Dh_allocInit (Nstr, StrNam, 0)
    Success = success .and. Dh_AllocInit (Nstr, NVal3, StrPar, 0E0)
    Success = success .and. Dh_AllocInit (Nstr, MsFact, 0E0)
    Success = success .and. Dh_AllocInit (Nstr, 5, OnOffMatLab, 0E0)

    Success = success .and. Dh_AllocInit (NcStru, RtcHisLoc, 0)
    if (.not. success) call ErrMsgStandard (981, 0, ' Error allocating arrays in subroutine ', ' Structure_ConfAr1')

   !*** Results structures

    Success = success .and. Dh_AllocInit (NStr, StrSta, StrSt0, 0)
    Success = success .and. Dh_AllocInit (NStr, QStru, QStru1, QStru2, 0E0)
    Success = success .and. Dh_AllocInit (NStr, QStru01, QStru02, 0E0)
    Success = success .and. Dh_AllocInit (NStr, ActCrestLevel, QOwUpr, QOwDwr, 0E0)
    Success = success .and. Dh_AllocInit (NStr, BottomCrestLevel, CrestMissingValue)
    Success = success .and. Dh_AllocInit (NStr, PumpReductionFactor, 1E0)
    if (.not. success) call ErrMsgStandard (981, 0, ' Error allocating arrays in subroutine ', ' Structure_ConfAr1')

    Success = success .and. Dh_AllocInit (NStr, TypeControl, 0)
    if (.not. success) call ErrMsgStandard (981, 0, ' Error allocating arrays in subroutine ', ' Structure_ConfAr1')

    Allocate(pumpCapacities(nStr), Stat=Allocation_Error )
    If (Allocation_Error .ne. 0) &
       call ErrMsgStandard (981, Allocation_Error, ' Error allocating arrays in subroutine ', &
                                           ' Structure_ConfAr1')
!   Initialisatie
    pumpCapacities%low  = 0.0
    pumpCapacities%high = 0.0
    Success = success .and. Dh_AllocInit (NcStru, StrRefOnOffTable, StrRefInitTable, StrRefTimeTable, 0)
    Success = success .and. Dh_AllocInit (NcStru, StrLastInterpIndex, 1)
    Success = success .and. Dh_AllocInit (6, NcStru, LevelPumpFact, 0.0)
    Success = success .and. Dh_AllocInit (6, NcStru, PumpFact, 1.0)
    if (.not. success) call ErrMsgStandard (981, 0, ' Error allocating arrays in subroutine ', ' Structure_ConfAr1')

  Return
  End subroutine Structures_confAr1



  Subroutine StructureOutput_Confar (Nevnt)
    Integer Nevnt
    Logical Success

! Structure output allocation & initialisation
    Success = Dh_AllocInit (Nstr, Nevnt, QStrMx,QStrMx1, QStrMx2, 0E0)
    Success = success .and. Dh_AllocInit (Nstr, Nevnt, ActCrestLevelMx, CrestMissingValue)
    if (.not. success) call ErrMsgStandard (981, 0, ' Error allocating arrays in subroutine ', ' Structure_OutputConfAr')

    Return
  End subroutine StructureOutput_Confar



  Subroutine Structures_confAr0
    Logical Success

    NVAL3 = 20  ! structures

! additional data for VShape weirs
    MaxNrVShapeWeirReductionPoints=25

    Success = Dh_AllocInit (MaxNrVShapeWeirReductionPoints, VShapeWeirH2H1Ratio, 0E0)
    Success = success .and. Dh_AllocInit (MaxNrVShapeWeirReductionPoints, VShapeWeirFlowReductionFactor, 0E0)
    if (.not. success) call ErrMsgStandard (981, 0, ' Error allocating arrays in subroutine ', ' Structure_ConfAr0')
    Return
  End subroutine Structures_confAr0





  Subroutine Structures_readAsciiInput(infile1, infile2, infile3, infile4)

    IMPLICIT NONE

    Integer :: RetVal

    Integer(4) Infile1, Infile2, Infile3, Infile4
    Integer    inod, index
    Integer    teller, teller2, teller3, istru
    Integer    inletdum, FilStrTyDum, iecode, iout1, idebug, strtydum
    Integer    contdum
    Character(Len=CharIdLength) name, NodeId
    Integer    numberCapacities
    Integer    NrColumns, TableNr, ileft, iright, LocalNrRows
    Logical    Err969

    Character(len=1)     teken
    Character(len=1000)  string
    Integer              IDUM(32)
    REAL                 RDUM(32)
    Character(len=CharIdLength) CDUM(32), TableName
    Logical Success
    Character(Len=CharIdLength)  FileName
    Character(Len=1000000)       KeepBufString
    Integer                      IoUnit, LenString, ipos

    Type Weir
        Integer wt,rt
        Real dc,cl,cw,cl2,cw2,cp,sl,fl
        Character(CharIdLength) ws,so
    End type Weir
    Type (Weir) :: WeirDum

    Type pump
         Integer dn
         Real pc1,pc2
         Character (CharIdLength) so
         integer   pumpreductioncurve
         Real      levels(6), reductionfactors(6)
    End Type pump
    Type (pump) :: PumpDum

    Type Gate
         Real mu,dc,cl,cw,gh, fl
         Integer rt
         Character (CharIdLength) gs, so
    End Type Gate
    Type (Gate) :: GateDum

    Type Manning
         Real mn,cl,dp,bw,ss
    End Type Manning
    Type (Manning) :: ManningDum

    Type QH
       Integer hr
       Real qv(6), hv(6)
    End type QH
    Type (QH) :: QHDUM

    Logical  allow, found, endfil, occurs, TabYesNo, SwlvTable, InstTable
    Real, parameter :: powerCoeff = 0.5

    Character(Len=CharIdLength), Pointer :: STRUDEF(:),CONDEF(:), ConTblDef(:), &
                                  TBLOnOffDEF(:), TblInitDef(:)
    Integer, Pointer :: ReferenceToDefinition(:)
    Logical, Pointer :: AlreadyRead(:)

    Success = Dh_AllocInit (NCStru, StruDef, ConDef, ConTblDef, ' ')
    Success = success .and. DH_AllocInit (NcStru, TblOnOffDef, TblInitDef, ' ')
    Success = success .and. DH_AllocInit (NcStru, ReferenceToDefinition, 0)
    Success = success .and. DH_AllocInit (NcStru, AlreadyRead, .false. )
    if (.not. success) call ErrMsgStandard (981, 0, ' Error allocating arrays in subroutine ', ' Structure_ReadAscii')


    iOut1 = ConfFil_get_iOut1()
    iDebug = ConfFil_get_iDebug()

    allow = .false.
    found = .false.
    SwlvTable = .false.
    InstTable = .false.

! *********************************************************************
! ***  If CleanRRFiles, also write cleaned input RR structures
! *********************************************************************
   if (CleanRRFiles) then
        FileName = ConfFil_get_namFil(52)
        FileName(1:) = Filename(1:Len_trim(FileName)) // '_cleaned'
        Call Openfl (iounit, FileName,1,2)  !struct3b.dat_cleaned
        Call ErrMsgStandard (999, 1, ' Cleaning Struct3b.dat for RR-structure input to file', FileName)
   endif
! *********************************************************************
! Read STRUCT3B.DAT file
! *********************************************************************
   call SetMessage(LEVEL_DEBUG, 'Read Struct3b.Dat file')
   if (idebug .ne. 0) write(idebug,*) ' Read struct3b.Dat file'
   Endfil = .false.
   teller = 0
   RetVal = 0
   Call SKPCOM (INfile1, ENDFIL,'ODS')
   do while (.not. endfil)
      READ(Infile1,'(A1000)',END=21,ERR=150,IOSTAT=IECODE)  STRING
! skip regel als hij niet begint met juist keyword (STRU)
      If (STRING(1:4) .EQ. 'STRU') then
! STRU node id
       RetVal = RetVal + GetVAR2 (STRING,' id ',1,' STRU-ReadAscii',' STRUCT3B.DAT file',IOUT1, &
                     CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
       index = 0
       call fndNd2(index, CDUM(1))
       if (index .gt. 0) then
        inod = index
        index = EiNode(inod,2)
        if (EiNode(inod,3) .eq. 5) then   ! knoop is een kunstwerk
         if (AlreadyRead(index)) then
           call SetMessage(LEVEL_ERROR, 'Data for Structure node '//Cdum(1)(1:Len_trim(Cdum(1)))//' double in datafile Struct.Dat')
         else
! cleaning RR files
          If (CleanRRFiles) write(Iounit,'(A)') String (1:len_trim(String))

          AlreadyRead(index) = .true.
          StrNam(index) = inod
          teller = teller + 1
! structure identification
          RetVal = RetVal + GetVAR2 (STRING,' dd ',1,' stru-ReadAscii',' Struct3b.Dat file',IOUT1, &
                        CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
          STRUDEF(index) = CDUM(1)
! controller active? nb: slechts 1 controller mogelijk!
          RetVal = RetVal + GetVAR2 (STRING,' ca ',3,' stru-ReadAscii',' Struct3b.Dat file',IOUT1, &
                        CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
          If (Abs(IDUM(1)) .eq. 1) THEN
             RetVal = RetVal + GetVAR2 (STRING,' cj ',1,' stru-ReadAscii',' Struct3b.Dat file',IOUT1, &
                           CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
             CONDEF (index) = CDUM(1)
          Endif
          if (idebug .ne. 0) then
            write(idebug,*) ' Read ',string(1:160)
            write(idebug,*) ' teller, index, strudef(index), condef(index)'
            write(idebug,'(2I7,A,1X,A)')  teller, index, &
                                          strudef(index)(1:Len_trim(Strudef(index))), &
                                          condef(index)(1:Len_trim(Condef(index)))
          Endif
         Endif
        Endif
       Endif
      Endif
      Call SKPCOM (INfile1, ENDFIL,'ODS')
    Enddo
 21 Continue
    If (RetVal .gt. 0)  call ErrMsgStandard (972, 0, ' Error reading Struct3B.Dat ', ' Error getting STRU records')
    If (teller .lt. NcStru) then
        Do inod=1,NcNode
          istru = EiNode(inod,2)
          if (EiNode(inod,3) .eq. 5) then   ! en is structure
            if (.not. AlReadyRead(istru)) then
              NodeId = ' '
              NodeId = Id_Nod(inod)
              String = ' '
              String = ' ' // NodeId(1:Len_trim(NodeId)) // ' is missing'
              call ErrMsgStandard (977, 0, ' Data for structure node ',String(1:Len_trim(String)) )
            endif
          endif
        Enddo
        call ErrMsgStandard (972, 0, ' Not enough data for all structures in schematisation found', &
                             ' Some structures not present in Struct3B.Dat file')
    Endif

! cleaning RR files
   If (CleanRRFiles) Call closeGP (Iounit)

! *********************************************************************
! ***  If CleanRRFiles, also write cleaned input for struct.def
! *********************************************************************
   if (CleanRRFiles) then
        FileName = ConfFil_get_namFil(53)
        FileName(1:) = Filename(1:Len_trim(FileName)) // '_cleaned'
        Call Openfl (iounit, FileName,1,2)  !struct3b.def_cleaned
        Call ErrMsgStandard (999, 1, ' Cleaning Struct3b.def for RR-structure input to file', FileName)
   endif
! *********************************************************************
! Read Struct3b.Def file
! *********************************************************************
     Endfil = .false.
     Err969 = .false.
     teller = 0
     call SetMessage(LEVEL_DEBUG, 'Read Struct3b.Def file')
     if (idebug .ne. 0) write(idebug,*) ' Read struct3b.Def file'
     Call SKPCOM (Infile2, ENDFIL,'ODS')
     Do while (.not. endfil)
        READ(Infile2,'(A1000)',END=211,ERR=150,IOSTAT=IECODE)  STRING
! skip regel als hij niet begint met juist keyword (STDS)
        If (STRING(1:4) .EQ. 'STDS')  Then
          Backspace(infile2)
          Success = GetRecord(Infile2, 'STDS', Endfil, idebug, Iout1)
          Success = success .and. GetStringFromBuffer(String)
          if (.not. success) goto 211
! id structure definition
          RetVal = RetVal + GetVAR2 (STRING,' id ',1,' STRU-ReadAscii',' STRUCT3B.Def file',IOUT1, &
                        CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
          name = CDUM(1)
!         Eerst testen of structure definition wel gebruikt wordt
          Istru = FindString (Ncstru, StruDef, Name, Ncstru, CaseSensitive)
          Occurs = (Istru .gt. 0)
          if (Istru .gt. 0) then
             if (ReferencetoDefinition(istru) .gt. 0) then
               call SetMessage(LEVEL_ERROR, 'Structure Definition '//name(1:Len_trim(Name))//' double in datafile Struct.Def')
               Occurs = .false.  ! om verdere verwerking te stoppen
             else
 !             cleaning RR files
               If (CleanRRFiles) write(Iounit,'(A)') String (1:len_trim(String))
             endif
          endif

!         verwerken structure definition
          if (occurs) then
            teller = teller + 1
! FILE structure TYPE (niet hetzelfde als structure type!)
            RetVal = RetVal + GetVAR2 (STRING,' ty ',3,' stru-ReadAscii',' struct3b.def file',IOUT1, &
                          CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
            FILSTRTYDUM = IDUM(1)
! structure parameters
            select case (FILSTRTYDUM)
            case (8)  ! pump or inletpump
              RetVal = RetVal + GetVAR2 (STRING,' in ',3,' stru-ReadAscii',' struct3b.def file',IOUT1, &
                            CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
              inletdum = IDUM(1)
              if (inletdum .eq. 0) then      !normal pump
                  strtydum = 1
              elseif (inletdum .eq.  1) then !inlet pump
                  strtydum = 8
              else
!                 goto 150
!                 Zet default op normal pump, ook als invoer niet klopt
                  strtydum = 1
              endif
              ! data for all pump types
              numberCapacities = 2        ! in toekomst via keyword nc inlezen
              RetVal = RetVal + GetVAR2 (STRING,' dn ',3,' stru-ReadAscii',' struct3b.def file',IOUT1, &
                            CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
              pumpDum%dn = IDUM(1)
              RetVal = RetVal + GetVRS2 (STRING,' pc ',2,' stru-ReadAscii',' struct3b.3b file', &
                            IOUT1, CDUM(1), RDUM(1), IDUM(1), 2, IflRtn)
              pumpDum%pc1 = RDUM(1)
              pumpDum%pc2 = RDUM(2)
              RetVal = RetVal + GetVAR2 (STRING,' so ',1,' stru-ReadAscii',' struct3b.def file',IOUT1, &
                            CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
              pumpDum%so = CDUM(1)
!             March 2005 added pump reduction factor data
              pumpDum%pumpreductioncurve = 0
              do teller3 =1,6
                 pumpDum%levels(teller3) = 0.1
                 pumpDum%reductionfactors(teller3) = 1.0
              enddo
              Allow = .true.     ! rt cr reduction factor data may be missing
              RetVal = RetVal + GetVAR2 (STRING,' rt cr ',3,' stru-ReadAscii',' struct3b.def file',IOUT1, &
                            CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
              if (found) then
                  if (Idum(1) .eq. 0) then
                    RetVal = RetVal + GetVRS2 (STRING,' rt cr ',2,' stru-ReadAscii',' struct3b.3b file', &
                                       IOUT1, CDUM(1), RDUM(1), IDUM(1), 2, IflRtn)
                    do teller3 =1,6
                       pumpDum%levels(teller3) = 0.1
!                      pump reduction factors between 0 and 1
                       pumpDum%reductionfactors(teller3) = max (0.0, Rdum(2))
                       pumpDum%reductionfactors(teller3) = min (1.0, pumpDum%reductionfactors(teller3))
                    enddo
                    pumpDum%pumpreductioncurve = 1  ! fictief, om factor<> 1.0 af te handelen.
                  elseif (Idum(1) .eq. 1) then
                    pumpDum%pumpreductioncurve = 1
                    ! get table data to be added; possible using TBLE tble only occurs once in record
                    ! find nr. rows in table, at most 6, clear the string from < signs and read free format
                    Ileft = FndFrst ('TBLE', String(1:Len_Trim(String)),.false. )
                    Iright= FndFrst ('tble', String(1:Len_Trim(String)),.false. )
                    LocalNrRows = Max (0, CntStr ('<', string(ILeft:IRight)) )
                    LocalNrRows = Min (6, LocalNrRows)
                    Do teller=ileft, iright
                       teken = string(teller:teller)
                       if (teken .eq. '<') string(teller:teller) = ' '
                    Enddo
                    Read (string(ileft+4:),*) (Rdum(teller3),teller3=1,2*LocalNrRows)
                    do teller3=1,6
                       pumpDum%levels(teller3) = Rdum(teller3*2-1)
!                      pump reduction factors between 0 and 1
                       pumpDum%reductionfactors(teller3) = max (0.0, Rdum(teller3*2))
                       pumpDum%reductionfactors(teller3) = min (1.0, pumpDum%reductionfactors(teller3))
                    enddo
                  endif
              endif
              Allow = .false.
            case (9) !weir /inlet weir
              RetVal = RetVal + GetVAR2 (STRING,' in ',3,' stru-ReadAscii',' struct3b.def file',IOUT1, &
                            CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
              inletdum = IDUM(1)
              if (inletdum .eq. 0) then         !normal weir
                 strtydum = 2
              elseif (inletdum .eq. 1) then     !inlet weir
                 strtydum = 6
                 RetVal = RetVal + GetVAR2 (STRING,' fl ',2,' stru-ReadAscii',' struct3b.def file',IOUT1, &
                               CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
                 WeirDum%fl = RDUM (1)
                 RetVal = RetVal + GetVAR2 (STRING,' so ',1,' stru-ReadAscii',' struct3b.def file',IOUT1, &
                               CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
                 WeirDum%so = CDUM (1)
              else
!                 goto 150
!                 Zet default op normal weir, ook als invoer niet klopt
                  strtydum = 2
              end if
              ! data for all weir types:
              WeirDum%cw2 = 0.0
              WeirDum%cl2 = 0.0
              RetVal = RetVal + GetVAR2 (STRING,' wt ',3,' stru-ReadAscii',' struct3b.def file',IOUT1, &
                            CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
              WeirDum%wt = IDUM (1)
              RetVal = RetVal + GetVAR2 (STRING,' dc ',2,' stru-ReadAscii',' struct3b.def file',IOUT1, &
                            CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
              WeirDum%dc = RDUM (1)
              RetVal = RetVal + GetVAR2 (STRING,' cl ',2,' stru-ReadAscii',' struct3b.def file',IOUT1, &
                            CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
              WeirDum%cl = RDUM (1)
              RetVal = RetVal + GetVAR2 (STRING,' cw ',2,' stru-ReadAscii',' struct3b.def file',IOUT1, &
                            CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
              WeirDum%cw = RDUM (1)
              RetVal = RetVal + GetVAR2 (STRING,' cp ',2,' stru-ReadAscii',' struct3b.def file',IOUT1, &
                            CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
              WeirDum%cp = RDUM (1)
! veld sl alleen verplicht voor V shape weir
              WeirDum%sl = 1.0
              Allow = (WeirDum%wt .ne. 2)
              RetVal = RetVal + GetVAR2 (STRING,' sl ',2,' stru-ReadAscii',' struct3b.def file',IOUT1, &
                            CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
              if (found) WeirDum%sl = RDUM (1)
! veld cw2 en cl2 alleen verplicht voor weir type 3: getrapte stuw met 2 crest levels and crest widths
              Allow = (WeirDum%wt .ne. 3)
              RetVal = RetVal + GetVAR2 (STRING,' cl2 ',2,' stru-ReadAscii',' struct3b.def file',IOUT1, &
                            CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
              if (found) WeirDum%cl2 = RDUM (1)
              RetVal = RetVal + GetVAR2 (STRING,' cw2 ',2,' stru-ReadAscii',' struct3b.def file',IOUT1, &
                            CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
              if (found) WeirDum%cw2 = RDUM (1)
! return flow possible?
              Allow = .false.
              RetVal = RetVal + GetVAR2 (STRING,' rt ',3,' stru-ReadAscii',' struct3b.def file',IOUT1, &
                            CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
              WeirDum%rt = IDUM (1)
! ws veld mag ontbreken
              WeirDum%ws = ''
              Found = .false.
              Allow = .true.
              if (idebug .ne. 0) then
                 write(idebug,*) ' Voor GetVar ws ', WeirDum%ws, Cdum(1)
                 write(idebug,*) ' Voor GetVar ws in string  ', string(1:100)
              endif
              RetVal = RetVal + GetVAR2 (STRING,' ws ',1,' stru-ReadAscii',' struct3b.def file',IOUT1, &
                            CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
              if (found .and. Cdum(1) .ne. '' .and. Cdum(1) .ne. ' ') WeirDum%ws = CDUM (1)
              If (WeirDum%ws .ne. '') InstTable = .true.
              if (idebug .ne. 0) then
                 write(idebug,*) ' Na GetVar ws ', WeirDum%ws, Cdum(1)
                 write(idebug,*) ' Na GetVar ws in string  ', string(1:100)
              endif
              Allow = .false.
           case (10) !gate
              RetVal = RetVal + GetVAR2 (STRING,' in ',3,' stru-ReadAscii',' struct3b.def file',IOUT1, &
                            CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
              inletdum = IDUM(1)
              if (inletdum .eq. 0) then         !normal gate
                 strtydum = 3
              elseif (inletdum .eq. 1) then             !inlet gate
                 strtydum = 7
                 RetVal = RetVal + GetVAR2 (STRING,' fl ',2,' stru-ReadAscii',' struct3b.def file',IOUT1, &
                               CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
                 GateDum%fl = RDUM (1)
                 RetVal = RetVal + GetVAR2 (STRING,' so ',1,' stru-ReadAscii',' struct3b.def file',IOUT1, &
                               CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
                 GateDum%so = CDUM (1)
              else
!                 goto 150
!                 Zet default op normal gate, ook als invoer niet klopt
                  strtydum = 3
              endif
              ! data for all gate types:
              RetVal = RetVal + GetVAR2 (STRING,' mu ',2,' stru-ReadAscii',' struct3b.def file',IOUT1, &
                            CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
              GateDum%mu = RDUM (1)
              RetVal = RetVal + GetVAR2 (STRING,' dc ',2,' stru-ReadAscii',' struct3b.def file',IOUT1, &
                            CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
              GateDum%dc = RDUM (1)
              RetVal = RetVal + GetVAR2 (STRING,' cl ',2,' stru-ReadAscii',' struct3b.def file',IOUT1, &
                            CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
              GateDum%cl = RDUM (1)
              RetVal = RetVal + GetVAR2 (STRING,' cw ',2,' stru-ReadAscii',' struct3b.def file',IOUT1, &
                            CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
              GateDum%cw = RDUM (1)
              RetVal = RetVal + GetVAR2 (STRING,' gh ',2,' stru-ReadAscii',' struct3b.def file',IOUT1, &
                            CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
              GateDum%gh = RDUM (1)
              RetVal = RetVal + GetVAR2 (STRING,' rt ',3,' stru-ReadAscii',' struct3b.def file',IOUT1, &
                            CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
              GateDum%rt = IDUM (1)
              GateDum%gs = ''
! gs veld mag ontbreken
              Allow = .true.
              RetVal = RetVal + GetVAR2 (STRING,' gs ',1,' stru-ReadAscii',' struct3b.def file',IOUT1, &
                            CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
              if (found .and. Cdum(1) .ne. '' .and. Cdum(1) .ne. ' ') GateDum%gs = CDUM (1)
              If (GateDum%gs .ne. '') InstTable = .true.
              Allow = .false.
           case (11) !manning
              strtydum = 4
              RetVal = RetVal + GetVAR2 (STRING,' mn ',2,' stru-ReadAscii',' struct3b.def file',IOUT1, &
                            CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
              ManningDum%mn = RDUM (1)
              RetVal = RetVal + GetVAR2 (STRING,' cl ',2,' stru-ReadAscii',' struct3b.def file',IOUT1, &
                            CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
              ManningDum%cl = RDUM (1)
              If (ManningDum%cl .le. 0.001) then
                  write(iout1,'(A,A)') ' Structure Definition - Manning Friction: ',name(1:Len_trim(Name))
                  call ErrMsgStandard (969, 0, ' Manning characteristic length should be > 0', ' Check input data')
                  Err969 = .true.
              Endif
! ARS 3071: dp veld mag ontbreken; zet voor de zekerheid even default=0.1 meter
              ManningDum%dp = 0.1
              Allow = .true.
              RetVal = RetVal + GetVAR2 (STRING,' dp ',2,' stru-ReadAscii',' struct3b.def file',IOUT1, &
                            CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
              if (found) ManningDum%dp = RDUM (1)
              Allow = .false.
! End ARS 3071
              RetVal = RetVal + GetVAR2 (STRING,' bw ',2,' stru-ReadAscii',' struct3b.def file',IOUT1, &
                            CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
              ManningDum%bw = RDUM (1)
              RetVal = RetVal + GetVAR2 (STRING,' ss ',2,' stru-ReadAscii',' struct3b.def file',IOUT1, &
                            CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
              ManningDum%ss = RDUM (1)
          case (12) !QH relation
              strtydum = 5
! veld hr ontbreekt in oude files; dus niet verplicht
              QHDUM%hr = 1
              Allow = .true.
              RetVal = RetVal + GetVAR2 (STRING,' hr ',3,' stru-ReadAscii',' struct3b.def file',IOUT1, &
                            CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
              if (found) QHDUM%hr = IDUM (1)
              Allow = .false.
              RetVal = RetVal + GetVRS2 (STRING,' qv ',2,' stru-ReadAscii',' struct3b.def file', &
                            IOUT1, CDUM(1), RDUM(1), IDUM(1), 6, IflRtn)
              do teller2 = 1,6
                 QHDUM%qv(teller2) = RDUM(teller2)
              Enddo
              RetVal = RetVal + GetVRS2 (STRING,' hv ',2,' stru-ReadAscii',' struct3b.def file', &
                            IOUT1, CDUM(1), RDUM(1), IDUM(1), 6, IflRtn)
              do teller2 = 1,6
                 QHDUM%hv(teller2) = RDUM(teller2)
              Enddo
          end select
          if (idebug .ne. 0) then
             write(idebug,*) ' Weir dum data ', WeirDum%wt, WeirDum%rt, WeirDum%dc, WeirDum%cl, WeirDum%cw
             write(idebug,*) ' Weir dum data ', WeirDum%cp, WeirDum%sl, WeirDum%fl
             write(idebug,*) ' Weir dum data2', WeirDum%cl2, WeirDum%cw2
             write(idebug,*) ' Weir dum data settings', WeirDum%ws, WeirDum%so
             write(idebug,*) ' Gate dum data ', GateDum%mu, GateDum%dc, GateDum%cl, GateDum%cw, GateDum%gh, GateDum%fl
             write(idebug,*) ' Gate dum data ', GateDum%rt, GateDum%gs, GateDum%so
             write(idebug,*) ' Pump dum data ', PumpDum%dn, PumpDum%pc1, PumpDum%pc2, PumpDum%so
             write(idebug,*) ' Manning dum data ', ManningDum%mn,  ManningDum%cl,  ManningDum%dp,  ManningDum%bw
          Endif

          Do teller2 = 1, ncstru
            if (StringComp(StruDef(teller2), Name, CaseSensitive) )  then
               ReferenceToDefinition(teller2)=teller
               strtyp (teller2) = strtydum
               select case (STRTYDUM)
                 case (1)  ! pump
                   TypeControl (teller2) = pumpDum%dn
                   PumpCapacities(teller2)%low = pumpdum%pc1
                   PumpCapacities(teller2)%high = pumpdum%pc2
                   TblOnOffDef(teller2) = Pumpdum%so
                   SwlvTable = .true.
                   Strpar(teller2,5) = PumpCapacities(teller2)%low
                   Strpar(teller2,10) = PumpCapacities(teller2)%high
                   STRPAR(teller2,20) = pumpdum%pumpreductioncurve
                   do teller3 =1,6
                      LevelPumpFact(teller3,teller2) = Pumpdum%Levels(teller3)
                      PumpFact(teller3,teller2) = PumpDum%reductionfactors(teller3)
                   enddo
        ! ON-OFF tabel inlezen!
                 case(8) ! inlet pump
                   TypeControl (teller2) = pumpDum%dn
                   PumpCapacities(teller2)%low = pumpdum%pc1
                   PumpCapacities(teller2)%high = pumpdum%pc2
                   TblOnOffDef(teller2) = Pumpdum%so
                   SwlvTable = .true.
                   Strpar(teller2,5) = PumpCapacities(teller2)%low
                   Strpar(teller2,10) = PumpCapacities(teller2)%high
                   STRPAR(teller2,20) = pumpdum%pumpreductioncurve
                   do teller3 =1,6
                      LevelPumpFact(teller3,teller2) = Pumpdum%Levels(teller3)
                      PumpFact(teller3,teller2) = PumpDum%reductionfactors(teller3)
                   enddo
                 case(2) ! normal weir
                    STRPAR(teller2,1) = Weirdum%dc
                    STRPAR(teller2,2) = Weirdum%cw
                    STRPAR(teller2,3) = Weirdum%cl
                    STRPAR(teller2,4) = Weirdum%cp
                    TblInitDef(teller2) = Weirdum%ws
                    ! adjusted GP sept97
                    select case (Weirdum%rt)
                    case (0) ! both directions
                            STRPAR(teller2, 9) = 1
                            STRPAR(teller2,15) = 1
                    case (1) ! only positive
                            STRPAR(teller2, 9) = 0
                            STRPAR(teller2,15) = 1
                    case (2) ! only negative
                            STRPAR(teller2, 9) = 1
                            STRPAR(teller2,15) = 0
                    case (3) ! no flow
                            STRPAR(teller2, 9) = 0
                            STRPAR(teller2,15) = 0
                    end select !end GP; was altijd strpar(index,9)=1

                    STRPAR(teller2,11) = Weirdum%wt
                    select case (Weirdum%wt)
                    case (1) ! default nothing to do
                    case (2) ! Vshape
                    STRPAR(teller2,12) = Weirdum%sl
                    case (3) ! getrapte stuw
                    STRPAR(teller2,12) = Weirdum%cl2
                    STRPAR(teller2,13) = Weirdum%cw2
                    end select
                 case (6) !inlet weir
                    STRPAR(teller2,1) = Weirdum%dc
                    STRPAR(teller2,2) = Weirdum%cw
                    STRPAR(teller2,3) = Weirdum%cl
                    STRPAR(teller2,4) = Weirdum%cp
                    TblInitDef(teller2) = Weirdum%ws
                    STRPAR(teller2,9) = Weirdum%fl
                    TblOnOffDef(teller2) = Weirdum%so
                    SwlvTable = .true.

                    STRPAR(teller2,11) = Weirdum%wt
                    select case (Weirdum%wt)
                    case (1) ! default nothing to do
                    case (2) ! Vshape
                    STRPAR(teller2,12) = Weirdum%sl
                    case (3) ! getrapte stuw
                    STRPAR(teller2,12) = Weirdum%cl2
                    STRPAR(teller2,13) = Weirdum%cw2
                    end select
                    ! added GP jan 2004
                    select case (Weirdum%rt)
                    case (0) ! both directions
                            STRPAR(teller2,14) = 1
                            STRPAR(teller2,15) = 1
                    case (1) ! only positive
                            STRPAR(teller2,14) = 0
                            STRPAR(teller2,15) = 1
                    case (2) ! only negative
                            STRPAR(teller2,14) = 1
                            STRPAR(teller2,15) = 0
                    case (3) ! no flow
                            STRPAR(teller2,14) = 0
                            STRPAR(teller2,15) = 0
                    end select !end

                 case (3) !gate
                    STRPAR(teller2,1) = GateDum%mu
                    STRPAR(teller2,2) = GateDum%cw
                    STRPAR(teller2,3) = GateDum%cl
                    STRPAR(teller2,4) = Gatedum%gh      ! + GateDum%cl
                    STRPAR(teller2,5) = powercoeff
!ARS 2746
                    STRPAR(teller2,6) = GateDum%dc
!EndARS 2746
                    TblInitDef(teller2) = Gatedum%gs
                    select case (Gatedum%rt)
                    case (0) ! both directions
                            STRPAR(teller2, 7) = 1
                            STRPAR(teller2, 9) = 1
                    case (1) ! only positive
                            STRPAR(teller2, 7) = 0
                            STRPAR(teller2, 9) = 1
                    case (2) ! only negative
                            STRPAR(teller2, 7) = 1
                            STRPAR(teller2, 9) = 0
                    case (3) ! no flow
                            STRPAR(teller2, 7) = 0
                            STRPAR(teller2, 9) = 0
                    end select
                 case (7) !inlet gate
                    STRPAR(teller2,1) = GateDum%mu
                    STRPAR(teller2,2) = GateDum%cw
                    STRPAR(teller2,3) = GateDum%cl
                    STRPAR(teller2,4) = Gatedum%gh      ! + GateDum%cl
                    STRPAR(teller2,5) = powercoeff
!ARS 2746
                    STRPAR(teller2,6) = GateDum%dc
!EndARS 2746
                    TblInitDef(teller2) = Gatedum%gs
                    STRPAR(teller2,10) = GateDum%fl
                    TblOnOffDef(teller2) = Gatedum%so
                    SwlvTable = .true.
                    ! added GP Jan 2004
                    select case (Gatedum%rt)
                    case (0) ! both directions
                            STRPAR(teller2,12) = 1
                            STRPAR(teller2,13) = 1
                    case (1) ! only positive
                            STRPAR(teller2,12) = 0
                            STRPAR(teller2,13) = 1
                    case (2) ! only negative
                            STRPAR(teller2,12) = 1
                            STRPAR(teller2,13) = 0
                    case (3) ! no flow
                            STRPAR(teller2,12) = 0
                            STRPAR(teller2,13) = 0
                    end select
                    ! end addition jan 2004
                 case (4) !manning
                    STRPAR(teller2,1) = ManningDum%mn
                    STRPAR(teller2,2) = ManningDum%cl
                    STRPAR(teller2,3) = ManningDum%dp
                    STRPAR(teller2,4) = ManningDum%bw
                    STRPAR(teller2,5) = ManningDum%ss
                 case (5) !QH relation
                    do teller3 =1,6
                       STRPAR(teller2,teller3) = QHDUM%qv(teller3)
                       STRPAR(teller2,teller3+6) = QHDUM%hv(teller3)
                    enddo
! ARS 8734
                    STRPAR(teller2,13) = QHDUM%hr
               end select
             endif
          Enddo
          if (idebug .ne. 0) then
            write(idebug,*) ' Read ',string(1:160)
            write(idebug,*) filstrtydum, inletdum
          Endif
        Endif
      Endif
      Call SKPCOM (Infile2, ENDFIL,'ODS')
    Enddo
211 continue

    If (RetVal .gt. 0)  call ErrMsgStandard (972, 0, ' Error reading Struct3B.Def ', ' Error getting STDS records')
    If (err969)  call ErrMsgStandard (972, 0, ' Some friction data not correct', &
                                      ' Make sure characteristic lengths at frictions is > 0.0 in STRUCT3B.Def file')
    Err969 = .false.
    Do istru = 1, ncstru
       if (ReferenceToDefinition(istru) .eq. 0) then
           Err969 = .true.
           call ErrMsgStandard (969, 0, ' Structure Definition not present in STRUCT3B.Def file',StruDef(istru))
       Endif
    Enddo
    If (err969)  call ErrMsgStandard (972, 0, ' Not enough Structure data found', &
                                      ' Some Structure Definitions not present in STRUCT3B.Def file')

! cleaning RR files
   If (CleanRRFiles) Call closeGP (Iounit)

! *********************************************************************
! ***  If CleanRRFiles, also write cleaned input for contr3b.def
! *********************************************************************
   if (CleanRRFiles) then
        FileName = ConfFil_get_namFil(54)
        FileName(1:) = Filename(1:Len_trim(FileName)) // '_cleaned'
        Call Openfl (iounit, FileName,1,2)  !contr3b.def_cleaned
        Call ErrMsgStandard (999, 1, ' Cleaning Contr3b.def for RR-structure input to file', FileName)
   endif
! *********************************************************************
! read contr3b.def
! *********************************************************************
    call SetMessage(LEVEL_DEBUG, 'Read Contr3b.Def file')
    if (idebug .ne. 0) write(idebug,*) ' Read Contr3b.Def file'
    teller = 0
    RetVal = 0
    Do istru = 1, ncstru
       ReferenceToDefinition(istru) = 0
    Enddo
    endfil = .false.
    Call SKPCOM (Infile3, ENDFIL,'ODS')
    Do while (.not. endfil)
       READ(Infile3,'(A1000)',END=2111,ERR=150,IOSTAT=IECODE)  STRING
! skip regel als hij niet begint met juist keyword (CNTL)
       If (STRING(1:4) .eq. 'CNTL')  Then
! id control definition
          allow  = .false.
          RetVal = RetVal + GetVAR2 (STRING,' id ',1,' STRU-ReadAscii',' Contr3b.Def file',IOUT1, &
                        CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
          name = CDUM(1)
!         Eerst testen of controller definition wel gebruikt wordt, dan pas verwerken
          Istru = FindString (Ncstru, ConDef, Name, Ncstru, CaseSensitive)
          Occurs = (Istru .gt. 0)
          if (Istru .gt. 0) then
             if (ReferencetoDefinition(istru) .gt. 0) then
               call SetMessage(LEVEL_ERROR, 'Controller Definition '//name(1:Len_trim(Name))//' double in datafile Contr3b.Def')
               Occurs = .false.  ! om verdere verwerking te stoppen
             else
 !             cleaning RR files
               If (CleanRRFiles) write(Iounit,'(A)') String (1:len_trim(String))
             endif
          endif

!         Verwerk controller definitie
          if (occurs) then
            teller = teller + 1
! controller type
            RetVal = RetVal + GetVAR2 (STRING,' ty ',3,' STRU-ReadAscii',' Contr3b.Def file',IOUT1, &
                          CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
            ContDum = IDUM(1)
            controller%controltype = 0
            select case (contdum)
              Case (11) !PID presently not supported
                 controller%controltype = 0         ! zou anders 1 moeten zijn;
              Case (12) !fixed upstream
                 controller%controltype = 2
                 RetVal = RetVal + GetVAR2 (STRING,' mf ',2,' STRU-ReadAscii',' Contr3b.Def file',IOUT1, &
                               CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
                 controller%maxflow = RDUM(1)
                 RetVal = RetVal + GetVAR2 (STRING,' ml ',2,' STRU-ReadAscii',' Contr3b.Def file',IOUT1, &
                               CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
                 controller%level = RDUM(1)
                 RetVal = RetVal + GetVAR2 (STRING,' md ',2,' STRU-ReadAscii',' Contr3b.Def file',IOUT1, &
                               CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
                 controller%maxDownstreamLvl = RDUM(1)
! zmin veld (min. crest level) mag ontbreken
                 controller%minCrestLevel = CrestMissingValue
                 Found = .false.
                 Allow = .true.
                 RetVal = RetVal + GetVAR2 (STRING,' zmin ',2,' STRU-ReadAscii',' Contr3b.Def file',IOUT1, &
                               CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
                 if (found) controller%minCrestLevel = RDUM(1)
                 if (idebug .ne. 0) Write(idebug,*) ' Read zmin MinCrestLevel ', RDUM(1)
! zmax veld (max. crest level) mag ontbreken
                 controller%maxCrestLevel = 9999.99
                 Found = .false.
                 Allow = .true.
                 RetVal = RetVal + GetVAR2 (STRING,' zmax ',2,' STRU-ReadAscii',' Contr3b.Def file',IOUT1, &
                               CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
                 if (found) controller%maxCrestLevel = RDUM(1)
                 if (idebug .ne. 0) Write(idebug,*) ' Read zmax MaxCrestLevel ', RDUM(1)
                 Allow = .false.
              Case (13) !fixed-leveldifference controller
                 controller%controltype = 3
              Case (14) !minimum-leveldifference controller
                 controller%controltype = 4
! ARS xxxx: time controller in RR
              Case (15) !time controller
                 RetVal = RetVal + GetVAR2 (STRING,' tb ',1,' STRU-ReadAscii',' Contr3b.Def file',IOUT1, &
                               CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
                 TableName = CDum(1)
                 controller%controltype = 5
              Case (16) !vullingsgraad controller
                 controller%controltype = 6
                 RetVal = RetVal + GetVAR2 (STRING,' mf ',2,' STRU-ReadAscii',' Contr3b.Def file',IOUT1, &
                               CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
                 controller%maxflow = RDUM(1)
                 RetVal = RetVal + GetVAR2 (STRING,' mf2 ',2,' STRU-ReadAscii',' Contr3b.Def file',IOUT1, &
                               CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
                 controller%MaxFlowAtMaxLvl = RDUM(1)
! zmin veld (min. crest level) mag ontbreken
                 controller%minCrestLevel = CrestMissingValue
                 Found = .false.
                 Allow = .true.
                 RetVal = RetVal + GetVAR2 (STRING,' zmin ',2,' STRU-ReadAscii',' Contr3b.Def file',IOUT1, &
                               CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
                 if (found) controller%minCrestLevel = RDUM(1)
                 if (idebug .ne. 0) Write(idebug,*) ' Read zmin MinCrestLevel ', RDUM(1)
! zmax veld (max. crest level) mag ontbreken
                 controller%maxCrestLevel = 9999.99
                 Found = .false.
                 Allow = .true.
                 RetVal = RetVal + GetVAR2 (STRING,' zmax ',2,' STRU-ReadAscii',' Contr3b.Def file',IOUT1, &
                               CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
                 if (found) controller%maxCrestLevel = RDUM(1)
                 if (idebug .ne. 0) Write(idebug,*) ' Read zmax MaxCrestLevel ', RDUM(1)
                 Allow = .false.
            End select
! Controller parameters
            Do teller2 = 1, ncstru
              if (StringComp(ConDef(teller2), Name, CaseSensitive) )  then
                 ReferenceToDefinition(teller2) = teller
                 select case (STRTYP(teller2))
                    case (1)  ! pump          time controller to be added
                       STRPAR(teller2,19) = controller%controltype
                       if (controller%controltype .eq. 5) then
                           ConTblDef(teller2) = TableName
                           InstTable = .true.
                       else
                           call ErrMsgStandard (969, 0, ' Controller type not yet implemented for this RR structure; data ignored', &
                                                ConDef(teller2))
                       endif
                    case (2)  ! weir
                      STRPAR(teller2,5) = controller%controltype
                      select case (controller%controltype)
                         Case (1) !PID          presently not supported
                         Case (2) !fixed upstream
                              STRPAR(teller2,6) = controller%maxflow
                              STRPAR(teller2,7) = controller%level
                              STRPAR(teller2,8) = controller%maxdownstreamlvl
                              STRPAR(teller2,10) = controller%minCrestLevel
                              STRPAR(teller2,14) = controller%maxCrestLevel
                         Case(3) !fixed-leveldifference controller; do nothing
                         Case(4) ! minimum-lvldiff controller     ; do nothing
                         Case(5) ! time controller
                              ConTblDef(teller2) = TableName
                              InstTable = .true.
                         Case(6) ! vullingsgraad controller
                              STRPAR(teller2,6) = controller%maxflow
                              STRPAR(teller2,7) = controller%maxFlowAtMaxLvl
                              STRPAR(teller2,10) = controller%minCrestLevel
                              STRPAR(teller2,14) = controller%maxCrestLevel
                      End select
                    case (3)  ! gate          only time controller supported
                       STRPAR(teller2, 8) = controller%controlType
                       if (controller%controltype .eq. 5) then
                           ConTblDef(teller2) = TableName
                           InstTable = .true.
                       else
                           call ErrMsgStandard (969, 0, ' Controller type not yet implemented for this RR structure; data ignored', &
                                                ConDef(teller2))
                       endif
                    case (4)  ! Manning       presently not supported
                    case (5)  ! Q-h relation  presently not supported
                    case (6)  ! inlet weir    all controllers or only time controller?
                       STRPAR(teller2,10) = controller%controltype
                       if (controller%controltype .eq. 5) then
                           ConTblDef(teller2) = TableName
                           InstTable = .true.
                       else
                           call ErrMsgStandard (969, 0, ' Controller type not yet implemented for this RR structure; data ignored', &
                                                ConDef(teller2))
                       endif
                    case (7)  ! inlet gate    only time controller
                       STRPAR(teller2,11) = controller%controltype
                       if (controller%controltype .eq. 5) then
                           ConTblDef(teller2) = TableName
                           InstTable = .true.
                       else
                           call ErrMsgStandard (969, 0, ' Controller type not yet implemented for this RR structure; data ignored', &
                                                ConDef(teller2))
                       endif
                    case (8)  ! inlet pump    only time controller
                       STRPAR(teller2,19) = controller%controltype
                       if (controller%controltype .eq. 5) then
                           ConTblDef(teller2) = TableName
                           InstTable = .true.
                       else
                           call ErrMsgStandard (969, 0, ' Controller type not yet implemented for this RR structure; data ignored', &
                                                ConDef(teller2))
                       endif
                 end select
              endif
            Enddo
            if (idebug .ne. 0) then
              write(idebug,*) ' Read ',string(1:160)
              write(idebug,*) ' control type', controller%controltype
              write(idebug,*) ' minCrestLevel',controller%minCrestLevel
              write(idebug,*) ' occurs (Y/N) for structure ?', occurs, istru
            Endif
          Endif
       Endif
       Call SKPCOM (Infile3, ENDFIL,'ODS')
     Enddo
2111 Continue

    If (RetVal .gt. 0)  call ErrMsgStandard (972, 0, ' Error reading Contr3B.Def ', ' Error getting CNTL records')
    Err969 = .false.
    Do istru = 1, ncstru
       if (ReferenceToDefinition(istru) .eq. 0 .and. CONDEF (istru) .ne. '')  Then
           Err969 = .true.
           call ErrMsgStandard (969, 0, ' Controller Definition not present in Contr3B.Def file', ConDef(iSTRU))
       Endif
    Enddo
    If (err969)  call ErrMsgStandard (972, 0, ' Not enough Structure data found', &
                                      ' Some Controller Definitions not present in Contr3B.Def file')

! cleaning RR files
   If (CleanRRFiles) Call closeGP (Iounit)

! *********************************************************************
! ***  If CleanRRFiles, also write cleaned input for struct3b.tbl
! *********************************************************************
   if (CleanRRFiles) then
        FileName = ConfFil_get_namFil(55)
        FileName(1:) = Filename(1:Len_trim(FileName)) // '_cleaned'
        Call Openfl (iounit, FileName,1,2)  !struct3b.tbl_cleaned
        Call ErrMsgStandard (999, 1, ' Cleaning struct3b.tbl for RR-structure input to file', FileName)
   endif

! *********************************************************************
! Read struct3b.tbl: de tabellen met switch on/off levels van kunstwerken (SWLV records)
!                    de tabellen met initial settings van weir/gate       (INST records)
!                    de tabellen met time controller weir/gate/pump       (INST records)
! records kunnen over meerdere regels verspreid staan!!
! *********************************************************************
    if (idebug .ne. 0) write(idebug,*) ' Read Structb.Tbl file'
    Do istru = 1, ncstru
       StrRefOnOffTable(istru) = 0
       StrRefInitTable(istru) = 0
       StrRefTimeTable(istru) = 0
    Enddo

! SWLV records, alleen als SwlvTable = .true.
    endfil = .not. SwlvTable
    if (.not. endfil) call SetMessage(LEVEL_DEBUG, 'Read Structb.Tbl file; SWLV records')
    if (idebug .ne. 0) write(idebug,*) ' SwlvTable =', SwlvTable, Endfil
    Call SKPCOM (Infile4, ENDFIL,'ODS')
    Do while (.not. endfil)
       Success = GetRecord(Infile4, 'SWLV', Endfil, idebug, Iout1) ! get record van keyword SWLV tot swlv, zet in buffer
       If (.not. Success) Goto 3111
       IF (ENDFIL) GOTO 3111
       Success = GetStringFromBuffer (KeepBufString)
       IF (.not. Success .and. CleanRRFiles)   then
           Call ErrMsgStandard (999, 3, ' Local buffer Structuremodule SWLV record too small', ' Input skipped')
           GOTO 3111
       Endif
       Success = GetTableName (TabYesNo, TableName, ' id ', Iout1)     ! get table name via keyword ' id ', TabYesNo=TBLE found
       If (.not. Success) Goto 3111
       If (TabYesNo .and. TableName .ne. '') Then
!         Er is een tabel gedefinieerd, met een niet-lege naam
!         Eerst testen of tabel definition wel gebruikt wordt, dan pas verwerken
          Istru = FindString (Ncstru, TblOnOffDef, TableName, Ncstru, CaseSensitive)
          NrColumns = 0
          Occurs = (Istru .gt. 0)
          if (Istru .gt. 0) then
             if (StrTyp(istru) .eq. 1 .or. StrTyp(istru) .eq. 8) NrColumns = 8   ! pump table
             if (StrTyp(istru) .eq. 6 .or. StrTyp(istru) .eq. 7) NrColumns = 2   ! weir/orifice table
             if (StrRefOnOffTable(istru) .gt. 0) then
                call SetMessage(LEVEL_ERROR, 'Structure On-Off Definition '//Tablename(1:Len_trim(TableName))//' double in datafile Struct3b.Tbl')
                NrColumns = 0     ! om verdere verwerking uit te zetten
             endif
          endif
          if (occurs .and. NrColumns .gt. 0) then
! Get table with name TableName, Nrcolumns data fields, result in global arrays; tabel nummer is TableNr
            Success = GetTable (TableHandle, TableName, NrColumns, TableNr, idebug, Iout1)
            If (.not. Success) Goto 3111
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
 1041          continue
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
            Do istru = 1, ncstru
              if (StringComp(TblOnOffDef(istru), TableName, CaseSensitive) )  StrRefOnOffTable(istru) = TableNr
            Enddo
          Endif
       Endif
       Call SKPCOM (Infile4, ENDFIL,'ODS')
     Enddo
3111 Continue

! INST records, alleen als InstTable = .true., dus of Initial level table of time controller table
    Rewind(infile4)
    endfil = .not. InstTable
    if (.not. endfil) call SetMessage(LEVEL_DEBUG, 'Read Structb.Tbl file; INST records')
    Call SKPCOM (Infile4, ENDFIL,'ODS')
    Do while (.not. endfil)
       Success = GetRecord(Infile4, 'INST', Endfil, idebug, Iout1)    ! get record van keyword INST tot inst, zet in buffer
       If (.not. Success) Goto 4111
       IF (ENDFIL) GOTO 4111
       Success = GetStringFromBuffer (KeepBufString)
       IF (.not. Success .and. CleanRRFiles)   then
           Call ErrMsgStandard (999, 3, ' Local buffer Structuremodule INST record too small', ' Input skipped')
           GOTO 4111
       Endif
       Success = GetTableName (TabYesNo, TableName, ' id ', Iout1)     ! get table name via keyword ' id ', if table defined
       If (.not. Success) Goto 4111
       If (TabYesNo .and. TableName .ne. '') Then
!         Er is een tabel gedefinieerd, met een niet-lege naam
!         Eerst testen of tabel definition wel gebruikt wordt, dan pas verwerken
!         Test of tabel als initial level gebruikt wordt
          Istru = FindString (Ncstru, TblInitDef, TableName, Ncstru, CaseSensitive)
          NrColumns = 1   ! voor initial setting altijd slechts 1 data veld
          Occurs = (Istru .gt. 0)
          if (Istru .gt. 0) then
             if (StrRefInitTable(istru) .gt. 0) then
                call SetMessage(LEVEL_ERROR, 'Structure Initial Definition '//Tablename(1:Len_trim(TableName))//' double in datafile Struct3b.Tbl')
                occurs = .false.  ! om verdere verwerking uit te zetten
             endif
          endif
!         Als niet als Initial table gebruikt, test of tabel als time controller gebruikt wordt
          if (.not. occurs) then
            Istru = FindString (Ncstru, ConTblDef, TableName, Ncstru, CaseSensitive)
            NrColumns = 1   ! voor initial setting altijd slechts 1 data veld
            Occurs = (Istru .gt. 0)
            if (Istru .gt. 0) then
               if (StrRefTimeTable(istru) .gt. 0) then
                  call SetMessage(LEVEL_ERROR, 'Structure TimeController Definition '//Tablename(1:Len_trim(TableName))//' double in datafile Struct3b.Tbl')
                  occurs = .false.  ! om verdere verwerking uit te zetten
               endif
            endif
          endif
!         Verwerk tabel definitie
          if (occurs) then
! Get table with name TableName, Nrcolumns data fields, result in global arrays; tabel nummer is TableNr
            Success = GetTable (TableHandle, TableName, NrColumns, TableNr, Idebug, Iout1)
            If (.not. Success) Goto 4111
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
 1051          continue
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
            Do istru = 1, ncstru
              if (StringComp(TblInitDef(istru), TableName, CaseSensitive) )  StrRefInitTable(istru) = TableNr
              if (StringComp(ConTblDef(istru), TableName, CaseSensitive) )  StrRefTimeTable(istru) = TableNr
            Enddo
          Endif
       Endif
       Call SKPCOM (Infile4, ENDFIL,'ODS')
     Enddo
4111 Continue

! cleaning RR files
   If (CleanRRFiles) Call closeGP (Iounit)

! Check of alle referenties naar tabellen opgelost
    Err969 = .false.
    Do istru = 1, ncstru
       if (StrRefOnOffTable(istru) .eq. 0 .and. TblOnOffDEF (istru) .ne. '') then
         Err969 = .true.
!        write(*,*) ' Structure ', istru, ' refers to table ',TblOnOffDef(istru)
         call ErrMsgStandard (969, 0, ' Some SWLV Table Definitions not present in Struct3B.Tbl file', TblOnOffDef(istru))
       endif
       if (StrRefInitTable(istru) .eq. 0 .and. TblInitDEF (istru) .ne. '')  then
         Err969 = .true.
!        write(*,*) ' Structure ', istru, ' refers to table ',TblInitDef(istru)
         call ErrMsgStandard (969, 0, ' Some INST Table Definitions not present in Struct3B.Tbl file', TblInitDef(istru))
       endif
       if (StrRefTimeTable(istru) .eq. 0 .and. ConTblDEF (istru) .ne. '')  then
         Err969 = .true.
!        write(*,*) ' Structure ', istru, ' refers to table ',TblInitDef(istru)
         call ErrMsgStandard (969, 0, ' Some INST TimeTable Definitions not present in Struct3B.Tbl file', ConTblDef(istru))
       endif
    Enddo
    If (err969)  call ErrMsgStandard (972, 0, ' Not enough Structure data found', &
                                      ' Some Table Definitions not present in Struct3B.Tbl file')

    if (idebug .ne. 0) then
      Write(Idebug,*) ' Structure data '
      Do istru = 1, ncstru
         Write(Idebug,*) ' Structure type and controller', StrTyp(istru), ConDef(Istru), ConTblDef(Istru)
         If (StrTyp(istru) .eq. 1 .or. Strtyp(istru) .eq. 8) then
            Write(Idebug,*) ' Pump data', (StrPar(istru,teller2),teller2=1,19)
            Write(Idebug,*) ' Pump type', StrTyp(istru)
         ElseIf (StrTyp(istru) .eq. 2 .or. Strtyp(istru) .eq. 6) then
            Write(Idebug,*) ' Weir data', (StrPar(istru,teller2),teller2=1,13)
         ElseIf (StrTyp(istru) .eq. 3 .or. Strtyp(istru) .eq. 7) then
            Write(Idebug,*) ' Gate data', (StrPar(istru,teller2),teller2=1,12)
         Endif
      Enddo
    Endif

    Deallocate (TblOnOffDef, TblInitDef, StruDef, ConDef, ConTblDef)
    Deallocate (ReferenceToDefinition)
    Deallocate (AlreadyRead)

    return

    150 CONTINUE
    call SetMessage(LEVEL_FATAL, 'Read error in Structure Readascii')

    return
    end subroutine Structures_readAsciiInput





  SUBROUTINE CMPCNT (ISTR)
    ! *********************************************************************
    ! *** Last update:  March 1995
    ! *********************************************************************
    ! *********************************************************************
    ! *** Brief description:
    ! *** ------------------
    ! ***    This subroutine computes controllers for structures
    ! *********************************************************************
    ! *** Input/output parameters:
    ! *** ------------------------
    ! ***  IEVENT = event number
    ! ***  ITMSTP = timestep number
    ! ***  IDEBUG = file unit number of debug file
    ! ***  IOUT1  = file unit number of output file with run messages
    ! ***  ISTR   = intern structure nr
    ! ***  IMETEO = meteostation code
    ! ***  INODE  = knoopnr
    ! ***  Z      = structure setting
    ! *********************************************************************
    ! *** Berekeningen voor controllers structure
    ! *********************************************************************
    Integer iStr, iDebug

    iDebug = ConfFil_get_iDebug()

    IF (iDebug .ne. 0)  WRITE(IDEBUG,*) 'CMPCNT istr=',ISTR

    ! *********************************************************************
    ! *** Controller
    ! *********************************************************************


    RETURN
  END subroutine cmpcnt



  SUBROUTINE DetermineStartStopLevels (StartLevelLow, StartLevelHigh, StopLevelLow,  StopLevelHigh, &
                                       Tlvl, Hlvl, Hlvl2, Hvol, Hvol2, &
                                       Istr, INode, Iow, IDayWk, Idebug, Imode)
    ! *********************************************************************
    ! *** Last update:  1 September 1999                    by: Geert Prinsen
    ! *********************************************************************
    ! *** Brief description:
    ! *** ------------------
    ! ***    Bepaal pomp aan en afslagpeilen
    ! *********************************************************************
    ! *** Input/output parameters:
    ! *** ------------------------
    ! ***  ISTR   = index kunstwerk
    ! ***  INode  = index knoop
    ! ***  IDAYWK = day of the week (0=zondag, 1=maandag, etc)
    ! ***  IDebug = unit nr debug file
    ! ***  Start/Stop Level Low/High = output start and stop levels
    ! ***  Tlvl   = Target level related open water (upstream or downstream)!!
    ! ***  Hlvl, Hlvl2 = afslagpeilen laag/hoog toeren
    ! ***  Hvol, Hvol2 = afslagvolumina laag/hoog toeren
    ! ***  IOW  = index related open water , upstream or downstream
    ! *********************************************************************

    INTEGER IStr, Inode, Iow, iMode, IDayWk, Idebug
    Integer Rownr, TabelNr, IHour, idum, iout1
    Real startLevelLow, startLevelHigh, stopLevelLow, stopLevelHigh
    Real Tlvl, Hlvl, Hlvl2, Hvol, Hvol2
    CHARACTER(80)  STRING
    logical DateTimeOutsideTable

    Real    PeilArray(6), VolumeArray(6)  ! aanname dat NVAL=6 zoals in Conf_Arr.
    integer i

    type (Date) currentDate
    type (Time) currentTime

    iOut1 = ConfFil_get_iOut1()

     IHour = ConfArr_get_iHour()

     currentDate%year = ConfArr_get_IYear()
     currentDate%month = ConfArr_get_iMonth()
     currentDate%day = ConfArr_get_iDay()
     currentTime%hour = ConfArr_get_iHour()
     currentTime%minute = ConfArr_get_iMinute()
     currentTime%second = 0

! nieuwe methode   ! Volgorde kolommen nu volgens invoerfile, dus afwijkend van oude volgorde!!
     RowNr = -1
     TabelNr = StrRefOnOffTable (iStr)
     onOffLevels%startLevelHighDay = GetNewValue(TableHandle, TabelNr, 3, RowNr, CurrentDate, CurrentTime, &
                                                  Idebug, iout1, DateTimeOutsideTable, .true. )
     onOffLevels%stopLevelHighDay  = GetNewValue(TableHandle, TabelNr, 4, RowNr, CurrentDate, CurrentTime, &
                                                  Idebug, iout1, DateTimeOutsideTable, .true. )
     onOffLevels%startLevelLowDay  = GetNewValue(TableHandle, TabelNr, 1, RowNr, CurrentDate, CurrentTime, &
                                                  Idebug, iout1, DateTimeOutsideTable, .true. )
     onOffLevels%stopLevelLowDay   = GetNewValue(TableHandle, TabelNr, 2, RowNr, CurrentDate, CurrentTime, &
                                                  Idebug, iout1, DateTimeOutsideTable, .true. )
     onOffLevels%startLevelHighNight= GetNewValue(TableHandle, TabelNr, 7, RowNr, CurrentDate, CurrentTime, &
                                                  Idebug,iout1, DateTimeOutsideTable, .true. )
     onOffLevels%stopLevelHighNight = GetNewValue(TableHandle, TabelNr, 8, RowNr, CurrentDate, CurrentTime, &
                                                  Idebug,iout1, DateTimeOutsideTable, .true. )
     onOffLevels%startLevelLowNight = GetNewValue(TableHandle, TabelNr, 5, RowNr, CurrentDate, CurrentTime, &
                                                  Idebug,iout1, DateTimeOutsideTable, .true. )
     onOffLevels%stopLevelLowNight  = GetNewValue(TableHandle, TabelNr, 6, RowNr, CurrentDate, CurrentTime, &
                                                  Idebug,iout1, DateTimeOutsideTable, .true. )

     if (idebug .ne. 0) then
        write(idebug,*) ' New Method OnOffLevels',                            &
           onOffLevels%startLevelHighDay  , onOffLevels%stopLevelHighDay   ,  &
           onOffLevels%startLevelLowDay   , onOffLevels%stopLevelLowDay    ,  &
           onOffLevels%startLevelHighNight, onOffLevels%stopLevelHighNight ,  &
           onOffLevels%startLevelLowNight , onOffLevels%stopLevelLowNight
     Endif


! September 2000 ARS 5852
! Get Matlab switch on/off levels, convert to levels relative to target level
     If (OnOffMatlab(Istr,1) .gt. 0) then
       onOffLevels%startLevelLowDay  = OnOffMatlab(Istr,2) - Tlvl
       onOffLevels%stopLevelLowDay   = OnOffMatlab(Istr,3) - Tlvl
       onOffLevels%startLevelHighDay = OnOffMatlab(Istr,4) - Tlvl
       onOffLevels%stopLevelHighDay  = OnOffMatlab(Istr,5) - Tlvl
! geen verschil dag en nacht, doorgegeven Matlab peilen gelden voor huidige tijdstap
       onOffLevels%startLevelHighNight= onOffLevels%startLevelHighDay
       onOffLevels%stopLevelHighNight = onOffLevels%stopLevelHighDay
       onOffLevels%startLevelLowNight = onOffLevels%startLevelLowDay
       onOffLevels%stopLevelLowNight  = onOffLevels%stopLevelLowDay
       if (idebug .ne. 0) then
          write(idebug,*) ' Matlab levels ',(OnOffMatlab(Istr,idum),idum=2,5)
          write(idebug,*) ' target level  ',Tlvl
          write(idebug,*) ' After Matlab Measures ',                            &
             onOffLevels%startLevelHighDay  , onOffLevels%stopLevelHighDay   ,  &
             onOffLevels%startLevelLowDay   , onOffLevels%stopLevelLowDay    ,  &
             onOffLevels%startLevelHighNight, onOffLevels%stopLevelHighNight ,  &
             onOffLevels%startLevelLowNight , onOffLevels%stopLevelLowNight
       Endif
     Endif
! End Get Matlab switch on/off levels


     IF ((timeSettings%weekendHrsAsNight .AND. (IDAYWK .EQ. 0 .OR. IDAYWK .EQ. 6)) .OR. &
                                        LNACHT(IHOUR) )  THEN
       startLevelLow = onOffLevels%startLevelLowNight
       startLevelHigh = onOffLevels%startLevelHighNight
       stopLevelLow = onOffLevels%stopLevelLowNight
       stopLevelHigh = onOffLevels%stopLevelHighNight
     else
       startLevelLow = onOffLevels%startLevelLowDay
       startLevelHigh = onOffLevels%startLevelHighDay
       stopLevelLow = onOffLevels%stopLevelLowDay
       stopLevelHigh = onOffLevels%stopLevelHighDay
     endif

   !Jan 98: correct start/stoplevels high in case no high pump capacities
     if (pumpCapacities(iStr)%high .le. 0.0) then
       IF (iDebug .ne. 0)  WRITE(IDEBUG,*) ' Correction of high start/stop levels because capacity=0'
       startLevelHigh = startLevelLow
       stopLevelHigh  = stopLevelLow
    endif


    ! hlvl =afslagpeil laagtoeren, hlvl2=afslagpeil hoogtoeren
      hLvl  = tLvl + stopLevelLow
      hLvl2 = tlvl + stopLevelHigh


      If (iDebug .ne. 0) Then
        WRITE(IDEBUG,*) ' Day of week (0 = zondag)', iDayWk
        WRITE(IDEBUG,*) ' iHour, weekend', iHour, timeSettings%weekendHrsAsNight
        WRITE(IDEBUG,*) ' Pumpstation startlevels', startLevelLow, startLevelHigh
        WRITE(IDEBUG,*) ' Pumpstation stoplevels', HLVL, HLVL2
      Endif

      If (imode .eq. 1) then      ! uitlaat: afslagpeil hoogtoeren boven laagtoeren afslagpeil
         Hlvl2 = max (Hlvl, Hlvl2)
      elseif (imode .eq. 2) then  ! inlaatpomp: hoogtoeren afslagpeil onder laagtoeren afslagpeil
         Hlvl2 = min (Hlvl, Hlvl2)
      Endif
      IF (iDebug .ne. 0)  WRITE(IDEBUG,*) ' Corrected Pumpstation stop-levels ', HLVL, HLVL2

      Do i=1,NVal
         PeilArray(i) = PeilOw(i,iow)
         VolumeArray(i) = VoluOw(i,iow)
      Enddo

      Call RR_INTERP (NVAL, PeilArray, VolumeArray, HLVL,  HVOL, OwLastInterpIndex(iow))
      Call RR_INTERP (NVAL, PeilArray, VolumeArray, HLVL2,  HVOL2, OwLastInterpIndex(iow))

    !check stop volumes: if negative then error (below bottom of open water)
      if (HVOL .LT. 0 .OR. HVOL2 .LT. 0) then
         STRING(1:1)  = ' '
         STRING(2:33) = Id_Nod(INODE)
         call ErrMsgStandard (938, EiNode(INODE,1), ' CMPSTR', STRING(1:33))
      endif

    RETURN
  END subroutine DetermineStartStopLevels


  SUBROUTINE CheckTimeControllerPump (Istr, QCapController, Idebug, Iout1)

! Subroutine voor limiteren van pomp capaciteit volgens time controller
! Input: structure nr, debugfile and output unit
! Output: capacity from time controller

    INTEGER IStr
    Real    QCapController
    Integer Iout1, Idebug
    Integer Rownr, TabelNr
    logical DateTimeOutsideTable

    type (Date) currentDate
    type (Time) currentTime

! intialise QCapController to big number
    QCapController = 99999999.

    ! Check for Time controller for pump capacity
    IF (STRPAR(ISTR,19) .eq. 5.0) THEN
       currentDate%year  = ConfArr_get_iYear()
       currentDate%month = ConfArr_get_iMonth()
       currentDate%day   = ConfArr_get_iDay()
       currentTime%hour  = ConfArr_get_iHour()
       currentTime%minute = ConfArr_get_iMinute()
       currentTime%second = 0
       RowNr = -1
       TabelNr = StrRefTimeTable (iStr)
       If (TabelNr .gt. 0) then
         QCAPController = GetNewValue(TableHandle, TabelNr, 1, RowNr, CurrentDate, CurrentTime, &
                                      Idebug, Iout1, DateTimeOutsideTable, .true. )
       Endif
    Endif
    if (idebug .ne. 0) Write(Idebug,*) ' CheckTimeControllerPump QCapController=',QCapController

    RETURN
  END subroutine CheckTimeControllerPump


  Subroutine CheckPumpReductionFactor (Istr, ReductionFactor, Idebug, Iout1, UpstreamLevel, DownstreamLevel)

! Subroutine voor bepalen pump reductiecapaciteit
! Input: structure nr, debugfile and output unit
! Output: capacity from time controller

    INTEGER IStr
    Real    ReductionFactor, TempLevel, UpstreamLevel, DownstreamLevel
    Integer Iout1, Idebug

    integer i
    Real    LevelPumpFactor(6), PumpFactor (6)

!    type (Date) currentDate
!    type (Time) currentTime

! intialise ReductionFactor
    ReductionFactor = 1.0

    Do i=1, 6
       LevelPumpFactor(i) = LevelPumpFact(i,istr)
       PumpFactor (i)     = PumpFact(i,istr)
    Enddo

    ! Check for Pump reduction curve  (incl. specified constant value in the struct3b.def file)
    ! If not specified in file, the factor is default equal to 1
    IF (STRPAR(ISTR,20) .eq. 1) THEN
       TempLevel = UpstreamLevel - DownstreamLevel
       Call RR_INTERP (NVAL, LevelPumpFactor, Pumpfactor, TempLevel, ReductionFactor, StrLastInterpIndex(istr))
    Endif
    if (idebug .ne. 0) Write(Idebug,*) ' CheckPumpReductionFactor =',ReductionFactor
    PumpReductionFactor(istr) = ReductionFactor

    RETURN
  END subroutine CheckPumpReductionFactor



  SUBROUTINE CMPSTR (ITMSTP, ISTR, INODE, IDAYWK)
    ! *********************************************************************
    ! *** Last update:  March 1995
    ! *********************************************************************
    ! *********************************************************************
    ! *** Brief description:
    ! *** ------------------
    ! ***    This subroutine performs computations for structures
    ! *********************************************************************
    ! *** Input/output parameters:
    ! *** ------------------------
    ! ***  ITMSTP = timestep number
    ! ***  IDEBUG = file unit number of debug file
    ! ***  IOUT1  = file unit number of output file with run messages
    ! ***  ISTR   = intern structure nr
    ! ***  IMETEO = meteostation code
    ! ***  INODE  = knoopnr
    ! ***  IDAYWK = day of the week (0=zondag, 1=maandag, etc)
    ! *********************************************************************
    ! *** Berekeningen voor structures
    ! *********************************************************************

IMPLICIT NONE

    REAL          ARR1(NVAL), ARR2(NVAL), QCAP, QOUT, QCapController
    INTEGER       NTIML, NTIML2, NTiml3, NTIMLC, NTimLc_Old, iDayWk, iTmStp
    INTEGER       ISTR, IOW, IOWD, NODOWN, NODEUP, IBND, IBNDUP, IDUM
    Integer       ITL, iNode
    Integer       iDebug, IOut1, iHour
!    CHARACTER(80)  STRING

    PARAMETER (NTIML2 = 60)
    REAL      QTEMP(NTIML2)

    Real doLvl, doLv0, doVol0, tqInDw, tLvlDo, doVol, upLvl, vol0, tqIn, DoVolInit
    Real tLvl, upLv0, dLvl, dlvl0, hLvl, hLvl2, hVol, hVol2, tMax
    Real volNu, volTgt, volMax, qDif, volTmp, upLvT, doLvT
    Real Delta_volume, UpArea, DoArea, FractionTime
    Real upLvT0, doLvT0, rdummy
    Real startLevelLow, startLevelHigh, stopLevelLow, stopLevelHigh
    Real startLevel, stopLevel, peilBottomDown, peilBottomUp
    Real UpMax, DoMin, ZMax
!   vullingsgraad controller
    Real VullingUpstream, VullingDownstream, TempVullingUp, TempVullingDo
    Real QMax, DeltaVMax, TempVolUp, TempLvlUp, TempVolDo, TempLvlDo
    Real DoTMax, DoTlvl, DoVolNu, DoVolTgt, DoVolMax
    Real UpLvlIncFlows, DoLvlIncFlows, VolNuIncFlows, DoVolNuIncFlows
    Real VullingUpstreamIncFlows, VullingDownstreamIncFlows
!   end vullingsgraad controller
    Real TmpCrestLevel
    Real ReductionFactor

    Real, parameter :: G = 9.81
    Integer K
    logical DateTimeOutsideTable

    Real    PeilArrayUp(6), AreaArrayUp(6), VolumeArrayUp(6)  ! aanname dat NVAL=6 zoals in Conf_Arr.
    Real    PeilArrayDown(6), AreaArrayDown(6), VolumeArrayDown(6)  ! aanname dat NVAL=6 zoals in Conf_Arr.
    integer i

    Integer Rownr, TabelNr

    type (Date) currentDate
    type (Time) currentTime

    iOut1  = ConfFil_get_iOut1 ()
    iDebug = ConfFil_get_iDebug()
    iHour = ConfArr_get_iHour()

    IF (iDebug .ne. 0)  WRITE(IDEBUG,*) 'CMPSTR istr=',ISTR
    NTIML = MIN (NVAL2, NTIML2, 10)
    IF (timeSettings%timestepSize/NTIML .LT. NRSMIN)  NTIML = timeSettings%timestepSize / NRSMIN

    ! *********************************************************************
    ! *** Bepaal downstream level (open water of boundary)
    ! *** Bepaal upstream open water level
    ! *********************************************************************
    ! ***  *** nog tijdmiddeling inbouwen?
    ! *********************************************************************

    IOW    = 0
    IOWD   = 0
    IBND   = 0
    IBNDUP = 0
    NODOWN = DONODE(INODE)

    select case (EiNode(NoDown, 3))
      case (4)   ! downstream node is open water
             IOWD  = EiNode(NODOWN,2)
             DOLVL = (1.0 - timesettings%timeWeightFactor) * LVLOW0(IOWD) + &
                      timesettings%timeWeightFactor * LVLOW(IOWD)
             DOLV0 = LVLOW0(IOWD)
             Do i=1,NVal
                PeilArrayDown(i)   = PeilOw(i,iowd)
                AreaArrayDown(i)   = AreaOw(i,iowd)
                VolumeArrayDown(i) = VoluOw(i,iowd)
             Enddo
             Call RR_INTERP (NVAL, PeilArrayDown, VolumeArrayDown, DOLV0, DOVOL0, OwLastInterpIndex(iowd))
             Call RR_INTERP (NVAL, PeilArrayDown, AreaArrayDown, DOLV0, DoArea, OwLastInterpIndex(iowd))
             DOVOL = DOVOL0
           ! bepaal inkomend debiet TQINDW, excl. via kunstwerken
             TQINDW = QINOW(IOWD,1) + QINOW(IOWD,3) + QinOw(Iowd,7) + QIN0(IOWD,2) + Qin0 (IOWD,5) + Qin0(IOWD,6)
           ! bepaal downstream target level (alleen gebruikt bij inlaatkunstwerken)
             TlvlDo = CurrentTargetLevel (Iowd)  !Call DTTLVL (TLVLDO, IOWD)
      case (6)   ! downststream node is boundary -
             IBND  = EiNode(NODOWN,2)
             DOLV0 = BNDPAR(IBND,1)
             DOLVL = BNDPAR(IBND,1)
             DOAREA = BNDPAR(IBND,5)
    end select

    NODEUP = UPNODE(INODE)
    select case (EiNode(NodeUp, 3))
        case (4)   ! upstream node is open water, dus uitlaatkunstwerk
                 IOW    = EiNode(NODEUP,2)
                 UPLV0  = LVLOW0(IOW)
                 UPLVL  = LVLOW (IOW)
                 Do i=1,NVal
                    PeilArrayUp(i)   = PeilOw(i,iow)
                    AreaArrayUp(i)   = AreaOw(i,iow)
                    VolumeArrayUp(i) = VoluOw(i,iow)
                 Enddo
                 Call RR_INTERP (NVAL, PeilArrayUp, VolumeArrayUp, UPLVL, VOL0, OwLastInterpIndex(iow))
                 Call RR_INTERP (NVAL, PeilArrayUp, AreaArrayUp, UPLVL, UpArea, OwLastInterpIndex(iow))
                 IF (iDebug .ne. 0) WRITE(IDEBUG,*) ' UPLVL , VOL0=',UPLVL,VOL0
               ! bepaal volume zonder eerder berekende QOUT0 lozing mee te nemen
                 VOL0 = VOL0 + QOUT0(IOW) * timeSettings%timestepSize
                 Call RR_INTERP (NVAL, VolumeArrayUp, PeilArrayUp, VOL0, UPLVL, OwLastInterpIndex(iow))
                 IF (iDebug .ne. 0) WRITE(IDEBUG,*) ' QOUT0 =',QOUT0(IOW)
                 IF (iDebug .ne. 0) WRITE(IDEBUG,*) ' UPLVL , VOL0=',UPLVL,VOL0
               ! bepaal inkomend debiet TQIN
                 TQIN = QINOW(IOW,1) + QINOW(IOW,3) + QinOw(Iow,7)
                 TQIN = TQIN + QIN0(IOW,2) + QIN0(IOW,4) + QIN0(IOW,5) + QIN0(IOW,6)
               ! bepaal upstream target level
                 Tlvl = CurrentTargetLevel (Iow)  !Call DTTLVL (TLVL, IOW)
        case (6)   ! upstream is boundary, dus een inlaatkunstwerk!
                 IBNDUP = EiNode(NODEUP,2)
                 UPLV0  = BNDPAR(IBNDUP,1)
                 UPLVL  = BNDPAR(IBNDUP,1)
                 TLVL   = BNDPAR(IBNDUP,1)
                 UPAREA = BNDPAR(IBNDUP,5)
                 TQIN   = 0.0
    end select

    select case (EiNode(nodeUp, 3))
        case (4)   ! upstream node is open water
            peilBottomUp = bottomLevel(EiNode(nodeUp,2))
        case (6)   ! upstream node is boundary -> bottomlevel unknown; set at minimum of boundary level and downstream level
!                     since downstream level not yet known here, set at special number and correct afterwards
            peilBottomUp = - 10.0
        case default
            call ErrMsgStandard(972, EiNode(INODE,1), 'Node upstream of this RR-structure is incorrect', &
                                                          'Check input data ')
    end select

    select case (EiNode(noDown, 3))
        case (4)   ! downstream node is open water
            peilBottomDown = bottomLevel(EiNode(noDown,2))
        case (6)   ! downstream node is boundary -> bottomlevel is minimum van boundary level en upstream bottom level
            peilBottomDown = min (dolvl, peilBottomUp)
        case default
            call ErrMsgStandard(972, EiNode(INODE,1), 'Node downstream of this RR-structure is incorrect', &
                                                          'Check input data ')
    end select

    if (peilBottomUp .eq. -10.0) then        ! upstream is boundary
        peilBottomUp = min (peilBottomDown, UPLVL)
    end if

!   ARS 3357: introductie FractionTime = de fractie van de tijdstap dat het gemaal actief kan zijn
!             (overschrijden aan/afslagpeil kan halverwege tijdstap zijn)
    FractionTime = 1.0
!   ARS 10414 getrapte stuw
    ActCrestLevel(ISTR) = CrestMissingValue
    QSTRU1(ISTR) = 0.0
    QSTRU2(ISTR) = 0.0

    ! *********************************************************************
    ! *** Werk voor gemaal met de oorspronkelijke H (UPLVL)
    ! ***   dwz voor QOUT0=0
    ! *********************************************************************
    ! *********************************************************************
    ! *** Structure type 1: gemaal
    ! *********************************************************************

    IF (STRTYP(ISTR) .EQ. 1) THEN

    ! bepaal target level is hiervoor reeds gebeurd
    ! bepaal afwijking van target level
      DLVL   = UPLVL - TLVL
      DLVL0  = UPLV0 - TLVL

      QCAP = pumpCapacities(iStr)%low + pumpCapacities(iStr)%high ! was STRPAR(ISTR,5)+STRPAR(ISTR,10)
      Call CheckTimeControllerPump (Istr, QCapController, Idebug, Iout1)
      QCap = Min (QCap, QCapController)
      Call CheckPumpReductionFactor (Istr, ReductionFactor, Idebug, Iout1, UpLv0, DoLv0)
      QCap = QCap * ReductionFactor

      IF (iDebug .ne. 0) WRITE(IDEBUG,*) ' DLVL =',DLVL, DLVL0, UPLVL, TLVL, qcap

    ! Juni 1996: check ook op maalstop!
      If (QCAP .LE. .0 .OR. MSFACT(ISTR) .GE. 1.) THEN
         ! Maalstop
         QSTRU(ISTR) = 0.0
      Else
         ! Geen maalstop
         Call DetermineStartStopLevels (StartLevelLow, StartLevelHigh, StopLevelLow,  StopLevelHigh, &
                       Tlvl, Hlvl, Hlvl2, Hvol, Hvol2, Istr, INode, Iow, IDayWk, Idebug, 1)

         Call ComputePump (Istr, StartLevelLow, StartLevelHigh, StopLevelLow, StopLevelHigh, &
                           Dlvl, Dlvl0, Vol0, HVol, Hvol2, QCap, QCapController, FractionTime, ReductionFactor)
      Endif


    ELSEIF (STRTYP(ISTR) .EQ. 2) THEN
    ! *********************************************************************
    ! *** Structure type 2 = stuw;   default weir type 1=rechte stuw, 2= V stuw of Cipoletti stuw, 3=getrapte stuw
    ! *********************************************************************

    !c upstream level
    !c average: UPLVL = (1.0-timesettings.timeWeightFactor) * LVLOW0(IOW) + timesettings.timeWeightFactor * LVLOW(IOW)
! Dec 2001; careful checking whether upstream is open water or boundary, open water was always assumed
      if (iow .gt. 0) then
         UPLVL = LVLOW0(IOW)
      elseif (ibndup .gt. 0) then
         IF (ITmstp .le. 1) &
           call ErrMsgStandard (969, 0, ' For normal weir an upstream open water node is expected; node:',Id_Nod(INODE))
         UPLVL = BndPar(Ibndup,1)
      endif

! Maalstop ook voor willekeurige stuw mogelijk
      if (idebug .ne. 0)  write(IDEBUG,*) ' Maalstop? ', MSFACT(ISTR)

      IF (MSFACT(ISTR) .GE. 1.) THEN
         ! 'Maal'stop
         QSTRU(ISTR) = 0.0
      Else
!        Getrapte stuw (weir type 3), bepaal debiet over 2e vaste trap
         IF (STRPAR(ISTR,11) .EQ. 3) THEN
!           ARS 15315: save DoVolInit and restore it after CmpStf2 computations, otherwise DoLvl and DoVol are not consistent
            DoVolInit = DoVol
            Call CMPSTF2 (ISTR,  INODE, IOW, IOWD,  IBNDUP, IBND, &
                          NODOWN, DOLV0, DOVOL, TQINDW, UPLVL, TQIN, UpArea, DoArea, &
                          NODEUP, peilBottomDown, peilBottomUp)
!           ARS 15315: restore DoVolInit
            DoVol = DoVolInit
         ENDIF

         ! Geen 'Maal'stop
         ZMax = 9999.99
         NTiml3 = NTiml2
         Call CMPSTF (ISTR,  INODE, IOW, IOWD,  IBNDUP, IBND, &
                      NODOWN, DOLVL, DOLV0, DOVOL, TQINDW, UpArea, DoArea, &
                      UPLVL, TLVL, TQIN, .FALSE., .False., Zmax, &
                      NODEUP, peilBottomDown, peilBottomUp, NTiml3)

       ! *********************************************************************
       ! *** Stuw (type 2) met Vast peil regelaar met debietbegrenzer
       ! *********************************************************************

         IF (INT(STRPAR(ISTR,5)) .EQ. 2 .or. INT(StrPar(Istr,5)) .eq. 6) THEN
       ! *** type2=vast peil: streefpeil
       ! ***       tot debietbegrenzing MAXQ
       ! ***       dan mag peil overschreden worden, totdat peil MAXP bereikt wordt
       ! ***       dan mag ook MAXQ overschreden worden
       ! ***       bij daling: als benedenstrooms peil > opgegeven max; dan beperking t
       ! ***      IOW = index bovenstrooms open water
       ! ***      UPLVL=huidig bovenstroom peil, zonder lozing over kunstwerk
       ! ***       bepaal eerst huidig peil, streefpeil TLVL, max.peil TMAX
       ! *** type6=vullingsgraad controller
       ! ***       doel: gelijke vullingsgraad voor beide open waters
       ! ***             (deze controller alleen voor 2 open water knopen!!)
             IF (iDebug .ne. 0) THEN
                WRITE(IDEBUG,*) ' QSTR according to CMPSTF:',QSTRU(ISTR)
                WRITE(IDEBUG,*) ' init-endlevel', LVLOW0(IOW), LVLOW(IOW)
                WRITE(IDEBUG,*) ' QOUT0', QOUT0(IOW)
             ENDIF

             Tlvl = CurrentTargetLevel (Iow)  !Call DTTLVL (TLVL, IOW)

             IF (INT(STRPAR(ISTR,5)) .EQ. 2) THEN
                 ! Streefpeilcontroller met debietbegrenzer
                 !
                 ! Tmax = maximum peil van de streefpeilcontroller met debietbegrenzer
                 !        boven dit peil mag het maximum debiet worden overschreden

                 ! bepaal huidig peil zonder QOUT0 lozing vorige iteratie
                 UPLVL  = LVLOW(IOW)
                 Call RR_INTERP (NVAL, PeilArrayUp, VolumeArrayUp, UPLVL, VOL0, OwLastInterpIndex(iow))
                 VOL0 = VOL0 + QOUT0(IOW) * timeSettings%timestepSize
                 Call RR_INTERP (NVAL, VolumeArrayUp, PeilArrayUp,  VOL0, UPLVL, OwLastInterpIndex(iow))

                 TMAX = STRPAR(ISTR,7)
                 ! n.a.v. ARS 2684: zet Tmax voor controller >= actueel streefpeil
                 Tmax = max (Tmax, Tlvl)
                 ! Bepaal volumina huidig peil, streefpeil, maxpeil
                 Call RR_INTERP (NVAL,  PeilArrayUp, VolumeArrayUp, UPLVL, VOLNU,  OwLastInterpIndex(iow))
                 Call RR_INTERP (NVAL,  PeilArrayUp, VolumeArrayUp, TLVL,  VOLTGT, OwLastInterpIndex(iow))
                 Call RR_INTERP (NVAL,  PeilArrayUp, VolumeArrayUp, TMAX,  VOLMAX, OwLastInterpIndex(iow))
                 IF (iDebug .ne. 0) THEN
                    WRITE(IDEBUG,*) ' Interpolatie volumes and levels '
                    WRITE(IDEBUG,*) '  Actual    Target    Maximum'
                    WRITE(IDEBUG,*) VOLNU, VOLTGT, VOLMAX
                    WRITE(IDEBUG,*) UPLVL, TLVL, TMAX
                 ENDIF

                 ! Bepaal benedenstrooms peil, en of strengere debietbegrenzing nodig is
                 ! Set QOUT = desired flow over the structure
                 NODOWN = DONODE(INODE)
                 IF (EiNode(NODOWN,3) .EQ. 4) THEN
                    IOWD  = EiNode(NODOWN,2)
                    DOLV0 = LVLOW0(IOWD)
                    UPLV0 = LVLOW0(IOW)
                    IF (DOLV0 .GE. STRPAR(ISTR,8)) THEN
                       VOLTGT = VOLMAX
                       IF (iDebug .ne. 0) THEN
                        WRITE(IDEBUG,*) ' Limitation',DOLV0, STRPAR(ISTR,8)
                        WRITE(IDEBUG,*) UPLV0, STRPAR(ISTR,7)
                        WRITE(IDEBUG,*) VOLTGT, VOLMAX
                       ENDIF
                    ENDIF
                 ENDIF
! Jan 2003 ARS 11093
!   Before: target level controller before only allowed positive flow
!           (assuming crest level could rise up to infinity)
!   Adjustment: maximum crest level zmax is introduced
!                QOUT = 0.0
                 TmpCrestLevel = ActCrestLevel(istr)
                 ActCrestLevel(ISTR) = CrestMissingValue    ! actual crest level is not computed
                 UpMax = max (LvlOw0(iow), LvlOw(iow))
                 if (iowd .gt. 0) then
                    Domin = max (LvlOw0(iowd), LvlOw(iowd))   ! max ipv min genomen !!!
                 else
                    Domin = BndPar(ibnd,1)
                 endif
                 ZMax = Strpar(Istr,14)
!                IF (VOLNU .GT. VOLTGT .and. UpMax .gt. DoMin .and. UpMax .le. Zmax) THEN
                 IF (VOLNU .GT. VOLTGT .and. UpMax .gt. DoMin) THEN
                    QOUT = (VOLNU - VOLTGT) / timeSettings%timestepSize
                    IF (QOUT .GT. STRPAR(ISTR,6)) THEN
                       QDIF = QOUT - STRPAR(ISTR,6)
                       QOUT = STRPAR(ISTR,6)
                       VOLTMP = VOLNU - QOUT*timeSettings%timestepSize
                       IF (VOLTMP .GT. VOLMAX) THEN
                           QOUT = MAX (QOUT, (VOLNU-VOLMAX) / timeSettings%timestepSize )
                           IF (iDebug .ne. 0) WRITE(IDEBUG,*) ' QOUT=',QOUT
                       ENDIF
                    ENDIF
                    ! check if flow is realistic: Qout>0 computed by controller only possible if Uplevel>downLevel
                    UpMax = max (LvlOw0(iow), LvlOw(iow))
                    if (iowd .gt. 0) then
                       Domin = min (LvlOw0(iowd), LvlOw(iowd))
                    else
                       Domin = BndPar(ibnd,1)
                    endif
                    if (FixARSControllerLvlCHeck .and. (UpMax .lt. DoMin)) Qout = min (0.0, Qout)
                 ElseIf (Volnu .le. VolTgt .and. Upmax .le. Zmax .and. DoMin .lt. Zmax) then
                    QOut = 0.0
                    ActCrestLevel(ISTR) = ZMax
                 Elseif (Upmax .le. Domin .and. Domin .ge. Zmax) then
                      ! negative flow, try to prevent as much as possible by using Zmax
                      NTiml3 = NTiml2
                      Call CMPSTF  (ISTR, INODE,  IOW,    IOWD,  IBNDUP, IBND, &
                                    NODOWN, DOLVL, DOLV0, DOVOL, TQINDW, UpArea, DoArea, &
                                    UPLVL, TLVL, TQIN, .False., .TRUE., ZMax, &
                                    NODEUP, peilBottomDown, peilBottomUp, NTiml3)
                      QOUT = QSTRU(ISTR)
                      ActCrestLevel(ISTR) = ZMax
                 Else
                     if (idebug .ne. 0)  then
                        write(idebug,*) ' Else tak target level controller'
                        write(idebug,*) ' volnu voltgt=', volnu, voltgt
                        write(idebug,*) ' qout = ', qout
                        write(idebug,*) ' qstru(istr) = ', qstru(istr)
                        write(idebug,*) ' uplvl upmax=', uplvl, upmax
                        write(idebug,*) ' domin ', domin
                        write(idebug,*) ' zmax  ', zmax
                     endif
                     Qout = QStru(istr)
                     ! leave original calculated Qout unchanged, no controller!!
                 ENDIF
!                ActCrestLevel(ISTR) = CrestMissingValue    ! actual crest level is not computed
                 ! Einde streefpeil controller met debietbegrenzer; QOut = computed flow over structure
             ElseIF (INT(STRPAR(ISTR,5)) .EQ. 6) THEN
!                Vullingsgraad controller
                 If (ITmstp .le. 1  .and. (iow .eq. 0 .or. iowd .eq. 0)) then
                     call ErrMsgStandard (969, 0, ' Equal filling controller only allowed with upstream and downstream open water nodes. Check input for weir:', Id_Nod(INODE))
                 endif
                 ! bovenstrooms open water iow
                 UpLvl= LvlOw0(Iow)
                 Tlvl = CurrentTargetLevel(iow)
                 TMAX = MAxLvl(iow)
                 Tmax = max (Tmax, Tlvl)
                 Call RR_INTERP (NVAL, PeilArrayUp, VolumeArrayUp, UPLVL, VOLNU,  OwLastInterpIndex(iow))
                 Call RR_INTERP (NVAL, PeilArrayUp, VolumeArrayUp, TLVL,  VOLTGT, OwLastInterpIndex(iow))
                 Call RR_INTERP (NVAL, PeilArrayUp, VolumeArrayUp, TMAX,  VOLMAX, OwLastInterpIndex(iow))
                 VullingUpstream = (LvlOw0(Iow) - TLvl) / (MaxLvl(Iow) - Tlvl)
                 VullingUpstream = Max (0., VullingUpstream)
                 If (VullingsGraadMaximum100) VullingUpstream = Min (1., VullingUpstream)
                 Call SetVulling (VullingUpstream, LvlOw0(iow), MaxLvl(iow),Tlvl)
!                QMax determined based on initial upstream level (vullingUpstream) and max. flows
                 QMax = StrPar(Istr,6) +  VullingUpstream *(StrPar(Istr,7)-StrPar(Istr,6))
! ARS 11055      restrict Qmax such that VolNuIncFlows >= volume at target level
                 VolNuIncFlows = VolNu + TQIN * timeSettings%timestepSize        &
                                   - QOWUPR(ISTR) * timeSettings%timestepSize    &
                                    - QSTRU02(ISTR) * timeSettings%timestepSize
                 QMax = min (Qmax, (VolnuIncFlows-VolTgt)/timeSettings%timestepSize)
! end adjustment
                 ! benedenstrooms open water iowd
                 DoTMAX = MaxLvl(iowd)
                 DoTlvl = CurrentTargetLevel (Iowd)  !Call DtTlvl (DoTLVL, Iowd)
                 DoTmax = max (DoTmax, DoTlvl)
                 Call RR_INTERP (NVAL, PeilArrayDown, VolumeArrayDown, DoLVL,  DoVOLNU,  OwLastInterpIndex(iowd))
                 Call RR_INTERP (NVAL, PeilArrayDown, VolumeArrayDown, DoTLVL, DoVOLTGT, OwLastInterpIndex(iowd))
                 Call RR_INTERP (NVAL, PeilArrayDown, VolumeArrayDown, DoTMAX, DoVOLMAX, OwLastInterpIndex(iowd))
                 Call SetVulling (VullingDownstream, LvlOw0(iowd), MaxLvl(iowd), DoTlvl)
! Adjust VullingUpstream and Downstream for other flows to these open waters
                 VolNuIncFlows = VolNu + TQIN * timeSettings%timestepSize        &
                                   - QOWUPR(ISTR) * timeSettings%timestepSize    &
                                    - QSTRU02(ISTR) * timeSettings%timestepSize
                 DOVOLNuIncFlows = DOVOLNu + TQINDW * timeSettings%timestepSize  &
                                    + QOWDWR(ISTR) * timeSettings%timestepSize   &
                                      - QOUT0(IOWD) * timeSettings%timestepSize  &
                                       + QSTRU02(ISTR) * timeSettings%timestepSize
                 Call RR_INTERP (NVAL, VolumeArrayUp, PeilArrayUp, VolNuIncFlows, UPLVLIncFlows, OwLastInterpIndex(iow))
                 Call RR_INTERP (NVAL, VolumeArrayDown, PeilArrayDown, DoVolNuIncFlows, DoLVLIncFlows, OwLastInterpIndex(iowd))
                 Call SetVulling (VullingUpstreamIncFlows, UpLvlIncFlows, MaxLvl(iow),Tlvl)
                 Call SetVulling (VullingDownstreamIncFlows, DoLvlIncFlows, MaxLvl(iowd), DoTlvl)

                 ! Calculate filling percentage in case of maximum flow QMax
                 DeltaVMax = Qmax * TimeSettings%TimestepSize
                 TempVolUp = VolNuIncFlows - DeltaVMax
                 Call RR_INTERP (NVAL, VolumeArrayUp, PeilArrayUp, TempVolUp, TempLvlUp, OwLastInterpIndex(iow))
                 Call SetVulling (TempVullingUp, TempLvlUp, MaxLvl(iow),Tlvl)
                 TempVolDo = DoVolNuIncFlows + DeltaVMax
                 Call RR_INTERP (NVAL, VolumeArrayDown, PeilArrayDown, TempVolDo, TempLvlDo, OwLastInterpIndex(iowd))
                 Call SetVulling (TempVullingDo, TempLvlDo, MaxLvl(iowd),DoTlvl)
                 ! Debug
                 IF (iDebug .ne. 0) THEN
                    WRITE(IDEBUG,*) ' UpLvl   ', UpLvl
                    WRITE(IDEBUG,*) ' TLVL    ', TLVL
                    WRITE(IDEBUG,*) ' TMAX    ', TMAX
                    WRITE(IDEBUG,*) ' VOLNU   ', VOLNU
                    WRITE(IDEBUG,*) ' VOLTGT  ', VOLTGT
                    WRITE(IDEBUG,*) ' VOLMax  ', VOLMax
                    WRITE(IDEBUG,*) ' Vullingupstream', VullingUpstream
                    WRITE(IDEBUG,*) ' UpLvlIncFlows   ', UpLvlIncFlows
                    WRITE(IDEBUG,*) ' VOLNUIncFlows   ', VOLNUIncFlows
                    WRITE(IDEBUG,*) ' VullingupstreamIncFlows', VullingUpstreamIncFlows
                    WRITE(IDEBUG,*) ' DoLvl   ', DoLvl
                    WRITE(IDEBUG,*) ' DoTLVL  ', DoTLVL
                    WRITE(IDEBUG,*) ' DoTMAX  ', DoTMAX
                    WRITE(IDEBUG,*) ' DoVOLNU ', DoVOLNU
                    WRITE(IDEBUG,*) ' DoVOLTGT', DoVOLTGT
                    WRITE(IDEBUG,*) ' DoVOLMax', DoVolMax
                    WRITE(IDEBUG,*) ' Vullingdownstream', VullingDownstream
                    WRITE(IDEBUG,*) ' DoLvlIncFlows   ', DoLvlIncFlows
                    WRITE(IDEBUG,*) ' DoVOLNUIncFlows ', DoVOLNUIncFlows
                    WRITE(IDEBUG,*) ' VullingdownstreamIncFlows', VullingDownstreamIncFlows
                    WRITE(IDEBUG,*) ' QMAX    ', QMAX
                    WRITE(IDEBUG,*) ' TempVullingUp with Q=QMax ', TempVullingUp
                    WRITE(IDEBUG,*) ' TempVullingDo with Q=QMax ', TempVullingDo
                 ENDIF
                 Qout = Qmax
                 ! If needed, reduce flow such that filling percentages will be equal
                 If (VullingDownstreamIncFlows .gt. VullingUpstreamIncFlows) then
                    Qout = 0.0
! addition for ARS 11055
                 Elseif (VullingUpstreamIncFlows .le. 0.0) then      ! vulling up <=0, vullingdown >0
                    Qout = 0.0
! end addition
                 Elseif (TempVullingDo .gt. TempVullingUp) then
                    ! assume linear relation level-volume
                    QOut = QMax * (VullingUpstreamIncFlows - VullingDownstreamIncFlows) / &
                           (VullingUpstreamIncFlows - VullingDownstreamIncFlows + TempVullingDo - TempVullingUp)
                 Endif
                 ActCrestLevel(ISTR) = CrestMissingValue   ! actual crest level is not computed
             Endif

             if (idebug .ne. 0) then
                 Write(Idebug,*) ' Before checking phys.max. flow using CMPSTF with ZBottm; QOut=',Qout
             Endif
             ! Check fysisch maximum debiet bij evt. opgegeven min. kruinhoogte (of bodemniveau)
             UPLVL = LVLOW0(IOW)
!            ARS 11093: only check max. flow in case of positive Qout
             if (Qout .gt. 0) then
                NTiml3 = NTiml2
                Call CMPSTF  (ISTR, INODE,  IOW,    IOWD,  IBNDUP, IBND, &
                              NODOWN, DOLVL, DOLV0, DOVOL, TQINDW, UpArea, DoArea, &
                              UPLVL, TLVL, TQIN, .TRUE., .False., Zmax, &
                              NODEUP, peilBottomDown, peilBottomUp, NTiml3)
                if (QStru(Istr) .le. 0 .and. Dolvl .lt. UpLvl) then
                   NTiml3 = 1
                   if (idebug .ne. 0) write(Idebug,*) ' Call Cmpstf with Ntiml3=1'
                   Call CMPSTF  (ISTR, INODE,  IOW,    IOWD,  IBNDUP, IBND, &
                                 NODOWN, DOLVL, DOLV0, DOVOL, TQINDW, UpArea, DoArea, &
                                 UPLVL, TLVL, TQIN, .TRUE., .False., Zmax, &
                                 NODEUP, peilBottomDown, peilBottomUp, NTiml3)
                endif
             else
                QStru(istr) = Qout
             endif
             ! begrenzing op fysische maximum debiet
             if (Qstru(istr) .lt. Qout) then
                 ActCrestLevel(istr) = BottomCrestLevel(istr)
             Endif
             QOUT = MIN (QOUT, QSTRU(ISTR))
             IF (iDebug .ne. 0) WRITE(IDEBUG,*) ' QOUT=',QOUT


! ARS 11093  A negative QOut is allowed if Zmax defined in input
             If (ZMax .ge. 9999.) then
                QOUT = MAX (0.0, QOUT)
             Else
!               do not change Qout; a negative QOut is allowed !!
             Endif

             QSTRU(ISTR) = QOUT
             IF (iDebug .ne. 0) THEN
                WRITE(IDEBUG,*) ' TLVL    ', TLVL
                WRITE(IDEBUG,*) ' QMAX    ', STRPAR(ISTR,6)
                WRITE(IDEBUG,*) ' TMAX    ', TMAX
                WRITE(IDEBUG,*) ' VOLNU   ', VOLNU
                WRITE(IDEBUG,*) ' VOLTGT  ', VOLTGT
                WRITE(IDEBUG,*) ' VOLTMP  ', VOLTMP
                WRITE(IDEBUG,*) ' QOUT    ', QOUT
             ENDIF
!            Add computation of ActCrestLevel
             If (ActCrestLevel(istr) .eq. CrestMissingValue) &
                 Call ComputeActCrestLevel (QStru(istr), istr, UpLvl, DoLvl, Iout1, Idebug)
         ENDIF
         QSTRU1(ISTR) = QSTRU(ISTR)

!        Bepaal totaal debiet over de stuw
         QSTRU(ISTR) = QSTRU1(ISTR) + QSTRU2(ISTR)

      ENDIF


    ELSEIF (STRTYP(ISTR) .EQ. 3) THEN
    ! *********************************************************************
    ! *** Structure type 3 = onderlaat
    ! *********************************************************************

    ! average UPLVL = (1.0-timesettings.timeWeightFactor) * LVLOW0(IOW) + timesettings.timeWeightFactor * LVLOW(IOW)
! Dec 2001; careful checking whether upstream is open water or boundary, open water was always assumed
      if (iow .gt. 0) then
         UPLVL = LVLOW0(IOW)
      elseif (ibndup .gt. 0) then
         IF (ITmstp .le. 1) &
          call ErrMsgStandard (969, 0, ' For normal orifice an upstream open water node is expected; node:', Id_Nod(INODE))
         UPLVL = BndPar(Ibndup,1)
      endif

    !c ipv gemiddeld peil: neem loop per 5 minuten
    !c werk met eindpeil per 5 minuten
    !C 021195: bij grote fluctuaties wordt met kleinere tijdstap gerekend

 ! Maalstop ook voor willekeurige onderlaat mogelijk
      if (idebug .ne. 0) write(IDEBUG,*) ' Maalstop? ', MSFACT(ISTR)
      IF (MSFACT(ISTR) .GE. 1.) THEN
         QSTRU(ISTR) = 0.0
      Else
         Call CMPOND (ISTR,  INODE, IOW,    IOWD,  IBNDUP, IBND, &
                      NODOWN, DOLV0, DOVOL, TQINDW, UpArea, DoArea, &
                      UPLVL, TQIN, NODEUP, peilBottomDown, peilBottomUp)
      ENDIF

    ELSEIF (STRTYP(ISTR) .EQ. 4) THEN
    ! *********************************************************************
    ! *** Structure type 4 = weerstand: Manning
    ! *********************************************************************

! Dec 2001; careful checking whether upstream is open water or boundary, open water was always assumed
      if (iow .gt. 0) then
         UPLVL = LVLOW0(IOW)
      elseif (ibndup .gt. 0) then
         IF (ITmstp .le. 1) &
          call ErrMsgStandard (969, 0, ' For a Manning friction an upstream open water node is expected; node:', &
                               Id_Nod(INODE))
         UPLVL = BndPar(Ibndup,1)
      endif

    !021195
    !c ipv gemiddeld peil: neem loop per 5 minuten
    !c werk met eindpeil per 5 minuten
    !c 021195: bij grote fluctuaties wordt met nog kleine tijdstap gerekend

      NTIMLC = MAX (NTIML, 1)
204   CONTINUE
      UPLVT = UPLVL
      Call RR_INTERP (NVAL, PeilArrayUp, VolumeArrayUp, UPLVL, VOL0, OwLastInterpIndex(iow))
      DOLVT  = DOLV0
      DOLVT0 = DOLVT

      IF (iDebug .ne. 0) WRITE(IDEBUG,*) ' Upstream level, Qstr'

      DO ITL = 1,NTIMLC

        Call ComputeManningFormula (StrPar(Istr,1), StrPar(istr,2), StrPar(istr,4), StrPar(istr,5), &
                                    Istr, Inode, UpLvt, DoLvT, PeilBottomUp, PeilBottomDown)

        rdummy = -999.99
        Call VolumeCheck (ISTR, INODE, IBNDUP, IBND, iow, iowd, Qstru(Istr), &
                          NODOWN, NODEUP, peilBottomDown, peilBottomUp, &
                          dolvt, uplvt, Uparea, DoArea, NtimLc, 3, rdummy, 0.0)

        QTEMP(ITL) = QSTRU(ISTR)
        VOL0 = VOL0 - QTEMP(ITL) * timeSettings%timestepSize / NTIMLC &
                    + TQIN * timeSettings%timestepSize / NTIMLC &
                    - QOWUPR(ISTR) * timeSettings%timestepSize / NTIMLC
        UPLVT0 = UPLVT
        Call RR_INTERP (NVAL, VolumeArrayUp, PeilArrayUp, VOL0, UPLVT, OwLastInterpIndex(iow))
    ! Jan 1996: UPLVT mag niet het maximum peil OWMNMA overschrijden
        UPLVT = MIN (UPLVT, OWMNMA(IOW))
        if (upLvT < peilBottomUp)  upLvt = peilBottomUp

    ! extra check of kleinere tijdstap nodig is
        IF (iDebug .ne. 0) then
           WRITE(IDEBUG,*) ' Test restart resistance computations'
           WRITE(IDEBUG,*) ' Structcomp         ', StructComp
           WRITE(IDEBUG,*) ' abs (uplvt-uplvt0) ', Abs(Uplvt-Uplvt0)
           WRITE(IDEBUG,*) ' abs (uplvt0-dolvt) ', Abs(Uplvt0-dolvt)
           WRITE(IDEBUG,*) ' abs (dolvt-dolvt0) ', Abs(dolvt-dolvt0)
           WRITE(IDEBUG,*) ' ntimlc ntiml2      ', ntimlc, ntiml2
           WRITE(IDEBUG,*) ' timestep/ntimlc nrsmin', TimeSettings%TimestepSize/NtimLc, NrsMin
        Endif
        If (StructComp .eq. 1) then
          ! nieuwe methode
          IF ( (ABS(UPLVT-UPLVT0) .GT. ABS(UPLVT0-DOLVT) .OR. &
                 ABS(UPLVT-UPLVT0) .GT. .05 .OR. &
                  ABS(DOLVT-DOLVT0) .GT. .05         ) &
                   .AND. NTIMLC .NE. NTIML2 .AND. timeSettings%timestepSize/NTIMLC .GE. NRSMIN) THEN
             IF (iDebug .ne. 0) WRITE(IDEBUG,*) ' Restart resistance computations'
             NTimLc_old = NTimLc
             NTIMLC = Min (NTIML2, 2*TimeSettings%timestepSize/NrsMin)
             NTimLc = Max (NTimLc, NTimLc_Old + 1)
             GOTO 204
          ENDIF
         Else
          ! oude test, zoals voorheen
          IF ( (ABS(UPLVT-UPLVT0) .GT. ABS(UPLVT0-DOLVT) .OR. &
                 ABS(UPLVT-UPLVT0) .GT. .05 .OR. &
                  ABS(DOLVT-DOLVT0) .GT. .05         ) &
                   .AND. NTIMLC .NE. NTIML2) Then
             IF (iDebug .ne. 0) WRITE(IDEBUG,*) ' Restart resistance computations'
             NTimLc_old = NTimLc
             NTIMLC = NTIML2
             NTimLc = Max (NTimLc, NTimLc_Old + 1)
             GOTO 204
          ENDIF
         ENDIF
    ! evt. aanpassing benedenstrooms peil;
        IF (EiNode(NODOWN,3) .EQ. 4) THEN
           DOLVT0 = DOLVT
           DOVOL = DOVOL + TQINDW * timeSettings%timestepSize / NTIMLC &
                    + QOWDWR(ISTR) * timeSettings%timestepSize / NTIMLC &
                    + QTEMP(ITL) * timeSettings%timestepSize / NTIMLC &
                    - QOUT0(IOWD) * timeSettings%timestepSize / NTIMLC
           Call RR_INTERP (NVAL, VolumeArrayDown, PeilArrayDown, DOVOL, DOLVT, OwLastInterpIndex(iowd))
    ! Jan 1996: DOLVT mag niet het maximum peil OWMNMA overschrijden
           DOLVT = MIN (DOLVT, OWMNMA(IOWD))

            if (doLvT < peilBottomDown) then
              doLvt = peilBottomDown
            end if
        ELSEIF (EiNode(NODOWN,3) .EQ. 6) THEN
            DELTA_volume = QTEMP(ITL)* timeSettings%timestepSize / NTIMLC
            DOLVT0 = DOLVT
! Mar 2002  only change of DOLVT within timestep if switch CFBoundaryConstantInTimestep is not on
            if (CFBoundaryConstantInTimestep .eq. 0) then
              DOLVT  = DOLVT + ( DELTA_volume / BNDPAR(ibnd,5) / VolumeCheckFactorToCF )   !! ARS15351 Feb 2006
            endif
            if (idebug .ne. 0) &
              write(IDEBUG,*) ' Delta_Volume, DoLvT0, DoLvt, area ', Delta_volume, DoLvt0, DoLvt, BndPar(ibnd,5)
        ENDIF
      ENDDO

      QSTRU(ISTR) = 0.0
      DO ITL=1,NTIMLC
         QSTRU(ISTR) = QSTRU(ISTR) + QTEMP(ITL)
      ENDDO
      QSTRU(ISTR) = QSTRU(ISTR)/NTIMLC

! Maalstop ook voor Manning weerstand mogelijk
     if (idebug .ne. 0)  write(IDEBUG,*) ' Maalstop? ', MSFACT(ISTR)
      IF (MSFACT(ISTR) .GE. 1.) THEN
         QSTRU(ISTR) = 0.0
      ENDIF


    ELSEIF (STRTYP(ISTR) .EQ. 5) THEN

    ! *********************************************************************
    ! *** Structure type 5 = Q-h relaties
    ! *********************************************************************

! Modifications for ARS 8734: compute upstream and downstream levels, take care of open water and boundary
      if (iow .gt. 0) then
         UPLVL = (1.0 - timesettings%timeWeightFactor) * LVLOW0(IOW) + &
                  timesettings%timeWeightFactor * LVLOW(IOW)
      elseif (ibndup .gt. 0) then
         UPLVL = BNDPAR (IBNDup, 1)
      endif
      if (IOWD .gt. 0) then
          DOLVL = (1.0 - timesettings%timeWeightFactor) * LVLOW0(IOWD) + &
                   timesettings%timeWeightFactor * LVLOW(IOWD)
      elseif (ibnd .gt. 0) then
          DOLVL = BNDPAR(IBND,1)
      endif

      DO K=1,6
         ARR1(K) = STRPAR(ISTR,K)
         ARR2(K) = STRPAR(ISTR,K+6)
      ENDDO
! ARS 8734: take care of different cases, h=hupstream, hdownstream, or deltah
      If (STRPar(Istr,13) .eq. 0) then
!       Qh relation with h=upstream level
        Call RR_INTERP (NVAL, ARR2, ARR1, UPLVL, QOUT, IDUM)
        IF (iDebug .ne. 0) WRITE(IDEBUG,*) ' upstream level    ', UPLVL
      ElseIf (STRPar(Istr,13) .eq. 1) then
!       Qh relation with h=downstream level
        Call RR_INTERP (NVAL, ARR2, ARR1, DOLVL, QOUT, IDUM)
        IF (iDebug .ne. 0) WRITE(IDEBUG,*) ' downstream level    ', DOLVL
      ElseIf (STRPar(Istr,13) .eq. 2) then
!       Qh relation with h=level difference
        Call RR_INTERP (NVAL, ARR2, ARR1, UPLVL-DOLVL, QOUT, IDUM)
        IF (iDebug .ne. 0) WRITE(IDEBUG,*) ' level difference    ', UPLVL-DOLVL
      Endif
      IF (iDebug .ne. 0)  WRITE(IDEBUG,*) ' flow through structure', QOUT
      QSTRU(ISTR) = QOUT

! Maalstop ook voor Q-h relatie mogelijk
      if (idebug .ne. 0) write(IDEBUG,*) ' Maalstop? ', MSFACT(ISTR)
      IF (MSFACT(ISTR) .GE. 1.) THEN
         QSTRU(ISTR) = 0.0
      ENDIF

    ELSEIF (STRTYP(ISTR) .EQ. 6) THEN
    ! *********************************************************************
    ! *** Structure type 6: inlaatstuw
    ! *********************************************************************
    ! bepaal afwijking initieel downstream level tov target level
! Dec 2001; careful checking whether downstream node is open water or boundary, open water was always assumed
      if (iowd .gt. 0) then
!         downstream open water is ok
      elseif (ibnd .gt. 0) then
         IF (ITmstp .le. 1) &
          call ErrMsgStandard (969, 0, ' For an inlet weir, a downstream open water node is expected; node:', Id_Nod(INODE))
      endif

       DLVL   = LVLOW  (IOWD) - TLVLDO     ! stuur dus op eind peil ipv initieel peil
       DLVL0  = LVLOW0 (IOWD) - TLVLDO

       IF (iDebug .ne. 0) WRITE(IDEBUG,*) ' DLVL =',DLVL, DLVL0

      currentDate%year = ConfArr_get_IYear()
      currentDate%month = ConfArr_get_iMonth()
      currentDate%day = ConfArr_get_iDay()
      currentTime%hour = ConfArr_get_iHour()
      currentTime%minute = ConfArr_get_iMinute()
      currentTime%second = 0
      RowNr = -1
      TabelNr = StrRefOnOffTable (iStr)
      StartLevel = GetNewValue(TableHandle, TabelNr, 1, RowNr, CurrentDate, CurrentTime, &
                               Idebug, iout1, DateTimeOutsideTable, .true. )
      StopLevel  = GetNewValue(TableHandle, TabelNr, 2, RowNr, CurrentDate, CurrentTime, &
                               Idebug, iout1, DateTimeOutsideTable, .true. )
      if (idebug .ne. 0)  write(idebug,*) ' New Method OnOffLevels', StartLevel, StopLevel

! Maalstop ook voor inlaatstuw mogelijk
      if (idebug .ne. 0) write(IDEBUG,*) ' Maalstop? ', MSFACT(ISTR)
      IF (MSFACT(ISTR) .GE. 1.) THEN
         ! Maalstop
         QSTRU(ISTR) = 0.0
      Else
         ! Geen Maalstop
         ! opereer inlaatstuw
         ! stuw laat water in

! Backwards compatibility: operation on initial level !
           if (StructureOperation .eq. 0) Dlvl = Dlvl0

           select case (strSt0(iStr))
             case (1)    ! stuw laat water in
               if ( DLVL >= stopLevel) THEN
                    FractionTime = 0.
                    If (Dlvl0 .lt. stopLevel .and. StructureOperation .ne. 0) &
                                             Call SetFractionTime (stoplevel, Dlvl, Dlvl0, FractionTime,1)
                 ! inlaatstuw slaat af gedurende de tijdstap
                    STRSTA(ISTR) = 0
                 !  QSTRU(ISTR) = 0.0   oud
                    NTiml3 = NTiml2
                    Call CMPSTF (ISTR,  INODE, IOW,    IOWD,  IBNDUP, IBND, &
                                 NODOWN, DOLVL, DOLV0, DOVOL, TQINDW, UpArea, DoArea, &
                                 UPLVL, TLVL, TQIN, .FALSE., .False., ZMax, &
                                 NODEUP, peilBottomDown, peilBottomUp, NTiml3)
                    QSTRU(ISTR) = FractionTime * QStru(istr)
               else
                 ! inlaatstuw blijft aan; bereken stuw flow
                    STRSTA(ISTR) = 1
                    NTiml3 = NTiml2
                    Call CMPSTF (ISTR,  INODE, IOW,    IOWD,  IBNDUP, IBND, &
                                 NODOWN, DOLVL, DOLV0, DOVOL, TQINDW, UpArea, DoArea, &
                                 UPLVL, TLVL, TQIN, .FALSE., .False., ZMax,&
                                 NODEUP, peilBottomDown, peilBottomUp, NTiml3)
               endif
             case (0)  ! stuw laat geen water in
               IF ( DLVL < startLevel ) THEN
                 ! inlaatstuw slaat aan gedurende tijdstap; bereken stuw flow
                    FractionTime = 1.
                    If (Dlvl0 .gt. startLevel .and. StructureOperation .ne. 0) &
                                             Call SetFractionTime (startlevel, Dlvl, Dlvl0, FractionTime,2)
                    STRSTA(ISTR) = 1
                    NTiml3 = NTiml2
                    Call CMPSTF (ISTR,  INODE, IOW,    IOWD,  IBNDUP, IBND, &
                                 NODOWN, DOLVL, DOLV0, DOVOL, TQINDW, UpArea, DoArea, &
                                 UPLVL, TLVL, TQIN, .FALSE., .False., ZMax, &
                                 NODEUP, peilBottomDown, peilBottomUp, NTiml3)
                    QSTRU(ISTR) = FractionTime * QStru(istr)
               ELSE
                 ! inlaatstuw blijft uit
                    QSTRU(ISTR) = 0.0
                    STRSTA(ISTR) = 0
               ENDIF
           end select
           ! plus minimum continue doorspoeling
           QSTRU(ISTR) = QSTRU(ISTR) + StrPar(iStr, 9)

           QSTRU1(ISTR) = QSTRU(ISTR)
! Bij Getrapte Stuw (weir type 3) voeg debiet over 2e trap toe
           IF (STRPAR(ISTR,11) .EQ. 3) THEN
              Call CMPSTF2 (ISTR,  INODE, IOW, IOWD,  IBNDUP, IBND, &
                            NODOWN, DOLV0, DOVOL, TQINDW, UPLVL, TQIN, UpArea, DoArea, &
                            NODEUP, peilBottomDown, peilBottomUp)
           ENDIF
           QSTRU(ISTR) = QSTRU1(ISTR) + QSTRU2(ISTR)

      ENDIF

    ELSEIF (STRTYP(ISTR) .EQ. 7) THEN
    ! *********************************************************************
    ! *** Structure type 7: inlaatonderlaat
    ! *********************************************************************
! Dec 2001; careful checking whether downstream node is open water or boundary, open water was always assumed
      if (iowd .gt. 0) then
!         downstream open water is ok
      elseif (ibnd .gt. 0) then
         IF (ITmstp .le. 1) &
          call ErrMsgStandard (969, 0, ' For an inlet orifice, a downstream open water node is expected; node:',&
                               Id_Nod(INODE))
      endif

    ! bepaal afwijking initieel downstream level tov target level
       DLVL   = LVLOW  (IOWD) - TLVLDO     ! stuur dus op eind peil ipv initieel peil
       DLVL0  = LVLOW0 (IOWD) - TLVLDO
       IF (iDebug .ne. 0) WRITE(IDEBUG,*) ' DLVL =',DLVL, DLVL0


      currentDate%year = ConfArr_get_IYear()
      currentDate%month = ConfArr_get_iMonth()
      currentDate%day = ConfArr_get_iDay()
      currentTime%hour = ConfArr_get_iHour()
      currentTime%minute = ConfArr_get_iMinute()
      currentTime%second = 0
      RowNr = -1
      TabelNr = StrRefOnOffTable (iStr)
      StartLevel = GetNewValue(TableHandle, TabelNr, 1, RowNr, CurrentDate, CurrentTime, &
                               Idebug, iout1, DateTimeOutsideTable, .true. )
      StopLevel  = GetNewValue(TableHandle, TabelNr, 2, RowNr, CurrentDate, CurrentTime, &
                               Idebug, iout1, DateTimeOutsideTable, .true. )
      if (idebug .ne. 0)  write(idebug,*) ' New Method OnOffLevels', StartLevel, StopLevel

    ! opereer inlaatonderlaat
      if (idebug .ne. 0) THEN
         WRITE(IDEBUG,*) ' Inlaatonderlaat', ISTR
         WRITE(IDEBUG,*) ' init.status', STRST0(ISTR)
         write(IDEBUG,*) ' DLVL START STOP', DLVL, StartLevel, StopLevel
         write(IDEBUG,*) ' Maalstop? ', MSFACT(ISTR)
      endif

! Maalstop ook voor inlaatonderlaat mogelijk
      IF (MSFACT(ISTR) .GE. 1.) THEN
         ! Maalstop
         QSTRU(ISTR) = 0.0
      Else
         ! Geen Maalstop
         ! Opereer inlaatonderlaat

! Backwards compatibility: operation on initial level !
         if (StructureOperation .eq. 0) Dlvl = Dlvl0

         select case (strSt0(iStr))
           case (1)    ! onderlaat laat water in
             if ( DLVL >= stopLevel ) THEN
                  FractionTime = 0.
                  If (Dlvl0 .lt. stopLevel .and. StructureOperation .ne. 0) &
                                         Call SetFractionTime (stoplevel, Dlvl, Dlvl0, FractionTime,1)
               ! inlaatonderlaat slaat af
                  STRSTA(ISTR) = 0
               !  QSTRU(ISTR) = 0.0
                  Call CMPOND (ISTR, INODE,  IOW,    IOWD,  IBNDUP, IBND, &
                             NODOWN, DOLV0, DOVOL, TQINDW, UpArea, DoArea, &
                             UPLVL, TQIN, NODEUP, peilBottomDown, peilBottomUp)
                  QSTRU(ISTR) = FractionTime * QStru(istr)
             else
               ! inlaatonderlaat blijft aan; bereken onderlaat flow
                STRSTA(ISTR) = 1
                Call CMPOND (ISTR, INODE,  IOW,    IOWD,  IBNDUP, IBND, &
                             NODOWN, DOLV0, DOVOL, TQINDW, UpArea, DoArea, &
                             UPLVL, TQIN, NODEUP, peilBottomDown, peilBottomUp)
             end if
           case (0)  ! inlaat-onderlaat laat geen water in
             IF ( DLVL < startLevel) THEN
               ! inlaatonderlaat slaat aan gedurende tijdstap; bereken onderlaat flow
                FractionTime = 1.
                If (Dlvl0 .gt. startLevel .and. StructureOperation .ne. 0) &
                                         Call SetFractionTime (startlevel, Dlvl, Dlvl0, FractionTime,2)
                STRSTA(ISTR) = 1
                Call CMPOND (ISTR,  INODE, IOW,    IOWD,  IBNDUP, IBND, &
                             NODOWN, DOLV0, DOVOL, TQINDW, UpArea, DoArea, &
                             UPLVL, TQIN, NODEUP, peilBottomDown, peilBottomUp)
                QSTRU(ISTR) = FractionTime * QStru(istr)
             ELSE
               ! inlaat onderlaat blijft uit
                  QSTRU(ISTR) = 0.0
                  STRSTA(ISTR) = 0
             ENDIF
         end select

         if (idebug .ne. 0) THEN
            WRITE(IDEBUG,*) ' Debiet Inlaatonderlaat excl.doorspoeling', QSTRU(ISTR)
         endif


         ! plus minimum continue doorspoeling
          QSTRU(ISTR) = QSTRU(ISTR) + STRPAR (ISTR,10)

      ENDIF

    ELSEIF (STRTYP(ISTR) .EQ. 8) THEN
    ! *********************************************************************
    ! *** Structure type 8: inlaatgemaal
    ! *********************************************************************
    ! bepaal afwijking van benedenstrooms streefpeil (NB open water IOWD)
 ! ARS 8003: consequent IOWD gebruiken voor het benedenstroomse open water
 ! bij de bepaling van het debiet QBND wordt er vanuit gegaan dat IOW, IOWD, IBNDUP en IBND
 ! netjes zijn gedefinieerd, met IOW voor het bovenstroomse open water
 !                               IOWD voor het benedenstroomse open water
 !                               IBNDUP voor de bovenstroomse rand
 !                               IBND voor de benedenstroomse rand
 !  IOW en IBNDUP zijn niet beide ongelijk aan nul
 !  IOWD en IBND zijn niet beide ongelijk aan nul
 !
! Dec 2001; careful checking whether downstream node is open water or boundary, open water was always assumed
      if (iowd .gt. 0) then
!         downstream open water is ok
      elseif (ibnd .gt. 0) then
         IF (ITmstp .le. 1) &
          call ErrMsgStandard (969, 0, ' For an inlet pump, a downstream open water node is expected; node:', Id_Nod(INODE))
      endif

      IOWD   = EiNode(NODOWN,2)
      DOLVL  = LVLOW (IOWD)
      Call RR_INTERP (NVAL, PeilArrayDown, VolumeArrayDown, DOLVL, VOL0, OwLastInterpIndex(iowd))
      IF (iDebug .ne. 0) WRITE(IDEBUG,*) ' DOLVL , VOL0=',DOLVL,VOL0
    ! Werk met de oorspronkelijke H (DOLVL)
    ! Bepaal volume zonder eerder berekende inlaat mee te nemen
      VOL0 = VOL0 - QIN0(IOWD,4) * timeSettings%timestepSize
      Call RR_INTERP (NVAL, VolumeArrayDown, PeilArrayDown, VOL0, DOLVL, OwLastInterpIndex(iowd))
      IF (iDebug .ne. 0) WRITE(IDEBUG,*) ' QIN0 inlaat=',QIN0(IOWD,4)
      IF (iDebug .ne. 0) WRITE(IDEBUG,*) ' DOLVL , VOL0=',DOLVL,VOL0

    ! afwijking van target level
      DLVL   = DOLVL - TLVLDO
      DLVL0  = DOLV0 - TLVLDO
      IF (iDebug .ne. 0) WRITE(IDEBUG,*) ' DLVL =',DLVL, DLVL0

      QCAP = pumpCapacities(iStr)%low + pumpCapacities(iStr)%high ! was STRPAR(ISTR,5)+STRPAR(ISTR,10)
      Call CheckTimeControllerPump (Istr, QCapController, Idebug, Iout1)
      QCap = Min (QCap, QCapController)
      Call CheckPumpReductionFactor (Istr, ReductionFactor, Idebug, Iout1, UpLv0, DoLv0)
      QCap = QCap * ReductionFactor

      ! Juni 1996: check ook op maalstop!
      IF (QCAP .LE. .0 .OR. MSFACT(ISTR) .GE. 1.) THEN
         ! Maalstop
         QSTRU(ISTR) = 0.0
      Else
         ! Geen Maalstop
         ! Opereer inlaatpomp
          Call DetermineStartStopLevels (StartLevelLow, StartLevelHigh, StopLevelLow,  StopLevelHigh, &
                        TlvlDo, Hlvl, Hlvl2, Hvol, Hvol2, Istr, INode, IowD, IDayWk, Idebug, 2)

          Call ComputeInletPump (Istr, StartLevelLow, StartLevelHigh, StopLevelLow, StopLevelHigh, &
                           Dlvl, Dlvl0, Vol0, HVol, Hvol2, QCap, QCapController, FractionTime, ReductionFactor)

      ENDIF

    ENDIF

    ! *********************************************************************
    ! *** Zet output voor bovenstroomse open water
    ! *** 01-1996: correctie voor meer benedenstr. kunstwerken bij 1 open water
    ! *** Bepaal inflow naar benedenstroomse open water of boundary
    ! *********************************************************************
    !old  QOUTOW(IOW) = QSTRU(ISTR)
    !new
    if (idebug .ne. 0) write(idebug,*) ' IOW IOWD IBNDUP IBND ', IOW, IOWD, IBNDUP, IBND

! June 2001 ARS ....; July ARS 8003
    IF (IOW .gt. 0) then
       QOUTOW(IOW) = QOUTOW(IOW) + QSTRU(ISTR)
! before: only for normal structures, not for inlet structures (were assumed to be with upstream boundary)
!    IF (STRTYP(ISTR) .LE. 5) THEN
!       QOUTOW(IOW) = QOUTOW(IOW) + QSTRU(ISTR)
    ELSEIF (IBNDUP .GT. 0) THEN
       NODEUP = UPNODE(INODE)
       IBND = EiNode(NODEUP,2)
       QBND(IBND) = QBND(IBND) - QSTRU(ISTR)
       QINBND(IBND)%TotalStructure = QINBND(IBND)%TotalStructure - QSTRU(ISTR)
 ! TEST
 !      if (idebug .ne. 0) write(idebug,*) ' Extra call CmpBnd uit CmpStr'
 !      Call CmpBnd(itmstp, ibnd, nodeup)
 ! End debug
    ENDIF

    NODOWN = doNode(INODE)
    IF (EiNode(NODOWN,3) .EQ. 4) THEN
       IOWD   = EiNode(NODOWN,2)
       QINOW(IOWD,4) = QINOW(IOWD,4) + QSTRU(ISTR)
    ELSEIF (EiNode(NODOWN,3) .EQ. 6) THEN
       IBND   = EiNode(NODOWN,2)
       QBND(IBND) = QBND(IBND) + QSTRU(ISTR)
       QINBND(IBND)%TotalStructure = QINBND(IBND)%TotalStructure + QSTRU(ISTR)
 ! TEST
 !      if (idebug .ne. 0) write(idebug,*) ' Extra call CmpBnd uit CmpStr'
 !      Call CmpBnd(itmstp, ibnd, nodown)
 ! End debug
    ENDIF

    ! *********************************************************************
    ! *** debug
    ! *********************************************************************

    IF (iDebug .ne. 0) THEN
       WRITE(IDEBUG,*) ' Structure', Id_Nod(INODE)
       WRITE(IDEBUG,*) ' Timestep nr                :',ITMSTP
       WRITE(IDEBUG,*) ' Timestepsize in s          :',timeSettings%timestepSize
       IF (IOW .GT. 0) THEN
          WRITE(IDEBUG,*) ' Initial upstream level     :',LVLOW0(IOW)
          WRITE(IDEBUG,*) ' Final upstream level       :',LVLOW(IOW)
       ELSE
          WRITE(IDEBUG,*) ' Upstream boundary level    :',UPLV0
       ENDIF
       WRITE(IDEBUG,*) ' Used in computations       :',UPLVL
       WRITE(IDEBUG,*) ' Downstream level           :',DOLVL
       WRITE(IDEBUG,*) ' Timestepsize in s          :',timeSettings%timestepSize
       WRITE(IDEBUG,*) ' Initial status structure   :',STRST0(ISTR)
       WRITE(IDEBUG,*) ' Status structure           :',STRSTA(ISTR)
       WRITE(IDEBUG,*) ' Actual Crest level/opening :',ActCrestLevel(ISTR)
       WRITE(IDEBUG,*) ' Bottom Crest level/opening :',BottomCrestLevel(ISTR)
       WRITE(IDEBUG,*) ' Outflow structure m3/s,m3  :',QSTRU(ISTR), QSTRU(ISTR) * timeSettings%timestepSize
       WRITE(IDEBUG,*) ' FractionTime               :',FractionTime
    ENDIF

    RETURN
  END subroutine cmpstr



    Subroutine ComputeActCrestLevel (ActualFlow, istr, UpstreamLevel, DownstreamLevel, Iout1, Idebug)
    Integer istr
    Real    ActualFlow, UpstreamLevel, DownstreamLevel

    Real, parameter ::   G  = 9.81
    Real  Gcoef, VSlope, C, Width, U, Z, ZFree
    Real  RefDo, RefUp, Y, Y2, Z1, DeltaH, ActualRatio, ActualReductionFactor, CheckFlow
    Integer  iter, IfReal, idum
    Logical  NegativeFlow
    Integer iDebug, Iout1


    NegativeFlow = (ActualFlow .lt. 0.0)

    If (StrPar(Istr,11) .eq. 2) then
      ! Vstuw werkt met GCOEF = 16/25 * ((2./5.* G) ** 0.5 )
      GCOEF = (2./5.* G) ** 0.5
      GCOEF = 16./25. * Gcoef
      VSlope = Strpar(Istr,12)
    Else
      ! rechte stuw werkt met GCOEF = 2/3 *  ((2./3.* G) ** 0.5 )
      GCOEF = (2./3.* G) ** 0.5
      GCOEF = 2./3. * GCOEF
    Endif
    C     = STRPAR(ISTR,1)
    WIDTH = STRPAR(ISTR,2)
    U     = STRPAR(ISTR,4)

    If (StrPar(Istr,11) .eq. 2) then
!     Vstuw
!     First estimate of Z based on rectangular weir
!     ARS 14739  consistent use of Actual flow; in some cases due to lumping of timesteps it could be the case that
!                                               the actual flow is negative while last upstreamLevel>DownstreamLevel
!     If (UpstreamLevel .gt. DownstreamLevel) then
      If (ActualFlow .ge. 0.0) then
          Z  = UpstreamLevel - ( ActualFlow / ( C * WIDTH * GCOEF ) ) ** (1/U)
      Else
          Z  = DownstreamLevel - ( Abs(ActualFlow) / ( C * WIDTH * GCOEF ) ) ** (1/U)
      ENDIF
      iter = 0
 100  Continue
      Z1 = Z
      REFDO = DownstreamLevel - Z      ! H2
      REFUP = UpstreamLevel - Z      ! H1
      IF (iDebug .ne. 0) WRITE(IDEBUG,*) ' RefDo = ',RefDo, ' RefUp=',RefUp
      IF (iDebug .ne. 0) WRITE(IDEBUG,*) ' C     = ',C    , ' Gcoef=',GCoef
      IF (iDebug .ne. 0) WRITE(IDEBUG,*) ' Vslope= ',Vslope
      ActualRatio = 0.
      If (RefUp .gt. 0) then
          ActualRatio = Refdo / RefUp
          CheckFlow = C * GCOEF * VSlope * ( (RefUp) ** U )
          Z = UpstreamLevel - ( CheckFlow / ( C * GCOEF * VSlope )) ** ( 1/U )
      Endif
!     Test of ratio >0 is (Ifreal geeft 1 als resultaat)
      If (IfReal(ActualRatio, 0.0, 1E-6) .eq. 1) then
          If (ActualRatio .ge. 1) then
              ActualReductionFactor = 0.0
!  VShapeWeirH2H1ratio should be in increasing order
          Elseif (ActualRatio .le. VShapeWeirH2H1Ratio(1)) then
              ActualReductionFactor = 1.0
          Else
            Call RR_INTERP (NrVShapeWeirReductionPoints, VShapeWeirH2H1Ratio, VshapeWeirFlowReductionFactor, &
                            ActualRatio, ActualReductionFactor, idum)
            if (idebug .ne. 0) then
               write(Idebug,*) ' idum=',idum,' ActualRatio=',ActualRatio
               write(Idebug,*) ' reduction factor ', ActualReductionFactor
            endif
            If (Idum .eq. 0) then
              ActualReductionFactor = 1.
            Elseif (Idum .gt. NrVShapeWeirReductionPoints) then
              ActualReductionFactor = VShapeWeirFlowReductionFactor(NrVShapeWeirReductionPoints)
            Endif
          Endif
          IF (iDebug .ne. 0) then
             write(Idebug,*) ' Vnotch ratio, reduction factor ', ActualRatio, ActualReductionFactor, Qstru(istr)
          Endif
          ActualReductionFactor = min (1.0, ActualReductionFactor)
          ActualReductionFactor = max (0.0, ActualReductionFactor)
          CheckFlow = ActualReductionFactor * CheckFlow
          IF (iDebug .ne. 0) then
             write(Idebug,*) ' After correction reduction factor ', ActualReductionFactor, CheckFlow
          Endif
          If (iter .le. 1) goto 100
          If (abs(CheckFlow-ActualFlow) .gt. 1E-3) then
            If (abs(Z1-Z) .gt. 1E-3 .and. iter .lt. 5) goto 100
          endif
      Endif

    Else
!      rechthoekige stuw
    ! Free weir flow case
!      ARS 14739  consistent use of Actual flow; in some cases due to lumping of timesteps it could be the case that
!                                                the actual flow is negative while last upstreamLevel>DownstreamLevel
!      If (UpstreamLevel .gt. DownstreamLevel) then
       If (ActualFlow .ge. 0.0) then
            Z  = UpstreamLevel - ( ActualFlow / ( C * WIDTH * GCOEF ) ) ** (1/U)
       Else
            Z  = DownstreamLevel - ( Abs(ActualFlow) / ( C * WIDTH * GCOEF ) ) ** (1/U)
       ENDIF
       ZFree = Z
       if (idebug .ne. 0) write(idebug,*) ' free weir flow results in z=', Zfree

    ! Check Drowned weir flow
       iter = 0
 101   Continue
       Z1 = Z
       If (NegativeFlow) then
         REFDO = UpstreamLevel - Z
         REFUP = (DownstreamLevel - Z) * 2./3.
       else
         REFDO = DownstreamLevel - Z
         REFUP = (UpstreamLevel - Z) * 2./3.
       Endif
       IF (REFDO .GT. REFUP .AND. REFDO .GT. .0) THEN
          U      = 0.5
          DELTAH = abs(UpstreamLevel - DownstreamLevel)
          Y      = REFDO
          if (idebug .ne. 0) write(idebug,*) ' drowned weir flow iteration=, z=,y= ',iter, Z, Y
!         ARS 14102: prevent division by zero
          If (Abs(Deltah) .le. 1.0E-6) then
             If (DeltaH .ge. 0) then
               DeltaH= 1.0E-6
             else
               DeltaH= -1.0E-6
             endif
          else
             Y2 = Abs(ActualFlow) / ( C * WIDTH * ( (2*G * DELTAH) ** U ) )
          Endif
          if (DrownedWeirDepth .eq. 0) then
             Z = max(UpstreamLevel,DownstreamLevel)-Y2
          elseif (DrownedWeirDepth .eq. 1) then
             Z = min(DownstreamLevel,UpstreamLevel)-Y2
          else
             Z = 0.5 * (UpstreamLevel + DownstreamLevel) - Y2
          endif
          iter = iter+1
          if (idebug .ne. 0) write(idebug,*) ' drowned weir flow iteration results iter z y2= ',iter, Z, Y2
          If (iter .le. 1) goto 101
          If (abs(Z1-Z) .gt. 1E-3 .and. iter .lt. 5) goto 101
       Elseif (Iter .ge. 1) then
          ! minstens 1 iteratie drowned flow gedaan, en nu weer terug naar free flow?
          ! neem maar gemiddelde van de free weir en drowned weir crest levels
          if (idebug .ne. 0) write(idebug,*) ' using average of Zfree and Z', Zfree, Z
          Z = (Zfree + Z) / 2
       ENDIF
    Endif

    ActCrestLevel(istr) = z

    Return
    End subroutine ComputeActCrestLevel



    Subroutine ComputeInletPump (Istr, StartLevelLow, StartLevelHigh, StopLevelLow, StopLevelHigh, &
                                 Dlvl, Dlvl0, Vol0, HVol, Hvol2, QCap, QCapController, FractionTime, ReductionFactor)

    ! *********************************************************************
    ! *** Last update:  Jan 1996
    ! *********************************************************************
    ! *** Brief description:
    ! *** ------------------
    ! ***    This subroutine performs computations of pump
    ! *********************************************************************
    ! *** Input/output parameters:
    ! *** ------------------------
    ! ***  IEVENT = event number
    ! ***  ITMSTP = timestep number
    ! ***  IDEBUG = file unit number of debug file
    ! ***  IOUT1  = file unit number of output file with run messages
    ! ***  ISTR   = intern structure nr
    ! ***  IMETEO = meteostation code
    ! ***  INODE  = knoopnr
    ! *********************************************************************
    ! *** Berekeningen voor gemaal
    ! *********************************************************************


    Integer iStr
    Real StopLevelLow, StopLevelHigh, StartLevelLow, StartLevelHigh, &
         Dlvl, Dlvl0, Vol0, Hvol, Hvol2, QCap, QCapController
    Real ReductionFactor

    Integer iDebug, Iout1

    Real    Qin, Qin1, Qover, TmFulC, FractionTime

    iOut1  = ConfFil_get_iOut1 ()
    iDebug = ConfFil_get_iDebug()

        !Operatie gemaal met 2 aanslag/afslagpeilen
          IF ((DLVL .LT. startLevelLow) .AND. (DLVL .LT. startLevelHigh) ) THEN
        ! gemaal slaat aan, hoog toeren
             If (Dlvl0 .gt. startLevelLow .and. StructureOperation .ne. 0) &
                                            Call SetFractionTime (startlevelLow, Dlvl, Dlvl0, FractionTime,2)
             STRSTA(ISTR) = 2
             QIN  = (HVOL2 - VOL0) / timeSettings%timestepSize
             QIN  = MAX (QIN, 0.0)

             QCAP = pumpCapacities(iStr)%low + pumpCapacities(iStr)%high ! was STRPAR(ISTR,5)+STRPAR(ISTR,10)
             QCap = Min (QCap, QCapController)
             QCAP = QCAP * FractionTime
             QCAP = QCAP * ReductionFactor

             IF (QIN .GT. QCAP) THEN
                QIN  = QCAP
             ELSE
        !       extra inlaten laag toeren naar HVOL; TMFULC = tijd volle capaciteit
                QIN1   = (HVOL - HVOL2) / timeSettings%timestepSize
                QOVER  = MIN (QCap, pumpCapacities(iStr)%low*ReductionFactor, QIN1)
                TMFULC = QIN / QCAP * timeSettings%timestepSize
                QIN    = (QCAP*TMFULC + &
                          QOVER*(timeSettings%timestepSize-TMFULC))/ timeSettings%timestepSize
             ENDIF
          ELSEIF ( DLVL .LT. startLevelLow .AND.  DLVL .GT. startLevelHigh ) THEN
            IF ( (STRST0(ISTR) .LE. 1) .OR. &
                   (STRST0(ISTR) .EQ. 2 .AND. &
                     DLVL .GT. stopLevelHigh) )  THEN
        ! gemaal slaat aan, laag toeren; of switcht van hoog naar laag
               If (Dlvl0 .gt. startLevelLow .and. StrSt0(istr) .eq. 0 .and. StructureOperation .ne. 0) &
                                             Call SetFractionTime (startlevelLow, Dlvl, Dlvl0, FractionTime,2)
               STRSTA(ISTR) = 1
               QIN = (HVOL - VOL0) / timeSettings%timestepSize
               QIN = MAX (QIN , 0.0)
               QIN = MIN (QIN , min(QCapController, pumpCapacities(iStr)%low) * ReductionFactor * FractionTime)
            ELSEIF (STRST0(ISTR) .EQ. 2) THEN
        ! gemaal blijft aan, hoog toeren
              STRSTA(ISTR) = 2
              QIN  = (HVOL2 - VOL0) / timeSettings%timestepSize
              QIN  = MAX (QIN, 0.0)

              QCAP = pumpCapacities(iStr)%low + pumpCapacities(iStr)%high ! was STRPAR(ISTR,5)+STRPAR(ISTR,10)
              QCap = Min (QCap, QCapController) * ReductionFactor

              IF (QIN .GT. QCAP) THEN
                 QIN   = QCAP
              ELSE
        !       extra inlaten op laag toeren naar HVOL, mits nodig
                QIN1  = (HVOL - HVOL2) / timeSettings%timestepSize
                QOVER = MIN (pumpCapacities(iStr)%low*ReductionFactor, QCapController*ReductionFactor, QIN1)
                TMFULC = QIN / QCAP * timeSettings%timestepSize
                QIN    = (QCAP*TMFULC + &
                          QOVER*(timeSettings%timestepSize-TMFULC))/ timeSettings%timestepSize
              ENDIF
            ENDIF
          ELSEIF ( DLVL .GT. stopLevelLow .AND. DLVL .GT. stopLevelHigh ) THEN
        ! gemaal slaat af
             FractionTime = 0.
             If (Dlvl0 .lt. stopLevelLow .and. StructureOperation .ne. 0) &
                                               Call SetFractionTime (stoplevelLow, Dlvl, Dlvl0, FractionTime,1)
             STRSTA(ISTR) = 0
           ! QIN  = 0
             QIN  = min (QCapController,pumpCapacities(iStr)%low) * FractionTime * ReductionFactor
          ELSEIF (STRST0(ISTR) .EQ. 2 .AND. DLVL .GT. stopLevelHigh ) THEN
        ! gemaal switcht van hoog naar laag toeren
               STRSTA(ISTR) = 1
               QIN  = (HVOL - VOL0) / timeSettings%timestepSize
               QIN  = MAX (QIN, 0.0)
               QIN  = MIN (QIN, pumpCapacities(iStr)%low * ReductionFactor, QCapController * ReductionFactor )
          ELSEIF (STRST0(ISTR) .EQ. 2) THEN
        ! gemaal blijft aan, hoog toeren
             STRSTA(ISTR) = 2
             QIN  = (HVOL2 - VOL0) / timeSettings%timestepSize
             QIN  = MAX (QIN, 0.0)

             QCAP = pumpCapacities(iStr)%low + pumpCapacities(iStr)%high ! was STRPAR(ISTR,5)+STRPAR(ISTR,10)
             QCap = Min (QCap, QCapController)  * ReductionFactor

             IF (QIN .GT. QCAP) THEN
                QIN  = QCAP
             ELSE
        !       extra uitmalen op laag toeren naar HVOL
                QIN1 = (HVOL2 - HVOL) / timeSettings%timestepSize
                QOVER = MIN (pumpCapacities(iStr)%low * ReductionFactor, QCapController * ReductionFactor, QIN1)
                TMFULC = QIN / QCAP * timeSettings%timestepSize
                QIN    = (QCAP*TMFULC + &
                          QOVER*(timeSettings%timestepSize-TMFULC))/ timeSettings%timestepSize
             ENDIF
          ELSEIF (STRST0(ISTR) .EQ. 1) THEN
        ! gemaal blijft aan, laag toeren
             STRSTA(ISTR) = 1
             QIN = (HVOL - VOL0) / timeSettings%timestepSize
             QIN = MAX (QIN, 0.0)
             QIN = MIN (QIN, pumpCapacities(iStr)%low * ReductionFactor, QCapController * ReductionFactor)
          ELSEIF (STRST0(ISTR) .EQ. 0) THEN
        ! gemaal blijft uit
             STRSTA(ISTR) = 0
             QIN = 0
          ENDIF

          QSTRU(ISTR) = QIN
         ! structure with time controller, check status for switch off
         If (STRPAR(ISTR,19) .eq. 5.0 .and. Qstru(Istr) .lt. QCapController * ReductionFactor) StrSta(Istr) = 0

    RETURN
  END subroutine ComputeInletPump


    Subroutine ComputePump (Istr, StartLevelLow, StartLevelHigh, StopLevelLow, StopLevelHigh, &
                            Dlvl, Dlvl0, Vol0, HVol, Hvol2, QCap, QCapController, FractionTime, ReductionFactor)

    ! *********************************************************************
    ! *** Last update:  Jan 1996
    ! *********************************************************************
    ! *** Brief description:
    ! *** ------------------
    ! ***    This subroutine performs computations of pump
    ! *********************************************************************
    ! *** Input/output parameters:
    ! *** ------------------------
    ! ***  IEVENT = event number
    ! ***  ITMSTP = timestep number
    ! ***  IDEBUG = file unit number of debug file
    ! ***  IOUT1  = file unit number of output file with run messages
    ! ***  ISTR   = intern structure nr
    ! ***  IMETEO = meteostation code
    ! ***  INODE  = knoopnr
    ! *********************************************************************
    ! *** Berekeningen voor gemaal
    ! *********************************************************************


    Integer iStr
    Real StopLevelLow, StopLevelHigh, StartLevelLow, StartLevelHigh, &
         Dlvl, Dlvl0, Vol0, Hvol, Hvol2, QCap, QCapController
    Real ReductionFactor

    Integer iDebug, Iout1

    Real    Qout, QOut1, Qover, TmFulC, FractionTime

    iOut1  = ConfFil_get_iOut1 ()
    iDebug = ConfFil_get_iDebug()


        !Operatie gemaal met 2 aanslag/afslagpeilen
         IF ((DLVL .GT. startLevelLow) .and. (DLVL .GT. startLevelHigh)) then
           ! gemaal slaat aan, hoog toeren
            If (Dlvl0 .lt. startLevelLow .and. StructureOperation .ne. 0) &
                                           Call SetFractionTime (startlevelLow, Dlvl, Dlvl0, FractionTime,2)
            IF (iDebug .ne. 0) WRITE(IDEBUG,*) ' Case 1', FractionTime
            STRSTA(ISTR) = 2
            QOUT  = (VOL0 - HVOL2) / timeSettings%timestepSize
            QOUT  = MAX (QOUT, 0.0)

            QCAP = pumpCapacities(iStr)%low + pumpCapacities(iStr)%high ! was STRPAR(ISTR,5)+STRPAR(ISTR,10)
            QCap = Min (QCap, QCapController)
            QCAP = QCAP * FractionTime
            QCAP = QCAP * ReductionFactor

            IF (QOUT .GT. QCAP) THEN
               QOUT  = QCAP
            ELSE
       !       extra uitmalen op laag toeren naar HVOL
               QOUT1 = (HVOL2 - HVOL) / timeSettings%timestepSize
               QOVER = MIN (QCap, pumpCapacities(iStr)%low * ReductionFactor, QOUT1)
       !       TMFULC = tijd volle capaciteit
               TMFULC = QOUT / QCAP * timeSettings%timestepSize
               QOUT   = (QCAP*TMFULC + &
                         QOVER*(timeSettings%timestepSize-TMFULC)) / timeSettings%timestepSize
            ENDIF
         ELSEIF ( (DLVL .GT. startLevelLow) .and. &  ! was STRPAR(ISTR,INDX3) .AND. &
                  (DLVL .LT. startLevelHigh)) then ! was STRPAR(ISTR,INDX4) ) THEN
           IF ( (STRST0(ISTR) .LE. 1) .OR. &
                  (STRST0(ISTR) .EQ. 2) .AND. &
                    (DLVL .LT. stopLevelHigh)) then ! was STRPAR(ISTR,INDX2)) )  THEN
       ! gemaal slaat aan, laag toeren; of switcht van hoog naar laag
              If (Dlvl0 .lt. startLevelLow .and. StrSt0(istr) .eq. 0 .and. StructureOperation .ne. 0) &
                                            Call SetFractionTime (startlevelLow, Dlvl, Dlvl0, FractionTime,2)
              IF (iDebug .ne. 0) WRITE(IDEBUG,*) ' Case 2', FractionTime
              STRSTA(ISTR) = 1
              QOUT = (VOL0 - HVOL) / timeSettings%timestepSize
              QOUT = MAX (QOUT, 0.0)
              QOUT = MIN (QOUT, Min(QCapController, pumpCapacities(iStr)%low) * FractionTime  * ReductionFactor)! was STRPAR(ISTR,5))
           ELSEIF (STRST0(ISTR) .EQ. 2) THEN
       ! gemaal blijft aan, hoog toeren
             IF (iDebug .ne. 0) WRITE(IDEBUG,*) ' Case 3'
             STRSTA(ISTR) = 2
             QOUT  = (VOL0 - HVOL2) / timeSettings%timestepSize
             QOUT  = MAX (QOUT, 0.0)

             QCAP = pumpCapacities(iStr)%low + pumpCapacities(iStr)%high ! was STRPAR(ISTR,5)+STRPAR(ISTR,10)
             QCap = Min (QCap, QCapController)  * ReductionFactor

             IF (QOUT .GT. QCAP) THEN
                QOUT  = QCAP
             ELSE
       !       extra uitmalen op laag toeren naar HVOL
               QOUT1 = (HVOL2 - HVOL) / timeSettings%timestepSize
               QOVER = MIN (pumpCapacities(iStr)%low * ReductionFactor, QCapController * ReductionFactor, QOUT1)
               TMFULC = QOUT / QCAP * timeSettings%timestepSize
               QOUT   = (QCAP*TMFULC + &
                         QOVER*(timeSettings%timestepSize-TMFULC))/ timeSettings%timestepSize
             ENDIF
           ENDIF
         ELSEIF ( (DLVL .LT. stopLevelLow) .and. & ! was STRPAR(ISTR,INDX1) .AND. &
                    (DLVL .LT. stopLevelHigh)) then ! was STRPAR(ISTR,INDX2) ) THEN
       ! gemaal slaat af
            FractionTime = 0.
            If (Dlvl0 .gt. stopLevelLow .and. StructureOperation .ne. 0) &
                                              Call SetFractionTime (stoplevelLow, Dlvl, Dlvl0, FractionTime,1)
            IF (iDebug .ne. 0) WRITE(IDEBUG,*) ' Case 4', FractionTime
            STRSTA(ISTR) = 0
            QOUT = min(QCapController,pumpCapacities(iStr)%low) * FractionTime  * ReductionFactor
          ! QOUT = 0
         ELSEIF ((STRST0(ISTR) .EQ. 2) .AND. &
                         (DLVL .LT. stopLevelHigh)) then ! was STRPAR(ISTR,INDX2) ) THEN
       ! gemaal switcht van hoog naar laag toeren
              IF (iDebug .ne. 0) WRITE(IDEBUG,*) ' Case 5'
              STRSTA(ISTR) = 1
              QOUT = (VOL0 - HVOL) / timeSettings%timestepSize
              QOUT = MAX (QOUT, 0.0)
              QOUT = MIN (QOUT, pumpCapacities(iStr)%low * ReductionFactor, QCapController * ReductionFactor)   ! was STRPAR(ISTR,5))
         ELSEIF (STRST0(ISTR) .EQ. 2) THEN
       ! gemaal blijft aan, hoog toeren
            IF (iDebug .ne. 0) WRITE(IDEBUG,*) ' Case 6'
            STRSTA(ISTR) = 2
            QOUT  = (VOL0 - HVOL2) / timeSettings%timestepSize
            QOUT  = MAX (QOUT, 0.0)

            QCAP = pumpCapacities(iStr)%low + pumpCapacities(iStr)%high ! was STRPAR(ISTR,5)+STRPAR(ISTR,10)
            QCap = Min (QCap, QCapController)  * ReductionFactor

            IF (iDebug .ne. 0) WRITE(IDEBUG,*) ' VOL0 HVOL2',VOL0,HVOL2
            IF (iDebug .ne. 0) WRITE(IDEBUG,*) ' Qout QCAP',QOUT, QCAP
            IF (QOUT .GT. QCAP) THEN
               QOUT  = QCAP
            ELSE
     !        extra uitmalen op laag toeren naar HVOL, mits afslagpeil laag toeren onder afslagpeil hoog toeren
               QOUT1 = (HVOL2 - HVOL) / timeSettings%timestepSize
               QOVER = MIN (pumpCapacities(iStr)%low * ReductionFactor, QCapController * ReductionFactor, QOUT1)
               TMFULC = QOUT / QCAP * timeSettings%timestepSize
               QOUT   = (QCAP*TMFULC + &
                       QOVER*(timeSettings%timestepSize-TMFULC))/ timeSettings%timestepSize
            ENDIF
         ELSEIF (STRST0(ISTR) .EQ. 1) THEN
       ! gemaal blijft aan, laag toeren
            IF (iDebug .ne. 0) WRITE(IDEBUG,*) ' Case 7'
            STRSTA(ISTR) = 1
            QOUT = (VOL0 - HVOL) / timeSettings%timestepSize
            QOUT = MAX (QOUT, 0.0)
            QOUT = MIN (QOUT, pumpCapacities(iStr)%low * ReductionFactor, QCapController * ReductionFactor) ! was STRPAR(ISTR,5))
         ELSEIF (STRST0(ISTR) .EQ. 0) THEN
       ! gemaal blijft uit
            IF (iDebug .ne. 0) WRITE(IDEBUG,*) ' Case 8'
            STRSTA(ISTR) = 0
            QOUT = 0
         ENDIF

         QSTRU(ISTR) = QOUT
         ! structure with time controller, check status for switch off
         If (STRPAR(ISTR,19) .eq. 5.0 .and. Qstru(Istr) .lt. QCapController * ReductionFactor) StrSta(Istr) = 0

    RETURN
  END subroutine ComputePump



  Subroutine ComputeManningFormula (ManningCoefficient, CharacteristicLength, BottomWidth, Helling, &
                                    Istr, Inode, UpLvt, DoLvT, PeilBottomUp, PeilBottomDown)

    ! *********************************************************************
    ! *** Berekeningen voor Manning Friction
    ! *********************************************************************

    Real UpLvt, DoLvt, PeilBottomUp, peilBottomDown, CharacteristicLength
    Real ManningCoefficient, Slope, PeilBo, BottomWidth, Helling, AvPeil, Width, WetPerimeter
    Real cArea, Radius, Expo !upLvT0, doLvT0

    Integer iDebug, Iout1, Istr, Inode
    Character(Len=80) String

    iOut1  = ConfFil_get_iOut1 ()
    iDebug = ConfFil_get_iDebug()

    IF (iDebug .ne. 0)  WRITE(IDEBUG,*) 'ComputeManning'

! Input data
!       ManningCoefficient = STRPAR(ISTR,1)
!       WIDBOT = STRPAR(ISTR,4)
!       HELLI  = STRPAR(ISTR,5)

        SLOPE  = (UPLVT-DOLVT) / CharacteristicLength
    !nieuwe invoer ipv bodempeil: diepte bij bovenstrooms streefpeil(ref.peil)
    !conversie naar bodempeil: bovenstrooms ref.peil - diepte

        peilBo = 0.5 * (peilBottomUp + peilBottomDown)! was max(peilBottomUp, peilBottomDown)

    ! check of openwaterpeilen niet onder bodempeilen zakken
        if (upLvT < peilBottomUp) upLvt = peilBottomUp
        if (doLvT < peilBottomDown) doLvt = peilBottomDown

    ! bepaling natte doorsnede
        AVPEIL = 0.5 * (UPLVT + DOLVT)

! Test Mei 2004
! Alles op basis bovenstrooms peil en doorsnede, bovenstrooms op basis van de actuele peilen !!!
!       If (Slope .gt. 0) then
!         PeilBo = PeilBottomUp
!         AvPeil = UpLvt
!       Else
!         PeilBo = PeilBottomDown
!         AvPeil = DoLvt
!       Endif

    ! bepaal diepte-gemiddelde breedte
    ! ** als helli=0 ==> rechte bak, dus constante breedte
    ! ** als helli=1 ==> 45% hoek, opp.breedte = bodembreedte + 2 * ...
    !                              dus gem.breedte = bodembreedte + 1 * ...
        WIDTH  = BottomWidth + Helling * (AVPEIL-PEILBO)
        CAREA  = WIDTH  * (AVPEIL-PEILBO)
! ARS 14910 Henk Roskam
        CAREA  = max (0.0, CArea)
    ! bepaal natte omtrek
        WetPerimeter = BottomWidth + 2. * (AVPEIL-PEILBO) * SQRT(1+Helling**2.)
        IF (iDebug .ne. 0) THEN
           WRITE(IDEBUG,*) ' slope   ', SLOPE
           WRITE(IDEBUG,*) ' Uplvl   ', UPLVT
           WRITE(IDEBUG,*) ' Dolvl   ', DOLVT
           WRITE(IDEBUG,*) ' avg.level', AVPEIL
           WRITE(IDEBUG,*) ' avg.width ',WIDTH
           WRITE(IDEBUG,*) ' cross sectie ', CAREA
           WRITE(IDEBUG,*) ' wet perimeter', WetPerimeter
           write(iDebug,*) ' hydr. radius', cArea/wetPerimeter
        ENDIF

        RADIUS = CAREA / WetPerimeter
        EXPO   = 2./3.

        IF (RADIUS .LT. 0) then
            Write(Iout1,*) ' Node id = ',Id_Nod(INODE)
            call ErrMsgStandard (935, EiNode(INODE,1), ' CMPSTR', ' ')
        Endif
        IF (SLOPE  .LT. 0) THEN
           SLOPE = -1*SLOPE
           STRING = ' Slope becomes negative'
        ENDIF

        QSTRU(ISTR) = CAREA * ( RADIUS ** EXPO) * ( SLOPE ** 0.5) / ManningCoefficient
        IF (iDebug .ne. 0) THEN
          WRITE(IDEBUG,*) ' WetPerimeter   ', WetPerimeter
          WRITE(IDEBUG,*) ' RADIUS ** 2/3  ', RADIUS ** EXPO
          WRITE(IDEBUG,*) ' SLOPE  ** 0.5  ', SLOPE ** 0.5
        ENDIF
    ! check dat downstream peil niet hoger is dan upstream peil,
    ! want dan gaat Manning met bovenstroomse peilen/volumina niet op!
    ! draai dan teken om
        IF (AVPEIL .GT. UPLVT) THEN
           STRING = ' Upstream level lower then downstream level. Manning flow becomes negative'
           QSTRU(ISTR) = -1. * QSTRU(ISTR)
        ENDIF
        IF (iDebug .ne. 0) WRITE(IDEBUG,*) UPLVT,DOLVT, QSTRU(ISTR)

    RETURN
  END subroutine ComputeManningFormula



    SUBROUTINE CMPOND (ISTR, INODE, IOW, IOWD, IBNDUP, IBND, &
                     NODOWN, DOLV0, DOVOL, TQINDW, UpArea, DoArea, &
                     UPLVL, TQIN, NODEUP, peilBottomDown, peilBottomUp)
    ! *********************************************************************
    ! *** Last update:  Jan 1996
    ! *********************************************************************
    ! *** Brief description:
    ! *** ------------------
    ! ***    This subroutine performs onderlaat computations
    ! *********************************************************************
    ! *** Input/output parameters:
    ! *** ------------------------
    ! ***  IEVENT = event number
    ! ***  ITMSTP = timestep number
    ! ***  IDEBUG = file unit number of debug file
    ! ***  IOUT1  = file unit number of output file with run messages
    ! ***  ISTR   = intern structure nr
    ! ***  IMETEO = meteostation code
    ! ***  INODE  = knoopnr
    ! *********************************************************************
    ! *** Berekeningen voor onderlaten
    ! *********************************************************************

    ! constants
    ! versnelling van de zwaartekracht (9.81 m/s2)

    Real, parameter :: G = 9.81


    Integer iStr, iOw, iOWd, iBndUp, iBnd, inode, noDown, NodeUp, itl
    Real doLv0, doVol, tqInDw, upLvl, tqIn, upLvT, Vol0, doLvT
    Real Delta_volume, peilBottomDown, peilBottomUp, UpArea, DoArea

    Real c, dc, u, y,  width, upLv2, doLv2, deltaH, upLvt0, doLvt0, depth, crestlvl
    Integer, PARAMETER :: NTIML2 = 60
    REAL      QTEMP(NTIML2)
    Integer nTimL, nTimLc, nTimLc_Old

    logical DateTimeOutsideTable
    Integer RowNr, TabelNr
    type (Date) currentDate
    type (Time) currentTime

    Integer iDebug, Iout1

    Real    PeilArrayUp(6), AreaArrayUp(6), VolumeArrayUp(6)  ! aanname dat NVAL=6 zoals in Conf_Arr.
    Real    PeilArrayDown(6), AreaArrayDown(6), VolumeArrayDown(6)  ! aanname dat NVAL=6 zoals in Conf_Arr.
    integer i

    if (iow .gt. 0) then
        Do i=1,NVal
           PeilArrayUp(i)   = PeilOw(i,iow)
           AreaArrayUp(i)   = AreaOw(i,iow)
           VolumeArrayUp(i) = VoluOw(i,iow)
        Enddo
    endif
    if (iowd .gt. 0) then
        Do i=1,NVal
           PeilArrayDown(i)   = PeilOw(i,iowd)
           AreaArrayDown(i)   = AreaOw(i,iowd)
           VolumeArrayDown(i) = VoluOw(i,iowd)
        Enddo
    endif

    iOut1  = ConfFil_get_iOut1 ()
    iDebug = ConfFil_get_iDebug()

    IF (iDebug .ne. 0)  WRITE(IDEBUG,*) 'CMPOND istr=',ISTR
    NTIML = MIN (NVAL2, NTIML2, 6)
    IF (timeSettings%timestepSize/NTIML .LT. NRSMIN) &
      NTIML = timeSettings%timestepSize / NRSMIN

    ! *********************************************************************
    ! *** Structure type 3 = onderlaat; type 7= inlaat-onderlaat
    ! *********************************************************************

      IF (iDebug .ne. 0) WRITE(IDEBUG,*) ' IOW IOWD', IOW, IOWD
      IF (iDebug .ne. 0) WRITE(IDEBUG,*) ' IBND IBNDUP', IBND, IBNDUP
      NTIMLC = MAX (NTIML, 1)
203   CONTINUE
      UPLVT  = UPLVL
      IF (IOW .GT. 0) THEN
         Call RR_INTERP (NVAL, PeilArrayUp, VolumeArrayUp, UPLVL, VOL0, OwLastInterpIndex(iow))
         Call RR_INTERP (NVAL, PeilArrayUp, AreaArrayUp, UPLVL, UpArea, OwLastInterpIndex(iow))
      ENDIF
      DOLVT  = DOLV0
      DOLVT0 = DOLVT
      IF (IOWD .GT. 0) THEN
      !IOWD ipv IOW; bug inlaatonderlaat gevonden door som Rijnland KJ
         Call RR_INTERP (NVAL, PeilArrayDown, AreaArrayDown, DOLVT, DoArea, OwLastInterpIndex(iowd))
      ENDIF


      DO ITL = 1,NTIMLC

        ! C = afvoer coefficient mu
        ! W = breedte onderlaat
        ! U = Exponent (standaard 0.5)
        ! Y = openingshoogte doorlaat
        ! DC= discharge coefficient
        C     = STRPAR(ISTR,1)
        WIDTH = STRPAR(ISTR,2)
        U     = STRPAR(ISTR,5)
        Y     = STRPAR(ISTR,4) - STRPAR(ISTR,3)
!ARS 2746
        DC    = STRPAR(IStr,6)

        ! Check for Time controller for opening height
        IF ( (STRPAR(ISTR,8) .eq. 5.0 .AND. STRTYP(ISTR) .EQ. 3) .or. &
               (STRPAR(ISTR,11) .eq. 5.0 .AND. STRTYP(ISTR) .EQ. 7)) THEN
             currentDate%year  = ConfArr_get_iYear()
             currentDate%month = ConfArr_get_iMonth()
             currentDate%day   = ConfArr_get_iDay()
             currentTime%hour  = ConfArr_get_iHour()
             currentTime%minute = ConfArr_get_iMinute()
             currentTime%second = 0
             RowNr = -1
             TabelNr = StrRefTimeTable (iStr)
             If (TabelNr .gt. 0) then
                 Y = GetNewValue(TableHandle, TabelNr, 1, RowNr, CurrentDate, CurrentTime, &
                                 Idebug, Iout1, DateTimeOutsideTable, .true. )
             Endif
             if (idebug .ne. 0) Write(Idebug,*) ' CheckTimeControllerGate Y=',Y
        Endif
! ARS 14732/14733: opening height should be positive!
        Y     = Max (0.0, Y)

    !   Uplv2 = upstream level tov bodem onderlaat
    !   als gate opening Y groter is dan 2/3 * peilen, dan weir flow
        UPLV2 = UPLVT - STRPAR(ISTR,3)
        DoLv2 = DOLVT - STRPAR(ISTR,3)

!ARS 2746
! Bij Alle flow situaties rekening houden met de opgegeven discharge coefficient dc
!Nov 2001

        IF ( (Y .GT. 2./3. * UPLV2) .and. (Y .gt. 2./3. * DoLv2) ) THEN
          IF (iDebug .ne. 0) WRITE(IDEBUG,*) ' Switch to weir'
! ARS 2693: Weir flow, check of er free weir of drowned weir flow optreedt

          IF ((UpLV2 .GT. 3./2. * doLv2) .or. (DoLv2 .gt. 3./2. * UpLv2)) THEN
    !      vrije overlaat met default parameters (C=1.7,U=1.5)
           IF (UPLVT .GT. DOLVT) THEN
              IF (iDebug .ne. 0) WRITE(IDEBUG,*) ' Free weir'
              UPLV2 = MAX (1E-10, UPLV2)    ! aug 96: check op droogvallen    !ARS 8634 Dec2001: check op 1E-10 ipv 0
              IF (iDebug .ne. 0) WRITE(IDEBUG,*) ' UPLV2 = ', UpLv2
              QSTRU(ISTR) = 1.7 * WIDTH * (UPLV2 ** 1.5) * Dc
           ELSE
              IF (iDebug .ne. 0)  WRITE(IDEBUG,*) ' Weir flow <0, Uplvl<Dolvl'
              DOLV2 = MAX (1E-10, DOLV2)  ! aug 96: check op droogvallen
              QSTRU(ISTR) = -1.7 * WIDTH * (DOLV2 ** 1.5) * Dc
           ENDIF
          ELSE
    !       drowned weir flow met default parameters (C=1.0,U=0.5)
             IF (iDebug .ne. 0) WRITE(IDEBUG,*) ' Drowned weir'
             DELTAH = UPLVT - DOLVT
             UPLV2 = MAX (0.0, UPLV2)
             DOLV2 = MAX (0.0, DOLV2)
             IF (UPLVT .GE. DOLVT) THEN
                If (DrownedWeirDepth .eq. 0) then      ! kleinste diepte nemen
                   Depth = DoLv2
                Elseif (DrownedWeirDepth .eq. 1) then  ! grootste diepte
                   Depth = UpLv2
                Else  ! gemiddelde diepte
                   Depth = 0.5 * (DoLv2+UpLv2)
                Endif
! ARS 3479 drowned weir flow: natte doorsnede (Width*depth) max. het oppervlak van de onderlaat, dus diepte <= openingshoogte
                Depth = Min (Depth, Y)
! eerst op nul zetten, alleen bij deltah > 0 machtsverheggen
                QSTRU(ISTR) = 0.0
                If (Deltah .GT. 0)  QSTRU(ISTR) = 1.0 * WIDTH * Depth * ( ABS (2*G*Deltah) ** 0.5) * Dc
             Else
                If (DrownedWeirDepth .eq. 0) then      ! kleinste diepte nemen
                   Depth = UpLv2
                Elseif (DrownedWeirDepth .eq. 1) then  ! grootste diepte
                   Depth = DoLv2
                Else ! gemiddelde diepte
                   Depth = 0.5 * (DoLv2+UpLv2)
                Endif
! ARS 3479 drowned weir flow: natte doorsnede (Width*depth) max. het oppervlak van de onderlaat, dus diepte <= openingshoogte
                Depth = Min (Depth, Y)
! eerst op nul zetten, alleen bij deltah > 0 machtsverheggen
                QSTRU(ISTR) = 0.0
!fout           If (Deltah .GT. 0)  QSTRU(ISTR) = -1.0 * WIDTH * Depth * ( ABS (2*G*Deltah) ** 0.5) * Dc
                If (Deltah .LT. 0)  QSTRU(ISTR) = -1.0 * WIDTH * Depth * ( ABS (2*G*Deltah) ** 0.5) * Dc
             Endif
          ENDIF

        ELSE
          IF (iDebug .ne. 0) WRITE(IDEBUG,*) ' Gate flow, upstream level > downstream'
          IF (UPLVT .GT. DOLVT) THEN
    !       gate flow: onderlaat formules met upstream level hoger dan downstream
            IF ( (Y .LT. 2./3. * UPLV2) .and. DOLVT .GT. STRPAR(ISTR,3)+Y) THEN
    !          drowned gate flow, Upstream level hoger dan downstream leel
    !          correctie van DELTAH en C
               IF (iDebug .ne. 0) WRITE(IDEBUG,*) ' Drowned gate'
               IF (iDebug .ne. 0) WRITE(IDEBUG,*) ' C_0 = ',C
               DELTAH = UPLVT - DOLVT
               C      = C * ((UPLV2-STRPAR(ISTR,1)*Y)/(UPLV2-Y))**U
            ELSEIF ( (Y .LT. 2./3. * UPLV2) .and. DOLVT .LE. STRPAR(ISTR,3)+Y) THEN
    !         free gate flow, upstream level hoger dan downstream level
              IF (iDebug .ne. 0) WRITE(IDEBUG,*) ' Free gate'
              DELTAH = UPLV2 - STRPAR(ISTR,1) * Y
            ENDIF
          ELSE
            IF (iDebug .ne. 0) WRITE(IDEBUG,*) ' Gate flow, upstream level < downstream'
    !       gate flow: downstream level hoger
            IF ( (Y .LT. 2./3. * DOLV2) .and. UPLVT .GT. STRPAR(ISTR,3)+Y) THEN
    !         drowned gate flow,
    !         correctie van DELTAH en C
               IF (iDebug .ne. 0) WRITE(IDEBUG,*) ' Drowned gate'
               IF (iDebug .ne. 0) WRITE(IDEBUG,*) ' C_0 = ',C
               DELTAH = UPLVT - DOLVT
               C      = C * ((DOLV2-STRPAR(ISTR,1)*Y)/(DOLV2-Y))**U
            ELSEIF ( (Y .LT. 2./3. * DOLV2) .and. UPLVT .LE. STRPAR(ISTR,3)+Y) THEN
    !         free gate flow
              IF (iDebug .ne. 0) WRITE(IDEBUG,*) ' Free gate'
              DELTAH = -1. * (DOLV2 - STRPAR(ISTR,1) * Y)
            ENDIF
          ENDIF
! invullen in formules
          IF (DELTAH .GT. 0) Then
             QSTRU(ISTR) = Dc * C * WIDTH * Y * ((2*G * DELTAH) ** U )
          ELSEIF (DELTAH .LT. 0) Then
             QSTRU(ISTR) = -Dc * C * WIDTH * Y * ((2*G * ABS(DELTAH)) ** U )
          ELSE
             QSTRU(ISTR) = 0.0
          ENDIF
        ENDIF

        IF (iDebug .ne. 0) THEN
          WRITE(IDEBUG,*) ' DELTAH         ', DELTAH
          WRITE(IDEBUG,*) ' UPLV2          ', UPLV2
          WRITE(IDEBUG,*) ' UPLVT          ', UPLVT
          WRITE(IDEBUG,*) ' DOLVT          ', DOLVT
          WRITE(IDEBUG,*) ' DOLV2          ', DOLV2
          WRITE(IDEBUG,*) ' Y              ', Y
          WRITE(IDEBUG,*) ' C              ', C
          WRITE(IDEBUG,*) ' DC             ', Dc
          WRITE(IDEBUG,*) ' Width          ', Width
        ENDIF

    !april96: check retourstroom
        IF (STRTYP(ISTR) .EQ. 3 .AND. STRPAR(ISTR,7) .EQ. 0) THEN
            QSTRU(ISTR) = MAX (0.0, QSTRU(ISTR))
            IF (iDebug .ne. 0) WRITE(IDEBUG,*) ' Correction backflow'
    ! jan 2004 added check for inlet gate
        ELSEIF (STRTYP(ISTR) .EQ. 7 .AND. STRPAR(ISTR,12) .EQ. 0) THEN
            QSTRU(ISTR) = MAX (0.0, QSTRU(ISTR))
            IF (iDebug .ne. 0) WRITE(IDEBUG,*) ' Correction backflow'
        ENDIF
    !end april96
   ! ARS 16175 check pos. flow possible:
        IF (STRTYP(ISTR) .EQ. 3 .AND. STRPAR(ISTR,9) .EQ. 0) THEN
            QSTRU(ISTR) = Min (0.0, QSTRU(ISTR))
            IF (iDebug .ne. 0) WRITE(IDEBUG,*) ' Correction pos.flow'
        ELSEIF (STRTYP(ISTR) .EQ. 7 .AND. STRPAR(ISTR,13) .EQ. 0) THEN
            QSTRU(ISTR) = Min (0.0, QSTRU(ISTR))
            IF (iDebug .ne. 0) WRITE(IDEBUG,*) ' Correction pos.flow'
        ENDIF

        IF (iDebug .ne. 0) WRITE(IDEBUG,*) UPLVT,DOLVT, QSTRU(ISTR), ' BACKFL', strpar(istr,7)

        crestlvl = StrPar(istr,3)
        Call VolumeCheck (ISTR, INODE, IBNDUP, IBND, iow, iowd, Qstru(Istr),&
                          NODOWN, NODEUP, peilBottomDown, peilBottomUp, &
                          dolvt, uplvt, Uparea, DoArea, NtimLc, 2, crestlvl, 0.0)

        QTEMP(ITL) = QSTRU(ISTR)
        IF (IOW .GT. 0) THEN
          VOL0 = VOL0 - QTEMP(ITL) * timeSettings%timestepSize / NTIMLC &
                      + TQIN * timeSettings%timestepSize / NTIMLC &
                      - QOWUPR(ISTR) * timeSettings%timestepSize / NTIMLC
          UPLVT0 = UPLVT
          Call RR_INTERP (NVAL, VolumeArrayUp, PeilArrayUp, VOL0, UPLVT, OwLastInterpIndex(iow))
    ! Jan 1996: UPLVT mag niet het maximum peil OWMNMA overschrijden
          UPLVT = MIN (UPLVT, OWMNMA(IOW))
        ELSEIF (IBNDUP .GT. 0) THEN
          UPLVT0 = UPLVT
    !oud  UPLVT  = UPLVL
        !Okt 97 bij randen check op peilvariatie via areaal
          Delta_volume = QTEMP(ITL)* timeSettings%timestepSize / NTIMLC
! Mar 2002 only change of UPLVT within timestep if switch CFBoundaryConstantInTimestep is not on
          UPLVT  = UPLVL
          if (CFBoundaryConstantInTimestep .eq. 0) then
            UPLVT  = UPLVL - Delta_volume / BNDPAR(ibndup,5)
          else
            UPLVT  = UPLVL
          endif
        ENDIF
    ! extra check of kleinere tijdstap nodig is
        IF (iDebug .ne. 0) then
           WRITE(IDEBUG,*) ' Test restart gate computations'
           WRITE(IDEBUG,*) ' Structcomp         ', StructComp
           WRITE(IDEBUG,*) ' abs (uplvt-uplvt0) ', Abs(Uplvt-Uplvt0)
           WRITE(IDEBUG,*) ' abs (uplvt0-dolvt) ', Abs(Uplvt0-dolvt)
           WRITE(IDEBUG,*) ' abs (dolvt-dolvt0) ', Abs(dolvt-dolvt0)
           WRITE(IDEBUG,*) ' ntimlc ntiml2      ', ntimlc, ntiml2
           WRITE(IDEBUG,*) ' timestep/ntimlc nrsmin', TimeSettings%TimestepSize/NtimLc, NrsMin
        Endif
        IF ( ( ABS(UPLVT - UPLVT0) .GT. ABS(UPLVT0-DOLVT) .OR. &
                ABS(UPLVT-UPLVT0) .GT. .05 .OR. &
                  ABS(DOLVT-DOLVT0) .GT. .05         )  .AND. &
               NTIMLC .NE. NTIML2 .AND. timeSettings%timestepSize/NTIMLC .GE. NRSMIN) THEN
           IF (iDebug .ne. 0) WRITE(IDEBUG,*) ' Restart gate computations'
! May 2002: StructComp backw. compatibility option also for Orifice
           NTimLc_old = NTimLc
           If (StructComp .eq. 1) then
              NTIMLC = Min (NTIML2, 2*TimeSettings%timestepSize/NrsMin)
           else
              NTIMLC = NTIML2
           Endif
           NTimLc = max (NTimLc, NTimLc_Old + 1)
           GOTO 203
        ENDIF
    ! evt. aanpassing benedenstrooms peil;
        IF (EiNode(NODOWN,3) .EQ. 4) THEN
           DOLVT0 = DOLVT
           DOVOL = DOVOL + TQINDW * timeSettings%timestepSize / NTIMLC &
                    + QOWDWR(ISTR) * timeSettings%timestepSize / NTIMLC &
                    + QTEMP(ITL) * timeSettings%timestepSize / NTIMLC &
                    - QOUT0(IOWD) * timeSettings%timestepSize / NTIMLC
           Call RR_INTERP (NVAL, VolumeArrayDown, PeilArrayDown, DOVOL, DOLVT, OwLastInterpIndex(iowd))
    ! Jan 1996: DOLVT mag niet het maximum peil OWMNMA overschrijden
           DOLVT = MIN (DOLVT, OWMNMA(IOWD))
        ELSEIF (EiNode(NODOWN,3) .EQ. 6) THEN
           DOLVT0 = DOLVT
           Delta_volume = QTEMP(ITL)* timeSettings%timestepSize / NTIMLC
! Mar 2002 only change of DOLVT within timestep if switch CFBoundaryConstantInTimestep is not on
           if (CFBoundaryConstantInTimestep .eq. 0) then
              DOLVT = DOLVT + Delta_volume / BNDPAR(ibnd,5)
           endif
        ENDIF
      ENDDO

      QSTRU(ISTR) = 0.0
      DO ITL=1,NTIMLC
         QSTRU(ISTR) = QSTRU(ISTR) + QTEMP(ITL)
      ENDDO
      QSTRU(ISTR) = QSTRU(ISTR)/NTIMLC

      ActCrestLevel(ISTR) = Y

    RETURN
  END subroutine cmpond


    SUBROUTINE VolumeCheck (ISTR, INODE, IBNDUP, IBND, iow, iowd, StrucFlow, &
                            NODOWN, NODEUP, peilBottomDown, peilBottomUp, &
                            dolvt, uplvt, Uparea, DoArea, NtimLc, Itype, crestlvl, q2)
    ! *********************************************************************
    ! *** Last update:  Jan 1996
    ! *********************************************************************
    ! *** Brief description:
    ! *** ------------------
    ! ***    This subroutine performs volume checks
    ! *********************************************************************
    ! *** Input/output parameters:
    ! *** ------------------------
    ! ***  IEVENT = event number
    ! ***  ITMSTP = timestep number
    ! ***  IDEBUG = file unit number of debug file
    ! ***  IOUT1  = file unit number of output file with run messages
    ! ***  ISTR   = intern structure nr
    ! ***  IMETEO = meteostation code
    ! ***  INODE  = knoopnr
    ! *********************************************************************
    ! *** Volume check voor kunstwerken (weir, orifice, manning)
    ! *** Itype = 1 weir
    ! ***         2 orifice
    ! ***         3 manning
    ! *********************************************************************
    ! ***  Crestlvl = crest level (if applicable)
    ! ***  q2       = flow over other part of same structure, e.g. in case of 2stage weir
    ! *********************************************************************

    Integer iStr, iBndUp, iBnd, inode, noDown, NodeUp, iow, iowd
    Real    upLvT, doLvT, StrucFlow, crestlvl
    Real    Delta_volume, MaxVolChk, peilBottomDown, peilBottomUp, &
            UpArea, DoArea, tmplvl, Tqin, MinVolOw, q2
    Integer Itype, idum

    Integer       nTimLc, iDebug
    CHARACTER(80) STRING
    Logical       CheckActive

    Real    PeilArrayUp(6), AreaArrayUp(6), VolumeArrayUp(6)  ! aanname dat NVAL=6 zoals in Conf_Arr.
    Real    PeilArrayDown(6), AreaArrayDown(6), VolumeArrayDown(6)  ! aanname dat NVAL=6 zoals in Conf_Arr.
    integer i

    if (iow .gt. 0) then
        Do i=1,NVal
           PeilArrayUp(i)   = PeilOw(i,iow)
           AreaArrayUp(i)   = AreaOw(i,iow)
           VolumeArrayUp(i) = VoluOw(i,iow)
        Enddo
    endif
    if (iowd .gt. 0) then
        Do i=1,NVal
           PeilArrayDown(i)   = PeilOw(i,iowd)
           AreaArrayDown(i)   = AreaOw(i,iowd)
           VolumeArrayDown(i) = VoluOw(i,iowd)
        Enddo
    endif

    iDebug = ConfFil_get_iDebug()
    CheckActive = .false.

! ARS xxxx: aug 2000; voorkom problemen met negatieve oppervlakken door lage open water peilen
    UpArea = max (0.0001, UpArea)
    DoArea = max (0.0001, DoArea)

    !okt97 check op max. volume (Delta_volume) bij openwater, randen
        IF (EiNode(NODOWN,3) .EQ. 6) THEN
        ! uitlaat, rand benedenstrooms
           if (idebug .ne. 0) write(idebug,*) ' outlet:',bndpar(ibnd,5), Uparea, dolvt, uplvT, PeilBottomDown
           Delta_volume = bndpar(ibnd,5) * (dolvt - peilBottomDown)
           if (uplvt .lt. dolvt) then      ! bovenstrooms ow peil beneden peil op rand, dus onttrekking aan CF
             Delta_volume = bndpar(ibnd,5) * (dolvt - max (uplvt, peilBottomDown))
! 16/3/2000  ! check op bodem in CF, indien beschikbaar
! Okt 2000: voeg MinimumDepthCF toe, en eis Delta_Volume >=0
             If (BndPar(Ibnd,6) .lt. 998.) &
                Delta_volume = min (Delta_Volume, bndpar(ibnd,5) * (BndPar(Ibnd,6)-MinimumDepthCF) )
             If (BndPar(Ibnd,6) .lt. 0.)  Delta_volume = 0.0
             Delta_volume = max (0.0, Delta_Volume)
! okt 2000: volumecheckfactor hoort hier ipv bij else tak
             MaxVolChk =  -1.* Delta_volume/timeSettings%timestepSize*NTIMLC  * VolumeCheckFactorToCF
             if (idebug .ne. 0) write(idebug,*) ' Q<0; VolumeCheckFactorToCF ', VolumeCheckFactorToCF
             if (idebug .ne. 0) write(idebug,*) ' Delta_volume, MaxVolChk', Delta_volume, MaxVolChk
             Call SetCheckActive (MaxVolChk, CheckActive, Istr, Ibnd, iowd)
             StrucFlow = max (StrucFlow, MaxVolChk)
           else   !bovenstrooms ow peil is hoger dan peil op rand, dus lozing op CF
             Delta_volume = Uparea * (uplvt - dolvt)
             MaxVolChk = Delta_volume/timeSettings%timestepSize*NTIMLC * VolumeCheckFactorToOW
             Call SetCheckActive (MaxVolChk, CheckActive, Istr, Ibnd, iowd)
             StrucFlow = min (StrucFlow, MaxVolChk)
             if (idebug .ne. 0) write(idebug,*) ' StrucFlow after check op max. volume up', StrucFlow, CheckActive
!  ARS 11610 Hiervoor is alleen de ow-peildaling onderzocht (zakt niet beneden dolvt)
!            Check nu ook of door deze lozing de peilstijging in CF niet boven UpLvlt uitkomt!!!
             if (FixArs11610Struct) then
               Delta_volume = bndpar(ibnd,5) * (dolvt - max (uplvt, peilBottomDown))
               Delta_volume = abs(Delta_volume)
               Delta_volume = max (0.0, Delta_Volume)
               MaxVolChk =  Delta_volume/timeSettings%timestepSize*NTIMLC  * VolumeCheckFactorToCF
               Call SetCheckActive (MaxVolChk, CheckActive, Istr, Ibnd, iowd)
               StrucFlow = min (StrucFlow, MaxVolChk)
               if (idebug .ne. 0) write(idebug,*) ' VolumeCheckFactorToCF ', VolumeCheckFactorToCF
               if (idebug .ne. 0) write(idebug,*) ' StrucFlow after check op max. volume down', StrucFlow, CheckActive
             endif
!  end aanpassingen ARS 11610
           endif
           if (idebug .ne. 0) write(idebug,*) ' StrucFlow after check op max. volume', StrucFlow, CheckActive
        ELSEIF (EiNode(NODEUP,3) .EQ. 6) THEN
        ! inlaat, rand bovenstrooms
           if (idebug .ne. 0) write(idebug,*) ' inlet:', bndpar(ibndup,5), DoArea, dolvt, uplvt, PeilBottomUp
           Delta_volume = bndpar(ibndup,5) * (uplvt - peilBottomUp)
           if (dolvt .lt. uplvt) then   ! positief debiet = water inlaten, onttrekken aan CF
             Delta_volume = bndpar(ibndup,5) * (uplvt - max (dolvt, peilBottomUp))
! 16/3/2000  ! check op bodem in CF, indien beschikbaar
! Okt 2000: voeg MinimumDepthCF toe, en eis Delta_Volume >=0
             If (BndPar(IbndUp,6) .lt. 998.) &
                Delta_volume = min (Delta_Volume, bndpar(ibndup,5) * (BndPar(IbndUp,6)-MinimumDepthCF) )
! was           Delta_volume = min (Delta_Volume, bndpar(ibndup,5) * BndPar(IbndUp,6))
             If (BndPar(IbndUp,6) .lt. 0.)  Delta_volume = 0.0
             Delta_volume = max (0.0, Delta_Volume)
!  nav ARS 11610 consistent gemaakt met geval waar rand benedenstrooms is
!            MaxVolChk = Delta_volume/timeSettings%timestepSize*NTIMLC
             MaxVolChk = Delta_volume/timeSettings%timestepSize*NTIMLC  * VolumeCheckFactorToCF
!            Sept2000: use correct ibndup index for counting
             Call SetCheckActive (MaxVolChk, CheckActive, Istr, Ibndup, iow)
             StrucFlow = min (StrucFlow, MaxVolChk)
           else  ! dolvt>=uplvt: benedenstrooms ow peil is hoger dan peil op rand, dus negatief debiet (lozing op CF)
             Delta_volume = -1. * DoArea * (dolvt - uplvt)
             MaxVolChk = Delta_volume/timeSettings%timestepSize*NTIMLC * VolumeCheckFactorToOW
!            Sept2000: use correct ibndup index for counting
             Call SetCheckActive (MaxVolChk, CheckActive, Istr, Ibndup, iow)
             StrucFlow = max (StrucFlow, MaxVolChk)
!  ARS 11610 Hiervoor is alleen de ow-peildaling onderzocht (zakt niet beneden uplvt)
!            Check nu ook of door deze lozing de peilstijging in CF niet boven dolvt uitkomt!!!
             if (FixArs11610Struct) then
               Delta_volume = bndpar(ibndup,5) * (uplvt - max (dolvt, peilBottomUp))
               Delta_volume = abs(Delta_volume)
               Delta_volume = max (0.0, Delta_Volume)
               MaxVolChk =  -1. * Delta_volume/timeSettings%timestepSize*NTIMLC  * VolumeCheckFactorToCF
               Call SetCheckActive (MaxVolChk, CheckActive, Istr, Ibndup, iow)
               StrucFlow = max (StrucFlow, MaxVolChk)
               if (idebug .ne. 0) write(idebug,*) ' StrucFlow after check op max. volume down', StrucFlow, CheckActive
             endif
!  end aanpassingen ARS 11610
           endif
           if (idebug .ne. 0) write(idebug,*) ' Qstru after check on max. volume', StrucFlow, CheckActive
        ELSE
        !kunstwerk tussen 2 open waters
          tmplvl = (Uparea * uplvt + DoArea * dolvt ) / (Uparea + DoArea)
          if (dolvt .lt. uplvt) then    ! bovenstrooms peil is hoger
! ARS 18445
             TmpLvl = max (TmpLvl, PeilBottomUp)
! End ARS 18445
             Delta_volume = Uparea * (uplvt - tmplvl)
             MaxVolChk = Delta_volume/timeSettings%timestepSize*NTIMLC * VolumeCheckFactorToOW
             Call SetCheckActive (MaxVolChk, CheckActive, Istr, Ibnd, iowd)
             StrucFlow = min (StrucFlow, MaxVolChk)
          else   ! benedenstrooms peil is hoger
! ARS 18445
             TmpLvl = max (TmpLvl, PeilBottomDown)
! End ARS 18445
             Delta_volume = -1. * DoArea * (dolvt - tmplvl)
             MaxVolChk = Delta_volume/timeSettings%timestepSize*NTIMLC * VolumeCheckFactorToOW
             Call SetCheckActive (MaxVolChk, CheckActive, Istr, Ibnd, iowd)
             StrucFlow = max (StrucFlow, MaxVolChk)
          endif
          if (idebug .ne. 0) write(idebug,*) ' Qstru after simple check max. volume', StrucFlow, SimpleVolumeCheck
        ENDIF
        STRING(1:1)  = ' '
        STRING(2:33) = Id_Nod(INODE)
        !end check max. volume

! ARS 15315 Additional volume check using crest levels
        If (CrestLvl .gt. -999.) then
           If (StrucFlow .gt. 0) then
              MaxVolChk = max(0.0, UpArea * (UpLvt - CrestLvl)) &
                                       / TimeSettings%timestepSize * NtimLc * VolumeCheckFactorToOW
              StrucFlow = min (StrucFlow, MaxVolChk)
              SimpleVolumeCheck = .true.
           ElseIf (StrucFlow .lt. 0) then
              MaxVolChk = -1. * max (0.0, DoArea * (DoLvt - CrestLvl)) &
                                      / TimeSettings%timestepSize * NtimLc * VolumeCheckFactorToOW
              StrucFlow = max (StrucFlow, MaxVolChk)
              SimpleVolumeCheck = .true.
           Endif
           if (idebug .ne. 0) write(idebug,*) ' Qstru after additional check crest levels', StrucFlow
        Endif

! ARS 15315 Additional volume check on open water volume: use known flows; do not use NtimLc!!
!           max. outflow limited by initial volume + total known net inflow, excluding current structure
        MinVolOw = 0.0
        If (Iow .gt. 0 .and. StrucFlow .gt. 0) then
           TQIN = QINOW(IOW,1) + QINOW(IOW,3) + Qinow(Iow,7)
           TQIN = TQIN + QIN0(IOW,2) + QIN0(IOW,4) + QIN0(IOW,5) + QIN0(IOW,6)
           Tqin = Tqin - QOUT0(IOW)
           ! total net inflow tqin, now exclude current structure flow of previous iteration
           !     but take into account flow q2 over parallell part of structure
           TQin = TQin + (QStru01(istr)+ QStru02(istr)) - q2
           If (CrestLvl .gt. -999.) then
               CALL RR_INTERP (NVAL, VolumeArrayUp, PeilArrayUp, MinVolOw, CrestLvl, idum)
           Endif
           MaxVolChk = max (0.0d0, VolOw0(iow) - MinVolOw + Tqin * timeSettings%timestepSize) / TimeSettings%timestepSize
           if (idebug .ne. 0) write(idebug,*) ' Tqin iow MaxVolChkflow ', Tqin, MaxVolChk
           StrucFlow = min (StrucFlow, MaxVolChk)
           SimpleVolumeCheck = .true.
           if (idebug .ne. 0) write(idebug,*) ' Qstru after additional open water check', StrucFlow
        Elseif (Iowd .gt. 0 .and. StrucFlow .lt. 0) then
           TQIN = QINOW(IOWd,1) + QINOW(IOWd,3) + Qinow(Iowd,7)
           TQIN = TQIN + QIN0(IOWd,2) + QIN0(IOWd,4) + QIN0(IOWd,5) + QIN0(IOWd,6)
           Tqin = Tqin - QOUT0(IOWd)
           ! total net inflow tqin, now exclude current structure flow of previous iteration
           !     but take into account flow q2 over parallell part of structure
           TQin = TQin - (QStru01(istr)+ QStru02(istr)) + q2
           If (CrestLvl .gt. -999.) then
               CALL RR_INTERP (NVAL, VolumeArrayDown, PeilArrayDown, MinVolOw, CrestLvl, idum)
           Endif
           MaxVolChk = -1. * max (0.0d0, VolOw0(iowd)-MinVolOw + Tqin * timeSettings%timestepSize) / TimeSettings%timestepSize
           if (idebug .ne. 0) write(idebug,*) ' Tqin iowd MaxVolChkFlow', Tqin, MaxVolChk
           StrucFlow = max (StrucFlow, MaxVolChk)
           SimpleVolumeCheck = .true.
           if (idebug .ne. 0) write(idebug,*) ' Qstru after additional open water check', StrucFlow
        Endif


        If (CheckActive) then
          Select Case (Itype)
             Case  (1)
                MaxVolChkWeirBoundary =.true.
             Case  (2)
                MaxVolChkOrificeBoundary =.true.
             Case  (3)
                MaxVolChkFrictionBoundary =.true.
          End select
        Endif

    RETURN
  END subroutine VolumeCheck



  Subroutine SetCheckActive (MaxVolChk, CheckActive, Istr, Ibnd, Iow)

! MaxVolChk = Maximum debiet volgens volume check
! CheckActive = true/fals (output)
! istr = kunstwerk index
! ibnd = boundary index
!
! Alleen voor randen met variabel peil volgens Sobek wordt de Severe variabele gezet

! zet Volume Check active als debiet met factor 4 of meer geknepen wordt en Q >= 0.1 m3/s.
! Deze factor is hard gecodeerd

  Real    MaxVolChk
  Integer Istr, Ibnd, iow
  Logical CheckActive
  Character(len=CharIdLength) IdStructure, IdBoundary, IdOpenWater

  If ( Abs (MaxVolChk) .lt. Abs (0.25 * Qstru(istr)) .and. Abs(QStru(istr)) .gt. .1) then
     If (ibnd .gt. 0) then
        If (bndpar(ibnd,2) .eq. 2) then
           ! on-line gekoppelde randen RR-CF/SF
           SevereVolumeCheck = .true.
           CheckActive       = .true.
        Else
           SimpleVolumeCheck = .true.
        Endif
        if (DetailedVolumeCheckMessages) then
            idstructure = Id_nod((strnam(istr)))
            idboundary  = Id_nod((bndnam(ibnd)))
            call ErrMsgStandard (982, 982, idstructure, idboundary)
        endif
     Else
        SimpleVolumeCheck = .true.
        if (DetailedVolumeCheckMessages .and. iow .gt. 0) then
            idstructure = Id_nod((strnam(istr)))
            idopenwater = Id_nod((ownam(iow)))
            call ErrMsgStandard (982, 982, idstructure, idboundary)
        endif
     Endif
  Endif

  RETURN
  END subroutine SetCheckActive


  Subroutine  SetFractionTime (Checklevel, Finallevel, Initlevel, FractionTime, Imode)

! Subroutine bepaalt fractie van tijdstap *--------*----------------------*
!                                         Init   Check                Final
!                                         ==========
!                                                   =======================

  Real CheckLevel, FinalLevel, Initlevel, FractionTime
  Integer Imode
  Integer Idebug

  iDebug = ConfFil_get_iDebug()
  FractionTime = 1.0

  If (abs (initLevel - FinalLevel) .gt. .000001) Then
     FractionTime = abs ( (CheckLevel-InitLevel) / (FinalLevel - InitLevel) )
     FractionTime = min (FractionTime, 1.0)
     If (imode .eq. 2) FractionTime = 1.0 - FractionTime
  Endif

  if (idebug .ne. 0) then
    write(idebug,*) ' Initlevel ', Initlevel
    write(idebug,*) ' Finallevel', Finallevel
    write(idebug,*) ' Checklevel', Checklevel
    write(idebug,*) ' Imode     ', Imode
    write(idebug,*) ' Fraction  ', FractionTime
  Endif

  RETURN
  END subroutine SetFractionTime




  SUBROUTINE CMPSTF (ISTR, INODE, IOW, IOWD, IBNDUP, IBND, &
                     NODOWN, DOLVL, DOLV0, DOVOL, TQINDW, UpArea, DoArea, &
                     UPLVL, TLVL, TQIN, ZBOTTM, CheckZMax, ZMax, &
                     NODEUP, peilBottomDown, peilBottomUp, NTiml3)
    ! *********************************************************************
    ! *** Last update:  7 March 1997                      by: Peter Schrier
    ! *********************************************************************
    ! *** Brief description:
    ! *** ------------------
    ! ***    This subroutine performs stuw computations
    ! *********************************************************************
    ! *** Input/output parameters:
    ! *** ------------------------
    ! ***  IEVENT = event number
    ! ***  ITMSTP = timestep number
    ! ***  IDEBUG = file unit number of debug file
    ! ***  IOUT1  = file unit number of output file with run messages
    ! ***  ISTR   = intern structure nr
    ! ***  IMETEO = meteostation code
    ! ***  INODE  = knoopnr
    ! *********************************************************************
    Integer iStr, iOW, iOWD, iBndUp, iBnd, iNode, noDown, NodeUp
    Real doLvl, doLv0, doVol, tqIndW, upLvl, tLvl, tqIn
    Real Delta_volume, peilBottomDown, peilBottomUp, ZMax, crestLvl

    Integer nTiml, nTimLc, nTimLc_Old, iT, iTl
    Real gCoef, upLvT, UpArea, DoArea, vol0, doLvt, c, width, tLvlUp, tLvlDo, z, u
    Real refDo, refUp, deltaH, y, rHlp, upLvT0, doLvt0, Vslope, ActualRatio, ActualReductionFactor

    LOGICAL  DROWN, WISSEL, ZBOTTM, CheckZMax
    Logical DateTimeOutsideTable
    Integer Rownr, TabelNr, idum, IfReal, NtimL3


    Integer, PARAMETER :: NTIML2 = 60
    REAL      QTEMP(NTIML2)

    type (Date) currentDate
    type (Time) currentTime

    Real    PeilArrayUp(6), AreaArrayUp(6), VolumeArrayUp(6)  ! aanname dat NVAL=6 zoals in Conf_Arr.
    Real    PeilArrayDown(6), AreaArrayDown(6), VolumeArrayDown(6)  ! aanname dat NVAL=6 zoals in Conf_Arr.
    integer i

    ! versnelling van de zwaartekracht (9.81 m/s2)
    Real, parameter ::   G  = 9.81
    Integer iDebug, Iout1

    if (iow .gt. 0) then
        Do i=1,NVal
           PeilArrayUp(i)   = PeilOw(i,iow)
           AreaArrayUp(i)   = AreaOw(i,iow)
           VolumeArrayUp(i) = VoluOw(i,iow)
        Enddo
    endif
    if (iowd .gt. 0) then
        Do i=1,NVal
           PeilArrayDown(i)   = PeilOw(i,iowd)
           AreaArrayDown(i)   = AreaOw(i,iowd)
           VolumeArrayDown(i) = VoluOw(i,iowd)
        Enddo
    endif

    iOut1  = ConfFil_get_iOut1 ()
    iDebug = ConfFil_get_iDebug()

! ARS 11196: bepaling max. debiet op basis 1 tijdstap; gebruik NTiml3; via aanroep geregeld
!   NTiml3 = NTiml2
!   if (ZBottm) NTiml3 = 1
! end ARS 11196

    IF (iDebug .ne. 0)  WRITE(IDEBUG,*) 'CMPSTF istr=',ISTR, ' ibnd', ibnd

    NTIML = MIN (NVAL2, NTIML2, 6)
    IF (timeSettings%timestepSize/NTIML .LT. NRSMIN)  NTIML = timeSettings%timestepSize / NRSMIN
! Aanpassing Okt 2002: Gcoef depends on weir type
    If (StrPar(Istr,11) .eq. 2) then
      ! Vstuw werkt met GCOEF = 16/25 * ((2./5.* G) ** 0.5 )
      GCOEF = (2./5.* G) ** 0.5
      GCOEF = 16./25. * Gcoef
      VSlope = Strpar(Istr,12)
    Else
      ! rechte stuw werkt met GCOEF = 2/3 *  ((2./3.* G) ** 0.5 )
      GCOEF = (2./3.* G) ** 0.5
      GCOEF = 2./3. * GCOEF
    Endif

    DO IT=1,NTIML2
      QTEMP(IT) = 0
    ENDDO

    ! *********************************************************************
    ! *** Structure type 2 = stuw; type 6= inlaatstuw
    ! *********************************************************************

      NTIMLC = MAX (NTIML, 1)
202   CONTINUE
      UPLVT  = UPLVL
      IF (IOW .GT. 0) THEN
         Call RR_INTERP (NVAL, PeilArrayUp, VolumeArrayUp, UPLVL, VOL0, OwLastInterpIndex(iow))
         Call RR_INTERP (NVAL, PeilArrayUp, AreaArrayUp, UPLVL, UpArea, OwLastInterpIndex(iow))
      ENDIF
      DOLVT  = DOLV0
      DOLVT0 = DOLVT
      IF (IOWD .GT. 0) THEN
         Call RR_INTERP (NVAL, PeilArrayDown, AreaArrayDown, DOLVT, DoArea, OwLastInterpIndex(iowd))
      ENDIF


      IF (iDebug .ne. 0) WRITE(IDEBUG,*) ' Upstream level, Qstr'

      DO ITL = 1,NTIMLC
        if (idebug .ne. 0) Write(Idebug,*) ' Start Itl loop ', Itl,' of ',NtimLc
        C     = STRPAR(ISTR,1)
        WIDTH = STRPAR(ISTR,2)
        IF (STRPAR(ISTR,5) .EQ. 1.0) THEN
    ! ***  PI regelaar: z volgens controller; CMPCNT is lege routine
    !      Call CMPCNT (IEVENT, ITMSTP, ISTR  , IMETEO, INODE , Z)
        ELSEIF ((STRPAR(ISTR,5) .GE. 3.0 .and. STRPAR(ISTR,5) .le. 4.0) .AND. (STRTYP(ISTR) .EQ. 2)) THEN
    ! ***        vast peilverschil regelaar: alleen bij uitlaatstuw (IOW>0)
    ! ***        3=vast peilverschil, 4=minimum peilverschil
    ! ***          stuwhoogte = ben.strooms peil + peilverschil streefpeilen

           currentDate%year = ConfArr_get_IYear()
           currentDate%month = ConfArr_get_iMonth()
           currentDate%day = ConfArr_get_iDay()
           currentTime%hour = ConfArr_get_iHour()
           currentTime%minute = ConfArr_get_iMinute()
           currentTime%second = 0

! Zoek in open water tabel het streefpeil op
           Tlvlup = CurrentTargetLevel(Iow)
           if (idebug .ne. 0)  write(idebug,*) ' New Method Target Level', TLvlUp

           IF (IOWD .GT. 0) THEN
           !  benedenstrooms is open water
               Tlvldo = CurrentTargetLevel(Iowd)
               if (idebug .ne. 0)  write(idebug,*) ' New Method Downstream Target Level', TLvlDo
           ELSE
           ! benedenstrooms is boundary
               TLVLDO = BNDPAR(IBND,1)
           ENDIF

           Z = STRPAR(ISTR,3)
           IF (ZBOTTM) THEN
              Call RR_INTERP (NVAL, VolumeArrayUp, PeilArrayUp, 0.0, Z, OwLastInterpIndex(iow))
! Sept 2000: Waarom eigenlijk Z >= Target level - 1 meter??
              Z = MAX (TLVL -1.0, Z)
              IF (iDebug .ne. 0) WRITE(IDEBUG,*) ' ZBOTTM', Z
           ENDIF
           IF (STRPAR(ISTR,5) .EQ. 3) THEN
              Z = DOLVL + TLVLUP - TLVLDO
           ELSEIF (STRPAR(ISTR,5) .EQ. 4) THEN
              Z = MAX (Z, DOLVL + STRPAR(ISTR,6))
           ENDIF
           IF (iDebug .ne. 0) &
              WRITE(IDEBUG,*) ' Difference-level controller', &
                                 Z, DOLVL, TLVLUP, TLVLDO
        ELSE

           Z = STRPAR(ISTR,3)
           ! Check for Time controller for crest level
           IF ( (STRPAR(ISTR,5) .eq. 5.0 .AND. STRTYP(ISTR) .EQ. 2) .or. &
                (STRPAR(ISTR,10) .eq. 5.0 .AND. STRTYP(ISTR) .EQ. 6)) THEN
              currentDate%year  = ConfArr_get_iYear()
              currentDate%month = ConfArr_get_iMonth()
              currentDate%day   = ConfArr_get_iDay()
              currentTime%hour  = ConfArr_get_iHour()
              currentTime%minute = ConfArr_get_iMinute()
              currentTime%second = 0
              RowNr = -1
              TabelNr = StrRefTimeTable (iStr)
              If (TabelNr .gt. 0) then
                  Z = GetNewValue(TableHandle, TabelNr, 1, RowNr, CurrentDate, CurrentTime, &
                                  Idebug, Iout1, DateTimeOutsideTable, .true. )
              Endif
              if (idebug .ne. 0) Write(Idebug,*) ' CheckTimeControllerWeir Z=',Z
              If (.not. ZBottm) ActCrestLevel(ISTR) = Z
           Endif

           IF (ZBOTTM) THEN
! ZBottm=true geeft aan dat fysisch maximum debiet bepaald wordt.
! Neem hiervoor Z=bodemniveau=peil bij volume nul.
! Sept 2000: Waarom eigenlijk ook >= Target level - 1 meter??
              Call RR_INTERP (NVAL, VolumeArrayUp, PeilArrayUp,  0.0, Z, OwLastInterpIndex(iow))
              Z = MAX (TLVL -1.0, Z)
              IF (iDebug .ne. 0) WRITE(IDEBUG,*) ' ZBOTTM', Z
! ARS 8736: begrenzing crest level bij streefpeilcontroller met debietbegrenzer
! ARS 8736: begrens crest level van onderen niet op bodem, maar op opgegeven min.crest level
              if (idebug .ne. 0) Write(Idebug,*) ' Strpar(istr,10)', StrPar(istr,10)
              If (StrPar(istr,10) .gt. CrestMissingValue) then
                  Z = MAX (Z, StrPar(istr,10))
                  IF (iDebug .ne. 0) WRITE(IDEBUG,*) ' ZBOTTM min.crest level met debietbegrenzer', Z
              Endif
           ENDIF
! ARS 11093 target level controller also with a maximum crest level
           IF (CheckZMax) THEN
              Z = ZMax
              IF (iDebug .ne. 0) WRITE(IDEBUG,*) ' CheckZMax max.crest level met debietbegrenzer', Z
           Endif
    ! addition 11-10:
    !   bij ongeregelde stuw: stuwhoogte <= bovenstrooms streefpeil
           IF (STRPAR(ISTR,5) .LE. 0 .AND. UPDSTW)  Z = MIN (Z,TLVL)
        ENDIF

        U = STRPAR(ISTR,4)
        IF (iDebug .ne. 0) WRITE(IDEBUG,*) ' Z=',Z, ' U=',U
        IF (iDebug .ne. 0) WRITE(IDEBUG,*) ' UpLVT =', UpLVT, 'DoLvT=', DoLVT

    ! bepaal of UPLVT ook echt bovenstrooms is; mogelijke stroomomkering!
        IF (UPLVT .GE. DOLVT) THEN
           WISSEL = .FALSE.
        ELSEIF (UPLVT .LT. DOLVT) THEN
           WISSEL = .TRUE.
           RHLP = UPLVT
           UPLVT= DOLVT
           DOLVT= RHLP
        ENDIF
        IF (iDebug .ne. 0) WRITE(IDEBUG,*) ' after check Wissel - UpLVT =', UpLVT, 'DoLvT=', DoLVT

        If (StrPar(Istr,11) .eq. 2) then
!          V-stuw
           REFDO = DOLVT - Z      ! H2
           REFUP = UPLVT - Z      ! H1
           IF (iDebug .ne. 0) WRITE(IDEBUG,*) ' RefDo = ',RefDo, ' RefUp=',RefUp
           IF (iDebug .ne. 0) WRITE(IDEBUG,*) ' C     = ',C    , ' Gcoef=',GCoef
           IF (iDebug .ne. 0) WRITE(IDEBUG,*) ' Vslope= ',Vslope
           QSTRU(ISTR) = 0.0
           ActualRatio = 0.
           If (RefUp .gt. 0) then
               ActualRatio = Refdo / RefUp
               QSTRU(ISTR) = C * GCOEF * VSlope * ( (RefUp) ** U )
           Endif
!          Test of ratio >0 is (Ifreal geeft 1 als resultaat)
           If (IfReal(ActualRatio, 0.0, 1E-6) .eq. 1) then
              If (ActualRatio .ge. 1) then
                  ActualReductionFactor = 0.0
!  VShapeWeirH2H1ratio should be in increasing order
              Elseif (ActualRatio .le. VShapeWeirH2H1Ratio(1)) then
                  ActualReductionFactor = 1.0
              Else
                Call RR_INTERP (NrVShapeWeirReductionPoints, VShapeWeirH2H1Ratio, VshapeWeirFlowReductionFactor, &
                                ActualRatio, ActualReductionFactor, idum)
                if (idebug .ne. 0) then
                   write(Idebug,*) ' idum=',idum,' ActualRatio=',ActualRatio
                   write(Idebug,*) ' reduction factor ', ActualReductionFactor
                endif
                If (Idum .eq. 0) then
                  ActualReductionFactor = 1.
                Elseif (Idum .gt. NrVShapeWeirReductionPoints) then
                  ActualReductionFactor = VShapeWeirFlowReductionFactor(NrVShapeWeirReductionPoints)
                Endif
              Endif
              IF (iDebug .ne. 0) then
                 write(Idebug,*) ' Vnotch ratio, reduction factor ', ActualRatio, ActualReductionFactor, Qstru(istr)
              Endif
              ActualReductionFactor = min (1.0, ActualReductionFactor)
              ActualReductionFactor = max (0.0, ActualReductionFactor)
              QSTRU(ISTR) = ActualReductionFactor * QStru(istr)
              IF (iDebug .ne. 0) then
                 write(Idebug,*) ' After correction reduction factor ', ActualReductionFactor, Qstru(istr)
              Endif
           Endif
        Else
!       broad crested rechthoekige stuw, of getrapte stuw
       ! check of stroming verdronken stroming is
           DROWN = .FALSE.
           REFDO = DOLVT - Z
           REFUP = (UPLVT - Z) * 2./3.
           IF (REFDO .GT. REFUP .AND. REFDO .GT. .0) THEN
              DROWN  = .TRUE.
              U      = 0.5
              DELTAH = UPLVT - DOLVT
              Y      = REFDO
           ENDIF

           IF (.NOT. DROWN) THEN
             QSTRU(ISTR) = 0.0
             IF (UPLVT .GT. Z) THEN
                QSTRU(ISTR) = C * WIDTH * GCOEF * ( (UPLVT - Z) ** U )
             ENDIF
           ELSE
             IF (iDebug .ne. 0) WRITE(IDEBUG,*) ' Drowned weir'
             If (DrownedWeirDepth .eq. 0) then      ! kleinste diepte nemen
                Y = Min (UpLvt-Z, Dolvt-Z)
             Elseif (DrownedWeirDepth .eq. 1) then  ! grootste diepte
                Y = Max (UpLvt-Z, Dolvt-Z)
             Else   ! gemiddelde diepte
                Y = 0.5 * (UpLvt + Dolvt) - Z
             Endif
             QSTRU(ISTR) = 0.0
             If (Deltah .GT. 0)  QSTRU(ISTR) = C * WIDTH * Y * ( (2*G * DELTAH) ** U )
           ENDIF
        Endif

        IF (WISSEL) THEN
           RHLP = UPLVT
           UPLVT= DOLVT
           DOLVT= RHLP
           QSTRU(ISTR) = -1. * QSTRU(ISTR)
        ENDIF
        IF (iDebug .ne. 0) WRITE(IDEBUG,*) ' after check Wissel2 - QSTru =', QStru(istr)
        IF (iDebug .ne. 0) WRITE(IDEBUG,*) ' after check Wissel2 - UpLVT =', UpLVT, 'DoLvT=', DoLVT
    !april96: check retourstroom
        IF (STRTYP(ISTR) .EQ. 2 .AND. STRPAR(ISTR,9) .EQ. 0) THEN
            QSTRU(ISTR) = MAX (0.0, QSTRU(ISTR))
            IF (iDebug .ne. 0)  WRITE(IDEBUG,*) ' Correction backflow'
    !jan 2004: added check for inlet weir
        ElseIF (STRTYP(ISTR) .EQ. 6 .AND. STRPAR(ISTR,14) .EQ. 0) THEN
            QSTRU(ISTR) = MAX (0.0, QSTRU(ISTR))
            IF (iDebug .ne. 0)  WRITE(IDEBUG,*) ' Correction backflow'
        ENDIF
    !021195   IF (UPLVT .LT. Z .AND. DOLVT .LT. Z) QSTRU(ISTR) = 0.0
    ! ARS 16175: check pos. flow possible
        IF (STRTYP(ISTR) .EQ. 2 .AND. STRPAR(ISTR,15) .EQ. 0) THEN
            QSTRU(ISTR) = Min (0.0, QSTRU(ISTR))
            IF (iDebug .ne. 0)  WRITE(IDEBUG,*) ' Correction pos.low'
        ElseIF (STRTYP(ISTR) .EQ. 6 .AND. STRPAR(ISTR,15) .EQ. 0) THEN
            QSTRU(ISTR) = Min (0.0, QSTRU(ISTR))
            IF (iDebug .ne. 0)  WRITE(IDEBUG,*) ' Correction pos.flow'
        ENDIF
        IF (iDebug .ne. 0) WRITE(IDEBUG,*) UPLVT,DOLVT, QSTRU(ISTR), ' BACKFL', strpar(istr,9)

        crestlvl = Z
        Call VolumeCheck (ISTR, INODE, IBNDUP, IBND, iow, iowd, Qstru(Istr),&
                          NODOWN, NODEUP, peilBottomDown, peilBottomUp, &
                          dolvt, uplvt, Uparea, DoArea, NtimLc, 1, crestLvl, QStru2(istr))

        QTEMP(ITL) = QSTRU(ISTR)
        if (idebug .ne. 0) write(Idebug,*) 'after VolumeCheck  itl Q(itl)', itl, QTemp(itl)
        if (idebug .ne. 0) then
           write(idebug,*) ' before volume-correction', timeSettings%timestepSize,ntimlc
           if (iow .gt. 0)  write(idebug,*) ' upstream ow', iow, vol0, qtemp(itl), tqin, qowupr(istr)
           if (iowd .gt. 0) write(idebug,*) ' downstream ow', iowd, dovol, qtemp(itl), tqindw, qowdwr(istr), qout0(iowd)
        endif

        IF (IOW .GT. 0) THEN
          if (idebug .ne. 0) Write(Idebug,*) ' upstream is open water '
          VOL0 = VOL0 - QTEMP(ITL) * timeSettings%timestepSize / NTIMLC &
                      + TQIN * timeSettings%timestepSize / NTIMLC &
                      - QOWUPR(ISTR) * timeSettings%timestepSize / NTIMLC &
                      - QSTRU02(ISTR) * timeSettings%timestepSize / NTIMLC
          UPLVT0 = UPLVT
          if (idebug .ne. 0) then
             Write(Idebug,*) ' upstream open water voor Interp'
             Write(Idebug,*) ' Nval =', Nval
             Write(Idebug,*) ' Iow  =', Iow
             Write(Idebug,*) ' Voluow ', (Voluow(idum,iow),idum=1,Nval)
             Write(Idebug,*) ' Peilow ', (Peilow(idum,iow),idum=1,Nval)
             Write(Idebug,*) ' Vol0   ', Vol0
             Write(Idebug,*) ' UpLVT  ', UPLvt
             Write(Idebug,*) ' LastInterpIndex ',OwLastInterpIndex(iow)
          endif
          Call RR_INTERP (NVAL, VolumeArrayUp, PeilArrayUp, VOL0, UPLVT, OwLastInterpIndex(iow))
          if (idebug .ne. 0) Write(Idebug,*) ' upstream open water na Interp'
    ! Jan 1996: UPLVT mag niet het maximum peil OWMNMA overschrijden
          UPLVT = MIN (UPLVT, OWMNMA(IOW))
          if (idebug .ne. 0) Write(Idebug,*) ' end upstream is open water '
        ELSEIF (IBNDUP .GT. 0) THEN
          UPLVT0 = UPLVT
        !Okt 97 bij randen check op peilvariatie via areaal
          Delta_volume = QTEMP(ITL)* timeSettings%timestepSize / NTIMLC
! Mar 2002 only change of UPLVT within timestep if switch CFBoundaryConstantInTimestep is not on
           if (CFBoundaryConstantInTimestep .eq. 0) then
              UPLVT  = UPLVL - Delta_volume / BNDPAR(ibndup,5)
           else
              UPLVT  = UPLVL
           endif
        ENDIF
    ! extra check of kleinere tijdstap nodig is
! May 2002: StructComp backw. compatibility option also for Orifice
! Feb2003: ARS 11196: bepaling max. debiet op NTiml3 ipv NTiml2
        IF (iDebug .ne. 0) then
           WRITE(IDEBUG,*) ' Test restart weir computations'
           WRITE(IDEBUG,*) ' Structcomp         ', StructComp
           WRITE(IDEBUG,*) ' abs (uplvt-uplvt0) ', Abs(Uplvt-Uplvt0)
           WRITE(IDEBUG,*) ' abs (uplvt0-dolvt) ', Abs(Uplvt0-dolvt)
           WRITE(IDEBUG,*) ' abs (dolvt-dolvt0) ', Abs(dolvt-dolvt0)
           WRITE(IDEBUG,*) ' ntimlc ntiml3      ', ntimlc, ntiml3
           WRITE(IDEBUG,*) ' timestep/ntimlc nrsmin', TimeSettings%TimestepSize/NtimLc, NrsMin
        Endif
        If (StructComp .eq. 0) then
          If ( ( ABS(UPLVT - UPLVT0) .GT. ABS(UPLVT0-DOLVT) .OR. &
                  ABS(UPLVT-UPLVT0) .GT. .05 .OR. &
                   ABS(DOLVT-DOLVT0) .GT. .05         )  .AND. &
                    NTIMLC .NE. NTIML3 .AND. timeSettings%timestepSize/NTIMLC .GT. NRSMIN) THEN
             IF (iDebug .ne. 0) WRITE(IDEBUG,*) ' Restart weir computations'
             NTimLc_old = NTimLc
             NTIMLC = NTIML3
             IF (timeSettings%timestepSize/NTIMLC .LT. NRSMIN) &
                NTIMLC = timeSettings%timestepSize / NRSMIN
             NTimLc = max (NTimLc, NTimLc_old + 1)
             GOTO 202
           Endif
        Elseif (StructComp .eq. 1) then
           If ( ( ABS(UPLVT - UPLVT0) .GT. ABS(UPLVT0-DOLVT) .OR. &
                   ABS(UPLVT-UPLVT0) .GT. .05 .OR. &
                    ABS(DOLVT-DOLVT0) .GT. .05         )  .AND. &
                     NTIMLC .NE. NTIML3 .AND. timeSettings%timestepSize/NTIMLC .GE. NRSMIN) THEN
             IF (iDebug .ne. 0) WRITE(IDEBUG,*) ' Restart weir computations'
             NTimLc_old = NTimLc
             NTIMLC = Min (NTIML3, 2*TimeSettings%timestepSize/NrsMin)
             NTimLc = max (NTimLc, NTimLc_old + 1)
             GOTO 202
          Endif
        ENDIF
    ! aanpassing benedenstrooms peil;
        IF (EiNode(NODOWN,3) .EQ. 4) THEN
           if (idebug .ne. 0) then
             Write(Idebug,*) ' downstream is open water '
             Write(Idebug,*) ' Iowd =', Iowd
             Write(Idebug,*) ' Voluow ', (Voluow(idum,iowd),idum=1,Nval)
             Write(Idebug,*) ' Peilow ', (Peilow(idum,iowd),idum=1,Nval)
             Write(Idebug,*) ' Dovol DoLvT  ', DoVol, DolVt
          endif
    !nieuw gebruik inflow en outflow
           DOLVT0 = DOLVT
           DOVOL = DOVOL + TQINDW * timeSettings%timestepSize / NTIMLC &
                    + QOWDWR(ISTR) * timeSettings%timestepSize / NTIMLC &
                    + QTEMP(ITL) * timeSettings%timestepSize / NTIMLC &
                    - QOUT0(IOWD) * timeSettings%timestepSize / NTIMLC &
                    + QSTRU02(ISTR) * timeSettings%timestepSize / NTIMLC
           Call RR_INTERP (NVAL, VolumeArrayDown, PeilArrayDown, DOVOL, DOLVT, OwLastInterpIndex(iowd))
    ! Jan 1996: DOLVT mag niet het maximum peil OWMNMA overschrijden
           if (idebug .ne. 0) Write(Idebug,*) ' DoLvt new ='
           DOLVT = MIN (DOLVT, OWMNMA(IOWD))
           if (idebug .ne. 0) Write(Idebug,*) DoLvt, DoVol
         ELSEIF (EiNode(NODOWN,3) .EQ. 6) THEN
           DOLVT0 = DOLVT
           Delta_volume = QTEMP(ITL)* timeSettings%timestepSize / NTIMLC
! Mar 2002 only change of DOLVT within timestep if switch CFBoundaryConstantInTimestep is not on
           if (CFBoundaryConstantInTimestep .eq. 0) then
              DOLVT = DOLVT + Delta_volume / BNDPAR(ibnd,5)
           endif
         ENDIF
         if (idebug .ne. 0) Write(Idebug,*) ' End Itl loop ', Itl,' of ',NtimLc

      ENDDO

      QSTRU(ISTR) = 0.0
      if (idebug .ne. 0) Write(Idebug,*) ' All Itl'
      DO ITL=1,NTIMLC
         QSTRU(ISTR) = QSTRU(ISTR) + QTEMP(ITL)
         if (idebug .ne. 0) Write(Idebug,*) itl, Qtemp(itl)
      ENDDO
      QSTRU(ISTR) = QSTRU(ISTR)/NTIMLC

      If (.not. ZBottm) then
         ActCrestLevel(ISTR) = Z
      Else
         BottomCrestLevel(ISTR) = Z
      Endif

      if (idebug .ne. 0) then
         Write(Idebug,*) ' End CmpStf: Computed flow QStru =', Qstru(istr), ' internal timesteps used',NTimLc
      Endif

    RETURN
  END subroutine cmpstf



  SUBROUTINE CMPSTF2 (ISTR, INODE, IOW, IOWD, IBNDUP, IBND, &
                      NODOWN, DOLV0, DOVOL, TQINDW, UPLVL, TQIN, UpArea, DoArea, &
                      NODEUP, peilBottomDown, peilBottomUp)
    ! *********************************************************************
    ! *** Last update:  7 March 1997                      by: Peter Schrier
    ! *********************************************************************
    ! *** Brief description:
    ! *** ------------------
    ! ***    This subroutine performs stuw computations for weir type 3, 2nd crest level (fixed level)
    ! *********************************************************************
    ! *** Input/output parameters:
    ! *** ------------------------
    ! ***  IEVENT = event number
    ! ***  ITMSTP = timestep number
    ! ***  IDEBUG = file unit number of debug file
    ! ***  IOUT1  = file unit number of output file with run messages
    ! ***  ISTR   = intern structure nr
    ! ***  IMETEO = meteostation code
    ! ***  INODE  = knoopnr
    ! *********************************************************************
    Integer iStr, iOW, iOWD, iBndUp, iBnd, iNode, noDown, NodeUp
    Real doLv0, doVol, tqIndW, upLvl, tqIn
    Real Delta_volume, peilBottomDown, peilBottomUp, crestlvl

    Integer nTiml, nTimLc, nTimLc_Old, iT, iTl
    Real gCoef, upLvT, UpArea, DoArea, vol0, doLvt, c, width, z, u
    Real refDo, refUp, deltaH, y, rHlp, upLvT0, doLvt0

    LOGICAL  DROWN, WISSEL


    Integer, PARAMETER :: NTIML2 = 60
    REAL      QTEMP(NTIML2)


    ! versnelling van de zwaartekracht (9.81 m/s2)
    Real, parameter ::   G  = 9.81
    Integer iDebug, Iout1

    Real    PeilArrayUp(6), AreaArrayUp(6), VolumeArrayUp(6)  ! aanname dat NVAL=6 zoals in Conf_Arr.
    Real    PeilArrayDown(6), AreaArrayDown(6), VolumeArrayDown(6)  ! aanname dat NVAL=6 zoals in Conf_Arr.
    integer i

    if (iow .gt. 0) then
        Do i=1,NVal
           PeilArrayUp(i)   = PeilOw(i,iow)
           AreaArrayUp(i)   = AreaOw(i,iow)
           VolumeArrayUp(i) = VoluOw(i,iow)
        Enddo
    endif
    if (iowd .gt. 0) then
        Do i=1,NVal
           PeilArrayDown(i)   = PeilOw(i,iowd)
           AreaArrayDown(i)   = AreaOw(i,iowd)
           VolumeArrayDown(i) = VoluOw(i,iowd)
        Enddo
    endif

    iOut1  = ConfFil_get_iOut1 ()
    iDebug = ConfFil_get_iDebug()

    IF (iDebug .ne. 0)  WRITE(IDEBUG,*) 'CMPSTF2 istr=',ISTR, ' ibnd', ibnd

    NTIML = MIN (NVAL2, NTIML2, 6)
    IF (timeSettings%timestepSize/NTIML .LT. NRSMIN) &
       NTIML = timeSettings%timestepSize / NRSMIN
    GCOEF = (2./3.* G) ** 0.5
    GCOEF = 2./3. * GCOEF

    DO IT=1,NTIML2
      QTEMP(IT) = 0
    ENDDO

    ! *********************************************************************
    ! *** Structure type 2 = stuw; type 6= inlaatstuw
    ! *********************************************************************

      NTIMLC = MAX (NTIML, 1)
202   CONTINUE
      UPLVT  = UPLVL
      IF (IOW .GT. 0) THEN
         Call RR_INTERP (NVAL, PeilArrayUp, VolumeArrayUp, UPLVL, VOL0, OwLastInterpIndex(iow))
         Call RR_INTERP (NVAL, PeilArrayUp, AreaArrayUp, UPLVL, UpArea, OwLastInterpIndex(iow))
      ENDIF
      DOLVT  = DOLV0
      DOLVT0 = DOLVT
      IF (IOWD .GT. 0) THEN
         Call RR_INTERP (NVAL, PeilArrayDown, AreaArrayDown, DOLVT, DoArea, OwLastInterpIndex(iowd))
      ENDIF



      IF (iDebug .ne. 0) WRITE(IDEBUG,*) ' Upstream level, Qstr'

      DO ITL = 1,NTIMLC
        C     = STRPAR(ISTR,1)
!       take crest width and crest level 2nd step
        WIDTH = STRPAR(ISTR,13)
        Z = STRPAR(ISTR,12)
        U = STRPAR(ISTR,4)
        IF (iDebug .ne. 0) WRITE(IDEBUG,*) ' Z=',Z, ' U=',U

    ! bepaal of UPLVT ook echt bovenstrooms is; mogelijke stroomomkering!

        IF (UPLVT .GE. DOLVT) THEN
           WISSEL = .FALSE.
        ELSEIF (UPLVT .LT. DOLVT) THEN
           WISSEL = .TRUE.
           RHLP = UPLVT
           UPLVT= DOLVT
           DOLVT= RHLP
        ENDIF

    ! check of stroming verdronken stroming is

        DROWN = .FALSE.
        REFDO = DOLVT - Z
        REFUP = (UPLVT - Z) * 2./3.
        IF (REFDO .GT. REFUP .AND. REFDO .GT. .0) THEN
           DROWN  = .TRUE.
           U      = 0.5
           DELTAH = UPLVT - DOLVT
           Y      = REFDO
        ENDIF


        IF (.NOT. DROWN) THEN
          QSTRU2(ISTR) = 0.0
          IF (UPLVT .GT. Z) THEN
             QSTRU2(ISTR) = C * WIDTH * GCOEF * ( (UPLVT - Z) ** U )
          ENDIF
        ELSE
          IF (iDebug .ne. 0) WRITE(IDEBUG,*) ' Drowned weir'
          If (DrownedWeirDepth .eq. 0) then      ! kleinste diepte nemen
             Y = Min (UpLvt-Z, Dolvt-Z)
          Elseif (DrownedWeirDepth .eq. 1) then  ! grootste diepte
             Y = Max (UpLvt-Z, Dolvt-Z)
          Else  ! gemiddelde diepte
             Y = 0.5 * (UpLvt + Dolvt) - Z
          Endif
          QSTRU2(ISTR) = 0.0
          If (Deltah .GT. 0)  QSTRU2(ISTR) = C * WIDTH * Y * ( (2*G * DELTAH) ** U )
        ENDIF

        IF (WISSEL) THEN
           RHLP = UPLVT
           UPLVT= DOLVT
           DOLVT= RHLP
           QSTRU2(ISTR) = -1. * QSTRU2(ISTR)
        ENDIF
    !april96: check retourstroom
        IF (STRTYP(ISTR) .EQ. 2 .AND. STRPAR(ISTR,9) .EQ. 0) THEN
            QSTRU2(ISTR) = MAX (0.0, QSTRU2(ISTR))
            IF (iDebug .ne. 0)  WRITE(IDEBUG,*) ' Correction backflow'
    !dec 2006: added check for inlet weir as well !!!!
        ElseIF (STRTYP(ISTR) .EQ. 6 .AND. STRPAR(ISTR,14) .EQ. 0) THEN
            QSTRU2(ISTR) = MAX (0.0, QSTRU2(ISTR))
            IF (iDebug .ne. 0)  WRITE(IDEBUG,*) ' Correction backflow'
        ENDIF
    ! ARS 16175: check pos. flow possible; dec 2006
        IF (STRTYP(ISTR) .EQ. 2 .AND. STRPAR(ISTR,15) .EQ. 0) THEN
            QSTRU2(ISTR) = Min (0.0, QSTRU2(ISTR))
            IF (iDebug .ne. 0)  WRITE(IDEBUG,*) ' Correction pos.flow'
        ElseIF (STRTYP(ISTR) .EQ. 6 .AND. STRPAR(ISTR,15) .EQ. 0) THEN
            QSTRU2(ISTR) = Min (0.0, QSTRU2(ISTR))
            IF (iDebug .ne. 0)  WRITE(IDEBUG,*) ' Correction pos.flow'
        ENDIF
        IF (iDebug .ne. 0) WRITE(IDEBUG,*) UPLVT,DOLVT, QSTRU2(ISTR), ' BACKFL', strpar(istr,9)

        crestlvl = Z
        Call VolumeCheck (ISTR, INODE, IBNDUP, IBND, iow, iowd, Qstru2(Istr),&
                          NODOWN, NODEUP, peilBottomDown, peilBottomUp, &
                          dolvt, uplvt, Uparea, DoArea, NtimLc, 1, crestlvl, Qstru1(istr))


        QTEMP(ITL) = QSTRU2(ISTR)
        if (idebug .ne. 0) then
           write(idebug,*) ' before volume-correction', timeSettings%timestepSize,ntimlc
           if (iow .gt. 0)  write(idebug,*) ' upstream ow', iow, vol0, qtemp(itl), tqin, qowupr(istr)
           if (iowd .gt. 0) write(idebug,*) ' downstream ow', iowd, dovol, qtemp(itl), tqindw, qowdwr(istr), qout0(iowd)
        endif

        IF (IOW .GT. 0) THEN
          VOL0 = VOL0 - QTEMP(ITL) * timeSettings%timestepSize / NTIMLC &
                      + TQIN * timeSettings%timestepSize / NTIMLC &
                      - QOWUPR(ISTR) * timeSettings%timestepSize / NTIMLC &
                      - QSTRU01(ISTR) * timeSettings%timestepSize / NTIMLC
          UPLVT0 = UPLVT
          Call RR_INTERP (NVAL, VolumeArrayUp, PeilArrayUp, VOL0, UPLVT, OwLastInterpIndex(iow))
    ! Jan 1996: UPLVT mag niet het maximum peil OWMNMA overschrijden
          UPLVT = MIN (UPLVT, OWMNMA(IOW))
        ELSEIF (IBNDUP .GT. 0) THEN
          UPLVT0 = UPLVT
        !Okt 97 bij randen check op peilvariatie via areaal
          Delta_volume = QTEMP(ITL)* timeSettings%timestepSize / NTIMLC
! Mar 2002 only change of UPLVT within timestep if switch CFBoundaryConstantInTimestep is not on
           if (CFBoundaryConstantInTimestep .eq. 0) then
              UPLVT  = UPLVL - Delta_volume / BNDPAR(ibndup,5)
           else
              UPLVT  = UPLVL
           endif
        ENDIF
    ! extra check of kleinere tijdstap nodig is
! May 2002: StructComp backw. compatibility option also for Orifice
        If (StructComp .eq. 0) then
          If ( ( ABS(UPLVT - UPLVT0) .GT. ABS(UPLVT0-DOLVT) .OR. &
                  ABS(UPLVT-UPLVT0) .GT. .05 .OR. &
                   ABS(DOLVT-DOLVT0) .GT. .05         )  .AND. &
                    NTIMLC .NE. NTIML2 .AND. timeSettings%timestepSize/NTIMLC .GT. NRSMIN) THEN
             IF (iDebug .ne. 0) WRITE(IDEBUG,*) ' Restart weir computations'
             NTimLc_Old = NTimLc
             NTIMLC = NTIML2
             IF (timeSettings%timestepSize/NTIMLC .LT. NRSMIN) &
                NTIMLC = timeSettings%timestepSize / NRSMIN
             nTimLc = max (NTimLc, NTimLc_Old + 1)
             GOTO 202
           Endif
        Elseif (StructComp .eq. 1) then
           If ( ( ABS(UPLVT - UPLVT0) .GT. ABS(UPLVT0-DOLVT) .OR. &
                   ABS(UPLVT-UPLVT0) .GT. .05 .OR. &
                    ABS(DOLVT-DOLVT0) .GT. .05         )  .AND. &
                     NTIMLC .NE. NTIML2 .AND. timeSettings%timestepSize/NTIMLC .GE. NRSMIN) THEN
             IF (iDebug .ne. 0) WRITE(IDEBUG,*) ' Restart weir computations'
             NTimLc_Old = NTimLc
             NTIMLC = Min (NTIML2, 2*TimeSettings%timestepSize/NrsMin)
             nTimLc = max (NTimLc, NTimLc_Old + 1)
             GOTO 202
          Endif
        ENDIF
    ! aanpassing benedenstrooms peil;
        IF (EiNode(NODOWN,3) .EQ. 4) THEN
    !nieuw gebruik inflow en outflow
           DOLVT0 = DOLVT
           DOVOL = DOVOL + TQINDW * timeSettings%timestepSize / NTIMLC &
                    + QOWDWR(ISTR) * timeSettings%timestepSize / NTIMLC &
                    + QTEMP(ITL) * timeSettings%timestepSize / NTIMLC &
                    - QOUT0(IOWD) * timeSettings%timestepSize / NTIMLC &
                    + QSTRU01(ISTR) * timeSettings%timestepSize / NTIMLC
           Call RR_INTERP (NVAL, VolumeArrayDown, PeilArrayDown, DOVOL, DOLVT, OwLastInterpIndex(iowd))
    ! Jan 1996: DOLVT mag niet het maximum peil OWMNMA overschrijden
           DOLVT = MIN (DOLVT, OWMNMA(IOWD))
         ELSEIF (EiNode(NODOWN,3) .EQ. 6) THEN
           DOLVT0 = DOLVT
           Delta_volume = QTEMP(ITL)* timeSettings%timestepSize / NTIMLC
! Mar 2002 only change of DOLVT within timestep if switch CFBoundaryConstantInTimestep is not on
           if (CFBoundaryConstantInTimestep .eq. 0) then
              DOLVT = DOLVT + Delta_volume / BNDPAR(ibnd,5)
           endif
         ENDIF

      ENDDO

      QSTRU2(ISTR) = 0.0
      DO ITL=1,NTIMLC
         QSTRU2(ISTR) = QSTRU2(ISTR) + QTEMP(ITL)
      ENDDO
      QSTRU2(ISTR) = QSTRU2(ISTR)/NTIMLC

      if (idebug .ne. 0) then
         Write(Idebug,*) ' End CmpStf2: Computed flow QStru2 =', Qstru2(istr), ' internal timesteps used',NTimLc
      Endif

    RETURN
  END subroutine cmpstf2



      SUBROUTINE Init1Structures

! *********************************************************************
! ***                D E L F T         H Y D R A U L I C S
! ***
! ***              WATER RESOURCES AND ENVIRONMENT DIVISION
! *********************************************************************
! *** Program :  DELFT_3B version 1.00                 Date: March 1995
! *********************************************************************
! *** Last update: March  1995       By : Geert Prinsen
! *********************************************************************
! *** Brief description:
! *** ------------------
! ***   Initialisatie per event for structures
! *********************************************************************
! *********************************************************************

      Implicit NONE

      Integer Istr

      Integer Idebug, Iout1
      Integer Rownr, TabelNr
      Logical DateTimeOutsideTable

      type (Date) currentDate
      type (Time) currentTime

      iOut1 = ConfFil_get_iOut1()
      iDebug = ConfFil_get_iDebug()



      DO ISTR = 1,NCSTRU
         STRSTA (ISTR) = 0
         MSFACT (ISTR) = 0
         OnOffMatLab (ISTR,1) = 0

         currentDate%year = ConfArr_get_IYear()
         currentDate%month = ConfArr_get_iMonth()
         currentDate%day = ConfArr_get_iDay()
         currentTime%hour = ConfArr_get_iHour()
         currentTime%minute = ConfArr_get_iMinute()
         currentTime%second = 0
         RowNr = -1
         TabelNr = StrRefInitTable (iStr)
         select case (strTyp(iStr))
           case (2, 6) ! 2, 6 stuw c.q.inlaatstuw
              If (TabelNr .gt. 0) then
                 StrPar (Istr,3) = GetNewValue(TableHandle, TabelNr, 1, RowNr, &
                                     CurrentDate, CurrentTime, Idebug, Iout1, DateTimeOutsideTable, .true. )
              Endif
           case (3, 7) ! 3, 7 onderlaat c.q.inlaat-onderlaat
              If (TabelNr .gt. 0) then
                 StrPar (Istr,4) = GetNewValue(TableHandle, TabelNr, 1, RowNr, &
                                     CurrentDate, CurrentTime, Idebug, Iout1, DateTimeOutsideTable, .true.)
                 StrPar (Istr,4) = StrPar(istr,4) + StrPar(istr,3)
              Endif
         end select
      Enddo


      Return
      END Subroutine Init1Structures



      SUBROUTINE INIT2str (IDAYWK, ITMSTP)

! *********************************************************************
! ***                D E L F T         H Y D R A U L I C S
! ***
! ***              WATER RESOURCES AND ENVIRONMENT DIVISION
! *********************************************************************
! *** Program :  DELFT_3B version 1.00                 Date: March 1995
! *********************************************************************
! *** Last update: March  1995       By : Geert Prinsen
! *********************************************************************
! *** Brief description:
! *** ------------------
! ***   Initialisatie per tijdstap in een event for structures only
! *********************************************************************
! *** Input/output parameters:
! *** ------------------------
! ***  IDEBUG = debug file unit
! ***  IHOUR  = uur
! ***  IDAYWK = dag van de week
! *********************************************************************

      IMPLICIT NONE

      INTEGER IDAYWK, iOW, iNode, iStr, iOwUp
      Integer iTmStp, iOwDo, iOwDwn, iOwD
      REAL    TLVL, RLVL1, RLVL2, HVol, HVol2
      Integer iDebug, Iout1, iHour
      Real    startLevelLow, startLevelHigh, stopLevelLow, stopLevelHigh
      Real    startLevel, stopLevel  !, QCapController
      Real    ToleranceCheck

      Logical DateTimeOutsideTable, switch
      Integer Rownr, TabelNr

      type (Date) currentDate
      type (Time) currentTime

      iOut1 = ConfFil_get_iOut1()
      iDebug = ConfFil_get_iDebug()

!     ToleranceCheck = 0.0
      ToleranceCheck = 0.000001

      IF (iDebug .ne. 0)  WRITE(IDEBUG,*) ' INIT2str: IHOUR=', ConfArr_get_IHOUR()

      DO INODE = 1, NCNODE
       IF (EiNode(INODE,3) .EQ. 5) THEN
         ISTR = EiNode(INODE, 2)

         QSTRu01(istr) = 0.0
         QSTRu02(istr) = 0.0
!gp aug 1996
!only if timestep not equal to first timestep then:
         IF (ITMSTP .GT. 1) THEN
            currentDate%year = ConfArr_get_IYear()
            currentDate%month = ConfArr_get_iMonth()
            currentDate%day = ConfArr_get_iDay()
            currentTime%hour = ConfArr_get_iHour()
            currentTime%minute = ConfArr_get_iMinute()
            currentTime%second = 0

          IF (STRTYP(ISTR) .EQ. 1) THEN
! uitlaatgemaal, bij open water IOWUP, afslagpeilen (lg/hg) RLVL1 RLVL2
           IOWUP = UPNODE(INODE)
           IOW   = EiNode(IOWUP, 2)
           Tlvl = CurrentTargetLevel (Iow)  !Call DTTLVL(TLVL, IOW)
!          Call DTINDX(Summer, IDAYWK, INDX1, INDX2, INDX3, INDX4, iStr)

            iHour = ConfArr_get_iHour()

            ! nieuwe methode   ! Volgorde kolommen nu volgens invoerfile, dus afwijkend van oude volgorde!!

            Call DetermineStartStopLevels (StartLevelLow, StartLevelHigh, StopLevelLow,  StopLevelHigh, &
                                           Tlvl, Rlvl1, Rlvl2, Hvol, Hvol2, Istr, INode, Iow, IDayWk, Idebug, 1)

! Call DetermineStartStopLevels ipv stuk nu verwijderde code
!  NB in DetermineStartStopLevels gebeuren 3 extra dingen tov wat hier eerst stond
!       - controle op via Matlab gezette aan/afslagpeilen (moet goed gaan omdat dit voor Itmstp>1 gebeurd)
!       - correctie van start/stoplevels highpumpcap in case no high pump capacities
!       - controle op volume van de gerelateerde open waterknoop

           IF (QSTRU(ISTR) .LE. 0.0) THEN
              STRSTA(ISTR)= 0
           ElseIf (STRPAR(ISTR,19) .eq. 5.0) THEN
              ! structure with time controller, only check for switch off;
              ! Call CheckTimeControllerPump (Istr, QCapController, Idebug, Iout1)
              ! If (Qstru(Istr) .lt. QCapController) StrSta(Istr) = 0
              ! ARS xxxx: also take into account pump reduction factor here, otherwise pump switches off too early!
           ELSEIF (QSTRU(ISTR) .LT. (pumpCapacities(iStr)%low - ToleranceCheck) * PumpReductionFactor(iStr) ) THEN
              switch = .false.
              IF (StructureOperation .ne. 0 .and. StrSt0(istr) .ne. 0) THEN
                 switch = .true.
              ELSEIF (StructureOperation .eq. 0) Then
                 switch = .true.
              Endif
              If (switch) then
                IF (STRSTA(ISTR) .NE. 0 .AND. iDebug .ne. 0) THEN
                   WRITE(IDEBUG,*) ' Switch-off structure',ISTR
                   WRITE(IDEBUG,*) ' QSTRU(ISTR) was ',QSTRU(ISTR)
                ENDIF
                STRSTA(ISTR)= 0
              Endif
              ! ARS xxxx: also take into account pump reduction factor here, otherwise pump switches off too early!
           ELSEIF (QSTRU(ISTR) .LT.  (pumpCapacities(iStr)%low +  &
                                        pumpCapacities(iStr)%high - ToleranceCheck) &
                                        * PumpReductionFactor(iStr)) Then
              switch = .false.
              IF (StructureOperation .ne. 0 .and. StrSt0(istr) .ne. 0) THEN
                 switch = .true.
              ELSEIF (StructureOperation .eq. 0) Then
                 switch = .true.
              Endif
              If (switch) then
                IF (STRSTA(ISTR) .EQ. 2 .AND. iDebug .ne. 0) THEN
                   WRITE(IDEBUG,*) ' Switching to low capacity structure',ISTR
                   WRITE(IDEBUG,*) ' QSTRU(ISTR) was ',QSTRU(ISTR)
                   WRITE(IDEBUG,*) ' Total pump cap was ', pumpCapacities(iStr)%low + pumpCapacities(iStr)%high
                ENDIF
                STRSTA(ISTR)= 1
              Endif
           ENDIF
          ELSEIF (STRTYP(ISTR) .EQ. 8) THEN
! inlaatgemaal, bij open water IOWDO, afslagpeilen (lg/hg) RLVL1 RLVL2
           IOWDO = DONODE(INODE)
           IOW   = EiNode(IOWDO, 2)
           Tlvl = CurrentTargetLevel (Iow)  !Call DTTLVL(TLVL, IOW)
!          Call DTINDX(SUMMER, IDAYWK, INDX1, INDX2, INDX3, INDX4, iStr)

           ! nieuwe methode   ! Volgorde kolommen nu volgens invoerfile, dus afwijkend van oude volgorde!!

           Call DetermineStartStopLevels (StartLevelLow, StartLevelHigh, StopLevelLow,  StopLevelHigh, &
                                          Tlvl, Rlvl1, Rlvl2, Hvol, Hvol2, Istr, INode, Iow, IDayWk, Idebug, 2)

! Call DetermineStartStopLevels ipv nu verwijderde code
!  NB in DetermineStartStopLevels gebeuren 3 extra dingen tov wat hier eerst stond
!       - controle op via Matlab gezette aan/afslagpeilen (moet goed gaan omdat dit voor Itmstp>1 gebeurd)
!       - correctie van start/stoplevels highpumpcap in case no high pump capacities
!       - controle op volume van de gerelateerde open waterknoop

           If (STRPAR(ISTR,19) .eq. 5.0) THEN
              ! structure with time controller, only check for switch off
              ! Call CheckTimeControllerPump (Istr, QCapController, Idebug, Iout1)
              ! If (Qstru(Istr) .lt. QCapController) StrSta(Istr) = 0
              ! ARS xxxx: also take into account pump reduction factor here, otherwise pump switches off too early!
           ELSEIF (QSTRU(ISTR) .LT. (pumpCapacities(iStr)%low - ToleranceCheck) * PumpReductionFactor(iStr) ) THEN
              switch = .false.
              IF (StructureOperation .ne. 0 .and. StrSt0(istr) .ne. 0) THEN
                 switch = .true.
              ELSEIF (StructureOperation .eq. 0) Then
                 switch = .true.
              Endif
              If (switch) then
                IF (STRSTA(ISTR) .NE. 0 .AND. iDebug .ne. 0) THEN
                   WRITE(IDEBUG,*) ' Switch-off structure',ISTR
                   WRITE(IDEBUG,*) ' QSTRU(ISTR) was ',QSTRU(ISTR)
                ENDIF
                STRSTA(ISTR)= 0
              Endif
              ! ARS xxxx: also take into account pump reduction factor here, otherwise pump switches off too early!
           ELSEIF (QSTRU(ISTR) .LT. (pumpCapacities(iStr)%low +  &
                                       pumpCapacities(iStr)%high - ToleranceCheck) &
                                        * PumpReductionFactor(iStr)) Then
              switch = .false.
              IF (StructureOperation .ne. 0 .and. StrSt0(istr) .ne. 0) THEN
                 switch = .true.
              ELSEIF (StructureOperation .eq. 0) Then
                 switch = .true.
              Endif
              If (switch) then
                IF (STRSTA(ISTR) .EQ. 2 .AND. iDebug .ne. 0) THEN
                   WRITE(IDEBUG,*) ' Switching to low capacity structure',ISTR
                   WRITE(IDEBUG,*) ' QSTRU(ISTR) was ',QSTRU(ISTR)
                   WRITE(IDEBUG,*) ' Total inlet pump cap was ', pumpCapacities(iStr)%low + pumpCapacities(iStr)%high
                ENDIF
                STRSTA(ISTR)= 1
              Endif
           ENDIF

          ELSEIF (STRTYP(ISTR) .EQ. 6) THEN
! inlaatstuw
           IOWDWN = DONODE(INODE)
           IOWD   = EiNode(IOWDWN,2)
           RowNr = -1
           TabelNr = StrRefOnOffTable (iStr)
           StartLevel = GetNewValue(TableHandle, TabelNr, 1, RowNr, CurrentDate, CurrentTime, &
                                    Idebug, iout1, DateTimeOutsideTable, .true. )
           StopLevel  = GetNewValue(TableHandle, TabelNr, 2, RowNr, CurrentDate, CurrentTime, &
                                    Idebug, iout1, DateTimeOutsideTable, .true. )
           if (idebug .ne. 0)  write(idebug,*) ' New Method OnOffLevels', StartLevel, StopLevel
           Tlvl = CurrentTargetLevel (Iowd)  !Call DTTLVL(TLVL, IOWD)
           RLVL1 = TLVL + stopLevel
           IF (LVLOW(IOWD) .GT. RLVL1) THEN
              IF (iDebug .ne. 0) WRITE(IDEBUG,*) ' Switch-off inletweir'
              STRSTA(ISTR) = 0
           ENDIF
! end inlaatstuw

          ELSEIF (STRTYP(ISTR) .EQ. 7) THEN
! inlaatonderlaat
! analoog aan inlaatstuw
           IOWDWN = DONODE(INODE)
           IOWD   = EiNode(IOWDWN,2)
           RowNr = -1
           TabelNr = StrRefOnOffTable (iStr)
           StartLevel = GetNewValue(TableHandle, TabelNr, 1, RowNr, CurrentDate, CurrentTime, &
                                    Idebug, iout1, DateTimeOutsideTable, .true. )
           StopLevel  = GetNewValue(TableHandle, TabelNr, 2, RowNr, CurrentDate, CurrentTime, &
                                    Idebug, iout1, DateTimeOutsideTable, .true. )
           if (idebug .ne. 0)  write(idebug,*) ' New Method OnOffLevels', StartLevel, StopLevel
           Tlvl = CurrentTargetLevel (Iowd)  !Call DTTLVL(TLVL, IOWD)
           RLVL1 = TLVL + stopLevel
           IF (LVLOW(IOWD) .GT. RLVL1) THEN
              IF (iDebug .ne. 0) WRITE(IDEBUG,*) ' Switch-off inletorifice'
              STRSTA(ISTR) = 0
           ENDIF

! end inlaatonderlaat
          ENDIF
         ENDIF

! and do for all timesteps only this:
         STRST0(ISTR) = STRSTA(ISTR)
         QOWUPR(ISTR) = 0.0
         QOWDWR(ISTR) = 0.0
       ENDIF
      ENDDO


      Return
      END Subroutine Init2Str



      Subroutine WrInputDataStructures (Iout9, Iout6, RnDate, RnTime)
        ! *********************************************************************
        ! *** Last update:
        ! *********************************************************************
        ! *** Brief description:
        ! *** ------------------
        ! ***    Stuk uit sub WrInputData: uitvoer van Structures in *.Out files
        ! *********************************************************************

        Implicit none

        Integer      INODE, IKIND, INR, i, k
        Integer      IOUT9, IOUT6
        Integer*2    RNDATE(3), RNTIME(4)
        CHARACTER(len=20) NAMSTR(8)

        NAMSTR (1) = ' Pumpstation'
        NAMSTR (2) = ' Weir'
        NAMSTR (3) = ' Gate'
        NAMSTR (4) = ' Resistance'
        NAMSTR (5) = ' Q-h relation'
        NAMSTR (6) = ' Inletweir'
        NAMSTR (7) = ' Inletgate'
        NAMSTR (8) = ' Inlet pumpstation'

! Kunstwerken
      IF (NCSTRU .GT. 0) THEN
         WRITE(IOUT9,15)
   15    FORMAT (//,' Summary input data structures',//, &
              ' Node identification    Node      Type          Parameters',/,&
              '                        name      structure     (max. 18) ',/,236('='))
         DO INODE =1,NCNODE
          IKIND = EiNode(INODE,3)
          INR   = EiNode(INODE,2)
                IF (IKIND .EQ. 5) THEN
                  WRITE(IOUT9,25) Id_Nod(INODE), &
                            NamNod(INODE),&
                            NAMSTR(STRTYP(INR)), (STRPAR(INR,K),K=1,NVAL3)
   25       FORMAT (A20,1X,A12,1X,A20,18(F9.3,1X))
          ENDIF
         ENDDO
      ENDIF

! RR structures
      If (ncstru .gt. 0 .and. OutputDesired(5) ) then
        WRITE(IOUT6,51) RNDATE(3),RNDATE(2),RNDATE(1),(RNTIME(I),I=1,2), CASENM
   51   FORMAT (' Run datum (DD-MM-YY) and time:',I2,'-',I2,'-',I4,2X,I2,':',I2,//,&
                ' Case name                 ',A20,/)
        WRITE(IOUT6,1009) '[m3/s]'
 1009   FORMAT(//,' Maxima per event',//,&         ! structures
                  ' Event   Start     Node identification   Node ',10X,'Maximum_flow  ',/,&
                  '  nr  year-mon-day', 23X, 'name ',14X,A6,/,70('='))
      Endif


      Return
      END subroutine WrInputDataStructures


  Subroutine Wr1OutStructure (Iout6, Ievent, Month, INode, IStr)
    ! *********************************************************************
    ! *** Last update:
    ! *********************************************************************
    ! *** Brief description:
    ! *** ------------------
    ! ***    Stuk uit sub Wr1Out: uitvoer van Paved node: maxima per event in OUT file
    ! *********************************************************************

      Implicit none

    ! variables

    Integer     INODE, IStr, Iout6, Ievent
    Real        QFlw
    CHARACTER(len=3) MONTH(12)

           if (.not. associated(QSTRMX)) return  ! If there is nothing, do nothing

! flow in m3/s
           QFLW = QSTRMX(ISTR,IEVENT)
           WRITE(IOUT6,1010) IEVENT, EventStartDateTime(IEVENT,1),MONTH(EventStartDateTime(IEVENT,2)), &
                EventStartDateTime(IEVENT,3),&
                Id_Nod(INODE),NamNod(INODE),QFLW
 1010      FORMAT (I4,1X,I4,1X,A3,1X,I3,3X,A20,1X,A15,1X,F12.3)

  Return
  END subroutine Wr1OutStructure



  Subroutine Structure_DeAllocateArrays

    if (Allocated(PumpCapacities)) DeAllocate(PumpCapacities)

  Return
  End subroutine Structure_DeallocateArrays



end module Structures
