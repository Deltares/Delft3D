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
! at:               $Modtime:: 29-08-97 1:44p   $
!
! current revision: $Revision:: 6               $


module Openwater

  use Network
  use Conf_Fil
  use Conf_Arr
  use Crop
  use NewTables
  use RR_Meteo
  use DH_alloc
  use ReadLib

  IMPLICIT NONE !!!

  ! variables
  ! *** Data open water

! ARS 1887 (Scurve) deel B: open water berekeningen met extra bergend oppervlak
! OpenwaterLevelComputations = 0 Simple
!                            = 1 Default: Advanced, met bergend oppervlak onverhard evt. met Scurve
! OpenwaterPrecipitationComp = 0 Default, als voorheen (variabel oppervlak)
!                              1 constant oppervlak voor neerslag, nl. corr. met laagste maaiveld.
!                                indien geen aanliggend vhg/ovh/kasknoop, dan oppervlak bij max. toelaatbaar peil.
! OpenwaterSeepageComp       = 0 Default, als voorheen (variabel oppervlak)
!                              1 constant oppervlak voor seepage, nl. corr. met laagste maaiveld.
!                                indien geen aanliggend vhg/ovh/kasknoop, dan oppervlak bij max. toelaatbaar peil.
! NStorageCurvePoints    = Aantal subgebieden voor berekeningen met extra bergend oppervlak
! StorageAreaCurve       = de extra bergend oppervlak curve
! OpenWaterRelatedNodes  = array met aan ow knoop gerelateerde knopen (max. NrRelatedNodes)

  Integer OpenWaterLevelComputations, OpenWaterPrecipitationComp, OpenwaterSeepageComp
  integer NStorageCurvePoints, NRelatedNodes
  parameter (NStorageCurvePoints=500)  ! =5*UseUnpavedScurveMax
  parameter (NRelatedNodes=25)

! toch dmv aparte arrays, ivm gebruikmaking van algemene zoek/sorteer routines
!  type StorageAreaCurve
!     Real Level           !Peil
!     Real Area            !Extra oppervlak bij dit peil
!     Real Volume          !Extra geborgen volume tot en met dit peil (vanaf het laagste peil)
!  end type StorageAreaCurve

  Real, Pointer, save :: ExtraBergendOppPeil(:,:)
  Real, Pointer, save :: ExtraBergendOppOppervlak(:,:)
  Real, Pointer, save :: ExtraBergendOppVolume(:,:)
  Real, Pointer, save :: ActualExtraBergendVolume(:), PreviousExtraBergendVolume(:)
  Integer, Pointer, save :: OpenWaterRelatedNodes(:,:), NrBergendOppervlakIndices(:)
  Integer, Pointer, save :: OpwRefOW_TTABLE(:), OpwRefH0_TTable(:), OpwRefSeepage_TTable(:) , OpwRefSeepageConc_TTable(:)
  Logical   OwOnLineModFlowUsed
! ARS 6345
!  Relatie peil - volume, rekening houdend met open water en met bergend oppervlak op land
!  Tevens totaal volume en initieel totaal volume
  Real, Pointer, save :: TotalOwBergPeil(:,:)
  Real, Pointer, save :: TotalOwBergVolume(:,:)
  Real, Pointer, save :: TotalVolume(:), PreviousVolume(:)

! end declaraties ARS 1887B

  ! *** OWNAM       = geeft bij index iow de knoop index INODE
  ! *** OWPrecipNAM = geeft bij index iow de knoop index INODE
  Integer, Pointer, SAVE ::      OwNam(:)
  Integer, Pointer, SAVE ::      OwPrecipNam(:)
  REAL, Pointer, SAVE ::         AREAOW(:,:), PEILOW(:,:), &
                                     VOLUOW(:,:), &
                                     REFLVL(:), MAXLVL(:), &
                                     SUMLVL(:), WINLVL(:), &
                                     OWKWEL(:), OWWEGZ(:), &
                                     OWMNMA(:)
  ! variabele kwel/wegzijging
  ! Cvalue = weerstandswaarde in dagen, bij variabele kwelbepaling via H0 als tabel hetzij uit Modflow
  Integer, Pointer, SAVE ::  OWSeepageCompOption(:)
  REAL, Pointer, SAVE ::  OWCValue(:), OWH0Actual(:)

  INTEGER, Pointer, SAVE ::      SUMFRO(:,:), SUMTO(:,:)

  ! *** Results open water
  ! *** CurrentTargetLevel = target level open water
  ! *** LVLOW0 = initial level open water
  ! *** VOLOW0 = initial volume open water
  ! *** AROW0  = initial area
  ! *** AROW   = computed area
  ! *** RainArea = area for rainfall
  ! *** SeepageArea = area for seepage
  ! *** LVLOW  = computed level open water
  ! *** VOLOW  = computed volume open water
  ! *** ROW    = rainfall open water
  ! *** VOW    = verdamping open water
  ! *** KWOW   = kwel-wegzijging open water
  ! *** QOUT0  = outflow open water previous iteration
  ! *** QOUTOW = outflow open water current iteration
  ! *** QINOW  (.,1)  = from related verhard gebied
  ! *** QINOW  (.,2)  = from related onverhard gebied
  ! *** QINOW  (.,3)  = from related kasgebied
  ! *** QINOW  (.,4)  = from structures
  ! *** QINOW  (.,5)  = from RWZI
  ! *** QINOW  (.,6)  = from Industry
  ! *** QINOW  (.,7)  = from Sacramento / RRRunoff
  ! *** QIN0     ,2)  = inflow from onverhard gebied previous iteration
  ! *** QIN0     ,4)  = inflow from structures previous iteration
  ! *** QIN0     ,5)  = inflow from RWZI previous iteration
  ! *** QIN0     ,6)  = inflow from industry previous iteration
  ! ***

  Double precision, Pointer, SAVE :: VOLOW(:), VOLOW0(:)
  REAL, Pointer, SAVE :: &
               LVLOW(:),  AROW(:),  &
               LVLOW0(:), AROW0(:), &
               CurrentTargetLevel(:),  &
               RainArea(:), SeepageArea(:), ROW(:), VOW(:), KWOW(:), &
               QIN0(:,:), QOUT0(:), QOUTOW(:), QINOW(:,:)


  Real, Pointer :: bottomLevel(:)


!Open water output
 ! *** OWMLVL = 1 = maximum open water level for each event
 ! ***          2 = maximum open water volume for each event
 ! ***          3 = maximum neerslag          for each event
 ! ***          4 = maximum verdamping        for each event
 ! ***          5 = maximum kwel-wegzijging   for each event
 ! ***          6 = maximum iteration error   for each event
 ! ***          7 = maximum filling percentage for each event
 ! *** OWEXC (,1) = 0/1 indicator whether reference open water level
 ! ***              is exceeded (1=yes, 0=no)
 ! ***       (,2) = duration of exceedance (in seconds),
 ! ***              total for each event

   REAL, Pointer, SAVE ::  OWMLVL(:,:,:)
   INTEGER, Pointer, SAVE :: OWEXC(:,:,:)

   Integer, Pointer, SAVE :: OwLastInterpIndex (:), OwBergendOppLastInterpIndex (:), OwTotalLastInterpIndex (:)

   logical  VullingsgraadMaximum100
   logical  FixARS13964, ReduceInfiltrationAtNegativeOpenWaterVolume

  ! *** OWRainArea = constant open water area for simple openwater precipitation
  ! *** ROWRain    = rainfall open water precipitation node
  ! *** VOWRain    = verdamping open water precipitation node
  Integer, Pointer, SAVE :: OWRainBnd(:)
  REAL, Pointer, SAVE :: OWRainArea(:), ROWRain(:), VOWRain(:)

  integer indexevap, indexprecip
contains


  Subroutine Openwater_confAr1(iOut1)
    ! *********************************************************************
    ! *** Last update: 6 March  1997                    By : Peter Schrier
    ! *********************************************************************
    ! *** Brief description:
    ! *** ------------------
    ! ***   Alloceren van geheugenruimte voor alle arrays van open water
    ! *********************************************************************
    ! *** Input/output parameters:
    ! ***   geen
    ! *********************************************************************

    Integer iOut1
    Logical Success

    NOW  = MAX (1, NCOW   ) !openwater
    IF ((NCOW  .GT. 0) .and. (iOut1 .ne. 0)) then
      WRITE(IOUT1,*) ' Open water            =',NOW
    end if

! owrain
    NOWRain  = MAX (1, NCOWRain   ) !openwater precip only
    IF ((NCOWRain  .GT. 0) .and. (iOut1 .ne. 0)) then
      WRITE(IOUT1,*) ' Open water Precip/Evap=',NOWRain
    end if

    !*** Data open water precip/evap only
    success = Dh_AllocInit (NOwRain, OWRainArea, 0E0)
    success = Dh_AllocInit (NOwRain, OWRainBnd, 0)

    !*** Data open water

    success = Dh_AllocInit (NVal, NOw, AreaOw, PeilOw, VoluOw, 0E0)
    success = success .and. Dh_AllocInit (NOw, RefLvl, MaxLvl, 0E0)
    success = success .and. Dh_AllocInit (NOw, SumLvl, WinLvl, -999.99E0)
    success = success .and. Dh_AllocInit (NOw, OWKwel, OwWegZ, 0E0)
    success = success .and. Dh_AllocInit (NOw, OwMnMa, 99999E0)
    success = success .and. Dh_AllocInit (NOw, BottomLevel, 0E0)
    success = success .and. Dh_AllocInit (NOw, OwSeepageCompOption, 0)
    success = success .and. Dh_AllocInit (NOw, OwCValue, OwH0Actual, 0E0)
    success = success .and. Dh_AllocInit (NOw, 2, SumFro, SumTo, 0)
    success = success .and. Dh_AllocInit (NOw, OWNam, 0)
    If (.not. success) call ErrMsgStandard (981, 0, ' Error allocating arrays in subroutine ', ' OpenWater_ConfAr1')
!   ALLOCATE ( AREAOW(NVAL,NOW), PEILOW(NVAL,NOW), &
!              VOLUOW(NVAL,NOW), &
!              REFLVL(NOW), MAXLVL(NOW), &
!              SUMLVL(NOW), WINLVL(NOW), &
!              OWKWEL(NCOW), OWWEGZ(NCOW), &
!              OWMNMA(NOW), bottomLevel(nOW), Stat=Allocation_Error )
!   ALLOCATE ( OWSeepageCompOption(NOW), Stat=Allocation_Error )
!   ALLOCATE ( OWCValue(NOW), OWH0Actual(NOW), Stat=Allocation_Error )
!   ALLOCATE ( SUMFRO(NOW,2), SUMTO(NOW,2), Stat=Allocation_Error )

! owrain computations
    success = success .and. Dh_AllocInit (NOwRain, OWPrecipNam, 0)
    success = success .and. Dh_AllocInit (NOwRain, ROWRain, VowRain, 0E0)

   !*** Results open water
    success = success .and. Dh_AllocInit (NOw, VolOw, VolOw0, 0D0)
    success = success .and. Dh_AllocInit (NOw, LvlOw, LvlOw0, 0E0)
    success = success .and. Dh_AllocInit (NOw, ArOw,  ArOw0, 0E0)
    success = success .and. Dh_AllocInit (NOw, CurrentTargetLevel, 0E0)
    success = success .and. Dh_AllocInit (NOw, RainArea, 0E0)
    success = success .and. Dh_AllocInit (NOw, SeepageArea, 0E0)
    success = success .and. Dh_AllocInit (NOw, ROW, Vow, KwOw, 0E0)
    success = success .and. Dh_AllocInit (NOw, Qout0, QoutOW, 0E0)
    success = success .and. Dh_AllocInit (NOw, 7, QInOw, Qin0, 0E0)
    If (.not. success) call ErrMsgStandard (981, 0, ' Error allocating arrays in subroutine ', ' OpenWater_ConfAr1')
!   ALLOCATE ( VOLOW (NOW), VOLOW0(NOW), Stat=Allocation_Error )
!    ALLOCATE ( LVLOW (NOW), AROW (NOW), &
!               LVLOW0(NOW), AROW0(NOW), &
!               CurrentTargetLevel(NOW),  &
!               RainArea(NOW), ROW(NOW), VOW (NOW), KWOW(NOW), QIN0(NOW,6), &
!               QOUT0 (NOW), QOUTOW(NOW), QINOW (NOW,6), Stat=Allocation_Error )

!   do teller = 1, now
!      owmnma(teller) = 99999.0
!   end do

!StorageAreaCurve extra bergend oppervlak
! met type ALLOCATE ( ExtraBergendOppervlak(NOW,NStorageCurvePoints) )
    success = success .and. Dh_AllocInit (NStorageCurvePoints, Now, ExtraBergendOppPeil, 0E0)
    success = success .and. Dh_AllocInit (NStorageCurvePoints, Now, ExtraBergendOppOppervlak, 0E0)
    success = success .and. Dh_AllocInit (NStorageCurvePoints, Now, ExtraBergendOppVolume, 0E0)
    If (.not. success) call ErrMsgStandard (981, 0, ' Error allocating arrays in subroutine ', ' OpenWater_ConfAr1')
!    allocate ( ExtraBergendOppPeil     (NStorageCurvePoints,Now), Stat=Allocation_Error )
!    allocate ( ExtraBergendOppOppervlak(NStorageCurvePoints,Now), Stat=Allocation_Error )
!    allocate ( ExtraBergendOppVolume   (NStorageCurvePoints,Now), Stat=Allocation_Error )
    success = success .and. Dh_AllocInit (Now, ActualExtraBergendVolume, PreviousExtraBergendVolume, 0E0)
    If (.not. success) call ErrMsgStandard (981, 0, ' Error allocating arrays in subroutine ', ' OpenWater_ConfAr1')
!    Allocate ( ActualExtraBergendVolume(NOW), Stat=Allocation_Error )
!    Allocate ( PreviousExtraBergendVolume(NOW), Stat=Allocation_Error )
    success = success .and. Dh_AllocInit (Now, NRelatedNodes, OpenWaterRelatedNodes, 0)
    success = success .and. Dh_AllocInit (Now, NrBergendOppervlakIndices, 0)
    success = success .and. Dh_AllocInit (Now, OpwRefOW_TTable, OpwRefH0_TTable, OpwRefSeepage_TTable, 0)
    success = success .and. Dh_AllocInit (Now, OpwRefSeepageConc_TTable, 0)
    If (.not. success) call ErrMsgStandard (981, 0, ' Error allocating arrays in subroutine ', ' OpenWater_ConfAr1')
!   ALLOCATE ( OpenWaterRelatedNodes(NOW,NRelatedNodes), Stat=Allocation_Error )
!   ALLOCATE ( NrBergendOppervlakIndices(NOW), Stat=Allocation_Error )
!   Allocate ( OpwRefOW_TTABLE (NOW), OpwRefH0_TTABLE (NOW), OpwRefSeepage_TTable(NOW), Stat=Allocation_Error )

! Ars 6345
    success = success .and. Dh_AllocInit (NStorageCurvePoints, Now, TotalOwBergPeil, 0E0)
    success = success .and. Dh_AllocInit (NStorageCurvePoints, Now, TotalOwBergVolume, 0E0)
    success = success .and. Dh_AllocInit (Now, TotalVolume, PreviousVolume, 0E0)
    If (.not. success) call ErrMsgStandard (981, 0, ' Error allocating arrays in subroutine ', ' OpenWater_ConfAr1')
!   allocate ( TotalOwBergPeil     (NStorageCurvePoints,Now), Stat=Allocation_Error )
!   allocate ( TotalOwBergVolume   (NStorageCurvePoints,Now), Stat=Allocation_Error )
!   Allocate ( TotalVolume(NOW), PreviousVolume(NOW), Stat=Allocation_Error )

  return
  End subroutine Openwater_confAr1


  Subroutine OpenWaterOutput_Confar (Nevnt)
    Integer Nevnt
    Logical Success

    success = Dh_AllocInit (NcOw, Nevnt, 8, OwMlvl, -999.99E0)
    success = success .and. Dh_AllocInit (NcOw, 2, Nevnt, OwExc, 0)
    success = success .and. Dh_AllocInit (NcOw, OwBergendOppLastInterpIndex, 1)
    success = success .and. Dh_AllocInit (NcOw, OwLastInterpIndex, 1)
    success = success .and. Dh_AllocInit (NcOw, OwTotalLastInterpIndex, 1)
    If (.not. success) call ErrMsgStandard (981, 0, ' Error allocating arrays in subroutine ', ' OpenWater_OutputConfAr')
!    ALLOCATE  ( OWMLVL(NcOW,NEvnt,8), OWEXC(NcOW,2,Nevnt), &
!                OwBergendOppLastInterpIndex(NcOW), &
!                OwLastInterpIndex(NcOW), OwTotalLastInterpIndex(NcOW), Stat=Allocation_Error )

!   OwLastInterpIndex = 1
!   OwBergendOppLastInterpIndex = 1
!   OwTotalLastInterpIndex = 1

    Return
  End subroutine OpenwaterOutput_confAr



  subroutine Openwater_readAsciiInput(infile1, infile2, infile3)
    ! *********************************************************************
    ! *** Last update: 21 March  1997                    By : Peter Schrier
    ! *********************************************************************
    ! *** Brief description:
    ! *** ------------------
    ! ***   Ascii files lezen met gegevens openwater
    ! *********************************************************************
    ! *** Input/output parameters:
    ! *** input: infile1,infile2,infile3 : 3 ascii files
    ! *********************************************************************

    Integer :: RetVal

    Real          owSeepage, ScSeepage
    Character(CharIdLength) name, id
    Integer(4)    Infile1, Infile2, Infile3
    Integer       teller, teller1, iecode, iout1, index, inod, idebug
    Integer       IOw
    Logical       allow, found, endfil, Err969
    Integer       IDUM(32), dummyCompOption
    REAL          RDUM(32), c1dum, c2dum, dummyCValue, dummyKwel
    Character(CharIdLength) CDUM(32), Tablename, DummyH0Table, NodeId, dummyConcTable
    Character(CharIdLength), Pointer :: SEPDEF(:), TLVDEF(:), H0Def(:), SaltConcDef(:)
    Character(1000) string
    Integer       NrColumns, TableNr
    Logical       occurs, TabYesNo, TargetTableDefined, H0Defined, SeepageTableDefined, SeepageConcTableDefined
    Integer, Pointer :: ReferenceToDefinition(:)
    Logical, Pointer :: AlreadyRead(:)

    Character(Len=CharIdLength)  FileName
    Character(Len=1000000)       KeepBufString
    Integer                      IoUnit, LenString, ipos

    Logical Success

    success = Dh_AllocInit (NcOw, SepDef, TlvDef, H0Def, ' ')
    success = Dh_AllocInit (NcOw, SaltConcDef, ' ')
    success = success .and. Dh_AllocInit (NcOw, ReferenceToDefinition, 0)
    success = success .and. Dh_AllocInit (NcOw, AlreadyRead, .false. )
    If (.not. success) call ErrMsgStandard (981, 0, ' Error allocating arrays in subroutine ', ' OpenWater_ReadAscii')

!   ALLOCATE   (SEPDEF(ncow), TLVDEF(ncow), H0Def(ncow), Stat=Allocation_Error )
!   ALLOCATE   (ReferenceToDefinition(NCow), Stat=Allocation_Error )
!   ALLOCATE   (AlreadyRead(NCow), Stat=Allocation_Error )

    H0Defined = .false.
    SeepageTableDefined = .false.
    SeepageConcTableDefined = .false.
    TargetTableDefined = .false.
    allow = .false.
    found = .false.
    iOut1  = ConfFil_get_iOut1()
    iDebug = ConfFil_get_iDebug()


! *********************************************************************
! resetten parameters
! *********************************************************************
!   Areaow  = 0
!   Peilow  = 0
!   Voluow  = 0
!   Bottomlevel = 0
!   Maxlvl = 0
!   Reflvl = 0
!   Lvlow  = 0
!   AlreadyRead = .false.
!   SepDef = ''
!   TlvDef = ''
!   OpwRefOW_TTable = 0
!   OpwRefH0_TTable = 0
!   OpwRefSeepage_TTable = 0

! *********************************************************************
! ***  If CleanRRFiles, also write cleaned input
! *********************************************************************
   if (CleanRRFiles) then
        FileName = ConfFil_get_namFil(49)
        FileName(1:) = Filename(1:Len_trim(FileName)) // '_cleaned'
        Call Openfl (iounit, FileName,1,2)  !paved.3b_cleaned
        Write(*,*) ' Cleaning openwate.3b to file:', FileName
        Write(iout1,*) ' Cleaning openwate.3b to file:', FileName
   endif

! *********************************************************************
! Read openwate.3b file
! *********************************************************************
   call SetMessage(LEVEL_DEBUG, 'Read Openwate.3b file')
   if (idebug .ne. 0) write(idebug,*) ' Read openwate.3b file'
   Endfil = .false.
   teller = 0
   RetVal = 0
   CALL SKPCOM (Infile1, ENDFIL,'ODS')
   do while (.not. endfil)
     READ(infile1,'(A1000)',END=21,ERR=150,IOSTAT=IECODE) STRING
! Skip regel als hij niet begin met juist keyword (PAVE)
     IF (STRING(1:4) .EQ. 'OPWA') Then
! open water id
       RetVal = RetVal + GetVAR2 (STRING,' id ',1,' openwater_readAscii',' openwate.3b file',IOUT1, &
                     ID, RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
       index = 0
       call fndNd2(index, id)
       if (index .gt. 0) then
        inod = index
        index = EINode(inod,2)
        if (EINode(inod,3) .eq. 4) then   ! en is open water knoop
         if (alreadyRead(index)) then
           call SetMessage(LEVEL_ERROR, 'Data for Openwater node '//id(1:Len_trim(id))//' double in datafile Openwate.3B')
         else
! cleaning RR files
          If (CleanRRFiles) write(Iounit,'(A)') String (1:len_trim(String))

          AlreadyRead(index) = .true.
          OwNam(index) = inod
          teller = teller + 1
! maximum allowable level
          RetVal = RetVal + GetVAR2 (STRING,' ml ',2,' openwater_readAscii',' openwate.3b file',IOUT1, &
                        CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
          MAXLVL(index) = RDUM(1)
! reference level
          RetVal = RetVal + GetVAR2 (STRING,' rl ',2,' openwater_readAscii',' openwate.3b file',IOUT1, &
                        CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
          REFLVL(index) = RDUM(1)
! area-level relation => wordt niet gebruikt door sobek_3b !!
! number of area/level combinations => staat altijd op 6!!
!         RetVal = RetVal + GetVAR (STRING,' ar ',3,' openwater_readAscii',' openwate.3b file',IOUT1, &
!                      CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND)
!         NVAL = IDUM(1)
! area per level
          RetVal = RetVal + GetVRS2 (STRING,' ar ',2,' openwater-ReadAscii',' openwate.3b file', &
                        IOUT1, CDUM(1), RDUM(1), IDUM(1), NVAL, IflRtn)
          Do teller1 = 1, NVAL
             AREAOW (teller1, index) = RDUM(teller1)
          Enddo
! level per area
          RetVal = RetVal + GetVRS2 (STRING,' lv ',2,' openwate-ReadAscii',' openwate.3b file', &
                       IOUT1, CDUM(1), RDUM(1), IDUM(1), NVAL, IflRtn)
          Do teller1 = 1, NVAL
             PEILOW (teller1, index) = RDUM(teller1)
          Enddo
! bottom level
          RetVal = RetVal + GetVAR2 (STRING,' bl ',2,' openwater_readAscii',' openwate.3b file',IOUT1, &
                        CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
          bottomlevel(index) = RDUM(1)
! target level
          RetVal = RetVal + GetVAR2 (STRING,' tl ',3,' openwater_readAscii',' openwate.3b file',IOUT1, &
                        CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
          IF (IDUM(1) .eq. 0) THEN ! target level constante waarde
             RetVal = RetVal + GetVAR2 (STRING,' tl 0 ',2,' openwater_readAscii',' openwate.3b file',IOUT1, &
                           CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
             TLVDEF(index) = ''
             c1dum = RDUM(1)
             c2dum = 0
             WinLvl(index) = c1dum
             SumLvl(index) = c1dum
             success = MakeConstTable(TableHandle, 1,TableNr,c1dum,c2dum, Idebug)
             if (.not. success) goto 21
             OpwRefOW_TTable(index) = TableNr
          ELSE
             RetVal = RetVal + GetVAR2 (STRING,' tl 1 ',1,' openwater_readAscii',' openwate.3b file',IOUT1, &
                           CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
             TLVDEF(index) = CDUM(1)
             TargetTableDefined = .true.
          ENDIF
! seepage definition
          H0DEF(index) = ''
          RetVal = RetVal + GetVAR2 (STRING,' sp ',1,' openwater_readAscii',' openwate.3b file',IOUT1, &
                        CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
          SEPDEF(index) = CDUM(1)
! Meteo station id
          RetVal = RetVal + GetVAR2 (STRING,' ms ',1,' openwater_readAscii',' openwate.3b file',IOUT1, &
                        CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
          NAMMET(inod) = CDUM(1)
! Initial salt concentration
          RetVal = RetVal + GetVAR2 (STRING,' is ',2,' openwater_readAscii',' openwate.3b file',IOUT1, &
                        CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
          SltIni(inod) = RDUM(1)
! areal adjustment factor rainfall on node, maybe missing,
          allow = .true.
          RetVal = RetVal + GetVAR2(STRING,' aaf ',2,' openwater-readAscii',' openwate.3b file',IOUT1, &
                           CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, Iflrtn)
          if (found) AAFNodeRainfall(inod) = max(0.0, RDUM(1))    ! AAF >= 0
          allow = .false.
         endif
        endif
       endif
     endif
     CALL SKPCOM (Infile1, ENDFIL,'ODS')
   Enddo
21 continue

    If (RetVal .gt. 0)  call ErrMsgStandard (972, 0, ' Error reading Openwate.3B file', ' Error getting OPWA records')
    If (teller .lt. NcOw)  Then
        Do inod=1,NcNode
          iow = EINode(inod,2)
          if (EINode(inod,3) .eq. 4) then   ! en is open water knoop
            if (.not. AlReadyRead(iow)) then
              NodeId = ' '
              NodeId = Id_Nod(inod)
              String = ' '
              String = ' ' // NodeId(1:Len_trim(NodeId)) // ' is missing'
              call ErrMsgStandard (977, 0, ' Data for open water node ',String(1:Len_trim(String)) )
            endif
          endif
        Enddo
        call ErrMsgStandard (972, 0, ' Not enough data for all open water nodes in schematisation found', &
                             ' Some open water nodes in schematisation not present in OpenWate.3B file')
    Endif

! cleaning RR files
   If (CleanRRFiles) Call closeGP (Iounit)

! *********************************************************************
! ***  If CleanRRFiles, also write cleaned input for openwate.sep
! *********************************************************************
   if (CleanRRFiles) then
        FileName = ConfFil_get_namFil(50)
        FileName(1:) = Filename(1:Len_trim(FileName)) // '_cleaned'
        Call Openfl (iounit, FileName,1,2)  !openwate.sep_cleaned
        Write(*,*) ' Cleaning Openwate.sep to file:', FileName
        Write(iout1,*) ' Cleaning Openwate.sep to file:', FileName
   endif

! *********************************************************************
! Read openwate.sep file
! *********************************************************************
   call SetMessage(LEVEL_DEBUG, 'Read Openwate.Sep file')
   Endfil = .false.
   Do iOw = 1, ncOw
      ReferenceToDefinition(iOw) = 0
   Enddo
   teller = 0
   RetVal = 0
   if (idebug .ne. 0) Write(idebug,*) ' Read openwate.sep file'
    CALL SKPCOM (Infile2, ENDFIL, 'ODS')
    Do while (.not. endfil)
      READ (Infile2,'(A1000)',END=211,ERR=150,IOSTAT=IECODE) STRING
      IF (STRING(1:4) .EQ. 'SEEP') Then
        teller = teller + 1
! Read seepage id
        RetVal = RetVal + GetVAR2 (STRING,' id ',1,' Openwate_readAscii',' Openwate.sep file',IOUT1, &
                      CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
        name = CDUM(1)
!       Eerst testen of seepage definition wel gebruikt wordt, dan pas verwerken
        Iow = FindString (Ncow, Sepdef, Name, NcOw, CaseSensitive)
        Occurs = (Iow .gt. 0)
        if (Iow .gt. 0) then
           if (ReferenceToDefinition(iow) .gt. 0) then
             call SetMessage(LEVEL_ERROR, 'Seepage Definition '//name(1:Len_trim(name))//' double in datafile Unpaved.Sep')
             occurs = .false. ! om verdere verwerking te stoppen
           endif
        endif

        allow = .true.
!       Computation option added March 2000; If not present, set to default value 1 = constant seepage
        RetVal = RetVal + GetVAR2 (STRING,' co ',3,' OpenWater_readAscii',' Openwate.sep file',IOUT1, &
                      CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
        if (.not. found) Idum(1) = 1
        dummyCompOption = Idum(1)
!       Verwerken Seepage definition
        if (occurs) then
          allow = .false.
!         Read seepage data from file, depending on computation option
          dummyKwel    = 0.0
          dummyCvalue  = 1000.0
          dummyH0Table = ' '
          owseepage = 0.0
          if (dummyCompOption .le. 0 .or. dummyCompOption .gt. 5) call ErrMsgStandard (972, 0, ' Unknown seepage computation option', ' Error in SEEP records')
          if (dummyCompOption .eq. 1) then
!           1 = constant seepage
!           Read constant seepage or percolation
            RetVal = RetVal + GetVAR2 (STRING,' sp ',2,' Openwate_readAscii',' Openwate.sep file',IOUT1, &
                          CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
            owseepage = RDUM(1)
           ! cleaning RR files
            If (CleanRRFiles) write(Iounit,'(A)') String (1:len_trim(String))
          elseif (dummyCompOption .ge. 2 .and. dummyCompOption .le. 3) then
! 2 = variable seepage with H0 from a table
! 3 = variable seepage with H0 on line from Modflow
           RetVal = RetVal + GetVAR2 (STRING,' cv ',2,' OpenWater_readAscii',' Openwate.sep file',IOUT1, &
                         CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
           DummyCvalue = RDUM(1)
           if (dummyCompOption .eq. 2) then
             RetVal = RetVal + GetVAR2 (STRING,' h0 ',1,' OpenWater_readAscii',' Openwate.sep file',IOUT1, &
                          CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
             DummyH0Table = CDUM(1)
             H0Defined = .true.
             ! cleaning RR files
             If (CleanRRFiles) write(Iounit,'(A)') String (1:len_trim(String))
           else
             OwOnLineModflowUsed = .true.
!            write(*,*) ' OwOnlineModFlowUsed= ',OwOnLineModflowUsed
             ! cleaning RR files
             If (CleanRRFiles) write(Iounit,'(A)') String (1:len_trim(String))
           endif
! 4 = seepage as time table
! 5 = seepage and seepage salt concentration time table
          elseif (dummyCompOption .ge. 4) then
             OpwRefSeepage_TTable(iow) = -1
             SeepageTableDefined = .true.
             Backspace(infile2)
             Success = GetRecord(Infile2, 'SEEP', Endfil, idebug, Iout1)     ! get record van keyword SEEP tot seep, zet in buffer
             IF (.not. success) goto 211
             IF (ENDFIL) GOTO 211
             Success = GetStringFromBuffer (KeepBufString)
             IF (.not. Success .and. CleanRRFiles)   then
                 Write(*,*) 'local buffer OpenwaterModule too small, OW_T record'
                 Write(iout1,*) 'local buffer OpenwaterModule too small, OW_T record'
                 GOTO 211
             Endif
             Success = GetTableName (TabYesNo, TableName, ' id ', Iout1)     ! get table name via keyword ' id ', TabYesNo=TBLE found
             IF (.not. success) goto 211
             If (TabYesNo .and. TableName .ne. '') Then
!               Er is een tabel gedefinieerd, met een niet-lege naam
                NrColumns = 1
!               Eerst testen of seepage definition wel gebruikt wordt, dan pas verwerken; Iow is al bepaald
!               Iow = FindString (Ncow, SepDef, TableName, NcOw, CaseSensitive)
                Occurs = (Iow .gt. 0)
                if (Iow .gt. 0) then
                   if ( OpwRefSeepage_TTable(iow) .eq. 0) then
                      occurs = .false.
                   elseif ( OpwRefSeepage_TTable(iow) .gt. 0) then
                      call SetMessage(LEVEL_ERROR, 'OWSeepage table definition '//Tablename(1:Len_trim(TableName))//' double in datafile Openwate.Tbl')
                      NrColumns = 0     ! om verdere verwerking uit te zetten
                   endif
                endif
!               Verwerken Target level definition
                if (occurs .and. NrColumns .gt. 0) then
! Get table with name TableName, Nrcolumns data fields, result in global arrays; tabel nummer is TableNr
                   Success = GetTable (TableHandle, TableName, NrColumns, TableNr, idebug, Iout1)
                   IF (.not. success) goto 211
                   if (idebug .ne. 0) write(idebugLunRR,*) ' Read owseep table ',TableName(1:32), ' Table index ', TableNr
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
 1031                continue
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
                   Do iOw = 1, ncOw
                      if (StringComp(SepDef(Iow), TableName, CaseSensitive) )  OpwRefSeepage_TTable(iOw) = TableNr
                   Enddo
                Endif
             Endif

          endif
        endif
! Read seepage salt concentration
! bug fix jan 2012; introduced with option of time depending seepage and possible also time depending salt conc
        if (dummyCompOption .le. 3) then
           RetVal = GetVAR2 (STRING,' ss ',2,' Openwate_readAscii',' Openwate.sep file',IOUT1, &
                             CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
           ScSeepage = RDUM(1)
        elseif (dummyCompOption .eq. 4) then
           RetVal = RetVal + GetVAR2 (STRING,' ss ',2,' Openwate_readAscii',' Openwate.sep file',IOUT1, &
                                      CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
           ScSeepage = RDUM(1)
        elseif (dummyCompOption .eq. 5) then
           RetVal = RetVal + GetVAR2 (STRING,' ss ',1,' Openwate_readAscii',' Openwate.sep file',IOUT1, &
                                      CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
           dummyCValue = 0.
           DummyConcTable = Cdum(1)
           OpwRefSeepageConc_TTable(iow) = -1
           SeepageConcTableDefined = .true.
        endif

! Assign definition to individual nodes
        Do iNod=1,ncNode
          if (EINode(Inod, 3) .eq. 4) then
            iOW = EINode(Inod, 2)
            if (StringComp(SepDef(Iow), Name, CaseSensitive) )  then
               ReferenceToDefinition(iOw) = teller
!bepaling kwel en wegzijging; beide zijn positieve getallen. correctie 22-12-1997
 ! correctie arrayindex March 1998: moet zijn Index ipv Teller !!
               if (owSeepage .ge. 0) then
                  owKwel(iOw) = owSeepage
                  owWegz(iOw) = 0.0
                  SltKwl(iNod) = ScSeepage
               else
                  owKwel(iOw) = 0.0
                  owWegz(iOw) = -owSeepage
               endif
               OWSeepageCompOption(iOw) = DummyCompOption
               OWCValue(iOw) = max (0.001, DummyCValue)
               H0Def (iOw) = DummyH0Table
               SaltConcDef (iOw) = DummyConcTable
            endif
          endif
        Enddo
      Endif
      CALL SKPCOM (Infile2, ENDFIL, 'ODS')
    Enddo
211 Continue

    If (RetVal .gt. 0)  call ErrMsgStandard (972, 0, ' Error reading Openwate.Sep file', ' Error getting SEEP records')
    Err969 = .false.
    Do iOw = 1, ncOw
       if (ReferenceToDefinition(iOw) .eq. 0)  Then
          Err969 = .true.
          call ErrMsgStandard (969, 0, ' Some Seepage definitions not present in openwate.sep file', SepDef(iow))
       Endif
    Enddo
    If (Err969) call ErrMsgStandard (972, 0, ' Not enough Openwater data found', &
                                     ' Some Seepage Definitions not present in Openwate.Sep file')

     Err969 = .false.
     Do iOw = 1, ncOw
       If (OpwRefSeepage_TTable(iOw) .lt. 0) Then
         Err969 = .true.
         call ErrMsgStandard (969, 0, ' Some Seepage table Definitions not present in Openwate.Sep file',SepDef(iow))
       Endif
     Enddo
     If (Err969) call ErrMsgStandard (972, 0, ' Not enough Openwater data found', &
                                      ' Some Seepage Table Definitions not present in Openwate.Sep file')


! cleaning RR files
   If (CleanRRFiles) Call closeGP (Iounit)

! *********************************************************************
! ***  If CleanRRFiles, also write cleaned input for openwate.tbl
! *********************************************************************
   if (CleanRRFiles) then
        FileName = ConfFil_get_namFil(51)
        FileName(1:) = Filename(1:Len_trim(FileName)) // '_cleaned'
        Call Openfl (iounit, FileName,1,2)  !openwate.tbl_cleaned
        Write(*,*) ' Cleaning Openwate.tbl to file:', FileName
        Write(iout1,*) ' Cleaning Openwate.tbl to file:', FileName
   endif

! *********************************************************************
! Read openwate.tbl:
! De tabellen met target levels van open water nodes (OW_T records)
! records kunnen over meerdere regels verspreid staan!!
! File alleen lezen als er verwezen is naar tabel definities
! *********************************************************************
     endfil = .not. TargetTableDefined
     if (.not. endfil) call SetMessage(LEVEL_DEBUG, 'Read Openwate.Tbl file')
     if (idebug .ne. 0) write(idebug,*) ' Read Openwate.Tbl file'
! OW_T records
     if (.not. endfil) Call SkpCom (Infile3, ENDFIL,'ODS')
     Do while (.not. endfil)
       Success = GetRecord(Infile3, 'OW_T', Endfil, idebug, Iout1)     ! get record van keyword OW_T tot ow_t, zet in buffer
       IF (.not. success) goto 3111
       IF (ENDFIL) GOTO 3111
       Success = GetStringFromBuffer (KeepBufString)
       IF (.not. Success .and. CleanRRFiles)   then
           Write(*,*) 'local buffer OpenwaterModule too small, OW_T record'
           Write(iout1,*) 'local buffer OpenwaterModule too small, OW_T record'
           GOTO 3111
       Endif
       Success = GetTableName (TabYesNo, TableName, ' id ', Iout1)     ! get table name via keyword ' id ', TabYesNo=TBLE found
       IF (.not. success) goto 3111
       If (TabYesNo .and. TableName .ne. '') Then
!         Er is een tabel gedefinieerd, met een niet-lege naam
!         Eerst testen of tabel definition wel gebruikt wordt, dan pas verwerken
          NrColumns = 1
!         Eerst testen of seepage definition wel gebruikt wordt, dan pas verwerken
          Iow = FindString (Ncow, Tlvdef, TableName, NcOw, CaseSensitive)
          Occurs = (Iow .gt. 0)
          if (Iow .gt. 0) then
             if ( OpwRefOW_TTable(iow) .gt. 0) then
                call SetMessage(LEVEL_ERROR, 'pen water target level Definition '//Tablename(1:Len_trim(TableName))//' double in datafile Openwate.Tbl')
                NrColumns = 0     ! om verdere verwerking uit te zetten
             endif
          endif
!         Verwerken Target level definition
          if (occurs .and. NrColumns .gt. 0) then
! Get table with name TableName, Nrcolumns data fields, result in global arrays; tabel nummer is TableNr
             Success = GetTable (TableHandle, TableName, NrColumns, TableNr, idebug, Iout1)
             IF (.not. success) goto 3111
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
             Do iOw = 1, ncOw
                if (StringComp(TlvDef(Iow), TableName, CaseSensitive) )  then
                   OpwRefOW_TTable(iOw) = TableNr
! ARS xxx dec 1999: zet WinLvl etc. op laatst ingelezen getal uit tabel
                   c1dum = GetLastValue(TableHandle) !TableData (FirstFreeData-1)
                   WinLvl(Iow) = c1dum
                   SumLvl(Iow) = c1dum
                Endif
             Enddo
          Endif
       Endif
       Call SKPCOM (Infile3, ENDFIL,'ODS')
     Enddo
3111 Continue

! H0Tables, if defined
     Rewind (infile3)
     endfil = .not. H0Defined
     if (.not. endfil) call SetMessage(LEVEL_DEBUG, 'Read Openwate.Tbl file; H0_T records')
     if (idebug .ne. 0) write(idebug,*) ' Read Openwate.Tbl file; H0_T records'
! H0_T records
     if (.not. endfil) Call SKPCOM (Infile3, ENDFIL,'ODS')
     Do while (.not. endfil)
       Success = GetRecord(Infile3, 'H0_T', Endfil, idebug, Iout1)     ! get record van keyword H0_T tot h0_t, zet in buffer
       IF (.not. success) goto 4111
       IF (ENDFIL) GOTO 4111
       Success = GetStringFromBuffer (KeepBufString)
       IF (.not. Success .and. CleanRRFiles)   then
           Write(*,*) 'local buffer OpenwaterModule too small, H0_T record'
           Write(iout1,*) 'local buffer OpenwaterModule too small, H0_T record'
           GOTO 4111
       Endif
       Success = GetTableName (TabYesNo, TableName, ' id ', Iout1)     ! get table name via keyword ' id ', TabYesNo=TBLE found
       IF (.not. success) goto 4111
       If (TabYesNo .and. TableName .ne. '') Then
!         Er is een tabel gedefinieerd, met een niet-lege naam
          NrColumns = 1
!         Eerst testen of seepage definition wel gebruikt wordt, dan pas verwerken
          Iow = FindString (Ncow, H0def, TableName, NcOw, CaseSensitive)
          Occurs = (Iow .gt. 0)
          if (Iow .gt. 0) then
             if ( OpwRefH0_TTable(iow) .gt. 0) then
                call SetMessage(LEVEL_ERROR, 'H0 table definition '//Tablename(1:Len_trim(TableName))//' double in datafile Openwate.Tbl')
                NrColumns = 0  ! om verdere verwerking te stoppen
             endif
          endif
!         Verwerken Target level definition
          if (occurs .and. NrColumns .gt. 0) then
! Get table with name TableName, Nrcolumns data fields, result in global arrays; tabel nummer is TableNr
             Success = GetTable (TableHandle, TableName, NrColumns, TableNr, idebug, Iout1)
             IF (.not. success) goto 4111
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
             Do iOw = 1, ncOw
                if (StringComp(H0Def(Iow), TableName, CaseSensitive) )  OpwRefH0_TTable(iOw) = TableNr
             Enddo
          Endif
       Endif
       Call SKPCOM (Infile3, ENDFIL,'ODS')
     Enddo
4111 Continue

! SaltConcTables, if defined
     Rewind (infile3)
     endfil = .not. SeepageConcTableDefined
     if (.not. endfil) call SetMessage(LEVEL_DEBUG, 'Read Openwate.Tbl file; SPCO records')
     if (idebug .ne. 0) write(idebug,*) ' Read Openwate.Tbl file; SPCO records'
! SPCO records
     if (.not. endfil) Call SKPCOM (Infile3, ENDFIL,'ODS')
     Do while (.not. endfil)
       Success = GetRecord(Infile3, 'SPCO', Endfil, idebug, Iout1)     ! get record van keyword SPCO tot spco, zet in buffer
       IF (.not. success) goto 5111
       IF (ENDFIL) GOTO 5111
       Success = GetStringFromBuffer (KeepBufString)
       IF (.not. Success .and. CleanRRFiles)   then
           Write(*,*) 'local buffer OpenwaterModule too small, SPCO record'
           Write(iout1,*) 'local buffer OpenwaterModule too small, SPCO record'
           GOTO 5111
       Endif
       Success = GetTableName (TabYesNo, TableName, ' id ', Iout1)     ! get table name via keyword ' id ', TabYesNo=TBLE found
       IF (.not. success) goto 5111
       If (TabYesNo .and. TableName .ne. '') Then
!         Er is een tabel gedefinieerd, met een niet-lege naam
          NrColumns = 1
!         Eerst testen of seepage definition wel gebruikt wordt, dan pas verwerken
          Iow = FindString (Ncow, SaltConcdef, TableName, NcOw, CaseSensitive)
          Occurs = (Iow .gt. 0)
          if (Iow .gt. 0) then
             if ( OpwRefSeepageConc_TTable(iow) .gt. 0) then
                call SetMessage(LEVEL_ERROR, 'Seeepage concentration table definition '//Tablename(1:Len_trim(TableName))//' double in datafile Openwate.Tbl')
                NrColumns = 0  ! om verdere verwerking te stoppen
             endif
          endif
!         Verwerken Target level definition
          if (occurs .and. NrColumns .gt. 0) then
! Get table with name TableName, Nrcolumns data fields, result in global arrays; tabel nummer is TableNr
             Success = GetTable (TableHandle, TableName, NrColumns, TableNr, idebug, Iout1)
             IF (.not. success) goto 4111
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
 1061          continue
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
             Do iOw = 1, ncOw
                if (StringComp(SaltConcDef(Iow), TableName, CaseSensitive) )  OpwRefSeepageConc_TTable(iOw) = TableNr
             Enddo
          Endif
       Endif
       Call SKPCOM (Infile3, ENDFIL,'ODS')
     Enddo
5111 Continue

! cleaning RR files
     If (CleanRRFiles) Call closeGP (Iounit)

! *********************************************************************
! Check of alle referenties naar tabellen opgelost
! *********************************************************************
    Err969 = .false.
    Do iOw = 1, ncOw
       If (OpwRefOW_TTable(iOw) .eq. 0 .and. TLVDEF (iOw) .ne. '') Then
         Err969 = .true.
         call ErrMsgStandard (969, 0, ' Some OW_T Target level Definitions not present in Openwate.Tbl file',TlvDef(iow))
       Endif
       If (OpwRefH0_TTable(iOw) .eq. 0 .and. H0Def (iOw) .ne. '') Then
         Err969 = .true.
         call ErrMsgStandard (969, 0, ' Some H0_T Table Definitions not present in Openwate.Tbl file',H0Def(iow))
       Endif
       If (OpwRefSeepageConc_TTable(iOw) .lt. 0) Then
         Err969 = .true.
         call ErrMsgStandard (969, 0, ' Some Seepage concentration table Definitions not present in Openwate.tbl file',SaltConcDef(iow))
       Endif
    Enddo
    If (Err969) call ErrMsgStandard (972, 0, ' Not enough Openwater data found', &
                                     ' Some Table Definitions not present in Openwate.Tbl file')

! *********************************************************************
! write(*,*) ' Initialisations open water nodes'
! berekening volume open water
! *********************************************************************
    Call DetermineVolumeOpenWater

    DEALLOCATE (SEPDEF, TLVDEF, H0DEF)
    Deallocate (AlreadyRead)
    Deallocate (ReferenceToDefinition)
    return

150 continue
    call SetMessage(LEVEL_FATAL, 'Read error in Openwater ascii')

    return
    end subroutine Openwater_readAsciiInput


  subroutine OpenwaterPrecip_readAsciiInput(infile1)
    ! *********************************************************************
    ! *** Last update: 21 March  1997                    By : Peter Schrier
    ! *********************************************************************
    ! *** Brief description:
    ! *** ------------------
    ! ***   Ascii files lezen met gegevens openwater
    ! *********************************************************************
    ! *** Input/output parameters:
    ! *** input: infile1,infile2,infile3 : 3 ascii files
    ! *********************************************************************

    Integer :: RetVal

    Character(CharIdLength) id
    Integer(4)    Infile1
    Integer       teller, iecode, iout1, index, inod, idebug
    Integer       IOw
    Logical       allow, found, endfil
    Integer       IDUM(32)
    REAL          RDUM(32)
    Character(CharIdLength) CDUM(32), NodeId
    Character(1000) string
    Logical, Pointer :: AlreadyRead(:)

    Logical Success

    Character(Len=CharIdLength)  FileName
    Integer                      IoUnit


    success = Dh_AllocInit (NcOwRain, AlreadyRead, .false. )
    If (.not. success) call ErrMsgStandard (981, 0, ' Error allocating arrays in subroutine ', ' OpenWaterPrecip_ReadAscii')

    allow = .false.
    found = .false.
    iOut1  = ConfFil_get_iOut1()
    iDebug = ConfFil_get_iDebug()


! *********************************************************************
! resetten parameters
! *********************************************************************

! *********************************************************************
! ***  If CleanRRFiles, also write cleaned input - open water precipitation, only 3b. file (overlap with open water nodes)
! *********************************************************************
   if (CleanRRFiles) then
        FileName = ConfFil_get_namFil(49)
        FileName(1:) = Filename(1:Len_trim(FileName)) // '_cleaned'
        Call Openfl (iounit, FileName,1,3)  !openwate.3b_cleaned      ! in append mode, since already used by openwater nodes
        Write(*,*) ' Cleaning openwate.3b to file:', FileName
        Write(iout1,*) ' Cleaning openwate.3b to file:', FileName
   endif

! *********************************************************************
! Read openwate.3b file
! *********************************************************************
   call SetMessage(LEVEL_DEBUG, 'Read Openwate.3b file')
   if (idebug .ne. 0) write(idebug,*) ' Read openwate.3b file'
   Endfil = .false.
   teller = 0
   RetVal = 0
   CALL SKPCOM (Infile1, ENDFIL,'ODS')
   do while (.not. endfil)
     READ(infile1,'(A1000)',END=21,ERR=150,IOSTAT=IECODE) STRING
! Skip regel als hij niet begin met juist keyword
     IF (STRING(1:4) .EQ. 'OWRR') Then
! open water id
       RetVal = RetVal + GetVAR2 (STRING,' id ',1,' OpenwaterPrecip_readAscii',' openwate.3b file',IOUT1, &
                     ID, RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
       index = 0
       call fndNd2(index, id)
       if (index .gt. 0) then
        inod = index
        index = EINode(inod,2)
        if (EINode(inod,3) .eq. 21) then   ! en is open water knoop - only precip/evap
         if (alreadyRead(index)) then
           call SetMessage(LEVEL_ERROR, 'Data for Openwater node '//id(1:Len_trim(id))//' double in datafile Openwate.3B')
         else
! cleaning RR files
          If (CleanRRFiles) write(Iounit,'(A)') String (1:len_trim(String))

          AlreadyRead(index) = .true.
          OwPrecipNam(index) = inod
          teller = teller + 1
! area
          RetVal = RetVal + GetVAR2 (STRING,' ar ',2,' openwater-ReadAscii',' openwate.3b file', &
                        IOUT1, CDUM(1), RDUM(1), IDUM(1), ALlow, Found, IflRtn)
          OWRainAREA(index) = RDUM(1)
! Meteo station id
          RetVal = RetVal + GetVAR2 (STRING,' ms ',1,' OpenwaterPrecip_readAscii',' openwate.3b file',IOUT1, &
                        CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
          NAMMET(inod) = CDUM(1)
! areal adjustment factor rainfall on node, maybe missing, default 1
          allow = .true.
          RetVal = RetVal + GetVAR2(STRING,' aaf ',2,' openwater-readAscii',' openwate.3b file',IOUT1, &
                           CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, Iflrtn)
          if (found) AAFNodeRainfall(inod) = max(0.0, RDUM(1))    ! AAF >= 0
          allow = .false.
         endif
        endif
       endif
     endif
     CALL SKPCOM (Infile1, ENDFIL,'ODS')
   Enddo
21 continue

! cleaning RR files
   If (CleanRRFiles) Call closeGP (Iounit)

    If (RetVal .gt. 0)  call ErrMsgStandard (972, 0, ' Error reading Openwate.3B file', ' Error getting OWRR records')
    If (teller .lt. NcOwRain)  Then
        Do inod=1,NcNode
          iow = EINode(inod,2)
          if (EINode(inod,3) .eq. 21) then   ! en is open water precipitation knoop
            if (.not. AlReadyRead(iow)) then
              NodeId = ' '
              NodeId = Id_Nod(inod)
              String = ' '
              String = NodeId(1:Len_trim(NodeId)) // ' is missing'
              call ErrMsgStandard (977, 0, ' Data for open water node ',String(1:Len_trim(String)) )
            endif
          endif
        Enddo
        call ErrMsgStandard (972, 0, ' Not enough data for all open water -precipitation nodes in schematisation found', &
                             ' Some open water-precipitation nodes in schematisation not present in OpenWate.3B file')
    Endif
    return

150 continue
    call SetMessage(LEVEL_FATAL, 'Read error in Openwaterprecip ascii')

    return
    end subroutine OpenwaterPrecip_readAsciiInput



    Subroutine DetermineVolumeOpenWater

! *********************************************************************
! ** write(*,*) ' Initialisations open water nodes'
! ** berekening volume open water
! *********************************************************************

    Real          RDept
    Real          Localarea(6), Locallevel(6), tlvl
    Integer       teller, teller1, idebug
    Integer       teller0, IOw
    Character(CharIdLength) string


    iDebug = ConfFil_get_iDebug()

    if (ncOw .gt. 0) then
      do teller0 = 1, ncNode
        if (EINode(teller0, 3) == 4) then
          iOW = EINode(teller0, 2)
! 4 regels toegevoegd; Geert Prinsen; array in functie constant is nl. 1 dimensionaal
          do teller1=1,nval
             localarea(teller1) =areaow(teller1,iOW)
             locallevel(teller1)=peilow(teller1,iOW)
          enddo
          if (constant(localarea) .and. constant(locallevel)) then
            call ErrMsgStandard(973, 0, 'Area AND levels for an open water node are constant', Id_Nod(teller0))
          endif
! ARS 14974
          if (constant(localarea)) then
              call DetermineTargetlevel (Tlvl, iow)
              if (tlvl .lt. peilow(1,iow)) then
                 do teller1 = 1, NVAL
                    peilow(teller1,iow) = bottomlevel(iow) + 0.1 * teller1
                 enddo
              endif
          endif

          DO teller = 1, NVAL
            IF (teller .gt. 1) THEN
              teller1 = teller - 1
              VOLUOW(teller, iOW) = VOLUOW(teller1, iOW) &
                                    + 0.5 * (AREAOW(teller,iOW) + AREAOW(teller1,iOW)) &
                                    * (PEILOW(teller,iOW) - PEILOW(teller1, iOW))
            ELSE
      ! veronderstel onder laagste areaal nog rechte bak.
      ! RDEPT = bodemnivo
              RDEPT = BottomLevel(iOW)
              IF (RDEPT .GT. PEILOW(1,IOW))  THEN
! april 1998; Initialise volume at lowest level to zero, if bottom level higher
                VOLUOW (1,IOW) = 0.
                STRING(1:CharIdLength) = Id_Nod(teller0)
                call ErrMsgStandard(927, 0, ' Read open water input data ', STRING(1:CharIdLength))
              ELSE
                RDEPT = PEILOW(1,IOW) - RDEPT
                VOLUOW (1,IOW) = AREAOW(1,IOW) * RDEPT
              ENDIF
            ENDIF
          ENDDO
          LVLOW(IOW) = MIN (LVLOW(IOW), OWMNMA(IOW))
          IF (LVLOW(IOW) .GE. OWMNMA(IOW) .AND. iDebug /= 0)  WRITE(IDEBUG,*) ' Beperking open waterpeil'
!         write(*,*) ' open water peil', LvlOw(iow), OwMnMa(iow)
        end if
        ! berekening maximum toelaatbare open water nivo wat getoond wordt
      end do
    end if

    if (idebug .ne. 0) Then
       Do iow=1,ncow
          write(idebug,*) ' open water iow=', iow
          write(idebug,*) ' level   area   volume relation'
          do teller=1,6
             write(idebug,*) peilow(teller,iow), areaow(teller,iow), voluow(teller,iow)
          enddo
       Enddo
    Endif


    Return
    End subroutine DetermineVolumeOpenWater



!  SUBROUTINE DTTLVL (TLVL, IOW)
! *********************************************************************
! *** Last update:  7 March 1997                      by: Peter Schrier
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

!    INTEGER IOW
!    REAL TLVL

!    Tlvl = CurrentTargetLevel (iow)

!    RETURN
!  END subroutine dttLvl
!


  SUBROUTINE DetermineTargetLevel (TLVL, IOW)
    ! *********************************************************************
    ! *** Last update:  7 March 1997                      by: Peter Schrier
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

    INTEGER IOW, rowNr, TabelNr, Idebug, Iout1
    REAL TLVL
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
    ! nieuwe methode NewTables
    RowNr = -1
    TabelNr = OpwRefOW_TTABLE (iow)
    Tlvl = GetNewValue(TableHandle, TabelNr, 1, RowNr, CurrentDate, CurrentTime, &
                       Idebug, iout1, DateTimeOutsideTable, .true.)
    if (idebug .ne. 0) WRITE (IDEBUG,*) ' nieuwe methode iow, rowNr tlvl', iow, RowNr, tlvl

    RETURN
  END subroutine DetermineTargetLevel


  Subroutine ReadOpenDAOpenWater (Infile1, iout1)

  ! read Paved restart info from ASCII OpenDA file
  integer      Infile1, iout1, idebug

  Integer      RetVal

  Integer       inod
  Integer       i, iow, iecode
  Character(Len=1000) String
  Integer        Nhlp
  Parameter     (Nhlp = 32)
  Integer       IDUM(NHLP)
  Real          RDUM(NHLP), rhelp, rdum1
  Character(Len=CharIdLength) CDUM(NHLP)
  Logical       allow, found, endfil

  Real    PeilArray(6), AreaArray(6), VolumeArray(6)
  Real    ExtraPeilArray(NStorageCurvePoints), ExtraVolumeArray(NStorageCurvePoints)

  ! file is already opened, rewind it
  Rewind(Infile1)
  iDebug = ConfFil_get_iDebug()
  RetVal = 0

  Call SKPCOM (Infile1, ENDFIL,'ODS')
  call SetMessage(LEVEL_INFO, 'Read OpenDA open water data')
  do while (.not. endfil)
    ! read OpenDA record
     Read(Infile1,'(A1000)',END=21,ERR=150,IOSTAT=IECODE)  STRING
     ! skip regel als hij niet begint met juist keyword (OPWA)
      If (STRING(1:4) .EQ. 'OPWA') then
      ! OPWA node id
        allow = .false.
        Retval = Retval + GetVAR2 (STRING,' id ',1,' OPWA-ReadAscii',' OpenDA file',IOUT1, &
                                   CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
        call fndNd2(inod, CDUM(1))
        if (inod .gt. 0) then      ! knoop bestaat in schematisatie
            iow = EiNode(inod,2)
            if (EiNode(inod,3) .eq. 4) then  ! en is open water
                ! get the data
                ! update the corresponding RR variables and related variables
                allow = .true.
                Retval = Retval + GetVAR2 (STRING,' openwaterlevel ',2, ' OPWA-ReadAscii',' OPENDA file',IOUT1, &
                                              CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
                if (found) then
                   write(*,*) ' found open water id and water level ', Id_nod(inod)(1:len_trim(id_nod(inod))), rdum(1)
                   LVLOW(iow) = Rdum(1)
                   ! determine corresponding volume (double precision), area, volume inundated at paved/unpaved
                   Do i=1,NVal
                      PeilArray(i) = PeilOW(i,iow)
                      AreaArray(i) = AreaOW(i,iow)
                      VolumeArray(i) = VOLUOW(i,iow)
                   Enddo
                   CALL RR_INTERP (NVAL, PeilArray, AreaArray, LVLOW (IOW),  AROW (IOW), IDUM(1))
                   CALL RR_INTERP (NVAL, PeilArray, VolumeArray, LVLOW (IOW),  RDum1, IDUM(1))
                   ArOw(iow) = max (0.0001, ArOw(iow))
                   VolOw(iow) = Rdum1
                   Do i=1,NrBergendOppervlakIndices(iow)
                      ExtraPeilArray(i)   = ExtraBergendOppPeil(i,iow)
                      ExtraVolumeArray(i) = ExtraBergendOppVolume(i,iow)
                   Enddo
                   CALL RR_INTERP (NrBergendOppervlakIndices(iow), ExtraPeilArray, &
                                   ExtraVolumeArray, LvlOw(IOW), Rhelp,idum(1))
                   ActualExtraBergendVolume(iow) = max (0.0, RHelp)
                   TotalVolume(iow) = VolOw(iow) + ActualExtraBergendVolume(iow)

                   RSLMAP4_OW(1,iow,1) = LVLOW(iow)
                   OW_Tnul(1,iow) = RSLMAP4_ow(1,iow,1)
                   RSLMAP4_OW(2,iow,1) = VolOW(iow) + ActualExtraBergendVolume(iow)
                   OW_Tnul(2,iow) = RSLMAP4_ow(2,iow,1)
                   RSLMAP4_ow(6,Iow,1) =  0
                   IF (LVLOW(Iow) .GT. MAXLVL(Iow)) then
                       RSLMAP4_ow(6,Iow,1)= LVLOW(Iow)-MAXLVL(Iow)
                   endif
                endif
            endif
        endif
     Endif
  enddo
  goto 21

150 continue
    call SetMessage(LEVEL_ERROR, 'Error reading OpenDA restart record '//String(1:len_trim(string)))
21  continue

  End subroutine ReadOpenDAOpenWater


  Subroutine WriteOpenDAOpenWater (Infile1)

  ! write Open water restart info to ASCII OpenDA file
  integer      Infile1, idebug

  Integer       inode, iow
  Character(len=1) Quote

  ! file is already opened
  iDebug = ConfFil_get_iDebug()
  quote = ''''

  do inode=1,ncnode
     iow = EiNode(inode,2)
     if (EiNode(inode,3) .eq. 4) then  ! en is open water
         write(Infile1,'(A,A1,A,A1,1(1X,A,G15.8),A)') 'OPWA id ',&
                                       quote,id_nod(inode)(1:len_trim(id_nod(inode))),quote,&
                                       ' openwaterlevel ', LVLOW(iow), ' opwa'
     Endif
  enddo

  End subroutine WriteOpenDAOpenWater




  SUBROUTINE CMPOWRain (ITMSTP, IOW  , IMETEO, INODE, iDebug)

    use Boundary

    Integer iTmStp, iOW, iMeteo, iNode, iDebug, iout1, ihour, ibnd

    iDebug = ConfFil_get_iDebug()
    iOut1  = ConfFil_get_iOut1 ()
    iHour = ConfArr_get_iHour()
    ibnd  = OwRainBnd(iow)
    if (ibnd .le. 0) then
        call SetMessage(LEVEL_FATAL, 'OWRain node: no valid downstream RR-boundary')
    endif

    IF (iDebug /= 0)  WRITE(IDEBUG,*) 'CMPOWRain iow=',IOW

    ROWRain(IOW) = AAFNodeRainfall(inode) * Rain(IMETEO) * OWRainArea(iow)
    VOWRain(IOW) = 0.0
    IF (IHOUR .GE. timeSettings%evaporationFromHr .AND. IHOUR .LT. timeSettings%evaporationToHr) THEN
       VOWRain(IOW) = Evap(IMETEO) * OwRainAREA(iow) * CROPO * TMEVAP
    ENDIF
    Qbnd(ibnd) = Qbnd(ibnd) + RowRain(iow) - VowRain(iow)

    if (idebug .ne. 0) then
       write(idebug,*) ' ow-precipitation (m3/s)= ', RowRain(iow)
       write(idebug,*) ' ow-evaporation   (m3/s)= ', VowRain(iow)
    Endif

    RETURN
  END subroutine CmpOwRain


  SUBROUTINE CMPOW (ITMSTP, IOW  , IMETEO, INODE, iDebug)
    ! *********************************************************************
    ! *** Last update:  March 1995
    ! *********************************************************************
    ! *********************************************************************
    ! *** Brief description:
    ! *** ------------------
    ! ***    This subroutine performs computations for open water.
    ! *********************************************************************
    ! *** Input/output parameters:
    ! *** ------------------------
    ! ***  IEVENT = event number
    ! ***  ITMSTP = timestep number
    ! ***  IDEBUG = file unit number of debug file
    ! ***  IOUT1  = file unit number of output file with run messages
    ! ***  IOW    = intern open water nr
    ! ***  IMETEO = meteostation code
    ! ***  INODE  = knoopnr
    ! *********************************************************************
    ! *** Berekeningen voor open water
    ! *********************************************************************

    REAL    RKWEL, Vkwel, tQIn, qOutp, rArea, rhelp
    Real    AvLvl, LvlTemp, RDum
    Integer iTmStp, iOW, iMeteo, iNode, iDum1, iDebug, iHour, idim, i
    Character(len=40) TempString

    Real PeilArray(6), AreaArray(6), VolumeArray(6)  ! aanname dat NVAL=6 zoals in Conf_Arr.
    Real TotalBergPeilArray(NStorageCurvePoints), TotalBergVolumeArray(NStorageCurvePoints)
    Real ExtraBergPeilArray(NStorageCurvePoints), ExtraBergVolumeArray(NStorageCurvePoints)

! voor debug uitvoer tabellen
! integer i,j, itab, irow, icol

    iDebug = ConfFil_get_iDebug()

! Test Parbo
!    id = Id_Nod(INODE)
!    if (id(1:4) .eq. '1730' .and. itmstp .ge. 4) Idebug = idebugLunRR


    IF (iDebug /= 0)  WRITE(IDEBUG,*) 'CMPOW iow=',IOW

    ! *********************************************************************
    ! *** Voorlopige Open water balans
    ! ***   - bepaal kwel/wegzijging
    ! ***   - bepaal totale instroom uit verhard/kasgebied
    ! ***   - schat instroom onverhard gebied/structures met 'oude' waarde
    ! ***   - schat uitstroom via structures gelijk aan 'oude' waarde
    ! ***   - bepaal volume neerslag en verdamping
    ! *********************************************************************

    Do i=1,NVal
       PeilArray(i) = PeilOw(i,iow)
       AreaArray(i) = AreaOw(i,iow)
       VolumeArray(i) = VoluOw(i,iow)
    Enddo

    If (OWSeepageCompOption(iow) .eq. 1 .or. OWSeepageCompOption(iow) .ge. 4) then
!      old: constant seepage
       RKWEL= OWKWEL(IOW) - OWWEGZ(IOW)
    Else
! March 2000: addition variable seepage, SeepageCompOption 2,3: use H0 from table or from Modflow
! H0, Lvl in m, Cvalue in dagen, RKwel in m/s
       AvLvl = (LvlOw0(iow) + LvlOw(iow) ) / 2.0
       RKwel = ( OWH0Actual(iow) - AvLvl ) / OwCValue(iow) / NrsDay
       if (idebug .ne. 0) THEN
         WRITE(IDEBUG,*) ' Compute RKwel using'
         WRITE(IDEBUG,*) ' H0Actual  Owlvl  Cvalue',OwH0Actual(iow), AvLvl, OwCvalue(iow)
         WRITE(IDEBUG,*) ' Rkwel  ', Rkwel
       endif
! beperk RKwel (volumecheck):
! als H0 < LvlOw0 mag de wegzijging niet zo groot zijn dat owlvl onder H0 zakt
! als H0 > LvlOw0 mag de kwel       niet zo groot zijn dat owlvl boven H0 stijgt
       Lvltemp  = LvlOw0(Iow) + RKwel * timeSettings%timestepSize
       if (Rkwel .gt. 0 .and. LvlTemp .gt. OwH0Actual(iow) .and. LvlOw0(iow) .lt. OwH0Actual(iow))  then
          Rkwel = min (Rkwel, (OwH0Actual(iow) - LvlOw0(iow)) / timeSettings%timestepSize)
       elseif (rkwel .lt. 0 .and. Lvltemp .lt. OwH0Actual(iow) .and. LvlOw0(iow) .gt. OwH0Actual(iow))  then
          Rkwel = max (Rkwel, (OwH0Actual(iow) - LvlOw0(iow)) / timeSettings%timestepSize)
       endif
       if (idebug .ne. 0)  WRITE(IDEBUG,*) ' Computed RKwel after VolumeCheck ', RKwel
    Endif

!   volumecheck: Rkwel mag niet groter zijn dan beschikbaar volume!!
    If (ReduceInfiltrationAtNegativeOpenWaterVolume) then
       if (Rkwel .lt. 0) then
           Vkwel = Rkwel * TimeSettings%TimestepSize
           If (VolOw0(iow) + Vkwel .lt. 0) then
              Vkwel = max (-1. * max (0.0, real(VolOw0(iow))), Vkwel)
              Rkwel = Vkwel / TimeSettings%TimestepSize
           Endif
       Endif
       if (idebug .ne. 0)  WRITE(IDEBUG,*) ' Computed RKwel after ReduceInfiltration ', RKwel
    Endif

    TQIN = QINOW(IOW,1) + QINOW(IOW,3) + Qinow(Iow,7)
    TQIN = TQIN + QIN0(IOW,2) + QIN0(IOW,4) + QIN0(IOW,5) + QIN0(IOW,6)

    QOUTP = QOUT0(IOW)

    DO IDUM1=1,3
      iHour = ConfArr_get_iHour()
      RAREA = (1.0 - timesettings%timeWeightFactor) * AROW0(IOW) + &
              timesettings%timeWeightFactor * AROW(IOW)
! ARS xxxx: voorkom problemen met negatieve oppervlakken door laag peil
      RArea = max (0.0001, RArea)
      If (OpenWaterPrecipitationComp .eq. 0) RainArea(iow) = Rarea
      If (OpenWaterSeepageComp .eq. 0) SeepageArea(iow) = Rarea
      ROW(IOW) = AAFNodeRainfall(inode) * Rain(IMETEO) * RainArea(iow) * timeSettings%timestepSize
      IF (IHOUR .GE. timeSettings%evaporationFromHr .AND. IHOUR .LT. timeSettings%evaporationToHr) THEN
         VOW(IOW) = Evap(IMETEO) * RAREA * timeSettings%timestepSize * CROPO * TMEVAP
!        bij droogval: neem aan verdamping gaat door op dezelfde manier ("gewasverdamping" uit bodemvocht)
      ELSE
         VOW(IOW) = 0.0
      ENDIF
      KWOW(IOW)= RKWEL * SeepageArea(iow) * timeSettings%timestepSize
      VOLOW(IOW) = VOLOW0(IOW) - VOW(IOW) + ROW(IOW) + KWOW(IOW) + &
            TQIN * timeSettings%timestepSize - QOUTP * timeSettings%timestepSize
    ! ***  Bepaal nieuw peil en area
      RDum = VolOw(iow)
      CALL RR_INTERP (NVAL, VolumeArray, PeilArray, RDum, LVLOW(IOW), OwLastInterpIndex(iow))
      CALL RR_INTERP (NVAL, VolumeArray, AreaArray, RDum, AROW(IOW), OwLastInterpIndex(iow))
! ARS xxxx: voorkom problemen met negatieve oppervlakken door laag peil
      ArOw(iow) = max (0.0001, ArOw(iow))

    ! correctie: peil niet meer dan 50 cm boven laagste maaiveld
      LVLOW(IOW) = MIN (LVLOW(IOW), OWMNMA(IOW))
      IF (LVLOW(IOW) .GE. OWMNMA(IOW) .AND. iDebug .ne. 0) WRITE(IDEBUG,*) ' Restricting open waterlevel'
    ! eind correctie 5 juli 95

    ! pm
    ! check of open waterpeil niet onder bodemhoogte zakt.
    ! als open waterpeil onder bodemhoogte zakt dan warning
    ! en peil wordt bodemhoogte

    ENDDO

! Bewaar berekende kwel in OWKWEL en OWWEGZ arrays, indien variabel
    If (OwSeepageCompOption(iow) .ge. 2) then
       if (Rkwel .gt. 0) then
          OwKwel(iow)  = Rkwel
          OwWegZ(iow) = 0.0
       else
          OwKwel(iow)  = 0.0
          OwWegZ(iow) = -1 * RKwel
       endif
    ENDIF

    ! *********************************************************************
    ! *** debug
    ! *********************************************************************
    IF (iDebug .ne. 0) THEN
       WRITE(IDEBUG,*) ' Simple Open water ', Id_Nod(INODE)
       WRITE(IDEBUG,*) ' Timestep nr                :',ITMSTP
       WRITE(IDEBUG,*) ' Timestepsize in s          :',timeSettings%timestepSize
       WRITE(IDEBUG,*) ' Time coefficient t,t+1     :',timesettings%timeWeightFactor
       WRITE(IDEBUG,*) ' Bottom Level               :',BottomLevel(IOW)
       WRITE(IDEBUG,*) ' Old volume in M3           :',VOLOW0(IOW)
       WRITE(IDEBUG,*) ' New volume in M3           :',VOLOW (IOW)
       WRITE(IDEBUG,*) ' Old area   in M2           :',AROW0 (IOW)
       WRITE(IDEBUG,*) ' New area   in M2           :',AROW  (IOW)
       WRITE(IDEBUG,*) ' Old level  in M            :',LVLOW0(IOW)
       WRITE(IDEBUG,*) ' New level  in M            :',LVLOW (IOW)
       WRITE(IDEBUG,*) ' OWMNMA max. ow level       :',OWMNMA(IOW)
       WRITE(IDEBUG,*) ' Rainfall Area              :',RainArea(IOW)
       WRITE(IDEBUG,*) ' rainfall open water M3     :',ROW   (IOW)
       WRITE(IDEBUG,*) ' evaporation open water M3   :',VOW   (IOW)
       If (OwSeepageCompOption(iow) .ge. 2) then
          WRITE(IDEBUG,*) ' OwH0Actual for SeepageComp   :',OwH0Actual(iow)
       Endif
       WRITE(IDEBUG,*) ' Seepage Area               :',SeepageArea(IOW)
       WRITE(IDEBUG,*) ' seepage in M3              :',KWOW  (IOW)
       WRITE(IDEBUG,*) ' inflow paved area m3/s     :',QINOW (IOW,1), &
                                                 QINOW (IOW,1)*timeSettings%timestepSize
       WRITE(IDEBUG,*) ' inflow unpaved area m3/s   :',QIN0  (IOW,2), &
                                                 QIN0  (IOW,2)*timeSettings%timestepSize
       WRITE(IDEBUG,*) ' inflow greenhouse area m3/s:',QINOW (IOW,3), &
                                                 QINOW (IOW,3)*timeSettings%timestepSize
       WRITE(IDEBUG,*) ' inflow structures prev.iter:',QIN0  (IOW,4), &
                                                 QIN0  (IOW,4)*timeSettings%timestepSize
       WRITE(IDEBUG,*) ' outflow via structures     :',QOUTP
       WRITE(IDEBUG,*) ' inflow from RWZI prev.iter :',QIN0 (IOW,5), &
                                                 QIN0 (IOW,5)*timeSettings%timestepSize
       WRITE(IDEBUG,*) ' inflow from RWZI m3/s      :',QINOW (IOW,5), &
                                                 QINOW (IOW,5)*timeSettings%timestepSize
       WRITE(IDEBUG,*) ' inflow Industry prev.iter  :',QIN0 (IOW,6), &
                                                 QIN0 (IOW,6)*timeSettings%timestepSize
       WRITE(IDEBUG,*) ' inflow from Industry m3/s  :',QINOW (IOW,6), &
                                                 QINOW (IOW,6)*timeSettings%timestepSize
       WRITE(IDEBUG,*) ' inflow from Sacr/RRRunoff m3/s  :',QINOW (IOW,7), &
                                                 QINOW (IOW,7)*timeSettings%timestepSize
    Endif

    ! *********************************************************************
    ! Check of bergend oppervlak op verhard/onverhard/kassen meedoet
    ! *********************************************************************
    if (OpenWaterLevelComputations .eq. 1) then
        if (itmstp .ge. 24141) then
            continue
        endif
        TotalVolume(IOW) = PreviousVolume(IOW) - VOW(IOW) + ROW(IOW) + KWOW(IOW) + &
                           TQIN * timeSettings%timestepSize - QOUTP * timeSettings%timestepSize
        RHelp = TotalVolume(IOW)
        if (idebug .ne. 0) write(idebug,*) ' Rhelp=Totalvolume=',rhelp
!       Bepaal bij totaal volume het peil
        idim = min (NrBergendOppervlakIndices(iow)+6, NStorageCurvePoints)
        Do i=1,idim
           TotalBergPeilArray(i)   = TotalOwBergPeil(i,iow)
           TotalBergVolumeArray(i) = TotalOwBergVolume(i,iow)
        Enddo
        Do i=1,NrBergendOppervlakIndices(iow)
           ExtraBergPeilArray(i)   = ExtraBergendOppPeil(i,iow)
           ExtraBergVolumeArray(i) = ExtraBergendOppVolume(i,iow)
        Enddo
        CALL RR_INTERP (idim, TotalBergVolumeArray, TotalBergPeilArray, &
                                        Rhelp, LvlOw(IOW), OwTotalLastInterpIndex(iow))
!       Bepaal bij dit peil het bergend volume en het open water volume
        CALL RR_INTERP (NrBergendOppervlakIndices(iow), ExtraBergPeilArray, &
                        ExtraBergVolumeArray, LvlOw(IOW), Rhelp,OwBergendOppLastInterpIndex(iow))
        ActualExtraBergendVolume(iow) = max (0.0, RHelp)
        CALL RR_INTERP (NVAL, PeilArray, VolumeArray, LVLOW(IOW), Rdum, OwLastInterpIndex(iow))
        VolOw(iow) = Rdum

        if (idebug .ne. 0) then
          write(idebug,*) ' Level ', LvlOw(iow)
          write(idebug,*) ' Volume ', VolOw(iow)
          write(idebug,*) ' Bergend Volume ', ActualExtraBergendVolume(iow)
          write(idebug,*) ' Total Volume ', TotalVolume(iow)
          write(idebug,*) ' Actual Extra Vol',  ActualExtraBergendVolume(iow)
          write(idebug,*) ' Prev.  Extra Vol',  PreviousExtraBergendVolume(iow)
          write(idebug,*) ' Volow, Volow0   ',  Volow(iow), Volow0(iow)
        endif
        VOLOW(IOW) = VOLOW0(IOW) - VOW(IOW) + ROW(IOW) + KWOW(IOW) + &
            TQIN * timeSettings%timestepSize - QOUTP * timeSettings%timestepSize &
              - ActualExtraBergendVolume(iow) + PreviousExtraBergendVolume(iow)
        RDum = VolOw(Iow)
        CALL RR_INTERP (NVAL, VolumeArray, PeilArray, RDum, LVLOW(IOW),OwLastInterpIndex(iow))
        CALL RR_INTERP (NVAL, VolumeArray, AreaArray, RDum, AROW(IOW), OwLastInterpIndex(iow))
! ARS xxxx: voorkom problemen met negatieve oppervlakken door laag peil
        ArOw(iow) = max (0.0001, ArOw(iow))

        LVLOW(IOW) = MIN (LVLOW(IOW), OWMNMA(IOW))
        if (idebug .ne. 0) then
          write(idebug,*) ' Na Interp: Volow  LvlOw  ', Volow(iow), LvlOw(iow)
        endif
        IF (LVLOW(IOW) .GE. OWMNMA(IOW) .AND. iDebug .ne. 0) WRITE(IDEBUG,*) ' Restricting advanced open waterlevel'
! nav ARS 10867;
        if (ArOw(Iow) .le. .0001) then
           TempString =  Id_Nod(INODE)

! Tijdelijk voor NSConsult uitgezet (version 3.207.59 and 59b)
! 28 Jan 2003 Check switched on again
!
            call ErrMsgStandard (972, 0, &
               ' Open water level low; area becomes negative. Check input level-area relations for node', &
                 Tempstring)
        Endif


    ! *** debug
      IF (iDebug .ne. 0) THEN
       WRITE(IDEBUG,*) ' Advanced Open water', Id_Nod(INODE)
       WRITE(IDEBUG,*) ' Timestep nr                :',ITMSTP
       WRITE(IDEBUG,*) ' Timestepsize in s          :',timeSettings%timestepSize
       WRITE(IDEBUG,*) ' Time coefficient t,t+1     :',timesettings%timeWeightFactor
       WRITE(IDEBUG,*) ' Bottom Level               :',BottomLevel(IOW)
       WRITE(IDEBUG,*) ' Old volume in M3           :',VOLOW0(IOW)
       WRITE(IDEBUG,*) ' New volume in M3           :',VOLOW (IOW)
       WRITE(IDEBUG,*) ' Old area   in M2           :',AROW0 (IOW)
       WRITE(IDEBUG,*) ' New area   in M2           :',AROW  (IOW)
       WRITE(IDEBUG,*) ' Old level  in M            :',LVLOW0(IOW)
       WRITE(IDEBUG,*) ' New level  in M            :',LVLOW (IOW)
       WRITE(IDEBUG,*) ' New Extra Bergend Volume   :',ActualExtraBergendVolume (IOW)
       WRITE(IDEBUG,*) ' Old Extra Bergend Volume   :',PreviousExtraBergendVolume (IOW)
       WRITE(IDEBUG,*) ' OWMNMA max. ow level       :',OWMNMA(IOW)
       WRITE(IDEBUG,*) ' rainfall open water M3     :',ROW   (IOW)
       WRITE(IDEBUG,*) ' evaporation open water M3   :',VOW   (IOW)
       WRITE(IDEBUG,*) ' seepage in M3              :',KWOW  (IOW)
       WRITE(IDEBUG,*) ' inflow paved area m3/s     :',QINOW (IOW,1), &
                                                 QINOW (IOW,1)*timeSettings%timestepSize
       WRITE(IDEBUG,*) ' inflow unpaved area m3/s   :',QIN0  (IOW,2), &
                                                 QIN0  (IOW,2)*timeSettings%timestepSize
       WRITE(IDEBUG,*) ' inflow greenhouse area m3/s:',QINOW (IOW,3), &
                                                 QINOW (IOW,3)*timeSettings%timestepSize
       WRITE(IDEBUG,*) ' inflow structures prev.iter:',QIN0  (IOW,4), &
                                                 QIN0  (IOW,4)*timeSettings%timestepSize
       WRITE(IDEBUG,*) ' outflow via structures     :',QOUTP
       WRITE(IDEBUG,*) ' inflow from RWZI prev.iter :',QIN0 (IOW,5), &
                                                 QIN0 (IOW,5)*timeSettings%timestepSize
       WRITE(IDEBUG,*) ' inflow from RWZI m3/s      :',QINOW (IOW,5), &
                                                 QINOW (IOW,5)*timeSettings%timestepSize
       WRITE(IDEBUG,*) ' inflow industry prev.iter  :',QIN0  (IOW,6), &
                                                 QIN0  (IOW,6)*timeSettings%timestepSize
       WRITE(IDEBUG,*) ' inflow from Industry m3/s  :',QINOW (IOW,6), &
                                                 QINOW (IOW,6)*timeSettings%timestepSize
       WRITE(IDEBUG,*) ' inflow from Sacr/RRRunoff m3/s  :',QINOW (IOW,7), &
                                                 QINOW (IOW,7)*timeSettings%timestepSize
      ENDIF
    endif

!   idebug = 0
    RETURN
  END  subroutine cmpOW



Logical function constant(testArr)

  ! als tussen het eerste en x-te element het verschil > 1.0E-9 (groter nul)
  ! dan kan er geen sprake zijn van een constante reeks
  ! Als alle elementen onderzocht zijn en er is geen verschil geconstateerd
  ! dan is er een constante reeks

  Real testArr(6)
  Integer Teller

  constant = .TRUE.
  do teller = 2, 6
    if ( (testArr(teller) - testArr(1)) .gt. 1.0E-9) then
      constant = .FALSE.
      return
    end if
  end do

  return
end function constant


  Subroutine GetOwH0fromTable (H0Lvl, Iow)
    ! *********************************************************************
    ! *** Last update: 21 March 2000                      by: Geert Prinsen
    ! *********************************************************************
    ! *** Brief description:
    ! *** ------------------
    ! ***    Bepaal H0 stijghoogte grondwater bij open water knoop
    ! *********************************************************************
    ! *** Input/output parameters:
    ! *** ------------------------
    ! ***  H0lvl  = stijghoogte diep grondwater
    ! ***  IOW    = index open water
    ! *********************************************************************

    INTEGER Iow, rowNr, TabelNr, Idebug, Iout1
    REAL H0Lvl
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
    TabelNr = OpwRefH0_TTable(iow)
    H0Lvl = GetNewValue(TableHandle, TabelNr, 1, RowNr, CurrentDate, CurrentTime, &
                        Idebug, iout1, DateTimeOutsideTable, .true.)
    if (idebug .ne. 0) WRITE (IDEBUG,*) ' H0 iow rowNr h0lvl', iow, RowNr, H0lvl

    RETURN
  END subroutine GetOwH0fromTable


  Subroutine GetOwSeepageFromTable (QSeep, QPerc, Iow)
    ! *********************************************************************
    ! *** Last update: 21 March 2000                      by: Geert Prinsen
    ! *********************************************************************
    ! *** Brief description:
    ! *** ------------------
    ! ***    Bepaal H0 stijghoogte grondwater bij open water knoop
    ! *********************************************************************
    ! *** Input/output parameters:
    ! *** ------------------------
    ! ***  H0lvl  = stijghoogte diep grondwater
    ! ***  IOW    = index open water
    ! *********************************************************************

    INTEGER Iow, rowNr, TabelNr, Idebug, Iout1
    REAL QSeep, QPerc
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
    TabelNr = OpwRefSeepage_TTable(iow)
    QPerc = 0
    QSeep = GetNewValue(TableHandle, TabelNr, 1, RowNr, CurrentDate, CurrentTime, &
                        Idebug, iout1, DateTimeOutsideTable, .true.)
!   convert to m/s
    if (idebug .ne. 0) WRITE (IDEBUG,*) ' From table GetNewValue Qseep', QSeep
    QSeep = QSeep * Mm2M / NrsDay
!   assign separate values to Kwel and Wegzijging
    If (QSeep .lt. 0) then
        QPerc = -1. * QSeep
        QSeep = 0.
    Endif
    if (idebug .ne. 0) WRITE (IDEBUG,*) ' Iow rowNr QSeep QPerc', iow, RowNr, QSeep, QPerc

    RETURN
  END subroutine GetOwSeepageFromTable

  Subroutine GetOwSeepageConcFromTable (CSeep, Iow)
    ! *********************************************************************
    ! *** Last update: 21 March 2000                      by: Geert Prinsen
    ! *********************************************************************
    ! *** Brief description:
    ! *** ------------------
    ! ***    Bepaal H0 stijghoogte grondwater bij open water knoop
    ! *********************************************************************
    ! *** Input/output parameters:
    ! *** ------------------------
    ! ***  H0lvl  = stijghoogte diep grondwater
    ! ***  IOW    = index open water
    ! *********************************************************************

    INTEGER Iow, rowNr, TabelNr, Idebug, Iout1
    REAL CSeep
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
    TabelNr = OpwRefSeepageConc_TTable(iow)
    if (idebug .ne. 0) WRITE (IDEBUG,*) ' TableNr Cseep', TabelNr
    CSeep = GetNewValue(TableHandle, TabelNr, 1, RowNr, CurrentDate, CurrentTime, &
                        Idebug, iout1, DateTimeOutsideTable, .true.)
    if (idebug .ne. 0) WRITE (IDEBUG,*) ' Iow rowNr CSeep ', iow, RowNr, CSeep

    RETURN
  END subroutine GetOwSeepageConcFromTable


  Subroutine Init1OpenWater (Ievent, Idebug, Iout1)
    ! *********************************************************************
    ! *** Last update:
    ! *********************************************************************
    ! *** Brief description:
    ! *** ------------------
    ! ***    Initialisatie Openwater aan begin van bui
    ! *********************************************************************

      Implicit none

      Integer IEvent, iDebug, Iout1

      INTEGER iknoop, inode, iow, idim, i
      Real    Rdum, RHelp, RHelp2
      Logical Err972
      Character(len=40) TempString

      Real PeilArray(6), AreaArray(6), VolumeArray(6)  ! aanname dat NVAL=6 zoals in Conf_Arr.
      Real TotalBergPeilArray(NStorageCurvePoints), TotalBergVolumeArray(NStorageCurvePoints)
      Real ExtraBergPeilArray(NStorageCurvePoints), ExtraBergVolumeArray(NStorageCurvePoints)

        Err972 = .false.

        DO IOW  = 1,NCOW
            Call DetermineTargetLevel (CurrentTargetLevel(iow), IOW)
            LvlOw(iow) = CurrentTargetLevel(iow)
            if (idebug .ne. 0)  write(idebug,*) ' New Method Target levels', lvlOW(iOw)
! mei 99: zet ook LvlOw0
            LvlOw0(iow) = LvlOw(iow)
! juli 99: let op bergend oppervlak volume
            ActualExtraBergendVolume(iow) = 0.0
            PreviousExtraBergendVolume(iow) = 0.0

           if (idebug .ne. 0)  WRITE(IDEBUG,*) ' Open water level', iow, lvlow(iow)

           Do i=1,NVal
              PeilArray(i) = PeilOw(i,iow)
              AreaArray(i) = AreaOw(i,iow)
              VolumeArray(i) = VoluOw(i,iow)
           Enddo
           Do i=1,NStorageCurvePoints
              TotalBergPeilArray(i)   = TotalOwBergPeil(i,iow)
              TotalBergVolumeArray(i) = TotalOwBergVolume(i,iow)
           Enddo
           Do i=1,NrBergendOppervlakIndices(iow)
              ExtraBergPeilArray(i)   = ExtraBergendOppPeil(i,iow)
              ExtraBergVolumeArray(i) = ExtraBergendOppVolume(i,iow)
           Enddo

           CALL RR_INTERP (NVAL, PeilArray, AreaArray, LVLOW(IOW), AROW (IOW), OwLastInterpIndex(iow))
           CALL RR_INTERP (NVAL, PeilArray, VolumeArray, LVLOW(IOW), RDum      , OwLastInterpIndex(iow))
           VolOw(Iow) = Rdum
! Total volume; assume no initial inundation !!!!
           TotalVolume(Iow) = VolOw(iow)
! ARS 13964: Check if at initial level inundation occurs; if so, give message!
           If (FixARS13964) then
             RHelp = TotalVolume(IOW)
             idim = min (NrBergendOppervlakIndices(iow)+6, NStorageCurvePoints)
             CALL RR_INTERP (idim, TotalBergVolumeArray, TotalBergPeilArray, &
                                              Rhelp, Rhelp2, OwTotalLastInterpIndex(iow))
             If (Rhelp2 .lt. (LvlOw(iow)-0.001)) then
                call ErrMsgStandard (974, 0, &
                   ' For one or more open water nodes there is a connected node' // &
                   ' with surface level below initial open water level.' // &
                   ' This will result in an initial inundation of that area','')
                DO INODE=1,NCNODE
                   IF (EINode(INODE,3).EQ. 4 .AND. &
                        EINode(INODE,2) .EQ. IOW) GOTO 110
                ENDDO
  110           IKNOOP=INODE
                TempString =  Id_Nod(INODE)
                call SetMessage(LEVEL_INFO, 'Open water node '//trim(TempString))
             Endif
             CALL RR_INTERP (idim, TotalBergPeilArray, TotalBergVolumeArray, &
                          LVLOW(IOW), RDum, OwBergendOppLastInterpIndex(iow))
             TotalVolume(Iow) = Rdum
!            Bepaal bij dit peil het bergend volume en het open water volume
             CALL RR_INTERP (NrBergendOppervlakIndices(iow), ExtraBergPeilArray, &
                             ExtraBergVolumeArray, LvlOw(IOW), Rhelp,OwBergendOppLastInterpIndex(iow))
             ActualExtraBergendVolume(iow) = max (0.0, RHelp)
             CALL RR_INTERP (NVAL, PeilArray, VolumeArray, LVLOW(IOW), Rdum, OwLastInterpIndex(iow))
             VolOw(iow) = Rdum
           Endif
!          CALL RR_INTERP (NrBergendOppervlakIndices(iow), TotalOwBergPeil(1,IOW), TotalOwBergVolume(1,IOW),
!                       LVLOW(IOW), RDum, OwBergendOppLastInterpIndex(iow))
!          TotalVolume(Iow) = Rdum
! ARS xxxx: voorkom problemen met negatieve oppervlakken door laag peil
           ArOw(iow) = max (0.0001, ArOw(iow))

           QOUT0  (IOW) = 0.0
           QOUTOW (IOW) = 0.0
!ARS926 extra check
           IF (VOLOW (IOW) .LT. -0.01 .OR. AROW(IOW) .LE.  0.0001) THEN
               DO INODE=1,NCNODE
                  IF (EINode(INODE,3).EQ. 4 .AND. &
                      EINode(INODE,2) .EQ. IOW) GOTO 111
               ENDDO
!              call SetMessage(LEVEL_FATAL, 'IKNOOP open water not found and negative area or volume')
  111          IKNOOP=INODE
               Err972 = .true.
               TempString = Id_Nod(IKNOOP)
               call SetMessage(LEVEL_ERROR, 'Open water '//trim(TempString)//' negative volume or area at start of event')
           ENDIF
        ENDDO

        if (Err972) then
! Tijdelijk voor NSConsult uitgezet (version 3.207.59 and 59b)
! 28 Jan 2003 Check switched on again
           call ErrMsgStandard (972, 0, &
              ' For one or more nodes the open water level is low; area becomes negative. ', &
              ' Check input level-area relations ')
        Endif


      Return
      END subroutine Init1OpenWater



  Subroutine Init2OpenWater
    ! *********************************************************************
    ! *** Last update:
    ! *********************************************************************
    ! *** Brief description:
    ! *** ------------------
    ! ***    Stuk uit sub Init2: initialisie van openwater per tijdstap
    ! *********************************************************************

      Implicit none

      Integer Iow, inode

      DO IOW  = 1,NCOW
!        Determine Target level current timestep
         Call DetermineTargetLevel (CurrentTargetLevel(iow), IOW)

!        Initialisation for variable seepage using a H0Table
         If (OwSeepageCompOption(iow) .eq. 2) Then
            Call GetOwH0FromTable (OwH0Actual(iow), iow)
         ElseIf (OwSeepageCompOption(iow) .ge. 4) Then
            Call GetOwSeepageFromTable (OWKwel(iow), OWWegZ(iow), iow)
         Endif
      ENDDO
      DO INode= 1,NCNode
         if (Einode(inode,3) .eq. 4) then
            IOW  = Einode(Inode,2)
            If (OwSeepageCompOption(iow) .eq. 5) Then
               Call GetOwSeepageConcFromTable (SltKwl(inode), iow)
            Endif
         Endif
      ENDDO

!     Vector initialisations
      VOLOW0  = VOLOW
      AROW0   = AROW
      LVLOW0  = LVLOW
      QOUT0   = 0.0
      QOUTOW  = 0.0
      QINOW   = 0.0
      QIN0    = 0.0
! juli 99: let op bergend oppervlak volume
      PreviousExtraBergendVolume = ActualExtraBergendVolume
      PreviousVolume  = TotalVolume


      Return
      END subroutine Init2OpenWater





      Subroutine WrInputDataOpenWater (Iout9, Iout5, RnDate, RnTime)
        ! *********************************************************************
        ! *** Last update:
        ! *********************************************************************
        ! *** Brief description:
        ! *** ------------------
        ! ***    Stuk uit sub WrData: uitvoer van Openwater in *.Out files
        ! *********************************************************************

        Implicit none


        Integer      INODE, IKIND, INR, i, k
        Integer      IOUT9, IOUT5
        Integer*2    RNDATE(3), RNTIME(4)

! Open water
      IF (NCOW .GT. 0) THEN
         WRITE(IOUT9,14)
   14    FORMAT (//,' Summary input data open water      ',//,  &
              ' Node identification    Node        Maximum  Reference   Target',&
              30X,'  6 levels',20X,' 6 surfaces    ',/,        &
              '                        name         level     level     levels',&
              30X,'  in m NAP',20X,' in hectare',/,185('='))
         DO INODE =1,NCNODE
          IKIND = EINode(INODE,3)
          INR   = EINode(INODE,2)
          IF (IKIND .EQ. 4) THEN
            WRITE(IOUT9,24) Id_Nod(INODE),&
                            NamNod(INODE), &
                            MAXLVL(INR), REFLVL(INR),&
                            (PEILOW(K,INR),K=1,NVAL), (AREAOW(K,INR)/HA2M,K=1,NVAL)
   24      FORMAT (A20,1X,A12,1X,2F10.2,3X,'Table ',6(F9.2,1X),6(F9.3,1X))
          ENDIF
         ENDDO
      ENDIF


! open water
      If (ncow .gt. 0 .and. OutputDesired(4) ) then
        WRITE(IOUT5,51) RNDATE(3),RNDATE(2),RNDATE(1),(RNTIME(I),I=1,2), CASENM
   51   FORMAT (' Run datum (DD-MM-YY) and time:',I2,'-',I2,'-',I4,2X,I2,':',I2,//,&
                ' Case name                 ',A20,/)
        IF (IOPT2(2) .EQ. 0) THEN
          WRITE(IOUT5,1007) '[m]','[m]','[hour]'
        ELSE
          WRITE(IOUT5,10071) '[hour]'
        ENDIF
 1007   FORMAT(//,' Maxima per event',//,&
                  ' Event   Start     Node identification   Node   ',&
                  '         Max_level    Max_accepted   Exceedance   ',/,&
                  '  nr  year-mon-day', 23X, 'name            ',A6,10X,A6,10X,A6,/,99('='))
10071   FORMAT(//,' Maxima per event',//, &
                  ' Event   Start     Node identification  Node    ',&
                  '         Max_level    Max_accepted   Exceedance   ',/,&
                  '  nr  year-mon-day                      name',&
                  '            -ref_Lvl (m) -ref_Lvl (m)  ',6X,A6,/,99('='))
        WRITE(IOUT5,10072)
10072   FORMAT(' N.B.  Output only as max. accepted waterlevel is exceeded',/,99('='))
      Endif

      Return
      END subroutine WrInputDataOpenWater


  Subroutine Wr1OutOpenWater (Iout5, Ievent, Month, INode, Iow)
    ! *********************************************************************
    ! *** Last update:
    ! *********************************************************************
    ! *** Brief description:
    ! *** ------------------
    ! ***    Stuk uit sub Wr1Out: uitvoer van OpenWater node: maxima per event in OUT file
    ! *********************************************************************

      Implicit none

    ! variables

    Integer     INODE, IOw, Iout5, Ievent
    Integer     ExcDur
    Real        Peil, PeilMx
    CHARACTER(len=3) MONTH(12)

           if (.not. associated(OWMLVL)) return  ! If there is nothing, do nothing

! peil tov ref. level
           PEIL = OWMLVL(IOW,IEVENT,1)
           PEILMX = MAXLVL(IOW)
!          IF (IOPT2(2) .EQ. 1) THEN
!              PEIL = PEIL - REFLVL(IOW)
!              PEILMX = PEILMX - REFLVL(IOW)
!          ENDIF
! overschrijding in uren
           EXCDUR = OWEXC (IOW,2,IEVENT) / NRSHR
           If (ExcDur .gt. 0) then
             WRITE(IOUT5,1008) IEVENT, EventStartDateTime(IEVENT,1),MONTH(EventStartDateTime(IEVENT,2)), &
                   EventStartDateTime(IEVENT,3),&
                   Id_Nod(INODE),NamNod(INODE), &
                   PEIL, PEILMX,EXCDUR
 1008        FORMAT (I4,1X,I4,1X,A3,1X,I3,3X,A20,1X,A15,1X,F12.3,1X,F12.3,1X,I10)
           Endif

  Return
  END subroutine Wr1OutOpenWater


  Subroutine SetVulling (Vulling, ActualLevel, MaximumLevel, TargetLevel)
!  Compute filling percentage open water as number between 0 and 1
!   0 :  actual level <= target level
!   1 :  actual level = maximum level
!   May 2002: > 1 also allowed

     Real Vulling, ActualLevel, MaximumLevel, TargetLevel

! ARS 10719: also take into account possility that MaxLvl = TargetLvl
     If (MaximumLevel .gt. TargetLevel) then
        Vulling = (ActualLevel - TargetLevel) / (MaximumLevel - TargetLevel)
     Else
        Vulling = 1.0
     Endif
     Vulling = Max (0.0, Vulling)
! ARS ... : only limit to 100 % is switch in Delft_3b.Ini file is on.
     if (VullingsGraadMaximum100) Vulling = Min (1.0, Vulling)

     Return
  End Subroutine SetVulling

  Subroutine RROpenWater_IndexEvapPrecip

    Integer iOut1

    iOut1  = ConfFil_get_iOut1()

    indexevap = 5
    indexprecip = 4

!   write(*,*) ' DSRMAP(13,3) =', DSRMAP(13,3)(1:)
!   write(*,*) ' DSRMAP(13,2) =', DSRMAP(13,2)(1:)
    indexevap   = max (index(DSRMAP(13,3)(1:),'evap'), index(DSRMAP(13,3)(1:),'Evap'))
    if (indexevap .le. 0) then
       indexevap = 5
    else
       indexevap = 3
    endif

    indexprecip = max (index(DSRMAP(13,2)(1:),'prec'), index(DSRMAP(13,2)(1:),'Prec'))
    if (indexprecip .le. 0) then
       indexprecip = 4
    else
       indexprecip = 2
    endif

    if (ncowrain .le. 0) then
       indexevap = 1
       indexprecip = 1
       call SetMessage(LEVEL_WARN, 'RROpenWater_IndexEvap and IndexPrecip put to 1, not available since no open water')
    endif

    if (Iout1 .ne. 0) write(iout1,*) ' RROpenWater_IndexEvap=', indexevap
    if (Iout1 .ne. 0) write(iout1,*) ' RROpenWater_IndexPrecip=', indexprecip

  return
  end Subroutine RROpenWater_IndexEvapPrecip




   !> If success, function returns Values array of length ElementCount
  !! for unpaved elementset on specific quantity handle
  function RR_GetOpenWaterDataByIntId(QuantityHandle, ElementCount, Values) result(success)

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
    integer indexevap, indexprecip

    indexevap = 5
    indexprecip = 4

    Values  = 0
    success = .true.

    indexevap   = max (index(DSRMAP(13,3)(1:),'evap'), index(DSRMAP(13,3)(1:),'Evap'))
    if (indexevap .le. 0) then
       indexevap = 5
    else
       indexevap = 3
    endif

    indexprecip = max (index(DSRMAP(13,2)(1:),'prec'), index(DSRMAP(13,2)(1:),'Prec'))
    if (indexprecip .le. 0) then
       indexprecip = 4
    else
       indexprecip = 2
    endif

    select case (QuantityHandle)
    ! Subdivide for various variables
    case(RRiEvaporationSurface)
    !RR Open water surface evaporation in m3/s
            ! If a stack overflow occurs due to this array operation, use a do loop
            if (NLNK > 0) then
                Values(1:NLNK) = RslMap16_flows(indexevap, 1:NLNK, 1)
            else
                success = .false.
            endif
    case(RRiRainfall)
    !RR Open water rainfall in m3/s
            ! If a stack overflow occurs due to this array operation, use a do loop
            if (NLNK > 0) then
                Values(1:NLNK) = RslMap16_flows(indexprecip, 1:NLNK, 1)
            else
                success = .false.
            endif
    case (RRiOpenwaterlevel)
            Values = LvlOw
    case default
    ! Something is wrong
        success = .false.
    end select

  end function

end module Openwater
