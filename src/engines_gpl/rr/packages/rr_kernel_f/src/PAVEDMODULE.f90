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


module Paved

  use Conf_Fil
! use Conf_Arr
  use Boundary
  use RWZI
  use Network
  use Crop
  use NWRW
  use Openwater
! use RR_Meteo
  use NewTables

  use Dh_alloc
  use ReadLib

  implicit none

  ! types
  type SewerVars
    Integer systemType
    Real BMAXST      !maximumStorageStreet
    Real BINIST      !initialStorageStreet
    Real BMAXRI(2)   !maximumStorage(2)
    Real BINIRI(2)   !initialStorage(2)
    Integer Q2VOW(2) !directionPumps(2) discharge to open water or boundary
                     !  komt het weggemalen water in openwater (true, =1?)
                     !  of boundary (false, =0?) of RWZI (2)
    Real OverflowLevel(2)       ! sewer overflow level (overstort drempelhoogte)
    Logical Inflowpossible(2)   ! is inflow van RR-open water of boundary mogelijk?
  end type SewerVars


  ! variables

  INTEGER, Pointer, SAVE ::  VHGBND(:), VHGRWZI(:)
  Real, Pointer ::  nrPeople (:)
  Integer, Pointer ::  dwaCalcOption(:), RunoffCalcOption(:)
  Logical  OldPavedComputations

  ! temporarily stored here because otherwise a circular reference between
  ! NWRW and Paved occurred
  ! dwaDisPaved(:, :) originates in Paved and is used in NWRW
  Real, Pointer, save :: dwaDisPaved(:, :), DWAPaved(:), DWASalt(:)

  Real sewerPumpCapacity(2)
  REAL, Pointer, SAVE ::     AREAVH(:), LVLVH(:), Q2VMAX(:,:)
  Integer, Pointer, Save ::      VhgRefQC_TTable(:)

  type (SewerVars), allocatable :: sewer(:)

  ! *** results verhard gebied
  ! ***
  ! *** BVSTR = berging op straat aan eind huidige tijdstap
  ! *** BVSTR0= berging op straat aan eind vorige tijdstap
  ! *** BVRL  = berging in riool aan eind huidige tijdstap; 1 = RWA of gemengd; 2=DWA
  ! *** BVRL0 = berging in riool aan eind vorige tijdstap
  ! *** RV    = regenval op straat huidige tijdstap
  ! *** VV    = verdamping van straat huidige tijdstap
  ! *** INV   = instroom riolering huidige tijdstap
  !                     ( ,1)=van oppervlak naar RWA, ( ,2)=van RWA naar DWA bij verbeterd gescheiden stelsels
  ! *** Q1V   = overstort riolering huidige tijdstap   ( ,1) = RWA of gemengd; ( ,2)= DWA
  ! *** Q2V   = uitmaling riolering huidige tijdstap   ( ,1) = RWA of gemengd; ( ,2)= DWA

  REAL, Pointer, SAVE ::     BVSTR (:), BVRL (:,:), &
                                 BVSTR0(:), BVRL0(:,:), &
                                 RV    (:), VV   (:), &
                                 INV   (:,:), Q2V  (:,:), &
                                 Q1V   (:,:)
! Runoff Calculation options
  REAL, Pointer, SAVE ::     RunoffFactor (:)
  REAL, Pointer, SAVE ::     PavedVolDyn  (:), PavedVolDyn0(:)


!Paved output
  ! *** VHMBPC = maximum bergingspercentage verhard gebied riool, per event
  !              ( ,1) = mixed of RWA riool in mm
  !              ( ,2) = DWA riool in mm
  !              ( ,3) = storage on street in mm
  ! *** VHMQOU = maximum outflow verhard gebied, per event
  ! ***          ( ,1) = via gemaal naar open water of boezem
  ! ***          ( ,2) = overstort naar open water
  ! ***          ( ,3) = totaal naar open water
  ! ***          ( ,4) = regenval
  ! ***          ( ,5) = DWA-inflow in RWA/mixed
  ! ***          ( ,6) = DWA-inflow in DWA/mixed
  ! ***          ( ,7) =
  ! ***          ( ,8) =
  ! ***          ( ,9) =
  ! ***          ( ,10)=
  ! ***          ( ,11)=
  ! ***          ( ,12)=
  ! ***          ( ,13)=
  Real, Pointer, save :: VHMBPC(:,:,:), VHMQOU(:,:,:)

  Integer   MaxQHPoints
  parameter (MaxQHPoints=100)
  Real, Pointer, save :: QH_Q(:,:), QH_h(:,:)
  Integer, Pointer, save :: QHTableLength(:)

contains

  subroutine Paved_confAr1(iOut1)

    implicit none

    Integer iOut1, nrNodes, nrVhg, Allocation_Error
!   Integer nVhg  ! nvhg is al gedeclareerd in module Network !!!
    Logical Success

    success = .true.
    nrNodes = Network_get_nrNodes()
    nrVhg = Network_get_nrVhg()

    NVHG = MAX (1, nrVHG) !paved

    IF ((NrVHG .GT. 0) .and. (iOut1 .ne. 0))  WRITE(IOUT1,*) ' Paved areas           =',NVHG

          !*** Data verhard gebied
    success = dh_allocinit (NVHG, AREAVH, LVLVH, NrPeople, 0E0)
    success = success .and. dh_allocinit (NVHG, dwaCalcOption, RunoffCalcOption, 0)
    success = success .and. dh_allocinit (NVHG, 2, Q2VMax, 0E0)
    ALLOCATE ( sewer(nVhg), Stat=Allocation_Error )
    success = success .and. (allocation_error .eq. 0)
!   Initialise sewer
    sewer%systemtype = 0
    sewer%BMAXST    = 0.
    sewer%BiniSt    = 0.
    sewer%BMaxRi(1) = 0.
    sewer%BMaxRi(2) = 0.
    sewer%BIniRi(1) = 0.
    sewer%BIniRi(2) = 0.
    sewer%Q2Vow(1)  = 0
    sewer%Q2Vow(2)  = 0
    sewer%overflowlevel(1) = -999.99
    sewer%overflowlevel(2) = -999.99
    sewer%inflowpossible(1) = .false.
    sewer%inflowpossible(2) = .false.

!   ALLOCATE ( AREAVH(NVHG), LVLVH(NVHG), Q2VMAX(NVHG,2), sewer(nVhg), nrPeople(nVhg), &
!              dwaCalcOption(nVhg), Stat=Allocation_Error )
!   ALLOCATE ( VHGBND(NVHG), VHGRWZI(NVHG), Stat=Allocation_Error )
    success = success .and. dh_allocinit (NVHG, VhgBnd, VhgRwzi, 0)
    if (.not. success) call ErrMsgStandard (981, 0, ' Error allocating arrays in subroutine ',' Paved_ConfAr1')

          !*** Results verhard gebied
!    ALLOCATE  ( BVSTR (NVHG), BVRL (NVHG,2), &
!                BVSTR0(NVHG), BVRL0(NVHG,2), &
!                RV    (NVHG), VV   (NVHG), &
!                INV   (NVHG,2), Q2V  (NVHG,2), &
!                Q1V   (NVHG,2), Stat=Allocation_Error )
!   Allocate (dwaDisPaved(nrNodes, 26), DWAPaved(NVHG), DWASalt(NVHG), Stat=Allocation_Error )
!    Allocate (VhgRefQC_TTable(NVHG), Stat=Allocation_Error )
    success = success .and. dh_allocinit (NVHG, BVSTR, BVSTR0, 0E0)
    success = success .and. dh_allocinit (NVHG, 2, BVRL, BVRL0, 0E0)
    success = success .and. dh_allocinit (NVHG, RV, VV, 0E0)
    success = success .and. dh_allocinit (NVHG, 2, INV, Q2V, Q1V, 0E0)
    success = success .and. dh_allocinit (NrNodes, 26, DwaDisPaved, 0E0)
    success = success .and. dh_allocinit (NVHG, DwaPaved, DwaSalt, 0E0)
    success = success .and. dh_allocinit (NVHG, RunoffFactor, 0E0)
    success = success .and. dh_allocinit (NVHG, PavedVolDyn, PavedVolDyn0, 0E0)
    success = success .and. dh_allocinit (MaxQHPoints, NVHG, QH_Q, 0E0)
    success = success .and. dh_allocinit (MaxQHPoints, NVHG, QH_h, 0E0)
    success = success .and. dh_allocinit (NVHG, QHTableLength, 0)
    success = success .and. dh_allocinit (NVHG, VhgRefQC_TTable, 0)
    if (.not. success) call ErrMsgStandard (981, 0, ' Error allocating arrays in subroutine ',' Paved_ConfAr1')

! Vector/Array initialisation
!     VhgBnd  = 0
!     VhgRwzi = 0

    return
  end subroutine Paved_confAr1


  subroutine PavedOutput_Confar (Nevnt)

    Integer Nevnt
    Logical Success

    success = dh_allocinit (NVHG, 3, Nevnt, VhmBpc, 0E0)
    success = success .and. dh_allocinit (NVHG, Nmsr, Nevent, VhmQou, 0E0)
    if (.not. success) call ErrMsgStandard (981, 0, ' Error allocating arrays in subroutine ',' Paved_OutputConfAr')
! paved output
!    ALLOCATE ( VHMBPC(nVHG,3,Nevnt), VHMQOU(nVHG,Nmsr,Nevnt), Stat=Allocation_Error )
!                                           ' Output_ConfAr' )


    Return
  End subroutine PavedOutput_Confar




  SUBROUTINE Paved_readascii (infile1, infile2, infile3, infile4, SaltConcentrationDWF)

  Integer :: RetVal

  Integer(4)    Infile1, Infile2, Infile3, Infile4
  Integer        teller, i, iecode, iout1, inod, idebug, nrNodes, &
                 IOW, IBND, IPluv, IRWZI, IVhg, Nhlp
  Character(CharIdLength)   name, id
  Character(1000) string
  Logical        allow, found, endfil, QC_TTable, Occurs, TabYesNo
  Logical        Err969
  Parameter     (NHLP=30)
  Integer       IDUM(32), NrColumns, TableNr !, FindString
  REAL          RDUM(32)
  Character(CharIdLength) CDUM(32), Tablename, NodeId
  Character(CharIdLength), Pointer :: STODEF(:), TBLDEF(:),DWADEF(:),QHDefinition(:)
  Character(Len=9999) :: BufString

  REAL          bmaxstdum, binistdum, bmaxridum(2), biniridum(2)
  INTEGER       compopt, nrVhg
  REAL          wc, wd, wh(24), c1dum, c2dum
  logical       err917, QHdefined
  Logical, Pointer :: AlreadyRead(:)
  Integer, Pointer :: ReferenceToDefinition(:)
  Logical Success
!
  integer       ileft, iright, ipoint, NrQHPoints, idum1
  Character(len=1) Klteken
  REAL          qhrelation_q(MaxQHPoints),qhrelation_h(MaxQHPoints)
  REAL          SaltConcentrationDWF

  allow = .false.
  found = .false.
  err917 = .false.
  QHDefined = .false.

  nrVhg = Network_get_nrVhg()
  iOut1  = ConfFil_get_iOut1()
  iDebug = ConfFil_get_iDebug()

  success = dh_allocinit (NrVHG, StoDef, TblDef, DwaDef, ' ')
  success = dh_allocinit (NrVHG, QHDefinition, ' ')
  success = success .and. dh_allocinit (NrVHG, ReferenceToDefinition, 0)
  success = success .and. dh_allocinit (NrVHG, AlreadyRead, .false.)
  if (.not. success) call ErrMsgStandard (981, 0, ' Error allocating arrays in subroutine ',' Paved_ReadAscii')
!  ALLOCATE   (STODEF(NRVHG), TBLDEF(NRVHG), DWADEF(NRVHG), Stat=Allocation_Error )
!  ALLOCATE   (ReferenceToDefinition(NRVHG), Stat=Allocation_Error )
!  ALLOCATE   (AlreadyRead(NRVHG), Stat=Allocation_Error )

! initialisatie parameters op nul
  if (nrVhg .gt. 0) then
     do teller = 1, nrVhg
        sewer(teller)%systemtype = 0
        sewer(teller)%bmaxst = 0
        sewer(teller)%binist = 0
        sewer(teller)%bmaxri(1) = 0
        sewer(teller)%bmaxri(2) = 0
        sewer(teller)%biniri(1) = 0
        sewer(teller)%biniri(2) = 0
        sewer(teller)%q2vow(1) = 0
        sewer(teller)%q2vow(2) = 0
        sewer(teller)%overflowlevel(1) = -999.99
        sewer(teller)%overflowlevel(2) = -999.99
        sewer(teller)%inflowpossible(1) = .false.
        sewer(teller)%inflowpossible(2) = .false.
     enddo
  endif
! eind initialisatie


! Read paved.3b file
  call SetMessage(LEVEL_DEBUG, 'Read Paved.3b file')
  Endfil = .false.
  teller = 0
  RetVal = 0
  QC_TTable = .False.
  Call SKPCOM (INfile1, ENDFIL,'ODS')
  do while (.not. endfil)
    READ(infile1,'(A1000)',END=21,ERR=150,IOSTAT=IECODE) STRING
! Skip regel als hij niet begin met juist keyword (PAVE)
    IF (STRING(1:4) .EQ. 'PAVE') then
! paved node id
     RetVal = RetVal + GetVAR2 (STRING,' id ',1,' Paved_readAscii',' paved.3b file',IOUT1, &
                   ID, RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
     ivhg = 0
     Call fndNd2(inod, id)
     if (inod .gt. 0) then
      ivhg = EiNode(inod,2)
      if (EiNode(inod,3) .eq. 1) then !  knoop bestaat en is verhard gebied
       if (alreadyRead(ivhg)) then
         call SetMessage(LEVEL_ERROR, 'Data for paved node '//id(1:Len_trim(id))//' double in datafile Paved.3B')
       else
        teller = teller + 1
        AlreadyRead(ivhg) = .true.
        Q2VMAX(ivhg,1) = 0.0
        Q2VMAX(ivhg,2) = 0.0
! area
        RetVal = RetVal + GetVAR2 (STRING,' ar ',2,' Paved_readAscii',' paved.3b file',IOUT1, &
                      CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
        AREAVH(ivhg) = RDUM(1)

! street level
        RetVal = RetVal + GetVAR2 (STRING,' lv ',2,' Paved_readAscii',' paved.3b file',IOUT1, &
                      CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
        LVLVH(ivhg) = RDUM(1)
! storage identification
        RetVal = RetVal + GetVAR2 (STRING,' sd ',1,' Paved_readAscii',' paved.3b file',IOUT1, &
                      CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
        STODEF(ivhg) = CDUM(1)

! sewer system type
        RetVal = RetVal + GetVAR2 (STRING,' ss ',3,' Paved_readAscii',' paved.3b file',IOUT1, &
                      CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
        SEWER(ivhg)%systemtype = IDUM(1)
! sewer pump capacity
        RetVal = RetVal + GetVAR2 (STRING,' qc ',3,' Paved_readAscii',' paved.3b file',IOUT1, &
                      CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
        IF (IDUM(1) .EQ. 1) THEN
           RetVal = RetVal + GetVAR2 (STRING,' qc 1 ',1,' Paved_readAscii',' paved.3b file',IOUT1, &
                         CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
           TBLDEF(ivhg) = CDUM(1)
           QC_TTable = .true.
        ELSE
           RetVal = RetVal + GetVRS2 (STRING,' qc 0 ',2,' paved-ReadAscii',' Paved.3b file', Iout1, &
                         CDUM(1), RDUM(1), IDUM(1), 2, IflRtn)
           TBLDEF(ivhg) = ''
           c1dum = RDUM(1)
           c2dum = RDUM(2)
           Q2VMAX(ivhg,1) = max ( Q2VMAX(ivhg,1), c1dum )
           Q2VMAX(ivhg,2) = max ( Q2VMAX(ivhg,2), c2dum )
           success = MakeConstTable(TableHandle, 2,TableNr,c1dum,c2dum, Idebug)
           if (.not. success) goto 21
           VhgRefQC_TTable(ivhg) = TableNr
           if (idebug .ne. 0) write(idebug,*) ' MakeConstTable TabelNr', TableNr
        ENDIF
! Direction of sewer pump discharge
        RetVal = RetVal + GetVRS2 (STRING,' qo ',3,' paved-ReadAscii',' Paved.3b file', &
                      IOUT1, CDUM(1), RDUM(1), IDUM(1), 2, IflRtn)
! Let op: inlezen q2vov(1) en q2vow(2) omgedraaid, zoals dat ook in 3bascbin gebeurt!
        SEWER(Ivhg)%Q2VOW(2) = IDUM(1)
        SEWER(Ivhg)%Q2VOW(1) = IDUM(2)
! Optional data on sewer overflow level
        Sewer(ivhg)%OverflowLevel(1) = LvlVh(ivhg)
        Sewer(ivhg)%OverflowLevel(2) = LvlVh(ivhg)
        ALLOW = .true.
        RetVal = RetVal + GetVAR2 (STRING,' so ',2,' Paved_readAscii',' paved.3b file',IOUT1, &
                          CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
        If (found .and. RetVal .eq. 0) then
           RetVal = RetVal + GetVRS2 (STRING,' so ',2,' paved-ReadAscii',' Paved.3b file', Iout1, &
                             CDUM(1), RDUM(1), IDUM(1), 2, IflRtn)
           Sewer(ivhg)%OverflowLevel(1) = min ( LvlVh(ivhg), Rdum(1) )
           Sewer(ivhg)%OverflowLevel(2) = min ( LvlVh(ivhg), Rdum(2) )
        Endif
! Optional data on sewer inflow possible
        Sewer(ivhg)%InflowPossible(1) = .false.
        Sewer(ivhg)%InflowPossible(2) = .false.
        RetVal = RetVal + GetVAR2 (STRING,' si ',2,' Paved_readAscii',' paved.3b file',IOUT1, &
                          CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
        If (found .and. RetVal .eq. 0) then
           RetVal = RetVal + GetVRS2 (STRING,' si ',3,' paved-ReadAscii',' Paved.3b file', Iout1, &
                             CDUM(1), RDUM(1), IDUM(1), 2, IflRtn)
           Sewer(ivhg)%InflowPossible(1) = (Idum(1) .ne. 0)
           Sewer(ivhg)%InflowPossible(2) = (Idum(2) .ne. 0)
        Endif
! Metro station id
        Allow = .false.
        RetVal = RetVal + GetVAR2 (STRING,' ms ',1,' Paved_readAscii',' paved.3b file',IOUT1, &
                      CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
        NAMMET(inod) = CDUM(1)
! areal adjustment factor rainfall on node, maybe missing,
        allow = .true.
        RetVal = RetVal + GetVAR2(STRING,' aaf ',2,' Paved-readAscii',' paved.3b file',IOUT1, &
                         CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, Iflrtn)
        if (found) AAFNodeRainfall(inod) = max(0.0, RDUM(1))    ! AAF >= 0
        allow = .false.
! Initial salt concentration
        RetVal = RetVal + GetVAR2 (STRING,' is ',2,' Paved_readAscii',' paved.3b file',IOUT1, &
                      CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
        SltIni(inod) = RDUM(1)
! number of people
        RetVal = RetVal + GetVAR2 (STRING,' np ',3,' Paved_readAscii',' paved.3b file',IOUT1, &
                      CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
        nrPeople(ivhg) = IDUM(1)
! DWA identification
        allow = .true.
        RetVal = RetVal + GetVAR2 (STRING,' dw ',1,' Paved_readAscii',' paved.3b file',IOUT1, &
                      CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
        if (found)  DWADEF(ivhg) = CDUM(1)
        if (idebug .ne. 0) then
            write(idebug,*) ' Read ',string(1:160)
            write(idebug,*) teller, ivhg, stodef(ivhg),tbldef(ivhg), dwadef(ivhg)
        Endif
!Salt concentration DWA not yet read; put by default at value read from Delft_3b.Ini file
!if missing, the default value is 400
        DWASalt(ivhg) = SaltConcentrationDWF
! runoff option; March 2004 Taiwan
        RetVal = RetVal + GetVAR2 (STRING,' ro ',3,' Paved_readAscii',' paved.3b file',IOUT1, &
                      CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
        RunoffCalcOption(ivhg) = IDUM(1)
        ! only support option 0=old method, 1=runoffdelay NWRW method, 2=Q-h relation, 3 = surface runoff-delay and sewer spill Q-h,  others -->  0
! Sobek 22739: Qh relation is implemented
        If (RunoffCalcOption(ivhg) .gt. 3 .or. RunoffCalcOption(ivhg) .lt. 0) RunoffCalcOption(ivhg) = 0
        If (RunoffCalcOption(ivhg) .eq. 1 .or. RunoffCalcOption(ivhg) .eq. 3) then
           ! rational method for surface inflow to sewer
           allow = .false.
           RetVal = RetVal + GetVAR2 (STRING,' ru ',2,' Paved_readAscii',' paved.3b file',IOUT1, &
                         CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
           RunoffFactor(ivhg) = RDUM(1)
        endif
        If (RunoffCalcOption(ivhg) .ge. 2) then
           ! qh relation for spill
           allow = .false.
           RetVal = RetVal + GetVAR2 (STRING,' qh ',1,' Paved_readAscii',' paved.3b file',IOUT1, &
                         CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
           QHDefinition(ivhg) = CDUM(1)
           QHDefined = .true.
        Endif
! end reading data from Paved.3B for runoff option; March 2004 Taiwan
       Endif
      Endif
     Endif
    Endif
    Call SKPCOM (INfile1, ENDFIL,'ODS')
  Enddo
 21 CONTINUE
    If (RetVal .gt. 0) call ErrMsgStandard (972, 0, ' Error reading Paved.3B file ', ' Error getting PAVE records')
    If (teller .lt. NrVhg)  then
        Do inod=1,NcNode
          ivhg= EiNode(inod,2)
          if (EiNode(inod,3) .eq. 1) then   ! en is paved node
            if (.not. AlReadyRead(ivhg)) then
              NodeId = ' '
              NodeId = Id_Nod(inod)
              String = ' '
              String = ' ' // NodeId(1:Len_trim(NodeId)) // ' is missing'
              call ErrMsgStandard (977, 0, ' Data for paved node ',String(1:Len_trim(String)) )
            endif
          endif
        Enddo
        call ErrMsgStandard (972, 0, ' Not enough Paved data found', &
                             ' Some paved nodes in netwerk schematisation are not present in Paved.3b file')
    Endif

! Read paved.sto file
  call SetMessage(LEVEL_DEBUG, 'Read Paved.sto file')
  Endfil = .false.
  teller = 0
  RetVal = 0
  Do ivhg = 1, nrvhg
     ReferenceToDefinition(ivhg) = 0
  Enddo

  Call SKPCOM (Infile2, ENDFIL,'ODS')
  Do while (.not. endfil)
    READ (Infile2,'(A1000)',END=211,ERR=150,IOSTAT=IECODE) STRING
    IF (STRING(1:4) .EQ. 'STDF') Then
      teller = teller + 1
! Read storage id
      RetVal = RetVal + GetVAR2 (STRING,' id ',1,' Paved_readAscii',' paved.sto file',IOUT1, &
                    CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
      name = CDUM(1)
! Eerst testen of storage definition wel gebruikt wordt, dan pas verwerken
      Ivhg = FindString (NcVhg, Stodef, Name, NcVhg, CaseSensitive)
      Occurs = (Ivhg .gt. 0)
      if (Ivhg .gt. 0) then
         if (ReferenceToDefinition(ivhg) .gt. 0) then
           call SetMessage(LEVEL_ERROR, 'Storage Definition '//name(1:Len_trim(Name))//' double in datafile Paved.Sto')
         endif
      endif
!     Verwerk storage definitie
      if (occurs) then
! Read maximum storage on streets
          RetVal = RetVal + GetVAR2 (STRING,' ms ',2,' Paved_readAscii',' paved.sto file',IOUT1, &
                        CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
          bmaxstdum = RDUM(1)
! Read initial storage on streets
          RetVal = RetVal + GetVAR2 (STRING,' is ',2,' Paved_readAscii',' paved.sto file',IOUT1, &
                        CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
          binistdum = RDUM(1)
! Read maximum storage sewer
          RetVal = RetVal + GetVRS2 (STRING,' mr ',2,' paved-ReadAscii',' paved.sto file', &
                        IOUT1, CDUM(1), RDUM(1), IDUM(1), 2, IflRtn)
          bmaxridum(1) = RDUM(1)
          bmaxridum(2) = RDUM(2)
! Read initial storage sewer
          RetVal = RetVal + GetVRS2 (STRING,' ir ',2,' paved-ReadAscii',' paved.sto file', &
                        IOUT1, CDUM(1), RDUM(1), IDUM(1), 2, IflRtn)
          biniridum(1) = RDUM(1)
          biniridum(2) = RDUM(2)
! Assign definition to individual nodes
          Do ivhg = 1, nrVhg
            if (StringComp(StoDef(Ivhg), Name, CaseSensitive) )  then
              ReferenceToDefinition(ivhg)=teller
              SEWER(ivhg)%BMAXST = (bmaxstdum * areavh(ivhg) *0.001)
              SEWER(ivhg)%BINIST = (binistdum * areavh(ivhg) *0.001)
              SEWER(ivhg)%BMAXRI(1) = (bmaxridum(1) * areavh(ivhg) *0.001)
              SEWER(ivhg)%BMAXRI(2) = (bmaxridum(2) * areavh(ivhg) *0.001)
              SEWER(ivhg)%BINIRI(1) = (biniridum(1) * areavh(ivhg) *0.001)
              SEWER(ivhg)%BINIRI(2) = (biniridum(2) * areavh(ivhg) *0.001)
            endif
          Enddo
      Endif
    Endif
    Call SKPCOM (Infile2, ENDFIL,'ODS')
  Enddo
211 Continue

   If (RetVal .gt. 0) call ErrMsgStandard (972, 0, ' Error reading Paved.Sto file ', ' Error getting STDF records')
!  Check if all references to storage definitions are resolved
   Err969 = .false.
   Do ivhg = 1, nrvhg
     if (ReferenceToDefinition(ivhg) .eq. 0 .and. STODEF (ivhg) .ne. '')  then
         Err969 = .true.
         call ErrMsgStandard (969, 0, ' Storage definition not found in .Sto file.', StoDef(ivhg))
     endif
   Enddo
   If (Err969) call ErrMsgStandard (972, 0, ' Not enough Paved data found', &
                              ' Some storage Definitions not present in Paved.Sto file')


! Read paved.dwa file
  Endfil = .false.
  call SetMessage(LEVEL_DEBUG, 'Read Paved.dwa file')
  teller = 0
  RetVal = 0
  Do ivhg = 1, nrvhg
      ReferenceToDefinition(ivhg) = 0
  Enddo

  Call SKPCOM (Infile3, ENDFIL,'ODS')
  Do while (.not. endfil)
    READ (Infile3,'(A1000)',END=2111,ERR=150,IOSTAT=IECODE) STRING
    IF (STRING(1:4) .eq. 'DWA') Then
       teller = teller + 1
! DWA definition node id
       RetVal = RetVal + GetVAR2 (STRING,' id ',1,' paved-ReadAscii',' Paved.DWA file',IOUT1, &
                     CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
       Name = cdum(1)
! Eerst testen of DWA definition wel gebruikt wordt, dan pas verwerken
      Ivhg = FindString (NcVhg, DWAdef, Name, NcVhg, CaseSensitive)
      Occurs = (Ivhg .gt. 0)
      if (Ivhg .gt. 0) then
         if (ReferenceToDefinition(ivhg) .gt. 0) then
           call SetMessage(LEVEL_ERROR, 'DWA Definition '//name(1:Len_trim(Name))//' double in datafile Paved.Dwa')
         endif
      endif
!     Verwerk DWA definitie
       if (occurs) then
 ! DWA keyword do: computation option
         RetVal = RetVal + GetVAR2 (STRING,' do ',3,' paved-ReadAscii',' Paved.DWA file',IOUT1, &
                       ID, RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
         CompOpt = idum(1)
! DWA keyword wc
         RetVal = RetVal + GetVAR2 (STRING,' wc ',2,' paved-ReadAscii',' Paved.DWA file',IOUT1, &
                       ID, RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
         wc = rdum(1)
! DWA keyword wd
         RetVal = RetVal + GetVAR2 (STRING,' wd ',2,' paved-eadAscii',' Paved.DWA file',IOUT1, &
                       ID, RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
         wd = rdum(1)
! DWA 24 hours
         RetVal = RetVal + GetVRS2 (STRING,' wh ',2,' paved-ReadAscii',' Paved.DWA file', &
                       IOUT1, CDUM(1), RDUM(1), IDUM(1), 24, IflRtn)
         DO I=1,24
            wh(i) = RDUM(I)
         ENDDO

! Assign definition to individual nodes
         Do Ivhg = 1, NrVhg
            If (StringComp(DWADef(Ivhg), Name, CaseSensitive) )  then
               ReferenceToDefinition(ivhg)=teller
               DwaCalcoption(Ivhg) = CompOpt
               DwaDispaved(Ivhg,1) = wc
               DwaDispaved(Ivhg,2) = wd
               DO I=1,24
                  DwaDispaved(Ivhg,i+2) = wh(i)
               ENDDO
            endif
         Enddo
       Endif
    Endif
    Call SKPCOM (Infile3, ENDFIL,'ODS')
  Enddo
2111 Continue

    If (RetVal .gt. 0) call ErrMsgStandard (972, 0, ' Error reading Paved.dwa file ', ' Error getting DWA  records')
    Err969 = .false.
    Do ivhg = 1, nrvhg
      if (DWADEF(ivhg) .eq. '') then
         DwaCalcoption(Ivhg) = 1
         DwaDispaved(Ivhg,1) = 0
         DwaDispaved(Ivhg,2) = 0
         DO I=1,23
            DwaDispaved(Ivhg,i+2) = 4
         ENDDO
         DwaDispaved(Ivhg,26) = 8
      elseif (ReferenceToDefinition(ivhg) .eq. 0 .and. DWADef(Ivhg) .ne. '') then
         Err969 = .true.
         call ErrMsgStandard (969, 0, ' DWA definition not found in .DWA file.', DWADef(ivhg))
      endif
    Enddo
    If (Err969) call ErrMsgStandard (972, 0, ' Not enough Paved data found', &
                                     ' Some DWA-definitions not present in Paved.dwa file')


! read paved.tbl file: tabellen 'QC_T' met sewer pump capacity als functie van tijd

! QC_T records, alleen als QC_TTable = .true.
    endfil = .not. QC_TTable
    if (.not. endfil) call SetMessage(LEVEL_DEBUG, 'Read Paved.Tbl file')
    if (idebug .ne. 0) write(idebug,*) ' QC_TTable =', QC_TTable, Endfil
    if (.not. endfil) Call SKPCOM (Infile4, ENDFIL,'ODS')
    Do while (.not. endfil)
       Success = GetRecord(Infile4, 'QC_T', Endfil, idebug, Iout1)     ! get record van keyword QC_T tot qc_t, zet in buffer
       IF (.not. success) GOTO 3111
       IF (ENDFIL) GOTO 3111
       Success = GetTableName (TabYesNo, TableName, ' id ', Iout1)     ! get table name via keyword ' id ', TabYesNo=TBLE found
       IF (.not. success) GOTO 3111
       If (TabYesNo .and. TableName .ne. '') Then
!         Er is een tabel gedefinieerd, met een niet-lege naam
!         Eerst testen of tabel definition wel gebruikt wordt, dan pas verwerken
          Ivhg = FindString (NcVhg, Tbldef, TableName, NcVhg, CaseSensitive)
          Occurs = (Ivhg .gt. 0)
          if (Ivhg .gt. 0) then
             if (VhgRefQC_TTable(ivhg) .gt. 0) then
                call SetMessage(LEVEL_ERROR, 'Table Definition '//Tablename(1:Len_trim(TableName))//' double in datafile Paved.Tbl')
             endif
          endif
          NrColumns = 2
          if (occurs .and. NrColumns .gt. 0) then
! Get table with name TableName, Nrcolumns data fields, result in global arrays; tabel nummer is TableNr
            Success = GetTable (TableHandle, TableName, NrColumns, TableNr, idebug, Iout1)
            IF (.not. success) GOTO 3111
! Set references
            Do ivhg = 1, nrvhg
              If (StringComp(TblDef(Ivhg), TableName, CaseSensitive) )  then
                 VhgRefQC_TTable(ivhg) = TableNr
! zet Q2VMax ivm uitvoer in 3B_Gener.Out
                 ! i = Ntab(TableNr,3)
                 c1dum = GetFirstTableValue(TableHandle, TableNr)  !TableData (i)
                 ! i = Ntab(TableNr,3) + NTab(TableNr,1)
                 c2dum = GetLastValue(TableHandle)  !TableData (i)
                 Q2VMAX(ivhg,1) = max ( Q2VMAX(ivhg,1), c1dum )
                 Q2VMAX(ivhg,2) = max ( Q2VMAX(ivhg,2), c2dum )
              endif
            Enddo
          Endif
       Endif
       Call SKPCOM (Infile4, ENDFIL,'ODS')
    Enddo

3111 Continue

! Check of alle referenties naar tabellen opgelost
    Err969 = .false.
    Do ivhg = 1, nrvhg
      if (VhgRefQC_TTable(ivhg) .eq. 0 .and. TBLDef(Ivhg) .ne. '') then
         Err969 = .true.
         call ErrMsgStandard (969, 0, ' QC_T Table definition not found in .Tbl file.', TblDef(ivhg))
      endif
    Enddo
    If (Err969) call ErrMsgStandard (972, 0, ' Not enough Paved data found', &
                                     ' Some QC_T Table Definitions not present in Paved.Tbl file')

! March 2004 Taiwan: Qh-relations for Runoff computations
! May 2010 Sobek JIRA 22739 Delfland Qh relation completely implemented
    Err969 = .false.
    Rewind (Infile4)
    KlTeken = '<'
    endfil = .not. QhDefined
    if (.not. endfil) call SetMessage(LEVEL_DEBUG, 'Read Paved.Tbl file; QH-relation')
    if (.not. endfil) Call SKPCOM (Infile4, ENDFIL,'ODS')
    Do while (.not. endfil)
       Success = GetRecord(Infile4, 'QHTB', Endfil, idebug, Iout1)    ! get record van keyword QHTB tot inst, zet in buffer
       IF (.not. success) GOTO 6111
       IF (ENDFIL) GOTO 6111
       Success = GetTableName (TabYesNo, TableName, ' id ', Iout1)     ! get table name via keyword ' id ', if table defined
       IF (.not. success) GOTO 6111
       If (TabYesNo .and. TableName .ne. '') Then
!         Er is een tabel gedefinieerd, met een niet-lege naam
          NrColumns = 1   ! voor Scurve 1 kolom
!         Eerst testen of QHdefinition wel gebruikt wordt, dan pas verwerken
          IVhg = FindString (NcVhg, QHDefinition, TableName, NcVhg, CaseSensitive)
          Occurs = (Ivhg .gt. 0)
!         Verwerken QH definitie
          if (occurs) then
! Get QHrelation
             success = GetStringFromBuffer(BufString)
             IF (.not. success) GOTO 6111
             Ileft  = INDEX(BufString(1:nbuf), 'TBLE') +4
             Iright = INDEX(BufString(1:nbuf), 'tble')
             NrQHPoints = Max (1, CntStr (klteken, BufString(ileft:iright)) )
             if (NrQHPoints .gt. MaxQhPoints) then
                 call SetMessage(LEVEL_FATAL, 'Reduce input QH-relation to 100 points')
             endif
             ! get Q and h; eerst < tekens verwijderen, dan free format inlezen
             do idum1=ileft, iright
                if (BufString(idum1:idum1) .eq. klteken) BufString(idum1:idum1)=' '
             enddo
             read (BufString(ileft:),*) (Qhrelation_Q(ipoint),Qhrelation_h(ipoint), ipoint=1, NrQHPoints)
             do ipoint=1,NrQHPoints
                if (idebug .ne. 0) write(idebug,*) ' QHPoints Q and h',Qhrelation_Q(ipoint),Qhrelation_h(ipoint)
             enddo
! check that h values are increasing
             do ipoint=2,NrQHPoints
                if (QHrelation_h(ipoint) .le. Qhrelation_h(ipoint-1)) then
                   Err969 = .true.
                   call ErrMsgStandard (969, 0, ' QHrelation  hvalues should be increasing, but they are not! Adjust qh-relation ', QHDefinition(ivhg))
                endif
             enddo
! Set references
             Do ivhg = 1, ncvhg
               If (StringComp(QHDefinition(Ivhg), TableName, CaseSensitive))  Then
                   QHDefinition(ivhg) = ''
                   Do ipoint=1,NrQHPoints
                      QH_Q(ipoint,ivhg) = Qhrelation_q(ipoint)
                      QH_h(ipoint,ivhg) = Qhrelation_h(ipoint)
                   Enddo
                   QHTableLength(ivhg)=NrQHPoints
!                  Write(*,*) ' ivhg QHTableLength', ivhg, NrQHPoints
               Endif
             Enddo
           endif
        Endif
        Call SKPCOM (Infile4, ENDFIL,'ODS')
     Enddo
 6111 Continue

    If (Err969) call ErrMsgStandard (972, 0, ' Qh-relation h values should be specified in increasing order.', &
                                     ' Some Qh-relation are not correctly specified in Paved.Tbl file')

! *********************************************************************
! Check of alle referenties naar QH-tabellen opgelost zijn
! *********************************************************************
    Err969 = .false.
    Do ivhg = 1, ncvhg
! Check of alle referenties naar tabellen opgelost
       if (RunoffCalcOption(ivhg) .gt. 0 .and. QHDefinition(ivhg) .ne. '')  then
           Err969 = .true.
           call ErrMsgStandard (969, 0, ' QHrelation definition not found in Paved.Tbl file.', QHDefinition(ivhg))
       endif
    Enddo

    If (Err969) call ErrMsgStandard (972, 0, ' Not enough unpaved data found', &
                                     ' Some Table Definitions not present in Paved.Tbl file')

! End March 2004 Taiwan: Qh-relations


! if one of the sewers (RWA and/or DWA) pumps to boundary, corresponding boundary VHGBND should be defined and positive.
! was: Q2VOW .eqv. .false.
  nrNodes = Network_get_nrNodes()
  Do teller =1, nrNodes
    if (EiNode(teller,3) == 1) then
      ivhg = EiNode(teller,2)
      name = Id_Nod(teller)
! ARS 12563: extended checking on case sewer(ivhg)%Q2Vow=0, either  boundary or NWRW node
      IF ( (sewer(ivhg)%Q2VOW(1) .eq. 0) .AND.  (VHGBND(ivhg) <= 0)) THEN
         IPluv = EiPluv(teller)
         IF (ipluv .gt. 0) then
            sewer(ivhg)%Q2VOW(1) = 3
         else
            call ErrMsgStandard (917, 0, 'Rdvhg Q2VOW-1 for node id:', name)
            Err917 =.true.
         Endif
      ENDIF
      IF ( (sewer(ivhg)%systemType .ge. 1) .and. (sewer(ivhg)%Q2VOW(2) .eq. 0) .AND.  &
          (VHGBND(ivhg) <= 0)) THEN
         IPluv = EiPluv(teller)
         IF (ipluv .gt. 0) then
            sewer(ivhg)%Q2VOW(2) = 3
         else
            call ErrMsgStandard (917, 0, 'Rdvhg Q2VOW-1 for node id:', name)
            Err917 =.true.
         Endif
      ENDIF

! for discharge to RWZI, EIRWZI should be defined
      IF ( (sewer(ivhg)%Q2VOW(1) .eq. 2) .AND.  (VHGRWZI(ivhg) <= 0)) THEN
         call ErrMsgStandard (917, 0, 'Rdvhg Q2VOW-1 for node id:', name)
         Err917 =.true.
      ENDIF
      IF ( (sewer(ivhg)%systemType .ge. 1) .and. (sewer(ivhg)%Q2VOW(2) .eq. 2) .AND.  &
          (VHGRWZI(ivhg) <= 0)) THEN
         call ErrMsgStandard (917, 0, 'Rdvhg Q2VOW-2 for node id:', name)
         Err917 =.true.
      ENDIF


! if no downstream open water, but only downstream boundary, all flows should go to boundary
! Houdt ook rekening met RWZI!!!!
      IOW = EiOW(teller)
      IBND = EiBnd(teller)
      IRWZI = EiRWZI(teller)
      IPluv = EiPluv(teller)

!     write(*,*) ' inod iow', inod, iow
      IF (IOW .GT. 0 .and. IRWZI .LE. 0 .and. Ibnd .le. 0) then
         sewer(ivhg)%Q2VOW(1) = 1
         sewer(ivhg)%Q2VOW(2) = 1
      ELSEIF (IOW .LE. 0 .and. IRWZI .LE. 0 .and. Ibnd .gt. 0) then
         sewer(ivhg)%Q2VOW(1) = 0
         sewer(ivhg)%Q2VOW(2) = 0
      ELSEIF (IRWZI .GT. 0 .and. IOW .LE. 0 .and. Ibnd .le. 0) then
         sewer(ivhg)%Q2VOW(1) = 2
         sewer(ivhg)%Q2VOW(2) = 2
      ELSEIF (IPluv .GT. 0) then
         sewer(ivhg)%Q2VOW(1) = 3
         sewer(ivhg)%Q2VOW(2) = 3
      ENDIF
    ENDif
  Enddo

  if (err917) call ErrMsgStandard (972, 0, ' Configuration/schematisation error:', &
                                     ' Paved node data and connections inconsistent ')

  Deallocate (TBLDEF)
  Deallocate (STODef)
  Deallocate (ReferenceToDefinition)
  Deallocate (AlreadyRead)
  return

  150 CONTINUE
  call SetMessage(LEVEL_FATAL, 'Read error in Paved ascii')

  return
  END SUBROUTINE Paved_readascii





  Subroutine ReadOpenDAPaved (Infile1, iout1)

  ! read Paved restart info from ASCII OpenDA file
  integer      Infile1, iout1, idebug


  Integer      RetVal

  Integer       inod
  Integer       ivhg, iecode
  Character(Len=1000) String
  Integer        Nhlp
  Parameter     (Nhlp = 32)
  Integer       IDUM(NHLP)
  Real          RDUM(NHLP)
  Character(Len=CharIdLength) CDUM(NHLP)
  Logical       allow, found, endfil

  ! file is already opened, rewind it
  Rewind(Infile1)
  iDebug = ConfFil_get_iDebug()
  retVal = 0

  Call SKPCOM (Infile1, ENDFIL,'ODS')
  call SetMessage(LEVEL_INFO, 'Read OpenDA paved data')
  do while (.not. endfil)
    ! read OpenDA record
     Read(Infile1,'(A1000)',END=21,ERR=150,IOSTAT=IECODE)  STRING
     ! skip regel als hij niet begint met juist keyword (PAVE)
     ! PAVE id 'paved' gwvolume 1234 onvzonevolume 4567  bergingland 1234 unpv
     If (STRING(1:4) .EQ. 'PAVE') then
      ! PAVE node id
        allow = .false.
        Retval = Retval + GetVAR2 (STRING,' id ',1,' PAVE-ReadAscii',' OpenDA file',IOUT1, &
                                   CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
        call fndNd2(inod, CDUM(1))
        if (inod .gt. 0) then      ! knoop bestaat in schematisatie
            ivhg = EiNode(inod,2)
            if (EiNode(inod,3) .eq. 1) then  ! en is verhard gebied
                ! get the data
                ! update the corresponding RR variables and related variables
                allow = .true.
                Retval = Retval + GetVAR2 (STRING,' streetvolume ',2, ' PAVE-ReadAscii',' OPENDA file',IOUT1, &
                                              CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
                if (found) then
                   write(*,*) ' found paved id and streetvolume ', Id_nod(inod)(1:len_trim(id_nod(inod))), rdum(1)
                   BVSTR(ivhg) = Rdum(1)
                   RSLMAP1_VHG(3,ivhg,1) = BVSTR(ivhg) / AreaVh(ivhg) * 1000.
                   VHG_Tnul(3,ivhg) = RSLMAP1_vhg(3,ivhg,1)
                endif
                allow = .true.
                Retval = Retval + GetVAR2 (STRING,' sewervolumeRWA ',2,' PAVE-ReadAscii',' OPENDA file',IOUT1, &
                                             CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
                if (found) then
                   write(*,*) ' found paved id and sewervolumeRWA ', Id_nod(inod)(1:len_trim(id_nod(inod))), rdum(1)
                   BVRL(ivhg,1) = Rdum(1)
                   RSLMAP1_VHG(1,ivhg,1) = BVRL (ivhg,1) / AreaVh(ivhg) * 1000.
                   VHG_Tnul(1,ivhg) = RSLMAP1_vhg(1,ivhg,1)
                endif
                allow = .true.
                Retval = Retval + GetVAR2 (STRING,' sewervolumeDWA ',2,' PAVE-ReadAscii',' OPENDA file',IOUT1, &
                                             CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
                if (found) then
                   write(*,*) ' found paved id and sewervolumeDWA ', Id_nod(inod)(1:len_trim(id_nod(inod))), rdum(1)
                   BVRL(ivhg,2) = Rdum(1)
                   RSLMAP1_VHG(2,ivhg,1) = BVRL (ivhg,2) / AreaVh(ivhg) * 1000.
                   VHG_Tnul(2,ivhg) = RSLMAP1_vhg(2,ivhg,1)
                endif
                allow = .true.
                Retval = Retval + GetVAR2 (STRING,' dynamicvolume ',2,' PAVE-ReadAscii',' OPENDA file',IOUT1, &
                                             CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
                if (found) then
                   write(*,*) ' found paved id and dynamicvolume ', Id_nod(inod)(1:len_trim(id_nod(inod))), rdum(1)
                   PavedVolDyn(ivhg) = Rdum(1)
                   RSLMAP1_VHG(17,ivhg,1) = PavedVolDyn(ivhg) / AreaVh(ivhg) * 1000.
                   VHG_Tnul(17,ivhg) = RSLMAP1_vhg(17,ivhg,1)
                endif
            endif
        endif
     Endif
  enddo
  goto 21

150 continue
    call SetMessage(LEVEL_ERROR, 'Error reading OpenDA restart record '//String(1:len_trim(string)))
21  continue

  End subroutine ReadOpenDAPaved


  Subroutine WriteOpenDAPaved (Infile1)

  ! write Paved restart info to ASCII OpenDA file
  integer      Infile1, idebug

  Integer       inode, ivhg
  Character(len=1) Quote

  ! file is already opened
  Rewind(infile1)
  iDebug = ConfFil_get_iDebug()
  quote = ''''

  do inode=1,ncnode
     ivhg = EiNode(inode,2)
     if (EiNode(inode,3) .eq. 1) then  ! en is verhard gebied
         write(Infile1,'(A,A1,A,A1,4(1X,A,G15.8),A)') 'PAVE id ',&
                                       quote,id_nod(inode)(1:len_trim(id_nod(inode))),quote,&
                                       ' streetvolume ', BVSTR(ivhg), &
                                       ' sewervolumeRWA ', BVRL(ivhg,1), &
                                       ' sewervolumeDWA ', BVRL(ivhg,2), &
                                       ' dynamicvolume ', PavedVolDyn(ivhg), ' pave'
     Endif
  enddo

  End subroutine WriteOpenDAPaved






  SUBROUTINE CMPVHG (IEVENT, ITMSTP, IVHG  , IMETEO, INODE, MessageInundation)
    ! *********************************************************************
    ! *** Last update:  March 1995
    ! *********************************************************************
    ! *********************************************************************
    ! *** Brief description:
    ! *** ------------------
    ! ***    This subroutine performs computations for verhard gebied.
    ! *********************************************************************
    ! *** Input/output parameters:
    ! *** ------------------------
    ! ***  IEVENT = event number
    ! ***  ITMSTP = timestep number
    ! ***  IDEBUG = file unit number of debug file
    ! ***  IOUT1  = file unit number of output file with run messages
    ! ***  IVHG   = intern verhard gebied nr
    ! ***  IMETEO = meteostation code
    ! ***  INODE  = knoopnr
    ! *********************************************************************
    ! *** Berekeningen voor verhard gebied
    ! *********************************************************************

     Implicit none

    ! variables
    Integer iEvent, iTmStp, iVhg,iMeteo, iNode, iOw, ibnd, ipluv, iRwzi, rowNr, ipoint, i
    Integer MessageInundation
    Real sewerCapacity(2)
    type (Date) currentDate
    type (Time) currentTime
    Integer iDebug, iOut1
    logical DateTimeOutsideTable
    Integer TabelNr

    Real TotUit, VNow, NetRain, Vinf

   ! Q-h relation ; SOBEK JIRA 22739 May 2011
    Real    ActualLevel, ResultingMaxQ, FlownOutDyn, BvrlOutFlow
    Integer LastInterpIndex

    Real    QHPeilArray(100), QHFlowArray(100)

    iDebug = ConfFil_get_iDebug()
    iOut1 = ConfFil_get_iOut1()

    IF (iDebug .ne.  0)  WRITE(IDEBUG,*) 'CMPVHG ivhg=',IVHG


    ! *********************************************************************
    ! *** Verdamping op straat; berging op straat
    ! *********************************************************************

    RV(IVHG) =  AAFNodeRainfall(inode) * RAIN(IMETEO) * AREAVH(IVHG) * timeSettings%timestepSize
    VV(IVHG) = 0.0
    IF (ConfArr_get_IHOUR() .GE. timeSettings%evaporationFromHr .AND. &
        ConfArr_get_IHOUR() .LT. timeSettings%evaporationToHr) THEN
 !Oct 1997: voeg crop factor open water toe voor verdamping vanaf straat
 !49802 April 2015: no check on initial storage street>0       IF (BVSTR0(IVHG) .GT. 0) &
        VV(IVHG) = EVAP (IMETEO) * CROPO * AREAVH(IVHG) * timeSettings%timestepSize * TMEVAP
    ENDIF
    BVSTR(IVHG) = BVSTR0(IVHG) - VV(IVHG) + RV (IVHG)

    ! *********************************************************************
    ! *** Check minimum/maximum berging op straat
    ! *********************************************************************

    IF (BVSTR(IVHG) .GT. sewer(ivhg)%BMAXST) THEN
       INV(IVHG,1) = (BVSTR(IVHG) - sewer(ivhg)%BMAXST)
       BVSTR(IVHG) = sewer(ivhg)%BMAXST
    ELSE
       INV(IVHG,1) = 0.0
       IF (BVSTR(IVHG) .LT. 0) THEN
          VV(IVHG) = VV(IVHG) + BVSTR(IVHG)
          BVSTR(IVHG) = 0.0
       ENDIF
    ENDIF

    ! *********************************************************************
    ! *** DWA inloop: is berekend in INIT2 in m3/s;
    ! *********************************************************************

    IF (iDebug .ne.  0)  WRITE(IDEBUG,*) 'CMPVHG ivhg DWA=',IVHG, DWAPaved(ivhg)

    IF (sewer(iVhg)%systemType .eq. 0) then
      ! Gemengd stelsel
      BVRL(IVHG,1) = BVRL0(IVHG,1) + INV(IVHG,1) + DWAPaved(IVHG)* timeSettings%timestepSize
    ELSE
      ! gescheiden of verbeterd gescheiden stelsel
      BVRL(IVHG,1) = BVRL0(IVHG,1) + INV(IVHG,1)
      BVRL(IVHG,2) = BVRL0(IVHG,2) + DWAPaved(IVHG)* timeSettings%timestepSize
    ENDIF
    IF (iDebug .ne.  0)  WRITE(IDEBUG,*) 'CMPVHG  na DWA: BVRL=',BVRL(IVHG,1), BVRL(IVHG,2)


    ! *********************************************************************
    ! *** Berging in riool; inflow=from surface + DWA;  determine outflow
    ! *********************************************************************

    Q2V(IVHG,1)  = 0.0
    Q2V(IVHG,2)  = 0.0
    IF (BVRL(IVHG,1) .GT. 0 .OR. BVRL(IVHG,2) .GT. 0) THEN
! oude methode
        currentDate%year = ConfArr_get_iYear()
        currentDate%day = ConfArr_get_iDay()
        currentDate%month = ConfArr_get_iMonth()
        currentTime%hour = ConfArr_get_iHour()
        currentTime%minute = ConfArr_get_iMinute()
        currentTime%second = 0.0
        ! nieuwe methode
        RowNr = -1
        TabelNr = VhgRefQC_TTable (iVhg)
        sewerCapacity(1) = getNewValue(TableHandle, TabelNr, 1, RowNr, CurrentDate, CurrentTime, &
                                        IDebug, Iout1, DateTimeOutsideTable, .true. )
        sewerCapacity(2) = getNewValue(TableHandle, TabelNr, 2, RowNr, CurrentDate, CurrentTime, &
                                        IDebug, Iout1, DateTimeOutsideTable, .true.)
        if (idebug .ne. 0)  write (idebug,*) 'new method sewer pump capacities', sewerCapacity(1), sewerCapacity(2)

        IF (BVRL(IVHG,1) .GT. 0) THEN
         ! index 1: RWA of gemengd riool
            Q2V(IVHG,1) = MIN ( BVRL(IVHG,1)/timeSettings%timestepSize, sewerCapacity(1))
            BVRL(IVHG,1) = BVRL(IVHG,1) - Q2V(IVHG,1) * timeSettings%timestepSize
            IF (BVRL(IVHG,1) .LT. -0.01) THEN
               call ErrMsgStandard(971, 0,  ' Error: negative sewerstorage', '')
            ENDIF
            BVRL(IVHG,1) = MAX (0.0, BVRL(IVHG,1))
        endif
        IF (BVRL(IVHG,2) .GT. 0) THEN
          ! index 2: DWA riool
          Q2V(IVHG,2) = MIN ( BVRL(IVHG,2)/timeSettings%timestepSize, sewerCapacity(2))
          BVRL(IVHG,2) = BVRL(IVHG,2) - Q2V(IVHG,2) * timeSettings%timestepSize
          IF (BVRL(IVHG,2) .LT. -0.01) THEN
             call ErrMsgStandard(971, 0,  ' Error: negative sewerstorage', '')
          ENDIF
          BVRL(IVHG,2) = MAX (0.0, BVRL(IVHG,2))
        ENDIF
     ENDIF

    ! *********************************************************************
    ! *** Overstort
    ! *********************************************************************

     Q1V(IVHG,1) = 0.0
     Q1V(IVHG,2) = 0.0
     INV(IVHG,2) = 0.0

! March 2004, Taiwan: add RunoffCalcOption for RWA/mixed sewer
      If (RunoffCalcOption(ivhg) .eq. 0) then
         if (idebug .ne. 0) write(idebug,*) 'RunoffCalcOption' , RunoffCalcOption(ivhg)
         ! old method, immediate discharge of excess water
         IF (BVRL(IVHG,1) .GT. sewer(iVhg)%BMAXRI(1) .and. sewer(iVhg)%systemType .ne. 2) THEN
         ! Gemengd stelsel of gescheiden stelsel: overstort van gemengd/RWA riool naar buiten
            Q1V(IVHG,1)  = (BVRL(IVHG,1)-sewer(iVhg)%BMAXRI(1)) / timeSettings%timestepSize
            BVRL(IVHG,1) = sewer(iVhg)%BMAXRI(1)
         ELSEIF (BVRL(IVHG,1) .GT. sewer(iVhg)%BMAXRI(1) .and. sewer(iVhg)%systemType .eq. 2) THEN
          ! Verbeterd gescheiden stelsel: overstort van gemengd/RWA riool eerst naar DWA, dan naar buiten.
           IF (BVRL(IVHG,2) .LT. sewer(iVhg)%BMAXRI(2)) THEN
             INV(IVHG,2)  = min (sewer(ivhg)%BMAXRI(2)-BVRL(IVHG,2), BVRL(IVHG,1)-sewer(iVhg)%BMAXRI(1) )
             BVRL(IVHG,1) =  BVRL(IVHG,1) - INV(IVHG,2)
             BVRL(IVHG,2) =  BVRL(IVHG,2) + INV(IVHG,2)
           ENDIF
           Q1V(IVHG,1)  = (BVRL(IVHG,1)-sewer(iVhg)%BMAXRI(1)) / timeSettings%timestepSize
           BVRL(IVHG,1) = sewer(iVhg)%BMAXRI(1)
         ENDIF
         !Check op overstort DWA riool
         IF (BVRL(IVHG,2) .GT. sewer(iVhg)%BMAXRI(2) .and. sewer(iVhg)%systemType .ge. 1) THEN
           Q1V(IVHG,2)  = (BVRL(IVHG,2)-sewer(iVhg)%BMAXRI(2)) / timeSettings%timestepSize
           BVRL(IVHG,2) = sewer(iVhg)%BMAXRI(2)
         ENDIF
      ElseIf (RunoffCalcOption(ivhg) .eq. 1) then
         if (idebug .ne. 0) write(idebug,*) 'RunoffCalcOption' ,RunoffCalcOption(ivhg), RunoffFactor(ivhg)
          ! Runoff factor - delay NWRW method; infiltration=0
         TOTUIT = 0.0
         VNOW   = PavedVolDyn0(ivhg)

         IF ((Vnow .gt. 0 .or. BVRL(IVHG,1) .GT. sewer(iVhg)%BMAXRI(1)) .and. sewer(iVhg)%systemType .ne. 2) THEN
         ! Gemengd stelsel of gescheiden stelsel: overstort van gemengd/RWA riool naar buiten
! ARS 16398 Feb 2007
            NetRain= max(0.0, BVRL(IVHG,1)-sewer(iVhg)%BMAXRI(1))
            Call RunoffFactorFormulation (VNow, NetRain, RunoffFactor(ivhg), 0., &
                                          Idebug, TimeSettings%TimestepSize, TotUit, Vinf, .true. )
            Q1V(ivhg,1) = Totuit / timeSettings%TimestepSize
            if (NetRain .gt. 0) then
               BVRL(IVHG,1) = sewer(iVhg)%BMAXRI(1)
            endif
! end
            PavedVolDyn (ivhg) = Vnow
         ELSEIF ((Vnow .gt. 0 .or. BVRL(IVHG,1) .GT. sewer(iVhg)%BMAXRI(1)) .and. sewer(iVhg)%systemType .eq. 2) THEN
          ! Verbeterd gescheiden stelsel: overstort van gemengd/RWA riool eerst naar DWA, dan naar buiten.
           IF (BVRL(IVHG,2) .LT. sewer(iVhg)%BMAXRI(2)) THEN
              INV(IVHG,2)  = min (sewer(ivhg)%BMAXRI(2)-BVRL(IVHG,2),BVRL(IVHG,1)-sewer(iVhg)%BMAXRI(1) )
              BVRL(IVHG,1) =  BVRL(IVHG,1) - INV(IVHG,2)
              BVRL(IVHG,2) =  BVRL(IVHG,2) + INV(IVHG,2)
           ENDIF
! ARS 16398 Feb 2007
           NetRain= max(0.0, (BVRL(IVHG,1)-sewer(iVhg)%BMAXRI(1)))
           Call RunoffFactorFormulation (VNow, NetRain, RunoffFactor(ivhg), 0., &
                                         Idebug, TimeSettings%TimestepSize, TotUit, Vinf, .true. )
           Q1V(ivhg,1) = Totuit / timeSettings%TimestepSize
           if (NetRain .gt. 0) then
              BVRL(IVHG,1) = sewer(iVhg)%BMAXRI(1)
           endif
! end
           PavedVolDyn (ivhg) = Vnow
         ENDIF
         !Check op overstort DWA riool; no runoff formulation for DWA, since overflow may not occur
         IF (BVRL(IVHG,2) .GT. sewer(iVhg)%BMAXRI(2) .and. sewer(iVhg)%systemType .ge. 1) THEN
            Q1V(IVHG,2)  = (BVRL(IVHG,2)-sewer(iVhg)%BMAXRI(2)) / timeSettings%timestepSize
            BVRL(IVHG,2) = sewer(iVhg)%BMAXRI(2)
         ENDIF

      ElseIf (RunoffCalcOption(ivhg) .eq. 2) then
         if (idebug .ne. 0) then
            write(idebug,*) 'RunoffCalcOption' , RunoffCalcOption(ivhg)
            write(idebug,*) 'Qh_Q(ipoint,ivhg),QH_h(ipoint,ivhg), ivhg and length', ivhg, QHTableLength(ivhg)
            Do ipoint=1,QHTableLength(ivhg)
              write(idebug,*) ipoint,Qh_Q(ipoint,ivhg),QH_h(ipoint,ivhg)
            Enddo
         Endif
         ! Q-h relation ; SOBEK JIRA 22739 May 2011
          LastInterpIndex = 1
          Actuallevel = -999.99
          IOW = EIOW(INODE)
          IBND = EIBND(INODE)
          IPluv = EIPluv(INODE)
          IRWZI = EIRWZI(INODE)
          if (ipluv .gt. 0) Actuallevel = -999.99
          if (iow .gt. 0)   Actuallevel = LvlOw0(iow)
          if (ibnd .gt. 0)  Actuallevel = BndPar(ibnd,1)
          Do i=1,QhTableLength(ivhg)
             QHPeilArray(i) = QH_h(i,ivhg)
             QHFlowArray(i) = QH_q(i,ivhg)
          Enddo
          CALL RR_INTERP (QhTableLength(ivhg), QhPeilArray, QHFlowArray, ActualLevel,ResultingMaxQ, LastInterpIndex)
          if (idebug .ne. 0) write(idebug,*) 'RunoffCalcOption' ,RunoffCalcOption(ivhg), RunoffFactor(ivhg)
          if (idebug .ne. 0) write(idebug,*) 'Qh relation level and maxflow ',ActualLevel, ResultingMaxQ
          TOTUIT = 0.0
          VNOW   = PavedVolDyn0(ivhg)
          IF ((Vnow .gt. 0 .or. BVRL(IVHG,1) .GT. sewer(iVhg)%BMAXRI(1)) .and. sewer(iVhg)%systemType .ne. 2) THEN
         ! Gemengd stelsel of gescheiden stelsel: overstort van gemengd/RWA riool naar buiten, beperkt door ResultingMaxQ
             ! zie hieronder in gemeenschappelijk stuk
          ELSEIF ((Vnow .gt. 0 .or. BVRL(IVHG,1) .GT. sewer(iVhg)%BMAXRI(1)) .and. sewer(iVhg)%systemType .eq. 2) THEN
          ! Verbeterd gescheiden stelsel: overstort van gemengd/RWA riool eerst naar DWA, dan naar buiten.
            IF (BVRL(IVHG,2) .LT. sewer(iVhg)%BMAXRI(2)) THEN
               INV(IVHG,2)  = min (sewer(ivhg)%BMAXRI(2)-BVRL(IVHG,2),BVRL(IVHG,1)-sewer(iVhg)%BMAXRI(1) )
               BVRL(IVHG,1) =  BVRL(IVHG,1) - INV(IVHG,2)
               BVRL(IVHG,2) =  BVRL(IVHG,2) + INV(IVHG,2)
            ENDIF
            ! rest in gemeenschappelijk stuk
          ENDIF
         !Check op overstort DWA riool; no runoff formulation for DWA, since overflow may not occur
          IF (BVRL(IVHG,2) .GT. sewer(iVhg)%BMAXRI(2) .and. sewer(iVhg)%systemType .ge. 1) THEN
             Q1V(IVHG,2)  = (BVRL(IVHG,2)-sewer(iVhg)%BMAXRI(2)) / timeSettings%timestepSize
             BVRL(IVHG,2) = sewer(iVhg)%BMAXRI(2)
          ENDIF
! altijd voor sewer system 1 / gemengd of RWA riool
          ! gewenste overstort
          TotUit= Vnow + max(0.0, BVRL(IVHG,1)-sewer(iVhg)%BMAXRI(1))
          ! max. overstort
          TotUit= min(ResultingMaxQ, TotUit/timeSettings%TimestepSize)
          Q1V(ivhg,1) = Totuit
          ! overstort eerst uit dynamische berging
          PavedVolDyn (ivhg) = PavedVolDyn0(ivhg) - Totuit * TimeSettings%Timestepsize
          FlownOutDyn = TotUit
          ! maar niet meer dan mogelijk
          if (PavedVolDyn (ivhg) .le. 0) then
             FlownOutDyn = PavedVolDyn0(ivhg) / TimeSettings%TimestepSize
             PavedVolDyn(ivhg) = 0.0
          endif
          ! uit riool:
          BvrlOutflow = Totuit - FlownOutDyn
          BVRL(IVHG,1) = BVRL(ivhg,1) - BvrlOutflow * Timesettings%TimestepSize
          if (BVRL(IVHG,1) .gt. sewer(iVhg)%BMAXRI(1)) then
              PavedVolDyn(ivhg) = PavedVolDyn0(ivhg) - FlownOutDyn*TimeSettings%TimestepSize + BVRL(IVHG,1) - sewer(iVhg)%BMAXRI(1)
              BVRL(IVHG,1) = sewer(iVhg)%BMAXRI(1)
          elseif (BVRL(IVHG,1) .le. 0.) then
              Q1V(ivhg,1)  = Q1V(ivhg,1) + ( BVRL(ivhg,1)/ TimeSettings%TimestepSize)
              BvrlOutflow  = BvrlOutflow + ( BVRL(ivhg,1)/ TimeSettings%TimestepSize)
              Totuit       = Totuit      + ( BVRL(ivhg,1)/ TimeSettings%TimestepSize)
              BVRL(ivhg,1) = 0.0
          endif
          ! for Qh-relation option:
          ! June 2011: make sure if total Bvrl+dynamic storage > max. sewer, that Bvrl is full
          if (BVRL(ivhg,1) .lt. sewer(ivhg)%BMaxRi(1) .and. BVRL(ivhg,1)+PavedVolDyn(ivhg) .gt. sewer(ivhg)%BMaxRi(1)) then
              PavedVolDyn(ivhg) = PavedVolDyn(ivhg) + BVRL(ivhg,1) - sewer(iVhg)%BMAXRI(1)
              BVRL(IVHG,1) = sewer(iVhg)%BMAXRI(1)
          endif
          IF (iDebug .ne.  0) THEN
             WRITE(IDEBUG,*) ' after Qh relation test '
             WRITE(IDEBUG,*) ' Vnow ', Vnow
             WRITE(IDEBUG,*) ' Totuit, FlownOutDyn, BvrlOutflow', Totuit, FlownOutDyn, BvrlOutflow
             WRITE(IDEBUG,*) ' PavedVolDyn0 and VolDyn ',PavedVolDyn0(ivhg), PavedVolDyn(ivhg)
             WRITE(IDEBUG,*) ' BVRL and BVRL0 - mixed/RWA ',BVRL(ivhg,1), BVRL0(ivhg,1)
             WRITE(IDEBUG,*) ' BVRL and BVRL0 - DWA       ',BVRL(ivhg,2), BVRL0(ivhg,2)
          ENDIF

      Endif

    ! *********************************************************************
    ! *** Inundatie
    ! *********************************************************************

    IOW = EIOW(INODE)
    IBND = EIBND(INODE)
    IPluv = EIPluv(INODE)
    IRWZI = EIRWZI(INODE)
    IF (iDebug .ne.  0) THEN
       WRITE(IDEBUG,*) ' IVHG  INODE IOW  IBND ipluv IRWZI '
       WRITE(IDEBUG,'(6I7)')  IVHG,  INODE, IOW,  IBND, ipluv, IRWZI
       WRITE(IDEBUG,*)  ' Q2VOW ',sewer(iVhg)%Q2VOW(1), sewer(iVhg)%Q2VOW(2)
       WRITE(IDEBUG,*)  ' BMAXRI',sewer(iVhg)%BMAXRI(1), sewer(iVhg)%BMAXRI(2)
       WRITE(IDEBUG,*)  ' Sewercap',sewercapacity(1), sewercapacity(2)
    ENDIF

    IF (IOW .GT. 0) THEN
!      inundation
       IF (LVLOW0(IOW) .GT. LVLVH(IVHG)) THEN
         call ConfArr_set_fldVhg(.TRUE.)
         if (iOut1 .ne. 0 .and. MessageInundation .gt. 0) then
           WRITE(IOUT1, '(A,A,A,2I5)') ' Inundation paved area (from open water)', &
                 Id_Nod(INODE),' in event/timestep', IEVENT, ITMSTP
         endif
       ENDIF
!      check sewer overflow levels
       Call CheckSewerOverflowOW (Ivhg, iow, 1, messageinundation)   ! 1= RWF index
       If (sewer(ivhg)%Systemtype .ne. 0) Call CheckSewerOverflowOW (Ivhg, iow, 2, messageinundation) ! 2= DWF index

    ELSEIF (IBND .GT. 0) THEN
       IF (BNDPAR(IBND,1) .GT. LVLVH(IVHG)) THEN
!        inundation
         call ConfArr_set_fldVhg(.TRUE.)
         if (iOut1 .ne. 0 .and. MessageInundation .gt. 0) then
           WRITE(IOUT1, '(A,A,A,2I5)') ' Inundation paved area (from boundary)', &
                 Id_Nod(INODE),' in event/timestep', IEVENT, ITMSTP
         endif
       ENDIF
       Call CheckSewerOverflowBnd (Ivhg, ibnd, 1, messageinundation)   ! 1= RWF index
       If (sewer(ivhg)%Systemtype .ne. 0) Call CheckSewerOverflowBnd(Ivhg, ibnd, 2, messageinundation) ! 2= DWF index

    ELSEIF (IPluv .GT. 0) THEN
        ! no limitation of discharge to NWRW node
    ELSE
       Write(*,*) ' Internal error Sobek-3B: CMPVHG: IOW=IBND=0'
       call ErrMsgStandard (981, 0, 'Paved node should be connected to open water/boundary/NWRW runoff node for spilling ', Id_Nod(inode))
    ENDIF

    ! *********************************************************************
    ! *** Totaal naar open water of naar boundary, of naar RWZI
    ! Q1 (ivhg,*) = overstort
    !               * = 1,2, voor gescheiden systemen
    ! Q2 (ivhg,*) = rioolgemaal
    !               *=1,2, voor gescheiden systemen
    ! *********************************************************************

    IF (iDebug .ne.  0) WRITE(IDEBUG,'(A,5I7)') ' Paved area TOEWIJZING SPIL EN PUMPED FLOW', &
                                                 IVHG, iow, ibnd, ipluv, irwzi
    IF (IOW .GT. 0) THEN
       QINOW(IOW,1) = QINOW(IOW,1) + Q1V(IVHG,1) + Q1V(IVHG,2)
        ! gemengd of RWA riool
       If (sewer(iVhg)%Q2VOW(1) .eq. 1) then
          QINOW(IOW,1) = QINOW(IOW,1) + Q2V(IVHG,1)
       Elseif (sewer(iVhg)%Q2VOW(1) .eq. 0) then
    ! outflow naar welke boundary?
          IBND = VHGBND(IVHG)
          QBND (IBND)  = QBND(IBND) +  Q2V(IVHG,1)
          QINBND(IBND)%totalPaved = QINBND(IBND)%totalPaved + Q2V(IVHG,1)
       Elseif (sewer(iVhg)%Q2VOW(1) .eq. 2) then
    ! outflow naar welke RWZI?
          IRWZI = VHGRWZI(IVHG)
          QRWZI (IRWZI)  = QRWZI(IRWZI) +  Q2V(IVHG,1)
       Elseif (sewer(iVhg)%Q2VOW(1) .eq. 3) then
    ! outflow naar NWRW node
          IPluv = EIPluv(INODE)
          QInPluv(iPluv)  = QinPluv(ipluv) + Q2V(IVHG,1)
          QPluv(Ipluv)%totalPaved = QPluv(IPluv)%totalPaved + Q2V(IVHG,1)
       else
          call SetMessage(LEVEL_FATAL, 'Unknown value of Q2vow(1)')
       ENDIF
       ! DWA riool; only in case of seperated system!
       IF (sewer(iVhg)%systemType .ne. 0) THEN
           IF (sewer(iVhg)%Q2VOW(2) .eq. 1) THEN
              QINOW(IOW,1) = QINOW(IOW,1) + Q2V(IVHG,2)
           ELSEif (sewer(iVhg)%Q2VOW(2) .eq. 0) then
            !      outflow naar welke boundary?
             IBND = VHGBND(IVHG)
             QBND (IBND)  = QBND(IBND) +  Q2V(IVHG,2)
             QINBND(IBND)%totalPaved = QINBND(IBND)%totalPaved + Q2V(IVHG,2)
           Elseif (sewer(iVhg)%Q2VOW(2) .eq. 2) then
           ! outflow naar welke RWZI?
              IRWZI = VHGRWZI(IVHG)
              QRWZI (IRWZI)  = QRWZI(IRWZI) +  Q2V(IVHG,2)
           ELSEif (sewer(iVhg)%Q2VOW(2) .eq. 3) then
            !      outflow naar NWRW node
              QInPluv(iPluv)  = QinPluv(IPluv) +  Q2V(IVHG,2)
              QPluv(IPluv)%totalPaved = QPluv(IPluv)%totalPaved + Q2V(IVHG,2)
           ENDIF
       ENDIF
       IF (iDebug .ne.  0) THEN
         WRITE(IDEBUG,*) ' IVHG  INODE IOW  IBND  IPluv IRWZI '
         WRITE(IDEBUG,'(6I7)')  IVHG,  INODE, IOW,  IBND, Ipluv, iRwzi
         WRITE(IDEBUG,*) ' QINOW(IOW,1)   QBND(IBND)/QRWZI   QINBND_totalpaved'
         IF (IRWZI .GT. 0) THEN
            WRITE(IDEBUG,*) QINOW(IOW,1), ' Rwzi', QRWZI(IRWZI)
         ENDIF
         IF (IBND .GT. 0) THEN
            WRITE(IDEBUG,*) QINOW(IOW,1), QBND(IBND), QINBND(IBND)%totalPaved
         Elseif (Ipluv .GT. 0) THEN
            WRITE(IDEBUG,*) QINOW(IOW,1), QinPluv(IPluv) ,QPluv(IPluv)%totalPaved
         ELSE
            WRITE(IDEBUG,*) QINOW(IOW,1)
         ENDIF
       ENDIF
    ELSEIF (IBND .GT. 0 .and. IRWZI .le. 0) THEN
! all flows to boundary regardless of specified sewer-to-openwater/boundary;
! no downstream open water, only downstream boundary
       QINBND(IBND)%totalPaved = QINBND(IBND)%totalPaved &
              + Q1V(IVHG,1) + Q1V(IVHG,2) + Q2V(IVHG,1) + Q2V(IVHG,2)
       QBND(IBND) = QBND(IBND) + Q1V(IVHG,1) + Q1V(IVHG,2) + Q2V(IVHG,1) + Q2V(IVHG,2)
       IF (iDebug .ne.  0) THEN
         WRITE(IDEBUG,*) ' IVHG  INODE  IBND  QINBND(IBND)_totalPaved QBND(IBND)'
         WRITE(IDEBUG,*) IVHG, INODE, IBND, QINBND(IBND)%totalPaved, QBND(IBND)
       ENDIF
    ELSEIF (IBND .GT. 0 .and. IRWZI .gt. 0) THEN
! all flows to boundary or RWZI; spilling to boundary
       QINBND(IBND)%totalPaved = QINBND(IBND)%totalPaved + Q1V(IVHG,1) + Q1V(IVHG,2)
       QBND(IBND) = QBND(IBND) + Q1V(IVHG,1) + Q1V(IVHG,2)

       if (sewer(iVhg)%Q2VOW(1) .eq. 2) then
         ! outflow naar welke RWZI?
         IRWZI = VHGRWZI(IVHG)
         QRWZI (IRWZI)  = QRWZI(IRWZI) +  Q2V(IVHG,1)
       else
         ! to welke boundary?
         QINBND(IBND)%totalPaved = QINBND(IBND)%totalPaved + Q2V(IVHG,1)
         QBND(IBND) = QBND(IBND) + Q2V(IVHG,1)
       endif

       if (sewer(iVhg)%Q2VOW(2) .eq. 2) then
         ! outflow naar welke RWZI?
         IRWZI = VHGRWZI(IVHG)
         QRWZI (IRWZI)  = QRWZI(IRWZI) +  Q2V(IVHG,2)
       else
        ! to welke boundary?
         QINBND(IBND)%totalPaved = QINBND(IBND)%totalPaved + Q2V(IVHG,2)
         QBND(IBND) = QBND(IBND) + Q2V(IVHG,2)
       endif

       IF (iDebug .ne.  0) THEN
         write(idebug,*) ' ivhg     iow     ibnd     irwzi  '
         write(idebug,*)   ivhg, iow, ibnd, irwzi
         WRITE(IDEBUG,*) ' QINBND(IBND)_totalPaved QBND(IBND)'
         WRITE(IDEBUG,*)  QINBND(IBND)%totalPaved, QBND(IBND)
         WRITE(IDEBUG,*) ' IRWZI  QRWZI(IRWZI)'
         WRITE(IDEBUG,*) IRWZI, QRWZI(IRWZI)
         WRITE(IDEBUG,*) ' Q2V(1-2)', Q2V(IVHG,1), Q2V(IVHG,2)
       ENDIF
    ELSEIF (IPluv .GT. 0 .and. irwzi .ne. 0 ) THEN
!    pump to rwzi, spill to NWRW node
       QPluv(IPluv)%totalPaved = QPluv(IPluv)%totalPaved &
               + Q1V(IVHG,1) + Q1V(IVHG,2)
       QinPluv(IPluv) = QinPluv(IPluv) + Q1V(IVHG,1) + Q1V(IVHG,2)
       ! pumped flow to RWZI
       if (sewer(iVhg)%Q2VOW(1) .eq. 2) then
         ! outflow naar welke RWZI?
         IRWZI = VHGRWZI(IVHG)
         QRWZI (IRWZI)  = QRWZI(IRWZI) +  Q2V(IVHG,1)
       else  ! also to NWRW
          QinPluv(IPluv) = QinPluv(IPluv) + Q2V(IVHG,1)
          QPluv(IPluv)%totalPaved = QPluv(IPluv)%totalPaved + Q2V(IVHG,1)
       endif
       if (sewer(iVhg)%Q2VOW(2) .eq. 2) then
         ! outflow naar welke RWZI?
         IRWZI = VHGRWZI(IVHG)
         QRWZI (IRWZI)  = QRWZI(IRWZI) +  Q2V(IVHG,2)
       else  ! also to NWRW
          QinPluv(IPluv) = QinPluv(IPluv) + Q2V(IVHG,2)
          QPluv(IPluv)%totalPaved = QPluv(IPluv)%totalPaved + Q2V(IVHG,2)
       endif
       IF (iDebug .ne.  0) THEN
         WRITE(IDEBUG,*) ' IVHG  INODE  IPluv QPluv(IPluv)_totalPaved QBND(IBND)'
         WRITE(IDEBUG,*) IVHG, INODE, IPluv, QPluv(IPluv)%totalPaved, QinPluv(IPluv)
       ENDIF
    ELSEIF (IPluv .GT. 0 .and. iow .eq. 0 .and. ibnd .eq. 0 .and. irwzi .eq. 0 ) THEN
!    all flows to NWRW node
       QPluv(IPluv)%totalPaved = QPluv(IPluv)%totalPaved &
               + Q1V(IVHG,1) + Q1V(IVHG,2) + Q2V(IVHG,1) + Q2V(IVHG,2)
       QinPluv(IPluv) = QinPluv(IPluv) + Q1V(IVHG,1) + Q1V(IVHG,2) + Q2V(IVHG,1) + Q2V(IVHG,2)
       IF (iDebug .ne.  0) THEN
         WRITE(IDEBUG,*) ' IVHG  INODE  IPluv QPluv(IPluv)_totalPaved QBND(IBND)'
         WRITE(IDEBUG,*) IVHG, INODE, IPluv, QPluv(IPluv)%totalPaved, QinPluv(IPluv)
       ENDIF
    ELSE
       Write(*,*) ' Internal error Sobek-3B: CMPVHG: IOW=IBND=0'
       call ErrMsgStandard (981, 0, 'Paved node should be connected to open water/boundary/NWRW runoff node for spilling ', Id_Nod(inode))
    ENDIF

    ! *********************************************************************
    ! *** debug
    ! *********************************************************************

    IF (iDebug .ne.  0) THEN
       WRITE(IDEBUG,*) ' Paved area', Id_Nod(INODE)
       WRITE(IDEBUG,*) ' Timestep nr                :',ITMSTP
       WRITE(IDEBUG,*) ' Timestepsize in s          :',timeSettings%timestepSize
       WRITE(IDEBUG,*) ' Surface level m            :',LVLVH (IVHG)
       WRITE(IDEBUG,*) ' Total surface in m2        :',AREAVH(IVHG)
       WRITE(IDEBUG,*) ' Max. cap. RWA pump in m3/s :',Q2VMAX(IVHG,1)
       WRITE(IDEBUG,*) ' Max. cap. DWA pump in m3/s :',Q2VMAX(IVHG,2)
       WRITE(IDEBUG,*) ' Initial storage street m3  :',BVSTR0(IVHG)
       WRITE(IDEBUG,*) ' Initial stor. RWA sewer m3 :',BVRL0 (IVHG,1)
       WRITE(IDEBUG,*) ' Initial stor. DWA sewer m3 :',BVRL0 (IVHG,2)
       WRITE(IDEBUG,*) ' Final   storage street m3  :',BVSTR (IVHG)
       WRITE(IDEBUG,*) ' Final storage RWA sewer m3 :',BVRL  (IVHG,1)
       WRITE(IDEBUG,*) ' Final storage DWA sewer m3 :',BVRL  (IVHG,2)
       WRITE(IDEBUG,*) ' Voldyn initial             :',PavedVolDyn0(IVHG)
       WRITE(IDEBUG,*) ' Voldyn final               :',PavedVolDyn (IVHG)
       WRITE(IDEBUG,*) ' Evaporation street in m3   :',VV    (IVHG)
       WRITE(IDEBUG,*) ' Precipitation in m3        :',RV    (IVHG)
       WRITE(IDEBUG,*) ' Surface inflow to sewer(m3):',INV   (IVHG,1)
       WRITE(IDEBUG,*) ' DWA-Inflow sewer in m3/s   :',DWAPaved(IVHG)
       WRITE(IDEBUG,*) ' DWA-Inflow sewer in m3     :',DWAPaved(IVHG) * timeSettings%timestepSize
       WRITE(IDEBUG,*) ' Pumped Q RWA sewer in m3/s :',Q2V   (IVHG,1)
       WRITE(IDEBUG,*) ' Overflow RWA to DWA in m3  :',INV   (IVHG,2)
       WRITE(IDEBUG,*) ' Overflow RWA sewer in m3/s :',Q1V   (IVHG,1)
       WRITE(IDEBUG,*) ' Pumped Q DWA sewer in m3/s :',Q2V   (IVHG,2)
       WRITE(IDEBUG,*) ' Overflow DWA sewer in m3/s :',Q1V   (IVHG,2)
    ENDIF

    RETURN
  END subroutine cmpvhg


  SUBROUTINE CMPVHG2016 (IEVENT, ITMSTP, IVHG  , IMETEO, INODE, MessageInundation)
    ! *********************************************************************
    ! *** Last update:  March 1995
    ! *********************************************************************
    ! *********************************************************************
    ! *** Brief description:
    ! *** ------------------
    ! ***    This subroutine performs computations for verhard gebied.
    ! *********************************************************************
    ! *** Input/output parameters:
    ! *** ------------------------
    ! ***  IEVENT = event number
    ! ***  ITMSTP = timestep number
    ! ***  IDEBUG = file unit number of debug file
    ! ***  IOUT1  = file unit number of output file with run messages
    ! ***  IVHG   = intern verhard gebied nr
    ! ***  IMETEO = meteostation code
    ! ***  INODE  = knoopnr
    ! *********************************************************************
    ! *** Berekeningen voor verhard gebied
    ! *********************************************************************

     Implicit none

    ! variables
    Integer iEvent, iTmStp, iVhg,iMeteo, iNode, iOw, ibnd, ipluv, iRwzi, rowNr, ipoint, i
    Integer MessageInundation
    Real sewerCapacity(2)
    type (Date) currentDate
    type (Time) currentTime
    Integer iDebug, iOut1
    logical DateTimeOutsideTable
    Integer TabelNr

    Real QswfMax, Qswf, EvapMax, QSpillMax
    Real TotUit, VNow, NetRain, Vinf

   ! Q-h relation ; SOBEK JIRA 22739 May 2011
    Real    ActualLevel, ResultingMaxQ, BvrlOutFlow, TotalStorage
    Integer LastInterpIndex

    Real    QHPeilArray(100), QHFlowArray(100)

    iDebug = ConfFil_get_iDebug()
    iOut1 = ConfFil_get_iOut1()

    IF (iDebug .ne.  0)  WRITE(IDEBUG,*) 'CMPVHG2016 ivhg=',IVHG


    ! *********************************************************************
    ! *** Rainfall and evaporation from surface; all in m3
    ! *********************************************************************

    RV(IVHG) =  AAFNodeRainfall(inode) * RAIN(IMETEO) * AREAVH(IVHG) * timeSettings%timestepSize
    VV(IVHG) = 0.0
    IF (ConfArr_get_IHOUR() .GE. timeSettings%evaporationFromHr .AND. &
        ConfArr_get_IHOUR() .LT. timeSettings%evaporationToHr) THEN
        VV(IVHG) = EVAP (IMETEO) * CROPO * AREAVH(IVHG) * timeSettings%timestepSize * TMEVAP
    ENDIF
    EvapMax = VV(IVHG)

    ! *********************************************************************
    ! *** maximum discharge from surface to sewer Qswfmax
    ! *********************************************************************

    currentDate%year = ConfArr_get_iYear()
    currentDate%day = ConfArr_get_iDay()
    currentDate%month = ConfArr_get_iMonth()
    currentTime%hour = ConfArr_get_iHour()
    currentTime%minute = ConfArr_get_iMinute()
    currentTime%second = 0.0
    RowNr = -1
    TabelNr = VhgRefQC_TTable (iVhg)
    sewerCapacity(1) = getNewValue(TableHandle, TabelNr, 1, RowNr, CurrentDate, CurrentTime, &
                                    IDebug, Iout1, DateTimeOutsideTable, .true. )
    sewerCapacity(2) = getNewValue(TableHandle, TabelNr, 2, RowNr, CurrentDate, CurrentTime, &
                                    IDebug, Iout1, DateTimeOutsideTable, .true.)
    if (idebug .ne. 0)  write (idebug,*) 'new method sewer pump capacities', sewerCapacity(1), sewerCapacity(2)

    ! QSwfMax = sewer pump capacity + spill capacity + free storage - DWA (combined system)
    QSwfmax = sewerCapacity(1) * TimeSettings%TimestepSize
    ! estimate spill capacity
    If (RunoffCalcOption(ivhg) .le. 1) then
       QSpillMax = 99999.   ! m3/s
    ElseIf (RunoffCalcOption(ivhg) .ge. 2) then
       ! Q-h relation
        LastInterpIndex = 1
        Actuallevel = -999.99
        IOW = EIOW(INODE)
        IBND = EIBND(INODE)
        IPluv = EIPluv(INODE)
        IRWZI = EIRWZI(INODE)
        if (ipluv .gt. 0) Actuallevel = -999.99
        if (iow .gt. 0)   Actuallevel = LvlOw0(iow)
        if (ibnd .gt. 0)  Actuallevel = BndPar(ibnd,1)
        Do i=1,QhTableLength(ivhg)
           QHPeilArray(i) = QH_h(i,ivhg)
           QHFlowArray(i) = QH_q(i,ivhg)
        Enddo
        CALL RR_INTERP (QhTableLength(ivhg), QhPeilArray, QHFlowArray, ActualLevel,QSpillMax, LastInterpIndex)
        IF (iDebug .ne.  0)  WRITE(IDEBUG,*) 'QspillMax =', QSpillMax
    endif

    QSwfmax = QSwfMax + QSpillMax * timeSettings%timestepSize
    IF (iDebug .ne.  0)  WRITE(IDEBUG,*) 'QspillMax =', QSpillMax
    ! plus free storage minus DWF
    QSwfmax = QSwfMax + (sewer(iVhg)%BMAXRI(1) - BVRL0(ivhg,1))
    IF (iDebug .ne.  0)  WRITE(IDEBUG,*) 'discharge cap + free space ',QSwfMax
    IF (sewer(iVhg)%systemType .eq. 0) QSwfMax = QSwfMax - DWAPaved(IVHG)* timeSettings%timestepSize

    IF (iDebug .ne.  0)  WRITE(IDEBUG,*) 'discharge cap + free space - DWF'
    IF (iDebug .ne.  0)  WRITE(IDEBUG,*) 'QSwfmax =', QSwfMax

    ! possible limiation by rational method surface runoff!
    if (RunoffCalcOption(ivhg) .eq. 1 .or. RunoffCalcOption(ivhg) .eq. 3) then
       ! rational method to compute inflow from dynamic storage into sewer
         TOTUIT = 0.0
         VNOW   = PavedVolDyn0(ivhg)
         NetRain= max(0.0, RV(ivhg) - EvapMax)
         if (Vnow + NetRain .gt. 0.) then
            Call RunoffFactorFormulation (VNow, NetRain, RunoffFactor(ivhg), 0., Idebug, TimeSettings%TimestepSize, TotUit, Vinf, .true. )
         endif
         QSWFMax = min (TotUit, QSwfMax)
         IF (iDebug .ne.  0)  WRITE(IDEBUG,*) 'QSwfmax after limitation rational method surface runoff=', QSwfMax
    endif

    ! *********************************************************************
    ! *** surface balance
    ! *********************************************************************

    if (EvapMax + QswfMax .lt. RV(ivhg) + BVSTR0(IVHG) + PavedVolDyn0(ivhg) ) then
     ! evap plus outflow capacity is less than actual storage plus rainfall
     ! so Evap could be at potential rate, outflow to sewer at potential rate; fill storage on street and possibly dynamic storage
     ! however take into account that QSwf only from dynamic storage
       IF (iDebug .ne.  0)  WRITE(IDEBUG,*) 'CMPVHG2016 case 1'
       IF (iDebug .ne.  0)  WRITE(IDEBUG,*) 'BMaxSt =', Sewer(ivhg)%BMaxSt
       VV(ivhg) = EvapMax
       ! check storages to find actual Qswf
       BVSTR(ivhg) = BVSTR0(ivhg) + RV(ivhg) - VV(ivhg)
       if (BVSTR(ivhg) .le. 0.0) then
          PavedVolDyn(ivhg) = PavedVolDyn0(ivhg) + BVSTR(ivhg)
          BVSTR(ivhg) = 0.0
       elseif (BVSTR(ivhg) .gt. sewer(ivhg)%BMaxSt) then
          PavedVolDyn(ivhg) = PavedVolDyn0(ivhg) + BVSTR(ivhg) - sewer(ivhg)%BMaxSt
          BVSTR (ivhg) =  Sewer(ivhg)%BMaxSt
       endif
       Qswf  = min (QswfMax, PavedVolDyn0(ivhg))
       ! compute storages
       if (BVSTR0(ivhg) + PavedVolDyn0(ivhg) + RV(ivhg) - VV(ivhg) - QSWF .lt. sewer(ivhg)%BMaxSt) then
          IF (iDebug .ne.  0)  WRITE(IDEBUG,*) 'CMPVHG2016 case 1a'
          BVSTR (ivhg) =  BVSTR0(ivhg) + PavedVolDyn0(ivhg) + RV(ivhg) - VV(ivhg) - QSWF
          PavedVolDyn(ivhg) = 0.
       else
          IF (iDebug .ne.  0)  WRITE(IDEBUG,*) 'CMPVHG2016 case 1b'
          BVSTR (ivhg) =  sewer(ivhg)%BmaxSt
          PavedVolDyn(ivhg) = max (0.0, PavedVolDyn0(ivhg) + RV(ivhg) - VV(ivhg) - QSWF + BVSTR0(ivhg) - BVSTR(ivhg))
       endif
    elseif (BVSTR0(ivhg) + PavedVolDyn0(ivhg) .le.  EvapMax + QSWFMax - RV(ivhg)) then
      ! evap plus outflow capacity is equal or larger than actual storage plus rainfall
      ! so evap or outflow will be limited, storage on surface will be emptied
       if ( (BVSTR0(ivhg) + RV(ivhg) .le. EvapMax) .and. (PavedVolDyn0(ivhg) .le. QswfMax) ) then
          IF (iDebug .ne.  0)  WRITE(IDEBUG,*) 'CMPVHG2016 case 2a'
          VV    (ivhg) =  BVSTR0(ivhg) + RV(ivhg)
          Qswf         =  PavedVolDyn0(ivhg)
          PavedVolDyn(ivhg) = 0.
          BVSTR (ivhg) =  0.
       elseif ( (BVSTR0(ivhg) + RV(ivhg) .le. EvapMax) .and. (PavedVolDyn0(ivhg) .gt. QswfMax) ) then
          IF (iDebug .ne.  0)  WRITE(IDEBUG,*) 'CMPVHG2016 case 2b'
          Qswf         =  QswfMax
          VV    (ivhg) =  min (EvapMax, BVSTR0(ivhg) + PavedVolDyn0(ivhg) + (RV(ivhg)-QSWFMax))
          BVSTR (ivhg) =  0.
          PavedVolDyn(ivhg) = 0.
       elseif ( (BVSTR0(ivhg) + RV(ivhg) .gt. EvapMax) ) then
          IF (iDebug .ne.  0)  WRITE(IDEBUG,*) 'CMPVHG2016 case 2c'
          VV    (ivhg) =  EvapMax
          BVSTR (ivhg) =  BVSTR0(ivhg) + RV(ivhg) - VV(ivhg)
          PavedVolDyn(ivhg) = PavedVolDyn0(ivhg)
          if (BVSTR (ivhg) .gt. sewer(ivhg)%BMaxSt) then
             PavedVolDyn(ivhg) = PavedVolDyn0(ivhg) + BVSTR(ivhg) - sewer(ivhg)%BMaxSt
             BVSTR (ivhg) = sewer(ivhg)%BMaxSt
          endif
          Qswf =  min (QSwfMax, PavedVolDyn(ivhg))
          PavedVolDyn(ivhg) = PavedVolDyn(ivhg) - Qswf
       else
          IF (iDebug .ne.  0)  WRITE(IDEBUG,*) 'CMPVHG2016 case 2d'
          call SetMessage(LEVEL_FATAL, 'Unhandled case CmpVHG2016')
       endif
    endif
    IF (iDebug .ne.  0)  WRITE(IDEBUG,*) 'after surface balance; Qswf  =', Qswf
    IF (iDebug .ne.  0)  WRITE(IDEBUG,*) 'BVSTR =', BVSTR(ivhg)
    IF (iDebug .ne.  0)  WRITE(IDEBUG,*) 'PavedVolDyn =', PavedVolDyn(ivhg)

!   redistribution if storage street not full, and dynamic storage
    if (BVSTR(ivhg) .gt. 0 .and. BVSTR(ivhg) .lt. sewer(ivhg)%BMaxSt .and. PavedVolDyn(ivhg) .gt. 0) then
       BVStr(ivhg) = BVStr(ivhg) + PavedVolDyn(ivhg)
       PavedVolDyn(ivhg) = 0.
       if (BVStr(ivhg) .gt. sewer(ivhg)%BMaxSt) then
         PavedVolDyn(ivhg) = BVStr(ivhg) - sewer(ivhg)%BMaxSt
         BVStr(ivhg) = sewer(ivhg)%BMaxSt
       endif
       IF (iDebug .ne.  0)  WRITE(IDEBUG,*) 'BVSTR redistributed =', BVSTR(ivhg)
       IF (iDebug .ne.  0)  WRITE(IDEBUG,*) 'PavedVolDyn redistr =', PavedVolDyn(ivhg)
    endif

    ! *********************************************************************
    ! *** Surface runoff options: no delay, or rational method
    ! *********************************************************************

    INV(IVHG,1) = QSWF
    If (RunoffCalcOption(ivhg) .eq. 0) then
       ! no delay
        INV(IVHG,1) = QSWF
        IF (iDebug .ne.  0)  WRITE(IDEBUG,*) ' inflow sewer no delay INV(ivhg,1) = QSWF =', QSWF
    elseif (RunoffCalcOption(ivhg) .eq. 1 .or. RunoffCalcOption(ivhg) .eq. 3) then
       ! rational method to compute inflow from dynamic storage into sewer
       ! means adjusted inflow INV instead of QSWF, adjusted PavedVolDyn at end of timestep
         TOTUIT = 0.0
         VNOW   = PavedVolDyn0(ivhg)
         NetRain= max(0.0, PavedVolDyn(IVHG)-PavedVolDyn0(iVhg)+Qswf)
!        NetRain= max(0.0, RV(ivhg) - VV(ivhg))
         if (Vnow + NetRain .gt. 0.) then
            Call RunoffFactorFormulation (VNow, NetRain, RunoffFactor(ivhg), 0., Idebug, TimeSettings%TimestepSize, TotUit, Vinf, .true. )
         endif
         INV(ivhg,1) = Totuit
         TotalStorage = max (0.0, BVSTR0(ivhg) + PavedVolDyn0(ivhg) + RV(ivhg) - VV(ivhg) - TotUit)
         BVSTR(ivhg) = max (0.0, min (TotalStorage, sewer(ivhg)%BMaxSt))
         PavedVolDyn(ivhg) = TotalStorage - BVSTR(ivhg)
         IF (iDebug .ne.  0)  WRITE(IDEBUG,*) ' inflow sewer rational method QSWF INV(ivhg,1) =', QSWF, INV(ivhg,1)
!         PavedVolDyn (ivhg) = PavedVolDyn(ivhg) + INV(ivhg,1) - Qswf
!         IF (iDebug .ne.  0)  WRITE(IDEBUG,*) ' PavedVolDyn adjusted  =', PavedVolDyn(ivhg)
!         PavedVolDyn (ivhg) = max (0.0, PavedVolDyn(ivhg))
         IF (iDebug .ne.  0)  WRITE(IDEBUG,*) 'BVSTR after rational method =', BVSTR(ivhg)
         IF (iDebug .ne.  0)  WRITE(IDEBUG,*) 'PavedVolDyn rational method =', PavedVolDyn(ivhg)
    endif
    IF (iDebug .ne.  0)  WRITE(IDEBUG,*) 'CMPVHG2016 ivhg INV=',IVHG, INV(ivhg,1)

    ! *********************************************************************
    ! *** DWA inloop: is berekend in INIT2 in m3/s;
    ! *********************************************************************

    IF (iDebug .ne.  0)  WRITE(IDEBUG,*) 'CMPVHG2016 ivhg DWA=',IVHG, DWAPaved(ivhg)

    IF (sewer(iVhg)%systemType .eq. 0) then
      ! Gemengd stelsel
      BVRL(IVHG,1) = BVRL0(IVHG,1) + INV(IVHG,1) + DWAPaved(IVHG)* timeSettings%timestepSize
    ELSE
      ! gescheiden of verbeterd gescheiden stelsel
      BVRL(IVHG,1) = BVRL0(IVHG,1) + INV(IVHG,1)
      BVRL(IVHG,2) = BVRL0(IVHG,2) + DWAPaved(IVHG)* timeSettings%timestepSize
    ENDIF
    IF (iDebug .ne.  0)  WRITE(IDEBUG,*) 'CMPVHG2016  na DWA: BVRL=',BVRL(IVHG,1), BVRL(IVHG,2)


    ! *********************************************************************
    ! *** Berging in riool; inflow=from surface + DWA;  determine outflow
    ! *********************************************************************

    Q2V(IVHG,1)  = 0.0
    Q2V(IVHG,2)  = 0.0
    IF (BVRL(IVHG,1) .GT. 0 .OR. BVRL(IVHG,2) .GT. 0) THEN
        currentDate%year = ConfArr_get_iYear()
        currentDate%day = ConfArr_get_iDay()
        currentDate%month = ConfArr_get_iMonth()
        currentTime%hour = ConfArr_get_iHour()
        currentTime%minute = ConfArr_get_iMinute()
        currentTime%second = 0.0
        RowNr = -1
        TabelNr = VhgRefQC_TTable (iVhg)
        sewerCapacity(1) = getNewValue(TableHandle, TabelNr, 1, RowNr, CurrentDate, CurrentTime, &
                                        IDebug, Iout1, DateTimeOutsideTable, .true. )
        sewerCapacity(2) = getNewValue(TableHandle, TabelNr, 2, RowNr, CurrentDate, CurrentTime, &
                                        IDebug, Iout1, DateTimeOutsideTable, .true.)
        if (idebug .ne. 0)  write (idebug,*) 'new method sewer pump capacities', sewerCapacity(1), sewerCapacity(2)

        IF (BVRL(IVHG,1) .GT. 0) THEN
         ! index 1: RWA of gemengd riool
            Q2V(IVHG,1) = MIN ( BVRL(IVHG,1)/timeSettings%timestepSize, sewerCapacity(1))
            BVRL(IVHG,1) = BVRL(IVHG,1) - Q2V(IVHG,1) * timeSettings%timestepSize
            IF (BVRL(IVHG,1) .LT. -0.01) THEN
               call ErrMsgStandard(971, 0,  ' Error: negative sewerstorage', '')
            ENDIF
            BVRL(IVHG,1) = MAX (0.0, BVRL(IVHG,1))
        endif
        IF (BVRL(IVHG,2) .GT. 0) THEN
          ! index 2: DWA riool
          Q2V(IVHG,2) = MIN ( BVRL(IVHG,2)/timeSettings%timestepSize, sewerCapacity(2))
          BVRL(IVHG,2) = BVRL(IVHG,2) - Q2V(IVHG,2) * timeSettings%timestepSize
          IF (BVRL(IVHG,2) .LT. -0.01) THEN
             call ErrMsgStandard(971, 0,  ' Error: negative sewerstorage', '')
          ENDIF
          BVRL(IVHG,2) = MAX (0.0, BVRL(IVHG,2))
        ENDIF
     ENDIF

    ! *********************************************************************
    ! *** Overstort
    ! *********************************************************************

     Q1V(IVHG,1) = 0.0
     Q1V(IVHG,2) = 0.0
     INV(IVHG,2) = 0.0

! March 2004, Taiwan: add RunoffCalcOption for RWA/mixed sewer
      If (RunoffCalcOption(ivhg) .le. 1) then
         ! no delay, corresponding with old options 0 and 1
         if (idebug .ne. 0) write(idebug,*) 'RunoffCalcOption' , RunoffCalcOption(ivhg)
         ! no delay; old method, immediate discharge of excess water
         IF (BVRL(IVHG,1) .GT. sewer(iVhg)%BMAXRI(1) .and. sewer(iVhg)%systemType .ne. 2) THEN
         ! Gemengd stelsel of gescheiden stelsel: overstort van gemengd/RWA riool naar buiten
            Q1V(IVHG,1)  = (BVRL(IVHG,1)-sewer(iVhg)%BMAXRI(1)) / timeSettings%timestepSize
            BVRL(IVHG,1) = sewer(iVhg)%BMAXRI(1)
         ELSEIF (BVRL(IVHG,1) .GT. sewer(iVhg)%BMAXRI(1) .and. sewer(iVhg)%systemType .eq. 2) THEN
          ! Verbeterd gescheiden stelsel: overstort van gemengd/RWA riool eerst naar DWA, dan naar buiten.
           IF (BVRL(IVHG,2) .LT. sewer(iVhg)%BMAXRI(2)) THEN
             INV(IVHG,2)  = min (sewer(ivhg)%BMAXRI(2)-BVRL(IVHG,2), BVRL(IVHG,1)-sewer(iVhg)%BMAXRI(1) )
             BVRL(IVHG,1) =  BVRL(IVHG,1) - INV(IVHG,2)
             BVRL(IVHG,2) =  BVRL(IVHG,2) + INV(IVHG,2)
           ENDIF
           Q1V(IVHG,1)  = (BVRL(IVHG,1)-sewer(iVhg)%BMAXRI(1)) / timeSettings%timestepSize
           BVRL(IVHG,1) = sewer(iVhg)%BMAXRI(1)
         ENDIF
         !Check op overstort DWA riool
         IF (BVRL(IVHG,2) .GT. sewer(iVhg)%BMAXRI(2) .and. sewer(iVhg)%systemType .ge. 1) THEN
           Q1V(IVHG,2)  = (BVRL(IVHG,2)-sewer(iVhg)%BMAXRI(2)) / timeSettings%timestepSize
           BVRL(IVHG,2) = sewer(iVhg)%BMAXRI(2)
         ENDIF
      ElseIf (RunoffCalcOption(ivhg) .eq. 1) then
         ! not available for sewer overflow
      ElseIf (RunoffCalcOption(ivhg) .ge. 2) then
         ! Qh relation for sewer spill
         if (idebug .ne. 0) then
            write(idebug,*) 'RunoffCalcOption sewer overflow' , RunoffCalcOption(ivhg)
            write(idebug,*) 'Qh_Q(ipoint,ivhg),QH_h(ipoint,ivhg), ivhg and length', ivhg, QHTableLength(ivhg)
            Do ipoint=1,QHTableLength(ivhg)
              write(idebug,*) ipoint,Qh_Q(ipoint,ivhg),QH_h(ipoint,ivhg)
            Enddo
         Endif
         ! Q-h relation ; SOBEK JIRA 22739 May 2011
          LastInterpIndex = 1
          Actuallevel = -999.99
          IOW = EIOW(INODE)
          IBND = EIBND(INODE)
          IPluv = EIPluv(INODE)
          IRWZI = EIRWZI(INODE)
          if (ipluv .gt. 0) Actuallevel = -999.99
          if (iow .gt. 0)   Actuallevel = LvlOw0(iow)
          if (ibnd .gt. 0)  Actuallevel = BndPar(ibnd,1)
          Do i=1,QhTableLength(ivhg)
             QHPeilArray(i) = QH_h(i,ivhg)
             QHFlowArray(i) = QH_q(i,ivhg)
          Enddo
          CALL RR_INTERP (QhTableLength(ivhg), QhPeilArray, QHFlowArray, ActualLevel,ResultingMaxQ, LastInterpIndex)
          if (idebug .ne. 0) write(idebug,*) 'RunoffCalcOption' ,RunoffCalcOption(ivhg), RunoffFactor(ivhg)
          if (idebug .ne. 0) write(idebug,*) 'Qh relation level and maxflow ',ActualLevel, ResultingMaxQ
          TOTUIT = 0.0
          VNOW   = 0.0
          IF ((Vnow .gt. 0 .or. BVRL(IVHG,1) .GT. sewer(iVhg)%BMAXRI(1)) .and. sewer(iVhg)%systemType .ne. 2) THEN
         ! Gemengd stelsel of gescheiden stelsel: overstort van gemengd/RWA riool naar buiten, beperkt door ResultingMaxQ
             ! zie hieronder in gemeenschappelijk stuk
          ELSEIF ((Vnow .gt. 0 .or. BVRL(IVHG,1) .GT. sewer(iVhg)%BMAXRI(1)) .and. sewer(iVhg)%systemType .eq. 2) THEN
          ! Verbeterd gescheiden stelsel: overstort van gemengd/RWA riool eerst naar DWA, dan naar buiten.
            IF (BVRL(IVHG,2) .LT. sewer(iVhg)%BMAXRI(2)) THEN
               INV(IVHG,2)  = min (sewer(ivhg)%BMAXRI(2)-BVRL(IVHG,2),BVRL(IVHG,1)-sewer(iVhg)%BMAXRI(1) )
               BVRL(IVHG,1) =  BVRL(IVHG,1) - INV(IVHG,2)
               BVRL(IVHG,2) =  BVRL(IVHG,2) + INV(IVHG,2)
            ENDIF
            ! rest in gemeenschappelijk stuk
          ENDIF
         !Check op overstort DWA riool; no runoff formulation for DWA, since overflow may not occur
          IF (BVRL(IVHG,2) .GT. sewer(iVhg)%BMAXRI(2) .and. sewer(iVhg)%systemType .ge. 1) THEN
             Q1V(IVHG,2)  = (BVRL(IVHG,2)-sewer(iVhg)%BMAXRI(2)) / timeSettings%timestepSize
             BVRL(IVHG,2) = sewer(iVhg)%BMAXRI(2)
          ENDIF
! altijd voor sewer system 1 / gemengd of RWA riool
          ! gewenste overstort
          TotUit= Vnow + max(0.0, BVRL(IVHG,1)-sewer(iVhg)%BMAXRI(1))
          ! max. overstort
          TotUit= min(ResultingMaxQ, TotUit/timeSettings%TimestepSize)
          Q1V(ivhg,1) = Totuit
          ! overstort
          BvrlOutflow = Totuit
          BVRL(IVHG,1) = BVRL(ivhg,1) - BvrlOutflow * Timesettings%TimestepSize
          if (BVRL(IVHG,1) .gt. sewer(iVhg)%BMAXRI(1)) then
              PavedVolDyn(ivhg) = PavedVolDyn(ivhg) + BVRL(IVHG,1) - sewer(iVhg)%BMAXRI(1)
              BVRL(IVHG,1) = sewer(iVhg)%BMAXRI(1)
          elseif (BVRL(IVHG,1) .le. 0.) then
              Q1V(ivhg,1)  = Q1V(ivhg,1) + ( BVRL(ivhg,1)/ TimeSettings%TimestepSize)
              BvrlOutflow  = BvrlOutflow + ( BVRL(ivhg,1)/ TimeSettings%TimestepSize)
              Totuit       = Totuit      + ( BVRL(ivhg,1)/ TimeSettings%TimestepSize)
              BVRL(ivhg,1) = 0.0
          endif
          ! for Qh-relation option:
          IF (iDebug .ne.  0) THEN
             WRITE(IDEBUG,*) ' after Qh relation test '
             WRITE(IDEBUG,*) ' Vnow ', Vnow
             WRITE(IDEBUG,*) ' Totuit, BvrlOutflow', Totuit, BvrlOutflow
             WRITE(IDEBUG,*) ' PavedVolDyn0 and VolDyn ',PavedVolDyn0(ivhg), PavedVolDyn(ivhg)
             WRITE(IDEBUG,*) ' BVRL and BVRL0 - mixed/RWA ',BVRL(ivhg,1), BVRL0(ivhg,1)
             WRITE(IDEBUG,*) ' BVRL and BVRL0 - DWA       ',BVRL(ivhg,2), BVRL0(ivhg,2)
          ENDIF

      Endif

    ! *********************************************************************
    ! *** Inundatie
    ! *********************************************************************

    IOW = EIOW(INODE)
    IBND = EIBND(INODE)
    IPluv = EIPluv(INODE)
    IRWZI = EIRWZI(INODE)
    IF (iDebug .ne.  0) THEN
       WRITE(IDEBUG,*) ' IVHG  INODE IOW  IBND ipluv IRWZI '
       WRITE(IDEBUG,'(6I7)')  IVHG,  INODE, IOW,  IBND, ipluv, IRWZI
       WRITE(IDEBUG,*)  ' Q2VOW ',sewer(iVhg)%Q2VOW(1), sewer(iVhg)%Q2VOW(2)
       WRITE(IDEBUG,*)  ' BMAXRI',sewer(iVhg)%BMAXRI(1), sewer(iVhg)%BMAXRI(2)
       WRITE(IDEBUG,*)  ' Sewercap',sewercapacity(1), sewercapacity(2)
    ENDIF

    IF (IOW .GT. 0) THEN
!      inundation
       IF (LVLOW0(IOW) .GT. LVLVH(IVHG)) THEN
         call ConfArr_set_fldVhg(.TRUE.)
         if (iOut1 .ne. 0 .and. MessageInundation .gt. 0) then
           WRITE(IOUT1, '(A,A,A,2I5)') ' Inundation paved area (from open water)', &
                 Id_Nod(INODE),' in event/timestep', IEVENT, ITMSTP
         endif
       ENDIF
!      check sewer overflow levels
       Call CheckSewerOverflowOW (Ivhg, iow, 1, messageinundation)   ! 1= RWF index
       If (sewer(ivhg)%Systemtype .ne. 0) Call CheckSewerOverflowOW (Ivhg, iow, 2, messageinundation) ! 2= DWF index

    ELSEIF (IBND .GT. 0) THEN
       IF (BNDPAR(IBND,1) .GT. LVLVH(IVHG)) THEN
!        inundation
         call ConfArr_set_fldVhg(.TRUE.)
         if (iOut1 .ne. 0 .and. MessageInundation .gt. 0) then
           WRITE(IOUT1, '(A,A,A,2I5)') ' Inundation paved area (from boundary)', &
                 Id_Nod(INODE),' in event/timestep', IEVENT, ITMSTP
         endif
       ENDIF
       Call CheckSewerOverflowBnd (Ivhg, ibnd, 1, messageinundation)   ! 1= RWF index
       If (sewer(ivhg)%Systemtype .ne. 0) Call CheckSewerOverflowBnd(Ivhg, ibnd, 2, messageinundation) ! 2= DWF index

    ELSEIF (IPluv .GT. 0) THEN
        ! no limitation of discharge to NWRW node
    ELSE
       Write(*,*) ' Internal error Sobek-3B: CMPVHG2016: IOW=IBND=0'
       call ErrMsgStandard (981, 0, 'Paved node should be connected to open water/boundary/NWRW runoff node for spilling ', Id_Nod(inode))
    ENDIF

    ! *********************************************************************
    ! *** Totaal naar open water of naar boundary, of naar RWZI
    ! Q1 (ivhg,*) = overstort
    !               * = 1,2, voor gescheiden systemen
    ! Q2 (ivhg,*) = rioolgemaal
    !               *=1,2, voor gescheiden systemen
    ! *********************************************************************

    IF (iDebug .ne.  0) WRITE(IDEBUG,'(A,5I7)') ' Paved area TOEWIJZING SPIL EN PUMPED FLOW', &
                                                 IVHG, iow, ibnd, ipluv, irwzi
    IF (IOW .GT. 0) THEN
       QINOW(IOW,1) = QINOW(IOW,1) + Q1V(IVHG,1) + Q1V(IVHG,2)
        ! gemengd of RWA riool
       If (sewer(iVhg)%Q2VOW(1) .eq. 1) then
          QINOW(IOW,1) = QINOW(IOW,1) + Q2V(IVHG,1)
       Elseif (sewer(iVhg)%Q2VOW(1) .eq. 0) then
    ! outflow naar welke boundary?
          IBND = VHGBND(IVHG)
          QBND (IBND)  = QBND(IBND) +  Q2V(IVHG,1)
          QINBND(IBND)%totalPaved = QINBND(IBND)%totalPaved + Q2V(IVHG,1)
       Elseif (sewer(iVhg)%Q2VOW(1) .eq. 2) then
    ! outflow naar welke RWZI?
          IRWZI = VHGRWZI(IVHG)
          QRWZI (IRWZI)  = QRWZI(IRWZI) +  Q2V(IVHG,1)
       Elseif (sewer(iVhg)%Q2VOW(1) .eq. 3) then
    ! outflow naar NWRW node
          IPluv = EIPluv(INODE)
          QInPluv(iPluv)  = QinPluv(ipluv) + Q2V(IVHG,1)
          QPluv(Ipluv)%totalPaved = QPluv(IPluv)%totalPaved + Q2V(IVHG,1)
       else
          call SetMessage(LEVEL_FATAL, 'Unknown value of Q2vow(1)')
       ENDIF
       ! DWA riool; only in case of seperated system!
       IF (sewer(iVhg)%systemType .ne. 0) THEN
           IF (sewer(iVhg)%Q2VOW(2) .eq. 1) THEN
              QINOW(IOW,1) = QINOW(IOW,1) + Q2V(IVHG,2)
           ELSEif (sewer(iVhg)%Q2VOW(2) .eq. 0) then
            !      outflow naar welke boundary?
             IBND = VHGBND(IVHG)
             QBND (IBND)  = QBND(IBND) +  Q2V(IVHG,2)
             QINBND(IBND)%totalPaved = QINBND(IBND)%totalPaved + Q2V(IVHG,2)
           Elseif (sewer(iVhg)%Q2VOW(2) .eq. 2) then
           ! outflow naar welke RWZI?
              IRWZI = VHGRWZI(IVHG)
              QRWZI (IRWZI)  = QRWZI(IRWZI) +  Q2V(IVHG,2)
           ELSEif (sewer(iVhg)%Q2VOW(2) .eq. 3) then
            !      outflow naar NWRW node
              QInPluv(iPluv)  = QinPluv(IPluv) +  Q2V(IVHG,2)
              QPluv(IPluv)%totalPaved = QPluv(IPluv)%totalPaved + Q2V(IVHG,2)
           ENDIF
       ENDIF
       IF (iDebug .ne.  0) THEN
         WRITE(IDEBUG,*) ' IVHG  INODE IOW  IBND  IPluv IRWZI '
         WRITE(IDEBUG,'(6I7)')  IVHG,  INODE, IOW,  IBND, Ipluv, iRwzi
         WRITE(IDEBUG,*) ' QINOW(IOW,1)   QBND(IBND)/QRWZI   QINBND_totalpaved'
         IF (IRWZI .GT. 0) THEN
            WRITE(IDEBUG,*) QINOW(IOW,1), ' Rwzi', QRWZI(IRWZI)
         ENDIF
         IF (IBND .GT. 0) THEN
            WRITE(IDEBUG,*) QINOW(IOW,1), QBND(IBND), QINBND(IBND)%totalPaved
         Elseif (Ipluv .GT. 0) THEN
            WRITE(IDEBUG,*) QINOW(IOW,1), QinPluv(IPluv) ,QPluv(IPluv)%totalPaved
         ELSE
            WRITE(IDEBUG,*) QINOW(IOW,1)
         ENDIF
       ENDIF
    ELSEIF (IBND .GT. 0 .and. IRWZI .le. 0) THEN
! all flows to boundary regardless of specified sewer-to-openwater/boundary;
! no downstream open water, only downstream boundary
       QINBND(IBND)%totalPaved = QINBND(IBND)%totalPaved &
              + Q1V(IVHG,1) + Q1V(IVHG,2) + Q2V(IVHG,1) + Q2V(IVHG,2)
       QBND(IBND) = QBND(IBND) + Q1V(IVHG,1) + Q1V(IVHG,2) + Q2V(IVHG,1) + Q2V(IVHG,2)
       IF (iDebug .ne.  0) THEN
         WRITE(IDEBUG,*) ' IVHG  INODE  IBND  QINBND(IBND)_totalPaved QBND(IBND)'
         WRITE(IDEBUG,*) IVHG, INODE, IBND, QINBND(IBND)%totalPaved, QBND(IBND)
       ENDIF
    ELSEIF (IBND .GT. 0 .and. IRWZI .gt. 0) THEN
! all flows to boundary or RWZI; spilling to boundary
       QINBND(IBND)%totalPaved = QINBND(IBND)%totalPaved + Q1V(IVHG,1) + Q1V(IVHG,2)
       QBND(IBND) = QBND(IBND) + Q1V(IVHG,1) + Q1V(IVHG,2)

       if (sewer(iVhg)%Q2VOW(1) .eq. 2) then
         ! outflow naar welke RWZI?
         IRWZI = VHGRWZI(IVHG)
         QRWZI (IRWZI)  = QRWZI(IRWZI) +  Q2V(IVHG,1)
       else
         ! to welke boundary?
         QINBND(IBND)%totalPaved = QINBND(IBND)%totalPaved + Q2V(IVHG,1)
         QBND(IBND) = QBND(IBND) + Q2V(IVHG,1)
       endif

       if (sewer(iVhg)%Q2VOW(2) .eq. 2) then
         ! outflow naar welke RWZI?
         IRWZI = VHGRWZI(IVHG)
         QRWZI (IRWZI)  = QRWZI(IRWZI) +  Q2V(IVHG,2)
       else
        ! to welke boundary?
         QINBND(IBND)%totalPaved = QINBND(IBND)%totalPaved + Q2V(IVHG,2)
         QBND(IBND) = QBND(IBND) + Q2V(IVHG,2)
       endif

       IF (iDebug .ne.  0) THEN
         write(idebug,*) ' ivhg     iow     ibnd     irwzi  '
         write(idebug,*)   ivhg, iow, ibnd, irwzi
         WRITE(IDEBUG,*) ' QINBND(IBND)_totalPaved QBND(IBND)'
         WRITE(IDEBUG,*)  QINBND(IBND)%totalPaved, QBND(IBND)
         WRITE(IDEBUG,*) ' IRWZI  QRWZI(IRWZI)'
         WRITE(IDEBUG,*) IRWZI, QRWZI(IRWZI)
         WRITE(IDEBUG,*) ' Q2V(1-2)', Q2V(IVHG,1), Q2V(IVHG,2)
       ENDIF
    ELSEIF (IPluv .GT. 0 .and. irwzi .ne. 0 ) THEN
!    pump to rwzi, spill to NWRW node
       QPluv(IPluv)%totalPaved = QPluv(IPluv)%totalPaved &
               + Q1V(IVHG,1) + Q1V(IVHG,2)
       QinPluv(IPluv) = QinPluv(IPluv) + Q1V(IVHG,1) + Q1V(IVHG,2)
       ! pumped flow to RWZI
       if (sewer(iVhg)%Q2VOW(1) .eq. 2) then
         ! outflow naar welke RWZI?
         IRWZI = VHGRWZI(IVHG)
         QRWZI (IRWZI)  = QRWZI(IRWZI) +  Q2V(IVHG,1)
       else  ! also to NWRW
          QinPluv(IPluv) = QinPluv(IPluv) + Q2V(IVHG,1)
          QPluv(IPluv)%totalPaved = QPluv(IPluv)%totalPaved + Q2V(IVHG,1)
       endif
       if (sewer(iVhg)%Q2VOW(2) .eq. 2) then
         ! outflow naar welke RWZI?
         IRWZI = VHGRWZI(IVHG)
         QRWZI (IRWZI)  = QRWZI(IRWZI) +  Q2V(IVHG,2)
       else  ! also to NWRW
          QinPluv(IPluv) = QinPluv(IPluv) + Q2V(IVHG,2)
          QPluv(IPluv)%totalPaved = QPluv(IPluv)%totalPaved + Q2V(IVHG,2)
       endif
       IF (iDebug .ne.  0) THEN
         WRITE(IDEBUG,*) ' IVHG  INODE  IPluv QPluv(IPluv)_totalPaved QBND(IBND)'
         WRITE(IDEBUG,*) IVHG, INODE, IPluv, QPluv(IPluv)%totalPaved, QinPluv(IPluv)
       ENDIF
    ELSEIF (IPluv .GT. 0 .and. iow .eq. 0 .and. ibnd .eq. 0 .and. irwzi .eq. 0 ) THEN
!    all flows to NWRW node
       QPluv(IPluv)%totalPaved = QPluv(IPluv)%totalPaved &
               + Q1V(IVHG,1) + Q1V(IVHG,2) + Q2V(IVHG,1) + Q2V(IVHG,2)
       QinPluv(IPluv) = QinPluv(IPluv) + Q1V(IVHG,1) + Q1V(IVHG,2) + Q2V(IVHG,1) + Q2V(IVHG,2)
       IF (iDebug .ne.  0) THEN
         WRITE(IDEBUG,*) ' IVHG  INODE  IPluv QPluv(IPluv)_totalPaved QBND(IBND)'
         WRITE(IDEBUG,*) IVHG, INODE, IPluv, QPluv(IPluv)%totalPaved, QinPluv(IPluv)
       ENDIF
    ELSE
       call SetMessage(LEVEL_ERROR, 'Internal error Sobek-3B: CMPVHG2016: IOW=IBND=0')
       call ErrMsgStandard (981, 0, 'Paved node should be connected to open water/boundary/NWRW runoff node for spilling', Id_Nod(inode))
    ENDIF

    ! *********************************************************************
    ! *** debug
    ! *********************************************************************

    IF (iDebug .ne.  0) THEN
       WRITE(IDEBUG,*) ' Paved area', Id_Nod(INODE)
       WRITE(IDEBUG,*) ' Timestep nr                :',ITMSTP
       WRITE(IDEBUG,*) ' Timestepsize in s          :',timeSettings%timestepSize
       WRITE(IDEBUG,*) ' Surface level m            :',LVLVH (IVHG)
       WRITE(IDEBUG,*) ' Total surface in m2        :',AREAVH(IVHG)
       WRITE(IDEBUG,*) ' Max. cap. RWA pump in m3/s :',Q2VMAX(IVHG,1)
       WRITE(IDEBUG,*) ' Max. cap. DWA pump in m3/s :',Q2VMAX(IVHG,2)
       WRITE(IDEBUG,*) ' Initial storage street m3  :',BVSTR0(IVHG)
       WRITE(IDEBUG,*) ' Initial stor. RWA sewer m3 :',BVRL0 (IVHG,1)
       WRITE(IDEBUG,*) ' Initial stor. DWA sewer m3 :',BVRL0 (IVHG,2)
       WRITE(IDEBUG,*) ' Final   storage street m3  :',BVSTR (IVHG)
       WRITE(IDEBUG,*) ' Final storage RWA sewer m3 :',BVRL  (IVHG,1)
       WRITE(IDEBUG,*) ' Final storage DWA sewer m3 :',BVRL  (IVHG,2)
       WRITE(IDEBUG,*) ' Voldyn initial             :',PavedVolDyn0(IVHG)
       WRITE(IDEBUG,*) ' Voldyn final               :',PavedVolDyn (IVHG)
       WRITE(IDEBUG,*) ' Evaporation street in m3   :',VV    (IVHG)
       WRITE(IDEBUG,*) ' Precipitation in m3        :',RV    (IVHG)
       WRITE(IDEBUG,*) ' Surface inflow to sewer(m3):',INV   (IVHG,1)
       WRITE(IDEBUG,*) ' DWA-Inflow sewer in m3/s   :',DWAPaved(IVHG)
       WRITE(IDEBUG,*) ' DWA-Inflow sewer in m3     :',DWAPaved(IVHG) * timeSettings%timestepSize
       WRITE(IDEBUG,*) ' Pumped Q RWA sewer in m3/s :',Q2V   (IVHG,1)
       WRITE(IDEBUG,*) ' Overflow RWA to DWA in m3  :',INV   (IVHG,2)
       WRITE(IDEBUG,*) ' Overflow RWA sewer in m3/s :',Q1V   (IVHG,1)
       WRITE(IDEBUG,*) ' Pumped Q DWA sewer in m3/s :',Q2V   (IVHG,2)
       WRITE(IDEBUG,*) ' Overflow DWA sewer in m3/s :',Q1V   (IVHG,2)
    ENDIF

    RETURN
  END subroutine cmpvhg2016


  Subroutine CheckSewerOverFlowOW (Ivhg, iow, index, MessageInundation)
  Integer ivhg, iow, index, MessageInundation, Iout1, idebug

  Real Dh, DV   !, Peil, Heq, ActSewerLevel
  Character(Len=3) SewerType(2)

  iDebug = ConfFil_get_idebug()
  iOut1 = ConfFil_get_iOut1()
  SewerType(1) = 'RWF'
  SewerType(2) = 'DWF'

    IF (LvlOW0(iow) .GT. sewer(ivhg)%OverflowLevel(index)) THEN
       IF (Q1V(IVHG,index) .GT. 0) THEN
         if (iOut1 .ne. 0 .and. MessageInundation .gt. 0) &
            call SetMessage(LEVEL_WARN, 'Set sewer overflow to zero for'//Sewertype(Index))
         if (idebug .ne. 0) WRITE(Idebug,'(A,A)') ' Set sewer overflow to zero for',Sewertype(Index)
         BVRL(IVHG,index) = BVRL(IVHG,index) + Q1V(IVHG,index) * timeSettings%timestepSize
         Q1V (IVHG,index) = 0.0
       Endif
       If (sewer(ivhg)%InflowPossible(index)) then
         if (iOut1 .ne. 0 .and. MessageInundation .gt. 0) &
            call SetMessage(LEVEL_WARN, 'External inflow from RR-openwater into sewer'//Sewertype(Index))
          ! first sewer can be filled up to maximum
         if (idebug .ne. 0) WRITE(Idebug,'(A,A)') ' External inflow to sewer for ',Sewertype(Index)
         If (BVRL(Ivhg,index) .lt. sewer(ivhg)%BMaxRi(index)) then
            Dh = LvlOw0(iow) - sewer(ivhg)%OverflowLevel(index)
            DV = min (Dh * AROW0(iow), sewer(ivhg)%BMaxRi(index) - BVRL(ivhg,index))
            Q1V(ivhg,index) = DV / timesettings%timestepSize
         endif
         ! then level possible excess water? Dat doen we niet!!
!        peil = Lvlow0(iow) - DV / ArOw0(iow)
!        ActSewerLevel = max (0.0, (Bvrl(ivhg,index)-sewer(ivhg)%BMaxRi(index))/AreaVh(ivhg))
!        ActSewerLevel = sewer(ivhg)%OverflowLevel(index) + actSewerLevel
!        Heq = (peil * Arow0(iow) + actsewerlevel * AreaVh(ivhg)) / (Arow0(iow) + AreaVh(ivhg))
!        DV  = (peil - Heq) * ArOw0(iow)
!        Q1V (IVHG,index) = Q1V(ivhg,index) - DV / timeSettings%TimestepSize
!        BVRL(IVHG,index) = BVRL(IVHG,index) + DV
       ENDIF
    ENDIF

  Return
  End subroutine CheckSewerOverFlowOW


  Subroutine CheckSewerOverFlowBnd (Ivhg, ibnd, index, MessageInundation)
  Integer ivhg, ibnd, index, MessageInundation, Iout1, idebug

  Real    Dh, DV   !, Peil, Heq, ActSewerLevel
  Character(Len=3) SewerType(2)

  iDebug = ConfFil_get_idebug()
  iOut1 = ConfFil_get_iOut1()
  SewerType(1) = 'RWF'
  SewerType(2) = 'DWF'

    IF (BndPar(ibnd,1) .GT. sewer(ivhg)%OverflowLevel(index)) THEN
       IF (Q1V(IVHG,index) .GT. 0) THEN
         if (iOut1 .ne. 0 .and. MessageInundation .gt. 0) &
            call SetMessage(LEVEL_WARN, 'Set sewer overflow to zero for'//Sewertype(Index))
         if (idebug .ne. 0) WRITE(Idebug,'(A,A)') ' Set sewer overflow to zero for',Sewertype(Index)
         BVRL(IVHG,index) = BVRL(IVHG,index) + Q1V(IVHG,index) * timeSettings%timestepSize
         Q1V (IVHG,index) = 0.0
       Endif
       If (sewer(ivhg)%InflowPossible(index)) then
         if (iOut1 .ne. 0 .and. MessageInundation .gt. 0) &
            call SetMessage(LEVEL_WARN, 'External inflow from boundary into sewer'//Sewertype(Index))
          ! first sewer can be filled up to maximum
         if (idebug .ne. 0) WRITE(Idebug,'(A,A)') ' External inflow to sewer for ',Sewertype(Index)
         If (BVRL(Ivhg,index) .lt. sewer(ivhg)%BMaxRi(index)) then
            Dh = BndPar(ibnd,1) - sewer(ivhg)%OverflowLevel(index)
            DV = min (Dh * BndPar(ibnd,5), sewer(ivhg)%BMaxRi(index) - BVRL(ivhg,index))
            Q1V(ivhg,index) = DV / timesettings%timestepSize
            if (idebug .ne. 0) write(idebug,*) ' reduce outflow with DV=', DV
         endif
         ! then level possible excess water? Dat doen we niet!!
!        if (idebug .ne. 0) write(idebug,*) ' inflow into sewer'
!        ActSewerLevel = max (0.0, (Bvrl(ivhg,index)-sewer(ivhg)%BMaxRi(index))/AreaVh(ivhg))
!        ActSewerLevel = sewer(ivhg)%OverflowLevel(index) + actSewerLevel
!        peil = BndPar(ibnd,1) - DV / BndPar(ibnd,5)
!        if (idebug .ne. 0) write(idebug,*) ' BndPar(ibnd,*) ', Bndpar(ibnd,1), bndpar(ibnd,5)
!        if (idebug .ne. 0) write(idebug,*) ' peil = ', peil
!        Heq = (peil * BndPar(ibnd,5) + ActSewerLevel * AreaVh(ivhg)) / (BndPar(ibnd,5) + AreaVh(ivhg))
!        if (idebug .ne. 0) write(idebug,*) ' sewer overflow level', sewer(ivhg)%OverflowLevel(index)
!        if (idebug .ne. 0) write(idebug,*) ' Act. sewer level',ActSewerLevel
!        if (idebug .ne. 0) write(idebug,*) ' Heq = ', Heq
!        DV  = (peil - Heq) * BndPar(ibnd,5)
!        if (idebug .ne. 0) write(idebug,*) ' DV  = ', DV
!        Q1V (IVHG,index) = Q1V(ivhg,index) - DV / timeSettings%TimestepSize
!        BVRL(IVHG,index) = BVRL(IVHG,index) + DV
!        if (idebug .ne. 0) write(idebug,*) ' Q1V  = ', Q1V (ivhg,index)
!        if (idebug .ne. 0) write(idebug,*) ' BVRL = ', BVRL(ivhg,index)
       ENDIF
    ENDIF

  Return
  End subroutine CheckSewerOverFlowBnd


  Subroutine Init1Paved
    ! *********************************************************************
    ! *** Last update:
    ! *********************************************************************
    ! *** Brief description:
    ! *** ------------------
    ! ***    Stuk uit sub Init1: initialisie van verhard gebied aan begin bui
    ! *********************************************************************

      Implicit none

    ! variables

      Integer Ivhg

        DO IVHG = 1,NCVHG
           BVSTR (IVHG) = sewer(iVhg)%BINIST
           BVRL  (IVHG,1) = sewer(iVhg)%BINIRI(1)
           BVRL  (IVHG,2) = sewer(iVhg)%BINIRI(2)
        ENDDO
        PavedVolDyn = 0.0
        PavedVolDyn0= 0.0
        ! if present in restart file, BVRL BVSTR PavedVolDyn will be overwritten
  Return
  END subroutine Init1Paved


  Subroutine Init2Paved (IHour, NrSHr)
    ! *********************************************************************
    ! *** Last update:
    ! *********************************************************************
    ! *** Brief description:
    ! *** ------------------
    ! ***    Stuk uit sub Init2: initialisie van verhard gebied per tijdstap
    ! ***    NrSHr = nr seconds per hour
    ! *********************************************************************

      Implicit none

    ! variables

      Integer NrSHr, IHour
      Integer Ivhg


      PavedVolDyn0 = PavedVolDyn

      DO IVHG = 1,NCVHG
         BVSTR0 (IVHG)   = BVSTR (IVHG)
         BVRL0  (IVHG,1) = BVRL  (IVHG,1)
         BVRL0  (IVHG,2) = BVRL  (IVHG,2)

         IF (DWACalcOption(Ivhg) .EQ. 1) THEN
             DWAPaved(Ivhg) = NrPeople(Ivhg) * DWADISPaved(Ivhg,1) * 0.001 / NRSHR
         ELSEIF (DWACalcOption(Ivhg) .EQ. 2) THEN
             DWAPaved(Ivhg) = NrPeople(Ivhg) * DWADISPaved(Ivhg,2) * 0.001 / nrsHr   &
                                      * 0.01 * DWADISPaved(Ivhg, Ihour + 3)
         !+ 3 omdat de eerste 2 elementen uit het array ergens anders voor worden gebruikt !
         ELSEIF (DWACalcOption(Ivhg) .EQ. 3) THEN
             DWAPaved(Ivhg) = DWADISPaved(Ivhg,1) * 0.001 / NRSHR
         ELSEIF (DWACalcOption(Ivhg) .EQ. 4) THEN
             DWAPaved(Ivhg) = DWADISPaved(Ivhg,2) * 0.001 / nrsHr  &
                                      * 0.01 * DWADISPaved(Ivhg,IHour + 3)
         !+ 3 omdat de eerste 2 elementen uit het array ergens anders voor worden gebruikt !
         ENDIF
 !        IF (iDebug .ne. 0)  WRITE(IDEBUG,*) ' DWAPaved ', DWAPaved(IVHG)
      ENDDO

  Return
  END subroutine Init2Paved


  Subroutine WrInputDataPaved (Iout9, Iout2, RnDate, RnTime)
    ! *********************************************************************
    ! *** Last update:
    ! *********************************************************************
    ! *** Brief description:
    ! *** ------------------
    ! ***    Stuk uit sub Wrdata: uitvoer van Paved node
    ! *********************************************************************

      Implicit none

    ! variables

    Integer      INODE, IKIND, INR, i
    Integer      IOUT9, IOUT2
    Integer*2    RNDATE(3), RNTIME(4)


! Verhard gebied
      IF (NCVHG .GT. 0) THEN
         WRITE(IOUT9,11)
   11    FORMAT (//,' Summary input data paved area          ',//,&
              ' Node identification     Node    Surface    Surf. max storage   ', &
              ' max.storage  Capacity  ',/,       &
              '                         name                         street    ', &
              '  sewer       sewerpump  ',/,  &
              '                                   (ha)      (m NAP)    (mm)    ', &
              '   (mm)         (m3/s)   ',/,91('='))
         DO INODE =1,NCNODE
          IKIND = EiNode(INODE,3)
          INR   = EiNode(INODE,2)
          IF (IKIND .EQ. 1) THEN
            IF (AREAVH(INR) .LE. .00001) AREAVH(INR) = 0.00001
            WRITE(IOUT9,21) Id_Nod(INODE),&
                           NamNod(INODE), &
                           AREAVH(INR)/HA2M, LVLVH(INR), &
                           sewer(inr)%BMAXST/AREAVH(INR)/MM2M, &
                           sewer(inr)%BMAXRI(1)/AREAVH(INR)/MM2M, Q2VMAX(INR,1)
   21       FORMAT (A20,1X,A12,1X,F9.3,1X,3(F9.2,1X),F9.3)
          ENDIF
         ENDDO
      ENDIF

! paved area
      If (ncvhg .gt. 0 .and. OutputDesired(1) ) then
        WRITE(IOUT2,51) RNDATE(3),RNDATE(2),RNDATE(1),(RNTIME(I),I=1,2), CASENM
   51   FORMAT (' Run datum (DD-MM-YY) and time:',I2,'-',I2,'-',I4,2X,I2,':',I2,//,&
                ' Case name                 ',A20,/)
        WRITE(IOUT2,1001) '[mm]','[m3/s]','[m3/s]', '[m3/s]', '[m3/s]'
 1001   FORMAT(//,' Maxima per event',//, &
                  ' Event   Start     Node identification   Node  ',7X,&
                  '  Storage_sewer   Overflow    Discharge   Total open water Rainfall',/,&
                  '  nr  year-mon-day                       name',&
                  14X,A6,9X,A6,7X,A6,9X,A6,9X,A6,/,123('='))
      Endif

  Return
  END subroutine WrInputDataPaved



  Subroutine Wr1OutPaved (Iout2, Ievent, Month, INode, IVhg)
    ! *********************************************************************
    ! *** Last update:
    ! *********************************************************************
    ! *** Brief description:
    ! *** ------------------
    ! ***    Stuk uit sub Wr1Out: uitvoer van Paved node: maxima per event in OUT file
    ! *********************************************************************

      Implicit none

    ! variables

    Integer      INODE, IVhg, Iout2, Ievent
    Real         Bergng, QOver, QMaal, QTotl, QRain
    CHARACTER(len=3) MONTH(12)

      if (.not. associated(VHMBPC)) return    ! If there is nothing, do nothing

      ! berging in mm
      BERGNG = VHMBPC(IVHG,1,IEVENT)
      ! flows in m3/s
      QOVER = VHMQOU(IVHG,1,IEVENT)
      QMAAL = VHMQOU(IVHG,2,IEVENT)
      QTOTL = VHMQOU(IVHG,3,IEVENT)
      QRAIN = VHMQOU(IVHG,4,IEVENT)
      WRITE(IOUT2,1002) IEVENT, EventStartDateTime(IEVENT,1),MONTH(EventStartDateTime(IEVENT,2)), &
          EventStartDateTime(IEVENT,3),&
          Id_Nod(INODE),NamNod(INODE),&
          BERGNG, QOVER, QMAAL, QTOTL, QRAIN
 1002 FORMAT (I4,1X,I4,1X,A3,1X,I3,3X,A20,1X,A15,1X,5(F12.3,1X))

  Return
  END subroutine Wr1OutPaved


  Subroutine Paved_DeAllocateArrays

    if (Allocated(Sewer)) DeAllocate(Sewer)

  Return
  End subroutine Paved_DeallocateArrays

  !> If success, function returns Values array of length ElementCount
  !! for paved elementset on specific quantity handle
  function RR_GetPavedDataByIntId(QuantityHandle, ElementCount, Values) result(success)

    use RR_open_mi_support
    use wl_open_mi_support

    implicit none

    ! return value
    logical  :: success

    ! arguments
    integer,          intent(in)      :: QuantityHandle   !< quant. handle
    integer,          intent(in)      :: ElementCount     !< #elems in paved elementset
    double precision, intent(out), &
            dimension(1:ElementCount) :: Values           !< values in paved elemenset

    ! locals

    Values  = 0
    success = .true.

    select case (QuantityHandle)
    ! Subdivide for various variables
    case(RRiStorageRWA_mm)
    !RR Storage RWA in mm
            ! If a stack overflow occurs due to this array operation, use a do loop
            if (NVHG > 0) then
                Values(1:NVHG) = RslMap1_vhg(1, 1:NVHG, 1)
            else
                success = .false.
            endif
    case (RRiStorageDWA_mm)
    !RR Storage DWA in mm
            if (NVHG > 0) then
                Values(1:NVHG) = RslMap1_vhg(2, 1:NVHG, 1)
            else
                success = .false.
            endif
    case(RRiStorageStreet_mm)
    !RR Storage street in mm
            ! If a stack overflow occurs due to this array operation, use a do loop
            if (NVHG > 0) then
                Values(1:NVHG) = RslMap1_vhg(3, 1:NVHG, 1)
            else
                success = .false.
            endif
    case(RRiSpillingTotal)
    !RR Spilling in m3/s
            ! If a stack overflow occurs due to this array operation, use a do loop
            if (NVHG > 0) then
                Values(1:NVHG) = RslMap1_vhg(4, 1:NVHG, 1)
            else
                success = .false.
            endif
    case(RRiPumpedTotal)
    !RR Pumped Flow m3/s
            ! If a stack overflow occurs due to this array operation, use a do loop
            if (NVHG > 0) then
                Values(1:NVHG) = RslMap1_vhg(5, 1:NVHG, 1)
            else
                success = .false.
            endif
    case(RRiRainfall)
    !RR Rainfall in m3/s
            ! If a stack overflow occurs due to this array operation, use a do loop
            if (NVHG > 0) then
                Values(1:NVHG) = RslMap1_vhg(7, 1:NVHG, 1)
            else
                success = .false.
            endif
    case(RRiDWA2RWA)
    !RR DWA inflow - RWA in m3/s
            ! If a stack overflow occurs due to this array operation, use a do loop
            if (NVHG > 0) then
                Values(1:NVHG) = RslMap1_vhg(8, 1:NVHG, 1)
            else
                success = .false.
            endif
    case(RRiDWA2DWA)
    !RR DWA inflow - DWA in m3/s
            ! If a stack overflow occurs due to this array operation, use a do loop
            if (NVHG > 0) then
                Values(1:NVHG) = RslMap1_vhg(9, 1:NVHG, 1)
            else
                success = .false.
            endif
    case(RRiSurfaceRWA)
    !RR Surface RWA in m3/s
            ! If a stack overflow occurs due to this array operation, use a do loop
            if (NVHG > 0) then
                Values(1:NVHG) = RslMap1_vhg(10, 1:NVHG, 1)
            else
                success = .false.
            endif
    case (RRiRWA2DWA)
    !RR RWA inflow - DWA in m3/s
            ! If a stack overflow occurs due to this array operation, use a do loop
            if (NVHG > 0) then
                Values(1:NVHG) = RslMap1_vhg(11, 1:NVHG, 1)
            else
                success = .false.
            endif
    case(RRiSpillingRWA)
    !RR Spilling RWA in m3/s
            ! If a stack overflow occurs due to this array operation, use a do loop
            if (NVHG > 0) then
                Values(1:NVHG) = RslMap1_vhg(12, 1:NVHG, 1)
            else
                success = .false.
            endif
    case(RRiPumpedRWA)
    !RR Pumped RWA in m3/s
            ! If a stack overflow occurs due to this array operation, use a do loop
            if (NVHG > 0) then
                Values(1:NVHG) = RslMap1_vhg(13, 1:NVHG, 1)
            else
                success = .false.
            endif
    case(RRiSpillingDWA)
    !RR Spilling DWA in m3/s
            ! If a stack overflow occurs due to this array operation, use a do loop
            if (NVHG > 0) then
                Values(1:NVHG) = RslMap1_vhg(14, 1:NVHG, 1)
            else
                success = .false.
            endif
    case(RRiPumpedDWA)
    !RR Pumped DWA in m3/s
            ! If a stack overflow occurs due to this array operation, use a do loop
            if (NVHG > 0) then
                Values(1:NVHG) = RslMap1_vhg(15, 1:NVHG, 1)
            else
                success = .false.
            endif
    case(RRiEvaporationSurface)
    !RR Evaporation at surface in m3/s
            ! If a stack overflow occurs due to this array operation, use a do loop
            if (NVHG > 0) then
                Values(1:NVHG) = RslMap1_vhg(16, 1:NVHG, 1)
            else
                success = .false.
            endif
    case (RRiStorageVolDyn)
    ! RR Vol. Dyn. Storage in mm
            ! If a stack overflow occurs due to this array operation, use a do loop
            if (NVHG > 0) then
                Values(1:NVHG) = RslMap1_vhg(17, 1:NVHG, 1)
            else
                success = .false.
            endif
    case default
    ! Something is wrong
        success = .false.
    end select

  end function

end module Paved
