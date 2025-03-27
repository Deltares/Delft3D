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


      module Greenhouse

      use Conf_Fil
      use Conf_Arr
      use Network
      use Crop
      use Openwater
      use Boundary
      use Nwrw
! Feb 2002 NewFormatKasdata
      use NewTables
      use dh_alloc
      use ReadLib


! *** Data kasgebieden
! AREAKK = totaal areaal per kasklasse
! AREABK = bassin areaal per kasklasse
! BMXDAK = max. berging op daken
! LVLKAS = maaiveld (niet gebruikt)
! AREAS  = areaal met silo, default=0
! SILOC  = capaciteit silo (in file in m3/ha ipv m3/m2, in UI in m3/ha)
! PMPCAP = pompcapaciteit silo (m3/s)

! omvormen tot de volgende structures als volgt
! type Greenhouse
!
! end type Greenhouse
!
! type GreenhouseClass
!   real area       ! area of greenhouse-class
!
! end type GreenhouseClass

      implicit none

      REAL, Pointer, SAVE ::  AREAKK(:,:), BMXDAK(:), &
                                  AREABK(:,:), LVLKAS(:), &
                                  AREAS (:), SILOC(:), PMPCAP(:)

      Integer, Pointer :: numberKKl(:)

! *** results kasgebied
! ***
!     BKAS0  = initiele berging bassins in m3
!     BKAS   = finale berging bassins in m3
!     BKASD0 = initiele berging kasdaken in m3
!     BKASD  = finale berging kasdaken in m3
!     QKAS   = outflow per kas/kasklasse in m3/s
!     RKD    = regenval op daken per kasgebied in m3
!     VKD    = verdamping van daken per kasgebied in m3
!     RKB    = regenval op bassins per kasgebied in m3
!     VKB    = verdamping van bassins per kasgebied in m3
!     INK    = inflow naar kas/kasklasse in m3
!     GEBR   = gebruik per kasgebiedsklasse in m3
!     GEBRKL = typisch gebruik voor alle kasklassen in m3/ha
!     SILOB0 = initiele berging in silo in m3
!     SILOB  = finale berging in silo
!     INKS   = inflow in silo (vanaf daken)
!     QSILGW = debiet van silo naar grondwater (via pompje)
!     QSILOW = debiet van silo naar open water (overstort)

      REAL, Pointer, SAVE ::  BKAS0 (:,:),BKAS (:,:), &
                              BKASD0(:),  BKASD(:), &
                              QKAS  (:,:), &
                              RKD   (:),  VKD(:), &
                              RKB   (:,:),VKB (:,:), &
                              INK   (:,:), GEBR  (:,:), &
                              SILOB0(:), SILOB(:), INKS(:), &
                              QSILGW(:), QSILOW(:)
      REAL  GEBRKL
! ivm ARS 2978 een variabele GreenhouseYear om aan te geven hoe met initialisatie om te gaan (welk jaar te nemen?)
      Integer GreenhouseYear


! *** Data kasklassen
! *** KKLCOD = code
! *** KKLNAM = naam
! *** KKLDPT = max. diepte
! *** KKLOPN = indicator open/dichte berging (wel of geen verdamping)
! *** KKLMXB = max.berging

      LOGICAL, Pointer, SAVE ::      KKLOPN(:)
      INTEGER, Pointer, SAVE ::      KKLCOD(:)
      REAL, Pointer, SAVE ::         KKLDPT(:), KKLMXB(:)
      CHARACTER(Len=20), Pointer, SAVE :: KKLNAM(:)

 ! Greenhouse output
  ! *** KSMBPC = maximum bergingspercentage per kasklasse, per event
  ! *** KSMQOU = maximum outflow per kasklasse, per event
  ! ***            NB. Index NCKKL+1: voor totalen over kasklassen
  ! ***           ,1 = max. flow
  ! ***           ,2 = max. rain (voorheen KSMRIN)
  ! ***           ,3 = max. evap
  ! ***           ,4 = max. water use


   REAL, Pointer, SAVE ::  KSMBPC(:,:,:), KSMQOU(:,:,:,:)

! Feb 2002      Kas data files in new format, with choice of Kasklasse definition, KasInit, Kasgebr
      Logical       NewFormatKasdata
      Character(Len=CharIdLength) KasDefinition
      Character(Len=CharIdLength) KasKlassDef, KasInitDef, KasGebrDef
      Integer       KasInitTableNr, KasGebrTableNr


      contains

      subroutine Greenhouse_confAr1

      implicit none
      ! variables
      Integer in, iOut1
      Logical Success

      iOut1 = ConfFil_get_iOut1()

      NKAS = MAX (1, NCKAS  ) !greenhouse

      !kasklasse
!     write(*,*) ' GreenhouseConfar1 NewFormatKasData=',NewFormatKasData
      If (NewFormatKasData) then
         CALL OPENFL(IN, ConfFil_get_NAMFIL(108),1,1)
      Else
         CALL OPENFL(IN, ConfFil_get_NAMFIL(12),1,1)
      Endif
      CALL RDKKLS (IN, 1)
      CLOSE(IN)

      NKKL = MAX (1, NCKKL )

      IF ((NCKAS .GT. 0) .and. (iOut1 .ne. 0)) then
         WRITE(IOUT1,*) ' Greenhouse nodes      =',NKAS
         if ((NKKL .GT. 0) .and. (iOut1 .ne. 0)) then
             WRITE(IOUT1,*) ' Greenhouse classes    =',NKKL
         endif
      endif

      !*** Data kasgebieden

      Success = Dh_AllocInit (NKas, BMxDak, LvlKas, Areas, 0E0)
      Success = success .and. Dh_AllocInit (NKas, Nkkl, AreaBk, 0E0)
      Success = success .and. Dh_AllocInit (NKas, SiloC, PmpCap, 0E0)
      Success = success .and. Dh_AllocInit (NKas, NumberKkl, 10)
      if (.not. success) call ErrMsgStandard (981, 0, ' Error allocating arrays in subroutine ',' Greenhouse_ConfAr1')
!      ALLOCATE ( BMXDAK(NKAS), &
!                 AREABK(NKAS,NKKL), LVLKAS(NKAS), &
!                 AREAS (NKAS)     , SILOC (NKAS), PMPCAP(NKAS), &
!                 numberKkl(NKas), Stat=Allocation_Error )
!     If (Allocation_Error .ne. 0) &
!        call ErrMsgStandard (981, Allocation_Error, ' Error allocating arrays in subroutine ', &
!                                            ' GreenHouse_ConfAr1' )

            !*** Results kasgebied

      Success = success .and. Dh_AllocInit (NKas, NKkl, BKas, BKas0, QKas, 0E0)
      Success = success .and. Dh_AllocInit (NKas, BKasD, BKasD0, 0E0)
      Success = success .and. Dh_AllocInit (NKas, Rkd, Vkd, 0E0)
      Success = success .and. Dh_AllocInit (NKas, NKkl, Rkb, Vkb, 0E0)
      Success = success .and. Dh_AllocInit (NKas, NKkl, Gebr, Ink, 0E0)
      Success = success .and. Dh_AllocInit (NKas, SiloB, SiloB0, 0E0)
      Success = success .and. Dh_AllocInit (NKas, QSilGw, QSilOw, 0E0)
      Success = success .and. Dh_AllocInit (NKas, INKS, 0E0)
      Success = success .and. Dh_AllocInit (NKas, NKkl, AreaKK, 0E0)
      if (.not. success) call ErrMsgStandard (981, 0, ' Error allocating arrays in subroutine ',' Greenhouse_ConfAr1')
!      ALLOCATE  ( BKAS0 (NKAS,NKKL), BKAS (NKAS,NKKL), &
!                  BKASD0(NKAS),  BKASD(NKAS), &
!                  QKAS  (NKAS,NKKL), &
!                  RKD   (NKAS),  VKD(NKAS), &
!                  RKB   (NKAS, NKKL), VKB(NKAS,NKKL), &
!                  INK(NKAS,NKKL), GEBR  (NKAS,NKKL), &
!                  SILOB0(NKAS), SILOB (NKAS), INKS(NKAS), &
!                  QSILGW(NKAS), QSILOW(NKAS), Stat=Allocation_Error )
!      Allocate (areaKK(ncKAS, nKKL), Stat=Allocation_Error )


            !*** Data kasklassen

      Success = success .and. Dh_AllocInit (NKkl, KKLOPN, .false.)
      Success = success .and. Dh_AllocInit (NKkl, KKLCod, 0)
      Success = success .and. Dh_AllocInit (NKkl, KKLDPT, KklMxB, 0E0)
      Success = success .and. Dh_AllocInit (NKkl, KKLNAM, ' ')
      if (.not. success) call ErrMsgStandard (981, 0, ' Error allocating arrays in subroutine ',' Greenhouse_ConfAr1')
!     ALLOCATE ( KKLOPN(NKKL), Stat=Allocation_Error )
!     ALLOCATE ( KKLCOD(NKKL), Stat=Allocation_Error )
!     ALLOCATE ( KKLDPT(NKKL), KKLMXB(NKKL), Stat=Allocation_Error )
!     ALLOCATE ( KKLNAM(NKKL), Stat=Allocation_Error )

      Return
      end subroutine Greenhouse_confAr1


  subroutine GreenhouseOutput_Confar (Nevnt)

    Implicit None
    Integer Nevnt
    Logical Success

    Success = Dh_AllocInit (NCKas, NCKKL+1, Nevnt, KSMBPC, 0E0)
    Success = success .and. Dh_AllocInit (NCKas, NCKKL+1, Nevnt, 4, KSMQOU, 0E0)
    if (.not. success) call ErrMsgStandard (981, 0, ' Error allocating arrays in subroutine ',' Greenhouse_OutputConfAr')
!   ALLOCATE  ( KSMBPC(NCKAS,NCKKL+1,NEvnt), KSMQOU(NcKAS,NcKKL+1,NEvnt,4), Stat=Allocation_Error )


    Return
  End subroutine GreenhouseOutput_Confar





      subroutine Greenhouse_readAsciiInput(Infile1, Infile2, Infile3)

      ! return value
      Integer :: RetVal

      ! variables
      Integer(4) Infile1, Infile2, Infile3
      Integer teller, teller1, teller2, in, ikas, index, inod, idebug, iecode, iout1
      Character(Len=CharIdLength) name
      Character(Len=1000) string
      Logical         allow, found, endfil, Err969
      Integer         NHLP
      Parameter       (NHLP=32)
      Integer         IDUM(NHLP)
      REAL            RDUM(NHLP)
      Character(Len=CharIdLength)   CDUM(NHLP), NodeId
      Real            BmxDakDum,silocdum, pmpcapdum

      Character(Len=CharIdLength), Pointer :: STODEF(:), SILDEF(:)
      Real, Pointer :: AREATOT(:)

      Logical, Pointer :: AlreadyRead(:)
      Integer, Pointer :: ReferenceToDefinition(:)
      Logical Success

      Character(Len=CharIdLength)  FileName
      Integer                      IoUnit

      Success = Dh_AllocInit (NCKas, AlreadyRead, .false.)
      Success = success .and. Dh_AllocInit (NCKas, ReferenceToDefinition, 0)
      if (.not. success) call ErrMsgStandard (981, 0, ' Error allocating arrays in subroutine ',' Greenhouse_ReadAscii')
!     ALLOCATE   (AlreadyRead(NCKas), Stat=Allocation_Error )
!     ALLOCATE   (ReferenceToDefinition(NCKas), Stat=Allocation_Error )

      allow = .false.
      found = .false.
      iOut1  = ConfFil_get_iOut1 ()
      iDebug = ConfFil_get_iDebug()

! AREATOT is nodig voor bepaling Maximale berging op daken in m3
       Success = success .and. Dh_AllocInit (NKas, StoDef, SilDef, ' ')
       Success = success .and. Dh_AllocInit (NKas, AreaTot, 0E0)
       if (.not. success) call ErrMsgStandard (981, 0, ' Error allocating arrays in subroutine ',' Greenhouse_ReadAscii')
!      ALLOCATE  (STODEF(NKas), SILDEF(NKas), AREATOT(NKas), Stat=Allocation_Error )

! Vector/Array initialisation
!      Numberkkl = 10
!      AREAKK = 0
!      AREABK = 0
!      BMXDAK = 0
!      LVLKAS = 0
!      AREAS  = 0
!      SILOC  = 0
!      PMPCAP = 0
!      AlreadyRead = .false.
!      StoDef = ''
!      SilDef = ''
! einde initialisatie

! *********************************************************************
! ***  If CleanRRFiles, also write cleaned input
! *********************************************************************
   if (CleanRRFiles) then
        FileName = ConfFil_get_namFil(35)
        FileName(1:) = Filename(1:Len_trim(FileName)) // '_cleaned'
        Call Openfl (iounit, FileName,1,2)  !greenhse.3b_cleaned
        Call ErrMsgStandard (999, 1, ' Cleaning Greenhse.3b for RR-greenhouse input to file', FileName)
   endif

! *********************************************************************
! read file greenhse.3b
! *********************************************************************
      call SetMessage(LEVEL_DEBUG, 'Read Greenhouse.3b file')
      teller = 0
      RetVal = 0
      Endfil = .false.
      CALL SKPCOM (Infile1, ENDFIL,'ODS')
      do while (.not. endfil)
        READ(infile1,'(A1000)',END=21,ERR=150,IOSTAT=IECODE) STRING
! Skip regel als hij niet begin met juist keyword (PAVE)
        IF (STRING(1:4) .EQ. 'GRHS') Then
! greenhouse node id
          Retval = RetVal + GetVAR2 (STRING,' id ',1,' greenhouse_readAscii',' geenhse.3b file',IOUT1, &
                        CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
          index = 0
          call fndNd2(index, cdum(1))
          if (index .gt. 0) then      ! knoop bestaat
           inod = index
           index = EiNode(inod,2)
           if (alreadyRead(index)) then
             call SetMessage(LEVEL_ERROR, 'Data for greenhouse node '//cdum(1)(1:Len_trim(Cdum(1)))//' double in datafile Greenhse.3B')
           else
! cleaning RR files
            If (CleanRRFiles) write(Iounit,'(A)') String (1:len_trim(String))

            AlreadyRead(index) = .true.
            teller = teller + 1
! number of areas = 10 by default
!           Numberkkl(index) = 10
!           Retval = RetVal + GetVAR (STRING,' na ',3,' greenhouse_readAscii',' geenhse.3b file',IOUT1, &
!                        CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND)
!           Numberkkl(index) = idum(1)
! area
            Retval = RetVal + GetVRS2 (STRING,' ar ',2,' greenhouse-ReadAscii',' greenhse.3b file', &
                          IOUT1, CDUM(1), RDUM(1), IDUM(1), Numberkkl(nkas), IflRtn)
            AREATOT(index) = 0
            DO teller1=1,Numberkkl(nkas)
               AREAKK(index,teller1) = RDUM(teller1)
               AREATOT(index) = AREATOT(index) + RDUM(teller1) !totaal kasoppervlak per knoop
            ENDDO
! greenhouse area connected to silo storage
            Retval = RetVal + GetVAR2 (STRING,' as ',2,' greenhouse_readAscii',' geenhse.3b file',IOUT1, &
                          CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
            AREAS(index) = RDUM(1)
! surface level
            Retval = RetVal + GetVAR2 (STRING,' sl ',2,' greenhouse_readAscii',' geenhse.3b file',IOUT1, &
                          CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
            LVLKAS(index) = RDUM(1)
! storage identification
            Retval = RetVal + GetVAR2 (STRING,' sd ',1,' Greenhouse_readAscii',' greenhse.3b file',IOUT1, &
                          CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
            STODEF(index) = CDUM(1)
! silo identification
            Retval = RetVal + GetVAR2 (STRING,' si ',1,' Greenhouse_readAscii',' greenhse.3b file',IOUT1, &
                             CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
            SILDEF(index) = CDUM(1)
! Meteo station id
            Retval = RetVal + GetVAR2 (STRING,' ms ',1,' Greenhouse_readAscii',' greenhse.3b file',IOUT1, &
                          CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
            NAMMET(inod) = CDUM(1)
! areal adjustment factor rainfall on node, maybe missing,
            allow = .true.
            Retval = RetVal + GetVAR2(STRING,' aaf ',2,' Greenhouse-readAscii',' greenhse.3b file',IOUT1, &
                             CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, Iflrtn)
            if (found) AAFNodeRainfall(inod) = max(0.0, RDUM(1))    ! AAF >= 0
            allow = .false.
! initial salt concentration
            Retval = RetVal + GetVAR2 (STRING,' is ',2,' Greenhouse_readAscii',' greenhse.3b file',IOUT1, &
                          CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
            SltIni(inod) = RDUM(1)
           endif
          endif
        endif
        CALL SKPCOM (Infile1, ENDFIL,'ODS')
      enddo
 21   continue
      If (RetVal .gt. 0) call ErrMsgStandard (972, 0, ' Error reading Greenhouse.3B file ', ' Error getting GRHS records')
      If (teller .lt. NcKas)  Then
        Do inod=1,NcNode
          ikas = EiNode(inod,2)
          if (EiNode(inod,3) .eq. 3) then   ! en is greenhouse knoop
            if (.not. AlReadyRead(ikas)) then
              NodeId = ' '
              NodeId = Id_Nod(inod)
              String = ' '
              String = ' ' // NodeId(1:Len_trim(NodeId)) // ' is missing'
              call ErrMsgStandard (977, 0, ' Data for greenhouse node ',String(1:Len_trim(String)) )
            endif
          endif
        Enddo
        call ErrMsgStandard (972, 0, ' Not enough Greenhouse data found', &
                           ' Some greenhouse nodes in netwerk schematisation are not present in Greenhse.3b file')
      Endif
! cleaning RR files
   If (CleanRRFiles) Call closeGP (Iounit)

! *********************************************************************
! ***  If CleanRRFiles, also write cleaned input for greenhse.rf
! *********************************************************************
   if (CleanRRFiles) then
        FileName = ConfFil_get_namFil(36)
        FileName(1:) = Filename(1:Len_trim(FileName)) // '_cleaned'
        Call Openfl (iounit, FileName,1,2)  !greenhse.rf_cleaned
        Call ErrMsgStandard (999, 1, ' Cleaning Greenhse.rf for RR-greenhouse input to file', FileName)
   endif

! *********************************************************************
! Read greenhse.rf file
! *********************************************************************
      call SetMessage(LEVEL_DEBUG, 'Read Greenhouse.rf file')
!     Vector/Array initialisation
      ReferenceToDefinition = 0
      teller = 0
      RetVal = 0
      Endfil = .false.
      CALL SKPCOM (Infile2, ENDFIL, 'ODS')
      do while (.not. endfil)
         READ (Infile2,'(A1000)',END=211,ERR=150,IOSTAT=IECODE) STRING
         IF (STRING(1:4) .EQ. 'STDF') Then
! Read storage id
           teller = teller + 1
           Retval = RetVal + GetVAR2 (STRING,' id ',1,' greenhse_readAscii',' greenhse.sto file',IOUT1, &
                         CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
           name = CDUM(1)
! Read maximum storage on roofs (in mm)
           Retval = RetVal + GetVAR2 (STRING,' mk ',2,' greenhse_readAscii',' greenhse.rf file',IOUT1, &
                         CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
           bmxdakdum = RDUM(1)
! Initial storage by default equal to zero

! Assign definition to individual nodes
           Do iKas = 1, ncKas
              if (StringComp(StoDef(Ikas), Name, CaseSensitive) )  then
                  if (ReferenceToDefinition(iKas) .gt. 0) then
                     ! double !! skip last definition
                  else
                    ! cleaning RR files
                    If (CleanRRFiles) write(Iounit,'(A)') String (1:len_trim(String))

                    ReferenceToDefinition(iKas) = teller
                    BMXDAK(iKas) = bmxDAKdum * AREATOT(iKas) / 1000 !in m3
                  endif
              endif
           Enddo
        endif
        CALL SKPCOM (Infile2, ENDFIL, 'ODS')
      Enddo
  211 Continue

      If (RetVal .gt. 0) call ErrMsgStandard (972, 0, ' Error reading Greenhouse.sto file ', ' Error getting STDF records')
      Err969 = .false.
      Do iKas = 1, ncKas
        If (ReferenceToDefinition(iKas) .eq. 0) Then
            Err969 = .true.
            call ErrMsgStandard (969, 0, ' Some Roof Storage definitions not present in Greenhouse.rf file.',StoDef(ikas))
        Endif
      Enddo
      If (Err969) call ErrMsgStandard (972, 0, ' Not enough Greenhouse data found',&
                                     ' Some Roof Storage definitions not present in Greenhouse.rf file')

! cleaning RR files
   If (CleanRRFiles) Call closeGP (Iounit)

! *********************************************************************
! ***  If CleanRRFiles, also write cleaned input for greenhse.rf
! *********************************************************************
   if (CleanRRFiles) then
        FileName = ConfFil_get_namFil(48)
        FileName(1:) = Filename(1:Len_trim(FileName)) // '_cleaned'
        Call Openfl (iounit, FileName,1,2)  !greenhse.sil_cleaned
        Call ErrMsgStandard (999, 1, ' Cleaning Greenhse.sil for RR-greenhouse input to file', FileName)
   endif

! *********************************************************************
! Read greenhse.sil file
! *********************************************************************
      call SetMessage(LEVEL_DEBUG, 'Read Greenhouse.Sil file')
!     Vector/Array initialisation
      ReferenceToDefinition = 0
      teller = 0
      RetVal = 0
      Endfil = .false.
      CALL SKPCOM (Infile3, ENDFIL, 'ODS')
      do while (.not. endfil)
         READ (Infile3,'(A1000)',END=2111,ERR=150,IOSTAT=IECODE) STRING
         IF (STRING(1:4) .eq. 'SILO') Then
! Read silo id
           teller = teller + 1
           Retval = RetVal + GetVAR2 (STRING,' id ',1,' greenhse_readAscii',' greenhse.sil file',IOUT1, &
                         CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
           name = CDUM(1)
! Read silo capacity m3/ha in file, convert to m3/m2 (ARS 14685)
           Retval = RetVal + GetVAR2 (STRING,' sc ',2,' greenhse_readAscii',' greenhse.sil file',IOUT1, &
                         CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
           silocdum = RDUM(1) * 0.0001
! Read silo pump capacity m3/s
           Retval = RetVal + GetVAR2 (STRING,' pc ',2,' greenhse_readAscii',' greenhse.sil file',IOUT1, &
                         CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
           pmpcapdum = RDUM(1)
! Assign definition to individual nodes
           Do iKas = 1, ncKas
              if (StringComp(SilDef(Ikas), Name, CaseSensitive) )  then
                  if (ReferenceToDefinition(iKas) .gt. 0) then
                     ! double !! skip last definition
                  else
                    ! cleaning RR files
                    If (CleanRRFiles) write(Iounit,'(A)') String (1:len_trim(String))

                    ReferenceToDefinition(ikas) = teller
                    SILOC(iKas) = silocdum
                    PMPCAP(iKas) = pmpcapdum
                  endif
              endif
           Enddo
         endif
         CALL SKPCOM (Infile3, ENDFIL, 'ODS')
      Enddo
 2111 Continue
!   Check if all silo references are resolved
      Err969 = .false.
      Do iKas = 1, ncKas
        if (ReferenceToDefinition(iKas) .eq. 0 .and. AreaS(ikas) .gt. 0) Then
           Err969 = .true.
           call ErrMsgStandard (969, 0, ' Some Silo Storage definitions not present in Greenhouse.sil file.',SilDef(ikas))
        Endif
      Enddo
      If (RetVal .gt. 0) call ErrMsgStandard (972, 0, ' Error reading Greenhouse.sil file ', ' Error getting SILO records')
      If (Err969) call ErrMsgStandard (969, 0, ' Not enough Greenhouse data found',&
                                     ' Some Silo definitions not present in Greenhouse.Sil file')

! cleaning RR files
   If (CleanRRFiles) Call closeGP (Iounit)

! reading kasklasse-data
!     write(*,*) ' Greenhouse ReadAscii NewFormatKasData=',NewFormatKasData
      If (NewFormatKasData) then
         CALL OPENFL(IN, ConfFil_get_NAMFIL(108),1,1)
      Else
         CALL OPENFL(IN, ConfFil_get_NAMFIL(12),1,1)
      Endif
      CALL RDKKLS (IN, 2)
      CLOSE(IN)

! calculation of areaBK
      do ikas = 1, ncKas
         do teller2 = 1, NCKKL
            IF (KKLDPT(teller2) .GT. 0)  THEN
                AREABK (IKAS,teller2) = KKLMXB(teller2) * areaKK(IKAS, teller2) / KKLDPT(teller2)
            ELSE
                AREABK (IKAS,teller2) = 0.0
            ENDIF
         enddo
      enddo

      DEALLOCATE (STODEF,SILDEF)
      Deallocate (AlreadyRead)
      Deallocate (ReferenceToDefinition)
      Return

  150 CONTINUE
      call SetMessage(LEVEL_FATAL, 'Read error in Greenhouse ascii')

      Return
      end subroutine Greenhouse_readAsciiInput





      SUBROUTINE RDKKLS (IN, ICALL)
! *********************************************************************
! *** Last update: March  1995       By : Geert Prinsen
! *********************************************************************
! *** Brief description:
! *** ------------------
! ***   Inlezen kasklasse gegevens
! *********************************************************************
! *** Input/output parameters:
! *** ------------------------
! ***  IDEBUG = file unit number of debug file
! ***  IN     = file unit number of input file
! ***  IOUT1  = file unit number of output file with messages
! *********************************************************************

      Integer :: RetVal

      INTEGER      IN, ICALL, iECode, i, iCode, iOpN
      Real         bMax, bDepth
      LOGICAL      ENDFIL
      CHARACTER(Len=20) NAME
      Integer       iDebug, Iout1
! Feb 2002
      Logical       Allow, Found, TabYesNo, Success
      Integer       NHLP, teller1, len1, len2
      Parameter     (NHLP=25)   ! moet >= NCKKL zijn
      Integer       IDUM(NHLP)
      Real          RDUM(NHLP)
      Character(Len=CharIdLength) CDUM(NHLP), TableName
      Character(Len=9999) BufString


      iDebug = ConfFil_get_iDebug()
      iOut1 = ConfFil_get_iOut1()

      IF (iDebug .ne. 0) WRITE (IDEBUG,1)
    1 FORMAT (' RDKKLS')

! *********************************************************************
! *** skip header of file
! *********************************************************************

      CALL SKPCOM (IN, ENDFIL, 'ODS ')
      IF (ENDFIL) &
       call ErrMsgStandard (911, IECODE, 'Rdkkl', ' Greenhouse-classes file')

! *********************************************************************
! *** read data: maximaal NKKL
! ***  per kasklasse de naam en maximale berging in m3/ha.
! *********************************************************************

!     write(*,*) ' Greenhouse RdKKls NewFormatKasData=',NewFormatKasData
      RetVal = 0
      If (.not. NewFormatKasdata) then
         NCKKL = 0
         DO I=1,9999
            READ(IN,*,END=30,ERR=150,IOSTAT=IECODE) ICODE, NAME, BMAX, &
                                                    BDEPTH, IOPN
            NCKKL  = NCKKL  + 1
            IF (ICALL .EQ. 2) THEN
              KKLCOD (NCKKL) = ICODE
              KKLNAM (NCKKL) = NAME
              KKLMXB (NCKKL) = BMAX / Float(HA2M)
              KKLDPT (NCKKL) = BDEPTH
              KKLOPN (NCKKL) = .TRUE.
              IF (IOPN .NE. 1)  KKLOPN (NCKKL) = .FALSE.
            ENDIF
         enddo
         GOTO 30
      Else
!        New format KasKlasse, KasInit and KasGebr file
         Allow = .false.
         found = .false.
         RetVal = 0
         Idebug = Conffil_Get_Idebug()
!        Write(*,*) ' RdKkls New format, look for KasDefinition:', KasDefinition(1:Len_trim(KasDefinition))
         Do while (.not. endfil)
            Success = GetRecord (In,'KASD', Endfil, Idebug, Iout1)  ! get record van keyword KASD until kasd
            If (Endfil) goto 30
            Success = GetStringFromBuffer(BufString)
            if (.not. Success) goto 150
            if (idebug .ne. 0) Write(idebug,*) ' Read buffer: ', BufString(1:100)
            Success = GetTableName (TabYesNo, TableName, ' id ', IOut1)
            if (.not. Success) goto 150
            if (idebug .ne. 0) Write(idebug,*) ' TableName got from buffer: ', &
                                                 TableName(1:Len_trim(TableName))
            Call UpperC (TableName)   ! Id put in UPPERCASE, since all data from INI file also converted to UPPERCASE
            Len1 = Len_trim(TableName)
            Len2 = Len_trim(KasDefinition)
            if (idebug .ne. 0) Write(idebug,*) ' TableName length and KasDefinition length', len1, len2
!           Write(*,*) ' TableName got from buffer: ', TableName(1:Len_trim(TableName))
!           Write(*,*) ' TableName length and KasKlassDefinition length', len1, len2
            If (TableName(1:Len1) .eq. KasDefinition(1:Len2)) then
! specified Kas definition (e.g. 'Default', 'Parbo', or 'Taiwan' found
! get id's of kasklasse definition, kasinitialisation and kasgebruik
!             Write(*,*) ' RdKkls New format, KasDefinition found: ', KasDefinition(1:Len_trim(KasDefinition))
              if (idebug .ne. 0) Write(idebug,*) ' TableName = KasDefinition found!: ',&
                                 KasDefinition(1:Len_trim(KasDefinition))
              TabYesNo = .true.
! keyword nc = aantal kasklassen
              Retval = RetVal + GetVAR2 (BufString(1:nbuf),' nc ',3,' RDKkls-Kassen',' KasKlasse file',IOUT1, &
                            CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
              NCKKL = IDUM(1)
              if (idebug .ne. 0) Write(idebug,*) ' NCKkl = ', NCKkl
              IF (NCKKL .gt. NKKL .and. icall .eq. 2) &
                  call ErrMsgStandard (912, IECODE, '  RdKkls', ' Number of greenhouse classes')
              IF (ICALL .EQ. 1) GOTO 999
              KasKlassDef = ' '
              Success = GetTableName (TabYesNo, KasKlassDef, ' kk ', IOut1)
              if (.not. Success) goto 150
              if (idebug .ne. 0) Write(idebug,*) ' KasKlassDef ', KasKlassDef
              KasInitDef = ' '
              Success = GetTableName (TabYesNo, KasInitDef, ' ki ', IOut1)
              if (.not. Success) goto 150
              if (idebug .ne. 0) Write(idebug,*) ' KasInitDef ', KasInitDef
              KasGebrDef = ' '
              Success = GetTableName (TabYesNo, KasGebrDef, ' kg ', IOut1)
              if (.not. Success) goto 150
              if (idebug .ne. 0) Write(idebug,*) ' KasGebrDef ', KasGebrDef
! get kasklasse data
              Do While (.not. endfil)
                Success = GetRecord (In,'KLAS', Endfil, Idebug, Iout1)  ! get record van keyword KLAS tot klas
                if (.not. Success) goto 150
                 !! two lines added Taiwan Oct 2007
                Success = GetStringFromBuffer(BufString)
                if (.not. Success) goto 150
                 !! end Taiwan Oct 2007
                if (idebug .ne. 0) Write(idebug,*) ' Read buffer: ', BufString(1:100)
                Success = GetTableName (TabYesNo, TableName, ' id ', IOut1)
                if (.not. Success) goto 150
                if (idebug .ne. 0) Write(idebug,*) ' TableName got from buffer: ', &
                                                     TableName(1:Len_trim(TableName))
                if (idebug .ne. 0) Write(idebug,*) ' Test on KasKlassDef : ', KasKlassDef(1:100)
                Len1 = Len_trim(TableName)
                Len2 = Len_trim(KasKlassDef)
                if (idebug .ne. 0) Write(idebug,*) ' TableName length and KasKlassDefinition length', len1, len2
!               Write(*,*) ' TableName got from buffer: ', TableName(1:Len_trim(TableName))
!               Write(*,*) ' TableName length and KasKlassDefinition length', len1, len2
                If (TableName(1:Len1) .eq. KasKlassDef(1:Len2)) then
                   if (idebug .ne. 0) Write(idebug,*) ' TableName = KasKlassDef found!: ', KasKlassDef(1:Len_trim(KasKlassDef))
                   Retval = RetVal + GetVRS2 (BufString(1:nbuf),' nm ',1,' RdKkls ',' KasKlasse file', &
                                 IOUT1, CDUM(1), RDUM(1), IDUM(1), NCKKL, IflRtn)
                   Do teller1 = 1, NCKKL
                      KKLCOD (teller1) = teller1
                      KKLNAM (teller1) = CDUM(teller1)
                   Enddo
                   Retval = RetVal + GetVRS2 (BufString(1:nbuf),' mxstorage ',2,' RdKkls ',' Kasklasse file', &
                                 IOUT1, CDUM(1), RDUM(1), IDUM(1), NCKKL, IflRtn)
                   Do teller1 = 1, NCKKL
                      KKLMXB (teller1) = RDUM(teller1)/ Float(HA2M)
                   Enddo
                   Retval = RetVal + GetVRS2 (BufString(1:nbuf),' mxdepth ',2,' RdKkls ',' Kasklasse file', &
                                 IOUT1, CDUM(1), RDUM(1), IDUM(1), NCKKL, IflRtn)
                   Do teller1 = 1, NCKKL
                      KKLDPT (teller1) = RDUM(teller1)
                   Enddo
                   Retval = RetVal + GetVRS2 (BufString(1:nbuf),' evap ',3,' RdKkls ',' Kasklasse file', &
                                 IOUT1, CDUM(1), RDUM(1), IDUM(1), NCKKL, IflRtn)
                   Do teller1 = 1, NCKKL
                      KKLOPN (teller1) = .true.
                      if (Idum(teller1) .ne. 1) KKLOPN (teller1) = .false.
                   Enddo
                   TabYesNo = .true.
                   GOTO 999
                Endif
              Enddo
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
      call ErrMsgStandard (902, IECODE, 'Rdkkl', ' greenhouse-classes file')

! *********************************************************************
! *** end of file
! *********************************************************************

  30  CONTINUE
 999  CONTINUE
!     write(*,*) ' End of RdKkls RetVal=', RetVal
      If (RetVal .gt. 0) call ErrMsgStandard (972, 0, ' Error reading New KasKlasse file ', ' Error getting KLAS records')

!     IF (ICALL .EQ. 2) THEN
!        WRITE (*,*) ' Data greenhouse-classes'
!        WRITE (*,*) ' Iklasse    name    max_storage in m,', &
!                         '      max_depth ,    open/close'
!        DO I=1,NCKKL
!           WRITE(*,*) KKLCOD(I), KKLNAM(I), KKLMXB(I), &
!                           KKLDPT(I), KKLOPN(I)
!        ENDDO
!     ENDIF

      IF (iDebug .ne. 0 .AND. ICALL .EQ. 2) THEN
         WRITE (IDEBUG,*) ' Data greenhouse-classes'
         WRITE (IDEBUG,*) ' Iklasse    name    max_storage in m,', &
                          '      max_depth ,    open/close'
         DO I=1,NCKKL
            WRITE(IDEBUG,*) KKLCOD(I), KKLNAM(I), KKLMXB(I), &
                            KKLDPT(I), KKLOPN(I)
         ENDDO
      ENDIF

      RETURN
      END subroutine rdKkls



      SUBROUTINE RDKINI (IEvent)

! *********************************************************************
! *** Last update: March  1995       By : Geert Prinsen
! *********************************************************************
! *** Brief description:
! *** ------------------
! ***   Read kas initialisatie file (initiele berging)
! ***    nieuw: initiele vrije berging ipv initiele berging!
! *********************************************************************
! *** Input/output parameters:
! *** ------------------------
! ***  IDEBUG = file unit number of debug file
! ***  INKINI = file unit number of input file
! ***  IOUT1  = file unit number of output file with messages
! ***  IMode  = 1 read from file
! ***           2 read from array KasInitData
! *********************************************************************

       INTEGER       iCount, iECode, iKas, iKKl
       REAL          HELP(NKKL)
       CHARACTER(Len=CharIdLength) STRING
       Integer       iDebug, Iout1, IEvent

       Integer       RowNr
       logical       DateTimeOutsideTable

       type (Date) currentDate
       type (Time) currentTime

       String = ' '
       iOut1 = ConfFil_get_iOut1()

       iDebug = ConfFil_get_iDebug()

       IF (iDebug .ne. 0) WRITE (IDEBUG,1)
    1  FORMAT (' RDKINI')
       ICOUNT = 0

! *********************************************************************
! *** read initiele vrije berging (in m3/ha) en converteer naar m3
! *********************************************************************

!      write(*,*) ' Greenhouse RdKINI NewFormatKasData=',NewFormatKasData
       If (.not. NewFormatKasdata) then
!!!!      from KasInitdata
          DO IKKL=1,NCKKL
             HELP(Ikkl) = KasInitData (Ievent, Ikkl)
          ENDDO
       Else
        currentDate%year = ConfArr_get_IYear()
        if (GreenhouseYear .ne. -1) currentDate%year = GreenHouseYear
        currentDate%month = ConfArr_get_iMonth()
        currentDate%day = ConfArr_get_iDay()
        currentTime%hour = ConfArr_get_iHour()
        currentTime%minute = ConfArr_get_iMinute()
        currentTime%second = 0
        DO IKKL=1,NCKKL
           RowNr = -1
           HELP(ikkl) = GetNewValue(TableHandle, KasInitTableNr, ikkl, RowNr, CurrentDate, CurrentTime, &
                                    Idebug, iout1, DateTimeOutsideTable, .true.)
        ENDDO
        if (idebug .ne. 0) Write(idebugLunRR,*) ' GetNewValue KasInitData(I)', (Help(ikkl),ikkl=1,nckkl)
       Endif

! *** converteer vrije berging (in m3/ha) naar m3
       DO IKAS=1,NCKAS
          DO IKKL=1,NCKKL
             BKAS (IKAS,IKKL) = KKLMXB(IKKL) * AREAKK (IKAS,IKKL) - &
                                  HELP(IKKL) * AREAKK (IKAS,IKKL) / Float(HA2M)
                 BKAS (IKAS,IKKL) = MAX (BKAS(IKAS,IKKL), 0.0)
          ENDDO
       ENDDO

       Return

! *********************************************************************
! *** Error during reading of file
! *********************************************************************

  150  CONTINUE
       call ErrMsgStandard (902, IECODE, '  Rdkini', STRING)

! *********************************************************************
! *** end of file
! *********************************************************************

   30  CONTINUE
       call ErrMsgStandard (911, IECODE, '  Rdkini', STRING)


       RETURN
       END subroutine Rdkini





      SUBROUTINE RDGEBR (Ievent, DayInEvent)
! *********************************************************************
! *** Last update: March  1995       By : Geert Prinsen
! *********************************************************************
! *** Brief description:
! *** ------------------
! ***   Read greenhouse usage file
! *********************************************************************
! *** Input/output parameters:
! *** ------------------------
! ***  IDEBUG = file unit number of debug file
! ***  INGEBR = file unit number of input file
! ***  IOUT1  = file unit number of output file with messages
! *********************************************************************

       INTEGER       iCount, iECode
       CHARACTER(Len=CharIdLength) STRING
       Integer       iDebug, Iout1, DayInEvent, IEvent

       Integer       RowNr
       logical       DateTimeOutsideTable

       type (Date) currentDate
       type (Time) currentTime

       String = ' '
       iOut1 = ConfFil_get_iOut1()


       iDebug = ConfFil_get_iDebug()
       IF (iDebug .ne. 0) WRITE (IDEBUG,1)
    1  FORMAT (' RDGEBR')
       ICOUNT = 0

! *********************************************************************
! *** read data, convert from m3/ha/day to m/s.
! *********************************************************************

!    Data from KasgebruikData array
!      write(*,*) ' Greenhouse RdGebr NewFormatKasData=',NewFormatKasData
       if (.not. NewFormatKasData) then
          GebrKl = KasGebruikData(Ievent,DayInEvent)
       else
          currentDate%year = ConfArr_get_IYear()
          if (GreenhouseYear .ne. -1) currentDate%year = GreenHouseYear
          currentDate%month = ConfArr_get_iMonth()
          currentDate%day = ConfArr_get_iDay()
          currentTime%hour = ConfArr_get_iHour()
          currentTime%minute = ConfArr_get_iMinute()
          currentTime%second = 0
          RowNr = -1
          GebrKl = GetNewValue(TableHandle, KasGebrTableNr, 1, RowNr, CurrentDate, CurrentTime, &
                               Idebug, iout1, DateTimeOutsideTable, .true.)
       Endif

       IF (iDebug .ne. 0) WRITE(IDEBUG,*) ' Usage read in m3/ha/day', GEBRKL

       GEBRKL = GEBRKL / Float(HA2M) / Float(NRSDAY)

       Return

! *********************************************************************
! *** Error during reading of file
! *********************************************************************

  150 CONTINUE
      call ErrMsgStandard (902, IECODE, '  Rdgebr', STRING)

! *********************************************************************
! *** end of file
! *********************************************************************

   30 CONTINUE
      call ErrMsgStandard (911, IECODE, '  Rdgebr', STRING)


      RETURN
      END subroutine rdgebr


      SUBROUTINE ReadKasInit (INKINI)

! *********************************************************************
! *** Last update: February 2000
! *********************************************************************
! *** Brief description:
! *** ------------------
! ***   Read whole greenhouse initialisation file
! ***   Store data in array KasInitData (Nevent, NrTypesofKasKlasses)
! *********************************************************************

      INTEGER       INKINI, iCount, iECode, i, iKKl, IDefltl
      REAL          HELP(NKKL)
      LOGICAL       ENDFIL, YearNul
      CHARACTER(Len=CharIdLength) STRING
      Integer       iDebug, Iout1, Ievent, idum1, idum2, idum3
! Feb 2002
      Logical       Allow, Found, TabYesNo, Doorgaan, Success
      Integer       len1, len2
      Character(Len=CharIdLength) TableName


      String = ' '
      iDebug = ConfFil_get_iDebug()
      iOut1  = ConfFil_get_iOut1()
      iDefltl = Network_get_Default()


      IF (iDebug /= 0) WRITE (IDEBUG,1) IDefltl
    1 FORMAT (' ReadKasInit  IDeflt=',I3)

! *********************************************************************
! *** read initiele vrije berging (in m3/ha) en converteer naar m3
! *********************************************************************

      String = ' '
      String = ' Greenhouse initialisation file'
!     Read from file
!     write(*,*) ' Greenhouse RdKasInit NewFormatKasData=',NewFormatKasData
      If (.not. NewFormatKasdata) then
         Do Ievent=1,Nevent
           ICOUNT = 0
           If (Idefltl .eq. 1) then
             Rewind(InKini)
             Call SplFil (InKini, String)
           Endif
           Do i=1,6
              EvStrt(i) = EventStartDateTime(Ievent,i)
           Enddo
           String = ' '
           String = ' Greenhouse initialisation file'
           Call SplFl3(InKini,IEvent, GreenhouseYear, String,YearNul)
           Doorgaan = .true.
           Do While (Doorgaan)
              CALL SKPCOM (INKINI, ENDFIL, 'ODS ')
              STRING = ' kas-initialisatie file'
              IF (ENDFIL .AND. IDEFLTl .EQ. 0) THEN
                 If (YearNul) then
                   REWIND(INKINI)
                   Doorgaan = .true.
                 Else
                   call ErrMsgStandard (911, 0, '  ReadKasInit', STRING)
                 Endif
              ELSE
                Doorgaan = .false.
                IF (ENDFIL .AND. IDEFLTl .EQ. 1 .AND. ICOUNT .EQ. 0) THEN
                  ICOUNT = ICOUNT +1
                  REWIND(INKINI)
                  Doorgaan = .true.
                ENDIF
              ENDIF
           EndDo

           READ(INKINI,*,END=30,ERR=150,IOSTAT=IECODE) &
                                 IDUM1, Idum2, IDum3, (HELP(I),I=1,NCKKL)
           IF (iDebug .ne. 0) WRITE(IDEBUG,*) ' Init_greenhouse_storage', (HELP(I),I=1,NCKKL)
!          KasInit is door SplFl3 gezet op de juiste datum (Checkyear of EvStartDateYear, en juiste EvStart maand en dag)
           DO IKKL=1,NCKKL
              KasInitData(Ievent,Ikkl) = Help (Ikkl)
           ENDDO
         Enddo
      Else
!       New format KasInit file, find selected KasInitDef and read table in INIT records
!       Write(*,*) ' RdKasInit New format, look for KasInitDefinition:', KasInitDef(1:Len_trim(KasInitDef))
        CALL SKPCOM (INKini, ENDFIL, 'ODS ')
        Allow = .false.
        Found = .false.
        Do While (.not. Endfil)
           Success = GetRecord (Inkini,'INIT', Endfil, Idebug, Iout1)  ! get record van keyword INIT tot init
           If (Endfil) goto 30
           Success = GetTableName (TabYesNo, TableName, ' id ', IOut1)
           if (.not. Success) goto 150
           if (idebug .ne. 0) Write(idebug,*) ' TableName got from buffer: ', &
                                                TableName(1:Len_trim(TableName))
           Len1 = Len_trim(TableName)
           Len2 = Len_trim(KasInitDef)
           if (idebug .ne. 0) Write(idebug,*) ' TableName length and KasInitDef length', len1, len2
!          Write(*,*) ' TableName got from buffer: ', TableName(1:Len_trim(TableName))
!          Write(*,*) ' TableName length and KasInitDef length', len1, len2
           If (TableName(1:Len1) .eq. KasInitDef(1:Len2)) then
! specified KasInit definition found
! get table of KasInit values with name TableName, NrColumns=NcKkl, TableNr found=Table
!            Write(*,*) ' RdKasInit New format, KasInitDefinition FOUND:', KasInitDef(1:Len_trim(KasInitDef))
             if (idebug .ne. 0) Write(idebug,*) ' TableName = KasInitDef found!: ', KasInitDef
             Success = GetTable (TableHandle, TableName, NCKkl, KasInitTableNr, Idebug, Iout1)
             if (.not. Success) goto 150
             TabYesNo = .true.
             EndFil = .true.
           Else
              TabYesNo = .false.
           Endif
        Enddo
        If (.not. TabYesNo) GoTo 30
      Endif
      GOTO 999

! *********************************************************************
! *** Error during reading of file
! *********************************************************************

  150 CONTINUE
      call ErrMsgStandard (902, IECODE, '  ReadKasInit', STRING)

! *********************************************************************
! *** end of file
! *********************************************************************

   30 CONTINUE
      call ErrMsgStandard (911, IECODE, '  ReadKasInit', STRING)

  999 Continue
      RETURN
      END subroutine ReadKasInit


      SUBROUTINE ReadKasGebruik (INGEBR)
! *********************************************************************
! *** Last update: March 2000      Sobek-Parallell
! *********************************************************************
! *** Brief description:
! *** ------------------
! ***   Read greenhouse usage file completely
! ***   Store data in array KasGebruikData
! *********************************************************************
! *********************************************************************

      INTEGER       INGEBR, iCount, iECode, idum1, idum2, idum3, Idefltl
      LOGICAL       ENDFIL, LeapYear, YearNul
      CHARACTER(Len=CharIdLength) STRING
      Integer       iDebug, Iout1, IYear, IEvent, DayInEvent, i
! Feb 2002
      Logical       Allow, Found, TabYesNo, Doorgaan, Success
      Integer       len1, len2
      Character(Len=CharIdLength) TableName


      iDebug = ConfFil_get_iDebug()
      iOut1  = ConfFil_get_iOut1()
      iDefltl = Network_get_Default()

      IF (iDebug .ne. 0) WRITE (IDEBUG,1)
    1 FORMAT (' ReadKasGebruik')

! *********************************************************************
! *** read data, convert from m3/ha/day to m/s.
! *********************************************************************

      String = ' '
      String = ' Greenhouse water use file'
!     write(*,*) ' Greenhouse RdKasGebruik NewFormatKasData=',NewFormatKasData
      If (.not. NewFormatKasdata) then
        Do IEvent=1,NEvent
         ICOUNT = 0
         If (Idefltl .eq. 1) then
            Rewind(InGebr)
            Call SplFil (Ingebr, String)
         Endif
         Do i=1,6
            EvStrt(i) = EventStartDateTime(Ievent,i)
         Enddo
         Call SplFl3(InGebr,IEvent, GreenHouseYear, String, YearNul)
         Do DayInEvent=1, EventDuration(Ievent,5)
           Doorgaan = .true.
           Do While (Doorgaan)
              CALL SKPCOM (INGEBR, ENDFIL, 'ODS ')
              STRING = ' kas-gebruik file'
              IF (ENDFIL .AND. IDEFLTl .EQ. 0) THEN
                If (YearNul) then
                  REWIND(INGEBR)
                  Doorgaan = .true.
                else
                  call ErrMsgStandard (911, 0, ' ReadKasGebruik', STRING)
                Endif
              ELSE
                Doorgaan = .false.
!               IF (ENDFIL .AND. IDEFLTl .EQ. 1 .AND. ICOUNT .EQ. 0) THEN
                IF (ENDFIL .AND. IDEFLTl .EQ. 1 .AND. ICOUNT .LE. 9999) THEN
                  ICOUNT = ICOUNT +1
                  REWIND(INGEBR)
                  Doorgaan = .true.
                ENDIF
              ENDIF
           Enddo

           READ(INGEBR,*,END=30,ERR=150,IOSTAT=IECODE) IDUM1, IDUM2, IDUM3, GEBRKL
!     ARS 569: 29 feb always in the file; check on date always
!              means that 3 out of 4 years 29 feb has to be skipped;
           if (Idum2 .eq. 2 .and. idum3 .eq. 29) then
!             IYear = year read from evaporation file; either startyear event or Checkyear
              if (Idefltl .eq. 0 .or. GreenhouseYear .eq. -1) then
                 IYear = EventStartDateTime(IEvent,1)
              elseIf (GreenhouseYear .ne. -1) then
                 IYear = GreenhouseYear
              endif
              LeapYear = .false.
              If (IYear/4   *  4 .eq. IYear) LeapYear = .true.
              if (IYear/100 *100 .eq. IYear) LeapYear = .false.
              if (IYear/400 *400 .eq. IYear) LeapYear = .true.
              if (.NOT. LeapYear) Read (INGEBR,*,END=30,ERR=150,IOSTAT=IECODE) IDUM1, IDUM2, IDUM3, GEBRKL
           endif
!     End check ARS 569
           KasGebruikData(Ievent, DayInEvent) = Gebrkl
         Enddo
!     extra backspace noodzakelijk ivm dicht op elkaar vallende events
         Backspace(Ingebr)
!     ARS 11511: add an extra backspace necessary (see also reading Rainfall/Evaporation files in Meteomodule)
         Backspace(Ingebr)
        Enddo
      Else
!       New format KasGebr file, find selected KasGebrDef and read table in GEBR records
        CALL SKPCOM (INGebr, ENDFIL, 'ODS ')
        Allow = .false.
        Found = .false.
        Do While (.not. Endfil)
           Success = GetRecord (Ingebr,'GEBR', Endfil, Idebug, Iout1)  ! get record van keyword GEBR tot gebr
           If (Endfil) goto 30
           Success = GetTableName (TabYesNo, TableName, ' id ', IOut1)
           if (.not. Success) goto 150
           if (idebug .ne. 0) Write(idebug,*) ' TableName got from buffer: ', &
                                                TableName(1:Len_trim(TableName))
           Len1 = Len_trim(TableName)
           Len2 = Len_trim(KasGebrDef)
           if (idebug .ne. 0) Write(idebug,*) ' TableName length and KasGebrDef length', len1, len2
           If (TableName(1:Len1) .eq. KasGebrDef(1:Len2)) then
! specified KasGebr definition found
! get table of KasGebr values with name TableName, NrColumns=1, TableNr found=Table
             if (idebug .ne. 0) Write(idebug,*) ' TableName = KasGebrDef found!: ', KasGebrDef
             Success = GetTable (TableHandle, TableName, 1, KasGebrTableNr, Idebug, Iout1)
             if (.not. Success) goto 150
             TabYesNo = .true.
             GOTO 999
           Else
              TabYesNo = .false.
           Endif
        Enddo
        If (.not. TabYesNo) GoTo 30
      Endif


      IF (iDebug .ne. 0)  WRITE(IDEBUG,*) ' Usage read in m3/ha/day', GEBRKL

      GEBRKL = GEBRKL / Float(HA2M) / Float (NRSDAY)

      Return

! *********************************************************************
! *** Error during reading of file
! *********************************************************************

  150 CONTINUE
      call ErrMsgStandard (902, IECODE, '  ReadKasGebruik', STRING)

! *********************************************************************
! *** end of file
! *********************************************************************

   30 CONTINUE
      call ErrMsgStandard (911, IECODE, '  ReadKasGebruik', STRING)

  999 CONTINUE
      RETURN
      END subroutine ReadKasGebruik


  Subroutine ReadOpenDAGreenhouse (Infile1, iout1)

  ! read Greenhouse restart info from ASCII OpenDA file
  integer      Infile1, iout1, idebug


  Integer      RetVal

  Integer       inod
  Integer       ikas, ikkl, iecode
  Character(Len=1000) String
  Integer        Nhlp
  Parameter     (Nhlp = 32)  ! moet groter zijn dan NCKKL
  Integer       IDUM(NHLP)
  Real          RDUM(NHLP), Btot
  Character(Len=CharIdLength) CDUM(NHLP)
  Logical       allow, found, endfil

  ! file is already opened, rewind it
  rewind(Infile1)
  iDebug = ConfFil_get_iDebug()
  RetVal = 0

  Call SKPCOM (Infile1, ENDFIL,'ODS')
  call SetMessage(LEVEL_INFO, 'Read OpenDA greenhouse')
  do while (.not. endfil)
    ! read OpenDA record
     Read(Infile1,'(A1000)',END=21,ERR=150,IOSTAT=IECODE)  STRING
     ! skip regel als hij niet begint met juist keyword (GRHS)
      If (STRING(1:4) .EQ. 'GRHS') then
      ! GRHS node id
        allow = .false.
        Retval = Retval + GetVAR2 (STRING,' id ',1,' GRHS-ReadAscii',' OpenDA file',IOUT1, &
                                   CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
        call fndNd2(inod, CDUM(1))
        if (inod .gt. 0) then      ! knoop bestaat in schematisatie
            ikas = EiNode(inod,2)
            if (EiNode(inod,3) .eq. 3) then  ! en is greenhouse
                ! get the data
                ! update the corresponding RR variables and related variables
                allow = .true.
                Retval = Retval + GetVAR2 (STRING,' roofvolume ',2, ' GRHS-ReadAscii',' OPENDA file',IOUT1, &
                                              CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
                if (found) then
!                   write(*,*) ' found greenhouse id and roofvolume ', Id_nod(inod)(1:len_trim(id_nod(inod))), rdum(1)
                   BKASD(ikas) = Rdum(1)
                endif
                allow = .true.
                Retval = RetVal + GetVRS2 (STRING,' basinvolumes ',2,' GRHS-ReadAscii',' OPENDA file', IOUT1, &
                                             CDUM(1), RDUM(1), IDUM(1), Nckkl, IflRtn)
                if (found) then
!                   write(*,*) ' found greenhouse id and basinvolumes ', Id_nod(inod)(1:len_trim(id_nod(inod))), (rdum(ikkl), ikkl=1,nckkl)
                   Do ikkl =1, NCkkl
                      Bkas(ikas,ikkl) = Rdum(ikkl)
                   enddo
                endif
                allow = .true.
                Retval = Retval + GetVAR2 (STRING,' silovolume ',2,' GRHS-ReadAscii',' OPENDA file',IOUT1, &
                                             CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
                if (found) then
!                   write(*,*) ' found greenhouse id and silovolume ', Id_nod(inod)(1:len_trim(id_nod(inod))), rdum(1)
                   SiloB(ikas) = Rdum(1)
                endif

                Btot = SiloB(ikas)
                Do ikkl =1, NCkkl
                   BTot = Btot + Bkas(ikas,ikkl)
                enddo
!                write(*,*) ' updated BTot in Kas_Tnul =', BTot
                RSLMAP3_kas(1,Ikas,1) = Btot
                Kas_Tnul(1,Ikas) = Btot
            endif
        endif
     Endif
  enddo
  goto 21

150 continue
    call SetMessage(LEVEL_ERROR, 'Error reading OpenDA restart record '//String(1:len_trim(string)))
21  continue

  End subroutine ReadOpenDAGreenhouse


  Subroutine WriteOpenDAGreenhouse (Infile1)

  ! write Greenhouse restart info to ASCII OpenDA file
  integer      Infile1, idebug

  Integer       inode, ikas, ikkl
  Character(len=1)   Quote

  ! file is already opened
  iDebug = ConfFil_get_iDebug()
  quote = ''''

  do inode=1,ncnode
     ikas = EiNode(inode,2)
     if (EiNode(inode,3) .eq. 3) then  ! en is greenhouse
         write(Infile1,'(A,A1,A,A1,1X,A,G15.8,1X,A,10(G15.8,1X),A,G15.8,A)') 'GRHS id ',&
                                       quote,id_nod(inode)(1:len_trim(id_nod(inode))),quote,&
                                       ' roofvolume ', BKASD(ikas), &
                                       ' basinvolumes ', (BKAS(ikas,ikkl),ikkl=1,nckkl), &
                                       ' silovolume ', SiloB(ikas), ' grhs'
     Endif
  enddo

  End subroutine WriteOpenDAGreenhouse




      SUBROUTINE CMPKAS (IEVENT, ITMSTP, IKAS  , IMETEO, INODE, makelogfile)
! *********************************************************************
! *** Last update:  30 January 1997                   Peter Schrier
! ***     Aanpassing melding als watervraag > hoveelheid in bassin
! ***     Als max. berging < 0 dan deze melding niet op scherm
! *********************************************************************
! *********************************************************************
! *** Brief description:
! *** ------------------
! ***    This subroutine performs computations for kasgebieden
! *********************************************************************
! *** Input/output parameters:
! *** ------------------------
! ***  IEVENT = event number
! ***  ITMSTP = timestep number
! ***  IDEBUG = file unit number of debug file
! ***  IOUT1  = file unit number of output file with run messages
! *********************************************************************
! *** Berekeningen voor kasgebied
! *********************************************************************

      Implicit none

      ! variables
      Integer iEvent, iTmStp, iKas, iMeteo, iNode, iKKl, iOW, ibnd, ipluv
      Real area, area2, rInfl, bMax, pmpVOl
      Integer iDebug, iOut1, iHour, MakeLogfile


      iDebug = ConfFil_get_iDebug()
      iOut1 = ConfFil_get_iOut1()

      IF (iDebug .ne. 0)  WRITE(IDEBUG,*) 'CMPKAS ikas=',IKAS

! *********************************************************************
! *** Totaal areaal
! *** AREA = totaal, AREA2=totaal excl. bassins
! *********************************************************************

      AREA = 0.0
      AREA2= 0.0
      DO IKKL=1,NCKKL
         AREA = AREA + AREAKK(IKAS,IKKL)
         AREA2= AREA2+ AREAKK(IKAS,IKKL)- AREABK(IKAS,IKKL)
      ENDDO
! plus kasareaal aangesloten op silo
      AREA = AREA + AREAS(IKAS)
      AREA2= AREA2+ AREAS(IKAS)

! **********************************************************************
! *** Balans van de daken (neerslag, verdamping, berging, inflow bassins)
! *** Okt. 97: voeg crop factor open water toe
! **********************************************************************

      RKD (IKAS) = AAFNodeRainfall(inode) * RAIN(IMETEO) * AREA2 * timeSettings%timestepSize
      VKD (IKAS) = 0.0
      iHour = ConfArr_get_IHOUR()
      IF (iHour .GE. timeSettings%evaporationFromHr .AND. &
          iHour .LT. timeSettings%evaporationToHr) THEN
        IF (BKASD0(IKAS) .GT. 0) &
           VKD(IKAS) = EVAP (IMETEO) * CROPO * AREA2 * timeSettings%timestepSize * TMEVAP
      ENDIF
      RINFL      = 0.0

      BKASD(IKAS) = BKASD0(IKAS) - VKD(IKAS) + RKD(IKAS)
      IF (BKASD(IKAS) .GT. BMXDAK(IKAS)) THEN
         RINFL       = (BKASD(IKAS) - BMXDAK(IKAS)) / AREA2
         BKASD(IKAS) = BMXDAK(IKAS)
      ENDIF

! *********************************************************************
! *** Inflow naar bassins in m3
! *** Verdamping bassins alleen voor open bassins, berging>0,
! ***       okt. 97: voeg crop factor open water toe
! *** Berging in bassins in m3 = oude berging + inflow
! ***                              - verdamping - gebruik
! *** Gebruik uit berging alleen als boven minimale vullingsgraad
! *** okt 1996: leegpompen bassins boven bepaald nivo met bepaalde capaciteit
! *********************************************************************

      DO IKKL=1,NCKKL
       RKB (IKAS,IKKL) = 0.0
       VKB (IKAS,IKKL) = 0.0
       INK (IKAS,IKKL) = 0.0
       GEBR(IKAS,IKKL) = 0.0
       QKAS(IKAS,IKKL) = 0.0

       IF (AREAKK(IKAS,IKKL) .GT. 0) THEN

         VKB (IKAS,IKKL) = 0.0
         iHour = ConfArr_get_IHOUR()
         IF (iHour .GE. timeSettings%evaporationFromHr .AND. &
             iHour .LT. timeSettings%evaporationToHr) THEN
            IF (BKAS0(IKAS,IKKL) .GT. 0 .AND. KKLOPN(IKKL)) THEN
              VKB(IKAS,IKKL) = EVAP(IMETEO) * CROPO * AREABK(IKAS,IKKL) * &
                               timeSettings%timestepSize  * TMEVAP
              VKB(IKAS,IKKL) = MIN (VKB(IKAS,IKKL), BKAS0(IKAS,IKKL))
            ENDIF
         ENDIF

         RKB (IKAS,IKKL) = AAFNodeRainfall(inode) * RAIN(IMETEO) * AREABK(IKAS,IKKL) * timeSettings%timestepSize
         INK (IKAS,IKKL)= RKB(IKAS,IKKL) + &
                          RINFL * (AREAKK(IKAS,IKKL)-AREABK(IKAS,IKKL))

         BMAX  = KKLMXB(IKKL) * AREAKK(IKAS,IKKL)

         GEBR(IKAS,IKKL) = 0.0
         IF (BKAS0(IKAS,IKKL) .GT. PRCKAS*BMAX)  GEBR(IKAS,IKKL) = &
                GEBRKL * (AREAKK(IKAS,IKKL)-AREABK(IKAS,IKKL)) * timeSettings%timestepSize
! gebruik nu alleen over echt kasareaal, dus excl. bassins

         BKAS(IKAS,IKKL) = BKAS0(IKAS,IKKL) + INK(IKAS,IKKL) &
                              -VKB(IKAS,IKKL) - GEBR (IKAS,IKKL)

! *********************************************************************
! *** Check minimum/maximum berging
! *** B>BMAX: outflow naar open water
! *** B<0 : kort gebruik
! *********************************************************************

         QKAS(IKAS,IKKL) = 0.0
         IF (BKAS(IKAS,IKKL) .GT. BMAX) THEN
            QKAS(IKAS,IKKL) = (BKAS(IKAS,IKKL) - BMAX) / timeSettings%timestepSize
            BKAS(IKAS,IKKL) = BMAX
         ELSEIF ((BKAS(IKAS,IKKL) .LT. 0) .AND. (BMAX .GT. 0)) THEN
            if (makelogfile .gt. 0) call ErrMsgStandard(974, 0, ' ', 'Greenhouse uses more water than available in basin.')
! *** Melding was 'Berging bassin <0, kort gebruik'

            GEBR(IKAS,IKKL) =  GEBR(IKAS,IKKL) + BKAS(IKAS,IKKL)
            BKAS(IKAS,IKKL) =  0.0
         ENDIF

       ENDIF
      ENDDO

! *********************************************************************
! ** Dec 1996: silo's met ondergrondse opslag
! **  INKS   = inflow vanaf daken in silo
! **  QSILOW = overstort van silo naar open water
! *********************************************************************

      INKS (IKAS) = 0.0
      QSILOW(IKAS) = 0.0
      QSILGW(IKAS) = 0.0
      IF (AREAS(IKAS) .GT. 0) THEN
         INKS (IKAS) = RINFL * AREAS(IKAS)
         SILOB(IKAS) = SILOB0(IKAS) + INKS (IKAS)
         PMPVOL = MIN (PMPCAP(IKAS)* timeSettings%timestepSize, SILOB(IKAS))
         QSILGW(IKAS) = PMPVOL / timeSettings%timestepSize
         SILOB(IKAS) = SILOB(IKAS) - PMPVOL
         BMAX = SILOC(IKAS)*AREAS(IKAS)
         IF (SILOB(IKAS) .GT. BMAX) THEN
            QSILOW(IKAS) = (SILOB(IKAS) - BMAX) / timeSettings%timestepSize
            SILOB(IKAS) = BMAX
         ENDIF
         IF (iDebug .ne. 0) &
              WRITE(IDEBUG,*) SILOB0(IKAS), INKS(IKAS), SILOB(IKAS), &
                              PMPVOL, QSILGW(IKAS), BMAX, QSILOW(IKAS)
      ENDIF

! *********************************************************************
! *** check inundatie!; alleen melding;
! ***                   want bassins liggen hoger en overstromen niet
! *********************************************************************

      IOW  = EIOW(INODE)
      IBND = EIBND(INODE)
      IPluv = EIPluv(INODE)
      IF (IOW .GT. 0) THEN
         IF (LVLOW0(IOW) .GT. LVLKAS(IKAS)) THEN
           call ConfArr_set_FLDKAS(.TRUE.)
           if (makelogfile .gt. 0) then
             WRITE(IOUT1,'(A,A,A,2I5)') ' Inundation greenhouse area in event', &
                  Id_Nod(INODE),' in event/tijdstap', IEVENT, ITMSTP
           endif
         ENDIF
      ELSEIF (IBND .GT. 0) THEN
         IF (BNDPAR(IBND,1) .GT. LVLKAS(IKAS)) THEN
           call ConfArr_set_FLDKAS(.TRUE.)
           if (makelogfile .gt.  0) then
             WRITE(IOUT1,'(A,A,A,2I5)')  ' Inundation greenhouse area in event', &
                  Id_Nod(INODE),' in event/tijdstap', IEVENT, ITMSTP
           endif
         ENDIF
      ELSEIF (IPluv .GT. 0) THEN
         ! no check on inundation
      ELSE
         call SetMessage(LEVEL_FATAL, 'Greenhouse node should be connected to open water/boundary for spilling')
      ENDIF

! *********************************************************************
! *** Totaal naar open water of boundary
! *********************************************************************

      IF (IOW .GT. 0) THEN
         DO IKKL = 1,NCKKL
            QINOW(IOW,3) = QINOW(IOW,3) + QKAS(IKAS,IKKL)
         ENDDO
         QINOW(IOW,3) = QINOW(IOW,3) + QSILOW(IKAS)
         IF (iDebug .ne. 0) THEN
            WRITE(IDEBUG,*) ' IKAS  INODE IOW  QINOW(IOW,3)'
            WRITE(IDEBUG,*) IKAS, INODE, IOW, QINOW(IOW,3)
         ENDIF
      ELSEIF (IBND .GT. 0) THEN
         DO IKKL = 1,NCKKL
            QINBND(IBND)%totalGreenhouse = QINBND(IBND)%totalGreenhouse + QKAS(IKAS,IKKL)
            QBND(IBND) = QBND(IBND) + QKAS(IKAS,IKKL)
         ENDDO
         QINBND(IBND)%totalGreenhouse = QINBND(IBND)%totalGreenhouse + QSILOW(IKAS)
         QBND(IBND) = QBND(IBND) + QSILOW(IKAS)
         IF (iDebug .ne. 0) THEN
            WRITE(IDEBUG,*) ' IKAS  INODE IBND  QINBND(IBND)_totalGreenhouse QBND'
            WRITE(IDEBUG,*) IKAS, INODE, IBND, QINBND(IBND)%totalGreenhouse, QBND(IBND)
         ENDIF
      ELSEIF (IPluv .GT. 0) THEN
         DO IKKL = 1,NCKKL
            QPluv(IPluv)%totalGreenhouse = QPluv(IPluv)%totalGreenhouse + QKAS(IKAS,IKKL)
            QInPluv(IPluv) = QInPluv(IPluv) + QKAS(IKAS,IKKL)
         ENDDO
         QPluv(IPluv)%totalGreenhouse = QPluv(IPluv)%totalGreenhouse + QSILOW(IKAS)
         QInPluv(IPluv) = QInPluv(IPluv) + QSILOW(IKAS)
         IF (iDebug .ne. 0) THEN
            WRITE(IDEBUG,*) ' IKAS  INODE IPluv QPluv(IPluv)_totalGreenhouse QinPluv'
            WRITE(IDEBUG,*) IKAS, INODE, IPluv, QPluv(IPluv)%totalGreenhouse, QInPluv(IPluv)
         ENDIF
      ENDIF

! *********************************************************************
! *** debug
! *********************************************************************

      IF (iDebug .ne. 0) THEN
         WRITE(IDEBUG,*) ' Greenhouse area', NamNod(INODE)
         WRITE(IDEBUG,*) ' Timestep nr                :',ITMSTP
         WRITE(IDEBUG,*) ' Timestepsize in s          :',timeSettings%timestepSize
         WRITE(IDEBUG,*) ' Total surface              :',AREA
         WRITE(IDEBUG,*) ' Precipitation on roofs m3  :', RKD(IKAS)
         WRITE(IDEBUG,*) ' Evaporation from roofs m3  :', VKD(IKAS)
         WRITE(IDEBUG,*) ' Initial storage  roofd m3  :', BKASD0(IKAS)
         WRITE(IDEBUG,*) ' Final   storage  roofs m3  :', BKASD(IKAS)
         WRITE(IDEBUG,'(A,10F10.2)') ' Inflow bassins in m3        :', (INK  (IKAS,IKKL), IKKL=1,NCKKL)
         WRITE(IDEBUG,'(A,10F10.3)') ' Initial storage bassins [m3]:', (BKAS0(IKAS,IKKL), IKKL=1,NCKKL)
         WRITE(IDEBUG,'(A,10F10.2)') ' Final   storage bassins [m3]:', (BKAS (IKAS,IKKL), IKKL=1,NCKKL)
         WRITE(IDEBUG,'(A,10F10.2)') ' Evaporation bassins [m3]    :', (VKB  (IKAS,IKKL), IKKL=1,NCKKL)
         WRITE(IDEBUG,'(A,10F10.4)') ' Usage from bassins [m3]     :', (GEBR (IKAS,IKKL), IKKL=1,NCKKL)
         WRITE(IDEBUG,'(A,10F10.5)') ' Outflow from bassins [m3/s] :', (QKAS (IKAS,IKKL), IKKL=1,NCKKL)
         WRITE(IDEBUG,'(A,10F10.5)') ' Outflow from bassins [m3]   :', (QKAS(IKAS,IKKL)*timeSettings%timestepSize,IKKL=1,NCKKL)
         WRITE(IDEBUG,'(A,10F10.2)') ' Inflow silo     [m3]        :', INKS(IKAS)
         WRITE(IDEBUG,'(A,10F10.2)') ' Initial storage silo [m3]   :', SILOB0(IKAS)
         WRITE(IDEBUG,'(A,10F10.2)') ' Final   storage silo [m3]   :', SILOB (IKAS)
         WRITE(IDEBUG,'(A,10F10.5)') ' Out via silo to gw [m3/s]   :', QSILGW (IKAS)
         WRITE(IDEBUG,'(A,10F10.5)') ' Overflow from silo [m3/s]   :', QSILOW (IKAS)
         WRITE(IDEBUG,'(A,10F10.2)') ' AreaKasklassen    :', (AreaKK(ikas,ikkl), IKKL=1,NCKKL)
         WRITE(IDEBUG,'(A,10F10.2)') ' AreaBassins       :', (AreaBK(ikas,ikkl), IKKL=1,NCKKL)
      ENDIF

      RETURN
      END subroutine cmpkas




  Subroutine Init1Kas
    ! *********************************************************************
    ! *** Last update:
    ! *********************************************************************
    ! *** Brief description:
    ! *** ------------------
    ! ***    Initialisatie kassen aan het begin van een bui
    ! *********************************************************************

      Implicit none
!      Integer Ikas

!       DO IKAS = 1,NCKAS
!          BKASD (IKAS) = 0.0
!          SILOB (IKAS) = 0.0
!       ENDDO
!
!  Vector/Array initialisation
      BKASD = 0.0
      SILOB = 0.0

      RETURN
      END subroutine Init1Kas



  Subroutine Init2Kas
    ! *********************************************************************
    ! *** Last update:
    ! *********************************************************************
    ! *** Brief description:
    ! *** ------------------
    ! ***    Stuk uit sub Init2: initialisie van kasgebied per tijdstap
    ! *********************************************************************

      Implicit none

!      Integer Ikas, Ikkl

!      DO IKAS = 1,NCKAS
!         DO IKKL = 1,NCKKL
!            BKAS0 (IKAS,IKKL) = BKAS (IKAS,IKKL)
!         ENDDO
!         BKASD0(IKAS) = BKASD(IKAS)
!         SILOB0(IKAS) = SILOB(IKAS)
!      ENDDO
!  Vector/Array initialisation
      BKAS0  = BKAS
      BKASD0 = BKASD
      SILOB0 = SILOB

      RETURN
      END subroutine Init2Kas


      Subroutine WrInputDataKas (Iout9, Iout4, RnDate, RnTime)
        ! *********************************************************************
        ! *** Last update:
        ! *********************************************************************
        ! *** Brief description:
        ! *** ------------------
        ! ***    Stuk uit sub WrData: uitvoer van Kasgebied in *.Out files
        ! *********************************************************************

        Implicit none


        Integer      INODE, IKIND, INR, IKKL, i
        Real         AREAT
        Integer      IOUT9, IOUT4
        Integer*2    RNDATE(3), RNTIME(4)


! Kasgebied
      IF (NCKAS .GT. 0) THEN
         WRITE(IOUT9,13)
 13      FORMAT (//,' Summary inputdata greenhouse-areas  ',//,&
              ' Node identification   Node      Max.storage   surface    ',           &
              'Surfaces per greenhouse class',/,                           &
              '                                      on roofs                 ',/, &
              '                       name       (mm)            (m)     ',   &
              '    (in hectare)',/,156('='))
         DO INODE =1,NCNODE
          IKIND = EiNode(INODE,3)
          INR   = EiNode(INODE,2)
          IF (IKIND .EQ. 3) THEN
! nu ook met silo areaal
            AREAT = AREAS(INR)
            DO IKKL=1,NCKKL
               AREAT = AREAT + AREAKK(INR,IKKL)
            ENDDO
            IF (AREAT .LE. 0) THEN
              WRITE(IOUT9,23) Id_Nod(INODE), &
                              NamNod(INODE),&
                              0.0, LVLKAS(INR),  (AREAKK(INR,IKKL)/Float(HA2M),IKKL=1,NCKKL)
            ELSE
              WRITE(IOUT9,23) Id_Nod(INODE),&
                              NamNod(INODE), &
                              BMXDAK(INR)/AREAT/MM2M, LVLKAS(INR), (AREAKK(INR,IKKL)/Float(HA2M),IKKL=1,NCKKL)
            ENDIF
   23       FORMAT (A20,1X,A12,1X,12(F9.3,1X))
          ENDIF
         ENDDO
      ENDIF


! greenhouse area
      If (nckas .gt. 0 .and. OutputDesired(3) ) then
        WRITE(IOUT4,51) RNDATE(3),RNDATE(2),RNDATE(1),(RNTIME(I),I=1,2), CASENM
   51   FORMAT (' Run datum (DD-MM-YY) and time:',I2,'-',I2,'-',I4,2X,I2,':',I2,//,&
                ' Case name                 ',A20,/)
        WRITE(IOUT4,1005) '[m3]','[m3/s]','[m3/s]'
 1005   FORMAT(//,' Maxima per event',//,&
                ' Event   Start     Node identification   Node  ',&
                '       Total_storage  Total_discharge  Rainfall',/,&
                '  nr  year-mon-day                       name',&
                '             ',A6,10X,A6,7X,A6,/,95('='))
      Endif

      Return
      END subroutine WrInputDataKas




  Subroutine Wr1OutGreenhouse (Iout4, Ievent, Month, INode, Ikas)
    ! *********************************************************************
    ! *** Last update:
    ! *********************************************************************
    ! *** Brief description:
    ! *** ------------------
    ! ***    Stuk uit sub Wr1Out: uitvoer van Greenhouse node: maxima per event in OUT file
    ! *********************************************************************

      Implicit none

    ! variables

    Integer      INODE, Ikas, Iout4, Ievent
    Integer      Ikkl
    Real         BMax, Area, RksBrg, RksFlw, RksRai
    CHARACTER(len=3) MONTH(12)

           if (.not. associated(KSMBPC)) return  ! If there is nothing, do nothing

           BMAX = 0.
           AREA = 0.
           DO IKKL=1,NCKKL
              BMAX = BMAX + KKLMXB(IKKL)*AREAKK(IKAS,IKKL)
              AREA = AREA + AREAKK(IKAS,IKKL)
           ENDDO
! berging in m3
           RKSBRG = KSMBPC(IKAS,NCKKL+1,IEVENT)
! ARS ....: adjust Bmax for Silo's
           BMAX   = BMax + SiloC(ikas) * AreaS(IKAS)
           IF (BMAX .LE. .00001) RKSBRG = 0.0
! flows in m3/s
           RKSFLW = KSMQOU(IKAS,NCKKL+1,IEVENT,1)
           RKSRAI = KSMQOU(IKAS,NCKKL+1,IEVENT,2)
           WRITE(IOUT4,1006) IEVENT, EventStartDateTime(IEVENT,1),MONTH(EventStartDateTime(IEVENT,2)), &
                EventStartDateTime(IEVENT,3),&
                Id_Nod(INODE), NamNod(INODE),&
                RKSBRG, RKSFLW, RKSRAI
 1006      FORMAT (I4,1X,I4,1X,A3,1X,I3,3X,A20,1X,A15,1X,3(F12.3,1X))

  Return
  END subroutine Wr1OutGreenhouse

  !> If success, function returns Values array of length ElementCount
  !! for greenhouse elementset on specific quantity handle
  function RR_GetGreenhouseDataByIntId(QuantityHandle, ElementCount, Values) result(success)

    use RR_open_mi_support
    use wl_open_mi_support

    implicit none

    ! return value
    logical  :: success

    ! arguments
    integer,          intent(in)      :: QuantityHandle   !< quant. handle
    integer,          intent(in)      :: ElementCount     !< #elems in greenhouse elementset
    double precision, intent(out), &
            dimension(1:ElementCount) :: Values           !< values in greenhouse elemenset

    ! locals

    Values  = 0
    success = .true.

    select case (QuantityHandle)
    ! Subdivide for various variables
    case(RRiStorage_m3)
    !RR Storage basins in m3
            ! If a stack overflow occurs due to this array operation, use a do loop
            if (NKAS > 0) then
                Values(1:NKAS) = RslMap3_kas(1, 1:NKAS, 1)
            else
                success = .false.
            endif
    case (RRiFlow)
    !RR Flow basins in m3/s
            if (NKAS > 0) then
                Values(1:NKAS) = RslMap3_kas(2, 1:NKAS, 1)
            else
                success = .false.
            endif
    case(RRiRainfall)
    !RR Rainfall in m3/s
            ! If a stack overflow occurs due to this array operation, use a do loop
            if (NKAS > 0) then
                Values(1:NKAS) = RslMap3_kas(3, 1:NKAS, 1)
            else
                success = .false.
            endif
    case(RRiEvaporationSurface)
    !RR Evaporation in m3/s
            ! If a stack overflow occurs due to this array operation, use a do loop
            if (NKAS > 0) then
                Values(1:NKAS) = RslMap3_kas(4, 1:NKAS, 1)
            else
                success = .false.
            endif
    case(RRiWaterUse)
    !RR Water use in m3/s
            ! If a stack overflow occurs due to this array operation, use a do loop
            if (NKAS > 0) then
                Values(1:NKAS) = RslMap3_kas(5, 1:NKAS, 1)
            else
                success = .false.
            endif
    case default
    ! Something is wrong
        success = .false.
    end select

  end function

end module Greenhouse
