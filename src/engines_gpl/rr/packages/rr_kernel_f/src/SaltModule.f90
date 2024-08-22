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
! current revision: $Revision:: 5               $


module Salts

  !use
  use Conf_arr
  use Paved
  use Unpaved
  use Greenhouse
  use Structures
  use Boundary
  use RWZI
  use Industry
  use Link
!
  use Messages
  use DH_Alloc
  use ReadLib

  !variables
  ! *** Zout data
  ! ***
  !  ISTCMP: indicator ja/nee zoutberekeningen uitvoeren (default=0=nee)
  !  INDSLT: geeft conversie knopen - zoutknopen
  !  SLTINI: initiele zoutconcentraties op alle knopen
  !          verondersteld is dat deelbakjes op een knoop dezelfde init.concentrat
  !          dus init. conc. berging op land en berging bodem met zelfde zoutconce
  !  SALTD : gemiddelde zoutconcentratie lozingen op zoutknopen
  !  SALT  : gemiddelde zoutconcentratie op zoutknopen
  !  SALTF : zoutconcentratie aan het einde van de tijdstap op knopen
  !  SLTKWL: zoutconcentratie kwel per knoop; alleen gevuld voor onverhard gebied
  !  SASLT : coefficienten in stelsel vergelijkingen
  !  BSASLT: rechterlid in stelsel vergelijkingen
  !          (stelsel vergelijkingen: saslt * salt = bsaslt)
  ! *** SLTMXC = maximum zoutconcentraties per knoop

  implicit none

  INTEGER, Pointer, SAVE ::  INDSLT(:)
  Double Precision, Pointer, SAVE ::     SALTD(:), SALT(:), SALTF(:), SaltF0(:),&
                                         SASLT(:,:), BSASLT(:), &
                                         SLTMXC(:)
  Integer  NrRwziWarnings

contains



  SUBROUTINE Salt_confar2

    ! determine array size for salt arrays-NSLT
    Implicit none
    Logical success

    NrRwziWarnings = 0
    NSLT = NCSALT
    IF (ISLCMP .EQ. 0) NSLT = 1
    IF ((ISLCMP /= 0) .and. (ConfFil_get_iOut1() > 0)) then
      WRITE(ConfFil_get_iOut1(), *) ' Zoutnodes    =',NSLT
    end if

    success = DH_AllocInit (NNod, IndSlt, 0)
!    ALLOCATE ( INDSLT (NNOD), Stat=Allocation_Error )
    success = success .and. DH_AllocInit (NSlt, SaltD, Salt, SaltF, 0D0)
    success = success .and. DH_AllocInit (NSlt, Saltf0, 0D0)
    success = success .and. DH_AllocInit (NSlt, NSLT, SaSlt, 0D0)
    success = success .and. DH_AllocInit (NSlt, BsaSlt, 0D0)
    success = success .and. DH_AllocInit (NcNode, SltMxC, 0D0)
!    ALLOCATE ( SALTD  (NSLT), SALT  (NSLT), SALTF(NSLT), &
!               SASLT  (NSLT,NSLT), BSASLT (NSLT), SLTMXC(ncNODE), Stat=Allocation_Error )
    If (.not. success) call ErrMsgStandard (981, 0, ' Error allocating arrays in subroutine ', &
                                           ' Salt_ConfAr2' )

    RETURN
  End subroutine Salt_confar2



  SUBROUTINE Salt_initSl (ICALL)
    ! ********************************************************************
    ! ** Bepaling array INDSLT tbv zoutberekeningen
    ! **   en initialisatie zoutconcentraties
    ! ********************************************************************
    ! **  Onderscheid per knoop de volgende bakjes:
    ! **    - verhard gebied: straat, riool
    ! **    - onverhard gebied: land, bodem (incl. zomerberging)
    ! **    - kasgebied: dak, 10 bassins
    ! **    - open water: 1 bak
    ! **    - kunstwerk/randknoop als externe lozing/onttrekking
    ! **    - Boundary geen eigen bakje
    ! **    - pluvius knopen: voorlopig nog niet meenemen!
    ! **    - RWZI
    ! **    - Industry geen eigen bakje
    ! **    - Sacramento
    ! **  Array INDSLT geeft per knoop de index van de eerste bak
    ! **  horend bij de knoop in het zoutnetwerk
    ! ********************************************************************
    ! **  ICALL = 1 first call
    ! **        = 2 second call
    ! ********************************************************************

    Implicit none

    INTEGER ICALL
    INTEGER INODE, INDX, INDEX, INDEX0, KIND, NODEUP, NODOWN !, iNod
    Integer iDebug
    Logical SetValue
    Character(Len=CharIdLength) Name

!   iDebug = ConfFil_get_iDebug()
    iDebug = idebugLunRR

    IF (ICALL .EQ. 1) THEN
       INDEX = 1
       DO INODE = 1, NCNODE
          KIND = EiNode(INODE,3)
          Select Case (Kind)
             Case (1)         ! Verhard gebied
                INDEX = INDEX + 3  ! Jan 1998: 2 sewer storage instead of 1;
             Case (2)         ! Onverhard gebied
                INDEX = INDEX + 3  ! Juli 1998: onverzadigde zone toegevoegd
             Case (3)         ! Kassen
                INDEX = INDEX + 12
             Case (4)         ! Open water
                INDEX = INDEX + 1
             Case (14)        ! RWZI
                INDEX = INDEX + 1
!            Case (16)        ! Sacramento
!               INDEX = INDEX + 5
          End Select
!         structures, boundary, NWRW, Industry; geen zoutbakjes voor deze knopen
       ENDDO
       NCSALT = INDEX-1

    ELSE

!  Already taken case of in DH_AllocInit
!       DO INOD=1,NNOD
!          INDSLT(INOD) = 0
!       ENDDO
       INDEX = 1

    ! **************************************************************************
    ! ** NETWERK
    ! ***********************************************************************

       DO INODE = 1, NCNODE
          KIND = EiNode(INODE,3)
          INDSLT (INODE) = 0
          INDEX0=INDEX
          SetValue = .true.
          Select Case (Kind)
             Case (5)         ! Kunstwerken
                NODEUP = UPNODE(INODE)
                NODOWN = DONODE(INODE)
                IF (EiNode(NODEUP,3) .NE. 4 .OR. &
                    EiNode(NODOWN, 3) .NE. 4) THEN
                    GOTO 20
                ENDIF
             Case (6:13)      ! Boundary, NWRW, Industry, Kunstwerken not in use
                GOTO 20
          End Select

          INDSLT (INODE) = INDEX
          Select Case (Kind)
             Case (1)         ! Verhard gebied
                INDEX = INDEX + 3   ! Jan 1998: with 2 types of sewer storage now +3 instead of +2.
             Case (2)         ! Onverhard gebied
                INDEX = INDEX + 3   ! Juli 1998: onverzadigde zone toegevoegd
             Case (3)         ! Kassen
                INDEX = INDEX + 12
             Case (4)         ! Open water
                INDEX = INDEX + 1
             Case (5)         ! Kunstwerken
                NODEUP = UPNODE(INODE)
                NODOWN = DONODE(INODE)
                IF (EiNode(NODEUP,3) .NE. 4 .OR. &
                    EiNode(NODOWN, 3) .NE. 4) THEN
                    SetValue = .false.
                ENDIF
             Case (14)        ! RWZI
                INDEX = INDEX + 1
             Case (6:13)      ! Boundary, NWRW
                  SetValue = .false.
             Case (15)        ! Industry
                  SetValue = .false.
!            Case (16)        ! Sacramento
!                 SetValue = .false.
          End Select

          IF (ISLCMP /= 0 .and. SetValue) THEN
            DO INDX =INDEX0, INDEX - 1
               SALTF (INDX) = SLTINI(INODE)
            ENDDO
          ENDIF

 20       CONTINUE
       ENDDO

       NCSALT = INDEX-1
       IF (NCSALT .GT. NSLT .AND. ISLCMP /= 0 .AND.  ICALL .EQ. 2 ) THEN
          call ErrMsgStandard (912, NCSALT, '  Initsl', ' saltnodes')
       ENDIF

! zet index voor boundaries, via de kunstwerken of de RWZI/Industry?

       DO INODE = 1, NCNODE
          KIND = EiNode(INODE,3)
          IF (KIND .EQ. 5) THEN         ! Structures;
             NODEUP = UPNODE(INODE)
             NODOWN = DONODE(INODE)
             IF (EiNode(NODEUP,3) .EQ. 6) THEN
                INDSLT (NODEUP) = INDSLT(NODOWN)  ! zet zoutindex upstream bnd op downstream open water
             ELSEIF (EiNode(NODOWN,3) .EQ. 6) THEN
                INDSLT (NODOWN) = INDSLT(NODEUP)  ! zet zoutindex downstream bnd op upstream open water
             ENDIF
          ELSEIF (KIND .EQ. 14) THEN    ! RWZI
             NODOWN = DONODE(INODE)
             IF (EiNode(NODOWN,3) .EQ. 6) THEN
                INDSLT (NODOWN) = INDSLT(INODE)   ! zet zoutindex op RWZI
             ENDIF
          ELSEIF (KIND .EQ. 15) THEN    !Industry
             NODOWN = DONODE(INODE)
!            n.a.v. ARS 8610: test Nodown toegevoegd
             If (Nodown .gt. 0) then
                IF (EiNode(NODOWN,3) .EQ. 6) THEN
                   INDSLT (NODOWN) = INDSLT(INODE)   ! zet zoutindex op Industry
                ENDIF
             ENDIF
          ENDIF
       ENDDO

       IF (iDebug /= 0 .AND. ISLCMP /= 0) THEN
         WRITE(IDEBUG,*) ' Initsl ncNode, ncSalt=', ncNode, ncSalt
         DO INODE=1,NCNODE
            IF (INDSLT(INODE) .LE. 0 .or. IndSlt(Inode) .gt. NcSalt) THEN
! Dec2003       WRITE(IDEBUG,*) INODE, EiNode(INODE,1), INDSLT(INODE)
                Name = Id_Nod(INODE)
                WRITE(IDEBUG,*) INODE, Name(1:Len_trim(Name)), INDSLT(INODE)
! Nov 2002 Taiwan
!               If (IndSlt(Inode) .gt. NcSalt) IndSlt(Inode) = 0
! Correction Dec 2004 ARS 12338 (bug salt case Marieke ten Voorde)
                If (IndSlt(Inode) .gt. NcSalt) then
                   KIND = EiNode(INODE,3)
                   NODEUP = UPNODE(INODE)
                   NODOWN = DONODE(INODE)
                   If (NodeUp .gt. 0) then
                      if (IndSlt(NodeUp) .gt. 0 .and. IndSlt(Nodeup) .le. NcSalt) then
                         IndSlt(Inode) = IndSlt(Nodeup)
                      else  ! don't know any better
                         IndSlt(Inode) = 1
                      endif
                   Elseif (NoDown .gt. 0) then
                      if (IndSlt(Nodown) .gt. 0 .and. IndSlt(Nodown) .le. NcSalt) then
                         IndSlt(Inode) = IndSlt(Nodown)
                      else  ! don't know any better
                         IndSlt(Inode) = 1
                      endif
                   Else  ! don't know any better
                      IndSlt(Inode) = 1
                   Endif
                Endif
! End correction ARS 12338
! End Nov 2002 Taiwan
            ELSE
! Dec2003       WRITE(IDEBUG,*) INODE, EiNode(INODE,1), INDSLT(INODE), SALTF(INDSLT(INODE))
                Name = Id_Nod(INODE)
                WRITE(IDEBUG,*) INODE, Name(1:Len_trim(Name)), INDSLT(INODE), SALTF(INDSLT(INODE))
            ENDIF
         ENDDO
       ENDIF
    ENDIF
    RETURN
  END subroutine Salt_initSl




  SUBROUTINE SALINE (ITMSTP, RTMSZ, IFLZT)
    ! *********************************************************************
    ! *** DELFT_3B   version 1.21
    ! ***   Zoutberekeningen gebaseerd op DM
    ! *********************************************************************
    ! *** SALT COMPUTATIONS
    ! ********************************************************************
    ! *** Subroutine waarin de zoutbeweging in de netwerken wordt bepaald
    ! ********************************************************************

     Implicit none

    Double precision  COEFF(4), RTMSZ
    Double precision  DISCH(NSLT), EXTRCT(NSLT), VOL(NSLT), VOL0(NSLT),&
                      QINS(NSLT), QOUT(NSLT), COEF(4,NSLT), qEvap
    INTEGER NRIJ2(NSLT), iTmStp, iFlzt, iNode, i, j, iSalt, kind
    Integer iNr, iKKl, iOW, iNodeW, iNodeU, iHlp1, iHlp2, Irwzi, InodeRwzi
    Integer Iow2, INodeW2
    Double Precision    qInf, qRUit, qLand, qBUit, qKUit, qSUit
    Logical found, RwziWarning
    Character(Len=CharIdLength) Name

    Integer Ilink, ISaltUp, ISaltDown, INodeUp, INodeDown, IKindUp, IKindDown
    Double Precision    Q

    ! ********************************************************************
    ! ** Het array INDSLT geeft voor elke knoop het nummer van de
    ! **  bijbehorende zoutknoop
    ! ** Voor Kunstwerken: >0 betekent wederzijds Open water
    ! ** Voor Kunstwerken: =0 betekent een van de twee is een randknoop
    ! ** Voor Boundaries :ISALT = de index van het bijbehorende open water
    ! ********************************************************************
    ! ********************************************************************
    ! ** INITIALISATIES.
    ! ********************************************************************

    Integer iDebug, iOut1

    iDebug = ConfFil_get_iDebug()
    iOut1 = ConfFil_get_iOut1()
    RwziWarning = .false.

    IF (iDebug /= 0) WRITE(IDEBUG,*) ' CALL SALINE '
      !c    BIGM  = 1000000.
    IF (iDebug /= 0)  THEN
       WRITE(IDEBUG,*)  ' Start Saline; NCNODE=', NCNODE, NCSALT
       DO INODE = 1,NCNODE
         Name = Id_Nod(INODE)
         WRITE(IDEBUG,*) INODE, Name(1:Len_trim(Name)), INDSLT(INODE), sltini(inode), sltkwl(inode)
       ENDDO

    ENDIF

! Vector initialisation
    QINS = 0.0
    QOUT = 0.0
    BSASLT = 0.0
    SASLT = 0.0
    COEF = 0.0
    VOL  = 0.0001D0 ! GP: was 0; op 0.0001 gezet om lege bakjes te voorkomen.
    VOL0 = 0.0001D0 ! GP: was 0
    DO I=1,NCSALT
       COEF(1,I) = 1.0
!       VOL(I)  = 0.0001   ! GP: was 0; op 0.0001 gezet om lege bakjes te voorkomen.
!       VOL0(I) = 0.0001   ! GP: was 0
    ENDDO

    ! *********************************************************************
    ! ** BEPALEN VAN ZOUTLASTEN
    ! ********************************************************************

    CALL DTSLTD (DISCH, EXTRCT, RTMSZ, RwziWarning)
    IF (iDebug /= 0) WRITE(IDEBUG,*)  ' End Dtsltd'

    ! ********************************************************************
    ! **  OPBOUWEN MATRIX SASLT, RECHTERLID BSASLT
    ! **  INITIALISEREN KNOOPGEGEVENS MBT
    ! **  VOLUME (VOL), TOTAAL INKOMEND/UITGAAND DEBIET (QINS, QOUT)
    ! **  REGENVAL AL IN SUB DTSLTD; HIER NOG VERDAMPING TOEVOEGEN
    ! ********************************************************************

    IF (iDebug /= 0)  WRITE(IDEBUG,1101)
  1101 FORMAT(' ISALT       QINS        QOUT     ', &
           ' DISCH     EXTRCT       ', &
           ' QEVAP      SALTD        BSA       VOL  VOL0')
    ! *********************************************************************
    ! ** Loop alle bakjes langs
    ! ** Verdamping hoort volgens PAWN XI behandeld te worden door QINS-QEVAP
    ! **   ipv QOUT+QEVAP  zoals in routine SALINE van DM gedaan is!!!
    ! ********************************************************************

    DO INODE=1,NCNODE
      ISALT = INDSLT(INODE)
      IF (ISALT .GT. 0) THEN
        KIND = EiNode(INODE,3)
        INR  = EiNode(INODE,2)
        IF (KIND .EQ. 1) THEN
    ! verhard gebied
    ! surface area
          VOL(ISALT) = VOL(ISALT) + BVSTR(INR)
          VOL0(ISALT) = VOL0(ISALT) + BVSTR0(INR)
          QEVAP = VV (INR) / RTMSZ
          QINS(ISALT) = QINS(ISALT) + (DISCH (ISALT)-QEVAP) * RTMSZ
          QOUT(ISALT) = QOUT(ISALT) + (EXTRCT(ISALT)      ) * RTMSZ
          BSASLT (ISALT) = BSASLT(ISALT) + (DISCH(ISALT) * SALTD(ISALT)) * RTMSZ
    ! RWA sewer
          ISALT = ISALT + 1
          VOL(ISALT) = VOL(ISALT) + BVRL (INR,1)
          VOL0(ISALT) = VOL0(ISALT) + BVRL0 (INR,1)
          QINS(ISALT) = QINS(ISALT) + (DISCH (ISALT)      ) * RTMSZ
          QOUT(ISALT) = QOUT(ISALT) + (EXTRCT(ISALT)      ) * RTMSZ
          BSASLT (ISALT) = BSASLT(ISALT) + (DISCH(ISALT) * SALTD(ISALT)) * RTMSZ
        ! DWA sewer
          ISALT = ISALT + 1
          VOL(ISALT) = VOL(ISALT) + BVRL (INR,2)
          VOL0(ISALT) = VOL0(ISALT) + BVRL0 (INR,2)
          QINS(ISALT) = QINS(ISALT) + (DISCH (ISALT)      ) * RTMSZ
          QOUT(ISALT) = QOUT(ISALT) + (EXTRCT(ISALT)      ) * RTMSZ
          BSASLT (ISALT) = BSASLT(ISALT) + (DISCH(ISALT) * SALTD(ISALT)) * RTMSZ
        ELSEIF (KIND .EQ. 2) THEN
    ! onverhard gebied
    ! berging op land
          VOL(ISALT) = VOL(ISALT) + BOLND(INR)
          VOL0(ISALT) = VOL0(ISALT) + BOLND0(INR)
          QEVAP = VO (INR)/RTMSZ
          QINS(ISALT) = QINS(ISALT) + (DISCH (ISALT)-QEVAP) * RTMSZ
          QOUT(ISALT) = QOUT(ISALT) + (EXTRCT(ISALT)      ) * RTMSZ
          BSASLT (ISALT) = BSASLT(ISALT) + (DISCH(ISALT) * SALTD(ISALT)) * RTMSZ
! onverzadigde zone;
! volume onverzadigde zone
          ISALT = ISALT + 1
          VOL(ISALT) = VOL(ISALT) + OnvZone(INR)%Actual_Volume
          VOL0(ISALT) = VOL0(ISALT) + OnvZone(INR)%Init_Volume
!Minstens 1 mm berging in onv.zone aanhouden om consistentie met oude sommen te houden
! (anders verdwijnt er nl. zout zodra het bakje leeg is;
          VOL (ISALT) = MAX (0.001 * AREAOH(INR), VOL(ISALT))
          VOL0(ISALT) = MAX (0.001 * AREAOH(INR), VOL0(ISALT))
          QEVAP = VBO(INR)/ RTMSZ
          QINS(ISALT) = QINS(ISALT) + (DISCH (ISALT)-QEVAP) * RTMSZ
          QOUT(ISALT) = QOUT(ISALT) + (EXTRCT(ISALT)      ) * RTMSZ
          BSASLT (ISALT) = BSASLT(ISALT) + (DISCH(ISALT) * SALTD(ISALT)) * RTMSZ
! bodem
          ISALT = ISALT + 1
          VOL(ISALT) = VOL(ISALT) + BOBD(INR)
          VOL0(ISALT) = VOL0(ISALT) + BOBD0(INR)
          QEVAP = 0.0
          QINS(ISALT) = QINS(ISALT) + (DISCH (ISALT)-QEVAP) * RTMSZ
          QOUT(ISALT) = QOUT(ISALT) + (EXTRCT(ISALT)      ) * RTMSZ
          BSASLT (ISALT) = BSASLT(ISALT) + (DISCH(ISALT) * SALTD(ISALT)) * RTMSZ
        ELSEIF (KIND .EQ. 3) THEN
    ! kasgebied
          VOL(ISALT) = VOL(ISALT) + BKASD(INR)
          VOL0(ISALT) = VOL0(ISALT) + BKASD0(INR)
          QEVAP = VKD(INR)/RTMSZ
          QINS(ISALT) = QINS(ISALT) + (DISCH (ISALT)-QEVAP) * RTMSZ
          QOUT(ISALT) = QOUT(ISALT) + (EXTRCT(ISALT)      ) * RTMSZ
          BSASLT (ISALT) = BSASLT(ISALT) + (DISCH(ISALT) * SALTD(ISALT)) * RTMSZ
          DO IKKL=1,NKKL
            ISALT = ISALT + 1
            VOL(ISALT) = VOL(ISALT) + BKAS(INR,IKKL)
            VOL0(ISALT) = VOL0(ISALT) + BKAS0(INR,IKKL)
            QEVAP = VKB(INR,IKKL)/RTMSZ
            QINS(ISALT) = QINS(ISALT) + (DISCH (ISALT)-QEVAP) * RTMSZ
            QOUT(ISALT) = QOUT(ISALT) + (EXTRCT(ISALT)      ) * RTMSZ
            BSASLT (ISALT) = BSASLT(ISALT) + (DISCH(ISALT) * SALTD(ISALT)) * RTMSZ
          ENDDO
    ! Silo
          ISALT = ISALT + 1
          VOL(ISALT) = VOL(ISALT) + SILOB(INR)
          VOL0(ISALT) = VOL0(ISALT) + SILOB0(INR)
          QEVAP = 0.
          QINS(ISALT) = QINS(ISALT) + (DISCH (ISALT)-QEVAP) * RTMSZ
          QOUT(ISALT) = QOUT(ISALT) + (EXTRCT(ISALT)      ) * RTMSZ
          BSASLT (ISALT) = BSASLT(ISALT) + (DISCH(ISALT) * SALTD(ISALT)) * RTMSZ
        ELSEIF (KIND .EQ. 4) THEN
    ! open water
          VOL(ISALT) = VOL(ISALT) + VOLOW(INR) + ActualExtraBergendVolume(inr)
          VOL0(ISALT) = VOL0(ISALT) + VOLOW0(INR) + PreviousExtraBergendVolume(inr)
          QEVAP = VOW (INR)/RTMSZ
          QINS(ISALT) = QINS(ISALT) + (DISCH (ISALT)-QEVAP) * RTMSZ
          QOUT(ISALT) = QOUT(ISALT) + (EXTRCT(ISALT)      ) * RTMSZ
          BSASLT (ISALT) = BSASLT(ISALT) + (DISCH(ISALT) * SALTD(ISALT)) * RTMSZ
        ELSEIF (KIND .EQ. 14) THEN
    ! RWZI
          VOL (ISALT) = 0.001
          VOL0(ISALT) = 0.001
          QINS(ISALT) = QINS(ISALT) +  DISCH (ISALT) * RTMSZ
          QOUT(ISALT) = QOUT(ISALT) +  EXTRCT(ISALT) * RTMSZ
          BSASLT (ISALT) = BSASLT(ISALT) + (DISCH(ISALT) * SALTD(ISALT)) * RTMSZ
        ELSEIF (KIND .EQ. 16) THEN
    ! Sacramento
    ! not yet filled in
        ENDIF

        IF (iDebug /= 0  .AND. ISalt .le. NcSalt) THEN
           WRITE(IDEBUG,1102) ISALT, QINS(ISALT), QOUT(ISALT), &
                 DISCH(ISALT), EXTRCT(ISALT), QEVAP, &
                 SALTD(ISALT), BSASLT(ISALT), VOL(ISALT), VOL0(ISALT)
  1102     FORMAT(I4,10(1X,E10.3))
        ENDIF

        IF (Vol(isalt) .lt. -0.001 .or. Vol0(isalt) .lt. -0.001) then
           WRITE(IOUT1,11021) ITMSTP, Isalt, Einode(inode,1)
  11021    FORMAT(//,' MSG of saltcomputations:',/,   &
                 ' Error/Warning: a negative watervolume may lead to strange results', ' Timestep:', I8,/,   &
                 ' Salt index and node = ',2I5)
        ENDIF
      ENDIF
    ENDDO

    IF (iDebug /= 0) THEN
       WRITE(IDEBUG,1103)
  1103    FORMAT(' ISALT   QINS      QOUT      BSA       VOL  VOL0')
       DO ISALT = 1,NCSALT
         WRITE(IDEBUG,1104) ISALT, QINS(ISALT), QOUT(ISALT), &
                           BSASLT(ISALT), VOL(ISALT), VOL0(ISALT)
  1104      FORMAT(1X,I4,5(1X,E10.3))
       ENDDO
    ENDIF

    ! ********************************************************************
    ! **  OPBOUWEN MATRIX SA
    ! **  DEBIETEN TUSSEN BAKJES
    ! ********************************************************************

    IF (iDebug /= 0) WRITE(IDEBUG,*) '   From node to node  FLOW'
    DO INODE=1,NCNODE
      ISALT = INDSLT(INODE)
      IF (ISALT .GT. 0) THEN
        KIND = EiNode(INODE,3)
        INR  = EiNode(INODE,2)
        IF (KIND .LE. 3 .or. KIND .EQ. 14) THEN
        ! Voor verhard, onverhard, kas en RWZI: index benedenstrooms open water
        ! bepaal gerelateerd open water index INODEW, if existing!
           IOW = EIOW(INODE)
           INODEW = 0
           IF (IOW .GT. 0) THEN
              Found = .false.
              I = 0
              DO While (.not. Found .and. I .lt. NcNode)
                 I=I+1
                 IF (EiNode(I,3) .EQ. 4 .AND. &
                     EiNode(I,2) .EQ. IOW) THEN
                    INODEW = I
                    Found = .true.
                 ENDIF
              ENDDO
           ENDIF
        ! bepaal eventueel gerelateerd RWZI bij verhard knoop
           InodeRwzi = 0
           IF (KIND .eq. 1)  THEN
                Irwzi = VHGRWZI(Inr)
                IF (Irwzi .GT. 0) THEN
                   Found = .false.
                   I = 0
                   DO While (.not. Found .and. I .lt. NcNode)
                      I=I+1
                      IF (EiNode(I,3) .EQ. 14 .AND. &
                         EiNode(I,2) .EQ. Irwzi) THEN
                        InodeRwzi = I
                        Found = .true.
                      ENDIF
                   ENDDO
                ENDIF
           ENDIF
        ! bepaal eventueel gerelateerd InodeW2 bij unpaved knoop
           InodeW2 = 0
           IF (KIND .eq. 2 .and. SWLinkFromExists(inode)) then
               IOW2 = EIOWSWLink(INODE)
               InodeW2 = 0
               IF (IOW2 .GT. 0) THEN
                  Found = .false.
                  I = 0
                  DO While (.not. Found .and. I .lt. NcNode)
                     I=I+1
                     IF (EiNode(I,3) .EQ. 4 .AND. &
                         EiNode(I,2) .EQ. IOW2) THEN
                        INODEW2 = I
                        Found = .true.
                     ENDIF
                  ENDDO
               ENDIF
           ENDIF

           IF (iDebug /= 0) THEN
              WRITE(IDEBUG,*) ' INODE, INODEW InodeRwzi, INodeW2'
              WRITE(IDEBUG,*) INODE, INODEW, InodeRwzi, INodeW2
           ENDIF
        ELSEIF (KIND .EQ. 5) THEN
    ! bepaal gerelateerd boven/benedenstrooms open water index INODEW
           INODEU = UPNODE(INODE)
           INODEW = DONODE(INODE)
        ENDIF

        IF (KIND .EQ. 1) THEN
    ! verhard gebied
    ! debiet van straat naar RWA/gemengd riool
          QINF  = INV(INR,1)/RTMSZ
          IF  (QINF .GT. 0.0) THEN
            IHLP1 = ISALT
            IHLP2 = ISALT+1
            QOUT(IHLP1) = QOUT(IHLP1) + QINF * RTMSZ
            QINS(IHLP2) = QINS(IHLP2) + QINF * RTMSZ
            SASLT (IHLP2,IHLP1) = - QINF * RTMSZ
            IF (iDebug /= 0) WRITE(IDEBUG,*) IHLP1, IHLP2, QINF
          ENDIF
    ! debiet van RWA/gemengd riool naar downstream open water, if existing
          QRUIT = Q1V(INR,1)
          IF (sewer(iNr)%Q2VOW(1) .eq. 1) QRUIT = QRUIT + Q2V(INR,1)
          IF  (QRUIT .GT. 0.0 .and. INODEW .GT. 0) THEN
            IHLP1 = ISALT+1
            IHLP2 = INDSLT (INODEW)
            QOUT(IHLP1) = QOUT(IHLP1) + QRUIT * RTMSZ
            QINS(IHLP2) = QINS(IHLP2) + QRUIT * RTMSZ
            SASLT (IHLP2,IHLP1) = - QRUIT * RTMSZ
            IF (iDebug /= 0) WRITE(IDEBUG,*) IHLP1, IHLP2, QRUIT
          ENDIF
    ! debiet van RWA/gemengd riool naar RWZI, if existing
          QRUIT = 0.0
          IF (sewer(iNr)%Q2VOW(1) .eq. 2) QRUIT = Q2V(INR,1)
          IF  (QRUIT .GT. 0.0 .and. InodeRwzi .GT. 0) THEN
            IHLP1 = ISALT+1
            IHLP2 = INDSLT (InodeRwzi)
            QOUT(IHLP1) = QOUT(IHLP1) + QRUIT * RTMSZ
            QINS(IHLP2) = QINS(IHLP2) + QRUIT * RTMSZ
            SASLT (IHLP2,IHLP1) = - QRUIT * RTMSZ
            IF (iDebug /= 0) WRITE(IDEBUG,*) IHLP1, IHLP2, QRUIT
          ENDIF
        ! debiet van RWA/gemengd riool naar DWA riool (alleen verbeterd gescheiden stelsel)
          QRUIT = INV(INR,2)/RTMSZ
          IF  (QRUIT .GT. 0.0) THEN
            IHLP1 = ISALT+1
            IHLP2 = ISALT+2
            QOUT(IHLP1) = QOUT(IHLP1) + QRUIT * RTMSZ
            QINS(IHLP2) = QINS(IHLP2) + QRUIT * RTMSZ
            SASLT (IHLP2,IHLP1) = - QRUIT * RTMSZ
            IF (iDebug /= 0) WRITE(IDEBUG,*) IHLP1, IHLP2, QRUIT
          ENDIF
        ! debiet van DWA riool naar downstream open water, if existing
          QRUIT = Q1V(INR,2)
          IF (sewer(iNr)%Q2VOW(2) .eq. 1) QRUIT = QRUIT + Q2V(INR,2)
          IF  (QRUIT .GT. 0.0 .and. INODEW .GT. 0) THEN
            IHLP1 = ISALT+2
            IHLP2 = INDSLT (INODEW)
            QOUT(IHLP1) = QOUT(IHLP1) + QRUIT * RTMSZ
            QINS(IHLP2) = QINS(IHLP2) + QRUIT * RTMSZ
            SASLT (IHLP2,IHLP1) = - QRUIT * RTMSZ
            IF (iDebug /= 0) WRITE(IDEBUG,*) IHLP1, IHLP2, QRUIT
          ENDIF
    ! debiet van DWA riool naar RWZI, if existing
          QRUIT = 0.0
          IF (sewer(iNr)%Q2VOW(2) .eq. 2) QRUIT = Q2V(INR,2)
          IF  (QRUIT .GT. 0.0 .and. InodeRwzi .GT. 0) THEN
            IHLP1 = ISALT+2
            IHLP2 = INDSLT (InodeRwzi)
            QOUT(IHLP1) = QOUT(IHLP1) + QRUIT * RTMSZ
            QINS(IHLP2) = QINS(IHLP2) + QRUIT * RTMSZ
            SASLT (IHLP2,IHLP1) = - QRUIT * RTMSZ
            IF (iDebug /= 0) WRITE(IDEBUG,*) IHLP1, IHLP2, QRUIT
          ENDIF

        ELSEIF (KIND .EQ. 2) THEN
    ! onverhard gebied
    ! debiet van land naar onverzadigde zone
          QINF  = INO(INR)/RTMSZ
          IF  (QINF .GT. 0.0) THEN
            IHLP1 = ISALT
            IHLP2 = ISALT+1
            QOUT(IHLP1) = QOUT(IHLP1) + QINF * RTMSZ
            QINS(IHLP2) = QINS(IHLP2) + QINF * RTMSZ
            SASLT (IHLP2,IHLP1) = - QINF * RTMSZ
            IF (iDebug /= 0) WRITE(IDEBUG,*) IHLP1, IHLP2, QINF, ' van land naar onv.zone'
          ENDIF
    ! debiet van land naar downstream open water, if existing
          QLAND = Q1O(INR)
          IF  (QLAND .GT. 0.0 .and. INODEW2 .GT. 0) THEN
            ! via surface runoff link to open water?
            IHLP1 = ISALT
            IHLP2 = INDSLT (INODEW2)
            QOUT(IHLP1) = QOUT(IHLP1) + QLAND * RTMSZ
            QINS(IHLP2) = QINS(IHLP2) + QLAND * RTMSZ
            SASLT (IHLP2,IHLP1) = - QLAND * RTMSZ
            IF (iDebug /= 0) WRITE(IDEBUG,*) IHLP1, IHLP2, QLAND
          ElseIF  (QLAND .GT. 0.0 .and. INODEW .GT. 0) THEN
            ! or via normal RRlink to open water
            IHLP1 = ISALT
            IHLP2 = INDSLT (INODEW)
            QOUT(IHLP1) = QOUT(IHLP1) + QLAND * RTMSZ
            QINS(IHLP2) = QINS(IHLP2) + QLAND * RTMSZ
            SASLT (IHLP2,IHLP1) = - QLAND * RTMSZ
            IF (iDebug /= 0) WRITE(IDEBUG,*) IHLP1, IHLP2, QLAND
          ENDIF
        ! debiet van onverzadigde zone naar bodem; of andersom!
          QINF  = QINB(INR)/RTMSZ
          IF (ABS(QINF) .GT. 0.0) then
            IF  (QINF .GT. 0.0) THEN      !percolatie
               IHLP1 = ISALT+1
               IHLP2 = ISALT+2
               QOUT(IHLP1) = QOUT(IHLP1) + QINF * RTMSZ
               QINS(IHLP2) = QINS(IHLP2) + QINF * RTMSZ
               SASLT (IHLP2,IHLP1) = - QINF * RTMSZ
               IF (iDebug /= 0) &
                 WRITE(IDEBUG,*) IHLP1, IHLP2, QINF, ' percolatie'
            ELSEIF (QINF .LT. 0.0) THEN  !capillaire opstijging
               IHLP1 = ISALT+2
               IHLP2 = ISALT+1
               QOUT(IHLP1) = QOUT(IHLP1) - QINF * RTMSZ
               QINS(IHLP2) = QINS(IHLP2) - QINF * RTMSZ
               SASLT (IHLP2,IHLP1) = + QINF * RTMSZ
               IF (iDebug /= 0) WRITE(IDEBUG,*) IHLP1, IHLP2, QINF, ' cap.opstijging'
            ENDIF
          ENDIF
  ! debiet van bodem naar downstream open water, if existing
  ! ARS 15464: No action, since always related to INodeW and never to surface runoff link
          QBUIT = Q2O(INR)
          IF  (QBUIT .GT. 0.0 .and. INODEW .GT. 0) THEN
            IHLP1 = ISALT+2
            IHLP2 = INDSLT (INODEW)
            QOUT(IHLP1) = QOUT(IHLP1) + QBUIT * RTMSZ
            QINS(IHLP2) = QINS(IHLP2) + QBUIT * RTMSZ
            SASLT (IHLP2,IHLP1) = - QBUIT * RTMSZ
            IF (iDebug /= 0) WRITE(IDEBUG,*) IHLP1, IHLP2, QBUIT
          ELSEIF  (QBUIT .LT. 0.0 .and. INODEW .GT. 0) THEN
    ! debiet van open water naar bodem! (if existing open water)
            IHLP1 = INDSLT (INODEW)
            IHLP2 = ISALT+2
            QOUT(IHLP1) = QOUT(IHLP1) - QBUIT * RTMSZ
            QINS(IHLP2) = QINS(IHLP2) - QBUIT * RTMSZ
            SASLT (IHLP2,IHLP1) = + QBUIT * RTMSZ
            IF (iDebug /= 0) WRITE(IDEBUG,*) IHLP1, IHLP2, -QBUIT
          ENDIF

        ELSEIF (KIND .EQ. 3) THEN
    ! kasgebied
    ! debiet van daken naar bassins
          DO IKKL=1,NKKL
            QINF  = INK(INR,IKKL)/RTMSZ
            IF  (QINF .GT. 0.0) THEN
              IHLP1 = ISALT
              IHLP2 = ISALT+IKKL
              QOUT(IHLP1) = QOUT(IHLP1) + QINF * RTMSZ
              QINS(IHLP2) = QINS(IHLP2) + QINF * RTMSZ
              SASLT (IHLP2,IHLP1) = - QINF * RTMSZ
              IF (iDebug /= 0)  WRITE(IDEBUG,*) IHLP1, IHLP2, QINF
            ENDIF
          ENDDO
    ! debiet van daken naar silo
          QINF  = INKS(INR)/RTMSZ
          IF  (QINF .GT. 0.0) THEN
            IHLP1 = ISALT
            IHLP2 = ISALT+NKKL+1
            QOUT(IHLP1) = QOUT(IHLP1) + QINF * RTMSZ
            QINS(IHLP2) = QINS(IHLP2) + QINF * RTMSZ
            SASLT (IHLP2,IHLP1) = - QINF * RTMSZ
            IF (iDebug /= 0) WRITE(IDEBUG,*) ' Qroof-silo',IHLP1,IHLP2,QINF
          ENDIF
    ! debiet van bassins naar downstream open water, if existing
          DO IKKL=1,NKKL
            QKUIT  = QKAS(INR,IKKL)
            IF  (QKUIT .GT. 0.0 .and. INODEW .GT. 0) THEN
              IHLP1 = ISALT+IKKL
              IHLP2 = INDSLT (INODEW)
              QOUT(IHLP1) = QOUT(IHLP1) + QKUIT * RTMSZ
              QINS(IHLP2) = QINS(IHLP2) + QKUIT * RTMSZ
              SASLT (IHLP2,IHLP1) = - QKUIT * RTMSZ
              IF (iDebug /= 0) WRITE(IDEBUG,*) IHLP1, IHLP2, QKUIT
            ENDIF
          ENDDO
    ! debiet van silo naar downstream open water, if existing
          QKUIT  = QSILOW(INR)
          IF  (QKUIT .GT. 0.0 .and. INODEW .GT. 0) THEN
            IHLP1 = ISALT+NKKL+1
            IHLP2 = INDSLT (INODEW)
            QOUT(IHLP1) = QOUT(IHLP1) + QKUIT * RTMSZ
            QINS(IHLP2) = QINS(IHLP2) + QKUIT * RTMSZ
            SASLT (IHLP2,IHLP1) = - QKUIT * RTMSZ
            IF (iDebug /= 0) WRITE(IDEBUG,*) ' QSilo-ow',IHLP1, IHLP2, QKUIT
          ENDIF

        ELSEIF (KIND .EQ. 5) THEN
    ! debiet van open water 1 naar open water 2, via structure X
          QSUIT  = QSTRU(INR)
          IF  (QSUIT .GT. 0.0) THEN
            IHLP1 = INDSLT (INODEU)
            IHLP2 = INDSLT (INODEW)
            QOUT(IHLP1) = QOUT(IHLP1) + QSUIT * RTMSZ
            QINS(IHLP2) = QINS(IHLP2) + QSUIT * RTMSZ
            SASLT (IHLP2,IHLP1) = - QSUIT * RTMSZ
            IF (iDebug /= 0) WRITE(IDEBUG,*) IHLP1, IHLP2, QSUIT
          ELSEIF  (QSUIT .LT. 0.0) THEN
    ! debiet van open water 2 naar open water 1, via structure X
            IHLP1 = INDSLT (INODEW)
            IHLP2 = INDSLT (INODEU)
            QOUT(IHLP1) = QOUT(IHLP1) - QSUIT * RTMSZ
            QINS(IHLP2) = QINS(IHLP2) - QSUIT * RTMSZ
            SASLT (IHLP2,IHLP1) = + QSUIT * RTMSZ
            IF (iDebug /= 0) WRITE(IDEBUG,*) IHLP1, IHLP2, -QSUIT
          ENDIF

        ELSEIF (KIND .EQ. 14) THEN
        ! Debiet van RWZI naar downstream open water, if existing
          QRUIT = QEffluent(INr)
          if ( Abs (QEffluent(INr)-Qrwzi(INr)) .gt. .1)  RwziWarning = .true.
          IF  (QRUIT .GT. 0.0 .and. INODEW .GT. 0) THEN
            IHLP1 = ISALT
            IHLP2 = INDSLT (INODEW)
            QOUT(IHLP1) = QOUT(IHLP1) + QRUIT * RTMSZ
            QINS(IHLP2) = QINS(IHLP2) + QRUIT * RTMSZ
            SASLT (IHLP2,IHLP1) = - QRUIT * RTMSZ
            IF (iDebug /= 0) WRITE(IDEBUG,*) IHLP1, IHLP2, QRUIT
          ENDIF
        ELSEIF (KIND .EQ. 15) THEN
        ! Industry
        ELSEIF (KIND .EQ. 16) THEN
        ! Sacramento
        ENDIF
      ENDIF
    ENDDO

    DO ILink=1,NCLink
!     if (itmstp .eq. 720) Write(Idebug,*) ' check link ', ilink, LinkType(ilink)
      if (LinkType(ilink) .eq. 31) then
         InodeUp   = LnkFrm(ilink)
         InodeDown = LnkTo(ilink)
         ! InodeUp and InodeDown are unpaved; find salt indices of groundwater layer
         IKindUp   = Einode(InodeUp,3)
         IKindDown = Einode(InodeDown,3)
         ISaltUp   = IndSlt(InodeUp) + 2
         ISaltDown = IndSlt(InodeDown) + 2
!        if (itmstp .eq. 720) Write(Idebug,*) ' check link ikind/isalt indices', ikindup, ikinddown, isaltup, isaltdown
         IF (ISALTUp .GT. 0 .and. ISaltDown .gt. 0 .and. IKindUp .eq. 2 .and. IKindDown .eq. 2) then
            Q = QinLink(ilink)
            IF  (Q .GT. 0.0) THEN
               QOUT(ISaltUp) = QOUT(ISaltUp) + Q * RTMSZ
               QINS(ISaltDown) = QINS(ISaltDown) + Q * RTMSZ
               SASLT (ISaltDown,ISaltUp) = - Q * RTMSZ
               IF (iDebug /= 0) WRITE(IDEBUG,*) ' GWLink positive flow',ilink, ISaltUp, ISaltDown, Q
            ELSEIF  (Q .LT. 0.0) THEN
              ! negative flow
              QOUT(ISaltDown) = QOUT(ISaltDown) - Q * RTMSZ
              QINS(ISaltUp) = QINS(ISaltUp) - Q * RTMSZ
              SASLT (ISaltUp,ISaltDown) = + Q * RTMSZ
              IF (iDebug /= 0) WRITE(IDEBUG,*) ' GWlink negative flow', ilink, ISaltDown, ISaltUp, -Q
            ENDIF
         Endif
      Endif
    EndDo

    ! *********************************************************************
    ! ** DEBUG UITVOER
    ! *********************************************************************
    IF (iDebug /= 0) THEN
      WRITE(IDEBUG,191)
  191 FORMAT(' ISALT     QINS     QOUT      BSA    VOL ',/, &
            ' ======================================== '/)
      DO ISALT=1,NCSALT
          WRITE(IDEBUG,193) ISALT, QINS(ISALT), &
                           QOUT(ISALT), BSASLT(ISALT), VOL(ISALT)
      ENDDO
  193 FORMAT(1X,I5,2X,4E10.3)
    ENDIF

    ! ********************************************************************
    ! **  INBRENGEN STORAGE
    ! ********************************************************************

    IF (iDebug /= 0) THEN
      WRITE(IDEBUG,*) ' Add STORAGE '
      WRITE(IDEBUG,*) ' ISALT  QINS   QOUT   BSA   VOL   SALTF  COEFF'
    ENDIF
    DO 200  I = 1, NCSALT
      SASLT(I,I) = QOUT(I)
    !testgp
      IF (VOL(I) .GT. 0.001 .OR. VOL0(I) .GT. .001) THEN
        VOL(I) = Dble ( AMAX1 (0.001E0, Sngl(VOL(I)) ) )
    !testgp
        CALL STCOEF (I, VOL(I), QINS(I), QOUT(I), 0.0D0, COEFF, &
                                        IOUT1, IDEBUG, ITMSTP)
        COEF(1,I) = COEFF(1)
        COEF(2,I) = COEFF(2)
        COEF(3,I) = COEFF(3)
        COEF(4,I) = COEFF(4)
    !gp addition GP: bij initieel leeg bakje zet init.conc. op regenvalconcentratie!
        IF (VOL0(I) .LE. 0.0) SALTF(I) = SLTRAI
        BSASLT(I) = BSASLT(I) + SALTF(I) * COEF(4,I)
        SASLT(I,I) = COEF(3,I)
        IF (ABS(SASLT(I,I)) .LE. 0.000001)  SASLT(I,I) = RTMSZ
      ENDIF
      IF (iDebug /= 0) THEN
         ISALT = I
         WRITE(IDEBUG,2001) ISALT,QINS(ISALT), QOUT(ISALT), &
                           BSASLT(ISALT), VOL(ISALT), SALTF(ISALT), &
                           COEF(1,I), COEF(2,I), COEF(3,I),COEF(4,I)
  2001   FORMAT(1X,I4,9(1X,E11.4))
      ENDIF
  200 CONTINUE

    ! ********************************************************************
    ! **  LOS STELSEL OP TER BEPALING GEMIDDELDE ZOUTCONCENTRATIES (SALT)
    ! **  BEPAAL CONCENTRATIE AAN EINDE TIJDSTAP  (SALTF)
    ! ********************************************************************

    IF (iDebug /= 0) THEN
      WRITE(IDEBUG,*) ' Matrix to solve: SA * SALT = BSASLT'
      DO ISALT=1,NCSALT
        WRITE(IDEBUG,'(20(G15.5,1X))')  (SASLT(ISALT,J),J=1,NCSALT), &
                                                      BSASLT(ISALT)
      ENDDO
    ENDIF

    if (RwziWarning) then
       NrRwziWarnings = NrRwziWarnings + 1
       if (NrRwziWarnings .lt. 10) then
          call ErrMsgStandard (977, 0, ' Measured effluent RR-WWTP is different from calculated influent', &
                               ' This may cause errors in salt computations ')
       elseif (NrRwziWarnings .eq. 10) then
          call ErrMsgStandard (977, 0, ' Warning not repeated anymore: Measured effluent RR-WWTP is different from calculated influent',&
                               ' Message This may cause errors in salt computations ')
       endif
    endif

    IF (iDebug /= 0) WRITE(IDEBUG,*)  ' Call LU '
    CALL LU (NCSALT, SASLT, BSASLT, NCSALT, SALT, NRIJ2, ITMSTP)
    IF (iDebug /= 0) WRITE(IDEBUG,*)  ' After LU '

    SaltF0 = SaltF
    DO I = 1, NCSALT
       SALTF(I) = COEF(1,I) * SALT(I) + COEF(2,I) * SALTF(I)
    !gp: add check
       IF ((SALTF(I) .LT. -0.000001)) THEN
         IF ((ConfFil_get_iOut1() > 0)) then
           WRITE(IOUT1,101) I, ITMSTP
  101      FORMAT(' MSG of saltcomputations :',/, &
               ' Set final saltconcentration to 0 for ', &
               ' node',I4,'timestep ',I5)
         ENDIF
         SALTF(I) = 0.0
       ENDIF
       IF (SALT(I) .LT. -0.000001) THEN
         SALT(I) = 0.0
       ENDIF
    end do

    IF (iDebug /= 0) THEN
      WRITE(iDebug,284)
  284 FORMAT(' ISALT  InitSaltc. Avg.Saltconc. FinalSaltc',/, &
            ' ============================================ '/)
      DO ISALT=1,NCSALT
        WRITE(Idebug,286) ISALT, SaltF0(ISALT),SALT(ISALT), SALTF(ISALT)
      ENDDO
  286 FORMAT(1X,I6,2X,3F10.3)
    ENDIF

  !Uitvoer zout
    IF (IFLZT > 0) THEN
       WRITE(IFLZT,*) 'Timestep', ITMSTP
       WRITE(IFLZT,284)
       DO ISALT=1,NCSALT
         WRITE(IFLZT,286) ISALT, SALT(ISALT), SALTF(ISALT)
       ENDDO
    ENDIF
    RETURN
  END subroutine saline



  SUBROUTINE LU (IORDE,   ALU,     B,    NX,    X,  NRIJ, ITMSTP)
    !
    ! **********************************************************************
    ! ** SUBROUTINE VOOR HET OPLOSSEN VAN HET STELSEL VERGELIJKINGEN
    ! ** WAARUIT DE ZOUTCONCENTRATIES BEPAALD WORDEN.
    ! **********************************************************************
    !     BIJ AANROEP: IORDE = NCSALT


    Integer iOrde, nx, nRij, iTmStp, i, j, k, l, isalt
    Double precision    X, tol, ahulp, som, s, alu, b

    DIMENSION ALU(IORDE,IORDE)
    DIMENSION X(NX),B(IORDE), NRIJ(IORDE)

    DIMENSION S(NSLT)
    Double precision  MJI
    Integer iDebug, iOut1

    iDebug = ConfFil_get_iDebug()
    iOut1 = ConfFil_get_iOut1()

    TOL = .004

    DO 10  I=1,IORDE
       NRIJ(I) = I
    10 CONTINUE

    DO 70  I=1, IORDE-1
    ! BEPAAL PIVOT-ELEMENT
      AHULP = 0.0
      L = I

      DO 20  K=I,IORDE
        IF (ABS(ALU(K,I)) .LE. AHULP) GOTO 20
        L = K
        AHULP = ABS(ALU(K,I))
    20 CONTINUE

      IF (AHULP .EQ. 0.0) GOTO 70
      IF (L .EQ. I) GOTO 40
      K = NRIJ(L)
      NRIJ(L)  = NRIJ(I)
      NRIJ(I) = K
    !
      DO 30  K=1,IORDE
        AHULP = ALU(I,K)
        ALU(I,K) = ALU(L,K)
        ALU(L,K) = AHULP
    30   CONTINUE
    !
    40   CONTINUE
      DO 60  J=I+1,IORDE
        IF (ABS(ALU(J,I)) .GT. 0.0)  THEN
           MJI = ALU(J,I) / ALU(I,I)
           DO 50  K=I,IORDE
             ALU(J,K) = ALU(J,K) - MJI*ALU(I,K)
    50        CONTINUE
           ALU(J,I) = MJI
        ENDIF
    60 CONTINUE
    70 CONTINUE
    !
    ! **********************************************************************
    ! ** BEPAAL OPLOSSING VAN STELSEL VAN N VERGELIJKINGEN MET N ONBEKENDEN
    ! **********************************************************************
    !
    DO 90  L=1,IORDE
      X(L) = B(NRIJ(L))
    90 CONTINUE
    !
    DO 110  I=1,IORDE
      SOM = 0.0
      S(I) = 0.0
      DO 100  J=1,I-1
        SOM = SOM + ALU(I,J)*S(J)
    100   CONTINUE
      S(I) = X(I) - SOM
    110 CONTINUE
    !
    DO 130  I=IORDE,1,-1
      SOM = 0.0
      X(I) = 0.0
      DO 120  J=IORDE,I+1,-1
        SOM = SOM + ALU(I,J)*X(J)
    120   CONTINUE
      X(I) = S(I) - SOM
      AHULP = ALU(I,I)
      IF (ABS(AHULP) .EQ. 0.0)  THEN
        IF (ABS(X(I)) .GT. TOL) THEN
          if (iOut1 .ne. 0) WRITE(IOUT1,999) ITMSTP
          IF (iDebug .ne. 0) WRITE(IDEBUG,999)  ITMSTP
          WRITE(*,999) ITMSTP
    999   FORMAT(' Error in LU, probably related to emptying surface storage: undefined matrix in timestep',I6)
          if (iOut1 .ne. 0)  WRITE(IOUT1,*)  I, X(I), AHULP
          X(I) = 0
        ENDIF
        AHULP = 1.0
      ENDIF
      X(I) = X(I) / AHULP
    130 CONTINUE
    140 CONTINUE
    !
    ! **********************************************************************
    ! ** DEBUG UITVOER
    ! **********************************************************************
    !
    IF (iDebug /= 0) THEN
      WRITE(IDEBUG,1001)
    1001   FORMAT(' Solution of Matrix:',//,   &
            '  ISALT    SALT(ISALT) ',/,    &
            ' --------------------- ',/)
    1002        FORMAT(I5,1X,F10.2)
    !
      DO ISALT=1,IORDE
         WRITE(IDEBUG,1002) ISALT, X(ISALT)
      ENDDO
    ENDIF
    !
    !
    RETURN
  END subroutine lu



  SUBROUTINE DtCbnd
    ! *********************************************************************
    ! *** DELFT_3B   version 2.25
    ! ***   Bepaal zoutconcentraties op randen (voorheen in main Delft_3B)
    ! *********************************************************************

    IMPLICIT none

    Integer iDebug, iOut1, Inod, ikind, inr, ikkl, teller, teller2, idown, iup
    Double Precision    QKuit

    iDebug = ConfFil_get_iDebug()
    iOut1 = ConfFil_get_iOut1()

    IF (iDebug /= 0) WRITE(IDEBUG,*) ' CALL DtCBnd '


! Bepaal CBND; de zoutconcentratie op de randen
    IF (ISLCMP /= 0) THEN
         DO INOD =1,NCNODE
            IKIND = EiNode(INOD,3)
            INR   = EiNode(INOD,2)
! flow van paved area
            IF (IKIND .EQ. 1) THEN
                teller = VHGBND(INR)
                IF (sewer(iNr)%Q2VOW(1) .eq. 0) CBND(teller) = CBND(teller) + Q2V(INR,1) * SALTF(INDSLT(INOD)+1)
                IF (sewer(iNr)%Q2VOW(2) .eq. 0) CBND(teller) = CBND(teller) + Q2V(INR,2) * SALTF(INDSLT(INOD)+2)
 ! flow van unpaved area
            ELSEIF (IKIND .EQ. 2) THEN
               teller = EiBND(INOD)
               teller2 = EIBNDSWLink(INOD)
               IF (teller .gt. 0 .and. .not. SWLinkFromExists(Inod)) then
                  ! old default situation, all unpaved outflow on one link
                  CBND(teller) = CBND(teller) + Q1O (INR)  * SALTF(INDSLT(INOD))
                  IF (Q2O(INR) .GE. 0) CBND(teller) = CBND(teller) + Q2O (INR) * SALTF(INDSLT(INOD)+2)
               Else
                  ! ARS 15464: separate unpaved surface runoff link (to node with Bnd-index teller2, flow Q1O)
                   IF (teller .gt. 0) then
                      IF (Q2O(INR) .GE. 0) CBND(teller) = CBND(teller) + Q2O (INR) * SALTF(INDSLT(INOD)+2)
                   endif
                   IF (teller2 .gt. 0) CBND(teller2) = CBND(teller2) + Q1O (INR)  * SALTF(INDSLT(INOD))
               ENDIF
 ! flow van greenhouse area
            ELSEIF (IKIND .EQ. 3) THEN
               teller = EiBND(INOD)
               IF (teller .gt. 0) then
                DO IKKL=1,NKKL
                   QKUIT = QKAS(INR,IKKL)
                   IF (QKUIT .GT. 0.0) CBND(teller) = CBND(teller) + QKUIT * SALTF(INDSLT(INOD)+IKKL)
                ENDDO
                QKUIT  = QSILOW(INR)
                IF (QKUIT .GT. 0.0)    CBND(teller) = CBND(teller) + QKUIT * SALTF(INDSLT(INOD)+NKKL+1)
               ENDIF
 ! flow van structures
            ELSEIF (IKIND .EQ. 5) THEN
               IDOWN = DONODE(INOD)
               IUP   = UPNODE(INOD)
               IF (EiNode(IDOWN,3) .EQ. 6) THEN
                  teller = EiNode(IDOWN,2)
                  CBND(teller) = CBND(teller) +QSTRU(INR) * SALTF(INDSLT(IUP))
               ELSEIF (EiNode(IUP,3) .EQ. 6) THEN
                  TELLER = EiNode(IUP,2)
                  CBND(teller) = SLTBND(teller)
                  IF (iDebug /=  0) WRITE(IDEBUG, *) ' teller CBND=',teller, CBND(teller)
               ENDIF
!  flow van RWZI
            ELSEIF (IKIND .EQ. 14) THEN
               teller = eiBND(INOD)
               IF (teller .gt. 0)  CBND(teller) = CBND(teller) + QEffluent(INR) * SALTF(INDSLT(INOD))
! flow van Industry
            ELSEIF (IKIND .EQ. 15) THEN
               teller = eiBND(INOD)
               IF (teller .gt. 0) CBND(teller) = CBND(teller) + QIndDis(INR) * SltIndDis(INR)
            ENDIF
        ENDDO
    ENDIF

! conversie van vracht naar concentratie: deel door debiet
    DO teller =1,NCBOUN
       IF (QBND(teller) .GT. 0) then
          CBND(teller) = CBND(teller) / QBND(teller)
       Else  ! net inflow from boundary
          CBND(teller) = SLTBND(teller)
       Endif
    ENDDO


    RETURN
  END subroutine DtCBnd




   SUBROUTINE DTSLTD (DISCH, EXTRCT, RTMSZ, RwziWarning)
!
! *********************************************************************
! *** Delft_3b
! *********************************************************************
! ** Determine salt discharges at nodes
! *********************************************************************
! ** Bepaling totale discharges op knoop isalt
! ** Bepaling gemiddelde zoutconcentratie lozing op knoop isalt
! ** ISALT = index bakje
! ** DISCH = array with discharges at nodes
! ** EXTRCT = array with extractions at node
! ** RTMSZ = conversion factor (=timestepsize as a Double Precision variable)
! *********************************************************************

!
      Implicit none

      Double Precision  RTMSZ
      Double Precision  DISCH (NSLT)
      Double Precision  EXTRCT(NSLT)
      CHARACTER(20) NAME
      Integer iSalt, iNode, kind, iNr, iKKl, iow, ibnd, i, teller, NodeUp, NodeDown, INodeOw
      Double Precision    qRain, rKwel, rWegz, QDWA, QRuit, Qland, QBuit, QKuit, QIrri
      Integer iDebug
      Logical RwziWarning
!
! ********************************************************************
! ** INITIALISATIE
! ********************************************************************
!
      I = 0

! Vector initialisation
      SALTD  = 0.0
      EXTRCT = 0.0
      DISCH  = 0.0
!
! ********************************************************************
! ** Bepaal gemiddelde zoutconcentratie van de lozingen  (excl. regen)
! ** Bepalen totale lozingen en onttrekkingen op zoutknopen
! ** Lozingen/onttrekkingen: inlaten via boundaries
! ** Regenval en verdamping worden in de routine saline meegenomen
! ** daar rechterlid = discharge * av. salt conc. discharge
! **                  (= zoutlast)
! ********************************************************************
! *** NETWERK
! ********************************************************************
!

      iDebug = ConfFil_get_iDebug()
      IF (iDebug /= 0) THEN
        WRITE(IDEBUG,*) ' DTSLTD: RTMSZ sltrai=', RTMSZ, sltrai
        WRITE(IDEBUG,49)
   49   FORMAT(' KNOOP  ISALT    SALTD    DISCH    EXTRCT',/,     &
              ' ----------------------------------------')
      ENDIF

      DO INODE = 1, NCNODE
         KIND  = EiNode (INODE,3)
         ISALT = INDSLT (INODE)
         INR = EiNode(INODE,2)
         IF (ISALT .EQ. 0 .and. kind .ne. 15) GOTO 10              ! Industry apart!
         IF (iDebug /= 0)  WRITE(IDEBUG,*) ' ISALT =',ISALT

         IF (KIND .EQ. 1) THEN
! Verhard gebied
! IOW = downstream open water, if existing
! is used here to check that in case no downstream open water, also spilling goes to boundary
            IOW = EIOW(INODE)
! verhard gebied:  regenval op straat
            QRAIN = RV(INR) / RTMSZ
            IF (QRAIN .GT. 0.0) THEN
              SALTD (ISALT) = SALTD(ISALT) * DISCH (ISALT) + SLTRAI * QRAIN
              DISCH (ISALT) = DISCH(ISALT) + QRAIN
              SALTD (ISALT) = SALTD(ISALT) / DISCH(ISALT)
            ENDIF
            IF (iDebug /= 0) THEN
               WRITE(IDEBUG,55) Id_Nod(INODE),ISALT, SALTD(ISALT), DISCH(ISALT), EXTRCT(ISALT)
            ENDIF
!outflow RWA rioolgemaal to boundary;
            ISALT = ISALT + 1
            IF (sewer(iNr)%Q2VOW(1) .eq. 0) EXTRCT(ISALT) = EXTRCT(ISALT) + Q2V(INR,1)
            IF (iDebug /= 0) THEN
               WRITE(IDEBUG,55) Id_Nod(INODE),ISALT, SALTD(ISALT),    &
                               DISCH(ISALT), EXTRCT(ISALT)
            ENDIF
!also add RWA spilling to boundary, if no downstream open water
            IF (IOW .LE. 0) THEN
              QRUIT = Q1V(INR,1)
              IF  (QRUIT .GT. 0.0) EXTRCT(ISALT) = EXTRCT(ISALT) + QRUIT
              IF (iDebug /= 0) THEN
                 WRITE(IDEBUG,55) Id_Nod(INODE),ISALT, SALTD(ISALT),    &
                                  DISCH(ISALT), EXTRCT(ISALT)
              ENDIF
            ENDIF
            QDWA = DWAPaved(INR)
            IF (sewer(inr)%systemType .eq. 0) THEN
!Jan 1998: DWA in gemengd/RWA riool
               IF (QDWA .GT. 0.0) THEN
                 SALTD (ISALT) = SALTD(ISALT) * DISCH (ISALT) + DWASalt(INR) * QDWA
                 DISCH (ISALT) = DISCH(ISALT) + QDWA
                 SALTD (ISALT) = SALTD(ISALT) / DISCH(ISALT)
               ENDIF
               IF (iDebug /= 0) THEN
                 WRITE(IDEBUG,55) Id_Nod(INODE),ISALT, SALTD(ISALT), DISCH(ISALT), EXTRCT(ISALT)
               ENDIF
            ELSE
!DWA in DWA riool
               ISALT = ISALT+1
               IF (QDWA .GT. 0.0) THEN
                 SALTD (ISALT) = SALTD(ISALT) * DISCH (ISALT) + DWASalt(INR) * QDWA
                 DISCH (ISALT) = DISCH(ISALT) + QDWA
                 SALTD (ISALT) = SALTD(ISALT) / DISCH(ISALT)
               ENDIF
               IF (iDebug /= 0) THEN
                  WRITE(IDEBUG,55) Id_Nod(INODE),ISALT, SALTD(ISALT), DISCH(ISALT), EXTRCT(ISALT)
               ENDIF
               ISALT = ISALT-1
            ENDIF
!outflow DWA rioolgemaal to boundary
            ISALT = ISALT + 1
            IF (sewer(iNr)%Q2VOW(2) .eq. 0) EXTRCT(ISALT) = EXTRCT(ISALT) + Q2V(INR,2)
            IF (iDebug /= 0) THEN
               WRITE(IDEBUG,55) Id_Nod(INODE),ISALT, SALTD(ISALT),    &
                               DISCH(ISALT), EXTRCT(ISALT)
            ENDIF
!also add DWA spilling to boundary, if no downstream open water
            IF (IOW .LE. 0) THEN
              QRUIT = Q1V(INR,2)
              IF  (QRUIT .GT. 0.0) EXTRCT(ISALT) = EXTRCT(ISALT) + QRUIT
              IF (iDebug /= 0) THEN
                 WRITE(IDEBUG,55) Id_Nod(INODE),ISALT, SALTD(ISALT),    &
                                  DISCH(ISALT), EXTRCT(ISALT)
              ENDIF
            ENDIF

         ELSEIF (KIND .EQ. 2) THEN
! Onverhard gebied
! IOW = downstream open water, if existing
! is used here to check that in case no downstream open water, flows go to boundary IBND
            IOW  = EIOW(INODE)
            IBND = EIBND(INODE)
! onverhard gebied: regenval op land
            QRAIN =  RO(INR) / RTMSZ
            IF (QRAIN .GT. 0.0) THEN
              SALTD (ISALT) = SALTD(ISALT) * DISCH (ISALT) + SLTRAI * QRAIN
              DISCH (ISALT) = DISCH(ISALT) + QRAIN
              SALTD (ISALT) = SALTD(ISALT) / DISCH(ISALT)
            ENDIF
! onverhard gebied: irrigatie
            QIRRI = IrrigationSupply(inr)
            IF (QIRRI .GT. 0.0) THEN
              If (IrrigationSource(inr) .eq. 1) then
                 IF (IOW .GT. 0) THEN
                    Do InodeOW=1,NCNode
                       If (Einode(inodeOw,2) .eq. iow .and. Einode(inodeow,3) .eq. 4) then
                          IrrigationSaltConcentration(inr) = SaltF(INDSLT(INODEOW))
                          IF (iDebug /= 0) WRITE(IDEBUG,*) ' inodeow, IrrigationSaltConc', InodeOw, IrrigationSaltConcentration(inr)
                       Endif
                    Enddo
                 ELSEIF (IBND .GT. 0) THEN
                    IrrigationSaltConcentration(inr) = SLTBND(IBND)
                 ENDIF
              elseIf (IrrigationSource(inr) .eq. 2) then
                  IrrigationSaltConcentration(inr) = SaltF(ISALT+2)
              endif
              SALTD (ISALT) = SALTD(ISALT) * DISCH (ISALT) + IrrigationSaltConcentration(inr)* QIRRI
              DISCH (ISALT) = DISCH(ISALT) + QIRRI
              SALTD (ISALT) = SALTD(ISALT) / DISCH(ISALT)
            ENDIF
            IF (iDebug /= 0) THEN
               WRITE(IDEBUG,55) Id_Nod(INODE),ISALT, SALTD(ISALT), DISCH(ISALT), EXTRCT(ISALT)
            ENDIF
! onverzadigde zone: geen bijzonderheden
            ISALT = ISALT+1
! bodem: kwel in bodem
            ISALT = ISALT+1
!           RKWEL = KWEL(INR) * AREAOH(INR)
            RKWEL = KWEL(INR) * AreaGwComp(INR)
!           RWEGZ = WEGZG(INR) * AREAOH(INR)
            RWEGZ = WEGZG(INR) * AreaGwComp(INR)
            IF (RKWEL .GT. 0.0) THEN
              SALTD (ISALT) = SALTD(ISALT) * DISCH (ISALT) + SLTKWL(INODE) * RKWEL
              DISCH (ISALT) = DISCH(ISALT) + RKWEL
              SALTD (ISALT) = SALTD(ISALT) / DISCH(ISALT)
            ENDIF
! wegzijging in bodem
            EXTRCT(ISALT) = EXTRCT(ISALT) + RWEGZ
            IF (iDebug /= 0) THEN
               WRITE(IDEBUG,55) Id_Nod(INODE),ISALT, SALTD(ISALT), DISCH(ISALT), EXTRCT(ISALT)
            ENDIF
! grondwateronttrekking irrigatie
            if (IrrigationSource(inr) .eq. 2) EXTRCT(ISALT) = EXTRCT(ISALT) + IrrigationDemand(inr)
            IF (iDebug /= 0) THEN
               WRITE(IDEBUG,55) Id_Nod(INODE),ISALT, SALTD(ISALT), DISCH(ISALT), EXTRCT(ISALT)
            ENDIF

! Unpaved area with no downstream open water, but a boundary
            IF (IOW .LE. 0) THEN
! debiet van land naar downstream boundary
!   ARS 15464: NO action needed, since abstraction from unpaved area
               ISALT = ISALT-2
               QLAND = Q1O(INR)
               IF  (QLAND .GT. 0.0) THEN
                  EXTRCT(ISALT) = EXTRCT(ISALT) + QLAND
                  IF (iDebug /= 0) THEN
                    WRITE(IDEBUG,55) Id_Nod(INODE),ISALT, SALTD(ISALT), DISCH(ISALT), EXTRCT(ISALT)
                  ENDIF
               ENDIF
 ! debiet van bodem naar downstream boundary
!   ARS 15464: NO action needed, since abstraction from unpaved area
               ISALT = ISALT+2
               QBUIT = Q2O(INR)
               IF  (QBUIT .GT. 0.0) THEN
                  EXTRCT(ISALT) = EXTRCT(ISALT) + QBUIT
                  IF (iDebug /= 0) THEN
                    WRITE(IDEBUG,55) Id_Nod(INODE),ISALT, SALTD(ISALT), DISCH(ISALT), EXTRCT(ISALT)
                  ENDIF
               ELSEIF  (QBUIT .LT. 0.0) THEN
 ! debiet van boundary to unpaved area - soil
!   ARS 15464: NO action needed, since flow always from the normal link or soil interaction link, NOT from the surface runoff link
!   SEPT 2010 note: qbuit is negative, so use absolute value
                 SALTD (ISALT) = SALTD(ISALT) * DISCH (ISALT) + SLTBND(IBND) * Abs(QBUIT)
                 DISCH (ISALT) = DISCH(ISALT) + Abs(QBUIT)
                 SALTD (ISALT) = SALTD(ISALT) / DISCH(ISALT)
               ENDIF
            ENDIF

         ELSEIF (KIND .EQ. 3) THEN
! Kasgebied
! IOW = downstream open water, if existing
! is used here to check that in case no downstream open water, flows go to boundary IBND
            IOW  = EIOW(INODE)
            IBND = EIBND(INODE)
! kasgebied: regenval op daken
            QRAIN = RKD(INR) / RTMSZ
            IF (QRAIN .GT. 0.0) THEN
              SALTD (ISALT) = SALTD(ISALT) * DISCH (ISALT) + SLTRAI * QRAIN
              DISCH (ISALT) = DISCH(ISALT) + QRAIN
              SALTD (ISALT) = SALTD(ISALT) / DISCH(ISALT)
            ENDIF
            IF (iDebug /= 0) THEN
               WRITE(IDEBUG,*)  ' precipitation greenhouse-roofs'
               WRITE(IDEBUG,55) Id_Nod(INODE),ISALT, SALTD(ISALT), DISCH(ISALT), EXTRCT(ISALT)
            ENDIF
! kasgebied: regenval op bassins
            DO IKKL=1,NKKL
               ISALT = ISALT + 1
               QRAIN = RKB(INR,IKKL) / RTMSZ
               IF (QRAIN .GT. 0.0) THEN
                 SALTD (ISALT) = SALTD(ISALT) * DISCH (ISALT) + SLTRAI * QRAIN
                 DISCH (ISALT) = DISCH(ISALT) + QRAIN
                 SALTD (ISALT) = SALTD(ISALT) / DISCH(ISALT)
               ENDIF
               IF (iDebug /= 0) THEN
                  WRITE(IDEBUG,*)  ' Precipitation greenhouse basins', IKKL
                  WRITE(IDEBUG,55) Id_Nod(INODE),ISALT, SALTD(ISALT), DISCH(ISALT), EXTRCT(ISALT)
               ENDIF
            ENDDO
! kasgebied: regenval op dakareaal silo; komt via daken
            ISALT = ISALT + 1
            DISCH(ISALT) = 0.
            SALTD(ISALT) = 0.
! reset salt index
            ISALT = INDSLT(INODE)
! onttrekkingen per bassin
            DO IKKL=1,NKKL
              ISALT = ISALT + 1
              EXTRCT(ISALT) = EXTRCT(ISALT) + GEBR(INR,IKKL)/RTMSZ
              IF (iDebug /= 0) THEN
                WRITE(IDEBUG,*)  ' Discharge from greenhouse-basins', IKKL
                WRITE(IDEBUG,55) Id_Nod(INODE),ISALT, SALTD(ISALT), DISCH(ISALT), EXTRCT(ISALT)
              ENDIF
              ! overflow bassins naar downstream boundary, bij geen downstream open water
              QKUIT  = QKAS(INR,IKKL)
              IF (IOW .LE. 0 .AND. QKUIT .GT. 0) THEN
                 EXTRCT(ISALT) = EXTRCT(ISALT) + QKUIT
                 IF (iDebug /= 0) THEN
                   WRITE(IDEBUG,*)  ' Overflow from greenhouse-basins', IKKL
                   WRITE(IDEBUG,55) Id_Nod(INODE),ISALT, SALTD(ISALT), DISCH(ISALT), EXTRCT(ISALT)
                 ENDIF
              ENDIF
            ENDDO
! onttrekkingen silo via pomp naar gw
            ISALT = ISALT + 1
            EXTRCT(ISALT) = EXTRCT(ISALT) + QSILGW(INR)
            if (idebug > 0) then
              WRITE(IDEBUG,*)  ' Discharge silo via pump'
              WRITE(IDEBUG,55) Id_Nod(INODE),ISALT, SALTD(ISALT), DISCH(ISALT), EXTRCT(ISALT)
            endif
 ! overflow silo naar boundary, bij gebrek aan open water
            IF (IOW .LE. 0 .AND. QSILOW(INR) .GT. 0) THEN
               EXTRCT(ISALT) = EXTRCT(ISALT) + QSILGW(INR)
               if (idebug > 0) then
                 WRITE(IDEBUG,*)  ' Discharge silo via pump'
                 WRITE(IDEBUG,55) Id_Nod(INODE),ISALT, SALTD(ISALT), DISCH(ISALT), EXTRCT(ISALT)
               endif
            ENDIF

         ELSEIF (KIND .EQ. 4) THEN
! Open water
! regenval op open water
            QRAIN = ROW(INR) / RTMSZ
            IF (QRAIN .GT. 0.0) THEN
              SALTD (ISALT) = SALTD(ISALT) * DISCH (ISALT) + SLTRAI * QRAIN
              DISCH (ISALT) = DISCH(ISALT) + QRAIN
              SALTD (ISALT) = SALTD(ISALT) / DISCH(ISALT)
            ENDIF
! kwel/wegzijging
            RKWEL = KWOW(INR) / RTMSZ
            IF (RKWEL .GT. 0.0) THEN
              SALTD (ISALT) = SALTD(ISALT) * DISCH (ISALT) + SLTKWL(INode) * RKWEL
              DISCH (ISALT) = DISCH(ISALT) + RKWEL
              SALTD (ISALT) = SALTD(ISALT) / DISCH(ISALT)
            ELSEIF (RKWEL .LT. 0.0) THEN
!22-12-1997: correctie teken: kwel is nl <0, dus onttrekking.
               EXTRCT(ISALT) = EXTRCT(ISALT) - RKWEL
            ENDIF
! end kwel/wegzijging
            IF (iDebug /= 0) THEN
                WRITE(IDEBUG,*)  ' open water: rain and seepage/percolation'
                WRITE(IDEBUG,55) Id_Nod(INODE),ISALT, SALTD(ISALT), DISCH(ISALT), EXTRCT(ISALT)
            ENDIF

         ELSEIF (KIND .EQ. 6) THEN
!! Boundary
!!
!! to be checked for overlap openwater/structure
!!
            teller  = EiNode (INODE,2)
! inlaat van boezem
! QBND negatief, dus ook + veranderen in -
!           IF (qBnd(teller) < 0.0) THEN
            IF (qinBnd(teller)%totalStructure < 0.0) THEN
              SALTD (ISALT) = SALTD(ISALT) * DISCH (ISALT) - SLTBND(teller) * QINBND(teller)%totalStructure
              DISCH (ISALT) = DISCH(ISALT) - QINBND(teller)%totalStructure
              SALTD (ISALT) = SALTD(ISALT) / DISCH(ISALT)
              IF (iDebug /= 0) THEN
                  WRITE(IDEBUG,*) ' QBND            = ',QBND(teller)
                  WRITE(IDEBUG,*) ' QINBND_structure= ',QINBND(teller)%totalStructure
                  WRITE(IDEBUG,*) ' SLTBND          = ',SLTBND(teller)
              ENDIF
            ENDIF
! uitlaat naar boezem
!           IF (QBND(teller) > 0.0) EXTRCT(ISALT) = EXTRCT (ISALT) + QBND(INR)
            IF (qinBnd(teller)%totalStructure > 0.0) EXTRCT(ISALT) = EXTRCT (ISALT) + QINBND(INR)%totalStructure
            IF (iDebug /= 0 .and. ISalt .le. NcSalt) THEN
                WRITE(IDEBUG,*)  ' Boundary inlet/outlet'
                WRITE(IDEBUG,55) Id_Nod(INODE),ISALT, SALTD(ISALT), DISCH(ISALT), EXTRCT(ISALT)
            ENDIF

          ELSEIF (KIND .EQ. 14) THEN
! RWZI
! IOW = downstream open water, if existing
! is used here to check that in case no downstream open water, flows go to boundary IBND
            IOW  = EIOW(INODE)
            IBND = EIBND(INODE)
! RWZI: outflow naar rand als onttrekking op zoutknoop RWZI behandelen
            IF (IOW .LE. 0 .AND. IBND .GT. 0) THEN
              QRUIT = QEffluent(INR)
              if ( Abs (QEffluent(INr)-Qrwzi(INr)) .gt. .1) RwziWarning = .true.
              IF  (QRUIT .GT. 0.0) EXTRCT(ISALT) = EXTRCT(ISALT) + QRUIT
              IF (iDebug /= 0) THEN
                 WRITE(IDEBUG,*)  ' RWZI '
                 WRITE(IDEBUG,55) Id_Nod(INODE),ISALT, SALTD(ISALT), DISCH(ISALT), EXTRCT(ISALT)
              ENDIF
            ENDIF

          ELSEIF (KIND .EQ. 15) THEN
! Industry
! IOW = downstream open water, if existing
! Behandel industriele lozing/onttrekking als externe lozing/onttrekking op de open waters
! Industriele lozing op benedenstrooms open water; voor rand geen actie hier.
            IOW  = EIOW(INODE)
            IF (IOW .GT. 0) THEN
               QRUIT = QIndDis(INR)
               IF  (QRUIT .GT. 0.0) THEN
                 NodeDown = Donode(Inode)
                 ISALT = INDSLT(NodeDown)
                 SALTD (ISALT) = SALTD(ISALT) * DISCH (ISALT) + SltIndDis(Inr) * QRUIT
                 DISCH (ISALT) = DISCH(ISALT) + QRUIT
                 SALTD (ISALT) = SALTD(ISALT) / DISCH(ISALT)
               ENDIF
               IF (iDebug /= 0) THEN
                 WRITE(IDEBUG,*)  ' Industry discharge'
                 WRITE(IDEBUG,55) Id_Nod(INODE),ISALT, SALTD(ISALT), DISCH(ISALT), EXTRCT(ISALT)
               ENDIF
            ENDIF
 !Industriele onttrekking op bovenstrooms open water
            NodeUp = Upnode(Inode)
            IF (Nodeup .GT. 0) THEN
               QRUIT = QIndDem(INR)
               IF  (QRUIT .GT. 0.0) THEN
                  ISALT = INDSLT (NodeUp)
                  EXTRCT (ISALT) = EXTRCT(ISALT) + QRUIT
               ENDIF
               IF (iDebug /= 0) THEN
                 WRITE(IDEBUG,*)  ' Industry extraction'
                 WRITE(IDEBUG,55) Id_Nod(INODE),ISALT, SALTD(ISALT), DISCH(ISALT), EXTRCT(ISALT)
               ENDIF
            ENDIF

         ENDIF
   10    CONTINUE
      ENDDO
!
! ********************************************************************
! *** DEBUG
! ********************************************************************
!
      IF (iDebug /= 0) THEN
        WRITE(iDEBUG,*) ' At end of DtSltD'
        NAME = ' '
        DO ISALT=1,NCSALT
           WRITE(IDEBUG,55) NAME, ISALT, SALTD(ISALT), DISCH(ISALT), EXTRCT(ISALT)
        ENDDO
      ENDIF
   55 FORMAT(1X,A20,1X,I6,3X,F8.2,1X,F9.3,1X,F9.3)
!
      RETURN
      END subroutine DtSltD




      SUBROUTINE STCOEF (KNK, VO, WI, WO, AK, COEFF, IFOUT, IDEBUG, ITMSTP)
!
! ********************************************************************
! ** DELFT_3B version 1.21
! ** SUBROUTINE TER BEPALING VAN COEFFICIENTEN TBV ZOUTBEWEGING
! ** OVERGENOMEN UIT DM - Distributiemodel
! ********************************************************************
! ** VARIABELEN:
! **
! **     KNK = KNOOPNUMMER
! **     VO  = VOLUME VAN DE KNOOP
! **     WI  = INKOMEND DEBIET (MM3/DECADE)
! **     W0  = UITGAAND DEBIET (MM3/DECADE)
! **     AK  = IN HUIDIGE DM ALTIJD NUL
! **   COEFF = BEVAT DE 4 BEREKENDE COEFFICIENTEN
! ********************************************************************
! ** VRAGEN:
! **   - IS AK ALTIJD NUL? ZO JA, SCHEELT IN STRUCTUUR PROGRAMMA
! **        AK = 0 altijd voor conservatieve stoffen zoals zout!
! **   - WAAROM DIMENSION (51)
! ********************************************************************
!
      Double Precision    VO, WI, WO, AK, dw, v1, x, y, del, xl, yl, u, em
      Double Precision    coeff, aVal, dar, dv, ddr, vt, ekt, dr, ar, t, rHlp
      Double Precision    rHlp2, c1, c2, c3, c4, f, gCum, g, r
      INTEGER KNK, iFout, iTmStp, iDebug, nPts, k, iSgnm, nPts1
      DIMENSION F(51), G(51), GCUM(51), COEFF(4)
!
! ********************************************************************
!   COMPUTE COEFFICIENTS FOR A DECAYING POLLUTANT IN STORAGE
!   BASED ON ROUTINE STCOEF OF EXISTIND OLD DM,  NO CHANGES MADE
! ********************************************************************
!
      DW = WI - WO
      V1 = VO + DW
      X  = WI/VO + AK
      Y  = WO/VO + AK
      DEL = X - Y
      IF (DEL .LT. -.99)  THEN
        WRITE(IFOUT,110) ITMSTP, KNK, VO, WI, WO, DEL
  110   FORMAT(//,' MSG of saltcomputations:',/,   &
                 ' Warning: a watervolume decreased with 99% or more',    &
                 ' This may lead to strange results',/,   &
                 ' Timestep:', I8,/,   &
                 ' ISALT Vol Win Wout Del = ',I5,1X,4E18.5)
        DEL = -.999
      ENDIF

      IF (X .LT. 0.01 .OR. Y .LT. .01) GOTO 50
      IF (AK .EQ. 0.0) GOTO 10
      IF (WO/VO .LE. 5.0) GOTO 50
      IF (ABS(DEL) .GT. 0.2) GOTO 50

   10 IF (iDebug /= 0)  WRITE(IDEBUG,*) ' STCOEF LBL 10'
      IF (DEL .LT. 1.0 .AND. Y .LT. 20.0/(2.0+DEL))  GOTO 20
      IF (DEL .GE. 1.0 .AND. Y*DLOG(1.0+DEL) .LT. 10.0*DEL) GOTO 20
      XL = 0.0
      YL = 0.0
      GOTO 40

   20 IF (iDebug /= 0)  WRITE(IDEBUG,*) ' STCOEF LBL 20'
      IF (ABS(DEL) .GT. .01) GOTO 30
      XL = EXP (-X * (1.0 -0.5*DEL))
      YL = EXP (-Y * (1.0 -0.5*DEL))
      GOTO 40

   30 IF (iDebug /= 0)  WRITE(IDEBUG,*) ' STCOEF LBL 30'
      XL = ( 1 + DEL) ** (-X/DEL)
      YL = ( 1 + DEL) ** (-Y/DEL)

   40 IF (iDebug /= 0)  WRITE(IDEBUG,*) ' STCOEF LBL 40'
      U = Y - 1.0 + YL
      COEFF(1) = (Y - Y * XL) / U
      COEFF(2) = 1.0 - COEFF(1)
      COEFF(3) = VO * X * Y / U
      COEFF(4) = VO * X * (1.0 - YL) / U
      GOTO 100

   50 IF (iDebug /= 0)  WRITE(IDEBUG,*) ' STCOEF LBL 50'
      AVAL = 0.1
      IF (AK .GT. 2.5) AVAL = 1.0 / (4.0 * AK)
      NPTS = Int ( AMIN1 (1.5E0 + 1.0E0/Sngl(AVAL), 51.0E0) )
      AVAL = 1.0 / (NPTS -1)
      DAR = AVAL * WI /VO
      DV = DW * AVAL
      IF (iDebug /= 0) WRITE(IDEBUG,*) ' Before Exp', KNK, AK, AVAL
      EM = EXP (-AK * AVAL)
      IF (iDebug /= 0) WRITE(IDEBUG,*) ' After Exp', KNK, EM, DW, VO
      DDR = AVAL * DW / VO
      VT = VO
      EKT = 1.0
      DR = 0.0
      AR = 0.0
      T  = 0.0

!     COMPUTE F AND G

      DO 60  K=1,NPTS
         IF (iDebug /= 0) WRITE(IDEBUG,*) ' Lbl 60', KNK, K, DR, AR
         IF (ABS(DR) .LE. .01) R = EXP(-(1.0 - 0.5*DR)*AR)
         IF (iDebug /= 0) WRITE(IDEBUG,*) ' Lbl 60_2', VO, VT,WI,DW
         IF (ABS(DW) .GT. 0.0000001) THEN
             RHLP  = WI/DW
             RHLP2 = 1000000.
             IF (VT .NE. 0)  RHLP2 = VO/VT
             ISGNM = SIGN (1.0d0, RHLP2)
             RHLP2 = ABS (RHLP2)
             IF (ABS(DR) .GT. .01) R = ISGNM * (RHLP2 ** RHLP)
         ELSEIF (ABS(WI) .LE. .0000001) THEN
             IF (ABS(DR) .GT. .01) THEN
               R = 1000000.
               IF (VT .NE. 0)  R =  VO/VT
             ENDIF
         ENDIF
!ENDC0
         F(K) = R * EKT
         IF (iDebug /= 0) WRITE(IDEBUG,*) ' Lbl 60_3', F(K)
         IF (VT*F(K) .NE. 0) THEN
             G(K) = VO / (VT*F(K))
         ELSEIF (EKT .NE. 0) THEN
             G(K) = VO / EKT
         ELSE
             G(K) = 0
         ENDIF
!ENDC0
         EKT = EKT * EM
         VT = VT + DV
         DR = DR + DDR
         AR = AR + DAR
         T = T + AVAL
   60 CONTINUE

!     COMPUTE GCUM

      GCUM (1) = 0.0
      DO 70  K=2,NPTS
         GCUM(K) = GCUM(K-1) + 0.5 * (G(K-1) + G(K)) * AVAL
   70 CONTINUE

!     COMPUTE 4 BASIC COEFFIECIENTS

      C1 = 0.5 * (F(1) + F(NPTS))
      C2 = 0.5 * (F(1) * GCUM(1) + F(NPTS) * GCUM(NPTS))
      C3 = F(NPTS)
      C4 = F(NPTS) * GCUM(NPTS)
      NPTS1 = NPTS - 1

      DO 80  K=2,NPTS1
         C1 = C1 + F(K)
         C2 = C2 + F(K) * GCUM(K)
   80 CONTINUE

      C1 = C1 * AVAL
      C2 = C2 * AVAL
      COEFF(1) = C4 / C2
      COEFF(2) = C3 - C1*COEFF(1)
      COEFF(3) = VO / C2
      COEFF(4) = C1 * VO / C2

  100 CONTINUE

      RETURN
    END subroutine StCoef


end module Salts
