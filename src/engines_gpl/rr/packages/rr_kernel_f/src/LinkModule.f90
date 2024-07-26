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
! at:               $Modtime:: 29-08-97 11:55a  $
!
! current revision: $Revision:: 5               $


module Link

!  use Messages
  use Network
  use Conf_fil
  use Conf_Arr
  use Paved
  use Boundary
  use Structures
  use Unpaved
  use OpenWater
!  use Salts
  use ReadLib

  use  m_1d_networkreader_rr

  implicit none

  ! variables
  ! *** Link data
  ! *** EILINK ( ) = extern nummer
  ! *** LNKFRM     = van knoop
  ! *** LKNTO      = naar knoop
! Taiwan April 2004
! Groundwater links, January 2012
  ! *** LinkType   = link type, 0=3BLink, 1=Sewerage link, 21/22=UnpavedSWLink, 30=RR-Routing link  31 = GWLink
  ! *** QLink      = flow in link
  ! *** QinLink    = inflow in link
  ! *** QoutLink   = Outflow in Link
  ! *** NAMLNK     = naam vd link: voorlopig geschrapt! want niet gebruikt
  Logical  RoutingLinkExists
  Logical  GWLinkExists
  INTEGER  NrGwLinks, NrRoutingLinks
  INTEGER  MuskingumMaxLayer
  INTEGER, Pointer, SAVE ::       EILINK(:), LNKFRM(:), LNKTO(:), LinkType(:)
  INTEGER, Pointer, SAVE ::       MuskingumNLayers(:)
  Real   , Pointer, SAVE ::       Qlink(:,:), QinLINK(:), QoutLink(:)
  Real   , Pointer, Save ::       MuskingumX(:,:),MuskingumK(:,:),MuskingumQmax(:,:)
  Real   , Pointer, Save ::       MuskingumQout(:,:),MuskingumQin(:,:)
  Real   , Pointer, Save ::       MuskingumQoutOld(:,:),MuskingumQinOld(:,:)

  Real   , Pointer, SAVE ::       GWLink_kDValue(:), GWLink_Length(:), GWLink_Width(:)
! CHARACTER(Len=20), Pointer, SAVE ::  NAMLNK(:)

  Character(Len=CharIdLength), Pointer :: nameLink(:), LinkDescr(:), NameFromNode(:), NameToNode(:)

! ARS 12867
  Logical AddLinkPrefix
  Character(Len=CharIdLength) LinkPrefix


contains

  Subroutine Link_confAr1

   implicit none

   Logical Success

    NLNK = MAX (1, NCLINK )
    NrGwLinks = 0
    NrRoutingLinks = 0

    IF ((NLNK .GT. 0) .and. (ConfFil_get_iOut1() > 0)) then
        WRITE(ConfFil_get_iOut1(),*) ' Number of 3B-branches =',NLNK
    Endif

! juli 99: nlnk <= 2* NNod
! March 2012: switched off
!   If (NLNK .gt. 2*Nnod) then
!      call ErrMsgStandard (912, 0, ' Linkmodule ConfAr1 ',  'NLNK > 2*NNOD ')
!   Endif

    Success = Dh_AllocInit (Nlnk, Eilink, LnkFrm, LnkTo, 0)
    Success = Success .and. Dh_AllocInit (Nlnk, LinkType, 0)
!   Success = Success .and. Dh_AllocInit (Nlnk, Namlnk, '' )
    Success = Success .and. Dh_AllocInit (Nlnk, NameLink, NameFromNode, NameToNode, '')
    Success = Success .and. Dh_AllocInit (Nlnk, LinkDescr, '')
    If (.not. success)  call ErrMsgStandard (981,0, ' Error allocating arrays in subroutine ', ' Link_ConfAr1' )

  Return
  End subroutine Link_confAr1


subroutine Link_ReadAscii_ini(infile, infile4)
!infile: 3b_topology.ini (nodes and branches(links))
!infile4: 3B_Rout.3B file (routing link data and groundwater link data)
    character(1000), intent(in)  :: infile            ! input
    integer :: RetVal
    integer :: brscount
    type(t_network), target :: network2
    integer :: teller, tp, idebug, ifrom, ito, infile4
    integer :: iecode, iout1, ilink, ilink2,i,j, ilen, ilen2
    character(CharIdLength) ::  id,nm, fn_name, tn_name
    type(t_node) :: fn, tn
    Logical    ::     allow, found, endfil, success, occurs

    Integer       IDUM(10)
    Real          RDUM(10)
    character(Len=CharIdLength) CDum(10), IdTemp
    character(Len=CharIdLength), pointer :: RoutingDefinition(:)
    character(Len=CharIdLength), pointer :: GWLinkDefinition(:)
    Logical                    , pointer :: RoutingDefinitionFound(:)
    Logical                    , pointer :: GWLinkDefinitionFound(:)

    Character(1000) string
    character(1000) :: tmpfile
    logical :: check

    check = .true.
    RoutingLinkExists = .false.
    GWLinkExists = .false.
    tmpfile = "tmp_nodes.ini"
    call NetworkReader(network2, tmpfile, check)
    brscount = network2%brs%count

    RoutingLinkExists = .false.
    GWLinkExists = .false.

    do teller = 1, brscount
        id = network2%brs%branch(teller)%id
        nm = network2%brs%branch(teller)%name
        tp = network2%brs%branch(teller)%brtype
        fn_name = network2%brs%branch(teller)%fromnode2
        tn_name = network2%brs%branch(teller)%tonode2

        NameLink(teller) = id
        LinkDescr(teller) = nm
        NameFromNode(teller) = fn_name
        NameToNode(teller) = tn_name

        select case (tp)
            case (30)
                RoutingLinkExists = .true.
                NrRoutingLinks = NrRoutingLinks + 1
                LinkType(teller) = tp
            case (31)
                GWLinkExists = .true.
                NrGWLinks = NrGWLinks + 1
                LinkType(teller) = tp
            case default
                LinkType(teller) = tp

        end select

        ! Fill working arrays
        CALL FNDNOD (IFROM, nameFromNode(teller))
        CALL FNDNOD (ITO  , nameToNode(teller))
        IF (IFROM .EQ. -1 .OR. ITO .EQ. -1)   call ErrMsgStandard (951, 0, ' Link_ReadAscii', ' linkfile')
           EILINK (teller) = teller
           LNKFRM (teller) = IFROM
           LNKTO  (teller) = ITO
    end do
    goto 311
150 call SetMessage(LEVEL_FATAL, 'read error in Link_ReadAscii')
311 CONTINUE


!   write(*,*) ' AddLinkPrefix =', AddLinkPrefix
!   write(*,*) ' LinkPrefix    =', LinkPrefix(1:Len_Trim(LinkPrefix))

!  always allocate Qinlink and Qoutlink
    Success = Dh_AllocInit (Nlnk, 5, QLink, 0E0)
    Success = Dh_AllocInit (Nlnk, QinLink, QoutLink, 0E0)
    If (RoutingLinkExists) then
        MuskingumMaxLayer = 4
        Success = success .and. Dh_AllocInit (Nlnk, MuskingumNLayers, 0)
        Success = success .and. Dh_AllocInit (Nlnk, MuskingumMaxLayer, MuskingumX, 0E0)
        Success = success .and. Dh_AllocInit (Nlnk, MuskingumMaxLayer, MuskingumK, 0E0)
        Success = success .and. Dh_AllocInit (Nlnk, MuskingumMaxLayer, MuskingumQMax, 0E0)
        Success = success .and. Dh_AllocInit (Nlnk, MuskingumMaxLayer, MuskingumQoutOld, 0E0)
        Success = success .and. Dh_AllocInit (Nlnk, MuskingumMaxLayer, MuskingumQout, 0E0)
        Success = success .and. Dh_AllocInit (Nlnk, MuskingumMaxLayer, MuskingumQinOld, 0E0)
        Success = success .and. Dh_AllocInit (Nlnk, MuskingumMaxLayer, MuskingumQin, 0E0)
        Success = success .and. Dh_AllocInit (Nlnk, RoutingDefinition, '')
        Success = success .and. Dh_AllocInit (Nlnk, RoutingDefinitionFound, .false.)
        If (.not. success)  call ErrMsgStandard (981,0, ' Error allocating arrays in subroutine ', ' Link_ReadAscii' )

!3B_rout.3b file
! Read routing link input file; first find the link definitions
        endfil = .false.
        teller = 0
        RetVal = 0
        do while (.not. endfil)
           CALL SKPCOM (INfile4, ENDFIL,'ODS')
           IF (ENDFIL) GOTO 411
           READ(Infile4,'(A1000)',END=411,ERR=150,IOSTAT=IECODE) STRING
           if (STRING(1:4) .eq. 'ROUT')  then
              teller = teller + 1
              RetVal = RetVal + GetVAR2 (STRING,' id ',1,' Link_ReadAscii',' 3B_Rout.3B file',IOUT1, &
                           ID, RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
! remove prefix?
! ARS 12867
              If (AddLinkPrefix) Then
                 ilen = Len_Trim(LinkPrefix)
                 IdTemp = id(1:)
                 Call UpperC(IdTemp)
                 if (ilen .gt. 0) then
                   if (IdTemp (1:ilen) .ne. LinkPrefix(1:ilen)) then
                      call SetMessage(LEVEL_WARN, ' Link '//id(1:Len_Trim(Id))//' does not contain the specified link prefix string; data is skipped')
                      goto 399
                   else
                      ilen2 = Len_Trim(id)
                      id = id(ilen+1:)
                      id(ilen2:) = ' '
                   endif
                 endif
              Endif
! find link nr
              ILink = FindString (NLnk, NameLink, Id, NLnk, CaseSensitive)
              If (ILink .gt. 0) then
                If (LinkType(ilink) .ne. 30) then
                  call SetMessage(LEVEL_WARN, 'Link '//id(1:Len_Trim(Id))//' routing definition ignored since link is not a routing link')
                Elseif (RoutingDefinition(iLink) .ne. '')  then
                  call SetMessage(LEVEL_ERROR, 'Link '//id(1:Len_Trim(Id))//' double routing definition in datafile 3B_rout.3B; last data skipped')
                Else
! routing definition id
                  RetVal = RetVal + GetVAR2 (STRING,' di ',1,' Link_ReadAscii',' 3B_Rout.3B file',IOUT1, &
                                ID, RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
                  RoutingDefinition(ilink) = id
                Endif
              Else
                  call SetMessage(LEVEL_WARN, 'Link '//LinkPrefix(1:Len_Trim(LinkPrefix))//id(1:Len_Trim(Id))//' with routing definition is an unknown link; data is skipped')
              Endif
           endif
399        CONTINUE
        enddo

411     CONTINUE

        If (RetVal .gt. 0) call ErrMsgStandard (972, 0, ' Error reading from 3BRout.3B ', ' Error getting ROUT records')
        If (teller .lt. NrRoutingLinks) then
            call ErrMsgStandard (972, 0, ' Error reading from 3BRout.3B ', ' Not all routing links have a routing link definition')
        Endif

        if (idebug .ne. 0) then
           write(idebug,*) ' ilink,   Namelink   LinkType  Routingdefinition'
           do i=1,nlnk
              write(idebug,*) i, Namelink(i)(1:10),LinkType(i),RoutingDefinition(i)(1:32)
           enddo
        endif

! Read routing link input file; next find the routing definitions
        Rewind(infile4)
        endfil = .false.
        teller = 0
        RetVal = 0
        do while (.not. endfil)
           CALL SKPCOM (INfile4, ENDFIL,'ODS')
           IF (ENDFIL) GOTO 511
           READ(Infile4,'(A1000)',END=511,ERR=150,IOSTAT=IECODE)  STRING
           if (STRING(1:4) .eq. 'RDEF')  then
              teller = teller + 1
              RetVal = RetVal + GetVAR2 (STRING,' id ',1,' Link_ReadAscii',' 3B_Rout.3B file',IOUT1, &
                           ID, RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
              ! test if routing definition is used
              ILink = FindString (NLnk, RoutingDefinition, Id, NLnk, CaseSensitive)
              occurs = (ilink .gt. 0)
              If (occurs) then
                ! get routing definition data
                 RetVal = RetVal + GetVAR2 (STRING,' nl ',3,' Link_ReadAscii',' 3B_Rout.3B file',IOUT1, &
                           ID, RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
                 MuskingumNLayers(ilink) = idum(1)
                 RetVal = RetVal + GetVRS2 (STRING,' x ',2,' Link_ReadAscii',' 3B_Rout.3B file',IOUT1, &
                           Cdum(1), RDUM(1), IDUM(1), MuskingumNLayers(ilink), IflRtn)
                 Do i=1,MuskingumNLayers(ilink)
                    MuskingumX(ilink,i) = rdum(i)
                 Enddo
                 RetVal = RetVal + GetVRS2 (STRING,' k ',2,' Link_ReadAscii',' 3B_Rout.3B file',IOUT1, &
                           Cdum(1), RDUM(1), IDUM(1), MuskingumNLayers(ilink),  IflRtn)
                 Do i=1,MuskingumNLayers(ilink)
                    MuskingumK(ilink,i) = rdum(i)
                 Enddo
                 RetVal = RetVal + GetVRS2 (STRING,' qm ',2,' Link_ReadAscii',' 3B_Rout.3B file',IOUT1, &
                           Cdum(1), RDUM(1), IDUM(1), MuskingumNLayers(ilink)-1,  IflRtn)
                 Do i=1,MuskingumNLayers(ilink)-1
                    MuskingumQMax(ilink,i) = rdum(i)
                 Enddo
                 MuskingumQMax(ilink,MuskingumNLayers(ilink)) = 1E10

!                Optional initial value Qoutold; read it, and assign it to correct nodes and layers
                 Allow = .true.
                 RetVal = RetVal + GetVAR2 (STRING,' qo ',2,' Link_ReadAscii',' 3B_Rout.3B file',IOUT1, &
                           ID, RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
                 if (found) then
                    Do i=1,MuskingumNLayers(ilink)-1
                       MuskingumQOutOld(ilink,i) = min (Rdum(1), MuskingumQmax(ilink,i))
                       Rdum(1) = max (0.0, Rdum(1)- MuskingumQOutOld(ilink,i))
                    Enddo
                    MuskingumQOutOld(ilink,MuskingumNLayers(ilink)) = Rdum(1)
                 Endif
!                Optional initial value Qinold; read it, and assign it to correct nodes and layers
                 RetVal = RetVal + GetVAR2 (STRING,' qi ',2,' Link_ReadAscii',' 3B_Rout.3B file',IOUT1, &
                           ID, RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
                 if (found) then
                    Do i=1,MuskingumNLayers(ilink)-1
                       MuskingumQInOld(ilink,i) = min (Rdum(1), MuskingumQmax(ilink,i))
                       Rdum(1) = max (0.0, Rdum(1)- MuskingumQInOld(ilink,i))
                    Enddo
                    MuskingumQInOld(ilink,MuskingumNLayers(ilink)) = Rdum(1)
                 Endif
                 Allow = .false.

                 RoutingDefinitionFound(ilink) = .true.
                 ! Assign definition to individual links
                 Do ilink2 = 1, nLnk
                    if (StringComp(RoutingDefinition(Ilink2),RoutingDefinition(ilink), CaseSensitive) )  then
                      RoutingDefinitionFound(ilink2) = .true.
                      MuskingumNLayers(ilink2) = MuskingumNLayers(ilink)
                      Do i=1,MuskingumNLayers(ilink)
                         MuskingumX   (ilink2,i) = MuskingumX(ilink,i)
                         MuskingumK   (ilink2,i) = MuskingumK(ilink,i)
                         MuskingumQMax(ilink2,i) = MuskingumQMax(ilink,i)
                         MuskingumQInOld(ilink2,i) = MuskingumQInOld(ilink,i)
                         MuskingumQOutOld(ilink2,i) = MuskingumQOutOld(ilink,i)
                      Enddo
                    endif
                 Enddo
              Endif
            endif
         enddo
 Endif


511 CONTINUE

    If (RoutingLinkExists) then
       if (idebug .ne. 0) then
          write(idebug,*) ' ilink,   Namelink   LinkType  Routingdefinition  found'
          do i=1,nlnk
             write(idebug,*) i, Namelink(i)(1:10),LinkType(i),RoutingDefinition(i)(1:32), RoutingDefinitionFound(i)
          enddo
       endif
       do i=1,nlnk
          if (RoutingDefinition(i) .ne. '' .and. .not. RoutingDefinitionFound(i)) Retval= 972
       enddo
       If (RetVal .gt. 0) call ErrMsgStandard (972, 0, ' Error reading from 3BRout.3B ', ' Not All Routing link definitions RDEF found')

       if (idebug .ne. 0) then
          write(idebug,*) ' ilink,   Nlayers,  X(1..Nlayers)  ,K(1..nLayers)  ,Qmax (1..nLayers) '
          do i=1,nlnk
             write(idebug,'(I4,I4,999(1X,F6.1))') i, MuskingumNlayers(i),&
                           (MuskingumX(i,j),j=1,MuskingumNlayers(i)), &
                            (MuskingumK(i,j),j=1,MuskingumNlayers(i)), &
                             (MuskingumQmax(i,j),j=1,MuskingumNlayers(i))
         enddo
       endif
    endif

    If (GWLinkExists) then
        Success = success .and. Dh_AllocInit (Nlnk, GWLink_KdValue, 1E0)
        Success = success .and. Dh_AllocInit (Nlnk, GWLink_Length, 1E0)
        Success = success .and. Dh_AllocInit (Nlnk, GWLink_Width, 1E0)
        Success = success .and. Dh_AllocInit (Nlnk, GWLinkDefinition, '')
        Success = success .and. Dh_AllocInit (Nlnk, GWLinkDefinitionFound, .false.)
        Success = success .and. Dh_AllocInit (Nlnk, SeqGWLink, 0)
        If (.not. success)  call ErrMsgStandard (981,0, ' Error allocating arrays in subroutine ', ' Link_ReadAscii' )

!3B_rout.3b file
! Read routing link input file; find the groundwater link definitions
        Rewind(infile4)
        endfil = .false.
        teller = 0
        RetVal = 0
        do while (.not. endfil)
           CALL SKPCOM (INfile4, ENDFIL,'ODS')
           IF (ENDFIL) GOTO 611
           READ(Infile4,'(A1000)',END=611,ERR=150,IOSTAT=IECODE) STRING
           if (STRING(1:4) .eq. 'GWLK')  then
              teller = teller + 1
              RetVal = RetVal + GetVAR2 (STRING,' id ',1,' Link_ReadAscii',' 3B_Rout.3B file',IOUT1, &
                           ID, RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
              If (AddLinkPrefix) Then
                 ilen = Len_Trim(LinkPrefix)
                 IdTemp = id(1:)
                 Call UpperC(IdTemp)
                 if (ilen .gt. 0) then
                   if (IdTemp (1:ilen) .ne. LinkPrefix(1:ilen)) then
                      write(iout1,'(A,A,A)') ' Link ',id(1:Len_Trim(Id)),&
                                           ' does not contain the specified link prefix string; data is skipped'
                      goto 599
                   else
                      ilen2 = Len_Trim(id)
                      id = id(ilen+1:)
                      id(ilen2:) = ' '
                   endif
                 endif
              Endif
! find link nr
              ILink = FindString (NLnk, NameLink, Id, NLnk, CaseSensitive)
              If (ILink .gt. 0) then
                If (LinkType(ilink) .ne. 31) then
                  write(iout1,'(A,A,A)') ' Link ',id(1:Len_Trim(Id)),&
                                         ' groundwater link definition ignored since link is not a groundwater link '
                Elseif (GWLinkDefinition(iLink) .ne. '')  then
                  Write(iout1,'(A,A,A)') ' Link ',id(1:Len_Trim(Id)),&
                                         ' double groundwater link finition in datafile 3B_rout.3B; last data skipped'
                Else
! routing definition id
                  RetVal = RetVal + GetVAR2 (STRING,' id ',1,' Link_ReadAscii',' 3B_Rout.3B file',IOUT1, &
                                ID, RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
                  GWLinkDefinition(ilink) = id
! read kd
                  RetVal = RetVal + GetVAR2 (STRING,' kd ',2,' Link_ReadAscii',' 3B_Rout.3B file',IOUT1, &
                                             ID, RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
                  GWLink_kDValue(ilink) = Rdum(1)
! read length
                  RetVal = RetVal + GetVAR2 (STRING,' le ',2,' Link_ReadAscii',' 3B_Rout.3B file',IOUT1, &
                                             ID, RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
                  GWLink_Length(ilink) = Rdum(1)
! read width
                  RetVal = RetVal + GetVAR2 (STRING,' wi ',2,' Link_ReadAscii',' 3B_Rout.3B file',IOUT1, &
                                             ID, RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
                  GWLink_Width(ilink) = Rdum(1)
                Endif
              Else
                  write(iout1,'(A,A,A,A)') ' Link ',LinkPrefix(1:Len_Trim(LinkPrefix)),id(1:Len_Trim(Id)),&
                                         ' with routing definition is an unknown link; data is skipped'
              Endif
           endif
599        CONTINUE
        enddo

611     CONTINUE

        If (RetVal .gt. 0) call ErrMsgStandard (972, 0, ' Error reading from 3BRout.3B ', ' Error getting GWLK records')
        If (teller .lt. NrGwLinks) then
            call ErrMsgStandard (972, 0, ' Error reading from 3BRout.3B ', ' Not all groundwaterlinks have a Groundwater Link definition')
        Endif
        if (idebug .ne. 0) then
           write(idebug,*) ' ilink,   Namelink   LinkType  GWLinkdefinition   kD(m2/day)     Length(m)     Width(m)'
           do i=1,nlnk
              write(idebug,*) i, Namelink(i)(1:10),LinkType(i),GWLinkDefinition(i)(1:32), GWLink_kDValue(i), GWLink_Length(i), GwLink_Width(i)
           enddo
        endif

    Endif

  return
end subroutine Link_ReadAscii_ini


  subroutine Link_ReadAscii (infile3, infile4)

! Reads Links from ASCII files
! infile3 = 3B_Link.Tp file (takken)
! infile4 = 3B_Rout.3B file (routing link data and groundwater link data)

    implicit none

    integer :: RetVal

    Integer(4)     infile3, infile4
    Integer        teller, iecode, iout1, idebug, ifrom, ito, ilink, ilink2,i,j, ilen, ilen2
    Character(CharIdLength)  id
    Character(1000) string
    Logical         allow, found, endfil, success, occurs

    Integer       IDUM(10)
    Real          RDUM(10)
    character(Len=CharIdLength) CDum(10), IdTemp
    character(Len=CharIdLength), pointer :: RoutingDefinition(:)
    character(Len=CharIdLength), pointer :: GWLinkDefinition(:)
    Logical                    , pointer :: RoutingDefinitionFound(:)
    Logical                    , pointer :: GWLinkDefinitionFound(:)

    allow = .false.
    found = .false.
    iOut1 = ConfFil_get_iOut1()
    idebug = ConfFil_get_idebug()
    RoutingLinkExists = .false.
    GWLinkExists = .false.

!3B_link.tp file
    endfil = .false.
    teller = 0
    RetVal = 0
    do while (.not. endfil)
         CALL SKPCOM (INfile3, ENDFIL,'ODS')
         IF (ENDFIL) GOTO 311
         READ(Infile3,'(A1000)',END=311,ERR=150,IOSTAT=IECODE)  STRING
         if (STRING(1:4) .eq. 'BRCH')  then
           teller = teller + 1
! Link id
           RetVal = RetVal + GetVAR2 (STRING,' id ',1,' Link_ReadAscii',' 3B_Link.TP file',IOUT1, &
                         ID, RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
           NameLink(teller) = id
! Link name
           allow = .true.
           RetVal = RetVal + GetVAR2 (STRING,' nm ',1,' Link_ReadAscii',' 3B_Link.TP file',IOUT1, &
                         ID, RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
           if (Found) LinkDescr(teller) = id
           allow = .false.
! Link from
           RetVal = RetVal + GetVAR2 (STRING,' bn ',1,' Link_ReadAscii',' 3B_Link.TP file',IOUT1, &
                         ID, RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
           NameFromNode(teller) = id
! Link to
           RetVal = RetVal + GetVAR2 (STRING,' en ',1,' Link_ReadAscii',' 3B_Link.TP file',IOUT1, &
                         ID, RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
           NameToNode(teller) = id
! Link type
           allow = .true.
           RetVal = RetVal + GetVAR2 (STRING,' mt 1 ',1,' Link_ReadAscii',' 3B_Link.TP file',IOUT1, &
                         ID, RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
           if (found) then
              Read(id,*) idum(1)
              if (idum(1) .eq. 30) then
                  RoutingLinkExists = .true.
                  NrRoutingLinks = NrRoutingLinks + 1
              elseif (idum(1) .eq. 31) then
                  GWLinkExists = .true.
                  NrGWLinks = NrGWLinks + 1
              endif
           else
              idum(1) = 0
           Endif
           LinkType(teller) = idum(1)
           allow = .false.
! Fill working arrays
           CALL FNDNOD (IFROM, nameFromNode(teller))
           CALL FNDNOD (ITO  , nameToNode(teller))
           if (idebug .ne. 0) then
              write(idebug,*) ' Read from 3B_link.tp file record ',teller
              write(idebug,*) NameLink(teller)(1:40)
              write(idebug,*) NameFromNode(teller) (1:40)
              write(idebug,*) NameToNode(teller) (1:40)
              write(idebug,*) ' ifrom ito', ifrom, ito
           endif
           IF (IFROM .EQ. -1 .OR. ITO .EQ. -1)   call ErrMsgStandard (951, 0, ' Link_ReadAscii', ' linkfile')
           EILINK (teller) = teller
           LNKFRM (teller) = IFROM
           LNKTO  (teller) = ITO
         endif
    enddo



 150 CONTINUE
     call SetMessage(LEVEL_FATAL, 'ead error in Link_ReadAscii')
311 CONTINUE

    If (RetVal .gt. 0) call ErrMsgStandard (972, 0, ' Error reading from 3B_Link.Tp ', ' Error getting BRCH records')

!   write(*,*) ' AddLinkPrefix =', AddLinkPrefix
!   write(*,*) ' LinkPrefix    =', LinkPrefix(1:Len_Trim(LinkPrefix))

!  always allocate Qinlink and Qoutlink
    Success = Dh_AllocInit (Nlnk, 5, QLink, 0E0)
    Success = Dh_AllocInit (Nlnk, QinLink, QoutLink, 0E0)
    If (RoutingLinkExists) then
        MuskingumMaxLayer = 4
        Success = success .and. Dh_AllocInit (Nlnk, MuskingumNLayers, 0)
        Success = success .and. Dh_AllocInit (Nlnk, MuskingumMaxLayer, MuskingumX, 0E0)
        Success = success .and. Dh_AllocInit (Nlnk, MuskingumMaxLayer, MuskingumK, 0E0)
        Success = success .and. Dh_AllocInit (Nlnk, MuskingumMaxLayer, MuskingumQMax, 0E0)
        Success = success .and. Dh_AllocInit (Nlnk, MuskingumMaxLayer, MuskingumQoutOld, 0E0)
        Success = success .and. Dh_AllocInit (Nlnk, MuskingumMaxLayer, MuskingumQout, 0E0)
        Success = success .and. Dh_AllocInit (Nlnk, MuskingumMaxLayer, MuskingumQinOld, 0E0)
        Success = success .and. Dh_AllocInit (Nlnk, MuskingumMaxLayer, MuskingumQin, 0E0)
        Success = success .and. Dh_AllocInit (Nlnk, RoutingDefinition, '')
        Success = success .and. Dh_AllocInit (Nlnk, RoutingDefinitionFound, .false.)
        If (.not. success)  call ErrMsgStandard (981,0, ' Error allocating arrays in subroutine ', ' Link_ReadAscii' )

!3B_rout.3b file
! Read routing link input file; first find the link definitions
        endfil = .false.
        teller = 0
        RetVal = 0
        do while (.not. endfil)
           CALL SKPCOM (INfile4, ENDFIL,'ODS')
           IF (ENDFIL) GOTO 411
           READ(Infile4,'(A1000)',END=411,ERR=150,IOSTAT=IECODE) STRING
           if (STRING(1:4) .eq. 'ROUT')  then
              teller = teller + 1
              RetVal = RetVal + GetVAR2 (STRING,' id ',1,' Link_ReadAscii',' 3B_Rout.3B file',IOUT1, &
                           ID, RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
! remove prefix?
! ARS 12867
              If (AddLinkPrefix) Then
                 ilen = Len_Trim(LinkPrefix)
                 IdTemp = id(1:)
                 Call UpperC(IdTemp)
                 if (ilen .gt. 0) then
                   if (IdTemp (1:ilen) .ne. LinkPrefix(1:ilen)) then
                      call SetMessage(LEVEL_WARN, ' Link '//id(1:Len_Trim(Id))//' does not contain the specified link prefix string; data is skipped')
                      goto 399
                   else
                      ilen2 = Len_Trim(id)
                      id = id(ilen+1:)
                      id(ilen2:) = ' '
                   endif
                 endif
              Endif
! find link nr
              ILink = FindString (NLnk, NameLink, Id, NLnk, CaseSensitive)
              If (ILink .gt. 0) then
                If (LinkType(ilink) .ne. 30) then
                  call SetMessage(LEVEL_WARN, 'Link '//id(1:Len_Trim(Id))//' routing definition ignored since link is not a routing link')
                Elseif (RoutingDefinition(iLink) .ne. '')  then
                  call SetMessage(LEVEL_ERROR, 'Link '//id(1:Len_Trim(Id))//' double routing definition in datafile 3B_rout.3B; last data skipped')
                Else
! routing definition id
                  RetVal = RetVal + GetVAR2 (STRING,' di ',1,' Link_ReadAscii',' 3B_Rout.3B file',IOUT1, &
                                ID, RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
                  RoutingDefinition(ilink) = id
                Endif
              Else
                  call SetMessage(LEVEL_WARN, 'Link '//LinkPrefix(1:Len_Trim(LinkPrefix))//id(1:Len_Trim(Id))//' with routing definition is an unknown link; data is skipped')
              Endif
           endif
399        CONTINUE
        enddo

411     CONTINUE

        If (RetVal .gt. 0) call ErrMsgStandard (972, 0, ' Error reading from 3BRout.3B ', ' Error getting ROUT records')
        If (teller .lt. NrRoutingLinks) then
            call ErrMsgStandard (972, 0, ' Error reading from 3BRout.3B ', ' Not all routing links have a routing link definition')
        Endif

        if (idebug .ne. 0) then
           write(idebug,*) ' ilink,   Namelink   LinkType  Routingdefinition'
           do i=1,nlnk
              write(idebug,*) i, Namelink(i)(1:10),LinkType(i),RoutingDefinition(i)(1:32)
           enddo
        endif

! Read routing link input file; next find the routing definitions
        Rewind(infile4)
        endfil = .false.
        teller = 0
        RetVal = 0
        do while (.not. endfil)
           CALL SKPCOM (INfile4, ENDFIL,'ODS')
           IF (ENDFIL) GOTO 511
           READ(Infile4,'(A1000)',END=511,ERR=150,IOSTAT=IECODE)  STRING
           if (STRING(1:4) .eq. 'RDEF')  then
              teller = teller + 1
              RetVal = RetVal + GetVAR2 (STRING,' id ',1,' Link_ReadAscii',' 3B_Rout.3B file',IOUT1, &
                           ID, RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
              ! test if routing definition is used
              ILink = FindString (NLnk, RoutingDefinition, Id, NLnk, CaseSensitive)
              occurs = (ilink .gt. 0)
              If (occurs) then
                ! get routing definition data
                 RetVal = RetVal + GetVAR2 (STRING,' nl ',3,' Link_ReadAscii',' 3B_Rout.3B file',IOUT1, &
                           ID, RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
                 MuskingumNLayers(ilink) = idum(1)
                 RetVal = RetVal + GetVRS2 (STRING,' x ',2,' Link_ReadAscii',' 3B_Rout.3B file',IOUT1, &
                           Cdum(1), RDUM(1), IDUM(1), MuskingumNLayers(ilink), IflRtn)
                 Do i=1,MuskingumNLayers(ilink)
                    MuskingumX(ilink,i) = rdum(i)
                 Enddo
                 RetVal = RetVal + GetVRS2 (STRING,' k ',2,' Link_ReadAscii',' 3B_Rout.3B file',IOUT1, &
                           Cdum(1), RDUM(1), IDUM(1), MuskingumNLayers(ilink),  IflRtn)
                 Do i=1,MuskingumNLayers(ilink)
                    MuskingumK(ilink,i) = rdum(i)
                 Enddo
                 RetVal = RetVal + GetVRS2 (STRING,' qm ',2,' Link_ReadAscii',' 3B_Rout.3B file',IOUT1, &
                           Cdum(1), RDUM(1), IDUM(1), MuskingumNLayers(ilink)-1,  IflRtn)
                 Do i=1,MuskingumNLayers(ilink)-1
                    MuskingumQMax(ilink,i) = rdum(i)
                 Enddo
                 MuskingumQMax(ilink,MuskingumNLayers(ilink)) = 1E10

!                Optional initial value Qoutold; read it, and assign it to correct nodes and layers
                 Allow = .true.
                 RetVal = RetVal + GetVAR2 (STRING,' qo ',2,' Link_ReadAscii',' 3B_Rout.3B file',IOUT1, &
                           ID, RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
                 if (found) then
                    Do i=1,MuskingumNLayers(ilink)-1
                       MuskingumQOutOld(ilink,i) = min (Rdum(1), MuskingumQmax(ilink,i))
                       Rdum(1) = max (0.0, Rdum(1)- MuskingumQOutOld(ilink,i))
                    Enddo
                    MuskingumQOutOld(ilink,MuskingumNLayers(ilink)) = Rdum(1)
                 Endif
!                Optional initial value Qinold; read it, and assign it to correct nodes and layers
                 RetVal = RetVal + GetVAR2 (STRING,' qi ',2,' Link_ReadAscii',' 3B_Rout.3B file',IOUT1, &
                           ID, RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
                 if (found) then
                    Do i=1,MuskingumNLayers(ilink)-1
                       MuskingumQInOld(ilink,i) = min (Rdum(1), MuskingumQmax(ilink,i))
                       Rdum(1) = max (0.0, Rdum(1)- MuskingumQInOld(ilink,i))
                    Enddo
                    MuskingumQInOld(ilink,MuskingumNLayers(ilink)) = Rdum(1)
                 Endif
                 Allow = .false.

                 RoutingDefinitionFound(ilink) = .true.
                 ! Assign definition to individual links
                 Do ilink2 = 1, nLnk
                    if (StringComp(RoutingDefinition(Ilink2),RoutingDefinition(ilink), CaseSensitive) )  then
                      RoutingDefinitionFound(ilink2) = .true.
                      MuskingumNLayers(ilink2) = MuskingumNLayers(ilink)
                      Do i=1,MuskingumNLayers(ilink)
                         MuskingumX   (ilink2,i) = MuskingumX(ilink,i)
                         MuskingumK   (ilink2,i) = MuskingumK(ilink,i)
                         MuskingumQMax(ilink2,i) = MuskingumQMax(ilink,i)
                         MuskingumQInOld(ilink2,i) = MuskingumQInOld(ilink,i)
                         MuskingumQOutOld(ilink2,i) = MuskingumQOutOld(ilink,i)
                      Enddo
                    endif
                 Enddo
              Endif
            endif
         enddo
 Endif


511 CONTINUE

    If (RoutingLinkExists) then
       if (idebug .ne. 0) then
          write(idebug,*) ' ilink,   Namelink   LinkType  Routingdefinition  found'
          do i=1,nlnk
             write(idebug,*) i, Namelink(i)(1:10),LinkType(i),RoutingDefinition(i)(1:32), RoutingDefinitionFound(i)
          enddo
       endif
       do i=1,nlnk
          if (RoutingDefinition(i) .ne. '' .and. .not. RoutingDefinitionFound(i)) Retval= 972
       enddo
       If (RetVal .gt. 0) call ErrMsgStandard (972, 0, ' Error reading from 3BRout.3B ', ' Not All Routing link definitions RDEF found')

       if (idebug .ne. 0) then
          write(idebug,*) ' ilink,   Nlayers,  X(1..Nlayers)  ,K(1..nLayers)  ,Qmax (1..nLayers) '
          do i=1,nlnk
             write(idebug,'(I4,I4,999(1X,F6.1))') i, MuskingumNlayers(i),&
                           (MuskingumX(i,j),j=1,MuskingumNlayers(i)), &
                            (MuskingumK(i,j),j=1,MuskingumNlayers(i)), &
                             (MuskingumQmax(i,j),j=1,MuskingumNlayers(i))
         enddo
       endif
    endif

    If (GWLinkExists) then
        Success = success .and. Dh_AllocInit (Nlnk, GWLink_KdValue, 1E0)
        Success = success .and. Dh_AllocInit (Nlnk, GWLink_Length, 1E0)
        Success = success .and. Dh_AllocInit (Nlnk, GWLink_Width, 1E0)
        Success = success .and. Dh_AllocInit (Nlnk, GWLinkDefinition, '')
        Success = success .and. Dh_AllocInit (Nlnk, GWLinkDefinitionFound, .false.)
        Success = success .and. Dh_AllocInit (Nlnk, SeqGWLink, 0)
        If (.not. success)  call ErrMsgStandard (981,0, ' Error allocating arrays in subroutine ', ' Link_ReadAscii' )

!3B_rout.3b file
! Read routing link input file; find the groundwater link definitions
        Rewind(infile4)
        endfil = .false.
        teller = 0
        RetVal = 0
        do while (.not. endfil)
           CALL SKPCOM (INfile4, ENDFIL,'ODS')
           IF (ENDFIL) GOTO 611
           READ(Infile4,'(A1000)',END=611,ERR=150,IOSTAT=IECODE) STRING
           if (STRING(1:4) .eq. 'GWLK')  then
              teller = teller + 1
              RetVal = RetVal + GetVAR2 (STRING,' id ',1,' Link_ReadAscii',' 3B_Rout.3B file',IOUT1, &
                           ID, RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
              If (AddLinkPrefix) Then
                 ilen = Len_Trim(LinkPrefix)
                 IdTemp = id(1:)
                 Call UpperC(IdTemp)
                 if (ilen .gt. 0) then
                   if (IdTemp (1:ilen) .ne. LinkPrefix(1:ilen)) then
                      write(iout1,'(A,A,A)') ' Link ',id(1:Len_Trim(Id)),&
                                           ' does not contain the specified link prefix string; data is skipped'
                      goto 599
                   else
                      ilen2 = Len_Trim(id)
                      id = id(ilen+1:)
                      id(ilen2:) = ' '
                   endif
                 endif
              Endif
! find link nr
              ILink = FindString (NLnk, NameLink, Id, NLnk, CaseSensitive)
              If (ILink .gt. 0) then
                If (LinkType(ilink) .ne. 31) then
                  write(iout1,'(A,A,A)') ' Link ',id(1:Len_Trim(Id)),&
                                         ' groundwater link definition ignored since link is not a groundwater link '
                Elseif (GWLinkDefinition(iLink) .ne. '')  then
                  Write(iout1,'(A,A,A)') ' Link ',id(1:Len_Trim(Id)),&
                                         ' double groundwater link finition in datafile 3B_rout.3B; last data skipped'
                Else
! routing definition id
                  RetVal = RetVal + GetVAR2 (STRING,' id ',1,' Link_ReadAscii',' 3B_Rout.3B file',IOUT1, &
                                ID, RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
                  GWLinkDefinition(ilink) = id
! read kd
                  RetVal = RetVal + GetVAR2 (STRING,' kd ',2,' Link_ReadAscii',' 3B_Rout.3B file',IOUT1, &
                                             ID, RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
                  GWLink_kDValue(ilink) = Rdum(1)
! read length
                  RetVal = RetVal + GetVAR2 (STRING,' le ',2,' Link_ReadAscii',' 3B_Rout.3B file',IOUT1, &
                                             ID, RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
                  GWLink_Length(ilink) = Rdum(1)
! read width
                  RetVal = RetVal + GetVAR2 (STRING,' wi ',2,' Link_ReadAscii',' 3B_Rout.3B file',IOUT1, &
                                             ID, RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
                  GWLink_Width(ilink) = Rdum(1)
                Endif
              Else
                  write(iout1,'(A,A,A,A)') ' Link ',LinkPrefix(1:Len_Trim(LinkPrefix)),id(1:Len_Trim(Id)),&
                                         ' with routing definition is an unknown link; data is skipped'
              Endif
           endif
599        CONTINUE
        enddo

611     CONTINUE

        If (RetVal .gt. 0) call ErrMsgStandard (972, 0, ' Error reading from 3BRout.3B ', ' Error getting GWLK records')
        If (teller .lt. NrGwLinks) then
            call ErrMsgStandard (972, 0, ' Error reading from 3BRout.3B ', ' Not all groundwaterlinks have a Groundwater Link definition')
        Endif
        if (idebug .ne. 0) then
           write(idebug,*) ' ilink,   Namelink   LinkType  GWLinkdefinition   kD(m2/day)     Length(m)     Width(m)'
           do i=1,nlnk
              write(idebug,*) i, Namelink(i)(1:10),LinkType(i),GWLinkDefinition(i)(1:32), GWLink_kDValue(i), GWLink_Length(i), GwLink_Width(i)
           enddo
        endif

    Endif

  return
  end subroutine Link_ReadAscii





  subroutine Link_determineLinkData
    ! *********************************************************************
    ! *** Determine downstream nodes for each node
    ! ***   NB Jan 1996: bij open waters nu meerdere kunstwerken benedenstrooms
    ! ***   NB Jan 1996: DONODE geeft dus de laatste volgens de tak-file.
    ! *** Determine upstream nodes for each structure
    ! *********************************************************************


   implicit none

   ! variables
    Integer iLink, iNode, iNode2, iVhg, iBnd, iRwzi, iPluv, ieCod  !, Len_Trim
    Integer nodeUp, ikind, iow, ikindup, ikinddw, iowSWLink, IBndSWLink
    Integer noDown
    Integer, Pointer :: ExistUp(:), ExistDown(:)
    Character(Len=CharIdLength) string
    Character*999 errorString
    Integer iDebug
    Logical Err917, Err974, Err920, Success

    success = DH_AllocInit (NcNode, ExistUp, ExistDown, 0)
    If (.not. Success) call ErrMsgStandard (981, 0, ' Error allocating arrays in subroutine ', &
                                           ' Link_DetermineLinkData')
!    allocate (ExistUp(ncnode), Stat=Allocation_Error )
!    allocate (ExistDown(ncnode), Stat=Allocation_Error )
!   DO INode=1,NCNode
!      ExistUp  (Inode) = 0
!      ExistDown(Inode) = 0
!   Enddo
!   Vector/Array initialisation
!    ExistUp   = 0
!    ExistDown = 0

    Err920 = .false.
    Err917 = .false.
    Err974 = .false.
    iDebug = ConfFil_get_iDebug()

    idebug = 32

! First loop only to set connection / bifurcation node type
    DO ILINK=1,NCLINK
      INODE = LNKFRM(ILINK)
      INODE2 = LNKTO(ILINK)
      IF (INODE .LE. 0 .OR. INODE2 .LE. 0) THEN
        IECOD = INODE*10000 + INODE2
                STRING = NameLink(ILINK)
                Err917 = .true.
        call ErrMsgStandard (917, IECOD, ' Linkmodule for link ',  STRING)
      ENDIF
      ikindup = EINode(INODE,3)
      ikinddw = EINode(INODE2,3)
      ! check nr upstream and downstream nodes, except for the groundwater links
      if (LinkType(ilink) .ne. 31)  then
         ExistUp  (Inode2) = Existup(Inode2) + 1
         ExistDown(Inode)  = ExistDown(Inode) + 1
         DownStreamLinkNr(Inode) = Ilink
         NrDownStreamLinks(Inode) = NrDownStreamLinks(inode) + 1
      endif
      If (ikindup .eq. 30 .and. NrDownStreamLinks(Inode) .gt. 1) then
         Einode(inode,3) = 32   ! change type from connection to bifurcation
      Endif
    ENDDO

!   reset everything
    ExistUp   = 0
    ExistDown = 0
    DownStreamLinkNr = 0
    NrDownStreamLinks = 0
!   again, now for real

    DO ILINK=1,NCLINK
      INODE = LNKFRM(ILINK)
      INODE2 = LNKTO(ILINK)
      IF (INODE .LE. 0 .OR. INODE2 .LE. 0) THEN
        IECOD = INODE*10000 + INODE2
                STRING = NameLink(ILINK)
                Err917 = .true.
        call ErrMsgStandard (917, IECOD, ' Linkmodule for link ',  STRING)
      ENDIF

! ikindup = upstream node type
! ikinddw = downstream node type
      ikindup = EINode(INODE,3)
      ikinddw = EINode(INODE2,3)
      ! check nr upstream and downstream nodes, except for the groundwater links
      if (LinkType(ilink) .ne. 31)  then
         ExistUp  (Inode2) = Existup(Inode2) + 1
         ExistDown(Inode)  = ExistDown(Inode) + 1
         DownStreamLinkNr(Inode) = Ilink
         NrDownStreamLinks(Inode) = NrDownStreamLinks(inode) + 1
      endif
      If (ikindup .eq. 30) NrUpstreamConnections(inode2) = NrUpstreamConnections(inode2) + 1
      If (ikindup .eq. 32) NrUpstreamConnections(inode2) = NrUpstreamConnections(inode2) + 1
      If (ikindup .eq. 30 .or. ikindup .eq. 16 .or. ikindup .eq. 5) then
         If (NrDownStreamLinks(Inode) .gt.1) then
            errorString = "Error: Only 1 downstream Link allowed at node " // Id_Nod(INode)
            Err974 = .true.
            call ErrMsgStandard (974, IECOD, errorString, ' Linkmodule ')
         Endif
      Endif
! RR routing link
      If (LinkType(Ilink) .eq. 30) then
         NrDownStreamRoutinglinks(Inode) =  NrDownStreamRoutingLinks(inode) + 1
         ! test adjusted to also allow NAM, Sobek-49707
         If (ikindup .ne. 16 .and. ikindup .ne. 18 .and. ikindup .ne. 19 .and. &
              ikindup .ne. 20 .and. ikindup .ne. 22 .and. ikindup .ne. 23 .and. ikindup .ne. 30 .and. ikindup .ne. 31) then
             errorString = "Error: Downstream routing link only allowed at Sacramento, ExtRunoff,HBV, SCS, LGSI, WagMod, Walrus, NAM or connection node node " &
                            // Id_Nod(INode)
             Err974 = .true.
             call ErrMsgStandard (974, IECOD, errorString, ' Linkmodule ')
         Endif
         If (ikinddw .ne. 32 .and. ikinddw .ne. 30 .and. ikinddw .ne. 6) then
             errorString = "Error: Routing link only allowed with downstream boundary, bifurcation or connection node node " &
                            // Id_Nod(INode)
             Err974 = .true.
             call ErrMsgStandard (974, IECOD, errorString, ' Linkmodule ')
         Endif
         If (ikindup .eq. 16 .and. ikinddw .ne. 30 .and. ikinddw .ne. 32) then
             errorString = "Error: Routing link with upstream Sacramento should have downstream bifurcation or connection node " &
                            // Id_Nod(INode)
             Err974 = .true.
             call ErrMsgStandard (974, IECOD, errorString, ' Linkmodule ')
         Endif
         If (ikindup .eq. 18 .and. ikinddw .ne. 30 .and. ikinddw .ne. 32) then
             errorString = "Error: Routing link with upstream External Runoff should have downstream bifurcation or connection node " &
                            // Id_Nod(INode)
             Err974 = .true.
             call ErrMsgStandard (974, IECOD, errorString, ' Linkmodule ')
         Endif
         If (ikindup .eq. 19 .and. ikinddw .ne. 30 .and. ikinddw .ne. 32) then
             errorString = "Error: Routing link with upstream HBV-runoff should have downstream bifurcation or connection node " &
                            // Id_Nod(INode)
             Err974 = .true.
             call ErrMsgStandard (974, IECOD, errorString, ' Linkmodule ')
         Endif
         If (ikindup .eq. 20 .and. ikinddw .ne. 30 .and. ikinddw .ne. 32) then
             errorString = "Error: Routing link with upstream SCS runoff should have downstream bifurcation or connection node " &
                            // Id_Nod(INode)
             Err974 = .true.
             call ErrMsgStandard (974, IECOD, errorString, ' Linkmodule ')
         Endif
         If (ikindup .eq. 22 .and. ikinddw .ne. 30 .and. ikinddw .ne. 32) then
             errorString = "Error: Routing link with upstream LGSI runoff should have downstream bifurcation or connection node " &
                            // Id_Nod(INode)
             Err974 = .true.
             call ErrMsgStandard (974, IECOD, errorString, ' Linkmodule ')
         Endif
         If (ikindup .eq. 23 .and. ikinddw .ne. 30 .and. ikinddw .ne. 32) then
             errorString = "Error: Routing link with upstream Wagmod or Walrus runoff should have downstream bifurcation or connection node " &
                            // Id_Nod(INode)
             Err974 = .true.
             call ErrMsgStandard (974, IECOD, errorString, ' Linkmodule ')
         Endif
         If (ikindup .eq. 31 .and. ikinddw .ne. 30 .and. ikinddw .ne. 32) then
             errorString = "Error: Routing link with upstream NAM runoff should have downstream bifurcation or connection node " &
                            // Id_Nod(INode)
             Err974 = .true.
             call ErrMsgStandard (974, IECOD, errorString, ' Linkmodule ')
         Endif
         If (NrDownStreamRoutinglinks(Inode) .gt.1) then
            errorString = "Error: Only 1 downstream Routing link allowed at node " // Id_Nod(INode)
            Err974 = .true.
            call ErrMsgStandard (974, IECOD, errorString, ' Linkmodule ')
         Endif
      Endif
! RR groundwater link
      If (LinkType(Ilink) .eq. 31) then
         !groundwater links between two unpaved nodes
         NrUpstreamGwLinks(inode2)  = NrUpstreamGWLinks(inode2) + 1
         NrDownstreamGwLinks(inode) = NrDownstreamGWLinks(inode) + 1
         If (ikindup .ne. 2) then
             errorString = "Error: Downstream RR-gwlink only allowed at RR-unpaved node " // Id_Nod(INode)
             Err974 = .true.
             call ErrMsgStandard (974, IECOD, errorString, ' Linkmodule ')
         Endif
         If (ikinddw .ne. 2) then
             errorString = "Error: RR-gwlink only allowed with downstream unpaved node " // Id_Nod(INode)
             Err974 = .true.
             call ErrMsgStandard (974, IECOD, errorString, ' Linkmodule ')
         Endif
      Endif
! ARS 15464:RR UnpavedSWLink; tested as type 21 (May 2006); in Sept 2006 implemented in Sobek 2.11 as type 22
      If (LinkType(Ilink) .eq. 21 .or. LinkType(Ilink) .eq. 22) then
         If (ikindup .ne. 2) then
             errorString = "Error: Downstream UnpavedSWLink link only allowed at RR-Unpaved node" &
                            // Id_Nod(INode)
             Err974 = .true.
             call ErrMsgStandard (974, IECOD, errorString, ' Linkmodule ')
         Endif
         If (ikinddw .ne. 4 .and. ikinddw .ne. 6) then
             errorString = "Error: UnpavedSWLink only allowed with downstream open water or boundary" &
                            // Id_Nod(INode)
             Err974 = .true.
             call ErrMsgStandard (974, IECOD, errorString, ' Linkmodule ')
         Endif
         SWLinkFromExists(Inode) = .true.
      Endif


! ARS 5322; two nodes of the same type may not be connected!.
! nav ARS ... RRRouting link and RRConnection node,
! 2 connection nodes may be linked with each other using RRlink or Routing link , also bifurcation node is allowed
! 2 unpaved nodes may be linked with each other using RRgroundwater link
      if (IKindUp .eq. IKindDw .and. IKindup .ne. 30 .and. Ikindup .ne. 2 .and. Ikindup .ne. 32) then
          errorString = "Error: Trying to connect 2 RR nodes of the same type " // &
                         Id_Nod(INode) // " and " // Id_Nod(INode2)
          Err974 = .true.
          call ErrMsgStandard (974, IECOD, errorString, ' Linkmodule ')
      endif

!     call ConfArr_set_DONODE(INODE, INODE2)
      if (DONODE(INODE) .LE. 0) then
         call ConfArr_set_DONODE(INODE, INODE2)
      elseif (ikindup .ne. 1 .and. ikindup .ne. 2 .and. ikindup .ne. 4 .and. ikindup .ne. 6 .and. ikindup .ne. 32) then
          errorString = "Error: Trying to connect more than 1 downstream node to node " // &
                         Id_Nod(INode2)
          Err920 = .true.
          call ErrMsgStandard (920, IECOD, ' Linkmodule ', errorString)
      elseif (ikinddw .ne. 15) then
          call ConfArr_set_DONODE(INODE, INODE2)
      endif

    ! bepaling EIOW, EIBND, VHGBND voor verhard, onverhard, kasgebied, RWZI, Industry, Sacramento, RRRunoff
    ! bepaling EIRWZI, VHGRWZI alleen voor verhard gebied
    ! bepaling EIRWZI ook voor industry naar RWZI
    ! extra tests added, to make sure only 1 downstream open water
    ! and only 1 downstream boundary
    ! ARS 15464: unpaved optional 2 downstream links, 1 RR-link  and 1 unpavedSWLink (Linktype 21/22)
      IF (ikindup .LE. 3) THEN
        NODOWN = DONODE(INODE)
        ikinddw = EINode(nodown,3)
        IF (ikinddw .eq. 4) THEN                                   ! verhard/onverhard/kas naar open water
    ! ARS 15464: unpaved optional 2 downstream links, 1 RR-link  and 1 unpavedSWLink (Linktype 21/22)
          If (ikindup .eq. 2 .and. (LinkType(Ilink) .eq. 21 .or. LinkType(Ilink) .eq. 22)) then
            if (EIOWSWLink(INODE) .gt. 0) THEN
               STRING = Id_Nod(INode)
               Err917 = .true.
               call ErrMsgStandard (917, IECOD, ' Linkmodule for node ', STRING(1:Len_Trim(string)))
            else
               call ConfArr_set_EIOWSWLink(INODE, EINode(DONODE(INODE),2))
            endif
          Else
            if (EIOW(INODE) .gt. 0) THEN
               STRING = Id_Nod(INode)
               Err917 = .true.
               call ErrMsgStandard (917, IECOD, ' Linkmodule for node ', STRING(1:Len_Trim(string)))
            else
               call ConfArr_set_EIOW(INODE, EINode(DONODE(INODE),2))
            endif
          Endif
        ELSEIF (ikindup  .EQ. 1 .AND. ikinddw .EQ. 6) THEN         ! verhard gebied naar rand
          if (EIBND(INODE) .gt. 0) THEN
             STRING = Id_Nod(INode)
             Err917 = .true.
             call ErrMsgStandard (917, IECOD, ' Linkmodule for node ', STRING(1:Len_Trim(string)))
          else
             IVHG = EINode(INODE, 2)
             IBND = EINode(NODOWN, 2)
             VHGBND(IVHG) = IBND
             ! initialise EIBND array
             call ConfArr_set_EIBND(INODE, EINode(DONODE(INODE),2))
          endif
        ELSEIF (ikindup .EQ. 1 .AND.  ikinddw .EQ. 14) THEN         ! verhard gebied naar rwzi
          if (EIRWZI(INODE) .gt. 0) THEN
             STRING = Id_Nod(INode)
             Err917 = .true.
             call ErrMsgStandard (917, IECOD, ' Linkmodule for node ', STRING(1:Len_Trim(string)))
          else
             IVHG = EINode(INODE, 2)
             IRWZI = EINode(NODOWN, 2)
             VHGRWZI(IVHG) = IRWZI
     ! initialise EIRWZI array
             call ConfArr_set_EIRWZI(INODE, EINode(DONODE(INODE),2))
          endif
        ELSEIF (ikindup .EQ. 2 .AND. ikinddw .EQ. 6) THEN            ! onverhard naar rand
    ! ARS 15464: unpaved optional 2 downstream links, 1 RR-link  and 1 unpavedSWLink (Linktype 21/22)
          If (LinkType(Ilink) .ne. 21 .and. LinkType(Ilink) .ne. 22) then
             if (EIBND(INODE) .gt. 0) THEN
                STRING = Id_Nod(INode)
                Err917 = .true.
                call ErrMsgStandard (917, IECOD, ' Linkmodule for node ', STRING(1:Len_Trim(string)))
             else
                call ConfArr_set_EIBND(INODE, EINode(DONODE(INODE),2))
             endif
          ElseIf (LinkType(Ilink) .eq. 21 .or. LinkType(Ilink) .eq. 22) then
             if (EIBNDSWLink(INODE) .gt. 0) THEN
                STRING = Id_Nod(INode)
                Err917 = .true.
                call ErrMsgStandard (917, IECOD, ' Linkmodule for node ', STRING(1:Len_Trim(string)))
             else
                call ConfArr_set_EIBNDSWLink(INODE, EINode(DONODE(INODE),2))
             endif
          Endif
        ELSEIF (ikindup .EQ. 3 .AND.  ikinddw .EQ. 6) THEN       ! kasgebied naar rand
          if (EIBND(INODE) .gt. 0) THEN
             STRING = Id_Nod(INode)
             Err917 = .true.
             call ErrMsgStandard (917, IECOD, ' Linkmodule for node ', STRING(1:Len_Trim(string)))
          else
             call ConfArr_set_EIBND(INODE, EINode(DONODE(INODE),2))
          endif
 ! end additions EIBND initialisation
! ARS 12563  Add option to connect paved, unpaved, greenhouse, cel to NWRW node
        ELSEIF (ikinddw .EQ. 7) THEN
!          write(*,*) ' Test paved/unpaved/greenhouse connection to NWRW node'
           call ConfArr_set_EIPluv(INODE,EINode(DONODE(INODE),2))
           RRCFConnect(nodown) = .true.
           If (RunSimultaneous) OnlineSobekLevelUsed = .true.
           UrbanRuralConnected = .true.
         !
        ELSEIF (LinkType(ilink) .ne. 31) then
           IECOD = EINode(INODE,1)*10000 + EINode(NODOWN,1)
           STRING = NameLink(ILINK)
           Err917 = .true.
           call ErrMsgStandard (917, IECOD, ' Linkmodule for link ',  STRING(1:Len_Trim(string)))
        ENDIF

    !upstream node for structures only
    !extra check: kunstwerk heeft slechts 1 bovenstrooms open water

      ELSEIF (ikinddw .EQ. 5)  THEN
        IF (UPNODE(INODE2) .LE. 0)  THEN
          call ConfArr_set_UPNODE(INODE2, INODE)
        ELSE
          errorString = "Error: Trying to connect more then 1 open water to structure " // &
                         Id_Nod(INode2)
          Err920 = .true.
          call ErrMsgStandard (920, IECOD, ' Linkmodule ', errorString)
        ENDIF

!downstream node for RWZI
      Elseif (ikindup .eq. 14) THEN
        NODOWN = DONODE(INODE)
        if (EINode(NODOWN,3) .eq. 6) then
            call ConfArr_set_EIBND(INODE, EINode(DONODE(INODE),2))
        elseif (EINode(NODOWN,3) .eq. 4) then
            call ConfArr_set_EIOW(INODE, EINode(DONODE(INODE),2))
        else
           STRING = Id_Nod(INode)
           Err917 = .true.
           call ErrMsgStandard (917, IECOD, ' Linkmodule for node ', STRING(1:Len_Trim(string)))
        endif

!downstream node of Industry
      Elseif (ikindup .eq. 15) THEN
        NODOWN = DONODE(INODE)
        if (EINode(NODOWN,3) .eq. 6) then                ! industrie naar rand
            call ConfArr_set_EIBND(INODE, EINode(DONODE(INODE),2))
        elseif (EINode(NODOWN,3) .eq. 4) then            ! industrie naar open water
            call ConfArr_set_EIOW(INODE, EINode(DONODE(INODE),2))
        elseif (ikinddw .EQ. 7) THEN
!            write(*,*) ' Test industry connection to NWRW node'
             call ConfArr_set_EIPluv(INODE,EINode(DONODE(INODE),2))
             RRCFConnect(nodown) = .true.
             If (RunSimultaneous) OnlineSobekLevelUsed = .true.
             UrbanRuralConnected = .true.
        elseif (EINode(NODOWN,3) .eq. 14) then           ! industrie naar Rwzi
            call ConfArr_set_EIRWZI(INODE, EINode(DONODE(INODE),2))
! also set the upstream node of RWZI to be this Industry
            call ConfArr_set_UPNODE(NODOWN, INODE)
! ARS 12563  Add option to connect industry to NWRW node
        elseif (EINode(NODOWN,3) .eq. 7) then           ! industrie naar NWRW
!           write(*,*) ' test Industry connection to NWRW'
! ARS .....  Add Muskingum Routing link and RRConnection node
        elseif (EINode(NODOWN,3) .eq. 30) then           ! industry to RRConnection
!            write(*,*) ' Test connection to Connection node'
             call ConfArr_set_EIConn(INODE,EINode(DONODE(INODE),2))
        elseif (EINode(NODOWN,3) .eq. 32) then           ! industry to RRBifurcation
!            write(*,*) ' Test connection to Bifurcation node'
             call ConfArr_set_EIBifur(INODE,EINode(DONODE(INODE),2))
        else
           STRING = Id_Nod(INode)
           Err917 = .true.
           call ErrMsgStandard (917, IECOD, ' Linkmodule for node ', STRING(1:Len_Trim(string)))
        endif
      ! ow-precipitation node
      ELSEIF (ikindup  .EQ. 21 .AND. ikinddw .EQ. 6) THEN         ! ow-precip naar rand
          if (EIBND(INODE) .gt. 0) THEN
             STRING = Id_Nod(INode)
             Err917 = .true.
             call ErrMsgStandard (917, IECOD, ' Linkmodule for node ', STRING(1:Len_Trim(string)))
          else
             IOW  = EINode(INODE, 2)
             NODOWN = DONODE(INODE)
             IBND = EINode(NODOWN, 2)
             OWRainBND(IOW) = IBND
     ! initialise EIBND array
             call ConfArr_set_EIBND(INODE, EINode(DONODE(INODE),2))
          endif

!downstream node for Sacramento or RRRunoff (External/HBV/SCS/LGSI/NAM/Wagmod/Walrus)
      Elseif (ikindup .eq. 16 .or. ikindup .eq. 18 .or. ikindup .eq. 19 .or. ikindup .eq. 20 .or. &
                 ikindup .eq. 22 .or. ikindup .eq. 23 .or. ikindup .eq. 31) THEN
        NODOWN = DONODE(INODE)
        if (EINode(NODOWN,3) .eq. 6) then                ! Sacramento/RRRunoff naar rand
            call ConfArr_set_EIBND(INODE, EINode(DONODE(INODE),2))
! Nov 2003 Taiwan Sacramento mag ook naar open water
        elseif (EINode(NODOWN,3) .eq. 4) then            ! Sacramento/RRRunoff naar open water
            call ConfArr_set_EIOW(INODE, EINode(DONODE(INODE),2))
! ARS 12563  Add option to connect industry to NWRW node
        elseif (EINode(NODOWN,3) .eq. 7) then            ! Sacramento/RRRunoff to NWRW
!           write(*,*) ' test Sacramento/RRRunoff connection to NWRW'
            call ConfArr_set_EIPluv(INODE,EINode(DONODE(INODE),2))
            RRCFConnect(nodown) = .true.
            If (RunSimultaneous) OnlineSobekLevelUsed = .true.
            UrbanRuralConnected = .true.
! Taiwan April 2004
! ARS .....  Add Muskingum Routing link and RRConnection node
        ELSEIF (ikinddw .EQ. 30) THEN                                ! Sacramento/RRRunoff to Connection
!            write(*,*) ' Test connection to Connection node'
             call ConfArr_set_EIConn(INODE,EINode(DONODE(INODE),2))
        ELSEIF (ikinddw .EQ. 32) THEN                                ! Sacramento/RRRunoff to Bifurcations
!            write(*,*) ' Test connection to RRBifurcation node'
             call ConfArr_set_EIBifur(INODE,EINode(DONODE(INODE),2))
        Else
           STRING = Id_Nod(INode)
           Err917 = .true.
           call ErrMsgStandard (917, IECOD, ' Linkmodule for node ', STRING(1:Len_Trim(string)))
        endif

!downstream node for cell
      Elseif (ikindup .eq. 17) THEN
        NODOWN = DONODE(INODE)
        if (EINode(NODOWN,3) .eq. 6) then                          ! to boundary
            call ConfArr_set_EIBND(INODE, EINode(DONODE(INODE),2))
        elseif (EINode(NODOWN,3) .eq. 4) then                      ! to open water
            call ConfArr_set_EIOW(INODE, EINode(DONODE(INODE),2))
        elseif (EINode(NODOWN,3) .eq. 7) then                      ! to NWRW node
            call ConfArr_set_EIPluv(INODE,EINode(DONODE(INODE),2))
            RRCFConnect(nodown) = .true.
            If (RunSimultaneous) OnlineSobekLevelUsed = .true.
            UrbanRuralConnected = .true.
        else
           STRING = Id_Nod(INode)
           Err917 = .true.
           call ErrMsgStandard (917, IECOD, ' Linkmodule for node ', STRING(1:Len_Trim(string)))
        endif

!downstream node for Connection node /bifurcation node
      Elseif (ikindup .eq. 30 .or. IkindUp .eq. 32) THEN
        NODOWN = DONODE(INODE)
        if (EINode(NODOWN,3) .eq. 6) then                ! Connection naar rand
             call ConfArr_set_EIBnd(INODE,EINode(DONODE(INODE),2))
        elseif (ikinddw .EQ. 30) THEN                                ! Connection naar Connection
             call ConfArr_set_EIConn(INODE,EINode(DONODE(INODE),2))
        elseif (ikinddw .EQ. 32) THEN                                ! Connection naar Bifurcation
             call ConfArr_set_EIBifur(INODE,EINode(DONODE(INODE),2))
        else
           STRING = Id_Nod(INode)
           Err917 = .true.
           call ErrMsgStandard (917, IECOD, ' Linkmodule for node ', STRING(1:Len_Trim(string)))
        endif

      Endif

! additional tests
!upstream node of Industry
      IF (EINode(INODE2,3) .eq. 15) THEN
        NodeUp = UPNODE(INODE2)
        if (NodeUp .le. 0 .and. EINode(INODE,3) .eq. 4) then
            call ConfArr_set_UPNODE(INODE2, INODE)
        elseif (NodeUp .le. 0 .and. EINode(INODE,3) .eq. 6) then
            call ConfArr_set_UPNODE(INODE2, INODE)
        else
          errorString = "Error: Upstream of industry node should be an open water or a boundary node. " // &
                         Id_Nod(INode2)
          Err920 = .true.
          call ErrMsgStandard (920, IECOD, ' Linkmodule ', errorString)
        endif
      endif

!upstream node of Connection node / bifurcation node
      IF (EINode(INODE2,3) .eq. 30 .or. EINode(Inode2,3) .eq. 32) THEN
        NodeUp = UPNODE(INODE2)
        if (NodeUp .le. 0 .and. EINode(INODE,3) .eq. 16) then
            call ConfArr_set_UPNODE(INODE2, INODE)
        elseif (NodeUp .le. 0 .and. EINode(INODE,3) .eq. 15) then
            call ConfArr_set_UPNODE(INODE2, INODE)
        elseif (NodeUp .le. 0 .and. EINode(INODE,3) .eq. 18) then
            call ConfArr_set_UPNODE(INODE2, INODE)
        elseif (NodeUp .le. 0 .and. EINode(INODE,3) .eq. 19) then
            call ConfArr_set_UPNODE(INODE2, INODE)
        elseif (NodeUp .le. 0 .and. EINode(INODE,3) .eq. 20) then
            call ConfArr_set_UPNODE(INODE2, INODE)
        elseif (NodeUp .le. 0 .and. EINode(INODE,3) .eq. 22) then
            call ConfArr_set_UPNODE(INODE2, INODE)
        elseif (NodeUp .le. 0 .and. EINode(INODE,3) .eq. 23) then
            call ConfArr_set_UPNODE(INODE2, INODE)
        elseif (NodeUp .le. 0 .and. EINode(INODE,3) .eq. 31) then
            call ConfArr_set_UPNODE(INODE2, INODE)
        elseif (NodeUp .le. 0 .and. EINode(INODE,3) .eq. 30) then
            call ConfArr_set_UPNODE(INODE2, INODE)
        elseif (NodeUp .le. 0 .and. EINode(INODE,3) .eq. 32) then
            call ConfArr_set_UPNODE(INODE2, INODE)
        elseif (Nodeup .le. 0) then
          errorString = "Error: Upstream of RR-Connection node should be a RR-bifurcation node, RR-connection node, Sacramento, " // &
                        "Ext Runoff, HBV, SCS, LGSI, Wagmod, Walrus, NAM or Industry node. " // &
                         Id_Nod(INode2)
          Err920 = .true.
          call ErrMsgStandard (920, IECOD, ' Linkmodule ', errorString)
        endif
      endif


    ENDDO


    ! *********************************************************************
    ! *** Check consistency
    ! ***  - verhard, onverhard, kas: benedenstrooms moet open water of boundary zijn, of NWRW (ARS 12653)
    ! ***  - openwater:             : benedenstrooms moet kunstwerk zijn
    ! ***  - kunstwerk:             : benedenstrooms moet openwater/boundary
    ! ***  - pluvius  :             : nog geen beperking
    ! *********************************************************************

    If (NCLINK > 0) then
      DO INODE = 1,NCNODE
        IKIND = EINode(INODE,3)
        NODOWN = DONODE(INODE)
        IF (IKIND .LE. 3) THEN
          IOW = eiOW(inode)
          IBND= eiBND(inode)
          IOWSWLink  = eiOWSWLink(inode)
          IBNDSWLink = eiBNDSWLink(inode)
! ARS 12563 also option to connect to NWRW node
!         IPLUV= eiPLUV(inode)
          IPLUV= 1
          IF (IOW .le. 0 .and. ibnd .le. 0 .and. ipluv .le. 0) then
            IECOD = EINode(INODE,1)*10000 + EINode(NODOWN,1)
            STRING = Id_Nod(INode)
            Err917 = .true.
            call ErrMsgStandard (917, IECOD, ' Linkmodule for node ',  STRING(1:Len_Trim(string)))
          ELSEIF (iow .gt. 0 .and. ibnd .gt. 0 .and. ikind .ne. 1) then
 ! for unpaved/greenhouse node: only 1 open water or 1 boundary downstream allowed
            IECOD = EINode(INODE,1)*10000 + EINode(NODOWN,1)
            STRING = Id_Nod(INode)
            Err917 = .true.
            call ErrMsgStandard (917, IECOD, ' Linkmodule for node ',  STRING(1:Len_Trim(string)))
          ELSEIF (iowSwlink .gt. 0 .and. ibndSwlink .gt. 0) then
 ! only 1 open water or 1 boundary via downstream SWLink allowed
            IECOD = EINode(INODE,1)*10000 + EINode(NODOWN,1)
            STRING = Id_Nod(INode)
            Err917 = .true.
            call ErrMsgStandard (917, IECOD, ' Linkmodule for node ',  STRING(1:Len_Trim(string)))
          ENDIF
        ELSEIF (IKIND .EQ. 4 .and. nodown .gt. 0) THEN
          IF (EiNode(NODOWN,3) .NE. 5) THEN
             IECOD = EINode(INODE,1)*10000 + EINode(NODOWN,1)
             STRING = Id_Nod(INode)
             call ErrMsgStandard (918, IECOD, ' Linkmodule',  STRING(1:Len_Trim(string)))
          ENDIF
        ELSEIF (IKIND .EQ. 5) THEN
          IF (EiNode(NODOWN,3) .NE. 4 .AND. &
              EiNode(NODOWN,3) .NE. 6) THEN
             IECOD = EiNode(INODE,1)*10000 + EiNode(NODOWN,1)
             STRING = Id_Nod(INode)
             Err920 = .true.
             call ErrMsgStandard (919, IECOD, ' Linkmodule for node ', STRING(1:Len_Trim(string)))
          ENDIF
          IF (EiNode(NODOWN,3) .EQ. 4) then
             OWDOKW (NODOWN) = .TRUE.
          end if
      ! test op upstream node moved to sub RDINPT!
        ENDIF
      ENDDO
    Endif

! ARS 5443: Check op verbonden zijn van knopen

    If (Err974) Err917 = .true.
    DO INODE=1,NCNODE
      Err974 = .false.
      IKind = EINode(INode,3)
! paved, unpaved, kas: should have no upstream link; ARS 13919: otherwise fatal error
      If (Ikind .le. 3 .and. ExistUp  (Inode) .gt. 0) then
        Err974 = .true.
        Err917 = .true.
      Endif
! paved, unpaved, kas: should have at least 1 downstream link; ARS 13919: otherwise fatal error
      If (Ikind .le. 3 .and. ExistDown(Inode) .eq. 0) then
        Err974 = .true.
        Err917 = .true.
      Endif
! RR connections: at least 1 downstream link
      If (Ikind .eq. 30 .and. ExistDown(Inode) .eq. 0) then
        Err974 = .true.
        Err917 = .true.
      Endif
! RR connections: at least 1 downstream link
      If (Ikind .eq. 32 .and. ExistDown(Inode) .eq. 0) then
        Err974 = .true.
        Err917 = .true.
      Endif
! All nodes except NWRW and Sacramento nodes should have at least 1 upstream or 1 downstream link
      If (Ikind .ne. 7 .and. Ikind .ne. 16 .and. &
           ExistUp  (Inode) .eq. 0 .and. ExistDown(Inode) .eq. 0) Err974 = .true.

      if (Err974) then
          errorString = "Error: Node is not correctly connected to other nodes " // &
                         Id_Nod(INode)
          call ErrMsgStandard (974, IECOD, errorString, ' Linkmodule ')
      endif
! ARS 5335: geen fatal error geven.
!     If (Err974) Err917 = .true.
    Enddo
    deallocate (ExistUp, ExistDown)

    if (err917 .or. err920) call ErrMsgStandard (972, 0, ' Configuration/schematisation error 1:', &
                                         ' Errors in network schematisation ')

    Call SetSimulationSequence
    if (GWLinkExists) Call SetSequenceGWLinks

    Return
    End subroutine Link_determineLinkData


    Subroutine SetSimulationSequence

    ! *********************************************************************
    ! *** Simulatievolgorde:
    ! *** - Eerst alle verhard gebied, onverhard gebied, kas, Pluvius, Sacramento, RRRunoff
    ! *** - Dan de OW-Precip nodes (alleen neerslag/verdamping open water)
    ! *** - Dan randknopen met inlaatkunstwerken
    ! *** - Dan Industry zonder bovenstrooms open water
    ! *** - Dan open water zonder bovenstroomse kunstwerk
    ! ***     met direkt gelegen benedenstrooms kunstwerk / Industrie / RWZI
    ! *** - Dan de benedenstroomse open waters van de reeds benoemde kunstwerken
    ! ***   etc, mits niet ook benedenstrooms open water van een nog niet
    ! ***        gesimuleerd kunstwerk!
    ! *** - Tot slot de boundaries en evt. ivm loops niet gelabelde knopen
    ! *********************************************************************

    ! variables
    Integer iNode, Inode1, Inode2, Ilink, idum
    Integer index, ncBou1, iter, mxIter, nodeUp, kindNd, i
    Integer iNod2, kind2, noDown
    Integer, Pointer :: iHelp(:)
    Integer iDebug, iOut1
    Logical Success


    idebug = ConfFil_get_idebug()
    iout1  = ConfFil_get_iOut1()

    ! Verhard gebied, onverhard gebied, kas, Pluvius, Sacramento, RRRunoff
    INDEX = 0
    If (NcVhg+NcOvhg+NcKas .gt. 0) Call SimSeqAddNodeType (3, Index, 'LE')    ! paved, unpaved and greenhouse area
    If (NcPluv .gt. 0)  Call SimSeqAddNodeType (7, Index, 'EQ')               ! NWRW or Pluvius area
    If (NcSacr .gt. 0)  Call SimSeqAddNodeType (16, Index, 'EQ')              ! Sacramento area
    If (NcRRRunoffExternal .gt. 0)  Call SimSeqAddNodeType (18, Index, 'EQ')  ! RRRunoff area  External Runoff
    If (NcRRRunoffHBV .gt. 0)  Call SimSeqAddNodeType (19, Index, 'EQ')       ! RRRunoff area  HBV
    If (NcRRRunoffSCS .gt. 0)  Call SimSeqAddNodeType (20, Index, 'EQ')       ! RRRunoff area  SCS
    If (NcRRRunoffLGSI .gt. 0)  Call SimSeqAddNodeType (22, Index, 'EQ')      ! RRRunoff area  LGSI
    If (NcRRRunoffWagmod .gt. 0 .or. NcRRRunoffWalrus .gt. 0)  Call SimSeqAddNodeType (23, Index, 'EQ')    ! RRRunoff area Wageningen Wagmod/Walrus
    If (NcRRRunoffNAM .gt. 0)  Call SimSeqAddNodeType (31, Index, 'EQ')       ! RRRunoff area  NAM
    If (NcOwRain .gt. 0)  Call SimSeqAddNodeType (21, Index, 'EQ')            ! RR OW-precip

!   WRITE (IOut1,*) ' I,ext.knoopnr, type, upstream/downstream nodes,', ' SimSeq Step 1'
!   DO I=1,NCNODE
!      WRITE (IOut1,'(6I8,1X,A20,1X,A20)')  I,EINode(I,1), Einode(I,3),&
!            UPNODE(I),DONODE(I),SIMSEQ(I), &
!            NamNod(I),Id_Nod(I)
!   ENDDO

   ! Industrie en RWZI zonder bovenstroomse knoop (dus alleen lozingen op deze industrieen!)
    DO INODE=1,NCNODE
       KINDND = EINode(INODE,3)
       NODEUP = UPNODE(INODE)
       IF (KINDND .EQ. 15 .and. NodeUp .le. 0)  Call SimSeqAddNode (Inode, Index)
       IF (KINDND .EQ. 14 .and. NodeUp .le. 0)  Call SimSeqAddNode (Inode, Index)
    ENDDO
   ! RWZI
    If (NcRWZI .gt. 0 .and. NcIndus .le. 0) Call SimSeqAddNodeTypeIfNotThere(14, Index)
    !randknopen met een benedenstrooms kunstwerk (dus inlaten!)
    !neem aan slechts een benedenstrooms inlaatkunstwerk bij randknoop
   ! Connection nodes zonder bovenstroomse connection nodes
    NCBOU1= NCBOUN
    DO INODE=1,NCNODE
       KINDND = EINode(INODE,3)
       IF (KINDND .EQ. 6 .AND. DONODE(INODE) .GT. 0) THEN
          NCBOU1= NCBOU1 - 1
          Call SimSeqAddNode (Inode, Index)
          NODOWN = DONODE(INODE)
          ! Add downstream structure, although it is not yet known here whether the structure is of correct type
          IF (EINode(NODOWN,3) .EQ. 5 .AND. SIMSEQ(NODOWN) .EQ. 0)  Call SimSeqAddNode (NoDown, Index)
       ELSEIF (KINDND .EQ. 30 .AND. NrUpstreamConnections(INODE) .eq. 0) then
   ! Connection nodes zonder bovenstroomse connection nodes
          Call SimSeqAddNode (Inode, Index)
       ELSEIF (KINDND .EQ. 32 .AND. NrUpstreamConnections(INODE) .eq. 0) then
   ! Bifurcation nodes zonder bovenstroomse nodes
          Call SimSeqAddNode (Inode, Index)
       Endif
    ENDDO

!   WRITE (IOut1,*) ' I,ext.knoopnr, type, upstream/downstream nodes,', ' SimSeq Step 2'
!   DO I=1,NCNODE
!      WRITE (IOut1,'(6I8,1X,A20,1X,A20,1X,I8)')  I,EINode(I,1), Einode(I,3),&
!            UPNODE(I),DONODE(I),SIMSEQ(I), &
!            NamNod(I),Id_Nod(I), NrUpstreamConnections(i)
!   ENDDO

    !open waters die niet benedenstrooms van een kunstwerk liggen
    DO INODE=1,NCNODE
       KINDND = EINode(INODE,3)
       IF (KINDND .EQ. 4 .AND. .NOT. OWDOKW(INODE)) THEN
          Call SimSeqAddNode (Inode, Index)
!         and downstream node of this open water
          NODOWN = DONODE(INODE)
          Call SimSeqAddNode (Nodown, Index)
       ENDIF
    ENDDO
    !extra loop voor test bij meerdere benedenstroomse kunstwerken en/of industries bij 1 open water
    DO INODE=1,NCNODE
       KINDND = EINode(INODE,3)
       ! voeg knoop toe als het een connection is en bovenstroomse Sacramento/RRRunoff zit al in SimSeq (July 2007)
!      if (NrUpstreamConnections(Inode) .eq. 1) Call SimSeqAddNodeIfUpnodeAlready (Inode, KindNd, 30, 16, Index)
!      if (NrUpstreamConnections(Inode) .eq. 1) Call SimSeqAddNodeIfUpnodeAlready (Inode, KindNd, 30, 18, Index)
!      if (NrUpstreamConnections(Inode) .eq. 1) Call SimSeqAddNodeIfUpnodeAlready (Inode, KindNd, 30, 19, Index)
!      if (NrUpstreamConnections(Inode) .eq. 1) Call SimSeqAddNodeIfUpnodeAlready (Inode, KindNd, 30, 20, Index)
!      if (NrUpstreamConnections(Inode) .eq. 1) Call SimSeqAddNodeIfUpnodeAlready (Inode, KindNd, 30, 22, Index)
!      if (NrUpstreamConnections(Inode) .eq. 1) Call SimSeqAddNodeIfUpnodeAlready (Inode, KindNd, 30, 23, Index)
!      if (NrUpstreamConnections(Inode) .eq. 1) Call SimSeqAddNodeIfUpnodeAlready (Inode, KindNd, 30, 31, Index)
       if (NrUpstreamConnections(Inode) .eq. 1) Call SimSeqAddNodeIfUpnodeAlready (Inode, KindNd, 30, 30, Index)
       ! voeg knoop toe als het een kunstwerk is en bovenstrooms open water zit al in SimSeq
       Call SimSeqAddNodeIfUpnodeAlready (Inode, KindNd,  5, 4, Index)
       ! voeg knoop toe als het een industrie is en bovenstrooms open water zit al in SimSeq
       Call SimSeqAddNodeIfUpnodeAlready (Inode, KindNd, 15, 4, Index)
       ! voeg knoop toe als het een industrie is en bovenstroomse boundary zit al in SimSeq
       Call SimSeqAddNodeIfUpnodeAlready (Inode, KindNd, 15, 6, Index)
       ! voeg knoop toe als het een RWZI is en bovenstroomse industrie zit al in SimSeq
       Call SimSeqAddNodeIfUpnodeAlready (Inode, KindNd, 14, 15, Index)
       ! voeg bifurcation knoop toe als het 1 bovenstroomse knoop heeft die al in Simseq zit
!      if (NrUpstreamConnections(Inode) .eq. 1) Call SimSeqAddNodeIfUpnodeAlready (Inode, KindNd, 32, 16, Index)
!      if (NrUpstreamConnections(Inode) .eq. 1) Call SimSeqAddNodeIfUpnodeAlready (Inode, KindNd, 32, 18, Index)
!      if (NrUpstreamConnections(Inode) .eq. 1) Call SimSeqAddNodeIfUpnodeAlready (Inode, KindNd, 32, 19, Index)
!      if (NrUpstreamConnections(Inode) .eq. 1) Call SimSeqAddNodeIfUpnodeAlready (Inode, KindNd, 32, 20, Index)
!      if (NrUpstreamConnections(Inode) .eq. 1) Call SimSeqAddNodeIfUpnodeAlready (Inode, KindNd, 32, 22, Index)
!      if (NrUpstreamConnections(Inode) .eq. 1) Call SimSeqAddNodeIfUpnodeAlready (Inode, KindNd, 32, 23, Index)
       if (NrUpstreamConnections(Inode) .eq. 1) Call SimSeqAddNodeIfUpnodeAlready (Inode, KindNd, 32, 30, Index)
!      if (NrUpstreamConnections(Inode) .eq. 1) Call SimSeqAddNodeIfUpnodeAlready (Inode, KindNd, 32, 31, Index)
       if (NrUpstreamConnections(Inode) .eq. 1) Call SimSeqAddNodeIfUpnodeAlready (Inode, KindNd, 32, 32, Index)
    ENDDO

!   WRITE (IOut1,*) ' I,ext.knoopnr, type, upstream/downstream nodes,', ' SimSeq Step 3'
!   DO I=1,NCNODE
!      WRITE (IOut1,'(6I8,1X,A20,1X,A20,I1X,I8)')  I,EINode(I,1), Einode(I,3),&
!            UPNODE(I),DONODE(I),SIMSEQ(I), &
!            NamNod(I),Id_Nod(I), NrUpstreamConnections(i)
!   ENDDO

    ! voeg knoop toe als alle bovenstroomse knopen al in Simseq zitten
    DO INODE=1,NCNODE
       If (NrUpstreamConnections(inode) .ge. 1) Call SimSeqAddNodeIfUpNodeSAlready (Inode, Index)
    ENDDO

!   WRITE (IOut1,*) ' I,ext.knoopnr, type, upstream/downstream nodes,', ' SimSeq Step 3b'
!   DO I=1,NCNODE
!      WRITE (IOut1,'(6I8,1X,A20,1X,A20,I1X,I8)')  I,EINode(I,1), Einode(I,3),&
!            UPNODE(I),DONODE(I),SIMSEQ(I), NamNod(I),Id_Nod(I), NrUpstreamConnections(i)
!   ENDDO

    ITER = 0
    MXITER = 15   ! Nov 2001, also RWZI and Industry in iteration loop
  99 CONTINUE
    ITER = ITER + 1
    DO INODE=1,NCNODE
       KINDND = EINode(INODE,3)
       IF (KINDND .EQ. 5 .AND. SIMSEQ(INODE) .GT. 0) THEN
          NODOWN = DONODE(INODE)
          IF (EINode(NODOWN,3) .EQ. 4 .AND. SIMSEQ(NODOWN) .EQ. 0) THEN
             DO INOD2=1,NCNODE
                KIND2 = EINode(INOD2,3)
                IF (KIND2 .EQ. 5 .AND. SIMSEQ(INOD2) .EQ. 0) THEN
                   IF (DONODE(INOD2) .EQ. NODOWN) GOTO 991
                ENDIF
             ENDDO
             Call SimSeqAddNode (NoDown, Index)
          ENDIF
       ENDIF
  991  CONTINUE
    ENDDO
    DO INODE=1,NCNODE
       KINDND = EINode(INODE,3)
       IF (KINDND .EQ. 4 .AND. SIMSEQ(INODE) .GT. 0) THEN
          NODOWN = DONODE(INODE)
          if (Nodown .gt. 0) then
            IF (EINode(NODOWN,3) .EQ. 5 .AND. SIMSEQ(NODOWN) .EQ. 0) THEN
              Call SimSeqAddNode (NoDown, Index)
            ENDIF
          ENDIF
       ENDIF
    ENDDO

    ! voeg knoop toe als alle bovenstroomse knopen al in Simseq zitten
    DO INODE=1,NCNODE
       If (NrUpstreamConnections(inode) .ge. 1) Call SimSeqAddNodeIfUpNodeSAlready (Inode, Index)
    ENDDO

!   WRITE (IOut1,*) ' I,ext.knoopnr, type, upstream/downstream nodes,', ' SimSeq Step 4'
!   DO I=1,NCNODE
!      WRITE (IOut1,'(6I8,1X,A20,1X,A20)')  I,EINode(I,1), Einode(I,3),&
!            UPNODE(I),DONODE(I),SIMSEQ(I), &
!            NamNod(I),Id_Nod(I)
!   ENDDO


    DO INODE=1,NCNODE
    !extra loop voor test bij meerdere benedenstroomse kunstwerken en/of industries bij 1 open water
       KINDND = EINode(INODE,3)
       ! voeg knoop toe als het een connection is en bovenstroomse connection zit al in SimSeq
       if (NrUpstreamConnections(Inode) .eq. 1) Call SimSeqAddNodeIfUpnodeAlready (Inode, KindNd, 30,30, Index)
       if (NrUpstreamConnections(Inode) .eq. 1) Call SimSeqAddNodeIfUpnodeAlready (Inode, KindNd, 30,32, Index)
       ! voeg knoop toe als het een bifurecation is en bovenstroomse knoop zit al in SimSeq
       if (NrUpstreamConnections(Inode) .eq. 1) Call SimSeqAddNodeIfUpnodeAlready (Inode, KindNd, 32,30, Index)
       if (NrUpstreamConnections(Inode) .eq. 1) Call SimSeqAddNodeIfUpnodeAlready (Inode, KindNd, 32,32, Index)
       ! voeg knoop toe als het een kunstwerk is en bovenstrooms open water zit al in SimSeq
       Call SimSeqAddNodeIfUpnodeAlready (Inode, KindNd, 5, 4, Index)
       ! voeg knoop toe als het een industrie is en bovenstrooms open water zit al in SimSeq
       Call SimSeqAddNodeIfUpnodeAlready (Inode, KindNd, 15, 4, Index)
       ! voeg knoop toe als het een industrie is en bovenstroomse boundary zit al in SimSeq
       Call SimSeqAddNodeIfUpnodeAlready (Inode, KindNd, 15, 6, Index)
       ! voeg knoop toe als het een RWZI is en bovenstroomse industrie zit al in SimSeq
       Call SimSeqAddNodeIfUpnodeAlready (Inode, KindNd, 14, 15, Index)
    ENDDO

    ! voeg knoop toe als alle bovenstroomse knopen al in Simseq zitten
    DO INODE=1,NCNODE
       If (NrUpstreamConnections(inode) .ge. 1) Call SimSeqAddNodeIfUpNodeSAlready (Inode, Index)
    ENDDO

!   WRITE (IOut1,*) ' I,ext.knoopnr, type, upstream/downstream nodes,', ' SimSeq Step 5'
!   DO I=1,NCNODE
!      WRITE (IOut1,'(6I8,1X,A20,1X,A20)')  I,EINode(I,1), Einode(I,3),&
!            UPNODE(I),DONODE(I),SIMSEQ(I), &
!            NamNod(I),Id_Nod(I)
!   ENDDO


! Connection nodes met meerdere bovenstroomse connections
     If (NcConn .gt. 0)  then
      Do idum=1,NcConn
        DO INODE=1,NCNODE
           KINDND = EINode(INODE,3)
           if (KindNd .eq. 30 .and. NrUpstreamConnections(Inode) .gt. 1) then
              DO ILINK=1,NCLINK
                 INODE1 = LNKFRM(ILINK)
                 INODE2 = LNKTO(ILINK)
                 if (Inode2 .eq. Inode .and. SimSeq(Inode1) .eq. 0) then
                    ! upstream node found and not yet in SimSeq, quit loop
                    goto 101
                 elseif (Inode2 .eq. Inode .and. SimSeq(Inode1) .gt. 0) then
                    ! continue to find other upstream nodes
                 endif
              ENDDO
              ! all links handled and all upstream nodes already in SimSeq -> add current node
              Call SimSeqAddNode (Inode, Index)
           endif
   101     Continue
        EndDo
!       WRITE (IOut1,*) ' I,ext.knoopnr, type, upstream/downstream nodes,', ' SimSeq Step 6'
!       DO I=1,NCNODE
!          WRITE (IOut1,'(6I8,1X,A20,1X,A20)')  I,EINode(I,1), Einode(I,3),&
!                UPNODE(I),DONODE(I),SIMSEQ(I), &
!                NamNod(I),Id_Nod(I)
!       ENDDO

        ! Feb 2010: also backwards
        DO INODE=NCNode,-1,1
           KINDND = EINode(INODE,3)
           if (KindNd .eq. 30 .and. NrUpstreamConnections(Inode) .gt. 1) then
              DO ILINK=1,NCLINK
                 INODE1 = LNKFRM(ILINK)
                 INODE2 = LNKTO(ILINK)
                 if (Inode2 .eq. Inode .and. SimSeq(Inode1) .eq. 0) then
                    ! upstream node found and not yet in SimSeq, quit loop
                    goto 1011
                 elseif (Inode2 .eq. Inode .and. SimSeq(Inode1) .gt. 0) then
                    ! continue to find other upstream nodes
                 endif
              ENDDO
              ! all links handled and all upstream nodes already in SimSeq -> add current node
              Call SimSeqAddNode (Inode, Index)
           endif
   1011    Continue
        EndDo
       Enddo
     Endif

    ! voeg knoop toe als alle bovenstroomse knopen al in Simseq zitten
    DO INODE=1,NCNODE
       If (NrUpstreamConnections(inode) .ge. 1) Call SimSeqAddNodeIfUpNodeSAlready (Inode, Index)
    ENDDO

!   WRITE (IOut1,*) ' I,ext.knoopnr, type, upstream/downstream nodes,', ' SimSeq Step 7'
!   DO I=1,NCNODE
!      WRITE (IOut1,'(6I8,1X,A20,1X,A20)')  I,EINode(I,1), Einode(I,3),&
!            UPNODE(I),DONODE(I),SIMSEQ(I), &
!            NamNod(I),Id_Nod(I)
!   ENDDO


! Bifurcation nodes met meerdere bovenstroomse nodes
     If (NcBifur .gt. 0)  then
      Do idum=1,NcBifur
        DO INODE=1,NCNODE
           KINDND = EINode(INODE,3)
           if (KindNd .eq. 32 .and. NrUpstreamConnections(Inode) .gt. 1) then
              DO ILINK=1,NCLINK
                 INODE1 = LNKFRM(ILINK)
                 INODE2 = LNKTO(ILINK)
                 if (Inode2 .eq. Inode .and. SimSeq(Inode1) .eq. 0) then
                    ! upstream node found and not yet in SimSeq, quit loop
                    goto 102
                 elseif (Inode2 .eq. Inode .and. SimSeq(Inode1) .gt. 0) then
                    ! continue to find other upstream nodes
                 endif
              ENDDO
              ! all links handled and all upstream nodes already in SimSeq -> add current node
              Call SimSeqAddNode (Inode, Index)
           endif
   102     Continue
        EndDo
        ! also backwards
        DO INODE=NCNode,-1,1
           KINDND = EINode(INODE,3)
           if (KindNd .eq. 32 .and. NrUpstreamConnections(Inode) .gt. 1) then
              DO ILINK=1,NCLINK
                 INODE1 = LNKFRM(ILINK)
                 INODE2 = LNKTO(ILINK)
                 if (Inode2 .eq. Inode .and. SimSeq(Inode1) .eq. 0) then
                    ! upstream node found and not yet in SimSeq, quit loop
                    goto 1021
                 elseif (Inode2 .eq. Inode .and. SimSeq(Inode1) .gt. 0) then
                    ! continue to find other upstream nodes
                 endif
              ENDDO
              ! all links handled and all upstream nodes already in SimSeq -> add current node
              Call SimSeqAddNode (Inode, Index)
           endif
   1021    Continue
        EndDo
       Enddo
     Endif

!    WRITE (IOut1,*) ' I,ext.knoopnr, type, upstream/downstream nodes,', ' SimSeq Step 8'
!    DO I=1,NCNODE
!       WRITE (IOut1,'(6I8,1X,A20,1X,A20)')  I,EINode(I,1), Einode(I,3),&
!             UPNODE(I),DONODE(I),SIMSEQ(I), &
!             NamNod(I),Id_Nod(I)
!    ENDDO

    IF (INDEX .LT. NCNODE-NCBOU1 .AND. ITER .LT. MXITER) THEN
       GOTO 99
    ELSEIF (INDEX .LT. NCNODE-NCBOU1) THEN

    ! voeg openwaters,kunstwerken, RWZI en Industry toe die hierboven nog niet 'gevangen' zijn
    ! dit zal te maken hebben met loops of meerdere kunstwerken benedenstrooms
    ! van een open water

      If (NcOw    .gt. 0) Call SimSeqAddNodeTypeIfNotThere(4, Index)  ! Remaining open waters
      If (NcStru  .gt. 0) Call SimSeqAddNodeTypeIfNotThere(5, Index)  ! Remaining structures
      If (NcIndus .gt. 0) Call SimSeqAddNodeTypeIfNotThere(15,Index)  ! Remaining Industrial nodes
      If (NcRwzi  .gt. 0) Call SimSeqAddNodeTypeIfNotThere(14,Index)  ! Remaining RWZI's
      If (NcConn  .gt. 0) Call SimSeqAddNodeTypeIfNotThere(30,Index)  ! Remaining Connections
      If (NcBifur .gt. 0) Call SimSeqAddNodeTypeIfNotThere(32,Index)  ! Remaining Bifurcations
      If (NcCell  .gt. 0) Call SimSeqAddNodeTypeIfNotThere(17,Index)  ! Cell nodes
    Endif
    Call SimSeqAddNodeTypeIfNotThere(6, Index)                     ! Remaining boundaries

    IF (iDebug .ne. 0) THEN
       WRITE (IDEBUG,*) ' I,ext.knoopnr, type,upstream/downstream nodes,', ' SimSeq'
       DO I=1,NCNODE
          WRITE (IDEBUG,'(6I8,1X,A20,1X,A20)')  I,EINode(I,1), Einode(I,3),&
                UPNODE(I),DONODE(I),SIMSEQ(I), &
                NamNod(I),Id_Nod(I)
       ENDDO
    ENDIF
    ! Is already printed to DEBUG-File
    !WRITE (IOut1,*) ' I,ext.knoopnr, type, upstream/downstream nodes,', ' SimSeq'
    !DO I=1,NCNODE
    !   WRITE (IOut1,'(6I8,1X,A20,1X,A20)')  I,EINode(I,1), Einode(I,3),&
    !         UPNODE(I),DONODE(I),SIMSEQ(I), &
    !         NamNod(I),Id_Nod(I)
    !ENDDO

    ! inverteren array SIMSEQ
    success = DH_AllocInit (NcNode, Ihelp, 0 )
    If (.not. success) call ErrMsgStandard (981, 0, ' Error allocating arrays in subroutine ', &
                                            ' Link_DetermineLinkData')
    DO INODE=1,NCNODE
       IHELP(SIMSEQ(INODE)) = INODE
    ENDDO
    DO INODE=1,NCNODE
        SIMSEQ(INODE) = IHELP(INODE)
    ENDDO
    Deallocate (ihelp)

  End subroutine SetSimulationSequence


    Subroutine SimSeqAddNodeType(Ikind, Index, Stringoption)
! Add all nodes of type =IKIND, or type <= IKIND (depending on StringOption) to SimSeq array

    Character*2 StringOption
    Integer Ikind, Index
    Integer Inode, KindNd

    Do INODE=1,NCNODE
       KINDND = EINode(INODE,3)
       If (StringOption .eq. 'LE') then
          If (KINDND .LE. Ikind) Then
             INDEX = INDEX+1
             SIMSEQ(INODE) = INDEX
          Endif
       ElseIf (StringOption .eq. 'EQ') then
          If (KINDND .EQ. Ikind) Then
             INDEX = INDEX+1
             SIMSEQ(INODE) = INDEX
          Endif
       Endif
    Enddo

    Return
    End Subroutine SimSeqAddNodeType


    Subroutine SimSeqAddNodeTypeIfNotThere(Ikind, Index)
! Add all nodes of type IKIND to SimSeq array, if node is not already mentioned
    Integer Ikind, Index, iout1
    Integer Inode, KindNd

    Do INODE=1,NCNODE
       KINDND = EINode(INODE,3)
       If (KINDND .EQ. Ikind .and. SimSeq(Inode) .EQ. 0) then
          If (Ikind .eq. 30) then
              iout1  = ConfFil_get_iOut1()
              call SetMessage(LEVEL_WARN, 'Added connection node at end of simulation sequence; check for balance errors')
          Endif
          INDEX = INDEX+1
          SIMSEQ(INODE) = INDEX
       Endif
    Enddo

    Return
    End Subroutine SimSeqAddNodeTypeIfNotThere


    Subroutine SimSeqAddNodeIfUpnodeAlready (Inode, KindNd, Ikind, IUpkind, Index)
! bepaalt bij knoop Inode van type ikind
! de upstream node NODEUP en als deze van type IUpKind is, en al voorkomt in Simseq,
! dan wordt knoop INODE toegevoegd
    Integer Inode, KindNd, Ikind, IupKind, Index, NodeUp

    IF (KINDND .EQ. ikind .AND. SIMSEQ(INODE) .LE. 0) THEN
       NODEUP = UPNODE(INODE)
       IF (EINode(NODEUP,3) .EQ. iupkind .AND. SIMSEQ(NODEUP) .GT. 0) THEN
          INDEX = INDEX+1
          SIMSEQ(INODE) = INDEX
       ENDIF
    ENDIF

    Return
    End Subroutine SimSeqAddNodeIfUpnodeAlready


    Subroutine SimSeqAddNodeIfUpNodeSAlready (Inode, Index)
! bepaalt bij knoop Inode van type ikind
! de upstream node NODEUP , en al voorkomt in Simseq,
! dan wordt knoop INODE toegevoegd
    Integer Inode, Index, NodeUp, ilink
    Logical NodeCanBeAddedToSimseq

    NodeCanBeAddedToSimSeq = .true.

    IF (SIMSEQ(INODE) .LE. 0) THEN
       Do ilink = 1,nclink
          if (lnkTo(ilink) .eq. inode) then
             NODEUP = LnkFrm(ilink)
             if (SimSeq(nodeup) .le. 0) NodeCanBeAddedToSimSeq = .false.
          endif
       Enddo
       IF (NodeCanBeAddedToSimSeq) then
          INDEX = INDEX+1
          SIMSEQ(INODE) = INDEX
       ENDIF
    ENDIF

    Return
    End Subroutine SimSeqAddNodeIfUpNodeSAlready


    Subroutine SimSeqAddNode (NoDown, Index)
!   Add node NODOWN to SIMSEQ array at location Index, only if it doesn't yet occur
    Integer NoDown, Index

    IF (SIMSEQ(NODOWN) .EQ. 0) THEN
        INDEX = INDEX+1
        SIMSEQ(NODOWN) = INDEX
    ENDIF

    Return
    End Subroutine SimSeqAddNode


    Subroutine SetSequenceGWLinks

    ! *********************************************************************
    ! *** Simulatievolgorde grondwaterlinks
    ! *** - Eerst alle unpaved area zonder upstream gwlinks
    ! *** - Dan unpaved knopen met alle upstream unpaved gwlinks al behandeld
    ! *** - herhaal dit
    ! *** - Tot slot de evt. ivm loops niet gelabelde knopen
    ! *********************************************************************

    ! variables
    Integer iNode, Ilink
    Integer index, iter, kindNd, i
    Integer, Pointer :: iHelp(:)
    Integer iDebug, iOut1
    Logical Success
    Logical, Pointer, SAVE ::  DownstreamGWLinksHandled(:)

    idebug = ConfFil_get_idebug()
    iout1  = ConfFil_get_iOut1()

    INDEX = 0
    Success = success .and. Dh_AllocInit (Nnod, DownstreamGwLinksHandled, .false.)

    NrUpstreamGwLinksToBeHandled = NrUpstreamGWLinks
    NrDownstreamGwLinksToBeHandled = NrDownstreamGWLinks

   ! GWLinks downstream of Unpaved nodes without upstream gw links first
    DO INODE=1,NCNODE
       KINDND = EINode(INODE,3)
       IF (KINDND .EQ. 2 .and. NrUpstreamGWLinks(inode) .le. 0) then
           if (nrDownstreamGWLinks(inode) .gt. 0) Call SeqGwLinkAddDownstreamGWLinks (inode, index)
           DownstreamGwLinksHandled(inode) = .true.
       Endif
    ENDDO

    ! Go through list of gwlinks, repeatedly, to generate Simulation sequence of GWLinks
    ! works best if links specified in upstream to downstream order
    Iter = 0
 11 Continue
    DO INODE=1,NCNODE
       KINDND = EINode(INODE,3)
       IF (KINDND .EQ. 2 .and. NrUpstreamGWLinksToBeHandled(inode) .le. 0 .and. (.not. DownstreamGwLinksHandled(inode)) ) then
           if (nrDownstreamGWLinks(inode) .gt. 0) Call SeqGwLinkAddDownstreamGWLinks (inode, index)
           DownstreamGwLinksHandled(inode) = .true.
       Endif
    ENDDO
    Iter = Iter + 1
    if (Iter .lt. 20) goto 11

   ! add remainder; this may result in a not so nice order
    DO INODE=1,NCNODE
       KINDND = EINode(INODE,3)
       IF (KINDND .EQ. 2 .and. .not. DownstreamGWLinksHandled(inode)) then
           if (nrDownstreamGWLinks(inode) .gt. 0) Call SeqGwLinkAddDownstreamGWLinks (inode, index)
           DownstreamGwLinksHandled(inode) = .true.
       Endif
    ENDDO

    IF (iout1 .ne. 0) THEN
       WRITE (IOUT1,*) ' I,gwlink, upnode, donode, SeqGwLink before'
       DO I=1,NCLink
          WRITE (IOut1,'(5I8,1X)')  I,EILink(I), LnkFrm(I),LnkTo(I),SeqGWLink(I)
       ENDDO
    ENDIF


    ! inverteren array SeqGwLink
    success = DH_AllocInit (NcLink, Ihelp, 0 )
    If (.not. success) call ErrMsgStandard (981, 0, ' Error allocating arrays in subroutine ', &
                                            ' Link_DetermineLinkData')
    DO ILink=1,NCLink
       if (LinkType(ilink) .eq. 31) IHELP(SeqGwLink(Ilink)) = ILink
    ENDDO
    DO ILink=1,NCLink
       SeqGwLink(ilink) = 0
       If (Ihelp(ilink) .gt. 0) SeqGwLink(Ilink) = IHELP(Ilink)
    ENDDO
    Deallocate (ihelp)


    IF (iOut1 .ne. 0) THEN
       WRITE (Iout1,*) ' I,gwlink, upnode, donode, SeqGwLink final'
       DO I=1,NCLink
          WRITE (IOut1,'(5I8,1X)')  I,EILink(I), LnkFrm(I),LnkTo(I),SeqGWLink(I)
       ENDDO
    ENDIF


  Return
  End subroutine SetSequenceGWLinks


  subroutine SeqGwLinkAddDownstreamGWLinks (inode, index)

  integer inode, index, inode2
  integer ilink

  Do ilink=1,NcLink
     if (LinkType(ilink) .eq. 31) then
        if (LnkFrm(ilink) .eq. inode) then
           inode2 = LnkTo(ilink)
           NrUpstreamGwLinksToBeHandled(inode2) = NrUpstreamGwLinksToBeHandled(inode2) -1
           index = index + 1
           SeqGWLink(ilink) = index
        endif
     endif
  Enddo

  return
  End subroutine SeqGwLinkAddDownstreamGWLinks


  subroutine GWLinkComputations
  integer ilink, indexseq
  ! gwlink computations of gwlinks between 2 unpaved nodes

  Do indexseq=1,NcLink
     Ilink = SeqGwlink(indexseq)
     If (ilink .gt. 0) then
         If (LinkType(ilink) .eq. 31) Call DetermineGWLinkFlow(ilink)
     Endif
  Enddo

  ! to be added

  return
  End subroutine GWLinkComputations


  subroutine DetermineGWLinkFlow(ilink)

  integer ilink
  integer iout1, Idebug
  integer inodeup, inodedown
  integer iovhup, iovhdown
  real    DeltaH, GwLinkFlow, GwlDownOld, GwLUpOld, GWFlowVol, GWAreaUp, GWAreaDown

  Iout1 = ConfFil_get_iOut1()
  idebug = ConfFil_get_idebug()

  INodeUp = LnkFrm(ilink)
  INodeDown = LnkTo(ilink)

  IovhUp = Einode(InodeUp,2)
  IovhDown = Einode(InodeDown,2)
  if (Einode(InodeUp,3) .ne. 2 .or. Einode(InodeDown,3) .ne. 2) then
     call SetMessage(LEVEL_WARN, 'NodeUp  : '//trim(ID_Nod(inodeup)))
     call SetMessage(LEVEL_WARN, 'NodeDown: '//trim(ID_Nod(inodedown)))
     call SetMessage(LEVEL_FATAL, 'Upstream/downstream nodes of RR-groundwaterlink should be of type unpaved ')
  endif

  DeltaH = Gwl(iovhUp) - Gwl(iovhDown)
  GwLinkFlow = GWLink_kDValue(ilink) * DeltaH * GWLink_width(ilink) / GWLink_Length(ilink) / 86400.  ! in m3/s

  ! update upstream gw level, make sure it is below surface; otherwise limit the GWLinkFlow
   GWFlowVol = GwLinkFlow * Timesettings%TimestepSize
   BoBD(iovhup) = BoBD(iovhup) - GWFlowVol
   GwAreaUp = max (AreaGwComp(iovhUp), 0.0001)
   GWLupold = Gwl(iovhup)
   GWL(iovhup) = GWL(iovhup) - 1/BergC(iovhup) * GWFlowVol / GwAreaUp
  ! update downstream gw level, make sure it is below surface, otherwise limit the GWLinkFlow
   BoBD(iovhdown) = BoBD(iovhdown) + GWFlowVol
   GwAreaDown = max (AreaGwComp(iovhDown), 0.0001)
   GWLdownold = Gwl(iovhdown)
   GWL(iovhdown) = GWL(iovhdown) + 1/BergC(iovhDown) * GWFlowVol / GwAreaDown

   if (Gwl(iovhUp) .gt. LvlOh(iovhUp) .or. Gwl(IovhDown) .gt. LvlOh(iovhDown)) then
     ! adjustments to be made
     ! find volume correction

     ! adjust flows, volumes and gwl

     ! to be added
   endif

  ! store computed flow in output array
  QinLink(ilink) = GwLinkFlow

  if (idebug .ne. 0) then
      write(idebug,*) ' gwlink', ilink
      write(idebug,*) ' upstream node', InodeUp, iovhUp
      write(idebug,*) ' downstream node ', InodeDown, iOvhDown
      write(idebug,*) ' upstream node id:', Id_Nod(Inodeup)
      write(idebug,*) ' downstream node id:', Id_Nod(Inodedown)
      write(idebug,*) ' GwlUp old and new:', GwlupOld, GWL(iovhUp)
      write(idebug,*) ' GwlDown old and new:', GwldownOld, GWL(iovhdown)
      write(idebug,*) ' Used DeltaH, Kd    :', DeltaH, GwLink_kdValue(ilink)
      write(idebug,*) ' Used Length, Width :', GwLink_Length(ilink), GWLink_Width(ilink)
      write(idebug,*) ' Link Flow:', GwLinkFlow
  endif

  return
  End subroutine DetermineGWLinkFlow



  subroutine CheckNetwork

  Character(len=132) string
  integer       inode, nodown, nodeup, istr, ieCode
  logical       err920

! Consistentiechecks netwerk: upstream/downstream nodes types etc;
! cf. Checks in routine Rdinput uit Delft_3B versie 1.

  err920 = .false.
  DO INODE = 1,NCNODE
     NODEUP = UPNODE(INODE)
     NODOWN = DONODE(INODE)
     STRING(1:) = ' '
!    STRING(2:4) = INTCHR(EiNode(INODE,1))
     STRING(6:37) = Id_Nod(INODE)

     IF (EiNode(INODE,3) .EQ. 5) THEN
         ISTR = EiNode(INODE,2)
         IF (EiNode(NODOWN,3) .NE. 4 .AND. EiNode(NODOWN,3) .NE. 6) then
             err920 = .true.
             call ErrMsgStandard (919, 0, '  Rdinpt',  STRING)
         endif
         IF (EiNode(NODOWN,3) .EQ. 4) OWDOKW (NODOWN) = .TRUE.
         NODEUP = UPNODE(INODE)
         IF (EiNode(NODEUP,3) .NE. 4 .AND. STRTYP(ISTR) .LE. 5) THEN
             String(39:) = ' RR-structure inlet/normal type and branch directions inconsistent.'
             IeCode = EiNode(INODE,1)*10000 + EiNode(NODEUP,1)
             err920 = .true.
             call ErrMsgStandard (920, Iecode, ' LinkModule ',  STRING)
 ! ARS 3559: inlet pump etc also allowed between open water nodes
 !        ELSEIF (EiNode(NODEUP,3) .NE. 6 .AND. STRTYP(ISTR) .GE. 6) THEN
 !            call ErrMsgStandard (942, 0,' LinkModule ',STRING)
         ENDIF
! related to ARS 16510: add some other checks
     ELSEIF (EiNode(INODE,3) .EQ. 1 .and. Nodown .le. 0) THEN
         String(39:) = ' Paved node has no downstream node'
         IeCode = EiNode(INODE,1)*10000
         err920 = .true.
         call ErrMsgStandard (920, Iecode, ' LinkModule ',  STRING)
     ELSEIF (EiNode(INODE,3) .EQ. 2 .and. Nodown .le. 0) THEN
         String(39:) = ' Unpaved node has no downstream node'
         IeCode = EiNode(INODE,1)*10000
         err920 = .true.
         call ErrMsgStandard (920, Iecode, ' LinkModule ',  STRING)
     ELSEIF (EiNode(INODE,3) .EQ. 3 .and. Nodown .le. 0) THEN
         String(39:) = ' Greenhouse node has no downstream node'
         IeCode = EiNode(INODE,1)*10000
         err920 = .true.
         call ErrMsgStandard (920, Iecode, ' LinkModule ',  STRING)
     ELSEIF (EiNode(INODE,3) .EQ. 14 .and. Nodown .le. 0) THEN
         String(39:) = ' WWTP has no downstream node'
         IeCode = EiNode(INODE,1)*10000
         err920 = .true.
         call ErrMsgStandard (920, Iecode, ' LinkModule ',  STRING)
!    upstream node for WWTP is not set
!    ELSEIF (EiNode(INODE,3) .EQ. 14 .and. Nodeup .le. 0) THEN
!        String(39:) = ' WWTP has no upstream node'
!        IeCode = EiNode(INODE,1)*10000
!        err920 = .true.
!        call ErrMsgStandard (920, Iecode, ' LinkModule ',  STRING)
     ELSEIF (EiNode(INODE,3) .EQ. 15 .and. Nodeup .le. 0 .and. Nodown .le. 0) THEN
         String(39:) = ' Industry node has no upstream and no downstream node'
         IeCode = EiNode(INODE,1)*10000
         err920 = .true.
         call ErrMsgStandard (920, Iecode, ' LinkModule ',  STRING)
     ELSEIF (EiNode(INODE,3) .EQ. 16 .and. Nodown .le. 0) THEN
         String(39:) = ' Sacramento node has no downstream node'
         IeCode = EiNode(INODE,1)*10000
         err920 = .true.
         call ErrMsgStandard (920, Iecode, ' LinkModule ',  STRING)
     ELSEIF (EiNode(INODE,3) .EQ. 18 .and. Nodown .le. 0) THEN
         String(39:) = ' External Runoff node has no downstream node'
         IeCode = EiNode(INODE,1)*10000
         err920 = .true.
         call ErrMsgStandard (920, Iecode, ' LinkModule ',  STRING)
     ELSEIF (EiNode(INODE,3) .EQ. 19 .and. Nodown .le. 0) THEN
         String(39:) = ' HBV-Runoff node has no downstream node'
         IeCode = EiNode(INODE,1)*10000
         err920 = .true.
         call ErrMsgStandard (920, Iecode, ' LinkModule ',  STRING)
     ELSEIF (EiNode(INODE,3) .EQ. 20 .and. Nodown .le. 0) THEN
         String(39:) = ' SCS-Runoff node has no downstream node'
         IeCode = EiNode(INODE,1)*10000
         err920 = .true.
         call ErrMsgStandard (920, Iecode, ' LinkModule ',  STRING)
     ELSEIF (EiNode(INODE,3) .EQ. 22 .and. Nodown .le. 0) THEN
         String(39:) = ' LGSI-Runoff node has no downstream node'
         IeCode = EiNode(INODE,1)*10000
         err920 = .true.
         call ErrMsgStandard (920, Iecode, ' LinkModule ',  STRING)
     ELSEIF (EiNode(INODE,3) .EQ. 23 .and. Nodown .le. 0) THEN
         String(39:) = ' Wageningen/Walrus-Runoff node has no downstream node'
         IeCode = EiNode(INODE,1)*10000
         err920 = .true.
         call ErrMsgStandard (920, Iecode, ' LinkModule ',  STRING)
     ELSEIF (EiNode(INODE,3) .EQ. 31 .and. Nodown .le. 0) THEN
         String(39:) = ' NAM-Runoff node has no downstream node'
         IeCode = EiNode(INODE,1)*10000
         err920 = .true.
         call ErrMsgStandard (920, Iecode, ' LinkModule ',  STRING)
     ENDIF
  ENDDO
  if (err920) call ErrMsgStandard (972, 0, ' Configuration/schematisation error 2:', &
                                   ' Errors in network schematisation ')

  end subroutine CheckNetwork

  !> If success, function returns Values array of length ElementCount
  !! for Link elementset on specific quantity handle
  function RR_GetLinkDataByIntId(QuantityHandle, ElementCount, Values) result(success)

    use RR_open_mi_support
    use wl_open_mi_support

    implicit none

    ! return value
    logical  :: success

    ! arguments
    integer,          intent(in)      :: QuantityHandle   !< quant. handle
    integer,          intent(in)      :: ElementCount     !< #elems in link elementset
    double precision, intent(out), &
            dimension(1:ElementCount) :: Values           !< values in link elemenset

    ! locals

    Values  = 0
    success = .true.

    select case (QuantityHandle)
    ! Subdivide for various variables
    case(RRiFlow)
    !RR Link flows in m3/s
            ! If a stack overflow occurs due to this array operation, use a do loop
            if (NLNK > 0) then
                Values(1:NLNK) = RslMap16_flows(1, 1:NLNK, 1)
            else
                success = .false.
            endif
    case default
    ! Something is wrong
        success = .false.
    end select

  end function

end module Link
