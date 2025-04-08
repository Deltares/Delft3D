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

 module RRConnectionBifurcationNodes

  use Conf_fil
  use Conf_Arr
  use NewTables
  use Network
  use DH_alloc
  use Link

  ! variables

  implicit none

  type QtoConn
    Real totalSacramento
    Real totalIndustry
    Real totalConnection
  end type QtoConn

  type (QtoConn), allocatable :: QinConn(:)

  Integer  MaxNrBifurLinks
  Integer, Pointer, SAVE ::  BifurNrLinks(:)
  Integer, Pointer, SAVE ::  BifurLinkNumbers(:,:)
  REAL, Pointer, SAVE ::     BifurFractions(:,:)
  Real, Pointer, SAVE ::     QBifur(:)

  REAL, Pointer, SAVE ::     QConn(:), QConnMx(:)

contains

  Subroutine RRBifurcation_confAr1

    implicit none
    Integer iOut1
    Logical Success

    iOut1 = ConfFil_get_iOut1()

    MaxNrBifurLinks = 10
    NBifur = MAX (1, NCBifur ) !RRBifurcation
    If ( (NBifur .GT. 0) .and. (iOut1 .ne. 0) ) then
      WRITE(IOUT1,*) ' RRBifurcation nodes    =',NBifur
    endif

   !*** Data RRBifurcations
    Success = Dh_AllocInit (NBifur, BifurNrLinks,2)
    Success = success .and. Dh_AllocInit (NBifur,MaxNrBifurLinks, BifurLinkNumbers,0)
    Success = success .and. Dh_AllocInit (NBifur,MaxNrBifurLinks, BifurFractions,0E0)
    Success = success .and. Dh_AllocInit (NBifur, QBifur,0.E0)

    If (.not. success) call ErrMsgStandard (981, 0, ' Error allocating arrays in subroutine ', &
                                           ' Bifurcation_ConfAr1')
!   Initialisatie
    BifurNrLinks     = 0
    BifurLinkNumbers = 0
    BifurFractions   = 0.0
    QBifur           = 0.0E0

  Return
  End subroutine RRBifurcation_confAr1



  Subroutine RRConnection_confAr1

    implicit none
    Integer iOut1, Allocation_Error
    Logical Success

    iOut1 = ConfFil_get_iOut1()

    NConn = MAX (1, NCConn ) !RRConnection
    If ( (NConn .GT. 0) .and. (iOut1 .ne. 0) ) then
      WRITE(IOUT1,*) ' RRConnection nodes    =',NConn
    endif

   !*** Data RRconnections
   !*** Results RRConnections
    Success = Dh_AllocInit (NConn, QConn,0E0)
    Success = success .and. Dh_AllocInit (NConn, QConnMx,0E0)

    ALLOCATE ( QINConn(NConn), Stat=Allocation_Error )
    If (Allocation_Error .ne. 0) &
       call ErrMsgStandard (981, Allocation_Error, ' Error allocating arrays in subroutine ', &
                                           ' Connection_ConfAr1')
!   Initialisatie
    QINConn%totalConnection = 0.0
    QINConn%totalIndustry   = 0.0
    QINConn%totalSacramento = 0.0

  Return
  End subroutine RRConnection_confAr1



  Subroutine RRBifurcation_ReadAsciiInput (infile1)

    implicit none

    integer infile1
    integer :: RetVal

    Integer    teller, index, iNod
!   Integer    ncBifur

    Integer         iecode, iout1,idebug, IBifur, i,j, inode
    Character(1000) string
    Logical         allow, found, endfil
    Integer         NHLP
    Parameter       (NHLP=32)
    Integer         IDUM(NHLP)
    REAL            RDUM(NHLP)
    Character(CharIdLength)   CDUM(NHLP), id, LinkName
    Logical       , Pointer :: AlreadyRead(:)
    Logical success
    Real    TotalFrac
    Character(Len=CharIdLength)  FileName
    Integer                      IoUnit


    iOut1 = ConfFil_get_iOut1()
    iDebug = ConfFil_get_iDebug()

    allow = .false.
    found = .false.

    Success = Dh_AllocInit (NBifur, AlreadyRead, .false.)

! *********************************************************************
! ***  If CleanRRFiles, also write cleaned input
! *********************************************************************
   if (CleanRRFiles) then
        FileName = ConfFil_get_namFil(44)
        FileName(1:) = Filename(1:Len_trim(FileName)) // '_cleaned'
        Call Openfl (iounit, FileName,1,3)  !sacrmnto.3b cleand in append mode !
        Call ErrMsgStandard (999, 1, ' Cleaning sacrmnto.3b for RR-connection node input to file', FileName)
   endif

! *********************************************************************
! read BIFU records from Sacramento file
! *********************************************************************
    call SetMessage(LEVEL_DEBUG, 'Read BIFU records')
    Endfil = .false.
    teller = 0
    RetVal = 0
    CALL SKPCOM (Infile1, ENDFIL,'ODS')
    do while (.not. endfil)
      READ(infile1,'(A1000)',END=21,ERR=150,IOSTAT=IECODE) STRING
! Skip regel als hij niet begin met juist keyword (BIFU)
! example:    BIFU id '12' nl 3 li '8' '9' '10' fr 0.2 0.5 0.3 bifu
      If (STRING(1:4) .eq. 'BIFU') Then
! bifurcation node id
       RetVal = RetVal + GetVAR2 (STRING,' id ',1,' Bifurcation_readAscii',' 3B_Link.tp file',IOUT1, &
                     ID, RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
       index = 0
       call fndNd2(index, id)
       if (index .gt. 0) then
        inod = index
        index = EiNode(inod,2)
        if (EiNode(inod,3) .eq. 32) then  ! knoop is een Bifurcation
         if (AlreadyRead(index)) then
           call SetMessage(LEVEL_ERROR, 'Data for Bifurcation node '//trim(id(1:Len_trim(id)))//' double in datafile')
         else
! cleaning RR files
           If (CleanRRFiles) write(Iounit,'(A)') String (1:len_trim(String))
           AlreadyRead(index) = .true.
           teller = teller + 1
! number of downstream links
           RetVal = RetVal + GetVAR2 (STRING,' nl ',3,' Bifurcation_readAscii',' 3B_Link.tp',IOUT1, &
                         CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
           BifurNrLinks(index)= IDUM(1)
           if (BifurNrLinks(Index) .gt. NrDownstreamLinks(inod)) then
               call SetMessage(LEVEL_FATAL, 'Number of specified bifurcation links exceeds nr of downstream links at node')
           endif
           if (BifurNrLinks(Index) .lt. NrDownstreamLinks(inod)) then
               call SetMessage(LEVEL_WARN, 'Number of specified bifurcation links is less than nr of downstream links at node; other downstream link(s) at node will have zero flow')
           endif
           if (BifurNrLinks(index) .gt. 0) then
               RetVal = RetVal + GetVRS2(STRING,' fr ',2,' NWRW-ReadAscii',' Pluvius.3B file', &
                                         IOUT1, CDUM(1), RDUM(1), IDUM(1), BifurNrLinks(index), IflRtn)
               Totalfrac = 0.0
               Do I=1,BifurNrLinks(index)
                  BifurFractions(Index,I) = RDUM(I)
                  Totalfrac = TotalFrac + Rdum(i)
               Enddo
               if ( (Abs(totalFrac-1.0)) .gt. 1E-4) then
                   call ErrMsgStandard (972, 0, ' Sum of bifurcation fractions <> 1.0', &
                                        ' Correct your input')
               endif
               RetVal = RetVal + GetVRS2(STRING,' li ',1,' NWRW-ReadAscii',' Pluvius.3B file', &
                                         IOUT1, CDUM(1), RDUM(1), IDUM(1), BifurNrLinks(index), IflRtn)
               Do I=1,BifurNrLinks(index)
                  Read (CDum(i),*) linkname
                  Do j=1,NcLink
                      if (NameLink(j) .eq. linkname) BifurLinkNumbers(Index,I) = j
                  Enddo
                  if (BifurLinkNumbers(Index,i) .le. 0) then
                   call ErrMsgStandard (972, 0, ' Bifurcation link name not found, correct your input.', &
                                        LinkName)
                  endif
               Enddo
           endif
         Endif
        Endif
       Endif
      Endif
      CALL SKPCOM (Infile1, ENDFIL,'ODS')
    Enddo
 21 continue
! cleaning RR files
    If (CleanRRFiles) Call closeGP (Iounit)

    If (RetVal .gt. 0)  call ErrMsgStandard (972, 0, ' Error reading Bifurcation data', ' Error getting BIFU records')
    If (teller .lt. NcBifur)  Then
        Do inode=1,NCNode
          if (Einode(Inode,3) .eq. 32) then   ! bifurcation node
             iBifur=Einode(Inode,2)
             ! use default fractions if not specified
             If (BifurNrLinks(Ibifur) .le. 0) then
                BifurNrLinks(IBifur) = NrDownstreamLinks(inode)
                Do i=1,NrDownstreamLinks(inode)
                   BifurFractions(IBifur,i) = 1.0 / NrDownstreamLinks(inode)
                Enddo
                i = 0
                Do j=1,NcLink
                   If (LnkFrm(j) .eq. inode) then
                       i = i+1
                       BifurLinkNumbers(IBifur,i) = j
                   Endif
                Enddo
             endif
          endif
        Enddo
        ! only warning, not fatal error
        call SetMessage(LEVEL_WARN, 'Some Bifurcation nodes in schematisation not found in Link file; default fractions will be used')
    Endif

    Deallocate (AlreadyRead)

    Return

150 CONTINUE
     call SetMessage(LEVEL_FATAL, 'Read error in Bifurcation ASCII')
     If (CleanRRFiles) Call closeGP (Iounit)

  Return
  End subroutine RRBifurcation_ReadAsciiInput



  SUBROUTINE CmpRRConnection (ITMSTP, IConn, INODE, Ilink)
    ! *********************************************************************
    ! *** Last update:  March 1995
    ! *********************************************************************
    ! *********************************************************************
    ! *** Brief description:
    ! *** ------------------
    ! ***    This subroutine performs computations for RR-connections
    ! *********************************************************************
    ! *** Input/output parameters:
    ! *** ------------------------
    ! ***  IEVENT = event number
    ! ***  ITMSTP = timestep number
    ! ***  IDEBUG = file unit number of debug file
    ! ***  IOUT1  = file unit number of output file with run messages
    ! ***  IBND   = intern boundary nr
    ! ***  IMETEO = meteostation code
    ! ***  INODE  = knoopnr
    ! *********************************************************************
    ! *** Berekeningen voor boundaries
    ! *********************************************************************


    implicit none
    Integer iConn, iTmStp, iNode, iDebug, ilink, ibnd, iConnDwn, IBifur

    iDebug = ConfFil_get_iDebug()

    Qinlink(ilink)  = QConn(iConn)
    Qoutlink(ilink) = QConn(iConn)
!   Qoutlink will be recalculated later in case of a routing link

    if (LinkType(ilink) .ne. 30) then
         Ibnd = EIBND(INODE)  ! benedenstrooms een rand
         Ibifur = EIBIFUR(INODE)  ! benedenstrooms een RRBifurcation
         IConnDwn = EIConn(INODE)  ! benedenstrooms een RRConnection
         If (IConnDwn .gt. 0) then
            QinConn(iConnDwn)%TotalConnection = QinConn(iConnDwn)%TotalConnection + QinLink(ilink)
            QConn(iConnDwn) = QConn(iConnDwn) + Qinlink(ilink)
         elseIf (IBifur .gt. 0) then
            QBifur(Ibifur) = QBifur(IBifur) + Qinlink(ilink)
         elseIf (IBnd .gt. 0) then
!  since Sacramento and connection both only once in iteration loop, they can be taken together
            QinBnd(iBnd)%TotalSacramento = QinBnd(iBnd)%TotalSacramento + QinLink(ilink)
            QBnd(iBnd) = QBnd(iBnd) + Qinlink(ilink)
         endif
    Endif

    IF (IDEBUG .ne. 0)  WRITE(IDEBUG,*) 'CMPConnection iconn=',IConn


    ! *********************************************************************
    ! *** DEBUG
    ! *********************************************************************

    IF (IDEBUG .ne. 0) THEN
       WRITE(IDEBUG,*) ' Connection name', NamNod(INODE)
       WRITE(IDEBUG,*) ' Connection node id', ID_Nod(Inode)
       WRITE(IDEBUG,*) ' Timestep nr                :',ITMSTP
       WRITE(IDEBUG,*) ' Timestepsize in s          :',timeSettings%timestepSize
       WRITE(IDEBUG,*) ' Total inflow to connection :',QConn(Iconn), &
                                         QConn(IConn) * timeSettings%timestepSize
       WRITE(IDEBUG,*) ' Total inflow Sacramento    :',QINConn(IConn)%totalSacramento
       WRITE(IDEBUG,*) ' Total inflow Industry      :',QINConn(IConn)%totalIndustry
       WRITE(IDEBUG,*) ' Total inflow Connections   :',QINConn(IConn)%totalConnection
       WRITE(IDEBUG,*) ' Total outflow to downstream link id:',NameLink(ilink)
       WRITE(IDEBUG,*) ' Total outflow downstream link:',QINlink(ilink), Qoutlink(ilink)
    ENDIF

    RETURN
  END subroutine CMPRRConnection




  Subroutine Init1RRConnection
    ! *********************************************************************
    ! *** Last update:
    ! *********************************************************************
    ! *** Brief description:
    ! *** ------------------
    ! ***    Initialise per event
    ! *********************************************************************

      Implicit none

      QConn = 0.
      QInConn%totalSacramento = 0.
      QInConn%totalIndustry   = 0.
      QInConn%totalConnection = 0.

  Return
  END subroutine Init1RRConnection


  Subroutine Init2RRConnection
    ! *********************************************************************
    ! *** Last update:
    ! *********************************************************************
    ! *** Brief description:
    ! *** ------------------
    ! ***    initialisatie per tijdstap
    ! *********************************************************************

      Implicit none

      QConn = 0.
      QInConn%totalSacramento = 0.
      QInConn%totalIndustry   = 0.
      QInConn%totalConnection = 0.

  Return
  END subroutine Init2RRConnection



  Subroutine RRConnection_DeAllocateArrays

    implicit none
    if (Allocated(QinConn)) DeAllocate(QINConn)

  Return
  End subroutine RRConnection_DeallocateArrays


  SUBROUTINE CmpRRBifurcation (ITMSTP, IBifur, INODE, Ilink)
    ! *********************************************************************
    ! *** Last update:  March 1995
    ! *********************************************************************
    ! *********************************************************************
    ! *** Brief description:
    ! *** ------------------
    ! ***    This subroutine performs computations for RR-connections
    ! *********************************************************************
    ! *** Input/output parameters:
    ! *** ------------------------
    ! ***  IEVENT = event number
    ! ***  ITMSTP = timestep number
    ! ***  IDEBUG = file unit number of debug file
    ! ***  IOUT1  = file unit number of output file with run messages
    ! ***  IBND   = intern boundary nr
    ! ***  IMETEO = meteostation code
    ! ***  INODE  = knoopnr
    ! *********************************************************************
    ! *** Berekeningen voor boundaries
    ! *********************************************************************


   implicit none
   Integer iBifur, iTmStp, iNode, iDebug, ilink, ibnd, iConnDwn, IlinkBif, iBifDwn, ilink2, nodown2

    iDebug = ConfFil_get_iDebug()

    if (Idebug .ne. 0) write (Idebug,*) 'Nr Downstream bifurcation links =', BifurNrLinks(Ibifur)
    Do ilinkBif=1,BifurNrLinks(IBifur)
       Ilink2 = BifurLinkNumbers(IBifur,ilinkBif)
       if (Idebug .ne. 0) write (Idebug,*) 'Link nr   Link id   fraction', IlinkBif, Ilink2, BifurLinkNumbers(Ibifur,IlinkBif), BifurFractions(Ibifur,IlinkBif)
       Qinlink(ilink2)  = QBifur(iBifur) * BifurFractions(Ibifur,ilinkBif)
       Qoutlink(ilink2) = QBifur(iBifur) * BifurFractions(Ibifur,ilinkBif)
!      Qoutlink will be recalculated later in case of a routing link
       Nodown2 =LnkTo(ilink2)
       if (LinkType(ilink2) .ne. 30) then
          if (EiNode(Nodown2,3) .eq. 6) then
             ! boundary
              IBnd = Einode(Nodown2,2)
              QinBnd(iBnd)%TotalSacramento = QinBnd(iBnd)%TotalSacramento + QinLink(ilink2)
              QBnd(iBnd) = QBnd(iBnd) + Qinlink(ilink2)
          elseif (EiNode(Nodown2,3) .eq. 30) then
             ! connection
              IConnDwn = Einode(Nodown2,2)
              QinConn(iConnDwn)%TotalConnection = QinConn(iConnDwn)%TotalConnection + QinLink(ilink2)
              QConn(iConnDwn) = QConn(iConnDwn) + Qinlink(ilink2)
          elseif (EiNode(Nodown2,3) .eq. 32) then
             ! bifurcation
              IBifDwn = Einode(Nodown2,2)
              QBifur(iBifDwn) = QBifur(iBifDwn) +  Qinlink(ilink2)
          endif
       Endif
    Enddo

    IF (IDEBUG .ne. 0)  WRITE(IDEBUG,*) 'CMPRRBifurcation ibifur=',IBifur

    ! *********************************************************************
    ! *** DEBUG
    ! *********************************************************************

    IF (IDEBUG .ne. 0) THEN
       WRITE(IDEBUG,*) ' Bifurcation', NamNod(INODE)
       WRITE(IDEBUG,*) ' Timestep nr                 :',ITMSTP
       WRITE(IDEBUG,*) ' Timestepsize in s           :',timeSettings%timestepSize
       WRITE(IDEBUG,*) ' Total inflow to bifurcation :',QBifur(IBifur), QBifur(IBifur) * timeSettings%timestepSize
    ENDIF

    RETURN
  END subroutine CMPRRBifurcation




  Subroutine Init1RRBifurcation
    ! *********************************************************************
    ! *** Last update:
    ! *********************************************************************
    ! *** Brief description:
    ! *** ------------------
    ! ***    Initialise per event
    ! *********************************************************************

      Implicit none

      QBifur = 0.

  Return
  END subroutine Init1RRBifurcation


  Subroutine Init2RRBifurcation
    ! *********************************************************************
    ! *** Last update:
    ! *********************************************************************
    ! *** Brief description:
    ! *** ------------------
    ! ***    initialisatie per tijdstap
    ! *********************************************************************

      Implicit none

      QBifur = 0.

  Return
  END subroutine Init2RRBifurcation



  Subroutine RRBifurcation_DeAllocateArrays



  Return
  End subroutine RRBifurcation_DeallocateArrays






end module RRConnectionBifurcationNodes
