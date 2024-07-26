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


      SUBROUTINE NODELP (IEVENT, ITMSTP, ITER, IDAYWK, makelogfile, MessageInundation,&
                         NegativeVolumeInCurrentTimestep, NrTimestepsNegativeVolume, GlobalNAMAlfa)
! *********************************************************************
! ***                D E L F T         H Y D R A U L I C S
! ***              WATER RESOURCES AND ENVIRONMENT DIVISION
! *********************************************************************
! *** Program :  DELFT_3B version 1.00                Date: March 1995
! *********************************************************************
! *** Last update:  November 0207
! *********************************************************************
! *********************************************************************
! *** Brief description:
! *** ------------------
! ***    This subroutine performs the simulation.
! ***    All nodes are simulated according to the order
! ***    in the simulation sequence.
! ***    For each node type which requires some computations
! ***    a seperate subroutine is called.
! *********************************************************************
! *** Input/output parameters:
! *** ------------------------
! ***  IEVENT = event number
! ***  ITMSTP = timestep number
! ***  IDEBUG = file unit number of debug file
! ***  IOUT1  = file unit number of output file with run messages
! ***  ITER   = iteration counter
! ***  IDAYWK = day of the week
! *********************************************************************
! *** LOOP OVER THE NUMBER OF NODES
! *********************************************************************
!
      USE CONF_FIL
      USE CONF_ARR
      use Boundary
      use Greenhouse
      use NWRW
      use Openwater
      use Paved
      use Structures
      use Unpaved
      use Rwzi
      Use Industry
      Use Sacramento
      Use RRConnectionBifurcationNodes
      Use RRRunoff
      Use Link
      Use RRRouting
      Use ReadLib

      Integer iEvent, iTmStp, iDayWk, makelogfile, MessageInundation, NrTimestepsNegativeVolume
      Integer iPlv2, iplv3, iplv4, j, iSNo, iKnoop, kindNd, iMeteo, IEvap, iRunoff, iTemperature, iKndNr, ILink, iter, iOw, ibnd
      Integer iDebug, Iout1
      Integer len
      Logical SkipNode, NegativeVolumeInCurrentTimestep
      Double precision MinDepthCF
      Character(Len=CharIdLength) NodeName
      REAL    GlobalNAMAlfa


      iDebug = ConfFil_get_iDebug()
      iOut1  = ConfFil_get_iOut1()
      MinDepthCF = MinimumDepthCF

!     Vector/Array initialisation
      COMPPV = .false.
      SpecialCOMPPV = .false.

      SimpleVolumeCheck         = .false.
      SevereVolumeCheck         = .false.
      MaxVolChkFrictionBoundary = .false.
      MaxVolChkWeirBoundary     = .false.
      MaxVolChkOrificeBoundary  = .false.

      DO ISNO=1,NCNODE

          IKNOOP = SIMSEQ(ISNO)
          KINDND = EINODE(IKNOOP,3)
          IKNDNR = EINODE(IKNOOP,2)
          IMETEO = NODMET(IKNOOP)
          IEvap  = NODEVAP(IKNOOP)
          IRunoff = NODRunoff(IKNOOP)
          ITemperature = NODTemperature(IKNOOP)
          ILink  = DownstreamLinkNr(Iknoop)
          SkipNode = .false.
          if (idebug .ne. 0) then
             write(idebug,*) ' debugNodelp    ',isno, iknoop
             NodeName = Id_Nod(IKNOOP)
             write(idebug,*) ' node id      = ',NodeName(1:32)
             write(idebug,*) ' kind         = ',KindNd
             write(idebug,*) ' seq.inkind   = ',IKndNr
             write(idebug,*) ' meteostation = ',IMeteo
             write(idebug,*) ' runoffstation = ',IRunoff
             write(idebug,*) ' Temperaturestation = ',ITemperature
             write(idebug,*) ' Ilink        = ',ILink
             write(idebug,*) ' DownstreamLinkNr =', DownstreamLinkNr
          endif

          if ((kindNd .ne. 5) .and. (kindNd .ne. 6) .and. (kindNd .ne. 14) .and. &
               (kindNd .ne. 15) .and. (kindNd .ne. 18) .and. (kindNd .ne. 30) .and. (kindNd .ne. 32) ) then
!               no check for structures, boundaries, RWZI and industry, ExtRunoff, RR Connection/bifurcation nodes
                if ((iMeteo .eq. 0) .or. (iMeteo .gt. ncMet)) then
                   call ErrMsgStandard(972, 0, "Nodelp: ", "no corresponding meteostation found in rainfallfile")
                endif
          endif
! Aanpassing Taiwan Nov 2001: industrie en dus ook RWZI kan gekort worden, dus in iteratieloop meenemen!!
! in iteration loop: voor ITER>1 alleen open water, structures, onverhard gebied, industrie, RWZI
!               dus: verhard gebied, kassen, NWRW, Sacramento niet!
          IF (ITER .GT. 1 .AND. KINDND .EQ. 1)   SkipNode = .true.
          IF (ITER .GT. 1 .AND. KINDND .EQ. 3)   SkipNode = .true.
! 25 Oct 2002 NWRW ook skippen (was blijkbaar weggevallen)
          IF (ITER .GT. 1 .AND. KINDND .eq. 7)  SkipNode = .true.
! Sacramento
          IF (ITER .GT. 1 .AND. KINDND .eq. 16)  SkipNode = .true.
! External Runoff, HBV, SCS, NAM, LGSI, WagMod, also the OW-precip node
          IF (ITER .GT. 1 .AND. KINDND .eq. 18)  SkipNode = .true.
          IF (ITER .GT. 1 .AND. KINDND .eq. 19)  SkipNode = .true.
          IF (ITER .GT. 1 .AND. KINDND .eq. 20)  SkipNode = .true.
          IF (ITER .GT. 1 .AND. KINDND .eq. 21)  SkipNode = .true.
          IF (ITER .GT. 1 .AND. KINDND .eq. 22)  SkipNode = .true.
          IF (ITER .GT. 1 .AND. KINDND .eq. 23)  SkipNode = .true.
          IF (ITER .GT. 1 .AND. KINDND .eq. 31)  SkipNode = .true.
! Taiwan April 2004
          IF (ITER .GT. 1 .AND. KINDND .eq. 30)  SkipNode = .true.
          IF (ITER .GT. 1 .AND. KINDND .eq. 32)  SkipNode = .true.
          ! Bifurcation en Connection nodes ook buiten de iteraties, zolang er alleen Sacrmento, RRRunoff of connection nodes op afwateren

          If (.Not. SkipNode) then
            select case (KindNd)
            case (1)
!              Verhard gebied knoop
               IF (iDebug .ne. 0) WRITE(IDEBUG,*) ' Paved area'
               IF (OldPavedComputations) then
                   CALL CmpVHG (IEVENT, ITMSTP, IKNDNR, IMETEO, IKNOOP, MessageInundation)
               else
                   CALL CmpVHG2016 (IEVENT, ITMSTP, IKNDNR, IMETEO, IKNOOP, MessageInundation)
               endif
            case (2)
!              Onverhard gebied
               IF (iDebug .ne. 0) WRITE(IDEBUG,*) ' Unpaved area'
               CALL CmpOVH (IEVENT, ITMSTP, IKNDNR, IMETEO, IKNOOP, makelogfile, MessageInundation, MinDepthCF, Iter)
               IF (ITER .GT. 2) THEN
                 IOW  = EIOW(IKNOOP)
                 IBND = EIBND(IKNOOP)
                 IF (IOW .GT. 0) THEN
                   IF (LVLOW0(IOW) .GT. LVLOH(IKNDNR)) THEN
                     if (iOut1 .ne. 0 .and. MessageInundation .gt. 0) then
                       NodeName = Id_Nod(IKNOOP)
                       Len = Len_trim (NodeName)
                       WRITE(Iout1,'(A,A,A,2I5)')   &
                        ' Inundation unpaved area', NodeName(1:Len), ' in event/timestep', IEVENT, ITMSTP
                     endif
                   ENDIF
                 ELSEIF (IBND .GT. 0) THEN
                   IF (BNDPAR(IBND,1) .GT. LVLOH(IKNDNR)) THEN
                     if (iOut1 .ne. 0 .and. MessageInundation .gt. 0) then
                       NodeName = Id_Nod(IKNOOP)
                       Len = Len_trim (NodeName)
                       WRITE(Iout1,'(A,A,A,2I5)')   &
                        ' Inundation unpaved area', NodeName(1:len), ' in event/timestep', IEVENT, ITMSTP
                     endif
                   ENDIF
                 ENDIF
               ENDIF
            case (3)
!              Kasgebied
               IF (iDebug .ne. 0) WRITE(IDEBUG,*) ' Greenhouse area'
               CALL CmpKAS (IEVENT, ITMSTP, IKNDNR, IMETEO, IKNOOP, makelogfile)
            case (4)
!              Open water
!              if (itmstp .eq. 161)  call ConfFil_set_IDEBUG(IdebugLunRR)
               IF (iDebug .ne. 0) WRITE(IDEBUG,*) ' Open water'
               CALL CmpOW  (ITMSTP, IKNDNR, IMETEO, IKNOOP, iDebug)
               IF (ITER .GT. 2) THEN
                 iOW = iKndNr
                 IF (VOLOW (IOW) .LT. -0.01 .OR. AROW(IOW) .LT. -0.01) Then
                    If (.not. NegativeVolumeInCurrentTimestep) THEN
                        NegativeVolumeInCurrentTimestep = .true.
                        NrTimestepsNegativeVolume = NrTimestepsNegativeVolume+1
                    Endif
                    if (IOUT1 .ne. 0 .and. NrTimestepsNegativeVolume .lt. 10) then
                      NodeName = Id_Nod(IKNOOP)
                      Len = Len_trim (NodeName)
                      WRITE(Iout1,'(A,A,A,2I5)')   &
                       ' Open water ', NodeName(1:len),   &
                       ' negative volume or area in event/timestep',IEVENT,ITMSTP
                    endif
                 ENDIF
               ENDIF
!              call ConfFil_set_IDEBUG(0)
            case (5,8,9,10,11,12,13)
!              Structure
!              if (itmstp .eq. 161)  call ConfFil_set_IDEBUG(IdebugLunRR)
               IF (iDebug .ne. 0) WRITE(IDEBUG,*) ' Structure '
               CALL CmpSTR (ITMSTP, IKNDNR, IKNOOP, IDAYWK)
!              call ConfFil_set_IDEBUG(0)
            case (6)
!              Boundary; only action in routine is to write debug output, so do not call it if debug is off
               IF (iDebug .ne. 0) then
                  WRITE(IDEBUG,*) ' Boundary  '
                  CALL CmpBND (ITMSTP, IKNDNR, iKnoop)
               Endif
            case (7)
!              Pluvius - NWRW model
               IPLV2 = INDIKP(IKNDNR)
               IF (.NOT. COMPPV(IPLV2)) THEN
                 CALL CmpPLV (ITMSTP, IKNDNR, IMETEO, IPLV2, IKNOOP)
                 IF (iDebug .ne. 0) WRITE(IDEBUG,*) ' Pluvius  '
                 COMPPV(IPLV2) = .TRUE.
               ENDIF
               If (NrSpecialNwrwAreas(ikndnr) .gt. 0) then
                  Do j=1,NrSpecialNwrwAreas(ikndnr)
                     Iplv3 = SpecialInDikP(Ikndnr,j)
                     Iplv4 = Reference2SpecialDef(Ikndnr,j)
                     IF (.NOT. SpecialComppV(IPLV3)) THEN
                       CALL SpecialCmpPLV (ITMSTP, IKNDNR, IMETEO, IPLV3, Iplv4, IKNOOP)
                       IF (iDebug .ne. 0) Write(IDEBUG,*) ' Pluvius  Special'
                       SpecialComppv(IPLV3) = .true.
                     ENDIF
                  Enddo
               Endif
            case (14)
!              RWZI knoop
               IF (iDebug .ne. 0) WRITE(IDEBUG,*) ' WWTP node'
               CALL CmpRwzi (ITMSTP, IKNDNR, IKNOOP)
            case (15)
!              Industrie knoop
               IF (iDebug .ne. 0) WRITE(IDEBUG,*) ' Industry node'
               CALL CmpInd (ITMSTP, IKNDNR,  IKNOOP)
            case (16)
!              Sacramento knoop
               IF (iDebug .ne. 0) WRITE(IDEBUG,*) ' Sacramento node'
               CALL CmpSacramento (ITMSTP, IKNDNR, IMETEO, IKNOOP,ILink)
               if (NrDownstreamRoutingLinks(iknoop) .eq. 1) then
                   Call RRRoutingLink(iknoop, ilink, itmstp)
               endif
            case (17)
!              Cel
               IF (iDebug .ne. 0) WRITE(IDEBUG,*) ' Cel node'
!              CALL CmpCel (ITMSTP, IKndNr, IMeteo, IKNOOP)
            case (18,19,20,22,23,31)
!              RRRunoff Node
               IF (iDebug .ne. 0) WRITE(IDEBUG,*) ' RR Runoff node'
               CALL CmpRRRunoffNode (ITMSTP, IKndNr, IRunoff, IMeteo, IEvap, iTemperature, IKNOOP, ILink, GlobalNAMAlfa)
               if (NrDownstreamRoutingLinks(iknoop) .eq. 1) then
                   Call RRRoutingLink(iknoop, ilink, itmstp)
               endif
            case (21)
               IF (iDebug .ne. 0) WRITE(IDEBUG,*) ' Open water Precip/Evap only'
               CALL CmpOWRain  (ITMSTP, IKNDNR, IMETEO, IKNOOP, iDebug)
            case (30)
!              RR-Connection knoop
               IF (iDebug .ne. 0) WRITE(IDEBUG,*) ' Rr-Connection node'
               CALL CmpRRConnection (ITMSTP, IKNDNR, IKNOOP, ilink)
               if (NrDownstreamRoutingLinks(iknoop) .eq. 1)  then
                  Call RRRoutingLink(iknoop, ilink, itmstp)
               endif
            case (32)
!              RR-Bifurcation knoop
               IF (iDebug .ne. 0) WRITE(IDEBUG,*) ' RR-Bifurcation node'
               CALL CmpRRBifurcation(ITMSTP, IKNDNR, IKNOOP, ilink)
               if (NrDownstreamRoutingLinks(iknoop) .eq. 1)  then
                  Call RRRoutingLink(iknoop, ilink, itmstp)
               endif
            case default
               call SetMessage(LEVEL_FATAL, 'Error in sub nodelp of Sobek_3B: Unknown kind of node')
            end select
          Endif
       ENDDO
!
!
      RETURN
      END
