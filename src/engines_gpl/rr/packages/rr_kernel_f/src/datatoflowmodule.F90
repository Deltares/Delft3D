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

 
module DataToFlowModule

 ! Module for output of data to Flow module
 ! - File Runoff.Out for off-line or on-line coupling (boundary nodes and/or NWRW nodes)
 ! - Files with boundary flows and salt concentrations (Rijnland)


  !use
  use Conf_fil
  use Conf_Arr
  use Network
  use Boundary
  use NWRW
  use Output
! DIO
!!  use Dio_Plt_Rw
  use DH_Alloc
  use ReadLib

  implicit none
 ! variables
  type(DioPltType) :: Runoff_Out



contains

      Subroutine WRHDR (IOUTPL, IOUTCB)

! *********************************************************************
! ***                D E L F T         H Y D R A U L I C S
! ***
! ***              WATER RESOURCES AND ENVIRONMENT DIVISION
! *********************************************************************
! *** Program :  DELFT_3B version 1.2.                 Date: March 1996
! *********************************************************************
! *** Last update: March  1996       By : Geert Prinsen
! *********************************************************************
! *** Brief description:
! *** ------------------
! ***   Write headers to Pluvius / Boundaries ASCII file, if desired
! *********************************************************************
! *** Input/output parameters:
! *** ------------------------
! ***  IDEBUG = file unit number of debug file
! ***  IOUTPL = file unit number of Pluvius output file
! ***  IOUTCB = file unit number of Concentrations to Boundary output file
! *********************************************************************

      IMPLICIT NONE

      INTEGER       IOUTPL, IOUTCB, i, teller, iDebug
      CHARACTER(1)  QUOTE

      Logical       Success

      Character(Len=CharIdLength), Pointer, SAVE :: tmpIDNode1(:), TmpIDNode2(:)

      success = DH_AllocInit (Nnod, tmpIDNode1, TmpIDNode2, ' ')
      If (.not. Success) call ErrMsgStandard (981, 0, ' Error allocating arrays in subroutine ', &
                                           ' DataToFlowModule_WrHdr'  )

!      Allocate( tmpIDNode1(nNod), Stat=Allocation_Error )
!      Allocate( tmpIDNode2(nNod), Stat=Allocation_Error )

! *********************************************************************
! ***   Write headers to Pluvius / Boundaries ASCII file, if desired
! *********************************************************************


    QUOTE = ''''

    iDebug = ConfFil_get_iDebug()

! Runoff.Out file with Pluvius/NWRW and Boundary flows
      IF (IDEBUG /= 0) WRITE (IDEBUG,*) ' Open file',ConfFil_get_NAMFIL(37)
      Call OpenFl (IoutPl,ConfFil_get_NAMFIL(37),1,2)
      WRITE(IOUTPL,*) NCPLUV+NCBOUN
      Do teller = 1, ncPluv
         tmpIDNode1(teller) = Id_Nod(PLVNAM(teller))
      Enddo
      Do teller = 1, ncBoun
         tmpIDNode2(teller) = Id_Nod(BNDNAM(teller))
      Enddo
      WRITE(IOUTPL,'(10(A,A,A,1X))')   &
         (QUOTE,tmpIDNode1(i)(1:Len_Trim(tmpIDNode1(i))),QUOTE, I=1,NCPLUV),    &
         (QUOTE,tmpIDNode2(i)(1:Len_Trim(tmpIDNode2(i))),QUOTE, I=1,NCBOUN)

! output concentrations on boundaries
      IF (NCBOUN .GT. 0) Call WrHdrCBFile (IoutCb)

    DeAllocate ( tmpIDNode1 )
    DeAllocate ( tmpIDNode2 )

    RETURN

   End subroutine WrHdr



      Subroutine WrHdrCbFile (IOUTCB)

! *********************************************************************
! ***                D E L F T         H Y D R A U L I C S
! ***
! ***              WATER RESOURCES AND ENVIRONMENT DIVISION
! *********************************************************************
! *** Program :  DELFT_3B version 1.2.                 Date: March 1996
! *********************************************************************
! *** Last update: March  1996       By : Geert Prinsen
! *********************************************************************
! *** Brief description:
! *** ------------------
! ***   Write headers to Pluvius / Boundaries ASCII file, if desired
! *********************************************************************
! *** Input/output parameters:
! *** ------------------------
! ***  IOUTCB = file unit number of Concentrations to Boundary output file
! *********************************************************************

      IMPLICIT NONE

      INTEGER       IOUTCB, i, iDebug
      CHARACTER(1)  QUOTE
      Character(CharIdLength) nodeID

! *********************************************************************
! ***   Write headers to Concentration Boundary file
! *********************************************************************

    QUOTE = ''''

    iDebug = ConfFil_get_iDebug()

! output concentraties op randen
      IF (NCBOUN .GT. 0) THEN
        IF (IDEBUG /= 0) WRITE (IDEBUG,*) ' Open file',ConfFil_get_NAMFIL(46)
        Call OpenFl (IoutCb,ConfFil_get_NAMFIL(46),1,2)
        WRITE(IOUTCB,*) NCBOUN
        Do i = 1, ncBoun
           nodeID = Id_Nod(BNDNAM(I))
           WRITE(IOUTCB,'(10(A,A,A,1X))') QUOTE, nodeID(1:Len_Trim(nodeID)),QUOTE
        Enddo
! sluit file als uitvoer niet gewenst is
        IF (IQCBND .EQ. 0) Call CloseGP (IOUTCB)
      ENDIF

    RETURN
   End subroutine WrHdrCBFile



   Subroutine WrHdrRunoffOut (LastTm, Ievent, IOutPl, cMAA, cDAG, cUUR, cMIN, cSEC, HeaderRunoffOutAlways, &
                              NwrwContinuous, EmulateUnixOnPC, &
                              IOutPeriod, NrOutputPeriods, OutputEventStartDateTime, OutputEventDuration)

! *********************************************************************
! ***                D E L F T         H Y D R A U L I C S
! ***
! ***              WATER RESOURCES AND ENVIRONMENT DIVISION
! *********************************************************************
! *** Program :  DELFT_3B version 1.2.                 Date: March 1996
! *********************************************************************
! *** Last update: March  1996       By : Geert Prinsen
! *********************************************************************
! *** Brief description:
! *** ------------------
! ***   Write headers to Pluvius / Boundaries ASCII file, if desired
! *********************************************************************
! *** Input/output parameters:
! *** ------------------------
! ***  IDEBUG = file unit number of debug file
! ***  IOUTPL = file unit number of Pluvius/NWRW and Boundary Flows output file (Runoff.Out)
! ***  IOUTCB = file unit number of Concentrations to Boundary output file
! ***  IOUTDY = file with daily totals at boundary nodes
! *********************************************************************

  Integer  LastTm, IEvent, IOutPl, Idebug
  Real     RNrSecs
  Integer  nrsEvn, nDyEv, nHrEv, nMnEv, i
  Integer  iSec2, iMin2, iHour2, iDay2, iMo2, iYear2
  Logical  HeaderRunoffOutAlways, NwrwContinuous, EmulateUnixOnPC
  Integer  NrOutputPeriods, IoutPeriod
  Integer  OutputEventStartDateTime (NrOutputPeriods,7), OutputEventDuration(NrOutputPeriods,6)

  Integer       teller, TmSize, IYear, Imo, Iday, Ihour, IMin, ISec
  Character*160 String
! DIO
  Character(Len=DioMaxStreamLen)  :: outName   ! name of out dataset
  character(len=HisRunIdSize), dimension(HisRunIdDim) :: runId         ! (HIS) runid
  Character(Len=DioMaxParLen), dimension(1)           :: parNames = &  ! variable(s) in dataset
                                                         'Flow (m3/s)'
  Character(Len=DioMaxLocLen), pointer, dimension(:)  :: LocIds      ! locations(s) in dataset
  Character(Len=DioMaxLocLen), pointer, dimension(:)  :: LocDescr      ! locations(s) in dataset
  Logical, save                                       :: doHeaderRunoffOut = .True.   ! Process Header next time?
! End DIO

  Integer      LenDwa, LenRwa, LHelp
  Real         RHelp
  CHARACTER(2) cMAA, cDAG, cUUR, cMIN, cSEC, cMAA2, cDAG2, cUUR2, cMIN2, cSEC2
  Logical      success


    If (RunoffOutHis .eq. 0) then
       Idebug = ConfFil_get_iDebug()
       IF ( NCPLUV .GT. 0 .OR. NcBoun .gt. 0) THEN
          LHelp = Lasttm
          if (NwrwContinuous) then
             RHelp = Float(OutputEventDuration(IOutPeriod,1))*NRSDAY + &
                          Float(OutputEventDuration(IoutPeriod,2))*NRSHR + &
                             Float(OutputEventDuration(IoutPeriod,3))*NRSMIN + &
                               Float(OutputEventDuration(IoutPeriod,4))
             LHelp  = RHelp / timeSettings%timestepSize
          Endif
          RNrSecs = Float(LHelp) * timeSettings%timestepSize
!          IF (iDebug /=  0)  WRITE(IDEBUG ,*) ' RNrSecs',RNrSecs
          NDYEV  = RNrSecs / NRSDAY
          RNrSecs = RNrSecs - Float(NDYEV) * NRSDAY
!          IF (iDebug /=  0) WRITE(IDEBUG(),*) ' NRSEVN',NRSEVN, NDYEV
          NHREV  = RNrSecs / NRSHR
          RNrSecs = RNrSecs - Float(NHREV) * NRSHR
!          IF (iDebug /=  0)  WRITE(IDEBUG,*) ' NRSEVN',NRSEVN, NDYEV, NHREV
          NMNEV  = RNrSecs / NRSMIN
          RNrSecs = RNrSecs - Float(NMNEV) * NRSMIN
          IF (RNrSecs .GE. NRSMIN) THEN
              RNrSecs = RNrSecs - NRSMIN
              NMNEV  = NMNEV + 1
          ENDIF
          NRSEvn = RNrSecs
!          IF (iDebug /=  0)  WRITE(IDEBUG, *) ' NRSEVN',NRSEVN, NDYEV, NHREV, NMNEV
          ISEC2  = ConfArr_get_iSecond()  + NRSEVN
          IMIN2  = ConfArr_get_iMinute()  + NMNEV
          IHOUR2 = ConfArr_get_IHOUR() + NHREV
          IDAY2  = ConfArr_get_IDAY()
          IMO2   = ConfArr_get_iMonth()
          call ConfArr_set_iYear(EVSTRT(1))
          if (NwrwContinuous) call ConfArr_set_iYear(OutputEventStartDateTime(IoutPeriod,1))
          IYEAR2 = ConfArr_get_IYEAR()
          IF (ISEC2 .GE. NRSMIN) THEN
             ISEC2 = ISEC2 - NRSMIN
             IMIN2 = IMIN2 + 1
          ENDIF
          IF (IMIN2 .GE. NRSMIN) THEN
             IMIN2  = IMIN2 - NRSMIN
             IHOUR2 = IHOUR2 + 1
          ENDIF
          IF (IHOUR2 .GE. 24) THEN
             IHOUR2 = IHOUR2 - 24
             NDYEV  = NDYEV  + 1
          ENDIF
          DO I=1,NDYEV
              CALL NXTDAY (IDEBUG, IYEAR2, IMO2, IDAY2)
          ENDDO

          IYear = ConfArr_get_IYear()
          if (.not. TimeSettings%Output2CFUserDefinedPeriod) then
             cMAA = INTCH2(ConfArr_get_iMonth())
             cDAG = INTCH2(ConfArr_get_IDAY())
             cUUR = INTCH2(ConfArr_get_IHOUR())
             cMIN = INTCH2(ConfArr_get_iMinute())
             cSEC = INTCH2(ConfArr_get_iSecond())

             cMAA2 = INTCH2(IMO2)
             cDAG2 = INTCH2(IDAY2)
             cUUR2 = INTCH2(IHOUR2)
             cMIN2 = INTCH2(IMIN2)
             cSEC2 = INTCH2(ISEC2)
          Else
             IYear = TimeSettings%OutputStartYear
             cMAA = INTCH2(TimeSettings%OutputStartMonth)
             cDAG = INTCH2(TimeSettings%OutputStartDay)
             cUUR = INTCH2(TimeSettings%OutputStartHour)
             cMIN = INTCH2(TimeSettings%OutputStartMinute)
             cSEC = INTCH2(TimeSettings%OutputStartSecond)

             cMAA2 = INTCH2(TimeSettings%OutputEndMonth)
             cDAG2 = INTCH2(TimeSettings%OutputEndDay)
             cUUR2 = INTCH2(TimeSettings%OutputEndHour)
             cMIN2 = INTCH2(TimeSettings%OutputEndMinute)
             cSEC2 = INTCH2(TimeSettings%OutputEndSecond)
          Endif

          if (ncnode /= 0) then
             if (NwrwContinuous) then
               WRITE(IOutPl, 192) IOutPeriod, NrOutputPeriods, &
                                IYear, cMAA, cDAG, cUUR, cMIN, cSEC, &
                                IYEAR2, cMAA2, cDAG2, cUUR2, cMIN2, cSEC2
             elseif (HeaderRunoffOutAlways .or. TimeSettings%Output2CFUserDefinedPeriod) then
! altijd de header aangeven met aantal events=1 voor Nevent keer Sobeksim starten
! Mogelijk moet dit aangepast worden ivm gebruik PARSEN met DLG functionaliteit
               WRITE(IOutPl, 192) 1, 1, &
                                IYear, cMAA, cDAG, cUUR, cMIN, cSEC, &
                                IYEAR2, cMAA2, cDAG2, cUUR2, cMIN2, cSEC
             else
               WRITE(IOutPl, 192) IEVENT, NEVENT, &
                                IYear, cMAA, cDAG, cUUR, cMIN, cSEC, &
                                IYEAR2, cMAA2, cDAG2, cUUR2, cMIN2, cSEC2
             endif
          else
             WRITE(IOutPl, '(I4)') ncnode
          end if
       ENDIF
  192  FORMAT('"REEKS" ',I4,1X,I4,1X, &
              '"',I4,'/',A2,'/',A2,';',2(A2,':'),A2,'" ', &
              '"',I4,'/',A2,'/',A2,';',2(A2,':'),A2,'" ')
!end sub-header Pluvius output

    Else
      ! DIO
      if (DoHeaderRunoffOut .or. HeaderRunoffOutAlways) then
         if (SeparateRwa_Dwa .eq. 0) then
              success = DH_AllocInit (NcPluv+NcBoun, LocIds, ' ')
              success = success .and. DH_AllocInit (NcPluv+NcBoun, LocDescr, ' ')
         else
              success = DH_AllocInit (2*NcPluv+NcBoun, LocIds, ' ')
              success = success .and. DH_AllocInit (2*NcPluv+NcBoun, LocDescr, ' ')
         endif
         If (.not. success) call ErrMsgStandard (981, 0, ' Error allocating arrays in subroutine ', &
                                                 ' WrHdrRunoffOut '  )
         runId  = ' '
         runId(1) = CASENM(1:)
         runId(3) = 'TITLE: Flows transferred from RR to CFSF'
         TmSize = TimeSettings%TimestepSize
         IYEAR = EventStartDateTime(1, 1)
         IMO   = EventStartDateTime(1, 2)
         IDAY  = EventStartDateTime(1, 3)
         IHOUR = EventStartDateTime(1, 4)
         IMIN  = EventStartDateTime(1, 5)
         ISEC  = EventStartDateTime(1, 6)
         Call WriteT0String (String, IYear, Imo, Iday, Ihour, Imin, Isec, TmSize)
         RunId(4) = String(121:)
         If (SeparateRwa_Dwa .eq. 0) then
             Do teller = 1, ncPluv
                LocIds(teller) = Id_Nod(PLVNAM(teller))
                LocDescr(teller) = NamNod(PLVNAM(teller))
             Enddo
             Do teller = 1, ncBoun
                LocIds(teller+NcPluv) = Id_Nod(BNDNAM(teller))
                LocDescr(teller+NcPluv) = NamNod(BNDNAM(teller))
             Enddo
         Else
             LenDwa = Len_Trim(Dwastring)
             LenRwa = Len_Trim(Rwastring)
             Do teller = 1, ncPluv
                LocIds(teller) = Id_Nod(PLVNAM(teller))
                LocIds(teller) = LocIds(teller)(1:Len_Trim(LocIds(teller))) // RwaString(1:LenRwa)
                LocIds(NcPluv+teller) = Id_Nod(PLVNAM(teller))
                LocIds(NcPluv+teller) = LocIds(NcPluv+teller)(1:Len_Trim(LocIds(NcPluv+teller))) // DwaString(1:LenDwa)
                LocDescr(teller) = NamNod(PLVNAM(teller))
                LocDescr(teller) = LocDescr(teller)(1:Len_Trim(LocDescr(teller))) // RwaString(1:LenRwa)
                LocDescr(NcPluv+teller) = NamNod(PLVNAM(teller))
                LocDescr(NcPluv+teller) = LocDescr(NcPluv+teller)(1:Len_Trim(LocDescr(NcPluv+teller))) // DwaString(1:LenDwa)
             Enddo
             Do teller = 1, ncBoun
                LocIds(teller+NcPluv+NcPluv) = Id_Nod(BNDNAM(teller))
                LocDescr(teller+NcPluv+NcPluv) = NamNod(BNDNAM(teller))
             Enddo
         Endif
         OutName = ConfFil_get_NamFIL(37)
         Runoff_Out = DioPltDefine(outName, runId, Dio_Plt_Real, parNames, LocIds)
         Call DioPltAddDescriptions (Runoff_Out, dio_plt_locs, LocDescr)
         deallocate(LocIds)
         deallocate(LocDescr)

! aanpassing April 2003
#if (defined(SOBEK_PARALLEL))
         doHeaderRunoffOut = .true.
#else
         doHeaderRunoffOut = .false.
         if (EmulateUnixOnPC) doHeaderRunoffOut = .true.
#endif
! nav testen RR-Unix en EmulateUnixOnPc
         if (NwrwContinuous .or. NEvent .eq. 1) doHeaderRunoffOut = .false.

!        if ( DioPltGetStreamType(Runoff_Out) .eq. Dio_SharedMem_stream ) then
!            doHeaderRunoffOut = .false.
!        else
!            call DioPltDestroy (Runoff_Out)
!        endif
      Endif

    Endif

   Return
   End subroutine WrHdrRunoffOut



   Subroutine WrDataRunoffOut (IOutPl, cMAA, cDAG, cUUR, cMIN, cSEC, Itmstp)

! ***     Write ASCII file Pluvius rioolinloop per Pluvius knoop and Boundary node
!
  Integer      IOutPl, Itmstp
  CHARACTER(2) cMAA, cDAG, cUUR, cMIN, cSEC

    Integer  Iplv, teller, DioIndex

    Real, Pointer :: DioResult (:,:)
    Logical  success
    Integer  loc


       If (RunoffOutHis .eq. 0) then
          IF (NCPLUV .GT. 0) THEN
!            Pluvius/NWRW nodes
             WRITE(IOUTPL,191) ConfArr_get_IYEAR(), cMAA, cDAG, cUUR, cMIN, cSEC
             IF (outputUnits%nwrwFlow ==  1) THEN
               WRITE(IOUTPL,1911) (RSLMAP7_plv(1,IPLV,1),IPLV=1, NCPLUV)
             ELSEIF (outputUnits%nwrwFlow ==  2) THEN
               WRITE(IOUTPL,1911) (RSLMAP7_plv(1,IPLV,1)/timeSettings%timestepSize, IPLV=1,NCPLUV)
             ELSEIF (outputUnits%nwrwFlow ==  5) THEN
              WRITE(IOUTPL,*) ' The unit m3/ha is not yet implemented'
             ENDIF
!         Add boundary nodes
             IF (NCBOUN .GT. 0) THEN
                IF (outputUnits%boundaryFlow == 2) THEN
                  WRITE(IOUTPL,1911) (RSLMAP6_bnd(1,teller,1)/timeSettings%timestepSize,teller = 1,NCBOUN)
                ELSE
                  WRITE(IOUTPL,1911) (RSLMAP6_bnd(1,teller,1),teller = 1,NCBOUN)
                ENDIF
!               Formats
  191           FORMAT('"',I4,'/',A2,'/',A2,';',2(A2,':'),A2,'" ',9999(E11.4,1X))
 1911           FORMAT(10(E11.4,1X))
             ENDIF
          ENDIF
       Else
        ! Use DIO
          if (SeparateRwa_Dwa .eq. 0) then
              success = DH_AllocInit (1, NcPluv+NcBoun, DioResult, 0E0)
!              Allocate  ( DioResult(1, NcPluv+NcBoun), Stat=Allocation_Error )
          else
              success = DH_AllocInit (1, NcPluv+NCPluv+NcBoun, DioResult, 0E0)
!              Allocate  ( DioResult(1, NcPluv+NcPluv+NcBoun), Stat=Allocation_Error )
          endif
          If (.not. success) call ErrMsgStandard (981, 0, ' Error allocating arrays in subroutine ', &
                                                  ' WrDataRunoffOut '  )
          Do loc=1,NcPluv
             If (SeparateRwa_Dwa .ne. 0) then
                DioResult(1,loc) = RslMap7_plv(6,loc,1)
                IF (outputUnits%nwrwFlow == 2) DioResult(1,loc) = DioResult(1,loc) / timeSettings%timestepSize
                DioIndex = loc + NcPluv
                DioResult(1,DioIndex) = RslMap7_plv(7,loc,1)
                IF (outputUnits%nwrwFlow == 2) DioResult(1,DioIndex) = DioResult(1,DioIndex) / timeSettings%timestepSize
             Else
                DioResult(1,loc) = RslMap7_plv(1,loc,1)
                IF (outputUnits%nwrwFlow == 2) DioResult(1,loc) = DioResult(1,loc) / timeSettings%timestepSize
             Endif
          Enddo
          Do loc=1,NcBoun
             DioIndex = NcPluv + loc
             If (SeparateRwa_Dwa .ne. 0) DioIndex = DioIndex + NcPluv
             DioResult(1,DioIndex) = RslMap6_bnd(1,loc,1)
             IF (outputUnits%boundaryFlow == 2) DioResult(1,DioIndex) = DioResult(1,DioIndex) / timeSettings%timestepSize
          Enddo
          Call DioPltPut (Runoff_Out, itmstp, DioResult)
          Deallocate(DioResult)

       Endif

      Return
   End subroutine WrDataRunoffOut




   Subroutine WrDataBoundaries (IOutPl, IOutCb, cMAA, cDAG, cUUR, cMIN, cSEC, Itmstp)

! ***     Write ASCII file Q en C op randen indien gewenst

  Integer  IOutPl, IOutCb, Itmstp
  CHARACTER(2) cMAA, cDAG, cUUR, cMIN, cSEC

  Integer teller

    Real, Pointer :: DioResult (:,:)
    Logical  success
    Integer  loc


          IF (NCBOUN .GT. 0 .AND. IQCBND .EQ. 1) THEN
! Pluvius format zo mogelijk gecombineerd met Pluvius knopen;
! dus alleen hier schrijven als nog niet via NWRW/Pluvius nodes WrDataRunoffOut geschreven
             IF (NCPLUV .LE. 0) THEN
               If (RunoffOutHis .eq. 0) then
                 ! Runoff.Out old format
                 WRITE(IOutPl,191) ConfArr_get_IYEAR(), cMAA, cDAG, cUUR, cMIN, cSEC
                 IF (outputUnits%boundaryFlow ==  2) THEN
                    WRITE(IOutPl,1911) (RSLMAP6_bnd(1,teller,1)/timeSettings%timestepSize,teller=1,NCBOUN)
                 ELSE
                    WRITE(IOutPl,1911) (RSLMAP6_bnd(1,teller,1),teller=1,NCBOUN)
                 ENDIF
               Else
                 ! Use Dio
                  success = DH_AllocInit (1, NcBoun, DioResult, 0E0)
!                  Allocate  ( DioResult(1, NcBoun), Stat=Allocation_Error )
                  If (.not. success) call ErrMsgStandard (981, 0, ' Error allocating arrays in subroutine ', &
                                                          ' WrDataBoundaries'  )
                  Do loc=1,NcBoun
                     DioResult(1,loc) = RslMap6_bnd(1,loc,1)
                  Enddo
                  Call DioPltPut (Runoff_Out, itmstp, DioResult)
                  Deallocate(DioResult)
               Endif
             ENDIF

             IF (ISLCMP /= 0) THEN
                 If (OutputDesired(9) ) then
                     WRITE(IOUTCB,191) ConfArr_get_IYEAR(), cMAA, cDAG, cUUR, cMIN, cSEC,(CBND(teller),teller=1,NCBOUN)
                 Endif
             ENDIF
          ENDIF
  191     FORMAT('"',I4,'/',A2,'/',A2,';',2(A2,':'),A2,'" ',9999(E11.4,1X))
 1911     FORMAT(10(E11.4,1X))


      Return
   End subroutine WrDataBoundaries



End Module DataToFlowModule
