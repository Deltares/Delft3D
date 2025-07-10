!----- AGPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2025.                                
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
! at:               $Modtime:: 15-08-97 11:31a  $
!
! current revision: $Revision:: 4               $


      SUBROUTINE RR_INIT1 (IEVENT, Idebug, Iout1, ICache, FirstCallRRInitializeEvent, DefaultT0Value, GlobalNAMAlfa, ReadLGSICacheFile)

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
! ***   Initialisatie per event
! *********************************************************************
! *** Input/output parameters:
! *** ------------------------
! ***  IEVENT = event number
! ***  IDEBUG = debug file unit
! ***  IMO    = month
! ***  IDAY   = day
! *********************************************************************

!   USE CONF_FIL
    use Network
    USE CONF_ARR
    use Unpaved
    use Openwater
    use Boundary
    use NWRW
    use Paved
    use RWZI
    use Industry
    use RRRunoff
    use Sacramento
    use Salts
    use Structures
    use NewTables
    use Greenhouse
    use RRConnectionBifurcationNodes
    use Output

  Implicit none

! variables
  INTEGER IMAAND, IDAG, hour, minute, iEvent, iVhg, iOvh, iKas, IRwzi, IIndus, ISacr
  Integer iOw, iStr, Iplv, teller
  Integer iKKl, iMap, iMsr, iMsr2, iLoc, nod_type, iNode

  Integer iDebug, Iout1, i, index, index0, indx
  Integer Idoor
  Integer ICache
  Logical FirstCall, FirstCallRRInitializeEvent, ReadLGSICacheFile
  Real    DefaultT0Value, GlobalNAMAlfa

!
      IF (iDebug /= 0) WRITE (IDEBUG,1)
    1 FORMAT (' INIT1')
!
! *********************************************************************
! *** Initialiseer
! ***   - initiele berging verhard gebied  op straat en in riool
! ***   - initiele berging op daken kassen en in silo's -> 0.0
! ***      (werkelijke initiele berging op daken kassen via sub RDKINI)
! ***   - initieel peil open water; aanname: = streefpeil
! ***   - initiele status structure: uit (=0)
! ***   - initele status boundary-knopen
! ***   - initiele status pluvius-knopen
! *********************************************************************
!
        IMAAND = ConfArr_get_iMonth()
        IDAG   = ConfArr_get_iDay()
        hour   = ConfArr_get_iHour()
        minute = ConfArr_get_iMinute()

        if (idebug .ne. 0)  WRITE(IDEBUG,*) ' -month-day-hour', imaand, idag, hour, minute

!Verhard gebied
        Call Init1Paved

! Kasgebied
        Call Init1Kas

! open water
        Call Init1OpenWater(Ievent, Idebug, Iout1)

!Boundary; moet voor initialisatie onverhard gebied
        Call Init1Boundary

!Onverhard gebied
        CALL Unpaved_INITBC (IMAAND, IDAG)
        Call Init1Unpaved (Idebug, Ievent, imaand, .false.)

!Structures
        Call Init1Structures

!NWRW inloopknopen
        Call Init1NWRW (Idebug)

!RWZI no actions

!Industry no actions

!Sacramento altijd zonder restart file
        IDoor = 0
        FirstCall = .true.
        If (IEvent .eq. 1) FirstCall = FirstCallRRInitializeEvent
        Call Sacramento_Init1 (IEvent, Iout1, idoor, FirstCall)

!RR Connection nodes / Bifurcation nodes
        Call Init1RRConnection
        Call Init1RRBifurcation

!Cel
!       CALL Cel_INITBC (IMAAND, IDAG)
!       Call Init1Cel (Idebug, IEvent, imaand, .false.)
!RRRunoff
        FirstCall = .true.
        Call RRRunoffNode_Init1 (IEVENT, Iout1, ICache, GlobalNAMAlfa, FirstCall, ReadLGSICacheFile)

! *********************************************************************
! *** Initialiseer summary results for each event
! *********************************************************************

        DO IVHG = 1,NCVHG
           VHMBPC(IVHG,1,Ievent) = -999.99
           VHMBPC(IVHG,2,Ievent) = -999.99
           DO I=1,NMSR
              VHMQOU(IVHG,I,Ievent) =     0.
           ENDDO
        ENDDO

        DO IOVH = 1,NCOVHG
           OvMxPercInund(iovh,Ievent) = 0.
           OVMGWS(IOVH,Ievent  ) = -999.99
           OVMGWV(IOVH,Ievent  ) = 0.0
           OVMONV(IOVH,Ievent,1) = -999.99
           OVMONV(IOVH,Ievent,2) = 0.0
           OVMONL(IOVH,Ievent,1) = -999.99
           OVMONL(IOVH,Ievent,2) = 0.0
           ! added Mar 2003
           OVMBGC(IOVH,Ievent  ) = 0.0
           DO I=1,10
              OVMQOU(IOVH,I,Ievent) = -999.99
           ENDDO
           GWEXC (IOVH,1,Ievent) = 0
           GWEXC (IOVH,2,Ievent) = 0
           GWEXC (IOVH,3,Ievent) = 0
           GWEXC (IOVH,4,Ievent) = 0
        ENDDO

        DO IKAS = 1,NCKAS
         DO IKKL = 1,NCKKL+1
           KSMBPC(IKAS,IKKL,Ievent) =     0.
           Do I=1,4
             KSMQOU(IKAS,IKKL,Ievent,i) =     0.
           Enddo
         ENDDO
        ENDDO

        DO IOW  = 1,NCOW
           Do I=1,8  ! Mar 2003; was 5
             OWMLVL(IOW,Ievent,i) = -999.99
           Enddo
           OWEXC (IOW,1,Ievent) = 0
           OWEXC (IOW,2,Ievent) = 0
        ENDDO

        DO ISTR = 1,NCSTRU
           QSTRMX(ISTR,Ievent) = -999.99
           QSTRMX1(ISTR,Ievent) = -999.99
           QSTRMX2(ISTR,Ievent) = -999.99
           ActCrestLevelMx(ISTR,Ievent) = -999.99
        ENDDO

        DO teller = 1,NCBOUN
           QBNDMX(teller,Ievent,1) = -999.99
           QBNDMX(teller,Ievent,2) = -999.99
        ENDDO

        DO IPLV = 1,NCPLUV
           PLVBPC(IPLV,1,Ievent) = -999.99
           PLVBPC(IPLV,2,Ievent) = -999.99
           PLVBPC(IPLV,3,Ievent) = -999.99
        ENDDO

        DO IPLV = 1,NCPLUV
           PLVQOU(IPLV,1,Ievent) =     0.
           PLVQOU(IPLV,2,Ievent) =     0.
           PLVQOU(IPLV,3,Ievent) =     0.
           PLVQOU(IPLV,4,Ievent) =     0.
           PLVQOU(IPLV,5,Ievent) =     0.
           PLVQOU(IPLV,6,Ievent) =     0.
           PLVQOU(IPLV,7,Ievent) =     0.
        ENDDO

        DO Irwzi = 1,NCRwzi
           QRWZIMX(IRwzi,Ievent) = 0.
           QEffRWZIMX(IRwzi,Ievent) = 0.
        ENDDO

        DO IIndus = 1,NCIndus
           QDisMx   (IIndus,Ievent) = 0.
           QDemMx   (IIndus,Ievent) = 0.
           SltIndMx (IIndus,Ievent) = 0.
           ! added March 2003
           QShortMx (IIndus,Ievent) = 0.
           QAllMx   (IIndus,Ievent) = 0.
        ENDDO

! Sacramento: added March 2003
        DO ISacr = 1,NCSacr
           SacMxUZTWC(ISacr,Ievent) = 0.
           SacMxUZFWC(ISacr,Ievent) = 0.
           SacMxLZTWC(ISacr,Ievent) = 0.
           SacMxLZFSC(ISacr,Ievent) = 0.
           SacMxLZFPC(ISacr,Ievent) = 0.
           SacMxPrecip(ISacr,Ievent) = 0.
           SacMxPotEvp(ISacr,Ievent) = 0.
           SacMxActEvp(ISacr,Ievent) = 0.
           SacMxBasFlw(ISacr,Ievent) = 0.
           SacMxSurFlw(ISacr,Ievent) = 0.
           SacMxImpFlw(ISacr,Ievent) = 0.
           SacMxTotRun(ISacr,Ievent) = 0.
           SacMxChaInf(ISacr,Ievent) = 0.
           SacMxLosFlw(ISacr,Ievent) = 0.
           SacMxAdimC (ISacr,Ievent) = 0.
        ENDDO


        DO teller = 1,NCLink
           QLinkMX(teller,Ievent) = -999.99
        ENDDO

        IF (ISLCMP /= 0) THEN
!         DO INOD=1,NCNODE
!            SLTMXC(INOD) = 0.0
!         ENDDO
!         Vector/Array initialisation
          SLTMXC = 0.0
! Jan 1998: salt concentration initialisation added (otherwise not ok for series computations)
          DO INODE = 1, NCNODE
             nod_type = EINODE(INODE,3)
             INDEX0=INDSLT(INODE)
             INDEX = INDEX0
             IF (nod_type .EQ. 1) THEN
               INDEX = INDEX + 3  ! with 2 sewer storages now +3 instead of +2
             ELSEIF (nod_type .EQ. 2) THEN
               INDEX = INDEX + 2
             ELSEIF (nod_type .EQ. 3) THEN
               INDEX = INDEX + 12
             ELSEIF (nod_type .EQ. 4) THEN
               INDEX = INDEX + 1
             ELSEIF (nod_type .GE. 5) THEN
     !          do nothing for type >= 5 (structure, boundary, NWRW)
             ENDIF
             DO INDX =INDEX0, INDEX - 1
               SALTF (INDX) = SLTINI(INODE)
 20          ENDDO
          ENDDO
       ENDIF
!
! *********************************************************************
! *** Init. arrays for Mappix summary output
! *********************************************************************
! Vector initialisation

      RSLMAP1_vhg    = DefaultT0Value   ! 0. !-999.99
      RSLMAP2_ovh    = DefaultT0Value   ! 0. !-999.99
      RSLMAP3_kas    = DefaultT0Value   ! 0. !-999.99
      RSLMAP4_ow     = DefaultT0Value   ! 0. !-999.99
      RSLMAP5_str    = DefaultT0Value   ! 0. !-999.99
      RSLMAP6_bnd    = DefaultT0Value   ! 0. !-999.99
      RSLMAP7_plv    = DefaultT0Value   ! 0. !-999.99
      CUMRSLMAP8_bal = 0.0
      RSLMAP8_bal    = 0.0
      BckCumBal      = 0.0
      RSLMAP9_slt    = DefaultT0Value   ! 0. !-999.99
      RSLMAP14_rwzi  = DefaultT0Value   ! 0. !-999.99
      RSLMAP15_ind   = DefaultT0Value   ! 0. !-999.99
      RSLMAP17_sacr  = DefaultT0Value   ! 0. !-999.99
      RSLMAP16_flows = DefaultT0Value   ! 0. !-999.99
!     RSLMAP18_cel   = 0.0
      RSLMAP19_RRrunoff= DefaultT0Value ! 0. !-999.99
!      RSLMAP1_vhg    = 0.0
!      RSLMAP2_ovh    = 0.0
!      RSLMAP3_kas    = 0.0
!      RSLMAP4_ow     = 0.0
!      RSLMAP5_str    = 0.0
!      RSLMAP6_bnd    = 0.0
!      RSLMAP7_plv    = 0.0
!      CUMRSLMAP8_bal = 0.0
!      RSLMAP8_bal    = 0.0
!      BckCumBal      = 0.0
!      RSLMAP9_slt    = 0.0
!      RSLMAP14_rwzi  = 0.0
!      RSLMAP15_ind   = 0.0
!      RSLMAP17_sacr  = 0.0
!      RSLMAP16_flows = 0.0
!!     RSLMAP18_cel   = 0.0
!      RSLMAP19_RRrunoff= 0.0
! special
      DO IMAP=1,NKAART
        If (Imap .eq. 2 .or. imap .eq. 4 .or. imap .eq. 6) then
          DO IMSR=1,NSRMAP(IMAP)
             If ((Imap .eq. 6 .and. Imsr .eq. 2) .or. &
                 (Imap .eq. 4 .and. Imsr .eq. 1) .or. &
                 (Imap .eq. 2 .and. Imsr .eq. 11)) then
                 DO ILOC=1,NLCMAP(IMAP)
                   Do I=1,2
! bndlevel, ow_level, gw_level
! ARS 14159: only if not average output!!
                     If (OutputAtTimestepOption .ne. 2) then
                       IF (IMAP .EQ. 6 .AND. IMSR .EQ. 2)  RSLMAP6_bnd(IMSR,ILOC,i) = -999.99
                       IF (IMAP .EQ. 4 .AND. IMSR .EQ. 1)  RSLMAP4_ow (IMSR,ILOC,i) = -999.99
                       IF (IMAP .EQ. 2 .AND. IMSR .EQ. 11) RSLMAP2_ovh(IMSR,ILOC,i) = -999.99
                       IF (IMAP .EQ. 2 .AND. IMSR .EQ. 17) RSLMAP2_ovh(IMSR,ILOC,i) = -999.99
                     Endif
                   Enddo
                 EndDo
             EndIf
          EndDo
        EndIf
      EndDo

! up to oct 25 it was like this:
!     DO IMAP=1,NKAART
!        DO IMSR=1,NSRMAP(IMAP)
!          DO ILOC=1,NLCMAP(IMAP)
!            Do I=1,2
!              if (imap .eq. 1) then
!                  RSLMAP1_vhg (IMSR,ILOC,i) = 0.0
!              elseif (imap .eq. 2) then
!                  RSLMAP2_ovh (IMSR,ILOC,i) = 0.0
!              elseif (imap .eq. 3) then
!                  RSLMAP3_kas (IMSR,ILOC,i) = 0.0
!              elseif (imap .eq. 4) then
!                  RSLMAP4_ow  (IMSR,ILOC,i) = 0.0
!              elseif (imap .eq. 5) then
!                  RSLMAP5_str (IMSR,ILOC,i) = 0.0
!              elseif (imap .eq. 6) then
!                  RSLMAP6_bnd (IMSR,ILOC,i) = 0.0
!              elseif (imap .eq. 7) then
!                  RSLMAP7_plv (IMSR,ILOC,i) = 0.0
!              elseif (imap .eq. 8) then
!                  CUMRSLMAP8_bal (IMSR,ILOC,i) = 0.0
!                  RSLMAP8_bal (IMSR,ILOC,i) = 0.0
!                  BckCumBal   (IMSR,ILOC,i) = 0.0
!              elseif (imap .eq. 9) then
!                  RSLMAP9_slt (IMSR,ILOC,i) = 0.0
!              elseif (imap .eq. 10) then
!                  RSLMAP14_rwzi (IMSR,ILOC,i) = 0.0
!              elseif (imap .eq. 11) then
!                  RSLMAP15_ind (IMSR,ILOC,i) = 0.0
!              elseif (imap .eq. 12) then
! Sacramento
!                  RSLMAP17_sacr(IMSR,ILOC,i) = 0.0
!              elseif (imap .eq. 13) then
! laatste kaart is de link flows
!                  RSLMAP16_flows (IMSR,ILOC,i) = 0.0
!              endif
! bndlevel, ow_level, gw_level
!              IF (IMAP .EQ. 6 .AND. IMSR .EQ. 2)  RSLMAP6_bnd(IMSR,ILOC,i) = -9999.
!              IF (IMAP .EQ. 4 .AND. IMSR .EQ. 1)  RSLMAP4_ow (IMSR,ILOC,i) = -9999.
!              IF (IMAP .EQ. 2 .AND. IMSR .EQ. 11) RSLMAP2_ovh(IMSR,ILOC,i) = -9999.
!            Enddo
!          EndDo
!        EndDo
!     EndDo
!
! *********************************************************************
! *** Special att. voor initialisatie berging per event
! ***  (map 8, file 1,serie 3)
! *********************************************************************
!
      IMAP = 8 !NKIND + 1
      IMSR = 3
      If (ExtendedBalanceOutput) Imsr = 5  !Bij extended output is serie 5 de delta berging
      IMSR2 = IMSR * 2

      DO INODE=1,NCNODE
         DO I=IMSR,IMSR2
            RSLMAP8_bal (I,INODE,1) = 0.0
            RSLMAP8_bal (I,INODE,2) = 0.0
         ENDDO
      ENDDO


! *********************************************************************
! *** Initialiseer: nr. timesteps flooding in huidige event
! *********************************************************************

      ITIMEF = 0

      RETURN
      END
