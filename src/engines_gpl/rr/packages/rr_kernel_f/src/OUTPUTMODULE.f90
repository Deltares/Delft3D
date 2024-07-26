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

 
module Output

  !use
  use Conf_fil
  use Conf_Arr
  use Network
  use Link
  use Paved
  use Unpaved
  use Greenhouse
  use OpenWater
  use Structures
  use Boundary
  use NWRW
  Use RWZI
  Use Industry
  use Salts
  use Sacramento
  use RRRunoff
  use RRConnectionBifurcationNodes
  use Messages

  use dio_plt_rw, prop_file_unused => prop_file
  use globals


! variables
  implicit none

! taken from other modules (Paved, Unpaved, etc)

  type(DioPltType)  :: DataSetConvergence, &   ! output dataset HIS file Convergence info
                       DataSetDailyBndFlows, & ! output dataset HIS file Daily totals boundary flows
                       DataSetRRtoWLM          ! output dataset HIS file RR data to WLM
  type(DioPltType)  :: DataSet(16)        ! output datasets for standard HIS files   ! check that NMap <15


! Link flows
   ! *** QLinkMX = maximum link flow
   REAL, Pointer, SAVE ::     QLinkMX(:,:)

! Backup van Cumulatieve balans data; nodig i.g.v. niet uitvoer elke tijdstap
   REAL, Pointer, SAVE ::     BckCumBal (:,:,:)

   REAL, Pointer, SAVE ::     Converge(:)

   contains


  Subroutine Output_Confar

    implicit none

    Logical Success

    Nevnt = NEvent
!!  Allocate ( EventStartDateTime (Nevnt,6) )
!!  Allocate ( EventDuration(Nevnt,6) )

! paved output
    Call PavedOutput_Confar (Nevnt)
    Call UnPavedOutput_Confar (Nevnt)
    Call GreenhouseOutput_Confar (Nevnt)
    Call OpenWaterOutput_Confar (Nevnt)
    Call StructureOutput_Confar (Nevnt)
    Call BoundaryOutput_Confar (Nevnt)
    Call NWRWOutput_Confar (Nevnt)
    Call RWZIOutput_Confar (Nevnt)
    Call IndustryOutput_Confar (Nevnt)
    Call SacramentoOutput_Confar (Nevnt)
!   Call CelOutput_Confar (Nevnt)
    Call RRRunoffNodeOutput_Confar (Nevnt)

    If (NMap .gt. 15) call ErrMsgStandard (981, 0, ' Error: Number of Dataset arrays smaller than number of maps ', &
                                           ' OutputModule' )

! Link Maxima
    Success = DH_AllocInit (NcLink, NEvnt, QLinkMx, 0E0)
    Success = success .and. DH_AllocInit (12, NNOD, 2, BckCumBal, 0E0)
    Success = success .and. DH_AllocInit (NNOD, Converge, 0E0)
    If (.not. success)  call ErrMsgStandard (981, 0, ' Error allocating arrays in subroutine ', ' Output_ConfAr' )
!   ALLOCATE   ( QLinkMX(NcLink,NEvnt), Stat=Allocation_Error )
! Cumulatieve balans backup      BckCumBal (:,:,:,:)
!   ALLOCATE   ( BckCumBal(12,NNOD,2), Stat=Allocation_Error )
!   ALLOCATE ( Converge(NNOD), Stat=Allocation_Error )

  Return
  End subroutine Output_Confar



  SUBROUTINE WRDATA (IOUT9,IOUT2, IOUT3, IOUT4, IOUT5, IOUT6, IOUT7,IOUT8)

! *********************************************************************
! ***                D E L F T         H Y D R A U L I C S
! ***
! ***              WATER RESOURCES AND ENVIRONMENT DIVISION
! *********************************************************************
! *** Program :  DELFLAND version 4.0.                 Date: March 1995
! *********************************************************************
! *** Last update: March  1995       By : Geert Prinsen
! *********************************************************************
! *** Brief description:
! *** ------------------
! ***   Write overview of input data; taken from 3BPostprocessor
! *********************************************************************
! *** Input/output parameters:
! *** ------------------------
! *********************************************************************
!
      IMPLICIT NONE

      CHARACTER*20 NAMTYP(NKND)
      CHARACTER*10 CDATE, CTIME, CZONE
      INTEGER      Time_values(8), I,j, IKKL, IPTYP, IPOPP, idum
      INTEGER      IOUT9, IOUT2, IOUT3, IOUT4, IOUT5, IOUT6,IOUT7,IOUT8
      INTEGER*2    RNDATE(3), RNTIME(4)
      REAL         VH_AR, OVH_AR, KAS_AR, KASBAS, OW_AR, PLV_AR, Sacr_Ar, CelArea, &
                   RRRunoff_Area, TOT_AR, RAREA, RLVL

      Real         PeilArray(6), AreaArray(6)

!
! Zet File unit nummers en open files
!
     Call OpenFl (IOut9,ConfFil_get_NAMFIL(23),1,2)
! ARS 4845: ook testen op optie ReducedOutput (SmallOutput)
! Nov 2001: SmallOutput vervangen door optie per knooptype
     If (ncvhg  .gt. 0 .and. OutputDesired(1) ) Call OpenFl (IOut2,ConfFil_get_NAMFIL(24),1,2)
     if (ncovhg .gt. 0 .and. OutputDesired(2) ) Call OpenFl (IOut3,ConfFil_get_NAMFIL(25),1,2)
     if (nckas  .gt. 0 .and. OutputDesired(3) ) Call OpenFl (IOut4,ConfFil_get_NAMFIL(26),1,2)
     if (ncow   .gt. 0 .and. OutputDesired(4) ) Call OpenFl (IOut5,ConfFil_get_NAMFIL(27),1,2)
     if (ncstru .gt. 0 .and. OutputDesired(5) ) Call OpenFl (IOut6,ConfFil_get_NAMFIL(28),1,2)
     if (ncboun .gt. 0 .and. OutputDesired(6) ) Call OpenFl (IOut7,ConfFil_get_NAMFIL(29),1,2)
     if (ncpluv .gt. 0 .and. OutputDesired(7) ) Call OpenFl (IOut8,ConfFil_get_NAMFIL(30),1,2)
!                            1         2
!                   12345678901234567890
      NAMTYP (1) = ' Paved area'
      NAMTYP (2) = ' Unpaved area'
      NAMTYP (3) = ' Greenhouse area'
      NAMTYP (4) = ' Open water'
      NAMTYP (5) = ' Structure'
      NAMTYP (6) = ' Boundary'
      NAMTYP (7) = ' NWRW node'
!
! *** Determine date and time
!
       CALL DATE_AND_TIME (CDATE,CTIME,CZONE,Time_values)
       RNDATE(1) = Time_VALUES(1)
       RNDATE(2) = Time_VALUES(2)
       RNDATE(3) = Time_VALUES(3)
       RNTIME(1) = Time_VALUES(5)
       RNTIME(2) = Time_VALUES(6)
       RNTIME(3) = Time_VALUES(7)
       RNTIME(4) = Time_VALUES(8)

! *********************************************************************
! *** Compute total areas
! *********************************************************************
!
      VH_AR  = 0.0
      OVH_AR = 0.0
      KAS_AR = 0.0
      KASBAS = 0.0
      OW_AR  = 0.0
      PLV_AR  = 0.0
      Sacr_AR  = 0.0
      CelArea  = 0.0
      RRRunoff_Area  = 0.0

      DO I=1,NCVHG
         VH_AR = VH_AR + AREAVH(I)
      ENDDO
      DO I=1,NCOVHG
         OVH_AR = OVH_AR + AREAOH(I)
      ENDDO
      DO I=1,NCKAS
         KAS_AR = KAS_AR + AREAS(I)
         DO IKKL=1,NCKKL
           KAS_AR = KAS_AR + AREAKK(I,IKKL)
           KASBAS = KASBAS + AREABK(I,IKKL)
         ENDDO
      ENDDO
      DO I=1,NCOW
!        bepaal ow areaal bij winterstreefpeil
         RLVL = WINLVL(I)
         Do j=1,NVal
            PeilArray(j) = PeilOw(j,i)
            AreaArray(j) = AreaOw(j,i)
         Enddo
         CALL RR_INTERP (NVAL, PeilArray, AreaArray, RLVL, RAREA, IDUM)
         OW_AR = OW_AR + RAREA
      ENDDO
      DO I=1,NCPLUV
         DO IPTYP=1,NPTYP
            DO IPOPP=1,NPOPP
               PLV_AR = PLV_AR + AREAPV(I,IPTYP,IPOPP)
            ENDDO
         ENDDO
      ENDDO
      DO I=1,NCSacr
         Sacr_AR = Sacr_AR + AREASA(I)
      ENDDO
!     DO I=1,NcCell
!        CelArea = CelArea + CellData(i)%TotalArea
!     ENDDO
      DO I=1,NCRRRunoff
         RRRunoff_Area = RRRunoff_Area + AREA_RRRunoffNode(I)
      ENDDO

      VH_AR  = VH_AR  / Float(HA2M)
      OVH_AR = OVH_AR / Float(HA2M)
      KAS_AR = KAS_AR / Float(HA2M)
      KASBAS = KASBAS / Float(HA2M)
      OW_AR  = OW_AR  / Float(HA2M)
      PLV_AR = PLV_AR  / Float(HA2M)
      Sacr_AR= Sacr_AR / Float(HA2M)
      CelArea= CelArea / Float(HA2M)
      RRRunoff_Area = RRRunoff_Area / Float(HA2M)
      TOT_AR = VH_AR + OVH_AR + KAS_AR + OW_AR + PLV_AR + Sacr_Ar + CelArea + RRRunoff_Area
!
! *********************************************************************
! *** Write overview of input data
! *********************************************************************
!
      WRITE(IOUT9,1) RNDATE(3),RNDATE(2),RNDATE(1),(RNTIME(I),I=1,2), &
                     CASENM, NCNODE, TOT_AR, NCLINK,                              &
                     NCVHG, VH_AR, NCOVHG, OVH_AR, NCKAS, KAS_AR, KASBAS,&
                     NCOW, OW_AR, NCSTRU, NCBOUN, NCPLUV, PLV_AR, NCSacr, Sacr_Ar, &
                     NCRRRunoff, RRRunoff_Area, NcCell, CelArea
    1 FORMAT (' Run datum (DD-MM-YY) and time: ',I2,'-',I2,'-',I4,2X,I2,':',I2,//,   &
              ' Summary of common data    ',/,1X,31('='),//,    &
              ' Case name                 ',A20,/,                              &
              ' Number of nodes            ',I5,F10.2,' ha',/,  &
              ' Number of links            ',I5,/,                              &
              ' Number of paved areas      ',I5,F10.2,' ha',/,  &
              ' Number of unpaved areas    ',I5,F10.2,' ha',/,  &
              ' Number of greenhouse-areas ',I5,F10.2,' ha',/,  &
              '           with basins      ',5X,F10.2,' ha',/,  &
              ' Number of open water nodes ',I5,F10.2,' ha',/,  &
              ' Number of structures       ',I5,/,                              &
              ' Number of boundaries       ',I5,/,                              &
              ' Number of NWRW nodes       ',I5,F10.2,' ha',/,  &
              ' Number of Sacramento nodes ',I5,F10.2,' ha',/,  &
              ' Number of RR Runoff nodes  ',I5,F10.2,' ha',/,  &
              ' Number of Cells            ',I5,F10.2,' ha',/)
!
      WRITE(IOUT9,2) NEVENT, Timesettings%timestepsize
    2 FORMAT (' Number of events           ',I5,/,&
              ' Timestepsize    (s)        ',I5,//)


! Verhard gebied
      IF (NCVHG .GT. 0) Call WrInputDataPaved (Iout9, Iout2, RnDate, RnTime)
      IF (NCOVHG .GT. 0) Call WrInputDataUnpaved (Iout9, Iout3, RnDate, RnTime)
      IF (NCKAS .GT. 0) Call WrInputDataKas (Iout9, Iout4, RnDate, RnTime)
      IF (NCOW .GT. 0) Call WrInputDataOpenWater (Iout9, Iout5, RnDate, RnTime)
      IF (NCSTRU .GT. 0) Call WrInputDataStructures (Iout9, Iout6, RnDate, RnTime)
      IF (NCBOUN .GT. 0) Call WrInputDataBoundaries (Iout9, Iout7, RnDate, RnTime)
! Pluvius-NWRW inloopmodel; uitvoer in m2 ipv in hectare
      IF (NCPLUV .GT. 0) Call WrInputDataNWRW (Iout9, Iout8, RnDate, RnTime)
      IF (NCSacr .GT. 0) Call WrInputDataSacramento (Iout9)
!     IF (NCRRRunoff .GT. 0) Call WrInputDataRRRunoff (Iout9)

! Zet datum/tijd en andere header in alle uitvoerfiles
! ARS 4845: ook testen op optie ReducedOutput
! is opgenomen in bovengenoemde WrInputDataxxxx routines
!
! Sluit algemene file
      Call CloseGP(IOUT9)

      RETURN
      END Subroutine WrData




      SUBROUTINE WR1OUT (IOUT2, IOUT3, IOUT4, IOUT5, IOUT6, IOUT7,IOUT8, IEvent)

! *********************************************************************
! ***                D E L F T         H Y D R A U L I C S
! ***
! ***              WATER RESOURCES AND ENVIRONMENT DIVISION
! *********************************************************************
! *** Program :  DELFT_3B version 4.0.                 Date: Nov   1995
! *********************************************************************
! *** Last update: March  1995       By : Geert Prinsen
! *********************************************************************
! *** Brief description:
! *** ------------------
! ***   Write overall output
! *********************************************************************
!
      IMPLICIT NONE

!
      CHARACTER*3 MONTH(12)
      INTEGER IOUT2, IOUT3, IOUT4, IOUT5, IOUT6, IOUT7, IOUT8
      INTEGER IEVENT, INODE, IKIND, INr

! *********************************************************************
! *** Write output
! *********************************************************************
!
      MONTH(1)  = 'Jan'
      MONTH(2)  = 'Feb'
      MONTH(3)  = 'Mar'
      MONTH(4)  = 'Apr'
      MONTH(5)  = 'Mei'
      MONTH(6)  = 'Jun'
      MONTH(7)  = 'Jul'
      MONTH(8)  = 'Aug'
      MONTH(9)  = 'Sep'
      MONTH(10) = 'Okt'
      MONTH(11) = 'Nov'
      MONTH(12) = 'Dec'
!
! ARS 4845: ook testen op optie ReducedOutput (Nov2001: test op OutputDesired(per type))
! ARS 5305/5306/3549 standaard eenheden in uitvoer

      DO INODE=1,NCNODE
         IKind=EiNode(INODE,3)
         INr  =EiNode(INODE,2)
         IF (IKIND .EQ. 1 .and. OutputDesired(1)) THEN
            Call Wr1OutPaved (Iout2, Ievent, Month, Inode, Inr)
         ElseIf (IKIND .EQ. 2 .and. OutputDesired(2)) THEN
            Call Wr1OutUnpaved (Iout3, Ievent, Month, Inode, Inr)
         ElseIf (IKIND .EQ. 3 .and. OutputDesired(3)) THEN
            Call Wr1OutGreenhouse (Iout4,  Ievent, Month, INode, Inr)
         ElseIf (IKIND .EQ. 4 .and. OutputDesired(4)) THEN
            Call Wr1OutOpenWater (Iout5,  Ievent, Month, INode, Inr)
         ElseIf (IKIND .EQ. 5 .and. OutputDesired(5)) THEN
            Call Wr1OutStructure (Iout6,  Ievent, Month, INode, Inr)
         ElseIf (IKIND .EQ. 6 .and. OutputDesired(6)) THEN
            Call Wr1OutBoundary (Iout7,  Ievent, Month, INode, Inr)
         ElseIf (IKIND .EQ. 7 .and. OutputDesired(7)) THEN
            Call Wr1OutNWRW (Iout8,  Ievent, Month, INode, Inr)
         Endif
      Enddo


      RETURN
      END Subroutine Wr1Out




      SUBROUTINE WrOOut (Ievent)

! *********************************************************************
! ***                D E L F T         H Y D R A U L I C S
! ***
! ***              WATER RESOURCES AND ENVIRONMENT DIVISION
! *********************************************************************
! *** Program :  DELFT_3B version 1.0.                 Date: March 1995
! *********************************************************************
! *** Last update: March  1995       By : Geert Prinsen
! *********************************************************************
! *** Brief description:
! *** ------------------
! ***   Write output per event
! ***   Update/store results for overall output
! *********************************************************************
! *** Input/output parameters:
! *** ------------------------
! ***  IOUT3  = file unit number of unformatted output file
! *********************************************************************


    IMPLICIT NONE

    Integer iKKl, iNode, iKind, iMap, iLoc, iNr, iPTyp, iPOpp, idum, IEvent
    Real    area, bMax, rArea

    Integer iDebug, ilink

    if (.not. associated(RSLMAP8_bal)) return

    Idebug = ConfFil_get_iDebug()

! *********************************************************************
! *** Fill arrays for Mappix summary output
! *********************************************************************

      DO INODE=1,NCNODE
         IKIND = EiNode(INODE,3)
         INR   = EiNode(INODE,2)
         IMAP  = IKIND
         ILOC  = INR
!Oct 2001
! balance
         RSLMAP8_bal(1,INODE,1) = CUMRSLMAP8_bal(1,INODE,1)
         RSLMAP8_bal(2,INODE,1) = CUMRSLMAP8_bal(2,INODE,1)
         RSLMAP8_bal(3,INODE,1) = CUMRSLMAP8_bal(3,INODE,1)
         RSLMAP8_bal(4,INODE,1) = CUMRSLMAP8_bal(4,INODE,1)
         RSLMAP8_bal(5,INODE,1) = CUMRSLMAP8_bal(5,INODE,1)
! salt
         IF (ISLCMP .ne. 0) RSLMAP9_slt(1,INODE,1) = SLTMXC(INODE)
!End Oct 2001


         if (idebug .ne. 0)  WRITE (IDEBUG,*) ' WROOUT IMAP INR ILOC',IMAP, INR, ILOC
         IF (IKIND .EQ. 1) THEN
! RWA/mixed sewer, DWA sewer, Storage on street
            RSLMAP1_vhg(1,ILOC,1) = VHMBPC(INR,1,Ievent)
            RSLMAP1_vhg(2,ILOC,1) = VHMBPC(INR,2,Ievent)
            RSLMAP1_vhg(3,ILOC,1) = VHMBPC(INR,3,Ievent)
!           flows default in m3/s
            DO idum=1,13
               RSLMAP1_vhg(idum+3,ILOC,1) = VHMQOU(INR,idum,Ievent)
            enddo
! Unpaved
         ELSEIF (IKIND .EQ. 2) THEN
!           flows default in m3/s
            Do Idum=1,10
              RSLMAP2_ovh(idum,ILOC,1) =  OVMQOU(INR,idum,Ievent)
            Enddo
! ARS 5117: Volgorde in uitvoer:
! 1=gw level
! 2=%inundatie
! 3=storage on land
! 4=volume
!          grondwaterstand tov reference level
            RSLMAP2_ovh(11,ILOC,1) =  OVMGWS(INR,Ievent)
! tov maaiveld
!           IF (IOPT2(1) .EQ. 1)  RSLMAP2_ovh(11,ILOC,1) = OVMGWS(INR,Ievent) - LVLOH(INR)

!% inundation, ARS 1887
! without S curve: inundation = 0 or 100% (LvlOhMx < LvlOw
! with S curve: inundation if SurfaceOutflow >0 or LvlohMx < LvlOw
            RSLMAP2_ovh(12,ILOC,1) =  OvMxPercInund(Inr,Ievent)
! Storage on land (mm)
! Max. volume grondwater, Max. volume on land (m3)
            RSLMAP2_ovh(13,ILOC,1) =  OVMONL(INR,Ievent,1)
            RSLMAP2_ovh(14,ILOC,1) =  OVMGWV(INR,Ievent)
            RSLMAP2_ovh(15,ILOC,1) =  OVMONL(INR,Ievent,2)

!Overschrijding max. gw. peilen toch nog niet gebruikt werd/wordt.
!          overschrijding van gw-peilen in eenheid als voor open water
!            RSLMAP2_ovh(2,3,ILOC,1) = GWEXC (INR,2,Ievent)
!            IF (IOPTND(IKIND+1,2) .EQ. 6) RSLMAP2_ovh(2,3,ILOC,1) = GWEXC (INR,2,Ievent)/ NRSHR
            RSLMAP2_ovh(16,ILOC,1) =  GWEXC (INR,3,Ievent)
!           gw wrt surface level
            RSLMAP2_ovh(17,ILOC,1) =  OVMGWS(INR,Ievent) - LVLOH(INR)
!Max. irrigation
            RSLMAP2_ovh(18,ILOC,1) =  IrrSupply(INR,Ievent)
            RSLMAP2_ovh(19,ILOC,1) =  IrrGWDemand(INR,Ievent)
!Max. vulling onverzadigde zone
            RSLMAP2_ovh(20,ILOC,1) =  OVMBGC(INR,Ievent)
            RSLMAP2_ovh(21,ILOC,1) =  OVMONV(INR,Ievent,1)
            RSLMAP2_ovh(22,ILOC,1) =  OVMONV(INR,Ievent,2)
!
! Greenhouse
         ELSEIF (IKIND .EQ. 3) THEN
!
            BMAX = 0.
            AREA = 0.
            DO IKKL=1,NCKKL
               BMAX = BMAX + KKLMXB(IKKL)*AREAKK(INR,IKKL)
               AREA = AREA + AREAKK(INR,IKKL)
            ENDDO
! add silo
            BMAX = BMAX + SILOC(INR) * AREAS(INR)
            AREA = AREA + AREAS (INR)
! end
           if (idebug .ne. 0) THEN
             WRITE(IDEBUG,*) ' berging greenhouses', KSMBPC(INR,NCKKL+1,Ievent),BMAX
             WRITE(IDEBUG,*) ' outflow greenhouse area', KSMQOU(INR,NCKKL+1,Ievent,1), AREA
           ENDIF
            IF (BMAX .GT. 0) THEN
               RSLMAP3_kas(1,ILOC,1) = KSMBPC(INR,NCKKL+1,Ievent)
            ELSE
               RSLMAP3_kas(1,ILOC,1) = 0.0
            ENDIF
!          flows default in m3/s
            DO idum=1,4
              RSLMAP3_kas(idum+1,ILOC,1) = KSMQOU(INR,NCKKL+1,Ievent,idum)
            enddo

! RR open water
         ELSEIF (IKIND .EQ. 4) THEN
!          open water peil tov NAP (default)
           RSLMAP4_ow(1,ILOC,1) = OWMLVL(INR,Ievent,1)
           RSLMAP4_ow(2,ILOC,1) = OWMLVL(INR,Ievent,2)
           RSLMAP4_ow(3,ILOC,1) = OWMLVL(INR,Ievent,3)
           RSLMAP4_ow(4,ILOC,1) = OWMLVL(INR,Ievent,4)
           RSLMAP4_ow(5,ILOC,1) = OWMLVL(INR,Ievent,5)
!
!          open water peil of tov NAP (default) of tov referentiepeil
!          IF (IOPT2(2) .EQ. 1)  RSLMAP4_ow(1,ILOC,1)= OWMLVL(INR,Ievent,1)-REFLVL(INR)
!          overschrijding van peilen in uren
           RSLMAP4_ow(6,ILOC,1) = OWEXC (INR,2,Ievent) / NrsHr
           RSLMAP4_ow(7,ILOC,1) = OWMLVL(INR,Ievent,6)
           RSLMAP4_ow(8,ILOC,1) = OWMLVL(INR,Ievent,7)
           RSLMAP4_ow(9,ILOC,1) = OWMLVL(INR,Ievent,8)

! Structures
         ELSEIF (IKIND .EQ. 5) THEN
!          flows default in m3/s
            RSLMAP5_str(1,ILOC,1) =  QSTRMX(INR,Ievent)
            RSLMAP5_str(2,ILOC,1) =  ActCrestLevelMx(Inr,Ievent)
            RSLMAP5_str(3,ILOC,1) =  QSTRMX1(INR,Ievent)
            RSLMAP5_str(4,ILOC,1) =  QSTRMX2(INR,Ievent)

! Boundaries
         ELSEIF (IKIND .EQ. 6) THEN
!           flows default in m3/s
            RSLMAP6_bnd(1,ILOC,1) =  QBNDMX(INR,Ievent,1)
            RSLMAP6_bnd(2,ILOC,1) =  QBNDMX(INR,Ievent,2)

! NWRW - Pluvius
         ELSEIF (IKIND .EQ. 7) THEN
!          riool later
!          flows default in m3/s
            RAREA = 0.0
            DO IPTYP=1,NPTYP
              DO IPOPP=1,NPOPP
                 RAREA = RAREA + AREAPV(INR,IPTYP,IPOPP)
              ENDDO
            ENDDO
! ook berging doorgeven aan 3BP!
            RSLMAP7_plv(8 ,ILOC,1) = PLVBPC(INR,1,Ievent)
            RSLMAP7_plv(9 ,ILOC,1) = PLVBPC(INR,2,Ievent)
            RSLMAP7_plv(10,ILOC,1) = PLVBPC(INR,3,Ievent)
! end
            RSLMAP7_plv(1,ILOC,1) = PLVQOU(INR,1,Ievent)
            RSLMAP7_plv(2,ILOC,1) = PLVQOU(INR,2,Ievent)
            RSLMAP7_plv(3,ILOC,1) = PLVQOU(INR,3,Ievent)
            RSLMAP7_plv(4,ILOC,1) = PLVQOU(INR,4,Ievent)
            RSLMAP7_plv(5,ILOC,1) = PLVQOU(INR,5,Ievent)
            RSLMAP7_plv(6,ILOC,1) = PLVQOU(INR,6,Ievent)
            RSLMAP7_plv(7,ILOC,1) = PLVQOU(INR,7,Ievent)
! not added DWA companies RSLMAP7_plv(8,ILOC,1) = PLVQOU(INR,8,Ievent)
!RWZI
         ELSEIF (IKIND .EQ. 14) THEN
!           flows default in m3/s
            RSLMAP14_rwzi(1,ILOC,1) =  QRWZIMX(INR,Ievent)
            RSLMAP14_rwzi(2,ILOC,1) =  QEffRWZIMX(INR,Ievent)
         ELSEIF (IKIND .EQ. 15) THEN
!           flows default in m3/s
!  correctie reeksuitvoer April 2002
            RSLMAP15_ind(1,ILOC,1) = QDemMX(INR,Ievent)
            RSLMAP15_ind(2,ILOC,1) = QAllMX(INR,Ievent)
            RSLMAP15_ind(3,ILOC,1) = QShortMX(INR,Ievent)
            RSLMAP15_ind(4,ILOC,1) = QDisMX(INR,Ievent)
! Sacramento
         ELSEIF (IKIND .EQ. 16) THEN
!           default units
            RSLMAP17_sacr(1,ILOC,1) =  SacMXUZTWC  (INr,Ievent)
            RSLMAP17_sacr(2,ILOC,1) =  SacMXUZFWC  (INr,Ievent)
            RSLMAP17_sacr(3,ILOC,1) =  SacMXLZTWC  (INr,Ievent)
            RSLMAP17_sacr(4,ILOC,1) =  SacMXLZFSC  (INr,Ievent)
            RSLMAP17_sacr(5,ILOC,1) =  SacMXLZFPC  (INr,Ievent)
            RSLMAP17_sacr(6,ILOC,1) =  SacMXPrecip (INr,Ievent)
            RSLMAP17_sacr(7,ILOC,1) =  SacMXPotEvp (INr,Ievent)
            RSLMAP17_sacr(8,ILOC,1) =  SacMXActEvp (INr,Ievent)
            RSLMAP17_sacr(9,ILOC,1) =  SacMXBasFlw (INr,Ievent)
            RSLMAP17_sacr(10,ILOC,1) = SacMXSurFlw (INr,Ievent)
            RSLMAP17_sacr(11,ILOC,1) = SacMXImpFlw (INr,Ievent)
            RSLMAP17_sacr(12,ILOC,1) = SacMXTotRun (INr,Ievent)
            RSLMAP17_sacr(13,ILOC,1) = SacMXChaInf (INr,Ievent)
            RSLMAP17_sacr(14,ILOC,1) = SacMXLosFlw (INr,Ievent)
            RSLMAP17_sacr(15,ILOC,1) = SacMXAdimC  (INr,Ievent)
! Cel
         ELSEIF (IKIND .EQ. 17) THEN
            ! to be added
!            RSLMAP18_cel(1,ILOC,1) = SacMXAdimC  (INr,Ievent)
! RR Runoff     ! ExtRunoff = 18, HBV=19, SCS=20, NAM=31
         ELSEIF (IKIND .EQ. 18) THEN
            ! to be added
!             RSLMAP19_RRRunoff(1,ILOC,1) = 0.0

         ENDIF
      ENDDO

      Do Ilink=1,NcLink
!        flows default in m3/s
         RSLMAP16_flows(1,ILink,1) = QLinkMX(Ilink,Ievent)
      ENDDO


      RETURN
    END Subroutine WrOOut




    SUBROUTINE WRTOUT (IEVENT, Bal3B, MaxBalTerms, UpdateDailyValues, Itmstp)

! *********************************************************************
! ***                D E L F T         H Y D R A U L I C S
! ***
! ***              WATER RESOURCES AND ENVIRONMENT DIVISION
! *********************************************************************
! *** Program :  DELFT_3B version 1.0.                 Date: March 1995
! *********************************************************************
! *** Last update: March  1995       By : Geert Prinsen
! *********************************************************************
! *** Brief description:
! *** ------------------
! ***   Write output per timestep
! ***   Update summary output per event
! *********************************************************************
! *** Input/output parameters:
! *** ------------------------
! ***  IEVENT = event number
! ***  IOUT4  = file unit number of output file
! *********************************************************************


    Implicit none

    Integer iEvent, iNode, iKind, iNr, iMap, iLoc, iSlt, iKKl, i, j, Itmstp, IRRRunoffSub
    Integer iPlv, iPlv2, iPlv3, iplv4, ipTyp, iPOpp
    Integer iOW, Ibnd, IRwzi, iPluv
    Integer NodeUp, NodeDown, kind, inr2, idum
    Real    area, qOw, tVol, rKwel, bTot, bTot0, bMax
    Real    qTot, vKas, UKas, rainT, tSlt, qIn, qOut, rKwl, rWegz, rRain
    Real    rEvapT, rArea, rVolOp, rVolO0, rVolDN, rInRi,rInFa
! Jan 2013: double precision
    Double precision  rVolD0, rInFd, ROnvZone, DeltaVol, DeltaVol2, TempVol, DeltaRouting, RemainingUnitHydComp
    Real    QInNow, QinPrev, QOutNow, QOutPrev
    Integer iDebug
    Logical UpdateDailyValues

    Integer Ilink, Inode1, Inode2, IKind1, IKind2, ihelp
    Real    QvhgOw, QVhgBnd, QVhgRwzi, QVhgPluv, FracPervious, Qingw

! Balansuitvoer 3B
    Integer MaxBalTerms
    Double Precision Bal3B(MaxBalTerms)
!
! *********************************************************************
! *** Store detailed output for Mappix/Mapper
! *********************************************************************

!       RSLMAP16_Flows = 0.0

        iDebug = ConfFil_get_iDebug()
        if (idebug .ne. 0) write(idebug,*) ' wrtout'

        ! vullen van de array RSLMAP van waaruit de TMP files beschreven worden
        ! in de minimum-uitvoer modus wordt alleen de RSLMAP van openwater
        ! kunstwerken en balans gevuld, de rest wordt overgeslagen.
         DO INODE=1,NCNODE
            IKIND = EiNode(INODE,3)
            INR   = EiNode(INODE,2)
            IMAP  = IKIND
            ILOC  = INR
            IF (IKIND .EQ. 1) THEN        ! verhard gebied
!              RWA/mixed sewer in mm
               RSLMAP1_vhg(1,ILOC,1) =  BVRL(INR,1) / AreaVh(inr) *1000.
!              DWA sewer in mm
               RSLMAP1_vhg(2,ILOC,1) =  BVRL(INR,2) / AreaVh(inr) *1000.
!c             storage street in mm
               RSLMAP1_vhg(3,ILOC,1) =  BVSTR(INR) / AreaVh(inr) *1000.

!c             flows default in m3/s
               RSLMAP1_vhg(4,ILOC,1) =  Q1V(INR,1) + Q1V(INR,2)
               RSLMAP1_vhg(5,ILOC,1) =  Q2V(INR,1) + Q2V(INR,2)
               QOW = Q1V(INR,1) + Q1V(INR,2)
               IF (sewer(iNr)%Q2VOW(1) .EQ. 1)  QOW = QOW + Q2V(INR,1)
               IF (sewer(iNr)%Q2VOW(2) .EQ. 1)  QOW = QOW + Q2V(INR,2)
               RSLMAP1_vhg(6,ILOC,1) = QOW
               RSLMAP1_vhg(7,ILOC,1) = RV(INR)/timeSettings%timestepSize
               if (sewer(inr)%systemType .eq. 0) then
                  RSLMAP1_vhg(8,ILOC,1) = DWAPaved(INR)
                  RSLMAP1_vhg(9,ILOC,1) = 0.
               else
                  RSLMAP1_vhg(8,ILOC,1) = 0.
                  RSLMAP1_vhg(9,ILOC,1) = DWAPaved(INR)
               endif
               RSLMAP1_vhg(10,ILOC,1) = INV(INR,1)/timeSettings%timestepSize
               RSLMAP1_vhg(11,ILOC,1) = INV(INR,2)/timeSettings%timestepSize
               RSLMAP1_vhg(12,ILOC,1) = Q1V(INR,1)
               RSLMAP1_vhg(13,ILOC,1) = Q2V(INR,1)
               RSLMAP1_vhg(14,ILOC,1) = Q1V(INR,2)
               RSLMAP1_vhg(15,ILOC,1) = Q2V(INR,2)
               RSLMAP1_vhg(16,ILOC,1) = VV(INR)/timeSettings%timestepSize
               RSLMAP1_vhg(17,ILOC,1) = PavedVolDyn(inr) / AreaVh(inr) *1000.
!              balans uitvoer in map NKIND+1; aangepast voor DWA; Jan 1998.
               ILOC = INODE
! added PavedVolDyn Feb2007 ARS 16398 (was present at one place only)
               If (ExtendedBalanceOutput) then
                 RSLMAP8_bal(1,ILOC,1) = RV(INR) + DWAPaved(inr) * timeSettings%timestepSize
                 RSLMAP8_bal(2,ILOC,1) = 0.0
                 RSLMAP8_bal(3,ILOC,1) = VV(INR)
                 RSLMAP8_bal(4,ILOC,1) = timeSettings%timestepSize * (Q1V(INR,1)+Q2V(INR,1)+Q1V(INR,2)+Q2V(INR,2))
                 RSLMAP8_bal(5,ILOC,1) = BVRL(INR,1) + BVRL(INR,2) + BVSTR(INR) + PavedVolDyn(inr)   &
                                             - BVRL0(INR,1) - BVRL0(INR,2) - BVSTR0(INR) - PavedVolDyn0(inr)
                 RSLMAP8_bal(6,ILOC,1) = RSLMAP8_bal(1,ILOC,1) +  RSLMAP8_bal(2,ILOC,1) - &
                                             RSLMAP8_bal(3,ILOC,1) - RSLMAP8_bal(4,ILOC,1) - &
                                               RSLMAP8_bal(5,ILOC,1)
               Else
                 RSLMAP8_bal(1,ILOC,1) = RV(INR) + DWAPaved(inr) * timeSettings%timestepSize
                 RSLMAP8_bal(2,ILOC,1) = VV(INR) +    &
                       timeSettings%timestepSize * (Q1V(INR,1)+Q2V(INR,1)+Q1V(INR,2)+Q2V(INR,2))
                 RSLMAP8_bal(3,ILOC,1) =  BVRL(INR,1) + BVRL(INR,2) + BVSTR(INR) + PavedVolDyn(inr)  &
                                            - BVRL0(INR,1) - BVRL0(INR,2) - BVSTR0(INR) - PavedVolDyn0(inr)
               Endif
!              zout uitvoer in map NKIND+2
               IF (ISLCMP .ne. 0) THEN
                 ILOC = INODE
                 ISLT = INDSLT(INODE)
                 TVOL = BVRL(INR,1) + BVRL(INR,2) + BVSTR(INR)
                 IF (TVOL .GT. 0.0) THEN
                    RSLMAP9_slt(1,ILOC,1) =   &
                     ( SALTF(ISLT)*BVSTR(INR) + SALTF(ISLT+1)*BVRL(INR,1) &
                                                   + SALTF(ISLT+2)*BVRL(INR,2)) / TVOL
                 ELSE
                    RSLMAP9_slt(1,ILOC,1) = 0.0
                 ENDIF
               ENDIF
!update cumulatieve balans 3B
               Bal3B (2) = Bal3B (2) + RV(inr)
               Bal3B (3) = Bal3B (3) + VV (inr)
               Bal3B (9) = Bal3B (9) + BVSTR(inr)-BVSTR0(inr) + BVRL(inr,1) - BVRL0(inr,1) + BVRL(inr,2) - BVRL0(inr,2) + PavedVolDyn(inr) - PavedVolDyn0(inr)
               Bal3B(15) = Bal3B(15) + DWAPaved(inr) * timeSettings%timestepSize
               if (idebug .ne. 0) THEN
                  WRITE(IDEBUG,*) ' PAVED AREA', INR, VV(INR), BVSTR(INR), BVSTR0(INR), &
                                    BVRL(INR,1), BVRL0(INR,1), BVRL(INR,2), BVRL0(INR,2), DWAPaved(INR)
                  WRITE(IDEBUG,*) ' BAL3B 3,9 ', Bal3B(3), Bal3B(9), Bal3B(15)
                  WRITE(IDEBUG,*) ' PAVED AREA VolDyn', PavedVolDyn(inr), PavedVolDyn0(inr)
               endif


            ELSEIF (IKIND .EQ. 2) THEN      ! onverhard gebied
!             flows default in m3/s
               RKWEL = (KWEL(INR)-WEGZG(INR) ) *AreaGwComp(INR)* timeSettings%timestepSize
               RSLMAP2_ovh(1,ILOC,1) =  Q1O(INR)
               RSLMAP2_ovh(2,ILOC,1) =  Q2O(INR)
               RSLMAP2_ovh(3,ILOC,1) =  RO (INR)/timeSettings%timestepSize
               RSLMAP2_ovh(4,ILOC,1) =  VO (INR)/timeSettings%timestepSize
               RSLMAP2_ovh(5,ILOC,1) =  INO(INR)/timeSettings%timestepSize
               RSLMAP2_ovh(6,ILOC,1) =  RKWEL /timeSettings%timestepSize
               RSLMAP2_ovh(7,ILOC,1) =  VBO(INR)/timeSettings%timestepSize
               RSLMAP2_ovh(8,ILOC,1) =  RzEPOT(INR)/timeSettings%timestepSize
               RSLMAP2_ovh(9,ILOC,1) =  MAX (0.0, QINB(INR)/timeSettings%timestepSize)
               RSLMAP2_ovh(10,ILOC,1) =  MAX (0.0, -1*QINB(INR)/timeSettings%timestepSize)
!  April 2004: even kwel in mm/day toevoegen
!!!               RSLMAP2_ovh(20,ILOC,1) = (Kwel(inr)-WegZg(inr)) * 86400 * 1000.

! ARS 5117: Volgorde in uitvoer:
! 1=gw level
! 2=%inundatie
! 3=storage on land in mm
! 4=volume gw
! 5=volume on land

!              grondwaterstand tov reference level
               RSLMAP2_ovh(11,ILOC,1) =  GWL(INR)
!! tov maaiveld IF (IOPT2(1) .EQ. 1) RSLMAP2_ovh(11,ILOC,1) = GWL(INR) - LVLOH(INR)
! % inundation, ARS 1887 Scurve
               RSLMAP2_ovh(12,ILOC,1) =  PercInundation(inr)
!c             storage land in mm
               RSLMAP2_ovh(13,ILOC,1) =  BOLND(INR) / AreaOH(INR) * 1000.
!              grondwatervolume in m3
               RSLMAP2_ovh(14,ILOC,1) =  BOBD(INR)
!c             storage land in m3
               RSLMAP2_ovh(15,ILOC,1) =  BOLND(INR)

! Overschrijdingen van maximum toegestaan gw-peil wordt niet gebruikt
!               RSLMAP2_ovh(4,3,ILOC,1) =  0
!               IF (GWL(INR) .GT. MAXGWL(INR)) RSLMAP2_ovh(4,3,ILOC,1)= GWL(INR)-MAXGWL(INR)

                IF (GWL(INR) .GT. MAXGWL2(INR)) RSLMAP2_ovh(16,ILOC,1)= GWL(INR)-MAXGWL2(INR)

! GW below surface
               RSLMAP2_ovh(17,ILOC,1) =  GWL(INR) - LVLOH(INR)
! irrigation
               RSLMAP2_ovh(18,ILOC,1) =  IrrigationSupply(Inr)
               RSLMAP2_ovh(19,ILOC,1) =  0.0
               if (IrrigationSource(inr) .eq. 2) RSLMAP2_ovh(19,ILOC,1) =  IrrigationDemand(inr)
! bergingscoefficient
               RSLMAP2_ovh(20,ILOC,1) =  BERGC(INR)
! Onverzadigde Zone in mm
               RSLMAP2_ovh(21,ILOC,1) =  OnvZone(INR)%Actual_mm
               RSLMAP2_ovh(22,ILOC,1) =  OnvZone(INR)%Actual_volume

               ! loop over RRGWLinks to compute groundwater inflow via gwlinks
               Qingw = 0.0
               Do Ilink=1,NcLink
                  if (LinkType(Ilink) .eq. 31) then
                     Inode1 = LnkFrm(ilink)
                     IKIND1 = EiNode(INODE1,3)
                     Inode2 = LnkTo (ilink)
                     IKIND2 = EiNode(INODE2,3)
                     if (inode1 .eq. inode .and. ikind1 .eq. 2) then
                        Qingw = Qingw - QinLink(ilink)
                     elseif (inode2 .eq. inode .and. ikind2 .eq. 2) then
                        Qingw = Qingw + QinLink(ilink)
                     endif
                  endif
               enddo

!              balans uitvoer in map NKIND+1
               ILOC = INODE
               If (ExtendedBalanceOutput) then
                 RSLMAP8_bal(1,ILOC,1) = RO(INR) + RKWEL
! in case of external supply only, add irrigation at node
                 if (IrrigationSource(inr) .eq. 3) RSLMAP8_bal(1,ILOC,1) = RO(INR) + RKWEL + IrrigationDemand(inr) * TimeSettings%TimestepSize
                 RSLMAP8_bal(2,ILOC,1) = Qingw * TimeSettings%TimestepSize
                 RSLMAP8_bal(3,ILOC,1) = VO(INR) + VBO(INR)
                 RSLMAP8_bal(3,ILOC,1) = RSLMAP8_bal(3,ILOC,1) + (IrrigationDemand(inr)-IrrigationSupply(inr)) * TimeSettings%TimestepSize
                 RSLMAP8_bal(4,ILOC,1) = timeSettings%timestepSize * (Q1O(INR)+Q2O(INR))
! in case of irrigation from surface water: subtract irrigation demand from link inflow
                 If (IrrigationSource(inr) .eq. 1) RSLMAP8_bal(4,ILOC,1) = RSLMAP8_bal(4,iloc,1) - IrrigationDemand(inr)* TimeSettings%TimestepSize
                 RSLMAP8_bal(5,ILOC,1) = BOLND(INR) + BOBD(INR) - BOLND0(INR) - BOBD0(INR)  &
                                   + OnvZone(INR)%Actual_Volume - OnvZone(INR)%Init_Volume
                 RSLMAP8_bal(6,ILOC,1) = RSLMAP8_bal(1,ILOC,1) +  RSLMAP8_bal(2,ILOC,1) - &
                                             RSLMAP8_bal(3,ILOC,1) - RSLMAP8_bal(4,ILOC,1) - &
                                               RSLMAP8_bal(5,ILOC,1)
               Else
                 RSLMAP8_bal(1,ILOC,1) = RO(INR) + RKWEL + Qingw * TimeSettings%TimestepSize
! in case of external supply only, add irrigation at node
                 if (IrrigationSource(inr) .eq. 3) RSLMAP8_bal(1,ILOC,1) = RSLMAP8_bal(1,ILOC,1) + IrrigationDemand(inr) * TimeSettings%TimestepSize
                 RSLMAP8_bal(2,ILOC,1) = VO(INR) + VBO(INR) + timeSettings%timestepSize * (Q1O(INR)+Q2O(INR))
                 RSLMAP8_bal(2,ILOC,1) = RSLMAP8_bal(2,ILOC,1) + (IrrigationDemand(inr)-IrrigationSupply(inr)) * TimeSettings%TimestepSize
! in case of irrigation from surface water: subtract irrigation demand from link inflow
                 If (IrrigationSource(inr) .eq. 1) RSLMAP8_bal(2,ILOC,1) = RSLMAP8_bal(2,iloc,1) - IrrigationDemand(inr)*TimeSettings%TimestepSize
                 RSLMAP8_bal(3,ILOC,1) =  BOLND(INR) + BOBD(INR) - BOLND0(INR) - BOBD0(INR)  &
                                   + OnvZone(INR)%Actual_Volume - OnvZone(INR)%Init_Volume
               Endif
!              zout uitvoer in map NKIND+2
               IF (ISLCMP .ne. 0) THEN
                 ILOC = INODE
                 ISLT = INDSLT(INODE)
                 ROnvZone = MAX (0.001 * AREAOH(INR), OnvZone(INR)%Actual_Volume)
                 TVOL = ROnvZone + BOLND(INR) + BOBD(INR)
                 IF (TVOL .GT. 0.0) THEN
                    RSLMAP9_slt(1,ILOC,1) =  ( SALTF(ISLT) * BOLND(INR) +  &
                             SALTF(ISLT+1) * ROnvZone + SALTF(ISLT+2) * BOBD(INR) ) / TVOL
                 ELSE
                    RSLMAP9_slt(1,ILOC,1) = 0.0
                 ENDIF
               ENDIF
!update cumulatieve balans 3B
               Bal3B (2) = Bal3B (2) + RO(inr)
! in case of external supply only, add irrigation at node
               if (IrrigationSource(inr) .eq. 3) Bal3B(2) = Bal3B(2) + IrrigationDemand(inr) * TimeSettings%TimestepSize
               Bal3B (4) = Bal3B (4) + VO(inr) + VBO(inr)
! add irrigation losses
               Bal3B(4) = Bal3B(4) + (IrrigationDemand(inr)-IrrigationSupply(inr)) * TimeSettings%TimestepSize
               Bal3B (7) = Bal3B (7) + RKWEL
               Bal3B (10) = Bal3B (10) + BOLND(inr)-BOLND0(inr) + BOBD(inr) - BOBD0(inr) &
                            + OnvZone(INR)%Actual_Volume - OnvZone(INR)%Init_Volume
               if (idebug .ne. 0) THEN
                  WRITE(IDEBUG,*) ' Unpaved Area', INR, VO(INR), VBO(INR), BOLND(INR), BOLND0(INR), &
                                    BOBD(INR), BOBD0(INR), OnvZone(INR)%Actual_Volume, OnvZone(INR)%Init_Volume
                  WRITE(IDEBUG,*) ' BAL3B 4,10 ', Bal3B(4), Bal3B(10)
               endif

            ELSEIF (IKIND .EQ. 3) THEN            ! kasgebied
               BTOT = 0.
               BTOT0= 0.
               BMAX = 0.
               QTOT = 0.
               VKAS = 0.
               UKAS = 0.
               AREA = 0.
               RAINT= RKD(INR)
               DO IKKL=1,NCKKL
                  BMAX = BMAX + KKLMXB(IKKL) * AREAKK(INR,IKKL)
                  BTOT = BTOT + BKAS(INR,IKKL)
                  BTOT0= BTOT0+ BKAS0(INR,IKKL)
                  VKAS = VKAS + GEBR (INR,IKKL) + VKB(INR,IKKL)
                  UKAS = UKAS + GEBR (INR,IKKL)
                  QTOT = QTOT + QKAS(INR,IKKL)
                  AREA = AREA + AREAKK(INR,IKKL)
                  RAINT= RAINT + RKB(INR,IKKL)
               ENDDO
! add silo
               BMAX = BMAX + SILOC(INR) * AREAS(INR)
               BTOT = BTOT + SILOB(INR)
               BTOT0= BTOT0 + SILOB0(INR)
               VKAS = VKAS + QSILGW(INR)*timeSettings%timestepSize
               QTOT = QTOT + QSILOW(INR)
               AREA = AREA + AREAS (INR)
! end
               IF (BMAX .GT. 0) THEN
                  RSLMAP3_kas(1,ILOC,1) = BTOT
               ELSE
                 RSLMAP3_kas(1,ILOC,1) = 0.0
               ENDIF
!             flows default in m3/s
               RSLMAP3_kas(2,ILOC,1) = QTOT
               RSLMAP3_kas(3,ILOC,1) = RAINT / timeSettings%timestepSize
               RSLMAP3_kas(4,ILOC,1) = (Vkd(inr) + VKas - UKas) / timeSettings%timestepSize
               RSLMAP3_kas(5,ILOC,1) = UKas / timeSettings%timestepSize
!              balans uitvoer in map NKIND+1
               ILOC = INODE
               If (ExtendedBalanceOutput) then
                 RSLMAP8_bal(1,ILOC,1) = RAINT
                 RSLMAP8_bal(2,ILOC,1) = 0.0
                 RSLMAP8_bal(3,ILOC,1) = VKD(INR) + VKAS
                 RSLMAP8_bal(4,ILOC,1) = timeSettings%timestepSize * QTOT
                 RSLMAP8_bal(5,ILOC,1) = BTOT - BTOT0 + BKASD(INR) - BKASD0(INR)
                 RSLMAP8_bal(6,ILOC,1) = RSLMAP8_bal(1,ILOC,1) +  RSLMAP8_bal(2,ILOC,1) - &
                                             RSLMAP8_bal(3,ILOC,1) - RSLMAP8_bal(4,ILOC,1) - &
                                               RSLMAP8_bal(5,ILOC,1)
               Else
                 RSLMAP8_bal(1,ILOC,1) = RAINT
                 RSLMAP8_bal(2,ILOC,1) = VKD(INR) + VKAS + timeSettings%timestepSize * QTOT
                 RSLMAP8_bal(3,ILOC,1) = BTOT - BTOT0 + BKASD(INR) - BKASD0(INR)
               Endif

!              zout uitvoer in map NKIND+2
               IF (ISLCMP .ne. 0) THEN
!                 IMAP = NKIND+2
                 ILOC = INODE
                 ISLT = INDSLT(INODE)
                 IF (BTOT .GT. 0.0) THEN
                    TSLT = 0.0
                    TSLT = TSLT + SALTF(ISLT) * BKASD(INR)
                    DO IKKL=1,NCKKL
                      TSLT = TSLT + SALTF(ISLT+IKKL) * BKAS(INR,IKKL)
                    ENDDO
! add silo zout
                    TSLT = TSLT + SALTF(ISLT+NCKKL+1) * SILOB(INR)
! end
                    RSLMAP9_slt(1,ILOC,1) = TSLT / (BTOT + BKASD(INR))
                 ELSE
                    RSLMAP9_slt(1,ILOC,1) = SALTF(ISLT)
                 ENDIF
               ENDIF

!update cumulatieve balans 3B
               Bal3B (2) = Bal3B (2) + RAINT
               Bal3B (5) = Bal3B (5) + VKD(inr) + VKAS
               Bal3B (11) = Bal3B (11) + BTOT - BTOT0 + BKASD(INR) - BKASD0(INR)
               if (idebug .ne. 0) THEN
                  WRITE(IDEBUG,*) ' Greenhouse AREA', INR, VKD(INR), VKAS, BTOT, BTOT0, BKASD(INR), BKASD0(INR)
                  WRITE(IDEBUG,*) ' BAL3B 5,11 ', Bal3B(5), Bal3B(11)
               endif

            ELSEIF (IKIND .EQ. 4) THEN          ! open water

!             open water peil tov reference level
               RSLMAP4_ow(1,ILOC,1) =  LVLOW(INR)
! tov maaiveld
!              IF (IOPT2(2) .EQ. 1)   then
!                RSLMAP4_ow(1,ILOC,1) = LVLOW(INR) - REFLVL(INR)
!              endif
! also volume (m3)
               RSLMAP4_ow(2,ILOC,1) =  Volow(INR) + ActualExtraBergendVolume(inr)
! rain, evap, seepage (all in m3/s)
               RSLMAP4_ow(3,ILOC,1) =  Row  (INR) / timeSettings%timestepSize
               RSLMAP4_ow(4,ILOC,1) =  Vow  (INR) / timeSettings%timestepSize
               RSLMAP4_ow(5,ILOC,1) =  KwOw (INR) / timeSettings%timestepSize

!             overschrijdingen van maximum peil?
               RSLMAP4_ow(6,ILOC,1) =  0
               IF (LVLOW(INR) .GT. MAXLVL(INR)) then
                 RSLMAP4_ow(6,ILOC,1)= LVLOW(INR)-MAXLVL(INR)
               endif

! April 2002: extra output iteration balance error
               QinNow = QinOw(inr,1) + QinOw(inr,2) + QinOw(inr,3) + QinOw(inr,4)+ QinOw(inr,5) + QinOw(inr,6) + QinOw(inr,7)
               QinPrev = QinOw(inr,1) + Qin0(inr,2) + QinOw(inr,3) + Qin0(inr,4)+ QinOw(inr,5) + Qin0(inr,6) + QinOw(inr,7)
               QOutNow  = QOutOw(inr)
               QOutPrev = QOut0(inr)
               RSLMAP4_ow(7,ILOC,1) = (QinNow - QinPrev + QoutNow - QoutPrev) * TimeSettings%timestepSize
! Oct 2002: extra output filling percentage in terms of water level between target level and maximum level
               Call SetVulling (RSLMAP4_ow(8,ILOC,1), LvlOw(Inr), MaxLvl(Inr), CurrentTargetLevel(INr))
               RSLMAP4_ow(8,ILOC,1) = 100. * RSLMAP4_ow(8,ILOC,1)
               RSLMAP4_ow(9,ILOC,1) = CurrentTargetLevel(INr)


!End April 2002
!              balans uitvoer in map NKIND+1
               ILOC = INODE
               QIN = QINOW(INR,1) + QIN0 (INR,2) + QINOW(INR,3) + QIN0 (INR,4) + QinOw(INR,5) + Qin0(Inr,6) + QinOw(Inr,7)
               QOUT= QOUT0(INR)
!check: correctie voor negatieve debieten: moet dit wel of niet? oorspronkelijk niet aanwezig
              IF (QOUT .LT. 0) THEN
                  QIN = QIN - QOUT
                  QOUT= 0.
              ENDIF
! end

! met kwel/wegzijging op open water:
               RKWL = 0.0
               RWEGZ= 0.0
               IF (KWOW(INR) .GT. 0.0) RKWL  = KWOW(INR)
               IF (KWOW(INR) .LT. 0.0) RWEGZ = -1 * KWOW(INR)

               ILOC = INODE
               If (ExtendedBalanceOutput) then
                 RSLMAP8_bal(1,ILOC,1) = ROW(INR) + RKWL
                 RSLMAP8_bal(2,ILOC,1) = QIN*timeSettings%timestepSize
                 RSLMAP8_bal(3,ILOC,1) = VOW(INR) + RWEGZ
                 RSLMAP8_bal(4,ILOC,1) = QOUT*timeSettings%timestepSize
                 RSLMAP8_bal(5,ILOC,1) = VOLOW(INR) - VOLOW0(INR) &
                                            + ActualExtraBergendVolume(inr) - PreviousExtraBergendVolume(inr)
                 RSLMAP8_bal(6,ILOC,1) = RSLMAP8_bal(1,ILOC,1) +  RSLMAP8_bal(2,ILOC,1) - &
                                           RSLMAP8_bal(3,ILOC,1) - RSLMAP8_bal(4,ILOC,1) - &
                                             RSLMAP8_bal(5,ILOC,1)
               Else
                 RSLMAP8_bal(1,ILOC,1) = ROW(INR) + RKWL + QIN*timeSettings%timestepSize
                 RSLMAP8_bal(2,ILOC,1) = VOW(INR) + RWEGZ + QOUT*timeSettings%timestepSize
                 RSLMAP8_bal(3,ILOC,1) = VOLOW(INR) - VOLOW0(INR) &
                                              + ActualExtraBergendVolume(inr) - PreviousExtraBergendVolume(inr)
               Endif

!              zout uitvoer in map NKIND+2
               IF (ISLCMP .ne. 0) THEN
                 ILOC = INODE
                 ISLT = INDSLT(INODE)
                 RSLMAP9_slt(1,ILOC,1) = SALTF(ISLT)
               ENDIF
!update cumulatieve balans 3B
               Bal3B (2) = Bal3B (2) + ROW(inr)
               Bal3B (6) = Bal3B (6) + VOW(inr)
               Bal3B (8) = Bal3B (8) + RKWL - RWEGZ
               Bal3B (12) = Bal3B (12) + VOLOW(inr) - VOLOW0(inr) + ActualExtraBergendVolume(inr) &
                                                                  - PreviousExtraBergendVolume(inr)

               if (idebug .ne. 0) THEN
                  WRITE(IDEBUG,*) ' kwel ow', RKWL, RWEGZ, BAL3B(8)
               ENDIF

            ELSEIF (IKIND .EQ. 5) THEN          ! structure
!             flows default in m3/s
               RSLMAP5_str(1,ILOC,1) =  QSTRU(INR)
               RslMap5_str(2,ILoc,1) =  ActCrestLevel(inr)
               RSLMAP5_str(3,ILOC,1) =  QSTRU1(INR)
               RSLMAP5_str(4,ILOC,1) =  QSTRU2(INR)
!              balans uitvoer in map NKIND+1
               ILOC = INODE
               If (ExtendedBalanceOutput) then
                 RSLMAP8_bal(1,ILOC,1) = 0.0
                 RSLMAP8_bal(2,ILOC,1) = QSTRU(INR)* timeSettings%timestepSize
                 RSLMAP8_bal(3,ILOC,1) = 0.0
                 RSLMAP8_bal(4,ILOC,1) = QSTRU(INR)* timeSettings%timestepSize
                 RSLMAP8_bal(5,ILOC,1) = 0.0
                 RSLMAP8_bal(6,ILOC,1) = RSLMAP8_bal(1,ILOC,1) +  RSLMAP8_bal(2,ILOC,1) - &
                                           RSLMAP8_bal(3,ILOC,1) - RSLMAP8_bal(4,ILOC,1) - &
                                             RSLMAP8_bal(5,ILOC,1)
               Else
                 RSLMAP8_bal(1,ILOC,1) = QSTRU(INR)* timeSettings%timestepSize
                 RSLMAP8_bal(2,ILOC,1) = QSTRU(INR)* timeSettings%timestepSize
                 RSLMAP8_bal(3,ILOC,1) = 0.0
               Endif
! Zout uitvoer; geeft zoutconcentratie van de bovenstroomse knoop
! Let op stroomrichting en knooptype
               IF (ISLCMP .ne. 0) THEN
                 ILOC = INODE
                 RSLMAP9_slt(1,ILOC,1) = 0.
                 If (QSTRU (inr) .gt. 0) then
                    NodeUp = Upnode(Inode)
                    ISLT = INDSLT(Nodeup)
                    KIND = EiNode(NodeUp,3)
                    Inr2 = EiNode(NodeUp,2)
                    If (kind .eq. 6) then
                       RSLMAP9_slt(1,ILOC,1) = CBND (inr2)
                    else
                       RSLMAP9_slt(1,ILOC,1) = SALTF (islt)
                    endif
                 elseif (QSTRU(inr) .lt. 0) then
                    NodeDown = Donode(Inode)
                    ISLT = INDSLT(NodeDown)
                    KIND = EiNode(NodeDown,3)
                    Inr2 = EiNode(NodeDown,2)
                    If (kind .eq. 6) then
                       RSLMAP9_slt(1,ILOC,1) = CBND (inr2)
                    else
                       RSLMAP9_slt(1,ILOC,1) = SALTF (islt)
                    endif
                 endif
               ENDIF

            ELSEIF (IKIND .EQ. 6) THEN            ! boundary
               if (idebug .ne. 0) write(idebug,*) ' Boundary node ', inr, iloc
               RSLMAP6_bnd(1,ILOC,1) =  QBND(INR)          ! flow in m3/s
               RSLMAP6_bnd(2,ILOC,1) =  BNDPAR(INR,1)      ! peil op de rand in m
!              balans uitvoer in map NKIND+1
               ILOC = INODE
               If (ExtendedBalanceOutput) then
                 If (QBnd(inr) .lt. 0) then
                   RSLMAP8_bal(1,ILOC,1) = -QBND(INR)* timeSettings%timestepSize
                   RSLMAP8_bal(2,ILOC,1) = 0.0
                   RSLMAP8_bal(3,ILOC,1) = 0.0
                   RSLMAP8_bal(4,ILOC,1) = -QBND(INR)* timeSettings%timestepSize
                   RSLMAP8_bal(5,ILOC,1) = 0.0
                   Bal3B (14) = Bal3B (14) + RSLMAP8_bal(1,ILOC,1)
                 else
                   RSLMAP8_bal(1,ILOC,1) = 0.0
                   RSLMAP8_bal(2,ILOC,1) = QBND(INR)* timeSettings%timestepSize
                   RSLMAP8_bal(3,ILOC,1) = QBND(INR)* timeSettings%timestepSize
                   RSLMAP8_bal(4,ILOC,1) = 0.0
                   RSLMAP8_bal(5,ILOC,1) = 0.0
                   Bal3B (13) = Bal3B (13) + RSLMAP8_bal(2,ILOC,1)
                 endif
                 RSLMAP8_bal(6,ILOC,1) = RSLMAP8_bal(1,ILOC,1) +  RSLMAP8_bal(2,ILOC,1) - &
                                             RSLMAP8_bal(3,ILOC,1) - RSLMAP8_bal(4,ILOC,1) - &
                                               RSLMAP8_bal(5,ILOC,1)
               Else
                 RSLMAP8_bal(1,ILOC,1) = QBND(INR)* timeSettings%timestepSize
                 RSLMAP8_bal(2,ILOC,1) = QBND(INR)* timeSettings%timestepSize
                 RSLMAP8_bal(3,ILOC,1) = 0.0
!update cumulatieve balans 3B
                 IF (QBND(INR) .GT. 0) THEN
                    Bal3B (13) = Bal3B (13) + RSLMAP8_bal(1,ILOC,1)
                 ELSE
                    Bal3B (14) = Bal3B (14) - RSLMAP8_bal(1,ILOC,1)
                 ENDIF
               Endif
! Zout uitvoer
               IF (ISLCMP .ne. 0) THEN
                 ILOC = INODE
                 RSLMAP9_slt(1,ILOC,1) = CBND (inr)
               ENDIF

            ELSEIF (IKIND .EQ. 7) THEN
! Pluvius knopen
!  riool later toevoegen
!             flows default in m3/s
               IPLV2 = INDIKP(INR)
               RRAIN = 0.
               REVAPT= 0.
               RAREA = 0.
               RVOLOP= 0.
               RVOLO0= 0.
               RVOLDN= 0.
               RVOLD0= 0.
               DO IPTYP=1,NPTYP
                 DO IPOPP=1,NPOPP
                   RAREA = RAREA + AREAPV (INR,IPTYP,IPOPP)
                   RRAIN = RRAIN + RPV (IPLV2,IPTYP,IPOPP) * AREAPV(INR,IPTYP,IPOPP)
                   REVAPT= REVAPT+ VPV (IPLV2,IPTYP,IPOPP) * AREAPV(INR,IPTYP,IPOPP)
                   RVOLOP= RVOLOP+ BVOP(IPLV2,IPTYP,IPOPP) * AREAPV(INR,IPTYP,IPOPP)
                   RVOLDN= RVOLDN+ NTRRST(IPLV2,IPTYP,IPOPP) * AREAPV(INR,IPTYP,IPOPP)
                   RVOLO0= RVOLO0+ BVOP0(IPLV2,IPTYP,IPOPP) * AREAPV(INR,IPTYP,IPOPP)
                   RVOLD0= RVOLD0+ NTRRS0(IPLV2,IPTYP,IPOPP) * AREAPV(INR,IPTYP,IPOPP)
                   IF ( AREAPV(INR,IPTYP,IPOPP) .GT. 0) THEN
                        RSLMAP7_plv(10,ILOC,1) = infcp(IPLV2,IPTYP,IPOPP,1)
                        RSLMAP7_plv(11,ILOC,1) = infcp(IPLV2,IPTYP,IPOPP,2)
                   ENDIF
                 ENDDO
               ENDDO
! ARS 11173-74 add special area RArea, RRain, REvapt, RVolOp, RVolDn, RVolO0, RVolD0
               if (NrSpecialNwrwAreas(inr) .gt. 0) then
                  if (idebug .ne. 0) then
                     write(Idebug,*) ' NWRW node inode=', inode, ' inr=', inr
                  Endif
                  Do j=1,NrSpecialNwrwAreas(Inr)
                     iplv3  = SpecialInDikP(inr,j)
                     iplv4  = Reference2SpecialDef(inr,j)
                     if (idebug .ne. 0) then
                        write(Idebug,*) ' Specialarea ', j,' iplv3=', iplv3
                        write(Idebug,*) ' area=', SpecialNwrwAreas(Inr,j)
                        write(Idebug,*) ' Rain on iplv3 area=', SpecialRPV(Iplv3)
                        write(Idebug,*) ' NtrRstt on iplv3 area=', SpecialNtrRst(Iplv3)
                     Endif
                     RArea  = RArea  + SpecialNwrwAreas(Inr,j)
                     RRain  = RRain  + SpecialRpv(iplv3) * SpecialNwrwAreas(Inr,j)
                     REvapT = REvapT + SpecialVpv(iplv3) * SpecialNwrwAreas(Inr,j)
                     RVolOp = RVolOp + SpecialBVOP(iplv3) * SpecialNwrwAreas(Inr,j)
                     RVolDn = RVolDn + SpecialNTRRST(iplv3) * SpecialNwrwAreas(Inr,j)
                     RVolO0 = RVolO0 + SpecialBVOP0(iplv3)  * SpecialNwrwAreas(Inr,j)
                     RVolD0 = RVolD0 + SpecialNTRRS0(iplv3) * SpecialNwrwAreas(Inr,j)
                     IF ( SpecialNwrwAreas(inr,j) .GT. 0) THEN
                          RSLMAP7_plv(10,ILOC,1) = SpecialInfcp(iplv3,1)
                          RSLMAP7_plv(11,ILOC,1) = SpecialInfcp(iplv3,2)
                     ENDIF
                  Enddo
               endif
! End ARS 11173-74

! laat Rarea in m2      ! RAREA=RAREA / HA2M
               RINFD=INFDP (IPLV2)
! Pluvius berging in m3
               RSLMAP7_plv(8,ILOC,1) = RVOLOP ! naar mm: / RAREA * 1000.
               RSLMAP7_plv(9,ILOC,1) = RVOLDN ! naar mm: / RArea * 1000.
! Pluvius flows in m3 ipv m3/s, want anders zulke kleine getallen
!              RINRI = DWA(INR)
               RINRI = DWA(INR) + DWA2(INR) + QinPluv(Inr)  ! ARS 12563, add flow from RRlinks connected to this NWRW node
               RINFD = 0.0
               RINFA = 0.0
               DO IPTYP=1,NPTYP
                 DO IPOPP=1,NPOPP
                    RINRI = RINRI + AREAPV(INR,IPTYP,IPOPP) * INPR(IPLV2,IPTYP,IPOPP)/ timeSettings%timestepSize
                    RINFD = RINFD + AREAPV(INR,IPTYP,IPOPP) * IPV (IPLV2,IPTYP,IPOPP)/ timeSettings%timestepSize
                    if (idebug .ne. 0) WRITE(IDEBUG,*) IPTYP, IPOPP, RINFD
                    IF (INFAF) THEN
                      RINFA = RINFA + AREAPV(INR,IPTYP,IPOPP) * INFLAF(IPLV2,IPTYP,IPOPP)
                    ENDIF
                 ENDDO
               ENDDO
! ARS 11173-74 add special area to RINRi, RinfD, RInfA
               if (NrSpecialNwrwAreas(inr) .gt. 0) then
                  if (idebug .ne. 0) then
                     write(Idebug,*) ' NWRW node inode=', inode, ' inr=', inr
                  Endif
                  Do j=1,NrSpecialNwrwAreas(Inr)
                    iplv3  = SpecialInDikP(inr,j)
                    iplv4  = Reference2SpecialDef(inr,j)
                    if (idebug .ne. 0) then
                       write(Idebug,*) ' Specialarea ', j,' iplv3=', iplv3
                       write(Idebug,*) ' area=', SpecialNwrwAreas(Inr,j)
                       write(Idebug,*) ' RINRI on iplv3 area=', SpecialINPR(Iplv3)
                       write(Idebug,*) ' RINFD on iplv3 area=', SpecialIPV(Iplv3)
                    Endif
                    RINRI = RINRI + SpecialNwrwAreas(inr,j) * SpecialINPR(IPLV3)/ timeSettings%timestepSize
                    RINFD = RINFD + SpecialNwrwAreas(inr,j) * SpecialIPV (IPLV3)/ timeSettings%timestepSize
                    IF (SpecialINFilRunoff(iplv4)) THEN
                       RINFA = RINFA + SpecialNwrwAreas(inr,j) * SpecialINFLAF(IPLV3)
                    ENDIF
                  Enddo
               endif
! End ARS 11173-74
               if (idebug .ne. 0) WRITE (IDEBUG,*) ' Total Infiltration-dep',RINFD

               if (UseWadi(iloc).ne. 0) then
                 Call ComputeWadi(iloc, inode, RinRi)
                 RSLMAP7_plv(48,ILOC,1) =  RINRI                  !* timeSettings%timestepSize
                 RSLMAP7_plv(49,ILOC,1) =  WadiInfiltration(iloc) !* timeSettings%timestepSize
                 RSLMAP7_plv(50,ILOC,1) =  WadiSpillOutflow(iloc) !* timeSettings%timestepSize
                 RSLMAP7_plv(51,ILOC,1) =  WadiDrainOutflow(iloc) !* timeSettings%timestepSize
                 RSLMAP7_plv(52,ILOC,1) =  WadiFinalStorage(iloc)
                 RSLMAP7_plv(53,ILOC,1) =  WadiFinalLevel(iloc)
                 RSLMAP7_plv(1,ILOC,1)  =  RslMap7_plv(50,iloc,1) + RslMap7_plv(51,iloc,1)
               else
                 RSLMAP7_plv(48,ILOC,1) =  0.0
                 RSLMAP7_plv(49,ILOC,1) =  0.0
                 RSLMAP7_plv(50,ILOC,1) =  0.0
                 RSLMAP7_plv(51,ILOC,1) =  0.0
                 RSLMAP7_plv(52,ILOC,1) =  0.0
                 RSLMAP7_plv(53,ILOC,1) =  0.0
                 RSLMAP7_plv(1,ILOC,1) =  RINRI !* timeSettings%timestepSize
               endif
! Nov 2009: add detailed info surface inflows into sewer
               DO IPTYP=1,NPTYP
                 DO IPOPP=1,NPOPP
                    ihelp = 11 + (iptyp-1)*NpOpp + IpOpp
                    RSLMAP7_plv(ihelp,ILOC,1) =  AREAPV(INR,IPTYP,IPOPP) * INPR(IPLV2,IPTYP,IPOPP) / timeSettings%timestepSize
                 ENDDO
               ENDDO
               Do j=1,NrSpecialNwrwAreas(Inr)
                  iplv3  = SpecialInDikP(inr,j)
                  iplv4  = Reference2SpecialDef(inr,j)
                  ihelp = 23 + min (12, iplv4)
                  RSLMAP7_plv(ihelp,ILOC,1) = RSLMAP7_plv(ihelp,ILOC,1) + SpecialNwrwAreas(inr,j) * SpecialINPR(IPLV3) / timeSettings%timestepSize
                  ihelp = 35 + min (12, iplv4)
                  RSLMAP7_plv(ihelp,ILOC,1) = RSLMAP7_plv(ihelp,ILOC,1) + SpecialNwrwAreas(inr,j) * (SpecialBVOP(IPLV3) + SpecialNTRRST(iplv3))
               Enddo
! End Nov 2009
               RSLMAP7_plv(2,ILOC,1) =  RINFD !* timeSettings%timestepSize
               RSLMAP7_plv(3,ILOC,1) =  RINFA !* timeSettings%timestepSize
               RSLMAP7_plv(4,ILOC,1) =  RRAIN  / timeSettings%timestepSize
               RSLMAP7_plv(5,ILOC,1) =  REVAPT / timeSettings%timestepSize
               RSLMAP7_plv(6,ILOC,1) =  (RINRI - DWA(INR) - DWA2(INR)) !* timeSettings%timestepSize
               RSLMAP7_plv(7,ILOC,1) =  DWA(INR)   !* timeSettings%timestepSize
               RSLMAP7_plv(54,ILOC,1) =  DWA2(INR) !* timeSettings%timestepSize
!              balans uitvoer in map NKIND+1: toe te voegen
               ILOC = INODE
! ARS 12183: DWA added
               RSLMAP8_bal(1,ILOC,1) = RRAIN + (DWA(INR) + DWA2(INR)) * timeSettings%timestepSize
               RSLMAP8_bal(2,ILOC,1) = REVAPT + timeSettings%timestepSize * (RINFA+RINFD)
               RSLMAP8_bal(3,ILOC,1) = RVOLOP + RVOLDN - RVOLO0 - RVOLD0
! Optie IRiool nog niet actief, altijd nul
               IF (IRIOOL(INR) .EQ. 0) THEN
                  RSLMAP8_bal(2,ILOC,1) = RSLMAP8_bal(2,ILOC,1) +   &
                                    RINRI * timeSettings%timestepSize
               ELSE
                  RSLMAP8_bal(2,ILOC,1) = RSLMAP8_bal(2,ILOC,1)
!                     + overstort riool + reguliere lozing riool
                  RSLMAP8_bal(3,ILOC,1) = RSLMAP8_bal(3,ILOC,1)
!                     + delta berging riool
               ENDIF
               If (ExtendedBalanceOutput) then
                  RSLMAP8_bal(5,ILOC,1) = RSLMAP8_bal(3,ILOC,1)
                  RSLMAP8_bal(3,ILOC,1) = RSLMAP8_bal(2,ILOC,1)
                  RSLMAP8_bal(1,ILOC,1) = RSLMAP8_bal(1,ILOC,1)
                  RSLMAP8_bal(2,ILOC,1) = 0.0                       ! geen verbindingen via RR takken
                  RSLMAP8_bal(4,ILOC,1) = 0.0                       ! geen verbindingen via RR takken
                  RSLMAP8_bal(6,ILOC,1) = RSLMAP8_bal(1,ILOC,1) +  RSLMAP8_bal(2,ILOC,1) - &
                                             RSLMAP8_bal(3,ILOC,1) - RSLMAP8_bal(4,ILOC,1) - &
                                               RSLMAP8_bal(5,ILOC,1)
               Endif
               if (idebug .ne. 0) THEN
                  WRITE(IDEBUG,*) ' Check balans Pluvius'
                  WRITE(IDEBUG,*) RRAIN, REVAPT, RINFA, RINFD
                  WRITE(IDEBUG,*) RINRI, timeSettings%timestepSize
                  WRITE(IDEBUG,*) RVOLOP, RVOLO0
                  WRITE(IDEBUG,*) RVOLDN, RVOLD0
               ENDIF
!update cumulatieve balans Pluvius
               PLVBAL(2) = PLVBAL(2) + RRAIN
               PLVBAL(3) = PLVBAL(3) + REVAPT
               PLVBAL(4) = PLVBAL(4) + RINFD * timeSettings%timestepSize
               PLVBAL(5) = PLVBAL(5) + RINFA * timeSettings%timestepSize
               PLVBAL(6) = PLVBAL(6) + RVOLOP-RVOLO0 + RVOLDN-RVOLD0
               PLVBAL(7) = PLVBAL(7) + ( RINRI - DWA(INR) - DWA2(INR) ) * timeSettings%timestepSize
               PLVBAL(8) = PLVBAL(8) + ( DWA(INR) * timeSettings%timestepSize   )
               PLVBAL(9) = PLVBAL(9) + ( DWA2(INR) * timeSettings%timestepSize )
               PLVBAL(10) = PLVBAL(10) + ( RINRI * timeSettings%timestepSize )
               Bal3B (22) = Bal3B (22) + QInPluv(Inr) * timeSettings%timestepSize

! update maxima pluvius
               IPLV = INR
               PLVBPC(IPLV,1,Ievent) = MAX (PLVBPC(IPLV,1,Ievent), RVOLOP)
               PLVBPC(IPLV,2,Ievent) = MAX (PLVBPC(IPLV,2,Ievent), RVOLDN)
               PLVBPC(IPLV,3,Ievent) = MAX (PLVBPC(IPLV,3,Ievent), 0.0)

               PLVQOU(IPLV,1,Ievent) = MAX (PLVQOU(IPLV,1,Ievent), RINRI) !* timeSettings%timestepSize)
               PLVQOU(IPLV,2,Ievent) = MAX (PLVQOU(IPLV,2,Ievent), RINFD) !* timeSettings%timestepSize)
               PLVQOU(IPLV,3,Ievent) = MAX (PLVQOU(IPLV,3,Ievent), RINFA) !* timeSettings%timestepSize)
               PLVQOU(IPLV,4,Ievent) = MAX (PLVQOU(IPLV,4,Ievent), RRAIN /timeSettings%timestepSize)
               PLVQOU(IPLV,5,Ievent) = MAX (PLVQOU(IPLV,5,Ievent), REVAPT/timeSettings%timestepSize)
               PLVQOU(IPLV,6,Ievent) = MAX (PLVQOU(IPLV,6,Ievent), (RINRI-DWA(IPLV)-DWA2(IPLV))) !*timeSettings%timestepSize)
               PLVQOU(IPLV,7,Ievent) = MAX (PLVQOU(IPLV,7,Ievent), DWA(IPLV) ) !*timeSettings%timestepSize)
               PLVQOU(IPLV,8,Ievent) = MAX (PLVQOU(IPLV,8,Ievent), DWA2(IPLV)) !*timeSettings%timestepSize)

            ELSEIF (IKIND .EQ. 14) THEN            ! RWZI
!             flows default in m3/s
               RSLMAP14_rwzi(1,ILOC,1) =  QRWZI(INR)
               RSLMAP14_rwzi(2,ILOC,1) =  QEffluent(INR)
! balans Map HIS file
               ILOC = INODE
               RSLMAP8_bal(1,ILOC,1) = QRWZI(INR)* timeSettings%timestepSize
               RSLMAP8_bal(2,ILOC,1) = QEffluent(INR)* timeSettings%timestepSize
               RSLMAP8_bal(3,ILOC,1) = (Qrwzi(inr)-QEffluent(inr)) * timeSettings%timestepSize
               If (ExtendedBalanceOutput) then
                  RSLMAP8_bal(5,ILOC,1) = RSLMAP8_bal(3,ILOC,1)
                  RSLMAP8_bal(4,ILOC,1) = RSLMAP8_bal(2,ILOC,1)
                  RSLMAP8_bal(2,ILOC,1) = RSLMAP8_bal(1,ILOC,1)
                  RSLMAP8_bal(1,ILOC,1) = 0.0           ! geen lozingen op WWTP
                  RSLMAP8_bal(3,ILOC,1) = 0.0           ! geen onttrekkingen op WWTP
                  RSLMAP8_bal(6,ILOC,1) = RSLMAP8_bal(1,ILOC,1) +  RSLMAP8_bal(2,ILOC,1) - &
                                             RSLMAP8_bal(3,ILOC,1) - RSLMAP8_bal(4,ILOC,1) - &
                                               RSLMAP8_bal(5,ILOC,1)
                  Bal3B (17) = Bal3B (17) + RSLMAP8_bal(5,ILOC,1)
               Else
                  Bal3B (17) = Bal3B (17) + RSLMAP8_bal(3,ILOC,1)
               Endif
! Zout uitvoer
               IF (ISLCMP .ne. 0) THEN
                 ILOC = INODE
                 ISLT = INDSLT(INODE)
                 RSLMAP9_slt(1,ILOC,1) = SALTF(ISLT)
               ENDIF


            ELSEIF (IKIND .EQ. 15) THEN            ! Industry
!             flows default in m3/s
               RSLMAP15_Ind(1,ILOC,1) =  QIndDem(INR)
               RSLMAP15_Ind(2,ILOC,1) =  QIndAll(INR)
               RSLMAP15_Ind(3,ILOC,1) =  QIndShortage(INR)
               RSLMAP15_Ind(4,ILOC,1) =  QIndReturnFlow(INR)
! balans Map
               ILOC = INODE
               If (ExtendedBalanceOutput) then
                 RSLMAP8_bal(1,ILOC,1) = QIndReturnFlow(INR) * timeSettings%timestepSize
                 RSLMAP8_bal(2,ILOC,1) = QIndAll(INR) * timeSettings%timestepSize
                 RSLMAP8_bal(3,ILOC,1) = QIndAll(INR) * timeSettings%timestepSize
                 RSLMAP8_bal(4,ILOC,1) = QIndReturnFlow(INR) * timeSettings%timestepSize
                 RSLMAP8_bal(5,ILOC,1) = 0.0
                 RSLMAP8_bal(6,ILOC,1) = RSLMAP8_bal(1,ILOC,1) +  RSLMAP8_bal(2,ILOC,1) - &
                                             RSLMAP8_bal(3,ILOC,1) - RSLMAP8_bal(4,ILOC,1) - &
                                               RSLMAP8_bal(5,ILOC,1)
!update cumulatieve balans 3B
                 Bal3B (16) = Bal3B (16) + RSLMAP8_bal(1,ILOC,1) - RSLMAP8_bal(2,ILOC,1)
               Else
                 RSLMAP8_bal(1,ILOC,1) = QIndReturnFlow(INR) * timeSettings%timestepSize
                 RSLMAP8_bal(2,ILOC,1) = QIndAll(INR) * timeSettings%timestepSize
                 RSLMAP8_bal(3,ILOC,1) = 0.0
!update cumulatieve balans 3B
                 Bal3B (16) = Bal3B (16) + RSLMAP8_bal(1,ILOC,1) - RSLMAP8_bal(2,ILOC,1)
               Endif
!zout Map
! als er vraag is, is er een bovenstroomse knoop. Laat dan die zoutconcentratie zien.
! als er geen vraag, maar wel een lozing is: Laat dan de zoutconc. van de lozing zien.
! als er geen vraag of lozing is, dan nullen.
! als er zowel vraag als lozing is, Laat dan de vraag -inlaatconcentratie zien.
               IF (ISLCMP .ne. 0) THEN
                 ILOC = INODE
                 RSLMAP9_slt(1,ILOC,1) = 0.
                 If (QindDem (inr) .gt. 0) then
                    NodeUp = Upnode(Inode)
                    ISLT = INDSLT(Nodeup)
                    RSLMAP9_slt(1,ILOC,1) = SALTF(ISLT)
                 elseif (QindDis(inr) .gt. 0) then
                    RSLMAP9_slt(1,ILOC,1) = SltIndDis(Inr)
                 endif
               ENDIF


            ELSEIF (IKIND .EQ. 16) THEN            ! Sacramento
!             default units: mm, except for the channel inflow (m3/s)
               if (idebug .ne. 0) write(idebug,*) ' Sacramento node ', inr, iloc
               RSLMAP17_Sacr(1,ILOC,1) =  UZTWC(INR)
               RSLMAP17_Sacr(2,ILOC,1) =  UZFWC(INR)
               RSLMAP17_Sacr(3,ILOC,1) =  LZTWC(INR)
               RSLMAP17_Sacr(4,ILOC,1) =  LZFPC(INR)
               RSLMAP17_Sacr(5,ILOC,1) =  LZFSC(INR)
               RSLMAP17_Sacr(6,ILOC,1) =  SacPrecip(INR)
               RSLMAP17_Sacr(7,ILOC,1) =  SacPotEvap(INR)
               RSLMAP17_Sacr(8,ILOC,1) =  SacActEvap(INR)
               RSLMAP17_Sacr(9,ILOC,1) =  SacBaseflow(INR)
               RSLMAP17_Sacr(10,ILOC,1)=  SacSurfaceflow(INR)
               RSLMAP17_Sacr(11,ILOC,1)=  SacRunoffImperv(INR)
               RSLMAP17_Sacr(12,ILOC,1)=  SacTotalRunoff(INR)
               RSLMAP17_Sacr(13,ILOC,1)=  SacChannelInflow(INR)
               RSLMAP17_Sacr(14,ILOC,1)=  SacLossFlow(INR)
               RSLMAP17_Sacr(15,ILOC,1)=  ADIMC(INR)
               RSLMAP17_Sacr(16,ILOC,1)=  SacRunoffSurface(INR)
               RSLMAP17_Sacr(17,ILOC,1)=  SacInterflow(INR)
               RSLMAP17_Sacr(18,ILOC,1)=  SacRoutedDirectFlow(INR)
               RSLMAP17_Sacr(19,ILOC,1)=  SacRoutedSurfaceFlow(INR)
               RSLMAP17_Sacr(20,ILOC,1)=  SacRoutedInterFlow(INR)
               RSLMAP17_Sacr(21,ILOC,1)=  SacPercolation(INR)
! balans Map Sacramento
               ILOC = INODE
               FracPervious = 1. - PCTIM(inr) - ADIMP(Inr)
!              FracPervious = 1.
               DeltaVol = ( UZTWC(INR) - UZTWC0(INR) + &
                            UZFWC(INR) - UZFWC0(INR) + &
                            LZTWC(INR) - LZTWC0(INR) + &
                            LZFPC(INR) - LZFPC0(INR) + &
                            LZFSC(INR) - LZFSC0(INR) ) * AreaSa(inr) * FracPervious * mm2m &
                            + (ADIMC(INR) - ADIMC0(INR)) * AreaSa(inr) * ADIMP(Inr) * mm2m
               DeltaRouting = 0.0
               RemainingUnitHydComp = 1.0 - UnitHydComp(Inr,1)
               Do Idum=2,UnitHyDt(INr)
                  DeltaRouting = DeltaRouting + (Sacr_qq(Inr,idum) - Sacr_Qq0(INr,idum))* RemainingUnitHydComp
                  RemainingUnitHydComp = RemainingUnitHydComp - UnitHydComp(Inr,idum)
               Enddo
               DeltaRouting = DeltaRouting * AreaSa(Inr) * mm2m
!              Write(*,*) FracPervious, Adimp(inr), Deltavol
               ILOC = INODE
               If (ExtendedBalanceOutput) then
                 RSLMAP8_bal(1,ILOC,1) = SacPrecip(INR) * AreaSa(Inr) * mm2m
                 RSLMAP8_bal(2,ILOC,1) = 0.0
                 RSLMAP8_bal(3,ILOC,1) = (SacActEvap(INR) + SacLossFlow(inr)) * AreaSa(Inr) * mm2m
                 RSLMAP8_bal(4,ILOC,1) = SacChannelInflow(inr) * timeSettings%timestepSize
! include routing surface runoff on delta storage
                 RSLMAP8_bal(5,ILOC,1) = DeltaVol + DeltaRouting
                 RSLMAP8_bal(6,ILOC,1) = RSLMAP8_bal(1,ILOC,1) +  RSLMAP8_bal(2,ILOC,1) - &
                                             RSLMAP8_bal(3,ILOC,1) - RSLMAP8_bal(4,ILOC,1) - &
                                               RSLMAP8_bal(5,ILOC,1)
               Else
                 RSLMAP8_bal(1,ILOC,1) =  SacPrecip(INR) * AreaSa(Inr) * mm2m
                 RSLMAP8_bal(2,ILOC,1) = (SacActEvap(INR)+SacLossFlow(inr)) * AreaSa(Inr) * mm2m + &
                                            SacChannelInflow(inr) * timeSettings%timestepSize
                 RSLMAP8_bal(3,ILOC,1) = DeltaVol + DeltaRouting
               Endif
!update cumulatieve balans 3B
               Bal3B (2) = Bal3B (2) + SacPrecip(inr) * AreaSa(Inr) * mm2m
               Bal3B (18) = Bal3B (18) + SacActEvap(inr) * AreaSa(Inr) * mm2m
               Bal3B (19) = Bal3B (19) + SacLossFlow(inr) * AreaSa(Inr) * mm2m
               Bal3B (20) = Bal3B (20) + DeltaVol
               Bal3B (21) = Bal3B (21) + DeltaRouting

!zout Map: niet van toepassing voor Sacramento
               IF (ISLCMP .ne. 0) THEN
                 ILOC = INODE
                 RSLMAP9_slt(1,ILOC,1) = 0.
               ENDIF
            ELSEIF (IKIND .EQ. 17) THEN            ! Cel
               if (idebug .ne. 0) write(idebug,*) ' Cell ', inr, iloc
! assume only 1 paved high, 1 paved low, 1 unpaved high, 1 unpaved low etc
!               Icel = inr
!               Idef = CellData(inr)%PavedHighIndx
!               RSLMAP18_Cel(1,ILOC,1) =  CellData(icel)%PavedHighRainfall(1)
!               RSLMAP18_Cel(2,ILOC,1) =  CellData(icel)%PavedHighSurfaceEvap(1)
!               RSLMAP18_Cel(3,ILOC,1) =  CellData(icel)%PavedHighSewerInflow(1)
!               RSLMAP18_Cel(4,ILOC,1) =  CellData(icel)%PavedHighThroughPut(1)
!               RSLMAP18_Cel(5,ILOC,1) =  CellData(icel)%PavedHighFinalRoofStorage(1) + &
!                                            CellData(icel)%PavedHighFinalGutterStorage(1)
!               Idef = CellData(inr)%PavedLowIndx
!               RSLMAP18_Cel(6,ILOC,1) =  CellData(icel)%PavedLowRainfall(1)
!               RSLMAP18_Cel(7,ILOC,1) =  CellData(icel)%PavedLowSurfaceEvap(1)
!               RSLMAP18_Cel(8,ILOC,1) =  CellData(icel)%PavedLowInfiltration(1)
!               RSLMAP18_Cel(9,ILOC,1) =  CellData(icel)%PavedLowSewerInflow(1)
!               RSLMAP18_Cel(10,ILOC,1) = CellData(icel)%PavedLowThroughPut(1)
!               RSLMAP18_Cel(11,ILOC,1) = CellData(icel)%PavedLowFinalStorage(1)
!               Idef = CellData(inr)%UnPavedHighIndx
!               RSLMAP18_Cel(12,ILOC,1) = CellData(icel)%UnPavedHighRainfall(1)
!               RSLMAP18_Cel(13,ILOC,1) = CellData(icel)%UnPavedHighHighEvap(1)
!               RSLMAP18_Cel(14,ILOC,1) = CellData(icel)%UnPavedHighThroughFall(1)
!               RSLMAP18_Cel(15,ILOC,1) = CellData(icel)%UnPavedHighFinalStorageHigh(1)
!               RSLMAP18_Cel(16,ILOC,1) = CellData(icel)%UnPavedHighLowEvap(1)
!               RSLMAP18_Cel(17,ILOC,1) = CellData(icel)%UnPavedHighInfiltration(1)
!
!               RSLMAP18_Cel(19,ILOC,1) = CellData(icel)%UnPavedHighThroughPut(1)
!               RSLMAP18_Cel(20,ILOC,1) = CellData(icel)%UnPavedHighFinalStorageLow(1)
!               Idef = CellData(inr)%UnPavedLowIndx
!               RSLMAP18_Cel(21,ILOC,1) = CellData(icel)%UnPavedLowRainfall(1)
!               RSLMAP18_Cel(22,ILOC,1) = CellData(icel)%UnPavedLowSurfaceEvap(1)
!               RSLMAP18_Cel(23,ILOC,1) = CellData(icel)%UnPavedLowInfiltration(1)
!!              RSLMAP18_Cel(24,ILOC,1) = CellData(icel)%UnPavedLowSewerInflow(1)
!!
!               RSLMAP18_Cel(25,ILOC,1) = CellData(icel)%UnPavedLowThroughPut(1)
!               RSLMAP18_Cel(26,ILOC,1) = CellData(icel)%UnPavedLowFinalStorage(1)
!               Idef = CellData(inr)%OpenWaterIndx
!               RSLMAP18_Cel(27,ILOC,1) = CellData(icel)%OpenWaterRainfall(1)
!               RSLMAP18_Cel(28,ILOC,1) = CellData(icel)%OpenWaterSurfaceEvap(1)
!               RSLMAP18_Cel(29,ILOC,1) = CellData(icel)%OpenWaterFinalStorage(1)
!               RSLMAP18_Cel(30,ILOC,1) = CellData(icel)%FinalOpenWaterLevel(1)
!               RSLMAP18_Cel(31,ILOC,1) = CellData(icel)%OpenWaterInfiltration(1)
!               RSLMAP18_Cel(32,ILOC,1) = CellData(icel)%OpenWaterIrrDemand(1)
!               RSLMAP18_Cel(33,ILOC,1) = CellData(icel)%OpenWaterIrrSupply(1)
!               RSLMAP18_Cel(34,ILOC,1) = CellData(icel)%OpenWaterThroughPut(1)
!               Idef = CellData(inr)%SewerIndx
!               RSLMAP18_Cel(35,ILOC,1) = CellData(icel)%SewerFinalStorage
!               RSLMAP18_Cel(36,ILOC,1) = CellData(icel)%SewerTotalSewerInflow
!               RSLMAP18_Cel(37,ILOC,1) = CellData(icel)%SewerTotalDWAInflow
!               RSLMAP18_Cel(38,ILOC,1) = CellData(icel)%TotalSewerLeakage
!               RSLMAP18_Cel(39,ILOC,1) = CellData(icel)%SewerDischarge
!               RSLMAP18_Cel(40,ILOC,1) = CellData(icel)%SewerThroughPut
!               Idef = CellData(inr)%DrainageIndx
!               RSLMAP18_Cel(41,ILOC,1) = CellData(inr)%FinalGroundwaterlevel
!               RSLMAP18_Cel(42,ILOC,1) = CellData(inr)%FinalGWStorage
!               RSLMAP18_Cel(43,ILOC,1) = CellData(inr)%TotalDrainageFlow
!               RSLMAP18_Cel(44,ILOC,1) = CellData(inr)%IrrSupplyFromGW
!!              Unsaturated zone
!               RSLMAP18_Cel(45,ILOC,1) = CellData(inr)%PotEvap / CellData(inr)%TotalArea / mm2m
!               RSLMAP18_Cel(46,ILOC,1) = CellData(inr)%ActEvap / CellData(inr)%TotalArea / mm2m
!               RSLMAP18_Cel(47,ILOC,1) = CellData(inr)%Percolation / CellData(inr)%TotalArea / mm2m
!               RSLMAP18_Cel(48,ILOC,1) = CellData(inr)%UnsatZoneFinalMM
!!              RSLMAP18_Cel(49,ILOC,1) = CellData(inr)%ThroughPut
!
!               RSLMAP18_Cel(50,ILOC,1) = CellData(inr)%TotalRainfall
!               RSLMAP18_Cel(51,ILOC,1) = CellData(inr)%TotalEvap
!               RSLMAP18_Cel(52,ILOC,1) = CellData(inr)%DeltaStorage
!               RSLMAP18_Cel(53,ILOC,1) = CellData(inr)%IrrSupplyFromExt
!               RSLMAP18_Cel(54,ILOC,1) = CellData(inr)%TotalCellInflowSeepDWA
!               RSLMAP18_Cel(55,ILOC,1) = CellData(inr)%TotalCellOutflowToExternal
!
!! balans Map Cel
!               ILOC = INODE
!               If (ExtendedBalanceOutput) then
!                 RSLMAP8_bal(1,ILOC,1) = CellData(inr)%TotalRainfall
!                 RSLMAP8_bal(2,ILOC,1) = 0.0
!                 Idef = CellData(inr)%DrainageIndx
!                 RSLMAP8_bal(3,ILOC,1) = CellData(inr)%TotalEvap
!                 RSLMAP8_bal(4,ILOC,1) = CellData(inr)%TotalCellOutflowToExternal
!                 RSLMAP8_bal(5,ILOC,1) = CellData(inr)%DeltaStorage
!                 RSLMAP8_bal(6,ILOC,1) = RSLMAP8_bal(1,ILOC,1) +  RSLMAP8_bal(2,ILOC,1) - &
!                                             RSLMAP8_bal(3,ILOC,1) - RSLMAP8_bal(4,ILOC,1) - &
!                                               RSLMAP8_bal(5,ILOC,1)
!               Else
!                 RSLMAP8_bal(1,ILOC,1) = CellData(inr)%TotalRainfall
!                 RSLMAP8_bal(2,ILOC,1) = CellData(inr)%TotalEvap + CellData(inr)%TotalCellOutflowToExternal
!                 RSLMAP8_bal(3,ILOC,1) = CellData(inr)%DeltaStorage
!               Endif
!!              to be added
!!update cumulatieve balans 3B
!               Bal3B (2)  = Bal3B (2)  + CellData(inr)%TotalRainfall !* CellData(Inr)%TotalArea * mm2m
!               Bal3B (24) = Bal3B (24) + CellData(inr)%TotalEvap
!               Bal3B (25) = Bal3B (25) + CellData(inr)%TotalCellInflowSeepDWA -CellData(inr)%IrrSupplyFromExt
!               Bal3B (26) = Bal3B (26) + CellData(inr)%DeltaStorage


            ELSEIF (IKIND .EQ. 18 .or. Ikind .eq. 19 .or. &
                     ikind .eq. 20 .or. ikind .eq. 22 .or. ikind .eq. 23 .or. ikind .eq. 31)  THEN     ! External Runoff, HBV, SCS, LGSI, Wagmod, Walrus, NAM
!             default units: m3/s
               ILOC  = INR
               IRRRunoffSub = RRRUnoff_SubIndex(inr)
               if (idebug .ne. 0) write(idebug,*) ' RR Runoff node ', inr, iloc, iRRRunoffSub
               if (RRRunoff_CompOption(Inr) .eq. 0 ) then
                   ! simple runoff node: specified inflows
                   RSLMAP19_RRRunoff(5,ILOC,1) =  RRRunoffNode_Outflow(INR)
               elseif (RRRunoff_CompOption(Inr) .eq. 1 ) then
                   ! simple runoff node: HBV model ! first in m3/s, rest in mm
                   RSLMAP19_RRRunoff(1,ILOC,1) =  HBV_Rainfall(IRRRunoffSub)
                   RSLMAP19_RRRunoff(2,ILOC,1) =  HBV_PotEvap(IRRRunoffSub)
                   RSLMAP19_RRRunoff(3,ILOC,1) =  HBV_ActEvap(IRRRunoffSub)
                   RSLMAP19_RRRunoff(4,ILOC,1) =  HBV_QRunoffInMM(IRRRunoffSub)
                   RSLMAP19_RRRunoff(5,ILOC,1) =  RRRunoffNode_Outflow(INR)
                   RSLMAP19_RRRunoff(NStartHBV,ILOC,1) =  HBV_SnowFall(IRRRunoffSub)
                   RSLMAP19_RRRunoff(NStartHBV+1,ILOC,1) = HBV_BaseFlow(IRRRunoffSub)
                   RSLMAP19_RRRunoff(NStartHBV+2,ILOC,1) = HBV_InterFlow(IRRRunoffSub)
                   RSLMAP19_RRRunoff(NStartHBV+3,ILOC,1) = HBV_QuickFlow(IRRRunoffSub)
                   RSLMAP19_RRRunoff(NStartHBV+4,ILOC,1) = HBV_DrySnowContent(IRRRunoffSub)
                   RSLMAP19_RRRunoff(NStartHBV+5,ILOC,1) = HBV_FreeWaterContent(IRRRunoffSub)
                   RSLMAP19_RRRunoff(NStartHBV+6,ILOC,1) = HBV_SoilMoisture(IRRRunoffSub)
                   RSLMAP19_RRRunoff(NStartHBV+7,ILOC,1) = HBV_UpperZoneContent(IRRRunoffSub)
                   RSLMAP19_RRRunoff(NStartHBV+8,ILOC,1) = HBV_LowerZoneContent(IRRRunoffSub)
                   RSLMAP19_RRRunoff(NStartHBV+9,ILOC,1) = HBV_Temperature(IRRRunoffSub)
                   RSLMAP19_RRRunoff(NStartHBV+10,ILOC,1) = HBV_Snowmelt    (IRRRunoffSub)
                   RSLMAP19_RRRunoff(NStartHBV+11,ILOC,1) = HBV_Refreezing  (IRRRunoffSub)
                   RSLMAP19_RRRunoff(NStartHBV+12,ILOC,1) = HBV_Infiltration(IRRRunoffSub)
                   RSLMAP19_RRRunoff(NStartHBV+13,ILOC,1) = HBV_Seepage     (IRRRunoffSub)
                   RSLMAP19_RRRunoff(NStartHBV+14,ILOC,1) = HBV_DirectRunoff(IRRRunoffSub)
                   RSLMAP19_RRRunoff(NStartHBV+15,ILOC,1) = HBV_Percolation (IRRRunoffSub)
! not needed       RSLMAP19_RRRunoff(NStartHBV+16,ILOC,1) = HBV_InUpperZone (IRRRunoffSub)
               elseif (RRRunoff_CompOption(Inr) .eq. 2 ) then
                   ! simple runoff node: SCS model- curve number; use SCS rainfall, storage, qrunoff
                   RSLMAP19_RRRunoff(1,ILOC,1) = SCS_Rainfall(IRRRunoffSub)
                   RSLMAP19_RRRunoff(4,ILOC,1) = RRRunoffNode_Outflow(INR) * timeSettings%TimestepSize / Area_RRRunoffNode(INR) / mm2m
                   RSLMAP19_RRRunoff(5,ILOC,1) = RRRunoffNode_Outflow(INR)
                   RSLMAP19_RRRunoff(NStartSCS,ILOC,1)= SCS_Storage(IRRRunoffSub)
               elseif (RRRunoff_CompOption(Inr) .eq. 3 ) then
                   ! simple runoff node: NAM model
                   RSLMAP19_RRRunoff(1,ILOC,1) =  NAMRainfall(IRRRunoffSub)
                   RSLMAP19_RRRunoff(2,ILOC,1) =  NAMPotEvapTot(IRRRunoffSub)
                   RSLMAP19_RRRunoff(3,ILOC,1) =  NAMActEvapTot(IRRRunoffSub)
                   RSLMAP19_RRRunoff(4,ILOC,1) =  RRRunoffNode_Outflow(INR) * timeSettings%TimestepSize / Area_RRRunoffNode(INR) / mm2m
                   RSLMAP19_RRRunoff(5,ILOC,1) =  RRRunoffNode_Outflow(INR)
                   RSLMAP19_RRRunoff(NStartNAM,ILOC,1)    = NAM_Houtside(IRRRunoffSub)
                   RSLMAP19_RRRunoff(NStartNAM+1,ILOC,1)  = NAM_GWPump (IRRRunoffSub)
                   RSLMAP19_RRRunoff(NStartNAM+2,ILOC,1)  = NAM_E1(IRRRunoffSub)
                   RSLMAP19_RRRunoff(NStartNAM+3,ILOC,1)  = NAM_E2(IRRRunoffSub)
                   RSLMAP19_RRRunoff(NStartNAM+4,ILOC,1)  = NAM_E2GWSRZ(IRRRunoffSub)
                   RSLMAP19_RRRunoff(NStartNAM+5,ILOC,1)  = NAM_E2LZS(IRRRunoffSub)
                   RSLMAP19_RRRunoff(NStartNAM+6,ILOC,1)  = NAM_OF(IRRRUnoffSub)
                   RSLMAP19_RRRunoff(NStartNAM+7,ILOC,1)  = NAM_IF(IRRRunoffSub)
                   RSLMAP19_RRRunoff(NStartNAM+8,ILOC,1)  = NAM_FastBF(IRRRunoffSub)
                   RSLMAP19_RRRunoff(NStartNAM+9,ILOC,1)  = NAM_SlowBF(IRRRunoffSub)
                   RSLMAP19_RRRunoff(NStartNAM+10,ILOC,1) = NAM_GWInflow(IRRRunoffSub)
                   RSLMAP19_RRRunoff(NStartNAM+11,ILOC,1) = NAM_DLExt(IRRRunoffSub)
                   RSLMAP19_RRRunoff(NStartNAM+12,ILOC,1) = NAM_GWext(IRRRunoffSub)
                   RSLMAP19_RRRunoff(NStartNAM+13,ILOC,1) = NAM_GWAbsAct(IRRRunoffSub)
                   RSLMAP19_RRRunoff(NStartNAM+14,ILOC,1) = NAM_DLGWPump(iRRRunoffSub)
                   RSLMAP19_RRRunoff(NStartNAM+15,ILOC,1) = NAM_GWGWpump(IRRRunoffSub)
                   RSLMAP19_RRRunoff(NStartNAM+16,ILOC,1) = NAM_INF(IRRRUnoffSub)
                   RSLMAP19_RRRunoff(NStartNAM+17,ILOC,1) = NAM_DL(IRRRunoffSub)
                   RSLMAP19_RRRunoff(NStartNAM+18,ILOC,1) = NAM_G(IRRRunoffSub)
                   RSLMAP19_RRRunoff(NStartNAM+19,ILOC,1) = NAM_CR(IRRRunoffSub)
                   RSLMAP19_RRRunoff(NStartNAM+20,ILOC,1) = NAM_U(IRRRunoffSub)
                   RSLMAP19_RRRunoff(NStartNAM+21,ILOC,1) = NAM_L(IRRRUnoffSub)
                   RSLMAP19_RRRunoff(NStartNAM+22,ILOC,1) = NAM_GWSD (IRRRunoffSub)
                   RSLMAP19_RRRunoff(NStartNAM+23,ILOC,1) = NAM_VU(IRRRunoffSub)
                   RSLMAP19_RRRunoff(NStartNAM+24,ILOC,1) = NAM_VL(IRRRunoffSub)
                   RSLMAP19_RRRunoff(NStartNAM+25,ILOC,1) = NAM_VGWS(IRRRunoffSub)
                   RSLMAP19_RRRunoff(NStartNAM+26,ILOC,1) = NAM_GWL(IRRRunoffSub)
                   RSLMAP19_RRRunoff(NStartNAM+27,ILOC,1) = NAM_GWTD(IRRRunoffSub)
                   RSLMAP19_RRRunoff(NStartNAM+28,ILOC,1) = NAM_AVSoil(IRRRunoffSub)
                   RSLMAP19_RRRunoff(NStartNAM+29,ILOC,1) = NAM_BF(IRRRunoffSub)
                   RSLMAP19_RRRunoff(NStartNAM+30,ILOC,1) = NAM_GWPumpAct (IRRRunoffSub)
                   RSLMAP19_RRRunoff(NStartNAM+31,ILOC,1) = NAM_GWPumpShortage (IRRRunoffSub)
               elseif (RRRunoff_CompOption(Inr) .eq. 4 ) then
                   ! simple runoff node: LGSI model
                   RSLMAP19_RRRunoff(1,ILOC,1) =  LGSI_Precipitation(IRRRunoffSub,1)
                   RSLMAP19_RRRunoff(2,ILOC,1) =  LGSI_PotEvap(IRRRunoffSub,1)
                   RSLMAP19_RRRunoff(3,ILOC,1) =  LGSI_Evaporation(IRRRunoffSub,1)
                   RSLMAP19_RRRunoff(4,ILOC,1) = RRRunoffNode_Outflow(INR) * timeSettings%TimestepSize / Area_RRRunoffNode(INR) / mm2m
                   RSLMAP19_RRRunoff(5,ILOC,1) =  RRRunoffNode_Outflow(INR)
                   RSLMAP19_RRRunoff(NStartLGSI,ILOC,1) = LGSI_Precipitation(IRRRunoffSub,1)
                   RSLMAP19_RRRunoff(NStartLGSI+2,ILOC,1) = LGSI_PotEvap(IRRRunoffSub,1)
                   RSLMAP19_RRRunoff(NStartLGSI+4,ILOC,1) = LGSI_Evaporation(IRRRunoffSub,1)
                   RSLMAP19_RRRunoff(NStartLGSI+6,ILOC,1) = LGSI_Recharge(IRRRunoffSub,1)
                   RSLMAP19_RRRunoff(NStartLGSI+8,ILOC,1) = LGSI_Drainage(IRRRunoffSub,1)    ! drainage flow
                   RSLMAP19_RRRunoff(NStartLGSI+10,ILOC,1) = LGSI_GWFlow(IRRRunoffSub,1)   ! seepage from 2 to 1
                   RSLMAP19_RRRunoff(NStartLGSI+11,ILOC,1) = LGSI_OverlandFlow(IRRRunoffSub,1) ! overland flow
                   RSLMAP19_RRRunoff(NStartLGSI+13,ILOC,1) = LGSI_QpDirect(IRRRunoffSub,1)
                   RSLMAP19_RRRunoff(NStartLGSI+15,ILOC,1) = LGSI_QuickFlow(IRRRunoffSub,1)    ! river flow
                   RSLMAP19_RRRunoff(NStartLGSI+17,ILOC,1) = LGSI_OverLandStorage(IRRRunoffSub,1)
                   RSLMAP19_RRRunoff(NStartLGSI+19,ILOC,1) = LGSI_GwStorage(IRRRunoffSub,1) ! LGSI_UnsatVolume(Inr,1)
                   RSLMAP19_RRRunoff(NStartLGSI+21,ILOC,1) = LGSI_NewVolume(IRRRunoffSub,1)
                   RSLMAP19_RRRunoff(NStartLGSI+23,ILOC,1) = LGSI_NewGWL(IRRRunoffSub,1)
                   RSLMAP19_RRRunoff(NStartLGSI+28,ILOC,1) = LGSI_SurfaceLevel(IRRRunoffSub,1) - LGSI_NewGWL(IRRRunoffSub,1)
                   If (LGSI_NrSubAreas(IRRRunoffSub) .eq. 2) then
                      RSLMAP19_RRRunoff(1,ILOC,1) = ( RSLMAP19_RRRunoff(2,ILOC,1)  * LGSI_Area(iRRRunoffSub,1) + &
                                                       LGSI_Precipitation(IRRRunoffSub,2)  * LGSI_Area(iRRRunoffSub,2) ) / Area_RRRunoffNode(INR)
                      RSLMAP19_RRRunoff(2,ILOC,1) = ( RSLMAP19_RRRunoff(3,ILOC,1)  * LGSI_Area(iRRRunoffSub,1) + &
                                                       LGSI_PotEvap(IRRRunoffSub,2) * LGSI_Area(iRRRunoffSub,2) ) / Area_RRRunoffNode(INR)
                      RSLMAP19_RRRunoff(3,ILOC,1) = ( RSLMAP19_RRRunoff(4,ILOC,1) * LGSI_Area(iRRRunoffSub,1) + &
                                                       LGSI_Evaporation(IRRRunoffSub,2) * LGSI_Area(iRRRunoffSub,2) ) / Area_RRRunoffNode(INR)
                      RSLMAP19_RRRunoff(NStartLGSI+1,ILOC,1) = LGSI_Precipitation(IRRRunoffSub,2)
                      RSLMAP19_RRRunoff(NStartLGSI+3,ILOC,1) = LGSI_PotEvap(IRRRunoffSub,2)
                      RSLMAP19_RRRunoff(NStartLGSI+5,ILOC,1) = LGSI_Evaporation(IRRRunoffSub,2)
                      RSLMAP19_RRRunoff(NStartLGSI+7,ILOC,1) = LGSI_Recharge(IRRRunoffSub,2)
                      RSLMAP19_RRRunoff(NStartLGSI+9,ILOC,1) = LGSI_Drainage(IRRRunoffSub,2)
                      RSLMAP19_RRRunoff(NStartLGSI+12,ILOC,1) = LGSI_OverlandFlow(IRRRunoffSub,2)
                      RSLMAP19_RRRunoff(NStartLGSI+14,ILOC,1) = LGSI_QpDirect(IRRRunoffSub,2)
                      RSLMAP19_RRRunoff(NStartLGSI+16,ILOC,1) = LGSI_Quickflow(IRRRunoffSub,2)
                      RSLMAP19_RRRunoff(NStartLGSI+18,ILOC,1) = LGSI_OverlandStorage(IRRRunoffSub,2)
                      RSLMAP19_RRRunoff(NStartLGSI+20,ILOC,1) = LGSI_GWStorage(IRRRunoffSub,2)
                      RSLMAP19_RRRunoff(NStartLGSI+22,ILOC,1) = LGSI_NewVolume(IRRRunoffSub,2)
                      RSLMAP19_RRRunoff(NStartLGSI+24,ILOC,1) = LGSI_NewGWL(IRRRunoffSub,2)
                      RSLMAP19_RRRunoff(NStartLGSI+29,ILOC,1) = LGSI_SurfaceLevel(IRRRunoffSub,2) - LGSI_NewGWL(IRRRunoffSub,2)
                   Endif
                   RSLMAP19_RRRunoff(NStartLGSI+25,ILOC,1) = LGSI_Runoff(IRRRunoffSub)
                   RSLMAP19_RRRunoff(NStartLGSI+26,ILOC,1) = LGSI_HistoryQtot(IRRRunoffSub,1)       ! term current timestep Qtot
                   RSLMAP19_RRRunoff(NStartLGSI+27,ILOC,1) = LGSI_HistoryQDelayed(IRRRunoffSub,1)   ! term current timestep Qshift (delayed)
               elseif (RRRunoff_CompOption(Inr) .eq. 5 ) then   !Wageningen model
                   RSLMAP19_RRRunoff(1,ILOC,1) = WagMod_P(IRRRunoffSub)
                   RSLMAP19_RRRunoff(2,ILOC,1) = WagMod_ETG(IRRRunoffSub)
                   RSLMAP19_RRRunoff(3,ILOC,1) = WagMod_ETA(IRRRunoffSub)
                   RSLMAP19_RRRunoff(4,ILOC,1) = WagMod_QC(IRRRunoffSub)
                   RSLMAP19_RRRunoff(5,ILOC,1) = WagMod_Runoff(IRRRunoffSub)
                   RSLMAP19_RRRunoff(NStartWagmod  ,ILOC,1) = WagMod_QG(IRRRunoffSub,1)
                   RSLMAP19_RRRunoff(NStartWagmod+1,ILOC,1) = WagMod_QD(IRRRunoffSub,1)
                   RSLMAP19_RRRunoff(NStartWagmod+2,ILOC,1) = WagMod_SM(IRRRunoffSub)
                   RSLMAP19_RRRunoff(NStartWagmod+3,ILOC,1) = WagMod_GStore(IRRRunoffSub)
                   RSLMAP19_RRRunoff(NStartWagmod+4,ILOC,1) = WagMod_Seep(IRRRunoffSub)
               elseif (RRRunoff_CompOption(Inr) .eq. 6 ) then   !Walrus model
                   RSLMAP19_RRRunoff(1,ILOC,1) = Walrus_P    (IRRRunoffSub)       ! P
                   RSLMAP19_RRRunoff(2,ILOC,1) = Walrus_ETPot(IRRRunoffSub)       ! Epot
                   RSLMAP19_RRRunoff(3,ILOC,1) = Walrus_ETAct(IRRRunoffSub)       ! Eact
                   RSLMAP19_RRRunoff(4,ILOC,1) = Walrus_lastQ    (IRRRunoffSub)       ! total runoff in mm
                   RSLMAP19_RRRunoff(5,ILOC,1) = Walrus_lastQdis (IRRRunoffSub)       ! total runoff in m3/s
                   RSLMAP19_RRRunoff(NStartWalrus  ,ILOC,1) = WALRUS_DVCurrent(IRRRunoffSub)  ! storage deficit vadose zone
                   RSLMAP19_RRRunoff(NStartWalrus+1,ILOC,1) = Walrus_DGCurrent(IRRRunoffSub)  ! groundwater depth
                   RSLMAP19_RRRunoff(NStartWalrus+2,ILOC,1) = Walrus_HQCurrent(IRRRunoffSub)  ! level quickflow reservoir
                   RSLMAP19_RRRunoff(NStartWalrus+3,ILOC,1) = Walrus_HSCurrent(IRRRunoffSub)  ! surface water level
                   RSLMAP19_RRRunoff(NStartWalrus+4,ILOC,1) = Walrus_FXSDef (IRRRunoffSub)   ! cum. external to-from surface water, defined
                   RSLMAP19_RRRunoff(NStartWalrus+5,ILOC,1) = Walrus_lastFXG (IRRRunoffSub)   ! cum. external to-from groundwater
                   RSLMAP19_RRRunoff(NStartWalrus+6,ILOC,1) = Walrus_lastPQ(IRRRunoffSub)     ! cum. precipitation in quickflow rsv
                   RSLMAP19_RRRunoff(NStartWalrus+7,ILOC,1) = Walrus_lastPV(IRRRunoffSub)     ! cum. precipitation in vadose zone
                   RSLMAP19_RRRunoff(NStartWalrus+8,ILOC,1) = Walrus_lastPS(IRRRunoffSub)     ! cum. precipitation in surface water
                   RSLMAP19_RRRunoff(NStartWalrus+9,ILOC,1) = Walrus_lastETV(IRRRunoffSub)    ! cum. ET vadose zone
                   RSLMAP19_RRRunoff(NStartWalrus+10,ILOC,1) = Walrus_lastETS(IRRRunoffSub)   ! cum. ET surface water
                   RSLMAP19_RRRunoff(NStartWalrus+11,ILOC,1) = Walrus_dVEQCurrent(IRRRunoffSub) ! equilibrium storage deficit
                   RSLMAP19_RRRunoff(NStartWalrus+12,ILOC,1) = Walrus_lastFQS(IRRRunoffSub)   ! cum. quickflow
                   RSLMAP19_RRRunoff(NStartWalrus+13,ILOC,1) = Walrus_lastFGS(IRRRunoffSub)   ! cum. baseflow
                   RSLMAP19_RRRunoff(NStartWalrus+14,ILOC,1) = Walrus_lastQ(IRRRunoffSub)     ! cum. open water outflow
                   RSLMAP19_RRRunoff(NStartWalrus+15,ILOC,1) = WALRUS_WICurrent(IRRRunoffSub) ! current wetness index value
                   RSLMAP19_RRRunoff(NStartWalrus+16,ILOC,1) = WALRUS_BETACurrent(IRRRunoffSub) ! current evaporation reduction factor
                   RSLMAP19_RRRunoff(NStartWalrus+17,ILOC,1) = WALRUS_HSMIN(IRRRunoffSub)       ! current HSMIN
                   RSLMAP19_RRRunoff(NStartWalrus+18,ILOC,1) = Walrus_FXSAct (IRRRunoffSub)   ! cum. external to-from surface water, realised
!
               endif
! balans Map RR Runoff
               ILOC = INODE
               if (RRRunoff_CompOption(Inr) .eq. 0 ) then
                 ! specified time series: no volume change
                  DeltaVol = 0.0
               elseif (RRRunoff_CompOption(Inr) .eq. 1 ) then
                 ! HBV
                  DeltaVol = (HBV_UpperZoneContent(IRRRunoffSub) - HBV_UpperZoneContent0(IRRRunoffSub) + &
                               HBV_LowerZoneContent(IRRRunoffSub) - HBV_LowerZoneContent0(IRRRunoffSub) + &
                                HBV_SoilMoisture(IRRRunoffSub) - HBV_SoilMoisture0(IRRRunoffSub) + &
                                 HBV_DrySnowContent(IRRRunoffSub) - HBV_DrySnowContent0(IRRRunoffSub) + &
                                  HBV_FreeWaterContent(IRRRunoffSub) - HBV_FreeWaterContent0(IRRRunoffSub) ) &
                                 * Area_RRRunoffNode(inr) * mm2m
               elseif (RRRunoff_CompOption(Inr) .eq. 2 ) then
                  DeltaVol = ( SCS_Storage(IRRRunoffSub) - SCS_Storage0(IRRRunoffSub) ) * Area_RRRunoffNode(inr) * mm2m
               elseif (RRRunoff_CompOption(Inr) .eq. 3 ) then
                 ! NAM
                  DeltaVol = (NAM_U(IRRRunoffSub) - NAM_UInitial(IRRRunoffSub) + &
                               NAM_L(IRRRunoffSub) - NAM_LInitial(IRRRunoffSub) + &
                                NAM_GWSD(IRRRunoffSub) - NAM_GWSDInitial(IRRRunoffSub)) * Area_RRRunoffNode(inr) * mm2m
               elseif (RRRunoff_CompOption(Inr) .eq. 4 ) then
                  ! LGSI
                  DeltaVol = ( LGSI_NewVolume(IRRRunoffSub,1) - LGSI_InitVolume(IRRRunoffSub,1) ) * LGSI_Area(iRRRunoffSub,1)
                  if (idebug .ne. 0) write (idebug,*) 'LGSI area 1',DeltaVol
                  if (LGSI_NrSubAreas(iRRRunoffSub) .eq. 2) DeltaVol = DeltaVol +  ( LGSI_NewVolume(IRRRunoffSub,2) - LGSI_InitVolume(IRRRunoffSub,2) ) * LGSI_Area(iRRRunoffSub,2)
                  if (idebug .ne. 0) write (idebug,*) '+LGSI area 2',DeltaVol
                  ! dynamic storage
                  TempVol = (LGSI_HistoryQTot(iRRRunoffSub,1) - LGSI_HistoryQDelayed(iRRRunoffSub,1)) * Area_RRRunoffNode(Inr) * mm2m
                  if (idebug .ne. 0) write (idebug,*) 'LGSI dynamic storage change',TempVol
                  DeltaVol = DeltaVol + TempVol
                  if (idebug .ne. 0) write (idebug,*) 'LGSI Deltavol Total',DeltaVol
!                  if (idebug .ne. 0) THEN
!                     WRITE(IDEBUG,*) ' LGSI', INR, VV(INR), BVSTR(INR), BVSTR0(INR), &
!                                       BVRL(INR,1), BVRL0(INR,1), BVRL(INR,2), BVRL0(INR,2), DWAPaved(INR)
!                     WRITE(IDEBUG,*) ' BAL3B 3,9 ', Bal3B(3), Bal3B(9), Bal3B(15)
!                     WRITE(IDEBUG,*) ' PAVED AREA VolDyn', PavedVolDyn(inr), PavedVolDyn0(inr)
!                  endif
               elseif (RRRunoff_CompOption(Inr) .eq. 5 ) then  ! Wageningen model
                  DeltaVol = (Wagmod_GSTORE(IRRRunoffSub) +  (-WagMod_GstoreT1(IRRRunoffSub)) + &
                                       WagMod_SM(IRRRunoffSub) - WagMod_SMT1(IRRRunoffSub) ) * Area_RRRunoffNode(inr) * mm2m
                  ! add temporary storage - still in runoff
                  TempVol = (WagMod_RoutVol(IRRRunoffSub) - WagMod_RoutVol1(IRRRunoffSub)) * Area_RRRunoffNode(inr) * mm2m
                  DeltaVol = DeltaVol + TempVol
               elseif (RRRunoff_CompOption(Inr) .eq. 6 ) then  ! Walrus model
!  to be adjusted
                  DeltaVol = - (Walrus_DVCurrent(IRRRunoffSub) - Walrus_DVPrevious(IRRRunoffSub)) * Area_RRRunoffNode(inr) * mm2m * Walrus_AG(IRRRunoffSub) &
                              + (Walrus_HQCurrent(IRRRunoffSub) - Walrus_HQPrevious(IRRRunoffSub)) * Area_RRRunoffNode(inr) * mm2m * Walrus_AG(IRRRunoffSub) &
                               + (Walrus_HSCurrent(IRRRunoffSub) - Walrus_HSPrevious(IRRRunoffSub)) * Area_RRRunoffNode(inr) * mm2m * Walrus_AS(IRRRunoffSub)
!                  de ledigheid zit volledig in de DV, DG niet meer meenemen in de balans
!                 - (Walrus_DGCurrent(IRRRunoffSub) - Walrus_DGPrevious(IRRRunoffSub)) * Area_RRRunoffNode(inr) * mm2m &
!
               Endif
               If (ExtendedBalanceOutput) then
                if (RRRunoff_CompOption(Inr) .eq. 0 ) then
                 RSLMAP8_bal(1,ILOC,1) = RRRunoffNode_Outflow(inr) * timeSettings%timestepSize
!                RSLMAP8_bal(1,ILOC,1) = 0.0
                 RSLMAP8_bal(2,ILOC,1) = 0.0
                 RSLMAP8_bal(3,ILOC,1) = 0.0
                 RSLMAP8_bal(4,ILOC,1) = RRRunoffNode_Outflow(inr) * timeSettings%timestepSize
                 RSLMAP8_bal(5,ILOC,1) = 0.0
                elseif (RRRunoff_CompOption(Inr) .eq. 1 ) then   ! HBV
                 RSLMAP8_bal(1,ILOC,1) = ( HBV_Rainfall(iRRRunoffSub) + HBV_Snowfall(iRRRunoffSub) ) * Area_RRRunoffNode(inr) * mm2m
                 RSLMAP8_bal(2,ILOC,1) = 0.0
                 RSLMAP8_bal(3,ILOC,1) = HBV_ActEvap(iRRRunoffSub)  * Area_RRRunoffNode(inr) * mm2m
                 RSLMAP8_bal(4,ILOC,1) = RRRunoffNode_Outflow(inr) * timeSettings%timestepSize
                 RSLMAP8_bal(5,ILOC,1) = DeltaVol
                elseif (RRRunoff_CompOption(Inr) .eq. 2 ) then    ! SCS
                 RSLMAP8_bal(1,ILOC,1) = SCS_Rainfall(iRRRunoffSub) * Area_RRRunoffNode(inr) * mm2m
                 RSLMAP8_bal(2,ILOC,1) = 0.0
                 RSLMAP8_bal(3,ILOC,1) = 0.0
                 RSLMAP8_bal(4,ILOC,1) = RRRunoffNode_Outflow(inr) * timeSettings%timestepSize
                 RSLMAP8_bal(5,ILOC,1) = DeltaVol
                elseif (RRRunoff_CompOption(Inr) .eq. 3 ) then     ! NAM
                 RSLMAP8_bal(1,ILOC,1) = NAMRainfall(iRRRunoffSub) * Area_RRRunoffNode(inr) * mm2m
                 RSLMAP8_bal(2,ILOC,1) = 0.0
                 RSLMAP8_bal(3,ILOC,1) = NAMActEvapTot(iRRRunoffSub) * Area_RRRunoffNode(inr) * mm2m
                 RSLMAP8_bal(4,ILOC,1) = RRRunoffNode_Outflow(inr) * timeSettings%timestepSize
                 RSLMAP8_bal(5,ILOC,1) = DeltaVol
                 ! add pumpflow, positive = pumping out, negative = pumping into NAM
                 if (NAM_GWPumpAct(iRRRunoffSub) .lt. 0) then
                     RslMap8_bal(1,ILOC,1) = RslMap8_Bal(1,ILoc,1) - NAM_GWPumpAct(IRRRunoffSub) * timeSettings%timestepSize
                 elseif (NAM_GWPumpAct(iRRRunoffSub) .gt. 0) then
                     RslMap8_bal(3,ILOC,1) = RslMap8_Bal(3,ILoc,1) + NAM_GWPumpAct(IRRRunoffSub) * timeSettings%timestepSize
                 endif
                elseif (RRRunoff_CompOption(Inr) .eq. 4 ) then     ! LGSI
                  RSLMAP8_bal(1,ILOC,1) = LGSI_Precipitation(iRRRunoffSub,1)* LGSI_Area(iRRRunoffSub,1) * mm2m + &
                                           LGSI_Precipitation(iRRRunoffSub,2)* LGSI_Area(iRRRunoffSub,2) * mm2m
!                  RSLMAP8_bal(1,ILOC,1) = RSLMAP8_bal(1,ILOC,1) * timeSettings%timestepSize
                  RSLMAP8_bal(2,ILOC,1) = 0.0
                  RSLMAP8_bal(3,ILOC,1) = LGSI_Evaporation(iRRRunoffSub,1) * LGSI_Area(iRRRunoffSub,1) * mm2m + &
                                           LGSI_Evaporation(iRRRunoffSub,2) * LGSI_Area(iRRRunoffSub,2) * mm2m + &
                                            LGSI_Qout(IRRRunoffSub,1) * LGSI_Area(iRRRunoffSub,1) * mm2m + &
                                             LGSI_Qout(IRRRunoffSub,2) * LGSI_Area(iRRRunoffSub,2) * mm2m
!                  RSLMAP8_bal(3,ILOC,1) = RSLMAP8_bal(3,ILOC,1) * timeSettings%timestepSize
                  RSLMAP8_bal(4,ILOC,1) = RRRunoffNode_Outflow(inr) * timeSettings%timestepSize
                  RSLMAP8_bal(5,ILOC,1) = DeltaVol
                elseif (RRRunoff_CompOption(Inr) .eq. 5 ) then     ! Wageningen
                  RSLMAP8_bal(1,ILOC,1) = WagMod_P(iRRRunoffSub) * Area_RRRunoffNode(inr) * mm2m
                  RSLMAP8_bal(2,ILOC,1) = 0.0
                  RSLMAP8_bal(3,ILOC,1) = WagMod_ETA(iRRRunoffSub) * Area_RRRunoffNode(inr) * mm2m
                  if (Wagmod_Seep(iRRRunoffSub) .gt. 0) then
                     RSLMAP8_bal(1,ILOC,1) = RSLMAP8_bal(1,iloc,1) + WagMod_Seep(iRRRunoffSub) * Area_RRRunoffNode(inr) * mm2m * TimeSettings%TimestepSize / 3600.
                  else
                     RSLMAP8_bal(3,ILOC,1) = RSLMAP8_bal(3,iloc,1) - WagMod_Seep(iRRRunoffSub) * Area_RRRunoffNode(inr) * mm2m * TimeSettings%TimestepSize / 3600.
                  endif
                  RSLMAP8_bal(4,ILOC,1) = WagMod_Runoff(iRRRunoffSub) * timeSettings%timestepSize
                  RSLMAP8_bal(5,ILOC,1) = DeltaVol
                elseif (RRRunoff_CompOption(Inr) .eq. 6 ) then     ! Walrus
! to be updated
                   RSLMAP8_bal(1,ILOC,1) = Walrus_P(iRRRunoffSub) * Area_RRRunoffNode(inr) * mm2m
                   RSLMAP8_bal(2,ILOC,1) = 0.0
                   RSLMAP8_bal(3,ILOC,1) = Walrus_ETAct(iRRRunoffSub) * Area_RRRunoffNode(inr) * mm2m
                   if (Walrus_FXGAct(IRRRunoffSub) .gt. 0) then
                      RSLMAP8_bal(1,ILOC,1) = RSLMAP8_bal(1,iloc,1) + Walrus_FXGAct(iRRRunoffSub) * Area_RRRunoffNode(inr) * mm2m !* TimeSettings%TimestepSize / 3600.
                   else
                      RSLMAP8_bal(3,ILOC,1) = RSLMAP8_bal(3,iloc,1) - Walrus_FXGAct(iRRRunoffSub) * Area_RRRunoffNode(inr) * mm2m !* TimeSettings%TimestepSize / 3600.
                   endif
                   if (Walrus_FXSAct(IRRRunoffSub) .gt. 0) then
                      RSLMAP8_bal(1,ILOC,1) = RSLMAP8_bal(1,iloc,1) + Walrus_FXSAct(iRRRunoffSub) * Area_RRRunoffNode(inr) * mm2m !* TimeSettings%TimestepSize / 3600.
                   else
                      RSLMAP8_bal(3,ILOC,1) = RSLMAP8_bal(3,iloc,1) - Walrus_FXSAct(iRRRunoffSub) * Area_RRRunoffNode(inr) * mm2m !* TimeSettings%TimestepSize / 3600.
                   endif
                   RSLMAP8_bal(4,ILOC,1) = Walrus_lastQDis(iRRRunoffSub) * timeSettings%timestepSize
                   RSLMAP8_bal(5,ILOC,1) = DeltaVol
!
                endif
                RSLMAP8_bal(6,ILOC,1) = RSLMAP8_bal(1,ILOC,1) +  RSLMAP8_bal(2,ILOC,1) - &
                                         RSLMAP8_bal(3,ILOC,1) - RSLMAP8_bal(4,ILOC,1) - &
                                          RSLMAP8_bal(5,ILOC,1)
               Else
                if (RRRunoff_CompOption(Inr) .eq. 0 ) then
!                RSLMAP8_bal(1,ILOC,1) = 0.0
                 RSLMAP8_bal(1,ILOC,1) = RRRunoffNode_Outflow(inr) * timeSettings%timestepSize
                 RSLMAP8_bal(2,ILOC,1) = RRRunoffNode_Outflow(inr) * timeSettings%timestepSize
                 RSLMAP8_bal(3,ILOC,1) = 0.0
                elseif (RRRunoff_CompOption(Inr) .eq. 1 ) then
                 RSLMAP8_bal(1,ILOC,1) = ( HBV_Rainfall(iRRRunoffSub) + HBV_Snowfall(iRRRunoffSub) ) * Area_RRRunoffNode(inr) * mm2m
                 RSLMAP8_bal(2,ILOC,1) = HBV_ActEvap(iRRRunoffSub) * Area_RRRunoffNode(inr) * mm2m + &
                                           RRRunoffNode_Outflow(inr) * timeSettings%timestepSize
                 RSLMAP8_bal(3,ILOC,1) = DeltaVol
                elseif (RRRunoff_CompOption(Inr) .eq. 2 ) then
                 RSLMAP8_bal(1,ILOC,1) = SCS_Rainfall(iRRRunoffSub) * Area_RRRunoffNode(inr) * mm2m
                 RSLMAP8_bal(2,ILOC,1) = RRRunoffNode_Outflow(inr) * timeSettings%timestepSize
                 RSLMAP8_bal(3,ILOC,1) = DeltaVol
                elseif (RRRunoff_CompOption(Inr) .eq. 3 ) then
                 RSLMAP8_bal(1,ILOC,1) = NAMRainfall(iRRRunoffSub) * Area_RRRunoffNode(inr) * mm2m
                 RSLMAP8_bal(2,ILOC,1) = NAMActEvapTot(iRRRunoffSub) * Area_RRRunoffNode(inr) * mm2m + &
                                           RRRunoffNode_Outflow(inr) * timeSettings%timestepSize
                 RSLMAP8_bal(3,ILOC,1) = DeltaVol
                 ! add pumpflow, positive = pump in, negative = outflow
                 if (NAM_GWPumpAct(iRRRunoffSub) .lt. 0) then
                     RslMap8_bal(1,ILOC,1) = RslMap8_Bal(1,ILoc,1) - NAM_GWPumpAct(IRRRunoffSub) * timeSettings%timestepSize
                 elseif (NAM_GWPumpAct(iRRRunoffSub) .gt. 0) then
                     RslMap8_bal(2,ILOC,1) = RslMap8_Bal(2,ILoc,1) + NAM_GWPumpAct(IRRRunoffSub) * timeSettings%timestepSize
                 endif
                elseif (RRRunoff_CompOption(Inr) .eq. 4 ) then
                  RSLMAP8_bal(1,ILOC,1) = LGSI_Precipitation(iRRRunoffSub,1)* LGSI_Area(iRRRunoffSub,1) * mm2m + &
                                           LGSI_Precipitation(iRRRunoffSub,2)* LGSI_Area(iRRRunoffSub,2) * mm2m
                  RSLMAP8_bal(2,ILOC,1) = LGSI_Evaporation(iRRRunoffSub,1) * LGSI_Area(iRRRunoffSub,1) * mm2m + &
                                           LGSI_Evaporation(iRRRunoffSub,2) * LGSI_Area(iRRRunoffSub,2) * mm2m + &
                                            LGSI_Qout(IRRRunoffSub,1) * LGSI_Area(iRRRunoffSub,1) * mm2m + &
                                             LGSI_Qout(IRRRunoffSub,2) * LGSI_Area(iRRRunoffSub,2) * mm2m + &
                                              RRRunoffNode_Outflow(inr) * timeSettings%timestepSize
!                  RSLMAP8_bal(1,ILOC,1) = RSLMAP8_bal(1,ILOC,1) * timeSettings%timestepSize
!                  RSLMAP8_bal(2,ILOC,1) = RSLMAP8_bal(2,ILOC,1) * timeSettings%timestepSize
                  RSLMAP8_bal(3,ILOC,1) = DeltaVol
                elseif (RRRunoff_CompOption(Inr) .eq. 5 ) then   ! Wageningen
                  RSLMAP8_bal(1,ILOC,1) = WagMod_P(iRRRunoffSub) * Area_RRRunoffNode(inr) * mm2m
                  RSLMAP8_bal(2,ILOC,1) = WagMod_ETA(iRRRunoffSub) * Area_RRRunoffNode(inr) * mm2m + &
                                            Wagmod_Runoff(iRRRunoffSub) * timeSettings%timestepSize
                  if (Wagmod_Seep(iRRRunoffSub) .gt. 0) then
                     RSLMAP8_bal(1,ILOC,1) = RSLMAP8_bal(1,iloc,1) + WagMod_Seep(iRRRunoffSub) * Area_RRRunoffNode(inr) * mm2m * TimeSettings%TimestepSize / 3600.
                  else
                     RSLMAP8_bal(2,ILOC,1) = RSLMAP8_bal(2,iloc,1) - WagMod_Seep(iRRRunoffSub) * Area_RRRunoffNode(inr) * mm2m * TimeSettings%TimestepSize / 3600.
                  endif
                  RSLMAP8_bal(3,ILOC,1) = DeltaVol
                elseif (RRRunoff_CompOption(Inr) .eq. 6 ) then   ! Walrus
! to be adjusted
                  RSLMAP8_bal(1,ILOC,1) = Walrus_P(iRRRunoffSub) * Area_RRRunoffNode(inr) * mm2m
                  RSLMAP8_bal(2,ILOC,1) = Walrus_ETAct(iRRRunoffSub) * Area_RRRunoffNode(inr) * mm2m + &
                                            Walrus_lastQDis(iRRRunoffSub) * timeSettings%timestepSize
                  if (Walrus_FXGAct(IRRRunoffSub) .gt. 0) then
                     RSLMAP8_bal(1,ILOC,1) = RSLMAP8_bal(1,iloc,1) + Walrus_FXGAct(iRRRunoffSub) * Area_RRRunoffNode(inr) * mm2m * TimeSettings%TimestepSize / 3600.
                  else
                     RSLMAP8_bal(2,ILOC,1) = RSLMAP8_bal(2,iloc,1) - Walrus_FXGAct(iRRRunoffSub) * Area_RRRunoffNode(inr) * mm2m * TimeSettings%TimestepSize / 3600.
                  endif
                  if (Walrus_FXSAct(IRRRunoffSub) .gt. 0) then
                     RSLMAP8_bal(1,ILOC,1) = RSLMAP8_bal(1,iloc,1) + Walrus_FXSAct(iRRRunoffSub) * Area_RRRunoffNode(inr) * mm2m !* TimeSettings%TimestepSize / 3600.
                  else
                     RSLMAP8_bal(2,ILOC,1) = RSLMAP8_bal(2,iloc,1) - Walrus_FXSAct(iRRRunoffSub) * Area_RRRunoffNode(inr) * mm2m !* TimeSettings%TimestepSize / 3600.
                  endif
                  RSLMAP8_bal(3,ILOC,1) = DeltaVol
                endif
               Endif
!update cumulatieve balans 3B
               if (RRRunoff_CompOption(Inr) .eq. 0 ) then              ! EXTR
                 Bal3B (28) = Bal3B (28) + RRRunoffNode_Outflow(inr) * timeSettings%timestepSize
                 Bal3B (29) = Bal3B (29) + 0.0
                elseif (RRRunoff_CompOption(Inr) .eq. 1 ) then         ! HBV
                 Bal3B ( 2) = Bal3B ( 2) + (HBV_Rainfall(iRRRunoffSub) +HBV_SnowFall(IRRRunoffSub) ) * Area_RRRunoffNode(inr) * mm2m
                 Bal3B (30) = Bal3B (30) + HBV_ActEvap(iRRRunoffSub) * Area_RRRunoffNode(inr) * mm2m
                 Bal3B (29) = Bal3B (29) + DeltaVol
                elseif (RRRunoff_CompOption(Inr) .eq. 2 ) then         ! SCS
                 Bal3B ( 2) = Bal3B ( 2) + SCS_Rainfall(iRRRunoffSub) * Area_RRRunoffNode(inr) * mm2m
                 Bal3B (29) = Bal3B (29) + DeltaVol
                elseif (RRRunoff_CompOption(Inr) .eq. 3 ) then         ! NAM
                 Bal3B ( 2) = Bal3B ( 2) + NAMRainfall(iRRRunoffSub) * Area_RRRunoffNode(inr) * mm2m
                 ! add pumpflow, positive = pump in, negative = outflow
                 if (NAM_GWPumpAct(iRRRunoffSub) .lt. 0) then
                     Bal3B (28) = Bal3B (28) - NAM_GWPumpAct(iRRRunoffSub) * timeSettings%timestepSize
                 elseif (NAM_GWPumpAct(iRRRunoffSub) .gt. 0) then
                     Bal3B (19) = Bal3B (19) + NAM_GWPumpAct(iRRRunoffSub) * timeSettings%timestepSize
                 endif
                 Bal3B (30) = Bal3B (30) + NAMActEvapTot(iRRRunoffSub) * Area_RRRunoffNode(inr) * mm2m
                 Bal3B (29) = Bal3B (29) + DeltaVol
                elseif (RRRunoff_CompOption(Inr) .eq. 4 ) then         ! LGSI
! adjusted DeltaVol2 computation 18Oct2013
                 DeltaVol2 = 0
!                DeltaVol2 = ( LGSI_NewVolume(Inr,1) - LGSI_InitVolume(Inr,1) ) * LGSI_Area(inr,1)
!                If (LGSI_NrSubAreas(Inr) .eq. 2) DeltaVol2 = DeltaVol2 + ( LGSI_NewVolume(Inr,2) - LGSI_InitVolume(Inr,2) ) * LGSI_Area(inr,2)
!                DeltaVol2 = (LGSI_HistoryQTot(inr,1) - LGSI_HistoryQDelayed(inr,1)) * Area_RRRunoffNode(Inr) * mm2m
                 Bal3B ( 2) = Bal3B ( 2) + LGSI_Precipitation(iRRRunoffSub,1) * LGSI_Area(iRRRunoffSub,1) * mm2m
                 Bal3B (30) = Bal3B (30) + LGSI_Evaporation(iRRRunoffSub,1) * LGSI_Area(iRRRunoffSub,1) * mm2m
                 Bal3B (29) = Bal3B (29) + DeltaVol - DeltaVol2
                 Bal3B (19) = Bal3B (19) + LGSI_Qout(IRRRunoffSub,1) * LGSI_Area(iRRRunoffSub,1) * mm2m
                 Bal3B (21) = Bal3B (21) + DeltaVol2
                 If (LGSI_NrSubAreas(IRRRunoffSub) .eq. 2) then
                     Bal3B ( 2) = Bal3B ( 2) + LGSI_Precipitation(iRRRunoffSub,2) * LGSI_Area(iRRRunoffSub,2) * mm2m
                     Bal3B (30) = Bal3B (30) + LGSI_Evaporation(iRRRunoffSub,2) * LGSI_Area(iRRRunoffSub,2) * mm2m
                     Bal3B (19) = Bal3B (19) + LGSI_Qout(IRRRunoffSub,2) * LGSI_Area(iRRRunoffSub,2) * mm2m
                 Endif
                elseif (RRRunoff_CompOption(Inr) .eq. 5 ) then         ! Wageningen
                  Bal3B ( 2) = Bal3B ( 2) + WagMod_P(iRRRunoffSub) * Area_RRRunoffNode(inr) * mm2m
                  Bal3B (30) = Bal3B (30) + WagMod_ETA(iRRRunoffSub) * Area_RRRunoffNode(inr) * mm2m
                  Bal3B (29) = Bal3B (29) + DeltaVol - TempVol
                  Bal3B (21) = Bal3B (21) + TempVol
                  Bal3B (19) = Bal3B (19) - WagMod_Seep(iRRRunoffSub) * Area_RRRunoffNode(inr) * mm2m * TimeSettings%TimestepSize / 3600.
                elseif (RRRunoff_CompOption(Inr) .eq. 6 ) then         ! Walrus
! to be added
                  Bal3B ( 2) = Bal3B ( 2) + Walrus_P(iRRRunoffSub) * Area_RRRunoffNode(inr) * mm2m
                  Bal3B (30) = Bal3B (30) + Walrus_ETact(iRRRunoffSub) * Area_RRRunoffNode(inr) * mm2m
                  Bal3B (29) = Bal3B (29) + DeltaVol
                  Bal3B (19) = Bal3B (19) - (Walrus_FXGAct(iRRRunoffSub)+Walrus_FXSAct(iRRRunoffSub)) &
                                              * Area_RRRunoffNode(inr) * mm2m !* TimeSettings%TimestepSize / 3600.
                endif

!zout Map: niet van toepassing voor RR Runoff
               IF (ISLCMP .ne. 0) THEN
                 ILOC = INODE
                 RSLMAP9_slt(1,ILOC,1) = 0.
               ENDIF
            ELSEIF (IKIND .EQ. 21) THEN          ! open water PRECIPITATION / EVAP ONLY
! rain, evap, seepage (all in m3/s)
!End April 2002
!              balans uitvoer in map NKIND+1
               ILOC = INODE
               QIN = VOWRain(inr)
               QOUT= RowRain(inr)
!check: correctie voor negatieve debieten: moet dit wel of niet? oorspronkelijk niet aanwezig
              IF (QOUT .LT. 0) THEN
                  QIN = QIN - QOUT
                  QOUT= 0.
              ENDIF
! end
               ILOC = INODE
               If (ExtendedBalanceOutput) then
                 RSLMAP8_bal(1,ILOC,1) = ROWRain(INR) * timeSettings%timestepSize  ! ROWRain in m3/s, not in m3
                 RSLMAP8_bal(2,ILOC,1) = QIN*timeSettings%timestepSize
                 RSLMAP8_bal(3,ILOC,1) = VOWRain(INR) * timeSettings%timestepSize
                 RSLMAP8_bal(4,ILOC,1) = QOUT*timeSettings%timestepSize
                 RSLMAP8_bal(5,ILOC,1) = 0.0
                 RSLMAP8_bal(6,ILOC,1) = RSLMAP8_bal(1,ILOC,1) +  RSLMAP8_bal(2,ILOC,1) - &
                                           RSLMAP8_bal(3,ILOC,1) - RSLMAP8_bal(4,ILOC,1) - &
                                             RSLMAP8_bal(5,ILOC,1)
               Else
                 RSLMAP8_bal(1,ILOC,1) = (ROWRain(INR) + QIN ) * timeSettings%timestepSize
                 RSLMAP8_bal(2,ILOC,1) = (VOWRain(INR) + QOUT) * timeSettings%timestepSize
                 RSLMAP8_bal(3,ILOC,1) = 0.0
               Endif

!              zout uitvoer in map NKIND+2   (if net rain, = salt conc rainfall; if net evap, = saltconc of related node!!
!              to be updated
!               IF (ISLCMP .ne. 0) THEN
!                 ILOC = INODE
!                 ISLT = INDSLT(INODE)
!                 RSLMAP9_slt(1,ILOC,1) = SALTF(ISLT)
!               ENDIF
!update cumulatieve balans 3B
               Bal3B (2) = Bal3B (2) + ROWRain(inr) * timeSettings%timestepSize
               Bal3B (6) = Bal3B (6) + VOWRain(inr) * timeSettings%timestepSize
               Bal3B (12) = Bal3B (12)

            ELSEIF (IKIND .EQ. 30) THEN            ! RR Connection nodes
               ILOC = INODE
               If (ExtendedBalanceOutput) then
                 RSLMAP8_bal(1,ILOC,1) = 0.0
                 RSLMAP8_bal(2,ILOC,1) = QConn(inr) * TimeSettings%TimestepSize
                 RSLMAP8_bal(3,ILOC,1) = 0.0
                 RSLMAP8_bal(4,ILOC,1) = QConn(inr) * TimeSettings%TimestepSize
                 RSLMAP8_bal(5,ILOC,1) = 0.0
                 RSLMAP8_bal(6,ILOC,1) = 0.0
               Else
                 RSLMAP8_bal(1,ILOC,1) = QConn(inr) * TimeSettings%TimestepSize
                 RSLMAP8_bal(2,ILOC,1) = Qconn(inr) * TimeSettings%TimestepSize
                 RSLMAP8_bal(3,ILOC,1) = 0.0
               Endif

            ELSEIF (IKIND .EQ. 32) THEN            ! RR Bifurcation nodes
               ILOC = INODE
               If (ExtendedBalanceOutput) then
                 RSLMAP8_bal(1,ILOC,1) = 0.0
                 RSLMAP8_bal(2,ILOC,1) = (QBifur(inr)* TimeSettings%TimestepSize)
                 RSLMAP8_bal(3,ILOC,1) = 0.0
                 RSLMAP8_bal(4,ILOC,1) = (QBifur(inr)* TimeSettings%TimestepSize)
                 RSLMAP8_bal(5,ILOC,1) = 0.0
                 RSLMAP8_bal(6,ILOC,1) = 0.0
               Else
                 RSLMAP8_bal(1,ILOC,1) = (QBifur(inr) * TimeSettings%TimestepSize)
                 RSLMAP8_bal(2,ILOC,1) = (QBifur(inr) * TimeSettings%TimestepSize)
                 RSLMAP8_bal(3,ILOC,1) = 0.0
               Endif

            ENDIF
!Vullen cumulatieve balans voor CumBaldt.HIS file
            ILOC = INODE
            If (ExtendedBalanceOutput) then
              RSLMAP8_bal( 7,ILOC,1) = RSLMAP8_bal( 7,ILOC,1) + RSLMAP8_bal(1,ILOC,1)
              RSLMAP8_bal( 8,ILOC,1) = RSLMAP8_bal( 8,ILOC,1) + RSLMAP8_bal(2,ILOC,1)
              RSLMAP8_bal( 9,ILOC,1) = RSLMAP8_bal( 9,ILOC,1) + RSLMAP8_bal(3,ILOC,1)
              RSLMAP8_bal(10,ILOC,1) = RSLMAP8_bal(10,ILOC,1) + RSLMAP8_bal(4,ILOC,1)
              RSLMAP8_bal(11,ILOC,1) = RSLMAP8_bal(11,ILOC,1) + RSLMAP8_bal(5,ILOC,1)
              RSLMAP8_bal(12,ILOC,1) = RSLMAP8_bal(12,ILOC,1) + RSLMAP8_bal(6,ILOC,1)
            Else
              RSLMAP8_bal( 4,ILOC,1) = RSLMAP8_bal( 4,ILOC,1) + RSLMAP8_bal(1,ILOC,1)
              RSLMAP8_bal( 5,ILOC,1) = RSLMAP8_bal( 5,ILOC,1) + RSLMAP8_bal(2,ILOC,1)
              RSLMAP8_bal( 6,ILOC,1) = RSLMAP8_bal( 6,ILOC,1) + RSLMAP8_bal(3,ILOC,1)
            Endif

         ENDDO ! vullen van de RSLMAP


! Uitvoer voor LinkFlows, detail; altijd in m3/s
         Do Ilink=1,NcLink

            if (idebug .ne. 0) write(idebug,*) ' Link nr', ilink
            Inode1 = LnkFrm(ilink)
            IKIND1 = EiNode(INODE1,3)
            INR    = EiNode(INODE1,2)

            Inode2 = LnkTo (ilink)
            IKIND2 = EiNode(INODE2,3)
            if (idebug .ne. 0) write(idebug,*) ' inode1 inr inode2 ', inode1, inr, inode2
            if (idebug .ne. 0) write(idebug,*) ' ikind1 ikind2     ', ikind1, ikind2

            if (ikind1.eq. 1) then ! upstream is verhard
              IOW = EIOW(INODE1)
              IBND = EIBND(INODE1)
              IPluv = EIPluv(INODE1)
              IRWZI = EIRWZI(INODE1)
              QVhgRwzi = 0.0
              QVhgBnd  = 0.0
              QVhgOW   = 0.0
              QVhgPluv = 0.0
              if (iow .gt. 0) then
                 QVhgOW = Q1V(INR,1) + Q1V(INR,2)
              elseif (ibnd .gt. 0) then
                 QVhgBnd  = Q1V(INR,1) + Q1V(INR,2)
              elseif (ipluv .gt. 0) then
                 QvhgPluv = Q1V(INR,1) + Q1V(INR,2)
              elseif (irwzi .gt. 0) then
                 QvhgRwzi = Q1V(INR,1) + Q1V(INR,2)
              else
                 call SetMessage(LEVEL_FATAL, 'Sobek_3B error output link flows')
              endif
              If (sewer(iNr)%Q2VOW(1) .EQ. 1) Then
                  QVhgOW  = QVhgOW + Q2V(INR,1)
              ElseIf (sewer(iNr)%Q2VOW(1) .EQ. 0) Then
                  QVhgBnd = QVhgBnd + Q2V(INR,1)
              ElseIf (sewer(iNr)%Q2VOW(1) .EQ. 2) Then
                  QVhgRwzi= QVhgRwzi + Q2V(INR,1)
              ElseIf (sewer(iNr)%Q2VOW(1) .EQ. 3) Then
                  QVhgPluv= QVhgPluv + Q2V(INR,1)
              Endif
              If (sewer(iNr)%Q2VOW(2) .EQ. 1) Then
                  QVhgOW  = QVhgOW + Q2V(INR,2)
              ElseIf (sewer(iNr)%Q2VOW(2) .EQ. 0) Then
                  QVhgBnd = QVhgBnd + Q2V(INR,2)
              ElseIf (sewer(iNr)%Q2VOW(2) .EQ. 2) Then
                  QVhgRwzi= QVhgRwzi + Q2V(INR,2)
              ElseIf (sewer(iNr)%Q2VOW(2) .EQ. 3) Then
                  QVhgPluv= QVhgPluv + Q2V(INR,2)
              Endif

              if (ikind2 .eq. 4) then ! verhard - open water
               RSLMAP16_Flows(1,Ilink,1) = QVhgOW
              elseif (ikind2 .eq. 6) then ! verhard - boundary
               RSLMAP16_Flows(1,Ilink,1) = QVhgBnd
              elseif (ikind2 .eq. 7) then ! verhard - NWRW
               RSLMAP16_Flows(1,Ilink,1) = QVhgPluv
              elseif (ikind2 .eq. 14) then ! verhard - RWZI
               RSLMAP16_Flows(1,Ilink,1) = QVhgRwzi
              endif
            elseif (ikind1 .eq. 2) then ! upstream is onverhard
               If (LinkType(Ilink) .eq. 21 .or. LinkType(Ilink) .eq. 22) then
                  ! on this link type only surface runoff
                  RSLMAP16_Flows(1,Ilink,1) = Q1O(INR)
               Elseif (LinkType(Ilink) .eq. 31) then
                  ! groundwater link
                  RSLMAP16_Flows(1,Ilink,1) = QinLink(ilink)
               Else
                  If (.not. SWLinkFromExists(INode1)) then
                     ! for index nodenr Inode1 there is no downstrream swlink, so all flow over normal link
                     RSLMAP16_Flows(1,Ilink,1) = Q1O(INR) + Q2O(INR)
                  Else
                     ! for index inr=iovh there is an sw link, so only drainage flow here
                     RSLMAP16_Flows(1,Ilink,1) =  Q2O(INR)
                  Endif
                  ! add irrigation from surface water,  July 2010
                  If (IrrigationSource(inr) .eq. 1)  then
                     if (idebug .ne. 0) write(Idebug,*) ' OutputFlows -IrrDemand', IrrigationDemand(inr)
                     RSLMAP16_Flows(1,Ilink,1) = RSLMAP16_FLOWS(1,ILink,1) - IrrigationDemand(inr)
                  endif
               Endif
            elseif (ikind1 .eq. 3) then  ! upstream is kas
               RSLMAP16_Flows(1,ILink,1) = RSLMAP3_kas(2,INR,1)
            elseif (ikind1 .eq. 5) then ! upstream is kunstwerk
               RSLMAP16_flows(1,ILink,1) = QSTRU(INR)
            elseif (ikind2 .eq. 5) then ! downstream is kunstwerk
               INR    = EiNode(INODE2,2)
               RSLMAP16_flows(1,ILink,1) =  QSTRU(INR)
            elseif (ikind1 .eq. 14) then ! upstream is RWZI
               RSLMAP16_flows(1,ILink,1) = QEffluent(INR)
            elseif (ikind1 .eq. 15) then ! upstream is Industry
! ars 6361
!              RSLMAP16_Flows(2,1,ILink,1) = -QIndDem(INR) + QIndDis(inr)
! Nov 2001: actual return flow, may be less then input data if allocation is less than demand)
               RSLMAP16_Flows(1,ILink,1) = QIndReturnFlow(inr)
            elseif (ikind2 .eq. 15) then ! downstream is Industry
               INR    = EiNode(INODE2,2)
! ars 6361
!              RSLMAP16_Flows(2,1,ILink,1) = QIndDem(INR) - QIndDis(inr)
! Nov 2001: actual allocation instead of demand (allocation may be less than demand)
               RSLMAP16_Flows(1,ILink,1) = QIndAll(INR)
            elseif (ikind1 .eq. 16) then ! upstream is Sacramento
               RSLMAP16_Flows(1,ILink,1) = SacChannelInflow(inr)
            elseif (ikind1 .eq. 17) then ! upstream is Cell
!               RSLMAP16_Flows(1,ILink,1) = CellData(icel)%TotalCellOutflowToExternal / TimeSettings%TimestepSize
            elseif ( ikind1 .eq. 18 .or. ikind1 .eq. 19 .or. ikind1 .eq. 20 .or. &
                     ikind1 .eq. 22 .or. ikind1 .eq. 23 .or. ikind1 .eq. 31 ) then ! upstream is RR_Runoff
               RSLMAP16_Flows(1,ILink,1) = RRRunoffNode_Outflow(inr)
            elseif (ikind1 .eq. 21) then
               RSLMAP16_Flows(1,ILink,1) = RowRain(inr) - VowRain(inr)
               if (RoutingLinkExists) then
                  RSLMAP16_Flows(4,ILink,1) = RowRain(inr)
                  RSLMAP16_Flows(5,ILink,1) = VowRain(inr)
               else
                  RSLMAP16_Flows(2,ILink,1) = RowRain(inr)
                  RSLMAP16_Flows(3,ILink,1) = VowRain(inr)
               endif
            elseif (ikind1 .eq. 30) then ! upstream is Connection
               RSLMAP16_Flows(1,ILink,1) = Qinlink(ilink)
            elseif (ikind1 .eq. 32) then ! upstream is Bifurcation
               RSLMAP16_Flows(1,ILink,1) = Qinlink(ilink)
            endif

            ! Routing links
            If (LinkType(ilink) .eq. 30) then
               ! er is minstens 1 routing link, dus indices 2 en 3 in het array zijn gereserveerd voor de routing link inflow/outflow
               RSLMAP16_Flows(1,Ilink,1) = Qinlink(ilink)
               RSLMAP16_Flows(2,Ilink,1) = Qinlink(ilink)
               RSLMAP16_Flows(3,Ilink,1) = Qoutlink(ilink)
               Bal3B (23) = Bal3B (23) + (Qinlink(ilink) - Qoutlink(ilink)) * Timesettings%TimestepSize
            Else
               if (RoutingLinkExists) then
                  ! als er 1 of meer RoutingLinks bestaan, dan ook uitvoer voor gewone links in de variabelen 2 en 3 zetten
                  RSLMAP16_Flows(2,Ilink,1) = RslMap16_flows(1,Ilink,1)
                  RSLMAP16_Flows(3,Ilink,1) = RslMap16_flows(1,Ilink,1)
               Endif
            Endif
         Enddo

! store output in Tnul arrays
         Do Ilink=1,NcLink
            Do j=1,MaxSeriesPerMap(13)
               QLink(ilink,j) = RSLMAP16_Flows(j,ilink,1)
            Enddo
         Enddo

         Do iloc=1,ncvhg
            Do j=1,MaxSeriesPerMap(1)
               VHG_Tnul(j,iloc)  = RSLMAP1_vhg (j,ILOC,1)
            Enddo
         Enddo
         Do iloc=1,ncovhg
            Do j=1,MaxSeriesPerMap(2)
               OVH_Tnul(j,iloc) = RSLMAP2_ovh (j,ILOC,1)
            Enddo
         Enddo
         Do iloc=1,nckas
            Do j=1,MaxSeriesPerMap(3)
               KAS_Tnul(j,iloc) = RSLMAP3_Kas (j,ILOC,1)
            Enddo
         Enddo
         Do iloc=1,ncow
            Do j=1,MaxSeriesPerMap(4)
               OW_Tnul(j,iloc)  = RSLMAP4_Ow  (j,ILOC,1)
            Enddo
         Enddo
         Do iloc=1,ncStru
            Do j=1,MaxSeriesPerMap(5)
               STR_Tnul(j,iloc) = RSLMAP5_STR (j,ILOC,1)
            Enddo
         Enddo
         Do iloc=1,ncboun
            Do j=1,MaxSeriesPerMap(6)
               BND_Tnul(j,iloc) = RSLMAP6_bnd (j,ILOC,1)
            Enddo
         Enddo
         Do iloc=1,ncpluv
            Do j=1,MaxSeriesPerMap(7)
               PLV_Tnul(j,iloc) = RSLMAP7_PLV (j,ILOC,1)
            Enddo
         Enddo

         ! check salt is on
         IF (ISLCMP .NE. 0) THEN
! 14 jan 2013  NcNode ipv NcSalt
             Do iloc=1,ncnode
                Do j=1,MaxSeriesPerMap(9)
                   SLT_Tnul(j,iloc) = RSLMAP9_SLT (j,ILOC,1)
                Enddo
             Enddo
         ELSE
            Do j=1,MaxSeriesPerMap(9)
                SLT_Tnul(j,1) = RSLMAP9_SLT (j,1,1)
            Enddo
         ENDIF

         Do iloc=1,ncrwzi
            Do j=1,MaxSeriesPerMap(10)
               RWZI_Tnul(j,iloc) = RSLMAP14_RWZI(j,ILOC,1)
            Enddo
         Enddo
         Do iloc=1,ncIndus
            Do j=1,MaxSeriesPerMap(11)
               IND_Tnul(j,iloc) = RSLMAP15_Ind (j,ILOC,1)
            Enddo
         Enddo
         Do iloc=1,ncSacr
            Do j=1,MaxSeriesPerMap(12)
               SACR_Tnul(j,iloc) = RSLMAP17_Sacr (j,ILOC,1)
            Enddo
         Enddo
         Do iloc=1,ncRRRunoff
            Do j=1,MaxSeriesPerMap(15)
               RRRunoff_Tnul(j,iloc) = RSLMAP19_RRRunoff (j,ILOC,1)
            Enddo
         Enddo


         if (idebug .ne. 0) write(idebug,*) ' before WrtOutStoreSummary'
         Call WrtOutStoreSummary (Ievent, UpdateDailyValues)
         if (idebug .ne. 0) write(idebug,*) ' after WrtOutStoreSummary'
         UpdateDailyValues = .false.

      RETURN
      END Subroutine WrTOut



      Subroutine WrtOutStoreSummary(Ievent, UpdateDailyValues)


! *********************************************************************
! *** Store output (summary for each event)
! *********************************************************************
    Implicit none

    Integer iEvent, iNode, iKind, iNr, iLoc, iKKl
    Integer iNod, iVhg
    Integer iOvh, iKas, iOW, iStr, teller
!   Integer Nodeup, NodeDown, kind, inr2
    Real    area, qOw, rKwel, bTot, bTot0, bMax, RHelp
    Real    qTot, vKas, UKas, rainT, qIn, qOut, rKwl, rWegz
!   Real    rVolOp, rVolO0, rVolDN, rInRi,rInFa, rVolD0, rInFd,
    Real    qSum, bSum
    Integer iDebug
    Logical UpdateDailyValues

    Integer Ilink
!    Integer Inode1, Inode2, IKind1, IKind2
!    Real    QvhgOw, QVhgBnd, QVhgRwzi
    Real    EvapT, UseT
!

      iDebug = ConfFil_get_iDebug()

! Store Uitvoer voor LinkFlows, maxima
      Do Ilink=1,NcLink
         QLinkMX(ilink,Ievent) = MAX (QLinkMx(ilink,Ievent), RSLMAP16_Flows(1,ILink,1) )
      Enddo
!
      DO INODE=1,NCNODE
         IKIND = EiNode(INODE,3)
         INR   = EiNode(INODE,2)
 !       IMAP  = NKIND+1
         ILOC  = INODE
         if (idebug .ne. 0) write(idebug,*) ' ikind inr iloc ', ikind, inr, iloc
         IF  (IKIND .EQ. 1) THEN     ! verhard gebied
           If (ExtendedBalanceOutput) then
            CUMRSLMAP8_bal(1,ILOC,1) = CUMRSLMAP8_bal(1,ILOC,1) + RV(INR) + DWAPaved(inr) * timeSettings%timestepSize
            CUMRSLMAP8_bal(2,ILOC,1) = 0.0
            CUMRSLMAP8_bal(3,ILOC,1) = CUMRSLMAP8_bal(3,ILOC,1) + VV(INR)
            CUMRSLMAP8_bal(4,ILOC,1) = CUMRSLMAP8_bal(4,ILOC,1) + &
                   timeSettings%timestepSize * (Q1V(INR,1)+Q2V(INR,1)+ Q1V(INR,2)+Q2V(INR,2))
            CUMRSLMAP8_bal(5,ILOC,1) = CUMRSLMAP8_bal(5,ILOC,1) +   &
                  BVRL(INR,1)+ BVRL(INR,2) +BVSTR(INR) - BVRL0(INR,1) -BVRL0(INR,2)- BVSTR0(INR)
           Else
            CUMRSLMAP8_bal(1,ILOC,1) = CUMRSLMAP8_bal(1,ILOC,1) + RV(INR) + DWAPaved(inr) * timeSettings%timestepSize
            CUMRSLMAP8_bal(2,ILOC,1) = CUMRSLMAP8_bal(2,ILOC,1) + VV(INR) +   &
                   timeSettings%timestepSize * (Q1V(INR,1)+Q2V(INR,1)+ Q1V(INR,2)+Q2V(INR,2))
            CUMRSLMAP8_bal(3,ILOC,1) = CUMRSLMAP8_bal(3,ILOC,1) +   &
                  BVRL(INR,1)+ BVRL(INR,2) +BVSTR(INR) - BVRL0(INR,1) -BVRL0(INR,2)- BVSTR0(INR)
           Endif
         ELSEIF (IKIND .EQ. 2)  THEN       ! onverhard gebied
           RKWEL = KWEL(INR)-WEGZG(INR)
           If (ExtendedBalanceOutput) then
            CUMRSLMAP8_bal(1,ILOC,1) = CUMRSLMAP8_bal(1,ILOC,1) + RO(INR) + &
                         RKWEL*AreaGwComp(INR)*timeSettings%timestepSize + IrrigationSupply(inr) * TimeSettings%TimestepSize
            CUMRSLMAP8_bal(2,ILOC,1) = 0.0
            CUMRSLMAP8_bal(3,ILOC,1) = CUMRSLMAP8_bal(3,ILOC,1) + VO(INR) + VBO(INR)
            CUMRSLMAP8_bal(4,ILOC,1) = CUMRSLMAP8_bal(4,ILOC,1) + &
                                          timeSettings%timestepSize * (Q1O(INR)+Q2O(INR))
            If (IrrigationSource(inr) .eq. 1) CUMRSLMAP8_bal(4,ILOC,1) = CUMRSLMAP8_bal(4,iloc,1) - IrrigationDemand(inr)* TimeSettings%TimestepSize
            CUMRSLMAP8_bal(5,ILOC,1) = CUMRSLMAP8_bal(5,ILOC,1) +   &
                                      BOLND(INR) + BOBD(INR) - BOLND0(INR) - BOBD0(INR)  &
                                         + OnvZone(INR)%Actual_Volume - OnvZone(INR)%Init_Volume
           Else
            CUMRSLMAP8_bal(1,ILOC,1) = CUMRSLMAP8_bal(1,ILOC,1) + RO(INR) + &
                                      RKWEL*AreaGwComp(INR)*timeSettings%timestepSize + IrrigationSupply(inr) * TimeSettings%TimestepSize
            CUMRSLMAP8_bal(2,ILOC,1) = CUMRSLMAP8_bal(2,ILOC,1) + VO(INR) + &
                                      VBO(INR) + timeSettings%timestepSize * (Q1O(INR)+Q2O(INR))
            If (IrrigationSource(inr) .eq. 1) CUMRSLMAP8_bal(2,ILOC,1) = CUMRSLMAP8_bal(2,iloc,1) - IrrigationDemand(inr)* TimeSettings%TimestepSize
            CUMRSLMAP8_bal(3,ILOC,1) = CUMRSLMAP8_bal(3,ILOC,1) +   &
                                      BOLND(INR) + BOBD(INR) - BOLND0(INR) - BOBD0(INR)  &
                                        + OnvZone(INR)%Actual_Volume - OnvZone(INR)%Init_Volume
           Endif
         ELSEIF (IKIND .EQ. 3) THEN       ! kasgebied
! VKas, RainT was niet bepaald !! April 2000
           BTOT = 0.
           BTOT0= 0.
           BMAX = 0.
           QTOT = 0.
           VKAS = 0.
           UKAS = 0.
           AREA = 0.
           RAINT= RKD(INR)
           DO IKKL=1,NCKKL
              BMAX = BMAX + KKLMXB(IKKL) * AREAKK(INR,IKKL)
              BTOT = BTOT + BKAS(INR,IKKL)
              BTOT0= BTOT0+ BKAS0(INR,IKKL)
              VKAS = VKAS + GEBR (INR,IKKL) + VKB(INR,IKKL)
              UKAS = UKAS + GEBR (INR,IKKL)
              QTOT = QTOT + QKAS(INR,IKKL)
              AREA = AREA + AREAKK(INR,IKKL)
              RAINT= RAINT + RKB(INR,IKKL)
           ENDDO
           BMAX = BMAX + SILOC(INR) * AREAS(INR)
           BTOT = BTOT + SILOB(INR)
           BTOT0= BTOT0 + SILOB0(INR)
           VKAS = VKAS + QSILGW(INR)*timeSettings%timestepSize
           QTOT = QTOT + QSILOW(INR)
           AREA = AREA + AREAS (INR)
! end bepaling VKas etc.
           If (ExtendedBalanceOutput) then
            CUMRSLMAP8_bal(1,ILOC,1) = CUMRSLMAP8_bal(1,ILOC,1) + RAINT
            CUMRSLMAP8_bal(2,ILOC,1) = 0.0
            CUMRSLMAP8_bal(3,ILOC,1) = CUMRSLMAP8_bal(3,ILOC,1) + VKD(INR) +  VKAS
            CUMRSLMAP8_bal(4,ILOC,1) = CUMRSLMAP8_bal(4,ILOC,1) + timeSettings%timestepSize * QTOT
            CUMRSLMAP8_bal(5,ILOC,1) = CUMRSLMAP8_bal(5,ILOC,1) + BTOT- BTOT0 + BKASD(INR)-BKASD0(INR)
           Else
            CUMRSLMAP8_bal(1,ILOC,1) = CUMRSLMAP8_bal(1,ILOC,1) + RAINT
            CUMRSLMAP8_bal(2,ILOC,1) = CUMRSLMAP8_bal(2,ILOC,1) + VKD(INR) +  &
                              VKAS + timeSettings%timestepSize * QTOT
            CUMRSLMAP8_bal(3,ILOC,1) = CUMRSLMAP8_bal(3,ILOC,1) + BTOT- BTOT0 + BKASD(INR)-BKASD0(INR)
           Endif
         ELSEIF (IKIND .EQ. 4) THEN       ! open water
! met kwel/wegzijging op open water:
           RKWL = 0.0
           RWEGZ= 0.0
           IF (KWOW(INR) .GT. 0.0) RKWL  = KWOW(INR)
           IF (KWOW(INR) .LT. 0.0) RWEGZ = -1 * KWOW(INR)
           QIN = QINOW(INR,1) + QIN0(INR,2) + QINOW(INR,3) + QIN0(INR,4) + QinOW(INR,5) + Qin0(Inr,6) + QinOw(Inr,7)
           QOUT= QOUT0(INR)
           If (ExtendedBalanceOutput) then
            CUMRSLMAP8_bal(1,ILOC,1) = CUMRSLMAP8_bal(1,ILOC,1) + ROW(INR) + RKWL
            CUMRSLMAP8_bal(2,ILOC,1) = CUMRSLMAP8_bal(2,ILOC,1) + QIN*timeSettings%timestepSize
            CUMRSLMAP8_bal(3,ILOC,1) = CUMRSLMAP8_bal(3,ILOC,1) + VOW(INR) + RWEGZ
            CUMRSLMAP8_bal(4,ILOC,1) = CUMRSLMAP8_bal(4,ILOC,1) + QOUT*timeSettings%timestepSize
            CUMRSLMAP8_bal(5,ILOC,1) = CUMRSLMAP8_bal(5,ILOC,1) + VOLOW(INR) - VOLOW0(INR) + &
                                       ActualExtraBergendVolume(inr) - PreviousExtraBergendVolume(inr)
           Else
            CUMRSLMAP8_bal(1,ILOC,1) = CUMRSLMAP8_bal(1,ILOC,1) + ROW(INR) + RKWL + QIN*timeSettings%timestepSize
            CUMRSLMAP8_bal(2,ILOC,1) = CUMRSLMAP8_bal(2,ILOC,1) + VOW(INR) + RWEGZ + QOUT*timeSettings%timestepSize
            CUMRSLMAP8_bal(3,ILOC,1) = CUMRSLMAP8_bal(3,ILOC,1) + VOLOW(INR) - VOLOW0(INR) + &
                                         ActualExtraBergendVolume(inr) - PreviousExtraBergendVolume(inr)
           Endif
         ELSEIF (IKIND .EQ. 5) THEN     ! kunstwerk
           If (ExtendedBalanceOutput) then
            CUMRSLMAP8_bal(1,ILOC,1) = 0.0
            CUMRSLMAP8_bal(2,ILOC,1) = CUMRSLMAP8_bal(2,ILOC,1) + QSTRU(INR)*timeSettings%timestepSize
            CUMRSLMAP8_bal(3,ILOC,1) = 0.0
            CUMRSLMAP8_bal(4,ILOC,1) = CUMRSLMAP8_bal(4,ILOC,1) + QSTRU(INR)*timeSettings%timestepSize
            CUMRSLMAP8_bal(5,ILOC,1) = 0.0
           Else
            CUMRSLMAP8_bal(1,ILOC,1) = CUMRSLMAP8_bal(1,ILOC,1) + QSTRU(INR)*timeSettings%timestepSize
            CUMRSLMAP8_bal(2,ILOC,1) = CUMRSLMAP8_bal(2,ILOC,1) + QSTRU(INR)*timeSettings%timestepSize
            CUMRSLMAP8_bal(3,ILOC,1) = 0.0
           Endif
         ELSEIF  (IKIND .EQ. 6) THEN     ! boundary
           If (ExtendedBalanceOutput) then
            CUMRSLMAP8_bal(1,ILOC,1) = 0.0
            CUMRSLMAP8_bal(2,ILOC,1) = CUMRSLMAP8_bal(2,ILOC,1) + QBND(INR)*timeSettings%timestepSize
            CUMRSLMAP8_bal(3,ILOC,1) = 0.0
            CUMRSLMAP8_bal(4,ILOC,1) = CUMRSLMAP8_bal(4,ILOC,1) + QBND(INR)*timeSettings%timestepSize
            CUMRSLMAP8_bal(5,ILOC,1) = 0.0
           Else
            CUMRSLMAP8_bal(1,ILOC,1) = CUMRSLMAP8_bal(1,ILOC,1) + QBND(INR)*timeSettings%timestepSize
            CUMRSLMAP8_bal(2,ILOC,1) = CUMRSLMAP8_bal(2,ILOC,1) + QBND(INR)*timeSettings%timestepSize
            CUMRSLMAP8_bal(3,ILOC,1) = 0.0
           Endif
         ELSEIF (IKIND .EQ. 7) THEN
! correctie indices laatste kolom april 2001 GP
            CUMRSLMAP8_bal(1,ILOC,1) = CUMRSLMAP8_bal(1,ILOC,1) +  RSLMAP8_bal(1,ILOC,1)
            CUMRSLMAP8_bal(2,ILOC,1) = CUMRSLMAP8_bal(2,ILOC,1) +  RSLMAP8_bal(2,ILOC,1)
            CUMRSLMAP8_bal(3,ILOC,1) = CUMRSLMAP8_bal(3,ILOC,1) +  RSLMAP8_bal(3,ILOC,1)
            CUMRSLMAP8_bal(4,ILOC,1) = CUMRSLMAP8_bal(4,ILOC,1) +  RSLMAP8_bal(4,ILOC,1)
            CUMRSLMAP8_bal(5,ILOC,1) = CUMRSLMAP8_bal(5,ILOC,1) +  RSLMAP8_bal(5,ILOC,1)
         ELSEIF  (IKIND .EQ. 14) THEN     ! RWZI
           If (ExtendedBalanceOutput) then
            CUMRSLMAP8_bal(1,ILOC,1) = 0.0
            CUMRSLMAP8_bal(2,ILOC,1) = CUMRSLMAP8_bal(2,ILOC,1) +  QRWZI(INR)*timeSettings%timestepSize
            CUMRSLMAP8_bal(3,ILOC,1) = 0.0
            CUMRSLMAP8_bal(4,ILOC,1) = CUMRSLMAP8_bal(4,ILOC,1) +  QEffluent(INR)*timeSettings%timestepSize
            CUMRSLMAP8_bal(5,ILOC,1) = CUMRSLMAP8_bal(5,ILOC,1) +  RSLMAP8_bal(5,ILOC,1)
           Else
            CUMRSLMAP8_bal(1,ILOC,1) = CUMRSLMAP8_bal(1,ILOC,1) +  QRWZI(INR)*timeSettings%timestepSize
            CUMRSLMAP8_bal(2,ILOC,1) = CUMRSLMAP8_bal(2,ILOC,1) +  QEffluent(INR)*timeSettings%timestepSize
            CUMRSLMAP8_bal(3,ILOC,1) = CUMRSLMAP8_bal(3,ILOC,1) +  RSLMAP8_bal(3,ILOC,1)
           Endif
         ELSEIF  (IKIND .EQ. 15) THEN     ! Industry
           If (ExtendedBalanceOutput) then
            CUMRSLMAP8_bal(1,ILOC,1) = CUMRSLMAP8_bal(1,ILOC,1) +  QIndReturnFlow(INR)*timeSettings%timestepSize
            CUMRSLMAP8_bal(2,ILOC,1) = CUMRSLMAP8_bal(2,ILOC,1) +  QIndAll(INR)*timeSettings%timestepSize
            CUMRSLMAP8_bal(3,ILOC,1) = CUMRSLMAP8_bal(3,ILOC,1) +  QIndAll(INR)*timeSettings%timestepSize
            CUMRSLMAP8_bal(4,ILOC,1) = CUMRSLMAP8_bal(4,ILOC,1) +  QIndReturnFlow(INR)*timeSettings%timestepSize
            CUMRSLMAP8_bal(5,ILOC,1) = 0.0
           Else
            CUMRSLMAP8_bal(1,ILOC,1) = CUMRSLMAP8_bal(1,ILOC,1) +  QIndReturnFlow(INR)*timeSettings%timestepSize
            CUMRSLMAP8_bal(2,ILOC,1) = CUMRSLMAP8_bal(2,ILOC,1) +  QIndAll(INR)*timeSettings%timestepSize
            CUMRSLMAP8_bal(3,ILOC,1) = 0.0
           Endif
         ELSEIF  (IKIND .EQ. 16) THEN     ! Sacramento
           if (idebug .ne. 0) write(idebug,*) ' ikind inr iloc ', ikind, inr, iloc
           If (ExtendedBalanceOutput) then
            CUMRSLMAP8_bal(1,ILOC,1) = CUMRSLMAP8_bal(1,ILOC,1) + RSLMAP8_bal(1,ILOC,1)
            CUMRSLMAP8_bal(2,ILOC,1) = CUMRSLMAP8_bal(2,ILOC,1) + RSLMAP8_bal(2,ILOC,1)
            CUMRSLMAP8_bal(3,ILOC,1) = CUMRSLMAP8_bal(3,ILOC,1) + RSLMAP8_bal(3,ILOC,1)
            CUMRSLMAP8_bal(4,ILOC,1) = CUMRSLMAP8_bal(4,ILOC,1) + RSLMAP8_bal(4,ILOC,1)
! kleine indexcorrectie 31 juli 2001 (5 ipv 4)
            CUMRSLMAP8_bal(5,ILOC,1) = CUMRSLMAP8_bal(5,ILOC,1) + RSLMAP8_bal(5,ILOC,1)
           Else
            CUMRSLMAP8_bal(1,ILOC,1) = CUMRSLMAP8_bal(1,ILOC,1) + RSLMAP8_bal(1,ILOC,1)
            CUMRSLMAP8_bal(2,ILOC,1) = CUMRSLMAP8_bal(2,ILOC,1) + RSLMAP8_bal(2,ILOC,1)
            CUMRSLMAP8_bal(3,ILOC,1) = CUMRSLMAP8_bal(3,ILOC,1) + RSLMAP8_bal(3,ILOC,1)
           Endif

         ELSEIF  (IKIND .EQ. 17) THEN     ! Cell
! to be added
         ELSEIF  (IKIND .EQ. 18 .or. ikind .eq. 19 .or. ikind .eq. 20 .or. ikind .eq. 22 .or. ikind .eq. 23 .or. ikind .eq. 31) THEN     ! RR Runoff
           if (idebug .ne. 0) write(idebug,*) ' ikind inr iloc ', ikind, inr, iloc
           If (ExtendedBalanceOutput) then
            CUMRSLMAP8_bal(1,ILOC,1) = CUMRSLMAP8_bal(1,ILOC,1) + RSLMAP8_bal(1,ILOC,1)
            CUMRSLMAP8_bal(2,ILOC,1) = CUMRSLMAP8_bal(2,ILOC,1) + RSLMAP8_bal(2,ILOC,1)
            CUMRSLMAP8_bal(3,ILOC,1) = CUMRSLMAP8_bal(3,ILOC,1) + RSLMAP8_bal(3,ILOC,1)
            CUMRSLMAP8_bal(4,ILOC,1) = CUMRSLMAP8_bal(4,ILOC,1) + RSLMAP8_bal(4,ILOC,1)
            CUMRSLMAP8_bal(5,ILOC,1) = CUMRSLMAP8_bal(5,ILOC,1) + RSLMAP8_bal(5,ILOC,1)
           Else
            CUMRSLMAP8_bal(1,ILOC,1) = CUMRSLMAP8_bal(1,ILOC,1) + RSLMAP8_bal(1,ILOC,1)
            CUMRSLMAP8_bal(2,ILOC,1) = CUMRSLMAP8_bal(2,ILOC,1) + RSLMAP8_bal(2,ILOC,1)
            CUMRSLMAP8_bal(3,ILOC,1) = CUMRSLMAP8_bal(3,ILOC,1) + RSLMAP8_bal(3,ILOC,1)
           Endif
         ELSEIF  (IKIND .EQ. 30) THEN     ! RRConnection
! to be added
           if (idebug .ne. 0) write(idebug,*) ' ikind inr iloc ', ikind, inr, iloc
         ELSEIF  (IKIND .EQ. 32) THEN     ! RRBifurcation
! to be added; all output available in link flows
           if (idebug .ne. 0) write(idebug,*) ' ikind inr iloc ', ikind, inr, iloc
         ENDIF
! Balance error toegevoegd
         CUMRSLMAP8_bal(6,ILOC,1) = CUMRSLMAP8_bal(6,ILOC,1) + RSLMAP8_bal(6,ILOC,1)
      ENDDO

      IF (ISLCMP .ne. 0) THEN
!         IMAP = 9
         DO INOD = 1, NCNODE
            SLTMXC(INOD) = MAX (SLTMXC(INOD), RSLMAP9_slt(1,INOD,1) )
!           RSLMAP9_slt(1,INOD,1) = MAX (RSLMAP9_slt(1,INOD,1), RSLMAP9_slt(1,INOD,1))
            if (idebug .ne. 0)  WRITE(IDEBUG,*) ' sltmxc(inod) ', inod, sltmxc(inod)
         ENDDO
      ENDIF

      DO IVHG = 1, NCVHG
        VHMBPC(IVHG,1,Ievent) = MAX (VHMBPC(IVHG,1,Ievent), BVRL(IVHG,1)*1000./AreaVh(ivhg))
        VHMBPC(IVHG,2,Ievent) = MAX (VHMBPC(IVHG,2,Ievent), BVRL(IVHG,2)*1000./AreaVh(ivhg))
        VHMBPC(IVHG,3,Ievent) = MAX (VHMBPC(IVHG,3,Ievent), BVSTR(IVHG)*1000./AreaVh(ivhg))
        VHMQOU(IVHG,1,Ievent) = MAX (VHMQOU(IVHG,1,Ievent), Q1V(IVHG,1)+ Q1V(IVHG,2))
        VHMQOU(IVHG,2,Ievent) = MAX (VHMQOU(IVHG,2,Ievent), Q2V(IVHG,1)+ Q2V(IVHG,2))
        QOW = Q1V(IVHG,1) + Q1V(IVHG,2)
        IF (sewer(iVhg)%Q2VOW(1) .EQ. 1)  QOW = QOW + Q2V(IVHG,1)
        IF (sewer(iVhg)%Q2VOW(1) .EQ. 1)  QOW = QOW + Q2V(IVHG,2)
        VHMQOU(IVHG,3,Ievent) = MAX (VHMQOU(IVHG,3,Ievent), QOW)
        VHMQOU(IVHG,4,Ievent) = MAX (VHMQOU(IVHG,4,Ievent), RV(IVHG)/timeSettings%timestepSize )
        if (sewer(iVhg)%systemType .eq. 0) VHMQOU(IVHG,5,Ievent) = MAX (VHMQOU(IVHG,5,Ievent), DWAPaved(IVHG) )
        if (sewer(iVhg)%systemType .ne. 0) VHMQOU(IVHG,6,Ievent) = MAX (VHMQOU(IVHG,6,Ievent), DWAPaved(IVHG) )
        VHMQOU(IVHG,7,Ievent)  = MAX (VHMQOU(IVHG,7,Ievent), INV(IVHG,1)/timeSettings%timestepSize )
        VHMQOU(IVHG,8,Ievent)  = MAX (VHMQOU(IVHG,8,Ievent), INV(IVHG,2)/timeSettings%timestepSize )
        VHMQOU(IVHG,9,Ievent)  = MAX (VHMQOU(IVHG,9,Ievent), Q1V(IVHG,1)  )
        VHMQOU(IVHG,10,Ievent) = MAX (VHMQOU(IVHG,10,Ievent), Q2V(IVHG,1) )
        VHMQOU(IVHG,11,Ievent) = MAX (VHMQOU(IVHG,11,Ievent), Q1V(IVHG,2) )
        VHMQOU(IVHG,12,Ievent) = MAX (VHMQOU(IVHG,12,Ievent), Q2V(IVHG,2) )
        VHMQOU(IVHG,13,Ievent) = MAX (VHMQOU(IVHG,13,Ievent), VV(IVHG)/timeSettings%timestepSize )
      ENDDO

      DO IOVH = 1, NCOVHG
        OvMxPercInund(IOVH,Ievent) = MAX (OvMxPercInund(Iovh,Ievent), PercInundation(iovh))
        OVMGWS(IOVH,Ievent  ) = MAX (OVMGWS(IOVH,Ievent  ), GWL(IOVH))
        OVMGWV(IOVH,Ievent  ) = MAX (OVMGWV(IOVH,Ievent  ), BOBD(IOVH))
        OVMONL(IOVH,Ievent,1) = MAX (OVMONL(IOVH,Ievent,1), BOLND(IOVH)/Areaoh(iovh)*1000.)
        OVMONL(IOVH,Ievent,2) = MAX (OVMONL(IOVH,Ievent,2), BOLND(IOVH))
        OVMONV(IOVH,Ievent,1) = MAX (OVMONV(IOVH,Ievent,1), OnvZone(IOVH)%Actual_mm)
        OVMONV(IOVH,Ievent,2) = MAX (OVMONV(IOVH,Ievent,2), OnvZone(IOVH)%Actual_Volume)
        OVMBGC(IOVH,Ievent  ) = MAX (OVMBGC(IOVH,Ievent  ), BERGC(IOVH))
        IrrSupply(IOVH,Ievent) = MAX (IrrSupply(iovh,Ievent), IrrigationSupply(IOVH))
        If (IrrigationSource(iovh) .eq. 2) IrrGWDemand(IOVH,Ievent) = MAX (IrrGWDemand(iovh,Ievent), IrrigationDemand(IOVH))
        OVMQOU(IOVH,1,Ievent) = MAX (OVMQOU(IOVH,1,Ievent), Q1O(IOVH) )
        OVMQOU(IOVH,2,Ievent) = MAX (OVMQOU(IOVH,2,Ievent), Q2O(IOVH) )
        OVMQOU(IOVH,3,Ievent) = MAX (OVMQOU(IOVH,3,Ievent), RO(IOVH)/timeSettings%timestepSize )
        OVMQOU(IOVH,4,Ievent) = MAX (OVMQOU(IOVH,4,Ievent), VO(IOVH)/timeSettings%timestepSize )
        OVMQOU(IOVH,5,Ievent) = MAX (OVMQOU(IOVH,5,Ievent), INO(IOVH)/timeSettings%timestepSize )
        RKWEL = (KWEL(IOVH)-WEGZG(IOVH) ) *AreaGwComp(IOVH)* timeSettings%timestepSize
        OVMQOU(IOVH,6,Ievent) = MAX (OVMQOU(IOVH,6,Ievent), RKWEL/timeSettings%timestepSize )
        OVMQOU(IOVH,7,Ievent) = MAX (OVMQOU(IOVH,7,Ievent), VBO(IOVH)/timeSettings%timestepSize )
        OVMQOU(IOVH,8,Ievent) = MAX (OVMQOU(IOVH,8,Ievent), RzEPOT(IOVH)/timeSettings%timestepSize )
        OVMQOU(IOVH,9,Ievent) = MAX (OVMQOU(IOVH,9,Ievent), QINB(IOVH)/timeSettings%timestepSize )
        OVMQOU(IOVH,10,Ievent) = MAX (OVMQOU(IOVH,10,Ievent), -1*QINB(IOVH)/timeSettings%timestepSize )
        IF (GWL(IOVH) .GE. MAXGWL(IOVH)) THEN
            GWEXC (IOVH,1,Ievent) = 1
            GWEXC (IOVH,2,Ievent) = GWEXC (IOVH,2,Ievent) + timeSettings%timestepSize
        ENDIF
! ARS 6315
        If (UpdateDailyValues) Then
           GWEXC (IOVH,3,Ievent) = GWEXC (IOVH,3,Ievent) + GwExc(Iovh,4,Ievent)
           GwExC (IOVH,4,Ievent) = 0.0
        Endif
        IF (GWL(IOVH) .GE. MAXGWL2(IOVH)) THEN
           RHelp = Max (0.0, Gwl(iovh) - MaxGwl2(iovh))
           If (CumGroundwaterExceedanceOption .eq. 1) RHelp = Rhelp * RHelp
           GWEXC (IOVH,4,Ievent) = Max (GWEXC (IOVH,4,Ievent), Rhelp)
        ENDIF
      ENDDO

      DO IKAS = 1, NCKAS
! ipv nul nu op silo data initialiseren
         BSUM  = SILOB(IKAS)
         QSUM  = QSILOW(IKAS)
! ipv rain, evap: init. op waarde voor daken
         RAINT = RKD(IKAS)
         EVAPT = VKD(IKAS)
         USET  = 0.0
         DO IKKL = 1, NCKKL
            KSMBPC(IKAS,IKKL,Ievent) = MAX (KSMBPC(IKAS,IKKL,Ievent), BKAS(IKAS,IKKL))
            KSMQOU(IKAS,IKKL,Ievent,1) = MAX (KSMQOU(IKAS,IKKL,Ievent,1), QKAS(IKAS,IKKL))
            KSMQOU(IKAS,IKKL,Ievent,2) = MAX (KSMQOU(IKAS,IKKL,Ievent,2), RKB(IKAS,IKKL))
            KSMQOU(IKAS,IKKL,Ievent,3) = MAX (KSMQOU(IKAS,IKKL,Ievent,3), VKB(IKAS,IKKL))
            KSMQOU(IKAS,IKKL,Ievent,4) = MAX (KSMQOU(IKAS,IKKL,Ievent,4), GEBR(IKAS,IKKL))
            BSUM  = BSUM  + BKAS(IKAS,IKKL)
            QSUM  = QSUM  + QKAS(IKAS,IKKL)
            RAINT = RAINT + RKB (IKAS,IKKL)
            EVAPT = EVAPT + VKB (IKAS,IKKL)
            USET  = USET  + GEBR(IKAS,IKKL)
         ENDDO
         KSMBPC(IKAS,NCKKL+1,Ievent) = MAX (KSMBPC(IKAS,NCKKL+1,Ievent), BSUM)
         KSMQOU(IKAS,NCKKL+1,Ievent,1) = MAX (KSMQOU(IKAS,NCKKL+1,Ievent,1), QSUM)
         KSMQOU(IKAS,NCKKL+1,Ievent,2) = MAX (KSMQOU(IKAS,NCKKL+1,Ievent,2), RAINT/timeSettings%timestepSize)
         KSMQOU(IKAS,NCKKL+1,Ievent,3) = MAX (KSMQOU(IKAS,NCKKL+1,Ievent,3), EVAPT/timeSettings%timestepSize)
         KSMQOU(IKAS,NCKKL+1,Ievent,4) = MAX (KSMQOU(IKAS,NCKKL+1,Ievent,4), USET /timeSettings%timestepSize)
      ENDDO

      DO IOW  = 1,NCOW
         OWMLVL(IOW,Ievent,1) = MAX (OWMLVL(IOW,Ievent,1), LVLOW(IOW))
         OWMLVL(IOW,Ievent,2) = MAX (OWMLVL(IOW,Ievent,2), VOLOW(IOW)+ActualExtraBergendVolume(iow) )
         OWMLVL(IOW,Ievent,3) = MAX (OWMLVL(IOW,Ievent,3), ROW(IOW)/timeSettings%timestepSize)
         OWMLVL(IOW,Ievent,4) = MAX (OWMLVL(IOW,Ievent,4), VOW(IOW)/timeSettings%timestepSize)
         OWMLVL(IOW,Ievent,5) = MAX (OWMLVL(IOW,Ievent,5), KWOW(IOW)/timeSettings%timestepSize)
         IF (LVLOW(IOW) .GT. MAXLVL(IOW)) THEN
             OWEXC (IOW,1,Ievent) = 1
             OWEXC (IOW,2,Ievent) = OWEXC (IOW,2,Ievent) + timeSettings%timestepSize
         ENDIF
         OWMLVL(IOW,Ievent,6) = MAX (OWMLVL(IOW,Ievent,6), RSLMAP4_Ow(7,Iow,1))
         OWMLVL(IOW,Ievent,7) = MAX (OWMLVL(IOW,Ievent,7), RSLMAP4_Ow(8,Iow,1))
         OWMLVL(IOW,Ievent,8) = MAX (OWMLVL(IOW,Ievent,8), RSLMAP4_Ow(9,Iow,1))
      ENDDO

      DO ISTR = 1,NCSTRU
         QSTRMX(ISTR,Ievent) = MAX (QSTRMX(ISTR,Ievent), QSTRU(ISTR))
         ActCrestLevelMx(ISTR,Ievent) = MAX (ActCrestLevelMx(ISTR,Ievent), ActCrestLevel(ISTR))
         QSTRMX1(ISTR,Ievent) = MAX (QSTRMX1(ISTR,Ievent), QSTRU1(ISTR))
         QSTRMX2(ISTR,Ievent) = MAX (QSTRMX2(ISTR,Ievent), QSTRU2(ISTR))
      ENDDO

      DO teller = 1,NCBOUN
         QBNDMX(teller,Ievent,1) = MAX (QBNDMX(teller,Ievent,1), QBND(teller))
         QBNDMX(teller,Ievent,2) = MAX (QBNDMX(teller,Ievent,2), BNDPAR(teller,1))
      ENDDO
 !RWZI
      DO teller = 1,NCRWZI
         QRWZIMX(teller,Ievent) = MAX (QRWZIMX(teller,Ievent), QRWZI(teller))
         QEffRWZIMX(teller,Ievent) = MAX (QEffRWZIMX(teller,Ievent), QEffluent(teller))
      ENDDO
 !Industry
      DO teller = 1,NCIndus
         QDemMX  (teller,Ievent) = MAX (QDemMX(teller,Ievent)  , QIndDem(teller))
         QAllMX  (teller,Ievent) = MAX (QAllMX(teller,Ievent)  , QIndAll(teller))
         QShortMX(teller,Ievent) = MAX (QShortMX(teller,Ievent), QIndShortage(teller))
         QDisMX  (teller,Ievent) = MAX (QDisMX(teller,Ievent)  , QIndReturnFlow(teller))
         SltIndMx(teller,Ievent) = MAX (SltIndMx(teller,Ievent), SltIndDis(teller))
      ENDDO
 !Sacramento
      DO teller = 1,NCSacr
         if (idebug .ne. 0) write(idebug,*) ' Sacramento teller ', teller, ievent
         SacMXUZTWC  (teller,Ievent) = MAX ( SacMXUZTWC (teller,Ievent), RSLMAP17_Sacr(1,teller,1))
         if (idebug .ne. 0) write(idebug,*) ' Sacramento 1 '
         SacMXUZFWC  (teller,Ievent) = MAX ( SacMXUZFWC (teller,Ievent), RSLMAP17_Sacr(2,teller,1))
         if (idebug .ne. 0) write(idebug,*) ' Sacramento 2 '
         SacMXLZTWC  (teller,Ievent) = MAX ( SacMXLZTWC (teller,Ievent), RSLMAP17_Sacr(3,teller,1))
         if (idebug .ne. 0) write(idebug,*) ' Sacramento 3'
         SacMXLZFSC  (teller,Ievent) = MAX ( SacMXLZFSC (teller,Ievent), RSLMAP17_Sacr(4,teller,1))
         if (idebug .ne. 0) write(idebug,*) ' Sacramento 4'
         SacMXLZFPC  (teller,Ievent) = MAX ( SacMXLZFPC (teller,Ievent), RSLMAP17_Sacr(5,teller,1))
         if (idebug .ne. 0) write(idebug,*) ' Sacramento 5'
         SacMXPrecip (teller,Ievent) = MAX ( SacMXPrecip(teller,Ievent), RSLMAP17_Sacr(6,teller,1))
         if (idebug .ne. 0) write(idebug,*) ' Sacramento 6'
         SacMXPotEvp (teller,Ievent) = MAX ( SacMXPotEvp(teller,Ievent), RSLMAP17_Sacr(7,teller,1))
         if (idebug .ne. 0) write(idebug,*) ' Sacramento 7'
         SacMXActEvp (teller,Ievent) = MAX ( SacMXActEvp(teller,Ievent), RSLMAP17_Sacr(8,teller,1))
         if (idebug .ne. 0) write(idebug,*) ' Sacramento 8'
         SacMXBasFlw (teller,Ievent) = MAX ( SacMXBasFlw(teller,Ievent), RSLMAP17_Sacr(9,teller,1))
         if (idebug .ne. 0) write(idebug,*) ' Sacramento 9'
         SacMXSurFlw (teller,Ievent) = MAX ( SacMXSurFlw(teller,Ievent), RSLMAP17_Sacr(10,teller,1))
         if (idebug .ne. 0) write(idebug,*) ' Sacramento 10'
         SacMXImpFlw (teller,Ievent) = MAX ( SacMXImpFlw(teller,Ievent), RSLMAP17_Sacr(11,teller,1))
         if (idebug .ne. 0) write(idebug,*) ' Sacramento 11'
         SacMXTotRun (teller,Ievent) = MAX ( SacMXTotRun(teller,Ievent), RSLMAP17_Sacr(12,teller,1))
         if (idebug .ne. 0) write(idebug,*) ' Sacramento 12'
         SacMXChaInf (teller,Ievent) = MAX ( SacMXChaInf(teller,Ievent), RSLMAP17_Sacr(13,teller,1))
         if (idebug .ne. 0) write(idebug,*) ' Sacramento 13'
         SacMXLosFlw (teller,Ievent) = MAX ( SacMXLosFlw(teller,Ievent), RSLMAP17_Sacr(14,teller,1))
         if (idebug .ne. 0) write(idebug,*) ' Sacramento 14'
         SacMXAdimC  (teller,Ievent) = MAX ( SacMXAdimC (teller,Ievent), RSLMAP17_Sacr(15,teller,1))
         if (idebug .ne. 0) write(idebug,*) ' end Sacramento teller ', teller
      ENDDO
 !Cel
 ! to be added

 !RR Runoff
 ! to be added

 !RRConnection/Bifurcation
 ! to be added


      RETURN
      END Subroutine WrtOutStoreSummary



 Subroutine WriteFixedAreaNodes (Totalarea)

! Routine writes fixed areas of nodes in HIS files

! for netCdf
      use netCdfData
      use NetCdf


  IMPLICIT NONE

  Integer       i, j, idum, iKind, iNr, ikkl, iptyp, ipopp, TimeSeriesVar
  Integer       TmpYear, TmpMonth, TmpDay
  Integer       ihour, imin, isec
  Real          Rarea, Rlvl
  Double Precision TotalArea
  Logical Success

  REAL, Pointer, SAVE ::     AreaNode(:)

  Real          PeilArray(6), AreaArray(6)

! NetCdf
  Integer                             ipos1
  Character(Len=40), pointer, dimension(:)  :: locationid
  integer                             nitem, refdate, reftime
  Character(Len=40)                   VariableName
  Character(Len=80)                   LongVariableName
  double precision, dimension(:), allocatable       :: X1Coor, Y1Coor


! DIO
  type(DioPltType)                :: InOutDataSet   ! output dataset
  Character(Len=DioMaxStreamLen)  :: outName   ! name of out dataset
  character(len=HisRunIdSize), dimension(HisRunIdDim) :: runId         ! (HIS) runid
  Character(Len=DioMaxParLen), dimension(1)           :: parNames = &  ! variable(s) in dataset
                                       'Node area (ha)         '
  Character(Len=DioMaxLocLen), pointer, dimension(:)  :: LocIds      ! locations(s) in dataset
  Character(Len=DioMaxLocLen), pointer, dimension(:)  :: LocDescr      ! location descriptions in dataset

! create empty dataset (no name, var. type unknown)
  InOutDataSet = DioPltCreate('NoName', Dio_PLT_Unknown)

! locals
    TimeSeriesVar = -1
    outName = ConfFil_get_NAMFIL(86)
    IF (outName .NE. ' ' .AND. NCNode .GT. 0) THEN
        Success = DH_AllocInit (NcNode, LocIds, ' ')
        Success = success .and. DH_AllocInit (NcNode, LocDescr, ' ')
        If (.not. success)  call ErrMsgStandard (981, 0, ' Error allocating arrays in subroutine ', ' Output_FixedAreaNodes' )
        ! header
        runId  = ' '
        runId(1) = CASENM(1:)
        runId(3) = 'TITLE: Fixed node data            '
        tmpYear = ConfArr_get_IYEAR()
        tmpMonth = ConfArr_get_iMonth()
        tmpDay = ConfArr_get_IDAY()
        ihour  = ConfArr_get_IHour()
        imin   = ConfArr_get_IMinute()
        isec   = ConfArr_get_ISecond()
        WRITE(RunId(4),111) tmpYear, tmpMonth, tmpDay
 111    FORMAT ('T0: ',I4,'.',I2,'.',I2,1X,'00:00:00 (scu=    86400s)')
        Do i = 1, ncnode
           LocIds(i) = Id_Nod(I)
           LocDescr(i) = NamNod(I)
        Enddo
        InOutDataSet = DioPltDefine(outName, runId, Dio_Plt_Real, parNames, LocIds)
        call DioPltAddDescriptions(InOutDataSet, dio_plt_locs, LocDescr)
        deallocate(LocIds)
        deallocate(LocDescr)

!       Compute the data
        TotalArea = 0.0
        Success = DH_AllocInit (NNod, AreaNode, 0E0)
        If (.not. success)  call ErrMsgStandard (981, 0, ' Error allocating arrays in subroutine ', ' Output_FixedAreaNodes' )
!       ALLOCATE ( AREAnode(NNOD), Stat=Allocation_Error )
! Bepaal areaal per knoop
        Do i = 1, ncNode
          IKIND = EiNode(i,3)
          INR   = EiNode(i,2)
          IF (IKIND .EQ. 1) THEN
              AREANODE(I) = AREAVH(INR)
          ELSEIF (IKIND .EQ. 2) THEN
              AreaNode(i) = AREAOH(INR)
          ELSEIF (IKIND .EQ. 3) THEN
              AreaNode(i) = AREAS(inr)
              DO IKKL =1,NCKKL
                 AreaNode(i) = AreaNode(i) + AREAKK(inr,ikkl)
              ENDDO
          ELSEIF (IKIND .EQ. 4) THEN
 !oud;       AreaNode(i) = AREAOW(3,INR)
 ! ARS 813: zorg dat dit areaal gelijk is aan dat wat in de 3B_Gener.Out wordt genoemd, nl areaal bij laatste streefpeil
             RLVL = WINLVL(INR)
             IDUM = 1
             Do j=1,NVal
                PeilArray(j) = PeilOw(j,inr)
                AreaArray(j) = AreaOw(j,inr)
             Enddo
             CALL RR_INTERP (NVAL, PeilArray, AreaArray, RLVL, RAREA, IDUM)
             AreaNode(i) = RAREA
          ELSEIF (IKIND .EQ. 5) THEN
              AreaNode(i) = 0
          ELSEIF (IKIND .EQ. 6) THEN
              AreaNode(i) = 0
          ELSEIF (IKIND .EQ. 7) THEN
              AreaNode(i) = 0
              DO IPTYP=1,NPTYP
                 DO IPOPP=1,NPOPP
                    AreaNode(i) = AreaNode(i) + AREAPV(Inr,IPTYP,IPOPP)
                 ENDDO
              ENDDO
! Feb 2003; ARS 11073-11174; add special area from *AFK records
              if (NrSpecialNwrwAreas(Inr) .gt. 0) then
                 do j=1, NrSpecialNwrwAreas(Inr)
                    AreaNode(i) = AreaNode(i) + SpecialNWRWAreas(Inr,j)
                 Enddo
              endif
! End Feb 2003
          ELSEIF (IKIND .EQ. 14) THEN     !RWZI
              AreaNode(i) = 0
          ELSEIF (IKIND .EQ. 15) THEN     !Industry
              AreaNode(i) = 0
          ELSEIF (IKIND .EQ. 16) THEN     !Sacramento
              AreaNode(i) = AreaSa(INr)
          ELSEIF (IKIND .EQ. 17) THEN     !Cel
!              AreaNode(i) = CellData(Inr)%TotalArea
          ELSEIF (IKIND .EQ. 18) THEN     !RR Runoff - External runoff
              AreaNode(i) = Area_RRRunoffNode(INr)
          ELSEIF (IKIND .EQ. 19) THEN     !RR Runoff - HBV
              AreaNode(i) = Area_RRRunoffNode(INr)
          ELSEIF (IKIND .EQ. 20) THEN     !RR Runoff - SCS
              AreaNode(i) = Area_RRRunoffNode(INr)
          ELSEIF (IKIND .EQ. 22) THEN     !RR Runoff - LGSI
              AreaNode(i) = Area_RRRunoffNode(INr)
          ELSEIF (IKIND .EQ. 23) THEN     !RR Runoff - Wageningen/Walrus
              AreaNode(i) = Area_RRRunoffNode(INr)
          ELSEIF (IKIND .EQ. 31) THEN     !RR Runoff - NAM
              AreaNode(i) = Area_RRRunoffNode(INr)
          ENDIF
          AreaNode(i) = AreaNode(i) / Float(HA2M)
! totaal 3B areaal
          if (ikind .ne. 7) Totalarea = Totalarea + AreaNode(i)
       Enddo

        ! Send the data
        idum = 0   ! HIS timestep indicator
!       Deze routine wordt slechts 1 keer uitgevoerd, dus Reshape memory leak niet erg
!       call DioPltPut (InOutDataSet, idum, RESHAPE (AreaNode, (/1, NcNode/)) )
! Dec 2002: Dio 1.06.02 can also handle 1D array
        call DioPltPut (InOutDataSet, idum, AreaNode)    ! deze HIS file altijd genereren, is slechts 1 tijdstap
        if (GenerateNetCdfOutput) then
               NetCdfOutputFileName = Outname
               Ipos1 = FndFrst('.his', NetCdfOutputFileName, .false.)
               Ipos1 = max (ipos1, FndFrst('.His', NetCdfOutputFileName(1:), .false.) )
               Ipos1 = max (ipos1, FndFrst('.HIS', NetCdfOutputFileName(1:), .false.) )
               if (ipos1 .le. 0) then
                  call ErrMsgStandard(972, 0, ' Error in setting Netcdf output file name for map output output',' BalanceModule_MapOutputFile')
               endif
               NetCdfOutputFileName(ipos1:) = ''
               NetCdfOutputFileName(ipos1:) = '.nc'
               ! create NetCdf file
               INetCdfFile(FixedAreanetCdfFileNr) = nc_create (NetCdfOutputFileName)
               nitem = ncnode ! nr of locations
               Success = DH_AllocInit (Ncnode, LocationId, ' ')
               Do i=1,Ncnode
                  locationid(i)(1:) = Id_Nod(I)(1:40)
               enddo
               ! set X and Y coordinates for NetCdf files
               if (allocated(X1Coor)) deallocate(X1Coor)
               if (allocated(Y1Coor)) deallocate(Y1Coor)
               allocate(X1Coor(nitem))
               allocate(Y1Coor(nitem))
               Do i=1,nitem
                  X1Coor(i) = dble (XCoor(i))
                  Y1Coor(i) = dble (YCoor(i))
               Enddo
               refdate = tmpYear * 10000 + tmpMonth * 100 + tmpDay
               reftime = ihour * 10000 + Imin * 100 + Isec
               call nc_prepare(iNetCdfFile(FixedAreaNetCdfFileNr), nitem, '', loc_dimid(FixedAreaNetCdfFileNr), time_dimid(FixedAreaNetCdfFileNr), refdate, reftime, time_varid(FixedAreaNetCdfFileNr),locationid, .false., xc=X1Coor,yc=Y1Coor)
               id_locdim = loc_dimid(FixedAreaNetCdfFileNr)
               id_timedim = time_dimid(FixedAreaNetCdfFileNr)
               id_strlendim = 40
!               write(*,*) ' FixedAreaNode nc file'
!               write(*,*) ' id_locdim', id_locdim
!               write(*,*) ' id_timedim', id_timedim
!               write(*,*) ' id_strlendim', id_strlendim
!               write(*,*) ' nitem ', nitem

               VariableName = Parnames(1)(1:9)
               LongVariableName = Parnames(1)(1:9)
               ! Variable name may only contain letters, cijfers and underscores (so no ()*&^%$#@!?><=+-;:.,space)
               VariableName = NetCdfName(VariableName)
               id_vars(FixedAreaNetCdfFileNr,1) = HisSetupVariable (INetCdfFile(FixedAreaNetCdfFileNr), VariableName(1:len_trim(VariableName)), nf90_double, (/ id_locdim, id_timedim /), &
                                                    VariableName(1:len_trim(VariableName)), LongVariableName(1:len_trim(LongVariableName)), 'ha', TimeSeries=TimeSeriesVar)
               ierr = nf90_enddef(INetCdfFile(FixedAreaNetCdfFileNr))
               Call NetCdfCheck(' WriteFixedAreaNodes NetCdf file after Enddef',ierr)
               Deallocate(LocationId)
               ! Put variable
               ierr = nf90_put_var(INetCdfFile(FixedAreaNetCdfFileNr), time_varid(FixedAreaNetCdfFileNr), 0D0, (/ 1 /))
               Call NetCdfCheck(' WriteFixedAreaNodes after Putvar  DDays',ierr)
               Do I=1,NCNode
                  ierr = nf90_put_var(INetCdfFile(FixedAreaNetCdfFileNr), id_vars(FixedAreaNetCdfFileNr,1), dble(AreaNode(i)),(/i,1/))
                  Call NetCdfCheck(' WriteFixedAreaNodes after Putvar DioResult',ierr)
               Enddo
               ! close file
               ierr = nf90_close(iNetCdfFile(FixedAreaNetCdfFileNr))
        endif
        call DioPltDestroy (InOutDataSet)

        DEALLOCATE ( AREAnode )

    Endif

!   De dataset kan direct worden afgesloten, omdat het een eenmalige schrijfactie betreft
    Call CloseDioPlt (InOutDataset)

    Return
   End Subroutine WriteFixedAreaNodes




 Subroutine WriteFixedAreaNWRW

! Routine writes detailed areas of NWRW nodes in HIS files

! for netCdf
      use netCdfData
      use NetCdf

  IMPLICIT NONE

  Integer       i, j, iKind, idum, TimeSeriesVar
  Integer       TmpYear, TmpMonth, TmpDay
  Integer       ihour, imin, isec
!
  Integer       ipluv, iptyp, ipopp
  Logical Success

! NetCdf
  Integer                             jpar, ipos1
  Character(Len=40), pointer, dimension(:)  :: locationid
  integer                             nitem, refdate, reftime
  Character(Len=40)                   VariableName
  Character(Len=80)                   LongVariableName
  double precision, dimension(:), allocatable       :: X1Coor, Y1Coor


! DIO
  type(DioPltType)                :: InOutDataSet   ! output dataset
  Character(Len=DioMaxStreamLen)  :: outName   ! name of out dataset
  character(len=HisRunIdSize), dimension(HisRunIdDim) :: runId         ! (HIS) runid
! Feb 2003; ARS 11073-11174; add special area from *AFK records, so dimension(13) instead of (12)
  Character(Len=DioMaxParLen), dimension(13)          :: parNames      ! variable(s) in dataset
  Character(Len=DioMaxLocLen), pointer, dimension(:)  :: LocIds      ! locations(s) in dataset
  Character(Len=DioMaxLocLen), pointer, dimension(:)  :: LocDescr      ! location descriptions  in dataset
  Integer  loc
  real, Pointer, save:: NwrwAreas(:,:)

  TimeSeriesVar = -1
  if (nptyp .gt. 4 .or. npopp .gt. 3) then
     call SetMessage(LEVEL_FATAL, 'Outputmodule: local dimensions NWRW area types')
  endif

    ParNames(1) = 'Sloped closed paved'
    ParNames(2) = 'Sloped open paved'
    ParNames(3) = 'Sloped roofs'
    ParNames(4) = 'Sloped unpaved'
    ParNames(5) = 'Flat closed paved'
    ParNames(6) = 'Flat open paved'
    ParNames(7) = 'Flat roofs'
    ParNames(8) = 'Flat unpaved'
    ParNames(9) = 'Very flat closed paved'
    ParNames(10) = 'Very flat open paved'
    ParNames(11) = 'Very flat roofs'
    ParNames(12) = 'Very flat unpaved'
    ParNames(13) = 'Special areas'

    ParNames(1) =  TranslateString (LanguageHandle,ParNames(1))
    ParNames(2) =  TranslateString (LanguageHandle,ParNames(2))
    ParNames(3) =  TranslateString (LanguageHandle,ParNames(3))
    ParNames(4) =  TranslateString (LanguageHandle,ParNames(4))
    ParNames(5) =  TranslateString (LanguageHandle,ParNames(5))
    ParNames(6) =  TranslateString (LanguageHandle,ParNames(6))
    ParNames(7) =  TranslateString (LanguageHandle,ParNames(7))
    ParNames(8) =  TranslateString (LanguageHandle,ParNames(8))
    ParNames(9) =  TranslateString (LanguageHandle,ParNames(9))
    ParNames(10) =  TranslateString (LanguageHandle,ParNames(10))
    ParNames(11) =  TranslateString (LanguageHandle,ParNames(11))
    ParNames(12) =  TranslateString (LanguageHandle,ParNames(12))
    ParNames(13) =  TranslateString (LanguageHandle,ParNames(13))

! create empty dataset (no name, var. type unknown)
  InOutDataSet = DioPltCreate('NoName', Dio_PLT_Unknown)


    outName = ConfFil_get_NAMFIL(100)
    If (outName .NE. ' ' .AND. NCPluv .GT. 0) THEN
        Success = DH_AllocInit (NcPluv, LocIds, ' ')
        Success = success .and. DH_AllocInit (NcPluv, LocDescr, ' ')
        If (.not. success)  call ErrMsgStandard (981, 0, ' Error allocating arrays in subroutine ', ' Output_NWRWAreaNWRW' )
        ! header
        runId  = ' '
        runId(1) = CASENM(1:)
        runId(3) = 'TITLE: Fixed node data            '
        tmpYear = ConfArr_get_IYEAR()
        tmpMonth = ConfArr_get_iMonth()
        tmpDay = ConfArr_get_IDAY()
        ihour  = ConfArr_get_IHour()
        imin   = ConfArr_get_IMinute()
        isec   = ConfArr_get_ISecond()
        WRITE(RunId(4),111) tmpYear, tmpMonth, tmpDay
 111    FORMAT ('T0: ',I4,'.',I2,'.',I2,1X,'00:00:00 (scu=    86400s)')
        loc = 1
        Do i = 1, ncnode
          IKIND = EiNode(i,3)
          If (IKIND .EQ. 7) THEN
             LocIds(loc) = Id_Nod(i)
             LocDescr(loc) = NamNod(i)
             loc = loc + 1
          Endif
        Enddo
        InOutDataSet = DioPltDefine(outName, runId, Dio_Plt_Real, parNames, LocIds)
        call DioPltAddDescriptions(InOutDataSet, dio_plt_locs, LocDescr)
        deallocate(LocIds)
        deallocate(LocDescr)

        ! Send the data
        idum = 0   ! HIS timestep indicator
! check Stack overflow Eindhoven
        Success = DH_AllocInit (13, NPlv,  NWRWAreaS, 0E0)
        If (.not. success)  call ErrMsgStandard (981, 0, ' Error allocating arrays in subroutine ', ' Output_NWRWAreaNodes' )
!        Allocate(NwrwAreaS(13,Nplv), Stat=Allocation_error)
!        NWRWAreas = RESHAPE (AreaPv,(/12, NPlv/),Order=(/2,1/) )
         do ipluv=1,ncpluv
           i = 0
           do ipopp=1,npopp
              do iptyp=1,nptyp
                 i = i+1
                 NwrwAreas(i,ipluv)=AreaPv(ipluv,iptyp,ipopp)
              enddo
           enddo
! Feb 2003; ARS 11073-11174; add special area from *AFK records
           if (NrSpecialNwrwAreas(Ipluv) .gt. 0) then
              do j=1, NrSpecialNwrwAreas(Ipluv)
                 NwrwAreas(13,ipluv) = NwrwAreas(13,ipluv) + SpecialNWRWAreas(Ipluv,j)
              Enddo
           endif
! End Feb 2003
        enddo

        call DioPltPut (InOutDataSet, Idum, NwrwAreas)           ! hier wel altijd de HIS file schrijven, is toch maar 1 tijdstap

        if (GenerateNetCdfOutput) then
               NetCdfOutputFileName = Outname
               Ipos1 = FndFrst('.his', NetCdfOutputFileName, .false.)
               Ipos1 = max (ipos1, FndFrst('.His', NetCdfOutputFileName(1:), .false.) )
               Ipos1 = max (ipos1, FndFrst('.HIS', NetCdfOutputFileName(1:), .false.) )
               if (ipos1 .le. 0) then
                  call ErrMsgStandard(972, 0, ' Error in setting Netcdf output file name for map output output',' BalanceModule_MapOutputFile')
               endif
               NetCdfOutputFileName(ipos1:) = ''
               NetCdfOutputFileName(ipos1:) = '.nc'
               ! create NetCdf file
               INetCdfFile(NWRWAreaNetCdfFileNr) = nc_create (NetCdfOutputFileName)
               nitem = ncpluv
               Success = DH_AllocInit (Ncnode, LocationId, ' ')
               loc = 1
               Do i = 1, ncnode
                 IKIND = EiNode(i,3)
                 If (IKIND .EQ. 7) THEN
                    Locationid(loc) = Id_Nod(i)(1:40)
                    loc = loc + 1
                 Endif
               Enddo
               ! set X and Y coordinates for NetCdf files
               if (allocated(X1Coor)) deallocate(X1Coor)
               if (allocated(Y1Coor)) deallocate(Y1Coor)
               allocate(X1Coor(nitem))
               allocate(Y1Coor(nitem))
               Do i=1,nitem
                  X1Coor(i) = dble (XCoor(i))
                  Y1Coor(i) = dble (YCoor(i))
               Enddo
               refdate = tmpYear * 10000 + tmpMonth * 100 + tmpDay
               reftime = ihour * 10000 + Imin * 100 + Isec
               call nc_prepare(iNetCdfFile(NWRWAreaNetCdfFileNr), nitem, '', loc_dimid(NWRWAreaNetCdfFileNr), time_dimid(NWRWAreanetCdfFileNr), refdate, reftime, time_varid(NWRWAreaNetCdfFileNr),locationid, .false., xc=X1Coor,yc=Y1Coor)
               id_locdim = loc_dimid(NWRWAreaNetCdfFileNr)
               id_timedim = time_dimid(NWRWAreaNetCdfFileNr)
               id_strlendim = 40
!               write(*,*) ' NWRWAreaNode nc file'
!               write(*,*) ' id_locdim', id_locdim
!               write(*,*) ' id_timedim', id_timedim
!               write(*,*) ' id_strlendim', id_strlendim
!               write(*,*) ' nitem ', nitem

               do jpar=1,13
                  VariableName = Parnames(jpar)
                  LongVariableName = Parnames(jpar)
                  ! Variable name may only contain letters, cijfers and underscores (so no ()*&^%$#@!?><=+-;:.,space)
                  VariableName = NetCdfName(VariableName)
                  id_vars(NWRWAreaNetCdfFileNr,jpar) = HisSetupVariable (INetCdfFile(NWRWAreaNetCdfFileNr), VariableName(1:len_trim(VariableName)), nf90_double, (/ id_locdim, id_timedim /), &
                                                       VariableName(1:len_trim(VariableName)), LongVariableName(1:len_trim(LongVariableName)), 'ha', TimeSeries=TimeSeriesVar)
               enddo
               ierr = nf90_enddef(INetCdfFile(NWRWAreaNetCdfFileNr))
               Call NetCdfCheck(' WriteNWRWArea NetCdf file after Enddef',ierr)
               Deallocate(LocationId)
              ! Put variables
               ierr = nf90_put_var(INetCdfFile(NWRWAreaNetCdfFileNr), time_varid(NWRWAreaNetCdfFileNr), 0D0, (/ 1 /))
               Call NetCdfCheck(' WriteNWRWArea after Putvar  DDays',ierr)
               do jpar=1,13
                  Do i=1,ncpluv
                     ierr = nf90_put_var(INetCdfFile(NWRWAreaNetCdfFileNr), id_vars(NWRWAreaNetCdfFileNr,jpar), dble(NWRWAreas(jpar,i)),(/i,1/))
                     Call NetCdfCheck(' WriteNWRWArea after Putvar DioResult',ierr)
                     if (ierr .ne. nf90_noerr) then
                        write(*,*) ' error for parameter and node index ', jpar, i
                     endif
                  Enddo
               Enddo
               ! close file
               ierr = nf90_close(iNetCdfFile(NWRWAreaNetCdfFileNr))
        endif
        deallocate(NwrwAreas)
        call DioPltDestroy (InOutDataSet)

!   De dataset kan direct worden afgesloten, omdat het een eenmalige schrijfactie betreft
        Call CloseDioPlt (InOutDataset)
    Endif

    Return
   End Subroutine WriteFixedAreaNWRW


 Subroutine WriteFixedStructuredata

!*********************************************************************
!***                D E L F T         H Y D R A U L I C S
!***
!***              WATER RESOURCES AND ENVIRONMENT DIVISION
!*********************************************************************
!*** Program :  DELFT-3B version 2.00                Date: Mar 1999
!*** Module  :
!*********************************************************************
!*** Created    : March  1999                     By : Geert Prinsen
!*********************************************************************
!*********************************************************************
!*** Brief description:
!*** ------------------
!***   Routine to write areas of nodes in HIS files`
!*********************************************************************
!*** Input/output parameters:     none.
!*** ------------------------
!*********************************************************************
!*** ADRESS     : DELFT HYDRAULICS,
!***              P.O.BOX 177,
!***              2600 MH DELFT, THE NETHERLANDS
!**********************************************************************


! for netCdf
      use netCdfData
      use NetCdf

  IMPLICIT NONE !!!!!!!!!

  Integer iNode, iKind, istr, ityp, idum, TimeSeriesVar
  Integer TmpYear, TmpMonth, TmpDay
  Integer ihour, imin, isec
  Logical Success

  REAL, Pointer, SAVE ::     StructData(:,:)

! NetCdf
  Integer                             jpar, ipos1, i, loc
  Character(Len=40), pointer, dimension(:)  :: locationid
  integer                             nitem, refdate, reftime
  Character(Len=40)                   VariableName
  Character(Len=80)                   LongVariableName
  Character(Len=8)                    unit
  double precision, dimension(:), allocatable       :: X1Coor, Y1Coor

! DIO
  type(DioPltType)                :: DataSet   ! output dataset
  Character(Len=DioMaxStreamLen)  :: outName   ! name of out dataset
  character(len=HisRunIdSize), dimension(HisRunIdDim) :: runId         ! (HIS) runid
  Character(Len=DioMaxParLen), dimension(4)           :: parNames      ! variable(s) in dataset
  Character(Len=DioMaxLocLen), pointer, dimension(:)  :: LocIds      ! locations(s) in dataset
  Character(Len=DioMaxLocLen), pointer, dimension(:)  :: LocDescr      ! location descriptions in dataset


   TimeSeriesVar = -1
   Success = DH_AllocInit (4, NSTR, StructData, 0E0)
   If (.not. success)  call ErrMsgStandard (981, 0, ' Error allocating arrays in subroutine ', ' Output_FixedStructureData' )

! create empty dataset (no name, var. type unknown)
  DataSet = DioPltCreate('NoName', Dio_PLT_Unknown)

! locals
    outName = ConfFil_get_NAMFIL(87)
    IF (outName .NE. ' ' .AND. NCStru .GT. 0) THEN
        Success = DH_AllocInit (NcStru, LocIds, ' ')
        Success = success .and. DH_AllocInit (NcStru, LocDescr, ' ')
        If (.not. success)  call ErrMsgStandard (981, 0, ' Error allocating arrays in subroutine ', ' Output_FixedStructureData' )
        ! header
        runId  = ' '
        runId(1) = CASENM(1:)
        runId(3) = 'TITLE: Fixed structure data       '
        tmpYear = ConfArr_get_IYEAR()
        tmpMonth = ConfArr_get_iMonth()
        tmpDay = ConfArr_get_IDAY()
        ihour  = ConfArr_get_IHour()
        imin   = ConfArr_get_IMinute()
        isec   = ConfArr_get_ISecond()
        WRITE(RunId(4),111) tmpYear, tmpMonth, tmpDay
 111    FORMAT ('T0: ',I4,'.',I2,'.',I2,1X,'00:00:00 (scu=    86400s)')

        ParNames(1) =  'PumpCapacity [m3/s] '
        ParNames(2) =  'CrestLevel Weir [m] '
        ParNames(3) =  'OpeningHeightGate[m]'
        ParNames(4) =  'Manning coefficient '

!       Set the location id's and Compute the data
        Do inode = 1, ncnode
          IKIND = EiNode(inode,3)
          Istr  = EiNode(inode,2)
          IF (IKIND .EQ. 5) THEN
              Structdata(1,istr) = 0
              Structdata(2,istr) = 0
              Structdata(3,istr) = 0
              Structdata(4,istr) = 0
              ityp = STRTYP(istr)
              IF (ityp .eq. 1 .OR. ityp .eq. 8) THEN
                  Structdata(1,istr) = pumpCapacities(istr)%low + pumpCapacities(istr)%high
              ELSEIF (ITYP .EQ. 2 .OR. ityp .eq. 6) THEN
                  Structdata(2,istr) = STRPAR(istr,3)
              ELSEIF (ITYP .EQ. 3 .OR. ityp .eq. 7) THEN
                  Structdata(2,istr) = STRPAR(istr,3)
                  Structdata(3,istr) = STRPAR(istr,4) - STRPAR(istr,3)
              ELSEIF (ITYP .EQ. 4) THEN
                  Structdata(4,istr) = STRPAR(istr,1)
              ENDIF
              LocIds(istr) = Id_Nod(Inode)
              LocDescr(istr) = NamNod(Inode)
          endif
        Enddo
        DataSet = DioPltDefine(outName, runId, Dio_Plt_Real, parNames, LocIds)
        call DioPltAddDescriptions (DataSet, dio_plt_locs, LocDescr)
        deallocate(LocIds)
        deallocate(LocDescr)

        ! Send the data
        idum = 0
        if (GenerateHisOutput) call DioPltPut (DataSet, idum, StructData)
        if (GenerateNetCdfOutput) then
               NetCdfOutputFileName = Outname
               Ipos1 = FndFrst('.his', NetCdfOutputFileName, .false.)
               Ipos1 = max (ipos1, FndFrst('.His', NetCdfOutputFileName(1:), .false.) )
               Ipos1 = max (ipos1, FndFrst('.HIS', NetCdfOutputFileName(1:), .false.) )
               if (ipos1 .le. 0) then
                  call ErrMsgStandard(972, 0, ' Error in setting Netcdf output file name for map output output',' BalanceModule_MapOutputFile')
               endif
               NetCdfOutputFileName(ipos1:) = ''
               NetCdfOutputFileName(ipos1:) = '.nc'
               ! create NetCdf file
               INetCdfFile(RRStrDimNetCdfFileNr) = nc_create (NetCdfOutputFileName)
               nitem = ncstru
               Success = DH_AllocInit (Ncnode, LocationId, ' ')
               loc = 1
               Do i = 1, ncnode
                 IKIND = EiNode(i,3)
                 If (IKIND .EQ. 5) THEN
                    Locationid(loc) = Id_Nod(i)(1:40)
                    loc = loc + 1
                 Endif
               Enddo
               ! set X and Y coordinates for NetCdf files
               if (allocated(X1Coor)) deallocate(X1Coor)
               if (allocated(Y1Coor)) deallocate(Y1Coor)
               allocate(X1Coor(nitem))
               allocate(Y1Coor(nitem))
               Do i=1,nitem
                  X1Coor(i) = dble (XCoor(i))
                  Y1Coor(i) = dble (YCoor(i))
               Enddo
               refdate = tmpYear * 10000 + tmpMonth * 100 + tmpDay
               reftime = ihour * 10000 + Imin * 100 + Isec
               call nc_prepare(iNetCdfFile(RRStrDimNetCdfFileNr), nitem, '', loc_dimid(RRStrDimNetCdfFileNr), time_dimid(RRStrDimNetCdfFileNr), refdate, reftime, time_varid(RRStrDimNetCdfFileNr),locationid, .false., xc=X1Coor,yc=Y1Coor)
               id_locdim = loc_dimid(RRStrDimNetCdfFileNr)
               id_timedim = time_dimid(RRStrDimNetCdfFileNr)
               id_strlendim = 40
!               write(*,*) ' RRStrDimNode nc file'
!               write(*,*) ' id_locdim', id_locdim
!               write(*,*) ' id_timedim', id_timedim
!               write(*,*) ' id_strlendim', id_strlendim
!               write(*,*) ' nitem ', nitem

!              for NetCdf: use parameter names without units (units are specified separately)
               ParNames(1) =  'PumpCapacity'
               ParNames(2) =  'CrestLevel Weir'
               ParNames(3) =  'OpeningHeight Gate'
               ParNames(4) =  'Manning coefficient'
               do jpar=1,4
                  VariableName = Parnames(jpar)
                  LongVariableName = Parnames(jpar)
                  ! Variable name may only contain letters, cijfers and underscores (so no ()*&^%$#@!?><=+-;:.,space)
                  VariableName = NetCdfName(VariableName)
                  if (jpar .eq. 1)                  unit = 'm3 s-1'
                  if (jpar .eq. 2 .or. jpar .eq. 3) unit = 'm'
                  if (jpar .eq. 4)                  unit = 's m-1/3'
                  id_vars(RRStrDimNetCdfFileNr,jpar) = HisSetupVariable (INetCdfFile(RRStrDimNetCdfFileNr), VariableName(1:len_trim(VariableName)), nf90_double, (/ id_locdim, id_timedim /), &
                                                       VariableName(1:len_trim(VariableName)), LongVariableName(1:len_trim(LongVariableName)), unit, TimeSeries=TimeSeriesVar)
               enddo
               ierr = nf90_enddef(INetCdfFile(RRStrDimNetCdfFileNr))
               Call NetCdfCheck(' WriteFixedStructData NetCdf file after Enddef',ierr)
               Deallocate(LocationId)
              ! Put variables
               ierr = nf90_put_var(INetCdfFile(RRStrDimNetCdfFileNr), time_varid(RRStrDimNetCdfFileNr), 0D0, (/ 1 /))
               Call NetCdfCheck(' WriteFixedStructData after Putvar  DDays',ierr)
               do jpar=1,4
                  Do i=1,ncstru
                     ierr = nf90_put_var(INetCdfFile(RRStrDimNetCdfFileNr), id_vars(RRStrDimNetCdfFileNr,jpar), dble(StructData(jpar,i)),(/i,1/))
                     Call NetCdfCheck(' WriteFixedStructData after Putvar DioResult',ierr)
                     if (ierr .ne. nf90_noerr) then
                        write(*,*) ' error for parameter and node index ', jpar, i
                     endif
                  Enddo
               Enddo
               ! close file
               ierr = nf90_close(iNetCdfFile(RRStrDimNetCdfFileNr))
        endif
    Endif

!   De dataset kan direct worden afgesloten, omdat het een eenmalige schrijfactie betreft
    Call CloseDioPlt (Dataset)

    DEALLOCATE ( Structdata )

    Return
    End SubRoutine WriteFixedStructureData



 Subroutine WriteHisHeaderConvergence

! Routine writes header of HIS file with convergence data

  IMPLICIT NONE

  Character*160 String
  Integer       i, TmSize, IYear, Imo, Iday, Ihour, IMin, ISec
  Logical Success


! DIO
  Character(Len=DioMaxStreamLen)  :: outName   ! name of out dataset
  character(len=HisRunIdSize), dimension(HisRunIdDim) :: runId         ! (HIS) runid
  Character(Len=DioMaxParLen), dimension(1)           :: parNames = &  ! variable(s) in dataset
                                                         'Convergence            '
  Character(Len=DioMaxLocLen), pointer, dimension(:)  :: LocIds      ! locations(s) in dataset
  Character(Len=DioMaxLocLen), pointer, dimension(:)  :: LocDescr    ! locations descriptions in dataset

! create empty dataset (no name, var. type unknown)
  DataSetConvergence = DioPltCreate('NoName', Dio_PLT_Unknown)

    outName = 'Convergence.His'
    Success = DH_AllocInit (NcNode, LocIds, ' ')
    Success = success .and. DH_AllocInit (NcNode, LocDescr, ' ')
    If (.not. success)  call ErrMsgStandard (981, 0, ' Error allocating arrays in subroutine ', ' Output_HisHeaderConvergence' )
    runId  = ' '
    runId(1) = CASENM(1:)
    runId(3) = 'TITLE: Convergence results        '
    TmSize = TimeSettings%TimestepSize
    IYEAR = EventStartDateTime(1, 1)
    IMO   = EventStartDateTime(1, 2)
    IDAY  = EventStartDateTime(1, 3)
    IHOUR = EventStartDateTime(1, 4)
    IMIN  = EventStartDateTime(1, 5)
    ISEC  = EventStartDateTime(1, 6)
    Call WriteT0String (String(1:160), IYear, Imo, Iday, Ihour, Imin, Isec, TmSize)
    RunId(4) = String(121:)
    Do i = 1, ncnode
       LocIds(i) = Id_Nod(I)
       LocDescr(i) = NamNod(I)
    Enddo
    DataSetConvergence = DioPltDefine(outName, runId, Dio_Plt_Real, parNames, LocIds)
    Call DioPltAddDescriptions(DataSetConvergence, dio_plt_locs, LocDescr)
    deallocate(LocIds)
    deallocate(LocDescr)

    Converge = 0

    Return
    End Subroutine WriteHisHeaderConvergence



    Subroutine HisInfoConvergence (itmstp)

! for netCdf
      use netCdfData, only : GenerateHisOutput

    Integer itmstp
!    Integer IoHisConvergence


!    Real, Pointer :: DioResult (:,:)
!    Integer  Allocation_error
!    Integer  loc

! Dec 2002: Dio 1.06.02 can also handle 1D array

!   Allocate  ( DioResult(1, NcNode), Stat=Allocation_Error )
!   If (Allocation_Error .ne. 0) &
!       call ErrMsgStandard (981, Allocation_Error, ' Error allocating arrays in subroutine ', &
!                                           ' HISInfoConvergence'  )
!   Do loc=1,NcNode
!      DioResult(1,loc) = Converge(Loc)
!   Enddo
!
!   Call DioPltPut (DataSetConvergence, itmstp, DioResult)
!   Deallocate(DioResult)

    if (GenerateHisOutput) Call DioPltPut (DataSetConvergence, itmstp, Converge)

!    ioHisConvergence =  901
!    Write (IOHisConvergence) ItmStp, (Converge(ILOC),ILOC=1,NcNode)

    Return
    End Subroutine HisInfoConvergence



 Subroutine WriteHisHeaderDailyBndFlows (OutName)

! Routine writes header of HIS file with daily boundary flows

  use NetCdfData
  use NetCdf


  IMPLICIT NONE

  Integer       i, Tmpyear, TmpMonth, tmpDay, TimeSeriesVar
  Integer       ihour, iminute, isecond
  Logical Success

  Character(Len=*)  :: outName   ! name of out dataset
! DIO
  character(len=HisRunIdSize), dimension(HisRunIdDim) :: runId         ! (HIS) runid
  Character(Len=DioMaxParLen), dimension(1)           :: parNames = &  ! variable(s) in dataset
                                                         'Daily Vol.toBnd [m3]'
  Character(Len=DioMaxLocLen), pointer, dimension(:)  :: LocIds      ! locations(s) in dataset
  Character(Len=DioMaxLocLen), pointer, dimension(:)  :: LocDescr      ! location descriptions in dataset

! NetCdf
  Integer                             jpar, ipos1
  Character(Len=40), pointer, dimension(:)  :: locationid
  integer                             nitem, refdate, reftime, ipos, j, ikind
  Character(Len=40)                   VariableName
  Character(Len=80)                   LongVariableName
  double precision, dimension(:), allocatable       :: X1Coor, Y1Coor

  TimeSeriesVar = -1

! create empty dataset (no name, var. type unknown)
  DataSetDailyBndFlows = DioPltCreate('NoName', Dio_PLT_Unknown)

    Success = DH_AllocInit (NcBoun, LocIds, ' ')
    Success = success .and. Dh_AllocInit(NcBoun, LocDescr, ' ')
    If (.not. success)  call ErrMsgStandard (981, 0, ' Error allocating arrays in subroutine ', ' Output_HisHeaderDailyBndFlows' )
    runId  = ' '
    runId(1) = CASENM(1:)
    runId(3) = 'TITLE: Daily totals to boundarynode (m3)'
    TmpYear = ConfArr_get_IYEAR()
    TmpMonth = ConfArr_get_iMonth()
    TmpDay = ConfArr_get_IDAY()
    Ihour   = ConfArr_get_IHour()
    Iminute = ConfArr_get_iMinute()
    Isecond = ConfArr_get_ISecond()
    WRITE(runId(4)(1:),111) tmpYear, tmpMonth, tmpDay
111 FORMAT ('T0: ',I4,'.',I2,'.',I2,1X,'00:00:00 (scu=    86400s)')
    Do i = 1, ncBoun
       LocIds(i) = Id_Nod(BndNam(i))
       LocDescr(i) = NamNod(BndNam(i))
    Enddo
    DataSetDailyBndFlows = DioPltDefine(outName, runId, Dio_Plt_Real, parNames, LocIds)
    Call DioPltAddDescriptions(DataSetDailyBndFlows, dio_plt_locs, LocDescr)
    if (GenerateNetCdfOutput) then
        NetCdfOutputFileName = Outname
        Ipos1 = FndFrst('.his', NetCdfOutputFileName, .false.)
        Ipos1 = max (ipos1, FndFrst('.His', NetCdfOutputFileName(1:), .false.) )
        Ipos1 = max (ipos1, FndFrst('.HIS', NetCdfOutputFileName(1:), .false.) )
        if (ipos1 .le. 0) then
           call ErrMsgStandard(972, 0, ' Error in setting Netcdf output file name for map output output',' BalanceModule_MapOutputFile')
        endif
        NetCdfOutputFileName(ipos1:) = ''
        NetCdfOutputFileName(ipos1:) = '.nc'
        ! create NetCdf file
!       write(*,*) ' NetCdf output - imap  nrlocations', imap, NlcMap(imap)
        INetCdfFile(BndFloTotNetCdfFileNr) = nc_create (NetCdfOutputFileName)
        nitem = Ncboun
        Success = DH_AllocInit (Ncboun, LocationId, ' ')
        Do i=1,Ncboun
           locationid(i)(1:) = LocIds(i)(1:40)
        enddo
        ! set X and Y coordinates for NetCdf files
        allocate(X1Coor(nitem))
        allocate(Y1Coor(nitem))
        Do j=1,ncnode
           ikind = Einode(j,3)
           if (ikind .eq. 6) then
              i = Einode(j,2)
              X1Coor(i) = dble (XCoor(j))
              Y1Coor(i) = dble (YCoor(j))
           endif
        enddo
        refdate = tmpYear * 10000 + tmpMonth * 100 + tmpDay
        reftime = ihour * 10000 + Iminute * 100 + Isecond
        call nc_prepare(iNetCdfFile(BndFloTotNetCdfFileNr), nitem, '', loc_dimid(BndFloTotNetCdfFileNr), time_dimid(BndFloTotNetCdfFileNr), refdate, reftime, time_varid(BndFloTotNetCdfFileNr),locationid, .true., xc=X1Coor,yc=Y1Coor)
        id_locdim = loc_dimid(BndFloTotNetCdfFileNr)
        id_timedim = time_dimid(BndFloTotNetCdfFileNr)
        id_strlendim = 40
!       write(*,*) ' Nc dimensions DailyBndFloTot '
!       write(*,*) ' id_locdim', id_locdim
!       write(*,*) ' id_timedim', id_timedim
!       write(*,*) ' id_strlendim', id_strlendim
!       write(*,*) ' Max Nr daily timesteps ',NetCdfMaxNrDays

        Do jpar=1,1
           VariableName = Parnames(jpar)(1:40)
           LongVariableName = 'Daily Volume to boundary'
           ipos = FndFrst('[',VariableName,.false.)
           if (ipos .gt. 0) then
              VariableName(ipos:) = ''
           endif
           ! Variable name may only contain letters, cijfers and underscores (so no ()*&^%$#@!?><=+-;:.,space)
           VariableName = NetCdfName(VariableName)
           id_vars(BndFloTotNetCdfFileNr,jpar) = HisSetupVariable (INetCdfFile(BndFloTotNetCdfFileNr), VariableName(1:len_trim(VariableName)), nf90_double, (/ id_locdim, id_timedim /), &
                                                 VariableName(1:len_trim(VariableName)), LongVariableName(1:len_trim(LongVariableName)),  'm3', TimeSeries=TimeSeriesVar, AggregationOption='average')
        Enddo
        ierr = nf90_enddef(INetCdfFile(BndFloTotNetCdfFileNr))
        Call NetCdfCheck(' WriteHdrDailyBndFlow NetCdf file after Enddef',ierr)
        Deallocate(LocationId)

    endif
    deallocate(LocIds)
    deallocate(LocDescr)

    QDyBnd = 0

    Return
    End Subroutine WriteHisHeaderDailyBndFlows



    Subroutine HisDailyBndFlows (itmstp, DaysCounter)

      use netCdfData
      use NetCdf
!      use ParallelData, only : JulianStartSimulation, JulianNowSimulation, JulianStartEventSimulation, IDateAct, iTimeAct

!     voor NetCdf output
      Integer       loc, itmstp1

    Integer itmstp, DaysCounter

!
     if (GenerateHisOutput) Call DioPltPut (DataSetDailyBndFlows, itmstp, QDyBnd )

     If (GenerateNetCdfOutput) then
        ! output at timestep (day number)

!  DDays in seconds, minutes, hours or days
        if (trim(NetCdfTimestep) .eq. 'Seconds') then
           DDays = itmstp * 86400.D0
        elseif (trim(NetCdfTimestep) .eq. 'Minutes') then
           DDays = itmstp * 1440.D0
        elseif (trim(NetCdfTimestep) .eq. 'Hours') then
           DDays = itmstp * 24.D0
        elseif (trim(NetCdfTimestep) .eq. 'Days') then
           DDays = itmstp * 1.D0
        endif
!
        IDays = Nint(DDays)
!       write(*,*) ' Daily BNDFloTot  '
!       write(*,*) ' itmstp           ', itmstp
!       write(*,*) ' Ddays            ', DDays
        itmstp1 = itmstp
        itmstp1 = DaysCounter

        ierr = nf90_put_var(INetCdfFile(BndFloTotNetCdfFileNr), time_varid(BndFloTotNetCdfFileNr), DDays, (/ itmstp1 /))
        Call NetCdfCheck(' OutputModule HisDailyBndFlows after Putvar  DDays',ierr)
        ! time series
        Do loc=1,ncboun
           ierr = nf90_put_var(INetCdfFile(BndFloTotNetCdfFileNr), id_vars(BndFloTotNetCdfFileNr,1), dble(QDyBnd(loc)), (/ loc, itmstp1/))
           Call NetCdfCheck(' OutputModule WrtHis after Putvar QDyBnd',ierr)
           if (ierr .ne. 0)  write(*,*) ' Some error occurred'
        Enddo
        if (ierr .ne. 0)  write(*,*) ' Some error occurred'
     endif

    Return
    End Subroutine HisDailyBndFlows


   Subroutine WriteHdrWLMfile (Ievent, WriteOption)

! writes headers 3B WLM HIS file
! if WriteOption=False then no header will be written, but file will only be opened
!    Jan 2003: using DIO, this is done by writing the header and rewinding the file


      IMPLICIT NONE

      INTEGER       I
      Integer       NAMLNG, Ievent
      Integer       IYear, Imo, Iday, iHour, IMin, ISec, TmSize
      CHARACTER*160 Header
      Logical       WriteOption, Success

! DIO
    Character(Len=DioMaxStreamLen)  :: outName   ! name of out dataset
    character(len=HisRunIdSize), dimension(HisRunIdDim) :: runId         ! (HIS) runid
    Character(Len=DioMaxParLen), dimension(3)           :: parNames      ! variable(s) in dataset
    Character(Len=DioMaxLocLen), pointer, dimension(:)  :: LocIds      ! locations(s) in dataset
    Character(Len=DioMaxLocLen), pointer, dimension(:)  :: LocDescr    ! location descriptions in dataset

! create empty dataset (no name, var. type unknown)
  DataSetRRtoWLM = DioPltCreate('NoName', Dio_PLT_Unknown)


! voorlopig alleen maar voor tijdstap 1; later de check inbouwen voor om de x tijdstappen

      OutName = ConfFil_get_NAMFIL(104)

!c 4*char40 identification header
        Header = ' Transferfile RR-WLM'
        NAMLNG = Len_Trim(Header)
        Header (NAMLNG+1:NAMLNG+6) = ' Event'
        Header (NAMLNG+7:NAMLNG+9) = INTCHR ( IOPT1(4) )
        NAMLNG = Len_Trim(Header)

! include T0 string
        IYEAR = EventStartDateTime(IEVENT, 1)
        IMO   = EventStartDateTime(IEVENT, 2)
        IDAY  = EventStartDateTime(IEVENT, 3)
        IHOUR = EventStartDateTime(IEVENT, 4)
        IMIN  = EventStartDateTime(IEVENT, 5)
        ISEC  = EventStartDateTime(IEVENT, 6)
        TmSize = TimeSettings%TimestepSize
        Call WriteT0String (Header, IYear,Imo,Iday,Ihour,Imin,Isec,TmSize)

        Success = DH_AllocInit (NcNode, LocIds, ' ')
        Success = success .and. DH_AllocInit (NcNode, LocDescr, ' ')
        If (.not. success)  call ErrMsgStandard (981, 0, ' Error allocating arrays in subroutine ', ' Output_WriteHdrWlmFile')
!        runId  = ' '
!        runId(1) = CASENM(1:)
!        runId(3) = 'TITLE: Daily totals to boundarynode (m3)'
        runId(1) = Header(1:40)
        runId(4) = Header(121:)
        ParNames(1) = 'Total in [m3] cur'
        ParNames(2) = 'Total out [m3] cur'
        ParNames(3) = 'Delta storage [m3] cur'
        Do i = 1, ncNode
           LocIds(i) = Id_Nod(i)
           LocDescr(i) = NamNod(i)
        Enddo
        DataSetRRtoWLM = DioPltDefine(outName, runId, Dio_Plt_Real, parNames, LocIds)
        Call DioPltAddDescriptions (DataSetRRtoWLM, dio_plt_locs, LocDescr)
        deallocate(LocIds)
        deallocate(LocDescr)

        if (.not. WriteOption)  Call DioPltRewind(DataSetRRtoWLM)

   Return
   End subroutine WriteHdrWLMFile



  Subroutine Write3BWLMHisfile (Ievent, Itmstp)

! writes output 3B WLM HIS file
! Ievent = event number
! Itmstp = timestep

! two modes of calling:
! Ievent=0: series summary output; itmstp=daynumber since start first event
! Ievent>0: detailed output per event, for all timesteps

! Note: no additional NetCdf file, this file is for communiation with WLM only, contains same info as BalansDt.HIS and NC files, file is obselete

   Integer Imap, ILoc, Idebug, Ievent, Itmstp
   Real, Pointer :: DioResult (:,:)
   Logical Success


   Idebug = Conffil_get_idebug()
   IMap   = 8

! detailed HIS files per timestep; NB bij reeks nog niet helemaal nette tijdstap aanduiding in bui 2 e.v.
     if (ExtendedBalanceOutput) Then
        Success = DH_AllocInit (3, NlcMap(imap), DioResult, 0E0)
        If (.not. success)  call ErrMsgStandard (981, 0, ' Error allocating arrays in subroutine ', ' Output_Write3BWLMHisFile')
!       Allocate  ( DioResult(5, NLcMap(imap)), Stat=Allocation_Error )
!       If (Allocation_Error .ne. 0) &
!           call ErrMsgStandard (981, Allocation_Error, ' Error allocating arrays in subroutine ', &
!                                               ' Write3BWLMHISfile'  )
        Do ILOC=1,NLCMAP(imap)
           DioResult(1,iloc) = RSLmap8_BAL(1,ILOC,1) + RSLmap8_BAL(2,ILOC,1)
           DioResult(2,iloc) = RSLmap8_BAL(3,ILOC,1) + RSLmap8_BAL(4,ILOC,1)
           DioResult(3,iloc) = RSLmap8_BAL(5,ILOC,1)
        Enddo
     Else
        Success = DH_AllocInit (3, NlcMap(8), DioResult, 0E0)
        If (.not. success)  call ErrMsgStandard (981, 0, ' Error allocating arrays in subroutine ', ' Output_Write3BWLMHisFile')
!       Allocate  ( DioResult(3, NLcMap(8)), Stat=Allocation_Error )
!       If (Allocation_Error .ne. 0) &
!           call ErrMsgStandard (981, Allocation_Error, ' Error allocating arrays in subroutine ', &
!                                               ' Write3BWLMHISfile'  )
        Do ILOC=1,NLCMAP(IMAP)
           DioResult(1,iloc) = RSLmap8_BAL(1,ILOC,1)
           DioResult(2,iloc) = RSLmap8_BAL(2,ILOC,1)
           DioResult(3,iloc) = RSLmap8_BAL(3,ILOC,1)
        Enddo
     Endif
     Call DioPltPut (DataSetRRtoWLM, itmstp, DioResult)
     Deallocate  ( DioResult )

   Return
   End subroutine Write3BWLMHisFile






   SUBROUTINE DEFHIS (IMAP, IDEBUG, IEVENT, WriteOption)

! *********************************************************************
! *** Delfland / Bergings- en bemalingsbehoefte model
! *** Last update : June 1996            by : Geert Prinsen
! *********************************************************************
! ***         Define HIS file
! ***         ===============
! ***
! *** imap   = map index
! *** casnam = case name
! *** IEvent = Event number
! *** WriteOption = true:  file will be opened + header will be written
! ***               false: file will be opened only
! ***                      Jan 2003: using DIO, this is done by writing the header and rewinding the file
! **********************************************************************

! for netCdf
      use netCdfData
      use NetCdf

      IMPLICIT NONE

      INTEGER       IDEBUG, I, NSERIE, IMAP, Imfl, TmSize, TimeSeriesVar
      Integer       LENGTE, NAMLNG, IMSR, Len, Ievent, JEvent, Ihour, NrSelectedSeries
      Integer       imsr2, imsr2old
      Integer       IYear, Imo, Iday, IMin, ISec
      Logical       WriteOption
      CHARACTER(Len=CharIdLength) FILNAM
      CHARACTER*500 NAME, LongName
      CHARACTER(Len=CharIdLength) SERNAM(NMSR), UNIT(NMSR), LongSerNam(NMSR)

! DIO
  Character(Len=DioMaxStreamLen)  :: outName   ! name of out dataset
  character(len=HisRunIdSize), dimension(HisRunIdDim) :: runId         ! (HIS) runid
  Character(Len=DioMaxParLen), pointer, dimension(:)  :: parNames      ! variable(s) in dataset
  Character(Len=DioMaxParLen), pointer, dimension(:)  :: LongParNames  ! long variable(s) in dataset
  Character(Len=DioMaxLocLen), pointer, dimension(:)  :: LocIds      ! locations(s) in dataset
  Character(Len=DioMaxLocLen), pointer, dimension(:)  :: LocDescr    ! locations descriptions in dataset
  Integer  loc
  Logical Success

! NetCdf
  Integer                             jpar, ipos1
! Character(Len=40)                :: DateString
  Character(Len=40), pointer, dimension(:)  :: locationid
  integer                             nitem, refdate, reftime, ipos, j, ikind
  Character(Len=40)                   Id, VariableName
  Character(Len=80)                   LongVariableName
  Character(Len=7)                    AggregationVar
  double precision, dimension(:), allocatable       :: X1Coor, X2Coor, Y1Coor, Y2Coor

  TimeSeriesVar = -1

      if (idebug .ne. 0)  WRITE (IDEBUG,*) ' Defhis'

      outName = ' '
      TmSize = TimeSettings%TimestepSize
      Imfl   = 1

      if (allocated(X1Coor)) deallocate(X1Coor)
      if (allocated(Y1Coor)) deallocate(Y1Coor)
      if (allocated(X2Coor)) deallocate(Y2Coor)
      if (allocated(Y2Coor)) deallocate(Y2Coor)

!     write(*,*) ' Defhis imap ievent',imap, ievent

!c **********************************************************************
!c *** Define complete HIS - file name
!c **********************************************************************

      if (idebug .ne. 0) then
       write(idebug,*) ' Defhis: imap ', imap
       write(idebug,*) ' Defhis: number of files per map  =', nflmap(imap)
       write(idebug,*) ' Defhis: dirname                  =', dirmap(imap,imfl)
       write(idebug,*) ' Defhis: filename met    extensie =', nmfmap(imap,imfl)
       write(idebug,*) ' Defhis: Nsrmap =', nsrmap(imap)
       write(idebug,*) ' Defhis: Ievent  =', Ievent
       write(idebug,*) ' Defhis: DsrMap =', (DsrMap(imap,imsr),imsr=1,nsrmap(imap))
       write(idebug,*) ' Defhis: NlcMap =', NlcMap(imap)
       write(idebug,*) ' Defhis: IxlMap =', (IxlMap(imap,i),i=1,nlcmap(imap))
       write(idebug,*) ' Defhis: IxfMap =', IxfMap(imap,imfl)
      endif

      FILNAM = ' '
      LEN    = Len_Trim(DIRMAP(IMAP,IMFL))
      IF (LEN .GT. 0) FILNAM = DIRMAP(IMAP,IMFL)(1:Len_Trim(DIRMAP(IMAP,IMFL)))
      LEN    = Len_Trim(FILNAM)
      FILNAM (LEN+1:) = NMFMAP(IMAP,IMFL)(1:Len_Trim(NMFMAP(IMAP,IMFL)))
      outName = FilNam(1:Len_Trim(Filnam))
      if (idebug .ne. 0) then
         write(Idebug,*) ' Defhis Filnam=', Filnam(1:Len_Trim(Filnam))
         write(Idebug,*) ' Defhis Outname=', Outname
      endif

       NrSelectedSeries = 0
       UNIT = ''
       select case (imap)
          case (1)
             Do IMSR=1,NSRMAP(IMAP)
                if (OutputPaved(imsr) .ne. 0) NrSelectedSeries = NrSelectedSeries + 1
             Enddo
          case (2)
             Do IMSR=1,NSRMAP(IMAP)
                if (OutputUnpaved(imsr) .ne. 0) NrSelectedSeries = NrSelectedSeries + 1
             Enddo
          case (3)
             Do IMSR=1,NSRMAP(IMAP)
                if (OutputGreenhouse(imsr) .ne. 0) NrSelectedSeries = NrSelectedSeries + 1
             Enddo
          case (4)
             Do IMSR=1,NSRMAP(IMAP)
                if (OutputOpenWater(imsr) .ne. 0) NrSelectedSeries = NrSelectedSeries + 1
             Enddo
          case (5)
             Do IMSR=1,NSRMAP(IMAP)
                if (OutputStructure(imsr) .ne. 0) NrSelectedSeries = NrSelectedSeries + 1
             Enddo
          case (6)
             Do IMSR=1,NSRMAP(IMAP)
                if (OutputBoundary(imsr) .ne. 0) NrSelectedSeries = NrSelectedSeries + 1
             Enddo
          case (7)
             Do IMSR=1,NSRMAP(IMAP)
                if (OutputNWRW(imsr) .ne. 0) NrSelectedSeries = NrSelectedSeries + 1
             Enddo
          case (8)
             Do IMSR=1,NSRMAP(IMAP)
                if (OutputBalance(imsr) .ne. 0) NrSelectedSeries = NrSelectedSeries + 1
             Enddo
          case (9)
             Do IMSR=1,NSRMAP(IMAP)
                if (OutputSalt(imsr) .ne. 0) NrSelectedSeries = NrSelectedSeries + 1
             Enddo
          case (10)
             Do IMSR=1,NSRMAP(IMAP)
                if (OutputWWTP(imsr) .ne. 0) NrSelectedSeries = NrSelectedSeries + 1
             Enddo
          case (11)
             Do IMSR=1,NSRMAP(IMAP)
                if (OutputIndustry(imsr) .ne. 0) NrSelectedSeries = NrSelectedSeries + 1
             Enddo
          case (12)
             Do IMSR=1,NSRMAP(IMAP)
                if (OutputSacramento(imsr) .ne. 0) NrSelectedSeries = NrSelectedSeries + 1
             Enddo
          case (13)
             Do IMSR=1,NSRMAP(IMAP)
                if (OutputLink(imsr) .ne. 0) NrSelectedSeries = NrSelectedSeries + 1
             Enddo
          case (14)
             Do IMSR=1,NSRMAP(IMAP)
!                if (OutputCell(imsr) .ne. 0) NrSelectedSeries = NrSelectedSeries + 1
             Enddo
          case (15)
             Do IMSR=1,NSRMAP(IMAP)
                if (OutputRRRunoff(imsr) .ne. 0) NrSelectedSeries = NrSelectedSeries + 1
             Enddo
         end select
       NrSelectedSeries = max (1, NrSelectedSeries)

!C     - parameter type, map, time series, dimensions: -
       NSERIE = NSRMAP(IMAP)
!c 4*char40 identification
       NAME = ' '
       LENGTE = Len_Trim(CaseNm)
       IF (LENGTE .GT. 0)  NAME (1:) = CaseNm(1:LENGTE)
       NAMLNG = Len_Trim(NAME)
       NAME (NAMLNG+1:NAMLNG+6) = ' Event'
       NAME (NAMLNG+7:NAMLNG+9) = INTCHR ( IOPT1(4) )
       NAMLNG = Len_Trim(NAME)

       IF (IEVENT .GT. 0 .or. Iopt1(3) .eq. 1) THEN
!Cvoor Netter: detail HIS file voor 1 gebeurtenis
         Jevent = max(1, ievent)
         IYEAR = EventStartDateTime(JEVENT, 1)
         IMO   = EventStartDateTime(JEVENT, 2)
         IDAY  = EventStartDateTime(JEVENT, 3)
         IHOUR = EventStartDateTime(JEVENT, 4)
         IMIN  = EventStartDateTime(JEVENT, 5)
         ISEC  = EventStartDateTime(JEVENT, 6)
         Call WriteT0String (Name, IYear, Imo, Iday, Ihour, Imin, Isec, TmSize)
       ELSE
!Cvoor Netter: HIS file voor reeks: starttijdstip = start gebeurtenis 1
         IYEAR = EventStartDateTime(1, 1)
         IMO   = EventStartDateTime(1, 2)
         IDAY  = EventStartDateTime(1, 3)
         IHOUR = EventStartDateTime(1, 4)
         IMIN  = EventStartDateTime(1, 5)
         ISEC  = EventStartDateTime(1, 6)
         WRITE(NAME(121:160),111) IYEAR, IMO, IDAY, IHOUR, IMIN, ISEC
  111    FORMAT ('T0: ',I4,'.',I2,'.',I2,1X,I2,':',I2,':',I2,'  (scu=   86400s)')
       ENDIF

! create empty dataset (no name, var. type unknown)
       DataSet(imap) = DioPltCreate('NoName', Dio_PLT_Unknown)

!c Header RunId
       RunId(1) = Name(1:)
       RunId(2) = Name(41:)
       select case (imap)
          case (1)
            RunId(3) = 'TITLE: Results of RR-paved nodes'
          case (2)
            RunId(3) = 'TITLE: Results of RR-unpaved nodes'
          case (3)
            RunId(3) = 'TITLE: Results of RR-greenhouse nodes'
          case (4)
            RunId(3) = 'TITLE: Results of RR-open water nodes'
          case (5)
            RunId(3) = 'TITLE: Results of RR-structures'
          case (6)
            RunId(3) = 'TITLE: Results of RR-boundary nodes'
          case (7)
            RunId(3) = 'TITLE: Results of RR-NWRW nodes'
          case (8)
            RunId(3) = 'TITLE: Balances of RR-nodes'
          case (9)
            RunId(3) = 'TITLE: Salt concentrations RR-nodes'
          case (10)
            RunId(3) = 'TITLE: Results of RR-WWTP nodes'
          case (11)
            RunId(3) = 'TITLE: Results of RR-industry nodes'
          case (12)
            RunId(3) = 'TITLE: Results of RR-Sacramento nodes'
          case (13)
            RunId(3) = 'TITLE: Results of RR-links'
          case (14)
            RunId(3) = 'TITLE: Results of RR-Cells'
          case (15)
            RunId(3) = 'TITLE: Results of RR-Runoff nodes'
         end select
       RunId(4) = Name(121:)
       if (idebug .ne. 0) then
          Do i=1,4
             write(Idebug,*) ' Defhis Runid ',i,' =', Runid(i)
          Enddo
       Endif

       Success = DH_AllocInit (NrSelectedSeries, ParNames, ' ')
       Success = DH_AllocInit (NrSelectedSeries, LongParNames, ' ')
       If (.not. success)  call ErrMsgStandard (981, 0, ' Error allocating arrays in subroutine ', ' Output_DEFHIS')
!c construeer char20 voor alle selected series
       SerNam = ''
       LongSerNam = ''
       Imsr2 = 0
!      write(*,*) ' dll_mode =', dll_mode
       DO IMSR=1,MaxSeriesPerMap(IMap) !  was NSRMAP(IMAP) EMRP March 2008;
          NAME = ' '
          LongNAME = ' '
          IF (DSRMAP(IMAP,IMSR) .NE. ' ') THEN
             NAME (1:) = DSRMAP(IMAP,IMSR) (1:Len_Trim(DSRMAP(IMAP,IMSR)))
             LongNAME (1:) = LongDSRMAP(IMAP,IMSR) (1:Len_Trim(LongDSRMAP(IMAP,IMSR)))
             !SH & RP: Ugly fix for hisfile writer, can be removed if either keys are truncated to 20 chars or if we are using netCdf
             !GP 03052017 temporary fix
             !GP     : temporary fix only for dll_mode, to be removed when Dimr Testbank can properly update reference case with new HISfiles,
             !         when DeltaShell supports HIS/HIA,or when NetCdf is standard and HIS/HIS obsolete
             ! GP: only do the next ugly fix for the rr_dll
!            write(*,*) ' Dsrmap(imap,imsr) =', DSRMAP(imap,imsr)(1:40)
             if (dimr_mode) then
                if (DSRMAP(IMAP,IMSR) == 'Storage Land depth [mm]') then
                    NAME (1:) = 'Storage Land   [mm]'
                elseif  (DSRMAP(IMAP,IMSR) == 'Storage Land volume [m3]') then
                    NAME (1:) = 'Storage Land   [m3]'
                elseif  (DSRMAP(IMAP,IMSR) == 'GW above Threshold [hour]') then
                    NAME (1:) ='GW>Threshold [hour]'
                elseif  (DSRMAP(IMAP,IMSR) == 'Mx.above Max.Level [hour]') then
                    NAME (1:) ='Mx.>Max.Level [hour]'
                else
                    NAME (1:) = DSRMAP(IMAP,IMSR) (1:Len_Trim(DSRMAP(IMAP,IMSR)))
                endif
             endif
!            write(*,*) ' NAME =', NAME(1:40)
             UNIT(IMSR)(1:) = UNITS(IMAP,IMSR) (1:Len_Trim(UNITS(IMAP,IMSR)))
          ENDIF
          NAMLNG = Len_Trim(NAME)
          Imsr2old = imsr2
          select case (imap)
            case (1)
                  if (OutputPaved(imsr) .ne. 0) Imsr2 = Imsr2 + 1
            case (2)
                  if (OutputUnpaved(imsr) .ne. 0)  Imsr2 = Imsr2 + 1
            case (3)
                  if (OutputGreenhouse(imsr) .ne. 0) Imsr2 = Imsr2 + 1
            case (4)
                  if (OutputOpenWater(imsr) .ne. 0)  Imsr2 = Imsr2 + 1
            case (5)
                  if (OutputStructure(imsr) .ne. 0)  Imsr2 = Imsr2 + 1
            case (6)
                  if (OutputBoundary(imsr) .ne. 0)  Imsr2 = Imsr2 + 1
            case (7)
                  if (OutputNWRW(imsr) .ne. 0)    Imsr2 = Imsr2 + 1
            case (8)
                  if (OutputBalance(imsr) .ne. 0)  Imsr2 = Imsr2 + 1
            case (9)
                  if (OutputSalt(imsr) .ne. 0)     Imsr2 = Imsr2 + 1
            case (10)
                  if (OutputWWTP(imsr) .ne. 0)     Imsr2 = Imsr2 + 1
            case (11)
                  if (OutputIndustry(imsr) .ne. 0)  Imsr2 = Imsr2 + 1
            case (12)
                  if (OutputSacramento(imsr) .ne. 0) Imsr2 = Imsr2 + 1
            case (13)
                  if (OutputLink(imsr) .ne. 0)  Imsr2 = Imsr2 + 1
            case (14)
                  if (OutputCell(imsr) .ne. 0)  Imsr2 = Imsr2 + 1
            case (15)
                  if (OutputRRRunoff(imsr) .ne. 0) Imsr2 = Imsr2 + 1
          end select
          if (Imsr2 .ne. imsr2old) then
             SERNAM(IMSR2) = NAME(1:)
             LongSERNAM(IMSR2) = LongNAME(1:)
             select case (OutputAtTimestepOption)
                case (1)
                  serNam(iMSr2) = trim(serNam(iMSr2))
                case (2)
                  serNam(iMSr2) = trim(serNam(iMSr2)) // " avg"
                  LongserNam(iMSr2) = trim(longSerNam(iMSr2)) // " avg"
                case (3)
                  serNam(iMSr2) = trim(serNam(iMSr2)) // " max"
                  LongSerNam(iMSr2) = trim(LongSerNam(iMSr2)) // " max"
             end select
          Endif
       EndDo
       ParNames = SerNam
       LongParNames = LongSerNam

! locatie id's
       If (outName .NE. ' ' .AND. NlcMap(imap) .GT. 0) THEN
           Success = DH_AllocInit (NLcMap(imap), LocIds, ' ')
           Success = success .and. DH_AllocInit (NLcMap(imap), LocDescr, ' ')
           If (.not. success)  call ErrMsgStandard (981, 0, ' Error allocating arrays in subroutine ', ' Output_DEFHIS')
           If (imap .le. 12 .or. Imap .ge. 14) then      ! knoop id's
             Do i = 1, NLcMap(imap)
                loc = Iinode(Ixlmap(Imap,i))
                LocIds(i) = Id_Nod(loc)
                LocDescr(i) = NamNod(loc)
             Enddo
           Elseif (imap .eq. 13) then  ! tak id's
             Do i = 1, nclink
                LocIds(i) = NameLink(i)
                LocDescr(i) = LinkDescr(i)
             Enddo
           Endif
           if (idebug .ne. 0) then
               write(Idebug,*) ' Defhis ParNames  ', ParNames
               write(Idebug,*) ' Defhis LocIds    ', LocIds
           Endif
           DataSet(imap) = DioPltDefine(OutName, runId, Dio_Plt_Real, parNames, LocIds)
           Call DioPltAddDescriptions (DataSet(imap), dio_plt_locs, LocDescr)

           if (WriteOption .and. GenerateNetCdfOutput) then
               NetCdfOutputFileName = Outname
               Ipos1 = FndFrst('.his', NetCdfOutputFileName, .false.)
               Ipos1 = max (ipos1, FndFrst('.His', NetCdfOutputFileName(1:), .false.) )
               Ipos1 = max (ipos1, FndFrst('.HIS', NetCdfOutputFileName(1:), .false.) )
               if (ipos1 .le. 0) then
                  call ErrMsgStandard(972, 0, ' Error in setting Netcdf output file name for map output output',' BalanceModule_MapOutputFile')
               endif
               NetCdfOutputFileName(ipos1:) = ''
               NetCdfOutputFileName(ipos1:) = '.nc'
               ! create NetCdf file
!              write(*,*) ' NetCdf output - imap  nrlocations', imap, NlcMap(imap)
               INetCdfFile(imap) = nc_create (NetCdfOutputFileName)
               nitem = NLcMap(imap)  ! nr of location
               Success = DH_AllocInit (NLcMap(imap), LocationId, ' ')
               Do i=1,NLcMap(imap)
                  locationid(i)(1:) = LocIds(i)(1:40)
               enddo
               ! set X and Y coordinates for NetCdf files
               allocate(X1Coor(nitem))
               allocate(Y1Coor(nitem))
               if (imap .eq. 13) then
                  allocate(X2Coor(nitem))
                  allocate(Y2Coor(nitem))
               endif
               if (imap .eq. 8 .or. imap .eq. 9 .or. imap .eq. 13) then
                   ! maps for all nodes or links: balance map, salt map, link flow map
                   Do i=1,nitem
                      if (imap .eq. 8 .or. imap .eq. 9) then
                         j = i
                      elseif (imap .eq. 13) then
                         j = LnkFrm(i)
                      endif
                      X1Coor(i) = dble (XCoor(j))
                      Y1Coor(i) = dble (YCoor(j))
                      if (imap .eq. 13) then
                         j = LnkTo(i)
                         X2Coor(i) = dble (XCoor(j))
                         Y2Coor(i) = dble (YCoor(j))
                      endif
                   enddo
               elseif (imap .ne. 8 .and. imap .ne. 9 .and. imap .ne. 13) then
                   ! other maps
                   Do j=1,ncnode
                      ikind = Einode(j,3)
                      if (MapType(ikind) .eq. imap) then
                         i = Einode(j,2)
                         X1Coor(i) = dble (XCoor(j))
                         Y1Coor(i) = dble (YCoor(j))
                      endif
                   enddo
               endif
               refdate = iYear * 10000 + Imo * 100 + IDay
               reftime = ihour * 10000 + Imin * 100 + Isec
               if (imap .ne. 13) then
                 call nc_prepare(iNetCdfFile(imap), nitem, '', loc_dimid(imap), time_dimid(imap), refdate, reftime, time_varid(imap),locationid, .false., xc=X1Coor,yc=Y1Coor)
               elseif (imap .eq. 13) then
                 call nc_prepare(iNetCdfFile(imap), nitem, '', loc_dimid(imap), time_dimid(imap), refdate, reftime, time_varid(imap),locationid, .false., xc=X1Coor,yc=Y1Coor, xc2=X2Coor,yc2=Y2Coor)
               endif
               id_locdim = loc_dimid(imap)
               id_timedim = time_dimid(imap)
               id_strlendim = 40
!              write(*,*) ' imap ', imap
!              write(*,*) ' Nc dimensions '
!              write(*,*) ' id_locdim', id_locdim
!              write(*,*) ' id_timedim', id_timedim
!              write(*,*) ' id_strlendim', id_strlendim

               Do jpar=1,min(imsr2,NSRMAP(imap))
!                 write(*,*) ' NetCdf output - imap  parameter jpar imsr2 ', imap, jpar, imsr2
                  VariableName = Parnames(jpar)(1:40)
                  LongVariableName = LongParnames(jpar)(1:80)
                  if (dimr_mode) then
                    !GP 03052017 temporary fix
!                    GP: only adjust the above ugly fix for the dll mode, to make sure variable names for NetCdf file are ok again
                     if (VariableName(1:20) == 'Storage Land   [mm]') then
                         VariableName = 'Storage Land depth [mm]'
                     elseif (VariableName(1:20) == 'Storage Land   [m3]') then
                         VariableName = 'Storage Land volume [m3]'
                     elseif (VariableName(1:20) == 'GW>Threshold [hour]') then
                         VariableName = 'GW above Threshold [hour]'
                     elseif (VariableName(1:20) == 'Mx.>Max.Level [hour]') then
                         VariableName = 'Mx.above Max.Level [hour]'
                     else
                        ! no change needed
                     endif
                  endif
!                 write(*,*) ' NetCdf output VariableName', VariableName
                  ipos = FndFrst('[',VariableName,.false.)
                  if (ipos .gt. 0) then
                     VariableName(ipos:) = ''
                     select case (OutputAtTimestepOption)
                        case (1)
                          VariableName = trim(VariableName)
                          AggregationVar = 'current'
                        case (2)
                          VariableName = trim(VariableName)  // " avg"
                          AggregationVar = 'average'
                        case (3)
                          VariableName = trim(VariableName)  // " max"
                          AggregationVar = 'maximum'
                     end select
                  endif
!                 write(*,*) ' NetCdf output VariableName with avg/max ',VariableName
                  ! Variable name may only contain letters, cijfers and underscores (so no ()*&^%$#@!?><=+-;:.,space)
                  VariableName = NetCdfName(VariableName)
!                 write(*,*) ' NetCdf output VariableName with only allowed NetCdf characters', VariableName
                  id_vars(imap,jpar) = HisSetupVariable (INetCdfFile(imap), VariableName(1:len_trim(VariableName)), nf90_double, (/ id_locdim, id_timedim /), &
                                                        VariableName(1:len_trim(VariableName)), LongVariableName(1:len_trim(LongVariableName)), trim(unit(jpar)), &
                                                        TimeSeries=TimeSeriesVar, AggregationOption=AggregationVar)
               Enddo
               ierr = nf90_enddef(INetCdfFile(imap))
               Call NetCdfCheck(' DefHIS NetCdf file imap after Enddef',ierr)
               Deallocate(LocationId)

           endif
           Deallocate(LocIds)
           Deallocate(LocDescr)
       Endif
       Deallocate(ParNames)
       Deallocate(LongParNames)

!c integer voor alle lokaties
!c char20 voor alle lokaties
!c aug 1996: correctie: IXLMAP geeft extern knoopnr ipv intern knoopnr
!      If (imap .le. 12) then      ! knoop id's
!        WRITE (IOUT) ( IXLMAP(IMAP,I), Id_nod20(IINODE(IXLMAP(IMAP,I))),I=1,NLCMAP(IMAP) )
!      Elseif (imap .eq. 13) then  ! tak id's
!        WRITE (IOUT) ( IXLMAP(IMAP,I), NameLink(I)(1:20),I=1,NLCMAP(IMAP) )
!      Endif

! ARS xxxx RR Unix met DelftIO
    If (.not. WriteOption) Call DioPltRewind(DataSet(imap))

    RETURN
    END Subroutine DefHis




    SUBROUTINE WRTHIS (IMAP, IDEBUG, Ievent, Itmstp, OutputNow, NumberOfTimesteps, &
                           RslDet,  OutputSelected, N2, N3, N4)

!C *********************************************************************
!C *** DELFT3B: WRITE HIS FILES
!C ***   Imap   = map number
!C ***   Outputnow = logical, indicating if writing files is needed
!C ***   NumberOfTimesteps = OutputAtTimestepOption; needed for determining averages
!C ***   ievent, itmstp = event and timestep number
!C ***                    ievent=0 for series output.
!C ***   RslDet = output array; N1 to N4 are the four dimensions of this array
!C ***            Actual call e.g. with RslMap1_vhg,
!C *********************************************************************

! for netCdf
      use netCdfData
      use NetCdf
      use ParallelData, only : JulianStartSimulation, JulianNowSimulation, JulianStartEventSimulation, IDateAct, iTimeAct

      implicit none

      INTEGER       IDEBUG, IMAP, IMFL, IMSR, Imsr2, NrSelectedSeries, IOUT, ILOC, Nr
      Integer       N2, N3, N4, NumberOfTimesteps, Itmstp, Ievent
      Real          RslDet(N2,N3,N4)
      Integer       OutputSelected(N2)
      Real, Pointer :: DioResult(:,:)
      Logical       OutputNow, success

!     voor NetCdf output
      Integer       itmstp1
      Integer       IYear, IMo, IDay, IHour, IMin, ISec
      double precision Julian

!     write(*,*) ' Wrthis imap ievent itmstp',imap, ievent, itmstp

      itmstp1 = NrTimestepsPreviousEventsUntilNow + 1
      if (ievent .eq. 0) itmstp1 = NrEventInSeries

      Imfl = 1
      if (idebug .ne. 0) THEN
         WRITE (IDEBUG,*) ' WrtHis'
         WRITE (IDEBUG,'(A,5I5)')  ' IMAP,IMFL,NLCMAP(IMAP) : ',&
                                     IMAP,IMFL,NLCMAP(IMAP)
         WRITE(IDEBUG,*) ' OutputAtTimestepOption',OutputAtTimestepOption
      ENDIF

!c *** Write results for year

      IOUT = IXFMAP(IMAP,IMFL)

      IF (NLCMAP(IMAP) .GT. 0) THEN
        if (imap .ne. 8) then
          select case (OutputAtTimestepOption)
            case(1)  !   current
              Do IMSR=1,NSRMAP(IMAP)
                Do ILOC=1,NLCMAP(IMAP)
                   RSLDET(IMSR,ILOC,2) = RSLDET(IMSR,ILOC,1)
                Enddo
              Enddo
            case(2)  !   average
              Do IMSR=1,NSRMAP(IMAP)
                Do ILOC=1,NLCMAP(IMAP)
                   If (itmstp .eq. 0) then         ! geval voor t=0 apart afhandelen
                      RSLDET(IMSR,ILOC,2) = RSLDET(IMSR,ILOC,1)
                   Else
                      RSLDET(IMSR,ILOC,2) = RSLDET(IMSR,ILOC,2) + RSLDET(IMSR,ILOC,1)
                   Endif
                Enddo
              Enddo
            case(3)  !   maximum
              Do IMSR=1,NSRMAP(IMAP)
                Do ILOC=1,NLCMAP(IMAP)
                   If (itmstp .eq. 0) then         ! geval voor t=0 apart afhandelen
                      RSLDET(IMSR,ILOC,2) = RSLDET(IMSR,ILOC,1)
                   Else
                      RSLDET(IMSR,ILOC,2) = Max (RSLDET(IMSR,ILOC,2), RSLDET(IMSR,ILOC,1) )
                   Endif
                Enddo
              Enddo
          end select
! Schrijf uitvoer en initialiseer opnieuw
          if (outputNow) then
             NrSelectedSeries = 0
             Do IMSR=1,NSRMAP(IMAP)
                if (OutputSelected(imsr) .ne. 0) NrSelectedSeries = NrSelectedSeries + 1
             Enddo
             NrSelectedSeries = max(1, NrSelectedSeries)
             Success = DH_AllocInit (NrSelectedSeries, NLcMap(imap), DIOResult, 0E0)
             If (.not. success)  call ErrMsgStandard (981, 0, ' Error allocating arrays in subroutine ', ' Output_WrtHis')
! ARS 10903 output for selected series only
             Imsr2 = 0
             Do IMSR=1,NSRMAP(IMAP)
                if (OutputSelected(imsr) .ne. 0) then
                   imsr2 = imsr2 + 1
                   Do ILOC=1,NLCMAP(IMAP)
                      DioResult(Imsr2,ILoc) = RSLDET(IMSR,ILOC,2)
                   Enddo
                endif
             Enddo
! end ARS
             select case (OutputAtTimestepOption)
               case(1)  !   current
                 if (GenerateHisOutput) Call DioPltPut (DataSet(imap), Itmstp, DioResult)
               case(2)  !   average
                 DioResult = DioResult / NumberOfTimesteps
                 if (GenerateHisOutput) Call DioPltPut (DataSet(imap), Itmstp, DioResult)
               case(3)  !   maximum
                 if (GenerateHisOutput) Call DioPltPut (DataSet(imap), Itmstp, DioResult)
             end select
             ! output to netcdf
             if (GenerateNetCdfOutput) then
               ! output at timestep itmstep,
                iYear = ConfArr_get_IYEAR()
                iMo   = ConfArr_get_iMonth()
                iDay  = ConfArr_get_IDAY()
                iHour = ConfArr_get_IHour()
                iMin  = ConfArr_get_IMinute()
                iSec  = ConfArr_get_ISecond()
!               write(*,*) ' imap ', imap
!               write(*,*) ' Nc dimensions '
!               write(*,*) ' Current Timestep WrtHis', itmstp1
!               write(*,*) ' JulianNowSimulation  ', JulianNowSimulation
!               write(*,*) ' JulianStartSimulation', JulianStartSimulation
                ! adjust JulianTime for NetCdfOutput
                IDateAct = ConfArr_get_IYEAR()*10000 + ConfArr_get_iMonth()* 100 + ConfArr_get_IDAY()
                ITimeAct = ConfArr_get_iHour()*10000 + ConfArr_get_iMinute()* 100 + ConfArr_get_iSecond()
                JulianNowSimulation = Julian(IdateAct, ITimeAct)
                if (itmstp .ge. 1) JulianNowSimulation = JulianNowSimulation + timeSettings%timestepSize /86400.D0

!               write(*,*) ' JulianNowSimulation again ', JulianNowSimulation
!  DDays in seconds, minutes, hours or days
                if (trim(NetCdfTimestep) .eq. 'Seconds') then
                   DDays = (JulianNowSimulation - JulianStartSimulation) * 86400.D0
                elseif (trim(NetCdfTimestep) .eq. 'Minutes') then
                   DDays = (JulianNowSimulation - JulianStartSimulation) * 1440.D0
                elseif (trim(NetCdfTimestep) .eq. 'Hours') then
                   DDays = (JulianNowSimulation - JulianStartSimulation) * 24.D0
                elseif (trim(NetCdfTimestep) .eq. 'Days') then
                   DDays = (JulianNowSimulation - JulianStartSimulation) * 1.D0
                endif
!               write(*,*) ' Ddays            ', DDays

! Ievent=0: Series output
                if (ievent .eq. 0) then
!                  DDays in seconds, minutes, hours or days
                   if (trim(NetCdfTimestep) .eq. 'Seconds') then
                      DDays = (JulianStartEventSimulation - JulianStartSimulation) * 86400.D0
                   elseif (trim(NetCdfTimestep) .eq. 'Minutes') then
                      DDays = (JulianStartEventSimulation - JulianStartSimulation) * 1440.D0
                   elseif (trim(NetCdfTimestep) .eq. 'Hours') then
                      DDays = (JulianStartEventSimulation - JulianStartSimulation) * 24.D0
                   elseif (trim(NetCdfTimestep) .eq. 'Days') then
                      DDays = (JulianStartEventSimulation - JulianStartSimulation) * 1.D0
                   endif
                endif
                IDays = Nint(DDays)

!               write(*,*) ' JulianNowSimulation ', JulianNowSimulation
!               write(*,*) ' JulianStartSimulation ', JulianStartSimulation
!               write(*,*) ' JulianStartEventSimulation ', JulianStartEventSimulation
!               write(*,*) ' ievent ', ievent
!               write(*,*) ' Ddays            ', DDays
!               ierr = nf90_put_var(INetCdfFile(imap), time_varid(imap), DDays, (/ itmstp1 /))
                ierr = nf90_put_var(INetCdfFile(imap), time_varid(imap), IDays, (/ itmstp1 /))
                Call NetCdfCheck(' OutputModule WrtHis after Putvar  DDays',ierr)
                ! time series
                imsr2 = 0
                Do Imsr = 1,NsrMap(imap)
                   if (OutputSelected(imsr) .ne. 0) then
                      imsr2 = imsr2 + 1
                      ierr = nf90_put_var(INetCdfFile(imap), id_vars(imap,imsr2), DioResult(imsr2,1:NLCMAP(IMAP)), (/1,itmstp1/))

                      !Do ILOC=1,NLCMAP(IMAP)
                      !
                      !    ierr = nf90_put_var(INetCdfFile(imap), id_vars(imap,imsr2), DioResult(imsr2,iloc), (/iloc, itmstp1/))
                      !   Call NetCdfCheck(' OutputModule WrtHis after Putvar DioResult',ierr)
                      !Enddo
                      if (ierr .ne. 0)  write(*,*) ' Some error occurred'
                   endif
                Enddo
             endif
             ! re-initialise for next timestep
             Do IMSR=1,NSRMAP(IMAP)
                Do ILOC=1,NLCMAP(IMAP)
                   if (OutputAtTimestepOption .le. 2)  RSLDET(IMSR,ILOC,2) = 0.0
                   if (OutputAtTimestepOption .eq. 3)  RSLDET(IMSR,ILOC,2) = -999.99
                Enddo
             Enddo
             Deallocate (DioResult)
          endif
        elseif (imap .eq. 8) then
! afwijkend: balans uitvoer kaart 8: altijd sommeren.
          Do IMSR=1,NSRMAP(IMAP)
             Do ILOC=1,NLCMAP(IMAP)
                RSLDET(IMSR,ILOC,2) = RSLDET(IMSR,ILOC,2) + RSLDET(IMSR,ILOC,1)
             Enddo
          Enddo
          if (outputNow) then
             Success = DH_AllocInit (NSRMap(imap), NLcMap(imap), DIOResult, 0E0)
             If (.not. success)  call ErrMsgStandard (981, 0, ' Error allocating arrays in subroutine ', ' Output_WrtHis')
             if (NumberOfTimesteps .gt. 1) then
               if (Nevent .gt. 1 .and. Iopt1(3) .eq. 0) then
!                 geen acties uitvoer cumulatieve balans
               else
                  Nr = NSRMap(imap) / 2
                  Do IMSR=1,NR
                     Do ILOC=1,NLCMAP(IMAP)
                        BckCumBal(imsr,iloc,1) = RSLDET(IMSR,ILOC,2)
                     Enddo
                  Enddo
                  Do IMSR=1,Nr
                     Do ILOC=1,NLCMAP(IMAP)
                        BckCumBal(imsr,iloc,2) = BckCumBal(imsr, iloc,2) + BckCumBal(imsr,iloc,1)
                     Enddo
                  Enddo
               endif
               Do IMSR=1,NR
                  Do ILOC=1,NLCMAP(IMAP)
                     DioResult(Imsr,ILoc) = RSLDET(IMSR,ILOC,2)
                  Enddo
               Enddo
               Do IMSR=1,NR
                  Do ILOC=1,NLCMAP(IMAP)
                     DioResult(Nr+Imsr,ILoc) = BckCumBal(Imsr,ILOC,2)
                  Enddo
               Enddo
!              Write (IOUT) ItmStp, ((RSLDET(IMSR,ILOC,2),IMSR=1,NR),(BckCumBal(imsr,iloc,2),IMSR=1,Nr),ILOC=1,NLCMAP(IMAP))
               if (GenerateHisOutput) Call DioPltPut (DataSet(imap), ItmStp, DioResult)
             else
! normale situatie, output every timestep
               Do IMSR=1,NSRMAP(IMAP)
                  Do ILOC=1,NLCMAP(IMAP)
                     DioResult(Imsr,ILoc) = RSLDET(IMSR,ILOC,2)
                  Enddo
               Enddo
               if (GenerateHisOutput) Call DioPltPut (DataSet(imap), ItmStp, DioResult)
             endif

             ! output to netcdf
             if (GenerateNetCdfOutput) then
!               write(*,*) ' imap ', imap
!               write(*,*) ' Current Timestep WrtHis', itmstp1
!               write(*,*) ' JulianNowSimulation  ', JulianNowSimulation
!               write(*,*) ' JulianStartSimulation', JulianStartSimulation
                ! adjust JulianTime for NetCdfOutput
                IDateAct = ConfArr_get_IYEAR()*10000 + ConfArr_get_iMonth()* 100 + ConfArr_get_IDAY()
                ITimeAct = ConfArr_get_iHour()*10000 + ConfArr_get_iMinute()* 100 + ConfArr_get_iSecond()
                JulianNowSimulation = Julian(IdateAct, ITimeAct)
                if (itmstp .ge. 1) JulianNowSimulation = JulianNowSimulation + timeSettings%timestepSize /86400.D0

!               write(*,*) ' JulianNowSimulation again ', JulianNowSimulation
!               DDays in seconds, minutes, hours or days
                if (trim(NetCdfTimestep) .eq. 'Seconds') then
                   DDays = (JulianNowSimulation - JulianStartSimulation) * 86400.D0
                elseif (trim(NetCdfTimestep) .eq. 'Minutes') then
                   DDays = (JulianNowSimulation - JulianStartSimulation) * 1440.D0
                elseif (trim(NetCdfTimestep) .eq. 'Hours') then
                   DDays = (JulianNowSimulation - JulianStartSimulation) * 24.D0
                elseif (trim(NetCdfTimestep) .eq. 'Days') then
                   DDays = (JulianNowSimulation - JulianStartSimulation) * 1.D0
                endif

                if (ievent .eq. 0) then
!                  DDays in seconds, minutes, hours or days
                   if (trim(NetCdfTimestep) .eq. 'Seconds') then
                      DDays = (JulianStartEventSimulation - JulianStartSimulation) * 86400.D0
                   elseif (trim(NetCdfTimestep) .eq. 'Minutes') then
                      DDays = (JulianStartEventSimulation - JulianStartSimulation) * 1440.D0
                   elseif (trim(NetCdfTimestep) .eq. 'Hours') then
                      DDays = (JulianStartEventSimulation - JulianStartSimulation) * 24.D0
                   elseif (trim(NetCdfTimestep) .eq. 'Days') then
                      DDays = (JulianStartEventSimulation - JulianStartSimulation) * 1.D0
                   endif
                endif
                IDays = Nint(DDays)
!               write(*,*) ' Ddays            ', DDays
!               ierr = nf90_put_var(INetCdfFile(imap), time_varid(imap), DDays, (/ itmstp1 /))
                ierr = nf90_put_var(INetCdfFile(imap), time_varid(imap), IDays, (/ itmstp1 /))
                Call NetCdfCheck(' OutputModule WrtHis after Putvar  DDays',ierr)
                ! time series
                imsr2 = 0
                Do Imsr = 1,NsrMap(imap)
                   if (OutputSelected(imsr) .ne. 0) then
                      imsr2 = imsr2 + 1
                      Do ILOC=1,NLCMAP(IMAP)
!                        write(*,*) ' Put variable ',imsr2, id_vars(imap,imsr2), ' value        ',DioResult(imsr2,iloc)
                         ierr = nf90_put_var(INetCdfFile(imap), id_vars(imap,imsr2), dble(DioResult(imsr2,iloc)), (/ iloc, itmstp1 /))
                         Call NetCdfCheck(' OutputModule WrtHis after Putvar DioResult',ierr)
                      Enddo
                      if (ierr .ne. 0)  write(*,*) ' Some error occurred'
                   endif
                Enddo

             endif
! End
             Do IMSR=1,NSRMAP(IMAP)
                Do ILOC=1,NLCMAP(IMAP)
                   RSLDET(IMSR,ILOC,2) = 0.0
                Enddo
             Enddo
             Deallocate (DioResult)
          endif
        endif
      ENDIF


      RETURN
    END Subroutine WrtHis



   Subroutine WriteHdr3BHisfiles (Ievent, WriteOption)

! writes headers 3B His files
! if WriteOptions=False then no header will be written, but file will only be opened
!                             Jan 2003: using DIO, header is written, but file is rewinded)

   Integer Imap, Idebug, Ievent
   Logical WriteOption

!  Write(*,*) ' WriteHdr3BHisFiles ievent writeoption', ievent, writeoption

   Idebug = Conffil_get_idebug()

! Aanpassing 9 april 1999: geen ..MX.HIS files meer
! Zorg daarom dat DIRMAP netjes is aangepast.

! Oktober 2001: 0 of 1 HIS uitvoerfile per map

!  Write(*,*) ' WriteHdr3BHisFiles nevent nkaart ', nevent, nkaart
   If (Nevent .eq. 1) then
     ! header HIS files detailed output per timestep
     Do IMAP=1,NKAART
        if (NFlMap(imap) .gt. 0 .and. NlcMap(imap) .gt. 0 .and. outputDesired(imap)) then
           Call DEFHIS (IMAP, IDEBUG, Ievent, WriteOption)
        endif
     Enddo
   Else
     ! Header summarized output per event
     Do IMAP=1,NKAART
        if (NflMap(imap) .gt. 0 .and. NlcMap(imap) .gt. 0) then
           Call DEFHIS (IMAP, IDEBUG, 0, WriteOption)
        endif
     Enddo
   Endif


   Return
   End subroutine WriteHdr3BHisFiles



  Subroutine Write3BHisfiles (Ievent, Itmstp, LastTm)

! writes output 3B His files
! Ievent = event number
! Itmstp = timestep
! Lasttm = last timestep of event

! two modes of calling:
! Ievent=0: series summary output; itmstp=daynumber since start first event
! Ievent>0: detailed output per event, for all timesteps

   use NetCdfData, only : NrTimestepsPreviousEventsUntilNow

   Integer Imap, Idebug, Ievent, Itmstp, LastTm, ihelp, NumberOfTimeSteps
   Logical OutputNow

   Idebug = Conffil_get_idebug()
!  write(*,*) ' Call Write3BHisFiles', ievent, itmstp, lasttm


  If (Ievent .ge. 1) then
! detailed HIS files per output timestep
! zet eerst OutputNow op basis van Integer deling
     OutputNow = .false.
     NumberOfTimesteps = OutputAtTimestep
     ihelp = ITmstp / OutputAtTimestep
     if (itmstp .eq. 0) then
        OutputNow=.true.
        NumberOfTimeSteps = 1
     elseif (ihelp*OutputAtTimestep .eq. Itmstp) then
        OutputNow=.true.
     elseif (itmstp .eq. lasttm) then
        OutputNow=.true.
        NumberOfTimeSteps = Lasttm - ihelp*OutputAtTimestep
     endif

     Do IMAP=1,NKAART
       If (IOPT1(1) .EQ. 1) Then
        If (OutputDesired(imap)) then
          select case (imap)
              case (1)
                  Call WrtHis (IMAP, IDEBUG, Ievent, Itmstp, OutputNow, NumberOfTimesteps, &
                               Rslmap1_vhg, OutputPaved, MaxSeriesPerMap(imap), Nvhg, 2 )
              case (2)
                   Call WrtHis (IMAP, IDEBUG, Ievent, Itmstp, OutputNow, NumberOfTimesteps, &
                                Rslmap2_ovh, OutputUnpaved, MaxSeriesPerMap(imap), Novh, 2)
              case (3)
                   Call WrtHis (IMAP, IDEBUG, Ievent, Itmstp, OutputNow, NumberOfTimesteps, &
                                Rslmap3_kas, OutputGreenhouse, MaxSeriesPerMap(imap), Nkas, 2)
              case (4)
                   Call WrtHis (IMAP, IDEBUG, Ievent, Itmstp, OutputNow, NumberOfTimesteps, &
                                Rslmap4_ow , OutputOpenWater, MaxSeriesPerMap(imap), Now , 2)
              case (5)
                   Call WrtHis (IMAP, IDEBUG, Ievent, Itmstp, OutputNow, NumberOfTimesteps, &
                                Rslmap5_str, OutputStructure, MaxSeriesPerMap(imap), Nstr, 2)
              case (6)
                   Call WrtHis (IMAP, IDEBUG, Ievent, Itmstp, OutputNow, NumberOfTimesteps, &
                                Rslmap6_bnd, OutputBoundary, MaxSeriesPerMap(imap), Nbnd, 2)
              case (7)
                   Call WrtHis (IMAP, IDEBUG, Ievent, Itmstp, OutputNow, NumberOfTimesteps, &
                                Rslmap7_plv, OutputNWRW, MaxSeriesPerMap(imap), Nplv, 2)
              case (8)
                   Call WrtHis (IMAP, IDEBUG, Ievent, Itmstp, OutputNow, NumberOfTimesteps, &
                                Rslmap8_bal, OutputBalance, MaxSeriesPerMap(imap), Nnod, 2)
              case (9)
                  if (islcmp .eq. 0) then
                    Call WrtHis (IMAP, IDEBUG, Ievent, Itmstp, OutputNow, NumberOfTimesteps, &
                                 Rslmap9_slt, OutputSalt, MaxSeriesPerMap(imap), 1, 2)
                  else
                    Call WrtHis (IMAP, IDEBUG, Ievent, Itmstp, OutputNow, NumberOfTimesteps, &
                                 Rslmap9_slt, OutputSalt, MaxSeriesPerMap(imap), Nnod, 2)
                  endif
              case (10)
                   Call WrtHis (IMAP, IDEBUG, Ievent, Itmstp, OutputNow, NumberOfTimesteps, &
                                Rslmap14_rwzi, OutputWWTP, MaxSeriesPerMap(imap), Nrwzi, 2)
              case (11)
                   Call WrtHis (IMAP, IDEBUG, Ievent, Itmstp, OutputNow, NumberOfTimesteps, &
                                Rslmap15_ind, OutputIndustry, MaxSeriesPerMap(imap), Nindus, 2)
              case (12)
! Sacramento
                   Call WrtHis (IMAP, IDEBUG, Ievent, Itmstp, OutputNow, NumberOfTimesteps, &
                                Rslmap17_Sacr, OutputSacramento, MaxSeriesPerMap(imap), NcSacr, 2)
              case (13)
! Link flows
                   Call WrtHis (IMAP, IDEBUG, Ievent, Itmstp, OutputNow, NumberOfTimesteps, &
                                Rslmap16_flows, OutputLink, MaxSeriesPerMap(imap), Nclink, 2)
              case (14)
! Cells
!                  Call WrtHis (IMAP, IDEBUG, Ievent, Itmstp, OutputNow, NumberOfTimesteps, &
!                                Rslmap18_cel, OutputCell, MaxSeriesPerMap(imap), NcCell, 2)
              case (15)
! Runoff nodes
                   Call WrtHis (IMAP, IDEBUG, Ievent, Itmstp, OutputNow, NumberOfTimesteps, &
                                Rslmap19_RRRunoff, OutputRRRunoff, MaxSeriesPerMap(imap), NcRRRunoff, 2)
          end select
        Endif
       Endif
     Enddo
     if (OutputNow) NrTimestepsPreviousEventsUntilNow = NrTimestepsPreviousEventsUntilNow + 1

   Elseif (Ievent .eq. 0) then
! Summary output per event
     NrTimestepsPreviousEventsUntilNow = NrTimestepsPreviousEventsUntilNow + 1
!    write(*,*) ' Call Write3BSeriesHisFiles', ievent, itmstp, lasttm
     Call Write3BSeriesHisfiles (Ievent, Itmstp, LastTm)
   Endif

   Return
   End subroutine Write3BHisFiles



  Subroutine Write3BSeriesHisfiles (Ievent, Itmstp, LastTm)

! writes output 3B His files series
! Ievent = event number
! Itmstp = timestep
! Lasttm = last timestep of event

! two modes of calling:
! Ievent=0: series summary output; itmstp=daynumber since start first event
! Ievent>0: detailed output per event, for all timesteps

   use NetCdfData, only : NrTimestepsPreviousEventsUntilNow

   Integer Imap, Idebug, Ievent, Itmstp, LastTm, NumberOfTimeSteps
   Logical OutputNow

   Idebug = Conffil_get_idebug()


! Summary output per event

     OutputNow = .true.
     NumberOfTimesteps = 1
     Do IMAP=1,NKAART
      If (OutputDesired(imap)) then
        select case (imap)
             case (1)
                Call WrtHis (IMAP, IDEBUG, Ievent, Itmstp, OutputNow, NumberOfTimesteps, &
                             Rslmap1_vhg, OutputPaved, MaxSeriesPerMap(imap), Nvhg, 2)
             case (2)
                Call WrtHis (IMAP, IDEBUG, Ievent, Itmstp, OutputNow, NumberOfTimesteps, &
                             Rslmap2_ovh, OutputUnpaved, MaxSeriesPerMap(imap), Novh, 2)
             case (3)
                Call WrtHis (IMAP, IDEBUG, Ievent, Itmstp, OutputNow, NumberOfTimesteps, &
                             Rslmap3_kas, OutputGreenhouse, MaxSeriesPerMap(imap), Nkas, 2)
             case (4)
                Call WrtHis (IMAP, IDEBUG, Ievent, Itmstp, OutputNow, NumberOfTimesteps, &
                             Rslmap4_ow , OutputOpenWater, MaxSeriesPerMap(imap), Now , 2)
             case (5)
                Call WrtHis (IMAP, IDEBUG, Ievent, Itmstp, OutputNow, NumberOfTimesteps, &
                             Rslmap5_str, OutputStructure, MaxSeriesPerMap(imap), Nstr, 2)
             case (6)
                Call WrtHis (IMAP, IDEBUG, Ievent, Itmstp, OutputNow, NumberOfTimesteps, &
                             Rslmap6_bnd, OutputBoundary, MaxSeriesPerMap(imap), Nbnd, 2)
             case (7)
                Call WrtHis (IMAP, IDEBUG, Ievent, Itmstp, OutputNow, NumberOfTimesteps, &
                             Rslmap7_plv, OutputNWRW, MaxSeriesPerMap(imap), Nplv, 2)
             case (8)
                Call WrtHis (IMAP, IDEBUG, Ievent, Itmstp, OutputNow, NumberOfTimesteps, &
                             Rslmap8_bal, OutputBalance, MaxSeriesPerMap(imap), Nnod, 2)
             case (9)
                if (islcmp .eq. 0) then
                   Call WrtHis (IMAP, IDEBUG, Ievent, Itmstp, OutputNow, NumberOfTimesteps, &
                                Rslmap9_slt, OutputSalt, MaxSeriesPerMap(imap),  1, 2)
                else
                   Call WrtHis (IMAP, IDEBUG, Ievent, Itmstp, OutputNow, NumberOfTimesteps, &
                                Rslmap9_slt, OutputSalt, MaxSeriesPerMap(imap), Nnod, 2)
                endif
             case (10)
                Call WrtHis (IMAP, IDEBUG, Ievent, Itmstp, OutputNow, NumberOfTimesteps, &
                             Rslmap14_rwzi, OutputWWTP, MaxSeriesPerMap(imap),  Nrwzi, 2)
             case (11)
                Call WrtHis (IMAP, IDEBUG, Ievent, Itmstp, OutputNow, NumberOfTimesteps, &
                             Rslmap15_ind,  OutputIndustry, MaxSeriesPerMap(imap), Nindus, 2)
! Sacramento
             case (12)
                Call WrtHis (IMAP, IDEBUG, Ievent, Itmstp, OutputNow, NumberOfTimesteps, &
                             Rslmap17_sacr, OutputSacramento, MaxSeriesPerMap(imap),  NcSacr, 2)
! Link flows
             case (13)
                Call WrtHis (IMAP, IDEBUG, Ievent, Itmstp, OutputNow, NumberOfTimesteps, &
                             Rslmap16_flows, OutputLink, MaxSeriesPerMap(imap), Nclink, 2)
! Cells
             case (14)
!                Call WrtHis (IMAP, IDEBUG, Ievent, Itmstp, OutputNow, NumberOfTimesteps, &
!                             Rslmap18_cel, OutputCell, MaxSeriesPerMap(imap), NcCell, 2)
! RR Runoff
             case (15)
                Call WrtHis (IMAP, IDEBUG, Ievent, Itmstp, OutputNow, NumberOfTimesteps, &
                             Rslmap19_RRRunoff, OutputRRRunoff, MaxSeriesPerMap(imap),  NcRRRunoff, 2)
        end select
      Endif
     Enddo
     if (OutputNow) NrTimestepsPreviousEventsUntilNow = NrTimestepsPreviousEventsUntilNow + 1


   Return
   End subroutine Write3BSeriesHisfiles



   Subroutine Init3BHisVariables (Ievent, LastTm, DefaultT0OutputValue, RestartVersion)

! writes output 3B His files for t=0
! Ievent = event number
! Itmstp = timestep

   Integer Ievent, Lasttm, iout1
   Real    DefaultT0OutputValue
   Character(len=32) RestartVersion

   double precision dtemp
   Real     Area, BTot, BMax, RVolOp, RVolDn
   Integer  N4

   Integer Inode, ikind, inr, ihelp, imap, iloc,ikkl, iptyp, ipopp, j, iplv2, iplv3, iplv4, IRRRunoffSub

! local: N4 dimension of arrays
   N4 = 2

! zet initiele waterstanden en grondwaterstanden in uitvoer

   Do INODE=1,NCNODE
      IKIND = EiNode(INODE,3)
      INR   = EiNode(INODE,2)
      IMAP  = IKIND
      ILOC  = INR
      if (islcmp .ne. 0) RslMap9_slt(1,inode,1) = SltIni(inode)  ! ARS 16155
      if (ikind .eq. 1) then
!        default storage in mm
         RSLMAP1_vhg (1,ILOC,1) =  0.0
         RSLMAP1_vhg (2,ILOC,1) =  0.0
         RSLMAP1_vhg (3,ILOC,1) =  0.0
         If (AreaVh(inr) .gt. 0) then
!          Set storage RWA sewer, DWA sewer, street
           RSLMAP1_vhg (1,ILOC,1) =  BVRL(INR,1) / AreaVh(inr) * 1000
           RSLMAP1_vhg (2,ILOC,1) =  BVRL(INR,2) / AreaVh(inr) * 1000
           RSLMAP1_vhg (3,ILOC,1) =  BVSTR(INR) / AreaVh(inr) *1000.
         Endif
      elseif (ikind .eq. 2) then
         RSLMAP2_ovh (11,ILOC,1) =  GWL(INR)
         RSLMAP2_ovh (17,ILOC,1) =  GWL(INR) - LVLOH(INR)
!        IF (IOPT2(1) .EQ. 1) RSLMAP2_ovh(11,ILOC,1) = GWL(INR) - LVLOH(INR)
!c       storage land in mm
         RSLMAP2_ovh(13,ILOC,1) =  BOLND(INR) / AreaOH(INR) * 1000.
         RSLMAP2_ovh(14,ILOC,1) =  BOBD(INR)
         RSLMAP2_ovh(16,ILOC,1) =  Max (0.0, GWL(INR)-MaxGwl2(INR))
!        unsaturated zone
         RSLMAP2_ovh(18,ILOC,1) =  0.0
         RSLMAP2_ovh(19,ILOC,1) =  0.0
!        unsaturated zone
         RSLMAP2_ovh(20,ILOC,1) =  BERGC(INR)
         RSLMAP2_ovh(21,ILOC,1) =  OnvZone(INR)%Actual_mm
         RSLMAP2_ovh(22,ILOC,1) =  OnvZone(INR)%Actual_Volume
!c       storage land in m3
         RSLMAP2_ovh(15,ILOC,1) =  BOLND(INR)
!        ARS 14034 add Initial percentage inundation
         IOUT1 = ConfFil_get_IOUT1()
         Call DeterminePercentageInundationUnpaved(inr, inode, dtemp,iout1)
         rslmap2_ovh(12,iloc,1) = dtemp
!
      elseif (ikind .eq. 3) then
!        Initieel volume kassen
         BTOT = 0.
         BMAX = 0.
         AREA = 0.
         DO IKKL=1,NCKKL
            BMAX = BMAX + KKLMXB(IKKL) * AREAKK(INR,IKKL)
            BTOT = BTOT + BKAS(INR,IKKL)
            AREA = AREA + AREAKK(INR,IKKL)
         ENDDO
         BMAX = BMAX + SILOC(INR) * AREAS(INR)
         BTOT = BTOT + SILOB(INR)
         AREA = AREA + AREAS(INR)
!        Default volume in m3
         RSLMAP3_kas(1,ILOC,1) = BTOT
      elseif (ikind .eq. 4) then
         RSLMAP4_ow(1,ILOC,1) =  LVLOW(INR)
!        IF (IOPT2(2) .EQ. 1) RSLMAP4_ow(1,ILOC,1) = LVLOW(INR) - REFLVL(INR)
         RSLMAP4_ow(2,ILOC,1) = VolOW(INR) + ActualExtraBergendVolume(inr)
      elseif (ikind .eq. 6) then
         RSLMAP6_bnd(2,ILOC,1) = BNDPAR(INR,1)  ! peil op de rand toegevoegd, nov. 1999
   ! ARS 16155 initial salt conc, but only for boundary inlets (i.e. there is a downstream node of the boundary)
         if (islcmp .ne. 0 .and. DoNode(inode) .ne. 0) RslMap9_slt(1,inode,1) = SltBnd(inr)
      elseif (ikind .eq. 7) then
! NWRW situation T=0 (added March 2011)
         IPLV2 = INDIKP(INR)
         RVolOp = 0.0
         RVolDn = 0.0
         DO IPTYP=1,NPTYP
           DO IPOPP=1,NPOPP
             RVOLOP= RVOLOP+ BVOP(IPLV2,IPTYP,IPOPP) * AREAPV(INR,IPTYP,IPOPP)
             RVOLDN= RVOLDN+ NTRRST(IPLV2,IPTYP,IPOPP) * AREAPV(INR,IPTYP,IPOPP)
           Enddo
         Enddo
         Do j=1,NrSpecialNwrwAreas(Inr)
            iplv3  = SpecialInDikP(inr,j)
            iplv4  = Reference2SpecialDef(inr,j)
            RVolOp = RVolOp + SpecialBVOP(iplv3) * SpecialNwrwAreas(Inr,j)
            RVolDn = RVolDn + SpecialNTRRST(iplv3) * SpecialNwrwAreas(Inr,j)
         Enddo
         RSLMAP7_plv(8,ILOC,1) = RVolOp
         RSLMAP7_plv(9,ILOC,1) = RVolDN
         Do j=1,NrSpecialNwrwAreas(Inr)
            iplv3  = SpecialInDikP(inr,j)
            iplv4  = Reference2SpecialDef(inr,j)
            ihelp = 35 + min (12, iplv4)
            RSLMAP7_plv(ihelp,ILOC,1) = RSLMAP7_plv(ihelp,ILOC,1) + SpecialNwrwAreas(inr,j) * (SpecialBVOP(IPLV3) + SpecialNTRRST(iplv3))
         Enddo
         RSLMAP7_plv(52,ILOC,1) = WadiInitialStorage(iloc)
         RSLMAP7_plv(53,ILOC,1) = WadiInitialLevel(iloc)
      elseif (ikind .eq. 16) then
! Sacramento op T=0
         RSLMAP17_Sacr(1,ILOC,1) =  UZTWC(INR)
         RSLMAP17_Sacr(2,ILOC,1) =  UZFWC(INR)
         RSLMAP17_Sacr(3,ILOC,1) =  LZTWC(INR)
         RSLMAP17_Sacr(4,ILOC,1) =  LZFPC(INR)
         RSLMAP17_Sacr(5,ILOC,1) =  LZFSC(INR)
         RSLMAP17_Sacr(15,ILOC,1) = ADIMC(INR)
      elseif (ikind .eq. 17) then
! Cells op T=0
!         RSLMAP18_Cel(29,ILOC,1) = CellData(iloc)%OpenWaterFinalStorage(1)
!         RSLMAP18_Cel(30,ILOC,1) = CellData(iloc)%FinalOpenWaterLevel(1)
!         RSLMAP18_Cel(35,ILOC,1) = CellData(iloc)%SewerFinalStorage
!         RSLMAP18_Cel(41,ILOC,1) = CellData(iloc)%FinalGroundwaterlevel
!         RSLMAP18_Cel(42,ILOC,1) = CellData(iloc)%FinalGWStorage
!         RSLMAP18_Cel(48,ILOC,1) = CellData(iloc)%UnsatZoneFinalStorage
! RRRUnoff op T=0
      elseif (ikind .eq. 19) then   ! HBV
         IRRRunoffSub = RRRunoff_SubIndex(iloc)
         RSLMAP19_RRRunoff(NStartHBV+4,ILOC,1) = HBV_DrySnowContent(IRRRunoffSub)
         RSLMAP19_RRRunoff(NStartHBV+5,ILOC,1) = HBV_FreeWaterContent(IRRRunoffSub)
         RSLMAP19_RRRunoff(NStartHBV+6,ILOC,1) = HBV_SoilMoisture(IRRRunoffSub)
         RSLMAP19_RRRunoff(NStartHBV+7,ILOC,1) = HBV_UpperZoneContent(IRRRunoffSub)
         RSLMAP19_RRRunoff(NStartHBV+8,ILOC,1) = HBV_LowerZoneContent(IRRRunoffSub)
      elseif (ikind .eq. 20) then   ! SCS
         IRRRunoffSub = RRRunoff_SubIndex(iloc)
         RSLMAP19_RRRunoff(NStartSCS,ILOC,1) = SCS_Storage(IRRRunoffSub)
      elseif (ikind .eq. 22) then   ! LGSI
         IRRRunoffSub = RRRunoff_SubIndex(iloc)
         RSLMAP19_RRRunoff(NStartLGSI+17,ILOC,1) = LGSI_OverlandStorage(IRRRunoffSub,1)
         RSLMAP19_RRRunoff(NStartLGSI+19,ILOC,1) = LGSI_GWStorage(IRRRunoffSub,1)
         RSLMAP19_RRRunoff(NStartLGSI+21,ILOC,1) = LGSI_NewVolume(IRRRunoffSub,1)
         RSLMAP19_RRRunoff(NStartLGSI+23,ILOC,1) = LGSI_NewGwl(IRRRunoffSub,1)
         If (LGSI_NRSubAreas(IRRRunoffSub) .eq. 2) then
            RSLMAP19_RRRunoff(NStartLGSI+18,ILOC,1) = LGSI_OverlandStorage(IRRRunoffSub,2)
            RSLMAP19_RRRunoff(NStartLGSI+20,ILOC,1) = LGSI_GWStorage(IRRRunoffSub,2)
            RSLMAP19_RRRunoff(NStartLGSI+22,ILOC,1) = LGSI_NewVolume(IRRRunoffSub,2)
            RSLMAP19_RRRunoff(NStartLGSI+24,ILOC,1) = LGSI_NewGwl(IRRRunoffSub,2)
         Endif
      elseif (ikind .eq. 23) then   ! Wageningen / Walrus
         if (useWalrus) then
            IRRRunoffSub = RRRunoff_SubIndex(iloc)
            RSLMAP19_RRRunoff(NStartWalrus  ,ILOC,1) = Walrus_DVCurrent(IRRRunoffSub)  ! storage deficit vadose zone
            RSLMAP19_RRRunoff(NStartWalrus+1,ILOC,1) = Walrus_DGCurrent(IRRRunoffSub)  ! groundwater depth
            RSLMAP19_RRRunoff(NStartWalrus+2,ILOC,1) = Walrus_HQCurrent(IRRRunoffSub)  ! level quickflow reservoir
            RSLMAP19_RRRunoff(NStartWalrus+3,ILOC,1) = Walrus_HSCurrent(IRRRunoffSub)  ! surface water level
         else
            IRRRunoffSub = RRRunoff_SubIndex(iloc)
            RSLMAP19_RRRunoff(NStartWagMod+2,ILOC,1) = Wagmod_SM(IRRRunoffSub)
            RSLMAP19_RRRunoff(NStartWagMod+3,ILOC,1) = WagMod_GStore(IRRRunoffSub)
!           RSLMAP19_RRRunoff(NStartWagMod+4,ILOC,1) = WagMod_Seep(IRRRunoffSub)
         endif
      elseif (ikind .eq. 31) then   ! NAM
         IRRRunoffSub = RRRunoff_SubIndex(iloc)
         RSLMAP19_RRRunoff(NStartNAM,ILOC,1)    = NAM_Houtside(IRRRunoffSub)
         RSLMAP19_RRRunoff(NStartNAM+20,ILOC,1) = NAM_U(IRRRunoffSub)
         RSLMAP19_RRRunoff(NStartNAM+21,ILOC,1) = NAM_L(IRRRunoffSub)
         RSLMAP19_RRRunoff(NStartNAM+22,ILOC,1) = NAM_GWSD(iRRRunoffSub)
         RSLMAP19_RRRunoff(NStartNAM+26,ILOC,1) = NAM_GWL(iRRRunoffSub)
         RSLMAP19_RRRunoff(NStartNAM+27,ILOC,1) = NAM_GWTD(iRRRunoffSub)
         RSLMAP19_RRRunoff(NStartNAM+23,ILOC,1) = NAM_VU(iRRRunoffSub)
         RSLMAP19_RRRunoff(NStartNAM+24,ILOC,1) = NAM_VL(IRRRunoffSub)
         RSLMAP19_RRRunoff(NStartNAM+25,ILOC,1) = NAM_VGWS(IRRRunoffSub)
         RSLMAP19_RRRunoff(NStartNAM+28,ILOC,1) = NAM_AVSoil(IRRRunoffSub)
      endif
   Enddo

   Return
   End subroutine Init3BHisVariables


   Subroutine Write3BHisfilesTnul (Ievent, LastTm, DefaultT0OutputValue, RestartVersion)

! writes output 3B His files for t=0
! Ievent = event number
! Itmstp = timestep

   Integer Ievent, Lasttm, iout1
   Real    DefaultT0OutputValue
   Character(len=32) RestartVersion

! RValue = real value array met initialisatiewaarden
   Real     RValue(2)
   Integer  N4

   Integer imap, iloc, j

! local: N4 dimension of arrays
   N4 = 2

   if (RestIO(1) .eq. 1 .and. &
       (RestartVersion .eq. ' RestartFile RR version 3.213.10' .or. &
         RestartVersion .eq. ' RestartFile RR version 3.213.18' .or. &
          RestartVersion .eq. ' RestartFile RR version 3.213.22' .or. &
           RestartVersion .eq. ' RestartFile RR version 3.213.29' .or. &
            RestartVersion .eq. ' RestartFile RR version 3.213.33' .or. &
             RestartVersion .eq. ' RestartFile RR version 3.214.27' .or. &
              RestartVersion .eq. ' RestartFile RR version 3.216.21' .or. &
               RestartVersion .eq. ' RestartFile RR version 3.216.29'  .or. &
                RestartVersion .eq. ' RestartFile RR version 3.216.33') )  then
      ! set all values from restart
      Do iloc=1,ncvhg
         Do j=1,MaxSeriesPerMap(1)
            RSLMAP1_vhg (j,ILOC,1) =  VHG_Tnul(j,iloc)
         Enddo
      Enddo
      Do iloc=1,ncovhg
         Do j=1,MaxSeriesPerMap(2)
            RSLMAP2_ovh (j,ILOC,1) =  OVH_Tnul(j,iloc)
         Enddo
      Enddo
      Do iloc=1,nckas
         Do j=1,MaxSeriesPerMap(3)
            RSLMAP3_Kas (j,ILOC,1) =  KAS_Tnul(j,iloc)
         Enddo
      Enddo
      Do iloc=1,ncow
         Do j=1,MaxSeriesPerMap(4)
            RSLMAP4_Ow (j,ILOC,1) =  OW_Tnul(j,iloc)
         Enddo
      Enddo
      Do iloc=1,ncStru
         Do j=1,MaxSeriesPerMap(5)
            RSLMAP5_STR (j,ILOC,1) =  STR_Tnul(j,iloc)
         Enddo
      Enddo
      Do iloc=1,ncboun
         Do j=1,MaxSeriesPerMap(6)
            RSLMAP6_bnd (j,ILOC,1) =  BND_Tnul(j,iloc)
         Enddo
      Enddo
      Do iloc=1,ncpluv
         Do j=1,MaxSeriesPerMap(7)
            RSLMAP7_PLV (j,ILOC,1) =  PLV_Tnul(j,iloc)
         Enddo
      Enddo

      ! check if salt is on
      IF (ISLCMP .NE. 0) THEN
          Do iloc=1,ncsalt
             Do j=1,MaxSeriesPerMap(9)
                RSLMAP9_SLT (j,ILOC,1) =  SLT_Tnul(j,iloc)
             Enddo
          Enddo
      ELSE
        Do j=1,MaxSeriesPerMap(9)
            RSLMAP9_SLT (j,1,1) =  SLT_Tnul(j,1)
        Enddo
      ENDIF

      Do iloc=1,ncrwzi
         Do j=1,MaxSeriesPerMap(10)
            RSLMAP14_RWZI(j,ILOC,1) =  RWZI_Tnul(j,iloc)
         Enddo
      Enddo
      Do iloc=1,ncIndus
         Do j=1,MaxSeriesPerMap(11)
            RSLMAP15_Ind (j,ILOC,1) =  IND_Tnul(j,iloc)
         Enddo
      Enddo
      Do iloc=1,ncSacr
         Do j=1,MaxSeriesPerMap(12)
            RSLMAP17_Sacr (j,ILOC,1) =  SACR_Tnul(j,iloc)
         Enddo
      Enddo
      Do iloc=1,ncRRRunoff
         Do j=1,MaxSeriesPerMap(15)
            RSLMAP19_RRRunoff (j,ILOC,1) =  RRRunoff_Tnul(j,iloc)
         Enddo
      Enddo
   Endif

   Call Write3BHisfiles (Ievent, 0, Lasttm)

! reset alle arrays op nul of -999.99(bij uitvoer van maximum values, behalve voor de balans map)

      DO IMAP=1,NKAART

        RValue(1) = 0.0
        RValue(2) = 0.0
        if (OutputAtTimestepOption .eq. 3 .and. imap .ne. 8) RValue(2) = -999.99
           select case (imap)
             case (1)
               Call InitResultArray  (IMAP, Rslmap1_vhg    ,  MaxSeriesPerMap(imap), Nvhg  , 2, RValue, N4)
             case (2)
               Call InitResultArray  (IMAP, Rslmap2_ovh    ,  MaxSeriesPerMap(imap), Novh  , 2, RValue, N4)
             case (3)
               Call InitResultArray  (IMAP, Rslmap3_kas    ,  MaxSeriesPerMap(imap), Nkas  , 2, RValue, N4)
             case (4)
               Call InitResultArray  (IMAP, Rslmap4_ow     ,  MaxSeriesPerMap(imap), Now   , 2, RValue, N4)
             case (5)
               Call InitResultArray  (IMAP, Rslmap5_str    ,  MaxSeriesPerMap(imap), Nstr  , 2, RValue, N4)
             case (6)
               Call InitResultArray  (IMAP, Rslmap6_bnd    ,  MaxSeriesPerMap(imap), Nbnd  , 2, RValue, N4)
             case (7)
               Call InitResultArray  (IMAP, Rslmap7_plv    ,  MaxSeriesPerMap(imap), Nplv  , 2, RValue, N4)
             case (8)
               Call InitResultArray  (IMAP, Rslmap8_bal    ,  MaxSeriesPerMap(imap), NNod  , 2, RValue, N4)
             case (9)
               if (islcmp .eq. 0) then
                 Call InitResultArray  (IMAP, Rslmap9_slt  , MaxSeriesPerMap(imap), 1     , 2, RValue, N4)
               else
                 Call InitResultArray  (IMAP, Rslmap9_slt  , MaxSeriesPerMap(imap), NNod  , 2, RValue, N4)
               endif
             case (10)
               Call InitResultArray  (IMAP,  Rslmap14_rwzi , MaxSeriesPerMap(imap), Nrwzi , 2, RValue, N4)
             case (11)
               Call InitResultArray  (IMAP,  Rslmap15_ind  , MaxSeriesPerMap(imap), Nindus, 2, RValue, N4)
             case (12)
               Call InitResultArray  (IMAP,  Rslmap17_Sacr , MaxSeriesPerMap(imap), NcSacr, 2, RValue, N4)
             case (13)
               Call InitResultArray  (IMAP,  Rslmap16_flows, MaxSeriesPerMap(imap), Nclink, 2, RValue, N4)
             case (14)
!               Call InitResultArray  (IMAP,  Rslmap18_Cel, MaxSeriesPerMap(imap), NcCell, 2, RValue, N4)
             case (15)
               Call InitResultArray  (IMAP,  Rslmap19_RRRunoff, MaxSeriesPerMap(imap), NcRRRunoff, 2, RValue, N4)
           end select
      Enddo

   Return
   End subroutine Write3BHisFilesTnul


   Subroutine InitResultArray (IMAP, RslDet, N2, N3, N4, RValue, Last)

!C *********************************************************************
!C *** DELFT3B: Initialise Result array RslDet(N1,N2,N3,N4) op value Rvalue  voor HIS file
!C ***   Imap   = map number
!C ***   NumberOfTimesteps = OutputAtTimestepOption; needed for determining averages
!C ***   ievent, itmstp = event and timestep number
!C ***                    ievent=0 for series output.
!C ***   RslDet = output array; N2 to N4 are the three dimensions of this array
!C ***            Actual call e.g. with RslMap1_vhg,
!C ***   RValue = array met initialisatiewaarden
!C ***   Last   = laatste index die aangeeft tot hoever te initialiseren. Last=1 of Last=N4=2.
!C *********************************************************************

      implicit none

      INTEGER       IMAP, IMSR, ILOC, I
      Integer       N2, N3, N4, Last
      Real          RslDet(N2,N3,N4), RValue(N4)

      Do IMSR=1,NSRMAP(IMAP)
         Do ILOC=1,NLCMAP(IMAP)
           Do I=1,Last
            RSLDET(IMSR,ILOC,i) = RValue(i)
           Enddo
         Enddo
      Enddo

   Return
   End subroutine InitResultArray




   SUBROUTINE WrHdr_Modflow (IOutModFlow,IEvent,Itmstp)

! *********************************************************************
!c *** Delfland / Bergings- en bemalingsbehoefte model
!c *** Last update : June 1996            by : Geert Prinsen
!c *********************************************************************
!c *** Write Header Modflow HIS file
!c **********************************************************************

      IMPLICIT NONE

      INTEGER       IoutModFlow, IEvent, Itmstp
!     integer       Idebug
      Integer       NAMLNG, I, imap
      Integer       IYear, Imo, Iday, iHour, IMin, ISec, TmSize
      CHARACTER*20  Name
      CHARACTER(Len=CharIdLength) FILNAM
      CHARACTER*160 Header

! voorlopig alleen maar voor tijdstap 1; later de check inbouwen voor om de x tijdstappen

      if (Itmstp .eq. 1) then
        FilNam = ConfFil_get_NAMFIL(103)
        Call OpenFl (IOutModFlow,FilNam,3,2)           ! open als Binary file, status=unknown  (HIS file)

!c 4*char40 identification header
        Header = ' Transferfile RR-Modflow'
        NAMLNG = Len_Trim(Header)
        Header (NAMLNG+1:NAMLNG+6) = ' Event'
        Header (NAMLNG+7:NAMLNG+9) = INTCHR ( IOPT1(4) )
        NAMLNG = Len_Trim(Header)

! include T0 string
        IYEAR = ConfArr_get_iYear()
        IMO   = ConfArr_get_iMonth()
        IDAY  = ConfArr_get_IDAY()
        IHOUR = ConfArr_get_iHour()
        IMIN  = ConfArr_get_iMinute()
        ISEC  = ConfArr_get_iSecond()
        TmSize = TimeSettings%TimestepSize
        Call WriteT0String (Header, IYear, Imo, Iday, Ihour, Imin, Isec, TmSize)
!c write Header
        WRITE (IoutModflow) Header(1:160)
!c aantal series en lokaties
        WRITE (IoutModflow)  1, NcOvhg+NcOw
!c write char20 voor alle series
        Name = ' RR-seepage (mm/day) '
        WRITE (IoutModflow) Name(1:20)
! write lokatie id's (integer + char(20))
! gebruik id's van map 2: unpaved area en 4: open water
        IMap = 2
        WRITE (IoutModflow) ( IXLMAP(IMAP,I), Id_Nod(IINODE(IXLMAP(Imap,I))),I=1,NLCMAP(IMAP) )
        IMap = 4
        WRITE (IoutModflow) ( IXLMAP(IMAP,I), Id_Nod(IINODE(IXLMAP(Imap,I))),I=1,NLCMAP(IMAP) )

      Endif

      RETURN
    END Subroutine WrHdr_Modflow



   SUBROUTINE WrData_Modflow (IOutModFlow,Itmstp)

! *********************************************************************
!c *** Delfland / Bergings- en bemalingsbehoefte model
!c *** Last update : June 1996            by : Geert Prinsen
!c *********************************************************************
!c *** Write Header Modflow HIS file
!c **********************************************************************

      IMPLICIT NONE

      INTEGER       IoutModFlow, itmstp, iovh, iow
!     Integer       Idebug

! voorlopig elke tijdstap uitvoer; zie kopje in WrHdr_Modflow
! uitvoer in mm/day
! uitvoer in Kwel() in in m/s

      Write(IoutModFlow)  Itmstp, ((Kwel(iovh)-WegZg(iovh))*NrsDay*1000.,iovh=1,ncovhg)
      Write(IoutModFlow)  ((OwKwel(iow)-OwWegZ(iow))*NrsDay*1000.,iow=1,ncow)

      RETURN
    END Subroutine WrData_Modflow



    Subroutine  WriteT0String (Name, IYear, Imo,Iday,Ihour,Imin,Isec,TmSize)

      Character*160 Name
      Integer       IYear, Imo, Iday, Ihour,Imin,Isec,TmSize

      IF (TMSIZE .LT. 10) THEN
        WRITE(NAME(121:160),101) IYEAR,IMO,IDAY,IHOUR,IMIN,ISEC,TMSIZE
      ELSEIF (TMSIZE .LT. 100) THEN
        WRITE(NAME(121:160),102) IYEAR,IMO,IDAY,IHOUR,IMIN,ISEC,TMSIZE
      ELSEIF (TMSIZE .LT. 1000) THEN
        WRITE(NAME(121:160),103) IYEAR,IMO,IDAY,IHOUR,IMIN,ISEC,TMSIZE
      ELSEIF (TMSIZE .LT. 10000) THEN
        WRITE(NAME(121:160),104) IYEAR,IMO,IDAY,IHOUR,IMIN,ISEC,TMSIZE
      ELSEIF (TMSIZE .LT. 100000) THEN
        WRITE(NAME(121:160),105) IYEAR,IMO,IDAY,IHOUR,IMIN,ISEC,TMSIZE
      ELSEIF (TMSIZE .LT. 1000000) THEN
        WRITE(NAME(121:160),106) IYEAR,IMO,IDAY,IHOUR,IMIN,ISEC,TMSIZE
      ELSEIF (TMSIZE .LT. 10000000) THEN
        WRITE(NAME(121:160),107) IYEAR,IMO,IDAY,IHOUR,IMIN,ISEC,TMSIZE
      ELSE
        WRITE(NAME(121:160),108) IYEAR,IMO,IDAY,IHOUR,IMIN,ISEC,TMSIZE
      ENDIF
  101 FORMAT ('T0: ',I4,'.',I2,'.',I2,1X,I2,':',I2,':',I2,'  (scu=       ',I1,'s)')
  102 FORMAT ('T0: ',I4,'.',I2,'.',I2,1X,I2,':',I2,':',I2,'  (scu=      ',I2 ,'s)')
  103 FORMAT ('T0: ',I4,'.',I2,'.',I2,1X,I2,':',I2,':',I2,'  (scu=     ',I3  ,'s)')
  104 FORMAT ('T0: ',I4,'.',I2,'.',I2,1X,I2,':',I2,':',I2,'  (scu=    ',I4   ,'s)')
  105 FORMAT ('T0: ',I4,'.',I2,'.',I2,1X,I2,':',I2,':',I2,'  (scu=   ',I5    ,'s)')
  106 FORMAT ('T0: ',I4,'.',I2,'.',I2,1X,I2,':',I2,':',I2,'  (scu=  ',I6     ,'s)')
  107 FORMAT ('T0: ',I4,'.',I2,'.',I2,1X,I2,':',I2,':',I2,'  (scu= ',I7      ,'s)')
  108 FORMAT ('T0: ',I4,'.',I2,'.',I2,1X,I2,':',I2,':',I2,'  (scu=',I8       ,'s)')

      Return
    End Subroutine WriteT0String



   Subroutine Close3BHisfiles

! Close 3B His files

! for netCdf
    use netCdfData
   use NetCdf

   Integer Imap, Nr

   If (Nevent .eq. 1) then
     ! HIS files detailed output per timestep
     Do IMAP=1,NKAART
        If (IOPT1(1) .EQ. 1) NR = NFLMAP(IMAP)/2
        if (NlcMap(imap) .gt. 0) then
!          Do IMFL=NR+1,NFLMAP(IMAP)
!             IOUT = IXFMAP(IMAP,IMFL)
!             Close  (IOut)
!          Enddo
           Call CloseDioPlt(Dataset(imap))
           if (GenerateNetCdfOutput)  ierr = nf90_close(INetCdfFile(imap))
        endif
     Enddo
   Else
     ! Summarized output per event
     Do IMAP=1,NKAART
         NR = NFLMAP(IMAP)
         If (IOPT1(1) .EQ. 1) NR = NR/2
         if (NlcMap(imap) .gt. 0) then
!           Do IMFL=1,NR
!             IOUT = IXFMAP(IMAP,IMFL)
!             Close  (IOut)
!           Enddo
           Call CloseDioPlt(Dataset(imap))
           if (GenerateNetCdfOutput)  ierr = nf90_close(INetCdfFile(imap))
         endif
     Enddo
   Endif

   Return
   End subroutine Close3BHisFiles


  Subroutine InitDioPlt (DataSet)

    ! arguments
    type(DioPltType), intent(inout) :: DataSet   ! openwater dataset

    ! body: create empty dataset (no name, var. type unknown)
     DataSet = DioPltCreate('NoName', Dio_PLT_Unknown)

    Return
  End Subroutine InitDioPlt


  Subroutine WriteOpenWaterDioOrg (outDataSet, Itmstp)

    IMPLICIT NONE !!!!!!!!!
    ! arguments
    type(DioPltType), intent(inout) :: outDataSet   ! openwater dataset
    Integer, intent(in)             :: ItmStp       ! current timestep

    ! locals

    Character(Len=DioMaxStreamLen)  :: outName       ! name of out dataset
    character(len=HisRunIdSize), &
             dimension(HisRunIdDim) :: runId         ! (HIS) runid
    Character(Len=DioMaxParLen), &
             dimension(1)           :: parNames = &  ! variable(s) in dataset
                                       'Open water level'
    Character(Len=DioMaxLocLen), &
             pointer, dimension(:)  :: LocIds                        ! locations(s) in dataset
    Logical, save                   :: doHeaderOpenWaterDio = .True.   ! Process Header next time?

    Integer  loc, i
    Logical  success

    outName = ConfFil_get_NAMFIL(84)
!    write(*,*) ' WriteDio Outname=',OutName

    IF (outName .NE. ' ' .AND. NCOW .GT. 0) THEN
        if ( doHeaderOpenWaterDio ) then
            Success = DH_AllocInit (NcOw, LocIds, ' ')
            If (.not. success)  call ErrMsgStandard (981, 0, ' Error allocating arrays in subroutine ', ' Output_WrtOpenWaterDio')
!           Allocate(LocIds(ncOw))
            ! header
            runId  = ' '
            runId(1) = CASENM(1:)
!           runId(2) = CASENM(HisRunIdSize + 1: HisRunIdSize * 2)
            runId(3) = 'TITLE: Levels open waternodes (m NAP)'
            loc = 1
            do i = 1, ncnode
                if (EiNode(i, 3) == 4) then
                    LocIds(loc) = Id_Nod(I)
                    loc = loc + 1
                endif
            end do
!           write(*,*) ' WriteDio voor DioPLtDefine=',OutName
            outDataSet = DioPltDefine(outName, runId, Dio_Plt_Real, parNames, LocIds)
!           write(*,*) ' WriteDio na DioPLtDefine=',OutName
            deallocate(LocIds)
        endif

        !
        ! Send the data
        !
!        write(*,*) ' WriteDio voor DioPltPut=',OutName
!        call DioPltPut (outDataSet, Itmstp, RESHAPE (LvlOw, (/1, NcOw/)) )
         call DioPltPut (outDataSet, Itmstp, LvlOw )
!        write(*,*) ' WriteDio na DioPltPut=',OutName
        !
        ! If this is a shared mem strea:
        ! - keep it open (and don't send the header next timen)
        ! - close it (next step will exactly behave as present one)
        !
        if ( DioPltGetStreamType(outDataSet) .eq. Dio_SharedMem_stream ) then
!            write(*,*) ' WriteDio voor doHeaderOpenWaterDio=',OutName
            doHeaderOpenWaterDio = .false.
        else
            call DioPltDestroy (outDataSet)
        endif
    Endif
!   write(*,*) ' WriteDio is ready=',OutName

    Return
  End Subroutine WriteOpenWaterDioOrg



  Subroutine WriteOpenWaterDio (outDataSet, Itmstp)

! ARS 11612  in output file to RTC not only open water levels, but also groundwaterlevels
!            so 2 series in HIS files
!            locations: open waters, unpaved nodes
! Aug 2010   Add boundary flow and salt concentrations

    IMPLICIT NONE !!!!!!!!!
    ! arguments
    type(DioPltType), intent(inout) :: outDataSet   ! openwater dataset
    Integer, intent(in)             :: ItmStp       ! current timestep

    ! locals

    Character(Len=DioMaxStreamLen)  :: outName       ! name of out dataset
    character(len=HisRunIdSize), &
             dimension(HisRunIdDim) :: runId         ! (HIS) runid
    Character(Len=DioMaxParLen), &
             dimension(4)           :: parNames      ! variable(s) in dataset
    Character(Len=DioMaxLocLen), &
             pointer, dimension(:)  :: LocIds                        ! locations(s) in dataset
    Logical, save                   :: doHeaderOpenWaterDio = .True.   ! Process Header next time?

    Integer  loc, i, iow, iovh, iboun, inode
    Logical  success
    Real, Pointer :: DioResult (:,:)


    ParNames(1) = 'Open water level'
    ParNames(2) = 'Groundwaterlevel'
    ParNames(3) = 'Flow'
    ParNames(4) = 'Cl'
    outName = ConfFil_get_NAMFIL(84)
!    write(*,*) ' WriteDio Outname=',OutName

    IF (outName .NE. ' ' .AND. NCOW+ncOvhg+NcBoun .GT. 0) THEN
        if ( doHeaderOpenWaterDio ) then
            Success = DH_AllocInit (NcOw+NcOvhg+NcBoun, LocIds, ' ')
            If (.not. success)  call ErrMsgStandard (981, 0, ' Error allocating arrays in subroutine ', ' Output_WrtOpenWaterDio')
            ! header
            runId  = ' '
            runId(1) = CASENM(1:)
!           runId(2) = CASENM(HisRunIdSize + 1: HisRunIdSize * 2)
            runId(3) = 'TITLE: Levels open water/unpaved nodes (m NAP)'
            loc = 0
!           First all open water nodes
            do i = 1, ncnode
                if (EiNode(i, 3) == 4) then
                    loc = loc + 1
                    LocIds(loc) = Id_Nod(I)
                endif
            end do
!           Then all unpaved nodes
            do i = 1, ncnode
                if (EiNode(i, 3) == 2) then
                    loc = loc + 1
                    LocIds(loc) = Id_Nod(I)
                endif
            end do
!           Then all boundary nodes
            do i = 1, ncnode
                if (EiNode(i, 3) == 6) then
                    loc = loc + 1
                    LocIds(loc) = Id_Nod(I)
                endif
            end do
!           write(*,*) ' WriteDio voor DioPLtDefine=',OutName
            outDataSet = DioPltDefine(outName, runId, Dio_Plt_Real, parNames, LocIds)
!           write(*,*) ' WriteDio na DioPLtDefine=',OutName
            deallocate(LocIds)
        endif

        !
        ! Send the data
        !
!        write(*,*) ' WriteDio voor DioPltPut=',OutName
         Success = DH_AllocInit (4,NcOw+NcOvhg+ncBoun, DioResult, 0E0)
         DioResult = 0.
         Do iow = 1, NcOw
            DioResult(1,iow) = LvlOw(iow)
            DioResult(2,iow) = -999.99
            DioResult(3,iow) = -999.99
            DioResult(4,iow) = -999.99
         Enddo
         Do iovh = 1, NcOvhg
            DioResult(1,NcOw+iovh) = -999.99
            DioResult(2,NcOw+iovh) = Gwl(iovh)
            DioResult(3,NcOw+iovh) = -999.99
            DioResult(4,ncOw+iovh) = -999.99
         Enddo
         Do iboun= 1, NcBoun
            DioResult(1,NcOw+Ncovhg+iboun) = -999.99
            DioResult(2,NcOw+Ncovhg+iboun) = -999.99
            DioResult(3,NcOw+Ncovhg+iboun) = Qbnd(iboun)
            DioResult(4,NcOw+Ncovhg+iboun) = CBnd(iboun)
         Enddo
         Do inode=1,NcNode
            if (Einode(inode,3) .eq. 4) then
                iow = Einode(inode,2)
                if (islcmp .ne. 0) DioResult(4,iow) = RslMap9_slt(1,inode,1)
            elseif (Einode(inode,2) .eq. 2) then
                iovh = Einode(inode,2)
                if (islcmp .ne. 0) DioResult(4,ncOw+iovh) = RslMap9_slt(1,inode,1)
            endif
         Enddo
         call DioPltPut (outDataSet, Itmstp, DioResult)
         Deallocate(DioResult)
        !
        ! If this is a shared mem strea:
        ! - keep it open (and don't send the header next timen)
        ! - close it (next step will exactly behave as present one)
        !
        if ( DioPltGetStreamType(outDataSet) .eq. Dio_SharedMem_stream ) then
!            write(*,*) ' WriteDio voor doHeaderOpenWaterDio=',OutName
            doHeaderOpenWaterDio = .false.
        else
            call DioPltDestroy (outDataSet)
        endif
    Endif
!   write(*,*) ' WriteDio is ready=',OutName

    Return
  End Subroutine WriteOpenWaterDio







  Subroutine ReadDioPltRTCFile (inDataSet, inName, ITMSTP)

    implicit none

    CHARACTER*6  NAME
    Character*(*)  inName       ! name of input dataset from RTC

    INTEGER           NLOCHIS, NHISRtc, IDEBUG, ItmStp, Iout1
    INTEGER,Save  ::  NLOCFRtc, NHISPRtc
    Integer       I, ILOC, IPAR
!   PARAMETER     (NHISRtc=7,NLOCHIS=999)
    PARAMETER     (NHISRtc=16,NLOCHIS=15000)
    CHARACTER(Len=CharIdLength) IDREADRtc (NLOCHIS)
    CHARACTER(Len=CharIdLength) IDPARARtc (NHISRtc)

    Integer        iMStp, id, iNod, iStr
    CHARACTER(CharIdLength) IDNODE

! DIO var.s
    Character(Len=DioMaxStreamLen)    :: locInName    ! name of in dataset
    type(DioPltType), intent(inout)   :: inDataSet    ! plt dataset to be read
    character(Len=DioMaxParLen), pointer, dimension(:) :: parNamesRtc     ! par. names
    character(Len=DioMaxLocLen), pointer, dimension(:) :: LocIdsRtc     ! loc. names
    Logical, save                     :: doHeaderRtc = .True.     ! Process Header next time?

    real, Pointer, save :: readRtcValues(:,:)

    real, dimension(:,:), pointer :: readRealRtcValues
    double precision, dimension(:,:), pointer :: readDoubleRtcValues
    logical  success
!
! body
!
      iOut1 = ConfFil_get_iOut1()
      iDebug = ConfFil_get_iDebug()
      if (idebug .ne. 0) WRITE (IDEBUG,1)
    1 FORMAT (' ReadDioPltRtcFile')

! *********************************************************************
! *** read header
! ***   NLOCFRtc = aantal lokaties in His file
! ***   NHISPRtc = aantal parameters in His file
! *********************************************************************

    if ( doHeaderRtc ) then
        ! Get dataset header and dimensions
        locInName = inName
!        write(*,*) ' ReadDioPlt before DioPltGetDataset Name=',locInName
        inDataSet = DioPltGetDataset(locInName)
        NHISPRtc =  DioPltGetNPar(inDataSet)
        NLOCFRtc =  DioPltGetNLoc(inDataSet)

        ! check dimensions
        If (NHISPRtc .le. 0 .or. NLocFRtc .le. 0) then
           ! no data in HIS file, since apparently no feedback from RTC to RR
           goto 9991
        Endif
        IF (NLOCFRtc .GT. NLOCHIS) THEN
            Write(iout1,*) 'ReadDioPlt Nloc error: ', NLocFRtc, NLocHis
            call ErrMsgStandard (912, 0, 'ReadDioPltRTC',' NLOCaties in HIS file')
        ELSEIF (NHISPRtc .GT. NHISRtc) THEN
            Write(iout1,*) 'ReadDioPlt NPar error: ', NHISPRtc, NHISRtc
            call ErrMsgStandard (912, 0, 'ReadDioPltRTC',' NPARameters in HIS file')
        ENDIF

        ! Get parameter and location names
        parNamesRtc => DioPltGetPars(inDataSet)
        LocIdsRtc => DioPltGetLocs(inDataSet)
        IDPARARtc(1:NHISPRtc) = parNamesRtc(1:NHISPRtc)
        IDREADRtc(1:NLOCFRtc) = LocIdsRtc(1:NLOCFRtc)

        if (idebug .ne. 0) THEN
            WRITE(IDEBUG,*)  NLOCFRtc
            WRITE(IDEBUG,*)  (IDREADRtc(I),I=1,NLOCFRtc)
            WRITE(IDEBUG,*)  NHISPRtc
            WRITE(IDEBUG,*)  (IDPARARtc(I),I=1,NHISPRtc)
        ENDIF

        ! *********************************************************************
        ! *** Check dimensions
        ! *********************************************************************
          IF (NHISPRtc .GT. NHISRtc) then
              Write(iout1,*) 'ReadDioPlt NPar error: ', NHISPRtc, NHISRtc
              call ErrMsgStandard (912, 0, NAME,' NPARameters in HIS file')
          endif 
              
        ! *********************************************************************
        ! *** In First timestep: determine conversion array mapping id's
        ! *** from HIS file to RTC.Loc file
        ! *********************************************************************
        ! WRITE(*,*) ' RdHis itmstp= ', Itmstp
        If (ITmstp .eq. 1) then
            RTCHisLoc = 0
            Do Iloc = 1, nLocFRtc
               success = .false.
               IDNODE = IdReadRtc(Iloc)
               Call FNDND2 (ID, IDnode)
               Do INOD=1,NCNODE
                  If (ID .EQ. EiNode(INOD,1) .AND. EiNode(INOD,3) .EQ. 5) THEN
                     ISTR = EiNode(INOD,2)
                     RtcHisLoc (IStr) = iloc
                     success = .true.
                  Endif
               Enddo
               ! warning if info from RTC is not used, ARS 14408
               if (.not. success) then
                 call SetMessage(LEVEL_WARN, 'Reading RTC info: RR does not use RTC info with id '//IDNode(1:Len_Trim(idNode)))
               endif
            Enddo
        Endif
        if (idebug .ne. 0) then
            Write(Idebug,*) ' Structure Istr    HisLoc    HisId '
            do IStr = 1, ncStru
               ILoc = RTCHisLoc(IStr)
               if (iloc .gt. 0) then
                  Write(Idebug,*) Istr, ILoc, IdReadRtc(iloc)(1:Len_Trim(IdReadRtc(iloc)))
               else
                  Write(Idebug,*) Istr, ' is not coupled with RTC '
               endif
            enddo
        Endif

    Endif  !!!(doHeaderRtc)

    ! *********************************************************************
    ! *** read data, store in ReadRtcValues
    ! *********************************************************************
    !
    If (NHISPRtc .le. 0 .or. NLocFRtc .le. 0) then
       ! no data in HIS file, since apparently no feedback from RTC to RR
       goto 9991
    Endif
!    write(*,*) ' ReadDioPlt before DioPltGet'
!   Let op het type van de dataset
!   if (InDataSet%Header%Vartype .eq. 2) then
    if (InDataSet%Header%Vartype .eq. Dio_Plt_Real) then
       if (.not. DioPltGet(inDataSet, readRealRtcValues)) then
         If (NLOCFRtc * NHisPRtc .gt. 0) then
            write(*,*) 'Could not read real values in DioPltGet'
            call ErrMsgStandard (971, 0, NAME,' Could not get the values from RTC ')
         endif
       endif
       Success = Dh_AllocInit (NHisPRtc, NLocfRtc, ReadRtcValues, 0E0)
       If (.not. success)  call ErrMsgStandard (981, 0, ' Error allocating arrays in subroutine ', ' Output_ReadDioPLT')
!      Allocate(ReadRtcValues(NHisPRtc, NLocfRtc))
       ReadRtcValues = readRealRtcValues
    else
       if (.not. DioPltGet(inDataSet, readDoubleRtcValues)) then
         If (NLOCFRtc * NHisPRtc .gt. 0) then
            call ErrMsgStandard (971, 0, NAME,' Could not get the values from RTC ')
         endif
       endif
       Success = Dh_AllocInit (NHisPRtc, NLocfRtc, ReadRtcValues, 0E0)
       If (.not. success)  call ErrMsgStandard (981, 0, ' Error allocating arrays in subroutine ', ' Output_ReadDioPLT')
!      Allocate(ReadRtcValues(NHisPRtc, NLocfRtc))
       Do IPar=1,NHisPRtc
          Do ILOC=1,NLOCFRtc
             ReadRtcValues(ipar,iloc) = Sngl (ReadDoubleRtcValues(ipar,iloc))
          Enddo
       Enddo
    endif

    ! *********************************************************************
    ! *** Debug
    ! *********************************************************************

    if (idebug .ne. 0) THEN
        WRITE(IDEBUG,*)  ' Output from DIO with locationid, parametervalue 1 tm7'
        DO ILOC=1,NLOCFRtc
            WRITE(IDEBUG,'(A,7F8.3)') IDReadRtc(ILOC)(1:Len_Trim(IdReadRtc(Iloc))), &
                                      (ReadRtcValues(IPar,ILOC),IPAR=1,NHISPRtc)
        Enddo
    Endif

!    write(*,*) ' ReadDioPlt after DioPltGet'
    !
    ! If this is a shared mem strea:
    ! - keep it open (and don't send the header next timen)
    ! - close it (next step will exactly behave as present one)
    !
 ! Assign data from HIS file to structures
      DO IStr=1,NcStru
         i = RTCHisLoc(IStr)
!        Assign Pumpstop (all structures: pumps, weir, orifices)
         IF (I .gt. 0) then
             IMSTP = ReadRtcValues(1,i)
             IDNODE = IDREADRtc(I)
             IF (IMSTP .GE. 1 .AND. MSFACT(ISTR) .LE. 0. .and. Iout1 .ne. 0) then
                 WRITE(IOUT1,'(A,A,A,I4)')  ' Maalstop voor gemaal',IDNODE(1:Len_Trim(IdNode)),' na tijdstap',ITMSTP
             ELSEIF (IMSTP .LE. 0 .AND. MSFACT(ISTR) .GE. 1. .and. Iout1 .ne. 0) then
                WRITE(IOUT1,'(A,A,A,I4)')   ' Einde maalstop gemaal',IDNODE(1:Len_Trim(IdNode)),' na tijdstap',ITMSTP
             ENDIF
             MSFACT(ISTR) = IMSTP
             if (idebug .ne. 0) then
                 Write(Idebug,*) ' Structure Istr  Id              HisLoc  Maalstop 0=nee/1=ja '
                 WRITE(IDEBUG,'(I5,A,2I5,F8.3)') ISTR, IdNode(1:Len_Trim(idNode)), &
                                                  i, IMSTP, ReadRtcValues(1,i)
             Endif
!            Matlab measures: switch-on/off levels: pumps only
             IF (STRTYP(ISTR) .EQ. 1 .or. StrTyp(Istr) .eq. 8 .and. ReadRtcValues(3,I) .gt. 0) then
               OnOffMatlab(Istr,1) = ReadRtcValues(3,I)
               OnOffMatlab(Istr,2) = ReadRtcValues(4,I)
               OnOffMatlab(Istr,3) = ReadRtcValues(5,I)
               OnOffMatlab(Istr,4) = ReadRtcValues(6,I)
               OnOffMatlab(Istr,5) = ReadRtcValues(7,I)
             ENDIF
         ENDIF
      ENDDO
! *********************************************************************
! *** Error
! *********************************************************************
!
  999 Continue

    if ( DioPltGetStreamType(inDataSet) .eq. Dio_SharedMem_stream ) then
!        write(*,*) ' ReadDioPlt voor doHeaderRtc =false'
        doHeaderRtc = .false.
    else
!        write(*,*) ' ReadDioPlt voor destroy'
        call DioPltDestroy (inDataSet)
    endif

      deallocate(ReadRtcValues)
!      write(*,*) ' ReadDioPlt is ready'
 9991 Continue

      Return
  End Subroutine ReadDioPltRtcFile



  Subroutine ReadDioPltSobekFile (inDataSet, inName, ITMSTP)


    implicit none

    CHARACTER*6  NAME
    Character*(*)  inName       ! name of input dataset from CFSF
    CHARACTER(CharIdLength) IDNODE

    INTEGER       NLOCHIS, NHIS, IDEBUG, ItmStp, Iout1
    INTEGER,Save  ::  NLOCF, NPara
    Integer       I
! ARS 13421: increased dimension of HIS file from 4 parameters to 16 parameters
!    PARAMETER     (NHIS=16,NLOCHIS=9999)
    PARAMETER     (NHIS=16,NLOCHIS=15000)
    CHARACTER(Len=CharIdLength) IDREAD (NLOCHIS)
    CHARACTER(Len=CharIdLength) IDPARA (NHIS)
!   Double Precision RESNOW (NLOC, NHIS)

    Integer        IBoun, IPluv, ILoc

! DIO var.s
    Character(Len=DioMaxStreamLen)    :: locInName                     ! name of in dataset
    type(DioPltType), intent(inout)   :: inDataSet                     ! plt dataset to be read
    character(Len=DioMaxParLen), pointer, dimension(:) :: parNames     ! par. names
    character(Len=DioMaxLocLen), pointer, dimension(:) :: LocIds     ! loc. names
    Logical, save                     :: doHeaderSobek =  .True.       ! Process Header next time?

    real * 4, dimension(:,:), pointer :: readValues
!
! body
!
      iOut1 = ConfFil_get_iOut1()
      iDebug = ConfFil_get_iDebug()
      if (idebug .ne. 0) WRITE (IDEBUG,1)
    1 FORMAT (' ReadDioPltSobekFile')

! *********************************************************************
! *** read header using DIO
! ***   NLOCF = aantal lokaties in His file
! ***   NPARA = aantal parameters in His file
! *********************************************************************

    if ( doHeaderSobek ) then
        ! Get dataset header and dimensions
        locInName = inName
!        write(*,*) ' ReadDioPlt before DioPltGetDataset Name=',locInName
        inDataSet = DioPltGetDataset(locInName)
        NPARA =  DioPltGetNPar(inDataSet)
        NLOCF =  DioPltGetNLoc(inDataSet)

        ! check dimensions
        IF (NLOCF .GT. NLOCHIS) THEN
            Write(iout1,*) 'ReadDioPltSobek NLoc error: ', NLOCF, NLOCHIS
            call ErrMsgStandard (912, 0, NAME,' NLOCaties in HIS file')
        ELSEIF (NPARA .GT. NHIS) THEN
            Write(iout1,*) 'ReadDioPltSobek NPar error: ', NPARA, NHIS
            call ErrMsgStandard (912, 0, NAME,' NPARameters in HIS file')
        ENDIF

! ARS 13421: extension of SBK_RTC.HIS file (communication file of Sobeksim with RR/RTC)
        IF (NPARA .LT. 2 .OR. NPARA .GT. 16) call ErrMsgStandard(971, 0,' ReadDioPltSobekFile: Sobek file must contain 2-16 parameters', '')

        ! Get parameter and location names
        parNames => DioPltGetPars(inDataSet)
        LocIds => DioPltGetLocs(inDataSet)
        IDPARA(1:NPara) = parNames(1:NPara)
        IDREAD(1:NLOCF) = LocIds(1:NLOCF)

        if (idebug .ne. 0) THEN
            WRITE(IDEBUG,*)  NLOCF
            WRITE(IDEBUG,*)  (IDREAD(I),I=1,NLOCF)
            WRITE(IDEBUG,*)  NPara
            WRITE(IDEBUG,*)  (IDPARA(I),I=1,NPara)
        ENDIF

        ! *********************************************************************
        ! *** Check dimensions
        ! *********************************************************************
          IF (NPara .GT. NHIS)  call ErrMsgStandard (912, 0, NAME,' NPARameters in HIS file')

        ! *********************************************************************
        ! *** In First timestep: determine conversion array mapping id's
        ! *** from Sobek HIS file to RR boundaries
        ! *** Note: nrOfSobekNodes may be smaller or larger than nrOfBoundaryNodes !!!!!
        ! ***      - not all boundaries need to be on-line coupled with Sobek
        ! ***      - Sobek His file may contain more locations (for RTC)
        ! *********************************************************************

        If (ITmstp .eq. 1) then
            SobekHisLoc = 0
            do IBoun = 1, ncBoun
              do Iloc = 1, nLocF
                if (IDRead(Iloc) == sobekNodeID(Iboun)) then
                      SobekHisLoc (Iboun) = iloc
                endif
              enddo
            enddo
            ! ARS 15307; also add connections with manholes possible on-line info
            SobekNodeIdPluv = ' '
            Do I=1,NCNode
               ipluv = EiNode(i,2)
               if (EiNode(i,3) .eq. 7) SobekNodeIdPluv(Ipluv) = Id_Nod(i)
            Enddo
            SobekHisLocPluv = 0
            do IPluv = 1, ncPluv
              do Iloc = 1, nLocF
                if (IDRead(Iloc) == SobekNodeIdPluv(Ipluv)) then
                      SobekHisLocPluv (IPluv) = iloc
                endif
              enddo
            enddo
        Endif
        if (idebug .ne. 0) then
            Write(Idebug,*) ' Boundary iboun  id    HisLoc    HisId '
            do IBoun = 1, ncBoun
               ILoc = SobekHisLoc(Iboun)
               if (iloc .gt. 0) then
                  Write(Idebug,*) Iboun, SobekNodeId(iboun)(1:Len_Trim(SobekNodeId(iboun))), &
                                  ILoc, IdRead(iloc)(1:Len_Trim(IdRead(iloc)))
               else
                  Write(Idebug,*) Iboun, SobekNodeId(iboun)(1:Len_Trim(SobekNodeId(iboun)))
               endif
            enddo
            Write(Idebug,*) ' NWRW node ipluv  id    HisLoc    HisId '
            do IPluv = 1, ncPluv
               ILoc = SobekHisLocPluv(Ipluv)
               if (iloc .gt. 0) then
                  Write(Idebug,*) Ipluv, SobekNodeIdPluv(ipluv)(1:Len_Trim(SobekNodeIdPluv(ipluv))), &
                                  ILoc, IdRead(iloc)(1:Len_Trim(IdRead(iloc)))
               else
                  Write(Idebug,*) Ipluv, SobekNodeIdPluv(ipluv)(1:Len_Trim(SobekNodeIdPluv(ipluv)))
               endif
            enddo
        endif

    Endif  !!!(doHeaderSobek)

    ! *********************************************************************
    ! *** read data, store in ReadValues
    ! *********************************************************************
    !
    if (.not. DioPltGet(inDataSet, readValues)) then
       If (NLOCF * NPara .gt. 0) then
          write(*,*) 'Could not read values in DioPltGet'
          call ErrMsgStandard (971, 0, ' Could not get the values from Sobek-CF/SF',NAME)
       endif
    endif

! Put the data from DIO in Sobek arrays
    Do IBoun = 1, ncBoun
       Iloc = SobekHisLoc(Iboun)
       if (Iloc .gt. 0) then
 ! GP sept97 let op indices!
            sbkLvl(Iboun) = ReadValues(1,Iloc)   !tmpSBKLVL(teller)
            bndPar(Iboun, 1) = sbkLvl(Iboun)
!  ARS 546/996: ook initialisatie tijdstap 1
            if (itmstp .eq. 1) bndPar(Iboun,4) = sbkLvl(Iboun)
            if (nPara .ge. 3)  bndPar(Iboun,5) = max (0.01, ReadValues(3,Iloc)) ! tmpArea (teller))
                                                                                ! * timeSettings%timestepSize /600.)
            if (nPara .ge. 4)  bndPar(Iboun,6) = max (0.0,  ReadValues(4,Iloc)) ! tmpDepth(teller))
        ElseIf (bndPar(Iboun,2) .eq. 2) then
        !   on-line coupling of boundary indicated, but no corresponding Sobek location found in HIS file
            IDNode = Id_Nod(BndNam(iboun))
            Write(iOut1,*) ' Error for boundary ', iboun, ' id=',IDNode(1:Len_Trim(idNode))
            call ErrMsgStandard (991,0,' Rdsbkh-DIO',' Sobek_results')
        endif
    Enddo
    Do IPluv = 1, ncPluv
       Iloc = SobekHisLocPluv(Ipluv)
       if (Iloc .gt. 0) then
 ! GP sept97 let op indices!
            sbkLvlPluv(Ipluv) = ReadValues(1,Iloc)   !tmpSBKLVL(teller)
            if (NPara .ge. 3) sbkAreaPluv(Ipluv) = ReadValues(3,Iloc)   !tmpSBKLVL(teller)
            if (NPara .ge. 4) sbkDepthPluv(Ipluv) = ReadValues(4,Iloc)   !tmpSBKLVL(teller)
!       Else
        !   on-line coupling of boundary indicated, but no corresponding Sobek location found in HIS file
!            call ErrMsgStandard (991,0,' Rdsbkh-DIO',' Sobek_results')
       endif
    Enddo


! *********************************************************************
! *** Error
! *********************************************************************

  999 Continue
    !
    ! If this is a shared mem strea:
    ! - keep it open (and don't send the header next timen)
    ! - close it (next step will exactly behave as present one)
    !
    if ( DioPltGetStreamType(inDataSet) .eq. Dio_SharedMem_stream ) then
!        write(*,*) ' ReadDioPlt voor doHeaderSobek =false'
        doHeaderSobek = .false.
    else
!       write(*,*) ' ReadDioPlt voor destroy'
        call DioPltDestroy (inDataSet)
    endif

    if (idebug .ne. 0) write(Idebug,*) ' ReadDioPLtSobekFile is ready'
!   write(*,*) ' ReadDioPlt is ready'

    Return
  End Subroutine ReadDioPltSobekFile



  Subroutine ReadDioPltSaltFile (inDataSet, inName, ITMSTP)


    implicit none

    CHARACTER*6  NAME
    Character*(*)  inName       ! name of input dataset from WQ - Salt
    CHARACTER(CharIdLength) IDNODE

    INTEGER       NLOCHIS, NHIS, IDEBUG, ItmStp, Iout1
    INTEGER,Save  ::  NLOCF, NPara, ClPar
    Integer       I
! ARS 13421: increased dimension of HIS file from 4 parameters to 16 parameters
!    PARAMETER     (NHIS=16,NLOCHIS=9999)
    PARAMETER     (NHIS=16,NLOCHIS=15000)
    CHARACTER(Len=CharIdLength) IDREAD (NLOCHIS)
    CHARACTER(Len=CharIdLength) IDPARA (NHIS)
!   Double Precision RESNOW (NLOC, NHIS)

    Integer        IBoun, ILoc

! DIO var.s
    Character(Len=DioMaxStreamLen)    :: locInName                     ! name of in dataset
    type(DioPltType), intent(inout)   :: inDataSet                     ! plt dataset to be read
    character(Len=DioMaxParLen), pointer, dimension(:) :: parNames     ! par. names
    character(Len=DioMaxLocLen), pointer, dimension(:) :: LocIds     ! loc. names
    Logical, save                     :: doHeaderSalt =  .True.       ! Process Header next time?

    real * 4, dimension(:,:), pointer :: readValues
!
! body
!
      iOut1 = ConfFil_get_iOut1()
      iDebug = ConfFil_get_iDebug()
      if (idebug .ne. 0) WRITE (IDEBUG,1)
    1 FORMAT (' ReadDioPltSaltFile')

! *********************************************************************
! *** read header using DIO
! ***   NLOCF = aantal lokaties in His file
! ***   NPARA = aantal parameters in His file
! *********************************************************************

    if ( doHeaderSalt ) then
        ! Get dataset header and dimensions
        locInName = inName
!        write(*,*) ' ReadDioPlt before DioPltGetDataset Name=',locInName
        inDataSet = DioPltGetDataset(locInName)
        NPARA =  DioPltGetNPar(inDataSet)
        NLOCF =  DioPltGetNLoc(inDataSet)

        ! check dimensions
        IF (NLOCF .GT. NLOCHIS) THEN
            Write(iout1,*) 'ReadDioPltSalt NLoc error: ', NLOCF, NLOCHIS
            call ErrMsgStandard (912, 0, NAME,' NLOCaties in HIS file')
        ELSEIF (NPARA .GT. NHIS) THEN
            Write(iout1,*) 'ReadDioPltSalt NPar error: ', NPara, NHIS
            call ErrMsgStandard (912, 0, NAME,' NPARameters in HIS file')
        ENDIF

! ARS 13421: extension of WQ-RR.HIS file (communication file of Sobeksim with RR/RTC)
        IF (NPARA .LT. 1 .OR. NPARA .GT. 16) call ErrMsgStandard(971, 0,' ReadDioPltSaltFile: Sobek file must contain 2-16 parameters', '')

        ! Get parameter and location names
        parNames => DioPltGetPars(inDataSet)
        LocIds => DioPltGetLocs(inDataSet)
        IDPARA(1:NPara) = parNames(1:NPara)
        IDREAD(1:NLOCF) = LocIds(1:NLOCF)

        if (idebug .ne. 0) THEN
            WRITE(IDEBUG,*)  ' nr nlocations ', NLOCF
            WRITE(IDEBUG,*)  ' location ids ' ,(IDREAD(I),I=1,NLOCF)
            WRITE(IDEBUG,*)  ' nr parameters ', NPara
            WRITE(IDEBUG,*)  ' parameter ids ', (IDPARA(I),I=1,NPara)
        ENDIF
        Do i=1,NPara
           if (IdPara(i)(1:len_trim(IdPara(i))) .eq. 'Cl') ClPar = i
        Enddo

        ! *********************************************************************
        ! *** Check dimensions
        ! *********************************************************************
          IF (NPara .GT. NHIS)  call ErrMsgStandard (912, 0, NAME,' NPARameters in HIS file')

        ! *********************************************************************
        ! *** In First timestep: determine conversion array mapping id's
        ! *** from Sobek HIS file to RR boundaries
        ! *** Note: nrOfSobekNodes may be smaller or larger than nrOfBoundaryNodes !!!!!
        ! ***      - not all boundaries need to be on-line coupled with Sobek
        ! ***      - Sobek His file may contain more locations (for RTC)
        ! *********************************************************************

        If (ITmstp .eq. 1) then
            SobekSaltHisLoc = 0
            do IBoun = 1, ncBoun
              do Iloc = 1, nLocF
                if (IDRead(Iloc) == id_Nod(BndNam(iboun)) ) then
                    SobekSaltHisLoc (Iboun) = iloc
                endif
              enddo
            enddo
        Endif
        if (idebug .ne. 0) then
            Write(Idebug,*) ' Boundary iboun  id    HisLoc    HisId '
            do IBoun = 1, ncBoun
               ILoc = SobekSaltHisLoc(Iboun)
               IdNode = Id_Nod(BndNam(iboun))
               if (iloc .gt. 0) then
                  Write(Idebug,*) Iboun, IDNode(1:Len_Trim(IDNode)), &
                                  ILoc, IdRead(iloc)(1:Len_Trim(IdRead(iloc)))
               else
                  Write(Idebug,*) Iboun, IDNode(1:Len_Trim(IdNode))
               endif
            enddo
        endif

    Endif  !!!(doHeaderSalt)

    ! *********************************************************************
    ! *** read data, store in ReadValues
    ! *********************************************************************
    !
    if (.not. DioPltGet(inDataSet, readValues)) then
       If (NLOCF * NPara .gt. 0) then
          write(*,*) 'Could not read values in DioPltGet'
          call ErrMsgStandard (971, 0, ' Could not get the values from Sobek-WQ-Salt',NAME)
       endif
    endif

! Put the data from DIO in Sobek arrays
    Do IBoun = 1, ncBoun
       Iloc = SobekSaltHisLoc(Iboun)
       if (Iloc .gt. 0 .and. ClPar .gt. 0) then
 ! GP sept97 let op indices!
            SltBnd(Iboun) = ReadValues(Clpar,iloc)
            CBnd  (Iboun) = SltBnd(iboun)
        endif
    Enddo

        if (idebug .ne. 0) THEN
            WRITE(IDEBUG,*)  ' ReadValues(  )', (ReadValues(Clpar,iloc),iloc=1,nlocf)
            WRITE(IDEBUG,*)  ' SltBnd        ', (SltBnd(iboun),iboun=1,NcBoun)
        Endif

! *********************************************************************
! *** Error
! *********************************************************************

  999 Continue
    !
    ! If this is a shared mem strea:
    ! - keep it open (and don't send the header next timen)
    ! - close it (next step will exactly behave as present one)
    !
    if ( DioPltGetStreamType(inDataSet) .eq. Dio_SharedMem_stream ) then
!        write(*,*) ' ReadDioPlt voor doHeaderSobek =false'
        doHeaderSalt = .false.
    else
!       write(*,*) ' ReadDioPlt voor destroy'
        call DioPltDestroy (inDataSet)
    endif

    if (idebug .ne. 0) write(Idebug,*) ' ReadDioPLtSaltFile is ready'
!   write(*,*) ' ReadDioPlt is ready'
    Idebug = 0

    Return
  End Subroutine ReadDioPltSaltFile



  Subroutine CloseDioPlt (InOutDataSet)

    ! arguments
    type(DioPltType), intent(inout) :: InOutDataSet   ! dataset

    ! body: if shared mem, close now
    if ( DioPltGetStreamType(InOutDataSet) .eq. Dio_SharedMem_stream ) then
        call DioPltDestroy (InOutDataSet)
    endif

    return
  End Subroutine CloseDioPlt


end Module Output
