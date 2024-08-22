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

 
module Restart

  !use
  use Conf_fil
  use Conf_Arr
  use Network
  use Link
  use Paved
  use Unpaved
  use Greenhouse
  use Openwater
  use Structures
  use RRRunoff
  use Boundary
  use NWRW
  use RWZI
  use Industry
  use Sacramento
  use RRRouting
  use Salts
  use Messages
  use RR_Meteo
  use ReadLib

#if (!defined(HAVE_CONFIG_H))
!  use dio_states
#endif

  implicit none

#if (!defined(HAVE_CONFIG_H))
! type(DioStates) :: myStates
#endif


 real LowestRestartGroundwaterLevel
 ! LowestRestartGroundwaterLevel = maximale waarde laagste gwl beneden maaiveld
 !  na inlezen van een restart file mag de grondwaterstand niet lager dan deze waarde zijn
 Logical SkipBoundLevelFromRestartFile
 Logical SkipStorageCoefFromRestartFile
 ! oktober 2001
 ! ARS 8471 verzoek Siebe om optioneel de peilen op de randen uit de restart file over te slaan
 Logical RestartOrderStrict
 ! ARS xxxxx March 2002 optie om volgorde in restart file en 3b_nod.Tp file niet hetzelfde te veronderstellen

 Integer MaxNodeIdLength, MaxLinkIdLength


 contains


    SUBROUTINE WRREST (INREST)

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
! ***   Schrijf naar restart file
! *********************************************************************
! *** Input/output parameters:
! *** ------------------------
! ***  INREST = file unit
! ***  IDEBUG = debug file unit
! ***  IMO    = month
! ***  IDAY   = day
! ***  IHOUR  = uur
! *********************************************************************



      CHARACTER(1) QUOTE
      Integer     inRest, iNod, iVhg, iOvh, iKas, iKKl, iOw, iStr, iBnd, iPlv2
      Integer     iPTyp, iPOpp, ISacr, Ilink, ilayer
      Integer     i, j, idum, iDebug, ipoint, icrop, IRRRunoffSub
      Character(CharIdLength) name
      real        rdum

      iDebug = ConfFil_get_iDebug()

      IF (IDEBUG .ne. 0) WRITE (IDEBUG,1)
    1 FORMAT (' WRREST')
      QUOTE = ''''

! *********************************************************************
! *** Schrijf initiele toestand volgende tijdstap in restart file
! *********************************************************************

!Schematisatie
! ARS 11389  Adjustment of RR restartfile --> version 3.208.44
!    Additional info: node external id's
!    Additional info: link id's, node from, node to
! ARS 12818  Adjustment of RR restartfile --> version 3.209.38
!    Additional info: RR routing link info
! ARS 16102  Adjustment of RR restartfile for Scurve data --> version 3.210.08
!    Additional info: RR Scurve unpaved detailed restart info
! ARS *****  Adjustment of RR restartfile for Sacramento data --> version 3.211.04
! ARS *****  Adjustment of RR restartfile for SCS, HBV data --> version 3.212.06
! ARS *****  Adjustment of RR restartfile for NWRW green roof and wadi data --> version 3.212.28
! ARS *****  Adjustment of RR restartfile for NAM --> version 3.212.31
! ARS *****  Adjustment of RR restartfile for OWRain/Evap only --> version 3.212.46
! ARS *****  Adjustment of RR restartfile for LGSI             --> version 3.212.52
! ARS 24832  Adjustment of RR restartfile: add flux info       --> version 3.213.10
! ARS *****  Adjustment of RR restartfile for WagMod           --> version 3.213.18
! ARS *****  Adjustment of RR restartfile for PavedVolDyn      --> version 3.213.22
! ARS *****  Adjustment of RR restartfile for UnpavedKvdLHist. --> version 3.213.29
! ARS *****  Adjustment of RR restartfile for WagMod           --> version 3.213.33
! ARS *****  Adjustment of RR restartfile for new NAM          --> version 3.214.27
! ARS *****  Adjustment of RR restartfile for Walrus           --> version 3.216.21
! ARS *****  Adjustment of RR restartfile for Sacramento       --> version 3.216.29
! ARS *****  Adjustment of RR restartfile for Walrus (addition)--> version 3.216.33

      Write(InRest) ' RestartFile RR version 3.216.33'
      if (idebug .ne. 0) write(Idebug,*) ' RestartFile RR version 3.216.33'

!Additional info: number of nodes, links
      Write(InRest) Ncnode, Nclink
      if (idebug .ne. 0) Write(IDebug,*) 'NcNode, NcLink', Ncnode, Nclink
!Additional info: node ids; first find max. length needed
      MaxNodeIdLength = 0
      DO INOD = 1,NCNODE
         name = Id_Nod(iNod)
         MaxNodeIdLength = Max (MaxNodeIdLength, Len_trim(name))
      ENDDO
      Write(InRest) MaxNodeIdLength
      if (idebug .ne. 0) Write(IDebug,*) 'MaxNodeIdLength', MaxNodeIdLength
      DO INOD = 1,NCNODE
         name = Id_Nod(iNod)
         WRITE (INREST)  EiNode(INOD,1), EiNode(INOD,3), INDSLT(INOD), name(1:MaxNodeIdLength)
         if (idebug .ne. 0) WRITE (IDebug,*)  EiNode(INOD,1), EiNode(INOD,3), INDSLT(INOD), name(1:MaxNodeIdLength)
      ENDDO
!Additional info: link ids; first find max. length needed
      MaxLinkIdLength = 0
      DO ILink= 1,NCLink
         name = NameLink(ilink)
         MaxLinkIdLength = Max (MaxLinkIdLength, Len_trim(name))
      ENDDO
      Write (InRest)  MaxLinkIdLength, MaxNodeIdLength
      if (idebug .ne. 0) Write (IDebug,*)  'MaxLinkIdLength, MaxNodeIdLength', MaxLinkIdLength, MaxNodeIdLength
      DO Ilink= 1,NCLink
         WRITE (INREST)  Namelink(ilink)(1:MaxLinkIdLength), &
                          NameFromNode(ilink)(1:MaxNodeIdLength), &
                           NameToNode(ilink)(1:MaxNodeIdLength)
         if (idebug .ne. 0) WRITE (IDebug,*)  Namelink(ilink)(1:MaxLinkIdLength), &
                          NameFromNode(ilink)(1:MaxNodeIdLength), &
                           NameToNode(ilink)(1:MaxNodeIdLength)
      ENDDO

!General: internal id's, node type, salt index and name
      DO INOD = 1,NCNODE
         name = NamNod(iNod)
         WRITE (INREST)   EiNode(INOD,1), EiNode(INOD,3), INDSLT(INOD), name(1:CharIdLength)
         if (idebug .ne. 0) WRITE (IDebug,*)   EiNode(INOD,1), EiNode(INOD,3), INDSLT(INOD), name(1:CharIdLength)
      ENDDO

!Verhard
      if (idebug .ne. 0) WRITE (Idebug,*) 'paved'
      DO INOD = 1,NCNODE
         IF (EiNode(INOD,3) .EQ. 1) THEN
           IVHG = EiNode(INOD,2)
           WRITE (INREST) EiNode(INOD,1), BVSTR(IVHG), BVRL(IVHG,1), BVRL(IVHG,2), PavedVolDyn(ivhg)
           if (idebug .ne. 0) WRITE (Idebug,*) EiNode(INOD,1), BVSTR(IVHG), BVRL(IVHG,1), BVRL(IVHG,2), PavedVolDyn(ivhg)
         ENDIF
      ENDDO

!Onverhard
      if (idebug .ne. 0) WRITE (Idebug,*) 'unpaved'
      DO INOD = 1,NCNODE
         IF (EiNode(INOD,3) .EQ. 2) THEN
           IOVH = EiNode(INOD,2)
! Nu met OnvZone volume ipv summer storage SumSto
!          WRITE (INREST) EiNode(INOD,1), BOLND(IOVH), BOBD(IOVH), GWL(IOVH), SUMSTO(IOVH)
! Nu BoBd en OnvZone%ActualVolume double precision zijn, als real lezen/schrijven ivm backwards compatibility
           WRITE (INREST) EiNode(INOD,1), Sngl(BOLND(IOVH)), Sngl(BOBD(IOVH)), Sngl(GWL(IOVH)), Sngl(OnvZone(IOVH)%Actual_Volume)
           if (idebug .ne. 0) WRITE (IDebug,*) EiNode(INOD,1), Sngl(BOLND(IOVH)), Sngl(BOBD(IOVH)), Sngl(GWL(IOVH)), Sngl(OnvZone(IOVH)%Actual_Volume)
           WRITE (INREST) BERGC(IOVH)
           if (idebug .ne. 0) WRITE (IDebug,*) BERGC(IOVH)
! ARS 16102 Write Scurve data
           WRITE (INREST)  UseScurve(IOVH),UseUnpavedScurve, CapsimPerCropArea, UnsatZoneOption
           if (idebug .ne. 0) WRITE (IDebug,*) 'Scurve', UseScurve(IOVH),UseUnpavedScurve, CapsimPerCropArea, UnsatZoneOption
           if (UnsatZoneOption .ne. 0) then
              WRITE (INREST)  (CropOnvZone(IOVH,iCrop)%Actual_Volume,iCrop=1,NCrop)
              if (idebug .ne. 0) WRITE (IDebug,*)  (CropOnvZone(IOVH,iCrop)%Actual_Volume,iCrop=1,NCrop)
           endif
           if (UseScurve(iovh) .eq. 1) then
              write(InRest)  (AreaScurve(iovh,ipoint)%ActualSurfaceStorage, &
                               AreaScurve(iovh,ipoint)%ActualSoilStorage,&
                                ipoint=1,UseUnpavedScurve)
              if (idebug .ne. 0) write(Idebug,*)  (AreaScurve(iovh,ipoint)%ActualSurfaceStorage, &
                               AreaScurve(iovh,ipoint)%ActualSoilStorage,&
                                ipoint=1,UseUnpavedScurve)
              if (CapsimPerCropArea .eq. 1 .and. UnsatZoneOption .ne. 0) then
                  write(InRest) ((AreaScurvePerCrop(iovh,icrop,ipoint)%ActualSurfaceStorage, &
                                   AreaScurveperCrop(iovh,icrop,ipoint)%ActualSoilStorage,&
                                    icrop=1,NCrop),ipoint=1,UseUnpavedScurve)
                  if (idebug .ne. 0) write(IDebug,*) ((AreaScurvePerCrop(iovh,icrop,ipoint)%ActualSurfaceStorage, &
                                   AreaScurveperCrop(iovh,icrop,ipoint)%ActualSoilStorage,&
                                    icrop=1,NCrop),ipoint=1,UseUnpavedScurve)
              endif
           endif
! JIRA xxxxx Write KvdLHistory, only relevant if CompOption=2
           Write(Inrest) KvdlDimensie
           Write(Inrest) (KvdLeur(iovh,i)%neerslag, KvdLeur(iovh,i)%Kopbolling, KvdLeur(iovh,i)%Kdebiet, i=1,KvdLDimensie)
           Write(Inrest) KvdLeurnul(iovh)%KOpbollingnul,&
                          KvdLeurnul(iovh)%Kdebietnul, &
                           KvdLeurnul(iovh)%KOpbollinglast,&
                            KvdLeurnul(iovh)%Kdebietlast,&
                             KvdLeurnul(iovh)%peilnul,&
                              KvdLeurnul(iovh)%openwaterlevel
         ENDIF
      ENDDO

 !Kassen
      if (idebug .ne. 0) WRITE (Idebug,*) ' Greenhouses'
      DO INOD = 1,NCNODE
         IF (EiNode(INOD,3) .EQ. 3) THEN
           IKAS = EiNode(INOD,2)
           WRITE (INREST) EiNode(INOD,1), BKASD(IKAS),  &
                              (BKAS(IKAS,IKKL),IKKL=1,NCKKL), SiloB(ikas)
           if (idebug .ne. 0) WRITE (IDebug,*) EiNode(INOD,1), BKASD(IKAS),  &
                              (BKAS(IKAS,IKKL),IKKL=1,NCKKL), SiloB(ikas)
         ENDIF
      ENDDO

!Open water
      if (idebug .ne. 0) WRITE (Idebug,*) ' OpenWater'
      DO INOD = 1,NCNODE
         IF (EiNode(INOD,3) .EQ. 4) THEN
         IOW = EiNode(INOD,2)
         WRITE (INREST) EiNode(INOD,1), LVLOW(IOW)
         if (idebug .ne. 0) WRITE (IDebug,*) EiNode(INOD,1), LVLOW(IOW)
         ENDIF
      ENDDO

!Kunstwerken
      if (idebug .ne. 0) WRITE (Idebug,*) ' RR-Structures'
      DO INOD = 1,NCNODE
         IF (EiNode(INOD,3) .EQ. 5) THEN
           ISTR = EiNode(INOD,2)
           WRITE (INREST) EiNode(INOD,1), STRSTA(ISTR)
           if (idebug .ne. 0) WRITE (IDebug,*) EiNode(INOD,1), STRSTA(ISTR)
         ENDIF
      ENDDO

!Randen
      if (idebug .ne. 0) WRITE (Idebug,*) ' RR-Boundaries'
      DO INOD = 1,NCNODE
         IF (EiNode(INOD,3) .EQ. 6) THEN
           IBND = EiNode(INOD,2)
           WRITE (INREST) EiNode(INOD,1), (BNDPAR(IBND,I),I=1,5)
           if (idebug .ne. 0) WRITE (IDebug,*) EiNode(INOD,1), (BNDPAR(IBND,I),I=1,5)
         ENDIF
      ENDDO

!NWRW
      if (idebug .ne. 0) WRITE (Idebug,*) ' NWRW'
      DO IPLV2 = 1,NCPLV2
        WRITE (INREST) IPLV2, VOLOP(IPLV2), VOLDYN(IPLV2)
        if (idebug .ne. 0) WRITE (IDebug,*) IPLV2, VOLOP(IPLV2), VOLDYN(IPLV2)
        DO IPTYP= 1,NPTYP
          DO IPOPP= 1,NPOPP
             WRITE (INREST) BVOP(IPLV2,IPTYP,IPOPP),    &
                            DT (IPLV2,IPTYP, IPOPP,1),   &
                            DT (IPLV2,IPTYP, IPOPP,2),   &
                            INFCP  (IPLV2,IPTYP, IPOPP,1),   &
                            INFCP  (IPLV2,IPTYP, IPOPP,2),   &
                            INFSTS (IPLV2,IPTYP, IPOPP,1),   &
                            INFSTS (IPLV2,IPTYP, IPOPP,2),   &
                            NTRAIN (IPLV2,IPTYP, IPOPP),   &
                            NTRRST (IPLV2,IPTYP, IPOPP),   &
                            INFCP(IPLV2,IPTYP,IPOPP,1),    &
                            INFCP(IPLV2,IPTYP,IPOPP,2)
!!! April 2003: INFCP dubbel?
             if (idebug .ne. 0) WRITE (IDebug,*) BVOP(IPLV2,IPTYP,IPOPP),    &
                              DT (IPLV2,IPTYP, IPOPP,1),   &
                              DT (IPLV2,IPTYP, IPOPP,2),   &
                              INFCP  (IPLV2,IPTYP, IPOPP,1),   &
                              INFCP  (IPLV2,IPTYP, IPOPP,2),   &
                              INFSTS (IPLV2,IPTYP, IPOPP,1),   &
                              INFSTS (IPLV2,IPTYP, IPOPP,2),   &
                              NTRAIN (IPLV2,IPTYP, IPOPP),   &
                              NTRRST (IPLV2,IPTYP, IPOPP),   &
                              INFCP(IPLV2,IPTYP,IPOPP,1),    &
                              INFCP(IPLV2,IPTYP,IPOPP,2)
          ENDDO
        ENDDO
      ENDDO

!RWZI, Industry: no data needed
!RWZI = type 14
!Industry = type 15

!Sacramento
      if (idebug .ne. 0) Write (Idebug,*) 'Sacramento'
      Write (Inrest) WriteRestartFileWithAdimC
      if (idebug .ne. 0) Write (Idebug,*) WriteRestartFileWithAdimC
      DO INOD = 1,NCNODE
         IF (EiNode(INOD,3) .EQ. 16) THEN
           ISacr = EiNode(INOD,2)
           If (WriteRestartFileWithAdimC) then
              WRITE (INREST) EiNode(INOD,1), UZTWC(ISacr), UZFWC(ISacr), &
                                                      LZTWC(ISacr), LZFPC(ISacr), &
                                                      LZFSC(ISacr), ADIMC(ISacr), &
                                                      (Sacr_QQ (iSacr,idum), idum=1,501), &
                                                      (Sacr_QD (iSacr,idum), idum=1,501), &
                                                      (Sacr_QS (iSacr,idum), idum=1,501), &
                                                      (Sacr_QI (iSacr,idum), idum=1,501)
              if (idebug .ne. 0) WRITE (IDebug,*) EiNode(INOD,1), UZTWC(ISacr), UZFWC(ISacr), &
                                                      LZTWC(ISacr), LZFPC(ISacr), &
                                                      LZFSC(ISacr), ADIMC(ISacr), &
                                                      (Sacr_QQ (iSacr,idum), idum=1,501), &
                                                      (Sacr_QD (iSacr,idum), idum=1,501), &
                                                      (Sacr_QS (iSacr,idum), idum=1,501), &
                                                      (Sacr_QI (iSacr,idum), idum=1,501)
           Else
              WRITE (INREST) EiNode(INOD,1), UZTWC(ISacr), UZFWC(ISacr), &
                                                      LZTWC(ISacr), LZFPC(ISacr), &
                                                      LZFSC(ISacr), &
                                                      (Sacr_QQ (iSacr,idum), idum=1,501), &
                                                      (Sacr_QD (iSacr,idum), idum=1,501), &
                                                      (Sacr_QS (iSacr,idum), idum=1,501), &
                                                      (Sacr_QI (iSacr,idum), idum=1,501)
              if (idebug .ne. 0) WRITE (IDebug,*) EiNode(INOD,1), UZTWC(ISacr), UZFWC(ISacr), &
                                                      LZTWC(ISacr), LZFPC(ISacr), &
                                                      LZFSC(ISacr), &
                                                      (Sacr_QQ (iSacr,idum), idum=1,501), &
                                                      (Sacr_QD (iSacr,idum), idum=1,501), &
                                                      (Sacr_QS (iSacr,idum), idum=1,501), &
                                                      (Sacr_QI (iSacr,idum), idum=1,501)
           Endif
         ENDIF
      ENDDO

      if (idebug .ne. 0) Write (Idebug,*) 'Links'
      DO ILink = 1,NCLink
         If (LinkType(ilink) .eq. 30) then     ! RRRouting link
            Write(Inrest) MuskingumNlayers(ilink)
            if (idebug .ne. 0) Write(Idebug,*) MuskingumNlayers(ilink)
            DO Ilayer= 1,MuskingumNLayers(ilink)
               write(Inrest) MuskingumQoutold(ilink,ilayer), MuskingumQinold(ilink,ilayer)
               if (idebug .ne. 0) write(IDebug,*) MuskingumQoutold(ilink,ilayer), MuskingumQinold(ilink,ilayer)
            Enddo
         Endif
      Enddo

!Zout
      if (idebug .ne. 0) Write (Idebug,*) 'Salt'
      IF (ISLCMP .ne. 0) THEN
        DO INOD = 1,NCSALT
           rdum = SaltF(inod)
           WRITE (INREST) INOD, Rdum
           if (idebug .ne. 0) WRITE (IDebug,*) INOD, Rdum
        ENDDO
      ELSE
        DO INOD = 1,NCSALT
           WRITE (INREST) INOD, 0.0
           if (idebug .ne. 0) WRITE (IDebug,*) INOD, 0.0
        ENDDO
      ENDIF

!NWRW special
      if (idebug .ne. 0) Write (Idebug,*) 'NWRW Special'
      DO IPLV2 = 1,NCPLV3
        WRITE (INREST) IPLV2, SpecialVOLOP(IPLV2), SpecialVOLDYN(IPLV2)
        if (idebug .ne. 0) WRITE (IDebug,*) IPLV2, SpecialVOLOP(IPLV2), SpecialVOLDYN(IPLV2)
        WRITE (INREST) SpecialBVOP(IPLV2),    &
                       SpecialDT (IPLV2,1),   &
                       SpecialDT (IPLV2,2),   &
                       SpecialINFCP  (IPLV2,1),   &
                       SpecialINFCP  (IPLV2,2),   &
                       SpecialINFSTS (IPLV2,1),   &
                       SpecialINFSTS (IPLV2,2),   &
                       SpecialNTRAIN (IPLV2),   &
                       SpecialNTRRST (IPLV2),   &
                       SpecialINFCP(IPLV2,1),    &
                       SpecialINFCP(IPLV2,2)
        if (idebug .ne. 0) WRITE (IDebug,*) SpecialBVOP(IPLV2),    &
                       SpecialDT (IPLV2,1),   &
                       SpecialDT (IPLV2,2),   &
                       SpecialINFCP  (IPLV2,1),   &
                       SpecialINFCP  (IPLV2,2),   &
                       SpecialINFSTS (IPLV2,1),   &
                       SpecialINFSTS (IPLV2,2),   &
                       SpecialNTRAIN (IPLV2),   &
                       SpecialNTRRST (IPLV2),   &
                       SpecialINFCP(IPLV2,1),    &
                       SpecialINFCP(IPLV2,2)
!!! April 2003: Special INFCP dubbel?
      ENDDO

! EXTR, HBV, SCS, NAM       ! Jira 20559
      if (idebug .ne. 0) Write (Idebug,*) 'EXT, HBV, SCS, NAM'
      If (NcRRRunoff .gt. 0) then
         Do i=1,NCRRRunoff
            write(InRest) RRRunoff_CompOption(i)
            IRRRunoffSub = RRRunoff_SubIndex(i)
            if (idebug .ne. 0) write(IDebug,*) RRRunoff_CompOption(i)
            if (RRRunoff_CompOption(i) .eq. 0) then
               ! External Runoff node: no additional info
                write(IDebug,*) 'EXT no data'
            elseif (RRRunoff_CompOption(i) .eq. 1) then
               ! HBV node
               if (idebug .ne. 0) write(IDebug,*) 'HBV'
               Write(InRest) HBV_DrySnowContent(iRRRunoffSub), HBV_FreeWaterContent(iRRRunoffSub), HBV_SoilMoisture(iRRRunoffSub), &
                                HBV_UpperZoneContent(iRRRunoffSub), HBV_LowerZoneContent(iRRRunoffSub)
               if (idebug .ne. 0) Write(IDebug,*) HBV_DrySnowContent(iRRRunoffSub), HBV_FreeWaterContent(iRRRunoffSub), &
                                                   HBV_SoilMoisture(iRRRunoffSub), HBV_UpperZoneContent(iRRRunoffSub), &
                                                     HBV_LowerZoneContent(iRRRunoffSub)
            elseif (RRRunoff_CompOption(i) .eq. 2) then
               ! SCS node
               if (idebug .ne. 0) write(IDebug,*) 'SCS'
               Write(InRest) SCS_PAccum(iRRRunoffSub), SCS_PExcess(iRRRunoffSub), SCS_Storage(iRRRunoffSub), SCS_UseBaseflow(iRRRunoffSub)
               if (idebug .ne. 0) Write(IDebug,*) SCS_PAccum(iRRRunoffSub), SCS_PExcess(iRRRunoffSub), SCS_Storage(iRRRunoffSub), SCS_UseBaseflow(iRRRunoffSub)
               Write(InRest) (SCS_AvailableRunoff(iRRRunoffSub,j),j=1,MaxTc)
               if (idebug .ne. 0) Write(IDebug,*) (SCS_AvailableRunoff(iRRRunoffSub,j),j=1,MaxTc)
               if (SCS_UseBaseFlow(iRRRunoffSub)) then
                  Write(InRest) SCS_SurfAct(iRRRunoffSub), SCS_SubSurfAct(iRRRunoffSub), SCS_GWAct(iRRRunoffSub)
                  if (idebug .ne. 0) Write(Idebug,*) SCS_SurfAct(iRRRunoffSub), SCS_SubSurfAct(iRRRunoffSub), SCS_GWAct(iRRRunoffSub)
               endif
            elseif (RRRunoff_CompOption(i) .eq. 3) then
               ! NAM node; updated to new NAM sept 2016
               if (idebug .ne. 0) write(IDebug,*) 'NAM'
               Write(InRest) NAM_U(iRRRunoffSub), NAM_L(iRRRunoffSub), NAM_GWSD(iRRRunoffSub)
               if (idebug .ne. 0) Write(IDebug,*) NAM_U(iRRRunoffSub), NAM_L(iRRRunoffSub), NAM_GWSD(iRRRunoffSub)
            elseif (RRRunoff_CompOption(i) .eq. 4) then    ! LGSI
               Write(InRest) (LGSI_NewGwl(IRRRunoffSub,j),j=1,LGSI_NrSubAreas(iRRRunoffSub))
               Write(InRest) (LGSI_HistoryQtot(iRRRunoffSub,j),j=1,LGSI_MaxDelayLengthPlus1)
            elseif (RRRunoff_CompOption(i) .eq. 5) then    ! Wageningen model
               Write(InRest) WagMod_SM(iRRRunoffSub), WagMod_GStore(iRRRunoffSub), WagMod_QSNEW(iRRRunoffSub)
! Issue 50469
!              Write(InRest) WagMod_QG(iRRRunoffSub,Wagmod_ActNrTimestepsSimulation)
!              Write(InRest) Wagmod_MaxNrTimestepsSimulation - Wagmod_ActNrTimestepsSimulation
!              Write(InRest) (WagMod_QG(iRRRunoffSub,j),j=Wagmod_ActNrTimestepsSimulation+1, Wagmod_MaxNrTimestepsSimulation)
               Write(InRest) WagMod_QG(iRRRunoffSub,1)
               Write(InRest) Wagmod_MaxNrTimesteps+1
               Write(InRest) (WagMod_QG(iRRRunoffSub,j),j=1, Wagmod_MaxNrTimesteps+1)
               if (idebug .ne. 0) Write(Idebug, *) ' Wrrest IRRunoff, Wagmod', I, IRRRunoffSub
               if (idebug .ne. 0) Write(Idebug, *) ' SM     SMT1   ', WagMod_SM(IRRRunoffSub), WagMod_SMT1(IRRRunoffSub)
               if (idebug .ne. 0) Write(Idebug, *) ' GStore Gstore1', WagMod_GStore(IRRRunoffSub), WagMod_GStoreT1(IRRRunoffSub)
! Issue 50469
!              if (idebug .ne. 0) Write(Idebug, *) ' QGT1', WagMod_QG(IRRRunoffSub,WagMod_ActNrTimestepsSimulation)
!              if (idebug .ne. 0) Write(Idebug, *) ' QG',  (WagMod_QG(IRRRUnoffSub,j),j=Wagmod_ActNrTimestepsSimulation+1,Wagmod_ActNrTimestepsSimulation+10)
               if (idebug .ne. 0) Write(Idebug, *) ' QGT1', WagMod_QG(IRRRunoffSub,1)
               if (idebug .ne. 0) Write(Idebug, *) ' QG',  (WagMod_QG(IRRRUnoffSub,j),j=1,11)
! Issue 50469
!              Write(InRest) (WagMod_QD(iRRRunoffSub,j),j=Wagmod_ActNrTimestepsSimulation+1, Wagmod_MaxNrTimestepsSimulation)
               Write(InRest) (WagMod_QD(iRRRunoffSub,j),j=1,Wagmod_MaxNrTimesteps+1)
               if (idebug .ne. 0) Write(Idebug, *) ' QD',  (WagMod_QD(IRRRunoffSub,j),j=1,11)
            elseif (RRRunoff_CompOption(i) .eq. 6) then    ! Walrus model
! to be added: Walrus
               Write(InRest) Walrus_HSCurrent(iRRRunoffSub)
               Write(InRest) Walrus_HQCurrent(iRRRunoffSub)
               Write(InRest) Walrus_DGCurrent(iRRRunoffSub)
               Write(InRest) Walrus_DVCurrent(iRRRunoffSub)
               Write(InRest) Walrus_lastQ (iRRRunoffSub)
! additional for 3.216.33:
               Write(InRest) Walrus_dVEQCurrent (iRRRunoffSub)
               Write(InRest) Walrus_lastFGS (iRRRunoffSub)
               Write(InRest) Walrus_lastFQS (iRRRunoffSub)
               Write(InRest) Walrus_WICurrent (iRRRunoffSub)
               Write(InRest) Walrus_BetaCurrent (iRRRunoffSub)
            elseif (RRRunoff_CompOption(i) .gt. 6) then
               call ErrMsgStandard(981, 0,'  Wrrest',' Error in RR-Runoff computation option - unknown')
            endif
         Enddo
      endif

!green roofs NWRW


!Wadi infiltration NWRW
      if (idebug .ne. 0) write(IDebug,*) 'Wadi infiltration'
      Do i=1,NCPluv
         Write(InRest) WadiFinalLevel(i), WadiFinalStorage(i)
         if (idebug .ne. 0) Write(IDebug,*) WadiFinalLevel(i), WadiFinalStorage(i)
      Enddo
!Open water precip/evap only
      if (idebug .ne. 0) write(IDebug,*) 'Open water precip/evap only'
      Do i=1,NCOwRain
         Write(InRest) OwRainArea(i)
         if (idebug .ne. 0) Write(IDebug,*) OwRainArea(i)
      Enddo

! Link flows    (not needed for proper restart, but needed for continuity of lines in FEWS)
       Do i=1,NcLink
          Write(InRest) (QLink(i,j),j=1,MaxSeriesPerMap(13))
       Enddo
! Other node fluxes
       Do i=1,Ncvhg
          Write(InRest) (VHG_Tnul(j,i),j=1,MaxSeriesPerMap(1))
       Enddo
       Do i=1,Ncovhg
          Write(InRest) (OVH_Tnul(j,i),j=1,MaxSeriesPerMap(2))
       Enddo
       Do i=1,NcKas
          Write(InRest) (Kas_Tnul(j,i),j=1,MaxSeriesPerMap(3))
       Enddo
       Do i=1,NcOw
          Write(InRest) (OW_Tnul(j,i),j=1,MaxSeriesPerMap(4))
       Enddo
       Do i=1,NcStru
          Write(InRest) (Str_Tnul(j,i),j=1,MaxSeriesPerMap(5))
       Enddo
       Do i=1,NcBoun
          Write(InRest) (Bnd_Tnul(j,i),j=1,MaxSeriesPerMap(6))
       Enddo
       Do i=1,NcPluv
          Write(InRest) (Plv_Tnul(j,i),j=1,MaxSeriesPerMap(7))
       Enddo
! 8 = balansuitvoer, skip (initieel per definitie nul)
        IF (ISLCMP .NE. 0) THEN
           Do i=1,NcSalt
              Write(InRest) (Slt_Tnul(j,i),j=1,MaxSeriesPerMap(9))
           Enddo
        ELSE
           Do i=1,NcSalt ! still write as much, but don't use i
              Write(InRest) (Slt_Tnul(j,1),j=1,MaxSeriesPerMap(9))
           Enddo
        ENDIF
       Do i=1,NcRwzi
          Write(InRest) (RWZI_Tnul(j,i),j=1,MaxSeriesPerMap(10))
       Enddo
       Do i=1,NcIndus
          Write(InRest) (Ind_Tnul(j,i),j=1,MaxSeriesPerMap(11))
       Enddo
       Do i=1,NcSacr
          Write(InRest) (Sacr_Tnul(j,i),j=1,MaxSeriesPerMap(12))
       Enddo
! 13 = flows, 14=cel
       Do i=1,NcRRRunoff
          Write(InRest) (RRRunoff_Tnul(j,i),j=1,MaxSeriesPerMap(15))
       Enddo



      RETURN
    END subroutine WrRest




      SUBROUTINE RDREST (INREST, RestartVersion)

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
! ***   Lees initialisatie uit restart file
! *********************************************************************
! *** Input/output parameters:
! *** ------------------------
! ***  INREST = file unit
! ***  IDEBUG = debug file unit
! *********************************************************************

      Integer inRest, iNod, iDum, iDum1, iDum2, iDum3
      Integer iVhg, iOvh, iKas, iKKl, iOW,  iStr, ibnd, iplv, iplv2, iplv3, iPOpp, iPTyp, ISacr
      Integer jVhg, jOvh, jKas, jOW, jStr,  jSacr
      integer i, j, teller, JNod, ilink, jlink, ilayer, inode
      Real    rdum, rDum1, rDum2, rDum3, rdum4, RDumm(10), RdumSacr_qq(501), rhelp
      Real    RdumSacr_qd(501),  RdumSacr_qs(501),  RdumSacr_qi(501)
      real    inputinitialpercentage
      CHARACTER(CharIdLength) NAME, nodeName, LinkNm1,FromNodeNm, ToNodeNm
      CHARACTER(32) Version
      CHARACTER(32) RestartVersion
      Integer iDebug, RestartOrderConversion(Nnod), LinkRestartOrderConversion(NLnk)
      Logical Found, NewVersion, OrderSame
      integer  nrpoints, idumuse, ipoint, icrop, idumCapsimPerCrop, idumUnSatZoneOption, iout
      ! OrderSame=true means restart file and current schematisation are in same order
      Real ROnvZone, TVol, Btot, TSlt
      Integer ikind, islt, inr, RestartKvdLDimensie
      integer IRRRunoffSub

      Real    PeilArray(6), AreaArray(6), VolumeArray(6)
      Real    ExtraPeilArray(NStorageCurvePoints), ExtraVolumeArray(NStorageCurvePoints)

      Integer          D_ifreal

      iDebug = ConfFil_get_iDebug()
      iOut = ConfFil_get_iOut1()
!
      IF (iDebug .ne. 0) WRITE (IDEBUG,1)
    1 FORMAT (' RDREST')
!
! *********************************************************************
! *** Lees initiele toestand uit restart file
! *********************************************************************
! Schematisatie
      IF (iDebug .ne. 0)  WRITE(IDEBUG,*) '*Schematisation '
      RestartOrderConversion = 0
      LinkRestartOrderConversion = 0
      OrderSame = .true.

      Read (InRest,Err=999,End=999)  Version
      RestartVersion = Version
      if (idebug .ne. 0) Write(Idebug,*) Version
!     NewVersion = restart file with Sacramento
      NewVersion = .true.

      If (Version .eq. ' RestartFile RR version 3.208.44' .or. &
           Version .eq. ' RestartFile RR version 3.209.38' .or. &
            Version .eq. ' RestartFile RR version 3.210.08' .or. &
             Version .eq. ' RestartFile RR version 3.211.04' .or. &
              Version .eq. ' RestartFile RR version 3.212.06' .or. &
               Version .eq. ' RestartFile RR version 3.212.28' .or. &
                Version .eq. ' RestartFile RR version 3.212.31' .or. &
                 Version .eq. ' RestartFile RR version 3.212.46' .or. &
                  Version .eq. ' RestartFile RR version 3.212.52' .or. &
                   Version .eq. ' RestartFile RR version 3.213.10' .or. &
                    Version .eq. ' RestartFile RR version 3.213.18' .or. &
                     Version .eq. ' RestartFile RR version 3.213.22' .or. &
                      Version .eq. ' RestartFile RR version 3.213.29' .or. &
                       Version .eq. ' RestartFile RR version 3.213.33' .or. &
                        Version .eq. ' RestartFile RR version 3.214.27' .or. &
                         Version .eq. ' RestartFile RR version 3.216.21' .or. &
                          Version .eq. ' RestartFile RR version 3.216.29' .or. &
                           Version .eq. ' RestartFile RR version 3.216.33') then
          RestartOrderStrict = .false.
!         Read additional info in this file: number of node and links, node id's, link id's
          Read (InRest,Err=999,End=999)  Idum1, Idum2
          if (idebug .ne. 0) Write(Idebug,*) 'nr nodes and links ', Idum1, Idum2
          If (Idum1 .ne. NcNode .or. Idum2 .ne. NcLink) call ErrMsgStandard(952, 0,'  Rdrest',' Restart file Number of nodes/links')
!         Read node id's from restart file, check with current id's, set RestartOrderConversion array
!         Order of node id's in current 3B_Nod.Tp file and restart file may be different!
          Read (InRest,Err=999,End=999)  MaxNodeIdLength
          if (idebug .ne. 0) Write(Idebug,*) 'maxnodeIdlength',  MaxNodeIdLength
          If (MaxNodeIdLength .gt. CharIdLength) call ErrMsgStandard(952, 0,'  Rdrest',' Restart file length of node ids')
          DO INOD = 1,NCNODE
             Name = ' '
             READ (INREST,Err=999,End=999)  IDUM1, IDUM2, IDUM3, NAME(1:MaxNodeIdLength)
             if (idebug .ne. 0) Write(Idebug,*) IDUM1, IDUM2, IDUM3, NAME(1:MaxNodeIdLength)
             Found = .false.
             Jnod  = 0
             Do while (.not. found .and. (Jnod .lt. Ncnode))
                JNOD=Jnod+1
                nodeName = Id_Nod(JNOD)
                IF ( NAME(1:Len_trim(NAME)) .eq. nodeName(1:Len_trim(nodeName)) .and. &
                      EiNode(JNOD,3) .EQ. Idum2) then
                    Found = .true.
                    RestartOrderConversion(Jnod) = idum1
                    if (Jnod .ne. idum1) OrderSame = .false.
                Endif
             ENDDO
             If (.not. found) call ErrMsgStandard (952, 0,'  Rdrest',' Restart file node info')
          Enddo
          if (idebug .ne. 0) then
             if (idebug .ne. 0) write(idebug,*) ' RestartOrderConversion '
             Do inod=1,Ncnode
                if (idebug .ne. 0) write(idebug,*) inod, RestartOrderConversion(inod)
             Enddo
          Endif
!         Additional link info
          Read (InRest,Err=999,End=999)  MaxLinkIdLength, MaxNodeIdLength
          if (idebug .ne. 0) Write(Idebug,*) 'MaxlinkIdLength MaxnodIdLength ', MaxLinkIdLength, MaxNodeIdLength
          If (MaxNodeIdLength .gt. CharIdLength) call ErrMsgStandard(952, 0,'  Rdrest',' Restart file length of node ids')
          If (MaxLinkIdLength .gt. CharIdLength) call ErrMsgStandard(952, 0,'  Rdrest',' Restart file length of link ids')
          DO Ilink= 1,NCLink
             Linknm1 = ' '
             FromNodeNm = ' '
             ToNodeNm = ' '
             Read (INREST)  Linknm1(1:MaxLinkIdLength), FromNodeNm(1:MaxNodeIdLength),ToNodeNm(1:MaxNodeIdLength)
             if (idebug .ne. 0) Write(Idebug,*) Linknm1(1:MaxLinkIdLength), FromNodeNm(1:MaxNodeIdLength),ToNodeNm(1:MaxNodeIdLength)
             ! Check whether link has same id, same from and same to node
             Found = .false.
             Jlink = 0
             Do while (.not. found .and. (Jlink .lt. Nclink))
                Jlink=Jlink+1
                nodeName = NameLink(Jlink)
                IF ( LinkNm1(1:Len_trim(LinkNm1)) .eq. nodeName(1:Len_trim(nodeName)) .and. &
                      FromNodeNm(1:Len_trim(FromNodeNm)) .eq. NameFromNode(ilink)(1:Len_trim(NameFromNode(ilink))) .and. &
                       ToNodeNm(1:Len_trim(ToNodeNm)) .eq. NameToNode(ilink)(1:Len_trim(NameToNode(ilink))) ) then
                    Found = .true.
                    LinkRestartOrderConversion(Jlink) = ilink
                Endif
             ENDDO
             If (.not. found) call ErrMsgStandard (952, 0,'  Rdrest',' Restart file link info')
          ENDDO
          if (idebug .ne. 0) then
             write(idebug,*) ' LinkRestartOrderConversion '
             Do ilink=1,Nclink
                write(idebug,*) ilink, LinkRestartOrderConversion(ilink)
             Enddo
          Endif

      ElseIf (Version .eq. ' RestartFile RR version 3.207.43') then
!         ivm problems with Regge, put restart order to strict
          RestartOrderStrict = .true.
      Else
          RestartOrderStrict = .true.
          NewVersion = .false.
          Rewind(InRest)
          if (idebug .ne. 0) Write(Idebug,*) 'rewind restart file'
      Endif

      DO INOD = 1,NCNODE
         If (NewVersion) then
            READ (INREST,Err=999,End=999)  IDUM1, IDUM2, IDUM3, NAME(1:CharIdLength)
            if (idebug .ne. 0) Write(Idebug,*) IDUM1, IDUM2, IDUM3, NAME(1:CharIdLength)
         Else
            READ (INREST,Err=999,End=999)  IDUM1, IDUM2, IDUM3, NAME(1:32)
            if (idebug .ne. 0) Write(Idebug,*) IDUM1, IDUM2, IDUM3, NAME(1:32)
         Endif
         IF (iDebug .ne. 0) WRITE (IDEBUG,*)  IDUM1, IDUM2, IDUM3, NAME

         If (RestartOrderStrict) then
! oude test, vereist zelfde volgorde in restart file als in 3b_nod.tp file
           IF (IDUM1 .NE. EiNode(INOD,1)    &
              .OR. IDUM2 .NE. EiNode(INOD,3)   &
               .OR. IDUM3 .NE. INDSLT(INOD) )    &
                 call ErrMsgStandard (952, 1,'  Rdrest',' Restart file')
            RestartOrderConversion(Inod) = Idum1
! ARS 10362 no check on name anymore
         Else
!           RestartOrder not strict only allowed in restart file version 3.208.44, taken care of in block 0
         Endif
      ENDDO
!Verhard
      IF (iDebug .ne. 0)  WRITE(IDEBUG,*) '*Paved area'
      DO jVHG = 1,NCVHG
         If (Version .eq. ' RestartFile RR version 3.213.22' .or. &
              Version .eq. ' RestartFile RR version 3.213.29' .or. &
               Version .eq. ' RestartFile RR version 3.213.33' .or. &
                Version .eq. ' RestartFile RR version 3.214.27' .or. &
                 Version .eq. ' RestartFile RR version 3.216.21' .or. &
                  Version .eq. ' RestartFile RR version 3.216.29' .or. &
                   Version .eq. ' RestartFile RR version 3.216.33') then
            READ (INREST,Err=999,End=999) IDUM, RDUM1, RDUM2, RDUM3, RDUM4
         else
            READ (INREST,Err=999,End=999) IDUM, RDUM1, RDUM2, RDUM3
            RDUM4 = 0.0
         endif
!         Write(*,*) ' Read Restart file block 2-pav', Idum
         IF (iDebug .ne. 0) WRITE (IDEBUG,*) IDUM, RDUM1, RDUM2, RDUM3, RDUM4
         Found = .false.
         Inod  = 0
         Do while (.not. found .and. (Inod .lt. Ncnode))
            INOD=Inod+1
! Oud       IF (EiNode(INOD,1) .EQ. IDUM .AND. EiNode(INOD,3) .EQ. 1) Found = .true.
            IF (RestartOrderConversion(Inod) .eq. Idum .and. EiNode(INOD,3) .EQ. 1) Found = .true.
         ENDDO
         if (idebug .ne. 0) write(idebug,*)  inod, RestartOrderConversion(inod), idum, EiNode(inod,3)
         If (.not. found) call ErrMsgStandard (952, 2,'  Rdrest',' Restart file')
         IF (RestartOrderStrict .and. jVHG .NE. EiNode(INOD,2)) call ErrMsgStandard (952, 2,'  Rdrest',' Restart file')
         Ivhg = EiNode(INOD,2)
         BVSTR (IVHG)   = RDUM1
         BVRL (IVHG,1)  = RDUM2
         BVRL (IVHG,2)  = RDUM3
         PavedVolDyn(IVHG) = RDUM4
! Initial conditions
         BVSTR0(IVHG)   = RDUM1
         BVRL0(IVHG,1)  = RDUM2
         BVRL0(IVHG,2)  = RDUM3
         PavedVolDyn0(IVHG) = RDUM4
      ENDDO

!Onverhard
      IF (iDebug .ne. 0)  WRITE(IDEBUG,*) '*Unpaved area'
      DO jOVH = 1,NCOVHG
! Nu met OnvZone volume ipv summer storage SumSto
!         READ (INREST) IDUM, BOLND(IOVH), BOBD(IOVH), GWL(IOVH), SUMSTO(IOVH)
! April 2002: also Actual Volume unsaturated zone read as real, in arrays stored as double

!        READ (INREST,Err=999,End=999) IDUM, BOLND(IOVH), RDum1, GWL(IOVH), RDum2
         READ (INREST,Err=999,End=999) IDUM, Rdum3, RDum1, Rdum4, RDum2
         if (idebug .ne. 0) Write(Idebug,*) IDUM, Rdum3, RDum1, Rdum4, RDum2
!         Write(*,*) ' Read Restart file block 3-unp', Idum
         Found = .false.
         Inod  = 0
         Do while (.not. found .and. (Inod .lt. Ncnode))
            INOD=Inod+1
            IF (RestartOrderConversion(Inod) .eq. Idum .and. EiNode(INOD,3) .EQ. 2) Found = .true.
         ENDDO
         If (.not. found) call ErrMsgStandard (952, 3,'  Rdrest',' Restart file')
         IF (RestartOrderStrict .and. jovh .NE. EiNode(INOD,2)) call ErrMsgStandard (952, 3,'  Rdrest',' Restart file')
         Iovh = EiNode(INOD,2)

         BoBd (Iovh) = Rdum1
         BoLnd(Iovh) = Rdum3
         Gwl  (Iovh) = Rdum4
         OnvZone(IOVH)%Actual_Volume = Rdum2
         OnvZone(Iovh)%Actual_mm = OnvZone(Iovh)%Actual_Volume / AreaOh(Iovh) / mm2m
         if (SkipStorageCoefFromRestartFile) then
            Read (InRest,Err=999,End=999) rdum
         else
            Read (InRest,Err=999,End=999) BERGC(IOVH)
         endif
         if (idebug .ne. 0) Write(Idebug,*) BERGC(IOVH), rdum
! ARS 16102 Read Scurve data
         If (Version .eq. ' RestartFile RR version 3.210.08' .or. &
              Version .eq. ' RestartFile RR version 3.211.04' .or. &
               Version .eq. ' RestartFile RR version 3.212.06' .or. &
                Version .eq. ' RestartFile RR version 3.212.28' .or. &
                 Version .eq. ' RestartFile RR version 3.212.31' .or. &
                  Version .eq. ' RestartFile RR version 3.212.46' .or. &
                   Version .eq. ' RestartFile RR version 3.212.52' .or. &
                    Version .eq. ' RestartFile RR version 3.213.10' .or. &
                     Version .eq. ' RestartFile RR version 3.213.18' .or. &
                      Version .eq. ' RestartFile RR version 3.213.22' .or. &
                       Version .eq. ' RestartFile RR version 3.213.29' .or. &
                        Version .eq. ' RestartFile RR version 3.213.33' .or. &
                         Version .eq. ' RestartFile RR version 3.214.27' .or. &
                          Version .eq. ' RestartFile RR version 3.216.21' .or. &
                           Version .eq. ' RestartFile RR version 3.216.29' .or. &
                            Version .eq. ' RestartFile RR version 3.216.33') then
           Read (INREST)  idumuse, nrPoints, idumCapsimPerCrop, idumUnsatZoneOption
           if (idebug .ne. 0) Write(Idebug,*)  idumuse, nrPoints, idumCapsimPerCrop, idumUnsatZoneOption
           if (idumUnsatZoneOption .ne. 0) then
               if (UnsatZoneOption .eq. idumUnsatZoneOption) then
                  Read (INREST)  (CropOnvZone(IOVH,iCrop)%Actual_Volume,iCrop=1,NCrop)
                  if (idebug .ne. 0) Write(Idebug,*)  (CropOnvZone(IOVH,iCrop)%Actual_Volume,iCrop=1,NCrop)
                  Do ICrop=1,NCrop
                     CropOnvZone(IOVH,icrop)%Actual_mm = 0.0
                     if (AreaGW(iovh,icrop) .gt. 0) CropOnvZone(IOVH,icrop)%Actual_mm = CropOnvZONE(IOVH,icrop)%Actual_Volume / AreaGW(Iovh,icrop) / mm2m
                     CropOnvZone(IOVH,icrop)%init_mm   = CropOnvZONE(IOVH,icrop)%Actual_mm
                  Enddo
                  useRestart210(iovh) = .true.
               else
                  write(iout,*) ' Restart file Capsim data skipped for unpaved node', id_nod(inod)
                  Read (INREST)  (rdum,iCrop=1,NCrop) ! TODO: shouldn't this be a double precision?
                  if (idebug .ne. 0) Write(Idebug,*) (rdum,iCrop=1,NCrop)
               endif
           endif
           if (idumuse .eq. 1) then
             if (nrPoints .eq. UseUnpavedScurve .and. UseScurve(iovh) .eq. 1) then
                  read(InRest) (AreaScurve(iovh,ipoint)%ActualSurfaceStorage, &
                                 AreaScurve(iovh,ipoint)%ActualSoilStorage, &
                                  ipoint=1,UseUnpavedScurve)
                  if (idebug .ne. 0) Write(Idebug,*) (AreaScurve(iovh,ipoint)%ActualSurfaceStorage, &
                                    AreaScurve(iovh,ipoint)%ActualSoilStorage, &
                                     ipoint=1,UseUnpavedScurve)
                  do ipoint=1,UseUnpavedScurve
                     AreaScurve(iovh,ipoint)%InitialSoilStorage = AreaScurve(iovh,ipoint)%ActualSoilStorage
                  enddo
                  ScurveDataToBeSetByRR(iovh) = .false.
             else
                  write(iout,*) ' Restart file Scurve data skipped for unpaved node', id_nod(inod)
                  write(iout,*) ' Either the number of Scurve points has changed, or the Scurve is not used anymore'
                  read(InRest) (rdum, rdum, ipoint=1,nrPoints)  ! TODO: shouldn't this be a double precision?
                  if (idebug .ne. 0) Write(Idebug,*)  (rdum, rdum, ipoint=1,nrPoints)
                  ScurveDataToBeSetByRR(iovh) = .true.
             endif
             if (CapsimPerCropArea .eq. 1 .and. UnsatZoneOption .ne. 0 .and. idumCapsimPerCrop .eq. CapsimPerCropArea) then
                read(InRest) ((AreaScurvePerCrop(iovh,icrop,ipoint)%ActualSurfaceStorage, &
                                AreaScurvePerCrop(iovh,icrop,ipoint)%ActualSoilStorage,&
                                 icrop=1,NCrop),ipoint=1,UseUnpavedScurve)
                if (idebug .ne. 0) Write(Idebug,*) ((AreaScurvePerCrop(iovh,icrop,ipoint)%ActualSurfaceStorage, &
                                AreaScurvePerCrop(iovh,icrop,ipoint)%ActualSoilStorage,&
                                 icrop=1,NCrop),ipoint=1,UseUnpavedScurve)
                do icrop=1,NCrop
                   do ipoint=1,UseUnpavedScurve
                      AreaScurvePerCrop(iovh,icrop,ipoint)%InitialSoilStorage = AreaScurvePerCrop(iovh,icrop,ipoint)%ActualSoilStorage
                   enddo
                enddo
                ScurveDataPerCropToBeSetByRR(iovh) = .false.
             elseif (idumUnsatZoneOption .ne. 0) then
                  write(iout,*) ' Restart file Scurve data per crop skipped for unpaved node', id_nod(inod)
                  write(iout,*) ' The switch CapsimPerCropArea has been changed or Capsim has been switched off'
                  read(InRest) ((rdum, rdum, icrop=1,NCrop), ipoint=1,nrPoints)  ! TODO: shouldn't this be a double precision?
                  if (idebug .ne. 0) Write(Idebug,*) ((rdum, rdum, icrop=1,NCrop), ipoint=1,nrPoints)
                  ScurveDataPerCropToBeSetByRR(iovh) = .true.
             endif
           endif
! JIRA xxxxx KvdLHistory, only relevant if CompOption=2
           If (Version .eq. ' RestartFile RR version 3.213.29' .or. &
                Version .eq. ' RestartFile RR version 3.213.33' .or. &
                 Version .eq. ' RestartFile RR version 3.214.27' .or. &
                  Version .eq. ' RestartFile RR version 3.216.21' .or. &
                   Version .eq. ' RestartFile RR version 3.216.29' .or. &
                    Version .eq. ' RestartFile RR version 3.216.33') then
              Read(Inrest) RestartKvdLDimensie
              if (RestartKvdLDimensie .ne. KvdLDimensie) then
                  write(iout,*) ' Cannot read Krayenhoff-vdLeur restart info since dimensions have been changed; Probably other value for KVDLDIMDAYS= option in Delft_3b.Ini file or other computation timestepsize'
                  write(iout,*) ' Found in restart file:',RestartKvdLDimensie
                  write(iout,*) ' In present version/input:',KvdLDimensie
                  call ErrMsgStandard(981, 0,'  Rdrest',' Error in RR-Unpaved Krayenhoff vd Leur restart info')
              endif
              Read(Inrest) (KvdLeur(iovh,i)%neerslag, KvdLeur(iovh,i)%Kopbolling, KvdLeur(iovh,i)%Kdebiet, i=1,RestartKvdLDimensie)
              if (idebug .ne. 0) then
                 write(Idebug,*) ' KvdLHistory info: idum , neerslag, Kopbolling, Kdebiet '
                 do idum=1,RestartKvdLDimensie
                    write(Idebug,*) KvdLeur(iovh,idum)%neerslag, KvdLeur(iovh,idum)%KOpbolling, KvdLeur(iovh,idum)%Kdebiet
                 enddo
              endif
              Read(Inrest) KvdLeurnul(iovh)%KOpbollingnul,&
                            KvdLeurnul(iovh)%Kdebietnul, &
                             KvdLeurnul(iovh)%KOpbollinglast,&
                              KvdLeurnul(iovh)%Kdebietlast,&
                               KvdLeurnul(iovh)%peilnul,&
                                KvdLeurnul(iovh)%openwaterlevel
           Endif
         Endif
         If (Gwl(iovh) .lt. LvlOh(iovh)-LowestRestartGroundwaterLevel) then
! aanpassing initieel grondwaterpeil en berging bodem, zodat init. gwl niet meer dan LowestRestartGwl beneden maaiveld
            if (idebug .ne. 0) write(idebug,*) ' Adjust init. gwl en Bobd',Gwl(iovh), Bobd(iovh), &
                                                 LvlOh(iovh), LowestRestartGroundWaterLevel
            Rdum1 = LvlOh(iovh) - LowestRestartGroundwaterLevel !nieuwe init. grondwaterstand
            Rdum2 = Gwl(iovh)  !oude init. groundwaterstand
            Gwl(iovh)  = Rdum1
            BoBd(iovh) = BoBd(iovh) * (1.0 + ((Rdum1-Rdum2)*BergC(iovh) / Inidepthgwl(iovh) ) )
         endif
! Initial conditions
         BoBd0(Iovh) = BoBd(iovh)
         Gwl0(iovh) = Gwl(iovh)
         OnvZone(IOVH)%Init_Volume = OnvZone(IOVH)%Actual_Volume
         OnvZone(Iovh)%Init_mm     = OnvZone(Iovh)%Actual_mm
! ARS 12450
! also set Initial storage bodem, on land
         BIniBd(Iovh) = BoBd(iovh)
         BIniOl(Iovh) = BoLnd(iovh)
! end ARS 12450

         IF (iDebug .ne. 0) WRITE(IDEBUG,*) IDUM, BOLND(IOVH), BOBD(IOVH), GWL(IOVH), OnvZone(iovh)%Actual_Volume, BERGC(IOVH)
      ENDDO

!Kassen
      IF (iDebug .ne. 0)  WRITE(IDEBUG,*) '*Greenhouse area'
      DO jKAS = 1,NCKAS
!        READ (INREST,Err=999,End=999) IDUM, BKASD(IKAS), (BKAS(IKAS,IKKL),IKKL=1,NCKKL), SiloB(ikas)
!         Write(*,*) ' Read Restart file block 4-kas', Idum
         READ (INREST,Err=999,End=999) IDUM, Rdum1, (RDumm(ikkl),IKKL=1,NCKKL), Rdum2
         if (idebug .ne. 0) Write(IDebug,*) IDUM, Rdum1, (RDumm(ikkl),IKKL=1,NCKKL), Rdum2
         Found = .false.
         Inod  = 0
         Do while (.not. found .and. (Inod .lt. Ncnode))
            INOD=Inod+1
            IF (RestartOrderConversion(Inod) .eq. Idum .and. EiNode(INOD,3) .EQ. 3) Found = .true.
         ENDDO
        If (.not. found) call ErrMsgStandard (952, 4,'  Rdrest',' Restart file')
         IF (RestartOrderStrict .and. JKas .NE. EiNode(INOD,2)) call ErrMsgStandard (952, 4,'  Rdrest',' Restart file')
         Ikas = EiNode(INOD,2)
         BKASD(IKAS) = Rdum1
         Do ikkl=1,NcKkl
            BKAS(IKAS,IKKL) = Rdumm(ikkl)
         Enddo
         SiloB(ikas) = Rdum2
         IF (iDebug .ne. 0) WRITE(IDEBUG,*) IDUM, BKASD(IKAS), (BKAS(IKAS,IKKL),IKKL=1,NCKKL), SiloB(ikas)
      ENDDO
! Vector Init initial condition
      BKasD0 = BkasD
      BKas0  = Bkas

!Open water
      IF (iDebug .ne. 0)  WRITE(IDEBUG,*) '*Openwater'
      DO jOW  = 1,NCOW
!        READ (INREST,Err=999,End=999) IDUM, LVLOW(IOW)
         READ (INREST,Err=999,End=999) IDUM, Rdum1
!         Write(*,*) ' Read Restart file block 5-ow', Idum
         Found = .false.
         Inod  = 0
         Do while (.not. found .and. (Inod .lt. Ncnode))
            INOD=Inod+1
            IF (RestartOrderConversion(Inod) .eq. Idum .and. EiNode(INOD,3) .EQ. 4) Found = .true.
         ENDDO
         If (.not. found) call ErrMsgStandard (952, 5,'  Rdrest',' Restart file')
         IF (RestartOrderStrict .and. Jow .NE. EiNode(INOD,2)) call ErrMsgStandard (952, 5,'  Rdrest',' Restart file')
         Iow  = EiNode(INOD,2)
         LVLOW(IOW)  = Rdum1
         IF (iDebug .ne. 0) WRITE(IDEBUG,*) IDUM, LVLOW(IOW)
         Do i=1,NVal
            PeilArray(i) = PeilOW(i,iow)
            AreaArray(i) = AreaOW(i,iow)
            VolumeArray(i) = VOLUOW(i,iow)
         Enddo
         CALL RR_INTERP (NVAL, PeilArray, AreaArray, LVLOW (IOW),  AROW (IOW), IDUM)
! ARS 6115 Okt 2000: VolOw is double, Rdum is single precision
         CALL RR_INTERP (NVAL, PeilArray, VolumeArray, LVLOW (IOW),  RDum1, IDUM)
         VolOw(iow) = Rdum1
! ARS 16190: add initial volume on inundated area!!
         Do i=1,NrBergendOppervlakIndices(iow)
            ExtraPeilArray(i)   = ExtraBergendOppPeil(i,iow)
            ExtraVolumeArray(i) = ExtraBergendOppVolume(i,iow)
         Enddo
         CALL RR_INTERP (NrBergendOppervlakIndices(iow), ExtraPeilArray, &
                         ExtraVolumeArray, LvlOw(IOW), Rhelp,idum)
         ActualExtraBergendVolume(iow) = max (0.0, RHelp)
         TotalVolume(iow) = VolOw(iow) + ActualExtraBergendVolume(iow)
!        VolOw(iow) = VolOw(iow) + ActualExtraBergendVolume(iow)
! ARS xxxx: voorkom problemen met negatieve oppervlakken door laag peil
         ArOw(iow) = max (0.0001, ArOw(iow))
         IF (iDebug .ne. 0) WRITE(IDEBUG,*) IOW, LVLOW(IOW), VolOw(iow)
! initial condition
         LvlOw0(iow) = LvlOw(iow)
         VolOw0(iow) = VolOw(iow)
         ArOw0(iow)  = ArOw(iow)
      ENDDO

!Structure
      IF (iDebug .ne. 0)  WRITE(IDEBUG,*) '*Structure'
      DO jSTR = 1,NCSTRU
!        READ (INREST,Err=999,End=999) IDUM, STRSTA(ISTR)
         READ (INREST,Err=999,End=999) IDUM, Idum2
!         Write(*,*) ' Read Restart file block 6-str', Idum
         Found = .false.
         Inod  = 0
         Do while (.not. found .and. (Inod .lt. Ncnode))
            INOD=Inod+1
            IF (RestartOrderConversion(Inod) .eq. Idum .and. EiNode(INOD,3) .EQ. 5) Found = .true.
         ENDDO
         If (.not. found) call ErrMsgStandard (952, 6,'  Rdrest',' Restart file')
         IF (RestartOrderStrict .and. Jstr .NE. EiNode(INOD,2)) call ErrMsgStandard (952, 6,'  Rdrest',' Restart file')

         Istr = EiNode(INOD,2)
         StrSta(istr) = idum2
         IF (iDebug .ne. 0) WRITE(IDEBUG,*) IDUM, STRSTA(ISTR)
      ENDDO

!Boundary
      IF (iDebug .ne. 0)  WRITE(IDEBUG,*) '*Boundary'
      DO teller = 1,NCBOUN
! ARS 8471 optioneel boundary levels overslaan
!        if (SkipBoundLevelFromRestartFile) then
            READ (INREST,Err=999,End=999) IDUM, (RDumm(I),I=1,5)
!            Write(*,*) ' Read Restart file block 7-bound', Idum
!        else
!           READ (INREST,Err=999,End=999) IDUM, (BNDPAR(teller,I),I=1,5)
!        endif
         Found = .false.
         Inod  = 0
         Do while (.not. found .and. (Inod .lt. Ncnode))
            INOD=Inod+1
            IF (RestartOrderConversion(Inod) .eq. Idum .and. EiNode(INOD,3) .EQ. 6) Found = .true.
         ENDDO
         If (.not. found) call ErrMsgStandard (952, 7,'  Rdrest',' Restart file')
         IF (RestartOrderStrict .and. teller .NE. EiNode(INOD,2)) call ErrMsgStandard (952, 7,'  Rdrest',' Restart file')
         Ibnd = EiNode(INOD,2)
! ARS 8471 optioneel boundary levels overslaan
         If (.not. SkipBoundLevelFromRestartFile) then
            Do i=1,5
               BndPar(ibnd,I) = Rdumm(i)
            Enddo
         Endif
         if (idebug .ne. 0) WRITE(IDEBUG,*) IDUM,(Rdumm(I),I=1,5)
      ENDDO


      If (NCPLV2 .GT. 0) Then
!NWRW nodes
        IF (iDebug .ne. 0)  WRITE(IDEBUG,*) '*Pluvius'
        If (.not. OrderSame .and. NcPlv2 .gt. 1) &
           call ErrMsgStandard (974, 8,'  Restart file NWRW not in same order; may give strange results',' ')
        DO IPLV2 = 1,NCPLV2
          READ (INREST,Err=999,End=999) IDUM, VOLOP(IPLV2), VOLDYN(IPLV2)
          if (idebug .ne. 0) Write(Idebug,*) IDUM, VOLOP(IPLV2), VOLDYN(IPLV2)
!          Write(*,*) ' Read Restart file block 8-nwrw', Idum
          IF (iDebug .ne. 0) WRITE(IDEBUG,*) ' Pluvius', IDUM
          IF (IDUM .NE. IPLV2) call ErrMsgStandard (952, 8,'  Rdrest',' Restart file')
          DO IPTYP= 1,NPTYP
            DO IPOPP= 1,NPOPP
               READ (INREST,Err=999,End=999) BVOP(IPLV2,IPTYP,IPOPP),   &
                             DT (IPLV2,IPTYP, IPOPP,1),   &
                             DT (IPLV2,IPTYP, IPOPP,2),   &
                             INFCP  (IPLV2,IPTYP, IPOPP,1),   &
                             INFCP  (IPLV2,IPTYP, IPOPP,2),   &
                             INFSTS (IPLV2,IPTYP, IPOPP,1),   &
                             INFSTS (IPLV2,IPTYP, IPOPP,2),   &
                             NTRAIN (IPLV2,IPTYP, IPOPP),     &
                             NTRRST (IPLV2,IPTYP, IPOPP),     &
                             INFCP(IPLV2,IPTYP,IPOPP,1),    &
                             INFCP(IPLV2,IPTYP,IPOPP,2)
               if (idebug .ne. 0) Write(Idebug,*) BVOP(IPLV2,IPTYP,IPOPP),   &
                             DT (IPLV2,IPTYP, IPOPP,1),   &
                             DT (IPLV2,IPTYP, IPOPP,2),   &
                             INFCP  (IPLV2,IPTYP, IPOPP,1),   &
                             INFCP  (IPLV2,IPTYP, IPOPP,2),   &
                             INFSTS (IPLV2,IPTYP, IPOPP,1),   &
                             INFSTS (IPLV2,IPTYP, IPOPP,2),   &
                             NTRAIN (IPLV2,IPTYP, IPOPP),     &
                             NTRRST (IPLV2,IPTYP, IPOPP),     &
                             INFCP(IPLV2,IPTYP,IPOPP,1),    &
                             INFCP(IPLV2,IPTYP,IPOPP,2)
!!! April 2003: INFCP dubbel?
               VOLOP0 (IPLV2) = VOLOP0 (IPLV2) + BVOP(IPLV2,IPTYP,IPOPP)
            ENDDO
          ENDDO
        ENDDO
      Endif

! RWZI, Industry: no data needed

! Sacramento
        If (NewVersion) then
           Read (Inrest,Err=999,End=999) ReadAdimCInRestartFile
           if (idebug .ne. 0) Write(Idebug,*)  ReadAdimCInRestartFile
           if (idebug .ne. 0) Write (Idebug,*) 'ReadAdimC', ReadAdimCInRestartFile
           DO JSacr = 1,NcSacr
              If (ReadAdimCInRestartFile) then
! correction test
                If (Version .eq. ' RestartFile RR version 3.211.04' .or. &
                     Version .eq. ' RestartFile RR version 3.212.06' .or. &
                      Version .eq. ' RestartFile RR version 3.212.28' .or. &
                       Version .eq. ' RestartFile RR version 3.212.31' .or. &
                        Version .eq. ' RestartFile RR version 3.212.46' .or. &
                         Version .eq. ' RestartFile RR version 3.212.52' .or. &
                          Version .eq. ' RestartFile RR version 3.213.10' .or. &
                           Version .eq. ' RestartFile RR version 3.213.18' .or. &
                            Version .eq. ' RestartFile RR version 3.213.22' .or. &
                             Version .eq. ' RestartFile RR version 3.213.29' .or. &
                              Version .eq. ' RestartFile RR version 3.213.33' .or. &
                               Version .eq. ' RestartFile RR version 3.214.27' .or. &
                                Version .eq. ' RestartFile RR version 3.216.21' .or. &
                                 Version .eq. ' RestartFile RR version 3.216.29' .or. &
                                  Version .eq. ' RestartFile RR version 3.216.33') then
                  RdumSacr_QQ = 0
                  RdumSacr_QD = 0
                  RdumSacr_QS = 0
                  RdumSacr_QI = 0
                  If (Version .eq. ' RestartFile RR version 3.216.29' .or. &
                       Version .eq. ' RestartFile RR version 3.216.33') then
                     READ (INREST,Err=999,End=999) IDUM, (Rdumm(idum1),idum1=1,6), &
                                                   (RdumSacr_QQ (idum2), idum2=1,501), &
                                                    (RdumSacr_QD (idum2), idum2=1,501), &
                                                     (RdumSacr_QS (idum2), idum2=1,501), &
                                                      (RdumSacr_QI (idum2), idum2=1,501)
                     if (idebug .ne. 0) Write(Idebug,*) ' Sacr', IDUM, (Rdumm(idum1),idum1=1,6), &
                                             (RdumSacr_QQ (idum2), idum2=1,501)
                     if (idebug .ne. 0) Write(Idebug,*) ' Sacr QD, QS, QI', (RdumSacr_QD (idum2), idum2=1,501)
                  else
                     READ (INREST,Err=999,End=999) IDUM, (Rdumm(idum1),idum1=1,6), &
                                             (RdumSacr_QQ (idum2), idum2=1,501)
                     if (idebug .ne. 0) Write(Idebug,*) ' Sacr', IDUM, (Rdumm(idum1),idum1=1,6), &
                                             (RdumSacr_QQ (idum2), idum2=1,501)
                  endif
                else
                  READ (INREST,Err=999,End=999) IDUM, (Rdumm(idum1),idum1=1,6), &
                                          (RdumSacr_QQ (idum2), idum2=1,121)
                  if (idebug .ne. 0) Write(Idebug,*) IDUM, (Rdumm(idum1),idum1=1,6), &
                                          (RdumSacr_QQ (idum2), idum2=1,121)
                endif
              Else
! correction test
                If (Version .eq. ' RestartFile RR version 3.211.04' .or. &
                     Version .eq. ' RestartFile RR version 3.212.06' .or. &
                      Version .eq. ' RestartFile RR version 3.212.28' .or. &
                       Version .eq. ' RestartFile RR version 3.212.31' .or. &
                        Version .eq. ' RestartFile RR version 3.212.46' .or. &
                         Version .eq. ' RestartFile RR version 3.212.52' .or. &
                          Version .eq. ' RestartFile RR version 3.213.10' .or. &
                           Version .eq. ' RestartFile RR version 3.213.18' .or. &
                            Version .eq. ' RestartFile RR version 3.213.22' .or. &
                             Version .eq. ' RestartFile RR version 3.213.29' .or. &
                              Version .eq. ' RestartFile RR version 3.213.33' .or. &
                               Version .eq. ' RestartFile RR version 3.214.27' .or. &
                                Version .eq. ' RestartFile RR version 3.216.21' .or. &
                                 Version .eq. ' RestartFile RR version 3.216.29' .or. &
                                  Version .eq. ' RestartFile RR version 3.216.33') then
                  RdumSacr_QQ = 0
                  RdumSacr_QD = 0
                  RdumSacr_QS = 0
                  RdumSacr_QI = 0
                  If (Version .eq. ' RestartFile RR version 3.216.29' .or. &
                       Version .eq. ' RestartFile RR version 3.216.33') then
                     READ (INREST,Err=999,End=999) IDUM, (Rdumm(idum1),idum1=1,5), &
                                          (RdumSacr_QQ (idum2), idum2=1,501), &
                                          (RdumSacr_QD (idum2), idum2=1,501), &
                                          (RdumSacr_QS (idum2), idum2=1,501), &
                                          (RdumSacr_QI (idum2), idum2=1,501)
                  else
                     READ (INREST,Err=999,End=999) IDUM, (Rdumm(idum1),idum1=1,5), &
                                          (RdumSacr_QQ (idum2), idum2=1,501)
                     if (idebug .ne. 0) Write(Idebug,*) IDUM, (Rdumm(idum1),idum1=1,5), &
                                          (RdumSacr_QQ (idum2), idum2=1,501)
                  endif
                else
                  READ (INREST,Err=999,End=999) IDUM, (Rdumm(idum1),idum1=1,5), &
                                          (RdumSacr_QQ (idum2), idum2=1,121)
                  if (idebug .ne. 0) Write(Idebug,*) IDUM, (Rdumm(idum1),idum1=1,5), &
                                          (RdumSacr_QQ (idum2), idum2=1,121)
                endif
              Endif
!              Write(*,*) ' Read Restart file block 9-Sacr', Idum

              Found = .false.
              Inod  = 0
              Do while (.not. found .and. (Inod .lt. Ncnode))
                 INOD=Inod+1
                 IF (RestartOrderConversion(Inod) .eq. Idum .and. EiNode(INOD,3) .EQ. 16) Found = .true.
              ENDDO
              If (.not. found) call ErrMsgStandard (952, 9,'  Rdrest',' Restart file')
              IF (RestartOrderStrict .and. JSacr .NE. EiNode(INOD,2)) call ErrMsgStandard (952, 9,'  Rdrest',' Restart file')
              ISacr = EiNode(INOD,2)

              UZTWCInit(Isacr) = Rdumm(1)
              UZFWCInit(Isacr) = Rdumm(2)
              LZTWCInit(Isacr) = Rdumm(3)
              LZFPCInit(Isacr) = Rdumm(4)
              LZFSCInit(Isacr) = Rdumm(5)
              if (ReadAdimCInRestartFile) AdimC (ISacr) = Rdumm(6)
              Do idum1=1,501
                 Sacr_QQ (Isacr,idum1) = RDumSacr_Qq(idum1)
              Enddo
              IF (iDebug .ne. 0) WRITE(IDEBUG,*) IDUM
           ENDDO
        Endif

! Routing link info added in 3.209.38
        If (Version .eq. ' RestartFile RR version 3.209.38' .or. &
             Version .eq. ' RestartFile RR version 3.210.08' .or. &
              Version .eq. ' RestartFile RR version 3.211.04' .or. &
               Version .eq. ' RestartFile RR version 3.212.06' .or. &
                Version .eq. ' RestartFile RR version 3.212.28' .or. &
                 Version .eq. ' RestartFile RR version 3.212.31' .or. &
                  Version .eq. ' RestartFile RR version 3.212.46' .or. &
                   Version .eq. ' RestartFile RR version 3.212.52' .or. &
                    Version .eq. ' RestartFile RR version 3.213.10' .or. &
                     Version .eq. ' RestartFile RR version 3.213.18' .or. &
                      Version .eq. ' RestartFile RR version 3.213.22' .or. &
                       Version .eq. ' RestartFile RR version 3.213.29' .or. &
                        Version .eq. ' RestartFile RR version 3.213.33' .or. &
                         Version .eq. ' RestartFile RR version 3.214.27' .or. &
                          Version .eq. ' RestartFile RR version 3.216.21' .or. &
                           Version .eq. ' RestartFile RR version 3.216.29' .or. &
                            Version .eq. ' RestartFile RR version 3.216.33') then
           if (idebug .ne. 0) Write(Idebug,*) 'Routing links'
           DO ILink = 1,NCLink
              If (LinkType(ilink) .eq. 30) then     ! RRRouting link
                 Read(Inrest) idum
                 if (idebug .ne. 0) Write(Idebug,*) idum
                 If (Idum .ne. MuskingumNLayers(ilink)) then
                    call ErrMsgStandard(952, 10,' ',' Restart file; Routing link number of layers has changed')
                 Endif
                 DO Ilayer= 1,MuskingumNLayers(ilink)
                    Read(Inrest) MuskingumQoutold(ilink,ilayer), MuskingumQinold(ilink,ilayer)
                    if (idebug .ne. 0) Write(Idebug,*) MuskingumQoutold(ilink,ilayer), MuskingumQinold(ilink,ilayer)
                 Enddo
              Endif
           Enddo
        Endif
!       Write(*,*) ' Read Restart file block 10-RRRoutinglink', Idum

! Salt
        IF (ISLCMP .ne. 0) THEN
           IF (iDebug .ne. 0)  WRITE(IDEBUG,*) '*Salt'
           If (.not. OrderSame .and. NcSalt .gt. 1) &
              call ErrMsgStandard (974, 10,'  Restart file Salt not in same order; use initial salt data',' ')
           DO jNOD = 1,NCSALT
!             READ (INREST,END=999,ERR=999) IDUM, SALTF(INOD)
              READ (INREST,END=999,ERR=999) IDUM, Rdum1
              if (idebug .ne. 0) Write(Idebug,*) 'Salt', IDUM, Rdum1
!             Write(*,*) ' Read Restart file block 10-Salt', Idum
! ARS !!      No conversion yet (only warning above:974), conversion can be done using IndSlt and looking at nodetype
              if (OrderSame) SaltF(jNod) = Rdum1
          ENDDO
        ELSE
           DO jNOD = 1,NCSALT
              READ (INREST,END=999,ERR=999) IDUM, Rdum1
              if (idebug .ne. 0) Write(Idebug,*) 'Salt', IDUM, Rdum1
          ENDDO
        ENDIF

! NWRW Special areas, March 2003 Taiwan
! Added at the end of the restart file to make sure it is backwards compatible
        If (NCPLV3 .GT. 0) Then
          IF (iDebug .ne. 0)  WRITE(IDEBUG,*) '*Pluvius Special areas'
          If (.not. OrderSame .and. NcPlv3 .gt. 1) &
              call ErrMsgStandard (974, 10,'  Restart file Special NWRW not in same order; may give strange results',' ')
          DO IPLV2 = 1,NCPLV3
            READ (INREST,Err=999,End=999) IDUM, SpecialVOLOP(IPLV2), SpecialVOLDYN(IPLV2)
            if (idebug .ne. 0) Write(Idebug,*) 'Special area', IDUM, SpecialVOLOP(IPLV2), SpecialVOLDYN(IPLV2)
            IF (iDebug .ne. 0) WRITE(IDEBUG,*) ' Pluvius', IDUM
            IF (IDUM .NE. IPLV2) call ErrMsgStandard (952, 11,'  Rdrest',' Restart file')
            READ (INREST,Err=999,End=999) SpecialBVOP(IPLV2),   &
                                  SpecialDT (IPLV2,1),   &
                                  SpecialDT (IPLV2,2),   &
                                  SpecialINFCP  (IPLV2,1),   &
                                  SpecialINFCP  (IPLV2,2),   &
                                  SpecialINFSTS (IPLV2,1),   &
                                  SpecialINFSTS (IPLV2,2),   &
                                  SpecialNTRAIN (IPLV2),     &
                                  SpecialNTRRST (IPLV2),     &
                                  SpecialINFCP(IPLV2,1),    &
                                  SpecialINFCP(IPLV2,2)
            if (idebug .ne. 0) Write(Idebug,*) SpecialBVOP(IPLV2),   &
                                  SpecialDT (IPLV2,1),   &
                                  SpecialDT (IPLV2,2),   &
                                  SpecialINFCP  (IPLV2,1),   &
                                  SpecialINFCP  (IPLV2,2),   &
                                  SpecialINFSTS (IPLV2,1),   &
                                  SpecialINFSTS (IPLV2,2),   &
                                  SpecialNTRAIN (IPLV2),     &
                                  SpecialNTRRST (IPLV2),     &
                                  SpecialINFCP(IPLV2,1),    &
                                  SpecialINFCP(IPLV2,2)
!!! Dec 2010
            SpecialBVOP0 (IPLV2)  = SpecialBVOP(IPLV2)
            SpecialNTRRS0 (IPLV2) = SpecialNTRRST (IPLV2)
            SpecialVOLOP0 (IPLV2) = SpecialVOLOP0 (IPLV2) + SpecialBVOP(IPLV2)
            SpecialVOLDY0 (IPLV2) = SpecialVOLDY0 (IPLV2) + SpecialNTRRST(IPLV2)
          ENDDO
          Do iplv =1,Ncpluv
             Do j=1,NrSpecialNwrwAreas(iplv)
                Iplv2 = SpecialInDikP(Iplv,j)
                Iplv3 = Reference2SpecialDef(Iplv,j)
                if (SpecialNwrwAreaCompOption(iplv3) .eq. 1) then
                  InputInitialPercentage = SpecialNtrRst(iplv2)*1000 * 100 / SpecialNwrwAreaSoilThickness(iplv3)
                  SpecialNwrwAreaThetaInit(iplv2) = InputInitialPercentage * SpecialNwrwAreaSoilThickness(iplv3) / 100.
                  SpecialNwrwAreaThetaFinal(iplv2) = SpecialNwrwAreaThetaInit(iplv2)
                  if (idebug .ne. 0) write (idebug,*) ' green roof ', iplv2, SpecialNwrwAreaThetaInit(iplv2)
                endif
            enddo
          enddo
        Endif

! EXTR, HBV, SCS       = Jira 20559,      added in version 3.212.06
        If (NcRRRunoff .gt. 0 .and. &
              (Version .eq. ' RestartFile RR version 3.212.06' .or. &
                Version .eq. ' RestartFile RR version 3.212.28' .or. &
                 Version .eq. ' RestartFile RR version 3.212.31' .or. &
                  Version .eq. ' RestartFile RR version 3.212.46' .or. &
                   Version .eq. ' RestartFile RR version 3.212.52' .or. &
                    Version .eq. ' RestartFile RR version 3.213.10' .or. &
                     Version .eq. ' RestartFile RR version 3.213.18' .or. &
                      Version .eq. ' RestartFile RR version 3.213.22' .or. &
                       Version .eq. ' RestartFile RR version 3.213.29' .or. &
                        Version .eq. ' RestartFile RR version 3.213.33' .or. &
                         Version .eq. ' RestartFile RR version 3.214.27' .or. &
                          Version .eq. ' RestartFile RR version 3.216.21' .or. &
                           Version .eq. ' RestartFile RR version 3.216.29' .or. &
                            Version .eq. ' RestartFile RR version 3.216.33')) then
             Do i=1,NCRRRunoff
!               read (InRest) RRRunoff_CompOption(i)
                read (InRest) idum
                if (idebug .ne. 0) Write(Idebug,*) 'Runoff computation option ', idum
                IRRRunoffSub = RRRunoff_SubIndex(i)
                RRRunoff_CompOption(i) = idum
                if (RRRunoff_CompOption(i) .eq. 0) then
                   ! External Runoff node: no additional info
                   if (idebug .ne. 0) Write(Idebug,*) 'EXT'
                elseif (RRRunoff_CompOption(i) .eq. 1) then
                   ! HBV node
                   if (idebug .ne. 0) Write(Idebug,*) 'HBV'
!                  Read (InRest) HBV_DrySnowContent(i), HBV_FreeWaterContent(i), HBV_SoilMoisture(i), HBV_UpperZoneContent(i), HBV_LowerZoneContent(i)
                   Read (InRest) (rdumm(idum1),idum1=1,5)
!                  Read (InRest) rdummy(1)
!                  Read (InRest) rdummy(2)
!                  Read (InRest) rdummy(3)
!                  Read (InRest) rdummy(4)
!                  Read (InRest) rdummy(5)
                   if (idebug .ne. 0) Write(Idebug,*) RDumm(1)
                   if (idebug .ne. 0) Write(Idebug,*) RDumm(2)
                   if (idebug .ne. 0) Write(Idebug,*) RDumm(3)
                   if (idebug .ne. 0) Write(Idebug,*) RDumm(4)
                   if (idebug .ne. 0) Write(Idebug,*) RDumm(5)
                   HBV_DrySnowContent(iRRRunoffSub)   = Rdumm(1)
                   HBV_FreeWaterContent(iRRRunoffSub) = Rdumm(2)
                   HBV_SoilMoisture(iRRRunoffSub)     = Rdumm(3)
                   HBV_UpperZoneContent(iRRRunoffSub) = Rdumm(4)
                   HBV_LowerZoneContent(iRRRunoffSub) = Rdumm(5)
                   HBV_DrySnowContent0(iRRRunoffSub)   = HBV_DrySnowContent(iRRRunoffSub)
                   HBV_FreeWaterContent0(iRRRunoffSub) = HBV_FreeWaterContent(iRRRunoffSub)
                   HBV_SoilMoisture0(iRRRunoffSub)     = HBV_SoilMoisture(iRRRunoffSub)
                   HBV_UpperZoneContent0(iRRRunoffSub) = HBV_UpperZoneContent(iRRRunoffSub)
                   HBV_LowerZoneContent0(iRRRunoffSub) = HBV_LowerZoneContent(iRRRunoffSub)
                   HBV_InitialDrySnowContent(iRRRunoffSub)   = HBV_DrySnowContent(iRRRunoffSub)
                   HBV_InitialFreeWaterContent(iRRRunoffSub) = HBV_FreeWaterContent(iRRRunoffSub)
                   HBV_InitialMoisture(iRRRunoffSub)         = HBV_SoilMoisture(iRRRunoffSub)
                   HBV_InitialUpperZoneContent(iRRRunoffSub) = HBV_UpperZoneContent(iRRRunoffSub)
                   HBV_InitialLowerZoneContent(iRRRunoffSub) = HBV_LowerZoneContent(iRRRunoffSub)
                elseif (RRRunoff_CompOption(i) .eq. 2) then
                   ! SCS node
                   if (idebug .ne. 0) Write(Idebug,*) 'SCS'
                   Read (InRest) SCS_PAccum(iRRRunoffSub), SCS_PExcess(iRRRunoffSub), SCS_Storage(iRRRunoffSub), SCS_UseBaseflow(iRRRunoffSub)
                   Read (InRest) (SCS_AvailableRunoff(iRRRunoffSub,j),j=1,MaxTc)
                   if (SCS_UseBaseFlow(iRRRunoffSub)) Read (InRest) SCS_SurfAct(iRRRunoffSub), SCS_SubSurfAct(iRRRunoffSub), SCS_GWAct(iRRRunoffSub)
                   SCS_PAccum0(iRRRunoffSub)  = SCS_PAccum(iRRRunoffSub)
                   SCS_PExcess0(iRRRunoffSub) = SCS_PExcess(iRRRunoffSub)
                   SCS_Storage0(iRRRunoffSub) = SCS_Storage(iRRRunoffSub)
                   if (SCS_UseBaseflow(iRRRunoffSub)) then
                      SCS_SurfAct0(iRRRunoffSub) = SCS_SurfAct(iRRRunoffSub)
                      SCS_SubSurfAct0(iRRRunoffSub) = SCS_SubSurfAct(iRRRunoffSub)
                      SCS_GWAct0(iRRRunoffSub) = SCS_GWAct(iRRRunoffSub)
                   endif
                elseif (RRRunoff_CompOption(i) .eq. 3) then
                   ! NAM node
                   if (idebug .ne. 0) Write(Idebug,*) 'NAM'
                   if (Version .eq. ' RestartFile RR version 3.214.27' .or. &
                        Version .eq. ' RestartFile RR version 3.216.21' .or. &
                         Version .eq. ' RestartFile RR version 3.216.29' .or. &
                          Version .eq. ' RestartFile RR version 3.216.33') then
                      Read (InRest) NAM_U(iRRRunoffSub), NAM_L(iRRRunoffSub), NAM_GWD0(iRRRunoffSub)
                      if (idebug .ne. 0) Write(IDebug,*) ' NAM initial U, L GWD0 from restart'
                      if (idebug .ne. 0) Write(IDebug,*) NAM_U(iRRRunoffSub), NAM_L(iRRRunoffSub), NAM_GWD0(iRRRunoffSub)
                      NAM_U0  (iRRRunoffSub) = Nam_U(iRRRunoffSub)
                      NAM_L0  (iRRRunoffSub) = NAM_L(iRRRunoffSub) ! added 29 June 2017
                      NAM_LInitial  (iRRRunoffSub) = NAM_L(iRRRunoffSub)
                      NAM_InitialGWDepth (iRRRunoffSub) = NAM_GWD0(iRRRunoffSub)
                      NAM_GWSDInitial (iRRRunoffSub) = NAM_GWD0(iRRRunoffSub)
                      NAM_GWSD (iRRRunoffSub) = NAM_GWD0(iRRRunoffSub)
! add checks on validity
                      Do j=1,ncnode
                         if (Einode(j,2) .eq. i .and. Einode(j,3) .eq. 31) Inode = j        ! kind 31= NAM
                      Enddo
! NAM_LMAX SOBEK-50506
                      idum = D_Ifreal (NAM_LInitial(IRRRunoffSub), NAM_LMAX(IRRRunoffSub),1D-6)
                      if (idum .gt. 0) then
!                        NAM_LInitial(IRRRunoffSub) .gt. NAM_LMAX(IRRRunoffSub)
                         write(Iout,'(A,A)') ' Fatal error regarding the D-NAM model input for node:',Id_Nod(inode)(1:len_trim(Id_Nod(inode)))
                         write(Iout,'(A)')   ' Specified lower zone storage depth in the restart file exceeds maximum lower zone storage depth'
                         write(Iout,'(A,F6.1,A)')  ' Lower zone storage depth from restart file ', NAM_LInitial(IRRRunoffSub), ' mm'
                         write(Iout,'(A,F6.1,A)')  ' Lower zone storage maximum depth derived from input ', NAM_LMax(IRRRunoffSub), ' mm'
                         write(Iout,'(A,F5.3,A)')  ' root depth (SL-RZBL) ', NAM_RD(IRRRunoffSub), ' m'
                         write(Iout,'(A)')  ' Some input has changed after generating the restart file (e.g. selected soil type, GWTD_fc, Surface level, root depth) '
                         write(Iout,'(A)')  ' Adjust this or generate the restart file again'
                         call ErrMsgStandard(972, 0,'  Rdrest',' Restart file inconsistent with present D-NAM input')
                      else
                         ! to prevent rounding off errors
                         NAM_LInitial(IRRRunoffSub) = min (NAM_LMAX(IRRRUnoffSub), NAM_LInitial(IRRRunoffSub))
                      endif
! NAM_GWSDMAX SOBEK-50507
                      idum = D_Ifreal (NAM_GWSDInitial(IRRRunoffSub), NAM_GWSDMAX(IRRRunoffSub),1D-6)
                      if (idum .gt. 0) then
                         ! NAM_GWSDInitial(IRRRunoffSub) .gt. NAM_GWSDMax(IRRRunoffSub)
                         write(Iout,'(A,A)') ' Fatal error regarding the D-NAM model input for node:',Id_Nod(inode)(1:len_trim(Id_Nod(inode)))
                         write(Iout,'(A)')   ' Specified groundwater storage depth in the restart file exceeds maximum groundwater storage depth'
                         write(Iout,'(A,F6.1,A)')  ' Groundwater storage depth from restart file ', NAM_GWSDInitial(IRRRunoffSub), ' mm'
                         write(Iout,'(A,F6.1,A)')  ' Groundwater storage maximum depth from input ', NAM_GWSDMax(IRRRUnoffSub), ' mm; based on:'
                         write(Iout,'(A,F6.3,A)')  ' surface level (SL)) ', NAM_SurfaceLevel(IRRRunoffSub), ' m AD'
                         write(Iout,'(A,F7.2,A)')  ' root zone bed level (RZBL)', NAM_RZBL(IRRRunoffSub), ' m AD'
                         write(Iout,'(A,F7.2,A)')  ' groundwater bed level (GWSBL) ',NAM_GWSBL(IRRRunoffSub), ' m AD'
                         write(Iout,'(A,A)')  ' Some input has changed after generating the restart file (e.g. selected soil type, GWTD_fc, surface level, root zone or groundwater bed level) '
                         write(Iout,'(A)')  ' Adjust this or generate the restart file again'
                         call ErrMsgStandard(972, 0,'  Rdrest',' Restart file inconsistent with present D-NAM input')
                      else
                         ! to prevent rounding off errors
                         NAM_GWSDInitial(IRRRunoffSub) = min (NAM_GWSDMAX(IRRRUnoffSub), NAM_GWSDInitial(IRRRunoffSub))
                      endif
                   else
                      write(*,*) ' Using D-NAM requires a new restart file; regenerate your restart file'
                      call ErrMsgStandard(972, 0,'  Rdrest',' NEW D-NAM version requires new restart file')
                   endif
                elseif (RRRunoff_CompOption(i) .eq. 4) then  ! LGSI
                   Read (InRest) (LGSI_NewGwl(iRRRunoffSub,j),j=1,LGSI_NrSubAreas(iRRRunoffSub))
                   Read (InRest) (LGSI_HistoryQtot(iRRRunoffSub,j),j=1,LGSI_MaxDelayLengthPlus1)
                elseif (RRRunoff_CompOption(i) .eq. 5) then  ! Wageningen model
                   Read(InRest) WagMod_SM0(iRRRunoffSub), WagMod_GStoreT1(iRRRunoffSub), WagMod_QSNEW(iRRRunoffSub)
                   Read(InRest) WagMod_QGT1(iRRRunoffSub)
                   Read(InRest) idum
                   Read(InRest) (WagMod_QG(iRRRunoffSub,j),j=1,idum)
                   Read(InRest) (WagMod_QD(iRRRunoffSub,j),j=1,idum)
                elseif (RRRunoff_CompOption(i) .eq. 6) then  ! Walrus model
                   if (Version .eq. ' RestartFile RR version 3.216.21' .or. &
                        Version .eq. ' RestartFile RR version 3.216.29' .or. &
                         Version .eq. ' RestartFile RR version 3.216.33') then
                      Read(InRest) Walrus_HS0 (iRRRunoffSub)
                      Read(InRest) Walrus_HQ0 (iRRRunoffSub)
                      Read(InRest) Walrus_DG0 (iRRRunoffSub)
                      Read(InRest) Walrus_DV0 (iRRRunoffSub)
                      Read(InRest) Walrus_Q0  (iRRRunoffSub)
                      if (Version .eq. ' RestartFile RR version 3.216.33') then
                         Read(InRest) Walrus_dVEQCurrent (iRRRunoffSub)
                         Read(InRest) Walrus_lastFGS (iRRRunoffSub)
                         Read(InRest) Walrus_lastFQS (iRRRunoffSub)
                         Read(InRest) Walrus_WICurrent (iRRRunoffSub)
                         Read(InRest) Walrus_BetaCurrent (iRRRunoffSub)
                      endif
                   endif
                elseif (RRRunoff_CompOption(i) .gt. 6) then
                  call ErrMsgStandard(981, 0,'  Rdrest',' Error in RR-Runoff computation option - unknown')
                endif
             Enddo
        endif

        if (NcRRRunoffLGSI .gt. 0) LGSI_InitGwl = LGSI_NewGwl

        if (NcRRRunoffWagmod .gt. 0) then

        endif
        if (NcRRRunoffWalrus .gt. 0) then

        endif

! NWRW Greenroof and Wadi data,      added in version 3.212.28
        If (NcPluv .gt. 0 .and. &
             (Version .eq. ' RestartFile RR version 3.212.28' .or. &
               Version .eq. ' RestartFile RR version 3.212.31' .or. &
                Version .eq. ' RestartFile RR version 3.212.46' .or. &
                 Version .eq. ' RestartFile RR version 3.212.52' .or. &
                  Version .eq. ' RestartFile RR version 3.213.10' .or. &
                   Version .eq. ' RestartFile RR version 3.213.18' .or. &
                    Version .eq. ' RestartFile RR version 3.213.22' .or. &
                     Version .eq. ' RestartFile RR version 3.213.29' .or. &
                      Version .eq. ' RestartFile RR version 3.213.33' .or. &
                       Version .eq. ' RestartFile RR version 3.214.27' .or. &
                        Version .eq. ' RestartFile RR version 3.216.21' .or. &
                         Version .eq. ' RestartFile RR version 3.216.29' .or. &
                          Version .eq. ' RestartFile RR version 3.216.33')) then
!green roofs NWRW


!Wadi infiltration NWRW
          Do i=1,NCPluv
            Read(InRest) WadiFinalLevel(i), WadiFinalStorage(i)
            WadiInitialLevel(i)   = WadiFinalLevel(i)
            WadiInitialStorage(i) = WadiFinalStorage(i)
          Enddo

        endif

        If (NcOwRain .gt. 0 .and. &
             (Version .eq. ' RestartFile RR version 3.212.46' .or. &
               Version .eq. ' RestartFile RR version 3.212.52' .or. &
                Version .eq. ' RestartFile RR version 3.213.10' .or. &
                 Version .eq. ' RestartFile RR version 3.213.18' .or. &
                  Version .eq. ' RestartFile RR version 3.213.22' .or. &
                   Version .eq. ' RestartFile RR version 3.213.29' .or. &
                    Version .eq. ' RestartFile RR version 3.213.33' .or. &
                      Version .eq. ' RestartFile RR version 3.214.27' .or. &
                       Version .eq. ' RestartFile RR version 3.216.21' .or. &
                        Version .eq. ' RestartFile RR version 3.216.29' .or. &
                         Version .eq. ' RestartFile RR version 3.216.33')) then
            Do i=1,NCOwRain
               Read(InRest) OwRainArea(i)
            Enddo
        endif

        Goto 9991

  999 CONTINUE
! juli 2001: ARS 7962: if read error, try to read as ASCII file
! unformatted restart files created in 2.05 with MS Powerstation will cause read error in 2.07 Digital version
! with conversion tool ConvRestart205 the original restart file can be converted to ASCII
! 207 will default first try to read the file unformatted, if that files is will try to read it as ASCII
      write(*,*) ' Error reading Restart file RR'
      write(iout,*) ' Error reading Restart file RR: premature end of restart file'
      write(iout,*) '  First generate a new restart file'
      Call CloseGP (Inrest)
      call ErrMsgStandard(997, 0,'  Rdrest',' Restart file premature end')
!!   switched off Oct 2012
!!    if (idebug .ne. 0) Write(Idebug,*) ' Try RdRestAscii on file: ', ConfFil_get_NAMFIL(41)
!!    CALL OPENFL(InRest, ConfFil_get_NAMFIL(41),1,1)
!!    Call RdRestAscii(Inrest)
!     call ErrMsgStandard (911, 10,'  Rdrest',' Restart file')
 9991 CONTINUE

      if (islcmp .ne. 0) then
         Do inod =1, ncnode
            ikind = einode(inod,3)
            inr   = einode(inod,2)
            islt  = indslt(inod)
            if (ikind .eq. 1) then
                TVOL = BVRL(INR,1) + BVRL(INR,2) + BVSTR(INR)
                If (TVOL .GT. 0.0) Then
                   SltIni(inod) = ( SALTF(ISLT)*BVSTR(INR) + SALTF(ISLT+1)*BVRL(INR,1) &
                                                        + SALTF(ISLT+2)*BVRL(INR,2)) / TVOL
                Else
                   SltIni(inod) = 0.0
                Endif
            elseif (ikind .eq. 2) then
                 ROnvZone = MAX (0.001 * AREAOH(INR), OnvZone(INR)%Actual_Volume)
                 TVOL = ROnvZone + BOLND(INR) + BOBD(INR)
                 If (TVOL .GT. 0.0) Then
                    SltIni(inod) =  ( SALTF(ISLT) * BOLND(INR) +  &
                             SALTF(ISLT+1) * ROnvZone + SALTF(ISLT+2) * BOBD(INR) ) / TVOL
                 Else
                    SltIni(inod) = 0.0
                 Endif
            elseif (ikind .eq. 3) then
               BTOT = 0.
               DO IKKL=1,NCKKL
                  BTOT = BTOT + BKAS(INR,IKKL)
               ENDDO
               BTOT = BTOT + SILOB(INR)
               If (BTOT .GT. 0.0) Then
                  TSLT = 0.0
                  TSLT = TSLT + SALTF(ISLT) * BKASD(INR)
                  DO IKKL=1,NCKKL
                    TSLT = TSLT + SALTF(ISLT+IKKL) * BKAS(INR,IKKL)
                  ENDDO
                  TSLT = TSLT + SALTF(ISLT+NCKKL+1) * SILOB(INR)
                  SltIni(inod) = Tslt / (Btot + BkasD(inr))
               Else
                  SltIni(inod) = 0.0
               Endif
            elseif (ikind .eq. 4) then
                 SltIni(inod) = SaltF(islt)
            elseif (ikind .eq. 5) then
                 SltIni(inod) = 0.0
            elseif (ikind .eq. 6) then
                 SltIni(inod) = Cbnd(inr)
            elseif (ikind .eq. 14) then
                 SltIni(inod) = SaltF(islt)
            elseif (ikind .eq. 15) then
                 if (QindDem(inr) .gt. 0) SltIni(inod) = SaltF(islt)
                 if (QindDis(inr) .gt. 0) SltIni(inod) = SltIndDis(inr)
            endif
         Enddo
      EndIf

      If (Version .eq. ' RestartFile RR version 3.213.10' .or. &
           Version .eq. ' RestartFile RR version 3.213.18' .or. &
            Version .eq. ' RestartFile RR version 3.213.22' .or. &
             Version .eq. ' RestartFile RR version 3.213.29' .or. &
              Version .eq. ' RestartFile RR version 3.213.33' .or. &
               Version .eq. ' RestartFile RR version 3.214.27' .or. &
                Version .eq. ' RestartFile RR version 3.216.21' .or. &
                 Version .eq. ' RestartFile RR version 3.216.29' .or. &
                  Version .eq. ' RestartFile RR version 3.216.33') then
!        Link flows
         Do i=1,NcLink
            Read(InRest) (QLink(i,j),j=1,5)
            Do j=1,5
               RSLMAP16_flows(j,i,1) = QLink(i,j)
            Enddo
         Enddo
!        Other node fluxes
         Do i=1,Ncvhg
            Read (InRest) (VHG_Tnul(j,i),j=1,MaxSeriesPerMap(1))
         Enddo
         Do i=1,Ncovhg
            Read (InRest) (OVH_Tnul(j,i),j=1,MaxSeriesPerMap(2))
         Enddo
         Do i=1,NcKas
            Read (InRest) (Kas_Tnul(j,i),j=1,MaxSeriesPerMap(3))
         Enddo
         Do i=1,NcOw
            Read (InRest) (OW_Tnul(j,i),j=1,MaxSeriesPerMap(4))
         Enddo
         Do i=1,NcStru
            Read (InRest) (Str_Tnul(j,i),j=1,MaxSeriesPerMap(5))
         Enddo
         Do i=1,NcBoun
            Read (InRest) (Bnd_Tnul(j,i),j=1,MaxSeriesPerMap(6))
         Enddo
         Do i=1,NcPluv
            Read (InRest) (Plv_Tnul(j,i),j=1,MaxSeriesPerMap(7))
         Enddo
!   8 = balansuitvoer, skip (initieel per definitie nul)
         IF (ISLCMP .NE. 0) THEN
             Do i=1,NcSalt
                Read (InRest) (Slt_Tnul(j,i),j=1,MaxSeriesPerMap(9))
             Enddo
         ELSE
              Do i=1,NcSalt ! still read as much, but don't use i
                Read (InRest) (Slt_Tnul(j,1),j=1,MaxSeriesPerMap(9))
             Enddo
         ENDIF
         Do i=1,NcRwzi
            Read (InRest) (RWZI_Tnul(j,i),j=1,MaxSeriesPerMap(10))
         Enddo
         Do i=1,NcIndus
            Read (InRest) (Ind_Tnul(j,i),j=1,MaxSeriesPerMap(11))
         Enddo
         Do i=1,NcSacr
            if (Version .eq. ' RestartFile RR version 3.216.29' .or. &
                 Version .eq. ' RestartFile RR version 3.216.33') then
                Read (InRest) (Sacr_Tnul(j,i),j=1,MaxSeriesPerMap(12))
            else
                Read (InRest) (Sacr_Tnul(j,i),j=1,15)
            endif
         Enddo
!        13 = flows, 14=cel
         Do i=1,NcRRRunoff
            if (Version .eq. ' RestartFile RR version 3.214.27' .or. &
                 Version .eq. ' RestartFile RR version 3.216.21' .or. &
                  Version .eq. ' RestartFile RR version 3.216.29' .or. &
                   Version .eq. ' RestartFile RR version 3.216.33') then
               Read (InRest) (RRRunoff_Tnul(j,i),j=1,MaxSeriesPerMap(15))
            else
               write(iout,'(A)') ' Because of changed dimensions and output variables, RRRunoff_output at Tnul not read from restart file'
               Write(*,*) ' Because of changed dimensions and output variables, RRRunoff_output at Tnul not read from restart'
            endif
         Enddo
      Endif

      RETURN
      END Subroutine RdRest

!   SUBROUTINE WrRestAscii (INREST)
!     niet meer actief; zie VSS voor oude code

end module Restart
