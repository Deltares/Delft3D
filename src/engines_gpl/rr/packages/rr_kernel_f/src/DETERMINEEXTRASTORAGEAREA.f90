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

    Subroutine DetermineExtraStorageArea (iOut1,Idebug)
    ! *********************************************************************
    ! *** Last update: July 1998                       By : Geert Prinsen
    ! *********************************************************************
    ! *** Brief description:
    ! *** ------------------
    ! ***   Bepalen van array met bergend oppervlak verhard/onverhard horend
    ! ***   bij open water knoop
    ! ***   Bepalen van array met oppervlak voor neerslag op open water
    ! ***   RainArea = areaal bij minimum aangelegen maaiveld indien gedefinieerd,
    ! ***              anders = areaal bij maximum toelaatbaar peil
    ! *********************************************************************
    ! *** Input parameters:
    ! ***   - relatie knopen (array EIOW per knoop geeft intern nr gerelateerd open water)
    ! ***   - maaiveld en/of Scurve data
    ! ***   - areaal
    ! *** Output:   gevuld array ExtraBerging
    ! ***           gevuld array RainArea
    ! *********************************************************************

!   Use Conf_fil
    Use Conf_arr
    Use Paved
    Use Unpaved
    Use OpenWater

    implicit none

    Integer iOut1, Idebug, teller, teller2, iow, iow2, iovh, ivhg, inode, ikind, inod, i
    Integer Irefow, IrefBerg, ipoint, idum, IndexNr, IndexNr0
    Real    Rhelp, Rhelp0, DeltaOwVolume, DeltaBergVolume
    Real TempLevels(NStorageCurvePoints), TempAreas(NStorageCurvePoints), TmpLvl
    Character(Len=CharIdLength)   TempId
    Real                          PeilArray(nVal), AreaArray(nVal), VolumeArray(nVal)
    Real                          ExtraPeilArray(nStorageCurvePoints), ExtraVolumeArray(nStorageCurvePoints)

! Vector/Array initialisations
    NrBergendOppervlakIndices = 0
    OpenWaterRelatedNodes     = 0
! Bepaal de knopen die gerelateerd zijn aan open water knoop IOW
    DO IOW =1,NCOW
!      NrBergendOppervlakIndices(iow) = 0
!      DO teller=1,NRelatedNodes
!         OpenWaterRelatedNodes(Iow,teller) = 0
!      ENDDO
       teller  = 0
       DO INODE=1,NCNODE
          IOW2  = EIOW(INODE)
          IKIND = EiNode(INODE,3)
          IF (IOW .eq. IOW2 .AND. IKIND .LE. 3) then  ! alleen verhard, onverhard, kas;
                                                      ! geen RWZI, industry, NWRW, kunstwerken, openwater
             teller = teller +1
             OpenWaterRelatedNodes(Iow,teller) = Inode
          ENDIF
       ENDDO
       if (idebug .ne. 0) then
         write(idebug,*) ' DetermineExtraStorageAreaOpenWater',IOW
         write(idebug,*) ' Open water',IOW
         write(idebug,*) ' Related nodes',(OpenwaterRelatednodes(iow,teller),teller=1,10)
       endif
    ENDDO

! Bepaal de peilen en oppervlakken
    DO IOW =1,NCOW
!      DO teller2=1,NStorageCurvePoints
!         TempLevels(teller2) = 0.0
!         TempAreas (teller2) = 0.0
!      Enddo
!      Vector/Array initialisations
       TempLevels = 0.0
       TempAreas  = 0.0
       teller2 = 0
! Vul het array met peilen en oppervlakken
       DO teller=1,NRelatedNodes
          Inode = OpenWaterRelatedNodes(Iow,Teller)
          If (Inode .le. 0) goto 998
          IF (EiNode(INODE,3) .EQ. 1) then
             teller2 = teller2 +1
             IVHG = EiNode(INODE,2)
             TempLevels(teller2) = LvlVh(ivhg)
             TempAreas (teller2) = AreaVh(ivhg)
          ElseIF (EiNode(INODE,3) .EQ. 2) then
             IOVH = EiNode(INODE,2)
             if (UseScurve(iovh) .eq. 0) then    ! geen S curve
                teller2 = teller2 +1
                TempLevels(teller2) = LvlOh(iovh)
                TempAreas (teller2) = AreaOh(iovh)
             elseif (UseScurve(iovh) .eq. 1) then  ! wel S curve
                DO ipoint =1,UseUnpavedScurve
                   teller2=teller2+1
                   if (teller2 .gt. NStorageCurvePoints)  Stop 'NStorageCurvePointsDimension'
                   TempLevels(teller2) = AreaScurve(Iovh,ipoint)%Level
                   TempAreas (teller2) = AreaScurve(Iovh,ipoint)%Area
                Enddo
             endif
          endif
       ENDDO
 998   CONTINUE
! Voeg extra index toe aan array TempLevels en TempAreas
       if (teller2 .gt. 0) then
         NrBergendOppervlakIndices(iow) = teller2+1
         TempLevels(teller2+1) = TempLevels(teller2) + 999.5 ! was +0.5; veranderd ivm verzoek ARS 8584/8603
         TempAreas (teller2+1) = 0.0
       endif

       idum = min (210, NrBergendOppervlakIndices(iow))
       if (idebug .ne. 0) then
         write(idebug,*) ' Open water',IOW
         write(idebug,*) ' Relevant nr',NrBergendOppervlakIndices(iow)
         write(idebug,*) ' nStorageCurvePoints', NStorageCurvePoints
         write(idebug,*) ' Levels',(TempLevels(teller2),teller2=1,idum)
         write(idebug,*) ' Areas', (TempAreas(teller2) ,teller2=1,idum)
       endif

! Sorteer het array en bewaar het; bepaal volumes
       Call BubbleSort (NStorageCurvePoints, TempLevels, TempAreas, NrBergendOppervlakIndices(iow), .true.)
       DO teller2=1,NStorageCurvePoints
          ExtraBergendOppPeil      (teller2,Iow) = TempLevels(teller2)
          ExtraBergendOppOppervlak (teller2,Iow) = TempAreas (teller2)
          ExtraBergendOppVolume    (teller2,Iow) = 0.
          if (teller2 .gt. 1) then
            if (ExtraBergendOppPeil(teller2,Iow).le. ExtraBergendOppPeil(teller2-1,Iow)) then
                ExtraBergendOppPeil(teller2,Iow) = ExtraBergendOppPeil(teller2-1,Iow) + 0.00001
            endif
            ExtraBergendOppVolume(teller2,iow) = &
              ExtraBergendOppVolume(teller2-1,iow) + &
                (ExtraBergendOppPeil(teller2,iow)- ExtraBergendOppPeil(teller2-1,iow)) * ExtraBergendOppOppervlak (teller2-1,iow)
            ExtraBergendOppOppervlak (teller2,Iow) = ExtraBergendOppOppervlak (teller2-1,Iow) + TempAreas (teller2)
          endif
       Enddo
! zorg voor monotoon array met levels
       DO teller2=2,NStorageCurvePoints
          if (ExtraBergendOppPeil(teller2,Iow).le. ExtraBergendOppPeil(teller2-1,Iow)) then
              ExtraBergendOppPeil(teller2,Iow) = ExtraBergendOppPeil(teller2-1,Iow) + 0.00001
          endif
       Enddo
! Bepaal Rainarea
       if (NrBergendOppervlakIndices(iow) .gt. 0) then
          TmpLvl = ExtraBergendOppPeil(1,iow)
       else
          TmpLvl = MaxLvl(iow)
       Endif
       Do i=1,NVal
          PeilArray (i) = PeilOw(i,iow)
          AreaArray (i) = AreaOw(i,iow)
       Enddo
       CALL RR_INTERP (NVal, PEILArray, AreaArray, TmpLvl, RainArea(iow), Idum)
! Debug
       idum = min (210, NrBergendOppervlakIndices(iow))
       if (idebug .ne. 0) then
         write(idebug,*) ' Sorted Levels',(ExtraBergendOppPeil(teller2,iow),teller2=1,idum)
         write(idebug,*) ' Areas', (ExtraBergendOppOppervlak(teller2,iow) ,teller2=1,idum)
         write(idebug,*) ' Volumes', (ExtraBergendOppVolume(teller2,iow) ,teller2=1,idum)
         write(idebug,*) ' RainArea', RainArea(iow)
       endif
     ENDDO

! ARS 6345
!      Vector/Array initialisations
       TotalVolume = 0.0
       PreviousVolume = 0.0
       TotalOwBergPeil = -999.0
       TotalOwBergVolume = -999.0

! Bepaal relatie Total peil, volume voor ow + bergend oppervlak
    DO IOW =1,NCOW
!      TotalVolume(iow) = 0.0
!      PreviousVolume(iow) = 0.0
!      DO teller2=1,NStorageCurvePoints
!         TotalOwBergPeil(teller2,iow) = -999.0
!         TotalOwBergVolume(teller2,iow) = -999.0
!      Enddo
       irefow = 1
       irefberg = 1
! neem aan: peilow begint altijd lager, anders problematisch
       if (Peilow(irefow,iow) .gt. ExtraBergendOppPeil(Irefberg,iow) .and. NrBergendOppervlakIndices(iow) .gt. 0) then
          Do inod=1,ncnode
             If (EiNode(inod,3) .eq. 4 .and. EiNode(inod,2) .eq. iow) then
                TempId = Id_Nod(inod)
             Endif
          Enddo
!         write(iout1,'(A,A,A)') &
!             ' Warning in DetermineExtraStorageArea:', &
!             ' Levels in Level-area relation of Open water higherthan surface level connected nodes for open water node', TempId

          TotalOwBergPeil  (1,iow) = BottomLevel(iow)
          TotalOwBergVolume(1,iow) = 0.0
          if (BottomLevel(iow) .gt. ExtraBergendOppPeil(Irefberg,iow)) then
            call ErrMsgStandard (972,0, &
                          ' Error in DetermineExtraStorageArea', &
                          ' Bottom Level Open water higher than surface level connected nodes for open water node '// TempId )
          endif
       else
         TotalOwBergPeil  (1,iow) = PeilOw(1,iow)
         TotalOwBergVolume(1,iow) = VoluOw(1,iow)
         Irefow = 2
         irefberg = 1
       Endif
! rest van de curve vullen; Voluow interpoleren, ExtraBergendOppVolume ook?

       if (NrBergendOppervlakIndices(iow) .eq. 0) then
! geen verbonden knoop met bergend oppervlak; alleen open water; nav ARS 7484 gevonden
        Do teller2=2,6
          TotalOwBergPeil(teller2,iow) = Peilow(Irefow,iow)
          irefow = irefow + 1
! Bepaal DeltaOWVolume
          Do i=1,NVal
             PeilArray (i) = PeilOw(i,iow)
             VolumeArray (i) = VoluOw(i,iow)
          Enddo
          CALL RR_INTERP (NVAL, PEILArray, VOLUMEArray, TotalOwBergPeil(teller2,IOW), Rhelp, IDUM)
          CALL RR_INTERP (NVAL, PEILArray, VolumeArray, TotalOwBergPeil(teller2-1,Iow), Rhelp0, IDUM)
          DeltaOwVolume  = Rhelp - Rhelp0
! Zet nieuw totaal volume
          TotalOwBergVolume(teller2,iow) = TotalOwBergVolume(teller2-1,iow) + DeltaOwVolume
        EndDo

       else
! Wel enkele verbonden knopen met bergend oppervlak
        Do teller2=2,NrBergendOppervlakIndices(iow)+6
! Zet nieuw totaal peil
          if (irefow .le. nval) then
             if (Peilow(irefow,iow) .lt. ExtraBergendOppPeil(irefberg,iow)) then
                TotalOwBergPeil(teller2,iow) = Peilow(Irefow,iow)
                irefow = irefow + 1
             elseif (Peilow(irefow,iow) .gt. ExtraBergendOppPeil(irefberg,iow)) then
                TotalOwBergPeil(teller2,iow) = ExtraBergendOppPeil(Irefberg,iow)
                irefberg = irefberg + 1
             elseif (Peilow(irefow,iow) .eq. ExtraBergendOppPeil(irefberg,iow)) then
                TotalOwBergPeil(teller2,iow) = ExtraBergendOppPeil(Irefberg,iow)
                irefow = irefow + 1
                irefberg = irefberg + 1
             endif
          else
             TotalOwBergPeil(teller2,iow) = ExtraBergendOppPeil(Irefberg,iow)
             irefberg = irefberg + 1
          endif
! Bepaal DeltaOWVolume
          Do i=1,NVal
             PeilArray (i) = PeilOw(i,iow)
             VolumeArray (i) = VoluOw(i,iow)
          Enddo
          CALL RR_INTERP (NVAL, PEILArray, VolumeArray, TotalOwBergPeil(teller2,IOW), Rhelp, IDUM)
          CALL RR_INTERP (NVAL, PEILArray, VolumeArray, TotalOwBergPeil(teller2-1,Iow), Rhelp0, IDUM)
          DeltaOwVolume  = Rhelp - Rhelp0
! Bepaal DeltaBergVolume
!         Call D_LOCATE (ExtraBergendOppPeil(1,iow), NrBergendOppervlakIndices(iow),TotalOwBergPeil(teller2,iow),Indexnr)
          Do i=1,NStorageCurvePoints
             ExtraPeilArray (i) = ExtraBergendOppPeil(i,iow)
             ExtraVolumeArray (i) = ExtraBergendOppVolume(i,iow)
          Enddo
          Call LOCATE (ExtraPeilArray, NrBergendOppervlakIndices(iow),TotalOwBergPeil(teller2,iow),Indexnr)
          if (Indexnr .lt. 1) then
             DeltaBergVolume = 0.0
          else
             CALL RR_INTERP (NrBergendOppervlakIndices(iow), &
                          ExtraPeilArray, ExtraVolumeArray, &
                          TotalOwBergPeil(teller2,iow), Rhelp,IDUM)
!            Call D_LOCATE (ExtraBergendOppPeil(1,iow), NrBergendOppervlakIndices(iow),&
!                         TotalOwBergPeil(teller2-1,iow),Indexnr0)
             Call LOCATE (ExtraPeilArray, NrBergendOppervlakIndices(iow),&
                          TotalOwBergPeil(teller2-1,iow),Indexnr0)
             CALL RR_INTERP (NrBergendOppervlakIndices(iow), &
                          ExtraPeilArray, ExtraVolumeArray, &
                          TotalOwBergPeil(teller2-1,iow), Rhelp0,IDUM)
             DeltaBergVolume = Rhelp - Rhelp0
          endif
! Zet nieuw totaal volume
          TotalOwBergVolume(teller2,iow) = TotalOwBergVolume(teller2-1,iow) + DeltaOwVolume + DeltaBergVolume
        Enddo
       Endif

       idum = min (210, NrBergendOppervlakIndices(iow)+6)
!      idebug = 0
!      if (iow .eq. 42) idebug = IdebugLunRR
       if (idebug .ne. 0) then
         write(idebug,*) ' open water iow=',iow
         write(idebug,*) ' Total Levels',(TotalOwBergPeil(teller2,iow),teller2=1,idum)
         write(idebug,*) ' Total Volumes', (TotalOwBergVolume(teller2,iow) ,teller2=1,idum)
       endif
    ENDDO
  Return
  end subroutine DetermineExtraStorageArea
