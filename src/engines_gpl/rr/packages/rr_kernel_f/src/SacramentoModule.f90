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
! by:               $Author:: Prinsen           $
! at:               $Modtime:: April 2001       $
!
! current revision: $Revision:: 1               $



module Sacramento

  use Conf_fil
! use Conf_arr
  use Network
  use Link
  use NWRW
  use RR_Meteo
  use Boundary
  use Openwater
  use RRConnectionBifurcationNodes
  use DH_Alloc
  use ReadLib

  implicit none

  ! variables

  ! *** Data Sacramento knopen

  REAL, Pointer, SAVE ::         AREASA(:), &
                                     UZTWMInit(:), UZFWMInit(:), LZTWMInit(:), LZFSMInit(:), LZFPMInit(:), &
                                     UZTWCInit(:), UZFWCInit(:), LZTWCInit(:), LZFSCInit(:), LZFPCInit(:), &
                                     UZTWM(:), UZFWM(:), LZTWM(:), LZFSM(:), LZFPM(:), &
                                     UZTWC(:), UZFWC(:), LZTWC(:), LZFSC(:), LZFPC(:), &
                                     UZTWC0(:), UZFWC0(:), LZTWC0(:), LZFSC0(:), LZFPC0(:), &
                                     UZK  (:), LZSK (:), LZPK (:), &
                                     UNITHYdComp(:,:), UnitHyDt(:), &
                                     ZPERC(:), REXP (:), PFREE(:), RSERV(:), PCTIM(:), ADIMP(:),&
                                     SARVA(:), SIDE (:), SSOUT(:), PM   (:), PT1  (:), PT2  (:)
  REAL, Pointer, SAVE ::         SID  (:), SAVED(:), PERCM(:), ADIMC(:), ADIMC0(:), STOR (:), SACR_QQ(:,:)
  REAL, Pointer, SAVE ::         SACR_QQ0(:,:)
!GP separation of routed direct/surface/interflow
  REAL, Pointer, SAVE ::         SACR_QD(:,:), SACR_QS(:,:), SACR_QI(:,:)
  REAL, Pointer, SAVE ::         SACR_QD0(:,:), SACR_QS0(:,:), SACR_QI0(:,:)
!end GP
  INTEGER, Pointer, SAVE ::      SACBND(:), SacNam(:), SacrCompOption(:)



  ! *** results Sacramento knopen
  ! ***

  REAL, Pointer, SAVE :: &
               SacPrecip(:), SacPotEvap(:), &
               SacActEvap(:), SacBaseFlow(:), &
               SacInterFlow(:), SacSurfaceFlow(:), &
               SacTotalRunoff(:), SacLossFlow(:), SacChannelInflow(:), SacRunoffImperv(:)
  REAL, Pointer, SAVE :: SacRunoffSurface(:), &
                         SacRoutedDirectFlow(:), SacRoutedSurfaceFlow(:), SacRoutedInterFlow(:), SacPercolation(:)

!Sacramento
   REAL, Pointer, SAVE ::   SacMXUZTWC  (:,:), SacMXUZFWC  (:,:), SacMXLZTWC  (:,:), SacMXLZFSC  (:,:), &
                                SacMXLZFPC  (:,:),  SacMXPrecip (:,:),SacMXPotEvp (:,:), SacMXActEvp (:,:), &
                                SacMXBasFlw (:,:), SacMXTotRun (:,:), SacMXChaInf (:,:), SacMXLosFlw (:,:), &
                                SacMXAdimC (:,:), SacMXSurFlw (:,:), SacMxImpFlw(:,:)

!  ARS 10403
   Logical WriteRestartFileWithAdimC
   Logical ReadAdimCInRestartFile

contains



  Subroutine Sacramento_confAr0

  implicit none

  Return
  End subroutine Sacramento_confAr0



  Subroutine Sacramento_confAr1

    implicit none
    Integer iOut1
    Logical Success

    iOut1 = ConfFil_get_iOut1()

    NSACR = MAX (1, NCSACR) !Sacramento

    If ((NCSacr .GT. 0) .and. (ConfFil_get_iOut1() .gt. 0)) then
      WRITE(IOUT1,*) ' Sacramento Nodes      =',NSACR
    endif

   !*** Input Data Sacramento knopen

    Success = Dh_AllocInit (NSacr, AreaSa, 0E0)
    Success = Success .and. Dh_AllocInit (NSacr, UZTWMInit, UZFWMInit,  0E0)
    Success = Success .and. Dh_AllocInit (NSacr, LZTWMInit, LZFSMInit, LZFPMInit,  0E0)
    Success = Success .and. Dh_AllocInit (NSacr, UZTWCInit, UZFWCInit,  0E0)
    Success = Success .and. Dh_AllocInit (NSacr, LZTWCInit, LZFSCInit, LZFPCInit,  0E0)
    Success = Success .and. Dh_AllocInit (NSacr, UZTWM, UZFWM,  0E0)
    Success = Success .and. Dh_AllocInit (NSacr, LZTWM, LZFSM, LZFPM,  0E0)
    Success = Success .and. Dh_AllocInit (NSacr, UZTWC, UZFWC,  0E0)
    Success = Success .and. Dh_AllocInit (NSacr, LZTWC, LZFSC, LZFPC,  0E0)
    Success = Success .and. Dh_AllocInit (NSacr, UZTWC0, UZFWC0,  0E0)
    Success = Success .and. Dh_AllocInit (NSacr, LZTWC0, LZFSC0, LZFPC0,  0E0)
    Success = Success .and. Dh_AllocInit (NSacr, UZK, LZSK, LZPK, 0E0)
    Success = Success .and. Dh_AllocInit (NSacr, 501, UNITHydComp, 0E0)     ! was 121
    Success = Success .and. Dh_AllocInit (NSacr, UNITHyDt, 0E0)
    Success = Success .and. Dh_AllocInit (NSacr, ZPERC, REXP, PFREE, 0E0)
    Success = Success .and. Dh_AllocInit (NSacr, RSERV, PCTIM, ADIMP, 0E0)
    Success = Success .and. Dh_AllocInit (NSacr, SARVA, SIDE, SSOUT, 0E0)
    Success = Success .and. Dh_AllocInit (NSacr, PT1, PT2, PM, 0E0)
    If (.not. success) call ErrMsgStandard (981, 0, ' Error allocating arrays in subroutine ', ' Sacramento_ConfAr1' )
!   ALLOCATE ( AREASA(NSACR), &
!              UZTWMInit (NSacr), UZFWMInit (NSacr), LZTWMInit (NSacr), LZFSMInit(NSacr), LZFPMInit(NSacr), &
!              UZTWCInit (NSacr), UZFWCInit (NSacr), LZTWCInit (NSacr), LZFSCInit(NSacr), LZFPCInit(NSacr), &
!              UZTWM (NSacr), UZFWM (NSacr), LZTWM (NSacr), LZFSM(NSacr), LZFPM(NSacr), &
!              UZTWC (NSacr), UZFWC (NSacr), LZTWC (NSacr), LZFSC(NSacr), LZFPC(NSacr), &
!              UZTWC0 (NSacr), UZFWC0 (NSacr), LZTWC0 (NSacr), LZFSC0(NSacr), LZFPC0(NSacr), &
!              UZK   (NSacr), LZSK  (NSacr), LZPK  (NSacr), &
!              UNITHYdComp(NSacr,121), UnitHyDt  (NSacr), &
!              ZPERC (NSacr), REXP  (NSacr), PFREE (NSacr), RSERV(NSacr), PCTIM(NSacr), ADIMP(NSacr),&
!              SARVA (NSacr), SIDE  (NSacr), SSOUT (NSacr), &
!              PM   (NSacr), PT1  (NSacr), PT2  (NSacr), Stat=Allocation_Error)
!
    Success = Success .and. Dh_AllocInit (NSacr, SID, SAVED, PERCM, 0E0)
    Success = Success .and. Dh_AllocInit (NSacr, ADIMC, ADIMC0, STOR, 0E0)
    Success = Success .and. Dh_AllocInit (NSacr, 501, Sacr_QQ, Sacr_QQ0, 0E0)    ! was 121
    Success = Success .and. Dh_AllocInit (NSacr, 501, Sacr_QD, Sacr_QD0, 0E0)
    Success = Success .and. Dh_AllocInit (NSacr, 501, Sacr_QS, Sacr_QS0, 0E0)
    Success = Success .and. Dh_AllocInit (NSacr, 501, Sacr_QI, Sacr_QI0, 0E0)
!    ALLOCATE ( SID(NSacr), SAVED(NSacr), PERCM(NSacr), ADIMC(NSacr), ADIMC0(NSacr), STOR(NSACR), &
!               SACR_QQ(NSacr,121), SACR_QQ0(NSacr,121), Stat=Allocation_Error)

    Success = Success .and. Dh_AllocInit (NSacr, SacBnd, 0)
    Success = Success .and. Dh_AllocInit (NSacr, SacNam, 0)
    Success = Success .and. Dh_AllocInit (NSacr, SacrCompOption, 1)
    If (.not. success) call ErrMsgStandard (981, 0, ' Error allocating arrays in subroutine ', ' Sacramento_ConfAr1' )
!   ALLOCATE ( SACBND (NPLV), Stat=Allocation_Error )

  Return
  End subroutine Sacramento_confAr1


  Subroutine SacramentoOutput_Confar (Nevnt)

    implicit none
    Integer Nevnt
    Logical Success

!Sacramento
    Success = Dh_AllocInit (NSacr, Nevnt, SacMxUZTWC, SacMxUZFWC, 0E0)
    Success = Success .and. Dh_AllocInit (NSacr, NEvnt, SacMxLZTWC, SacMxLZFSC, SacMxLZFPC, 0E0)
    Success = Success .and. Dh_AllocInit (NSacr, NEvnt, SacMxPrecip, SacMxPotEvp, SacMxActEvp, 0E0)
    Success = Success .and. Dh_AllocInit (NSacr, NEvnt, SacMxBasFlw, SacMxTotRun, SacMxSurFlw, 0E0)
    Success = Success .and. Dh_AllocInit (NSacr, NEvnt, SacMxChaInf, SacMxImpFlw, SacMxLosFlw, 0E0)
    Success = Success .and. Dh_AllocInit (NSacr, NEvnt, SacMxAdimC, 0E0)
    If (.not. success) call ErrMsgStandard (981, 0, ' Error allocating arrays in subroutine ', ' Sacramento_OutputConfAr' )
!   ALLOCATE (SacMXUZTWC  (NcSacr,Nevnt), SacMXUZFWC  (NcSacr,Nevnt), &
!              SacMXLZTWC  (NcSacr,Nevnt), SacMXLZFSC  (NcSacr,Nevnt), &
!               SacMXLZFPC  (NcSacr,Nevnt),  SacMXPrecip (NcSacr,Nevnt), &
!                SacMXPotEvp (NcSacr,Nevnt), SacMXActEvp (NcSacr,Nevnt), &
!                 SacMXBasFlw (NcSacr,Nevnt), SacMXTotRun (NcSacr,Nevnt), &
!                  SacMXSurFlw (NcSacr,Nevnt), SacMXChaInf (NcSacr,Nevnt), &
!                  SacMXImpFlw (NcSacr,Nevnt), &
!                   SacMXLosFlw (NcSacr,Nevnt), SacMXAdimC (NcSacr,NEvnt), Stat=Allocation_Error)
  Return
  End subroutine SacramentoOutput_Confar


  SUBROUTINE Sacramento_CONFAR3
    ! *** Output Data Sacramento knopen


    implicit none
    Logical Success

    Success = Dh_AllocInit (NSacr, SacPrecip, SacPotEvap, SacActEvap, 0E0)
    Success = Success .and. Dh_AllocInit (NSacr, SacBaseFlow, SacInterFlow, SacSurfaceFlow, 0E0)
    Success = Success .and. Dh_AllocInit (NSacr, SacTotalRunoff, SacLossFlow, SacChannelInflow, 0E0)
    Success = Success .and. Dh_AllocInit (NSacr, SacRunoffImperv, 0E0)
    Success = Success .and. Dh_AllocInit (NSacr, SacRunoffSurface, 0E0)
    Success = Success .and. Dh_AllocInit (NSacr, SacRoutedDirectFlow, 0E0)
    Success = Success .and. Dh_AllocInit (NSacr, SacRoutedSurfaceFlow, 0E0)
    Success = Success .and. Dh_AllocInit (NSacr, SacRoutedInterFlow, 0E0)
    Success = Success .and. Dh_AllocInit (NSacr, SacPercolation, 0E0)
    If (.not. success) call ErrMsgStandard (981, 0, ' Error allocating arrays in subroutine ', ' Sacramento_ConfAr3' )
!     ALLOCATE (SacPrecip(NSacr), SacPotEvap(nSacr), &
!               SacActEvap(nSacr), SacBaseFlow(NSacr), &
!               SacInterFlow(nSacr), SacSurfaceFlow(NSacr), &
!               SacTotalRunoff(NSacr), SacLossFlow(NSacr),SacChannelInflow(NSacr),&
!               SacRunoffImperv(NSacr), Stat=Allocation_Error )
    Return
  End subroutine Sacramento_confar3




  Subroutine Sacramento_readAscii(infile1)

    implicit none

    Integer :: RetVal

    Integer(4)      infile1
    Integer         teller, i, iSacr, index, inod, nhlp, iout1, iecode, idebug
    Character(CharIdLength)   name, id, NodeId
    Character(1000) string
    Logical         allow, found, endfil, occurs, Err969
    Real            uztwmDum, uzfwmdum, uztwcdum, uzfwcdum
    Real            lztwmdum, lztwcdum, lzfsmDum, lzfscdum, lzfpmdum, lzfpcdum
    Real            uzkdum, lzskdum, lzpkdum
    Real            uhdum(36)
    Integer         uhdtdum
    Real            zpercdum, rexpdum, pfreedum, rservdum, pctimdum, adimpdum, &
                    sarvadum, sidedum, ssoutdum, pmdum, pt1dum, pt2dum

    Parameter     (NHLP=36)
    Integer       IDUM(NHLP)
    Real          RDUM(NHLP)
    Character(CharIdLength) CDUM(NHLP)
    Logical, Pointer :: AlreadyRead(:)
    Integer, Pointer :: ReferenceToDefinition(:)

    Character(Len=CharIdLength), pointer, save ::  CAPDEF(:), UNITHYDEF(:), OtherDef(:)
    Logical Success

    Success = Dh_AllocInit (NSacr, CapDef, UnitHyDef, OtherDef, ' ')
    Success = Success .and. Dh_AllocInit (NCSacr, AlreadyRead, .false.)
    Success = Success .and. Dh_AllocInit (NCSacr, ReferenceToDefinition, 0)
    If (.not. success) call ErrMsgStandard (981, 0, ' Error allocating arrays in subroutine ', ' Sacramento_ReadAscii' )
!   ALLOCATE ( CAPDEF(NSACR), UNITHYDEF(NSACR), OtherDef(NCSacr), Stat=Allocation_Error )
!   ALLOCATE   (AlreadyRead(NCSacr), Stat=Allocation_Error )
!   ALLOCATE   (ReferenceToDefinition(NCSacr), Stat=Allocation_Error )

    iDebug = ConfFil_get_iDebug()

!   AlreadyRead = .false.

    allow = .false.
    found = .false.
    iOut1 = ConfFil_get_iOut1()

! Read Sacramento.3B file
    call SetMessage(LEVEL_DEBUG, 'Read Sacramento.3b file')
    endfil = .false.
    teller = 0
    RetVal = 0
    Call SKPCOM (INfile1, ENDFIL,'ODS')
    Do while (.not. endfil)
       READ(Infile1,'(A1000)',END=21,ERR=150,IOSTAT=IECODE)  STRING
! skip regel als hij niet begint met juist keyword (Sacramento)
       IF (STRING(1:4) .EQ. 'SACR') Then
! Sacramento node id
        RetVal = RetVal + GetVAR2 (STRING,' id ',1,' Sacramento-ReadAscii',' Sacramento.3B file',IOUT1, &
                      ID, RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
        index = 0
        call fndNd2(index, id)
        if (index .gt. 0) then
         Inod = index
         isacr = EiNode(inod,2)
         if (EiNode(inod,3) .eq. 16) then   ! en is Sacramento knoop
          if (AlreadyRead(isacr)) then
            call SetMessage(LEVEL_ERROR, 'Data for Sacramento node '//id(1:Len_Trim(id))//' double in datafile Sacramento.3B')
          else
           teller = teller + 1
           AlreadyRead(isacr) = .true.
!          SACNAM(isacr) = index
! optional: computation option
           allow = .true.
           found = .false.
           RetVal = RetVal + GetVAR2(STRING,' co ',3,' Sacramento-ReadAscii',' Sacramento.3B file',IOUT1, &
                        ID, RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
           if (found) SacrCompOption(isacr) = IDUM(1)
           allow = .false.
           found = .false.
! Sacramento area
           RetVal = RetVal + GetVAR2(STRING,' ar ',2,' Sacramento-ReadAscii',' Sacramento.3B file',IOUT1, &
                        ID, RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
           AREASA(isacr) = RDUM(1)
! Sacramento Meteostation definition
           RetVal = RetVal + GetVAR2(STRING,' ms ',1,' Sacramento-ReadAscii',' Sacramento.3B file',IOUT1, &
                        CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, Iflrtn)
           NAMMET(index) = CDUM(1)
! areal adjustment factor rainfall on node, maybe missing. Default value is 1
           allow = .true.
           RetVal = RetVal + GetVAR2(STRING,' aaf ',2,' Sacramento-readAscii',' paved.3b file',IOUT1, &
                            CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, Iflrtn)
           if (found) AAFNodeRainfall(index) = max(0.0, RDUM(1))    ! AAF >= 0
           allow = .false.
! Sacramento Capacities and Contents definition
           RetVal = RetVal + GetVAR2(STRING,' ca ',1,' Sacramento-ReadAscii',' Sacramento.3B file',IOUT1, &
                        CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
           CAPDEF(isacr) = CDUM(1)
!          Call Upperc (CAPDEF(isacr))
! Sacramento Unit Hydrograph definition
           RetVal = RetVal + GetVAR2(STRING,' uh ',1,' Sacramento-ReadAscii',' Sacramento.3B file',IOUT1, &
                        CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
           UNITHYDEF(isacr) = CDUM(1)
!          Call Upperc (UNITHYDEF(isacr))
! Sacramento Other Parameters definition
           RetVal = RetVal + GetVAR2(STRING,' op ',1,' Sacramento-ReadAscii',' Sacramento.3B file',IOUT1, &
                        CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
           OtherDEF(ISacr) = CDUM(1)
!          Call Upperc (OtherDEF(isacr))

! bepaal totaal areaal voor waterbalans
!           SacBal(1) = SacBal(1) + AreaSA(isacr)
          Endif
         Endif
        Endif
      Endif
      Call SKPCOM (INfile1, ENDFIL,'ODS')
    enddo
 21 Continue
    If (RetVal .gt. 0) call ErrMsgStandard (972, 0, ' Error reading Sacramento input', ' Error getting SACR records')
    If (teller .lt. NcSacr)  Then
        Do inod=1,NcNode
          isacr = EiNode(inod,2)
          if (EiNode(inod,3) .eq. 16) then   ! en is Sacramento knoop
            if (.not. AlReadyRead(isacr)) then
              NodeId = ' '
              NodeId = Id_Nod(inod)
              String = ' '
              String = ' ' // NodeId(1:Len_Trim(NodeId)) // ' is missing'
              call ErrMsgStandard (977, 0, ' Data for Sarcamento node ',String(1:Len_Trim(String)) )
            endif
          endif
        Enddo
       call ErrMsgStandard (972, 0, ' Not enough data for all Sacramento-nodes in schematisation found', &
                            ' Some Sacramento-nodes from schematisation not present in Sacramento.3B file')
    Endif


!Sacramento.Cap file
  rewind(infile1)
  call SetMessage(LEVEL_DEBUG, 'Read Sacramento.Cap info')
  if (idebug .ne. 0) write(idebug,*) ' Read Sacramento.Cap file'
  Call SetSacrReferenceToDefZero (ReferenceToDefinition)

  endfil = .false.
  teller = 0
  RetVal = 0
  CALL SKPCOM (Infile1, ENDFIL, 'ODS')
  Do while (.not. endfil)
    READ (Infile1,'(A1000)',END=2111,ERR=150,IOSTAT=IECODE) STRING
    IF (STRING(1:4) .EQ. 'CAPS') Then
! Read Capacity id
    RetVal = RetVal + GetVAR2 (STRING,' id ',1,' Sacramento_readAscii',' Sacramento.Cap file',IOUT1, &
                  CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
    name = CDUM(1)
    teller = teller + 1
! Eerst testen of capacity definition wel gebruikt wordt, dan pas verwerken
    ISacr = FindString (NcSacr, CapDef, Name, NcSacr, CaseSensitive)
    Occurs = (ISacr .gt. 0)
    if (ISacr .gt. 0) then
       if (ReferenceToDefinition(iSacr) .gt. 0) then
          call SetMessage(LEVEL_ERROR, 'Capacity Definition '//Name(1:Len_Trim(Name))//' double in datafile Sacramento.Cap')
       endif
    endif
! Verwerk Capacity definition
    if (occurs) then
! Read maximum capacities, initial contents and k-values
      RetVal = RetVal + GetVAR2 (STRING,' uztwm ',2,' Sacramento_readAscii',' Sacramento.Cap file',IOUT1, &
                  CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
      UZTWMDum = RDUM(1)
      RetVal = RetVal + GetVAR2 (STRING,' uztwc ',2,' Sacramento_readAscii',' Sacramento.Cap file',IOUT1, &
                  CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
      UZTWcDum = RDUM(1)
      RetVal = RetVal + GetVAR2 (STRING,' uzfwm ',2,' Sacramento_readAscii',' Sacramento.Cap file',IOUT1, &
                  CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
      UZFWMDum = RDUM(1)
      RetVal = RetVal + GetVAR2 (STRING,' uzfwc ',2,' Sacramento_readAscii',' Sacramento.Cap file',IOUT1, &
                  CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
      UZFWcDum = RDUM(1)
      RetVal = RetVal + GetVAR2 (STRING,' lztwm ',2,' Sacramento_readAscii',' Sacramento.Cap file',IOUT1, &
                  CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
      LZTWMDum = RDUM(1)
      RetVal = RetVal + GetVAR2 (STRING,' lztwc ',2,' Sacramento_readAscii',' Sacramento.Cap file',IOUT1, &
                  CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
      LZTWcDum = RDUM(1)
      RetVal = RetVal + GetVAR2 (STRING,' lzfsm ',2,' Sacramento_readAscii',' Sacramento.Cap file',IOUT1, &
                  CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
      LZFSMDum = RDUM(1)
      RetVal = RetVal + GetVAR2 (STRING,' lzfsc ',2,' Sacramento_readAscii',' Sacramento.Cap file',IOUT1, &
                  CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
      LZFScDum = RDUM(1)
      RetVal = RetVal + GetVAR2 (STRING,' lzfpm ',2,' Sacramento_readAscii',' Sacramento.Cap file',IOUT1, &
                  CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
      LZFPMDum = RDUM(1)
      RetVal = RetVal + GetVAR2 (STRING,' lzfpc ',2,' Sacramento_readAscii',' Sacramento.Cap file',IOUT1, &
                  CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
      LZFPcDum = RDUM(1)
      RetVal = RetVal + GetVAR2 (STRING,' uzk ',2,' Sacramento_readAscii',' Sacramento.Cap file',IOUT1, &
                  CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
      UzkDum = RDUM(1)
      RetVal = RetVal + GetVAR2 (STRING,' lzsk ',2,' Sacramento_readAscii',' Sacramento.Cap file',IOUT1, &
                  CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
      LzSkDum = RDUM(1)
      RetVal = RetVal + GetVAR2 (STRING,' lzpk ',2,' Sacramento_readAscii',' Sacramento.Cap file',IOUT1, &
                  CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
      LzPkDum = RDUM(1)
! nav Jira 25029
! Check consistency: make sure that initial content <= max. content
      UzTwCDum = min (UzTwMDum, UzTwCDum)
      UZFwCDum = min (UzFwMDum, UzFwCDum)
      LzTwCDum = min (LzTwMDum, LzTwCDum)
      LzFsCDum = min (LzFSMDum, LzFSCDum)
      LzFpCDum = min (LzFPMDum, LzFPCDum)
! End Jira 25029
! Assign definition to individual nodes
      Do iSacr = 1, ncSacr
        if (StringComp(CapDef(ISacr), Name, CaseSensitive) )  then
          ReferenceToDefinition(iSacr) = teller
          UZTWM (iSacr) = UzTwMDum
          UZTWC (iSacr) = UzTwCDum
          UZFWM (iSacr) = UzFwMDum
          UZFWC (iSacr) = UzFwCDum
          LZTWM (iSacr) = LzTwMDum
          LZTWC (iSacr) = LzTwCDum
          LZFSM (iSacr) = LzFSMDum
          LZFSC (iSacr) = LzFSCDum
          LZFPM (iSacr) = LzFPMDum
          LZFPC (iSacr) = LzFPCDum
          UZK   (iSacr) = UzKDum
          LZSK  (iSacr) = LZSKDum
          LZPK  (iSacr) = LZPKDum
          UZTWMInit (iSacr) = UzTwMDum
          UZTWCInit (iSacr) = UzTwCDum
          UZFWMInit (iSacr) = UzFwMDum
          UZFWCInit (iSacr) = UzFwCDum
          LZTWMInit (iSacr) = LzTwMDum
          LZTWCInit (iSacr) = LzTwCDum
          LZFSMInit (iSacr) = LzFSMDum
          LZFSCInit (iSacr) = LzFSCDum
          LZFPMInit (iSacr) = LzFPMDum
          LZFPCInit (iSacr) = LzFPCDum
        endif
      Enddo
    Endif
   Endif
   CALL SKPCOM (Infile1, ENDFIL, 'ODS')
  Enddo
2111 Continue

   If (RetVal .gt. 0) call ErrMsgStandard (972, 0, ' Error reading Sacramento input', ' Error getting CAPS records')
   Err969 = .false.
   Do iSacr = 1, ncSacr
     if (ReferenceToDefinition(iSacr) .eq. 0 .and. CapDEF (iSacr) .ne. '')  then
         Err969 = .true.
         call ErrMsgStandard (969, 0, ' Capacity definition not found in .Cap file.', CapDef(iSacr))
     endif
   Enddo
   If (Err969) call ErrMsgStandard (972, 0, ' Not enough Sacramento data found', &
                              ' Some CAPS Definitions not present in Sacramento.Cap file')




!Sacramento.Uh  file
  rewind(infile1)
  call SetMessage(LEVEL_DEBUG, 'Read Sacramento.Uh info')
  if (idebug .ne. 0) write(idebug,*) ' Read Sacramento.Uh  file'
  Call SetSacrReferenceToDefZero (ReferenceToDefinition)

  endfil = .false.
  teller = 0
  RetVal = 0
  CALL SKPCOM (Infile1, ENDFIL, 'ODS')
  Do while (.not. endfil)
    READ (Infile1,'(A1000)',END=3111,ERR=150,IOSTAT=IECODE) STRING
    IF (STRING(1:4) .EQ. 'UNIH') Then
! Read UnitHydrograph id
    RetVal = RetVal + GetVAR2 (STRING,' id ',1,' Sacramento_readAscii',' Sacramento.Uh file',IOUT1, &
                  CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
    name = CDUM(1)
    teller = teller + 1
! Eerst testen of unit hydrograph definition wel gebruikt wordt, dan pas verwerken
    ISacr = FindString (NcSacr, UnitHyDef, Name, NcSacr, CaseSensitive)
    Occurs = (ISacr .gt. 0)
    if (ISacr .gt. 0) then
       if (ReferenceToDefinition(iSacr) .gt. 0) then
          call SetMessage(LEVEL_ERROR, 'UnitHydrograph Definition '//name(1:Len_Trim(Name))//' double in datafile Sacramento.UH')
       endif
    endif
! Verwerk UnitHydrograph definition
    if (occurs) then
! Read unithydrograph components and time step
      RetVal = RetVal + GetVRS2(STRING,' uh ',2,' Sacramento-ReadAscii',' Sacramento.UH file', &
             IOUT1, CDUM(1), RDUM(1), IDUM(1), 36, IflRtn)
      Do i=1,36
         UHdum (I) = RDUM(I)
      Enddo
      RetVal = RetVal + GetVAR2 (STRING,' dt ',2,' Sacramento_readAscii',' Sacramento.UH file',IOUT1, &
                  CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
      UHDtDum = RDUM(1)
! Assign definition to individual nodes
      Do iSacr = 1, ncSacr
        if (StringComp(UnitHyDef(ISacr), Name, CaseSensitive) )  then
          ReferenceToDefinition(iSacr) = teller
          Do i=1,36
             UnitHydComp(iSacr,i) = UHDUM(i)
          Enddo
          UnitHyDt (iSacr) = max(1, UHdtDum)
        endif
      Enddo
    Endif
   Endif
   CALL SKPCOM (Infile1, ENDFIL, 'ODS')
  Enddo
3111 Continue

   If (RetVal .gt. 0) call ErrMsgStandard (972, 0, ' Error reading Sacramento input', ' Error getting UNIH records')
   Err969 = .false.
   Do iSacr = 1, ncSacr
     if (ReferenceToDefinition(iSacr) .eq. 0 .and. UnitHyDEF (iSacr) .ne. '')  then
         Err969 = .true.
         call ErrMsgStandard (969, 0, ' UnitHydrograph definition not found in .UH file.', UnitHyDef(iSacr))
     endif
   Enddo
   If (Err969) call ErrMsgStandard (972, 0, ' Not enough Sacramento data found', &
                              ' Some UNIH Definitions not present in Sacramento.UH file')


!
!
!Sacramento.Oth file
  rewind(infile1)
  call SetMessage(LEVEL_DEBUG, 'Read Sacramento.Oth info')
  if (idebug .ne. 0) write(idebug,*) ' Read Sacramento.Oth file'
  Call SetSacrReferenceToDefZero (ReferenceToDefinition)

  endfil = .false.
  teller = 0
  RetVal = 0
  CALL SKPCOM (Infile1, ENDFIL, 'ODS')
  Do while (.not. endfil)
    READ (Infile1,'(A1000)',END=4111,ERR=150,IOSTAT=IECODE) STRING
    IF (STRING(1:4) .EQ. 'OPAR') Then
! Read Other Parameter id
    RetVal = RetVal + GetVAR2 (STRING,' id ',1,' Sacramento_readAscii',' Sacramento.Oth file',IOUT1, &
                  CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
    name = CDUM(1)
    teller = teller + 1
! Eerst testen of other parameter definition wel gebruikt wordt, dan pas verwerken
    ISacr = FindString (NcSacr, OtherDef, Name, NcSacr, CaseSensitive)
    Occurs = (ISacr .gt. 0)
    if (ISacr .gt. 0) then
       if (ReferenceToDefinition(iSacr) .gt. 0) then
          call SetMessage(LEVEL_ERROR, 'OtherParameters Definition '//name(1:Len_Trim(Name))//' double in datafile Sacramento.Oth')
       endif
    endif
! Verwerk OtherParameters Definition
    if (occurs) then
! Read other parameters
      RetVal = RetVal + GetVAR2 (STRING,' zperc ',2,' Sacramento_readAscii',' Sacramento.Oth file',IOUT1, &
                  CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
      ZPERCDum = RDUM(1)
      RetVal = RetVal + GetVAR2 (STRING,' rexp ',2,' Sacramento_readAscii',' Sacramento.Oth file',IOUT1, &
                  CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
      REXPDum = RDUM(1)
      RetVal = RetVal + GetVAR2 (STRING,' pfree ',2,' Sacramento_readAscii',' Sacramento.Oth file',IOUT1, &
                  CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
      PFREEDum = RDUM(1)
      RetVal = RetVal + GetVAR2 (STRING,' rserv ',2,' Sacramento_readAscii',' Sacramento.Oth file',IOUT1, &
                  CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
      RSERVDum = RDUM(1)
      RetVal = RetVal + GetVAR2 (STRING,' pctim ',2,' Sacramento_readAscii',' Sacramento.Oth file',IOUT1, &
                  CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
      PCTIMDum = RDUM(1)
      RetVal = RetVal + GetVAR2 (STRING,' adimp ',2,' Sacramento_readAscii',' Sacramento.Oth file',IOUT1, &
                  CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
      ADIMPDum = RDUM(1)
      RetVal = RetVal + GetVAR2 (STRING,' sarva ',2,' Sacramento_readAscii',' Sacramento.Oth file',IOUT1, &
                  CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
      SARVADum = RDUM(1)
      RetVal = RetVal + GetVAR2 (STRING,' side ',2,' Sacramento_readAscii',' Sacramento.Oth file',IOUT1, &
                  CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
      SIDEDum = RDUM(1)
      RetVal = RetVal + GetVAR2 (STRING,' ssout ',2,' Sacramento_readAscii',' Sacramento.Oth file',IOUT1, &
                  CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
      SSOUTDum = RDUM(1)
      RetVal = RetVal + GetVAR2 (STRING,' pm ',2,' Sacramento_readAscii',' Sacramento.Oth file',IOUT1, &
                  CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
      PMDum = RDUM(1)
      RetVal = RetVal + GetVAR2 (STRING,' pt1 ',2,' Sacramento_readAscii',' Sacramento.Oth file',IOUT1, &
                  CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
      PT1Dum = RDUM(1)
      RetVal = RetVal + GetVAR2 (STRING,' pt2 ',2,' Sacramento_readAscii',' Sacramento.Oth file',IOUT1, &
                  CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
      PT2Dum = RDUM(1)
! Assign definition to individual nodes
      Do iSacr = 1, ncSacr
        if (StringComp(OtherDef(ISacr), Name, CaseSensitive) )  then
          ReferenceToDefinition(iSacr) = teller
          ZPERC (iSacr) = ZPERCDum
          REXP  (iSacr) = REXPDum
          PFREE (iSacr) = PFREEDum
          RSERV (iSacr) = RSERVDum
          PCTIM (iSacr) = PCTIMDum
          ADIMP (iSacr) = ADIMPDum
          SARVA (iSacr) = SARVADum
          SIDE  (iSacr) = SIDEDum
          SSOUT (iSacr) = SSOUTDum
          PM    (iSacr) = PMDum
          PT1   (iSacr) = PT1Dum
          PT2   (iSacr) = PT2Dum
        endif
      Enddo
    Endif
   Endif
   CALL SKPCOM (Infile1, ENDFIL, 'ODS')
  Enddo
4111 Continue

   If (RetVal .gt. 0) call ErrMsgStandard (972, 0, ' Error reading Sacramento input', ' Error getting OPAR records')
   Err969 = .false.
   Do iSacr = 1, ncSacr
     if (ReferenceToDefinition(iSacr) .eq. 0 .and. OtherDEF (iSacr) .ne. '')  then
         Err969 = .true.
         call ErrMsgStandard (969, 0, ' Other Parameters definition not found in .Oth file.', OtherDef(iSacr))
     endif
   Enddo
   If (Err969) call ErrMsgStandard (972, 0, ' Not enough Sacramento data found', &
                              ' Some Other Parameter Definitions not present in Sacramento.Oth file')



     iDebug = 0
     return

 150 CONTINUE
     DeAllocate ( AlreadyRead)
     DeAllocate ( ReferenceToDefinition)
     call SetMessage(LEVEL_FATAL, 'Read error in Sacramento ASCII')

     Return
  End subroutine Sacramento_readASCII



  Subroutine SetSacrReferenceToDefZero (ReferenceToDefinition)

  Integer ReferenceToDefinition(ncSacr)

  ReferenceToDefinition = 0

  Return
  End subroutine SetSacrReferenceToDefZero



  Subroutine Sacramento_Init1(IEvent, Iout1, IDoor, FirstCall)

  implicit none

  Integer IEvent, IOut1, IDoor
  Integer iSacr, i,j,k,juhsz, iow, nunit, Munit, KindNd, IKndNr, errstatus, SegmentUH, Samini
  Real    Dro(501)  ! 121
  Real    QQ (501), QD(501), QS(501), QI(501)
  Logical FirstCall, Err972
  Character(Len=CharIdLength) TempString

  iow = iout1
! Write(*,*) ' Sacramento_init1'
  Err972 = .false.


! Write(*,*) ' Sacramento_init1 RSeg part'
! this part only once, at start first event

  If (IEvent .eq. 1 .and. FirstCall) Then

    Do iSacr = 1, ncSacr
!      initialisation
       NUnit = 36
       MUnit = 36
       juhsz = UnitHyDt(ISacr)
       Do j=1,36
          Dro(j) = UnitHydComp(ISacr,j)
       enddo
! ARS 13413
       do i=1,Munit
          k=munit-i+1
          if(dro(k).gt.0.) goto 26
       enddo
  26   Nunit=k
! end ARS 13413
!  Check that number of components <= 501 (was 121)   ! improved test Jan 2008
      if ( (juhsz * Nunit + juhsz -1) .gt. 501) then
         do i=1,NcNode
            Kindnd = EiNode(I,3)
            Ikndnr = EiNode(I,2)
            if (kindnd .eq. 16 .and. ikndnr .eq. isacr) goto 27
         Enddo
      27 Continue
         TempString = Id_Nod(I)
         Write(Iout1,*) ' For Sacramento node ',Tempstring(1:Len_Trim(Tempstring))
         call ErrMsgStandard (974, 0, ' Nr. of computed unit hydrograph components exceeds Sacramento maximum of 501', ' in routine Sacr_Init1' )
         Err972 = .true.
      endif

!  Uit Sacramento_uni:
!     create components for nuseg(i) * juhsz time steps from munit;
!     given components by interpolation

      errStatus =  SegmentUH (dro, MUnit, NUnit, juhsz)

! Save results
      UnitHyDt(ISacr) =NUnit

! ARS 13413, zeker weten dat dimensies niet overschreden worden
      NUnit = min (501, NUnit)
      UnitHyDt(ISacr) = NUnit
! end ARS 13413

      Do j=1,Nunit
         UnitHydComp(ISacr,j) = Dro(j)
      enddo

      WRITE(IOW,111) juhsz, NUnit, (DRO(I),I=1,NUNIT)
  999 CONTINUE

  111 FORMAT(/,' Step (dt):',i3, &
             /,' Number of unit hydrograph components:',I3, &
             /,' Computed unit hydrograph components:',/,1X,6(F8.6,1X),/,(1X,6(F8.6,1X)))
    Enddo
    If (Err972) &
      call ErrMsgStandard (972, 0, ' For some Sacramento nodes the nr. of computed UH-components exceeds the maximum of 501', ' ')

  Endif


! This part: Always
!  Vector initialisation
   UZTWM = UZTWMInit
   UZTWC = UZTWCInit
   UZFWM = UZFWMInit
   UZFWC = UZFWCInit
   LZTWM = LZTWMInit
   LZTWC = LZTWCInit
   LZFSM = LZFSMInit
   LZFSC = LZFSCInit
   LZFPM = LZFPMInit
   LZFPC = LZFPCInit


! Write(*,*) ' Sacramento_init1 Samini part'
! Uit Sacramento_uni (initialisatie)
!     Samini COMPUTES SOME INITIAL VALUES OF LAND SUBROUTINE AND
!     ADJUSTS BASEFLOW CAPACITIES AND CONTENTS

   Do ISacr = 1, NSacr
! Aug 2016
! QQ() ipv SACR_QQ
      do k=1,501
         QQ(k) = Sacr_QQ(isacr,k)
! GP added separation direct/surface/interflow
         QD(k) = Sacr_QD(isacr,k)
         QS(k) = Sacr_QS(isacr,k)
         QI(k) = Sacr_QI(isacr,k)
      enddo
      errStatus = Samini(idoor,ReadAdimCInRestartFile,FirstCall, &
                         LZFSM(ISacr),LZFPM(ISacr), &
                         UZTWC(ISacr),UZFWC(ISacr),LZTWC(ISacr),LZFSC(ISacr),LZFPC(ISacr), &
                         LZSK(ISacr),LZPK(ISacr),RSERV(ISacr),PCTIM(ISacr), &
                         ADIMP(ISacr),SARVA(ISacr),SIDE(ISacr),STOR(ISacr),ADIMC(ISacr), &
                         SID(ISacr),SAVED(ISacr),PERCM(ISacr),QQ,QD,QS,QI)
   Enddo

   Call Sacramento_Init2

  Return
  End subroutine Sacramento_Init1




  Subroutine Sacramento_Init2

  implicit none

!  Integer ISacr
!
!  Do ISacr=1,NcSacr
!     UZTWC0(ISacr) = UZTWC(ISacr)
!     UZFWC0(ISacr) = UZFWC(ISacr)
!     LZTWC0(ISacr) = LZTWC(ISacr)
!     LZFPC0(ISacr) = LZFPC(ISacr)
!     LZFSC0(ISacr) = LZFSC(ISacr)
!     ADIMC0(ISacr) = ADIMC(ISacr)
!  Enddo
! Vector initialisation
      UZTWC0 = UZTWC
      UZFWC0 = UZFWC
      LZTWC0 = LZTWC
      LZFPC0 = LZFPC
      LZFSC0 = LZFSC
      ADIMC0 = ADIMC
      Sacr_qq0 = Sacr_qq
      Sacr_qd0 = Sacr_qd
      Sacr_qs0 = Sacr_qs
      Sacr_qi0 = Sacr_qi

  Return
  End subroutine Sacramento_Init2


  Subroutine ReadOpenDASacramento (Infile1, iout1, update)

  ! read Sacramento restart info from ASCII OpenDA file
  integer      Infile1, iout1, idebug
  Logical      update
  Integer      RetVal
  integer      Isacr

  Integer       inod
  Integer       iecode
  Character(Len=1000) String
  Integer        Nhlp
  Parameter     (Nhlp = 32)
  Integer       IDUM(NHLP)
  Real          RDUM(NHLP)
  Character(Len=CharIdLength) CDUM(NHLP)
  Logical       allow, found, endfil

  ! file is already opened, rewind it
  Rewind(Infile1)
  update = .false.
  RetVal = 0
  iDebug = ConfFil_get_iDebug()

  Call SKPCOM (Infile1, ENDFIL,'ODS')
  call SetMessage(LEVEL_INFO, 'Read OpenDA Sacramento data')
  do while (.not. endfil)
    ! read OpenDA record
     Read(Infile1,'(A1000)',END=21,ERR=150,IOSTAT=IECODE)  STRING
     ! skip regel als hij niet begint met juist keyword (SACR)
     If (STRING(1:4) .EQ. 'SACR') then
      ! SACR node id
        allow = .false.
        Retval = Retval + GetVAR2 (STRING,' id ',1,' SACR-ReadAscii',' OpenDA file',IOUT1, &
                                   CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
        call fndNd2(inod, CDUM(1))
        if (inod .gt. 0) then      ! knoop bestaat in schematisatie
            isacr = EiNode(inod,2)
            if (EiNode(inod,3) .eq. 16) then  ! en is Sacramento
                ! get the data
                ! update the corresponding RR variables and related variables
                allow = .true.
                Retval = Retval + GetVAR2 (STRING,' UZTWC ',2, ' SACR-ReadAscii',' OPENDA file',IOUT1, &
                                              CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
                if (found) then
                   write(*,*) ' found Sacramento id and UZTWC ', Id_nod(inod)(1:len_trim(id_nod(inod))), rdum(1)
                   UZTWCInit(isacr) = Rdum(1)
                   RSLMAP17_Sacr(1,isacr,1) = UZTWCInit(isacr)
                   SACR_Tnul(1,isacr) = RSLMAP17_sacr(1,isacr,1)
                   update = .true.
                endif
                allow = .true.
                Retval = Retval + GetVAR2 (STRING,' UZFWC ',2, ' SACR-ReadAscii',' OPENDA file',IOUT1, &
                                              CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
                if (found) then
                   write(*,*) ' found Sacramento id and UZFWC ', Id_nod(inod)(1:len_trim(id_nod(inod))), rdum(1)
                   UZFWCInit(isacr) = Rdum(1)
                   RSLMAP17_Sacr(2,isacr,1) = UZFWCInit(isacr)
                   SACR_Tnul(2,isacr) = RSLMAP17_sacr(2,isacr,1)
                   update = .true.
                endif
                allow = .true.
                Retval = Retval + GetVAR2 (STRING,' LZTWC ',2, ' SACR-ReadAscii',' OPENDA file',IOUT1, &
                                              CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
                if (found) then
                   write(*,*) ' found Sacramento id and LZTWC ', Id_nod(inod)(1:len_trim(id_nod(inod))), rdum(1)
                   LZTWCInit(isacr) = Rdum(1)
                   RSLMAP17_Sacr(3,isacr,1) = LZTWCInit(isacr)
                   SACR_Tnul(3,isacr) = RSLMAP17_sacr(3,isacr,1)
                   update = .true.
                endif
                allow = .true.
                Retval = Retval + GetVAR2 (STRING,' LZFPC ',2, ' SACR-ReadAscii',' OPENDA file',IOUT1, &
                                              CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
                if (found) then
                   write(*,*) ' found Sacramento id and LZFPC ', Id_nod(inod)(1:len_trim(id_nod(inod))), rdum(1)
                   LZFPCInit(isacr) = Rdum(1)
                   RSLMAP17_Sacr(4,isacr,1) = LZFPCInit(isacr)
                   SACR_Tnul(4,isacr) = RSLMAP17_sacr(4,isacr,1)
                   update = .true.
                endif
                allow = .true.
                Retval = Retval + GetVAR2 (STRING,' LZFSC ',2, ' SACR-ReadAscii',' OPENDA file',IOUT1, &
                                              CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
                if (found) then
                   write(*,*) ' found Sacramento id and LZFSC ', Id_nod(inod)(1:len_trim(id_nod(inod))), rdum(1)
                   LZFSCInit(isacr) = Rdum(1)
                   RSLMAP17_Sacr(5,isacr,1) = LZFSCInit(isacr)
                   SACR_Tnul(5,isacr) = RSLMAP17_sacr(5,isacr,1)
                   update = .true.
                endif
            endif
        endif
     Endif
  enddo
  goto 21

150 continue
    call SetMessage(LEVEL_FATAL, 'Error reading OpenDA restart record '//String(1:len_trim(string)))
21  continue

  End subroutine ReadOpenDASacramento


  Subroutine WriteOpenDASacramento (Infile1)

  ! write Sacramento restart info to ASCII OpenDA file
  integer      Infile1, idebug

  Integer       inode, isacr
  Character*1   Quote

  ! file is already opened
  iDebug = ConfFil_get_iDebug()
  quote = ''''

  do inode=1,ncnode
     isacr = EiNode(inode,2)
     if (EiNode(inode,3) .eq. 16) then  ! en is Sacramento
         write(Infile1,'(A,A1,A,A1,5(1X,A,G15.8),A)') 'SACR id ',&
                                       quote,id_nod(inode)(1:len_trim(id_nod(inode))),quote,&
                                       ' UZTWC ', UZTWC(isacr), &
                                       ' UZFWC ', UZFWC(isacr), &
                                       ' LZTWC ', LZTWC(isacr), &
                                       ' LZFPC ', LZFPC(isacr), &
                                       ' LZFSC ', LZFSC(isacr), ' sacr'
     Endif
  enddo

  End subroutine WriteOpenDaSacramento




  SUBROUTINE CMPSacramento (ITMSTP, ISacr, IMETEO, INODE, ILink)
    ! *********************************************************************
    ! *** Brief description:
    ! *** ------------------
    ! ***    This subroutine performs computations for Sacramento nodes
    ! *********************************************************************
    ! *** Input/output parameters:
    ! *** ------------------------
    ! ***  IEVENT = event number
    ! ***  ITMSTP = timestep number
    ! ***  IDEBUG = file unit number of debug file
    ! ***  IOUT1  = file unit number of output file with run messages
    ! ***  ISACR  = intern nr. Sacramento node
    ! ***  IMETEO = meteostation code
    ! ***  INODE  = knoopnr
    ! ***  ILink  = link nr
    ! *********************************************************************
    ! *** Berekeningen voor Sacramento model
    ! *********************************************************************

    implicit none

    Integer iTmStp, iSacr, iMeteo, iNode, ILink
    Integer iDebug, Iout1, idum

    Real    Fract
    Integer NUnit
    Real    Dro(501)
    Real    PLiq, Edmnd, QF, Qf2, RoImp, SSOUTAct, FloDirect, FloSurf, FloInter, PercAct
    Real    EUSED, FLOBF, FLOBS
    Real    FLOSF, FLOIN
    Real    QQ (501), QD(501), QS(501), QI(501)
    Integer IOW
    Integer IBND, iPluv, iConn, IBifur
    Integer errStatus, Landsc2  !, Landsc


    iDebug = ConfFil_get_iDebug()
    iOut1  = ConfFil_get_iOut1 ()
!   Write(*,*) ' Start CmpSacr', ISacr, Itmstp

! initial situation
      IF (iDebug .ne. 0) THEN
         WRITE(IDEBUG,*) 'CMPSacramento isacr=',ISacr
         WRITE(IDEBUG,*) ' Sacramento Node', Id_Nod(INODE)
         WRITE(IDEBUG,*) ' Downstream link index', ilink
         WRITE(IDEBUG,*) ' Timestep nr                 :',ITMSTP
         WRITE(IDEBUG,*) ' Timestepsize in s           :',timeSettings%timestepSize
         WRITE(IDEBUG,*) ' Node Area (m2)              :',AreaSa(ISacr)
         WRITE(IDEBUG,*) ' Initial storage contents    :'
         WRITE(IDEBUG,*) '    UZTWC                    :', UZTWC(ISacr)
         WRITE(IDEBUG,*) '    UZFWC                    :', UZFWC(ISacr)
         WRITE(IDEBUG,*) '    LZTWC                    :', LZTWC(ISacr)
         WRITE(IDEBUG,*) '    LZFPC                    :', LZFPC(ISacr)
         WRITE(IDEBUG,*) '    LZFSC                    :', LZFSC(ISacr)
      ENDIF

! simulate
! par. lijst = originele par. lijst (flosf) + common blocks - de niet gebruikte variabelen
! Initialise
      Fract = TimeSettings%Timestepsize / 86400.  ! fraction of day
      NUnit = UnitHyDt(ISacr)

      Do idum=1,Nunit
         DRO(idum) = UnitHydComp (ISacr,idum)
      Enddo
      Do idum=1,501
         QQ (idum) = Sacr_QQ (ISacr,idum)
         QD (idum) = Sacr_QD (ISacr,idum)
         QS (idum) = Sacr_QS (ISacr,idum)
         QI (idum) = Sacr_QI (ISacr,idum)
      Enddo
      PLiq = AAFNodeRainfall(inode)* Rain(Imeteo) * TimeSettings%Timestepsize / mm2m   ! in mm
      EDmnd= Evap(Imeteo) * TimeSettings%Timestepsize / mm2m   ! in mm
      iow = iout1

! Landsc from Sacramento-Uni
!     Write(*,*) ' Before Landsc'
   if (SacrCompOption(ISacr) .eq. 1) then
!     errStatus = LandSc (FLOSF, &
!                   FRACT, &
!                   UZTWM(ISacr),UZFWM(ISacr),LZTWM(ISacr),LZFSM(ISacr),LZFPM(ISacr), &
!                   UZTWC(ISacr),UZFWC(ISacr),LZTWC(ISacr),LZFSC(ISacr),LZFPC(ISacr), &
!                   UZK(ISacr),LZSK(ISacr),LZPK(ISacr),ZPERC(ISacr),REXP(ISacr),PFREE(ISacr), &
!                   PCTIM(ISacr),ADIMP(ISacr),SARVA(ISacr),SSOUT(ISacr), &
!                   DRO,PM(ISacr),PT1(ISacr),PT2(ISacr),NUNIT , &
!                   PLIQ,EDMND,QF,                              &
!                   STOR(ISacr),ADIMC(ISacr),SID(ISacr),SAVED(ISacr),PERCM(ISacr),EUSED,FLOBF,  &
!                   QQ, &
!                   FLOIN, FLOBS, ROIMP, SSOUTAct)
!     write(*,*) ' Sacramento Landsc2 for node', Id_Nod(INODE)
      errStatus = LandSc2 (FLOSF, FloDirect, FloSurf, FloInter, &
                    FRACT, &
                    UZTWM(ISacr),UZFWM(ISacr),LZTWM(ISacr),LZFSM(ISacr),LZFPM(ISacr), &
                    UZTWC(ISacr),UZFWC(ISacr),LZTWC(ISacr),LZFSC(ISacr),LZFPC(ISacr), &
                    UZK(ISacr),LZSK(ISacr),LZPK(ISacr),ZPERC(ISacr),REXP(ISacr),PFREE(ISacr), &
                    PCTIM(ISacr),ADIMP(ISacr),SARVA(ISacr),SSOUT(ISacr), &
                    DRO,PM(ISacr),PT1(ISacr),PT2(ISacr),NUNIT , &
                    PLIQ,EDMND,QF,                              &
                    STOR(ISacr),ADIMC(ISacr),SID(ISacr),SAVED(ISacr),PERCM(ISacr),EUSED,FLOBF,  &
                    QQ, QD, QS, QI, &
                    FLOIN, FLOBS, ROIMP, SSOUTAct, PercAct)
!     Write(*,*) ' After Landsc'
      if (errStatus .eq. 1) then
              call SetMessage(LEVEL_FATAL, 'Error in Landsc, UZTWM = 0')
              endif
      if (errStatus .eq. 2) then
         call SetMessage(LEVEL_FATAL, 'Error in Landsc, UZFWM = 0')
      endif
      if (errStatus .eq. 3) then
         call SetMessage(LEVEL_FATAL, 'Error in Landsc, PT2 = 0')
      endif
      if (errStatus .eq. 4) then
         call SetMessage(LEVEL_FATAL, 'Error in Landsc, PAV = 0')
      endif
      if (errStatus .eq. 5) then
        call SetMessage(LEVEL_FATAL, 'Error in Landsc, LZTWM = 0')
      endif
      if (errStatus .eq. 6) then
         call SetMessage(LEVEL_WARN, 'In Landsc, percolation set to 0')
      endif
!
! store results
      SacPRECIP(ISacr)      = PLIQ
      SacPOTEVAP(ISacr)     = EDMND
      SacACTEVAP(ISacr)     = EUSED
      SacBASEFLOW(ISacr)    = FLOBF
      SacRunoffImperv(ISacr)= ROIMP ! not routed
      SacINTERFLOW(ISacr)   = FLOIN ! not routed
      SacRunoffSurface(ISacr)= QS(1) ! not routed
      SacSurfaceFLOW(ISacr) = FLOSF ! total routed flow
      SacRoutedDirectFLOW(ISacr) = FLODirect ! routed direct paved/impervious flow
      SacRoutedSurfaceFLOW(ISacr) = FLOSurf ! routed direct paved/impervious flow
      SacRoutedInterFLOW(ISacr) = FLOInter ! routed direct paved/impervious flow
      SacTotalRunoff(ISacr) = QF
!     SacLossFLOW(ISacr)    = FLOBS + SSOUT(ISacr)
      SacLossFLOW(ISacr)    = FLOBS + SSOUTAct
      SacPercolation(ISacr) = PercAct
! NB In LandSc geen correctie van SSOUT als SSOUT > total runoff;
!     als een negatieve total runoff resteert na aftrek van SSOUT, dan wordt de total runoff op nul gezet,
!     SSOUT blijft ongewijzigd; gereduceerde waarde is aangegeven in SSOUTAct

      Do idum=1,501
         Sacr_QQ (iSacr,idum) = QQ (idum)
         Sacr_QD (iSacr,idum) = QD (idum)
         Sacr_QS (iSacr,idum) = QS (idum)
         Sacr_QI (iSacr,idum) = QI (idum)
      Enddo

      QF2 = QF * AreaSa(ISacr) * mm2m / TimeSettings%Timestepsize
      SacChannelInflow(ISacr) = QF2
   else
!     runoff node: use rainfall data read as runoff timeseries in m3/s
!     i.e. convert rain data back from m/s back to mm (original value in bui-file)
!     use NrSecsRai instead of TimeSettings%TimestepSize !!!
      QF2 = AAFNodeRainfall(inode) * Rain(imeteo) * NrSecsRai  / mm2m
      SacChannelInflow(ISacr) = AAFNodeRainfall(inode) * Rain(imeteo) * NrSecsRai / mm2m
!     for balance: put rainfall equal to runoff, but in mm
      SacPRECIP(ISacr)      = QF2 / AreaSa(isacr) / mm2m  * TimeSettings%TimestepSize
   endif

      IPluv= EIPluv(INODE) ! benedenstrooms een NWRW node
      IBND = EIBND(INODE)  ! benedenstrooms een rand
      IOw  = EIOW (INODE)  ! benedenstrooms een open water
      IConn = EIConn(INODE)  ! benedenstrooms een RRConnection
      IBifur= EIBifur(INODE)  ! benedenstrooms een RRBifurcation

      If (Ibnd .gt. 0) then
         QBND(IBND) = QBND(IBND) + QF2
! correctie 1 aug 2001
         QINBND(IBND)%totalSacramento = QINBND(IBND)%totalSacramento + QF2
      ElseIf (IPluv .gt. 0) then
         QinPluv(IPluv) = QInPluv(IPluv) + QF2
         QPluv(IPluv)%totalSacramento = QPluv(IPluv)%totalSacramento + QF2
      ElseIf (Iow .gt. 0) then
         QINOw(IOw,7) = QINOw(Iow,7) + QF2
      ElseIf (IBifur .gt. 0) then
         QINLink(ilink) = QF2
         if (LinkType(DownstreamLinkNr(inode)) .ne. 30) then
             QBifur(iBifur) = Qbifur(IBifur) +  QF2
         endif
      ElseIf (IConn .gt. 0) then
         QINLink(ilink) = QF2
         if (LinkType(DownstreamLinkNr(inode)) .ne. 30) then
            QinConn(iConn)%TotalConnection = QinConn(iConn)%TotalConnection + QinLink(ilink)
            QConn(iConn) = QConn(iConn) + Qinlink(ilink)
         else
            ! will be done in RRRoutingLink
         endif
      Endif
! final situation

      IF (iDebug .ne. 0) THEN
         WRITE(IDEBUG,*) ' Final storage contents      :'
         WRITE(IDEBUG,*) '    UZTWC                    :', UZTWC(ISacr)
         WRITE(IDEBUG,*) '    UZFWC                    :', UZFWC(ISacr)
         WRITE(IDEBUG,*) '    LZTWC                    :', LZTWC(ISacr)
         WRITE(IDEBUG,*) '    LZFPC                    :', LZFPC(ISacr)
         WRITE(IDEBUG,*) '    LZFSC                    :', LZFSC(ISacr)
         WRITE(IDEBUG,*) ' Precipitation               :', PLIQ
         WRITE(IDEBUG,*) ' Pot. Evaporation            :', EDMND
         WRITE(IDEBUG,*) ' Act. Evaporation            :', EUSED
         WRITE(IDEBUG,*) ' Baseflow                    :', FLOBF
         WRITE(IDEBUG,*) ' Interflow                   :', FLOIN
         WRITE(IDEBUG,*) ' Surface Runoff              :', FLOSF
         WRITE(IDEBUG,*) ' Runoff Impervious area      :', ROIMP
         WRITE(IDEBUG,*) ' Channel Inflow (mm)         :', QF
         WRITE(IDEBUG,*) ' Channel Inflow (m3/s)       :', QF2
         WRITE(IDEBUG,*) ' Loss flow Side+Ssout        :', SacLossFlow(ISacr)
         WRITE(IDEBUG,*) ' ADIMC                       :', ADIMC(ISacr)
         WRITE(IDEBUG,*) ' Storage                     :', STOR(ISacr)
         WRITE(IDEBUG,*) ' Sacr_qq                     :', (SACR_QQ(ISacr,idum),idum=1,10)
         WRITE(IDEBUG,*) ' Sacr_qq0                    :', (SACR_QQ0(ISacr,idum),idum=1,10)
      ENDIF


    ! *********************************************************************
    ! *** End
    ! *********************************************************************

    RETURN
  END subroutine CMPSacramento



  Subroutine WrInputDataSacramento (Iout9)

        ! *********************************************************************
        ! *** Last update:
        ! *********************************************************************
        ! *** Brief description:
        ! *** ------------------
        ! ***    Stuk uit sub WrData: uitvoer van Sacramento nodes in *.Out files
        ! *********************************************************************

        Implicit none

        Integer      INODE, IKIND, INR, idum
        Integer      IOUT9

! Sacramento area
      IF (NCSacr .GT. 0) THEN
         WRITE(IOUT9,161)
  161    FORMAT (//,' Summary input data Sacramento nodes    ',//,&
              ' Node identification     Node   Area       Storage Reservoir Capacities       ', &
              '   and initial contents (mm)        Depletion coefficients (1/day) ',/,  &
              '                        name    (ha)       UZTWM  UZFWM  LZTWM  LZFPM  LZFSM ', &
              '  UZTWC UZFWC  LZTWC  LZFSC  LZFPC     UZK    LZSK    LZPK',/,139('='))
         DO INODE =1,NCNODE
          IKIND = EiNode(INODE,3)
          INR   = EiNode(INODE,2)
          IF (IKIND .EQ. 16) THEN
            IF (AreaSa(INR) .LE. .00001) AREASa(INR) = 0.00001
            WRITE(IOUT9,162) Id_Nod(INODE),&
                           NamNod(INODE), &
                           AreaSa(INR)/HA2M, &
                           UZTWM(inr), UZFWM(inr), LZTWM(inr), LZFSM(inr), LZFPM(inr), &
                           UZTWC(inr), UZFWC(inr), LZTWC(inr), LZFSC(inr), LZFPC(inr), &
                           UZK(inr), LZSK(inr), LZPK(inr)
  162       FORMAT (A20,1X,A12,1X,F7.1,1X,10(F6.1,1X),3F8.3)
          ENDIF
         ENDDO

         WRITE(IOUT9,163)
  163    FORMAT (//,' Summary input data Sacramento nodes - part 2  ',//,&
              ' Node identification     Unit Hydrograph components               ', &
              '  and other parameters                      ',/,  &
              '                       1    2    3    4    5    6    7    8    9  ', &
              ' ZPERC REXP  PFREE RSERV PCTIM ADIMP SARVA SIDE  SSOUT  PM     PT1    PT2 ',/,140('='))
         DO INODE =1,NCNODE
          IKIND = EiNode(INODE,3)
          INR   = EiNode(INODE,2)
          IF (IKIND .EQ. 16) THEN
            WRITE(IOUT9,164) Id_Nod(INODE),&
                           (UnitHydComp(inr,idum),idum=1,9), &
                           ZPERC(inr), REXP (inr), PFREE(inr), RSERV(inr), PCTIM(inr), &
                           ADIMP(inr), SARVA(inr), SIDE (inr), SSOUT(inr), PM   (inr), &
                           PT1(inr), PT2 (inr)
  164       FORMAT (A20,1X,9(F4.2,1X),10(F5.2,1X),2F7.1)
          ENDIF
         ENDDO
      ENDIF


  Return
  End subroutine WrInputDataSacramento

  !> If success, function returns Values array of length ElementCount
  !! for paved elementset on specific quantity handle
  function RR_GetSacrDataByIntId(QuantityHandle, ElementCount, Values) result(success)

    use RR_open_mi_support
    use wl_open_mi_support

    implicit none

    ! return value
    logical  :: success

    ! arguments
    integer,          intent(in)      :: QuantityHandle   !< quant. handle
    integer,          intent(in)      :: ElementCount     !< #elems in Sacramento elementset
    double precision, intent(out), &
            dimension(1:ElementCount) :: Values           !< values in Sacramento elemenset

    ! locals

    Values  = 0
    success = .true.

    select case (QuantityHandle)
    ! Subdivide for various variables
    case(RRiSacrUZTWC)
    !RR Sacramento UZTW capacity
            ! If a stack overflow occurs due to this array operation, use a do loop
            if (NSACR > 0) then
                Values(1:NSACR) = RSLMAP17_Sacr(1, 1:NSACR, 1)
            else
                success = .false.
            endif
    case(RRiSacrUZFWC)
    !RR Sacramento UZFW capacity
            ! If a stack overflow occurs due to this array operation, use a do loop
            if (NSACR > 0) then
                Values(1:NSACR) = RSLMAP17_Sacr(2, 1:NSACR, 1)
            else
                success = .false.
            endif
   case(RRiSacrLZTWC)
    !RR Sacramento LZTW capacity
            ! If a stack overflow occurs due to this array operation, use a do loop
            if (NSACR > 0) then
                Values(1:NSACR) = RSLMAP17_Sacr(3, 1:NSACR, 1)
            else
                success = .false.
            endif
   case(RRiSacrLZFPC)
    !RR Sacramento LZFPW capacity
            ! If a stack overflow occurs due to this array operation, use a do loop
            if (NSACR > 0) then
                Values(1:NSACR) = RSLMAP17_Sacr(4, 1:NSACR, 1)
            else
                success = .false.
            endif
   case(RRiSacrLZFSC)
    !RR Sacramento LZFSW capacity
            ! If a stack overflow occurs due to this array operation, use a do loop
            if (NSACR > 0) then
                Values(1:NSACR) = RSLMAP17_Sacr(5, 1:NSACR, 1)
            else
                success = .false.
            endif
   case(RRiSacrPrecip)
   !RR Sacramento precipitation
            ! If a stack overflow occurs due to this array operation, use a do loop
            if (NSACR > 0) then
                Values(1:NSACR) = RSLMAP17_Sacr(6, 1:NSACR, 1)
            else
                success = .false.
            endif
   case(RRiSacrPotEvap)
   !RR Sacramento potential evaporation
            ! If a stack overflow occurs due to this array operation, use a do loop
            if (NSACR > 0) then
                Values(1:NSACR) = RSLMAP17_Sacr(7, 1:NSACR, 1)
            else
                success = .false.
            endif
   case(RRiSacrActEvap)
   !RR Sacramento actual evaporation
            ! If a stack overflow occurs due to this array operation, use a do loop
            if (NSACR > 0) then
                Values(1:NSACR) = RSLMAP17_Sacr(8, 1:NSACR, 1)
            else
                success = .false.
            endif
   case(RRiSacrBaseFlow)
   !RR Sacramento base flow
            ! If a stack overflow occurs due to this array operation, use a do loop
            if (NSACR > 0) then
                Values(1:NSACR) = RSLMAP17_Sacr(9, 1:NSACR, 1)
            else
                success = .false.
            endif
   case(RRiSacrSurfFlow)
   !RR Sacramento surface runoff
            ! If a stack overflow occurs due to this array operation, use a do loop
            if (NSACR > 0) then
                Values(1:NSACR) = RSLMAP17_Sacr(10, 1:NSACR, 1)
            else
                success = .false.
            endif
   case(RRiSacrRunoffImpArea)
   !RR Sacramento impervious area runoff
            ! If a stack overflow occurs due to this array operation, use a do loop
            if (NSACR > 0) then
                Values(1:NSACR) = RSLMAP17_Sacr(11, 1:NSACR, 1)
            else
                success = .false.
            endif
   case(RRiSacrTotalRunoff)
   !RR Sacramento total runoff
            ! If a stack overflow occurs due to this array operation, use a do loop
            if (NSACR > 0) then
                Values(1:NSACR) = RSLMAP17_Sacr(12, 1:NSACR, 1)
            else
                success = .false.
            endif
   case(RRiSacrChannelInflow)
   !RR Sacramento channel inflow
            ! If a stack overflow occurs due to this array operation, use a do loop
            if (NSACR > 0) then
                Values(1:NSACR) = RSLMAP17_Sacr(13, 1:NSACR, 1)
            else
                success = .false.
            endif
   case(RRiSacrSideSubSurfaceOutflow)
   !RR Sacramento side+subsurface outflow
            ! If a stack overflow occurs due to this array operation, use a do loop
            if (NSACR > 0) then
                Values(1:NSACR) = RSLMAP17_Sacr(14, 1:NSACR, 1)
            else
                success = .false.
            endif
   case(RRiSacrAddImpAreaContent)
   !RR Sacramento additional impervious area content
            ! If a stack overflow occurs due to this array operation, use a do loop
            if (NSACR > 0) then
                Values(1:NSACR) = RSLMAP17_Sacr(15, 1:NSACR, 1)
            else
                success = .false.
            endif
    case default
    ! Something is wrong
        success = .false.
    end select

  end function


end module Sacramento
