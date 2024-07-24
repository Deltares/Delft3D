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

    Module DecisionModule

    Use ParameterModule
    Use Dh_Alloc

    Implicit None

! Constants

! Variables
! *** Beslisparameter data
!
! *** PARAID = beslisparameter id
! *** PARADesc = beslisparameter description
! *** PARTYP = beslisparameter type 'internal', 'PARA' 'PAR2' or 'RSVP'
! *** PARDIM = beslisparameter lokaties per parameter ipar
! ***          (ipar,1) = aantal Sobek-Flow lokaties incl. 1D2D lokaties
! ***          (    ,2) = aantal 3B lokaties
! ***          (    ,3) = aantal precipitation lokaties
! ***          (    ,4) = aantal externe lokaties
! ***          (    ,5) = aantal WQ lokaties
! ***          (    ,6) = aantal beslisparameters
! ***          (    ,7) = aantal RSV beslisparameters gezet (default 3)
! ***          (    ,8) = aantal Interpolatie tabellen (0 of 1)
! ***          (    ,9) = aantal D3D lokaties
! *** PARSBK = sobek lokatie-id's per parameter (ipar,isbk)
! ***          per parameter, sobek lokatie van 1..pardim(ipar,1)
! *** PAR3B  = 3B lokatie-id's per parameter (ipar,i3b)
! ***          per parameter, 3B lokatie van 1..pardim(ipar,2)
! *** PARPRE = precipitation lokatieid's per parameter (ipar,iprec)
! ***          per parameter, precipitation lokatie van 1..pardim(ipar,3)
! *** PAREXT = externe lokatieid's per parameter (ipar,iext)
! ***          per parameter, externe lokatie van 1..pardim(ipar,4)
! *** PARWQ  = WQ lokatieid's per parameter (ipar,iswq)
! ***          per parameter, externe lokatie van 1..pardim(ipar,5)
! *** PARPAR = beslisparameter id's per parameter (ipar,idpar)
! ***          per parameter, decision parameter van 1..pardim(ipar,6)
! *** PAR3D  = D3DFlow lokatie-id's per parameter (ipar,i3d)
! ***          per parameter, 3D lokatie van 1..pardim(ipar,9)
! *** PARINTERP = interpolatietabel id per parameter (ipar,interpolationtable)
! ***          per parameter, interpolatie parameter van 1..pardim(ipar,8)
! *** PAROrder = volgorde van opgeven van de beslisparameter
! ***          parorder(1) = 1e variabele, geef type + volgnummer
! ***          parorder(2) = 2e variabele, geef type + volgnummer
! *** PARRSV = RSV beslisparameter id's per parameter (ipar,idpar)
! ***          per RSVP record, decision parameter van 1..pardim(ipar,7)
! *** NrRsvOutputPar = nr. RSV output parameters for decision parameter Ipara
! *** RSVINDX = RSV index number from decision parameter file
! *** Par3TableINDeX = Table index for PAR3 decision parameter
! *** VARSBK = Variabele nr. Sobek
! *** VAR3D  = Variabele nr. 3D
! *** VAR3B  = Variabele nr. 3B
! *** VARPRE = Variabele nr. precipitation
! *** VAREXT = Variabele nr. extern
! *** VARWQ  = Variabele nr. WQ
! *** VARIDSBK = Variabele id. Sobek
! *** VARID3DK = Variabele id. D3DFlow
! *** VARID3B  = Variabele id. 3B
! *** VARIDPRE = Variabele id. precipitation
! *** VARIDEXT = Variabele id. extern
! *** VARIDWQ  = Variabele id. WQ
! *** VARIDInterpolationTable  = Variabele id reference to InterpolationTable id
! *** CFSBK  = coefficients per parameter en sobek lokatie-id
! *** CF3D   = coefficients per parameter en D3DFlow lokatie-id
! *** CF3B   = coefficients per parameter en 3B lokatie-id
! *** CFPREC = coefficients per parameter en precipitation lokatie-id
! *** CFEXT  = coefficients per parameter en externe lokatie-id
! *** CFWQ   = coefficients per parameter en WQ lokatie-id
! *** CFPAR  = coefficients per parameter en parameter lokatie-id
! *** voor alle CF...: (ipar, i, 1) = coefficient vermenigvuldiging
! ***                  (ipar, i, 2) = coefficient optelling
! *** TISSBK  = time shift per parameter en sobek lokatie-id
! *** TIS3D   = time shift per parameter en D3DFlow lokatie-id
! *** TIS3B   = time shift per parameter en 3B lokatie-id
! *** TISPREC = time shift per parameter en precipitation lokatie-id
! *** TISEXT  = time shift per parameter en externe lokatie-id
!               time shift is integer waarde <= 0
! *** TISPAR  = time shift per parameter en beslisparameter; TISPAR < 0 (=0 not allowed)
! *** MxTmShift = maximum time shift per parameter/variabele
! ***          (1) = sobek variabelen
! ***          (2) = 3B data
! ***          (3) = precipitation data
! ***          (4) = externe data (wind)
! ***          (5) = beslisparameters
! ***          (6) = beslisparameters
! ***          (7) = bij RSVP records
! ***          (8) = not used (interpolation tables)
! ***          (9) = D3DFlow data
! *** SimSeq       = Simulation sequence of decision parameters (in which order are decision parameters computed?)
! ***                if SetSequenceDecisionParameters=false, then default =order in input file
! ***                elseif SetSequenceDecisionParameters=true, then determined by RTC
!
! *** IXSBHIS = index van sobek id in HIS file   per beslisparameter, lokatie Sobek
! *** IX3DHIS = index van 3D id in HIS file      per beslisparameter, lokatie D3DFlow
! *** IX3BHIS = index van 3B id in HIS file      per beslisparameter, lokatie 3B
! *** IXPRHIS = index van precip.id in HIS file  per beslisparameter, lokatie precipitation
! *** IXEXHIS = index van extern id in HIS file  per beslisparameter, lokatie extern
! *** IXWQHIS = index van WQ id in HIS file  per beslisparameter, lokatie WQ
! *** IXDPAR  = index van beslisparameter id in lijst beslisparameters, per beslisparameter
! *** IXInterpTable = index van interpolationtable id in lijst interpolation tables
! *** PAROPER = operation used in type 2 parameters
! *** DCVVAL  = berekende waarde beslisparameter
! ***           (ipar, it) ipar = parameter nr.
! ***                      it   = tijdstap index; 1  =huidig tijdstap,
! ***                                             1+x=x tijdstappen geleden

! *** Interpolation Table data
! *** InterpolationTableId = list if interpolation table ids
! *** InterpTableInput  (.,itable) = input interpolation table
! *** InterpTableOutput (.,itable) = output interpolation table


      CHARACTER(len=CharIdLength), Pointer, Save :: PARAID (:), ParaDescr(:), &
                                                    PARSBK(:,:), PAR3B(:,:), &
                                                    PAR3D(:,:), &
                                                    PARPRE(:,:), PAREXT(:,:), &
                                                    PARWQ(:,:), PARPAR(:,:), PARRSV(:,:), PARTYP(:), &
                                                    PARInterp(:,:)

      CHARACTER(len=CharIdLength), Pointer, Save :: InterpolationTableId(:)

      CHARACTER(len=CharIdLength), Pointer, Save :: VARIDSBK(:,:), VARID3B (:,:), &
                                                    VARIDPRE(:,:), VARIDEXT(:,:), &
                                                    VARID3D(:,:), &
                                                    VARIDWQ (:,:), VarIdInterpolationTable(:,:)

      INTEGER, Pointer, Save ::           PARDIM(:,:), MxTmShift (:), &
                                          NrRsvOutputPar(:), &
                                          VARSBK(:,:), VAR3B (:,:), &
                                          VARPRE(:,:), VAREXT(:,:), &
                                          VARWQ (:,:), VAR3D (:,:), &
                                          TISSBK(:,:), TIS3B (:,:), &
                                          TISPRE(:,:), TISEXT(:,:), &
                                          TISWQ (:,:), TISPAR(:,:),  &
                                          TIS3D (:,:), &
                                          IXSBHIS(:,:), IX3BHIS(:,:), &
                                          IXPRHIS(:,:), IXEXHIS(:,:), &
                                          IXWQHIS(:,:), IXDPAR (:,:),  &
                                          IX3DHIS(:,:), &
                                          IXInterpTable(:,:), &
                                          PAROPER(:), RSVINDX(:), Par3TableIndex(:), &
                                          SimSeq(:), ParOrder(:,:,:)

      Integer         , Pointer, Save ::   NrInterpolationPoints(:)
      Double Precision, Pointer, Save ::   InterpTableInput(:,:), InterpTableOutput(:,:)

      Double Precision, Pointer, Save ::   CFSBK(:,:,:), CF3B (:,:,:), &
                                           CFPRE(:,:,:), CFEXT(:,:,:), &
                                           CFWQ (:,:,:), CFPAR(:,:,:), &
                                           CF3D (:,:,:)

      Double Precision, Pointer, Save ::   DCVVAL(:,:)



   Contains

     Function AllocDecisionArrays (Iout1) result(RetVal)

      Integer :: RetVal

      Logical    success
      Integer    Iout1, idim

      RetVal = 0

      success = DH_AllocInit(NDecV, ParaId,'')
      success = success .and. DH_AllocInit(NDecv, ParaDescr, '')
      success = success .and. DH_AllocInit(NDecv, NSpar, ParSbk,'')
      success = success .and. DH_AllocInit(NDecv, N3Dpar, Par3D,'')
      success = success .and. DH_AllocInit(NDecv, N3par, Par3b ,'')
      success = success .and. DH_AllocInit(NDecv, NPpar, ParPre,'')
      success = success .and. DH_AllocInit(NDecv, NEpar, ParExt,'')
      success = success .and. DH_AllocInit(NDecv, NQpar, ParWQ ,'')
      success = success .and. DH_AllocInit(NDecv, NDpar, ParPar,'')
      success = success .and. DH_AllocInit(NDecv, 1    , ParInterp,'')
      success = success .and. DH_AllocInit(NDecv, NRpar, ParRsv,'')
      success = success .and. DH_AllocInit(NDecv, ParTyp,'Internal')

      If (.not. success)  then
         Call ErrMsg (929, 1, ' AllocDecisionArrays', ' ', IOUT1)
         RetVal = 929
         Return
      Endif

      success = DH_AllocInit(NDecv, Nspar, VarIdSbk,'')
      success = success .and. DH_AllocInit(NDecv, N3Dpar, VarId3D ,'')
      success = success .and. DH_AllocInit(NDecv, N3par, VarId3b ,'')
      success = success .and. DH_AllocInit(NDecv, Nppar, VarIdPre,'')
      success = success .and. DH_AllocInit(NDecv, Nepar, VarIdExt,'')
      success = success .and. DH_AllocInit(NDecv, Nqpar, VarIdWQ ,'')
      success = success .and. DH_AllocInit(NDecv, 1, VarIdInterpolationTable ,'')
      If (.not. success)  then
         Call ErrMsg (929, 1, ' AllocDecisionArrays', ' ', IOUT1)
         RetVal = 929
         Return
      Endif

      success = DH_AllocInit(NDecv, 9, ParDim,0)
      success = success .and. DH_AllocInit(9, MxTmShift,0)
      success = success .and. DH_AllocInit(NDecv, NrRsvOutputPar,0)
      success = success .and. DH_AllocInit(NDecv, Nspar, VarSbk,0)
      success = success .and. DH_AllocInit(NDecv, N3DPar, Var3D,0)
      success = success .and. DH_AllocInit(NDecv, N3par, Var3B ,0)
      success = success .and. DH_AllocInit(NDecv, Nppar, VarPre,0)
      success = success .and. DH_AllocInit(NDecv, Nepar, VarExt,0)
      success = success .and. DH_AllocInit(NDecv, Nqpar, VarWQ ,0)
      success = success .and. DH_AllocInit(NDecv, Nspar, TisSbk,0)
      success = success .and. DH_AllocInit(NDecv, N3Dpar, Tis3D ,0)
      success = success .and. DH_AllocInit(NDecv, N3par, Tis3B ,0)
      success = success .and. DH_AllocInit(NDecv, Nppar, TisPre,0)
      success = success .and. DH_AllocInit(NDecv, Nepar, TisExt,0)
      success = success .and. DH_AllocInit(NDecv, Nqpar, TisWQ ,0)
      success = success .and. DH_AllocInit(NDecv, Ndpar, TisPar,0)
      success = success .and. DH_AllocInit(NDecv, Nspar, IxSbHis,0)
      success = success .and. DH_AllocInit(NDecv, N3Dpar, Ix3DHis,0)
      success = success .and. DH_AllocInit(NDecv, N3par, Ix3BHis,0)
      success = success .and. DH_AllocInit(NDecv, Nppar, IxPrHis,0)
      success = success .and. DH_AllocInit(NDecv, Nepar, IxExHis,0)
      success = success .and. DH_AllocInit(NDecv, Nqpar, IxWQHis,0)
      success = success .and. DH_AllocInit(NDecv, 1, IxInterpTable,0)
      NTPAR = NSPar + N3par + Nppar + Nepar + Nqpar + N3DPar + NDPar + 1
      success = success .and. DH_AllocInit(NDecv, NTPAR, 2, ParOrder,0)
      Idim = max (NdPar, NrPar)
      success = success .and. DH_AllocInit(NDecv, idim, IxDPar ,0)
      success = success .and. DH_AllocInit(NDecv, ParOper ,0)
      success = success .and. DH_AllocInit(NDecv, RsvIndx ,0)
      success = success .and. DH_AllocInit(NDecv, Par3TableIndex ,0)
      success = success .and. DH_AllocInit(NDecv, SimSeq ,0)
      If (.not. success)  then
         Call ErrMsg (929, 1, ' AllocDecisionArrays', ' ', IOUT1)
         RetVal = 929
         Return
      EndIf
      success = DH_AllocInit(NDecv, Nspar, 2, CfSbk,0D0)
      success = success .and. DH_AllocInit(NDecv, N3Dpar, 2, Cf3D ,0D0)
      success = success .and. DH_AllocInit(NDecv, N3par, 2, Cf3b ,0D0)
      success = success .and. DH_AllocInit(NDecv, Nppar, 2, CfPre,0D0)
      success = success .and. DH_AllocInit(NDecv, Nepar, 2, CfExt,0D0)
      success = success .and. DH_AllocInit(NDecv, Nqpar, 2, CfWQ ,0D0)
      success = success .and. DH_AllocInit(NDecv, Ndpar, 2, CfPar,0D0)
      If (.not. success)  then
         Call ErrMsg (929, 1, ' AllocDecisionArrays', ' ', IOUT1)
         RetVal = 929
         Return
      EndIf

      success = DH_AllocInit(NDecv, NTims, DcvVal, 0D0)

      If (.not. success)  then
         Call ErrMsg (929, 1, ' AllocDecisionArrays', ' ', IOUT1)
         RetVal = 929
         Return
      EndIf



     Return
     End Function AllocDecisionArrays


    End Module DecisionModule
