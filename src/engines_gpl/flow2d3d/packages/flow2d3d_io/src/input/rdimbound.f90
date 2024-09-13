subroutine rdimbound(lundia, mmax, nmaxus, kmax, gdp)
!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2016.                                
!                                                                               
!  This program is free software: you can redistribute it and/or modify         
!  it under the terms of the GNU General Public License as published by         
!  the Free Software Foundation version 3.                                      
!                                                                               
!  This program is distributed in the hope that it will be useful,              
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                
!  GNU General Public License for more details.                                 
!                                                                               
!  You should have received a copy of the GNU General Public License            
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
!  $Id: rdimbound.f90 6742 2016-11-09 16:50:58Z canestrelli.x $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20160126_PLIC_VOF_bankEROSION/src/engines_gpl/flow2d3d/packages/io/src/input/rdimbound.f90 $
!!--description-----------------------------------------------------------------
!
!    Function: Reads the set of parameters and settings concerning the
!              Immersed Boundary Method for bank erosion from the MD-File
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use properties
    !
    use globaldata
    use string_module
    use dfparall
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    real(fp)                , pointer :: eps
    integer                 , pointer :: irov
    integer                 , pointer :: itis
    integer                 , pointer :: mfg
    integer                 , pointer :: nfg
    logical                 , pointer :: distr_qtq_per
    logical                 , pointer :: distr_bdl_per
    integer                 , pointer :: cutcell
    real(fp), dimension(:,:), pointer :: dpL
    real(fp), dimension(:,:), pointer :: dpH
    character(255)          , pointer :: filcc_in
    character(255)          , pointer :: filcc_out
    integer                 , pointer :: GHOSTimpl
    integer                 , pointer :: GhostMethod
    integer                 , pointer :: iDEBUGcut
    integer                 , pointer :: iDEBUGcutINI
    integer                 , pointer :: iDEBUGcutFIN
    integer                 , pointer :: IstencBANKer
    real(fp)                , pointer :: percEDGE
    integer                 , pointer :: ERODsubmBANKS
    integer                 , pointer :: idebugCUThardINI
    integer                 , pointer :: idebugCUThardFIN
    integer                 , pointer :: idebugCUThard
    integer                 , pointer :: int0comp1_s1SMALLcut
    integer                 , pointer :: kFLcutEQ1
    integer                 , pointer :: continuity_cc
    integer                 , pointer :: free_S1_sud
    integer                 , pointer :: cutBC
    integer                 , pointer :: extrapGHOST1fluid2
    integer                 , pointer :: doNOTdebugGHOSTS
    real(fp)                , pointer :: THRESextCUTedge
    logical                 , pointer :: massBALhdt
    real(fp)                , pointer :: THRESsmallCELL
    logical                 , pointer :: massbalLOC
    real(fp)                , pointer :: facws
    real(fp)                , pointer :: THRlocalMASSbal
    logical                 , pointer :: PRINTedgeVEL
    logical                 , pointer :: printGHOSTmap
    logical                 , pointer :: floodplain_inflow
    logical                 , pointer :: UavWETtau
    logical                 , pointer :: EXACTpolygons
    logical                 , pointer :: printINTERMghost
    integer                 , pointer :: iprintINTERMghost01
    logical                 , pointer :: printUZDITERghost
    logical                 , pointer :: printSUDITERghost
    integer                 , pointer :: TYPEfreeSLIP
    logical                 , pointer :: periodSURFACE
    logical                 , pointer :: PERIODICwaterDEPTH
    integer                 , pointer :: nrPER
    integer, dimension(:,:) , pointer :: iPERs1
    real(fp)                , pointer :: perSMOfac
    logical                 , pointer :: interpS1beforeUZD
    logical                 , pointer :: IGNOREwrongDEPTH
    logical                 , pointer :: bndBEDfromFILE
    integer                 , pointer :: typeEXTRAPstencil
    logical                 , pointer :: TRANSVperIMPL
    integer                 , pointer :: PERIODalongM
    logical                 , pointer :: EXCLouterVEL
    logical                 , pointer :: WAQUAfullyCENTRED
    logical                 , pointer :: twoCELLSperiod
    logical                 , pointer :: CYCLICtransCENTR
    integer, dimension(:)   , pointer :: mPH_ext
    integer, dimension(:)   , pointer :: nPH_ext
    integer, dimension(:)   , pointer :: mPQ_ext
    integer, dimension(:)   , pointer :: nPQ_ext
    logical                 , pointer :: FORCEuAThPERbnd
    logical                 , pointer :: PERIODICtangVEL
    logical                 , pointer :: PERIODICorthVEL
    logical                 , pointer :: ITERATEfree
    logical                 , pointer :: callSUBR_WATERlevelPERIOD
    logical                 , pointer :: periodGHOST
    real(fp)                , pointer :: reltim_qtq
    logical                 , pointer :: corrSURFslopeSUD
    logical                 , pointer :: originalMOMENTUM
    logical                 , pointer :: freeU0fixed
    logical                 , pointer :: bedLOADper
    logical                 , pointer :: smoCURV
    logical                 , pointer :: constSOLUTION
    logical                 , pointer :: virtualMERGEupdBED
    real(fp)                , pointer :: reltim_qtq_bdl
    integer                 , pointer :: typeVIRTmergeUPDbed
    logical                 , pointer :: EXACTcurv
    logical                 , pointer :: AVvelCUT
    logical                 , pointer :: virtualMERGEupdVERT
    integer                 , pointer :: typeVIRTmergeUPDvert
    logical                 , pointer :: HORIZviscZERO
    logical                 , pointer :: bedPERIODIC
    logical                 , pointer :: useCUTstyle
    logical                 , pointer :: corrSURFslopeUZD
    real(fp)                , pointer :: reltim_s1
    logical                 , pointer :: virtualMERGEupdDEPTH
    integer                 , pointer :: typeVIRTmergeUPDdepth
    real(fp)                , pointer :: thresMERGE_w
    real(fp)                , pointer :: thresMERGE_d
    real(fp)                , pointer :: thresMERGE_zb
    logical                 , pointer :: RECdepth
    logical                 , pointer :: SECordVEL
    logical                 , pointer :: SECordLEVEL
    logical                 , pointer :: prescrDEPTH
    logical                 , pointer :: bedUPDandFIXdepth
    logical                 , pointer :: use_DPSavg_for_qtot
    logical                 , pointer :: skip_aval_adjust
    logical                 , pointer :: forceCHEZYtransp
    logical                 , pointer :: ignoreMUmeyer
    logical                 , pointer :: zavg_global
    real(fp)                , pointer :: perSMOfac_Qb
    logical                 , pointer :: distr_qtq_bdl_NNprism
    integer                 , pointer :: CORRbedSLOPEcut
    real(fp), dimension(:,:), pointer :: poros
    integer                 , pointer :: exactSLOPE
    logical                 , pointer :: bnd_distr_perC
    real(fp)                , pointer :: reltim_qtq_C
    logical                 , pointer :: suspLOADper
    logical                 , pointer :: compHALFDTss
    logical                 , pointer :: suspCONCper
    real(fp)                , pointer :: ratio_ca_c2d
    logical                 , pointer :: moveEDtoBED
    integer                 , pointer :: simpleVR84
    logical                 , pointer :: prescVR93refHEIGHT
    logical                 , pointer :: ratioVR84
    logical                 , pointer :: consistency_ce_Cav
    real(fp)                , pointer :: uAXIS
    real(fp)                , pointer :: hAXIS
    logical                 , pointer :: testGHOSTaccur
    real(fp)                , pointer :: tolFREEexact
    logical                 , pointer :: interpVinUexact
    logical                 , pointer :: shift_xycor
    real(fp)                , pointer :: DISSghost
    real(fp)                , pointer :: threshVELghost
    integer                 , pointer :: MODadvecGHOSTsud
    integer                 , pointer :: MODadvecGHOSTuzd
    logical                 , pointer :: getADJACENTgrad
    logical                 , pointer :: changeKFUVcut
    logical                 , pointer :: deactGHOST_smallcut
    logical                 , pointer :: dontRESETghost
    logical                 , pointer :: RESETV1TOV0
    integer                 , pointer :: typeHART
    real(fp)                , pointer :: Kbank
    real(fp)                , pointer :: TAUcrBANKcnst
    integer                 , pointer :: TYPEtauBANK
    integer                 , pointer :: TYPEdistrBANKerod
    logical                 , pointer :: EXACTpolygonsONLYfirst
    integer                 , pointer :: TYPEtauCRbank
    logical                 , pointer :: virtualMERGEdisch
    logical                 , pointer :: neuPERslope
    logical                 , pointer :: FORCEdisch   
    logical                 , pointer :: noFLOODINGbanks
    integer                 , pointer :: typeVEGencr
    real(fp)                , pointer :: ELEVencr
    real(fp)                , pointer :: thresMERGE_Q
    real(fp)                , pointer :: tmorB
    logical                 , pointer :: printFLUXuv
    logical                 , pointer :: USEfixedBEDequilQS
    logical                 , pointer :: USEfixedBEDequilQb
    integer                 , pointer :: TYPEinterpVELcurv
    real(fp)                , pointer :: thresCURVcut
    logical                 , pointer :: noCORfacCURV
    integer                 , pointer :: typeCOMPcurvSMALL
    integer                 , pointer :: TYPEangleCURV
    integer                 , pointer :: PREsmoothVELOCcurv
    integer                 , pointer :: NsmoCURV
    logical                 , pointer :: printCURV
    logical                 , pointer :: includeSMALLforCURV
    integer                 , pointer :: HOWmanyPOINTSforCURV
    logical                 , pointer :: CURVboogaard
    logical                 , pointer :: virtualMERGEupdCONC
    logical                 , pointer :: HORIZdiffZERO
    logical                 , pointer :: virtualLINK
    logical                 , pointer :: linkMINarea
    integer                 , pointer :: analyticalPOLY
    real(fp)                , pointer :: R1_anal
    real(fp)                , pointer :: R2_anal
    logical                 , pointer :: useFULL
    logical                 , pointer :: constSOLforCHECKmomTERM
    logical                 , pointer :: DOUBLEuvh
    real(fp)                , pointer :: DELAYfixedBEDequilQS
    logical                 , pointer :: hindered
    logical                 , pointer :: prescVR93settl
    logical                 , pointer :: modDWNVEL
    integer                 , pointer :: removeW1qzk
    real(fp)                , pointer :: fracBANKsuspWASH
    real(fp)                , pointer :: fracBANKdepos
    logical                 , pointer :: DEPOSbankMATERIAL
    logical                 , pointer :: bdslpINupwnbed
    real(fp)                , pointer :: facMERGElink
    integer                 , pointer :: SMOOTHbankVEL
    integer                 , pointer :: SMOOTHbankSHEAR
    integer                 , pointer :: BOUNDvof
    integer                 , pointer :: smoothEb
    logical                 , pointer :: virtuallinkSMOw1   
    logical                 , pointer :: PRINTbalanceUZD
    real(fp)                , pointer :: thrPRINTerrGRAD
    integer                 , pointer :: analSUDcenterACTIVE
    logical                 , pointer :: SingleLOOPuzd
    integer                 , pointer :: freeNONhomo
    integer                 , pointer :: subtypeTESTghost
    logical                 , pointer :: FORCEexplicitUPDATEs1

    integer                 , pointer :: NanglesANALcircle_FIXED
    integer                 , pointer :: NanglesANALcircle
    logical                 , pointer :: precisePOROSbaric
    integer                 , pointer :: VERSIONprecisePOROSbaric
    logical                 , pointer :: activeNEVERghost  
    real(fp)                , pointer :: THRdepVEGET
    real(fp)                , pointer :: timeFORenchr
    real(fp)                , pointer :: timeFORdisrupt
    logical                 , pointer :: gradDEFER3orderDIFF
    logical                 , pointer :: onlyUZD
    logical                 , pointer :: noUZD   
    logical                 , pointer :: cntrUZDbnd_n
    logical                 , pointer :: cntrUZDbnd_m
    logical                 , pointer :: analDEFERR
    integer                 , pointer :: typeHUDPU
    real(fp)                , pointer :: maxVELfac
    real(fp)                , pointer :: minVELfac
    integer                 , pointer :: typeEXTRAPux
    logical                 , pointer :: implDEFsud
    logical                 , pointer :: partIMPLgrad     
    logical                 , pointer :: SUDtoCONVERGENCE
    real(fp)                , pointer :: epsSUD
    real(fp), dimension(:,:,:)    , pointer :: INTx_GRS
    real(fp), dimension(:,:,:)    , pointer :: INTy_GRS
    real(fp), dimension(:,:,:)    , pointer :: INTwx_GRS
    real(fp), dimension(:,:,:)    , pointer :: INTwy_GRS
    real(fp), dimension(:,:)      , pointer :: Nx
    real(fp), dimension(:,:)      , pointer :: Ny
    real(fp), dimension(:,:,:)    , pointer :: u1_FLLYghst
    real(fp), dimension(:,:,:)    , pointer :: v1_FLLYghst
    real(fp), dimension(:,:)      , pointer :: xG_L
    real(fp), dimension(:,:)      , pointer :: xG_H
    real(fp), dimension(:,:)      , pointer :: yG_L
    real(fp), dimension(:,:)      , pointer :: yG_H
    logical                 , pointer :: vvvSECord
!    
!   start IBM_research variables, most of them will be eventually removed
!    
    logical                 , pointer :: DPUhuSECONDorder
    logical                 , pointer :: noCUTfac
    logical                 , pointer :: FORCEs1CIRCanal
    logical                 , pointer :: FORCEgradS1_sud
    logical                 , pointer :: FORCEgradS1_uzd
    logical                 , pointer :: FORCEu1_sud
    logical                 , pointer :: FORCEu1_uzd
    logical                 , pointer :: FORCEududx_uzd
    logical                 , pointer :: FORCEvdudy_uzd
    logical                 , pointer :: FORCEududx_sud
    logical                 , pointer :: FORCEvdudy_sud
    logical                 , pointer :: FORCEfrict_uzd
    logical                 , pointer :: FORCEfrict_sud
    logical                 , pointer :: forceU0_sud
    logical                 , pointer :: force_DPUhu
    logical                 , pointer :: force_QYK    
    logical                 , pointer :: FORCEghost_sud2
    logical                 , pointer :: FORCEghost_sud1
    logical                 , pointer :: FORCEghost_uzd2
    logical                 , pointer :: FORCEghost_uzd1   
    logical                 , pointer :: deferredS1sud
    logical                 , pointer :: deferredS1uzd    
    integer                 , pointer :: TYPEgradDEFERRsud
    integer                 , pointer :: TYPEgradDEFERRuzd
    integer                 , pointer :: TYPEpartIMPLgrad    
    logical                 , pointer :: vFACTORcutEDGESx
    logical                 , pointer :: vFACTORcutEDGESy    
    logical                 , pointer :: hu2SUDiter
    logical                 , pointer :: vFACsudITER  
    logical                 , pointer :: extrADVECTsud    
    logical                 , pointer :: forceVVVuzd 
    logical                 , pointer :: forceVVVsud  
    logical                 , pointer :: huRHS
    logical                 , pointer :: forceQYKallSTEPS
    logical                 , pointer :: forceANALforSTAGE2  
    logical                 , pointer :: skipBOUNDvelSUD 
    integer                 , pointer :: TYPEofFORCING
    logical                 , pointer :: forceEb 
    logical                 , pointer :: FORCEnormBIinFINDbi 
    logical                 , pointer :: forceN 
    logical                 , pointer :: FIXEDcoastBANKS    
!   end IBM_research
!
! end IBM_research variables
!
! Global variables
!
    integer                        , intent(in)  :: kmax   !  Description and declaration in esm_alloc_int.f90
    integer                                      :: lundia !  Description and declaration in inout.igs
    integer                        , intent(in)  :: mmax   !  Description and declaration in esm_alloc_int.f90
    integer                        , intent(in)  :: nmaxus !  Description and declaration in esm_alloc_int.f90
!
! Local variables
!
    integer                      :: k
    integer                      :: lunPER ! Unit number of local scratch file for periodic locations
    integer       , external     :: newlun
    logical                      :: ex
    character(200)               :: txtput2
    character(60)                :: txtput1
    real(sp)                     :: percEDGE_sp
    real(sp)                     :: THRESextCUTedge_sp
    real(sp)                     :: THRlocalMASSbal_sp
    real(sp)                     :: THRESsmallCELL_sp
    real(sp)                     :: perSMOfac_sp
    real(sp)                     :: perSMOfac_Qb_sp
    real(sp)                     :: reltim_qtq_sp
    real(sp)                     :: reltim_qtq_C_sp
    real(sp)                     :: thresMERGE_d_sp
    real(sp)                     :: thresMERGE_w_sp
    real(sp)                     :: thresMERGE_zb_sp
    real(sp)                     :: reltim_S1_sp
    real(sp)                     :: reltim_qtq_bdl_sp
!
!
!! executable statements -------------------------------------------------------
!
    cutcell                   => gdp%gdimbound%cutcell
    dpL                       => gdp%gdimbound%dpL
    dpH                       => gdp%gdimbound%dpH
    filcc_in                  => gdp%gdimbound%filcc_in
    filcc_out                 => gdp%gdimbound%filcc_out
    GHOSTimpl                 => gdp%gdimbound%GHOSTimpl
    GhostMethod               => gdp%gdimbound%GhostMethod
    iDEBUGcut                 => gdp%gdimbound%iDEBUGcut
    iDEBUGcutINI              => gdp%gdimbound%iDEBUGcutINI
    iDEBUGcutFIN              => gdp%gdimbound%iDEBUGcutFIN
    IstencBANKer              => gdp%gdimbound%IstencBANKer
    percEDGE                  => gdp%gdimbound%percEDGE
    ERODsubmBANKS             => gdp%gdimbound%ERODsubmBANKS
    idebugCUThardINI          => gdp%gdimbound%idebugCUThardINI
    idebugCUThardFIN          => gdp%gdimbound%idebugCUThardFIN
    idebugCUThard             => gdp%gdimbound%idebugCUThard
    int0comp1_s1SMALLcut      => gdp%gdimbound%int0comp1_s1SMALLcut
    kFLcutEQ1                 => gdp%gdimbound%kFLcutEQ1
    continuity_cc             => gdp%gdimbound%continuity_cc
    free_S1_sud               => gdp%gdimbound%free_S1_sud
    cutBC                     => gdp%gdimbound%cutBC
    extrapGHOST1fluid2        => gdp%gdimbound%extrapGHOST1fluid2
    doNOTdebugGHOSTS          => gdp%gdimbound%doNOTdebugGHOSTS
    THRESextCUTedge           => gdp%gdimbound%THRESextCUTedge
    massBALhdt                => gdp%gdimbound%massBALhdt
    THRESsmallCELL            => gdp%gdimbound%THRESsmallCELL
    massbalLOC                => gdp%gdimbound%massbalLOC
    facws                     => gdp%gdimbound%facws
    THRlocalMASSbal           => gdp%gdimbound%THRlocalMASSbal
    PRINTedgeVEL              => gdp%gdimbound%PRINTedgeVEL
    printGHOSTmap             => gdp%gdimbound%printGHOSTmap
    floodplain_inflow         => gdp%gdimbound%floodplain_inflow
    UavWETtau                 => gdp%gdimbound%UavWETtau
    EXACTpolygons             => gdp%gdimbound%EXACTpolygons
    printINTERMghost          => gdp%gdimbound%printINTERMghost
    iprintINTERMghost01       => gdp%gdimbound%iprintINTERMghost01
    printUZDITERghost         => gdp%gdimbound%printUZDITERghost
    printSUDITERghost         => gdp%gdimbound%printSUDITERghost
    TYPEfreeSLIP              => gdp%gdimbound%TYPEfreeSLIP
    periodSURFACE             => gdp%gdimbound%periodSURFACE
    PERIODICwaterDEPTH        => gdp%gdimbound%PERIODICwaterDEPTH
    nrPER                     => gdp%gdimbound%nrPER
    iPERs1                    => gdp%gdimbound%iPERs1
    perSMOfac                 => gdp%gdimbound%perSMOfac
    interpS1beforeUZD         => gdp%gdimbound%interpS1beforeUZD
    IGNOREwrongDEPTH          => gdp%gdimbound%IGNOREwrongDEPTH
    bndBEDfromFILE            => gdp%gdimbound%bndBEDfromFILE
    typeEXTRAPstencil         => gdp%gdimbound%typeEXTRAPstencil
    TRANSVperIMPL             => gdp%gdimbound%TRANSVperIMPL
    PERIODalongM              => gdp%gdimbound%PERIODalongM
    EXCLouterVEL              => gdp%gdimbound%EXCLouterVEL
    WAQUAfullyCENTRED         => gdp%gdimbound%WAQUAfullyCENTRED
    twoCELLSperiod            => gdp%gdimbound%twoCELLSperiod
    CYCLICtransCENTR          => gdp%gdimbound%CYCLICtransCENTR
    mPH_ext                   => gdp%gdimbound%mPH_ext
    nPH_ext                   => gdp%gdimbound%nPH_ext
    mPQ_ext                   => gdp%gdimbound%mPQ_ext
    nPQ_ext                   => gdp%gdimbound%nPQ_ext
    mPH_ext                   => gdp%gdimbound%mPH_ext
    FORCEuAThPERbnd           => gdp%gdimbound%FORCEuAThPERbnd
    PERIODICtangVEL           => gdp%gdimbound%PERIODICtangVEL
    PERIODICorthVEL           => gdp%gdimbound%PERIODICorthVEL
    ITERATEfree               => gdp%gdimbound%ITERATEfree
    callSUBR_WATERlevelPERIOD => gdp%gdimbound%callSUBR_WATERlevelPERIOD
    periodGHOST               => gdp%gdimbound%periodGHOST
    reltim_qtq                => gdp%gdimbound%reltim_qtq
    corrSURFslopeSUD          => gdp%gdimbound%corrSURFslopeSUD
    originalMOMENTUM          => gdp%gdimbound%originalMOMENTUM
    freeU0fixed               => gdp%gdimbound%freeU0fixed
    bedLOADper                => gdp%gdimbound%bedLOADper
    smoCURV                   => gdp%gdimbound%smoCURV
    constSOLUTION             => gdp%gdimbound%constSOLUTION
    virtualMERGEupdBED        => gdp%gdimbound%virtualMERGEupdBED
    reltim_qtq_bdl            => gdp%gdimbound%reltim_qtq_bdl
    typeVIRTmergeUPDbed       => gdp%gdimbound%typeVIRTmergeUPDbed
    EXACTcurv                 => gdp%gdimbound%EXACTcurv
    AVvelCUT                  => gdp%gdimbound%AVvelCUT
    virtualMERGEupdVERT       => gdp%gdimbound%virtualMERGEupdVERT
    typeVIRTmergeUPDvert      => gdp%gdimbound%typeVIRTmergeUPDvert
    HORIZviscZERO             => gdp%gdimbound%HORIZviscZERO
    bedPERIODIC               => gdp%gdimbound%bedPERIODIC
    useCUTstyle               => gdp%gdimbound%useCUTstyle
    corrSURFslopeUZD          => gdp%gdimbound%corrSURFslopeUZD
    reltim_s1                 => gdp%gdimbound%reltim_s1
    virtualMERGEupdDEPTH      => gdp%gdimbound%virtualMERGEupdDEPTH
    typeVIRTmergeUPDdepth     => gdp%gdimbound%typeVIRTmergeUPDdepth
    thresMERGE_w              => gdp%gdimbound%thresMERGE_w
    thresMERGE_d              => gdp%gdimbound%thresMERGE_d
    thresMERGE_zb             => gdp%gdimbound%thresMERGE_zb
    RECdepth                  => gdp%gdimbound%RECdepth
    SECordVEL                 => gdp%gdimbound%SECordVEL
    SECordLEVEL               => gdp%gdimbound%SECordLEVEL
    prescrDEPTH               => gdp%gdimbound%prescrDEPTH
    bedUPDandFIXdepth         => gdp%gdimbound%bedUPDandFIXdepth
    use_DPSavg_for_qtot       => gdp%gdimbound%use_DPSavg_for_qtot
    skip_aval_adjust          => gdp%gdimbound%skip_aval_adjust
    forceCHEZYtransp          => gdp%gdimbound%forceCHEZYtransp
    ignoreMUmeyer             => gdp%gdimbound%ignoreMUmeyer
    zavg_global               => gdp%gdimbound%zavg_global
    perSMOfac_Qb              => gdp%gdimbound%perSMOfac_Qb
    distr_qtq_bdl_NNprism     => gdp%gdimbound%distr_qtq_bdl_NNprism
    CORRbedSLOPEcut           => gdp%gdimbound%CORRbedSLOPEcut
    poros                     => gdp%gdimbound%poros
    exactSLOPE                => gdp%gdimbound%exactSLOPE
    bnd_distr_perC            => gdp%gdimbound%bnd_distr_perC
    reltim_qtq_C              => gdp%gdimbound%reltim_qtq_C
    suspLOADper               => gdp%gdimbound%suspLOADper
    compHALFDTss              => gdp%gdimbound%compHALFDTss
    suspCONCper               => gdp%gdimbound%suspCONCper
    ratio_ca_c2d              => gdp%gdimbound%ratio_ca_c2d
    moveEDtoBED               => gdp%gdimbound%moveEDtoBED
    simpleVR84                => gdp%gdimbound%simpleVR84
    prescVR93refHEIGHT        => gdp%gdimbound%prescVR93refHEIGHT
    ratioVR84                 => gdp%gdimbound%ratioVR84
    consistency_ce_Cav        => gdp%gdimbound%consistency_ce_Cav
    uAXIS                     => gdp%gdimbound%uAXIS
    hAXIS                     => gdp%gdimbound%hAXIS
    testGHOSTaccur            => gdp%gdimbound%testGHOSTaccur
    tolFREEexact              => gdp%gdimbound%tolFREEexact
    interpVinUexact           => gdp%gdimbound%interpVinUexact
    shift_xycor               => gdp%gdimbound%shift_xycor
    DISSghost                 => gdp%gdimbound%DISSghost
    threshVELghost            => gdp%gdimbound%threshVELghost
    MODadvecGHOSTsud          => gdp%gdimbound%MODadvecGHOSTsud
    MODadvecGHOSTuzd          => gdp%gdimbound%MODadvecGHOSTuzd
    getADJACENTgrad           => gdp%gdimbound%getADJACENTgrad
    changeKFUVcut             => gdp%gdimbound%changeKFUVcut
    deactGHOST_smallcut       => gdp%gdimbound%deactGHOST_smallcut
    dontRESETghost            => gdp%gdimbound%dontRESETghost
    RESETV1TOV0               => gdp%gdimbound%RESETV1TOV0
    typeHART                  => gdp%gdimbound%typeHART
    Kbank                     => gdp%gdimbound%Kbank
    TAUcrBANKcnst             => gdp%gdimbound%TAUcrBANKcnst
    TYPEtauBANK               => gdp%gdimbound%TYPEtauBANK
    TYPEdistrBANKerod         => gdp%gdimbound%TYPEdistrBANKerod
    EXACTpolygonsONLYfirst    => gdp%gdimbound%EXACTpolygonsONLYfirst
    TYPEtauCRbank             => gdp%gdimbound%TYPEtauCRbank
    virtualMERGEdisch         => gdp%gdimbound%virtualMERGEdisch
    neuPERslope               => gdp%gdimbound%neuPERslope
    FORCEdisch                => gdp%gdimbound%FORCEdisch 
    noFLOODINGbanks           => gdp%gdimbound%noFLOODINGbanks
    typeVEGencr               => gdp%gdimbound%typeVEGencr
    ELEVencr                  => gdp%gdimbound%ELEVencr
    thresMERGE_Q              => gdp%gdimbound%thresMERGE_Q
    tmorB                     => gdp%gdimbound%tmorB
    printFLUXuv               => gdp%gdimbound%printFLUXuv
    USEfixedBEDequilQS        => gdp%gdimbound%USEfixedBEDequilQS
    USEfixedBEDequilQb        => gdp%gdimbound%USEfixedBEDequilQb
    TYPEinterpVELcurv         => gdp%gdimbound%TYPEinterpVELcurv
    thresCURVcut              => gdp%gdimbound%thresCURVcut
    noCORfacCURV              => gdp%gdimbound%noCORfacCURV
    typeCOMPcurvSMALL         => gdp%gdimbound%typeCOMPcurvSMALL
    TYPEangleCURV             => gdp%gdimbound%TYPEangleCURV
    PREsmoothVELOCcurv        => gdp%gdimbound%PREsmoothVELOCcurv
    NsmoCURV                  => gdp%gdimbound%NsmoCURV
    printCURV                 => gdp%gdimbound%printCURV
    includeSMALLforCURV       => gdp%gdimbound%includeSMALLforCURV
    HOWmanyPOINTSforCURV      => gdp%gdimbound%HOWmanyPOINTSforCURV
    CURVboogaard              => gdp%gdimbound%CURVboogaard
    virtualMERGEupdCONC       => gdp%gdimbound%virtualMERGEupdCONC
    HORIZdiffZERO             => gdp%gdimbound%HORIZdiffZERO
    virtualLINK               => gdp%gdimbound%virtualLINK
    linkMINarea               => gdp%gdimbound%linkMINarea
    analyticalPOLY            => gdp%gdimbound%analyticalPOLY
    R1_anal                   => gdp%gdimbound%R1_anal
    R2_anal                   => gdp%gdimbound%R2_anal
    useFULL                   => gdp%gdimbound%useFULL
    constSOLforCHECKmomTERM   => gdp%gdimbound%constSOLforCHECKmomTERM
    DOUBLEuvh                 => gdp%gdimbound%DOUBLEuvh
    DELAYfixedBEDequilQS      => gdp%gdimbound%DELAYfixedBEDequilQS
    hindered                  => gdp%gdimbound%hindered
    prescVR93settl            => gdp%gdimbound%prescVR93settl
    modDWNVEL                 => gdp%gdimbound%modDWNVEL
    removeW1qzk               => gdp%gdimbound%removeW1qzk
    fracBANKsuspWASH          => gdp%gdimbound%fracBANKsuspWASH
    fracBANKdepos             => gdp%gdimbound%fracBANKdepos
    DEPOSbankMATERIAL         => gdp%gdimbound%DEPOSbankMATERIAL
    bdslpINupwnbed            => gdp%gdimbound%bdslpINupwnbed
    facMERGElink              => gdp%gdimbound%facMERGElink
    SMOOTHbankVEL             => gdp%gdimbound%SMOOTHbankVEL
    SMOOTHbankSHEAR           => gdp%gdimbound%SMOOTHbankSHEAR
    BOUNDvof                  => gdp%gdimbound%BOUNDvof
    smoothEb                  => gdp%gdimbound%smoothEb
    virtuallinkSMOw1          => gdp%gdimbound%virtuallinkSMOw1  
    PRINTbalanceUZD           => gdp%gdimbound%PRINTbalanceUZD
    thrPRINTerrGRAD           => gdp%gdimbound%thrPRINTerrGRAD
    analSUDcenterACTIVE       => gdp%gdimbound%analSUDcenterACTIVE
    SingleLOOPuzd             => gdp%gdimbound%SingleLOOPuzd
    freeNONhomo               => gdp%gdimbound%freeNONhomo
    subtypeTESTghost          => gdp%gdimbound%subtypeTESTghost    
    FORCEexplicitUPDATEs1     => gdp%gdimbound%FORCEexplicitUPDATEs1       
    NanglesANALcircle_FIXED   => gdp%gdimbound%NanglesANALcircle_FIXED
    NanglesANALcircle         => gdp%gdimbound%NanglesANALcircle
    precisePOROSbaric         => gdp%gdimbound%precisePOROSbaric
    VERSIONprecisePOROSbaric  => gdp%gdimbound%VERSIONprecisePOROSbaric
    activeNEVERghost          => gdp%gdimbound%activeNEVERghost
    THRdepVEGET               => gdp%gdimbound%THRdepVEGET
    timeFORenchr              => gdp%gdimbound%timeFORenchr
    timeFORdisrupt            => gdp%gdimbound%timeFORdisrupt
    gradDEFER3orderDIFF       => gdp%gdimbound%gradDEFER3orderDIFF
    onlyUZD                   => gdp%gdimbound%onlyUZD
    noUZD                     => gdp%gdimbound%noUZD
    cntrUZDbnd_n              => gdp%gdimbound%cntrUZDbnd_n
    cntrUZDbnd_m              => gdp%gdimbound%cntrUZDbnd_m
    analDEFERR                => gdp%gdimbound%analDEFERR
    typeHUDPU                 => gdp%gdimbound%typeHUDPU
    maxVELfac                 => gdp%gdimbound%maxVELfac
    minVELfac                 => gdp%gdimbound%minVELfac
    typeEXTRAPux              => gdp%gdimbound%typeEXTRAPux
    implDEFsud                => gdp%gdimbound%implDEFsud
    partIMPLgrad              => gdp%gdimbound%partIMPLgrad    
    SUDtoCONVERGENCE          => gdp%gdimbound%SUDtoCONVERGENCE
    epsSUD                    => gdp%gdimbound%epsSUD
    vvvSECord                 => gdp%gdimbound%vvvSECord
    !   start IBM_research pointers, most of them will be eventually removed
    DPUhuSECONDorder          => gdp%gdimbound%DPUhuSECONDorder
    noCUTfac                  => gdp%gdimbound%noCUTfac
    FORCEs1CIRCanal           => gdp%gdimbound%FORCEs1CIRCanal
    FORCEgradS1_sud           => gdp%gdimbound%FORCEgradS1_sud
    FORCEgradS1_uzd           => gdp%gdimbound%FORCEgradS1_uzd
    FORCEu1_sud               => gdp%gdimbound%FORCEu1_sud
    FORCEu1_uzd               => gdp%gdimbound%FORCEu1_uzd
    FORCEududx_uzd            => gdp%gdimbound%FORCEududx_uzd
    FORCEvdudy_uzd            => gdp%gdimbound%FORCEvdudy_uzd
    FORCEududx_sud            => gdp%gdimbound%FORCEududx_sud
    FORCEvdudy_sud            => gdp%gdimbound%FORCEvdudy_sud
    FORCEfrict_uzd            => gdp%gdimbound%FORCEfrict_uzd
    FORCEfrict_sud            => gdp%gdimbound%FORCEfrict_sud
    force_QYK                 => gdp%gdimbound%force_QYK    
    FORCEghost_sud2           => gdp%gdimbound%FORCEghost_sud2
    FORCEghost_sud1           => gdp%gdimbound%FORCEghost_sud1
    FORCEghost_uzd2           => gdp%gdimbound%FORCEghost_uzd2
    FORCEghost_uzd1           => gdp%gdimbound%FORCEghost_uzd1
    forceVVVuzd               => gdp%gdimbound%forceVVVuzd
    forceVVVsud               => gdp%gdimbound%forceVVVsud    
    deferredS1sud             => gdp%gdimbound%deferredS1sud
    deferredS1uzd             => gdp%gdimbound%deferredS1uzd    
    TYPEgradDEFERRsud         => gdp%gdimbound%TYPEgradDEFERRsud
    TYPEgradDEFERRuzd         => gdp%gdimbound%TYPEgradDEFERRuzd
    TYPEpartIMPLgrad          => gdp%gdimbound%TYPEpartIMPLgrad    
    vFACTORcutEDGESx          => gdp%gdimbound%vFACTORcutEDGESx
    vFACTORcutEDGESy          => gdp%gdimbound%vFACTORcutEDGESy    
    hu2SUDiter                => gdp%gdimbound%hu2SUDiter
    vFACsudITER               => gdp%gdimbound%vFACsudITER    
    huRHS                     => gdp%gdimbound%huRHS    
    forceQYKallSTEPS          => gdp%gdimbound%forceQYKallSTEPS
    forceANALforSTAGE2        => gdp%gdimbound%forceANALforSTAGE2
    force_DPUhu               => gdp%gdimbound%force_DPUhu
    forceU0_sud               => gdp%gdimbound%forceU0_sud
    FORCEnormBIinFINDbi       => gdp%gdimbound%FORCEnormBIinFINDbi
    forceN                    => gdp%gdimbound%forceN
    forceEb                   => gdp%gdimbound%forceEb     
    extrADVECTsud             => gdp%gdimbound%extrADVECTsud       
    TYPEofFORCING             => gdp%gdimbound%TYPEofFORCING    
    skipBOUNDvelSUD           => gdp%gdimbound%skipBOUNDvelSUD   
    FIXEDcoastBANKS           => gdp%gdimbound%FIXEDcoastBANKS    
    !   end IBM_research
        
    eps              => gdp%gdconst%eps
    irov             => gdp%gdphysco%irov
    itis             => gdp%gdrdpara%itis
    mfg              => gdp%gdparall%mfg
    nfg              => gdp%gdparall%nfg
    distr_qtq_per    => gdp%gdbcdat%distr_qtq_per
    distr_bdl_per    => gdp%gdbcdat%distr_bdl_per
    !
    percEDGE_sp              =  0.1_fp               ! 0.99_sp
    perSMOfac_Qb_sp          =  1.0_sp                ! by default same smoothing of other hydrodynamic BC
    perSMOfac_sp             =  1.0_sp                ! by default same smoothing of other hydrodynamic BC
    reltim_qtq_bdl_sp        = -1.0_sp
    reltim_qtq_C_sp          = -1.0_sp
    reltim_qtq_sp            = -1.0_sp
    reltim_S1_sp             = -1.0_sp
    THRESextCUTedge_sp       =  0.5_sp
    thresMERGE_d_sp          =  0.5_sp
    thresMERGE_w_sp          =  0.5_sp
    thresMERGE_zb_sp         =  0.5_sp
    THRESsmallCELL_sp        =  0.00000000001_sp
    THRlocalMASSbal_sp       =  0.0000000000001_sp
    !
!   start IBM_research variables, most of them will be eventually removed
    call prop_get_logical(gdp%mdfile_ptr, '*', 'noCUTfac' , noCUTfac)
    call prop_get_logical(gdp%mdfile_ptr, '*', 'FORCEs1CIRCanal' , FORCEs1CIRCanal)
    call prop_get_logical(gdp%mdfile_ptr, '*', 'FORCEgradS1_sud' , FORCEgradS1_sud)
    call prop_get_logical(gdp%mdfile_ptr, '*', 'FORCEgradS1_uzd' , FORCEgradS1_uzd)
    call prop_get_logical(gdp%mdfile_ptr, '*', 'forceU0_sud'    , forceU0_sud)
    call prop_get_logical(gdp%mdfile_ptr, '*', 'force_DPUhu'    , force_DPUhu)
    call prop_get_logical(gdp%mdfile_ptr, '*', 'FORCEu1_sud'    , FORCEu1_sud)
    call prop_get_logical(gdp%mdfile_ptr, '*', 'FORCEu1_uzd'    , FORCEu1_uzd)  
    call prop_get_logical(gdp%mdfile_ptr, '*', 'force_QYK'    , force_QYK)     
    call prop_get_logical(gdp%mdfile_ptr, '*', 'forceQYKallSTEPS'    , forceQYKallSTEPS)     
    call prop_get_logical(gdp%mdfile_ptr, '*', 'FORCEfrict_sud'    , FORCEfrict_sud)
    call prop_get_logical(gdp%mdfile_ptr, '*', 'FORCEfrict_uzd'    , FORCEfrict_uzd)     
    call prop_get_logical(gdp%mdfile_ptr, '*', 'FORCEududx_uzd' , FORCEududx_uzd)
    call prop_get_logical(gdp%mdfile_ptr, '*', 'FORCEvdudy_uzd' , FORCEvdudy_uzd)
    call prop_get_logical(gdp%mdfile_ptr, '*', 'FORCEududx_sud' , FORCEududx_sud)
    call prop_get_logical(gdp%mdfile_ptr, '*', 'FORCEvdudy_sud' , FORCEvdudy_sud)
    call prop_get_logical(gdp%mdfile_ptr, '*', 'forceVVVuzd' , forceVVVuzd)
    call prop_get_logical(gdp%mdfile_ptr, '*', 'forceVVVsud' , forceVVVsud) 
    call prop_get_logical(gdp%mdfile_ptr, '*', 'implDEFsud' , implDEFsud)      
    call prop_get_logical(gdp%mdfile_ptr, '*', 'deferredS1uzd' , deferredS1uzd)
    call prop_get_logical(gdp%mdfile_ptr, '*', 'deferredS1sud' , deferredS1sud)     
    call prop_get_integer(gdp%mdfile_ptr, '*', 'TYPEgradDEFERRsud' , TYPEgradDEFERRsud)
    call prop_get_integer(gdp%mdfile_ptr, '*', 'TYPEgradDEFERRuzd' , TYPEgradDEFERRuzd)    
    call prop_get_integer(gdp%mdfile_ptr, '*', 'TYPEpartIMPLgrad' , TYPEpartIMPLgrad)
    call prop_get_logical(gdp%mdfile_ptr, '*', 'vFACTORcutEDGESx' , vFACTORcutEDGESx) 
    call prop_get_logical(gdp%mdfile_ptr, '*', 'vFACTORcutEDGESy' , vFACTORcutEDGESy)     
    call prop_get_logical(gdp%mdfile_ptr, '*', 'FORCEghost_sud2' , FORCEghost_sud2)
    call prop_get_logical(gdp%mdfile_ptr, '*', 'FORCEghost_sud1' , FORCEghost_sud1)
    call prop_get_logical(gdp%mdfile_ptr, '*', 'FORCEghost_uzd2' , FORCEghost_uzd2)
    call prop_get_logical(gdp%mdfile_ptr, '*', 'FORCEghost_uzd1' , FORCEghost_uzd1)
    call prop_get_logical(gdp%mdfile_ptr, '*', 'DPUhuSECONDorder' , DPUhuSECONDorder)
    call prop_get_logical(gdp%mdfile_ptr, '*', 'hu2SUDiter' , hu2SUDiter)
    call prop_get_logical(gdp%mdfile_ptr, '*', 'vFACsudITER' , vFACsudITER)    
    call prop_get_logical(gdp%mdfile_ptr, '*', 'huRHS' , huRHS)        
    call prop_get_logical(gdp%mdfile_ptr, '*', 'forceEb' , forceEb) 
    call prop_get_logical(gdp%mdfile_ptr, '*', 'forceN' , forceN) 
    call prop_get_logical(gdp%mdfile_ptr, '*', 'FORCEnormBIinFINDbi' , FORCEnormBIinFINDbi)    
    call prop_get_logical(gdp%mdfile_ptr, '*', 'forceANALforSTAGE2' , forceANALforSTAGE2)  
    call prop_get_logical(gdp%mdfile_ptr, '*', 'extrADVECTsud' , extrADVECTsud)    
    call prop_get_integer(gdp%mdfile_ptr, '*', 'TYPEofFORCING' , TYPEofFORCING)     
    call prop_get_logical(gdp%mdfile_ptr, '*', 'skipBOUNDvelSUD' , skipBOUNDvelSUD)   
    call prop_get_logical(gdp%mdfile_ptr, '*', 'FIXEDcoastBANKS' , FIXEDcoastBANKS)     
!   end IBM_research        

    call prop_get_integer(gdp%mdfile_ptr, '*', 'CutCell', cutcell)
    txtput1 = 'CutCell'
    if (cutcell==0) then
      txtput2 = '                 No cut-cells'
    elseif (cutcell==1) then
      txtput2 = '                 Cut cells with polygon'
    elseif (cutcell==2) then 
      txtput2 = '                 PLIC cut cells (Piecewise linear)'
    endif
    write (lundia, '(3a)') txtput1, ':', txtput2
    !
    ! If cutcell, allocate and initialize poros, needed in flow_nefis_restart if restart from map
    !
    if (cutcell > 0) then
       allocate(gdp%gdimbound%poros(gdp%d%nlb:gdp%d%nub,gdp%d%mlb:gdp%d%mub))
       gdp%gdimbound%poros = 0.0_fp
    endif
    !
    call prop_get_integer(gdp%mdfile_ptr, '*', 'continuity_cc', continuity_cc)
    txtput1 = 'Continuity eq. and cut cells'
    if (continuity_cc==0) then
      txtput2 = '                 No modification of continuity.'
    elseif (continuity_cc==1) then
      txtput2 = '                 active volumes and surfaces are modified.'
    else
      call prterr(lundia, 'U021', 'Value of continuity_cc not admitted')
      call d3stop(1, gdp)   
    endif
    write (lundia, '(3a)') txtput1, ':', txtput2
!
    call prop_get_integer(gdp%mdfile_ptr, '*', 'extrapGHOST1fluid2', extrapGHOST1fluid2)
    txtput1 = 'U and V small edges extrapolated from:'
    if (extrapGHOST1fluid2==1) then
      txtput2 = '                 ghost cell'
    elseif (extrapGHOST1fluid2==2) then
      txtput2 = '                 fluid cell'
    elseif (extrapGHOST1fluid2==0) then
      txtput2 = '                 Non-activated'
    elseif (extrapGHOST1fluid2==3) then
      txtput2 = '                 Non-activated, ghost not imposed it cut edge'
    else
      call prterr(lundia, 'U021', 'Value of extrapGHOST1fluid2 not admitted')
    !  call d3stop(1, gdp)   
    endif
    write (lundia, '(3a)') txtput1, ':', txtput2
!
    call prop_get_integer(gdp%mdfile_ptr, '*', 'doNOTdebugGHOSTS', doNOTdebugGHOSTS)
!
    call prop_get_integer(gdp%mdfile_ptr, '*', 'GhostMethod', GhostMethod)
    txtput1 = 'GhostMethod'
    if (GhostMethod==0) then
      txtput2 = '                 Ghost cell method is active in SUD and non-active in UZD. Cut cells for coupled momentum/continuity in SUD'
    elseif (GhostMethod==1) then
      txtput2 = '                 Ghost cell active in both SUD and UZD. Cut cells in SUD'
    elseif (GhostMethod==2) then
      txtput2 = '                 Ghost cell method is active in UZD and non-active in SUD. Cut cells for coupled momentum/continuity in SUD'
    elseif (GhostMethod==3) then
      txtput2 = '                 Ghost cell method never used. Only cut cells in continuity in SUD'
    else
      call prterr(lundia, 'U021', 'Value of GhostMethod not admitted')
      call d3stop(1, gdp)   
    endif
    write (lundia, '(3a)') txtput1, ':', txtput2
!
    call prop_get_integer(gdp%mdfile_ptr, '*', 'TYPEfreeSLIP', TYPEfreeSLIP)
    txtput1 = 'TYPEfreeSLIP'
    if (TYPEfreeSLIP==0) then
      txtput2 = '                 "Exact" free slip is prescribed on cut cells'
    elseif (TYPEfreeSLIP==1) then
      txtput2 ='                  Hartmann free slip with IP and BI is prescribed on cut cells'
    elseif (TYPEfreeSLIP==2) then
      txtput2 ='                  Simple Hartmann free slip is prescribed on cut cells'
    else
      call prterr(lundia, 'U021', 'Value of TYPEfreeSLIP not admitted')
      call d3stop(1, gdp)   
    endif
    write (lundia, '(3a)') txtput1, ':', txtput2

    if (cutcell.eq.0.and.GhostMethod.ne.0) then
      call prterr(lundia, 'U021', 'GhostMethod is active, cutcell must be >0!!')
      call d3stop(1, gdp)
      GhostMethod = 0 
      continuity_cc = 0
    endif
!
    call prop_get_real(gdp%mdfile_ptr, '*', 'percEDGE',percEDGE_sp)
    percEDGE = percEDGE_sp
    call prop_get_integer(gdp%mdfile_ptr, '*', 'IstencBANKer', IstencBANKer)
    if (IstencBANKer.LT.0.OR.IstencBANKer.GT.1) then
      call prterr(lundia, 'U021', 'IstencBANKer must be 0 or 1!!')
      call d3stop(1, gdp)
    endif
!
    call prop_get_integer(gdp%mdfile_ptr, '*', 'typeEXTRAPstencil', typeEXTRAPstencil)
!
    call prop_get_real(gdp%mdfile_ptr, '*', 'THRESextCUTedge',THRESextCUTedge_sp)
    THRESextCUTedge = THRESextCUTedge_sp
!
    call prop_get_real(gdp%mdfile_ptr, '*', 'THRESsmallCELL',THRESsmallCELL_sp)
    THRESsmallCELL = THRESsmallCELL_sp
!
    call prop_get_real(gdp%mdfile_ptr, '*', 'THRlocalMASSbal',THRlocalMASSbal_sp)
    THRlocalMASSbal = THRlocalMASSbal_sp
!
    call prop_get_real(gdp%mdfile_ptr, '*', 'perSMOfac',perSMOfac_sp)
    perSMOfac = perSMOfac_sp
!
    call prop_get_real(gdp%mdfile_ptr, '*', 'perSMOfac_Qb',perSMOfac_Qb_sp)
    perSMOfac_Qb = perSMOfac_Qb_sp
!    
    call prop_get_real(gdp%mdfile_ptr, '*', 'reltim_qtq',reltim_qtq_sp)
    reltim_qtq = reltim_qtq_sp
!
    call prop_get_real(gdp%mdfile_ptr, '*', 'reltim_qtq_C',reltim_qtq_C_sp)
    reltim_qtq_C = reltim_qtq_C_sp
!    
    call prop_get_real(gdp%mdfile_ptr, '*', 'reltim_s1',reltim_S1_sp)
    reltim_s1 = reltim_S1_sp
!    
    call prop_get_real(gdp%mdfile_ptr, '*', 'thresMERGE_w',thresMERGE_w_sp)
    thresMERGE_w = thresMERGE_w_sp
!    
    call prop_get_real(gdp%mdfile_ptr, '*', 'thresMERGE_d',thresMERGE_d_sp)
    thresMERGE_d = thresMERGE_d_sp
!
    call prop_get_real(gdp%mdfile_ptr, '*', 'thresMERGE_zb',thresMERGE_zb_sp)
    thresMERGE_zb = thresMERGE_zb_sp
!
    call prop_get_real(gdp%mdfile_ptr, '*', 'reltim_qtq_bdl',reltim_qtq_bdl_sp)
    reltim_qtq_bdl = reltim_qtq_bdl_sp
!
    call prop_get(gdp%mdfile_ptr, '*', 'epsCONVvel',eps)
    call prop_get(gdp%mdfile_ptr, '*', 'thresMERGE_Q',thresMERGE_Q)
    call prop_get(gdp%mdfile_ptr, '*', 'ratio_ca_c2d',ratio_ca_c2d)
    call prop_get(gdp%mdfile_ptr, '*', 'uAXIS',uAXIS)   
    call prop_get(gdp%mdfile_ptr, '*', 'hAXIS',hAXIS) 
    call prop_get(gdp%mdfile_ptr, '*', 'tolFREEexact',tolFREEexact) 
    call prop_get(gdp%mdfile_ptr, '*', 'epsSUD',epsSUD) 
    call prop_get(gdp%mdfile_ptr, '*', 'DISSghost',DISSghost) 
    call prop_get(gdp%mdfile_ptr, '*', 'threshVELghost',threshVELghost) 
    call prop_get(gdp%mdfile_ptr, '*', 'Kbank',Kbank) 
    call prop_get(gdp%mdfile_ptr, '*', 'TAUcrBANKcnst',TAUcrBANKcnst) 
    call prop_get(gdp%mdfile_ptr, '*', 'ELEVencr',ELEVencr)
    call prop_get(gdp%mdfile_ptr, '*', 'THRdepVEGET',THRdepVEGET)
    call prop_get(gdp%mdfile_ptr, '*', 'timeFORenchr',timeFORenchr)
    call prop_get(gdp%mdfile_ptr, '*', 'timeFORdisrupt',timeFORdisrupt)                      
    call prop_get(gdp%mdfile_ptr, '*', 'tmorB',tmorB)
    call prop_get(gdp%mdfile_ptr, '*', 'thresCURVcut',thresCURVcut)
    call prop_get(gdp%mdfile_ptr, '*', 'DELAYfixedBEDequilQS',DELAYfixedBEDequilQS)
    call prop_get(gdp%mdfile_ptr, '*', 'facMERGElink',facMERGElink)
    call prop_get(gdp%mdfile_ptr, '*', 'thrPRINTerrGRAD',thrPRINTerrGRAD)
    call prop_get(gdp%mdfile_ptr, '*', 'maxVELfac',maxVELfac)
    call prop_get(gdp%mdfile_ptr, '*', 'minVELfac',minVELfac)
    if (comparereal(abs(facMERGElink)-1._fp,0.00000001_fp)>1) then
       write(*,*) 'facMERGElink should be 1 (it has to be removed)'
       call d3stop(1, gdp)
    endif
    call prop_get(gdp%mdfile_ptr, '*', 'fracBANKsuspWASH',fracBANKsuspWASH)      
    call prop_get(gdp%mdfile_ptr, '*', 'fracBANKdepos',fracBANKdepos)  
!
    call prop_get_integer(gdp%mdfile_ptr, '*', 'MODadvecGHOSTsud' , MODadvecGHOSTsud)
    call prop_get_integer(gdp%mdfile_ptr, '*', 'MODadvecGHOSTuzd' , MODadvecGHOSTuzd)
    if ((irov==1.or.irov==2).and.(MODadvecGHOSTsud/=0.or.MODadvecGHOSTuzd/=0)) then
       write(*,*) 'MODadvecGHOSTsud/MODadvecGHOSTuzd have to be zero for no slip'
       call d3stop(1, gdp)
    endif
    call prop_get_integer(gdp%mdfile_ptr, '*', 'typeVEGencr' , typeVEGencr)
    call prop_get_integer(gdp%mdfile_ptr, '*', 'TYPEinterpVELcurv' , TYPEinterpVELcurv)
    call prop_get_integer(gdp%mdfile_ptr, '*', 'typeCOMPcurvSMALL' , typeCOMPcurvSMALL)
    call prop_get_integer(gdp%mdfile_ptr, '*', 'PREsmoothVELOCcurv' , PREsmoothVELOCcurv)
    call prop_get_integer(gdp%mdfile_ptr, '*', 'NsmoCURV' , NsmoCURV)
    call prop_get_integer(gdp%mdfile_ptr, '*', 'HOWmanyPOINTSforCURV' , HOWmanyPOINTSforCURV)
    call prop_get_integer(gdp%mdfile_ptr, '*', 'typeEXTRAPux' , typeEXTRAPux)
    call prop_get_integer(gdp%mdfile_ptr, '*', 'NanglesANALcircle_FIXED' , NanglesANALcircle_FIXED)
    call prop_get_integer(gdp%mdfile_ptr, '*', 'NanglesANALcircle' , NanglesANALcircle)
    call prop_get_integer(gdp%mdfile_ptr, '*', 'TYPEangleCURV' , TYPEangleCURV)
    call prop_get_integer(gdp%mdfile_ptr, '*', 'int0comp1_s1SMALLcut', int0comp1_s1SMALLcut)
    call prop_get_integer(gdp%mdfile_ptr, '*', 'iDEBUGcut', iDEBUGcut)
    call prop_get_integer(gdp%mdfile_ptr, '*', 'iDEBUGcutINI', iDEBUGcutINI)
    call prop_get_integer(gdp%mdfile_ptr, '*', 'iDEBUGcutFIN', iDEBUGcutFIN)
    call prop_get_integer(gdp%mdfile_ptr, '*', 'idebugCUThard', idebugCUThard)
    call prop_get_integer(gdp%mdfile_ptr, '*', 'idebugCUThardINI', idebugCUThardINI)
    call prop_get_integer(gdp%mdfile_ptr, '*', 'idebugCUThardFIN', idebugCUThardFIN)
    call prop_get_integer(gdp%mdfile_ptr, '*', 'ERODsubmBANKS', ERODsubmBANKS)
    call prop_get_integer(gdp%mdfile_ptr, '*', 'kFLcutEQ1', kFLcutEQ1)
    call prop_get_integer(gdp%mdfile_ptr, '*', 'TYPEtauCRbank', TYPEtauCRbank)
    call prop_get_integer(gdp%mdfile_ptr, '*', 'free_S1_sud', free_S1_sud)
    call prop_get_integer(gdp%mdfile_ptr, '*', 'cutBC', cutBC)
    call prop_get_integer(gdp%mdfile_ptr, '*', 'BOUNDvof', BOUNDvof)
    call prop_get_integer(gdp%mdfile_ptr, '*', 'iprintINTERMghost01', iprintINTERMghost01)
    call prop_get_integer(gdp%mdfile_ptr, '*', 'typeVIRTmergeUPDbed' , typeVIRTmergeUPDbed)
    call prop_get_integer(gdp%mdfile_ptr, '*', 'typeVIRTmergeUPDvert' , typeVIRTmergeUPDvert)
    call prop_get_integer(gdp%mdfile_ptr, '*', 'typeVIRTmergeUPDdepth' , typeVIRTmergeUPDdepth)
    call prop_get_integer(gdp%mdfile_ptr, '*', 'typeHART' , typeHART)

    call prop_get_integer(gdp%mdfile_ptr, '*', 'CORRbedSLOPEcut' , CORRbedSLOPEcut)
    call prop_get_integer(gdp%mdfile_ptr, '*', 'exactSLOPE' , exactSLOPE)
    call prop_get_integer(gdp%mdfile_ptr, '*', 'simpleVR84' , simpleVR84) 
    call prop_get_integer(gdp%mdfile_ptr, '*', 'TYPEtauBANK' , TYPEtauBANK)
    call prop_get_integer(gdp%mdfile_ptr, '*', 'TYPEdistrBANKerod' , TYPEdistrBANKerod)
    call prop_get_integer(gdp%mdfile_ptr, '*', 'analyticalPOLY' , analyticalPOLY)
    call prop_get_integer(gdp%mdfile_ptr, '*', 'VERSIONprecisePOROSbaric' , VERSIONprecisePOROSbaric)
    call prop_get_integer(gdp%mdfile_ptr, '*', 'removeW1qzk' , removeW1qzk) 
    call prop_get_integer(gdp%mdfile_ptr, '*', 'SMOOTHbankVEL' , SMOOTHbankVEL) 
    call prop_get_integer(gdp%mdfile_ptr, '*', 'analSUDcenterACTIVE' , analSUDcenterACTIVE)  
    call prop_get_integer(gdp%mdfile_ptr, '*', 'freeNONhomo' , freeNONhomo)  
    call prop_get_integer(gdp%mdfile_ptr, '*', 'subtypeTESTghost' , subtypeTESTghost) 
    call prop_get_integer(gdp%mdfile_ptr, '*', 'SMOOTHbankSHEAR' , SMOOTHbankSHEAR)
    call prop_get_integer(gdp%mdfile_ptr, '*', 'smoothEb' , smoothEb)
    call prop_get_integer(gdp%mdfile_ptr, '*', 'typeHUDPU' , typeHUDPU)
    if (analyticalPOLY>0) then
       if (analyticalPOLY==1) then
          call prop_get(gdp%mdfile_ptr, '*', 'R1_anal',R1_anal)
          call prop_get(gdp%mdfile_ptr, '*', 'R2_anal',R2_anal)
          if (R1_anal<0.or.R2_anal.lt.0) then
             write(*,*) 'Provide R1_anal and R2_anal'
             call d3stop(1, gdp)
          endif
       endif
    endif
!
    call prop_get_logical(gdp%mdfile_ptr, '*', 'massBALhdt' , massBALhdt) 
    call prop_get_logical(gdp%mdfile_ptr, '*', 'bnd_distr_perC' , bnd_distr_perC) 
    call prop_get_logical(gdp%mdfile_ptr, '*', 'DOUBLEuvh' , DOUBLEuvh) 
    call prop_get_logical(gdp%mdfile_ptr, '*', 'massbalLOC' , massbalLOC) 
    call prop_get_logical(gdp%mdfile_ptr, '*', 'floodplain_inflow' , floodplain_inflow) 
    call prop_get_logical(gdp%mdfile_ptr, '*', 'UavWETtau' , UavWETtau) 
    call prop_get_logical(gdp%mdfile_ptr, '*', 'DEPOSbankMATERIAL' , DEPOSbankMATERIAL)
    call prop_get_logical(gdp%mdfile_ptr, '*', 'IGNOREwrongDEPTH' , IGNOREwrongDEPTH) 
    call prop_get_logical(gdp%mdfile_ptr, '*', 'PRINTedgeVEL' , PRINTedgeVEL) 
    call prop_get_logical(gdp%mdfile_ptr, '*', 'interpS1beforeUZD' , interpS1beforeUZD) 
    call prop_get_logical(gdp%mdfile_ptr, '*', 'printGHOSTmap' , printGHOSTmap)
    call prop_get_logical(gdp%mdfile_ptr, '*', 'EXACTpolygons' , EXACTpolygons)
    call prop_get_logical(gdp%mdfile_ptr, '*', 'EXACTpolygonsONLYfirst' , EXACTpolygonsONLYfirst) 
    call prop_get_logical(gdp%mdfile_ptr, '*', 'printINTERMghost' , printINTERMghost)
    call prop_get_logical(gdp%mdfile_ptr, '*', 'printSUDITERghost' , printSUDITERghost)
    call prop_get_logical(gdp%mdfile_ptr, '*', 'printUZDITERghost' , printUZDITERghost)
    call prop_get_logical(gdp%mdfile_ptr, '*', 'periodSURFACE' , periodSURFACE) 
    call prop_get_logical(gdp%mdfile_ptr, '*', 'neuPERslope' , neuPERslope) 
    call prop_get_logical(gdp%mdfile_ptr, '*', 'FORCEdisch' , FORCEdisch) 
    call prop_get_logical(gdp%mdfile_ptr, '*', 'noFLOODINGbanks' , noFLOODINGbanks) 
    call prop_get_logical(gdp%mdfile_ptr, '*', 'printFLUXuv' , printFLUXuv) 
    call prop_get_logical(gdp%mdfile_ptr, '*', 'noCORfacCURV' , noCORfacCURV) 
    call prop_get_logical(gdp%mdfile_ptr, '*', 'printCURV' , printCURV)  
    call prop_get_logical(gdp%mdfile_ptr, '*', 'USEfixedBEDequilQS' , USEfixedBEDequilQS) 
    call prop_get_logical(gdp%mdfile_ptr, '*', 'USEfixedBEDequilQb' , USEfixedBEDequilQb) 
    call prop_get_logical(gdp%mdfile_ptr, '*', 'CURVboogaard' , CURVboogaard) 
    call prop_get_logical(gdp%mdfile_ptr, '*', 'useFULL' , useFULL) 
    call prop_get_logical(gdp%mdfile_ptr, '*', 'constSOLforCHECKmomTERM' , constSOLforCHECKmomTERM)
    call prop_get_logical(gdp%mdfile_ptr, '*', 'prescVR93settl' , prescVR93settl)
    call prop_get_logical(gdp%mdfile_ptr, '*', 'modDWNVEL' , modDWNVEL)  
    call prop_get_logical(gdp%mdfile_ptr, '*', 'PRINTbalanceUZD' , PRINTbalanceUZD)
    call prop_get_logical(gdp%mdfile_ptr, '*', 'SingleLOOPuzd' , SingleLOOPuzd)
    call prop_get_logical(gdp%mdfile_ptr, '*', 'FORCEexplicitUPDATEs1' , FORCEexplicitUPDATEs1)

    call prop_get_logical(gdp%mdfile_ptr, '*', 'SUDtoCONVERGENCE' , SUDtoCONVERGENCE)
    call prop_get_logical(gdp%mdfile_ptr, '*', 'precisePOROSbaric' , precisePOROSbaric)
    call prop_get_logical(gdp%mdfile_ptr, '*', 'activeNEVERghost' , activeNEVERghost)
    call prop_get_logical(gdp%mdfile_ptr, '*', 'hindered' , hindered)
    call prop_get_logical(gdp%mdfile_ptr, '*', 'PERIODICwaterDEPTH' , PERIODICwaterDEPTH) 
    call prop_get_logical(gdp%mdfile_ptr, '*', 'periodGHOST' , periodGHOST) 
    call prop_get_logical(gdp%mdfile_ptr, '*', 'callSUBR_WATERlevelPERIOD' , callSUBR_WATERlevelPERIOD) 
    call prop_get_logical(gdp%mdfile_ptr, '*', 'PERIODICtangVEL' , PERIODICtangVEL)
    call prop_get_logical(gdp%mdfile_ptr, '*', 'PERIODICorthVEL' , PERIODICorthVEL)
    call prop_get_logical(gdp%mdfile_ptr, '*', 'twoCELLSperiod' , twoCELLSperiod) 
    call prop_get_logical(gdp%mdfile_ptr, '*', 'smoCURV' , smoCURV) 
    call prop_get_logical(gdp%mdfile_ptr, '*', 'constSOLUTION' , constSOLUTION)
    call prop_get_logical(gdp%mdfile_ptr, '*', 'testGHOSTaccur' , testGHOSTaccur) 
    call prop_get_logical(gdp%mdfile_ptr, '*', 'interpVinUexact' , interpVinUexact) 
    call prop_get_logical(gdp%mdfile_ptr, '*', 'shift_xycor' , shift_xycor) 
    call prop_get_logical(gdp%mdfile_ptr, '*', 'AVvelCUT' , AVvelCUT) 
    call prop_get_logical(gdp%mdfile_ptr, '*', 'EXACTcurv' , EXACTcurv)
    call prop_get_logical(gdp%mdfile_ptr, '*', 'includeSMALLforCURV' , includeSMALLforCURV)
    call prop_get_logical(gdp%mdfile_ptr, '*', 'useCUTstyle' , useCUTstyle)
    call prop_get_logical(gdp%mdfile_ptr, '*', 'HORIZviscZERO' , HORIZviscZERO)
    call prop_get_logical(gdp%mdfile_ptr, '*', 'HORIZdiffZERO' , HORIZdiffZERO)
    call prop_get_logical(gdp%mdfile_ptr, '*', 'bedPERIODIC' , bedPERIODIC) 
    call prop_get_logical(gdp%mdfile_ptr, '*', 'prescrDEPTH' , prescrDEPTH) 
    call prop_get_logical(gdp%mdfile_ptr, '*', 'bedUPDandFIXdepth' , bedUPDandFIXdepth) 
    call prop_get_logical(gdp%mdfile_ptr, '*', 'use_DPSavg_for_qtot' , use_DPSavg_for_qtot) 
    if (prescrDEPTH) then
       use_DPSavg_for_qtot = .true.  !overwrites input !UNSTABLE IF use_DPSavg_for_qtot=FALSE
    endif
    call prop_get_logical(gdp%mdfile_ptr, '*', 'skip_aval_adjust' , skip_aval_adjust) 
    call prop_get_logical(gdp%mdfile_ptr, '*', 'forceCHEZYtransp' , forceCHEZYtransp) 
    call prop_get_logical(gdp%mdfile_ptr, '*', 'bdslpINupwnbed' , bdslpINupwnbed) 
    call prop_get_logical(gdp%mdfile_ptr, '*', 'ignoreMUmeyer' , ignoreMUmeyer) 
    call prop_get_logical(gdp%mdfile_ptr, '*', 'zavg_global' , zavg_global)     
    if (zavg_global) then
       if (.not.prescrDEPTH) then
          write(*,*) 'zavg_global can be only prescribed with prescrDEPTH=true '
          call d3stop(1, gdp)
       endif
    endif
    call prop_get_logical(gdp%mdfile_ptr, '*', 'distr_qtq_bdl_NNprism' , distr_qtq_bdl_NNprism)     
    call prop_get_logical(gdp%mdfile_ptr, '*', 'compHALFDTss' , compHALFDTss)  
    call prop_get_logical(gdp%mdfile_ptr, '*', 'bedLOADper' , bedLOADper) 
    call prop_get_logical(gdp%mdfile_ptr, '*', 'suspLOADper' , suspLOADper) 
    call prop_get_logical(gdp%mdfile_ptr, '*', 'suspCONCper' , suspCONCper) 
    call prop_get_logical(gdp%mdfile_ptr, '*', 'FORCEuAThPERbnd' , FORCEuAThPERbnd) 
    call prop_get_logical(gdp%mdfile_ptr, '*', 'moveEDtoBED' , moveEDtoBED) 
    call prop_get_logical(gdp%mdfile_ptr, '*', 'consistency_ce_Cav' , consistency_ce_Cav)     
    call prop_get_logical(gdp%mdfile_ptr, '*', 'prescVR93refHEIGHT' , prescVR93refHEIGHT) 
    call prop_get_logical(gdp%mdfile_ptr, '*', 'getADJACENTgrad' , getADJACENTgrad) 
    call prop_get_logical(gdp%mdfile_ptr, '*', 'changeKFUVcut' , changeKFUVcut) 
    call prop_get_logical(gdp%mdfile_ptr, '*', 'deactGHOST_smallcut' , deactGHOST_smallcut) 
    call prop_get_logical(gdp%mdfile_ptr, '*', 'dontRESETghost' , dontRESETghost) 
    call prop_get_logical(gdp%mdfile_ptr, '*', 'ratioVR84' , ratioVR84) 
    call prop_get_logical(gdp%mdfile_ptr, '*', 'resetV1toV0' , resetV1toV0)   
    call prop_get_logical(gdp%mdfile_ptr, '*', 'ITERATEfree' , ITERATEfree) 
    call prop_get_logical(gdp%mdfile_ptr, '*', 'freeU0fixed' , freeU0fixed) 
    call prop_get_logical(gdp%mdfile_ptr, '*', 'TRANSVperIMPL' , TRANSVperIMPL)
    call prop_get_logical(gdp%mdfile_ptr, '*', 'WAQUAfullyCENTRED' , WAQUAfullyCENTRED) 
    call prop_get_logical(gdp%mdfile_ptr, '*', 'CYCLICtransCENTR' , CYCLICtransCENTR) 
    call prop_get_logical(gdp%mdfile_ptr, '*', 'EXCLouterVEL' ,EXCLouterVEL)
    call prop_get_logical(gdp%mdfile_ptr, '*', 'bndBEDfromFILE' , bndBEDfromFILE)  
    call prop_get_logical(gdp%mdfile_ptr, '*', 'corrSURFslopeSUD' , corrSURFslopeSUD) 
    call prop_get_logical(gdp%mdfile_ptr, '*', 'corrSURFslopeUZD' , corrSURFslopeUZD) 
    call prop_get_logical(gdp%mdfile_ptr, '*', 'originalMOMENTUM' , originalMOMENTUM)  
    call prop_get_logical(gdp%mdfile_ptr, '*', 'virtualMERGEupdVERT' , virtualMERGEupdVERT)
    call prop_get_logical(gdp%mdfile_ptr, '*', 'virtualMERGEupdBED' , virtualMERGEupdBED)
    call prop_get_logical(gdp%mdfile_ptr, '*', 'virtualMERGEupdDEPTH' , virtualMERGEupdDEPTH)
    call prop_get_logical(gdp%mdfile_ptr, '*', 'onlyUZD' , onlyUZD)
    call prop_get_logical(gdp%mdfile_ptr, '*', 'noUZD' , noUZD)
    call prop_get_logical(gdp%mdfile_ptr, '*', 'cntrUZDbnd_n' , cntrUZDbnd_n)
    call prop_get_logical(gdp%mdfile_ptr, '*', 'cntrUZDbnd_m' , cntrUZDbnd_m)
    call prop_get_logical(gdp%mdfile_ptr, '*', 'vvvSECord' , vvvSECord)
    call prop_get_logical(gdp%mdfile_ptr, '*', 'gradDEFER3orderDIFF' , gradDEFER3orderDIFF)
    call prop_get_logical(gdp%mdfile_ptr, '*', 'analDEFERR' , analDEFERR)
    call prop_get_logical(gdp%mdfile_ptr, '*', 'partIMPLgrad' , partIMPLgrad)
    virtualMERGEupdCONC = virtualMERGEupdDEPTH !IF virtualMERGEupdCONC is NOT in the mdf it is set to the bed value
    if (virtualMERGEupdCONC.and..not.virtualMERGEupdDEPTH) then
       write(*,*) 'virtualMERGEupdCONC==.true.and.virtualMERGEupdBED==.false. not allowed' !unless I compute merged matrices also for conc separetely
       call d3stop(1, gdp)
    endif
    call prop_get_logical(gdp%mdfile_ptr, '*', 'virtualMERGEdisch' , virtualMERGEdisch)
    call prop_get_logical(gdp%mdfile_ptr, '*', 'virtualLINK' , virtualLINK)
    call prop_get_logical(gdp%mdfile_ptr, '*', 'virtuallinkSMOw1' , virtuallinkSMOw1) 
    call prop_get_logical(gdp%mdfile_ptr, '*', 'linkMINarea' , linkMINarea)
    call prop_get_logical(gdp%mdfile_ptr, '*', 'virtualMERGEupdCONC' , virtualMERGEupdCONC)
    call prop_get_logical(gdp%mdfile_ptr, '*', 'SECordLEVEL' , SECordLEVEL)
    call prop_get_logical(gdp%mdfile_ptr, '*', 'SECordVEL' , SECordVEL)

    IF (.not.virtuallink) THEN
       virtuallinkSMOw1 = .FALSE.
    ENDIF
    if (cutcell==0) then
       virtuallink = .false.
       virtuallinkSMOw1 = .FALSE.
    endif
!
    if ((uAXIS.LT.-999999..OR.hAXIS.LT.-999999.).AND.simpleVR84==1) THEN
       WRITE(*,*) 'Both uAXIS and hAXIS have to be provided when simpleVR84==1' 
       call d3stop(1, gdp)
    endif
!        
    if (kmax.gt.1) then
       if (virtualMERGEupdDEPTH) then
          if (.not.virtualMERGEupdVERT) then
             call prterr(lundia, 'U021', 'Virtualmerge has to be done for both levels and vertical vel or only vertical velocity!')
             call d3stop(1, gdp)
          else
             if (.not.virtuallink.and.typeVIRTmergeUPDvert.ne.typeVIRTmergeUPDdepth) then
                call prterr(lundia, 'U021', 'Same virtualmerge type has to be chosen for both levels and vertical velocity!' )
                call d3stop(1, gdp)
             endif
             if (.not.virtuallink.and.comparereal(thresMERGE_w,thresMERGE_d).ne.0)  then
                call prterr(lundia, 'U021', 'Same virtualmerge threshold has to be used for both levels and vertical velocity!' )
                call d3stop(1, gdp)
             endif
          endif
       endif
    endif
    if (virtualMERGEupdDEPTH) then
       if(.not.virtualMERGEupdBED) then
             call prterr(lundia, 'U021', 'Virtualmerge has to be done for both depths and bed elevation, or only bed elevation!')
             call d3stop(1, gdp)
       endif
    endif
    if (virtualMERGEdisch) then
       if(.not.virtualMERGEupdBED.or..not.virtualMERGEupdDEPTH) then
          !call prterr(lundia, 'U021', 'virtualMERGEdisch has to be combined with merge for both depths and bed elevation!')
          !call d3stop(1, gdp)
       endif
    endif
    !
    allocate(gdp%gdimbound%iPERs1(nmaxus,mmax))
    gdp%gdimbound%iPERs1 = 0
    iPERs1 => gdp%gdimbound%iPERs1
    nrPER = 0
    if (periodSURFACE) then !read periodic locations
       lunPER= newlun(gdp)
       open(lunPER, file = 'periodic.per', status = 'old',ERR=9999)
       read(lunPER,*) nrPER
       if (nrPER.gt.999) then
          write(*,*) 'Max number of periodic locations is 999!'
          call d3stop(1, gdp)
       endif
       do k=1,nrPER
          read(lunPER,*) mPH_ext(k),nPH_ext(k),mPQ_ext(k),nPQ_ext(k)  
          iPERs1(nPH_ext(k),mPH_ext(k)) = 1
          iPERs1(nPQ_ext(k),mPQ_ext(k)) = 1  
       enddo
       !
       PERIODalongM = 2
       if (mPH_ext(2).eq.mPH_ext(1)) then
          PERIODalongM = 1 !true
       elseif (nPH_ext(2).eq.nPH_ext(1)) then
          PERIODalongM = 0 !false
       endif

       if (PERIODalongM.eq.1) then
          !check that all the other values are on the same line
          do k=2,nrPER
             if (mPH_ext(k).NE.mPH_ext(1).OR.mPQ_ext(k).NE.mPQ_ext(1)) then
                call prterr(lundia, 'U021', 'Periodic BC have to be along coordinate axes!!')
                call d3stop(1, gdp)
             endif
          enddo
          !check that they are ordered from the biggest to the smallest n
          do k=2,nrPER
             if (nPH_ext(k).ge.nPH_ext(k-1).OR.nPQ_ext(k).ge.nPQ_ext(k-1)) then
                call prterr(lundia, 'U021', 'Periodic BC have to be ordered from the biggest to the smallest n!!')
                call d3stop(1, gdp)
             endif
          enddo

       elseif (PERIODalongM.eq.0) then
          !!check that all the other values are on the same line
          do k=2,nrPER
             if (nPH_ext(k).NE.nPH_ext(1).OR.nPQ_ext(k).NE.nPQ_ext(1)) then
                call prterr(lundia, 'U021', 'Periodic BC have to be along coordinate axes!!')
                call d3stop(1, gdp)
             endif
          enddo
          !check that they are ordered from the biggest to the smallest m
          do k=2,nrPER
             if (mPH_ext(k).ge.mPH_ext(k-1).OR.mPQ_ext(k).ge.mPQ_ext(k-1)) then
                call prterr(lundia, 'U021', 'Periodic BC have to be ordered from the biggest to the smallest m!!')
                call d3stop(1, gdp)
             endif
          enddo
       else
          call prterr(lundia, 'U021', 'Periodic BC have to be along coordinate axes!!')
          call d3stop(1, gdp)
       endif
       !  
       ! add lower one to have lower velocity point, and also upper one (so ghost point quantities are correctly made periodic)
       !
       if (PERIODalongM.eq.1) then   
          !lower 
          mPH_ext(nrPER+1) = mPH_ext(nrPER)
          nPH_ext(nrPER+1) = nPH_ext(nrPER)-1
          mPQ_ext(nrPER+1) = mPQ_ext(nrPER)
          nPQ_ext(nrPER+1) = nPQ_ext(nrPER)-1
          !uppper
          mPH_ext(0) = mPH_ext(1)
          nPH_ext(0) = nPH_ext(1)+1
          mPQ_ext(0) = mPQ_ext(1)
          nPQ_ext(0) = nPQ_ext(1)+1
       else
          !lower 
          mPH_ext(nrPER+1) = mPH_ext(nrPER)-1
          nPH_ext(nrPER+1) = nPH_ext(nrPER)
          mPQ_ext(nrPER+1) = mPQ_ext(nrPER)-1
          nPQ_ext(nrPER+1) = nPQ_ext(nrPER)
          !uppper
          mPH_ext(0) = mPH_ext(1)+1
          nPH_ext(0) = nPH_ext(1)
          mPQ_ext(0) = mPQ_ext(1)+1
          nPQ_ext(0) = nPQ_ext(1)
       endif
    else
       bedPERIODIC = .FALSE.
       TRANSVperIMPL = .false.
       distr_qtq_per = .false.   ! have to read distr_qtq_per here  in rdgrid cause otherwise distr_qtq_per is read after its assigned false here
       distr_bdl_per = .false.   ! have to read distr_bdl_per here  in rdgrid cause otherwise distr_bdl_per is read after its assigned false here
       PERIODICtangVEL = .false. ! not needed for now
       PERIODICorthVEL = .false. ! not needed for now
       periodGHOST     = .false.
       callSUBR_WATERlevelPERIOD = .false.
       bedLOADper =.false.
       suspLOADper = .false.
       neuPERslope = .false.
    endif
       
    if (ERODsubmBANKS.LT.0.OR.ERODsubmBANKS.GT.1) then
       call prterr(lundia, 'U021', 'ERODsubmBANKS must be 0 or 1!!')
       call d3stop(1, gdp)
    endif
    if (idebugCUT.eq.0) then
       idebugCUTini = 99999 
       idebugCUTfin = -99999     
    else
       txtput1 = 'Note: debug files for cut cells are written. The computation is slowed down.'
       write (lundia, '(3a)') txtput1 
    endif
    if (idebugCUThard.eq.0) then
       idebugCUThardINI =   99999
       idebugCUThardFIN = - 99999
    else
       txtput1 = 'Note: debug files for cut cells are written. The computation is slowed down.'
       write (lundia, '(3a)') txtput1 
    endif
    if (continuity_cc.eq.1.and.GhostMethod.eq.2) then 
       if( extrapGHOST1fluid2==0) then
            call prterr(lundia,'U021', 'extrapGHOST1fluid2=0 not compatible with continuity_cc=1 and GhostMethod=2')
            !call d3stop(1, gdp)
       endif
    endif
    !
    call prop_get_integer(gdp%mdfile_ptr, '*', 'GHOSTimpl', GHOSTimpl)
    txtput1 = 'GHOSTimpl'
    if (GHOSTimpl==0) then
      txtput2 = '                 Ghost cell prescribed explicitly'
    elseif (GHOSTimpl==1) then
      txtput2 = '                 Ghost cell prescribed implicitly'
    elseif (GHOSTimpl>=2) then 
      txtput2 = '                 Wrong value of GHOSTimpl. assigned GHOSTimpl=0'
    endif
    write (lundia, '(3a)') txtput1, ':', txtput2
    !
    if (cutcell.gt.0) then
        call prop_get_string(gdp%mdfile_ptr, '*', 'FilCCout', filcc_out)
        call prop_get_string(gdp%mdfile_ptr, '*', 'FilCCin', filcc_in)
        if (filcc_in=='') filcc_in = 'dummyname'
        inquire (file = filcc_in, exist = ex)
        if (.not. ex) then
            call prterr(lundia, 'U021', 'File ' // 'FilCCin' // ' does not exist')
            call d3stop(1, gdp)
        endif
    else
       corrSURFslopeSUD = .false.
    endif
    !
    if (.not. virtualMERGEupdBED) then
        !
        ! thresMERGE_zb is used in subroutine PER_dp
        !
        thresMERGE_zb  = 0._fp
    endif
    !
    ! Allocations and initializations for all cutcell types
    !
    allocate (gdp%gdimbound%dpL         (gdp%d%nlb:gdp%d%nub,gdp%d%mlb:gdp%d%mub))
    allocate (gdp%gdimbound%dpH         (gdp%d%nlb:gdp%d%nub,gdp%d%mlb:gdp%d%mub))
    gdp%gdimbound%dpL   = 0.0_fp
    gdp%gdimbound%dpH   = 0.0_fp
    !
    ! Specific allocations and initializations for cutcell type 2
    !
    if (cutcell == 2) then
       allocate (gdp%gdimbound%agsqs           (  gdp%d%nlb:gdp%d%nub,gdp%d%mlb:gdp%d%mub))
       allocate (gdp%gdimbound%u1_FLLYghst     (  gdp%d%nlb:gdp%d%nub,gdp%d%mlb:gdp%d%mub,kmax))
       allocate (gdp%gdimbound%v1_FLLYghst     (  gdp%d%nlb:gdp%d%nub,gdp%d%mlb:gdp%d%mub,kmax))
       allocate (gdp%gdimbound%INTx_GRS        (5,gdp%d%nlb:gdp%d%nub,gdp%d%mlb:gdp%d%mub))      
       allocate (gdp%gdimbound%INTy_GRS        (5,gdp%d%nlb:gdp%d%nub,gdp%d%mlb:gdp%d%mub))      
       allocate (gdp%gdimbound%INTwx_GRS       (5,gdp%d%nlb:gdp%d%nub,gdp%d%mlb:gdp%d%mub))      
       allocate (gdp%gdimbound%INTwy_GRS       (5,gdp%d%nlb:gdp%d%nub,gdp%d%mlb:gdp%d%mub))
       allocate (gdp%gdimbound%Nx              (  gdp%d%nlb:gdp%d%nub,gdp%d%mlb:gdp%d%mub))
       allocate (gdp%gdimbound%Ny              (  gdp%d%nlb:gdp%d%nub,gdp%d%mlb:gdp%d%mub))
       allocate (gdp%gdimbound%xG_L            (  gdp%d%nlb:gdp%d%nub,gdp%d%mlb:gdp%d%mub))      
       allocate (gdp%gdimbound%yG_L            (  gdp%d%nlb:gdp%d%nub,gdp%d%mlb:gdp%d%mub))      
       allocate (gdp%gdimbound%xG_H            (  gdp%d%nlb:gdp%d%nub,gdp%d%mlb:gdp%d%mub))      
       allocate (gdp%gdimbound%yG_H            (  gdp%d%nlb:gdp%d%nub,gdp%d%mlb:gdp%d%mub))      
       !
       gdp%gdimbound%agsqs       = 0.0_fp
       gdp%gdimbound%u1_FLLYghst = 0.0_fp
       gdp%gdimbound%v1_FLLYghst = 0.0_fp
       gdp%gdimbound%INTx_GRS    = 0.0_fp
       gdp%gdimbound%INTy_GRS    = 0.0_fp
       gdp%gdimbound%INTwx_GRS   = 0.0_fp
       gdp%gdimbound%INTwy_GRS   = 0.0_fp
       gdp%gdimbound%Nx          = 0.0_fp
       gdp%gdimbound%Ny          = 0.0_fp
       gdp%gdimbound%xG_L        = 0.0_fp
       gdp%gdimbound%yG_L        = 0.0_fp
       gdp%gdimbound%xG_H        = 0.0_fp
       gdp%gdimbound%yG_H        = 0.0_fp
    endif
    !
    return
    9999 write(*,*) 'Provide a file periodic.per with periodic locations!'
    call d3stop(1, gdp)
    !
end subroutine rdimbound
