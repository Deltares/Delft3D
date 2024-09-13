subroutine initimbound(gdp)
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
!  $Id: initimbound.f90 6742 2016-11-09 16:50:58Z canestrelli.x $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20160126_PLIC_VOF_bankEROSION/src/engines_gpl/flow2d3d/packages/data/src/gdp/initimbound.f90 $
!!--description-----------------------------------------------------------------
!
! NONE
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    logical        , pointer :: activeNEVERghost
    logical        , pointer :: analDEFERR
    integer        , pointer :: analSUDcenterACTIVE
    integer        , pointer :: analyticalPOLY
    logical        , pointer :: AVvelCUT
    logical        , pointer :: bdslpINupwnbed
    logical        , pointer :: bedLOADper
    logical        , pointer :: bedPERIODIC
    logical        , pointer :: bedUPDandFIXdepth
    logical        , pointer :: bnd_distr_perC
    logical        , pointer :: bndBEDfromFILE
    integer        , pointer :: BOUNDvof
    logical        , pointer :: callSUBR_WATERlevelPERIOD
    logical        , pointer :: changeKFUVcut
    logical        , pointer :: cntrUZDbnd_m
    logical        , pointer :: cntrUZDbnd_n
    logical        , pointer :: compHALFDTss
    logical        , pointer :: consistency_ce_Cav
    logical        , pointer :: constSOLforCHECKmomTERM
    logical        , pointer :: constSOLUTION
    integer        , pointer :: continuity_cc
    integer        , pointer :: CORRbedSLOPEcut
    logical        , pointer :: corrSURFslopeSUD  
    logical        , pointer :: corrSURFslopeUZD  
    logical        , pointer :: CURVboogaard
    integer        , pointer :: cutBC  
    integer        , pointer :: cutcell
    logical        , pointer :: CYCLICtransCENTR
    logical        , pointer :: deactGHOST_smallcut
    real(fp)       , pointer :: DELAYfixedBEDequilQS
    logical        , pointer :: DEPOSbankMATERIAL
    real(fp)       , pointer :: DISSghost
    real(fp)       , pointer :: distanceBOUNDper
    logical        , pointer :: distr_qtq_bdl_NNprism
    integer        , pointer :: doNOTdebugGHOSTS
    logical        , pointer :: dontRESETghost  
    logical        , pointer :: DOUBLEuvh       
    real(fp)       , pointer :: ELEVencr
    real(fp)       , pointer :: epsSUD
    integer        , pointer :: ERODsubmBANKS
    logical        , pointer :: EXACTcurv             
    logical        , pointer :: EXACTpolygons         
    logical        , pointer :: EXACTpolygonsONLYfirst
    integer        , pointer :: exactSLOPE
    logical        , pointer :: EXCLouterVEL 
    integer        , pointer :: extrapGHOST1fluid2
    real(fp)       , pointer :: facMERGElink
    real(fp)       , pointer :: facws
    character(255) , pointer :: filcc_in
	 character(255) , pointer :: filcc_out   
    logical        , pointer :: floodplain_inflow       
    logical        , pointer :: forceCHEZYtransp      
    logical        , pointer :: FORCEdisch                
    logical        , pointer :: FORCEexplicitUPDATEs1 
    real(fp)       , pointer :: fracBANKdepos   
    real(fp)       , pointer :: fracBANKsuspWASH
    integer        , pointer :: free_S1_sud
    integer        , pointer :: freeNONhomo
    logical        , pointer :: freeU0fixed    
    logical        , pointer :: getADJACENTgrad
    integer        , pointer :: GHOSTimpl  
    integer        , pointer :: GhostMethod
    logical        , pointer :: gradDEFER3orderDIFF
    real(fp)       , pointer :: hAXIS
    logical        , pointer :: hindered     
    logical        , pointer :: HORIZdiffZERO
    logical        , pointer :: HORIZviscZERO
    integer        , pointer :: HOWmanyPOINTSforCURV
    integer        , pointer :: iDEBUGcut       
    integer        , pointer :: iDEBUGcutFIN    
    integer        , pointer :: idebugCUThard   
    integer        , pointer :: idebugCUThardFIN
    integer        , pointer :: idebugCUThardINI
    integer        , pointer :: iDEBUGcutINI
    logical        , pointer :: ignoreMUmeyer      
    logical        , pointer :: IGNOREwrongDEPTH   
    logical        , pointer :: includeSMALLforCURV
    integer        , pointer :: int0comp1_s1SMALLcut
    logical        , pointer :: INTERPdhdx
    logical        , pointer :: interpS1beforeUZD
    logical        , pointer :: interpVinUexact
    integer        , pointer :: iprintINTERMghost01
    integer        , pointer :: IstencBANKer
    logical        , pointer :: ITERATEfree
    real(fp)       , pointer :: Kbank  
    integer        , pointer :: kFLcutEQ1
    logical        , pointer :: linkMINarea          
    logical        , pointer :: massBALhdt             
    logical        , pointer :: massbalLOC             
    real(fp)       , pointer :: maxVELfac              
    real(fp)       , pointer :: minVELfac              
    integer        , pointer :: MODadvecGHOSTsud       
    integer        , pointer :: MODadvecGHOSTuzd       
    logical        , pointer :: modDWNVEL              
    logical        , pointer :: moveEDtoBED            
    integer        , pointer :: NanglesANALcircle
    integer        , pointer :: NanglesANALcircle_FIXED
    logical        , pointer :: neuPERslope     
    logical        , pointer :: noCORfacCURV         
    logical        , pointer :: noFLOODINGbanks 
    logical        , pointer :: noUZD
    integer        , pointer :: nrPER
    integer        , pointer :: NsmoCURV
    logical        , pointer :: onlyUZD         
    logical        , pointer :: originalMOMENTUM
    logical        , pointer :: partIMPLgrad
    real(fp)       , pointer :: PERCedge
    real(fp)       , pointer :: perSMOfac
    real(fp)       , pointer :: perSMOfac_Qb
    integer        , pointer :: PERIODalongM
    logical        , pointer :: periodGHOST       
    logical        , pointer :: PERIODICorthVEL   
    logical        , pointer :: PERIODICtangVEL   
    logical        , pointer :: PERIODICwaterDEPTH
    logical        , pointer :: periodSURFACE
    logical        , pointer :: precisePOROSbaric 
    logical        , pointer :: prescrDEPTH       
    logical        , pointer :: prescVR93refHEIGHT
    logical        , pointer :: prescVR93settl    
    integer        , pointer :: PREsmoothVELOCcurv
    logical        , pointer :: PRINTbalanceUZD   
    logical        , pointer :: printCURV         
    logical        , pointer :: PRINTedgeVEL      
    logical        , pointer :: printFLUXuv       
    logical        , pointer :: printGHOSTmap     
    logical        , pointer :: printINTERMghost  
    logical        , pointer :: printSUDITERghost 
    logical        , pointer :: printUZDITERghost
    real(fp)       , pointer :: R1_anal
    real(fp)       , pointer :: R2_anal
    real(fp)       , pointer :: ratio_ca_c2d
    logical        , pointer :: ratioVR84        
    logical        , pointer :: RECdepth     
    real(fp)       , pointer :: reltim_qtq
    real(fp)       , pointer :: reltim_qtq_C
    real(fp)       , pointer :: reltim_s1
    real(fp)       , pointer :: reltim_qtq_bdl
    integer        , pointer :: removeW1qzk     
    logical        , pointer :: resetV1toV0     
    logical        , pointer :: SECordLEVEL     
    logical        , pointer :: SECordVEL       
    logical        , pointer :: shift_xycor     
    integer        , pointer :: simpleVR84      
    logical        , pointer :: SingleLOOPuzd   
    logical        , pointer :: skip_aval_adjust
    logical        , pointer :: smoCURV
    integer        , pointer :: SMOOTHbankSHEAR 
    integer        , pointer :: SMOOTHbankVEL   
    integer        , pointer :: smoothEb        
    integer        , pointer :: subtypeTESTghost
    logical        , pointer :: SUDtoCONVERGENCE
    logical        , pointer :: suspCONCper     
    logical        , pointer :: suspLOADper
    real(fp)       , pointer :: TAUcrBANKcnst
    logical        , pointer :: testGHOSTaccur    
    real(fp)       , pointer :: threshVELghost
    real(fp)       , pointer :: thresMERGE_d
    real(fp)       , pointer :: thresMERGE_w
    real(fp)       , pointer :: thresMERGE_zb
    real(fp)       , pointer :: thresMERGE_Q
    real(fp)       , pointer :: THRdepVEGET
    real(fp)       , pointer :: thresCURVcut
    real(fp)       , pointer :: THRESextCUTedge
    real(fp)       , pointer :: THRESsmallCELL
    real(fp)       , pointer :: THRlocalMASSbal
    real(fp)       , pointer :: thrPRINTerrGRAD
    real(fp)       , pointer :: timeFORdisrupt
    real(fp)       , pointer :: timeFORenchr
    real(fp)       , pointer :: tmorB
    real(fp)       , pointer :: tolFREEexact
    real(fp)       , pointer :: tolwet
    integer        , pointer :: totGHOSTu1 
    integer        , pointer :: totGHOSTv1 
    integer        , pointer :: totGHOSTs1
    logical        , pointer :: TRANSVperIMPL 
    logical        , pointer :: twoCELLSperiod
    integer        , pointer :: TYPEangleCURV        
    integer        , pointer :: typeCOMPcurvSMALL     
    integer        , pointer :: TYPEdistrBANKerod    
    integer        , pointer :: typeEXTRAPstencil    
    integer        , pointer :: typeEXTRAPux         
    integer        , pointer :: TYPEfreeSLIP
    integer        , pointer :: typeHART             
    integer        , pointer :: typeHUDPU            
    integer        , pointer :: TYPEinterpVELcurv   
    integer        , pointer :: TYPEpartIMPLgrad     
    integer        , pointer :: TYPEtauBANK          
    integer        , pointer :: TYPEtauCRbank
    integer        , pointer :: typeVEGencr
    integer        , pointer :: typeVIRTmergeUPDbed
    integer        , pointer :: typeVIRTmergeUPDdepth
    integer        , pointer :: typeVIRTmergeUPDvert
    logical        , pointer :: UavWETtau               
    real(fp)       , pointer :: uAXIS
    logical        , pointer :: use_DPSavg_for_qtot     
    logical        , pointer :: useCUTstyle             
    logical        , pointer :: USEfixedBEDequilQb      
    logical        , pointer :: USEfixedBEDequilQS      
    logical        , pointer :: useFULL                 
    integer        , pointer :: VERSIONprecisePOROSbaric    
    logical        , pointer :: virtualLINK             
    logical        , pointer :: virtuallinkSMOw1        
    logical        , pointer :: virtualMERGEdisch            
    logical        , pointer :: virtualMERGEupdBED      
    logical        , pointer :: virtualMERGEupdCONC     
    logical        , pointer :: virtualMERGEupdDEPTH    
    logical        , pointer :: virtualMERGEupdVERT     
    logical        , pointer :: vvvSECord               
    logical        , pointer :: WAQUAfullyCENTRED       
    logical        , pointer :: zavg_global
    !              
    integer        , dimension(:) , pointer :: mpH_int
    integer        , dimension(:) , pointer :: npH_int
    integer        , dimension(:) , pointer :: mpQ_int
    integer        , dimension(:) , pointer :: npQ_int
    integer        , dimension(:) , pointer :: mpH_ext
    integer        , dimension(:) , pointer :: npH_ext
    integer        , dimension(:) , pointer :: mpQ_ext
    integer        , dimension(:) , pointer :: npQ_ext
    integer        , dimension(:) , pointer :: mpH_intint
    integer        , dimension(:) , pointer :: npH_intint
    integer        , dimension(:) , pointer :: mpQ_intint
    integer        , dimension(:) , pointer :: npQ_intint
    integer        , dimension(:) , pointer :: mpH_extext
    integer        , dimension(:) , pointer :: npH_extext
    integer        , dimension(:) , pointer :: mpQ_extext
    integer        , dimension(:) , pointer :: npQ_extext
!   start IBM_research variables, most of them will be eventually removed
    logical        , pointer :: DPUhuSECONDorder
    logical        , pointer :: noCUTfac   
    logical        , pointer :: FORCEfrict_sud        
    logical        , pointer :: FORCEfrict_uzd        
    logical        , pointer :: FORCEghost_sud1       
    logical        , pointer :: FORCEghost_sud2       
    logical        , pointer :: FORCEghost_uzd1       
    logical        , pointer :: FORCEghost_uzd2       
    logical        , pointer :: FORCEgradS1_sud       
    logical        , pointer :: FORCEgradS1_uzd       
    logical        , pointer :: forceN                
    logical        , pointer :: FORCEnormBIinFINDbi   
    logical        , pointer :: forceQYKallSTEPS      
    logical        , pointer :: FORCEs1CIRCanal       
    logical        , pointer :: forceU0_sud           
    logical        , pointer :: FORCEu1_sud           
    logical        , pointer :: FORCEu1_uzd           
    logical        , pointer :: FORCEuAThPERbnd       
    logical        , pointer :: FORCEududx_sud        
    logical        , pointer :: FORCEududx_uzd        
    logical        , pointer :: FORCEvdudy_sud        
    logical        , pointer :: FORCEvdudy_uzd        
    logical        , pointer :: forceVVVsud           
    logical        , pointer :: forceVVVuzd    
    logical        , pointer :: implDEFsud     
    logical        , pointer :: deferredS1sud
    logical        , pointer :: deferredS1uzd    
    integer        , pointer :: TYPEgradDEFERRsud    
    integer        , pointer :: TYPEgradDEFERRuzd      
    logical        , pointer :: vFACsudITER             
    logical        , pointer :: vFACTORcutEDGESx        
    logical        , pointer :: vFACTORcutEDGESy       
    logical        , pointer :: hu2SUDiter
    logical        , pointer :: huRHS         
    logical        , pointer :: forceEb           
    logical        , pointer :: force_DPUhu           
    logical        , pointer :: force_QYK             
    logical        , pointer :: forceANALforSTAGE2    
    logical        , pointer :: extrADVECTsud    
    integer        , pointer :: TYPEofFORCING  
    logical        , pointer :: skipBOUNDvelSUD    
    logical        , pointer :: FIXEDcoastBANKS        
!   end IBM_research

!
! Global variables
!
!
!
!! executable statements -------------------------------------------------------
!
    !
    ! Initializations for the Immersed Boundary approach used for the bank (erosion) code
    ! using a Piecwise-Linear Interface reconstruction (PLIC) and a Volume of Fluid (VOF) cutcell method
    ! See Canestrelli et al. (2015).
    !
    gdp%gdimbound%activeNEVERghost         = .false.
    gdp%gdimbound%analDEFERR               = .false.
    gdp%gdimbound%analSUDcenterACTIVE      = 1
    gdp%gdimbound%analyticalPOLY           = 0
    gdp%gdimbound%AVvelCUT                 = .false.
    gdp%gdimbound%bdslpINupwnbed           = .false.
    gdp%gdimbound%bedLOADper               = .false.
    gdp%gdimbound%bedPERIODIC              = .TRUE.
    gdp%gdimbound%bedUPDandFIXdepth        = .FALSE.
    gdp%gdimbound%bnd_distr_perC           = .false.
    gdp%gdimbound%bndBEDfromFILE           = .false.
    gdp%gdimbound%BOUNDvof                 = 1
    gdp%gdimbound%callSUBR_WATERlevelPERIOD= .false.
    gdp%gdimbound%changeKFUVcut            = .true.
    gdp%gdimbound%cntrUZDbnd_m             = .false.
    gdp%gdimbound%cntrUZDbnd_n             = .false.
    gdp%gdimbound%compHALFDTss             = .false.
    gdp%gdimbound%consistency_ce_Cav       = .false.
    gdp%gdimbound%constSOLforCHECKmomTERM  = .false.
    gdp%gdimbound%constSOLUTION            = .false.
    gdp%gdimbound%continuity_cc            = 0                    ! the continuity equation is not changed for cut cells
    gdp%gdimbound%CORRbedSLOPEcut          = 0
    gdp%gdimbound%corrSURFslopeSUD         = .false.
    gdp%gdimbound%corrSURFslopeUZD         = .false.
    gdp%gdimbound%CURVboogaard             = .true.
    gdp%gdimbound%cutBC                    = 0
    gdp%gdimbound%cutcell                  = 0                    ! no cut-cell approach
    gdp%gdimbound%CYCLICtransCENTR         = .FALSE.
    gdp%gdimbound%deactGHOST_smallcut      = .false.
    gdp%gdimbound%DELAYfixedBEDequilQS     = 0.5_fp
    gdp%gdimbound%DEPOSbankMATERIAL        = .false.
    gdp%gdimbound%DISSghost                = 1._fp
    gdp%gdimbound%distr_qtq_bdl_NNprism    = .FALSE.
    gdp%gdimbound%doNOTdebugGHOSTS         = 0
    gdp%gdimbound%dontRESETghost           = .false.
    gdp%gdimbound%DOUBLEuvh                = .false.
    gdp%gdimbound%ELEVencr                 = 0._fp
    gdp%gdimbound%epsSUD                   = 0.000001_fp
    gdp%gdimbound%EXACTcurv                = .FALSE.
    gdp%gdimbound%EXACTpolygons            = .false.
    gdp%gdimbound%EXACTpolygonsONLYfirst   = .false.
    gdp%gdimbound%exactSLOPE               = 0
    gdp%gdimbound%EXCLouterVEL             = .false.
    gdp%gdimbound%extrapGHOST1fluid2       = 0                    ! NO  extrapolation
    gdp%gdimbound%facMERGElink             = 1._fp
    gdp%gdimbound%facws                    = 1._fp                ! tuning factor multiplying settling velocity ws in fallve.f90
    gdp%gdimbound%floodplain_inflow        = .true.
    gdp%gdimbound%force_DPUhu              = .false.
    gdp%gdimbound%force_QYK                = .false.
    gdp%gdimbound%forceANALforSTAGE2       = .false.
    gdp%gdimbound%forceCHEZYtransp         = .FALSE.
    gdp%gdimbound%FORCEdisch               = .false.
    gdp%gdimbound%forceEb                  = .false.
    gdp%gdimbound%FORCEexplicitUPDATEs1    = .FALSE.
    gdp%gdimbound%fracBANKdepos            = 1._fp
    gdp%gdimbound%fracBANKsuspWASH         = 0._fp
    gdp%gdimbound%free_S1_sud              = 0
    gdp%gdimbound%freeNONhomo              = 0
    gdp%gdimbound%freeU0fixed              = .true.
    gdp%gdimbound%getADJACENTgrad          = .false.
    gdp%gdimbound%GHOSTimpl                = 0                    ! explicit ghost cell method
    gdp%gdimbound%GhostMethod              = 0                    ! the fluid ghost cell method is not used.
    gdp%gdimbound%gradDEFER3orderDIFF      = .false.
    gdp%gdimbound%hAXIS                    = -999999999._fp
    gdp%gdimbound%hindered                 = .TRUE.
    gdp%gdimbound%HORIZdiffZERO            = .FALSE.
    gdp%gdimbound%HORIZviscZERO            = .FALSE.
    gdp%gdimbound%HOWmanyPOINTSforCURV     = 3
    gdp%gdimbound%iDEBUGcut                = 0                    ! 0:no debug 1: debug
    gdp%gdimbound%iDEBUGcutFIN             = -99999
    gdp%gdimbound%idebugCUThard            = 0
    gdp%gdimbound%idebugCUThardFIN         = - 99999
    gdp%gdimbound%idebugCUThardINI         = 99999
    gdp%gdimbound%iDEBUGcutINI             = 99999
    gdp%gdimbound%ignoreMUmeyer            = .FALSE.
    gdp%gdimbound%IGNOREwrongDEPTH         = .false.
    gdp%gdimbound%includeSMALLforCURV      = .true.
    gdp%gdimbound%int0comp1_s1SMALLcut     = 0
    gdp%gdimbound%INTERPdhdx               = .false.
    gdp%gdimbound%interpS1beforeUZD        = .true.
    gdp%gdimbound%interpVinUexact          = .false.
    gdp%gdimbound%iprintINTERMghost01      = 0
    gdp%gdimbound%IstencBANKer             = 1                    ! 0 : cell (n,m) can erode only itself. 
                                                                  ! 1 : cell (n,m) can erode also the adjacents (9 cells stencil domain of dependance)
    gdp%gdimbound%ITERATEfree              = .false.              
    gdp%gdimbound%Kbank                    = 0._fp                ! 1.e-9_fp
    gdp%gdimbound%kFLcutEQ1                = 0
    gdp%gdimbound%linkMINarea              = .true.
    gdp%gdimbound%massBALhdt               = .false.
    gdp%gdimbound%massbalLOC               = .false.
    gdp%gdimbound%maxVELfac                = 99999999999999._fp
    gdp%gdimbound%minVELfac                = 0.00000000000001_fp
    gdp%gdimbound%MODadvecGHOSTsud         = 0                    ! NO MODIFICATION OF ADVECTIVE TERMS in sud
    gdp%gdimbound%MODadvecGHOSTuzd         = 0                    ! NO MODIFICATION OF ADVECTIVE TERMS in sud
    gdp%gdimbound%modDWNVEL                = .true.
    gdp%gdimbound%moveEDtoBED              = .false.
    gdp%gdimbound%NanglesANALcircle        = 20000000             ! cannot be larger then NanglesANALcircle_FIXED
    gdp%gdimbound%NanglesANALcircle_FIXED  = 20000000             ! if increased, I have to change the size of matrices in the  C++ subroutine wrapUNI_intCEL
    gdp%gdimbound%neuPERslope              = .false.
    gdp%gdimbound%noCORfacCURV             = .false.
    gdp%gdimbound%noFLOODINGbanks          = .false.
    gdp%gdimbound%noUZD                    = .false.
    gdp%gdimbound%NsmoCURV                 = 1
    gdp%gdimbound%onlyUZD                  = .false.
    gdp%gdimbound%originalMOMENTUM         = .false.
    gdp%gdimbound%partIMPLgrad             = .false.
    gdp%gdimbound%PERCedge                 = 0.0_fp
    gdp%gdimbound%periodGHOST              = .true.
    gdp%gdimbound%PERIODICorthVEL          = .true.
    gdp%gdimbound%PERIODICtangVEL          = .true.
    gdp%gdimbound%PERIODICwaterDEPTH       = .true.               ! default value if periodic BC are activated
    gdp%gdimbound%periodSURFACE            = .false.
    gdp%gdimbound%precisePOROSbaric        = .false.
    gdp%gdimbound%prescrDEPTH              = .FALSE.
    gdp%gdimbound%prescVR93refHEIGHT       = .false.
    gdp%gdimbound%prescVR93settl           = .FALSE.
    gdp%gdimbound%PREsmoothVELOCcurv       = 0
    gdp%gdimbound%PRINTbalanceUZD          = .FALSE.
    gdp%gdimbound%printCURV                = .false.
    gdp%gdimbound%PRINTedgeVEL             = .false.
    gdp%gdimbound%printFLUXuv              = .false.
    gdp%gdimbound%printGHOSTmap            = .false.
    gdp%gdimbound%printINTERMghost         = .false.
    gdp%gdimbound%printSUDITERghost        = .false.
    gdp%gdimbound%printUZDITERghost        = .false.
    gdp%gdimbound%R1_anal                  = -999._fp
    gdp%gdimbound%R2_anal                  = -999._fp
    gdp%gdimbound%ratio_ca_c2d             = -999.0_fp
    gdp%gdimbound%ratioVR84                = .false.
    gdp%gdimbound%RECdepth                 = .false.
    gdp%gdimbound%removeW1qzk              = 0
    gdp%gdimbound%resetV1toV0              = .false.
    gdp%gdimbound%SECordLEVEL              = .FALSE.              ! by default, no reconstruction of water level is performed in slim.f90
    gdp%gdimbound%SECordVEL                = .TRUE.               ! by default,    reconstruction of velocity    is performed in ulim.f90
    gdp%gdimbound%shift_xycor              = .false.
    gdp%gdimbound%simpleVR84               = 0
    gdp%gdimbound%SingleLOOPuzd            = .FALSE.
    gdp%gdimbound%skip_aval_adjust         = .FALSE.
    gdp%gdimbound%smoCURV                  = .true.
    gdp%gdimbound%SMOOTHbankSHEAR          = 0
    gdp%gdimbound%SMOOTHbankVEL            = 0
    gdp%gdimbound%smoothEb                 = 0
    gdp%gdimbound%subtypeTESTghost         = 1
    gdp%gdimbound%SUDtoCONVERGENCE         = .false.
    gdp%gdimbound%suspCONCper              = .false.
    gdp%gdimbound%suspLOADper              = .false.
    gdp%gdimbound%TAUcrBANKcnst            = 1._fp
    gdp%gdimbound%testGHOSTaccur           = .false.
    gdp%gdimbound%THRdepVEGET              = 0.5_fp
    gdp%gdimbound%thresCURVcut             = 0.25_fp
    gdp%gdimbound%threshVELghost           = 0._fp                ! 0.001_fp
    gdp%gdimbound%thresMERGE_Q             = 0.0025_fp
    gdp%gdimbound%thrPRINTerrGRAD          = 0._FP
    gdp%gdimbound%timeFORdisrupt           = 60._fp
    gdp%gdimbound%timeFORenchr             = 60._fp
    gdp%gdimbound%tmorB                    = -1._fp               ! default, implying itmorB=itmor in tricom_init
    gdp%gdimbound%tolFREEexact             = 0.000001_fp
    gdp%gdimbound%tolwet                   = 0.0_fp
    gdp%gdimbound%totGHOSTu1               = 0
    gdp%gdimbound%totGHOSTv1               = 0
    gdp%gdimbound%totGHOSTs1               = 0
    gdp%gdimbound%TRANSVperIMPL            = .true.
    gdp%gdimbound%twoCELLSperiod           = .false.
    gdp%gdimbound%TYPEaguuAGVV             = 0
    gdp%gdimbound%TYPEangleCURV            = 1
    gdp%gdimbound%typeCOMPcurvSMALL        = 2
    gdp%gdimbound%TYPEdistrBANKerod        = 1
    gdp%gdimbound%typeEXTRAPstencil        = 2                    ! 0 : no extrap 
                                                                  ! 1 : extrap kcs=2, including points between two kcs=2, 
                                                                  !     but point between kcs=2 and kcs=0 are not part of the stencil. 
                                                                  ! 2 : extrap kcs=2, including in the stencil points between two kcs=2 
                                                                  !     and points between kcs=2 and kcs=0
    gdp%gdimbound%typeEXTRAPux             = 2
    gdp%gdimbound%TYPEfreeSLIP             = 1                    ! 0: "exact" free slip. 1:Hartmann approx
    gdp%gdimbound%TYPEgradDEFERRuzd        = 6
    gdp%gdimbound%typeHART                 = 1
    gdp%gdimbound%typeHUDPU                = 0
    gdp%gdimbound%TYPEinterpVELcurv        = 1
                                                                  ! 1 : only active cut edges
    gdp%gdimbound%TYPEpartIMPLgrad         = 3
    gdp%gdimbound%TYPEtauBANK              = 1
    gdp%gdimbound%TYPEtauCRbank            = 0
    gdp%gdimbound%typeVEGencr              = 0
    gdp%gdimbound%typeVIRTmergeUPDbed      = 3
    gdp%gdimbound%typeVIRTmergeUPDdepth    = 3
    gdp%gdimbound%typeVIRTmergeUPDvert     = 3
    gdp%gdimbound%UavWETtau                = .false.
    gdp%gdimbound%uAXIS                    = -999999999._fp
    gdp%gdimbound%use_DPSavg_for_qtot      = .false.
    gdp%gdimbound%useCUTstyle              = .FALSE.
    gdp%gdimbound%USEfixedBEDequilQb       = .false.
    gdp%gdimbound%USEfixedBEDequilQS       = .false.
    gdp%gdimbound%useFULL                  = .false.
    gdp%gdimbound%VERSIONprecisePOROSbaric = 1
    gdp%gdimbound%virtualLINK              = .false.
    gdp%gdimbound%virtuallinkSMOw1         = .false.
    gdp%gdimbound%virtualMERGEdisch        = .false.
    gdp%gdimbound%virtualMERGEupdBED       = .false.
    gdp%gdimbound%virtualMERGEupdCONC      = .false.
    gdp%gdimbound%virtualMERGEupdDEPTH     = .false.
    gdp%gdimbound%virtualMERGEupdVERT      = .false.
    gdp%gdimbound%vvvSECord                = .false.
    gdp%gdimbound%WAQUAfullyCENTRED        = .false.
    gdp%gdimbound%zavg_global              = .false.
!   start IBM_research variables, most of them will be eventually removed
    gdp%gdimbound%DPUhuSECONDorder         = .FALSE.
    gdp%gdimbound%noCUTfac                 = .false.
    gdp%gdimbound%FORCEfrict_sud           = .false.
    gdp%gdimbound%FORCEfrict_uzd           = .false.
    gdp%gdimbound%FORCEghost_sud1          = .false.
    gdp%gdimbound%FORCEghost_sud2          = .false.
    gdp%gdimbound%FORCEghost_uzd1          = .false.
    gdp%gdimbound%FORCEghost_uzd2          = .false.
    gdp%gdimbound%FORCEgradS1_sud          = .false.
    gdp%gdimbound%FORCEgradS1_uzd          = .false.
    gdp%gdimbound%forceN                   = .false.
    gdp%gdimbound%FORCEnormBIinFINDbi      = .false.
    gdp%gdimbound%forceQYKallSTEPS         = .false.
    gdp%gdimbound%FORCEs1CIRCanal          = .false.
    gdp%gdimbound%forceU0_sud              = .false.
    gdp%gdimbound%FORCEu1_sud              = .false.
    gdp%gdimbound%FORCEu1_uzd              = .false.
    gdp%gdimbound%FORCEuAThPERbnd          = .false.
    gdp%gdimbound%FORCEududx_sud           = .false.
    gdp%gdimbound%FORCEududx_uzd           = .false.
    gdp%gdimbound%FORCEvdudy_sud           = .false.
    gdp%gdimbound%FORCEvdudy_uzd           = .false.
    gdp%gdimbound%forceVVVsud              = .false.
    gdp%gdimbound%forceVVVuzd              = .false. 
    gdp%gdimbound%implDEFsud               = .false.    
    gdp%gdimbound%deferredS1sud            = .false.
    gdp%gdimbound%deferredS1uzd            = .false.    
    gdp%gdimbound%TYPEgradDEFERRsud        = 7    
    gdp%gdimbound%vFACsudITER              = .false.
    gdp%gdimbound%vFACTORcutEDGESx         = .false.
    gdp%gdimbound%vFACTORcutEDGESy         = .false. 
    gdp%gdimbound%hu2SUDiter               = .false.
    gdp%gdimbound%huRHS                    = .false.    
    gdp%gdimbound%extrADVECTsud            = .false.    
    gdp%gdimbound%TYPEofFORCING            = 0                    ! 0 : all active grid points are forced  
    gdp%gdimbound%skipBOUNDvelSUD          = .FALSE. 
    gdp%gdimbound%FIXEDcoastBANKS          = .false.    
!   end IBM_research    
    !
end subroutine initimbound