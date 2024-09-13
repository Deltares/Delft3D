subroutine PLIC_VOF_INIT(mmax      ,nmax      ,nmaxus  ,kmax     ,lunscr   ,  &
                     &   lundia    ,dt        ,irov    ,itstrt   ,nlb      ,  &
                     &   nub       ,mlb       ,mub     ,nmlb     ,nmub     ,  &
                     &   kcs       ,kcu       ,kcv     ,kfs      ,kfu      ,  &
                     &   kfv       ,restid    ,lsed                        ,  &
                     &   xcor      ,ycor      ,guu     ,gvv                ,  &
                     &   dps                                               ,  &
                     &   porosu    ,porosv                                 ,  &
                     &   qxk       ,qyk       ,umean   ,vmean    ,thick    ,  &
                     &   hu        ,hv        ,dpu     ,dpv                ,  &
                     &   s1        ,u1        ,v1      ,gsqs     ,gdp)
!
!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2012.                                
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
!  $Id: PLIC_VOF_INIT.F90 7020 2017-02-16 21:50:45Z platzek $
!  $HeadURL: 
!!--description-----------------------------------------------------------------
!
!    Function:  
!
! Method used:
!
!!--pseudo code and references--------------------------------------------------
!
!   Author: Alberto Canestrelli
!
!!--declarations----------------------------------------------------------------
!
!    USE IFCORE
!    use, intrinsic :: ieee_exceptions

    use globaldata
    use dfparall
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    integer                       , pointer :: nprintVOF
    integer                       , pointer :: Nedge
    integer                       , pointer :: totGHOSTu1
    integer                       , pointer :: totGHOSTv1
    integer                       , pointer :: totGHOSTs1
    integer                       , pointer :: idebugCUT
    integer                       , pointer :: dim_NMlist
    integer, dimension(:,:)       , pointer :: FREEs1_u
    integer, dimension(:,:)       , pointer :: FREEs1_v
    integer, dimension(:,:)       , pointer :: kfs_cc
    integer, dimension(:,:)       , pointer :: kWDu
    integer, dimension(:,:)       , pointer :: kWDv
    integer, dimension(:,:)       , pointer :: multEXITu
    integer, dimension(:,:)       , pointer :: multEXITv
    integer, dimension(:)         , pointer :: EDGEdry
    integer, dimension(:,:)       , pointer :: Ndry_GRS
    integer, dimension(:,:)       , pointer :: Nwet_GRS
    integer, dimension(:,:,:)     , pointer :: nAD
    integer, dimension(:,:,:)     , pointer :: mAD
    integer, dimension(:,:,:)     , pointer :: EDGEtypeBANK
    integer, dimension(:,:,:)     , pointer :: EDGEtypeBANKerod
    integer, dimension(:,:,:)     , pointer :: STOREedge2
    integer, dimension(:,:,:)     , pointer :: EDGEtypeDRY
    integer, dimension(:,:)       , pointer :: U1inDRY
    integer, dimension(:,:)       , pointer :: V1inDRY
    integer, dimension(:,:)       , pointer :: por012
    integer, dimension(:,:)       , pointer :: FROMmnTOghostS1
    integer, dimension(:,:)       , pointer :: FROMmnTOghostU1
    integer, dimension(:,:)       , pointer :: FROMmnTOghostV1
    integer, dimension(:)         , pointer :: nGPs1
    integer, dimension(:)         , pointer :: mGPs1
    integer, dimension(:)         , pointer :: nGPu1
    integer, dimension(:)         , pointer :: mGPu1
    integer, dimension(:)         , pointer :: nGPv1
    integer, dimension(:)         , pointer :: mGPv1
    integer, dimension(:)         , pointer :: mIPs1
    integer, dimension(:)         , pointer :: nIPs1
    integer, dimension(:)         , pointer :: mBIs1
    integer, dimension(:)         , pointer :: nBIs1
    integer, dimension(:)         , pointer :: mIPu1
    integer, dimension(:)         , pointer :: nIPu1
    integer, dimension(:)         , pointer :: mBIu1
    integer, dimension(:)         , pointer :: nBIu1
    integer, dimension(:)         , pointer :: mIPv1
    integer, dimension(:)         , pointer :: nIPv1
    integer, dimension(:)         , pointer :: mBIv1
    integer, dimension(:)         , pointer :: nBIv1
    integer, dimension(:)         , pointer :: MERGEDwith_d
    integer, dimension(:)         , pointer :: MERGEDwith_w
    integer, dimension(:)         , pointer :: MERGEDwith_bed
    integer, dimension(:)         , pointer :: isMERGEDu_bed
    integer, dimension(:)         , pointer :: isMERGEDv_bed
    integer, dimension(:)         , pointer :: isMERGEDu_w
    integer, dimension(:)         , pointer :: isMERGEDv_w
    integer, dimension(:)         , pointer :: isMERGEDu_d
    integer, dimension(:)         , pointer :: isMERGEDv_d
    integer, dimension(:,:)       , pointer :: NMlistMERGED_bed
    integer, dimension(:,:)       , pointer :: NMlistMERGED_w
    integer, dimension(:,:)       , pointer :: NMlistMERGED_d
    integer, dimension(:)         , pointer :: neuMERG
    integer, dimension(:)         , pointer :: Nmerged_bed
    integer, dimension(:)         , pointer :: Nmerged_w
    integer, dimension(:)         , pointer :: Nmerged_d
    integer, dimension(:,:)       , pointer :: inSTENCILu
    integer, dimension(:,:)       , pointer :: inSTENCILv
    integer, dimension(:,:)       , pointer :: GHOSTs1
    integer, dimension(:,:)       , pointer :: GHOSTu1
    integer, dimension(:,:)       , pointer :: GHOSTv1
    integer, dimension(:,:)       , pointer :: kfu_cc
    integer, dimension(:,:)       , pointer :: kfv_cc
    integer, dimension(:,:)       , pointer :: kFLcut
    integer, dimension(:,:)       , pointer :: SURFinDRY
    integer, dimension(:)         , pointer :: kGPumin
    integer, dimension(:)         , pointer :: kGPvmin
    integer, dimension(:)         , pointer :: kGPumax
    integer, dimension(:)         , pointer :: kGPvmax
    integer, dimension(:)         , pointer :: INTERFtype
    real(fp), dimension(:,:)      , pointer :: sourseBANK
    real(fp), dimension(:,:)      , pointer :: dpU0
    real(fp), dimension(:,:)      , pointer :: dpV0
    real(prec), dimension(:,:)    , pointer :: dps0
    real(fp), dimension(:,:)      , pointer :: Eb
    real(fp), dimension(:,:,:)    , pointer :: EbK
    real(fp), dimension(:,:)      , pointer :: dpH
    real(fp), dimension(:,:)      , pointer :: dpL
    real(fp), dimension(:,:)      , pointer :: dpsi
    real(fp), dimension(:,:)      , pointer :: deta
    real(fp), dimension(:,:)      , pointer :: POROS
    real(fp), dimension(:,:)      , pointer :: POROSold
    real(fp), dimension(:,:)      , pointer :: alphaD
    real(fp), dimension(:,:)      , pointer :: PSIx
    real(fp), dimension(:,:)      , pointer :: PSIy
    real(fp), dimension(:,:)      , pointer :: ETAx
    real(fp), dimension(:,:)      , pointer :: ETAy
    real(fp), dimension(:,:,:)    , pointer :: dutdn_U
    real(fp), dimension(:,:,:)    , pointer :: dutdn_V
    real(fp), dimension(:,:)      , pointer :: deltaUcut
    real(fp), dimension(:)        , pointer :: deltaS1cut
    real(fp), dimension(:,:,:)    , pointer :: EDGElenBANK
    real(fp), dimension(:,:,:)    , pointer :: EDGElenDRYeff
    real(fp), dimension(:,:,:,:,:), pointer :: EDGExyBANK
    real(fp), dimension(:,:,:,:,:), pointer :: EDGExyBANKerod
    real(fp), dimension(:,:,:,:,:), pointer :: EDGExyDRY
    real(fp), dimension(:,:,:)    , pointer :: tauBANK
    real(fp), dimension(:,:,:)    , pointer :: STOREedgeLEN
    real(fp), dimension(:,:,:)    , pointer :: EDGElenWET
    real(fp), dimension(:,:)      , pointer :: gsqs_cc
    real(fp), dimension(:,:)      , pointer :: guu_cc
    real(fp), dimension(:,:)      , pointer :: gvv_cc
    real(fp), dimension(:,:)      , pointer :: agsqs
    real(fp), dimension(:)        , pointer :: agsqs_link
    real(fp), dimension(:,:)      , pointer :: aguu
    real(fp), dimension(:,:)      , pointer :: agvv
    real(fp), dimension(:,:)      , pointer :: aguu0
    real(fp), dimension(:,:)      , pointer :: agvv0
    real(fp), dimension(:,:,:)    , pointer :: qxk_tinyCUT
    real(fp), dimension(:,:,:)    , pointer :: qyk_tinyCUT
    real(fp), dimension(:,:)      , pointer :: xcor0
    real(fp), dimension(:,:)      , pointer :: ycor0
    real(fp), dimension(:,:)      , pointer :: xcorU1
    real(fp), dimension(:,:)      , pointer :: ycorU1
    real(fp), dimension(:,:)      , pointer :: xcorV1
    real(fp), dimension(:,:)      , pointer :: ycorV1
    real(fp), dimension(:,:)      , pointer :: PSIcor0
    real(fp), dimension(:,:)      , pointer :: ETAcor0
    real(fp), dimension(:,:)      , pointer :: PSIcor
    real(fp), dimension(:,:)      , pointer :: ETAcor
    real(fp), dimension(:,:)      , pointer :: PSIcorU1
    real(fp), dimension(:,:)      , pointer :: ETAcorU1
    real(fp), dimension(:,:)      , pointer :: PSIcorV1
    real(fp), dimension(:,:)      , pointer :: ETAcorV1
    real(fp), dimension(:,:)      , pointer :: xG
    real(fp), dimension(:,:)      , pointer :: yG
    real(fp), dimension(:,:)      , pointer :: psiG
    real(fp), dimension(:,:)      , pointer :: etaG
    real(fp), dimension(:,:,:)    , pointer :: dxk
    real(fp), dimension(:,:,:)    , pointer :: dyk
    real(fp), dimension(:,:,:)    , pointer :: xcell
    real(fp), dimension(:,:,:)    , pointer :: ycell
    real(fp), dimension(:,:)      , pointer :: Npsi
    real(fp), dimension(:,:)      , pointer :: Neta
    real(fp), dimension(:,:)      , pointer :: DpsiG
    real(fp), dimension(:,:)      , pointer :: DetaG
    real(fp), dimension(:,:)      , pointer :: xG_V1
    real(fp), dimension(:,:)      , pointer :: xG_U1
    real(fp), dimension(:,:)      , pointer :: yG_V1
    real(fp), dimension(:,:)      , pointer :: yG_U1
    real(fp), dimension(:)        , pointer :: xIPs1
    real(fp), dimension(:)        , pointer :: yIPs1
    real(fp), dimension(:)        , pointer :: xBIs1
    real(fp), dimension(:)        , pointer :: yBIs1
    real(fp), dimension(:)        , pointer :: s1IP
    real(fp), dimension(:)        , pointer :: xIPu1
    real(fp), dimension(:)        , pointer :: yIPu1
    real(fp), dimension(:)        , pointer :: xBIu1
    real(fp), dimension(:)        , pointer :: yBIu1
    real(fp), dimension(:)        , pointer :: u1IP
    real(fp), dimension(:,:)      , pointer :: u1IPu
    real(fp), dimension(:,:)      , pointer :: u1IPv
    real(fp), dimension(:,:)      , pointer :: v1IPu
    real(fp), dimension(:,:)      , pointer :: v1IPv
    real(fp), dimension(:)        , pointer :: xIPv1
    real(fp), dimension(:)        , pointer :: yIPv1
    real(fp), dimension(:)        , pointer :: xBIv1
    real(fp), dimension(:)        , pointer :: yBIv1
    real(fp), dimension(:)        , pointer :: v1IP
    real(fp), dimension(:)        , pointer :: nxG_S1
    real(fp), dimension(:)        , pointer :: nyG_S1
    real(fp), dimension(:)        , pointer :: nxG_U1
    real(fp), dimension(:)        , pointer :: nyG_U1
    real(fp), dimension(:)        , pointer :: nxG_V1
    real(fp), dimension(:)        , pointer :: nyG_V1
    real(fp), dimension(:,:)      , pointer :: psiG_U1
    real(fp), dimension(:,:)      , pointer :: psiG_V1
    real(fp), dimension(:,:)      , pointer :: etaG_U1
    real(fp), dimension(:,:)      , pointer :: etaG_V1
    real(fp), dimension(:)        , pointer :: DISTs1
    real(fp), dimension(:)        , pointer :: DISTu1
    real(fp), dimension(:)        , pointer :: DISTv1
    real(fp), dimension(:)        , pointer :: dzduu_w
    real(fp), dimension(:)        , pointer :: dzdvv_w
    real(fp), dimension(:)        , pointer :: dzduuCENTR
    real(fp), dimension(:)        , pointer :: dzdvvCENTR
    real(fp), dimension(:,:)      , pointer :: z_aguu
    real(fp), dimension(:,:)      , pointer :: z_agvv
    real(fp), dimension(:,:)      , pointer :: z_agsqs
    real(fp), dimension(:,:)      , pointer :: shiftBIv_x
    real(fp), dimension(:,:)      , pointer :: shiftBIv_y
    real(fp), dimension(:,:)      , pointer :: shiftBIu_x
    real(fp), dimension(:,:)      , pointer :: shiftBIu_y
    real(fp), dimension(:,:)      , pointer :: dpLnew
    real(fp), dimension(:,:)      , pointer :: TIMElowDEPTH
    real(fp), dimension(:,:)      , pointer :: TIMEsubmerged
    real(fp), dimension(:,:)      , pointer :: VOLeros
    real(fp), dimension(:)        , pointer :: sourceU
    real(fp), dimension(:)        , pointer :: gradS1_sud
    real(fp), dimension(:)        , pointer :: gradS1_uzd
    real(fp), dimension(:)        , pointer :: frict_sud
    real(fp), dimension(:)        , pointer :: frict_uzd
    real(fp), dimension(:)        , pointer :: vdudy
    real(fp), dimension(:)        , pointer :: ududx
    real(fp), dimension(:)        , pointer :: xintU
    real(fp), dimension(:)        , pointer :: yintU
    real(fp), dimension(:)        , pointer :: dpuFAC
    real(fp), dimension(:)        , pointer :: huFAC
    real(fp), dimension(:,:)      , pointer :: eeC
    real(fp), dimension(:,:)      , pointer :: eeR
    real(fp), dimension(:,:)      , pointer :: eeL
    real(fp), dimension(:)        , pointer :: EXPsouC
    real(fp), dimension(:)        , pointer :: EXPsouR
    real(fp), dimension(:)        , pointer :: EXPsouL
    logical, dimension(:,:)       , pointer :: CELLtoRECON
    logical, dimension(:,:)       , pointer :: CELLadjCUT
    logical, dimension(:,:)       , pointer :: updatedBANK
    logical, dimension(:,:)       , pointer :: newGHOSTu
    logical, dimension(:,:)       , pointer :: newGHOSTv
    logical, dimension(:,:)       , pointer :: oneEXIT
    logical                       , pointer :: EXACTpolygons
    logical                       , pointer :: periodSURFACE
!   start IBM_research variables, most of them will be eventually removed
    real(fp), dimension(:)        , pointer :: cutfac
    real(fp), dimension(:,:,:)    , pointer :: uFACcut
    real(fp), dimension(:,:)      , pointer :: uFACcutINV    
    real(fp), dimension(:,:,:)    , pointer :: vFACcut    
!   end IBM_research   
!
! Global variables
!  
    real(fp), dimension(nlb:nub,mlb:mub)       , intent(inout) :: gsqs  
    real(fp), dimension(nlb:nub,mlb:mub)       , intent(inout) :: s1
    real(fp), dimension(nlb:nub,mlb:mub)       , intent(inout) :: dpu
    real(fp), dimension(nlb:nub,mlb:mub)       , intent(inout) :: dpv 
    real(fp), dimension(nlb:nub,mlb:mub)       , intent(inout) :: hu 
    real(fp), dimension(nlb:nub,mlb:mub)       , intent(inout) :: hv  
    real(fp), dimension(nlb:nub,mlb:mub)       , intent(inout) :: Umean 
    real(fp), dimension(nlb:nub,mlb:mub)       , intent(inout) :: Vmean      
    real(fp), dimension(nlb:nub,mlb:mub)       , intent(inout) :: guu   
    real(fp), dimension(nlb:nub,mlb:mub)       , intent(inout) :: gvv                      
    real(fp), dimension(nlb:nub,mlb:mub, kmax) , intent(inout) :: u1
    real(fp), dimension(nlb:nub,mlb:mub, kmax) , intent(inout) :: v1
    real(fp), dimension(nlb:nub,mlb:mub, kmax) , intent(in)    :: porosu 
    real(fp), dimension(nlb:nub,mlb:mub, kmax) , intent(in)    :: porosv
    real(fp), dimension(nlb:nub,mlb:mub, kmax) , intent(inout) :: qxk  
    real(fp), dimension(nlb:nub,mlb:mub, kmax) , intent(inout) :: qyk
    real(fp), dimension(nlb:nub,mlb:mub)       , intent(in)    :: xcor
    real(fp), dimension(nlb:nub,mlb:mub)       , intent(in)    :: ycor
    real(prec), dimension(nlb:nub,mlb:mub)     , intent(inout) :: dps 
    real(fp), dimension(kmax)                  , intent(inout) :: thick
    integer, dimension(nlb:nub,mlb:mub)        , intent(in)    :: kcs 
    integer, dimension(nlb:nub,mlb:mub)        , intent(in)    :: kcu 
    integer, dimension(nlb:nub,mlb:mub)        , intent(in)    :: kcv
    integer, dimension(nlb:nub,mlb:mub)        , intent(inout) :: kfs
    integer, dimension(nlb:nub,mlb:mub)        , intent(inout) :: kfu
    integer, dimension(nlb:nub,mlb:mub)        , intent(inout) :: kfv

    integer                                                             , intent(in)    :: mmax   !  Description and declaration in iidim.f90
    integer                                                             , intent(in)    :: nmax   !  Description and declaration in iidim.f90
    integer                                                             , intent(in)    :: nmaxus   !  Description and declaration in iidim.f90
    integer                                                             , intent(in)    :: kmax   !  Description and declaration in iidim.f90
    integer                                                             , intent(in)    :: lunscr
    integer                                                             , intent(in)    :: lundia
    integer                                                             , intent(in)    :: itstrt
    integer                                                             , intent(in)    :: irov 
    integer                                                             , intent(in)    :: nlb     
    integer                                                             , intent(in)    :: nub     
    integer                                                             , intent(in)    :: mlb
    integer                                                             , intent(in)    :: mub     
    integer                                                             , intent(in)    :: nmlb   
    integer                                                             , intent(in)    :: nmub
    integer                                                             , intent(in)    :: lsed
!
    real(fp)                                                            , intent(in)    :: dt
!
    character(256)                                                      , intent(in)    :: restid
!
! Local variables
!
!    INTEGER*4 OLD_FPE_FLAGS, NEW_FPE_FLAGS
   ! include 'tri-dyn.igd'
    !include 'fsm.i'
    integer                        :: ii
    integer                        :: j
    integer                        :: N
    integer                        :: M
    integer                        :: mfg,mlg,nfg,nlg,mc,nc
    integer                        :: iocond                 ! Help variable
    integer                        :: luntmp
    integer                        :: lunrgf
    integer                        :: nst
    integer                        :: iPROV
!
    real(fp)                       :: butta
    real(fp)                       :: dx,dxETA
    real(fp)                       :: dy,dyETA
    real(fp)                       :: sig(2)
    real(fp)                       :: detaDIV2
    real(fp)                       :: dpsiDIV2
    real(fp)                       :: Xpeak(2)
    real(fp)                       :: Ypeak(2)
    real(fp)                       :: peak(2)
    real(fp)                       :: gsqsPROV

    real(fp) dpMAXiniW
    real x
!
    logical error
!
    character*100 fildepW,fildepD,filrgf
    character*30 errmsg
    character(256)                        :: rec 
    character(4)                          :: errornr  ! Number of the errormessage which will be printed in case of error 
    character(10)                         :: dum     
!
! executable statements -------------------------------------------------------
!
    nprintVOF        => gdp%gdimbound%nprintVOF
    Nedge            => gdp%gdimbound%Nedge
    totGHOSTu1       => gdp%gdimbound%totGHOSTu1
    totGHOSTv1       => gdp%gdimbound%totGHOSTv1
    totGHOSTs1       => gdp%gdimbound%totGHOSTs1
    idebugCUT        => gdp%gdimbound%idebugCUT
    dim_NMlist       => gdp%gdimbound%dim_NMlist
    EXACTpolygons    => gdp%gdimbound%EXACTpolygons
    periodSURFACE    => gdp%gdimbound%periodSURFACE
    !
    ! Maximum 4 small adjacent sharing an edge to merge (plus the cell itself => dim_NMlist=5). +15 to allow for multiple merging of narrow stripes
    !
    dim_NMlist = 5+15
    !
    allocate (gdp%gdimbound%dpU0        (nlb:nub,mlb:mub))
    allocate (gdp%gdimbound%dpV0        (nlb:nub,mlb:mub))
    allocate (gdp%gdimbound%dps0        (nlb:nub,mlb:mub))
    allocate (gdp%gdimbound%Npsi        (1:nmax,1:mmax))
    allocate (gdp%gdimbound%Neta        (1:nmax,1:mmax))
    allocate (gdp%gdimbound%alphaD      (1:nmax,1:mmax))
    allocate (gdp%gdimbound%gsqs_cc     (1:nmax,1:mmax))
    allocate (gdp%gdimbound%guu_cc      (1:nmax,1:mmax))
    allocate (gdp%gdimbound%gvv_cc      (1:nmax,1:mmax))
    allocate (gdp%gdimbound%EDGElenWET  (nmax,mmax,4))
    !allocate (gdp%gdimbound%agsqs       (nlb:nub,mlb:mub))
    allocate (gdp%gdimbound%qxk_tinyCUT (nlb:nub,mlb:mub,1:kmax))
    allocate (gdp%gdimbound%qyk_tinyCUT (nlb:nub,mlb:mub,1:kmax))
    allocate (gdp%gdimbound%updatedBANK (1:nmax,1:mmax))
    allocate (gdp%gdimbound%xcor0       (0:nmax,0:mmax))
    allocate (gdp%gdimbound%ycor0       (0:nmax,0:mmax))
    allocate (gdp%gdimbound%kfu_cc      (nmax,mmax))
    allocate (gdp%gdimbound%kfv_cc      (nmax,mmax))      
    allocate (gdp%gdimbound%kFLcut      (nlb:nub,mlb:mub))   
    allocate (gdp%gdimbound%PSIcor0     (0:nmax,0:mmax))
    allocate (gdp%gdimbound%ETAcor0     (0:nmax,0:mmax))      
    allocate (gdp%gdimbound%PSIcorU1    (nlb:nub,mlb-1:mub-1))      
    allocate (gdp%gdimbound%ETAcorU1    (nlb:nub,mlb-1:mub-1))      
    allocate (gdp%gdimbound%PSIcorV1    (nlb-1:nub-1,mlb:mub))      
    allocate (gdp%gdimbound%ETAcorV1    (nlb-1:nub-1,mlb:mub)) 
    allocate (gdp%gdimbound%dpsi        (nmax,mmax))      
    allocate (gdp%gdimbound%deta        (nmax,mmax)) 
    allocate (gdp%gdimbound%xG          (nlb:nub,mlb:mub))      
    allocate (gdp%gdimbound%yG          (nlb:nub,mlb:mub))      
    allocate (gdp%gdimbound%psiG        (nmax,mmax))      
    allocate (gdp%gdimbound%etaG        (nmax,mmax))      
    allocate (gdp%gdimbound%PSIcor      (nlb:nub,mlb:mub))      
    allocate (gdp%gdimbound%ETAcor      (nlb:nub,mlb:mub))
 
    allocate (gdp%gdimbound%kfs_cc   (nlb:nub,mlb:mub))      
    allocate (gdp%gdimbound%SURFinDRY(nmax,mmax))      
    allocate (gdp%gdimbound%U1inDRY  (nmax,mmax))      
    allocate (gdp%gdimbound%V1inDRY  (nmax,mmax))      
    allocate (gdp%gdimbound%FREEs1_u (nmax,mmax))      
    allocate (gdp%gdimbound%FREEs1_v (nmax,mmax)) 
    allocate (gdp%gdimbound%xcell    (5,nmax,mmax))    !if RESHAPE_CYCLE  it has to be (5,nlb:nub,mlb:mub)
    allocate (gdp%gdimbound%ycell    (5,nmax,mmax))    !if RESHAPE_CYCLE  it has to be (5,nlb:nub,mlb:mub) 
    allocate (gdp%gdimbound%PSIx     (nlb:nub,mlb:mub))      
    allocate (gdp%gdimbound%PSIy     (nlb:nub,mlb:mub))      
    allocate (gdp%gdimbound%ETAx     (nlb:nub,mlb:mub))      
    allocate (gdp%gdimbound%ETAy     (nlb:nub,mlb:mub))      
    allocate (gdp%gdimbound%DpsiG    (nmax,mmax-1))      
    allocate (gdp%gdimbound%DetaG    (nmax-1,mmax))
 
    allocate (gdp%gdimbound%Ndry_GRS        (nlb:nub,mlb:mub))      
    allocate (gdp%gdimbound%Nwet_GRS        (nlb:nub,mlb:mub))           
    allocate (gdp%gdimbound%EDGEdry         (nmax*mmax*2))      
    allocate (gdp%gdimbound%nAD             (nmax,mmax,6))      
    allocate (gdp%gdimbound%mAD             (nmax,mmax,6))      
    allocate (gdp%gdimbound%STOREedge2      (4,nmax,mmax))      
    allocate (gdp%gdimbound%STOREedgeLEN    (4,nmax,mmax))      
    allocate (gdp%gdimbound%EDGEtypeDRY     (4,nmax,mmax))      
    allocate (gdp%gdimbound%EDGElenBANK     (4,nmax,mmax))      
    allocate (gdp%gdimbound%EDGElenDRYeff   (4,nmax,mmax))      
    allocate (gdp%gdimbound%EDGExyBANK      (nlb:nub,mlb:mub,4,2,2))
    allocate (gdp%gdimbound%EDGExyDRY       (nmax,mmax,4,2,2))      
    allocate (gdp%gdimbound%EDGExyBANKerod  (nmax,mmax,4,2,2))
    allocate (gdp%gdimbound%GHOSTs1         (nlb:nub,mlb:mub))      
    allocate (gdp%gdimbound%GHOSTu1         (nlb:nub,mlb:mub))      
    allocate (gdp%gdimbound%GHOSTv1         (nlb:nub,mlb:mub))      
    allocate (gdp%gdimbound%nGPs1           (nmax*mmax))      
    allocate (gdp%gdimbound%nGPu1           (nmax*mmax))      
    allocate (gdp%gdimbound%nGPv1           (nmax*mmax))      
    allocate (gdp%gdimbound%mGPs1           (nmax*mmax))      
    allocate (gdp%gdimbound%mGPu1           (nmax*mmax))      
    allocate (gdp%gdimbound%mGPv1           (nmax*mmax))      
    allocate (gdp%gdimbound%xIPs1           (nmax*mmax))      
    allocate (gdp%gdimbound%yIPs1           (nmax*mmax))      
    allocate (gdp%gdimbound%xBIs1           (nmax*mmax))      
    allocate (gdp%gdimbound%yBIs1           (nmax*mmax))      
    allocate (gdp%gdimbound%xIPu1           (nmax*mmax))      
    allocate (gdp%gdimbound%yIPu1           (nmax*mmax))      
    allocate (gdp%gdimbound%xBIu1           (nmax*mmax))      
    allocate (gdp%gdimbound%yBIu1           (nmax*mmax))      
    allocate (gdp%gdimbound%xIPv1           (nmax*mmax))      
    allocate (gdp%gdimbound%yIPv1           (nmax*mmax))      
    allocate (gdp%gdimbound%xBIv1           (nmax*mmax))      
    allocate (gdp%gdimbound%yBIv1           (nmax*mmax))      
    allocate (gdp%gdimbound%mIPs1           (nmax*mmax))      
    allocate (gdp%gdimbound%nIPs1           (nmax*mmax))      
    allocate (gdp%gdimbound%mBIs1           (nmax*mmax))      
    allocate (gdp%gdimbound%nBIs1           (nmax*mmax))      
    allocate (gdp%gdimbound%s1IP            (nmax*mmax))      
    allocate (gdp%gdimbound%mIPu1           (nmax*mmax))      
    allocate (gdp%gdimbound%nIPu1           (nmax*mmax))      
    allocate (gdp%gdimbound%mBIu1           (nmax*mmax))      
    allocate (gdp%gdimbound%nBIu1           (nmax*mmax))      
    allocate (gdp%gdimbound%u1IP            (nmax*mmax))      
    allocate (gdp%gdimbound%mIPv1           (nmax*mmax))      
    allocate (gdp%gdimbound%nIPv1           (nmax*mmax))      
    allocate (gdp%gdimbound%mBIv1           (nmax*mmax))      
    allocate (gdp%gdimbound%nBIv1           (nmax*mmax))      
    allocate (gdp%gdimbound%v1IP            (nmax*mmax))      
    allocate (gdp%gdimbound%u1IPu           (nmax*mmax,kmax))      
    allocate (gdp%gdimbound%u1IPv           (nmax*mmax,kmax))      
    allocate (gdp%gdimbound%v1IPu           (nmax*mmax,kmax))      
    allocate (gdp%gdimbound%v1IPv           (nmax*mmax,kmax))      
    allocate (gdp%gdimbound%DISTs1          (nmax*mmax))      
    allocate (gdp%gdimbound%DISTu1          (nmax*mmax))      
    allocate (gdp%gdimbound%DISTv1          (nmax*mmax))      
    allocate (gdp%gdimbound%nxG_S1          (nmax*mmax))      
    allocate (gdp%gdimbound%nyG_S1          (nmax*mmax))      
    allocate (gdp%gdimbound%nxG_U1          (nmax*mmax))      
    allocate (gdp%gdimbound%nyG_U1          (nmax*mmax))      
    allocate (gdp%gdimbound%nxG_V1          (nmax*mmax))      
    allocate (gdp%gdimbound%nyG_V1          (nmax*mmax))
    allocate (gdp%gdimbound%FROMmnTOghostS1 (nmax,mmax))      
    allocate (gdp%gdimbound%FROMmnTOghostU1 (nmax,mmax))      
    allocate (gdp%gdimbound%FROMmnTOghostV1 (nmax,mmax))      
    allocate (gdp%gdimbound%por012          (nlb:nub,mlb:mub))      
    allocate (gdp%gdimbound%inSTENCILu      (nlb:nub,mlb:mub))      
    allocate (gdp%gdimbound%inSTENCILv      (nlb:nub,mlb:mub))          
    allocate (gdp%gdimbound%dxk             (nlb:nub,mlb:mub,4))      
    allocate (gdp%gdimbound%dyk             (nlb:nub,mlb:mub,4))      
    allocate (gdp%gdimbound%dutdn_U         (nlb:nub,mlb:mub,kmax))      
    allocate (gdp%gdimbound%dutdn_V         (nlb:nub,mlb:mub,kmax))
    allocate (gdp%gdimbound%xG_V1           (nlb:nub,mlb:mub))      
    allocate (gdp%gdimbound%xG_U1           (nlb:nub,mlb:mub))      
    allocate (gdp%gdimbound%yG_V1           (nlb:nub,mlb:mub))      
    allocate (gdp%gdimbound%yG_U1           (nlb:nub,mlb:mub)) 
    allocate (gdp%gdimbound%porosOLD        (nlb:nub,mlb:mub)) 
    allocate (gdp%gdimbound%psiG_U1         (nlb:nub,mlb:mub))      
    allocate (gdp%gdimbound%psiG_V1         (nlb:nub,mlb:mub)) 
    allocate (gdp%gdimbound%etaG_U1         (nlb:nub,mlb:mub))      
    allocate (gdp%gdimbound%etaG_V1         (nlb:nub,mlb:mub)) 
    allocate (gdp%gdimbound%shiftBIu_x      (nlb:nub,mlb:mub))      
    allocate (gdp%gdimbound%shiftBIu_y      (nlb:nub,mlb:mub))      
    allocate (gdp%gdimbound%shiftBIv_x      (nlb:nub,mlb:mub))      
    allocate (gdp%gdimbound%shiftBIv_y      (nlb:nub,mlb:mub))
    allocate (gdp%gdimbound%xcorU1          (nlb:nub,mlb-1:mub-1))
    allocate (gdp%gdimbound%ycorU1          (nlb:nub,mlb-1:mub-1))
    allocate (gdp%gdimbound%xcorV1          (nlb-1:nub-1,mlb:mub))
    allocate (gdp%gdimbound%ycorV1          (nlb-1:nub-1,mlb:mub))     
    allocate (gdp%gdimbound%aguu0           (nlb:nub,mlb:mub))      
    allocate (gdp%gdimbound%agvv0           (nlb:nub,mlb:mub))
    allocate (gdp%gdimbound%CELLtoRECON     (Nmax,Mmax))      
    allocate (gdp%gdimbound%CELLadjCUT      (Nmax,Mmax))  
    allocate (gdp%gdimbound%oneEXIT         (nlb:nub,mlb:mub))      
    allocate (gdp%gdimbound%multEXITu       (nlb:nub,mlb:mub))      
    allocate (gdp%gdimbound%multEXITv       (nlb:nub,mlb:mub))
    allocate (gdp%gdimbound%newGHOSTu       (nlb:nub,mlb:mub))      
    allocate (gdp%gdimbound%newGHOSTv       (nlb:nub,mlb:mub))
    !
    ! Allocation with multiple dimensions unrolled in one dimension 
    ! Maximum 4 small adjacent sharing an edge to merge (plus the cell itself => dim_NMlist=5).
    !
    allocate (gdp%gdimbound%dzduu_w          (nmlb:nmub))      
    allocate (gdp%gdimbound%dzdvv_w          (nmlb:nmub))      
    allocate (gdp%gdimbound%dzduuCENTR       (nmlb:nmub))      
    allocate (gdp%gdimbound%dzdvvCENTR       (nmlb:nmub))      
    allocate (gdp%gdimbound%agsqs_link       (nmlb:nmub))      
    allocate (gdp%gdimbound%isMERGEDu_bed    (nmlb:nmub))      
    allocate (gdp%gdimbound%isMERGEDv_bed    (nmlb:nmub))      
    allocate (gdp%gdimbound%NMlistMERGED_bed (dim_NMlist,nmlb:nmub))      
    allocate (gdp%gdimbound%Nmerged_bed      (nmlb:nmub))      
    allocate (gdp%gdimbound%isMERGEDu_w      (nmlb:nmub))      
    allocate (gdp%gdimbound%isMERGEDv_w      (nmlb:nmub))      
    allocate (gdp%gdimbound%NMlistMERGED_w   (dim_NMlist,nmlb:nmub))      
    allocate (gdp%gdimbound%Nmerged_w        (nmlb:nmub))      
    allocate (gdp%gdimbound%isMERGEDu_d      (nmlb:nmub))      
    allocate (gdp%gdimbound%isMERGEDv_d      (nmlb:nmub))      
    allocate (gdp%gdimbound%NMlistMERGED_d   (dim_NMlist,nmlb:nmub))      
    allocate (gdp%gdimbound%Nmerged_d        (nmlb:nmub))      
    allocate (gdp%gdimbound%MERGEDwith_bed   (nmlb:nmub))      
    allocate (gdp%gdimbound%MERGEDwith_d     (nmlb:nmub))      
    allocate (gdp%gdimbound%MERGEDwith_w     (nmlb:nmub))      
    allocate (gdp%gdimbound%INTERFtype       (nmlb:nmub))      
    allocate (gdp%gdimbound%gradS1_sud       (nmlb:nmub))      
    allocate (gdp%gdimbound%gradS1_uzd       (nmlb:nmub))      
    allocate (gdp%gdimbound%vdudy            (nmlb:nmub))      
    allocate (gdp%gdimbound%ududx            (nmlb:nmub))      
    allocate (gdp%gdimbound%frict_uzd        (nmlb:nmub))      
    allocate (gdp%gdimbound%frict_sud        (nmlb:nmub))      
    allocate (gdp%gdimbound%kGPumin          (nmlb:nmub))      
    allocate (gdp%gdimbound%kGPvmin          (nmlb:nmub))      
    allocate (gdp%gdimbound%kGPumax          (nmlb:nmub))      
    allocate (gdp%gdimbound%kGPvmax          (nmlb:nmub))      
    allocate (gdp%gdimbound%xintU            (nmlb:nmub))      
    allocate (gdp%gdimbound%yintU            (nmlb:nmub))      
    allocate (gdp%gdimbound%dpuFAC           (nmlb:nmub))      
    allocate (gdp%gdimbound%huFAC            (nmlb:nmub))      
    allocate (gdp%gdimbound%kWDu             (nmlb:nmub,4))      
    allocate (gdp%gdimbound%kWDv             (nmlb:nmub,4))      
    allocate (gdp%gdimbound%z_aguu           (nmlb:nmub,1:kmax))      
    allocate (gdp%gdimbound%z_agvv           (nmlb:nmub,1:kmax))      
    allocate (gdp%gdimbound%z_agsqs          (nmlb:nmub,1:kmax))      
    allocate (gdp%gdimbound%uFACcutINV       (nmlb:nmub,1:kmax))      
    allocate (gdp%gdimbound%deltaUcut        (nmlb:nmub,1:kmax))      
    allocate (gdp%gdimbound%deltaS1cut       (nmlb:nmub))      
    allocate (gdp%gdimbound%EXPsouC          (nmlb:nmub))      
    allocate (gdp%gdimbound%EXPsouR          (nmlb:nmub))      
    allocate (gdp%gdimbound%EXPsouL          (nmlb:nmub))      
    allocate (gdp%gdimbound%eeC              (nmlb:nmub,1:3))      
    allocate (gdp%gdimbound%eeR              (nmlb:nmub,1:2))      
    allocate (gdp%gdimbound%eeL              (nmlb:nmub,2:3))      
    allocate (gdp%gdimbound%neuMERG          (nmlb:nmub))      
    allocate (gdp%gdimbound%sourceU          (nmlb:nmub))     
    allocate (gdp%gdimbound%sourseBANK       (nmlb:nmub,1:lsed))
    allocate (gdp%gdimbound%tauBANK          (5,nlb:nub,mlb:mub))      
    allocate (gdp%gdimbound%Eb               (nlb:nub,mlb:mub))      
    allocate (gdp%gdimbound%Ebk              (4,nlb:nub,mlb:mub))      
    allocate (gdp%gdimbound%dpLnew           (nlb:nub,mlb:mub))      
    allocate (gdp%gdimbound%VOLeros          (nlb:nub,mlb:mub))      
    allocate (gdp%gdimbound%EDGEtypeBANKerod (4,nlb:nub,mlb:mub))      
    allocate (gdp%gdimbound%TIMElowDEPTH     (nlb:nub,mlb:mub))      
    allocate (gdp%gdimbound%TIMEsubmerged    (nlb:nub,mlb:mub))
!   start IBM_research variables, most of them will be eventually removed
    allocate (gdp%gdimbound%cutfac           (nmlb:nmub))    
    allocate (gdp%gdimbound%uFACcut          (nlb:nub,mlb:mub,kmax))      
    allocate (gdp%gdimbound%vFACcut          (nlb:nub,mlb:mub,kmax))      
!   end IBM_research   
      
    !
    Npsi             => gdp%gdimbound%Npsi
    Neta             => gdp%gdimbound%Neta
    alphaD           => gdp%gdimbound%alphaD    
    aguu             => gdp%gdimbound%aguu
    agvv             => gdp%gdimbound%agvv
    deltaUcut        => gdp%gdimbound%deltaUcut
    deltaS1cut       => gdp%gdimbound%deltaS1cut
    GHOSTs1          => gdp%gdimbound%GHOSTs1
    GHOSTu1          => gdp%gdimbound%GHOSTu1
    GHOSTv1          => gdp%gdimbound%GHOSTv1
    isMERGEDu_bed    => gdp%gdimbound%isMERGEDu_bed
    isMERGEDv_bed    => gdp%gdimbound%isMERGEDv_bed
    kfs_cc           => gdp%gdimbound%kfs_cc
    kGPumin          => gdp%gdimbound%kGPumin
    kGPvmin          => gdp%gdimbound%kGPvmin
    kGPumax          => gdp%gdimbound%kGPumax
    kGPvmax          => gdp%gdimbound%kGPvmax
    kWDu             => gdp%gdimbound%kWDu
    kWDv             => gdp%gdimbound%kWDv
    neuMERG          => gdp%gdimbound%neuMERG
    newGHOSTu        => gdp%gdimbound%newGHOSTu
    newGHOSTv        => gdp%gdimbound%newGHOSTv
    Nmerged_bed      => gdp%gdimbound%Nmerged_bed
    Nmerged_w        => gdp%gdimbound%Nmerged_w
    Nmerged_d        => gdp%gdimbound%Nmerged_d
    por012           => gdp%gdimbound%por012
    POROSold         => gdp%gdimbound%POROSold
    qxk_tinyCUT      => gdp%gdimbound%qxk_tinyCUT
    qyk_tinyCUT      => gdp%gdimbound%qyk_tinyCUT
    shiftBIv_x       => gdp%gdimbound%shiftBIv_x
    shiftBIv_y       => gdp%gdimbound%shiftBIv_y
    shiftBIu_x       => gdp%gdimbound%shiftBIu_x
    shiftBIu_y       => gdp%gdimbound%shiftBIu_y
    sourceU          => gdp%gdimbound%sourceU
    sourseBANK       => gdp%gdimbound%sourseBANK
    xcor0            => gdp%gdimbound%xcor0
    ycor0            => gdp%gdimbound%ycor0
    !
    FREEs1_u         => gdp%gdimbound%FREEs1_u
    FREEs1_v         => gdp%gdimbound%FREEs1_v
    multEXITu        => gdp%gdimbound%multEXITu
    multEXITv        => gdp%gdimbound%multEXITv
    EDGEdry          => gdp%gdimbound%EDGEdry
    Ndry_GRS         => gdp%gdimbound%Ndry_GRS
    Nwet_GRS         => gdp%gdimbound%Nwet_GRS
    nAD              => gdp%gdimbound%nAD
    mAD              => gdp%gdimbound%mAD
    EDGEtypeBANK     => gdp%gdimbound%EDGEtypeBANK
    EDGEtypeBANKerod => gdp%gdimbound%EDGEtypeBANKerod
    STOREedge2       => gdp%gdimbound%STOREedge2
    EDGEtypeDRY      => gdp%gdimbound%EDGEtypeDRY
    U1inDRY          => gdp%gdimbound%U1inDRY
    V1inDRY          => gdp%gdimbound%V1inDRY
    FROMmnTOghostS1  => gdp%gdimbound%FROMmnTOghostS1
    FROMmnTOghostU1  => gdp%gdimbound%FROMmnTOghostU1
    FROMmnTOghostV1  => gdp%gdimbound%FROMmnTOghostV1
    nGPs1            => gdp%gdimbound%nGPs1
    mGPs1            => gdp%gdimbound%mGPs1
    nGPu1            => gdp%gdimbound%nGPu1
    mGPu1            => gdp%gdimbound%mGPu1
    nGPv1            => gdp%gdimbound%nGPv1
    mGPv1            => gdp%gdimbound%mGPv1
    mIPs1            => gdp%gdimbound%mIPs1
    nIPs1            => gdp%gdimbound%nIPs1
    mBIs1            => gdp%gdimbound%mBIs1
    nBIs1            => gdp%gdimbound%nBIs1
    mIPu1            => gdp%gdimbound%mIPu1
    nIPu1            => gdp%gdimbound%nIPu1
    mBIu1            => gdp%gdimbound%mBIu1
    nBIu1            => gdp%gdimbound%nBIu1
    mIPv1            => gdp%gdimbound%mIPv1
    nIPv1            => gdp%gdimbound%nIPv1
    mBIv1            => gdp%gdimbound%mBIv1
    nBIv1            => gdp%gdimbound%nBIv1
    MERGEDwith_d     => gdp%gdimbound%MERGEDwith_d
    MERGEDwith_w     => gdp%gdimbound%MERGEDwith_w
    MERGEDwith_bed   => gdp%gdimbound%MERGEDwith_bed
    isMERGEDu_w      => gdp%gdimbound%isMERGEDu_w
    isMERGEDv_w      => gdp%gdimbound%isMERGEDv_w
    isMERGEDu_d      => gdp%gdimbound%isMERGEDu_d
    isMERGEDv_d      => gdp%gdimbound%isMERGEDv_d
    NMlistMERGED_bed => gdp%gdimbound%NMlistMERGED_bed
    NMlistMERGED_w   => gdp%gdimbound%NMlistMERGED_w
    NMlistMERGED_d   => gdp%gdimbound%NMlistMERGED_d
    inSTENCILu       => gdp%gdimbound%inSTENCILu
    inSTENCILv       => gdp%gdimbound%inSTENCILv
    kfu_cc           => gdp%gdimbound%kfu_cc
    kfv_cc           => gdp%gdimbound%kfv_cc
    kFLcut           => gdp%gdimbound%kFLcut
    SURFinDRY        => gdp%gdimbound%SURFinDRY
    INTERFtype       => gdp%gdimbound%INTERFtype
    dpU0             => gdp%gdimbound%dpU0
    dpV0             => gdp%gdimbound%dpV0
    dps0             => gdp%gdimbound%dps0
    Eb               => gdp%gdimbound%Eb
    EbK              => gdp%gdimbound%EbK
    dpH              => gdp%gdimbound%dpH
    dpL              => gdp%gdimbound%dpL
    dpsi             => gdp%gdimbound%dpsi
    deta             => gdp%gdimbound%deta
    POROS            => gdp%gdimbound%POROS
    PSIx             => gdp%gdimbound%PSIx
    PSIy             => gdp%gdimbound%PSIy
    ETAx             => gdp%gdimbound%ETAx
    ETAy             => gdp%gdimbound%ETAy
    dutdn_U          => gdp%gdimbound%dutdn_U
    dutdn_V          => gdp%gdimbound%dutdn_V
    EDGElenBANK      => gdp%gdimbound%EDGElenBANK
    EDGElenDRYeff    => gdp%gdimbound%EDGElenDRYeff
    EDGExyBANK       => gdp%gdimbound%EDGExyBANK
    EDGExyBANKerod   => gdp%gdimbound%EDGExyBANKerod
    EDGExyDRY        => gdp%gdimbound%EDGExyDRY
    tauBANK          => gdp%gdimbound%tauBANK
    STOREedgeLEN     => gdp%gdimbound%STOREedgeLEN
    EDGElenWET       => gdp%gdimbound%EDGElenWET
    gsqs_cc          => gdp%gdimbound%gsqs_cc
    guu_cc           => gdp%gdimbound%guu_cc
    gvv_cc           => gdp%gdimbound%gvv_cc
    agsqs            => gdp%gdimbound%agsqs
    agsqs_link       => gdp%gdimbound%agsqs_link
    aguu0            => gdp%gdimbound%aguu0
    agvv0            => gdp%gdimbound%agvv0
    xcorU1           => gdp%gdimbound%xcorU1
    ycorU1           => gdp%gdimbound%ycorU1
    xcorV1           => gdp%gdimbound%xcorV1
    ycorV1           => gdp%gdimbound%ycorV1
    PSIcor0          => gdp%gdimbound%PSIcor0
    ETAcor0          => gdp%gdimbound%ETAcor0
    PSIcor           => gdp%gdimbound%PSIcor
    ETAcor           => gdp%gdimbound%ETAcor
    PSIcorU1         => gdp%gdimbound%PSIcorU1
    ETAcorU1         => gdp%gdimbound%ETAcorU1
    PSIcorV1         => gdp%gdimbound%PSIcorV1
    ETAcorV1         => gdp%gdimbound%ETAcorV1
    xG               => gdp%gdimbound%xG
    yG               => gdp%gdimbound%yG
    psiG             => gdp%gdimbound%psiG
    etaG             => gdp%gdimbound%etaG
    dxk              => gdp%gdimbound%dxk
    dyk              => gdp%gdimbound%dyk
    xcell            => gdp%gdimbound%xcell
    ycell            => gdp%gdimbound%ycell
    DpsiG            => gdp%gdimbound%DpsiG
    DetaG            => gdp%gdimbound%DetaG
    xG_V1            => gdp%gdimbound%xG_V1
    xG_U1            => gdp%gdimbound%xG_U1
    yG_V1            => gdp%gdimbound%yG_V1
    yG_U1            => gdp%gdimbound%yG_U1
    xIPs1            => gdp%gdimbound%xIPs1
    yIPs1            => gdp%gdimbound%yIPs1
    xBIs1            => gdp%gdimbound%xBIs1
    yBIs1            => gdp%gdimbound%yBIs1
    s1IP             => gdp%gdimbound%s1IP
    xIPu1            => gdp%gdimbound%xIPu1
    yIPu1            => gdp%gdimbound%yIPu1
    xBIu1            => gdp%gdimbound%xBIu1
    yBIu1            => gdp%gdimbound%yBIu1
    u1IP             => gdp%gdimbound%u1IP
    u1IPu            => gdp%gdimbound%u1IPu
    u1IPv            => gdp%gdimbound%u1IPv
    v1IPu            => gdp%gdimbound%v1IPu
    v1IPv            => gdp%gdimbound%v1IPv
    xIPv1            => gdp%gdimbound%xIPv1
    yIPv1            => gdp%gdimbound%yIPv1
    xBIv1            => gdp%gdimbound%xBIv1
    yBIv1            => gdp%gdimbound%yBIv1
    v1IP             => gdp%gdimbound%v1IP
    nxG_S1           => gdp%gdimbound%nxG_S1
    nyG_S1           => gdp%gdimbound%nyG_S1
    nxG_U1           => gdp%gdimbound%nxG_U1
    nyG_U1           => gdp%gdimbound%nyG_U1
    nxG_V1           => gdp%gdimbound%nxG_V1
    nyG_V1           => gdp%gdimbound%nyG_V1
    psiG_U1          => gdp%gdimbound%psiG_U1
    psiG_V1          => gdp%gdimbound%psiG_V1
    etaG_U1          => gdp%gdimbound%etaG_U1
    etaG_V1          => gdp%gdimbound%etaG_V1
    DISTs1           => gdp%gdimbound%DISTs1
    DISTu1           => gdp%gdimbound%DISTu1
    DISTv1           => gdp%gdimbound%DISTv1
    dzduu_w          => gdp%gdimbound%dzduu_w
    dzdvv_w          => gdp%gdimbound%dzdvv_w
    dzduuCENTR       => gdp%gdimbound%dzduuCENTR
    dzdvvCENTR       => gdp%gdimbound%dzdvvCENTR
    z_aguu           => gdp%gdimbound%z_aguu
    z_agvv           => gdp%gdimbound%z_agvv
    z_agsqs          => gdp%gdimbound%z_agsqs
    dpLnew           => gdp%gdimbound%dpLnew
    TIMElowDEPTH     => gdp%gdimbound%TIMElowDEPTH
    TIMEsubmerged    => gdp%gdimbound%TIMEsubmerged
    VOLeros          => gdp%gdimbound%VOLeros 
    gradS1_sud       => gdp%gdimbound%gradS1_sud
    gradS1_uzd       => gdp%gdimbound%gradS1_uzd
    frict_sud        => gdp%gdimbound%frict_sud
    frict_uzd        => gdp%gdimbound%frict_uzd
    vdudy            => gdp%gdimbound%vdudy
    ududx            => gdp%gdimbound%ududx
    xintU            => gdp%gdimbound%xintU
    yintU            => gdp%gdimbound%yintU
    dpuFAC           => gdp%gdimbound%dpuFAC
    huFAC            => gdp%gdimbound%huFAC
    eeC              => gdp%gdimbound%eeC
    eeR              => gdp%gdimbound%eeR
    eeL              => gdp%gdimbound%eeL
    EXPsouC          => gdp%gdimbound%EXPsouC
    EXPsouR          => gdp%gdimbound%EXPsouR
    EXPsouL          => gdp%gdimbound%EXPsouL
    CELLtoRECON      => gdp%gdimbound%CELLtoRECON
    CELLadjCUT       => gdp%gdimbound%CELLadjCUT
    updatedBANK      => gdp%gdimbound%updatedBANK
!   start IBM_research variables, most of them will be eventually removed
    cutfac           => gdp%gdimbound%cutfac
    uFACcut          => gdp%gdimbound%uFACcut
    vFACcut          => gdp%gdimbound%vFACcut    
    uFACcutINV       => gdp%gdimbound%uFACcutINV    
!   end IBM_research       
    !
    ! Initialize allocated variables
    !
    gdp%gdimbound%PSIcor0           = 0.0_fp
    gdp%gdimbound%ETAcor0           = 0.0_fp
    gdp%gdimbound%PSIcorU1          = 0.0_fp
    gdp%gdimbound%ETAcorU1          = 0.0_fp
    gdp%gdimbound%PSIcorV1          = 0.0_fp
    gdp%gdimbound%ETAcorV1          = 0.0_fp
    gdp%gdimbound%dpsi              = 0.0_fp
    gdp%gdimbound%deta              = 0.0_fp
    gdp%gdimbound%xG                = 0.0_fp
    gdp%gdimbound%yG                = 0.0_fp
    gdp%gdimbound%psiG              = 0.0_fp
    gdp%gdimbound%etaG              = 0.0_fp
    gdp%gdimbound%PSIcor            = 0.0_fp
    gdp%gdimbound%ETAcor            = 0.0_fp
    !
    gdp%gdimbound%kfs_cc            = -999
    gdp%gdimbound%SURFinDRY         = 0
    gdp%gdimbound%U1inDRY           = 0
    gdp%gdimbound%V1inDRY           = 0
    gdp%gdimbound%FREEs1_u          = 0
    gdp%gdimbound%FREEs1_v          = 0
    gdp%gdimbound%xcell             = 0.0_fp
    gdp%gdimbound%ycell             = 0.0_fp
    gdp%gdimbound%PSIx              = 0.0_fp
    gdp%gdimbound%PSIy              = 0.0_fp
    gdp%gdimbound%ETAx              = 0.0_fp
    gdp%gdimbound%ETAy              = 0.0_fp
    gdp%gdimbound%DpsiG             = 0.0_fp
    gdp%gdimbound%DetaG             = 0.0_fp
    !
    gdp%gdimbound%Ndry_GRS          = 0
    gdp%gdimbound%Nwet_GRS          = 0
    gdp%gdimbound%EDGEdry           = 0
    gdp%gdimbound%nAD               = 0
    gdp%gdimbound%mAD               = 0
    gdp%gdimbound%STOREedge2        = 0
    gdp%gdimbound%STOREedgeLEN      = 0.0_fp
    gdp%gdimbound%EDGEtypeDRY       = 0
    gdp%gdimbound%EDGElenBANK       = 0.0_fp
    gdp%gdimbound%EDGElenDRYeff     = 0.0_fp
    gdp%gdimbound%EDGExyBANK        = 0.0_fp
    gdp%gdimbound%EDGExyDRY         = 0.0_fp
    gdp%gdimbound%EDGExyBANKerod    = 0.0_fp
    gdp%gdimbound%GHOSTs1           = 0
    gdp%gdimbound%GHOSTu1           = 0
    gdp%gdimbound%GHOSTv1           = 0
    gdp%gdimbound%nGPs1             = 0
    gdp%gdimbound%nGPu1             = 0
    gdp%gdimbound%nGPv1             = 0
    gdp%gdimbound%mGPs1             = 0
    gdp%gdimbound%mGPu1             = 0
    gdp%gdimbound%mGPv1             = 0
    gdp%gdimbound%xIPs1             = 0.0_fp
    gdp%gdimbound%yIPs1             = 0.0_fp
    gdp%gdimbound%xBIs1             = 0.0_fp
    gdp%gdimbound%yBIs1             = 0.0_fp
    gdp%gdimbound%xIPu1             = 0.0_fp
    gdp%gdimbound%yIPu1             = 0.0_fp
    gdp%gdimbound%xBIu1             = 0.0_fp
    gdp%gdimbound%yBIu1             = 0.0_fp
    gdp%gdimbound%xIPv1             = 0.0_fp
    gdp%gdimbound%yIPv1             = 0.0_fp
    gdp%gdimbound%xBIv1             = 0.0_fp
    gdp%gdimbound%yBIv1             = 0.0_fp
    gdp%gdimbound%mIPs1             = 0
    gdp%gdimbound%nIPs1             = 0
    gdp%gdimbound%mBIs1             = 0
    gdp%gdimbound%nBIs1             = 0
    gdp%gdimbound%s1IP              = 0.0_fp
    gdp%gdimbound%mIPu1             = 0
    gdp%gdimbound%nIPu1             = 0
    gdp%gdimbound%mBIu1             = 0
    gdp%gdimbound%nBIu1             = 0
    gdp%gdimbound%u1IP              = 0.0_fp
    gdp%gdimbound%mIPv1             = 0
    gdp%gdimbound%nIPv1             = 0
    gdp%gdimbound%mBIv1             = 0
    gdp%gdimbound%nBIv1             = 0
    gdp%gdimbound%v1IP              = 0.0_fp
    gdp%gdimbound%u1IPu             = 0.0_fp
    gdp%gdimbound%u1IPv             = 0.0_fp
    gdp%gdimbound%v1IPu             = 0.0_fp
    gdp%gdimbound%v1IPv             = 0.0_fp
    gdp%gdimbound%DISTs1            = 0.0_fp
    gdp%gdimbound%DISTu1            = 0.0_fp
    gdp%gdimbound%DISTv1            = 0.0_fp
    gdp%gdimbound%nxG_S1            = 0.0_fp
    gdp%gdimbound%nyG_S1            = 0.0_fp
    gdp%gdimbound%nxG_U1            = 0.0_fp
    gdp%gdimbound%nyG_U1            = 0.0_fp
    gdp%gdimbound%nxG_V1            = 0.0_fp
    gdp%gdimbound%nyG_V1            = 0.0_fp
    gdp%gdimbound%FROMmnTOghostS1   = 0
    gdp%gdimbound%FROMmnTOghostU1   = 0
    gdp%gdimbound%FROMmnTOghostV1   = 0
    gdp%gdimbound%por012            = 0.0_fp
    gdp%gdimbound%inSTENCILu        = 0
    gdp%gdimbound%inSTENCILv        = 0
    gdp%gdimbound%dxk               = 0.0_fp
    gdp%gdimbound%dyk               = 0.0_fp
    gdp%gdimbound%dutdn_U           = 0.0_fp
    gdp%gdimbound%dutdn_V           = 0.0_fp
    gdp%gdimbound%xG_V1             = 0.0_fp
    gdp%gdimbound%xG_U1             = 0.0_fp
    gdp%gdimbound%yG_V1             = 0.0_fp
    gdp%gdimbound%yG_U1             = 0.0_fp
    gdp%gdimbound%porosOLD          = 0.0_fp
    gdp%gdimbound%psiG_U1           = 0.0_fp
    gdp%gdimbound%psiG_V1           = 0.0_fp
    gdp%gdimbound%etaG_U1           = 0.0_fp
    gdp%gdimbound%etaG_V1           = 0.0_fp
    gdp%gdimbound%shiftBIu_x        = 0.0_fp
    gdp%gdimbound%shiftBIu_y        = 0.0_fp
    gdp%gdimbound%shiftBIv_x        = 0.0_fp
    gdp%gdimbound%shiftBIv_y        = 0.0_fp
    gdp%gdimbound%xcorU1            = 0.0_fp
    gdp%gdimbound%ycorU1            = 0.0_fp
    gdp%gdimbound%xcorV1            = 0.0_fp
    gdp%gdimbound%ycorV1            = 0.0_fp
    gdp%gdimbound%aguu0             = 0.0_fp
    gdp%gdimbound%agvv0             = 0.0_fp
    gdp%gdimbound%CELLtoRECON       = .false.
    gdp%gdimbound%CELLadjCUT        = .false.
    gdp%gdimbound%oneEXIT           = .false.
    gdp%gdimbound%multEXITu         = 0
    gdp%gdimbound%multEXITv         = 0
    gdp%gdimbound%newGHOSTu         = .false.
    gdp%gdimbound%newGHOSTv         = .false.
    !
    gdp%gdimbound%dzduu_w           = 0.0_fp
    gdp%gdimbound%dzdvv_w           = 0.0_fp
    gdp%gdimbound%dzduuCENTR        = 0.0_fp
    gdp%gdimbound%dzdvvCENTR        = 0.0_fp
    gdp%gdimbound%agsqs_link        = 0.0_fp
    gdp%gdimbound%isMERGEDu_bed     = 0
    gdp%gdimbound%isMERGEDv_bed     = 0
    gdp%gdimbound%NMlistMERGED_bed  = 0
    gdp%gdimbound%Nmerged_bed       = 0
    gdp%gdimbound%isMERGEDu_w       = 0
    gdp%gdimbound%isMERGEDv_w       = 0
    gdp%gdimbound%NMlistMERGED_w    = 0
    gdp%gdimbound%Nmerged_w         = 0
    gdp%gdimbound%isMERGEDu_d       = 0
    gdp%gdimbound%isMERGEDv_d       = 0
    gdp%gdimbound%NMlistMERGED_d    = 0
    gdp%gdimbound%Nmerged_d         = 0
    gdp%gdimbound%MERGEDwith_bed    = 0
    gdp%gdimbound%MERGEDwith_d      = 0
    gdp%gdimbound%MERGEDwith_w      = 0
    gdp%gdimbound%INTERFtype        = 0
    gdp%gdimbound%gradS1_sud        = 0.0_fp
    gdp%gdimbound%gradS1_uzd        = 0.0_fp
    gdp%gdimbound%vdudy             = 0.0_fp
    gdp%gdimbound%ududx             = 0.0_fp
    gdp%gdimbound%frict_uzd         = 0.0_fp
    gdp%gdimbound%frict_sud         = 0.0_fp
    gdp%gdimbound%kGPumin           = 0
    gdp%gdimbound%kGPvmin           = 0
    gdp%gdimbound%kGPumax           = 0
    gdp%gdimbound%kGPvmax           = 0
    gdp%gdimbound%xintU             = 0.0_fp
    gdp%gdimbound%yintU             = 0.0_fp
    gdp%gdimbound%dpuFAC            = 1.0_fp
    gdp%gdimbound%huFAC             = 1.0_fp
    gdp%gdimbound%kWDu              = 0.0_fp
    gdp%gdimbound%kWDv              = 0.0_fp
    gdp%gdimbound%z_aguu            = 0.0_fp
    gdp%gdimbound%z_agvv            = 0.0_fp
    gdp%gdimbound%z_agsqs           = 0.0_fp
    gdp%gdimbound%deltaUcut         = 0.0_fp
    gdp%gdimbound%deltaS1cut        = 0.0_fp
    gdp%gdimbound%EXPsouC           = 0.0_fp
    gdp%gdimbound%EXPsouR           = 0.0_fp
    gdp%gdimbound%EXPsouL           = 0.0_fp
    gdp%gdimbound%eeC               = 0.0_fp
    gdp%gdimbound%eeR               = 0.0_fp
    gdp%gdimbound%eeL               = 0.0_fp
    gdp%gdimbound%neuMERG           = 0
    gdp%gdimbound%sourceU           = 0.0_fp
    gdp%gdimbound%sourseBANK        = 0.0_fp
    gdp%gdimbound%tauBANK           = 0.0_fp
    gdp%gdimbound%Eb                = 0.0_fp
    gdp%gdimbound%Ebk               = 0.0_fp
    gdp%gdimbound%dpLnew            = 0.0_fp
    gdp%gdimbound%VOLeros           = 0.0_fp
    gdp%gdimbound%EDGEtypeBANKerod  = 0
    gdp%gdimbound%TIMElowDEPTH      = 0.0_fp
    gdp%gdimbound%TIMEsubmerged     = 0.0_fp
!   start IBM_research variables, most of them will be eventually removed
    gdp%gdimbound%cutfac            = 1.0_fp
    gdp%gdimbound%uFACcut           = 1.0_fp
    gdp%gdimbound%vFACcut           = 1.0_fp   
    gdp%gdimbound%uFACcutINV        = 1.0_fp    
!   end IBM_research            
    !
    ! At the first time step we always do reconstruction of interface
    !
    updatedBANK(:,:) = .true.
    kFLcut(:,:)      = 1
    !
    ! Check that all the nodes are active:
    !
    do m=1,mmax-1
       do n=1,nmaxus-1
          if (comparereal(xcor(n,m),0._fp).eq.0.and.comparereal(ycor(n,m),0._fp).eq.0) then
             write(lundia,*) 'ERROR: Partial grid with removed point is not allowed! Workaround: provide a full grid and partial enclosure.' 
             call d3stop(1, gdp)
          endif
       enddo
    enddo
    !
    ! Initialize xcor0 ycor0
    !
    xcor0(1:nmax,1:mmax) = xcor(1:nmax,1:mmax)
    ycor0(1:nmax,1:mmax) = ycor(1:nmax,1:mmax)
    !
    ! Open output files
    !
    if (idebugCUT.ge.1) THEN    
       open(1000,file='dry.bin'         ,form = 'unformatted' ,ACCESS = 'STREAM',STATUS='REPLACE')
       open(2000,file='interface.bin'   ,form = 'unformatted' ,ACCESS = 'STREAM',STATUS='REPLACE')
       open(2001,file='interface.def'   ,form = 'unformatted' ,ACCESS = 'STREAM',STATUS='REPLACE')
       open(2003,file='interfaceDRY.def',form = 'unformatted' ,ACCESS = 'STREAM',STATUS='REPLACE')
       !open(100,file='sim.def'          ,form = 'unformatted' ,ACCESS = 'STREAM',STATUS='REPLACE')
       open(3000,file='SURFACEghost.bin',form = 'unformatted' ,ACCESS = 'STREAM',STATUS='REPLACE')
       open(4000,file='U1ghost.bin'     ,form = 'unformatted' ,ACCESS = 'STREAM',STATUS='REPLACE')
       open(5000,file='V1ghost.bin'     ,form = 'unformatted' ,ACCESS = 'STREAM',STATUS='REPLACE')   
       open(6000,file='ghostS1varie.bin',form = 'unformatted' ,ACCESS = 'STREAM',STATUS='REPLACE')
       open(6001,file='totGHOSTs1.bin'  ,form = 'unformatted' ,ACCESS = 'STREAM',STATUS='REPLACE')
       open(7000,file='ghostU1varie.bin',form = 'unformatted' ,ACCESS = 'STREAM',STATUS='REPLACE')
       open(7001,file='totGHOSTu1.bin'  ,form = 'unformatted' ,ACCESS = 'STREAM',STATUS='REPLACE')
       open(8000,file='ghostV1varie.bin',form = 'unformatted' ,ACCESS = 'STREAM',STATUS='REPLACE')
       open(8001,file='totGHOSTv1.bin'  ,form = 'unformatted' ,ACCESS = 'STREAM',STATUS='REPLACE')
       open(10000,file='s1.bin'         ,form = 'unformatted' ,ACCESS = 'STREAM',STATUS='REPLACE')
       open(11000,file='u1.bin'         ,form = 'unformatted' ,ACCESS = 'STREAM',STATUS='REPLACE')
       open(12000,file='v1.bin'         ,form = 'unformatted' ,ACCESS = 'STREAM',STATUS='REPLACE')
       open(13000,file='dpu.bin'        ,form = 'unformatted' ,ACCESS = 'STREAM',STATUS='REPLACE')
       open(14000,file='dpv.bin'        ,form = 'unformatted' ,ACCESS = 'STREAM',STATUS='REPLACE')
       open(15000,file='dps.bin'        ,form = 'unformatted' ,ACCESS = 'STREAM',STATUS='REPLACE')
       open(16000,file='hu.bin'         ,form = 'unformatted' ,ACCESS = 'STREAM',STATUS='REPLACE')
       open(17000,file='hv.bin'         ,form = 'unformatted' ,ACCESS = 'STREAM',STATUS='REPLACE')
       open(18000,file='qxk.bin'        ,form = 'unformatted' ,ACCESS = 'STREAM',STATUS='REPLACE')
       open(19000,file='qyk.bin'        ,form = 'unformatted' ,ACCESS = 'STREAM',STATUS='REPLACE')
       open(20000,file='Umean.bin'      ,form = 'unformatted' ,ACCESS = 'STREAM',STATUS='REPLACE')
       open(21000,file='Vmean.bin'      ,form = 'unformatted' ,ACCESS = 'STREAM',STATUS='REPLACE')
       open(22000,file='guu_cc.bin'     ,form = 'unformatted' ,ACCESS = 'STREAM',STATUS='REPLACE')
       open(23000,file='gvv_cc.bin'     ,form = 'unformatted' ,ACCESS = 'STREAM',STATUS='REPLACE')
       open(66666,file='normals.txt'    ,form = 'formatted')
       open(167890,file='dryPOINTS.txt' ,form = 'formatted')       
    endif
    !
    nprintVOF = 0
    !
    ! Initialize aguu (so it is not undefined outside the domain)
    ! 
    sourseBANK(:,:)           = 0._fp
    newGHOSTu (:,:)           = .false.
    newGHOSTv (:,:)           = .false.
    shiftBIu_x                = 0._fp
    shiftBIu_y                = 0._fp
    shiftBIv_x                = 0._fp
    shiftBIv_y                = 0._fp
    kWDu(nmlb:nmub,1:4)       = 1._fp
    kWDv(nmlb:nmub,1:4)       = 1._fp
    aguu(nlb:nub,mlb:mub)     = 1._fp
    agvv(nlb:nub,mlb:mub)     = 1._fp
    qxk_tinyCUT(:,:,:)        = 0._fp
    qyk_tinyCUT(:,:,:)        = 0._fp
    GHOSTs1(:,:)              = 2          ! so outside of the domain it is dry non-ghost  by default, then modified in FIndGhosPoints in the domain
    GHOSTu1(:,:)              = 2
    GHOSTv1(:,:)              = 2
    deltaUcut (:,:)           = 0._fp
    deltaS1cut (:)            = 0._fp    
    por012(:,:)               = 0
    isMERGEDu_bed(:)          = 0
    isMERGEDv_bed(:)          = 0
    Nmerged_bed(:)            = 1           ! EACH CELL IS MERGED ONLY WITH ITSELF (= NO MERGING)
    Nmerged_d(:)              = 1
    neuMERG(:)                = 0
    porosOLD(nlb:nub,mlb:mub) = 1           ! SO NO stuff is done for new bank-eroded cells
    !GHOSTu1DBL(:,:)           = 0
    !GHOSTv1DBL(:,:)           = 0
    kGPumin(:)                = 999999
    kGPvmin(:)                = 999999
    sourceU(:)                = 0._fp
    kfs_cc(:,:)               = -999        ! otherwise the lines of cell at n=nmax will catch all the if (kfs_cc==0). kfs_cc(nm) = -2 
                                            ! might work too but we prefer -999, more safe
    TIMElowDEPTH              = 0._fp
    TIMEsubmerged             = 0._fp
    eeC(:,:)                  = 0._fp
    eeR(:,:)                  = 0._fp
    eeL(:,:)                  = 0._fp
    EXPsouR(:)                = 0._fp
    EXPsouL(:)                = 0._fp    
    !
    ! Intialize updatedBANK to true on the boundaries so it always does reconstruction and creates the dry region there
    !  MAYBE TO BE REMOVED after RESHAPE_CYCLE
    updatedBANK(1:nmaxus,mmax) = .true.
    updatedBANK(1:nmaxus,1)    = .true.
    updatedBANK(nmaxus,1:mmax) = .true.
    updatedBANK(1,1:mmax)      = .true.
    !
    ! Initialize CELLadjCUT to false on the boundaries (i.e. never included on the extrapolation stencil for computing porosity at boundaries
    ! MAYBE TO BE REMOVED after RESHAPE_CYCLE
    CELLadjCUT(1:nmaxus,mmax) = .false.
    CELLadjCUT(1:nmaxus,1)    = .false.
    CELLadjCUT(nmaxus,1:mmax) = .false.
    CELLadjCUT(1,1:mmax)      = .false.    
    !
    ! Depth in ghost cell
    !MAYBE TO BE REMOVED (to be checked)
    dpL(nmax,:) = -999
    dpH(nmax,:) = -999
    !
    ! Note: in case of restart map file, dpL and dpH are read from the nefis file in subr flow_nefis_restart. 
    !
    ! Determine coordinate of cell center and dpsi deta
    ! Determine direction of the cell (the cell is supposed to be rectangular (i.e. 90 degrees angles)!!!
    !
    do n = 2, nmaxus -1  !RESHAPE_CYCLE  1, nmaxus         
       do m = 2, mmax -1 !RESHAPE_CYCLE  1, mmax   
          !                                                       _________ 
          ! it is the lower edge (anti-clockwise):               |         |
          !                                             (n-1,m-1)|__lower__|(n-1,m)
          !
          dx = xcor0(n-1,m) - xcor0(n-1,m-1)
          dy = ycor0(n-1,m) - ycor0(n-1,m-1)
          dpsi(n,m) = sqrt(dx**2 + dy**2)
          dxETA     = xcor0(n,m) - xcor0(n-1,m)
          dyETA     = ycor0(n,m) - ycor0(n-1,m)
          deta(n,m) = sqrt(dxETA**2 + dyETA**2)
          if (comparereal(dpsi(n,m),0.0_fp).ne.0.0_fp) then
             PSIx(n,m) =  dx/dpsi(n,m)         !x-component of the versor pointing in m direction
             PSIy(n,m) =  dy/dpsi(n,m)         !y-component of the versor pointing in m direction
             ETAx(n,m) = -dy/dpsi(n,m)         !x-component of the versor pointing in n direction
             ETAy(n,m) =  dx/dpsi(n,m)         !y-component of the versor pointing in n direction
          else
             if (comparereal(deta(n,m),0._fp).ne.0._fp) then
                PSIx(n,m) =  dyETA/deta(n,m)   !x-component of the versor pointing in n direction
                PSIy(n,m) = -dxETA/deta(n,m)   !y-component of the versor pointing in n direction
                ETAx(n,m) =  dxETA/deta(n,m)   !x-component of the versor pointing in m direction
                ETAy(n,m) =  dyETA/deta(n,m)   !y-component of the versor pointing in m direction
             else
                !
                ! There are 4 coincident points
                !
                ETAx(n,m) = 0.0_fp
                ETAy(n,m) = 0.0_fp
                PSIx(n,m) = 0.0_fp
                PSIy(n,m) = 0.0_fp
             endif    
          endif
       enddo
    enddo
    !
    ! Unitialized use!! Put corner separately later on     !to be removed if RESHAPE_CYCLE
    !
    dpsi(1,:)      = dpsi(2,:)             ! ghost
    dpsi(nmaxus,:) = dpsi(nmaxus-1,:)
    dpsi(:,1)      = dpsi(:,2)
    dpsi(:,mmax)   = dpsi(:,mmax-1)
    deta(1,:)      = deta(2,:)
    deta(nmaxus,:) = deta(nmaxus-1,:)
    deta(:,1)      = deta(:,2)
    deta(:,mmax)   = deta(:,mmax-1)
    PSIx(1,:)      = PSIx(2,:)             ! ghost
    PSIx(nmaxus,:) = PSIx(nmaxus-1,:)
    PSIx(:,1)      = PSIx(:,2)
    PSIx(:,mmax)   = PSIx(:,mmax-1)
    PSIy(1,:)      = PSIy(2,:)             ! ghost
    PSIy(nmaxus,:) = PSIy(nmaxus-1,:)      
    PSIy(:,1)      = PSIy(:,2)             
    PSIy(:,mmax)   = PSIy(:,mmax-1)        
    ETAx(1,:)      = ETAx(2,:)             ! ghost
    ETAx(nmaxus,:) = ETAx(nmaxus-1,:)      
    ETAx(:,1)      = ETAx(:,2)             
    ETAx(:,mmax)   = ETAx(:,mmax-1)        
    ETAy(1,:)      = ETAy(2,:)             ! ghost
    ETAy(nmaxus,:) = ETAy(nmaxus-1,:)
    ETAy(:,1)      = ETAy(:,2)
    ETAy(:,mmax)   = ETAy(:,mmax-1)
    !
    ! Compute xcor0,ycor0 for the 4 boundary edges excluding corners
    !
    xcor0(1:nmaxus-1,0)    = xcor0(1:nmaxus-1,1) - PSIx(1:nmaxus-1,1)*dpsi(1:nmaxus-1,1)   !ghost
    ycor0(1:nmaxus-1,0)    = ycor0(1:nmaxus-1,1) - PSIy(1:nmaxus-1,1)*dpsi(1:nmaxus-1,1)    
    xcor0(1:nmaxus-1,mmax) = xcor0(1:nmaxus-1,mmax-1) + PSIx(1:nmaxus-1,mmax-1)*dpsi(1:nmaxus-1,mmax-1)    
    ycor0(1:nmaxus-1,mmax) = ycor0(1:nmaxus-1,mmax-1) + PSIy(1:nmaxus-1,mmax-1)*dpsi(1:nmaxus-1,mmax-1)   
    xcor0(0,1:mmax-1)      = xcor0(1,1:mmax-1) - ETAx(1,:)*deta(1,:)   
    ycor0(0,1:mmax-1)      = ycor0(1,1:mmax-1) - ETAy(1,:)*deta(1,:)   
    xcor0(nmaxus,1:mmax-1) = xcor0(nmaxus-1,1:mmax-1) + ETAx(nmaxus-1,1:mmax-1)*deta(nmaxus-1,1:mmax-1)   
    ycor0(nmaxus,1:mmax-1) = ycor0(nmaxus-1,1:mmax-1) + ETAy(nmaxus-1,1:mmax-1)*deta(nmaxus-1,1:mmax-1) 
    !
    ! Compute xcor0,ycor0 for the 4 corners (needed for checking CFL of bank erosion)
    !
    xcor0(0,0)             = xcor0(1,0) - ETAx(1,1)*deta(1,1)
    ycor0(0,0)             = ycor0(1,0) - ETAy(1,1)*deta(1,1)
    xcor0(nmaxus,0)        = xcor0(nmaxus-1,0) + ETAx(nmaxus-1,1)*deta(nmaxus-1,1)
    ycor0(nmaxus,0)        = ycor0(nmaxus-1,0) + ETAy(nmaxus-1,1)*deta(nmaxus-1,1)
    xcor0(0,mmax)          = xcor0(0,mmax-1) + PSIx(1,mmax-1)*dpsi(1,mmax-1)
    ycor0(0,mmax)          = ycor0(0,mmax-1) + PSIy(1,mmax-1)*dpsi(1,mmax-1)
    xcor0(nmaxus,mmax)     = xcor0(nmaxus,mmax-1) + PSIx(nmaxus,mmax-1)*dpsi(nmaxus,mmax-1)
    ycor0(nmaxus,mmax)     = ycor0(nmaxus,mmax-1) + PSIy(nmaxus,mmax-1)*dpsi(nmaxus,mmax-1)
    if (idebugCUT.ge.1) THEN    
       do n=1,nmaxus
         write(7171711,'(200f25.15)') (xcor0(n,m), m=1,mmax)
         write(7171712,'(200f25.15)') (ycor0(n,m), m=1,mmax)
       enddo
    endif
    !
    ! Under the hypothesis that the domain is a rectangle i just use ETAx(2,2) to compute the PSIcor0 and ETAcor0 of vertices
    ! that can be necessary if the domain is rotated.
    !
    ! I THINK ALL ...cor variables ARE WRONG!! DRAW a rotated grid and you ll see. 
    ! These gives the component in ETA but for the same eta they give different etas!
    !
    do n = 1, nmaxus-1              
       do m = 1, mmax-1      
          PSIcor0(n,m) = xcor0(n,m)*PSIx(2,2)+ycor0(n,m)*PSIy(2,2) ! psi-component of the node               
          ETAcor0(n,m) = xcor0(n,m)*ETAx(2,2)+ycor0(n,m)*ETAy(2,2) ! eta-component of the node    
       enddo
    enddo
    do n = 1, nmaxus            
       do m = 1, mmax    
          PSIcor(n,m) = xcor(n,m)*PSIx(2,2)+ycor(n,m)*PSIy(2,2) ! psi-component of the node               
          ETAcor(n,m) = xcor(n,m)*ETAx(2,2)+ycor(n,m)*ETAy(2,2) ! eta-component of the node    
       enddo
    enddo
    !
    !  adjacent cells characteristics
    !
    do m=1,mmax
       do n=1,nmaxus  
          !
          ! Adjacent to the lower edge
          !
          nAD(n,m,1) = n-1
          mAD(n,m,1) = m
          !
          ! Adjacent to the right edge
          !
          nAD(n,m,2) = n
          mAD(n,m,2) = m+1
          !
          ! Adjacent to the upper edge
          !
          nAD(n,m,3) = n+1
          mAD(n,m,3) = m
          !
          ! Adjacent to the left edge
          !
          nAD(n,m,4) = n
          mAD(n,m,4) = m-1
          !
          ! Continue for cycles in k
          !
          nAD(n,m,5) = nAD(n,m,1)
          mAD(n,m,5) = mAD(n,m,1)
          nAD(n,m,6) = nAD(n,m,2)
          mAD(n,m,6) = mAD(n,m,2)
       enddo
    enddo
    !
    ! x- and y-coordinates of the 4 corners of cell (n,m) in counter-clockwise direction
    !
    do n=2,nmaxus-1      !RESHAPE_CYCLE  1, nmax      
       do m=2,mmax-1     !RESHAPE_CYCLE  1, mmaxus   
          xcell(:,n,m) = (/ xcor0(n-1,m-1), xcor0(n-1,m), xcor0(n,m), xcor0(n,m-1), xcor0(n-1,m-1)/)
          ycell(:,n,m) = (/ ycor0(n-1,m-1), ycor0(n-1,m), ycor0(n,m), ycor0(n,m-1), ycor0(n-1,m-1)/)
       enddo
    enddo
    !
    do n = 2, nmaxus-1      !RESHAPE_CYCLE  1, nmax       
       do m = 2, mmax-1           !RESHAPE_CYCLE  1, mmaxus   
          CALL A_G_Poly(xcell(:,n,m),ycell(:,n,m),5,gsqsPROV,xG(n,m),yG(n,m),2,lunscr,gdp)  
          if (kcs(n,m).ne.2) then 
             !
             ! Otherwise it messes up area in anular testcase
             ! We compute gsqs for a general polygon to be sure the gsqs is exact to machine precision (when angles are not exactly 90 degrees)
             !
             gsqs(n,m) = gsqsPROV
          endif
       enddo
    enddo
    !   
    do m=2,mmax-1  !to be removed if RESHAPE_CYCLE  
       ! 
       ! Lower boundary
       !
       xG(1,m) = xG(2,m) - ETAx(2,m)*deta(2,m)  
       yG(1,m) = yG(2,m) - ETAy(2,m)*deta(2,m) 
       !
       ! Upper boundary
       !
       xG(nmaxus,m) = xG(nmaxus-1,m) + ETAx(nmaxus-1,m)*deta(nmaxus-1,m)  
       yG(nmaxus,m) = yG(nmaxus-1,m) + ETAy(nmaxus-1,m)*deta(nmaxus-1,m) 
    enddo
    !
    do n=1,nmaxus !to be removed if RESHAPE_CYCLE  
       !
       ! Left boundary
       !
       xG(n,1) = xG(n,2) - PSIx(n,2)*deta(n,2)  
       yG(n,1) = yG(n,2) - PSIy(n,2)*deta(n,2) 
       !
       ! Right boundary
       !
       xG(n,mmax) = xG(n,mmax) + PSIx(n,mmax-1)*deta(n,mmax-1)  
       yG(n,mmax) = yG(n,mmax) + PSIy(n,mmax-1)*deta(n,mmax-1) 
    enddo
    !
    ! Unitialized use here!! check corner and put them separately later on !to be removed if RESHAPE_CYCLE  
    xG(1,1:mmax)      = xG(2,1:mmax) 
    xG(nmaxus,1:mmax) = xG(nmaxus-1,1:mmax) 
    xG(1:nmaxus,1)    = xG(1:nmaxus,2)-dpsi(1:nmaxus,2)
    xG(1:nmaxus,mmax) = xG(1:nmaxus,mmax-1)+dpsi(1:nmaxus,mmax-1)
    !
    ! This works only if grid is along x-direction. 
    ! Otherwise compute dpsi and deta (see above) and use sin and cos    yG(nmaxus,:) = dpsi(nmaxus-1,:)+dpsi(nmaxus-1,:) !to be removed if RESHAPE_CYCLE  
    !
    yG(1,1:mmax)      = yG(2,1:mmax)-deta(2,1:mmax)   
    yG(nmaxus,1:mmax) = yG(nmaxus-1,1:mmax)+deta(nmaxus-1,1:mmax)  
    yG(1:nmaxus,1)    = yG(1:nmaxus,2) 
    yG(1:nmaxus,mmax) = yG(1:nmaxus,mmax-1)
    !
    ! The following can be changed now. Can be moved after computation of PsiG and etaG and they can be used instead. Cheaper.
    !
    do m=1,mmax-1 
       do n=1,nmaxus
          DpsiG(n,m)  = sqrt((xG(n,m+1)-xG(n,m))**2+(yG(n,m+1)-yG(n,m))**2)
       enddo
    enddo
    do m=1,mmax
       do n=1,nmaxus-1
          DetaG(n,m)  = sqrt((xG(n+1,m)-xG(n,m))**2+(yG(n+1,m)-yG(n,m))**2)
       enddo
    enddo
    !
    ! Location of velocity points
    !
    DO m=1,mmax
       DO n=1,nmaxus
          xG_V1(n,m) = (xcor0(n,m)+xcor0(n,m-1))*0.5_fp
          yG_V1(n,m) = (ycor0(n,m)+ycor0(n,m-1))*0.5_fp       
       ENDDO                                           
    ENDDO                                              
    DO m=1,mmax                                        
       DO n=1,nmaxus                                   
          xG_U1(n,m) = (xcor0(n,m)+xcor0(n-1,m))*0.5_fp      
          yG_U1(n,m) = (ycor0(n,m)+ycor0(n-1,m))*0.5_fp       
       ENDDO
    ENDDO
    !
    if (periodSURFACE) THEN
       call periodic_xGyG_U(xG_U1,yG_U1,nlb,nub,mlb,mub,kmax, gdp)
       call periodic_xGyG_V(xG_V1,yG_V1,nlb,nub,mlb,mub,kmax, gdp)
    endif
    !
    ! I THINK ALL ...cor variables ARE WRONG!! 
    ! DRAW a rotated grid and you ll see. 
    ! These gives the component in ETA but for the same eta they give different etas!
    !
    do n = 1, nmaxus            
       do m = 1, mmax    
          psiG_U1(n,m) =  xG_U1(n,m)*PSIx(2,2)+yG_U1(n,m)*PSIy(2,2) ! psi-component of the node               
          etaG_U1(n,m) =  xG_U1(n,m)*ETAx(2,2)+yG_U1(n,m)*ETAy(2,2) ! eta-component of the node    
       enddo                                                          
    enddo                                                             
    do n = 1, nmaxus                                                  
       do m = 1, mmax                                                 
          psiG_V1(n,m) =  xG_V1(n,m)*PSIx(2,2)+yG_V1(n,m)*PSIy(2,2) ! psi-component of the node               
          etaG_V1(n,m) =  xG_V1(n,m)*ETAx(2,2)+yG_V1(n,m)*ETAy(2,2) ! eta-component of the node    
       enddo
    enddo
    !
    ! We compute nodes of U-velocity grid from barycenter of V-velocity grid and vice versa.
    !
    ! The convention here is that the cell of the U and V velocity grid has the same index of the 
    ! barycenter of the cell. E.g. u1(14,24) is the horiz velocity point, that its location is xG_U1(14,24),
    ! that is the barycenter of the U1 grid, that has as an upper right corner xcorU1(14,24) ( that is the vertical 
    ! velocity point V1 but clearly V1 there has an index increased
    !
    do n = 1, nmaxus              
       do m = 0, mmax-1      
          xcorU1(n,m) =  xG_V1(n,m+1)  ! x-component of the node               
          ycorU1(n,m) =  yG_V1(n,m+1)  ! y-component of the node    
       enddo
    enddo
    do n = 0, nmaxus-1              
       do m = 1, mmax 
          xcorV1(n,m) =  xG_U1(n+1,m)  ! x-component of the node               
          ycorV1(n,m) =  yG_U1(n+1,m)  ! y-component of the node   
       enddo
    enddo           
    !
    ! Under the hypotesis that the domain is a rectangle i just use ETAx(2,2) to compute the PSIcor0 and ETAcor0 of vertices
    ! that can be necessary if the domain is rotated.
    !
    do n = 1, nmaxus              
       do m = 0, mmax-1      
          PSIcorU1(n,m) =  xcorU1(n,m)*PSIx(2,2)+ycorU1(n,m)*PSIy(2,2) ! psi-component of the node               
          ETAcorU1(n,m) =  xcorU1(n,m)*ETAx(2,2)+ycorU1(n,m)*ETAy(2,2) ! eta-component of the node      
       enddo                                                             
    enddo                                                                
    do n = 0, nmaxus-1                                                   
       do m = 1, mmax                                                    
          PSIcorV1(n,m) =  xcorV1(n,m)*PSIx(2,2)+ycorV1(n,m)*PSIy(2,2) ! psi-component of the node               
          ETAcorV1(n,m) =  xcorV1(n,m)*ETAx(2,2)+ycorV1(n,m)*ETAy(2,2) ! eta-component of the node  
       enddo
    enddo
    do n = 1, nmaxus              
       do m = 1, mmax      
          psiG(n,m) =  xG(n,m)*PSIx(2,2)+yG(n,m)*PSIy(2,2) ! psi-component of the node               
          etaG(n,m) =  xG(n,m)*ETAx(2,2)+yG(n,m)*ETAy(2,2) ! eta-component of the node      
       enddo
    enddo
    !
    if (periodSURFACE) THEN
       !
       ! Needed only in order not to have undefined Npsi and Neta when ALPHAvof is called for kcs==2 and periodic condition. 
       ! In fact, porosity is made periodic in incbc but no normal is defined
       !
       CALL PER_GRIDgeometry(gdp)
    ENDIF
    !
    if (idebugCUT.ge.1) THEN    
       OPEN(20,file = 'xG_U1.txt')    
       do n=1,nmaxus      
         write(20,'(<mmax>f25.15)') (xG_U1(n,m),m=1,mmax) 
       enddo
       close(20)
       OPEN(20,file = 'yG_U1.txt')     
       do n=1,nmaxus       
         write(20,'(<mmax>f25.15)') (yG_U1(n,m),m=1,mmax) 
       enddo
       close(20)
       OPEN(20,file = 'xG_V1.txt')   
       do n=1,nmaxus       
         write(20,'(<mmax>f25.15)') (xG_V1(n,m),m=1,mmax) 
       enddo
       close(20)
       OPEN(20,file = 'yG_V1.txt') 
       do n=1,nmaxus         
         write(20,'(<mmax>f25.15)') (yG_V1(n,m),m=1,mmax) 
       enddo
       close(20)
       OPEN(20,file = 'xG.txt')     
       do n=1,nmaxus     
         write(20,'(<mmax>f25.15)') (xg(n,m),m=1,mmax)
       enddo
       close(20)
       OPEN(20,file = 'yG.txt')  
       do n=1,nmaxus           
         write(20,'(<mmax>f25.15)') (yg(n,m),m=1,mmax)
       enddo
       close(20)
    endif
    !
    ! Read polygon file
    !
    CALL readPOLY(lunscr, gdp) 
    !
    open(515151,file = 'log.TXT')
    !
    ! Skip intCELLS if restart from map, since poros is read from map file and I do not want to use polygons to compute poros
    ! if banks are moving.
    ! However, if EXACTpolygons=true I need to call it in order to compute stuff like Ndry_GRS that is skipped in reconVOF.
    !
    if (restid == ' '.or.EXACTpolygons) then 
       call intCELLS(gsqs,kfs,kcs,s1,u1,v1,dps,lunscr,Irov,mmax,nmax,nmaxus,kmax,nlb,nub,mlb,mub,nmlb,nmub, gdp)
    endif
    if(EXACTpolygons.and.restid /= ' ') then
       write(*,*) 'Error: restart from map file and EXACTpolygons=Y could give problems if banks non continuous or in general non coincident with input polygon. Press enter to continue on your own risk.' !otherwise dpL and dpH can be wrong if I restart from a simulation with EXACTpolygons=N
       !pause
       !
       ! Best thing is printing on map file a variables that tells if morphodynamics of banks started or not
       !         
       !stop
    endif
    !
    ! Update porosity due to encroachment of vegetation
    !
    CALL POROSupdate(kcs,dps,s1,nub,mub,nlb,mlb,nmaxus,mmax,0._fp,1._Fp, gdp) !dt=0.fp MF=1.Fp
    !
end subroutine PLIC_VOF_INIT
