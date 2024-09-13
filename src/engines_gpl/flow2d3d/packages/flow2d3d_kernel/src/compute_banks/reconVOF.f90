subroutine reconVOF(gsqs  , kfs   , kcs   , kfu , kfv  , kcu   , kcv   , &
                  & s1    , u1    , v1    , qxk , qyk  , dps   , hu    , hv  , &
                  & dpu   , dpv   , guu   , gvv , thick, lunscr, lundia, irov, &
                  & mmax  , nmax  , nmaxus, kmax, nst  , &
                  & nlb   , nub   , mlb   , mub , nmlb , nmub, &
                  & zmodel, drycrt, gdp   )
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
!  $Id: reconVOF.f90 6271 2016-07-08 13:36:04Z platzek $
!  $HeadURL: 
!!--description-----------------------------------------------------------------`
!
!    Function: GIVEN the porosity function f for each cell, this function 
!              determines the normal m and the intersection points
!
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
!
    use globaldata
    use mathconsts, only: pi
    use Cplusplus
    use dfparall
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    real(fp)                       , pointer :: t
    real(fp)                       , pointer :: PERCedge
    integer                        , pointer :: analyticalPOLY
    integer                        , pointer :: VERSIONprecisePOROSbaric
    integer                        , pointer :: PERIODalongM
    integer                        , pointer :: free_S1_sud
    integer                        , pointer :: continuity_cc
    integer                        , pointer :: ERODsubmBANKS
    integer                        , pointer :: nCUTcell
    integer                        , pointer :: TYPErecVOF
    integer                        , pointer :: BOUNDvof
    integer                        , pointer :: nstREST
    integer                        , pointer :: idebugCUT
    integer                        , pointer :: idebugCUTini
    integer                        , pointer :: idebugCUTfin
    integer                        , pointer :: itmorB
    integer                        , pointer :: NanglesANALcircle_FIXED
    integer                        , pointer :: NanglesANALcircle
    integer , dimension(:)         , pointer :: edge6
    integer , dimension(:,:)       , pointer :: kfs_cc
    integer , dimension(:,:)       , pointer :: multEXITu
    integer , dimension(:,:)       , pointer :: multEXITv
    integer , dimension(:,:)       , pointer :: Ndry_GRS
    integer , dimension(:,:)       , pointer :: Nwet_GRS
    integer , dimension(:,:,:)     , pointer :: nAD
    integer , dimension(:,:,:)     , pointer :: mAD
    integer , dimension(:,:,:)     , pointer :: EDGEtypeBANK
    integer , dimension(:,:,:)     , pointer :: EDGEtypeBANKerod
    integer , dimension(:,:,:)     , pointer :: STOREedge2
    integer , dimension(:,:)       , pointer :: por012
    real(fp), dimension(:,:)       , pointer :: dpsi
    real(fp), dimension(:,:)       , pointer :: deta
    real(fp), dimension(:,:)       , pointer :: POROS
    real(fp), dimension(:,:)       , pointer :: alphaD
    real(fp), dimension(:,:)       , pointer :: PSIx
    real(fp), dimension(:,:)       , pointer :: PSIy
    real(fp), dimension(:,:)       , pointer :: xG_L
    real(fp), dimension(:,:)       , pointer :: xG_H
    real(fp), dimension(:,:)       , pointer :: yG_L
    real(fp), dimension(:,:)       , pointer :: yG_H
    real(fp), dimension(:,:,:)     , pointer :: INTx_GRS
    real(fp), dimension(:,:,:)     , pointer :: INTy_GRS
    real(fp), dimension(:,:,:)     , pointer :: INTwx_GRS
    real(fp), dimension(:,:,:)     , pointer :: INTwy_GRS
    real(fp), dimension(:,:,:)     , pointer :: EDGElenBANK
    real(fp), dimension(:,:,:)     , pointer :: EDGElenDRYeff
    real(fp), dimension(:,:,:,:,:) , pointer :: EDGExyBANK
    real(fp), dimension(:,:,:,:,:) , pointer :: EDGExyBANKerod
    real(fp), dimension(:,:,:)     , pointer :: STOREedgeLEN
    real(fp), dimension(:,:,:)     , pointer :: EDGElenWET
    real(fp), dimension(:,:)       , pointer :: guu_cc
    real(fp), dimension(:,:)       , pointer :: gvv_cc
    real(fp), dimension(:,:)       , pointer :: agsqs
    real(fp), dimension(:,:)       , pointer :: aguu
    real(fp), dimension(:,:)       , pointer :: agvv
    real(fp), dimension(:,:)       , pointer :: xcor0
    real(fp), dimension(:,:)       , pointer :: ycor0
    real(fp), dimension(:,:)       , pointer :: xG
    real(fp), dimension(:,:)       , pointer :: yG
    real(fp), dimension(:,:,:)     , pointer :: dxk
    real(fp), dimension(:,:,:)     , pointer :: dyk
    real(fp), dimension(:,:,:)     , pointer :: xcell
    real(fp), dimension(:,:,:)     , pointer :: ycell
    real(fp), dimension(:,:)       , pointer :: Npsi
    real(fp), dimension(:,:)       , pointer :: Neta
    real(fp), dimension(:,:)       , pointer :: Nx
    real(fp), dimension(:,:)       , pointer :: Ny
    real(fp), dimension(:,:)       , pointer :: DpsiG
    real(fp), dimension(:,:)       , pointer :: DetaG
    logical , dimension(:,:)       , pointer :: CELLtoRECON
    logical , dimension(:,:)       , pointer :: updatedBANK
    logical , dimension(:,:)       , pointer :: newGHOSTu
    logical , dimension(:,:)       , pointer :: newGHOSTv
    logical , dimension(:,:)       , pointer :: oneEXIT
    logical                        , pointer :: EXACTpolygons
    logical                        , pointer :: periodSURFACE
    logical                        , pointer :: precisePOROSbaric
    logical                        , pointer :: periodGHOST
    logical                        , pointer :: twoCELLSperiod
    logical                        , pointer :: forceN
    logical , dimension(:,:)       , pointer :: aguuOLDnotZERO
    logical , dimension(:,:)       , pointer :: agvvOLDnotZERO
!
! Global variables
!  
    real(fp), dimension(nlb:nub,mlb:mub, kmax)                          , intent(inout) :: qxk  
    real(fp), dimension(nlb:nub,mlb:mub, kmax)                          , intent(inout) :: qyk  
    real(fp), dimension(nlb:nub,mlb:mub)                                , intent(in)    :: guu     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(nlb:nub,mlb:mub)                                , intent(in)    :: gvv     !  Description and declaration in esm_alloc_real.f90                            
    real(fp), dimension(nlb:nub,mlb:mub)                                , intent(inout) :: s1 
    real(fp), dimension(nlb:nub,mlb:mub, kmax)                          , intent(inout) :: u1
    real(fp), dimension(nlb:nub,mlb:mub, kmax)                          , intent(inout) :: v1
    real(fp), dimension(nlb:nub,mlb:mub)                                , intent(inout) :: gsqs
    real(fp), dimension(nlb:nub,mlb:mub)                                , intent(inout) :: hu
    real(fp), dimension(nlb:nub,mlb:mub)                                , intent(inout) :: hv
    real(fp), dimension(nlb:nub,mlb:mub)                                , intent(out)   :: dpu
    real(fp), dimension(nlb:nub,mlb:mub)                                , intent(out)   :: dpv
    real(prec), dimension(nlb:nub,mlb:mub)                              , intent(inout) :: dps
    real(fp), dimension(kmax)                                           , intent(in)    :: thick 
    real(fp)                                                            , intent(in)    :: drycrt
    integer , dimension(nlb:nub,mlb:mub)                                , intent(in)    :: kfs 
    integer , dimension(nlb:nub,mlb:mub)                                , intent(in)    :: kcs 
    integer , dimension(nlb:nub,mlb:mub)                                , intent(out)   :: kfu
    integer , dimension(nlb:nub,mlb:mub)                                , intent(out)   :: kfv
    integer , dimension(nlb:nub,mlb:mub)                                , intent(in)    :: kcu
    integer , dimension(nlb:nub,mlb:mub)                                , intent(in)    :: kcv
    integer                                                             , intent(in)    :: mmax     !  Description and declaration in iidim.f90
    integer                                                             , intent(in)    :: nmax     !  Description and declaration in iidim.f90
    integer                                                             , intent(in)    :: nmaxus   !  Description and declaration in iidim.f90
    integer                                                             , intent(in)    :: kmax     !  Description and declaration in iidim.f90
    integer                                                             , intent(in)    :: lunscr   !  Description and declaration in iidim.f90
    integer                                                             , intent(in)    :: lundia   !  Description and declaration in iidim.f90
    integer                                                             , intent(in)    :: irov     !  Description and declaration in iidim.f90
    integer                                                             , intent(in)    :: nst      !  Description and declaration in iidim.f90
    integer                                                             , intent(in)    :: nlb
    integer                                                             , intent(in)    :: nub
    integer                                                             , intent(in)    :: mlb
    integer                                                             , intent(in)    :: mub
    integer                                                             , intent(in)    :: nmlb
    integer                                                             , intent(in)    :: nmub
    logical                                                             , intent(in)    :: zmodel
!
! none
!
! parameter
!
  integer, parameter             :: maxNODdry     = 1000
  integer, parameter             :: maxINTERSedge = 10
!
! Local variables
!
  integer                                 :: i
  integer                                 :: kPER
  integer                                 :: n
  integer                                 :: m
  integer                                 :: kk
  integer                                 :: kn
  integer                                 :: km
  integer                                 :: kn_n
  integer                                 :: km_m
  integer , allocatable, dimension(:)     :: nL
  integer , allocatable, dimension(:)     :: mL
  integer                                 :: K
  integer                                 :: L
  integer                                 :: L1
  integer                                 :: L2
  integer                                 :: Ndry
  integer                                 :: Nwet
  integer                                 :: signINT
  integer                                 :: Nrecon
  integer                                 :: intPROV
  integer                                 :: typeINTER
  integer                                 :: nADJ
  integer                                 :: kADJ
  integer                                 :: mADJ
  integer                                 :: NwAD
  integer                                 :: numSEGout
  integer                                 :: cont
  integer                                 :: Ndryp1
  integer                                 :: Nwetp1
  integer                                 :: j
  integer                                 :: intBUTTA2
  integer                                 :: intBUTTA1
  !
  real(fp), allocatable, dimension(:)     ::  polyx
  real(fp), allocatable, dimension(:)     ::  polyy
  real(fp), allocatable, dimension(:)     ::  PP   
  real(fp), allocatable, dimension(:)     ::  NpsiL
  real(fp), allocatable, dimension(:)     ::  NetaL
  real(fp)                                ::  min_dist
  real(fp)                                ::  scale
  real(fp)                                ::  modN
  real(fp)                                ::  sumN
  real(fp)                                ::  dummyR
  real(fp)                                ::  areaWET
  real(fp)                                ::  realPROV
  real(fp)                                ::  Cx1
  real(fp)                                ::  Cy1 
  real(fp)                                ::  Cx2  
  real(fp)                                ::  Cy2 
  real(fp)                                ::  dx  
  real(fp)                                ::  dy 
  real(fp)                                ::  xB1
  real(fp)                                ::  yB1
  real(fp)                                ::  xB2
  real(fp)                                ::  yB2
  real(fp)                                ::  xW2
  real(fp)                                ::  yW2
  real(fp), allocatable, dimension(:)     ::  xxx  
  real(fp), allocatable, dimension(:)     ::  yyy 
  real(fp)                                ::  Len1  
  real(fp)                                ::  Len2 
  real(fp)                                ::  LENGTHeros
  real(fp)                                ::  NpsiPR
  real(fp)                                ::  NetaPR
  real(fp)                                ::  areaDRY
  real(fp), allocatable, dimension(:)     ::  INTx       ! max number of dry points Ndry is 5
  real(fp), allocatable, dimension(:)     ::  INTy       ! max number of dry points Ndry is 5
  real(fp), allocatable, dimension(:)     ::  INTxR
  real(fp), allocatable, dimension(:)     ::  INTyR
  real(fp), allocatable, dimension(:)     ::  INTwx      ! max number of wet points Nwet is 5
  real(fp), allocatable, dimension(:)     ::  INTwy      ! max number of wet points Nwet is 5
  real(fp), allocatable, dimension(:)     ::  INTwxR
  real(fp), allocatable, dimension(:)     ::  INTwyR
  real(fp)                                ::  absNpsi
  real(fp)                                ::  absNeta
  real(fp)                                ::  absNpsiTRANS
  real(fp)                                ::  absNetaTRANS
  real(fp), allocatable, dimension(:)     ::  YY
  real(fp), allocatable, dimension(:)     ::  XX
  real(fp), allocatable, dimension(:,:,:) ::  EDGxyDRY
  real(fp), allocatable, dimension(:,:)   ::  provEDGxy
  real(fp)                                ::  sumpsi0
  real(fp)                                ::  sumeta0
  real(fp), allocatable, dimension(:,:)   ::  xint
  real(fp), allocatable, dimension(:,:)   ::  yint
  LOGICAL                                 ::  swapEDGEtype
  LOGICAL , allocatable, dimension(:)     ::  SEGisEXTRpoint
  LOGICAL                                 ::  anySEGisEXTRpoint
  LOGICAL                                 ::  EDGEtypeBANK_1_LEFT
  LOGICAL                                 ::  EDGEtypeBANK_1_RIGHT  
  LOGICAL                                 ::  EDGEtypeBANK_2_LEFT
  LOGICAL                                 ::  EDGEtypeBANK_2_RIGHT  
  LOGICAL                                 ::  ADJnonDRY
  LOGICAL,SAVE                            ::  firstCALL = .true.
  CHARACTER*256                           ::  NAMEfileXGYG
  !
  real(fp), allocatable, dimension(:,:) :: Xcirc
  real(fp), allocatable, dimension(:,:) :: Ycirc
  real(fp), allocatable, dimension(:)   :: radius
  real(fp), allocatable, dimension(:)   :: angANTIclock
  real(fp)                              :: midRADIUS
  real(fp)                              :: maxang
  real(fp)                              :: minang
  real(fp)                              :: ang
  real(fp)                              :: radCIRCLE
  real(fp)                              :: Dteta    
  real(fp)                              :: teta     
  real(fp)                              :: area
  integer                               :: outercircle
  logical                               :: zeroNODE
  !
  ! Declaration of variable to be passed to C subroutine 
  ! WORST CASE SCENARIO: 9 POLYGONS FROM THE INTERFACES OF THE STENCIL PLUS 4 FROM THE 4 EDGES (OR PART OF EDGES) OF THE CELL n,m. 
  ! Vertices of unions are maximum 4 per number of poligons.
  !
  REAL(kind=C_DOUBLE), allocatable  :: POLYintersX(:,:)
  REAL(kind=C_DOUBLE), allocatable  :: POLYintersY(:,:)
  REAL(kind=C_DOUBLE), allocatable  :: POLYStoBEjoinedX(:,:)
  REAL(kind=C_DOUBLE), allocatable  :: POLYStoBEjoinedY(:,:)
  REAL(kind=C_DOUBLE), allocatable  :: POLYSunionX(:,:)
  REAL(kind=C_DOUBLE), allocatable  :: POLYSunionY(:,:)  
  INTEGER(kind=c_int), dimension(1) :: VERTinters
  INTEGER(kind=c_int), dimension(1) :: VERTtoBEjoined
  INTEGER(kind=c_int), dimension(1) :: VERTunion
  INTEGER(kind=c_int)               :: NPOLYStoBEjoined
  INTEGER(kind=c_int)               :: NPOLYSinters
  INTEGER(kind=c_int)               :: NPOLYSunion
  REAL(kind=C_DOUBLE)               :: xclip(5)
  REAL(kind=C_DOUBLE)               :: yclip(5)
  REAL(kind=C_DOUBLE)               :: absMAXx(1)
  REAL(kind=C_DOUBLE)               :: absMAXy(1)
!
! executable statements -------------------------------------------------------
!
    t                        => gdp%gdimbound%t
    PERCedge                 => gdp%gdimbound%PERCedge
    analyticalPOLY           => gdp%gdimbound%analyticalPOLY
    VERSIONprecisePOROSbaric => gdp%gdimbound%VERSIONprecisePOROSbaric
    PERIODalongM             => gdp%gdimbound%PERIODalongM
    free_S1_sud              => gdp%gdimbound%free_S1_sud
    continuity_cc            => gdp%gdimbound%continuity_cc
    ERODsubmBANKS            => gdp%gdimbound%ERODsubmBANKS
    nCUTcell                 => gdp%gdimbound%nCUTcell
    TYPErecVOF               => gdp%gdimbound%TYPErecVOF
    BOUNDvof                 => gdp%gdimbound%BOUNDvof
    nstREST                  => gdp%gdimbound%nstREST
    idebugCUT                => gdp%gdimbound%idebugCUT
    idebugCUTini             => gdp%gdimbound%idebugCUTini
    idebugCUTfin             => gdp%gdimbound%idebugCUTfin
    itmorB                   => gdp%gdimbound%itmorB
    NanglesANALcircle_FIXED  => gdp%gdimbound%NanglesANALcircle_FIXED
    NanglesANALcircle        => gdp%gdimbound%NanglesANALcircle
    edge6                    => gdp%gdimbound%edge6
    kfs_cc                   => gdp%gdimbound%kfs_cc
    multEXITu                => gdp%gdimbound%multEXITu
    multEXITv                => gdp%gdimbound%multEXITv
    Ndry_GRS                 => gdp%gdimbound%Ndry_GRS
    Nwet_GRS                 => gdp%gdimbound%Nwet_GRS
    nAD                      => gdp%gdimbound%nAD
    mAD                      => gdp%gdimbound%mAD
    EDGEtypeBANK             => gdp%gdimbound%EDGEtypeBANK
    EDGEtypeBANKerod         => gdp%gdimbound%EDGEtypeBANKerod
    STOREedge2               => gdp%gdimbound%STOREedge2
    por012                   => gdp%gdimbound%por012
    dpsi                     => gdp%gdimbound%dpsi
    deta                     => gdp%gdimbound%deta
    POROS                    => gdp%gdimbound%POROS
    alphaD                   => gdp%gdimbound%alphaD
    PSIx                     => gdp%gdimbound%PSIx
    PSIy                     => gdp%gdimbound%PSIy
    xG_L                     => gdp%gdimbound%xG_L
    xG_H                     => gdp%gdimbound%xG_H
    yG_L                     => gdp%gdimbound%yG_L
    yG_H                     => gdp%gdimbound%yG_H
    INTx_GRS                 => gdp%gdimbound%INTx_GRS
    INTy_GRS                 => gdp%gdimbound%INTy_GRS
    INTwx_GRS                => gdp%gdimbound%INTwx_GRS
    INTwy_GRS                => gdp%gdimbound%INTwy_GRS
    EDGElenBANK              => gdp%gdimbound%EDGElenBANK
    EDGElenDRYeff            => gdp%gdimbound%EDGElenDRYeff
    EDGExyBANK               => gdp%gdimbound%EDGExyBANK
    EDGExyBANKerod           => gdp%gdimbound%EDGExyBANKerod
    STOREedgeLEN             => gdp%gdimbound%STOREedgeLEN
    EDGElenWET               => gdp%gdimbound%EDGElenWET
    guu_cc                   => gdp%gdimbound%guu_cc
    gvv_cc                   => gdp%gdimbound%gvv_cc
    agsqs                    => gdp%gdimbound%agsqs
    aguu                     => gdp%gdimbound%aguu
    agvv                     => gdp%gdimbound%agvv
    xcor0                    => gdp%gdimbound%xcor0
    ycor0                    => gdp%gdimbound%ycor0
    xG                       => gdp%gdimbound%xG
    yG                       => gdp%gdimbound%yG
    dxk                      => gdp%gdimbound%dxk
    dyk                      => gdp%gdimbound%dyk
    xcell                    => gdp%gdimbound%xcell
    ycell                    => gdp%gdimbound%ycell
    Npsi                     => gdp%gdimbound%Npsi
    Neta                     => gdp%gdimbound%Neta
    Nx                       => gdp%gdimbound%Nx
    Ny                       => gdp%gdimbound%Ny
    DpsiG                    => gdp%gdimbound%DpsiG
    DetaG                    => gdp%gdimbound%DetaG
    CELLtoRECON              => gdp%gdimbound%CELLtoRECON
    updatedBANK              => gdp%gdimbound%updatedBANK
    newGHOSTu                => gdp%gdimbound%newGHOSTu
    newGHOSTv                => gdp%gdimbound%newGHOSTv
    oneEXIT                  => gdp%gdimbound%oneEXIT
    EXACTpolygons            => gdp%gdimbound%EXACTpolygons
    periodSURFACE            => gdp%gdimbound%periodSURFACE
    precisePOROSbaric        => gdp%gdimbound%precisePOROSbaric
    periodGHOST              => gdp%gdimbound%periodGHOST
    twoCELLSperiod           => gdp%gdimbound%twoCELLSperiod
    forceN                   => gdp%gdimbound%forceN
    aguuOLDnotZERO           => gdp%gdimbound%Lwrka1_E
    agvvOLDnotZERO           => gdp%gdimbound%Lwrka2_E
    !
    ! Allocation of local variables
    !
    allocate(nL            (4       ))
    allocate(mL            (4       ))
    allocate(polyx         (6       ))
    allocate(polyy         (6       ))
    allocate(PP            (2       ))
    allocate(NpsiL         (4       ))
    allocate(NetaL         (4       ))
    allocate(xxx           (4       ))
    allocate(yyy           (4       ))
    allocate(INTx          (5       ))
    allocate(INTy          (5       ))
    allocate(INTxR         (5       ))
    allocate(INTyR         (5       ))
    allocate(INTwx         (5       ))
    allocate(INTwy         (5       ))
    allocate(INTwxR        (5       ))
    allocate(INTwyR        (5       ))
    allocate(YY            (-1:1    ))
    allocate(XX            (-1:1    ))
    allocate(EDGxyDRY      (4   ,2,2))
    allocate(provEDGxy     (2   ,2  ))
    allocate(xint          (2   ,2  ))
    allocate(yint          (2   ,2  ))
    allocate(SEGisEXTRpoint(2       ))
    allocate(radius        (2       ))
    allocate(angANTIclock  (4       ))
    !
    if (firstCALL) then
       aguuOLDnotZERO(:,:) = .FALSE.
       agvvOLDnotZERO(:,:) = .FALSE.
       !firstCALL = .FALSE.
    else
       do m=1,mmax 
          do n=1,nmaxus  
             aguuOLDnotZERO(n,m) = comparereal(aguu(n,m),0._fp).ne.0
             agvvOLDnotZERO(n,m) = comparereal(agvv(n,m),0._fp).ne.0
          enddo
       enddo
    endif
    !
    ! Normal reconstruction
    !
    ! for non-cartesian meshes: all xG(nL(K),mL(K)+1) - xG(nL(K),mL(K)) (similar for yG) 
    ! must be replaced by the distances along eta and psi directions
    !
    TYPErecVOF = 1
    !
    ! If exactpolygons the normal is already computed and never computed here unless banks started to move.
    !
    if (nst >= itmorB.or.(.not.EXACTpolygons)) then
       if (precisePOROSbaric) then
          write(*,*) 'normals are wrong if precisePOROSbaric=Y, use EXACTpolygons'
          call d3stop(1, gdp)
       endif
       select case(TYPErecVOF)
       case(1)  
         !
         ! Parker and Youngs method
         ! To be optimized
         !
         do m=1,mmax 
            do n=1,nmaxus
              !
              ! note reconstruction is not done if bank not eroded in the previous time step 
              ! and not first time step (CELLtoRECON=.false from checkDRY.f90)
              !
              if ((kcs(n,m)==1.or.kcs(n,m)==2) .and. CELLtoRECON(n,m)) then 
                NpsiPR = 0._fp
                NetaPR = 0._fp
                cont   = 0
                !
                ! upper right     
                ! to be optimized can be precomputed
                !
                K      = 1
                nL(1)  = n
                mL(1)  = m 
                if (n+1<=nmaxus .and. m+1<=mmax) then
                   cont   = cont+1
                   NpsiPR = NpsiPR -(( poros(nL(K)+1,mL(K)+1) - poros(nL(K)+1,mL(K)) ) * deta(nL(K),mL(K)) + &
                                     ( poros(nL(K)  ,mL(K)+1) - poros(nL(K)  ,mL(K)) ) * deta(nL(K)+1,mL(K)) )/ &
                                   ( ( deta(nL(K)+1,mL(K)) + deta(nL(K),mL(K)) )* DpsiG(nL(K),mL(K)))  
                   NetaPR = NetaPR -(( poros(nL(K)+1,mL(K)+1) - poros(nL(K),mL(K)+1) ) * dpsi(nL(K),mL(K)) + &
                                     ( poros(nL(K)+1,mL(K)  ) - poros(nL(K),mL(K)  ) ) * dpsi(nL(K)+1,mL(K)) )/ &
                                   ( ( dpsi(nL(K),mL(K)+1) + dpsi(nL(K),mL(K)) )* DetaG(nL(K),mL(K)))
                endif
                !
                ! upper left
                !
                K     = 2
                nL(2) = n
                mL(2) = m-1 
                if (mL(2) >= 1) then
                   if (n+1<=nmaxus .and. m>=2) then
                      cont   = cont+1
                      NpsiPR = NpsiPR -(( poros(nL(K)+1,mL(K)+1) - poros(nL(K)+1,mL(K)) ) * deta(nL(K),mL(K)) + &
                                        ( poros(nL(K)  ,mL(K)+1) - poros(nL(K)  ,mL(K)) ) * deta(nL(K)+1,mL(K)) )/ &
                                      ( ( deta(nL(K)+1,mL(K)) + deta(nL(K),mL(K)) )* DpsiG(nL(K),mL(K)))  
                      NetaPR = NetaPR -(( poros(nL(K)+1,mL(K)+1) - poros(nL(K),mL(K)+1) ) * dpsi(nL(K),mL(K)) + &
                                        ( poros(nL(K)+1,mL(K)  ) - poros(nL(K),mL(K)  ) ) * dpsi(nL(K)+1,mL(K)) )/ &
                                      ( ( dpsi(nL(K),mL(K)+1) + dpsi(nL(K),mL(K)) )* DetaG(nL(K),mL(K)))
                   endif
                endif
                !
                ! lower left
                !
                K=3
                nL(3)=n-1
                mL(3)=m-1 
                if (nL(3).ge.1.and.mL(3).ge.1) then
                   if (n.le.nmaxus.or.m.le.mmax) then
                      cont = cont+1
                      NpsiPR = NpsiPR -(( poros(nL(K)+1,mL(K)+1) - poros(nL(K)+1,mL(K)) ) * deta(nL(K),mL(K)) + &
                                          ( poros(nL(K)  ,mL(K)+1) - poros(nL(K)  ,mL(K)) ) * deta(nL(K)+1,mL(K)) )/ &
                                        ( ( deta(nL(K)+1,mL(K)) + deta(nL(K),mL(K)) )* DpsiG(nL(K),mL(K)))  
                      NetaPR = NetaPR -(( poros(nL(K)+1,mL(K)+1) - poros(nL(K),mL(K)+1) ) * dpsi(nL(K),mL(K)) + &
                                        ( poros(nL(K)+1,mL(K)  ) - poros(nL(K),mL(K)  ) ) * dpsi(nL(K)+1,mL(K)) )/ &
                                      ( ( dpsi(nL(K),mL(K)+1) + dpsi(nL(K),mL(K)) )* DetaG(nL(K),mL(K)))
                   endif
                endif
                !
                ! lower right
                !
                k     = 4
                nL(4) = n-1
                mL(4) = m 
                if (nL(4) >= 1) then
                   if (n>=2 .and. m+1<=mmax) then
                      cont   = cont+1
                      NpsiPR = NpsiPR -(( poros(nL(K)+1,mL(K)+1) - poros(nL(K)+1,mL(K)) ) * deta(nL(K),mL(K)) + &
                                        ( poros(nL(K)  ,mL(K)+1) - poros(nL(K)  ,mL(K)) ) * deta(nL(K)+1,mL(K)) )/ &
                                      ( ( deta(nL(K)+1,mL(K)) + deta(nL(K),mL(K)) )* DpsiG(nL(K),mL(K)))  
                      NetaPR = NetaPR -(( poros(nL(K)+1,mL(K)+1) - poros(nL(K),mL(K)+1) ) * dpsi(nL(K),mL(K)) + &
                                        ( poros(nL(K)+1,mL(K)  ) - poros(nL(K),mL(K)  ) ) * dpsi(nL(K)+1,mL(K)) )/ &
                                      ( ( dpsi(nL(K),mL(K)+1) + dpsi(nL(K),mL(K)) )* DetaG(nL(K),mL(K)))
                   endif
                endif
                !      
                Npsi(n,m) = NpsiPR / cont
                Neta(n,m) = NetaPR / cont
              endif
            enddo
         enddo      
         !      
       CASE(2)
         !
         ! Weymouth 2010
         !
         DO m=2,mmax-1
            DO n=2,nmaxus-1
              if (kcs(n,m).eq.1.and.CELLtoRECON(n,m)) then
                !
                ! Compute a first approximation of the normal using Parker and Youngs method
                !
                ! upper right   
                !
                nL(1)=n
                mL(1)=m
                !
                ! upper left
                !
                nL(2)=n
                mL(2)=m-1 
                !
                ! lower left
                !
                nL(3)=n-1
                mL(3)=m-1 
                !
                ! lower right  
                !
                nL(4)=n-1
                mL(4)=m 
                !
                ! note if the derivative of poros is positive it means that the gradient is directed toward the dry area. 
                ! To reverse the behaviour we put a minus in front of the differences
                !
                DO K=1,4
                   NpsiL(K) = -(( poros(nL(K)+1,mL(K)+1) - poros(nL(K)+1,mL(K)) ) * deta(nL(K),mL(K)) + &
                               ( poros(nL(K)  ,mL(K)+1) - poros(nL(K)  ,mL(K)) ) * deta(nL(K)+1,mL(K)) )/ &
                             ( ( deta(nL(K)+1,mL(K)) + deta(nL(K),mL(K)) )* DpsiG(n,m)) !*( xG(nL(K),mL(K)+1) - xG(nL(K),mL(K)) ) ) 
                   NetaL(K) = -(( poros(nL(K)+1,mL(K)+1) - poros(nL(K),mL(K)+1) ) * dpsi(nL(K),mL(K)) + &
                               ( poros(nL(K)+1,mL(K)  ) - poros(nL(K),mL(K)  ) ) * dpsi(nL(K)+1,mL(K)) )/ &
                             ( ( dpsi(nL(K),mL(K)+1) + dpsi(nL(K),mL(K)) )* DetaG(n,m)) !( yG(nL(K)+1,mL(K)) - yG(nL(K),mL(K)) ) ) 
                ENDDO  
                Npsi(n,m) = 0.25_fp*(NpsiL(1)+NpsiL(2)+NpsiL(3)+NpsiL(4))
                Neta(n,m) = 0.25_fp*(NetaL(1)+NetaL(2)+NetaL(3)+NetaL(4))
                scale     = (deta(n,m)+dpsi(n,m))*0.5_fp*100*3  !for debug
                !
                ! Correct the normal using Weymouth method
                !
                ! Note this is the normal referred to coordinates parallel to the edges
                !
                if ( ( comparereal(Neta(n,m),0._fp).eq.0               ) .or. &
                     ( abs(Neta(n,m)/Npsi(n,m)).lt.dpsi(n,m)/deta(n,m) ) ) then 
                   !      
                   XX(-1:1) = 0.d0
                   do kn = n-1,n+1
                      kn_n = kn-n !can be avoided declaring XX(1:nmax)
                      do km = m-1,m+1    
                         XX(kn_n) = XX(kn_n) + poros(kn,km)*dpsi(kn,km)   !note dpsi can change in the stencil for non uniform grids.
                      enddo
                   enddo
                   !
                   if ((XX(0).lt.XX(1).and.XX(0).gt.XX(-1)).or.(XX(0).gt.XX(1).and.XX(0).lt.XX(-1))) then
                      Npsi(n,m) = sign(1._fp,Npsi(n,m))
                      signINT   = m - sign(1,NINT(Npsi(n,m)))
                      if (XX(0).gt.dpsi(n,signINT)+0.5D0*dpsi(n,m)) then
                         if(XX(-1).gt.XX(0)) then
                            Neta(n,m) = - (XX( 0)-XX(1))/( yG(n  ,m) - yG(n+1,m))
                         else
                            Neta(n,m) = - (XX(-1)-XX(0))/( yG(n-1,m) - yG(n  ,m))
                         endif
                      else
                         if(XX(-1).gt.XX(0)) then
                            Neta(n,m) = - (XX(-1)-XX(0))/( yG(n-1,m) - yG(n  ,m)) 
                         else
                            Neta(n,m) = - (XX( 0)-XX(1))/( yG(n  ,m) - yG(n+1,m))
                         endif                     
                      endif  
                   endif
                   !
                 else
                   YY(-1:1) = 0.d0
                   do km = m-1,m+1    
                      km_m = km-m !can be avoided declaring XX(1:nmax)
                      do kn = n-1,n+1
                         YY(km_m) = YY(km_m) + poros(kn,km)*deta(kn,km)
                      enddo
                   enddo
                   !
                   if ((YY(0).lt.YY(1).and.YY(0).gt.YY(-1)).or.(YY(0).gt.YY(1).and.YY(0).lt.YY(-1))) then
                      Neta(n,m) = sign(1._fp,Neta(n,m))
                      signINT =  n-sign(1,NINT(Neta(n,m)))
                      if (YY(0).gt.deta(signINT,m)+0.5D0*deta(n,m)) then !This way it handles also non-uniform grids
                         if(YY(-1).gt.YY(0)) then
                            Npsi(n,m) = - (YY( 0)-YY(1))/( xG(n,m  ) - xG(n,m+1))
                         else
                            Npsi(n,m) = - (YY(-1)-YY(0))/( xG(n,m-1) - xG(n,m  ))
                         endif
                      else
                         if(YY(-1).gt.YY(0)) then
                            Npsi(n,m) = - (YY(-1)-YY(0))/( xG(n,m-1) - xG(n,m  ))
                         else
                            Npsi(n,m) = - (YY( 0)-YY(1))/( xG(n,m  ) - xG(n,m+1))
                         endif                     
                      endif  
                   endif
                 endif
                 scale = (deta(n,m)+dpsi(n,m))*0.5_fp  !for debug
              endif
            ENDDO
         ENDDO
         !
       case(3)
         !      
       end select
    endif
    !
    if (forceN) then
       if (mod(NST,100) == 0) then
          write(*,*) 'Remove exact normals'
       endif
       do m = 1, mmax
          do n = 1, nmaxus
              if (sqrt(xg(n,m)**2+yg(n,m)**2) > 60) then
                 Npsi(n,m) = xG(n,m)
                 Neta(n,m) = yG(n,m)
                 modN      = sqrt(xG(n,m)**2 + yG(n,m)**2) 
                 Npsi(n,m) =  Npsi(n,m) / modN
                 Neta(n,m) =  Neta(n,m) / modN
              endif
          enddo
       enddo
    endif
    !
    ! Determine alpha
    !
    Nrecon      = 0
    alphaD(:,:) = 0.d0 ! needed only for debugging purposes
    !
    if (periodSURFACE) then
       !
       ! Needed only in order not to have undefined Npsi and Neta when ALPHAvof is called for kcs==2 and periodic condition. 
       ! In fact porosity is made periodic in incbc but no normal is defined
       !
       call PER_NpsiNeta(gdp)  
    endif
    !    
    !if (analyticalPOLY==1.and.precisePOROSbaric) goto 9999
    do m=1,mmax
       do n=1,nmaxus
          if (kcs(n,m)==1 .or. kcs(n,m)==2) then
          !if ((kcs(n,m).eq.1).or.(kcs(n,m).eq.2).or.(kcs(n,m).eq.0.and.por012(n,m)==2)) then ! the third one is in case of internal immission of discharge
             !
             ! From second call on for analyticalPOLY==1.and.precisePOROSbaric we don't reconstruct since porosity changed and is analytical
             !
             if (CELLtoRECON(n,m) .and..not. (analyticalPOLY==1.and.precisePOROSbaric.and..not.firstCALL)) then 
                Nrecon = Nrecon + 1  
                modN   = sqrt(Npsi(n,m)**2+Neta(n,m)**2)                
                !if (comparereal(Npsi(n,m),0._fp)==0.and.comparereal(Neta(n,m),0._fp)==0) then
                if (comparereal(modN,0._fp) == 0) then
                   !
                   ! This occurs only for the specular case, almost impossible to occur so no point to optimize it
                   !
                   !call prterr(lundia, 'U021', 'The module of the normal in the VOF method is zero')
                   !call d3stop(1,gdp) 
                   !
                   ! default values for the specular case in 2 directions, we just choose a random direction
                   !
                   Npsi(n,m) = 1._fp                      
                   Neta(n,m) = 0._fp
                   !                                     
                   if (m+1 <= mmax) then
                      if (comparereal(poros(n,m),poros(n,m+1)) == 0) then
                         if (comparereal(Npsi(n,m+1),0._fp)==0 .and. comparereal(Neta(n,m+1),0._fp)==0) then
                            !
                            ! if the adjacent has zero too I just pick up the x-positive direction, otherwise I use the same normal of the adjacent
                            !
                            Npsi(n,m) = 0._fp
                            Neta(n,m) = 1._fp
                         else
                            Npsi(n,m) = Npsi(n,m+1)
                            Neta(n,m) = Neta(n,m+1)
                         endif
                      endif
                   endif
                   if (n+1 <= nmaxus) then
                      if (comparereal(poros(n,m),poros(n+1,m)) == 0) then
                         if (comparereal(Npsi(n+1,m),0._fp)==0 .and. comparereal(Neta(n+1,m),0._fp)==0) then
                            Npsi(n,m) = 1._fp
                            Neta(n,m) = 0._fp 
                         else
                            Npsi(n,m) = Npsi(n+1,m)
                            Neta(n,m) = Neta(n+1,m)
                         endif
                      endif
                   endif
                   if(m-1 >= 1) then
                      if (comparereal(poros(n,m),poros(n,m-1)) == 0) then
                         if (comparereal(Npsi(n,m-1),0._fp)==0 .and. comparereal(Neta(n,m-1),0._fp)==0) then
                            Npsi(n,m) = 0._fp
                            Neta(n,m) = 1._fp
                         else
                            Npsi(n,m) = Npsi(n,m-1)
                            Neta(n,m) = Neta(n,m-1)
                         endif
                      endif
                   endif
                   if (n-1 >= 1) then
                      if (comparereal(poros(n,m),poros(n-1,m)) == 0) then
                         if (comparereal(Npsi(n-1,m),0._fp)==0 .and. comparereal(Neta(n-1,m),0._fp)==0) then
                            Npsi(n,m) = 1._fp
                            Neta(n,m) = 0._fp 
                         else
                            Npsi(n,m) = Npsi(n-1,m)
                            Neta(n,m) = Neta(n-1,m)
                         endif                     
                      endif
                   endif
                   !
                   ! recompute new module
                   !
                   modN = sqrt(Npsi(n,m)**2+Neta(n,m)**2)
                   !
                else
                   !
                   ! correct one strip of porous cells
                   !
                   if ((comparereal(poros(n+1,m  ),1._fp).lt.0.and.comparereal(poros(n-1,m  ),1._fp).lt.0).and. &
                       (comparereal(poros(n  ,m+1),1._fp).eq.0.and.comparereal(poros(n  ,m-1),1._fp).eq.0) ) then
                      !
                      ! default value i just choose a random direction
                      !
                      Npsi(n,m) = 1._fp
                      Neta(n,m) = 0._fp 
                       elseif ((comparereal(poros(n+1,m  ),1._fp).eq.0.and.comparereal(poros(n-1,m  ),1._fp).eq.0).and. &
                           (comparereal(poros(n  ,m+1),1._fp).lt.0.and.comparereal(poros(n  ,m-1),1._fp).lt.0) ) then
                      !
                      ! default value i just choose a random direction
                      !
                      Npsi(n,m) = 0._fp
                      Neta(n,m) = 1._fp 
                   endif
                   modN = sqrt(Npsi(n,m)**2+Neta(n,m)**2)
                endif
                !
                ! ROTATE is used to computed Nx and Ny that are not used here but in update
                ! Npsi(n,m) and Neta(n,m) are the components of the normal to the interface LOCALLY in the reference element
                !
                CALL ROTATE(Npsi(n,m),Neta(n,m),PSIx(n,m),PSIy(n,m),1,Nx(n,m),Ny(n,m)) 
                !
                !write(19911991,'(3i8,6f25.15)') nst,m,n,Npsi(n,m),Neta(n,m),PSIx(n,m),PSIy(n,m),Nx(n,m),Ny(n,m)
                Nx(n,m) = Nx(n,m)/modN
                Ny(n,m) = Ny(n,m)/modN
                !if (nst.ge.idebugCUTini.and.nst.le.idebugCUTfin) THEN
                if (idebugCUT.eq.1) then
                    if(nst-nstREST.eq.0) then
                       scale = (deta(n,m)+dpsi(n,m))*0.5_fp !*100
                       write(66666,'(4f25.15)') xG(n,m)      ,yG(n,m)
                       write(66666,'(4f25.15)' )  xG(n,m)+ Nx(n,m) *scale,yG(n,m)+ Ny(n,m) *scale
                    endif
                endif
                absNpsi = abs(Npsi(n,m))
                absNeta = abs(Neta(n,m))

                absNpsiTRANS = absNpsi*dpsi(n,m)
                absNetaTRANS = absNeta*deta(n,m)
                sumN         = absNpsiTRANS+absNetaTRANS
                CALL ALPHAvof(absNpsiTRANS/sumN, absNetaTRANS/sumN, sumN               , poros(n,m)        , alphaD(n,m), &
                              INTx             , INTy             , INTwx              , INTwy             , L1         , L2, &
                              Ndry             , Nwet             , EDGEtypeBANK(1,n,m), EDGElenBANK(1,n,m), EDGxyDRY   ) ! 1_fp-poros(n,m). I use the absolute value of the slopes and I mirrow the results below                
                !do k=1,Ndry
                !   write(9999911,'(f25.15,2i9,4f25.15)')t,m,n,INTx(k),INTy(k)
                !   if (isnan(INTx(k)).or.isnan(INTy(k))) then
                !     Ndry = Ndry
                !     continue
                !   endif
                !enddo
                !
                ! multiply for the the edges to find the locations for the local non-unit rectangle
                !
                INTx(1:Ndry)  = INTx(1:Ndry)*dpsi(n,m)
                INTy(1:Ndry)  = INTy(1:Ndry)*deta(n,m)
                INTwx(1:Nwet) = INTwx(1:Nwet)*dpsi(n,m)
                INTwy(1:Nwet) = INTwy(1:Nwet)*deta(n,m)
                !
                ! multiply for the the edges to compute the right length of dry edge and the 2 points defining it
                !
                EDGElenBANK(1,n,m) = EDGElenBANK(1,n,m)*dpsi(n,m) 
                EDGElenBANK(2,n,m) = EDGElenBANK(2,n,m)*deta(n,m) 
                EDGElenBANK(3,n,m) = EDGElenBANK(3,n,m)*dpsi(n,m) 
                EDGElenBANK(4,n,m) = EDGElenBANK(4,n,m)*deta(n,m) 
                EDGxyDRY(1,1:2,1)  = EDGxyDRY(1,1:2,1)*dpsi(n,m)  !psi-comp
                !EDGxyDRY(1,1:2,2)  = EDGxyDRY(1,1:2,2)*deta(n,m)  !eta-comp (it is zero not needed)
                EDGxyDRY(2,1:2,1)  = EDGxyDRY(2,1:2,1)*dpsi(n,m)  !psi-comp
                EDGxyDRY(2,1:2,2)  = EDGxyDRY(2,1:2,2)*deta(n,m)  !eta-comp
                EDGxyDRY(3,1:2,1)  = EDGxyDRY(3,1:2,1)*dpsi(n,m)  !psi-comp
                EDGxyDRY(3,1:2,2)  = EDGxyDRY(3,1:2,2)*deta(n,m)  !eta-comp
                !EDGxyDRY(4,1:2,1)  = EDGxyDRY(4,1:2,1)*dpsi(n,m)  !psi-comp (it is zero not needed)
                EDGxyDRY(4,1:2,2)  = EDGxyDRY(4,1:2,2)*deta(n,m)  !eta-comp
                !
                ! x-mirroring
                !
                if (Npsi(n,m) < 0.0_fp) then  
                   INTx(1:Ndry)  = dpsi(n,m) - INTx(1:Ndry) 
                   INTwx(1:Nwet) = dpsi(n,m) - INTwx(1:Nwet) 
                   !INTx(2)      = dpsi(n,m) - INTx(2) 
                   !swapEDGEtype = .false.
                   if (L1 == 2) then
                      !swapEDGEtype = .true.
                      L1 = 4
                   endif
                   if (L2 == 4) then
                      !swapEDGEtype =.true.
                      L2 = 2
                   endif
                   !if (swapEDGEtype) then
                      intPROV             = EDGEtypeBANK(4,n,m)            ! SWAP  EDGEtypeBANK 4 WITH 2 
                      EDGEtypeBANK(4,n,m) = EDGEtypeBANK(2,n,m)
                      EDGEtypeBANK(2,n,m) = intPROV
                      realPROV            = EDGElenBANK(4,n,m)
                      EDGElenBANK(4,n,m)  = EDGElenBANK(2,n,m)
                      EDGElenBANK(2,n,m)  = realPROV
                      provEDGxy(1:2,2)    = EDGxyDRY(4,1:2,2)              ! note I swap only y, x are already correct
                      EDGxyDRY(4,1:2,2)   = EDGxyDRY(2,1:2,2)
                      EDGxyDRY(2,1:2,2)   = provEDGxy(1:2,2)
                      EDGxyDRY(3,1:2,1)   = dpsi(n,m) - EDGxyDRY(3,1:2,1)  ! note I only translate in x, y are already correct
                      EDGxyDRY(1,1:2,1)   = dpsi(n,m) - EDGxyDRY(1,1:2,1)                       
                   !endif
                endif
                !
                ! y-mirroring
                !
                if (Neta(n,m) < 0.0_fp) then 
                   INTy(1:Ndry)  = deta(n,m) - INTy(1:Ndry)        
                   INTwy(1:Nwet) = deta(n,m) - INTwy(1:Nwet)    
                   !swapEDGEtype =.false.
                   !INTy(2)      = deta(n,m) - INTy(2)  
                   if (L1 == 1) then
                     L1 = 3
                   !  swapEDGEtype =.true.
                   endif
                   if (L2 == 3) then
                     L2 = 1  
                     !swapEDGEtype =.true.                      
                   endif
                   !if (swapEDGEtype) then                 
                      intPROV             = EDGEtypeBANK(3,n,m)             ! SWAP  EDGEtypeBANK 1 WITH 3 
                      EDGEtypeBANK(3,n,m) = EDGEtypeBANK(1,n,m)
                      EDGEtypeBANK(1,n,m) = intPROV
                      realPROV            = EDGElenBANK(3,n,m)
                      EDGElenBANK(3,n,m)  = EDGElenBANK(1,n,m)
                      EDGElenBANK(1,n,m)  = realPROV
                      provEDGxy(1:2,1)    =  EDGxyDRY(3,1:2,1)              ! note I swap only x, y are already correct
                      EDGxyDRY(3,1:2,1)   = EDGxyDRY(1,1:2,1)
                      EDGxyDRY(1,1:2,1)   = provEDGxy(1:2,1)
                      EDGxyDRY(4,1:2,2)   = deta(n,m) -  EDGxyDRY(4,1:2,2)  ! note I only translate in y, x are already correct
                      EDGxyDRY(2,1:2,2)   = deta(n,m) -  EDGxyDRY(2,1:2,2)
                   !endif
                endif
                !
                ! rotate back the element
                !
                CALL ROTATE(INTx(1) ,INTy(1) ,PSIx(n,m),PSIy(n,m),Ndry,INTxR(1) ,INTyR(1)) 
                CALL ROTATE(INTwx(1),INTwy(1),PSIx(n,m),PSIy(n,m),Nwet,INTwxR(1),INTwyR(1)) 
                CALL ROTATE(EDGxyDRY(L1,1:2,1),EDGxyDRY(L1,1:2,2),PSIx(n,m),PSIy(n,m),2,provEDGxy(1:2,1),provEDGxy(1:2,2))
                EDGxyDRY(L1,1:2,1) =  provEDGxy(1:2,1)
                EDGxyDRY(L1,1:2,2) =  provEDGxy(1:2,2)
                CALL ROTATE(EDGxyDRY(L2,1:2,1),EDGxyDRY(L2,1:2,2),PSIx(n,m),PSIy(n,m),2,provEDGxy(1:2,1),provEDGxy(1:2,2))
                EDGxyDRY(L2,1:2,1) =  provEDGxy(1:2,1)
                EDGxyDRY(L2,1:2,2) =  provEDGxy(1:2,2)
                !
                ! find position on global reference system (GRS)
                !
                INTx_GRS(1:Ndry,n,m)      = INTxR(1:Ndry)  + xcor0(n-1,m-1)
                INTy_GRS(1:Ndry,n,m)      = INTyR(1:Ndry)  + ycor0(n-1,m-1)
                INTwx_GRS(1:Nwet,n,m)     = INTwxR(1:Nwet) + xcor0(n-1,m-1)
                INTwy_GRS(1:Nwet,n,m)     = INTwyR(1:Nwet) + ycor0(n-1,m-1)
                EDGExyBANK(n,m,1:4,1:2,1) = EDGxyDRY(1:4,1:2,1) + xcor0(n-1,m-1)   
                EDGExyBANK(n,m,1:4,1:2,2) = EDGxyDRY(1:4,1:2,2) + ycor0(n-1,m-1)                 
                !L_GRS(1,n,m) = L1
                !L_GRS(2,n,m) = L2
                Ndry_GRS(n,m) = Ndry
                Nwet_GRS(n,m) = Nwet
                if (nst.ge.idebugCUTini .and. nst.le.idebugCUTfin) THEN
                   if (nst-nstREST.eq.0) then
                      write(167890,'(2f25.15)')    INTx_GRS(1,n,m),INTy_GRS(1,n,m)
                      write(167890,'(2f25.15)')    INTx_GRS(Ndry,n,m),INTy_GRS(Ndry,n,m)
                      do I = 1,Ndry
                         write(167891,'(2f25.15)')    INTx_GRS(I,n,m),INTy_GRS(I,n,m)
                      enddo
                      write(167892,'(i9)') Ndry
                   endif
                endif
                !
                EDGEtypeBANKerod(:,n,m) = EDGEtypeBANK(:,n,m)
                !
                ! this is needed for time varying wetting and drying of banks. 
                ! If it dries EDGEtypeBANK becomes -1 and then if it did not erode it has the wrong value
                !
                STOREedge2(:,n,m) = EDGEtypeBANK(1:4,n,m) 
                EDGElenWET(n,m,1) = dpsi(n,m) - EDGElenBANK(1,n,m)
                EDGElenWET(n,m,2) = deta(n,m) - EDGElenBANK(2,n,m)
                EDGElenWET(n,m,3) = dpsi(n,m) - EDGElenBANK(3,n,m)
                EDGElenWET(n,m,4) = deta(n,m) - EDGElenBANK(4,n,m)
                !
                ! this is needed for time varying wetting and drying of banks. 
                ! If it dries EDGEtypeBANK becomes -1 and then if it did not erode it has the wrong value
                !
                STOREedgeLEN(:,n,m) =  EDGElenWET(n,m,:)
             endif !end if CELLtoRECON
             !
             ! define EDGEtypeBANK, EDGElenBANK and EDGExyBANK for the cases not considered 
             ! (and overwrite on the first time step when I reconstruct even where banks are dry and far from the channel)
             !
             ! poros not 1 or 0 and cell dry
             !
             !if (comparereal(poros(n,m),1._fp)==0) then  
             if (kfs_cc(n,m).eq.-1) then 
                do k=1,4
                   if(EDGEtypeBANK(k,n,m).eq.0) then
                      !
                      ! the cut cell becomes dry
                      !
                      EDGEtypeBANK(k,n,m) = -1
                   else
                      ! it becomes dry
                      !
                      EDGEtypeBANK(k,n,m) = - abs(EDGEtypeBANK(k,n,m))
                   endif
                enddo
                !
                ! in this way I have the active length correct for the dry part
                !
                EDGElenWET(n,m,:)         = STOREedgeLEN(:,n,m) 
                EDGEtypeBANKerod(1:4,n,m) = EDGEtypeBANK(1:4,n,m)
             elseif(kfs_cc(n,m).eq.0) then
                 !
                 ! continue (it has been recontructed)   
                 !
                 ! I use the stored value, it is not implied that  I do reconstruction, 
                 ! it could just become wet and I don't have the values of EDGEtypeBANK on the 4 sides anymore
                 !
                 ! as an alternative, in checkDRY at each n,m cycle define dryold and if new dry is different 
                 ! from dry old define drySTATUSchange=.true. 
                 ! if dry goes from 0 to 1 and reconstruction has to be done in this case. 
                 ! But this is more expensive, reconstruction has to be done every time even if no erosion and only tide.
                 !
                 EDGEtypeBANK(1:4,n,m)      = STOREedge2(1:4,n,m)     
                 EDGEtypeBANKerod(1:4,n,m)  = STOREedge2(1:4,n,m)  
                 EDGElenWET(n,m,1:4)        = STOREedgeLEN(1:4,n,m) 
             elseif(kfs_cc(n,m).eq.1) THEN 
                !
                ! poros not 1 or 0 and cell wet
                !
                do k=1,4
                   if(EDGEtypeBANK(k,n,m).eq.0) then
                      !
                      ! the cut cell becomes submerged
                      !
                      EDGEtypeBANK(k,n,m) = 1 
                   else
                      !
                      ! it becomes wet
                      !
                      EDGEtypeBANK(k,n,m) = abs(EDGEtypeBANK(k,n,m)) 
                   endif
                enddo
                EDGElenWET(n,m,1) = dpsi(n,m)  
                EDGElenWET(n,m,2) = deta(n,m) 
                EDGElenWET(n,m,3) = dpsi(n,m)  
                EDGElenWET(n,m,4) = deta(n,m)
                EDGEtypeBANKerod(1:4,n,m) = EDGEtypeBANK(1:4,n,m)
             !elseif (comparereal(poros(n,m),0._fp)==0) then
             elseif(kfs_cc(n,m).eq.-2) THEN
                !
                ! poros=0 (vegetated) and cell dry
                !
                ! fully land edge
                !
                EDGEtypeBANK(1:4,n,m)       = -2
                EDGEtypeBANKerod(1:4,n,m)   = -2
                EDGElenBANK(1,n,m) = dpsi(n,m) 
                EDGElenBANK(2,n,m) = deta(n,m) 
                EDGElenBANK(3,n,m) = dpsi(n,m) 
                EDGElenBANK(4,n,m) = deta(n,m) 
                EDGElenWET(n,m,1) = 0._fp
                EDGElenWET(n,m,2) = 0._fp
                EDGElenWET(n,m,3) = 0._fp
                EDGElenWET(n,m,4) = 0._fp
                EDGExyBANK(n,m,1,1,1) = xcor0(n-1,m-1)
                EDGExyBANK(n,m,2,1,1) = xcor0(n-1,m)
                EDGExyBANK(n,m,3,1,1) = xcor0(n,m)
                EDGExyBANK(n,m,4,1,1) = xcor0(n,m-1)
                EDGExyBANK(n,m,1,2,1) = xcor0(n-1,m)
                EDGExyBANK(n,m,2,2,1) = xcor0(n,m)
                EDGExyBANK(n,m,3,2,1) = xcor0(n,m-1)
                EDGExyBANK(n,m,4,2,1) = xcor0(n-1,m-1)
                EDGExyBANK(n,m,1,1,2) = ycor0(n-1,m-1)
                EDGExyBANK(n,m,2,1,2) = ycor0(n-1,m)
                EDGExyBANK(n,m,3,1,2) = ycor0(n,m)
                EDGExyBANK(n,m,4,1,2) = ycor0(n,m-1)
                EDGExyBANK(n,m,1,2,2) = ycor0(n-1,m)
                EDGExyBANK(n,m,2,2,2) = ycor0(n,m)
                EDGExyBANK(n,m,3,2,2) = ycor0(n,m-1)
                EDGExyBANK(n,m,4,2,2) = ycor0(n-1,m-1)
             elseif(kfs_cc(n,m).eq.2) THEN
                !
                ! poros=0 (vegetated)  and cell wet
                !
                EDGEtypeBANK(1:4,n,m)     = 2
                EDGEtypeBANKerod(1:4,n,m) = 2
                EDGElenBANK(1,n,m) = dpsi(n,m) 
                EDGElenBANK(2,n,m) = deta(n,m) 
                EDGElenBANK(3,n,m) = dpsi(n,m) 
                EDGElenBANK(4,n,m) = deta(n,m) 
                EDGElenWET(n,m,1) = dpsi(n,m)  
                EDGElenWET(n,m,2) = deta(n,m) 
                EDGElenWET(n,m,3) = dpsi(n,m)  
                EDGElenWET(n,m,4) = deta(n,m)
                EDGExyBANK(n,m,1,1,1) = xcor0(n-1,m-1)
                EDGExyBANK(n,m,2,1,1) = xcor0(n-1,m)
                EDGExyBANK(n,m,3,1,1) = xcor0(n,m)
                EDGExyBANK(n,m,4,1,1) = xcor0(n,m-1)
                EDGExyBANK(n,m,1,2,1) = xcor0(n-1,m)
                EDGExyBANK(n,m,2,2,1) = xcor0(n,m)
                EDGExyBANK(n,m,3,2,1) = xcor0(n,m-1)
                EDGExyBANK(n,m,4,2,1) = xcor0(n-1,m-1)
                EDGExyBANK(n,m,1,1,2) = ycor0(n-1,m-1)
                EDGExyBANK(n,m,2,1,2) = ycor0(n-1,m)
                EDGExyBANK(n,m,3,1,2) = ycor0(n,m)
                EDGExyBANK(n,m,4,1,2) = ycor0(n,m-1)
                EDGExyBANK(n,m,1,2,2) = ycor0(n-1,m)
                EDGExyBANK(n,m,2,2,2) = ycor0(n,m)
                EDGExyBANK(n,m,3,2,2) = ycor0(n,m-1)
                EDGExyBANK(n,m,4,2,2) = ycor0(n-1,m-1)
             elseif(kfs_cc(n,m).eq.-3) THEN
                !
                ! poros=1 and cell dry
                !
                EDGEtypeBANK(1:4,n,m)     = -3
                EDGEtypeBANKerod(1:4,n,m) = -3
                EDGElenWET(n,m,1) = dpsi(n,m)  
                EDGElenWET(n,m,2) = deta(n,m) 
                EDGElenWET(n,m,3) = dpsi(n,m)  
                EDGElenWET(n,m,4) = deta(n,m)
             elseif(kfs_cc(n,m).eq.3) then
                !
                ! poros=1 and cell wet
                !
                ! fully channel edge
                !
                EDGEtypeBANK(1:4,n,m)     = 3
                EDGEtypeBANKerod(1:4,n,m) = 3
                EDGElenBANK(1:4,n,m) = 0 
                EDGElenWET(n,m,1) = dpsi(n,m)  
                EDGElenWET(n,m,2) = deta(n,m) 
                EDGElenWET(n,m,3) = dpsi(n,m)  
                EDGElenWET(n,m,4) = deta(n,m)
             endif       
          else !if(kcs(n,m).eq.0)
             !
             ! fully land edge
             !
             EDGEtypeBANK(1:4,n,m)     = -2 
             EDGEtypeBANKerod(1:4,n,m) = -2
             EDGElenBANK(1,n,m) = dpsi(n,m) 
             EDGElenBANK(2,n,m) = deta(n,m) 
             EDGElenBANK(3,n,m) = dpsi(n,m) 
             EDGElenBANK(4,n,m) = deta(n,m) 
             EDGElenWET(n,m,1) = 0._fp  
             EDGElenWET(n,m,2) = 0._fp
             EDGElenWET(n,m,3) = 0._fp
             EDGElenWET(n,m,4) = 0._fp
             EDGExyBANK(n,m,1,1,1) = xcor0(n-1,m-1)
             EDGExyBANK(n,m,2,1,1) = xcor0(n-1,m)
             EDGExyBANK(n,m,3,1,1) = xcor0(n,m)
             EDGExyBANK(n,m,4,1,1) = xcor0(n,m-1)
             EDGExyBANK(n,m,1,2,1) = xcor0(n-1,m)
             EDGExyBANK(n,m,2,2,1) = xcor0(n,m)
             EDGExyBANK(n,m,3,2,1) = xcor0(n,m-1)
             EDGExyBANK(n,m,4,2,1) = xcor0(n-1,m-1)
             EDGExyBANK(n,m,1,1,2) = ycor0(n-1,m-1)
             EDGExyBANK(n,m,2,1,2) = ycor0(n-1,m)
             EDGExyBANK(n,m,3,1,2) = ycor0(n,m)
             EDGExyBANK(n,m,4,1,2) = ycor0(n,m-1)
             EDGExyBANK(n,m,1,2,2) = ycor0(n-1,m)
             EDGExyBANK(n,m,2,2,2) = ycor0(n,m)
             EDGExyBANK(n,m,3,2,2) = ycor0(n,m-1)
             EDGExyBANK(n,m,4,2,2) = ycor0(n-1,m-1)             
          endif
       enddo
    enddo
    !
    !
    !   july 2014: I should think to do not do reconstruction for kcs==2. I didnt remove kcs==2 yet cause 
    !   I am not sure something like Ndry or normals is needed. If not it can be removed. Either way,
    !   I would add the following code, since it could be that since the reconstruciton in kcs==2 is not exact,
    !   the interface is not continuous at the domain boundary and this changes the values of active edge aguu and agvv 
    !   at the boundary. In the follow, I force the edges to have the same property of the the adjacent (with kcs/=2) 
    !   in the way that aguu and agvv are given as the interface is continuous and exits the boundary properly).
    !
    !   penso inoltre che la etrapolazione della porosita fatta in BC_VOF sia inutile, e che il calcolo della normale potrebbe essere 
    !   fatto escludendo il stencil con 4 celle di Young and Parker che hanno anche un solo  kcs=2. da verificare quale e` piu` accurato
    !   pero' devo tenerla per le condizioni periodiche!! perche` la porosity e` fatta periodica e mi serve per fare le cose simmetriche nel 
    !   canale circolare o dritto. quindi mettere in parker e young if (kcs==2.and.iper==1) considera lo stencil se no salta!!
    !   however note that in the circular case alphavof does the riconstruction in kcs=2 with an area equal to zero, but thats not a
    !   prblem since it is done on the reference element 1x1 and then scaled with the edges but without division so it should be ok. if
    !   problems i can do the xcor,ycor (maybe xcor0,ycor0) periodic, and then add an if in the search of the ghost cell saying that 
    !   if(initial_area_grid==0) skip  
    !
    !    DO m=1,mmax  
    !       DO n=1,nmaxus  
    !          if (kcs(n,m)==2) then
    !             do K=1,4
    !                nADJ = nAD(n,m,K)
    !                mADJ = mAD(n,m,K)             
    !                if (kcs(nADJ,mADJ)==1) then !maybe kcs/=1 but i dont think i need to copy the values from edges with kcs=0
    !                   kADJ = edge6(k+2)   
    !                   EDGEtypeBANK(K,n,m)      = EDGEtypeBANK(kADJ,nADJ,mADJ)
    !                   EDGEtypeBANKerod(K,n,m)  = EDGEtypeBANKerod(kADJ,nADJ,mADJ)
    !                   EDGElenBANK(K,n,m)       = EDGElenBANK(kADJ,nADJ,mADJ)
    !                   EDGElenWET(n,m,k)        = EDGElenWET(nADJ,mADJ,kADJ) 
    !                   EDGExyBANK(n,m,K,1,1) = EDGExyBANK(nADJ,mADJ,kADJ,1,1)
    !                   EDGExyBANK(n,m,K,2,1) = EDGExyBANK(nADJ,mADJ,kADJ,2,1)
    !                   EDGExyBANK(n,m,K,1,2) = EDGExyBANK(nADJ,mADJ,kADJ,1,2)
    !                   EDGExyBANK(n,m,K,2,2) = EDGExyBANK(nADJ,mADJ,kADJ,2,2)
    !                endif
    !             enddo
    !          endif
    !       ENDDO
    !    ENDDO
    !
    !   UPDATE APRIL 2015: CELLtoRECON IS   TRUE IN KCS==2   SINCE i SET UPDATEDbank = TRUE AT THE BOUND IN PLIC_VOF_INIT. Therefore normal is ALWAYS computed 
    !                     . I think though the best is copying the normal from the adjacent cells with kfs==1 and kcs==1 (average if more than one, i.e. corner 
    !                    boundary. And copying edge properties. And use a random value of porosity (like 0.5) only to have correct checkdry, and avoid kcs=2 and   0
    !                    from computation of normal with Parker (if any of the 4 cells in each the Parker differences has kcs==0 or 2 exclude it from  computation
    !   
    select case(BOUNDvof) 
    case(1)
       !
       ! copy nx and ny
       !
       ! This should be a do loop over n and m with checks on kcs(n,m) == 0 or 2 and use the average
       ! of adjacent nx and ny in the 9 cell stencil, including only cell having |kcs_cc| <= 1 
       ! (cutcell: kfs_cc=0,     cutcell submerged: kfs_cc=1,     cutcell dry: kfs_cc=-1)
       ! (                   non-cutcell submerged: kfs_cc=2, non-cutcell dry: kfs_cc=-2)
       !
       !do n = 1, nmax
       !    do m = 1, mmax
       !        if (kcs(n,m) == 0 .or. kcs(n,m) == 2) then
       !           ...
       !        endif
       !    enddo
       !enddo
       nx(1,:)    = nx(2,:)
       nx(nmaxus,:) = nx(nmaxus-1,:)
       nx(:,1)    = nx(:,2)
       nx(:,mmax) = nx(:,mmax-1)
       ny(1,:)    = ny(2,:)
       ny(nmaxus,:) = ny(nmaxus-1,:)
       ny(:,1)    = ny(:,2)
       ny(:,mmax) = ny(:,mmax-1)
    case default
       write(*,*) 'Nx and Ny has still to be computed for this value of BOUNDvof'
       call d3stop(1,gdp)
    end select

    if (periodSURFACE) then
       if(periodGHOST) then
          call extrapPERIODICedgeBANK1(gdp)
          call periodic_NxNy(gdp)
       endif
    endif
!
    if (PERIODalongM==1) then
       !
       ! Used to exclude kcs=2 with adjacent kcs=0 othogonal to periodic direction 
       ! (it would skip computation of guu_cc gvv_cc below at the margin of the boundary)
       !
       kPER = 2
    elseif (PERIODalongM==0) then
       kPER = 3
    elseif (PERIODalongM==2) then
       !       
       ! non periodic
       !
       kPER = -1
    endif
    !
    ! define the erosion length  on the edges that are cut at both sides. This is needed only for 
    ! morphodynamics (i.e. for performing bank erosion)
    !
    do m=1,mmax !-1 
       do n=1,nmaxus !-1
          !if (CELLtoRECON(n,m)) then
          do K=2,3  ! It is only a cycle  ON THE EDGES with the velocity points (k=2,3)
             nADJ = nAD(n,m,K)
             mADJ = mAD(n,m,K)
             !
             ! to be optimized, the cycle should be done on the  edges otherwise when EDGEtypeBANK is 2 on both sides it is done twice.
             ! (maybe a cycle in n and m with k=2,3 ( i.e. only the hor and vert velocity points) should work, by changing something below)
             !
             kADJ = edge6(k+2) 
             if (nADJ.lt.1.or.nADJ.gt.nmaxus) cycle
             if (mADJ.lt.1.or.mADJ.gt.mmax) cycle
             if (.not.(((kcs(n,m)==0.and.kcs(nADJ,mADJ)==0).or.(kcs(nADJ,mADJ)==0.and.kcs(n,m)==0).or. &
                      & (kcs(n,m)==2.and.kcs(nADJ,mADJ)==0).or.(kcs(n,m)==0.and.kcs(nADJ,mADJ)==2)) .and. (k==kPER))) then 
                !
                ! I exclude interface between first and second periodic row (if twoCELLSperiod) 
                ! aguu,agvv EDGExyBANKerod EDGEtypeBANKerod and other stays undefined but it should be fine
                !
             !if (CELLtoRECON(n,m).OR.CELLtoRECON(nADJ,mADJ)) then
                !! So it is done only if one of them is reconstructed. 
                !! There is not point to do it for fully dry cut cells that are not reconstructed and not eroded)
                !!if ((EDGEtypeBANK(k,n,m).eq.2.or.EDGEtypeBANK(k,n,m).eq.0).and.(EDGEtypeBANK(kADJ,nADJ,mADJ).eq.2)) then !BOTH EDGES ARE CUT 
                if (ERODsubmBANKS.EQ.1) then
                   !
                   ! EDGEtypeBANK(k,n,m).eq.1.or.EDGEtypeBANK(k,n,m).eq.0 !abs(EDGEtypeBANK(k,n,m)).le.1 : i catch -1,0,1
                   !
                   EDGEtypeBANK_1_LEFT  = abs(EDGEtypeBANK(k,n,m)).le.1 
                   !
                   ! EDGEtypeBANK(kADJ,nADJ,mADJ).eq.1.or.EDGEtypeBANK(kADJ,nADJ,mADJ).eq.0  !abs(EDGEtypeBANK(kADJ,nADJ,mADJ)).le.1  : i catch -1,0,1
                   !
                   EDGEtypeBANK_1_RIGHT = abs(EDGEtypeBANK(kADJ,nADJ,mADJ)).le.1  
                   EDGEtypeBANK_2_LEFT  = abs(EDGEtypeBANK(k,n,m)).eq.2
                   EDGEtypeBANK_2_RIGHT = abs(EDGEtypeBANK(kADJ,nADJ,mADJ)).eq.2
                   !
                   ! These are the potentially erodible banks. 
                   ! The case EDGEtypeBANK(k,n,m)=-1.and.EDGEtypeBANK(kADJ,nADJ,mADJ)).eq.-1 is also here, that it
                   ! shouldnt. I exclude it after (see my eroding or not notes)
                   !
                else
                   !
                   ! EDGEtypeBANK(k,n,m).eq.1.or.EDGEtypeBANK(k,n,m).eq.0 !abs(EDGEtypeBANK(k,n,m)).le.1 : i catch -1,0,1
                   !
                   EDGEtypeBANK_1_LEFT  = abs(EDGEtypeBANK(k,n,m)).le.1 
                   !
                   ! EDGEtypeBANK(kADJ,nADJ,mADJ).eq.1.or.EDGEtypeBANK(kADJ,nADJ,mADJ).eq.0  !abs(EDGEtypeBANK(kADJ,nADJ,mADJ)).le.1 : i catch -1,0,1
                   !
                   EDGEtypeBANK_1_RIGHT =  abs(EDGEtypeBANK(kADJ,nADJ,mADJ)).le.1 
                   EDGEtypeBANK_2_LEFT  = EDGEtypeBANK(k,n,m).eq.-2
                   EDGEtypeBANK_2_RIGHT = EDGEtypeBANK(kADJ,nADJ,mADJ).eq.-2
                   !
                   ! NOTE: For cut cells I dont know a priori what cell is eroded (n,m or nadj,madj). 
                   ! So i considered all the cases. 
                   ! I can safely exclude only EDGEtypeBANK=2 since its submerged and i do not have to erode it
                   ! These are the potentially erodible banks. 
                   ! The case EDGEtypeBANK(k,n,m)=1.and.EDGEtypeBANK(kADJ,nADJ,mADJ)).eq.1 (and others) is also here, that it
                   ! shouldnt. I exclude it after (see my eroding or not notes, case (7) is admissible and has EDGEtypeBANK.eq.1 on one side. 
                   ! And even case (8) so i need to consider EDGEtypeBANK=-1)
                   !
                endif
                !
                if ((EDGEtypeBANK_1_LEFT.and.EDGEtypeBANK_2_RIGHT).or. &
                    (EDGEtypeBANK_2_LEFT.and.EDGEtypeBANK_1_RIGHT).or. &
                    (EDGEtypeBANK_1_LEFT.and.EDGEtypeBANK_1_RIGHT)) then                  
                !if ((EDGEtypeBANK(k,n,m).eq.2.and.EDGEtypeBANK(kADJ,nADJ,mADJ).eq.0).or. &
                !    (EDGEtypeBANK(k,n,m).eq.0.and.EDGEtypeBANK(kADJ,nADJ,mADJ).eq.2).or. &
                !    (EDGEtypeBANK(k,n,m).eq.2.and.EDGEtypeBANK(kADJ,nADJ,mADJ).eq.2)) then
                   call Diff_segm(EDGExyBANK(n,m,k,1,1)         ,EDGExyBANK(n,m,k,1,2)          ,EDGExyBANK(n,m,k,2,1)         ,EDGExyBANK(n,m,k,2,2)      , &
                                  EDGExyBANK(nADJ,mADJ,kADJ,1,1),EDGExyBANK(nADJ,mADJ,kADJ,1,2) ,EDGExyBANK(nADJ,mADJ,kADJ,2,1), & 
                                  EDGExyBANK(nADJ,mADJ,kADJ,2,2), &
                                  xint     ,yint     ,typeINTER ,numSEGout  ,SEGisEXTRpoint(1)  )
                   if (typeINTER.eq.2) then
                      !
                      ! collinear and intersecting
                      !
                      !EDGElenDRYeff(K,n,m)  = abs(EDGElenBANK(K,n,m)-EDGElenBANK(kADJ,nADJ,mADJ))
                      !
                      ! Note: here kk=1 and kk=2 since I always have 2 segments when I compute the difference, 
                      ! only that one is of zero length if they both start on the same vertex 
                      !
                      !
                      ! First segment to be eroded, belonging to the  cell (n,m)  (might be degenerate, i.e. of zero length).
                      ! Note: by construction in Diff_segm, it is always the subsegment of the first edge 
                      ! (i.e. EDGExyBANK(n,m), not EDGExyBANK(nADJ,mADJ,kADJ))
                      !
                      kk = 1
                      !
                      ! note they cannot be both, otherwise it would not be a cut edge                      
                      !
                      anySEGisEXTRpoint = ANY(SEGisEXTRpoint(1:2))
                      !
                      ! if SEGisEXTRpoint(1).eq.true kk=1 contains a segment degenerated to a point coinciding with the corner of the quadrilateral cell. 
                      ! I skip it.
                      !
                      if (.not.SEGisEXTRpoint(kk)) then 
                         ! note: SEGisEXTRpoint tells only if the corner points of the cell are degenerated segment. 
                         ! It could be still that the 2 interfaces of adj cells meet at the same point in the edge. 
                         ! Than that point could coincide with a velocity point. 
                         ! I want to keep open the option to consider that as a ghost cell, so typeBANKeros is 4 or 5 in that case not 6
                         !
                         LENGTHeros = sqrt((xint(1,kk)-xint(2,kk))**2+(yint(1,kk)-yint(2,kk))**2)
                         if (comparereal(LENGTHeros,PERCedge*max(dpsi(n,m),deta(n,m))) .gt. 0) then ! real(k-2,fp)*dpsi(n,m)+real(abs(k-3))*deta(n,m) ! if k=2  use deta, if k=3 use dpsi
                             ADJnonDRY = EDGEtypeBANK(kADJ,nADJ,mADJ).ne.-1
                             if (ERODsubmBANKS.eq.1.and.ADJnonDRY) then
                                !
                                ! i exclude the case (2) and (3) in my notes titled "eroding or not"
                                !
                                ! to be eroded (the adjacent cell has always a EDGEtypeBANK(nADJ,mADJ,kADJ) = 2)
                                !
                                EDGEtypeBANKerod(K,n,m) = 4
                             elseif (ERODsubmBANKS.eq.0.and.ADJnonDRY.and.EDGEtypeBANK(k,n,m).ne.1) then
                                !
                                ! i exclude the case (6) and (9) in my notes titled "eroding or not"
                                !
                                EDGEtypeBANKerod(K,n,m) = 4 
                             else
                                !EDGEtypeBANKerod(K,n,m) =   not changing
                             endif
                         else
                             !
                             ! the discontinuity is very small I can neglect it (I STILL WRITE EDGExyBANKerod it can be used in find_BI_PI.f90)
                             !
                             EDGEtypeBANKerod(k,n,m) = 5
                         endif
                         !
                         ! I overwrite with the effective dry edge (the intersection of the two dry edges)
                         !
                         EDGExyBANKerod(n,m,k,1,1) = xint(1,kk) 
                         EDGExyBANKerod(n,m,k,2,1) = xint(2,kk)
                         EDGExyBANKerod(n,m,k,1,2) = yint(1,kk)
                         EDGExyBANKerod(n,m,k,2,2) = yint(2,kk) 
                         IF (anySEGisEXTRpoint) THEN
                             !
                             ! there is only non-point segment in output from Diff_segm, i can safely ignore the adjacent
                             !
                             ! the adjacent has not to be eroded
                             !
                             EDGEtypeBANKerod(kADJ,nADJ,mADJ) = 6 
                             !
                             ! I overwrite with the effective dry edge (the intersection of the two dry edges) It should never be needed though.
                             !
                             EDGExyBANKerod(nADJ,mADJ,kADJ,1,1) = xint(1,kk) 
                             EDGExyBANKerod(nADJ,mADJ,kADJ,2,1) = xint(2,kk)
                             EDGExyBANKerod(nADJ,mADJ,kADJ,1,2) = yint(1,kk)
                             EDGExyBANKerod(nADJ,mADJ,kADJ,2,2) = yint(2,kk)      
                         endif     
                      endif
                      !
                      ! Second segment to be eroded, belonging to the adjacent cell (might be degenerate, i.e. of zero length)                       
                      !
                      kk = 2 
                      if (.not.SEGisEXTRpoint(kk)) then
                         !
                         ! if SEGisEXTRpoint(1).eq.true kk=1 contains a segment degenerated to a point coinciding with the corner of the quadrilater cell. 
                         ! I skip it.
                         !
                         LENGTHeros = sqrt((xint(1,kk)-xint(2,kk))**2+(yint(1,kk)-yint(2,kk))**2)
                         if (comparereal(LENGTHeros,PERCedge*max(dpsi(n,m),deta(n,m))) > 0) then
                            ADJnonDRY = EDGEtypeBANK(k,n,m).ne.-1
                            if (ERODsubmBANKS.eq.1.and.ADJnonDRY) then 
                               !
                               ! i exclude the case (2) and (3) in my notes titled "eroding or not"
                               !
                               ! to be eroded (the adjacent cell has always a EDGEtypeBANKerod(nADJ,mADJ,kADJ) = 2)
                               !
                               EDGEtypeBANKerod(kADJ,nADJ,mADJ) = 4
                            elseif (ERODsubmBANKS.eq.0.and.ADJnonDRY.and.EDGEtypeBANK(kADJ,nADJ,mADJ).ne.1) then
                               !
                               ! i exclude the case (6) in my notes titled "eroding or not"
                               !
                               EDGEtypeBANKerod(K,n,m) = 4 
                            else
                               ! 
                               ! EDGEtypeBANKerod(kADJ,nADJ,mADJ) =  not changing
                               !
                            endif
                         else
                            !
                            ! the discontinuity is very small I can neglect it (I STILL WRITE EDGExyBANKerod it can be used in find_BI_PI.f90)
                            !
                            EDGEtypeBANKerod(kADJ,nADJ,mADJ) = 5
                         endif    
                         !
                         ! I overwrite with the effective dry edge (the intersection of the two dry edges)
                         !
                         EDGExyBANKerod(nADJ,mADJ,kADJ,1,1) = xint(1,kk)
                         EDGExyBANKerod(nADJ,mADJ,kADJ,2,1) = xint(2,kk)
                         EDGExyBANKerod(nADJ,mADJ,kADJ,1,2) = yint(1,kk)
                         EDGExyBANKerod(nADJ,mADJ,kADJ,2,2) = yint(2,kk)  
                         IF (anySEGisEXTRpoint) THEN
                            !
                            ! there is only non-point segment in output from Diff_segm, i can safely ignore the adjacent (i.e. the cell m,n here) 
                            ! the adjacent has not to be eroded
                            !
                            EDGEtypeBANKerod(k,n,m) = 6 
                            !
                            ! I overwrite with the effective dry edge (the intersection of the two dry edges). It should never be needed though.
                            !
                            EDGExyBANKerod(n,m,k,1,1) = xint(1,kk)
                            EDGExyBANKerod(n,m,k,2,1) = xint(2,kk)
                            EDGExyBANKerod(n,m,k,1,2) = yint(1,kk)
                            EDGExyBANKerod(n,m,k,2,2) = yint(2,kk)                                                                                            
                         endif     
                      endif             
                      !
                      ! define wet length
                      !            
                      if (.NOT.SEGisEXTRpoint(1).and..NOT.SEGisEXTRpoint(2)) THEN
                         !
                         ! they are both erodible, that means that the edge is completely dry
                         !
                         if (k==2) then
                            !
                            ! U-point
                            !
                            guu_cc(n,m) = 0._fp
                         else
                            !
                            ! (k==3) V-point
                            !
                            gvv_cc(n,m) = 0._fp
                         endif
                      else
                         kk   = 1
                         Len1 = sqrt( ( EDGExyBANK(n,m,k,1,1) - EDGExyBANK(n,m,k,2,1) )**2 + ( EDGExyBANK(n,m,k,1,2) - EDGExyBANK(n,m,k,2,2) )**2)
                         kk   = 2
                         Len2 = sqrt( ( EDGExyBANK(nADJ,mADJ,kADJ,1,1) - EDGExyBANK(nADJ,mADJ,kADJ,2,1) )**2 + &
                                    & ( EDGExyBANK(nADJ,mADJ,kADJ,1,2) - EDGExyBANK(nADJ,mADJ,kADJ,2,2) )**2)
                         if (k==2) then
                            !
                            ! U-point
                            !
                            guu_cc(n,m) = deta(n,m)  - max(Len1,Len2)
                         else
                            !
                            ! (k==3) V-point
                            !
                            gvv_cc(n,m) = dpsi(n,m)  - max(Len1,Len2)
                         endif
                      endif
                   elseif (typeINTER.eq.0) then
                      !
                      ! collinear but not intersecting (I erode both)
                      !
                      EDGEtypeBANKerod(K,n,m)            = 4
                      EDGEtypeBANKerod(kADJ,nADJ,mADJ)   = 4 
                      EDGExyBANKerod(n,m,k,1,1)          = EDGExyBANK(n,m,k,1,1)
                      EDGExyBANKerod(n,m,k,2,1)          = EDGExyBANK(n,m,k,2,1)
                      EDGExyBANKerod(n,m,k,1,2)          = EDGExyBANK(n,m,k,1,2)
                      EDGExyBANKerod(n,m,k,2,2)          = EDGExyBANK(n,m,k,2,2)
                      EDGExyBANKerod(nADJ,mADJ,kADJ,1,1) = EDGExyBANK(nADJ,mADJ,kADJ,1,1)
                      EDGExyBANKerod(nADJ,mADJ,kADJ,2,1) = EDGExyBANK(nADJ,mADJ,kADJ,2,1)
                      EDGExyBANKerod(nADJ,mADJ,kADJ,1,2) = EDGExyBANK(nADJ,mADJ,kADJ,1,2)
                      EDGExyBANKerod(nADJ,mADJ,kADJ,2,2) = EDGExyBANK(nADJ,mADJ,kADJ,2,2)
                      kk=1
                      Len1 = sqrt( ( EDGExyBANK(n,m,k,1,1) - EDGExyBANK(n,m,k,2,1) )**2 + ( EDGExyBANK(n,m,k,1,2) - EDGExyBANK(n,m,k,2,2) )**2)
                      kk=2
                      Len2 = sqrt( ( EDGExyBANK(nADJ,mADJ,kADJ,1,1) - EDGExyBANK(nADJ,mADJ,kADJ,2,1) )**2 + &
                                 & ( EDGExyBANK(nADJ,mADJ,kADJ,1,2) - EDGExyBANK(nADJ,mADJ,kADJ,2,2) )**2)
                      if (k==2) then
                         !
                         ! U-point
                         !
                         guu_cc(n,m) = dpsi(n,m) - (Len1 + Len2)
                      else
                         !
                         ! (k==3) V-point
                         ! 
                         gvv_cc(n,m) = deta(n,m) - (Len1 + Len2)
                      endif
                   else
                      write(*,*) 'error: dry edges are not parallel' ,m,n   
                      call d3stop(1, gdp)
                   endif
                else
                   !
                   ! all the other cases!!!     REMOVE THIS COMMENT: !if (EDGEtypeBANK(k,n,m).eq.1) then  
                   ! wet dry interface! n,m in the channel and the adjacent is full  bank (0) or partial bank (2)
                   !
                   EDGEtypeBANKerod(K,n,m) = EDGEtypeBANK(K,n,m)
                   EDGEtypeBANKerod(kADJ,nADJ,mADJ) = EDGEtypeBANK(kADJ,nADJ,mADJ)
                   EDGExyBANKerod(nADJ,mADJ,kADJ,1,1) = EDGExyBANK(nADJ,mADJ,kADJ,1,1)
                   EDGExyBANKerod(nADJ,mADJ,kADJ,2,1) = EDGExyBANK(nADJ,mADJ,kADJ,2,1)
                   EDGExyBANKerod(nADJ,mADJ,kADJ,1,2) = EDGExyBANK(nADJ,mADJ,kADJ,1,2)
                   EDGExyBANKerod(nADJ,mADJ,kADJ,2,2) = EDGExyBANK(nADJ,mADJ,kADJ,2,2)
                   EDGExyBANKerod(n,m,k,1,1) = EDGExyBANK(n,m,k,1,1)
                   EDGExyBANKerod(n,m,k,2,1) = EDGExyBANK(n,m,k,2,1)
                   EDGExyBANKerod(n,m,k,1,2) = EDGExyBANK(n,m,k,1,2)
                   EDGExyBANKerod(n,m,k,2,2) = EDGExyBANK(n,m,k,2,2)
                   if (k==2) then !U-point
                     guu_cc(n,m) = min(EDGElenWET(n,m,2),EDGElenWET(nadj,madj,4))
                   else !if(k==3) then  !V-point
                     gvv_cc(n,m) = min(EDGElenWET(n,m,3),EDGElenWET(nadj,madj,1))
                   endif
                !elseif (EDGEtypeBANK(k,n,m).eq.0) then  
                   ! 
                   ! wet dry interface! n,m is bank and the adjacent is in the channel (1) or can be (0) but nothing would happen
                   !
                   !EDGExyBANKerod(n,m,k,1,1) = EDGExyBANK(n,m,k,1,1)
                   !EDGExyBANKerod(n,m,k,2,1) = EDGExyBANK(n,m,k,2,1)
                   !EDGExyBANKerod(n,m,k,1,2) = EDGExyBANK(n,m,k,1,2)
                   !EDGExyBANKerod(n,m,k,2,2) = EDGExyBANK(n,m,k,2,2)
                   !  
                endif
             else  
                ! needed to have no flux in sud between kfc=0 and kfc=2 in the anular circ period case with 2 periodic layers
                if (k==2) then !U-point
                   guu_cc(n,m) = 0._fp
                else !if(k==3) then  !V-point
                   gvv_cc(n,m) = 0._fp
                endif
                !
                ! not sure these are needed.
                !
                EDGEtypeBANKerod(K,n,m) = EDGEtypeBANK(K,n,m)
                EDGEtypeBANKerod(kADJ,nADJ,mADJ) = EDGEtypeBANK(kADJ,nADJ,mADJ)
                EDGExyBANKerod(nADJ,mADJ,kADJ,1,1) = EDGExyBANK(nADJ,mADJ,kADJ,1,1)
                EDGExyBANKerod(nADJ,mADJ,kADJ,2,1) = EDGExyBANK(nADJ,mADJ,kADJ,2,1)
                EDGExyBANKerod(nADJ,mADJ,kADJ,1,2) = EDGExyBANK(nADJ,mADJ,kADJ,1,2)
                EDGExyBANKerod(nADJ,mADJ,kADJ,2,2) = EDGExyBANK(nADJ,mADJ,kADJ,2,2)
                EDGExyBANKerod(n,m,k,1,1) = EDGExyBANK(n,m,k,1,1)
                EDGExyBANKerod(n,m,k,2,1) = EDGExyBANK(n,m,k,2,1)
                EDGExyBANKerod(n,m,k,1,2) = EDGExyBANK(n,m,k,1,2)
                EDGExyBANKerod(n,m,k,2,2) = EDGExyBANK(n,m,k,2,2)
             endif
          enddo
       enddo
    enddo  
    !
    ! It is called here and also EDGExyBANKerod is copied at periodic Bound because for circular case cells of zero
    ! area were giving wrong values of EDGExyBANKerod from above. So everything is copied here
    !
    if (periodSURFACE) then
       if (periodGHOST) then
          call extrapPERIODICedgeBANK2(gdp)
       endif
    endif
    !
    if (nst.ge.idebugCUTini .and. nst.le.idebugCUTfin) THEN
       !write(2001) nCUTcell
       do m = 2, mmax-1
          do n = 2, nmaxus-1
             if (kfs_cc(n,m) == 0) then  
                !write(2001) Ndry_GRS(n,m) 
                !DO I = 1,Ndry_GRS(n,m) 
                !  write(2000) INTx_GRS(I,n,m),INTy_GRS(I,n,m)
                !ENDDO
                if(nst.eq.0) write(1001010,*)n,m,Ndry_GRS(n,m) 
            endif
         enddo
      enddo
      do n=2,nmaxus-1
         if (nst.eq.0) write(567890,'(<mmax-2>f25.15)')  (alphaD(n,m),m=2,mmax-1)
      enddo
    endif
    !
    !  compute adjusting factors agsqs,aguu and agvv to be used in the continuity equation
    !
    if (mod(NST,1000)==0) write(*,*) 'correct kfu/v in reconVOF for new active edges. Not sure they should be based on aguu'
    if (continuity_cc==0) then
       do m=1,mmax 
          do n=1,nmaxus 
             agsqs(n,m) = 1._fp
             aguu(n,m)  = 1._fp
             agvv(n,m)  = 1._fp
          enddo
       enddo
    else
       do m=1,mmax 
          do n=1,nmaxus 
             if (kfs_cc(n,m).eq.0) then
                !
                ! partially wetted cut cell
                !
                if (comparereal(poros(n,m),0.5_fp).le.0) then
                   !
                   ! it is a ghost point 
                   !
                   if (free_S1_sud.eq.1) then
                      !
                      ! water surface might be solved if
                      !
                      agsqs(n,m) = poros(n,m)
                   else
                      !
                      ! 1.0_fp
                      !
                      agsqs(n,m) = poros(n,m) 
                   endif
                else
                   !
                   ! it is an active water surface point (no ghost) that has to be solved
                   !
                   agsqs(n,m) = poros(n,m)                  
                endif
             elseif (kfs_cc(n,m).ge.1) then
                !
                ! wet cell
                !
                agsqs(n,m) = 1._fp
             elseif (kfs_cc(n,m).le.-1) then
                !
                ! dry cell
                ! I leave 1 since I do not want division by zero in sud 
                ! (the diagonal coefficient b that multiplies the water surface becomes zero!!)
                !
                agsqs(n,m) = 1._fp           
             endif
             !
             ! for m=1, n=all and similar guu its not defined and i dont want NaN
             !
             aguu(n,m)  = guu_cc(n,m)/max(guu(n,m),0.000000000000001_fp) 
             agvv(n,m)  = gvv_cc(n,m)/max(gvv(n,m),0.000000000000001_fp) ! for n=1, m=all and similar gvv its not defined and i dont want NaN
             if (aguu(n,m) < 1.e-15_fp) aguu(n,m) = 0.0_fp
             if (agvv(n,m) < 1.e-15_fp) agvv(n,m) = 0.0_fp
             !
             ! only first step: CORRECT discharges
             !
             if (zmodel) then
                write(*,*) 'Initial qxk and u1 not modified for zmodel and cutcells!!!'
                call d3stop(1,gdp)
             else               
                if (nst.eq.-100) then  
                   !
                   ! Correct dishcharges computed in chkdry initialization (only at first time step)
                   !
                   do k = 1, kmax
                      qxk(n,m,K) = aguu(n,m)*guu(n,m)*hu(n,m)*thick(k)*u1(n,m, k)
                      qyk(n,m,k) = agvv(n,m)*gvv(n,m)*hv(n,m)*thick(k)*v1(n,m, k)
                   enddo
                   !
                   ! if I start from ini file and I have non-zero velocity on banks, it keeps that value
                   !
                   if (comparereal(aguu(n,m),0._fp)==0) then
                      u1 (n,m,1:kmax) = 0._fp
                      qxk(n,m,1:kmax) = 0._fp
                      kfu(n,m) = 0
                      !
                      ! The following 2 statements can be removed, needed only cause caldpu_cc is not called at first time step
                      !
                      dpu(n,m) = -1000._fp 
                      hu(n,m)  = -1000._fp
                   endif
                   if (comparereal(agvv(n,m),0._fp)==0) then
                      v1 (n,m,1:kmax)= 0._fp
                      qyk(n,m,1:kmax) = 0._fp
                      kfv(n,m) = 0 
                      !
                      ! The following 2 statements can be removed, needed only cause caldpu_cc is not called at first time step
                      !
                      dpv(n,m) = -1000._fp
                      hv(n,m)  = -1000._fp
                   endif
                else !if(updatedBANK(n,m)) and if flooded, pensarci forse farlo sempre then
                   !REDEFINE qxk(n,m,k)
                   !PASSARE *porosu(nGP,mGP,k) 
                   !I have to move this to Findghostpoints.f90 since there I see which velocity points are dry or wet or ghost, and there I have to redefine hu, hv,u1 and v1 (and Umean Vmean) for fresh cells and then qxk qyk
                   !do k = 1, kmax !i have to redefine since aguu and agvv changed
                      !qxk(n,m,k) = aguu(n,m)*guu(n,m)*hu(n,m)*thick(k)*u1(n,m,k) !*porosu(n,m,k) 
                      !qyk(n,m,k) = agvv(n,m)*gvv(n,m)*hv(n,m)*thick(k)*v1(n,m,k) !*porosv(n,m,k)
                   !enddo 
                   !when eroding banks, some edges can go from fully bank to partial bank velocity points and can become activated.
                   !THIS COULD GIVE PROBLEM FOR DRY SAND NON VEGETATED CELLS, FOR WHICH I THINK AGUU IS 1, AND I DONT WANT 
                   ! TO KFU TO ACTIVE. MAYBE I SHOULD USE TYPE BANK IN THE 2 ADJACENT CELLS. !also problem if cut edge but dry! checku after uzd should  automatically 
                   !THIS IS NOT GOOODDDD SINCE IT ACTIVATES THEM ALSO WHEN THEY ARE BELOW THRESHOLD IN CHECKU. PROBABLY CHECKU SHOULD BE CALLED AFTER THIS IS  DONE.NOTE
                   ! ALSO THAT in sud a sort of checku is done using drytrsh. BUT ONLY IN THE ADI DIRECTION, IN THE OTHER KFV ARE
                   ! ALREADY OK SINCE HV STAYS THE OLD ONES. BUT IF I UPDATE THEM AFTER BANK EROSION I NEED TO DO A CHECK. IF FRESHHU IS USED I CAN SIMPLY USE
                   ! if (min(hu(NM), hucres)<=drytrsh) THEN MAYBE IGNORING hucres
                   !
                   if (comparereal(aguu(n,m),0._fp)>0.and.kcu(n,m)==1.and.hu(n,m)>drycrt) then !kcu  needed otherwise I activate also points that cannot be,  like edge betweeen two kcs==2 (and dpu is not computed there in caldpu)
                      kfu(n,m) = 1
                   endif
                   if (comparereal(agvv(n,m),0._fp)>0.and.kcv(n,m)==1.and.hv(n,m)>drycrt) then !kcv  needed otherwise I activate also points that cannot be, like edge betweeen two kcs==2 (and dpv is not computed there in caldpu)
                      kfv(n,m) = 1
                   endif
                endif
             endif
          enddo
       enddo
    endif
    !
    ! Make Ndry,Nwet,INTx_GRS,INTy_GRS,INTwx_GRS,INTwy_GRS periodic 
    !
    call periodic_dryWET_poly(gdp)
    !
    ! compute baricenters of upper and lower region
    !
    do m=1,mmax 
       do n=1,nmaxus 
          ! Note: XG_L and xG_R are still wrong in kcs=2 especially if circular testcase with zero area, 
          ! in the sense that they are not periodic but they are computed using the wet and dry polygon in the zero area cell
          ! I have to make Ndry,Nwet,INTx_GRS,INTy_GRS,INTwx_GRS,INTwy_GRS periodic before this double for cycle to fix that. 
          ! But  XG_L and xG_R kcs=2 should never be used
          if (comparereal(poros(n,m),0._fp)>0.and.comparereal(poros(n,m),1._fp)<0.and.kcs(n,m)>=1) then
             !
             ! if kcs==0 I could have poros/=0 cause of BC_VOF condition
             !
             if (.not.(analyticalPOLY.and.precisePOROSbaric)) then
                !
                ! if analyticalPOLY xG_L and yG_L are computed below
                !
                Ndry          = Ndry_GRS(n,m)  
                Ndryp1        = Ndry+1
                polyx(1:Ndry) = INTx_GRS(1:Ndry,n,m) ; polyx(Ndryp1) = INTx_GRS(1,n,m)
                polyy(1:Ndry) = INTy_GRS(1:Ndry,n,m) ; polyy(Ndryp1) = INTy_GRS(1,n,m)
                !
                CALL A_G_Poly(polyx,polyy,Ndryp1,areaDRY,xG_H(n,m),yG_H(n,m),2,lunscr,gdp) 
                !
                ! old version using the complementar area and G
                ! this gives rounding error for small cut cells
                !
                !xG_L(n,m) = (xg(n,m)*gsqs(n,m) - xG_H(n,m)*areaDRY)/(gsqs(n,m)-areaDRY) 
                !yG_L(n,m) = (yg(n,m)*gsqs(n,m) - yG_H(n,m)*areaDRY)/(gsqs(n,m)-areaDRY)
                !write(982132,'(3i9,3f25.15)') m,n,(m-1)*83+n,xG_L(n,m),yG_L(n,m),1._fp
                !
                ! new version using wet polygon
                !
                Nwet          = Nwet_GRS(n,m)  
                Nwetp1        = Nwet+1
                polyx(1:Nwet) = INTwx_GRS(1:Nwet,n,m) ; polyx(Nwetp1) = INTwx_GRS(1,n,m)
                polyy(1:Nwet) = INTwy_GRS(1:Nwet,n,m) ; polyy(Nwetp1) = INTwy_GRS(1,n,m)
                !
                CALL A_G_Poly(polyx,polyy,Nwetp1,DUMMYr,xG_L(n,m),yG_L(n,m),2,lunscr, gdp) 
                !write(982131,'(3i9,3f25.15)') m,n,(m-1)*83+n,xG_L(n,m),yG_L(n,m),1._fp
             endif
          else
             xG_L(n,m) = xg(n,m)
             yG_L(n,m) = yg(n,m)
             xG_H(n,m) = xg(n,m)
             yG_H(n,m) = yg(n,m)
          endif
          !if (comparereal(agsqs(n,m),1._fp).lt.0) then
             !   
             ! used only for curvature of elical flow for now, I do it for all cells, so it works also for rotated cartesian grid or curvilinear
             !
          !PP(:) = (/xG_L(n,m),yG_L(n,m)/)
          ! dxk(n,m,1) = min_dist( (/xcor0(n-1,m-1), ycor0(n-1,m-1)/) , (/xcor0(n-1,m  ), ycor0(n-1,m  )/) , PP,1)
          !dxk(n,m,2) = min_dist( (/xcor0(n-1,m  ), ycor0(n-1,m  )/) , (/xcor0(n  ,m  ), ycor0(n  ,m  )/) , PP,1)
          !dxk(n,m,3) = min_dist( (/xcor0(n  ,m  ), ycor0(n  ,m  )/) , (/xcor0(n  ,m-1), ycor0(n  ,m-1)/) , PP,1)
          !dxk(n,m,4) = min_dist( (/xcor0(n  ,m-1), ycor0(n  ,m-1)/) , (/xcor0(n-1,m-1), ycor0(n-1,m-1)/) , PP,1)
          do k = 1, 4
             if(EDGEtypeBANK(k,n,m)==0) then
                !
                ! cut edge partially wet (this part its used for secondary flow so only 2D so it should be fine
                !
                ! first point of the bank on the edge (first point of EDGExyBANK is always the one inside the edge and not on the  vertex)
                !
                xB1 = EDGExyBANK(n,m,k,1,1) 
                yB1 = EDGExyBANK(n,m,k,1,2)
                !
                ! second point of the bank on the edge (second point of EDGExyBANK is always on the vertex)
                !
                xB2 = EDGExyBANK(n,m,k,2,1)
                yB2 = EDGExyBANK(n,m,k,2,2)  
                if ( (comparereal(xB2,xcell(k,n,m)).eq.0) .and. (comparereal(yB2,ycell(k,n,m)).eq.0) ) then
                   !
                   ! xcell(k,n,m),ycell(k,n,m) is on the bank, so k+1 is on the water
                   ! Water vertex
                   !
                   xW2 = xcell(k+1,n,m) 
                   yW2 = ycell(k+1,n,m)
                else
                   !
                   ! xcell(k,n,m),ycell(k,n,m) is on the water
                   ! Water vertex
                   !
                   xW2 = xcell(k,n,m) 
                   yW2 = ycell(k,n,m)
                endif
                !
                ! coord of center of active water edge
                !
                xxx(k) = (xB1+xW2)*0.5_fp
                yyy(k) = (yB1+yW2)*0.5_fp
             else
                !
                ! coord of center of water edge
                !
                xxx(k) = (xcell(k,n,m)+xcell(k+1,n,m))*0.5_fp 
                yyy(k) = (ycell(k,n,m)+ycell(k+1,n,m))*0.5_fp
             endif
             !
             ! x-distance baricenter to center of edge
             !
             dxk(n,m,k) = xxx(k)-xG_L(n,m)
             dyk(n,m,k) = yyy(k)-yG_L(n,m)
          enddo
       enddo
    enddo    
    !
    ! update newGHOSTu, newGHOSTv
    !   
    do m=1,mmax
       do n=1,nmaxus
          !
          ! NOT TRUE: new ghost can appear couse of bank erosion when a ghost point becames water 
          ! and the adjacent one became ghost (while it was nothing before)
          ! new ghost points are created only by deposition 
          ! (check for zmodel if its the case, new ghost points in the vertical can occur also when water surface moves vertical)
          if (comparereal(aguu(n,m),0._fp) ==0 .and. aguuOLDnotZERO(n,m)) then
             newGHOSTu(n,m) = .true.
          else
             newGHOSTu(n,m) = .false.
          endif
          if (comparereal(agvv(n,m),0._fp) ==0 .and. agvvOLDnotZERO(n,m)) then
             newGHOSTv(n,m) = .true.
          else
             newGHOSTv(n,m) = .false.
          endif
       enddo
    enddo
    !
    ! compute oneEXIT
    !
    do n=2,nmaxus-1   !RESHAPE_CYCLE 1,nmax
       do m=2,mmax-1  !RESHAPE_CYCLE 1,mmax
          oneEXIT(n,m) = .false.
          if (comparereal(agsqs(n,m),0._fp).gt.0) then
             cont = 0
             if (comparereal(agvv(n,m),0._fp)==0) then
                cont = cont + 1
             endif
             if (comparereal(agvv(n-1,m),0._fp)==0) then
                cont = cont + 1
             endif
             if (comparereal(aguu(n,m),0._fp)==0) then
                cont = cont + 1
             endif
             if (comparereal(aguu(n,m-1),0._fp)==0) then
                cont = cont + 1
             endif
             if (cont==3) then
                oneEXIT(n,m) = .true.
             endif
          endif
       enddo
    enddo
    !
    ! define mask for bank erosion (it smooths the bank by ignoring velocity where edge is the only exit edge)
    !
    multEXITu(:,:) = 1
    multEXITv(:,:) = 1
    do n=2,nmaxus-1   !RESHAPE_CYCLE 1,nmax
       do m=2,mmax-1  !RESHAPE_CYCLE 1,mmax
          if(oneEXIT(n,m)) then  
             multEXITu(n,m) = 0 
             multEXITu(n,m-1) = 0 
             multEXITv(n,m) = 0 
             multEXITv(n-1,m) = 0 
          endif
       enddo
    enddo
    !
9999 continue
    !    
    !
    ! overwrite porosity and xG_L and yG_L by intersection of a circle made of many segments and the cell.
    !
    ! moved here from intCELLS. I first compute the right edges sizes here in reconvof (using a vof reconstruction with
    ! the normal and the porosity of the linear interpolation in order to get back a continuous polygon with exact cut 
    ! edges. Now the porosity is not the analytical one and does not take in account of the curvature of the circle
    ! so here I recompute it.At this point only Nx and Ny are not analytical since they are the normal to the linear exact
    ! polygon. But i thing they are never used if I owerwrite the Nx_U1,Ny_U1 and Nx_V1,Ny_V1 with the exact ones
    !
    if (analyticalPOLY==1 .and. precisePOROSbaric .and. firstCALL) then 
       allocate(POLYintersX     (NanglesANALcircle_FIXED,1))
       allocate(POLYintersY     (NanglesANALcircle_FIXED,1))
       allocate(POLYStoBEjoinedX(NanglesANALcircle_FIXED,1))
       allocate(POLYStoBEjoinedY(NanglesANALcircle_FIXED,1))
       allocate(POLYSunionX     (NanglesANALcircle_FIXED,1))
       allocate(POLYSunionY     (NanglesANALcircle_FIXED,1))
       allocate(Xcirc           (NanglesANALcircle_FIXED,1))
       allocate(Ycirc           (NanglesANALcircle_FIXED,1))
       Radius(1) = 70
       Radius(2) = 50
       midRADIUS = (Radius(1)+Radius(2))*0.5_FP
       select case(VERSIONprecisePOROSbaric)
       case(1)
          if (NanglesANALcircle>NanglesANALcircle_FIXED) then
             write(*,*) 'error in intCELLS NanglesANALcircle>NanglesANALcircle_FIXED'
             call d3stop(1,gdp)
          endif
          !
          ! define polygon approximating circle
          !
          Dteta = 2._fp*pi/NanglesANALcircle
          do j=2,1,-1
             Xcirc(1,1) = 0._fp    
             Ycirc(1,1) = Radius(j)   
             do i=2,NanglesANALcircle +1
                !
                ! Anticlockwise
                !
                teta       = -Dteta*real(i-1,fp) 
                Xcirc(i,1) = sin(teta)*Radius(j)
                Ycirc(i,1) = cos(teta)*Radius(j)
             enddo
             VERTtoBEjoined = NanglesANALcircle + 1
             if (j==2) then
                !
                ! add points in a way to consider complementary area
                !
                VERTtoBEjoined          = VERTtoBEjoined + 1
                Xcirc(VERTtoBEjoined,1) = 0._fp
                Ycirc(VERTtoBEjoined,1) = 100._fp
                VERTtoBEjoined          = VERTtoBEjoined + 1
                Xcirc(VERTtoBEjoined,1) =  100._fp
                Ycirc(VERTtoBEjoined,1) =  100._fp
                VERTtoBEjoined          = VERTtoBEjoined + 1
                Xcirc(VERTtoBEjoined,1) =  100._fp
                Ycirc(VERTtoBEjoined,1) = -100._fp
                VERTtoBEjoined          = VERTtoBEjoined + 1
                Xcirc(VERTtoBEjoined,1) = -100._fp
                Ycirc(VERTtoBEjoined,1) = -100._fp
                VERTtoBEjoined          = VERTtoBEjoined + 1
                Xcirc(VERTtoBEjoined,1) = -100._fp
                Ycirc(VERTtoBEjoined,1) = +100._fp
                VERTtoBEjoined          = VERTtoBEjoined + 1
                Xcirc(VERTtoBEjoined,1) = 0._fp
                Ycirc(VERTtoBEjoined,1) = 100._fp
             endif
             !
             ! intersect it with the cells
             !
             do m=1,mmax
                do n=1,nmaxus
                   if (comparereal(poros(n,m),0._fp)>0.and.comparereal(poros(n,m),1._fp)<0) then
                      !
                      ! x- and y-coordinates of the 4 corners of cell (n,m) in counter-clockwise direction
                      !
                      xclip(1:4) = xcell(:,n,m) 
                      yclip(1:4) = ycell(:,n,m)        
                      absMAXx    = maxval( (/ abs(xcor0(n-1,m-1)), abs(xcor0(n-1,m)), abs(xcor0(n,m)), abs(xcor0(n,m-1)) /) ) 
                      absMAXy    = maxval( (/ abs(ycor0(n-1,m-1)), abs(ycor0(n-1,m)), abs(ycor0(n,m)), abs(ycor0(n,m-1)) /) )
                      if (j==2.and.sqrt(absMAXx(1)**2+absMAXy(1)**2)>midRADIUS) then
                         !
                         ! these cut cells belong the other circle
                         !
                         cycle
                      endif
                      if (j==1.and.sqrt(absMAXx(1)**2+absMAXy(1)**2)<midRADIUS) then
                         cycle
                      endif
                      NPOLYStoBEjoined = 1
                      call wrapUNI_intCEL(4, xclip, yclip, &
                                             Xcirc, Ycirc, NPOLYStoBEjoined, VERTtoBEjoined, &             !polygons to join (union)
                                             POLYSunionX , POLYSunionY     , NPOLYSunion   , VERTunion , & !union of polygons 
                                             POLYintersX , POLYintersY     , NPOLYSinters  , VERTinters, & !intersection of union with dry cell
                                             absMAXx     , absMAXy         )  
                      !
                      ! Compute area and baricenter
                      !
                      if (NPOLYSinters>0) then
                         !
                         ! if not it intersects the other circle
                         ! add last point conciding with first
                         !
                         VERTinters(1)                = VERTinters(1)+1
                         POLYintersX(VERTinters(1),1) = POLYintersX(1,1)
                         POLYintersY(VERTinters(1),1) = POLYintersY(1,1)
                         !
                         CALL A_G_Poly(POLYintersX,POLYintersY,VERTinters(1),AREA,xG_L(n,m),yG_L(n,m),2,lunscr,gdp) 
                         area = area/gsqs(n,m) 
                         !if (j==1) then
                            write(9999991,'(3i8,15f25.15)') n,m,j,poros(n,m),area,poros(n,m)-area
                            poros(n,m) = area
                         !else
                         !   write(9999991,'(3i8,15f25.15)') n,m,j,poros(n,m),1._fp-area,poros(n,m)-(1._fp-area)
                         !   poros(n,m) = 1._fp-area
                         !endif
                      endif
                      !if(m.eq.4.and.n.eq.20.and.j==1) then      !.and.nst.eq.75
                      if(m.eq.9.and.n.eq.18.and.j==2) then
                         open(78,file='solution.txt',form = 'formatted',status='replace')
                         write(78,*)NPOLYSinters
                         do k = 1,NPOLYSinters
                            write(78,*)VERTinters(K)
                            do kk = 1,VERTinters(K)
                               write(78,*)POLYintersX(kk,k),POLYintersY(kk,k)
                            enddo
                         enddo
                         close(78)
                         open(78,file='subj.txt',form = 'formatted',status='replace')
                         write(78,*)NPOLYStoBEjoined
                         do k = 1,NPOLYStoBEjoined
                            write(78,*)VERTtoBEjoined(K)
                            do kk = 1,VERTtoBEjoined(K)
                               write(78,*)POLYStoBEjoinedX(kk,k),POLYStoBEjoinedY(kk,k)
                            enddo
                         enddo
                         close(78)
                         open(78,file='CLIP.txt',form = 'formatted',status='replace')
                         write(78,*)1
                         do k = 1,1
                            write(78,*)4
                            do kk = 1,4
                               write(78,*)xclip(kk),yclip(kk)
                            enddo
                         enddo
                         close(78)
                         open(78,file='union.txt',form = 'formatted',status='replace')
                         write(78,*)NPOLYSunion
                         do k = 1,NPOLYSunion
                            write(78,*)VERTunion(K)
                            do kk = 1,VERTunion(K)
                               write(78,*)POLYSunionX(kk,k),POLYSunionY(kk,k)
                            enddo
                         enddo
                         close(78)
                         !write(55554444,'(2i8,10f25.15)')nst,NPOLYSinters,AREAeros,gsqs(n,m), poros(n,m),AREAeros/gsqs(n,m) 
                      endif
                   endif
                enddo
             enddo
          enddo
       case(2)
          !
          ! Much faster and more accurate (only section of circle local to the cell)
          !
          do m=1,mmax
             do n=1,nmaxus
                if (comparereal(poros(n,m),0._fp)>0.and.comparereal(poros(n,m),1._fp)<0) then
                   !
                   ! x- and y-coordinates of the 4 corners of cell (n,m) in counter-clockwise direction
                   !
                   xclip(1:4) = xcell(:,n,m) 
                   yclip(1:4) = ycell(:,n,m)  
                   do K=1,4
                       !
                       ! the discharge is prescribed North, so this gives the angle clockwise between 0 and 2*pi starting from the y axis
                       !
                       angANTIclock(k) = modulo(1._fp/2._fp*pi-atan2(yclip(k),xclip(k)),2._fp*pi)
                   enddo
                   if (maxANG > pi) then 
                      !
                      ! check if node intersect y axis (0 becomes 2*pi)
                      !
                      zeroNODE = .FALSE.
                      do k =1,4
                         if (comparereal(angANTIclock(k),0._fp)==0) then
                            !
                            ! the y axis can be 0 or 2*pi depending if I am on the left or the right of it
                            !
                            !minANG = maxANG
                            !maxANG = 2._fp*pi
                            zeroNODE = .TRUE.
                            exit
                         endif
                      enddo
                   endif
                   if (zeroNODE) then
                      !
                      ! 0 becomes 2*pi
                      !
                      do k =1,4
                         if (comparereal(angANTIclock(k),0._fp)==0) then
                            !
                            ! the y axis can be 0 or 2*pi depending if I am on the left or the right of it
                            !
                            angANTIclock(k) = 2._fp*pi
                         endif
                      enddo
                   endif
                   maxANG  = maxval(angANTIclock(:))
                   minANG  = minval(angANTIclock(:))
                   ang     = abs(maxANG-minANG) !abs not needed
                   Dteta   = ang/NanglesANALcircle
                   absMAXx = maxval( (/ abs(xcor0(n-1,m-1)), abs(xcor0(n-1,m)), abs(xcor0(n,m)), abs(xcor0(n,m-1)) /) ) 
                   absMAXy = maxval( (/ abs(ycor0(n-1,m-1)), abs(ycor0(n-1,m)), abs(ycor0(n,m)), abs(ycor0(n,m-1)) /) )
                   if (sqrt(absMAXx(1)**2+absMAXy(1)**2)>midRADIUS) then
                      !
                      ! these cut cells belong the outer circle
                      !
                      outerCIRCLE = 1
                      radCIRCLE   = Radius(1)
                   else
                      outerCIRCLE = 0 
                      radCIRCLE   = Radius(2)
                   endif
                   !
                   ! define polygon                   
                   !
                   Xcirc(1,1) = sin(maxANG)*radCIRCLE
                   Ycirc(1,1) = cos(maxANG)*radCIRCLE
                   do i=2,NanglesANALcircle +1
                      !
                      ! Anticlockwise
                      !
                      teta       = maxANG -Dteta*real(i-1,fp)
                      Xcirc(i,1) = sin(teta)*radCIRCLE
                      Ycirc(i,1) = cos(teta)*radCIRCLE
                   enddo
                   VERTtoBEjoined = NanglesANALcircle + 1
                   if (outerCIRCLE == 1) then
                      !
                      ! add points in a way that polygon goes toward the centre of the circle
                      !
                      VERTtoBEjoined          = VERTtoBEjoined + 1
                      Xcirc(VERTtoBEjoined,1) = sin(minANG)*0.5_fp*radCIRCLE
                      Ycirc(VERTtoBEjoined,1) = cos(minANG)*0.5_fp*radCIRCLE
                      VERTtoBEjoined          = VERTtoBEjoined + 1
                      Xcirc(VERTtoBEjoined,1) = sin(maxANG)*0.5_fp*radCIRCLE
                      Ycirc(VERTtoBEjoined,1) = cos(maxANG)*0.5_fp*radCIRCLE
                   else
                      !
                      ! add points in a way that polygon goes far away from the centre of the circle
                      !
                      VERTtoBEjoined          = VERTtoBEjoined + 1
                      Xcirc(VERTtoBEjoined,1) = sin(minANG)*2._fp*radCIRCLE
                      Ycirc(VERTtoBEjoined,1) = cos(minANG)*2._fp*radCIRCLE
                      VERTtoBEjoined          = VERTtoBEjoined + 1
                      Xcirc(VERTtoBEjoined,1) = sin(maxANG)*2._fp*radCIRCLE
                      Ycirc(VERTtoBEjoined,1) = cos(maxANG)*2._fp*radCIRCLE
                   endif                      
                   NPOLYStoBEjoined = 1
                   call wrapUNI_intCEL(4,xclip      , yclip      , &
                                         Xcirc      , Ycirc      , NPOLYStoBEjoined, VERTtoBEjoined, &  ! polygons to join (union)
                                         POLYSunionX, POLYSunionY, NPOLYSunion     , VERTunion     , &  ! union of polygons 
                                         POLYintersX, POLYintersY, NPOLYSinters    , VERTinters    , &  ! intersection of union with dry cell
                                         absMAXx    , absMAXy    )  
                   !
                   ! Compute area and baricenter   
                   !
                   if (NPOLYSinters>0) then
                      !
                      ! if not it intersects the other circle
                      ! add last point conciding with first                      
                      !
                      VERTinters(1)                = VERTinters(1)+1
                      POLYintersX(VERTinters(1),1) = POLYintersX(1,1)
                      POLYintersY(VERTinters(1),1) = POLYintersY(1,1)
                      !
                      call A_G_Poly(POLYintersX,POLYintersY,VERTinters(1),AREA,xG_L(n,m),yG_L(n,m),2,lunscr,gdp) 
                      !
                      area = area/gsqs(n,m) 
                      !if (j==1) then
                         write(9999991,'(3i8,15f25.15)') n,m,j,poros(n,m),area,poros(n,m)-area
                         poros(n,m) = area
                      !else
                      !   write(9999991,'(3i8,15f25.15)') n,m,j,poros(n,m),1._fp-area,poros(n,m)-(1._fp-area)
                      !   poros(n,m) = 1._fp-area
                      !endif
                   endif
                   !if(m.eq.4.and.n.eq.20.and.j==1) then      !.and.nst.eq.75
                   if(m.eq.17.and.n.eq.41) then
                      open(78,file='solution.txt',form = 'formatted',status='replace')
                      write(78,*)NPOLYSinters
                      do k = 1,NPOLYSinters
                         write(78,*)VERTinters(K)
                         do kk = 1,VERTinters(K)
                            write(78,*)POLYintersX(kk,k),POLYintersY(kk,k)
                         enddo
                      enddo
                      close(78)
                      open(78,file='subj.txt',form = 'formatted',status='replace')
                      write(78,*)NPOLYStoBEjoined
                      do k = 1,NPOLYStoBEjoined
                         write(78,*)VERTtoBEjoined(K)
                         do kk = 1,VERTtoBEjoined(K)
                            write(78,*)POLYStoBEjoinedX(kk,k),POLYStoBEjoinedY(kk,k)
                         enddo
                      enddo
                      close(78)
                      open(78,file='CLIP.txt',form = 'formatted',status='replace')
                      write(78,*)1
                      do k = 1,1
                         write(78,*)4
                         do kk = 1,4
                            write(78,*)xclip(kk),yclip(kk)
                         enddo
                      enddo
                      close(78)
                      open(78,file='union.txt',form = 'formatted',status='replace')
                      write(78,*)NPOLYSunion
                      do k = 1,NPOLYSunion
                         write(78,*)VERTunion(K)
                         do kk = 1,VERTunion(K)
                            write(78,*)POLYSunionX(kk,k),POLYSunionY(kk,k)
                         enddo
                      enddo
                      close(78)
                      !write(55554444,'(2i8,10f25.15)')nst,NPOLYSinters,AREAeros,gsqs(n,m), poros(n,m),AREAeros/gsqs(n,m) 
                   endif
                endif
             enddo
          enddo
          !
       CASE(3)
          !
          ! read from file
          !
          write(NAMEfileXGYG,'(a,i0,a)') 'xGyG_',NanglesANALcircle,'.txt'
          open(100,file = NAMEfileXGYG)
          do m=1,mmax 
             do n=1,nmaxus 
                read(100,*) intBUTTA1,intBUTTA2,xG_L(n,m),yG_L(n,m),poros(n,m)
             enddo
          enddo 
          close(100)
       end select
       !
       if (VERSIONprecisePOROSbaric/=3) then
          deallocate(POLYintersX,POLYintersY,POLYStoBEjoinedX,POLYStoBEjoinedY,POLYSunionX,POLYSunionY)
       endif
       !
       ! recompute agsqs since they were computed with wrong poros
       !
       do m=1,mmax 
          do n=1,nmaxus 
             if (kfs_cc(n,m).eq.0) then
                !
                ! partially wetted cut cell
                !
                if (comparereal(poros(n,m),0.5_fp).le.0) then
                   !
                   ! it is a ghost point
                   !
                   if (free_S1_sud.eq.1) then
                      !
                      ! water surface might be solved if
                      !
                      agsqs(n,m) = poros(n,m)
                   else
                      !
                      ! 1._fp 
                      !
                      agsqs(n,m) = poros(n,m)
                   endif
                else
                   !
                   ! it is an active water surface point (no ghost) that has to be solved
                   !
                   agsqs(n,m) = poros(n,m)                  
                endif
             elseif (kfs_cc(n,m).ge.1) then
                !
                ! wet cell
                ! 
                agsqs(n,m) = 1._fp
             elseif (kfs_cc(n,m).le.-1) then
                !
                ! dry cell
                ! I leave 1 since I do not want division by zero in sud 
                ! (the diagonal coefficient b that multiplies the water surface becomes zero)
                !
                agsqs(n,m) = 1._fp
             endif
          enddo
       enddo
    endif
    if (analyticalPOLY==1.and.precisePOROSbaric) then 
       !
       ! they are not computed in kcs==2 above so they are made periodic here
       !
       call periodic_xGLyGL(xG_L,yG_L,nlb,nub,mlb,mub,kmax, gdp) 
    endif
    if (VERSIONprecisePOROSbaric/=3.and.nst<0) THEN
       write(NAMEfileXGYG,'(a,i0,a)') 'xGyG_',NanglesANALcircle,'.txt'
       open(100,file = NAMEfileXGYG)
       do m=1,mmax 
          do n=1,nmaxus 
             write(100,'(2i8,25f25.15)') n,m,xG_L(n,m),yG_L(n,m),poros(n,m)
          enddo
       enddo 
       close(100)
    endif
    !
    firstCALL = .false.
    !
end subroutine reconVOF
