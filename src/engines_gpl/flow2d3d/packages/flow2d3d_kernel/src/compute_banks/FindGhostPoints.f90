subroutine FindGhostPoints(gsqs,kfs,kfu,kfv,kcs,kcu,kcv,s1,u1,v1,dps,lunscr,Irov,mmax,nmax,nmaxus,kmax,nst,nlb,nub,mlb,mub,nmlb,nmub,Zmodel,gdp)
!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011.                                     
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
!  Ndryact: delft3d.support@deltares.nl                                         
!  Stichting Deltares                                                           
!  P.O. Box 177                                                                 
!  2600 MH Delft, The Netherlands                                               
!                                                                               
!  All indications and logos of, and references to, "Delft3D" and "Deltares"    
!  are registered trademarks of Stichting Deltares, and remain the property of  
!  Stichting Deltares. All rights reserved.                                     
!                                                                               
!-------------------------------------------------------------------------------
!  $Id: My_intersec.f90 
!  $HeadURL:
!!--description-----------------------------------------------------------------
!
!   Function: Find which velocity (x and y) and level point is solid or liquid.
!             And find ghost points needed to apply Mittal's immersed boundary algorithm 
!             with ghost points. It is first checked if the vel o surface point is dry, 
!             if it is it is checked if it is a ghost point but looking for a wet adjacent
!             with a specific type of edges.
!
!             mGPu1,nGPu1: refers to the location (m,n) of the ghost points on
!                          the U1 grid (barycenter of cell (m,n) is xG_U1(n,m),yG_U1(n,m) 
!                          and has value u1(n,m).
!             mBIu1,nBIu1: refers to the location (m,n) of the boundary intersection
!                          in the S1 grid (barycenter of cell (m,n) is xG_U1(n,m),yG_U1(n,m) 
!                          and has value u1(n,m).
!             mIPu1,nIPu1: refers to the location (m,n) of the boundary intersection
!                          in the V1 grid (barycenter of cell (m,n) is xG_V1(n,m),yG_V1(n,m) 
!                          and has value V1(n,m).
!               
!!--declarations----------------------------------------------------------------
!
    use globaldata
    use mathconsts, only: pi
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    integer                       , pointer :: free_S1_sud
    integer                       , pointer :: kFLcutEQ1
    integer                       , pointer :: totGHOSTu1
    integer                       , pointer :: totGHOSTv1
    integer                       , pointer :: totGHOSTs1
    integer                       , pointer :: typeEXTRAPstencil
    integer, dimension(:)         , pointer :: edge6
    integer, dimension(:,:)       , pointer :: FREEs1_u
    integer, dimension(:,:)       , pointer :: FREEs1_v
    integer, dimension(:,:)       , pointer :: kfs_cc
    integer, dimension(:,:)       , pointer :: Ndry_GRS
    integer, dimension(:,:,:)     , pointer :: EDGEtypeBANK
    integer, dimension(:,:,:)     , pointer :: EDGEtypeBANKerod
    integer, dimension(:,:)       , pointer :: U1inDRY
    integer, dimension(:,:)       , pointer :: V1inDRY
    integer, dimension(:,:)       , pointer :: FROMmnTOghostS1
    integer, dimension(:,:)       , pointer :: FROMmnTOghostU1
    integer, dimension(:,:)       , pointer :: FROMmnTOghostV1
    integer, dimension(:)         , pointer :: nGPs1
    integer, dimension(:)         , pointer :: mGPs1
    integer, dimension(:)         , pointer :: nGPu1
    integer, dimension(:)         , pointer :: mGPu1
    integer, dimension(:)         , pointer :: nGPv1
    integer, dimension(:)         , pointer :: mGPv1
    integer, dimension(:)         , pointer :: mIPu1
    integer, dimension(:)         , pointer :: nIPu1
    integer, dimension(:)         , pointer :: mBIu1
    integer, dimension(:)         , pointer :: nBIu1
    integer, dimension(:,:)       , pointer :: GHOSTs1
    integer, dimension(:,:)       , pointer :: GHOSTu1
    integer, dimension(:,:)       , pointer :: GHOSTv1
    integer, dimension(:,:)       , pointer :: kFLcut
    integer, dimension(:,:)       , pointer :: SURFinDRY
    real(fp), dimension(:,:)      , pointer :: POROS
    real(fp), dimension(:,:,:)    , pointer :: INTx_GRS
    real(fp), dimension(:,:,:)    , pointer :: INTy_GRS
    real(fp), dimension(:,:,:,:,:), pointer :: EDGExyBANK
    real(fp), dimension(:,:)      , pointer :: aguu
    real(fp), dimension(:,:)      , pointer :: agvv
    real(fp), dimension(:,:)      , pointer :: xG
    real(fp), dimension(:,:)      , pointer :: yG
    real(fp), dimension(:,:)      , pointer :: Nx
    real(fp), dimension(:,:)      , pointer :: Ny
    real(fp), dimension(:,:)      , pointer :: xG_V1
    real(fp), dimension(:,:)      , pointer :: xG_U1
    real(fp), dimension(:,:)      , pointer :: yG_V1
    real(fp), dimension(:,:)      , pointer :: yG_U1
    logical                       , pointer :: noFLOODINGbanks
    logical                       , pointer :: activeNEVERghost
!
! global variables
!
    real(fp), dimension(nlb:nub,mlb:mub)                                , intent(inout) :: s1 
    real(fp), dimension(nlb:nub,mlb:mub, kmax)                          , intent(inout) :: u1
    real(fp), dimension(nlb:nub,mlb:mub, kmax)                          , intent(inout) :: v1
    real(fp), dimension(nlb:nub,mlb:mub)                                , intent(inout) :: gsqs
    real(prec), dimension(nlb:nub,mlb:mub)                                , intent(in)    :: dps
    integer, dimension(nlb:nub,mlb:mub)                                 , intent(in)    :: kfs 
    integer, dimension(nlb:nub,mlb:mub)                                 , intent(in)    :: kfu 
    integer, dimension(nlb:nub,mlb:mub)                                 , intent(in)    :: kfv
    integer, dimension(nlb:nub,mlb:mub)                                 , intent(in)    :: kcs 
    integer, dimension(nlb:nub,mlb:mub)                                 , intent(in)    :: kcu 
    integer, dimension(nlb:nub,mlb:mub)                                 , intent(in)    :: kcv 
    integer                                                             , intent(in)    :: mmax   !  Description and declaration in iidim.f90
    integer                                                             , intent(in)    :: nmax   !  Description and declaration in iidim.f90
    integer                                                             , intent(in)    :: nmaxus   !  Description and declaration in iidim.f90
    integer                                                             , intent(in)    :: kmax   !  Description and declaration in iidim.f90
    integer                                                             , intent(in)    :: lunscr   !  Description and declaration in iidim.f90
    integer                                                             , intent(in)    :: irov   !  Description and declaration in iidim.f90
    integer                                                             , intent(in)    :: nlb
    integer                                                             , intent(in)    :: nub
    integer                                                             , intent(in)    :: mlb
    integer                                                             , intent(in)    :: mub
    integer                                                             , intent(in)    :: nmlb
    integer                                                             , intent(in)    :: nmub
    integer                                                             , intent(in)    :: nst
    logical                                                             , intent(in)    :: Zmodel
!
!
! local variables
!
  integer                    :: I 
  integer                    :: nL(4)
  integer                    :: mL(4)
  integer                    :: kL(4)
  integer                    :: kADJ 
  integer                    :: nADJ 
  integer                    :: mADJ
  integer                    :: kk
  integer                    :: n
  integer                    :: m
  integer                    :: nAD
  integer                    :: mAD
  integer                    :: k
  integer                    :: cont
  integer                    :: contWDint
  integer                    :: vectADJ(1:4)
  integer                    :: contAD
  integer                    :: nd
  integer                    :: md
!
  logical                    :: vectADJ_L(1:4)
  logical                    :: DRYU
  logical                    :: DRYUadj
  logical                    :: FOUNDwet
  logical                    :: ZmodR,ZmodL
  logical                    :: zmod_or_noFLOOD
!
  real(fp)                   :: dxADJ
  real(fp)                   :: dyADJ
  real(fp)                   :: dx
  real(fp)                   :: dy
  real(fp)                   :: angleADJ
  real(fp)                   :: angle
!
! executable statements -------------------------------------------------------
!
    free_S1_sud       => gdp%gdimbound%free_S1_sud
    kFLcutEQ1         => gdp%gdimbound%kFLcutEQ1
    totGHOSTu1        => gdp%gdimbound%totGHOSTu1
    totGHOSTv1        => gdp%gdimbound%totGHOSTv1
    totGHOSTs1        => gdp%gdimbound%totGHOSTs1
    typeEXTRAPstencil => gdp%gdimbound%typeEXTRAPstencil
    edge6             => gdp%gdimbound%edge6
    FREEs1_u          => gdp%gdimbound%FREEs1_u
    FREEs1_v          => gdp%gdimbound%FREEs1_v
    kfs_cc            => gdp%gdimbound%kfs_cc
    Ndry_GRS          => gdp%gdimbound%Ndry_GRS
    EDGEtypeBANK      => gdp%gdimbound%EDGEtypeBANK
    EDGEtypeBANKerod  => gdp%gdimbound%EDGEtypeBANKerod
    U1inDRY           => gdp%gdimbound%U1inDRY
    V1inDRY           => gdp%gdimbound%V1inDRY
    FROMmnTOghostS1   => gdp%gdimbound%FROMmnTOghostS1
    FROMmnTOghostU1   => gdp%gdimbound%FROMmnTOghostU1
    FROMmnTOghostV1   => gdp%gdimbound%FROMmnTOghostV1
    nGPs1             => gdp%gdimbound%nGPs1
    mGPs1             => gdp%gdimbound%mGPs1
    nGPu1             => gdp%gdimbound%nGPu1
    mGPu1             => gdp%gdimbound%mGPu1
    nGPv1             => gdp%gdimbound%nGPv1
    mGPv1             => gdp%gdimbound%mGPv1
    mIPu1             => gdp%gdimbound%mIPu1
    nIPu1             => gdp%gdimbound%nIPu1
    mBIu1             => gdp%gdimbound%mBIu1
    nBIu1             => gdp%gdimbound%nBIu1
    GHOSTs1           => gdp%gdimbound%GHOSTs1
    GHOSTu1           => gdp%gdimbound%GHOSTu1
    GHOSTv1           => gdp%gdimbound%GHOSTv1
    kFLcut            => gdp%gdimbound%kFLcut
    SURFinDRY         => gdp%gdimbound%SURFinDRY
    POROS             => gdp%gdimbound%POROS
    INTx_GRS          => gdp%gdimbound%INTx_GRS
    INTy_GRS          => gdp%gdimbound%INTy_GRS
    EDGExyBANK        => gdp%gdimbound%EDGExyBANK
    aguu              => gdp%gdimbound%aguu
    agvv              => gdp%gdimbound%agvv
    xG                => gdp%gdimbound%xG
    yG                => gdp%gdimbound%yG
    Nx                => gdp%gdimbound%Nx
    Ny                => gdp%gdimbound%Ny
    xG_V1             => gdp%gdimbound%xG_V1
    xG_U1             => gdp%gdimbound%xG_U1
    yG_V1             => gdp%gdimbound%yG_V1
    yG_U1             => gdp%gdimbound%yG_U1
    noFLOODINGbanks   => gdp%gdimbound%noFLOODINGbanks
    activeNEVERghost  => gdp%gdimbound%activeNEVERghost
    zmod_or_noFLOOD = zmodel.or.noFLOODINGbanks !IF TRUE, ghost points are found also if bank is flooded (flooded ghost points) 
!
!ghost points for water surface REMOVED
!
! 
! ghost points for along-x velocity
!
! Note: normal to interface points toward the dry cell. Therefore if the velocity point is
! on the same side of the normal it is dry, otherwise it is wet.
!    
!    
    U1inDRY(:,mmax) = -1 !  excluded
!   
    DO m=1,mmax-1
       DO n=1,nmaxus
        !  if (kcu(n,m).eq.1) then
          !optimiz: write mADJ=m+1
          !note: with the new version EDGEtypeBANK can be only 2.
            U1inDRY(n,m) = -1 ! EDGEtypeBANK has one of the following:   =-1 poros not in the open interval (0,1) and cell dry;    =-5 poros=0 (vegetated) and cell wet;    =-6 poros=1 and cell dry,   = -3 poros not 1 or 0 and cell wet
            !cycle if at the boundary:
            if (typeEXTRAPstencil ==1) then
               if ( (kcs(n,m).eq.2.and.(kcs(n,m+1).eq.2.or.kcs(n,m+1).eq.0)) .or. ((kcs(n,m).eq.2.or.kcs(n,m).eq.0).and.kcs(n,m+1).eq.2) ) then
                  cycle
               endif
            elseif (typeEXTRAPstencil ==2) then
               if (kcs(n,m).eq.2.and.kcs(n,m+1).eq.2) then 
                 U1inDRY(n,m) = 0 !considered as fluid point, that its extrapolated from the domain in InterpG_XXX in order to have transmissive boundary condition
                 cycle
               elseif ( (kcs(n,m).eq.0.and.kcs(n,m+1).eq.2) .or. (kcs(n,m).eq.2.and.kcs(n,m+1).eq.0)) then 
                 cycle  ! considered like a dry point not used for interpolation              
               endif
            else
               continue
            endif
!
            ZmodL = zmod_or_noFLOOD.and.EDGEtypeBANK(2,n,m).eq.2
            ZmodR = zmod_or_noFLOOD.and.EDGEtypeBANK(4,n,m+1).eq.2
            if (.not.activeNEVERghost) then
               if (EDGEtypeBANK(2,n,m).eq.3.and.EDGEtypeBANK(4,n,m+1).eq.3) then   !both side channel 
                  U1inDRY(n,m) = 0
               elseif((EDGEtypeBANK(2,n,m).eq.-2.or.ZmodL).and.(EDGEtypeBANK(4,n,m+1).eq.-2.or.ZmodR)) then   !both side bank 
                  U1inDRY(n,m) = 1 
               elseif(((EDGEtypeBANK(2,n,m).eq.-2.or.ZmodL).and.EDGEtypeBANK(4,n,m+1).eq.3).or.(EDGEtypeBANK(2,n,m).eq.3.and.(EDGEtypeBANK(4,n,m+1).eq.-2.or.ZmodR))) then   !both side dry 
                  if (comparereal(aguu(n,m),0._fp)==0) then
                     U1inDRY(n,m) = 3          !its on the (fully dry/fully wet) bank/water interface
                  else
                     U1inDRY(n,m)   = 1 !otherwise it sets velocity to zero at partially active edge in  subroutine kfuvGHOST3
                  endif
               elseif (EDGEtypeBANK(2,n,m).eq.0.and.EDGEtypeBANK(4,n,m+1).eq.0) then  ! edge is cut on both sides
                  k    = 2
                  kADJ = 4
                  nADJ = n
                  mADJ = m+1
                  dx = xG_U1(n,m) - EDGExyBANK(n,m,k,1,1) ! FIRST point of EDGExyBANK is always in the middle of the edge
                  dy = yG_U1(n,m) - EDGExyBANK(n,m,k,1,2) ! FIRST point of EDGExyBANK is always in the middle of the edge
                  angle = atan2(dx*Ny(n,m)-dy*Nx(n,m),dx*Nx(n,m)+dy*Ny(n,m))   
                  dxADJ = xG_U1(n,m) - EDGExyBANK(nADJ,mADJ,kADJ,1,1) ! FIRST point of EDGExyBANK is always in the middle of the edge
                  dyADJ = yG_U1(n,m) - EDGExyBANK(nADJ,mADJ,kADJ,1,2) ! FIRST point of EDGExyBANK is always in the middle of the edge
                  angleADJ = atan2(dxADJ*Ny(nADJ,mADJ)-dyADJ*Nx(nADJ,mADJ),dxADJ*Nx(nADJ,mADJ)+dyADJ*Ny(nADJ,mADJ))    
                  DRYU    = (angle   .gt.-pi/2.0_fp.and.angle   .lt.pi/2.0_fp)
                  DRYUadj = (angleADJ.gt.-pi/2.0_fp.and.angleADJ.lt.pi/2.0_fp)
                  if (DRYU.AND.DRYUadj) then   !both dry
                     U1inDRY(n,m)   = 1
                  elseif(DRYU.or.DRYUadj) then !one wet one dry
                     if ((EDGEtypeBANKerod(2,n,m).eq.5).or.(EDGEtypeBANKerod(4,n,m+1).eq.5)) THEN   !we consider it as a ghost cell  if the discontiuity is  very small and look for a image point  in the classical  way, it would find a normal (or maybe not normal vertexintersection) and I prescribe my condition. Maybe it could help for having velocity vector along boundary instead of setting velocity equal to zero  
                        !alternatevily: I maybe could play with IntersSemilineSegm_always and modified to accept NEAREST_or_int=2 to give back both normal intersection external to segment and intersection to vertex and choose the shorter but I dont think it would change much. Or easier: i just go vertical for horizontal U and vertical for V and I do a 1D linear interpolation, or I just copy the above value of velocity (that s free slip!!!)
                        U1inDRY(n,m)   = 1 ! i consider it dry so it might become a ghost cell
                     else
                        if (comparereal(aguu(n,m),0._fp)==0) then
                           U1inDRY(n,m)   = 2 ! its on the bank/water interface CONSIDER TO SET ALWAYS 1
                        else
                           U1inDRY(n,m)   = 1 !otherwise it sets velocity to zero at partially active edge in  subroutine kfuvGHOST3
                        endif
                     endif
                  else                         
                     U1inDRY(n,m)   = 0 !both wet 
                  endif
               elseif (EDGEtypeBANK(2,n,m).eq.0.and.(EDGEtypeBANK(4,n,m+1).eq.-2.or.ZmodR)) then  ! edge is cut on one side and dry on the other
                  k    = 2   !cut edge
                  dx = xG_U1(n,m) - EDGExyBANK(n,m,k,1,1) ! FIRST point of EDGExyBANK is always in the middle of the edge
                  dy = yG_U1(n,m) - EDGExyBANK(n,m,k,1,2) ! FIRST point of EDGExyBANK is always in the middle of the edge
                  angle = atan2(dx*Ny(n,m)-dy*Nx(n,m),dx*Nx(n,m)+dy*Ny(n,m))                
                  DRYU = (angle.gt.-pi/2.0_fp.and.angle.lt.pi/2.0_fp)
                  if (DRYU) then   
                     U1inDRY(n,m)   = 1  !dry
                  else            
                     if (EDGEtypeBANKerod(2,n,m).eq.5) THEN   !we consider it as a ghost cell  if the discontiuity is  very small and look for a image point  in the classical  way, it would find a normal (or maybe not normal vertexintersection) and I prescribe my condition. Maybe it could help for having velocity vector along boundary instead of setting velocity equal to zero                      
                        U1inDRY(n,m)   = 1 ! i consider it dry so it might become a ghost cell
                     else
                        U1inDRY(n,m)   = 2 ! its on the bank/water interface
                     endif
                  endif               
               elseif ((EDGEtypeBANK(2,n,m).eq.-2.or.ZmodL).and.EDGEtypeBANK(4,n,m+1).eq.0) then  ! as before(cut one side and dry the other)but reversed
                  kADJ = 4   !cut edge
                  nADJ = n
                  mADJ = m+1
                  dxADJ = xG_U1(n,m) - EDGExyBANK(nADJ,mADJ,kADJ,1,1) ! FIRST point of EDGExyBANK is always in the middle of the edge
                  dyADJ = yG_U1(n,m) - EDGExyBANK(nADJ,mADJ,kADJ,1,2) ! FIRST point of EDGExyBANK is always in the middle of the edge
                  angleADJ= atan2(dxADJ*Ny(nADJ,mADJ)-dyADJ*Nx(nADJ,mADJ),dxADJ*Nx(nADJ,mADJ)+dyADJ*Ny(nADJ,mADJ)) 
                  DRYUadj = (angleADJ.gt.-pi/2.0_fp.and.angleADJ.lt.pi/2.0_fp)
                  if (DRYUadj) then  
                     U1inDRY(n,m)   = 1 !dry
                  else                  
                     if (EDGEtypeBANKerod(4,n,m+1).eq.5) THEN   !we consider it as a ghost cell  if the discontiuity is  very small and look for a image point  in the classical  way, it would find a normal (or maybe not normal vertexintersection) and I prescribe my condition. Maybe it could help for having velocity vector along boundary instead of setting velocity equal to zero                      
                        U1inDRY(n,m)   = 1 ! i consider it dry so it might become a ghost cell
                     else
                        U1inDRY(n,m)   = 2 ! its on the bank/water interface 
                     endif
                  endif 
               elseif (EDGEtypeBANK(2,n,m).eq.0.and.EDGEtypeBANK(4,n,m+1).eq.3) then   ! edge is cut on one side and wet on the other
                  k    = 2   !cut edge
                  dx = xG_U1(n,m) - EDGExyBANK(n,m,k,1,1) ! FIRST point of EDGExyBANK is always in the middle of the edge
                  dy = yG_U1(n,m) - EDGExyBANK(n,m,k,1,2) ! FIRST point of EDGExyBANK is always in the middle of the edge
                  angle = atan2(dx*Ny(n,m)-dy*Nx(n,m),dx*Nx(n,m)+dy*Ny(n,m))                
                  DRYU = (angle.gt.-pi/2.0_fp.and.angle.lt.pi/2.0_fp)
                  if (DRYU) then    
                     if (EDGEtypeBANKerod(2,n,m).eq.5) THEN   !we consider it as a ghost cell  if the discontiuity is  very small and look for a image point  in the classical  way, it would find a normal (or maybe not normal vertexintersection) and I prescribe my condition. Maybe it could help for having velocity vector along boundary instead of setting velocity equal to zero                      
                        U1inDRY(n,m)   = 1 ! i consider it dry so it might become a ghost cell
                     else
                        if (comparereal(aguu(n,m),0._fp)==0) then !i think it cannot happen since its cut/wet interface
                           U1inDRY(n,m)   = 2 ! its on the bank/water interface
                        else
                           U1inDRY(n,m)   = 1 !otherwise it sets velocity to zero at partially active edge in  subroutine kfuvGHOST3
                        endif
                     endif
                  else             
                     U1inDRY(n,m)   = 0  !wet  
                  endif   
               elseif (EDGEtypeBANK(2,n,m).eq.3.and.EDGEtypeBANK(4,n,m+1).eq.0) then   ! as before(cut one side and wet the other)but reversed  
                  kADJ = 4   !cut edge
                  nADJ = n
                  mADJ = m+1
                  dxADJ = xG_U1(n,m) - EDGExyBANK(nADJ,mADJ,kADJ,1,1) ! FIRST point of EDGExyBANK is always in the middle of the edge
                  dyADJ = yG_U1(n,m) - EDGExyBANK(nADJ,mADJ,kADJ,1,2) ! FIRST point of EDGExyBANK is always in the middle of the edge
                  angleADJ= atan2(dxADJ*Ny(nADJ,mADJ)-dyADJ*Nx(nADJ,mADJ),dxADJ*Nx(nADJ,mADJ)+dyADJ*Ny(nADJ,mADJ)) 
                  DRYUadj = (angleADJ.gt.-pi/2.0_fp.and.angleADJ.lt.pi/2.0_fp)
                  if (DRYUadj) then  
                     if (EDGEtypeBANKerod(4,n,m+1).eq.5) THEN   !we consider it as a ghost cell  if the discontiuity is  very small and look for a image point  in the classical  way, it would find a normal (or maybe not normal vertexintersection) and I prescribe my condition. Maybe it could help for having velocity vector along boundary instead of setting velocity equal to zero                      
                        U1inDRY(n,m)   = 1 ! i consider it dry so it might become a ghost cell
                     else
                        if (comparereal(aguu(n,m),0._fp)==0) then !i think it cannot happen since its cut/wet interface
                           U1inDRY(n,m)   = 2 ! its on the bank/water interface
                        else
                           U1inDRY(n,m)   = 1 !otherwise it sets velocity to zero at partially active edge in  subroutine kfuvGHOST3
                        endif
                     endif
                  else                  
                     U1inDRY(n,m)   = 0 !wet  
                  endif 
               elseif (zmodel.and.EDGEtypeBANK(2,n,m).eq.3.and.EDGEtypeBANK(4,n,m+1).eq.0) then !I THINK THIS SHOULD BE DELETED
               endif
            else !if (activeNEVERghost) then
               ! to be modified for flooding and similar stuff
               if (comparereal(aguu(n,m),0._fp)>0) then
                  U1inDRY(n,m) = 0 !wet
               elseif (comparereal(aguu(n,m),0._fp)==0) then  
                  U1inDRY(n,m) = 1 !dry
               endif
            endif
       !   else !if (kcu(n,m).eq.0) then
       !      U1inDRY(n,m) = 1 !dry point
       !   endif
    !        write(98919191,'(4i6)') nst,m,n,U1inDRY(n,m) 
       ENDDO
    ENDDO  
!
    totGHOSTu1 = 0
    DO m=1,mmax 
       DO n=1,nmaxus
          !initialize GHOSTu1
         ! if(kcu(n,m).ge.1) then
         !    GHOSTu1(n,m) = 0 !fluid cell
         ! else !if (kcu(n,m).eq.0) THEN
         !    GHOSTu1(n,m) = 2 ! dry non-ghost 
         ! endif
         ! GHOSTu1DBL(n,m) = 0
          !if in dry, check if it is a ghost point
          if(U1inDRY(n,m).eq.1) then     
            contWDint = 0
            ! lower                     
            nAD=n-1
            mAD=m 
            if ((nAD.ge.1)) then
               if (kcu(nAD,mAD).ne.0) then !the first condition (and similar below) can be avoided passing the array as a single column                         
               
                  if ((EDGEtypeBANK(1,n,m).eq.3.and.EDGEtypeBANK(3,nAD,mAD).eq.-2.or.     &!  If a horizontal ghost point has an adjacent cell with wet/dry interface, then that is not a ghost cell 
                     EDGEtypeBANK(1,n,m).eq.-2.and.EDGEtypeBANK(3,nAD,mAD).eq.3).or.    &
                     (EDGEtypeBANK(1,n,m+1).eq.3.and.EDGEtypeBANK(3,nAD,mAD+1).eq.-2.or. & !  If a horizontal ghost point has an adjacent cell with wet/dry interface, then that is not a ghost cell 
                     EDGEtypeBANK(1,n,m+1).eq.-2.and.EDGEtypeBANK(3,nAD,mAD+1).eq.3)) then
                     contWDint = contWDint + 1
                     vectADJ(1) = 1 ! I assign 1, I dont want it to be a ghost.
                  else
                     vectADJ(1) = U1inDRY(nAD,mAD)
                  endif
               else
                  vectADJ(1) = 1  
               endif
            else
               vectADJ(1) = 1 !It is outside the domain,I consider it dry
            endif
            ! right
            nAD=n
            mAD=m+1 
            if (mAD.le.mmax)  then !this if (and similar below) can be avoided passing the array as a single column
               if (kcu(nAD,mAD).ne.0) then
                  vectADJ(2) = U1inDRY(nAD,mAD)
               else
                  vectADJ(2) = 1 
               endif
            else
               vectADJ(2) = 1 !It is outside the domain,I consider it dry
            endif
            ! upper
            nAD=n+1
            mAD=m  
            if  (nAD.le.nmaxus) then
               if (kcu(nAD,mAD).ne.0)  then !this if (and similar below) can be avoided passing the array as a single column    
                  if ((EDGEtypeBANK(3,n,m).eq.3.and.EDGEtypeBANK(1,nAD,mAD).eq.-2.or.     & !  If a horizontal ghost point has an adjacent cell with wet/dry interface, then that is not a ghost cell 
                     EDGEtypeBANK(3,n,m).eq.-2.and.EDGEtypeBANK(1,nAD,mAD).eq.3).or.    &
                     (EDGEtypeBANK(3,n,m+1).eq.3.and.EDGEtypeBANK(1,nAD,mAD+1).eq.-2.or. &  !  If a horizontal ghost point has an adjacent cell with wet/dry interface, then that is not a ghost cell 
                     EDGEtypeBANK(3,n,m+1).eq.-2.and.EDGEtypeBANK(1,nAD,mAD+1).eq.3)) then
                     contWDint = contWDint + 1
                     vectADJ(3) = 1 ! I assign 1, I dont want it to be a ghost.
                  else
                     vectADJ(3) = U1inDRY(nAD,mAD)
                  endif
               else
                  vectADJ(3) = 1  
               endif
            else
               vectADJ(3) = 1 !It is outside the domain,I consider it dry
            endif
            ! left
            nAD=n 
            mAD=m-1 
            if (mAD.ge.1)  then !this if (and similar below) can be avoided passing the array as a single column
               if (kcu(nAD,mAD).ne.0) then
                  vectADJ(4) = U1inDRY(nAD,mAD)
               else
                  vectADJ(4) = 1  
               endif
            else
               vectADJ(4) = 1 !It is outside the domain,I consider it dry
            endif
        
            IF (ANY(vectADJ(1:4).EQ.0)) then
               GHOSTu1(n,m) = 1 !dry ghost
               totGHOSTu1 = totGHOSTu1+1
               nGPu1(totGHOSTu1) = n
               mGPu1(totGHOSTu1) = m
               FROMmnTOghostU1(n,m) = totGHOSTu1
               !check of double valued ghost cells (2 dry cells on one direct and 2 wet in the other.
               if ((vectADJ(1).eq.0.and.vectADJ(3).eq.0.and.vectADJ(2).eq.1.and.vectADJ(4).eq.1).or.&
                   (vectADJ(2).eq.0.and.vectADJ(4).eq.0.and.vectADJ(1).eq.1.and.vectADJ(3).eq.1)) then
                   write(*,*) 'double valued ghost cell u1 m,n',m,n
                   !GHOSTu1DBL(n,m) = 1
                   !call d3stop(1, gdp)
                   ! I would mark a logical matrix(n,m) that is true if n,m is double valued. Then in InterpG subroutines
                   ! I would do a check and I would use the right normal every time i deal with a ghost
               else
                   !GHOSTu1DBL(n,m) = 0
               endif
            ELSEIF (contWDint.gt.0) then  
               GHOSTu1(n,m) = 4  !ORTHOGONAL to wet-dry interface 
            ELSE
               GHOSTu1(n,m) = 2 ! dry non-ghost 
            ENDIF
           ! if(n.eq.2.and.m.eq.10) write(888888,'(10i8)') EDGEtypeBANK(2,2,10),EDGEtypeBANK(4,2,10+1),vectADJ(:),contWDint
          ELSEif (U1inDRY(n,m).eq.3) then   
             GHOSTu1(n,m) = 3 ! ON THE (fully dry-fully wet) WET/DRY interface ( non-ghost )
          ELSEif (U1inDRY(n,m).eq.2) then   
             GHOSTu1(n,m) = 3 
             ! ON THE WET/DRY interface ( ghost ) only after doing reconstruction of interface in boundary cells otherwise it does not find intersection sometimes
            ! totGHOSTu1 = totGHOSTu1+1
            ! nGPu1(totGHOSTu1) = n
             !mGPu1(totGHOSTu1) = m
            ! FROMmnTOghostU1(n,m) = totGHOSTu1
          ELSEif (U1inDRY(n,m).eq.0) then   
             GHOSTu1(n,m) = 0
          ELSEif (U1inDRY(n,m).eq.-1) then   
             GHOSTu1(n,m) = 2 !dry non-ghost 
          ENDIF
       ENDDO
    ENDDO   
! 
! ghost points for along-y velocity
!
! Note: normal to interface points toward the dry cell. Therefore if the velocity point is
! on the same side of the normal it is dry, otherwise it is wet.
!    
    V1inDRY(nmaxus,:) = -1 ! excluded
!
    DO m=1,mmax
       DO n=1,nmaxus-1
!
            V1inDRY(n,m) = -1 ! EDGEtypeBANK has one of the following:   =-1 poros not in the open interval (0,1) and cell dry;    =-5 poros=0 (vegetated) and cell wet;    =-6 poros=1 and cell dry
            !cycle if at the boundary:
            if (typeEXTRAPstencil ==1) then
               if ( (kcs(n,m).eq.2.and.(kcs(n+1,m).eq.2.or.kcs(n+1,m).eq.0)) .or. ((kcs(n,m).eq.2.or.kcs(n,m).eq.0).and.kcs(n+1,m).eq.2) ) then
                  cycle
               endif
            elseif (typeEXTRAPstencil ==2) then
               if (kcs(n+1,m).eq.2.and.kcs(n,m).eq.2) then 
                 V1inDRY(n,m) = 0 !considered as fluid point, that its extrapolated from the domain in InterpG_XXX in order to have transmissive boundary condition
                 cycle
               elseif ( (kcs(n,m).eq.0.and.kcs(n+1,m).eq.2) .or. (kcs(n,m).eq.2.and.kcs(n+1,m).eq.0)) then 
                 cycle  ! considered like a dry point not used for interpolation              
               endif
            else
              continue
            endif
  !       if (kcv(n,m).eq.1) then
            ZmodL = zmod_or_noFLOOD.and.EDGEtypeBANK(3,n,m).eq.2
            ZmodR = zmod_or_noFLOOD.and.EDGEtypeBANK(1,n+1,m).eq.2
            if (.not.activeNEVERghost) then
               if (EDGEtypeBANK(3,n,m).eq.3.and.EDGEtypeBANK(1,n+1,m).eq.3) then   !both side wet 
                  V1inDRY(n,m) = 0
               elseif((EDGEtypeBANK(3,n,m).eq.-2.or.ZmodL).and.(EDGEtypeBANK(1,n+1,m).eq.-2.or.ZmodR)) then   !both side dry 
                  V1inDRY(n,m) = 1 
               elseif(((EDGEtypeBANK(3,n,m).eq.-2.or.ZmodL).and.EDGEtypeBANK(1,n+1,m).eq.3).OR.(EDGEtypeBANK(3,n,m).eq.3.and.(EDGEtypeBANK(1,n+1,m).eq.-2.or.ZmodR))) then   !bank/WATER INTERFACE 
                  V1inDRY(n,m) = 3                   !its on the bank/water interface (full dry/full wet though)
               elseif (EDGEtypeBANK(3,n,m).eq.0.and.EDGEtypeBANK(1,n+1,m).eq.0) then  ! edge is cut on both sides
                  k    = 3
                  kADJ = 1
                  nADJ = n+1
                  mADJ = m
                  dx = xG_V1(n,m) - EDGExyBANK(n,m,k,1,1) ! FIRST point of EDGExyBANK is always in the middle of the edge
                  dy = yG_V1(n,m) - EDGExyBANK(n,m,k,1,2) ! FIRST point of EDGExyBANK is always in the middle of the edge
                  angle = atan2(dx*Ny(n,m)-dy*Nx(n,m),dx*Nx(n,m)+dy*Ny(n,m))   
                  dxADJ = xG_V1(n,m) - EDGExyBANK(nADJ,mADJ,kADJ,1,1) ! FIRST point of EDGExyBANK is always in the middle of the edge
                  dyADJ = yG_V1(n,m) - EDGExyBANK(nADJ,mADJ,kADJ,1,2) ! FIRST point of EDGExyBANK is always in the middle of the edge
                  angleADJ = atan2(dxADJ*Ny(nADJ,mADJ)-dyADJ*Nx(nADJ,mADJ),dxADJ*Nx(nADJ,mADJ)+dyADJ*Ny(nADJ,mADJ))    
                  DRYU    = (angle   .gt.-pi/2.0_fp.and.angle   .lt.pi/2.0_fp)
                  DRYUadj = (angleADJ.gt.-pi/2.0_fp.and.angleADJ.lt.pi/2.0_fp)
                  if (DRYU.AND.DRYUadj) then   !both dry
                     V1inDRY(n,m)   = 1
                  elseif(DRYU.or.DRYUadj) then !one wet one dry
                     if ((EDGEtypeBANKerod(3,n,m).eq.5).or.(EDGEtypeBANKerod(1,n+1,m).eq.5)) THEN   !we consider it as a ghost cell  if the discontiuity is  very small and look for a image point  in the classical  way, it would find a normal (or maybe not normal vertexintersection) and I prescribe my condition. Maybe it could help for having velocity vector along boundary instead of setting velocity equal to zero  
                        !alternatevily: I maybe could play with IntersSemilineSegm_always and modified to accept NEAREST_or_int=2 to give back both normal intersection external to segment and intersection to vertex and choose the shorter but I dont think it would change much. Or easier: i just go vertical for horizontal U and vertical for V and I do a 1D linear interpolation, or I just copy the above value of velocity (that s free slip!!!)
                        V1inDRY(n,m)   = 1 ! i consider it dry so it might become a ghost cell
                     else
                        if (comparereal(agvv(n,m),0._fp)==0) then !i think it cannot happen since its cut/wet interface
                           V1inDRY(n,m)   = 2 ! its on the bank/water interface
                        else
                           V1inDRY(n,m)   = 1 !otherwise it sets velocity to zero at partially active edge in  subroutine kfuvGHOST3
                        endif
                     endif
                  else                         
                     V1inDRY(n,m)   = 0 !both wet 
                  endif
               elseif (EDGEtypeBANK(3,n,m).eq.0.and.(EDGEtypeBANK(1,n+1,m).eq.-2.or.ZmodR)) then  ! edge is cut on one side and dry on the other
                  k    = 3   !cut edge
                  dx = xG_V1(n,m) - EDGExyBANK(n,m,k,1,1) ! FIRST point of EDGExyBANK is always in the middle of the edge
                  dy = yG_V1(n,m) - EDGExyBANK(n,m,k,1,2) ! FIRST point of EDGExyBANK is always in the middle of the edge
                  angle = atan2(dx*Ny(n,m)-dy*Nx(n,m),dx*Nx(n,m)+dy*Ny(n,m))                
                  DRYU = (angle.gt.-pi/2.0_fp.and.angle.lt.pi/2.0_fp)
                  if (DRYU) then   
                     V1inDRY(n,m)   = 1  !dry
                  else            
                     if (EDGEtypeBANKerod(3,n,m).eq.5) THEN   !we consider it as a ghost cell  if the discontiuity is  very small and look for a image point  in the classical  way, it would find a normal (or maybe not normal vertexintersection) and I prescribe my condition. Maybe it could help for having velocity vector along boundary instead of setting velocity equal to zero                      
                        V1inDRY(n,m)   = 1 ! i consider it dry so it might become a ghost cell
                     else
                        if (comparereal(agvv(n,m),0._fp)==0) then !i think it cannot happen since its cut/wet interface
                           V1inDRY(n,m)   = 2 ! its on the bank/water interface
                        else
                           V1inDRY(n,m)   = 1 !otherwise it sets velocity to zero at partially active edge in  subroutine kfuvGHOST3
                        endif
                     endif
                  endif               
               elseif ((EDGEtypeBANK(3,n,m).eq.-2.or.ZmodL).and.EDGEtypeBANK(1,n+1,m).eq.0) then  ! as before(cut one side and dry the other)but reversed
                  kADJ = 1   !cut edge
                  nADJ = n+1
                  mADJ = m
                  dxADJ = xG_V1(n,m) - EDGExyBANK(nADJ,mADJ,kADJ,1,1) ! FIRST point of EDGExyBANK is always in the middle of the edge
                  dyADJ = yG_V1(n,m) - EDGExyBANK(nADJ,mADJ,kADJ,1,2) ! FIRST point of EDGExyBANK is always in the middle of the edge
                  angleADJ= atan2(dxADJ*Ny(nADJ,mADJ)-dyADJ*Nx(nADJ,mADJ),dxADJ*Nx(nADJ,mADJ)+dyADJ*Ny(nADJ,mADJ))   
                  DRYUadj = (angleADJ.gt.-pi/2.0_fp.and.angleADJ.lt.pi/2.0_fp)
                  if (DRYUadj) then  
                     V1inDRY(n,m)   = 1 !dry
                  else                  
                     if (EDGEtypeBANKerod(1,n+1,m).eq.5) THEN   !we consider it as a ghost cell  if the discontiuity is  very small and look for a image point  in the classical  way, it would find a normal (or maybe not normal vertexintersection) and I prescribe my condition. Maybe it could help for having velocity vector along boundary instead of setting velocity equal to zero                      
                        V1inDRY(n,m)   = 1 ! i consider it dry so it might become a ghost cell
                     else
                        V1inDRY(n,m)   = 2 ! its on the bank/water interface 
                     endif
                  endif 
               elseif (EDGEtypeBANK(3,n,m).eq.0.and.EDGEtypeBANK(1,n+1,m).eq.3) then   ! cut one side and wet the other
                  k    = 3   !cut edge
                  dx = xG_V1(n,m) - EDGExyBANK(n,m,k,1,1) ! FIRST point of EDGExyBANK is always in the middle of the edge
                  dy = yG_V1(n,m) - EDGExyBANK(n,m,k,1,2) ! FIRST point of EDGExyBANK is always in the middle of the edge
                  angle = atan2(dx*Ny(n,m)-dy*Nx(n,m),dx*Nx(n,m)+dy*Ny(n,m))                
                  DRYU = (angle.gt.-pi/2.0_fp.and.angle.lt.pi/2.0_fp)
                  if (DRYU) then    
                     if (EDGEtypeBANKerod(3,n,m).eq.5) THEN   !we consider it as a ghost cell  if the discontiuity is  very small and look for a image point  in the classical  way, it would find a normal (or maybe not normal vertexintersection) and I prescribe my condition. Maybe it could help for having velocity vector along boundary instead of setting velocity equal to zero                      
                        V1inDRY(n,m)   = 1 ! i consider it dry so it might become a ghost cell
                     else
                        if (comparereal(agvv(n,m),0._fp)==0) then !i think it cannot happen since its cut/wet interface
                           V1inDRY(n,m)   = 2 ! its on the bank/water interface
                        else
                           V1inDRY(n,m)   = 1 !otherwise it sets velocity to zero at partially active edge in  subroutine kfuvGHOST3
                        endif
                     endif
                  else             
                     V1inDRY(n,m)   = 0  !wet  
                  endif   
               elseif (EDGEtypeBANK(3,n,m).eq.3.and.EDGEtypeBANK(1,n+1,m).eq.0) then   ! as before(cut one side and wet the other)but reversed  
                  kADJ = 1   !cut edge
                  nADJ = n+1
                  mADJ = m
                  dxADJ = xG_V1(n,m) - EDGExyBANK(nADJ,mADJ,kADJ,1,1) ! FIRST point of EDGExyBANK is always in the middle of the edge
                  dyADJ = yG_V1(n,m) - EDGExyBANK(nADJ,mADJ,kADJ,1,2) ! FIRST point of EDGExyBANK is always in the middle of the edge
                  angleADJ= atan2(dxADJ*Ny(nADJ,mADJ)-dyADJ*Nx(nADJ,mADJ),dxADJ*Nx(nADJ,mADJ)+dyADJ*Ny(nADJ,mADJ)) 
                  DRYUadj = (angleADJ.gt.-pi/2.0_fp.and.angleADJ.lt.pi/2.0_fp)
                  if (DRYUadj) then  
                     if (EDGEtypeBANKerod(1,n+1,m).eq.5) THEN   !we consider it as a ghost cell  if the discontiuity is  very small and look for a image point  in the classical  way, it would find a normal (or maybe not normal vertexintersection) and I prescribe my condition. Maybe it could help for having velocity vector along boundary instead of setting velocity equal to zero                      
                        V1inDRY(n,m)   = 1 ! i consider it dry so it might become a ghost cell
                     else
                        if (comparereal(agvv(n,m),0._fp)==0) then !i think it cannot happen since its cut/wet interface
                           V1inDRY(n,m)   = 2 ! its on the bank/water interface
                        else
                           V1inDRY(n,m)   = 1 !otherwise it sets velocity to zero at partially active edge in  subroutine kfuvGHOST3
                        endif
                     endif
                  else                  
                     V1inDRY(n,m)   = 0 !wet  
                  endif 
               endif
            else !if (activeNEVERghost) then
               ! to be modified for flooding and similar stuff
               if (comparereal(agvv(n,m),0._fp)>0) then
                  V1inDRY(n,m) = 0 !wet
               elseif (comparereal(agvv(n,m),0._fp)==0) then  
                  V1inDRY(n,m) = 1 !dry
               endif
            endif
       !   else !if (kcv(n,m).eq.0) THEN
       !      V1inDRY(n,m) = 1 ! dry cell
       !   endif
          !  write(98919191,'(4i6)') nst,m,n,V1inDRY(n,m) 
          ! write(80808081,'(13i11)') nst,m,n, V1inDRY(n,m) 
       ENDDO
    ENDDO  
!

    totGHOSTv1 = 0
    DO m=1,mmax
       DO n=1,nmaxus
          !initialize GHOSTv1  
          !if (kcv(n,m).GE.1) then
          !   GHOSTv1(n,m) = 0 ! fluid cell
          !else !if (kcv(n,m).eq.0) THEN
          !   GHOSTv1(n,m) = 2 ! dry non-ghost 
          !endif
          !GHOSTv1DBL(n,m) = 0
          !if in dry, check if it is a ghost point 
          if (V1inDRY(n,m).eq.1) then   
            contWDint = 0
            ! lower                      
            nAD=n-1
            if (nAD.ge.1)  then !this if (and similar below) can be avoided passing the array as a single column
               mAD=m 
               if (kcv(nAD,mAD).ne.0) then
                  vectADJ(1) = V1inDRY(nAD,mAD)
               else
                  vectADJ(1) = 1
               endif
            else
               vectADJ(1) = 1
            endif
            ! right
            nAD=n
            mAD=m+1 ! i dont need an if mAD.gt.mmax since kcv(n,m) cannot be 1 at the boundary
            if (mAD.LE.mmax) then
   !            
               if (kcv(nAD,mAD).ne.0) then
                  if ((EDGEtypeBANK(2,n,m).eq.3.and.EDGEtypeBANK(4,nAD,mAD).eq.-2.or.     &!  If a vertical ghost point has an adjacent cell with wet/dry interface, then that is not a ghost cell 
                     EDGEtypeBANK(2,n,m).eq.-2.and.EDGEtypeBANK(4,nAD,mAD).eq.3).or.    &
                     (EDGEtypeBANK(2,n+1,m).eq.3.and.EDGEtypeBANK(4,nAD+1,mAD).eq.-2.or. & !  If a vertical ghost point has an adjacent cell with wet/dry interface, then that is not a ghost cell 
                     EDGEtypeBANK(2,n+1,m).eq.-2.and.EDGEtypeBANK(4,nAD+1,mAD).eq.3)) then
                     contWDint = contWDint + 1
                     vectADJ(2) = 1 ! I assign 1, I dont want it to be a ghost.
                  else
                     vectADJ(2) = V1inDRY(nAD,mAD)
                  endif
               else
                  vectADJ(2) = 1  
               endif
            else
               vectADJ(2) = 1 !It is outside the domain,I consider it dry
            endif
            ! upper              
            nAD=n+1
            if (nAD.le.nmaxus)  then !this if (and similar below) can be avoided passing the array as a single column
               mAD=m  
               if (kcv(nAD,mAD).ne.0) then
                  vectADJ(3) = V1inDRY(nAD,mAD)
               else
                  vectADJ(3) = 1 !It is outside the domain,I consider it dry
               endif
            else
               vectADJ(3) = 1 !It is outside the domain,I consider it dry
            endif
            ! left ! i dont need an if mAD.gt.mmax since kcv(n,m) cannot be 1 at the boundary
            nAD=n 
            mAD=m-1 
            if (mAD.ge.1) then
               if (kcv(nAD,mAD).ne.0) then
                  if ((EDGEtypeBANK(4,n,m).eq.3.and.EDGEtypeBANK(2,nAD,mAD).eq.-2.or.     & !  If a vertical ghost point has an adjacent cell with wet/dry interface, then that is not a ghost cell 
                     EDGEtypeBANK(4,n,m).eq.-2.and.EDGEtypeBANK(2,nAD,mAD).eq.3).or.    &
                     (EDGEtypeBANK(4,n+1,m).eq.3.and.EDGEtypeBANK(2,nAD+1,mAD).eq.-2.or. &  !  If a vertical ghost point has an adjacent cell with wet/dry interface, then that is not a ghost cell 
                     EDGEtypeBANK(4,n+1,m).eq.-2.and.EDGEtypeBANK(2,nAD+1,mAD).eq.3)) then
                     contWDint = contWDint + 1
                     vectADJ(4) = 1 ! I assign 1, I dont want it to be a ghost.
                  else
                     vectADJ(4) = V1inDRY(nAD,mAD)
                  endif
               else
                  vectADJ(4) = 1  
               endif
            else
               vectADJ(4) = 1  
            endif
!                
            IF (ANY(vectADJ(1:4).EQ.0)) then
               GHOSTv1(n,m) =  1 ! dry ghost 
               totGHOSTv1 = totGHOSTv1+1
               nGPv1(totGHOSTv1) = n
               mGPv1(totGHOSTv1) = m
               FROMmnTOghostV1(n,m) = totGHOSTv1
               !check of double valued ghost cells (2 dry cells on one direct and 2 wet in the other.
               if ((vectADJ(1).eq.0.and.vectADJ(3).eq.0.and.vectADJ(2).eq.1.and.vectADJ(4).eq.1).or.&
                   (vectADJ(2).eq.0.and.vectADJ(4).eq.0.and.vectADJ(1).eq.1.and.vectADJ(3).eq.1)) then
                   write(*,*) 'double valued ghost cell v1 in m,n',m,n
                   !GHOSTv1DBL(n,m) = 1
                   !call d3stop(1, gdp)
                   ! I would mark a logical matrix(n,m) that is true if n,m is double valued. Then in InterpG subroutines
                   ! I would do a check and I would use the right normal every time i deal with a ghost
               else
           !       GHOSTv1DBL(n,m) = 0
               endif
            ELSEIF (contWDint.gt.0) then  
               GHOSTv1(n,m) = 4  !ORTHOGONAL to wet-dry interface 
            ELSE 
               GHOSTv1(n,m) = 2 ! dry non-ghost 
            ENDIF
          ELSEif (V1inDRY(n,m).eq.3) then   
             GHOSTv1(n,m) = 3 ! ON THE (fully dry-fully wet) WET/DRY interface ( non-ghost )
          ELSEif (V1inDRY(n,m).eq.2) then   
             GHOSTv1(n,m) = 3 
             ! ON THE WET/DRY interface ( ghost ) only after doing recunstruction of interface in boundary cells otherwise it does not find intersection sometimes
           !  totGHOSTv1 = totGHOSTv1+1
           !  nGPv1(totGHOSTv1) = n
            ! mGPv1(totGHOSTv1) = m
            ! FROMmnTOghostV1(n,m) = totGHOSTv1
         !ELSE !if (V1inDRY(n,m).eq.0) then   
         !    GHOSTv1(n,m) = 0 ! fluid cell
          ELSEif (V1inDRY(n,m).eq.0) then   
             GHOSTv1(n,m) = 0
          ELSEif (V1inDRY(n,m).eq.-1) then   
             GHOSTv1(n,m) = 2 !dry non-ghost 
          ENDIF
       ENDDO
    ENDDO   
!
!  define fluid points all the points on the border of the computational domain and set the velocities and levels according
!  to the boundary conditions. This points are needed (not very often indeed) when the interpolation stencil falls 
!  in a cell that is outside the domain. This should never happen for water level, but it could for velocities, as 
!  I noticed for the u-shaped channel
!    
  ! do m=1,mmax
    !  do n=1,nmaxus
     !    if (kcs(n,m).eq.1) then
     !
     !i decided to do it directly in InterpGhost_s_u_v
!
!  compute kFLcut, masking coefficient for mass fluxes at ghost cells in continuity equation
!
    DO m=1,mmax
       DO n=1,nmaxus
          cont = 2
          nd = n-1
          md = m-1
          vectADJ(1) = kfu(n,m)
          vectADJ(2) = kfv(n,m)
          cont = 2
          if (nd.ge.1) then 
             cont = cont + 1
             vectADJ(cont) = kfv(nd,m)
          endif
          if (md.ge.1) then 
             cont = cont + 1
             vectADJ(cont) = kfu(n,md)
          endif
          if (ANY(vectADJ(1:cont)==1)) then  ! at least one edge is open so water flows
             kFLcut(n,m) = 1
          else
             if (kfs_cc(n,m)==0) then  ! water still flows cause some cut-edges are wet
                kFLcut(n,m) = 1
             else
                kFLcut(n,m) = 0
             endif
          endif

          !kFLcut(n,m) = kFLcut(n,m)==1.and.poros012(n,m).eq.2
       enddo
    enddo
    !
    if (kFLcutEQ1==1) THEN
       kFLcut(:,:) =1
    endif

RETURN
END
