  SUBROUTINE deferred_GRADs1(kfs       ,s0        ,kfs_cc    ,xG_L      ,yG_L      ,&
                             xG        ,yG        ,PSIx      ,PSIy      ,ETAx      ,&
                             ETAy      ,gvu       ,cutfac    ,icx       ,icy       ,&
                             Nx        ,Ny        ,INTx_GRS  ,INTy_GRS  ,Ndry_GRS  ,&
                             xcor      ,ycor      ,EDGEtypeBANK,EDGExBANK,EDGEyBANK,&
                             agsqs     ,gsqs      ,nlb       ,mlb       ,nub       ,&
                             mub       ,nmlb      ,nmub      ,nmmax     ,nst       ,&
                             kcs       ,kfu       ,guu       ,gvv       ,dps       ,&
                             aguu      ,gradS1anal,TYPEgradDEFERR, gdp)
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
!   Function: Compute corrective terms for the gradient of water surface for non-orthogonaly in cut cells
!
!
!   Author: Alberto Canestrelli
!
!--declarations----------------------------------------------------------------
!
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    real(fp), dimension(:)        , pointer :: sourceU
    integer, dimension(:)         , pointer :: edge6
    logical                       , pointer :: periodSURFACE
    real(fp), dimension(:,:)      , pointer :: xcorV1
    real(fp), dimension(:,:)      , pointer :: ycorV1
    real(fp), dimension(:,:)      , pointer :: xG_U1
    real(fp), dimension(:,:)      , pointer :: yG_U1
    real(fp), dimension(:,:,:,:,:), pointer :: EDGExyBANK
    real(fp), dimension(:,:)      , pointer :: xcorU1
    real(fp), dimension(:,:)      , pointer :: ycorU1
    real(fp), dimension(:,:)      , pointer :: xG_V1
    real(fp), dimension(:,:)      , pointer :: yG_V1
    real(fp), dimension(:,:)      , pointer :: agvv
    integer, dimension(:,:)       , pointer :: ghostu1
    integer, dimension(:,:)       , pointer :: ghostv1
    logical                       , pointer :: gradDEFER3orderDIFF
    logical                       , pointer :: analDEFERR
    logical                       , pointer :: noCUTfac
    real(fp), dimension(:)        , pointer :: deltaS1cut
    real(fp), dimension(:)        , pointer :: EXPsouC
    real(fp), dimension(:)        , pointer :: EXPsouR
    real(fp), dimension(:)        , pointer :: EXPsouL
    real(fp), dimension(:,:)      , pointer :: eeC
    real(fp), dimension(:,:)      , pointer :: eeR
    real(fp), dimension(:,:)      , pointer :: eeL
    logical                       , pointer :: partIMPLgrad
    integer                       , pointer :: TYPEpartIMPLgrad
!
! global variables
!
    real(prec), dimension(nmlb:nmub)                               , intent(inout) :: dps
    real(fp), dimension(nmlb:nmub)                                 , intent(in)    :: gradS1anal
    real(fp), dimension(nmlb:nmub)                                 , intent(in)    :: aguu    
    real(fp), dimension(nmlb:nmub)                                 , intent(in)    :: xcor
    real(fp), dimension(nmlb:nmub)                                 , intent(in)    :: ycor
    real(fp), dimension(nmlb:nmub)                                 , intent(in)    :: s0
    real(fp), dimension(nmlb:nmub)                                 , intent(in)    :: Nx
    real(fp), dimension(nmlb:nmub)                                 , intent(in)    :: Ny
    real(fp), dimension(nmlb:nmub)                                 , intent(in)    :: xG_L
    real(fp), dimension(nmlb:nmub)                                 , intent(in)    :: yG_L
    real(fp), dimension(nmlb:nmub)                                 , intent(in)    :: xG
    real(fp), dimension(nmlb:nmub)                                 , intent(in)    :: yG
    real(fp), dimension(nmlb:nmub)                                 , intent(in)    :: PSIx
    real(fp), dimension(nmlb:nmub)                                 , intent(in)    :: PSIy
    real(fp), dimension(nmlb:nmub)                                 , intent(in)    :: ETAx
    real(fp), dimension(nmlb:nmub)                                 , intent(in)    :: ETAy
    real(fp), dimension(nmlb:nmub)                                 , intent(in)    :: gvu
    real(fp), dimension(nmlb:nmub)                                 , intent(in)    :: guu    
    real(fp), dimension(nmlb:nmub)                                 , intent(in)    :: gvv    
    real(fp), dimension(nmlb:nmub)                                 , intent(in)    :: agsqs
    real(fp), dimension(nmlb:nmub)                                 , intent(in)    :: gsqs
    real(fp), dimension(nmlb:nmub)                                 , intent(out)   :: cutfac
    real(fp), dimension(5,nmlb:nmub)                               , intent(in)    :: INTx_GRS
    real(fp), dimension(5,nmlb:nmub)                               , intent(in)    :: INTy_GRS
    real(fp), dimension(nmlb:nmub,1:4,1:2)                         , intent(in)    :: EDGExBANK
    real(fp), dimension(nmlb:nmub,1:4,1:2)                         , intent(in)    :: EDGEyBANK
                                                                
    integer, dimension(nmlb:nmub)                                  , intent(in)    :: Ndry_GRS
    integer, dimension(nmlb:nmub)                                  , intent(in)    :: kfs
    integer, dimension(nmlb:nmub)                                  , intent(in)    :: kfs_cc
    integer, dimension(nmlb:nmub)                                  , intent(in)    :: kcs
    integer, dimension(nmlb:nmub)                                  , intent(in)    :: kfu    
    integer, dimension(4,nmlb:nmub)                                , intent(in)    :: EDGEtypeBANK
    integer                                                           , intent(in)    :: nmlb
    integer                                                           , intent(in)    :: nmub
    integer                                                           , intent(in)    :: nlb
    integer                                                           , intent(in)    :: nub
    integer                                                           , intent(in)    :: mlb
    integer                                                           , intent(in)    :: mub
    integer                                                           , intent(in)    :: nmmax
    integer                                                           , intent(in)    :: nst
    integer                                                           , intent(in)    :: icx
    integer                                                           , intent(in)    :: icy
    integer                                                           , intent(in)    :: TYPEgradDEFERR  
!
!
! local variables
!
  integer                               :: kADJ
  integer                               :: iter
  integer                               :: KK
  integer                               :: KKy
  integer                               :: k
  integer                               :: nm
  integer                               :: nmu
  integer                               :: nmd  
  integer                               :: num
  integer                               :: ndm
  integer                               :: cont
  integer                               :: nmj
  integer                               :: typeINTER
  integer                               :: nm4(1:4)
  integer                               :: NwAD
  integer                               :: Nedges
  real(fp)                              :: Ns_x
  real(fp)                              :: Ns_y
  real(fp)                              :: dsduu
  real(fp)                              :: dsduuPROV
  real(fp)                              :: dsdvv
  real(fp)                              :: xcCUT(5)
  real(fp)                              :: ycCUT(5)
  real(fp)                              :: xcEDGE(4)
  real(fp)                              :: ycEDGE(4)
  real(fp)                              :: aguv(4)
  real(fp)                              :: xmir
  real(fp)                              :: ymir 
  real(fp)                              :: xint
  real(fp)                              :: yint
  real(fp)                              :: distx
  real(fp)                              :: disty
  real(fp)                              :: PiPj(4)
  real(fp)                              :: xjxi 
  real(fp)                              :: xjxiK(4)
  real(fp)                              :: xjxi2 
  real(fp)                              :: yjyi 
  real(fp)                              :: yjyik(4)  
  real(fp)                              :: yjyi2 
  real(fp)                              :: xjxiyjyi 
  real(fp)                              :: xjxiPiPj 
  real(fp)                              :: xjxiPiPjK(4)  
  real(fp)                              :: yjyiPiPj 
  real(fp)                              :: yjyiPiPjK(4)  
  real(fp)                              :: INVxjxi2 
  real(fp)                              :: xjxiyjyi_X_INVxjxi2
  real(fp)                              :: aa(1)
  real(fp)                              :: bb(1)
  real(fp)                              :: xP1
  real(fp)                              :: xP2
  real(fp)                              :: yP1
  real(fp)                              :: yP2
  real(fp)                              :: xE1
  real(fp)                              :: xE2
  real(fp)                              :: yE1
  real(fp)                              :: yE2
  real(fp)                              :: xi(2)
  real(fp)                              :: yi(2)
  real(fp)                              :: dx
  real(fp)                              :: dy
  real(fp)                              :: dn
  real(fp)                              :: ds
  real(fp)                              :: ds1
  real(fp)                              :: ds1dn
  real(fp)                              :: ds1ds
  real(fp)                              :: butta(1)
  real(fp)                              :: dsduuINT
  real(fp)                              :: dsdvvINT
  real(fp)                              :: maskL
  real(fp)                              :: maskR
  real(fp)                              :: dG
  real(fp)                              :: bb_impL  
  real(fp)                              :: aa_impL  
  real(fp)                              :: bb_impR  
  real(fp)                              :: aa_impR  
  real(fp)                              :: deltaS1_L   
  real(fp)                              :: deltaS1_R   
  real(fp)                              :: deltaS1_C   
  real(fp)                              :: bb_souC  
  real(fp)                              :: aa_souC  
  real(fp)                              :: bb_souR
  real(fp)                              :: aa_souR
  real(fp)                              :: bb_souL
  real(fp)                              :: aa_souL
  real(fp)                              :: bb_souD
  real(fp)                              :: aa_souD 
  real(fp)                              :: bb_souU
  real(fp)                              :: aa_souU  
  real(fp)                              :: deltaS1_Cexp
  real(fp)                              :: deltaS1_Rexp
  real(fp)                              :: deltaS1_Lexp
  real(fp)                              :: denom
  real(fp)                              :: inv_denom  
  real(fp)                              :: xjxiyjyi_X_INVxjxi2_div_denom  
  real(fp), allocatable ,dimension(:)   :: dsduuCENTR  
  real(fp), allocatable ,dimension(:)   :: dsdvvCENTR  
  real(fp), allocatable ,dimension(:)   :: xG_        
  real(fp), allocatable ,dimension(:)   :: yG_        
  real(fp), allocatable ,dimension(:)   :: s0_
  real(fp), allocatable ,dimension(:)   :: s1anal
  real(fp), allocatable ,dimension(:,:) :: normxk
  real(fp), allocatable ,dimension(:,:) :: normyk
  real(fp), allocatable ,dimension(:,:) :: lenWET
  real(fp), allocatable ,dimension(:,:) :: eex
  real(fp), allocatable ,dimension(:,:) :: eey
  real(fp), allocatable ,dimension(:,:) :: alpha
  real(fp), allocatable ,dimension(:,:) :: s1c
  real(fp), allocatable ,dimension(:)   :: dsduuEDGEu
  real(fp), allocatable ,dimension(:)   :: dsdvvEDGEu
  real(fp), allocatable ,dimension(:)   :: dsduuEDGEv
  real(fp), allocatable ,dimension(:)   :: dsdvvEDGEv
  real(fp), allocatable ,dimension(:)   :: dsduuEDGEuOK
  real(fp), allocatable ,dimension(:)   :: dhdx 
  logical , allocatable ,dimension(:,:) :: channelEDGE
  logical , allocatable ,dimension(:,:) :: active
  logical , allocatable ,dimension(:)   :: compSLOPE
!
! executable statements -------------------------------------------------------
!
    sourceU             => gdp%gdimbound%sourceU
    edge6               => gdp%gdimbound%edge6
    periodSURFACE       => gdp%gdimbound%periodSURFACE
    xcorV1              => gdp%gdimbound%xcorV1
    ycorV1              => gdp%gdimbound%ycorV1
    xG_U1               => gdp%gdimbound%xG_U1
    yG_U1               => gdp%gdimbound%yG_U1
    EDGExyBANK          => gdp%gdimbound%EDGExyBANK
    xcorU1              => gdp%gdimbound%xcorU1
    ycorU1              => gdp%gdimbound%ycorU1
    xG_V1               => gdp%gdimbound%xG_V1
    yG_V1               => gdp%gdimbound%yG_V1
    agvv                => gdp%gdimbound%agvv
    ghostu1             => gdp%gdimbound%ghostu1
    ghostv1             => gdp%gdimbound%ghostv1
    gradDEFER3orderDIFF => gdp%gdimbound%gradDEFER3orderDIFF
    analDEFERR          => gdp%gdimbound%analDEFERR
    noCUTfac            => gdp%gdimbound%noCUTfac
    deltaS1cut          => gdp%gdimbound%deltaS1cut
    EXPsouC             => gdp%gdimbound%EXPsouC
    EXPsouR             => gdp%gdimbound%EXPsouR
    EXPsouL             => gdp%gdimbound%EXPsouL
    eeC                 => gdp%gdimbound%eeC
    eeR                 => gdp%gdimbound%eeR
    eeL                 => gdp%gdimbound%eeL
    partIMPLgrad        => gdp%gdimbound%partIMPLgrad
    TYPEpartIMPLgrad    => gdp%gdimbound%TYPEpartIMPLgrad
    !
    allocate(dsduuCENTR(nmlb:nmub))
    allocate(dsdvvCENTR(nmlb:nmub),xG_(nmlb:nmub),yG_(nmlb:nmub),s0_(nmlb:nmub),dhdx(nmlb:nmub),s1anal(nmlb:nmub))
    if (TYPEgradDEFERR/=3) THEN
       !
       !  compute derivatives at baricenter with the least square method. If TYPEgradDEFERR==2 the neumann condition of water surface is also included
       !
       !determine which cells have to be used to compute slope
       allocate(compSLOPE(nmlb:nmub))
       nm4(1)=     -icy !lower 
       nm4(2)= +icx     !right
       nm4(3)=     +icy !upper
       nm4(4)= -icx     !left
       do nm = 1, nmmax
          nm4(1:4) = nm4(1:4) + 1
          !if any of adjacent are cut, slopes are computed with least square
          !otherwise if  adjacent are not cut no modification is needed
          !
          compSLOPE(nm) = kfs_cc(nm    )==0 .or.&  
                       !   kfs_cc(nm4(1))==0 .or.&
                          kfs_cc(nm4(2))==0 .or.&
                        !  kfs_cc(nm4(3))==0 .or.&
                          kfs_cc(nm4(4))==0
       enddo              
       !
       !compute slope
       nm4(1)=     -icy !lower 
       nm4(2)= +icx     !right
       nm4(3)=     +icy !upper
       nm4(4)= -icx     !left
       do nm = 1, nmmax
          eeC(nm,1:3) = 0._fp
          eeR(nm,1:2) = 0._fp
          eeL(nm,2:3) = 0._fp
          EXPsouC(nm) = 0._fp ! to be removed if TYPEpartIMPLgrad==2 is used
          EXPsouR(nm) = 0._fp 
          EXPsouL(nm) = 0._fp          
          nmu = nm + icx
          nm4(1:4) = nm4(1:4) + 1
          !
          dsduuCENTR(nm) = 0._fp !otherwise it could be NaN and mask1 and mask2 below would give NaN
          dsdvvCENTR(nm) = 0._fp !otherwise it could be NaN and mask1 and mask2 below would give NaN
          if (compSLOPE(nm).and.kfs(nm)==1) then !if(kfs_cc(nm    )==0 .or.  (kfs(nm)==1.and.kfs_cc(nmu    )==0) )then ! the second option is only used for  TYPEgradDEFERR==2
             !
             !if (comparereal(poros(nm),thresMERGE_zb).ge.0) then !I compute slope only for non merged cells that are not on the banks
             xjxi      = 0.d0
             xjxiK     = 0.d0
             yjyi      = 0.d0
             yjyiK     = 0.d0
             xjxiyjyi  = 0.d0
             xjxiPiPj  = 0.d0
             xjxiPiPjK = 0.d0
             yjyiPiPj  = 0.d0
             yjyiPiPjK = 0.d0
             xjxi2     = 0.d0
             yjyi2     = 0.d0 
             cont      = 0
             DO k = 1, 4
                nmj = nm4(k)
                !
                ! Maybe here the if kcs==1 is needed, but actually s1 is already peridodic in halo
                ! Note: we can replace kcs==1 with comparereal(aguv(k),0._fp)>0. but we have to pass agvv correctly
                ! (uzd does not have agvv inverted at each call
                ! kcs==1 is only needed to remove cells that have no active part shared with halo 
                ! and therefore the gradient of water surface is not kept in sud during iteration 
                ! (sloping newmann valid only if there is connection)
                !
                IF (kfs(nmj) == 1 .and. kcs(nmj) == 1) then ! .and. agsqs(nmj).gt.thresMERGE_zb) then
                   cont     = cont +1
                   PiPj(k)  = s0(nm) - s0(nmj)
                   xjxiK(k) =  xG_L(nmj) - xG_L(nm)   
                   !
                   ! Note the first 5 can be precomputed but they have to be passed to the subroutine and inverted for each m and n direction 
                   !
                   xjxi         = xjxi + xjxiK(k)
                   xjxi2        = xjxi2 + xjxiK(k)**2      
                   yjyiK(k)     = yG_L(nmj) - yG_L(nm)
                   yjyi         = yjyi + yjyiK(k) 
                   yjyi2        = yjyi2 + yjyiK(k)**2     
                   xjxiyjyi     = xjxiyjyi + xjxiK(k)*yjyiK(k)
                   xjxiPiPjK(k) = xjxiK(k)*PiPj(k)
                   yjyiPiPjK(k) = yjyiK(k)*PiPj(k)
                   xjxiPiPj     = xjxiPiPj + xjxiPiPjK(k)
                   yjyiPiPj     = yjyiPiPj + yjyiPiPjK(k)  
                ENDIF
             ENDDO
             if (TYPEgradDEFERR == 2) then
                !
                ! Add also boundary conditions
                !
                xP1  = xG_L(nm) 
                yP1  = yG_L(nm) 
                !
                ! Two random points in Nx,Ny direction
                !
                xP2  = xG_L(nm) + Nx(nm)*999._fp
                yP2  = yG_L(nm) + Ny(nm)*999._fp
                NwAD = Ndry_GRS(nm)    
                xE1  = INTx_GRS(1,nm)
                yE1  = INTy_GRS(1,nm)                  
                xE2  = INTx_GRS(NwAD,nm) 
                yE2  = INTy_GRS(NwAD,nm)
                !
                ! Look for the intersection of the two lines
                !
                call My_intersec( xP1         ,yP1        ,xP2       ,yP2       ,& 
                                  xE1         ,yE1        ,xE2       ,yE2       ,&
                                  xi(:)       ,yi(:)      ,typeINTER       )
                if (typeINTER==1 .or. typeINTER==3) then
                   xint  = xi(1)
                   yint  = yi(1) 
                   distx = xint-xG_L(nm)! = sqrt((xint-xG_L(nm))**2+(yint-yG_L(nm))**2)
                   disty = yint-yG_L(nm)
                   !
                   ! Compute coordinates of mirrored point
                   !
                   xmir = xint + distx
                   ymir = yint + disty
                else
                   write(*,*) 'Error in deferred_GRADs1',NM,xP1         ,yP1        ,xP2       ,yP2       ,& 
                                                            xE1         ,yE1        ,xE2       ,yE2       ,&
                                                            xi(:)       ,yi(:)      ,typeINTER      
                   !pause
                   stop
                endif
                !
                ! Include mirrored point in the least square sum
                !
                cont      = cont +1
                !PiPj(5)   = 0._fp !s0(nm) - s0(nmj)
                !
                ! Note the first 5 can be precomputed but they have to be passed to the subroutine and inverted  for each m and n direction
                !
                xjxi     = xjxi  +  xmir - xG_L(nm)          
                xjxi2    = xjxi2 + (xmir - xG_L(nm))**2      
                yjyi     = yjyi  +  ymir - yG_L(nm)
                yjyi2    = yjyi2 + (ymir - yG_L(nm))**2     
                xjxiyjyi = xjxiyjyi + (xmir - xG_L(nm))*(ymir - yG_L(nm))
                !xjxiPiPj = xjxiPiPj + (xG_L(nmj) - xG_L(nm))*PiPj(5)
                !yjyiPiPj = yjyiPiPj + (yG_L(nmj) - yG_L(nm))*PiPj(5)
             endif
             if (cont >= 2) then
                INVxjxi2            = 1._fp/xjxi2
                xjxiyjyi_X_INVxjxi2 = xjxiyjyi*INVxjxi2
                denom = (yjyi2-xjxiyjyi * xjxiyjyi_X_INVxjxi2)
                xjxiyjyi_X_INVxjxi2_div_denom = xjxiyjyi_X_INVxjxi2/denom
                inv_denom = 1._fp/denom
                bb(1) =   xjxiPiPj*xjxiyjyi_X_INVxjxi2_div_denom-yjyiPiPj/denom 
                aa(1) = -bb(1)*xjxiyjyi_X_INVxjxi2-xjxiPiPj*INVxjxi2
                !
                if (TYPEgradDEFERR==7.and.partIMPLgrad) then
                   if (TYPEpartIMPLgrad==1) then
                      nmu = nm4(2)
                      nmd = nm4(4)
                      dy = yG(nm)-yG_L(nm) !sbagliato devo fare dyU and dyD also for up and down
                      dx = xG(nm)-xG_L(nm)   
                      !compute implicit terms 
                      bb_impL = xjxiyjyi_X_INVxjxi2_div_denom*xjxiK(4) - yjyiK(4)/denom !*(s1(nm)-s1(nmd) 
                      aa_impL = - bb_impL*xjxiyjyi_X_INVxjxi2 - xjxiK(4)*INVxjxi2         !*(s1(nm)-s1(nmd) 
                      bb_impR = xjxiyjyi_X_INVxjxi2_div_denom*xjxiK(2) - yjyiK(2)/denom !*(s1(nm)-s1(nmu)
                      aa_impR = - bb_impR*xjxiyjyi_X_INVxjxi2 - xjxiK(2)*INVxjxi2         !*(s1(nm)-s1(nmu)
                      deltaS1_L = aa_impL*dx + bb_impL*dy  !twice minus becomes plus
                      deltaS1_R = aa_impR*dx + bb_impR*dy  !twice minus becomes plus
                      deltaS1_C = - deltaS1_L - deltaS1_R
                      !compute explicit terms 
                      bb_souR = xjxiyjyi_X_INVxjxi2_div_denom*(xjxiPiPjK(2)) - (yjyiPiPjK(2))/denom  !on the right, the left part is taken explicit so only  the right explicit stays as source
                      aa_souR = -bb_souR*xjxiyjyi_X_INVxjxi2 - xjxiPiPjK(2)*INVxjxi2                  
                      bb_souL = xjxiyjyi_X_INVxjxi2_div_denom*(xjxiPiPjK(4)) - (yjyiPiPjK(4))/denom  !on the left, the right part is taken explicit so only  the left explicit stays as source
                      aa_souL = -bb_souL*xjxiyjyi_X_INVxjxi2 - xjxiPiPjK(4)*INVxjxi2       
                      !bb_souC = xjxiyjyi_X_INVxjxi2_div_denom*(xjxiPiPjK(2)+xjxiPiPjK(4)) - (yjyiPiPjK(2)+yjyiPiPjK(4))/denom 
                     ! aa_souC = -bb_souC*xjxiyjyi_X_INVxjxi2 - (xjxiPiPjK(2)+xjxiPiPjK(4))*INVxjxi2 
                      bb_souC = bb_souR + bb_souL
                      aa_souC = aa_souR + aa_souL 
                      deltaS1_Cexp = - aa_souC*dx - bb_souC*dy   !this is the explicit component of s1(nm)-s1_(nm) using only values along icx
                      deltaS1_Rexp = - aa_souR*dx - bb_souR*dy   !this is the explicit component of s1(nm)-s1_(nm) using only Right value along icx
                      deltaS1_Lexp = - aa_souL*dx - bb_souL*dy   !this is the explicit component of s1(nm)-s1_(nm) using only left value along icx
                      if (kfs_cc(nm)==0) then !cell nm is cut
                         !implicit
                         eeC(nm,1) = deltaS1_L   !coeff multiplying left    s1 in tridiagonal matrix line nm
                         eeC(nm,2) = deltaS1_C   !coeff multiplying central s1 in tridiagonal matrix line nm                       
                         eeC(nm,3) = deltaS1_R   !coeff multiplying right   s1 in tridiagonal matrix line nm                     
                         eeR(nm,1) = - deltaS1_R !coeff multiplying left    s1 in tridiagonal matrix line nmu
                         eeR(nm,2) =   deltaS1_R !coeff multiplying central s1 in tridiagonal matrix line nmu
                         eeL(nm,3) = - deltaS1_L !coeff multiplying right   s1 in tridiagonal matrix line nmd                       
                         eeL(nm,2) =   deltaS1_L !coeff multiplying central s1 in tridiagonal matrix line nmd                       
                         !explicit
                         EXPsouC(nm) = deltaS1_Cexp
                         EXPsouR(nm) = deltaS1_Rexp
                         EXPsouL(nm) = deltaS1_Lexp  
                      else
                         eeC(nm,1:3) = 0._fp
                         eeR(nm,1:2) = 0._fp
                         eeL(nm,2:3) = 0._fp 
                         EXPsouC(nm) = 0._fp 
                         EXPsouR(nm) = 0._fp 
                         EXPsouL(nm) = 0._fp 
                      endif
                   elseif (TYPEpartIMPLgrad==2) then
                      nmu = nm4(2)
                     ! nmuu = nmu + icx
                      nmd = nm4(4)
                      dy = yG(nm)-yG_L(nm)
                      dx = xG(nm)-xG_L(nm)   
                      !compute implicit terms 
                      bb_impL = xjxiyjyi_X_INVxjxi2_div_denom*xjxiK(4) - yjyiK(4)/denom !*(s1(nm)-s1(nmd) 
                      aa_impL = - bb_impL*xjxiyjyi_X_INVxjxi2 - xjxiK(4)*INVxjxi2         !*(s1(nm)-s1(nmd) 
                      bb_impR = xjxiyjyi_X_INVxjxi2_div_denom*xjxiK(2) - yjyiK(2)/denom !*(s1(nm)-s1(nmu)
                      aa_impR = - bb_impR*xjxiyjyi_X_INVxjxi2 - xjxiK(2)*INVxjxi2         !*(s1(nm)-s1(nmu)
                      deltaS1_L = - aa_impL*dx - bb_impL*dy  !twice minus becomes plus. 
                      deltaS1_R = - aa_impR*dx - bb_impR*dy  !twice minus becomes plus deltaS1 is the factor used to obtain  s1(nm)-s1_(nm), in fact  deltaS1_C*s1(nm) + deltaS1_R*s1(nmu)+ explicit terms = s1(nm)-s1_(nm)
                      !deltaS1_C = - deltaS1_R
                      !compute explicit terms 
                      bb_souR = xjxiyjyi_X_INVxjxi2_div_denom*(xjxiPiPjK(2)) - (yjyiPiPjK(2))/denom  !on the right, the left part is taken explicit so only  the right explicit stays as source
                      aa_souR = -bb_souR*xjxiyjyi_X_INVxjxi2 - xjxiPiPjK(2)*INVxjxi2                  
                      bb_souL = xjxiyjyi_X_INVxjxi2_div_denom*(xjxiPiPjK(4)) - (yjyiPiPjK(4))/denom  !on the left, the right part is taken explicit so only  the left explicit stays as source
                      aa_souL = -bb_souL*xjxiyjyi_X_INVxjxi2 - xjxiPiPjK(4)*INVxjxi2       
                      deltaS1_Rexp = - aa_souR*dx - bb_souR*dy   !this is the explicit component of s1(nm)-s1_(nm) using only Right value along icx
                      deltaS1_Lexp = - aa_souL*dx - bb_souL*dy   !this is the explicit component of s1(nm)-s1_(nm) using only left value along icx
                      if (kfs_cc(nm)==0) then !cell nm is cut
                         !implicit
                         eeC(nm,1) = deltaS1_R  !coeff multiplying s1(nm) in momentum equation descretized for velocity point nm
                         eeC(nm,2) = deltaS1_L  !coeff multiplying s1(nm) in momentum equation descretized for velocity point nmd     
                         eeR(nm,1:2) = 0._fp ! to be removed
                         eeL(nm,2:3) = 0._fp ! to be removed                          
                         !explicit
                         EXPsouC(nm) = 0._fp ! to be removed
                         EXPsouR(nm) = deltaS1_Rexp !explicit contribution of s1(nm) to momentum equation descretized for velocity point nm
                         EXPsouL(nm) = deltaS1_Lexp !explicit contribution of s1(nm) to momentum equation descretized for velocity point nmd  
                      else
                         eeC(nm,1:3) = 0._fp
                         eeR(nm,1:2) = 0._fp ! to be removed
                         eeL(nm,2:3) = 0._fp ! to be removed
                         EXPsouC(nm) = 0._fp ! to be removed
                         EXPsouR(nm) = 0._fp 
                         EXPsouL(nm) = 0._fp 
                      endif   
                  elseif (TYPEpartIMPLgrad==3) then
                      nmu = nm4(2)
                     ! nmuu = nmu + icx
                      nmd = nm4(4)
                      dy = yG(nm)-yG_L(nm)
                      dx = xG(nm)-xG_L(nm)   
                      !compute implicit terms 
                      bb_impL = xjxiyjyi_X_INVxjxi2_div_denom*xjxiK(4) - yjyiK(4)/denom !*(s1(nm)-s1(nmd) 
                      aa_impL = - bb_impL*xjxiyjyi_X_INVxjxi2 - xjxiK(4)*INVxjxi2         !*(s1(nm)-s1(nmd) 
                      bb_impR = xjxiyjyi_X_INVxjxi2_div_denom*xjxiK(2) - yjyiK(2)/denom !*(s1(nm)-s1(nmu)
                      aa_impR = - bb_impR*xjxiyjyi_X_INVxjxi2 - xjxiK(2)*INVxjxi2         !*(s1(nm)-s1(nmu)
                      deltaS1_L = + aa_impL*dx + bb_impL*dy  ! 
                      deltaS1_R = + aa_impR*dx + bb_impR*dy  !  deltaS1 is the factor used to obtain  s1(nm)-s1_(nm), in fact deltaS1_C*s1(nm) +  deltaS1_R*s1(nmu)+ explicit terms = s1(nm)-s1_(nm)
                      !deltaS1_C = - deltaS1_R
                      !compute explicit terms 
                      bb_souU = xjxiyjyi_X_INVxjxi2_div_denom*(xjxiPiPjK(3)) - (yjyiPiPjK(3))/denom  !on the TOP
                      aa_souU = -bb_souU*xjxiyjyi_X_INVxjxi2 - xjxiPiPjK(3)*INVxjxi2          
                      bb_souD = xjxiyjyi_X_INVxjxi2_div_denom*(xjxiPiPjK(1)) - (yjyiPiPjK(1))/denom  !on the BOTTOM
                      aa_souD = -bb_souD*xjxiyjyi_X_INVxjxi2 - xjxiPiPjK(1)*INVxjxi2                         
                      bb_souR = xjxiyjyi_X_INVxjxi2_div_denom*(xjxiPiPjK(2)) - (yjyiPiPjK(2))/denom  !on the right, the left part is taken explicit so only  the right explicit stays as source
                      aa_souR = -bb_souR*xjxiyjyi_X_INVxjxi2 - xjxiPiPjK(2)*INVxjxi2                  
                      bb_souL = xjxiyjyi_X_INVxjxi2_div_denom*(xjxiPiPjK(4)) - (yjyiPiPjK(4))/denom  !on the left, the right part is taken explicit so only  the left explicit stays as source
                      aa_souL = -bb_souL*xjxiyjyi_X_INVxjxi2 - xjxiPiPjK(4)*INVxjxi2       
                      deltaS1_Rexp = - aa_souL*dx - bb_souL*dy - aa_souU*dx - bb_souU*dy - aa_souD*dx - bb_souD*dy  !this is the explicit component of s1_ (nm)-s1(nm) using all values except Right value  
                      deltaS1_Lexp = - aa_souR*dx - bb_souR*dy - aa_souU*dx - bb_souU*dy - aa_souD*dx - bb_souD*dy  !this is the explicit component of s1_ (nm)-s1(nm) using only left value along icx
                      if (kfs_cc(nm)==0) then !cell nm is cut
                         !implicit
                         eeC(nm,1) = deltaS1_R  !coeff multiplying s1(nm) in momentum equation descretized for velocity point nm
                         eeC(nm,2) = deltaS1_L  !coeff multiplying s1(nm) in momentum equation descretized for velocity point nmd     
                         eeR(nm,1:2) = 0._fp ! to be removed
                         eeL(nm,2:3) = 0._fp ! to be removed                          
                         !explicit
                         EXPsouC(nm) = 0._fp ! to be removed
                         EXPsouR(nm) = deltaS1_Rexp !explicit contribution of s1(nm) to momentum equation descretized for velocity point nm
                         EXPsouL(nm) = deltaS1_Lexp !explicit contribution of s1(nm) to momentum equation descretized for velocity point nmd  
                      else
                         eeC(nm,1:3) = 0._fp
                         eeR(nm,1:2) = 0._fp ! to be removed
                         eeL(nm,2:3) = 0._fp ! to be removed
                         EXPsouC(nm) = 0._fp ! to be removed
                         EXPsouR(nm) = 0._fp 
                         EXPsouL(nm) = 0._fp 
                      endif  
                   endif
                endif
             else
                if (kfs(nm)==1 .and. kcs(nm) == 1) then
                   write(*,*) 'Warning: only 2 points for computing surface slope correction'
                endif
                bb(1) = 0._fp
                aa(1) = 0._fp
             endif
             !
             ! Transform from x,y slope to n,m slopes
             !
             !CALL ROTATEback(aa,bb,PSIx(nm),PSIy(nm),1,dsduuCENTR(nm),dsdvvCENTR(nm))   
             dsduuCENTR(nm) = aa(1)
             dsdvvCENTR(nm) = bb(1)
             !write(33669911,'(2i8,15f25.15)') nst,nm,aa(1),bb(1)
           !endif
          endif
       enddo 
       deallocate(compSLOPE)
       if (periodSURFACE) then
          call periodicGRADs1(dsduuCENTR, dsdvvCENTR, nlb, mlb, nub, mub, gdp)  
       endif
    elseif( TYPEgradDEFERR == 3) THEN
       !
       ! Compute gradients iteratively as in Peric and Felziger notes
       ! To be corrected, it is wrong, quantities in k should be rotated somehow when called in n since the subroutine is called both in m and n directions
       !
       allocate(normxk      (5  ,nmlb:nmub))
       allocate(normyk      (1:5,nmlb:nmub))
       allocate(channelEDGE (1:4,nmlb:nmub))
       allocate(active      (1:4,nmlb:nmub))
       allocate(lenWET      (5  ,nmlb:nmub))
       allocate(eex         (5  ,nmlb:nmub))
       allocate(eey         (5  ,nmlb:nmub))
       allocate(alpha       (5  ,nmlb:nmub))
       allocate(s1c         (5  ,nmlb:nmub))
       allocate(dsduuEDGEu  (    nmlb:nmub))
       allocate(dsdvvEDGEu  (    nmlb:nmub))
       allocate(dsduuEDGEuOK(    nmlb:nmub))
       allocate(dsduuEDGEv  (    nmlb:nmub))
       allocate(dsdvvEDGEv  (    nmlb:nmub))
       !
       if (icy==1) then
          KK  = 2
          KKy = 3
       else
          KK  = 3
          KKy = 2
       endif
       nm4(1)=     -icy !lower 
       nm4(2)= +icx     !right
       nm4(3)=     +icy !upper
       nm4(4)= -icx     !left
       do nm = 1, nmmax
          nm4(1:4) = nm4(1:4) + 1        
          if(kfs_cc(nm    )==0 )then 
             xcEDGE(1) = (xcor(nm-icy)-xcor(nm-icy-icx))/2._fp
             ycEDGE(1) = (ycor(nm-icy)-ycor(nm-icy-icx))/2._fp
             xcEDGE(2) = (xcor(nm)-xcor(nm-icy))/2._fp
             ycEDGE(2) = (ycor(nm)-ycor(nm-icy))/2._fp
             xcEDGE(3) = (xcor(nm)-xcor(nm-icx))/2._fp
             ycEDGE(3) = (ycor(nm)-ycor(nm-icx))/2._fp
             xcEDGE(4) = (xcor(nm-icx)-xcor(nm-icy-icx))/2._fp
             if (icy==1) then
                normxk(1,nm)= -ETAx(nm)
                normyk(1,nm)= -ETAy(nm)
                normxk(2,nm)=  PSIx(nm)
                normyk(2,nm)=  PSIy(nm)
                normxk(3,nm)=  ETAx(nm)
                normyk(3,nm)=  ETAy(nm)
                normxk(4,nm)= -PSIx(nm)
                normyk(4,nm)= -PSIy(nm)
             else !invert x with y
                normxk(1,nm)= -ETAy(nm)
                normyk(1,nm)= -ETAx(nm)
                normxk(2,nm)=  PSIy(nm)
                normyk(2,nm)=  PSIx(nm)
                normxk(3,nm)=  ETAy(nm)
                normyk(3,nm)=  ETAx(nm)
                normxk(4,nm)= -PSIy(nm)
                normyk(4,nm)= -PSIx(nm)
             endif
            ! aguv(1) = agvv(nm - icy)
            ! aguv(2) = aguu(nm)
            ! aguv(3) = agvv(nm)
            ! aguv(4) = aguu(nm - icx)
             DO k=1,4
                nmj = nm4(k)    
                channelEDGE(k,nm) = (EDGEtypeBANK (k,nm) == 0 .or. EDGEtypeBANK (k,nm) == 3)  
                active(k,nm) = kfs(nmj)==1.and.channelEDGE(k,nm)  !
                IF (active(k,nm)) then
                   xP1  = xG_L(nm) 
                   yP1  = yG_L(nm)  
                   xP2  = xG_L(nmj)  
                   yP2  = yG_L(nmj)  
                   NwAD = Ndry_GRS(nm)    
                   if (EDGEtypeBANK (k,nm) == 0) then !cut edge
                      if ( (comparereal(EDGExBANK(nm,KK,2),xcor(nm)).eq.0).and.(comparereal(EDGEyBANK(nm,KK,2),ycor(nm)).eq.0) ) then    !(2,2,1) =>  (edge=2,2:second extreme,x coord).  EDGE K=2 for U. Second extreme is always on the vertex.
                        !it is coincident with the upper vertex, the water is below
                         xE1  = xcor(nm-icy)
                         yE1  = ycor(nm-icy)         
                      else
                         xE1  = xcor(nm)
                         yE1  = ycor(nm)                   
                      endif
                      xE2  = EDGExBANK(nm,KK,1) !1st extreme is never on the vertex
                      yE2  = EDGEyBANK(nm,KK,1) !1st extreme is never on the vertex
                      if ((comparereal(xE1,xE2).eq.0).and.(comparereal(yE1,yE2).eq.0) ) then !fully water ( it is cut but they coincide)
                         xE1  = xcor(nm)
                         yE1  = ycor(nm)   
                         xE2  = xcor(nm-icy)
                         yE2  = ycor(nm-icy)  
                      endif  
                   else !fully water
                      xE1  = xcor(nm)
                      yE1  = ycor(nm)   
                      xE2  = xcor(nm-icy)
                      yE2  = ycor(nm-icy)  
                   endif
                   lenWET(k,nm) = sqrt((xE1-xE2)**2+(yE1-yE2))**2
                   !I look for the intersection of the two lines
                   call My_intersec( xP1         ,yP1        ,xP2       ,yP2       ,& 
                                     xE1         ,yE1        ,xE2       ,yE2       ,&
                                     xi(:)       ,yi(:)      ,typeINTER       )
                   if (typeINTER==1.or.typeINTER==3) then
                      xint = xi(1)
                      yint = yi(1) 
                      eex(k,nm) = xcEDGE(k) - xint 
                      eey(k,nm) = ycEDGE(k) - yint
                   else
                      write(*,*) 'Error in deferred_GRADs1',NM,xP1         ,yP1        ,xP2       ,yP2       ,& 
                                                               xE1         ,yE1        ,xE2       ,yE2       ,&
                                                               xi(:)       ,yi(:)      ,typeINTER      
                      !pause
                      stop
                   endif
                   ds    = sqrt((xG_L(nm)-xG_L(nmj))**2 + (yG_L(nm)-yG_L(nmj))**2) !should be precomputed
                   ds1   = sqrt((xG_L(nm)-xint     )**2 + (yG_L(nm)-yint     )**2) !should be precomputed
                   alpha(k,nm) = ds1/ds 
                elseif (channelEDGE(k,nm)) then !there is a wall since kfs(nmj) is not zero                  
                   xP1  = xG_L(nm) 
                   yP1  = yG_L(nm) 
                   xP2  = xG_L(nm) + normxk(k,nm)*999._fp  !random point in Nx,Ny direction
                   yP2  = yG_L(nm) + normyk(k,nm)*999._fp  !random point in Nx,Ny direction 
                   if (EDGEtypeBANK (k,nm) == 0) then !cut edge
                      if ( (comparereal(EDGExBANK(nm,KK,2),xcor(nm)).eq.0).and.(comparereal(EDGEyBANK(nm,KK,2),ycor(nm)).eq.0) ) then    !(2,2,1) =>  (edge=2,2:second extreme,x coord).  EDGE K=2 for U. Second extreme is always on the vertex.
                        !it is coincident with the upper vertex, the water is below
                         xE1  = xcor(nm-icy)
                         yE1  = ycor(nm-icy)         
                      else
                         xE1  = xcor(nm)
                         yE1  = ycor(nm)                   
                      endif
                      xE2  = EDGExBANK(nm,KK,1) !1st extreme is never on the vertex
                      yE2  = EDGEyBANK(nm,KK,1) !1st extreme is never on the vertex
                      if ((comparereal(xE1,xE2).eq.0).and.(comparereal(yE1,yE2).eq.0) ) then !fully water ( it is cut but they coincide)
                         xE1  = xcor(nm)
                         yE1  = ycor(nm)   
                         xE2  = xcor(nm-icy)
                         yE2  = ycor(nm-icy)  
                      endif  
                   else !fully water
                      xE1  = xcor(nm)
                      yE1  = ycor(nm)   
                      xE2  = xcor(nm-icy)
                      yE2  = ycor(nm-icy)  
                   endif
                   lenWET(k,nm) = sqrt((xE1-xE2)**2+(yE1-yE2))**2
                   xCcut(k) = (EDGExBANK(nm,k,1)+EDGExBANK(nm,k,2))*0.5_fp !midpoint cutcell interface
                   yCcut(k) = (EDGEyBANK(nm,k,1)+EDGEyBANK(nm,k,2))*0.5_fp !midpoint cutcell interface
                   !I look for the intersection of the two lines
                   call My_intersec( xP1         ,yP1        ,xP2       ,yP2       ,& 
                                     xE1         ,yE1        ,xE2       ,yE2       ,&
                                     xi(:)       ,yi(:)      ,typeINTER       )
                   if (typeINTER==1.or.typeINTER==3) then
                      xint = xi(1)
                      yint = yi(1) 
                      eex(k,nm) = xCcut(k) - xint 
                      eey(k,nm) = xCcut(k) - yint
                   else
                      write(*,*) 'Error in deferred_GRADs1',NM,xP1         ,yP1        ,xP2       ,yP2       ,& 
                                                               xE1         ,yE1        ,xE2       ,yE2       ,&
                                                               xi(:)       ,yi(:)      ,typeINTER      
                      !pause
                      stop
                   endif
                endif
             ENDDO
             !add interface edge
             xP1  = xG_L(nm) 
             yP1  = yG_L(nm) 
             xP2  = xG_L(nm) + Nx(nm)*999._fp  !random point in Nx,Ny direction
             yP2  = yG_L(nm) + Ny(nm)*999._fp  !random point in Nx,Ny direction
             normxk(5,nm)= Nx(nm)
             normyk(5,nm)= Ny(nm)
             NwAD = Ndry_GRS(nm)    
             xE1  = INTx_GRS(1,nm)
             yE1  = INTy_GRS(1,nm)                  
             xE2  = INTx_GRS(NwAD,nm) 
             yE2  = INTy_GRS(NwAD,nm) 
             xCcut(5) = (INTx_GRS(1,nm)+INTx_GRS(NwAD,nm))*0.5_fp !midpoint cutcell interface
             yCcut(5) = (INTy_GRS(1,nm)+INTy_GRS(NwAD,nm))*0.5_fp !midpoint cutcell interface
             !I look for the intersection of the two lines
             call My_intersec( xP1         ,yP1        ,xP2       ,yP2       ,& 
                               xE1         ,yE1        ,xE2       ,yE2       ,&
                               xi(:)       ,yi(:)      ,typeINTER       )
             if (typeINTER==1.or.typeINTER==3) then
                xint = xi(1)
                yint = yi(1) 
                eex(k,nm) = xCcut(5) - xint 
                eey(k,nm) = xCcut(5) - yint
             else
                write(*,*) 'Error in deferred_GRADs1',NM,xP1         ,yP1        ,xP2       ,yP2       ,& 
                                                         xE1         ,yE1        ,xE2       ,yE2       ,&
                                                         xi(:)       ,yi(:)      ,typeINTER      
                !pause
                stop
             endif
          endif !if kfs_cc
       enddo !nm
       !
       ! start iterations
       !
       do iter = 1,10
          do nm = 1,nmmax
             nm4(1:4) = nm4(1:4) + 1        
             if(kfs_cc(nm    )==0 )then          
                Nedges = 0
                DO k=1,4
                   nmj = nm4(k)    
                   IF (active(k,nm)) then
                      Nedges = Nedges + 1
                      s1c(k,nm) = s0(nm) * alpha(k,nm) + s0(nmj) * (1._fp - alpha(k,nm))
                   elseif (channelEDGE(k,nm)) then !there is a wall since kfs(nmj) is not zero         
                      !mirroring after wall
                      Nedges = Nedges + 1
                      s1c(k,nm) = s0(nm)
                   endif
                enddo
                k=5
                s1c(k,nm) = s0(nm)
             endif
          enddo
          !
          ! compute cell centered slope
          !
          do nm = 1,nmmax
             if(kfs_cc(nm    )==0 )then  
                dsduuCENTR(nm) = 0._fp
                dsdvvCENTR(nm) = 0._fp
                DO k=1,5             
                   dsduuCENTR(nm) = dsduuCENTR(nm) + s1c(k,nm)*lenWET(k,nm)*(normxk(KK,nm) *normxk(k,nm) + normyk(KK,nm) *normyk(k,nm))
                   dsdvvCENTR(nm) = dsdvvCENTR(nm) + s1c(k,nm)*lenWET(k,nm)*(normxk(KKy,nm)*normxk(k,nm) + normyk(KKy,nm)*normyk(k,nm)) 
                ENDDO
                dsduuCENTR(nm) = dsduuCENTR(nm)/(agsqs(nm)*gsqs(nm))
                dsdvvCENTR(nm) = dsdvvCENTR(nm)/(agsqs(nm)*gsqs(nm))
             endif
          enddo
          !
          ! compute edge centered slope
          !
          do nm = 1,nmmax
             nm4(1:4) = nm4(1:4) + 1  
             ! U point
             nmu = nm+icx    
             if(kfs_cc(nm    )==0 .or. kfs_cc(nmu    )==0)then     
                if (active(KK,nm)) then
                   dsduuEDGEu(nm) = dsduuCENTR(nm) * alpha(KK,nm) + dsduuCENTR(nmu) * (1._fp - alpha(KK,nm)) !dsduu at u point
                   dsdvvEDGEu(nm) = dsdvvCENTR(nm) * alpha(KK,nm) + dsdvvCENTR(nmu) * (1._fp - alpha(KK,nm)) !dsdvv at u point
                elseif (channelEDGE(k,nm))  then
                   dsduuEDGEu(nm) = dsduuCENTR(nm) !I should actually invert   slope normal to wall
                   dsdvvEDGEu(nm) = dsdvvCENTR(nm) !I should actually invert   slope normal to wall
                endif
                s1c(KK,nm) = s1c(KK,nm) + dsduuEDGEu(nm)*eex(KK,nm) +  dsdvvEDGEu(nm)*eey(KK,nm)
                kADJ = edge6(KK+2)
                s1c(kADJ,nm) = s1c(KK,nm)
             endif
             ! V point
             num = nm+icy    
             if(kfs_cc(nm    )==0 .or. kfs_cc(num    )==0)then     
                if (active(KKy,nm)) then                 
                   dsduuEDGEv(nm) = dsduuCENTR(nm) * alpha(KKy,nm) + dsduuCENTR(num) * (1._fp - alpha(KKy,nm)) !dsduu at v point
                   dsdvvEDGEv(nm) = dsdvvCENTR(nm) * alpha(KKy,nm) + dsdvvCENTR(num) * (1._fp - alpha(KKy,nm)) !dsdvv at v point
                   s1c(KKy,nm) = s1c(KKy,nm) + dsduuEDGEv(nm)*eex(KKy,nm) +  dsdvvEDGEv(nm)*eey(KKy,nm)
                elseif (channelEDGE(k,nm))  then
                   dsduuEDGEu(nm) = dsduuCENTR(nm) !I should actually invert   slope normal to wall
                   dsdvvEDGEu(nm) = dsdvvCENTR(nm) !I should actually invert   slope normal to wall
                   s1c(KKy,nm) = s1c(KKy,nm) + dsduuEDGEu(nm)*eex(KKy,nm) +  dsdvvEDGEu(nm)*eey(KKy,nm)
                endif
                kADJ = edge6(KKy+2)
                s1c(kADJ,nm) = s1c(KKy,nm)
             endif
             !interface
             k=5 
             if(kfs_cc(nm    )==0) then
                dsduuINT = dsduuCENTR(nm) ! I should actually invert   slope normal to wall
                dsdvvINT = dsdvvCENTR(nm) ! I should actually invert   slope normal to wall
                s1c(KK,nm) = s1c(KK,nm) + dsduuINT*eex(KK,nm) +  dsdvvINT*eey(KK,nm)
                kADJ = edge6(KK+2)
                s1c(kADJ,nm) = s1c(KK,nm)
             endif
          enddo
       enddo
       if (icy==1) then
          do nm = 1,nmmax
             CALL ROTATE(dsduuEDGEu(nm) ,dsdvvEDGEu(nm) ,PSIx(nm),PSIy(nm),1,dsduuEDGEuOK(nm) ,butta(1) ) 
          enddo
       else
          do nm = 1,nmmax
             CALL ROTATE(dsduuEDGEu(nm) ,dsdvvEDGEu(nm) ,PSIy(nm),PSIx(nm),1,dsduuEDGEuOK(nm) ,butta(1) )  ! DA VERIFICARE
          enddo
       endif
       !
       deallocate(normxk,normyk,channelEDGE,active,&
                lenWET,eex,eey,alpha,s1c,&
                dsduuEDGEu,dsdvvEDGEu,dsduuEDGEuOK,&
                dsduuEDGEv,dsdvvEDGEv)
    endif
    !
    !  compute water surface on the mid point of the eta-edge passing for the barycenter
    !
    if (TYPEgradDEFERR<4) then
       do nm=1,nmmax
          if(kfs_cc(nm    )==0 )then
             !I take the two "infinite" lines, one passing per xG_L,yG_L and along eta, and one passing per xG,yG and along psi
             xP1  = xG_L(nm) + ETAx(nm) !here at each call of the subr in m and n, xG,yG and xG_L,yG_L are inverted, but not ETAx,y and PSIx,y
             yP1  = yG_L(nm) + ETAy(nm)
             xP2  = xG  (nm) + PSIx(nm) 
             yP2  = yG  (nm) + PSIy(nm)
             !I look for the intersection of the two lines
             call My_intersec( xG_L(nm)    ,yG_L(nm)   ,xP1       ,yP1       ,& 
                               xG  (nm)    ,yG  (nm)   ,xP2       ,yP2       ,&
                               xi(:)       ,yi(:)      ,typeINTER       )
             if (typeINTER==1.or.typeINTER==3) then
                xG_(nm) = xi(1)
                yG_(nm) = yi(1) 
             else
                write(*,*) 'Error in deferred_GRADs1',NM,typeINTER,xG_L(nm)    ,yG_L(nm)   ,xP1       ,yP1       ,& 
                               xG  (nm)    ,yG  (nm)   ,xP2       ,yP2       ,&
                               xi(:)       ,yi(:)
                !pause
                stop
             endif
             dy = yG_(nm)-yG_L(nm)
             dx = xG_(nm)-xG_L(nm) 
            ! num = nm + icy
            ! ndm = nm -icy
            ! !overwrite least square
            ! if (kfs(ndm)==1) then
            !    dsdvvCENTR(nm)  = (s0(nm)-s0(ndm))/(yG_L(nm)-yG_L(ndm))
            ! elseif (kfs(num)==1) then
            !    dsdvvCENTR(nm)  = (s0(num)-s0(nm))/(yG_L(num)-yG_L(nm))
            ! else
            !    dsdvvCENTR(nm) = 0._fp
            ! endif         
             s0_(nm) = s0(nm)   + dx*dsduuCENTR(nm) + dy*dsdvvCENTR(nm) 
          else
             xG_(nm) = xG(nm)
             yG_(nm) = yG(nm)
             s0_(nm) = s0(nm) 
          endif
       enddo  
    elseif (TYPEgradDEFERR==6.or.TYPEgradDEFERR==7) then          
       do nm=1,nmmax
          xG_(nm) = xG(nm)
          yG_(nm) = yG(nm)             
          if(kfs_cc(nm    )==0 )then
             dy = yG_(nm)-yG_L(nm)
             dx = xG_(nm)-xG_L(nm)        
             s0_(nm) = s0(nm)   + dx*dsduuCENTR(nm) + dy*dsdvvCENTR(nm) 
          else
             s0_(nm) = s0(nm) 
          endif
       enddo  
    endif
    if (TYPEgradDEFERR==7) then
       if (.not.partIMPLgrad) then
          do nm=1,nmmax      
             deltaS1cut(nm) = 0._fp 
             if(kfs_cc(nm    )==0 )then       
                deltaS1cut(nm) =   s0(nm) - s0_(nm)
              !  if (mod(nm,100)==0) write(*,*) 'correggi deltaS1cut '
                write(11992288,'(2i8,15f25.15)') nst,nm,s0(nm),s0_(nm),s0(nm) - s0_(nm)
             endif
          enddo  
       endif
       return
    endif
    !!
    !! Check if s0_ is second order accurate
    !!
    !if (icy==1) then
    !   call FORCE_s1_analCIRC_centrCELL(s1anal  ,s0_     ,xG     ,yG     ,kfs      ,kcs      ,nmlb      ,nmub      ,nmmax     ,nst)
    !else
    !   call FORCE_s1_analCIRC_centrCELL(s1anal  ,s0_     ,yG     ,xG     ,kfs      ,kcs      ,nmlb      ,nmub      ,nmmax     ,nst) 
    !endif
    !!
    !! Check if du/dx is second order accurate
    !!
    !! s0_ = s1anal
    !! if (periodSURFACE) call WATERlevelPERIOD(s0_,dps,icx,nlb,nub,mlb,mub,1)!da canc uses 1
    !do nm=1,nmmax   
    !   !if (mod(nm,100)==0) write(*,*) 'cancellare s0_ = s1anal'
    !   dhdx(nm) = (s0_(nm+icx) -s0_(nm))/gvu(nm) ! debug, to be removed
    !enddo
    !if (icy==1) then
    !   call FORCE_ds1dx_analCIRC_verifica(dhdx        ,kfs_cc      ,PSIx        ,PSIy     ,guu         ,aguu        ,&                        
    !                                  &   xcorV1      ,ycorV1      ,xG_U1       ,yG_U1    ,EDGExyBANK  ,xcor        ,&   
    !                                  &   ycor        ,nmlb        ,nmub        ,nmmax    ,icx         ,icy         ,&
    !                                  &   nst         ,kfu         ,kcs         ,ghostu1  ,0           ,gdp)
    !else
    !   call FORCE_ds1dx_analCIRC_verifica(dhdx        ,kfs_cc      ,ETAy        ,ETAx     ,gvv         ,agvv        ,& !guu and aguu are gvv and  agvv                        .ETAy        ,ETAx  inverted
    !                                  &   xcorU1      ,ycorU1      ,xG_V1       ,yG_V1    ,EDGExyBANK  ,ycor        ,&   !ycor xcor inverted
    !                                  &   xcor        ,nmlb        ,nmub        ,nmmax    ,icx         ,icy         ,&
    !                                  &   nst         ,kfu         ,kcs         ,ghostv1  ,0           ,gdp)!0: do not move to center of active edge
    !endif
    ! 
    ! Compute correct gradient and deferred source term
    !
    do nm=1,nmmax
       nmu = nm + icx
       if(comparereal(aguu(nm),0._fp).gt.0.and.(kfs_cc(nm )==0 .or.kfs_cc(nmu)==0.or.analDEFERR)) then
          !
          ! Compute gradient along s
          !
          dG = sqrt((xG_L(nm)-xG_L(nmu))**2 + (yG_L(nm)-yG_L(nmu))**2)
          if (noCUTfac) then
             ds = gvu(nm)
          else
             ds = dG
          endif
          ds1ds = (s0(nmu) - s0(nm))/ds
          if( TYPEgradDEFERR==3) then !not working
             ds1dn = dsduuEDGEuOK(nm)
          elseif( TYPEgradDEFERR==4.OR.TYPEgradDEFERR==5) then
             !
             ! Average of adjacent cell centered slopes
             !
             if (kfs(nm )==1) then !if (kfs_cc(nm )==0) then
                maskL = 1
             else
                maskL = 0
             endif
             if (kfs(nmu)==1) then !if (kfs_cc(nmu)==0) then
                maskR = 1
             else
                maskR = 0
             endif
             ds1dn = (dsduuCENTR(nm)*maskL+dsduuCENTR(nmu)*maskR)/(maskR+maskL)
          else
             dn = sqrt((xG_(nm) -xG_(nmu))**2  + (yG_(nm) -yG_(nmu))**2)
             ds1dn = (s0_(nmu) - s0_(nm))/dn
          endif
          IF (gradDEFER3orderDIFF) then
             !
             ! Add third order numerical smoothing as in Muzaferija and Gosman (1997)
             !
             if (kfs(nm )==1) then !if (kfs_cc(nm )==0) then
                maskL = 1
             else
                maskL = 0
             endif
             if (kfs(nmu)==1) then !if (kfs_cc(nmu)==0) then
                maskR = 1
             else
                maskR = 0
             endif            
             dsduuPROV = ds1dn 
             dsdvv     = (dsdvvCENTR(nm)*maskL+dsdvvCENTR(nmu)*maskR)/(maskR+maskL)
             Ns_x      = (xG_L(nmu)-xG_L(nm))/dG
             Ns_y      = (yG_L(nmu)-yG_L(nm))/dG
             !dsduu     = dsduuPROV + (ds1ds-(dsduuPROV*Ns_x + dsdvv*Ns_y))*PSIx(nm) !IN 1995 PAPER Muzaferija uses  PSIx instead of Ns_x
             dsduu     = dsduuPROV + (ds1ds-(dsduuPROV*Ns_x + dsdvv*Ns_y))*Ns_x
             ds1dn     = dsduu 
          endif         
          !write(33669912,'(2i8,15f25.15)') nst,nm,ds1dn,ds1ds,ds1dn - ds1ds,dG,s0(nmu),s0(nm)
          sourceU(nm) = ds1dn - ds1ds
          if (analDEFERR) then
             sourceU(nm) = -gradS1anal(nm)/9.81_fp - ds1ds
          endif
          if (noCUTfac) then
             cutFAC(nm) = 1._fp
          else
             cutFAC(nm)  = ds / gvu(nm)
          endif
          !if ((icy==1.and.ghostU1(nm) ==0).or.(icy/=1.and.ghostV1(nm) ==0)) then
          !write(1010102,'(i8,13f25.15)') nm,-gradS1anal(nm)/9.81_fp,ds1dn,ds1dn+gradS1anal(nm)/9.81_fp
          !write(1010101,'(i8,13f25.15)') nm,sourceU(nm),cutFAC(nm),ds1dn,ds1ds,Ns_x,Ns_y,dsduu,dsduuPROV,dsdvv,dsdvvCENTR(nm),dsdvvCENTR(nmu)
       else
          cutFAC(nm)  = 1._fp
          sourceU(nm) = 0._fp
       endif
    enddo
    !   
    deallocate(dsduuCENTR)
    deallocate(dsdvvCENTR)
    deallocate(xG_)
    deallocate(yG_)
    deallocate(s0_)
    deallocate(dhdx)
    !   
end subroutine deferred_GRADs1
