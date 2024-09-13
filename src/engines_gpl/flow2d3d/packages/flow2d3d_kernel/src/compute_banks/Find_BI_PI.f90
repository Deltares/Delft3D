subroutine Find_BI_PI(gsqs,kfs,kcs,s1,u1,v1,dps,lunscr,Irov,mmax,nmax,nmaxus,kmax,nst,nlb,nub,mlb,mub,nmlb,nmub,zmodel,gdp)
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
!   Function: Find boundary interception drawing a line from to ghost cell normal to the boundary
!             All exceptions are handled.
!             Note: mBI and nBI refer to the water level grid both for boundary intersection of 
!             velocity and surface ghost points. 
!             mIP and nIP instead refer to the respective grid from which they have to be interpolated
!             (see desciption of FindGhostPoints for more details)
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
    integer                       , pointer :: subtypeTESTghost
    integer                       , pointer :: totGHOSTu1
    integer                       , pointer :: totGHOSTv1
    integer                       , pointer :: totGHOSTs1
    integer, dimension(:)         , pointer :: edge6
    integer, dimension(:,:)       , pointer :: kfs_cc
    integer, dimension(:,:)       , pointer :: Ndry_GRS
    integer, dimension(:,:,:)     , pointer :: nAD
    integer, dimension(:,:,:)     , pointer :: mAD
    integer, dimension(:,:,:)     , pointer :: EDGEtypeBANKerod
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
    integer, dimension(:,:)       , pointer :: inSTENCILu
    integer, dimension(:,:)       , pointer :: inSTENCILv
    integer, dimension(:,:)       , pointer :: GHOSTu1
    integer, dimension(:,:)       , pointer :: GHOSTv1
    real(fp), dimension(:,:)      , pointer :: PSIx
    real(fp), dimension(:,:)      , pointer :: PSIy
    real(fp), dimension(:,:)      , pointer :: ETAx
    real(fp), dimension(:,:)      , pointer :: ETAy
    real(fp), dimension(:,:,:)    , pointer :: INTx_GRS
    real(fp), dimension(:,:,:)    , pointer :: INTy_GRS
    real(fp), dimension(:,:,:,:,:), pointer :: EDGExyBANKerod
    real(fp), dimension(:,:)      , pointer :: xG
    real(fp), dimension(:,:)      , pointer :: yG
    real(fp), dimension(:,:)      , pointer :: Npsi
    real(fp), dimension(:,:)      , pointer :: Neta
    real(fp), dimension(:,:)      , pointer :: Nx
    real(fp), dimension(:,:)      , pointer :: Ny
    real(fp), dimension(:,:)      , pointer :: xG_V1
    real(fp), dimension(:,:)      , pointer :: xG_U1
    real(fp), dimension(:,:)      , pointer :: yG_V1
    real(fp), dimension(:,:)      , pointer :: yG_U1
    real(fp), dimension(:)        , pointer :: xIPs1
    real(fp), dimension(:)        , pointer :: yIPs1
    real(fp), dimension(:)        , pointer :: xBIs1
    real(fp), dimension(:)        , pointer :: yBIs1
    real(fp), dimension(:)        , pointer :: xIPu1
    real(fp), dimension(:)        , pointer :: yIPu1
    real(fp), dimension(:)        , pointer :: xBIu1
    real(fp), dimension(:)        , pointer :: yBIu1
    real(fp), dimension(:)        , pointer :: xIPv1
    real(fp), dimension(:)        , pointer :: yIPv1
    real(fp), dimension(:)        , pointer :: xBIv1
    real(fp), dimension(:)        , pointer :: yBIv1
    real(fp), dimension(:)        , pointer :: nxG_S1
    real(fp), dimension(:)        , pointer :: nyG_S1
    real(fp), dimension(:)        , pointer :: nxG_U1
    real(fp), dimension(:)        , pointer :: nyG_U1
    real(fp), dimension(:)        , pointer :: nxG_V1
    real(fp), dimension(:)        , pointer :: nyG_V1
    real(fp), dimension(:)        , pointer :: DISTs1
    real(fp), dimension(:)        , pointer :: DISTu1
    real(fp), dimension(:)        , pointer :: DISTv1
    logical                       , pointer :: FORCEnormBIinFINDbi
!
! global variables
!
!
    real(fp), dimension(nlb:nub,mlb:mub)                                , intent(inout) :: s1 
    real(fp), dimension(nlb:nub,mlb:mub, kmax)                          , intent(inout) :: u1
    real(fp), dimension(nlb:nub,mlb:mub, kmax)                          , intent(inout) :: v1
    real(fp), dimension(nlb:nub,mlb:mub)                                , intent(inout) :: gsqs
    real(prec), dimension(nlb:nub,mlb:mub)                              , intent(in)    :: dps
    integer, dimension(nlb:nub,mlb:mub)                                 , intent(in)    :: kfs 
    integer, dimension(nlb:nub,mlb:mub)                                 , intent(in)    :: kcs 
    integer                                                             , intent(in)    :: mmax   !  Description and declaration in iidim.f90
    integer                                                             , intent(in)    :: nmax   !  Description and declaration in iidim.f90
    integer                                                             , intent(in)    :: nmaxus   !  Description and declaration in iidim.f90
    integer                                                             , intent(in)    :: kmax   !  Description and declaration in iidim.f90
    integer                                                             , intent(in)    :: lunscr   !  Description and declaration in iidim.f90
    integer                                                             , intent(in)    :: irov   !  Description and declaration in iidim.f90
    integer                                                             , intent(in)    :: nst   !  Description and declaration in iidim.f90
    integer                                                             , intent(in)    :: nlb
    integer                                                             , intent(in)    :: nub
    integer                                                             , intent(in)    :: mlb
    integer                                                             , intent(in)    :: mub
    integer                                                             , intent(in)    :: nmlb
    integer                                                             , intent(in)    :: nmub
    logical                                                             , intent(in)    :: Zmodel
!
!
!
! local variables
!
  integer                    :: I 
  integer                    :: nL(4)
  integer                    :: mL(4)
  integer                    :: kADJ 
  integer                    :: mADJ
  integer                    :: nADJ
  integer                    :: typeINTER
  integer                    :: n
  integer                    :: m
  integer                    :: k
  integer                    :: kn
  integer                    :: km
  integer                    :: knOK
  integer                    :: kmOK
  integer                    :: NwDRY
  integer                    :: contINTout
  integer                    :: contINT
  integer                    :: contMIN
  integer                    :: contMINout
  integer                    :: mPROV(13)
  integer                    :: nPROV(13)
  integer                    :: mADJprov(4)
  integer                    :: nADJprov(4)
  integer                    :: mPROVout(13)
  integer                    :: nPROVout(13)
  integer                    :: Ninterf
  integer                    :: nIPp1,nIPm1,nIP,mIP,mIPp1,mIPm1
!
  real(fp)                   :: x2
  real(fp)                   :: y2
  real(fp)                   :: x1
  real(fp)                   :: y1
  real(fp)                   :: xBI 
  real(fp)                   :: yBI
  real(fp)                   :: dirX 
  real(fp)                   :: dirY
  real(fp)                   :: angle
  real(fp)                   :: xi(2)
  real(fp)                   :: yi(2)
  real(fp)                   :: xBIprov(13) !9:STENCIL SIZE +4 possible dry edges on the 4 edges of the cells
  real(fp)                   :: yBIprov(13)
  real(fp)                   :: DISTprov(13)
  real(fp)                   :: xBIprovOUT(13)  
  real(fp)                   :: yBIprovOUT(13)
  real(fp)                   :: DISTprovOUT(13)
  real(fp)                   :: dx(13)
  real(fp)                   :: dy(13)
  real(fp)                   :: nxEDGE(5)
  real(fp)                   :: nyEDGE(5)
  real(fp)                   :: x1EDGE(5)
  real(fp)                   :: y1EDGE(5)
  real(fp)                   :: x2EDGE(5)
  real(fp)                   :: y2EDGE(5)
  real(fp)                   :: x2vec(5)
  real(fp)                   :: y2vec(5)
  real(fp)                   :: nxPROV(13)
  real(fp)                   :: nyPROV(13)
  real(fp)                   :: nxPROVout(13)
  real(fp)                   :: nyPROVout(13)
  real(fp)                   :: nxOK
  real(fp)                   :: nyOK
  real(fp)                   :: nxOK_out
  real(fp)                   :: nyOK_out
  real(fp)                   :: nxOKloc(1)
  real(fp)                   :: nyOKloc(1)
  real(fp)                   :: nxOKvec(1)
  real(fp)                   :: nyOKvec(1)

!
  logical                    :: inside
  logical                    :: sameDIR
!
! executable statements -------------------------------------------------------
!     
    subtypeTESTghost    => gdp%gdimbound%subtypeTESTghost
    totGHOSTu1          => gdp%gdimbound%totGHOSTu1
    totGHOSTv1          => gdp%gdimbound%totGHOSTv1
    totGHOSTs1          => gdp%gdimbound%totGHOSTs1
    edge6               => gdp%gdimbound%edge6
    kfs_cc              => gdp%gdimbound%kfs_cc
    Ndry_GRS            => gdp%gdimbound%Ndry_GRS
    nAD                 => gdp%gdimbound%nAD
    mAD                 => gdp%gdimbound%mAD
    EDGEtypeBANKerod    => gdp%gdimbound%EDGEtypeBANKerod
    nGPs1               => gdp%gdimbound%nGPs1
    mGPs1               => gdp%gdimbound%mGPs1
    nGPu1               => gdp%gdimbound%nGPu1
    mGPu1               => gdp%gdimbound%mGPu1
    nGPv1               => gdp%gdimbound%nGPv1
    mGPv1               => gdp%gdimbound%mGPv1
    mIPs1               => gdp%gdimbound%mIPs1
    nIPs1               => gdp%gdimbound%nIPs1
    mBIs1               => gdp%gdimbound%mBIs1
    nBIs1               => gdp%gdimbound%nBIs1
    mIPu1               => gdp%gdimbound%mIPu1
    nIPu1               => gdp%gdimbound%nIPu1
    mBIu1               => gdp%gdimbound%mBIu1
    nBIu1               => gdp%gdimbound%nBIu1
    mIPv1               => gdp%gdimbound%mIPv1
    nIPv1               => gdp%gdimbound%nIPv1
    mBIv1               => gdp%gdimbound%mBIv1
    nBIv1               => gdp%gdimbound%nBIv1
    inSTENCILu          => gdp%gdimbound%inSTENCILu
    inSTENCILv          => gdp%gdimbound%inSTENCILv
    GHOSTu1             => gdp%gdimbound%GHOSTu1
    GHOSTv1             => gdp%gdimbound%GHOSTv1
    PSIx                => gdp%gdimbound%PSIx
    PSIy                => gdp%gdimbound%PSIy
    ETAx                => gdp%gdimbound%ETAx
    ETAy                => gdp%gdimbound%ETAy
    INTx_GRS            => gdp%gdimbound%INTx_GRS
    INTy_GRS            => gdp%gdimbound%INTy_GRS
    EDGExyBANKerod      => gdp%gdimbound%EDGExyBANKerod
    xG                  => gdp%gdimbound%xG
    yG                  => gdp%gdimbound%yG
    Npsi                => gdp%gdimbound%Npsi
    Neta                => gdp%gdimbound%Neta
    Nx                  => gdp%gdimbound%Nx
    Ny                  => gdp%gdimbound%Ny
    xG_V1               => gdp%gdimbound%xG_V1
    xG_U1               => gdp%gdimbound%xG_U1
    yG_V1               => gdp%gdimbound%yG_V1
    yG_U1               => gdp%gdimbound%yG_U1
    xIPs1               => gdp%gdimbound%xIPs1
    yIPs1               => gdp%gdimbound%yIPs1
    xBIs1               => gdp%gdimbound%xBIs1
    yBIs1               => gdp%gdimbound%yBIs1
    xIPu1               => gdp%gdimbound%xIPu1
    yIPu1               => gdp%gdimbound%yIPu1
    xBIu1               => gdp%gdimbound%xBIu1
    yBIu1               => gdp%gdimbound%yBIu1
    xIPv1               => gdp%gdimbound%xIPv1
    yIPv1               => gdp%gdimbound%yIPv1
    xBIv1               => gdp%gdimbound%xBIv1
    yBIv1               => gdp%gdimbound%yBIv1
    nxG_S1              => gdp%gdimbound%nxG_S1
    nyG_S1              => gdp%gdimbound%nyG_S1
    nxG_U1              => gdp%gdimbound%nxG_U1
    nyG_U1              => gdp%gdimbound%nyG_U1
    nxG_V1              => gdp%gdimbound%nxG_V1
    nyG_V1              => gdp%gdimbound%nyG_V1
    DISTs1              => gdp%gdimbound%DISTs1
    DISTu1              => gdp%gdimbound%DISTu1
    DISTv1              => gdp%gdimbound%DISTv1
    FORCEnormBIinFINDbi => gdp%gdimbound%FORCEnormBIinFINDbi
!    
!   water surface REMOVED
!     
!  along psi velocity U1
! 
    inSTENCILu(1:nmaxus,1:mmax) = 0
!
    do i = 1,totGHOSTu1       
!
       m = mGPu1(i) 
       n = nGPu1(i)  
!
       x1 = xG_U1(n,m)
       y1 = yG_U1(n,m)
!
       Ninterf =0
       do k=1,3 !starting from FIRST edge (one before the velocity point, I cannot have intersections with k=4
         nADJ = nAD(n,m,k)
         mADJ = mAD(n,m,k)
       !  k = mod(kk,4)
         if (nADJ.lt.1.or.nADJ.gt.nmaxus) cycle
         if (mADJ.lt.1.or.mADJ.gt.mmax) cycle
         kADJ = edge6(k+2)
         if (EDGEtypeBANKerod(K,n,m).ge.4.or.EDGEtypeBANKerod(K,n,m).eq.0.and.(EDGEtypeBANKerod(kADJ,nADJ,mADJ).eq.3)) then ! EDGEtypeBANKerod 4 or 5, or if its a edge that face a wet cell
            Ninterf = Ninterf+1
            !note this is just a search direction, allowing to give the righe intersection (no typeINTER=4 occurs).  The actual direction is later computed
            if(mod(k,2).eq.0) then !even k
               if (k.eq.2) then ! think about defining normal to edges
                  nxEDGE(Ninterf) = + PSIx(n,m) 
                  nyEDGE(Ninterf) = + PSIy(n,m) 
               else !if (k.eq.4) then
                  nxEDGE(Ninterf) = - PSIx(n,m) 
                  nyEDGE(Ninterf) = - PSIy(n,m)      
               endif
            else !odd k
               if (k.eq.3) then ! think about defining normal to edges
                  nxEDGE(Ninterf) = + ETAx(n,m) 
                  nyEDGE(Ninterf) = + ETAy(n,m) 
               else !if (k.eq.1) then
                  nxEDGE(Ninterf) = - ETAx(n,m) 
                  nyEDGE(Ninterf) = - ETAy(n,m)                 
               endif
            endif
            x2vec(Ninterf) = x1 + nxEDGE(Ninterf) ! just a random point in the direction of the normal (minus cause the normal points toward the dry subarea)
            y2vec(Ninterf) = y1 + nyEDGE(Ninterf) ! just a random point in the direction of the normal (minus cause the normal points toward the dry subarea
            x1EDGE(Ninterf) = EDGExyBANKerod(n,m,k,1,1)
            y1EDGE(Ninterf) = EDGExyBANKerod(n,m,k,1,2)
            x2EDGE(Ninterf) = EDGExyBANKerod(n,m,k,2,1)
            y2EDGE(Ninterf) = EDGExyBANKerod(n,m,k,2,2)
            nADJprov(Ninterf) = nADJ
            mADJprov(Ninterf) = mADJ
         endif
       enddo
       contINT = 0
       contINTout = 0
       do k=1,Ninterf
         CALL IntersSemilineSegm_always( x1              , y1                  , x2vec(k)             , y2vec(k)            ,&
                                       & x1EDGE(k)       , y1EDGE(k)           , x2EDGE(k)            , y2EDGE(k)           , &
                                       & xBI             , yBI                 , 0                    , typeINTER  )   !NEAREST_or_int: 0: return nearest point. 1: return intersection
         if (typeINTER.eq.1) then
            contINT = contINT+1  
            dx(contINT) = xBI-x1
            dy(contINT) = yBI-y1  
            DISTprov(contINT)   = sqrt( dx(contINT)**2 + dy(contINT)**2 )
            xBIprov(contINT) = xBI
            yBIprov(contINT) = yBI
            mPROV(contINT) = mADJprov(k)
            nPROV(contINT) = nADJprov(k)
          !  nxPROV(contINT) = nxEDGE(k) ! this 2 could work, but not sure in the case (d_bis) of my notes, since its gonna give me typeINTER=3 but the normal to the edge is not the right direction
          !  nyPROV(contINT) = nyEDGE(k)
           !if (comparereal(DISTprov(contINT),0._fp).ne.0) then !with this test when xBI and x1 where equal to machine precision and large, the difference dx could have still be 10-12,10-13
            if (.not.((comparereal(xBI,x1).eq.0).and.(comparereal(yBI,y1).eq.0)) .and.(comparereal(DISTprov(contINT),0._fp).ne.0)) then !the second after and might be needed for small grids,xBI and x1 are not equal to machine precision but the distance is zero
               nxPROV(contINT) = - dx(contINT)/DISTprov(contINT)
               nyPROV(contINT) = - dy(contINT)/DISTprov(contINT)
            else
               nxPROV(contINT) = 0_fp ! this case is handled below
               nyPROV(contINT) = 0_fp ! this case is handled below                 
            endif
         elseif (typeINTER.eq.3) then
            contINTout = contINTout+1  
            dx(contINTout) = xBI-x1
            dy(contINTout) = yBI-y1  
            DISTprovOUT(contINTout)   = sqrt( dx(contINTout)**2 + dy(contINTout)**2 )
            xBIprovOUT(contINTout) = xBI
            yBIprovOUT(contINTout) = yBI
            mPROVout(contINTout) = mADJprov(k)
            nPROVout(contINTout) = nADJprov(k)
          !  nxPROV(contINT) = nxEDGE(k) ! this 2 could work, but not sure in the case (d_bis) of my notes, since its gonna give me typeINTER=3 but the normal to the edge is not the right direction
          !  nyPROV(contINT) = nyEDGE(k)
            ! if (comparereal(DISTprovOUT(contINTout),0._fp).ne.0) then !with this test when xBI and x1 where equal to machine precision and large, the difference dx could have still be 10-12,10-13
            if (.not.((comparereal(xBI,x1).eq.0).and.(comparereal(yBI,y1).eq.0)) .and.(comparereal(DISTprovOUT(contINTout),0._fp).ne.0)) then
               nxPROVout(contINTout) = - dx(contINTout)/DISTprovOUT(contINTout)
               nyPROVout(contINTout) = - dy(contINTout)/DISTprovOUT(contINTout)
            else
               nxPROVout(contINTout) = 0_fp ! this case is handled below
               nyPROVout(contINTout) = 0_fp ! this case is handled below                 
            endif
         endif  
       enddo  
 !
 !      contINT = 0
 !      contINTout = 0
 !      x1 = xG_U1(n,m)
 !      y1 = yG_U1(n,m)
       ! cycle on the 6 cells stencil, the interface can be only there
       do kn=max(n-1,1),min(n+1,nmaxus)   !use mask instead 
          do km=max(m,1),min(m+1,mmax)  !use mask instead
             if (kfs_cc(kn,km).eq.0.or.(zmodel.and.kfs_cc(kn,km).eq.2)) then
                x2 = x1 - Nx(kn,km) ! just a random point in the direction of the normal (minus cause the normal points toward the dry subarea)
                y2 = y1 - Ny(kn,km) ! just a random point in the direction of the normal (minus cause the normal points toward the dry subarea
                NwDRY = Ndry_GRS(kn,km)   
                CALL IntersSemilineSegm_always( x1                , y1                  , x2                     , y2                  ,&
                                              & INTx_GRS(1,kn,km) , INTy_GRS(1,kn,km)   , INTx_GRS(NwDRY,kn,km)  , INTy_GRS(NwDRY,kn,km) ,&
                                              & xBI               , yBI                 , 0 , typeINTER  ) !NEAREST_or_int: 0: return nearest point. 1: return intersection  
                if (typeINTER.eq.1) then
                   contINT = contINT+1  
                   dx(contINT) = xBI-x1
                   dy(contINT) = yBI-y1  
                   DISTprov(contINT)   = sqrt( dx(contINT)**2 + dy(contINT)**2 )
                   xBIprov(contINT) = xBI
                   yBIprov(contINT) = yBI
                   mPROV(contINT) = km
                   nPROV(contINT) = kn                              
                   !if (comparereal(DISTprov(contINT),0._fp).ne.0) then !with this test when xBI and x1 where equal to machine precision and large, the difference dx could have still be 10-12,10-13
                   if (.not.((comparereal(xBI,x1).eq.0).and.(comparereal(yBI,y1).eq.0)) .and.(comparereal(DISTprov(contINT),0._fp).ne.0)) then !the second after and might be needed for small grids,xBI and x1 are not equal to machine precision but the distance is zero
                      nxPROV(contINT) = - dx(contINT)/DISTprov(contINT)
                      nyPROV(contINT) = - dy(contINT)/DISTprov(contINT)
                   else
                      nxPROV(contINT) = 0_fp ! this case is handled below
                      nyPROV(contINT) = 0_fp ! this case is handled below                 
                   endif
                  ! nxPROV(contINT) = - Nx(kn,km) not needed normal dirx is computed   from dx and DISTprovOUT
                  ! nyPROV(contINT) = - Ny(kn,km) not needed normal dirx is computed   from dx and DISTprovOUT
                elseif (typeINTER.eq.3) then
                   contINTout = contINTout+1  
                   dx(contINTout) = xBI-x1
                   dy(contINTout) = yBI-y1  
                   DISTprovOUT(contINTout)   = sqrt( dx(contINTout)**2 + dy(contINTout)**2 )
                   xBIprovOUT(contINTout) = xBI
                   yBIprovOUT(contINTout) = yBI
                   mPROVout(contINTout) = km
                   nPROVout(contINTout) = kn                              
                  ! if (comparereal(DISTprovOUT(contINTout),0._fp).ne.0) then !with this test when xBI and x1 where equal to machine precision and large, the difference dx could have still be 10-12,10-13
                   if (.not.((comparereal(xBI,x1).eq.0).and.(comparereal(yBI,y1).eq.0)) .and.(comparereal(DISTprovOUT(contINTout),0._fp).ne.0)) then
                      nxPROVout(contINTout) = - dx(contINTout)/DISTprovOUT(contINTout)
                      nyPROVout(contINTout) = - dy(contINTout)/DISTprovOUT(contINTout)
                   else
                      nxPROVout(contINTout) = 0_fp ! this case is handled below
                      nyPROVout(contINTout) = 0_fp ! this case is handled below                 
                   endif
               ! elseif (typeINTER.eq.4) then
               !    write(*,*) 'Error Boundary intersection on the wrong side of ghost point',m,n,i,nst
               !    pause
               !    call d3stop(1,gdp)
                endif               
             endif
          enddo
       enddo
!
!
       if (subtypeTESTghost == 3.and.FORCEnormBIinFINDbi) then
          if (sqrt(x1**2+y1**2)<60) then !inner circle
             nxPROV(:) = - xg_U1(n,m)/sqrt(xg_U1(n,m)**2+yg_U1(n,m)**2)  !it points toward land and then it is inverted below
             nyPROV(:) = - yg_U1(n,m)/sqrt(xg_U1(n,m)**2+yg_U1(n,m)**2)  !it points toward land and then it is inverted below
             nxPROVout(:) = - xg_U1(n,m)/sqrt(xg_U1(n,m)**2+yg_U1(n,m)**2)  !it points toward land and then it is inverted below
             nyPROVout(:) = - yg_U1(n,m)/sqrt(xg_U1(n,m)**2+yg_U1(n,m)**2)  !it points toward land and then it is inverted below
          else !outer circle
             nxPROV(:) =   xg_U1(n,m)/sqrt(xg_U1(n,m)**2+yg_U1(n,m)**2)  !it points toward land and then it is inverted below
             nyPROV(:) =   yg_U1(n,m)/sqrt(xg_U1(n,m)**2+yg_U1(n,m)**2)  !it points toward land and then it is inverted below
             nxPROVout(:) =   xg_U1(n,m)/sqrt(xg_U1(n,m)**2+yg_U1(n,m)**2)  !it points toward land and then it is inverted below
             nyPROVout(:) =   yg_U1(n,m)/sqrt(xg_U1(n,m)**2+yg_U1(n,m)**2)  !it points toward land and then it is inverted be
          endif
          if (mod(i,77)==0.and.(mod(nst,100)==0)) write(*,*) 'remove exact normals' 
       endif
!
       IF ((contINTout.EQ.0).and.(contINT.gt.0)) THEN ! I TAKE THE ONE WITH THE CLOSEST DISTANCE AMONG ALL orthonal
!
          contMIN = MINLOC(DISTprov(1:contINT),1)
          DISTu1(i) = DISTprov(contMIN)
          xBIu1(i)  = xBIprov(contMIN)      
          yBIu1(i)  = yBIprov(contMIN) 
          if (subtypeTESTghost == 3.and.FORCEnormBIinFINDbi) then
             if (sqrt(x1**2+y1**2)<60) then !inner circle
                xBIu1(i) = - nxPROV(1)*50._fp
                yBIu1(i) = - nyPROV(1)*50._fp
             else
                xBIu1(i) = nxPROV(1)*70._fp
                yBIu1(i) = nyPROV(1)*70._fp
             endif
             DISTu1(i) = sqrt((x1-xBIu1(i))**2+(y1-yBIu1(i))**2)
             if (mod(i,77)==0.and.(mod(nst,100)==0)) write(*,*) 'remove exact normals'  
          endif
          kmOK = mPROV(contMIN)
          knOK = nPROV(contMIN) 
          mBIu1(i) = kmOK
          nBIu1(i) = knOK
          DISTu1(i) = DISTu1(i)*2._fp
          nxOK = nxPROV(contMIN)
          nyOK = nyPROV(contMIN)
          xIPu1(i) = x1 - DISTu1(i)*nxOK
          yIPu1(i) = y1 - DISTu1(i)*nyOK

       ELSEIF ((contINT.eq.0).and.(contINTout.gt.0)) THEN  ! I TAKE THE ONE WITH THE CLOSEST DISTANCE AMONG ALL non-orthonal
!
          contMINout = MINLOC(DISTprovOUT(1:contINTout),1)
          DISTu1(i) = DISTprovOUT(contMINout)
          xBIu1(i)  = xBIprovOUT(contMINout)      
          yBIu1(i)  = yBIprovOUT(contMINout) 
          if (subtypeTESTghost == 3.and.FORCEnormBIinFINDbi) then
             if (sqrt(x1**2+y1**2)<60) then !inner circle
                xBIu1(i) = - nxPROV(1)*50._fp
                yBIu1(i) = - nyPROV(1)*50._fp
             else
                xBIu1(i) = nxPROV(1)*70._fp
                yBIu1(i) = nyPROV(1)*70._fp
             endif
             DISTu1(i) = sqrt((x1-xBIu1(i))**2+(y1-yBIu1(i))**2)
             if (mod(i,77)==0.and.(mod(nst,100)==0)) write(*,*) 'remove exact normals'  
          endif
          kmOK = mPROVout(contMINout)
          knOK = nPROVout(contMINout) 
          mBIu1(i) = kmOK
          nBIu1(i) = knOK
          DISTu1(i) = DISTu1(i)*2._fp
          nxOK = nxPROVout(contMINout)
          nyOK = nyPROVout(contMINout)
          xIPu1(i) = x1 - DISTu1(i)*nxOK
          yIPu1(i) = y1 - DISTu1(i)*nyOK
!
       elseif ((contINTout.gt.0).and.(contINT.gt.0)) then  !I take the normal if it is in the same direction of the non-normal intersection. If in opposite direct, I take it only if it is closer (to avoid unlucky case (e) in my notes)      
!
          contMIN = MINLOC(DISTprov(1:contINT),1)
          contMINout = MINLOC(DISTprovOUT(1:contINTout),1)
          nxOK = nxPROV(contMIN)
          nyOK = nyPROV(contMIN)
          nxOK_out = nxPROVout(contMINout)
          nyOK_out = nyPROVout(contMINout)
          angle = atan2(nxOK*nyOK_out-nyOK*nxOK_out,nxOK*nxOK_out+nyOK*nyOK_out) 
          sameDIR = (angle.gt.-pi/2.0_fp.and.angle.lt.pi/2.0_fp)
          if (.not.sameDIR.and.(DISTprov(contMIN).gt.DISTprovOUT(contMINout))) then  !OPPOSITE DIRECTION   and distance bigger
             DISTu1(i) = DISTprovOUT(contMINout)
             xBIu1(i)  = xBIprovOUT(contMINout)      
             yBIu1(i)  = yBIprovOUT(contMINout) 
             if (subtypeTESTghost == 3.and.FORCEnormBIinFINDbi) then
                if (sqrt(x1**2+y1**2)<60) then !inner circle
                   xBIu1(i) = - nxPROV(1)*50._fp
                   yBIu1(i) = - nyPROV(1)*50._fp
                else
                   xBIu1(i) = nxPROV(1)*70._fp
                   yBIu1(i) = nyPROV(1)*70._fp
                endif
                DISTu1(i) = sqrt((x1-xBIu1(i))**2+(y1-yBIu1(i))**2)
                if (mod(i,77)==0.and.(mod(nst,100)==0)) write(*,*) 'remove exact normals'  
             endif
             kmOK = mPROVout(contMINout)
             knOK = nPROVout(contMINout) 
             mBIu1(i) = kmOK
             nBIu1(i) = knOK
             DISTu1(i) = DISTu1(i)*2._fp
             nxOK = nxPROVout(contMINout)
             nyOK = nyPROVout(contMINout)
             xIPu1(i) = x1 - DISTu1(i)*nxOK
             yIPu1(i) = y1 - DISTu1(i)*nyOK
          else  !elseif sameDIR or if opposDIR but distance smaller, contMIN is the one pointing to the normal
             DISTu1(i) = DISTprov(contMIN)
             xBIu1(i)  = xBIprov(contMIN)      
             yBIu1(i)  = yBIprov(contMIN) 
             if (subtypeTESTghost == 3.and.FORCEnormBIinFINDbi) then
                if (sqrt(x1**2+y1**2)<60) then !inner circle
                   xBIu1(i) = - nxPROV(1)*50._fp
                   yBIu1(i) = - nyPROV(1)*50._fp
                else
                   xBIu1(i) = nxPROV(1)*70._fp
                   yBIu1(i) = nyPROV(1)*70._fp
                endif
                DISTu1(i) = sqrt((x1-xBIu1(i))**2+(y1-yBIu1(i))**2)
                if (mod(i,77)==0.and.(mod(nst,100)==0)) write(*,*) 'remove exact normals'  
             endif
             kmOK = mPROV(contMIN)
             knOK = nPROV(contMIN) 
             mBIu1(i) = kmOK
             nBIu1(i) = knOK
             DISTu1(i) = DISTu1(i)*2._fp
             nxOK = nxPROV(contMIN)
             nyOK = nyPROV(contMIN)
             xIPu1(i) = x1 - DISTu1(i)*nxOK
             yIPu1(i) = y1 - DISTu1(i)*nyOK
          endif 
!
       else
          write(*,*) 'Error no Boundary intersection found for u1',m,n,i,nst
          !pause
          call d3stop(1,gdp)
       ENDIF
!
       IF (comparereal(nxOK,0._fp).eq.0.and.comparereal(nyOK,0._fp).eq.0) then !if both are zero, the search below fails
          !IP, BI and GP exactly coincides. take the average of all the normal found (to be checked, but it should only find maximim  2 intersections since the search is done on the 6 cells stencil
          nxOK=0_fp
          nyOK=0_fp
          do k=1,contINT
            nxOK = nxOK +  nx(nPROV(k),mPROV(k)) 
            nyOK = nyOK +  ny(nPROV(k),mPROV(k)) 
          enddo
          do k=1,contINTout
            nxOK = nxOK +  nx(nPROVout(k),mPROVout(k)) 
            nyOK = nyOK +  ny(nPROVout(k),mPROVout(k)) 
          enddo
          nxG_U1(i) = -nxOK/(contINT+contINTout) !locateMN_U1 wants the vector pointing toward the fluid. note that ROTATEback wants a vector as input
          nyG_U1(i) = -nyOK/(contINT+contINTout) !locateMN_U1 wants the vector pointing toward the fluid. note that ROTATEback wants a vector as input
  
       else !standard case
          nxG_U1(i) = -nxOK !locateMN_V1 wants the vector pointing toward the fluid. note that ROTATEback wants a vector as input
          nyG_U1(i) = -nyOK !locateMN_V1 wants the vector pointing toward the fluid. note that ROTATEback wants a vector as input
       endif
     ! locate the cell (nIPs1,mIPs1) that contains the image point
       CALL ROTATEback(nxG_U1(i),nyG_U1(i),PSIx(n,m),PSIy(n,m),1,nxOKloc(1),nyOKloc(1))
       if (n.eq.1) then
          if (nyOKloc(1).lt.0._fp) then
            nyOKloc(1) = 0._fp
          endif
       elseif (n.eq.nmaxus) then
          if (nyOKloc(1).gt.0._fp) then
            nyOKloc(1) = 0._fp
          endif
       endif
       if (m.eq.1) then
          if (nxOKloc(1).lt.0._fp) then
            nxOKloc(1) = 0._fp
          endif
       elseif (m.eq.mmax) then
          if (nxOKloc(1).gt.0._fp) then
            nxOKloc(1) = 0._fp
          endif
       endif
       CALL locateMN_U1(m,n,nxOKloc(1),nyOKloc(1),xIPu1(i),yIPu1(i),mIPu1(i),nIPu1(i),mmax,nmaxus, gdp)  ! finds the location (m,n) of a given point given a initial cell and a search direction in psi and eta (vector)
!      4 points of the stencil
       mIP = mIPu1(i)
       nIP = nIPu1(i) 
       mIPm1 = mIP-1
       nIPp1 = nIP+1
       ! define inSTENCILu
       if (GHOSTu1(nIP,mIP) == 0) then
          inSTENCILu(nIP,mIP)     = 1
       endif
       if (GHOSTu1(nIP,mIPm1) == 0) then
          inSTENCILu(nIP,mIPm1)   = 1
       endif
       if (GHOSTu1(nIPp1,mIP) == 0) then
          inSTENCILu(nIPp1,mIP)   = 1
       endif
       if (GHOSTu1(nIPp1,mIPm1) == 0) then
          inSTENCILu(nIPp1,mIPm1) = 1
       endif
!
    enddo
!     
!  along eta velocity V1
! 
    inSTENCILv(1:nmaxus,1:mmax) = 0
!
    do i = 1,totGHOSTv1       
!
       m = mGPv1(i) 
       n = nGPv1(i)  
!
       x1 = xG_V1(n,m)
       y1 = yG_V1(n,m)
!
       Ninterf =0
       do k=2,4 !starting from second edge (one before the velocity point, I cannot have intersections with k=1
         nADJ = nAD(n,m,k)
         mADJ = mAD(n,m,k)
       !  k = mod(kk,4)
         if (nADJ.lt.1.or.nADJ.gt.nmaxus) cycle
         if (mADJ.lt.1.or.mADJ.gt.mmax) cycle
         kADJ = edge6(k+2)
         if (EDGEtypeBANKerod(K,n,m).ge.4.or.EDGEtypeBANKerod(K,n,m).eq.0.and.(EDGEtypeBANKerod(kADJ,nADJ,mADJ).eq.3)) then ! EDGEtypeBANKerod 4 or 5, or if its a edge that face a wet cell
            Ninterf = Ninterf+1
            !note this is just a search direction, allowing to give the righe intersection (no typeINTER=4 occurs).  The actual direction is later computed
            if(mod(k,2).eq.0) then !even k
               if (k.eq.2) then ! think about defining normal to edges
                  nxEDGE(Ninterf) = + PSIx(n,m) 
                  nyEDGE(Ninterf) = + PSIy(n,m) 
               else !if (k.eq.4) then
                  nxEDGE(Ninterf) = - PSIx(n,m) 
                  nyEDGE(Ninterf) = - PSIy(n,m)      
               endif
            else !odd k
               if (k.eq.3) then ! think about defining normal to edges
                  nxEDGE(Ninterf) = + ETAx(n,m) 
                  nyEDGE(Ninterf) = + ETAy(n,m) 
               else !if (k.eq.1) then
                  nxEDGE(Ninterf) = - ETAx(n,m) 
                  nyEDGE(Ninterf) = - ETAy(n,m)                 
               endif
            endif
            x2vec(Ninterf) = x1 + nxEDGE(Ninterf) ! just a random point in the direction of the normal (minus cause the normal points toward the dry subarea)
            y2vec(Ninterf) = y1 + nyEDGE(Ninterf) ! just a random point in the direction of the normal (minus cause the normal points toward the dry subarea
            x1EDGE(Ninterf) = EDGExyBANKerod(n,m,k,1,1)
            y1EDGE(Ninterf) = EDGExyBANKerod(n,m,k,1,2)
            x2EDGE(Ninterf) = EDGExyBANKerod(n,m,k,2,1)
            y2EDGE(Ninterf) = EDGExyBANKerod(n,m,k,2,2)
            nADJprov(Ninterf) = nADJ
            mADJprov(Ninterf) = mADJ
         endif
       enddo
       contINT = 0
       contINTout = 0
       do k=1,Ninterf
         CALL IntersSemilineSegm_always( x1              , y1                  , x2vec(k)             , y2vec(k)            ,&
                                       & x1EDGE(k)       , y1EDGE(k)           , x2EDGE(k)            , y2EDGE(k)           , &
                                       & xBI             , yBI                 , 0                    , typeINTER  )   !NEAREST_or_int: 0: return nearest point. 1: return intersection
         if (typeINTER.eq.1) then
            contINT = contINT+1  
            dx(contINT) = xBI-x1
            dy(contINT) = yBI-y1  
            DISTprov(contINT)   = sqrt( dx(contINT)**2 + dy(contINT)**2 )
            xBIprov(contINT) = xBI
            yBIprov(contINT) = yBI
            mPROV(contINT) = mADJprov(k)
            nPROV(contINT) = nADJprov(k)
          !  nxPROV(contINT) = nxEDGE(k) ! this 2 could work, but not sure in the case (d_bis) of my notes, since its gonna give me typeINTER=3 but the normal to the edge is not the right direction
          !  nyPROV(contINT) = nyEDGE(k)
           !if (comparereal(DISTprov(contINT),0._fp).ne.0) then !with this test when xBI and x1 where equal to machine precision and large, the difference dx could have still be 10-12,10-13
            if (.not.((comparereal(xBI,x1).eq.0).and.(comparereal(yBI,y1).eq.0)) .and.(comparereal(DISTprov(contINT),0._fp).ne.0)) then !the second after and might be needed for small grids,xBI and x1 are not equal to machine precision but the distance is zero
               nxPROV(contINT) = - dx(contINT)/DISTprov(contINT)
               nyPROV(contINT) = - dy(contINT)/DISTprov(contINT)
            else
               nxPROV(contINT) = 0_fp ! this case is handled below
               nyPROV(contINT) = 0_fp ! this case is handled below                 
            endif
         elseif (typeINTER.eq.3) then
            contINTout = contINTout+1  
            dx(contINTout) = xBI-x1
            dy(contINTout) = yBI-y1  
            DISTprovOUT(contINTout)   = sqrt( dx(contINTout)**2 + dy(contINTout)**2 )
            xBIprovOUT(contINTout) = xBI
            yBIprovOUT(contINTout) = yBI
            mPROVout(contINTout) = mADJprov(k)
            nPROVout(contINTout) = nADJprov(k)
          !  nxPROV(contINT) = nxEDGE(k) ! this 2 could work, but not sure in the case (d_bis) of my notes, since its gonna give me typeINTER=3 but the normal to the edge is not the right direction
          !  nyPROV(contINT) = nyEDGE(k)
            ! if (comparereal(DISTprovOUT(contINTout),0._fp).ne.0) then !with this test when xBI and x1 where equal to machine precision and large, the difference dx could have still be 10-12,10-13
            if (.not.((comparereal(xBI,x1).eq.0).and.(comparereal(yBI,y1).eq.0)) .and.(comparereal(DISTprovOUT(contINTout),0._fp).ne.0)) then
               nxPROVout(contINTout) = - dx(contINTout)/DISTprovOUT(contINTout)
               nyPROVout(contINTout) = - dy(contINTout)/DISTprovOUT(contINTout)
            else
               nxPROVout(contINTout) = 0_fp ! this case is handled below
               nyPROVout(contINTout) = 0_fp ! this case is handled below                 
            endif
         endif  
       enddo  
 !
 !      contINT = 0
 !      contINTout = 0
 !      x1 = xG_V1(n,m)
 !      y1 = yG_V1(n,m)
       ! cycle on the 6 cells stencil, the interface can be only there
       do kn=max(n,1),min(n+1,nmaxus)   !use mask instead
          do km=max(m-1,1),min(m+1,mmax)  !use mask instead
             if (kfs_cc(kn,km).eq.0.or.(zmodel.and.kfs_cc(kn,km).eq.2)) then ! 
                x2 = x1 - Nx(kn,km) ! just a random point in the direction of the normal (minus cause the normal points toward the dry subarea)
                y2 = y1 - Ny(kn,km) ! just a random point in the direction of the normal (minus cause the normal points toward the dry subarea
                NwDRY = Ndry_GRS(kn,km)   
                CALL IntersSemilineSegm_always( x1                , y1                  , x2                     , y2                  ,&
                                              & INTx_GRS(1,kn,km) , INTy_GRS(1,kn,km)   , INTx_GRS(NwDRY,kn,km)  , INTy_GRS(NwDRY,kn,km) ,&
                                              & xBI               , yBI                 , 0 , typeINTER  ) !NEAREST_or_int: 0: return nearest point. 1: return intersection  
                if (typeINTER.eq.1) then
                   contINT = contINT+1  
                   dx(contINT) = xBI-x1
                   dy(contINT) = yBI-y1  
                   DISTprov(contINT)   = sqrt( dx(contINT)**2 + dy(contINT)**2 )
                   xBIprov(contINT) = xBI
                   yBIprov(contINT) = yBI
                   mPROV(contINT) = km
                   nPROV(contINT) = kn                              
                  !if (comparereal(DISTprov(contINT),0._fp).ne.0) then !with this test when xBI and x1 where equal to machine precision and large, the difference dx could have still be 10-12,10-13
                   if (.not.((comparereal(xBI,x1).eq.0).and.(comparereal(yBI,y1).eq.0)) .and.(comparereal(DISTprov(contINT),0._fp).ne.0)) then !the second after and might be needed for small grids,xBI and x1 are not equal to machine precision but the distance is zero
                      nxPROV(contINT) = - dx(contINT)/DISTprov(contINT)
                      nyPROV(contINT) = - dy(contINT)/DISTprov(contINT)
                   else
                      nxPROV(contINT) = 0_fp ! this case is handled below
                      nyPROV(contINT) = 0_fp ! this case is handled below                 
                   endif
                  ! nxPROV(contINT) = - Nx(kn,km) not needed normal dirx is computed   from dx and DISTprovOUT
                  ! nyPROV(contINT) = - Ny(kn,km) not needed normal dirx is computed   from dx and DISTprovOUT
                elseif (typeINTER.eq.3) then
                   contINTout = contINTout+1  
                   dx(contINTout) = xBI-x1
                   dy(contINTout) = yBI-y1  
                   DISTprovOUT(contINTout)   = sqrt( dx(contINTout)**2 + dy(contINTout)**2 )
                   xBIprovOUT(contINTout) = xBI
                   yBIprovOUT(contINTout) = yBI
                   mPROVout(contINTout) = km
                   nPROVout(contINTout) = kn                              
                  ! if (comparereal(DISTprovOUT(contINTout),0._fp).ne.0) then !with this test when xBI and x1 where equal to machine precision and large, the difference dx could have still be 10-12,10-13
                   if (.not.((comparereal(xBI,x1).eq.0).and.(comparereal(yBI,y1).eq.0)) .and.(comparereal(DISTprovOUT(contINTout),0._fp).ne.0)) then
                      nxPROVout(contINTout) = - dx(contINTout)/DISTprovOUT(contINTout)
                      nyPROVout(contINTout) = - dy(contINTout)/DISTprovOUT(contINTout)
                   else
                      nxPROVout(contINTout) = 0_fp ! this case is handled below
                      nyPROVout(contINTout) = 0_fp ! this case is handled below                 
                   endif
               ! elseif (typeINTER.eq.4) then
               !    write(*,*) 'Error Boundary intersection on the wrong side of ghost point',m,n,i,nst
               !    pause
               !    call d3stop(1,gdp)
                endif               
             endif
          enddo
       enddo
!
       if (subtypeTESTghost == 3.and.FORCEnormBIinFINDbi) then
          if (sqrt(x1**2+y1**2)<60) then !inner circle
             nxPROV(:) = - xg_V1(n,m)/sqrt(xg_V1(n,m)**2+yg_V1(n,m)**2)  !it points toward land and then it is inverted below
             nyPROV(:) = - yg_V1(n,m)/sqrt(xg_V1(n,m)**2+yg_V1(n,m)**2)  !it points toward land and then it is inverted below
             nxPROVout(:) = - xg_V1(n,m)/sqrt(xg_V1(n,m)**2+yg_V1(n,m)**2)  !it points toward land and then it is inverted below
             nyPROVout(:) = - yg_V1(n,m)/sqrt(xg_V1(n,m)**2+yg_V1(n,m)**2)  !it points toward land and then it is inverted below
          else !outer circle
             nxPROV(:) =   xg_V1(n,m)/sqrt(xg_V1(n,m)**2+yg_V1(n,m)**2)  !it points toward land and then it is inverted below
             nyPROV(:) =   yg_V1(n,m)/sqrt(xg_V1(n,m)**2+yg_V1(n,m)**2)  !it points toward land and then it is inverted below
             nxPROVout(:) =   xg_V1(n,m)/sqrt(xg_V1(n,m)**2+yg_V1(n,m)**2)  !it points toward land and then it is inverted below
             nyPROVout(:) =   yg_V1(n,m)/sqrt(xg_V1(n,m)**2+yg_V1(n,m)**2)  !it points toward land and then it is inverted below
          endif
          if (mod(i,77)==0.and.(mod(nst,100)==0)) write(*,*) 'remove exact normals'  
       endif

       IF ((contINTout.EQ.0).and.(contINT.gt.0)) THEN ! I TAKE THE ONE WITH THE CLOSEST DISTANCE AMONG ALL orthonal
!
          contMIN = MINLOC(DISTprov(1:contINT),1)
          DISTv1(i) = DISTprov(contMIN)
          xBIv1(i)  = xBIprov(contMIN)      
          yBIv1(i)  = yBIprov(contMIN) 
          if (subtypeTESTghost == 3.and.FORCEnormBIinFINDbi) then
             if (sqrt(x1**2+y1**2)<60) then !inner circle
                xBIv1(i) = - nxPROV(1)*50._fp
                yBIv1(i) = - nyPROV(1)*50._fp
             else
                xBIv1(i) = nxPROV(1)*70._fp
                yBIv1(i) = nyPROV(1)*70._fp
             endif
             DISTv1(i) = sqrt((x1-xBIv1(i))**2+(y1-yBIv1(i))**2)
             if (mod(i,77)==0.and.(mod(nst,100)==0)) write(*,*) 'remove exact normals'  
          endif
          kmOK = mPROV(contMIN)
          knOK = nPROV(contMIN) 
          mBIv1(i) = kmOK
          nBIv1(i) = knOK
          DISTv1(i) = DISTv1(i)*2._fp
          nxOK = nxPROV(contMIN)
          nyOK = nyPROV(contMIN)
          xIPv1(i) = x1 - DISTv1(i)*nxOK
          yIPv1(i) = y1 - DISTv1(i)*nyOK

       ELSEIF ((contINT.eq.0).and.(contINTout.gt.0)) THEN  ! I TAKE THE ONE WITH THE CLOSEST DISTANCE AMONG ALL non-orthonal
!
          contMINout = MINLOC(DISTprovOUT(1:contINTout),1)
          DISTv1(i) = DISTprovOUT(contMINout)
          xBIv1(i)  = xBIprovOUT(contMINout)      
          yBIv1(i)  = yBIprovOUT(contMINout) 
          if (subtypeTESTghost == 3.and.FORCEnormBIinFINDbi) then
             if (sqrt(x1**2+y1**2)<60) then !inner circle
                xBIv1(i) = - nxPROV(1)*50._fp
                yBIv1(i) = - nyPROV(1)*50._fp
             else
                xBIv1(i) = nxPROV(1)*70._fp
                yBIv1(i) = nyPROV(1)*70._fp
             endif
             DISTv1(i) = sqrt((x1-xBIv1(i))**2+(y1-yBIv1(i))**2)
             if (mod(i,77)==0.and.(mod(nst,100)==0)) write(*,*) 'remove exact normals'  
          endif
          kmOK = mPROVout(contMINout)
          knOK = nPROVout(contMINout) 
          mBIv1(i) = kmOK
          nBIv1(i) = knOK
          DISTv1(i) = DISTv1(i)*2._fp
          nxOK = nxPROVout(contMINout)
          nyOK = nyPROVout(contMINout)
          xIPv1(i) = x1 - DISTv1(i)*nxOK
          yIPv1(i) = y1 - DISTv1(i)*nyOK
!
       elseif ((contINTout.gt.0).and.(contINT.gt.0)) then  !I take the normal if it is in the same direction of the non-normal intersection. If in opposite direct, I take it only if it is closer (to avoid unlucky case (e) in my notes)      
!
          contMIN = MINLOC(DISTprov(1:contINT),1)
          contMINout = MINLOC(DISTprovOUT(1:contINTout),1)
          nxOK = nxPROV(contMIN)
          nyOK = nyPROV(contMIN)
          nxOK_out = nxPROVout(contMINout)
          nyOK_out = nyPROVout(contMINout)
          angle = atan2(nxOK*nyOK_out-nyOK*nxOK_out,nxOK*nxOK_out+nyOK*nyOK_out) 
          sameDIR = (angle.gt.-pi/2.0_fp.and.angle.lt.pi/2.0_fp)
          if (.not.sameDIR.and.(DISTprov(contMIN).gt.DISTprovOUT(contMINout))) then  !OPPOSITE DIRECTION   and distance bigger
             DISTv1(i) = DISTprovOUT(contMINout)
             xBIv1(i)  = xBIprovOUT(contMINout)      
             yBIv1(i)  = yBIprovOUT(contMINout) 
             if (subtypeTESTghost == 3.and.FORCEnormBIinFINDbi) then
                if (sqrt(x1**2+y1**2)<60) then !inner circle
                   xBIv1(i) = - nxPROV(1)*50._fp
                   yBIv1(i) = - nyPROV(1)*50._fp
                else
                   xBIv1(i) = nxPROV(1)*70._fp
                   yBIv1(i) = nyPROV(1)*70._fp
                endif
                DISTv1(i) = sqrt((x1-xBIv1(i))**2+(y1-yBIv1(i))**2)
                if (mod(i,77)==0.and.(mod(nst,100)==0)) write(*,*) 'remove exact normals'  
             endif
             kmOK = mPROVout(contMINout)
             knOK = nPROVout(contMINout) 
             mBIv1(i) = kmOK
             nBIv1(i) = knOK
             DISTv1(i) = DISTv1(i)*2._fp
             nxOK = nxPROVout(contMINout)
             nyOK = nyPROVout(contMINout)
             xIPv1(i) = x1 - DISTv1(i)*nxOK
             yIPv1(i) = y1 - DISTv1(i)*nyOK
          else  !elseif sameDIR or if opposDIR but distance smaller, contMIN is the one pointing to the normal
             DISTv1(i) = DISTprov(contMIN)
             xBIv1(i)  = xBIprov(contMIN)      
             yBIv1(i)  = yBIprov(contMIN) 
             if (subtypeTESTghost == 3.and.FORCEnormBIinFINDbi) then
                if (sqrt(x1**2+y1**2)<60) then !inner circle
                   xBIv1(i) = - nxPROV(1)*50._fp
                   yBIv1(i) = - nyPROV(1)*50._fp
                else
                   xBIv1(i) = nxPROV(1)*70._fp
                   yBIv1(i) = nyPROV(1)*70._fp
                endif
                DISTv1(i) = sqrt((x1-xBIv1(i))**2+(y1-yBIv1(i))**2)
                if (mod(i,77)==0.and.(mod(nst,100)==0)) write(*,*) 'remove exact normals'  
             endif
             kmOK = mPROV(contMIN)
             knOK = nPROV(contMIN) 
             mBIv1(i) = kmOK
             nBIv1(i) = knOK
             DISTv1(i) = DISTv1(i)*2._fp
             nxOK = nxPROV(contMIN)
             nyOK = nyPROV(contMIN)
             xIPv1(i) = x1 - DISTv1(i)*nxOK
             yIPv1(i) = y1 - DISTv1(i)*nyOK
          endif 
!
       else
          write(*,*) 'Error no Boundary intersection found for v1',m,n,i,nst
          !pause
          call d3stop(1,gdp)
       ENDIF
!
       IF (comparereal(nxOK,0._fp).eq.0.and.comparereal(nyOK,0._fp).eq.0) then
          !IP, BI and GP exactly coincides. take the average of all the normal found (to be checked, but it should only find maximim  2 intersections since the search is done on the 6 cells stencil
          nxOK=0_fp
          nyOK=0_fp
          do k=1,contINT
            nxOK = nxOK +  nx(nPROV(k),mPROV(k)) 
            nyOK = nyOK +  ny(nPROV(k),mPROV(k)) 
          enddo
          do k=1,contINTout
            nxOK = nxOK +  nx(nPROVout(k),mPROVout(k)) 
            nyOK = nyOK +  ny(nPROVout(k),mPROVout(k)) 
          enddo
          nxG_V1(i) = -nxOK/(contINT+contINTout) !locateMN_V1 wants the vector pointing toward the fluid. note that ROTATEback wants a vector as input
          nyG_V1(i) = -nyOK/(contINT+contINTout) !locateMN_V1 wants the vector pointing toward the fluid. note that ROTATEback wants a vector as input
  
       else !standard case
          nxG_V1(i) = -nxOK !locateMN_V1 wants the vector pointing toward the fluid. note that ROTATEback wants a vector as input
          nyG_V1(i) = -nyOK !locateMN_V1 wants the vector pointing toward the fluid. note that ROTATEback wants a vector as input
       endif
       ! write(*,*)m,n,mBIs1(i),nBIs1(i)

     ! locate the cell (nIPs1,mIPs1) that contains the image point
       CALL ROTATEback(nxG_V1(i),nyG_V1(i),PSIx(n,m),PSIy(n,m),1,nxOKloc(1),nyOKloc(1))
     ! correct normals pointing slightly outside the domain
     !
       if (n.eq.1) then
          if (nyOKloc(1).lt.0._fp) then
            nyOKloc(1) = 0._fp
          endif
       elseif (n.eq.nmaxus) then
          if (nyOKloc(1).gt.0._fp) then
            nyOKloc(1) = 0._fp
          endif
       endif
       if (m.eq.1) then
          if (nxOKloc(1).lt.0._fp) then
            nxOKloc(1) = 0._fp
          endif
       elseif (m.eq.mmax) then
          if (nxOKloc(1).gt.0._fp) then
            nxOKloc(1) = 0._fp
          endif
       endif
       CALL locateMN_V1(m,n,nxOKloc(1),nyOKloc(1),xIPv1(i),yIPv1(i),mIPv1(i),nIPv1(i),mmax,nmaxus, gdp)  ! finds the location (m,n) of a given point given a initial cell and a search direction in psi and eta (vector)
!      4 points of the stencil
       mIP = mIPv1(i)
       nIP = nIPv1(i) 
       mIPp1 = mIP+1
       nIPm1 = nIP-1
       ! define inSTENCILu
       if (GHOSTv1(nIP,mIP) == 0) then
          inSTENCILv(nIP,mIP)     = 1
       endif
       if (GHOSTv1(nIPm1,mIP) == 0) then
          inSTENCILv(nIPm1,mIP)   = 1
       endif
       if (GHOSTv1(nIP,mIPp1) == 0) then
          inSTENCILv(nIP,mIPp1)   = 1
       endif
       if (GHOSTv1(nIPm1,mIPp1) == 0) then
          inSTENCILv(nIPm1,mIPp1) = 1
       endif
    enddo
!
RETURN
end subroutine Find_BI_PI
