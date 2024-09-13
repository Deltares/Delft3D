subroutine forceGHOSTsud(icx        ,icy        ,u0         ,v1         ,kcs        ,&
                       & xcor       ,ycor       ,guu        ,gvv        ,v1INTu     ,&
                       & hu         ,aguu       ,u1         ,Umean      ,s1         ,&
                       & kfs        ,kfu        ,kfv        ,thick      ,qxk        ,&
                       & aa         ,bb         ,cc         ,dd         ,&
                       & aak        ,bbk        ,cck        ,ddk        ,&
                       & mmax       ,nmax       ,nmmax      ,nmaxus     ,kmax       ,&
                       & nst        ,nlb        ,nub        ,mlb        ,mub        ,&
                       & nmlb       ,nmub       ,ddbound    ,lunscr     ,Irov       ,&
                       & iter       ,gdp)
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
!   Function: Call subroutine that are needed for cutcells before sud computation (stage 2)
! 
!   Author: Alberto Canestrelli
!             
!!--declarations----------------------------------------------------------------
!
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    integer                 , pointer :: GHOSTimpl
    integer, dimension(:,:) , pointer :: ghostu1
    integer, dimension(:,:) , pointer :: ghostv1
    integer                 , pointer :: totGHOSTu1
    integer                 , pointer :: totGHOSTv1
    integer, dimension(:)   , pointer :: mGPv1
    integer, dimension(:)   , pointer :: nGPv1
    integer, dimension(:)   , pointer :: mGPu1
    integer, dimension(:)   , pointer :: nGPu1
    integer                 , pointer :: extrapGHOST1fluid2
    integer                 , pointer :: TYPEfreeSLIP
    real(fp), dimension(:,:), pointer :: Nx
    real(fp), dimension(:,:), pointer :: Ny
    real(fp), dimension(:,:), pointer :: xG_U1
    real(fp), dimension(:,:), pointer :: yG_U1
    real(fp), dimension(:,:), pointer :: xG_V1
    real(fp), dimension(:,:), pointer :: yG_V1
    integer, dimension(:,:) , pointer :: kfs_cc
    integer                 , pointer :: free_S1_sud
    real(fp), dimension(:,:), pointer :: u1inp
!
! global variables 
!
    real(fp), dimension(nmlb:nmub)                                      , intent(in)    :: xcor    
    real(fp), dimension(nmlb:nmub)                                      , intent(in)    :: ycor
    real(fp), dimension(nmlb:nmub,kmax)                                 , intent(inout) :: u0
    real(fp), dimension(nmlb:nmub,kmax)                                 , intent(inout) :: u1
    real(fp), dimension(nmlb:nmub,kmax)                                 , intent(in)    :: v1
    real(fp), dimension(nmlb:nmub)                                      , intent(inout) :: s1
    real(fp), dimension(nmlb:nmub, kmax)                                , intent(inout) :: v1INTu
    real(fp), dimension(nmlb:nmub, kmax)                                , intent(inout) :: qxk
    real(fp), dimension(nmlb:nmub)                                      , intent(inout) :: Umean
    real(fp), dimension(nmlb:nmub)                                      , intent(in)    :: guu
    real(fp), dimension(nmlb:nmub)                                      , intent(in)    :: gvv
    real(fp), dimension(nmlb:nmub)                                      , intent(in)    :: aguu
    real(fp), dimension(nmlb:nmub)                                      , intent(inout)    :: hu
    real(fp), dimension(kmax)                                           , intent(in)    :: thick 
    real(fp), dimension(nmlb:nmub)                                      , intent(inout) :: aa      !!  Internal work array, coupling mean velocity with water level point in (N,M,K) left (down)
    real(fp), dimension(nmlb:nmub)                                      , intent(inout) :: bb      !!  Internal work array, coefficient mean velocity
    real(fp), dimension(nmlb:nmub)                                      , intent(inout) :: cc      !!  Internal work array, coupling mean velocity with water level point right (upper)
    real(fp), dimension(nmlb:nmub)                                      , intent(inout) :: dd      !!  Internal work array, Right hand side of the momentum eq. at (N,M)
    real(fp), dimension(nmlb:nmub, kmax)                    , intent(inout) :: aak     !!  Internal work array (in CUCNP & UZD)
    real(fp), dimension(nmlb:nmub, kmax)                    , intent(inout) :: bbk     !!  Internal work array (in CUCNP & UZD)
    real(fp), dimension(nmlb:nmub, kmax)                    , intent(inout) :: cck     !!  Internal work array (in CUCNP & UZD)
    real(fp), dimension(nmlb:nmub, kmax)                    , intent(inout) :: ddk     !!  Internal work array, diagonal space at (N,M,K)    
    integer, dimension(nmlb:nmub)                                       , intent(in)    :: kcs
    integer, dimension(nmlb:nmub)                                       , intent(in)    :: kfs
    integer, dimension(nmlb:nmub)                                       , intent(inout) :: kfu
    integer, dimension(nmlb:nmub)                                       , intent(inout) :: kfv
    integer                                                             , intent(in)    :: lunscr
    integer                                                             , intent(in)    :: nst   !  Description and declaration in iidim.f90
    integer                                                             , intent(in)    :: nmmax
    integer                                                             , intent(in)    :: nmlb
    integer                                                             , intent(in)    :: nmub
    integer                                                             , intent(in)    :: icx
    integer                                                             , intent(in)    :: icy
    integer                                                             , intent(in)    :: Irov
    integer                                                             , intent(in)    :: mmax
    integer                                                             , intent(in)    :: nmax
    integer                                                             , intent(in)    :: nmaxus
    integer                                                             , intent(in)    :: nlb
    integer                                                             , intent(in)    :: nub
    integer                                                             , intent(in)    :: mlb
    integer                                                             , intent(in)    :: mub
    integer                                                             , intent(in)    :: ddbound
    integer                                                             , intent(in)    :: kmax
    integer                                                             , intent(in)    :: iter
!
! local variables
!
   integer :: i
   integer :: k
   integer :: nmaxOK
   integer :: mGP
   integer :: nGP
   integer :: nm
   integer :: idummy1
   integer :: idummy2
   !real(fp), dimension(nmlb:nmub, kmax)                       :: u1inp
!
! executable statements -------------------------------------------------------
!   
    GHOSTimpl          => gdp%gdimbound%GHOSTimpl
    ghostu1            => gdp%gdimbound%ghostu1
    ghostv1            => gdp%gdimbound%ghostv1
    totGHOSTu1         => gdp%gdimbound%totGHOSTu1
    totGHOSTv1         => gdp%gdimbound%totGHOSTv1
    mGPv1              => gdp%gdimbound%mGPv1
    nGPv1              => gdp%gdimbound%nGPv1
    mGPu1              => gdp%gdimbound%mGPu1
    nGPu1              => gdp%gdimbound%nGPu1
    extrapGHOST1fluid2 => gdp%gdimbound%extrapGHOST1fluid2
    TYPEfreeSLIP       => gdp%gdimbound%TYPEfreeSLIP
    Nx                 => gdp%gdimbound%Nx
    Ny                 => gdp%gdimbound%Ny
    xG_U1              => gdp%gdimbound%xG_U1
    yG_U1              => gdp%gdimbound%yG_U1
    xG_V1              => gdp%gdimbound%xG_V1
    yG_V1              => gdp%gdimbound%yG_V1
    kfs_cc             => gdp%gdimbound%kfs_cc
    free_S1_sud        => gdp%gdimbound%free_S1_sud
    u1inp              => gdp%gdimbound%Dwrkak6
!          COMMENTED, DEPTH SHOULD NOT BE NEEDED
          if ((GHOSTimpl.eq.0).AND.(iter.gt.1)) then !it is explcit but I still have to reimpose hu and hv  them since they are recomputed at the end of each iteration
!             if (icx.eq.1) then ! along v   
!                nmaxOK = mmax  !mmax is nmax here   
!                do i = 1,totGHOSTv1   
!                   mGP = mGPv1(i)
!                   nGP = nGPv1(i) 
!                   nm = (mGP-1)*nmaxOK+nGP  
!                   hu(nm) = hu0(nm)
!                enddo
!             else
!                nmaxOK = nmax  !mmax is nmax here 
!                do i = 1,totGHOSTu1   
!                   mGP = mGPu1(i)
!                   nGP = nGPu1(i) 
!                   nm = (mGP-1)*nmaxOK+nGP !nmax is nmax
!                   hu(nm) = hu0(nm)
!                enddo
!             endif
          elseif ((GHOSTimpl.eq.1).AND.(iter.gt.1)) then
!             if (icx.eq.1) then ! along v            
!                huPROV(:) = hu(:) 
!                CALL interpG_ATv1LOCATION(huPROV,kcs,lunscr,Irov,mmax,nmax,nmaxus,kmax,nst,1,nlb,nub,mlb,mub,nmlb,nmub)   
!                  ! the value of the velocity ghost cell is NOT forced on the small cut edge but its computed normally
!                  ! therefore I dont need to use a ghost depth for small cut cells otherwise it would mess things 
!                  ! (eg in the case a lot of water accomulates in the small cut cell, hNEW=big, h(nm) computed by
!                  ! ghost is small, and bb(nm) assumes a small value while it should be close to 1
!                do i = 1,totGHOSTv1   
!                   mGP = mGPv1(i)
!                   nGP = nGPv1(i) 
!                   nm = (mGP-1)*nmaxOK+nGP  
!                   if (extrapGHOST1fluid2.eq.3.and.comparereal(aguu(nm),0._fp).gt.0) cycle
!                   !if extrapGHOST1fluid2==0.OR.extrapGHOST1fluid2==1.OR.extrapGHOST1fluid2==2 I prescribe it
!                   hu(nm) = huPROV(nm)
!                enddo
!             else
!                huPROV(:) = hu(:) 
!                CALL interpG_ATu1LOCATION(huPROV,kcs,lunscr,Irov,mmax,nmax,nmaxus,kmax,nst,1,nlb,nub,mlb,mub,nmlb,nmub)  
!                do i = 1,totGHOSTu1   
!                   mGP = mGPu1(i)
!                   nGP = nGPu1(i) 
!                   nm = (mGP-1)*nmaxOK+nGP  
!                   if (extrapGHOST1fluid2.eq.3.and.comparereal(aguu(nm),0._fp).gt.0) cycle
!                   !if extrapGHOST1fluid2==0.OR.extrapGHOST1fluid2==1.OR.extrapGHOST1fluid2==2 I prescribe it
!                   hu(nm) = huPROV(nm)
!                enddo
!             endif     
!             !recompute the ghost value at water surface point if cutcell is implicit 
              if (free_S1_sud==0) then
                 CALL interpG_ATs1LOCATION(s1,lunscr,Irov,mmax,nmax,nmaxus,kmax,nst,nlb,nub,mlb,mub,nmlb,nmub, gdp)   !if s1 is never prescribed below, this is not needed
              endif
          endif
          !recompute the ghost value at u-velocity point if cutcell is implicit (note that only u and s1 are updated, not v)
          !note that if extrapGHOST1fluid2>0 IT CAN BE REMOVED!!!!! I dont need the ghost point anymore, if the edge is dry nothing happens (and velocity is
          !beside removing, I should also set kfu to zero above at ghost points with aguu==0 after the ghost value is used, otherwise itr is set to 1 below and the iteartion is reapated
          ! set to zero in extraV anyways> this is not true for 3D with Z-layers, lower layer active upper un-active
          if (extrapGHOST1fluid2==3) then
             !continue
             !Vel at ghost points are never used in the sud iterations,cause of cut in continuity that rule out that velocity point.
             ! Only the explicit values at time t0 is used.
          elseif(extrapGHOST1fluid2==0) then
             if ((GHOSTimpl.eq.1).AND.(iter.gt.1)) then
                if (icx.eq.1) then ! along v    
                   IF ((Irov/=0.AND.Irov/=3).or.TYPEfreeSLIP==1) then 
                      do k=1,kmax
                         CALL interpG_ATv1LOCATION(u1(nmlb,k),kcs,lunscr,Irov,nmax,mmax,nmaxus,kmax,nst,-1,nlb,nub,mlb,mub,nmlb,nmub,0._fp, gdp) !mmax,nmax are correctly inverted but they are never used                 
                      enddo
                   ELSEIF (TYPEfreeSLIP==0) then 
                      CALL interpG_ATv1LOCATION_exactFREE(v1INTu,u1,kcs,lunscr,mmax,nmax,nmaxus,kmax,nst,nlb,nub,mlb,mub,nmlb,nmub,idummy1,idummy2,0._fp, gdp) !v1INTu is u1INTv, u1 is v1
                   ELSEIF (TYPEfreeSLIP==2) then  
                      !icx is icy, u1 is v1
                      CALL interpG_ATu1LOCATION_hart(icx,icy,u1,ghostv1,totGHOSTv1,Nx,Ny,xG_V1,yG_V1,kfs_cc,lunscr,kmax,nst,nmlb,nmub,ddbound,0._fp, gdp)
                   ENDIF
                   call UorVmean_ghosts(u1,nmax,mmax,nmaxus,kmax,nst,totGHOSTv1,mGPv1,nGPv1,Umean,thick,nlb,nub,mlb,mub,nmlb,nmub) !u1,Umean are indeed along v
                else
                   IF ((Irov/=0.AND.Irov/=3).or.TYPEfreeSLIP==1) then 
                      do k=1,kmax
                         CALL interpG_ATu1LOCATION(u1(nmlb,k),kcs,lunscr,Irov,mmax,nmax,nmaxus,kmax,nst,-1,nlb,nub,mlb,mub,nmlb,nmub,0._fp, gdp)
                      enddo      
                   ELSEif (TYPEfreeSLIP==0) then 
                      CALL interpG_ATu1LOCATION_exactFREE(u1,v1INTu,kcs,lunscr,mmax,nmax,nmaxus,kmax,nst,nlb,nub,mlb,mub,nmlb,nmub,idummy1,idummy2,0._fp, gdp)   !dummy values for exitloop and iterFR!v1INTu is v1INTu
                   ELSEIF (TYPEfreeSLIP==2) then  
                      !icy is icy, u1 is u1
                      CALL interpG_ATu1LOCATION_hart(icx,icy,u1,ghostu1,totGHOSTu1,Nx,Ny,xG_U1,yG_U1,kfs_cc,lunscr,kmax,nst,nmlb,nmub,ddbound,0._fp, gdp)
                   ENDIF
                   call UorVmean_ghosts(u1,mmax,nmax,nmaxus,kmax,nst,totGHOSTu1,mGPu1,nGPu1,Umean,thick,nlb,nub,mlb,mub,nmlb,nmub)             
                endif             
             endif
          else
             !write(*,*) 'Obsolete option, code has been commented'
             !call d3stop(1, gdp)
             !
             ! i should commented the code below, too much stuff to pass
             !
             u1inp(nmlb:nmub,1:kmax) = u1(nmlb:nmub,1:kmax) ! I use this since I am afraid that passing u1,u1 can give aliasing problem since u1 is modified http://www.sunsite.ualberta.ca/Documentation/Gnu/gcc-3.0.2/html_node/g77_561.html
             if (icx.eq.1) then ! along v     (nmax is mmax and mmax is mmax)             
                call extrapV(nst,kcs,kfu,u1inp,u1,Umean,qxk,hu,thick,guu,nmax,mmax,kmax,nmaxus,nlb,nub,mlb,mub,nmlb,nmub,Irov,1,aa,bb,cc,dd,aak,bbk,cck,ddk,gdp) 
             else
                call extrapU(nst,kcs,kfu,u1inp,u1,Umean,qxk,hu,thick,guu,mmax,nmax,kmax,nmaxus,nlb,nub,mlb,mub,nmlb,nmub,Irov,1,aa,bb,cc,dd,aak,bbk,cck,ddk,gdp) 
             endif
          endif
          if (extrapGHOST1fluid2==0.or.extrapGHOST1fluid2==3) then
             ! prescribe v and u at ghost cells
             if (icx.eq.1) then ! along v    
                nmaxOK = mmax  !mmax is nmax here       
                do i = 1,totGHOSTv1   
                   mGP = mGPv1(i)
                   nGP = nGPv1(i) 
                   nm = (mGP-1)*nmaxOK+nGP  
                   if (extrapGHOST1fluid2==3.and.comparereal(aguu(nm),0._fp).ne.0) cycle
                   aa(nm) = 0._fp
                   bb(nm) = 1._fp
                   cc(nm) = 0._fp
                   dd(nm) = umean(nm) !*hu(nm) !NOTE umean=vmean ,hu=hv. NOTE IT IS SOLVED FOR umean, since dd has the dimension of a velocity (it is multiplied by an area below)
                   DO K=1,kmax
                      aak(nm, k) = 0._fp !these are only used to compute u1 for each layer from s1 at the end of the subroutine
                      bbk(nm, k) = 1._fp
                      cck(nm, k) = 0._fp
                      ddk(nm, k) = u1(nm,k) 
                  !    write(3333302,'(6i6,15f21.15)') nst,nm,mGP,nGP,i,k,u1(nm, k),umean(nm)*hu(nm)
                   ENDDO           
                enddo
             else
                nmaxOK =nmax       
                do i = 1,totGHOSTu1   
                   mGP = mGPu1(i)
                   nGP = nGPu1(i) 
                   nm = (mGP-1)*nmaxOK+nGP !nmax is nmax
                   if (extrapGHOST1fluid2==3.and.comparereal(aguu(nm),0._fp).ne.0) cycle
                   aa(nm) = 0._fp
                   bb(nm) = 1._fp
                   cc(nm) = 0._fp
                   dd(nm) = umean(nm) !*hu(nm) !NOTE umean=umean ,hu=hu  NOTE IT IS SOLVED FOR umean, since dd has the dimension of a velocity (it is multiplied by an area below)
                   DO K=1,kmax
                      aak(nm, k) = 0._fp !these are only used to compute u1 for each layer from s1 at the end of the subroutine
                      bbk(nm, k) = 1._fp
                      cck(nm, k) = 0._fp
                      ddk(nm, k) = u1(nm,k) 
                   !   write(3333302,'(6i6,15f21.15)') nst,nm,mGP,nGP,i,k,u1(nm, k),umean(nm)*hu(nm)
                   ENDDO
                enddo
             endif
          endif
!
!
   RETURN 
end subroutine forceGHOSTsud
