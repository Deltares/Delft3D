subroutine recompGHOSTiterUZD(icx        ,icy        ,u1         ,v          ,u0INTv     ,&
                            & v0INTu     ,kcs        ,guu        ,xcor       ,ycor       ,&
                            & gvv        ,mmax       ,nmax       ,kmax                   ,&
                            & nst        ,nlb        ,nub        ,mlb        ,mub        ,&
                            & nmlb       ,nmub       ,iter       ,Irov       ,ddbound    ,&
                            & kcu        ,lunscr     ,nmmax      ,nmaxus     ,gdp)
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
!   Function: Call subroutine that are needed for cutcells before in uzd iterations
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
    integer, dimension(:,:)       , pointer :: ghostu1
    integer, dimension(:,:)       , pointer :: ghostv1
    integer                       , pointer :: totGHOSTu1
    integer                       , pointer :: totGHOSTv1
    integer, dimension(:)         , pointer :: mGPv1
    integer, dimension(:)         , pointer :: nGPv1
    integer, dimension(:)         , pointer :: mGPu1
    integer, dimension(:)         , pointer :: nGPu1
    integer                       , pointer :: TYPEfreeSLIP
    logical                       , pointer :: ITERATEfree
    logical                       , pointer :: freeU0fixed
    real(fp), dimension(:,:)      , pointer :: Nx
    real(fp), dimension(:,:)      , pointer :: Ny
    real(fp), dimension(:,:)      , pointer :: xG_U1
    real(fp), dimension(:,:)      , pointer :: yG_U1
    real(fp), dimension(:,:)      , pointer :: xG_V1
    real(fp), dimension(:,:)      , pointer :: yG_V1
    integer, dimension(:,:)       , pointer :: kfs_cc
    real(fp), dimension(:,:)      , pointer :: PSIx
    real(fp), dimension(:,:)      , pointer :: PSIy
    real(fp), dimension(:,:)      , pointer :: ETAx
    real(fp), dimension(:,:)      , pointer :: ETAy
    real(fp), dimension(:,:)      , pointer :: xcorV1
    real(fp), dimension(:,:)      , pointer :: ycorV1
    real(fp), dimension(:,:)      , pointer :: xcorU1
    real(fp), dimension(:,:)      , pointer :: ycorU1
    real(fp), dimension(:,:,:,:,:), pointer :: EDGExyBANK
    real(fp), dimension(:,:)      , pointer :: aguu
    real(fp), dimension(:,:)      , pointer :: agvv
!
! global variables 
!
    real(fp), dimension(nmlb:nmub,kmax)                                 , intent(inout) :: u1     
    real(fp), dimension(nmlb:nmub,kmax)                                 , intent(inout) :: v    
    real(fp), dimension(nmlb:nmub,kmax)                                 , intent(inout) :: u0INTv
    real(fp), dimension(nmlb:nmub,kmax)                                 , intent(inout) :: v0INTu
    real(fp), dimension(nmlb:nmub)                                      , intent(in)    :: guu
    real(fp), dimension(nmlb:nmub)                                      , intent(in)    :: gvv    
    real(fp), dimension(nmlb:nmub)                                      , intent(in)    :: xcor    
    real(fp), dimension(nmlb:nmub)                                      , intent(in)    :: ycor    
    integer, dimension(nmlb:nmub)                                       , intent(in)    :: kcu       
    integer, dimension(nmlb:nmub)                                       , intent(in)    :: kcs
    integer                                                             , intent(in)    :: nmlb
    integer                                                             , intent(in)    :: nmub
    integer                                                             , intent(in)    :: icx
    integer                                                             , intent(in)    :: icy
    integer                                                             , intent(in)    :: mmax
    integer                                                             , intent(in)    :: nmax
    integer                                                             , intent(in)    :: nlb
    integer                                                             , intent(in)    :: nub
    integer                                                             , intent(in)    :: mlb
    integer                                                             , intent(in)    :: mub
    integer                                                             , intent(in)    :: kmax
    integer                                                             , intent(in)    :: iter
    integer                                                             , intent(in)    :: nst
    integer                                                             , intent(in)    :: Irov
    integer                                                             , intent(in)    :: ddbound
    integer                                                             , intent(in)    :: lunscr
    integer                                                             , intent(in)    :: nmaxus
    integer                                                             , intent(in)    :: nmmax
!
! local variables
!
   integer :: i
   integer :: k
   integer :: mGP
   integer :: nGP
   integer :: nm
   integer :: nmaxOK
   integer :: iterFR 
   integer :: exitloop 
   REAL(FP),ALLOCATABLE       :: Vexact(:,:)
   REAL(FP),ALLOCATABLE       :: Uexact(:,:)   
!
! executable statements -------------------------------------------------------
!   
    ghostu1         => gdp%gdimbound%ghostu1
    ghostv1         => gdp%gdimbound%ghostv1
    totGHOSTu1      => gdp%gdimbound%totGHOSTu1
    totGHOSTv1      => gdp%gdimbound%totGHOSTv1
    mGPv1           => gdp%gdimbound%mGPv1
    nGPv1           => gdp%gdimbound%nGPv1
    mGPu1           => gdp%gdimbound%mGPu1
    nGPu1           => gdp%gdimbound%nGPu1
    TYPEfreeSLIP    => gdp%gdimbound%TYPEfreeSLIP
    ITERATEfree     => gdp%gdimbound%ITERATEfree
    TYPEfreeSLIP    => gdp%gdimbound%TYPEfreeSLIP
    freeU0fixed     => gdp%gdimbound%freeU0fixed
    Nx              => gdp%gdimbound%Nx
    Ny              => gdp%gdimbound%Ny
    xG_U1           => gdp%gdimbound%xG_U1
    yG_U1           => gdp%gdimbound%yG_U1
    xG_V1           => gdp%gdimbound%xG_V1
    yG_V1           => gdp%gdimbound%yG_V1
    kfs_cc          => gdp%gdimbound%kfs_cc
    PSIx            => gdp%gdimbound%PSIx
    PSIy            => gdp%gdimbound%PSIy
    ETAx            => gdp%gdimbound%ETAx
    ETAy            => gdp%gdimbound%ETAy
    xcorV1          => gdp%gdimbound%xcorV1
    ycorV1          => gdp%gdimbound%ycorV1
    xcorU1          => gdp%gdimbound%xcorU1
    ycorU1          => gdp%gdimbound%ycorU1
    EDGExyBANK      => gdp%gdimbound%EDGExyBANK
    aguu            => gdp%gdimbound%aguu
    agvv            => gdp%gdimbound%agvv
      if (icx.eq.1) then ! along v   
         IF ((Irov/=0.AND.Irov/=3).or.TYPEfreeSLIP==1) then 
            do k=1,kmax
               CALL interpG_ATv1LOCATION(u1(nmlb,k),kcs,lunscr,Irov,mmax,nmax,nmaxus,kmax,nst,-1,nlb,nub,mlb,mub,nmlb,nmub,1._fp, gdp)                  
            enddo
         ELSEIF (TYPEfreeSLIP==0) then 
            if (.not.ITERATEfree.or.freeU0fixed) then
               CALL interpG_ATv1LOCATION_exactFREE(v0INTu,u1,kcs,lunscr,mmax,nmax,nmaxus,kmax,nst,nlb,nub,mlb,mub,nmlb,nmub,exitloop,iterFR,1._fp, gdp) !v0INTu is u0INTv, u1 is v1                
            else
               iterFR = 0
               exitloop = 0
               do while (exitloop == 0 .and. iterFR < 50)
                  exitloop = 1
                  iterFR =iterFR+1
                  CALL interpUinV_FROMu1STENCIL_exactFREE(v,u0INTv,v0INTu,kcs,lunscr,mmax,nmax,nmaxus,kmax,nst,nlb,nub,mlb,mub,nmlb,nmub,exitloop,iterFR, gdp)
                  CALL interpVinU_FROMv1STENCIL_exactFREE(v0INTu,u1,u0INTv,kcs,lunscr,mmax,nmax,nmaxus,kmax,nst,nlb,nub,mlb,mub,nmlb,nmub,exitloop,iterFR, gdp)
                  CALL interpG_ATu1LOCATION_exactFREE(v,u0INTv,kcs,lunscr,mmax,nmax,nmaxus,kmax,nst,nlb,nub,mlb,mub,nmlb,nmub,exitloop,iterFR,1._fp, gdp) !used un uzd and also sud
                  CALL interpG_ATv1LOCATION_exactFREE(v0INTu,u1,kcs,lunscr,mmax,nmax,nmaxus,kmax,nst,nlb,nub,mlb,mub,nmlb,nmub,exitloop,iterFR,1._fp, gdp) !used in uzd
                  !solvedU_ATv =.true. !u has to be solved only the first time of the iteration (for the case in which only fluid points are used in the interpolation)
                  !    if (nmax*mmax<10000) write(9090898,*) nst,iterFR,'uzd stage1'
               enddo
               if (iterFR >= 50) then
                  write(*,*) 'Ghost velocity does not converge'
                  call d3stop(1, gdp)
               endif
            endif
         ELSEIF (TYPEfreeSLIP==2) then  
            !icx is icy, u1 is v1
            CALL interpG_ATu1LOCATION_hart(icx,icy,u1,ghostv1,totGHOSTv1,Nx,Ny,xG_V1,yG_V1,kfs_cc,lunscr,kmax,nst,nmlb,nmub,ddbound,1._fp, gdp)
         ENDIF
      else
         IF ((Irov/=0.AND.Irov/=3).or.TYPEfreeSLIP==1) then 
            do k=1,kmax
               CALL interpG_ATu1LOCATION(u1(nmlb,k),kcs,lunscr,Irov,mmax,nmax,nmaxus,kmax,nst,-1,nlb,nub,mlb,mub,nmlb,nmub,1._fp, gdp)
            enddo     
         ELSEIF (TYPEfreeSLIP==0) then 
            if (.not.ITERATEfree.or.freeU0fixed) then
               CALL interpG_ATu1LOCATION_exactFREE(u1,v0INTu,kcs,lunscr,mmax,nmax,nmaxus,kmax,nst,nlb,nub,mlb,mub,nmlb,nmub,exitloop,iterFR,1._fp, gdp) !v0INTu is v0INTu               
            else
               iterFR = 0
               exitloop = 0
               do while (exitloop == 0 .and. iterFR < 50)
                  exitloop = 1
                  iterFR = iterFR+1                      
                  CALL interpUinV_FROMu1STENCIL_exactFREE(u1,v0INTu,u0INTv,kcs,lunscr,mmax,nmax,nmaxus,kmax,nst,nlb,nub,mlb,mub,nmlb,nmub,exitloop,iterFR, gdp)
                  CALL interpVinU_FROMv1STENCIL_exactFREE(u0INTv,v,v0INTu,kcs,lunscr,mmax,nmax,nmaxus,kmax,nst,nlb,nub,mlb,mub,nmlb,nmub,exitloop,iterFR, gdp)
                  CALL interpG_ATu1LOCATION_exactFREE(u1,v0INTu,kcs,lunscr,mmax,nmax,nmaxus,kmax,nst,nlb,nub,mlb,mub,nmlb,nmub,exitloop,iterFR,1._fp, gdp) !used un uzd and also sud
                  CALL interpG_ATv1LOCATION_exactFREE(u0INTv,v,kcs,lunscr,mmax,nmax,nmaxus,kmax,nst,nlb,nub,mlb,mub,nmlb,nmub,exitloop,iterFR,1._fp, gdp) !used in uzd
                  !solvedU_ATv =.true. !u has to be solved only the first time of the iteration (for the case in which only fluid points are used in the interpolation)
                  !   if (nmax*mmax<10000)  write(9090898,*) nst,iterFR,iter,'uzd stage2'
               enddo
               if (iterFR >= 50) then
                  write(*,*) 'Ghost velocity does not converge'
                  call d3stop(1, gdp)
               endif
            endif
         ELSEIF (TYPEfreeSLIP==2) then  
            !icy is icy, u1 is u1
            CALL interpG_ATu1LOCATION_hart(icx,icy,u1,ghostu1,totGHOSTu1,Nx,Ny,xG_U1,yG_U1,kfs_cc,lunscr,kmax,nst,nmlb,nmub,ddbound,1._fp, gdp)
         ENDIF       
         if (icx.eq.1) then ! along v   
            call DISSvelGHOST_uzd(u1,mGPv1,nGPv1,totGHOSTv1,nlb,nub,mlb,mub,kmax, gdp)
         else
            call DISSvelGHOST_uzd(u1,mGPu1,nGPu1,totGHOSTu1,nlb,nub,mlb,mub,kmax, gdp)
         endif      
      endif    
!
   RETURN 
end subroutine recompGHOSTiterUZD
