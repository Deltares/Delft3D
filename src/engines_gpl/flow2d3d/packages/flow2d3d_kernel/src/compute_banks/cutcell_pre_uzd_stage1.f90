subroutine cutcell_pre_uzd_stage1(icx        ,icy        ,u0         ,v0         ,u1         ,&
                                & u0INTv     ,v0INTu     ,guu        ,gvv        ,xcor       ,&
                                & ycor       ,&
                                & v1         ,gsqs       ,kcs        ,dpu        ,dpv        ,&
                                & Umean      ,Vmean      ,thick      ,qxk        ,qyk        ,&
                                & hu         ,hv         ,s0         ,s1         ,dps        ,&
                                & kfs        ,kfu        ,kfv        ,kcu        ,kcv        ,&
                                & mmax       ,nmax       ,nmmax      ,nmaxus     ,kmax       ,&
                                & nst        ,nlb        ,nub        ,mlb        ,mub        ,&
                                & nmlb       ,nmub       ,ddbound    ,lunscr     ,Irov       ,gdp)
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
!   Function: Call subroutine that are needed for cutcells before uzd computation (stage 1)
!             It computes u0 and v0 at ghost points and set the masking coefficient to 1.
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
    integer                       , pointer :: freeNONhomo
    integer                       , pointer :: PERIODalongM
    integer                       , pointer :: free_S1_sud
    integer                       , pointer :: totGHOSTu1
    integer                       , pointer :: totGHOSTv1
    integer                       , pointer :: TYPEfreeSLIP
    integer, dimension(:,:)       , pointer :: kfs_cc
    integer, dimension(:)         , pointer :: nGPu1
    integer, dimension(:)         , pointer :: mGPu1
    integer, dimension(:)         , pointer :: nGPv1
    integer, dimension(:)         , pointer :: mGPv1
    integer, dimension(:,:)       , pointer :: inSTENCILu
    integer, dimension(:,:)       , pointer :: inSTENCILv
    integer, dimension(:,:)       , pointer :: GHOSTu1
    integer, dimension(:,:)       , pointer :: GHOSTv1
    real(fp), dimension(:,:)      , pointer :: POROS
    real(fp), dimension(:,:)      , pointer :: PSIx
    real(fp), dimension(:,:)      , pointer :: PSIy
    real(fp), dimension(:,:)      , pointer :: ETAx
    real(fp), dimension(:,:)      , pointer :: ETAy
    real(fp), dimension(:,:,:)    , pointer :: vFACcut
    real(fp), dimension(:,:,:,:,:), pointer :: EDGExyBANK
    real(fp), dimension(:,:)      , pointer :: aguu
    real(fp), dimension(:,:)      , pointer :: agvv
    real(fp), dimension(:,:)      , pointer :: xcorU1
    real(fp), dimension(:,:)      , pointer :: ycorU1
    real(fp), dimension(:,:)      , pointer :: xcorV1
    real(fp), dimension(:,:)      , pointer :: ycorV1
    real(fp), dimension(:,:)      , pointer :: Nx
    real(fp), dimension(:,:)      , pointer :: Ny
    real(fp), dimension(:,:)      , pointer :: xG_V1
    real(fp), dimension(:,:)      , pointer :: xG_U1
    real(fp), dimension(:,:)      , pointer :: yG_V1
    real(fp), dimension(:,:)      , pointer :: yG_U1
    real(fp), dimension(:,:,:)    , pointer :: u1_FLLYghst
    logical                       , pointer :: interpS1beforeUZD
    logical                       , pointer :: periodSURFACE
    logical                       , pointer :: callSUBR_WATERlevelPERIOD
    logical                       , pointer :: solvedU_ATu
    logical                       , pointer :: solvedV_ATu
    logical                       , pointer :: solvedU_ATv
    logical                       , pointer :: solvedV_ATv
    logical                       , pointer :: ITERATEfree
    logical                       , pointer :: freeU0fixed
    logical                       , pointer :: interpVinUexact
    logical                       , pointer :: changeKFUVcut
    logical                       , pointer :: useFULL
!
! global variables 
!
    real(fp), dimension(nmlb:nmub,kmax)                                 , intent(inout)    :: u0INTv
    real(fp), dimension(nmlb:nmub,kmax)                                 , intent(inout)    :: v0INTu
    real(fp), dimension(nmlb:nmub,kmax)                                 , intent(inout)    :: u0
    real(fp), dimension(nmlb:nmub,kmax)                                 , intent(inout)    :: u1
    real(fp), dimension(nmlb:nmub,kmax)                                 , intent(inout)    :: v0
    real(fp), dimension(nmlb:nmub,kmax)                                 , intent(inout)    :: v1
    real(fp), dimension(nmlb:nmub,kmax)                                 , intent(inout)    :: qxk
    real(fp), dimension(nmlb:nmub,kmax)                                 , intent(inout)    :: qyk
    real(fp), dimension(nmlb:nmub)                                      , intent(in)       :: hu
    real(fp), dimension(nmlb:nmub)                                      , intent(in)       :: hv
    real(fp), dimension(nmlb:nmub)                                      , intent(in)       :: guu
    real(fp), dimension(nmlb:nmub)                                      , intent(in)       :: gvv
    real(fp), dimension(nmlb:nmub)                                      , intent(in)       :: xcor
    real(fp), dimension(nmlb:nmub)                                      , intent(in)       :: ycor
    real(fp), dimension(nmlb:nmub)                                      , intent(in)       :: dpu
    real(fp), dimension(nmlb:nmub)                                      , intent(in)       :: dpv
    real(fp), dimension(nmlb:nmub)                                      , intent(in)       :: gsqs
    real(fp), dimension(nmlb:nmub)                                      , intent(inout)    :: Umean
    real(fp), dimension(nmlb:nmub)                                      , intent(inout)    :: Vmean
    real(fp), dimension(nmlb:nmub)                                      , intent(in)       :: s1
    real(fp), dimension(nmlb:nmub)                                      , intent(inout)    :: s0  
    real(prec), dimension(nmlb:nmub)                                    , intent(inout)    :: dps  
    real(fp), dimension(kmax)                                           , intent(in)       :: thick   !  Description and declaration in esm_alloc_real.f90
    integer, dimension(nmlb:nmub)                                       , intent(in)    :: kcs
    integer, dimension(nmlb:nmub)                                       , intent(in)    :: kfs
    integer, dimension(nmlb:nmub)                                       , intent(inout) :: kfu
    integer, dimension(nmlb:nmub)                                       , intent(inout) :: kfv
    integer, dimension(nmlb:nmub)                                       , intent(in)    :: kcu
    integer, dimension(nmlb:nmub)                                       , intent(in)    :: kcv
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
!nub,nlb
! local variables
!
  integer                    :: exitloop                   
  integer                    :: nm
  integer                    :: nmj
  integer                    :: k   
  integer                    :: nmk(4)
  integer                    :: iterFR
  REAL(FP),ALLOCATABLE       :: Vexact(:,:)
  REAL(FP),ALLOCATABLE       :: Uexact(:,:)
!
!
! executable statements -------------------------------------------------------
!     
    freeNONhomo               => gdp%gdimbound%freeNONhomo
    PERIODalongM              => gdp%gdimbound%PERIODalongM
    free_S1_sud               => gdp%gdimbound%free_S1_sud
    totGHOSTu1                => gdp%gdimbound%totGHOSTu1
    totGHOSTv1                => gdp%gdimbound%totGHOSTv1
    TYPEfreeSLIP              => gdp%gdimbound%TYPEfreeSLIP
    kfs_cc                    => gdp%gdimbound%kfs_cc
    nGPu1                     => gdp%gdimbound%nGPu1
    mGPu1                     => gdp%gdimbound%mGPu1
    nGPv1                     => gdp%gdimbound%nGPv1
    mGPv1                     => gdp%gdimbound%mGPv1
    inSTENCILu                => gdp%gdimbound%inSTENCILu
    inSTENCILv                => gdp%gdimbound%inSTENCILv
    GHOSTu1                   => gdp%gdimbound%GHOSTu1
    GHOSTv1                   => gdp%gdimbound%GHOSTv1
    POROS                     => gdp%gdimbound%POROS
    PSIx                      => gdp%gdimbound%PSIx
    PSIy                      => gdp%gdimbound%PSIy
    ETAx                      => gdp%gdimbound%ETAx
    ETAy                      => gdp%gdimbound%ETAy
    vFACcut                   => gdp%gdimbound%vFACcut
    EDGExyBANK                => gdp%gdimbound%EDGExyBANK
    aguu                      => gdp%gdimbound%aguu
    agvv                      => gdp%gdimbound%agvv
    xcorU1                    => gdp%gdimbound%xcorU1
    ycorU1                    => gdp%gdimbound%ycorU1
    xcorV1                    => gdp%gdimbound%xcorV1
    ycorV1                    => gdp%gdimbound%ycorV1
    Nx                        => gdp%gdimbound%Nx
    Ny                        => gdp%gdimbound%Ny
    xG_V1                     => gdp%gdimbound%xG_V1
    xG_U1                     => gdp%gdimbound%xG_U1
    yG_V1                     => gdp%gdimbound%yG_V1
    yG_U1                     => gdp%gdimbound%yG_U1
    u1_FLLYghst               => gdp%gdimbound%u1_FLLYghst
    interpS1beforeUZD         => gdp%gdimbound%interpS1beforeUZD
    periodSURFACE             => gdp%gdimbound%periodSURFACE
    callSUBR_WATERlevelPERIOD => gdp%gdimbound%callSUBR_WATERlevelPERIOD
    solvedU_ATu               => gdp%gdimbound%solvedU_ATu
    solvedV_ATu               => gdp%gdimbound%solvedV_ATu
    solvedU_ATv               => gdp%gdimbound%solvedU_ATv
    solvedV_ATv               => gdp%gdimbound%solvedV_ATv
    ITERATEfree               => gdp%gdimbound%ITERATEfree
    freeU0fixed               => gdp%gdimbound%freeU0fixed
    interpVinUexact           => gdp%gdimbound%interpVinUexact
    changeKFUVcut             => gdp%gdimbound%changeKFUVcut
    useFULL                   => gdp%gdimbound%useFULL
   ! note f0isf1 has just been called in trisol and the values coincide (I can use either var0 or var1). However,
          ! u0 only is passed to uzd (not u1). And v0 is used to compute all the terms of momentum eq. So I use u0 and v0 to
          ! interpolate velocities. Then before iter cycle in uzd is set v1=v0 and v1 computed iteratively. So v1 is not needed now at ghost points
    if (useFULL) THEN
       call u1isuFULL(u1_FLLYghst,u0,nlb,nub,mlb,mub,kmax, gdp) !note for zmodel:new ghost points in the vertical can occur also when water surface moves vertical
    endif
    IF ((Irov/=0.AND.Irov/=3).or.TYPEfreeSLIP==1) then !du/dn=0,dv/dn=0
       do k=1,kmax
         if (.not.useFULL) CALL interpG_ATu1LOCATION(u0(nmlb,k),kcs,lunscr,Irov,mmax,nmax,nmaxus,kmax,nst,-1,nlb,nub,mlb,mub,nmlb,nmub,1._fp, gdp)!!needed only because I set v1 to zero at ghosts between stages, OTHERWISE IT WOULD BE ACTUALLY NEEDED ONLY FIRST TIME ADI IS CALLED, SINCE IT IS COMPUTED AFTER uzd in stage 2
         CALL interpG_ATv1LOCATION(v0(nmlb,k),kcs,lunscr,Irov,mmax,nmax,nmaxus,kmax,nst,-1,nlb,nub,mlb,mub,nmlb,nmub,1._fp, gdp) !NOTE:v0 is needed at least for explicit wall stress and explicit SPIRAL MOTION INTENSITY. So I comput v0. Moreover this avoid to reconstruct v1, since v1 becomes v0 before the iteration loop
         !CALL v1isv0_ghost(v0(nmlb,k),v1(nmlb,k),nlb,nub,mlb,mub,kmax) !not necessary since v1 is not used in uzd (v0 is copied in v1 before starting iteration)
         !CALL u1isu0_ghost(u0(nmlb,k),u1(nmlb,k),nlb,nub,mlb,mub,kmax) !not necessary since u1 is not passed to uzd
       enddo
    ELSEIF (TYPEfreeSLIP==0) then 
       solvedU_ATu = .false.      
       solvedV_ATu = .false.
       solvedU_ATv = .false.      
       solvedV_ATv = .false.
       if (.not.ITERATEfree) then !if (.not.ITERATEfree.or.freeU0fixed) then 
          CALL INTERPvATuPOINT(u0INTv,u0,ghostu1,inSTENCILv,kcs,agvv,gsqs,icx,icy,kmax,nmmax,nst,nlb,nub,mlb,mub,nmlb,nmub, gdp) !note:icx here is = 1, i.e. it is along y
       !   CALL INTERPvATuPOINT(v0INTu,v0,ghostv1,inSTENCILu,kcs,aguu,gsqs,icy,icx,kmax,nmmax,nst,nlb,nub,mlb,mub,nmlb,nmub) !inverted icy and icx
          !CALL INTERPvATuPOINT(v0INTu,v0,ghostv1,inSTENCILu,kcs,aguu,gsqs,icy,icx,kmax,nmmax,nst,nlb,nub,mlb,mub,nmlb,nmub)
          !CALL INTERPvelATotherVELpoint(v0intU,u0,ghostu1,kfs,gsqs,kmax,nmmax,nst,nlb,nub,mlb,mub,nmlb,nmub)
         ! CALL interpG_ATu1LOCATION_exactFREE(u0,v0INTu,kcs,lunscr,mmax,nmax,nmaxus,kmax,nst,nlb,nub,mlb,mub,nmlb,nmub,exitloop,iterFR,0._fp)
          CALL interpG_ATv1LOCATION_exactFREE(u0INTv,v0,kcs,lunscr,mmax,nmax,nmaxus,kmax,nst,nlb,nub,mlb,mub,nmlb,nmub,exitloop,iterFR,1._fp, gdp)
          !solvedU_ATv =.true. !u has to be solved only the first time of the iteration (for the case in which only fluid points are used in the interpolation)
       else
          !get hartman approximation of u0 and v0 at ghost points AS FIRST GUESS
          if (periodSURFACE) THEN  !make halo periodic TO BE SURE before calling interpG_ATu1LOCATION_hart
             call periodic_vATuPOINTS(u0,nlb,nub,mlb,mub,kmax, gdp)
             call periodic_uATvPOINTS(v0,nlb,nub,mlb,mub,kmax, gdp)
          endif
          CALL interpG_ATu1LOCATION_hart(icy,icx,u0,ghostu1,totGHOSTu1,Nx,Ny,xG_U1,yG_U1,kfs_cc,lunscr,kmax,nst,nmlb,nmub,ddbound,1._fp, gdp)   !This is in order not to have ghost points non-initialized for freeNONhomo>0 (gradient is computed using ghost point)
          CALL interpG_ATu1LOCATION_hart(icx,icy,v0,ghostv1,totGHOSTv1,Nx,Ny,xG_V1,yG_V1,kfs_cc,lunscr,kmax,nst,nmlb,nmub,ddbound,1._fp, gdp)    !This is in order not to have ghost points non-initialized for freeNONhomo>0 (gradient is computed using ghost point)
        !  V0(613,1) = 0.882019787825636 
        !  WRITE(*,*) 'CANCEL V1(613,1)'
          ! note here the effect of "call u1isuFULL"  is overwritten 
          iterFR = 0
          exitloop = 0
          do while (exitloop == 0 .and. iterFR < 50)
             exitloop = 1
             iterFR =iterFR+1
             if (periodSURFACE) THEN !make computed ghost periodic
                call periodic_vATuPOINTS(u0,nlb,nub,mlb,mub,kmax, gdp)
                call periodic_uATvPOINTS(v0,nlb,nub,mlb,mub,kmax, gdp)
             endif
             !compute interpolation u0INTv and v0INTu 
             IF (iterFR==1) then !compute it by bilinear interp without BC
                CALL INTERPvATuPOINT(u0INTv,u0,ghostu1,inSTENCILv,kcs,agvv,gsqs,icx,icy,kmax,nmmax,nst,nlb,nub,mlb,mub,nmlb,nmub, gdp) !note:icx here is = 1, i.e. it is along y
                CALL INTERPvATuPOINT(v0INTu,v0,ghostv1,inSTENCILu,kcs,aguu,gsqs,icy,icx,kmax,nmmax,nst,nlb,nub,mlb,mub,nmlb,nmub, gdp) !inverted icy and icx
                !get hartman approximation of v0INTu and u0INTv at ghost points
                IF (freeNONhomo>0) then
                   CALL interpG_ATu1LOCATION_hart(icy,icx,v0INTu,ghostu1,totGHOSTu1,Nx,Ny,xG_U1,yG_U1,kfs_cc,lunscr,kmax,nst,nmlb,nmub,ddbound,1._fp, gdp)   !This is in order not to have ghost points non-initialized for freeNONhomo>0 (gradient is computed using ghost point)
                   CALL interpG_ATu1LOCATION_hart(icx,icy,u0INTv,ghostv1,totGHOSTv1,Nx,Ny,xG_V1,yG_V1,kfs_cc,lunscr,kmax,nst,nmlb,nmub,ddbound,1._fp, gdp)    !This is in order not to have ghost points non-initialized for freeNONhomo>0 (gradient is computed using ghost point)
                endif
                exitloop = 0
             else !compute it by bilinear interp withBC
                IF (interpVinUexact) THEN
                   CALL interpUinV_FROMu1STENCIL_exactFREE(u0,v0INTu,u0INTv,kcs,lunscr,mmax,nmax,nmaxus,kmax,nst,nlb,nub,mlb,mub,nmlb,nmub,exitloop,iterFR, gdp)
                   CALL interpVinU_FROMv1STENCIL_exactFREE(u0INTv,v0,v0INTu,kcs,lunscr,mmax,nmax,nmaxus,kmax,nst,nlb,nub,mlb,mub,nmlb,nmub,exitloop,iterFR, gdp)
                ELSE
                   CALL INTERPvATuPOINT(u0INTv,u0,ghostu1,inSTENCILv,kcs,agvv,gsqs,icx,icy,kmax,nmmax,nst,nlb,nub,mlb,mub,nmlb,nmub, gdp) !note:icx here is = 1, i.e. it is along y
                   CALL INTERPvATuPOINT(v0INTu,v0,ghostv1,inSTENCILu,kcs,aguu,gsqs,icy,icx,kmax,nmmax,nst,nlb,nub,mlb,mub,nmlb,nmub, gdp) !inverted icy and icx
                ENDIF
             endif
             !
             ! DA CANCELLARE, IMPONGO  v0INTu and u0INTv esatte
             !
            ! call FORCE_u_circEX            (v0INTu      ,kfs_cc      ,PSIx        ,PSIy     ,guu         ,aguu        ,&                        
            !                             &   xcorV1      ,ycorV1      ,xG_U1       ,yG_U1    ,EDGExyBANK  ,xcor        ,&   
            !                             &   ycor        ,nmlb        ,nmub        ,nmmax    ,icy         ,icx         ,&
            !                             &   nst         ,kmax        ,kfu         ,1        ,0)  
            ! call FORCE_u_circEX            (u0INTv      ,kfs_cc      ,ETAx        ,ETAy     ,gvv         ,agvv        ,& !guu and aguu are gvv and agvv                        
            !                             &   xcorU1      ,ycorU1      ,xG_V1       ,yG_V1    ,EDGExyBANK  ,xcor        ,&   
            !                             &   ycor        ,nmlb        ,nmub        ,nmmax    ,icx         ,icy         ,&
            !                             &   nst         ,kmax        ,kfv         ,1        ,0) 
             !
             ! FINE DA CANCELLARe
             !
             if (periodSURFACE) THEN  !make computed u0INTv and v0INTu periodic
                call periodic_vATuPOINTS(v0INTu,nlb,nub,mlb,mub,kmax, gdp)
                call periodic_uATvPOINTS(u0INTv,nlb,nub,mlb,mub,kmax, gdp)
             endif
             CALL interpG_ATu1LOCATION_exactFREE(u0,v0INTu,kcs,lunscr,mmax,nmax,nmaxus,kmax,nst,nlb,nub,mlb,mub,nmlb,nmub,exitloop,iterFR,1._fp, gdp) !used un uzd and also sud
             CALL interpG_ATv1LOCATION_exactFREE(u0INTv,v0,kcs,lunscr,mmax,nmax,nmaxus,kmax,nst,nlb,nub,mlb,mub,nmlb,nmub,exitloop,iterFR,1._fp, gdp) !used in uzd
             !solvedU_ATv =.true. !u has to be solved only the first time of the iteration (for the case in which only fluid points are used in the interpolation)
             !
             ! DA CANCELLARE, COMPUTE EXACT FOR ACCURACY
              
            ! call FORCE_u_circEXdaCANC      (Vexact      ,v0          ,ghostv1     ,kfs_cc      ,ETAx        ,ETAy     ,gvv         ,agvv        ,& !guu and aguu are gvv and agvv                        
            !                             &   xcorU1      ,ycorU1      ,xG_V1       ,yG_V1    ,EDGExyBANK  ,xcor        ,&   
            !                             &   ycor        ,nmlb        ,nmub        ,nmmax    ,icx         ,icy         ,&
            !                             &   nst         ,kmax        ,kcv         ,kcs      ,0        ,0)  
        
               !note: (xcorV1(n-1,m),ycorV1(n-1,m) are the coord of the U1(n,m) velocity point)
             !
             ! FINE DA CANCELLARe
             !
             !if (nmax*mmax<10000) write(9090898,*) nst,iterFR
          enddo
          if (iterFR >= 50) then
             write(*,*) 'Ghost velocity does not converge'
             !call d3stop(1, gdp)
          endif
       endif
    ELSEIF (TYPEfreeSLIP==2) then  
       !here icx is icy and vice versa
       CALL interpG_ATu1LOCATION_hart(icy,icx,u0,ghostu1,totGHOSTu1,Nx,Ny,xG_U1,yG_U1,kfs_cc,lunscr,kmax,nst,nmlb,nmub,ddbound,1._fp, gdp)   !it was 0._fp I use 1 for bank erosion so I have always fresh grid with a value
       CALL interpG_ATu1LOCATION_hart(icx,icy,v0,ghostv1,totGHOSTv1,Nx,Ny,xG_V1,yG_V1,kfs_cc,lunscr,kmax,nst,nmlb,nmub,ddbound,1._fp, gdp)
    ENDIF
    !   interpolate depth and water surface at s0 ghost point (dps and s0 are used in uzd to compute slope term and other stuff). 
    !s00 = s0 ! in order to conserve mass for small cut cells (where s0 is a ghost point and comparereal(poros(nGP,mGP),0._fp))
    !if (interpS1beforeUZD.OR.free_S1_sud.EQ.0) THEN
    !   CALL interpG_ATs1LOCATION(s0,lunscr,Irov,mmax,nmax,nmaxus,kmax,nst,nlb,nub,mlb,mub,nmlb,nmub) ! we might consider to remove it (see my notes)
    !endif
   ! CALL interpG_ATs1LOCATION(dps,lunscr,Irov,mmax,nmax,nmaxus,kmax,nst,nlb,nub,mlb,mub,nmlb,nmub) mai usato in uzd, soprattututto se interpS1beforeUZD=false
      !this next line and the 2 discharges (interpG_ATu1LOCATION,qxORqy_ghosts,qxORqy_ghosts) might be commented since discharges and hu should not be used in uzd
    !CALL interpG_ATu1LOCATION(hu,kcs,lunscr,Irov,mmax,nmax,nmaxus,kmax,nst,1,nlb,nub,mlb,mub,nmlb,nmub) ! hu not used in uzd, if it is interpolated is only to compute discharge qx, but the latter should not be used in uzd (to be double checked)
   !interpolate  hv in ghost cells, needed for computing discharges and cause  hv is actually also used in uzd. note:checku recomputes hv everywhere so  I had to compute hv here after it.  
   !note May 2014: only hv in the ghost cell would be used, but note that the coefficient for the ghost cell are forced so
   ! hv is actually not needed (the adjacent hv is only used for immission of discharge, but it should not be used with cut cells,and also if it zero it should be fine)
   ! CALL interpG_ATv1LOCATION(hv,kcs,lunscr,Irov,mmax,nmax,nmaxus,kmax,nst,1,nlb,nub,mlb,mub,nmlb,nmub)
    !dpu and dpv not needed, they are not passed to uzd
   ! CALL interpG_ATu1LOCATION(dpU,kcs,lunscr,Irov,mmax,nmax,nmaxus,kmax,nst,1,nlb,nub,mlb,mub,nmlb,nmub)
   ! CALL interpG_ATv1LOCATION(dpV,kcs,lunscr,Irov,mmax,nmax,nmaxus,kmax,nst,1,nlb,nub,mlb,mub,nmlb,nmub)
    !! no need to compute interpolated discharges: those are used only for fluxes in sud and I do not want to use the wrong ghost value to compute fluxes! and we can use the explicit one computed from previous sud ( moreover I think they are never used in uzd though or only for special structures, double check). Whats the point of a special structure on the ghost point next to the moving bank?
    !call qxORqy_ghosts(u0,hu,guu,aguu,thick,porosu,qxk,mmax,nmax,nmaxus,kmax,nst,totGHOSTu1,mGPu1,nGPu1,nlb,nub,mlb,mub,nmlb,nmub,icx)
    !call qxORqy_ghosts(v0,hv,gvv,agvv,thick,porosv,qyk,mmax,nmax,nmaxus,kmax,nst,totGHOSTv1,mGPv1,nGPv1,nlb,nub,mlb,mub,nmlb,nmub,icx) 
   !vmean needed in stage 1 in mom_fls (flood solver) and usrbrl2d both called in uzd. umean not passed to uzd.
   !call UorVmean_ghosts(u0,mmax,nmax,nmaxus,kmax,nst,totGHOSTu1,mGPu1,nGPu1,Umean,thick,nlb,nub,mlb,mub,nmlb,nmub) !
    call UorVmean_ghosts(v0,mmax,nmax,nmaxus,kmax,nst,totGHOSTv1,mGPv1,nGPv1,Vmean,thick,nlb,nub,mlb,mub,nmlb,nmub) ! (I think they are never used in uzd though or only for special structures, double check). Whats the point of a special structure on the ghost point next to the moving bank?          
    call DISSvelGHOST(u0,v0,umean,vmean,nlb,nub,mlb,mub,kmax,gdp)
    if (changeKFUVcut) call kfsuv_ghost(Umean,Vmean,qxk,qyk,hu,hv,dpu,dpv,gsqs,kfs,kfu,kfv,kcs,kcu,kcv,s1,u1,v1,s0,u0,v0,dps,mmax,nmax,kmax,nmaxus,1,0,nlb,nub,mlb,mub,nmlb,nmub, gdp) !set kfs,kfu,kfv active in ghost points     
   ! ONLY FOR CUT CELLS: represcribe periodic velocity components at velocity ghost points.
    if (periodSURFACE) then  
       if (PERIODalongM==1) then 
          call velocityPERIOD_ghost(u0,v0,icx,nlb,nub,mlb,mub,kmax, gdp) ! the second argument has to be the tangential velocity
       else
          call velocityPERIOD_ghost(v0,u0,icx,nlb,nub,mlb,mub,kmax, gdp) ! the second argument has to be the tangential velocity
       endif
       !if (callSUBR_WATERlevelPERIOD)  call WATERlevelPERIOD(s0,dps,icx,nlb,nub,mlb,mub,kmax) !it should not be needed, after sud its already periodic
    endif
    !
end subroutine cutcell_pre_uzd_stage1
