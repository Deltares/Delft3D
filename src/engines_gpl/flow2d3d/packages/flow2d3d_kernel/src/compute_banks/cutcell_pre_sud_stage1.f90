subroutine cutcell_pre_sud_stage1(icx        ,icy        ,u0         ,v0         ,u1         ,&
                                & u0INTv     ,v0INTu     ,v1INTu                             ,&
                                & v1         ,gsqs       ,kcs        ,dpu        ,dpv        ,&
                                & Umean      ,Vmean      ,thick      ,qxk        ,qyk        ,&
                                & hu         ,hv         ,s0         ,s1         ,dps        ,&
                                & guu        ,gvv        ,xcor       ,ycor                   ,&
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
!   Function: Call subroutine that are needed for cutcells before sud computation (stage 1)
!             It computes u0 and v1 at ghost points and set the masking coefficient to 1.
!             u1 is set equal to u0 as well.
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
    integer                       , pointer :: PERIODalongM
    integer                       , pointer :: free_S1_sud
    integer                       , pointer :: extrapGHOST1fluid2
    integer                       , pointer :: totGHOSTu1
    integer                       , pointer :: totGHOSTv1
    integer                       , pointer :: cutcell
    integer                       , pointer :: GhostMethod
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
    real(fp), dimension(:,:)      , pointer :: dpH
    real(fp), dimension(:,:)      , pointer :: POROS
    real(fp), dimension(:,:)      , pointer :: PSIx
    real(fp), dimension(:,:)      , pointer :: PSIy
    real(fp), dimension(:,:)      , pointer :: ETAx
    real(fp), dimension(:,:)      , pointer :: ETAy
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
    real(fp), dimension(:)        , pointer :: aa_dummy
    real(fp), dimension(:)        , pointer :: bb_dummy
    real(fp), dimension(:)        , pointer :: cc_dummy
    real(fp), dimension(:)        , pointer :: dd_dummy
    real(fp), dimension(:,:)      , pointer :: aak_dummy
    real(fp), dimension(:,:)      , pointer :: bbk_dummy
    real(fp), dimension(:,:)      , pointer :: cck_dummy
    real(fp), dimension(:,:)      , pointer :: ddk_dummy
!
! global variables
!
    real(fp), dimension(nmlb:nmub,kmax)                                 , intent(inout)    :: u0INTv
    real(fp), dimension(nmlb:nmub,kmax)                                 , intent(inout)    :: v0INTu
    real(fp), dimension(nmlb:nmub,kmax)                                 , intent(inout)    :: v1INTu
    real(fp), dimension(nmlb:nmub,kmax)                                 , intent(inout)    :: u0
    real(fp), dimension(nmlb:nmub,kmax)                                 , intent(inout)    :: u1
    real(fp), dimension(nmlb:nmub,kmax)                                 , intent(inout)    :: v0
    real(fp), dimension(nmlb:nmub,kmax)                                 , intent(inout)    :: v1
    real(fp), dimension(nmlb:nmub,kmax)                                 , intent(inout)    :: qxk
    real(fp), dimension(nmlb:nmub,kmax)                                 , intent(inout)    :: qyk
    real(fp), dimension(nmlb:nmub)                                      , intent(in)       :: hu
    real(fp), dimension(nmlb:nmub)                                      , intent(in)       :: hv
    real(fp), dimension(nmlb:nmub)                                      , intent(in)       :: dpu
    real(fp), dimension(nmlb:nmub)                                      , intent(in)       :: dpv
    real(fp), dimension(nmlb:nmub)                                      , intent(in)       :: guu
    real(fp), dimension(nmlb:nmub)                                      , intent(in)       :: gvv
    real(fp), dimension(nmlb:nmub)                                      , intent(in)       :: xcor
    real(fp), dimension(nmlb:nmub)                                      , intent(in)       :: ycor
    real(fp), dimension(nmlb:nmub)                                      , intent(in)       :: gsqs
    real(fp), dimension(nmlb:nmub)                                      , intent(inout)    :: Umean
    real(fp), dimension(nmlb:nmub)                                      , intent(inout)    :: Vmean
    real(fp), dimension(nmlb:nmub)                                      , intent(inout)    :: s1
    real(fp), dimension(nmlb:nmub)                                      , intent(inout)    :: s0
    real(prec), dimension(nmlb:nmub)                                    , intent(inout)    :: dps
    real(fp), dimension(kmax)                                           , intent(in)       :: thick   !  Description and declaration in esm_alloc_real.f90
    integer, dimension(nmlb:nmub)                                       , intent(in)       :: kcs
    integer, dimension(nmlb:nmub)                                       , intent(in)       :: kfs
    integer, dimension(nmlb:nmub)                                       , intent(inout)    :: kfu
    integer, dimension(nmlb:nmub)                                       , intent(inout)    :: kfv
    integer, dimension(nmlb:nmub)                                       , intent(in)       :: kcu
    integer, dimension(nmlb:nmub)                                       , intent(in)       :: kcv
    integer                                                             , intent(in)       :: lunscr
    integer                                                             , intent(in)       :: nst     !  Description and declaration in iidim.f90
    integer                                                             , intent(in)       :: nmmax
    integer                                                             , intent(in)       :: nmlb
    integer                                                             , intent(in)       :: nmub
    integer                                                             , intent(in)       :: icx
    integer                                                             , intent(in)       :: icy
    integer                                                             , intent(in)       :: Irov
    integer                                                             , intent(in)       :: mmax
    integer                                                             , intent(in)       :: nmax
    integer                                                             , intent(in)       :: nmaxus
    integer                                                             , intent(in)       :: nlb
    integer                                                             , intent(in)       :: nub
    integer                                                             , intent(in)       :: mlb
    integer                                                             , intent(in)       :: mub
    integer                                                             , intent(in)       :: ddbound
    integer                                                             , intent(in)       :: kmax
!
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
  !real(fp), dimension(nmlb:nmub)                      :: aa_dummy
  !real(fp), dimension(nmlb:nmub)                      :: bb_dummy
  !real(fp), dimension(nmlb:nmub)                      :: cc_dummy
  !real(fp), dimension(nmlb:nmub)                      :: dd_dummy
  !real(fp), dimension(nmlb:nmub, kmax)                :: aak_dummy
  !real(fp), dimension(nmlb:nmub, kmax)                :: bbk_dummy
  !real(fp), dimension(nmlb:nmub, kmax)                :: cck_dummy
  !real(fp), dimension(nmlb:nmub, kmax)                :: ddk_dummy
!
! executable statements -------------------------------------------------------
!
    PERIODalongM              => gdp%gdimbound%PERIODalongM
    free_S1_sud               => gdp%gdimbound%free_S1_sud
    extrapGHOST1fluid2        => gdp%gdimbound%extrapGHOST1fluid2
    totGHOSTu1                => gdp%gdimbound%totGHOSTu1
    totGHOSTv1                => gdp%gdimbound%totGHOSTv1
    cutcell                   => gdp%gdimbound%cutcell
    GhostMethod               => gdp%gdimbound%GhostMethod
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
    dpH                       => gdp%gdimbound%dpH
    POROS                     => gdp%gdimbound%POROS
    PSIx                      => gdp%gdimbound%PSIx
    PSIy                      => gdp%gdimbound%PSIy
    ETAx                      => gdp%gdimbound%ETAx
    ETAy                      => gdp%gdimbound%ETAy
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
    aa_dummy                  => gdp%gdimbound%Dwrka1
    bb_dummy                  => gdp%gdimbound%Dwrka2
    cc_dummy                  => gdp%gdimbound%Dwrka3
    dd_dummy                  => gdp%gdimbound%Dwrka4
    aak_dummy                 => gdp%gdimbound%Dwrkak1
    bbk_dummy                 => gdp%gdimbound%Dwrkak2
    cck_dummy                 => gdp%gdimbound%Dwrkak3
    ddk_dummy                 => gdp%gdimbound%Dwrkak4
    !
    if (cutcell.gt.0.and.GhostMethod.le.1) then
       !
       ! Note: in uzd.f90 v1 is changed and it does not coincide with v0!
       ! I call with v1 since it is used in sud.f90 inside cucnp.f90
       ! Therefore sud uses the updated velocity v in cucnp.f90,
       ! but note that it uses the old discharge qyk to compute d0k
       !
       !do nm=1,nmmax
       !   write(787878,*) nst,nm,v1(nm,1)
       !enddo
       !
       if ((Irov/=0.AND.Irov/=3).or.TYPEfreeSLIP==1) then
          do k=1,kmax
             !CALL interpG_ATu1LOCATION(u1(nmlb,k),kcs,lunscr,Irov,mmax,nmax,nmaxus,kmax,nst,-1,nlb,nub,mlb,mub,nmlb,nmub) !u1=u0 (maybe can be removed since its already extrap above, and below use u1isu0_ghost instead of u0isu1_ghost)
             !
             ! I have to use v1 since it was updated in uzd (and v1 is passed to sud)
             ! this is not needed in my opinion since after uzd ghost point (that are active points) converged too
             ! up to the desired accuracy. Needed only if explicit. Just check if it loses well balancing.
             !
             call interpG_ATv1LOCATION(v1(nmlb,k),kcs,lunscr,Irov,mmax,nmax,nmaxus,kmax,nst,-1,nlb,nub,mlb,mub,nmlb,nmub,0._fp, gdp)
             !CALL interpG_ATv1LOCATION(v0(nmlb,k),kcs,lunscr,Irov,mmax,nmax,nmaxus,kmax,nst,-1,nlb,nub,mlb,mub,nmlb,nmub) ! not efficient, maybe it is better not to reset in kfsuv_ghost the  value of v0 from previous quarter of step.  CALL v0isv1_ghost(v0(nmlb,k),v1(nmlb,k),nlb,nub,mlb,mub,kmax)  !v0 is needed below to compute qyk. v0 is not passed to sud though, only qxk
             !CALL u0isu1_ghost(u0(nmlb,k),u1(nmlb,k),nlb,nub,mlb,mub,kmax)  !u0 is used to compute mean velocity in sud and momentum  in cucnp/mom_cw
             call u1isu0_ghost(u0(nmlb,k),u1(nmlb,k),nlb,nub,mlb,mub,kmax, gdp)
          enddo
       elseif(TYPEfreeSLIP==0) then
          solvedU_ATu = .false.
          solvedV_ATu = .false.
          solvedU_ATv = .false.
          solvedV_ATv = .false.
          if (.not.ITERATEfree.or.freeU0fixed) then
             !CALL INTERPvATuPOINT(u0INTv,u1,ghostu1,inSTENCILv,kcs,agvv,gsqs,icy,icx,kmax,nmmax,nst,nlb,nub,mlb,mub,nmlb,nmub) !inverted icy and icx! u0INTv is the same as u1INTv since u is not updated yet
             !if (interpVinUexact) then
             !CALL interpUinV_FROMu1STENCIL_exactFREE(u0,v0INTu,u0INTv,kcs,lunscr,mmax,nmax,nmaxus,kmax,nst,nlb,nub,mlb,mub,nmlb,nmub,exitloop,iterFR)      !this could be maybe remuved if I keep u0INTv fixed
             !CALL interpVinU_FROMv1STENCIL_exactFREE(u0INTv,v1,v0INTu,kcs,lunscr,mmax,nmax,nmaxus,kmax,nst,nlb,nub,mlb,mub,nmlb,nmub,exitloop,iterFR)
          !else
          !   CALL INTERPvATuPOINT(u0INTv,u1,ghostu1,inSTENCILv,kcs,agvv,gsqs,icy,icx,kmax,nmmax,nst,nlb,nub,mlb,mub,nmlb,nmub) !inverted icy and icx!
          !   CALL INTERPvATuPOINT(v1INTu,v1,ghostv1,inSTENCILu,kcs,aguu,gsqs,icx,icy,kmax,nmmax,nst,nlb,nub,mlb,mub,nmlb,nmub)! v1 was updated in uzd (and v1 is passed to sud). BUT NOTE this is not needed in my opinion since after uzd ghost point (that are active points) converged too up to the desired accuracy. Needed only if explicit.Just check if it loses well balancing.
          !endif
             !CALL interpG_ATu1LOCATION_exactFREE(u1,v1INTu,kcs,lunscr,mmax,nmax,nmaxus,kmax,nst,nlb,nub,mlb,mub,nmlb,nmub,exitloop,iterFR,1._fp)
             !
             ! v1 was updated in uzd (and v1 is passed to sud).
             ! BUT NOTE this is not needed in my opinion since after uzd ghost points (that are active points) converged too
             ! up to the desired accuracy. Needed only if explicit. Just check if it loses well balancing.
             !
             call interpG_ATv1LOCATION_exactFREE(u0INTv,v1,kcs,lunscr,mmax,nmax,nmaxus,kmax,nst,nlb,nub,mlb,mub,nmlb,nmub,exitloop,iterFR,1._fp, gdp)
          else
             iterFR = 0
             exitloop = 0
             !
             ! compute v1INTu to be used in sud (maybe not needed if small cut are soveld for the velocity) SINCE
             ! recomputed ghost in the sud iteration are not reused!However if I decide to compute an updated
             ! u0 at ghost in "free slip equilibrium" with the modified v1 this is all needed.
             !
             do while (exitloop == 0 .and. iterFR < 50)
                exitloop = 1
                iterFR =iterFR+1
                !
                ! the first guess is the one computed by uzd (if implicit) or before uzd (if explicit)
                ! this call could be maybe removed if I keep u0INTv fixed
                !
                call interpUinV_FROMu1STENCIL_exactFREE(u0,v0INTu,u0INTv,kcs,lunscr,mmax,nmax,nmaxus,kmax, &
                                                      & nst,nlb,nub,mlb,mub,nmlb,nmub,exitloop,iterFR, gdp)
                call interpVinU_FROMv1STENCIL_exactFREE(u0INTv,v1,v0INTu,kcs,lunscr,mmax,nmax,nmaxus,kmax, &
                                                      & nst,nlb,nub,mlb,mub,nmlb,nmub,exitloop,iterFR, gdp)
                !
                ! this call could be maybe removed if I keep u0 fixed
                !
                call interpG_ATu1LOCATION_exactFREE(u0,v0INTu,kcs,lunscr,mmax,nmax,nmaxus,kmax, &
                                                      & nst,nlb,nub,mlb,mub,nmlb,nmub,exitloop,iterFR,1._fp, gdp)
                call interpG_ATv1LOCATION_exactFREE(u0INTv,v1,kcs,lunscr,mmax,nmax,nmaxus,kmax, &
                                                      & nst,nlb,nub,mlb,mub,nmlb,nmub,exitloop,iterFR,1._fp, gdp)
                !
                ! u has to be solved only the first time of the iteration
                ! (for the case in which only fluid points are used in the interpolation)
                !
                !solvedU_ATv =.true.
                !if (nmax*mmax<10000) write(9090898,*) nst,iterFR,u0,v1,u0INTv,v0INTu
             enddo
             if (iterFR >= 50) then
                write(*,*) 'Ghost velocity does not converge'
                call d3stop(1, gdp)
             endif
          endif
          !
          ! copy  v0INTu into v1INTu
          !
          call u1INTv_is_u0INTv(v1INTu,v0INTu,inSTENCILu,nmlb,nmub,nmmax,kmax)
          !
       elseif (TYPEfreeSLIP==2) then
          !
          ! here icx is icx
          !
          ! The last argument before GDP was 0._fp.
          ! I use 1 for bank erosion so I have always fresh grid with a value
          !
          call interpG_ATu1LOCATION_hart(icy,icx,v1,ghostv1,totGHOSTv1,Nx,Ny,xG_V1,yG_V1,kfs_cc, &
                                       & lunscr,kmax,nst,nmlb,nmub,ddbound,1._fp, gdp)
          do k=1,kmax
             call u1isu0_ghost(u0(nmlb,k),u1(nmlb,k),nlb,nub,mlb,mub,kmax, gdp)
          enddo
       endif
       !do nm=1,nmmax
       !   write(787879,*) nst,nm,v1(nm,1)
       !enddo
       !
       ! interpolate depth and water surface at s1 ghost point (s1=s0 here)
       !
       if (free_S1_sud.EQ.0) then
          !
          ! s1=s0 here, and s0 is already interpolated before s0 (if I will keep it) so maybe I dont need it, I just copy s0 to s1
          !
          call interpG_ATs1LOCATION(s1,lunscr,Irov,mmax,nmax,nmaxus,kmax,nst,nlb,nub,mlb,mub,nmlb,nmub, gdp)
       endif
       !if (interpS1beforeUZD.AND.free_S1_sud.EQ.1) THEN
       !   CALL s0iss1_ghost(s0,s00,nlb,nub,mlb,mub,kmax,0) !basically s0 = s00 ! in order to conserve mass for small cut cells (where s0 is a ghost point and comparereal(poros(nGP,mGP),0._fp))
       !endif
       !CALL s0iss1_ghost(s0,s1,nlb,nub,mlb,mub,kmax,1) !IGNOREactive=1, so I compute ghost only for the fully emerged ones not for the cut.
       !not needed, should be inserted inside if (interpS1beforeUZD)....
       !CALL interpG_ATs1LOCATION(dps,lunscr,Irov,mmax,nmax,nmaxus,kmax,nst,nlb,nub,mlb,mub,nmlb,nmub) !this can be removed if I dont reset the value
       ! interpolate hu, used in sud. Note: hv is not used in sud at all, but qyk0 (explicit) is used, and I compute it on the ghost cells from hv0 and v0
       !hu is never used at the ghost point in sud since its forced. small cut edges wanna keep the correct hu value
       !CALL interpG_ATu1LOCATION(hu ,kcs,lunscr,Irov,mmax,nmax,nmaxus,kmax,nst,1,nlb,nub,mlb,mub,nmlb,nmub)
       !CALL interpG_ATv1LOCATION(hv0,kcs,lunscr,Irov,mmax,nmax,nmaxus,kmax,nst,1,nlb,nub,mlb,mub,nmlb,nmub) ! if one day I dont use the ghost value to compute flux on cut edges it can be removed
       ! interpolate dpu. dpv not needed, it is not passed to uzd. dpu is passed but since I solve small cut cells is not needed
       !CALL interpG_ATu1LOCATION(dpU,kcs,lunscr,Irov,mmax,nmax,nmaxus,kmax,nst,1,nlb,nub,mlb,mub,nmlb,nmub)
       !CALL interpG_ATv1LOCATION(dpV,kcs,lunscr,Irov,mmax,nmax,nmaxus,kmax,nst,1,nlb,nub,mlb,mub,nmlb,nmub)
       !compute interpolated discharges
       !call qxORqy_ghosts(u1,hu ,guu,aguu,thick,porosu,qxk,mmax,nmax,nmaxus,kmax,nst,totGHOSTu1,mGPu1,nGPu1,nlb,nub,mlb,mub,nmlb,nmub,icx) !u1=u0 here. qxk is used only for weirs in sud, this call could be commented.
       !call qxORqy_ghosts(v0,hv0,gvv,agvv,thick,porosv,qyk,mmax,nmax,nmaxus,kmax,nst,totGHOSTv1,mGPv1,nGPv1,nlb,nub,mlb,mub,nmlb,nmub,icx) !explicit, from hv0 and v0 (note: hv is passed to sud but its not used, so I can safely interpolate only hv0 without changing hv).  ! if one day I dont use the ghost value to compute flux on cut edges it can be removed
       !
       ! No need to compute Vmean at ghost cells (even if v1 is changed in uzd) since Vmean is not passed to SUD
       !
       call UorVmean_ghosts(u1,mmax,nmax,nmaxus,kmax,nst,totGHOSTu1,mGPu1,nGPu1,Umean,thick,nlb,nub,mlb,mub,nmlb,nmub)
       !call UorVmean_ghosts(v1,mmax,nmax,nmaxus,kmax,nst,totGHOSTv1,mGPv1,nGPv1,Vmean,thick,nlb,nub,mlb,mub,nmlb,nmub)
       if (changeKFUVcut) then
           !
           ! set kfs,kfu,kfv active in ghost points
           !
           call kfsuv_ghost(Umean, Vmean, qxk , qyk   , hu  , hv  , dpu,dpv,gsqs, &
                          & kfs  , kfu  , kfv , kcs   , kcu , kcv , &
                          & s1   , u1   , v1  , s0    , u0  , v0  , dps, &
                          & mmax , nmax , kmax, nmaxus, 1   , 0   , &
                          & nlb  , nub  , mlb , mub   , nmlb, nmub, gdp) 
       endif
       call kfuvGHOST3(nst, kcs, kfu  , kfv , umean, vmean, &
                     & hu , hv , u1   , v1  , u0   , v0   , &
                     & qxk, qyk, thick, mmax, nmax , kmax , nmaxus, &
                     & nlb, nub, mlb  , mub , nmlb , nmub , Irov  , gdp)
       if (extrapGHOST1fluid2.ne.0.and.extrapGHOST1fluid2.ne.3) then
           call extrapV(nst      , kcs      , kfv      , v0       , v1  , Vmean , qyk , hv, &
                      & thick    , gvv      , mmax     , nmax     , kmax, nmaxus, &
                      & nlb      , nub      , mlb      , mub      , nmlb, nmub  , Irov, 1 , &
                      & aa_dummy , bb_dummy , cc_dummy , dd_dummy , &
                      & aak_dummy, bbk_dummy, cck_dummy, ddk_dummy, gdp)
       endif
    elseif (cutcell.gt.0.and.GhostMethod.ge.2) then
       !
       ! it is needed otherwise it has the value -dpH
       !
       ! interpolate depth and water surface at s1 ghost point (s1=s0 here)
       !
       !CALL interpG_ATs1LOCATION(s1,lunscr,Irov,mmax,nmax,nmaxus,kmax,nst,nlb,nub,mlb,mub,nmlb,nmub) !s1=s0 here
       !if (interpS1beforeUZD.AND.free_S1_sud.EQ.1) THEN
       !   CALL s0iss1_ghost(s0,s00,nlb,nub,mlb,mub,kmax,0) !basically s0 = s00 ! in order to conserve mass for small cut cells (where s0 is a ghost point and comparereal(poros(nGP,mGP),0._fp))
       !endif
       !CALL s0iss1_ghost(s0,s1,nlb,nub,mlb,mub,kmax,1) !IGNOREactive=1, so I compute ghost only for the fully emerged ones not for the cut.
       call kfuvGHOST3(nst , kcs , kfu , kfv   , umean, vmean, hu   , hv  , &
                     & u1  , v1  , u0  , v0    , qxk  , qyk  , thick, &
                     & mmax, nmax, kmax, nmaxus, &
                     & nlb , nub , mlb , mub   , nmlb, nmub  , irov , gdp )
       call extrapUV(nst  , guu , gvv , kcs , kfu   , kfv, umean, vmean, &
                   & hu   , hv  , u1  , v1  , u0    , v0 , qxk  , qyk  , &
                   & thick, mmax, nmax, kmax, nmaxus, &
                   & nlb  , nub , mlb , mub , nmlb  , nmub, irov, gdp  )
    endif
    call DISSvelGHOST(u1, v1, umean, vmean, nlb, nub, mlb, mub, kmax, gdp)
    if (cutcell.gt.0) then
       !
       ! Re-prescribe periodic velocity components at velocity ghost points
       !
       if (periodSURFACE) then
          if (PERIODalongM==1) then
             !
             ! The second argument has to be the tangential velocity
             !
             call velocityPERIOD_ghost(u1,v1,icx,nlb,nub,mlb,mub,kmax, gdp)
          else
             !
             ! The second argument has to be the tangential velocity
             !
             call velocityPERIOD_ghost(v1,u1,icx,nlb,nub,mlb,mub,kmax, gdp)
          endif
          if (callSUBR_WATERlevelPERIOD) then
             !
             ! This should not be needed, after previous sud or after bed update its already periodic
             !
             call WATERlevelPERIOD(s0,dps,icx,nlb,nub,mlb,mub,kmax, gdp)
          endif
       endif
    endif
    !
end subroutine cutcell_pre_sud_stage1
