subroutine mom_cyclic &
               &(icx       ,icy       ,nmmax     ,kmax      ,kcu       ,kcs       , &
               & kfu       ,kfv       ,kspu      ,kadu      ,kadv      ,            &
               & dps       ,s0        ,u0        ,v         ,qxk       ,qyk       , &
               & hu        ,guu       ,gvv       ,gvd       ,gvu       ,gsqiu     , &
               & umean     ,bbk       ,ddk       ,bddx      ,bddy      ,bdx       , &
               & bdy       ,bux       ,buy       ,buux      ,buuy      ,mom_output, &
               & u1        ,kWDu      ,kWDv      ,ghostU1   ,ghostV1   ,aguu      , &
               & xcor      ,ycor      ,nst       ,gdp)
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
!  $Id: mom_cyclic.f90 6388 2016-08-01 01:05:36Z canestrelli.x $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20160126_PLIC_VOF_bankEROSION/src/engines_gpl/flow2d3d/packages/kernel/src/compute/mom_cyclic.f90 $
!!--description-----------------------------------------------------------------
!
! This subroutine is part of (called by) UZD. It computes the Horizontal
! Advection in U- and V-direction following the default cyclic scheme.
! In both U- and V-direction an implicit 2-nd order upwind.
! (Ref.: Stelling & Leendertse
!        "Approximation of Convective Processes by Cyclic
!         AOI methods", Proc. 2nd ASCE Conf. on Estuarine
!         and Coastal Modelling, Tampa, 1991)
!
! Along open boundaries the advection terms normal to the open boundaries can
! be switched off (option: CSTBND = TRUE)
!
! In case of hydraulic structure a special energy conserving discretisation is
! implemented.
! It computes the contribution of the advection terms to the matrix elements
! and the right hand side of the system of discretised momentum equations.
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
   !
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    logical      , pointer :: cstbnd
    logical      , pointer :: wind
    logical      , pointer :: struct
    real(fp), dimension(:,:)          , pointer :: mom_m_convec        ! convection u*du/dx term
    real(fp), dimension(:,:)          , pointer :: mom_m_xadvec        ! cross-advection v*du/dy term
    logical                       , pointer :: EXCLouterVEL
    integer                       , pointer :: cutcell
    integer                       , pointer :: MODadvecGHOSTuzd
    logical                       , pointer :: getADJACENTgrad
    integer, dimension(:,:)       , pointer :: kfs_cc
    real(fp), dimension(:,:)      , pointer :: PSIx
    real(fp), dimension(:,:)      , pointer :: PSIy
    real(fp), dimension(:,:)      , pointer :: xG_V1
    real(fp), dimension(:,:)      , pointer :: yG_V1
    real(fp), dimension(:,:,:,:,:), pointer :: EDGExyBANK
    real(fp), dimension(:,:)      , pointer :: xG_U1
    real(fp), dimension(:,:)      , pointer :: yG_U1
    logical                       , pointer :: cntrUZDbnd_n
    logical                       , pointer :: cntrUZDbnd_m
    logical                       , pointer :: vvvSECord
!
! Global variables
!
    integer                                                           :: icx
    integer                                                           :: icy
    integer                                                           :: kmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                           :: nmmax  !  Description and declaration in dimens.igs
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)                      :: GHOSTu1
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)                      :: GHOSTv1
    integer,    dimension(gdp%d%nmlb:gdp%d%nmub)         , intent(in) :: kcs    !  Description and declaration in esm_alloc_int.f90
    integer,    dimension(gdp%d%nmlb:gdp%d%nmub)         , intent(in) :: kcu    !  Description and declaration in esm_alloc_int.f90
    integer,    dimension(gdp%d%nmlb:gdp%d%nmub)         , intent(in) :: kfu    !  Description and declaration in esm_alloc_int.f90
    integer,    dimension(gdp%d%nmlb:gdp%d%nmub)         , intent(in) :: kfv    !  Description and declaration in esm_alloc_int.f90
    integer,    dimension(gdp%d%nmlb:gdp%d%nmub, kmax)   , intent(in) :: kadu   !  Description and declaration in esm_alloc_int.f90
    integer,    dimension(gdp%d%nmlb:gdp%d%nmub, kmax)   , intent(in) :: kadv   !  Description and declaration in esm_alloc_int.f90
    integer,    dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax) , intent(in) :: kspu   !  Description and declaration in esm_alloc_int.f90
    real(fp),   dimension(gdp%d%nmlb:gdp%d%nmub)         , intent(in) :: s0     !  Description and declaration in esm_alloc_real.f90
    real(fp),   dimension(gdp%d%nmlb:gdp%d%nmub)         , intent(in) :: hu     !  Description and declaration in esm_alloc_real.f90
    real(prec), dimension(gdp%d%nmlb:gdp%d%nmub)         , intent(in) :: dps    !  Description and declaration in esm_alloc_real.f90
    real(fp),   dimension(gdp%d%nmlb:gdp%d%nmub)         , intent(in) :: guu    !  Description and declaration in esm_alloc_real.f90
    real(fp),   dimension(gdp%d%nmlb:gdp%d%nmub)         , intent(in) :: gvv    !  Description and declaration in esm_alloc_real.f90
    real(fp),   dimension(gdp%d%nmlb:gdp%d%nmub)         , intent(in) :: gvd    !  Description and declaration in esm_alloc_real.f90
    real(fp),   dimension(gdp%d%nmlb:gdp%d%nmub)         , intent(in) :: gvu    !  Description and declaration in esm_alloc_real.f90
    real(fp),   dimension(gdp%d%nmlb:gdp%d%nmub)         , intent(in) :: xcor    !  Description and declaration in esm_alloc_real.f90
    real(fp),   dimension(gdp%d%nmlb:gdp%d%nmub)         , intent(in) :: ycor    !  Description and declaration in esm_alloc_real.f90    
    integer,    dimension(gdp%d%nmlb:gdp%d%nmub,4)       , intent(in) :: kWDu
    integer,    dimension(gdp%d%nmlb:gdp%d%nmub,4)       , intent(in) :: kWDv
    real(fp),   dimension(gdp%d%nmlb:gdp%d%nmub)         , intent(in) :: gsqiu  !  Description and declaration in esm_alloc_real.f90
    real(fp),   dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: bddx
    real(fp),   dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: bddy
    real(fp),   dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: bdx
    real(fp),   dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: bdy
    real(fp),   dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: buux
    real(fp),   dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: buuy
    real(fp),   dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: bux
    real(fp),   dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: buy
    real(fp),   dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: bbk
    real(fp),   dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: ddk
    real(fp),   dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: qxk    !  Description and declaration in esm_alloc_real.f90
    real(fp),   dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: qyk    !  Description and declaration in esm_alloc_real.f90
    real(fp),   dimension(gdp%d%nmlb:gdp%d%nmub, kmax)   , intent(in) :: u0     !  Description and declaration in esm_alloc_real.f90
    real(fp),   dimension(gdp%d%nmlb:gdp%d%nmub, kmax)   , intent(in) :: u1     !  Description and declaration in esm_alloc_real.f90
                                                                                !  Only used in case mom_output = .true.
    real(fp),   dimension(gdp%d%nmlb:gdp%d%nmub, kmax)   , intent(in) :: v
    real(fp),   dimension(gdp%d%nmlb:gdp%d%nmub)                      :: umean  !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                      :: aguu      !  Description and declaration in esm_alloc_real.f90
    logical                                              , intent(in) :: mom_output
    integer                                              , intent(in) :: nst    
!
! Local variables
!
    integer :: iad3
    integer :: iad1
    integer :: iad2
    integer :: iad4
    integer :: k
    integer :: kenm
    integer :: kspu0k
    integer :: nddm
    integer :: nddmu
    integer :: ndm
    integer :: ndmd
    integer :: ndmu
    integer :: neigat  ! =0 for neighbour point is gate
    integer :: nm
    integer :: nmd
    integer :: nmdd
    integer :: nmu
    integer :: nmuu
    integer :: num
    integer :: numu
    integer :: nuum
    integer :: numdd
    integer :: numd
    integer :: ndmdd
    integer :: numuu
    integer :: ndmuu
    integer :: kfv_ndm  
    integer :: kfv_ndmu  
    integer :: kfv_nm   
    integer :: kfv_nmu
    integer :: nmLmax
    real(fp):: Lmax 
    real(fp):: adfac
    real(fp):: gsqi
    real(fp):: termc
    real(fp):: termd
    real(fp):: termdd
    real(fp):: termex
    real(fp):: termu
    real(fp):: termuu
    real(fp):: uu
    real(fp):: vvv
    real(fp):: vvvANA
    real(fp):: vvhr
    real(fp):: uvdgdy
    real(fp):: vvdgdx
    real(fp):: advEXP 
    logical :: BOUNDpoint
!
!! executable statements -------------------------------------------------------
!
    EXCLouterVEL     => gdp%gdimbound%EXCLouterVEL
    cutcell          => gdp%gdimbound%cutcell
    MODadvecGHOSTuzd => gdp%gdimbound%MODadvecGHOSTuzd
    getADJACENTgrad  => gdp%gdimbound%getADJACENTgrad
    kfs_cc           => gdp%gdimbound%kfs_cc
    PSIx             => gdp%gdimbound%PSIx
    PSIy             => gdp%gdimbound%PSIy
    xG_V1            => gdp%gdimbound%xG_V1
    yG_V1            => gdp%gdimbound%yG_V1
    EDGExyBANK       => gdp%gdimbound%EDGExyBANK
    xG_U1            => gdp%gdimbound%xG_U1
    yG_U1            => gdp%gdimbound%yG_U1
    cntrUZDbnd_n     => gdp%gdimbound%cntrUZDbnd_n
    cntrUZDbnd_m     => gdp%gdimbound%cntrUZDbnd_m
    vvvSECord        => gdp%gdimbound%vvvSECord
    wind       => gdp%gdprocs%wind
    struct     => gdp%gdprocs%struct
    cstbnd     => gdp%gdnumeco%cstbnd
    !
    if (mom_output) then
       if (icx==1) then ! solve V/N component
          mom_m_convec => gdp%gdflwpar%mom_n_convec
          mom_m_xadvec => gdp%gdflwpar%mom_n_xadvec
       else ! solve U/M component
          mom_m_convec => gdp%gdflwpar%mom_m_convec
          mom_m_xadvec => gdp%gdflwpar%mom_m_xadvec
       endif
    endif
    !
    !  INITIALIZE
    !
    do k = 1, kmax
       nmd   = -icx
       nmdd  = -icx - icx
       ndm   = -icy
       nddm  = -icy - icy
       nddmu = -icy - icy + icx
       ndmd  = -icy - icx
       nmu   = icx
       num   = icy
       nuum  = icy + icy
       numu  = icx + icy
       nmuu  = icx + icx
       ndmu  = -icy + icx
       !added for transmissive gradient (IBM)
       numd  = +icy - icx 
       numdd = +icy - icx - icx 
       numuu = +icy + icx + icx
       ndmdd = -icy - icx - icx
       ndmuu = -icy + icx + icx
! 
       Lmax=-1
       do nm = 1, nmmax
          nmd   = nmd + 1
          nmdd  = nmdd + 1
          ndm   = ndm + 1
          nddm  = nddm + 1
          nddmu = nddmu + 1
          ndmd  = ndmd + 1
          nmu   = nmu + 1
          num   = num + 1
          nuum  = nuum + 1
          numu  = numu + 1
          nmuu  = nmuu + 1
          ndmu  = ndmu + 1
          !added for transmissive gradient (IBM)
          numd  = numd + 1
          numdd = numdd + 1
          numuu = numuu + 1 
          ndmdd = ndmdd + 1 
          ndmuu = ndmuu + 1

          kspu0k= kspu(nm, 0)*kspu(nm, k)
          !
          ! For an active point and not a gate or plate
          !
          if ( ((kcu(nm)==1) .and. (kfu(nm)==1)) .and. kspu0k /=4 .and. kspu0k /=10) then
             gsqi   = gsqiu(nm)
             kfv_ndm  = kfv(ndm) * kWDv(ndm,2) !* NINT(1._fp - real(min(ghostV1(ndm),1),fp))
             kfv_ndmu = kfv(ndmu)* kWDv(ndmu,2)!* NINT(1._fp - real(min(ghostV1(ndmu),1),fp))
             kfv_nm   = kfv(nm)  * kWDv(nm,1)  !* NINT(1._fp - real(min(ghostV1(nm),1),fp))
             kfv_nmu  = kfv(nmu) * kWDv(nmu,1) !* NINT(1._fp - real(min(ghostV1(nmu),1),fp))
             kenm = max(kfv_ndm+kfv_ndmu+kfv_nm+kfv_nmu, 1)             
             if (cutcell>0.and.vvvSECord.and.kenm>1.and.kenm<4) then !ghostu1(nm)/=1.and.(kcs(nm)==1.or.kcs(nmu)==1)
                call vvvORD2sub(vvv,v,guu,gvv,kfv,nm,k,nst,icx,icy,gdp%d%nmlb,gdp%d%nmub,kmax,kenm)
             else !standard delft3D             
             if (       (cstbnd .and. (kcs(nm)==2 .or. kcs(nmu)==2)) &
                    & .or. (kcs(nm)==3 .or. kcs(nmu)==3               ) .or. EXCLouterVEL) then
                   vvv = (v(ndm, k)*kfv_ndm + v(ndmu, k)*kfv_ndmu + v(nm, k)     &
                       & *kfv_nm + v(nmu, k)*kfv_nmu)/kenm
             else
                vvv = .25*(v(nm, k) + v(nmu, k) + v(ndm, k) + v(ndmu, k))
             endif
             endif          
             !
             ! CURVATURE TERM DUE TO CONVECTION IN U-DIRECTION
             !
             uvdgdy = vvv*gsqi*0.5*((gvv(nm) + gvv(nmu) - gvv(ndm) - gvv(ndmu)))
             !
             ! CURVATURE TERM DUE TO ADVECTION IN V-DIRECTION
             !
             vvdgdx = 0.5*vvv*gsqi*(guu(nmu) - guu(nmd))
             !
             adfac  = 0.50/gvu(nm)
             vvhr   = 0.5*vvv/guu(nm)
             !
             ! CONTRIBUTION OF CONVECTION IN X DIRECTION
             !
             ! begin standard delft3d-flow (compare to mom_waqua)
             !
             if (u0(nm, k)>0.0) then
                kspu0k = kspu(nmd, 0)*kspu(nmd, k)
                if (kspu0k==4 .or. kspu0k==10) then
                   neigat = 0
                else
                   neigat = 1
                endif
                if (kadu(nm, k)==0) then
                   !
                   ! Energy conservative discretisation for structure points
                   !
                   uu = adfac*(u0(nm, k) + u0(nmd, k))
                   termc = (uu + uvdgdy)*kfu(nmd)*kWDu(nmd,2)*neigat
                   termd = -uu*kfu(nmd)*kWDu(nmd,2)*neigat
                   if (cutcell.eq.2.and.MODadvecGHOSTuzd==2) THEN
                      if (GHOSTu1(nm).eq.1.or.GHOSTu1(nmd).EQ.1) then  
                         termc = 0._fp
                         termd = 0._fp
                      endif
                   endif
                   if (mom_output) then
                      if (ghostu1(nm)/=1) then
                      mom_m_convec(nm, k) = mom_m_convec(nm, k) &
                                          & - termc*u1(nm, k) - termd*u1(nmd, k)
                      endif
                   else
                      bbk(nm, k) = bbk(nm, k) + termc
                      bdx(nm, k) = termd
                   endif
                else
                   !
                   ! Upwind approach near structure points and inactive u-points
                   !
                   !
                   ! CONSERVATIVE FORM ( LESS STABLE! )
                   !             BBK (NM,K)=BBK(NM,K)
                   !    *                  +U0(NM  ,K)*ADFAC*( IAD1+IAD2)+UVDGDY*KFU(NMD)
                   !             BDX (NM,K)=U0(NMD ,K)*ADFAC*(-IAD1-IAD2-IAD2)
                   !             BDDX(NM,K)=U0(NMDD,K)*ADFAC*(           IAD2)
                   !
                   ! NON CONSERVATIVE FORM
                   !
                   if (cntrUZDbnd_m) then     
                      ! new version: i dont send gradient to zero if kfu(nmdd) = 0 as in the original version, but I make it centered
                      iad1 = kfu(nmd)*kWDu(nmd,2)*kadu(nmd, k)
                      iad2 = iad1*kfu(nmdd)*kWDu(nmdd,2)                      
                      if (iad2==0) then !centred
                         iad4 = iad1*kfu(nmu)*kWDu(nmu,1)                           
                         termu =  u0(nm, k)*adfac* iad4
                         termd = -u0(nm, k)*adfac*(2*iad1 - iad4)
                         termc =  u0(nm, k)*adfac*(2*iad1 - 2*iad4)
                         termdd = 0._fp                         
                      else !upwind
                         termu  = 0._fp
                         termc  = u0(nm, k)*adfac*(2*iad1 + iad2) + uvdgdy*neigat
                         termd  = u0(nm, k)*adfac*( - 2*iad1 - 2*iad2)
                         termdd = u0(nm, k)*adfac*(iad2)                            
                      endif
                   else
                      iad1 = (2*kfu(nmd)*kWDu(nmd,2))*kadu(nmd, k)
                      iad2 = kfu(nmd)*kWDu(nmd,2)*kfu(nmdd)*kWDu(nmdd,2)*kfu(nmu)*kWDu(nmu,1)*kadu(nmd, k)*kadu(nmdd, k)&
                           & *kadu(nmu, k)                       
                      termu  = 0._fp
                      termc  = u0(nm, k)*adfac*(iad1 + iad2) + uvdgdy*neigat
                   termd  = u0(nm, k)*adfac*( - iad1 - iad2 - iad2)
                   termdd = u0(nm, k)*adfac*(iad2)
                   endif
                   if (cutcell.eq.2.and.MODadvecGHOSTuzd==2) THEN
                      if (getADJACENTgrad) then
                         if (comparereal(aguu(nm),0._fp).gt.0.and.comparereal(aguu(nm),1._fp).lt.0) then
                            if (kfu(num)==1) then !use gradient above (explicit)
                               iad1 = (2*kfu(numd)*kWDu(numd,2))*kadu(numd, k)
                               iad2 = kfu(numd)*kWDu(numd,2)*kfu(numdd)*kWDu(numdd,2)*kfu(numu)*kWDu(numu,1)*kadu(numd, k)*kadu(numdd, k)&
                                    & *kadu(numu, k)
                               termc  = u0(nm, k)*adfac*(iad1 + iad2) !+ uvdgdy*neigat !no curv for cutcell
                               termd  = u0(nm, k)*adfac*( - iad1 - iad2 - iad2)
                               termdd = u0(nm, k)*adfac*(iad2)
                               advEXP =  - termc*u0(num, k) - termd*u0(numd, k) - termdd*u0(numdd, k)
                               ddk(nm,k) = ddk(nm,k) + advEXP
                               termc  = 0._fp
                               termd  = 0._fp
                               termdd = 0._fp
                               if (mom_output) then
                                  mom_m_convec(nm, k) = mom_m_convec(nm, k) + advEXP
                               endif
                            elseif (kfu(ndm)==1) then
                               iad1 = (2*kfu(ndmd)*kWDu(ndmd,2))*kadu(ndmd, k)
                               iad2 = kfu(ndmd)*kWDu(ndmd,2)*kfu(ndmdd)*kWDu(ndmdd,2)*kfu(ndmu)*kWDu(ndmu,1)*kadu(ndmd, k)*kadu(ndmdd, k)&
                                    & *kadu(ndmu, k)
                               termc  = u0(nm, k)*adfac*(iad1 + iad2) !+ uvdgdy*neigat !no curv for cutcell
                               termd  = u0(nm, k)*adfac*( - iad1 - iad2 - iad2)
                               termdd = u0(nm, k)*adfac*(iad2)
                               advEXP =  - termc*u0(ndm, k) - termd*u0(ndmd, k) - termdd*u0(ndmdd, k)
                               ddk(nm,k) = ddk(nm,k) + advEXP
                               termc  = 0._fp
                               termd  = 0._fp
                               termdd = 0._fp
                               if (mom_output) then
                                  mom_m_convec(nm, k) = mom_m_convec(nm, k) + advEXP
                               endif
                            else
                               iad1 = 0
                               iad2 = 0
                            endif
                         endif
                      else
                         if (GHOSTu1(nm).eq.1.or.GHOSTu1(nmd).EQ.1) then !.or.GHOSTu1(nmdd).EQ.1) then  
                            termc = 0._fp
                            termd = 0._fp
                            termdd= 0._fp
                         endif
                      endif
                   endif
                   if (mom_output) then
                      if (ghostu1(nm)/=1) then
                         mom_m_convec(nm, k) = mom_m_convec(nm, k) &
                                             & - termu*u1(nmu, k) - termc*u1(nm, k) - termd*u1(nmd, k) - termdd*u1(nmdd, k)
                      endif
                   else
                      bux(nm, k)  = termu
                      bbk(nm, k)  = bbk(nm, k) + termc
                      bdx(nm, k)  = termd
                      bddx(nm, k) = termdd
                   endif
                endif
             else
                kspu0k = kspu(nmu, 0)*kspu(nmu, k)
                if (kspu0k==4 .or. kspu0k==10) then
                   neigat = 0
                else
                   neigat = 1
                endif
                if (kadu(nm, k)==0) then
                   !
                   ! Energy conservative discretisation for structure points
                   !
                   uu = adfac*(u0(nm, k) + u0(nmu, k))
                   termc = (uvdgdy - uu)*kfu(nmu)*kWDu(nmu,1)*neigat
                   termu = uu*kfu(nmu)*kWDu(nmu,1)*neigat
                   if (cutcell.eq.2.and.MODadvecGHOSTuzd==2) THEN
                      if (GHOSTu1(nm).eq.1.or.GHOSTu1(nmu).EQ.1) then  
                         termc = 0._fp
                         termu = 0._fp
                      endif
                   endif
                   if (mom_output) then
                      if (ghostu1(nm)/=1) then
                      mom_m_convec(nm, k) = mom_m_convec(nm, k) &
                                          & - termc*u1(nm, k) - termu*u1(nmu, k)
                      endif
                   else
                      bbk(nm, k) = bbk(nm, k) + termc
                      bux(nm, k) = termu
                   endif
                else
                   !
                   ! Upwind approach near structure points and inactive u-points
                   !
                   !
                   ! CONSERVATIVE FORM ( LESS STABLE! )
                   !
                   !             BBK (NM,K)=BBK(NM,K)
                   !    *                  +U0(NM  ,K)*ADFAC*(-IAD1-IAD2)+UVDGDY*KFU(NMU)
                   !             BUX (NM,K)=U0(NMU ,K)*ADFAC*( IAD1+IAD2+IAD2)
                   !             BUUX(NM,K)=U0(NMUU,K)*ADFAC*(          -IAD2)
                   !
                   !
                   ! NON CONSERVATIVE FORM
                   !
                   if (cntrUZDbnd_m) then      
                      ! new version: i dont send gradient to zero if kfu(nmuu) = 0 as in the original version, but I make it centered
                      iad1 = kfu(nmu)*kWDu(nmu,1)*kadu(nmu, k)
                      iad2 = iad1*kfu(nmuu)*kWDu(nmuu,1)                      
                      if (iad2==0) then !centred
                         iad4 = iad1*kfu(nmd)*kWDu(nmd,1)                           
                         termu =  u0(nm, k)*adfac* (2*iad1 - iad4)
                         termd = -u0(nm, k)*adfac* iad4
                         termc =  u0(nm, k)*adfac* (2*iad1 - 2*iad4)
                         termuu = 0._fp
                      else !upwind     
                         termd  = 0._fp
                         termc  = u0(nm, k)*adfac*(-2*iad1 -   iad2) + uvdgdy*neigat
                         termu  = u0(nm, k)*adfac*( 2*iad1 + 2*iad2)
                         termuu = u0(nm, k)*adfac*( - iad2)                            
                      endif
                   else       
                       iad1 = (2*kfu(nmu)*kWDu(nmu,1))*kadu(nmu, k)
                       iad2 = kfu(nmu)*kWDu(nmu,1)*kfu(nmuu)*kWDu(nmuu,1)*kfu(nmd)*kWDu(nmd,2)*kadu(nmu, k)*kadu(nmuu, k)&
                            & *kadu(nmd, k)    
                       termd  = 0._fp
                       termc  = u0(nm, k)*adfac*( - iad1 - iad2) + uvdgdy*neigat
                   termu  = u0(nm, k)*adfac*(iad1 + iad2 + iad2)
                   termuu = u0(nm, k)*adfac*( - iad2)
                   endif
                   if (cutcell.eq.2.and.MODadvecGHOSTuzd==2) THEN
                     if (getADJACENTgrad) then
                         if (comparereal(aguu(nm),0._fp).gt.0.and.comparereal(aguu(nm),1._fp).lt.0) then
                            if(kfu(num)==1) then !use gradient above (explicit)
                               iad1 = (2*kfu(numu)*kWDu(numu,1))*kadu(numu, k)
                               iad2 = kfu(numu)*kWDu(numu,1)*kfu(numuu)*kWDu(numuu,1)*kfu(numd)*kWDu(numd,2)*kadu(numu, k)*kadu(numuu, k)&
                                    & *kadu(numd, k)
                               termc  = u0(nm, k)*adfac*( - iad1 - iad2) !+ uvdgdy*neigat
                               termu  = u0(nm, k)*adfac*(iad1 + iad2 + iad2)
                               termuu = u0(nm, k)*adfac*( - iad2)
                               advEXP = - termc*u0(num, k) - termu*u0(numu, k) - termuu*u0(numuu, k)
                               ddk(nm,k) = ddk(nm,k) + advEXP
                               termc  = 0._fp
                               termu  = 0._fp
                               termuu = 0._fp
                               if (mom_output) then
                                  mom_m_convec(nm, k) = mom_m_convec(nm, k) + advEXP
                               endif
                            elseif (kfu(ndm)==1) then
                               iad1 = (2*kfu(ndmu)*kWDu(ndmu,1))*kadu(ndmu, k)
                               iad2 = kfu(ndmu)*kWDu(ndmu,1)*kfu(ndmuu)*kWDu(ndmuu,1)*kfu(ndmd)*kWDu(ndmd,2)*kadu(ndmu, k)*kadu(ndmuu, k)&
                                    & *kadu(ndmd, k)
                               termc  = u0(nm, k)*adfac*( - iad1 - iad2) !+ uvdgdy*neigat
                               termu  = u0(nm, k)*adfac*(iad1 + iad2 + iad2)
                               termuu = u0(nm, k)*adfac*( - iad2)
                               advEXP =  - termc*u0(ndm, k) - termu*u0(ndmu, k) - termuu*u0(ndmuu, k)
                               ddk(nm,k) = ddk(nm,k) + advEXP
                               termc  = 0._fp
                               termu  = 0._fp
                               termuu = 0._fp
                               if (mom_output) then
                                  mom_m_convec(nm, k) = mom_m_convec(nm, k) + advEXP
                               endif
                            else
                               iad1 = 0
                               iad2 = 0
                            endif
                         endif
                      else
                         if (GHOSTu1(nm).eq.1.or.GHOSTu1(nmu).EQ.1) then !.or.GHOSTu1(nmuu).EQ.1) then  
                            termc = 0._fp
                            termu = 0._fp
                            termuu= 0._fp
                         endif
                      endif
                   endif
                   if (mom_output) then
                      if (ghostu1(nm)/=1) then
                          mom_m_convec(nm, k) = mom_m_convec(nm, k) &
                                              & - termd*u1(nmd, k) - termc*u1(nm, k) - termu*u1(nmu, k) - termuu*u1(nmuu, k)
                      endif
                   else
                      bdx(nm, k)  = termd
                      bbk(nm, k)  = bbk(nm, k) + termc
                      bux(nm, k)  = termu
                      buux(nm, k) = termuu
                   endif
                endif
             endif
             !
             ! end standard delft3d-flow (compare to mom_waqua)
             !
             !
             ! CONTRIBUTION OF ADVECTION IN Y DIRECTION
             !           IAD1      =KFV(NDM) *KFV(NDMU)*KFU(NDM) for VVHR > 0
             !           IAD1      =KFV(NM) *KFV(NMU)*KFU(NUM) for VVHR < 0
             !
             if (kadu(num, k)*kadu(ndm, k)*kadu(nm, k)==1) then
                if (vvhr>0.0) then
                   if (MODadvecGHOSTuzd==2) then !it works also with cutcell==2
                      iad1 = kfu(ndm)*kWDu(ndm,4) !kfv(ndm)*kWDv(ndm,2)*kfv(ndmu)*kWDv(ndmu,2)
                      iad2 = iad1*kfu(nddm)*kWDu(nddm,4)
                      iad3 = iad1
                   else
                   if (cstbnd) then
                      if (kcs(nm)==2) then
                            iad1 = kfu(ndm)*kWDu(ndm,4)*kfv(ndmu)*kWDv(ndmu,2)
                            iad2 = iad1*kfv(nddmu)*kWDv(nddmu,2)*kfu(nddm)*kWDu(nddm,4)
                            iad3 = iad1
                           ! vvdgdx = 0.0
                      elseif (kcs(nmu)==2) then
                            iad1 = kfu(ndm)*kWDu(ndm,4)*kfv(ndm)*kWDv(ndm,2)
                            iad2 = iad1*kfv(nddm)*kWDv(nddm,2)*kfu(nddm)*kWDu(nddm,4)
                            iad3 = iad1
                           ! vvdgdx = 0.0
                         else
                            !iad1 = kfu(ndm)*kfv(ndm)*kfv(ndmu)
                            iad3 = kfv(ndm)*kWDv(ndm,2)*kfv(ndmu)*kWDv(ndmu,2)
                            iad1 = kfu(ndm)*kWDu(ndm,4)*iad3
                            iad2 = iad1*kfv(nddm)*kWDv(nddm,2)*kfv(nddmu)*kWDv(nddmu,2)*kfu(nddm)*kWDu(nddm,4)
                         endif
                      else
                         iad1 = kfu(ndm)*kWDu(ndm,4) !v(ndm)*kWDv(ndm,2)*kfv(ndmu)*kWDv(ndmu,2) !removed all kfv, they are taken care of above (same in mom_cyclic called from sud)
                         iad2 = iad1*kfu(nddm)*kWDu(nddm,4) !iad1*kfv(nddm)*kWDv(nddm,2)*kfv(nddmu)*kWDv(nddmu,2)*kfu(nddm)*kWDu(nddm,4)
                         iad3 = iad1
                      endif
                   endif
                   if (iad2==0.and.cntrUZDbnd_n) then
                      iad4 = iad1*kfu(num)*kWDu(num,4) !kWDu(num,4) to be changed
                      termu =  vvhr * iad4
                      termd = -vvhr *(2*iad1 - iad4)
                      termc =  vvhr*(2*iad1 - 2*iad4)
                   else
                      termu = 0._fp
                   termc  = vvhr*(iad1 + iad1 + iad2)
                   termd  = vvhr*( - iad1 - iad1 - iad2 - iad2)
                   termdd = vvhr*(iad2)
                      termex = vvv*vvdgdx*iad3                      
                   endif
                   if (cutcell.eq.2.and.MODadvecGHOSTuzd==2) THEN
                      if (GHOSTu1(nm).eq.1 .or.GHOSTu1(ndm).EQ.1) then !.or.GHOSTu1(nddm).EQ.1) then  
                         termc = 0._fp
                         termd = 0._fp
                         termdd= 0._fp
                      endif
                   endif
                   if (mom_output) then
                      if (ghostu1(nm)/=1) then
                      mom_m_xadvec(nm, k) = mom_m_xadvec(nm, k) &
                                             & - termu*u1(nmu, k) - termc*u1(nm, k) - termd*u1(ndm, k) - termdd*u1(nddm, k) + termex
                      endif
                   else 
                      buy(nm, k)  = termu
                      bbk(nm, k)  = bbk(nm, k) + termc
                      bdy(nm, k)  = termd
                      bddy(nm, k) = termdd
                      ddk(nm, k)  = ddk(nm, k) + termex
                   endif
                else
                   if (MODadvecGHOSTuzd==2) then !it works also with cutcell==2
                     ! if (getADJACENTgrad) then
                     ! else
                      iad1 = kfu(num)*kWDu(num,3) 
                      iad2 = iad1*kfu(nuum)*kWDu(nuum,3)
                      iad3 = iad1
                     ! endif
                   else
                      if (cstbnd) then
                         if (kcs(nm)==2) then
                            iad1 = kfu(num)*kWDu(num,3)*kfv(nmu)*kWDv(nmu,1)
                            iad2 = iad1*kfv(numu)*kWDv(numu,1)*kfu(nuum)*kWDu(nuum,3)
                            iad3 = iad1
                  !          vvdgdx = 0.0
                         elseif (kcs(nmu)==2) then
                            iad1 = kfu(num)*kWDu(num,3)*kfv(nm)*kWDv(nm,1)
                            iad2 = iad1*kfv(num)*kWDv(num,1)*kfu(nuum)*kWDu(nuum,3)
                            iad3 = iad1
                   !         vvdgdx = 0.0
                         else
                          !  iad1 = kfu(num)*kfv(nm)*kfv(nmu)
                            iad3 = kfv(nm)*kWDv(nm,1)*kfv(nmu)*kWDv(nmu,1)
                            iad1 = kfu(num)*kWDu(num,3)*iad3
                            iad2 = iad1*kfv(num)*kWDv(num,1)*kfv(numu)*kWDv(numu,1)*kfu(nuum)*kWDu(nuum,3)
                         endif
                      else
                         iad1 = kfu(num)*kWDu(num,4) !kfv(nm)*kWDv(nm,1)*kfv(nmu)*kWDv(nmu,1)
                         iad2 = iad1*kfu(nuum)*kWDu(nuum,4) !iad1*kfv(num)*kWDv(num,1)*kfv(numu)*kWDv(numu,1)*kfu(nuum)*kWDu(nuum,3)
                         iad3 = iad1
                      endif
                   endif
                   if (iad2==0.and.cntrUZDbnd_n) then    
                      iad4 = iad1*kfu(ndm)*kWDu(ndm,4) !4 to be changed
                      termu =  vvhr * (2*iad1 - iad4)
                      termd = -vvhr * iad4
                      termc =  vvhr * (2*iad1 - 2*iad4)
                   else
                      termd  = 0._fp
                      termc  = vvhr*( - iad1 - iad1 - iad2)
                      termu  = vvhr*(iad1 + iad1 + iad2 + iad2)
                      termuu = vvhr*( - iad2)
                      termex = vvv*vvdgdx*iad3
                   endif
                   if (cutcell.eq.2.and.MODadvecGHOSTuzd==2) THEN
                      if ((vvhr>0.and.GHOSTu1(ndm).EQ.1).or.(vvhr<0.and.GHOSTu1(num).EQ.1)) then !.or.GHOSTu1(nuum).EQ.1) then 
                          termc = 0._fp
                          termu = 0._fp
                          termuu= 0._fp
                      endif
                   endif
                   if (mom_output) then
                      if (ghostu1(nm)/=1) then
                      mom_m_xadvec(nm, k) = mom_m_xadvec(nm, k) &
                                             & - termd*u1(ndm, k) - termc*u1(nm, k) - termu*u1(num, k) - termuu*u1(nuum, k) + termex
                      endif
                   else
                      bdy(nm, k)  = termd
                      bbk(nm, k)  = bbk(nm, k) + termc
                      buy(nm, k)  = termu
                      buuy(nm, k) = termuu
                      ddk(nm, k)  = ddk(nm, k) + termex
                   endif
                endif
             endif
          endif
       enddo
    enddo
    !WRITE (289457,*) Lmax,nmLmax
    !
end subroutine mom_cyclic
